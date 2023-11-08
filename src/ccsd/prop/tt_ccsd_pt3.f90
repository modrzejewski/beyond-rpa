module tt_ccsd_pt3
 use ccsd_transformed_integrals
    use cc3_intermediates
    use arithmetic
    use s_gen
    use basis
    use eom_vectors

        
    implicit none
    !
    ! File generated automatically on 2018-04-18 11:17:39
    !
    real(F64), dimension(:, :, :, :), allocatable :: wm_interm_0_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_1_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_2_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_3_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_4_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_5_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_6_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_7_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_8_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_9_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_10_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_11_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_12_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_13_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_14_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_15_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_16_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_17_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_18_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_19_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_20_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_21_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_22_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_23_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_24_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_25_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_26_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_27_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_28_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_29_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_30_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_31_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_32_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_33_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_34_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_35_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_36_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_37_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_38_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_39_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_40_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_41_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_42_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_43_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_44_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_45_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_46_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_47_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_48_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_49_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_50_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_51_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_52_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_53_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_54_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_55_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_56_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_57_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_58_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_59_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_60_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_61_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_62_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_63_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_64_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_65_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_66_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_67_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_68_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_69_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_70_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_71_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_72_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_73_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_74_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_75_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_76_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_77_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_78_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_79_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_80_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_81_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_82_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_83_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_84_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_85_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_86_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_87_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_88_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_89_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_90_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_91_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_92_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_93_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_94_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_95_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_96_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_97_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_98_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_99_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_100_triplet_pt3 
real(F64) :: wm_interm_101_triplet_pt3 
real(F64) :: wm_interm_102_triplet_pt3 
real(F64) :: wm_interm_103_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_104_triplet_pt3 
real(F64) :: wm_interm_105_triplet_pt3 
real(F64) :: wm_interm_106_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_107_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_108_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_109_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_110_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_111_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_112_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_113_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_114_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_115_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_116_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_117_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_118_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_119_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_120_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_121_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_122_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_123_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_124_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_125_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_126_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_127_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_128_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_129_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_130_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_131_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_132_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_133_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_134_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_135_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_136_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_137_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_138_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_139_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_140_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_141_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_142_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_143_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_144_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_145_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_146_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_147_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_148_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_149_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_150_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_151_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_152_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_153_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_154_triplet_pt3 
real(F64) :: wm_interm_155_triplet_pt3 
real(F64) :: wm_interm_156_triplet_pt3 
real(F64) :: wm_interm_157_triplet_pt3 
real(F64) :: wm_interm_158_triplet_pt3 
real(F64) :: wm_interm_159_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_160_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_161_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_162_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_163_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_164_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_165_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_166_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_167_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_168_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_169_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_170_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_171_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_172_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_173_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_174_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_175_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_176_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_177_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_178_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_179_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_180_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_181_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_182_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_183_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_184_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_185_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_186_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_187_triplet_pt3 
real(F64) :: wm_interm_188_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_189_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_190_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_191_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_192_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_193_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_194_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_195_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_196_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_197_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_198_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_199_triplet_pt3 

    contains
    
    subroutine wm_triplet_intermediates_ccsd_init_pt3(nocc, nactive)
    integer, intent(in) :: nocc
    integer, intent(in) :: nactive
    allocate(wm_interm_0_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_1_triplet_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_2_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_3_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_4_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_5_triplet_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_6_triplet_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_7_triplet_pt3(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_8_triplet_pt3(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_9_triplet_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_10_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_11_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_12_triplet_pt3(1: nocc, 1: nocc))
allocate(wm_interm_13_triplet_pt3(1: nocc, 1: nocc))
allocate(wm_interm_14_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_15_triplet_pt3(1: nocc, 1: nocc))
allocate(wm_interm_16_triplet_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_17_triplet_pt3(1: nocc, 1: nocc))
allocate(wm_interm_18_triplet_pt3(1: nocc, 1: nocc))
allocate(wm_interm_19_triplet_pt3(1: nocc, 1: nocc))
allocate(wm_interm_20_triplet_pt3(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_21_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_22_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_23_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_24_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_25_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_26_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_27_triplet_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_28_triplet_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_29_triplet_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_30_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_31_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_32_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_33_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_34_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_35_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_36_triplet_pt3(1: nocc, 1: nocc))
allocate(wm_interm_37_triplet_pt3(1: nocc, 1: nocc))
allocate(wm_interm_38_triplet_pt3(1: nocc, 1: nocc))
allocate(wm_interm_39_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_40_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_41_triplet_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_42_triplet_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_43_triplet_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_44_triplet_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_45_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_46_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_47_triplet_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_48_triplet_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_49_triplet_pt3(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_50_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_51_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_52_triplet_pt3(1: nocc, 1: nocc))
allocate(wm_interm_53_triplet_pt3(1: nocc, 1: nocc))
allocate(wm_interm_54_triplet_pt3(1: nocc, 1: nocc))
allocate(wm_interm_55_triplet_pt3(1: nocc, 1: nocc))
allocate(wm_interm_56_triplet_pt3(1: nocc, 1: nocc))
allocate(wm_interm_57_triplet_pt3(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_58_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_59_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_60_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_61_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_62_triplet_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_63_triplet_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_64_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_65_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_66_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_67_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_68_triplet_pt3(1: nocc, 1: nocc))
allocate(wm_interm_69_triplet_pt3(1: nocc, 1: nocc))
allocate(wm_interm_70_triplet_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_71_triplet_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_72_triplet_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_73_triplet_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_74_triplet_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_75_triplet_pt3(1: nocc, 1: nocc))
allocate(wm_interm_76_triplet_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_77_triplet_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_78_triplet_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_79_triplet_pt3(1: nocc, 1: nocc))
allocate(wm_interm_80_triplet_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_81_triplet_pt3(1: nocc, 1: nocc))
allocate(wm_interm_82_triplet_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_83_triplet_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_84_triplet_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_85_triplet_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_86_triplet_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_87_triplet_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_88_triplet_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_89_triplet_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_90_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_91_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_92_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_93_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_94_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_95_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_96_triplet_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_97_triplet_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_98_triplet_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_99_triplet_pt3(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_100_triplet_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_104_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_107_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_108_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_109_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_110_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_111_triplet_pt3(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_112_triplet_pt3(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_113_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_114_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_115_triplet_pt3(1: nocc, 1: nocc))
allocate(wm_interm_116_triplet_pt3(1: nocc, 1: nocc))
allocate(wm_interm_117_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_118_triplet_pt3(1: nocc, 1: nocc))
allocate(wm_interm_119_triplet_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_120_triplet_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_121_triplet_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_122_triplet_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_123_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_124_triplet_pt3(1: nocc, 1: nocc))
allocate(wm_interm_125_triplet_pt3(1: nocc, 1: nocc))
allocate(wm_interm_126_triplet_pt3(1: nocc, 1: nocc))
allocate(wm_interm_127_triplet_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_128_triplet_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_129_triplet_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_130_triplet_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_131_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_132_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_133_triplet_pt3(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_134_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_135_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_136_triplet_pt3(1: nocc, 1: nocc))
allocate(wm_interm_137_triplet_pt3(1: nocc, 1: nocc))
allocate(wm_interm_138_triplet_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_139_triplet_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_140_triplet_pt3(1: nocc, 1: nocc))
allocate(wm_interm_141_triplet_pt3(1: nocc, 1: nocc))
allocate(wm_interm_142_triplet_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_143_triplet_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_144_triplet_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_145_triplet_pt3(1: nocc, 1: nocc))
allocate(wm_interm_146_triplet_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_147_triplet_pt3(1: nocc, 1: nocc))
allocate(wm_interm_148_triplet_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_149_triplet_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_150_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_151_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_152_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_153_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_154_triplet_pt3(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_160_triplet_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_161_triplet_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_162_triplet_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_163_triplet_pt3(1: nocc, 1: nocc))
allocate(wm_interm_164_triplet_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_165_triplet_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_166_triplet_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_167_triplet_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_168_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_169_triplet_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_170_triplet_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_171_triplet_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_172_triplet_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_173_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_174_triplet_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_175_triplet_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_176_triplet_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_177_triplet_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_178_triplet_pt3(1: nocc, 1: nocc))
allocate(wm_interm_179_triplet_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_180_triplet_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_181_triplet_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_182_triplet_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_183_triplet_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_184_triplet_pt3(1: nocc, 1: nocc))
allocate(wm_interm_185_triplet_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_186_triplet_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_187_triplet_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_189_triplet_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_190_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_191_triplet_pt3(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_192_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_193_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_194_triplet_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_195_triplet_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_196_triplet_pt3(1: nocc, 1: nocc))
allocate(wm_interm_197_triplet_pt3(1: nocc, 1: nocc))
allocate(wm_interm_198_triplet_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_199_triplet_pt3(nocc+1: nactive, nocc+1: nactive))
wm_interm_0_triplet_pt3 = zero 
wm_interm_1_triplet_pt3 = zero 
wm_interm_2_triplet_pt3 = zero 
wm_interm_3_triplet_pt3 = zero 
wm_interm_4_triplet_pt3 = zero 
wm_interm_5_triplet_pt3 = zero 
wm_interm_6_triplet_pt3 = zero 
wm_interm_7_triplet_pt3 = zero 
wm_interm_8_triplet_pt3 = zero 
wm_interm_9_triplet_pt3 = zero 
wm_interm_10_triplet_pt3 = zero 
wm_interm_11_triplet_pt3 = zero 
wm_interm_12_triplet_pt3 = zero 
wm_interm_13_triplet_pt3 = zero 
wm_interm_14_triplet_pt3 = zero 
wm_interm_15_triplet_pt3 = zero 
wm_interm_16_triplet_pt3 = zero 
wm_interm_17_triplet_pt3 = zero 
wm_interm_18_triplet_pt3 = zero 
wm_interm_19_triplet_pt3 = zero 
wm_interm_20_triplet_pt3 = zero 
wm_interm_21_triplet_pt3 = zero 
wm_interm_22_triplet_pt3 = zero 
wm_interm_23_triplet_pt3 = zero 
wm_interm_24_triplet_pt3 = zero 
wm_interm_25_triplet_pt3 = zero 
wm_interm_26_triplet_pt3 = zero 
wm_interm_27_triplet_pt3 = zero 
wm_interm_28_triplet_pt3 = zero 
wm_interm_29_triplet_pt3 = zero 
wm_interm_30_triplet_pt3 = zero 
wm_interm_31_triplet_pt3 = zero 
wm_interm_32_triplet_pt3 = zero 
wm_interm_33_triplet_pt3 = zero 
wm_interm_34_triplet_pt3 = zero 
wm_interm_35_triplet_pt3 = zero 
wm_interm_36_triplet_pt3 = zero 
wm_interm_37_triplet_pt3 = zero 
wm_interm_38_triplet_pt3 = zero 
wm_interm_39_triplet_pt3 = zero 
wm_interm_40_triplet_pt3 = zero 
wm_interm_41_triplet_pt3 = zero 
wm_interm_42_triplet_pt3 = zero 
wm_interm_43_triplet_pt3 = zero 
wm_interm_44_triplet_pt3 = zero 
wm_interm_45_triplet_pt3 = zero 
wm_interm_46_triplet_pt3 = zero 
wm_interm_47_triplet_pt3 = zero 
wm_interm_48_triplet_pt3 = zero 
wm_interm_49_triplet_pt3 = zero 
wm_interm_50_triplet_pt3 = zero 
wm_interm_51_triplet_pt3 = zero 
wm_interm_52_triplet_pt3 = zero 
wm_interm_53_triplet_pt3 = zero 
wm_interm_54_triplet_pt3 = zero 
wm_interm_55_triplet_pt3 = zero 
wm_interm_56_triplet_pt3 = zero 
wm_interm_57_triplet_pt3 = zero 
wm_interm_58_triplet_pt3 = zero 
wm_interm_59_triplet_pt3 = zero 
wm_interm_60_triplet_pt3 = zero 
wm_interm_61_triplet_pt3 = zero 
wm_interm_62_triplet_pt3 = zero 
wm_interm_63_triplet_pt3 = zero 
wm_interm_64_triplet_pt3 = zero 
wm_interm_65_triplet_pt3 = zero 
wm_interm_66_triplet_pt3 = zero 
wm_interm_67_triplet_pt3 = zero 
wm_interm_68_triplet_pt3 = zero 
wm_interm_69_triplet_pt3 = zero 
wm_interm_70_triplet_pt3 = zero 
wm_interm_71_triplet_pt3 = zero 
wm_interm_72_triplet_pt3 = zero 
wm_interm_73_triplet_pt3 = zero 
wm_interm_74_triplet_pt3 = zero 
wm_interm_75_triplet_pt3 = zero 
wm_interm_76_triplet_pt3 = zero 
wm_interm_77_triplet_pt3 = zero 
wm_interm_78_triplet_pt3 = zero 
wm_interm_79_triplet_pt3 = zero 
wm_interm_80_triplet_pt3 = zero 
wm_interm_81_triplet_pt3 = zero 
wm_interm_82_triplet_pt3 = zero 
wm_interm_83_triplet_pt3 = zero 
wm_interm_84_triplet_pt3 = zero 
wm_interm_85_triplet_pt3 = zero 
wm_interm_86_triplet_pt3 = zero 
wm_interm_87_triplet_pt3 = zero 
wm_interm_88_triplet_pt3 = zero 
wm_interm_89_triplet_pt3 = zero 
wm_interm_90_triplet_pt3 = zero 
wm_interm_91_triplet_pt3 = zero 
wm_interm_92_triplet_pt3 = zero 
wm_interm_93_triplet_pt3 = zero 
wm_interm_94_triplet_pt3 = zero 
wm_interm_95_triplet_pt3 = zero 
wm_interm_96_triplet_pt3 = zero 
wm_interm_97_triplet_pt3 = zero 
wm_interm_98_triplet_pt3 = zero 
wm_interm_99_triplet_pt3 = zero 
wm_interm_100_triplet_pt3 = zero 
wm_interm_101_triplet_pt3 = zero 
wm_interm_102_triplet_pt3 = zero 
wm_interm_103_triplet_pt3 = zero 
wm_interm_104_triplet_pt3 = zero 
wm_interm_105_triplet_pt3 = zero 
wm_interm_106_triplet_pt3 = zero 
wm_interm_107_triplet_pt3 = zero 
wm_interm_108_triplet_pt3 = zero 
wm_interm_109_triplet_pt3 = zero 
wm_interm_110_triplet_pt3 = zero 
wm_interm_111_triplet_pt3 = zero 
wm_interm_112_triplet_pt3 = zero 
wm_interm_113_triplet_pt3 = zero 
wm_interm_114_triplet_pt3 = zero 
wm_interm_115_triplet_pt3 = zero 
wm_interm_116_triplet_pt3 = zero 
wm_interm_117_triplet_pt3 = zero 
wm_interm_118_triplet_pt3 = zero 
wm_interm_119_triplet_pt3 = zero 
wm_interm_120_triplet_pt3 = zero 
wm_interm_121_triplet_pt3 = zero 
wm_interm_122_triplet_pt3 = zero 
wm_interm_123_triplet_pt3 = zero 
wm_interm_124_triplet_pt3 = zero 
wm_interm_125_triplet_pt3 = zero 
wm_interm_126_triplet_pt3 = zero 
wm_interm_127_triplet_pt3 = zero 
wm_interm_128_triplet_pt3 = zero 
wm_interm_129_triplet_pt3 = zero 
wm_interm_130_triplet_pt3 = zero 
wm_interm_131_triplet_pt3 = zero 
wm_interm_132_triplet_pt3 = zero 
wm_interm_133_triplet_pt3 = zero 
wm_interm_134_triplet_pt3 = zero 
wm_interm_135_triplet_pt3 = zero 
wm_interm_136_triplet_pt3 = zero 
wm_interm_137_triplet_pt3 = zero 
wm_interm_138_triplet_pt3 = zero 
wm_interm_139_triplet_pt3 = zero 
wm_interm_140_triplet_pt3 = zero 
wm_interm_141_triplet_pt3 = zero 
wm_interm_142_triplet_pt3 = zero 
wm_interm_143_triplet_pt3 = zero 
wm_interm_144_triplet_pt3 = zero 
wm_interm_145_triplet_pt3 = zero 
wm_interm_146_triplet_pt3 = zero 
wm_interm_147_triplet_pt3 = zero 
wm_interm_148_triplet_pt3 = zero 
wm_interm_149_triplet_pt3 = zero 
wm_interm_150_triplet_pt3 = zero 
wm_interm_151_triplet_pt3 = zero 
wm_interm_152_triplet_pt3 = zero 
wm_interm_153_triplet_pt3 = zero 
wm_interm_154_triplet_pt3 = zero 
wm_interm_155_triplet_pt3 = zero 
wm_interm_156_triplet_pt3 = zero 
wm_interm_157_triplet_pt3 = zero 
wm_interm_158_triplet_pt3 = zero 
wm_interm_159_triplet_pt3 = zero 
wm_interm_160_triplet_pt3 = zero 
wm_interm_161_triplet_pt3 = zero 
wm_interm_162_triplet_pt3 = zero 
wm_interm_163_triplet_pt3 = zero 
wm_interm_164_triplet_pt3 = zero 
wm_interm_165_triplet_pt3 = zero 
wm_interm_166_triplet_pt3 = zero 
wm_interm_167_triplet_pt3 = zero 
wm_interm_168_triplet_pt3 = zero 
wm_interm_169_triplet_pt3 = zero 
wm_interm_170_triplet_pt3 = zero 
wm_interm_171_triplet_pt3 = zero 
wm_interm_172_triplet_pt3 = zero 
wm_interm_173_triplet_pt3 = zero 
wm_interm_174_triplet_pt3 = zero 
wm_interm_175_triplet_pt3 = zero 
wm_interm_176_triplet_pt3 = zero 
wm_interm_177_triplet_pt3 = zero 
wm_interm_178_triplet_pt3 = zero 
wm_interm_179_triplet_pt3 = zero 
wm_interm_180_triplet_pt3 = zero 
wm_interm_181_triplet_pt3 = zero 
wm_interm_182_triplet_pt3 = zero 
wm_interm_183_triplet_pt3 = zero 
wm_interm_184_triplet_pt3 = zero 
wm_interm_185_triplet_pt3 = zero 
wm_interm_186_triplet_pt3 = zero 
wm_interm_187_triplet_pt3 = zero 
wm_interm_188_triplet_pt3 = zero 
wm_interm_189_triplet_pt3 = zero 
wm_interm_190_triplet_pt3 = zero 
wm_interm_191_triplet_pt3 = zero 
wm_interm_192_triplet_pt3 = zero 
wm_interm_193_triplet_pt3 = zero 
wm_interm_194_triplet_pt3 = zero 
wm_interm_195_triplet_pt3 = zero 
wm_interm_196_triplet_pt3 = zero 
wm_interm_197_triplet_pt3 = zero 
wm_interm_198_triplet_pt3 = zero 
wm_interm_199_triplet_pt3 = zero 

    end subroutine wm_triplet_intermediates_ccsd_init_pt3
    
    subroutine wm_triplet_intermediates_ccsd_free_pt3
    deallocate(wm_interm_0_triplet_pt3)
deallocate(wm_interm_1_triplet_pt3)
deallocate(wm_interm_2_triplet_pt3)
deallocate(wm_interm_3_triplet_pt3)
deallocate(wm_interm_4_triplet_pt3)
deallocate(wm_interm_5_triplet_pt3)
deallocate(wm_interm_6_triplet_pt3)
deallocate(wm_interm_7_triplet_pt3)
deallocate(wm_interm_8_triplet_pt3)
deallocate(wm_interm_9_triplet_pt3)
deallocate(wm_interm_10_triplet_pt3)
deallocate(wm_interm_11_triplet_pt3)
deallocate(wm_interm_12_triplet_pt3)
deallocate(wm_interm_13_triplet_pt3)
deallocate(wm_interm_14_triplet_pt3)
deallocate(wm_interm_15_triplet_pt3)
deallocate(wm_interm_16_triplet_pt3)
deallocate(wm_interm_17_triplet_pt3)
deallocate(wm_interm_18_triplet_pt3)
deallocate(wm_interm_19_triplet_pt3)
deallocate(wm_interm_20_triplet_pt3)
deallocate(wm_interm_21_triplet_pt3)
deallocate(wm_interm_22_triplet_pt3)
deallocate(wm_interm_23_triplet_pt3)
deallocate(wm_interm_24_triplet_pt3)
deallocate(wm_interm_25_triplet_pt3)
deallocate(wm_interm_26_triplet_pt3)
deallocate(wm_interm_27_triplet_pt3)
deallocate(wm_interm_28_triplet_pt3)
deallocate(wm_interm_29_triplet_pt3)
deallocate(wm_interm_30_triplet_pt3)
deallocate(wm_interm_31_triplet_pt3)
deallocate(wm_interm_32_triplet_pt3)
deallocate(wm_interm_33_triplet_pt3)
deallocate(wm_interm_34_triplet_pt3)
deallocate(wm_interm_35_triplet_pt3)
deallocate(wm_interm_36_triplet_pt3)
deallocate(wm_interm_37_triplet_pt3)
deallocate(wm_interm_38_triplet_pt3)
deallocate(wm_interm_39_triplet_pt3)
deallocate(wm_interm_40_triplet_pt3)
deallocate(wm_interm_41_triplet_pt3)
deallocate(wm_interm_42_triplet_pt3)
deallocate(wm_interm_43_triplet_pt3)
deallocate(wm_interm_44_triplet_pt3)
deallocate(wm_interm_45_triplet_pt3)
deallocate(wm_interm_46_triplet_pt3)
deallocate(wm_interm_47_triplet_pt3)
deallocate(wm_interm_48_triplet_pt3)
deallocate(wm_interm_49_triplet_pt3)
deallocate(wm_interm_50_triplet_pt3)
deallocate(wm_interm_51_triplet_pt3)
deallocate(wm_interm_52_triplet_pt3)
deallocate(wm_interm_53_triplet_pt3)
deallocate(wm_interm_54_triplet_pt3)
deallocate(wm_interm_55_triplet_pt3)
deallocate(wm_interm_56_triplet_pt3)
deallocate(wm_interm_57_triplet_pt3)
deallocate(wm_interm_58_triplet_pt3)
deallocate(wm_interm_59_triplet_pt3)
deallocate(wm_interm_60_triplet_pt3)
deallocate(wm_interm_61_triplet_pt3)
deallocate(wm_interm_62_triplet_pt3)
deallocate(wm_interm_63_triplet_pt3)
deallocate(wm_interm_64_triplet_pt3)
deallocate(wm_interm_65_triplet_pt3)
deallocate(wm_interm_66_triplet_pt3)
deallocate(wm_interm_67_triplet_pt3)
deallocate(wm_interm_68_triplet_pt3)
deallocate(wm_interm_69_triplet_pt3)
deallocate(wm_interm_70_triplet_pt3)
deallocate(wm_interm_71_triplet_pt3)
deallocate(wm_interm_72_triplet_pt3)
deallocate(wm_interm_73_triplet_pt3)
deallocate(wm_interm_74_triplet_pt3)
deallocate(wm_interm_75_triplet_pt3)
deallocate(wm_interm_76_triplet_pt3)
deallocate(wm_interm_77_triplet_pt3)
deallocate(wm_interm_78_triplet_pt3)
deallocate(wm_interm_79_triplet_pt3)
deallocate(wm_interm_80_triplet_pt3)
deallocate(wm_interm_81_triplet_pt3)
deallocate(wm_interm_82_triplet_pt3)
deallocate(wm_interm_83_triplet_pt3)
deallocate(wm_interm_84_triplet_pt3)
deallocate(wm_interm_85_triplet_pt3)
deallocate(wm_interm_86_triplet_pt3)
deallocate(wm_interm_87_triplet_pt3)
deallocate(wm_interm_88_triplet_pt3)
deallocate(wm_interm_89_triplet_pt3)
deallocate(wm_interm_90_triplet_pt3)
deallocate(wm_interm_91_triplet_pt3)
deallocate(wm_interm_92_triplet_pt3)
deallocate(wm_interm_93_triplet_pt3)
deallocate(wm_interm_94_triplet_pt3)
deallocate(wm_interm_95_triplet_pt3)
deallocate(wm_interm_96_triplet_pt3)
deallocate(wm_interm_97_triplet_pt3)
deallocate(wm_interm_98_triplet_pt3)
deallocate(wm_interm_99_triplet_pt3)
deallocate(wm_interm_100_triplet_pt3)
deallocate(wm_interm_104_triplet_pt3)
deallocate(wm_interm_107_triplet_pt3)
deallocate(wm_interm_108_triplet_pt3)
deallocate(wm_interm_109_triplet_pt3)
deallocate(wm_interm_110_triplet_pt3)
deallocate(wm_interm_111_triplet_pt3)
deallocate(wm_interm_112_triplet_pt3)
deallocate(wm_interm_113_triplet_pt3)
deallocate(wm_interm_114_triplet_pt3)
deallocate(wm_interm_115_triplet_pt3)
deallocate(wm_interm_116_triplet_pt3)
deallocate(wm_interm_117_triplet_pt3)
deallocate(wm_interm_118_triplet_pt3)
deallocate(wm_interm_119_triplet_pt3)
deallocate(wm_interm_120_triplet_pt3)
deallocate(wm_interm_121_triplet_pt3)
deallocate(wm_interm_122_triplet_pt3)
deallocate(wm_interm_123_triplet_pt3)
deallocate(wm_interm_124_triplet_pt3)
deallocate(wm_interm_125_triplet_pt3)
deallocate(wm_interm_126_triplet_pt3)
deallocate(wm_interm_127_triplet_pt3)
deallocate(wm_interm_128_triplet_pt3)
deallocate(wm_interm_129_triplet_pt3)
deallocate(wm_interm_130_triplet_pt3)
deallocate(wm_interm_131_triplet_pt3)
deallocate(wm_interm_132_triplet_pt3)
deallocate(wm_interm_133_triplet_pt3)
deallocate(wm_interm_134_triplet_pt3)
deallocate(wm_interm_135_triplet_pt3)
deallocate(wm_interm_136_triplet_pt3)
deallocate(wm_interm_137_triplet_pt3)
deallocate(wm_interm_138_triplet_pt3)
deallocate(wm_interm_139_triplet_pt3)
deallocate(wm_interm_140_triplet_pt3)
deallocate(wm_interm_141_triplet_pt3)
deallocate(wm_interm_142_triplet_pt3)
deallocate(wm_interm_143_triplet_pt3)
deallocate(wm_interm_144_triplet_pt3)
deallocate(wm_interm_145_triplet_pt3)
deallocate(wm_interm_146_triplet_pt3)
deallocate(wm_interm_147_triplet_pt3)
deallocate(wm_interm_148_triplet_pt3)
deallocate(wm_interm_149_triplet_pt3)
deallocate(wm_interm_150_triplet_pt3)
deallocate(wm_interm_151_triplet_pt3)
deallocate(wm_interm_152_triplet_pt3)
deallocate(wm_interm_153_triplet_pt3)
deallocate(wm_interm_154_triplet_pt3)
deallocate(wm_interm_160_triplet_pt3)
deallocate(wm_interm_161_triplet_pt3)
deallocate(wm_interm_162_triplet_pt3)
deallocate(wm_interm_163_triplet_pt3)
deallocate(wm_interm_164_triplet_pt3)
deallocate(wm_interm_165_triplet_pt3)
deallocate(wm_interm_166_triplet_pt3)
deallocate(wm_interm_167_triplet_pt3)
deallocate(wm_interm_168_triplet_pt3)
deallocate(wm_interm_169_triplet_pt3)
deallocate(wm_interm_170_triplet_pt3)
deallocate(wm_interm_171_triplet_pt3)
deallocate(wm_interm_172_triplet_pt3)
deallocate(wm_interm_173_triplet_pt3)
deallocate(wm_interm_174_triplet_pt3)
deallocate(wm_interm_175_triplet_pt3)
deallocate(wm_interm_176_triplet_pt3)
deallocate(wm_interm_177_triplet_pt3)
deallocate(wm_interm_178_triplet_pt3)
deallocate(wm_interm_179_triplet_pt3)
deallocate(wm_interm_180_triplet_pt3)
deallocate(wm_interm_181_triplet_pt3)
deallocate(wm_interm_182_triplet_pt3)
deallocate(wm_interm_183_triplet_pt3)
deallocate(wm_interm_184_triplet_pt3)
deallocate(wm_interm_185_triplet_pt3)
deallocate(wm_interm_186_triplet_pt3)
deallocate(wm_interm_187_triplet_pt3)
deallocate(wm_interm_189_triplet_pt3)
deallocate(wm_interm_190_triplet_pt3)
deallocate(wm_interm_191_triplet_pt3)
deallocate(wm_interm_192_triplet_pt3)
deallocate(wm_interm_193_triplet_pt3)
deallocate(wm_interm_194_triplet_pt3)
deallocate(wm_interm_195_triplet_pt3)
deallocate(wm_interm_196_triplet_pt3)
deallocate(wm_interm_197_triplet_pt3)
deallocate(wm_interm_198_triplet_pt3)
deallocate(wm_interm_199_triplet_pt3)

    end subroutine wm_triplet_intermediates_ccsd_free_pt3
    
    subroutine wm_triplet_intermediates_ccsd_pt3(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr)
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
sum = sum + r2p(vrdav_Rl, b,i,a,j) * r2p(vrdav_Rr, a,k,c,i)
end do 
end do 
wm_interm_0_triplet_pt3(b, c, j, k) = wm_interm_0_triplet_pt3(b, c, j, k) + sum 
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
wm_interm_1_triplet_pt3(b, j, i, k) = wm_interm_1_triplet_pt3(b, j, i, k) + sum 
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
sum = sum + r2p(vrdav_Rl, b,i,a,j) * r2p(vrdav_Rr, a,i,c,k)
end do 
end do 
wm_interm_2_triplet_pt3(b, c, j, k) = wm_interm_2_triplet_pt3(b, c, j, k) + sum 
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
sum = sum + r2p(vrdav_Rl, b,i,a,j) * r2p(vrdav_Rr, c,i,a,k)
end do 
end do 
wm_interm_3_triplet_pt3(b, c, j, k) = wm_interm_3_triplet_pt3(b, c, j, k) + sum 
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
sum = sum + r2p(vrdav_Rl, b,i,a,j) * r2p(vrdav_Rr, c,k,a,i)
end do 
end do 
wm_interm_4_triplet_pt3(b, c, j, k) = wm_interm_4_triplet_pt3(b, c, j, k) + sum 
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
wm_interm_5_triplet_pt3(b, c) = wm_interm_5_triplet_pt3(b, c) + sum 
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
wm_interm_6_triplet_pt3(b, c) = wm_interm_6_triplet_pt3(b, c) + sum 
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
sum = sum + r2p(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, a,k,b,l)
end do 
end do 
wm_interm_7_triplet_pt3(i, j, k, l) = wm_interm_7_triplet_pt3(i, j, k, l) + sum 
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
sum = sum + r2p(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, b,k,a,l)
end do 
end do 
wm_interm_8_triplet_pt3(i, j, k, l) = wm_interm_8_triplet_pt3(i, j, k, l) + sum 
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
wm_interm_9_triplet_pt3(b, c) = wm_interm_9_triplet_pt3(b, c) + sum 
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
sum = sum + r2p(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, a,k,c,i)
end do 
end do 
wm_interm_10_triplet_pt3(b, c, j, k) = wm_interm_10_triplet_pt3(b, c, j, k) + sum 
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
sum = sum + r2p(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, a,i,c,k)
end do 
end do 
wm_interm_11_triplet_pt3(b, c, j, k) = wm_interm_11_triplet_pt3(b, c, j, k) + sum 
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
sum = sum + r2p(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, a,k,b,i)
end do 
end do 
end do 
wm_interm_12_triplet_pt3(j, k) = wm_interm_12_triplet_pt3(j, k) + sum 
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
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, a,i,b,k)
end do 
end do 
end do 
wm_interm_13_triplet_pt3(j, k) = wm_interm_13_triplet_pt3(j, k) + sum 
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
sum = sum + r2p(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, c,i,a,k)
end do 
end do 
wm_interm_14_triplet_pt3(b, c, j, k) = wm_interm_14_triplet_pt3(b, c, j, k) + sum 
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
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do i = 1, nocc 
sum = sum + r2p(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, b,i,a,k)
end do 
end do 
end do 
wm_interm_15_triplet_pt3(j, k) = wm_interm_15_triplet_pt3(j, k) + sum 
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
sum = sum + r2p(vrdav_Rl, b,i,a,j) * t1(a,k)
end do 
wm_interm_16_triplet_pt3(b, i, j, k) = wm_interm_16_triplet_pt3(b, i, j, k) + sum 
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
wm_interm_17_triplet_pt3(j, k) = wm_interm_17_triplet_pt3(j, k) + sum 
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
wm_interm_18_triplet_pt3(j, k) = wm_interm_18_triplet_pt3(j, k) + sum 
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
wm_interm_19_triplet_pt3(j, k) = wm_interm_19_triplet_pt3(j, k) + sum 
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
wm_interm_20_triplet_pt3(i, j, k, l) = wm_interm_20_triplet_pt3(i, j, k, l) + sum 
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
wm_interm_21_triplet_pt3(b, c, j, k) = wm_interm_21_triplet_pt3(b, c, j, k) + sum 
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
wm_interm_22_triplet_pt3(b, c, j, k) = wm_interm_22_triplet_pt3(b, c, j, k) + sum 
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
wm_interm_23_triplet_pt3(b, c, j, k) = wm_interm_23_triplet_pt3(b, c, j, k) + sum 
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
wm_interm_24_triplet_pt3(b, c, j, k) = wm_interm_24_triplet_pt3(b, c, j, k) + sum 
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
wm_interm_25_triplet_pt3(b, c, j, k) = wm_interm_25_triplet_pt3(b, c, j, k) + sum 
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
wm_interm_26_triplet_pt3(b, c, j, k) = wm_interm_26_triplet_pt3(b, c, j, k) + sum 
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
sum = sum + r2p(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, a,j,c,i)
end do 
end do 
end do 
wm_interm_27_triplet_pt3(b, c) = wm_interm_27_triplet_pt3(b, c) + sum 
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
sum = sum + r2p(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, a,i,c,j)
end do 
end do 
end do 
wm_interm_28_triplet_pt3(b, c) = wm_interm_28_triplet_pt3(b, c) + sum 
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
sum = sum + r2p(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, c,i,a,j)
end do 
end do 
end do 
wm_interm_29_triplet_pt3(b, c) = wm_interm_29_triplet_pt3(b, c) + sum 
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
wm_interm_30_triplet_pt3(b, c, j, k) = wm_interm_30_triplet_pt3(b, c, j, k) + sum 
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
sum = sum + r2p(vrdav_Rl, b,j,a,i) * r2p(vrdav_Rr, a,k,c,i)
end do 
end do 
wm_interm_31_triplet_pt3(b, c, j, k) = wm_interm_31_triplet_pt3(b, c, j, k) + sum 
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
sum = sum + r2p(vrdav_Rl, b,j,a,i) * r2p(vrdav_Rr, c,i,a,k)
end do 
end do 
wm_interm_32_triplet_pt3(b, c, j, k) = wm_interm_32_triplet_pt3(b, c, j, k) + sum 
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
sum = sum + r2p(vrdav_Rl, b,j,a,i) * r2p(vrdav_Rr, c,k,a,i)
end do 
end do 
wm_interm_33_triplet_pt3(b, c, j, k) = wm_interm_33_triplet_pt3(b, c, j, k) + sum 
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
sum = sum + r2p(vrdav_Rl, a,j,b,i) * r2p(vrdav_Rr, a,k,c,i)
end do 
end do 
wm_interm_34_triplet_pt3(b, c, j, k) = wm_interm_34_triplet_pt3(b, c, j, k) + sum 
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
sum = sum + r2p(vrdav_Rl, a,j,b,i) * r2p(vrdav_Rr, a,i,c,k)
end do 
end do 
wm_interm_35_triplet_pt3(b, c, j, k) = wm_interm_35_triplet_pt3(b, c, j, k) + sum 
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
sum = sum + r2p(vrdav_Rl, a,j,b,i) * r2p(vrdav_Rr, a,i,b,k)
end do 
end do 
end do 
wm_interm_36_triplet_pt3(j, k) = wm_interm_36_triplet_pt3(j, k) + sum 
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
sum = sum + r2p(vrdav_Rl, a,j,b,i) * r2p(vrdav_Rr, a,k,b,i)
end do 
end do 
end do 
wm_interm_37_triplet_pt3(j, k) = wm_interm_37_triplet_pt3(j, k) + sum 
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
sum = sum + r2p(vrdav_Rl, a,j,b,i) * r2p(vrdav_Rr, b,k,a,i)
end do 
end do 
end do 
wm_interm_38_triplet_pt3(j, k) = wm_interm_38_triplet_pt3(j, k) + sum 
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
sum = sum + r2p(vrdav_Rl, a,j,b,i) * r2p(vrdav_Rr, c,i,a,k)
end do 
end do 
wm_interm_39_triplet_pt3(b, c, j, k) = wm_interm_39_triplet_pt3(b, c, j, k) + sum 
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
sum = sum + r2p(vrdav_Rl, a,j,b,i) * r2p(vrdav_Rr, c,k,a,i)
end do 
end do 
wm_interm_40_triplet_pt3(b, c, j, k) = wm_interm_40_triplet_pt3(b, c, j, k) + sum 
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
sum = sum + r2p(vrdav_Rl, a,i,b,j) * t1(a,k)
end do 
wm_interm_41_triplet_pt3(b, i, j, k) = wm_interm_41_triplet_pt3(b, i, j, k) + sum 
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
sum = sum + r2p(vrdav_Rl, b,i,a,j) * r2p(vrdav_Rr, a,i,c,j)
end do 
end do 
end do 
wm_interm_42_triplet_pt3(b, c) = wm_interm_42_triplet_pt3(b, c) + sum 
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
sum = sum + r2p(vrdav_Rl, b,i,a,j) * r2p(vrdav_Rr, c,j,a,i)
end do 
end do 
end do 
wm_interm_43_triplet_pt3(b, c) = wm_interm_43_triplet_pt3(b, c) + sum 
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
sum = sum + r2p(vrdav_Rl, b,i,a,j) * r2p(vrdav_Rr, c,i,a,j)
end do 
end do 
end do 
wm_interm_44_triplet_pt3(b, c) = wm_interm_44_triplet_pt3(b, c) + sum 
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
sum = sum + r2p(vrdav_Rl, b,i,a,j) * r2m(vrdav_Rr, a,k,c,i)
end do 
end do 
wm_interm_45_triplet_pt3(b, c, j, k) = wm_interm_45_triplet_pt3(b, c, j, k) + sum 
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
sum = sum + r2p(vrdav_Rl, b,i,a,j) * r2m(vrdav_Rr, a,i,c,k)
end do 
end do 
wm_interm_46_triplet_pt3(b, c, j, k) = wm_interm_46_triplet_pt3(b, c, j, k) + sum 
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
wm_interm_47_triplet_pt3(b, c) = wm_interm_47_triplet_pt3(b, c) + sum 
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
wm_interm_48_triplet_pt3(b, c) = wm_interm_48_triplet_pt3(b, c) + sum 
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
sum = sum + r2p(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,k,b,l)
end do 
end do 
wm_interm_49_triplet_pt3(i, j, k, l) = wm_interm_49_triplet_pt3(i, j, k, l) + sum 
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
sum = sum + r2p(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,k,c,i)
end do 
end do 
wm_interm_50_triplet_pt3(b, c, j, k) = wm_interm_50_triplet_pt3(b, c, j, k) + sum 
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
sum = sum + r2p(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,i,c,k)
end do 
end do 
wm_interm_51_triplet_pt3(b, c, j, k) = wm_interm_51_triplet_pt3(b, c, j, k) + sum 
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
sum = sum + r2p(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,k,b,i)
end do 
end do 
end do 
wm_interm_52_triplet_pt3(j, k) = wm_interm_52_triplet_pt3(j, k) + sum 
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
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,i,b,k)
end do 
end do 
end do 
wm_interm_53_triplet_pt3(j, k) = wm_interm_53_triplet_pt3(j, k) + sum 
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
wm_interm_54_triplet_pt3(j, k) = wm_interm_54_triplet_pt3(j, k) + sum 
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
wm_interm_55_triplet_pt3(j, k) = wm_interm_55_triplet_pt3(j, k) + sum 
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
wm_interm_56_triplet_pt3(j, k) = wm_interm_56_triplet_pt3(j, k) + sum 
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
wm_interm_57_triplet_pt3(i, j, k, l) = wm_interm_57_triplet_pt3(i, j, k, l) + sum 
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
wm_interm_58_triplet_pt3(b, c, j, k) = wm_interm_58_triplet_pt3(b, c, j, k) + sum 
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
wm_interm_59_triplet_pt3(b, c, j, k) = wm_interm_59_triplet_pt3(b, c, j, k) + sum 
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
wm_interm_60_triplet_pt3(b, c, j, k) = wm_interm_60_triplet_pt3(b, c, j, k) + sum 
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
wm_interm_61_triplet_pt3(b, c, j, k) = wm_interm_61_triplet_pt3(b, c, j, k) + sum 
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
sum = sum + r2p(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,j,c,i)
end do 
end do 
end do 
wm_interm_62_triplet_pt3(b, c) = wm_interm_62_triplet_pt3(b, c) + sum 
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
sum = sum + r2p(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,i,c,j)
end do 
end do 
end do 
wm_interm_63_triplet_pt3(b, c) = wm_interm_63_triplet_pt3(b, c) + sum 
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
sum = sum + r2p(vrdav_Rl, b,j,a,i) * r2m(vrdav_Rr, a,k,c,i)
end do 
end do 
wm_interm_64_triplet_pt3(b, c, j, k) = wm_interm_64_triplet_pt3(b, c, j, k) + sum 
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
sum = sum + r2p(vrdav_Rl, b,j,a,i) * r2m(vrdav_Rr, a,i,c,k)
end do 
end do 
wm_interm_65_triplet_pt3(b, c, j, k) = wm_interm_65_triplet_pt3(b, c, j, k) + sum 
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
sum = sum + r2p(vrdav_Rl, a,j,b,i) * r2m(vrdav_Rr, a,k,c,i)
end do 
end do 
wm_interm_66_triplet_pt3(b, c, j, k) = wm_interm_66_triplet_pt3(b, c, j, k) + sum 
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
sum = sum + r2p(vrdav_Rl, a,j,b,i) * r2m(vrdav_Rr, a,i,c,k)
end do 
end do 
wm_interm_67_triplet_pt3(b, c, j, k) = wm_interm_67_triplet_pt3(b, c, j, k) + sum 
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
sum = sum + r2p(vrdav_Rl, a,j,b,i) * r2m(vrdav_Rr, a,i,b,k)
end do 
end do 
end do 
wm_interm_68_triplet_pt3(j, k) = wm_interm_68_triplet_pt3(j, k) + sum 
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
sum = sum + r2p(vrdav_Rl, a,j,b,i) * r2m(vrdav_Rr, a,k,b,i)
end do 
end do 
end do 
wm_interm_69_triplet_pt3(j, k) = wm_interm_69_triplet_pt3(j, k) + sum 
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
sum = sum + r2p(vrdav_Rl, b,i,a,j) * r2m(vrdav_Rr, a,i,c,j)
end do 
end do 
end do 
wm_interm_70_triplet_pt3(b, c) = wm_interm_70_triplet_pt3(b, c) + sum 
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
sum = sum + r2p(vrdav_Rl, b,i,a,j) * r2m(vrdav_Rr, a,j,c,i)
end do 
end do 
end do 
wm_interm_71_triplet_pt3(b, c) = wm_interm_71_triplet_pt3(b, c) + sum 
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
wm_interm_72_triplet_pt3(b, j) = wm_interm_72_triplet_pt3(b, j) + sum 
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
wm_interm_73_triplet_pt3(b, j) = wm_interm_73_triplet_pt3(b, j) + sum 
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
wm_interm_74_triplet_pt3(b, c) = wm_interm_74_triplet_pt3(b, c) + sum 
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
wm_interm_75_triplet_pt3(j, k) = wm_interm_75_triplet_pt3(j, k) + sum 
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
wm_interm_76_triplet_pt3(b, i, j, k) = wm_interm_76_triplet_pt3(b, i, j, k) + sum 
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
sum = sum + r2p(vrdav_Rr, a,i,b,j) * s1(a,k)
end do 
wm_interm_77_triplet_pt3(b, i, j, k) = wm_interm_77_triplet_pt3(b, i, j, k) + sum 
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
wm_interm_78_triplet_pt3(b, c) = wm_interm_78_triplet_pt3(b, c) + sum 
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
wm_interm_79_triplet_pt3(j, k) = wm_interm_79_triplet_pt3(j, k) + sum 
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
wm_interm_80_triplet_pt3(b, c) = wm_interm_80_triplet_pt3(b, c) + sum 
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
wm_interm_81_triplet_pt3(j, k) = wm_interm_81_triplet_pt3(j, k) + sum 
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
wm_interm_82_triplet_pt3(b, j, i, k) = wm_interm_82_triplet_pt3(b, j, i, k) + sum 
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
wm_interm_83_triplet_pt3(b, i, j, k) = wm_interm_83_triplet_pt3(b, i, j, k) + sum 
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
sum = sum + r2p(vrdav_Rl, b,j,a,i) * t1(a,i)
end do 
end do 
wm_interm_84_triplet_pt3(b, j) = wm_interm_84_triplet_pt3(b, j) + sum 
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
sum = sum + r2p(vrdav_Rl, b,i,a,j) * t1(a,i)
end do 
end do 
wm_interm_85_triplet_pt3(b, j) = wm_interm_85_triplet_pt3(b, j) + sum 
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
sum = sum + r2p(vrdav_Rl, a,j,b,i) * t1(a,i)
end do 
end do 
wm_interm_86_triplet_pt3(b, j) = wm_interm_86_triplet_pt3(b, j) + sum 
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
wm_interm_87_triplet_pt3(b, j) = wm_interm_87_triplet_pt3(b, j) + sum 
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
wm_interm_88_triplet_pt3(b, j) = wm_interm_88_triplet_pt3(b, j) + sum 
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
wm_interm_89_triplet_pt3(b, j) = wm_interm_89_triplet_pt3(b, j) + sum 
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
wm_interm_90_triplet_pt3(b, c, j, k) = wm_interm_90_triplet_pt3(b, c, j, k) + sum 
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
sum = sum + r2p(vrdav_Rl, b,i,a,j) * t2(a,c,i,k)
end do 
end do 
wm_interm_91_triplet_pt3(b, c, j, k) = wm_interm_91_triplet_pt3(b, c, j, k) + sum 
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
sum = sum + r2p(vrdav_Rl, b,j,a,i) * t2(a,c,i,k)
end do 
end do 
wm_interm_92_triplet_pt3(b, c, j, k) = wm_interm_92_triplet_pt3(b, c, j, k) + sum 
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
wm_interm_93_triplet_pt3(b, c, j, k) = wm_interm_93_triplet_pt3(b, c, j, k) + sum 
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
wm_interm_94_triplet_pt3(b, c, j, k) = wm_interm_94_triplet_pt3(b, c, j, k) + sum 
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
wm_interm_95_triplet_pt3(b, c, j, k) = wm_interm_95_triplet_pt3(b, c, j, k) + sum 
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
wm_interm_96_triplet_pt3(b, j) = wm_interm_96_triplet_pt3(b, j) + sum 
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
wm_interm_97_triplet_pt3(b, j) = wm_interm_97_triplet_pt3(b, j) + sum 
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
wm_interm_98_triplet_pt3(b, i, k, j) = wm_interm_98_triplet_pt3(b, i, k, j) + sum 
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
wm_interm_99_triplet_pt3(i, j, k, l) = wm_interm_99_triplet_pt3(i, j, k, l) + sum 
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
wm_interm_100_triplet_pt3(b, i, j, k) = wm_interm_100_triplet_pt3(b, i, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

sum = zero 
do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, a,j,b,i)
end do 
end do 
end do 
end do 
wm_interm_101_triplet_pt3 = wm_interm_101_triplet_pt3 + sum 
sum = zero 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, a,i,b,j)
end do 
end do 
end do 
end do 
wm_interm_102_triplet_pt3 = wm_interm_102_triplet_pt3 + sum 
sum = zero 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do i = 1, nocc 
sum = sum + r2p(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, b,i,a,j)
end do 
end do 
end do 
end do 
wm_interm_103_triplet_pt3 = wm_interm_103_triplet_pt3 + sum 
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
wm_interm_104_triplet_pt3(b, c, j, k) = wm_interm_104_triplet_pt3(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

sum = zero 
do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,j,b,i)
end do 
end do 
end do 
end do 
wm_interm_105_triplet_pt3 = wm_interm_105_triplet_pt3 + sum 
sum = zero 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,i,b,j)
end do 
end do 
end do 
end do 
wm_interm_106_triplet_pt3 = wm_interm_106_triplet_pt3 + sum 
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
sum = sum + r2m(vrdav_Rl, a,j,b,i) * r2p(vrdav_Rr, a,k,c,i)
end do 
end do 
wm_interm_107_triplet_pt3(b, c, j, k) = wm_interm_107_triplet_pt3(b, c, j, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,j,b,i) * r2p(vrdav_Rr, a,i,c,k)
end do 
end do 
wm_interm_108_triplet_pt3(b, c, j, k) = wm_interm_108_triplet_pt3(b, c, j, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,j,b,i) * r2p(vrdav_Rr, c,i,a,k)
end do 
end do 
wm_interm_109_triplet_pt3(b, c, j, k) = wm_interm_109_triplet_pt3(b, c, j, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,j,b,i) * r2p(vrdav_Rr, c,k,a,i)
end do 
end do 
wm_interm_110_triplet_pt3(b, c, j, k) = wm_interm_110_triplet_pt3(b, c, j, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, a,k,b,l)
end do 
end do 
wm_interm_111_triplet_pt3(i, j, k, l) = wm_interm_111_triplet_pt3(i, j, k, l) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, b,k,a,l)
end do 
end do 
wm_interm_112_triplet_pt3(i, j, k, l) = wm_interm_112_triplet_pt3(i, j, k, l) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, a,k,c,i)
end do 
end do 
wm_interm_113_triplet_pt3(b, c, j, k) = wm_interm_113_triplet_pt3(b, c, j, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, a,i,c,k)
end do 
end do 
wm_interm_114_triplet_pt3(b, c, j, k) = wm_interm_114_triplet_pt3(b, c, j, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, a,k,b,i)
end do 
end do 
end do 
wm_interm_115_triplet_pt3(j, k) = wm_interm_115_triplet_pt3(j, k) + sum 
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
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, a,i,b,k)
end do 
end do 
end do 
wm_interm_116_triplet_pt3(j, k) = wm_interm_116_triplet_pt3(j, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, c,i,a,k)
end do 
end do 
wm_interm_117_triplet_pt3(b, c, j, k) = wm_interm_117_triplet_pt3(b, c, j, k) + sum 
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
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do i = 1, nocc 
sum = sum + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, b,i,a,k)
end do 
end do 
end do 
wm_interm_118_triplet_pt3(j, k) = wm_interm_118_triplet_pt3(j, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,j,b,i) * t1(a,k)
end do 
wm_interm_119_triplet_pt3(b, j, i, k) = wm_interm_119_triplet_pt3(b, j, i, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, a,j,c,i)
end do 
end do 
end do 
wm_interm_120_triplet_pt3(b, c) = wm_interm_120_triplet_pt3(b, c) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, a,i,c,j)
end do 
end do 
end do 
wm_interm_121_triplet_pt3(b, c) = wm_interm_121_triplet_pt3(b, c) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, c,i,a,j)
end do 
end do 
end do 
wm_interm_122_triplet_pt3(b, c) = wm_interm_122_triplet_pt3(b, c) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, c,k,a,i)
end do 
end do 
wm_interm_123_triplet_pt3(b, c, j, k) = wm_interm_123_triplet_pt3(b, c, j, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,j,b,i) * r2p(vrdav_Rr, a,i,b,k)
end do 
end do 
end do 
wm_interm_124_triplet_pt3(j, k) = wm_interm_124_triplet_pt3(j, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,j,b,i) * r2p(vrdav_Rr, a,k,b,i)
end do 
end do 
end do 
wm_interm_125_triplet_pt3(j, k) = wm_interm_125_triplet_pt3(j, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,j,b,i) * r2p(vrdav_Rr, b,k,a,i)
end do 
end do 
end do 
wm_interm_126_triplet_pt3(j, k) = wm_interm_126_triplet_pt3(j, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * t1(a,k)
end do 
wm_interm_127_triplet_pt3(b, i, j, k) = wm_interm_127_triplet_pt3(b, i, j, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,j,b,i) * r2p(vrdav_Rr, a,i,c,j)
end do 
end do 
end do 
wm_interm_128_triplet_pt3(b, c) = wm_interm_128_triplet_pt3(b, c) + sum 
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
sum = sum + r2m(vrdav_Rl, a,j,b,i) * r2p(vrdav_Rr, c,j,a,i)
end do 
end do 
end do 
wm_interm_129_triplet_pt3(b, c) = wm_interm_129_triplet_pt3(b, c) + sum 
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
sum = sum + r2m(vrdav_Rl, a,j,b,i) * r2p(vrdav_Rr, c,i,a,j)
end do 
end do 
end do 
wm_interm_130_triplet_pt3(b, c) = wm_interm_130_triplet_pt3(b, c) + sum 
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
sum = sum + r2m(vrdav_Rl, a,j,b,i) * r2m(vrdav_Rr, a,k,c,i)
end do 
end do 
wm_interm_131_triplet_pt3(b, c, j, k) = wm_interm_131_triplet_pt3(b, c, j, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,j,b,i) * r2m(vrdav_Rr, a,i,c,k)
end do 
end do 
wm_interm_132_triplet_pt3(b, c, j, k) = wm_interm_132_triplet_pt3(b, c, j, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,k,b,l)
end do 
end do 
wm_interm_133_triplet_pt3(i, j, k, l) = wm_interm_133_triplet_pt3(i, j, k, l) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,k,c,i)
end do 
end do 
wm_interm_134_triplet_pt3(b, c, j, k) = wm_interm_134_triplet_pt3(b, c, j, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,i,c,k)
end do 
end do 
wm_interm_135_triplet_pt3(b, c, j, k) = wm_interm_135_triplet_pt3(b, c, j, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,k,b,i)
end do 
end do 
end do 
wm_interm_136_triplet_pt3(j, k) = wm_interm_136_triplet_pt3(j, k) + sum 
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
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,i,b,k)
end do 
end do 
end do 
wm_interm_137_triplet_pt3(j, k) = wm_interm_137_triplet_pt3(j, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,j,c,i)
end do 
end do 
end do 
wm_interm_138_triplet_pt3(b, c) = wm_interm_138_triplet_pt3(b, c) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,i,c,j)
end do 
end do 
end do 
wm_interm_139_triplet_pt3(b, c) = wm_interm_139_triplet_pt3(b, c) + sum 
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
sum = sum + r2m(vrdav_Rl, a,j,b,i) * r2m(vrdav_Rr, a,i,b,k)
end do 
end do 
end do 
wm_interm_140_triplet_pt3(j, k) = wm_interm_140_triplet_pt3(j, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,j,b,i) * r2m(vrdav_Rr, a,k,b,i)
end do 
end do 
end do 
wm_interm_141_triplet_pt3(j, k) = wm_interm_141_triplet_pt3(j, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,j,b,i) * r2m(vrdav_Rr, a,i,c,j)
end do 
end do 
end do 
wm_interm_142_triplet_pt3(b, c) = wm_interm_142_triplet_pt3(b, c) + sum 
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
sum = sum + r2m(vrdav_Rl, a,j,b,i) * r2m(vrdav_Rr, a,j,c,i)
end do 
end do 
end do 
wm_interm_143_triplet_pt3(b, c) = wm_interm_143_triplet_pt3(b, c) + sum 
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
wm_interm_144_triplet_pt3(b, c) = wm_interm_144_triplet_pt3(b, c) + sum 
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
wm_interm_145_triplet_pt3(j, k) = wm_interm_145_triplet_pt3(j, k) + sum 
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
wm_interm_146_triplet_pt3(b, c) = wm_interm_146_triplet_pt3(b, c) + sum 
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
wm_interm_147_triplet_pt3(j, k) = wm_interm_147_triplet_pt3(j, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * t1(a,i)
end do 
end do 
wm_interm_148_triplet_pt3(b, j) = wm_interm_148_triplet_pt3(b, j) + sum 
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
sum = sum + r2m(vrdav_Rl, a,j,b,i) * t1(a,i)
end do 
end do 
wm_interm_149_triplet_pt3(b, j) = wm_interm_149_triplet_pt3(b, j) + sum 
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
wm_interm_150_triplet_pt3(b, c, j, k) = wm_interm_150_triplet_pt3(b, c, j, k) + sum 
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
wm_interm_151_triplet_pt3(b, c, j, k) = wm_interm_151_triplet_pt3(b, c, j, k) + sum 
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
wm_interm_152_triplet_pt3(b, c, j, k) = wm_interm_152_triplet_pt3(b, c, j, k) + sum 
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
wm_interm_153_triplet_pt3(b, c, j, k) = wm_interm_153_triplet_pt3(b, c, j, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * t2(a,b,k,l)
end do 
end do 
wm_interm_154_triplet_pt3(i, j, k, l) = wm_interm_154_triplet_pt3(i, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

sum = zero 
do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, a,j,b,i)
end do 
end do 
end do 
end do 
wm_interm_155_triplet_pt3 = wm_interm_155_triplet_pt3 + sum 
sum = zero 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, a,i,b,j)
end do 
end do 
end do 
end do 
wm_interm_156_triplet_pt3 = wm_interm_156_triplet_pt3 + sum 
sum = zero 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do i = 1, nocc 
sum = sum + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, b,i,a,j)
end do 
end do 
end do 
end do 
wm_interm_157_triplet_pt3 = wm_interm_157_triplet_pt3 + sum 
sum = zero 
do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,j,b,i)
end do 
end do 
end do 
end do 
wm_interm_158_triplet_pt3 = wm_interm_158_triplet_pt3 + sum 
sum = zero 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,i,b,j)
end do 
end do 
end do 
end do 
wm_interm_159_triplet_pt3 = wm_interm_159_triplet_pt3 + sum 
!$omp parallel private(i, a, b, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
sum = zero 
do i = 1, nocc 
sum = sum + r1(vrdav_Rl, a,i) * t1(b,i)
end do 
wm_interm_160_triplet_pt3(a, b) = wm_interm_160_triplet_pt3(a, b) + sum 
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
wm_interm_161_triplet_pt3(b, i, j, k) = wm_interm_161_triplet_pt3(b, i, j, k) + sum 
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
wm_interm_162_triplet_pt3(b, i, j, k) = wm_interm_162_triplet_pt3(b, i, j, k) + sum 
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
wm_interm_163_triplet_pt3(i, j) = wm_interm_163_triplet_pt3(i, j) + sum 
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
wm_interm_164_triplet_pt3(b, i, j, k) = wm_interm_164_triplet_pt3(b, i, j, k) + sum 
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
wm_interm_165_triplet_pt3(b, j) = wm_interm_165_triplet_pt3(b, j) + sum 
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
wm_interm_166_triplet_pt3(b, j) = wm_interm_166_triplet_pt3(b, j) + sum 
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
sum = sum + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, a,i,b,j)
end do 
end do 
wm_interm_167_triplet_pt3(b, j) = wm_interm_167_triplet_pt3(b, j) + sum 
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
wm_interm_168_triplet_pt3(a, b, i, j) = wm_interm_168_triplet_pt3(a, b, i, j) + sum 
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
wm_interm_169_triplet_pt3(b, j) = wm_interm_169_triplet_pt3(b, j) + sum 
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
wm_interm_170_triplet_pt3(b, j) = wm_interm_170_triplet_pt3(b, j) + sum 
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
sum = sum + r2p(vrdav_Rl, a,i,b,j) * r1(vrdav_Rr, a,k)
end do 
wm_interm_171_triplet_pt3(b, i, j, k) = wm_interm_171_triplet_pt3(b, i, j, k) + sum 
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
sum = sum + r2p(vrdav_Rl, a,i,b,j) * r1(vrdav_Rr, a,i)
end do 
end do 
wm_interm_172_triplet_pt3(b, j) = wm_interm_172_triplet_pt3(b, j) + sum 
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
wm_interm_173_triplet_pt3(a, b, i, j) = wm_interm_173_triplet_pt3(a, b, i, j) + sum 
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
sum = sum + r2p(vrdav_Rl, a,j,b,i) * r1(vrdav_Rr, a,i)
end do 
end do 
wm_interm_174_triplet_pt3(b, j) = wm_interm_174_triplet_pt3(b, j) + sum 
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
sum = sum + r2p(vrdav_Rl, b,i,a,j) * r1(vrdav_Rr, a,k)
end do 
wm_interm_175_triplet_pt3(b, i, j, k) = wm_interm_175_triplet_pt3(b, i, j, k) + sum 
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
sum = sum + r2p(vrdav_Rl, b,i,a,j) * r1(vrdav_Rr, a,i)
end do 
end do 
wm_interm_176_triplet_pt3(b, j) = wm_interm_176_triplet_pt3(b, j) + sum 
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
wm_interm_177_triplet_pt3(a, b) = wm_interm_177_triplet_pt3(a, b) + sum 
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
wm_interm_178_triplet_pt3(i, j) = wm_interm_178_triplet_pt3(i, j) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * r1(vrdav_Rr, a,k)
end do 
wm_interm_179_triplet_pt3(b, i, j, k) = wm_interm_179_triplet_pt3(b, i, j, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * r1(vrdav_Rr, a,i)
end do 
end do 
wm_interm_180_triplet_pt3(b, j) = wm_interm_180_triplet_pt3(b, j) + sum 
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
sum = sum + r2m(vrdav_Rl, a,j,b,i) * r1(vrdav_Rr, a,i)
end do 
end do 
wm_interm_181_triplet_pt3(b, j) = wm_interm_181_triplet_pt3(b, j) + sum 
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
sum = sum + r2m(vrdav_Rl, a,j,b,i) * r1(vrdav_Rr, a,k)
end do 
wm_interm_182_triplet_pt3(b, j, i, k) = wm_interm_182_triplet_pt3(b, j, i, k) + sum 
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
wm_interm_183_triplet_pt3(b, j) = wm_interm_183_triplet_pt3(b, j) + sum 
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
wm_interm_184_triplet_pt3(i, j) = wm_interm_184_triplet_pt3(i, j) + sum 
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
wm_interm_185_triplet_pt3(b, i, j, k) = wm_interm_185_triplet_pt3(b, i, j, k) + sum 
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
wm_interm_186_triplet_pt3(b, j) = wm_interm_186_triplet_pt3(b, j) + sum 
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
wm_interm_187_triplet_pt3(a, b) = wm_interm_187_triplet_pt3(a, b) + sum 
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
wm_interm_188_triplet_pt3 = wm_interm_188_triplet_pt3 + sum 
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
wm_interm_189_triplet_pt3(b, i, j, k) = wm_interm_189_triplet_pt3(b, i, j, k) + sum 
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
wm_interm_190_triplet_pt3(b, c, j, k) = wm_interm_190_triplet_pt3(b, c, j, k) + sum 
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
wm_interm_191_triplet_pt3(i, j, k, l) = wm_interm_191_triplet_pt3(i, j, k, l) + sum 
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
wm_interm_192_triplet_pt3(b, c, j, k) = wm_interm_192_triplet_pt3(b, c, j, k) + sum 
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
wm_interm_193_triplet_pt3(b, c, j, k) = wm_interm_193_triplet_pt3(b, c, j, k) + sum 
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
wm_interm_194_triplet_pt3(b, c, j, k) = wm_interm_194_triplet_pt3(b, c, j, k) + sum 
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
sum = sum + r2m(vrdav_Rr, a,i,b,j) * s2(a,c,j,i)
end do 
end do 
end do 
wm_interm_195_triplet_pt3(b, c) = wm_interm_195_triplet_pt3(b, c) + sum 
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
wm_interm_196_triplet_pt3(j, k) = wm_interm_196_triplet_pt3(j, k) + sum 
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
wm_interm_197_triplet_pt3(j, k) = wm_interm_197_triplet_pt3(j, k) + sum 
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
wm_interm_198_triplet_pt3(b, c) = wm_interm_198_triplet_pt3(b, c) + sum 
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
wm_interm_199_triplet_pt3(b, c) = wm_interm_199_triplet_pt3(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 



    end subroutine wm_triplet_intermediates_ccsd_pt3
    
    
    function calc_D_oo_wm_triplet_pt3(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_oo_wm_triplet_pt3
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
    real(F64), dimension(0:136) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do i = 1, nocc 
term(0) = term(0) + wm_interm_163_triplet_pt3(q,i) * wm_interm_17_triplet_pt3(p,i)
term(1) = term(1) + wm_interm_163_triplet_pt3(q,i) * wm_interm_18_triplet_pt3(p,i)
term(2) = term(2) + wm_interm_163_triplet_pt3(q,i) * wm_interm_19_triplet_pt3(p,i)
term(3) = term(3) + wm_interm_163_triplet_pt3(q,i) * wm_interm_54_triplet_pt3(p,i)
term(4) = term(4) + wm_interm_163_triplet_pt3(q,i) * wm_interm_55_triplet_pt3(p,i)
term(5) = term(5) + wm_interm_163_triplet_pt3(q,i) * wm_interm_56_triplet_pt3(p,i)
term(6) = term(6) + wm_interm_163_triplet_pt3(i,q) * wm_interm_17_triplet_pt3(i,p)
term(7) = term(7) + wm_interm_163_triplet_pt3(i,q) * wm_interm_18_triplet_pt3(i,p)
term(8) = term(8) + wm_interm_163_triplet_pt3(i,q) * wm_interm_19_triplet_pt3(i,p)
term(9) = term(9) + wm_interm_163_triplet_pt3(i,q) * wm_interm_54_triplet_pt3(i,p)
term(10) = term(10) + wm_interm_163_triplet_pt3(i,q) * wm_interm_55_triplet_pt3(i,p)
term(11) = term(11) + wm_interm_163_triplet_pt3(i,q) * wm_interm_56_triplet_pt3(i,p)
term(12) = term(12) + wm_interm_178_triplet_pt3(p,i) * wm_interm_75_triplet_pt3(q,i)
term(13) = term(13) + wm_interm_178_triplet_pt3(p,i) * wm_interm_79_triplet_pt3(q,i)
term(14) = term(14) + wm_interm_178_triplet_pt3(p,i) * wm_interm_81_triplet_pt3(q,i)
term(15) = term(15) + wm_interm_178_triplet_pt3(i,p) * wm_interm_75_triplet_pt3(i,q)
term(16) = term(16) + wm_interm_178_triplet_pt3(i,p) * wm_interm_81_triplet_pt3(i,q)
term(17) = term(17) + wm_interm_178_triplet_pt3(i,p) * wm_interm_79_triplet_pt3(i,q)
term(18) = term(18) + wm_interm_145_triplet_pt3(q,i) * wm_interm_178_triplet_pt3(p,i)
term(19) = term(19) + wm_interm_147_triplet_pt3(q,i) * wm_interm_178_triplet_pt3(p,i)
term(20) = term(20) + wm_interm_145_triplet_pt3(i,q) * wm_interm_178_triplet_pt3(i,p)
term(21) = term(21) + wm_interm_147_triplet_pt3(i,q) * wm_interm_178_triplet_pt3(i,p)
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
term(12) = term(12) * (-1.0d+0) 
term(13) = term(13) * (2.0d+0) 
term(14) = term(14) * (-1.0d+0) 
term(15) = term(15) * (-1.0d+0) 
term(16) = term(16) * (-1.0d+0) 
term(17) = term(17) * (2.0d+0) 
term(18) = term(18) * (-4.0d+0) 
term(19) = term(19) * (4.0d+0) 
term(20) = term(20) * (-4.0d+0) 
term(21) = term(21) * (4.0d+0) 

do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(22) = term(22) + wm_interm_160_triplet_pt3(a,b) * wm_interm_21_triplet_pt3(a,b,p,q)
term(23) = term(23) + wm_interm_160_triplet_pt3(a,b) * wm_interm_22_triplet_pt3(a,b,p,q)
term(24) = term(24) + wm_interm_160_triplet_pt3(a,b) * wm_interm_26_triplet_pt3(a,b,p,q)
term(25) = term(25) + wm_interm_160_triplet_pt3(a,b) * wm_interm_23_triplet_pt3(a,b,p,q)
term(26) = term(26) + wm_interm_160_triplet_pt3(a,b) * wm_interm_24_triplet_pt3(a,b,p,q)
term(27) = term(27) + wm_interm_160_triplet_pt3(a,b) * wm_interm_5_triplet_pt3(a,b)
term(28) = term(28) + wm_interm_160_triplet_pt3(a,b) * wm_interm_6_triplet_pt3(a,b)
term(29) = term(29) + wm_interm_160_triplet_pt3(a,b) * wm_interm_25_triplet_pt3(a,b,p,q)
term(30) = term(30) + wm_interm_160_triplet_pt3(a,b) * wm_interm_9_triplet_pt3(a,b)
term(31) = term(31) + wm_interm_160_triplet_pt3(a,b) * wm_interm_58_triplet_pt3(a,b,p,q)
term(32) = term(32) + wm_interm_160_triplet_pt3(a,b) * wm_interm_59_triplet_pt3(a,b,p,q)
term(33) = term(33) + wm_interm_160_triplet_pt3(a,b) * wm_interm_60_triplet_pt3(a,b,p,q)
term(34) = term(34) + wm_interm_160_triplet_pt3(a,b) * wm_interm_61_triplet_pt3(a,b,p,q)
term(35) = term(35) + wm_interm_160_triplet_pt3(a,b) * wm_interm_47_triplet_pt3(a,b)
term(36) = term(36) + wm_interm_160_triplet_pt3(a,b) * wm_interm_48_triplet_pt3(a,b)
term(37) = term(37) + wm_interm_168_triplet_pt3(a,b,p,q) * wm_interm_9_triplet_pt3(a,b)
term(38) = term(38) + wm_interm_168_triplet_pt3(a,b,p,q) * wm_interm_5_triplet_pt3(a,b)
term(39) = term(39) + wm_interm_168_triplet_pt3(a,b,p,q) * wm_interm_6_triplet_pt3(a,b)
term(40) = term(40) + wm_interm_168_triplet_pt3(a,b,p,q) * wm_interm_47_triplet_pt3(a,b)
term(41) = term(41) + wm_interm_168_triplet_pt3(a,b,p,q) * wm_interm_48_triplet_pt3(a,b)
term(42) = term(42) + wm_interm_173_triplet_pt3(a,b,p,q) * wm_interm_74_triplet_pt3(a,b)
term(43) = term(43) + wm_interm_177_triplet_pt3(a,b) * wm_interm_74_triplet_pt3(a,b)
term(44) = term(44) + wm_interm_173_triplet_pt3(a,b,p,q) * wm_interm_78_triplet_pt3(a,b)
term(45) = term(45) + wm_interm_177_triplet_pt3(a,b) * wm_interm_78_triplet_pt3(a,b)
term(46) = term(46) + wm_interm_173_triplet_pt3(a,b,p,q) * wm_interm_80_triplet_pt3(a,b)
term(47) = term(47) + wm_interm_177_triplet_pt3(a,b) * wm_interm_80_triplet_pt3(a,b)
term(48) = term(48) + wm_interm_104_triplet_pt3(a,b,p,q) * wm_interm_177_triplet_pt3(a,b)
term(49) = term(49) + wm_interm_177_triplet_pt3(a,b) * wm_interm_94_triplet_pt3(a,b,p,q)
term(50) = term(50) + wm_interm_177_triplet_pt3(a,b) * wm_interm_95_triplet_pt3(a,b,p,q)
term(51) = term(51) + wm_interm_177_triplet_pt3(a,b) * wm_interm_90_triplet_pt3(a,b,p,q)
term(52) = term(52) + wm_interm_177_triplet_pt3(a,b) * wm_interm_91_triplet_pt3(a,b,p,q)
term(53) = term(53) + wm_interm_177_triplet_pt3(a,b) * wm_interm_92_triplet_pt3(a,b,p,q)
term(54) = term(54) + wm_interm_144_triplet_pt3(a,b) * wm_interm_173_triplet_pt3(a,b,p,q)
term(55) = term(55) + wm_interm_144_triplet_pt3(a,b) * wm_interm_177_triplet_pt3(a,b)
term(56) = term(56) + wm_interm_146_triplet_pt3(a,b) * wm_interm_173_triplet_pt3(a,b,p,q)
term(57) = term(57) + wm_interm_146_triplet_pt3(a,b) * wm_interm_177_triplet_pt3(a,b)
term(58) = term(58) + wm_interm_153_triplet_pt3(a,b,p,q) * wm_interm_177_triplet_pt3(a,b)
term(59) = term(59) + wm_interm_152_triplet_pt3(a,b,p,q) * wm_interm_177_triplet_pt3(a,b)
term(60) = term(60) + wm_interm_150_triplet_pt3(a,b,p,q) * wm_interm_177_triplet_pt3(a,b)
term(61) = term(61) + wm_interm_151_triplet_pt3(a,b,p,q) * wm_interm_177_triplet_pt3(a,b)
end do 
end do 

term(22) = term(22) * (-1.0d+0) 
term(23) = term(23) * (2.0d+0) 
term(24) = term(24) * (-1.0d+0) 
term(25) = term(25) * (-1.0d+0) 
term(26) = term(26) * (2.0d+0) 
term(27) = term(27) * (2.0d+0) 
term(28) = term(28) * (-4.0d+0) 
term(29) = term(29) * (-1.0d+0) 
term(30) = term(30) * (2.0d+0) 
term(31) = term(31) * (-4.0d+0) 
term(32) = term(32) * (4.0d+0) 
term(33) = term(33) * (-4.0d+0) 
term(34) = term(34) * (4.0d+0) 
term(35) = term(35) * (8.0d+0) 
term(36) = term(36) * (-8.0d+0) 
term(37) = term(37) * (-1.0d+0) 
term(38) = term(38) * (-1.0d+0) 
term(39) = term(39) * (2.0d+0) 
term(40) = term(40) * (-4.0d+0) 
term(41) = term(41) * (4.0d+0) 
term(42) = term(42) * (-1.0d+0) 
term(43) = term(43) * (2.0d+0) 
term(44) = term(44) * (2.0d+0) 
term(45) = term(45) * (-4.0d+0) 
term(46) = term(46) * (-1.0d+0) 
term(47) = term(47) * (2.0d+0) 
term(48) = term(48) * (-1.0d+0) 
term(49) = term(49) * (2.0d+0) 
term(50) = term(50) * (-1.0d+0) 
term(51) = term(51) * (-1.0d+0) 
term(52) = term(52) * (-1.0d+0) 
term(53) = term(53) * (2.0d+0) 
term(54) = term(54) * (-4.0d+0) 
term(55) = term(55) * (8.0d+0) 
term(56) = term(56) * (4.0d+0) 
term(57) = term(57) * (-8.0d+0) 
term(58) = term(58) * (-4.0d+0) 
term(59) = term(59) * (4.0d+0) 
term(60) = term(60) * (-4.0d+0) 
term(61) = term(61) * (4.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(62) = term(62) + wm_interm_165_triplet_pt3(a,i) * wm_interm_1_triplet_pt3(a,p,i,q)
term(63) = term(63) + wm_interm_166_triplet_pt3(a,i) * wm_interm_1_triplet_pt3(a,p,i,q)
term(64) = term(64) + wm_interm_167_triplet_pt3(a,i) * wm_interm_1_triplet_pt3(a,p,i,q)
term(65) = term(65) + wm_interm_165_triplet_pt3(a,i) * wm_interm_1_triplet_pt3(a,i,p,q)
term(66) = term(66) + wm_interm_166_triplet_pt3(a,i) * wm_interm_1_triplet_pt3(a,i,p,q)
term(67) = term(67) + wm_interm_167_triplet_pt3(a,i) * wm_interm_1_triplet_pt3(a,i,p,q)
term(68) = term(68) + wm_interm_169_triplet_pt3(a,i) * wm_interm_1_triplet_pt3(a,p,i,q)
term(69) = term(69) + wm_interm_170_triplet_pt3(a,i) * wm_interm_1_triplet_pt3(a,p,i,q)
term(70) = term(70) + wm_interm_169_triplet_pt3(a,i) * wm_interm_1_triplet_pt3(a,i,p,q)
term(71) = term(71) + wm_interm_170_triplet_pt3(a,i) * wm_interm_1_triplet_pt3(a,i,p,q)
term(72) = term(72) + wm_interm_171_triplet_pt3(a,q,i,p) * wm_interm_72_triplet_pt3(a,i)
term(73) = term(73) + wm_interm_172_triplet_pt3(a,i) * wm_interm_72_triplet_pt3(a,i)
term(74) = term(74) + wm_interm_171_triplet_pt3(a,i,q,p) * wm_interm_72_triplet_pt3(a,i)
term(75) = term(75) + wm_interm_174_triplet_pt3(a,i) * wm_interm_72_triplet_pt3(a,i)
term(76) = term(76) + wm_interm_175_triplet_pt3(a,q,i,p) * wm_interm_72_triplet_pt3(a,i)
term(77) = term(77) + wm_interm_176_triplet_pt3(a,i) * wm_interm_72_triplet_pt3(a,i)
term(78) = term(78) + wm_interm_175_triplet_pt3(a,q,i,p) * wm_interm_73_triplet_pt3(a,i)
term(79) = term(79) + wm_interm_176_triplet_pt3(a,i) * wm_interm_73_triplet_pt3(a,i)
term(80) = term(80) + wm_interm_171_triplet_pt3(a,q,i,p) * wm_interm_73_triplet_pt3(a,i)
term(81) = term(81) + wm_interm_172_triplet_pt3(a,i) * wm_interm_73_triplet_pt3(a,i)
term(82) = term(82) + wm_interm_171_triplet_pt3(a,i,q,p) * wm_interm_73_triplet_pt3(a,i)
term(83) = term(83) + wm_interm_174_triplet_pt3(a,i) * wm_interm_73_triplet_pt3(a,i)
term(84) = term(84) + wm_interm_174_triplet_pt3(a,i) * wm_interm_98_triplet_pt3(a,p,q,i)
term(85) = term(85) + wm_interm_176_triplet_pt3(a,i) * wm_interm_98_triplet_pt3(a,p,q,i)
term(86) = term(86) + wm_interm_172_triplet_pt3(a,i) * wm_interm_98_triplet_pt3(a,p,q,i)
term(87) = term(87) + wm_interm_176_triplet_pt3(a,i) * wm_interm_98_triplet_pt3(a,p,i,q)
term(88) = term(88) + wm_interm_174_triplet_pt3(a,i) * wm_interm_98_triplet_pt3(a,p,i,q)
term(89) = term(89) + wm_interm_172_triplet_pt3(a,i) * wm_interm_98_triplet_pt3(a,p,i,q)
term(90) = term(90) + wm_interm_179_triplet_pt3(a,q,i,p) * wm_interm_72_triplet_pt3(a,i)
term(91) = term(91) + wm_interm_180_triplet_pt3(a,i) * wm_interm_72_triplet_pt3(a,i)
term(92) = term(92) + wm_interm_179_triplet_pt3(a,i,q,p) * wm_interm_72_triplet_pt3(a,i)
term(93) = term(93) + wm_interm_181_triplet_pt3(a,i) * wm_interm_72_triplet_pt3(a,i)
term(94) = term(94) + wm_interm_182_triplet_pt3(a,q,i,p) * wm_interm_72_triplet_pt3(a,i)
term(95) = term(95) + wm_interm_182_triplet_pt3(a,q,i,p) * wm_interm_73_triplet_pt3(a,i)
term(96) = term(96) + wm_interm_181_triplet_pt3(a,i) * wm_interm_73_triplet_pt3(a,i)
term(97) = term(97) + wm_interm_179_triplet_pt3(a,q,i,p) * wm_interm_73_triplet_pt3(a,i)
term(98) = term(98) + wm_interm_180_triplet_pt3(a,i) * wm_interm_73_triplet_pt3(a,i)
term(99) = term(99) + wm_interm_179_triplet_pt3(a,i,q,p) * wm_interm_73_triplet_pt3(a,i)
term(100) = term(100) + wm_interm_181_triplet_pt3(a,i) * wm_interm_98_triplet_pt3(a,p,q,i)
term(101) = term(101) + wm_interm_180_triplet_pt3(a,i) * wm_interm_98_triplet_pt3(a,p,q,i)
term(102) = term(102) + wm_interm_181_triplet_pt3(a,i) * wm_interm_98_triplet_pt3(a,p,i,q)
term(103) = term(103) + wm_interm_180_triplet_pt3(a,i) * wm_interm_98_triplet_pt3(a,p,i,q)
end do 
end do 

term(62) = term(62) * (-1.0d+0) 
term(63) = term(63) * (-1.0d+0) 
term(64) = term(64) * (2.0d+0) 
term(65) = term(65) * (2.0d+0) 
term(66) = term(66) * (2.0d+0) 
term(67) = term(67) * (-4.0d+0) 
term(68) = term(68) * (-4.0d+0) 
term(69) = term(69) * (4.0d+0) 
term(70) = term(70) * (8.0d+0) 
term(71) = term(71) * (-8.0d+0) 
term(72) = term(72) * (-4.0d+0) 
term(73) = term(73) * (8.0d+0) 
term(74) = term(74) * (2.0d+0) 
term(75) = term(75) * (-4.0d+0) 
term(76) = term(76) * (2.0d+0) 
term(77) = term(77) * (-4.0d+0) 
term(78) = term(78) * (-1.0d+0) 
term(79) = term(79) * (2.0d+0) 
term(80) = term(80) * (2.0d+0) 
term(81) = term(81) * (-4.0d+0) 
term(82) = term(82) * (-1.0d+0) 
term(83) = term(83) * (2.0d+0) 
term(84) = term(84) * (-1.0d+0) 
term(85) = term(85) * (-1.0d+0) 
term(86) = term(86) * (2.0d+0) 
term(87) = term(87) * (2.0d+0) 
term(88) = term(88) * (2.0d+0) 
term(89) = term(89) * (-4.0d+0) 
term(90) = term(90) * (-8.0d+0) 
term(91) = term(91) * (16.0d+0) 
term(92) = term(92) * (4.0d+0) 
term(93) = term(93) * (-16.0d+0) 
term(94) = term(94) * (4.0d+0) 
term(95) = term(95) * (-2.0d+0) 
term(96) = term(96) * (8.0d+0) 
term(97) = term(97) * (4.0d+0) 
term(98) = term(98) * (-8.0d+0) 
term(99) = term(99) * (-2.0d+0) 
term(100) = term(100) * (-4.0d+0) 
term(101) = term(101) * (4.0d+0) 
term(102) = term(102) * (8.0d+0) 
term(103) = term(103) * (-8.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
term(104) = term(104) + wm_interm_163_triplet_pt3(i,j) * wm_interm_20_triplet_pt3(p,i,j,q)
term(105) = term(105) + wm_interm_163_triplet_pt3(i,j) * wm_interm_20_triplet_pt3(i,p,j,q)
term(106) = term(106) + wm_interm_163_triplet_pt3(i,j) * wm_interm_17_triplet_pt3(i,j)
term(107) = term(107) + wm_interm_163_triplet_pt3(i,j) * wm_interm_18_triplet_pt3(i,j)
term(108) = term(108) + wm_interm_163_triplet_pt3(i,j) * wm_interm_20_triplet_pt3(i,p,q,j)
term(109) = term(109) + wm_interm_163_triplet_pt3(i,j) * wm_interm_19_triplet_pt3(i,j)
term(110) = term(110) + wm_interm_163_triplet_pt3(i,j) * wm_interm_57_triplet_pt3(p,i,j,q)
term(111) = term(111) + wm_interm_163_triplet_pt3(i,j) * wm_interm_57_triplet_pt3(i,p,j,q)
term(112) = term(112) + wm_interm_163_triplet_pt3(i,j) * wm_interm_54_triplet_pt3(i,j)
term(113) = term(113) + wm_interm_163_triplet_pt3(i,j) * wm_interm_55_triplet_pt3(i,j)
term(114) = term(114) + wm_interm_163_triplet_pt3(i,j) * wm_interm_57_triplet_pt3(i,p,q,j)
term(115) = term(115) + wm_interm_163_triplet_pt3(i,j) * wm_interm_56_triplet_pt3(i,j)
term(116) = term(116) + wm_interm_178_triplet_pt3(i,j) * wm_interm_75_triplet_pt3(i,j)
term(117) = term(117) + wm_interm_178_triplet_pt3(i,j) * wm_interm_79_triplet_pt3(i,j)
term(118) = term(118) + wm_interm_178_triplet_pt3(i,j) * wm_interm_81_triplet_pt3(i,j)
term(119) = term(119) + wm_interm_178_triplet_pt3(i,j) * wm_interm_99_triplet_pt3(i,p,q,j)
term(120) = term(120) + wm_interm_178_triplet_pt3(i,j) * wm_interm_99_triplet_pt3(p,i,j,q)
term(121) = term(121) + wm_interm_178_triplet_pt3(i,j) * wm_interm_99_triplet_pt3(i,p,j,q)
term(122) = term(122) + wm_interm_145_triplet_pt3(i,j) * wm_interm_178_triplet_pt3(i,j)
term(123) = term(123) + wm_interm_147_triplet_pt3(i,j) * wm_interm_178_triplet_pt3(i,j)
term(124) = term(124) + wm_interm_154_triplet_pt3(i,p,q,j) * wm_interm_178_triplet_pt3(i,j)
term(125) = term(125) + wm_interm_154_triplet_pt3(p,i,j,q) * wm_interm_178_triplet_pt3(i,j)
term(126) = term(126) + wm_interm_154_triplet_pt3(i,p,j,q) * wm_interm_178_triplet_pt3(i,j)
end do 
end do 

term(104) = term(104) * (-1.0d+0) 
term(105) = term(105) * (2.0d+0) 
term(106) = term(106) * (2.0d+0) 
term(107) = term(107) * (-4.0d+0) 
term(108) = term(108) * (-1.0d+0) 
term(109) = term(109) * (2.0d+0) 
term(110) = term(110) * (-2.0d+0) 
term(111) = term(111) * (4.0d+0) 
term(112) = term(112) * (4.0d+0) 
term(113) = term(113) * (-8.0d+0) 
term(114) = term(114) * (-2.0d+0) 
term(115) = term(115) * (4.0d+0) 
term(116) = term(116) * (2.0d+0) 
term(117) = term(117) * (-4.0d+0) 
term(118) = term(118) * (2.0d+0) 
term(119) = term(119) * (-1.0d+0) 
term(120) = term(120) * (-1.0d+0) 
term(121) = term(121) * (2.0d+0) 
term(122) = term(122) * (8.0d+0) 
term(123) = term(123) * (-8.0d+0) 
term(124) = term(124) * (-2.0d+0) 
term(125) = term(125) * (-2.0d+0) 
term(126) = term(126) * (4.0d+0) 

do a = nocc + 1, nactive 
term(127) = term(127) + wm_interm_176_triplet_pt3(a,p) * wm_interm_72_triplet_pt3(a,q)
term(128) = term(128) + wm_interm_174_triplet_pt3(a,p) * wm_interm_72_triplet_pt3(a,q)
term(129) = term(129) + wm_interm_172_triplet_pt3(a,p) * wm_interm_72_triplet_pt3(a,q)
term(130) = term(130) + wm_interm_176_triplet_pt3(a,p) * wm_interm_73_triplet_pt3(a,q)
term(131) = term(131) + wm_interm_174_triplet_pt3(a,p) * wm_interm_73_triplet_pt3(a,q)
term(132) = term(132) + wm_interm_172_triplet_pt3(a,p) * wm_interm_73_triplet_pt3(a,q)
term(133) = term(133) + wm_interm_181_triplet_pt3(a,p) * wm_interm_72_triplet_pt3(a,q)
term(134) = term(134) + wm_interm_180_triplet_pt3(a,p) * wm_interm_72_triplet_pt3(a,q)
term(135) = term(135) + wm_interm_181_triplet_pt3(a,p) * wm_interm_73_triplet_pt3(a,q)
term(136) = term(136) + wm_interm_180_triplet_pt3(a,p) * wm_interm_73_triplet_pt3(a,q)
end do 

term(127) = term(127) * (2.0d+0) 
term(128) = term(128) * (2.0d+0) 
term(129) = term(129) * (-4.0d+0) 
term(130) = term(130) * (-1.0d+0) 
term(131) = term(131) * (-1.0d+0) 
term(132) = term(132) * (2.0d+0) 
term(133) = term(133) * (8.0d+0) 
term(134) = term(134) * (-8.0d+0) 
term(135) = term(135) * (-4.0d+0) 
term(136) = term(136) * (4.0d+0) 


    calc_D_oo_wm_triplet_pt3 = zero
    do s = 0, 136
    calc_D_oo_wm_triplet_pt3 = calc_D_oo_wm_triplet_pt3 + term(s)
    end do

    end function calc_D_oo_wm_triplet_pt3
    
    function calc_D_ov_wm_triplet_pt3(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_ov_wm_triplet_pt3
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , j, i, a, k, l, b 
    real(F64), dimension(0:472) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + wm_interm_0_triplet_pt3(q,a,i,j) * wm_interm_1_triplet_pt3(a,j,p,i)
term(1) = term(1) + wm_interm_1_triplet_pt3(a,i,p,j) * wm_interm_2_triplet_pt3(q,a,j,i)
term(2) = term(2) + wm_interm_1_triplet_pt3(a,i,p,j) * wm_interm_3_triplet_pt3(q,a,j,i)
term(3) = term(3) + wm_interm_1_triplet_pt3(a,p,i,j) * wm_interm_2_triplet_pt3(q,a,j,i)
term(4) = term(4) + wm_interm_1_triplet_pt3(a,p,i,j) * wm_interm_4_triplet_pt3(q,a,j,i)
term(5) = term(5) + wm_interm_1_triplet_pt3(a,p,i,j) * wm_interm_3_triplet_pt3(q,a,j,i)
term(6) = term(6) + wm_interm_10_triplet_pt3(q,a,i,j) * wm_interm_1_triplet_pt3(a,p,j,i)
term(7) = term(7) + wm_interm_11_triplet_pt3(q,a,i,j) * wm_interm_1_triplet_pt3(a,p,j,i)
term(8) = term(8) + wm_interm_10_triplet_pt3(q,a,i,j) * wm_interm_1_triplet_pt3(a,j,p,i)
term(9) = term(9) + wm_interm_11_triplet_pt3(q,a,i,j) * wm_interm_1_triplet_pt3(a,j,p,i)
term(10) = term(10) + wm_interm_14_triplet_pt3(q,a,i,j) * wm_interm_1_triplet_pt3(a,p,j,i)
term(11) = term(11) + wm_interm_14_triplet_pt3(q,a,i,j) * wm_interm_1_triplet_pt3(a,j,p,i)
term(12) = term(12) + wm_interm_1_triplet_pt3(a,p,i,j) * wm_interm_31_triplet_pt3(q,a,j,i)
term(13) = term(13) + wm_interm_1_triplet_pt3(a,p,i,j) * wm_interm_32_triplet_pt3(q,a,j,i)
term(14) = term(14) + wm_interm_1_triplet_pt3(a,p,i,j) * wm_interm_33_triplet_pt3(q,a,j,i)
term(15) = term(15) + wm_interm_1_triplet_pt3(a,i,p,j) * wm_interm_34_triplet_pt3(q,a,j,i)
term(16) = term(16) + wm_interm_1_triplet_pt3(a,i,p,j) * wm_interm_35_triplet_pt3(q,a,j,i)
term(17) = term(17) + wm_interm_1_triplet_pt3(a,i,p,j) * wm_interm_39_triplet_pt3(q,a,j,i)
term(18) = term(18) + wm_interm_1_triplet_pt3(a,p,i,j) * wm_interm_35_triplet_pt3(q,a,j,i)
term(19) = term(19) + wm_interm_1_triplet_pt3(a,p,i,j) * wm_interm_34_triplet_pt3(q,a,j,i)
term(20) = term(20) + wm_interm_1_triplet_pt3(a,p,i,j) * wm_interm_40_triplet_pt3(q,a,j,i)
term(21) = term(21) + wm_interm_1_triplet_pt3(a,i,p,j) * wm_interm_45_triplet_pt3(q,a,j,i)
term(22) = term(22) + wm_interm_1_triplet_pt3(a,i,p,j) * wm_interm_46_triplet_pt3(q,a,j,i)
term(23) = term(23) + wm_interm_1_triplet_pt3(a,p,i,j) * wm_interm_46_triplet_pt3(q,a,j,i)
term(24) = term(24) + wm_interm_1_triplet_pt3(a,p,i,j) * wm_interm_45_triplet_pt3(q,a,j,i)
term(25) = term(25) + wm_interm_1_triplet_pt3(a,p,i,j) * wm_interm_50_triplet_pt3(q,a,j,i)
term(26) = term(26) + wm_interm_1_triplet_pt3(a,p,i,j) * wm_interm_51_triplet_pt3(q,a,j,i)
term(27) = term(27) + wm_interm_1_triplet_pt3(a,i,p,j) * wm_interm_50_triplet_pt3(q,a,j,i)
term(28) = term(28) + wm_interm_1_triplet_pt3(a,i,p,j) * wm_interm_51_triplet_pt3(q,a,j,i)
term(29) = term(29) + wm_interm_1_triplet_pt3(a,p,i,j) * wm_interm_64_triplet_pt3(q,a,j,i)
term(30) = term(30) + wm_interm_1_triplet_pt3(a,p,i,j) * wm_interm_65_triplet_pt3(q,a,j,i)
term(31) = term(31) + wm_interm_1_triplet_pt3(a,i,p,j) * wm_interm_66_triplet_pt3(q,a,j,i)
term(32) = term(32) + wm_interm_1_triplet_pt3(a,i,p,j) * wm_interm_67_triplet_pt3(q,a,j,i)
term(33) = term(33) + wm_interm_1_triplet_pt3(a,p,i,j) * wm_interm_67_triplet_pt3(q,a,j,i)
term(34) = term(34) + wm_interm_1_triplet_pt3(a,p,i,j) * wm_interm_66_triplet_pt3(q,a,j,i)
term(35) = term(35) + wm_interm_104_triplet_pt3(a,q,i,j) * wm_interm_77_triplet_pt3(a,i,p,j)
term(36) = term(36) + wm_interm_104_triplet_pt3(a,q,i,j) * wm_interm_77_triplet_pt3(a,p,i,j)
term(37) = term(37) + wm_interm_77_triplet_pt3(a,p,i,j) * wm_interm_94_triplet_pt3(a,q,i,j)
term(38) = term(38) + wm_interm_77_triplet_pt3(a,p,i,j) * wm_interm_93_triplet_pt3(a,q,i,j)
term(39) = term(39) + wm_interm_77_triplet_pt3(a,i,p,j) * wm_interm_93_triplet_pt3(a,q,i,j)
term(40) = term(40) + wm_interm_77_triplet_pt3(a,i,p,j) * wm_interm_95_triplet_pt3(a,q,i,j)
term(41) = term(41) + wm_interm_76_triplet_pt3(a,i,p,j) * wm_interm_94_triplet_pt3(a,q,i,j)
term(42) = term(42) + wm_interm_76_triplet_pt3(a,p,i,j) * wm_interm_94_triplet_pt3(a,q,i,j)
term(43) = term(43) + wm_interm_104_triplet_pt3(a,q,i,j) * wm_interm_76_triplet_pt3(a,p,i,j)
term(44) = term(44) + wm_interm_76_triplet_pt3(a,p,i,j) * wm_interm_95_triplet_pt3(a,q,i,j)
term(45) = term(45) + wm_interm_76_triplet_pt3(a,i,p,j) * wm_interm_95_triplet_pt3(a,q,i,j)
term(46) = term(46) + wm_interm_76_triplet_pt3(a,i,p,j) * wm_interm_93_triplet_pt3(a,q,i,j)
term(47) = term(47) + wm_interm_77_triplet_pt3(a,i,p,j) * wm_interm_90_triplet_pt3(a,q,i,j)
term(48) = term(48) + wm_interm_77_triplet_pt3(a,i,p,j) * wm_interm_91_triplet_pt3(a,q,i,j)
term(49) = term(49) + wm_interm_77_triplet_pt3(a,i,p,j) * wm_interm_92_triplet_pt3(a,q,i,j)
term(50) = term(50) + wm_interm_76_triplet_pt3(a,p,i,j) * wm_interm_90_triplet_pt3(a,q,i,j)
term(51) = term(51) + wm_interm_76_triplet_pt3(a,p,i,j) * wm_interm_91_triplet_pt3(a,q,i,j)
term(52) = term(52) + wm_interm_76_triplet_pt3(a,p,i,j) * wm_interm_92_triplet_pt3(a,q,i,j)
term(53) = term(53) + wm_interm_76_triplet_pt3(a,i,p,j) * wm_interm_90_triplet_pt3(a,q,i,j)
term(54) = term(54) + wm_interm_76_triplet_pt3(a,i,p,j) * wm_interm_91_triplet_pt3(a,q,i,j)
term(55) = term(55) + wm_interm_76_triplet_pt3(a,i,p,j) * wm_interm_92_triplet_pt3(a,q,i,j)
term(56) = term(56) + wm_interm_104_triplet_pt3(a,q,i,j) * wm_interm_82_triplet_pt3(a,p,i,j)
term(57) = term(57) + wm_interm_104_triplet_pt3(a,q,i,j) * wm_interm_82_triplet_pt3(a,i,p,j)
term(58) = term(58) + wm_interm_82_triplet_pt3(a,i,p,j) * wm_interm_94_triplet_pt3(a,q,i,j)
term(59) = term(59) + wm_interm_82_triplet_pt3(a,i,p,j) * wm_interm_93_triplet_pt3(a,q,i,j)
term(60) = term(60) + wm_interm_82_triplet_pt3(a,p,i,j) * wm_interm_93_triplet_pt3(a,q,i,j)
term(61) = term(61) + wm_interm_82_triplet_pt3(a,p,i,j) * wm_interm_95_triplet_pt3(a,q,i,j)
term(62) = term(62) + wm_interm_82_triplet_pt3(a,p,i,j) * wm_interm_94_triplet_pt3(a,q,i,j)
term(63) = term(63) + wm_interm_82_triplet_pt3(a,i,p,j) * wm_interm_95_triplet_pt3(a,q,i,j)
term(64) = term(64) + wm_interm_82_triplet_pt3(a,p,i,j) * wm_interm_90_triplet_pt3(a,q,i,j)
term(65) = term(65) + wm_interm_82_triplet_pt3(a,p,i,j) * wm_interm_91_triplet_pt3(a,q,i,j)
term(66) = term(66) + wm_interm_82_triplet_pt3(a,p,i,j) * wm_interm_92_triplet_pt3(a,q,i,j)
term(67) = term(67) + wm_interm_82_triplet_pt3(a,i,p,j) * wm_interm_90_triplet_pt3(a,q,i,j)
term(68) = term(68) + wm_interm_82_triplet_pt3(a,i,p,j) * wm_interm_91_triplet_pt3(a,q,i,j)
term(69) = term(69) + wm_interm_82_triplet_pt3(a,i,p,j) * wm_interm_92_triplet_pt3(a,q,i,j)
term(70) = term(70) + wm_interm_107_triplet_pt3(q,a,i,j) * wm_interm_1_triplet_pt3(a,j,p,i)
term(71) = term(71) + wm_interm_108_triplet_pt3(q,a,i,j) * wm_interm_1_triplet_pt3(a,j,p,i)
term(72) = term(72) + wm_interm_109_triplet_pt3(q,a,i,j) * wm_interm_1_triplet_pt3(a,j,p,i)
term(73) = term(73) + wm_interm_108_triplet_pt3(q,a,i,j) * wm_interm_1_triplet_pt3(a,p,j,i)
term(74) = term(74) + wm_interm_110_triplet_pt3(q,a,i,j) * wm_interm_1_triplet_pt3(a,p,j,i)
term(75) = term(75) + wm_interm_109_triplet_pt3(q,a,i,j) * wm_interm_1_triplet_pt3(a,p,j,i)
term(76) = term(76) + wm_interm_113_triplet_pt3(q,a,i,j) * wm_interm_1_triplet_pt3(a,p,j,i)
term(77) = term(77) + wm_interm_114_triplet_pt3(q,a,i,j) * wm_interm_1_triplet_pt3(a,p,j,i)
term(78) = term(78) + wm_interm_113_triplet_pt3(q,a,i,j) * wm_interm_1_triplet_pt3(a,j,p,i)
term(79) = term(79) + wm_interm_114_triplet_pt3(q,a,i,j) * wm_interm_1_triplet_pt3(a,j,p,i)
term(80) = term(80) + wm_interm_117_triplet_pt3(q,a,i,j) * wm_interm_1_triplet_pt3(a,p,j,i)
term(81) = term(81) + wm_interm_117_triplet_pt3(q,a,i,j) * wm_interm_1_triplet_pt3(a,j,p,i)
term(82) = term(82) + wm_interm_123_triplet_pt3(q,a,i,j) * wm_interm_1_triplet_pt3(a,p,j,i)
term(83) = term(83) + wm_interm_107_triplet_pt3(q,a,i,j) * wm_interm_1_triplet_pt3(a,p,j,i)
term(84) = term(84) + wm_interm_131_triplet_pt3(q,a,i,j) * wm_interm_1_triplet_pt3(a,j,p,i)
term(85) = term(85) + wm_interm_132_triplet_pt3(q,a,i,j) * wm_interm_1_triplet_pt3(a,j,p,i)
term(86) = term(86) + wm_interm_132_triplet_pt3(q,a,i,j) * wm_interm_1_triplet_pt3(a,p,j,i)
term(87) = term(87) + wm_interm_131_triplet_pt3(q,a,i,j) * wm_interm_1_triplet_pt3(a,p,j,i)
term(88) = term(88) + wm_interm_134_triplet_pt3(q,a,i,j) * wm_interm_1_triplet_pt3(a,p,j,i)
term(89) = term(89) + wm_interm_135_triplet_pt3(q,a,i,j) * wm_interm_1_triplet_pt3(a,p,j,i)
term(90) = term(90) + wm_interm_134_triplet_pt3(q,a,i,j) * wm_interm_1_triplet_pt3(a,j,p,i)
term(91) = term(91) + wm_interm_135_triplet_pt3(q,a,i,j) * wm_interm_1_triplet_pt3(a,j,p,i)
term(92) = term(92) + wm_interm_153_triplet_pt3(a,q,i,j) * wm_interm_77_triplet_pt3(a,i,p,j)
term(93) = term(93) + wm_interm_153_triplet_pt3(a,q,i,j) * wm_interm_77_triplet_pt3(a,p,i,j)
term(94) = term(94) + wm_interm_152_triplet_pt3(a,q,i,j) * wm_interm_77_triplet_pt3(a,p,i,j)
term(95) = term(95) + wm_interm_152_triplet_pt3(a,q,i,j) * wm_interm_77_triplet_pt3(a,i,p,j)
term(96) = term(96) + wm_interm_152_triplet_pt3(a,q,i,j) * wm_interm_76_triplet_pt3(a,i,p,j)
term(97) = term(97) + wm_interm_152_triplet_pt3(a,q,i,j) * wm_interm_76_triplet_pt3(a,p,i,j)
term(98) = term(98) + wm_interm_153_triplet_pt3(a,q,i,j) * wm_interm_76_triplet_pt3(a,p,i,j)
term(99) = term(99) + wm_interm_153_triplet_pt3(a,q,i,j) * wm_interm_76_triplet_pt3(a,i,p,j)
term(100) = term(100) + wm_interm_150_triplet_pt3(a,q,i,j) * wm_interm_77_triplet_pt3(a,i,p,j)
term(101) = term(101) + wm_interm_151_triplet_pt3(a,q,i,j) * wm_interm_77_triplet_pt3(a,i,p,j)
term(102) = term(102) + wm_interm_150_triplet_pt3(a,q,i,j) * wm_interm_76_triplet_pt3(a,p,i,j)
term(103) = term(103) + wm_interm_151_triplet_pt3(a,q,i,j) * wm_interm_76_triplet_pt3(a,p,i,j)
term(104) = term(104) + wm_interm_150_triplet_pt3(a,q,i,j) * wm_interm_76_triplet_pt3(a,i,p,j)
term(105) = term(105) + wm_interm_151_triplet_pt3(a,q,i,j) * wm_interm_76_triplet_pt3(a,i,p,j)
term(106) = term(106) + wm_interm_153_triplet_pt3(a,q,i,j) * wm_interm_82_triplet_pt3(a,p,i,j)
term(107) = term(107) + wm_interm_153_triplet_pt3(a,q,i,j) * wm_interm_82_triplet_pt3(a,i,p,j)
term(108) = term(108) + wm_interm_152_triplet_pt3(a,q,i,j) * wm_interm_82_triplet_pt3(a,i,p,j)
term(109) = term(109) + wm_interm_152_triplet_pt3(a,q,i,j) * wm_interm_82_triplet_pt3(a,p,i,j)
term(110) = term(110) + wm_interm_150_triplet_pt3(a,q,i,j) * wm_interm_82_triplet_pt3(a,p,i,j)
term(111) = term(111) + wm_interm_151_triplet_pt3(a,q,i,j) * wm_interm_82_triplet_pt3(a,p,i,j)
term(112) = term(112) + wm_interm_150_triplet_pt3(a,q,i,j) * wm_interm_82_triplet_pt3(a,i,p,j)
term(113) = term(113) + wm_interm_151_triplet_pt3(a,q,i,j) * wm_interm_82_triplet_pt3(a,i,p,j)
term(114) = term(114) + r1(vrdav_Rr, a,p) * wm_interm_104_triplet_pt3(a,q,i,j) * wm_interm_196_triplet_pt3(j,i)
term(115) = term(115) + r1(vrdav_Rr, a,p) * wm_interm_104_triplet_pt3(a,q,i,j) * wm_interm_197_triplet_pt3(j,i)
term(116) = term(116) + r1(vrdav_Rr, a,p) * wm_interm_193_triplet_pt3(a,q,i,j) * wm_interm_75_triplet_pt3(j,i)
term(117) = term(117) + r1(vrdav_Rr, a,p) * wm_interm_190_triplet_pt3(a,q,i,j) * wm_interm_75_triplet_pt3(j,i)
term(118) = term(118) + r1(vrdav_Rr, a,p) * wm_interm_194_triplet_pt3(a,q,i,j) * wm_interm_75_triplet_pt3(j,i)
term(119) = term(119) + r1(vrdav_Rr, a,p) * wm_interm_193_triplet_pt3(a,q,i,j) * wm_interm_79_triplet_pt3(j,i)
term(120) = term(120) + r1(vrdav_Rr, a,p) * wm_interm_190_triplet_pt3(a,q,i,j) * wm_interm_79_triplet_pt3(j,i)
term(121) = term(121) + r1(vrdav_Rr, a,p) * wm_interm_194_triplet_pt3(a,q,i,j) * wm_interm_79_triplet_pt3(j,i)
term(122) = term(122) + r1(vrdav_Rr, a,p) * wm_interm_193_triplet_pt3(a,q,i,j) * wm_interm_81_triplet_pt3(j,i)
term(123) = term(123) + r1(vrdav_Rr, a,p) * wm_interm_190_triplet_pt3(a,q,i,j) * wm_interm_81_triplet_pt3(j,i)
term(124) = term(124) + r1(vrdav_Rr, a,p) * wm_interm_194_triplet_pt3(a,q,i,j) * wm_interm_81_triplet_pt3(j,i)
term(125) = term(125) + r1(vrdav_Rr, a,p) * wm_interm_153_triplet_pt3(a,q,i,j) * wm_interm_196_triplet_pt3(j,i)
term(126) = term(126) + r1(vrdav_Rr, a,p) * wm_interm_153_triplet_pt3(a,q,i,j) * wm_interm_197_triplet_pt3(j,i)
term(127) = term(127) + r1(vrdav_Rr, a,p) * wm_interm_150_triplet_pt3(a,q,i,j) * wm_interm_196_triplet_pt3(j,i)
term(128) = term(128) + r1(vrdav_Rr, a,p) * wm_interm_150_triplet_pt3(a,q,i,j) * wm_interm_197_triplet_pt3(j,i)
term(129) = term(129) + r1(vrdav_Rr, a,p) * wm_interm_152_triplet_pt3(a,q,i,j) * wm_interm_196_triplet_pt3(j,i)
term(130) = term(130) + r1(vrdav_Rr, a,p) * wm_interm_152_triplet_pt3(a,q,i,j) * wm_interm_197_triplet_pt3(j,i)
term(131) = term(131) + r1(vrdav_Rr, a,p) * wm_interm_151_triplet_pt3(a,q,i,j) * wm_interm_196_triplet_pt3(j,i)
term(132) = term(132) + r1(vrdav_Rr, a,p) * wm_interm_151_triplet_pt3(a,q,i,j) * wm_interm_197_triplet_pt3(j,i)
end do 
end do 
end do 

term(0) = term(0) * (-0.5d+0) 
term(2) = term(2) * (-0.5d+0) 
term(3) = term(3) * (-0.5d+0) 
term(4) = term(4) * (-0.5d+0) 
term(6) = term(6) * (-0.5d+0) 
term(9) = term(9) * (-2.0d+0) 
term(10) = term(10) * (-0.5d+0) 
term(12) = term(12) * (-0.5d+0) 
term(13) = term(13) * (-0.5d+0) 
term(15) = term(15) * (-0.5d+0) 
term(17) = term(17) * (-0.5d+0) 
term(18) = term(18) * (-0.5d+0) 
term(20) = term(20) * (-0.5d+0) 
term(21) = term(21) * (-2.0d+0) 
term(22) = term(22) * (2.0d+0) 
term(23) = term(23) * (-2.0d+0) 
term(24) = term(24) * (2.0d+0) 
term(25) = term(25) * (-2.0d+0) 
term(26) = term(26) * (2.0d+0) 
term(27) = term(27) * (4.0d+0) 
term(28) = term(28) * (-4.0d+0) 
term(29) = term(29) * (-2.0d+0) 
term(30) = term(30) * (2.0d+0) 
term(31) = term(31) * (-2.0d+0) 
term(32) = term(32) * (2.0d+0) 
term(33) = term(33) * (-2.0d+0) 
term(34) = term(34) * (2.0d+0) 
term(35) = term(35) * (-0.5d+0) 
term(37) = term(37) * (-0.5d+0) 
term(38) = term(38) * (-0.5d+0) 
term(40) = term(40) * (-0.5d+0) 
term(41) = term(41) * (-0.5d+0) 
term(43) = term(43) * (-0.5d+0) 
term(44) = term(44) * (-0.5d+0) 
term(46) = term(46) * (-0.5d+0) 
term(47) = term(47) * (-0.5d+0) 
term(48) = term(48) * (-0.5d+0) 
term(50) = term(50) * (-0.5d+0) 
term(51) = term(51) * (-0.5d+0) 
term(55) = term(55) * (-2.0d+0) 
term(56) = term(56) * (-2.0d+0) 
term(57) = term(57) * (2.0d+0) 
term(58) = term(58) * (-2.0d+0) 
term(59) = term(59) * (-2.0d+0) 
term(60) = term(60) * (2.0d+0) 
term(61) = term(61) * (-2.0d+0) 
term(62) = term(62) * (2.0d+0) 
term(63) = term(63) * (2.0d+0) 
term(64) = term(64) * (-2.0d+0) 
term(65) = term(65) * (-2.0d+0) 
term(66) = term(66) * (4.0d+0) 
term(67) = term(67) * (2.0d+0) 
term(68) = term(68) * (2.0d+0) 
term(69) = term(69) * (-4.0d+0) 
term(70) = term(70) * (-2.0d+0) 
term(71) = term(71) * (4.0d+0) 
term(72) = term(72) * (-2.0d+0) 
term(73) = term(73) * (-2.0d+0) 
term(74) = term(74) * (-2.0d+0) 
term(75) = term(75) * (2.0d+0) 
term(76) = term(76) * (-2.0d+0) 
term(77) = term(77) * (2.0d+0) 
term(78) = term(78) * (2.0d+0) 
term(79) = term(79) * (-4.0d+0) 
term(80) = term(80) * (-2.0d+0) 
term(81) = term(81) * (2.0d+0) 
term(82) = term(82) * (2.0d+0) 
term(83) = term(83) * (2.0d+0) 
term(84) = term(84) * (-8.0d+0) 
term(85) = term(85) * (8.0d+0) 
term(86) = term(86) * (-8.0d+0) 
term(87) = term(87) * (8.0d+0) 
term(88) = term(88) * (-8.0d+0) 
term(89) = term(89) * (8.0d+0) 
term(90) = term(90) * (8.0d+0) 
term(91) = term(91) * (-8.0d+0) 
term(92) = term(92) * (-2.0d+0) 
term(93) = term(93) * (2.0d+0) 
term(94) = term(94) * (-2.0d+0) 
term(95) = term(95) * (2.0d+0) 
term(96) = term(96) * (-2.0d+0) 
term(97) = term(97) * (2.0d+0) 
term(98) = term(98) * (-2.0d+0) 
term(99) = term(99) * (2.0d+0) 
term(100) = term(100) * (-2.0d+0) 
term(101) = term(101) * (2.0d+0) 
term(102) = term(102) * (-2.0d+0) 
term(103) = term(103) * (2.0d+0) 
term(104) = term(104) * (4.0d+0) 
term(105) = term(105) * (-4.0d+0) 
term(106) = term(106) * (-8.0d+0) 
term(107) = term(107) * (8.0d+0) 
term(108) = term(108) * (-8.0d+0) 
term(109) = term(109) * (8.0d+0) 
term(110) = term(110) * (-8.0d+0) 
term(111) = term(111) * (8.0d+0) 
term(112) = term(112) * (8.0d+0) 
term(113) = term(113) * (-8.0d+0) 
term(114) = term(114) * (-1.0d+0) 
term(115) = term(115) * (2.0d+0) 
term(116) = term(116) * (-1.0d+0) 
term(117) = term(117) * (-1.0d+0) 
term(118) = term(118) * (2.0d+0) 
term(119) = term(119) * (2.0d+0) 
term(120) = term(120) * (2.0d+0) 
term(121) = term(121) * (-4.0d+0) 
term(122) = term(122) * (-1.0d+0) 
term(123) = term(123) * (-1.0d+0) 
term(124) = term(124) * (2.0d+0) 
term(125) = term(125) * (-4.0d+0) 
term(126) = term(126) * (8.0d+0) 
term(127) = term(127) * (-4.0d+0) 
term(128) = term(128) * (8.0d+0) 
term(129) = term(129) * (4.0d+0) 
term(130) = term(130) * (-8.0d+0) 
term(131) = term(131) * (4.0d+0) 
term(132) = term(132) * (-8.0d+0) 

do l = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(133) = term(133) + t2(a,q,j,i) * wm_interm_175_triplet_pt3(a,k,l,p) * wm_interm_191_triplet_pt3(i,j,k,l)
term(134) = term(134) + t2(a,q,j,i) * wm_interm_171_triplet_pt3(a,k,l,p) * wm_interm_191_triplet_pt3(j,i,k,l)
term(135) = term(135) + t2(a,q,j,i) * wm_interm_171_triplet_pt3(a,k,l,p) * wm_interm_191_triplet_pt3(i,j,k,l)
term(136) = term(136) + t2(a,q,j,i) * wm_interm_185_triplet_pt3(a,p,k,l) * wm_interm_99_triplet_pt3(j,i,k,l)
term(137) = term(137) + t2(a,q,j,i) * wm_interm_185_triplet_pt3(a,p,k,l) * wm_interm_99_triplet_pt3(i,j,k,l)
term(138) = term(138) + t2(a,q,j,i) * wm_interm_182_triplet_pt3(a,k,l,p) * wm_interm_191_triplet_pt3(i,j,k,l)
term(139) = term(139) + t2(a,q,j,i) * wm_interm_179_triplet_pt3(a,k,l,p) * wm_interm_191_triplet_pt3(j,i,k,l)
term(140) = term(140) + t2(a,q,j,i) * wm_interm_179_triplet_pt3(a,k,l,p) * wm_interm_191_triplet_pt3(i,j,k,l)
term(141) = term(141) + t2(a,q,j,i) * wm_interm_154_triplet_pt3(j,i,k,l) * wm_interm_185_triplet_pt3(a,p,k,l)
term(142) = term(142) + t2(a,q,j,i) * wm_interm_154_triplet_pt3(i,j,k,l) * wm_interm_185_triplet_pt3(a,p,k,l)
end do 
end do 
end do 
end do 
end do 

term(133) = term(133) * (-1.0d+0) 
term(134) = term(134) * (-1.0d+0) 
term(135) = term(135) * (2.0d+0) 
term(136) = term(136) * (-1.0d+0) 
term(137) = term(137) * (2.0d+0) 
term(138) = term(138) * (-2.0d+0) 
term(139) = term(139) * (-2.0d+0) 
term(140) = term(140) * (4.0d+0) 
term(141) = term(141) * (-2.0d+0) 
term(142) = term(142) * (4.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(143) = term(143) + t2(a,q,j,i) * wm_interm_185_triplet_pt3(a,p,k,l) * wm_interm_99_triplet_pt3(i,j,l,k)
term(144) = term(144) + t2(a,q,j,i) * wm_interm_154_triplet_pt3(i,j,k,l) * wm_interm_185_triplet_pt3(a,p,l,k)
end do 
end do 
end do 
end do 
end do 

term(143) = term(143) * (-1.0d+0) 
term(144) = term(144) * (-2.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(145) = term(145) + r1(vrdav_Rr, a,p) * wm_interm_104_triplet_pt3(b,q,i,j) * wm_interm_193_triplet_pt3(a,b,j,i)
term(146) = term(146) + r1(vrdav_Rr, a,p) * wm_interm_104_triplet_pt3(b,q,i,j) * wm_interm_194_triplet_pt3(a,b,j,i)
term(147) = term(147) + r1(vrdav_Rr, a,p) * wm_interm_192_triplet_pt3(b,q,i,j) * wm_interm_92_triplet_pt3(a,b,j,i)
term(148) = term(148) + r1(vrdav_Rr, a,p) * wm_interm_190_triplet_pt3(b,q,i,j) * wm_interm_92_triplet_pt3(a,b,j,i)
term(149) = term(149) + r1(vrdav_Rr, a,p) * wm_interm_193_triplet_pt3(b,q,i,j) * wm_interm_92_triplet_pt3(a,b,j,i)
term(150) = term(150) + r1(vrdav_Rr, a,p) * wm_interm_194_triplet_pt3(b,q,i,j) * wm_interm_92_triplet_pt3(a,b,j,i)
term(151) = term(151) + r1(vrdav_Rr, a,p) * wm_interm_193_triplet_pt3(a,b,i,j) * wm_interm_93_triplet_pt3(b,q,j,i)
term(152) = term(152) + r1(vrdav_Rr, a,p) * wm_interm_194_triplet_pt3(a,b,i,j) * wm_interm_93_triplet_pt3(b,q,j,i)
term(153) = term(153) + r1(vrdav_Rr, a,p) * wm_interm_192_triplet_pt3(b,q,i,j) * wm_interm_91_triplet_pt3(a,b,j,i)
term(154) = term(154) + r1(vrdav_Rr, a,p) * wm_interm_190_triplet_pt3(b,q,i,j) * wm_interm_91_triplet_pt3(a,b,j,i)
term(155) = term(155) + r1(vrdav_Rr, a,p) * wm_interm_193_triplet_pt3(b,q,i,j) * wm_interm_91_triplet_pt3(a,b,j,i)
term(156) = term(156) + r1(vrdav_Rr, a,p) * wm_interm_194_triplet_pt3(b,q,i,j) * wm_interm_91_triplet_pt3(a,b,j,i)
term(157) = term(157) + r1(vrdav_Rr, a,p) * wm_interm_193_triplet_pt3(a,b,i,j) * wm_interm_94_triplet_pt3(b,q,j,i)
term(158) = term(158) + r1(vrdav_Rr, a,p) * wm_interm_194_triplet_pt3(a,b,i,j) * wm_interm_94_triplet_pt3(b,q,j,i)
term(159) = term(159) + r1(vrdav_Rr, a,p) * wm_interm_192_triplet_pt3(b,q,i,j) * wm_interm_90_triplet_pt3(a,b,j,i)
term(160) = term(160) + r1(vrdav_Rr, a,p) * wm_interm_190_triplet_pt3(b,q,i,j) * wm_interm_90_triplet_pt3(a,b,j,i)
term(161) = term(161) + r1(vrdav_Rr, a,p) * wm_interm_193_triplet_pt3(b,q,i,j) * wm_interm_90_triplet_pt3(a,b,j,i)
term(162) = term(162) + r1(vrdav_Rr, a,p) * wm_interm_194_triplet_pt3(b,q,i,j) * wm_interm_90_triplet_pt3(a,b,j,i)
term(163) = term(163) + r1(vrdav_Rr, a,p) * wm_interm_190_triplet_pt3(a,b,i,j) * wm_interm_94_triplet_pt3(b,q,j,i)
term(164) = term(164) + r1(vrdav_Rr, a,p) * wm_interm_190_triplet_pt3(b,q,i,j) * wm_interm_93_triplet_pt3(a,b,j,i)
term(165) = term(165) + r1(vrdav_Rr, a,p) * wm_interm_194_triplet_pt3(b,q,i,j) * wm_interm_93_triplet_pt3(a,b,j,i)
term(166) = term(166) + r1(vrdav_Rr, a,p) * wm_interm_192_triplet_pt3(a,b,i,j) * wm_interm_91_triplet_pt3(b,q,j,i)
term(167) = term(167) + r1(vrdav_Rr, a,p) * wm_interm_192_triplet_pt3(a,b,i,j) * wm_interm_94_triplet_pt3(b,q,j,i)
term(168) = term(168) + r1(vrdav_Rr, a,p) * wm_interm_193_triplet_pt3(b,q,i,j) * wm_interm_93_triplet_pt3(a,b,j,i)
term(169) = term(169) + r1(vrdav_Rr, a,p) * wm_interm_104_triplet_pt3(b,q,i,j) * wm_interm_192_triplet_pt3(a,b,j,i)
term(170) = term(170) + r1(vrdav_Rr, a,p) * wm_interm_104_triplet_pt3(b,q,i,j) * wm_interm_190_triplet_pt3(a,b,j,i)
term(171) = term(171) + r1(vrdav_Rr, a,p) * wm_interm_192_triplet_pt3(b,q,i,j) * wm_interm_95_triplet_pt3(a,b,j,i)
term(172) = term(172) + r1(vrdav_Rr, a,p) * wm_interm_190_triplet_pt3(b,q,i,j) * wm_interm_95_triplet_pt3(a,b,j,i)
term(173) = term(173) + r1(vrdav_Rr, a,p) * wm_interm_193_triplet_pt3(b,q,i,j) * wm_interm_95_triplet_pt3(a,b,j,i)
term(174) = term(174) + r1(vrdav_Rr, a,p) * wm_interm_194_triplet_pt3(b,q,i,j) * wm_interm_95_triplet_pt3(a,b,j,i)
term(175) = term(175) + r1(vrdav_Rr, a,p) * wm_interm_190_triplet_pt3(a,b,i,j) * wm_interm_93_triplet_pt3(b,q,j,i)
term(176) = term(176) + r1(vrdav_Rr, a,p) * wm_interm_192_triplet_pt3(a,b,i,j) * wm_interm_90_triplet_pt3(b,q,j,i)
term(177) = term(177) + r1(vrdav_Rr, a,p) * wm_interm_193_triplet_pt3(b,q,i,j) * wm_interm_94_triplet_pt3(a,b,j,i)
term(178) = term(178) + r1(vrdav_Rr, a,p) * wm_interm_192_triplet_pt3(b,q,i,j) * wm_interm_94_triplet_pt3(a,b,j,i)
term(179) = term(179) + r1(vrdav_Rr, a,p) * wm_interm_190_triplet_pt3(b,q,i,j) * wm_interm_94_triplet_pt3(a,b,j,i)
term(180) = term(180) + r1(vrdav_Rr, a,p) * wm_interm_194_triplet_pt3(b,q,i,j) * wm_interm_94_triplet_pt3(a,b,j,i)
term(181) = term(181) + r1(vrdav_Rr, a,p) * wm_interm_192_triplet_pt3(a,b,i,j) * wm_interm_95_triplet_pt3(b,q,j,i)
term(182) = term(182) + r1(vrdav_Rr, a,p) * wm_interm_192_triplet_pt3(a,b,i,j) * wm_interm_92_triplet_pt3(b,q,j,i)
term(183) = term(183) + r1(vrdav_Rr, a,p) * wm_interm_104_triplet_pt3(a,b,i,j) * wm_interm_192_triplet_pt3(b,q,j,i)
term(184) = term(184) + r1(vrdav_Rr, a,p) * wm_interm_153_triplet_pt3(b,q,i,j) * wm_interm_193_triplet_pt3(a,b,j,i)
term(185) = term(185) + r1(vrdav_Rr, a,p) * wm_interm_153_triplet_pt3(b,q,i,j) * wm_interm_194_triplet_pt3(a,b,j,i)
term(186) = term(186) + r1(vrdav_Rr, a,p) * wm_interm_151_triplet_pt3(a,b,i,j) * wm_interm_192_triplet_pt3(b,q,j,i)
term(187) = term(187) + r1(vrdav_Rr, a,p) * wm_interm_151_triplet_pt3(a,b,i,j) * wm_interm_190_triplet_pt3(b,q,j,i)
term(188) = term(188) + r1(vrdav_Rr, a,p) * wm_interm_151_triplet_pt3(a,b,i,j) * wm_interm_193_triplet_pt3(b,q,j,i)
term(189) = term(189) + r1(vrdav_Rr, a,p) * wm_interm_151_triplet_pt3(a,b,i,j) * wm_interm_194_triplet_pt3(b,q,j,i)
term(190) = term(190) + r1(vrdav_Rr, a,p) * wm_interm_152_triplet_pt3(b,q,i,j) * wm_interm_193_triplet_pt3(a,b,j,i)
term(191) = term(191) + r1(vrdav_Rr, a,p) * wm_interm_152_triplet_pt3(b,q,i,j) * wm_interm_194_triplet_pt3(a,b,j,i)
term(192) = term(192) + r1(vrdav_Rr, a,p) * wm_interm_150_triplet_pt3(a,b,i,j) * wm_interm_192_triplet_pt3(b,q,j,i)
term(193) = term(193) + r1(vrdav_Rr, a,p) * wm_interm_150_triplet_pt3(a,b,i,j) * wm_interm_190_triplet_pt3(b,q,j,i)
term(194) = term(194) + r1(vrdav_Rr, a,p) * wm_interm_150_triplet_pt3(a,b,i,j) * wm_interm_193_triplet_pt3(b,q,j,i)
term(195) = term(195) + r1(vrdav_Rr, a,p) * wm_interm_150_triplet_pt3(a,b,i,j) * wm_interm_194_triplet_pt3(b,q,j,i)
term(196) = term(196) + r1(vrdav_Rr, a,p) * wm_interm_152_triplet_pt3(b,q,i,j) * wm_interm_190_triplet_pt3(a,b,j,i)
term(197) = term(197) + r1(vrdav_Rr, a,p) * wm_interm_152_triplet_pt3(a,b,i,j) * wm_interm_190_triplet_pt3(b,q,j,i)
term(198) = term(198) + r1(vrdav_Rr, a,p) * wm_interm_152_triplet_pt3(a,b,i,j) * wm_interm_194_triplet_pt3(b,q,j,i)
term(199) = term(199) + r1(vrdav_Rr, a,p) * wm_interm_150_triplet_pt3(b,q,i,j) * wm_interm_192_triplet_pt3(a,b,j,i)
term(200) = term(200) + r1(vrdav_Rr, a,p) * wm_interm_152_triplet_pt3(b,q,i,j) * wm_interm_192_triplet_pt3(a,b,j,i)
term(201) = term(201) + r1(vrdav_Rr, a,p) * wm_interm_152_triplet_pt3(a,b,i,j) * wm_interm_193_triplet_pt3(b,q,j,i)
term(202) = term(202) + r1(vrdav_Rr, a,p) * wm_interm_153_triplet_pt3(b,q,i,j) * wm_interm_192_triplet_pt3(a,b,j,i)
term(203) = term(203) + r1(vrdav_Rr, a,p) * wm_interm_153_triplet_pt3(b,q,i,j) * wm_interm_190_triplet_pt3(a,b,j,i)
term(204) = term(204) + r1(vrdav_Rr, a,p) * wm_interm_153_triplet_pt3(a,b,i,j) * wm_interm_192_triplet_pt3(b,q,j,i)
term(205) = term(205) + r1(vrdav_Rr, a,p) * wm_interm_153_triplet_pt3(a,b,i,j) * wm_interm_190_triplet_pt3(b,q,j,i)
term(206) = term(206) + r1(vrdav_Rr, a,p) * wm_interm_153_triplet_pt3(a,b,i,j) * wm_interm_193_triplet_pt3(b,q,j,i)
term(207) = term(207) + r1(vrdav_Rr, a,p) * wm_interm_153_triplet_pt3(a,b,i,j) * wm_interm_194_triplet_pt3(b,q,j,i)
term(208) = term(208) + r1(vrdav_Rr, a,p) * wm_interm_152_triplet_pt3(a,b,i,j) * wm_interm_192_triplet_pt3(b,q,j,i)
term(209) = term(209) + r1(vrdav_Rr, a,p) * wm_interm_151_triplet_pt3(b,q,i,j) * wm_interm_192_triplet_pt3(a,b,j,i)
end do 
end do 
end do 
end do 

term(145) = term(145) * (2.0d+0) 
term(146) = term(146) * (-4.0d+0) 
term(147) = term(147) * (2.0d+0) 
term(148) = term(148) * (-4.0d+0) 
term(149) = term(149) * (-4.0d+0) 
term(150) = term(150) * (8.0d+0) 
term(151) = term(151) * (-1.0d+0) 
term(152) = term(152) * (2.0d+0) 
term(153) = term(153) * (-1.0d+0) 
term(154) = term(154) * (2.0d+0) 
term(155) = term(155) * (2.0d+0) 
term(156) = term(156) * (-4.0d+0) 
term(157) = term(157) * (-1.0d+0) 
term(158) = term(158) * (2.0d+0) 
term(159) = term(159) * (-1.0d+0) 
term(160) = term(160) * (2.0d+0) 
term(161) = term(161) * (2.0d+0) 
term(162) = term(162) * (-4.0d+0) 
term(163) = term(163) * (-1.0d+0) 
term(164) = term(164) * (-1.0d+0) 
term(165) = term(165) * (2.0d+0) 
term(166) = term(166) * (-1.0d+0) 
term(167) = term(167) * (2.0d+0) 
term(168) = term(168) * (-1.0d+0) 
term(169) = term(169) * (-1.0d+0) 
term(170) = term(170) * (2.0d+0) 
term(171) = term(171) * (-1.0d+0) 
term(172) = term(172) * (2.0d+0) 
term(173) = term(173) * (2.0d+0) 
term(174) = term(174) * (-4.0d+0) 
term(175) = term(175) * (-1.0d+0) 
term(176) = term(176) * (-1.0d+0) 
term(177) = term(177) * (-1.0d+0) 
term(178) = term(178) * (2.0d+0) 
term(179) = term(179) * (-1.0d+0) 
term(180) = term(180) * (2.0d+0) 
term(181) = term(181) * (-1.0d+0) 
term(182) = term(182) * (2.0d+0) 
term(183) = term(183) * (-1.0d+0) 
term(184) = term(184) * (4.0d+0) 
term(185) = term(185) * (-8.0d+0) 
term(186) = term(186) * (4.0d+0) 
term(187) = term(187) * (-8.0d+0) 
term(188) = term(188) * (-8.0d+0) 
term(189) = term(189) * (16.0d+0) 
term(190) = term(190) * (-4.0d+0) 
term(191) = term(191) * (8.0d+0) 
term(192) = term(192) * (-4.0d+0) 
term(193) = term(193) * (8.0d+0) 
term(194) = term(194) * (8.0d+0) 
term(195) = term(195) * (-16.0d+0) 
term(196) = term(196) * (-4.0d+0) 
term(197) = term(197) * (-4.0d+0) 
term(198) = term(198) * (8.0d+0) 
term(199) = term(199) * (-4.0d+0) 
term(200) = term(200) * (4.0d+0) 
term(201) = term(201) * (-4.0d+0) 
term(202) = term(202) * (-4.0d+0) 
term(203) = term(203) * (4.0d+0) 
term(204) = term(204) * (-4.0d+0) 
term(205) = term(205) * (4.0d+0) 
term(206) = term(206) * (4.0d+0) 
term(207) = term(207) * (-8.0d+0) 
term(208) = term(208) * (4.0d+0) 
term(209) = term(209) * (4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(210) = term(210) + wm_interm_16_triplet_pt3(q,i,j,k) * wm_interm_20_triplet_pt3(i,j,k,p)
term(211) = term(211) + wm_interm_16_triplet_pt3(q,i,j,k) * wm_interm_20_triplet_pt3(j,i,p,k)
term(212) = term(212) + wm_interm_16_triplet_pt3(q,i,j,k) * wm_interm_20_triplet_pt3(i,j,p,k)
term(213) = term(213) + wm_interm_20_triplet_pt3(i,j,k,p) * wm_interm_41_triplet_pt3(q,i,j,k)
term(214) = term(214) + wm_interm_20_triplet_pt3(i,j,p,k) * wm_interm_41_triplet_pt3(q,i,j,k)
term(215) = term(215) + wm_interm_16_triplet_pt3(q,i,j,k) * wm_interm_57_triplet_pt3(i,j,k,p)
term(216) = term(216) + wm_interm_16_triplet_pt3(q,i,j,k) * wm_interm_57_triplet_pt3(j,i,p,k)
term(217) = term(217) + wm_interm_16_triplet_pt3(q,i,j,k) * wm_interm_57_triplet_pt3(i,j,p,k)
term(218) = term(218) + wm_interm_41_triplet_pt3(q,i,j,k) * wm_interm_57_triplet_pt3(j,i,k,p)
term(219) = term(219) + wm_interm_41_triplet_pt3(q,i,j,k) * wm_interm_57_triplet_pt3(i,j,k,p)
term(220) = term(220) + wm_interm_41_triplet_pt3(q,i,j,k) * wm_interm_57_triplet_pt3(i,j,p,k)
term(221) = term(221) + wm_interm_100_triplet_pt3(q,i,j,k) * wm_interm_7_triplet_pt3(j,k,i,p)
term(222) = term(222) + wm_interm_100_triplet_pt3(q,i,j,k) * wm_interm_8_triplet_pt3(j,k,i,p)
term(223) = term(223) + wm_interm_100_triplet_pt3(q,i,j,k) * wm_interm_7_triplet_pt3(k,j,i,p)
term(224) = term(224) + wm_interm_100_triplet_pt3(q,i,j,k) * wm_interm_49_triplet_pt3(j,k,i,p)
term(225) = term(225) + wm_interm_100_triplet_pt3(q,i,j,k) * wm_interm_49_triplet_pt3(k,j,i,p)
term(226) = term(226) + wm_interm_119_triplet_pt3(q,i,j,k) * wm_interm_20_triplet_pt3(i,j,k,p)
term(227) = term(227) + wm_interm_119_triplet_pt3(q,i,j,k) * wm_interm_20_triplet_pt3(j,i,p,k)
term(228) = term(228) + wm_interm_119_triplet_pt3(q,i,j,k) * wm_interm_20_triplet_pt3(i,j,p,k)
term(229) = term(229) + wm_interm_127_triplet_pt3(q,i,j,k) * wm_interm_20_triplet_pt3(j,i,k,p)
term(230) = term(230) + wm_interm_127_triplet_pt3(q,i,j,k) * wm_interm_20_triplet_pt3(i,j,k,p)
term(231) = term(231) + wm_interm_127_triplet_pt3(q,i,j,k) * wm_interm_20_triplet_pt3(i,j,p,k)
term(232) = term(232) + wm_interm_119_triplet_pt3(q,i,j,k) * wm_interm_57_triplet_pt3(i,j,k,p)
term(233) = term(233) + wm_interm_119_triplet_pt3(q,i,j,k) * wm_interm_57_triplet_pt3(j,i,p,k)
term(234) = term(234) + wm_interm_119_triplet_pt3(q,i,j,k) * wm_interm_57_triplet_pt3(i,j,p,k)
term(235) = term(235) + wm_interm_127_triplet_pt3(q,i,j,k) * wm_interm_57_triplet_pt3(j,i,k,p)
term(236) = term(236) + wm_interm_127_triplet_pt3(q,i,j,k) * wm_interm_57_triplet_pt3(i,j,k,p)
term(237) = term(237) + wm_interm_127_triplet_pt3(q,i,j,k) * wm_interm_57_triplet_pt3(i,j,p,k)
term(238) = term(238) + wm_interm_100_triplet_pt3(q,i,j,k) * wm_interm_111_triplet_pt3(j,k,i,p)
term(239) = term(239) + wm_interm_100_triplet_pt3(q,i,j,k) * wm_interm_112_triplet_pt3(j,k,i,p)
term(240) = term(240) + wm_interm_100_triplet_pt3(q,i,j,k) * wm_interm_111_triplet_pt3(k,j,i,p)
term(241) = term(241) + wm_interm_100_triplet_pt3(q,i,j,k) * wm_interm_133_triplet_pt3(j,k,i,p)
term(242) = term(242) + wm_interm_100_triplet_pt3(q,i,j,k) * wm_interm_133_triplet_pt3(k,j,i,p)
end do 
end do 
end do 

term(210) = term(210) * (-0.5d+0) 
term(211) = term(211) * (-0.5d+0) 
term(214) = term(214) * (-0.5d+0) 
term(215) = term(215) * (-1.0d+0) 
term(216) = term(216) * (-1.0d+0) 
term(217) = term(217) * (2.0d+0) 
term(218) = term(218) * (-1.0d+0) 
term(219) = term(219) * (2.0d+0) 
term(220) = term(220) * (-1.0d+0) 
term(222) = term(222) * (-0.5d+0) 
term(223) = term(223) * (-0.5d+0) 
term(224) = term(224) * (2.0d+0) 
term(225) = term(225) * (-2.0d+0) 
term(226) = term(226) * (-1.0d+0) 
term(227) = term(227) * (-1.0d+0) 
term(228) = term(228) * (2.0d+0) 
term(229) = term(229) * (-1.0d+0) 
term(230) = term(230) * (2.0d+0) 
term(231) = term(231) * (-1.0d+0) 
term(232) = term(232) * (-2.0d+0) 
term(233) = term(233) * (-2.0d+0) 
term(234) = term(234) * (4.0d+0) 
term(235) = term(235) * (-2.0d+0) 
term(236) = term(236) * (4.0d+0) 
term(237) = term(237) * (-2.0d+0) 
term(238) = term(238) * (2.0d+0) 
term(239) = term(239) * (-1.0d+0) 
term(240) = term(240) * (-1.0d+0) 
term(241) = term(241) * (4.0d+0) 
term(242) = term(242) * (-4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
term(243) = term(243) + r1(vrdav_Rl, q,j) * wm_interm_17_triplet_pt3(i,p) * wm_interm_196_triplet_pt3(i,j)
term(244) = term(244) + r1(vrdav_Rl, q,j) * wm_interm_17_triplet_pt3(i,p) * wm_interm_197_triplet_pt3(i,j)
term(245) = term(245) + r1(vrdav_Rl, q,j) * wm_interm_18_triplet_pt3(i,p) * wm_interm_196_triplet_pt3(i,j)
term(246) = term(246) + r1(vrdav_Rl, q,j) * wm_interm_18_triplet_pt3(i,p) * wm_interm_197_triplet_pt3(i,j)
term(247) = term(247) + r1(vrdav_Rl, q,j) * wm_interm_196_triplet_pt3(i,j) * wm_interm_19_triplet_pt3(i,p)
term(248) = term(248) + r1(vrdav_Rl, q,j) * wm_interm_197_triplet_pt3(i,j) * wm_interm_19_triplet_pt3(i,p)
term(249) = term(249) + r1(vrdav_Rl, q,j) * wm_interm_196_triplet_pt3(i,j) * wm_interm_54_triplet_pt3(i,p)
term(250) = term(250) + r1(vrdav_Rl, q,j) * wm_interm_197_triplet_pt3(i,j) * wm_interm_54_triplet_pt3(i,p)
term(251) = term(251) + r1(vrdav_Rl, q,j) * wm_interm_196_triplet_pt3(i,j) * wm_interm_55_triplet_pt3(i,p)
term(252) = term(252) + r1(vrdav_Rl, q,j) * wm_interm_197_triplet_pt3(i,j) * wm_interm_55_triplet_pt3(i,p)
term(253) = term(253) + r1(vrdav_Rl, q,j) * wm_interm_196_triplet_pt3(i,j) * wm_interm_56_triplet_pt3(i,p)
term(254) = term(254) + r1(vrdav_Rl, q,j) * wm_interm_197_triplet_pt3(i,j) * wm_interm_56_triplet_pt3(i,p)
end do 
end do 

term(243) = term(243) * (-1.0d+0) 
term(244) = term(244) * (2.0d+0) 
term(245) = term(245) * (2.0d+0) 
term(246) = term(246) * (-4.0d+0) 
term(247) = term(247) * (-1.0d+0) 
term(248) = term(248) * (2.0d+0) 
term(249) = term(249) * (-2.0d+0) 
term(250) = term(250) * (4.0d+0) 
term(251) = term(251) * (4.0d+0) 
term(252) = term(252) * (-8.0d+0) 
term(253) = term(253) * (-2.0d+0) 
term(254) = term(254) * (4.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(255) = term(255) + t2(a,q,j,i) * wm_interm_171_triplet_pt3(b,j,i,p) * wm_interm_198_triplet_pt3(a,b)
term(256) = term(256) + t2(a,q,j,i) * wm_interm_171_triplet_pt3(b,j,i,p) * wm_interm_199_triplet_pt3(a,b)
term(257) = term(257) + t2(a,q,j,i) * wm_interm_179_triplet_pt3(b,j,i,p) * wm_interm_198_triplet_pt3(a,b)
term(258) = term(258) + t2(a,q,j,i) * wm_interm_179_triplet_pt3(b,j,i,p) * wm_interm_199_triplet_pt3(a,b)
end do 
end do 
end do 
end do 

term(255) = term(255) * (-1.0d+0) 
term(256) = term(256) * (2.0d+0) 
term(257) = term(257) * (-2.0d+0) 
term(258) = term(258) * (4.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(259) = term(259) + r1(vrdav_Rl, q,i) * wm_interm_190_triplet_pt3(a,b,j,i) * wm_interm_30_triplet_pt3(a,b,j,p)
term(260) = term(260) + r1(vrdav_Rl, q,i) * wm_interm_190_triplet_pt3(a,b,j,i) * wm_interm_25_triplet_pt3(a,b,j,p)
term(261) = term(261) + r1(vrdav_Rl, q,i) * wm_interm_190_triplet_pt3(a,b,j,i) * wm_interm_24_triplet_pt3(a,b,j,p)
term(262) = term(262) + r1(vrdav_Rl, q,i) * wm_interm_192_triplet_pt3(a,b,j,i) * wm_interm_21_triplet_pt3(a,b,j,p)
term(263) = term(263) + r1(vrdav_Rl, q,i) * wm_interm_192_triplet_pt3(a,b,j,i) * wm_interm_22_triplet_pt3(a,b,j,p)
term(264) = term(264) + r1(vrdav_Rl, q,i) * wm_interm_192_triplet_pt3(a,b,j,i) * wm_interm_26_triplet_pt3(a,b,j,p)
term(265) = term(265) + r1(vrdav_Rl, q,i) * wm_interm_192_triplet_pt3(a,b,j,i) * wm_interm_23_triplet_pt3(a,b,j,p)
term(266) = term(266) + r1(vrdav_Rl, q,i) * wm_interm_192_triplet_pt3(a,b,j,i) * wm_interm_25_triplet_pt3(a,b,j,p)
term(267) = term(267) + r1(vrdav_Rl, q,i) * wm_interm_192_triplet_pt3(a,b,j,i) * wm_interm_30_triplet_pt3(a,b,j,p)
term(268) = term(268) + r1(vrdav_Rl, q,i) * wm_interm_193_triplet_pt3(a,b,j,i) * wm_interm_24_triplet_pt3(a,b,j,p)
term(269) = term(269) + r1(vrdav_Rl, q,i) * wm_interm_193_triplet_pt3(a,b,j,i) * wm_interm_30_triplet_pt3(a,b,j,p)
term(270) = term(270) + r1(vrdav_Rl, q,i) * wm_interm_193_triplet_pt3(a,b,j,i) * wm_interm_25_triplet_pt3(a,b,j,p)
term(271) = term(271) + r1(vrdav_Rl, q,i) * wm_interm_194_triplet_pt3(a,b,j,i) * wm_interm_24_triplet_pt3(a,b,j,p)
term(272) = term(272) + r1(vrdav_Rl, q,i) * wm_interm_194_triplet_pt3(a,b,j,i) * wm_interm_30_triplet_pt3(a,b,j,p)
term(273) = term(273) + r1(vrdav_Rl, q,i) * wm_interm_194_triplet_pt3(a,b,j,i) * wm_interm_25_triplet_pt3(a,b,j,p)
term(274) = term(274) + r1(vrdav_Rl, q,i) * wm_interm_190_triplet_pt3(a,b,j,i) * wm_interm_61_triplet_pt3(a,b,j,p)
term(275) = term(275) + r1(vrdav_Rl, q,i) * wm_interm_190_triplet_pt3(a,b,j,i) * wm_interm_60_triplet_pt3(a,b,j,p)
term(276) = term(276) + r1(vrdav_Rl, q,i) * wm_interm_192_triplet_pt3(a,b,j,i) * wm_interm_58_triplet_pt3(a,b,j,p)
term(277) = term(277) + r1(vrdav_Rl, q,i) * wm_interm_192_triplet_pt3(a,b,j,i) * wm_interm_59_triplet_pt3(a,b,j,p)
term(278) = term(278) + r1(vrdav_Rl, q,i) * wm_interm_192_triplet_pt3(a,b,j,i) * wm_interm_60_triplet_pt3(a,b,j,p)
term(279) = term(279) + r1(vrdav_Rl, q,i) * wm_interm_192_triplet_pt3(a,b,j,i) * wm_interm_61_triplet_pt3(a,b,j,p)
term(280) = term(280) + r1(vrdav_Rl, q,i) * wm_interm_193_triplet_pt3(a,b,j,i) * wm_interm_61_triplet_pt3(a,b,j,p)
term(281) = term(281) + r1(vrdav_Rl, q,i) * wm_interm_193_triplet_pt3(a,b,j,i) * wm_interm_60_triplet_pt3(a,b,j,p)
term(282) = term(282) + r1(vrdav_Rl, q,i) * wm_interm_194_triplet_pt3(a,b,j,i) * wm_interm_61_triplet_pt3(a,b,j,p)
term(283) = term(283) + r1(vrdav_Rl, q,i) * wm_interm_194_triplet_pt3(a,b,j,i) * wm_interm_60_triplet_pt3(a,b,j,p)
term(284) = term(284) + t2(a,q,j,i) * wm_interm_171_triplet_pt3(b,i,j,p) * wm_interm_198_triplet_pt3(a,b)
term(285) = term(285) + t2(a,q,j,i) * wm_interm_171_triplet_pt3(b,i,j,p) * wm_interm_199_triplet_pt3(a,b)
term(286) = term(286) + t2(a,q,j,i) * wm_interm_175_triplet_pt3(b,i,j,p) * wm_interm_198_triplet_pt3(a,b)
term(287) = term(287) + t2(a,q,j,i) * wm_interm_175_triplet_pt3(b,i,j,p) * wm_interm_199_triplet_pt3(a,b)
term(288) = term(288) + t2(a,q,j,i) * wm_interm_185_triplet_pt3(b,p,j,i) * wm_interm_74_triplet_pt3(a,b)
term(289) = term(289) + t2(a,q,j,i) * wm_interm_185_triplet_pt3(b,p,j,i) * wm_interm_78_triplet_pt3(a,b)
term(290) = term(290) + t2(a,q,j,i) * wm_interm_185_triplet_pt3(b,p,j,i) * wm_interm_80_triplet_pt3(a,b)
term(291) = term(291) + t2(a,q,j,i) * wm_interm_179_triplet_pt3(b,i,j,p) * wm_interm_198_triplet_pt3(a,b)
term(292) = term(292) + t2(a,q,j,i) * wm_interm_179_triplet_pt3(b,i,j,p) * wm_interm_199_triplet_pt3(a,b)
term(293) = term(293) + t2(a,q,j,i) * wm_interm_182_triplet_pt3(b,i,j,p) * wm_interm_198_triplet_pt3(a,b)
term(294) = term(294) + t2(a,q,j,i) * wm_interm_182_triplet_pt3(b,i,j,p) * wm_interm_199_triplet_pt3(a,b)
term(295) = term(295) + t2(a,q,j,i) * wm_interm_144_triplet_pt3(a,b) * wm_interm_185_triplet_pt3(b,p,j,i)
term(296) = term(296) + t2(a,q,j,i) * wm_interm_146_triplet_pt3(a,b) * wm_interm_185_triplet_pt3(b,p,j,i)
end do 
end do 
end do 
end do 

term(259) = term(259) * (2.0d+0) 
term(260) = term(260) * (-4.0d+0) 
term(261) = term(261) * (2.0d+0) 
term(262) = term(262) * (2.0d+0) 
term(263) = term(263) * (-4.0d+0) 
term(264) = term(264) * (2.0d+0) 
term(265) = term(265) * (2.0d+0) 
term(266) = term(266) * (2.0d+0) 
term(267) = term(267) * (-4.0d+0) 
term(268) = term(268) * (2.0d+0) 
term(269) = term(269) * (2.0d+0) 
term(270) = term(270) * (-4.0d+0) 
term(271) = term(271) * (-4.0d+0) 
term(272) = term(272) * (-4.0d+0) 
term(273) = term(273) * (8.0d+0) 
term(274) = term(274) * (8.0d+0) 
term(275) = term(275) * (-8.0d+0) 
term(276) = term(276) * (8.0d+0) 
term(277) = term(277) * (-8.0d+0) 
term(278) = term(278) * (8.0d+0) 
term(279) = term(279) * (-8.0d+0) 
term(280) = term(280) * (8.0d+0) 
term(281) = term(281) * (-8.0d+0) 
term(282) = term(282) * (-16.0d+0) 
term(283) = term(283) * (16.0d+0) 
term(284) = term(284) * (2.0d+0) 
term(285) = term(285) * (-4.0d+0) 
term(286) = term(286) * (-1.0d+0) 
term(287) = term(287) * (2.0d+0) 
term(288) = term(288) * (-1.0d+0) 
term(289) = term(289) * (2.0d+0) 
term(290) = term(290) * (-1.0d+0) 
term(291) = term(291) * (4.0d+0) 
term(292) = term(292) * (-8.0d+0) 
term(293) = term(293) * (-2.0d+0) 
term(294) = term(294) * (4.0d+0) 
term(295) = term(295) * (-4.0d+0) 
term(296) = term(296) * (4.0d+0) 

do a = nocc + 1, nactive 
term(297) = term(297) + wm_interm_80_triplet_pt3(a,q) * wm_interm_87_triplet_pt3(a,p)
term(298) = term(298) + wm_interm_74_triplet_pt3(a,q) * wm_interm_87_triplet_pt3(a,p)
term(299) = term(299) + wm_interm_78_triplet_pt3(a,q) * wm_interm_87_triplet_pt3(a,p)
term(300) = term(300) + wm_interm_80_triplet_pt3(a,q) * wm_interm_88_triplet_pt3(a,p)
term(301) = term(301) + wm_interm_74_triplet_pt3(a,q) * wm_interm_88_triplet_pt3(a,p)
term(302) = term(302) + wm_interm_78_triplet_pt3(a,q) * wm_interm_88_triplet_pt3(a,p)
term(303) = term(303) + wm_interm_80_triplet_pt3(a,q) * wm_interm_89_triplet_pt3(a,p)
term(304) = term(304) + wm_interm_74_triplet_pt3(a,q) * wm_interm_89_triplet_pt3(a,p)
term(305) = term(305) + wm_interm_78_triplet_pt3(a,q) * wm_interm_89_triplet_pt3(a,p)
term(306) = term(306) + wm_interm_80_triplet_pt3(a,q) * wm_interm_96_triplet_pt3(a,p)
term(307) = term(307) + wm_interm_74_triplet_pt3(a,q) * wm_interm_96_triplet_pt3(a,p)
term(308) = term(308) + wm_interm_78_triplet_pt3(a,q) * wm_interm_96_triplet_pt3(a,p)
term(309) = term(309) + wm_interm_80_triplet_pt3(a,q) * wm_interm_97_triplet_pt3(a,p)
term(310) = term(310) + wm_interm_74_triplet_pt3(a,q) * wm_interm_97_triplet_pt3(a,p)
term(311) = term(311) + wm_interm_78_triplet_pt3(a,q) * wm_interm_97_triplet_pt3(a,p)
term(312) = term(312) + wm_interm_144_triplet_pt3(a,q) * wm_interm_87_triplet_pt3(a,p)
term(313) = term(313) + wm_interm_146_triplet_pt3(a,q) * wm_interm_87_triplet_pt3(a,p)
term(314) = term(314) + wm_interm_144_triplet_pt3(a,q) * wm_interm_88_triplet_pt3(a,p)
term(315) = term(315) + wm_interm_146_triplet_pt3(a,q) * wm_interm_88_triplet_pt3(a,p)
term(316) = term(316) + wm_interm_144_triplet_pt3(a,q) * wm_interm_89_triplet_pt3(a,p)
term(317) = term(317) + wm_interm_146_triplet_pt3(a,q) * wm_interm_89_triplet_pt3(a,p)
term(318) = term(318) + wm_interm_144_triplet_pt3(a,q) * wm_interm_96_triplet_pt3(a,p)
term(319) = term(319) + wm_interm_146_triplet_pt3(a,q) * wm_interm_96_triplet_pt3(a,p)
term(320) = term(320) + wm_interm_144_triplet_pt3(a,q) * wm_interm_97_triplet_pt3(a,p)
term(321) = term(321) + wm_interm_146_triplet_pt3(a,q) * wm_interm_97_triplet_pt3(a,p)
term(322) = term(322) + wm_interm_160_triplet_pt3(q,a) * wm_interm_183_triplet_pt3(a,p)
end do 

term(297) = term(297) * (-1.0d+0) 
term(298) = term(298) * (-1.0d+0) 
term(299) = term(299) * (2.0d+0) 
term(300) = term(300) * (0.5d+0) 
term(301) = term(301) * (0.5d+0) 
term(302) = term(302) * (-1.0d+0) 
term(303) = term(303) * (0.5d+0) 
term(304) = term(304) * (0.5d+0) 
term(305) = term(305) * (-1.0d+0) 
term(306) = term(306) * (-2.0d+0) 
term(307) = term(307) * (-2.0d+0) 
term(308) = term(308) * (4.0d+0) 
term(309) = term(309) * (2.0d+0) 
term(310) = term(310) * (2.0d+0) 
term(311) = term(311) * (-4.0d+0) 
term(312) = term(312) * (-4.0d+0) 
term(313) = term(313) * (4.0d+0) 
term(314) = term(314) * (2.0d+0) 
term(315) = term(315) * (-2.0d+0) 
term(316) = term(316) * (2.0d+0) 
term(317) = term(317) * (-2.0d+0) 
term(318) = term(318) * (-8.0d+0) 
term(319) = term(319) * (8.0d+0) 
term(320) = term(320) * (8.0d+0) 
term(321) = term(321) * (-8.0d+0) 
term(322) = term(322) * (2.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(323) = term(323) + r1(vrdav_Rr, a,p) * wm_interm_196_triplet_pt3(i,j) * wm_interm_91_triplet_pt3(a,q,j,i)
term(324) = term(324) + r1(vrdav_Rr, a,p) * wm_interm_197_triplet_pt3(i,j) * wm_interm_91_triplet_pt3(a,q,j,i)
term(325) = term(325) + r1(vrdav_Rr, a,p) * wm_interm_196_triplet_pt3(i,j) * wm_interm_94_triplet_pt3(a,q,j,i)
term(326) = term(326) + r1(vrdav_Rr, a,p) * wm_interm_197_triplet_pt3(i,j) * wm_interm_94_triplet_pt3(a,q,j,i)
term(327) = term(327) + r1(vrdav_Rr, a,p) * wm_interm_196_triplet_pt3(i,j) * wm_interm_90_triplet_pt3(a,q,j,i)
term(328) = term(328) + r1(vrdav_Rr, a,p) * wm_interm_197_triplet_pt3(i,j) * wm_interm_90_triplet_pt3(a,q,j,i)
term(329) = term(329) + r1(vrdav_Rr, a,p) * wm_interm_196_triplet_pt3(i,j) * wm_interm_95_triplet_pt3(a,q,j,i)
term(330) = term(330) + r1(vrdav_Rr, a,p) * wm_interm_196_triplet_pt3(i,j) * wm_interm_92_triplet_pt3(a,q,j,i)
term(331) = term(331) + r1(vrdav_Rr, a,p) * wm_interm_197_triplet_pt3(i,j) * wm_interm_95_triplet_pt3(a,q,j,i)
term(332) = term(332) + r1(vrdav_Rr, a,p) * wm_interm_197_triplet_pt3(i,j) * wm_interm_92_triplet_pt3(a,q,j,i)
term(333) = term(333) + r1(vrdav_Rr, a,p) * wm_interm_145_triplet_pt3(i,j) * wm_interm_193_triplet_pt3(a,q,j,i)
term(334) = term(334) + r1(vrdav_Rr, a,p) * wm_interm_145_triplet_pt3(i,j) * wm_interm_190_triplet_pt3(a,q,j,i)
term(335) = term(335) + r1(vrdav_Rr, a,p) * wm_interm_145_triplet_pt3(i,j) * wm_interm_194_triplet_pt3(a,q,j,i)
term(336) = term(336) + r1(vrdav_Rr, a,p) * wm_interm_147_triplet_pt3(i,j) * wm_interm_193_triplet_pt3(a,q,j,i)
term(337) = term(337) + r1(vrdav_Rr, a,p) * wm_interm_147_triplet_pt3(i,j) * wm_interm_190_triplet_pt3(a,q,j,i)
term(338) = term(338) + r1(vrdav_Rr, a,p) * wm_interm_147_triplet_pt3(i,j) * wm_interm_194_triplet_pt3(a,q,j,i)
end do 
end do 
end do 

term(323) = term(323) * (-1.0d+0) 
term(324) = term(324) * (2.0d+0) 
term(325) = term(325) * (2.0d+0) 
term(326) = term(326) * (-4.0d+0) 
term(327) = term(327) * (-1.0d+0) 
term(328) = term(328) * (2.0d+0) 
term(329) = term(329) * (-1.0d+0) 
term(330) = term(330) * (2.0d+0) 
term(331) = term(331) * (2.0d+0) 
term(332) = term(332) * (-4.0d+0) 
term(333) = term(333) * (-4.0d+0) 
term(334) = term(334) * (-4.0d+0) 
term(335) = term(335) * (8.0d+0) 
term(336) = term(336) * (4.0d+0) 
term(337) = term(337) * (4.0d+0) 
term(338) = term(338) * (-8.0d+0) 

do l = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
term(339) = term(339) + r1(vrdav_Rl, q,i) * wm_interm_191_triplet_pt3(j,k,i,l) * wm_interm_20_triplet_pt3(j,k,l,p)
term(340) = term(340) + r1(vrdav_Rl, q,i) * wm_interm_191_triplet_pt3(j,k,i,l) * wm_interm_20_triplet_pt3(k,j,p,l)
term(341) = term(341) + r1(vrdav_Rl, q,i) * wm_interm_191_triplet_pt3(j,k,i,l) * wm_interm_20_triplet_pt3(j,k,p,l)
term(342) = term(342) + r1(vrdav_Rl, q,i) * wm_interm_191_triplet_pt3(j,k,i,l) * wm_interm_57_triplet_pt3(j,k,l,p)
term(343) = term(343) + r1(vrdav_Rl, q,i) * wm_interm_191_triplet_pt3(j,k,i,l) * wm_interm_57_triplet_pt3(k,j,p,l)
term(344) = term(344) + r1(vrdav_Rl, q,i) * wm_interm_191_triplet_pt3(j,k,i,l) * wm_interm_57_triplet_pt3(j,k,p,l)
end do 
end do 
end do 
end do 

term(339) = term(339) * (2.0d+0) 
term(340) = term(340) * (2.0d+0) 
term(341) = term(341) * (-4.0d+0) 
term(342) = term(342) * (4.0d+0) 
term(343) = term(343) * (4.0d+0) 
term(344) = term(344) * (-8.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(345) = term(345) + r1(vrdav_Rl, q,i) * wm_interm_190_triplet_pt3(a,b,p,i) * wm_interm_9_triplet_pt3(a,b)
term(346) = term(346) + r1(vrdav_Rl, q,i) * wm_interm_190_triplet_pt3(a,b,p,i) * wm_interm_5_triplet_pt3(a,b)
term(347) = term(347) + r1(vrdav_Rl, q,i) * wm_interm_190_triplet_pt3(a,b,p,i) * wm_interm_6_triplet_pt3(a,b)
term(348) = term(348) + r1(vrdav_Rl, q,i) * wm_interm_193_triplet_pt3(a,b,p,i) * wm_interm_5_triplet_pt3(a,b)
term(349) = term(349) + r1(vrdav_Rl, q,i) * wm_interm_193_triplet_pt3(a,b,p,i) * wm_interm_6_triplet_pt3(a,b)
term(350) = term(350) + r1(vrdav_Rl, q,i) * wm_interm_193_triplet_pt3(a,b,p,i) * wm_interm_9_triplet_pt3(a,b)
term(351) = term(351) + r1(vrdav_Rl, q,i) * wm_interm_194_triplet_pt3(a,b,p,i) * wm_interm_5_triplet_pt3(a,b)
term(352) = term(352) + r1(vrdav_Rl, q,i) * wm_interm_194_triplet_pt3(a,b,p,i) * wm_interm_6_triplet_pt3(a,b)
term(353) = term(353) + r1(vrdav_Rl, q,i) * wm_interm_194_triplet_pt3(a,b,p,i) * wm_interm_9_triplet_pt3(a,b)
term(354) = term(354) + r1(vrdav_Rl, q,i) * wm_interm_190_triplet_pt3(a,b,p,i) * wm_interm_195_triplet_pt3(a,b)
term(355) = term(355) + r1(vrdav_Rl, q,i) * wm_interm_190_triplet_pt3(a,b,p,i) * wm_interm_47_triplet_pt3(a,b)
term(356) = term(356) + r1(vrdav_Rl, q,i) * wm_interm_190_triplet_pt3(a,b,p,i) * wm_interm_48_triplet_pt3(a,b)
term(357) = term(357) + r1(vrdav_Rl, q,i) * wm_interm_193_triplet_pt3(a,b,p,i) * wm_interm_47_triplet_pt3(a,b)
term(358) = term(358) + r1(vrdav_Rl, q,i) * wm_interm_193_triplet_pt3(a,b,p,i) * wm_interm_48_triplet_pt3(a,b)
term(359) = term(359) + r1(vrdav_Rl, q,i) * wm_interm_193_triplet_pt3(a,b,p,i) * wm_interm_195_triplet_pt3(a,b)
term(360) = term(360) + r1(vrdav_Rl, q,i) * wm_interm_194_triplet_pt3(a,b,p,i) * wm_interm_47_triplet_pt3(a,b)
term(361) = term(361) + r1(vrdav_Rl, q,i) * wm_interm_194_triplet_pt3(a,b,p,i) * wm_interm_48_triplet_pt3(a,b)
term(362) = term(362) + r1(vrdav_Rl, q,i) * wm_interm_194_triplet_pt3(a,b,p,i) * wm_interm_195_triplet_pt3(a,b)
end do 
end do 
end do 

term(345) = term(345) * (2.0d+0) 
term(346) = term(346) * (2.0d+0) 
term(347) = term(347) * (-4.0d+0) 
term(348) = term(348) * (2.0d+0) 
term(349) = term(349) * (-4.0d+0) 
term(350) = term(350) * (2.0d+0) 
term(351) = term(351) * (-4.0d+0) 
term(352) = term(352) * (8.0d+0) 
term(353) = term(353) * (-4.0d+0) 
term(354) = term(354) * (4.0d+0) 
term(355) = term(355) * (4.0d+0) 
term(356) = term(356) * (-8.0d+0) 
term(357) = term(357) * (4.0d+0) 
term(358) = term(358) * (-8.0d+0) 
term(359) = term(359) * (4.0d+0) 
term(360) = term(360) * (-8.0d+0) 
term(361) = term(361) * (16.0d+0) 
term(362) = term(362) * (-8.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(363) = term(363) + wm_interm_20_triplet_pt3(i,j,k,p) * wm_interm_41_triplet_pt3(q,j,i,k)
term(364) = term(364) + wm_interm_100_triplet_pt3(q,i,j,k) * wm_interm_7_triplet_pt3(j,k,p,i)
term(365) = term(365) + wm_interm_100_triplet_pt3(q,i,j,k) * wm_interm_7_triplet_pt3(k,j,p,i)
term(366) = term(366) + wm_interm_100_triplet_pt3(q,i,j,k) * wm_interm_8_triplet_pt3(k,j,p,i)
term(367) = term(367) + wm_interm_100_triplet_pt3(q,i,j,k) * wm_interm_49_triplet_pt3(j,k,p,i)
term(368) = term(368) + wm_interm_100_triplet_pt3(q,i,j,k) * wm_interm_49_triplet_pt3(k,j,p,i)
term(369) = term(369) + wm_interm_100_triplet_pt3(q,i,j,k) * wm_interm_111_triplet_pt3(j,k,p,i)
term(370) = term(370) + wm_interm_100_triplet_pt3(q,i,j,k) * wm_interm_111_triplet_pt3(k,j,p,i)
term(371) = term(371) + wm_interm_100_triplet_pt3(q,i,j,k) * wm_interm_112_triplet_pt3(k,j,p,i)
term(372) = term(372) + wm_interm_100_triplet_pt3(q,i,j,k) * wm_interm_133_triplet_pt3(j,k,p,i)
term(373) = term(373) + wm_interm_100_triplet_pt3(q,i,j,k) * wm_interm_133_triplet_pt3(k,j,p,i)
term(374) = term(374) + r1(vrdav_Rl, q,i) * wm_interm_17_triplet_pt3(j,k) * wm_interm_191_triplet_pt3(j,p,i,k)
term(375) = term(375) + r1(vrdav_Rl, q,i) * wm_interm_18_triplet_pt3(j,k) * wm_interm_191_triplet_pt3(j,p,i,k)
term(376) = term(376) + r1(vrdav_Rl, q,i) * wm_interm_191_triplet_pt3(j,p,i,k) * wm_interm_19_triplet_pt3(j,k)
term(377) = term(377) + r1(vrdav_Rl, q,i) * wm_interm_191_triplet_pt3(j,p,i,k) * wm_interm_54_triplet_pt3(j,k)
term(378) = term(378) + r1(vrdav_Rl, q,i) * wm_interm_191_triplet_pt3(j,p,i,k) * wm_interm_55_triplet_pt3(j,k)
term(379) = term(379) + r1(vrdav_Rl, q,i) * wm_interm_191_triplet_pt3(j,p,i,k) * wm_interm_56_triplet_pt3(j,k)
end do 
end do 
end do 

term(363) = term(363) * (-0.5d+0) 
term(364) = term(364) * (-0.5d+0) 
term(366) = term(366) * (-0.5d+0) 
term(367) = term(367) * (-2.0d+0) 
term(368) = term(368) * (2.0d+0) 
term(369) = term(369) * (-1.0d+0) 
term(370) = term(370) * (2.0d+0) 
term(371) = term(371) * (-1.0d+0) 
term(372) = term(372) * (-4.0d+0) 
term(373) = term(373) * (4.0d+0) 
term(374) = term(374) * (2.0d+0) 
term(375) = term(375) * (-4.0d+0) 
term(376) = term(376) * (2.0d+0) 
term(377) = term(377) * (4.0d+0) 
term(378) = term(378) * (-8.0d+0) 
term(379) = term(379) * (4.0d+0) 

do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(380) = term(380) + r1(vrdav_Rr, a,p) * wm_interm_198_triplet_pt3(a,b) * wm_interm_80_triplet_pt3(b,q)
term(381) = term(381) + r1(vrdav_Rr, a,p) * wm_interm_199_triplet_pt3(a,b) * wm_interm_80_triplet_pt3(b,q)
term(382) = term(382) + r1(vrdav_Rr, a,p) * wm_interm_198_triplet_pt3(a,b) * wm_interm_74_triplet_pt3(b,q)
term(383) = term(383) + r1(vrdav_Rr, a,p) * wm_interm_198_triplet_pt3(a,b) * wm_interm_78_triplet_pt3(b,q)
term(384) = term(384) + r1(vrdav_Rr, a,p) * wm_interm_199_triplet_pt3(a,b) * wm_interm_74_triplet_pt3(b,q)
term(385) = term(385) + r1(vrdav_Rr, a,p) * wm_interm_199_triplet_pt3(a,b) * wm_interm_78_triplet_pt3(b,q)
term(386) = term(386) + r1(vrdav_Rr, a,p) * wm_interm_198_triplet_pt3(b,q) * wm_interm_74_triplet_pt3(a,b)
term(387) = term(387) + r1(vrdav_Rr, a,p) * wm_interm_199_triplet_pt3(b,q) * wm_interm_74_triplet_pt3(a,b)
term(388) = term(388) + r1(vrdav_Rr, a,p) * wm_interm_198_triplet_pt3(b,q) * wm_interm_78_triplet_pt3(a,b)
term(389) = term(389) + r1(vrdav_Rr, a,p) * wm_interm_199_triplet_pt3(b,q) * wm_interm_78_triplet_pt3(a,b)
term(390) = term(390) + r1(vrdav_Rr, a,p) * wm_interm_198_triplet_pt3(b,q) * wm_interm_80_triplet_pt3(a,b)
term(391) = term(391) + r1(vrdav_Rr, a,p) * wm_interm_199_triplet_pt3(b,q) * wm_interm_80_triplet_pt3(a,b)
term(392) = term(392) + r1(vrdav_Rr, a,p) * wm_interm_144_triplet_pt3(b,q) * wm_interm_198_triplet_pt3(a,b)
term(393) = term(393) + r1(vrdav_Rr, a,p) * wm_interm_144_triplet_pt3(b,q) * wm_interm_199_triplet_pt3(a,b)
term(394) = term(394) + r1(vrdav_Rr, a,p) * wm_interm_146_triplet_pt3(b,q) * wm_interm_198_triplet_pt3(a,b)
term(395) = term(395) + r1(vrdav_Rr, a,p) * wm_interm_146_triplet_pt3(b,q) * wm_interm_199_triplet_pt3(a,b)
term(396) = term(396) + r1(vrdav_Rr, a,p) * wm_interm_144_triplet_pt3(a,b) * wm_interm_198_triplet_pt3(b,q)
term(397) = term(397) + r1(vrdav_Rr, a,p) * wm_interm_144_triplet_pt3(a,b) * wm_interm_199_triplet_pt3(b,q)
term(398) = term(398) + r1(vrdav_Rr, a,p) * wm_interm_146_triplet_pt3(a,b) * wm_interm_198_triplet_pt3(b,q)
term(399) = term(399) + r1(vrdav_Rr, a,p) * wm_interm_146_triplet_pt3(a,b) * wm_interm_199_triplet_pt3(b,q)
end do 
end do 

term(380) = term(380) * (-1.0d+0) 
term(381) = term(381) * (2.0d+0) 
term(382) = term(382) * (-1.0d+0) 
term(383) = term(383) * (2.0d+0) 
term(384) = term(384) * (2.0d+0) 
term(385) = term(385) * (-4.0d+0) 
term(386) = term(386) * (-1.0d+0) 
term(387) = term(387) * (2.0d+0) 
term(388) = term(388) * (2.0d+0) 
term(389) = term(389) * (-4.0d+0) 
term(390) = term(390) * (-1.0d+0) 
term(391) = term(391) * (2.0d+0) 
term(392) = term(392) * (-4.0d+0) 
term(393) = term(393) * (8.0d+0) 
term(394) = term(394) * (4.0d+0) 
term(395) = term(395) * (-8.0d+0) 
term(396) = term(396) * (-4.0d+0) 
term(397) = term(397) * (8.0d+0) 
term(398) = term(398) * (4.0d+0) 
term(399) = term(399) * (-8.0d+0) 

do i = 1, nocc 
term(400) = term(400) + wm_interm_17_triplet_pt3(i,p) * wm_interm_84_triplet_pt3(q,i)
term(401) = term(401) + wm_interm_18_triplet_pt3(i,p) * wm_interm_84_triplet_pt3(q,i)
term(402) = term(402) + wm_interm_19_triplet_pt3(i,p) * wm_interm_84_triplet_pt3(q,i)
term(403) = term(403) + wm_interm_17_triplet_pt3(i,p) * wm_interm_85_triplet_pt3(q,i)
term(404) = term(404) + wm_interm_18_triplet_pt3(i,p) * wm_interm_85_triplet_pt3(q,i)
term(405) = term(405) + wm_interm_19_triplet_pt3(i,p) * wm_interm_85_triplet_pt3(q,i)
term(406) = term(406) + wm_interm_17_triplet_pt3(i,p) * wm_interm_86_triplet_pt3(q,i)
term(407) = term(407) + wm_interm_18_triplet_pt3(i,p) * wm_interm_86_triplet_pt3(q,i)
term(408) = term(408) + wm_interm_19_triplet_pt3(i,p) * wm_interm_86_triplet_pt3(q,i)
term(409) = term(409) + wm_interm_54_triplet_pt3(i,p) * wm_interm_84_triplet_pt3(q,i)
term(410) = term(410) + wm_interm_55_triplet_pt3(i,p) * wm_interm_84_triplet_pt3(q,i)
term(411) = term(411) + wm_interm_56_triplet_pt3(i,p) * wm_interm_84_triplet_pt3(q,i)
term(412) = term(412) + wm_interm_54_triplet_pt3(i,p) * wm_interm_85_triplet_pt3(q,i)
term(413) = term(413) + wm_interm_55_triplet_pt3(i,p) * wm_interm_85_triplet_pt3(q,i)
term(414) = term(414) + wm_interm_56_triplet_pt3(i,p) * wm_interm_85_triplet_pt3(q,i)
term(415) = term(415) + wm_interm_54_triplet_pt3(i,p) * wm_interm_86_triplet_pt3(q,i)
term(416) = term(416) + wm_interm_55_triplet_pt3(i,p) * wm_interm_86_triplet_pt3(q,i)
term(417) = term(417) + wm_interm_56_triplet_pt3(i,p) * wm_interm_86_triplet_pt3(q,i)
term(418) = term(418) + wm_interm_36_triplet_pt3(i,p) * wm_interm_73_triplet_pt3(q,i)
term(419) = term(419) + wm_interm_37_triplet_pt3(i,p) * wm_interm_73_triplet_pt3(q,i)
term(420) = term(420) + wm_interm_38_triplet_pt3(i,p) * wm_interm_73_triplet_pt3(q,i)
term(421) = term(421) + wm_interm_36_triplet_pt3(i,p) * wm_interm_72_triplet_pt3(q,i)
term(422) = term(422) + wm_interm_37_triplet_pt3(i,p) * wm_interm_72_triplet_pt3(q,i)
term(423) = term(423) + wm_interm_38_triplet_pt3(i,p) * wm_interm_72_triplet_pt3(q,i)
term(424) = term(424) + wm_interm_12_triplet_pt3(i,p) * wm_interm_73_triplet_pt3(q,i)
term(425) = term(425) + wm_interm_13_triplet_pt3(i,p) * wm_interm_73_triplet_pt3(q,i)
term(426) = term(426) + wm_interm_12_triplet_pt3(i,p) * wm_interm_72_triplet_pt3(q,i)
term(427) = term(427) + wm_interm_13_triplet_pt3(i,p) * wm_interm_72_triplet_pt3(q,i)
term(428) = term(428) + wm_interm_15_triplet_pt3(i,p) * wm_interm_73_triplet_pt3(q,i)
term(429) = term(429) + wm_interm_15_triplet_pt3(i,p) * wm_interm_72_triplet_pt3(q,i)
term(430) = term(430) + wm_interm_68_triplet_pt3(i,p) * wm_interm_73_triplet_pt3(q,i)
term(431) = term(431) + wm_interm_69_triplet_pt3(i,p) * wm_interm_73_triplet_pt3(q,i)
term(432) = term(432) + wm_interm_68_triplet_pt3(i,p) * wm_interm_72_triplet_pt3(q,i)
term(433) = term(433) + wm_interm_69_triplet_pt3(i,p) * wm_interm_72_triplet_pt3(q,i)
term(434) = term(434) + wm_interm_52_triplet_pt3(i,p) * wm_interm_73_triplet_pt3(q,i)
term(435) = term(435) + wm_interm_53_triplet_pt3(i,p) * wm_interm_73_triplet_pt3(q,i)
term(436) = term(436) + wm_interm_52_triplet_pt3(i,p) * wm_interm_72_triplet_pt3(q,i)
term(437) = term(437) + wm_interm_53_triplet_pt3(i,p) * wm_interm_72_triplet_pt3(q,i)
term(438) = term(438) + wm_interm_148_triplet_pt3(q,i) * wm_interm_17_triplet_pt3(i,p)
term(439) = term(439) + wm_interm_148_triplet_pt3(q,i) * wm_interm_18_triplet_pt3(i,p)
term(440) = term(440) + wm_interm_148_triplet_pt3(q,i) * wm_interm_19_triplet_pt3(i,p)
term(441) = term(441) + wm_interm_149_triplet_pt3(q,i) * wm_interm_17_triplet_pt3(i,p)
term(442) = term(442) + wm_interm_149_triplet_pt3(q,i) * wm_interm_18_triplet_pt3(i,p)
term(443) = term(443) + wm_interm_149_triplet_pt3(q,i) * wm_interm_19_triplet_pt3(i,p)
term(444) = term(444) + wm_interm_148_triplet_pt3(q,i) * wm_interm_54_triplet_pt3(i,p)
term(445) = term(445) + wm_interm_148_triplet_pt3(q,i) * wm_interm_55_triplet_pt3(i,p)
term(446) = term(446) + wm_interm_148_triplet_pt3(q,i) * wm_interm_56_triplet_pt3(i,p)
term(447) = term(447) + wm_interm_149_triplet_pt3(q,i) * wm_interm_54_triplet_pt3(i,p)
term(448) = term(448) + wm_interm_149_triplet_pt3(q,i) * wm_interm_55_triplet_pt3(i,p)
term(449) = term(449) + wm_interm_149_triplet_pt3(q,i) * wm_interm_56_triplet_pt3(i,p)
term(450) = term(450) + wm_interm_124_triplet_pt3(i,p) * wm_interm_73_triplet_pt3(q,i)
term(451) = term(451) + wm_interm_125_triplet_pt3(i,p) * wm_interm_73_triplet_pt3(q,i)
term(452) = term(452) + wm_interm_126_triplet_pt3(i,p) * wm_interm_73_triplet_pt3(q,i)
term(453) = term(453) + wm_interm_124_triplet_pt3(i,p) * wm_interm_72_triplet_pt3(q,i)
term(454) = term(454) + wm_interm_125_triplet_pt3(i,p) * wm_interm_72_triplet_pt3(q,i)
term(455) = term(455) + wm_interm_126_triplet_pt3(i,p) * wm_interm_72_triplet_pt3(q,i)
term(456) = term(456) + wm_interm_115_triplet_pt3(i,p) * wm_interm_73_triplet_pt3(q,i)
term(457) = term(457) + wm_interm_116_triplet_pt3(i,p) * wm_interm_73_triplet_pt3(q,i)
term(458) = term(458) + wm_interm_115_triplet_pt3(i,p) * wm_interm_72_triplet_pt3(q,i)
term(459) = term(459) + wm_interm_116_triplet_pt3(i,p) * wm_interm_72_triplet_pt3(q,i)
term(460) = term(460) + wm_interm_118_triplet_pt3(i,p) * wm_interm_73_triplet_pt3(q,i)
term(461) = term(461) + wm_interm_118_triplet_pt3(i,p) * wm_interm_72_triplet_pt3(q,i)
term(462) = term(462) + wm_interm_140_triplet_pt3(i,p) * wm_interm_73_triplet_pt3(q,i)
term(463) = term(463) + wm_interm_141_triplet_pt3(i,p) * wm_interm_73_triplet_pt3(q,i)
term(464) = term(464) + wm_interm_140_triplet_pt3(i,p) * wm_interm_72_triplet_pt3(q,i)
term(465) = term(465) + wm_interm_141_triplet_pt3(i,p) * wm_interm_72_triplet_pt3(q,i)
term(466) = term(466) + wm_interm_136_triplet_pt3(i,p) * wm_interm_73_triplet_pt3(q,i)
term(467) = term(467) + wm_interm_137_triplet_pt3(i,p) * wm_interm_73_triplet_pt3(q,i)
term(468) = term(468) + wm_interm_136_triplet_pt3(i,p) * wm_interm_72_triplet_pt3(q,i)
term(469) = term(469) + wm_interm_137_triplet_pt3(i,p) * wm_interm_72_triplet_pt3(q,i)
term(470) = term(470) + wm_interm_184_triplet_pt3(i,p) * wm_interm_73_triplet_pt3(q,i)
term(471) = term(471) + wm_interm_184_triplet_pt3(i,p) * wm_interm_72_triplet_pt3(q,i)
term(472) = term(472) + wm_interm_178_triplet_pt3(p,i) * wm_interm_186_triplet_pt3(q,i)
end do 

term(401) = term(401) * (-2.0d+0) 
term(403) = term(403) * (-0.5d+0) 
term(405) = term(405) * (-0.5d+0) 
term(406) = term(406) * (-0.5d+0) 
term(408) = term(408) * (-0.5d+0) 
term(409) = term(409) * (2.0d+0) 
term(410) = term(410) * (-4.0d+0) 
term(411) = term(411) * (2.0d+0) 
term(412) = term(412) * (-1.0d+0) 
term(413) = term(413) * (2.0d+0) 
term(414) = term(414) * (-1.0d+0) 
term(415) = term(415) * (-1.0d+0) 
term(416) = term(416) * (2.0d+0) 
term(417) = term(417) * (-1.0d+0) 
term(418) = term(418) * (-0.5d+0) 
term(420) = term(420) * (-0.5d+0) 
term(422) = term(422) * (-2.0d+0) 
term(424) = term(424) * (-0.5d+0) 
term(427) = term(427) * (-2.0d+0) 
term(428) = term(428) * (-0.5d+0) 
term(430) = term(430) * (-2.0d+0) 
term(431) = term(431) * (2.0d+0) 
term(432) = term(432) * (4.0d+0) 
term(433) = term(433) * (-4.0d+0) 
term(434) = term(434) * (-2.0d+0) 
term(435) = term(435) * (2.0d+0) 
term(436) = term(436) * (4.0d+0) 
term(437) = term(437) * (-4.0d+0) 
term(438) = term(438) * (2.0d+0) 
term(439) = term(439) * (-4.0d+0) 
term(440) = term(440) * (2.0d+0) 
term(441) = term(441) * (-2.0d+0) 
term(442) = term(442) * (4.0d+0) 
term(443) = term(443) * (-2.0d+0) 
term(444) = term(444) * (4.0d+0) 
term(445) = term(445) * (-8.0d+0) 
term(446) = term(446) * (4.0d+0) 
term(447) = term(447) * (-4.0d+0) 
term(448) = term(448) * (8.0d+0) 
term(449) = term(449) * (-4.0d+0) 
term(450) = term(450) * (-1.0d+0) 
term(451) = term(451) * (2.0d+0) 
term(452) = term(452) * (-1.0d+0) 
term(453) = term(453) * (2.0d+0) 
term(454) = term(454) * (-4.0d+0) 
term(455) = term(455) * (2.0d+0) 
term(456) = term(456) * (-1.0d+0) 
term(457) = term(457) * (2.0d+0) 
term(458) = term(458) * (2.0d+0) 
term(459) = term(459) * (-4.0d+0) 
term(460) = term(460) * (-1.0d+0) 
term(461) = term(461) * (2.0d+0) 
term(462) = term(462) * (-4.0d+0) 
term(463) = term(463) * (4.0d+0) 
term(464) = term(464) * (8.0d+0) 
term(465) = term(465) * (-8.0d+0) 
term(466) = term(466) * (-4.0d+0) 
term(467) = term(467) * (4.0d+0) 
term(468) = term(468) * (8.0d+0) 
term(469) = term(469) * (-8.0d+0) 
term(470) = term(470) * (2.0d+0) 
term(471) = term(471) * (-4.0d+0) 
term(472) = term(472) * (2.0d+0) 


    calc_D_ov_wm_triplet_pt3 = zero
    do s = 0, 472
    calc_D_ov_wm_triplet_pt3 = calc_D_ov_wm_triplet_pt3 + term(s)
    end do

    end function calc_D_ov_wm_triplet_pt3
    
    function calc_D_vo_wm_triplet_pt3(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vo_wm_triplet_pt3
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , k, i, j, a, b, l 
    real(F64), dimension(0:1991) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(0) = term(0) + wm_interm_1_triplet_pt3(p,i,j,k) * wm_interm_7_triplet_pt3(q,k,j,i)
term(1) = term(1) + wm_interm_1_triplet_pt3(p,i,j,k) * wm_interm_7_triplet_pt3(k,q,j,i)
term(2) = term(2) + wm_interm_1_triplet_pt3(p,i,j,k) * wm_interm_8_triplet_pt3(k,q,j,i)
term(3) = term(3) + wm_interm_1_triplet_pt3(p,i,j,k) * wm_interm_49_triplet_pt3(q,k,j,i)
term(4) = term(4) + wm_interm_1_triplet_pt3(p,i,j,k) * wm_interm_49_triplet_pt3(k,q,j,i)
term(5) = term(5) + wm_interm_111_triplet_pt3(q,i,j,k) * wm_interm_1_triplet_pt3(p,k,j,i)
term(6) = term(6) + wm_interm_111_triplet_pt3(q,i,j,k) * wm_interm_1_triplet_pt3(p,j,k,i)
term(7) = term(7) + wm_interm_112_triplet_pt3(q,i,j,k) * wm_interm_1_triplet_pt3(p,j,k,i)
term(8) = term(8) + wm_interm_111_triplet_pt3(i,q,j,k) * wm_interm_1_triplet_pt3(p,j,k,i)
term(9) = term(9) + wm_interm_111_triplet_pt3(i,q,j,k) * wm_interm_1_triplet_pt3(p,k,j,i)
term(10) = term(10) + wm_interm_112_triplet_pt3(i,q,j,k) * wm_interm_1_triplet_pt3(p,k,j,i)
term(11) = term(11) + wm_interm_133_triplet_pt3(q,i,j,k) * wm_interm_1_triplet_pt3(p,k,j,i)
term(12) = term(12) + wm_interm_133_triplet_pt3(q,i,j,k) * wm_interm_1_triplet_pt3(p,j,k,i)
term(13) = term(13) + wm_interm_133_triplet_pt3(i,q,j,k) * wm_interm_1_triplet_pt3(p,j,k,i)
term(14) = term(14) + wm_interm_133_triplet_pt3(i,q,j,k) * wm_interm_1_triplet_pt3(p,k,j,i)
term(15) = term(15) + wm_interm_154_triplet_pt3(i,j,q,k) * wm_interm_76_triplet_pt3(p,j,i,k)
term(16) = term(16) + wm_interm_154_triplet_pt3(i,j,k,q) * wm_interm_77_triplet_pt3(p,j,i,k)
term(17) = term(17) + wm_interm_154_triplet_pt3(i,j,q,k) * wm_interm_82_triplet_pt3(p,j,i,k)
term(18) = term(18) + wm_interm_154_triplet_pt3(i,j,k,q) * wm_interm_83_triplet_pt3(p,j,i,k)
end do 
end do 
end do 

term(0) = term(0) * (-0.5d+0) 
term(2) = term(2) * (-0.5d+0) 
term(3) = term(3) * (-2.0d+0) 
term(4) = term(4) * (2.0d+0) 
term(5) = term(5) * (-1.0d+0) 
term(6) = term(6) * (2.0d+0) 
term(7) = term(7) * (-1.0d+0) 
term(8) = term(8) * (-1.0d+0) 
term(9) = term(9) * (2.0d+0) 
term(10) = term(10) * (-1.0d+0) 
term(11) = term(11) * (-4.0d+0) 
term(12) = term(12) * (4.0d+0) 
term(13) = term(13) * (-4.0d+0) 
term(14) = term(14) * (4.0d+0) 
term(15) = term(15) * (-1.0d+0) 
term(16) = term(16) * (-1.0d+0) 
term(17) = term(17) * (-2.0d+0) 
term(18) = term(18) * (-2.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(19) = term(19) + wm_interm_1_triplet_pt3(p,i,j,k) * wm_interm_7_triplet_pt3(q,k,i,j)
term(20) = term(20) + wm_interm_1_triplet_pt3(p,i,j,k) * wm_interm_8_triplet_pt3(q,k,i,j)
term(21) = term(21) + wm_interm_1_triplet_pt3(p,i,j,k) * wm_interm_7_triplet_pt3(k,q,i,j)
term(22) = term(22) + wm_interm_1_triplet_pt3(p,i,j,k) * wm_interm_49_triplet_pt3(q,k,i,j)
term(23) = term(23) + wm_interm_1_triplet_pt3(p,i,j,k) * wm_interm_49_triplet_pt3(k,q,i,j)
term(24) = term(24) + wm_interm_76_triplet_pt3(p,i,j,k) * wm_interm_99_triplet_pt3(j,i,q,k)
term(25) = term(25) + wm_interm_76_triplet_pt3(p,i,j,k) * wm_interm_99_triplet_pt3(i,j,q,k)
term(26) = term(26) + wm_interm_76_triplet_pt3(p,i,j,k) * wm_interm_99_triplet_pt3(i,j,k,q)
term(27) = term(27) + wm_interm_77_triplet_pt3(p,i,j,k) * wm_interm_99_triplet_pt3(j,i,k,q)
term(28) = term(28) + wm_interm_77_triplet_pt3(p,i,j,k) * wm_interm_99_triplet_pt3(i,j,k,q)
term(29) = term(29) + wm_interm_77_triplet_pt3(p,i,j,k) * wm_interm_99_triplet_pt3(i,j,q,k)
term(30) = term(30) + wm_interm_82_triplet_pt3(p,i,j,k) * wm_interm_99_triplet_pt3(j,i,q,k)
term(31) = term(31) + wm_interm_82_triplet_pt3(p,i,j,k) * wm_interm_99_triplet_pt3(i,j,q,k)
term(32) = term(32) + wm_interm_82_triplet_pt3(p,i,j,k) * wm_interm_99_triplet_pt3(i,j,k,q)
term(33) = term(33) + wm_interm_83_triplet_pt3(p,i,j,k) * wm_interm_99_triplet_pt3(j,i,k,q)
term(34) = term(34) + wm_interm_83_triplet_pt3(p,i,j,k) * wm_interm_99_triplet_pt3(i,j,k,q)
term(35) = term(35) + wm_interm_83_triplet_pt3(p,i,j,k) * wm_interm_99_triplet_pt3(i,j,q,k)
term(36) = term(36) + wm_interm_154_triplet_pt3(i,j,q,k) * wm_interm_76_triplet_pt3(p,i,j,k)
term(37) = term(37) + wm_interm_154_triplet_pt3(i,j,k,q) * wm_interm_76_triplet_pt3(p,i,j,k)
term(38) = term(38) + wm_interm_154_triplet_pt3(i,j,k,q) * wm_interm_77_triplet_pt3(p,i,j,k)
term(39) = term(39) + wm_interm_154_triplet_pt3(i,j,q,k) * wm_interm_77_triplet_pt3(p,i,j,k)
term(40) = term(40) + wm_interm_154_triplet_pt3(i,j,q,k) * wm_interm_82_triplet_pt3(p,i,j,k)
term(41) = term(41) + wm_interm_154_triplet_pt3(i,j,k,q) * wm_interm_82_triplet_pt3(p,i,j,k)
term(42) = term(42) + wm_interm_154_triplet_pt3(i,j,k,q) * wm_interm_83_triplet_pt3(p,i,j,k)
term(43) = term(43) + wm_interm_154_triplet_pt3(i,j,q,k) * wm_interm_83_triplet_pt3(p,i,j,k)
term(44) = term(44) + r1(vrdav_Rr, p,i) * wm_interm_191_triplet_pt3(i,j,k,q) * wm_interm_75_triplet_pt3(k,j)
term(45) = term(45) + r1(vrdav_Rr, p,i) * wm_interm_191_triplet_pt3(i,j,k,q) * wm_interm_79_triplet_pt3(k,j)
term(46) = term(46) + r1(vrdav_Rr, p,i) * wm_interm_191_triplet_pt3(i,j,k,q) * wm_interm_81_triplet_pt3(k,j)
term(47) = term(47) + r1(vrdav_Rr, p,i) * wm_interm_154_triplet_pt3(i,j,k,q) * wm_interm_196_triplet_pt3(k,j)
term(48) = term(48) + r1(vrdav_Rr, p,i) * wm_interm_154_triplet_pt3(i,j,k,q) * wm_interm_197_triplet_pt3(k,j)
term(49) = term(49) + r1(vrdav_Rr, p,i) * wm_interm_154_triplet_pt3(i,j,q,k) * wm_interm_196_triplet_pt3(k,j)
term(50) = term(50) + r1(vrdav_Rr, p,i) * wm_interm_154_triplet_pt3(i,j,q,k) * wm_interm_197_triplet_pt3(k,j)
term(51) = term(51) + r1(vrdav_Rr, p,i) * wm_interm_154_triplet_pt3(j,i,q,k) * wm_interm_196_triplet_pt3(k,j)
term(52) = term(52) + r1(vrdav_Rr, p,i) * wm_interm_154_triplet_pt3(j,i,q,k) * wm_interm_197_triplet_pt3(k,j)
end do 
end do 
end do 

term(20) = term(20) * (-0.5d+0) 
term(21) = term(21) * (-0.5d+0) 
term(22) = term(22) * (2.0d+0) 
term(23) = term(23) * (-2.0d+0) 
term(24) = term(24) * (-0.5d+0) 
term(26) = term(26) * (-0.5d+0) 
term(27) = term(27) * (-0.5d+0) 
term(29) = term(29) * (-0.5d+0) 
term(30) = term(30) * (-1.0d+0) 
term(31) = term(31) * (2.0d+0) 
term(32) = term(32) * (-1.0d+0) 
term(33) = term(33) * (-1.0d+0) 
term(34) = term(34) * (2.0d+0) 
term(35) = term(35) * (-1.0d+0) 
term(36) = term(36) * (2.0d+0) 
term(37) = term(37) * (-1.0d+0) 
term(38) = term(38) * (2.0d+0) 
term(39) = term(39) * (-1.0d+0) 
term(40) = term(40) * (4.0d+0) 
term(41) = term(41) * (-2.0d+0) 
term(42) = term(42) * (4.0d+0) 
term(43) = term(43) * (-2.0d+0) 
term(44) = term(44) * (-1.0d+0) 
term(45) = term(45) * (2.0d+0) 
term(46) = term(46) * (-1.0d+0) 
term(47) = term(47) * (-2.0d+0) 
term(48) = term(48) * (4.0d+0) 
term(49) = term(49) * (4.0d+0) 
term(50) = term(50) * (-8.0d+0) 
term(51) = term(51) * (-2.0d+0) 
term(52) = term(52) * (4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(53) = term(53) + wm_interm_16_triplet_pt3(a,q,i,j) * wm_interm_21_triplet_pt3(a,p,i,j)
term(54) = term(54) + wm_interm_16_triplet_pt3(a,q,i,j) * wm_interm_22_triplet_pt3(a,p,i,j)
term(55) = term(55) + wm_interm_16_triplet_pt3(a,q,i,j) * wm_interm_23_triplet_pt3(a,p,i,j)
term(56) = term(56) + wm_interm_16_triplet_pt3(a,q,i,j) * wm_interm_24_triplet_pt3(a,p,i,j)
term(57) = term(57) + wm_interm_16_triplet_pt3(a,q,i,j) * wm_interm_25_triplet_pt3(a,p,i,j)
term(58) = term(58) + wm_interm_16_triplet_pt3(a,q,i,j) * wm_interm_26_triplet_pt3(a,p,i,j)
term(59) = term(59) + wm_interm_16_triplet_pt3(a,i,q,j) * wm_interm_21_triplet_pt3(a,p,i,j)
term(60) = term(60) + wm_interm_16_triplet_pt3(a,i,q,j) * wm_interm_22_triplet_pt3(a,p,i,j)
term(61) = term(61) + wm_interm_16_triplet_pt3(a,i,q,j) * wm_interm_24_triplet_pt3(a,p,i,j)
term(62) = term(62) + wm_interm_16_triplet_pt3(a,i,q,j) * wm_interm_23_triplet_pt3(a,p,i,j)
term(63) = term(63) + wm_interm_16_triplet_pt3(a,i,q,j) * wm_interm_26_triplet_pt3(a,p,i,j)
term(64) = term(64) + wm_interm_16_triplet_pt3(a,i,q,j) * wm_interm_30_triplet_pt3(a,p,i,j)
term(65) = term(65) + wm_interm_21_triplet_pt3(a,p,i,j) * wm_interm_41_triplet_pt3(a,i,q,j)
term(66) = term(66) + wm_interm_22_triplet_pt3(a,p,i,j) * wm_interm_41_triplet_pt3(a,i,q,j)
term(67) = term(67) + wm_interm_26_triplet_pt3(a,p,i,j) * wm_interm_41_triplet_pt3(a,i,q,j)
term(68) = term(68) + wm_interm_30_triplet_pt3(a,p,i,j) * wm_interm_41_triplet_pt3(a,q,i,j)
term(69) = term(69) + wm_interm_25_triplet_pt3(a,p,i,j) * wm_interm_41_triplet_pt3(a,q,i,j)
term(70) = term(70) + wm_interm_24_triplet_pt3(a,p,i,j) * wm_interm_41_triplet_pt3(a,q,i,j)
term(71) = term(71) + wm_interm_25_triplet_pt3(a,p,i,j) * wm_interm_41_triplet_pt3(a,i,q,j)
term(72) = term(72) + wm_interm_30_triplet_pt3(a,p,i,j) * wm_interm_41_triplet_pt3(a,i,q,j)
term(73) = term(73) + wm_interm_23_triplet_pt3(a,p,i,j) * wm_interm_41_triplet_pt3(a,i,q,j)
term(74) = term(74) + wm_interm_16_triplet_pt3(a,q,i,j) * wm_interm_58_triplet_pt3(a,p,i,j)
term(75) = term(75) + wm_interm_16_triplet_pt3(a,q,i,j) * wm_interm_59_triplet_pt3(a,p,i,j)
term(76) = term(76) + wm_interm_16_triplet_pt3(a,q,i,j) * wm_interm_60_triplet_pt3(a,p,i,j)
term(77) = term(77) + wm_interm_16_triplet_pt3(a,q,i,j) * wm_interm_61_triplet_pt3(a,p,i,j)
term(78) = term(78) + wm_interm_16_triplet_pt3(a,i,q,j) * wm_interm_58_triplet_pt3(a,p,i,j)
term(79) = term(79) + wm_interm_16_triplet_pt3(a,i,q,j) * wm_interm_59_triplet_pt3(a,p,i,j)
term(80) = term(80) + wm_interm_16_triplet_pt3(a,i,q,j) * wm_interm_61_triplet_pt3(a,p,i,j)
term(81) = term(81) + wm_interm_16_triplet_pt3(a,i,q,j) * wm_interm_60_triplet_pt3(a,p,i,j)
term(82) = term(82) + wm_interm_41_triplet_pt3(a,i,q,j) * wm_interm_58_triplet_pt3(a,p,i,j)
term(83) = term(83) + wm_interm_41_triplet_pt3(a,i,q,j) * wm_interm_59_triplet_pt3(a,p,i,j)
term(84) = term(84) + wm_interm_41_triplet_pt3(a,q,i,j) * wm_interm_61_triplet_pt3(a,p,i,j)
term(85) = term(85) + wm_interm_41_triplet_pt3(a,q,i,j) * wm_interm_60_triplet_pt3(a,p,i,j)
term(86) = term(86) + wm_interm_41_triplet_pt3(a,i,q,j) * wm_interm_60_triplet_pt3(a,p,i,j)
term(87) = term(87) + wm_interm_41_triplet_pt3(a,i,q,j) * wm_interm_61_triplet_pt3(a,p,i,j)
term(88) = term(88) + wm_interm_32_triplet_pt3(a,p,i,j) * wm_interm_98_triplet_pt3(a,j,q,i)
term(89) = term(89) + wm_interm_33_triplet_pt3(a,p,i,j) * wm_interm_98_triplet_pt3(a,j,q,i)
term(90) = term(90) + wm_interm_40_triplet_pt3(a,p,i,j) * wm_interm_98_triplet_pt3(a,j,q,i)
term(91) = term(91) + wm_interm_4_triplet_pt3(a,p,i,j) * wm_interm_98_triplet_pt3(a,j,q,i)
term(92) = term(92) + wm_interm_3_triplet_pt3(a,p,i,j) * wm_interm_98_triplet_pt3(a,j,q,i)
term(93) = term(93) + wm_interm_14_triplet_pt3(a,p,i,j) * wm_interm_98_triplet_pt3(a,j,q,i)
term(94) = term(94) + wm_interm_35_triplet_pt3(a,p,i,j) * wm_interm_98_triplet_pt3(a,j,q,i)
term(95) = term(95) + wm_interm_34_triplet_pt3(a,p,i,j) * wm_interm_98_triplet_pt3(a,j,q,i)
term(96) = term(96) + wm_interm_31_triplet_pt3(a,p,i,j) * wm_interm_98_triplet_pt3(a,j,q,i)
term(97) = term(97) + wm_interm_10_triplet_pt3(a,p,i,j) * wm_interm_98_triplet_pt3(a,j,q,i)
term(98) = term(98) + wm_interm_11_triplet_pt3(a,p,i,j) * wm_interm_98_triplet_pt3(a,j,q,i)
term(99) = term(99) + wm_interm_2_triplet_pt3(a,p,i,j) * wm_interm_98_triplet_pt3(a,j,q,i)
term(100) = term(100) + wm_interm_3_triplet_pt3(a,p,i,j) * wm_interm_98_triplet_pt3(a,j,i,q)
term(101) = term(101) + wm_interm_39_triplet_pt3(a,p,i,j) * wm_interm_98_triplet_pt3(a,j,i,q)
term(102) = term(102) + wm_interm_14_triplet_pt3(a,p,i,j) * wm_interm_98_triplet_pt3(a,j,i,q)
term(103) = term(103) + wm_interm_0_triplet_pt3(a,p,i,j) * wm_interm_98_triplet_pt3(a,j,i,q)
term(104) = term(104) + wm_interm_34_triplet_pt3(a,p,i,j) * wm_interm_98_triplet_pt3(a,j,i,q)
term(105) = term(105) + wm_interm_10_triplet_pt3(a,p,i,j) * wm_interm_98_triplet_pt3(a,j,i,q)
term(106) = term(106) + wm_interm_2_triplet_pt3(a,p,i,j) * wm_interm_98_triplet_pt3(a,j,i,q)
term(107) = term(107) + wm_interm_35_triplet_pt3(a,p,i,j) * wm_interm_98_triplet_pt3(a,j,i,q)
term(108) = term(108) + wm_interm_11_triplet_pt3(a,p,i,j) * wm_interm_98_triplet_pt3(a,j,i,q)
term(109) = term(109) + wm_interm_64_triplet_pt3(a,p,i,j) * wm_interm_98_triplet_pt3(a,j,q,i)
term(110) = term(110) + wm_interm_65_triplet_pt3(a,p,i,j) * wm_interm_98_triplet_pt3(a,j,q,i)
term(111) = term(111) + wm_interm_67_triplet_pt3(a,p,i,j) * wm_interm_98_triplet_pt3(a,j,q,i)
term(112) = term(112) + wm_interm_46_triplet_pt3(a,p,i,j) * wm_interm_98_triplet_pt3(a,j,q,i)
term(113) = term(113) + wm_interm_45_triplet_pt3(a,p,i,j) * wm_interm_98_triplet_pt3(a,j,q,i)
term(114) = term(114) + wm_interm_50_triplet_pt3(a,p,i,j) * wm_interm_98_triplet_pt3(a,j,q,i)
term(115) = term(115) + wm_interm_66_triplet_pt3(a,p,i,j) * wm_interm_98_triplet_pt3(a,j,q,i)
term(116) = term(116) + wm_interm_51_triplet_pt3(a,p,i,j) * wm_interm_98_triplet_pt3(a,j,q,i)
term(117) = term(117) + wm_interm_45_triplet_pt3(a,p,i,j) * wm_interm_98_triplet_pt3(a,j,i,q)
term(118) = term(118) + wm_interm_66_triplet_pt3(a,p,i,j) * wm_interm_98_triplet_pt3(a,j,i,q)
term(119) = term(119) + wm_interm_50_triplet_pt3(a,p,i,j) * wm_interm_98_triplet_pt3(a,j,i,q)
term(120) = term(120) + wm_interm_46_triplet_pt3(a,p,i,j) * wm_interm_98_triplet_pt3(a,j,i,q)
term(121) = term(121) + wm_interm_67_triplet_pt3(a,p,i,j) * wm_interm_98_triplet_pt3(a,j,i,q)
term(122) = term(122) + wm_interm_51_triplet_pt3(a,p,i,j) * wm_interm_98_triplet_pt3(a,j,i,q)
term(123) = term(123) + wm_interm_119_triplet_pt3(a,q,i,j) * wm_interm_21_triplet_pt3(a,p,i,j)
term(124) = term(124) + wm_interm_119_triplet_pt3(a,q,i,j) * wm_interm_22_triplet_pt3(a,p,i,j)
term(125) = term(125) + wm_interm_119_triplet_pt3(a,q,i,j) * wm_interm_23_triplet_pt3(a,p,i,j)
term(126) = term(126) + wm_interm_119_triplet_pt3(a,q,i,j) * wm_interm_24_triplet_pt3(a,p,i,j)
term(127) = term(127) + wm_interm_119_triplet_pt3(a,q,i,j) * wm_interm_25_triplet_pt3(a,p,i,j)
term(128) = term(128) + wm_interm_119_triplet_pt3(a,q,i,j) * wm_interm_26_triplet_pt3(a,p,i,j)
term(129) = term(129) + wm_interm_119_triplet_pt3(a,i,q,j) * wm_interm_21_triplet_pt3(a,p,i,j)
term(130) = term(130) + wm_interm_119_triplet_pt3(a,i,q,j) * wm_interm_22_triplet_pt3(a,p,i,j)
term(131) = term(131) + wm_interm_119_triplet_pt3(a,i,q,j) * wm_interm_24_triplet_pt3(a,p,i,j)
term(132) = term(132) + wm_interm_119_triplet_pt3(a,i,q,j) * wm_interm_23_triplet_pt3(a,p,i,j)
term(133) = term(133) + wm_interm_119_triplet_pt3(a,i,q,j) * wm_interm_26_triplet_pt3(a,p,i,j)
term(134) = term(134) + wm_interm_119_triplet_pt3(a,i,q,j) * wm_interm_30_triplet_pt3(a,p,i,j)
term(135) = term(135) + wm_interm_127_triplet_pt3(a,i,q,j) * wm_interm_21_triplet_pt3(a,p,i,j)
term(136) = term(136) + wm_interm_127_triplet_pt3(a,i,q,j) * wm_interm_22_triplet_pt3(a,p,i,j)
term(137) = term(137) + wm_interm_127_triplet_pt3(a,i,q,j) * wm_interm_26_triplet_pt3(a,p,i,j)
term(138) = term(138) + wm_interm_127_triplet_pt3(a,q,i,j) * wm_interm_30_triplet_pt3(a,p,i,j)
term(139) = term(139) + wm_interm_127_triplet_pt3(a,q,i,j) * wm_interm_25_triplet_pt3(a,p,i,j)
term(140) = term(140) + wm_interm_127_triplet_pt3(a,q,i,j) * wm_interm_24_triplet_pt3(a,p,i,j)
term(141) = term(141) + wm_interm_127_triplet_pt3(a,i,q,j) * wm_interm_25_triplet_pt3(a,p,i,j)
term(142) = term(142) + wm_interm_127_triplet_pt3(a,i,q,j) * wm_interm_30_triplet_pt3(a,p,i,j)
term(143) = term(143) + wm_interm_127_triplet_pt3(a,i,q,j) * wm_interm_23_triplet_pt3(a,p,i,j)
term(144) = term(144) + wm_interm_119_triplet_pt3(a,q,i,j) * wm_interm_58_triplet_pt3(a,p,i,j)
term(145) = term(145) + wm_interm_119_triplet_pt3(a,q,i,j) * wm_interm_59_triplet_pt3(a,p,i,j)
term(146) = term(146) + wm_interm_119_triplet_pt3(a,q,i,j) * wm_interm_60_triplet_pt3(a,p,i,j)
term(147) = term(147) + wm_interm_119_triplet_pt3(a,q,i,j) * wm_interm_61_triplet_pt3(a,p,i,j)
term(148) = term(148) + wm_interm_119_triplet_pt3(a,i,q,j) * wm_interm_58_triplet_pt3(a,p,i,j)
term(149) = term(149) + wm_interm_119_triplet_pt3(a,i,q,j) * wm_interm_59_triplet_pt3(a,p,i,j)
term(150) = term(150) + wm_interm_119_triplet_pt3(a,i,q,j) * wm_interm_61_triplet_pt3(a,p,i,j)
term(151) = term(151) + wm_interm_119_triplet_pt3(a,i,q,j) * wm_interm_60_triplet_pt3(a,p,i,j)
term(152) = term(152) + wm_interm_127_triplet_pt3(a,i,q,j) * wm_interm_58_triplet_pt3(a,p,i,j)
term(153) = term(153) + wm_interm_127_triplet_pt3(a,i,q,j) * wm_interm_59_triplet_pt3(a,p,i,j)
term(154) = term(154) + wm_interm_127_triplet_pt3(a,q,i,j) * wm_interm_61_triplet_pt3(a,p,i,j)
term(155) = term(155) + wm_interm_127_triplet_pt3(a,q,i,j) * wm_interm_60_triplet_pt3(a,p,i,j)
term(156) = term(156) + wm_interm_127_triplet_pt3(a,i,q,j) * wm_interm_60_triplet_pt3(a,p,i,j)
term(157) = term(157) + wm_interm_127_triplet_pt3(a,i,q,j) * wm_interm_61_triplet_pt3(a,p,i,j)
term(158) = term(158) + wm_interm_117_triplet_pt3(a,p,i,j) * wm_interm_98_triplet_pt3(a,j,q,i)
term(159) = term(159) + wm_interm_123_triplet_pt3(a,p,i,j) * wm_interm_98_triplet_pt3(a,j,q,i)
term(160) = term(160) + wm_interm_110_triplet_pt3(a,p,i,j) * wm_interm_98_triplet_pt3(a,j,q,i)
term(161) = term(161) + wm_interm_109_triplet_pt3(a,p,i,j) * wm_interm_98_triplet_pt3(a,j,q,i)
term(162) = term(162) + wm_interm_108_triplet_pt3(a,p,i,j) * wm_interm_98_triplet_pt3(a,j,q,i)
term(163) = term(163) + wm_interm_107_triplet_pt3(a,p,i,j) * wm_interm_98_triplet_pt3(a,j,q,i)
term(164) = term(164) + wm_interm_113_triplet_pt3(a,p,i,j) * wm_interm_98_triplet_pt3(a,j,q,i)
term(165) = term(165) + wm_interm_114_triplet_pt3(a,p,i,j) * wm_interm_98_triplet_pt3(a,j,q,i)
term(166) = term(166) + wm_interm_109_triplet_pt3(a,p,i,j) * wm_interm_98_triplet_pt3(a,j,i,q)
term(167) = term(167) + wm_interm_117_triplet_pt3(a,p,i,j) * wm_interm_98_triplet_pt3(a,j,i,q)
term(168) = term(168) + wm_interm_107_triplet_pt3(a,p,i,j) * wm_interm_98_triplet_pt3(a,j,i,q)
term(169) = term(169) + wm_interm_113_triplet_pt3(a,p,i,j) * wm_interm_98_triplet_pt3(a,j,i,q)
term(170) = term(170) + wm_interm_108_triplet_pt3(a,p,i,j) * wm_interm_98_triplet_pt3(a,j,i,q)
term(171) = term(171) + wm_interm_114_triplet_pt3(a,p,i,j) * wm_interm_98_triplet_pt3(a,j,i,q)
term(172) = term(172) + wm_interm_134_triplet_pt3(a,p,i,j) * wm_interm_98_triplet_pt3(a,j,q,i)
term(173) = term(173) + wm_interm_135_triplet_pt3(a,p,i,j) * wm_interm_98_triplet_pt3(a,j,q,i)
term(174) = term(174) + wm_interm_132_triplet_pt3(a,p,i,j) * wm_interm_98_triplet_pt3(a,j,q,i)
term(175) = term(175) + wm_interm_131_triplet_pt3(a,p,i,j) * wm_interm_98_triplet_pt3(a,j,q,i)
term(176) = term(176) + wm_interm_131_triplet_pt3(a,p,i,j) * wm_interm_98_triplet_pt3(a,j,i,q)
term(177) = term(177) + wm_interm_134_triplet_pt3(a,p,i,j) * wm_interm_98_triplet_pt3(a,j,i,q)
term(178) = term(178) + wm_interm_132_triplet_pt3(a,p,i,j) * wm_interm_98_triplet_pt3(a,j,i,q)
term(179) = term(179) + wm_interm_135_triplet_pt3(a,p,i,j) * wm_interm_98_triplet_pt3(a,j,i,q)
term(180) = term(180) + r1(vrdav_Rl, a,q) * wm_interm_17_triplet_pt3(i,j) * wm_interm_190_triplet_pt3(p,a,i,j)
term(181) = term(181) + r1(vrdav_Rl, a,q) * wm_interm_18_triplet_pt3(i,j) * wm_interm_190_triplet_pt3(p,a,i,j)
term(182) = term(182) + r1(vrdav_Rl, a,q) * wm_interm_190_triplet_pt3(p,a,i,j) * wm_interm_19_triplet_pt3(i,j)
term(183) = term(183) + r1(vrdav_Rl, a,q) * wm_interm_17_triplet_pt3(i,j) * wm_interm_193_triplet_pt3(p,a,i,j)
term(184) = term(184) + r1(vrdav_Rl, a,q) * wm_interm_18_triplet_pt3(i,j) * wm_interm_193_triplet_pt3(p,a,i,j)
term(185) = term(185) + r1(vrdav_Rl, a,q) * wm_interm_17_triplet_pt3(i,j) * wm_interm_194_triplet_pt3(p,a,i,j)
term(186) = term(186) + r1(vrdav_Rl, a,q) * wm_interm_18_triplet_pt3(i,j) * wm_interm_194_triplet_pt3(p,a,i,j)
term(187) = term(187) + r1(vrdav_Rl, a,q) * wm_interm_193_triplet_pt3(p,a,i,j) * wm_interm_19_triplet_pt3(i,j)
term(188) = term(188) + r1(vrdav_Rl, a,q) * wm_interm_194_triplet_pt3(p,a,i,j) * wm_interm_19_triplet_pt3(i,j)
term(189) = term(189) + s2(a,p,i,q) * wm_interm_17_triplet_pt3(i,j) * wm_interm_186_triplet_pt3(a,j)
term(190) = term(190) + s2(a,p,i,q) * wm_interm_186_triplet_pt3(a,j) * wm_interm_18_triplet_pt3(i,j)
term(191) = term(191) + s2(a,p,i,q) * wm_interm_186_triplet_pt3(a,j) * wm_interm_19_triplet_pt3(i,j)
term(192) = term(192) + r1(vrdav_Rl, a,q) * wm_interm_190_triplet_pt3(p,a,i,j) * wm_interm_54_triplet_pt3(i,j)
term(193) = term(193) + r1(vrdav_Rl, a,q) * wm_interm_190_triplet_pt3(p,a,i,j) * wm_interm_55_triplet_pt3(i,j)
term(194) = term(194) + r1(vrdav_Rl, a,q) * wm_interm_190_triplet_pt3(p,a,i,j) * wm_interm_56_triplet_pt3(i,j)
term(195) = term(195) + r1(vrdav_Rl, a,q) * wm_interm_193_triplet_pt3(p,a,i,j) * wm_interm_54_triplet_pt3(i,j)
term(196) = term(196) + r1(vrdav_Rl, a,q) * wm_interm_193_triplet_pt3(p,a,i,j) * wm_interm_55_triplet_pt3(i,j)
term(197) = term(197) + r1(vrdav_Rl, a,q) * wm_interm_194_triplet_pt3(p,a,i,j) * wm_interm_54_triplet_pt3(i,j)
term(198) = term(198) + r1(vrdav_Rl, a,q) * wm_interm_194_triplet_pt3(p,a,i,j) * wm_interm_55_triplet_pt3(i,j)
term(199) = term(199) + r1(vrdav_Rl, a,q) * wm_interm_193_triplet_pt3(p,a,i,j) * wm_interm_56_triplet_pt3(i,j)
term(200) = term(200) + r1(vrdav_Rl, a,q) * wm_interm_194_triplet_pt3(p,a,i,j) * wm_interm_56_triplet_pt3(i,j)
term(201) = term(201) + s2(a,p,i,q) * wm_interm_186_triplet_pt3(a,j) * wm_interm_54_triplet_pt3(i,j)
term(202) = term(202) + s2(a,p,i,q) * wm_interm_186_triplet_pt3(a,j) * wm_interm_55_triplet_pt3(i,j)
term(203) = term(203) + s2(a,p,i,q) * wm_interm_186_triplet_pt3(a,j) * wm_interm_56_triplet_pt3(i,j)
term(204) = term(204) + r1(vrdav_Rl, a,i) * wm_interm_196_triplet_pt3(j,i) * wm_interm_30_triplet_pt3(a,p,j,q)
term(205) = term(205) + r1(vrdav_Rl, a,i) * wm_interm_197_triplet_pt3(j,i) * wm_interm_30_triplet_pt3(a,p,j,q)
term(206) = term(206) + r1(vrdav_Rl, a,i) * wm_interm_196_triplet_pt3(j,i) * wm_interm_24_triplet_pt3(a,p,j,q)
term(207) = term(207) + r1(vrdav_Rl, a,i) * wm_interm_197_triplet_pt3(j,i) * wm_interm_24_triplet_pt3(a,p,j,q)
term(208) = term(208) + r1(vrdav_Rl, a,i) * wm_interm_196_triplet_pt3(j,i) * wm_interm_23_triplet_pt3(a,p,j,q)
term(209) = term(209) + r1(vrdav_Rl, a,i) * wm_interm_197_triplet_pt3(j,i) * wm_interm_23_triplet_pt3(a,p,j,q)
term(210) = term(210) + r1(vrdav_Rl, a,i) * wm_interm_196_triplet_pt3(j,i) * wm_interm_26_triplet_pt3(a,p,j,q)
term(211) = term(211) + r1(vrdav_Rl, a,i) * wm_interm_197_triplet_pt3(j,i) * wm_interm_26_triplet_pt3(a,p,j,q)
term(212) = term(212) + r1(vrdav_Rl, a,i) * wm_interm_196_triplet_pt3(j,i) * wm_interm_21_triplet_pt3(a,p,j,q)
term(213) = term(213) + r1(vrdav_Rl, a,i) * wm_interm_197_triplet_pt3(j,i) * wm_interm_21_triplet_pt3(a,p,j,q)
term(214) = term(214) + r1(vrdav_Rl, a,i) * wm_interm_196_triplet_pt3(j,i) * wm_interm_22_triplet_pt3(a,p,j,q)
term(215) = term(215) + r1(vrdav_Rl, a,i) * wm_interm_197_triplet_pt3(j,i) * wm_interm_22_triplet_pt3(a,p,j,q)
term(216) = term(216) + s2(a,p,i,j) * wm_interm_17_triplet_pt3(i,q) * wm_interm_186_triplet_pt3(a,j)
term(217) = term(217) + s2(a,p,i,j) * wm_interm_186_triplet_pt3(a,j) * wm_interm_18_triplet_pt3(i,q)
term(218) = term(218) + s2(a,p,i,j) * wm_interm_186_triplet_pt3(a,j) * wm_interm_19_triplet_pt3(i,q)
term(219) = term(219) + r1(vrdav_Rl, a,i) * wm_interm_196_triplet_pt3(j,i) * wm_interm_61_triplet_pt3(a,p,j,q)
term(220) = term(220) + r1(vrdav_Rl, a,i) * wm_interm_197_triplet_pt3(j,i) * wm_interm_61_triplet_pt3(a,p,j,q)
term(221) = term(221) + r1(vrdav_Rl, a,i) * wm_interm_196_triplet_pt3(j,i) * wm_interm_60_triplet_pt3(a,p,j,q)
term(222) = term(222) + r1(vrdav_Rl, a,i) * wm_interm_197_triplet_pt3(j,i) * wm_interm_60_triplet_pt3(a,p,j,q)
term(223) = term(223) + r1(vrdav_Rl, a,i) * wm_interm_196_triplet_pt3(j,i) * wm_interm_58_triplet_pt3(a,p,j,q)
term(224) = term(224) + r1(vrdav_Rl, a,i) * wm_interm_197_triplet_pt3(j,i) * wm_interm_58_triplet_pt3(a,p,j,q)
term(225) = term(225) + r1(vrdav_Rl, a,i) * wm_interm_196_triplet_pt3(j,i) * wm_interm_59_triplet_pt3(a,p,j,q)
term(226) = term(226) + r1(vrdav_Rl, a,i) * wm_interm_197_triplet_pt3(j,i) * wm_interm_59_triplet_pt3(a,p,j,q)
term(227) = term(227) + s2(a,p,i,j) * wm_interm_186_triplet_pt3(a,j) * wm_interm_54_triplet_pt3(i,q)
term(228) = term(228) + s2(a,p,i,j) * wm_interm_186_triplet_pt3(a,j) * wm_interm_55_triplet_pt3(i,q)
term(229) = term(229) + s2(a,p,i,j) * wm_interm_186_triplet_pt3(a,j) * wm_interm_56_triplet_pt3(i,q)
term(230) = term(230) + t2(a,p,i,q) * wm_interm_176_triplet_pt3(a,j) * wm_interm_196_triplet_pt3(i,j)
term(231) = term(231) + t2(a,p,i,q) * wm_interm_176_triplet_pt3(a,j) * wm_interm_197_triplet_pt3(i,j)
term(232) = term(232) + r1(vrdav_Rr, a,i) * wm_interm_196_triplet_pt3(i,j) * wm_interm_91_triplet_pt3(a,p,j,q)
term(233) = term(233) + r1(vrdav_Rr, a,i) * wm_interm_197_triplet_pt3(i,j) * wm_interm_91_triplet_pt3(a,p,j,q)
term(234) = term(234) + t2(a,p,i,q) * wm_interm_172_triplet_pt3(a,j) * wm_interm_196_triplet_pt3(i,j)
term(235) = term(235) + t2(a,p,i,q) * wm_interm_172_triplet_pt3(a,j) * wm_interm_197_triplet_pt3(i,j)
term(236) = term(236) + r1(vrdav_Rr, a,i) * wm_interm_196_triplet_pt3(i,j) * wm_interm_90_triplet_pt3(a,p,j,q)
term(237) = term(237) + r1(vrdav_Rr, a,i) * wm_interm_197_triplet_pt3(i,j) * wm_interm_90_triplet_pt3(a,p,j,q)
term(238) = term(238) + t2(a,p,i,q) * wm_interm_174_triplet_pt3(a,j) * wm_interm_196_triplet_pt3(i,j)
term(239) = term(239) + r1(vrdav_Rr, a,i) * wm_interm_196_triplet_pt3(i,j) * wm_interm_92_triplet_pt3(a,p,j,q)
term(240) = term(240) + t2(a,p,i,q) * wm_interm_174_triplet_pt3(a,j) * wm_interm_197_triplet_pt3(i,j)
term(241) = term(241) + r1(vrdav_Rr, a,i) * wm_interm_197_triplet_pt3(i,j) * wm_interm_92_triplet_pt3(a,p,j,q)
term(242) = term(242) + r1(vrdav_Rr, a,i) * wm_interm_193_triplet_pt3(a,p,j,q) * wm_interm_75_triplet_pt3(i,j)
term(243) = term(243) + t2(a,p,i,q) * wm_interm_183_triplet_pt3(a,j) * wm_interm_75_triplet_pt3(i,j)
term(244) = term(244) + r1(vrdav_Rr, a,i) * wm_interm_194_triplet_pt3(a,p,j,q) * wm_interm_75_triplet_pt3(i,j)
term(245) = term(245) + r1(vrdav_Rr, a,i) * wm_interm_193_triplet_pt3(a,p,j,q) * wm_interm_79_triplet_pt3(i,j)
term(246) = term(246) + t2(a,p,i,q) * wm_interm_183_triplet_pt3(a,j) * wm_interm_79_triplet_pt3(i,j)
term(247) = term(247) + r1(vrdav_Rr, a,i) * wm_interm_194_triplet_pt3(a,p,j,q) * wm_interm_79_triplet_pt3(i,j)
term(248) = term(248) + r1(vrdav_Rr, a,i) * wm_interm_193_triplet_pt3(a,p,j,q) * wm_interm_81_triplet_pt3(i,j)
term(249) = term(249) + t2(a,p,i,q) * wm_interm_183_triplet_pt3(a,j) * wm_interm_81_triplet_pt3(i,j)
term(250) = term(250) + r1(vrdav_Rr, a,i) * wm_interm_194_triplet_pt3(a,p,j,q) * wm_interm_81_triplet_pt3(i,j)
term(251) = term(251) + r1(vrdav_Rr, a,i) * wm_interm_196_triplet_pt3(i,j) * wm_interm_94_triplet_pt3(a,p,j,q)
term(252) = term(252) + r1(vrdav_Rr, a,i) * wm_interm_197_triplet_pt3(i,j) * wm_interm_94_triplet_pt3(a,p,j,q)
term(253) = term(253) + r1(vrdav_Rr, a,i) * wm_interm_196_triplet_pt3(i,j) * wm_interm_93_triplet_pt3(a,p,j,q)
term(254) = term(254) + r1(vrdav_Rr, a,i) * wm_interm_197_triplet_pt3(i,j) * wm_interm_93_triplet_pt3(a,p,j,q)
term(255) = term(255) + r1(vrdav_Rr, a,i) * wm_interm_196_triplet_pt3(i,j) * wm_interm_95_triplet_pt3(a,p,j,q)
term(256) = term(256) + r1(vrdav_Rr, a,i) * wm_interm_197_triplet_pt3(i,j) * wm_interm_95_triplet_pt3(a,p,j,q)
term(257) = term(257) + r1(vrdav_Rr, a,i) * wm_interm_192_triplet_pt3(a,p,j,q) * wm_interm_75_triplet_pt3(i,j)
term(258) = term(258) + r1(vrdav_Rr, a,i) * wm_interm_190_triplet_pt3(a,p,j,q) * wm_interm_75_triplet_pt3(i,j)
term(259) = term(259) + r1(vrdav_Rr, a,i) * wm_interm_192_triplet_pt3(a,p,j,q) * wm_interm_79_triplet_pt3(i,j)
term(260) = term(260) + r1(vrdav_Rr, a,i) * wm_interm_190_triplet_pt3(a,p,j,q) * wm_interm_79_triplet_pt3(i,j)
term(261) = term(261) + r1(vrdav_Rr, a,i) * wm_interm_192_triplet_pt3(a,p,j,q) * wm_interm_81_triplet_pt3(i,j)
term(262) = term(262) + r1(vrdav_Rr, a,i) * wm_interm_190_triplet_pt3(a,p,j,q) * wm_interm_81_triplet_pt3(i,j)
term(263) = term(263) + t2(a,p,i,q) * wm_interm_181_triplet_pt3(a,j) * wm_interm_196_triplet_pt3(i,j)
term(264) = term(264) + t2(a,p,i,q) * wm_interm_181_triplet_pt3(a,j) * wm_interm_197_triplet_pt3(i,j)
term(265) = term(265) + r1(vrdav_Rr, a,i) * wm_interm_150_triplet_pt3(a,p,j,q) * wm_interm_196_triplet_pt3(i,j)
term(266) = term(266) + r1(vrdav_Rr, a,i) * wm_interm_150_triplet_pt3(a,p,j,q) * wm_interm_197_triplet_pt3(i,j)
term(267) = term(267) + t2(a,p,i,q) * wm_interm_180_triplet_pt3(a,j) * wm_interm_196_triplet_pt3(i,j)
term(268) = term(268) + t2(a,p,i,q) * wm_interm_180_triplet_pt3(a,j) * wm_interm_197_triplet_pt3(i,j)
term(269) = term(269) + r1(vrdav_Rr, a,i) * wm_interm_151_triplet_pt3(a,p,j,q) * wm_interm_196_triplet_pt3(i,j)
term(270) = term(270) + r1(vrdav_Rr, a,i) * wm_interm_151_triplet_pt3(a,p,j,q) * wm_interm_197_triplet_pt3(i,j)
term(271) = term(271) + r1(vrdav_Rr, a,i) * wm_interm_145_triplet_pt3(i,j) * wm_interm_193_triplet_pt3(a,p,j,q)
term(272) = term(272) + t2(a,p,i,q) * wm_interm_145_triplet_pt3(i,j) * wm_interm_183_triplet_pt3(a,j)
term(273) = term(273) + r1(vrdav_Rr, a,i) * wm_interm_145_triplet_pt3(i,j) * wm_interm_194_triplet_pt3(a,p,j,q)
term(274) = term(274) + r1(vrdav_Rr, a,i) * wm_interm_147_triplet_pt3(i,j) * wm_interm_193_triplet_pt3(a,p,j,q)
term(275) = term(275) + t2(a,p,i,q) * wm_interm_147_triplet_pt3(i,j) * wm_interm_183_triplet_pt3(a,j)
term(276) = term(276) + r1(vrdav_Rr, a,i) * wm_interm_147_triplet_pt3(i,j) * wm_interm_194_triplet_pt3(a,p,j,q)
term(277) = term(277) + r1(vrdav_Rr, a,i) * wm_interm_152_triplet_pt3(a,p,j,q) * wm_interm_196_triplet_pt3(i,j)
term(278) = term(278) + r1(vrdav_Rr, a,i) * wm_interm_152_triplet_pt3(a,p,j,q) * wm_interm_197_triplet_pt3(i,j)
term(279) = term(279) + r1(vrdav_Rr, a,i) * wm_interm_153_triplet_pt3(a,p,j,q) * wm_interm_196_triplet_pt3(i,j)
term(280) = term(280) + r1(vrdav_Rr, a,i) * wm_interm_153_triplet_pt3(a,p,j,q) * wm_interm_197_triplet_pt3(i,j)
term(281) = term(281) + r1(vrdav_Rr, a,i) * wm_interm_145_triplet_pt3(i,j) * wm_interm_192_triplet_pt3(a,p,j,q)
term(282) = term(282) + r1(vrdav_Rr, a,i) * wm_interm_145_triplet_pt3(i,j) * wm_interm_190_triplet_pt3(a,p,j,q)
term(283) = term(283) + r1(vrdav_Rr, a,i) * wm_interm_147_triplet_pt3(i,j) * wm_interm_192_triplet_pt3(a,p,j,q)
term(284) = term(284) + r1(vrdav_Rr, a,i) * wm_interm_147_triplet_pt3(i,j) * wm_interm_190_triplet_pt3(a,p,j,q)
end do 
end do 
end do 

term(53) = term(53) * (-0.5d+0) 
term(55) = term(55) * (-0.5d+0) 
term(57) = term(57) * (-0.5d+0) 
term(58) = term(58) * (-0.5d+0) 
term(60) = term(60) * (-2.0d+0) 
term(61) = term(61) * (-0.5d+0) 
term(64) = term(64) * (-0.5d+0) 
term(65) = term(65) * (-0.5d+0) 
term(67) = term(67) * (-0.5d+0) 
term(68) = term(68) * (-0.5d+0) 
term(70) = term(70) * (-0.5d+0) 
term(71) = term(71) * (-0.5d+0) 
term(73) = term(73) * (-0.5d+0) 
term(74) = term(74) * (-2.0d+0) 
term(75) = term(75) * (2.0d+0) 
term(76) = term(76) * (-2.0d+0) 
term(77) = term(77) * (2.0d+0) 
term(78) = term(78) * (4.0d+0) 
term(79) = term(79) * (-4.0d+0) 
term(80) = term(80) * (-2.0d+0) 
term(81) = term(81) * (2.0d+0) 
term(82) = term(82) * (-2.0d+0) 
term(83) = term(83) * (2.0d+0) 
term(84) = term(84) * (-2.0d+0) 
term(85) = term(85) * (2.0d+0) 
term(86) = term(86) * (-2.0d+0) 
term(87) = term(87) * (2.0d+0) 
term(88) = term(88) * (-0.5d+0) 
term(90) = term(90) * (-0.5d+0) 
term(91) = term(91) * (-0.5d+0) 
term(93) = term(93) * (-0.5d+0) 
term(94) = term(94) * (-0.5d+0) 
term(96) = term(96) * (-0.5d+0) 
term(97) = term(97) * (-0.5d+0) 
term(99) = term(99) * (-0.5d+0) 
term(100) = term(100) * (-0.5d+0) 
term(101) = term(101) * (-0.5d+0) 
term(103) = term(103) * (-0.5d+0) 
term(104) = term(104) * (-0.5d+0) 
term(108) = term(108) * (-2.0d+0) 
term(109) = term(109) * (-2.0d+0) 
term(110) = term(110) * (2.0d+0) 
term(111) = term(111) * (-2.0d+0) 
term(112) = term(112) * (-2.0d+0) 
term(113) = term(113) * (2.0d+0) 
term(114) = term(114) * (-2.0d+0) 
term(115) = term(115) * (2.0d+0) 
term(116) = term(116) * (2.0d+0) 
term(117) = term(117) * (-2.0d+0) 
term(118) = term(118) * (-2.0d+0) 
term(119) = term(119) * (4.0d+0) 
term(120) = term(120) * (2.0d+0) 
term(121) = term(121) * (2.0d+0) 
term(122) = term(122) * (-4.0d+0) 
term(123) = term(123) * (-1.0d+0) 
term(124) = term(124) * (2.0d+0) 
term(125) = term(125) * (-1.0d+0) 
term(126) = term(126) * (2.0d+0) 
term(127) = term(127) * (-1.0d+0) 
term(128) = term(128) * (-1.0d+0) 
term(129) = term(129) * (2.0d+0) 
term(130) = term(130) * (-4.0d+0) 
term(131) = term(131) * (-1.0d+0) 
term(132) = term(132) * (2.0d+0) 
term(133) = term(133) * (2.0d+0) 
term(134) = term(134) * (-1.0d+0) 
term(135) = term(135) * (-1.0d+0) 
term(136) = term(136) * (2.0d+0) 
term(137) = term(137) * (-1.0d+0) 
term(138) = term(138) * (-1.0d+0) 
term(139) = term(139) * (2.0d+0) 
term(140) = term(140) * (-1.0d+0) 
term(141) = term(141) * (-1.0d+0) 
term(142) = term(142) * (2.0d+0) 
term(143) = term(143) * (-1.0d+0) 
term(144) = term(144) * (-4.0d+0) 
term(145) = term(145) * (4.0d+0) 
term(146) = term(146) * (-4.0d+0) 
term(147) = term(147) * (4.0d+0) 
term(148) = term(148) * (8.0d+0) 
term(149) = term(149) * (-8.0d+0) 
term(150) = term(150) * (-4.0d+0) 
term(151) = term(151) * (4.0d+0) 
term(152) = term(152) * (-4.0d+0) 
term(153) = term(153) * (4.0d+0) 
term(154) = term(154) * (-4.0d+0) 
term(155) = term(155) * (4.0d+0) 
term(156) = term(156) * (-4.0d+0) 
term(157) = term(157) * (4.0d+0) 
term(158) = term(158) * (-2.0d+0) 
term(159) = term(159) * (2.0d+0) 
term(160) = term(160) * (-2.0d+0) 
term(161) = term(161) * (2.0d+0) 
term(162) = term(162) * (-2.0d+0) 
term(163) = term(163) * (2.0d+0) 
term(164) = term(164) * (-2.0d+0) 
term(165) = term(165) * (2.0d+0) 
term(166) = term(166) * (-2.0d+0) 
term(167) = term(167) * (2.0d+0) 
term(168) = term(168) * (-2.0d+0) 
term(169) = term(169) * (2.0d+0) 
term(170) = term(170) * (4.0d+0) 
term(171) = term(171) * (-4.0d+0) 
term(172) = term(172) * (-8.0d+0) 
term(173) = term(173) * (8.0d+0) 
term(174) = term(174) * (-8.0d+0) 
term(175) = term(175) * (8.0d+0) 
term(176) = term(176) * (-8.0d+0) 
term(177) = term(177) * (8.0d+0) 
term(178) = term(178) * (8.0d+0) 
term(179) = term(179) * (-8.0d+0) 
term(180) = term(180) * (2.0d+0) 
term(181) = term(181) * (-4.0d+0) 
term(182) = term(182) * (2.0d+0) 
term(183) = term(183) * (2.0d+0) 
term(184) = term(184) * (-4.0d+0) 
term(185) = term(185) * (-4.0d+0) 
term(186) = term(186) * (8.0d+0) 
term(187) = term(187) * (2.0d+0) 
term(188) = term(188) * (-4.0d+0) 
term(189) = term(189) * (-4.0d+0) 
term(190) = term(190) * (8.0d+0) 
term(191) = term(191) * (-4.0d+0) 
term(192) = term(192) * (4.0d+0) 
term(193) = term(193) * (-8.0d+0) 
term(194) = term(194) * (4.0d+0) 
term(195) = term(195) * (4.0d+0) 
term(196) = term(196) * (-8.0d+0) 
term(197) = term(197) * (-8.0d+0) 
term(198) = term(198) * (16.0d+0) 
term(199) = term(199) * (4.0d+0) 
term(200) = term(200) * (-8.0d+0) 
term(201) = term(201) * (-8.0d+0) 
term(202) = term(202) * (16.0d+0) 
term(203) = term(203) * (-8.0d+0) 
term(204) = term(204) * (-1.0d+0) 
term(205) = term(205) * (2.0d+0) 
term(206) = term(206) * (-1.0d+0) 
term(207) = term(207) * (2.0d+0) 
term(208) = term(208) * (2.0d+0) 
term(209) = term(209) * (-4.0d+0) 
term(210) = term(210) * (2.0d+0) 
term(211) = term(211) * (-4.0d+0) 
term(212) = term(212) * (2.0d+0) 
term(213) = term(213) * (-4.0d+0) 
term(214) = term(214) * (-4.0d+0) 
term(215) = term(215) * (8.0d+0) 
term(216) = term(216) * (-1.0d+0) 
term(217) = term(217) * (2.0d+0) 
term(218) = term(218) * (-1.0d+0) 
term(219) = term(219) * (-4.0d+0) 
term(220) = term(220) * (8.0d+0) 
term(221) = term(221) * (4.0d+0) 
term(222) = term(222) * (-8.0d+0) 
term(223) = term(223) * (8.0d+0) 
term(224) = term(224) * (-16.0d+0) 
term(225) = term(225) * (-8.0d+0) 
term(226) = term(226) * (16.0d+0) 
term(227) = term(227) * (-2.0d+0) 
term(228) = term(228) * (4.0d+0) 
term(229) = term(229) * (-2.0d+0) 
term(230) = term(230) * (2.0d+0) 
term(231) = term(231) * (-4.0d+0) 
term(232) = term(232) * (2.0d+0) 
term(233) = term(233) * (-4.0d+0) 
term(234) = term(234) * (-4.0d+0) 
term(235) = term(235) * (8.0d+0) 
term(236) = term(236) * (2.0d+0) 
term(237) = term(237) * (-4.0d+0) 
term(238) = term(238) * (2.0d+0) 
term(239) = term(239) * (-4.0d+0) 
term(240) = term(240) * (-4.0d+0) 
term(241) = term(241) * (8.0d+0) 
term(242) = term(242) * (2.0d+0) 
term(243) = term(243) * (2.0d+0) 
term(244) = term(244) * (-4.0d+0) 
term(245) = term(245) * (-4.0d+0) 
term(246) = term(246) * (-4.0d+0) 
term(247) = term(247) * (8.0d+0) 
term(248) = term(248) * (2.0d+0) 
term(249) = term(249) * (2.0d+0) 
term(250) = term(250) * (-4.0d+0) 
term(251) = term(251) * (-1.0d+0) 
term(252) = term(252) * (2.0d+0) 
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
term(263) = term(263) * (8.0d+0) 
term(264) = term(264) * (-16.0d+0) 
term(265) = term(265) * (8.0d+0) 
term(266) = term(266) * (-16.0d+0) 
term(267) = term(267) * (-8.0d+0) 
term(268) = term(268) * (16.0d+0) 
term(269) = term(269) * (-8.0d+0) 
term(270) = term(270) * (16.0d+0) 
term(271) = term(271) * (8.0d+0) 
term(272) = term(272) * (8.0d+0) 
term(273) = term(273) * (-16.0d+0) 
term(274) = term(274) * (-8.0d+0) 
term(275) = term(275) * (-8.0d+0) 
term(276) = term(276) * (16.0d+0) 
term(277) = term(277) * (-4.0d+0) 
term(278) = term(278) * (8.0d+0) 
term(279) = term(279) * (4.0d+0) 
term(280) = term(280) * (-8.0d+0) 
term(281) = term(281) * (-4.0d+0) 
term(282) = term(282) * (8.0d+0) 
term(283) = term(283) * (4.0d+0) 
term(284) = term(284) * (-8.0d+0) 

do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
term(285) = term(285) + r1(vrdav_Rr, p,i) * wm_interm_196_triplet_pt3(j,k) * wm_interm_99_triplet_pt3(i,k,j,q)
term(286) = term(286) + r1(vrdav_Rr, p,i) * wm_interm_197_triplet_pt3(j,k) * wm_interm_99_triplet_pt3(i,k,j,q)
term(287) = term(287) + r1(vrdav_Rr, p,i) * wm_interm_196_triplet_pt3(j,k) * wm_interm_99_triplet_pt3(i,k,q,j)
term(288) = term(288) + r1(vrdav_Rr, p,i) * wm_interm_197_triplet_pt3(j,k) * wm_interm_99_triplet_pt3(i,k,q,j)
term(289) = term(289) + r1(vrdav_Rr, p,i) * wm_interm_196_triplet_pt3(j,k) * wm_interm_99_triplet_pt3(k,i,q,j)
term(290) = term(290) + r1(vrdav_Rr, p,i) * wm_interm_197_triplet_pt3(j,k) * wm_interm_99_triplet_pt3(k,i,q,j)
term(291) = term(291) + r1(vrdav_Rr, p,i) * wm_interm_145_triplet_pt3(j,k) * wm_interm_191_triplet_pt3(i,k,j,q)
term(292) = term(292) + r1(vrdav_Rr, p,i) * wm_interm_147_triplet_pt3(j,k) * wm_interm_191_triplet_pt3(i,k,j,q)
end do 
end do 
end do 

term(285) = term(285) * (-1.0d+0) 
term(286) = term(286) * (2.0d+0) 
term(287) = term(287) * (2.0d+0) 
term(288) = term(288) * (-4.0d+0) 
term(289) = term(289) * (-1.0d+0) 
term(290) = term(290) * (2.0d+0) 
term(291) = term(291) * (-4.0d+0) 
term(292) = term(292) * (4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(293) = term(293) + t2(a,p,i,q) * wm_interm_171_triplet_pt3(b,i,j,k) * wm_interm_193_triplet_pt3(a,b,k,j)
term(294) = term(294) + t2(a,p,i,q) * wm_interm_171_triplet_pt3(b,i,j,k) * wm_interm_194_triplet_pt3(a,b,k,j)
term(295) = term(295) + t2(a,p,i,q) * wm_interm_175_triplet_pt3(b,i,j,k) * wm_interm_193_triplet_pt3(a,b,k,j)
term(296) = term(296) + t2(a,p,i,q) * wm_interm_175_triplet_pt3(b,i,j,k) * wm_interm_194_triplet_pt3(a,b,k,j)
term(297) = term(297) + t2(a,p,i,q) * wm_interm_175_triplet_pt3(b,i,j,k) * wm_interm_190_triplet_pt3(a,b,k,j)
term(298) = term(298) + t2(a,p,i,q) * wm_interm_171_triplet_pt3(b,i,j,k) * wm_interm_192_triplet_pt3(a,b,k,j)
term(299) = term(299) + t2(a,p,i,q) * wm_interm_171_triplet_pt3(b,i,j,k) * wm_interm_190_triplet_pt3(a,b,k,j)
term(300) = term(300) + t2(a,p,i,q) * wm_interm_179_triplet_pt3(b,i,j,k) * wm_interm_193_triplet_pt3(a,b,k,j)
term(301) = term(301) + t2(a,p,i,q) * wm_interm_179_triplet_pt3(b,i,j,k) * wm_interm_194_triplet_pt3(a,b,k,j)
term(302) = term(302) + t2(a,p,i,q) * wm_interm_182_triplet_pt3(b,i,j,k) * wm_interm_193_triplet_pt3(a,b,k,j)
term(303) = term(303) + t2(a,p,i,q) * wm_interm_182_triplet_pt3(b,i,j,k) * wm_interm_194_triplet_pt3(a,b,k,j)
term(304) = term(304) + t2(a,p,i,q) * wm_interm_182_triplet_pt3(b,i,j,k) * wm_interm_190_triplet_pt3(a,b,k,j)
term(305) = term(305) + t2(a,p,i,q) * wm_interm_179_triplet_pt3(b,i,j,k) * wm_interm_192_triplet_pt3(a,b,k,j)
term(306) = term(306) + t2(a,p,i,q) * wm_interm_179_triplet_pt3(b,i,j,k) * wm_interm_190_triplet_pt3(a,b,k,j)
end do 
end do 
end do 
end do 
end do 

term(293) = term(293) * (-4.0d+0) 
term(294) = term(294) * (8.0d+0) 
term(295) = term(295) * (2.0d+0) 
term(296) = term(296) * (-4.0d+0) 
term(297) = term(297) * (2.0d+0) 
term(298) = term(298) * (2.0d+0) 
term(299) = term(299) * (-4.0d+0) 
term(300) = term(300) * (-8.0d+0) 
term(301) = term(301) * (16.0d+0) 
term(302) = term(302) * (4.0d+0) 
term(303) = term(303) * (-8.0d+0) 
term(304) = term(304) * (4.0d+0) 
term(305) = term(305) * (4.0d+0) 
term(306) = term(306) * (-8.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(307) = term(307) + r1(vrdav_Rl, a,i) * wm_interm_191_triplet_pt3(q,j,i,k) * wm_interm_21_triplet_pt3(a,p,j,k)
term(308) = term(308) + r1(vrdav_Rl, a,i) * wm_interm_191_triplet_pt3(q,j,i,k) * wm_interm_22_triplet_pt3(a,p,j,k)
term(309) = term(309) + r1(vrdav_Rl, a,i) * wm_interm_191_triplet_pt3(j,q,i,k) * wm_interm_21_triplet_pt3(a,p,j,k)
term(310) = term(310) + r1(vrdav_Rl, a,i) * wm_interm_191_triplet_pt3(j,q,i,k) * wm_interm_22_triplet_pt3(a,p,j,k)
term(311) = term(311) + r1(vrdav_Rl, a,i) * wm_interm_191_triplet_pt3(j,q,i,k) * wm_interm_24_triplet_pt3(a,p,j,k)
term(312) = term(312) + r1(vrdav_Rl, a,i) * wm_interm_191_triplet_pt3(j,q,i,k) * wm_interm_23_triplet_pt3(a,p,j,k)
term(313) = term(313) + r1(vrdav_Rl, a,i) * wm_interm_191_triplet_pt3(q,j,i,k) * wm_interm_23_triplet_pt3(a,p,j,k)
term(314) = term(314) + r1(vrdav_Rl, a,i) * wm_interm_191_triplet_pt3(q,j,i,k) * wm_interm_24_triplet_pt3(a,p,j,k)
term(315) = term(315) + r1(vrdav_Rl, a,i) * wm_interm_191_triplet_pt3(q,j,i,k) * wm_interm_26_triplet_pt3(a,p,j,k)
term(316) = term(316) + r1(vrdav_Rl, a,i) * wm_interm_191_triplet_pt3(j,q,i,k) * wm_interm_26_triplet_pt3(a,p,j,k)
term(317) = term(317) + r1(vrdav_Rl, a,i) * wm_interm_191_triplet_pt3(j,q,i,k) * wm_interm_30_triplet_pt3(a,p,j,k)
term(318) = term(318) + r1(vrdav_Rl, a,i) * wm_interm_191_triplet_pt3(q,j,i,k) * wm_interm_25_triplet_pt3(a,p,j,k)
term(319) = term(319) + r1(vrdav_Rl, a,i) * wm_interm_191_triplet_pt3(q,j,i,k) * wm_interm_58_triplet_pt3(a,p,j,k)
term(320) = term(320) + r1(vrdav_Rl, a,i) * wm_interm_191_triplet_pt3(q,j,i,k) * wm_interm_59_triplet_pt3(a,p,j,k)
term(321) = term(321) + r1(vrdav_Rl, a,i) * wm_interm_191_triplet_pt3(j,q,i,k) * wm_interm_58_triplet_pt3(a,p,j,k)
term(322) = term(322) + r1(vrdav_Rl, a,i) * wm_interm_191_triplet_pt3(j,q,i,k) * wm_interm_59_triplet_pt3(a,p,j,k)
term(323) = term(323) + r1(vrdav_Rl, a,i) * wm_interm_191_triplet_pt3(j,q,i,k) * wm_interm_61_triplet_pt3(a,p,j,k)
term(324) = term(324) + r1(vrdav_Rl, a,i) * wm_interm_191_triplet_pt3(j,q,i,k) * wm_interm_60_triplet_pt3(a,p,j,k)
term(325) = term(325) + r1(vrdav_Rl, a,i) * wm_interm_191_triplet_pt3(q,j,i,k) * wm_interm_60_triplet_pt3(a,p,j,k)
term(326) = term(326) + r1(vrdav_Rl, a,i) * wm_interm_191_triplet_pt3(q,j,i,k) * wm_interm_61_triplet_pt3(a,p,j,k)
term(327) = term(327) + r1(vrdav_Rr, a,i) * wm_interm_193_triplet_pt3(a,p,j,k) * wm_interm_99_triplet_pt3(i,k,q,j)
term(328) = term(328) + r1(vrdav_Rr, a,i) * wm_interm_193_triplet_pt3(a,p,j,k) * wm_interm_99_triplet_pt3(k,i,j,q)
term(329) = term(329) + r1(vrdav_Rr, a,i) * wm_interm_193_triplet_pt3(a,p,j,k) * wm_interm_99_triplet_pt3(i,k,j,q)
term(330) = term(330) + r1(vrdav_Rr, a,i) * wm_interm_190_triplet_pt3(a,p,j,k) * wm_interm_99_triplet_pt3(k,i,j,q)
term(331) = term(331) + r1(vrdav_Rr, a,i) * wm_interm_190_triplet_pt3(a,p,j,k) * wm_interm_99_triplet_pt3(i,k,q,j)
term(332) = term(332) + r1(vrdav_Rr, a,i) * wm_interm_194_triplet_pt3(a,p,j,k) * wm_interm_99_triplet_pt3(i,k,q,j)
term(333) = term(333) + r1(vrdav_Rr, a,i) * wm_interm_194_triplet_pt3(a,p,j,k) * wm_interm_99_triplet_pt3(k,i,j,q)
term(334) = term(334) + r1(vrdav_Rr, a,i) * wm_interm_190_triplet_pt3(a,p,j,k) * wm_interm_99_triplet_pt3(i,k,j,q)
term(335) = term(335) + r1(vrdav_Rr, a,i) * wm_interm_194_triplet_pt3(a,p,j,k) * wm_interm_99_triplet_pt3(i,k,j,q)
term(336) = term(336) + r1(vrdav_Rr, a,i) * wm_interm_192_triplet_pt3(a,p,j,k) * wm_interm_99_triplet_pt3(k,i,q,j)
term(337) = term(337) + r1(vrdav_Rr, a,i) * wm_interm_192_triplet_pt3(a,p,j,k) * wm_interm_99_triplet_pt3(i,k,j,q)
term(338) = term(338) + r1(vrdav_Rr, a,i) * wm_interm_192_triplet_pt3(a,p,j,k) * wm_interm_99_triplet_pt3(i,k,q,j)
term(339) = term(339) + r1(vrdav_Rr, a,i) * wm_interm_104_triplet_pt3(a,p,j,k) * wm_interm_191_triplet_pt3(k,i,j,q)
term(340) = term(340) + r1(vrdav_Rr, a,i) * wm_interm_154_triplet_pt3(i,j,q,k) * wm_interm_193_triplet_pt3(a,p,k,j)
term(341) = term(341) + r1(vrdav_Rr, a,i) * wm_interm_154_triplet_pt3(i,j,q,k) * wm_interm_190_triplet_pt3(a,p,k,j)
term(342) = term(342) + r1(vrdav_Rr, a,i) * wm_interm_154_triplet_pt3(i,j,q,k) * wm_interm_194_triplet_pt3(a,p,k,j)
term(343) = term(343) + r1(vrdav_Rr, a,i) * wm_interm_154_triplet_pt3(j,i,q,k) * wm_interm_192_triplet_pt3(a,p,k,j)
term(344) = term(344) + r1(vrdav_Rr, a,i) * wm_interm_154_triplet_pt3(i,j,q,k) * wm_interm_192_triplet_pt3(a,p,k,j)
term(345) = term(345) + r1(vrdav_Rr, a,i) * wm_interm_153_triplet_pt3(a,p,j,k) * wm_interm_191_triplet_pt3(k,i,j,q)
term(346) = term(346) + r1(vrdav_Rr, a,i) * wm_interm_152_triplet_pt3(a,p,j,k) * wm_interm_191_triplet_pt3(k,i,j,q)
term(347) = term(347) + r1(vrdav_Rr, a,i) * wm_interm_152_triplet_pt3(a,p,j,k) * wm_interm_191_triplet_pt3(i,k,j,q)
term(348) = term(348) + r1(vrdav_Rr, a,i) * wm_interm_153_triplet_pt3(a,p,j,k) * wm_interm_191_triplet_pt3(i,k,j,q)
term(349) = term(349) + r1(vrdav_Rr, a,i) * wm_interm_150_triplet_pt3(a,p,j,k) * wm_interm_191_triplet_pt3(k,i,j,q)
term(350) = term(350) + r1(vrdav_Rr, a,i) * wm_interm_150_triplet_pt3(a,p,j,k) * wm_interm_191_triplet_pt3(i,k,j,q)
term(351) = term(351) + r1(vrdav_Rr, a,i) * wm_interm_151_triplet_pt3(a,p,j,k) * wm_interm_191_triplet_pt3(k,i,j,q)
term(352) = term(352) + r1(vrdav_Rr, a,i) * wm_interm_151_triplet_pt3(a,p,j,k) * wm_interm_191_triplet_pt3(i,k,j,q)
end do 
end do 
end do 
end do 

term(307) = term(307) * (2.0d+0) 
term(308) = term(308) * (-4.0d+0) 
term(309) = term(309) * (-4.0d+0) 
term(310) = term(310) * (8.0d+0) 
term(311) = term(311) * (2.0d+0) 
term(312) = term(312) * (-4.0d+0) 
term(313) = term(313) * (2.0d+0) 
term(314) = term(314) * (-4.0d+0) 
term(315) = term(315) * (2.0d+0) 
term(316) = term(316) * (-4.0d+0) 
term(317) = term(317) * (2.0d+0) 
term(318) = term(318) * (2.0d+0) 
term(319) = term(319) * (8.0d+0) 
term(320) = term(320) * (-8.0d+0) 
term(321) = term(321) * (-16.0d+0) 
term(322) = term(322) * (16.0d+0) 
term(323) = term(323) * (8.0d+0) 
term(324) = term(324) * (-8.0d+0) 
term(325) = term(325) * (8.0d+0) 
term(326) = term(326) * (-8.0d+0) 
term(327) = term(327) * (-2.0d+0) 
term(328) = term(328) * (-2.0d+0) 
term(329) = term(329) * (4.0d+0) 
term(330) = term(330) * (-2.0d+0) 
term(331) = term(331) * (-2.0d+0) 
term(332) = term(332) * (4.0d+0) 
term(333) = term(333) * (4.0d+0) 
term(334) = term(334) * (4.0d+0) 
term(335) = term(335) * (-8.0d+0) 
term(336) = term(336) * (-2.0d+0) 
term(337) = term(337) * (-2.0d+0) 
term(338) = term(338) * (4.0d+0) 
term(339) = term(339) * (-2.0d+0) 
term(340) = term(340) * (-4.0d+0) 
term(341) = term(341) * (-4.0d+0) 
term(342) = term(342) * (8.0d+0) 
term(343) = term(343) * (-4.0d+0) 
term(344) = term(344) * (8.0d+0) 
term(345) = term(345) * (-8.0d+0) 
term(346) = term(346) * (8.0d+0) 
term(347) = term(347) * (-8.0d+0) 
term(348) = term(348) * (8.0d+0) 
term(349) = term(349) * (-8.0d+0) 
term(350) = term(350) * (16.0d+0) 
term(351) = term(351) * (8.0d+0) 
term(352) = term(352) * (-16.0d+0) 

do l = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(353) = term(353) + s2(a,p,q,i) * wm_interm_162_triplet_pt3(a,j,k,l) * wm_interm_191_triplet_pt3(l,k,j,i)
term(354) = term(354) + s2(a,p,q,i) * wm_interm_162_triplet_pt3(a,j,k,l) * wm_interm_191_triplet_pt3(k,l,j,i)
term(355) = term(355) + s2(a,p,i,q) * wm_interm_162_triplet_pt3(a,j,k,l) * wm_interm_191_triplet_pt3(l,k,j,i)
term(356) = term(356) + s2(a,p,i,q) * wm_interm_162_triplet_pt3(a,j,k,l) * wm_interm_191_triplet_pt3(k,l,j,i)
term(357) = term(357) + s2(a,p,q,i) * wm_interm_161_triplet_pt3(a,j,k,l) * wm_interm_191_triplet_pt3(k,l,j,i)
term(358) = term(358) + s2(a,p,i,q) * wm_interm_161_triplet_pt3(a,j,k,l) * wm_interm_191_triplet_pt3(k,l,j,i)
term(359) = term(359) + s2(a,p,q,i) * wm_interm_189_triplet_pt3(a,j,k,l) * wm_interm_20_triplet_pt3(i,j,k,l)
term(360) = term(360) + s2(a,p,q,i) * wm_interm_189_triplet_pt3(a,j,k,l) * wm_interm_20_triplet_pt3(j,i,k,l)
term(361) = term(361) + s2(a,p,j,i) * wm_interm_189_triplet_pt3(a,q,k,l) * wm_interm_20_triplet_pt3(j,i,k,l)
term(362) = term(362) + s2(a,p,j,i) * wm_interm_189_triplet_pt3(a,q,k,l) * wm_interm_20_triplet_pt3(i,j,k,l)
term(363) = term(363) + s2(a,p,q,i) * wm_interm_164_triplet_pt3(a,j,k,l) * wm_interm_191_triplet_pt3(l,k,j,i)
term(364) = term(364) + s2(a,p,q,i) * wm_interm_164_triplet_pt3(a,j,k,l) * wm_interm_191_triplet_pt3(k,l,j,i)
term(365) = term(365) + s2(a,p,i,q) * wm_interm_164_triplet_pt3(a,j,k,l) * wm_interm_191_triplet_pt3(l,k,j,i)
term(366) = term(366) + s2(a,p,i,q) * wm_interm_164_triplet_pt3(a,j,k,l) * wm_interm_191_triplet_pt3(k,l,j,i)
term(367) = term(367) + s2(a,p,q,i) * wm_interm_189_triplet_pt3(a,j,k,l) * wm_interm_57_triplet_pt3(i,j,k,l)
term(368) = term(368) + s2(a,p,q,i) * wm_interm_189_triplet_pt3(a,j,k,l) * wm_interm_57_triplet_pt3(j,i,k,l)
term(369) = term(369) + s2(a,p,j,i) * wm_interm_189_triplet_pt3(a,q,k,l) * wm_interm_57_triplet_pt3(j,i,k,l)
term(370) = term(370) + s2(a,p,j,i) * wm_interm_189_triplet_pt3(a,q,k,l) * wm_interm_57_triplet_pt3(i,j,k,l)
term(371) = term(371) + t2(a,p,q,i) * wm_interm_185_triplet_pt3(a,j,k,l) * wm_interm_99_triplet_pt3(i,j,k,l)
term(372) = term(372) + t2(a,p,q,i) * wm_interm_185_triplet_pt3(a,j,k,l) * wm_interm_99_triplet_pt3(j,i,k,l)
term(373) = term(373) + t2(a,p,q,i) * wm_interm_154_triplet_pt3(i,j,k,l) * wm_interm_185_triplet_pt3(a,j,k,l)
term(374) = term(374) + t2(a,p,q,i) * wm_interm_154_triplet_pt3(j,i,k,l) * wm_interm_185_triplet_pt3(a,j,k,l)
end do 
end do 
end do 
end do 
end do 

term(353) = term(353) * (2.0d+0) 
term(354) = term(354) * (-4.0d+0) 
term(355) = term(355) * (-4.0d+0) 
term(356) = term(356) * (8.0d+0) 
term(357) = term(357) * (2.0d+0) 
term(358) = term(358) * (-4.0d+0) 
term(359) = term(359) * (2.0d+0) 
term(360) = term(360) * (-4.0d+0) 
term(361) = term(361) * (2.0d+0) 
term(362) = term(362) * (-4.0d+0) 
term(363) = term(363) * (8.0d+0) 
term(364) = term(364) * (-8.0d+0) 
term(365) = term(365) * (-16.0d+0) 
term(366) = term(366) * (16.0d+0) 
term(367) = term(367) * (4.0d+0) 
term(368) = term(368) * (-8.0d+0) 
term(369) = term(369) * (4.0d+0) 
term(370) = term(370) * (-8.0d+0) 
term(371) = term(371) * (-1.0d+0) 
term(372) = term(372) * (2.0d+0) 
term(373) = term(373) * (-2.0d+0) 
term(374) = term(374) * (4.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(375) = term(375) + r1(vrdav_Rl, a,i) * wm_interm_192_triplet_pt3(p,b,j,i) * wm_interm_24_triplet_pt3(a,b,j,q)
term(376) = term(376) + r1(vrdav_Rl, a,i) * wm_interm_192_triplet_pt3(p,b,j,i) * wm_interm_23_triplet_pt3(a,b,j,q)
term(377) = term(377) + r1(vrdav_Rl, a,i) * wm_interm_192_triplet_pt3(p,b,j,i) * wm_interm_21_triplet_pt3(a,b,j,q)
term(378) = term(378) + r1(vrdav_Rl, a,i) * wm_interm_192_triplet_pt3(p,b,j,i) * wm_interm_22_triplet_pt3(a,b,j,q)
term(379) = term(379) + r1(vrdav_Rl, a,i) * wm_interm_190_triplet_pt3(p,b,j,i) * wm_interm_21_triplet_pt3(a,b,j,q)
term(380) = term(380) + r1(vrdav_Rl, a,i) * wm_interm_190_triplet_pt3(p,b,j,i) * wm_interm_22_triplet_pt3(a,b,j,q)
term(381) = term(381) + r1(vrdav_Rl, a,i) * wm_interm_190_triplet_pt3(p,b,j,i) * wm_interm_23_triplet_pt3(a,b,j,q)
term(382) = term(382) + r1(vrdav_Rl, a,i) * wm_interm_190_triplet_pt3(p,b,j,i) * wm_interm_24_triplet_pt3(a,b,j,q)
term(383) = term(383) + r1(vrdav_Rl, a,i) * wm_interm_192_triplet_pt3(p,b,j,i) * wm_interm_30_triplet_pt3(a,b,j,q)
term(384) = term(384) + r1(vrdav_Rl, a,i) * wm_interm_192_triplet_pt3(p,b,j,i) * wm_interm_26_triplet_pt3(a,b,j,q)
term(385) = term(385) + r1(vrdav_Rl, a,i) * wm_interm_190_triplet_pt3(p,b,j,i) * wm_interm_26_triplet_pt3(a,b,j,q)
term(386) = term(386) + r1(vrdav_Rl, a,i) * wm_interm_190_triplet_pt3(p,b,j,i) * wm_interm_25_triplet_pt3(a,b,j,q)
term(387) = term(387) + r1(vrdav_Rl, a,i) * wm_interm_193_triplet_pt3(p,b,j,i) * wm_interm_21_triplet_pt3(a,b,j,q)
term(388) = term(388) + r1(vrdav_Rl, a,i) * wm_interm_193_triplet_pt3(p,b,j,i) * wm_interm_22_triplet_pt3(a,b,j,q)
term(389) = term(389) + r1(vrdav_Rl, a,i) * wm_interm_194_triplet_pt3(p,b,j,i) * wm_interm_21_triplet_pt3(a,b,j,q)
term(390) = term(390) + r1(vrdav_Rl, a,i) * wm_interm_194_triplet_pt3(p,b,j,i) * wm_interm_22_triplet_pt3(a,b,j,q)
term(391) = term(391) + r1(vrdav_Rl, a,i) * wm_interm_193_triplet_pt3(p,b,j,i) * wm_interm_26_triplet_pt3(a,b,j,q)
term(392) = term(392) + r1(vrdav_Rl, a,i) * wm_interm_194_triplet_pt3(p,b,j,i) * wm_interm_26_triplet_pt3(a,b,j,q)
term(393) = term(393) + r1(vrdav_Rl, a,i) * wm_interm_193_triplet_pt3(p,b,j,i) * wm_interm_23_triplet_pt3(a,b,j,q)
term(394) = term(394) + r1(vrdav_Rl, a,i) * wm_interm_193_triplet_pt3(p,b,j,i) * wm_interm_24_triplet_pt3(a,b,j,q)
term(395) = term(395) + r1(vrdav_Rl, a,i) * wm_interm_194_triplet_pt3(p,b,j,i) * wm_interm_23_triplet_pt3(a,b,j,q)
term(396) = term(396) + r1(vrdav_Rl, a,i) * wm_interm_194_triplet_pt3(p,b,j,i) * wm_interm_24_triplet_pt3(a,b,j,q)
term(397) = term(397) + r1(vrdav_Rl, a,i) * wm_interm_193_triplet_pt3(p,b,j,i) * wm_interm_25_triplet_pt3(a,b,j,q)
term(398) = term(398) + r1(vrdav_Rl, a,i) * wm_interm_194_triplet_pt3(p,b,j,i) * wm_interm_25_triplet_pt3(a,b,j,q)
term(399) = term(399) + s2(a,p,j,i) * wm_interm_189_triplet_pt3(b,q,j,i) * wm_interm_9_triplet_pt3(a,b)
term(400) = term(400) + s2(a,p,j,i) * wm_interm_189_triplet_pt3(b,q,j,i) * wm_interm_5_triplet_pt3(a,b)
term(401) = term(401) + s2(a,p,j,i) * wm_interm_189_triplet_pt3(b,q,j,i) * wm_interm_6_triplet_pt3(a,b)
term(402) = term(402) + r1(vrdav_Rl, a,i) * wm_interm_192_triplet_pt3(p,b,j,i) * wm_interm_61_triplet_pt3(a,b,j,q)
term(403) = term(403) + r1(vrdav_Rl, a,i) * wm_interm_192_triplet_pt3(p,b,j,i) * wm_interm_60_triplet_pt3(a,b,j,q)
term(404) = term(404) + r1(vrdav_Rl, a,i) * wm_interm_192_triplet_pt3(p,b,j,i) * wm_interm_58_triplet_pt3(a,b,j,q)
term(405) = term(405) + r1(vrdav_Rl, a,i) * wm_interm_192_triplet_pt3(p,b,j,i) * wm_interm_59_triplet_pt3(a,b,j,q)
term(406) = term(406) + r1(vrdav_Rl, a,i) * wm_interm_190_triplet_pt3(p,b,j,i) * wm_interm_58_triplet_pt3(a,b,j,q)
term(407) = term(407) + r1(vrdav_Rl, a,i) * wm_interm_190_triplet_pt3(p,b,j,i) * wm_interm_59_triplet_pt3(a,b,j,q)
term(408) = term(408) + r1(vrdav_Rl, a,i) * wm_interm_190_triplet_pt3(p,b,j,i) * wm_interm_60_triplet_pt3(a,b,j,q)
term(409) = term(409) + r1(vrdav_Rl, a,i) * wm_interm_190_triplet_pt3(p,b,j,i) * wm_interm_61_triplet_pt3(a,b,j,q)
term(410) = term(410) + r1(vrdav_Rl, a,i) * wm_interm_193_triplet_pt3(p,b,j,i) * wm_interm_58_triplet_pt3(a,b,j,q)
term(411) = term(411) + r1(vrdav_Rl, a,i) * wm_interm_193_triplet_pt3(p,b,j,i) * wm_interm_59_triplet_pt3(a,b,j,q)
term(412) = term(412) + r1(vrdav_Rl, a,i) * wm_interm_194_triplet_pt3(p,b,j,i) * wm_interm_58_triplet_pt3(a,b,j,q)
term(413) = term(413) + r1(vrdav_Rl, a,i) * wm_interm_194_triplet_pt3(p,b,j,i) * wm_interm_59_triplet_pt3(a,b,j,q)
term(414) = term(414) + r1(vrdav_Rl, a,i) * wm_interm_193_triplet_pt3(p,b,j,i) * wm_interm_60_triplet_pt3(a,b,j,q)
term(415) = term(415) + r1(vrdav_Rl, a,i) * wm_interm_193_triplet_pt3(p,b,j,i) * wm_interm_61_triplet_pt3(a,b,j,q)
term(416) = term(416) + r1(vrdav_Rl, a,i) * wm_interm_194_triplet_pt3(p,b,j,i) * wm_interm_60_triplet_pt3(a,b,j,q)
term(417) = term(417) + r1(vrdav_Rl, a,i) * wm_interm_194_triplet_pt3(p,b,j,i) * wm_interm_61_triplet_pt3(a,b,j,q)
term(418) = term(418) + s2(a,p,j,i) * wm_interm_189_triplet_pt3(b,q,j,i) * wm_interm_195_triplet_pt3(a,b)
term(419) = term(419) + s2(a,p,j,i) * wm_interm_189_triplet_pt3(b,q,j,i) * wm_interm_47_triplet_pt3(a,b)
term(420) = term(420) + s2(a,p,j,i) * wm_interm_189_triplet_pt3(b,q,j,i) * wm_interm_48_triplet_pt3(a,b)
term(421) = term(421) + t2(a,p,j,i) * wm_interm_176_triplet_pt3(b,i) * wm_interm_193_triplet_pt3(a,b,j,q)
term(422) = term(422) + t2(a,p,j,i) * wm_interm_174_triplet_pt3(b,i) * wm_interm_193_triplet_pt3(a,b,j,q)
term(423) = term(423) + t2(a,p,j,i) * wm_interm_172_triplet_pt3(b,i) * wm_interm_193_triplet_pt3(a,b,j,q)
term(424) = term(424) + t2(a,p,j,i) * wm_interm_176_triplet_pt3(b,i) * wm_interm_192_triplet_pt3(a,b,j,q)
term(425) = term(425) + t2(a,p,j,i) * wm_interm_174_triplet_pt3(b,i) * wm_interm_192_triplet_pt3(a,b,j,q)
term(426) = term(426) + t2(a,p,j,i) * wm_interm_172_triplet_pt3(b,i) * wm_interm_192_triplet_pt3(a,b,j,q)
term(427) = term(427) + t2(a,p,j,i) * wm_interm_176_triplet_pt3(b,i) * wm_interm_194_triplet_pt3(a,b,j,q)
term(428) = term(428) + t2(a,p,j,i) * wm_interm_174_triplet_pt3(b,i) * wm_interm_194_triplet_pt3(a,b,j,q)
term(429) = term(429) + t2(a,p,j,i) * wm_interm_172_triplet_pt3(b,i) * wm_interm_194_triplet_pt3(a,b,j,q)
term(430) = term(430) + t2(a,p,j,i) * wm_interm_176_triplet_pt3(b,i) * wm_interm_190_triplet_pt3(a,b,j,q)
term(431) = term(431) + t2(a,p,j,i) * wm_interm_174_triplet_pt3(b,i) * wm_interm_190_triplet_pt3(a,b,j,q)
term(432) = term(432) + t2(a,p,j,i) * wm_interm_172_triplet_pt3(b,i) * wm_interm_190_triplet_pt3(a,b,j,q)
term(433) = term(433) + t2(a,p,j,i) * wm_interm_183_triplet_pt3(b,i) * wm_interm_93_triplet_pt3(a,b,j,q)
term(434) = term(434) + t2(a,p,j,i) * wm_interm_104_triplet_pt3(a,b,j,q) * wm_interm_183_triplet_pt3(b,i)
term(435) = term(435) + t2(a,p,j,i) * wm_interm_181_triplet_pt3(b,i) * wm_interm_193_triplet_pt3(a,b,j,q)
term(436) = term(436) + t2(a,p,j,i) * wm_interm_180_triplet_pt3(b,i) * wm_interm_193_triplet_pt3(a,b,j,q)
term(437) = term(437) + t2(a,p,j,i) * wm_interm_181_triplet_pt3(b,i) * wm_interm_192_triplet_pt3(a,b,j,q)
term(438) = term(438) + t2(a,p,j,i) * wm_interm_180_triplet_pt3(b,i) * wm_interm_192_triplet_pt3(a,b,j,q)
term(439) = term(439) + t2(a,p,j,i) * wm_interm_181_triplet_pt3(b,i) * wm_interm_194_triplet_pt3(a,b,j,q)
term(440) = term(440) + t2(a,p,j,i) * wm_interm_180_triplet_pt3(b,i) * wm_interm_194_triplet_pt3(a,b,j,q)
term(441) = term(441) + t2(a,p,j,i) * wm_interm_181_triplet_pt3(b,i) * wm_interm_190_triplet_pt3(a,b,j,q)
term(442) = term(442) + t2(a,p,j,i) * wm_interm_180_triplet_pt3(b,i) * wm_interm_190_triplet_pt3(a,b,j,q)
term(443) = term(443) + t2(a,p,j,i) * wm_interm_152_triplet_pt3(a,b,j,q) * wm_interm_183_triplet_pt3(b,i)
term(444) = term(444) + t2(a,p,j,i) * wm_interm_153_triplet_pt3(a,b,j,q) * wm_interm_183_triplet_pt3(b,i)
end do 
end do 
end do 
end do 

term(375) = term(375) * (2.0d+0) 
term(376) = term(376) * (-4.0d+0) 
term(377) = term(377) * (-4.0d+0) 
term(378) = term(378) * (8.0d+0) 
term(379) = term(379) * (2.0d+0) 
term(380) = term(380) * (-4.0d+0) 
term(381) = term(381) * (2.0d+0) 
term(382) = term(382) * (-4.0d+0) 
term(383) = term(383) * (2.0d+0) 
term(384) = term(384) * (-4.0d+0) 
term(385) = term(385) * (2.0d+0) 
term(386) = term(386) * (2.0d+0) 
term(387) = term(387) * (2.0d+0) 
term(388) = term(388) * (-4.0d+0) 
term(389) = term(389) * (-4.0d+0) 
term(390) = term(390) * (8.0d+0) 
term(391) = term(391) * (2.0d+0) 
term(392) = term(392) * (-4.0d+0) 
term(393) = term(393) * (2.0d+0) 
term(394) = term(394) * (-4.0d+0) 
term(395) = term(395) * (-4.0d+0) 
term(396) = term(396) * (8.0d+0) 
term(397) = term(397) * (2.0d+0) 
term(398) = term(398) * (-4.0d+0) 
term(399) = term(399) * (2.0d+0) 
term(400) = term(400) * (2.0d+0) 
term(401) = term(401) * (-4.0d+0) 
term(402) = term(402) * (8.0d+0) 
term(403) = term(403) * (-8.0d+0) 
term(404) = term(404) * (-16.0d+0) 
term(405) = term(405) * (16.0d+0) 
term(406) = term(406) * (8.0d+0) 
term(407) = term(407) * (-8.0d+0) 
term(408) = term(408) * (8.0d+0) 
term(409) = term(409) * (-8.0d+0) 
term(410) = term(410) * (8.0d+0) 
term(411) = term(411) * (-8.0d+0) 
term(412) = term(412) * (-16.0d+0) 
term(413) = term(413) * (16.0d+0) 
term(414) = term(414) * (8.0d+0) 
term(415) = term(415) * (-8.0d+0) 
term(416) = term(416) * (-16.0d+0) 
term(417) = term(417) * (16.0d+0) 
term(418) = term(418) * (4.0d+0) 
term(419) = term(419) * (4.0d+0) 
term(420) = term(420) * (-8.0d+0) 
term(421) = term(421) * (4.0d+0) 
term(422) = term(422) * (4.0d+0) 
term(423) = term(423) * (-8.0d+0) 
term(424) = term(424) * (-2.0d+0) 
term(425) = term(425) * (-2.0d+0) 
term(426) = term(426) * (4.0d+0) 
term(427) = term(427) * (-8.0d+0) 
term(428) = term(428) * (-8.0d+0) 
term(429) = term(429) * (16.0d+0) 
term(430) = term(430) * (4.0d+0) 
term(431) = term(431) * (4.0d+0) 
term(432) = term(432) * (-8.0d+0) 
term(433) = term(433) * (-2.0d+0) 
term(434) = term(434) * (4.0d+0) 
term(435) = term(435) * (16.0d+0) 
term(436) = term(436) * (-16.0d+0) 
term(437) = term(437) * (-8.0d+0) 
term(438) = term(438) * (8.0d+0) 
term(439) = term(439) * (-32.0d+0) 
term(440) = term(440) * (32.0d+0) 
term(441) = term(441) * (16.0d+0) 
term(442) = term(442) * (-16.0d+0) 
term(443) = term(443) * (-4.0d+0) 
term(444) = term(444) * (8.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(445) = term(445) + s2(a,p,q,i) * wm_interm_162_triplet_pt3(b,j,k,i) * wm_interm_190_triplet_pt3(b,a,k,j)
term(446) = term(446) + s2(a,p,q,i) * wm_interm_162_triplet_pt3(b,j,k,i) * wm_interm_192_triplet_pt3(b,a,k,j)
term(447) = term(447) + s2(a,p,i,q) * wm_interm_162_triplet_pt3(b,j,k,i) * wm_interm_190_triplet_pt3(b,a,k,j)
term(448) = term(448) + s2(a,p,i,q) * wm_interm_162_triplet_pt3(b,j,k,i) * wm_interm_192_triplet_pt3(b,a,k,j)
term(449) = term(449) + s2(a,p,q,i) * wm_interm_161_triplet_pt3(b,j,k,i) * wm_interm_192_triplet_pt3(b,a,k,j)
term(450) = term(450) + s2(a,p,i,q) * wm_interm_161_triplet_pt3(b,j,k,i) * wm_interm_192_triplet_pt3(b,a,k,j)
term(451) = term(451) + s2(a,p,q,i) * wm_interm_162_triplet_pt3(b,j,k,i) * wm_interm_193_triplet_pt3(b,a,k,j)
term(452) = term(452) + s2(a,p,q,i) * wm_interm_162_triplet_pt3(b,j,k,i) * wm_interm_194_triplet_pt3(b,a,k,j)
term(453) = term(453) + s2(a,p,i,q) * wm_interm_162_triplet_pt3(b,j,k,i) * wm_interm_193_triplet_pt3(b,a,k,j)
term(454) = term(454) + s2(a,p,i,q) * wm_interm_162_triplet_pt3(b,j,k,i) * wm_interm_194_triplet_pt3(b,a,k,j)
term(455) = term(455) + s2(a,p,q,i) * wm_interm_164_triplet_pt3(b,j,k,i) * wm_interm_190_triplet_pt3(b,a,k,j)
term(456) = term(456) + s2(a,p,q,i) * wm_interm_164_triplet_pt3(b,j,k,i) * wm_interm_192_triplet_pt3(b,a,k,j)
term(457) = term(457) + s2(a,p,i,q) * wm_interm_164_triplet_pt3(b,j,k,i) * wm_interm_190_triplet_pt3(b,a,k,j)
term(458) = term(458) + s2(a,p,i,q) * wm_interm_164_triplet_pt3(b,j,k,i) * wm_interm_192_triplet_pt3(b,a,k,j)
term(459) = term(459) + s2(a,p,q,i) * wm_interm_164_triplet_pt3(b,j,k,i) * wm_interm_193_triplet_pt3(b,a,k,j)
term(460) = term(460) + s2(a,p,q,i) * wm_interm_164_triplet_pt3(b,j,k,i) * wm_interm_194_triplet_pt3(b,a,k,j)
term(461) = term(461) + s2(a,p,i,q) * wm_interm_164_triplet_pt3(b,j,k,i) * wm_interm_193_triplet_pt3(b,a,k,j)
term(462) = term(462) + s2(a,p,i,q) * wm_interm_164_triplet_pt3(b,j,k,i) * wm_interm_194_triplet_pt3(b,a,k,j)
end do 
end do 
end do 
end do 
end do 

term(445) = term(445) * (2.0d+0) 
term(446) = term(446) * (-4.0d+0) 
term(447) = term(447) * (-4.0d+0) 
term(448) = term(448) * (8.0d+0) 
term(449) = term(449) * (2.0d+0) 
term(450) = term(450) * (-4.0d+0) 
term(451) = term(451) * (2.0d+0) 
term(452) = term(452) * (-4.0d+0) 
term(453) = term(453) * (-4.0d+0) 
term(454) = term(454) * (8.0d+0) 
term(455) = term(455) * (8.0d+0) 
term(456) = term(456) * (-8.0d+0) 
term(457) = term(457) * (-16.0d+0) 
term(458) = term(458) * (16.0d+0) 
term(459) = term(459) * (8.0d+0) 
term(460) = term(460) * (-16.0d+0) 
term(461) = term(461) * (-16.0d+0) 
term(462) = term(462) * (32.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(463) = term(463) + s2(a,p,q,i) * wm_interm_162_triplet_pt3(b,j,i,k) * wm_interm_190_triplet_pt3(b,a,k,j)
term(464) = term(464) + s2(a,p,q,i) * wm_interm_162_triplet_pt3(b,j,i,k) * wm_interm_192_triplet_pt3(b,a,k,j)
term(465) = term(465) + s2(a,p,j,i) * wm_interm_162_triplet_pt3(b,k,i,j) * wm_interm_190_triplet_pt3(b,a,q,k)
term(466) = term(466) + s2(a,p,i,q) * wm_interm_162_triplet_pt3(b,j,i,k) * wm_interm_190_triplet_pt3(b,a,k,j)
term(467) = term(467) + s2(a,p,i,q) * wm_interm_162_triplet_pt3(b,j,i,k) * wm_interm_192_triplet_pt3(b,a,k,j)
term(468) = term(468) + s2(a,p,j,i) * wm_interm_162_triplet_pt3(b,k,i,j) * wm_interm_192_triplet_pt3(b,a,q,k)
term(469) = term(469) + s2(a,p,q,i) * wm_interm_161_triplet_pt3(b,j,i,k) * wm_interm_190_triplet_pt3(b,a,k,j)
term(470) = term(470) + s2(a,p,j,i) * wm_interm_161_triplet_pt3(b,k,i,j) * wm_interm_192_triplet_pt3(b,a,q,k)
term(471) = term(471) + s2(a,p,i,q) * wm_interm_161_triplet_pt3(b,j,i,k) * wm_interm_190_triplet_pt3(b,a,k,j)
term(472) = term(472) + s2(a,p,q,i) * wm_interm_162_triplet_pt3(b,j,i,k) * wm_interm_193_triplet_pt3(b,a,k,j)
term(473) = term(473) + s2(a,p,q,i) * wm_interm_162_triplet_pt3(b,j,i,k) * wm_interm_194_triplet_pt3(b,a,k,j)
term(474) = term(474) + s2(a,p,q,i) * wm_interm_161_triplet_pt3(b,j,i,k) * wm_interm_193_triplet_pt3(b,a,k,j)
term(475) = term(475) + s2(a,p,q,i) * wm_interm_161_triplet_pt3(b,j,i,k) * wm_interm_194_triplet_pt3(b,a,k,j)
term(476) = term(476) + s2(a,p,j,i) * wm_interm_162_triplet_pt3(b,k,i,j) * wm_interm_193_triplet_pt3(b,a,q,k)
term(477) = term(477) + s2(a,p,i,q) * wm_interm_162_triplet_pt3(b,j,i,k) * wm_interm_193_triplet_pt3(b,a,k,j)
term(478) = term(478) + s2(a,p,j,i) * wm_interm_162_triplet_pt3(b,k,i,j) * wm_interm_194_triplet_pt3(b,a,q,k)
term(479) = term(479) + s2(a,p,i,q) * wm_interm_162_triplet_pt3(b,j,i,k) * wm_interm_194_triplet_pt3(b,a,k,j)
term(480) = term(480) + s2(a,p,i,q) * wm_interm_161_triplet_pt3(b,j,i,k) * wm_interm_193_triplet_pt3(b,a,k,j)
term(481) = term(481) + s2(a,p,i,q) * wm_interm_161_triplet_pt3(b,j,i,k) * wm_interm_194_triplet_pt3(b,a,k,j)
term(482) = term(482) + s2(a,p,q,i) * wm_interm_164_triplet_pt3(b,j,i,k) * wm_interm_190_triplet_pt3(b,a,k,j)
term(483) = term(483) + s2(a,p,q,i) * wm_interm_164_triplet_pt3(b,j,i,k) * wm_interm_192_triplet_pt3(b,a,k,j)
term(484) = term(484) + s2(a,p,j,i) * wm_interm_164_triplet_pt3(b,k,i,j) * wm_interm_190_triplet_pt3(b,a,q,k)
term(485) = term(485) + s2(a,p,i,q) * wm_interm_164_triplet_pt3(b,j,i,k) * wm_interm_190_triplet_pt3(b,a,k,j)
term(486) = term(486) + s2(a,p,i,q) * wm_interm_164_triplet_pt3(b,j,i,k) * wm_interm_192_triplet_pt3(b,a,k,j)
term(487) = term(487) + s2(a,p,j,i) * wm_interm_164_triplet_pt3(b,k,i,j) * wm_interm_192_triplet_pt3(b,a,q,k)
term(488) = term(488) + s2(a,p,q,i) * wm_interm_164_triplet_pt3(b,j,i,k) * wm_interm_193_triplet_pt3(b,a,k,j)
term(489) = term(489) + s2(a,p,q,i) * wm_interm_164_triplet_pt3(b,j,i,k) * wm_interm_194_triplet_pt3(b,a,k,j)
term(490) = term(490) + s2(a,p,j,i) * wm_interm_164_triplet_pt3(b,k,i,j) * wm_interm_193_triplet_pt3(b,a,q,k)
term(491) = term(491) + s2(a,p,i,q) * wm_interm_164_triplet_pt3(b,j,i,k) * wm_interm_193_triplet_pt3(b,a,k,j)
term(492) = term(492) + s2(a,p,j,i) * wm_interm_164_triplet_pt3(b,k,i,j) * wm_interm_194_triplet_pt3(b,a,q,k)
term(493) = term(493) + s2(a,p,i,q) * wm_interm_164_triplet_pt3(b,j,i,k) * wm_interm_194_triplet_pt3(b,a,k,j)
end do 
end do 
end do 
end do 
end do 

term(463) = term(463) * (-4.0d+0) 
term(464) = term(464) * (2.0d+0) 
term(465) = term(465) * (2.0d+0) 
term(466) = term(466) * (8.0d+0) 
term(467) = term(467) * (-4.0d+0) 
term(468) = term(468) * (-4.0d+0) 
term(469) = term(469) * (2.0d+0) 
term(470) = term(470) * (2.0d+0) 
term(471) = term(471) * (-4.0d+0) 
term(472) = term(472) * (-4.0d+0) 
term(473) = term(473) * (8.0d+0) 
term(474) = term(474) * (2.0d+0) 
term(475) = term(475) * (-4.0d+0) 
term(476) = term(476) * (2.0d+0) 
term(477) = term(477) * (8.0d+0) 
term(478) = term(478) * (-4.0d+0) 
term(479) = term(479) * (-16.0d+0) 
term(480) = term(480) * (-4.0d+0) 
term(481) = term(481) * (8.0d+0) 
term(482) = term(482) * (-8.0d+0) 
term(483) = term(483) * (8.0d+0) 
term(484) = term(484) * (8.0d+0) 
term(485) = term(485) * (16.0d+0) 
term(486) = term(486) * (-16.0d+0) 
term(487) = term(487) * (-8.0d+0) 
term(488) = term(488) * (-8.0d+0) 
term(489) = term(489) * (16.0d+0) 
term(490) = term(490) * (8.0d+0) 
term(491) = term(491) * (16.0d+0) 
term(492) = term(492) * (-16.0d+0) 
term(493) = term(493) * (-32.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
term(494) = term(494) + wm_interm_12_triplet_pt3(i,j) * wm_interm_1_triplet_pt3(p,j,q,i)
term(495) = term(495) + wm_interm_13_triplet_pt3(i,j) * wm_interm_1_triplet_pt3(p,j,q,i)
term(496) = term(496) + wm_interm_12_triplet_pt3(i,j) * wm_interm_1_triplet_pt3(p,q,j,i)
term(497) = term(497) + wm_interm_13_triplet_pt3(i,j) * wm_interm_1_triplet_pt3(p,q,j,i)
term(498) = term(498) + wm_interm_15_triplet_pt3(i,j) * wm_interm_1_triplet_pt3(p,j,q,i)
term(499) = term(499) + wm_interm_15_triplet_pt3(i,j) * wm_interm_1_triplet_pt3(p,q,j,i)
term(500) = term(500) + wm_interm_115_triplet_pt3(i,j) * wm_interm_1_triplet_pt3(p,j,q,i)
term(501) = term(501) + wm_interm_116_triplet_pt3(i,j) * wm_interm_1_triplet_pt3(p,j,q,i)
term(502) = term(502) + wm_interm_115_triplet_pt3(i,j) * wm_interm_1_triplet_pt3(p,q,j,i)
term(503) = term(503) + wm_interm_116_triplet_pt3(i,j) * wm_interm_1_triplet_pt3(p,q,j,i)
term(504) = term(504) + wm_interm_118_triplet_pt3(i,j) * wm_interm_1_triplet_pt3(p,j,q,i)
term(505) = term(505) + wm_interm_118_triplet_pt3(i,j) * wm_interm_1_triplet_pt3(p,q,j,i)
term(506) = term(506) + wm_interm_124_triplet_pt3(i,j) * wm_interm_1_triplet_pt3(p,j,q,i)
term(507) = term(507) + wm_interm_125_triplet_pt3(i,j) * wm_interm_1_triplet_pt3(p,j,q,i)
term(508) = term(508) + wm_interm_126_triplet_pt3(i,j) * wm_interm_1_triplet_pt3(p,j,q,i)
term(509) = term(509) + wm_interm_124_triplet_pt3(i,j) * wm_interm_1_triplet_pt3(p,q,j,i)
term(510) = term(510) + wm_interm_125_triplet_pt3(i,j) * wm_interm_1_triplet_pt3(p,q,j,i)
term(511) = term(511) + wm_interm_126_triplet_pt3(i,j) * wm_interm_1_triplet_pt3(p,q,j,i)
term(512) = term(512) + wm_interm_136_triplet_pt3(i,j) * wm_interm_1_triplet_pt3(p,j,q,i)
term(513) = term(513) + wm_interm_137_triplet_pt3(i,j) * wm_interm_1_triplet_pt3(p,j,q,i)
term(514) = term(514) + wm_interm_136_triplet_pt3(i,j) * wm_interm_1_triplet_pt3(p,q,j,i)
term(515) = term(515) + wm_interm_137_triplet_pt3(i,j) * wm_interm_1_triplet_pt3(p,q,j,i)
term(516) = term(516) + wm_interm_140_triplet_pt3(i,j) * wm_interm_1_triplet_pt3(p,j,q,i)
term(517) = term(517) + wm_interm_141_triplet_pt3(i,j) * wm_interm_1_triplet_pt3(p,j,q,i)
term(518) = term(518) + wm_interm_140_triplet_pt3(i,j) * wm_interm_1_triplet_pt3(p,q,j,i)
term(519) = term(519) + wm_interm_141_triplet_pt3(i,j) * wm_interm_1_triplet_pt3(p,q,j,i)
term(520) = term(520) + wm_interm_184_triplet_pt3(i,j) * wm_interm_1_triplet_pt3(p,j,q,i)
term(521) = term(521) + wm_interm_184_triplet_pt3(i,j) * wm_interm_1_triplet_pt3(p,q,j,i)
end do 
end do 

term(494) = term(494) * (-0.5d+0) 
term(497) = term(497) * (-2.0d+0) 
term(498) = term(498) * (-0.5d+0) 
term(500) = term(500) * (-1.0d+0) 
term(501) = term(501) * (2.0d+0) 
term(502) = term(502) * (2.0d+0) 
term(503) = term(503) * (-4.0d+0) 
term(504) = term(504) * (-1.0d+0) 
term(505) = term(505) * (2.0d+0) 
term(506) = term(506) * (-1.0d+0) 
term(507) = term(507) * (2.0d+0) 
term(508) = term(508) * (-1.0d+0) 
term(509) = term(509) * (2.0d+0) 
term(510) = term(510) * (-4.0d+0) 
term(511) = term(511) * (2.0d+0) 
term(512) = term(512) * (-4.0d+0) 
term(513) = term(513) * (4.0d+0) 
term(514) = term(514) * (8.0d+0) 
term(515) = term(515) * (-8.0d+0) 
term(516) = term(516) * (-4.0d+0) 
term(517) = term(517) * (4.0d+0) 
term(518) = term(518) * (8.0d+0) 
term(519) = term(519) * (-8.0d+0) 
term(520) = term(520) * (2.0d+0) 
term(521) = term(521) * (-4.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(522) = term(522) + s2(a,p,j,i) * wm_interm_162_triplet_pt3(b,k,j,i) * wm_interm_190_triplet_pt3(b,a,q,k)
term(523) = term(523) + s2(a,p,j,i) * wm_interm_162_triplet_pt3(b,k,j,i) * wm_interm_192_triplet_pt3(b,a,q,k)
term(524) = term(524) + s2(a,p,j,i) * wm_interm_161_triplet_pt3(b,k,j,i) * wm_interm_190_triplet_pt3(b,a,q,k)
term(525) = term(525) + s2(a,p,j,i) * wm_interm_162_triplet_pt3(b,k,j,i) * wm_interm_193_triplet_pt3(b,a,q,k)
term(526) = term(526) + s2(a,p,j,i) * wm_interm_162_triplet_pt3(b,k,j,i) * wm_interm_194_triplet_pt3(b,a,q,k)
term(527) = term(527) + s2(a,p,j,i) * wm_interm_161_triplet_pt3(b,k,j,i) * wm_interm_193_triplet_pt3(b,a,q,k)
term(528) = term(528) + s2(a,p,j,i) * wm_interm_161_triplet_pt3(b,k,j,i) * wm_interm_194_triplet_pt3(b,a,q,k)
term(529) = term(529) + s2(a,p,j,i) * wm_interm_164_triplet_pt3(b,k,j,i) * wm_interm_190_triplet_pt3(b,a,q,k)
term(530) = term(530) + s2(a,p,j,i) * wm_interm_164_triplet_pt3(b,k,j,i) * wm_interm_192_triplet_pt3(b,a,q,k)
term(531) = term(531) + s2(a,p,j,i) * wm_interm_164_triplet_pt3(b,k,j,i) * wm_interm_193_triplet_pt3(b,a,q,k)
term(532) = term(532) + s2(a,p,j,i) * wm_interm_164_triplet_pt3(b,k,j,i) * wm_interm_194_triplet_pt3(b,a,q,k)
end do 
end do 
end do 
end do 
end do 

term(522) = term(522) * (-4.0d+0) 
term(523) = term(523) * (2.0d+0) 
term(524) = term(524) * (2.0d+0) 
term(525) = term(525) * (-4.0d+0) 
term(526) = term(526) * (8.0d+0) 
term(527) = term(527) * (2.0d+0) 
term(528) = term(528) * (-4.0d+0) 
term(529) = term(529) * (-8.0d+0) 
term(530) = term(530) * (8.0d+0) 
term(531) = term(531) * (-8.0d+0) 
term(532) = term(532) * (16.0d+0) 

do j = 1, nocc 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(533) = term(533) + r1(vrdav_Rl, a,i) * wm_interm_17_triplet_pt3(i,j) * wm_interm_190_triplet_pt3(p,a,q,j)
term(534) = term(534) + r1(vrdav_Rl, a,i) * wm_interm_18_triplet_pt3(i,j) * wm_interm_190_triplet_pt3(p,a,q,j)
term(535) = term(535) + r1(vrdav_Rl, a,i) * wm_interm_190_triplet_pt3(p,a,q,j) * wm_interm_19_triplet_pt3(i,j)
term(536) = term(536) + r1(vrdav_Rl, a,i) * wm_interm_17_triplet_pt3(i,j) * wm_interm_192_triplet_pt3(p,a,q,j)
term(537) = term(537) + r1(vrdav_Rl, a,i) * wm_interm_18_triplet_pt3(i,j) * wm_interm_192_triplet_pt3(p,a,q,j)
term(538) = term(538) + r1(vrdav_Rl, a,i) * wm_interm_192_triplet_pt3(p,a,q,j) * wm_interm_19_triplet_pt3(i,j)
term(539) = term(539) + r1(vrdav_Rl, a,i) * wm_interm_17_triplet_pt3(i,j) * wm_interm_193_triplet_pt3(p,a,q,j)
term(540) = term(540) + r1(vrdav_Rl, a,i) * wm_interm_18_triplet_pt3(i,j) * wm_interm_193_triplet_pt3(p,a,q,j)
term(541) = term(541) + r1(vrdav_Rl, a,i) * wm_interm_17_triplet_pt3(i,j) * wm_interm_194_triplet_pt3(p,a,q,j)
term(542) = term(542) + r1(vrdav_Rl, a,i) * wm_interm_18_triplet_pt3(i,j) * wm_interm_194_triplet_pt3(p,a,q,j)
term(543) = term(543) + r1(vrdav_Rl, a,i) * wm_interm_193_triplet_pt3(p,a,q,j) * wm_interm_19_triplet_pt3(i,j)
term(544) = term(544) + r1(vrdav_Rl, a,i) * wm_interm_194_triplet_pt3(p,a,q,j) * wm_interm_19_triplet_pt3(i,j)
term(545) = term(545) + r1(vrdav_Rl, a,i) * wm_interm_190_triplet_pt3(p,a,q,j) * wm_interm_54_triplet_pt3(i,j)
term(546) = term(546) + r1(vrdav_Rl, a,i) * wm_interm_190_triplet_pt3(p,a,q,j) * wm_interm_55_triplet_pt3(i,j)
term(547) = term(547) + r1(vrdav_Rl, a,i) * wm_interm_190_triplet_pt3(p,a,q,j) * wm_interm_56_triplet_pt3(i,j)
term(548) = term(548) + r1(vrdav_Rl, a,i) * wm_interm_192_triplet_pt3(p,a,q,j) * wm_interm_54_triplet_pt3(i,j)
term(549) = term(549) + r1(vrdav_Rl, a,i) * wm_interm_192_triplet_pt3(p,a,q,j) * wm_interm_55_triplet_pt3(i,j)
term(550) = term(550) + r1(vrdav_Rl, a,i) * wm_interm_192_triplet_pt3(p,a,q,j) * wm_interm_56_triplet_pt3(i,j)
term(551) = term(551) + r1(vrdav_Rl, a,i) * wm_interm_193_triplet_pt3(p,a,q,j) * wm_interm_54_triplet_pt3(i,j)
term(552) = term(552) + r1(vrdav_Rl, a,i) * wm_interm_193_triplet_pt3(p,a,q,j) * wm_interm_55_triplet_pt3(i,j)
term(553) = term(553) + r1(vrdav_Rl, a,i) * wm_interm_194_triplet_pt3(p,a,q,j) * wm_interm_54_triplet_pt3(i,j)
term(554) = term(554) + r1(vrdav_Rl, a,i) * wm_interm_194_triplet_pt3(p,a,q,j) * wm_interm_55_triplet_pt3(i,j)
term(555) = term(555) + r1(vrdav_Rl, a,i) * wm_interm_193_triplet_pt3(p,a,q,j) * wm_interm_56_triplet_pt3(i,j)
term(556) = term(556) + r1(vrdav_Rl, a,i) * wm_interm_194_triplet_pt3(p,a,q,j) * wm_interm_56_triplet_pt3(i,j)
end do 
end do 
end do 

term(533) = term(533) * (-4.0d+0) 
term(534) = term(534) * (8.0d+0) 
term(535) = term(535) * (-4.0d+0) 
term(536) = term(536) * (2.0d+0) 
term(537) = term(537) * (-4.0d+0) 
term(538) = term(538) * (2.0d+0) 
term(539) = term(539) * (-4.0d+0) 
term(540) = term(540) * (8.0d+0) 
term(541) = term(541) * (8.0d+0) 
term(542) = term(542) * (-16.0d+0) 
term(543) = term(543) * (-4.0d+0) 
term(544) = term(544) * (8.0d+0) 
term(545) = term(545) * (-8.0d+0) 
term(546) = term(546) * (16.0d+0) 
term(547) = term(547) * (-8.0d+0) 
term(548) = term(548) * (4.0d+0) 
term(549) = term(549) * (-8.0d+0) 
term(550) = term(550) * (4.0d+0) 
term(551) = term(551) * (-8.0d+0) 
term(552) = term(552) * (16.0d+0) 
term(553) = term(553) * (16.0d+0) 
term(554) = term(554) * (-32.0d+0) 
term(555) = term(555) * (-8.0d+0) 
term(556) = term(556) * (16.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
term(557) = term(557) + wm_interm_16_triplet_pt3(p,i,q,j) * wm_interm_17_triplet_pt3(i,j)
term(558) = term(558) + wm_interm_16_triplet_pt3(p,i,q,j) * wm_interm_18_triplet_pt3(i,j)
term(559) = term(559) + wm_interm_16_triplet_pt3(p,i,q,j) * wm_interm_19_triplet_pt3(i,j)
term(560) = term(560) + wm_interm_1_triplet_pt3(p,i,q,j) * wm_interm_36_triplet_pt3(j,i)
term(561) = term(561) + wm_interm_1_triplet_pt3(p,i,q,j) * wm_interm_37_triplet_pt3(j,i)
term(562) = term(562) + wm_interm_1_triplet_pt3(p,i,q,j) * wm_interm_38_triplet_pt3(j,i)
term(563) = term(563) + wm_interm_1_triplet_pt3(p,q,i,j) * wm_interm_36_triplet_pt3(j,i)
term(564) = term(564) + wm_interm_1_triplet_pt3(p,q,i,j) * wm_interm_37_triplet_pt3(j,i)
term(565) = term(565) + wm_interm_1_triplet_pt3(p,q,i,j) * wm_interm_38_triplet_pt3(j,i)
term(566) = term(566) + wm_interm_17_triplet_pt3(i,j) * wm_interm_41_triplet_pt3(p,q,i,j)
term(567) = term(567) + wm_interm_18_triplet_pt3(i,j) * wm_interm_41_triplet_pt3(p,q,i,j)
term(568) = term(568) + wm_interm_19_triplet_pt3(i,j) * wm_interm_41_triplet_pt3(p,q,i,j)
term(569) = term(569) + wm_interm_17_triplet_pt3(i,j) * wm_interm_41_triplet_pt3(p,i,q,j)
term(570) = term(570) + wm_interm_18_triplet_pt3(i,j) * wm_interm_41_triplet_pt3(p,i,q,j)
term(571) = term(571) + wm_interm_19_triplet_pt3(i,j) * wm_interm_41_triplet_pt3(p,i,q,j)
term(572) = term(572) + wm_interm_1_triplet_pt3(p,i,q,j) * wm_interm_52_triplet_pt3(j,i)
term(573) = term(573) + wm_interm_1_triplet_pt3(p,i,q,j) * wm_interm_53_triplet_pt3(j,i)
term(574) = term(574) + wm_interm_1_triplet_pt3(p,q,i,j) * wm_interm_52_triplet_pt3(j,i)
term(575) = term(575) + wm_interm_1_triplet_pt3(p,q,i,j) * wm_interm_53_triplet_pt3(j,i)
term(576) = term(576) + wm_interm_16_triplet_pt3(p,i,q,j) * wm_interm_54_triplet_pt3(i,j)
term(577) = term(577) + wm_interm_16_triplet_pt3(p,i,q,j) * wm_interm_55_triplet_pt3(i,j)
term(578) = term(578) + wm_interm_16_triplet_pt3(p,i,q,j) * wm_interm_56_triplet_pt3(i,j)
term(579) = term(579) + wm_interm_1_triplet_pt3(p,i,q,j) * wm_interm_68_triplet_pt3(j,i)
term(580) = term(580) + wm_interm_1_triplet_pt3(p,i,q,j) * wm_interm_69_triplet_pt3(j,i)
term(581) = term(581) + wm_interm_1_triplet_pt3(p,q,i,j) * wm_interm_68_triplet_pt3(j,i)
term(582) = term(582) + wm_interm_1_triplet_pt3(p,q,i,j) * wm_interm_69_triplet_pt3(j,i)
term(583) = term(583) + wm_interm_41_triplet_pt3(p,q,i,j) * wm_interm_54_triplet_pt3(i,j)
term(584) = term(584) + wm_interm_41_triplet_pt3(p,q,i,j) * wm_interm_55_triplet_pt3(i,j)
term(585) = term(585) + wm_interm_41_triplet_pt3(p,q,i,j) * wm_interm_56_triplet_pt3(i,j)
term(586) = term(586) + wm_interm_41_triplet_pt3(p,i,q,j) * wm_interm_54_triplet_pt3(i,j)
term(587) = term(587) + wm_interm_41_triplet_pt3(p,i,q,j) * wm_interm_55_triplet_pt3(i,j)
term(588) = term(588) + wm_interm_41_triplet_pt3(p,i,q,j) * wm_interm_56_triplet_pt3(i,j)
term(589) = term(589) + wm_interm_75_triplet_pt3(i,j) * wm_interm_76_triplet_pt3(p,i,q,j)
term(590) = term(590) + wm_interm_75_triplet_pt3(i,j) * wm_interm_77_triplet_pt3(p,q,i,j)
term(591) = term(591) + wm_interm_75_triplet_pt3(i,j) * wm_interm_77_triplet_pt3(p,i,q,j)
term(592) = term(592) + wm_interm_76_triplet_pt3(p,i,q,j) * wm_interm_79_triplet_pt3(i,j)
term(593) = term(593) + wm_interm_77_triplet_pt3(p,q,i,j) * wm_interm_79_triplet_pt3(i,j)
term(594) = term(594) + wm_interm_77_triplet_pt3(p,i,q,j) * wm_interm_79_triplet_pt3(i,j)
term(595) = term(595) + wm_interm_76_triplet_pt3(p,i,q,j) * wm_interm_81_triplet_pt3(i,j)
term(596) = term(596) + wm_interm_77_triplet_pt3(p,q,i,j) * wm_interm_81_triplet_pt3(i,j)
term(597) = term(597) + wm_interm_77_triplet_pt3(p,i,q,j) * wm_interm_81_triplet_pt3(i,j)
term(598) = term(598) + wm_interm_75_triplet_pt3(i,j) * wm_interm_82_triplet_pt3(p,i,q,j)
term(599) = term(599) + wm_interm_75_triplet_pt3(i,j) * wm_interm_83_triplet_pt3(p,q,i,j)
term(600) = term(600) + wm_interm_75_triplet_pt3(i,j) * wm_interm_83_triplet_pt3(p,i,q,j)
term(601) = term(601) + wm_interm_79_triplet_pt3(i,j) * wm_interm_82_triplet_pt3(p,i,q,j)
term(602) = term(602) + wm_interm_79_triplet_pt3(i,j) * wm_interm_83_triplet_pt3(p,q,i,j)
term(603) = term(603) + wm_interm_79_triplet_pt3(i,j) * wm_interm_83_triplet_pt3(p,i,q,j)
term(604) = term(604) + wm_interm_81_triplet_pt3(i,j) * wm_interm_82_triplet_pt3(p,i,q,j)
term(605) = term(605) + wm_interm_81_triplet_pt3(i,j) * wm_interm_83_triplet_pt3(p,q,i,j)
term(606) = term(606) + wm_interm_81_triplet_pt3(i,j) * wm_interm_83_triplet_pt3(p,i,q,j)
term(607) = term(607) + wm_interm_100_triplet_pt3(p,i,j,q) * wm_interm_36_triplet_pt3(j,i)
term(608) = term(608) + wm_interm_100_triplet_pt3(p,i,j,q) * wm_interm_37_triplet_pt3(j,i)
term(609) = term(609) + wm_interm_100_triplet_pt3(p,i,j,q) * wm_interm_38_triplet_pt3(j,i)
term(610) = term(610) + wm_interm_100_triplet_pt3(p,i,j,q) * wm_interm_12_triplet_pt3(j,i)
term(611) = term(611) + wm_interm_100_triplet_pt3(p,i,j,q) * wm_interm_13_triplet_pt3(j,i)
term(612) = term(612) + wm_interm_100_triplet_pt3(p,i,j,q) * wm_interm_15_triplet_pt3(j,i)
term(613) = term(613) + wm_interm_100_triplet_pt3(p,i,q,j) * wm_interm_36_triplet_pt3(j,i)
term(614) = term(614) + wm_interm_100_triplet_pt3(p,i,q,j) * wm_interm_37_triplet_pt3(j,i)
term(615) = term(615) + wm_interm_100_triplet_pt3(p,i,q,j) * wm_interm_38_triplet_pt3(j,i)
term(616) = term(616) + wm_interm_100_triplet_pt3(p,i,q,j) * wm_interm_12_triplet_pt3(j,i)
term(617) = term(617) + wm_interm_100_triplet_pt3(p,i,q,j) * wm_interm_13_triplet_pt3(j,i)
term(618) = term(618) + wm_interm_100_triplet_pt3(p,i,q,j) * wm_interm_15_triplet_pt3(j,i)
term(619) = term(619) + wm_interm_100_triplet_pt3(p,i,j,q) * wm_interm_68_triplet_pt3(j,i)
term(620) = term(620) + wm_interm_100_triplet_pt3(p,i,j,q) * wm_interm_69_triplet_pt3(j,i)
term(621) = term(621) + wm_interm_100_triplet_pt3(p,i,j,q) * wm_interm_52_triplet_pt3(j,i)
term(622) = term(622) + wm_interm_100_triplet_pt3(p,i,j,q) * wm_interm_53_triplet_pt3(j,i)
term(623) = term(623) + wm_interm_100_triplet_pt3(p,i,q,j) * wm_interm_68_triplet_pt3(j,i)
term(624) = term(624) + wm_interm_100_triplet_pt3(p,i,q,j) * wm_interm_69_triplet_pt3(j,i)
term(625) = term(625) + wm_interm_100_triplet_pt3(p,i,q,j) * wm_interm_52_triplet_pt3(j,i)
term(626) = term(626) + wm_interm_100_triplet_pt3(p,i,q,j) * wm_interm_53_triplet_pt3(j,i)
term(627) = term(627) + wm_interm_119_triplet_pt3(p,i,q,j) * wm_interm_17_triplet_pt3(i,j)
term(628) = term(628) + wm_interm_119_triplet_pt3(p,i,q,j) * wm_interm_18_triplet_pt3(i,j)
term(629) = term(629) + wm_interm_119_triplet_pt3(p,i,q,j) * wm_interm_19_triplet_pt3(i,j)
term(630) = term(630) + wm_interm_127_triplet_pt3(p,q,i,j) * wm_interm_17_triplet_pt3(i,j)
term(631) = term(631) + wm_interm_127_triplet_pt3(p,q,i,j) * wm_interm_18_triplet_pt3(i,j)
term(632) = term(632) + wm_interm_127_triplet_pt3(p,q,i,j) * wm_interm_19_triplet_pt3(i,j)
term(633) = term(633) + wm_interm_127_triplet_pt3(p,i,q,j) * wm_interm_17_triplet_pt3(i,j)
term(634) = term(634) + wm_interm_127_triplet_pt3(p,i,q,j) * wm_interm_18_triplet_pt3(i,j)
term(635) = term(635) + wm_interm_127_triplet_pt3(p,i,q,j) * wm_interm_19_triplet_pt3(i,j)
term(636) = term(636) + wm_interm_119_triplet_pt3(p,i,q,j) * wm_interm_54_triplet_pt3(i,j)
term(637) = term(637) + wm_interm_119_triplet_pt3(p,i,q,j) * wm_interm_55_triplet_pt3(i,j)
term(638) = term(638) + wm_interm_119_triplet_pt3(p,i,q,j) * wm_interm_56_triplet_pt3(i,j)
term(639) = term(639) + wm_interm_127_triplet_pt3(p,q,i,j) * wm_interm_54_triplet_pt3(i,j)
term(640) = term(640) + wm_interm_127_triplet_pt3(p,q,i,j) * wm_interm_55_triplet_pt3(i,j)
term(641) = term(641) + wm_interm_127_triplet_pt3(p,q,i,j) * wm_interm_56_triplet_pt3(i,j)
term(642) = term(642) + wm_interm_127_triplet_pt3(p,i,q,j) * wm_interm_54_triplet_pt3(i,j)
term(643) = term(643) + wm_interm_127_triplet_pt3(p,i,q,j) * wm_interm_55_triplet_pt3(i,j)
term(644) = term(644) + wm_interm_127_triplet_pt3(p,i,q,j) * wm_interm_56_triplet_pt3(i,j)
term(645) = term(645) + wm_interm_145_triplet_pt3(i,j) * wm_interm_76_triplet_pt3(p,i,q,j)
term(646) = term(646) + wm_interm_145_triplet_pt3(i,j) * wm_interm_77_triplet_pt3(p,q,i,j)
term(647) = term(647) + wm_interm_145_triplet_pt3(i,j) * wm_interm_77_triplet_pt3(p,i,q,j)
term(648) = term(648) + wm_interm_147_triplet_pt3(i,j) * wm_interm_76_triplet_pt3(p,i,q,j)
term(649) = term(649) + wm_interm_147_triplet_pt3(i,j) * wm_interm_77_triplet_pt3(p,q,i,j)
term(650) = term(650) + wm_interm_147_triplet_pt3(i,j) * wm_interm_77_triplet_pt3(p,i,q,j)
term(651) = term(651) + wm_interm_145_triplet_pt3(i,j) * wm_interm_82_triplet_pt3(p,i,q,j)
term(652) = term(652) + wm_interm_145_triplet_pt3(i,j) * wm_interm_83_triplet_pt3(p,q,i,j)
term(653) = term(653) + wm_interm_145_triplet_pt3(i,j) * wm_interm_83_triplet_pt3(p,i,q,j)
term(654) = term(654) + wm_interm_147_triplet_pt3(i,j) * wm_interm_82_triplet_pt3(p,i,q,j)
term(655) = term(655) + wm_interm_147_triplet_pt3(i,j) * wm_interm_83_triplet_pt3(p,q,i,j)
term(656) = term(656) + wm_interm_147_triplet_pt3(i,j) * wm_interm_83_triplet_pt3(p,i,q,j)
term(657) = term(657) + wm_interm_100_triplet_pt3(p,i,j,q) * wm_interm_124_triplet_pt3(j,i)
term(658) = term(658) + wm_interm_100_triplet_pt3(p,i,j,q) * wm_interm_125_triplet_pt3(j,i)
term(659) = term(659) + wm_interm_100_triplet_pt3(p,i,j,q) * wm_interm_126_triplet_pt3(j,i)
term(660) = term(660) + wm_interm_100_triplet_pt3(p,i,j,q) * wm_interm_115_triplet_pt3(j,i)
term(661) = term(661) + wm_interm_100_triplet_pt3(p,i,j,q) * wm_interm_116_triplet_pt3(j,i)
term(662) = term(662) + wm_interm_100_triplet_pt3(p,i,j,q) * wm_interm_118_triplet_pt3(j,i)
term(663) = term(663) + wm_interm_100_triplet_pt3(p,i,q,j) * wm_interm_124_triplet_pt3(j,i)
term(664) = term(664) + wm_interm_100_triplet_pt3(p,i,q,j) * wm_interm_125_triplet_pt3(j,i)
term(665) = term(665) + wm_interm_100_triplet_pt3(p,i,q,j) * wm_interm_126_triplet_pt3(j,i)
term(666) = term(666) + wm_interm_100_triplet_pt3(p,i,q,j) * wm_interm_115_triplet_pt3(j,i)
term(667) = term(667) + wm_interm_100_triplet_pt3(p,i,q,j) * wm_interm_116_triplet_pt3(j,i)
term(668) = term(668) + wm_interm_100_triplet_pt3(p,i,q,j) * wm_interm_118_triplet_pt3(j,i)
term(669) = term(669) + wm_interm_100_triplet_pt3(p,i,j,q) * wm_interm_140_triplet_pt3(j,i)
term(670) = term(670) + wm_interm_100_triplet_pt3(p,i,j,q) * wm_interm_141_triplet_pt3(j,i)
term(671) = term(671) + wm_interm_100_triplet_pt3(p,i,j,q) * wm_interm_136_triplet_pt3(j,i)
term(672) = term(672) + wm_interm_100_triplet_pt3(p,i,j,q) * wm_interm_137_triplet_pt3(j,i)
term(673) = term(673) + wm_interm_100_triplet_pt3(p,i,q,j) * wm_interm_140_triplet_pt3(j,i)
term(674) = term(674) + wm_interm_100_triplet_pt3(p,i,q,j) * wm_interm_141_triplet_pt3(j,i)
term(675) = term(675) + wm_interm_100_triplet_pt3(p,i,q,j) * wm_interm_136_triplet_pt3(j,i)
term(676) = term(676) + wm_interm_100_triplet_pt3(p,i,q,j) * wm_interm_137_triplet_pt3(j,i)
term(677) = term(677) + wm_interm_163_triplet_pt3(i,j) * wm_interm_185_triplet_pt3(p,i,q,j)
term(678) = term(678) + wm_interm_163_triplet_pt3(i,j) * wm_interm_185_triplet_pt3(p,i,j,q)
term(679) = term(679) + wm_interm_100_triplet_pt3(p,i,j,q) * wm_interm_184_triplet_pt3(j,i)
term(680) = term(680) + wm_interm_178_triplet_pt3(i,j) * wm_interm_189_triplet_pt3(p,i,j,q)
term(681) = term(681) + wm_interm_100_triplet_pt3(p,i,q,j) * wm_interm_184_triplet_pt3(j,i)
term(682) = term(682) + wm_interm_178_triplet_pt3(i,j) * wm_interm_189_triplet_pt3(p,i,q,j)
term(683) = term(683) + r1(vrdav_Rr, p,i) * wm_interm_196_triplet_pt3(i,j) * wm_interm_81_triplet_pt3(j,q)
term(684) = term(684) + r1(vrdav_Rr, p,i) * wm_interm_197_triplet_pt3(i,j) * wm_interm_81_triplet_pt3(j,q)
term(685) = term(685) + r1(vrdav_Rr, p,i) * wm_interm_196_triplet_pt3(i,j) * wm_interm_75_triplet_pt3(j,q)
term(686) = term(686) + r1(vrdav_Rr, p,i) * wm_interm_197_triplet_pt3(i,j) * wm_interm_75_triplet_pt3(j,q)
term(687) = term(687) + r1(vrdav_Rr, p,i) * wm_interm_196_triplet_pt3(i,j) * wm_interm_79_triplet_pt3(j,q)
term(688) = term(688) + r1(vrdav_Rr, p,i) * wm_interm_197_triplet_pt3(i,j) * wm_interm_79_triplet_pt3(j,q)
term(689) = term(689) + r1(vrdav_Rr, p,i) * wm_interm_196_triplet_pt3(j,q) * wm_interm_75_triplet_pt3(i,j)
term(690) = term(690) + r1(vrdav_Rr, p,i) * wm_interm_197_triplet_pt3(j,q) * wm_interm_75_triplet_pt3(i,j)
term(691) = term(691) + r1(vrdav_Rr, p,i) * wm_interm_196_triplet_pt3(j,q) * wm_interm_79_triplet_pt3(i,j)
term(692) = term(692) + r1(vrdav_Rr, p,i) * wm_interm_197_triplet_pt3(j,q) * wm_interm_79_triplet_pt3(i,j)
term(693) = term(693) + r1(vrdav_Rr, p,i) * wm_interm_196_triplet_pt3(j,q) * wm_interm_81_triplet_pt3(i,j)
term(694) = term(694) + r1(vrdav_Rr, p,i) * wm_interm_197_triplet_pt3(j,q) * wm_interm_81_triplet_pt3(i,j)
term(695) = term(695) + r1(vrdav_Rr, p,i) * wm_interm_145_triplet_pt3(j,q) * wm_interm_196_triplet_pt3(i,j)
term(696) = term(696) + r1(vrdav_Rr, p,i) * wm_interm_145_triplet_pt3(j,q) * wm_interm_197_triplet_pt3(i,j)
term(697) = term(697) + r1(vrdav_Rr, p,i) * wm_interm_147_triplet_pt3(j,q) * wm_interm_196_triplet_pt3(i,j)
term(698) = term(698) + r1(vrdav_Rr, p,i) * wm_interm_147_triplet_pt3(j,q) * wm_interm_197_triplet_pt3(i,j)
term(699) = term(699) + r1(vrdav_Rr, p,i) * wm_interm_145_triplet_pt3(i,j) * wm_interm_196_triplet_pt3(j,q)
term(700) = term(700) + r1(vrdav_Rr, p,i) * wm_interm_145_triplet_pt3(i,j) * wm_interm_197_triplet_pt3(j,q)
term(701) = term(701) + r1(vrdav_Rr, p,i) * wm_interm_147_triplet_pt3(i,j) * wm_interm_196_triplet_pt3(j,q)
term(702) = term(702) + r1(vrdav_Rr, p,i) * wm_interm_147_triplet_pt3(i,j) * wm_interm_197_triplet_pt3(j,q)
end do 
end do 

term(557) = term(557) * (-0.5d+0) 
term(559) = term(559) * (-0.5d+0) 
term(560) = term(560) * (-0.5d+0) 
term(562) = term(562) * (-0.5d+0) 
term(564) = term(564) * (-2.0d+0) 
term(566) = term(566) * (-0.5d+0) 
term(568) = term(568) * (-0.5d+0) 
term(570) = term(570) * (-2.0d+0) 
term(572) = term(572) * (-2.0d+0) 
term(573) = term(573) * (2.0d+0) 
term(574) = term(574) * (4.0d+0) 
term(575) = term(575) * (-4.0d+0) 
term(576) = term(576) * (-1.0d+0) 
term(577) = term(577) * (2.0d+0) 
term(578) = term(578) * (-1.0d+0) 
term(579) = term(579) * (-2.0d+0) 
term(580) = term(580) * (2.0d+0) 
term(581) = term(581) * (4.0d+0) 
term(582) = term(582) * (-4.0d+0) 
term(583) = term(583) * (-1.0d+0) 
term(584) = term(584) * (2.0d+0) 
term(585) = term(585) * (-1.0d+0) 
term(586) = term(586) * (2.0d+0) 
term(587) = term(587) * (-4.0d+0) 
term(588) = term(588) * (2.0d+0) 
term(589) = term(589) * (-0.5d+0) 
term(590) = term(590) * (-0.5d+0) 
term(594) = term(594) * (-2.0d+0) 
term(595) = term(595) * (-0.5d+0) 
term(596) = term(596) * (-0.5d+0) 
term(598) = term(598) * (-1.0d+0) 
term(599) = term(599) * (-1.0d+0) 
term(600) = term(600) * (2.0d+0) 
term(601) = term(601) * (2.0d+0) 
term(602) = term(602) * (2.0d+0) 
term(603) = term(603) * (-4.0d+0) 
term(604) = term(604) * (-1.0d+0) 
term(605) = term(605) * (-1.0d+0) 
term(606) = term(606) * (2.0d+0) 
term(608) = term(608) * (-2.0d+0) 
term(611) = term(611) * (-2.0d+0) 
term(613) = term(613) * (-0.5d+0) 
term(615) = term(615) * (-0.5d+0) 
term(616) = term(616) * (-0.5d+0) 
term(618) = term(618) * (-0.5d+0) 
term(619) = term(619) * (4.0d+0) 
term(620) = term(620) * (-4.0d+0) 
term(621) = term(621) * (4.0d+0) 
term(622) = term(622) * (-4.0d+0) 
term(623) = term(623) * (-2.0d+0) 
term(624) = term(624) * (2.0d+0) 
term(625) = term(625) * (-2.0d+0) 
term(626) = term(626) * (2.0d+0) 
term(627) = term(627) * (-1.0d+0) 
term(628) = term(628) * (2.0d+0) 
term(629) = term(629) * (-1.0d+0) 
term(630) = term(630) * (-1.0d+0) 
term(631) = term(631) * (2.0d+0) 
term(632) = term(632) * (-1.0d+0) 
term(633) = term(633) * (2.0d+0) 
term(634) = term(634) * (-4.0d+0) 
term(635) = term(635) * (2.0d+0) 
term(636) = term(636) * (-2.0d+0) 
term(637) = term(637) * (4.0d+0) 
term(638) = term(638) * (-2.0d+0) 
term(639) = term(639) * (-2.0d+0) 
term(640) = term(640) * (4.0d+0) 
term(641) = term(641) * (-2.0d+0) 
term(642) = term(642) * (4.0d+0) 
term(643) = term(643) * (-8.0d+0) 
term(644) = term(644) * (4.0d+0) 
term(645) = term(645) * (-2.0d+0) 
term(646) = term(646) * (-2.0d+0) 
term(647) = term(647) * (4.0d+0) 
term(648) = term(648) * (2.0d+0) 
term(649) = term(649) * (2.0d+0) 
term(650) = term(650) * (-4.0d+0) 
term(651) = term(651) * (-4.0d+0) 
term(652) = term(652) * (-4.0d+0) 
term(653) = term(653) * (8.0d+0) 
term(654) = term(654) * (4.0d+0) 
term(655) = term(655) * (4.0d+0) 
term(656) = term(656) * (-8.0d+0) 
term(657) = term(657) * (2.0d+0) 
term(658) = term(658) * (-4.0d+0) 
term(659) = term(659) * (2.0d+0) 
term(660) = term(660) * (2.0d+0) 
term(661) = term(661) * (-4.0d+0) 
term(662) = term(662) * (2.0d+0) 
term(663) = term(663) * (-1.0d+0) 
term(664) = term(664) * (2.0d+0) 
term(665) = term(665) * (-1.0d+0) 
term(666) = term(666) * (-1.0d+0) 
term(667) = term(667) * (2.0d+0) 
term(668) = term(668) * (-1.0d+0) 
term(669) = term(669) * (8.0d+0) 
term(670) = term(670) * (-8.0d+0) 
term(671) = term(671) * (8.0d+0) 
term(672) = term(672) * (-8.0d+0) 
term(673) = term(673) * (-4.0d+0) 
term(674) = term(674) * (4.0d+0) 
term(675) = term(675) * (-4.0d+0) 
term(676) = term(676) * (4.0d+0) 
term(677) = term(677) * (2.0d+0) 
term(678) = term(678) * (-4.0d+0) 
term(679) = term(679) * (-4.0d+0) 
term(680) = term(680) * (-4.0d+0) 
term(681) = term(681) * (2.0d+0) 
term(682) = term(682) * (2.0d+0) 
term(683) = term(683) * (-1.0d+0) 
term(684) = term(684) * (2.0d+0) 
term(685) = term(685) * (-1.0d+0) 
term(686) = term(686) * (2.0d+0) 
term(687) = term(687) * (2.0d+0) 
term(688) = term(688) * (-4.0d+0) 
term(689) = term(689) * (-1.0d+0) 
term(690) = term(690) * (2.0d+0) 
term(691) = term(691) * (2.0d+0) 
term(692) = term(692) * (-4.0d+0) 
term(693) = term(693) * (-1.0d+0) 
term(694) = term(694) * (2.0d+0) 
term(695) = term(695) * (-4.0d+0) 
term(696) = term(696) * (8.0d+0) 
term(697) = term(697) * (4.0d+0) 
term(698) = term(698) * (-8.0d+0) 
term(699) = term(699) * (-4.0d+0) 
term(700) = term(700) * (8.0d+0) 
term(701) = term(701) * (4.0d+0) 
term(702) = term(702) * (-8.0d+0) 

term(703) = term(703) + wm_interm_101_triplet_pt3 * wm_interm_72_triplet_pt3(p,q)
term(704) = term(704) + wm_interm_102_triplet_pt3 * wm_interm_72_triplet_pt3(p,q)
term(705) = term(705) + wm_interm_103_triplet_pt3 * wm_interm_72_triplet_pt3(p,q)
term(706) = term(706) + wm_interm_101_triplet_pt3 * wm_interm_73_triplet_pt3(p,q)
term(707) = term(707) + wm_interm_102_triplet_pt3 * wm_interm_73_triplet_pt3(p,q)
term(708) = term(708) + wm_interm_103_triplet_pt3 * wm_interm_73_triplet_pt3(p,q)
term(709) = term(709) + wm_interm_105_triplet_pt3 * wm_interm_72_triplet_pt3(p,q)
term(710) = term(710) + wm_interm_106_triplet_pt3 * wm_interm_72_triplet_pt3(p,q)
term(711) = term(711) + wm_interm_105_triplet_pt3 * wm_interm_73_triplet_pt3(p,q)
term(712) = term(712) + wm_interm_106_triplet_pt3 * wm_interm_73_triplet_pt3(p,q)
term(713) = term(713) + wm_interm_155_triplet_pt3 * wm_interm_72_triplet_pt3(p,q)
term(714) = term(714) + wm_interm_156_triplet_pt3 * wm_interm_72_triplet_pt3(p,q)
term(715) = term(715) + wm_interm_157_triplet_pt3 * wm_interm_72_triplet_pt3(p,q)
term(716) = term(716) + wm_interm_155_triplet_pt3 * wm_interm_73_triplet_pt3(p,q)
term(717) = term(717) + wm_interm_156_triplet_pt3 * wm_interm_73_triplet_pt3(p,q)
term(718) = term(718) + wm_interm_157_triplet_pt3 * wm_interm_73_triplet_pt3(p,q)
term(719) = term(719) + wm_interm_158_triplet_pt3 * wm_interm_72_triplet_pt3(p,q)
term(720) = term(720) + wm_interm_159_triplet_pt3 * wm_interm_72_triplet_pt3(p,q)
term(721) = term(721) + wm_interm_158_triplet_pt3 * wm_interm_73_triplet_pt3(p,q)
term(722) = term(722) + wm_interm_159_triplet_pt3 * wm_interm_73_triplet_pt3(p,q)
term(723) = term(723) + wm_interm_188_triplet_pt3 * wm_interm_72_triplet_pt3(p,q)
term(724) = term(724) + wm_interm_188_triplet_pt3 * wm_interm_73_triplet_pt3(p,q)

term(703) = term(703) * (-2.0d+0) 
term(704) = term(704) * (4.0d+0) 
term(705) = term(705) * (-2.0d+0) 
term(707) = term(707) * (-2.0d+0) 
term(709) = term(709) * (-8.0d+0) 
term(710) = term(710) * (8.0d+0) 
term(711) = term(711) * (4.0d+0) 
term(712) = term(712) * (-4.0d+0) 
term(713) = term(713) * (-4.0d+0) 
term(714) = term(714) * (8.0d+0) 
term(715) = term(715) * (-4.0d+0) 
term(716) = term(716) * (2.0d+0) 
term(717) = term(717) * (-4.0d+0) 
term(718) = term(718) * (2.0d+0) 
term(719) = term(719) * (-16.0d+0) 
term(720) = term(720) * (16.0d+0) 
term(721) = term(721) * (8.0d+0) 
term(722) = term(722) * (-8.0d+0) 
term(723) = term(723) * (8.0d+0) 
term(724) = term(724) * (-4.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(725) = term(725) + t2(a,p,j,i) * wm_interm_174_triplet_pt3(b,j) * wm_interm_193_triplet_pt3(a,b,i,q)
term(726) = term(726) + t2(a,p,j,i) * wm_interm_174_triplet_pt3(b,j) * wm_interm_194_triplet_pt3(a,b,i,q)
term(727) = term(727) + t2(a,p,j,i) * wm_interm_176_triplet_pt3(b,j) * wm_interm_193_triplet_pt3(a,b,i,q)
term(728) = term(728) + t2(a,p,j,i) * wm_interm_176_triplet_pt3(b,j) * wm_interm_194_triplet_pt3(a,b,i,q)
term(729) = term(729) + t2(a,p,j,i) * wm_interm_172_triplet_pt3(b,j) * wm_interm_193_triplet_pt3(a,b,i,q)
term(730) = term(730) + t2(a,p,j,i) * wm_interm_172_triplet_pt3(b,j) * wm_interm_194_triplet_pt3(a,b,i,q)
term(731) = term(731) + t2(a,p,j,i) * wm_interm_174_triplet_pt3(b,j) * wm_interm_190_triplet_pt3(a,b,i,q)
term(732) = term(732) + t2(a,p,j,i) * wm_interm_176_triplet_pt3(b,j) * wm_interm_190_triplet_pt3(a,b,i,q)
term(733) = term(733) + t2(a,p,j,i) * wm_interm_172_triplet_pt3(b,j) * wm_interm_190_triplet_pt3(a,b,i,q)
term(734) = term(734) + t2(a,p,j,i) * wm_interm_174_triplet_pt3(b,j) * wm_interm_192_triplet_pt3(a,b,i,q)
term(735) = term(735) + t2(a,p,j,i) * wm_interm_176_triplet_pt3(b,j) * wm_interm_192_triplet_pt3(a,b,i,q)
term(736) = term(736) + t2(a,p,j,i) * wm_interm_172_triplet_pt3(b,j) * wm_interm_192_triplet_pt3(a,b,i,q)
term(737) = term(737) + t2(a,p,j,i) * wm_interm_104_triplet_pt3(a,b,i,q) * wm_interm_183_triplet_pt3(b,j)
term(738) = term(738) + t2(a,p,j,i) * wm_interm_183_triplet_pt3(b,j) * wm_interm_90_triplet_pt3(a,b,i,q)
term(739) = term(739) + t2(a,p,j,i) * wm_interm_181_triplet_pt3(b,j) * wm_interm_193_triplet_pt3(a,b,i,q)
term(740) = term(740) + t2(a,p,j,i) * wm_interm_181_triplet_pt3(b,j) * wm_interm_194_triplet_pt3(a,b,i,q)
term(741) = term(741) + t2(a,p,j,i) * wm_interm_180_triplet_pt3(b,j) * wm_interm_193_triplet_pt3(a,b,i,q)
term(742) = term(742) + t2(a,p,j,i) * wm_interm_180_triplet_pt3(b,j) * wm_interm_194_triplet_pt3(a,b,i,q)
term(743) = term(743) + t2(a,p,j,i) * wm_interm_181_triplet_pt3(b,j) * wm_interm_190_triplet_pt3(a,b,i,q)
term(744) = term(744) + t2(a,p,j,i) * wm_interm_180_triplet_pt3(b,j) * wm_interm_190_triplet_pt3(a,b,i,q)
term(745) = term(745) + t2(a,p,j,i) * wm_interm_181_triplet_pt3(b,j) * wm_interm_192_triplet_pt3(a,b,i,q)
term(746) = term(746) + t2(a,p,j,i) * wm_interm_180_triplet_pt3(b,j) * wm_interm_192_triplet_pt3(a,b,i,q)
term(747) = term(747) + t2(a,p,j,i) * wm_interm_153_triplet_pt3(a,b,i,q) * wm_interm_183_triplet_pt3(b,j)
term(748) = term(748) + t2(a,p,j,i) * wm_interm_150_triplet_pt3(a,b,i,q) * wm_interm_183_triplet_pt3(b,j)
end do 
end do 
end do 
end do 

term(725) = term(725) * (-2.0d+0) 
term(726) = term(726) * (4.0d+0) 
term(727) = term(727) * (-2.0d+0) 
term(728) = term(728) * (4.0d+0) 
term(729) = term(729) * (4.0d+0) 
term(730) = term(730) * (-8.0d+0) 
term(731) = term(731) * (-2.0d+0) 
term(732) = term(732) * (-2.0d+0) 
term(733) = term(733) * (4.0d+0) 
term(734) = term(734) * (4.0d+0) 
term(735) = term(735) * (4.0d+0) 
term(736) = term(736) * (-8.0d+0) 
term(737) = term(737) * (-2.0d+0) 
term(738) = term(738) * (-2.0d+0) 
term(739) = term(739) * (-8.0d+0) 
term(740) = term(740) * (16.0d+0) 
term(741) = term(741) * (8.0d+0) 
term(742) = term(742) * (-16.0d+0) 
term(743) = term(743) * (-8.0d+0) 
term(744) = term(744) * (8.0d+0) 
term(745) = term(745) * (16.0d+0) 
term(746) = term(746) * (-16.0d+0) 
term(747) = term(747) * (-4.0d+0) 
term(748) = term(748) * (-4.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(749) = term(749) + s2(a,p,i,q) * wm_interm_189_triplet_pt3(a,j,k,l) * wm_interm_20_triplet_pt3(j,i,l,k)
term(750) = term(750) + s2(a,p,i,q) * wm_interm_189_triplet_pt3(a,j,k,l) * wm_interm_20_triplet_pt3(i,j,k,l)
term(751) = term(751) + s2(a,p,i,q) * wm_interm_189_triplet_pt3(a,j,k,l) * wm_interm_20_triplet_pt3(j,i,k,l)
term(752) = term(752) + s2(a,p,i,q) * wm_interm_189_triplet_pt3(a,j,k,l) * wm_interm_57_triplet_pt3(j,i,l,k)
term(753) = term(753) + s2(a,p,i,q) * wm_interm_189_triplet_pt3(a,j,k,l) * wm_interm_57_triplet_pt3(i,j,k,l)
term(754) = term(754) + s2(a,p,i,q) * wm_interm_189_triplet_pt3(a,j,k,l) * wm_interm_57_triplet_pt3(j,i,k,l)
term(755) = term(755) + t2(a,p,i,q) * wm_interm_175_triplet_pt3(a,j,k,l) * wm_interm_191_triplet_pt3(l,i,j,k)
term(756) = term(756) + t2(a,p,i,q) * wm_interm_171_triplet_pt3(a,j,k,l) * wm_interm_191_triplet_pt3(i,l,j,k)
term(757) = term(757) + t2(a,p,i,q) * wm_interm_171_triplet_pt3(a,j,k,l) * wm_interm_191_triplet_pt3(l,i,j,k)
term(758) = term(758) + t2(a,p,i,q) * wm_interm_185_triplet_pt3(a,j,k,l) * wm_interm_99_triplet_pt3(i,j,k,l)
term(759) = term(759) + t2(a,p,i,q) * wm_interm_185_triplet_pt3(a,j,k,l) * wm_interm_99_triplet_pt3(j,i,k,l)
term(760) = term(760) + t2(a,p,i,q) * wm_interm_185_triplet_pt3(a,j,k,l) * wm_interm_99_triplet_pt3(j,i,l,k)
term(761) = term(761) + t2(a,p,i,q) * wm_interm_182_triplet_pt3(a,j,k,l) * wm_interm_191_triplet_pt3(l,i,j,k)
term(762) = term(762) + t2(a,p,i,q) * wm_interm_179_triplet_pt3(a,j,k,l) * wm_interm_191_triplet_pt3(i,l,j,k)
term(763) = term(763) + t2(a,p,i,q) * wm_interm_179_triplet_pt3(a,j,k,l) * wm_interm_191_triplet_pt3(l,i,j,k)
term(764) = term(764) + t2(a,p,i,q) * wm_interm_154_triplet_pt3(i,j,k,l) * wm_interm_185_triplet_pt3(a,j,k,l)
term(765) = term(765) + t2(a,p,i,q) * wm_interm_154_triplet_pt3(j,i,k,l) * wm_interm_185_triplet_pt3(a,j,k,l)
term(766) = term(766) + t2(a,p,i,q) * wm_interm_154_triplet_pt3(j,i,k,l) * wm_interm_185_triplet_pt3(a,j,l,k)
end do 
end do 
end do 
end do 
end do 

term(749) = term(749) * (-4.0d+0) 
term(750) = term(750) * (-4.0d+0) 
term(751) = term(751) * (8.0d+0) 
term(752) = term(752) * (-8.0d+0) 
term(753) = term(753) * (-8.0d+0) 
term(754) = term(754) * (16.0d+0) 
term(755) = term(755) * (2.0d+0) 
term(756) = term(756) * (2.0d+0) 
term(757) = term(757) * (-4.0d+0) 
term(758) = term(758) * (2.0d+0) 
term(759) = term(759) * (-4.0d+0) 
term(760) = term(760) * (2.0d+0) 
term(761) = term(761) * (4.0d+0) 
term(762) = term(762) * (4.0d+0) 
term(763) = term(763) * (-8.0d+0) 
term(764) = term(764) * (4.0d+0) 
term(765) = term(765) * (-8.0d+0) 
term(766) = term(766) * (4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(767) = term(767) + s2(a,b,q,i) * wm_interm_186_triplet_pt3(b,j) * wm_interm_21_triplet_pt3(a,p,i,j)
term(768) = term(768) + s2(a,b,q,i) * wm_interm_186_triplet_pt3(b,j) * wm_interm_22_triplet_pt3(a,p,i,j)
term(769) = term(769) + s2(a,b,q,i) * wm_interm_186_triplet_pt3(b,j) * wm_interm_26_triplet_pt3(a,p,i,j)
term(770) = term(770) + s2(a,b,q,i) * wm_interm_186_triplet_pt3(b,j) * wm_interm_25_triplet_pt3(a,p,i,j)
term(771) = term(771) + s2(a,b,q,i) * wm_interm_186_triplet_pt3(b,j) * wm_interm_30_triplet_pt3(a,p,i,j)
term(772) = term(772) + s2(a,b,q,i) * wm_interm_186_triplet_pt3(b,j) * wm_interm_23_triplet_pt3(a,p,i,j)
term(773) = term(773) + s2(a,b,q,i) * wm_interm_186_triplet_pt3(b,j) * wm_interm_58_triplet_pt3(a,p,i,j)
term(774) = term(774) + s2(a,b,q,i) * wm_interm_186_triplet_pt3(b,j) * wm_interm_59_triplet_pt3(a,p,i,j)
term(775) = term(775) + s2(a,b,q,i) * wm_interm_186_triplet_pt3(b,j) * wm_interm_60_triplet_pt3(a,p,i,j)
term(776) = term(776) + s2(a,b,q,i) * wm_interm_186_triplet_pt3(b,j) * wm_interm_61_triplet_pt3(a,p,i,j)
term(777) = term(777) + r1(vrdav_Rr, p,i) * wm_interm_192_triplet_pt3(a,b,j,q) * wm_interm_92_triplet_pt3(b,a,i,j)
term(778) = term(778) + r1(vrdav_Rr, p,i) * wm_interm_190_triplet_pt3(a,b,j,q) * wm_interm_92_triplet_pt3(b,a,i,j)
term(779) = term(779) + r1(vrdav_Rr, p,i) * wm_interm_104_triplet_pt3(a,b,j,q) * wm_interm_193_triplet_pt3(b,a,i,j)
term(780) = term(780) + r1(vrdav_Rr, p,i) * wm_interm_104_triplet_pt3(a,b,j,q) * wm_interm_194_triplet_pt3(b,a,i,j)
term(781) = term(781) + r1(vrdav_Rr, p,i) * wm_interm_193_triplet_pt3(a,b,j,q) * wm_interm_92_triplet_pt3(b,a,i,j)
term(782) = term(782) + r1(vrdav_Rr, p,i) * wm_interm_194_triplet_pt3(a,b,j,q) * wm_interm_92_triplet_pt3(b,a,i,j)
term(783) = term(783) + r1(vrdav_Rr, p,i) * wm_interm_192_triplet_pt3(a,b,j,q) * wm_interm_91_triplet_pt3(b,a,i,j)
term(784) = term(784) + r1(vrdav_Rr, p,i) * wm_interm_190_triplet_pt3(a,b,j,q) * wm_interm_91_triplet_pt3(b,a,i,j)
term(785) = term(785) + r1(vrdav_Rr, p,i) * wm_interm_193_triplet_pt3(a,b,i,j) * wm_interm_93_triplet_pt3(b,a,j,q)
term(786) = term(786) + r1(vrdav_Rr, p,i) * wm_interm_194_triplet_pt3(a,b,i,j) * wm_interm_93_triplet_pt3(b,a,j,q)
term(787) = term(787) + r1(vrdav_Rr, p,i) * wm_interm_193_triplet_pt3(a,b,j,q) * wm_interm_91_triplet_pt3(b,a,i,j)
term(788) = term(788) + r1(vrdav_Rr, p,i) * wm_interm_194_triplet_pt3(a,b,j,q) * wm_interm_91_triplet_pt3(b,a,i,j)
term(789) = term(789) + r1(vrdav_Rr, p,i) * wm_interm_192_triplet_pt3(a,b,j,q) * wm_interm_90_triplet_pt3(b,a,i,j)
term(790) = term(790) + r1(vrdav_Rr, p,i) * wm_interm_190_triplet_pt3(a,b,j,q) * wm_interm_90_triplet_pt3(b,a,i,j)
term(791) = term(791) + r1(vrdav_Rr, p,i) * wm_interm_193_triplet_pt3(a,b,i,j) * wm_interm_94_triplet_pt3(b,a,j,q)
term(792) = term(792) + r1(vrdav_Rr, p,i) * wm_interm_194_triplet_pt3(a,b,i,j) * wm_interm_94_triplet_pt3(b,a,j,q)
term(793) = term(793) + r1(vrdav_Rr, p,i) * wm_interm_193_triplet_pt3(a,b,j,q) * wm_interm_90_triplet_pt3(b,a,i,j)
term(794) = term(794) + r1(vrdav_Rr, p,i) * wm_interm_194_triplet_pt3(a,b,j,q) * wm_interm_90_triplet_pt3(b,a,i,j)
term(795) = term(795) + r1(vrdav_Rr, p,i) * wm_interm_190_triplet_pt3(a,b,j,q) * wm_interm_93_triplet_pt3(b,a,i,j)
term(796) = term(796) + r1(vrdav_Rr, p,i) * wm_interm_190_triplet_pt3(a,b,i,j) * wm_interm_94_triplet_pt3(b,a,j,q)
term(797) = term(797) + r1(vrdav_Rr, p,i) * wm_interm_194_triplet_pt3(a,b,j,q) * wm_interm_93_triplet_pt3(b,a,i,j)
term(798) = term(798) + r1(vrdav_Rr, p,i) * wm_interm_192_triplet_pt3(a,b,i,j) * wm_interm_91_triplet_pt3(b,a,j,q)
term(799) = term(799) + r1(vrdav_Rr, p,i) * wm_interm_192_triplet_pt3(a,b,j,q) * wm_interm_93_triplet_pt3(b,a,i,j)
term(800) = term(800) + r1(vrdav_Rr, p,i) * wm_interm_193_triplet_pt3(a,b,j,q) * wm_interm_93_triplet_pt3(b,a,i,j)
term(801) = term(801) + r1(vrdav_Rr, p,i) * wm_interm_192_triplet_pt3(a,b,j,q) * wm_interm_95_triplet_pt3(b,a,i,j)
term(802) = term(802) + r1(vrdav_Rr, p,i) * wm_interm_190_triplet_pt3(a,b,j,q) * wm_interm_95_triplet_pt3(b,a,i,j)
term(803) = term(803) + r1(vrdav_Rr, p,i) * wm_interm_104_triplet_pt3(a,b,j,q) * wm_interm_192_triplet_pt3(b,a,i,j)
term(804) = term(804) + r1(vrdav_Rr, p,i) * wm_interm_104_triplet_pt3(a,b,j,q) * wm_interm_190_triplet_pt3(b,a,i,j)
term(805) = term(805) + r1(vrdav_Rr, p,i) * wm_interm_193_triplet_pt3(a,b,j,q) * wm_interm_95_triplet_pt3(b,a,i,j)
term(806) = term(806) + r1(vrdav_Rr, p,i) * wm_interm_194_triplet_pt3(a,b,j,q) * wm_interm_95_triplet_pt3(b,a,i,j)
term(807) = term(807) + r1(vrdav_Rr, p,i) * wm_interm_190_triplet_pt3(a,b,j,q) * wm_interm_94_triplet_pt3(b,a,i,j)
term(808) = term(808) + r1(vrdav_Rr, p,i) * wm_interm_192_triplet_pt3(a,b,i,j) * wm_interm_90_triplet_pt3(b,a,j,q)
term(809) = term(809) + r1(vrdav_Rr, p,i) * wm_interm_193_triplet_pt3(a,b,j,q) * wm_interm_94_triplet_pt3(b,a,i,j)
term(810) = term(810) + r1(vrdav_Rr, p,i) * wm_interm_192_triplet_pt3(a,b,i,j) * wm_interm_93_triplet_pt3(b,a,j,q)
term(811) = term(811) + r1(vrdav_Rr, p,i) * wm_interm_190_triplet_pt3(a,b,i,j) * wm_interm_93_triplet_pt3(b,a,j,q)
term(812) = term(812) + r1(vrdav_Rr, p,i) * wm_interm_194_triplet_pt3(a,b,j,q) * wm_interm_94_triplet_pt3(b,a,i,j)
term(813) = term(813) + r1(vrdav_Rr, p,i) * wm_interm_104_triplet_pt3(a,b,i,j) * wm_interm_192_triplet_pt3(b,a,j,q)
term(814) = term(814) + r1(vrdav_Rr, p,i) * wm_interm_192_triplet_pt3(a,b,i,j) * wm_interm_92_triplet_pt3(b,a,j,q)
term(815) = term(815) + r1(vrdav_Rr, p,i) * wm_interm_192_triplet_pt3(a,b,i,j) * wm_interm_95_triplet_pt3(b,a,j,q)
term(816) = term(816) + t2(a,p,i,q) * wm_interm_172_triplet_pt3(b,j) * wm_interm_193_triplet_pt3(a,b,i,j)
term(817) = term(817) + t2(a,p,i,q) * wm_interm_172_triplet_pt3(b,j) * wm_interm_194_triplet_pt3(a,b,i,j)
term(818) = term(818) + t2(a,p,i,q) * wm_interm_174_triplet_pt3(b,j) * wm_interm_193_triplet_pt3(a,b,i,j)
term(819) = term(819) + t2(a,p,i,q) * wm_interm_174_triplet_pt3(b,j) * wm_interm_194_triplet_pt3(a,b,i,j)
term(820) = term(820) + t2(a,p,i,q) * wm_interm_176_triplet_pt3(b,j) * wm_interm_193_triplet_pt3(a,b,i,j)
term(821) = term(821) + t2(a,p,i,q) * wm_interm_176_triplet_pt3(b,j) * wm_interm_194_triplet_pt3(a,b,i,j)
term(822) = term(822) + t2(a,p,i,q) * wm_interm_176_triplet_pt3(b,j) * wm_interm_190_triplet_pt3(a,b,i,j)
term(823) = term(823) + t2(a,p,i,q) * wm_interm_183_triplet_pt3(b,j) * wm_interm_93_triplet_pt3(a,b,i,j)
term(824) = term(824) + t2(a,p,i,q) * wm_interm_176_triplet_pt3(b,j) * wm_interm_192_triplet_pt3(a,b,i,j)
term(825) = term(825) + t2(a,p,i,q) * wm_interm_172_triplet_pt3(b,j) * wm_interm_192_triplet_pt3(a,b,i,j)
term(826) = term(826) + t2(a,p,i,q) * wm_interm_172_triplet_pt3(b,j) * wm_interm_190_triplet_pt3(a,b,i,j)
term(827) = term(827) + t2(a,p,i,q) * wm_interm_174_triplet_pt3(b,j) * wm_interm_192_triplet_pt3(a,b,i,j)
term(828) = term(828) + t2(a,p,i,q) * wm_interm_174_triplet_pt3(b,j) * wm_interm_190_triplet_pt3(a,b,i,j)
term(829) = term(829) + t2(a,p,i,q) * wm_interm_104_triplet_pt3(a,b,i,j) * wm_interm_183_triplet_pt3(b,j)
term(830) = term(830) + t2(a,p,q,i) * wm_interm_172_triplet_pt3(b,j) * wm_interm_193_triplet_pt3(a,b,i,j)
term(831) = term(831) + t2(a,p,q,i) * wm_interm_172_triplet_pt3(b,j) * wm_interm_194_triplet_pt3(a,b,i,j)
term(832) = term(832) + t2(a,p,q,i) * wm_interm_174_triplet_pt3(b,j) * wm_interm_193_triplet_pt3(a,b,i,j)
term(833) = term(833) + t2(a,p,q,i) * wm_interm_174_triplet_pt3(b,j) * wm_interm_194_triplet_pt3(a,b,i,j)
term(834) = term(834) + t2(a,p,q,i) * wm_interm_176_triplet_pt3(b,j) * wm_interm_193_triplet_pt3(a,b,i,j)
term(835) = term(835) + t2(a,p,q,i) * wm_interm_176_triplet_pt3(b,j) * wm_interm_194_triplet_pt3(a,b,i,j)
term(836) = term(836) + t2(a,p,q,i) * wm_interm_176_triplet_pt3(b,j) * wm_interm_190_triplet_pt3(a,b,i,j)
term(837) = term(837) + t2(a,p,q,i) * wm_interm_183_triplet_pt3(b,j) * wm_interm_93_triplet_pt3(a,b,i,j)
term(838) = term(838) + t2(a,p,q,i) * wm_interm_176_triplet_pt3(b,j) * wm_interm_192_triplet_pt3(a,b,i,j)
term(839) = term(839) + t2(a,p,q,i) * wm_interm_172_triplet_pt3(b,j) * wm_interm_192_triplet_pt3(a,b,i,j)
term(840) = term(840) + t2(a,p,q,i) * wm_interm_172_triplet_pt3(b,j) * wm_interm_190_triplet_pt3(a,b,i,j)
term(841) = term(841) + t2(a,p,q,i) * wm_interm_174_triplet_pt3(b,j) * wm_interm_192_triplet_pt3(a,b,i,j)
term(842) = term(842) + t2(a,p,q,i) * wm_interm_174_triplet_pt3(b,j) * wm_interm_190_triplet_pt3(a,b,i,j)
term(843) = term(843) + t2(a,p,q,i) * wm_interm_104_triplet_pt3(a,b,i,j) * wm_interm_183_triplet_pt3(b,j)
term(844) = term(844) + r1(vrdav_Rr, a,i) * wm_interm_104_triplet_pt3(a,b,j,q) * wm_interm_193_triplet_pt3(b,p,i,j)
term(845) = term(845) + r1(vrdav_Rr, a,i) * wm_interm_193_triplet_pt3(b,p,i,j) * wm_interm_94_triplet_pt3(a,b,j,q)
term(846) = term(846) + r1(vrdav_Rr, a,i) * wm_interm_193_triplet_pt3(b,p,i,j) * wm_interm_95_triplet_pt3(a,b,j,q)
term(847) = term(847) + r1(vrdav_Rr, a,i) * wm_interm_193_triplet_pt3(b,p,i,j) * wm_interm_90_triplet_pt3(a,b,j,q)
term(848) = term(848) + r1(vrdav_Rr, a,i) * wm_interm_193_triplet_pt3(b,p,i,j) * wm_interm_91_triplet_pt3(a,b,j,q)
term(849) = term(849) + r1(vrdav_Rr, a,i) * wm_interm_193_triplet_pt3(b,p,i,j) * wm_interm_92_triplet_pt3(a,b,j,q)
term(850) = term(850) + r1(vrdav_Rr, a,i) * wm_interm_192_triplet_pt3(b,p,i,j) * wm_interm_93_triplet_pt3(a,b,j,q)
term(851) = term(851) + r1(vrdav_Rr, a,i) * wm_interm_190_triplet_pt3(b,p,i,j) * wm_interm_95_triplet_pt3(a,b,j,q)
term(852) = term(852) + r1(vrdav_Rr, a,i) * wm_interm_192_triplet_pt3(b,p,i,j) * wm_interm_95_triplet_pt3(a,b,j,q)
term(853) = term(853) + r1(vrdav_Rr, a,i) * wm_interm_104_triplet_pt3(a,b,j,q) * wm_interm_190_triplet_pt3(b,p,i,j)
term(854) = term(854) + r1(vrdav_Rr, a,i) * wm_interm_104_triplet_pt3(a,b,j,q) * wm_interm_194_triplet_pt3(b,p,i,j)
term(855) = term(855) + r1(vrdav_Rr, a,i) * wm_interm_192_triplet_pt3(b,p,i,j) * wm_interm_94_triplet_pt3(a,b,j,q)
term(856) = term(856) + r1(vrdav_Rr, a,i) * wm_interm_190_triplet_pt3(b,p,i,j) * wm_interm_94_triplet_pt3(a,b,j,q)
term(857) = term(857) + r1(vrdav_Rr, a,i) * wm_interm_194_triplet_pt3(b,p,i,j) * wm_interm_94_triplet_pt3(a,b,j,q)
term(858) = term(858) + r1(vrdav_Rr, a,i) * wm_interm_194_triplet_pt3(b,p,i,j) * wm_interm_95_triplet_pt3(a,b,j,q)
term(859) = term(859) + r1(vrdav_Rr, a,i) * wm_interm_190_triplet_pt3(b,p,i,j) * wm_interm_90_triplet_pt3(a,b,j,q)
term(860) = term(860) + r1(vrdav_Rr, a,i) * wm_interm_192_triplet_pt3(b,p,i,j) * wm_interm_90_triplet_pt3(a,b,j,q)
term(861) = term(861) + r1(vrdav_Rr, a,i) * wm_interm_190_triplet_pt3(b,p,i,j) * wm_interm_92_triplet_pt3(a,b,j,q)
term(862) = term(862) + r1(vrdav_Rr, a,i) * wm_interm_192_triplet_pt3(b,p,i,j) * wm_interm_92_triplet_pt3(a,b,j,q)
term(863) = term(863) + r1(vrdav_Rr, a,i) * wm_interm_194_triplet_pt3(b,p,i,j) * wm_interm_90_triplet_pt3(a,b,j,q)
term(864) = term(864) + r1(vrdav_Rr, a,i) * wm_interm_190_triplet_pt3(b,p,i,j) * wm_interm_91_triplet_pt3(a,b,j,q)
term(865) = term(865) + r1(vrdav_Rr, a,i) * wm_interm_192_triplet_pt3(b,p,i,j) * wm_interm_91_triplet_pt3(a,b,j,q)
term(866) = term(866) + r1(vrdav_Rr, a,i) * wm_interm_194_triplet_pt3(b,p,i,j) * wm_interm_91_triplet_pt3(a,b,j,q)
term(867) = term(867) + r1(vrdav_Rr, a,i) * wm_interm_194_triplet_pt3(b,p,i,j) * wm_interm_92_triplet_pt3(a,b,j,q)
term(868) = term(868) + r1(vrdav_Rr, a,i) * wm_interm_193_triplet_pt3(a,b,j,q) * wm_interm_95_triplet_pt3(b,p,i,j)
term(869) = term(869) + r1(vrdav_Rr, a,i) * wm_interm_104_triplet_pt3(b,p,i,j) * wm_interm_193_triplet_pt3(a,b,j,q)
term(870) = term(870) + r1(vrdav_Rr, a,i) * wm_interm_193_triplet_pt3(a,b,j,q) * wm_interm_93_triplet_pt3(b,p,i,j)
term(871) = term(871) + r1(vrdav_Rr, a,i) * wm_interm_192_triplet_pt3(a,b,j,q) * wm_interm_93_triplet_pt3(b,p,i,j)
term(872) = term(872) + r1(vrdav_Rr, a,i) * wm_interm_192_triplet_pt3(a,b,j,q) * wm_interm_94_triplet_pt3(b,p,i,j)
term(873) = term(873) + r1(vrdav_Rr, a,i) * wm_interm_192_triplet_pt3(a,b,j,q) * wm_interm_95_triplet_pt3(b,p,i,j)
term(874) = term(874) + t2(a,b,q,i) * wm_interm_183_triplet_pt3(b,j) * wm_interm_93_triplet_pt3(a,p,i,j)
term(875) = term(875) + r1(vrdav_Rr, a,i) * wm_interm_194_triplet_pt3(a,b,j,q) * wm_interm_95_triplet_pt3(b,p,i,j)
term(876) = term(876) + t2(a,b,q,i) * wm_interm_104_triplet_pt3(a,p,i,j) * wm_interm_183_triplet_pt3(b,j)
term(877) = term(877) + r1(vrdav_Rr, a,i) * wm_interm_104_triplet_pt3(b,p,i,j) * wm_interm_194_triplet_pt3(a,b,j,q)
term(878) = term(878) + r1(vrdav_Rr, a,i) * wm_interm_194_triplet_pt3(a,b,j,q) * wm_interm_93_triplet_pt3(b,p,i,j)
term(879) = term(879) + r1(vrdav_Rr, a,i) * wm_interm_190_triplet_pt3(a,b,j,q) * wm_interm_95_triplet_pt3(b,p,i,j)
term(880) = term(880) + r1(vrdav_Rr, a,i) * wm_interm_104_triplet_pt3(b,p,i,j) * wm_interm_190_triplet_pt3(a,b,j,q)
term(881) = term(881) + r1(vrdav_Rr, a,i) * wm_interm_190_triplet_pt3(a,b,j,q) * wm_interm_93_triplet_pt3(b,p,i,j)
term(882) = term(882) + t2(a,b,q,i) * wm_interm_183_triplet_pt3(a,j) * wm_interm_93_triplet_pt3(b,p,i,j)
term(883) = term(883) + t2(a,b,q,i) * wm_interm_104_triplet_pt3(b,p,i,j) * wm_interm_183_triplet_pt3(a,j)
term(884) = term(884) + r1(vrdav_Rr, a,i) * wm_interm_193_triplet_pt3(a,b,j,q) * wm_interm_90_triplet_pt3(b,p,i,j)
term(885) = term(885) + r1(vrdav_Rr, a,i) * wm_interm_193_triplet_pt3(a,b,j,q) * wm_interm_91_triplet_pt3(b,p,i,j)
term(886) = term(886) + r1(vrdav_Rr, a,i) * wm_interm_193_triplet_pt3(a,b,j,q) * wm_interm_92_triplet_pt3(b,p,i,j)
term(887) = term(887) + t2(a,b,q,i) * wm_interm_183_triplet_pt3(b,j) * wm_interm_90_triplet_pt3(a,p,i,j)
term(888) = term(888) + r1(vrdav_Rr, a,i) * wm_interm_194_triplet_pt3(a,b,j,q) * wm_interm_90_triplet_pt3(b,p,i,j)
term(889) = term(889) + r1(vrdav_Rr, a,i) * wm_interm_194_triplet_pt3(a,b,j,q) * wm_interm_91_triplet_pt3(b,p,i,j)
term(890) = term(890) + r1(vrdav_Rr, a,i) * wm_interm_194_triplet_pt3(a,b,j,q) * wm_interm_92_triplet_pt3(b,p,i,j)
term(891) = term(891) + r1(vrdav_Rr, a,i) * wm_interm_190_triplet_pt3(a,b,j,q) * wm_interm_90_triplet_pt3(b,p,i,j)
term(892) = term(892) + r1(vrdav_Rr, a,i) * wm_interm_190_triplet_pt3(a,b,j,q) * wm_interm_91_triplet_pt3(b,p,i,j)
term(893) = term(893) + r1(vrdav_Rr, a,i) * wm_interm_190_triplet_pt3(a,b,j,q) * wm_interm_92_triplet_pt3(b,p,i,j)
term(894) = term(894) + r1(vrdav_Rr, a,i) * wm_interm_192_triplet_pt3(a,b,j,q) * wm_interm_90_triplet_pt3(b,p,i,j)
term(895) = term(895) + r1(vrdav_Rr, a,i) * wm_interm_192_triplet_pt3(a,b,j,q) * wm_interm_91_triplet_pt3(b,p,i,j)
term(896) = term(896) + r1(vrdav_Rr, a,i) * wm_interm_192_triplet_pt3(a,b,j,q) * wm_interm_92_triplet_pt3(b,p,i,j)
term(897) = term(897) + r1(vrdav_Rr, p,i) * wm_interm_151_triplet_pt3(a,b,i,j) * wm_interm_192_triplet_pt3(b,a,j,q)
term(898) = term(898) + r1(vrdav_Rr, p,i) * wm_interm_151_triplet_pt3(a,b,i,j) * wm_interm_190_triplet_pt3(b,a,j,q)
term(899) = term(899) + r1(vrdav_Rr, p,i) * wm_interm_153_triplet_pt3(a,b,j,q) * wm_interm_193_triplet_pt3(b,a,i,j)
term(900) = term(900) + r1(vrdav_Rr, p,i) * wm_interm_153_triplet_pt3(a,b,j,q) * wm_interm_194_triplet_pt3(b,a,i,j)
term(901) = term(901) + r1(vrdav_Rr, p,i) * wm_interm_151_triplet_pt3(a,b,i,j) * wm_interm_193_triplet_pt3(b,a,j,q)
term(902) = term(902) + r1(vrdav_Rr, p,i) * wm_interm_151_triplet_pt3(a,b,i,j) * wm_interm_194_triplet_pt3(b,a,j,q)
term(903) = term(903) + r1(vrdav_Rr, p,i) * wm_interm_150_triplet_pt3(a,b,i,j) * wm_interm_192_triplet_pt3(b,a,j,q)
term(904) = term(904) + r1(vrdav_Rr, p,i) * wm_interm_150_triplet_pt3(a,b,i,j) * wm_interm_190_triplet_pt3(b,a,j,q)
term(905) = term(905) + r1(vrdav_Rr, p,i) * wm_interm_152_triplet_pt3(a,b,j,q) * wm_interm_193_triplet_pt3(b,a,i,j)
term(906) = term(906) + r1(vrdav_Rr, p,i) * wm_interm_152_triplet_pt3(a,b,j,q) * wm_interm_194_triplet_pt3(b,a,i,j)
term(907) = term(907) + r1(vrdav_Rr, p,i) * wm_interm_150_triplet_pt3(a,b,i,j) * wm_interm_193_triplet_pt3(b,a,j,q)
term(908) = term(908) + r1(vrdav_Rr, p,i) * wm_interm_150_triplet_pt3(a,b,i,j) * wm_interm_194_triplet_pt3(b,a,j,q)
term(909) = term(909) + r1(vrdav_Rr, p,i) * wm_interm_152_triplet_pt3(a,b,i,j) * wm_interm_190_triplet_pt3(b,a,j,q)
term(910) = term(910) + r1(vrdav_Rr, p,i) * wm_interm_152_triplet_pt3(a,b,j,q) * wm_interm_190_triplet_pt3(b,a,i,j)
term(911) = term(911) + r1(vrdav_Rr, p,i) * wm_interm_152_triplet_pt3(a,b,i,j) * wm_interm_194_triplet_pt3(b,a,j,q)
term(912) = term(912) + r1(vrdav_Rr, p,i) * wm_interm_150_triplet_pt3(a,b,j,q) * wm_interm_192_triplet_pt3(b,a,i,j)
term(913) = term(913) + r1(vrdav_Rr, p,i) * wm_interm_152_triplet_pt3(a,b,i,j) * wm_interm_192_triplet_pt3(b,a,j,q)
term(914) = term(914) + r1(vrdav_Rr, p,i) * wm_interm_152_triplet_pt3(a,b,i,j) * wm_interm_193_triplet_pt3(b,a,j,q)
term(915) = term(915) + r1(vrdav_Rr, p,i) * wm_interm_153_triplet_pt3(a,b,i,j) * wm_interm_192_triplet_pt3(b,a,j,q)
term(916) = term(916) + r1(vrdav_Rr, p,i) * wm_interm_153_triplet_pt3(a,b,i,j) * wm_interm_190_triplet_pt3(b,a,j,q)
term(917) = term(917) + r1(vrdav_Rr, p,i) * wm_interm_153_triplet_pt3(a,b,j,q) * wm_interm_192_triplet_pt3(b,a,i,j)
term(918) = term(918) + r1(vrdav_Rr, p,i) * wm_interm_153_triplet_pt3(a,b,j,q) * wm_interm_190_triplet_pt3(b,a,i,j)
term(919) = term(919) + r1(vrdav_Rr, p,i) * wm_interm_153_triplet_pt3(a,b,i,j) * wm_interm_193_triplet_pt3(b,a,j,q)
term(920) = term(920) + r1(vrdav_Rr, p,i) * wm_interm_153_triplet_pt3(a,b,i,j) * wm_interm_194_triplet_pt3(b,a,j,q)
term(921) = term(921) + r1(vrdav_Rr, p,i) * wm_interm_152_triplet_pt3(a,b,j,q) * wm_interm_192_triplet_pt3(b,a,i,j)
term(922) = term(922) + r1(vrdav_Rr, p,i) * wm_interm_151_triplet_pt3(a,b,j,q) * wm_interm_192_triplet_pt3(b,a,i,j)
term(923) = term(923) + t2(a,p,i,q) * wm_interm_180_triplet_pt3(b,j) * wm_interm_193_triplet_pt3(a,b,i,j)
term(924) = term(924) + t2(a,p,i,q) * wm_interm_180_triplet_pt3(b,j) * wm_interm_194_triplet_pt3(a,b,i,j)
term(925) = term(925) + t2(a,p,i,q) * wm_interm_181_triplet_pt3(b,j) * wm_interm_193_triplet_pt3(a,b,i,j)
term(926) = term(926) + t2(a,p,i,q) * wm_interm_181_triplet_pt3(b,j) * wm_interm_194_triplet_pt3(a,b,i,j)
term(927) = term(927) + t2(a,p,i,q) * wm_interm_181_triplet_pt3(b,j) * wm_interm_190_triplet_pt3(a,b,i,j)
term(928) = term(928) + t2(a,p,i,q) * wm_interm_152_triplet_pt3(a,b,i,j) * wm_interm_183_triplet_pt3(b,j)
term(929) = term(929) + t2(a,p,i,q) * wm_interm_181_triplet_pt3(b,j) * wm_interm_192_triplet_pt3(a,b,i,j)
term(930) = term(930) + t2(a,p,i,q) * wm_interm_180_triplet_pt3(b,j) * wm_interm_192_triplet_pt3(a,b,i,j)
term(931) = term(931) + t2(a,p,i,q) * wm_interm_180_triplet_pt3(b,j) * wm_interm_190_triplet_pt3(a,b,i,j)
term(932) = term(932) + t2(a,p,i,q) * wm_interm_153_triplet_pt3(a,b,i,j) * wm_interm_183_triplet_pt3(b,j)
term(933) = term(933) + t2(a,p,q,i) * wm_interm_180_triplet_pt3(b,j) * wm_interm_193_triplet_pt3(a,b,i,j)
term(934) = term(934) + t2(a,p,q,i) * wm_interm_180_triplet_pt3(b,j) * wm_interm_194_triplet_pt3(a,b,i,j)
term(935) = term(935) + t2(a,p,q,i) * wm_interm_181_triplet_pt3(b,j) * wm_interm_193_triplet_pt3(a,b,i,j)
term(936) = term(936) + t2(a,p,q,i) * wm_interm_181_triplet_pt3(b,j) * wm_interm_194_triplet_pt3(a,b,i,j)
term(937) = term(937) + t2(a,p,q,i) * wm_interm_181_triplet_pt3(b,j) * wm_interm_190_triplet_pt3(a,b,i,j)
term(938) = term(938) + t2(a,p,q,i) * wm_interm_152_triplet_pt3(a,b,i,j) * wm_interm_183_triplet_pt3(b,j)
term(939) = term(939) + t2(a,p,q,i) * wm_interm_181_triplet_pt3(b,j) * wm_interm_192_triplet_pt3(a,b,i,j)
term(940) = term(940) + t2(a,p,q,i) * wm_interm_180_triplet_pt3(b,j) * wm_interm_192_triplet_pt3(a,b,i,j)
term(941) = term(941) + t2(a,p,q,i) * wm_interm_180_triplet_pt3(b,j) * wm_interm_190_triplet_pt3(a,b,i,j)
term(942) = term(942) + t2(a,p,q,i) * wm_interm_153_triplet_pt3(a,b,i,j) * wm_interm_183_triplet_pt3(b,j)
term(943) = term(943) + r1(vrdav_Rr, a,i) * wm_interm_153_triplet_pt3(a,b,j,q) * wm_interm_193_triplet_pt3(b,p,i,j)
term(944) = term(944) + r1(vrdav_Rr, a,i) * wm_interm_152_triplet_pt3(a,b,j,q) * wm_interm_193_triplet_pt3(b,p,i,j)
term(945) = term(945) + r1(vrdav_Rr, a,i) * wm_interm_150_triplet_pt3(a,b,j,q) * wm_interm_193_triplet_pt3(b,p,i,j)
term(946) = term(946) + r1(vrdav_Rr, a,i) * wm_interm_151_triplet_pt3(a,b,j,q) * wm_interm_193_triplet_pt3(b,p,i,j)
term(947) = term(947) + r1(vrdav_Rr, a,i) * wm_interm_152_triplet_pt3(a,b,j,q) * wm_interm_192_triplet_pt3(b,p,i,j)
term(948) = term(948) + r1(vrdav_Rr, a,i) * wm_interm_153_triplet_pt3(a,b,j,q) * wm_interm_190_triplet_pt3(b,p,i,j)
term(949) = term(949) + r1(vrdav_Rr, a,i) * wm_interm_153_triplet_pt3(a,b,j,q) * wm_interm_192_triplet_pt3(b,p,i,j)
term(950) = term(950) + r1(vrdav_Rr, a,i) * wm_interm_153_triplet_pt3(a,b,j,q) * wm_interm_194_triplet_pt3(b,p,i,j)
term(951) = term(951) + r1(vrdav_Rr, a,i) * wm_interm_152_triplet_pt3(a,b,j,q) * wm_interm_190_triplet_pt3(b,p,i,j)
term(952) = term(952) + r1(vrdav_Rr, a,i) * wm_interm_152_triplet_pt3(a,b,j,q) * wm_interm_194_triplet_pt3(b,p,i,j)
term(953) = term(953) + r1(vrdav_Rr, a,i) * wm_interm_150_triplet_pt3(a,b,j,q) * wm_interm_190_triplet_pt3(b,p,i,j)
term(954) = term(954) + r1(vrdav_Rr, a,i) * wm_interm_150_triplet_pt3(a,b,j,q) * wm_interm_192_triplet_pt3(b,p,i,j)
term(955) = term(955) + r1(vrdav_Rr, a,i) * wm_interm_151_triplet_pt3(a,b,j,q) * wm_interm_190_triplet_pt3(b,p,i,j)
term(956) = term(956) + r1(vrdav_Rr, a,i) * wm_interm_151_triplet_pt3(a,b,j,q) * wm_interm_192_triplet_pt3(b,p,i,j)
term(957) = term(957) + r1(vrdav_Rr, a,i) * wm_interm_150_triplet_pt3(a,b,j,q) * wm_interm_194_triplet_pt3(b,p,i,j)
term(958) = term(958) + r1(vrdav_Rr, a,i) * wm_interm_151_triplet_pt3(a,b,j,q) * wm_interm_194_triplet_pt3(b,p,i,j)
term(959) = term(959) + r1(vrdav_Rr, a,i) * wm_interm_153_triplet_pt3(b,p,i,j) * wm_interm_193_triplet_pt3(a,b,j,q)
term(960) = term(960) + r1(vrdav_Rr, a,i) * wm_interm_152_triplet_pt3(b,p,i,j) * wm_interm_193_triplet_pt3(a,b,j,q)
term(961) = term(961) + r1(vrdav_Rr, a,i) * wm_interm_152_triplet_pt3(b,p,i,j) * wm_interm_192_triplet_pt3(a,b,j,q)
term(962) = term(962) + r1(vrdav_Rr, a,i) * wm_interm_153_triplet_pt3(b,p,i,j) * wm_interm_192_triplet_pt3(a,b,j,q)
term(963) = term(963) + t2(a,b,q,i) * wm_interm_152_triplet_pt3(a,p,i,j) * wm_interm_183_triplet_pt3(b,j)
term(964) = term(964) + r1(vrdav_Rr, a,i) * wm_interm_153_triplet_pt3(b,p,i,j) * wm_interm_194_triplet_pt3(a,b,j,q)
term(965) = term(965) + t2(a,b,q,i) * wm_interm_153_triplet_pt3(a,p,i,j) * wm_interm_183_triplet_pt3(b,j)
term(966) = term(966) + r1(vrdav_Rr, a,i) * wm_interm_152_triplet_pt3(b,p,i,j) * wm_interm_194_triplet_pt3(a,b,j,q)
term(967) = term(967) + r1(vrdav_Rr, a,i) * wm_interm_153_triplet_pt3(b,p,i,j) * wm_interm_190_triplet_pt3(a,b,j,q)
term(968) = term(968) + r1(vrdav_Rr, a,i) * wm_interm_152_triplet_pt3(b,p,i,j) * wm_interm_190_triplet_pt3(a,b,j,q)
term(969) = term(969) + t2(a,b,q,i) * wm_interm_152_triplet_pt3(b,p,i,j) * wm_interm_183_triplet_pt3(a,j)
term(970) = term(970) + t2(a,b,q,i) * wm_interm_153_triplet_pt3(b,p,i,j) * wm_interm_183_triplet_pt3(a,j)
term(971) = term(971) + r1(vrdav_Rr, a,i) * wm_interm_150_triplet_pt3(b,p,i,j) * wm_interm_193_triplet_pt3(a,b,j,q)
term(972) = term(972) + r1(vrdav_Rr, a,i) * wm_interm_151_triplet_pt3(b,p,i,j) * wm_interm_193_triplet_pt3(a,b,j,q)
term(973) = term(973) + t2(a,b,q,i) * wm_interm_150_triplet_pt3(a,p,i,j) * wm_interm_183_triplet_pt3(b,j)
term(974) = term(974) + r1(vrdav_Rr, a,i) * wm_interm_150_triplet_pt3(b,p,i,j) * wm_interm_194_triplet_pt3(a,b,j,q)
term(975) = term(975) + r1(vrdav_Rr, a,i) * wm_interm_151_triplet_pt3(b,p,i,j) * wm_interm_194_triplet_pt3(a,b,j,q)
term(976) = term(976) + r1(vrdav_Rr, a,i) * wm_interm_150_triplet_pt3(b,p,i,j) * wm_interm_190_triplet_pt3(a,b,j,q)
term(977) = term(977) + r1(vrdav_Rr, a,i) * wm_interm_151_triplet_pt3(b,p,i,j) * wm_interm_190_triplet_pt3(a,b,j,q)
term(978) = term(978) + r1(vrdav_Rr, a,i) * wm_interm_150_triplet_pt3(b,p,i,j) * wm_interm_192_triplet_pt3(a,b,j,q)
term(979) = term(979) + r1(vrdav_Rr, a,i) * wm_interm_151_triplet_pt3(b,p,i,j) * wm_interm_192_triplet_pt3(a,b,j,q)
end do 
end do 
end do 
end do 

term(767) = term(767) * (2.0d+0) 
term(768) = term(768) * (-4.0d+0) 
term(769) = term(769) * (2.0d+0) 
term(770) = term(770) * (2.0d+0) 
term(771) = term(771) * (-4.0d+0) 
term(772) = term(772) * (2.0d+0) 
term(773) = term(773) * (8.0d+0) 
term(774) = term(774) * (-8.0d+0) 
term(775) = term(775) * (8.0d+0) 
term(776) = term(776) * (-8.0d+0) 
term(777) = term(777) * (2.0d+0) 
term(778) = term(778) * (-4.0d+0) 
term(779) = term(779) * (2.0d+0) 
term(780) = term(780) * (-4.0d+0) 
term(781) = term(781) * (-4.0d+0) 
term(782) = term(782) * (8.0d+0) 
term(783) = term(783) * (-1.0d+0) 
term(784) = term(784) * (2.0d+0) 
term(785) = term(785) * (-1.0d+0) 
term(786) = term(786) * (2.0d+0) 
term(787) = term(787) * (2.0d+0) 
term(788) = term(788) * (-4.0d+0) 
term(789) = term(789) * (-1.0d+0) 
term(790) = term(790) * (2.0d+0) 
term(791) = term(791) * (-1.0d+0) 
term(792) = term(792) * (2.0d+0) 
term(793) = term(793) * (2.0d+0) 
term(794) = term(794) * (-4.0d+0) 
term(795) = term(795) * (-1.0d+0) 
term(796) = term(796) * (-1.0d+0) 
term(797) = term(797) * (2.0d+0) 
term(798) = term(798) * (-1.0d+0) 
term(799) = term(799) * (2.0d+0) 
term(800) = term(800) * (-1.0d+0) 
term(801) = term(801) * (-1.0d+0) 
term(802) = term(802) * (2.0d+0) 
term(803) = term(803) * (-1.0d+0) 
term(804) = term(804) * (2.0d+0) 
term(805) = term(805) * (2.0d+0) 
term(806) = term(806) * (-4.0d+0) 
term(807) = term(807) * (-1.0d+0) 
term(808) = term(808) * (-1.0d+0) 
term(809) = term(809) * (-1.0d+0) 
term(810) = term(810) * (2.0d+0) 
term(811) = term(811) * (-1.0d+0) 
term(812) = term(812) * (2.0d+0) 
term(813) = term(813) * (-1.0d+0) 
term(814) = term(814) * (2.0d+0) 
term(815) = term(815) * (-1.0d+0) 
term(816) = term(816) * (8.0d+0) 
term(817) = term(817) * (-16.0d+0) 
term(818) = term(818) * (-4.0d+0) 
term(819) = term(819) * (8.0d+0) 
term(820) = term(820) * (-4.0d+0) 
term(821) = term(821) * (8.0d+0) 
term(822) = term(822) * (-4.0d+0) 
term(823) = term(823) * (2.0d+0) 
term(824) = term(824) * (2.0d+0) 
term(825) = term(825) * (-4.0d+0) 
term(826) = term(826) * (8.0d+0) 
term(827) = term(827) * (2.0d+0) 
term(828) = term(828) * (-4.0d+0) 
term(829) = term(829) * (-4.0d+0) 
term(830) = term(830) * (-4.0d+0) 
term(831) = term(831) * (8.0d+0) 
term(832) = term(832) * (2.0d+0) 
term(833) = term(833) * (-4.0d+0) 
term(834) = term(834) * (2.0d+0) 
term(835) = term(835) * (-4.0d+0) 
term(836) = term(836) * (2.0d+0) 
term(837) = term(837) * (-1.0d+0) 
term(838) = term(838) * (-1.0d+0) 
term(839) = term(839) * (2.0d+0) 
term(840) = term(840) * (-4.0d+0) 
term(841) = term(841) * (-1.0d+0) 
term(842) = term(842) * (2.0d+0) 
term(843) = term(843) * (2.0d+0) 
term(844) = term(844) * (-2.0d+0) 
term(845) = term(845) * (4.0d+0) 
term(846) = term(846) * (-2.0d+0) 
term(847) = term(847) * (-2.0d+0) 
term(848) = term(848) * (-2.0d+0) 
term(849) = term(849) * (4.0d+0) 
term(850) = term(850) * (-2.0d+0) 
term(851) = term(851) * (-2.0d+0) 
term(852) = term(852) * (4.0d+0) 
term(853) = term(853) * (-2.0d+0) 
term(854) = term(854) * (4.0d+0) 
term(855) = term(855) * (-2.0d+0) 
term(856) = term(856) * (4.0d+0) 
term(857) = term(857) * (-8.0d+0) 
term(858) = term(858) * (4.0d+0) 
term(859) = term(859) * (-2.0d+0) 
term(860) = term(860) * (4.0d+0) 
term(861) = term(861) * (4.0d+0) 
term(862) = term(862) * (-8.0d+0) 
term(863) = term(863) * (4.0d+0) 
term(864) = term(864) * (-2.0d+0) 
term(865) = term(865) * (4.0d+0) 
term(866) = term(866) * (4.0d+0) 
term(867) = term(867) * (-8.0d+0) 
term(868) = term(868) * (-2.0d+0) 
term(869) = term(869) * (-2.0d+0) 
term(870) = term(870) * (4.0d+0) 
term(871) = term(871) * (-2.0d+0) 
term(872) = term(872) * (-2.0d+0) 
term(873) = term(873) * (4.0d+0) 
term(874) = term(874) * (4.0d+0) 
term(875) = term(875) * (4.0d+0) 
term(876) = term(876) * (-2.0d+0) 
term(877) = term(877) * (4.0d+0) 
term(878) = term(878) * (-8.0d+0) 
term(879) = term(879) * (-2.0d+0) 
term(880) = term(880) * (-2.0d+0) 
term(881) = term(881) * (4.0d+0) 
term(882) = term(882) * (-2.0d+0) 
term(883) = term(883) * (4.0d+0) 
term(884) = term(884) * (-2.0d+0) 
term(885) = term(885) * (-2.0d+0) 
term(886) = term(886) * (4.0d+0) 
term(887) = term(887) * (-2.0d+0) 
term(888) = term(888) * (4.0d+0) 
term(889) = term(889) * (4.0d+0) 
term(890) = term(890) * (-8.0d+0) 
term(891) = term(891) * (-2.0d+0) 
term(892) = term(892) * (-2.0d+0) 
term(893) = term(893) * (4.0d+0) 
term(894) = term(894) * (4.0d+0) 
term(895) = term(895) * (4.0d+0) 
term(896) = term(896) * (-8.0d+0) 
term(897) = term(897) * (4.0d+0) 
term(898) = term(898) * (-8.0d+0) 
term(899) = term(899) * (4.0d+0) 
term(900) = term(900) * (-8.0d+0) 
term(901) = term(901) * (-8.0d+0) 
term(902) = term(902) * (16.0d+0) 
term(903) = term(903) * (-4.0d+0) 
term(904) = term(904) * (8.0d+0) 
term(905) = term(905) * (-4.0d+0) 
term(906) = term(906) * (8.0d+0) 
term(907) = term(907) * (8.0d+0) 
term(908) = term(908) * (-16.0d+0) 
term(909) = term(909) * (-4.0d+0) 
term(910) = term(910) * (-4.0d+0) 
term(911) = term(911) * (8.0d+0) 
term(912) = term(912) * (-4.0d+0) 
term(913) = term(913) * (4.0d+0) 
term(914) = term(914) * (-4.0d+0) 
term(915) = term(915) * (-4.0d+0) 
term(916) = term(916) * (4.0d+0) 
term(917) = term(917) * (-4.0d+0) 
term(918) = term(918) * (4.0d+0) 
term(919) = term(919) * (4.0d+0) 
term(920) = term(920) * (-8.0d+0) 
term(921) = term(921) * (4.0d+0) 
term(922) = term(922) * (4.0d+0) 
term(923) = term(923) * (16.0d+0) 
term(924) = term(924) * (-32.0d+0) 
term(925) = term(925) * (-16.0d+0) 
term(926) = term(926) * (32.0d+0) 
term(927) = term(927) * (-16.0d+0) 
term(928) = term(928) * (4.0d+0) 
term(929) = term(929) * (8.0d+0) 
term(930) = term(930) * (-8.0d+0) 
term(931) = term(931) * (16.0d+0) 
term(932) = term(932) * (-8.0d+0) 
term(933) = term(933) * (-8.0d+0) 
term(934) = term(934) * (16.0d+0) 
term(935) = term(935) * (8.0d+0) 
term(936) = term(936) * (-16.0d+0) 
term(937) = term(937) * (8.0d+0) 
term(938) = term(938) * (-2.0d+0) 
term(939) = term(939) * (-4.0d+0) 
term(940) = term(940) * (4.0d+0) 
term(941) = term(941) * (-8.0d+0) 
term(942) = term(942) * (4.0d+0) 
term(943) = term(943) * (-8.0d+0) 
term(944) = term(944) * (8.0d+0) 
term(945) = term(945) * (-8.0d+0) 
term(946) = term(946) * (8.0d+0) 
term(947) = term(947) * (-8.0d+0) 
term(948) = term(948) * (-8.0d+0) 
term(949) = term(949) * (8.0d+0) 
term(950) = term(950) * (16.0d+0) 
term(951) = term(951) * (8.0d+0) 
term(952) = term(952) * (-16.0d+0) 
term(953) = term(953) * (-8.0d+0) 
term(954) = term(954) * (16.0d+0) 
term(955) = term(955) * (8.0d+0) 
term(956) = term(956) * (-16.0d+0) 
term(957) = term(957) * (16.0d+0) 
term(958) = term(958) * (-16.0d+0) 
term(959) = term(959) * (-8.0d+0) 
term(960) = term(960) * (8.0d+0) 
term(961) = term(961) * (-8.0d+0) 
term(962) = term(962) * (8.0d+0) 
term(963) = term(963) * (8.0d+0) 
term(964) = term(964) * (16.0d+0) 
term(965) = term(965) * (-4.0d+0) 
term(966) = term(966) * (-16.0d+0) 
term(967) = term(967) * (-8.0d+0) 
term(968) = term(968) * (8.0d+0) 
term(969) = term(969) * (-4.0d+0) 
term(970) = term(970) * (8.0d+0) 
term(971) = term(971) * (-8.0d+0) 
term(972) = term(972) * (8.0d+0) 
term(973) = term(973) * (-4.0d+0) 
term(974) = term(974) * (16.0d+0) 
term(975) = term(975) * (-16.0d+0) 
term(976) = term(976) * (-8.0d+0) 
term(977) = term(977) * (8.0d+0) 
term(978) = term(978) * (16.0d+0) 
term(979) = term(979) * (-16.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(980) = term(980) + s2(a,p,i,q) * wm_interm_189_triplet_pt3(b,j,i,k) * wm_interm_24_triplet_pt3(a,b,j,k)
term(981) = term(981) + s2(a,p,i,q) * wm_interm_189_triplet_pt3(b,j,i,k) * wm_interm_23_triplet_pt3(a,b,j,k)
term(982) = term(982) + s2(a,p,i,q) * wm_interm_189_triplet_pt3(b,j,i,k) * wm_interm_21_triplet_pt3(a,b,j,k)
term(983) = term(983) + s2(a,p,i,q) * wm_interm_189_triplet_pt3(b,j,i,k) * wm_interm_22_triplet_pt3(a,b,j,k)
term(984) = term(984) + s2(a,p,i,q) * wm_interm_189_triplet_pt3(b,j,i,k) * wm_interm_30_triplet_pt3(a,b,j,k)
term(985) = term(985) + s2(a,p,i,q) * wm_interm_189_triplet_pt3(b,j,i,k) * wm_interm_26_triplet_pt3(a,b,j,k)
term(986) = term(986) + s2(a,p,i,q) * wm_interm_189_triplet_pt3(b,j,i,k) * wm_interm_61_triplet_pt3(a,b,j,k)
term(987) = term(987) + s2(a,p,i,q) * wm_interm_189_triplet_pt3(b,j,i,k) * wm_interm_60_triplet_pt3(a,b,j,k)
term(988) = term(988) + s2(a,p,i,q) * wm_interm_189_triplet_pt3(b,j,i,k) * wm_interm_58_triplet_pt3(a,b,j,k)
term(989) = term(989) + s2(a,p,i,q) * wm_interm_189_triplet_pt3(b,j,i,k) * wm_interm_59_triplet_pt3(a,b,j,k)
term(990) = term(990) + t2(a,p,i,q) * wm_interm_185_triplet_pt3(b,j,i,k) * wm_interm_92_triplet_pt3(a,b,j,k)
term(991) = term(991) + t2(a,p,i,q) * wm_interm_185_triplet_pt3(b,j,i,k) * wm_interm_91_triplet_pt3(a,b,j,k)
term(992) = term(992) + t2(a,p,i,q) * wm_interm_171_triplet_pt3(b,j,i,k) * wm_interm_193_triplet_pt3(a,b,k,j)
term(993) = term(993) + t2(a,p,i,q) * wm_interm_171_triplet_pt3(b,j,i,k) * wm_interm_194_triplet_pt3(a,b,k,j)
term(994) = term(994) + t2(a,p,i,q) * wm_interm_185_triplet_pt3(b,j,i,k) * wm_interm_90_triplet_pt3(a,b,j,k)
term(995) = term(995) + t2(a,p,i,q) * wm_interm_185_triplet_pt3(b,j,i,k) * wm_interm_93_triplet_pt3(a,b,j,k)
term(996) = term(996) + t2(a,p,i,q) * wm_interm_185_triplet_pt3(b,j,i,k) * wm_interm_95_triplet_pt3(a,b,j,k)
term(997) = term(997) + t2(a,p,i,q) * wm_interm_185_triplet_pt3(b,j,i,k) * wm_interm_94_triplet_pt3(a,b,j,k)
term(998) = term(998) + t2(a,p,i,q) * wm_interm_171_triplet_pt3(b,j,i,k) * wm_interm_192_triplet_pt3(a,b,k,j)
term(999) = term(999) + t2(a,p,i,q) * wm_interm_171_triplet_pt3(b,j,i,k) * wm_interm_190_triplet_pt3(a,b,k,j)
term(1000) = term(1000) + t2(a,p,i,q) * wm_interm_175_triplet_pt3(b,j,i,k) * wm_interm_192_triplet_pt3(a,b,k,j)
term(1001) = term(1001) + t2(a,p,q,i) * wm_interm_171_triplet_pt3(b,i,j,k) * wm_interm_193_triplet_pt3(a,b,k,j)
term(1002) = term(1002) + t2(a,p,q,i) * wm_interm_171_triplet_pt3(b,i,j,k) * wm_interm_194_triplet_pt3(a,b,k,j)
term(1003) = term(1003) + t2(a,p,q,i) * wm_interm_171_triplet_pt3(b,j,i,k) * wm_interm_193_triplet_pt3(a,b,k,j)
term(1004) = term(1004) + t2(a,p,q,i) * wm_interm_171_triplet_pt3(b,j,i,k) * wm_interm_194_triplet_pt3(a,b,k,j)
term(1005) = term(1005) + t2(a,p,q,i) * wm_interm_175_triplet_pt3(b,i,j,k) * wm_interm_193_triplet_pt3(a,b,k,j)
term(1006) = term(1006) + t2(a,p,q,i) * wm_interm_175_triplet_pt3(b,i,j,k) * wm_interm_194_triplet_pt3(a,b,k,j)
term(1007) = term(1007) + t2(a,p,q,i) * wm_interm_175_triplet_pt3(b,i,j,k) * wm_interm_190_triplet_pt3(a,b,k,j)
term(1008) = term(1008) + t2(a,p,q,i) * wm_interm_171_triplet_pt3(b,i,j,k) * wm_interm_192_triplet_pt3(a,b,k,j)
term(1009) = term(1009) + t2(a,p,q,i) * wm_interm_171_triplet_pt3(b,i,j,k) * wm_interm_190_triplet_pt3(a,b,k,j)
term(1010) = term(1010) + t2(a,p,q,i) * wm_interm_171_triplet_pt3(b,j,i,k) * wm_interm_192_triplet_pt3(a,b,k,j)
term(1011) = term(1011) + t2(a,p,q,i) * wm_interm_171_triplet_pt3(b,j,i,k) * wm_interm_190_triplet_pt3(a,b,k,j)
term(1012) = term(1012) + t2(a,p,q,i) * wm_interm_175_triplet_pt3(b,j,i,k) * wm_interm_192_triplet_pt3(a,b,k,j)
term(1013) = term(1013) + t2(a,p,i,q) * wm_interm_151_triplet_pt3(a,b,j,k) * wm_interm_185_triplet_pt3(b,j,i,k)
term(1014) = term(1014) + t2(a,p,i,q) * wm_interm_150_triplet_pt3(a,b,j,k) * wm_interm_185_triplet_pt3(b,j,i,k)
term(1015) = term(1015) + t2(a,p,i,q) * wm_interm_179_triplet_pt3(b,j,i,k) * wm_interm_193_triplet_pt3(a,b,k,j)
term(1016) = term(1016) + t2(a,p,i,q) * wm_interm_179_triplet_pt3(b,j,i,k) * wm_interm_194_triplet_pt3(a,b,k,j)
term(1017) = term(1017) + t2(a,p,i,q) * wm_interm_152_triplet_pt3(a,b,j,k) * wm_interm_185_triplet_pt3(b,j,i,k)
term(1018) = term(1018) + t2(a,p,i,q) * wm_interm_153_triplet_pt3(a,b,j,k) * wm_interm_185_triplet_pt3(b,j,i,k)
term(1019) = term(1019) + t2(a,p,i,q) * wm_interm_179_triplet_pt3(b,j,i,k) * wm_interm_192_triplet_pt3(a,b,k,j)
term(1020) = term(1020) + t2(a,p,i,q) * wm_interm_179_triplet_pt3(b,j,i,k) * wm_interm_190_triplet_pt3(a,b,k,j)
term(1021) = term(1021) + t2(a,p,i,q) * wm_interm_182_triplet_pt3(b,j,i,k) * wm_interm_192_triplet_pt3(a,b,k,j)
term(1022) = term(1022) + t2(a,p,q,i) * wm_interm_179_triplet_pt3(b,i,j,k) * wm_interm_193_triplet_pt3(a,b,k,j)
term(1023) = term(1023) + t2(a,p,q,i) * wm_interm_179_triplet_pt3(b,i,j,k) * wm_interm_194_triplet_pt3(a,b,k,j)
term(1024) = term(1024) + t2(a,p,q,i) * wm_interm_179_triplet_pt3(b,j,i,k) * wm_interm_193_triplet_pt3(a,b,k,j)
term(1025) = term(1025) + t2(a,p,q,i) * wm_interm_179_triplet_pt3(b,j,i,k) * wm_interm_194_triplet_pt3(a,b,k,j)
term(1026) = term(1026) + t2(a,p,q,i) * wm_interm_182_triplet_pt3(b,i,j,k) * wm_interm_193_triplet_pt3(a,b,k,j)
term(1027) = term(1027) + t2(a,p,q,i) * wm_interm_182_triplet_pt3(b,i,j,k) * wm_interm_194_triplet_pt3(a,b,k,j)
term(1028) = term(1028) + t2(a,p,q,i) * wm_interm_182_triplet_pt3(b,i,j,k) * wm_interm_190_triplet_pt3(a,b,k,j)
term(1029) = term(1029) + t2(a,p,q,i) * wm_interm_179_triplet_pt3(b,i,j,k) * wm_interm_192_triplet_pt3(a,b,k,j)
term(1030) = term(1030) + t2(a,p,q,i) * wm_interm_179_triplet_pt3(b,i,j,k) * wm_interm_190_triplet_pt3(a,b,k,j)
term(1031) = term(1031) + t2(a,p,q,i) * wm_interm_179_triplet_pt3(b,j,i,k) * wm_interm_192_triplet_pt3(a,b,k,j)
term(1032) = term(1032) + t2(a,p,q,i) * wm_interm_179_triplet_pt3(b,j,i,k) * wm_interm_190_triplet_pt3(a,b,k,j)
term(1033) = term(1033) + t2(a,p,q,i) * wm_interm_182_triplet_pt3(b,j,i,k) * wm_interm_192_triplet_pt3(a,b,k,j)
end do 
end do 
end do 
end do 
end do 

term(980) = term(980) * (-4.0d+0) 
term(981) = term(981) * (8.0d+0) 
term(982) = term(982) * (8.0d+0) 
term(983) = term(983) * (-16.0d+0) 
term(984) = term(984) * (-4.0d+0) 
term(985) = term(985) * (8.0d+0) 
term(986) = term(986) * (-16.0d+0) 
term(987) = term(987) * (16.0d+0) 
term(988) = term(988) * (32.0d+0) 
term(989) = term(989) * (-32.0d+0) 
term(990) = term(990) * (8.0d+0) 
term(991) = term(991) * (-4.0d+0) 
term(992) = term(992) * (2.0d+0) 
term(993) = term(993) * (-4.0d+0) 
term(994) = term(994) * (-4.0d+0) 
term(995) = term(995) * (2.0d+0) 
term(996) = term(996) * (-4.0d+0) 
term(997) = term(997) * (2.0d+0) 
term(998) = term(998) * (-4.0d+0) 
term(999) = term(999) * (2.0d+0) 
term(1000) = term(1000) * (2.0d+0) 
term(1001) = term(1001) * (2.0d+0) 
term(1002) = term(1002) * (-4.0d+0) 
term(1003) = term(1003) * (-1.0d+0) 
term(1004) = term(1004) * (2.0d+0) 
term(1005) = term(1005) * (-1.0d+0) 
term(1006) = term(1006) * (2.0d+0) 
term(1007) = term(1007) * (-1.0d+0) 
term(1008) = term(1008) * (-1.0d+0) 
term(1009) = term(1009) * (2.0d+0) 
term(1010) = term(1010) * (2.0d+0) 
term(1011) = term(1011) * (-1.0d+0) 
term(1012) = term(1012) * (-1.0d+0) 
term(1013) = term(1013) * (16.0d+0) 
term(1014) = term(1014) * (-16.0d+0) 
term(1015) = term(1015) * (4.0d+0) 
term(1016) = term(1016) * (-8.0d+0) 
term(1017) = term(1017) * (8.0d+0) 
term(1018) = term(1018) * (-8.0d+0) 
term(1019) = term(1019) * (-8.0d+0) 
term(1020) = term(1020) * (4.0d+0) 
term(1021) = term(1021) * (4.0d+0) 
term(1022) = term(1022) * (4.0d+0) 
term(1023) = term(1023) * (-8.0d+0) 
term(1024) = term(1024) * (-2.0d+0) 
term(1025) = term(1025) * (4.0d+0) 
term(1026) = term(1026) * (-2.0d+0) 
term(1027) = term(1027) * (4.0d+0) 
term(1028) = term(1028) * (-2.0d+0) 
term(1029) = term(1029) * (-2.0d+0) 
term(1030) = term(1030) * (4.0d+0) 
term(1031) = term(1031) * (4.0d+0) 
term(1032) = term(1032) * (-2.0d+0) 
term(1033) = term(1033) * (-2.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(1034) = term(1034) + r1(vrdav_Rl, a,i) * wm_interm_190_triplet_pt3(b,a,q,j) * wm_interm_21_triplet_pt3(b,p,i,j)
term(1035) = term(1035) + r1(vrdav_Rl, a,i) * wm_interm_190_triplet_pt3(b,a,q,j) * wm_interm_22_triplet_pt3(b,p,i,j)
term(1036) = term(1036) + r1(vrdav_Rl, a,i) * wm_interm_190_triplet_pt3(b,a,q,j) * wm_interm_26_triplet_pt3(b,p,i,j)
term(1037) = term(1037) + r1(vrdav_Rl, a,q) * wm_interm_190_triplet_pt3(b,a,i,j) * wm_interm_30_triplet_pt3(b,p,i,j)
term(1038) = term(1038) + r1(vrdav_Rl, a,q) * wm_interm_190_triplet_pt3(b,a,i,j) * wm_interm_25_triplet_pt3(b,p,i,j)
term(1039) = term(1039) + r1(vrdav_Rl, a,q) * wm_interm_190_triplet_pt3(b,a,i,j) * wm_interm_24_triplet_pt3(b,p,i,j)
term(1040) = term(1040) + r1(vrdav_Rl, a,i) * wm_interm_190_triplet_pt3(b,a,q,j) * wm_interm_25_triplet_pt3(b,p,i,j)
term(1041) = term(1041) + r1(vrdav_Rl, a,i) * wm_interm_190_triplet_pt3(b,a,q,j) * wm_interm_30_triplet_pt3(b,p,i,j)
term(1042) = term(1042) + r1(vrdav_Rl, a,i) * wm_interm_190_triplet_pt3(b,a,q,j) * wm_interm_23_triplet_pt3(b,p,i,j)
term(1043) = term(1043) + r1(vrdav_Rl, a,q) * wm_interm_192_triplet_pt3(b,a,i,j) * wm_interm_21_triplet_pt3(b,p,i,j)
term(1044) = term(1044) + r1(vrdav_Rl, a,q) * wm_interm_192_triplet_pt3(b,a,i,j) * wm_interm_22_triplet_pt3(b,p,i,j)
term(1045) = term(1045) + r1(vrdav_Rl, a,q) * wm_interm_192_triplet_pt3(b,a,i,j) * wm_interm_26_triplet_pt3(b,p,i,j)
term(1046) = term(1046) + r1(vrdav_Rl, a,i) * wm_interm_192_triplet_pt3(b,a,q,j) * wm_interm_21_triplet_pt3(b,p,i,j)
term(1047) = term(1047) + r1(vrdav_Rl, a,i) * wm_interm_192_triplet_pt3(b,a,q,j) * wm_interm_22_triplet_pt3(b,p,i,j)
term(1048) = term(1048) + r1(vrdav_Rl, a,i) * wm_interm_192_triplet_pt3(b,a,q,j) * wm_interm_26_triplet_pt3(b,p,i,j)
term(1049) = term(1049) + r1(vrdav_Rl, a,i) * wm_interm_192_triplet_pt3(b,a,q,j) * wm_interm_30_triplet_pt3(b,p,i,j)
term(1050) = term(1050) + r1(vrdav_Rl, a,i) * wm_interm_192_triplet_pt3(b,a,q,j) * wm_interm_24_triplet_pt3(b,p,i,j)
term(1051) = term(1051) + r1(vrdav_Rl, a,i) * wm_interm_192_triplet_pt3(b,a,q,j) * wm_interm_23_triplet_pt3(b,p,i,j)
term(1052) = term(1052) + r1(vrdav_Rl, a,q) * wm_interm_192_triplet_pt3(b,a,i,j) * wm_interm_25_triplet_pt3(b,p,i,j)
term(1053) = term(1053) + r1(vrdav_Rl, a,q) * wm_interm_192_triplet_pt3(b,a,i,j) * wm_interm_23_triplet_pt3(b,p,i,j)
term(1054) = term(1054) + r1(vrdav_Rl, a,q) * wm_interm_192_triplet_pt3(b,a,i,j) * wm_interm_24_triplet_pt3(b,p,i,j)
term(1055) = term(1055) + r1(vrdav_Rl, a,i) * wm_interm_193_triplet_pt3(b,a,q,j) * wm_interm_21_triplet_pt3(b,p,i,j)
term(1056) = term(1056) + r1(vrdav_Rl, a,i) * wm_interm_193_triplet_pt3(b,a,q,j) * wm_interm_22_triplet_pt3(b,p,i,j)
term(1057) = term(1057) + r1(vrdav_Rl, a,i) * wm_interm_193_triplet_pt3(b,a,q,j) * wm_interm_26_triplet_pt3(b,p,i,j)
term(1058) = term(1058) + r1(vrdav_Rl, a,i) * wm_interm_194_triplet_pt3(b,a,q,j) * wm_interm_21_triplet_pt3(b,p,i,j)
term(1059) = term(1059) + r1(vrdav_Rl, a,i) * wm_interm_194_triplet_pt3(b,a,q,j) * wm_interm_22_triplet_pt3(b,p,i,j)
term(1060) = term(1060) + r1(vrdav_Rl, a,i) * wm_interm_194_triplet_pt3(b,a,q,j) * wm_interm_26_triplet_pt3(b,p,i,j)
term(1061) = term(1061) + r1(vrdav_Rl, a,q) * wm_interm_193_triplet_pt3(b,a,i,j) * wm_interm_30_triplet_pt3(b,p,i,j)
term(1062) = term(1062) + r1(vrdav_Rl, a,q) * wm_interm_193_triplet_pt3(b,a,i,j) * wm_interm_25_triplet_pt3(b,p,i,j)
term(1063) = term(1063) + r1(vrdav_Rl, a,q) * wm_interm_194_triplet_pt3(b,a,i,j) * wm_interm_30_triplet_pt3(b,p,i,j)
term(1064) = term(1064) + r1(vrdav_Rl, a,q) * wm_interm_194_triplet_pt3(b,a,i,j) * wm_interm_25_triplet_pt3(b,p,i,j)
term(1065) = term(1065) + r1(vrdav_Rl, a,q) * wm_interm_193_triplet_pt3(b,a,i,j) * wm_interm_24_triplet_pt3(b,p,i,j)
term(1066) = term(1066) + r1(vrdav_Rl, a,q) * wm_interm_194_triplet_pt3(b,a,i,j) * wm_interm_24_triplet_pt3(b,p,i,j)
term(1067) = term(1067) + r1(vrdav_Rl, a,i) * wm_interm_193_triplet_pt3(b,a,q,j) * wm_interm_25_triplet_pt3(b,p,i,j)
term(1068) = term(1068) + r1(vrdav_Rl, a,i) * wm_interm_193_triplet_pt3(b,a,q,j) * wm_interm_30_triplet_pt3(b,p,i,j)
term(1069) = term(1069) + r1(vrdav_Rl, a,i) * wm_interm_194_triplet_pt3(b,a,q,j) * wm_interm_25_triplet_pt3(b,p,i,j)
term(1070) = term(1070) + r1(vrdav_Rl, a,i) * wm_interm_194_triplet_pt3(b,a,q,j) * wm_interm_30_triplet_pt3(b,p,i,j)
term(1071) = term(1071) + r1(vrdav_Rl, a,i) * wm_interm_193_triplet_pt3(b,a,q,j) * wm_interm_23_triplet_pt3(b,p,i,j)
term(1072) = term(1072) + r1(vrdav_Rl, a,i) * wm_interm_194_triplet_pt3(b,a,q,j) * wm_interm_23_triplet_pt3(b,p,i,j)
term(1073) = term(1073) + r1(vrdav_Rl, a,i) * wm_interm_190_triplet_pt3(b,a,q,j) * wm_interm_58_triplet_pt3(b,p,i,j)
term(1074) = term(1074) + r1(vrdav_Rl, a,i) * wm_interm_190_triplet_pt3(b,a,q,j) * wm_interm_59_triplet_pt3(b,p,i,j)
term(1075) = term(1075) + r1(vrdav_Rl, a,q) * wm_interm_190_triplet_pt3(b,a,i,j) * wm_interm_61_triplet_pt3(b,p,i,j)
term(1076) = term(1076) + r1(vrdav_Rl, a,q) * wm_interm_190_triplet_pt3(b,a,i,j) * wm_interm_60_triplet_pt3(b,p,i,j)
term(1077) = term(1077) + r1(vrdav_Rl, a,i) * wm_interm_190_triplet_pt3(b,a,q,j) * wm_interm_60_triplet_pt3(b,p,i,j)
term(1078) = term(1078) + r1(vrdav_Rl, a,i) * wm_interm_190_triplet_pt3(b,a,q,j) * wm_interm_61_triplet_pt3(b,p,i,j)
term(1079) = term(1079) + r1(vrdav_Rl, a,q) * wm_interm_192_triplet_pt3(b,a,i,j) * wm_interm_58_triplet_pt3(b,p,i,j)
term(1080) = term(1080) + r1(vrdav_Rl, a,q) * wm_interm_192_triplet_pt3(b,a,i,j) * wm_interm_59_triplet_pt3(b,p,i,j)
term(1081) = term(1081) + r1(vrdav_Rl, a,i) * wm_interm_192_triplet_pt3(b,a,q,j) * wm_interm_58_triplet_pt3(b,p,i,j)
term(1082) = term(1082) + r1(vrdav_Rl, a,i) * wm_interm_192_triplet_pt3(b,a,q,j) * wm_interm_59_triplet_pt3(b,p,i,j)
term(1083) = term(1083) + r1(vrdav_Rl, a,i) * wm_interm_192_triplet_pt3(b,a,q,j) * wm_interm_61_triplet_pt3(b,p,i,j)
term(1084) = term(1084) + r1(vrdav_Rl, a,i) * wm_interm_192_triplet_pt3(b,a,q,j) * wm_interm_60_triplet_pt3(b,p,i,j)
term(1085) = term(1085) + r1(vrdav_Rl, a,q) * wm_interm_192_triplet_pt3(b,a,i,j) * wm_interm_60_triplet_pt3(b,p,i,j)
term(1086) = term(1086) + r1(vrdav_Rl, a,q) * wm_interm_192_triplet_pt3(b,a,i,j) * wm_interm_61_triplet_pt3(b,p,i,j)
term(1087) = term(1087) + r1(vrdav_Rl, a,i) * wm_interm_193_triplet_pt3(b,a,q,j) * wm_interm_58_triplet_pt3(b,p,i,j)
term(1088) = term(1088) + r1(vrdav_Rl, a,i) * wm_interm_193_triplet_pt3(b,a,q,j) * wm_interm_59_triplet_pt3(b,p,i,j)
term(1089) = term(1089) + r1(vrdav_Rl, a,i) * wm_interm_194_triplet_pt3(b,a,q,j) * wm_interm_58_triplet_pt3(b,p,i,j)
term(1090) = term(1090) + r1(vrdav_Rl, a,i) * wm_interm_194_triplet_pt3(b,a,q,j) * wm_interm_59_triplet_pt3(b,p,i,j)
term(1091) = term(1091) + r1(vrdav_Rl, a,q) * wm_interm_193_triplet_pt3(b,a,i,j) * wm_interm_61_triplet_pt3(b,p,i,j)
term(1092) = term(1092) + r1(vrdav_Rl, a,q) * wm_interm_193_triplet_pt3(b,a,i,j) * wm_interm_60_triplet_pt3(b,p,i,j)
term(1093) = term(1093) + r1(vrdav_Rl, a,q) * wm_interm_194_triplet_pt3(b,a,i,j) * wm_interm_61_triplet_pt3(b,p,i,j)
term(1094) = term(1094) + r1(vrdav_Rl, a,q) * wm_interm_194_triplet_pt3(b,a,i,j) * wm_interm_60_triplet_pt3(b,p,i,j)
term(1095) = term(1095) + r1(vrdav_Rl, a,i) * wm_interm_193_triplet_pt3(b,a,q,j) * wm_interm_60_triplet_pt3(b,p,i,j)
term(1096) = term(1096) + r1(vrdav_Rl, a,i) * wm_interm_193_triplet_pt3(b,a,q,j) * wm_interm_61_triplet_pt3(b,p,i,j)
term(1097) = term(1097) + r1(vrdav_Rl, a,i) * wm_interm_194_triplet_pt3(b,a,q,j) * wm_interm_60_triplet_pt3(b,p,i,j)
term(1098) = term(1098) + r1(vrdav_Rl, a,i) * wm_interm_194_triplet_pt3(b,a,q,j) * wm_interm_61_triplet_pt3(b,p,i,j)
end do 
end do 
end do 
end do 

term(1034) = term(1034) * (2.0d+0) 
term(1035) = term(1035) * (-4.0d+0) 
term(1036) = term(1036) * (2.0d+0) 
term(1037) = term(1037) * (2.0d+0) 
term(1038) = term(1038) * (-4.0d+0) 
term(1039) = term(1039) * (2.0d+0) 
term(1040) = term(1040) * (2.0d+0) 
term(1041) = term(1041) * (-4.0d+0) 
term(1042) = term(1042) * (2.0d+0) 
term(1043) = term(1043) * (2.0d+0) 
term(1044) = term(1044) * (-4.0d+0) 
term(1045) = term(1045) * (2.0d+0) 
term(1046) = term(1046) * (-4.0d+0) 
term(1047) = term(1047) * (8.0d+0) 
term(1048) = term(1048) * (-4.0d+0) 
term(1049) = term(1049) * (2.0d+0) 
term(1050) = term(1050) * (2.0d+0) 
term(1051) = term(1051) * (-4.0d+0) 
term(1052) = term(1052) * (2.0d+0) 
term(1053) = term(1053) * (2.0d+0) 
term(1054) = term(1054) * (-4.0d+0) 
term(1055) = term(1055) * (2.0d+0) 
term(1056) = term(1056) * (-4.0d+0) 
term(1057) = term(1057) * (2.0d+0) 
term(1058) = term(1058) * (-4.0d+0) 
term(1059) = term(1059) * (8.0d+0) 
term(1060) = term(1060) * (-4.0d+0) 
term(1061) = term(1061) * (2.0d+0) 
term(1062) = term(1062) * (-4.0d+0) 
term(1063) = term(1063) * (-4.0d+0) 
term(1064) = term(1064) * (8.0d+0) 
term(1065) = term(1065) * (2.0d+0) 
term(1066) = term(1066) * (-4.0d+0) 
term(1067) = term(1067) * (2.0d+0) 
term(1068) = term(1068) * (-4.0d+0) 
term(1069) = term(1069) * (-4.0d+0) 
term(1070) = term(1070) * (8.0d+0) 
term(1071) = term(1071) * (2.0d+0) 
term(1072) = term(1072) * (-4.0d+0) 
term(1073) = term(1073) * (8.0d+0) 
term(1074) = term(1074) * (-8.0d+0) 
term(1075) = term(1075) * (8.0d+0) 
term(1076) = term(1076) * (-8.0d+0) 
term(1077) = term(1077) * (8.0d+0) 
term(1078) = term(1078) * (-8.0d+0) 
term(1079) = term(1079) * (8.0d+0) 
term(1080) = term(1080) * (-8.0d+0) 
term(1081) = term(1081) * (-16.0d+0) 
term(1082) = term(1082) * (16.0d+0) 
term(1083) = term(1083) * (8.0d+0) 
term(1084) = term(1084) * (-8.0d+0) 
term(1085) = term(1085) * (8.0d+0) 
term(1086) = term(1086) * (-8.0d+0) 
term(1087) = term(1087) * (8.0d+0) 
term(1088) = term(1088) * (-8.0d+0) 
term(1089) = term(1089) * (-16.0d+0) 
term(1090) = term(1090) * (16.0d+0) 
term(1091) = term(1091) * (8.0d+0) 
term(1092) = term(1092) * (-8.0d+0) 
term(1093) = term(1093) * (-16.0d+0) 
term(1094) = term(1094) * (16.0d+0) 
term(1095) = term(1095) * (8.0d+0) 
term(1096) = term(1096) * (-8.0d+0) 
term(1097) = term(1097) * (-16.0d+0) 
term(1098) = term(1098) * (16.0d+0) 

do a = nocc + 1, nactive 
term(1099) = term(1099) + wm_interm_84_triplet_pt3(a,q) * wm_interm_9_triplet_pt3(a,p)
term(1100) = term(1100) + wm_interm_5_triplet_pt3(a,p) * wm_interm_84_triplet_pt3(a,q)
term(1101) = term(1101) + wm_interm_6_triplet_pt3(a,p) * wm_interm_84_triplet_pt3(a,q)
term(1102) = term(1102) + wm_interm_85_triplet_pt3(a,q) * wm_interm_9_triplet_pt3(a,p)
term(1103) = term(1103) + wm_interm_5_triplet_pt3(a,p) * wm_interm_85_triplet_pt3(a,q)
term(1104) = term(1104) + wm_interm_6_triplet_pt3(a,p) * wm_interm_85_triplet_pt3(a,q)
term(1105) = term(1105) + wm_interm_86_triplet_pt3(a,q) * wm_interm_9_triplet_pt3(a,p)
term(1106) = term(1106) + wm_interm_5_triplet_pt3(a,p) * wm_interm_86_triplet_pt3(a,q)
term(1107) = term(1107) + wm_interm_6_triplet_pt3(a,p) * wm_interm_86_triplet_pt3(a,q)
term(1108) = term(1108) + wm_interm_47_triplet_pt3(a,p) * wm_interm_84_triplet_pt3(a,q)
term(1109) = term(1109) + wm_interm_48_triplet_pt3(a,p) * wm_interm_84_triplet_pt3(a,q)
term(1110) = term(1110) + wm_interm_47_triplet_pt3(a,p) * wm_interm_85_triplet_pt3(a,q)
term(1111) = term(1111) + wm_interm_48_triplet_pt3(a,p) * wm_interm_85_triplet_pt3(a,q)
term(1112) = term(1112) + wm_interm_47_triplet_pt3(a,p) * wm_interm_86_triplet_pt3(a,q)
term(1113) = term(1113) + wm_interm_48_triplet_pt3(a,p) * wm_interm_86_triplet_pt3(a,q)
term(1114) = term(1114) + wm_interm_43_triplet_pt3(a,p) * wm_interm_72_triplet_pt3(a,q)
term(1115) = term(1115) + wm_interm_44_triplet_pt3(a,p) * wm_interm_72_triplet_pt3(a,q)
term(1116) = term(1116) + wm_interm_29_triplet_pt3(a,p) * wm_interm_72_triplet_pt3(a,q)
term(1117) = term(1117) + wm_interm_27_triplet_pt3(a,p) * wm_interm_72_triplet_pt3(a,q)
term(1118) = term(1118) + wm_interm_28_triplet_pt3(a,p) * wm_interm_72_triplet_pt3(a,q)
term(1119) = term(1119) + wm_interm_42_triplet_pt3(a,p) * wm_interm_72_triplet_pt3(a,q)
term(1120) = term(1120) + wm_interm_43_triplet_pt3(a,p) * wm_interm_73_triplet_pt3(a,q)
term(1121) = term(1121) + wm_interm_44_triplet_pt3(a,p) * wm_interm_73_triplet_pt3(a,q)
term(1122) = term(1122) + wm_interm_29_triplet_pt3(a,p) * wm_interm_73_triplet_pt3(a,q)
term(1123) = term(1123) + wm_interm_27_triplet_pt3(a,p) * wm_interm_73_triplet_pt3(a,q)
term(1124) = term(1124) + wm_interm_28_triplet_pt3(a,p) * wm_interm_73_triplet_pt3(a,q)
term(1125) = term(1125) + wm_interm_42_triplet_pt3(a,p) * wm_interm_73_triplet_pt3(a,q)
term(1126) = term(1126) + wm_interm_70_triplet_pt3(a,p) * wm_interm_72_triplet_pt3(a,q)
term(1127) = term(1127) + wm_interm_71_triplet_pt3(a,p) * wm_interm_72_triplet_pt3(a,q)
term(1128) = term(1128) + wm_interm_62_triplet_pt3(a,p) * wm_interm_72_triplet_pt3(a,q)
term(1129) = term(1129) + wm_interm_63_triplet_pt3(a,p) * wm_interm_72_triplet_pt3(a,q)
term(1130) = term(1130) + wm_interm_70_triplet_pt3(a,p) * wm_interm_73_triplet_pt3(a,q)
term(1131) = term(1131) + wm_interm_71_triplet_pt3(a,p) * wm_interm_73_triplet_pt3(a,q)
term(1132) = term(1132) + wm_interm_62_triplet_pt3(a,p) * wm_interm_73_triplet_pt3(a,q)
term(1133) = term(1133) + wm_interm_63_triplet_pt3(a,p) * wm_interm_73_triplet_pt3(a,q)
term(1134) = term(1134) + wm_interm_148_triplet_pt3(a,q) * wm_interm_9_triplet_pt3(a,p)
term(1135) = term(1135) + wm_interm_148_triplet_pt3(a,q) * wm_interm_5_triplet_pt3(a,p)
term(1136) = term(1136) + wm_interm_148_triplet_pt3(a,q) * wm_interm_6_triplet_pt3(a,p)
term(1137) = term(1137) + wm_interm_149_triplet_pt3(a,q) * wm_interm_9_triplet_pt3(a,p)
term(1138) = term(1138) + wm_interm_149_triplet_pt3(a,q) * wm_interm_5_triplet_pt3(a,p)
term(1139) = term(1139) + wm_interm_149_triplet_pt3(a,q) * wm_interm_6_triplet_pt3(a,p)
term(1140) = term(1140) + wm_interm_148_triplet_pt3(a,q) * wm_interm_47_triplet_pt3(a,p)
term(1141) = term(1141) + wm_interm_148_triplet_pt3(a,q) * wm_interm_48_triplet_pt3(a,p)
term(1142) = term(1142) + wm_interm_149_triplet_pt3(a,q) * wm_interm_47_triplet_pt3(a,p)
term(1143) = term(1143) + wm_interm_149_triplet_pt3(a,q) * wm_interm_48_triplet_pt3(a,p)
term(1144) = term(1144) + wm_interm_129_triplet_pt3(a,p) * wm_interm_72_triplet_pt3(a,q)
term(1145) = term(1145) + wm_interm_130_triplet_pt3(a,p) * wm_interm_72_triplet_pt3(a,q)
term(1146) = term(1146) + wm_interm_122_triplet_pt3(a,p) * wm_interm_72_triplet_pt3(a,q)
term(1147) = term(1147) + wm_interm_120_triplet_pt3(a,p) * wm_interm_72_triplet_pt3(a,q)
term(1148) = term(1148) + wm_interm_121_triplet_pt3(a,p) * wm_interm_72_triplet_pt3(a,q)
term(1149) = term(1149) + wm_interm_128_triplet_pt3(a,p) * wm_interm_72_triplet_pt3(a,q)
term(1150) = term(1150) + wm_interm_129_triplet_pt3(a,p) * wm_interm_73_triplet_pt3(a,q)
term(1151) = term(1151) + wm_interm_130_triplet_pt3(a,p) * wm_interm_73_triplet_pt3(a,q)
term(1152) = term(1152) + wm_interm_122_triplet_pt3(a,p) * wm_interm_73_triplet_pt3(a,q)
term(1153) = term(1153) + wm_interm_120_triplet_pt3(a,p) * wm_interm_73_triplet_pt3(a,q)
term(1154) = term(1154) + wm_interm_121_triplet_pt3(a,p) * wm_interm_73_triplet_pt3(a,q)
term(1155) = term(1155) + wm_interm_128_triplet_pt3(a,p) * wm_interm_73_triplet_pt3(a,q)
term(1156) = term(1156) + wm_interm_142_triplet_pt3(a,p) * wm_interm_72_triplet_pt3(a,q)
term(1157) = term(1157) + wm_interm_143_triplet_pt3(a,p) * wm_interm_72_triplet_pt3(a,q)
term(1158) = term(1158) + wm_interm_138_triplet_pt3(a,p) * wm_interm_72_triplet_pt3(a,q)
term(1159) = term(1159) + wm_interm_139_triplet_pt3(a,p) * wm_interm_72_triplet_pt3(a,q)
term(1160) = term(1160) + wm_interm_142_triplet_pt3(a,p) * wm_interm_73_triplet_pt3(a,q)
term(1161) = term(1161) + wm_interm_143_triplet_pt3(a,p) * wm_interm_73_triplet_pt3(a,q)
term(1162) = term(1162) + wm_interm_138_triplet_pt3(a,p) * wm_interm_73_triplet_pt3(a,q)
term(1163) = term(1163) + wm_interm_139_triplet_pt3(a,p) * wm_interm_73_triplet_pt3(a,q)
term(1164) = term(1164) + wm_interm_177_triplet_pt3(p,a) * wm_interm_186_triplet_pt3(a,q)
term(1165) = term(1165) + wm_interm_187_triplet_pt3(a,p) * wm_interm_72_triplet_pt3(a,q)
term(1166) = term(1166) + wm_interm_187_triplet_pt3(a,p) * wm_interm_73_triplet_pt3(a,q)
end do 

term(1101) = term(1101) * (-2.0d+0) 
term(1102) = term(1102) * (-0.5d+0) 
term(1103) = term(1103) * (-0.5d+0) 
term(1105) = term(1105) * (-0.5d+0) 
term(1106) = term(1106) * (-0.5d+0) 
term(1108) = term(1108) * (4.0d+0) 
term(1109) = term(1109) * (-4.0d+0) 
term(1110) = term(1110) * (-2.0d+0) 
term(1111) = term(1111) * (2.0d+0) 
term(1112) = term(1112) * (-2.0d+0) 
term(1113) = term(1113) * (2.0d+0) 
term(1115) = term(1115) * (-2.0d+0) 
term(1118) = term(1118) * (-2.0d+0) 
term(1120) = term(1120) * (-0.5d+0) 
term(1122) = term(1122) * (-0.5d+0) 
term(1123) = term(1123) * (-0.5d+0) 
term(1125) = term(1125) * (-0.5d+0) 
term(1126) = term(1126) * (4.0d+0) 
term(1127) = term(1127) * (-4.0d+0) 
term(1128) = term(1128) * (4.0d+0) 
term(1129) = term(1129) * (-4.0d+0) 
term(1130) = term(1130) * (-2.0d+0) 
term(1131) = term(1131) * (2.0d+0) 
term(1132) = term(1132) * (-2.0d+0) 
term(1133) = term(1133) * (2.0d+0) 
term(1134) = term(1134) * (2.0d+0) 
term(1135) = term(1135) * (2.0d+0) 
term(1136) = term(1136) * (-4.0d+0) 
term(1137) = term(1137) * (-2.0d+0) 
term(1138) = term(1138) * (-2.0d+0) 
term(1139) = term(1139) * (4.0d+0) 
term(1140) = term(1140) * (8.0d+0) 
term(1141) = term(1141) * (-8.0d+0) 
term(1142) = term(1142) * (-8.0d+0) 
term(1143) = term(1143) * (8.0d+0) 
term(1144) = term(1144) * (2.0d+0) 
term(1145) = term(1145) * (-4.0d+0) 
term(1146) = term(1146) * (2.0d+0) 
term(1147) = term(1147) * (2.0d+0) 
term(1148) = term(1148) * (-4.0d+0) 
term(1149) = term(1149) * (2.0d+0) 
term(1150) = term(1150) * (-1.0d+0) 
term(1151) = term(1151) * (2.0d+0) 
term(1152) = term(1152) * (-1.0d+0) 
term(1153) = term(1153) * (-1.0d+0) 
term(1154) = term(1154) * (2.0d+0) 
term(1155) = term(1155) * (-1.0d+0) 
term(1156) = term(1156) * (8.0d+0) 
term(1157) = term(1157) * (-8.0d+0) 
term(1158) = term(1158) * (8.0d+0) 
term(1159) = term(1159) * (-8.0d+0) 
term(1160) = term(1160) * (-4.0d+0) 
term(1161) = term(1161) * (4.0d+0) 
term(1162) = term(1162) * (-4.0d+0) 
term(1163) = term(1163) * (4.0d+0) 
term(1164) = term(1164) * (2.0d+0) 
term(1165) = term(1165) * (-4.0d+0) 
term(1166) = term(1166) * (2.0d+0) 

do a = nocc + 1, nactive 
do i = 1, nocc 
term(1167) = term(1167) + r1(vrdav_Rl, a,i) * wm_interm_17_triplet_pt3(i,q) * wm_interm_198_triplet_pt3(p,a)
term(1168) = term(1168) + r1(vrdav_Rl, a,i) * wm_interm_17_triplet_pt3(i,q) * wm_interm_199_triplet_pt3(p,a)
term(1169) = term(1169) + r1(vrdav_Rl, a,i) * wm_interm_18_triplet_pt3(i,q) * wm_interm_198_triplet_pt3(p,a)
term(1170) = term(1170) + r1(vrdav_Rl, a,i) * wm_interm_18_triplet_pt3(i,q) * wm_interm_199_triplet_pt3(p,a)
term(1171) = term(1171) + r1(vrdav_Rl, a,i) * wm_interm_198_triplet_pt3(p,a) * wm_interm_19_triplet_pt3(i,q)
term(1172) = term(1172) + r1(vrdav_Rl, a,i) * wm_interm_199_triplet_pt3(p,a) * wm_interm_19_triplet_pt3(i,q)
term(1173) = term(1173) + r1(vrdav_Rl, a,i) * wm_interm_198_triplet_pt3(p,a) * wm_interm_54_triplet_pt3(i,q)
term(1174) = term(1174) + r1(vrdav_Rl, a,i) * wm_interm_199_triplet_pt3(p,a) * wm_interm_54_triplet_pt3(i,q)
term(1175) = term(1175) + r1(vrdav_Rl, a,i) * wm_interm_198_triplet_pt3(p,a) * wm_interm_55_triplet_pt3(i,q)
term(1176) = term(1176) + r1(vrdav_Rl, a,i) * wm_interm_199_triplet_pt3(p,a) * wm_interm_55_triplet_pt3(i,q)
term(1177) = term(1177) + r1(vrdav_Rl, a,i) * wm_interm_198_triplet_pt3(p,a) * wm_interm_56_triplet_pt3(i,q)
term(1178) = term(1178) + r1(vrdav_Rl, a,i) * wm_interm_199_triplet_pt3(p,a) * wm_interm_56_triplet_pt3(i,q)
end do 
end do 

term(1167) = term(1167) * (-1.0d+0) 
term(1168) = term(1168) * (2.0d+0) 
term(1169) = term(1169) * (2.0d+0) 
term(1170) = term(1170) * (-4.0d+0) 
term(1171) = term(1171) * (-1.0d+0) 
term(1172) = term(1172) * (2.0d+0) 
term(1173) = term(1173) * (-2.0d+0) 
term(1174) = term(1174) * (4.0d+0) 
term(1175) = term(1175) * (4.0d+0) 
term(1176) = term(1176) * (-8.0d+0) 
term(1177) = term(1177) * (-2.0d+0) 
term(1178) = term(1178) * (4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(1179) = term(1179) + r1(vrdav_Rl, a,i) * wm_interm_190_triplet_pt3(p,a,j,k) * wm_interm_20_triplet_pt3(j,i,k,q)
term(1180) = term(1180) + r1(vrdav_Rl, a,i) * wm_interm_190_triplet_pt3(p,a,j,k) * wm_interm_20_triplet_pt3(i,j,k,q)
term(1181) = term(1181) + r1(vrdav_Rl, a,i) * wm_interm_190_triplet_pt3(p,a,j,k) * wm_interm_20_triplet_pt3(i,j,q,k)
term(1182) = term(1182) + r1(vrdav_Rl, a,i) * wm_interm_192_triplet_pt3(p,a,j,k) * wm_interm_20_triplet_pt3(i,j,k,q)
term(1183) = term(1183) + r1(vrdav_Rl, a,i) * wm_interm_192_triplet_pt3(p,a,j,k) * wm_interm_20_triplet_pt3(j,i,q,k)
term(1184) = term(1184) + r1(vrdav_Rl, a,i) * wm_interm_192_triplet_pt3(p,a,j,k) * wm_interm_20_triplet_pt3(i,j,q,k)
term(1185) = term(1185) + r1(vrdav_Rl, a,i) * wm_interm_193_triplet_pt3(p,a,j,k) * wm_interm_20_triplet_pt3(j,i,k,q)
term(1186) = term(1186) + r1(vrdav_Rl, a,i) * wm_interm_193_triplet_pt3(p,a,j,k) * wm_interm_20_triplet_pt3(i,j,k,q)
term(1187) = term(1187) + r1(vrdav_Rl, a,i) * wm_interm_194_triplet_pt3(p,a,j,k) * wm_interm_20_triplet_pt3(j,i,k,q)
term(1188) = term(1188) + r1(vrdav_Rl, a,i) * wm_interm_194_triplet_pt3(p,a,j,k) * wm_interm_20_triplet_pt3(i,j,k,q)
term(1189) = term(1189) + r1(vrdav_Rl, a,i) * wm_interm_193_triplet_pt3(p,a,j,k) * wm_interm_20_triplet_pt3(i,j,q,k)
term(1190) = term(1190) + r1(vrdav_Rl, a,i) * wm_interm_194_triplet_pt3(p,a,j,k) * wm_interm_20_triplet_pt3(i,j,q,k)
term(1191) = term(1191) + r1(vrdav_Rl, a,i) * wm_interm_190_triplet_pt3(p,a,j,k) * wm_interm_57_triplet_pt3(j,i,k,q)
term(1192) = term(1192) + r1(vrdav_Rl, a,i) * wm_interm_190_triplet_pt3(p,a,j,k) * wm_interm_57_triplet_pt3(i,j,k,q)
term(1193) = term(1193) + r1(vrdav_Rl, a,i) * wm_interm_190_triplet_pt3(p,a,j,k) * wm_interm_57_triplet_pt3(i,j,q,k)
term(1194) = term(1194) + r1(vrdav_Rl, a,i) * wm_interm_192_triplet_pt3(p,a,j,k) * wm_interm_57_triplet_pt3(i,j,k,q)
term(1195) = term(1195) + r1(vrdav_Rl, a,i) * wm_interm_192_triplet_pt3(p,a,j,k) * wm_interm_57_triplet_pt3(j,i,q,k)
term(1196) = term(1196) + r1(vrdav_Rl, a,i) * wm_interm_192_triplet_pt3(p,a,j,k) * wm_interm_57_triplet_pt3(i,j,q,k)
term(1197) = term(1197) + r1(vrdav_Rl, a,i) * wm_interm_193_triplet_pt3(p,a,j,k) * wm_interm_57_triplet_pt3(j,i,k,q)
term(1198) = term(1198) + r1(vrdav_Rl, a,i) * wm_interm_193_triplet_pt3(p,a,j,k) * wm_interm_57_triplet_pt3(i,j,k,q)
term(1199) = term(1199) + r1(vrdav_Rl, a,i) * wm_interm_194_triplet_pt3(p,a,j,k) * wm_interm_57_triplet_pt3(j,i,k,q)
term(1200) = term(1200) + r1(vrdav_Rl, a,i) * wm_interm_194_triplet_pt3(p,a,j,k) * wm_interm_57_triplet_pt3(i,j,k,q)
term(1201) = term(1201) + r1(vrdav_Rl, a,i) * wm_interm_193_triplet_pt3(p,a,j,k) * wm_interm_57_triplet_pt3(i,j,q,k)
term(1202) = term(1202) + r1(vrdav_Rl, a,i) * wm_interm_194_triplet_pt3(p,a,j,k) * wm_interm_57_triplet_pt3(i,j,q,k)
end do 
end do 
end do 
end do 

term(1179) = term(1179) * (2.0d+0) 
term(1180) = term(1180) * (-4.0d+0) 
term(1181) = term(1181) * (2.0d+0) 
term(1182) = term(1182) * (2.0d+0) 
term(1183) = term(1183) * (2.0d+0) 
term(1184) = term(1184) * (-4.0d+0) 
term(1185) = term(1185) * (2.0d+0) 
term(1186) = term(1186) * (-4.0d+0) 
term(1187) = term(1187) * (-4.0d+0) 
term(1188) = term(1188) * (8.0d+0) 
term(1189) = term(1189) * (2.0d+0) 
term(1190) = term(1190) * (-4.0d+0) 
term(1191) = term(1191) * (4.0d+0) 
term(1192) = term(1192) * (-8.0d+0) 
term(1193) = term(1193) * (4.0d+0) 
term(1194) = term(1194) * (4.0d+0) 
term(1195) = term(1195) * (4.0d+0) 
term(1196) = term(1196) * (-8.0d+0) 
term(1197) = term(1197) * (4.0d+0) 
term(1198) = term(1198) * (-8.0d+0) 
term(1199) = term(1199) * (-8.0d+0) 
term(1200) = term(1200) * (16.0d+0) 
term(1201) = term(1201) * (4.0d+0) 
term(1202) = term(1202) * (-8.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(1203) = term(1203) + s2(a,p,q,i) * wm_interm_189_triplet_pt3(b,j,k,i) * wm_interm_26_triplet_pt3(a,b,j,k)
term(1204) = term(1204) + s2(a,p,q,i) * wm_interm_189_triplet_pt3(b,j,k,i) * wm_interm_21_triplet_pt3(a,b,j,k)
term(1205) = term(1205) + s2(a,p,q,i) * wm_interm_189_triplet_pt3(b,j,k,i) * wm_interm_22_triplet_pt3(a,b,j,k)
term(1206) = term(1206) + s2(a,p,i,q) * wm_interm_189_triplet_pt3(b,j,k,i) * wm_interm_21_triplet_pt3(a,b,j,k)
term(1207) = term(1207) + s2(a,p,i,q) * wm_interm_189_triplet_pt3(b,j,k,i) * wm_interm_22_triplet_pt3(a,b,j,k)
term(1208) = term(1208) + s2(a,p,i,q) * wm_interm_189_triplet_pt3(b,j,k,i) * wm_interm_26_triplet_pt3(a,b,j,k)
term(1209) = term(1209) + s2(a,p,q,i) * wm_interm_189_triplet_pt3(b,j,k,i) * wm_interm_25_triplet_pt3(a,b,j,k)
term(1210) = term(1210) + s2(a,p,q,i) * wm_interm_189_triplet_pt3(b,j,k,i) * wm_interm_30_triplet_pt3(a,b,j,k)
term(1211) = term(1211) + s2(a,p,q,i) * wm_interm_189_triplet_pt3(b,j,k,i) * wm_interm_23_triplet_pt3(a,b,j,k)
term(1212) = term(1212) + s2(a,p,i,q) * wm_interm_189_triplet_pt3(b,j,k,i) * wm_interm_23_triplet_pt3(a,b,j,k)
term(1213) = term(1213) + s2(a,p,i,q) * wm_interm_189_triplet_pt3(b,j,k,i) * wm_interm_25_triplet_pt3(a,b,j,k)
term(1214) = term(1214) + s2(a,p,i,q) * wm_interm_189_triplet_pt3(b,j,k,i) * wm_interm_30_triplet_pt3(a,b,j,k)
term(1215) = term(1215) + s2(a,p,q,i) * wm_interm_189_triplet_pt3(b,j,i,k) * wm_interm_30_triplet_pt3(a,b,j,k)
term(1216) = term(1216) + s2(a,p,q,i) * wm_interm_189_triplet_pt3(b,j,i,k) * wm_interm_24_triplet_pt3(a,b,j,k)
term(1217) = term(1217) + s2(a,p,q,i) * wm_interm_189_triplet_pt3(b,j,i,k) * wm_interm_23_triplet_pt3(a,b,j,k)
term(1218) = term(1218) + s2(a,p,q,i) * wm_interm_189_triplet_pt3(b,j,i,k) * wm_interm_26_triplet_pt3(a,b,j,k)
term(1219) = term(1219) + s2(a,p,q,i) * wm_interm_189_triplet_pt3(b,j,i,k) * wm_interm_21_triplet_pt3(a,b,j,k)
term(1220) = term(1220) + s2(a,p,q,i) * wm_interm_189_triplet_pt3(b,j,i,k) * wm_interm_22_triplet_pt3(a,b,j,k)
term(1221) = term(1221) + s2(a,p,q,i) * wm_interm_189_triplet_pt3(b,j,k,i) * wm_interm_58_triplet_pt3(a,b,j,k)
term(1222) = term(1222) + s2(a,p,q,i) * wm_interm_189_triplet_pt3(b,j,k,i) * wm_interm_59_triplet_pt3(a,b,j,k)
term(1223) = term(1223) + s2(a,p,i,q) * wm_interm_189_triplet_pt3(b,j,k,i) * wm_interm_58_triplet_pt3(a,b,j,k)
term(1224) = term(1224) + s2(a,p,i,q) * wm_interm_189_triplet_pt3(b,j,k,i) * wm_interm_59_triplet_pt3(a,b,j,k)
term(1225) = term(1225) + s2(a,p,q,i) * wm_interm_189_triplet_pt3(b,j,k,i) * wm_interm_60_triplet_pt3(a,b,j,k)
term(1226) = term(1226) + s2(a,p,q,i) * wm_interm_189_triplet_pt3(b,j,k,i) * wm_interm_61_triplet_pt3(a,b,j,k)
term(1227) = term(1227) + s2(a,p,i,q) * wm_interm_189_triplet_pt3(b,j,k,i) * wm_interm_60_triplet_pt3(a,b,j,k)
term(1228) = term(1228) + s2(a,p,i,q) * wm_interm_189_triplet_pt3(b,j,k,i) * wm_interm_61_triplet_pt3(a,b,j,k)
term(1229) = term(1229) + s2(a,p,q,i) * wm_interm_189_triplet_pt3(b,j,i,k) * wm_interm_61_triplet_pt3(a,b,j,k)
term(1230) = term(1230) + s2(a,p,q,i) * wm_interm_189_triplet_pt3(b,j,i,k) * wm_interm_60_triplet_pt3(a,b,j,k)
term(1231) = term(1231) + s2(a,p,q,i) * wm_interm_189_triplet_pt3(b,j,i,k) * wm_interm_58_triplet_pt3(a,b,j,k)
term(1232) = term(1232) + s2(a,p,q,i) * wm_interm_189_triplet_pt3(b,j,i,k) * wm_interm_59_triplet_pt3(a,b,j,k)
term(1233) = term(1233) + t2(a,p,i,q) * wm_interm_185_triplet_pt3(b,j,k,i) * wm_interm_92_triplet_pt3(a,b,j,k)
term(1234) = term(1234) + t2(a,p,i,q) * wm_interm_185_triplet_pt3(b,j,k,i) * wm_interm_91_triplet_pt3(a,b,j,k)
term(1235) = term(1235) + t2(a,p,i,q) * wm_interm_185_triplet_pt3(b,j,k,i) * wm_interm_90_triplet_pt3(a,b,j,k)
term(1236) = term(1236) + t2(a,p,i,q) * wm_interm_185_triplet_pt3(b,j,k,i) * wm_interm_93_triplet_pt3(a,b,j,k)
term(1237) = term(1237) + t2(a,p,i,q) * wm_interm_185_triplet_pt3(b,j,k,i) * wm_interm_95_triplet_pt3(a,b,j,k)
term(1238) = term(1238) + t2(a,p,i,q) * wm_interm_104_triplet_pt3(a,b,j,k) * wm_interm_185_triplet_pt3(b,j,k,i)
term(1239) = term(1239) + t2(a,p,q,i) * wm_interm_185_triplet_pt3(b,j,k,i) * wm_interm_92_triplet_pt3(a,b,j,k)
term(1240) = term(1240) + t2(a,p,q,i) * wm_interm_185_triplet_pt3(b,j,i,k) * wm_interm_92_triplet_pt3(a,b,j,k)
term(1241) = term(1241) + t2(a,p,q,i) * wm_interm_185_triplet_pt3(b,j,k,i) * wm_interm_91_triplet_pt3(a,b,j,k)
term(1242) = term(1242) + t2(a,p,q,i) * wm_interm_185_triplet_pt3(b,j,i,k) * wm_interm_91_triplet_pt3(a,b,j,k)
term(1243) = term(1243) + t2(a,p,q,i) * wm_interm_185_triplet_pt3(b,j,k,i) * wm_interm_90_triplet_pt3(a,b,j,k)
term(1244) = term(1244) + t2(a,p,q,i) * wm_interm_185_triplet_pt3(b,j,i,k) * wm_interm_90_triplet_pt3(a,b,j,k)
term(1245) = term(1245) + t2(a,p,q,i) * wm_interm_185_triplet_pt3(b,j,i,k) * wm_interm_93_triplet_pt3(a,b,j,k)
term(1246) = term(1246) + t2(a,p,q,i) * wm_interm_185_triplet_pt3(b,j,k,i) * wm_interm_93_triplet_pt3(a,b,j,k)
term(1247) = term(1247) + t2(a,p,q,i) * wm_interm_185_triplet_pt3(b,j,k,i) * wm_interm_95_triplet_pt3(a,b,j,k)
term(1248) = term(1248) + t2(a,p,q,i) * wm_interm_185_triplet_pt3(b,j,i,k) * wm_interm_95_triplet_pt3(a,b,j,k)
term(1249) = term(1249) + t2(a,p,q,i) * wm_interm_185_triplet_pt3(b,j,i,k) * wm_interm_94_triplet_pt3(a,b,j,k)
term(1250) = term(1250) + t2(a,p,q,i) * wm_interm_104_triplet_pt3(a,b,j,k) * wm_interm_185_triplet_pt3(b,j,k,i)
term(1251) = term(1251) + t2(a,p,j,i) * wm_interm_171_triplet_pt3(b,i,j,k) * wm_interm_193_triplet_pt3(a,b,k,q)
term(1252) = term(1252) + t2(a,p,j,i) * wm_interm_171_triplet_pt3(b,i,j,k) * wm_interm_194_triplet_pt3(a,b,k,q)
term(1253) = term(1253) + t2(a,p,j,i) * wm_interm_171_triplet_pt3(b,i,j,k) * wm_interm_190_triplet_pt3(a,b,k,q)
term(1254) = term(1254) + t2(a,p,j,i) * wm_interm_175_triplet_pt3(b,i,j,k) * wm_interm_192_triplet_pt3(a,b,k,q)
term(1255) = term(1255) + t2(a,p,j,i) * wm_interm_171_triplet_pt3(b,i,j,k) * wm_interm_192_triplet_pt3(a,b,k,q)
term(1256) = term(1256) + t2(a,p,i,q) * wm_interm_151_triplet_pt3(a,b,j,k) * wm_interm_185_triplet_pt3(b,j,k,i)
term(1257) = term(1257) + t2(a,p,i,q) * wm_interm_150_triplet_pt3(a,b,j,k) * wm_interm_185_triplet_pt3(b,j,k,i)
term(1258) = term(1258) + t2(a,p,i,q) * wm_interm_152_triplet_pt3(a,b,j,k) * wm_interm_185_triplet_pt3(b,j,k,i)
term(1259) = term(1259) + t2(a,p,i,q) * wm_interm_153_triplet_pt3(a,b,j,k) * wm_interm_185_triplet_pt3(b,j,k,i)
term(1260) = term(1260) + t2(a,p,q,i) * wm_interm_151_triplet_pt3(a,b,j,k) * wm_interm_185_triplet_pt3(b,j,k,i)
term(1261) = term(1261) + t2(a,p,q,i) * wm_interm_151_triplet_pt3(a,b,j,k) * wm_interm_185_triplet_pt3(b,j,i,k)
term(1262) = term(1262) + t2(a,p,q,i) * wm_interm_150_triplet_pt3(a,b,j,k) * wm_interm_185_triplet_pt3(b,j,k,i)
term(1263) = term(1263) + t2(a,p,q,i) * wm_interm_150_triplet_pt3(a,b,j,k) * wm_interm_185_triplet_pt3(b,j,i,k)
term(1264) = term(1264) + t2(a,p,q,i) * wm_interm_152_triplet_pt3(a,b,j,k) * wm_interm_185_triplet_pt3(b,j,i,k)
term(1265) = term(1265) + t2(a,p,q,i) * wm_interm_152_triplet_pt3(a,b,j,k) * wm_interm_185_triplet_pt3(b,j,k,i)
term(1266) = term(1266) + t2(a,p,q,i) * wm_interm_153_triplet_pt3(a,b,j,k) * wm_interm_185_triplet_pt3(b,j,k,i)
term(1267) = term(1267) + t2(a,p,q,i) * wm_interm_153_triplet_pt3(a,b,j,k) * wm_interm_185_triplet_pt3(b,j,i,k)
term(1268) = term(1268) + t2(a,p,j,i) * wm_interm_179_triplet_pt3(b,i,j,k) * wm_interm_193_triplet_pt3(a,b,k,q)
term(1269) = term(1269) + t2(a,p,j,i) * wm_interm_179_triplet_pt3(b,i,j,k) * wm_interm_194_triplet_pt3(a,b,k,q)
term(1270) = term(1270) + t2(a,p,j,i) * wm_interm_179_triplet_pt3(b,i,j,k) * wm_interm_190_triplet_pt3(a,b,k,q)
term(1271) = term(1271) + t2(a,p,j,i) * wm_interm_182_triplet_pt3(b,i,j,k) * wm_interm_192_triplet_pt3(a,b,k,q)
term(1272) = term(1272) + t2(a,p,j,i) * wm_interm_179_triplet_pt3(b,i,j,k) * wm_interm_192_triplet_pt3(a,b,k,q)
end do 
end do 
end do 
end do 
end do 

term(1203) = term(1203) * (2.0d+0) 
term(1204) = term(1204) * (2.0d+0) 
term(1205) = term(1205) * (-4.0d+0) 
term(1206) = term(1206) * (-4.0d+0) 
term(1207) = term(1207) * (8.0d+0) 
term(1208) = term(1208) * (-4.0d+0) 
term(1209) = term(1209) * (2.0d+0) 
term(1210) = term(1210) * (-4.0d+0) 
term(1211) = term(1211) * (2.0d+0) 
term(1212) = term(1212) * (-4.0d+0) 
term(1213) = term(1213) * (-4.0d+0) 
term(1214) = term(1214) * (8.0d+0) 
term(1215) = term(1215) * (2.0d+0) 
term(1216) = term(1216) * (2.0d+0) 
term(1217) = term(1217) * (-4.0d+0) 
term(1218) = term(1218) * (-4.0d+0) 
term(1219) = term(1219) * (-4.0d+0) 
term(1220) = term(1220) * (8.0d+0) 
term(1221) = term(1221) * (8.0d+0) 
term(1222) = term(1222) * (-8.0d+0) 
term(1223) = term(1223) * (-16.0d+0) 
term(1224) = term(1224) * (16.0d+0) 
term(1225) = term(1225) * (8.0d+0) 
term(1226) = term(1226) * (-8.0d+0) 
term(1227) = term(1227) * (-16.0d+0) 
term(1228) = term(1228) * (16.0d+0) 
term(1229) = term(1229) * (8.0d+0) 
term(1230) = term(1230) * (-8.0d+0) 
term(1231) = term(1231) * (-16.0d+0) 
term(1232) = term(1232) * (16.0d+0) 
term(1233) = term(1233) * (-4.0d+0) 
term(1234) = term(1234) * (2.0d+0) 
term(1235) = term(1235) * (2.0d+0) 
term(1236) = term(1236) * (-4.0d+0) 
term(1237) = term(1237) * (2.0d+0) 
term(1238) = term(1238) * (2.0d+0) 
term(1239) = term(1239) * (2.0d+0) 
term(1240) = term(1240) * (-4.0d+0) 
term(1241) = term(1241) * (-1.0d+0) 
term(1242) = term(1242) * (2.0d+0) 
term(1243) = term(1243) * (-1.0d+0) 
term(1244) = term(1244) * (2.0d+0) 
term(1245) = term(1245) * (-1.0d+0) 
term(1246) = term(1246) * (2.0d+0) 
term(1247) = term(1247) * (-1.0d+0) 
term(1248) = term(1248) * (2.0d+0) 
term(1249) = term(1249) * (-1.0d+0) 
term(1250) = term(1250) * (-1.0d+0) 
term(1251) = term(1251) * (-2.0d+0) 
term(1252) = term(1252) * (4.0d+0) 
term(1253) = term(1253) * (-2.0d+0) 
term(1254) = term(1254) * (-2.0d+0) 
term(1255) = term(1255) * (4.0d+0) 
term(1256) = term(1256) * (-8.0d+0) 
term(1257) = term(1257) * (8.0d+0) 
term(1258) = term(1258) * (-8.0d+0) 
term(1259) = term(1259) * (8.0d+0) 
term(1260) = term(1260) * (4.0d+0) 
term(1261) = term(1261) * (-8.0d+0) 
term(1262) = term(1262) * (-4.0d+0) 
term(1263) = term(1263) * (8.0d+0) 
term(1264) = term(1264) * (-4.0d+0) 
term(1265) = term(1265) * (4.0d+0) 
term(1266) = term(1266) * (-4.0d+0) 
term(1267) = term(1267) * (4.0d+0) 
term(1268) = term(1268) * (-4.0d+0) 
term(1269) = term(1269) * (8.0d+0) 
term(1270) = term(1270) * (-4.0d+0) 
term(1271) = term(1271) * (-4.0d+0) 
term(1272) = term(1272) * (8.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(1273) = term(1273) + wm_interm_14_triplet_pt3(a,p,i,q) * wm_interm_72_triplet_pt3(a,i)
term(1274) = term(1274) + wm_interm_10_triplet_pt3(a,p,i,q) * wm_interm_72_triplet_pt3(a,i)
term(1275) = term(1275) + wm_interm_11_triplet_pt3(a,p,i,q) * wm_interm_72_triplet_pt3(a,i)
term(1276) = term(1276) + wm_interm_39_triplet_pt3(a,p,i,q) * wm_interm_72_triplet_pt3(a,i)
term(1277) = term(1277) + wm_interm_34_triplet_pt3(a,p,i,q) * wm_interm_72_triplet_pt3(a,i)
term(1278) = term(1278) + wm_interm_35_triplet_pt3(a,p,i,q) * wm_interm_72_triplet_pt3(a,i)
term(1279) = term(1279) + wm_interm_3_triplet_pt3(a,p,i,q) * wm_interm_72_triplet_pt3(a,i)
term(1280) = term(1280) + wm_interm_0_triplet_pt3(a,p,i,q) * wm_interm_72_triplet_pt3(a,i)
term(1281) = term(1281) + wm_interm_2_triplet_pt3(a,p,i,q) * wm_interm_72_triplet_pt3(a,i)
term(1282) = term(1282) + wm_interm_3_triplet_pt3(a,p,i,q) * wm_interm_73_triplet_pt3(a,i)
term(1283) = term(1283) + wm_interm_14_triplet_pt3(a,p,i,q) * wm_interm_73_triplet_pt3(a,i)
term(1284) = term(1284) + wm_interm_0_triplet_pt3(a,p,i,q) * wm_interm_73_triplet_pt3(a,i)
term(1285) = term(1285) + wm_interm_10_triplet_pt3(a,p,i,q) * wm_interm_73_triplet_pt3(a,i)
term(1286) = term(1286) + wm_interm_2_triplet_pt3(a,p,i,q) * wm_interm_73_triplet_pt3(a,i)
term(1287) = term(1287) + wm_interm_11_triplet_pt3(a,p,i,q) * wm_interm_73_triplet_pt3(a,i)
term(1288) = term(1288) + wm_interm_39_triplet_pt3(a,p,i,q) * wm_interm_73_triplet_pt3(a,i)
term(1289) = term(1289) + wm_interm_34_triplet_pt3(a,p,i,q) * wm_interm_73_triplet_pt3(a,i)
term(1290) = term(1290) + wm_interm_35_triplet_pt3(a,p,i,q) * wm_interm_73_triplet_pt3(a,i)
term(1291) = term(1291) + wm_interm_50_triplet_pt3(a,p,i,q) * wm_interm_72_triplet_pt3(a,i)
term(1292) = term(1292) + wm_interm_51_triplet_pt3(a,p,i,q) * wm_interm_72_triplet_pt3(a,i)
term(1293) = term(1293) + wm_interm_66_triplet_pt3(a,p,i,q) * wm_interm_72_triplet_pt3(a,i)
term(1294) = term(1294) + wm_interm_67_triplet_pt3(a,p,i,q) * wm_interm_72_triplet_pt3(a,i)
term(1295) = term(1295) + wm_interm_45_triplet_pt3(a,p,i,q) * wm_interm_72_triplet_pt3(a,i)
term(1296) = term(1296) + wm_interm_46_triplet_pt3(a,p,i,q) * wm_interm_72_triplet_pt3(a,i)
term(1297) = term(1297) + wm_interm_45_triplet_pt3(a,p,i,q) * wm_interm_73_triplet_pt3(a,i)
term(1298) = term(1298) + wm_interm_50_triplet_pt3(a,p,i,q) * wm_interm_73_triplet_pt3(a,i)
term(1299) = term(1299) + wm_interm_46_triplet_pt3(a,p,i,q) * wm_interm_73_triplet_pt3(a,i)
term(1300) = term(1300) + wm_interm_51_triplet_pt3(a,p,i,q) * wm_interm_73_triplet_pt3(a,i)
term(1301) = term(1301) + wm_interm_66_triplet_pt3(a,p,i,q) * wm_interm_73_triplet_pt3(a,i)
term(1302) = term(1302) + wm_interm_67_triplet_pt3(a,p,i,q) * wm_interm_73_triplet_pt3(a,i)
term(1303) = term(1303) + wm_interm_30_triplet_pt3(a,p,i,q) * wm_interm_84_triplet_pt3(a,i)
term(1304) = term(1304) + wm_interm_24_triplet_pt3(a,p,i,q) * wm_interm_84_triplet_pt3(a,i)
term(1305) = term(1305) + wm_interm_23_triplet_pt3(a,p,i,q) * wm_interm_84_triplet_pt3(a,i)
term(1306) = term(1306) + wm_interm_26_triplet_pt3(a,p,i,q) * wm_interm_84_triplet_pt3(a,i)
term(1307) = term(1307) + wm_interm_21_triplet_pt3(a,p,i,q) * wm_interm_84_triplet_pt3(a,i)
term(1308) = term(1308) + wm_interm_22_triplet_pt3(a,p,i,q) * wm_interm_84_triplet_pt3(a,i)
term(1309) = term(1309) + wm_interm_30_triplet_pt3(a,p,i,q) * wm_interm_85_triplet_pt3(a,i)
term(1310) = term(1310) + wm_interm_24_triplet_pt3(a,p,i,q) * wm_interm_85_triplet_pt3(a,i)
term(1311) = term(1311) + wm_interm_23_triplet_pt3(a,p,i,q) * wm_interm_85_triplet_pt3(a,i)
term(1312) = term(1312) + wm_interm_26_triplet_pt3(a,p,i,q) * wm_interm_85_triplet_pt3(a,i)
term(1313) = term(1313) + wm_interm_21_triplet_pt3(a,p,i,q) * wm_interm_85_triplet_pt3(a,i)
term(1314) = term(1314) + wm_interm_22_triplet_pt3(a,p,i,q) * wm_interm_85_triplet_pt3(a,i)
term(1315) = term(1315) + wm_interm_30_triplet_pt3(a,p,i,q) * wm_interm_86_triplet_pt3(a,i)
term(1316) = term(1316) + wm_interm_24_triplet_pt3(a,p,i,q) * wm_interm_86_triplet_pt3(a,i)
term(1317) = term(1317) + wm_interm_23_triplet_pt3(a,p,i,q) * wm_interm_86_triplet_pt3(a,i)
term(1318) = term(1318) + wm_interm_26_triplet_pt3(a,p,i,q) * wm_interm_86_triplet_pt3(a,i)
term(1319) = term(1319) + wm_interm_21_triplet_pt3(a,p,i,q) * wm_interm_86_triplet_pt3(a,i)
term(1320) = term(1320) + wm_interm_22_triplet_pt3(a,p,i,q) * wm_interm_86_triplet_pt3(a,i)
term(1321) = term(1321) + wm_interm_61_triplet_pt3(a,p,i,q) * wm_interm_84_triplet_pt3(a,i)
term(1322) = term(1322) + wm_interm_60_triplet_pt3(a,p,i,q) * wm_interm_84_triplet_pt3(a,i)
term(1323) = term(1323) + wm_interm_58_triplet_pt3(a,p,i,q) * wm_interm_84_triplet_pt3(a,i)
term(1324) = term(1324) + wm_interm_59_triplet_pt3(a,p,i,q) * wm_interm_84_triplet_pt3(a,i)
term(1325) = term(1325) + wm_interm_61_triplet_pt3(a,p,i,q) * wm_interm_85_triplet_pt3(a,i)
term(1326) = term(1326) + wm_interm_60_triplet_pt3(a,p,i,q) * wm_interm_85_triplet_pt3(a,i)
term(1327) = term(1327) + wm_interm_58_triplet_pt3(a,p,i,q) * wm_interm_85_triplet_pt3(a,i)
term(1328) = term(1328) + wm_interm_59_triplet_pt3(a,p,i,q) * wm_interm_85_triplet_pt3(a,i)
term(1329) = term(1329) + wm_interm_61_triplet_pt3(a,p,i,q) * wm_interm_86_triplet_pt3(a,i)
term(1330) = term(1330) + wm_interm_60_triplet_pt3(a,p,i,q) * wm_interm_86_triplet_pt3(a,i)
term(1331) = term(1331) + wm_interm_58_triplet_pt3(a,p,i,q) * wm_interm_86_triplet_pt3(a,i)
term(1332) = term(1332) + wm_interm_59_triplet_pt3(a,p,i,q) * wm_interm_86_triplet_pt3(a,i)
term(1333) = term(1333) + wm_interm_87_triplet_pt3(a,i) * wm_interm_90_triplet_pt3(a,p,i,q)
term(1334) = term(1334) + wm_interm_87_triplet_pt3(a,i) * wm_interm_91_triplet_pt3(a,p,i,q)
term(1335) = term(1335) + wm_interm_87_triplet_pt3(a,i) * wm_interm_92_triplet_pt3(a,p,i,q)
term(1336) = term(1336) + wm_interm_88_triplet_pt3(a,i) * wm_interm_90_triplet_pt3(a,p,i,q)
term(1337) = term(1337) + wm_interm_88_triplet_pt3(a,i) * wm_interm_91_triplet_pt3(a,p,i,q)
term(1338) = term(1338) + wm_interm_88_triplet_pt3(a,i) * wm_interm_92_triplet_pt3(a,p,i,q)
term(1339) = term(1339) + wm_interm_89_triplet_pt3(a,i) * wm_interm_90_triplet_pt3(a,p,i,q)
term(1340) = term(1340) + wm_interm_89_triplet_pt3(a,i) * wm_interm_91_triplet_pt3(a,p,i,q)
term(1341) = term(1341) + wm_interm_89_triplet_pt3(a,i) * wm_interm_92_triplet_pt3(a,p,i,q)
term(1342) = term(1342) + wm_interm_87_triplet_pt3(a,i) * wm_interm_93_triplet_pt3(a,p,i,q)
term(1343) = term(1343) + wm_interm_87_triplet_pt3(a,i) * wm_interm_94_triplet_pt3(a,p,i,q)
term(1344) = term(1344) + wm_interm_87_triplet_pt3(a,i) * wm_interm_95_triplet_pt3(a,p,i,q)
term(1345) = term(1345) + wm_interm_88_triplet_pt3(a,i) * wm_interm_93_triplet_pt3(a,p,i,q)
term(1346) = term(1346) + wm_interm_88_triplet_pt3(a,i) * wm_interm_94_triplet_pt3(a,p,i,q)
term(1347) = term(1347) + wm_interm_88_triplet_pt3(a,i) * wm_interm_95_triplet_pt3(a,p,i,q)
term(1348) = term(1348) + wm_interm_89_triplet_pt3(a,i) * wm_interm_93_triplet_pt3(a,p,i,q)
term(1349) = term(1349) + wm_interm_89_triplet_pt3(a,i) * wm_interm_94_triplet_pt3(a,p,i,q)
term(1350) = term(1350) + wm_interm_89_triplet_pt3(a,i) * wm_interm_95_triplet_pt3(a,p,i,q)
term(1351) = term(1351) + wm_interm_90_triplet_pt3(a,p,i,q) * wm_interm_96_triplet_pt3(a,i)
term(1352) = term(1352) + wm_interm_91_triplet_pt3(a,p,i,q) * wm_interm_96_triplet_pt3(a,i)
term(1353) = term(1353) + wm_interm_92_triplet_pt3(a,p,i,q) * wm_interm_96_triplet_pt3(a,i)
term(1354) = term(1354) + wm_interm_90_triplet_pt3(a,p,i,q) * wm_interm_97_triplet_pt3(a,i)
term(1355) = term(1355) + wm_interm_91_triplet_pt3(a,p,i,q) * wm_interm_97_triplet_pt3(a,i)
term(1356) = term(1356) + wm_interm_92_triplet_pt3(a,p,i,q) * wm_interm_97_triplet_pt3(a,i)
term(1357) = term(1357) + wm_interm_93_triplet_pt3(a,p,i,q) * wm_interm_96_triplet_pt3(a,i)
term(1358) = term(1358) + wm_interm_94_triplet_pt3(a,p,i,q) * wm_interm_96_triplet_pt3(a,i)
term(1359) = term(1359) + wm_interm_95_triplet_pt3(a,p,i,q) * wm_interm_96_triplet_pt3(a,i)
term(1360) = term(1360) + wm_interm_93_triplet_pt3(a,p,i,q) * wm_interm_97_triplet_pt3(a,i)
term(1361) = term(1361) + wm_interm_94_triplet_pt3(a,p,i,q) * wm_interm_97_triplet_pt3(a,i)
term(1362) = term(1362) + wm_interm_95_triplet_pt3(a,p,i,q) * wm_interm_97_triplet_pt3(a,i)
term(1363) = term(1363) + wm_interm_117_triplet_pt3(a,p,i,q) * wm_interm_72_triplet_pt3(a,i)
term(1364) = term(1364) + wm_interm_113_triplet_pt3(a,p,i,q) * wm_interm_72_triplet_pt3(a,i)
term(1365) = term(1365) + wm_interm_114_triplet_pt3(a,p,i,q) * wm_interm_72_triplet_pt3(a,i)
term(1366) = term(1366) + wm_interm_109_triplet_pt3(a,p,i,q) * wm_interm_72_triplet_pt3(a,i)
term(1367) = term(1367) + wm_interm_107_triplet_pt3(a,p,i,q) * wm_interm_72_triplet_pt3(a,i)
term(1368) = term(1368) + wm_interm_108_triplet_pt3(a,p,i,q) * wm_interm_72_triplet_pt3(a,i)
term(1369) = term(1369) + wm_interm_109_triplet_pt3(a,p,i,q) * wm_interm_73_triplet_pt3(a,i)
term(1370) = term(1370) + wm_interm_117_triplet_pt3(a,p,i,q) * wm_interm_73_triplet_pt3(a,i)
term(1371) = term(1371) + wm_interm_107_triplet_pt3(a,p,i,q) * wm_interm_73_triplet_pt3(a,i)
term(1372) = term(1372) + wm_interm_113_triplet_pt3(a,p,i,q) * wm_interm_73_triplet_pt3(a,i)
term(1373) = term(1373) + wm_interm_108_triplet_pt3(a,p,i,q) * wm_interm_73_triplet_pt3(a,i)
term(1374) = term(1374) + wm_interm_114_triplet_pt3(a,p,i,q) * wm_interm_73_triplet_pt3(a,i)
term(1375) = term(1375) + wm_interm_134_triplet_pt3(a,p,i,q) * wm_interm_72_triplet_pt3(a,i)
term(1376) = term(1376) + wm_interm_135_triplet_pt3(a,p,i,q) * wm_interm_72_triplet_pt3(a,i)
term(1377) = term(1377) + wm_interm_131_triplet_pt3(a,p,i,q) * wm_interm_72_triplet_pt3(a,i)
term(1378) = term(1378) + wm_interm_132_triplet_pt3(a,p,i,q) * wm_interm_72_triplet_pt3(a,i)
term(1379) = term(1379) + wm_interm_131_triplet_pt3(a,p,i,q) * wm_interm_73_triplet_pt3(a,i)
term(1380) = term(1380) + wm_interm_134_triplet_pt3(a,p,i,q) * wm_interm_73_triplet_pt3(a,i)
term(1381) = term(1381) + wm_interm_132_triplet_pt3(a,p,i,q) * wm_interm_73_triplet_pt3(a,i)
term(1382) = term(1382) + wm_interm_135_triplet_pt3(a,p,i,q) * wm_interm_73_triplet_pt3(a,i)
term(1383) = term(1383) + wm_interm_148_triplet_pt3(a,i) * wm_interm_30_triplet_pt3(a,p,i,q)
term(1384) = term(1384) + wm_interm_148_triplet_pt3(a,i) * wm_interm_24_triplet_pt3(a,p,i,q)
term(1385) = term(1385) + wm_interm_148_triplet_pt3(a,i) * wm_interm_23_triplet_pt3(a,p,i,q)
term(1386) = term(1386) + wm_interm_148_triplet_pt3(a,i) * wm_interm_26_triplet_pt3(a,p,i,q)
term(1387) = term(1387) + wm_interm_148_triplet_pt3(a,i) * wm_interm_21_triplet_pt3(a,p,i,q)
term(1388) = term(1388) + wm_interm_148_triplet_pt3(a,i) * wm_interm_22_triplet_pt3(a,p,i,q)
term(1389) = term(1389) + wm_interm_149_triplet_pt3(a,i) * wm_interm_30_triplet_pt3(a,p,i,q)
term(1390) = term(1390) + wm_interm_149_triplet_pt3(a,i) * wm_interm_24_triplet_pt3(a,p,i,q)
term(1391) = term(1391) + wm_interm_149_triplet_pt3(a,i) * wm_interm_23_triplet_pt3(a,p,i,q)
term(1392) = term(1392) + wm_interm_149_triplet_pt3(a,i) * wm_interm_26_triplet_pt3(a,p,i,q)
term(1393) = term(1393) + wm_interm_149_triplet_pt3(a,i) * wm_interm_21_triplet_pt3(a,p,i,q)
term(1394) = term(1394) + wm_interm_149_triplet_pt3(a,i) * wm_interm_22_triplet_pt3(a,p,i,q)
term(1395) = term(1395) + wm_interm_148_triplet_pt3(a,i) * wm_interm_61_triplet_pt3(a,p,i,q)
term(1396) = term(1396) + wm_interm_148_triplet_pt3(a,i) * wm_interm_60_triplet_pt3(a,p,i,q)
term(1397) = term(1397) + wm_interm_148_triplet_pt3(a,i) * wm_interm_58_triplet_pt3(a,p,i,q)
term(1398) = term(1398) + wm_interm_148_triplet_pt3(a,i) * wm_interm_59_triplet_pt3(a,p,i,q)
term(1399) = term(1399) + wm_interm_149_triplet_pt3(a,i) * wm_interm_61_triplet_pt3(a,p,i,q)
term(1400) = term(1400) + wm_interm_149_triplet_pt3(a,i) * wm_interm_60_triplet_pt3(a,p,i,q)
term(1401) = term(1401) + wm_interm_149_triplet_pt3(a,i) * wm_interm_58_triplet_pt3(a,p,i,q)
term(1402) = term(1402) + wm_interm_149_triplet_pt3(a,i) * wm_interm_59_triplet_pt3(a,p,i,q)
term(1403) = term(1403) + wm_interm_150_triplet_pt3(a,p,i,q) * wm_interm_87_triplet_pt3(a,i)
term(1404) = term(1404) + wm_interm_151_triplet_pt3(a,p,i,q) * wm_interm_87_triplet_pt3(a,i)
term(1405) = term(1405) + wm_interm_150_triplet_pt3(a,p,i,q) * wm_interm_88_triplet_pt3(a,i)
term(1406) = term(1406) + wm_interm_151_triplet_pt3(a,p,i,q) * wm_interm_88_triplet_pt3(a,i)
term(1407) = term(1407) + wm_interm_150_triplet_pt3(a,p,i,q) * wm_interm_89_triplet_pt3(a,i)
term(1408) = term(1408) + wm_interm_151_triplet_pt3(a,p,i,q) * wm_interm_89_triplet_pt3(a,i)
term(1409) = term(1409) + wm_interm_152_triplet_pt3(a,p,i,q) * wm_interm_87_triplet_pt3(a,i)
term(1410) = term(1410) + wm_interm_153_triplet_pt3(a,p,i,q) * wm_interm_87_triplet_pt3(a,i)
term(1411) = term(1411) + wm_interm_152_triplet_pt3(a,p,i,q) * wm_interm_88_triplet_pt3(a,i)
term(1412) = term(1412) + wm_interm_153_triplet_pt3(a,p,i,q) * wm_interm_88_triplet_pt3(a,i)
term(1413) = term(1413) + wm_interm_152_triplet_pt3(a,p,i,q) * wm_interm_89_triplet_pt3(a,i)
term(1414) = term(1414) + wm_interm_153_triplet_pt3(a,p,i,q) * wm_interm_89_triplet_pt3(a,i)
term(1415) = term(1415) + wm_interm_150_triplet_pt3(a,p,i,q) * wm_interm_96_triplet_pt3(a,i)
term(1416) = term(1416) + wm_interm_151_triplet_pt3(a,p,i,q) * wm_interm_96_triplet_pt3(a,i)
term(1417) = term(1417) + wm_interm_150_triplet_pt3(a,p,i,q) * wm_interm_97_triplet_pt3(a,i)
term(1418) = term(1418) + wm_interm_151_triplet_pt3(a,p,i,q) * wm_interm_97_triplet_pt3(a,i)
term(1419) = term(1419) + wm_interm_152_triplet_pt3(a,p,i,q) * wm_interm_96_triplet_pt3(a,i)
term(1420) = term(1420) + wm_interm_153_triplet_pt3(a,p,i,q) * wm_interm_96_triplet_pt3(a,i)
term(1421) = term(1421) + wm_interm_152_triplet_pt3(a,p,i,q) * wm_interm_97_triplet_pt3(a,i)
term(1422) = term(1422) + wm_interm_153_triplet_pt3(a,p,i,q) * wm_interm_97_triplet_pt3(a,i)
term(1423) = term(1423) + r1(vrdav_Rl, a,i) * wm_interm_196_triplet_pt3(q,i) * wm_interm_9_triplet_pt3(a,p)
term(1424) = term(1424) + r1(vrdav_Rl, a,i) * wm_interm_197_triplet_pt3(q,i) * wm_interm_9_triplet_pt3(a,p)
term(1425) = term(1425) + r1(vrdav_Rl, a,i) * wm_interm_196_triplet_pt3(q,i) * wm_interm_5_triplet_pt3(a,p)
term(1426) = term(1426) + r1(vrdav_Rl, a,i) * wm_interm_197_triplet_pt3(q,i) * wm_interm_5_triplet_pt3(a,p)
term(1427) = term(1427) + r1(vrdav_Rl, a,i) * wm_interm_196_triplet_pt3(q,i) * wm_interm_6_triplet_pt3(a,p)
term(1428) = term(1428) + r1(vrdav_Rl, a,i) * wm_interm_197_triplet_pt3(q,i) * wm_interm_6_triplet_pt3(a,p)
term(1429) = term(1429) + r1(vrdav_Rl, a,i) * wm_interm_196_triplet_pt3(q,i) * wm_interm_47_triplet_pt3(a,p)
term(1430) = term(1430) + r1(vrdav_Rl, a,i) * wm_interm_197_triplet_pt3(q,i) * wm_interm_47_triplet_pt3(a,p)
term(1431) = term(1431) + r1(vrdav_Rl, a,i) * wm_interm_196_triplet_pt3(q,i) * wm_interm_48_triplet_pt3(a,p)
term(1432) = term(1432) + r1(vrdav_Rl, a,i) * wm_interm_197_triplet_pt3(q,i) * wm_interm_48_triplet_pt3(a,p)
term(1433) = term(1433) + r1(vrdav_Rr, a,i) * wm_interm_198_triplet_pt3(a,p) * wm_interm_75_triplet_pt3(i,q)
term(1434) = term(1434) + r1(vrdav_Rr, a,i) * wm_interm_198_triplet_pt3(a,p) * wm_interm_81_triplet_pt3(i,q)
term(1435) = term(1435) + r1(vrdav_Rr, a,i) * wm_interm_198_triplet_pt3(a,p) * wm_interm_79_triplet_pt3(i,q)
term(1436) = term(1436) + r1(vrdav_Rr, a,i) * wm_interm_199_triplet_pt3(a,p) * wm_interm_75_triplet_pt3(i,q)
term(1437) = term(1437) + r1(vrdav_Rr, a,i) * wm_interm_199_triplet_pt3(a,p) * wm_interm_81_triplet_pt3(i,q)
term(1438) = term(1438) + r1(vrdav_Rr, a,i) * wm_interm_199_triplet_pt3(a,p) * wm_interm_79_triplet_pt3(i,q)
term(1439) = term(1439) + r1(vrdav_Rr, a,i) * wm_interm_196_triplet_pt3(i,q) * wm_interm_80_triplet_pt3(a,p)
term(1440) = term(1440) + r1(vrdav_Rr, a,i) * wm_interm_196_triplet_pt3(i,q) * wm_interm_74_triplet_pt3(a,p)
term(1441) = term(1441) + r1(vrdav_Rr, a,i) * wm_interm_196_triplet_pt3(i,q) * wm_interm_78_triplet_pt3(a,p)
term(1442) = term(1442) + r1(vrdav_Rr, a,i) * wm_interm_197_triplet_pt3(i,q) * wm_interm_80_triplet_pt3(a,p)
term(1443) = term(1443) + r1(vrdav_Rr, a,i) * wm_interm_197_triplet_pt3(i,q) * wm_interm_74_triplet_pt3(a,p)
term(1444) = term(1444) + r1(vrdav_Rr, a,i) * wm_interm_197_triplet_pt3(i,q) * wm_interm_78_triplet_pt3(a,p)
term(1445) = term(1445) + r1(vrdav_Rr, a,i) * wm_interm_145_triplet_pt3(i,q) * wm_interm_198_triplet_pt3(a,p)
term(1446) = term(1446) + r1(vrdav_Rr, a,i) * wm_interm_147_triplet_pt3(i,q) * wm_interm_198_triplet_pt3(a,p)
term(1447) = term(1447) + r1(vrdav_Rr, a,i) * wm_interm_145_triplet_pt3(i,q) * wm_interm_199_triplet_pt3(a,p)
term(1448) = term(1448) + r1(vrdav_Rr, a,i) * wm_interm_147_triplet_pt3(i,q) * wm_interm_199_triplet_pt3(a,p)
term(1449) = term(1449) + r1(vrdav_Rr, a,i) * wm_interm_144_triplet_pt3(a,p) * wm_interm_196_triplet_pt3(i,q)
term(1450) = term(1450) + r1(vrdav_Rr, a,i) * wm_interm_146_triplet_pt3(a,p) * wm_interm_196_triplet_pt3(i,q)
term(1451) = term(1451) + r1(vrdav_Rr, a,i) * wm_interm_144_triplet_pt3(a,p) * wm_interm_197_triplet_pt3(i,q)
term(1452) = term(1452) + r1(vrdav_Rr, a,i) * wm_interm_146_triplet_pt3(a,p) * wm_interm_197_triplet_pt3(i,q)
end do 
end do 

term(1273) = term(1273) * (-2.0d+0) 
term(1274) = term(1274) * (-2.0d+0) 
term(1275) = term(1275) * (4.0d+0) 
term(1278) = term(1278) * (-2.0d+0) 
term(1281) = term(1281) * (-2.0d+0) 
term(1282) = term(1282) * (-0.5d+0) 
term(1284) = term(1284) * (-0.5d+0) 
term(1287) = term(1287) * (-2.0d+0) 
term(1288) = term(1288) * (-0.5d+0) 
term(1289) = term(1289) * (-0.5d+0) 
term(1291) = term(1291) * (-8.0d+0) 
term(1292) = term(1292) * (8.0d+0) 
term(1293) = term(1293) * (4.0d+0) 
term(1294) = term(1294) * (-4.0d+0) 
term(1295) = term(1295) * (4.0d+0) 
term(1296) = term(1296) * (-4.0d+0) 
term(1297) = term(1297) * (-2.0d+0) 
term(1298) = term(1298) * (4.0d+0) 
term(1299) = term(1299) * (2.0d+0) 
term(1300) = term(1300) * (-4.0d+0) 
term(1301) = term(1301) * (-2.0d+0) 
term(1302) = term(1302) * (2.0d+0) 
term(1305) = term(1305) * (-2.0d+0) 
term(1306) = term(1306) * (-2.0d+0) 
term(1307) = term(1307) * (-2.0d+0) 
term(1308) = term(1308) * (4.0d+0) 
term(1309) = term(1309) * (-0.5d+0) 
term(1310) = term(1310) * (-0.5d+0) 
term(1314) = term(1314) * (-2.0d+0) 
term(1315) = term(1315) * (-0.5d+0) 
term(1316) = term(1316) * (-0.5d+0) 
term(1320) = term(1320) * (-2.0d+0) 
term(1321) = term(1321) * (4.0d+0) 
term(1322) = term(1322) * (-4.0d+0) 
term(1323) = term(1323) * (-8.0d+0) 
term(1324) = term(1324) * (8.0d+0) 
term(1325) = term(1325) * (-2.0d+0) 
term(1326) = term(1326) * (2.0d+0) 
term(1327) = term(1327) * (4.0d+0) 
term(1328) = term(1328) * (-4.0d+0) 
term(1329) = term(1329) * (-2.0d+0) 
term(1330) = term(1330) * (2.0d+0) 
term(1331) = term(1331) * (4.0d+0) 
term(1332) = term(1332) * (-4.0d+0) 
term(1333) = term(1333) * (2.0d+0) 
term(1334) = term(1334) * (2.0d+0) 
term(1335) = term(1335) * (-4.0d+0) 
term(1336) = term(1336) * (-1.0d+0) 
term(1337) = term(1337) * (-1.0d+0) 
term(1338) = term(1338) * (2.0d+0) 
term(1339) = term(1339) * (-1.0d+0) 
term(1340) = term(1340) * (-1.0d+0) 
term(1341) = term(1341) * (2.0d+0) 
term(1342) = term(1342) * (-1.0d+0) 
term(1343) = term(1343) * (-1.0d+0) 
term(1344) = term(1344) * (2.0d+0) 
term(1345) = term(1345) * (0.5d+0) 
term(1346) = term(1346) * (0.5d+0) 
term(1347) = term(1347) * (-1.0d+0) 
term(1348) = term(1348) * (0.5d+0) 
term(1349) = term(1349) * (0.5d+0) 
term(1350) = term(1350) * (-1.0d+0) 
term(1351) = term(1351) * (4.0d+0) 
term(1352) = term(1352) * (4.0d+0) 
term(1353) = term(1353) * (-8.0d+0) 
term(1354) = term(1354) * (-4.0d+0) 
term(1355) = term(1355) * (-4.0d+0) 
term(1356) = term(1356) * (8.0d+0) 
term(1357) = term(1357) * (-2.0d+0) 
term(1358) = term(1358) * (-2.0d+0) 
term(1359) = term(1359) * (4.0d+0) 
term(1360) = term(1360) * (2.0d+0) 
term(1361) = term(1361) * (2.0d+0) 
term(1362) = term(1362) * (-4.0d+0) 
term(1363) = term(1363) * (-4.0d+0) 
term(1364) = term(1364) * (-4.0d+0) 
term(1365) = term(1365) * (8.0d+0) 
term(1366) = term(1366) * (4.0d+0) 
term(1367) = term(1367) * (4.0d+0) 
term(1368) = term(1368) * (-8.0d+0) 
term(1369) = term(1369) * (-2.0d+0) 
term(1370) = term(1370) * (2.0d+0) 
term(1371) = term(1371) * (-2.0d+0) 
term(1372) = term(1372) * (2.0d+0) 
term(1373) = term(1373) * (4.0d+0) 
term(1374) = term(1374) * (-4.0d+0) 
term(1375) = term(1375) * (-16.0d+0) 
term(1376) = term(1376) * (16.0d+0) 
term(1377) = term(1377) * (16.0d+0) 
term(1378) = term(1378) * (-16.0d+0) 
term(1379) = term(1379) * (-8.0d+0) 
term(1380) = term(1380) * (8.0d+0) 
term(1381) = term(1381) * (8.0d+0) 
term(1382) = term(1382) * (-8.0d+0) 
term(1383) = term(1383) * (2.0d+0) 
term(1384) = term(1384) * (2.0d+0) 
term(1385) = term(1385) * (-4.0d+0) 
term(1386) = term(1386) * (-4.0d+0) 
term(1387) = term(1387) * (-4.0d+0) 
term(1388) = term(1388) * (8.0d+0) 
term(1389) = term(1389) * (-2.0d+0) 
term(1390) = term(1390) * (-2.0d+0) 
term(1391) = term(1391) * (4.0d+0) 
term(1392) = term(1392) * (4.0d+0) 
term(1393) = term(1393) * (4.0d+0) 
term(1394) = term(1394) * (-8.0d+0) 
term(1395) = term(1395) * (8.0d+0) 
term(1396) = term(1396) * (-8.0d+0) 
term(1397) = term(1397) * (-16.0d+0) 
term(1398) = term(1398) * (16.0d+0) 
term(1399) = term(1399) * (-8.0d+0) 
term(1400) = term(1400) * (8.0d+0) 
term(1401) = term(1401) * (16.0d+0) 
term(1402) = term(1402) * (-16.0d+0) 
term(1403) = term(1403) * (8.0d+0) 
term(1404) = term(1404) * (-8.0d+0) 
term(1405) = term(1405) * (-4.0d+0) 
term(1406) = term(1406) * (4.0d+0) 
term(1407) = term(1407) * (-4.0d+0) 
term(1408) = term(1408) * (4.0d+0) 
term(1409) = term(1409) * (-4.0d+0) 
term(1410) = term(1410) * (4.0d+0) 
term(1411) = term(1411) * (2.0d+0) 
term(1412) = term(1412) * (-2.0d+0) 
term(1413) = term(1413) * (2.0d+0) 
term(1414) = term(1414) * (-2.0d+0) 
term(1415) = term(1415) * (16.0d+0) 
term(1416) = term(1416) * (-16.0d+0) 
term(1417) = term(1417) * (-16.0d+0) 
term(1418) = term(1418) * (16.0d+0) 
term(1419) = term(1419) * (-8.0d+0) 
term(1420) = term(1420) * (8.0d+0) 
term(1421) = term(1421) * (8.0d+0) 
term(1422) = term(1422) * (-8.0d+0) 
term(1423) = term(1423) * (-1.0d+0) 
term(1424) = term(1424) * (2.0d+0) 
term(1425) = term(1425) * (-1.0d+0) 
term(1426) = term(1426) * (2.0d+0) 
term(1427) = term(1427) * (2.0d+0) 
term(1428) = term(1428) * (-4.0d+0) 
term(1429) = term(1429) * (-4.0d+0) 
term(1430) = term(1430) * (8.0d+0) 
term(1431) = term(1431) * (4.0d+0) 
term(1432) = term(1432) * (-8.0d+0) 
term(1433) = term(1433) * (-2.0d+0) 
term(1434) = term(1434) * (-2.0d+0) 
term(1435) = term(1435) * (4.0d+0) 
term(1436) = term(1436) * (4.0d+0) 
term(1437) = term(1437) * (4.0d+0) 
term(1438) = term(1438) * (-8.0d+0) 
term(1439) = term(1439) * (-2.0d+0) 
term(1440) = term(1440) * (-2.0d+0) 
term(1441) = term(1441) * (4.0d+0) 
term(1442) = term(1442) * (4.0d+0) 
term(1443) = term(1443) * (4.0d+0) 
term(1444) = term(1444) * (-8.0d+0) 
term(1445) = term(1445) * (-8.0d+0) 
term(1446) = term(1446) * (8.0d+0) 
term(1447) = term(1447) * (16.0d+0) 
term(1448) = term(1448) * (-16.0d+0) 
term(1449) = term(1449) * (-8.0d+0) 
term(1450) = term(1450) * (8.0d+0) 
term(1451) = term(1451) * (16.0d+0) 
term(1452) = term(1452) * (-16.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(1453) = term(1453) + r2p(vrdav_Rl, a,q,p,i) * t1(b,i) * wm_interm_5_triplet_pt3(a,b)
term(1454) = term(1454) + r2p(vrdav_Rl, a,q,p,i) * t1(b,i) * wm_interm_6_triplet_pt3(a,b)
term(1455) = term(1455) + r2p(vrdav_Rl, a,q,p,i) * t1(b,i) * wm_interm_9_triplet_pt3(a,b)
term(1456) = term(1456) + r2p(vrdav_Rl, a,q,p,i) * t1(b,i) * wm_interm_47_triplet_pt3(a,b)
term(1457) = term(1457) + r2p(vrdav_Rl, a,q,p,i) * t1(b,i) * wm_interm_48_triplet_pt3(a,b)
term(1458) = term(1458) + r2p(vrdav_Rr, a,q,p,i) * s1(b,i) * wm_interm_74_triplet_pt3(a,b)
term(1459) = term(1459) + r2p(vrdav_Rr, a,q,p,i) * s1(b,i) * wm_interm_78_triplet_pt3(a,b)
term(1460) = term(1460) + r2p(vrdav_Rr, a,q,p,i) * s1(b,i) * wm_interm_80_triplet_pt3(a,b)
term(1461) = term(1461) + r2m(vrdav_Rr, a,q,p,i) * s1(b,i) * wm_interm_74_triplet_pt3(a,b)
term(1462) = term(1462) + r2m(vrdav_Rr, a,q,p,i) * s1(b,i) * wm_interm_78_triplet_pt3(a,b)
term(1463) = term(1463) + r2m(vrdav_Rr, a,q,p,i) * s1(b,i) * wm_interm_80_triplet_pt3(a,b)
term(1464) = term(1464) + s1(b,i) * t2(a,p,q,i) * wm_interm_43_triplet_pt3(a,b)
term(1465) = term(1465) + s1(b,i) * t2(a,p,q,i) * wm_interm_44_triplet_pt3(a,b)
term(1466) = term(1466) + s1(b,i) * t2(a,p,q,i) * wm_interm_29_triplet_pt3(a,b)
term(1467) = term(1467) + s1(b,i) * t2(a,p,q,i) * wm_interm_27_triplet_pt3(a,b)
term(1468) = term(1468) + s1(b,i) * t2(a,p,q,i) * wm_interm_28_triplet_pt3(a,b)
term(1469) = term(1469) + s1(b,i) * t2(a,p,q,i) * wm_interm_42_triplet_pt3(a,b)
term(1470) = term(1470) + s1(b,i) * t2(a,p,q,i) * wm_interm_70_triplet_pt3(a,b)
term(1471) = term(1471) + s1(b,i) * t2(a,p,q,i) * wm_interm_71_triplet_pt3(a,b)
term(1472) = term(1472) + s1(b,i) * t2(a,p,q,i) * wm_interm_62_triplet_pt3(a,b)
term(1473) = term(1473) + s1(b,i) * t2(a,p,q,i) * wm_interm_63_triplet_pt3(a,b)
term(1474) = term(1474) + r2m(vrdav_Rl, a,q,p,i) * t1(b,i) * wm_interm_5_triplet_pt3(a,b)
term(1475) = term(1475) + r2m(vrdav_Rl, a,q,p,i) * t1(b,i) * wm_interm_6_triplet_pt3(a,b)
term(1476) = term(1476) + r2m(vrdav_Rl, a,q,p,i) * t1(b,i) * wm_interm_9_triplet_pt3(a,b)
term(1477) = term(1477) + r2m(vrdav_Rl, a,q,p,i) * t1(b,i) * wm_interm_47_triplet_pt3(a,b)
term(1478) = term(1478) + r2m(vrdav_Rl, a,q,p,i) * t1(b,i) * wm_interm_48_triplet_pt3(a,b)
term(1479) = term(1479) + r2p(vrdav_Rr, a,q,p,i) * s1(b,i) * wm_interm_144_triplet_pt3(a,b)
term(1480) = term(1480) + r2p(vrdav_Rr, a,q,p,i) * s1(b,i) * wm_interm_146_triplet_pt3(a,b)
term(1481) = term(1481) + r2m(vrdav_Rr, a,q,p,i) * s1(b,i) * wm_interm_144_triplet_pt3(a,b)
term(1482) = term(1482) + r2m(vrdav_Rr, a,q,p,i) * s1(b,i) * wm_interm_146_triplet_pt3(a,b)
term(1483) = term(1483) + s1(b,i) * t2(a,p,q,i) * wm_interm_129_triplet_pt3(a,b)
term(1484) = term(1484) + s1(b,i) * t2(a,p,q,i) * wm_interm_130_triplet_pt3(a,b)
term(1485) = term(1485) + s1(b,i) * t2(a,p,q,i) * wm_interm_122_triplet_pt3(a,b)
term(1486) = term(1486) + s1(b,i) * t2(a,p,q,i) * wm_interm_120_triplet_pt3(a,b)
term(1487) = term(1487) + s1(b,i) * t2(a,p,q,i) * wm_interm_121_triplet_pt3(a,b)
term(1488) = term(1488) + s1(b,i) * t2(a,p,q,i) * wm_interm_128_triplet_pt3(a,b)
term(1489) = term(1489) + s1(b,i) * t2(a,p,q,i) * wm_interm_142_triplet_pt3(a,b)
term(1490) = term(1490) + s1(b,i) * t2(a,p,q,i) * wm_interm_143_triplet_pt3(a,b)
term(1491) = term(1491) + s1(b,i) * t2(a,p,q,i) * wm_interm_138_triplet_pt3(a,b)
term(1492) = term(1492) + s1(b,i) * t2(a,p,q,i) * wm_interm_139_triplet_pt3(a,b)
term(1493) = term(1493) + r1(vrdav_Rl, a,i) * wm_interm_192_triplet_pt3(p,b,q,i) * wm_interm_5_triplet_pt3(a,b)
term(1494) = term(1494) + r1(vrdav_Rl, a,i) * wm_interm_192_triplet_pt3(p,b,q,i) * wm_interm_6_triplet_pt3(a,b)
term(1495) = term(1495) + r1(vrdav_Rl, a,i) * wm_interm_190_triplet_pt3(p,b,q,i) * wm_interm_5_triplet_pt3(a,b)
term(1496) = term(1496) + r1(vrdav_Rl, a,i) * wm_interm_190_triplet_pt3(p,b,q,i) * wm_interm_6_triplet_pt3(a,b)
term(1497) = term(1497) + r1(vrdav_Rl, a,i) * wm_interm_192_triplet_pt3(p,b,q,i) * wm_interm_9_triplet_pt3(a,b)
term(1498) = term(1498) + r1(vrdav_Rl, a,i) * wm_interm_190_triplet_pt3(p,b,q,i) * wm_interm_9_triplet_pt3(a,b)
term(1499) = term(1499) + r1(vrdav_Rl, a,i) * wm_interm_193_triplet_pt3(p,b,q,i) * wm_interm_5_triplet_pt3(a,b)
term(1500) = term(1500) + r1(vrdav_Rl, a,i) * wm_interm_193_triplet_pt3(p,b,q,i) * wm_interm_6_triplet_pt3(a,b)
term(1501) = term(1501) + r1(vrdav_Rl, a,i) * wm_interm_194_triplet_pt3(p,b,q,i) * wm_interm_5_triplet_pt3(a,b)
term(1502) = term(1502) + r1(vrdav_Rl, a,i) * wm_interm_194_triplet_pt3(p,b,q,i) * wm_interm_6_triplet_pt3(a,b)
term(1503) = term(1503) + r1(vrdav_Rl, a,i) * wm_interm_193_triplet_pt3(p,b,q,i) * wm_interm_9_triplet_pt3(a,b)
term(1504) = term(1504) + r1(vrdav_Rl, a,i) * wm_interm_194_triplet_pt3(p,b,q,i) * wm_interm_9_triplet_pt3(a,b)
term(1505) = term(1505) + s2(a,p,q,i) * wm_interm_186_triplet_pt3(b,i) * wm_interm_9_triplet_pt3(a,b)
term(1506) = term(1506) + s2(a,p,q,i) * wm_interm_186_triplet_pt3(b,i) * wm_interm_5_triplet_pt3(a,b)
term(1507) = term(1507) + s2(a,p,q,i) * wm_interm_186_triplet_pt3(b,i) * wm_interm_6_triplet_pt3(a,b)
term(1508) = term(1508) + r1(vrdav_Rl, a,i) * wm_interm_192_triplet_pt3(p,b,q,i) * wm_interm_47_triplet_pt3(a,b)
term(1509) = term(1509) + r1(vrdav_Rl, a,i) * wm_interm_192_triplet_pt3(p,b,q,i) * wm_interm_48_triplet_pt3(a,b)
term(1510) = term(1510) + r1(vrdav_Rl, a,i) * wm_interm_190_triplet_pt3(p,b,q,i) * wm_interm_47_triplet_pt3(a,b)
term(1511) = term(1511) + r1(vrdav_Rl, a,i) * wm_interm_190_triplet_pt3(p,b,q,i) * wm_interm_48_triplet_pt3(a,b)
term(1512) = term(1512) + r1(vrdav_Rl, a,i) * wm_interm_193_triplet_pt3(p,b,q,i) * wm_interm_47_triplet_pt3(a,b)
term(1513) = term(1513) + r1(vrdav_Rl, a,i) * wm_interm_193_triplet_pt3(p,b,q,i) * wm_interm_48_triplet_pt3(a,b)
term(1514) = term(1514) + r1(vrdav_Rl, a,i) * wm_interm_194_triplet_pt3(p,b,q,i) * wm_interm_47_triplet_pt3(a,b)
term(1515) = term(1515) + r1(vrdav_Rl, a,i) * wm_interm_194_triplet_pt3(p,b,q,i) * wm_interm_48_triplet_pt3(a,b)
term(1516) = term(1516) + s2(a,p,q,i) * wm_interm_186_triplet_pt3(b,i) * wm_interm_195_triplet_pt3(a,b)
term(1517) = term(1517) + s2(a,p,q,i) * wm_interm_186_triplet_pt3(b,i) * wm_interm_47_triplet_pt3(a,b)
term(1518) = term(1518) + s2(a,p,q,i) * wm_interm_186_triplet_pt3(b,i) * wm_interm_48_triplet_pt3(a,b)
term(1519) = term(1519) + r1(vrdav_Rr, p,i) * wm_interm_198_triplet_pt3(a,b) * wm_interm_90_triplet_pt3(b,a,i,q)
term(1520) = term(1520) + r1(vrdav_Rr, p,i) * wm_interm_199_triplet_pt3(a,b) * wm_interm_90_triplet_pt3(b,a,i,q)
term(1521) = term(1521) + r1(vrdav_Rr, p,i) * wm_interm_104_triplet_pt3(a,b,i,q) * wm_interm_198_triplet_pt3(b,a)
term(1522) = term(1522) + r1(vrdav_Rr, p,i) * wm_interm_104_triplet_pt3(a,b,i,q) * wm_interm_199_triplet_pt3(b,a)
term(1523) = term(1523) + r1(vrdav_Rr, p,i) * wm_interm_198_triplet_pt3(a,b) * wm_interm_93_triplet_pt3(b,a,i,q)
term(1524) = term(1524) + r1(vrdav_Rr, p,i) * wm_interm_199_triplet_pt3(a,b) * wm_interm_93_triplet_pt3(b,a,i,q)
term(1525) = term(1525) + r1(vrdav_Rr, p,i) * wm_interm_198_triplet_pt3(a,b) * wm_interm_91_triplet_pt3(b,a,i,q)
term(1526) = term(1526) + r1(vrdav_Rr, p,i) * wm_interm_198_triplet_pt3(a,b) * wm_interm_92_triplet_pt3(b,a,i,q)
term(1527) = term(1527) + r1(vrdav_Rr, p,i) * wm_interm_199_triplet_pt3(a,b) * wm_interm_91_triplet_pt3(b,a,i,q)
term(1528) = term(1528) + r1(vrdav_Rr, p,i) * wm_interm_199_triplet_pt3(a,b) * wm_interm_92_triplet_pt3(b,a,i,q)
term(1529) = term(1529) + r1(vrdav_Rr, p,i) * wm_interm_198_triplet_pt3(a,b) * wm_interm_95_triplet_pt3(b,a,i,q)
term(1530) = term(1530) + r1(vrdav_Rr, p,i) * wm_interm_199_triplet_pt3(a,b) * wm_interm_95_triplet_pt3(b,a,i,q)
term(1531) = term(1531) + r1(vrdav_Rr, p,i) * wm_interm_190_triplet_pt3(a,b,i,q) * wm_interm_74_triplet_pt3(b,a)
term(1532) = term(1532) + r1(vrdav_Rr, p,i) * wm_interm_193_triplet_pt3(a,b,i,q) * wm_interm_74_triplet_pt3(b,a)
term(1533) = term(1533) + r1(vrdav_Rr, p,i) * wm_interm_194_triplet_pt3(a,b,i,q) * wm_interm_74_triplet_pt3(b,a)
term(1534) = term(1534) + r1(vrdav_Rr, p,i) * wm_interm_190_triplet_pt3(a,b,i,q) * wm_interm_78_triplet_pt3(b,a)
term(1535) = term(1535) + r1(vrdav_Rr, p,i) * wm_interm_193_triplet_pt3(a,b,i,q) * wm_interm_78_triplet_pt3(b,a)
term(1536) = term(1536) + r1(vrdav_Rr, p,i) * wm_interm_194_triplet_pt3(a,b,i,q) * wm_interm_78_triplet_pt3(b,a)
term(1537) = term(1537) + r1(vrdav_Rr, p,i) * wm_interm_190_triplet_pt3(a,b,i,q) * wm_interm_80_triplet_pt3(b,a)
term(1538) = term(1538) + r1(vrdav_Rr, p,i) * wm_interm_193_triplet_pt3(a,b,i,q) * wm_interm_80_triplet_pt3(b,a)
term(1539) = term(1539) + r1(vrdav_Rr, p,i) * wm_interm_194_triplet_pt3(a,b,i,q) * wm_interm_80_triplet_pt3(b,a)
term(1540) = term(1540) + r1(vrdav_Rr, a,i) * wm_interm_198_triplet_pt3(a,b) * wm_interm_90_triplet_pt3(b,p,i,q)
term(1541) = term(1541) + r1(vrdav_Rr, a,i) * wm_interm_199_triplet_pt3(a,b) * wm_interm_90_triplet_pt3(b,p,i,q)
term(1542) = term(1542) + r1(vrdav_Rr, a,i) * wm_interm_198_triplet_pt3(a,b) * wm_interm_91_triplet_pt3(b,p,i,q)
term(1543) = term(1543) + r1(vrdav_Rr, a,i) * wm_interm_198_triplet_pt3(a,b) * wm_interm_92_triplet_pt3(b,p,i,q)
term(1544) = term(1544) + r1(vrdav_Rr, a,i) * wm_interm_199_triplet_pt3(a,b) * wm_interm_91_triplet_pt3(b,p,i,q)
term(1545) = term(1545) + r1(vrdav_Rr, a,i) * wm_interm_199_triplet_pt3(a,b) * wm_interm_92_triplet_pt3(b,p,i,q)
term(1546) = term(1546) + r1(vrdav_Rr, a,i) * wm_interm_193_triplet_pt3(b,p,i,q) * wm_interm_74_triplet_pt3(a,b)
term(1547) = term(1547) + r1(vrdav_Rr, a,i) * wm_interm_194_triplet_pt3(b,p,i,q) * wm_interm_74_triplet_pt3(a,b)
term(1548) = term(1548) + r1(vrdav_Rr, a,i) * wm_interm_193_triplet_pt3(b,p,i,q) * wm_interm_78_triplet_pt3(a,b)
term(1549) = term(1549) + r1(vrdav_Rr, a,i) * wm_interm_194_triplet_pt3(b,p,i,q) * wm_interm_78_triplet_pt3(a,b)
term(1550) = term(1550) + r1(vrdav_Rr, a,i) * wm_interm_193_triplet_pt3(b,p,i,q) * wm_interm_80_triplet_pt3(a,b)
term(1551) = term(1551) + r1(vrdav_Rr, a,i) * wm_interm_194_triplet_pt3(b,p,i,q) * wm_interm_80_triplet_pt3(a,b)
term(1552) = term(1552) + r1(vrdav_Rr, a,i) * wm_interm_198_triplet_pt3(a,b) * wm_interm_93_triplet_pt3(b,p,i,q)
term(1553) = term(1553) + r1(vrdav_Rr, a,i) * wm_interm_199_triplet_pt3(a,b) * wm_interm_93_triplet_pt3(b,p,i,q)
term(1554) = term(1554) + t2(a,p,q,i) * wm_interm_174_triplet_pt3(b,i) * wm_interm_198_triplet_pt3(a,b)
term(1555) = term(1555) + t2(a,p,q,i) * wm_interm_174_triplet_pt3(b,i) * wm_interm_199_triplet_pt3(a,b)
term(1556) = term(1556) + t2(a,p,q,i) * wm_interm_172_triplet_pt3(b,i) * wm_interm_198_triplet_pt3(a,b)
term(1557) = term(1557) + t2(a,p,q,i) * wm_interm_172_triplet_pt3(b,i) * wm_interm_199_triplet_pt3(a,b)
term(1558) = term(1558) + r1(vrdav_Rr, a,i) * wm_interm_198_triplet_pt3(a,b) * wm_interm_94_triplet_pt3(b,p,i,q)
term(1559) = term(1559) + r1(vrdav_Rr, a,i) * wm_interm_198_triplet_pt3(a,b) * wm_interm_95_triplet_pt3(b,p,i,q)
term(1560) = term(1560) + r1(vrdav_Rr, a,i) * wm_interm_199_triplet_pt3(a,b) * wm_interm_94_triplet_pt3(b,p,i,q)
term(1561) = term(1561) + r1(vrdav_Rr, a,i) * wm_interm_199_triplet_pt3(a,b) * wm_interm_95_triplet_pt3(b,p,i,q)
term(1562) = term(1562) + t2(a,p,q,i) * wm_interm_176_triplet_pt3(b,i) * wm_interm_198_triplet_pt3(a,b)
term(1563) = term(1563) + t2(a,p,q,i) * wm_interm_176_triplet_pt3(b,i) * wm_interm_199_triplet_pt3(a,b)
term(1564) = term(1564) + t2(a,p,q,i) * wm_interm_183_triplet_pt3(b,i) * wm_interm_74_triplet_pt3(a,b)
term(1565) = term(1565) + r1(vrdav_Rr, a,i) * wm_interm_192_triplet_pt3(b,p,i,q) * wm_interm_74_triplet_pt3(a,b)
term(1566) = term(1566) + r1(vrdav_Rr, a,i) * wm_interm_190_triplet_pt3(b,p,i,q) * wm_interm_74_triplet_pt3(a,b)
term(1567) = term(1567) + t2(a,p,q,i) * wm_interm_183_triplet_pt3(b,i) * wm_interm_78_triplet_pt3(a,b)
term(1568) = term(1568) + r1(vrdav_Rr, a,i) * wm_interm_192_triplet_pt3(b,p,i,q) * wm_interm_78_triplet_pt3(a,b)
term(1569) = term(1569) + r1(vrdav_Rr, a,i) * wm_interm_190_triplet_pt3(b,p,i,q) * wm_interm_78_triplet_pt3(a,b)
term(1570) = term(1570) + t2(a,p,q,i) * wm_interm_183_triplet_pt3(b,i) * wm_interm_80_triplet_pt3(a,b)
term(1571) = term(1571) + r1(vrdav_Rr, a,i) * wm_interm_192_triplet_pt3(b,p,i,q) * wm_interm_80_triplet_pt3(a,b)
term(1572) = term(1572) + r1(vrdav_Rr, a,i) * wm_interm_190_triplet_pt3(b,p,i,q) * wm_interm_80_triplet_pt3(a,b)
term(1573) = term(1573) + t2(a,b,q,i) * wm_interm_176_triplet_pt3(b,i) * wm_interm_198_triplet_pt3(a,p)
term(1574) = term(1574) + t2(a,b,q,i) * wm_interm_174_triplet_pt3(b,i) * wm_interm_198_triplet_pt3(a,p)
term(1575) = term(1575) + t2(a,b,q,i) * wm_interm_172_triplet_pt3(b,i) * wm_interm_198_triplet_pt3(a,p)
term(1576) = term(1576) + t2(a,b,q,i) * wm_interm_176_triplet_pt3(b,i) * wm_interm_199_triplet_pt3(a,p)
term(1577) = term(1577) + t2(a,b,q,i) * wm_interm_174_triplet_pt3(b,i) * wm_interm_199_triplet_pt3(a,p)
term(1578) = term(1578) + t2(a,b,q,i) * wm_interm_172_triplet_pt3(b,i) * wm_interm_199_triplet_pt3(a,p)
term(1579) = term(1579) + r1(vrdav_Rr, p,i) * wm_interm_150_triplet_pt3(a,b,i,q) * wm_interm_198_triplet_pt3(b,a)
term(1580) = term(1580) + r1(vrdav_Rr, p,i) * wm_interm_150_triplet_pt3(a,b,i,q) * wm_interm_199_triplet_pt3(b,a)
term(1581) = term(1581) + r1(vrdav_Rr, p,i) * wm_interm_153_triplet_pt3(a,b,i,q) * wm_interm_198_triplet_pt3(b,a)
term(1582) = term(1582) + r1(vrdav_Rr, p,i) * wm_interm_153_triplet_pt3(a,b,i,q) * wm_interm_199_triplet_pt3(b,a)
term(1583) = term(1583) + r1(vrdav_Rr, p,i) * wm_interm_152_triplet_pt3(a,b,i,q) * wm_interm_198_triplet_pt3(b,a)
term(1584) = term(1584) + r1(vrdav_Rr, p,i) * wm_interm_152_triplet_pt3(a,b,i,q) * wm_interm_199_triplet_pt3(b,a)
term(1585) = term(1585) + r1(vrdav_Rr, p,i) * wm_interm_151_triplet_pt3(a,b,i,q) * wm_interm_198_triplet_pt3(b,a)
term(1586) = term(1586) + r1(vrdav_Rr, p,i) * wm_interm_151_triplet_pt3(a,b,i,q) * wm_interm_199_triplet_pt3(b,a)
term(1587) = term(1587) + r1(vrdav_Rr, p,i) * wm_interm_144_triplet_pt3(a,b) * wm_interm_190_triplet_pt3(b,a,i,q)
term(1588) = term(1588) + r1(vrdav_Rr, p,i) * wm_interm_144_triplet_pt3(a,b) * wm_interm_193_triplet_pt3(b,a,i,q)
term(1589) = term(1589) + r1(vrdav_Rr, p,i) * wm_interm_144_triplet_pt3(a,b) * wm_interm_194_triplet_pt3(b,a,i,q)
term(1590) = term(1590) + r1(vrdav_Rr, p,i) * wm_interm_146_triplet_pt3(a,b) * wm_interm_190_triplet_pt3(b,a,i,q)
term(1591) = term(1591) + r1(vrdav_Rr, p,i) * wm_interm_146_triplet_pt3(a,b) * wm_interm_193_triplet_pt3(b,a,i,q)
term(1592) = term(1592) + r1(vrdav_Rr, p,i) * wm_interm_146_triplet_pt3(a,b) * wm_interm_194_triplet_pt3(b,a,i,q)
term(1593) = term(1593) + r1(vrdav_Rr, a,i) * wm_interm_150_triplet_pt3(b,p,i,q) * wm_interm_198_triplet_pt3(a,b)
term(1594) = term(1594) + r1(vrdav_Rr, a,i) * wm_interm_150_triplet_pt3(b,p,i,q) * wm_interm_199_triplet_pt3(a,b)
term(1595) = term(1595) + r1(vrdav_Rr, a,i) * wm_interm_151_triplet_pt3(b,p,i,q) * wm_interm_198_triplet_pt3(a,b)
term(1596) = term(1596) + r1(vrdav_Rr, a,i) * wm_interm_151_triplet_pt3(b,p,i,q) * wm_interm_199_triplet_pt3(a,b)
term(1597) = term(1597) + r1(vrdav_Rr, a,i) * wm_interm_144_triplet_pt3(a,b) * wm_interm_193_triplet_pt3(b,p,i,q)
term(1598) = term(1598) + r1(vrdav_Rr, a,i) * wm_interm_144_triplet_pt3(a,b) * wm_interm_194_triplet_pt3(b,p,i,q)
term(1599) = term(1599) + r1(vrdav_Rr, a,i) * wm_interm_146_triplet_pt3(a,b) * wm_interm_193_triplet_pt3(b,p,i,q)
term(1600) = term(1600) + r1(vrdav_Rr, a,i) * wm_interm_146_triplet_pt3(a,b) * wm_interm_194_triplet_pt3(b,p,i,q)
term(1601) = term(1601) + r1(vrdav_Rr, a,i) * wm_interm_152_triplet_pt3(b,p,i,q) * wm_interm_198_triplet_pt3(a,b)
term(1602) = term(1602) + r1(vrdav_Rr, a,i) * wm_interm_152_triplet_pt3(b,p,i,q) * wm_interm_199_triplet_pt3(a,b)
term(1603) = term(1603) + t2(a,p,q,i) * wm_interm_181_triplet_pt3(b,i) * wm_interm_198_triplet_pt3(a,b)
term(1604) = term(1604) + t2(a,p,q,i) * wm_interm_181_triplet_pt3(b,i) * wm_interm_199_triplet_pt3(a,b)
term(1605) = term(1605) + t2(a,p,q,i) * wm_interm_180_triplet_pt3(b,i) * wm_interm_198_triplet_pt3(a,b)
term(1606) = term(1606) + t2(a,p,q,i) * wm_interm_180_triplet_pt3(b,i) * wm_interm_199_triplet_pt3(a,b)
term(1607) = term(1607) + r1(vrdav_Rr, a,i) * wm_interm_153_triplet_pt3(b,p,i,q) * wm_interm_198_triplet_pt3(a,b)
term(1608) = term(1608) + r1(vrdav_Rr, a,i) * wm_interm_153_triplet_pt3(b,p,i,q) * wm_interm_199_triplet_pt3(a,b)
term(1609) = term(1609) + t2(a,p,q,i) * wm_interm_144_triplet_pt3(a,b) * wm_interm_183_triplet_pt3(b,i)
term(1610) = term(1610) + r1(vrdav_Rr, a,i) * wm_interm_144_triplet_pt3(a,b) * wm_interm_192_triplet_pt3(b,p,i,q)
term(1611) = term(1611) + r1(vrdav_Rr, a,i) * wm_interm_144_triplet_pt3(a,b) * wm_interm_190_triplet_pt3(b,p,i,q)
term(1612) = term(1612) + t2(a,p,q,i) * wm_interm_146_triplet_pt3(a,b) * wm_interm_183_triplet_pt3(b,i)
term(1613) = term(1613) + r1(vrdav_Rr, a,i) * wm_interm_146_triplet_pt3(a,b) * wm_interm_192_triplet_pt3(b,p,i,q)
term(1614) = term(1614) + r1(vrdav_Rr, a,i) * wm_interm_146_triplet_pt3(a,b) * wm_interm_190_triplet_pt3(b,p,i,q)
term(1615) = term(1615) + t2(a,b,q,i) * wm_interm_181_triplet_pt3(b,i) * wm_interm_198_triplet_pt3(a,p)
term(1616) = term(1616) + t2(a,b,q,i) * wm_interm_180_triplet_pt3(b,i) * wm_interm_198_triplet_pt3(a,p)
term(1617) = term(1617) + t2(a,b,q,i) * wm_interm_181_triplet_pt3(b,i) * wm_interm_199_triplet_pt3(a,p)
term(1618) = term(1618) + t2(a,b,q,i) * wm_interm_180_triplet_pt3(b,i) * wm_interm_199_triplet_pt3(a,p)
end do 
end do 
end do 

term(1453) = term(1453) * (-0.5d+0) 
term(1455) = term(1455) * (-0.5d+0) 
term(1456) = term(1456) * (-2.0d+0) 
term(1457) = term(1457) * (2.0d+0) 
term(1458) = term(1458) * (-0.5d+0) 
term(1460) = term(1460) * (-0.5d+0) 
term(1461) = term(1461) * (-2.0d+0) 
term(1462) = term(1462) * (4.0d+0) 
term(1463) = term(1463) * (-2.0d+0) 
term(1464) = term(1464) * (-0.5d+0) 
term(1466) = term(1466) * (-0.5d+0) 
term(1467) = term(1467) * (-0.5d+0) 
term(1469) = term(1469) * (-0.5d+0) 
term(1470) = term(1470) * (-2.0d+0) 
term(1471) = term(1471) * (2.0d+0) 
term(1472) = term(1472) * (-2.0d+0) 
term(1473) = term(1473) * (2.0d+0) 
term(1474) = term(1474) * (-2.0d+0) 
term(1475) = term(1475) * (4.0d+0) 
term(1476) = term(1476) * (-2.0d+0) 
term(1477) = term(1477) * (-8.0d+0) 
term(1478) = term(1478) * (8.0d+0) 
term(1479) = term(1479) * (-2.0d+0) 
term(1480) = term(1480) * (2.0d+0) 
term(1481) = term(1481) * (-8.0d+0) 
term(1482) = term(1482) * (8.0d+0) 
term(1483) = term(1483) * (-1.0d+0) 
term(1484) = term(1484) * (2.0d+0) 
term(1485) = term(1485) * (-1.0d+0) 
term(1486) = term(1486) * (-1.0d+0) 
term(1487) = term(1487) * (2.0d+0) 
term(1488) = term(1488) * (-1.0d+0) 
term(1489) = term(1489) * (-4.0d+0) 
term(1490) = term(1490) * (4.0d+0) 
term(1491) = term(1491) * (-4.0d+0) 
term(1492) = term(1492) * (4.0d+0) 
term(1493) = term(1493) * (2.0d+0) 
term(1494) = term(1494) * (-4.0d+0) 
term(1495) = term(1495) * (-4.0d+0) 
term(1496) = term(1496) * (8.0d+0) 
term(1497) = term(1497) * (2.0d+0) 
term(1498) = term(1498) * (-4.0d+0) 
term(1499) = term(1499) * (-4.0d+0) 
term(1500) = term(1500) * (8.0d+0) 
term(1501) = term(1501) * (8.0d+0) 
term(1502) = term(1502) * (-16.0d+0) 
term(1503) = term(1503) * (-4.0d+0) 
term(1504) = term(1504) * (8.0d+0) 
term(1505) = term(1505) * (2.0d+0) 
term(1506) = term(1506) * (2.0d+0) 
term(1507) = term(1507) * (-4.0d+0) 
term(1508) = term(1508) * (8.0d+0) 
term(1509) = term(1509) * (-8.0d+0) 
term(1510) = term(1510) * (-16.0d+0) 
term(1511) = term(1511) * (16.0d+0) 
term(1512) = term(1512) * (-16.0d+0) 
term(1513) = term(1513) * (16.0d+0) 
term(1514) = term(1514) * (32.0d+0) 
term(1515) = term(1515) * (-32.0d+0) 
term(1516) = term(1516) * (4.0d+0) 
term(1517) = term(1517) * (4.0d+0) 
term(1518) = term(1518) * (-8.0d+0) 
term(1519) = term(1519) * (-1.0d+0) 
term(1520) = term(1520) * (2.0d+0) 
term(1521) = term(1521) * (-1.0d+0) 
term(1522) = term(1522) * (2.0d+0) 
term(1523) = term(1523) * (2.0d+0) 
term(1524) = term(1524) * (-4.0d+0) 
term(1525) = term(1525) * (-1.0d+0) 
term(1526) = term(1526) * (2.0d+0) 
term(1527) = term(1527) * (2.0d+0) 
term(1528) = term(1528) * (-4.0d+0) 
term(1529) = term(1529) * (-1.0d+0) 
term(1530) = term(1530) * (2.0d+0) 
term(1531) = term(1531) * (-1.0d+0) 
term(1532) = term(1532) * (-1.0d+0) 
term(1533) = term(1533) * (2.0d+0) 
term(1534) = term(1534) * (2.0d+0) 
term(1535) = term(1535) * (2.0d+0) 
term(1536) = term(1536) * (-4.0d+0) 
term(1537) = term(1537) * (-1.0d+0) 
term(1538) = term(1538) * (-1.0d+0) 
term(1539) = term(1539) * (2.0d+0) 
term(1540) = term(1540) * (2.0d+0) 
term(1541) = term(1541) * (-4.0d+0) 
term(1542) = term(1542) * (2.0d+0) 
term(1543) = term(1543) * (-4.0d+0) 
term(1544) = term(1544) * (-4.0d+0) 
term(1545) = term(1545) * (8.0d+0) 
term(1546) = term(1546) * (2.0d+0) 
term(1547) = term(1547) * (-4.0d+0) 
term(1548) = term(1548) * (-4.0d+0) 
term(1549) = term(1549) * (8.0d+0) 
term(1550) = term(1550) * (2.0d+0) 
term(1551) = term(1551) * (-4.0d+0) 
term(1552) = term(1552) * (-1.0d+0) 
term(1553) = term(1553) * (2.0d+0) 
term(1554) = term(1554) * (-1.0d+0) 
term(1555) = term(1555) * (2.0d+0) 
term(1556) = term(1556) * (2.0d+0) 
term(1557) = term(1557) * (-4.0d+0) 
term(1558) = term(1558) * (-1.0d+0) 
term(1559) = term(1559) * (2.0d+0) 
term(1560) = term(1560) * (2.0d+0) 
term(1561) = term(1561) * (-4.0d+0) 
term(1562) = term(1562) * (-1.0d+0) 
term(1563) = term(1563) * (2.0d+0) 
term(1564) = term(1564) * (-1.0d+0) 
term(1565) = term(1565) * (-1.0d+0) 
term(1566) = term(1566) * (2.0d+0) 
term(1567) = term(1567) * (2.0d+0) 
term(1568) = term(1568) * (2.0d+0) 
term(1569) = term(1569) * (-4.0d+0) 
term(1570) = term(1570) * (-1.0d+0) 
term(1571) = term(1571) * (-1.0d+0) 
term(1572) = term(1572) * (2.0d+0) 
term(1573) = term(1573) * (4.0d+0) 
term(1574) = term(1574) * (4.0d+0) 
term(1575) = term(1575) * (-8.0d+0) 
term(1576) = term(1576) * (-8.0d+0) 
term(1577) = term(1577) * (-8.0d+0) 
term(1578) = term(1578) * (16.0d+0) 
term(1579) = term(1579) * (-4.0d+0) 
term(1580) = term(1580) * (8.0d+0) 
term(1581) = term(1581) * (-4.0d+0) 
term(1582) = term(1582) * (8.0d+0) 
term(1583) = term(1583) * (4.0d+0) 
term(1584) = term(1584) * (-8.0d+0) 
term(1585) = term(1585) * (4.0d+0) 
term(1586) = term(1586) * (-8.0d+0) 
term(1587) = term(1587) * (-4.0d+0) 
term(1588) = term(1588) * (-4.0d+0) 
term(1589) = term(1589) * (8.0d+0) 
term(1590) = term(1590) * (4.0d+0) 
term(1591) = term(1591) * (4.0d+0) 
term(1592) = term(1592) * (-8.0d+0) 
term(1593) = term(1593) * (8.0d+0) 
term(1594) = term(1594) * (-16.0d+0) 
term(1595) = term(1595) * (-8.0d+0) 
term(1596) = term(1596) * (16.0d+0) 
term(1597) = term(1597) * (8.0d+0) 
term(1598) = term(1598) * (-16.0d+0) 
term(1599) = term(1599) * (-8.0d+0) 
term(1600) = term(1600) * (16.0d+0) 
term(1601) = term(1601) * (-4.0d+0) 
term(1602) = term(1602) * (8.0d+0) 
term(1603) = term(1603) * (-4.0d+0) 
term(1604) = term(1604) * (8.0d+0) 
term(1605) = term(1605) * (4.0d+0) 
term(1606) = term(1606) * (-8.0d+0) 
term(1607) = term(1607) * (4.0d+0) 
term(1608) = term(1608) * (-8.0d+0) 
term(1609) = term(1609) * (-4.0d+0) 
term(1610) = term(1610) * (-4.0d+0) 
term(1611) = term(1611) * (8.0d+0) 
term(1612) = term(1612) * (4.0d+0) 
term(1613) = term(1613) * (4.0d+0) 
term(1614) = term(1614) * (-8.0d+0) 
term(1615) = term(1615) * (16.0d+0) 
term(1616) = term(1616) * (-16.0d+0) 
term(1617) = term(1617) * (-32.0d+0) 
term(1618) = term(1618) * (32.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(1619) = term(1619) + s2(a,p,j,i) * wm_interm_189_triplet_pt3(b,k,j,i) * wm_interm_30_triplet_pt3(a,b,k,q)
term(1620) = term(1620) + s2(a,p,j,i) * wm_interm_189_triplet_pt3(b,k,j,i) * wm_interm_24_triplet_pt3(a,b,k,q)
term(1621) = term(1621) + s2(a,p,j,i) * wm_interm_189_triplet_pt3(b,k,j,i) * wm_interm_23_triplet_pt3(a,b,k,q)
term(1622) = term(1622) + s2(a,p,j,i) * wm_interm_189_triplet_pt3(b,k,j,i) * wm_interm_26_triplet_pt3(a,b,k,q)
term(1623) = term(1623) + s2(a,p,j,i) * wm_interm_189_triplet_pt3(b,k,j,i) * wm_interm_21_triplet_pt3(a,b,k,q)
term(1624) = term(1624) + s2(a,p,j,i) * wm_interm_189_triplet_pt3(b,k,j,i) * wm_interm_22_triplet_pt3(a,b,k,q)
term(1625) = term(1625) + s2(a,p,j,i) * wm_interm_189_triplet_pt3(b,k,j,i) * wm_interm_61_triplet_pt3(a,b,k,q)
term(1626) = term(1626) + s2(a,p,j,i) * wm_interm_189_triplet_pt3(b,k,j,i) * wm_interm_60_triplet_pt3(a,b,k,q)
term(1627) = term(1627) + s2(a,p,j,i) * wm_interm_189_triplet_pt3(b,k,j,i) * wm_interm_58_triplet_pt3(a,b,k,q)
term(1628) = term(1628) + s2(a,p,j,i) * wm_interm_189_triplet_pt3(b,k,j,i) * wm_interm_59_triplet_pt3(a,b,k,q)
term(1629) = term(1629) + t2(a,p,j,i) * wm_interm_185_triplet_pt3(b,k,j,i) * wm_interm_94_triplet_pt3(a,b,k,q)
term(1630) = term(1630) + t2(a,p,j,i) * wm_interm_185_triplet_pt3(b,k,j,i) * wm_interm_93_triplet_pt3(a,b,k,q)
term(1631) = term(1631) + t2(a,p,j,i) * wm_interm_185_triplet_pt3(b,k,j,i) * wm_interm_95_triplet_pt3(a,b,k,q)
term(1632) = term(1632) + t2(a,p,j,i) * wm_interm_185_triplet_pt3(b,k,j,i) * wm_interm_90_triplet_pt3(a,b,k,q)
term(1633) = term(1633) + t2(a,p,j,i) * wm_interm_185_triplet_pt3(b,k,j,i) * wm_interm_91_triplet_pt3(a,b,k,q)
term(1634) = term(1634) + t2(a,p,j,i) * wm_interm_185_triplet_pt3(b,k,j,i) * wm_interm_92_triplet_pt3(a,b,k,q)
term(1635) = term(1635) + t2(a,p,j,i) * wm_interm_152_triplet_pt3(a,b,k,q) * wm_interm_185_triplet_pt3(b,k,j,i)
term(1636) = term(1636) + t2(a,p,j,i) * wm_interm_153_triplet_pt3(a,b,k,q) * wm_interm_185_triplet_pt3(b,k,j,i)
term(1637) = term(1637) + t2(a,p,j,i) * wm_interm_150_triplet_pt3(a,b,k,q) * wm_interm_185_triplet_pt3(b,k,j,i)
term(1638) = term(1638) + t2(a,p,j,i) * wm_interm_151_triplet_pt3(a,b,k,q) * wm_interm_185_triplet_pt3(b,k,j,i)
end do 
end do 
end do 
end do 
end do 

term(1619) = term(1619) * (2.0d+0) 
term(1620) = term(1620) * (2.0d+0) 
term(1621) = term(1621) * (-4.0d+0) 
term(1622) = term(1622) * (-4.0d+0) 
term(1623) = term(1623) * (-4.0d+0) 
term(1624) = term(1624) * (8.0d+0) 
term(1625) = term(1625) * (8.0d+0) 
term(1626) = term(1626) * (-8.0d+0) 
term(1627) = term(1627) * (-16.0d+0) 
term(1628) = term(1628) * (16.0d+0) 
term(1629) = term(1629) * (-2.0d+0) 
term(1630) = term(1630) * (-2.0d+0) 
term(1631) = term(1631) * (4.0d+0) 
term(1632) = term(1632) * (4.0d+0) 
term(1633) = term(1633) * (4.0d+0) 
term(1634) = term(1634) * (-8.0d+0) 
term(1635) = term(1635) * (-8.0d+0) 
term(1636) = term(1636) * (8.0d+0) 
term(1637) = term(1637) * (16.0d+0) 
term(1638) = term(1638) * (-16.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(1639) = term(1639) + s2(a,p,q,i) * t1(b,i) * wm_interm_27_triplet_pt3(b,a)
term(1640) = term(1640) + s2(a,p,q,i) * t1(b,i) * wm_interm_28_triplet_pt3(b,a)
term(1641) = term(1641) + s2(a,p,q,i) * t1(b,i) * wm_interm_29_triplet_pt3(b,a)
term(1642) = term(1642) + s2(a,p,q,i) * t1(b,i) * wm_interm_42_triplet_pt3(b,a)
term(1643) = term(1643) + s2(a,p,q,i) * t1(b,i) * wm_interm_43_triplet_pt3(b,a)
term(1644) = term(1644) + s2(a,p,q,i) * t1(b,i) * wm_interm_44_triplet_pt3(b,a)
term(1645) = term(1645) + s2(a,p,q,i) * t1(b,i) * wm_interm_62_triplet_pt3(b,a)
term(1646) = term(1646) + s2(a,p,q,i) * t1(b,i) * wm_interm_63_triplet_pt3(b,a)
term(1647) = term(1647) + s2(a,p,q,i) * t1(b,i) * wm_interm_70_triplet_pt3(b,a)
term(1648) = term(1648) + s2(a,p,q,i) * t1(b,i) * wm_interm_71_triplet_pt3(b,a)
term(1649) = term(1649) + s2(a,p,q,i) * t1(b,i) * wm_interm_120_triplet_pt3(b,a)
term(1650) = term(1650) + s2(a,p,q,i) * t1(b,i) * wm_interm_121_triplet_pt3(b,a)
term(1651) = term(1651) + s2(a,p,q,i) * t1(b,i) * wm_interm_122_triplet_pt3(b,a)
term(1652) = term(1652) + s2(a,p,q,i) * t1(b,i) * wm_interm_128_triplet_pt3(b,a)
term(1653) = term(1653) + s2(a,p,q,i) * t1(b,i) * wm_interm_129_triplet_pt3(b,a)
term(1654) = term(1654) + s2(a,p,q,i) * t1(b,i) * wm_interm_130_triplet_pt3(b,a)
term(1655) = term(1655) + s2(a,p,q,i) * t1(b,i) * wm_interm_138_triplet_pt3(b,a)
term(1656) = term(1656) + s2(a,p,q,i) * t1(b,i) * wm_interm_139_triplet_pt3(b,a)
term(1657) = term(1657) + s2(a,p,q,i) * t1(b,i) * wm_interm_142_triplet_pt3(b,a)
term(1658) = term(1658) + s2(a,p,q,i) * t1(b,i) * wm_interm_143_triplet_pt3(b,a)
term(1659) = term(1659) + r1(vrdav_Rl, a,i) * wm_interm_198_triplet_pt3(b,a) * wm_interm_30_triplet_pt3(b,p,i,q)
term(1660) = term(1660) + r1(vrdav_Rl, a,i) * wm_interm_199_triplet_pt3(b,a) * wm_interm_30_triplet_pt3(b,p,i,q)
term(1661) = term(1661) + r1(vrdav_Rl, a,i) * wm_interm_198_triplet_pt3(b,a) * wm_interm_24_triplet_pt3(b,p,i,q)
term(1662) = term(1662) + r1(vrdav_Rl, a,i) * wm_interm_199_triplet_pt3(b,a) * wm_interm_24_triplet_pt3(b,p,i,q)
term(1663) = term(1663) + r1(vrdav_Rl, a,i) * wm_interm_198_triplet_pt3(b,a) * wm_interm_23_triplet_pt3(b,p,i,q)
term(1664) = term(1664) + r1(vrdav_Rl, a,i) * wm_interm_199_triplet_pt3(b,a) * wm_interm_23_triplet_pt3(b,p,i,q)
term(1665) = term(1665) + r1(vrdav_Rl, a,i) * wm_interm_198_triplet_pt3(b,a) * wm_interm_26_triplet_pt3(b,p,i,q)
term(1666) = term(1666) + r1(vrdav_Rl, a,i) * wm_interm_199_triplet_pt3(b,a) * wm_interm_26_triplet_pt3(b,p,i,q)
term(1667) = term(1667) + r1(vrdav_Rl, a,i) * wm_interm_198_triplet_pt3(b,a) * wm_interm_21_triplet_pt3(b,p,i,q)
term(1668) = term(1668) + r1(vrdav_Rl, a,i) * wm_interm_199_triplet_pt3(b,a) * wm_interm_21_triplet_pt3(b,p,i,q)
term(1669) = term(1669) + r1(vrdav_Rl, a,i) * wm_interm_198_triplet_pt3(b,a) * wm_interm_22_triplet_pt3(b,p,i,q)
term(1670) = term(1670) + r1(vrdav_Rl, a,i) * wm_interm_199_triplet_pt3(b,a) * wm_interm_22_triplet_pt3(b,p,i,q)
term(1671) = term(1671) + r1(vrdav_Rl, a,i) * wm_interm_198_triplet_pt3(b,a) * wm_interm_61_triplet_pt3(b,p,i,q)
term(1672) = term(1672) + r1(vrdav_Rl, a,i) * wm_interm_199_triplet_pt3(b,a) * wm_interm_61_triplet_pt3(b,p,i,q)
term(1673) = term(1673) + r1(vrdav_Rl, a,i) * wm_interm_198_triplet_pt3(b,a) * wm_interm_60_triplet_pt3(b,p,i,q)
term(1674) = term(1674) + r1(vrdav_Rl, a,i) * wm_interm_199_triplet_pt3(b,a) * wm_interm_60_triplet_pt3(b,p,i,q)
term(1675) = term(1675) + r1(vrdav_Rl, a,i) * wm_interm_198_triplet_pt3(b,a) * wm_interm_58_triplet_pt3(b,p,i,q)
term(1676) = term(1676) + r1(vrdav_Rl, a,i) * wm_interm_199_triplet_pt3(b,a) * wm_interm_58_triplet_pt3(b,p,i,q)
term(1677) = term(1677) + r1(vrdav_Rl, a,i) * wm_interm_198_triplet_pt3(b,a) * wm_interm_59_triplet_pt3(b,p,i,q)
term(1678) = term(1678) + r1(vrdav_Rl, a,i) * wm_interm_199_triplet_pt3(b,a) * wm_interm_59_triplet_pt3(b,p,i,q)
end do 
end do 
end do 

term(1639) = term(1639) * (-0.5d+0) 
term(1641) = term(1641) * (-0.5d+0) 
term(1642) = term(1642) * (-0.5d+0) 
term(1643) = term(1643) * (-0.5d+0) 
term(1645) = term(1645) * (-2.0d+0) 
term(1646) = term(1646) * (2.0d+0) 
term(1647) = term(1647) * (-2.0d+0) 
term(1648) = term(1648) * (2.0d+0) 
term(1649) = term(1649) * (-1.0d+0) 
term(1650) = term(1650) * (2.0d+0) 
term(1651) = term(1651) * (-1.0d+0) 
term(1652) = term(1652) * (-1.0d+0) 
term(1653) = term(1653) * (-1.0d+0) 
term(1654) = term(1654) * (2.0d+0) 
term(1655) = term(1655) * (-4.0d+0) 
term(1656) = term(1656) * (4.0d+0) 
term(1657) = term(1657) * (-4.0d+0) 
term(1658) = term(1658) * (4.0d+0) 
term(1659) = term(1659) * (-1.0d+0) 
term(1660) = term(1660) * (2.0d+0) 
term(1661) = term(1661) * (-1.0d+0) 
term(1662) = term(1662) * (2.0d+0) 
term(1663) = term(1663) * (2.0d+0) 
term(1664) = term(1664) * (-4.0d+0) 
term(1665) = term(1665) * (2.0d+0) 
term(1666) = term(1666) * (-4.0d+0) 
term(1667) = term(1667) * (2.0d+0) 
term(1668) = term(1668) * (-4.0d+0) 
term(1669) = term(1669) * (-4.0d+0) 
term(1670) = term(1670) * (8.0d+0) 
term(1671) = term(1671) * (-4.0d+0) 
term(1672) = term(1672) * (8.0d+0) 
term(1673) = term(1673) * (4.0d+0) 
term(1674) = term(1674) * (-8.0d+0) 
term(1675) = term(1675) * (8.0d+0) 
term(1676) = term(1676) * (-16.0d+0) 
term(1677) = term(1677) * (-8.0d+0) 
term(1678) = term(1678) * (16.0d+0) 

do i = 1, nocc 
term(1679) = term(1679) + wm_interm_75_triplet_pt3(i,q) * wm_interm_87_triplet_pt3(p,i)
term(1680) = term(1680) + wm_interm_81_triplet_pt3(i,q) * wm_interm_87_triplet_pt3(p,i)
term(1681) = term(1681) + wm_interm_79_triplet_pt3(i,q) * wm_interm_87_triplet_pt3(p,i)
term(1682) = term(1682) + wm_interm_75_triplet_pt3(i,q) * wm_interm_88_triplet_pt3(p,i)
term(1683) = term(1683) + wm_interm_81_triplet_pt3(i,q) * wm_interm_88_triplet_pt3(p,i)
term(1684) = term(1684) + wm_interm_79_triplet_pt3(i,q) * wm_interm_88_triplet_pt3(p,i)
term(1685) = term(1685) + wm_interm_75_triplet_pt3(i,q) * wm_interm_89_triplet_pt3(p,i)
term(1686) = term(1686) + wm_interm_81_triplet_pt3(i,q) * wm_interm_89_triplet_pt3(p,i)
term(1687) = term(1687) + wm_interm_79_triplet_pt3(i,q) * wm_interm_89_triplet_pt3(p,i)
term(1688) = term(1688) + wm_interm_75_triplet_pt3(i,q) * wm_interm_96_triplet_pt3(p,i)
term(1689) = term(1689) + wm_interm_81_triplet_pt3(i,q) * wm_interm_96_triplet_pt3(p,i)
term(1690) = term(1690) + wm_interm_79_triplet_pt3(i,q) * wm_interm_96_triplet_pt3(p,i)
term(1691) = term(1691) + wm_interm_75_triplet_pt3(i,q) * wm_interm_97_triplet_pt3(p,i)
term(1692) = term(1692) + wm_interm_81_triplet_pt3(i,q) * wm_interm_97_triplet_pt3(p,i)
term(1693) = term(1693) + wm_interm_79_triplet_pt3(i,q) * wm_interm_97_triplet_pt3(p,i)
term(1694) = term(1694) + wm_interm_145_triplet_pt3(i,q) * wm_interm_87_triplet_pt3(p,i)
term(1695) = term(1695) + wm_interm_147_triplet_pt3(i,q) * wm_interm_87_triplet_pt3(p,i)
term(1696) = term(1696) + wm_interm_145_triplet_pt3(i,q) * wm_interm_88_triplet_pt3(p,i)
term(1697) = term(1697) + wm_interm_147_triplet_pt3(i,q) * wm_interm_88_triplet_pt3(p,i)
term(1698) = term(1698) + wm_interm_145_triplet_pt3(i,q) * wm_interm_89_triplet_pt3(p,i)
term(1699) = term(1699) + wm_interm_147_triplet_pt3(i,q) * wm_interm_89_triplet_pt3(p,i)
term(1700) = term(1700) + wm_interm_145_triplet_pt3(i,q) * wm_interm_96_triplet_pt3(p,i)
term(1701) = term(1701) + wm_interm_147_triplet_pt3(i,q) * wm_interm_96_triplet_pt3(p,i)
term(1702) = term(1702) + wm_interm_145_triplet_pt3(i,q) * wm_interm_97_triplet_pt3(p,i)
term(1703) = term(1703) + wm_interm_147_triplet_pt3(i,q) * wm_interm_97_triplet_pt3(p,i)
term(1704) = term(1704) + wm_interm_163_triplet_pt3(q,i) * wm_interm_183_triplet_pt3(p,i)
end do 

term(1679) = term(1679) * (-1.0d+0) 
term(1680) = term(1680) * (-1.0d+0) 
term(1681) = term(1681) * (2.0d+0) 
term(1682) = term(1682) * (0.5d+0) 
term(1683) = term(1683) * (0.5d+0) 
term(1684) = term(1684) * (-1.0d+0) 
term(1685) = term(1685) * (0.5d+0) 
term(1686) = term(1686) * (0.5d+0) 
term(1687) = term(1687) * (-1.0d+0) 
term(1688) = term(1688) * (-2.0d+0) 
term(1689) = term(1689) * (-2.0d+0) 
term(1690) = term(1690) * (4.0d+0) 
term(1691) = term(1691) * (2.0d+0) 
term(1692) = term(1692) * (2.0d+0) 
term(1693) = term(1693) * (-4.0d+0) 
term(1694) = term(1694) * (-4.0d+0) 
term(1695) = term(1695) * (4.0d+0) 
term(1696) = term(1696) * (2.0d+0) 
term(1697) = term(1697) * (-2.0d+0) 
term(1698) = term(1698) * (2.0d+0) 
term(1699) = term(1699) * (-2.0d+0) 
term(1700) = term(1700) * (-8.0d+0) 
term(1701) = term(1701) * (8.0d+0) 
term(1702) = term(1702) * (8.0d+0) 
term(1703) = term(1703) * (-8.0d+0) 
term(1704) = term(1704) * (2.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(1705) = term(1705) + s2(a,p,q,i) * wm_interm_189_triplet_pt3(a,j,k,l) * wm_interm_20_triplet_pt3(j,i,l,k)
term(1706) = term(1706) + s2(a,p,j,i) * wm_interm_189_triplet_pt3(a,q,k,l) * wm_interm_20_triplet_pt3(i,j,l,k)
term(1707) = term(1707) + s2(a,p,q,i) * wm_interm_189_triplet_pt3(a,j,k,l) * wm_interm_57_triplet_pt3(j,i,l,k)
term(1708) = term(1708) + s2(a,p,j,i) * wm_interm_189_triplet_pt3(a,q,k,l) * wm_interm_57_triplet_pt3(i,j,l,k)
term(1709) = term(1709) + t2(a,p,q,i) * wm_interm_175_triplet_pt3(a,j,k,l) * wm_interm_191_triplet_pt3(l,i,j,k)
term(1710) = term(1710) + t2(a,p,q,i) * wm_interm_171_triplet_pt3(a,j,k,l) * wm_interm_191_triplet_pt3(i,l,j,k)
term(1711) = term(1711) + t2(a,p,q,i) * wm_interm_171_triplet_pt3(a,j,k,l) * wm_interm_191_triplet_pt3(l,i,j,k)
term(1712) = term(1712) + t2(a,p,q,i) * wm_interm_185_triplet_pt3(a,j,k,l) * wm_interm_99_triplet_pt3(j,i,l,k)
term(1713) = term(1713) + t2(a,p,q,i) * wm_interm_182_triplet_pt3(a,j,k,l) * wm_interm_191_triplet_pt3(l,i,j,k)
term(1714) = term(1714) + t2(a,p,q,i) * wm_interm_179_triplet_pt3(a,j,k,l) * wm_interm_191_triplet_pt3(i,l,j,k)
term(1715) = term(1715) + t2(a,p,q,i) * wm_interm_179_triplet_pt3(a,j,k,l) * wm_interm_191_triplet_pt3(l,i,j,k)
term(1716) = term(1716) + t2(a,p,q,i) * wm_interm_154_triplet_pt3(j,i,k,l) * wm_interm_185_triplet_pt3(a,j,l,k)
end do 
end do 
end do 
end do 
end do 

term(1705) = term(1705) * (2.0d+0) 
term(1706) = term(1706) * (2.0d+0) 
term(1707) = term(1707) * (4.0d+0) 
term(1708) = term(1708) * (4.0d+0) 
term(1709) = term(1709) * (-1.0d+0) 
term(1710) = term(1710) * (-1.0d+0) 
term(1711) = term(1711) * (2.0d+0) 
term(1712) = term(1712) * (-1.0d+0) 
term(1713) = term(1713) * (-2.0d+0) 
term(1714) = term(1714) * (-2.0d+0) 
term(1715) = term(1715) * (4.0d+0) 
term(1716) = term(1716) * (-2.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(1717) = term(1717) + s2(a,p,j,i) * wm_interm_189_triplet_pt3(b,k,i,j) * wm_interm_21_triplet_pt3(a,b,k,q)
term(1718) = term(1718) + s2(a,p,j,i) * wm_interm_189_triplet_pt3(b,k,i,j) * wm_interm_22_triplet_pt3(a,b,k,q)
term(1719) = term(1719) + s2(a,p,j,i) * wm_interm_189_triplet_pt3(b,k,i,j) * wm_interm_26_triplet_pt3(a,b,k,q)
term(1720) = term(1720) + s2(a,p,j,i) * wm_interm_189_triplet_pt3(b,k,i,j) * wm_interm_23_triplet_pt3(a,b,k,q)
term(1721) = term(1721) + s2(a,p,j,i) * wm_interm_189_triplet_pt3(b,k,i,j) * wm_interm_25_triplet_pt3(a,b,k,q)
term(1722) = term(1722) + s2(a,p,j,i) * wm_interm_189_triplet_pt3(b,k,i,j) * wm_interm_30_triplet_pt3(a,b,k,q)
term(1723) = term(1723) + s2(a,p,j,i) * wm_interm_189_triplet_pt3(b,k,i,j) * wm_interm_58_triplet_pt3(a,b,k,q)
term(1724) = term(1724) + s2(a,p,j,i) * wm_interm_189_triplet_pt3(b,k,i,j) * wm_interm_59_triplet_pt3(a,b,k,q)
term(1725) = term(1725) + s2(a,p,j,i) * wm_interm_189_triplet_pt3(b,k,i,j) * wm_interm_60_triplet_pt3(a,b,k,q)
term(1726) = term(1726) + s2(a,p,j,i) * wm_interm_189_triplet_pt3(b,k,i,j) * wm_interm_61_triplet_pt3(a,b,k,q)
term(1727) = term(1727) + t2(a,p,j,i) * wm_interm_185_triplet_pt3(b,k,i,j) * wm_interm_95_triplet_pt3(a,b,k,q)
term(1728) = term(1728) + t2(a,p,j,i) * wm_interm_104_triplet_pt3(a,b,k,q) * wm_interm_185_triplet_pt3(b,k,i,j)
term(1729) = term(1729) + t2(a,p,j,i) * wm_interm_185_triplet_pt3(b,k,i,j) * wm_interm_93_triplet_pt3(a,b,k,q)
term(1730) = term(1730) + t2(a,p,j,i) * wm_interm_185_triplet_pt3(b,k,i,j) * wm_interm_90_triplet_pt3(a,b,k,q)
term(1731) = term(1731) + t2(a,p,j,i) * wm_interm_185_triplet_pt3(b,k,i,j) * wm_interm_91_triplet_pt3(a,b,k,q)
term(1732) = term(1732) + t2(a,p,j,i) * wm_interm_185_triplet_pt3(b,k,i,j) * wm_interm_92_triplet_pt3(a,b,k,q)
term(1733) = term(1733) + t2(a,p,j,i) * wm_interm_153_triplet_pt3(a,b,k,q) * wm_interm_185_triplet_pt3(b,k,i,j)
term(1734) = term(1734) + t2(a,p,j,i) * wm_interm_152_triplet_pt3(a,b,k,q) * wm_interm_185_triplet_pt3(b,k,i,j)
term(1735) = term(1735) + t2(a,p,j,i) * wm_interm_150_triplet_pt3(a,b,k,q) * wm_interm_185_triplet_pt3(b,k,i,j)
term(1736) = term(1736) + t2(a,p,j,i) * wm_interm_151_triplet_pt3(a,b,k,q) * wm_interm_185_triplet_pt3(b,k,i,j)
end do 
end do 
end do 
end do 
end do 

term(1717) = term(1717) * (2.0d+0) 
term(1718) = term(1718) * (-4.0d+0) 
term(1719) = term(1719) * (2.0d+0) 
term(1720) = term(1720) * (2.0d+0) 
term(1721) = term(1721) * (2.0d+0) 
term(1722) = term(1722) * (-4.0d+0) 
term(1723) = term(1723) * (8.0d+0) 
term(1724) = term(1724) * (-8.0d+0) 
term(1725) = term(1725) * (8.0d+0) 
term(1726) = term(1726) * (-8.0d+0) 
term(1727) = term(1727) * (-2.0d+0) 
term(1728) = term(1728) * (-2.0d+0) 
term(1729) = term(1729) * (4.0d+0) 
term(1730) = term(1730) * (-2.0d+0) 
term(1731) = term(1731) * (-2.0d+0) 
term(1732) = term(1732) * (4.0d+0) 
term(1733) = term(1733) * (-8.0d+0) 
term(1734) = term(1734) * (8.0d+0) 
term(1735) = term(1735) * (-8.0d+0) 
term(1736) = term(1736) * (8.0d+0) 

do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(1737) = term(1737) + r1(vrdav_Rl, b,q) * wm_interm_198_triplet_pt3(a,b) * wm_interm_9_triplet_pt3(a,p)
term(1738) = term(1738) + r1(vrdav_Rl, b,q) * wm_interm_199_triplet_pt3(a,b) * wm_interm_9_triplet_pt3(a,p)
term(1739) = term(1739) + r1(vrdav_Rl, b,q) * wm_interm_198_triplet_pt3(a,b) * wm_interm_5_triplet_pt3(a,p)
term(1740) = term(1740) + r1(vrdav_Rl, b,q) * wm_interm_199_triplet_pt3(a,b) * wm_interm_5_triplet_pt3(a,p)
term(1741) = term(1741) + r1(vrdav_Rl, b,q) * wm_interm_198_triplet_pt3(a,b) * wm_interm_6_triplet_pt3(a,p)
term(1742) = term(1742) + r1(vrdav_Rl, b,q) * wm_interm_199_triplet_pt3(a,b) * wm_interm_6_triplet_pt3(a,p)
term(1743) = term(1743) + r1(vrdav_Rl, b,q) * wm_interm_195_triplet_pt3(a,p) * wm_interm_198_triplet_pt3(a,b)
term(1744) = term(1744) + r1(vrdav_Rl, b,q) * wm_interm_195_triplet_pt3(a,p) * wm_interm_199_triplet_pt3(a,b)
term(1745) = term(1745) + r1(vrdav_Rl, b,q) * wm_interm_198_triplet_pt3(a,b) * wm_interm_47_triplet_pt3(a,p)
term(1746) = term(1746) + r1(vrdav_Rl, b,q) * wm_interm_199_triplet_pt3(a,b) * wm_interm_47_triplet_pt3(a,p)
term(1747) = term(1747) + r1(vrdav_Rl, b,q) * wm_interm_198_triplet_pt3(a,b) * wm_interm_48_triplet_pt3(a,p)
term(1748) = term(1748) + r1(vrdav_Rl, b,q) * wm_interm_199_triplet_pt3(a,b) * wm_interm_48_triplet_pt3(a,p)
end do 
end do 

term(1737) = term(1737) * (-1.0d+0) 
term(1738) = term(1738) * (2.0d+0) 
term(1739) = term(1739) * (-1.0d+0) 
term(1740) = term(1740) * (2.0d+0) 
term(1741) = term(1741) * (2.0d+0) 
term(1742) = term(1742) * (-4.0d+0) 
term(1743) = term(1743) * (-2.0d+0) 
term(1744) = term(1744) * (4.0d+0) 
term(1745) = term(1745) * (-2.0d+0) 
term(1746) = term(1746) * (4.0d+0) 
term(1747) = term(1747) * (4.0d+0) 
term(1748) = term(1748) * (-8.0d+0) 

do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(1749) = term(1749) + r2p(vrdav_Rl, a,i,p,q) * t1(b,i) * wm_interm_5_triplet_pt3(a,b)
term(1750) = term(1750) + r2p(vrdav_Rl, a,i,p,q) * t1(b,i) * wm_interm_6_triplet_pt3(a,b)
term(1751) = term(1751) + r2p(vrdav_Rl, a,i,p,q) * t1(b,i) * wm_interm_9_triplet_pt3(a,b)
term(1752) = term(1752) + r2p(vrdav_Rl, a,i,p,q) * t1(b,i) * wm_interm_47_triplet_pt3(a,b)
term(1753) = term(1753) + r2p(vrdav_Rl, a,i,p,q) * t1(b,i) * wm_interm_48_triplet_pt3(a,b)
term(1754) = term(1754) + r2p(vrdav_Rr, a,i,p,q) * s1(b,i) * wm_interm_74_triplet_pt3(a,b)
term(1755) = term(1755) + r2p(vrdav_Rr, a,i,p,q) * s1(b,i) * wm_interm_78_triplet_pt3(a,b)
term(1756) = term(1756) + r2p(vrdav_Rr, a,i,p,q) * s1(b,i) * wm_interm_80_triplet_pt3(a,b)
term(1757) = term(1757) + r2m(vrdav_Rr, a,i,p,q) * s1(b,i) * wm_interm_74_triplet_pt3(a,b)
term(1758) = term(1758) + r2m(vrdav_Rr, a,i,p,q) * s1(b,i) * wm_interm_78_triplet_pt3(a,b)
term(1759) = term(1759) + r2m(vrdav_Rr, a,i,p,q) * s1(b,i) * wm_interm_80_triplet_pt3(a,b)
term(1760) = term(1760) + r2m(vrdav_Rl, a,i,p,q) * t1(b,i) * wm_interm_5_triplet_pt3(a,b)
term(1761) = term(1761) + r2m(vrdav_Rl, a,i,p,q) * t1(b,i) * wm_interm_6_triplet_pt3(a,b)
term(1762) = term(1762) + r2m(vrdav_Rl, a,i,p,q) * t1(b,i) * wm_interm_9_triplet_pt3(a,b)
term(1763) = term(1763) + r2m(vrdav_Rl, a,i,p,q) * t1(b,i) * wm_interm_47_triplet_pt3(a,b)
term(1764) = term(1764) + r2m(vrdav_Rl, a,i,p,q) * t1(b,i) * wm_interm_48_triplet_pt3(a,b)
term(1765) = term(1765) + r2p(vrdav_Rr, a,i,p,q) * s1(b,i) * wm_interm_144_triplet_pt3(a,b)
term(1766) = term(1766) + r2p(vrdav_Rr, a,i,p,q) * s1(b,i) * wm_interm_146_triplet_pt3(a,b)
term(1767) = term(1767) + r2m(vrdav_Rr, a,i,p,q) * s1(b,i) * wm_interm_144_triplet_pt3(a,b)
term(1768) = term(1768) + r2m(vrdav_Rr, a,i,p,q) * s1(b,i) * wm_interm_146_triplet_pt3(a,b)
term(1769) = term(1769) + s2(a,p,i,q) * wm_interm_186_triplet_pt3(b,i) * wm_interm_5_triplet_pt3(a,b)
term(1770) = term(1770) + s2(a,p,i,q) * wm_interm_186_triplet_pt3(b,i) * wm_interm_6_triplet_pt3(a,b)
term(1771) = term(1771) + s2(a,p,i,q) * wm_interm_186_triplet_pt3(b,i) * wm_interm_9_triplet_pt3(a,b)
term(1772) = term(1772) + s2(a,p,i,q) * wm_interm_186_triplet_pt3(b,i) * wm_interm_47_triplet_pt3(a,b)
term(1773) = term(1773) + s2(a,p,i,q) * wm_interm_186_triplet_pt3(b,i) * wm_interm_48_triplet_pt3(a,b)
term(1774) = term(1774) + s2(a,p,i,q) * wm_interm_186_triplet_pt3(b,i) * wm_interm_195_triplet_pt3(a,b)
term(1775) = term(1775) + s2(a,b,i,q) * wm_interm_186_triplet_pt3(b,i) * wm_interm_9_triplet_pt3(a,p)
term(1776) = term(1776) + s2(a,b,i,q) * wm_interm_186_triplet_pt3(b,i) * wm_interm_5_triplet_pt3(a,p)
term(1777) = term(1777) + s2(a,b,i,q) * wm_interm_186_triplet_pt3(b,i) * wm_interm_6_triplet_pt3(a,p)
term(1778) = term(1778) + s2(a,b,i,q) * wm_interm_186_triplet_pt3(b,i) * wm_interm_195_triplet_pt3(a,p)
term(1779) = term(1779) + s2(a,b,i,q) * wm_interm_186_triplet_pt3(b,i) * wm_interm_47_triplet_pt3(a,p)
term(1780) = term(1780) + s2(a,b,i,q) * wm_interm_186_triplet_pt3(b,i) * wm_interm_48_triplet_pt3(a,p)
term(1781) = term(1781) + t2(a,p,i,q) * wm_interm_174_triplet_pt3(b,i) * wm_interm_198_triplet_pt3(a,b)
term(1782) = term(1782) + t2(a,p,i,q) * wm_interm_174_triplet_pt3(b,i) * wm_interm_199_triplet_pt3(a,b)
term(1783) = term(1783) + t2(a,p,i,q) * wm_interm_172_triplet_pt3(b,i) * wm_interm_198_triplet_pt3(a,b)
term(1784) = term(1784) + t2(a,p,i,q) * wm_interm_172_triplet_pt3(b,i) * wm_interm_199_triplet_pt3(a,b)
term(1785) = term(1785) + t2(a,p,i,q) * wm_interm_176_triplet_pt3(b,i) * wm_interm_198_triplet_pt3(a,b)
term(1786) = term(1786) + t2(a,p,i,q) * wm_interm_176_triplet_pt3(b,i) * wm_interm_199_triplet_pt3(a,b)
term(1787) = term(1787) + t2(a,p,i,q) * wm_interm_183_triplet_pt3(b,i) * wm_interm_74_triplet_pt3(a,b)
term(1788) = term(1788) + t2(a,p,i,q) * wm_interm_183_triplet_pt3(b,i) * wm_interm_78_triplet_pt3(a,b)
term(1789) = term(1789) + t2(a,p,i,q) * wm_interm_183_triplet_pt3(b,i) * wm_interm_80_triplet_pt3(a,b)
term(1790) = term(1790) + t2(a,b,i,q) * wm_interm_174_triplet_pt3(b,i) * wm_interm_198_triplet_pt3(a,p)
term(1791) = term(1791) + t2(a,b,i,q) * wm_interm_176_triplet_pt3(b,i) * wm_interm_198_triplet_pt3(a,p)
term(1792) = term(1792) + t2(a,b,i,q) * wm_interm_172_triplet_pt3(b,i) * wm_interm_198_triplet_pt3(a,p)
term(1793) = term(1793) + t2(a,b,i,q) * wm_interm_174_triplet_pt3(b,i) * wm_interm_199_triplet_pt3(a,p)
term(1794) = term(1794) + t2(a,b,i,q) * wm_interm_176_triplet_pt3(b,i) * wm_interm_199_triplet_pt3(a,p)
term(1795) = term(1795) + t2(a,b,i,q) * wm_interm_172_triplet_pt3(b,i) * wm_interm_199_triplet_pt3(a,p)
term(1796) = term(1796) + t2(a,b,i,q) * wm_interm_183_triplet_pt3(b,i) * wm_interm_80_triplet_pt3(a,p)
term(1797) = term(1797) + t2(a,b,i,q) * wm_interm_183_triplet_pt3(b,i) * wm_interm_74_triplet_pt3(a,p)
term(1798) = term(1798) + t2(a,b,i,q) * wm_interm_183_triplet_pt3(b,i) * wm_interm_78_triplet_pt3(a,p)
term(1799) = term(1799) + t2(a,p,i,q) * wm_interm_181_triplet_pt3(b,i) * wm_interm_198_triplet_pt3(a,b)
term(1800) = term(1800) + t2(a,p,i,q) * wm_interm_181_triplet_pt3(b,i) * wm_interm_199_triplet_pt3(a,b)
term(1801) = term(1801) + t2(a,p,i,q) * wm_interm_180_triplet_pt3(b,i) * wm_interm_198_triplet_pt3(a,b)
term(1802) = term(1802) + t2(a,p,i,q) * wm_interm_180_triplet_pt3(b,i) * wm_interm_199_triplet_pt3(a,b)
term(1803) = term(1803) + t2(a,p,i,q) * wm_interm_144_triplet_pt3(a,b) * wm_interm_183_triplet_pt3(b,i)
term(1804) = term(1804) + t2(a,p,i,q) * wm_interm_146_triplet_pt3(a,b) * wm_interm_183_triplet_pt3(b,i)
term(1805) = term(1805) + t2(a,b,i,q) * wm_interm_181_triplet_pt3(b,i) * wm_interm_198_triplet_pt3(a,p)
term(1806) = term(1806) + t2(a,b,i,q) * wm_interm_180_triplet_pt3(b,i) * wm_interm_198_triplet_pt3(a,p)
term(1807) = term(1807) + t2(a,b,i,q) * wm_interm_181_triplet_pt3(b,i) * wm_interm_199_triplet_pt3(a,p)
term(1808) = term(1808) + t2(a,b,i,q) * wm_interm_180_triplet_pt3(b,i) * wm_interm_199_triplet_pt3(a,p)
term(1809) = term(1809) + t2(a,b,i,q) * wm_interm_144_triplet_pt3(a,p) * wm_interm_183_triplet_pt3(b,i)
term(1810) = term(1810) + t2(a,b,i,q) * wm_interm_146_triplet_pt3(a,p) * wm_interm_183_triplet_pt3(b,i)
end do 
end do 
end do 

term(1750) = term(1750) * (-2.0d+0) 
term(1752) = term(1752) * (4.0d+0) 
term(1753) = term(1753) * (-4.0d+0) 
term(1755) = term(1755) * (-2.0d+0) 
term(1757) = term(1757) * (2.0d+0) 
term(1758) = term(1758) * (-4.0d+0) 
term(1759) = term(1759) * (2.0d+0) 
term(1760) = term(1760) * (2.0d+0) 
term(1761) = term(1761) * (-4.0d+0) 
term(1762) = term(1762) * (2.0d+0) 
term(1763) = term(1763) * (8.0d+0) 
term(1764) = term(1764) * (-8.0d+0) 
term(1765) = term(1765) * (4.0d+0) 
term(1766) = term(1766) * (-4.0d+0) 
term(1767) = term(1767) * (8.0d+0) 
term(1768) = term(1768) * (-8.0d+0) 
term(1769) = term(1769) * (-4.0d+0) 
term(1770) = term(1770) * (8.0d+0) 
term(1771) = term(1771) * (-4.0d+0) 
term(1772) = term(1772) * (-8.0d+0) 
term(1773) = term(1773) * (16.0d+0) 
term(1774) = term(1774) * (-8.0d+0) 
term(1775) = term(1775) * (-1.0d+0) 
term(1776) = term(1776) * (-1.0d+0) 
term(1777) = term(1777) * (2.0d+0) 
term(1778) = term(1778) * (-2.0d+0) 
term(1779) = term(1779) * (-2.0d+0) 
term(1780) = term(1780) * (4.0d+0) 
term(1781) = term(1781) * (2.0d+0) 
term(1782) = term(1782) * (-4.0d+0) 
term(1783) = term(1783) * (-4.0d+0) 
term(1784) = term(1784) * (8.0d+0) 
term(1785) = term(1785) * (2.0d+0) 
term(1786) = term(1786) * (-4.0d+0) 
term(1787) = term(1787) * (2.0d+0) 
term(1788) = term(1788) * (-4.0d+0) 
term(1789) = term(1789) * (2.0d+0) 
term(1790) = term(1790) * (-2.0d+0) 
term(1791) = term(1791) * (-2.0d+0) 
term(1792) = term(1792) * (4.0d+0) 
term(1793) = term(1793) * (4.0d+0) 
term(1794) = term(1794) * (4.0d+0) 
term(1795) = term(1795) * (-8.0d+0) 
term(1796) = term(1796) * (-2.0d+0) 
term(1797) = term(1797) * (-2.0d+0) 
term(1798) = term(1798) * (4.0d+0) 
term(1799) = term(1799) * (8.0d+0) 
term(1800) = term(1800) * (-16.0d+0) 
term(1801) = term(1801) * (-8.0d+0) 
term(1802) = term(1802) * (16.0d+0) 
term(1803) = term(1803) * (8.0d+0) 
term(1804) = term(1804) * (-8.0d+0) 
term(1805) = term(1805) * (-8.0d+0) 
term(1806) = term(1806) * (8.0d+0) 
term(1807) = term(1807) * (16.0d+0) 
term(1808) = term(1808) * (-16.0d+0) 
term(1809) = term(1809) * (-8.0d+0) 
term(1810) = term(1810) * (8.0d+0) 

do a = nocc + 1, nactive 
do i = 1, nocc 
do b = nocc + 1, nactive 
term(1811) = term(1811) + s2(a,p,i,q) * t1(b,i) * wm_interm_27_triplet_pt3(b,a)
term(1812) = term(1812) + s2(a,p,i,q) * t1(b,i) * wm_interm_28_triplet_pt3(b,a)
term(1813) = term(1813) + s2(a,p,i,q) * t1(b,i) * wm_interm_29_triplet_pt3(b,a)
term(1814) = term(1814) + s2(a,p,i,q) * t1(b,i) * wm_interm_43_triplet_pt3(b,a)
term(1815) = term(1815) + s2(a,p,i,q) * t1(b,i) * wm_interm_44_triplet_pt3(b,a)
term(1816) = term(1816) + s2(a,p,i,q) * t1(b,i) * wm_interm_42_triplet_pt3(b,a)
term(1817) = term(1817) + s2(a,p,i,q) * t1(b,i) * wm_interm_62_triplet_pt3(b,a)
term(1818) = term(1818) + s2(a,p,i,q) * t1(b,i) * wm_interm_63_triplet_pt3(b,a)
term(1819) = term(1819) + s2(a,p,i,q) * t1(b,i) * wm_interm_70_triplet_pt3(b,a)
term(1820) = term(1820) + s2(a,p,i,q) * t1(b,i) * wm_interm_71_triplet_pt3(b,a)
term(1821) = term(1821) + s1(a,i) * t2(b,p,i,q) * wm_interm_43_triplet_pt3(b,a)
term(1822) = term(1822) + s1(a,i) * t2(b,p,i,q) * wm_interm_44_triplet_pt3(b,a)
term(1823) = term(1823) + s1(a,i) * t2(b,p,i,q) * wm_interm_29_triplet_pt3(b,a)
term(1824) = term(1824) + s1(a,i) * t2(b,p,i,q) * wm_interm_27_triplet_pt3(b,a)
term(1825) = term(1825) + s1(a,i) * t2(b,p,i,q) * wm_interm_28_triplet_pt3(b,a)
term(1826) = term(1826) + s1(a,i) * t2(b,p,i,q) * wm_interm_42_triplet_pt3(b,a)
term(1827) = term(1827) + s1(a,i) * t2(b,p,i,q) * wm_interm_70_triplet_pt3(b,a)
term(1828) = term(1828) + s1(a,i) * t2(b,p,i,q) * wm_interm_71_triplet_pt3(b,a)
term(1829) = term(1829) + s1(a,i) * t2(b,p,i,q) * wm_interm_62_triplet_pt3(b,a)
term(1830) = term(1830) + s1(a,i) * t2(b,p,i,q) * wm_interm_63_triplet_pt3(b,a)
term(1831) = term(1831) + s2(a,p,i,q) * t1(b,i) * wm_interm_120_triplet_pt3(b,a)
term(1832) = term(1832) + s2(a,p,i,q) * t1(b,i) * wm_interm_121_triplet_pt3(b,a)
term(1833) = term(1833) + s2(a,p,i,q) * t1(b,i) * wm_interm_122_triplet_pt3(b,a)
term(1834) = term(1834) + s2(a,p,i,q) * t1(b,i) * wm_interm_129_triplet_pt3(b,a)
term(1835) = term(1835) + s2(a,p,i,q) * t1(b,i) * wm_interm_130_triplet_pt3(b,a)
term(1836) = term(1836) + s2(a,p,i,q) * t1(b,i) * wm_interm_128_triplet_pt3(b,a)
term(1837) = term(1837) + s2(a,p,i,q) * t1(b,i) * wm_interm_138_triplet_pt3(b,a)
term(1838) = term(1838) + s2(a,p,i,q) * t1(b,i) * wm_interm_139_triplet_pt3(b,a)
term(1839) = term(1839) + s2(a,p,i,q) * t1(b,i) * wm_interm_142_triplet_pt3(b,a)
term(1840) = term(1840) + s2(a,p,i,q) * t1(b,i) * wm_interm_143_triplet_pt3(b,a)
term(1841) = term(1841) + s1(a,i) * t2(b,p,i,q) * wm_interm_129_triplet_pt3(b,a)
term(1842) = term(1842) + s1(a,i) * t2(b,p,i,q) * wm_interm_130_triplet_pt3(b,a)
term(1843) = term(1843) + s1(a,i) * t2(b,p,i,q) * wm_interm_122_triplet_pt3(b,a)
term(1844) = term(1844) + s1(a,i) * t2(b,p,i,q) * wm_interm_120_triplet_pt3(b,a)
term(1845) = term(1845) + s1(a,i) * t2(b,p,i,q) * wm_interm_121_triplet_pt3(b,a)
term(1846) = term(1846) + s1(a,i) * t2(b,p,i,q) * wm_interm_128_triplet_pt3(b,a)
term(1847) = term(1847) + s1(a,i) * t2(b,p,i,q) * wm_interm_142_triplet_pt3(b,a)
term(1848) = term(1848) + s1(a,i) * t2(b,p,i,q) * wm_interm_143_triplet_pt3(b,a)
term(1849) = term(1849) + s1(a,i) * t2(b,p,i,q) * wm_interm_138_triplet_pt3(b,a)
term(1850) = term(1850) + s1(a,i) * t2(b,p,i,q) * wm_interm_139_triplet_pt3(b,a)
end do 
end do 
end do 

term(1812) = term(1812) * (-2.0d+0) 
term(1815) = term(1815) * (-2.0d+0) 
term(1817) = term(1817) * (4.0d+0) 
term(1818) = term(1818) * (-4.0d+0) 
term(1819) = term(1819) * (4.0d+0) 
term(1820) = term(1820) * (-4.0d+0) 
term(1822) = term(1822) * (-2.0d+0) 
term(1825) = term(1825) * (-2.0d+0) 
term(1827) = term(1827) * (4.0d+0) 
term(1828) = term(1828) * (-4.0d+0) 
term(1829) = term(1829) * (4.0d+0) 
term(1830) = term(1830) * (-4.0d+0) 
term(1831) = term(1831) * (2.0d+0) 
term(1832) = term(1832) * (-4.0d+0) 
term(1833) = term(1833) * (2.0d+0) 
term(1834) = term(1834) * (2.0d+0) 
term(1835) = term(1835) * (-4.0d+0) 
term(1836) = term(1836) * (2.0d+0) 
term(1837) = term(1837) * (8.0d+0) 
term(1838) = term(1838) * (-8.0d+0) 
term(1839) = term(1839) * (8.0d+0) 
term(1840) = term(1840) * (-8.0d+0) 
term(1841) = term(1841) * (2.0d+0) 
term(1842) = term(1842) * (-4.0d+0) 
term(1843) = term(1843) * (2.0d+0) 
term(1844) = term(1844) * (2.0d+0) 
term(1845) = term(1845) * (-4.0d+0) 
term(1846) = term(1846) * (2.0d+0) 
term(1847) = term(1847) * (8.0d+0) 
term(1848) = term(1848) * (-8.0d+0) 
term(1849) = term(1849) * (8.0d+0) 
term(1850) = term(1850) * (-8.0d+0) 

do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(1851) = term(1851) + r1(vrdav_Rr, a,i) * wm_interm_191_triplet_pt3(j,i,k,q) * wm_interm_95_triplet_pt3(a,p,k,j)
term(1852) = term(1852) + r1(vrdav_Rr, a,i) * wm_interm_191_triplet_pt3(j,i,k,q) * wm_interm_94_triplet_pt3(a,p,k,j)
term(1853) = term(1853) + r1(vrdav_Rr, a,i) * wm_interm_191_triplet_pt3(i,j,k,q) * wm_interm_93_triplet_pt3(a,p,k,j)
term(1854) = term(1854) + r1(vrdav_Rr, a,i) * wm_interm_191_triplet_pt3(i,j,k,q) * wm_interm_95_triplet_pt3(a,p,k,j)
term(1855) = term(1855) + r1(vrdav_Rr, a,i) * wm_interm_191_triplet_pt3(i,j,k,q) * wm_interm_94_triplet_pt3(a,p,k,j)
term(1856) = term(1856) + r1(vrdav_Rr, a,i) * wm_interm_191_triplet_pt3(j,i,k,q) * wm_interm_90_triplet_pt3(a,p,k,j)
term(1857) = term(1857) + r1(vrdav_Rr, a,i) * wm_interm_191_triplet_pt3(i,j,k,q) * wm_interm_90_triplet_pt3(a,p,k,j)
term(1858) = term(1858) + r1(vrdav_Rr, a,i) * wm_interm_191_triplet_pt3(j,i,k,q) * wm_interm_91_triplet_pt3(a,p,k,j)
term(1859) = term(1859) + r1(vrdav_Rr, a,i) * wm_interm_191_triplet_pt3(i,j,k,q) * wm_interm_91_triplet_pt3(a,p,k,j)
term(1860) = term(1860) + r1(vrdav_Rr, a,i) * wm_interm_191_triplet_pt3(j,i,k,q) * wm_interm_92_triplet_pt3(a,p,k,j)
term(1861) = term(1861) + r1(vrdav_Rr, a,i) * wm_interm_191_triplet_pt3(i,j,k,q) * wm_interm_92_triplet_pt3(a,p,k,j)
term(1862) = term(1862) + r1(vrdav_Rr, a,i) * wm_interm_154_triplet_pt3(j,i,k,q) * wm_interm_193_triplet_pt3(a,p,k,j)
term(1863) = term(1863) + r1(vrdav_Rr, a,i) * wm_interm_154_triplet_pt3(i,j,k,q) * wm_interm_193_triplet_pt3(a,p,k,j)
term(1864) = term(1864) + r1(vrdav_Rr, a,i) * wm_interm_154_triplet_pt3(j,i,k,q) * wm_interm_190_triplet_pt3(a,p,k,j)
term(1865) = term(1865) + r1(vrdav_Rr, a,i) * wm_interm_154_triplet_pt3(j,i,k,q) * wm_interm_194_triplet_pt3(a,p,k,j)
term(1866) = term(1866) + r1(vrdav_Rr, a,i) * wm_interm_154_triplet_pt3(i,j,k,q) * wm_interm_190_triplet_pt3(a,p,k,j)
term(1867) = term(1867) + r1(vrdav_Rr, a,i) * wm_interm_154_triplet_pt3(i,j,k,q) * wm_interm_194_triplet_pt3(a,p,k,j)
term(1868) = term(1868) + r1(vrdav_Rr, a,i) * wm_interm_154_triplet_pt3(i,j,k,q) * wm_interm_192_triplet_pt3(a,p,k,j)
end do 
end do 
end do 
end do 

term(1851) = term(1851) * (-2.0d+0) 
term(1852) = term(1852) * (4.0d+0) 
term(1853) = term(1853) * (-2.0d+0) 
term(1854) = term(1854) * (4.0d+0) 
term(1855) = term(1855) * (-2.0d+0) 
term(1856) = term(1856) * (-2.0d+0) 
term(1857) = term(1857) * (4.0d+0) 
term(1858) = term(1858) * (-2.0d+0) 
term(1859) = term(1859) * (4.0d+0) 
term(1860) = term(1860) * (4.0d+0) 
term(1861) = term(1861) * (-8.0d+0) 
term(1862) = term(1862) * (-4.0d+0) 
term(1863) = term(1863) * (8.0d+0) 
term(1864) = term(1864) * (-4.0d+0) 
term(1865) = term(1865) * (8.0d+0) 
term(1866) = term(1866) * (8.0d+0) 
term(1867) = term(1867) * (-16.0d+0) 
term(1868) = term(1868) * (-4.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(1869) = term(1869) + t2(a,p,j,i) * wm_interm_175_triplet_pt3(b,j,i,k) * wm_interm_193_triplet_pt3(a,b,k,q)
term(1870) = term(1870) + t2(a,p,j,i) * wm_interm_171_triplet_pt3(b,j,i,k) * wm_interm_193_triplet_pt3(a,b,k,q)
term(1871) = term(1871) + t2(a,p,j,i) * wm_interm_175_triplet_pt3(b,j,i,k) * wm_interm_194_triplet_pt3(a,b,k,q)
term(1872) = term(1872) + t2(a,p,j,i) * wm_interm_171_triplet_pt3(b,j,i,k) * wm_interm_194_triplet_pt3(a,b,k,q)
term(1873) = term(1873) + t2(a,p,j,i) * wm_interm_175_triplet_pt3(b,j,i,k) * wm_interm_190_triplet_pt3(a,b,k,q)
term(1874) = term(1874) + t2(a,p,j,i) * wm_interm_171_triplet_pt3(b,j,i,k) * wm_interm_190_triplet_pt3(a,b,k,q)
term(1875) = term(1875) + t2(a,p,j,i) * wm_interm_171_triplet_pt3(b,j,i,k) * wm_interm_192_triplet_pt3(a,b,k,q)
term(1876) = term(1876) + t2(a,p,j,i) * wm_interm_182_triplet_pt3(b,j,i,k) * wm_interm_193_triplet_pt3(a,b,k,q)
term(1877) = term(1877) + t2(a,p,j,i) * wm_interm_179_triplet_pt3(b,j,i,k) * wm_interm_193_triplet_pt3(a,b,k,q)
term(1878) = term(1878) + t2(a,p,j,i) * wm_interm_182_triplet_pt3(b,j,i,k) * wm_interm_194_triplet_pt3(a,b,k,q)
term(1879) = term(1879) + t2(a,p,j,i) * wm_interm_179_triplet_pt3(b,j,i,k) * wm_interm_194_triplet_pt3(a,b,k,q)
term(1880) = term(1880) + t2(a,p,j,i) * wm_interm_182_triplet_pt3(b,j,i,k) * wm_interm_190_triplet_pt3(a,b,k,q)
term(1881) = term(1881) + t2(a,p,j,i) * wm_interm_179_triplet_pt3(b,j,i,k) * wm_interm_190_triplet_pt3(a,b,k,q)
term(1882) = term(1882) + t2(a,p,j,i) * wm_interm_179_triplet_pt3(b,j,i,k) * wm_interm_192_triplet_pt3(a,b,k,q)
end do 
end do 
end do 
end do 
end do 

term(1869) = term(1869) * (-2.0d+0) 
term(1870) = term(1870) * (4.0d+0) 
term(1871) = term(1871) * (4.0d+0) 
term(1872) = term(1872) * (-8.0d+0) 
term(1873) = term(1873) * (-2.0d+0) 
term(1874) = term(1874) * (4.0d+0) 
term(1875) = term(1875) * (-2.0d+0) 
term(1876) = term(1876) * (-4.0d+0) 
term(1877) = term(1877) * (8.0d+0) 
term(1878) = term(1878) * (8.0d+0) 
term(1879) = term(1879) * (-16.0d+0) 
term(1880) = term(1880) * (-4.0d+0) 
term(1881) = term(1881) * (8.0d+0) 
term(1882) = term(1882) * (-4.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(1883) = term(1883) + r1(vrdav_Rr, p,i) * wm_interm_191_triplet_pt3(i,j,k,l) * wm_interm_99_triplet_pt3(k,l,j,q)
term(1884) = term(1884) + r1(vrdav_Rr, p,i) * wm_interm_154_triplet_pt3(j,k,q,l) * wm_interm_191_triplet_pt3(l,i,j,k)
term(1885) = term(1885) + r1(vrdav_Rr, p,i) * wm_interm_154_triplet_pt3(j,k,q,l) * wm_interm_191_triplet_pt3(i,l,j,k)
term(1886) = term(1886) + r1(vrdav_Rr, p,i) * wm_interm_154_triplet_pt3(j,i,k,l) * wm_interm_191_triplet_pt3(l,k,j,q)
term(1887) = term(1887) + r1(vrdav_Rr, p,i) * wm_interm_154_triplet_pt3(i,j,k,l) * wm_interm_191_triplet_pt3(l,k,j,q)
term(1888) = term(1888) + r1(vrdav_Rr, p,i) * wm_interm_154_triplet_pt3(i,j,k,l) * wm_interm_191_triplet_pt3(k,l,j,q)
end do 
end do 
end do 
end do 

term(1883) = term(1883) * (-1.0d+0) 
term(1884) = term(1884) * (-2.0d+0) 
term(1885) = term(1885) * (4.0d+0) 
term(1886) = term(1886) * (-2.0d+0) 
term(1887) = term(1887) * (4.0d+0) 
term(1888) = term(1888) * (-2.0d+0) 

do l = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
term(1889) = term(1889) + r1(vrdav_Rr, p,i) * wm_interm_191_triplet_pt3(j,i,k,l) * wm_interm_99_triplet_pt3(k,l,q,j)
term(1890) = term(1890) + r1(vrdav_Rr, p,i) * wm_interm_191_triplet_pt3(i,j,k,l) * wm_interm_99_triplet_pt3(k,l,q,j)
end do 
end do 
end do 
end do 

term(1889) = term(1889) * (-1.0d+0) 
term(1890) = term(1890) * (2.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(1891) = term(1891) + s2(a,p,q,i) * wm_interm_17_triplet_pt3(i,j) * wm_interm_186_triplet_pt3(a,j)
term(1892) = term(1892) + s2(a,p,q,i) * wm_interm_186_triplet_pt3(a,j) * wm_interm_18_triplet_pt3(i,j)
term(1893) = term(1893) + s2(a,p,q,i) * wm_interm_186_triplet_pt3(a,j) * wm_interm_19_triplet_pt3(i,j)
term(1894) = term(1894) + s2(a,p,q,i) * wm_interm_186_triplet_pt3(a,j) * wm_interm_54_triplet_pt3(i,j)
term(1895) = term(1895) + s2(a,p,q,i) * wm_interm_186_triplet_pt3(a,j) * wm_interm_55_triplet_pt3(i,j)
term(1896) = term(1896) + s2(a,p,q,i) * wm_interm_186_triplet_pt3(a,j) * wm_interm_56_triplet_pt3(i,j)
term(1897) = term(1897) + t2(a,p,q,i) * wm_interm_176_triplet_pt3(a,j) * wm_interm_196_triplet_pt3(i,j)
term(1898) = term(1898) + t2(a,p,q,i) * wm_interm_176_triplet_pt3(a,j) * wm_interm_197_triplet_pt3(i,j)
term(1899) = term(1899) + t2(a,p,q,i) * wm_interm_172_triplet_pt3(a,j) * wm_interm_196_triplet_pt3(i,j)
term(1900) = term(1900) + t2(a,p,q,i) * wm_interm_172_triplet_pt3(a,j) * wm_interm_197_triplet_pt3(i,j)
term(1901) = term(1901) + t2(a,p,q,i) * wm_interm_174_triplet_pt3(a,j) * wm_interm_196_triplet_pt3(i,j)
term(1902) = term(1902) + t2(a,p,q,i) * wm_interm_174_triplet_pt3(a,j) * wm_interm_197_triplet_pt3(i,j)
term(1903) = term(1903) + t2(a,p,q,i) * wm_interm_183_triplet_pt3(a,j) * wm_interm_75_triplet_pt3(i,j)
term(1904) = term(1904) + t2(a,p,q,i) * wm_interm_183_triplet_pt3(a,j) * wm_interm_79_triplet_pt3(i,j)
term(1905) = term(1905) + t2(a,p,q,i) * wm_interm_183_triplet_pt3(a,j) * wm_interm_81_triplet_pt3(i,j)
term(1906) = term(1906) + t2(a,p,j,i) * wm_interm_183_triplet_pt3(a,i) * wm_interm_75_triplet_pt3(j,q)
term(1907) = term(1907) + t2(a,p,j,i) * wm_interm_183_triplet_pt3(a,i) * wm_interm_79_triplet_pt3(j,q)
term(1908) = term(1908) + t2(a,p,j,i) * wm_interm_183_triplet_pt3(a,i) * wm_interm_81_triplet_pt3(j,q)
term(1909) = term(1909) + t2(a,p,j,i) * wm_interm_176_triplet_pt3(a,i) * wm_interm_196_triplet_pt3(j,q)
term(1910) = term(1910) + t2(a,p,j,i) * wm_interm_174_triplet_pt3(a,i) * wm_interm_196_triplet_pt3(j,q)
term(1911) = term(1911) + t2(a,p,j,i) * wm_interm_172_triplet_pt3(a,i) * wm_interm_196_triplet_pt3(j,q)
term(1912) = term(1912) + t2(a,p,j,i) * wm_interm_176_triplet_pt3(a,i) * wm_interm_197_triplet_pt3(j,q)
term(1913) = term(1913) + t2(a,p,j,i) * wm_interm_174_triplet_pt3(a,i) * wm_interm_197_triplet_pt3(j,q)
term(1914) = term(1914) + t2(a,p,j,i) * wm_interm_172_triplet_pt3(a,i) * wm_interm_197_triplet_pt3(j,q)
term(1915) = term(1915) + t2(a,p,j,i) * wm_interm_176_triplet_pt3(a,j) * wm_interm_196_triplet_pt3(i,q)
term(1916) = term(1916) + t2(a,p,j,i) * wm_interm_174_triplet_pt3(a,j) * wm_interm_196_triplet_pt3(i,q)
term(1917) = term(1917) + t2(a,p,j,i) * wm_interm_172_triplet_pt3(a,j) * wm_interm_196_triplet_pt3(i,q)
term(1918) = term(1918) + t2(a,p,j,i) * wm_interm_176_triplet_pt3(a,j) * wm_interm_197_triplet_pt3(i,q)
term(1919) = term(1919) + t2(a,p,j,i) * wm_interm_174_triplet_pt3(a,j) * wm_interm_197_triplet_pt3(i,q)
term(1920) = term(1920) + t2(a,p,j,i) * wm_interm_172_triplet_pt3(a,j) * wm_interm_197_triplet_pt3(i,q)
term(1921) = term(1921) + t2(a,p,q,i) * wm_interm_181_triplet_pt3(a,j) * wm_interm_196_triplet_pt3(i,j)
term(1922) = term(1922) + t2(a,p,q,i) * wm_interm_181_triplet_pt3(a,j) * wm_interm_197_triplet_pt3(i,j)
term(1923) = term(1923) + t2(a,p,q,i) * wm_interm_180_triplet_pt3(a,j) * wm_interm_196_triplet_pt3(i,j)
term(1924) = term(1924) + t2(a,p,q,i) * wm_interm_180_triplet_pt3(a,j) * wm_interm_197_triplet_pt3(i,j)
term(1925) = term(1925) + t2(a,p,q,i) * wm_interm_145_triplet_pt3(i,j) * wm_interm_183_triplet_pt3(a,j)
term(1926) = term(1926) + t2(a,p,q,i) * wm_interm_147_triplet_pt3(i,j) * wm_interm_183_triplet_pt3(a,j)
term(1927) = term(1927) + t2(a,p,j,i) * wm_interm_145_triplet_pt3(j,q) * wm_interm_183_triplet_pt3(a,i)
term(1928) = term(1928) + t2(a,p,j,i) * wm_interm_147_triplet_pt3(j,q) * wm_interm_183_triplet_pt3(a,i)
term(1929) = term(1929) + t2(a,p,j,i) * wm_interm_181_triplet_pt3(a,i) * wm_interm_196_triplet_pt3(j,q)
term(1930) = term(1930) + t2(a,p,j,i) * wm_interm_180_triplet_pt3(a,i) * wm_interm_196_triplet_pt3(j,q)
term(1931) = term(1931) + t2(a,p,j,i) * wm_interm_181_triplet_pt3(a,i) * wm_interm_197_triplet_pt3(j,q)
term(1932) = term(1932) + t2(a,p,j,i) * wm_interm_180_triplet_pt3(a,i) * wm_interm_197_triplet_pt3(j,q)
term(1933) = term(1933) + t2(a,p,j,i) * wm_interm_181_triplet_pt3(a,j) * wm_interm_196_triplet_pt3(i,q)
term(1934) = term(1934) + t2(a,p,j,i) * wm_interm_180_triplet_pt3(a,j) * wm_interm_196_triplet_pt3(i,q)
term(1935) = term(1935) + t2(a,p,j,i) * wm_interm_181_triplet_pt3(a,j) * wm_interm_197_triplet_pt3(i,q)
term(1936) = term(1936) + t2(a,p,j,i) * wm_interm_180_triplet_pt3(a,j) * wm_interm_197_triplet_pt3(i,q)
end do 
end do 
end do 

term(1891) = term(1891) * (2.0d+0) 
term(1892) = term(1892) * (-4.0d+0) 
term(1893) = term(1893) * (2.0d+0) 
term(1894) = term(1894) * (4.0d+0) 
term(1895) = term(1895) * (-8.0d+0) 
term(1896) = term(1896) * (4.0d+0) 
term(1897) = term(1897) * (-1.0d+0) 
term(1898) = term(1898) * (2.0d+0) 
term(1899) = term(1899) * (2.0d+0) 
term(1900) = term(1900) * (-4.0d+0) 
term(1901) = term(1901) * (-1.0d+0) 
term(1902) = term(1902) * (2.0d+0) 
term(1903) = term(1903) * (-1.0d+0) 
term(1904) = term(1904) * (2.0d+0) 
term(1905) = term(1905) * (-1.0d+0) 
term(1906) = term(1906) * (-2.0d+0) 
term(1907) = term(1907) * (4.0d+0) 
term(1908) = term(1908) * (-2.0d+0) 
term(1909) = term(1909) * (-2.0d+0) 
term(1910) = term(1910) * (-2.0d+0) 
term(1911) = term(1911) * (4.0d+0) 
term(1912) = term(1912) * (4.0d+0) 
term(1913) = term(1913) * (4.0d+0) 
term(1914) = term(1914) * (-8.0d+0) 
term(1915) = term(1915) * (4.0d+0) 
term(1916) = term(1916) * (4.0d+0) 
term(1917) = term(1917) * (-8.0d+0) 
term(1918) = term(1918) * (-8.0d+0) 
term(1919) = term(1919) * (-8.0d+0) 
term(1920) = term(1920) * (16.0d+0) 
term(1921) = term(1921) * (-4.0d+0) 
term(1922) = term(1922) * (8.0d+0) 
term(1923) = term(1923) * (4.0d+0) 
term(1924) = term(1924) * (-8.0d+0) 
term(1925) = term(1925) * (-4.0d+0) 
term(1926) = term(1926) * (4.0d+0) 
term(1927) = term(1927) * (-8.0d+0) 
term(1928) = term(1928) * (8.0d+0) 
term(1929) = term(1929) * (-8.0d+0) 
term(1930) = term(1930) * (8.0d+0) 
term(1931) = term(1931) * (16.0d+0) 
term(1932) = term(1932) * (-16.0d+0) 
term(1933) = term(1933) * (16.0d+0) 
term(1934) = term(1934) * (-16.0d+0) 
term(1935) = term(1935) * (-32.0d+0) 
term(1936) = term(1936) * (32.0d+0) 

do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(1937) = term(1937) + s2(a,b,i,q) * wm_interm_186_triplet_pt3(b,j) * wm_interm_30_triplet_pt3(a,p,i,j)
term(1938) = term(1938) + s2(a,b,i,q) * wm_interm_186_triplet_pt3(b,j) * wm_interm_25_triplet_pt3(a,p,i,j)
term(1939) = term(1939) + s2(a,b,i,q) * wm_interm_186_triplet_pt3(b,j) * wm_interm_24_triplet_pt3(a,p,i,j)
term(1940) = term(1940) + s2(a,b,i,q) * wm_interm_186_triplet_pt3(b,j) * wm_interm_61_triplet_pt3(a,p,i,j)
term(1941) = term(1941) + s2(a,b,i,q) * wm_interm_186_triplet_pt3(b,j) * wm_interm_60_triplet_pt3(a,p,i,j)
term(1942) = term(1942) + s2(a,b,i,j) * wm_interm_186_triplet_pt3(b,i) * wm_interm_30_triplet_pt3(a,p,j,q)
term(1943) = term(1943) + s2(a,b,i,j) * wm_interm_186_triplet_pt3(b,i) * wm_interm_24_triplet_pt3(a,p,j,q)
term(1944) = term(1944) + s2(a,b,i,j) * wm_interm_186_triplet_pt3(b,i) * wm_interm_23_triplet_pt3(a,p,j,q)
term(1945) = term(1945) + s2(a,b,i,j) * wm_interm_186_triplet_pt3(b,i) * wm_interm_26_triplet_pt3(a,p,j,q)
term(1946) = term(1946) + s2(a,b,i,j) * wm_interm_186_triplet_pt3(b,i) * wm_interm_21_triplet_pt3(a,p,j,q)
term(1947) = term(1947) + s2(a,b,i,j) * wm_interm_186_triplet_pt3(b,i) * wm_interm_22_triplet_pt3(a,p,j,q)
term(1948) = term(1948) + s2(a,b,i,j) * wm_interm_186_triplet_pt3(b,i) * wm_interm_61_triplet_pt3(a,p,j,q)
term(1949) = term(1949) + s2(a,b,i,j) * wm_interm_186_triplet_pt3(b,i) * wm_interm_60_triplet_pt3(a,p,j,q)
term(1950) = term(1950) + s2(a,b,i,j) * wm_interm_186_triplet_pt3(b,i) * wm_interm_58_triplet_pt3(a,p,j,q)
term(1951) = term(1951) + s2(a,b,i,j) * wm_interm_186_triplet_pt3(b,i) * wm_interm_59_triplet_pt3(a,p,j,q)
term(1952) = term(1952) + t2(a,b,i,j) * wm_interm_183_triplet_pt3(a,j) * wm_interm_90_triplet_pt3(b,p,i,q)
term(1953) = term(1953) + t2(a,b,i,j) * wm_interm_183_triplet_pt3(a,j) * wm_interm_93_triplet_pt3(b,p,i,q)
term(1954) = term(1954) + t2(a,b,i,j) * wm_interm_150_triplet_pt3(b,p,i,q) * wm_interm_183_triplet_pt3(a,j)
term(1955) = term(1955) + t2(a,b,i,j) * wm_interm_152_triplet_pt3(b,p,i,q) * wm_interm_183_triplet_pt3(a,j)
end do 
end do 
end do 
end do 

term(1937) = term(1937) * (2.0d+0) 
term(1938) = term(1938) * (-4.0d+0) 
term(1939) = term(1939) * (2.0d+0) 
term(1940) = term(1940) * (8.0d+0) 
term(1941) = term(1941) * (-8.0d+0) 
term(1942) = term(1942) * (-1.0d+0) 
term(1943) = term(1943) * (-1.0d+0) 
term(1944) = term(1944) * (2.0d+0) 
term(1945) = term(1945) * (2.0d+0) 
term(1946) = term(1946) * (2.0d+0) 
term(1947) = term(1947) * (-4.0d+0) 
term(1948) = term(1948) * (-4.0d+0) 
term(1949) = term(1949) * (4.0d+0) 
term(1950) = term(1950) * (8.0d+0) 
term(1951) = term(1951) * (-8.0d+0) 
term(1952) = term(1952) * (2.0d+0) 
term(1953) = term(1953) * (-1.0d+0) 
term(1954) = term(1954) * (4.0d+0) 
term(1955) = term(1955) * (-2.0d+0) 

do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
do i = 1, nocc 
term(1956) = term(1956) + r2p(vrdav_Rl, p,i,a,q) * t1(b,i) * wm_interm_5_triplet_pt3(a,b)
term(1957) = term(1957) + r2p(vrdav_Rl, p,i,a,q) * t1(b,i) * wm_interm_6_triplet_pt3(a,b)
term(1958) = term(1958) + r2p(vrdav_Rl, p,i,a,q) * t1(b,i) * wm_interm_9_triplet_pt3(a,b)
term(1959) = term(1959) + r2p(vrdav_Rl, p,i,a,q) * t1(b,i) * wm_interm_47_triplet_pt3(a,b)
term(1960) = term(1960) + r2p(vrdav_Rl, p,i,a,q) * t1(b,i) * wm_interm_48_triplet_pt3(a,b)
term(1961) = term(1961) + r2p(vrdav_Rr, p,i,a,q) * s1(b,i) * wm_interm_74_triplet_pt3(a,b)
term(1962) = term(1962) + r2p(vrdav_Rr, p,i,a,q) * s1(b,i) * wm_interm_78_triplet_pt3(a,b)
term(1963) = term(1963) + r2p(vrdav_Rr, p,i,a,q) * s1(b,i) * wm_interm_80_triplet_pt3(a,b)
term(1964) = term(1964) + r2p(vrdav_Rr, p,i,a,q) * s1(b,i) * wm_interm_144_triplet_pt3(a,b)
term(1965) = term(1965) + r2p(vrdav_Rr, p,i,a,q) * s1(b,i) * wm_interm_146_triplet_pt3(a,b)
end do 
end do 
end do 

term(1956) = term(1956) * (-0.5d+0) 
term(1958) = term(1958) * (-0.5d+0) 
term(1959) = term(1959) * (-2.0d+0) 
term(1960) = term(1960) * (2.0d+0) 
term(1961) = term(1961) * (-0.5d+0) 
term(1963) = term(1963) * (-0.5d+0) 
term(1964) = term(1964) * (-2.0d+0) 
term(1965) = term(1965) * (2.0d+0) 

do j = 1, nocc 
do l = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
term(1966) = term(1966) + r1(vrdav_Rr, p,i) * wm_interm_191_triplet_pt3(j,k,l,q) * wm_interm_99_triplet_pt3(l,i,k,j)
term(1967) = term(1967) + r1(vrdav_Rr, p,i) * wm_interm_191_triplet_pt3(j,k,l,q) * wm_interm_99_triplet_pt3(i,l,k,j)
end do 
end do 
end do 
end do 

term(1966) = term(1966) * (-1.0d+0) 
term(1967) = term(1967) * (2.0d+0) 

do k = 1, nocc 
do l = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(1968) = term(1968) + r1(vrdav_Rr, p,i) * wm_interm_191_triplet_pt3(j,k,l,q) * wm_interm_99_triplet_pt3(i,l,j,k)
term(1969) = term(1969) + r1(vrdav_Rr, p,i) * wm_interm_154_triplet_pt3(j,k,l,q) * wm_interm_191_triplet_pt3(i,l,j,k)
end do 
end do 
end do 
end do 

term(1968) = term(1968) * (-1.0d+0) 
term(1969) = term(1969) * (-2.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(1970) = term(1970) + s2(a,p,j,i) * wm_interm_186_triplet_pt3(a,k) * wm_interm_20_triplet_pt3(i,j,k,q)
term(1971) = term(1971) + s2(a,p,j,i) * wm_interm_186_triplet_pt3(a,k) * wm_interm_20_triplet_pt3(j,i,k,q)
term(1972) = term(1972) + s2(a,p,j,i) * wm_interm_186_triplet_pt3(a,k) * wm_interm_57_triplet_pt3(i,j,k,q)
term(1973) = term(1973) + s2(a,p,j,i) * wm_interm_186_triplet_pt3(a,k) * wm_interm_57_triplet_pt3(j,i,k,q)
term(1974) = term(1974) + t2(a,p,j,i) * wm_interm_183_triplet_pt3(a,k) * wm_interm_99_triplet_pt3(j,i,k,q)
term(1975) = term(1975) + t2(a,p,j,i) * wm_interm_183_triplet_pt3(a,k) * wm_interm_99_triplet_pt3(i,j,k,q)
term(1976) = term(1976) + t2(a,p,j,i) * wm_interm_176_triplet_pt3(a,k) * wm_interm_191_triplet_pt3(j,i,k,q)
term(1977) = term(1977) + t2(a,p,j,i) * wm_interm_174_triplet_pt3(a,k) * wm_interm_191_triplet_pt3(j,i,k,q)
term(1978) = term(1978) + t2(a,p,j,i) * wm_interm_172_triplet_pt3(a,k) * wm_interm_191_triplet_pt3(j,i,k,q)
term(1979) = term(1979) + t2(a,p,j,i) * wm_interm_174_triplet_pt3(a,k) * wm_interm_191_triplet_pt3(i,j,k,q)
term(1980) = term(1980) + t2(a,p,j,i) * wm_interm_176_triplet_pt3(a,k) * wm_interm_191_triplet_pt3(i,j,k,q)
term(1981) = term(1981) + t2(a,p,j,i) * wm_interm_172_triplet_pt3(a,k) * wm_interm_191_triplet_pt3(i,j,k,q)
term(1982) = term(1982) + t2(a,p,j,i) * wm_interm_154_triplet_pt3(j,i,k,q) * wm_interm_183_triplet_pt3(a,k)
term(1983) = term(1983) + t2(a,p,j,i) * wm_interm_154_triplet_pt3(i,j,k,q) * wm_interm_183_triplet_pt3(a,k)
term(1984) = term(1984) + t2(a,p,j,i) * wm_interm_181_triplet_pt3(a,k) * wm_interm_191_triplet_pt3(j,i,k,q)
term(1985) = term(1985) + t2(a,p,j,i) * wm_interm_180_triplet_pt3(a,k) * wm_interm_191_triplet_pt3(j,i,k,q)
term(1986) = term(1986) + t2(a,p,j,i) * wm_interm_181_triplet_pt3(a,k) * wm_interm_191_triplet_pt3(i,j,k,q)
term(1987) = term(1987) + t2(a,p,j,i) * wm_interm_180_triplet_pt3(a,k) * wm_interm_191_triplet_pt3(i,j,k,q)
end do 
end do 
end do 
end do 

term(1970) = term(1970) * (2.0d+0) 
term(1971) = term(1971) * (-4.0d+0) 
term(1972) = term(1972) * (4.0d+0) 
term(1973) = term(1973) * (-8.0d+0) 
term(1974) = term(1974) * (4.0d+0) 
term(1975) = term(1975) * (-2.0d+0) 
term(1976) = term(1976) * (4.0d+0) 
term(1977) = term(1977) * (4.0d+0) 
term(1978) = term(1978) * (-8.0d+0) 
term(1979) = term(1979) * (-2.0d+0) 
term(1980) = term(1980) * (-2.0d+0) 
term(1981) = term(1981) * (4.0d+0) 
term(1982) = term(1982) * (8.0d+0) 
term(1983) = term(1983) * (-4.0d+0) 
term(1984) = term(1984) * (16.0d+0) 
term(1985) = term(1985) * (-16.0d+0) 
term(1986) = term(1986) * (-8.0d+0) 
term(1987) = term(1987) * (8.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(1988) = term(1988) + s2(a,p,j,i) * wm_interm_186_triplet_pt3(a,k) * wm_interm_20_triplet_pt3(j,i,q,k)
term(1989) = term(1989) + s2(a,p,j,i) * wm_interm_186_triplet_pt3(a,k) * wm_interm_57_triplet_pt3(j,i,q,k)
term(1990) = term(1990) + t2(a,p,j,i) * wm_interm_183_triplet_pt3(a,k) * wm_interm_99_triplet_pt3(j,i,q,k)
term(1991) = term(1991) + t2(a,p,j,i) * wm_interm_154_triplet_pt3(j,i,q,k) * wm_interm_183_triplet_pt3(a,k)
end do 
end do 
end do 
end do 

term(1988) = term(1988) * (2.0d+0) 
term(1989) = term(1989) * (4.0d+0) 
term(1990) = term(1990) * (-2.0d+0) 
term(1991) = term(1991) * (-4.0d+0) 


    calc_D_vo_wm_triplet_pt3 = zero
    do s = 0, 1991
    calc_D_vo_wm_triplet_pt3 = calc_D_vo_wm_triplet_pt3 + term(s)
    end do

    end function calc_D_vo_wm_triplet_pt3
    
    function calc_D_vv_wm_triplet_pt3(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vv_wm_triplet_pt3
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
    real(F64), dimension(0:101) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(0) = term(0) + wm_interm_161_triplet_pt3(p,i,j,k) * wm_interm_1_triplet_pt3(q,k,j,i)
term(1) = term(1) + wm_interm_162_triplet_pt3(p,i,j,k) * wm_interm_1_triplet_pt3(q,j,k,i)
term(2) = term(2) + wm_interm_162_triplet_pt3(p,i,j,k) * wm_interm_1_triplet_pt3(q,k,j,i)
term(3) = term(3) + wm_interm_164_triplet_pt3(p,i,j,k) * wm_interm_1_triplet_pt3(q,j,k,i)
term(4) = term(4) + wm_interm_164_triplet_pt3(p,i,j,k) * wm_interm_1_triplet_pt3(q,k,j,i)
term(5) = term(5) + wm_interm_100_triplet_pt3(q,i,j,k) * wm_interm_175_triplet_pt3(p,j,k,i)
term(6) = term(6) + wm_interm_100_triplet_pt3(q,i,j,k) * wm_interm_171_triplet_pt3(p,j,k,i)
term(7) = term(7) + wm_interm_100_triplet_pt3(q,i,j,k) * wm_interm_171_triplet_pt3(p,k,j,i)
term(8) = term(8) + wm_interm_100_triplet_pt3(q,i,j,k) * wm_interm_182_triplet_pt3(p,j,k,i)
term(9) = term(9) + wm_interm_100_triplet_pt3(q,i,j,k) * wm_interm_179_triplet_pt3(p,j,k,i)
term(10) = term(10) + wm_interm_100_triplet_pt3(q,i,j,k) * wm_interm_179_triplet_pt3(p,k,j,i)
end do 
end do 
end do 

term(2) = term(2) * (-2.0d+0) 
term(3) = term(3) * (4.0d+0) 
term(4) = term(4) * (-4.0d+0) 
term(6) = term(6) * (-2.0d+0) 
term(8) = term(8) * (2.0d+0) 
term(9) = term(9) * (-4.0d+0) 
term(10) = term(10) * (2.0d+0) 

do a = nocc + 1, nactive 
term(11) = term(11) + wm_interm_160_triplet_pt3(q,a) * wm_interm_5_triplet_pt3(p,a)
term(12) = term(12) + wm_interm_160_triplet_pt3(q,a) * wm_interm_6_triplet_pt3(p,a)
term(13) = term(13) + wm_interm_160_triplet_pt3(q,a) * wm_interm_9_triplet_pt3(p,a)
term(14) = term(14) + wm_interm_160_triplet_pt3(q,a) * wm_interm_47_triplet_pt3(p,a)
term(15) = term(15) + wm_interm_160_triplet_pt3(q,a) * wm_interm_48_triplet_pt3(p,a)
term(16) = term(16) + wm_interm_160_triplet_pt3(a,q) * wm_interm_9_triplet_pt3(a,p)
term(17) = term(17) + wm_interm_160_triplet_pt3(a,q) * wm_interm_5_triplet_pt3(a,p)
term(18) = term(18) + wm_interm_160_triplet_pt3(a,q) * wm_interm_6_triplet_pt3(a,p)
term(19) = term(19) + wm_interm_160_triplet_pt3(a,q) * wm_interm_47_triplet_pt3(a,p)
term(20) = term(20) + wm_interm_160_triplet_pt3(a,q) * wm_interm_48_triplet_pt3(a,p)
term(21) = term(21) + wm_interm_177_triplet_pt3(p,a) * wm_interm_74_triplet_pt3(q,a)
term(22) = term(22) + wm_interm_177_triplet_pt3(p,a) * wm_interm_78_triplet_pt3(q,a)
term(23) = term(23) + wm_interm_177_triplet_pt3(p,a) * wm_interm_80_triplet_pt3(q,a)
term(24) = term(24) + wm_interm_177_triplet_pt3(a,p) * wm_interm_80_triplet_pt3(a,q)
term(25) = term(25) + wm_interm_177_triplet_pt3(a,p) * wm_interm_74_triplet_pt3(a,q)
term(26) = term(26) + wm_interm_177_triplet_pt3(a,p) * wm_interm_78_triplet_pt3(a,q)
term(27) = term(27) + wm_interm_144_triplet_pt3(q,a) * wm_interm_177_triplet_pt3(p,a)
term(28) = term(28) + wm_interm_146_triplet_pt3(q,a) * wm_interm_177_triplet_pt3(p,a)
term(29) = term(29) + wm_interm_144_triplet_pt3(a,q) * wm_interm_177_triplet_pt3(a,p)
term(30) = term(30) + wm_interm_146_triplet_pt3(a,q) * wm_interm_177_triplet_pt3(a,p)
end do 

term(12) = term(12) * (-2.0d+0) 
term(14) = term(14) * (4.0d+0) 
term(15) = term(15) * (-4.0d+0) 
term(18) = term(18) * (-2.0d+0) 
term(19) = term(19) * (4.0d+0) 
term(20) = term(20) * (-4.0d+0) 
term(22) = term(22) * (-2.0d+0) 
term(26) = term(26) * (-2.0d+0) 
term(27) = term(27) * (4.0d+0) 
term(28) = term(28) * (-4.0d+0) 
term(29) = term(29) * (4.0d+0) 
term(30) = term(30) * (-4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(31) = term(31) + wm_interm_168_triplet_pt3(a,q,i,j) * wm_interm_30_triplet_pt3(a,p,i,j)
term(32) = term(32) + wm_interm_168_triplet_pt3(a,q,i,j) * wm_interm_24_triplet_pt3(a,p,i,j)
term(33) = term(33) + wm_interm_168_triplet_pt3(a,q,i,j) * wm_interm_23_triplet_pt3(a,p,i,j)
term(34) = term(34) + wm_interm_168_triplet_pt3(a,q,i,j) * wm_interm_26_triplet_pt3(a,p,i,j)
term(35) = term(35) + wm_interm_168_triplet_pt3(a,q,i,j) * wm_interm_21_triplet_pt3(a,p,i,j)
term(36) = term(36) + wm_interm_168_triplet_pt3(a,q,i,j) * wm_interm_22_triplet_pt3(a,p,i,j)
term(37) = term(37) + wm_interm_168_triplet_pt3(a,q,i,j) * wm_interm_61_triplet_pt3(a,p,i,j)
term(38) = term(38) + wm_interm_168_triplet_pt3(a,q,i,j) * wm_interm_60_triplet_pt3(a,p,i,j)
term(39) = term(39) + wm_interm_168_triplet_pt3(a,q,i,j) * wm_interm_58_triplet_pt3(a,p,i,j)
term(40) = term(40) + wm_interm_168_triplet_pt3(a,q,i,j) * wm_interm_59_triplet_pt3(a,p,i,j)
term(41) = term(41) + wm_interm_173_triplet_pt3(p,a,i,j) * wm_interm_92_triplet_pt3(q,a,i,j)
term(42) = term(42) + wm_interm_173_triplet_pt3(p,a,i,j) * wm_interm_91_triplet_pt3(q,a,i,j)
term(43) = term(43) + wm_interm_173_triplet_pt3(p,a,i,j) * wm_interm_90_triplet_pt3(q,a,i,j)
term(44) = term(44) + wm_interm_173_triplet_pt3(p,a,i,j) * wm_interm_93_triplet_pt3(q,a,i,j)
term(45) = term(45) + wm_interm_173_triplet_pt3(p,a,i,j) * wm_interm_95_triplet_pt3(q,a,i,j)
term(46) = term(46) + wm_interm_173_triplet_pt3(p,a,i,j) * wm_interm_94_triplet_pt3(q,a,i,j)
term(47) = term(47) + wm_interm_173_triplet_pt3(a,p,i,j) * wm_interm_93_triplet_pt3(a,q,i,j)
term(48) = term(48) + wm_interm_173_triplet_pt3(a,p,i,j) * wm_interm_94_triplet_pt3(a,q,i,j)
term(49) = term(49) + wm_interm_173_triplet_pt3(a,p,i,j) * wm_interm_95_triplet_pt3(a,q,i,j)
term(50) = term(50) + wm_interm_173_triplet_pt3(a,p,i,j) * wm_interm_90_triplet_pt3(a,q,i,j)
term(51) = term(51) + wm_interm_173_triplet_pt3(a,p,i,j) * wm_interm_91_triplet_pt3(a,q,i,j)
term(52) = term(52) + wm_interm_173_triplet_pt3(a,p,i,j) * wm_interm_92_triplet_pt3(a,q,i,j)
term(53) = term(53) + wm_interm_151_triplet_pt3(q,a,i,j) * wm_interm_173_triplet_pt3(p,a,i,j)
term(54) = term(54) + wm_interm_150_triplet_pt3(q,a,i,j) * wm_interm_173_triplet_pt3(p,a,i,j)
term(55) = term(55) + wm_interm_152_triplet_pt3(q,a,i,j) * wm_interm_173_triplet_pt3(p,a,i,j)
term(56) = term(56) + wm_interm_153_triplet_pt3(q,a,i,j) * wm_interm_173_triplet_pt3(p,a,i,j)
term(57) = term(57) + wm_interm_152_triplet_pt3(a,q,i,j) * wm_interm_173_triplet_pt3(a,p,i,j)
term(58) = term(58) + wm_interm_153_triplet_pt3(a,q,i,j) * wm_interm_173_triplet_pt3(a,p,i,j)
term(59) = term(59) + wm_interm_150_triplet_pt3(a,q,i,j) * wm_interm_173_triplet_pt3(a,p,i,j)
term(60) = term(60) + wm_interm_151_triplet_pt3(a,q,i,j) * wm_interm_173_triplet_pt3(a,p,i,j)
end do 
end do 
end do 

term(33) = term(33) * (-2.0d+0) 
term(34) = term(34) * (-2.0d+0) 
term(35) = term(35) * (-2.0d+0) 
term(36) = term(36) * (4.0d+0) 
term(37) = term(37) * (4.0d+0) 
term(38) = term(38) * (-4.0d+0) 
term(39) = term(39) * (-8.0d+0) 
term(40) = term(40) * (8.0d+0) 
term(41) = term(41) * (4.0d+0) 
term(42) = term(42) * (-2.0d+0) 
term(43) = term(43) * (-2.0d+0) 
term(45) = term(45) * (-2.0d+0) 
term(49) = term(49) * (-2.0d+0) 
term(50) = term(50) * (-2.0d+0) 
term(51) = term(51) * (-2.0d+0) 
term(52) = term(52) * (4.0d+0) 
term(53) = term(53) * (8.0d+0) 
term(54) = term(54) * (-8.0d+0) 
term(55) = term(55) * (4.0d+0) 
term(56) = term(56) * (-4.0d+0) 
term(57) = term(57) * (4.0d+0) 
term(58) = term(58) * (-4.0d+0) 
term(59) = term(59) * (-8.0d+0) 
term(60) = term(60) * (8.0d+0) 

do i = 1, nocc 
term(61) = term(61) + wm_interm_174_triplet_pt3(p,i) * wm_interm_73_triplet_pt3(q,i)
term(62) = term(62) + wm_interm_176_triplet_pt3(p,i) * wm_interm_73_triplet_pt3(q,i)
term(63) = term(63) + wm_interm_176_triplet_pt3(p,i) * wm_interm_72_triplet_pt3(q,i)
term(64) = term(64) + wm_interm_174_triplet_pt3(p,i) * wm_interm_72_triplet_pt3(q,i)
term(65) = term(65) + wm_interm_172_triplet_pt3(p,i) * wm_interm_73_triplet_pt3(q,i)
term(66) = term(66) + wm_interm_172_triplet_pt3(p,i) * wm_interm_72_triplet_pt3(q,i)
term(67) = term(67) + wm_interm_181_triplet_pt3(p,i) * wm_interm_73_triplet_pt3(q,i)
term(68) = term(68) + wm_interm_181_triplet_pt3(p,i) * wm_interm_72_triplet_pt3(q,i)
term(69) = term(69) + wm_interm_180_triplet_pt3(p,i) * wm_interm_73_triplet_pt3(q,i)
term(70) = term(70) + wm_interm_180_triplet_pt3(p,i) * wm_interm_72_triplet_pt3(q,i)
end do 

term(63) = term(63) * (-2.0d+0) 
term(64) = term(64) * (-2.0d+0) 
term(65) = term(65) * (-2.0d+0) 
term(66) = term(66) * (4.0d+0) 
term(67) = term(67) * (4.0d+0) 
term(68) = term(68) * (-8.0d+0) 
term(69) = term(69) * (-4.0d+0) 
term(70) = term(70) * (8.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
term(71) = term(71) + wm_interm_163_triplet_pt3(i,j) * wm_interm_21_triplet_pt3(p,q,i,j)
term(72) = term(72) + wm_interm_163_triplet_pt3(i,j) * wm_interm_22_triplet_pt3(p,q,i,j)
term(73) = term(73) + wm_interm_163_triplet_pt3(i,j) * wm_interm_23_triplet_pt3(p,q,i,j)
term(74) = term(74) + wm_interm_163_triplet_pt3(i,j) * wm_interm_25_triplet_pt3(p,q,i,j)
term(75) = term(75) + wm_interm_163_triplet_pt3(i,j) * wm_interm_30_triplet_pt3(p,q,i,j)
term(76) = term(76) + wm_interm_163_triplet_pt3(i,j) * wm_interm_26_triplet_pt3(p,q,i,j)
term(77) = term(77) + wm_interm_163_triplet_pt3(i,j) * wm_interm_58_triplet_pt3(p,q,i,j)
term(78) = term(78) + wm_interm_163_triplet_pt3(i,j) * wm_interm_59_triplet_pt3(p,q,i,j)
term(79) = term(79) + wm_interm_163_triplet_pt3(i,j) * wm_interm_60_triplet_pt3(p,q,i,j)
term(80) = term(80) + wm_interm_163_triplet_pt3(i,j) * wm_interm_61_triplet_pt3(p,q,i,j)
term(81) = term(81) + wm_interm_168_triplet_pt3(p,q,i,j) * wm_interm_17_triplet_pt3(i,j)
term(82) = term(82) + wm_interm_168_triplet_pt3(p,q,i,j) * wm_interm_18_triplet_pt3(i,j)
term(83) = term(83) + wm_interm_168_triplet_pt3(p,q,i,j) * wm_interm_19_triplet_pt3(i,j)
term(84) = term(84) + wm_interm_168_triplet_pt3(p,q,i,j) * wm_interm_54_triplet_pt3(i,j)
term(85) = term(85) + wm_interm_168_triplet_pt3(p,q,i,j) * wm_interm_55_triplet_pt3(i,j)
term(86) = term(86) + wm_interm_168_triplet_pt3(p,q,i,j) * wm_interm_56_triplet_pt3(i,j)
term(87) = term(87) + wm_interm_173_triplet_pt3(p,q,i,j) * wm_interm_75_triplet_pt3(i,j)
term(88) = term(88) + wm_interm_173_triplet_pt3(p,q,i,j) * wm_interm_79_triplet_pt3(i,j)
term(89) = term(89) + wm_interm_173_triplet_pt3(p,q,i,j) * wm_interm_81_triplet_pt3(i,j)
term(90) = term(90) + wm_interm_178_triplet_pt3(i,j) * wm_interm_95_triplet_pt3(p,q,i,j)
term(91) = term(91) + wm_interm_104_triplet_pt3(p,q,i,j) * wm_interm_178_triplet_pt3(i,j)
term(92) = term(92) + wm_interm_178_triplet_pt3(i,j) * wm_interm_93_triplet_pt3(p,q,i,j)
term(93) = term(93) + wm_interm_178_triplet_pt3(i,j) * wm_interm_90_triplet_pt3(p,q,i,j)
term(94) = term(94) + wm_interm_178_triplet_pt3(i,j) * wm_interm_91_triplet_pt3(p,q,i,j)
term(95) = term(95) + wm_interm_178_triplet_pt3(i,j) * wm_interm_92_triplet_pt3(p,q,i,j)
term(96) = term(96) + wm_interm_145_triplet_pt3(i,j) * wm_interm_173_triplet_pt3(p,q,i,j)
term(97) = term(97) + wm_interm_147_triplet_pt3(i,j) * wm_interm_173_triplet_pt3(p,q,i,j)
term(98) = term(98) + wm_interm_153_triplet_pt3(p,q,i,j) * wm_interm_178_triplet_pt3(i,j)
term(99) = term(99) + wm_interm_152_triplet_pt3(p,q,i,j) * wm_interm_178_triplet_pt3(i,j)
term(100) = term(100) + wm_interm_150_triplet_pt3(p,q,i,j) * wm_interm_178_triplet_pt3(i,j)
term(101) = term(101) + wm_interm_151_triplet_pt3(p,q,i,j) * wm_interm_178_triplet_pt3(i,j)
end do 
end do 

term(72) = term(72) * (-2.0d+0) 
term(75) = term(75) * (-2.0d+0) 
term(77) = term(77) * (4.0d+0) 
term(78) = term(78) * (-4.0d+0) 
term(79) = term(79) * (4.0d+0) 
term(80) = term(80) * (-4.0d+0) 
term(82) = term(82) * (-2.0d+0) 
term(84) = term(84) * (2.0d+0) 
term(85) = term(85) * (-4.0d+0) 
term(86) = term(86) * (2.0d+0) 
term(88) = term(88) * (-2.0d+0) 
term(92) = term(92) * (-2.0d+0) 
term(95) = term(95) * (-2.0d+0) 
term(96) = term(96) * (4.0d+0) 
term(97) = term(97) * (-4.0d+0) 
term(98) = term(98) * (4.0d+0) 
term(99) = term(99) * (-4.0d+0) 
term(100) = term(100) * (4.0d+0) 
term(101) = term(101) * (-4.0d+0) 


    calc_D_vv_wm_triplet_pt3 = zero
    do s = 0, 101
    calc_D_vv_wm_triplet_pt3 = calc_D_vv_wm_triplet_pt3 + term(s)
    end do

    end function calc_D_vv_wm_triplet_pt3
    
    
    

  end module tt_ccsd_pt3
