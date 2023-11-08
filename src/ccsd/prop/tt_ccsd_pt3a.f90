module tt_ccsd_pt3a
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
    
    
    

  end module tt_ccsd_pt3a
