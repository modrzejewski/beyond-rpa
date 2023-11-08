module so_cc3_pt3a
 use ccsd_transformed_integrals
    use cc3_intermediates
    use arithmetic
    use s_gen
    use basis
    use eom_vectors

    !
    ! File generated automatically on 2018-04-18 11:46:20
    !

    real(F64), dimension(:, :, :, :), allocatable :: wm_interm_0_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_1_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_2_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_3_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_4_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_5_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_6_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_7_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_8_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_9_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_10_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_11_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_12_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_13_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_14_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_15_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_16_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_17_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_18_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_19_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_20_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_21_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_22_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_23_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_24_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_25_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_26_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_27_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_28_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_29_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_30_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_31_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_32_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_33_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_34_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_35_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_36_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_37_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_38_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_39_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_40_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_41_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_42_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_43_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_44_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_45_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_46_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_47_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_48_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_49_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_50_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_51_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_52_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_53_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_54_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_55_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_56_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_57_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_58_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_59_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_60_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_61_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_62_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_63_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_64_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_65_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_66_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_67_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_68_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_69_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_70_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_71_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_72_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_73_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_74_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_75_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_76_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_77_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_78_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_79_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_80_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_81_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_82_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_83_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_84_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_85_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_86_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_87_so_pt3 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_88_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_89_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_90_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_91_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_92_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_93_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_94_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_95_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_96_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_97_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_98_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_99_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_100_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_101_so_pt3 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_102_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_103_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_104_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_105_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_106_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_107_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_108_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_109_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_110_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_111_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_112_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_113_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_114_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_115_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_116_so_pt3 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_117_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_118_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_119_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_120_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_121_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_122_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_123_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_124_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_125_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_126_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_127_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_128_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_129_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_130_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_131_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_132_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_133_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_134_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_135_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_136_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_137_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_138_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_139_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_140_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_141_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_142_so_pt3 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_143_so_pt3 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_144_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_145_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_146_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_147_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_148_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_149_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_150_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_151_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_152_so_pt3 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_153_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_154_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_155_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_156_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_157_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_158_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_159_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_160_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_161_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_162_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_163_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_164_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_165_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_166_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_167_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_168_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_169_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_170_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_171_so_pt3 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_172_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_173_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_174_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_175_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_176_so_pt3 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_177_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_178_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_179_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_180_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_181_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_182_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_183_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_184_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_185_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_186_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_187_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_188_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_189_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_190_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_191_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_192_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_193_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_194_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_195_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_196_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_197_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_198_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_199_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_200_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_201_so_pt3 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_202_so_pt3 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_203_so_pt3 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_204_so_pt3 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_205_so_pt3 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_206_so_pt3 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_207_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_208_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_209_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_210_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_211_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_212_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_213_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_214_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_215_so_pt3 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_216_so_pt3 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_217_so_pt3 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_218_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_219_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_220_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_221_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_222_so_pt3 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_223_so_pt3 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_224_so_pt3 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_225_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_226_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_227_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_228_so_pt3 

    contains
    
    subroutine wm_so_intermediates_cc3_init_pt3(nocc, nactive)
    integer, intent(in) :: nocc
    integer, intent(in) :: nactive
    allocate(wm_interm_0_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_1_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_2_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_3_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_4_so_pt3(1: nocc, 1: nocc))
allocate(wm_interm_5_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_6_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_7_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_8_so_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_9_so_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_10_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_11_so_pt3(1: nocc, 1: nocc))
allocate(wm_interm_12_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_13_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_14_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_15_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_16_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_17_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_18_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_19_so_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_20_so_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_21_so_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_22_so_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_23_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_24_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_25_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_26_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_27_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_28_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_29_so_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_30_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_31_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_32_so_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_33_so_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_34_so_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_35_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_36_so_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_37_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_38_so_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_39_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_40_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_41_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_42_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_43_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_44_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_45_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_46_so_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_47_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_48_so_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_49_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_50_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_51_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_52_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_53_so_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_54_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_55_so_pt3(1: nocc, 1: nocc))
allocate(wm_interm_56_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_57_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_58_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_59_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_60_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_61_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_62_so_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_63_so_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_64_so_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_65_so_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_66_so_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_67_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_68_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_69_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_70_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_71_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_72_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_73_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_74_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_75_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_76_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_77_so_pt3(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_78_so_pt3(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_79_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_80_so_pt3(1: nocc, 1: nocc))
allocate(wm_interm_81_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_82_so_pt3(1: nocc, 1: nocc))
allocate(wm_interm_83_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_84_so_pt3(1: nocc, 1: nocc))
allocate(wm_interm_85_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_86_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_87_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_88_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_89_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_90_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_91_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_92_so_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_93_so_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_94_so_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_95_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_96_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_97_so_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_98_so_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_99_so_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_100_so_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_101_so_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_102_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_103_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_104_so_pt3(1: nocc, 1: nocc))
allocate(wm_interm_105_so_pt3(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_106_so_pt3(1: nocc, 1: nocc))
allocate(wm_interm_107_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_108_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_109_so_pt3(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_110_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_111_so_pt3(1: nocc, 1: nocc))
allocate(wm_interm_112_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_113_so_pt3(1: nocc, 1: nocc))
allocate(wm_interm_114_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_115_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_116_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_117_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_118_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_119_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_120_so_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_121_so_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_122_so_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_123_so_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_124_so_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_125_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_126_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_127_so_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_128_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_129_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_130_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_131_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_132_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_133_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_134_so_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_135_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_136_so_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_137_so_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_138_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_139_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_140_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_141_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_142_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_143_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_144_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_145_so_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_146_so_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_147_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_148_so_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_149_so_pt3(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_150_so_pt3(1: nocc, 1: nocc))
allocate(wm_interm_151_so_pt3(1: nocc, 1: nocc))
allocate(wm_interm_152_so_pt3(1: nocc, 1: nocc))
allocate(wm_interm_153_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_154_so_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_155_so_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_156_so_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_157_so_pt3(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_158_so_pt3(1: nocc, 1: nocc))
allocate(wm_interm_159_so_pt3(1: nocc, 1: nocc))
allocate(wm_interm_160_so_pt3(1: nocc, 1: nocc))
allocate(wm_interm_161_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_162_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_163_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_164_so_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_165_so_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_166_so_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_167_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_168_so_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_169_so_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_170_so_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_171_so_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_172_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_173_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_174_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_175_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_176_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_177_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_178_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_179_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_180_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_181_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_182_so_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_183_so_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_184_so_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_185_so_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_186_so_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_187_so_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_188_so_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_189_so_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_190_so_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_191_so_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_192_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_193_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_194_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_195_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_196_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_197_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_198_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_199_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_200_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_201_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_202_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_203_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_204_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_205_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_206_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_207_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_208_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_209_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_210_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_211_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_212_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_213_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_214_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_215_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_216_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_217_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_218_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_219_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_220_so_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_221_so_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_222_so_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_223_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_224_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_225_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_226_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_227_so_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_228_so_pt3(nocc+1: nactive, 1: nocc))
wm_interm_0_so_pt3 = zero 
wm_interm_1_so_pt3 = zero 
wm_interm_2_so_pt3 = zero 
wm_interm_3_so_pt3 = zero 
wm_interm_4_so_pt3 = zero 
wm_interm_5_so_pt3 = zero 
wm_interm_6_so_pt3 = zero 
wm_interm_7_so_pt3 = zero 
wm_interm_8_so_pt3 = zero 
wm_interm_9_so_pt3 = zero 
wm_interm_10_so_pt3 = zero 
wm_interm_11_so_pt3 = zero 
wm_interm_12_so_pt3 = zero 
wm_interm_13_so_pt3 = zero 
wm_interm_14_so_pt3 = zero 
wm_interm_15_so_pt3 = zero 
wm_interm_16_so_pt3 = zero 
wm_interm_17_so_pt3 = zero 
wm_interm_18_so_pt3 = zero 
wm_interm_19_so_pt3 = zero 
wm_interm_20_so_pt3 = zero 
wm_interm_21_so_pt3 = zero 
wm_interm_22_so_pt3 = zero 
wm_interm_23_so_pt3 = zero 
wm_interm_24_so_pt3 = zero 
wm_interm_25_so_pt3 = zero 
wm_interm_26_so_pt3 = zero 
wm_interm_27_so_pt3 = zero 
wm_interm_28_so_pt3 = zero 
wm_interm_29_so_pt3 = zero 
wm_interm_30_so_pt3 = zero 
wm_interm_31_so_pt3 = zero 
wm_interm_32_so_pt3 = zero 
wm_interm_33_so_pt3 = zero 
wm_interm_34_so_pt3 = zero 
wm_interm_35_so_pt3 = zero 
wm_interm_36_so_pt3 = zero 
wm_interm_37_so_pt3 = zero 
wm_interm_38_so_pt3 = zero 
wm_interm_39_so_pt3 = zero 
wm_interm_40_so_pt3 = zero 
wm_interm_41_so_pt3 = zero 
wm_interm_42_so_pt3 = zero 
wm_interm_43_so_pt3 = zero 
wm_interm_44_so_pt3 = zero 
wm_interm_45_so_pt3 = zero 
wm_interm_46_so_pt3 = zero 
wm_interm_47_so_pt3 = zero 
wm_interm_48_so_pt3 = zero 
wm_interm_49_so_pt3 = zero 
wm_interm_50_so_pt3 = zero 
wm_interm_51_so_pt3 = zero 
wm_interm_52_so_pt3 = zero 
wm_interm_53_so_pt3 = zero 
wm_interm_54_so_pt3 = zero 
wm_interm_55_so_pt3 = zero 
wm_interm_56_so_pt3 = zero 
wm_interm_57_so_pt3 = zero 
wm_interm_58_so_pt3 = zero 
wm_interm_59_so_pt3 = zero 
wm_interm_60_so_pt3 = zero 
wm_interm_61_so_pt3 = zero 
wm_interm_62_so_pt3 = zero 
wm_interm_63_so_pt3 = zero 
wm_interm_64_so_pt3 = zero 
wm_interm_65_so_pt3 = zero 
wm_interm_66_so_pt3 = zero 
wm_interm_67_so_pt3 = zero 
wm_interm_68_so_pt3 = zero 
wm_interm_69_so_pt3 = zero 
wm_interm_70_so_pt3 = zero 
wm_interm_71_so_pt3 = zero 
wm_interm_72_so_pt3 = zero 
wm_interm_73_so_pt3 = zero 
wm_interm_74_so_pt3 = zero 
wm_interm_75_so_pt3 = zero 
wm_interm_76_so_pt3 = zero 
wm_interm_77_so_pt3 = zero 
wm_interm_78_so_pt3 = zero 
wm_interm_79_so_pt3 = zero 
wm_interm_80_so_pt3 = zero 
wm_interm_81_so_pt3 = zero 
wm_interm_82_so_pt3 = zero 
wm_interm_83_so_pt3 = zero 
wm_interm_84_so_pt3 = zero 
wm_interm_85_so_pt3 = zero 
wm_interm_86_so_pt3 = zero 
wm_interm_87_so_pt3 = zero 
wm_interm_88_so_pt3 = zero 
wm_interm_89_so_pt3 = zero 
wm_interm_90_so_pt3 = zero 
wm_interm_91_so_pt3 = zero 
wm_interm_92_so_pt3 = zero 
wm_interm_93_so_pt3 = zero 
wm_interm_94_so_pt3 = zero 
wm_interm_95_so_pt3 = zero 
wm_interm_96_so_pt3 = zero 
wm_interm_97_so_pt3 = zero 
wm_interm_98_so_pt3 = zero 
wm_interm_99_so_pt3 = zero 
wm_interm_100_so_pt3 = zero 
wm_interm_101_so_pt3 = zero 
wm_interm_102_so_pt3 = zero 
wm_interm_103_so_pt3 = zero 
wm_interm_104_so_pt3 = zero 
wm_interm_105_so_pt3 = zero 
wm_interm_106_so_pt3 = zero 
wm_interm_107_so_pt3 = zero 
wm_interm_108_so_pt3 = zero 
wm_interm_109_so_pt3 = zero 
wm_interm_110_so_pt3 = zero 
wm_interm_111_so_pt3 = zero 
wm_interm_112_so_pt3 = zero 
wm_interm_113_so_pt3 = zero 
wm_interm_114_so_pt3 = zero 
wm_interm_115_so_pt3 = zero 
wm_interm_116_so_pt3 = zero 
wm_interm_117_so_pt3 = zero 
wm_interm_118_so_pt3 = zero 
wm_interm_119_so_pt3 = zero 
wm_interm_120_so_pt3 = zero 
wm_interm_121_so_pt3 = zero 
wm_interm_122_so_pt3 = zero 
wm_interm_123_so_pt3 = zero 
wm_interm_124_so_pt3 = zero 
wm_interm_125_so_pt3 = zero 
wm_interm_126_so_pt3 = zero 
wm_interm_127_so_pt3 = zero 
wm_interm_128_so_pt3 = zero 
wm_interm_129_so_pt3 = zero 
wm_interm_130_so_pt3 = zero 
wm_interm_131_so_pt3 = zero 
wm_interm_132_so_pt3 = zero 
wm_interm_133_so_pt3 = zero 
wm_interm_134_so_pt3 = zero 
wm_interm_135_so_pt3 = zero 
wm_interm_136_so_pt3 = zero 
wm_interm_137_so_pt3 = zero 
wm_interm_138_so_pt3 = zero 
wm_interm_139_so_pt3 = zero 
wm_interm_140_so_pt3 = zero 
wm_interm_141_so_pt3 = zero 
wm_interm_142_so_pt3 = zero 
wm_interm_143_so_pt3 = zero 
wm_interm_144_so_pt3 = zero 
wm_interm_145_so_pt3 = zero 
wm_interm_146_so_pt3 = zero 
wm_interm_147_so_pt3 = zero 
wm_interm_148_so_pt3 = zero 
wm_interm_149_so_pt3 = zero 
wm_interm_150_so_pt3 = zero 
wm_interm_151_so_pt3 = zero 
wm_interm_152_so_pt3 = zero 
wm_interm_153_so_pt3 = zero 
wm_interm_154_so_pt3 = zero 
wm_interm_155_so_pt3 = zero 
wm_interm_156_so_pt3 = zero 
wm_interm_157_so_pt3 = zero 
wm_interm_158_so_pt3 = zero 
wm_interm_159_so_pt3 = zero 
wm_interm_160_so_pt3 = zero 
wm_interm_161_so_pt3 = zero 
wm_interm_162_so_pt3 = zero 
wm_interm_163_so_pt3 = zero 
wm_interm_164_so_pt3 = zero 
wm_interm_165_so_pt3 = zero 
wm_interm_166_so_pt3 = zero 
wm_interm_167_so_pt3 = zero 
wm_interm_168_so_pt3 = zero 
wm_interm_169_so_pt3 = zero 
wm_interm_170_so_pt3 = zero 
wm_interm_171_so_pt3 = zero 
wm_interm_172_so_pt3 = zero 
wm_interm_173_so_pt3 = zero 
wm_interm_174_so_pt3 = zero 
wm_interm_175_so_pt3 = zero 
wm_interm_176_so_pt3 = zero 
wm_interm_177_so_pt3 = zero 
wm_interm_178_so_pt3 = zero 
wm_interm_179_so_pt3 = zero 
wm_interm_180_so_pt3 = zero 
wm_interm_181_so_pt3 = zero 
wm_interm_182_so_pt3 = zero 
wm_interm_183_so_pt3 = zero 
wm_interm_184_so_pt3 = zero 
wm_interm_185_so_pt3 = zero 
wm_interm_186_so_pt3 = zero 
wm_interm_187_so_pt3 = zero 
wm_interm_188_so_pt3 = zero 
wm_interm_189_so_pt3 = zero 
wm_interm_190_so_pt3 = zero 
wm_interm_191_so_pt3 = zero 
wm_interm_192_so_pt3 = zero 
wm_interm_193_so_pt3 = zero 
wm_interm_194_so_pt3 = zero 
wm_interm_195_so_pt3 = zero 
wm_interm_196_so_pt3 = zero 
wm_interm_197_so_pt3 = zero 
wm_interm_198_so_pt3 = zero 
wm_interm_199_so_pt3 = zero 
wm_interm_200_so_pt3 = zero 
wm_interm_201_so_pt3 = zero 
wm_interm_202_so_pt3 = zero 
wm_interm_203_so_pt3 = zero 
wm_interm_204_so_pt3 = zero 
wm_interm_205_so_pt3 = zero 
wm_interm_206_so_pt3 = zero 
wm_interm_207_so_pt3 = zero 
wm_interm_208_so_pt3 = zero 
wm_interm_209_so_pt3 = zero 
wm_interm_210_so_pt3 = zero 
wm_interm_211_so_pt3 = zero 
wm_interm_212_so_pt3 = zero 
wm_interm_213_so_pt3 = zero 
wm_interm_214_so_pt3 = zero 
wm_interm_215_so_pt3 = zero 
wm_interm_216_so_pt3 = zero 
wm_interm_217_so_pt3 = zero 
wm_interm_218_so_pt3 = zero 
wm_interm_219_so_pt3 = zero 
wm_interm_220_so_pt3 = zero 
wm_interm_221_so_pt3 = zero 
wm_interm_222_so_pt3 = zero 
wm_interm_223_so_pt3 = zero 
wm_interm_224_so_pt3 = zero 
wm_interm_225_so_pt3 = zero 
wm_interm_226_so_pt3 = zero 
wm_interm_227_so_pt3 = zero 
wm_interm_228_so_pt3 = zero 

    end subroutine wm_so_intermediates_cc3_init_pt3
    
    subroutine wm_so_intermediates_cc3_free_pt3
    deallocate(wm_interm_0_so_pt3)
deallocate(wm_interm_1_so_pt3)
deallocate(wm_interm_2_so_pt3)
deallocate(wm_interm_3_so_pt3)
deallocate(wm_interm_4_so_pt3)
deallocate(wm_interm_5_so_pt3)
deallocate(wm_interm_6_so_pt3)
deallocate(wm_interm_7_so_pt3)
deallocate(wm_interm_8_so_pt3)
deallocate(wm_interm_9_so_pt3)
deallocate(wm_interm_10_so_pt3)
deallocate(wm_interm_11_so_pt3)
deallocate(wm_interm_12_so_pt3)
deallocate(wm_interm_13_so_pt3)
deallocate(wm_interm_14_so_pt3)
deallocate(wm_interm_15_so_pt3)
deallocate(wm_interm_16_so_pt3)
deallocate(wm_interm_17_so_pt3)
deallocate(wm_interm_18_so_pt3)
deallocate(wm_interm_19_so_pt3)
deallocate(wm_interm_20_so_pt3)
deallocate(wm_interm_21_so_pt3)
deallocate(wm_interm_22_so_pt3)
deallocate(wm_interm_23_so_pt3)
deallocate(wm_interm_24_so_pt3)
deallocate(wm_interm_25_so_pt3)
deallocate(wm_interm_26_so_pt3)
deallocate(wm_interm_27_so_pt3)
deallocate(wm_interm_28_so_pt3)
deallocate(wm_interm_29_so_pt3)
deallocate(wm_interm_30_so_pt3)
deallocate(wm_interm_31_so_pt3)
deallocate(wm_interm_32_so_pt3)
deallocate(wm_interm_33_so_pt3)
deallocate(wm_interm_34_so_pt3)
deallocate(wm_interm_35_so_pt3)
deallocate(wm_interm_36_so_pt3)
deallocate(wm_interm_37_so_pt3)
deallocate(wm_interm_38_so_pt3)
deallocate(wm_interm_39_so_pt3)
deallocate(wm_interm_40_so_pt3)
deallocate(wm_interm_41_so_pt3)
deallocate(wm_interm_42_so_pt3)
deallocate(wm_interm_43_so_pt3)
deallocate(wm_interm_44_so_pt3)
deallocate(wm_interm_45_so_pt3)
deallocate(wm_interm_46_so_pt3)
deallocate(wm_interm_47_so_pt3)
deallocate(wm_interm_48_so_pt3)
deallocate(wm_interm_49_so_pt3)
deallocate(wm_interm_50_so_pt3)
deallocate(wm_interm_51_so_pt3)
deallocate(wm_interm_52_so_pt3)
deallocate(wm_interm_53_so_pt3)
deallocate(wm_interm_54_so_pt3)
deallocate(wm_interm_55_so_pt3)
deallocate(wm_interm_56_so_pt3)
deallocate(wm_interm_57_so_pt3)
deallocate(wm_interm_58_so_pt3)
deallocate(wm_interm_59_so_pt3)
deallocate(wm_interm_60_so_pt3)
deallocate(wm_interm_61_so_pt3)
deallocate(wm_interm_62_so_pt3)
deallocate(wm_interm_63_so_pt3)
deallocate(wm_interm_64_so_pt3)
deallocate(wm_interm_65_so_pt3)
deallocate(wm_interm_66_so_pt3)
deallocate(wm_interm_67_so_pt3)
deallocate(wm_interm_68_so_pt3)
deallocate(wm_interm_69_so_pt3)
deallocate(wm_interm_70_so_pt3)
deallocate(wm_interm_71_so_pt3)
deallocate(wm_interm_72_so_pt3)
deallocate(wm_interm_73_so_pt3)
deallocate(wm_interm_74_so_pt3)
deallocate(wm_interm_75_so_pt3)
deallocate(wm_interm_76_so_pt3)
deallocate(wm_interm_77_so_pt3)
deallocate(wm_interm_78_so_pt3)
deallocate(wm_interm_79_so_pt3)
deallocate(wm_interm_80_so_pt3)
deallocate(wm_interm_81_so_pt3)
deallocate(wm_interm_82_so_pt3)
deallocate(wm_interm_83_so_pt3)
deallocate(wm_interm_84_so_pt3)
deallocate(wm_interm_85_so_pt3)
deallocate(wm_interm_86_so_pt3)
deallocate(wm_interm_87_so_pt3)
deallocate(wm_interm_88_so_pt3)
deallocate(wm_interm_89_so_pt3)
deallocate(wm_interm_90_so_pt3)
deallocate(wm_interm_91_so_pt3)
deallocate(wm_interm_92_so_pt3)
deallocate(wm_interm_93_so_pt3)
deallocate(wm_interm_94_so_pt3)
deallocate(wm_interm_95_so_pt3)
deallocate(wm_interm_96_so_pt3)
deallocate(wm_interm_97_so_pt3)
deallocate(wm_interm_98_so_pt3)
deallocate(wm_interm_99_so_pt3)
deallocate(wm_interm_100_so_pt3)
deallocate(wm_interm_101_so_pt3)
deallocate(wm_interm_102_so_pt3)
deallocate(wm_interm_103_so_pt3)
deallocate(wm_interm_104_so_pt3)
deallocate(wm_interm_105_so_pt3)
deallocate(wm_interm_106_so_pt3)
deallocate(wm_interm_107_so_pt3)
deallocate(wm_interm_108_so_pt3)
deallocate(wm_interm_109_so_pt3)
deallocate(wm_interm_110_so_pt3)
deallocate(wm_interm_111_so_pt3)
deallocate(wm_interm_112_so_pt3)
deallocate(wm_interm_113_so_pt3)
deallocate(wm_interm_114_so_pt3)
deallocate(wm_interm_115_so_pt3)
deallocate(wm_interm_116_so_pt3)
deallocate(wm_interm_117_so_pt3)
deallocate(wm_interm_118_so_pt3)
deallocate(wm_interm_119_so_pt3)
deallocate(wm_interm_120_so_pt3)
deallocate(wm_interm_121_so_pt3)
deallocate(wm_interm_122_so_pt3)
deallocate(wm_interm_123_so_pt3)
deallocate(wm_interm_124_so_pt3)
deallocate(wm_interm_125_so_pt3)
deallocate(wm_interm_126_so_pt3)
deallocate(wm_interm_127_so_pt3)
deallocate(wm_interm_128_so_pt3)
deallocate(wm_interm_129_so_pt3)
deallocate(wm_interm_130_so_pt3)
deallocate(wm_interm_131_so_pt3)
deallocate(wm_interm_132_so_pt3)
deallocate(wm_interm_133_so_pt3)
deallocate(wm_interm_134_so_pt3)
deallocate(wm_interm_135_so_pt3)
deallocate(wm_interm_136_so_pt3)
deallocate(wm_interm_137_so_pt3)
deallocate(wm_interm_138_so_pt3)
deallocate(wm_interm_139_so_pt3)
deallocate(wm_interm_140_so_pt3)
deallocate(wm_interm_141_so_pt3)
deallocate(wm_interm_142_so_pt3)
deallocate(wm_interm_143_so_pt3)
deallocate(wm_interm_144_so_pt3)
deallocate(wm_interm_145_so_pt3)
deallocate(wm_interm_146_so_pt3)
deallocate(wm_interm_147_so_pt3)
deallocate(wm_interm_148_so_pt3)
deallocate(wm_interm_149_so_pt3)
deallocate(wm_interm_150_so_pt3)
deallocate(wm_interm_151_so_pt3)
deallocate(wm_interm_152_so_pt3)
deallocate(wm_interm_153_so_pt3)
deallocate(wm_interm_154_so_pt3)
deallocate(wm_interm_155_so_pt3)
deallocate(wm_interm_156_so_pt3)
deallocate(wm_interm_157_so_pt3)
deallocate(wm_interm_158_so_pt3)
deallocate(wm_interm_159_so_pt3)
deallocate(wm_interm_160_so_pt3)
deallocate(wm_interm_161_so_pt3)
deallocate(wm_interm_162_so_pt3)
deallocate(wm_interm_163_so_pt3)
deallocate(wm_interm_164_so_pt3)
deallocate(wm_interm_165_so_pt3)
deallocate(wm_interm_166_so_pt3)
deallocate(wm_interm_167_so_pt3)
deallocate(wm_interm_168_so_pt3)
deallocate(wm_interm_169_so_pt3)
deallocate(wm_interm_170_so_pt3)
deallocate(wm_interm_171_so_pt3)
deallocate(wm_interm_172_so_pt3)
deallocate(wm_interm_173_so_pt3)
deallocate(wm_interm_174_so_pt3)
deallocate(wm_interm_175_so_pt3)
deallocate(wm_interm_176_so_pt3)
deallocate(wm_interm_177_so_pt3)
deallocate(wm_interm_178_so_pt3)
deallocate(wm_interm_179_so_pt3)
deallocate(wm_interm_180_so_pt3)
deallocate(wm_interm_181_so_pt3)
deallocate(wm_interm_182_so_pt3)
deallocate(wm_interm_183_so_pt3)
deallocate(wm_interm_184_so_pt3)
deallocate(wm_interm_185_so_pt3)
deallocate(wm_interm_186_so_pt3)
deallocate(wm_interm_187_so_pt3)
deallocate(wm_interm_188_so_pt3)
deallocate(wm_interm_189_so_pt3)
deallocate(wm_interm_190_so_pt3)
deallocate(wm_interm_191_so_pt3)
deallocate(wm_interm_192_so_pt3)
deallocate(wm_interm_193_so_pt3)
deallocate(wm_interm_194_so_pt3)
deallocate(wm_interm_195_so_pt3)
deallocate(wm_interm_196_so_pt3)
deallocate(wm_interm_197_so_pt3)
deallocate(wm_interm_198_so_pt3)
deallocate(wm_interm_199_so_pt3)
deallocate(wm_interm_200_so_pt3)
deallocate(wm_interm_201_so_pt3)
deallocate(wm_interm_202_so_pt3)
deallocate(wm_interm_203_so_pt3)
deallocate(wm_interm_204_so_pt3)
deallocate(wm_interm_205_so_pt3)
deallocate(wm_interm_206_so_pt3)
deallocate(wm_interm_207_so_pt3)
deallocate(wm_interm_208_so_pt3)
deallocate(wm_interm_209_so_pt3)
deallocate(wm_interm_210_so_pt3)
deallocate(wm_interm_211_so_pt3)
deallocate(wm_interm_212_so_pt3)
deallocate(wm_interm_213_so_pt3)
deallocate(wm_interm_214_so_pt3)
deallocate(wm_interm_215_so_pt3)
deallocate(wm_interm_216_so_pt3)
deallocate(wm_interm_217_so_pt3)
deallocate(wm_interm_218_so_pt3)
deallocate(wm_interm_219_so_pt3)
deallocate(wm_interm_220_so_pt3)
deallocate(wm_interm_221_so_pt3)
deallocate(wm_interm_222_so_pt3)
deallocate(wm_interm_223_so_pt3)
deallocate(wm_interm_224_so_pt3)
deallocate(wm_interm_225_so_pt3)
deallocate(wm_interm_226_so_pt3)
deallocate(wm_interm_227_so_pt3)
deallocate(wm_interm_228_so_pt3)

    end subroutine wm_so_intermediates_cc3_free_pt3
    
    subroutine wm_so_intermediates_cc3_pt3(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr)
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
wm_interm_0_so_pt3(a, b, i, j) = wm_interm_0_so_pt3(a, b, i, j) + sum 
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
wm_interm_1_so_pt3(c, k, j, l) = wm_interm_1_so_pt3(c, k, j, l) + sum 
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
wm_interm_2_so_pt3(c, j, k, l) = wm_interm_2_so_pt3(c, j, k, l) + sum 
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
wm_interm_3_so_pt3(c, k, j, l) = wm_interm_3_so_pt3(c, k, j, l) + sum 
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
wm_interm_4_so_pt3(i, j) = wm_interm_4_so_pt3(i, j) + sum 
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
wm_interm_5_so_pt3(b, i, j, k) = wm_interm_5_so_pt3(b, i, j, k) + sum 
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
wm_interm_6_so_pt3(b, c, j, k) = wm_interm_6_so_pt3(b, c, j, k) + sum 
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
wm_interm_7_so_pt3(b, c, j, k) = wm_interm_7_so_pt3(b, c, j, k) + sum 
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
wm_interm_8_so_pt3(b, j) = wm_interm_8_so_pt3(b, j) + sum 
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
wm_interm_9_so_pt3(b, j) = wm_interm_9_so_pt3(b, j) + sum 
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
wm_interm_10_so_pt3(b, j, i, k) = wm_interm_10_so_pt3(b, j, i, k) + sum 
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
wm_interm_11_so_pt3(i, j) = wm_interm_11_so_pt3(i, j) + sum 
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
wm_interm_12_so_pt3(c, k, j, l) = wm_interm_12_so_pt3(c, k, j, l) + sum 
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
wm_interm_13_so_pt3(c, k, j, l) = wm_interm_13_so_pt3(c, k, j, l) + sum 
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
wm_interm_14_so_pt3(c, j, k, l) = wm_interm_14_so_pt3(c, j, k, l) + sum 
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
wm_interm_15_so_pt3(c, j, k, l) = wm_interm_15_so_pt3(c, j, k, l) + sum 
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
wm_interm_16_so_pt3(c, j, k, l) = wm_interm_16_so_pt3(c, j, k, l) + sum 
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
wm_interm_17_so_pt3(c, j, k, l) = wm_interm_17_so_pt3(c, j, k, l) + sum 
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
wm_interm_18_so_pt3(a, b, i, j) = wm_interm_18_so_pt3(a, b, i, j) + sum 
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
wm_interm_19_so_pt3(c, k) = wm_interm_19_so_pt3(c, k) + sum 
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
wm_interm_20_so_pt3(c, k) = wm_interm_20_so_pt3(c, k) + sum 
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
wm_interm_21_so_pt3(c, k) = wm_interm_21_so_pt3(c, k) + sum 
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
wm_interm_22_so_pt3(c, k) = wm_interm_22_so_pt3(c, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_23_so_pt3(b, c, j, k) = wm_interm_23_so_pt3(b, c, j, k) + sum 
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
wm_interm_24_so_pt3(b, c, j, k) = wm_interm_24_so_pt3(b, c, j, k) + sum 
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
wm_interm_25_so_pt3(b, c, j, k) = wm_interm_25_so_pt3(b, c, j, k) + sum 
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
wm_interm_26_so_pt3(b, c, k, j) = wm_interm_26_so_pt3(b, c, k, j) + sum 
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
wm_interm_27_so_pt3(b, c, k, j) = wm_interm_27_so_pt3(b, c, k, j) + sum 
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
wm_interm_28_so_pt3(b, c, j, k) = wm_interm_28_so_pt3(b, c, j, k) + sum 
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
wm_interm_29_so_pt3(a, b) = wm_interm_29_so_pt3(a, b) + sum 
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
wm_interm_30_so_pt3(a, b, i, j) = wm_interm_30_so_pt3(a, b, i, j) + sum 
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
wm_interm_31_so_pt3(c, j, l, k) = wm_interm_31_so_pt3(c, j, l, k) + sum 
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
wm_interm_32_so_pt3(a, b) = wm_interm_32_so_pt3(a, b) + sum 
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
wm_interm_33_so_pt3(c, k) = wm_interm_33_so_pt3(c, k) + sum 
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
wm_interm_34_so_pt3(c, k) = wm_interm_34_so_pt3(c, k) + sum 
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
wm_interm_35_so_pt3(c, j, l, k) = wm_interm_35_so_pt3(c, j, l, k) + sum 
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
wm_interm_36_so_pt3(c, k) = wm_interm_36_so_pt3(c, k) + sum 
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
wm_interm_37_so_pt3(c, j, k, l) = wm_interm_37_so_pt3(c, j, k, l) + sum 
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
wm_interm_38_so_pt3(c, k) = wm_interm_38_so_pt3(c, k) + sum 
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
wm_interm_39_so_pt3(c, j, k, l) = wm_interm_39_so_pt3(c, j, k, l) + sum 
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
sum = sum + r1(vrdav_Rl, a,i) * t3(nocc, nactive, a,b,c,j,k,i)
end do 
end do 
wm_interm_40_so_pt3(b, c, j, k) = wm_interm_40_so_pt3(b, c, j, k) + sum 
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
wm_interm_41_so_pt3(b, i, j, k) = wm_interm_41_so_pt3(b, i, j, k) + sum 
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
wm_interm_42_so_pt3(b, c, j, k) = wm_interm_42_so_pt3(b, c, j, k) + sum 
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
wm_interm_43_so_pt3(b, c, j, k) = wm_interm_43_so_pt3(b, c, j, k) + sum 
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
wm_interm_44_so_pt3(c, j, k, l) = wm_interm_44_so_pt3(c, j, k, l) + sum 
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
sum = sum + s2(a,b,j,i) * t3(nocc, nactive, a,b,c,k,i,l)
end do 
end do 
end do 
wm_interm_45_so_pt3(c, j, k, l) = wm_interm_45_so_pt3(c, j, k, l) + sum 
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
wm_interm_46_so_pt3(b, j) = wm_interm_46_so_pt3(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_47_so_pt3(b, c, j, k) = wm_interm_47_so_pt3(b, c, j, k) + sum 
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
wm_interm_48_so_pt3(b, j) = wm_interm_48_so_pt3(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_49_so_pt3(b, c, j, k) = wm_interm_49_so_pt3(b, c, j, k) + sum 
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
wm_interm_50_so_pt3(b, c, k, j) = wm_interm_50_so_pt3(b, c, k, j) + sum 
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
wm_interm_51_so_pt3(b, c, k, j) = wm_interm_51_so_pt3(b, c, k, j) + sum 
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
wm_interm_52_so_pt3(b, i, k, j) = wm_interm_52_so_pt3(b, i, k, j) + sum 
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
wm_interm_53_so_pt3(b, j) = wm_interm_53_so_pt3(b, j) + sum 
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
wm_interm_54_so_pt3(c, j, k, l) = wm_interm_54_so_pt3(c, j, k, l) + sum 
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
wm_interm_55_so_pt3(i, j) = wm_interm_55_so_pt3(i, j) + sum 
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
wm_interm_56_so_pt3(a, b, i, j) = wm_interm_56_so_pt3(a, b, i, j) + sum 
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
wm_interm_57_so_pt3(c, j, k, l) = wm_interm_57_so_pt3(c, j, k, l) + sum 
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
wm_interm_58_so_pt3(c, k, j, l) = wm_interm_58_so_pt3(c, k, j, l) + sum 
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
wm_interm_59_so_pt3(c, k, j, l) = wm_interm_59_so_pt3(c, k, j, l) + sum 
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
wm_interm_60_so_pt3(c, j, k, l) = wm_interm_60_so_pt3(c, j, k, l) + sum 
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
wm_interm_61_so_pt3(c, j, k, l) = wm_interm_61_so_pt3(c, j, k, l) + sum 
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
wm_interm_62_so_pt3(c, k) = wm_interm_62_so_pt3(c, k) + sum 
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
wm_interm_63_so_pt3(a, b) = wm_interm_63_so_pt3(a, b) + sum 
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
wm_interm_64_so_pt3(c, k) = wm_interm_64_so_pt3(c, k) + sum 
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
wm_interm_65_so_pt3(c, k) = wm_interm_65_so_pt3(c, k) + sum 
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
wm_interm_66_so_pt3(c, k) = wm_interm_66_so_pt3(c, k) + sum 
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
wm_interm_67_so_pt3(a, b, i, j) = wm_interm_67_so_pt3(a, b, i, j) + sum 
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
wm_interm_68_so_pt3(b, c, j, k) = wm_interm_68_so_pt3(b, c, j, k) + sum 
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
wm_interm_69_so_pt3(b, c, k, j) = wm_interm_69_so_pt3(b, c, k, j) + sum 
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
wm_interm_70_so_pt3(b, c, j, k) = wm_interm_70_so_pt3(b, c, j, k) + sum 
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
wm_interm_71_so_pt3(b, c, k, j) = wm_interm_71_so_pt3(b, c, k, j) + sum 
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
wm_interm_72_so_pt3(b, c, j, k) = wm_interm_72_so_pt3(b, c, j, k) + sum 
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
wm_interm_73_so_pt3(b, c, j, k) = wm_interm_73_so_pt3(b, c, j, k) + sum 
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
wm_interm_74_so_pt3(b, c, j, k) = wm_interm_74_so_pt3(b, c, j, k) + sum 
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
wm_interm_75_so_pt3(b, c, j, k) = wm_interm_75_so_pt3(b, c, j, k) + sum 
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
wm_interm_76_so_pt3(b, c, j, k) = wm_interm_76_so_pt3(b, c, j, k) + sum 
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
sum = sum + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, a,k,b,l)
end do 
end do 
wm_interm_77_so_pt3(i, j, k, l) = wm_interm_77_so_pt3(i, j, k, l) + sum 
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
wm_interm_78_so_pt3(i, j, k, l) = wm_interm_78_so_pt3(i, j, k, l) + sum 
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
wm_interm_79_so_pt3(b, c, j, k) = wm_interm_79_so_pt3(b, c, j, k) + sum 
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
wm_interm_80_so_pt3(j, k) = wm_interm_80_so_pt3(j, k) + sum 
end do 
end do 
!$omp end do nowait 
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
wm_interm_81_so_pt3(b, c, j, k) = wm_interm_81_so_pt3(b, c, j, k) + sum 
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
wm_interm_82_so_pt3(j, k) = wm_interm_82_so_pt3(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_83_so_pt3(b, c, j, k) = wm_interm_83_so_pt3(b, c, j, k) + sum 
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
wm_interm_84_so_pt3(j, k) = wm_interm_84_so_pt3(j, k) + sum 
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
sum = sum + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,k,i,l)
end do 
end do 
end do 
wm_interm_85_so_pt3(c, j, k, l) = wm_interm_85_so_pt3(c, j, k, l) + sum 
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
sum = sum + r2p(vrdav_Rr, a,j,b,i) * t3(nocc, nactive, a,b,c,k,i,l)
end do 
end do 
end do 
wm_interm_86_so_pt3(c, j, k, l) = wm_interm_86_so_pt3(c, j, k, l) + sum 
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
sum = sum + r2p(vrdav_Rr, a,j,b,i) * t3(nocc, nactive, a,b,c,i,k,l)
end do 
end do 
end do 
wm_interm_87_so_pt3(c, j, k, l) = wm_interm_87_so_pt3(c, j, k, l) + sum 
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
sum = sum + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,k,l,m)
end do 
end do 
wm_interm_88_so_pt3(c, i, j, k, l, m) = wm_interm_88_so_pt3(c, i, j, k, l, m) + sum 
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
wm_interm_89_so_pt3(b, c, j, k) = wm_interm_89_so_pt3(b, c, j, k) + sum 
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
sum = sum + r2p(vrdav_Rr, a,j,b,i) * t3(nocc, nactive, a,b,c,k,l,i)
end do 
end do 
end do 
wm_interm_90_so_pt3(c, j, k, l) = wm_interm_90_so_pt3(c, j, k, l) + sum 
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
sum = sum + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,k,l,i)
end do 
end do 
end do 
wm_interm_91_so_pt3(c, j, k, l) = wm_interm_91_so_pt3(c, j, k, l) + sum 
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
wm_interm_92_so_pt3(b, c) = wm_interm_92_so_pt3(b, c) + sum 
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
wm_interm_93_so_pt3(b, c) = wm_interm_93_so_pt3(b, c) + sum 
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
wm_interm_94_so_pt3(b, c) = wm_interm_94_so_pt3(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_95_so_pt3(b, c, j, k) = wm_interm_95_so_pt3(b, c, j, k) + sum 
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
wm_interm_96_so_pt3(b, c, j, k) = wm_interm_96_so_pt3(b, c, j, k) + sum 
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
sum = sum + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,k,i,j)
end do 
end do 
end do 
end do 
wm_interm_97_so_pt3(c, k) = wm_interm_97_so_pt3(c, k) + sum 
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
sum = sum + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,k,j,i)
end do 
end do 
end do 
end do 
wm_interm_98_so_pt3(c, k) = wm_interm_98_so_pt3(c, k) + sum 
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
sum = sum + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,j,k,i)
end do 
end do 
end do 
end do 
wm_interm_99_so_pt3(c, k) = wm_interm_99_so_pt3(c, k) + sum 
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
wm_interm_100_so_pt3(b, c) = wm_interm_100_so_pt3(b, c) + sum 
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
wm_interm_101_so_pt3(b, c) = wm_interm_101_so_pt3(b, c) + sum 
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
wm_interm_102_so_pt3(c, j, k, i, l, m) = wm_interm_102_so_pt3(c, j, k, i, l, m) + sum 
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
wm_interm_103_so_pt3(b, c, j, k) = wm_interm_103_so_pt3(b, c, j, k) + sum 
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
wm_interm_104_so_pt3(j, k) = wm_interm_104_so_pt3(j, k) + sum 
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
wm_interm_105_so_pt3(i, j, k, l) = wm_interm_105_so_pt3(i, j, k, l) + sum 
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
wm_interm_106_so_pt3(j, k) = wm_interm_106_so_pt3(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_107_so_pt3(b, c, j, k) = wm_interm_107_so_pt3(b, c, j, k) + sum 
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
wm_interm_108_so_pt3(b, c, j, k) = wm_interm_108_so_pt3(b, c, j, k) + sum 
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
sum = sum + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,k,b,l)
end do 
end do 
wm_interm_109_so_pt3(i, j, k, l) = wm_interm_109_so_pt3(i, j, k, l) + sum 
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
wm_interm_110_so_pt3(b, c, j, k) = wm_interm_110_so_pt3(b, c, j, k) + sum 
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
wm_interm_111_so_pt3(j, k) = wm_interm_111_so_pt3(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_112_so_pt3(b, c, j, k) = wm_interm_112_so_pt3(b, c, j, k) + sum 
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
wm_interm_113_so_pt3(j, k) = wm_interm_113_so_pt3(j, k) + sum 
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
sum = sum + r2m(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,k,i,l)
end do 
end do 
end do 
wm_interm_114_so_pt3(c, j, k, l) = wm_interm_114_so_pt3(c, j, k, l) + sum 
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
sum = sum + r2m(vrdav_Rr, a,j,b,i) * t3(nocc, nactive, a,b,c,k,i,l)
end do 
end do 
end do 
wm_interm_115_so_pt3(c, j, k, l) = wm_interm_115_so_pt3(c, j, k, l) + sum 
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
sum = sum + r2m(vrdav_Rr, a,j,b,i) * t3(nocc, nactive, a,b,c,i,k,l)
end do 
end do 
end do 
wm_interm_116_so_pt3(c, j, k, l) = wm_interm_116_so_pt3(c, j, k, l) + sum 
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
sum = sum + r2m(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,k,l,m)
end do 
end do 
wm_interm_117_so_pt3(c, i, j, k, l, m) = wm_interm_117_so_pt3(c, i, j, k, l, m) + sum 
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
sum = sum + r2m(vrdav_Rr, a,j,b,i) * t3(nocc, nactive, a,b,c,k,l,i)
end do 
end do 
end do 
wm_interm_118_so_pt3(c, j, k, l) = wm_interm_118_so_pt3(c, j, k, l) + sum 
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
sum = sum + r2m(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,k,l,i)
end do 
end do 
end do 
wm_interm_119_so_pt3(c, j, k, l) = wm_interm_119_so_pt3(c, j, k, l) + sum 
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
wm_interm_120_so_pt3(b, c) = wm_interm_120_so_pt3(b, c) + sum 
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
wm_interm_121_so_pt3(b, c) = wm_interm_121_so_pt3(b, c) + sum 
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
sum = sum + r2m(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,k,i,j)
end do 
end do 
end do 
end do 
wm_interm_122_so_pt3(c, k) = wm_interm_122_so_pt3(c, k) + sum 
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
sum = sum + r2m(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,k,j,i)
end do 
end do 
end do 
end do 
wm_interm_123_so_pt3(c, k) = wm_interm_123_so_pt3(c, k) + sum 
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
sum = sum + r2m(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,j,k,i)
end do 
end do 
end do 
end do 
wm_interm_124_so_pt3(c, k) = wm_interm_124_so_pt3(c, k) + sum 
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
wm_interm_125_so_pt3(c, j, k, l) = wm_interm_125_so_pt3(c, j, k, l) + sum 
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
wm_interm_126_so_pt3(c, j, k, l) = wm_interm_126_so_pt3(c, j, k, l) + sum 
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
wm_interm_127_so_pt3(c, k) = wm_interm_127_so_pt3(c, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_128_so_pt3(b, c, j, k) = wm_interm_128_so_pt3(b, c, j, k) + sum 
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
wm_interm_129_so_pt3(b, c, j, k) = wm_interm_129_so_pt3(b, c, j, k) + sum 
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
wm_interm_130_so_pt3(b, c, j, k) = wm_interm_130_so_pt3(b, c, j, k) + sum 
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
wm_interm_131_so_pt3(b, c, j, k) = wm_interm_131_so_pt3(b, c, j, k) + sum 
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
wm_interm_132_so_pt3(b, c, j, k) = wm_interm_132_so_pt3(b, c, j, k) + sum 
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
wm_interm_133_so_pt3(b, c, j, k) = wm_interm_133_so_pt3(b, c, j, k) + sum 
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
wm_interm_134_so_pt3(c, k) = wm_interm_134_so_pt3(c, k) + sum 
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
wm_interm_135_so_pt3(c, j, k, l) = wm_interm_135_so_pt3(c, j, k, l) + sum 
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
wm_interm_136_so_pt3(c, k) = wm_interm_136_so_pt3(c, k) + sum 
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
wm_interm_137_so_pt3(c, k) = wm_interm_137_so_pt3(c, k) + sum 
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
wm_interm_138_so_pt3(c, j, l, k) = wm_interm_138_so_pt3(c, j, l, k) + sum 
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
wm_interm_139_so_pt3(b, c, j, k) = wm_interm_139_so_pt3(b, c, j, k) + sum 
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
wm_interm_140_so_pt3(b, c, j, k) = wm_interm_140_so_pt3(b, c, j, k) + sum 
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
wm_interm_141_so_pt3(b, c, j, k) = wm_interm_141_so_pt3(b, c, j, k) + sum 
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
wm_interm_142_so_pt3(b, c, j, k) = wm_interm_142_so_pt3(b, c, j, k) + sum 
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
wm_interm_143_so_pt3(c, i, j, k, l, m) = wm_interm_143_so_pt3(c, i, j, k, l, m) + sum 
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
wm_interm_144_so_pt3(c, i, j, l, m, k) = wm_interm_144_so_pt3(c, i, j, l, m, k) + sum 
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
do a = nocc + 1, nactive 
do i = 1, nocc 
sum = sum + r2p(vrdav_Rr, b,i,a,j) * s2(a,c,i,j)
end do 
end do 
end do 
wm_interm_145_so_pt3(b, c) = wm_interm_145_so_pt3(b, c) + sum 
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
wm_interm_146_so_pt3(b, c) = wm_interm_146_so_pt3(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_147_so_pt3(b, c, j, k) = wm_interm_147_so_pt3(b, c, j, k) + sum 
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
wm_interm_148_so_pt3(b, c) = wm_interm_148_so_pt3(b, c) + sum 
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
wm_interm_149_so_pt3(i, j, k, l) = wm_interm_149_so_pt3(i, j, k, l) + sum 
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
wm_interm_150_so_pt3(j, k) = wm_interm_150_so_pt3(j, k) + sum 
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
wm_interm_151_so_pt3(j, k) = wm_interm_151_so_pt3(j, k) + sum 
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
wm_interm_152_so_pt3(j, k) = wm_interm_152_so_pt3(j, k) + sum 
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
sum = sum + s2(a,b,i,j) * t3(nocc, nactive, a,b,c,k,l,m)
end do 
end do 
wm_interm_153_so_pt3(c, i, j, k, l, m) = wm_interm_153_so_pt3(c, i, j, k, l, m) + sum 
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
sum = sum + r2m(vrdav_Rr, a,j,b,i) * s2(a,c,i,j)
end do 
end do 
end do 
wm_interm_154_so_pt3(b, c) = wm_interm_154_so_pt3(b, c) + sum 
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
wm_interm_155_so_pt3(b, c) = wm_interm_155_so_pt3(b, c) + sum 
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
wm_interm_156_so_pt3(b, c) = wm_interm_156_so_pt3(b, c) + sum 
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
wm_interm_157_so_pt3(i, j, k, l) = wm_interm_157_so_pt3(i, j, k, l) + sum 
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
wm_interm_158_so_pt3(j, k) = wm_interm_158_so_pt3(j, k) + sum 
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
wm_interm_159_so_pt3(j, k) = wm_interm_159_so_pt3(j, k) + sum 
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
wm_interm_160_so_pt3(j, k) = wm_interm_160_so_pt3(j, k) + sum 
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
wm_interm_161_so_pt3(b, i, j, k) = wm_interm_161_so_pt3(b, i, j, k) + sum 
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
sum = sum + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, b,j,a,k)
end do 
wm_interm_162_so_pt3(b, i, j, k) = wm_interm_162_so_pt3(b, i, j, k) + sum 
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
sum = sum + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, a,j,b,k)
end do 
wm_interm_163_so_pt3(b, i, j, k) = wm_interm_163_so_pt3(b, i, j, k) + sum 
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
wm_interm_164_so_pt3(b, j) = wm_interm_164_so_pt3(b, j) + sum 
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
wm_interm_165_so_pt3(b, j) = wm_interm_165_so_pt3(b, j) + sum 
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
wm_interm_166_so_pt3(b, j) = wm_interm_166_so_pt3(b, j) + sum 
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
wm_interm_167_so_pt3(c, k, j, l) = wm_interm_167_so_pt3(c, k, j, l) + sum 
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
wm_interm_168_so_pt3(b, j) = wm_interm_168_so_pt3(b, j) + sum 
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
wm_interm_169_so_pt3(b, j) = wm_interm_169_so_pt3(b, j) + sum 
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
wm_interm_170_so_pt3(b, j) = wm_interm_170_so_pt3(b, j) + sum 
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
wm_interm_171_so_pt3(b, j) = wm_interm_171_so_pt3(b, j) + sum 
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
wm_interm_172_so_pt3(c, i, j, k, l, m) = wm_interm_172_so_pt3(c, i, j, k, l, m) + sum 
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
wm_interm_173_so_pt3(c, j, l, k) = wm_interm_173_so_pt3(c, j, l, k) + sum 
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
wm_interm_174_so_pt3(c, j, k, l) = wm_interm_174_so_pt3(c, j, k, l) + sum 
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
wm_interm_175_so_pt3(c, j, k, l) = wm_interm_175_so_pt3(c, j, k, l) + sum 
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
wm_interm_176_so_pt3(b, j, i, k) = wm_interm_176_so_pt3(b, j, i, k) + sum 
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
wm_interm_177_so_pt3(c, j, k, i, l, m) = wm_interm_177_so_pt3(c, j, k, i, l, m) + sum 
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
wm_interm_178_so_pt3(c, j, k, l) = wm_interm_178_so_pt3(c, j, k, l) + sum 
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
wm_interm_179_so_pt3(b, i, j, k) = wm_interm_179_so_pt3(b, i, j, k) + sum 
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
wm_interm_180_so_pt3(b, i, j, k) = wm_interm_180_so_pt3(b, i, j, k) + sum 
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
wm_interm_181_so_pt3(b, j, i, k) = wm_interm_181_so_pt3(b, j, i, k) + sum 
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
wm_interm_182_so_pt3(c, k) = wm_interm_182_so_pt3(c, k) + sum 
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
wm_interm_183_so_pt3(c, k) = wm_interm_183_so_pt3(c, k) + sum 
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
wm_interm_184_so_pt3(c, k) = wm_interm_184_so_pt3(c, k) + sum 
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
wm_interm_185_so_pt3(c, k) = wm_interm_185_so_pt3(c, k) + sum 
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
wm_interm_186_so_pt3(b, j) = wm_interm_186_so_pt3(b, j) + sum 
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
wm_interm_187_so_pt3(b, j) = wm_interm_187_so_pt3(b, j) + sum 
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
wm_interm_188_so_pt3(b, j) = wm_interm_188_so_pt3(b, j) + sum 
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
wm_interm_189_so_pt3(b, j) = wm_interm_189_so_pt3(b, j) + sum 
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
wm_interm_190_so_pt3(b, j) = wm_interm_190_so_pt3(b, j) + sum 
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
wm_interm_191_so_pt3(b, j) = wm_interm_191_so_pt3(b, j) + sum 
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
wm_interm_192_so_pt3(c, j, k, l) = wm_interm_192_so_pt3(c, j, k, l) + sum 
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
wm_interm_193_so_pt3(c, j, k, l) = wm_interm_193_so_pt3(c, j, k, l) + sum 
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
wm_interm_194_so_pt3(c, j, k, l) = wm_interm_194_so_pt3(c, j, k, l) + sum 
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
wm_interm_195_so_pt3(c, j, k, l) = wm_interm_195_so_pt3(c, j, k, l) + sum 
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
wm_interm_196_so_pt3(c, j, k, l) = wm_interm_196_so_pt3(c, j, k, l) + sum 
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
wm_interm_197_so_pt3(c, j, k, l) = wm_interm_197_so_pt3(c, j, k, l) + sum 
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
wm_interm_198_so_pt3(c, k, j, l) = wm_interm_198_so_pt3(c, k, j, l) + sum 
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
wm_interm_199_so_pt3(c, k, j, l) = wm_interm_199_so_pt3(c, k, j, l) + sum 
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
wm_interm_200_so_pt3(c, k, j, l) = wm_interm_200_so_pt3(c, k, j, l) + sum 
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
wm_interm_201_so_pt3(c, j, k, l) = wm_interm_201_so_pt3(c, j, k, l) + sum 
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
sum = sum + r3(vrdav_Rl, a,j,b,k,c,i) * r2p(vrdav_Rr, a,l,b,m)
end do 
end do 
wm_interm_202_so_pt3(c, j, k, i, l, m) = wm_interm_202_so_pt3(c, j, k, i, l, m) + sum 
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
wm_interm_203_so_pt3(c, j, k, i, l, m) = wm_interm_203_so_pt3(c, j, k, i, l, m) + sum 
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
sum = sum + r3(vrdav_Rl, a,i,b,k,c,j) * r2p(vrdav_Rr, a,l,b,m)
end do 
end do 
wm_interm_204_so_pt3(c, i, k, j, l, m) = wm_interm_204_so_pt3(c, i, k, j, l, m) + sum 
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
wm_interm_205_so_pt3(c, i, j, k, l, m) = wm_interm_205_so_pt3(c, i, j, k, l, m) + sum 
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
wm_interm_206_so_pt3(c, i, j, k, l, m) = wm_interm_206_so_pt3(c, i, j, k, l, m) + sum 
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
wm_interm_207_so_pt3(c, i, k, j, l, m) = wm_interm_207_so_pt3(c, i, k, j, l, m) + sum 
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
sum = sum + r2p(vrdav_Rr, b,i,a,j) * s1(a,k)
end do 
wm_interm_208_so_pt3(b, i, j, k) = wm_interm_208_so_pt3(b, i, j, k) + sum 
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
wm_interm_209_so_pt3(b, i, j, k) = wm_interm_209_so_pt3(b, i, j, k) + sum 
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
wm_interm_210_so_pt3(c, j, k, l) = wm_interm_210_so_pt3(c, j, k, l) + sum 
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
wm_interm_211_so_pt3(c, j, k, l) = wm_interm_211_so_pt3(c, j, k, l) + sum 
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
wm_interm_212_so_pt3(c, j, k, l) = wm_interm_212_so_pt3(c, j, k, l) + sum 
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
wm_interm_213_so_pt3(c, j, k, l) = wm_interm_213_so_pt3(c, j, k, l) + sum 
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
wm_interm_214_so_pt3(c, k, j, l) = wm_interm_214_so_pt3(c, k, j, l) + sum 
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
wm_interm_215_so_pt3(c, k, j, l) = wm_interm_215_so_pt3(c, k, j, l) + sum 
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
sum = sum + r3(vrdav_Rl, a,j,b,k,c,i) * r2m(vrdav_Rr, a,l,b,m)
end do 
end do 
wm_interm_216_so_pt3(c, j, k, i, l, m) = wm_interm_216_so_pt3(c, j, k, i, l, m) + sum 
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
wm_interm_217_so_pt3(c, i, k, j, l, m) = wm_interm_217_so_pt3(c, i, k, j, l, m) + sum 
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
wm_interm_218_so_pt3(c, i, j, k, l, m) = wm_interm_218_so_pt3(c, i, j, k, l, m) + sum 
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
sum = sum + r2m(vrdav_Rr, a,j,b,i) * s1(a,k)
end do 
wm_interm_219_so_pt3(b, j, i, k) = wm_interm_219_so_pt3(b, j, i, k) + sum 
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
wm_interm_220_so_pt3(c, k) = wm_interm_220_so_pt3(c, k) + sum 
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
wm_interm_221_so_pt3(c, k) = wm_interm_221_so_pt3(c, k) + sum 
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
wm_interm_222_so_pt3(c, k) = wm_interm_222_so_pt3(c, k) + sum 
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
wm_interm_223_so_pt3(c, i, j, k, l, m) = wm_interm_223_so_pt3(c, i, j, k, l, m) + sum 
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
wm_interm_224_so_pt3(c, i, k, j, l, m) = wm_interm_224_so_pt3(c, i, k, j, l, m) + sum 
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
wm_interm_225_so_pt3(c, j, k, i, l, m) = wm_interm_225_so_pt3(c, j, k, i, l, m) + sum 
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
wm_interm_226_so_pt3(b, i, j, k) = wm_interm_226_so_pt3(b, i, j, k) + sum 
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
wm_interm_227_so_pt3(c, k) = wm_interm_227_so_pt3(c, k) + sum 
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
wm_interm_228_so_pt3(c, k) = wm_interm_228_so_pt3(c, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 



    end subroutine wm_so_intermediates_cc3_pt3    
    

  end module so_cc3_pt3a
