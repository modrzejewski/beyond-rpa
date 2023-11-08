module tt_ccsd_pt4a
 use ccsd_transformed_integrals
    use cc3_intermediates
    use arithmetic
    use s_gen
    use basis
    use eom_vectors

    implicit none

        !
    ! File generated automatically on 2018-04-18 11:42:14
    !
    real(F64), dimension(:, :), allocatable :: wm_interm_0_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_1_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_2_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_3_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_4_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_5_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_6_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_7_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_8_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_9_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_10_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_11_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_12_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_13_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_14_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_15_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_16_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_17_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_18_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_19_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_20_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_21_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_22_triplet_pt4 
real(F64) :: wm_interm_23_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_24_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_25_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_26_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_27_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_28_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_29_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_30_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_31_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_32_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_33_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_34_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_35_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_36_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_37_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_38_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_39_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_40_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_41_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_42_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_43_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_44_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_45_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_46_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_47_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_48_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_49_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_50_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_51_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_52_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_53_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_54_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_55_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_56_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_57_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_58_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_59_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_60_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_61_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_62_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_63_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_64_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_65_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_66_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_67_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_68_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_69_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_70_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_71_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_72_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_73_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_74_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_75_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_76_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_77_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_78_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_79_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_80_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_81_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_82_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_83_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_84_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_85_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_86_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_87_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_88_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_89_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_90_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_91_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_92_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_93_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_94_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_95_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_96_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_97_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_98_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_99_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_100_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_101_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_102_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_103_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_104_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_105_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_106_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_107_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_108_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_109_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_110_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_111_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_112_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_113_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_114_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_115_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_116_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_117_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_118_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_119_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_120_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_121_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_122_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_123_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_124_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_125_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_126_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_127_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_128_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_129_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_130_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_131_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_132_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_133_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_134_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_135_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_136_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_137_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_138_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_139_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_140_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_141_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_142_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_143_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_144_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_145_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_146_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_147_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_148_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_149_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_150_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_151_triplet_pt4 
real(F64) :: wm_interm_152_triplet_pt4 
real(F64) :: wm_interm_153_triplet_pt4 
real(F64) :: wm_interm_154_triplet_pt4 
real(F64) :: wm_interm_155_triplet_pt4 
real(F64) :: wm_interm_156_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_157_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_158_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_159_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_160_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_161_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_162_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_163_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_164_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_165_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_166_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_167_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_168_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_169_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_170_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_171_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_172_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_173_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_174_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_175_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_176_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_177_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_178_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_179_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_180_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_181_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_182_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_183_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_184_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_185_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_186_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_187_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_188_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_189_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_190_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_191_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_192_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_193_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_194_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_195_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_196_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_197_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_198_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_199_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_200_triplet_pt4 
real(F64) :: wm_interm_201_triplet_pt4 
real(F64) :: wm_interm_202_triplet_pt4 
real(F64) :: wm_interm_203_triplet_pt4 
real(F64) :: wm_interm_204_triplet_pt4 
real(F64) :: wm_interm_205_triplet_pt4 

    contains
    
    subroutine wm_triplet_intermediates_ccsd_init_pt4(nocc, nactive)
    integer, intent(in) :: nocc
    integer, intent(in) :: nactive
    allocate(wm_interm_0_triplet_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_1_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_2_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_3_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_4_triplet_pt4(1: nocc, 1: nocc))
allocate(wm_interm_5_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_6_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_7_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_8_triplet_pt4(1: nocc, 1: nocc))
allocate(wm_interm_9_triplet_pt4(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_10_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_11_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_12_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_13_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_14_triplet_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_15_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_16_triplet_pt4(1: nocc, 1: nocc))
allocate(wm_interm_17_triplet_pt4(1: nocc, 1: nocc))
allocate(wm_interm_18_triplet_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_19_triplet_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_20_triplet_pt4(1: nocc, 1: nocc))
allocate(wm_interm_21_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_22_triplet_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_24_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_25_triplet_pt4(1: nocc, 1: nocc))
allocate(wm_interm_26_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_27_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_28_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_29_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_30_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_31_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_32_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_33_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_34_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_35_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_36_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_37_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_38_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_39_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_40_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_41_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_42_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_43_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_44_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_45_triplet_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_46_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_47_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_48_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_49_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_50_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_51_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_52_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_53_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_54_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_55_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_56_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_57_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_58_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_59_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_60_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_61_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_62_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_63_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_64_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_65_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_66_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_67_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_68_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_69_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_70_triplet_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_71_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_72_triplet_pt4(1: nocc, 1: nocc))
allocate(wm_interm_73_triplet_pt4(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_74_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_75_triplet_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_76_triplet_pt4(1: nocc, 1: nocc))
allocate(wm_interm_77_triplet_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_78_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_79_triplet_pt4(1: nocc, 1: nocc))
allocate(wm_interm_80_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_81_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_82_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_83_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_84_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_85_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_86_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_87_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_88_triplet_pt4(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_89_triplet_pt4(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_90_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_91_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_92_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_93_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_94_triplet_pt4(1: nocc, 1: nocc))
allocate(wm_interm_95_triplet_pt4(1: nocc, 1: nocc))
allocate(wm_interm_96_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_97_triplet_pt4(1: nocc, 1: nocc))
allocate(wm_interm_98_triplet_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_99_triplet_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_100_triplet_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_101_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_102_triplet_pt4(1: nocc, 1: nocc))
allocate(wm_interm_103_triplet_pt4(1: nocc, 1: nocc))
allocate(wm_interm_104_triplet_pt4(1: nocc, 1: nocc))
allocate(wm_interm_105_triplet_pt4(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_106_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_107_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_108_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_109_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_110_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_111_triplet_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_112_triplet_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_113_triplet_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_114_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_115_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_116_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_117_triplet_pt4(1: nocc, 1: nocc))
allocate(wm_interm_118_triplet_pt4(1: nocc, 1: nocc))
allocate(wm_interm_119_triplet_pt4(1: nocc, 1: nocc))
allocate(wm_interm_120_triplet_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_121_triplet_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_122_triplet_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_123_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_124_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_125_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_126_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_127_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_128_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_129_triplet_pt4(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_130_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_131_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_132_triplet_pt4(1: nocc, 1: nocc))
allocate(wm_interm_133_triplet_pt4(1: nocc, 1: nocc))
allocate(wm_interm_134_triplet_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_135_triplet_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_136_triplet_pt4(1: nocc, 1: nocc))
allocate(wm_interm_137_triplet_pt4(1: nocc, 1: nocc))
allocate(wm_interm_138_triplet_pt4(1: nocc, 1: nocc))
allocate(wm_interm_139_triplet_pt4(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_140_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_141_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_142_triplet_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_143_triplet_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_144_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_145_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_146_triplet_pt4(1: nocc, 1: nocc))
allocate(wm_interm_147_triplet_pt4(1: nocc, 1: nocc))
allocate(wm_interm_148_triplet_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_149_triplet_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_150_triplet_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_151_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_157_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_158_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_159_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_160_triplet_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_161_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_162_triplet_pt4(1: nocc, 1: nocc))
allocate(wm_interm_163_triplet_pt4(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_164_triplet_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_165_triplet_pt4(1: nocc, 1: nocc))
allocate(wm_interm_166_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_167_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_168_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_169_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_170_triplet_pt4(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_171_triplet_pt4(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_172_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_173_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_174_triplet_pt4(1: nocc, 1: nocc))
allocate(wm_interm_175_triplet_pt4(1: nocc, 1: nocc))
allocate(wm_interm_176_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_177_triplet_pt4(1: nocc, 1: nocc))
allocate(wm_interm_178_triplet_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_179_triplet_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_180_triplet_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_181_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_182_triplet_pt4(1: nocc, 1: nocc))
allocate(wm_interm_183_triplet_pt4(1: nocc, 1: nocc))
allocate(wm_interm_184_triplet_pt4(1: nocc, 1: nocc))
allocate(wm_interm_185_triplet_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_186_triplet_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_187_triplet_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_188_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_189_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_190_triplet_pt4(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_191_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_192_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_193_triplet_pt4(1: nocc, 1: nocc))
allocate(wm_interm_194_triplet_pt4(1: nocc, 1: nocc))
allocate(wm_interm_195_triplet_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_196_triplet_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_197_triplet_pt4(1: nocc, 1: nocc))
allocate(wm_interm_198_triplet_pt4(1: nocc, 1: nocc))
allocate(wm_interm_199_triplet_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_200_triplet_pt4(nocc+1: nactive, nocc+1: nactive))
wm_interm_0_triplet_pt4 = zero 
wm_interm_1_triplet_pt4 = zero 
wm_interm_2_triplet_pt4 = zero 
wm_interm_3_triplet_pt4 = zero 
wm_interm_4_triplet_pt4 = zero 
wm_interm_5_triplet_pt4 = zero 
wm_interm_6_triplet_pt4 = zero 
wm_interm_7_triplet_pt4 = zero 
wm_interm_8_triplet_pt4 = zero 
wm_interm_9_triplet_pt4 = zero 
wm_interm_10_triplet_pt4 = zero 
wm_interm_11_triplet_pt4 = zero 
wm_interm_12_triplet_pt4 = zero 
wm_interm_13_triplet_pt4 = zero 
wm_interm_14_triplet_pt4 = zero 
wm_interm_15_triplet_pt4 = zero 
wm_interm_16_triplet_pt4 = zero 
wm_interm_17_triplet_pt4 = zero 
wm_interm_18_triplet_pt4 = zero 
wm_interm_19_triplet_pt4 = zero 
wm_interm_20_triplet_pt4 = zero 
wm_interm_21_triplet_pt4 = zero 
wm_interm_22_triplet_pt4 = zero 
wm_interm_23_triplet_pt4 = zero 
wm_interm_24_triplet_pt4 = zero 
wm_interm_25_triplet_pt4 = zero 
wm_interm_26_triplet_pt4 = zero 
wm_interm_27_triplet_pt4 = zero 
wm_interm_28_triplet_pt4 = zero 
wm_interm_29_triplet_pt4 = zero 
wm_interm_30_triplet_pt4 = zero 
wm_interm_31_triplet_pt4 = zero 
wm_interm_32_triplet_pt4 = zero 
wm_interm_33_triplet_pt4 = zero 
wm_interm_34_triplet_pt4 = zero 
wm_interm_35_triplet_pt4 = zero 
wm_interm_36_triplet_pt4 = zero 
wm_interm_37_triplet_pt4 = zero 
wm_interm_38_triplet_pt4 = zero 
wm_interm_39_triplet_pt4 = zero 
wm_interm_40_triplet_pt4 = zero 
wm_interm_41_triplet_pt4 = zero 
wm_interm_42_triplet_pt4 = zero 
wm_interm_43_triplet_pt4 = zero 
wm_interm_44_triplet_pt4 = zero 
wm_interm_45_triplet_pt4 = zero 
wm_interm_46_triplet_pt4 = zero 
wm_interm_47_triplet_pt4 = zero 
wm_interm_48_triplet_pt4 = zero 
wm_interm_49_triplet_pt4 = zero 
wm_interm_50_triplet_pt4 = zero 
wm_interm_51_triplet_pt4 = zero 
wm_interm_52_triplet_pt4 = zero 
wm_interm_53_triplet_pt4 = zero 
wm_interm_54_triplet_pt4 = zero 
wm_interm_55_triplet_pt4 = zero 
wm_interm_56_triplet_pt4 = zero 
wm_interm_57_triplet_pt4 = zero 
wm_interm_58_triplet_pt4 = zero 
wm_interm_59_triplet_pt4 = zero 
wm_interm_60_triplet_pt4 = zero 
wm_interm_61_triplet_pt4 = zero 
wm_interm_62_triplet_pt4 = zero 
wm_interm_63_triplet_pt4 = zero 
wm_interm_64_triplet_pt4 = zero 
wm_interm_65_triplet_pt4 = zero 
wm_interm_66_triplet_pt4 = zero 
wm_interm_67_triplet_pt4 = zero 
wm_interm_68_triplet_pt4 = zero 
wm_interm_69_triplet_pt4 = zero 
wm_interm_70_triplet_pt4 = zero 
wm_interm_71_triplet_pt4 = zero 
wm_interm_72_triplet_pt4 = zero 
wm_interm_73_triplet_pt4 = zero 
wm_interm_74_triplet_pt4 = zero 
wm_interm_75_triplet_pt4 = zero 
wm_interm_76_triplet_pt4 = zero 
wm_interm_77_triplet_pt4 = zero 
wm_interm_78_triplet_pt4 = zero 
wm_interm_79_triplet_pt4 = zero 
wm_interm_80_triplet_pt4 = zero 
wm_interm_81_triplet_pt4 = zero 
wm_interm_82_triplet_pt4 = zero 
wm_interm_83_triplet_pt4 = zero 
wm_interm_84_triplet_pt4 = zero 
wm_interm_85_triplet_pt4 = zero 
wm_interm_86_triplet_pt4 = zero 
wm_interm_87_triplet_pt4 = zero 
wm_interm_88_triplet_pt4 = zero 
wm_interm_89_triplet_pt4 = zero 
wm_interm_90_triplet_pt4 = zero 
wm_interm_91_triplet_pt4 = zero 
wm_interm_92_triplet_pt4 = zero 
wm_interm_93_triplet_pt4 = zero 
wm_interm_94_triplet_pt4 = zero 
wm_interm_95_triplet_pt4 = zero 
wm_interm_96_triplet_pt4 = zero 
wm_interm_97_triplet_pt4 = zero 
wm_interm_98_triplet_pt4 = zero 
wm_interm_99_triplet_pt4 = zero 
wm_interm_100_triplet_pt4 = zero 
wm_interm_101_triplet_pt4 = zero 
wm_interm_102_triplet_pt4 = zero 
wm_interm_103_triplet_pt4 = zero 
wm_interm_104_triplet_pt4 = zero 
wm_interm_105_triplet_pt4 = zero 
wm_interm_106_triplet_pt4 = zero 
wm_interm_107_triplet_pt4 = zero 
wm_interm_108_triplet_pt4 = zero 
wm_interm_109_triplet_pt4 = zero 
wm_interm_110_triplet_pt4 = zero 
wm_interm_111_triplet_pt4 = zero 
wm_interm_112_triplet_pt4 = zero 
wm_interm_113_triplet_pt4 = zero 
wm_interm_114_triplet_pt4 = zero 
wm_interm_115_triplet_pt4 = zero 
wm_interm_116_triplet_pt4 = zero 
wm_interm_117_triplet_pt4 = zero 
wm_interm_118_triplet_pt4 = zero 
wm_interm_119_triplet_pt4 = zero 
wm_interm_120_triplet_pt4 = zero 
wm_interm_121_triplet_pt4 = zero 
wm_interm_122_triplet_pt4 = zero 
wm_interm_123_triplet_pt4 = zero 
wm_interm_124_triplet_pt4 = zero 
wm_interm_125_triplet_pt4 = zero 
wm_interm_126_triplet_pt4 = zero 
wm_interm_127_triplet_pt4 = zero 
wm_interm_128_triplet_pt4 = zero 
wm_interm_129_triplet_pt4 = zero 
wm_interm_130_triplet_pt4 = zero 
wm_interm_131_triplet_pt4 = zero 
wm_interm_132_triplet_pt4 = zero 
wm_interm_133_triplet_pt4 = zero 
wm_interm_134_triplet_pt4 = zero 
wm_interm_135_triplet_pt4 = zero 
wm_interm_136_triplet_pt4 = zero 
wm_interm_137_triplet_pt4 = zero 
wm_interm_138_triplet_pt4 = zero 
wm_interm_139_triplet_pt4 = zero 
wm_interm_140_triplet_pt4 = zero 
wm_interm_141_triplet_pt4 = zero 
wm_interm_142_triplet_pt4 = zero 
wm_interm_143_triplet_pt4 = zero 
wm_interm_144_triplet_pt4 = zero 
wm_interm_145_triplet_pt4 = zero 
wm_interm_146_triplet_pt4 = zero 
wm_interm_147_triplet_pt4 = zero 
wm_interm_148_triplet_pt4 = zero 
wm_interm_149_triplet_pt4 = zero 
wm_interm_150_triplet_pt4 = zero 
wm_interm_151_triplet_pt4 = zero 
wm_interm_152_triplet_pt4 = zero 
wm_interm_153_triplet_pt4 = zero 
wm_interm_154_triplet_pt4 = zero 
wm_interm_155_triplet_pt4 = zero 
wm_interm_156_triplet_pt4 = zero 
wm_interm_157_triplet_pt4 = zero 
wm_interm_158_triplet_pt4 = zero 
wm_interm_159_triplet_pt4 = zero 
wm_interm_160_triplet_pt4 = zero 
wm_interm_161_triplet_pt4 = zero 
wm_interm_162_triplet_pt4 = zero 
wm_interm_163_triplet_pt4 = zero 
wm_interm_164_triplet_pt4 = zero 
wm_interm_165_triplet_pt4 = zero 
wm_interm_166_triplet_pt4 = zero 
wm_interm_167_triplet_pt4 = zero 
wm_interm_168_triplet_pt4 = zero 
wm_interm_169_triplet_pt4 = zero 
wm_interm_170_triplet_pt4 = zero 
wm_interm_171_triplet_pt4 = zero 
wm_interm_172_triplet_pt4 = zero 
wm_interm_173_triplet_pt4 = zero 
wm_interm_174_triplet_pt4 = zero 
wm_interm_175_triplet_pt4 = zero 
wm_interm_176_triplet_pt4 = zero 
wm_interm_177_triplet_pt4 = zero 
wm_interm_178_triplet_pt4 = zero 
wm_interm_179_triplet_pt4 = zero 
wm_interm_180_triplet_pt4 = zero 
wm_interm_181_triplet_pt4 = zero 
wm_interm_182_triplet_pt4 = zero 
wm_interm_183_triplet_pt4 = zero 
wm_interm_184_triplet_pt4 = zero 
wm_interm_185_triplet_pt4 = zero 
wm_interm_186_triplet_pt4 = zero 
wm_interm_187_triplet_pt4 = zero 
wm_interm_188_triplet_pt4 = zero 
wm_interm_189_triplet_pt4 = zero 
wm_interm_190_triplet_pt4 = zero 
wm_interm_191_triplet_pt4 = zero 
wm_interm_192_triplet_pt4 = zero 
wm_interm_193_triplet_pt4 = zero 
wm_interm_194_triplet_pt4 = zero 
wm_interm_195_triplet_pt4 = zero 
wm_interm_196_triplet_pt4 = zero 
wm_interm_197_triplet_pt4 = zero 
wm_interm_198_triplet_pt4 = zero 
wm_interm_199_triplet_pt4 = zero 
wm_interm_200_triplet_pt4 = zero 
wm_interm_201_triplet_pt4 = zero 
wm_interm_202_triplet_pt4 = zero 
wm_interm_203_triplet_pt4 = zero 
wm_interm_204_triplet_pt4 = zero 
wm_interm_205_triplet_pt4 = zero 

    end subroutine wm_triplet_intermediates_ccsd_init_pt4
    
    subroutine wm_triplet_intermediates_ccsd_free_pt4
    deallocate(wm_interm_0_triplet_pt4)
deallocate(wm_interm_1_triplet_pt4)
deallocate(wm_interm_2_triplet_pt4)
deallocate(wm_interm_3_triplet_pt4)
deallocate(wm_interm_4_triplet_pt4)
deallocate(wm_interm_5_triplet_pt4)
deallocate(wm_interm_6_triplet_pt4)
deallocate(wm_interm_7_triplet_pt4)
deallocate(wm_interm_8_triplet_pt4)
deallocate(wm_interm_9_triplet_pt4)
deallocate(wm_interm_10_triplet_pt4)
deallocate(wm_interm_11_triplet_pt4)
deallocate(wm_interm_12_triplet_pt4)
deallocate(wm_interm_13_triplet_pt4)
deallocate(wm_interm_14_triplet_pt4)
deallocate(wm_interm_15_triplet_pt4)
deallocate(wm_interm_16_triplet_pt4)
deallocate(wm_interm_17_triplet_pt4)
deallocate(wm_interm_18_triplet_pt4)
deallocate(wm_interm_19_triplet_pt4)
deallocate(wm_interm_20_triplet_pt4)
deallocate(wm_interm_21_triplet_pt4)
deallocate(wm_interm_22_triplet_pt4)
deallocate(wm_interm_24_triplet_pt4)
deallocate(wm_interm_25_triplet_pt4)
deallocate(wm_interm_26_triplet_pt4)
deallocate(wm_interm_27_triplet_pt4)
deallocate(wm_interm_28_triplet_pt4)
deallocate(wm_interm_29_triplet_pt4)
deallocate(wm_interm_30_triplet_pt4)
deallocate(wm_interm_31_triplet_pt4)
deallocate(wm_interm_32_triplet_pt4)
deallocate(wm_interm_33_triplet_pt4)
deallocate(wm_interm_34_triplet_pt4)
deallocate(wm_interm_35_triplet_pt4)
deallocate(wm_interm_36_triplet_pt4)
deallocate(wm_interm_37_triplet_pt4)
deallocate(wm_interm_38_triplet_pt4)
deallocate(wm_interm_39_triplet_pt4)
deallocate(wm_interm_40_triplet_pt4)
deallocate(wm_interm_41_triplet_pt4)
deallocate(wm_interm_42_triplet_pt4)
deallocate(wm_interm_43_triplet_pt4)
deallocate(wm_interm_44_triplet_pt4)
deallocate(wm_interm_45_triplet_pt4)
deallocate(wm_interm_46_triplet_pt4)
deallocate(wm_interm_47_triplet_pt4)
deallocate(wm_interm_48_triplet_pt4)
deallocate(wm_interm_49_triplet_pt4)
deallocate(wm_interm_50_triplet_pt4)
deallocate(wm_interm_51_triplet_pt4)
deallocate(wm_interm_52_triplet_pt4)
deallocate(wm_interm_53_triplet_pt4)
deallocate(wm_interm_54_triplet_pt4)
deallocate(wm_interm_55_triplet_pt4)
deallocate(wm_interm_56_triplet_pt4)
deallocate(wm_interm_57_triplet_pt4)
deallocate(wm_interm_58_triplet_pt4)
deallocate(wm_interm_59_triplet_pt4)
deallocate(wm_interm_60_triplet_pt4)
deallocate(wm_interm_61_triplet_pt4)
deallocate(wm_interm_62_triplet_pt4)
deallocate(wm_interm_63_triplet_pt4)
deallocate(wm_interm_64_triplet_pt4)
deallocate(wm_interm_65_triplet_pt4)
deallocate(wm_interm_66_triplet_pt4)
deallocate(wm_interm_67_triplet_pt4)
deallocate(wm_interm_68_triplet_pt4)
deallocate(wm_interm_69_triplet_pt4)
deallocate(wm_interm_70_triplet_pt4)
deallocate(wm_interm_71_triplet_pt4)
deallocate(wm_interm_72_triplet_pt4)
deallocate(wm_interm_73_triplet_pt4)
deallocate(wm_interm_74_triplet_pt4)
deallocate(wm_interm_75_triplet_pt4)
deallocate(wm_interm_76_triplet_pt4)
deallocate(wm_interm_77_triplet_pt4)
deallocate(wm_interm_78_triplet_pt4)
deallocate(wm_interm_79_triplet_pt4)
deallocate(wm_interm_80_triplet_pt4)
deallocate(wm_interm_81_triplet_pt4)
deallocate(wm_interm_82_triplet_pt4)
deallocate(wm_interm_83_triplet_pt4)
deallocate(wm_interm_84_triplet_pt4)
deallocate(wm_interm_85_triplet_pt4)
deallocate(wm_interm_86_triplet_pt4)
deallocate(wm_interm_87_triplet_pt4)
deallocate(wm_interm_88_triplet_pt4)
deallocate(wm_interm_89_triplet_pt4)
deallocate(wm_interm_90_triplet_pt4)
deallocate(wm_interm_91_triplet_pt4)
deallocate(wm_interm_92_triplet_pt4)
deallocate(wm_interm_93_triplet_pt4)
deallocate(wm_interm_94_triplet_pt4)
deallocate(wm_interm_95_triplet_pt4)
deallocate(wm_interm_96_triplet_pt4)
deallocate(wm_interm_97_triplet_pt4)
deallocate(wm_interm_98_triplet_pt4)
deallocate(wm_interm_99_triplet_pt4)
deallocate(wm_interm_100_triplet_pt4)
deallocate(wm_interm_101_triplet_pt4)
deallocate(wm_interm_102_triplet_pt4)
deallocate(wm_interm_103_triplet_pt4)
deallocate(wm_interm_104_triplet_pt4)
deallocate(wm_interm_105_triplet_pt4)
deallocate(wm_interm_106_triplet_pt4)
deallocate(wm_interm_107_triplet_pt4)
deallocate(wm_interm_108_triplet_pt4)
deallocate(wm_interm_109_triplet_pt4)
deallocate(wm_interm_110_triplet_pt4)
deallocate(wm_interm_111_triplet_pt4)
deallocate(wm_interm_112_triplet_pt4)
deallocate(wm_interm_113_triplet_pt4)
deallocate(wm_interm_114_triplet_pt4)
deallocate(wm_interm_115_triplet_pt4)
deallocate(wm_interm_116_triplet_pt4)
deallocate(wm_interm_117_triplet_pt4)
deallocate(wm_interm_118_triplet_pt4)
deallocate(wm_interm_119_triplet_pt4)
deallocate(wm_interm_120_triplet_pt4)
deallocate(wm_interm_121_triplet_pt4)
deallocate(wm_interm_122_triplet_pt4)
deallocate(wm_interm_123_triplet_pt4)
deallocate(wm_interm_124_triplet_pt4)
deallocate(wm_interm_125_triplet_pt4)
deallocate(wm_interm_126_triplet_pt4)
deallocate(wm_interm_127_triplet_pt4)
deallocate(wm_interm_128_triplet_pt4)
deallocate(wm_interm_129_triplet_pt4)
deallocate(wm_interm_130_triplet_pt4)
deallocate(wm_interm_131_triplet_pt4)
deallocate(wm_interm_132_triplet_pt4)
deallocate(wm_interm_133_triplet_pt4)
deallocate(wm_interm_134_triplet_pt4)
deallocate(wm_interm_135_triplet_pt4)
deallocate(wm_interm_136_triplet_pt4)
deallocate(wm_interm_137_triplet_pt4)
deallocate(wm_interm_138_triplet_pt4)
deallocate(wm_interm_139_triplet_pt4)
deallocate(wm_interm_140_triplet_pt4)
deallocate(wm_interm_141_triplet_pt4)
deallocate(wm_interm_142_triplet_pt4)
deallocate(wm_interm_143_triplet_pt4)
deallocate(wm_interm_144_triplet_pt4)
deallocate(wm_interm_145_triplet_pt4)
deallocate(wm_interm_146_triplet_pt4)
deallocate(wm_interm_147_triplet_pt4)
deallocate(wm_interm_148_triplet_pt4)
deallocate(wm_interm_149_triplet_pt4)
deallocate(wm_interm_150_triplet_pt4)
deallocate(wm_interm_151_triplet_pt4)
deallocate(wm_interm_157_triplet_pt4)
deallocate(wm_interm_158_triplet_pt4)
deallocate(wm_interm_159_triplet_pt4)
deallocate(wm_interm_160_triplet_pt4)
deallocate(wm_interm_161_triplet_pt4)
deallocate(wm_interm_162_triplet_pt4)
deallocate(wm_interm_163_triplet_pt4)
deallocate(wm_interm_164_triplet_pt4)
deallocate(wm_interm_165_triplet_pt4)
deallocate(wm_interm_166_triplet_pt4)
deallocate(wm_interm_167_triplet_pt4)
deallocate(wm_interm_168_triplet_pt4)
deallocate(wm_interm_169_triplet_pt4)
deallocate(wm_interm_170_triplet_pt4)
deallocate(wm_interm_171_triplet_pt4)
deallocate(wm_interm_172_triplet_pt4)
deallocate(wm_interm_173_triplet_pt4)
deallocate(wm_interm_174_triplet_pt4)
deallocate(wm_interm_175_triplet_pt4)
deallocate(wm_interm_176_triplet_pt4)
deallocate(wm_interm_177_triplet_pt4)
deallocate(wm_interm_178_triplet_pt4)
deallocate(wm_interm_179_triplet_pt4)
deallocate(wm_interm_180_triplet_pt4)
deallocate(wm_interm_181_triplet_pt4)
deallocate(wm_interm_182_triplet_pt4)
deallocate(wm_interm_183_triplet_pt4)
deallocate(wm_interm_184_triplet_pt4)
deallocate(wm_interm_185_triplet_pt4)
deallocate(wm_interm_186_triplet_pt4)
deallocate(wm_interm_187_triplet_pt4)
deallocate(wm_interm_188_triplet_pt4)
deallocate(wm_interm_189_triplet_pt4)
deallocate(wm_interm_190_triplet_pt4)
deallocate(wm_interm_191_triplet_pt4)
deallocate(wm_interm_192_triplet_pt4)
deallocate(wm_interm_193_triplet_pt4)
deallocate(wm_interm_194_triplet_pt4)
deallocate(wm_interm_195_triplet_pt4)
deallocate(wm_interm_196_triplet_pt4)
deallocate(wm_interm_197_triplet_pt4)
deallocate(wm_interm_198_triplet_pt4)
deallocate(wm_interm_199_triplet_pt4)
deallocate(wm_interm_200_triplet_pt4)

    end subroutine wm_triplet_intermediates_ccsd_free_pt4
    
    subroutine wm_triplet_intermediates_ccsd_pt4(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr)
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    real(F64) :: sum
    integer :: i, a, b, c, j, k, l 

    !$omp parallel private(i, a, b, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
sum = zero 
do i = 1, nocc 
sum = sum + r1(vrdav_Rr, a,i) * s1(b,i)
end do 
wm_interm_0_triplet_pt4(a, b) = wm_interm_0_triplet_pt4(a, b) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_1_triplet_pt4(b, c, j, k) = wm_interm_1_triplet_pt4(b, c, j, k) + sum 
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
wm_interm_2_triplet_pt4(b, j) = wm_interm_2_triplet_pt4(b, j) + sum 
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
wm_interm_3_triplet_pt4(b, i, j, k) = wm_interm_3_triplet_pt4(b, i, j, k) + sum 
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
wm_interm_4_triplet_pt4(i, j) = wm_interm_4_triplet_pt4(i, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_5_triplet_pt4(b, c, j, k) = wm_interm_5_triplet_pt4(b, c, j, k) + sum 
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
wm_interm_6_triplet_pt4(b, j) = wm_interm_6_triplet_pt4(b, j) + sum 
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
wm_interm_7_triplet_pt4(b, j) = wm_interm_7_triplet_pt4(b, j) + sum 
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
wm_interm_8_triplet_pt4(i, j) = wm_interm_8_triplet_pt4(i, j) + sum 
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
wm_interm_9_triplet_pt4(i, j, k, l) = wm_interm_9_triplet_pt4(i, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_10_triplet_pt4(b, c, j, k) = wm_interm_10_triplet_pt4(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_11_triplet_pt4(b, c, j, k) = wm_interm_11_triplet_pt4(b, c, j, k) + sum 
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
wm_interm_12_triplet_pt4(b, i, j, k) = wm_interm_12_triplet_pt4(b, i, j, k) + sum 
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
sum = sum + r1(vrdav_Rl, a,i) * t2(a,b,j,k)
end do 
wm_interm_13_triplet_pt4(b, i, j, k) = wm_interm_13_triplet_pt4(b, i, j, k) + sum 
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
wm_interm_14_triplet_pt4(a, b) = wm_interm_14_triplet_pt4(a, b) + sum 
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
wm_interm_15_triplet_pt4(b, j) = wm_interm_15_triplet_pt4(b, j) + sum 
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
wm_interm_16_triplet_pt4(j, k) = wm_interm_16_triplet_pt4(j, k) + sum 
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
wm_interm_17_triplet_pt4(j, k) = wm_interm_17_triplet_pt4(j, k) + sum 
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
wm_interm_18_triplet_pt4(b, c) = wm_interm_18_triplet_pt4(b, c) + sum 
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
wm_interm_19_triplet_pt4(b, c) = wm_interm_19_triplet_pt4(b, c) + sum 
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
wm_interm_20_triplet_pt4(i, j) = wm_interm_20_triplet_pt4(i, j) + sum 
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
wm_interm_21_triplet_pt4(b, j, i, k) = wm_interm_21_triplet_pt4(b, j, i, k) + sum 
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
wm_interm_22_triplet_pt4(a, b) = wm_interm_22_triplet_pt4(a, b) + sum 
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
wm_interm_23_triplet_pt4 = wm_interm_23_triplet_pt4 + sum 
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
wm_interm_24_triplet_pt4(b, i, j, k) = wm_interm_24_triplet_pt4(b, i, j, k) + sum 
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
sum = sum + s1(a,i) * t1(a,j)
end do 
wm_interm_25_triplet_pt4(i, j) = wm_interm_25_triplet_pt4(i, j) + sum 
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
wm_interm_26_triplet_pt4(b, i, j, k) = wm_interm_26_triplet_pt4(b, i, j, k) + sum 
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
wm_interm_27_triplet_pt4(b, i, j, k) = wm_interm_27_triplet_pt4(b, i, j, k) + sum 
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
wm_interm_28_triplet_pt4(b, i, j, k) = wm_interm_28_triplet_pt4(b, i, j, k) + sum 
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
wm_interm_29_triplet_pt4(b, i, j, k) = wm_interm_29_triplet_pt4(b, i, j, k) + sum 
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
wm_interm_30_triplet_pt4(b, j, i, k) = wm_interm_30_triplet_pt4(b, j, i, k) + sum 
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
wm_interm_31_triplet_pt4(b, i, j, k) = wm_interm_31_triplet_pt4(b, i, j, k) + sum 
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
wm_interm_32_triplet_pt4(a, b, i, j) = wm_interm_32_triplet_pt4(a, b, i, j) + sum 
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
sum = sum + r2p(vrdav_Rr, b,j,a,i) * s1(a,i)
end do 
end do 
wm_interm_33_triplet_pt4(b, j) = wm_interm_33_triplet_pt4(b, j) + sum 
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
wm_interm_34_triplet_pt4(b, j) = wm_interm_34_triplet_pt4(b, j) + sum 
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
sum = sum + s1(a,i) * s1(b,j)
wm_interm_35_triplet_pt4(a, b, i, j) = wm_interm_35_triplet_pt4(a, b, i, j) + sum 
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
sum = sum + r2p(vrdav_Rr, b,i,a,j) * s1(a,i)
end do 
end do 
wm_interm_36_triplet_pt4(b, j) = wm_interm_36_triplet_pt4(b, j) + sum 
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
wm_interm_37_triplet_pt4(b, j) = wm_interm_37_triplet_pt4(b, j) + sum 
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
wm_interm_38_triplet_pt4(b, j) = wm_interm_38_triplet_pt4(b, j) + sum 
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
wm_interm_39_triplet_pt4(b, j) = wm_interm_39_triplet_pt4(b, j) + sum 
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
wm_interm_40_triplet_pt4(b, j) = wm_interm_40_triplet_pt4(b, j) + sum 
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
wm_interm_41_triplet_pt4(b, j) = wm_interm_41_triplet_pt4(b, j) + sum 
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
wm_interm_42_triplet_pt4(b, j) = wm_interm_42_triplet_pt4(b, j) + sum 
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
wm_interm_43_triplet_pt4(b, j) = wm_interm_43_triplet_pt4(b, j) + sum 
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
sum = sum + s1(a,i) * t1(b,j)
wm_interm_44_triplet_pt4(a, b, i, j) = wm_interm_44_triplet_pt4(a, b, i, j) + sum 
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
sum = sum + s1(a,i) * t1(b,i)
end do 
wm_interm_45_triplet_pt4(a, b) = wm_interm_45_triplet_pt4(a, b) + sum 
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
wm_interm_46_triplet_pt4(b, j) = wm_interm_46_triplet_pt4(b, j) + sum 
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
wm_interm_47_triplet_pt4(b, i, j, k) = wm_interm_47_triplet_pt4(b, i, j, k) + sum 
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
wm_interm_48_triplet_pt4(b, j) = wm_interm_48_triplet_pt4(b, j) + sum 
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
wm_interm_49_triplet_pt4(b, i, j, k) = wm_interm_49_triplet_pt4(b, i, j, k) + sum 
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
wm_interm_50_triplet_pt4(b, j) = wm_interm_50_triplet_pt4(b, j) + sum 
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
wm_interm_51_triplet_pt4(b, i, j, k) = wm_interm_51_triplet_pt4(b, i, j, k) + sum 
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
wm_interm_52_triplet_pt4(b, i, j, k) = wm_interm_52_triplet_pt4(b, i, j, k) + sum 
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
wm_interm_53_triplet_pt4(b, j) = wm_interm_53_triplet_pt4(b, j) + sum 
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
wm_interm_54_triplet_pt4(b, j) = wm_interm_54_triplet_pt4(b, j) + sum 
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
wm_interm_55_triplet_pt4(b, j) = wm_interm_55_triplet_pt4(b, j) + sum 
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
wm_interm_56_triplet_pt4(a, b, i, j) = wm_interm_56_triplet_pt4(a, b, i, j) + sum 
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
sum = sum + t1(a,i) * t1(b,j)
wm_interm_57_triplet_pt4(a, b, i, j) = wm_interm_57_triplet_pt4(a, b, i, j) + sum 
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
sum = sum + r2m(vrdav_Rl, a,j,b,i) * r1(vrdav_Rr, a,i)
end do 
end do 
wm_interm_58_triplet_pt4(b, j) = wm_interm_58_triplet_pt4(b, j) + sum 
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
wm_interm_59_triplet_pt4(b, i, j, k) = wm_interm_59_triplet_pt4(b, i, j, k) + sum 
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
wm_interm_60_triplet_pt4(b, j) = wm_interm_60_triplet_pt4(b, j) + sum 
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
wm_interm_61_triplet_pt4(b, j, i, k) = wm_interm_61_triplet_pt4(b, j, i, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,j,b,i) * r1(vrdav_Rr, a,k)
end do 
wm_interm_62_triplet_pt4(b, j, i, k) = wm_interm_62_triplet_pt4(b, j, i, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * t1(a,k)
end do 
wm_interm_63_triplet_pt4(b, i, j, k) = wm_interm_63_triplet_pt4(b, i, j, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * t1(a,i)
end do 
end do 
wm_interm_64_triplet_pt4(b, j) = wm_interm_64_triplet_pt4(b, j) + sum 
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
wm_interm_65_triplet_pt4(b, j) = wm_interm_65_triplet_pt4(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_66_triplet_pt4(b, c, j, k) = wm_interm_66_triplet_pt4(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
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
wm_interm_67_triplet_pt4(b, c, j, k) = wm_interm_67_triplet_pt4(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_68_triplet_pt4(b, c, j, k) = wm_interm_68_triplet_pt4(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_69_triplet_pt4(b, c, j, k) = wm_interm_69_triplet_pt4(b, c, j, k) + sum 
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
wm_interm_70_triplet_pt4(b, c) = wm_interm_70_triplet_pt4(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_71_triplet_pt4(b, c, j, k) = wm_interm_71_triplet_pt4(b, c, j, k) + sum 
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
wm_interm_72_triplet_pt4(j, k) = wm_interm_72_triplet_pt4(j, k) + sum 
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
wm_interm_73_triplet_pt4(i, j, k, l) = wm_interm_73_triplet_pt4(i, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_74_triplet_pt4(b, c, j, k) = wm_interm_74_triplet_pt4(b, c, j, k) + sum 
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
wm_interm_75_triplet_pt4(b, c) = wm_interm_75_triplet_pt4(b, c) + sum 
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
wm_interm_76_triplet_pt4(j, k) = wm_interm_76_triplet_pt4(j, k) + sum 
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
wm_interm_77_triplet_pt4(b, c) = wm_interm_77_triplet_pt4(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_78_triplet_pt4(b, c, j, k) = wm_interm_78_triplet_pt4(b, c, j, k) + sum 
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
wm_interm_79_triplet_pt4(j, k) = wm_interm_79_triplet_pt4(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_80_triplet_pt4(b, c, j, k) = wm_interm_80_triplet_pt4(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
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
wm_interm_81_triplet_pt4(b, c, j, k) = wm_interm_81_triplet_pt4(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_82_triplet_pt4(b, c, j, k) = wm_interm_82_triplet_pt4(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
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
wm_interm_83_triplet_pt4(b, c, j, k) = wm_interm_83_triplet_pt4(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
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
wm_interm_84_triplet_pt4(b, c, j, k) = wm_interm_84_triplet_pt4(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_85_triplet_pt4(b, c, j, k) = wm_interm_85_triplet_pt4(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_86_triplet_pt4(b, c, j, k) = wm_interm_86_triplet_pt4(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_87_triplet_pt4(b, c, j, k) = wm_interm_87_triplet_pt4(b, c, j, k) + sum 
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
sum = sum + r2p(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, a,k,b,l)
end do 
end do 
wm_interm_88_triplet_pt4(i, j, k, l) = wm_interm_88_triplet_pt4(i, j, k, l) + sum 
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
wm_interm_89_triplet_pt4(i, j, k, l) = wm_interm_89_triplet_pt4(i, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_90_triplet_pt4(b, c, j, k) = wm_interm_90_triplet_pt4(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_91_triplet_pt4(b, c, j, k) = wm_interm_91_triplet_pt4(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_92_triplet_pt4(b, c, j, k) = wm_interm_92_triplet_pt4(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_93_triplet_pt4(b, c, j, k) = wm_interm_93_triplet_pt4(b, c, j, k) + sum 
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
wm_interm_94_triplet_pt4(j, k) = wm_interm_94_triplet_pt4(j, k) + sum 
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
wm_interm_95_triplet_pt4(j, k) = wm_interm_95_triplet_pt4(j, k) + sum 
end do 
end do 
!$omp end do nowait 
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
wm_interm_96_triplet_pt4(b, c, j, k) = wm_interm_96_triplet_pt4(b, c, j, k) + sum 
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
wm_interm_97_triplet_pt4(j, k) = wm_interm_97_triplet_pt4(j, k) + sum 
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
wm_interm_98_triplet_pt4(b, c) = wm_interm_98_triplet_pt4(b, c) + sum 
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
wm_interm_99_triplet_pt4(b, c) = wm_interm_99_triplet_pt4(b, c) + sum 
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
wm_interm_100_triplet_pt4(b, c) = wm_interm_100_triplet_pt4(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_101_triplet_pt4(b, c, j, k) = wm_interm_101_triplet_pt4(b, c, j, k) + sum 
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
wm_interm_102_triplet_pt4(j, k) = wm_interm_102_triplet_pt4(j, k) + sum 
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
wm_interm_103_triplet_pt4(j, k) = wm_interm_103_triplet_pt4(j, k) + sum 
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
wm_interm_104_triplet_pt4(j, k) = wm_interm_104_triplet_pt4(j, k) + sum 
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
wm_interm_105_triplet_pt4(i, j, k, l) = wm_interm_105_triplet_pt4(i, j, k, l) + sum 
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
wm_interm_106_triplet_pt4(b, i, k, j) = wm_interm_106_triplet_pt4(b, i, k, j) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_107_triplet_pt4(b, c, j, k) = wm_interm_107_triplet_pt4(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_108_triplet_pt4(b, c, j, k) = wm_interm_108_triplet_pt4(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_109_triplet_pt4(b, c, j, k) = wm_interm_109_triplet_pt4(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_110_triplet_pt4(b, c, j, k) = wm_interm_110_triplet_pt4(b, c, j, k) + sum 
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
wm_interm_111_triplet_pt4(b, c) = wm_interm_111_triplet_pt4(b, c) + sum 
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
wm_interm_112_triplet_pt4(b, c) = wm_interm_112_triplet_pt4(b, c) + sum 
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
wm_interm_113_triplet_pt4(b, c) = wm_interm_113_triplet_pt4(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_114_triplet_pt4(b, c, j, k) = wm_interm_114_triplet_pt4(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_115_triplet_pt4(b, c, j, k) = wm_interm_115_triplet_pt4(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_116_triplet_pt4(b, c, j, k) = wm_interm_116_triplet_pt4(b, c, j, k) + sum 
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
wm_interm_117_triplet_pt4(j, k) = wm_interm_117_triplet_pt4(j, k) + sum 
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
wm_interm_118_triplet_pt4(j, k) = wm_interm_118_triplet_pt4(j, k) + sum 
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
wm_interm_119_triplet_pt4(j, k) = wm_interm_119_triplet_pt4(j, k) + sum 
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
wm_interm_120_triplet_pt4(b, c) = wm_interm_120_triplet_pt4(b, c) + sum 
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
wm_interm_121_triplet_pt4(b, c) = wm_interm_121_triplet_pt4(b, c) + sum 
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
wm_interm_122_triplet_pt4(b, c) = wm_interm_122_triplet_pt4(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_123_triplet_pt4(b, c, j, k) = wm_interm_123_triplet_pt4(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
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
wm_interm_124_triplet_pt4(b, c, j, k) = wm_interm_124_triplet_pt4(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_125_triplet_pt4(b, c, j, k) = wm_interm_125_triplet_pt4(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_126_triplet_pt4(b, c, j, k) = wm_interm_126_triplet_pt4(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_127_triplet_pt4(b, c, j, k) = wm_interm_127_triplet_pt4(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_128_triplet_pt4(b, c, j, k) = wm_interm_128_triplet_pt4(b, c, j, k) + sum 
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
sum = sum + r2p(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,k,b,l)
end do 
end do 
wm_interm_129_triplet_pt4(i, j, k, l) = wm_interm_129_triplet_pt4(i, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_130_triplet_pt4(b, c, j, k) = wm_interm_130_triplet_pt4(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_131_triplet_pt4(b, c, j, k) = wm_interm_131_triplet_pt4(b, c, j, k) + sum 
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
wm_interm_132_triplet_pt4(j, k) = wm_interm_132_triplet_pt4(j, k) + sum 
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
wm_interm_133_triplet_pt4(j, k) = wm_interm_133_triplet_pt4(j, k) + sum 
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
wm_interm_134_triplet_pt4(b, c) = wm_interm_134_triplet_pt4(b, c) + sum 
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
wm_interm_135_triplet_pt4(b, c) = wm_interm_135_triplet_pt4(b, c) + sum 
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
wm_interm_136_triplet_pt4(j, k) = wm_interm_136_triplet_pt4(j, k) + sum 
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
wm_interm_137_triplet_pt4(j, k) = wm_interm_137_triplet_pt4(j, k) + sum 
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
wm_interm_138_triplet_pt4(j, k) = wm_interm_138_triplet_pt4(j, k) + sum 
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
wm_interm_139_triplet_pt4(i, j, k, l) = wm_interm_139_triplet_pt4(i, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_140_triplet_pt4(b, c, j, k) = wm_interm_140_triplet_pt4(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_141_triplet_pt4(b, c, j, k) = wm_interm_141_triplet_pt4(b, c, j, k) + sum 
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
wm_interm_142_triplet_pt4(b, c) = wm_interm_142_triplet_pt4(b, c) + sum 
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
wm_interm_143_triplet_pt4(b, c) = wm_interm_143_triplet_pt4(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_144_triplet_pt4(b, c, j, k) = wm_interm_144_triplet_pt4(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_145_triplet_pt4(b, c, j, k) = wm_interm_145_triplet_pt4(b, c, j, k) + sum 
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
wm_interm_146_triplet_pt4(j, k) = wm_interm_146_triplet_pt4(j, k) + sum 
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
wm_interm_147_triplet_pt4(j, k) = wm_interm_147_triplet_pt4(j, k) + sum 
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
wm_interm_148_triplet_pt4(b, c) = wm_interm_148_triplet_pt4(b, c) + sum 
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
wm_interm_149_triplet_pt4(b, c) = wm_interm_149_triplet_pt4(b, c) + sum 
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
wm_interm_150_triplet_pt4(b, c) = wm_interm_150_triplet_pt4(b, c) + sum 
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
sum = sum + s2(a,b,i,j) * t1(a,k)
end do 
wm_interm_151_triplet_pt4(b, i, j, k) = wm_interm_151_triplet_pt4(b, i, j, k) + sum 
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
wm_interm_152_triplet_pt4 = wm_interm_152_triplet_pt4 + sum 
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
wm_interm_153_triplet_pt4 = wm_interm_153_triplet_pt4 + sum 
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
wm_interm_154_triplet_pt4 = wm_interm_154_triplet_pt4 + sum 
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
wm_interm_155_triplet_pt4 = wm_interm_155_triplet_pt4 + sum 
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
wm_interm_156_triplet_pt4 = wm_interm_156_triplet_pt4 + sum 
!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_157_triplet_pt4(b, c, j, k) = wm_interm_157_triplet_pt4(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_158_triplet_pt4(b, c, j, k) = wm_interm_158_triplet_pt4(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_159_triplet_pt4(b, c, j, k) = wm_interm_159_triplet_pt4(b, c, j, k) + sum 
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
wm_interm_160_triplet_pt4(b, c) = wm_interm_160_triplet_pt4(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_161_triplet_pt4(b, c, j, k) = wm_interm_161_triplet_pt4(b, c, j, k) + sum 
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
wm_interm_162_triplet_pt4(j, k) = wm_interm_162_triplet_pt4(j, k) + sum 
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
wm_interm_163_triplet_pt4(i, j, k, l) = wm_interm_163_triplet_pt4(i, j, k, l) + sum 
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
wm_interm_164_triplet_pt4(b, c) = wm_interm_164_triplet_pt4(b, c) + sum 
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
wm_interm_165_triplet_pt4(j, k) = wm_interm_165_triplet_pt4(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_166_triplet_pt4(b, c, j, k) = wm_interm_166_triplet_pt4(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_167_triplet_pt4(b, c, j, k) = wm_interm_167_triplet_pt4(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_168_triplet_pt4(b, c, j, k) = wm_interm_168_triplet_pt4(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_169_triplet_pt4(b, c, j, k) = wm_interm_169_triplet_pt4(b, c, j, k) + sum 
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
wm_interm_170_triplet_pt4(i, j, k, l) = wm_interm_170_triplet_pt4(i, j, k, l) + sum 
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
wm_interm_171_triplet_pt4(i, j, k, l) = wm_interm_171_triplet_pt4(i, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_172_triplet_pt4(b, c, j, k) = wm_interm_172_triplet_pt4(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_173_triplet_pt4(b, c, j, k) = wm_interm_173_triplet_pt4(b, c, j, k) + sum 
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
wm_interm_174_triplet_pt4(j, k) = wm_interm_174_triplet_pt4(j, k) + sum 
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
wm_interm_175_triplet_pt4(j, k) = wm_interm_175_triplet_pt4(j, k) + sum 
end do 
end do 
!$omp end do nowait 
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
wm_interm_176_triplet_pt4(b, c, j, k) = wm_interm_176_triplet_pt4(b, c, j, k) + sum 
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
wm_interm_177_triplet_pt4(j, k) = wm_interm_177_triplet_pt4(j, k) + sum 
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
wm_interm_178_triplet_pt4(b, c) = wm_interm_178_triplet_pt4(b, c) + sum 
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
wm_interm_179_triplet_pt4(b, c) = wm_interm_179_triplet_pt4(b, c) + sum 
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
wm_interm_180_triplet_pt4(b, c) = wm_interm_180_triplet_pt4(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_181_triplet_pt4(b, c, j, k) = wm_interm_181_triplet_pt4(b, c, j, k) + sum 
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
wm_interm_182_triplet_pt4(j, k) = wm_interm_182_triplet_pt4(j, k) + sum 
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
wm_interm_183_triplet_pt4(j, k) = wm_interm_183_triplet_pt4(j, k) + sum 
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
wm_interm_184_triplet_pt4(j, k) = wm_interm_184_triplet_pt4(j, k) + sum 
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
wm_interm_185_triplet_pt4(b, c) = wm_interm_185_triplet_pt4(b, c) + sum 
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
wm_interm_186_triplet_pt4(b, c) = wm_interm_186_triplet_pt4(b, c) + sum 
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
wm_interm_187_triplet_pt4(b, c) = wm_interm_187_triplet_pt4(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_188_triplet_pt4(b, c, j, k) = wm_interm_188_triplet_pt4(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_189_triplet_pt4(b, c, j, k) = wm_interm_189_triplet_pt4(b, c, j, k) + sum 
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
wm_interm_190_triplet_pt4(i, j, k, l) = wm_interm_190_triplet_pt4(i, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_191_triplet_pt4(b, c, j, k) = wm_interm_191_triplet_pt4(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_192_triplet_pt4(b, c, j, k) = wm_interm_192_triplet_pt4(b, c, j, k) + sum 
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
wm_interm_193_triplet_pt4(j, k) = wm_interm_193_triplet_pt4(j, k) + sum 
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
wm_interm_194_triplet_pt4(j, k) = wm_interm_194_triplet_pt4(j, k) + sum 
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
wm_interm_195_triplet_pt4(b, c) = wm_interm_195_triplet_pt4(b, c) + sum 
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
wm_interm_196_triplet_pt4(b, c) = wm_interm_196_triplet_pt4(b, c) + sum 
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
wm_interm_197_triplet_pt4(j, k) = wm_interm_197_triplet_pt4(j, k) + sum 
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
wm_interm_198_triplet_pt4(j, k) = wm_interm_198_triplet_pt4(j, k) + sum 
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
wm_interm_199_triplet_pt4(b, c) = wm_interm_199_triplet_pt4(b, c) + sum 
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
wm_interm_200_triplet_pt4(b, c) = wm_interm_200_triplet_pt4(b, c) + sum 
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
wm_interm_201_triplet_pt4 = wm_interm_201_triplet_pt4 + sum 
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
wm_interm_202_triplet_pt4 = wm_interm_202_triplet_pt4 + sum 
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
wm_interm_203_triplet_pt4 = wm_interm_203_triplet_pt4 + sum 
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
wm_interm_204_triplet_pt4 = wm_interm_204_triplet_pt4 + sum 
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
wm_interm_205_triplet_pt4 = wm_interm_205_triplet_pt4 + sum 


    end subroutine wm_triplet_intermediates_ccsd_pt4
    
    
    
    
    function calc_D_vv_wm_triplet_pt4(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vv_wm_triplet_pt4
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , k, j, i, a, l, b 
    real(F64), dimension(0:1240) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(0) = term(0) + wm_interm_27_triplet_pt4(p,i,j,k) * wm_interm_49_triplet_pt4(q,j,i,k)
term(1) = term(1) + wm_interm_27_triplet_pt4(p,i,j,k) * wm_interm_49_triplet_pt4(q,i,j,k)
term(2) = term(2) + wm_interm_28_triplet_pt4(p,i,j,k) * wm_interm_49_triplet_pt4(q,i,j,k)
term(3) = term(3) + wm_interm_27_triplet_pt4(p,i,j,k) * wm_interm_52_triplet_pt4(q,i,j,k)
term(4) = term(4) + wm_interm_28_triplet_pt4(p,i,j,k) * wm_interm_52_triplet_pt4(q,j,i,k)
term(5) = term(5) + wm_interm_28_triplet_pt4(p,i,j,k) * wm_interm_52_triplet_pt4(q,i,j,k)
term(6) = term(6) + wm_interm_30_triplet_pt4(p,i,j,k) * wm_interm_49_triplet_pt4(q,j,i,k)
term(7) = term(7) + wm_interm_30_triplet_pt4(p,i,j,k) * wm_interm_49_triplet_pt4(q,i,j,k)
term(8) = term(8) + wm_interm_30_triplet_pt4(p,i,j,k) * wm_interm_52_triplet_pt4(q,i,j,k)
term(9) = term(9) + wm_interm_30_triplet_pt4(p,i,j,k) * wm_interm_52_triplet_pt4(q,j,i,k)
term(10) = term(10) + wm_interm_27_triplet_pt4(p,i,j,k) * wm_interm_61_triplet_pt4(q,j,i,k)
term(11) = term(11) + wm_interm_27_triplet_pt4(p,i,j,k) * wm_interm_61_triplet_pt4(q,i,j,k)
term(12) = term(12) + wm_interm_28_triplet_pt4(p,i,j,k) * wm_interm_61_triplet_pt4(q,i,j,k)
term(13) = term(13) + wm_interm_27_triplet_pt4(p,i,j,k) * wm_interm_63_triplet_pt4(q,i,j,k)
term(14) = term(14) + wm_interm_28_triplet_pt4(p,i,j,k) * wm_interm_63_triplet_pt4(q,j,i,k)
term(15) = term(15) + wm_interm_28_triplet_pt4(p,i,j,k) * wm_interm_63_triplet_pt4(q,i,j,k)
term(16) = term(16) + wm_interm_30_triplet_pt4(p,i,j,k) * wm_interm_61_triplet_pt4(q,j,i,k)
term(17) = term(17) + wm_interm_30_triplet_pt4(p,i,j,k) * wm_interm_61_triplet_pt4(q,i,j,k)
term(18) = term(18) + wm_interm_30_triplet_pt4(p,i,j,k) * wm_interm_63_triplet_pt4(q,i,j,k)
term(19) = term(19) + wm_interm_30_triplet_pt4(p,i,j,k) * wm_interm_63_triplet_pt4(q,j,i,k)
end do 
end do 
end do 

term(0) = term(0) * (0.5d+0) 
term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (0.5d+0) 
term(3) = term(3) * (0.5d+0) 
term(4) = term(4) * (0.5d+0) 
term(5) = term(5) * (-1.0d+0) 
term(6) = term(6) * (2.0d+0) 
term(7) = term(7) * (-2.0d+0) 
term(8) = term(8) * (2.0d+0) 
term(9) = term(9) * (-2.0d+0) 
term(11) = term(11) * (-2.0d+0) 
term(15) = term(15) * (-2.0d+0) 
term(16) = term(16) * (4.0d+0) 
term(17) = term(17) * (-4.0d+0) 
term(18) = term(18) * (4.0d+0) 
term(19) = term(19) * (-4.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(20) = term(20) + r1(vrdav_Rl, q,i) * wm_interm_27_triplet_pt4(p,j,k,l) * wm_interm_9_triplet_pt4(k,j,i,l)
term(21) = term(21) + r1(vrdav_Rl, q,i) * wm_interm_27_triplet_pt4(p,j,k,l) * wm_interm_9_triplet_pt4(j,k,i,l)
term(22) = term(22) + r1(vrdav_Rl, q,i) * wm_interm_28_triplet_pt4(p,j,k,l) * wm_interm_9_triplet_pt4(j,k,i,l)
term(23) = term(23) + r1(vrdav_Rl, q,i) * wm_interm_30_triplet_pt4(p,j,k,l) * wm_interm_9_triplet_pt4(k,j,i,l)
term(24) = term(24) + r1(vrdav_Rl, q,i) * wm_interm_30_triplet_pt4(p,j,k,l) * wm_interm_9_triplet_pt4(j,k,i,l)
end do 
end do 
end do 
end do 

term(20) = term(20) * (-2.0d+0) 
term(21) = term(21) * (4.0d+0) 
term(22) = term(22) * (-2.0d+0) 
term(23) = term(23) * (-8.0d+0) 
term(24) = term(24) * (8.0d+0) 

do l = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
term(25) = term(25) + s1(q,i) * wm_interm_24_triplet_pt4(p,j,k,l) * wm_interm_9_triplet_pt4(k,l,j,i)
term(26) = term(26) + s1(q,i) * wm_interm_26_triplet_pt4(p,j,k,l) * wm_interm_9_triplet_pt4(l,k,j,i)
term(27) = term(27) + s1(q,i) * wm_interm_26_triplet_pt4(p,j,k,l) * wm_interm_9_triplet_pt4(k,l,j,i)
term(28) = term(28) + s1(q,i) * wm_interm_29_triplet_pt4(p,j,k,l) * wm_interm_9_triplet_pt4(l,k,j,i)
term(29) = term(29) + s1(q,i) * wm_interm_29_triplet_pt4(p,j,k,l) * wm_interm_9_triplet_pt4(k,l,j,i)
end do 
end do 
end do 
end do 

term(25) = term(25) * (-2.0d+0) 
term(26) = term(26) * (-2.0d+0) 
term(27) = term(27) * (4.0d+0) 
term(28) = term(28) * (-8.0d+0) 
term(29) = term(29) * (8.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
term(30) = term(30) + wm_interm_25_triplet_pt4(i,j) * wm_interm_82_triplet_pt4(q,p,j,i)
term(31) = term(31) + wm_interm_25_triplet_pt4(i,j) * wm_interm_83_triplet_pt4(q,p,j,i)
term(32) = term(32) + wm_interm_25_triplet_pt4(i,j) * wm_interm_96_triplet_pt4(q,p,j,i)
term(33) = term(33) + wm_interm_25_triplet_pt4(i,j) * wm_interm_92_triplet_pt4(q,p,j,i)
term(34) = term(34) + wm_interm_25_triplet_pt4(i,j) * wm_interm_93_triplet_pt4(q,p,j,i)
term(35) = term(35) + wm_interm_25_triplet_pt4(i,j) * wm_interm_81_triplet_pt4(q,p,j,i)
term(36) = term(36) + wm_interm_117_triplet_pt4(i,j) * wm_interm_44_triplet_pt4(p,q,j,i)
term(37) = term(37) + wm_interm_118_triplet_pt4(i,j) * wm_interm_44_triplet_pt4(p,q,j,i)
term(38) = term(38) + wm_interm_119_triplet_pt4(i,j) * wm_interm_44_triplet_pt4(p,q,j,i)
term(39) = term(39) + wm_interm_146_triplet_pt4(i,j) * wm_interm_44_triplet_pt4(p,q,j,i)
term(40) = term(40) + wm_interm_147_triplet_pt4(i,j) * wm_interm_44_triplet_pt4(p,q,j,i)
term(41) = term(41) + wm_interm_132_triplet_pt4(i,j) * wm_interm_44_triplet_pt4(p,q,j,i)
term(42) = term(42) + wm_interm_133_triplet_pt4(i,j) * wm_interm_44_triplet_pt4(p,q,j,i)
term(43) = term(43) + wm_interm_182_triplet_pt4(i,j) * wm_interm_44_triplet_pt4(p,q,j,i)
term(44) = term(44) + wm_interm_183_triplet_pt4(i,j) * wm_interm_44_triplet_pt4(p,q,j,i)
term(45) = term(45) + wm_interm_184_triplet_pt4(i,j) * wm_interm_44_triplet_pt4(p,q,j,i)
term(46) = term(46) + wm_interm_174_triplet_pt4(i,j) * wm_interm_44_triplet_pt4(p,q,j,i)
term(47) = term(47) + wm_interm_175_triplet_pt4(i,j) * wm_interm_44_triplet_pt4(p,q,j,i)
term(48) = term(48) + wm_interm_177_triplet_pt4(i,j) * wm_interm_44_triplet_pt4(p,q,j,i)
term(49) = term(49) + wm_interm_197_triplet_pt4(i,j) * wm_interm_44_triplet_pt4(p,q,j,i)
term(50) = term(50) + wm_interm_198_triplet_pt4(i,j) * wm_interm_44_triplet_pt4(p,q,j,i)
term(51) = term(51) + wm_interm_193_triplet_pt4(i,j) * wm_interm_44_triplet_pt4(p,q,j,i)
term(52) = term(52) + wm_interm_194_triplet_pt4(i,j) * wm_interm_44_triplet_pt4(p,q,j,i)
end do 
end do 

term(30) = term(30) * (0.5d+0) 
term(31) = term(31) * (-1.0d+0) 
term(32) = term(32) * (0.5d+0) 
term(33) = term(33) * (0.5d+0) 
term(34) = term(34) * (-1.0d+0) 
term(35) = term(35) * (0.5d+0) 
term(36) = term(36) * (0.5d+0) 
term(37) = term(37) * (-1.0d+0) 
term(38) = term(38) * (0.5d+0) 
term(39) = term(39) * (2.0d+0) 
term(40) = term(40) * (-2.0d+0) 
term(41) = term(41) * (2.0d+0) 
term(42) = term(42) * (-2.0d+0) 
term(44) = term(44) * (-2.0d+0) 
term(47) = term(47) * (-2.0d+0) 
term(49) = term(49) * (4.0d+0) 
term(50) = term(50) * (-4.0d+0) 
term(51) = term(51) * (4.0d+0) 
term(52) = term(52) * (-4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do l = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(53) = term(53) + r2p(vrdav_Rr, a,i,p,j) * s1(b,i) * s2(a,q,l,k) * wm_interm_13_triplet_pt4(b,j,l,k)
term(54) = term(54) + r2p(vrdav_Rr, a,i,p,j) * s1(b,j) * s2(a,q,l,k) * wm_interm_13_triplet_pt4(b,i,l,k)
term(55) = term(55) + r2m(vrdav_Rr, a,i,p,j) * s1(b,i) * s2(a,q,l,k) * wm_interm_13_triplet_pt4(b,j,l,k)
term(56) = term(56) + r2m(vrdav_Rr, a,i,p,j) * s1(b,j) * s2(a,q,l,k) * wm_interm_13_triplet_pt4(b,i,l,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(53) = term(53) * (-2.0d+0) 
term(54) = term(54) * (4.0d+0) 
term(55) = term(55) * (-4.0d+0) 
term(56) = term(56) * (8.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
do l = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
term(57) = term(57) + r2p(vrdav_Rr, p,i,a,j) * s1(b,j) * s2(a,q,l,k) * wm_interm_13_triplet_pt4(b,i,l,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(57) = term(57) * (-2.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
do i = 1, nocc 
do b = nocc + 1, nactive 
term(58) = term(58) + r2p(vrdav_Rr, p,i,a,j) * s1(b,i) * s2(a,q,l,k) * wm_interm_13_triplet_pt4(b,j,k,l)
term(59) = term(59) + r2p(vrdav_Rr, p,i,a,j) * s1(b,j) * s2(a,q,l,k) * wm_interm_13_triplet_pt4(b,i,k,l)
end do 
end do 
end do 
end do 
end do 
end do 

term(58) = term(58) * (-2.0d+0) 
term(59) = term(59) * (4.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(60) = term(60) + r2p(vrdav_Rr, a,i,p,j) * s1(b,j) * s2(a,q,l,k) * wm_interm_13_triplet_pt4(b,i,k,l)
term(61) = term(61) + r2m(vrdav_Rr, a,i,p,j) * s1(b,j) * s2(a,q,l,k) * wm_interm_13_triplet_pt4(b,i,k,l)
end do 
end do 
end do 
end do 
end do 
end do 

term(60) = term(60) * (-2.0d+0) 
term(61) = term(61) * (-4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(62) = term(62) + wm_interm_44_triplet_pt4(q,a,i,j) * wm_interm_96_triplet_pt4(a,p,j,i)
term(63) = term(63) + wm_interm_44_triplet_pt4(q,a,i,j) * wm_interm_92_triplet_pt4(a,p,j,i)
term(64) = term(64) + wm_interm_44_triplet_pt4(q,a,i,j) * wm_interm_93_triplet_pt4(a,p,j,i)
term(65) = term(65) + wm_interm_110_triplet_pt4(a,p,i,j) * wm_interm_44_triplet_pt4(q,a,j,i)
term(66) = term(66) + wm_interm_107_triplet_pt4(a,p,i,j) * wm_interm_44_triplet_pt4(q,a,j,i)
term(67) = term(67) + wm_interm_108_triplet_pt4(a,p,i,j) * wm_interm_44_triplet_pt4(q,a,j,i)
term(68) = term(68) + wm_interm_44_triplet_pt4(q,a,i,j) * wm_interm_83_triplet_pt4(a,p,j,i)
term(69) = term(69) + wm_interm_44_triplet_pt4(q,a,i,j) * wm_interm_80_triplet_pt4(a,p,j,i)
term(70) = term(70) + wm_interm_44_triplet_pt4(q,a,i,j) * wm_interm_81_triplet_pt4(a,p,j,i)
term(71) = term(71) + wm_interm_130_triplet_pt4(a,p,i,j) * wm_interm_44_triplet_pt4(q,a,j,i)
term(72) = term(72) + wm_interm_131_triplet_pt4(a,p,i,j) * wm_interm_44_triplet_pt4(q,a,j,i)
term(73) = term(73) + wm_interm_140_triplet_pt4(a,p,i,j) * wm_interm_44_triplet_pt4(q,a,j,i)
term(74) = term(74) + wm_interm_141_triplet_pt4(a,p,i,j) * wm_interm_44_triplet_pt4(q,a,j,i)
term(75) = term(75) + wm_interm_123_triplet_pt4(a,p,i,j) * wm_interm_44_triplet_pt4(q,a,j,i)
term(76) = term(76) + wm_interm_124_triplet_pt4(a,p,i,j) * wm_interm_44_triplet_pt4(q,a,j,i)
term(77) = term(77) + wm_interm_44_triplet_pt4(a,q,i,j) * wm_interm_81_triplet_pt4(p,a,j,i)
term(78) = term(78) + wm_interm_108_triplet_pt4(p,a,i,j) * wm_interm_44_triplet_pt4(a,q,j,i)
term(79) = term(79) + wm_interm_44_triplet_pt4(a,q,i,j) * wm_interm_93_triplet_pt4(p,a,j,i)
term(80) = term(80) + wm_interm_44_triplet_pt4(a,q,i,j) * wm_interm_80_triplet_pt4(p,a,j,i)
term(81) = term(81) + wm_interm_107_triplet_pt4(p,a,i,j) * wm_interm_44_triplet_pt4(a,q,j,i)
term(82) = term(82) + wm_interm_44_triplet_pt4(a,q,i,j) * wm_interm_92_triplet_pt4(p,a,j,i)
term(83) = term(83) + wm_interm_44_triplet_pt4(a,q,i,j) * wm_interm_83_triplet_pt4(p,a,j,i)
term(84) = term(84) + wm_interm_110_triplet_pt4(p,a,i,j) * wm_interm_44_triplet_pt4(a,q,j,i)
term(85) = term(85) + wm_interm_44_triplet_pt4(a,q,i,j) * wm_interm_96_triplet_pt4(p,a,j,i)
term(86) = term(86) + wm_interm_124_triplet_pt4(p,a,i,j) * wm_interm_44_triplet_pt4(a,q,j,i)
term(87) = term(87) + wm_interm_141_triplet_pt4(p,a,i,j) * wm_interm_44_triplet_pt4(a,q,j,i)
term(88) = term(88) + wm_interm_131_triplet_pt4(p,a,i,j) * wm_interm_44_triplet_pt4(a,q,j,i)
term(89) = term(89) + wm_interm_123_triplet_pt4(p,a,i,j) * wm_interm_44_triplet_pt4(a,q,j,i)
term(90) = term(90) + wm_interm_140_triplet_pt4(p,a,i,j) * wm_interm_44_triplet_pt4(a,q,j,i)
term(91) = term(91) + wm_interm_130_triplet_pt4(p,a,i,j) * wm_interm_44_triplet_pt4(a,q,j,i)
term(92) = term(92) + wm_interm_176_triplet_pt4(a,p,i,j) * wm_interm_44_triplet_pt4(q,a,j,i)
term(93) = term(93) + wm_interm_172_triplet_pt4(a,p,i,j) * wm_interm_44_triplet_pt4(q,a,j,i)
term(94) = term(94) + wm_interm_173_triplet_pt4(a,p,i,j) * wm_interm_44_triplet_pt4(q,a,j,i)
term(95) = term(95) + wm_interm_169_triplet_pt4(a,p,i,j) * wm_interm_44_triplet_pt4(q,a,j,i)
term(96) = term(96) + wm_interm_166_triplet_pt4(a,p,i,j) * wm_interm_44_triplet_pt4(q,a,j,i)
term(97) = term(97) + wm_interm_167_triplet_pt4(a,p,i,j) * wm_interm_44_triplet_pt4(q,a,j,i)
term(98) = term(98) + wm_interm_191_triplet_pt4(a,p,i,j) * wm_interm_44_triplet_pt4(q,a,j,i)
term(99) = term(99) + wm_interm_192_triplet_pt4(a,p,i,j) * wm_interm_44_triplet_pt4(q,a,j,i)
term(100) = term(100) + wm_interm_188_triplet_pt4(a,p,i,j) * wm_interm_44_triplet_pt4(q,a,j,i)
term(101) = term(101) + wm_interm_189_triplet_pt4(a,p,i,j) * wm_interm_44_triplet_pt4(q,a,j,i)
term(102) = term(102) + wm_interm_167_triplet_pt4(p,a,i,j) * wm_interm_44_triplet_pt4(a,q,j,i)
term(103) = term(103) + wm_interm_173_triplet_pt4(p,a,i,j) * wm_interm_44_triplet_pt4(a,q,j,i)
term(104) = term(104) + wm_interm_166_triplet_pt4(p,a,i,j) * wm_interm_44_triplet_pt4(a,q,j,i)
term(105) = term(105) + wm_interm_172_triplet_pt4(p,a,i,j) * wm_interm_44_triplet_pt4(a,q,j,i)
term(106) = term(106) + wm_interm_169_triplet_pt4(p,a,i,j) * wm_interm_44_triplet_pt4(a,q,j,i)
term(107) = term(107) + wm_interm_176_triplet_pt4(p,a,i,j) * wm_interm_44_triplet_pt4(a,q,j,i)
term(108) = term(108) + wm_interm_189_triplet_pt4(p,a,i,j) * wm_interm_44_triplet_pt4(a,q,j,i)
term(109) = term(109) + wm_interm_192_triplet_pt4(p,a,i,j) * wm_interm_44_triplet_pt4(a,q,j,i)
term(110) = term(110) + wm_interm_188_triplet_pt4(p,a,i,j) * wm_interm_44_triplet_pt4(a,q,j,i)
term(111) = term(111) + wm_interm_191_triplet_pt4(p,a,i,j) * wm_interm_44_triplet_pt4(a,q,j,i)
term(112) = term(112) + s2(a,q,i,j) * wm_interm_15_triplet_pt4(a,j) * wm_interm_33_triplet_pt4(p,i)
term(113) = term(113) + s2(a,q,i,j) * wm_interm_15_triplet_pt4(a,j) * wm_interm_36_triplet_pt4(p,i)
term(114) = term(114) + s2(a,q,i,j) * wm_interm_15_triplet_pt4(a,j) * wm_interm_38_triplet_pt4(p,i)
term(115) = term(115) + s2(a,q,i,j) * wm_interm_15_triplet_pt4(a,j) * wm_interm_40_triplet_pt4(p,i)
term(116) = term(116) + s2(a,q,i,j) * wm_interm_15_triplet_pt4(a,j) * wm_interm_42_triplet_pt4(p,i)
term(117) = term(117) + r1(vrdav_Rl, a,i) * wm_interm_7_triplet_pt4(a,j) * wm_interm_84_triplet_pt4(p,q,i,j)
term(118) = term(118) + r1(vrdav_Rl, a,i) * wm_interm_7_triplet_pt4(a,j) * wm_interm_85_triplet_pt4(p,q,i,j)
term(119) = term(119) + r1(vrdav_Rl, a,i) * wm_interm_7_triplet_pt4(a,j) * wm_interm_86_triplet_pt4(p,q,i,j)
term(120) = term(120) + r1(vrdav_Rl, a,i) * wm_interm_7_triplet_pt4(a,j) * wm_interm_90_triplet_pt4(p,q,i,j)
term(121) = term(121) + r1(vrdav_Rl, a,i) * wm_interm_101_triplet_pt4(p,q,i,j) * wm_interm_7_triplet_pt4(a,j)
term(122) = term(122) + r1(vrdav_Rl, a,i) * wm_interm_7_triplet_pt4(a,j) * wm_interm_91_triplet_pt4(p,q,i,j)
term(123) = term(123) + r1(vrdav_Rl, a,i) * wm_interm_6_triplet_pt4(a,j) * wm_interm_84_triplet_pt4(p,q,i,j)
term(124) = term(124) + r1(vrdav_Rl, a,i) * wm_interm_6_triplet_pt4(a,j) * wm_interm_85_triplet_pt4(p,q,i,j)
term(125) = term(125) + r1(vrdav_Rl, a,i) * wm_interm_6_triplet_pt4(a,j) * wm_interm_86_triplet_pt4(p,q,i,j)
term(126) = term(126) + r1(vrdav_Rl, a,i) * wm_interm_6_triplet_pt4(a,j) * wm_interm_90_triplet_pt4(p,q,i,j)
term(127) = term(127) + r1(vrdav_Rl, a,i) * wm_interm_101_triplet_pt4(p,q,i,j) * wm_interm_6_triplet_pt4(a,j)
term(128) = term(128) + r1(vrdav_Rl, a,i) * wm_interm_6_triplet_pt4(a,j) * wm_interm_91_triplet_pt4(p,q,i,j)
term(129) = term(129) + s1(a,i) * wm_interm_15_triplet_pt4(a,j) * wm_interm_84_triplet_pt4(p,q,i,j)
term(130) = term(130) + s1(a,i) * wm_interm_15_triplet_pt4(a,j) * wm_interm_85_triplet_pt4(p,q,i,j)
term(131) = term(131) + s1(a,i) * wm_interm_15_triplet_pt4(a,j) * wm_interm_86_triplet_pt4(p,q,i,j)
term(132) = term(132) + s1(a,i) * wm_interm_15_triplet_pt4(a,j) * wm_interm_90_triplet_pt4(p,q,i,j)
term(133) = term(133) + s1(a,i) * wm_interm_101_triplet_pt4(p,q,i,j) * wm_interm_15_triplet_pt4(a,j)
term(134) = term(134) + s1(a,i) * wm_interm_15_triplet_pt4(a,j) * wm_interm_91_triplet_pt4(p,q,i,j)
term(135) = term(135) + r1(vrdav_Rl, a,i) * wm_interm_125_triplet_pt4(p,q,i,j) * wm_interm_7_triplet_pt4(a,j)
term(136) = term(136) + r1(vrdav_Rl, a,i) * wm_interm_126_triplet_pt4(p,q,i,j) * wm_interm_7_triplet_pt4(a,j)
term(137) = term(137) + r1(vrdav_Rl, a,i) * wm_interm_127_triplet_pt4(p,q,i,j) * wm_interm_7_triplet_pt4(a,j)
term(138) = term(138) + r1(vrdav_Rl, a,i) * wm_interm_128_triplet_pt4(p,q,i,j) * wm_interm_7_triplet_pt4(a,j)
term(139) = term(139) + r1(vrdav_Rl, a,i) * wm_interm_125_triplet_pt4(p,q,i,j) * wm_interm_6_triplet_pt4(a,j)
term(140) = term(140) + r1(vrdav_Rl, a,i) * wm_interm_126_triplet_pt4(p,q,i,j) * wm_interm_6_triplet_pt4(a,j)
term(141) = term(141) + r1(vrdav_Rl, a,i) * wm_interm_127_triplet_pt4(p,q,i,j) * wm_interm_6_triplet_pt4(a,j)
term(142) = term(142) + r1(vrdav_Rl, a,i) * wm_interm_128_triplet_pt4(p,q,i,j) * wm_interm_6_triplet_pt4(a,j)
term(143) = term(143) + s1(a,i) * wm_interm_125_triplet_pt4(p,q,i,j) * wm_interm_15_triplet_pt4(a,j)
term(144) = term(144) + s1(a,i) * wm_interm_126_triplet_pt4(p,q,i,j) * wm_interm_15_triplet_pt4(a,j)
term(145) = term(145) + s1(a,i) * wm_interm_127_triplet_pt4(p,q,i,j) * wm_interm_15_triplet_pt4(a,j)
term(146) = term(146) + s1(a,i) * wm_interm_128_triplet_pt4(p,q,i,j) * wm_interm_15_triplet_pt4(a,j)
term(147) = term(147) + s1(q,i) * wm_interm_15_triplet_pt4(a,j) * wm_interm_87_triplet_pt4(p,a,i,j)
term(148) = term(148) + s1(q,i) * wm_interm_101_triplet_pt4(p,a,i,j) * wm_interm_15_triplet_pt4(a,j)
term(149) = term(149) + s1(q,i) * wm_interm_15_triplet_pt4(a,j) * wm_interm_90_triplet_pt4(p,a,i,j)
term(150) = term(150) + s1(q,i) * wm_interm_128_triplet_pt4(p,a,i,j) * wm_interm_15_triplet_pt4(a,j)
term(151) = term(151) + s1(q,i) * wm_interm_127_triplet_pt4(p,a,i,j) * wm_interm_15_triplet_pt4(a,j)
term(152) = term(152) + s2(a,p,i,j) * wm_interm_15_triplet_pt4(q,i) * wm_interm_33_triplet_pt4(a,j)
term(153) = term(153) + s2(a,p,i,j) * wm_interm_15_triplet_pt4(q,i) * wm_interm_36_triplet_pt4(a,j)
term(154) = term(154) + s2(a,p,i,j) * wm_interm_15_triplet_pt4(q,i) * wm_interm_38_triplet_pt4(a,j)
term(155) = term(155) + s2(a,p,i,j) * wm_interm_15_triplet_pt4(q,i) * wm_interm_40_triplet_pt4(a,j)
term(156) = term(156) + s2(a,p,i,j) * wm_interm_15_triplet_pt4(q,i) * wm_interm_42_triplet_pt4(a,j)
term(157) = term(157) + s2(a,p,i,j) * wm_interm_39_triplet_pt4(a,j) * wm_interm_7_triplet_pt4(q,i)
term(158) = term(158) + s2(a,p,i,j) * wm_interm_37_triplet_pt4(a,j) * wm_interm_7_triplet_pt4(q,i)
term(159) = term(159) + s2(a,p,i,j) * wm_interm_34_triplet_pt4(a,j) * wm_interm_7_triplet_pt4(q,i)
term(160) = term(160) + s2(a,p,i,j) * wm_interm_39_triplet_pt4(a,j) * wm_interm_6_triplet_pt4(q,i)
term(161) = term(161) + s2(a,p,i,j) * wm_interm_37_triplet_pt4(a,j) * wm_interm_6_triplet_pt4(q,i)
term(162) = term(162) + s2(a,p,i,j) * wm_interm_34_triplet_pt4(a,j) * wm_interm_6_triplet_pt4(q,i)
term(163) = term(163) + s2(a,p,i,j) * wm_interm_43_triplet_pt4(a,j) * wm_interm_7_triplet_pt4(q,i)
term(164) = term(164) + s2(a,p,i,j) * wm_interm_41_triplet_pt4(a,j) * wm_interm_7_triplet_pt4(q,i)
term(165) = term(165) + s2(a,p,i,j) * wm_interm_43_triplet_pt4(a,j) * wm_interm_6_triplet_pt4(q,i)
term(166) = term(166) + s2(a,p,i,j) * wm_interm_41_triplet_pt4(a,j) * wm_interm_6_triplet_pt4(q,i)
term(167) = term(167) + r1(vrdav_Rr, p,i) * wm_interm_53_triplet_pt4(a,j) * wm_interm_5_triplet_pt4(q,a,i,j)
term(168) = term(168) + r1(vrdav_Rr, p,i) * wm_interm_54_triplet_pt4(a,j) * wm_interm_5_triplet_pt4(q,a,i,j)
term(169) = term(169) + r1(vrdav_Rr, p,i) * wm_interm_55_triplet_pt4(a,j) * wm_interm_5_triplet_pt4(q,a,i,j)
term(170) = term(170) + t1(q,i) * wm_interm_10_triplet_pt4(p,a,i,j) * wm_interm_48_triplet_pt4(a,j)
term(171) = term(171) + t1(q,i) * wm_interm_11_triplet_pt4(p,a,i,j) * wm_interm_48_triplet_pt4(a,j)
term(172) = term(172) + t1(q,i) * wm_interm_10_triplet_pt4(p,a,i,j) * wm_interm_50_triplet_pt4(a,j)
term(173) = term(173) + t1(q,i) * wm_interm_11_triplet_pt4(p,a,i,j) * wm_interm_50_triplet_pt4(a,j)
term(174) = term(174) + t1(q,i) * wm_interm_10_triplet_pt4(p,a,i,j) * wm_interm_46_triplet_pt4(a,j)
term(175) = term(175) + t1(q,i) * wm_interm_11_triplet_pt4(p,a,i,j) * wm_interm_46_triplet_pt4(a,j)
term(176) = term(176) + t1(q,i) * wm_interm_1_triplet_pt4(p,a,i,j) * wm_interm_46_triplet_pt4(a,j)
term(177) = term(177) + t1(q,i) * wm_interm_2_triplet_pt4(a,j) * wm_interm_69_triplet_pt4(p,a,i,j)
term(178) = term(178) + t1(q,i) * wm_interm_46_triplet_pt4(a,j) * wm_interm_5_triplet_pt4(p,a,i,j)
term(179) = term(179) + t1(q,i) * wm_interm_48_triplet_pt4(a,j) * wm_interm_5_triplet_pt4(p,a,i,j)
term(180) = term(180) + t1(q,i) * wm_interm_1_triplet_pt4(p,a,i,j) * wm_interm_48_triplet_pt4(a,j)
term(181) = term(181) + t1(q,i) * wm_interm_2_triplet_pt4(a,j) * wm_interm_74_triplet_pt4(p,a,i,j)
term(182) = term(182) + t1(q,i) * wm_interm_50_triplet_pt4(a,j) * wm_interm_5_triplet_pt4(p,a,i,j)
term(183) = term(183) + t1(q,i) * wm_interm_1_triplet_pt4(p,a,i,j) * wm_interm_50_triplet_pt4(a,j)
term(184) = term(184) + t1(q,i) * wm_interm_2_triplet_pt4(a,j) * wm_interm_78_triplet_pt4(p,a,i,j)
term(185) = term(185) + t1(a,i) * wm_interm_2_triplet_pt4(a,j) * wm_interm_78_triplet_pt4(p,q,i,j)
term(186) = term(186) + t1(a,i) * wm_interm_2_triplet_pt4(a,j) * wm_interm_67_triplet_pt4(p,q,i,j)
term(187) = term(187) + t1(a,i) * wm_interm_1_triplet_pt4(p,q,i,j) * wm_interm_50_triplet_pt4(a,j)
term(188) = term(188) + t1(a,i) * wm_interm_50_triplet_pt4(a,j) * wm_interm_5_triplet_pt4(p,q,i,j)
term(189) = term(189) + t1(a,i) * wm_interm_10_triplet_pt4(p,q,i,j) * wm_interm_50_triplet_pt4(a,j)
term(190) = term(190) + t1(a,i) * wm_interm_11_triplet_pt4(p,q,i,j) * wm_interm_50_triplet_pt4(a,j)
term(191) = term(191) + t1(a,i) * wm_interm_10_triplet_pt4(p,q,i,j) * wm_interm_48_triplet_pt4(a,j)
term(192) = term(192) + t1(a,i) * wm_interm_48_triplet_pt4(a,j) * wm_interm_5_triplet_pt4(p,q,i,j)
term(193) = term(193) + t1(a,i) * wm_interm_1_triplet_pt4(p,q,i,j) * wm_interm_48_triplet_pt4(a,j)
term(194) = term(194) + t1(a,i) * wm_interm_11_triplet_pt4(p,q,i,j) * wm_interm_48_triplet_pt4(a,j)
term(195) = term(195) + t1(a,i) * wm_interm_2_triplet_pt4(a,j) * wm_interm_68_triplet_pt4(p,q,i,j)
term(196) = term(196) + t1(a,i) * wm_interm_2_triplet_pt4(a,j) * wm_interm_69_triplet_pt4(p,q,i,j)
term(197) = term(197) + t1(a,i) * wm_interm_2_triplet_pt4(a,j) * wm_interm_71_triplet_pt4(p,q,i,j)
term(198) = term(198) + t1(a,i) * wm_interm_2_triplet_pt4(a,j) * wm_interm_66_triplet_pt4(p,q,i,j)
term(199) = term(199) + t1(a,i) * wm_interm_46_triplet_pt4(a,j) * wm_interm_5_triplet_pt4(p,q,i,j)
term(200) = term(200) + t1(a,i) * wm_interm_10_triplet_pt4(p,q,i,j) * wm_interm_46_triplet_pt4(a,j)
term(201) = term(201) + t1(a,i) * wm_interm_1_triplet_pt4(p,q,i,j) * wm_interm_46_triplet_pt4(a,j)
term(202) = term(202) + t1(a,i) * wm_interm_11_triplet_pt4(p,q,i,j) * wm_interm_46_triplet_pt4(a,j)
term(203) = term(203) + r1(vrdav_Rr, a,i) * wm_interm_10_triplet_pt4(p,q,i,j) * wm_interm_53_triplet_pt4(a,j)
term(204) = term(204) + r1(vrdav_Rr, a,i) * wm_interm_1_triplet_pt4(p,q,i,j) * wm_interm_53_triplet_pt4(a,j)
term(205) = term(205) + r1(vrdav_Rr, a,i) * wm_interm_53_triplet_pt4(a,j) * wm_interm_5_triplet_pt4(p,q,i,j)
term(206) = term(206) + r1(vrdav_Rr, a,i) * wm_interm_11_triplet_pt4(p,q,i,j) * wm_interm_53_triplet_pt4(a,j)
term(207) = term(207) + t2(a,q,i,j) * wm_interm_2_triplet_pt4(a,j) * wm_interm_53_triplet_pt4(p,i)
term(208) = term(208) + r1(vrdav_Rr, a,i) * wm_interm_10_triplet_pt4(p,q,i,j) * wm_interm_54_triplet_pt4(a,j)
term(209) = term(209) + r1(vrdav_Rr, a,i) * wm_interm_1_triplet_pt4(p,q,i,j) * wm_interm_54_triplet_pt4(a,j)
term(210) = term(210) + r1(vrdav_Rr, a,i) * wm_interm_54_triplet_pt4(a,j) * wm_interm_5_triplet_pt4(p,q,i,j)
term(211) = term(211) + r1(vrdav_Rr, a,i) * wm_interm_11_triplet_pt4(p,q,i,j) * wm_interm_54_triplet_pt4(a,j)
term(212) = term(212) + t2(a,q,i,j) * wm_interm_2_triplet_pt4(a,j) * wm_interm_54_triplet_pt4(p,i)
term(213) = term(213) + r1(vrdav_Rr, a,i) * wm_interm_10_triplet_pt4(p,q,i,j) * wm_interm_55_triplet_pt4(a,j)
term(214) = term(214) + r1(vrdav_Rr, a,i) * wm_interm_1_triplet_pt4(p,q,i,j) * wm_interm_55_triplet_pt4(a,j)
term(215) = term(215) + r1(vrdav_Rr, a,i) * wm_interm_55_triplet_pt4(a,j) * wm_interm_5_triplet_pt4(p,q,i,j)
term(216) = term(216) + r1(vrdav_Rr, a,i) * wm_interm_11_triplet_pt4(p,q,i,j) * wm_interm_55_triplet_pt4(a,j)
term(217) = term(217) + t2(a,q,i,j) * wm_interm_2_triplet_pt4(a,j) * wm_interm_55_triplet_pt4(p,i)
term(218) = term(218) + t2(a,q,i,j) * wm_interm_2_triplet_pt4(p,i) * wm_interm_53_triplet_pt4(a,j)
term(219) = term(219) + t2(a,q,i,j) * wm_interm_2_triplet_pt4(p,i) * wm_interm_54_triplet_pt4(a,j)
term(220) = term(220) + t2(a,q,i,j) * wm_interm_2_triplet_pt4(p,i) * wm_interm_55_triplet_pt4(a,j)
term(221) = term(221) + r1(vrdav_Rr, p,i) * wm_interm_5_triplet_pt4(q,a,i,j) * wm_interm_64_triplet_pt4(a,j)
term(222) = term(222) + r1(vrdav_Rr, p,i) * wm_interm_5_triplet_pt4(q,a,i,j) * wm_interm_65_triplet_pt4(a,j)
term(223) = term(223) + t1(q,i) * wm_interm_10_triplet_pt4(p,a,i,j) * wm_interm_60_triplet_pt4(a,j)
term(224) = term(224) + t1(q,i) * wm_interm_11_triplet_pt4(p,a,i,j) * wm_interm_60_triplet_pt4(a,j)
term(225) = term(225) + t1(q,i) * wm_interm_10_triplet_pt4(p,a,i,j) * wm_interm_58_triplet_pt4(a,j)
term(226) = term(226) + t1(q,i) * wm_interm_11_triplet_pt4(p,a,i,j) * wm_interm_58_triplet_pt4(a,j)
term(227) = term(227) + t1(q,i) * wm_interm_1_triplet_pt4(p,a,i,j) * wm_interm_58_triplet_pt4(a,j)
term(228) = term(228) + t1(q,i) * wm_interm_159_triplet_pt4(p,a,i,j) * wm_interm_2_triplet_pt4(a,j)
term(229) = term(229) + t1(q,i) * wm_interm_58_triplet_pt4(a,j) * wm_interm_5_triplet_pt4(p,a,i,j)
term(230) = term(230) + t1(q,i) * wm_interm_5_triplet_pt4(p,a,i,j) * wm_interm_60_triplet_pt4(a,j)
term(231) = term(231) + t1(q,i) * wm_interm_1_triplet_pt4(p,a,i,j) * wm_interm_60_triplet_pt4(a,j)
term(232) = term(232) + t1(q,i) * wm_interm_161_triplet_pt4(p,a,i,j) * wm_interm_2_triplet_pt4(a,j)
term(233) = term(233) + t1(a,i) * wm_interm_161_triplet_pt4(p,q,i,j) * wm_interm_2_triplet_pt4(a,j)
term(234) = term(234) + t1(a,i) * wm_interm_158_triplet_pt4(p,q,i,j) * wm_interm_2_triplet_pt4(a,j)
term(235) = term(235) + t1(a,i) * wm_interm_1_triplet_pt4(p,q,i,j) * wm_interm_58_triplet_pt4(a,j)
term(236) = term(236) + t1(a,i) * wm_interm_58_triplet_pt4(a,j) * wm_interm_5_triplet_pt4(p,q,i,j)
term(237) = term(237) + t1(a,i) * wm_interm_10_triplet_pt4(p,q,i,j) * wm_interm_58_triplet_pt4(a,j)
term(238) = term(238) + t1(a,i) * wm_interm_11_triplet_pt4(p,q,i,j) * wm_interm_58_triplet_pt4(a,j)
term(239) = term(239) + t1(a,i) * wm_interm_10_triplet_pt4(p,q,i,j) * wm_interm_60_triplet_pt4(a,j)
term(240) = term(240) + t1(a,i) * wm_interm_5_triplet_pt4(p,q,i,j) * wm_interm_60_triplet_pt4(a,j)
term(241) = term(241) + t1(a,i) * wm_interm_1_triplet_pt4(p,q,i,j) * wm_interm_60_triplet_pt4(a,j)
term(242) = term(242) + t1(a,i) * wm_interm_11_triplet_pt4(p,q,i,j) * wm_interm_60_triplet_pt4(a,j)
term(243) = term(243) + t1(a,i) * wm_interm_159_triplet_pt4(p,q,i,j) * wm_interm_2_triplet_pt4(a,j)
term(244) = term(244) + t1(a,i) * wm_interm_157_triplet_pt4(p,q,i,j) * wm_interm_2_triplet_pt4(a,j)
term(245) = term(245) + r1(vrdav_Rr, a,i) * wm_interm_10_triplet_pt4(p,q,i,j) * wm_interm_64_triplet_pt4(a,j)
term(246) = term(246) + r1(vrdav_Rr, a,i) * wm_interm_1_triplet_pt4(p,q,i,j) * wm_interm_64_triplet_pt4(a,j)
term(247) = term(247) + r1(vrdav_Rr, a,i) * wm_interm_5_triplet_pt4(p,q,i,j) * wm_interm_64_triplet_pt4(a,j)
term(248) = term(248) + r1(vrdav_Rr, a,i) * wm_interm_11_triplet_pt4(p,q,i,j) * wm_interm_64_triplet_pt4(a,j)
term(249) = term(249) + t2(a,q,i,j) * wm_interm_2_triplet_pt4(a,j) * wm_interm_64_triplet_pt4(p,i)
term(250) = term(250) + r1(vrdav_Rr, a,i) * wm_interm_10_triplet_pt4(p,q,i,j) * wm_interm_65_triplet_pt4(a,j)
term(251) = term(251) + r1(vrdav_Rr, a,i) * wm_interm_1_triplet_pt4(p,q,i,j) * wm_interm_65_triplet_pt4(a,j)
term(252) = term(252) + r1(vrdav_Rr, a,i) * wm_interm_5_triplet_pt4(p,q,i,j) * wm_interm_65_triplet_pt4(a,j)
term(253) = term(253) + r1(vrdav_Rr, a,i) * wm_interm_11_triplet_pt4(p,q,i,j) * wm_interm_65_triplet_pt4(a,j)
term(254) = term(254) + t2(a,q,i,j) * wm_interm_2_triplet_pt4(a,j) * wm_interm_65_triplet_pt4(p,i)
term(255) = term(255) + t2(a,q,i,j) * wm_interm_2_triplet_pt4(p,i) * wm_interm_64_triplet_pt4(a,j)
term(256) = term(256) + t2(a,q,i,j) * wm_interm_2_triplet_pt4(p,i) * wm_interm_65_triplet_pt4(a,j)
end do 
end do 
end do 

term(62) = term(62) * (-1.0d+0) 
term(63) = term(63) * (-1.0d+0) 
term(64) = term(64) * (2.0d+0) 
term(65) = term(65) * (0.5d+0) 
term(66) = term(66) * (0.5d+0) 
term(67) = term(67) * (-1.0d+0) 
term(68) = term(68) * (0.5d+0) 
term(69) = term(69) * (0.5d+0) 
term(70) = term(70) * (-1.0d+0) 
term(71) = term(71) * (-4.0d+0) 
term(72) = term(72) * (4.0d+0) 
term(73) = term(73) * (2.0d+0) 
term(74) = term(74) * (-2.0d+0) 
term(75) = term(75) * (2.0d+0) 
term(76) = term(76) * (-2.0d+0) 
term(79) = term(79) * (-2.0d+0) 
term(80) = term(80) * (-0.5d+0) 
term(81) = term(81) * (-0.5d+0) 
term(83) = term(83) * (-0.5d+0) 
term(84) = term(84) * (-0.5d+0) 
term(86) = term(86) * (2.0d+0) 
term(87) = term(87) * (2.0d+0) 
term(88) = term(88) * (-4.0d+0) 
term(89) = term(89) * (-2.0d+0) 
term(90) = term(90) * (-2.0d+0) 
term(91) = term(91) * (4.0d+0) 
term(92) = term(92) * (-2.0d+0) 
term(93) = term(93) * (-2.0d+0) 
term(94) = term(94) * (4.0d+0) 
term(95) = term(95) * (2.0d+0) 
term(96) = term(96) * (2.0d+0) 
term(97) = term(97) * (-4.0d+0) 
term(98) = term(98) * (-8.0d+0) 
term(99) = term(99) * (8.0d+0) 
term(100) = term(100) * (8.0d+0) 
term(101) = term(101) * (-8.0d+0) 
term(102) = term(102) * (4.0d+0) 
term(103) = term(103) * (-4.0d+0) 
term(104) = term(104) * (-2.0d+0) 
term(105) = term(105) * (2.0d+0) 
term(106) = term(106) * (-2.0d+0) 
term(107) = term(107) * (2.0d+0) 
term(108) = term(108) * (8.0d+0) 
term(109) = term(109) * (-8.0d+0) 
term(110) = term(110) * (-8.0d+0) 
term(111) = term(111) * (8.0d+0) 
term(112) = term(112) * (2.0d+0) 
term(113) = term(113) * (-1.0d+0) 
term(114) = term(114) * (-1.0d+0) 
term(115) = term(115) * (4.0d+0) 
term(116) = term(116) * (-4.0d+0) 
term(117) = term(117) * (-2.0d+0) 
term(118) = term(118) * (4.0d+0) 
term(119) = term(119) * (-2.0d+0) 
term(120) = term(120) * (-2.0d+0) 
term(121) = term(121) * (4.0d+0) 
term(122) = term(122) * (-2.0d+0) 
term(123) = term(123) * (4.0d+0) 
term(124) = term(124) * (-8.0d+0) 
term(125) = term(125) * (4.0d+0) 
term(126) = term(126) * (4.0d+0) 
term(127) = term(127) * (-8.0d+0) 
term(128) = term(128) * (4.0d+0) 
term(129) = term(129) * (-2.0d+0) 
term(130) = term(130) * (4.0d+0) 
term(131) = term(131) * (-2.0d+0) 
term(132) = term(132) * (-2.0d+0) 
term(133) = term(133) * (4.0d+0) 
term(134) = term(134) * (-2.0d+0) 
term(135) = term(135) * (-8.0d+0) 
term(136) = term(136) * (8.0d+0) 
term(137) = term(137) * (-8.0d+0) 
term(138) = term(138) * (8.0d+0) 
term(139) = term(139) * (16.0d+0) 
term(140) = term(140) * (-16.0d+0) 
term(141) = term(141) * (16.0d+0) 
term(142) = term(142) * (-16.0d+0) 
term(143) = term(143) * (-8.0d+0) 
term(144) = term(144) * (8.0d+0) 
term(145) = term(145) * (-8.0d+0) 
term(146) = term(146) * (8.0d+0) 
term(149) = term(149) * (-2.0d+0) 
term(150) = term(150) * (4.0d+0) 
term(151) = term(151) * (-4.0d+0) 
term(152) = term(152) * (2.0d+0) 
term(153) = term(153) * (-1.0d+0) 
term(154) = term(154) * (-1.0d+0) 
term(155) = term(155) * (4.0d+0) 
term(156) = term(156) * (-4.0d+0) 
term(157) = term(157) * (-2.0d+0) 
term(158) = term(158) * (-2.0d+0) 
term(159) = term(159) * (4.0d+0) 
term(160) = term(160) * (4.0d+0) 
term(161) = term(161) * (4.0d+0) 
term(162) = term(162) * (-8.0d+0) 
term(163) = term(163) * (-8.0d+0) 
term(164) = term(164) * (8.0d+0) 
term(165) = term(165) * (16.0d+0) 
term(166) = term(166) * (-16.0d+0) 
term(167) = term(167) * (-4.0d+0) 
term(168) = term(168) * (2.0d+0) 
term(169) = term(169) * (2.0d+0) 
term(170) = term(170) * (4.0d+0) 
term(171) = term(171) * (-8.0d+0) 
term(172) = term(172) * (-2.0d+0) 
term(173) = term(173) * (4.0d+0) 
term(174) = term(174) * (-2.0d+0) 
term(175) = term(175) * (4.0d+0) 
term(176) = term(176) * (-2.0d+0) 
term(179) = term(179) * (-2.0d+0) 
term(180) = term(180) * (4.0d+0) 
term(183) = term(183) * (-2.0d+0) 
term(184) = term(184) * (-2.0d+0) 
term(188) = term(188) * (-2.0d+0) 
term(190) = term(190) * (-2.0d+0) 
term(191) = term(191) * (-2.0d+0) 
term(192) = term(192) * (4.0d+0) 
term(193) = term(193) * (-2.0d+0) 
term(194) = term(194) * (4.0d+0) 
term(196) = term(196) * (-2.0d+0) 
term(198) = term(198) * (-2.0d+0) 
term(199) = term(199) * (-2.0d+0) 
term(202) = term(202) * (-2.0d+0) 
term(203) = term(203) * (-2.0d+0) 
term(204) = term(204) * (-2.0d+0) 
term(205) = term(205) * (4.0d+0) 
term(206) = term(206) * (4.0d+0) 
term(207) = term(207) * (-2.0d+0) 
term(210) = term(210) * (-2.0d+0) 
term(211) = term(211) * (-2.0d+0) 
term(215) = term(215) * (-2.0d+0) 
term(216) = term(216) * (-2.0d+0) 
term(218) = term(218) * (-2.0d+0) 
term(221) = term(221) * (-8.0d+0) 
term(222) = term(222) * (8.0d+0) 
term(223) = term(223) * (8.0d+0) 
term(224) = term(224) * (-16.0d+0) 
term(225) = term(225) * (-8.0d+0) 
term(226) = term(226) * (16.0d+0) 
term(227) = term(227) * (-8.0d+0) 
term(228) = term(228) * (4.0d+0) 
term(229) = term(229) * (4.0d+0) 
term(230) = term(230) * (-4.0d+0) 
term(231) = term(231) * (8.0d+0) 
term(232) = term(232) * (-4.0d+0) 
term(233) = term(233) * (4.0d+0) 
term(234) = term(234) * (4.0d+0) 
term(235) = term(235) * (4.0d+0) 
term(236) = term(236) * (-8.0d+0) 
term(237) = term(237) * (4.0d+0) 
term(238) = term(238) * (-8.0d+0) 
term(239) = term(239) * (-4.0d+0) 
term(240) = term(240) * (8.0d+0) 
term(241) = term(241) * (-4.0d+0) 
term(242) = term(242) * (8.0d+0) 
term(243) = term(243) * (-4.0d+0) 
term(244) = term(244) * (-4.0d+0) 
term(245) = term(245) * (-4.0d+0) 
term(246) = term(246) * (-4.0d+0) 
term(247) = term(247) * (8.0d+0) 
term(248) = term(248) * (8.0d+0) 
term(249) = term(249) * (-4.0d+0) 
term(250) = term(250) * (4.0d+0) 
term(251) = term(251) * (4.0d+0) 
term(252) = term(252) * (-8.0d+0) 
term(253) = term(253) * (-8.0d+0) 
term(254) = term(254) * (4.0d+0) 
term(255) = term(255) * (-4.0d+0) 
term(256) = term(256) * (4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
term(257) = term(257) + s1(q,i) * wm_interm_16_triplet_pt4(j,k) * wm_interm_26_triplet_pt4(p,k,i,j)
term(258) = term(258) + s1(q,i) * wm_interm_17_triplet_pt4(j,k) * wm_interm_26_triplet_pt4(p,k,i,j)
term(259) = term(259) + s1(q,i) * wm_interm_16_triplet_pt4(j,k) * wm_interm_29_triplet_pt4(p,k,i,j)
term(260) = term(260) + s1(q,i) * wm_interm_17_triplet_pt4(j,k) * wm_interm_29_triplet_pt4(p,k,i,j)
term(261) = term(261) + r1(vrdav_Rr, p,i) * wm_interm_16_triplet_pt4(j,k) * wm_interm_49_triplet_pt4(q,k,i,j)
term(262) = term(262) + r1(vrdav_Rr, p,i) * wm_interm_17_triplet_pt4(j,k) * wm_interm_49_triplet_pt4(q,k,i,j)
term(263) = term(263) + t1(q,i) * wm_interm_16_triplet_pt4(j,k) * wm_interm_51_triplet_pt4(p,k,i,j)
term(264) = term(264) + t1(q,i) * wm_interm_17_triplet_pt4(j,k) * wm_interm_51_triplet_pt4(p,k,i,j)
term(265) = term(265) + t1(q,i) * wm_interm_16_triplet_pt4(j,k) * wm_interm_47_triplet_pt4(p,k,i,j)
term(266) = term(266) + t1(q,i) * wm_interm_17_triplet_pt4(j,k) * wm_interm_47_triplet_pt4(p,k,i,j)
term(267) = term(267) + r1(vrdav_Rr, p,i) * wm_interm_162_triplet_pt4(j,k) * wm_interm_21_triplet_pt4(q,k,i,j)
term(268) = term(268) + r1(vrdav_Rr, p,i) * wm_interm_16_triplet_pt4(j,k) * wm_interm_61_triplet_pt4(q,k,i,j)
term(269) = term(269) + r1(vrdav_Rr, p,i) * wm_interm_17_triplet_pt4(j,k) * wm_interm_61_triplet_pt4(q,k,i,j)
term(270) = term(270) + r1(vrdav_Rr, p,i) * wm_interm_165_triplet_pt4(j,k) * wm_interm_21_triplet_pt4(q,k,i,j)
term(271) = term(271) + t1(q,i) * wm_interm_16_triplet_pt4(j,k) * wm_interm_62_triplet_pt4(p,k,i,j)
term(272) = term(272) + t1(q,i) * wm_interm_17_triplet_pt4(j,k) * wm_interm_62_triplet_pt4(p,k,i,j)
term(273) = term(273) + t1(q,i) * wm_interm_16_triplet_pt4(j,k) * wm_interm_59_triplet_pt4(p,k,i,j)
term(274) = term(274) + t1(q,i) * wm_interm_17_triplet_pt4(j,k) * wm_interm_59_triplet_pt4(p,k,i,j)
end do 
end do 
end do 

term(258) = term(258) * (-2.0d+0) 
term(259) = term(259) * (4.0d+0) 
term(260) = term(260) * (-8.0d+0) 
term(261) = term(261) * (2.0d+0) 
term(262) = term(262) * (-4.0d+0) 
term(264) = term(264) * (-2.0d+0) 
term(265) = term(265) * (-2.0d+0) 
term(266) = term(266) * (4.0d+0) 
term(267) = term(267) * (8.0d+0) 
term(268) = term(268) * (4.0d+0) 
term(269) = term(269) * (-8.0d+0) 
term(270) = term(270) * (-8.0d+0) 
term(271) = term(271) * (2.0d+0) 
term(272) = term(272) * (-4.0d+0) 
term(273) = term(273) * (-4.0d+0) 
term(274) = term(274) * (8.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(275) = term(275) + r2m(vrdav_Rr, a,j,p,i) * s1(b,j) * s2(a,q,l,k) * wm_interm_13_triplet_pt4(b,i,l,k)
term(276) = term(276) + s1(a,j) * s2(b,p,l,k) * t2(b,q,j,i) * wm_interm_26_triplet_pt4(a,i,l,k)
term(277) = term(277) + s1(a,j) * s2(b,p,l,k) * t2(b,q,j,i) * wm_interm_29_triplet_pt4(a,i,l,k)
term(278) = term(278) + r2p(vrdav_Rl, a,j,p,k) * t1(b,k) * t2(a,q,l,i) * wm_interm_12_triplet_pt4(b,j,l,i)
term(279) = term(279) + r2p(vrdav_Rl, a,j,p,k) * t1(b,j) * t2(a,q,l,i) * wm_interm_12_triplet_pt4(b,k,l,i)
term(280) = term(280) + r2m(vrdav_Rl, a,j,p,k) * t1(b,k) * t2(a,q,l,i) * wm_interm_12_triplet_pt4(b,j,l,i)
term(281) = term(281) + r2m(vrdav_Rl, a,j,p,k) * t1(b,j) * t2(a,q,l,i) * wm_interm_12_triplet_pt4(b,k,l,i)
end do 
end do 
end do 
end do 
end do 
end do 

term(275) = term(275) * (-4.0d+0) 
term(276) = term(276) * (-2.0d+0) 
term(277) = term(277) * (-8.0d+0) 
term(278) = term(278) * (-2.0d+0) 
term(280) = term(280) * (-4.0d+0) 
term(281) = term(281) * (2.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(282) = term(282) + r2m(vrdav_Rr, a,j,p,i) * s1(b,i) * s2(a,q,l,k) * wm_interm_13_triplet_pt4(b,j,k,l)
term(283) = term(283) + r2m(vrdav_Rr, a,j,p,i) * s1(b,j) * s2(a,q,l,k) * wm_interm_13_triplet_pt4(b,i,k,l)
term(284) = term(284) + s1(a,j) * s2(b,p,l,k) * t2(b,q,j,i) * wm_interm_24_triplet_pt4(a,i,k,l)
term(285) = term(285) + s1(a,j) * s2(b,p,l,k) * t2(b,q,j,i) * wm_interm_26_triplet_pt4(a,i,k,l)
term(286) = term(286) + s1(a,j) * s2(b,p,l,k) * t2(b,q,j,i) * wm_interm_29_triplet_pt4(a,i,k,l)
term(287) = term(287) + r2p(vrdav_Rl, a,j,p,k) * t1(b,k) * t2(a,q,l,i) * wm_interm_12_triplet_pt4(b,j,i,l)
term(288) = term(288) + r2m(vrdav_Rl, a,j,p,k) * t1(b,k) * t2(a,q,l,i) * wm_interm_12_triplet_pt4(b,j,i,l)
end do 
end do 
end do 
end do 
end do 
end do 

term(282) = term(282) * (-4.0d+0) 
term(283) = term(283) * (8.0d+0) 
term(284) = term(284) * (-2.0d+0) 
term(285) = term(285) * (4.0d+0) 
term(286) = term(286) * (8.0d+0) 
term(288) = term(288) * (2.0d+0) 

do a = nocc + 1, nactive 
term(289) = term(289) + wm_interm_113_triplet_pt4(a,p) * wm_interm_45_triplet_pt4(q,a)
term(290) = term(290) + wm_interm_111_triplet_pt4(a,p) * wm_interm_45_triplet_pt4(q,a)
term(291) = term(291) + wm_interm_112_triplet_pt4(a,p) * wm_interm_45_triplet_pt4(q,a)
term(292) = term(292) + wm_interm_121_triplet_pt4(a,p) * wm_interm_45_triplet_pt4(q,a)
term(293) = term(293) + wm_interm_122_triplet_pt4(a,p) * wm_interm_45_triplet_pt4(q,a)
term(294) = term(294) + wm_interm_120_triplet_pt4(a,p) * wm_interm_45_triplet_pt4(q,a)
term(295) = term(295) + wm_interm_142_triplet_pt4(a,p) * wm_interm_45_triplet_pt4(q,a)
term(296) = term(296) + wm_interm_143_triplet_pt4(a,p) * wm_interm_45_triplet_pt4(q,a)
term(297) = term(297) + wm_interm_148_triplet_pt4(a,p) * wm_interm_45_triplet_pt4(q,a)
term(298) = term(298) + wm_interm_149_triplet_pt4(a,p) * wm_interm_45_triplet_pt4(q,a)
term(299) = term(299) + wm_interm_121_triplet_pt4(p,a) * wm_interm_45_triplet_pt4(a,q)
term(300) = term(300) + wm_interm_122_triplet_pt4(p,a) * wm_interm_45_triplet_pt4(a,q)
term(301) = term(301) + wm_interm_113_triplet_pt4(p,a) * wm_interm_45_triplet_pt4(a,q)
term(302) = term(302) + wm_interm_111_triplet_pt4(p,a) * wm_interm_45_triplet_pt4(a,q)
term(303) = term(303) + wm_interm_112_triplet_pt4(p,a) * wm_interm_45_triplet_pt4(a,q)
term(304) = term(304) + wm_interm_120_triplet_pt4(p,a) * wm_interm_45_triplet_pt4(a,q)
term(305) = term(305) + wm_interm_148_triplet_pt4(p,a) * wm_interm_45_triplet_pt4(a,q)
term(306) = term(306) + wm_interm_149_triplet_pt4(p,a) * wm_interm_45_triplet_pt4(a,q)
term(307) = term(307) + wm_interm_142_triplet_pt4(p,a) * wm_interm_45_triplet_pt4(a,q)
term(308) = term(308) + wm_interm_143_triplet_pt4(p,a) * wm_interm_45_triplet_pt4(a,q)
term(309) = term(309) + wm_interm_180_triplet_pt4(a,p) * wm_interm_45_triplet_pt4(q,a)
term(310) = term(310) + wm_interm_178_triplet_pt4(a,p) * wm_interm_45_triplet_pt4(q,a)
term(311) = term(311) + wm_interm_179_triplet_pt4(a,p) * wm_interm_45_triplet_pt4(q,a)
term(312) = term(312) + wm_interm_186_triplet_pt4(a,p) * wm_interm_45_triplet_pt4(q,a)
term(313) = term(313) + wm_interm_187_triplet_pt4(a,p) * wm_interm_45_triplet_pt4(q,a)
term(314) = term(314) + wm_interm_185_triplet_pt4(a,p) * wm_interm_45_triplet_pt4(q,a)
term(315) = term(315) + wm_interm_195_triplet_pt4(a,p) * wm_interm_45_triplet_pt4(q,a)
term(316) = term(316) + wm_interm_196_triplet_pt4(a,p) * wm_interm_45_triplet_pt4(q,a)
term(317) = term(317) + wm_interm_199_triplet_pt4(a,p) * wm_interm_45_triplet_pt4(q,a)
term(318) = term(318) + wm_interm_200_triplet_pt4(a,p) * wm_interm_45_triplet_pt4(q,a)
term(319) = term(319) + wm_interm_186_triplet_pt4(p,a) * wm_interm_45_triplet_pt4(a,q)
term(320) = term(320) + wm_interm_187_triplet_pt4(p,a) * wm_interm_45_triplet_pt4(a,q)
term(321) = term(321) + wm_interm_180_triplet_pt4(p,a) * wm_interm_45_triplet_pt4(a,q)
term(322) = term(322) + wm_interm_178_triplet_pt4(p,a) * wm_interm_45_triplet_pt4(a,q)
term(323) = term(323) + wm_interm_179_triplet_pt4(p,a) * wm_interm_45_triplet_pt4(a,q)
term(324) = term(324) + wm_interm_185_triplet_pt4(p,a) * wm_interm_45_triplet_pt4(a,q)
term(325) = term(325) + wm_interm_199_triplet_pt4(p,a) * wm_interm_45_triplet_pt4(a,q)
term(326) = term(326) + wm_interm_200_triplet_pt4(p,a) * wm_interm_45_triplet_pt4(a,q)
term(327) = term(327) + wm_interm_195_triplet_pt4(p,a) * wm_interm_45_triplet_pt4(a,q)
term(328) = term(328) + wm_interm_196_triplet_pt4(p,a) * wm_interm_45_triplet_pt4(a,q)
term(329) = term(329) + wm_interm_0_triplet_pt4(p,a) * wm_interm_22_triplet_pt4(q,a)
term(330) = term(330) + wm_interm_14_triplet_pt4(a,p) * wm_interm_45_triplet_pt4(q,a)
term(331) = term(331) + wm_interm_0_triplet_pt4(a,p) * wm_interm_22_triplet_pt4(a,q)
term(332) = term(332) + wm_interm_14_triplet_pt4(p,a) * wm_interm_45_triplet_pt4(a,q)
end do 

term(289) = term(289) * (0.5d+0) 
term(290) = term(290) * (0.5d+0) 
term(291) = term(291) * (-1.0d+0) 
term(292) = term(292) * (0.5d+0) 
term(293) = term(293) * (-1.0d+0) 
term(294) = term(294) * (0.5d+0) 
term(295) = term(295) * (2.0d+0) 
term(296) = term(296) * (-2.0d+0) 
term(297) = term(297) * (2.0d+0) 
term(298) = term(298) * (-2.0d+0) 
term(299) = term(299) * (0.5d+0) 
term(300) = term(300) * (-1.0d+0) 
term(301) = term(301) * (0.5d+0) 
term(302) = term(302) * (0.5d+0) 
term(303) = term(303) * (-1.0d+0) 
term(304) = term(304) * (0.5d+0) 
term(305) = term(305) * (2.0d+0) 
term(306) = term(306) * (-2.0d+0) 
term(307) = term(307) * (2.0d+0) 
term(308) = term(308) * (-2.0d+0) 
term(311) = term(311) * (-2.0d+0) 
term(313) = term(313) * (-2.0d+0) 
term(315) = term(315) * (4.0d+0) 
term(316) = term(316) * (-4.0d+0) 
term(317) = term(317) * (4.0d+0) 
term(318) = term(318) * (-4.0d+0) 
term(320) = term(320) * (-2.0d+0) 
term(323) = term(323) * (-2.0d+0) 
term(325) = term(325) * (4.0d+0) 
term(326) = term(326) * (-4.0d+0) 
term(327) = term(327) * (4.0d+0) 
term(328) = term(328) * (-4.0d+0) 
term(329) = term(329) * (-2.0d+0) 
term(330) = term(330) * (-2.0d+0) 
term(331) = term(331) * (-2.0d+0) 
term(332) = term(332) * (-2.0d+0) 

do l = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
term(333) = term(333) + r1(vrdav_Rr, p,i) * wm_interm_21_triplet_pt4(q,j,k,l) * wm_interm_73_triplet_pt4(i,l,k,j)
term(334) = term(334) + r1(vrdav_Rr, p,i) * wm_interm_163_triplet_pt4(i,j,k,l) * wm_interm_21_triplet_pt4(q,l,k,j)
term(335) = term(335) + r1(vrdav_Rr, p,i) * wm_interm_163_triplet_pt4(i,j,k,l) * wm_interm_21_triplet_pt4(q,k,l,j)
term(336) = term(336) + r1(vrdav_Rr, p,i) * wm_interm_163_triplet_pt4(j,i,k,l) * wm_interm_21_triplet_pt4(q,k,l,j)
end do 
end do 
end do 
end do 

term(333) = term(333) * (2.0d+0) 
term(334) = term(334) * (4.0d+0) 
term(335) = term(335) * (-8.0d+0) 
term(336) = term(336) * (4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(337) = term(337) + s1(a,i) * s2(b,p,k,j) * t2(b,q,l,i) * wm_interm_24_triplet_pt4(a,l,k,j)
term(338) = term(338) + s1(a,i) * s2(b,p,k,j) * t2(b,q,l,i) * wm_interm_26_triplet_pt4(a,l,k,j)
term(339) = term(339) + s1(a,i) * s2(b,p,k,j) * t2(b,q,l,i) * wm_interm_29_triplet_pt4(a,l,k,j)
end do 
end do 
end do 
end do 
end do 
end do 

term(337) = term(337) * (-2.0d+0) 
term(338) = term(338) * (4.0d+0) 
term(339) = term(339) * (8.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(340) = term(340) + s1(a,i) * s2(b,p,k,j) * t2(b,q,l,i) * wm_interm_26_triplet_pt4(a,l,j,k)
term(341) = term(341) + s1(a,i) * s2(b,p,k,j) * t2(b,q,l,i) * wm_interm_29_triplet_pt4(a,l,j,k)
term(342) = term(342) + s2(a,p,k,j) * t1(b,j) * t2(a,q,l,i) * wm_interm_47_triplet_pt4(b,l,i,k)
term(343) = term(343) + s2(a,p,k,j) * t1(b,j) * t2(a,q,l,i) * wm_interm_47_triplet_pt4(b,i,l,k)
term(344) = term(344) + s2(a,p,k,j) * t1(b,j) * t2(a,q,l,i) * wm_interm_51_triplet_pt4(b,l,i,k)
term(345) = term(345) + s2(a,p,k,j) * t1(b,j) * t2(a,q,l,i) * wm_interm_59_triplet_pt4(b,l,i,k)
term(346) = term(346) + s2(a,p,k,j) * t1(b,j) * t2(a,q,l,i) * wm_interm_59_triplet_pt4(b,i,l,k)
term(347) = term(347) + s2(a,p,k,j) * t1(b,j) * t2(a,q,l,i) * wm_interm_62_triplet_pt4(b,l,i,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(340) = term(340) * (-2.0d+0) 
term(341) = term(341) * (-8.0d+0) 
term(342) = term(342) * (-2.0d+0) 
term(345) = term(345) * (-4.0d+0) 
term(346) = term(346) * (2.0d+0) 
term(347) = term(347) * (2.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(348) = term(348) + r1(vrdav_Rl, a,i) * wm_interm_1_triplet_pt4(q,a,j,k) * wm_interm_27_triplet_pt4(p,i,j,k)
term(349) = term(349) + r1(vrdav_Rl, a,i) * wm_interm_27_triplet_pt4(p,i,j,k) * wm_interm_5_triplet_pt4(q,a,j,k)
term(350) = term(350) + r1(vrdav_Rl, a,i) * wm_interm_1_triplet_pt4(q,a,j,k) * wm_interm_28_triplet_pt4(p,i,j,k)
term(351) = term(351) + r1(vrdav_Rl, a,i) * wm_interm_28_triplet_pt4(p,i,j,k) * wm_interm_5_triplet_pt4(q,a,j,k)
term(352) = term(352) + r1(vrdav_Rl, a,i) * wm_interm_10_triplet_pt4(q,a,j,k) * wm_interm_27_triplet_pt4(p,i,j,k)
term(353) = term(353) + r1(vrdav_Rl, a,i) * wm_interm_11_triplet_pt4(q,a,j,k) * wm_interm_27_triplet_pt4(p,i,j,k)
term(354) = term(354) + r1(vrdav_Rl, a,i) * wm_interm_10_triplet_pt4(q,a,j,k) * wm_interm_28_triplet_pt4(p,i,j,k)
term(355) = term(355) + r1(vrdav_Rl, a,i) * wm_interm_11_triplet_pt4(q,a,j,k) * wm_interm_28_triplet_pt4(p,i,j,k)
term(356) = term(356) + r1(vrdav_Rl, a,i) * wm_interm_1_triplet_pt4(q,a,j,k) * wm_interm_30_triplet_pt4(p,i,j,k)
term(357) = term(357) + r1(vrdav_Rl, a,i) * wm_interm_30_triplet_pt4(p,i,j,k) * wm_interm_5_triplet_pt4(q,a,j,k)
term(358) = term(358) + r1(vrdav_Rl, a,i) * wm_interm_1_triplet_pt4(q,a,j,k) * wm_interm_31_triplet_pt4(p,i,j,k)
term(359) = term(359) + r1(vrdav_Rl, a,i) * wm_interm_31_triplet_pt4(p,i,j,k) * wm_interm_5_triplet_pt4(q,a,j,k)
term(360) = term(360) + r1(vrdav_Rl, a,i) * wm_interm_10_triplet_pt4(q,a,j,k) * wm_interm_30_triplet_pt4(p,i,j,k)
term(361) = term(361) + r1(vrdav_Rl, a,i) * wm_interm_11_triplet_pt4(q,a,j,k) * wm_interm_30_triplet_pt4(p,i,j,k)
term(362) = term(362) + r1(vrdav_Rl, a,i) * wm_interm_10_triplet_pt4(q,a,j,k) * wm_interm_31_triplet_pt4(p,i,j,k)
term(363) = term(363) + r1(vrdav_Rl, a,i) * wm_interm_11_triplet_pt4(q,a,j,k) * wm_interm_31_triplet_pt4(p,i,j,k)
term(364) = term(364) + r1(vrdav_Rr, p,i) * wm_interm_151_triplet_pt4(a,i,j,k) * wm_interm_67_triplet_pt4(q,a,k,j)
term(365) = term(365) + r1(vrdav_Rr, p,i) * wm_interm_21_triplet_pt4(a,i,j,k) * wm_interm_74_triplet_pt4(q,a,k,j)
term(366) = term(366) + r1(vrdav_Rr, p,i) * wm_interm_21_triplet_pt4(a,i,j,k) * wm_interm_78_triplet_pt4(q,a,k,j)
term(367) = term(367) + r1(vrdav_Rr, p,i) * wm_interm_10_triplet_pt4(q,a,j,k) * wm_interm_49_triplet_pt4(a,i,k,j)
term(368) = term(368) + r1(vrdav_Rr, p,i) * wm_interm_11_triplet_pt4(q,a,j,k) * wm_interm_49_triplet_pt4(a,i,k,j)
term(369) = term(369) + r1(vrdav_Rr, p,i) * wm_interm_1_triplet_pt4(q,a,j,k) * wm_interm_49_triplet_pt4(a,i,k,j)
term(370) = term(370) + r1(vrdav_Rr, p,i) * wm_interm_49_triplet_pt4(a,i,j,k) * wm_interm_5_triplet_pt4(q,a,k,j)
term(371) = term(371) + r1(vrdav_Rr, p,i) * wm_interm_151_triplet_pt4(a,i,j,k) * wm_interm_66_triplet_pt4(q,a,k,j)
term(372) = term(372) + r1(vrdav_Rr, p,i) * wm_interm_151_triplet_pt4(a,i,j,k) * wm_interm_68_triplet_pt4(q,a,k,j)
term(373) = term(373) + r1(vrdav_Rr, p,i) * wm_interm_151_triplet_pt4(a,i,j,k) * wm_interm_69_triplet_pt4(q,a,k,j)
term(374) = term(374) + r1(vrdav_Rr, p,i) * wm_interm_151_triplet_pt4(a,i,j,k) * wm_interm_71_triplet_pt4(q,a,k,j)
term(375) = term(375) + r1(vrdav_Rr, p,i) * wm_interm_10_triplet_pt4(q,a,j,k) * wm_interm_52_triplet_pt4(a,i,k,j)
term(376) = term(376) + r1(vrdav_Rr, p,i) * wm_interm_11_triplet_pt4(q,a,j,k) * wm_interm_52_triplet_pt4(a,i,k,j)
term(377) = term(377) + r1(vrdav_Rr, p,i) * wm_interm_52_triplet_pt4(a,i,j,k) * wm_interm_5_triplet_pt4(q,a,k,j)
term(378) = term(378) + r1(vrdav_Rr, p,i) * wm_interm_1_triplet_pt4(q,a,j,k) * wm_interm_52_triplet_pt4(a,i,k,j)
term(379) = term(379) + t1(q,i) * wm_interm_10_triplet_pt4(p,a,j,k) * wm_interm_47_triplet_pt4(a,i,k,j)
term(380) = term(380) + t1(q,i) * wm_interm_11_triplet_pt4(p,a,j,k) * wm_interm_47_triplet_pt4(a,i,k,j)
term(381) = term(381) + t1(q,i) * wm_interm_10_triplet_pt4(p,a,j,k) * wm_interm_51_triplet_pt4(a,i,k,j)
term(382) = term(382) + t1(q,i) * wm_interm_11_triplet_pt4(p,a,j,k) * wm_interm_51_triplet_pt4(a,i,k,j)
term(383) = term(383) + t1(q,i) * wm_interm_1_triplet_pt4(p,a,j,k) * wm_interm_51_triplet_pt4(a,i,k,j)
term(384) = term(384) + t1(q,i) * wm_interm_47_triplet_pt4(a,i,j,k) * wm_interm_5_triplet_pt4(p,a,k,j)
term(385) = term(385) + t1(q,i) * wm_interm_1_triplet_pt4(p,a,j,k) * wm_interm_47_triplet_pt4(a,i,k,j)
term(386) = term(386) + r1(vrdav_Rr, p,i) * wm_interm_151_triplet_pt4(a,i,j,k) * wm_interm_158_triplet_pt4(q,a,k,j)
term(387) = term(387) + r1(vrdav_Rr, p,i) * wm_interm_159_triplet_pt4(q,a,j,k) * wm_interm_21_triplet_pt4(a,i,k,j)
term(388) = term(388) + r1(vrdav_Rr, p,i) * wm_interm_161_triplet_pt4(q,a,j,k) * wm_interm_21_triplet_pt4(a,i,k,j)
term(389) = term(389) + r1(vrdav_Rr, p,i) * wm_interm_10_triplet_pt4(q,a,j,k) * wm_interm_61_triplet_pt4(a,i,k,j)
term(390) = term(390) + r1(vrdav_Rr, p,i) * wm_interm_11_triplet_pt4(q,a,j,k) * wm_interm_61_triplet_pt4(a,i,k,j)
term(391) = term(391) + r1(vrdav_Rr, p,i) * wm_interm_1_triplet_pt4(q,a,j,k) * wm_interm_61_triplet_pt4(a,i,k,j)
term(392) = term(392) + r1(vrdav_Rr, p,i) * wm_interm_5_triplet_pt4(q,a,j,k) * wm_interm_61_triplet_pt4(a,i,k,j)
term(393) = term(393) + r1(vrdav_Rr, p,i) * wm_interm_151_triplet_pt4(a,i,j,k) * wm_interm_157_triplet_pt4(q,a,k,j)
term(394) = term(394) + r1(vrdav_Rr, p,i) * wm_interm_151_triplet_pt4(a,i,j,k) * wm_interm_159_triplet_pt4(q,a,k,j)
term(395) = term(395) + r1(vrdav_Rr, p,i) * wm_interm_151_triplet_pt4(a,i,j,k) * wm_interm_161_triplet_pt4(q,a,k,j)
term(396) = term(396) + r1(vrdav_Rr, p,i) * wm_interm_10_triplet_pt4(q,a,j,k) * wm_interm_63_triplet_pt4(a,i,k,j)
term(397) = term(397) + r1(vrdav_Rr, p,i) * wm_interm_11_triplet_pt4(q,a,j,k) * wm_interm_63_triplet_pt4(a,i,k,j)
term(398) = term(398) + r1(vrdav_Rr, p,i) * wm_interm_5_triplet_pt4(q,a,j,k) * wm_interm_63_triplet_pt4(a,i,k,j)
term(399) = term(399) + r1(vrdav_Rr, p,i) * wm_interm_1_triplet_pt4(q,a,j,k) * wm_interm_63_triplet_pt4(a,i,k,j)
term(400) = term(400) + t1(q,i) * wm_interm_10_triplet_pt4(p,a,j,k) * wm_interm_59_triplet_pt4(a,i,k,j)
term(401) = term(401) + t1(q,i) * wm_interm_11_triplet_pt4(p,a,j,k) * wm_interm_59_triplet_pt4(a,i,k,j)
term(402) = term(402) + t1(q,i) * wm_interm_10_triplet_pt4(p,a,j,k) * wm_interm_62_triplet_pt4(a,i,k,j)
term(403) = term(403) + t1(q,i) * wm_interm_11_triplet_pt4(p,a,j,k) * wm_interm_62_triplet_pt4(a,i,k,j)
term(404) = term(404) + t1(q,i) * wm_interm_1_triplet_pt4(p,a,j,k) * wm_interm_62_triplet_pt4(a,i,k,j)
term(405) = term(405) + t1(q,i) * wm_interm_59_triplet_pt4(a,i,j,k) * wm_interm_5_triplet_pt4(p,a,k,j)
term(406) = term(406) + t1(q,i) * wm_interm_1_triplet_pt4(p,a,j,k) * wm_interm_59_triplet_pt4(a,i,k,j)
end do 
end do 
end do 
end do 

term(348) = term(348) * (-2.0d+0) 
term(349) = term(349) * (4.0d+0) 
term(350) = term(350) * (4.0d+0) 
term(351) = term(351) * (-2.0d+0) 
term(352) = term(352) * (-2.0d+0) 
term(353) = term(353) * (4.0d+0) 
term(354) = term(354) * (4.0d+0) 
term(355) = term(355) * (-8.0d+0) 
term(356) = term(356) * (-4.0d+0) 
term(357) = term(357) * (8.0d+0) 
term(358) = term(358) * (8.0d+0) 
term(359) = term(359) * (-4.0d+0) 
term(360) = term(360) * (-4.0d+0) 
term(361) = term(361) * (8.0d+0) 
term(362) = term(362) * (8.0d+0) 
term(363) = term(363) * (-16.0d+0) 
term(364) = term(364) * (-4.0d+0) 
term(365) = term(365) * (-4.0d+0) 
term(366) = term(366) * (2.0d+0) 
term(367) = term(367) * (2.0d+0) 
term(368) = term(368) * (-4.0d+0) 
term(369) = term(369) * (2.0d+0) 
term(370) = term(370) * (-4.0d+0) 
term(371) = term(371) * (8.0d+0) 
term(372) = term(372) * (-4.0d+0) 
term(373) = term(373) * (2.0d+0) 
term(374) = term(374) * (-4.0d+0) 
term(375) = term(375) * (-4.0d+0) 
term(376) = term(376) * (8.0d+0) 
term(377) = term(377) * (2.0d+0) 
term(378) = term(378) * (-4.0d+0) 
term(379) = term(379) * (-2.0d+0) 
term(380) = term(380) * (4.0d+0) 
term(382) = term(382) * (-2.0d+0) 
term(385) = term(385) * (-2.0d+0) 
term(386) = term(386) * (-16.0d+0) 
term(387) = term(387) * (-8.0d+0) 
term(388) = term(388) * (4.0d+0) 
term(389) = term(389) * (4.0d+0) 
term(390) = term(390) * (-8.0d+0) 
term(391) = term(391) * (4.0d+0) 
term(392) = term(392) * (-8.0d+0) 
term(393) = term(393) * (16.0d+0) 
term(394) = term(394) * (4.0d+0) 
term(395) = term(395) * (-8.0d+0) 
term(396) = term(396) * (-8.0d+0) 
term(397) = term(397) * (16.0d+0) 
term(398) = term(398) * (4.0d+0) 
term(399) = term(399) * (-8.0d+0) 
term(400) = term(400) * (-4.0d+0) 
term(401) = term(401) * (8.0d+0) 
term(402) = term(402) * (2.0d+0) 
term(403) = term(403) * (-4.0d+0) 
term(404) = term(404) * (2.0d+0) 
term(405) = term(405) * (2.0d+0) 
term(406) = term(406) * (-4.0d+0) 

do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
term(407) = term(407) + r1(vrdav_Rr, p,i) * wm_interm_16_triplet_pt4(j,k) * wm_interm_49_triplet_pt4(q,i,k,j)
term(408) = term(408) + r1(vrdav_Rr, p,i) * wm_interm_17_triplet_pt4(j,k) * wm_interm_49_triplet_pt4(q,i,k,j)
term(409) = term(409) + r1(vrdav_Rr, p,i) * wm_interm_16_triplet_pt4(j,k) * wm_interm_52_triplet_pt4(q,i,k,j)
term(410) = term(410) + r1(vrdav_Rr, p,i) * wm_interm_17_triplet_pt4(j,k) * wm_interm_52_triplet_pt4(q,i,k,j)
term(411) = term(411) + t1(q,i) * wm_interm_16_triplet_pt4(j,k) * wm_interm_47_triplet_pt4(p,i,k,j)
term(412) = term(412) + t1(q,i) * wm_interm_17_triplet_pt4(j,k) * wm_interm_47_triplet_pt4(p,i,k,j)
term(413) = term(413) + r1(vrdav_Rr, p,i) * wm_interm_16_triplet_pt4(j,k) * wm_interm_61_triplet_pt4(q,i,k,j)
term(414) = term(414) + r1(vrdav_Rr, p,i) * wm_interm_17_triplet_pt4(j,k) * wm_interm_61_triplet_pt4(q,i,k,j)
term(415) = term(415) + r1(vrdav_Rr, p,i) * wm_interm_16_triplet_pt4(j,k) * wm_interm_63_triplet_pt4(q,i,k,j)
term(416) = term(416) + r1(vrdav_Rr, p,i) * wm_interm_17_triplet_pt4(j,k) * wm_interm_63_triplet_pt4(q,i,k,j)
term(417) = term(417) + t1(q,i) * wm_interm_16_triplet_pt4(j,k) * wm_interm_59_triplet_pt4(p,i,k,j)
term(418) = term(418) + t1(q,i) * wm_interm_17_triplet_pt4(j,k) * wm_interm_59_triplet_pt4(p,i,k,j)
end do 
end do 
end do 

term(407) = term(407) * (-4.0d+0) 
term(408) = term(408) * (8.0d+0) 
term(409) = term(409) * (2.0d+0) 
term(410) = term(410) * (-4.0d+0) 
term(412) = term(412) * (-2.0d+0) 
term(413) = term(413) * (-8.0d+0) 
term(414) = term(414) * (16.0d+0) 
term(415) = term(415) * (4.0d+0) 
term(416) = term(416) * (-8.0d+0) 
term(417) = term(417) * (2.0d+0) 
term(418) = term(418) * (-4.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(419) = term(419) + s1(a,i) * wm_interm_18_triplet_pt4(p,q) * wm_interm_34_triplet_pt4(a,i)
term(420) = term(420) + r1(vrdav_Rl, p,i) * wm_interm_18_triplet_pt4(a,q) * wm_interm_33_triplet_pt4(a,i)
term(421) = term(421) + s1(a,i) * wm_interm_19_triplet_pt4(p,q) * wm_interm_34_triplet_pt4(a,i)
term(422) = term(422) + r1(vrdav_Rl, p,i) * wm_interm_19_triplet_pt4(a,q) * wm_interm_33_triplet_pt4(a,i)
term(423) = term(423) + s1(a,i) * wm_interm_18_triplet_pt4(p,q) * wm_interm_37_triplet_pt4(a,i)
term(424) = term(424) + r1(vrdav_Rl, p,i) * wm_interm_18_triplet_pt4(a,q) * wm_interm_36_triplet_pt4(a,i)
term(425) = term(425) + s1(a,i) * wm_interm_19_triplet_pt4(p,q) * wm_interm_37_triplet_pt4(a,i)
term(426) = term(426) + r1(vrdav_Rl, p,i) * wm_interm_19_triplet_pt4(a,q) * wm_interm_36_triplet_pt4(a,i)
term(427) = term(427) + s1(a,i) * wm_interm_18_triplet_pt4(p,q) * wm_interm_39_triplet_pt4(a,i)
term(428) = term(428) + r1(vrdav_Rl, p,i) * wm_interm_18_triplet_pt4(a,q) * wm_interm_38_triplet_pt4(a,i)
term(429) = term(429) + s1(a,i) * wm_interm_19_triplet_pt4(p,q) * wm_interm_39_triplet_pt4(a,i)
term(430) = term(430) + r1(vrdav_Rl, p,i) * wm_interm_19_triplet_pt4(a,q) * wm_interm_38_triplet_pt4(a,i)
term(431) = term(431) + s1(a,i) * wm_interm_18_triplet_pt4(p,q) * wm_interm_41_triplet_pt4(a,i)
term(432) = term(432) + r1(vrdav_Rl, p,i) * wm_interm_18_triplet_pt4(a,q) * wm_interm_40_triplet_pt4(a,i)
term(433) = term(433) + s1(a,i) * wm_interm_19_triplet_pt4(p,q) * wm_interm_41_triplet_pt4(a,i)
term(434) = term(434) + r1(vrdav_Rl, p,i) * wm_interm_19_triplet_pt4(a,q) * wm_interm_40_triplet_pt4(a,i)
term(435) = term(435) + s1(a,i) * wm_interm_18_triplet_pt4(p,q) * wm_interm_43_triplet_pt4(a,i)
term(436) = term(436) + r1(vrdav_Rl, p,i) * wm_interm_18_triplet_pt4(a,q) * wm_interm_42_triplet_pt4(a,i)
term(437) = term(437) + s1(a,i) * wm_interm_19_triplet_pt4(p,q) * wm_interm_43_triplet_pt4(a,i)
term(438) = term(438) + r1(vrdav_Rl, p,i) * wm_interm_19_triplet_pt4(a,q) * wm_interm_42_triplet_pt4(a,i)
term(439) = term(439) + r1(vrdav_Rl, a,i) * wm_interm_100_triplet_pt4(a,p) * wm_interm_7_triplet_pt4(q,i)
term(440) = term(440) + r1(vrdav_Rl, a,i) * wm_interm_100_triplet_pt4(a,p) * wm_interm_6_triplet_pt4(q,i)
term(441) = term(441) + r1(vrdav_Rl, a,i) * wm_interm_7_triplet_pt4(q,i) * wm_interm_98_triplet_pt4(a,p)
term(442) = term(442) + r1(vrdav_Rl, a,i) * wm_interm_6_triplet_pt4(q,i) * wm_interm_98_triplet_pt4(a,p)
term(443) = term(443) + r1(vrdav_Rl, a,i) * wm_interm_7_triplet_pt4(q,i) * wm_interm_99_triplet_pt4(a,p)
term(444) = term(444) + r1(vrdav_Rl, a,i) * wm_interm_6_triplet_pt4(q,i) * wm_interm_99_triplet_pt4(a,p)
term(445) = term(445) + s1(a,i) * wm_interm_15_triplet_pt4(q,i) * wm_interm_98_triplet_pt4(a,p)
term(446) = term(446) + s1(a,i) * wm_interm_15_triplet_pt4(q,i) * wm_interm_99_triplet_pt4(a,p)
term(447) = term(447) + s1(a,i) * wm_interm_100_triplet_pt4(a,p) * wm_interm_15_triplet_pt4(q,i)
term(448) = term(448) + s1(p,i) * wm_interm_18_triplet_pt4(a,q) * wm_interm_39_triplet_pt4(a,i)
term(449) = term(449) + s1(p,i) * wm_interm_18_triplet_pt4(a,q) * wm_interm_37_triplet_pt4(a,i)
term(450) = term(450) + s1(p,i) * wm_interm_18_triplet_pt4(a,q) * wm_interm_34_triplet_pt4(a,i)
term(451) = term(451) + s1(p,i) * wm_interm_19_triplet_pt4(a,q) * wm_interm_39_triplet_pt4(a,i)
term(452) = term(452) + s1(p,i) * wm_interm_19_triplet_pt4(a,q) * wm_interm_37_triplet_pt4(a,i)
term(453) = term(453) + s1(p,i) * wm_interm_19_triplet_pt4(a,q) * wm_interm_34_triplet_pt4(a,i)
term(454) = term(454) + r1(vrdav_Rl, a,i) * wm_interm_134_triplet_pt4(a,p) * wm_interm_7_triplet_pt4(q,i)
term(455) = term(455) + r1(vrdav_Rl, a,i) * wm_interm_134_triplet_pt4(a,p) * wm_interm_6_triplet_pt4(q,i)
term(456) = term(456) + r1(vrdav_Rl, a,i) * wm_interm_135_triplet_pt4(a,p) * wm_interm_7_triplet_pt4(q,i)
term(457) = term(457) + r1(vrdav_Rl, a,i) * wm_interm_135_triplet_pt4(a,p) * wm_interm_6_triplet_pt4(q,i)
term(458) = term(458) + s1(a,i) * wm_interm_134_triplet_pt4(a,p) * wm_interm_15_triplet_pt4(q,i)
term(459) = term(459) + s1(a,i) * wm_interm_135_triplet_pt4(a,p) * wm_interm_15_triplet_pt4(q,i)
term(460) = term(460) + s1(a,i) * wm_interm_150_triplet_pt4(a,p) * wm_interm_15_triplet_pt4(q,i)
term(461) = term(461) + s1(p,i) * wm_interm_18_triplet_pt4(a,q) * wm_interm_43_triplet_pt4(a,i)
term(462) = term(462) + s1(p,i) * wm_interm_18_triplet_pt4(a,q) * wm_interm_41_triplet_pt4(a,i)
term(463) = term(463) + s1(p,i) * wm_interm_19_triplet_pt4(a,q) * wm_interm_43_triplet_pt4(a,i)
term(464) = term(464) + s1(p,i) * wm_interm_19_triplet_pt4(a,q) * wm_interm_41_triplet_pt4(a,i)
term(465) = term(465) + t1(a,i) * wm_interm_18_triplet_pt4(a,q) * wm_interm_46_triplet_pt4(p,i)
term(466) = term(466) + t1(a,i) * wm_interm_19_triplet_pt4(a,q) * wm_interm_46_triplet_pt4(p,i)
term(467) = term(467) + t1(a,i) * wm_interm_18_triplet_pt4(a,q) * wm_interm_48_triplet_pt4(p,i)
term(468) = term(468) + t1(a,i) * wm_interm_19_triplet_pt4(a,q) * wm_interm_48_triplet_pt4(p,i)
term(469) = term(469) + t1(a,i) * wm_interm_2_triplet_pt4(p,i) * wm_interm_77_triplet_pt4(a,q)
term(470) = term(470) + t1(a,i) * wm_interm_18_triplet_pt4(a,q) * wm_interm_50_triplet_pt4(p,i)
term(471) = term(471) + t1(a,i) * wm_interm_19_triplet_pt4(a,q) * wm_interm_50_triplet_pt4(p,i)
term(472) = term(472) + t1(a,i) * wm_interm_2_triplet_pt4(p,i) * wm_interm_70_triplet_pt4(a,q)
term(473) = term(473) + t1(a,i) * wm_interm_2_triplet_pt4(p,i) * wm_interm_75_triplet_pt4(a,q)
term(474) = term(474) + t1(a,i) * wm_interm_18_triplet_pt4(p,q) * wm_interm_48_triplet_pt4(a,i)
term(475) = term(475) + r1(vrdav_Rr, a,i) * wm_interm_18_triplet_pt4(a,q) * wm_interm_53_triplet_pt4(p,i)
term(476) = term(476) + t1(a,i) * wm_interm_19_triplet_pt4(p,q) * wm_interm_48_triplet_pt4(a,i)
term(477) = term(477) + r1(vrdav_Rr, a,i) * wm_interm_19_triplet_pt4(a,q) * wm_interm_53_triplet_pt4(p,i)
term(478) = term(478) + t1(a,i) * wm_interm_18_triplet_pt4(p,q) * wm_interm_50_triplet_pt4(a,i)
term(479) = term(479) + r1(vrdav_Rr, a,i) * wm_interm_18_triplet_pt4(a,q) * wm_interm_54_triplet_pt4(p,i)
term(480) = term(480) + t1(a,i) * wm_interm_19_triplet_pt4(p,q) * wm_interm_50_triplet_pt4(a,i)
term(481) = term(481) + r1(vrdav_Rr, a,i) * wm_interm_19_triplet_pt4(a,q) * wm_interm_54_triplet_pt4(p,i)
term(482) = term(482) + t1(a,i) * wm_interm_18_triplet_pt4(p,q) * wm_interm_46_triplet_pt4(a,i)
term(483) = term(483) + r1(vrdav_Rr, a,i) * wm_interm_18_triplet_pt4(a,q) * wm_interm_55_triplet_pt4(p,i)
term(484) = term(484) + t1(a,i) * wm_interm_19_triplet_pt4(p,q) * wm_interm_46_triplet_pt4(a,i)
term(485) = term(485) + r1(vrdav_Rr, a,i) * wm_interm_19_triplet_pt4(a,q) * wm_interm_55_triplet_pt4(p,i)
term(486) = term(486) + t1(a,i) * wm_interm_18_triplet_pt4(a,q) * wm_interm_58_triplet_pt4(p,i)
term(487) = term(487) + t1(a,i) * wm_interm_19_triplet_pt4(a,q) * wm_interm_58_triplet_pt4(p,i)
term(488) = term(488) + t1(a,i) * wm_interm_18_triplet_pt4(a,q) * wm_interm_60_triplet_pt4(p,i)
term(489) = term(489) + t1(a,i) * wm_interm_19_triplet_pt4(a,q) * wm_interm_60_triplet_pt4(p,i)
term(490) = term(490) + t1(a,i) * wm_interm_160_triplet_pt4(a,q) * wm_interm_2_triplet_pt4(p,i)
term(491) = term(491) + t1(a,i) * wm_interm_164_triplet_pt4(a,q) * wm_interm_2_triplet_pt4(p,i)
term(492) = term(492) + t1(a,i) * wm_interm_18_triplet_pt4(p,q) * wm_interm_60_triplet_pt4(a,i)
term(493) = term(493) + r1(vrdav_Rr, a,i) * wm_interm_18_triplet_pt4(a,q) * wm_interm_64_triplet_pt4(p,i)
term(494) = term(494) + t1(a,i) * wm_interm_19_triplet_pt4(p,q) * wm_interm_60_triplet_pt4(a,i)
term(495) = term(495) + r1(vrdav_Rr, a,i) * wm_interm_19_triplet_pt4(a,q) * wm_interm_64_triplet_pt4(p,i)
term(496) = term(496) + t1(a,i) * wm_interm_18_triplet_pt4(p,q) * wm_interm_58_triplet_pt4(a,i)
term(497) = term(497) + r1(vrdav_Rr, a,i) * wm_interm_18_triplet_pt4(a,q) * wm_interm_65_triplet_pt4(p,i)
term(498) = term(498) + t1(a,i) * wm_interm_19_triplet_pt4(p,q) * wm_interm_58_triplet_pt4(a,i)
term(499) = term(499) + r1(vrdav_Rr, a,i) * wm_interm_19_triplet_pt4(a,q) * wm_interm_65_triplet_pt4(p,i)
end do 
end do 

term(419) = term(419) * (-4.0d+0) 
term(420) = term(420) * (2.0d+0) 
term(421) = term(421) * (8.0d+0) 
term(422) = term(422) * (-4.0d+0) 
term(423) = term(423) * (2.0d+0) 
term(424) = term(424) * (-1.0d+0) 
term(425) = term(425) * (-4.0d+0) 
term(426) = term(426) * (2.0d+0) 
term(427) = term(427) * (2.0d+0) 
term(428) = term(428) * (-1.0d+0) 
term(429) = term(429) * (-4.0d+0) 
term(430) = term(430) * (2.0d+0) 
term(431) = term(431) * (-8.0d+0) 
term(432) = term(432) * (4.0d+0) 
term(433) = term(433) * (16.0d+0) 
term(434) = term(434) * (-8.0d+0) 
term(435) = term(435) * (8.0d+0) 
term(436) = term(436) * (-4.0d+0) 
term(437) = term(437) * (-16.0d+0) 
term(438) = term(438) * (8.0d+0) 
term(439) = term(439) * (-2.0d+0) 
term(440) = term(440) * (4.0d+0) 
term(441) = term(441) * (-2.0d+0) 
term(442) = term(442) * (4.0d+0) 
term(443) = term(443) * (4.0d+0) 
term(444) = term(444) * (-8.0d+0) 
term(445) = term(445) * (-2.0d+0) 
term(446) = term(446) * (4.0d+0) 
term(447) = term(447) * (-2.0d+0) 
term(448) = term(448) * (-2.0d+0) 
term(449) = term(449) * (-2.0d+0) 
term(450) = term(450) * (4.0d+0) 
term(451) = term(451) * (4.0d+0) 
term(452) = term(452) * (4.0d+0) 
term(453) = term(453) * (-8.0d+0) 
term(454) = term(454) * (-8.0d+0) 
term(455) = term(455) * (16.0d+0) 
term(456) = term(456) * (8.0d+0) 
term(457) = term(457) * (-16.0d+0) 
term(458) = term(458) * (-4.0d+0) 
term(459) = term(459) * (8.0d+0) 
term(460) = term(460) * (-4.0d+0) 
term(461) = term(461) * (-8.0d+0) 
term(462) = term(462) * (8.0d+0) 
term(463) = term(463) * (16.0d+0) 
term(464) = term(464) * (-16.0d+0) 
term(466) = term(466) * (-2.0d+0) 
term(467) = term(467) * (-2.0d+0) 
term(468) = term(468) * (4.0d+0) 
term(471) = term(471) * (-2.0d+0) 
term(473) = term(473) * (-2.0d+0) 
term(474) = term(474) * (4.0d+0) 
term(475) = term(475) * (-2.0d+0) 
term(476) = term(476) * (-8.0d+0) 
term(477) = term(477) * (4.0d+0) 
term(478) = term(478) * (-2.0d+0) 
term(480) = term(480) * (4.0d+0) 
term(481) = term(481) * (-2.0d+0) 
term(482) = term(482) * (-2.0d+0) 
term(484) = term(484) * (4.0d+0) 
term(485) = term(485) * (-2.0d+0) 
term(486) = term(486) * (4.0d+0) 
term(487) = term(487) * (-8.0d+0) 
term(488) = term(488) * (-4.0d+0) 
term(489) = term(489) * (8.0d+0) 
term(490) = term(490) * (4.0d+0) 
term(491) = term(491) * (-4.0d+0) 
term(492) = term(492) * (8.0d+0) 
term(493) = term(493) * (-4.0d+0) 
term(494) = term(494) * (-16.0d+0) 
term(495) = term(495) * (8.0d+0) 
term(496) = term(496) * (-8.0d+0) 
term(497) = term(497) * (4.0d+0) 
term(498) = term(498) * (16.0d+0) 
term(499) = term(499) * (-8.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
term(500) = term(500) + s1(q,i) * wm_interm_16_triplet_pt4(j,k) * wm_interm_24_triplet_pt4(p,k,j,i)
term(501) = term(501) + s1(q,i) * wm_interm_17_triplet_pt4(j,k) * wm_interm_24_triplet_pt4(p,k,j,i)
term(502) = term(502) + s1(q,i) * wm_interm_16_triplet_pt4(j,k) * wm_interm_26_triplet_pt4(p,k,j,i)
term(503) = term(503) + s1(q,i) * wm_interm_17_triplet_pt4(j,k) * wm_interm_26_triplet_pt4(p,k,j,i)
term(504) = term(504) + s1(q,i) * wm_interm_16_triplet_pt4(j,k) * wm_interm_29_triplet_pt4(p,k,j,i)
term(505) = term(505) + s1(q,i) * wm_interm_17_triplet_pt4(j,k) * wm_interm_29_triplet_pt4(p,k,j,i)
end do 
end do 
end do 

term(501) = term(501) * (-2.0d+0) 
term(502) = term(502) * (-2.0d+0) 
term(503) = term(503) * (4.0d+0) 
term(504) = term(504) * (-4.0d+0) 
term(505) = term(505) * (8.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
do l = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
term(506) = term(506) + r2p(vrdav_Rl, p,j,a,k) * t1(b,k) * t2(a,q,l,i) * wm_interm_12_triplet_pt4(b,j,l,i)
end do 
end do 
end do 
end do 
end do 
end do 


do l = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
do j = 1, nocc 
do b = nocc + 1, nactive 
term(507) = term(507) + r2p(vrdav_Rl, p,j,a,k) * t1(b,k) * t2(a,q,l,i) * wm_interm_12_triplet_pt4(b,j,i,l)
term(508) = term(508) + r2p(vrdav_Rl, p,j,a,k) * t1(b,j) * t2(a,q,l,i) * wm_interm_12_triplet_pt4(b,k,i,l)
end do 
end do 
end do 
end do 
end do 
end do 

term(507) = term(507) * (-2.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(509) = term(509) + t2(a,q,j,i) * wm_interm_100_triplet_pt4(b,a) * wm_interm_32_triplet_pt4(b,p,j,i)
term(510) = term(510) + t2(a,q,j,i) * wm_interm_32_triplet_pt4(b,p,j,i) * wm_interm_98_triplet_pt4(b,a)
term(511) = term(511) + t2(a,q,j,i) * wm_interm_32_triplet_pt4(b,p,j,i) * wm_interm_99_triplet_pt4(b,a)
term(512) = term(512) + t2(a,q,j,i) * wm_interm_134_triplet_pt4(b,a) * wm_interm_32_triplet_pt4(b,p,j,i)
term(513) = term(513) + t2(a,q,j,i) * wm_interm_135_triplet_pt4(b,a) * wm_interm_32_triplet_pt4(b,p,j,i)
term(514) = term(514) + s2(a,p,j,i) * wm_interm_56_triplet_pt4(b,q,j,i) * wm_interm_70_triplet_pt4(b,a)
term(515) = term(515) + s2(a,p,j,i) * wm_interm_56_triplet_pt4(b,q,j,i) * wm_interm_75_triplet_pt4(b,a)
term(516) = term(516) + s2(a,p,j,i) * wm_interm_56_triplet_pt4(b,q,j,i) * wm_interm_77_triplet_pt4(b,a)
term(517) = term(517) + s2(a,p,j,i) * wm_interm_160_triplet_pt4(b,a) * wm_interm_56_triplet_pt4(b,q,j,i)
term(518) = term(518) + s2(a,p,j,i) * wm_interm_164_triplet_pt4(b,a) * wm_interm_56_triplet_pt4(b,q,j,i)
end do 
end do 
end do 
end do 

term(509) = term(509) * (4.0d+0) 
term(510) = term(510) * (4.0d+0) 
term(511) = term(511) * (-8.0d+0) 
term(512) = term(512) * (16.0d+0) 
term(513) = term(513) * (-16.0d+0) 
term(514) = term(514) * (-2.0d+0) 
term(515) = term(515) * (4.0d+0) 
term(516) = term(516) * (-2.0d+0) 
term(517) = term(517) * (-8.0d+0) 
term(518) = term(518) * (8.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(519) = term(519) + s2(a,p,k,j) * t1(b,k) * t2(a,q,l,i) * wm_interm_47_triplet_pt4(b,l,i,j)
term(520) = term(520) + s2(a,p,k,j) * t1(b,k) * t2(a,q,l,i) * wm_interm_47_triplet_pt4(b,i,l,j)
term(521) = term(521) + s2(a,p,k,j) * t1(b,k) * t2(a,q,l,i) * wm_interm_51_triplet_pt4(b,i,l,j)
term(522) = term(522) + r2m(vrdav_Rl, a,k,p,j) * t1(b,k) * t2(a,q,l,i) * wm_interm_12_triplet_pt4(b,j,l,i)
term(523) = term(523) + s2(a,p,k,j) * t1(b,k) * t2(a,q,l,i) * wm_interm_59_triplet_pt4(b,l,i,j)
term(524) = term(524) + s2(a,p,k,j) * t1(b,k) * t2(a,q,l,i) * wm_interm_59_triplet_pt4(b,i,l,j)
term(525) = term(525) + s2(a,p,k,j) * t1(b,k) * t2(a,q,l,i) * wm_interm_62_triplet_pt4(b,i,l,j)
end do 
end do 
end do 
end do 
end do 
end do 

term(520) = term(520) * (-2.0d+0) 
term(522) = term(522) * (2.0d+0) 
term(523) = term(523) * (2.0d+0) 
term(524) = term(524) * (-4.0d+0) 
term(525) = term(525) * (2.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(526) = term(526) + t2(a,q,j,i) * wm_interm_100_triplet_pt4(b,a) * wm_interm_32_triplet_pt4(b,p,i,j)
term(527) = term(527) + t2(a,q,j,i) * wm_interm_32_triplet_pt4(b,p,i,j) * wm_interm_98_triplet_pt4(b,a)
term(528) = term(528) + t2(a,q,j,i) * wm_interm_32_triplet_pt4(b,p,i,j) * wm_interm_99_triplet_pt4(b,a)
term(529) = term(529) + t2(a,q,j,i) * wm_interm_134_triplet_pt4(b,a) * wm_interm_32_triplet_pt4(b,p,i,j)
term(530) = term(530) + t2(a,q,j,i) * wm_interm_135_triplet_pt4(b,a) * wm_interm_32_triplet_pt4(b,p,i,j)
term(531) = term(531) + r2p(vrdav_Rl, q,i,a,j) * wm_interm_18_triplet_pt4(b,a) * wm_interm_56_triplet_pt4(p,b,j,i)
term(532) = term(532) + r2p(vrdav_Rl, q,i,a,j) * wm_interm_19_triplet_pt4(b,a) * wm_interm_56_triplet_pt4(p,b,j,i)
term(533) = term(533) + r2p(vrdav_Rl, a,i,p,j) * wm_interm_18_triplet_pt4(b,a) * wm_interm_56_triplet_pt4(b,q,j,i)
term(534) = term(534) + r2p(vrdav_Rl, a,i,p,j) * wm_interm_18_triplet_pt4(b,a) * wm_interm_56_triplet_pt4(b,q,i,j)
term(535) = term(535) + r2p(vrdav_Rl, a,i,p,j) * wm_interm_19_triplet_pt4(b,a) * wm_interm_56_triplet_pt4(b,q,j,i)
term(536) = term(536) + r2p(vrdav_Rl, a,i,p,j) * wm_interm_19_triplet_pt4(b,a) * wm_interm_56_triplet_pt4(b,q,i,j)
term(537) = term(537) + s2(a,p,j,i) * wm_interm_56_triplet_pt4(b,q,i,j) * wm_interm_70_triplet_pt4(b,a)
term(538) = term(538) + s2(a,p,j,i) * wm_interm_56_triplet_pt4(b,q,i,j) * wm_interm_75_triplet_pt4(b,a)
term(539) = term(539) + s2(a,p,j,i) * wm_interm_56_triplet_pt4(b,q,i,j) * wm_interm_77_triplet_pt4(b,a)
term(540) = term(540) + r2m(vrdav_Rl, a,j,p,i) * wm_interm_18_triplet_pt4(b,a) * wm_interm_56_triplet_pt4(b,q,i,j)
term(541) = term(541) + r2m(vrdav_Rl, a,j,p,i) * wm_interm_19_triplet_pt4(b,a) * wm_interm_56_triplet_pt4(b,q,i,j)
term(542) = term(542) + r2m(vrdav_Rl, a,i,p,j) * wm_interm_18_triplet_pt4(b,a) * wm_interm_56_triplet_pt4(b,q,j,i)
term(543) = term(543) + r2m(vrdav_Rl, a,i,p,j) * wm_interm_18_triplet_pt4(b,a) * wm_interm_56_triplet_pt4(b,q,i,j)
term(544) = term(544) + r2m(vrdav_Rl, a,i,p,j) * wm_interm_19_triplet_pt4(b,a) * wm_interm_56_triplet_pt4(b,q,j,i)
term(545) = term(545) + r2m(vrdav_Rl, a,i,p,j) * wm_interm_19_triplet_pt4(b,a) * wm_interm_56_triplet_pt4(b,q,i,j)
term(546) = term(546) + s2(a,p,j,i) * wm_interm_160_triplet_pt4(b,a) * wm_interm_56_triplet_pt4(b,q,i,j)
term(547) = term(547) + s2(a,p,j,i) * wm_interm_164_triplet_pt4(b,a) * wm_interm_56_triplet_pt4(b,q,i,j)
end do 
end do 
end do 
end do 

term(526) = term(526) * (-2.0d+0) 
term(527) = term(527) * (-2.0d+0) 
term(528) = term(528) * (4.0d+0) 
term(529) = term(529) * (-8.0d+0) 
term(530) = term(530) * (8.0d+0) 
term(531) = term(531) * (2.0d+0) 
term(532) = term(532) * (-4.0d+0) 
term(534) = term(534) * (-2.0d+0) 
term(535) = term(535) * (-2.0d+0) 
term(536) = term(536) * (4.0d+0) 
term(538) = term(538) * (-2.0d+0) 
term(540) = term(540) * (2.0d+0) 
term(541) = term(541) * (-4.0d+0) 
term(542) = term(542) * (2.0d+0) 
term(543) = term(543) * (-4.0d+0) 
term(544) = term(544) * (-4.0d+0) 
term(545) = term(545) * (8.0d+0) 
term(546) = term(546) * (4.0d+0) 
term(547) = term(547) * (-4.0d+0) 

do l = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(548) = term(548) + r2m(vrdav_Rl, a,k,p,j) * t1(b,k) * t2(a,q,l,i) * wm_interm_12_triplet_pt4(b,j,i,l)
term(549) = term(549) + r2m(vrdav_Rl, a,k,p,j) * t1(b,j) * t2(a,q,l,i) * wm_interm_12_triplet_pt4(b,k,i,l)
end do 
end do 
end do 
end do 
end do 
end do 

term(548) = term(548) * (-4.0d+0) 
term(549) = term(549) * (2.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(550) = term(550) + r1(vrdav_Rl, a,i) * wm_interm_10_triplet_pt4(p,q,j,i) * wm_interm_33_triplet_pt4(a,j)
term(551) = term(551) + r1(vrdav_Rl, a,i) * wm_interm_1_triplet_pt4(p,q,j,i) * wm_interm_33_triplet_pt4(a,j)
term(552) = term(552) + r1(vrdav_Rl, a,i) * wm_interm_33_triplet_pt4(a,j) * wm_interm_5_triplet_pt4(p,q,j,i)
term(553) = term(553) + r1(vrdav_Rl, a,i) * wm_interm_11_triplet_pt4(p,q,j,i) * wm_interm_33_triplet_pt4(a,j)
term(554) = term(554) + r1(vrdav_Rl, p,i) * wm_interm_33_triplet_pt4(a,j) * wm_interm_5_triplet_pt4(a,q,j,i)
term(555) = term(555) + r1(vrdav_Rl, a,i) * wm_interm_10_triplet_pt4(p,q,j,i) * wm_interm_36_triplet_pt4(a,j)
term(556) = term(556) + r1(vrdav_Rl, a,i) * wm_interm_1_triplet_pt4(p,q,j,i) * wm_interm_36_triplet_pt4(a,j)
term(557) = term(557) + r1(vrdav_Rl, a,i) * wm_interm_36_triplet_pt4(a,j) * wm_interm_5_triplet_pt4(p,q,j,i)
term(558) = term(558) + r1(vrdav_Rl, a,i) * wm_interm_11_triplet_pt4(p,q,j,i) * wm_interm_36_triplet_pt4(a,j)
term(559) = term(559) + r1(vrdav_Rl, p,i) * wm_interm_36_triplet_pt4(a,j) * wm_interm_5_triplet_pt4(a,q,j,i)
term(560) = term(560) + r1(vrdav_Rl, a,i) * wm_interm_10_triplet_pt4(p,q,j,i) * wm_interm_38_triplet_pt4(a,j)
term(561) = term(561) + r1(vrdav_Rl, a,i) * wm_interm_1_triplet_pt4(p,q,j,i) * wm_interm_38_triplet_pt4(a,j)
term(562) = term(562) + r1(vrdav_Rl, a,i) * wm_interm_38_triplet_pt4(a,j) * wm_interm_5_triplet_pt4(p,q,j,i)
term(563) = term(563) + r1(vrdav_Rl, a,i) * wm_interm_11_triplet_pt4(p,q,j,i) * wm_interm_38_triplet_pt4(a,j)
term(564) = term(564) + r1(vrdav_Rl, p,i) * wm_interm_38_triplet_pt4(a,j) * wm_interm_5_triplet_pt4(a,q,j,i)
term(565) = term(565) + r1(vrdav_Rl, a,i) * wm_interm_10_triplet_pt4(p,q,j,i) * wm_interm_40_triplet_pt4(a,j)
term(566) = term(566) + r1(vrdav_Rl, a,i) * wm_interm_1_triplet_pt4(p,q,j,i) * wm_interm_40_triplet_pt4(a,j)
term(567) = term(567) + r1(vrdav_Rl, a,i) * wm_interm_40_triplet_pt4(a,j) * wm_interm_5_triplet_pt4(p,q,j,i)
term(568) = term(568) + r1(vrdav_Rl, a,i) * wm_interm_11_triplet_pt4(p,q,j,i) * wm_interm_40_triplet_pt4(a,j)
term(569) = term(569) + r1(vrdav_Rl, p,i) * wm_interm_40_triplet_pt4(a,j) * wm_interm_5_triplet_pt4(a,q,j,i)
term(570) = term(570) + r1(vrdav_Rl, a,i) * wm_interm_10_triplet_pt4(p,q,j,i) * wm_interm_42_triplet_pt4(a,j)
term(571) = term(571) + r1(vrdav_Rl, a,i) * wm_interm_1_triplet_pt4(p,q,j,i) * wm_interm_42_triplet_pt4(a,j)
term(572) = term(572) + r1(vrdav_Rl, a,i) * wm_interm_42_triplet_pt4(a,j) * wm_interm_5_triplet_pt4(p,q,j,i)
term(573) = term(573) + r1(vrdav_Rl, a,i) * wm_interm_11_triplet_pt4(p,q,j,i) * wm_interm_42_triplet_pt4(a,j)
term(574) = term(574) + r1(vrdav_Rl, p,i) * wm_interm_42_triplet_pt4(a,j) * wm_interm_5_triplet_pt4(a,q,j,i)
term(575) = term(575) + s2(a,p,j,i) * wm_interm_39_triplet_pt4(a,j) * wm_interm_7_triplet_pt4(q,i)
term(576) = term(576) + s2(a,p,j,i) * wm_interm_37_triplet_pt4(a,j) * wm_interm_7_triplet_pt4(q,i)
term(577) = term(577) + s2(a,p,j,i) * wm_interm_34_triplet_pt4(a,j) * wm_interm_7_triplet_pt4(q,i)
term(578) = term(578) + s2(a,p,j,i) * wm_interm_39_triplet_pt4(a,j) * wm_interm_6_triplet_pt4(q,i)
term(579) = term(579) + s2(a,p,j,i) * wm_interm_37_triplet_pt4(a,j) * wm_interm_6_triplet_pt4(q,i)
term(580) = term(580) + s2(a,p,j,i) * wm_interm_34_triplet_pt4(a,j) * wm_interm_6_triplet_pt4(q,i)
term(581) = term(581) + s1(p,i) * wm_interm_39_triplet_pt4(a,j) * wm_interm_5_triplet_pt4(a,q,j,i)
term(582) = term(582) + s1(p,i) * wm_interm_37_triplet_pt4(a,j) * wm_interm_5_triplet_pt4(a,q,j,i)
term(583) = term(583) + s1(p,i) * wm_interm_34_triplet_pt4(a,j) * wm_interm_5_triplet_pt4(a,q,j,i)
term(584) = term(584) + s1(p,i) * wm_interm_1_triplet_pt4(a,q,j,i) * wm_interm_39_triplet_pt4(a,j)
term(585) = term(585) + s1(p,i) * wm_interm_1_triplet_pt4(a,q,j,i) * wm_interm_37_triplet_pt4(a,j)
term(586) = term(586) + s1(p,i) * wm_interm_1_triplet_pt4(a,q,j,i) * wm_interm_34_triplet_pt4(a,j)
term(587) = term(587) + s1(a,i) * wm_interm_1_triplet_pt4(p,q,j,i) * wm_interm_39_triplet_pt4(a,j)
term(588) = term(588) + s1(a,i) * wm_interm_1_triplet_pt4(p,q,j,i) * wm_interm_37_triplet_pt4(a,j)
term(589) = term(589) + s1(a,i) * wm_interm_1_triplet_pt4(p,q,j,i) * wm_interm_34_triplet_pt4(a,j)
term(590) = term(590) + s1(a,i) * wm_interm_39_triplet_pt4(a,j) * wm_interm_5_triplet_pt4(p,q,j,i)
term(591) = term(591) + s1(a,i) * wm_interm_37_triplet_pt4(a,j) * wm_interm_5_triplet_pt4(p,q,j,i)
term(592) = term(592) + s1(a,i) * wm_interm_34_triplet_pt4(a,j) * wm_interm_5_triplet_pt4(p,q,j,i)
term(593) = term(593) + s1(p,i) * wm_interm_10_triplet_pt4(a,q,j,i) * wm_interm_39_triplet_pt4(a,j)
term(594) = term(594) + s1(p,i) * wm_interm_10_triplet_pt4(a,q,j,i) * wm_interm_37_triplet_pt4(a,j)
term(595) = term(595) + s1(p,i) * wm_interm_10_triplet_pt4(a,q,j,i) * wm_interm_34_triplet_pt4(a,j)
term(596) = term(596) + s1(p,i) * wm_interm_11_triplet_pt4(a,q,j,i) * wm_interm_39_triplet_pt4(a,j)
term(597) = term(597) + s1(p,i) * wm_interm_11_triplet_pt4(a,q,j,i) * wm_interm_37_triplet_pt4(a,j)
term(598) = term(598) + s1(p,i) * wm_interm_11_triplet_pt4(a,q,j,i) * wm_interm_34_triplet_pt4(a,j)
term(599) = term(599) + s1(a,i) * wm_interm_10_triplet_pt4(p,q,j,i) * wm_interm_39_triplet_pt4(a,j)
term(600) = term(600) + s1(a,i) * wm_interm_11_triplet_pt4(p,q,j,i) * wm_interm_39_triplet_pt4(a,j)
term(601) = term(601) + s1(a,i) * wm_interm_10_triplet_pt4(p,q,j,i) * wm_interm_37_triplet_pt4(a,j)
term(602) = term(602) + s1(a,i) * wm_interm_10_triplet_pt4(p,q,j,i) * wm_interm_34_triplet_pt4(a,j)
term(603) = term(603) + s1(a,i) * wm_interm_11_triplet_pt4(p,q,j,i) * wm_interm_37_triplet_pt4(a,j)
term(604) = term(604) + s1(a,i) * wm_interm_11_triplet_pt4(p,q,j,i) * wm_interm_34_triplet_pt4(a,j)
term(605) = term(605) + s2(a,p,j,i) * wm_interm_43_triplet_pt4(a,j) * wm_interm_7_triplet_pt4(q,i)
term(606) = term(606) + s2(a,p,j,i) * wm_interm_41_triplet_pt4(a,j) * wm_interm_7_triplet_pt4(q,i)
term(607) = term(607) + s2(a,p,j,i) * wm_interm_43_triplet_pt4(a,j) * wm_interm_6_triplet_pt4(q,i)
term(608) = term(608) + s2(a,p,j,i) * wm_interm_41_triplet_pt4(a,j) * wm_interm_6_triplet_pt4(q,i)
term(609) = term(609) + s1(p,i) * wm_interm_43_triplet_pt4(a,j) * wm_interm_5_triplet_pt4(a,q,j,i)
term(610) = term(610) + s1(p,i) * wm_interm_41_triplet_pt4(a,j) * wm_interm_5_triplet_pt4(a,q,j,i)
term(611) = term(611) + s1(p,i) * wm_interm_1_triplet_pt4(a,q,j,i) * wm_interm_43_triplet_pt4(a,j)
term(612) = term(612) + s1(p,i) * wm_interm_1_triplet_pt4(a,q,j,i) * wm_interm_41_triplet_pt4(a,j)
term(613) = term(613) + s1(a,i) * wm_interm_1_triplet_pt4(p,q,j,i) * wm_interm_43_triplet_pt4(a,j)
term(614) = term(614) + s1(a,i) * wm_interm_1_triplet_pt4(p,q,j,i) * wm_interm_41_triplet_pt4(a,j)
term(615) = term(615) + s1(a,i) * wm_interm_43_triplet_pt4(a,j) * wm_interm_5_triplet_pt4(p,q,j,i)
term(616) = term(616) + s1(a,i) * wm_interm_41_triplet_pt4(a,j) * wm_interm_5_triplet_pt4(p,q,j,i)
term(617) = term(617) + s1(p,i) * wm_interm_10_triplet_pt4(a,q,j,i) * wm_interm_43_triplet_pt4(a,j)
term(618) = term(618) + s1(p,i) * wm_interm_10_triplet_pt4(a,q,j,i) * wm_interm_41_triplet_pt4(a,j)
term(619) = term(619) + s1(p,i) * wm_interm_11_triplet_pt4(a,q,j,i) * wm_interm_43_triplet_pt4(a,j)
term(620) = term(620) + s1(p,i) * wm_interm_11_triplet_pt4(a,q,j,i) * wm_interm_41_triplet_pt4(a,j)
term(621) = term(621) + s1(a,i) * wm_interm_10_triplet_pt4(p,q,j,i) * wm_interm_43_triplet_pt4(a,j)
term(622) = term(622) + s1(a,i) * wm_interm_11_triplet_pt4(p,q,j,i) * wm_interm_43_triplet_pt4(a,j)
term(623) = term(623) + s1(a,i) * wm_interm_10_triplet_pt4(p,q,j,i) * wm_interm_41_triplet_pt4(a,j)
term(624) = term(624) + s1(a,i) * wm_interm_11_triplet_pt4(p,q,j,i) * wm_interm_41_triplet_pt4(a,j)
end do 
end do 
end do 

term(550) = term(550) * (2.0d+0) 
term(551) = term(551) * (2.0d+0) 
term(552) = term(552) * (-4.0d+0) 
term(553) = term(553) * (-4.0d+0) 
term(554) = term(554) * (2.0d+0) 
term(555) = term(555) * (-1.0d+0) 
term(556) = term(556) * (-1.0d+0) 
term(557) = term(557) * (2.0d+0) 
term(558) = term(558) * (2.0d+0) 
term(559) = term(559) * (-1.0d+0) 
term(560) = term(560) * (-1.0d+0) 
term(561) = term(561) * (-1.0d+0) 
term(562) = term(562) * (2.0d+0) 
term(563) = term(563) * (2.0d+0) 
term(564) = term(564) * (-1.0d+0) 
term(565) = term(565) * (4.0d+0) 
term(566) = term(566) * (4.0d+0) 
term(567) = term(567) * (-8.0d+0) 
term(568) = term(568) * (-8.0d+0) 
term(569) = term(569) * (4.0d+0) 
term(570) = term(570) * (-4.0d+0) 
term(571) = term(571) * (-4.0d+0) 
term(572) = term(572) * (8.0d+0) 
term(573) = term(573) * (8.0d+0) 
term(574) = term(574) * (-4.0d+0) 
term(575) = term(575) * (4.0d+0) 
term(576) = term(576) * (4.0d+0) 
term(577) = term(577) * (-8.0d+0) 
term(578) = term(578) * (-8.0d+0) 
term(579) = term(579) * (-8.0d+0) 
term(580) = term(580) * (16.0d+0) 
term(581) = term(581) * (-2.0d+0) 
term(582) = term(582) * (-2.0d+0) 
term(583) = term(583) * (4.0d+0) 
term(584) = term(584) * (4.0d+0) 
term(585) = term(585) * (4.0d+0) 
term(586) = term(586) * (-8.0d+0) 
term(587) = term(587) * (-2.0d+0) 
term(588) = term(588) * (-2.0d+0) 
term(589) = term(589) * (4.0d+0) 
term(590) = term(590) * (4.0d+0) 
term(591) = term(591) * (4.0d+0) 
term(592) = term(592) * (-8.0d+0) 
term(593) = term(593) * (4.0d+0) 
term(594) = term(594) * (4.0d+0) 
term(595) = term(595) * (-8.0d+0) 
term(596) = term(596) * (-8.0d+0) 
term(597) = term(597) * (-8.0d+0) 
term(598) = term(598) * (16.0d+0) 
term(599) = term(599) * (-2.0d+0) 
term(600) = term(600) * (4.0d+0) 
term(601) = term(601) * (-2.0d+0) 
term(602) = term(602) * (4.0d+0) 
term(603) = term(603) * (4.0d+0) 
term(604) = term(604) * (-8.0d+0) 
term(605) = term(605) * (16.0d+0) 
term(606) = term(606) * (-16.0d+0) 
term(607) = term(607) * (-32.0d+0) 
term(608) = term(608) * (32.0d+0) 
term(609) = term(609) * (-8.0d+0) 
term(610) = term(610) * (8.0d+0) 
term(611) = term(611) * (16.0d+0) 
term(612) = term(612) * (-16.0d+0) 
term(613) = term(613) * (-8.0d+0) 
term(614) = term(614) * (8.0d+0) 
term(615) = term(615) * (16.0d+0) 
term(616) = term(616) * (-16.0d+0) 
term(617) = term(617) * (16.0d+0) 
term(618) = term(618) * (-16.0d+0) 
term(619) = term(619) * (-32.0d+0) 
term(620) = term(620) * (32.0d+0) 
term(621) = term(621) * (-8.0d+0) 
term(622) = term(622) * (16.0d+0) 
term(623) = term(623) * (8.0d+0) 
term(624) = term(624) * (-16.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(625) = term(625) + t2(a,q,j,i) * wm_interm_32_triplet_pt4(p,b,j,i) * wm_interm_98_triplet_pt4(b,a)
term(626) = term(626) + t2(a,q,j,i) * wm_interm_32_triplet_pt4(p,b,j,i) * wm_interm_99_triplet_pt4(b,a)
term(627) = term(627) + t2(a,q,j,i) * wm_interm_100_triplet_pt4(b,a) * wm_interm_32_triplet_pt4(p,b,j,i)
term(628) = term(628) + t2(a,q,j,i) * wm_interm_134_triplet_pt4(b,a) * wm_interm_32_triplet_pt4(p,b,j,i)
term(629) = term(629) + t2(a,q,j,i) * wm_interm_135_triplet_pt4(b,a) * wm_interm_32_triplet_pt4(p,b,j,i)
term(630) = term(630) + t2(a,q,j,i) * wm_interm_150_triplet_pt4(b,a) * wm_interm_32_triplet_pt4(p,b,j,i)
term(631) = term(631) + s2(a,q,j,i) * wm_interm_56_triplet_pt4(p,b,j,i) * wm_interm_77_triplet_pt4(b,a)
term(632) = term(632) + s2(a,q,j,i) * wm_interm_56_triplet_pt4(p,b,j,i) * wm_interm_70_triplet_pt4(b,a)
term(633) = term(633) + s2(a,q,j,i) * wm_interm_56_triplet_pt4(p,b,j,i) * wm_interm_75_triplet_pt4(b,a)
term(634) = term(634) + s2(a,q,j,i) * wm_interm_160_triplet_pt4(b,a) * wm_interm_56_triplet_pt4(p,b,j,i)
term(635) = term(635) + r2m(vrdav_Rl, a,j,q,i) * wm_interm_18_triplet_pt4(b,a) * wm_interm_56_triplet_pt4(p,b,j,i)
term(636) = term(636) + r2m(vrdav_Rl, a,j,q,i) * wm_interm_19_triplet_pt4(b,a) * wm_interm_56_triplet_pt4(p,b,j,i)
term(637) = term(637) + s2(a,q,j,i) * wm_interm_164_triplet_pt4(b,a) * wm_interm_56_triplet_pt4(p,b,j,i)
end do 
end do 
end do 
end do 

term(625) = term(625) * (-2.0d+0) 
term(626) = term(626) * (4.0d+0) 
term(627) = term(627) * (-2.0d+0) 
term(628) = term(628) * (-4.0d+0) 
term(629) = term(629) * (8.0d+0) 
term(630) = term(630) * (-4.0d+0) 
term(631) = term(631) * (2.0d+0) 
term(632) = term(632) * (2.0d+0) 
term(633) = term(633) * (-4.0d+0) 
term(634) = term(634) * (8.0d+0) 
term(635) = term(635) * (4.0d+0) 
term(636) = term(636) * (-8.0d+0) 
term(637) = term(637) * (-8.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
term(638) = term(638) + s1(p,i) * wm_interm_102_triplet_pt4(j,k) * wm_interm_13_triplet_pt4(q,j,k,i)
term(639) = term(639) + s1(p,i) * wm_interm_103_triplet_pt4(j,k) * wm_interm_13_triplet_pt4(q,j,k,i)
term(640) = term(640) + s1(p,i) * wm_interm_104_triplet_pt4(j,k) * wm_interm_13_triplet_pt4(q,j,k,i)
term(641) = term(641) + s1(p,i) * wm_interm_136_triplet_pt4(j,k) * wm_interm_13_triplet_pt4(q,j,k,i)
term(642) = term(642) + s1(p,i) * wm_interm_137_triplet_pt4(j,k) * wm_interm_13_triplet_pt4(q,j,k,i)
term(643) = term(643) + s1(p,i) * wm_interm_138_triplet_pt4(j,k) * wm_interm_13_triplet_pt4(q,j,k,i)
term(644) = term(644) + t1(q,i) * wm_interm_12_triplet_pt4(p,j,k,i) * wm_interm_72_triplet_pt4(j,k)
term(645) = term(645) + t1(q,i) * wm_interm_12_triplet_pt4(p,j,k,i) * wm_interm_76_triplet_pt4(j,k)
term(646) = term(646) + t1(q,i) * wm_interm_12_triplet_pt4(p,j,k,i) * wm_interm_79_triplet_pt4(j,k)
term(647) = term(647) + t1(q,i) * wm_interm_12_triplet_pt4(p,j,k,i) * wm_interm_162_triplet_pt4(j,k)
term(648) = term(648) + t1(q,i) * wm_interm_12_triplet_pt4(p,j,k,i) * wm_interm_165_triplet_pt4(j,k)
end do 
end do 
end do 

term(638) = term(638) * (4.0d+0) 
term(639) = term(639) * (-8.0d+0) 
term(640) = term(640) * (4.0d+0) 
term(641) = term(641) * (8.0d+0) 
term(642) = term(642) * (-16.0d+0) 
term(643) = term(643) * (8.0d+0) 
term(644) = term(644) * (-2.0d+0) 
term(645) = term(645) * (4.0d+0) 
term(646) = term(646) * (-2.0d+0) 
term(647) = term(647) * (-8.0d+0) 
term(648) = term(648) * (8.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(649) = term(649) + s2(a,q,j,i) * wm_interm_24_triplet_pt4(p,k,j,i) * wm_interm_6_triplet_pt4(a,k)
term(650) = term(650) + s2(a,q,j,i) * wm_interm_26_triplet_pt4(p,k,j,i) * wm_interm_6_triplet_pt4(a,k)
term(651) = term(651) + s2(a,q,j,i) * wm_interm_24_triplet_pt4(p,k,j,i) * wm_interm_7_triplet_pt4(a,k)
term(652) = term(652) + s2(a,q,j,i) * wm_interm_26_triplet_pt4(p,k,j,i) * wm_interm_7_triplet_pt4(a,k)
term(653) = term(653) + s2(a,q,j,i) * wm_interm_29_triplet_pt4(p,k,j,i) * wm_interm_6_triplet_pt4(a,k)
term(654) = term(654) + s2(a,q,j,i) * wm_interm_29_triplet_pt4(p,k,j,i) * wm_interm_7_triplet_pt4(a,k)
term(655) = term(655) + s2(a,p,j,i) * wm_interm_13_triplet_pt4(q,k,j,i) * wm_interm_33_triplet_pt4(a,k)
term(656) = term(656) + s2(a,p,j,i) * wm_interm_13_triplet_pt4(q,k,j,i) * wm_interm_36_triplet_pt4(a,k)
term(657) = term(657) + s2(a,p,j,i) * wm_interm_13_triplet_pt4(q,k,j,i) * wm_interm_38_triplet_pt4(a,k)
term(658) = term(658) + s2(a,p,j,i) * wm_interm_13_triplet_pt4(q,k,j,i) * wm_interm_40_triplet_pt4(a,k)
term(659) = term(659) + s2(a,p,j,i) * wm_interm_13_triplet_pt4(q,k,j,i) * wm_interm_42_triplet_pt4(a,k)
term(660) = term(660) + s2(a,p,j,i) * wm_interm_39_triplet_pt4(a,k) * wm_interm_3_triplet_pt4(q,k,j,i)
term(661) = term(661) + s2(a,p,j,i) * wm_interm_37_triplet_pt4(a,k) * wm_interm_3_triplet_pt4(q,k,j,i)
term(662) = term(662) + s2(a,p,j,i) * wm_interm_34_triplet_pt4(a,k) * wm_interm_3_triplet_pt4(q,k,j,i)
term(663) = term(663) + s2(a,p,j,i) * wm_interm_3_triplet_pt4(q,k,j,i) * wm_interm_43_triplet_pt4(a,k)
term(664) = term(664) + s2(a,p,j,i) * wm_interm_3_triplet_pt4(q,k,j,i) * wm_interm_41_triplet_pt4(a,k)
term(665) = term(665) + t2(a,q,j,i) * wm_interm_12_triplet_pt4(p,k,j,i) * wm_interm_53_triplet_pt4(a,k)
term(666) = term(666) + t2(a,q,j,i) * wm_interm_12_triplet_pt4(p,k,j,i) * wm_interm_54_triplet_pt4(a,k)
term(667) = term(667) + t2(a,q,j,i) * wm_interm_12_triplet_pt4(p,k,j,i) * wm_interm_55_triplet_pt4(a,k)
term(668) = term(668) + t2(a,q,j,i) * wm_interm_12_triplet_pt4(p,k,j,i) * wm_interm_64_triplet_pt4(a,k)
term(669) = term(669) + t2(a,q,j,i) * wm_interm_12_triplet_pt4(p,k,j,i) * wm_interm_65_triplet_pt4(a,k)
end do 
end do 
end do 
end do 

term(649) = term(649) * (4.0d+0) 
term(650) = term(650) * (-8.0d+0) 
term(651) = term(651) * (-2.0d+0) 
term(652) = term(652) * (4.0d+0) 
term(653) = term(653) * (-16.0d+0) 
term(654) = term(654) * (8.0d+0) 
term(655) = term(655) * (-4.0d+0) 
term(656) = term(656) * (2.0d+0) 
term(657) = term(657) * (2.0d+0) 
term(658) = term(658) * (-8.0d+0) 
term(659) = term(659) * (8.0d+0) 
term(660) = term(660) * (4.0d+0) 
term(661) = term(661) * (4.0d+0) 
term(662) = term(662) * (-8.0d+0) 
term(663) = term(663) * (16.0d+0) 
term(664) = term(664) * (-16.0d+0) 
term(665) = term(665) * (4.0d+0) 
term(666) = term(666) * (-2.0d+0) 
term(667) = term(667) * (-2.0d+0) 
term(668) = term(668) * (8.0d+0) 
term(669) = term(669) * (-8.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(670) = term(670) + r1(vrdav_Rl, q,i) * wm_interm_3_triplet_pt4(a,j,k,i) * wm_interm_84_triplet_pt4(p,a,j,k)
term(671) = term(671) + r1(vrdav_Rl, q,i) * wm_interm_3_triplet_pt4(a,j,k,i) * wm_interm_85_triplet_pt4(p,a,j,k)
term(672) = term(672) + r1(vrdav_Rl, q,i) * wm_interm_3_triplet_pt4(a,j,k,i) * wm_interm_86_triplet_pt4(p,a,j,k)
term(673) = term(673) + r1(vrdav_Rl, q,i) * wm_interm_3_triplet_pt4(a,j,k,i) * wm_interm_87_triplet_pt4(p,a,j,k)
term(674) = term(674) + s1(a,i) * wm_interm_1_triplet_pt4(q,a,j,k) * wm_interm_26_triplet_pt4(p,k,j,i)
term(675) = term(675) + r1(vrdav_Rl, q,i) * wm_interm_3_triplet_pt4(a,j,k,i) * wm_interm_90_triplet_pt4(p,a,j,k)
term(676) = term(676) + r1(vrdav_Rl, q,i) * wm_interm_3_triplet_pt4(a,j,k,i) * wm_interm_91_triplet_pt4(p,a,j,k)
term(677) = term(677) + s1(a,i) * wm_interm_10_triplet_pt4(q,a,j,k) * wm_interm_26_triplet_pt4(p,k,j,i)
term(678) = term(678) + s1(a,i) * wm_interm_11_triplet_pt4(q,a,j,k) * wm_interm_26_triplet_pt4(p,k,j,i)
term(679) = term(679) + s1(q,i) * wm_interm_13_triplet_pt4(a,j,k,i) * wm_interm_84_triplet_pt4(p,a,j,k)
term(680) = term(680) + s1(q,i) * wm_interm_13_triplet_pt4(a,j,k,i) * wm_interm_85_triplet_pt4(p,a,j,k)
term(681) = term(681) + s1(q,i) * wm_interm_13_triplet_pt4(a,j,k,i) * wm_interm_86_triplet_pt4(p,a,j,k)
term(682) = term(682) + s1(q,i) * wm_interm_13_triplet_pt4(a,j,k,i) * wm_interm_91_triplet_pt4(p,a,j,k)
term(683) = term(683) + s1(q,i) * wm_interm_13_triplet_pt4(a,j,k,i) * wm_interm_90_triplet_pt4(p,a,j,k)
term(684) = term(684) + s1(q,i) * wm_interm_101_triplet_pt4(p,a,j,k) * wm_interm_13_triplet_pt4(a,j,k,i)
term(685) = term(685) + s2(a,q,j,i) * wm_interm_15_triplet_pt4(a,k) * wm_interm_27_triplet_pt4(p,j,i,k)
term(686) = term(686) + s2(a,q,j,i) * wm_interm_15_triplet_pt4(a,k) * wm_interm_28_triplet_pt4(p,i,j,k)
term(687) = term(687) + s2(a,q,j,i) * wm_interm_15_triplet_pt4(a,k) * wm_interm_28_triplet_pt4(p,j,i,k)
term(688) = term(688) + r1(vrdav_Rl, q,i) * wm_interm_125_triplet_pt4(p,a,j,k) * wm_interm_3_triplet_pt4(a,j,k,i)
term(689) = term(689) + r1(vrdav_Rl, q,i) * wm_interm_126_triplet_pt4(p,a,j,k) * wm_interm_3_triplet_pt4(a,j,k,i)
term(690) = term(690) + r1(vrdav_Rl, q,i) * wm_interm_127_triplet_pt4(p,a,j,k) * wm_interm_3_triplet_pt4(a,j,k,i)
term(691) = term(691) + r1(vrdav_Rl, q,i) * wm_interm_128_triplet_pt4(p,a,j,k) * wm_interm_3_triplet_pt4(a,j,k,i)
term(692) = term(692) + s1(a,i) * wm_interm_1_triplet_pt4(q,a,j,k) * wm_interm_29_triplet_pt4(p,k,j,i)
term(693) = term(693) + s1(a,i) * wm_interm_10_triplet_pt4(q,a,j,k) * wm_interm_29_triplet_pt4(p,k,j,i)
term(694) = term(694) + s1(a,i) * wm_interm_11_triplet_pt4(q,a,j,k) * wm_interm_29_triplet_pt4(p,k,j,i)
term(695) = term(695) + s1(q,i) * wm_interm_125_triplet_pt4(p,a,j,k) * wm_interm_13_triplet_pt4(a,j,k,i)
term(696) = term(696) + s1(q,i) * wm_interm_126_triplet_pt4(p,a,j,k) * wm_interm_13_triplet_pt4(a,j,k,i)
term(697) = term(697) + s1(q,i) * wm_interm_127_triplet_pt4(p,a,j,k) * wm_interm_13_triplet_pt4(a,j,k,i)
term(698) = term(698) + s1(q,i) * wm_interm_128_triplet_pt4(p,a,j,k) * wm_interm_13_triplet_pt4(a,j,k,i)
term(699) = term(699) + s2(a,q,j,i) * wm_interm_15_triplet_pt4(a,k) * wm_interm_30_triplet_pt4(p,j,i,k)
term(700) = term(700) + s2(a,q,j,i) * wm_interm_15_triplet_pt4(a,k) * wm_interm_31_triplet_pt4(p,i,j,k)
term(701) = term(701) + s2(a,q,j,i) * wm_interm_15_triplet_pt4(a,k) * wm_interm_31_triplet_pt4(p,j,i,k)
term(702) = term(702) + r1(vrdav_Rl, a,i) * wm_interm_3_triplet_pt4(q,j,k,i) * wm_interm_91_triplet_pt4(a,p,j,k)
term(703) = term(703) + r1(vrdav_Rl, a,i) * wm_interm_3_triplet_pt4(q,j,k,i) * wm_interm_84_triplet_pt4(a,p,j,k)
term(704) = term(704) + r1(vrdav_Rl, a,i) * wm_interm_3_triplet_pt4(q,j,k,i) * wm_interm_85_triplet_pt4(a,p,j,k)
term(705) = term(705) + r1(vrdav_Rl, a,i) * wm_interm_3_triplet_pt4(q,j,k,i) * wm_interm_90_triplet_pt4(a,p,j,k)
term(706) = term(706) + r1(vrdav_Rl, a,i) * wm_interm_3_triplet_pt4(q,j,k,i) * wm_interm_86_triplet_pt4(a,p,j,k)
term(707) = term(707) + r1(vrdav_Rl, a,i) * wm_interm_3_triplet_pt4(q,j,k,i) * wm_interm_87_triplet_pt4(a,p,j,k)
term(708) = term(708) + s1(p,i) * wm_interm_10_triplet_pt4(a,q,j,k) * wm_interm_26_triplet_pt4(a,k,j,i)
term(709) = term(709) + s1(p,i) * wm_interm_1_triplet_pt4(a,q,j,k) * wm_interm_26_triplet_pt4(a,k,j,i)
term(710) = term(710) + s1(p,i) * wm_interm_11_triplet_pt4(a,q,j,k) * wm_interm_26_triplet_pt4(a,k,j,i)
term(711) = term(711) + s1(a,i) * wm_interm_13_triplet_pt4(q,j,k,i) * wm_interm_84_triplet_pt4(a,p,j,k)
term(712) = term(712) + s1(a,i) * wm_interm_13_triplet_pt4(q,j,k,i) * wm_interm_85_triplet_pt4(a,p,j,k)
term(713) = term(713) + s1(a,i) * wm_interm_13_triplet_pt4(q,j,k,i) * wm_interm_86_triplet_pt4(a,p,j,k)
term(714) = term(714) + s1(a,i) * wm_interm_13_triplet_pt4(q,j,k,i) * wm_interm_90_triplet_pt4(a,p,j,k)
term(715) = term(715) + s1(a,i) * wm_interm_101_triplet_pt4(a,p,j,k) * wm_interm_13_triplet_pt4(q,j,k,i)
term(716) = term(716) + s1(a,i) * wm_interm_13_triplet_pt4(q,j,k,i) * wm_interm_91_triplet_pt4(a,p,j,k)
term(717) = term(717) + r1(vrdav_Rl, a,i) * wm_interm_125_triplet_pt4(a,p,j,k) * wm_interm_3_triplet_pt4(q,j,k,i)
term(718) = term(718) + r1(vrdav_Rl, a,i) * wm_interm_126_triplet_pt4(a,p,j,k) * wm_interm_3_triplet_pt4(q,j,k,i)
term(719) = term(719) + r1(vrdav_Rl, a,i) * wm_interm_127_triplet_pt4(a,p,j,k) * wm_interm_3_triplet_pt4(q,j,k,i)
term(720) = term(720) + r1(vrdav_Rl, a,i) * wm_interm_128_triplet_pt4(a,p,j,k) * wm_interm_3_triplet_pt4(q,j,k,i)
term(721) = term(721) + s1(p,i) * wm_interm_10_triplet_pt4(a,q,j,k) * wm_interm_29_triplet_pt4(a,k,j,i)
term(722) = term(722) + s1(p,i) * wm_interm_1_triplet_pt4(a,q,j,k) * wm_interm_29_triplet_pt4(a,k,j,i)
term(723) = term(723) + s1(p,i) * wm_interm_11_triplet_pt4(a,q,j,k) * wm_interm_29_triplet_pt4(a,k,j,i)
term(724) = term(724) + s1(a,i) * wm_interm_125_triplet_pt4(a,p,j,k) * wm_interm_13_triplet_pt4(q,j,k,i)
term(725) = term(725) + s1(a,i) * wm_interm_126_triplet_pt4(a,p,j,k) * wm_interm_13_triplet_pt4(q,j,k,i)
term(726) = term(726) + s1(a,i) * wm_interm_127_triplet_pt4(a,p,j,k) * wm_interm_13_triplet_pt4(q,j,k,i)
term(727) = term(727) + s1(a,i) * wm_interm_128_triplet_pt4(a,p,j,k) * wm_interm_13_triplet_pt4(q,j,k,i)
term(728) = term(728) + t1(q,i) * wm_interm_12_triplet_pt4(a,j,k,i) * wm_interm_66_triplet_pt4(p,a,j,k)
term(729) = term(729) + t1(q,i) * wm_interm_12_triplet_pt4(a,j,k,i) * wm_interm_67_triplet_pt4(p,a,j,k)
term(730) = term(730) + t1(q,i) * wm_interm_12_triplet_pt4(a,j,k,i) * wm_interm_68_triplet_pt4(p,a,j,k)
term(731) = term(731) + t1(q,i) * wm_interm_12_triplet_pt4(a,j,k,i) * wm_interm_69_triplet_pt4(p,a,j,k)
term(732) = term(732) + t1(q,i) * wm_interm_12_triplet_pt4(a,j,k,i) * wm_interm_71_triplet_pt4(p,a,j,k)
term(733) = term(733) + t1(q,i) * wm_interm_12_triplet_pt4(a,j,k,i) * wm_interm_78_triplet_pt4(p,a,j,k)
term(734) = term(734) + t2(a,q,j,i) * wm_interm_21_triplet_pt4(p,i,j,k) * wm_interm_46_triplet_pt4(a,k)
term(735) = term(735) + t2(a,q,j,i) * wm_interm_21_triplet_pt4(p,j,i,k) * wm_interm_46_triplet_pt4(a,k)
term(736) = term(736) + t2(a,q,j,i) * wm_interm_21_triplet_pt4(p,j,i,k) * wm_interm_48_triplet_pt4(a,k)
term(737) = term(737) + t2(a,q,j,i) * wm_interm_21_triplet_pt4(p,i,j,k) * wm_interm_48_triplet_pt4(a,k)
term(738) = term(738) + t1(a,i) * wm_interm_12_triplet_pt4(p,j,k,i) * wm_interm_68_triplet_pt4(a,q,j,k)
term(739) = term(739) + t1(a,i) * wm_interm_12_triplet_pt4(p,j,k,i) * wm_interm_78_triplet_pt4(a,q,j,k)
term(740) = term(740) + t1(a,i) * wm_interm_12_triplet_pt4(p,j,k,i) * wm_interm_69_triplet_pt4(a,q,j,k)
term(741) = term(741) + t2(a,q,j,i) * wm_interm_2_triplet_pt4(a,k) * wm_interm_49_triplet_pt4(p,j,i,k)
term(742) = term(742) + t2(a,q,j,i) * wm_interm_21_triplet_pt4(p,i,j,k) * wm_interm_50_triplet_pt4(a,k)
term(743) = term(743) + t2(a,q,j,i) * wm_interm_21_triplet_pt4(p,j,i,k) * wm_interm_50_triplet_pt4(a,k)
term(744) = term(744) + t1(a,i) * wm_interm_12_triplet_pt4(p,j,k,i) * wm_interm_67_triplet_pt4(a,q,j,k)
term(745) = term(745) + t2(a,q,j,i) * wm_interm_2_triplet_pt4(a,k) * wm_interm_52_triplet_pt4(p,j,i,k)
term(746) = term(746) + t1(a,i) * wm_interm_12_triplet_pt4(p,j,k,i) * wm_interm_71_triplet_pt4(a,q,j,k)
term(747) = term(747) + t1(a,i) * wm_interm_12_triplet_pt4(p,j,k,i) * wm_interm_66_triplet_pt4(a,q,j,k)
term(748) = term(748) + t2(a,q,j,i) * wm_interm_2_triplet_pt4(a,k) * wm_interm_52_triplet_pt4(p,i,j,k)
term(749) = term(749) + t1(q,i) * wm_interm_12_triplet_pt4(a,j,k,i) * wm_interm_157_triplet_pt4(p,a,j,k)
term(750) = term(750) + t1(q,i) * wm_interm_12_triplet_pt4(a,j,k,i) * wm_interm_158_triplet_pt4(p,a,j,k)
term(751) = term(751) + t1(q,i) * wm_interm_12_triplet_pt4(a,j,k,i) * wm_interm_159_triplet_pt4(p,a,j,k)
term(752) = term(752) + t1(q,i) * wm_interm_12_triplet_pt4(a,j,k,i) * wm_interm_161_triplet_pt4(p,a,j,k)
term(753) = term(753) + t2(a,q,j,i) * wm_interm_21_triplet_pt4(p,i,j,k) * wm_interm_58_triplet_pt4(a,k)
term(754) = term(754) + t2(a,q,j,i) * wm_interm_21_triplet_pt4(p,j,i,k) * wm_interm_58_triplet_pt4(a,k)
term(755) = term(755) + t2(a,q,j,i) * wm_interm_21_triplet_pt4(p,j,i,k) * wm_interm_60_triplet_pt4(a,k)
term(756) = term(756) + t2(a,q,j,i) * wm_interm_21_triplet_pt4(p,i,j,k) * wm_interm_60_triplet_pt4(a,k)
term(757) = term(757) + t1(a,i) * wm_interm_12_triplet_pt4(p,j,k,i) * wm_interm_158_triplet_pt4(a,q,j,k)
term(758) = term(758) + t1(a,i) * wm_interm_12_triplet_pt4(p,j,k,i) * wm_interm_161_triplet_pt4(a,q,j,k)
term(759) = term(759) + t1(a,i) * wm_interm_12_triplet_pt4(p,j,k,i) * wm_interm_159_triplet_pt4(a,q,j,k)
term(760) = term(760) + t2(a,q,j,i) * wm_interm_2_triplet_pt4(a,k) * wm_interm_61_triplet_pt4(p,j,i,k)
term(761) = term(761) + t2(a,q,j,i) * wm_interm_2_triplet_pt4(a,k) * wm_interm_63_triplet_pt4(p,j,i,k)
term(762) = term(762) + t1(a,i) * wm_interm_12_triplet_pt4(p,j,k,i) * wm_interm_157_triplet_pt4(a,q,j,k)
term(763) = term(763) + t2(a,q,j,i) * wm_interm_2_triplet_pt4(a,k) * wm_interm_63_triplet_pt4(p,i,j,k)
end do 
end do 
end do 
end do 

term(670) = term(670) * (-2.0d+0) 
term(671) = term(671) * (4.0d+0) 
term(672) = term(672) * (-2.0d+0) 
term(673) = term(673) * (4.0d+0) 
term(674) = term(674) * (-2.0d+0) 
term(675) = term(675) * (-2.0d+0) 
term(676) = term(676) * (-2.0d+0) 
term(677) = term(677) * (-2.0d+0) 
term(678) = term(678) * (4.0d+0) 
term(679) = term(679) * (-2.0d+0) 
term(680) = term(680) * (4.0d+0) 
term(681) = term(681) * (-2.0d+0) 
term(682) = term(682) * (-2.0d+0) 
term(683) = term(683) * (-2.0d+0) 
term(684) = term(684) * (4.0d+0) 
term(685) = term(685) * (-2.0d+0) 
term(686) = term(686) * (-2.0d+0) 
term(687) = term(687) * (4.0d+0) 
term(688) = term(688) * (-8.0d+0) 
term(689) = term(689) * (8.0d+0) 
term(690) = term(690) * (-8.0d+0) 
term(691) = term(691) * (8.0d+0) 
term(692) = term(692) * (-8.0d+0) 
term(693) = term(693) * (-8.0d+0) 
term(694) = term(694) * (16.0d+0) 
term(695) = term(695) * (-8.0d+0) 
term(696) = term(696) * (8.0d+0) 
term(697) = term(697) * (-8.0d+0) 
term(698) = term(698) * (8.0d+0) 
term(699) = term(699) * (-4.0d+0) 
term(700) = term(700) * (-4.0d+0) 
term(701) = term(701) * (8.0d+0) 
term(702) = term(702) * (-2.0d+0) 
term(703) = term(703) * (-2.0d+0) 
term(704) = term(704) * (4.0d+0) 
term(705) = term(705) * (-2.0d+0) 
term(706) = term(706) * (-2.0d+0) 
term(707) = term(707) * (4.0d+0) 
term(708) = term(708) * (-2.0d+0) 
term(709) = term(709) * (-2.0d+0) 
term(710) = term(710) * (4.0d+0) 
term(711) = term(711) * (-2.0d+0) 
term(712) = term(712) * (4.0d+0) 
term(713) = term(713) * (-2.0d+0) 
term(714) = term(714) * (-2.0d+0) 
term(715) = term(715) * (4.0d+0) 
term(716) = term(716) * (-2.0d+0) 
term(717) = term(717) * (-8.0d+0) 
term(718) = term(718) * (8.0d+0) 
term(719) = term(719) * (-8.0d+0) 
term(720) = term(720) * (8.0d+0) 
term(721) = term(721) * (-8.0d+0) 
term(722) = term(722) * (-8.0d+0) 
term(723) = term(723) * (16.0d+0) 
term(724) = term(724) * (-8.0d+0) 
term(725) = term(725) * (8.0d+0) 
term(726) = term(726) * (-8.0d+0) 
term(727) = term(727) * (8.0d+0) 
term(728) = term(728) * (-2.0d+0) 
term(731) = term(731) * (-2.0d+0) 
term(734) = term(734) * (-2.0d+0) 
term(736) = term(736) * (-2.0d+0) 
term(737) = term(737) * (4.0d+0) 
term(740) = term(740) * (-2.0d+0) 
term(742) = term(742) * (-2.0d+0) 
term(745) = term(745) * (-2.0d+0) 
term(747) = term(747) * (-2.0d+0) 
term(749) = term(749) * (-4.0d+0) 
term(750) = term(750) * (4.0d+0) 
term(751) = term(751) * (-4.0d+0) 
term(752) = term(752) * (4.0d+0) 
term(753) = term(753) * (-8.0d+0) 
term(754) = term(754) * (4.0d+0) 
term(755) = term(755) * (-4.0d+0) 
term(756) = term(756) * (8.0d+0) 
term(757) = term(757) * (4.0d+0) 
term(758) = term(758) * (4.0d+0) 
term(759) = term(759) * (-4.0d+0) 
term(760) = term(760) * (2.0d+0) 
term(761) = term(761) * (-4.0d+0) 
term(762) = term(762) * (-4.0d+0) 
term(763) = term(763) * (2.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(764) = term(764) + r1(vrdav_Rl, p,i) * wm_interm_105_triplet_pt4(i,j,k,l) * wm_interm_3_triplet_pt4(q,j,k,l)
term(765) = term(765) + r1(vrdav_Rl, p,i) * wm_interm_105_triplet_pt4(j,i,k,l) * wm_interm_3_triplet_pt4(q,j,l,k)
term(766) = term(766) + r1(vrdav_Rl, p,i) * wm_interm_105_triplet_pt4(i,j,k,l) * wm_interm_3_triplet_pt4(q,j,l,k)
term(767) = term(767) + s1(p,i) * wm_interm_105_triplet_pt4(i,j,k,l) * wm_interm_13_triplet_pt4(q,j,k,l)
term(768) = term(768) + s1(p,i) * wm_interm_105_triplet_pt4(j,i,k,l) * wm_interm_13_triplet_pt4(q,j,k,l)
term(769) = term(769) + s1(p,i) * wm_interm_105_triplet_pt4(j,i,k,l) * wm_interm_13_triplet_pt4(q,j,l,k)
term(770) = term(770) + r1(vrdav_Rl, p,i) * wm_interm_139_triplet_pt4(i,j,k,l) * wm_interm_3_triplet_pt4(q,j,k,l)
term(771) = term(771) + r1(vrdav_Rl, p,i) * wm_interm_139_triplet_pt4(j,i,k,l) * wm_interm_3_triplet_pt4(q,j,l,k)
term(772) = term(772) + r1(vrdav_Rl, p,i) * wm_interm_139_triplet_pt4(i,j,k,l) * wm_interm_3_triplet_pt4(q,j,l,k)
term(773) = term(773) + s1(p,i) * wm_interm_139_triplet_pt4(i,j,k,l) * wm_interm_13_triplet_pt4(q,j,k,l)
term(774) = term(774) + s1(p,i) * wm_interm_139_triplet_pt4(j,i,k,l) * wm_interm_13_triplet_pt4(q,j,k,l)
term(775) = term(775) + s1(p,i) * wm_interm_139_triplet_pt4(j,i,k,l) * wm_interm_13_triplet_pt4(q,j,l,k)
term(776) = term(776) + r1(vrdav_Rr, p,i) * wm_interm_21_triplet_pt4(q,j,k,l) * wm_interm_73_triplet_pt4(i,l,j,k)
term(777) = term(777) + r1(vrdav_Rr, p,i) * wm_interm_49_triplet_pt4(q,j,k,l) * wm_interm_9_triplet_pt4(l,i,j,k)
term(778) = term(778) + r1(vrdav_Rr, p,i) * wm_interm_49_triplet_pt4(q,j,k,l) * wm_interm_9_triplet_pt4(i,l,j,k)
term(779) = term(779) + r1(vrdav_Rr, p,i) * wm_interm_21_triplet_pt4(q,j,k,l) * wm_interm_73_triplet_pt4(l,i,j,k)
term(780) = term(780) + r1(vrdav_Rr, p,i) * wm_interm_52_triplet_pt4(q,j,k,l) * wm_interm_9_triplet_pt4(i,l,j,k)
term(781) = term(781) + t1(q,i) * wm_interm_51_triplet_pt4(p,j,k,l) * wm_interm_9_triplet_pt4(l,i,j,k)
term(782) = term(782) + t1(q,i) * wm_interm_47_triplet_pt4(p,j,k,l) * wm_interm_9_triplet_pt4(i,l,j,k)
term(783) = term(783) + t1(q,i) * wm_interm_47_triplet_pt4(p,j,k,l) * wm_interm_9_triplet_pt4(l,i,j,k)
term(784) = term(784) + t1(q,i) * wm_interm_12_triplet_pt4(p,j,k,l) * wm_interm_73_triplet_pt4(i,j,k,l)
term(785) = term(785) + t1(q,i) * wm_interm_12_triplet_pt4(p,j,k,l) * wm_interm_73_triplet_pt4(j,i,k,l)
term(786) = term(786) + t1(q,i) * wm_interm_12_triplet_pt4(p,j,k,l) * wm_interm_73_triplet_pt4(j,i,l,k)
term(787) = term(787) + r1(vrdav_Rr, p,i) * wm_interm_61_triplet_pt4(q,j,k,l) * wm_interm_9_triplet_pt4(l,i,j,k)
term(788) = term(788) + r1(vrdav_Rr, p,i) * wm_interm_61_triplet_pt4(q,j,k,l) * wm_interm_9_triplet_pt4(i,l,j,k)
term(789) = term(789) + r1(vrdav_Rr, p,i) * wm_interm_63_triplet_pt4(q,j,k,l) * wm_interm_9_triplet_pt4(i,l,j,k)
term(790) = term(790) + t1(q,i) * wm_interm_62_triplet_pt4(p,j,k,l) * wm_interm_9_triplet_pt4(l,i,j,k)
term(791) = term(791) + t1(q,i) * wm_interm_59_triplet_pt4(p,j,k,l) * wm_interm_9_triplet_pt4(i,l,j,k)
term(792) = term(792) + t1(q,i) * wm_interm_59_triplet_pt4(p,j,k,l) * wm_interm_9_triplet_pt4(l,i,j,k)
term(793) = term(793) + t1(q,i) * wm_interm_12_triplet_pt4(p,j,k,l) * wm_interm_163_triplet_pt4(i,j,k,l)
term(794) = term(794) + t1(q,i) * wm_interm_12_triplet_pt4(p,j,k,l) * wm_interm_163_triplet_pt4(j,i,k,l)
term(795) = term(795) + t1(q,i) * wm_interm_12_triplet_pt4(p,j,k,l) * wm_interm_163_triplet_pt4(j,i,l,k)
end do 
end do 
end do 
end do 

term(764) = term(764) * (-2.0d+0) 
term(765) = term(765) * (-2.0d+0) 
term(766) = term(766) * (4.0d+0) 
term(767) = term(767) * (-2.0d+0) 
term(768) = term(768) * (4.0d+0) 
term(769) = term(769) * (-2.0d+0) 
term(770) = term(770) * (-4.0d+0) 
term(771) = term(771) * (-4.0d+0) 
term(772) = term(772) * (8.0d+0) 
term(773) = term(773) * (-4.0d+0) 
term(774) = term(774) * (8.0d+0) 
term(775) = term(775) * (-4.0d+0) 
term(776) = term(776) * (-4.0d+0) 
term(777) = term(777) * (2.0d+0) 
term(778) = term(778) * (-4.0d+0) 
term(779) = term(779) * (2.0d+0) 
term(780) = term(780) * (2.0d+0) 
term(783) = term(783) * (-2.0d+0) 
term(785) = term(785) * (-2.0d+0) 
term(787) = term(787) * (4.0d+0) 
term(788) = term(788) * (-8.0d+0) 
term(789) = term(789) * (4.0d+0) 
term(790) = term(790) * (2.0d+0) 
term(791) = term(791) * (2.0d+0) 
term(792) = term(792) * (-4.0d+0) 
term(793) = term(793) * (2.0d+0) 
term(794) = term(794) * (-4.0d+0) 
term(795) = term(795) * (2.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(796) = term(796) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_18_triplet_pt4(a,b) * wm_interm_32_triplet_pt4(b,q,j,i)
term(797) = term(797) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_19_triplet_pt4(a,b) * wm_interm_32_triplet_pt4(b,q,j,i)
term(798) = term(798) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_18_triplet_pt4(a,b) * wm_interm_32_triplet_pt4(b,q,i,j)
term(799) = term(799) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_19_triplet_pt4(a,b) * wm_interm_32_triplet_pt4(b,q,i,j)
term(800) = term(800) + r2m(vrdav_Rr, a,j,p,i) * wm_interm_18_triplet_pt4(a,b) * wm_interm_32_triplet_pt4(b,q,i,j)
term(801) = term(801) + r2m(vrdav_Rr, a,j,p,i) * wm_interm_19_triplet_pt4(a,b) * wm_interm_32_triplet_pt4(b,q,i,j)
term(802) = term(802) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_18_triplet_pt4(a,b) * wm_interm_32_triplet_pt4(b,q,j,i)
term(803) = term(803) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_19_triplet_pt4(a,b) * wm_interm_32_triplet_pt4(b,q,j,i)
term(804) = term(804) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_18_triplet_pt4(a,b) * wm_interm_32_triplet_pt4(b,q,i,j)
term(805) = term(805) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_19_triplet_pt4(a,b) * wm_interm_32_triplet_pt4(b,q,i,j)
term(806) = term(806) + r2p(vrdav_Rl, a,i,q,j) * wm_interm_18_triplet_pt4(b,a) * wm_interm_56_triplet_pt4(p,b,i,j)
term(807) = term(807) + r2p(vrdav_Rl, a,i,q,j) * wm_interm_19_triplet_pt4(b,a) * wm_interm_56_triplet_pt4(p,b,i,j)
term(808) = term(808) + r2m(vrdav_Rl, a,j,q,i) * wm_interm_18_triplet_pt4(b,a) * wm_interm_56_triplet_pt4(p,b,i,j)
term(809) = term(809) + r2m(vrdav_Rl, a,j,q,i) * wm_interm_19_triplet_pt4(b,a) * wm_interm_56_triplet_pt4(p,b,i,j)
term(810) = term(810) + r2m(vrdav_Rl, a,i,q,j) * wm_interm_18_triplet_pt4(b,a) * wm_interm_56_triplet_pt4(p,b,i,j)
term(811) = term(811) + r2m(vrdav_Rl, a,i,q,j) * wm_interm_19_triplet_pt4(b,a) * wm_interm_56_triplet_pt4(p,b,i,j)
end do 
end do 
end do 
end do 

term(797) = term(797) * (-2.0d+0) 
term(798) = term(798) * (-2.0d+0) 
term(799) = term(799) * (4.0d+0) 
term(800) = term(800) * (2.0d+0) 
term(801) = term(801) * (-4.0d+0) 
term(802) = term(802) * (2.0d+0) 
term(803) = term(803) * (-4.0d+0) 
term(804) = term(804) * (-4.0d+0) 
term(805) = term(805) * (8.0d+0) 
term(806) = term(806) * (2.0d+0) 
term(807) = term(807) * (-4.0d+0) 
term(808) = term(808) * (-8.0d+0) 
term(809) = term(809) * (16.0d+0) 
term(810) = term(810) * (4.0d+0) 
term(811) = term(811) * (-8.0d+0) 

term(812) = term(812) + wm_interm_152_triplet_pt4 * wm_interm_45_triplet_pt4(p,q)
term(813) = term(813) + wm_interm_153_triplet_pt4 * wm_interm_45_triplet_pt4(p,q)
term(814) = term(814) + wm_interm_154_triplet_pt4 * wm_interm_45_triplet_pt4(p,q)
term(815) = term(815) + wm_interm_155_triplet_pt4 * wm_interm_45_triplet_pt4(p,q)
term(816) = term(816) + wm_interm_156_triplet_pt4 * wm_interm_45_triplet_pt4(p,q)
term(817) = term(817) + wm_interm_201_triplet_pt4 * wm_interm_45_triplet_pt4(p,q)
term(818) = term(818) + wm_interm_202_triplet_pt4 * wm_interm_45_triplet_pt4(p,q)
term(819) = term(819) + wm_interm_203_triplet_pt4 * wm_interm_45_triplet_pt4(p,q)
term(820) = term(820) + wm_interm_204_triplet_pt4 * wm_interm_45_triplet_pt4(p,q)
term(821) = term(821) + wm_interm_205_triplet_pt4 * wm_interm_45_triplet_pt4(p,q)
term(822) = term(822) + wm_interm_23_triplet_pt4 * wm_interm_45_triplet_pt4(p,q)

term(812) = term(812) * (-1.0d+0) 
term(813) = term(813) * (2.0d+0) 
term(814) = term(814) * (-1.0d+0) 
term(815) = term(815) * (-4.0d+0) 
term(816) = term(816) * (4.0d+0) 
term(817) = term(817) * (-2.0d+0) 
term(818) = term(818) * (4.0d+0) 
term(819) = term(819) * (-2.0d+0) 
term(820) = term(820) * (-8.0d+0) 
term(821) = term(821) * (8.0d+0) 
term(822) = term(822) * (4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(823) = term(823) + s1(a,i) * wm_interm_24_triplet_pt4(p,j,k,i) * wm_interm_5_triplet_pt4(q,a,k,j)
term(824) = term(824) + s1(a,i) * wm_interm_26_triplet_pt4(p,j,k,i) * wm_interm_5_triplet_pt4(q,a,k,j)
term(825) = term(825) + s2(a,q,j,i) * wm_interm_26_triplet_pt4(p,k,i,j) * wm_interm_6_triplet_pt4(a,k)
term(826) = term(826) + s2(a,q,j,i) * wm_interm_26_triplet_pt4(p,k,i,j) * wm_interm_7_triplet_pt4(a,k)
term(827) = term(827) + s1(a,i) * wm_interm_29_triplet_pt4(p,j,k,i) * wm_interm_5_triplet_pt4(q,a,k,j)
term(828) = term(828) + s2(a,q,j,i) * wm_interm_29_triplet_pt4(p,k,i,j) * wm_interm_6_triplet_pt4(a,k)
term(829) = term(829) + s2(a,q,j,i) * wm_interm_29_triplet_pt4(p,k,i,j) * wm_interm_7_triplet_pt4(a,k)
term(830) = term(830) + s2(a,p,j,i) * wm_interm_13_triplet_pt4(q,k,i,j) * wm_interm_33_triplet_pt4(a,k)
term(831) = term(831) + s2(a,p,j,i) * wm_interm_13_triplet_pt4(q,k,i,j) * wm_interm_36_triplet_pt4(a,k)
term(832) = term(832) + s2(a,p,j,i) * wm_interm_13_triplet_pt4(q,k,i,j) * wm_interm_38_triplet_pt4(a,k)
term(833) = term(833) + s2(a,p,j,i) * wm_interm_13_triplet_pt4(q,k,i,j) * wm_interm_40_triplet_pt4(a,k)
term(834) = term(834) + s2(a,p,j,i) * wm_interm_13_triplet_pt4(q,k,i,j) * wm_interm_42_triplet_pt4(a,k)
term(835) = term(835) + s2(a,p,j,i) * wm_interm_39_triplet_pt4(a,k) * wm_interm_3_triplet_pt4(q,k,i,j)
term(836) = term(836) + s2(a,p,j,i) * wm_interm_37_triplet_pt4(a,k) * wm_interm_3_triplet_pt4(q,k,i,j)
term(837) = term(837) + s2(a,p,j,i) * wm_interm_34_triplet_pt4(a,k) * wm_interm_3_triplet_pt4(q,k,i,j)
term(838) = term(838) + s1(p,i) * wm_interm_24_triplet_pt4(a,j,k,i) * wm_interm_5_triplet_pt4(a,q,k,j)
term(839) = term(839) + s1(p,i) * wm_interm_26_triplet_pt4(a,j,k,i) * wm_interm_5_triplet_pt4(a,q,k,j)
term(840) = term(840) + s2(a,p,j,i) * wm_interm_3_triplet_pt4(q,k,i,j) * wm_interm_43_triplet_pt4(a,k)
term(841) = term(841) + s2(a,p,j,i) * wm_interm_3_triplet_pt4(q,k,i,j) * wm_interm_41_triplet_pt4(a,k)
term(842) = term(842) + s1(p,i) * wm_interm_29_triplet_pt4(a,j,k,i) * wm_interm_5_triplet_pt4(a,q,k,j)
term(843) = term(843) + t2(a,q,j,i) * wm_interm_12_triplet_pt4(p,k,i,j) * wm_interm_53_triplet_pt4(a,k)
term(844) = term(844) + t2(a,q,j,i) * wm_interm_12_triplet_pt4(p,k,i,j) * wm_interm_54_triplet_pt4(a,k)
term(845) = term(845) + t2(a,q,j,i) * wm_interm_12_triplet_pt4(p,k,i,j) * wm_interm_55_triplet_pt4(a,k)
term(846) = term(846) + t2(a,q,j,i) * wm_interm_12_triplet_pt4(p,k,i,j) * wm_interm_64_triplet_pt4(a,k)
term(847) = term(847) + t2(a,q,j,i) * wm_interm_12_triplet_pt4(p,k,i,j) * wm_interm_65_triplet_pt4(a,k)
end do 
end do 
end do 
end do 

term(823) = term(823) * (-2.0d+0) 
term(824) = term(824) * (4.0d+0) 
term(825) = term(825) * (4.0d+0) 
term(826) = term(826) * (-2.0d+0) 
term(827) = term(827) * (8.0d+0) 
term(828) = term(828) * (16.0d+0) 
term(829) = term(829) * (-8.0d+0) 
term(830) = term(830) * (2.0d+0) 
term(831) = term(831) * (-1.0d+0) 
term(832) = term(832) * (-1.0d+0) 
term(833) = term(833) * (4.0d+0) 
term(834) = term(834) * (-4.0d+0) 
term(835) = term(835) * (-2.0d+0) 
term(836) = term(836) * (-2.0d+0) 
term(837) = term(837) * (4.0d+0) 
term(838) = term(838) * (-2.0d+0) 
term(839) = term(839) * (4.0d+0) 
term(840) = term(840) * (-8.0d+0) 
term(841) = term(841) * (8.0d+0) 
term(842) = term(842) * (8.0d+0) 
term(843) = term(843) * (-2.0d+0) 
term(846) = term(846) * (-4.0d+0) 
term(847) = term(847) * (4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(848) = term(848) + s1(a,i) * wm_interm_1_triplet_pt4(q,a,j,k) * wm_interm_24_triplet_pt4(p,k,i,j)
term(849) = term(849) + s1(a,i) * wm_interm_1_triplet_pt4(q,a,j,k) * wm_interm_26_triplet_pt4(p,k,i,j)
term(850) = term(850) + s1(a,i) * wm_interm_26_triplet_pt4(p,j,i,k) * wm_interm_5_triplet_pt4(q,a,k,j)
term(851) = term(851) + r1(vrdav_Rl, q,i) * wm_interm_3_triplet_pt4(a,j,i,k) * wm_interm_87_triplet_pt4(p,a,j,k)
term(852) = term(852) + r1(vrdav_Rl, q,i) * wm_interm_3_triplet_pt4(a,j,i,k) * wm_interm_86_triplet_pt4(p,a,j,k)
term(853) = term(853) + r1(vrdav_Rl, q,i) * wm_interm_3_triplet_pt4(a,j,i,k) * wm_interm_84_triplet_pt4(p,a,j,k)
term(854) = term(854) + r1(vrdav_Rl, q,i) * wm_interm_3_triplet_pt4(a,j,i,k) * wm_interm_85_triplet_pt4(p,a,j,k)
term(855) = term(855) + s1(a,i) * wm_interm_10_triplet_pt4(q,a,j,k) * wm_interm_24_triplet_pt4(p,k,i,j)
term(856) = term(856) + s1(a,i) * wm_interm_11_triplet_pt4(q,a,j,k) * wm_interm_24_triplet_pt4(p,k,i,j)
term(857) = term(857) + s1(a,i) * wm_interm_10_triplet_pt4(q,a,j,k) * wm_interm_26_triplet_pt4(p,k,i,j)
term(858) = term(858) + s1(a,i) * wm_interm_11_triplet_pt4(q,a,j,k) * wm_interm_26_triplet_pt4(p,k,i,j)
term(859) = term(859) + r1(vrdav_Rl, q,i) * wm_interm_101_triplet_pt4(p,a,j,k) * wm_interm_3_triplet_pt4(a,j,i,k)
term(860) = term(860) + r1(vrdav_Rl, q,i) * wm_interm_3_triplet_pt4(a,j,i,k) * wm_interm_91_triplet_pt4(p,a,j,k)
term(861) = term(861) + r1(vrdav_Rl, a,i) * wm_interm_27_triplet_pt4(p,j,i,k) * wm_interm_5_triplet_pt4(q,a,j,k)
term(862) = term(862) + r1(vrdav_Rl, a,i) * wm_interm_1_triplet_pt4(q,a,j,k) * wm_interm_28_triplet_pt4(p,j,i,k)
term(863) = term(863) + s1(q,i) * wm_interm_13_triplet_pt4(a,j,i,k) * wm_interm_87_triplet_pt4(p,a,j,k)
term(864) = term(864) + s1(q,i) * wm_interm_13_triplet_pt4(a,j,i,k) * wm_interm_86_triplet_pt4(p,a,j,k)
term(865) = term(865) + s1(q,i) * wm_interm_13_triplet_pt4(a,j,i,k) * wm_interm_84_triplet_pt4(p,a,j,k)
term(866) = term(866) + s1(q,i) * wm_interm_13_triplet_pt4(a,j,i,k) * wm_interm_85_triplet_pt4(p,a,j,k)
term(867) = term(867) + r1(vrdav_Rl, a,i) * wm_interm_10_triplet_pt4(q,a,j,k) * wm_interm_28_triplet_pt4(p,j,i,k)
term(868) = term(868) + r1(vrdav_Rl, a,i) * wm_interm_11_triplet_pt4(q,a,j,k) * wm_interm_28_triplet_pt4(p,j,i,k)
term(869) = term(869) + s1(q,i) * wm_interm_101_triplet_pt4(p,a,j,k) * wm_interm_13_triplet_pt4(a,j,i,k)
term(870) = term(870) + s1(q,i) * wm_interm_13_triplet_pt4(a,j,i,k) * wm_interm_91_triplet_pt4(p,a,j,k)
term(871) = term(871) + s1(a,i) * wm_interm_29_triplet_pt4(p,j,i,k) * wm_interm_5_triplet_pt4(q,a,k,j)
term(872) = term(872) + s1(a,i) * wm_interm_1_triplet_pt4(q,a,j,k) * wm_interm_29_triplet_pt4(p,k,i,j)
term(873) = term(873) + r1(vrdav_Rl, q,i) * wm_interm_128_triplet_pt4(p,a,j,k) * wm_interm_3_triplet_pt4(a,j,i,k)
term(874) = term(874) + r1(vrdav_Rl, q,i) * wm_interm_127_triplet_pt4(p,a,j,k) * wm_interm_3_triplet_pt4(a,j,i,k)
term(875) = term(875) + r1(vrdav_Rl, q,i) * wm_interm_125_triplet_pt4(p,a,j,k) * wm_interm_3_triplet_pt4(a,j,i,k)
term(876) = term(876) + r1(vrdav_Rl, q,i) * wm_interm_126_triplet_pt4(p,a,j,k) * wm_interm_3_triplet_pt4(a,j,i,k)
term(877) = term(877) + s1(a,i) * wm_interm_10_triplet_pt4(q,a,j,k) * wm_interm_29_triplet_pt4(p,k,i,j)
term(878) = term(878) + s1(a,i) * wm_interm_11_triplet_pt4(q,a,j,k) * wm_interm_29_triplet_pt4(p,k,i,j)
term(879) = term(879) + r1(vrdav_Rl, a,i) * wm_interm_30_triplet_pt4(p,j,i,k) * wm_interm_5_triplet_pt4(q,a,j,k)
term(880) = term(880) + r1(vrdav_Rl, a,i) * wm_interm_1_triplet_pt4(q,a,j,k) * wm_interm_31_triplet_pt4(p,j,i,k)
term(881) = term(881) + s1(q,i) * wm_interm_128_triplet_pt4(p,a,j,k) * wm_interm_13_triplet_pt4(a,j,i,k)
term(882) = term(882) + s1(q,i) * wm_interm_127_triplet_pt4(p,a,j,k) * wm_interm_13_triplet_pt4(a,j,i,k)
term(883) = term(883) + s1(q,i) * wm_interm_125_triplet_pt4(p,a,j,k) * wm_interm_13_triplet_pt4(a,j,i,k)
term(884) = term(884) + s1(q,i) * wm_interm_126_triplet_pt4(p,a,j,k) * wm_interm_13_triplet_pt4(a,j,i,k)
term(885) = term(885) + r1(vrdav_Rl, a,i) * wm_interm_10_triplet_pt4(q,a,j,k) * wm_interm_31_triplet_pt4(p,j,i,k)
term(886) = term(886) + r1(vrdav_Rl, a,i) * wm_interm_11_triplet_pt4(q,a,j,k) * wm_interm_31_triplet_pt4(p,j,i,k)
term(887) = term(887) + r1(vrdav_Rl, a,i) * wm_interm_101_triplet_pt4(a,p,j,k) * wm_interm_3_triplet_pt4(q,j,i,k)
term(888) = term(888) + r1(vrdav_Rl, a,i) * wm_interm_3_triplet_pt4(q,j,i,k) * wm_interm_87_triplet_pt4(a,p,j,k)
term(889) = term(889) + r1(vrdav_Rl, a,i) * wm_interm_3_triplet_pt4(q,j,i,k) * wm_interm_86_triplet_pt4(a,p,j,k)
term(890) = term(890) + r1(vrdav_Rl, a,i) * wm_interm_3_triplet_pt4(q,j,i,k) * wm_interm_91_triplet_pt4(a,p,j,k)
term(891) = term(891) + r1(vrdav_Rl, a,i) * wm_interm_3_triplet_pt4(q,j,i,k) * wm_interm_84_triplet_pt4(a,p,j,k)
term(892) = term(892) + r1(vrdav_Rl, a,i) * wm_interm_3_triplet_pt4(q,j,i,k) * wm_interm_85_triplet_pt4(a,p,j,k)
term(893) = term(893) + r1(vrdav_Rl, p,i) * wm_interm_1_triplet_pt4(a,q,j,k) * wm_interm_28_triplet_pt4(a,j,i,k)
term(894) = term(894) + r1(vrdav_Rl, p,i) * wm_interm_1_triplet_pt4(a,q,j,k) * wm_interm_28_triplet_pt4(a,i,j,k)
term(895) = term(895) + r1(vrdav_Rl, p,i) * wm_interm_1_triplet_pt4(a,q,j,k) * wm_interm_27_triplet_pt4(a,i,j,k)
term(896) = term(896) + r1(vrdav_Rl, p,i) * wm_interm_28_triplet_pt4(a,i,j,k) * wm_interm_5_triplet_pt4(a,q,j,k)
term(897) = term(897) + r1(vrdav_Rl, p,i) * wm_interm_27_triplet_pt4(a,j,i,k) * wm_interm_5_triplet_pt4(a,q,j,k)
term(898) = term(898) + r1(vrdav_Rl, p,i) * wm_interm_27_triplet_pt4(a,i,j,k) * wm_interm_5_triplet_pt4(a,q,j,k)
term(899) = term(899) + s1(a,i) * wm_interm_13_triplet_pt4(q,j,i,k) * wm_interm_87_triplet_pt4(a,p,j,k)
term(900) = term(900) + s1(a,i) * wm_interm_13_triplet_pt4(q,j,i,k) * wm_interm_86_triplet_pt4(a,p,j,k)
term(901) = term(901) + s1(a,i) * wm_interm_13_triplet_pt4(q,j,i,k) * wm_interm_84_triplet_pt4(a,p,j,k)
term(902) = term(902) + s1(a,i) * wm_interm_13_triplet_pt4(q,j,i,k) * wm_interm_85_triplet_pt4(a,p,j,k)
term(903) = term(903) + s1(a,i) * wm_interm_101_triplet_pt4(a,p,j,k) * wm_interm_13_triplet_pt4(q,j,i,k)
term(904) = term(904) + s1(a,i) * wm_interm_13_triplet_pt4(q,j,i,k) * wm_interm_91_triplet_pt4(a,p,j,k)
term(905) = term(905) + s1(p,i) * wm_interm_10_triplet_pt4(a,q,j,k) * wm_interm_24_triplet_pt4(a,k,i,j)
term(906) = term(906) + s1(p,i) * wm_interm_10_triplet_pt4(a,q,j,k) * wm_interm_26_triplet_pt4(a,k,i,j)
term(907) = term(907) + s1(p,i) * wm_interm_26_triplet_pt4(a,j,i,k) * wm_interm_5_triplet_pt4(a,q,k,j)
term(908) = term(908) + s1(p,i) * wm_interm_1_triplet_pt4(a,q,j,k) * wm_interm_24_triplet_pt4(a,k,i,j)
term(909) = term(909) + s1(p,i) * wm_interm_11_triplet_pt4(a,q,j,k) * wm_interm_24_triplet_pt4(a,k,i,j)
term(910) = term(910) + s1(p,i) * wm_interm_1_triplet_pt4(a,q,j,k) * wm_interm_26_triplet_pt4(a,k,i,j)
term(911) = term(911) + s1(p,i) * wm_interm_11_triplet_pt4(a,q,j,k) * wm_interm_26_triplet_pt4(a,k,i,j)
term(912) = term(912) + r1(vrdav_Rl, p,i) * wm_interm_10_triplet_pt4(a,q,j,k) * wm_interm_28_triplet_pt4(a,j,i,k)
term(913) = term(913) + r1(vrdav_Rl, p,i) * wm_interm_10_triplet_pt4(a,q,j,k) * wm_interm_28_triplet_pt4(a,i,j,k)
term(914) = term(914) + r1(vrdav_Rl, p,i) * wm_interm_11_triplet_pt4(a,q,j,k) * wm_interm_28_triplet_pt4(a,j,i,k)
term(915) = term(915) + r1(vrdav_Rl, p,i) * wm_interm_11_triplet_pt4(a,q,j,k) * wm_interm_28_triplet_pt4(a,i,j,k)
term(916) = term(916) + r1(vrdav_Rl, p,i) * wm_interm_10_triplet_pt4(a,q,j,k) * wm_interm_27_triplet_pt4(a,i,j,k)
term(917) = term(917) + r1(vrdav_Rl, p,i) * wm_interm_11_triplet_pt4(a,q,j,k) * wm_interm_27_triplet_pt4(a,i,j,k)
term(918) = term(918) + r1(vrdav_Rl, a,i) * wm_interm_128_triplet_pt4(a,p,j,k) * wm_interm_3_triplet_pt4(q,j,i,k)
term(919) = term(919) + r1(vrdav_Rl, a,i) * wm_interm_127_triplet_pt4(a,p,j,k) * wm_interm_3_triplet_pt4(q,j,i,k)
term(920) = term(920) + r1(vrdav_Rl, a,i) * wm_interm_125_triplet_pt4(a,p,j,k) * wm_interm_3_triplet_pt4(q,j,i,k)
term(921) = term(921) + r1(vrdav_Rl, a,i) * wm_interm_126_triplet_pt4(a,p,j,k) * wm_interm_3_triplet_pt4(q,j,i,k)
term(922) = term(922) + r1(vrdav_Rl, p,i) * wm_interm_1_triplet_pt4(a,q,j,k) * wm_interm_31_triplet_pt4(a,j,i,k)
term(923) = term(923) + r1(vrdav_Rl, p,i) * wm_interm_1_triplet_pt4(a,q,j,k) * wm_interm_31_triplet_pt4(a,i,j,k)
term(924) = term(924) + r1(vrdav_Rl, p,i) * wm_interm_1_triplet_pt4(a,q,j,k) * wm_interm_30_triplet_pt4(a,i,j,k)
term(925) = term(925) + r1(vrdav_Rl, p,i) * wm_interm_31_triplet_pt4(a,i,j,k) * wm_interm_5_triplet_pt4(a,q,j,k)
term(926) = term(926) + r1(vrdav_Rl, p,i) * wm_interm_30_triplet_pt4(a,j,i,k) * wm_interm_5_triplet_pt4(a,q,j,k)
term(927) = term(927) + r1(vrdav_Rl, p,i) * wm_interm_30_triplet_pt4(a,i,j,k) * wm_interm_5_triplet_pt4(a,q,j,k)
term(928) = term(928) + s1(a,i) * wm_interm_128_triplet_pt4(a,p,j,k) * wm_interm_13_triplet_pt4(q,j,i,k)
term(929) = term(929) + s1(a,i) * wm_interm_127_triplet_pt4(a,p,j,k) * wm_interm_13_triplet_pt4(q,j,i,k)
term(930) = term(930) + s1(a,i) * wm_interm_125_triplet_pt4(a,p,j,k) * wm_interm_13_triplet_pt4(q,j,i,k)
term(931) = term(931) + s1(a,i) * wm_interm_126_triplet_pt4(a,p,j,k) * wm_interm_13_triplet_pt4(q,j,i,k)
term(932) = term(932) + s1(p,i) * wm_interm_10_triplet_pt4(a,q,j,k) * wm_interm_29_triplet_pt4(a,k,i,j)
term(933) = term(933) + s1(p,i) * wm_interm_29_triplet_pt4(a,j,i,k) * wm_interm_5_triplet_pt4(a,q,k,j)
term(934) = term(934) + s1(p,i) * wm_interm_1_triplet_pt4(a,q,j,k) * wm_interm_29_triplet_pt4(a,k,i,j)
term(935) = term(935) + s1(p,i) * wm_interm_11_triplet_pt4(a,q,j,k) * wm_interm_29_triplet_pt4(a,k,i,j)
term(936) = term(936) + r1(vrdav_Rl, p,i) * wm_interm_10_triplet_pt4(a,q,j,k) * wm_interm_31_triplet_pt4(a,j,i,k)
term(937) = term(937) + r1(vrdav_Rl, p,i) * wm_interm_10_triplet_pt4(a,q,j,k) * wm_interm_31_triplet_pt4(a,i,j,k)
term(938) = term(938) + r1(vrdav_Rl, p,i) * wm_interm_11_triplet_pt4(a,q,j,k) * wm_interm_31_triplet_pt4(a,j,i,k)
term(939) = term(939) + r1(vrdav_Rl, p,i) * wm_interm_11_triplet_pt4(a,q,j,k) * wm_interm_31_triplet_pt4(a,i,j,k)
term(940) = term(940) + r1(vrdav_Rl, p,i) * wm_interm_10_triplet_pt4(a,q,j,k) * wm_interm_30_triplet_pt4(a,i,j,k)
term(941) = term(941) + r1(vrdav_Rl, p,i) * wm_interm_11_triplet_pt4(a,q,j,k) * wm_interm_30_triplet_pt4(a,i,j,k)
term(942) = term(942) + r1(vrdav_Rr, p,i) * wm_interm_151_triplet_pt4(a,j,i,k) * wm_interm_67_triplet_pt4(q,a,k,j)
term(943) = term(943) + r1(vrdav_Rr, p,i) * wm_interm_21_triplet_pt4(a,j,i,k) * wm_interm_74_triplet_pt4(q,a,k,j)
term(944) = term(944) + r1(vrdav_Rr, p,i) * wm_interm_49_triplet_pt4(a,j,i,k) * wm_interm_5_triplet_pt4(q,a,k,j)
term(945) = term(945) + r1(vrdav_Rr, p,i) * wm_interm_151_triplet_pt4(a,j,i,k) * wm_interm_66_triplet_pt4(q,a,k,j)
term(946) = term(946) + r1(vrdav_Rr, p,i) * wm_interm_151_triplet_pt4(a,j,i,k) * wm_interm_68_triplet_pt4(q,a,k,j)
term(947) = term(947) + r1(vrdav_Rr, p,i) * wm_interm_151_triplet_pt4(a,j,i,k) * wm_interm_71_triplet_pt4(q,a,k,j)
term(948) = term(948) + r1(vrdav_Rr, p,i) * wm_interm_10_triplet_pt4(q,a,j,k) * wm_interm_52_triplet_pt4(a,k,i,j)
term(949) = term(949) + r1(vrdav_Rr, p,i) * wm_interm_11_triplet_pt4(q,a,j,k) * wm_interm_52_triplet_pt4(a,k,i,j)
term(950) = term(950) + r1(vrdav_Rr, p,i) * wm_interm_1_triplet_pt4(q,a,j,k) * wm_interm_52_triplet_pt4(a,k,i,j)
term(951) = term(951) + t1(q,i) * wm_interm_12_triplet_pt4(a,j,i,k) * wm_interm_66_triplet_pt4(p,a,j,k)
term(952) = term(952) + t1(q,i) * wm_interm_12_triplet_pt4(a,j,i,k) * wm_interm_67_triplet_pt4(p,a,j,k)
term(953) = term(953) + t1(q,i) * wm_interm_10_triplet_pt4(p,a,j,k) * wm_interm_47_triplet_pt4(a,k,i,j)
term(954) = term(954) + t1(q,i) * wm_interm_11_triplet_pt4(p,a,j,k) * wm_interm_47_triplet_pt4(a,k,i,j)
term(955) = term(955) + t1(q,i) * wm_interm_12_triplet_pt4(a,j,i,k) * wm_interm_68_triplet_pt4(p,a,j,k)
term(956) = term(956) + t1(q,i) * wm_interm_12_triplet_pt4(a,j,i,k) * wm_interm_69_triplet_pt4(p,a,j,k)
term(957) = term(957) + t1(q,i) * wm_interm_12_triplet_pt4(a,j,i,k) * wm_interm_71_triplet_pt4(p,a,j,k)
term(958) = term(958) + t1(q,i) * wm_interm_12_triplet_pt4(a,j,i,k) * wm_interm_74_triplet_pt4(p,a,j,k)
term(959) = term(959) + t1(q,i) * wm_interm_47_triplet_pt4(a,j,i,k) * wm_interm_5_triplet_pt4(p,a,k,j)
term(960) = term(960) + t1(q,i) * wm_interm_1_triplet_pt4(p,a,j,k) * wm_interm_47_triplet_pt4(a,k,i,j)
term(961) = term(961) + t1(q,i) * wm_interm_51_triplet_pt4(a,j,i,k) * wm_interm_5_triplet_pt4(p,a,k,j)
term(962) = term(962) + r1(vrdav_Rr, a,i) * wm_interm_21_triplet_pt4(p,i,j,k) * wm_interm_78_triplet_pt4(a,q,k,j)
term(963) = term(963) + t1(a,i) * wm_interm_51_triplet_pt4(p,j,i,k) * wm_interm_5_triplet_pt4(a,q,k,j)
term(964) = term(964) + t1(a,i) * wm_interm_1_triplet_pt4(a,q,j,k) * wm_interm_47_triplet_pt4(p,k,i,j)
term(965) = term(965) + t1(a,i) * wm_interm_47_triplet_pt4(p,j,i,k) * wm_interm_5_triplet_pt4(a,q,k,j)
term(966) = term(966) + r1(vrdav_Rr, a,i) * wm_interm_21_triplet_pt4(p,i,j,k) * wm_interm_67_triplet_pt4(a,q,k,j)
term(967) = term(967) + r1(vrdav_Rr, a,i) * wm_interm_21_triplet_pt4(p,j,i,k) * wm_interm_67_triplet_pt4(a,q,k,j)
term(968) = term(968) + r1(vrdav_Rr, a,i) * wm_interm_21_triplet_pt4(p,j,i,k) * wm_interm_74_triplet_pt4(a,q,k,j)
term(969) = term(969) + r1(vrdav_Rr, a,i) * wm_interm_21_triplet_pt4(p,i,j,k) * wm_interm_74_triplet_pt4(a,q,k,j)
term(970) = term(970) + t1(a,i) * wm_interm_10_triplet_pt4(a,q,j,k) * wm_interm_47_triplet_pt4(p,k,i,j)
term(971) = term(971) + t1(a,i) * wm_interm_11_triplet_pt4(a,q,j,k) * wm_interm_47_triplet_pt4(p,k,i,j)
term(972) = term(972) + t1(a,i) * wm_interm_12_triplet_pt4(p,j,i,k) * wm_interm_68_triplet_pt4(a,q,j,k)
term(973) = term(973) + t1(a,i) * wm_interm_12_triplet_pt4(p,j,i,k) * wm_interm_69_triplet_pt4(a,q,j,k)
term(974) = term(974) + r1(vrdav_Rr, a,i) * wm_interm_49_triplet_pt4(p,j,i,k) * wm_interm_5_triplet_pt4(a,q,k,j)
term(975) = term(975) + r1(vrdav_Rr, a,i) * wm_interm_10_triplet_pt4(a,q,j,k) * wm_interm_49_triplet_pt4(p,i,k,j)
term(976) = term(976) + r1(vrdav_Rr, a,i) * wm_interm_49_triplet_pt4(p,i,j,k) * wm_interm_5_triplet_pt4(a,q,k,j)
term(977) = term(977) + r1(vrdav_Rr, a,i) * wm_interm_1_triplet_pt4(a,q,j,k) * wm_interm_49_triplet_pt4(p,i,k,j)
term(978) = term(978) + r1(vrdav_Rr, a,i) * wm_interm_11_triplet_pt4(a,q,j,k) * wm_interm_49_triplet_pt4(p,i,k,j)
term(979) = term(979) + r1(vrdav_Rr, a,i) * wm_interm_21_triplet_pt4(p,i,j,k) * wm_interm_68_triplet_pt4(a,q,k,j)
term(980) = term(980) + r1(vrdav_Rr, a,i) * wm_interm_21_triplet_pt4(p,j,i,k) * wm_interm_68_triplet_pt4(a,q,k,j)
term(981) = term(981) + r1(vrdav_Rr, a,i) * wm_interm_21_triplet_pt4(p,j,i,k) * wm_interm_69_triplet_pt4(a,q,k,j)
term(982) = term(982) + t1(a,i) * wm_interm_10_triplet_pt4(a,q,j,k) * wm_interm_51_triplet_pt4(p,i,k,j)
term(983) = term(983) + t1(a,i) * wm_interm_10_triplet_pt4(a,q,j,k) * wm_interm_47_triplet_pt4(p,i,k,j)
term(984) = term(984) + r1(vrdav_Rr, a,i) * wm_interm_21_triplet_pt4(p,i,j,k) * wm_interm_71_triplet_pt4(a,q,k,j)
term(985) = term(985) + t1(a,i) * wm_interm_1_triplet_pt4(a,q,j,k) * wm_interm_51_triplet_pt4(p,i,k,j)
term(986) = term(986) + t1(a,i) * wm_interm_11_triplet_pt4(a,q,j,k) * wm_interm_51_triplet_pt4(p,i,k,j)
term(987) = term(987) + r1(vrdav_Rr, a,i) * wm_interm_21_triplet_pt4(p,i,j,k) * wm_interm_66_triplet_pt4(a,q,k,j)
term(988) = term(988) + t1(a,i) * wm_interm_47_triplet_pt4(p,i,j,k) * wm_interm_5_triplet_pt4(a,q,k,j)
term(989) = term(989) + r1(vrdav_Rr, a,i) * wm_interm_21_triplet_pt4(p,j,i,k) * wm_interm_71_triplet_pt4(a,q,k,j)
term(990) = term(990) + r1(vrdav_Rr, a,i) * wm_interm_21_triplet_pt4(p,j,i,k) * wm_interm_66_triplet_pt4(a,q,k,j)
term(991) = term(991) + t1(a,i) * wm_interm_1_triplet_pt4(a,q,j,k) * wm_interm_47_triplet_pt4(p,i,k,j)
term(992) = term(992) + t1(a,i) * wm_interm_11_triplet_pt4(a,q,j,k) * wm_interm_47_triplet_pt4(p,i,k,j)
term(993) = term(993) + r1(vrdav_Rr, a,i) * wm_interm_52_triplet_pt4(p,i,j,k) * wm_interm_5_triplet_pt4(a,q,k,j)
term(994) = term(994) + r1(vrdav_Rr, a,i) * wm_interm_10_triplet_pt4(a,q,j,k) * wm_interm_52_triplet_pt4(p,k,i,j)
term(995) = term(995) + r1(vrdav_Rr, a,i) * wm_interm_10_triplet_pt4(a,q,j,k) * wm_interm_52_triplet_pt4(p,i,k,j)
term(996) = term(996) + r1(vrdav_Rr, a,i) * wm_interm_1_triplet_pt4(a,q,j,k) * wm_interm_52_triplet_pt4(p,k,i,j)
term(997) = term(997) + t1(a,i) * wm_interm_12_triplet_pt4(p,j,i,k) * wm_interm_67_triplet_pt4(a,q,j,k)
term(998) = term(998) + t1(a,i) * wm_interm_12_triplet_pt4(p,j,i,k) * wm_interm_74_triplet_pt4(a,q,j,k)
term(999) = term(999) + r1(vrdav_Rr, a,i) * wm_interm_11_triplet_pt4(a,q,j,k) * wm_interm_52_triplet_pt4(p,k,i,j)
term(1000) = term(1000) + t1(a,i) * wm_interm_12_triplet_pt4(p,j,i,k) * wm_interm_71_triplet_pt4(a,q,j,k)
term(1001) = term(1001) + t1(a,i) * wm_interm_12_triplet_pt4(p,j,i,k) * wm_interm_66_triplet_pt4(a,q,j,k)
term(1002) = term(1002) + r1(vrdav_Rr, a,i) * wm_interm_1_triplet_pt4(a,q,j,k) * wm_interm_52_triplet_pt4(p,i,k,j)
term(1003) = term(1003) + r1(vrdav_Rr, a,i) * wm_interm_11_triplet_pt4(a,q,j,k) * wm_interm_52_triplet_pt4(p,i,k,j)
term(1004) = term(1004) + r1(vrdav_Rr, p,i) * wm_interm_151_triplet_pt4(a,j,i,k) * wm_interm_158_triplet_pt4(q,a,k,j)
term(1005) = term(1005) + r1(vrdav_Rr, p,i) * wm_interm_159_triplet_pt4(q,a,j,k) * wm_interm_21_triplet_pt4(a,k,i,j)
term(1006) = term(1006) + r1(vrdav_Rr, p,i) * wm_interm_5_triplet_pt4(q,a,j,k) * wm_interm_61_triplet_pt4(a,k,i,j)
term(1007) = term(1007) + r1(vrdav_Rr, p,i) * wm_interm_151_triplet_pt4(a,j,i,k) * wm_interm_157_triplet_pt4(q,a,k,j)
term(1008) = term(1008) + r1(vrdav_Rr, p,i) * wm_interm_151_triplet_pt4(a,j,i,k) * wm_interm_161_triplet_pt4(q,a,k,j)
term(1009) = term(1009) + r1(vrdav_Rr, p,i) * wm_interm_10_triplet_pt4(q,a,j,k) * wm_interm_63_triplet_pt4(a,k,i,j)
term(1010) = term(1010) + r1(vrdav_Rr, p,i) * wm_interm_11_triplet_pt4(q,a,j,k) * wm_interm_63_triplet_pt4(a,k,i,j)
term(1011) = term(1011) + r1(vrdav_Rr, p,i) * wm_interm_1_triplet_pt4(q,a,j,k) * wm_interm_63_triplet_pt4(a,k,i,j)
term(1012) = term(1012) + t1(q,i) * wm_interm_12_triplet_pt4(a,j,i,k) * wm_interm_157_triplet_pt4(p,a,j,k)
term(1013) = term(1013) + t1(q,i) * wm_interm_12_triplet_pt4(a,j,i,k) * wm_interm_158_triplet_pt4(p,a,j,k)
term(1014) = term(1014) + t1(q,i) * wm_interm_10_triplet_pt4(p,a,j,k) * wm_interm_59_triplet_pt4(a,k,i,j)
term(1015) = term(1015) + t1(q,i) * wm_interm_11_triplet_pt4(p,a,j,k) * wm_interm_59_triplet_pt4(a,k,i,j)
term(1016) = term(1016) + t1(q,i) * wm_interm_12_triplet_pt4(a,j,i,k) * wm_interm_159_triplet_pt4(p,a,j,k)
term(1017) = term(1017) + t1(q,i) * wm_interm_12_triplet_pt4(a,j,i,k) * wm_interm_161_triplet_pt4(p,a,j,k)
term(1018) = term(1018) + t1(q,i) * wm_interm_59_triplet_pt4(a,j,i,k) * wm_interm_5_triplet_pt4(p,a,k,j)
term(1019) = term(1019) + t1(q,i) * wm_interm_1_triplet_pt4(p,a,j,k) * wm_interm_59_triplet_pt4(a,k,i,j)
term(1020) = term(1020) + t1(q,i) * wm_interm_5_triplet_pt4(p,a,j,k) * wm_interm_62_triplet_pt4(a,k,i,j)
term(1021) = term(1021) + r1(vrdav_Rr, a,i) * wm_interm_161_triplet_pt4(a,q,j,k) * wm_interm_21_triplet_pt4(p,i,k,j)
term(1022) = term(1022) + t1(a,i) * wm_interm_5_triplet_pt4(a,q,j,k) * wm_interm_62_triplet_pt4(p,k,i,j)
term(1023) = term(1023) + t1(a,i) * wm_interm_1_triplet_pt4(a,q,j,k) * wm_interm_59_triplet_pt4(p,k,i,j)
term(1024) = term(1024) + t1(a,i) * wm_interm_59_triplet_pt4(p,j,i,k) * wm_interm_5_triplet_pt4(a,q,k,j)
term(1025) = term(1025) + r1(vrdav_Rr, a,i) * wm_interm_158_triplet_pt4(a,q,j,k) * wm_interm_21_triplet_pt4(p,i,k,j)
term(1026) = term(1026) + r1(vrdav_Rr, a,i) * wm_interm_158_triplet_pt4(a,q,j,k) * wm_interm_21_triplet_pt4(p,k,i,j)
term(1027) = term(1027) + r1(vrdav_Rr, a,i) * wm_interm_159_triplet_pt4(a,q,j,k) * wm_interm_21_triplet_pt4(p,k,i,j)
term(1028) = term(1028) + r1(vrdav_Rr, a,i) * wm_interm_159_triplet_pt4(a,q,j,k) * wm_interm_21_triplet_pt4(p,i,k,j)
term(1029) = term(1029) + t1(a,i) * wm_interm_10_triplet_pt4(a,q,j,k) * wm_interm_59_triplet_pt4(p,k,i,j)
term(1030) = term(1030) + t1(a,i) * wm_interm_11_triplet_pt4(a,q,j,k) * wm_interm_59_triplet_pt4(p,k,i,j)
term(1031) = term(1031) + t1(a,i) * wm_interm_12_triplet_pt4(p,j,i,k) * wm_interm_158_triplet_pt4(a,q,j,k)
term(1032) = term(1032) + t1(a,i) * wm_interm_12_triplet_pt4(p,j,i,k) * wm_interm_159_triplet_pt4(a,q,j,k)
term(1033) = term(1033) + r1(vrdav_Rr, a,i) * wm_interm_5_triplet_pt4(a,q,j,k) * wm_interm_61_triplet_pt4(p,k,i,j)
term(1034) = term(1034) + r1(vrdav_Rr, a,i) * wm_interm_10_triplet_pt4(a,q,j,k) * wm_interm_61_triplet_pt4(p,i,k,j)
term(1035) = term(1035) + r1(vrdav_Rr, a,i) * wm_interm_5_triplet_pt4(a,q,j,k) * wm_interm_61_triplet_pt4(p,i,k,j)
term(1036) = term(1036) + r1(vrdav_Rr, a,i) * wm_interm_1_triplet_pt4(a,q,j,k) * wm_interm_61_triplet_pt4(p,i,k,j)
term(1037) = term(1037) + r1(vrdav_Rr, a,i) * wm_interm_11_triplet_pt4(a,q,j,k) * wm_interm_61_triplet_pt4(p,i,k,j)
term(1038) = term(1038) + t1(a,i) * wm_interm_10_triplet_pt4(a,q,j,k) * wm_interm_62_triplet_pt4(p,i,k,j)
term(1039) = term(1039) + t1(a,i) * wm_interm_10_triplet_pt4(a,q,j,k) * wm_interm_59_triplet_pt4(p,i,k,j)
term(1040) = term(1040) + t1(a,i) * wm_interm_1_triplet_pt4(a,q,j,k) * wm_interm_62_triplet_pt4(p,i,k,j)
term(1041) = term(1041) + t1(a,i) * wm_interm_11_triplet_pt4(a,q,j,k) * wm_interm_62_triplet_pt4(p,i,k,j)
term(1042) = term(1042) + r1(vrdav_Rr, a,i) * wm_interm_157_triplet_pt4(a,q,j,k) * wm_interm_21_triplet_pt4(p,i,k,j)
term(1043) = term(1043) + t1(a,i) * wm_interm_59_triplet_pt4(p,i,j,k) * wm_interm_5_triplet_pt4(a,q,k,j)
term(1044) = term(1044) + r1(vrdav_Rr, a,i) * wm_interm_161_triplet_pt4(a,q,j,k) * wm_interm_21_triplet_pt4(p,k,i,j)
term(1045) = term(1045) + r1(vrdav_Rr, a,i) * wm_interm_157_triplet_pt4(a,q,j,k) * wm_interm_21_triplet_pt4(p,k,i,j)
term(1046) = term(1046) + t1(a,i) * wm_interm_1_triplet_pt4(a,q,j,k) * wm_interm_59_triplet_pt4(p,i,k,j)
term(1047) = term(1047) + t1(a,i) * wm_interm_11_triplet_pt4(a,q,j,k) * wm_interm_59_triplet_pt4(p,i,k,j)
term(1048) = term(1048) + r1(vrdav_Rr, a,i) * wm_interm_5_triplet_pt4(a,q,j,k) * wm_interm_63_triplet_pt4(p,i,k,j)
term(1049) = term(1049) + r1(vrdav_Rr, a,i) * wm_interm_10_triplet_pt4(a,q,j,k) * wm_interm_63_triplet_pt4(p,k,i,j)
term(1050) = term(1050) + r1(vrdav_Rr, a,i) * wm_interm_10_triplet_pt4(a,q,j,k) * wm_interm_63_triplet_pt4(p,i,k,j)
term(1051) = term(1051) + r1(vrdav_Rr, a,i) * wm_interm_1_triplet_pt4(a,q,j,k) * wm_interm_63_triplet_pt4(p,k,i,j)
term(1052) = term(1052) + r1(vrdav_Rr, a,i) * wm_interm_11_triplet_pt4(a,q,j,k) * wm_interm_63_triplet_pt4(p,k,i,j)
term(1053) = term(1053) + t1(a,i) * wm_interm_12_triplet_pt4(p,j,i,k) * wm_interm_161_triplet_pt4(a,q,j,k)
term(1054) = term(1054) + t1(a,i) * wm_interm_12_triplet_pt4(p,j,i,k) * wm_interm_157_triplet_pt4(a,q,j,k)
term(1055) = term(1055) + r1(vrdav_Rr, a,i) * wm_interm_1_triplet_pt4(a,q,j,k) * wm_interm_63_triplet_pt4(p,i,k,j)
term(1056) = term(1056) + r1(vrdav_Rr, a,i) * wm_interm_11_triplet_pt4(a,q,j,k) * wm_interm_63_triplet_pt4(p,i,k,j)
end do 
end do 
end do 
end do 

term(848) = term(848) * (-2.0d+0) 
term(849) = term(849) * (4.0d+0) 
term(850) = term(850) * (-2.0d+0) 
term(851) = term(851) * (-2.0d+0) 
term(852) = term(852) * (4.0d+0) 
term(853) = term(853) * (4.0d+0) 
term(854) = term(854) * (-8.0d+0) 
term(855) = term(855) * (-2.0d+0) 
term(856) = term(856) * (4.0d+0) 
term(857) = term(857) * (4.0d+0) 
term(858) = term(858) * (-8.0d+0) 
term(859) = term(859) * (-2.0d+0) 
term(860) = term(860) * (4.0d+0) 
term(861) = term(861) * (-2.0d+0) 
term(862) = term(862) * (-2.0d+0) 
term(863) = term(863) * (-2.0d+0) 
term(864) = term(864) * (4.0d+0) 
term(865) = term(865) * (4.0d+0) 
term(866) = term(866) * (-8.0d+0) 
term(867) = term(867) * (-2.0d+0) 
term(868) = term(868) * (4.0d+0) 
term(869) = term(869) * (-2.0d+0) 
term(870) = term(870) * (4.0d+0) 
term(871) = term(871) * (-8.0d+0) 
term(872) = term(872) * (8.0d+0) 
term(873) = term(873) * (-8.0d+0) 
term(874) = term(874) * (8.0d+0) 
term(875) = term(875) * (16.0d+0) 
term(876) = term(876) * (-16.0d+0) 
term(877) = term(877) * (8.0d+0) 
term(878) = term(878) * (-16.0d+0) 
term(879) = term(879) * (-4.0d+0) 
term(880) = term(880) * (-4.0d+0) 
term(881) = term(881) * (-8.0d+0) 
term(882) = term(882) * (8.0d+0) 
term(883) = term(883) * (16.0d+0) 
term(884) = term(884) * (-16.0d+0) 
term(885) = term(885) * (-4.0d+0) 
term(886) = term(886) * (8.0d+0) 
term(887) = term(887) * (-2.0d+0) 
term(888) = term(888) * (-2.0d+0) 
term(889) = term(889) * (4.0d+0) 
term(890) = term(890) * (4.0d+0) 
term(891) = term(891) * (4.0d+0) 
term(892) = term(892) * (-8.0d+0) 
term(893) = term(893) * (-2.0d+0) 
term(894) = term(894) * (4.0d+0) 
term(895) = term(895) * (-2.0d+0) 
term(896) = term(896) * (-2.0d+0) 
term(897) = term(897) * (-2.0d+0) 
term(898) = term(898) * (4.0d+0) 
term(899) = term(899) * (-2.0d+0) 
term(900) = term(900) * (4.0d+0) 
term(901) = term(901) * (4.0d+0) 
term(902) = term(902) * (-8.0d+0) 
term(903) = term(903) * (-2.0d+0) 
term(904) = term(904) * (4.0d+0) 
term(905) = term(905) * (-2.0d+0) 
term(906) = term(906) * (4.0d+0) 
term(907) = term(907) * (-2.0d+0) 
term(908) = term(908) * (-2.0d+0) 
term(909) = term(909) * (4.0d+0) 
term(910) = term(910) * (4.0d+0) 
term(911) = term(911) * (-8.0d+0) 
term(912) = term(912) * (-2.0d+0) 
term(913) = term(913) * (4.0d+0) 
term(914) = term(914) * (4.0d+0) 
term(915) = term(915) * (-8.0d+0) 
term(916) = term(916) * (-2.0d+0) 
term(917) = term(917) * (4.0d+0) 
term(918) = term(918) * (-8.0d+0) 
term(919) = term(919) * (8.0d+0) 
term(920) = term(920) * (16.0d+0) 
term(921) = term(921) * (-16.0d+0) 
term(922) = term(922) * (-4.0d+0) 
term(923) = term(923) * (8.0d+0) 
term(924) = term(924) * (-4.0d+0) 
term(925) = term(925) * (-4.0d+0) 
term(926) = term(926) * (-4.0d+0) 
term(927) = term(927) * (8.0d+0) 
term(928) = term(928) * (-8.0d+0) 
term(929) = term(929) * (8.0d+0) 
term(930) = term(930) * (16.0d+0) 
term(931) = term(931) * (-16.0d+0) 
term(932) = term(932) * (8.0d+0) 
term(933) = term(933) * (-8.0d+0) 
term(934) = term(934) * (8.0d+0) 
term(935) = term(935) * (-16.0d+0) 
term(936) = term(936) * (-4.0d+0) 
term(937) = term(937) * (8.0d+0) 
term(938) = term(938) * (8.0d+0) 
term(939) = term(939) * (-16.0d+0) 
term(940) = term(940) * (-4.0d+0) 
term(941) = term(941) * (8.0d+0) 
term(942) = term(942) * (2.0d+0) 
term(943) = term(943) * (2.0d+0) 
term(944) = term(944) * (2.0d+0) 
term(945) = term(945) * (-4.0d+0) 
term(946) = term(946) * (2.0d+0) 
term(947) = term(947) * (2.0d+0) 
term(948) = term(948) * (2.0d+0) 
term(949) = term(949) * (-4.0d+0) 
term(950) = term(950) * (2.0d+0) 
term(951) = term(951) * (4.0d+0) 
term(952) = term(952) * (-2.0d+0) 
term(954) = term(954) * (-2.0d+0) 
term(955) = term(955) * (-2.0d+0) 
term(957) = term(957) * (-2.0d+0) 
term(959) = term(959) * (-2.0d+0) 
term(965) = term(965) * (-2.0d+0) 
term(967) = term(967) * (-2.0d+0) 
term(969) = term(969) * (-2.0d+0) 
term(971) = term(971) * (-2.0d+0) 
term(972) = term(972) * (-2.0d+0) 
term(976) = term(976) * (-2.0d+0) 
term(978) = term(978) * (-2.0d+0) 
term(980) = term(980) * (-2.0d+0) 
term(983) = term(983) * (-2.0d+0) 
term(986) = term(986) * (-2.0d+0) 
term(987) = term(987) * (-2.0d+0) 
term(989) = term(989) * (-2.0d+0) 
term(990) = term(990) * (4.0d+0) 
term(991) = term(991) * (-2.0d+0) 
term(992) = term(992) * (4.0d+0) 
term(995) = term(995) * (-2.0d+0) 
term(997) = term(997) * (-2.0d+0) 
term(999) = term(999) * (-2.0d+0) 
term(1000) = term(1000) * (-2.0d+0) 
term(1001) = term(1001) * (4.0d+0) 
term(1002) = term(1002) * (-2.0d+0) 
term(1003) = term(1003) * (4.0d+0) 
term(1004) = term(1004) * (8.0d+0) 
term(1005) = term(1005) * (4.0d+0) 
term(1006) = term(1006) * (4.0d+0) 
term(1007) = term(1007) * (-8.0d+0) 
term(1008) = term(1008) * (4.0d+0) 
term(1009) = term(1009) * (4.0d+0) 
term(1010) = term(1010) * (-8.0d+0) 
term(1011) = term(1011) * (4.0d+0) 
term(1012) = term(1012) * (8.0d+0) 
term(1013) = term(1013) * (-8.0d+0) 
term(1014) = term(1014) * (2.0d+0) 
term(1015) = term(1015) * (-4.0d+0) 
term(1016) = term(1016) * (4.0d+0) 
term(1017) = term(1017) * (-4.0d+0) 
term(1018) = term(1018) * (-4.0d+0) 
term(1019) = term(1019) * (2.0d+0) 
term(1020) = term(1020) * (2.0d+0) 
term(1021) = term(1021) * (4.0d+0) 
term(1022) = term(1022) * (2.0d+0) 
term(1023) = term(1023) * (2.0d+0) 
term(1024) = term(1024) * (-4.0d+0) 
term(1025) = term(1025) * (4.0d+0) 
term(1026) = term(1026) * (-8.0d+0) 
term(1027) = term(1027) * (4.0d+0) 
term(1028) = term(1028) * (-4.0d+0) 
term(1029) = term(1029) * (2.0d+0) 
term(1030) = term(1030) * (-4.0d+0) 
term(1031) = term(1031) * (-8.0d+0) 
term(1032) = term(1032) * (4.0d+0) 
term(1033) = term(1033) * (2.0d+0) 
term(1034) = term(1034) * (2.0d+0) 
term(1035) = term(1035) * (-4.0d+0) 
term(1036) = term(1036) * (2.0d+0) 
term(1037) = term(1037) * (-4.0d+0) 
term(1038) = term(1038) * (2.0d+0) 
term(1039) = term(1039) * (-4.0d+0) 
term(1040) = term(1040) * (2.0d+0) 
term(1041) = term(1041) * (-4.0d+0) 
term(1042) = term(1042) * (-4.0d+0) 
term(1043) = term(1043) * (2.0d+0) 
term(1044) = term(1044) * (-4.0d+0) 
term(1045) = term(1045) * (8.0d+0) 
term(1046) = term(1046) * (-4.0d+0) 
term(1047) = term(1047) * (8.0d+0) 
term(1048) = term(1048) * (2.0d+0) 
term(1049) = term(1049) * (2.0d+0) 
term(1050) = term(1050) * (-4.0d+0) 
term(1051) = term(1051) * (2.0d+0) 
term(1052) = term(1052) * (-4.0d+0) 
term(1053) = term(1053) * (-4.0d+0) 
term(1054) = term(1054) * (8.0d+0) 
term(1055) = term(1055) * (-4.0d+0) 
term(1056) = term(1056) * (8.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(1057) = term(1057) + r1(vrdav_Rl, p,i) * wm_interm_102_triplet_pt4(j,k) * wm_interm_3_triplet_pt4(q,j,i,k)
term(1058) = term(1058) + r1(vrdav_Rl, p,i) * wm_interm_103_triplet_pt4(j,k) * wm_interm_3_triplet_pt4(q,j,i,k)
term(1059) = term(1059) + r1(vrdav_Rl, p,i) * wm_interm_104_triplet_pt4(j,k) * wm_interm_3_triplet_pt4(q,j,i,k)
term(1060) = term(1060) + s1(p,i) * wm_interm_102_triplet_pt4(j,k) * wm_interm_13_triplet_pt4(q,j,i,k)
term(1061) = term(1061) + s1(p,i) * wm_interm_103_triplet_pt4(j,k) * wm_interm_13_triplet_pt4(q,j,i,k)
term(1062) = term(1062) + s1(p,i) * wm_interm_104_triplet_pt4(j,k) * wm_interm_13_triplet_pt4(q,j,i,k)
term(1063) = term(1063) + r1(vrdav_Rl, p,i) * wm_interm_136_triplet_pt4(j,k) * wm_interm_3_triplet_pt4(q,j,i,k)
term(1064) = term(1064) + r1(vrdav_Rl, p,i) * wm_interm_137_triplet_pt4(j,k) * wm_interm_3_triplet_pt4(q,j,i,k)
term(1065) = term(1065) + r1(vrdav_Rl, p,i) * wm_interm_138_triplet_pt4(j,k) * wm_interm_3_triplet_pt4(q,j,i,k)
term(1066) = term(1066) + s1(p,i) * wm_interm_136_triplet_pt4(j,k) * wm_interm_13_triplet_pt4(q,j,i,k)
term(1067) = term(1067) + s1(p,i) * wm_interm_137_triplet_pt4(j,k) * wm_interm_13_triplet_pt4(q,j,i,k)
term(1068) = term(1068) + s1(p,i) * wm_interm_138_triplet_pt4(j,k) * wm_interm_13_triplet_pt4(q,j,i,k)
term(1069) = term(1069) + r1(vrdav_Rr, p,i) * wm_interm_21_triplet_pt4(q,j,i,k) * wm_interm_79_triplet_pt4(k,j)
term(1070) = term(1070) + r1(vrdav_Rr, p,i) * wm_interm_21_triplet_pt4(q,j,i,k) * wm_interm_72_triplet_pt4(k,j)
term(1071) = term(1071) + r1(vrdav_Rr, p,i) * wm_interm_21_triplet_pt4(q,j,i,k) * wm_interm_76_triplet_pt4(k,j)
term(1072) = term(1072) + t1(q,i) * wm_interm_12_triplet_pt4(p,j,i,k) * wm_interm_72_triplet_pt4(j,k)
term(1073) = term(1073) + t1(q,i) * wm_interm_12_triplet_pt4(p,j,i,k) * wm_interm_76_triplet_pt4(j,k)
term(1074) = term(1074) + t1(q,i) * wm_interm_12_triplet_pt4(p,j,i,k) * wm_interm_79_triplet_pt4(j,k)
term(1075) = term(1075) + t1(q,i) * wm_interm_12_triplet_pt4(p,j,i,k) * wm_interm_162_triplet_pt4(j,k)
term(1076) = term(1076) + t1(q,i) * wm_interm_12_triplet_pt4(p,j,i,k) * wm_interm_165_triplet_pt4(j,k)
end do 
end do 
end do 

term(1057) = term(1057) * (-2.0d+0) 
term(1058) = term(1058) * (4.0d+0) 
term(1059) = term(1059) * (-2.0d+0) 
term(1060) = term(1060) * (-2.0d+0) 
term(1061) = term(1061) * (4.0d+0) 
term(1062) = term(1062) * (-2.0d+0) 
term(1063) = term(1063) * (-4.0d+0) 
term(1064) = term(1064) * (8.0d+0) 
term(1065) = term(1065) * (-4.0d+0) 
term(1066) = term(1066) * (-4.0d+0) 
term(1067) = term(1067) * (8.0d+0) 
term(1068) = term(1068) * (-4.0d+0) 
term(1069) = term(1069) * (2.0d+0) 
term(1070) = term(1070) * (2.0d+0) 
term(1071) = term(1071) * (-4.0d+0) 
term(1073) = term(1073) * (-2.0d+0) 
term(1075) = term(1075) * (4.0d+0) 
term(1076) = term(1076) * (-4.0d+0) 

do j = 1, nocc 
do a = nocc + 1, nactive 
do i = 1, nocc 
do b = nocc + 1, nactive 
term(1077) = term(1077) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_18_triplet_pt4(a,b) * wm_interm_32_triplet_pt4(b,q,i,j)
term(1078) = term(1078) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_19_triplet_pt4(a,b) * wm_interm_32_triplet_pt4(b,q,i,j)
term(1079) = term(1079) + r2p(vrdav_Rl, q,i,a,j) * wm_interm_18_triplet_pt4(b,a) * wm_interm_56_triplet_pt4(p,b,i,j)
term(1080) = term(1080) + r2p(vrdav_Rl, q,i,a,j) * wm_interm_19_triplet_pt4(b,a) * wm_interm_56_triplet_pt4(p,b,i,j)
term(1081) = term(1081) + r2p(vrdav_Rl, p,i,a,j) * wm_interm_18_triplet_pt4(b,a) * wm_interm_56_triplet_pt4(b,q,i,j)
term(1082) = term(1082) + r2p(vrdav_Rl, p,i,a,j) * wm_interm_19_triplet_pt4(b,a) * wm_interm_56_triplet_pt4(b,q,i,j)
end do 
end do 
end do 
end do 

term(1078) = term(1078) * (-2.0d+0) 
term(1079) = term(1079) * (-4.0d+0) 
term(1080) = term(1080) * (8.0d+0) 
term(1082) = term(1082) * (-2.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
term(1083) = term(1083) + wm_interm_115_triplet_pt4(q,p,i,j) * wm_interm_25_triplet_pt4(j,i)
term(1084) = term(1084) + wm_interm_116_triplet_pt4(q,p,i,j) * wm_interm_25_triplet_pt4(j,i)
term(1085) = term(1085) + wm_interm_109_triplet_pt4(q,p,i,j) * wm_interm_25_triplet_pt4(j,i)
term(1086) = term(1086) + wm_interm_108_triplet_pt4(q,p,i,j) * wm_interm_25_triplet_pt4(j,i)
term(1087) = term(1087) + wm_interm_107_triplet_pt4(q,p,i,j) * wm_interm_25_triplet_pt4(j,i)
term(1088) = term(1088) + wm_interm_114_triplet_pt4(q,p,i,j) * wm_interm_25_triplet_pt4(j,i)
term(1089) = term(1089) + wm_interm_124_triplet_pt4(q,p,i,j) * wm_interm_25_triplet_pt4(j,i)
term(1090) = term(1090) + wm_interm_123_triplet_pt4(q,p,i,j) * wm_interm_25_triplet_pt4(j,i)
term(1091) = term(1091) + wm_interm_130_triplet_pt4(q,p,i,j) * wm_interm_25_triplet_pt4(j,i)
term(1092) = term(1092) + wm_interm_131_triplet_pt4(q,p,i,j) * wm_interm_25_triplet_pt4(j,i)
term(1093) = term(1093) + wm_interm_144_triplet_pt4(q,p,i,j) * wm_interm_25_triplet_pt4(j,i)
term(1094) = term(1094) + wm_interm_145_triplet_pt4(q,p,i,j) * wm_interm_25_triplet_pt4(j,i)
term(1095) = term(1095) + wm_interm_141_triplet_pt4(q,p,i,j) * wm_interm_25_triplet_pt4(j,i)
term(1096) = term(1096) + wm_interm_140_triplet_pt4(q,p,i,j) * wm_interm_25_triplet_pt4(j,i)
term(1097) = term(1097) + wm_interm_44_triplet_pt4(p,q,i,j) * wm_interm_94_triplet_pt4(j,i)
term(1098) = term(1098) + wm_interm_44_triplet_pt4(p,q,i,j) * wm_interm_95_triplet_pt4(j,i)
term(1099) = term(1099) + wm_interm_44_triplet_pt4(p,q,i,j) * wm_interm_97_triplet_pt4(j,i)
term(1100) = term(1100) + wm_interm_168_triplet_pt4(q,p,i,j) * wm_interm_25_triplet_pt4(j,i)
term(1101) = term(1101) + wm_interm_169_triplet_pt4(q,p,i,j) * wm_interm_25_triplet_pt4(j,i)
term(1102) = term(1102) + wm_interm_176_triplet_pt4(q,p,i,j) * wm_interm_25_triplet_pt4(j,i)
term(1103) = term(1103) + wm_interm_172_triplet_pt4(q,p,i,j) * wm_interm_25_triplet_pt4(j,i)
term(1104) = term(1104) + wm_interm_173_triplet_pt4(q,p,i,j) * wm_interm_25_triplet_pt4(j,i)
term(1105) = term(1105) + wm_interm_167_triplet_pt4(q,p,i,j) * wm_interm_25_triplet_pt4(j,i)
term(1106) = term(1106) + wm_interm_181_triplet_pt4(q,p,i,j) * wm_interm_25_triplet_pt4(j,i)
term(1107) = term(1107) + wm_interm_166_triplet_pt4(q,p,i,j) * wm_interm_25_triplet_pt4(j,i)
term(1108) = term(1108) + wm_interm_189_triplet_pt4(q,p,i,j) * wm_interm_25_triplet_pt4(j,i)
term(1109) = term(1109) + wm_interm_188_triplet_pt4(q,p,i,j) * wm_interm_25_triplet_pt4(j,i)
term(1110) = term(1110) + wm_interm_191_triplet_pt4(q,p,i,j) * wm_interm_25_triplet_pt4(j,i)
term(1111) = term(1111) + wm_interm_192_triplet_pt4(q,p,i,j) * wm_interm_25_triplet_pt4(j,i)
term(1112) = term(1112) + r1(vrdav_Rl, q,j) * wm_interm_16_triplet_pt4(i,j) * wm_interm_33_triplet_pt4(p,i)
term(1113) = term(1113) + r1(vrdav_Rl, q,j) * wm_interm_17_triplet_pt4(i,j) * wm_interm_33_triplet_pt4(p,i)
term(1114) = term(1114) + r1(vrdav_Rl, q,j) * wm_interm_16_triplet_pt4(i,j) * wm_interm_36_triplet_pt4(p,i)
term(1115) = term(1115) + r1(vrdav_Rl, q,j) * wm_interm_17_triplet_pt4(i,j) * wm_interm_36_triplet_pt4(p,i)
term(1116) = term(1116) + r1(vrdav_Rl, q,j) * wm_interm_16_triplet_pt4(i,j) * wm_interm_38_triplet_pt4(p,i)
term(1117) = term(1117) + r1(vrdav_Rl, q,j) * wm_interm_17_triplet_pt4(i,j) * wm_interm_38_triplet_pt4(p,i)
term(1118) = term(1118) + r1(vrdav_Rl, q,j) * wm_interm_16_triplet_pt4(i,j) * wm_interm_40_triplet_pt4(p,i)
term(1119) = term(1119) + r1(vrdav_Rl, q,j) * wm_interm_17_triplet_pt4(i,j) * wm_interm_40_triplet_pt4(p,i)
term(1120) = term(1120) + r1(vrdav_Rl, q,j) * wm_interm_16_triplet_pt4(i,j) * wm_interm_42_triplet_pt4(p,i)
term(1121) = term(1121) + r1(vrdav_Rl, q,j) * wm_interm_17_triplet_pt4(i,j) * wm_interm_42_triplet_pt4(p,i)
term(1122) = term(1122) + r1(vrdav_Rl, p,i) * wm_interm_102_triplet_pt4(i,j) * wm_interm_7_triplet_pt4(q,j)
term(1123) = term(1123) + r1(vrdav_Rl, p,i) * wm_interm_103_triplet_pt4(i,j) * wm_interm_7_triplet_pt4(q,j)
term(1124) = term(1124) + r1(vrdav_Rl, p,i) * wm_interm_104_triplet_pt4(i,j) * wm_interm_7_triplet_pt4(q,j)
term(1125) = term(1125) + r1(vrdav_Rl, p,i) * wm_interm_102_triplet_pt4(i,j) * wm_interm_6_triplet_pt4(q,j)
term(1126) = term(1126) + r1(vrdav_Rl, p,i) * wm_interm_103_triplet_pt4(i,j) * wm_interm_6_triplet_pt4(q,j)
term(1127) = term(1127) + r1(vrdav_Rl, p,i) * wm_interm_104_triplet_pt4(i,j) * wm_interm_6_triplet_pt4(q,j)
term(1128) = term(1128) + s1(p,i) * wm_interm_102_triplet_pt4(i,j) * wm_interm_15_triplet_pt4(q,j)
term(1129) = term(1129) + s1(p,i) * wm_interm_103_triplet_pt4(i,j) * wm_interm_15_triplet_pt4(q,j)
term(1130) = term(1130) + s1(p,i) * wm_interm_104_triplet_pt4(i,j) * wm_interm_15_triplet_pt4(q,j)
term(1131) = term(1131) + r1(vrdav_Rl, p,i) * wm_interm_136_triplet_pt4(i,j) * wm_interm_7_triplet_pt4(q,j)
term(1132) = term(1132) + r1(vrdav_Rl, p,i) * wm_interm_137_triplet_pt4(i,j) * wm_interm_7_triplet_pt4(q,j)
term(1133) = term(1133) + r1(vrdav_Rl, p,i) * wm_interm_138_triplet_pt4(i,j) * wm_interm_7_triplet_pt4(q,j)
term(1134) = term(1134) + r1(vrdav_Rl, p,i) * wm_interm_136_triplet_pt4(i,j) * wm_interm_6_triplet_pt4(q,j)
term(1135) = term(1135) + r1(vrdav_Rl, p,i) * wm_interm_137_triplet_pt4(i,j) * wm_interm_6_triplet_pt4(q,j)
term(1136) = term(1136) + r1(vrdav_Rl, p,i) * wm_interm_138_triplet_pt4(i,j) * wm_interm_6_triplet_pt4(q,j)
term(1137) = term(1137) + s1(p,i) * wm_interm_136_triplet_pt4(i,j) * wm_interm_15_triplet_pt4(q,j)
term(1138) = term(1138) + s1(p,i) * wm_interm_137_triplet_pt4(i,j) * wm_interm_15_triplet_pt4(q,j)
term(1139) = term(1139) + s1(p,i) * wm_interm_138_triplet_pt4(i,j) * wm_interm_15_triplet_pt4(q,j)
term(1140) = term(1140) + r1(vrdav_Rr, p,i) * wm_interm_16_triplet_pt4(i,j) * wm_interm_53_triplet_pt4(q,j)
term(1141) = term(1141) + r1(vrdav_Rr, p,i) * wm_interm_17_triplet_pt4(i,j) * wm_interm_53_triplet_pt4(q,j)
term(1142) = term(1142) + r1(vrdav_Rr, p,i) * wm_interm_16_triplet_pt4(i,j) * wm_interm_54_triplet_pt4(q,j)
term(1143) = term(1143) + r1(vrdav_Rr, p,i) * wm_interm_17_triplet_pt4(i,j) * wm_interm_54_triplet_pt4(q,j)
term(1144) = term(1144) + r1(vrdav_Rr, p,i) * wm_interm_16_triplet_pt4(i,j) * wm_interm_55_triplet_pt4(q,j)
term(1145) = term(1145) + r1(vrdav_Rr, p,i) * wm_interm_17_triplet_pt4(i,j) * wm_interm_55_triplet_pt4(q,j)
term(1146) = term(1146) + t1(q,j) * wm_interm_16_triplet_pt4(j,i) * wm_interm_46_triplet_pt4(p,i)
term(1147) = term(1147) + t1(q,j) * wm_interm_17_triplet_pt4(j,i) * wm_interm_46_triplet_pt4(p,i)
term(1148) = term(1148) + t1(q,j) * wm_interm_16_triplet_pt4(j,i) * wm_interm_48_triplet_pt4(p,i)
term(1149) = term(1149) + t1(q,j) * wm_interm_17_triplet_pt4(j,i) * wm_interm_48_triplet_pt4(p,i)
term(1150) = term(1150) + t1(q,j) * wm_interm_16_triplet_pt4(j,i) * wm_interm_50_triplet_pt4(p,i)
term(1151) = term(1151) + t1(q,j) * wm_interm_17_triplet_pt4(j,i) * wm_interm_50_triplet_pt4(p,i)
term(1152) = term(1152) + t1(q,j) * wm_interm_2_triplet_pt4(p,i) * wm_interm_72_triplet_pt4(j,i)
term(1153) = term(1153) + t1(q,j) * wm_interm_2_triplet_pt4(p,i) * wm_interm_76_triplet_pt4(j,i)
term(1154) = term(1154) + t1(q,j) * wm_interm_2_triplet_pt4(p,i) * wm_interm_79_triplet_pt4(j,i)
term(1155) = term(1155) + r1(vrdav_Rr, p,i) * wm_interm_16_triplet_pt4(i,j) * wm_interm_64_triplet_pt4(q,j)
term(1156) = term(1156) + r1(vrdav_Rr, p,i) * wm_interm_17_triplet_pt4(i,j) * wm_interm_64_triplet_pt4(q,j)
term(1157) = term(1157) + r1(vrdav_Rr, p,i) * wm_interm_16_triplet_pt4(i,j) * wm_interm_65_triplet_pt4(q,j)
term(1158) = term(1158) + r1(vrdav_Rr, p,i) * wm_interm_17_triplet_pt4(i,j) * wm_interm_65_triplet_pt4(q,j)
term(1159) = term(1159) + t1(q,j) * wm_interm_16_triplet_pt4(j,i) * wm_interm_58_triplet_pt4(p,i)
term(1160) = term(1160) + t1(q,j) * wm_interm_17_triplet_pt4(j,i) * wm_interm_58_triplet_pt4(p,i)
term(1161) = term(1161) + t1(q,j) * wm_interm_16_triplet_pt4(j,i) * wm_interm_60_triplet_pt4(p,i)
term(1162) = term(1162) + t1(q,j) * wm_interm_17_triplet_pt4(j,i) * wm_interm_60_triplet_pt4(p,i)
term(1163) = term(1163) + t1(q,j) * wm_interm_162_triplet_pt4(j,i) * wm_interm_2_triplet_pt4(p,i)
term(1164) = term(1164) + t1(q,j) * wm_interm_165_triplet_pt4(j,i) * wm_interm_2_triplet_pt4(p,i)
end do 
end do 

term(1083) = term(1083) * (0.5d+0) 
term(1084) = term(1084) * (-1.0d+0) 
term(1085) = term(1085) * (0.5d+0) 
term(1086) = term(1086) * (0.5d+0) 
term(1087) = term(1087) * (-1.0d+0) 
term(1088) = term(1088) * (0.5d+0) 
term(1089) = term(1089) * (2.0d+0) 
term(1090) = term(1090) * (-2.0d+0) 
term(1091) = term(1091) * (2.0d+0) 
term(1092) = term(1092) * (-2.0d+0) 
term(1093) = term(1093) * (2.0d+0) 
term(1094) = term(1094) * (-2.0d+0) 
term(1095) = term(1095) * (2.0d+0) 
term(1096) = term(1096) * (-2.0d+0) 
term(1097) = term(1097) * (0.5d+0) 
term(1098) = term(1098) * (-1.0d+0) 
term(1099) = term(1099) * (0.5d+0) 
term(1100) = term(1100) * (2.0d+0) 
term(1101) = term(1101) * (-2.0d+0) 
term(1102) = term(1102) * (2.0d+0) 
term(1103) = term(1103) * (2.0d+0) 
term(1104) = term(1104) * (-2.0d+0) 
term(1105) = term(1105) * (2.0d+0) 
term(1106) = term(1106) * (-2.0d+0) 
term(1107) = term(1107) * (-2.0d+0) 
term(1108) = term(1108) * (8.0d+0) 
term(1109) = term(1109) * (-8.0d+0) 
term(1110) = term(1110) * (8.0d+0) 
term(1111) = term(1111) * (-8.0d+0) 
term(1112) = term(1112) * (2.0d+0) 
term(1113) = term(1113) * (-4.0d+0) 
term(1114) = term(1114) * (-1.0d+0) 
term(1115) = term(1115) * (2.0d+0) 
term(1116) = term(1116) * (-1.0d+0) 
term(1117) = term(1117) * (2.0d+0) 
term(1118) = term(1118) * (4.0d+0) 
term(1119) = term(1119) * (-8.0d+0) 
term(1120) = term(1120) * (-4.0d+0) 
term(1121) = term(1121) * (8.0d+0) 
term(1122) = term(1122) * (-2.0d+0) 
term(1123) = term(1123) * (4.0d+0) 
term(1124) = term(1124) * (-2.0d+0) 
term(1125) = term(1125) * (4.0d+0) 
term(1126) = term(1126) * (-8.0d+0) 
term(1127) = term(1127) * (4.0d+0) 
term(1128) = term(1128) * (-2.0d+0) 
term(1129) = term(1129) * (4.0d+0) 
term(1130) = term(1130) * (-2.0d+0) 
term(1131) = term(1131) * (-4.0d+0) 
term(1132) = term(1132) * (8.0d+0) 
term(1133) = term(1133) * (-4.0d+0) 
term(1134) = term(1134) * (8.0d+0) 
term(1135) = term(1135) * (-16.0d+0) 
term(1136) = term(1136) * (8.0d+0) 
term(1137) = term(1137) * (-4.0d+0) 
term(1138) = term(1138) * (8.0d+0) 
term(1139) = term(1139) * (-4.0d+0) 
term(1140) = term(1140) * (-4.0d+0) 
term(1141) = term(1141) * (8.0d+0) 
term(1142) = term(1142) * (2.0d+0) 
term(1143) = term(1143) * (-4.0d+0) 
term(1144) = term(1144) * (2.0d+0) 
term(1145) = term(1145) * (-4.0d+0) 
term(1147) = term(1147) * (-2.0d+0) 
term(1148) = term(1148) * (-2.0d+0) 
term(1149) = term(1149) * (4.0d+0) 
term(1151) = term(1151) * (-2.0d+0) 
term(1153) = term(1153) * (-2.0d+0) 
term(1155) = term(1155) * (-8.0d+0) 
term(1156) = term(1156) * (16.0d+0) 
term(1157) = term(1157) * (8.0d+0) 
term(1158) = term(1158) * (-16.0d+0) 
term(1159) = term(1159) * (4.0d+0) 
term(1160) = term(1160) * (-8.0d+0) 
term(1161) = term(1161) * (-4.0d+0) 
term(1162) = term(1162) * (8.0d+0) 
term(1163) = term(1163) * (4.0d+0) 
term(1164) = term(1164) * (-4.0d+0) 

do a = nocc + 1, nactive 
do i = 1, nocc 
term(1165) = term(1165) + r1(vrdav_Rl, a,i) * wm_interm_18_triplet_pt4(q,a) * wm_interm_33_triplet_pt4(p,i)
term(1166) = term(1166) + r1(vrdav_Rl, a,i) * wm_interm_19_triplet_pt4(q,a) * wm_interm_33_triplet_pt4(p,i)
term(1167) = term(1167) + r1(vrdav_Rl, a,i) * wm_interm_18_triplet_pt4(q,a) * wm_interm_36_triplet_pt4(p,i)
term(1168) = term(1168) + r1(vrdav_Rl, a,i) * wm_interm_19_triplet_pt4(q,a) * wm_interm_36_triplet_pt4(p,i)
term(1169) = term(1169) + r1(vrdav_Rl, a,i) * wm_interm_18_triplet_pt4(q,a) * wm_interm_38_triplet_pt4(p,i)
term(1170) = term(1170) + r1(vrdav_Rl, a,i) * wm_interm_19_triplet_pt4(q,a) * wm_interm_38_triplet_pt4(p,i)
term(1171) = term(1171) + r1(vrdav_Rl, a,i) * wm_interm_18_triplet_pt4(q,a) * wm_interm_40_triplet_pt4(p,i)
term(1172) = term(1172) + r1(vrdav_Rl, a,i) * wm_interm_19_triplet_pt4(q,a) * wm_interm_40_triplet_pt4(p,i)
term(1173) = term(1173) + r1(vrdav_Rl, a,i) * wm_interm_18_triplet_pt4(q,a) * wm_interm_42_triplet_pt4(p,i)
term(1174) = term(1174) + r1(vrdav_Rl, a,i) * wm_interm_19_triplet_pt4(q,a) * wm_interm_42_triplet_pt4(p,i)
term(1175) = term(1175) + r1(vrdav_Rl, q,i) * wm_interm_6_triplet_pt4(a,i) * wm_interm_98_triplet_pt4(p,a)
term(1176) = term(1176) + r1(vrdav_Rl, q,i) * wm_interm_6_triplet_pt4(a,i) * wm_interm_99_triplet_pt4(p,a)
term(1177) = term(1177) + r1(vrdav_Rl, q,i) * wm_interm_100_triplet_pt4(p,a) * wm_interm_6_triplet_pt4(a,i)
term(1178) = term(1178) + r1(vrdav_Rl, q,i) * wm_interm_7_triplet_pt4(a,i) * wm_interm_98_triplet_pt4(p,a)
term(1179) = term(1179) + r1(vrdav_Rl, q,i) * wm_interm_7_triplet_pt4(a,i) * wm_interm_99_triplet_pt4(p,a)
term(1180) = term(1180) + r1(vrdav_Rl, q,i) * wm_interm_100_triplet_pt4(p,a) * wm_interm_7_triplet_pt4(a,i)
term(1181) = term(1181) + s1(q,i) * wm_interm_15_triplet_pt4(a,i) * wm_interm_98_triplet_pt4(p,a)
term(1182) = term(1182) + s1(q,i) * wm_interm_15_triplet_pt4(a,i) * wm_interm_99_triplet_pt4(p,a)
term(1183) = term(1183) + s1(q,i) * wm_interm_100_triplet_pt4(p,a) * wm_interm_15_triplet_pt4(a,i)
term(1184) = term(1184) + r1(vrdav_Rl, q,i) * wm_interm_134_triplet_pt4(p,a) * wm_interm_6_triplet_pt4(a,i)
term(1185) = term(1185) + r1(vrdav_Rl, q,i) * wm_interm_135_triplet_pt4(p,a) * wm_interm_6_triplet_pt4(a,i)
term(1186) = term(1186) + r1(vrdav_Rl, q,i) * wm_interm_134_triplet_pt4(p,a) * wm_interm_7_triplet_pt4(a,i)
term(1187) = term(1187) + r1(vrdav_Rl, q,i) * wm_interm_135_triplet_pt4(p,a) * wm_interm_7_triplet_pt4(a,i)
term(1188) = term(1188) + s1(q,i) * wm_interm_134_triplet_pt4(p,a) * wm_interm_15_triplet_pt4(a,i)
term(1189) = term(1189) + s1(q,i) * wm_interm_135_triplet_pt4(p,a) * wm_interm_15_triplet_pt4(a,i)
term(1190) = term(1190) + s1(q,i) * wm_interm_150_triplet_pt4(p,a) * wm_interm_15_triplet_pt4(a,i)
term(1191) = term(1191) + r1(vrdav_Rr, p,i) * wm_interm_18_triplet_pt4(q,a) * wm_interm_53_triplet_pt4(a,i)
term(1192) = term(1192) + r1(vrdav_Rr, p,i) * wm_interm_19_triplet_pt4(q,a) * wm_interm_53_triplet_pt4(a,i)
term(1193) = term(1193) + r1(vrdav_Rr, p,i) * wm_interm_18_triplet_pt4(q,a) * wm_interm_54_triplet_pt4(a,i)
term(1194) = term(1194) + r1(vrdav_Rr, p,i) * wm_interm_19_triplet_pt4(q,a) * wm_interm_54_triplet_pt4(a,i)
term(1195) = term(1195) + r1(vrdav_Rr, p,i) * wm_interm_18_triplet_pt4(q,a) * wm_interm_55_triplet_pt4(a,i)
term(1196) = term(1196) + r1(vrdav_Rr, p,i) * wm_interm_19_triplet_pt4(q,a) * wm_interm_55_triplet_pt4(a,i)
term(1197) = term(1197) + t1(q,i) * wm_interm_18_triplet_pt4(p,a) * wm_interm_50_triplet_pt4(a,i)
term(1198) = term(1198) + t1(q,i) * wm_interm_19_triplet_pt4(p,a) * wm_interm_50_triplet_pt4(a,i)
term(1199) = term(1199) + t1(q,i) * wm_interm_18_triplet_pt4(p,a) * wm_interm_48_triplet_pt4(a,i)
term(1200) = term(1200) + t1(q,i) * wm_interm_19_triplet_pt4(p,a) * wm_interm_48_triplet_pt4(a,i)
term(1201) = term(1201) + t1(q,i) * wm_interm_18_triplet_pt4(p,a) * wm_interm_46_triplet_pt4(a,i)
term(1202) = term(1202) + t1(q,i) * wm_interm_19_triplet_pt4(p,a) * wm_interm_46_triplet_pt4(a,i)
term(1203) = term(1203) + t1(q,i) * wm_interm_2_triplet_pt4(a,i) * wm_interm_70_triplet_pt4(p,a)
term(1204) = term(1204) + t1(q,i) * wm_interm_2_triplet_pt4(a,i) * wm_interm_75_triplet_pt4(p,a)
term(1205) = term(1205) + t1(q,i) * wm_interm_2_triplet_pt4(a,i) * wm_interm_77_triplet_pt4(p,a)
term(1206) = term(1206) + r1(vrdav_Rr, p,i) * wm_interm_18_triplet_pt4(q,a) * wm_interm_64_triplet_pt4(a,i)
term(1207) = term(1207) + r1(vrdav_Rr, p,i) * wm_interm_19_triplet_pt4(q,a) * wm_interm_64_triplet_pt4(a,i)
term(1208) = term(1208) + r1(vrdav_Rr, p,i) * wm_interm_18_triplet_pt4(q,a) * wm_interm_65_triplet_pt4(a,i)
term(1209) = term(1209) + r1(vrdav_Rr, p,i) * wm_interm_19_triplet_pt4(q,a) * wm_interm_65_triplet_pt4(a,i)
term(1210) = term(1210) + t1(q,i) * wm_interm_18_triplet_pt4(p,a) * wm_interm_58_triplet_pt4(a,i)
term(1211) = term(1211) + t1(q,i) * wm_interm_19_triplet_pt4(p,a) * wm_interm_58_triplet_pt4(a,i)
term(1212) = term(1212) + t1(q,i) * wm_interm_18_triplet_pt4(p,a) * wm_interm_60_triplet_pt4(a,i)
term(1213) = term(1213) + t1(q,i) * wm_interm_19_triplet_pt4(p,a) * wm_interm_60_triplet_pt4(a,i)
term(1214) = term(1214) + t1(q,i) * wm_interm_160_triplet_pt4(p,a) * wm_interm_2_triplet_pt4(a,i)
term(1215) = term(1215) + t1(q,i) * wm_interm_164_triplet_pt4(p,a) * wm_interm_2_triplet_pt4(a,i)
end do 
end do 

term(1165) = term(1165) * (2.0d+0) 
term(1166) = term(1166) * (-4.0d+0) 
term(1167) = term(1167) * (-1.0d+0) 
term(1168) = term(1168) * (2.0d+0) 
term(1169) = term(1169) * (-1.0d+0) 
term(1170) = term(1170) * (2.0d+0) 
term(1171) = term(1171) * (4.0d+0) 
term(1172) = term(1172) * (-8.0d+0) 
term(1173) = term(1173) * (-4.0d+0) 
term(1174) = term(1174) * (8.0d+0) 
term(1175) = term(1175) * (4.0d+0) 
term(1176) = term(1176) * (-8.0d+0) 
term(1177) = term(1177) * (4.0d+0) 
term(1178) = term(1178) * (-2.0d+0) 
term(1179) = term(1179) * (4.0d+0) 
term(1180) = term(1180) * (-2.0d+0) 
term(1181) = term(1181) * (-2.0d+0) 
term(1182) = term(1182) * (4.0d+0) 
term(1183) = term(1183) * (-2.0d+0) 
term(1184) = term(1184) * (16.0d+0) 
term(1185) = term(1185) * (-16.0d+0) 
term(1186) = term(1186) * (-8.0d+0) 
term(1187) = term(1187) * (8.0d+0) 
term(1188) = term(1188) * (-4.0d+0) 
term(1189) = term(1189) * (8.0d+0) 
term(1190) = term(1190) * (-4.0d+0) 
term(1191) = term(1191) * (-4.0d+0) 
term(1192) = term(1192) * (8.0d+0) 
term(1193) = term(1193) * (2.0d+0) 
term(1194) = term(1194) * (-4.0d+0) 
term(1195) = term(1195) * (2.0d+0) 
term(1196) = term(1196) * (-4.0d+0) 
term(1198) = term(1198) * (-2.0d+0) 
term(1199) = term(1199) * (-2.0d+0) 
term(1200) = term(1200) * (4.0d+0) 
term(1202) = term(1202) * (-2.0d+0) 
term(1204) = term(1204) * (-2.0d+0) 
term(1206) = term(1206) * (-8.0d+0) 
term(1207) = term(1207) * (16.0d+0) 
term(1208) = term(1208) * (8.0d+0) 
term(1209) = term(1209) * (-16.0d+0) 
term(1210) = term(1210) * (4.0d+0) 
term(1211) = term(1211) * (-8.0d+0) 
term(1212) = term(1212) * (-4.0d+0) 
term(1213) = term(1213) * (8.0d+0) 
term(1214) = term(1214) * (4.0d+0) 
term(1215) = term(1215) * (-4.0d+0) 

do i = 1, nocc 
term(1216) = term(1216) + wm_interm_33_triplet_pt4(p,i) * wm_interm_53_triplet_pt4(q,i)
term(1217) = term(1217) + wm_interm_36_triplet_pt4(p,i) * wm_interm_53_triplet_pt4(q,i)
term(1218) = term(1218) + wm_interm_38_triplet_pt4(p,i) * wm_interm_53_triplet_pt4(q,i)
term(1219) = term(1219) + wm_interm_33_triplet_pt4(p,i) * wm_interm_54_triplet_pt4(q,i)
term(1220) = term(1220) + wm_interm_36_triplet_pt4(p,i) * wm_interm_54_triplet_pt4(q,i)
term(1221) = term(1221) + wm_interm_38_triplet_pt4(p,i) * wm_interm_54_triplet_pt4(q,i)
term(1222) = term(1222) + wm_interm_33_triplet_pt4(p,i) * wm_interm_55_triplet_pt4(q,i)
term(1223) = term(1223) + wm_interm_36_triplet_pt4(p,i) * wm_interm_55_triplet_pt4(q,i)
term(1224) = term(1224) + wm_interm_38_triplet_pt4(p,i) * wm_interm_55_triplet_pt4(q,i)
term(1225) = term(1225) + wm_interm_40_triplet_pt4(p,i) * wm_interm_53_triplet_pt4(q,i)
term(1226) = term(1226) + wm_interm_42_triplet_pt4(p,i) * wm_interm_53_triplet_pt4(q,i)
term(1227) = term(1227) + wm_interm_40_triplet_pt4(p,i) * wm_interm_54_triplet_pt4(q,i)
term(1228) = term(1228) + wm_interm_42_triplet_pt4(p,i) * wm_interm_54_triplet_pt4(q,i)
term(1229) = term(1229) + wm_interm_40_triplet_pt4(p,i) * wm_interm_55_triplet_pt4(q,i)
term(1230) = term(1230) + wm_interm_42_triplet_pt4(p,i) * wm_interm_55_triplet_pt4(q,i)
term(1231) = term(1231) + wm_interm_33_triplet_pt4(p,i) * wm_interm_64_triplet_pt4(q,i)
term(1232) = term(1232) + wm_interm_36_triplet_pt4(p,i) * wm_interm_64_triplet_pt4(q,i)
term(1233) = term(1233) + wm_interm_38_triplet_pt4(p,i) * wm_interm_64_triplet_pt4(q,i)
term(1234) = term(1234) + wm_interm_33_triplet_pt4(p,i) * wm_interm_65_triplet_pt4(q,i)
term(1235) = term(1235) + wm_interm_36_triplet_pt4(p,i) * wm_interm_65_triplet_pt4(q,i)
term(1236) = term(1236) + wm_interm_38_triplet_pt4(p,i) * wm_interm_65_triplet_pt4(q,i)
term(1237) = term(1237) + wm_interm_40_triplet_pt4(p,i) * wm_interm_64_triplet_pt4(q,i)
term(1238) = term(1238) + wm_interm_42_triplet_pt4(p,i) * wm_interm_64_triplet_pt4(q,i)
term(1239) = term(1239) + wm_interm_40_triplet_pt4(p,i) * wm_interm_65_triplet_pt4(q,i)
term(1240) = term(1240) + wm_interm_42_triplet_pt4(p,i) * wm_interm_65_triplet_pt4(q,i)
end do 

term(1216) = term(1216) * (-2.0d+0) 
term(1220) = term(1220) * (-0.5d+0) 
term(1221) = term(1221) * (-0.5d+0) 
term(1223) = term(1223) * (-0.5d+0) 
term(1224) = term(1224) * (-0.5d+0) 
term(1225) = term(1225) * (-4.0d+0) 
term(1226) = term(1226) * (4.0d+0) 
term(1227) = term(1227) * (2.0d+0) 
term(1228) = term(1228) * (-2.0d+0) 
term(1229) = term(1229) * (2.0d+0) 
term(1230) = term(1230) * (-2.0d+0) 
term(1231) = term(1231) * (-4.0d+0) 
term(1232) = term(1232) * (2.0d+0) 
term(1233) = term(1233) * (2.0d+0) 
term(1234) = term(1234) * (4.0d+0) 
term(1235) = term(1235) * (-2.0d+0) 
term(1236) = term(1236) * (-2.0d+0) 
term(1237) = term(1237) * (-8.0d+0) 
term(1238) = term(1238) * (8.0d+0) 
term(1239) = term(1239) * (8.0d+0) 
term(1240) = term(1240) * (-8.0d+0) 


    calc_D_vv_wm_triplet_pt4 = zero
    do s = 0, 1240
    calc_D_vv_wm_triplet_pt4 = calc_D_vv_wm_triplet_pt4 + term(s)
    end do

    end function calc_D_vv_wm_triplet_pt4
    
    
  end module tt_ccsd_pt4a
