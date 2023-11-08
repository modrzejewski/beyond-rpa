module tt_ccsd_pt012
 use ccsd_transformed_integrals
    use cc3_intermediates
    use arithmetic
    use s_gen
    use basis
    use eom_vectors
        !
    ! File generated automatically on 2018-04-18 11:08:29
    !

        real(F64), dimension(:, :), allocatable :: wm_interm_0_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_1_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_2_triplet_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_3_triplet_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_4_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_5_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_6_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_7_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_8_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_9_triplet_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_10_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_11_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_12_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_13_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_14_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_15_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_16_triplet_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_17_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_18_triplet_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_19_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_20_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_21_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_22_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_23_triplet_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_24_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_25_triplet_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_26_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_27_triplet_pt1 

    real(F64), dimension(:, :, :, :), allocatable :: wm_interm_0_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_1_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_2_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_3_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_4_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_5_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_6_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_7_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_8_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_9_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_10_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_11_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_12_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_13_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_14_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_15_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_16_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_17_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_18_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_19_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_20_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_21_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_22_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_23_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_24_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_25_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_26_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_27_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_28_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_29_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_30_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_31_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_32_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_33_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_34_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_35_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_36_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_37_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_38_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_39_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_40_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_41_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_42_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_43_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_44_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_45_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_46_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_47_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_48_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_49_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_50_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_51_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_52_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_53_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_54_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_55_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_56_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_57_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_58_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_59_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_60_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_61_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_62_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_63_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_64_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_65_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_66_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_67_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_68_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_69_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_70_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_71_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_72_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_73_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_74_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_75_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_76_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_77_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_78_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_79_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_80_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_81_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_82_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_83_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_84_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_85_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_86_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_87_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_88_triplet_pt2 
real(F64) :: wm_interm_89_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_90_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_91_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_92_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_93_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_94_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_95_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_96_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_97_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_98_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_99_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_100_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_101_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_102_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_103_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_104_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_105_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_106_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_107_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_108_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_109_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_110_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_111_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_112_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_113_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_114_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_115_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_116_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_117_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_118_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_119_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_120_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_121_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_122_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_123_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_124_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_125_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_126_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_127_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_128_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_129_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_130_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_131_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_132_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_133_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_134_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_135_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_136_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_137_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_138_triplet_pt2 
real(F64) :: wm_interm_139_triplet_pt2 
real(F64) :: wm_interm_140_triplet_pt2 
real(F64) :: wm_interm_141_triplet_pt2 
real(F64) :: wm_interm_142_triplet_pt2 
real(F64) :: wm_interm_143_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_144_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_145_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_146_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_147_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_148_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_149_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_150_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_151_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_152_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_153_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_154_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_155_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_156_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_157_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_158_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_159_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_160_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_161_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_162_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_163_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_164_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_165_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_166_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_167_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_168_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_169_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_170_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_171_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_172_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_173_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_174_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_175_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_176_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_177_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_178_triplet_pt2 
real(F64) :: wm_interm_179_triplet_pt2 
real(F64) :: wm_interm_180_triplet_pt2 
real(F64) :: wm_interm_181_triplet_pt2 
real(F64) :: wm_interm_182_triplet_pt2 
real(F64) :: wm_interm_183_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_184_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_185_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_186_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_187_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_188_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_189_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_190_triplet_pt2 

    
    contains
    
    subroutine wm_triplet_intermediates_ccsd_init_pt0(nocc, nactive)
    integer, intent(in) :: nocc
    integer, intent(in) :: nactive
    
    end subroutine wm_triplet_intermediates_ccsd_init_pt0
    
    subroutine wm_triplet_intermediates_ccsd_free_pt0
    
    end subroutine wm_triplet_intermediates_ccsd_free_pt0
    
    subroutine wm_triplet_intermediates_ccsd_pt0(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr)
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    real(F64) :: sum

    

  end subroutine wm_triplet_intermediates_ccsd_pt0

      subroutine wm_triplet_intermediates_ccsd_init_pt1(nocc, nactive)
    integer, intent(in) :: nocc
    integer, intent(in) :: nactive
    allocate(wm_interm_0_triplet_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_1_triplet_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_2_triplet_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_3_triplet_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_4_triplet_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_5_triplet_pt1(1: nocc, 1: nocc))
allocate(wm_interm_6_triplet_pt1(1: nocc, 1: nocc))
allocate(wm_interm_7_triplet_pt1(1: nocc, 1: nocc))
allocate(wm_interm_8_triplet_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_9_triplet_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_10_triplet_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_11_triplet_pt1(1: nocc, 1: nocc))
allocate(wm_interm_12_triplet_pt1(1: nocc, 1: nocc))
allocate(wm_interm_13_triplet_pt1(1: nocc, 1: nocc))
allocate(wm_interm_14_triplet_pt1(1: nocc, 1: nocc))
allocate(wm_interm_15_triplet_pt1(1: nocc, 1: nocc))
allocate(wm_interm_16_triplet_pt1(1: nocc, 1: nocc))
allocate(wm_interm_17_triplet_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_18_triplet_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_19_triplet_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_20_triplet_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_21_triplet_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_22_triplet_pt1(1: nocc, 1: nocc))
allocate(wm_interm_23_triplet_pt1(1: nocc, 1: nocc))
allocate(wm_interm_24_triplet_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_25_triplet_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_26_triplet_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_27_triplet_pt1(nocc+1: nactive, 1: nocc))
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

    end subroutine wm_triplet_intermediates_ccsd_init_pt1
    
    subroutine wm_triplet_intermediates_ccsd_free_pt1
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

    end subroutine wm_triplet_intermediates_ccsd_free_pt1
    
    subroutine wm_triplet_intermediates_ccsd_pt1(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr)
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
wm_interm_0_triplet_pt1(b, j) = wm_interm_0_triplet_pt1(b, j) + sum 
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
wm_interm_1_triplet_pt1(b, j) = wm_interm_1_triplet_pt1(b, j) + sum 
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
wm_interm_2_triplet_pt1(b, j) = wm_interm_2_triplet_pt1(b, j) + sum 
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
wm_interm_3_triplet_pt1(b, i, j, k) = wm_interm_3_triplet_pt1(b, i, j, k) + sum 
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
wm_interm_4_triplet_pt1(b, i, j, k) = wm_interm_4_triplet_pt1(b, i, j, k) + sum 
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
wm_interm_5_triplet_pt1(j, k) = wm_interm_5_triplet_pt1(j, k) + sum 
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
wm_interm_6_triplet_pt1(j, k) = wm_interm_6_triplet_pt1(j, k) + sum 
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
wm_interm_7_triplet_pt1(j, k) = wm_interm_7_triplet_pt1(j, k) + sum 
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
wm_interm_8_triplet_pt1(b, j) = wm_interm_8_triplet_pt1(b, j) + sum 
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
wm_interm_9_triplet_pt1(b, j) = wm_interm_9_triplet_pt1(b, j) + sum 
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
wm_interm_10_triplet_pt1(b, i, j, k) = wm_interm_10_triplet_pt1(b, i, j, k) + sum 
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
wm_interm_11_triplet_pt1(j, k) = wm_interm_11_triplet_pt1(j, k) + sum 
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
wm_interm_12_triplet_pt1(j, k) = wm_interm_12_triplet_pt1(j, k) + sum 
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
wm_interm_13_triplet_pt1(j, k) = wm_interm_13_triplet_pt1(j, k) + sum 
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
wm_interm_14_triplet_pt1(j, k) = wm_interm_14_triplet_pt1(j, k) + sum 
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
wm_interm_15_triplet_pt1(j, k) = wm_interm_15_triplet_pt1(j, k) + sum 
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
wm_interm_16_triplet_pt1(j, k) = wm_interm_16_triplet_pt1(j, k) + sum 
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
wm_interm_17_triplet_pt1(b, i, j, k) = wm_interm_17_triplet_pt1(b, i, j, k) + sum 
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
wm_interm_18_triplet_pt1(b, j) = wm_interm_18_triplet_pt1(b, j) + sum 
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
wm_interm_19_triplet_pt1(b, i, j, k) = wm_interm_19_triplet_pt1(b, i, j, k) + sum 
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
wm_interm_20_triplet_pt1(b, j) = wm_interm_20_triplet_pt1(b, j) + sum 
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
wm_interm_21_triplet_pt1(b, j) = wm_interm_21_triplet_pt1(b, j) + sum 
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
wm_interm_22_triplet_pt1(j, k) = wm_interm_22_triplet_pt1(j, k) + sum 
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
wm_interm_23_triplet_pt1(j, k) = wm_interm_23_triplet_pt1(j, k) + sum 
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
wm_interm_24_triplet_pt1(b, j, i, k) = wm_interm_24_triplet_pt1(b, j, i, k) + sum 
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
wm_interm_25_triplet_pt1(b, j) = wm_interm_25_triplet_pt1(b, j) + sum 
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
wm_interm_26_triplet_pt1(b, i, j, k) = wm_interm_26_triplet_pt1(b, i, j, k) + sum 
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
wm_interm_27_triplet_pt1(b, j) = wm_interm_27_triplet_pt1(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 



    end subroutine wm_triplet_intermediates_ccsd_pt1
    
    

    subroutine wm_triplet_intermediates_ccsd_init_pt2(nocc, nactive)
    integer, intent(in) :: nocc
    integer, intent(in) :: nactive
    allocate(wm_interm_0_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_1_triplet_pt2(1: nocc, 1: nocc))
allocate(wm_interm_2_triplet_pt2(1: nocc, 1: nocc))
allocate(wm_interm_3_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_4_triplet_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_5_triplet_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_6_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_7_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_8_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_9_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_10_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_11_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_12_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_13_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_14_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_15_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_16_triplet_pt2(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_17_triplet_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_18_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_19_triplet_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_20_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_21_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_22_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_23_triplet_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_24_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_25_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_26_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_27_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_28_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_29_triplet_pt2(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_30_triplet_pt2(1: nocc, 1: nocc))
allocate(wm_interm_31_triplet_pt2(1: nocc, 1: nocc))
allocate(wm_interm_32_triplet_pt2(1: nocc, 1: nocc))
allocate(wm_interm_33_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_34_triplet_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_35_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_36_triplet_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_37_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_38_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_39_triplet_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_40_triplet_pt2(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_41_triplet_pt2(1: nocc, 1: nocc))
allocate(wm_interm_42_triplet_pt2(1: nocc, 1: nocc))
allocate(wm_interm_43_triplet_pt2(1: nocc, 1: nocc))
allocate(wm_interm_44_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_45_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_46_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_47_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_48_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_49_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_50_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_51_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_52_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_53_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_54_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_55_triplet_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_56_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_57_triplet_pt2(1: nocc, 1: nocc))
allocate(wm_interm_58_triplet_pt2(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_59_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_60_triplet_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_61_triplet_pt2(1: nocc, 1: nocc))
allocate(wm_interm_62_triplet_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_63_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_64_triplet_pt2(1: nocc, 1: nocc))
allocate(wm_interm_65_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_66_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_67_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_68_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_69_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_70_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_71_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_72_triplet_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_73_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_74_triplet_pt2(1: nocc, 1: nocc))
allocate(wm_interm_75_triplet_pt2(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_76_triplet_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_77_triplet_pt2(1: nocc, 1: nocc))
allocate(wm_interm_78_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_79_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_80_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_81_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_82_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_83_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_84_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_85_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_86_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_87_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_88_triplet_pt2(1: nocc, 1: nocc))
allocate(wm_interm_90_triplet_pt2(1: nocc, 1: nocc))
allocate(wm_interm_91_triplet_pt2(1: nocc, 1: nocc))
allocate(wm_interm_92_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_93_triplet_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_94_triplet_pt2(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_95_triplet_pt2(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_96_triplet_pt2(1: nocc, 1: nocc))
allocate(wm_interm_97_triplet_pt2(1: nocc, 1: nocc))
allocate(wm_interm_98_triplet_pt2(1: nocc, 1: nocc))
allocate(wm_interm_99_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_100_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_101_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_102_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_103_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_104_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_105_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_106_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_107_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_108_triplet_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_109_triplet_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_110_triplet_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_111_triplet_pt2(1: nocc, 1: nocc))
allocate(wm_interm_112_triplet_pt2(1: nocc, 1: nocc))
allocate(wm_interm_113_triplet_pt2(1: nocc, 1: nocc))
allocate(wm_interm_114_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_115_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_116_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_117_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_118_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_119_triplet_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_120_triplet_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_121_triplet_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_122_triplet_pt2(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_123_triplet_pt2(1: nocc, 1: nocc))
allocate(wm_interm_124_triplet_pt2(1: nocc, 1: nocc))
allocate(wm_interm_125_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_126_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_127_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_128_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_129_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_130_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_131_triplet_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_132_triplet_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_133_triplet_pt2(1: nocc, 1: nocc))
allocate(wm_interm_134_triplet_pt2(1: nocc, 1: nocc))
allocate(wm_interm_135_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_136_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_137_triplet_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_138_triplet_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_144_triplet_pt2(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_145_triplet_pt2(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_146_triplet_pt2(1: nocc, 1: nocc))
allocate(wm_interm_147_triplet_pt2(1: nocc, 1: nocc))
allocate(wm_interm_148_triplet_pt2(1: nocc, 1: nocc))
allocate(wm_interm_149_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_150_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_151_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_152_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_153_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_154_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_155_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_156_triplet_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_157_triplet_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_158_triplet_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_159_triplet_pt2(1: nocc, 1: nocc))
allocate(wm_interm_160_triplet_pt2(1: nocc, 1: nocc))
allocate(wm_interm_161_triplet_pt2(1: nocc, 1: nocc))
allocate(wm_interm_162_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_163_triplet_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_164_triplet_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_165_triplet_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_166_triplet_pt2(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_167_triplet_pt2(1: nocc, 1: nocc))
allocate(wm_interm_168_triplet_pt2(1: nocc, 1: nocc))
allocate(wm_interm_169_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_170_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_171_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_172_triplet_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_173_triplet_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_174_triplet_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_175_triplet_pt2(1: nocc, 1: nocc))
allocate(wm_interm_176_triplet_pt2(1: nocc, 1: nocc))
allocate(wm_interm_177_triplet_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_178_triplet_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_184_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_185_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_186_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_187_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_188_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_189_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_190_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
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
wm_interm_151_triplet_pt2 = zero 
wm_interm_152_triplet_pt2 = zero 
wm_interm_153_triplet_pt2 = zero 
wm_interm_154_triplet_pt2 = zero 
wm_interm_155_triplet_pt2 = zero 
wm_interm_156_triplet_pt2 = zero 
wm_interm_157_triplet_pt2 = zero 
wm_interm_158_triplet_pt2 = zero 
wm_interm_159_triplet_pt2 = zero 
wm_interm_160_triplet_pt2 = zero 
wm_interm_161_triplet_pt2 = zero 
wm_interm_162_triplet_pt2 = zero 
wm_interm_163_triplet_pt2 = zero 
wm_interm_164_triplet_pt2 = zero 
wm_interm_165_triplet_pt2 = zero 
wm_interm_166_triplet_pt2 = zero 
wm_interm_167_triplet_pt2 = zero 
wm_interm_168_triplet_pt2 = zero 
wm_interm_169_triplet_pt2 = zero 
wm_interm_170_triplet_pt2 = zero 
wm_interm_171_triplet_pt2 = zero 
wm_interm_172_triplet_pt2 = zero 
wm_interm_173_triplet_pt2 = zero 
wm_interm_174_triplet_pt2 = zero 
wm_interm_175_triplet_pt2 = zero 
wm_interm_176_triplet_pt2 = zero 
wm_interm_177_triplet_pt2 = zero 
wm_interm_178_triplet_pt2 = zero 
wm_interm_179_triplet_pt2 = zero 
wm_interm_180_triplet_pt2 = zero 
wm_interm_181_triplet_pt2 = zero 
wm_interm_182_triplet_pt2 = zero 
wm_interm_183_triplet_pt2 = zero 
wm_interm_184_triplet_pt2 = zero 
wm_interm_185_triplet_pt2 = zero 
wm_interm_186_triplet_pt2 = zero 
wm_interm_187_triplet_pt2 = zero 
wm_interm_188_triplet_pt2 = zero 
wm_interm_189_triplet_pt2 = zero 
wm_interm_190_triplet_pt2 = zero 

    end subroutine wm_triplet_intermediates_ccsd_init_pt2
    
    subroutine wm_triplet_intermediates_ccsd_free_pt2
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
deallocate(wm_interm_144_triplet_pt2)
deallocate(wm_interm_145_triplet_pt2)
deallocate(wm_interm_146_triplet_pt2)
deallocate(wm_interm_147_triplet_pt2)
deallocate(wm_interm_148_triplet_pt2)
deallocate(wm_interm_149_triplet_pt2)
deallocate(wm_interm_150_triplet_pt2)
deallocate(wm_interm_151_triplet_pt2)
deallocate(wm_interm_152_triplet_pt2)
deallocate(wm_interm_153_triplet_pt2)
deallocate(wm_interm_154_triplet_pt2)
deallocate(wm_interm_155_triplet_pt2)
deallocate(wm_interm_156_triplet_pt2)
deallocate(wm_interm_157_triplet_pt2)
deallocate(wm_interm_158_triplet_pt2)
deallocate(wm_interm_159_triplet_pt2)
deallocate(wm_interm_160_triplet_pt2)
deallocate(wm_interm_161_triplet_pt2)
deallocate(wm_interm_162_triplet_pt2)
deallocate(wm_interm_163_triplet_pt2)
deallocate(wm_interm_164_triplet_pt2)
deallocate(wm_interm_165_triplet_pt2)
deallocate(wm_interm_166_triplet_pt2)
deallocate(wm_interm_167_triplet_pt2)
deallocate(wm_interm_168_triplet_pt2)
deallocate(wm_interm_169_triplet_pt2)
deallocate(wm_interm_170_triplet_pt2)
deallocate(wm_interm_171_triplet_pt2)
deallocate(wm_interm_172_triplet_pt2)
deallocate(wm_interm_173_triplet_pt2)
deallocate(wm_interm_174_triplet_pt2)
deallocate(wm_interm_175_triplet_pt2)
deallocate(wm_interm_176_triplet_pt2)
deallocate(wm_interm_177_triplet_pt2)
deallocate(wm_interm_178_triplet_pt2)
deallocate(wm_interm_184_triplet_pt2)
deallocate(wm_interm_185_triplet_pt2)
deallocate(wm_interm_186_triplet_pt2)
deallocate(wm_interm_187_triplet_pt2)
deallocate(wm_interm_188_triplet_pt2)
deallocate(wm_interm_189_triplet_pt2)
deallocate(wm_interm_190_triplet_pt2)

    end subroutine wm_triplet_intermediates_ccsd_free_pt2
    
    subroutine wm_triplet_intermediates_ccsd_pt2(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr)
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
wm_interm_0_triplet_pt2(b, i, j, k) = wm_interm_0_triplet_pt2(b, i, j, k) + sum 
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
wm_interm_3_triplet_pt2(b, i, j, k) = wm_interm_3_triplet_pt2(b, i, j, k) + sum 
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
wm_interm_4_triplet_pt2(b, c) = wm_interm_4_triplet_pt2(b, c) + sum 
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
wm_interm_5_triplet_pt2(b, c) = wm_interm_5_triplet_pt2(b, c) + sum 
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
wm_interm_6_triplet_pt2(b, j) = wm_interm_6_triplet_pt2(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_7_triplet_pt2(b, c, j, k) = wm_interm_7_triplet_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_8_triplet_pt2(b, c, j, k) = wm_interm_8_triplet_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_9_triplet_pt2(b, c, j, k) = wm_interm_9_triplet_pt2(b, c, j, k) + sum 
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
wm_interm_10_triplet_pt2(b, i, j, k) = wm_interm_10_triplet_pt2(b, i, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_11_triplet_pt2(b, c, j, k) = wm_interm_11_triplet_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_12_triplet_pt2(b, c, j, k) = wm_interm_12_triplet_pt2(b, c, j, k) + sum 
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
wm_interm_13_triplet_pt2(b, j) = wm_interm_13_triplet_pt2(b, j) + sum 
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
wm_interm_14_triplet_pt2(b, i, j, k) = wm_interm_14_triplet_pt2(b, i, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_15_triplet_pt2(b, c, j, k) = wm_interm_15_triplet_pt2(b, c, j, k) + sum 
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
wm_interm_17_triplet_pt2(b, c) = wm_interm_17_triplet_pt2(b, c) + sum 
end do 
end do 
!$omp end do nowait 
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
wm_interm_18_triplet_pt2(b, c, j, k) = wm_interm_18_triplet_pt2(b, c, j, k) + sum 
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
sum = sum + r2p(vrdav_Rr, b,i,a,j) * s2(a,c,j,i)
end do 
end do 
end do 
wm_interm_19_triplet_pt2(b, c) = wm_interm_19_triplet_pt2(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_20_triplet_pt2(b, c, j, k) = wm_interm_20_triplet_pt2(b, c, j, k) + sum 
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
sum = sum + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, a,j,b,i)
end do 
end do 
wm_interm_21_triplet_pt2(b, j) = wm_interm_21_triplet_pt2(b, j) + sum 
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
wm_interm_22_triplet_pt2(b, j) = wm_interm_22_triplet_pt2(b, j) + sum 
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
wm_interm_23_triplet_pt2(b, c) = wm_interm_23_triplet_pt2(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_24_triplet_pt2(b, c, j, k) = wm_interm_24_triplet_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_25_triplet_pt2(b, c, j, k) = wm_interm_25_triplet_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_26_triplet_pt2(b, c, j, k) = wm_interm_26_triplet_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_27_triplet_pt2(b, c, j, k) = wm_interm_27_triplet_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_28_triplet_pt2(b, c, j, k) = wm_interm_28_triplet_pt2(b, c, j, k) + sum 
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
wm_interm_29_triplet_pt2(i, j, k, l) = wm_interm_29_triplet_pt2(i, j, k, l) + sum 
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
wm_interm_30_triplet_pt2(j, k) = wm_interm_30_triplet_pt2(j, k) + sum 
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
wm_interm_31_triplet_pt2(j, k) = wm_interm_31_triplet_pt2(j, k) + sum 
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
wm_interm_32_triplet_pt2(j, k) = wm_interm_32_triplet_pt2(j, k) + sum 
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
wm_interm_33_triplet_pt2(b, j) = wm_interm_33_triplet_pt2(b, j) + sum 
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
wm_interm_34_triplet_pt2(b, c) = wm_interm_34_triplet_pt2(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_35_triplet_pt2(b, c, j, k) = wm_interm_35_triplet_pt2(b, c, j, k) + sum 
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
sum = sum + r2m(vrdav_Rr, a,j,b,i) * s2(a,c,j,i)
end do 
end do 
end do 
wm_interm_36_triplet_pt2(b, c) = wm_interm_36_triplet_pt2(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_37_triplet_pt2(b, c, j, k) = wm_interm_37_triplet_pt2(b, c, j, k) + sum 
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
sum = sum + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, a,i,b,j)
end do 
end do 
wm_interm_38_triplet_pt2(b, j) = wm_interm_38_triplet_pt2(b, j) + sum 
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
wm_interm_39_triplet_pt2(b, c) = wm_interm_39_triplet_pt2(b, c) + sum 
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
wm_interm_40_triplet_pt2(i, j, k, l) = wm_interm_40_triplet_pt2(i, j, k, l) + sum 
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
wm_interm_41_triplet_pt2(j, k) = wm_interm_41_triplet_pt2(j, k) + sum 
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
wm_interm_42_triplet_pt2(j, k) = wm_interm_42_triplet_pt2(j, k) + sum 
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
wm_interm_43_triplet_pt2(j, k) = wm_interm_43_triplet_pt2(j, k) + sum 
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
wm_interm_44_triplet_pt2(b, j) = wm_interm_44_triplet_pt2(b, j) + sum 
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
wm_interm_45_triplet_pt2(b, i, j, k) = wm_interm_45_triplet_pt2(b, i, j, k) + sum 
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
wm_interm_46_triplet_pt2(b, j) = wm_interm_46_triplet_pt2(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_47_triplet_pt2(b, c, j, k) = wm_interm_47_triplet_pt2(b, c, j, k) + sum 
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
wm_interm_48_triplet_pt2(b, i, j, k) = wm_interm_48_triplet_pt2(b, i, j, k) + sum 
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
wm_interm_49_triplet_pt2(b, j) = wm_interm_49_triplet_pt2(b, j) + sum 
end do 
end do 
!$omp end do nowait 
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
wm_interm_50_triplet_pt2(b, c, j, k) = wm_interm_50_triplet_pt2(b, c, j, k) + sum 
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
sum = sum + r2p(vrdav_Rl, b,i,a,j) * r1(vrdav_Rr, a,k)
end do 
wm_interm_51_triplet_pt2(b, i, j, k) = wm_interm_51_triplet_pt2(b, i, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
sum = sum + r2p(vrdav_Rl, a,j,b,i) * t2(a,c,k,i)
end do 
end do 
wm_interm_53_triplet_pt2(b, c, j, k) = wm_interm_53_triplet_pt2(b, c, j, k) + sum 
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
wm_interm_54_triplet_pt2(b, j) = wm_interm_54_triplet_pt2(b, j) + sum 
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
wm_interm_55_triplet_pt2(b, c) = wm_interm_55_triplet_pt2(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_56_triplet_pt2(b, c, j, k) = wm_interm_56_triplet_pt2(b, c, j, k) + sum 
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
wm_interm_57_triplet_pt2(j, k) = wm_interm_57_triplet_pt2(j, k) + sum 
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
wm_interm_58_triplet_pt2(i, j, k, l) = wm_interm_58_triplet_pt2(i, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_59_triplet_pt2(b, c, j, k) = wm_interm_59_triplet_pt2(b, c, j, k) + sum 
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
wm_interm_60_triplet_pt2(b, c) = wm_interm_60_triplet_pt2(b, c) + sum 
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
wm_interm_61_triplet_pt2(j, k) = wm_interm_61_triplet_pt2(j, k) + sum 
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
wm_interm_62_triplet_pt2(b, c) = wm_interm_62_triplet_pt2(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_63_triplet_pt2(b, c, j, k) = wm_interm_63_triplet_pt2(b, c, j, k) + sum 
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
wm_interm_64_triplet_pt2(j, k) = wm_interm_64_triplet_pt2(j, k) + sum 
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
wm_interm_65_triplet_pt2(b, j) = wm_interm_65_triplet_pt2(b, j) + sum 
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
wm_interm_66_triplet_pt2(b, i, j, k) = wm_interm_66_triplet_pt2(b, i, j, k) + sum 
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
wm_interm_67_triplet_pt2(b, j) = wm_interm_67_triplet_pt2(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_68_triplet_pt2(b, c, j, k) = wm_interm_68_triplet_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_69_triplet_pt2(b, c, j, k) = wm_interm_69_triplet_pt2(b, c, j, k) + sum 
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
wm_interm_70_triplet_pt2(b, j, i, k) = wm_interm_70_triplet_pt2(b, j, i, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_71_triplet_pt2(b, c, j, k) = wm_interm_71_triplet_pt2(b, c, j, k) + sum 
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
wm_interm_72_triplet_pt2(b, c) = wm_interm_72_triplet_pt2(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_73_triplet_pt2(b, c, j, k) = wm_interm_73_triplet_pt2(b, c, j, k) + sum 
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
wm_interm_74_triplet_pt2(j, k) = wm_interm_74_triplet_pt2(j, k) + sum 
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
wm_interm_75_triplet_pt2(i, j, k, l) = wm_interm_75_triplet_pt2(i, j, k, l) + sum 
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
wm_interm_76_triplet_pt2(b, c) = wm_interm_76_triplet_pt2(b, c) + sum 
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
wm_interm_77_triplet_pt2(j, k) = wm_interm_77_triplet_pt2(j, k) + sum 
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
wm_interm_78_triplet_pt2(b, j) = wm_interm_78_triplet_pt2(b, j) + sum 
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
wm_interm_79_triplet_pt2(b, j) = wm_interm_79_triplet_pt2(b, j) + sum 
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
wm_interm_80_triplet_pt2(b, j) = wm_interm_80_triplet_pt2(b, j) + sum 
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
wm_interm_81_triplet_pt2(b, j) = wm_interm_81_triplet_pt2(b, j) + sum 
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
wm_interm_82_triplet_pt2(b, j) = wm_interm_82_triplet_pt2(b, j) + sum 
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
wm_interm_83_triplet_pt2(b, j) = wm_interm_83_triplet_pt2(b, j) + sum 
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
wm_interm_84_triplet_pt2(b, j) = wm_interm_84_triplet_pt2(b, j) + sum 
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
wm_interm_85_triplet_pt2(b, j) = wm_interm_85_triplet_pt2(b, j) + sum 
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
wm_interm_86_triplet_pt2(b, j) = wm_interm_86_triplet_pt2(b, j) + sum 
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
wm_interm_87_triplet_pt2(b, j) = wm_interm_87_triplet_pt2(b, j) + sum 
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
wm_interm_88_triplet_pt2(i, j) = wm_interm_88_triplet_pt2(i, j) + sum 
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
wm_interm_89_triplet_pt2 = wm_interm_89_triplet_pt2 + sum 
!$omp parallel private(a, i, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do i = 1, nocc 
do j = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rr, a,i) * s1(a,j)
end do 
wm_interm_90_triplet_pt2(i, j) = wm_interm_90_triplet_pt2(i, j) + sum 
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
wm_interm_91_triplet_pt2(i, j) = wm_interm_91_triplet_pt2(i, j) + sum 
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
wm_interm_92_triplet_pt2(a, b, i, j) = wm_interm_92_triplet_pt2(a, b, i, j) + sum 
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
wm_interm_93_triplet_pt2(a, b) = wm_interm_93_triplet_pt2(a, b) + sum 
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
wm_interm_94_triplet_pt2(i, j, k, l) = wm_interm_94_triplet_pt2(i, j, k, l) + sum 
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
wm_interm_95_triplet_pt2(i, j, k, l) = wm_interm_95_triplet_pt2(i, j, k, l) + sum 
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
wm_interm_96_triplet_pt2(j, k) = wm_interm_96_triplet_pt2(j, k) + sum 
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
wm_interm_97_triplet_pt2(j, k) = wm_interm_97_triplet_pt2(j, k) + sum 
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
wm_interm_98_triplet_pt2(j, k) = wm_interm_98_triplet_pt2(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_99_triplet_pt2(b, c, j, k) = wm_interm_99_triplet_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
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
wm_interm_100_triplet_pt2(b, c, j, k) = wm_interm_100_triplet_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
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
wm_interm_101_triplet_pt2(b, c, j, k) = wm_interm_101_triplet_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_102_triplet_pt2(b, c, j, k) = wm_interm_102_triplet_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_103_triplet_pt2(b, c, j, k) = wm_interm_103_triplet_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
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
wm_interm_104_triplet_pt2(b, c, j, k) = wm_interm_104_triplet_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_105_triplet_pt2(b, c, j, k) = wm_interm_105_triplet_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_106_triplet_pt2(b, c, j, k) = wm_interm_106_triplet_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_107_triplet_pt2(b, c, j, k) = wm_interm_107_triplet_pt2(b, c, j, k) + sum 
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
wm_interm_108_triplet_pt2(b, c) = wm_interm_108_triplet_pt2(b, c) + sum 
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
wm_interm_109_triplet_pt2(b, c) = wm_interm_109_triplet_pt2(b, c) + sum 
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
wm_interm_110_triplet_pt2(b, c) = wm_interm_110_triplet_pt2(b, c) + sum 
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
wm_interm_111_triplet_pt2(j, k) = wm_interm_111_triplet_pt2(j, k) + sum 
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
wm_interm_112_triplet_pt2(j, k) = wm_interm_112_triplet_pt2(j, k) + sum 
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
wm_interm_113_triplet_pt2(j, k) = wm_interm_113_triplet_pt2(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_114_triplet_pt2(b, c, j, k) = wm_interm_114_triplet_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_115_triplet_pt2(b, c, j, k) = wm_interm_115_triplet_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_116_triplet_pt2(b, c, j, k) = wm_interm_116_triplet_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_117_triplet_pt2(b, c, j, k) = wm_interm_117_triplet_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_118_triplet_pt2(b, c, j, k) = wm_interm_118_triplet_pt2(b, c, j, k) + sum 
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
wm_interm_119_triplet_pt2(b, c) = wm_interm_119_triplet_pt2(b, c) + sum 
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
wm_interm_120_triplet_pt2(b, c) = wm_interm_120_triplet_pt2(b, c) + sum 
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
wm_interm_121_triplet_pt2(b, c) = wm_interm_121_triplet_pt2(b, c) + sum 
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
wm_interm_122_triplet_pt2(i, j, k, l) = wm_interm_122_triplet_pt2(i, j, k, l) + sum 
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
wm_interm_123_triplet_pt2(j, k) = wm_interm_123_triplet_pt2(j, k) + sum 
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
wm_interm_124_triplet_pt2(j, k) = wm_interm_124_triplet_pt2(j, k) + sum 
end do 
end do 
!$omp end do nowait 
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
wm_interm_125_triplet_pt2(b, c, j, k) = wm_interm_125_triplet_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
sum = sum + r2p(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,k,c,i)
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
sum = sum + r2p(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,i,c,k)
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
sum = sum + r2p(vrdav_Rl, a,j,b,i) * r2m(vrdav_Rr, a,i,c,k)
end do 
end do 
wm_interm_129_triplet_pt2(b, c, j, k) = wm_interm_129_triplet_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_130_triplet_pt2(b, c, j, k) = wm_interm_130_triplet_pt2(b, c, j, k) + sum 
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
wm_interm_131_triplet_pt2(b, c) = wm_interm_131_triplet_pt2(b, c) + sum 
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
wm_interm_132_triplet_pt2(b, c) = wm_interm_132_triplet_pt2(b, c) + sum 
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
wm_interm_133_triplet_pt2(j, k) = wm_interm_133_triplet_pt2(j, k) + sum 
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
wm_interm_134_triplet_pt2(j, k) = wm_interm_134_triplet_pt2(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_135_triplet_pt2(b, c, j, k) = wm_interm_135_triplet_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_136_triplet_pt2(b, c, j, k) = wm_interm_136_triplet_pt2(b, c, j, k) + sum 
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
sum = sum + r2p(vrdav_Rl, b,i,a,j) * r2m(vrdav_Rr, a,i,c,j)
end do 
end do 
end do 
wm_interm_137_triplet_pt2(b, c) = wm_interm_137_triplet_pt2(b, c) + sum 
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
wm_interm_138_triplet_pt2(b, c) = wm_interm_138_triplet_pt2(b, c) + sum 
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
wm_interm_139_triplet_pt2 = wm_interm_139_triplet_pt2 + sum 
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
wm_interm_140_triplet_pt2 = wm_interm_140_triplet_pt2 + sum 
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
wm_interm_141_triplet_pt2 = wm_interm_141_triplet_pt2 + sum 
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
wm_interm_142_triplet_pt2 = wm_interm_142_triplet_pt2 + sum 
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
wm_interm_143_triplet_pt2 = wm_interm_143_triplet_pt2 + sum 
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
wm_interm_144_triplet_pt2(i, j, k, l) = wm_interm_144_triplet_pt2(i, j, k, l) + sum 
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
wm_interm_145_triplet_pt2(i, j, k, l) = wm_interm_145_triplet_pt2(i, j, k, l) + sum 
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
wm_interm_146_triplet_pt2(j, k) = wm_interm_146_triplet_pt2(j, k) + sum 
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
wm_interm_147_triplet_pt2(j, k) = wm_interm_147_triplet_pt2(j, k) + sum 
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
wm_interm_148_triplet_pt2(j, k) = wm_interm_148_triplet_pt2(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_149_triplet_pt2(b, c, j, k) = wm_interm_149_triplet_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_150_triplet_pt2(b, c, j, k) = wm_interm_150_triplet_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
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
wm_interm_151_triplet_pt2(b, c, j, k) = wm_interm_151_triplet_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_152_triplet_pt2(b, c, j, k) = wm_interm_152_triplet_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_153_triplet_pt2(b, c, j, k) = wm_interm_153_triplet_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_154_triplet_pt2(b, c, j, k) = wm_interm_154_triplet_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_155_triplet_pt2(b, c, j, k) = wm_interm_155_triplet_pt2(b, c, j, k) + sum 
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
wm_interm_156_triplet_pt2(b, c) = wm_interm_156_triplet_pt2(b, c) + sum 
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
wm_interm_157_triplet_pt2(b, c) = wm_interm_157_triplet_pt2(b, c) + sum 
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
wm_interm_158_triplet_pt2(b, c) = wm_interm_158_triplet_pt2(b, c) + sum 
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
wm_interm_159_triplet_pt2(j, k) = wm_interm_159_triplet_pt2(j, k) + sum 
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
wm_interm_160_triplet_pt2(j, k) = wm_interm_160_triplet_pt2(j, k) + sum 
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
wm_interm_161_triplet_pt2(j, k) = wm_interm_161_triplet_pt2(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_162_triplet_pt2(b, c, j, k) = wm_interm_162_triplet_pt2(b, c, j, k) + sum 
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
wm_interm_163_triplet_pt2(b, c) = wm_interm_163_triplet_pt2(b, c) + sum 
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
wm_interm_164_triplet_pt2(b, c) = wm_interm_164_triplet_pt2(b, c) + sum 
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
wm_interm_165_triplet_pt2(b, c) = wm_interm_165_triplet_pt2(b, c) + sum 
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
wm_interm_166_triplet_pt2(i, j, k, l) = wm_interm_166_triplet_pt2(i, j, k, l) + sum 
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
wm_interm_167_triplet_pt2(j, k) = wm_interm_167_triplet_pt2(j, k) + sum 
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
wm_interm_168_triplet_pt2(j, k) = wm_interm_168_triplet_pt2(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_169_triplet_pt2(b, c, j, k) = wm_interm_169_triplet_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_170_triplet_pt2(b, c, j, k) = wm_interm_170_triplet_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_171_triplet_pt2(b, c, j, k) = wm_interm_171_triplet_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_172_triplet_pt2(b, c, j, k) = wm_interm_172_triplet_pt2(b, c, j, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,j,c,i)
end do 
end do 
end do 
wm_interm_173_triplet_pt2(b, c) = wm_interm_173_triplet_pt2(b, c) + sum 
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
wm_interm_174_triplet_pt2(b, c) = wm_interm_174_triplet_pt2(b, c) + sum 
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
wm_interm_175_triplet_pt2(j, k) = wm_interm_175_triplet_pt2(j, k) + sum 
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
wm_interm_176_triplet_pt2(j, k) = wm_interm_176_triplet_pt2(j, k) + sum 
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
wm_interm_177_triplet_pt2(b, c) = wm_interm_177_triplet_pt2(b, c) + sum 
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
wm_interm_178_triplet_pt2(b, c) = wm_interm_178_triplet_pt2(b, c) + sum 
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
wm_interm_179_triplet_pt2 = wm_interm_179_triplet_pt2 + sum 
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
wm_interm_180_triplet_pt2 = wm_interm_180_triplet_pt2 + sum 
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
wm_interm_181_triplet_pt2 = wm_interm_181_triplet_pt2 + sum 
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
wm_interm_182_triplet_pt2 = wm_interm_182_triplet_pt2 + sum 
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
wm_interm_183_triplet_pt2 = wm_interm_183_triplet_pt2 + sum 
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
wm_interm_184_triplet_pt2(b, i, j, k) = wm_interm_184_triplet_pt2(b, i, j, k) + sum 
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
wm_interm_185_triplet_pt2(b, i, j, k) = wm_interm_185_triplet_pt2(b, i, j, k) + sum 
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
wm_interm_186_triplet_pt2(b, j, i, k) = wm_interm_186_triplet_pt2(b, j, i, k) + sum 
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
wm_interm_187_triplet_pt2(b, i, j, k) = wm_interm_187_triplet_pt2(b, i, j, k) + sum 
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
sum = sum + r2p(vrdav_Rl, b,i,a,j) * t1(a,k)
end do 
wm_interm_188_triplet_pt2(b, i, j, k) = wm_interm_188_triplet_pt2(b, i, j, k) + sum 
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
wm_interm_189_triplet_pt2(b, i, j, k) = wm_interm_189_triplet_pt2(b, i, j, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,j,b,i) * t1(a,k)
end do 
wm_interm_190_triplet_pt2(b, j, i, k) = wm_interm_190_triplet_pt2(b, j, i, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 



    end subroutine wm_triplet_intermediates_ccsd_pt2
        
    function calc_D_oo_wm_triplet_pt0(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_oo_wm_triplet_pt0
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
    real(F64), dimension(0:20) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(0) = term(0) + r2p(vrdav_Rl, a,p,b,i) * r2p(vrdav_Rr, a,i,b,q)
term(1) = term(1) + r2p(vrdav_Rl, a,p,b,i) * r2p(vrdav_Rr, a,q,b,i)
term(2) = term(2) + r2p(vrdav_Rl, a,p,b,i) * r2p(vrdav_Rr, b,q,a,i)
term(3) = term(3) + r2p(vrdav_Rl, a,i,b,p) * r2p(vrdav_Rr, a,q,b,i)
term(4) = term(4) + r2p(vrdav_Rl, a,p,b,i) * r2m(vrdav_Rr, a,i,b,q)
term(5) = term(5) + r2p(vrdav_Rl, a,p,b,i) * r2m(vrdav_Rr, a,q,b,i)
term(6) = term(6) + r2p(vrdav_Rl, a,i,b,p) * r2m(vrdav_Rr, a,q,b,i)
term(7) = term(7) + r2m(vrdav_Rl, a,p,b,i) * r2p(vrdav_Rr, a,i,b,q)
term(8) = term(8) + r2m(vrdav_Rl, a,p,b,i) * r2p(vrdav_Rr, a,q,b,i)
term(9) = term(9) + r2m(vrdav_Rl, a,p,b,i) * r2p(vrdav_Rr, b,q,a,i)
term(10) = term(10) + r2m(vrdav_Rl, a,i,b,p) * r2p(vrdav_Rr, a,q,b,i)
term(11) = term(11) + r2m(vrdav_Rl, a,p,b,i) * r2m(vrdav_Rr, a,i,b,q)
term(12) = term(12) + r2m(vrdav_Rl, a,p,b,i) * r2m(vrdav_Rr, a,q,b,i)
term(13) = term(13) + r2m(vrdav_Rl, a,i,b,p) * r2m(vrdav_Rr, a,q,b,i)
end do 
end do 
end do 

term(0) = term(0) * (0.5d+0) 
term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (0.5d+0) 
term(3) = term(3) * (0.5d+0) 
term(4) = term(4) * (2.0d+0) 
term(5) = term(5) * (-2.0d+0) 
term(6) = term(6) * (2.0d+0) 
term(8) = term(8) * (-2.0d+0) 
term(11) = term(11) * (4.0d+0) 
term(12) = term(12) * (-4.0d+0) 
term(13) = term(13) * (4.0d+0) 

do a = nocc + 1, nactive 
term(14) = term(14) + r1(vrdav_Rl, a,p) * r1(vrdav_Rr, a,q)
end do 

term(14) = term(14) * (-2.0d+0) 

do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(15) = term(15) + r2p(vrdav_Rl, a,i,b,p) * r2p(vrdav_Rr, a,i,b,q)
term(16) = term(16) + r2p(vrdav_Rl, a,i,b,p) * r2m(vrdav_Rr, a,i,b,q)
term(17) = term(17) + r2m(vrdav_Rl, a,i,b,p) * r2p(vrdav_Rr, a,i,b,q)
term(18) = term(18) + r2m(vrdav_Rl, a,i,b,p) * r2m(vrdav_Rr, a,i,b,q)
end do 
end do 
end do 

term(15) = term(15) * (-1.0d+0) 
term(16) = term(16) * (-2.0d+0) 
term(17) = term(17) * (-2.0d+0) 
term(18) = term(18) * (-4.0d+0) 

do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(19) = term(19) + r2p(vrdav_Rl, a,i,b,p) * r2p(vrdav_Rr, b,i,a,q)
term(20) = term(20) + r2m(vrdav_Rl, a,i,b,p) * r2p(vrdav_Rr, b,i,a,q)
end do 
end do 
end do 

term(19) = term(19) * (0.5d+0) 


    calc_D_oo_wm_triplet_pt0 = zero
    do s = 0, 20
    calc_D_oo_wm_triplet_pt0 = calc_D_oo_wm_triplet_pt0 + term(s)
    end do

    end function calc_D_oo_wm_triplet_pt0
    
    function calc_D_ov_wm_triplet_pt0(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_ov_wm_triplet_pt0
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
    
    calc_D_ov_wm_triplet_pt0 = zero
    do s = 0, 0
    calc_D_ov_wm_triplet_pt0 = calc_D_ov_wm_triplet_pt0 + term(s)
    end do

    end function calc_D_ov_wm_triplet_pt0
    
    function calc_D_vo_wm_triplet_pt0(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vo_wm_triplet_pt0
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
    real(F64), dimension(0:9) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do a = nocc + 1, nactive 
do i = 1, nocc 
term(0) = term(0) + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, p,i,a,q)
term(1) = term(1) + r2p(vrdav_Rl, p,i,a,q) * r1(vrdav_Rr, a,i)
end do 
end do 

term(0) = term(0) * (-1.0d+0) 
term(1) = term(1) * (-1.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(2) = term(2) + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, a,q,p,i)
term(3) = term(3) + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, a,i,p,q)
term(4) = term(4) + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, a,q,p,i)
term(5) = term(5) + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, a,i,p,q)
term(6) = term(6) + r2p(vrdav_Rl, a,q,p,i) * r1(vrdav_Rr, a,i)
term(7) = term(7) + r2p(vrdav_Rl, a,i,p,q) * r1(vrdav_Rr, a,i)
term(8) = term(8) + r2m(vrdav_Rl, a,q,p,i) * r1(vrdav_Rr, a,i)
term(9) = term(9) + r2m(vrdav_Rl, a,i,p,q) * r1(vrdav_Rr, a,i)
end do 
end do 

term(2) = term(2) * (-1.0d+0) 
term(3) = term(3) * (2.0d+0) 
term(4) = term(4) * (-4.0d+0) 
term(5) = term(5) * (4.0d+0) 
term(6) = term(6) * (-1.0d+0) 
term(7) = term(7) * (2.0d+0) 
term(8) = term(8) * (-4.0d+0) 
term(9) = term(9) * (4.0d+0) 


    calc_D_vo_wm_triplet_pt0 = zero
    do s = 0, 9
    calc_D_vo_wm_triplet_pt0 = calc_D_vo_wm_triplet_pt0 + term(s)
    end do

    end function calc_D_vo_wm_triplet_pt0
    
    function calc_D_vv_wm_triplet_pt0(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vv_wm_triplet_pt0
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
    real(F64), dimension(0:20) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + r2p(vrdav_Rl, p,i,a,j) * r2p(vrdav_Rr, q,j,a,i)
term(1) = term(1) + r2p(vrdav_Rl, a,i,p,j) * r2p(vrdav_Rr, a,j,q,i)
term(2) = term(2) + r2p(vrdav_Rl, a,i,p,j) * r2p(vrdav_Rr, a,i,q,j)
term(3) = term(3) + r2p(vrdav_Rl, p,i,a,j) * r2m(vrdav_Rr, a,j,q,i)
term(4) = term(4) + r2p(vrdav_Rl, a,i,p,j) * r2m(vrdav_Rr, a,j,q,i)
term(5) = term(5) + r2p(vrdav_Rl, a,i,p,j) * r2m(vrdav_Rr, a,i,q,j)
term(6) = term(6) + r2m(vrdav_Rl, a,j,p,i) * r2p(vrdav_Rr, q,i,a,j)
term(7) = term(7) + r2m(vrdav_Rl, a,i,p,j) * r2p(vrdav_Rr, a,j,q,i)
term(8) = term(8) + r2m(vrdav_Rl, a,i,p,j) * r2p(vrdav_Rr, a,i,q,j)
term(9) = term(9) + r2m(vrdav_Rl, a,j,p,i) * r2p(vrdav_Rr, a,i,q,j)
term(10) = term(10) + r2m(vrdav_Rl, a,j,p,i) * r2m(vrdav_Rr, a,i,q,j)
term(11) = term(11) + r2m(vrdav_Rl, a,i,p,j) * r2m(vrdav_Rr, a,j,q,i)
term(12) = term(12) + r2m(vrdav_Rl, a,i,p,j) * r2m(vrdav_Rr, a,i,q,j)
end do 
end do 
end do 

term(0) = term(0) * (-0.5d+0) 
term(1) = term(1) * (-0.5d+0) 
term(3) = term(3) * (2.0d+0) 
term(4) = term(4) * (-2.0d+0) 
term(5) = term(5) * (2.0d+0) 
term(6) = term(6) * (2.0d+0) 
term(7) = term(7) * (-1.0d+0) 
term(8) = term(8) * (2.0d+0) 
term(9) = term(9) * (-1.0d+0) 
term(10) = term(10) * (-4.0d+0) 
term(11) = term(11) * (-4.0d+0) 
term(12) = term(12) * (4.0d+0) 

do j = 1, nocc 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(13) = term(13) + r2p(vrdav_Rl, p,i,a,j) * r2p(vrdav_Rr, q,i,a,j)
term(14) = term(14) + r2p(vrdav_Rl, a,i,p,j) * r2p(vrdav_Rr, q,i,a,j)
term(15) = term(15) + r2p(vrdav_Rl, p,i,a,j) * r2p(vrdav_Rr, a,i,q,j)
term(16) = term(16) + r2p(vrdav_Rl, p,i,a,j) * r2m(vrdav_Rr, a,i,q,j)
term(17) = term(17) + r2m(vrdav_Rl, a,i,p,j) * r2p(vrdav_Rr, q,i,a,j)
end do 
end do 
end do 

term(14) = term(14) * (-0.5d+0) 
term(15) = term(15) * (-0.5d+0) 
term(16) = term(16) * (-2.0d+0) 
term(17) = term(17) * (-1.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
do j = 1, nocc 
term(18) = term(18) + r2m(vrdav_Rl, a,j,p,i) * r2p(vrdav_Rr, q,j,a,i)
end do 
end do 
end do 

term(18) = term(18) * (-1.0d+0) 

do i = 1, nocc 
term(19) = term(19) + r1(vrdav_Rl, p,i) * r1(vrdav_Rr, q,i)
end do 

term(19) = term(19) * (2.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(20) = term(20) + r2m(vrdav_Rl, a,j,p,i) * r2m(vrdav_Rr, a,j,q,i)
end do 
end do 
end do 

term(20) = term(20) * (4.0d+0) 


    calc_D_vv_wm_triplet_pt0 = zero
    do s = 0, 20
    calc_D_vv_wm_triplet_pt0 = calc_D_vv_wm_triplet_pt0 + term(s)
    end do

    end function calc_D_vv_wm_triplet_pt0
    
   function calc_D_oo_wm_triplet_pt1(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_oo_wm_triplet_pt1
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
    
    calc_D_oo_wm_triplet_pt1 = zero
    do s = 0, 0
    calc_D_oo_wm_triplet_pt1 = calc_D_oo_wm_triplet_pt1 + term(s)
    end do

    end function calc_D_oo_wm_triplet_pt1
    
    function calc_D_ov_wm_triplet_pt1(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_ov_wm_triplet_pt1
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
    real(F64), dimension(0:10) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + s2(a,q,j,i) * wm_interm_3_triplet_pt1(a,p,i,j)
term(1) = term(1) + s2(a,q,j,i) * wm_interm_4_triplet_pt1(a,p,i,j)
term(2) = term(2) + s2(a,q,j,i) * wm_interm_10_triplet_pt1(a,p,i,j)
end do 
end do 
end do 

term(0) = term(0) * (-1.0d+0) 
term(1) = term(1) * (2.0d+0) 
term(2) = term(2) * (4.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(3) = term(3) + s2(a,q,j,i) * wm_interm_4_triplet_pt1(a,p,j,i)
term(4) = term(4) + s2(a,q,j,i) * wm_interm_10_triplet_pt1(a,p,j,i)
term(5) = term(5) + t2(a,q,j,i) * wm_interm_17_triplet_pt1(a,i,j,p)
term(6) = term(6) + t2(a,q,j,i) * wm_interm_19_triplet_pt1(a,j,i,p)
term(7) = term(7) + t2(a,q,j,i) * wm_interm_19_triplet_pt1(a,i,j,p)
term(8) = term(8) + t2(a,q,j,i) * wm_interm_24_triplet_pt1(a,i,j,p)
term(9) = term(9) + t2(a,q,j,i) * wm_interm_26_triplet_pt1(a,j,i,p)
term(10) = term(10) + t2(a,q,j,i) * wm_interm_26_triplet_pt1(a,i,j,p)
end do 
end do 
end do 

term(3) = term(3) * (-1.0d+0) 
term(4) = term(4) * (-4.0d+0) 
term(5) = term(5) * (-1.0d+0) 
term(6) = term(6) * (-1.0d+0) 
term(7) = term(7) * (2.0d+0) 
term(8) = term(8) * (-2.0d+0) 
term(9) = term(9) * (-2.0d+0) 
term(10) = term(10) * (4.0d+0) 


    calc_D_ov_wm_triplet_pt1 = zero
    do s = 0, 10
    calc_D_ov_wm_triplet_pt1 = calc_D_ov_wm_triplet_pt1 + term(s)
    end do

    end function calc_D_ov_wm_triplet_pt1
    
    function calc_D_vo_wm_triplet_pt1(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vo_wm_triplet_pt1
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
    real(F64), dimension(0:30) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do i = 1, nocc 
term(0) = term(0) + r1(vrdav_Rl, p,i) * wm_interm_5_triplet_pt1(i,q)
term(1) = term(1) + r1(vrdav_Rl, p,i) * wm_interm_6_triplet_pt1(i,q)
term(2) = term(2) + r1(vrdav_Rl, p,i) * wm_interm_7_triplet_pt1(i,q)
term(3) = term(3) + r1(vrdav_Rl, p,i) * wm_interm_11_triplet_pt1(i,q)
term(4) = term(4) + r1(vrdav_Rl, p,i) * wm_interm_12_triplet_pt1(i,q)
term(5) = term(5) + r1(vrdav_Rl, p,i) * wm_interm_13_triplet_pt1(i,q)
term(6) = term(6) + r1(vrdav_Rr, p,i) * wm_interm_14_triplet_pt1(i,q)
term(7) = term(7) + r1(vrdav_Rr, p,i) * wm_interm_15_triplet_pt1(i,q)
term(8) = term(8) + r1(vrdav_Rr, p,i) * wm_interm_16_triplet_pt1(i,q)
term(9) = term(9) + r1(vrdav_Rr, p,i) * wm_interm_22_triplet_pt1(i,q)
term(10) = term(10) + r1(vrdav_Rr, p,i) * wm_interm_23_triplet_pt1(i,q)
end do 

term(0) = term(0) * (-1.0d+0) 
term(1) = term(1) * (2.0d+0) 
term(2) = term(2) * (-1.0d+0) 
term(3) = term(3) * (-2.0d+0) 
term(4) = term(4) * (4.0d+0) 
term(5) = term(5) * (-2.0d+0) 
term(6) = term(6) * (-1.0d+0) 
term(7) = term(7) * (-1.0d+0) 
term(8) = term(8) * (2.0d+0) 
term(9) = term(9) * (-4.0d+0) 
term(10) = term(10) * (4.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(11) = term(11) + s2(a,p,q,i) * wm_interm_0_triplet_pt1(a,i)
term(12) = term(12) + s2(a,p,q,i) * wm_interm_1_triplet_pt1(a,i)
term(13) = term(13) + s2(a,p,q,i) * wm_interm_2_triplet_pt1(a,i)
term(14) = term(14) + s2(a,p,i,q) * wm_interm_0_triplet_pt1(a,i)
term(15) = term(15) + s2(a,p,i,q) * wm_interm_1_triplet_pt1(a,i)
term(16) = term(16) + s2(a,p,i,q) * wm_interm_2_triplet_pt1(a,i)
term(17) = term(17) + s2(a,p,q,i) * wm_interm_8_triplet_pt1(a,i)
term(18) = term(18) + s2(a,p,q,i) * wm_interm_9_triplet_pt1(a,i)
term(19) = term(19) + s2(a,p,i,q) * wm_interm_8_triplet_pt1(a,i)
term(20) = term(20) + s2(a,p,i,q) * wm_interm_9_triplet_pt1(a,i)
term(21) = term(21) + t2(a,p,i,q) * wm_interm_18_triplet_pt1(a,i)
term(22) = term(22) + t2(a,p,i,q) * wm_interm_20_triplet_pt1(a,i)
term(23) = term(23) + t2(a,p,i,q) * wm_interm_21_triplet_pt1(a,i)
term(24) = term(24) + t2(a,p,q,i) * wm_interm_18_triplet_pt1(a,i)
term(25) = term(25) + t2(a,p,q,i) * wm_interm_20_triplet_pt1(a,i)
term(26) = term(26) + t2(a,p,q,i) * wm_interm_21_triplet_pt1(a,i)
term(27) = term(27) + t2(a,p,i,q) * wm_interm_25_triplet_pt1(a,i)
term(28) = term(28) + t2(a,p,i,q) * wm_interm_27_triplet_pt1(a,i)
term(29) = term(29) + t2(a,p,q,i) * wm_interm_25_triplet_pt1(a,i)
term(30) = term(30) + t2(a,p,q,i) * wm_interm_27_triplet_pt1(a,i)
end do 
end do 

term(11) = term(11) * (-1.0d+0) 
term(12) = term(12) * (-1.0d+0) 
term(13) = term(13) * (2.0d+0) 
term(14) = term(14) * (2.0d+0) 
term(15) = term(15) * (2.0d+0) 
term(16) = term(16) * (-4.0d+0) 
term(17) = term(17) * (-4.0d+0) 
term(18) = term(18) * (4.0d+0) 
term(19) = term(19) * (8.0d+0) 
term(20) = term(20) * (-8.0d+0) 
term(21) = term(21) * (2.0d+0) 
term(22) = term(22) * (2.0d+0) 
term(23) = term(23) * (-4.0d+0) 
term(24) = term(24) * (-1.0d+0) 
term(25) = term(25) * (-1.0d+0) 
term(26) = term(26) * (2.0d+0) 
term(27) = term(27) * (8.0d+0) 
term(28) = term(28) * (-8.0d+0) 
term(29) = term(29) * (-4.0d+0) 
term(30) = term(30) * (4.0d+0) 


    calc_D_vo_wm_triplet_pt1 = zero
    do s = 0, 30
    calc_D_vo_wm_triplet_pt1 = calc_D_vo_wm_triplet_pt1 + term(s)
    end do

    end function calc_D_vo_wm_triplet_pt1
    
    function calc_D_vv_wm_triplet_pt1(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vv_wm_triplet_pt1
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
    
    calc_D_vv_wm_triplet_pt1 = zero
    do s = 0, 0
    calc_D_vv_wm_triplet_pt1 = calc_D_vv_wm_triplet_pt1 + term(s)
    end do

    end function calc_D_vv_wm_triplet_pt1
    
    
    function calc_D_oo_wm_triplet_pt2(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_oo_wm_triplet_pt2
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , i, b, a, k, j, l 
    real(F64), dimension(0:1381) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(0) = term(0) + wm_interm_47_triplet_pt2(a,b,q,i) * wm_interm_7_triplet_pt2(a,b,p,i)
term(1) = term(1) + wm_interm_15_triplet_pt2(a,b,p,i) * wm_interm_47_triplet_pt2(a,b,q,i)
term(2) = term(2) + wm_interm_18_triplet_pt2(a,b,p,i) * wm_interm_47_triplet_pt2(a,b,q,i)
term(3) = term(3) + wm_interm_20_triplet_pt2(a,b,p,i) * wm_interm_47_triplet_pt2(a,b,q,i)
term(4) = term(4) + wm_interm_47_triplet_pt2(a,b,q,i) * wm_interm_8_triplet_pt2(a,b,p,i)
term(5) = term(5) + wm_interm_24_triplet_pt2(a,b,p,i) * wm_interm_47_triplet_pt2(a,b,q,i)
term(6) = term(6) + wm_interm_102_triplet_pt2(a,b,i,p) * wm_interm_25_triplet_pt2(b,a,q,i)
term(7) = term(7) + wm_interm_103_triplet_pt2(a,b,i,p) * wm_interm_25_triplet_pt2(b,a,q,i)
term(8) = term(8) + wm_interm_102_triplet_pt2(a,b,i,p) * wm_interm_28_triplet_pt2(b,a,q,i)
term(9) = term(9) + wm_interm_103_triplet_pt2(a,b,i,p) * wm_interm_28_triplet_pt2(b,a,q,i)
term(10) = term(10) + wm_interm_101_triplet_pt2(a,b,i,p) * wm_interm_25_triplet_pt2(b,a,q,i)
term(11) = term(11) + wm_interm_101_triplet_pt2(a,b,i,p) * wm_interm_28_triplet_pt2(b,a,q,i)
term(12) = term(12) + wm_interm_50_triplet_pt2(a,b,q,i) * wm_interm_7_triplet_pt2(a,b,p,i)
term(13) = term(13) + wm_interm_15_triplet_pt2(a,b,p,i) * wm_interm_50_triplet_pt2(a,b,q,i)
term(14) = term(14) + wm_interm_18_triplet_pt2(a,b,p,i) * wm_interm_50_triplet_pt2(a,b,q,i)
term(15) = term(15) + wm_interm_20_triplet_pt2(a,b,p,i) * wm_interm_50_triplet_pt2(a,b,q,i)
term(16) = term(16) + wm_interm_106_triplet_pt2(a,b,i,p) * wm_interm_25_triplet_pt2(b,a,q,i)
term(17) = term(17) + wm_interm_105_triplet_pt2(a,b,i,p) * wm_interm_25_triplet_pt2(b,a,q,i)
term(18) = term(18) + wm_interm_106_triplet_pt2(a,b,i,p) * wm_interm_28_triplet_pt2(b,a,q,i)
term(19) = term(19) + wm_interm_105_triplet_pt2(a,b,i,p) * wm_interm_28_triplet_pt2(b,a,q,i)
term(20) = term(20) + wm_interm_50_triplet_pt2(a,b,q,i) * wm_interm_8_triplet_pt2(a,b,p,i)
term(21) = term(21) + wm_interm_24_triplet_pt2(a,b,p,i) * wm_interm_50_triplet_pt2(a,b,q,i)
term(22) = term(22) + wm_interm_114_triplet_pt2(a,b,i,p) * wm_interm_25_triplet_pt2(b,a,q,i)
term(23) = term(23) + wm_interm_114_triplet_pt2(a,b,i,p) * wm_interm_28_triplet_pt2(b,a,q,i)
term(24) = term(24) + wm_interm_52_triplet_pt2(a,b,q,i) * wm_interm_7_triplet_pt2(a,b,p,i)
term(25) = term(25) + wm_interm_15_triplet_pt2(a,b,p,i) * wm_interm_52_triplet_pt2(a,b,q,i)
term(26) = term(26) + wm_interm_18_triplet_pt2(a,b,p,i) * wm_interm_52_triplet_pt2(a,b,q,i)
term(27) = term(27) + wm_interm_20_triplet_pt2(a,b,p,i) * wm_interm_52_triplet_pt2(a,b,q,i)
term(28) = term(28) + wm_interm_52_triplet_pt2(a,b,q,i) * wm_interm_8_triplet_pt2(a,b,p,i)
term(29) = term(29) + wm_interm_24_triplet_pt2(a,b,p,i) * wm_interm_52_triplet_pt2(a,b,q,i)
term(30) = term(30) + wm_interm_118_triplet_pt2(a,b,i,p) * wm_interm_25_triplet_pt2(b,a,q,i)
term(31) = term(31) + wm_interm_104_triplet_pt2(a,b,i,p) * wm_interm_25_triplet_pt2(b,a,q,i)
term(32) = term(32) + wm_interm_118_triplet_pt2(a,b,i,p) * wm_interm_28_triplet_pt2(b,a,q,i)
term(33) = term(33) + wm_interm_104_triplet_pt2(a,b,i,p) * wm_interm_28_triplet_pt2(b,a,q,i)
term(34) = term(34) + wm_interm_100_triplet_pt2(a,b,i,p) * wm_interm_25_triplet_pt2(b,a,q,i)
term(35) = term(35) + wm_interm_100_triplet_pt2(a,b,i,p) * wm_interm_28_triplet_pt2(b,a,q,i)
term(36) = term(36) + wm_interm_18_triplet_pt2(a,b,p,i) * wm_interm_53_triplet_pt2(a,b,q,i)
term(37) = term(37) + wm_interm_20_triplet_pt2(a,b,p,i) * wm_interm_53_triplet_pt2(a,b,q,i)
term(38) = term(38) + wm_interm_24_triplet_pt2(a,b,p,i) * wm_interm_53_triplet_pt2(a,b,q,i)
term(39) = term(39) + wm_interm_118_triplet_pt2(a,b,i,p) * wm_interm_27_triplet_pt2(b,a,q,i)
term(40) = term(40) + wm_interm_104_triplet_pt2(a,b,i,p) * wm_interm_27_triplet_pt2(b,a,q,i)
term(41) = term(41) + wm_interm_100_triplet_pt2(a,b,i,p) * wm_interm_27_triplet_pt2(b,a,q,i)
term(42) = term(42) + wm_interm_104_triplet_pt2(a,b,i,p) * wm_interm_26_triplet_pt2(b,a,q,i)
term(43) = term(43) + wm_interm_26_triplet_pt2(a,b,q,i) * wm_interm_99_triplet_pt2(b,a,i,p)
term(44) = term(44) + wm_interm_100_triplet_pt2(a,b,i,p) * wm_interm_26_triplet_pt2(b,a,q,i)
term(45) = term(45) + wm_interm_15_triplet_pt2(a,b,p,i) * wm_interm_53_triplet_pt2(a,b,q,i)
term(46) = term(46) + wm_interm_53_triplet_pt2(a,b,q,i) * wm_interm_9_triplet_pt2(a,b,p,i)
term(47) = term(47) + wm_interm_53_triplet_pt2(a,b,q,i) * wm_interm_8_triplet_pt2(a,b,p,i)
term(48) = term(48) + wm_interm_56_triplet_pt2(a,b,q,i) * wm_interm_7_triplet_pt2(a,b,p,i)
term(49) = term(49) + wm_interm_15_triplet_pt2(a,b,p,i) * wm_interm_56_triplet_pt2(a,b,q,i)
term(50) = term(50) + wm_interm_18_triplet_pt2(a,b,p,i) * wm_interm_56_triplet_pt2(a,b,q,i)
term(51) = term(51) + wm_interm_20_triplet_pt2(a,b,p,i) * wm_interm_56_triplet_pt2(a,b,q,i)
term(52) = term(52) + wm_interm_56_triplet_pt2(a,b,q,i) * wm_interm_8_triplet_pt2(a,b,p,i)
term(53) = term(53) + wm_interm_24_triplet_pt2(a,b,p,i) * wm_interm_56_triplet_pt2(a,b,q,i)
term(54) = term(54) + wm_interm_102_triplet_pt2(a,b,i,p) * wm_interm_26_triplet_pt2(b,a,q,i)
term(55) = term(55) + wm_interm_103_triplet_pt2(a,b,i,p) * wm_interm_26_triplet_pt2(b,a,q,i)
term(56) = term(56) + wm_interm_102_triplet_pt2(a,b,i,p) * wm_interm_27_triplet_pt2(b,a,q,i)
term(57) = term(57) + wm_interm_103_triplet_pt2(a,b,i,p) * wm_interm_27_triplet_pt2(b,a,q,i)
term(58) = term(58) + wm_interm_101_triplet_pt2(a,b,i,p) * wm_interm_26_triplet_pt2(b,a,q,i)
term(59) = term(59) + wm_interm_101_triplet_pt2(a,b,i,p) * wm_interm_27_triplet_pt2(b,a,q,i)
term(60) = term(60) + wm_interm_18_triplet_pt2(a,b,p,i) * wm_interm_59_triplet_pt2(a,b,q,i)
term(61) = term(61) + wm_interm_20_triplet_pt2(a,b,p,i) * wm_interm_59_triplet_pt2(a,b,q,i)
term(62) = term(62) + wm_interm_15_triplet_pt2(a,b,p,i) * wm_interm_59_triplet_pt2(a,b,q,i)
term(63) = term(63) + wm_interm_59_triplet_pt2(a,b,q,i) * wm_interm_7_triplet_pt2(a,b,p,i)
term(64) = term(64) + wm_interm_106_triplet_pt2(a,b,i,p) * wm_interm_27_triplet_pt2(b,a,q,i)
term(65) = term(65) + wm_interm_105_triplet_pt2(a,b,i,p) * wm_interm_27_triplet_pt2(b,a,q,i)
term(66) = term(66) + wm_interm_105_triplet_pt2(a,b,i,p) * wm_interm_26_triplet_pt2(b,a,q,i)
term(67) = term(67) + wm_interm_106_triplet_pt2(a,b,i,p) * wm_interm_26_triplet_pt2(b,a,q,i)
term(68) = term(68) + wm_interm_59_triplet_pt2(a,b,q,i) * wm_interm_9_triplet_pt2(a,b,p,i)
term(69) = term(69) + wm_interm_24_triplet_pt2(a,b,p,i) * wm_interm_59_triplet_pt2(a,b,q,i)
term(70) = term(70) + wm_interm_107_triplet_pt2(a,b,i,p) * wm_interm_26_triplet_pt2(b,a,q,i)
term(71) = term(71) + wm_interm_114_triplet_pt2(a,b,i,p) * wm_interm_27_triplet_pt2(b,a,q,i)
term(72) = term(72) + wm_interm_63_triplet_pt2(a,b,q,i) * wm_interm_7_triplet_pt2(a,b,p,i)
term(73) = term(73) + wm_interm_63_triplet_pt2(a,b,q,i) * wm_interm_8_triplet_pt2(a,b,p,i)
term(74) = term(74) + wm_interm_63_triplet_pt2(a,b,q,i) * wm_interm_9_triplet_pt2(a,b,p,i)
term(75) = term(75) + wm_interm_117_triplet_pt2(a,b,i,p) * wm_interm_26_triplet_pt2(b,a,q,i)
term(76) = term(76) + wm_interm_115_triplet_pt2(a,b,i,p) * wm_interm_26_triplet_pt2(b,a,q,i)
term(77) = term(77) + wm_interm_116_triplet_pt2(a,b,i,p) * wm_interm_26_triplet_pt2(b,a,q,i)
term(78) = term(78) + wm_interm_11_triplet_pt2(a,b,p,i) * wm_interm_47_triplet_pt2(a,b,q,i)
term(79) = term(79) + wm_interm_12_triplet_pt2(a,b,p,i) * wm_interm_47_triplet_pt2(a,b,q,i)
term(80) = term(80) + wm_interm_35_triplet_pt2(a,b,p,i) * wm_interm_47_triplet_pt2(a,b,q,i)
term(81) = term(81) + wm_interm_37_triplet_pt2(a,b,p,i) * wm_interm_47_triplet_pt2(a,b,q,i)
term(82) = term(82) + wm_interm_127_triplet_pt2(a,b,i,p) * wm_interm_25_triplet_pt2(b,a,q,i)
term(83) = term(83) + wm_interm_128_triplet_pt2(a,b,i,p) * wm_interm_25_triplet_pt2(b,a,q,i)
term(84) = term(84) + wm_interm_127_triplet_pt2(a,b,i,p) * wm_interm_28_triplet_pt2(b,a,q,i)
term(85) = term(85) + wm_interm_128_triplet_pt2(a,b,i,p) * wm_interm_28_triplet_pt2(b,a,q,i)
term(86) = term(86) + wm_interm_11_triplet_pt2(a,b,p,i) * wm_interm_50_triplet_pt2(a,b,q,i)
term(87) = term(87) + wm_interm_12_triplet_pt2(a,b,p,i) * wm_interm_50_triplet_pt2(a,b,q,i)
term(88) = term(88) + wm_interm_35_triplet_pt2(a,b,p,i) * wm_interm_50_triplet_pt2(a,b,q,i)
term(89) = term(89) + wm_interm_37_triplet_pt2(a,b,p,i) * wm_interm_50_triplet_pt2(a,b,q,i)
term(90) = term(90) + wm_interm_130_triplet_pt2(a,b,i,p) * wm_interm_25_triplet_pt2(b,a,q,i)
term(91) = term(91) + wm_interm_129_triplet_pt2(a,b,i,p) * wm_interm_25_triplet_pt2(b,a,q,i)
term(92) = term(92) + wm_interm_130_triplet_pt2(a,b,i,p) * wm_interm_28_triplet_pt2(b,a,q,i)
term(93) = term(93) + wm_interm_129_triplet_pt2(a,b,i,p) * wm_interm_28_triplet_pt2(b,a,q,i)
term(94) = term(94) + wm_interm_11_triplet_pt2(a,b,p,i) * wm_interm_52_triplet_pt2(a,b,q,i)
term(95) = term(95) + wm_interm_12_triplet_pt2(a,b,p,i) * wm_interm_52_triplet_pt2(a,b,q,i)
term(96) = term(96) + wm_interm_35_triplet_pt2(a,b,p,i) * wm_interm_52_triplet_pt2(a,b,q,i)
term(97) = term(97) + wm_interm_37_triplet_pt2(a,b,p,i) * wm_interm_52_triplet_pt2(a,b,q,i)
term(98) = term(98) + wm_interm_126_triplet_pt2(a,b,i,p) * wm_interm_25_triplet_pt2(b,a,q,i)
term(99) = term(99) + wm_interm_125_triplet_pt2(a,b,i,p) * wm_interm_25_triplet_pt2(b,a,q,i)
term(100) = term(100) + wm_interm_126_triplet_pt2(a,b,i,p) * wm_interm_28_triplet_pt2(b,a,q,i)
term(101) = term(101) + wm_interm_125_triplet_pt2(a,b,i,p) * wm_interm_28_triplet_pt2(b,a,q,i)
term(102) = term(102) + wm_interm_35_triplet_pt2(a,b,p,i) * wm_interm_53_triplet_pt2(a,b,q,i)
term(103) = term(103) + wm_interm_37_triplet_pt2(a,b,p,i) * wm_interm_53_triplet_pt2(a,b,q,i)
term(104) = term(104) + wm_interm_126_triplet_pt2(a,b,i,p) * wm_interm_27_triplet_pt2(b,a,q,i)
term(105) = term(105) + wm_interm_125_triplet_pt2(a,b,i,p) * wm_interm_27_triplet_pt2(b,a,q,i)
term(106) = term(106) + wm_interm_125_triplet_pt2(a,b,i,p) * wm_interm_26_triplet_pt2(b,a,q,i)
term(107) = term(107) + wm_interm_126_triplet_pt2(a,b,i,p) * wm_interm_26_triplet_pt2(b,a,q,i)
term(108) = term(108) + wm_interm_12_triplet_pt2(a,b,p,i) * wm_interm_53_triplet_pt2(a,b,q,i)
term(109) = term(109) + wm_interm_11_triplet_pt2(a,b,p,i) * wm_interm_53_triplet_pt2(a,b,q,i)
term(110) = term(110) + wm_interm_11_triplet_pt2(a,b,p,i) * wm_interm_56_triplet_pt2(a,b,q,i)
term(111) = term(111) + wm_interm_12_triplet_pt2(a,b,p,i) * wm_interm_56_triplet_pt2(a,b,q,i)
term(112) = term(112) + wm_interm_35_triplet_pt2(a,b,p,i) * wm_interm_56_triplet_pt2(a,b,q,i)
term(113) = term(113) + wm_interm_37_triplet_pt2(a,b,p,i) * wm_interm_56_triplet_pt2(a,b,q,i)
term(114) = term(114) + wm_interm_127_triplet_pt2(a,b,i,p) * wm_interm_26_triplet_pt2(b,a,q,i)
term(115) = term(115) + wm_interm_128_triplet_pt2(a,b,i,p) * wm_interm_26_triplet_pt2(b,a,q,i)
term(116) = term(116) + wm_interm_127_triplet_pt2(a,b,i,p) * wm_interm_27_triplet_pt2(b,a,q,i)
term(117) = term(117) + wm_interm_128_triplet_pt2(a,b,i,p) * wm_interm_27_triplet_pt2(b,a,q,i)
term(118) = term(118) + wm_interm_35_triplet_pt2(a,b,p,i) * wm_interm_59_triplet_pt2(a,b,q,i)
term(119) = term(119) + wm_interm_37_triplet_pt2(a,b,p,i) * wm_interm_59_triplet_pt2(a,b,q,i)
term(120) = term(120) + wm_interm_12_triplet_pt2(a,b,p,i) * wm_interm_59_triplet_pt2(a,b,q,i)
term(121) = term(121) + wm_interm_11_triplet_pt2(a,b,p,i) * wm_interm_59_triplet_pt2(a,b,q,i)
term(122) = term(122) + wm_interm_130_triplet_pt2(a,b,i,p) * wm_interm_27_triplet_pt2(b,a,q,i)
term(123) = term(123) + wm_interm_129_triplet_pt2(a,b,i,p) * wm_interm_27_triplet_pt2(b,a,q,i)
term(124) = term(124) + wm_interm_129_triplet_pt2(a,b,i,p) * wm_interm_26_triplet_pt2(b,a,q,i)
term(125) = term(125) + wm_interm_130_triplet_pt2(a,b,i,p) * wm_interm_26_triplet_pt2(b,a,q,i)
term(126) = term(126) + wm_interm_11_triplet_pt2(a,b,p,i) * wm_interm_63_triplet_pt2(a,b,q,i)
term(127) = term(127) + wm_interm_12_triplet_pt2(a,b,p,i) * wm_interm_63_triplet_pt2(a,b,q,i)
term(128) = term(128) + wm_interm_135_triplet_pt2(a,b,i,p) * wm_interm_26_triplet_pt2(b,a,q,i)
term(129) = term(129) + wm_interm_136_triplet_pt2(a,b,i,p) * wm_interm_26_triplet_pt2(b,a,q,i)
term(130) = term(130) + wm_interm_100_triplet_pt2(a,b,p,i) * wm_interm_25_triplet_pt2(b,a,i,q)
term(131) = term(131) + wm_interm_114_triplet_pt2(a,b,p,i) * wm_interm_25_triplet_pt2(b,a,i,q)
term(132) = term(132) + wm_interm_101_triplet_pt2(a,b,p,i) * wm_interm_25_triplet_pt2(b,a,i,q)
term(133) = term(133) + wm_interm_118_triplet_pt2(a,b,p,i) * wm_interm_25_triplet_pt2(b,a,i,q)
term(134) = term(134) + wm_interm_106_triplet_pt2(a,b,p,i) * wm_interm_25_triplet_pt2(b,a,i,q)
term(135) = term(135) + wm_interm_102_triplet_pt2(a,b,p,i) * wm_interm_25_triplet_pt2(b,a,i,q)
term(136) = term(136) + wm_interm_104_triplet_pt2(a,b,p,i) * wm_interm_25_triplet_pt2(b,a,i,q)
term(137) = term(137) + wm_interm_105_triplet_pt2(a,b,p,i) * wm_interm_25_triplet_pt2(b,a,i,q)
term(138) = term(138) + wm_interm_103_triplet_pt2(a,b,p,i) * wm_interm_25_triplet_pt2(b,a,i,q)
term(139) = term(139) + wm_interm_63_triplet_pt2(a,b,i,q) * wm_interm_8_triplet_pt2(a,b,i,p)
term(140) = term(140) + wm_interm_63_triplet_pt2(a,b,i,q) * wm_interm_9_triplet_pt2(a,b,i,p)
term(141) = term(141) + wm_interm_59_triplet_pt2(a,b,i,q) * wm_interm_9_triplet_pt2(a,b,i,p)
term(142) = term(142) + wm_interm_53_triplet_pt2(a,b,i,q) * wm_interm_9_triplet_pt2(a,b,i,p)
term(143) = term(143) + wm_interm_53_triplet_pt2(a,b,i,q) * wm_interm_8_triplet_pt2(a,b,i,p)
term(144) = term(144) + wm_interm_56_triplet_pt2(a,b,i,q) * wm_interm_8_triplet_pt2(a,b,i,p)
term(145) = term(145) + wm_interm_15_triplet_pt2(a,b,i,p) * wm_interm_59_triplet_pt2(a,b,i,q)
term(146) = term(146) + wm_interm_59_triplet_pt2(a,b,i,q) * wm_interm_7_triplet_pt2(a,b,i,p)
term(147) = term(147) + wm_interm_63_triplet_pt2(a,b,i,q) * wm_interm_7_triplet_pt2(a,b,i,p)
term(148) = term(148) + wm_interm_56_triplet_pt2(a,b,i,q) * wm_interm_7_triplet_pt2(a,b,i,p)
term(149) = term(149) + wm_interm_15_triplet_pt2(a,b,i,p) * wm_interm_56_triplet_pt2(a,b,i,q)
term(150) = term(150) + wm_interm_15_triplet_pt2(a,b,i,p) * wm_interm_53_triplet_pt2(a,b,i,q)
term(151) = term(151) + wm_interm_24_triplet_pt2(a,b,i,p) * wm_interm_59_triplet_pt2(a,b,i,q)
term(152) = term(152) + wm_interm_24_triplet_pt2(a,b,i,p) * wm_interm_53_triplet_pt2(a,b,i,q)
term(153) = term(153) + wm_interm_100_triplet_pt2(a,b,p,i) * wm_interm_28_triplet_pt2(b,a,i,q)
term(154) = term(154) + wm_interm_114_triplet_pt2(a,b,p,i) * wm_interm_28_triplet_pt2(b,a,i,q)
term(155) = term(155) + wm_interm_24_triplet_pt2(a,b,i,p) * wm_interm_56_triplet_pt2(a,b,i,q)
term(156) = term(156) + wm_interm_101_triplet_pt2(a,b,p,i) * wm_interm_28_triplet_pt2(b,a,i,q)
term(157) = term(157) + wm_interm_18_triplet_pt2(a,b,i,p) * wm_interm_59_triplet_pt2(a,b,i,q)
term(158) = term(158) + wm_interm_18_triplet_pt2(a,b,i,p) * wm_interm_53_triplet_pt2(a,b,i,q)
term(159) = term(159) + wm_interm_118_triplet_pt2(a,b,p,i) * wm_interm_28_triplet_pt2(b,a,i,q)
term(160) = term(160) + wm_interm_106_triplet_pt2(a,b,p,i) * wm_interm_28_triplet_pt2(b,a,i,q)
term(161) = term(161) + wm_interm_18_triplet_pt2(a,b,i,p) * wm_interm_56_triplet_pt2(a,b,i,q)
term(162) = term(162) + wm_interm_102_triplet_pt2(a,b,p,i) * wm_interm_28_triplet_pt2(b,a,i,q)
term(163) = term(163) + wm_interm_20_triplet_pt2(a,b,i,p) * wm_interm_59_triplet_pt2(a,b,i,q)
term(164) = term(164) + wm_interm_20_triplet_pt2(a,b,i,p) * wm_interm_53_triplet_pt2(a,b,i,q)
term(165) = term(165) + wm_interm_104_triplet_pt2(a,b,p,i) * wm_interm_28_triplet_pt2(b,a,i,q)
term(166) = term(166) + wm_interm_105_triplet_pt2(a,b,p,i) * wm_interm_28_triplet_pt2(b,a,i,q)
term(167) = term(167) + wm_interm_20_triplet_pt2(a,b,i,p) * wm_interm_56_triplet_pt2(a,b,i,q)
term(168) = term(168) + wm_interm_103_triplet_pt2(a,b,p,i) * wm_interm_28_triplet_pt2(b,a,i,q)
term(169) = term(169) + wm_interm_100_triplet_pt2(a,b,p,i) * wm_interm_27_triplet_pt2(b,a,i,q)
term(170) = term(170) + wm_interm_114_triplet_pt2(a,b,p,i) * wm_interm_27_triplet_pt2(b,a,i,q)
term(171) = term(171) + wm_interm_101_triplet_pt2(a,b,p,i) * wm_interm_27_triplet_pt2(b,a,i,q)
term(172) = term(172) + wm_interm_118_triplet_pt2(a,b,p,i) * wm_interm_27_triplet_pt2(b,a,i,q)
term(173) = term(173) + wm_interm_106_triplet_pt2(a,b,p,i) * wm_interm_27_triplet_pt2(b,a,i,q)
term(174) = term(174) + wm_interm_102_triplet_pt2(a,b,p,i) * wm_interm_27_triplet_pt2(b,a,i,q)
term(175) = term(175) + wm_interm_104_triplet_pt2(a,b,p,i) * wm_interm_27_triplet_pt2(b,a,i,q)
term(176) = term(176) + wm_interm_105_triplet_pt2(a,b,p,i) * wm_interm_27_triplet_pt2(b,a,i,q)
term(177) = term(177) + wm_interm_103_triplet_pt2(a,b,p,i) * wm_interm_27_triplet_pt2(b,a,i,q)
term(178) = term(178) + wm_interm_115_triplet_pt2(a,b,p,i) * wm_interm_26_triplet_pt2(b,a,i,q)
term(179) = term(179) + wm_interm_116_triplet_pt2(a,b,p,i) * wm_interm_26_triplet_pt2(b,a,i,q)
term(180) = term(180) + wm_interm_107_triplet_pt2(a,b,p,i) * wm_interm_26_triplet_pt2(b,a,i,q)
term(181) = term(181) + wm_interm_26_triplet_pt2(a,b,i,q) * wm_interm_99_triplet_pt2(b,a,p,i)
term(182) = term(182) + wm_interm_100_triplet_pt2(a,b,p,i) * wm_interm_26_triplet_pt2(b,a,i,q)
term(183) = term(183) + wm_interm_101_triplet_pt2(a,b,p,i) * wm_interm_26_triplet_pt2(b,a,i,q)
term(184) = term(184) + wm_interm_105_triplet_pt2(a,b,p,i) * wm_interm_26_triplet_pt2(b,a,i,q)
term(185) = term(185) + wm_interm_106_triplet_pt2(a,b,p,i) * wm_interm_26_triplet_pt2(b,a,i,q)
term(186) = term(186) + wm_interm_117_triplet_pt2(a,b,p,i) * wm_interm_26_triplet_pt2(b,a,i,q)
term(187) = term(187) + wm_interm_102_triplet_pt2(a,b,p,i) * wm_interm_26_triplet_pt2(b,a,i,q)
term(188) = term(188) + wm_interm_103_triplet_pt2(a,b,p,i) * wm_interm_26_triplet_pt2(b,a,i,q)
term(189) = term(189) + wm_interm_104_triplet_pt2(a,b,p,i) * wm_interm_26_triplet_pt2(b,a,i,q)
term(190) = term(190) + wm_interm_52_triplet_pt2(a,b,i,q) * wm_interm_8_triplet_pt2(a,b,i,p)
term(191) = term(191) + wm_interm_50_triplet_pt2(a,b,i,q) * wm_interm_8_triplet_pt2(a,b,i,p)
term(192) = term(192) + wm_interm_47_triplet_pt2(a,b,i,q) * wm_interm_8_triplet_pt2(a,b,i,p)
term(193) = term(193) + wm_interm_52_triplet_pt2(a,b,i,q) * wm_interm_7_triplet_pt2(a,b,i,p)
term(194) = term(194) + wm_interm_50_triplet_pt2(a,b,i,q) * wm_interm_7_triplet_pt2(a,b,i,p)
term(195) = term(195) + wm_interm_47_triplet_pt2(a,b,i,q) * wm_interm_7_triplet_pt2(a,b,i,p)
term(196) = term(196) + wm_interm_15_triplet_pt2(a,b,i,p) * wm_interm_52_triplet_pt2(a,b,i,q)
term(197) = term(197) + wm_interm_15_triplet_pt2(a,b,i,p) * wm_interm_50_triplet_pt2(a,b,i,q)
term(198) = term(198) + wm_interm_15_triplet_pt2(a,b,i,p) * wm_interm_47_triplet_pt2(a,b,i,q)
term(199) = term(199) + wm_interm_24_triplet_pt2(a,b,i,p) * wm_interm_52_triplet_pt2(a,b,i,q)
term(200) = term(200) + wm_interm_24_triplet_pt2(a,b,i,p) * wm_interm_50_triplet_pt2(a,b,i,q)
term(201) = term(201) + wm_interm_24_triplet_pt2(a,b,i,p) * wm_interm_47_triplet_pt2(a,b,i,q)
term(202) = term(202) + wm_interm_18_triplet_pt2(a,b,i,p) * wm_interm_52_triplet_pt2(a,b,i,q)
term(203) = term(203) + wm_interm_18_triplet_pt2(a,b,i,p) * wm_interm_50_triplet_pt2(a,b,i,q)
term(204) = term(204) + wm_interm_18_triplet_pt2(a,b,i,p) * wm_interm_47_triplet_pt2(a,b,i,q)
term(205) = term(205) + wm_interm_20_triplet_pt2(a,b,i,p) * wm_interm_52_triplet_pt2(a,b,i,q)
term(206) = term(206) + wm_interm_20_triplet_pt2(a,b,i,p) * wm_interm_50_triplet_pt2(a,b,i,q)
term(207) = term(207) + wm_interm_20_triplet_pt2(a,b,i,p) * wm_interm_47_triplet_pt2(a,b,i,q)
term(208) = term(208) + wm_interm_126_triplet_pt2(a,b,p,i) * wm_interm_25_triplet_pt2(b,a,i,q)
term(209) = term(209) + wm_interm_130_triplet_pt2(a,b,p,i) * wm_interm_25_triplet_pt2(b,a,i,q)
term(210) = term(210) + wm_interm_127_triplet_pt2(a,b,p,i) * wm_interm_25_triplet_pt2(b,a,i,q)
term(211) = term(211) + wm_interm_125_triplet_pt2(a,b,p,i) * wm_interm_25_triplet_pt2(b,a,i,q)
term(212) = term(212) + wm_interm_129_triplet_pt2(a,b,p,i) * wm_interm_25_triplet_pt2(b,a,i,q)
term(213) = term(213) + wm_interm_128_triplet_pt2(a,b,p,i) * wm_interm_25_triplet_pt2(b,a,i,q)
term(214) = term(214) + wm_interm_11_triplet_pt2(a,b,i,p) * wm_interm_63_triplet_pt2(a,b,i,q)
term(215) = term(215) + wm_interm_12_triplet_pt2(a,b,i,p) * wm_interm_63_triplet_pt2(a,b,i,q)
term(216) = term(216) + wm_interm_12_triplet_pt2(a,b,i,p) * wm_interm_59_triplet_pt2(a,b,i,q)
term(217) = term(217) + wm_interm_12_triplet_pt2(a,b,i,p) * wm_interm_53_triplet_pt2(a,b,i,q)
term(218) = term(218) + wm_interm_11_triplet_pt2(a,b,i,p) * wm_interm_53_triplet_pt2(a,b,i,q)
term(219) = term(219) + wm_interm_11_triplet_pt2(a,b,i,p) * wm_interm_56_triplet_pt2(a,b,i,q)
term(220) = term(220) + wm_interm_11_triplet_pt2(a,b,i,p) * wm_interm_59_triplet_pt2(a,b,i,q)
term(221) = term(221) + wm_interm_12_triplet_pt2(a,b,i,p) * wm_interm_56_triplet_pt2(a,b,i,q)
term(222) = term(222) + wm_interm_35_triplet_pt2(a,b,i,p) * wm_interm_59_triplet_pt2(a,b,i,q)
term(223) = term(223) + wm_interm_35_triplet_pt2(a,b,i,p) * wm_interm_53_triplet_pt2(a,b,i,q)
term(224) = term(224) + wm_interm_126_triplet_pt2(a,b,p,i) * wm_interm_28_triplet_pt2(b,a,i,q)
term(225) = term(225) + wm_interm_130_triplet_pt2(a,b,p,i) * wm_interm_28_triplet_pt2(b,a,i,q)
term(226) = term(226) + wm_interm_35_triplet_pt2(a,b,i,p) * wm_interm_56_triplet_pt2(a,b,i,q)
term(227) = term(227) + wm_interm_127_triplet_pt2(a,b,p,i) * wm_interm_28_triplet_pt2(b,a,i,q)
term(228) = term(228) + wm_interm_37_triplet_pt2(a,b,i,p) * wm_interm_59_triplet_pt2(a,b,i,q)
term(229) = term(229) + wm_interm_37_triplet_pt2(a,b,i,p) * wm_interm_53_triplet_pt2(a,b,i,q)
term(230) = term(230) + wm_interm_125_triplet_pt2(a,b,p,i) * wm_interm_28_triplet_pt2(b,a,i,q)
term(231) = term(231) + wm_interm_129_triplet_pt2(a,b,p,i) * wm_interm_28_triplet_pt2(b,a,i,q)
term(232) = term(232) + wm_interm_37_triplet_pt2(a,b,i,p) * wm_interm_56_triplet_pt2(a,b,i,q)
term(233) = term(233) + wm_interm_128_triplet_pt2(a,b,p,i) * wm_interm_28_triplet_pt2(b,a,i,q)
term(234) = term(234) + wm_interm_126_triplet_pt2(a,b,p,i) * wm_interm_27_triplet_pt2(b,a,i,q)
term(235) = term(235) + wm_interm_130_triplet_pt2(a,b,p,i) * wm_interm_27_triplet_pt2(b,a,i,q)
term(236) = term(236) + wm_interm_127_triplet_pt2(a,b,p,i) * wm_interm_27_triplet_pt2(b,a,i,q)
term(237) = term(237) + wm_interm_125_triplet_pt2(a,b,p,i) * wm_interm_27_triplet_pt2(b,a,i,q)
term(238) = term(238) + wm_interm_129_triplet_pt2(a,b,p,i) * wm_interm_27_triplet_pt2(b,a,i,q)
term(239) = term(239) + wm_interm_128_triplet_pt2(a,b,p,i) * wm_interm_27_triplet_pt2(b,a,i,q)
term(240) = term(240) + wm_interm_135_triplet_pt2(a,b,p,i) * wm_interm_26_triplet_pt2(b,a,i,q)
term(241) = term(241) + wm_interm_136_triplet_pt2(a,b,p,i) * wm_interm_26_triplet_pt2(b,a,i,q)
term(242) = term(242) + wm_interm_129_triplet_pt2(a,b,p,i) * wm_interm_26_triplet_pt2(b,a,i,q)
term(243) = term(243) + wm_interm_125_triplet_pt2(a,b,p,i) * wm_interm_26_triplet_pt2(b,a,i,q)
term(244) = term(244) + wm_interm_126_triplet_pt2(a,b,p,i) * wm_interm_26_triplet_pt2(b,a,i,q)
term(245) = term(245) + wm_interm_127_triplet_pt2(a,b,p,i) * wm_interm_26_triplet_pt2(b,a,i,q)
term(246) = term(246) + wm_interm_130_triplet_pt2(a,b,p,i) * wm_interm_26_triplet_pt2(b,a,i,q)
term(247) = term(247) + wm_interm_128_triplet_pt2(a,b,p,i) * wm_interm_26_triplet_pt2(b,a,i,q)
term(248) = term(248) + wm_interm_11_triplet_pt2(a,b,i,p) * wm_interm_52_triplet_pt2(a,b,i,q)
term(249) = term(249) + wm_interm_11_triplet_pt2(a,b,i,p) * wm_interm_50_triplet_pt2(a,b,i,q)
term(250) = term(250) + wm_interm_11_triplet_pt2(a,b,i,p) * wm_interm_47_triplet_pt2(a,b,i,q)
term(251) = term(251) + wm_interm_12_triplet_pt2(a,b,i,p) * wm_interm_52_triplet_pt2(a,b,i,q)
term(252) = term(252) + wm_interm_12_triplet_pt2(a,b,i,p) * wm_interm_50_triplet_pt2(a,b,i,q)
term(253) = term(253) + wm_interm_12_triplet_pt2(a,b,i,p) * wm_interm_47_triplet_pt2(a,b,i,q)
term(254) = term(254) + wm_interm_35_triplet_pt2(a,b,i,p) * wm_interm_52_triplet_pt2(a,b,i,q)
term(255) = term(255) + wm_interm_35_triplet_pt2(a,b,i,p) * wm_interm_50_triplet_pt2(a,b,i,q)
term(256) = term(256) + wm_interm_35_triplet_pt2(a,b,i,p) * wm_interm_47_triplet_pt2(a,b,i,q)
term(257) = term(257) + wm_interm_37_triplet_pt2(a,b,i,p) * wm_interm_52_triplet_pt2(a,b,i,q)
term(258) = term(258) + wm_interm_37_triplet_pt2(a,b,i,p) * wm_interm_50_triplet_pt2(a,b,i,q)
term(259) = term(259) + wm_interm_37_triplet_pt2(a,b,i,p) * wm_interm_47_triplet_pt2(a,b,i,q)
term(260) = term(260) + wm_interm_68_triplet_pt2(a,b,q,i) * wm_interm_7_triplet_pt2(a,b,p,i)
term(261) = term(261) + wm_interm_15_triplet_pt2(a,b,p,i) * wm_interm_68_triplet_pt2(a,b,q,i)
term(262) = term(262) + wm_interm_18_triplet_pt2(a,b,p,i) * wm_interm_68_triplet_pt2(a,b,q,i)
term(263) = term(263) + wm_interm_20_triplet_pt2(a,b,p,i) * wm_interm_68_triplet_pt2(a,b,q,i)
term(264) = term(264) + wm_interm_68_triplet_pt2(a,b,q,i) * wm_interm_8_triplet_pt2(a,b,p,i)
term(265) = term(265) + wm_interm_24_triplet_pt2(a,b,p,i) * wm_interm_68_triplet_pt2(a,b,q,i)
term(266) = term(266) + wm_interm_152_triplet_pt2(a,b,i,p) * wm_interm_25_triplet_pt2(b,a,q,i)
term(267) = term(267) + wm_interm_153_triplet_pt2(a,b,i,p) * wm_interm_25_triplet_pt2(b,a,q,i)
term(268) = term(268) + wm_interm_152_triplet_pt2(a,b,i,p) * wm_interm_28_triplet_pt2(b,a,q,i)
term(269) = term(269) + wm_interm_153_triplet_pt2(a,b,i,p) * wm_interm_28_triplet_pt2(b,a,q,i)
term(270) = term(270) + wm_interm_151_triplet_pt2(a,b,i,p) * wm_interm_25_triplet_pt2(b,a,q,i)
term(271) = term(271) + wm_interm_151_triplet_pt2(a,b,i,p) * wm_interm_28_triplet_pt2(b,a,q,i)
term(272) = term(272) + wm_interm_69_triplet_pt2(a,b,q,i) * wm_interm_7_triplet_pt2(a,b,p,i)
term(273) = term(273) + wm_interm_15_triplet_pt2(a,b,p,i) * wm_interm_69_triplet_pt2(a,b,q,i)
term(274) = term(274) + wm_interm_18_triplet_pt2(a,b,p,i) * wm_interm_69_triplet_pt2(a,b,q,i)
term(275) = term(275) + wm_interm_20_triplet_pt2(a,b,p,i) * wm_interm_69_triplet_pt2(a,b,q,i)
term(276) = term(276) + wm_interm_155_triplet_pt2(a,b,i,p) * wm_interm_25_triplet_pt2(b,a,q,i)
term(277) = term(277) + wm_interm_154_triplet_pt2(a,b,i,p) * wm_interm_25_triplet_pt2(b,a,q,i)
term(278) = term(278) + wm_interm_155_triplet_pt2(a,b,i,p) * wm_interm_28_triplet_pt2(b,a,q,i)
term(279) = term(279) + wm_interm_154_triplet_pt2(a,b,i,p) * wm_interm_28_triplet_pt2(b,a,q,i)
term(280) = term(280) + wm_interm_69_triplet_pt2(a,b,q,i) * wm_interm_8_triplet_pt2(a,b,p,i)
term(281) = term(281) + wm_interm_24_triplet_pt2(a,b,p,i) * wm_interm_69_triplet_pt2(a,b,q,i)
term(282) = term(282) + wm_interm_150_triplet_pt2(a,b,i,p) * wm_interm_25_triplet_pt2(b,a,q,i)
term(283) = term(283) + wm_interm_150_triplet_pt2(a,b,i,p) * wm_interm_28_triplet_pt2(b,a,q,i)
term(284) = term(284) + wm_interm_18_triplet_pt2(a,b,p,i) * wm_interm_71_triplet_pt2(a,b,q,i)
term(285) = term(285) + wm_interm_20_triplet_pt2(a,b,p,i) * wm_interm_71_triplet_pt2(a,b,q,i)
term(286) = term(286) + wm_interm_24_triplet_pt2(a,b,p,i) * wm_interm_71_triplet_pt2(a,b,q,i)
term(287) = term(287) + wm_interm_155_triplet_pt2(a,b,i,p) * wm_interm_27_triplet_pt2(b,a,q,i)
term(288) = term(288) + wm_interm_154_triplet_pt2(a,b,i,p) * wm_interm_27_triplet_pt2(b,a,q,i)
term(289) = term(289) + wm_interm_150_triplet_pt2(a,b,i,p) * wm_interm_27_triplet_pt2(b,a,q,i)
term(290) = term(290) + wm_interm_154_triplet_pt2(a,b,i,p) * wm_interm_26_triplet_pt2(b,a,q,i)
term(291) = term(291) + wm_interm_149_triplet_pt2(a,b,i,p) * wm_interm_26_triplet_pt2(b,a,q,i)
term(292) = term(292) + wm_interm_150_triplet_pt2(a,b,i,p) * wm_interm_26_triplet_pt2(b,a,q,i)
term(293) = term(293) + wm_interm_15_triplet_pt2(a,b,p,i) * wm_interm_71_triplet_pt2(a,b,q,i)
term(294) = term(294) + wm_interm_71_triplet_pt2(a,b,q,i) * wm_interm_9_triplet_pt2(a,b,p,i)
term(295) = term(295) + wm_interm_71_triplet_pt2(a,b,q,i) * wm_interm_8_triplet_pt2(a,b,p,i)
term(296) = term(296) + wm_interm_73_triplet_pt2(a,b,q,i) * wm_interm_7_triplet_pt2(a,b,p,i)
term(297) = term(297) + wm_interm_15_triplet_pt2(a,b,p,i) * wm_interm_73_triplet_pt2(a,b,q,i)
term(298) = term(298) + wm_interm_18_triplet_pt2(a,b,p,i) * wm_interm_73_triplet_pt2(a,b,q,i)
term(299) = term(299) + wm_interm_20_triplet_pt2(a,b,p,i) * wm_interm_73_triplet_pt2(a,b,q,i)
term(300) = term(300) + wm_interm_73_triplet_pt2(a,b,q,i) * wm_interm_8_triplet_pt2(a,b,p,i)
term(301) = term(301) + wm_interm_24_triplet_pt2(a,b,p,i) * wm_interm_73_triplet_pt2(a,b,q,i)
term(302) = term(302) + wm_interm_152_triplet_pt2(a,b,i,p) * wm_interm_26_triplet_pt2(b,a,q,i)
term(303) = term(303) + wm_interm_153_triplet_pt2(a,b,i,p) * wm_interm_26_triplet_pt2(b,a,q,i)
term(304) = term(304) + wm_interm_152_triplet_pt2(a,b,i,p) * wm_interm_27_triplet_pt2(b,a,q,i)
term(305) = term(305) + wm_interm_153_triplet_pt2(a,b,i,p) * wm_interm_27_triplet_pt2(b,a,q,i)
term(306) = term(306) + wm_interm_151_triplet_pt2(a,b,i,p) * wm_interm_26_triplet_pt2(b,a,q,i)
term(307) = term(307) + wm_interm_151_triplet_pt2(a,b,i,p) * wm_interm_27_triplet_pt2(b,a,q,i)
term(308) = term(308) + wm_interm_71_triplet_pt2(a,b,q,i) * wm_interm_7_triplet_pt2(a,b,p,i)
term(309) = term(309) + wm_interm_155_triplet_pt2(a,b,i,p) * wm_interm_26_triplet_pt2(b,a,q,i)
term(310) = term(310) + wm_interm_73_triplet_pt2(a,b,q,i) * wm_interm_9_triplet_pt2(a,b,p,i)
term(311) = term(311) + wm_interm_162_triplet_pt2(a,b,i,p) * wm_interm_26_triplet_pt2(b,a,q,i)
term(312) = term(312) + wm_interm_11_triplet_pt2(a,b,p,i) * wm_interm_68_triplet_pt2(a,b,q,i)
term(313) = term(313) + wm_interm_12_triplet_pt2(a,b,p,i) * wm_interm_68_triplet_pt2(a,b,q,i)
term(314) = term(314) + wm_interm_35_triplet_pt2(a,b,p,i) * wm_interm_68_triplet_pt2(a,b,q,i)
term(315) = term(315) + wm_interm_37_triplet_pt2(a,b,p,i) * wm_interm_68_triplet_pt2(a,b,q,i)
term(316) = term(316) + wm_interm_171_triplet_pt2(a,b,i,p) * wm_interm_25_triplet_pt2(b,a,q,i)
term(317) = term(317) + wm_interm_172_triplet_pt2(a,b,i,p) * wm_interm_25_triplet_pt2(b,a,q,i)
term(318) = term(318) + wm_interm_171_triplet_pt2(a,b,i,p) * wm_interm_28_triplet_pt2(b,a,q,i)
term(319) = term(319) + wm_interm_172_triplet_pt2(a,b,i,p) * wm_interm_28_triplet_pt2(b,a,q,i)
term(320) = term(320) + wm_interm_11_triplet_pt2(a,b,p,i) * wm_interm_69_triplet_pt2(a,b,q,i)
term(321) = term(321) + wm_interm_12_triplet_pt2(a,b,p,i) * wm_interm_69_triplet_pt2(a,b,q,i)
term(322) = term(322) + wm_interm_35_triplet_pt2(a,b,p,i) * wm_interm_69_triplet_pt2(a,b,q,i)
term(323) = term(323) + wm_interm_37_triplet_pt2(a,b,p,i) * wm_interm_69_triplet_pt2(a,b,q,i)
term(324) = term(324) + wm_interm_170_triplet_pt2(a,b,i,p) * wm_interm_25_triplet_pt2(b,a,q,i)
term(325) = term(325) + wm_interm_169_triplet_pt2(a,b,i,p) * wm_interm_25_triplet_pt2(b,a,q,i)
term(326) = term(326) + wm_interm_170_triplet_pt2(a,b,i,p) * wm_interm_28_triplet_pt2(b,a,q,i)
term(327) = term(327) + wm_interm_169_triplet_pt2(a,b,i,p) * wm_interm_28_triplet_pt2(b,a,q,i)
term(328) = term(328) + wm_interm_35_triplet_pt2(a,b,p,i) * wm_interm_71_triplet_pt2(a,b,q,i)
term(329) = term(329) + wm_interm_37_triplet_pt2(a,b,p,i) * wm_interm_71_triplet_pt2(a,b,q,i)
term(330) = term(330) + wm_interm_170_triplet_pt2(a,b,i,p) * wm_interm_27_triplet_pt2(b,a,q,i)
term(331) = term(331) + wm_interm_169_triplet_pt2(a,b,i,p) * wm_interm_27_triplet_pt2(b,a,q,i)
term(332) = term(332) + wm_interm_169_triplet_pt2(a,b,i,p) * wm_interm_26_triplet_pt2(b,a,q,i)
term(333) = term(333) + wm_interm_170_triplet_pt2(a,b,i,p) * wm_interm_26_triplet_pt2(b,a,q,i)
term(334) = term(334) + wm_interm_12_triplet_pt2(a,b,p,i) * wm_interm_71_triplet_pt2(a,b,q,i)
term(335) = term(335) + wm_interm_11_triplet_pt2(a,b,p,i) * wm_interm_71_triplet_pt2(a,b,q,i)
term(336) = term(336) + wm_interm_11_triplet_pt2(a,b,p,i) * wm_interm_73_triplet_pt2(a,b,q,i)
term(337) = term(337) + wm_interm_12_triplet_pt2(a,b,p,i) * wm_interm_73_triplet_pt2(a,b,q,i)
term(338) = term(338) + wm_interm_35_triplet_pt2(a,b,p,i) * wm_interm_73_triplet_pt2(a,b,q,i)
term(339) = term(339) + wm_interm_37_triplet_pt2(a,b,p,i) * wm_interm_73_triplet_pt2(a,b,q,i)
term(340) = term(340) + wm_interm_171_triplet_pt2(a,b,i,p) * wm_interm_26_triplet_pt2(b,a,q,i)
term(341) = term(341) + wm_interm_172_triplet_pt2(a,b,i,p) * wm_interm_26_triplet_pt2(b,a,q,i)
term(342) = term(342) + wm_interm_171_triplet_pt2(a,b,i,p) * wm_interm_27_triplet_pt2(b,a,q,i)
term(343) = term(343) + wm_interm_172_triplet_pt2(a,b,i,p) * wm_interm_27_triplet_pt2(b,a,q,i)
term(344) = term(344) + wm_interm_150_triplet_pt2(a,b,p,i) * wm_interm_25_triplet_pt2(b,a,i,q)
term(345) = term(345) + wm_interm_151_triplet_pt2(a,b,p,i) * wm_interm_25_triplet_pt2(b,a,i,q)
term(346) = term(346) + wm_interm_155_triplet_pt2(a,b,p,i) * wm_interm_25_triplet_pt2(b,a,i,q)
term(347) = term(347) + wm_interm_152_triplet_pt2(a,b,p,i) * wm_interm_25_triplet_pt2(b,a,i,q)
term(348) = term(348) + wm_interm_154_triplet_pt2(a,b,p,i) * wm_interm_25_triplet_pt2(b,a,i,q)
term(349) = term(349) + wm_interm_153_triplet_pt2(a,b,p,i) * wm_interm_25_triplet_pt2(b,a,i,q)
term(350) = term(350) + wm_interm_73_triplet_pt2(a,b,i,q) * wm_interm_8_triplet_pt2(a,b,i,p)
term(351) = term(351) + wm_interm_73_triplet_pt2(a,b,i,q) * wm_interm_9_triplet_pt2(a,b,i,p)
term(352) = term(352) + wm_interm_71_triplet_pt2(a,b,i,q) * wm_interm_9_triplet_pt2(a,b,i,p)
term(353) = term(353) + wm_interm_71_triplet_pt2(a,b,i,q) * wm_interm_8_triplet_pt2(a,b,i,p)
term(354) = term(354) + wm_interm_15_triplet_pt2(a,b,i,p) * wm_interm_71_triplet_pt2(a,b,i,q)
term(355) = term(355) + wm_interm_71_triplet_pt2(a,b,i,q) * wm_interm_7_triplet_pt2(a,b,i,p)
term(356) = term(356) + wm_interm_73_triplet_pt2(a,b,i,q) * wm_interm_7_triplet_pt2(a,b,i,p)
term(357) = term(357) + wm_interm_15_triplet_pt2(a,b,i,p) * wm_interm_73_triplet_pt2(a,b,i,q)
term(358) = term(358) + wm_interm_24_triplet_pt2(a,b,i,p) * wm_interm_71_triplet_pt2(a,b,i,q)
term(359) = term(359) + wm_interm_150_triplet_pt2(a,b,p,i) * wm_interm_28_triplet_pt2(b,a,i,q)
term(360) = term(360) + wm_interm_24_triplet_pt2(a,b,i,p) * wm_interm_73_triplet_pt2(a,b,i,q)
term(361) = term(361) + wm_interm_151_triplet_pt2(a,b,p,i) * wm_interm_28_triplet_pt2(b,a,i,q)
term(362) = term(362) + wm_interm_18_triplet_pt2(a,b,i,p) * wm_interm_71_triplet_pt2(a,b,i,q)
term(363) = term(363) + wm_interm_155_triplet_pt2(a,b,p,i) * wm_interm_28_triplet_pt2(b,a,i,q)
term(364) = term(364) + wm_interm_18_triplet_pt2(a,b,i,p) * wm_interm_73_triplet_pt2(a,b,i,q)
term(365) = term(365) + wm_interm_152_triplet_pt2(a,b,p,i) * wm_interm_28_triplet_pt2(b,a,i,q)
term(366) = term(366) + wm_interm_20_triplet_pt2(a,b,i,p) * wm_interm_71_triplet_pt2(a,b,i,q)
term(367) = term(367) + wm_interm_154_triplet_pt2(a,b,p,i) * wm_interm_28_triplet_pt2(b,a,i,q)
term(368) = term(368) + wm_interm_20_triplet_pt2(a,b,i,p) * wm_interm_73_triplet_pt2(a,b,i,q)
term(369) = term(369) + wm_interm_153_triplet_pt2(a,b,p,i) * wm_interm_28_triplet_pt2(b,a,i,q)
term(370) = term(370) + wm_interm_150_triplet_pt2(a,b,p,i) * wm_interm_27_triplet_pt2(b,a,i,q)
term(371) = term(371) + wm_interm_151_triplet_pt2(a,b,p,i) * wm_interm_27_triplet_pt2(b,a,i,q)
term(372) = term(372) + wm_interm_155_triplet_pt2(a,b,p,i) * wm_interm_27_triplet_pt2(b,a,i,q)
term(373) = term(373) + wm_interm_152_triplet_pt2(a,b,p,i) * wm_interm_27_triplet_pt2(b,a,i,q)
term(374) = term(374) + wm_interm_154_triplet_pt2(a,b,p,i) * wm_interm_27_triplet_pt2(b,a,i,q)
term(375) = term(375) + wm_interm_153_triplet_pt2(a,b,p,i) * wm_interm_27_triplet_pt2(b,a,i,q)
term(376) = term(376) + wm_interm_151_triplet_pt2(a,b,p,i) * wm_interm_26_triplet_pt2(b,a,i,q)
term(377) = term(377) + wm_interm_162_triplet_pt2(a,b,p,i) * wm_interm_26_triplet_pt2(b,a,i,q)
term(378) = term(378) + wm_interm_149_triplet_pt2(a,b,p,i) * wm_interm_26_triplet_pt2(b,a,i,q)
term(379) = term(379) + wm_interm_150_triplet_pt2(a,b,p,i) * wm_interm_26_triplet_pt2(b,a,i,q)
term(380) = term(380) + wm_interm_154_triplet_pt2(a,b,p,i) * wm_interm_26_triplet_pt2(b,a,i,q)
term(381) = term(381) + wm_interm_155_triplet_pt2(a,b,p,i) * wm_interm_26_triplet_pt2(b,a,i,q)
term(382) = term(382) + wm_interm_152_triplet_pt2(a,b,p,i) * wm_interm_26_triplet_pt2(b,a,i,q)
term(383) = term(383) + wm_interm_153_triplet_pt2(a,b,p,i) * wm_interm_26_triplet_pt2(b,a,i,q)
term(384) = term(384) + wm_interm_69_triplet_pt2(a,b,i,q) * wm_interm_8_triplet_pt2(a,b,i,p)
term(385) = term(385) + wm_interm_68_triplet_pt2(a,b,i,q) * wm_interm_8_triplet_pt2(a,b,i,p)
term(386) = term(386) + wm_interm_69_triplet_pt2(a,b,i,q) * wm_interm_7_triplet_pt2(a,b,i,p)
term(387) = term(387) + wm_interm_68_triplet_pt2(a,b,i,q) * wm_interm_7_triplet_pt2(a,b,i,p)
term(388) = term(388) + wm_interm_15_triplet_pt2(a,b,i,p) * wm_interm_69_triplet_pt2(a,b,i,q)
term(389) = term(389) + wm_interm_15_triplet_pt2(a,b,i,p) * wm_interm_68_triplet_pt2(a,b,i,q)
term(390) = term(390) + wm_interm_24_triplet_pt2(a,b,i,p) * wm_interm_69_triplet_pt2(a,b,i,q)
term(391) = term(391) + wm_interm_24_triplet_pt2(a,b,i,p) * wm_interm_68_triplet_pt2(a,b,i,q)
term(392) = term(392) + wm_interm_18_triplet_pt2(a,b,i,p) * wm_interm_69_triplet_pt2(a,b,i,q)
term(393) = term(393) + wm_interm_18_triplet_pt2(a,b,i,p) * wm_interm_68_triplet_pt2(a,b,i,q)
term(394) = term(394) + wm_interm_20_triplet_pt2(a,b,i,p) * wm_interm_69_triplet_pt2(a,b,i,q)
term(395) = term(395) + wm_interm_20_triplet_pt2(a,b,i,p) * wm_interm_68_triplet_pt2(a,b,i,q)
term(396) = term(396) + wm_interm_170_triplet_pt2(a,b,p,i) * wm_interm_25_triplet_pt2(b,a,i,q)
term(397) = term(397) + wm_interm_171_triplet_pt2(a,b,p,i) * wm_interm_25_triplet_pt2(b,a,i,q)
term(398) = term(398) + wm_interm_169_triplet_pt2(a,b,p,i) * wm_interm_25_triplet_pt2(b,a,i,q)
term(399) = term(399) + wm_interm_172_triplet_pt2(a,b,p,i) * wm_interm_25_triplet_pt2(b,a,i,q)
term(400) = term(400) + wm_interm_11_triplet_pt2(a,b,i,p) * wm_interm_73_triplet_pt2(a,b,i,q)
term(401) = term(401) + wm_interm_12_triplet_pt2(a,b,i,p) * wm_interm_73_triplet_pt2(a,b,i,q)
term(402) = term(402) + wm_interm_12_triplet_pt2(a,b,i,p) * wm_interm_71_triplet_pt2(a,b,i,q)
term(403) = term(403) + wm_interm_11_triplet_pt2(a,b,i,p) * wm_interm_71_triplet_pt2(a,b,i,q)
term(404) = term(404) + wm_interm_35_triplet_pt2(a,b,i,p) * wm_interm_71_triplet_pt2(a,b,i,q)
term(405) = term(405) + wm_interm_170_triplet_pt2(a,b,p,i) * wm_interm_28_triplet_pt2(b,a,i,q)
term(406) = term(406) + wm_interm_35_triplet_pt2(a,b,i,p) * wm_interm_73_triplet_pt2(a,b,i,q)
term(407) = term(407) + wm_interm_171_triplet_pt2(a,b,p,i) * wm_interm_28_triplet_pt2(b,a,i,q)
term(408) = term(408) + wm_interm_37_triplet_pt2(a,b,i,p) * wm_interm_71_triplet_pt2(a,b,i,q)
term(409) = term(409) + wm_interm_169_triplet_pt2(a,b,p,i) * wm_interm_28_triplet_pt2(b,a,i,q)
term(410) = term(410) + wm_interm_37_triplet_pt2(a,b,i,p) * wm_interm_73_triplet_pt2(a,b,i,q)
term(411) = term(411) + wm_interm_172_triplet_pt2(a,b,p,i) * wm_interm_28_triplet_pt2(b,a,i,q)
term(412) = term(412) + wm_interm_170_triplet_pt2(a,b,p,i) * wm_interm_27_triplet_pt2(b,a,i,q)
term(413) = term(413) + wm_interm_171_triplet_pt2(a,b,p,i) * wm_interm_27_triplet_pt2(b,a,i,q)
term(414) = term(414) + wm_interm_169_triplet_pt2(a,b,p,i) * wm_interm_27_triplet_pt2(b,a,i,q)
term(415) = term(415) + wm_interm_172_triplet_pt2(a,b,p,i) * wm_interm_27_triplet_pt2(b,a,i,q)
term(416) = term(416) + wm_interm_171_triplet_pt2(a,b,p,i) * wm_interm_26_triplet_pt2(b,a,i,q)
term(417) = term(417) + wm_interm_172_triplet_pt2(a,b,p,i) * wm_interm_26_triplet_pt2(b,a,i,q)
term(418) = term(418) + wm_interm_169_triplet_pt2(a,b,p,i) * wm_interm_26_triplet_pt2(b,a,i,q)
term(419) = term(419) + wm_interm_170_triplet_pt2(a,b,p,i) * wm_interm_26_triplet_pt2(b,a,i,q)
term(420) = term(420) + wm_interm_11_triplet_pt2(a,b,i,p) * wm_interm_69_triplet_pt2(a,b,i,q)
term(421) = term(421) + wm_interm_11_triplet_pt2(a,b,i,p) * wm_interm_68_triplet_pt2(a,b,i,q)
term(422) = term(422) + wm_interm_12_triplet_pt2(a,b,i,p) * wm_interm_69_triplet_pt2(a,b,i,q)
term(423) = term(423) + wm_interm_12_triplet_pt2(a,b,i,p) * wm_interm_68_triplet_pt2(a,b,i,q)
term(424) = term(424) + wm_interm_35_triplet_pt2(a,b,i,p) * wm_interm_69_triplet_pt2(a,b,i,q)
term(425) = term(425) + wm_interm_35_triplet_pt2(a,b,i,p) * wm_interm_68_triplet_pt2(a,b,i,q)
term(426) = term(426) + wm_interm_37_triplet_pt2(a,b,i,p) * wm_interm_69_triplet_pt2(a,b,i,q)
term(427) = term(427) + wm_interm_37_triplet_pt2(a,b,i,p) * wm_interm_68_triplet_pt2(a,b,i,q)
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
term(8) = term(8) * (2.0d+0) 
term(9) = term(9) * (-4.0d+0) 
term(10) = term(10) * (-1.0d+0) 
term(11) = term(11) * (2.0d+0) 
term(12) = term(12) * (0.5d+0) 
term(13) = term(13) * (-1.0d+0) 
term(14) = term(14) * (-1.0d+0) 
term(15) = term(15) * (2.0d+0) 
term(16) = term(16) * (0.5d+0) 
term(17) = term(17) * (-1.0d+0) 
term(18) = term(18) * (-1.0d+0) 
term(19) = term(19) * (2.0d+0) 
term(20) = term(20) * (0.5d+0) 
term(21) = term(21) * (-1.0d+0) 
term(22) = term(22) * (0.5d+0) 
term(23) = term(23) * (-1.0d+0) 
term(24) = term(24) * (0.5d+0) 
term(25) = term(25) * (-1.0d+0) 
term(26) = term(26) * (-1.0d+0) 
term(27) = term(27) * (2.0d+0) 
term(28) = term(28) * (0.5d+0) 
term(29) = term(29) * (-1.0d+0) 
term(30) = term(30) * (0.5d+0) 
term(31) = term(31) * (-1.0d+0) 
term(32) = term(32) * (-1.0d+0) 
term(33) = term(33) * (2.0d+0) 
term(34) = term(34) * (0.5d+0) 
term(35) = term(35) * (-1.0d+0) 
term(36) = term(36) * (0.5d+0) 
term(37) = term(37) * (-1.0d+0) 
term(38) = term(38) * (0.5d+0) 
term(39) = term(39) * (0.5d+0) 
term(40) = term(40) * (-1.0d+0) 
term(41) = term(41) * (0.5d+0) 
term(42) = term(42) * (0.5d+0) 
term(43) = term(43) * (0.5d+0) 
term(44) = term(44) * (-1.0d+0) 
term(45) = term(45) * (0.5d+0) 
term(46) = term(46) * (0.5d+0) 
term(47) = term(47) * (-1.0d+0) 
term(48) = term(48) * (0.5d+0) 
term(49) = term(49) * (-1.0d+0) 
term(50) = term(50) * (-1.0d+0) 
term(51) = term(51) * (2.0d+0) 
term(52) = term(52) * (0.5d+0) 
term(53) = term(53) * (-1.0d+0) 
term(54) = term(54) * (0.5d+0) 
term(55) = term(55) * (-1.0d+0) 
term(56) = term(56) * (-1.0d+0) 
term(57) = term(57) * (2.0d+0) 
term(58) = term(58) * (0.5d+0) 
term(59) = term(59) * (-1.0d+0) 
term(60) = term(60) * (0.5d+0) 
term(61) = term(61) * (-1.0d+0) 
term(62) = term(62) * (0.5d+0) 
term(63) = term(63) * (-1.0d+0) 
term(64) = term(64) * (0.5d+0) 
term(65) = term(65) * (-1.0d+0) 
term(66) = term(66) * (0.5d+0) 
term(67) = term(67) * (-1.0d+0) 
term(68) = term(68) * (0.5d+0) 
term(69) = term(69) * (0.5d+0) 
term(70) = term(70) * (0.5d+0) 
term(71) = term(71) * (0.5d+0) 
term(72) = term(72) * (0.5d+0) 
term(73) = term(73) * (0.5d+0) 
term(74) = term(74) * (-1.0d+0) 
term(75) = term(75) * (0.5d+0) 
term(76) = term(76) * (0.5d+0) 
term(77) = term(77) * (-1.0d+0) 
term(78) = term(78) * (-4.0d+0) 
term(79) = term(79) * (4.0d+0) 
term(80) = term(80) * (8.0d+0) 
term(81) = term(81) * (-8.0d+0) 
term(82) = term(82) * (-4.0d+0) 
term(83) = term(83) * (4.0d+0) 
term(84) = term(84) * (8.0d+0) 
term(85) = term(85) * (-8.0d+0) 
term(86) = term(86) * (2.0d+0) 
term(87) = term(87) * (-2.0d+0) 
term(88) = term(88) * (-4.0d+0) 
term(89) = term(89) * (4.0d+0) 
term(90) = term(90) * (2.0d+0) 
term(91) = term(91) * (-2.0d+0) 
term(92) = term(92) * (-4.0d+0) 
term(93) = term(93) * (4.0d+0) 
term(94) = term(94) * (2.0d+0) 
term(95) = term(95) * (-2.0d+0) 
term(96) = term(96) * (-4.0d+0) 
term(97) = term(97) * (4.0d+0) 
term(98) = term(98) * (2.0d+0) 
term(99) = term(99) * (-2.0d+0) 
term(100) = term(100) * (-4.0d+0) 
term(101) = term(101) * (4.0d+0) 
term(102) = term(102) * (2.0d+0) 
term(103) = term(103) * (-2.0d+0) 
term(104) = term(104) * (2.0d+0) 
term(105) = term(105) * (-2.0d+0) 
term(106) = term(106) * (2.0d+0) 
term(107) = term(107) * (-2.0d+0) 
term(108) = term(108) * (2.0d+0) 
term(109) = term(109) * (-2.0d+0) 
term(110) = term(110) * (2.0d+0) 
term(111) = term(111) * (-2.0d+0) 
term(112) = term(112) * (-4.0d+0) 
term(113) = term(113) * (4.0d+0) 
term(114) = term(114) * (2.0d+0) 
term(115) = term(115) * (-2.0d+0) 
term(116) = term(116) * (-4.0d+0) 
term(117) = term(117) * (4.0d+0) 
term(118) = term(118) * (2.0d+0) 
term(119) = term(119) * (-2.0d+0) 
term(120) = term(120) * (2.0d+0) 
term(121) = term(121) * (-2.0d+0) 
term(122) = term(122) * (2.0d+0) 
term(123) = term(123) * (-2.0d+0) 
term(124) = term(124) * (2.0d+0) 
term(125) = term(125) * (-2.0d+0) 
term(126) = term(126) * (2.0d+0) 
term(127) = term(127) * (-2.0d+0) 
term(128) = term(128) * (2.0d+0) 
term(129) = term(129) * (-2.0d+0) 
term(130) = term(130) * (0.5d+0) 
term(131) = term(131) * (0.5d+0) 
term(132) = term(132) * (-1.0d+0) 
term(133) = term(133) * (0.5d+0) 
term(134) = term(134) * (0.5d+0) 
term(135) = term(135) * (-1.0d+0) 
term(136) = term(136) * (-1.0d+0) 
term(137) = term(137) * (-1.0d+0) 
term(138) = term(138) * (2.0d+0) 
term(139) = term(139) * (0.5d+0) 
term(140) = term(140) * (-1.0d+0) 
term(141) = term(141) * (0.5d+0) 
term(142) = term(142) * (0.5d+0) 
term(143) = term(143) * (-1.0d+0) 
term(144) = term(144) * (0.5d+0) 
term(145) = term(145) * (0.5d+0) 
term(146) = term(146) * (-1.0d+0) 
term(147) = term(147) * (0.5d+0) 
term(148) = term(148) * (0.5d+0) 
term(149) = term(149) * (-1.0d+0) 
term(150) = term(150) * (0.5d+0) 
term(151) = term(151) * (0.5d+0) 
term(152) = term(152) * (0.5d+0) 
term(153) = term(153) * (-1.0d+0) 
term(154) = term(154) * (-1.0d+0) 
term(155) = term(155) * (-1.0d+0) 
term(156) = term(156) * (2.0d+0) 
term(157) = term(157) * (0.5d+0) 
term(158) = term(158) * (0.5d+0) 
term(159) = term(159) * (-1.0d+0) 
term(160) = term(160) * (-1.0d+0) 
term(161) = term(161) * (-1.0d+0) 
term(162) = term(162) * (2.0d+0) 
term(163) = term(163) * (-1.0d+0) 
term(164) = term(164) * (-1.0d+0) 
term(165) = term(165) * (2.0d+0) 
term(166) = term(166) * (2.0d+0) 
term(167) = term(167) * (2.0d+0) 
term(168) = term(168) * (-4.0d+0) 
term(169) = term(169) * (0.5d+0) 
term(170) = term(170) * (0.5d+0) 
term(171) = term(171) * (-1.0d+0) 
term(172) = term(172) * (0.5d+0) 
term(173) = term(173) * (0.5d+0) 
term(174) = term(174) * (-1.0d+0) 
term(175) = term(175) * (-1.0d+0) 
term(176) = term(176) * (-1.0d+0) 
term(177) = term(177) * (2.0d+0) 
term(178) = term(178) * (0.5d+0) 
term(179) = term(179) * (-1.0d+0) 
term(180) = term(180) * (0.5d+0) 
term(181) = term(181) * (0.5d+0) 
term(182) = term(182) * (-1.0d+0) 
term(183) = term(183) * (0.5d+0) 
term(184) = term(184) * (0.5d+0) 
term(185) = term(185) * (-1.0d+0) 
term(186) = term(186) * (0.5d+0) 
term(187) = term(187) * (0.5d+0) 
term(188) = term(188) * (-1.0d+0) 
term(189) = term(189) * (0.5d+0) 
term(190) = term(190) * (0.5d+0) 
term(191) = term(191) * (0.5d+0) 
term(192) = term(192) * (-1.0d+0) 
term(193) = term(193) * (0.5d+0) 
term(194) = term(194) * (0.5d+0) 
term(195) = term(195) * (-1.0d+0) 
term(196) = term(196) * (-1.0d+0) 
term(197) = term(197) * (-1.0d+0) 
term(198) = term(198) * (2.0d+0) 
term(199) = term(199) * (-1.0d+0) 
term(200) = term(200) * (-1.0d+0) 
term(201) = term(201) * (2.0d+0) 
term(202) = term(202) * (-1.0d+0) 
term(203) = term(203) * (-1.0d+0) 
term(204) = term(204) * (2.0d+0) 
term(205) = term(205) * (2.0d+0) 
term(206) = term(206) * (2.0d+0) 
term(207) = term(207) * (-4.0d+0) 
term(208) = term(208) * (2.0d+0) 
term(209) = term(209) * (2.0d+0) 
term(210) = term(210) * (-4.0d+0) 
term(211) = term(211) * (-2.0d+0) 
term(212) = term(212) * (-2.0d+0) 
term(213) = term(213) * (4.0d+0) 
term(214) = term(214) * (2.0d+0) 
term(215) = term(215) * (-2.0d+0) 
term(216) = term(216) * (2.0d+0) 
term(217) = term(217) * (2.0d+0) 
term(218) = term(218) * (-2.0d+0) 
term(219) = term(219) * (2.0d+0) 
term(220) = term(220) * (-2.0d+0) 
term(221) = term(221) * (-2.0d+0) 
term(222) = term(222) * (2.0d+0) 
term(223) = term(223) * (2.0d+0) 
term(224) = term(224) * (-4.0d+0) 
term(225) = term(225) * (-4.0d+0) 
term(226) = term(226) * (-4.0d+0) 
term(227) = term(227) * (8.0d+0) 
term(228) = term(228) * (-2.0d+0) 
term(229) = term(229) * (-2.0d+0) 
term(230) = term(230) * (4.0d+0) 
term(231) = term(231) * (4.0d+0) 
term(232) = term(232) * (4.0d+0) 
term(233) = term(233) * (-8.0d+0) 
term(234) = term(234) * (2.0d+0) 
term(235) = term(235) * (2.0d+0) 
term(236) = term(236) * (-4.0d+0) 
term(237) = term(237) * (-2.0d+0) 
term(238) = term(238) * (-2.0d+0) 
term(239) = term(239) * (4.0d+0) 
term(240) = term(240) * (2.0d+0) 
term(241) = term(241) * (-2.0d+0) 
term(242) = term(242) * (2.0d+0) 
term(243) = term(243) * (2.0d+0) 
term(244) = term(244) * (-2.0d+0) 
term(245) = term(245) * (2.0d+0) 
term(246) = term(246) * (-2.0d+0) 
term(247) = term(247) * (-2.0d+0) 
term(248) = term(248) * (2.0d+0) 
term(249) = term(249) * (2.0d+0) 
term(250) = term(250) * (-4.0d+0) 
term(251) = term(251) * (-2.0d+0) 
term(252) = term(252) * (-2.0d+0) 
term(253) = term(253) * (4.0d+0) 
term(254) = term(254) * (-4.0d+0) 
term(255) = term(255) * (-4.0d+0) 
term(256) = term(256) * (8.0d+0) 
term(257) = term(257) * (4.0d+0) 
term(258) = term(258) * (4.0d+0) 
term(259) = term(259) * (-8.0d+0) 
term(260) = term(260) * (-2.0d+0) 
term(261) = term(261) * (4.0d+0) 
term(262) = term(262) * (4.0d+0) 
term(263) = term(263) * (-8.0d+0) 
term(264) = term(264) * (-2.0d+0) 
term(265) = term(265) * (4.0d+0) 
term(266) = term(266) * (-2.0d+0) 
term(267) = term(267) * (4.0d+0) 
term(268) = term(268) * (4.0d+0) 
term(269) = term(269) * (-8.0d+0) 
term(270) = term(270) * (-2.0d+0) 
term(271) = term(271) * (4.0d+0) 
term(272) = term(272) * (2.0d+0) 
term(273) = term(273) * (-4.0d+0) 
term(274) = term(274) * (-4.0d+0) 
term(275) = term(275) * (8.0d+0) 
term(276) = term(276) * (2.0d+0) 
term(277) = term(277) * (-4.0d+0) 
term(278) = term(278) * (-4.0d+0) 
term(279) = term(279) * (8.0d+0) 
term(280) = term(280) * (2.0d+0) 
term(281) = term(281) * (-4.0d+0) 
term(282) = term(282) * (2.0d+0) 
term(283) = term(283) * (-4.0d+0) 
term(284) = term(284) * (2.0d+0) 
term(285) = term(285) * (-4.0d+0) 
term(286) = term(286) * (2.0d+0) 
term(287) = term(287) * (2.0d+0) 
term(288) = term(288) * (-4.0d+0) 
term(289) = term(289) * (2.0d+0) 
term(290) = term(290) * (2.0d+0) 
term(291) = term(291) * (2.0d+0) 
term(292) = term(292) * (-2.0d+0) 
term(293) = term(293) * (2.0d+0) 
term(294) = term(294) * (2.0d+0) 
term(295) = term(295) * (-2.0d+0) 
term(296) = term(296) * (2.0d+0) 
term(297) = term(297) * (-2.0d+0) 
term(298) = term(298) * (-2.0d+0) 
term(299) = term(299) * (4.0d+0) 
term(300) = term(300) * (2.0d+0) 
term(301) = term(301) * (-2.0d+0) 
term(302) = term(302) * (2.0d+0) 
term(303) = term(303) * (-2.0d+0) 
term(304) = term(304) * (-2.0d+0) 
term(305) = term(305) * (4.0d+0) 
term(306) = term(306) * (2.0d+0) 
term(307) = term(307) * (-2.0d+0) 
term(308) = term(308) * (-2.0d+0) 
term(309) = term(309) * (-2.0d+0) 
term(310) = term(310) * (-2.0d+0) 
term(311) = term(311) * (-2.0d+0) 
term(312) = term(312) * (-8.0d+0) 
term(313) = term(313) * (8.0d+0) 
term(314) = term(314) * (16.0d+0) 
term(315) = term(315) * (-16.0d+0) 
term(316) = term(316) * (-8.0d+0) 
term(317) = term(317) * (8.0d+0) 
term(318) = term(318) * (16.0d+0) 
term(319) = term(319) * (-16.0d+0) 
term(320) = term(320) * (8.0d+0) 
term(321) = term(321) * (-8.0d+0) 
term(322) = term(322) * (-16.0d+0) 
term(323) = term(323) * (16.0d+0) 
term(324) = term(324) * (8.0d+0) 
term(325) = term(325) * (-8.0d+0) 
term(326) = term(326) * (-16.0d+0) 
term(327) = term(327) * (16.0d+0) 
term(328) = term(328) * (8.0d+0) 
term(329) = term(329) * (-8.0d+0) 
term(330) = term(330) * (8.0d+0) 
term(331) = term(331) * (-8.0d+0) 
term(332) = term(332) * (8.0d+0) 
term(333) = term(333) * (-8.0d+0) 
term(334) = term(334) * (8.0d+0) 
term(335) = term(335) * (-8.0d+0) 
term(336) = term(336) * (8.0d+0) 
term(337) = term(337) * (-8.0d+0) 
term(338) = term(338) * (-8.0d+0) 
term(339) = term(339) * (8.0d+0) 
term(340) = term(340) * (8.0d+0) 
term(341) = term(341) * (-8.0d+0) 
term(342) = term(342) * (-8.0d+0) 
term(343) = term(343) * (8.0d+0) 
term(344) = term(344) * (2.0d+0) 
term(345) = term(345) * (-2.0d+0) 
term(346) = term(346) * (2.0d+0) 
term(347) = term(347) * (-2.0d+0) 
term(348) = term(348) * (-4.0d+0) 
term(349) = term(349) * (4.0d+0) 
term(350) = term(350) * (2.0d+0) 
term(351) = term(351) * (-2.0d+0) 
term(352) = term(352) * (2.0d+0) 
term(353) = term(353) * (-2.0d+0) 
term(354) = term(354) * (2.0d+0) 
term(355) = term(355) * (-2.0d+0) 
term(356) = term(356) * (2.0d+0) 
term(357) = term(357) * (-2.0d+0) 
term(358) = term(358) * (2.0d+0) 
term(359) = term(359) * (-4.0d+0) 
term(360) = term(360) * (-2.0d+0) 
term(361) = term(361) * (4.0d+0) 
term(362) = term(362) * (2.0d+0) 
term(363) = term(363) * (-4.0d+0) 
term(364) = term(364) * (-2.0d+0) 
term(365) = term(365) * (4.0d+0) 
term(366) = term(366) * (-4.0d+0) 
term(367) = term(367) * (8.0d+0) 
term(368) = term(368) * (4.0d+0) 
term(369) = term(369) * (-8.0d+0) 
term(370) = term(370) * (2.0d+0) 
term(371) = term(371) * (-2.0d+0) 
term(372) = term(372) * (2.0d+0) 
term(373) = term(373) * (-2.0d+0) 
term(374) = term(374) * (-4.0d+0) 
term(375) = term(375) * (4.0d+0) 
term(376) = term(376) * (2.0d+0) 
term(377) = term(377) * (-2.0d+0) 
term(378) = term(378) * (2.0d+0) 
term(379) = term(379) * (-2.0d+0) 
term(380) = term(380) * (2.0d+0) 
term(381) = term(381) * (-2.0d+0) 
term(382) = term(382) * (2.0d+0) 
term(383) = term(383) * (-2.0d+0) 
term(384) = term(384) * (2.0d+0) 
term(385) = term(385) * (-2.0d+0) 
term(386) = term(386) * (2.0d+0) 
term(387) = term(387) * (-2.0d+0) 
term(388) = term(388) * (-4.0d+0) 
term(389) = term(389) * (4.0d+0) 
term(390) = term(390) * (-4.0d+0) 
term(391) = term(391) * (4.0d+0) 
term(392) = term(392) * (-4.0d+0) 
term(393) = term(393) * (4.0d+0) 
term(394) = term(394) * (8.0d+0) 
term(395) = term(395) * (-8.0d+0) 
term(396) = term(396) * (8.0d+0) 
term(397) = term(397) * (-8.0d+0) 
term(398) = term(398) * (-8.0d+0) 
term(399) = term(399) * (8.0d+0) 
term(400) = term(400) * (8.0d+0) 
term(401) = term(401) * (-8.0d+0) 
term(402) = term(402) * (8.0d+0) 
term(403) = term(403) * (-8.0d+0) 
term(404) = term(404) * (8.0d+0) 
term(405) = term(405) * (-16.0d+0) 
term(406) = term(406) * (-8.0d+0) 
term(407) = term(407) * (16.0d+0) 
term(408) = term(408) * (-8.0d+0) 
term(409) = term(409) * (16.0d+0) 
term(410) = term(410) * (8.0d+0) 
term(411) = term(411) * (-16.0d+0) 
term(412) = term(412) * (8.0d+0) 
term(413) = term(413) * (-8.0d+0) 
term(414) = term(414) * (-8.0d+0) 
term(415) = term(415) * (8.0d+0) 
term(416) = term(416) * (8.0d+0) 
term(417) = term(417) * (-8.0d+0) 
term(418) = term(418) * (8.0d+0) 
term(419) = term(419) * (-8.0d+0) 
term(420) = term(420) * (8.0d+0) 
term(421) = term(421) * (-8.0d+0) 
term(422) = term(422) * (-8.0d+0) 
term(423) = term(423) * (8.0d+0) 
term(424) = term(424) * (-16.0d+0) 
term(425) = term(425) * (16.0d+0) 
term(426) = term(426) * (16.0d+0) 
term(427) = term(427) * (-16.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
term(428) = term(428) + wm_interm_16_triplet_pt2(i,j,k,q) * wm_interm_94_triplet_pt2(k,p,j,i)
term(429) = term(429) + wm_interm_16_triplet_pt2(i,j,k,q) * wm_interm_94_triplet_pt2(p,k,j,i)
term(430) = term(430) + wm_interm_16_triplet_pt2(i,j,k,q) * wm_interm_95_triplet_pt2(p,k,j,i)
end do 
end do 
end do 

term(428) = term(428) * (0.5d+0) 
term(429) = term(429) * (-1.0d+0) 
term(430) = term(430) * (0.5d+0) 

do i = 1, nocc 
do j = 1, nocc 
term(431) = term(431) + wm_interm_1_triplet_pt2(i,j) * wm_interm_94_triplet_pt2(q,j,i,p)
term(432) = term(432) + wm_interm_1_triplet_pt2(i,j) * wm_interm_94_triplet_pt2(q,j,p,i)
term(433) = term(433) + wm_interm_2_triplet_pt2(i,j) * wm_interm_94_triplet_pt2(q,j,i,p)
term(434) = term(434) + wm_interm_2_triplet_pt2(i,j) * wm_interm_94_triplet_pt2(q,j,p,i)
term(435) = term(435) + wm_interm_1_triplet_pt2(i,j) * wm_interm_95_triplet_pt2(q,j,p,i)
term(436) = term(436) + wm_interm_2_triplet_pt2(i,j) * wm_interm_95_triplet_pt2(q,j,p,i)
term(437) = term(437) + wm_interm_1_triplet_pt2(i,j) * wm_interm_94_triplet_pt2(j,q,p,i)
term(438) = term(438) + wm_interm_2_triplet_pt2(i,j) * wm_interm_94_triplet_pt2(j,q,p,i)
term(439) = term(439) + wm_interm_1_triplet_pt2(i,j) * wm_interm_94_triplet_pt2(j,q,i,p)
term(440) = term(440) + wm_interm_2_triplet_pt2(i,j) * wm_interm_94_triplet_pt2(j,q,i,p)
term(441) = term(441) + wm_interm_1_triplet_pt2(i,j) * wm_interm_95_triplet_pt2(j,q,i,p)
term(442) = term(442) + wm_interm_2_triplet_pt2(i,j) * wm_interm_95_triplet_pt2(j,q,i,p)
term(443) = term(443) + wm_interm_111_triplet_pt2(i,j) * wm_interm_16_triplet_pt2(p,j,i,q)
term(444) = term(444) + wm_interm_112_triplet_pt2(i,j) * wm_interm_16_triplet_pt2(p,j,i,q)
term(445) = term(445) + wm_interm_113_triplet_pt2(i,j) * wm_interm_16_triplet_pt2(p,j,i,q)
term(446) = term(446) + wm_interm_111_triplet_pt2(i,j) * wm_interm_16_triplet_pt2(j,p,i,q)
term(447) = term(447) + wm_interm_112_triplet_pt2(i,j) * wm_interm_16_triplet_pt2(j,p,i,q)
term(448) = term(448) + wm_interm_113_triplet_pt2(i,j) * wm_interm_16_triplet_pt2(j,p,i,q)
term(449) = term(449) + wm_interm_133_triplet_pt2(i,j) * wm_interm_16_triplet_pt2(p,j,i,q)
term(450) = term(450) + wm_interm_134_triplet_pt2(i,j) * wm_interm_16_triplet_pt2(p,j,i,q)
term(451) = term(451) + wm_interm_133_triplet_pt2(i,j) * wm_interm_16_triplet_pt2(j,p,i,q)
term(452) = term(452) + wm_interm_134_triplet_pt2(i,j) * wm_interm_16_triplet_pt2(j,p,i,q)
term(453) = term(453) + wm_interm_123_triplet_pt2(i,j) * wm_interm_16_triplet_pt2(p,j,i,q)
term(454) = term(454) + wm_interm_124_triplet_pt2(i,j) * wm_interm_16_triplet_pt2(p,j,i,q)
term(455) = term(455) + wm_interm_123_triplet_pt2(i,j) * wm_interm_16_triplet_pt2(j,p,i,q)
term(456) = term(456) + wm_interm_124_triplet_pt2(i,j) * wm_interm_16_triplet_pt2(j,p,i,q)
term(457) = term(457) + wm_interm_159_triplet_pt2(i,j) * wm_interm_16_triplet_pt2(p,j,i,q)
term(458) = term(458) + wm_interm_160_triplet_pt2(i,j) * wm_interm_16_triplet_pt2(p,j,i,q)
term(459) = term(459) + wm_interm_161_triplet_pt2(i,j) * wm_interm_16_triplet_pt2(p,j,i,q)
term(460) = term(460) + wm_interm_159_triplet_pt2(i,j) * wm_interm_16_triplet_pt2(j,p,i,q)
term(461) = term(461) + wm_interm_160_triplet_pt2(i,j) * wm_interm_16_triplet_pt2(j,p,i,q)
term(462) = term(462) + wm_interm_161_triplet_pt2(i,j) * wm_interm_16_triplet_pt2(j,p,i,q)
term(463) = term(463) + wm_interm_146_triplet_pt2(i,j) * wm_interm_16_triplet_pt2(p,j,i,q)
term(464) = term(464) + wm_interm_147_triplet_pt2(i,j) * wm_interm_16_triplet_pt2(p,j,i,q)
term(465) = term(465) + wm_interm_146_triplet_pt2(i,j) * wm_interm_16_triplet_pt2(j,p,i,q)
term(466) = term(466) + wm_interm_147_triplet_pt2(i,j) * wm_interm_16_triplet_pt2(j,p,i,q)
term(467) = term(467) + wm_interm_148_triplet_pt2(i,j) * wm_interm_16_triplet_pt2(p,j,i,q)
term(468) = term(468) + wm_interm_148_triplet_pt2(i,j) * wm_interm_16_triplet_pt2(j,p,i,q)
term(469) = term(469) + wm_interm_167_triplet_pt2(i,j) * wm_interm_16_triplet_pt2(p,j,i,q)
term(470) = term(470) + wm_interm_168_triplet_pt2(i,j) * wm_interm_16_triplet_pt2(p,j,i,q)
term(471) = term(471) + wm_interm_167_triplet_pt2(i,j) * wm_interm_16_triplet_pt2(j,p,i,q)
term(472) = term(472) + wm_interm_168_triplet_pt2(i,j) * wm_interm_16_triplet_pt2(j,p,i,q)
end do 
end do 

term(431) = term(431) * (0.5d+0) 
term(432) = term(432) * (-1.0d+0) 
term(433) = term(433) * (-1.0d+0) 
term(434) = term(434) * (2.0d+0) 
term(435) = term(435) * (0.5d+0) 
term(436) = term(436) * (-1.0d+0) 
term(437) = term(437) * (0.5d+0) 
term(438) = term(438) * (-1.0d+0) 
term(439) = term(439) * (-1.0d+0) 
term(440) = term(440) * (2.0d+0) 
term(441) = term(441) * (0.5d+0) 
term(442) = term(442) * (-1.0d+0) 
term(443) = term(443) * (0.5d+0) 
term(444) = term(444) * (-1.0d+0) 
term(445) = term(445) * (0.5d+0) 
term(446) = term(446) * (-1.0d+0) 
term(447) = term(447) * (2.0d+0) 
term(448) = term(448) * (-1.0d+0) 
term(449) = term(449) * (2.0d+0) 
term(450) = term(450) * (-2.0d+0) 
term(451) = term(451) * (-4.0d+0) 
term(452) = term(452) * (4.0d+0) 
term(453) = term(453) * (2.0d+0) 
term(454) = term(454) * (-2.0d+0) 
term(455) = term(455) * (-4.0d+0) 
term(456) = term(456) * (4.0d+0) 
term(458) = term(458) * (-2.0d+0) 
term(460) = term(460) * (-2.0d+0) 
term(461) = term(461) * (4.0d+0) 
term(462) = term(462) * (-2.0d+0) 
term(464) = term(464) * (-2.0d+0) 
term(465) = term(465) * (-2.0d+0) 
term(466) = term(466) * (4.0d+0) 
term(468) = term(468) * (-2.0d+0) 
term(469) = term(469) * (4.0d+0) 
term(470) = term(470) * (-4.0d+0) 
term(471) = term(471) * (-8.0d+0) 
term(472) = term(472) * (8.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(473) = term(473) + wm_interm_102_triplet_pt2(a,b,i,j) * wm_interm_25_triplet_pt2(b,a,j,i)
term(474) = term(474) + wm_interm_103_triplet_pt2(a,b,i,j) * wm_interm_25_triplet_pt2(b,a,j,i)
term(475) = term(475) + wm_interm_102_triplet_pt2(a,b,i,j) * wm_interm_28_triplet_pt2(b,a,j,i)
term(476) = term(476) + wm_interm_103_triplet_pt2(a,b,i,j) * wm_interm_28_triplet_pt2(b,a,j,i)
term(477) = term(477) + wm_interm_101_triplet_pt2(a,b,i,j) * wm_interm_25_triplet_pt2(b,a,j,i)
term(478) = term(478) + wm_interm_101_triplet_pt2(a,b,i,j) * wm_interm_28_triplet_pt2(b,a,j,i)
term(479) = term(479) + wm_interm_106_triplet_pt2(a,b,i,j) * wm_interm_25_triplet_pt2(b,a,j,i)
term(480) = term(480) + wm_interm_105_triplet_pt2(a,b,i,j) * wm_interm_25_triplet_pt2(b,a,j,i)
term(481) = term(481) + wm_interm_106_triplet_pt2(a,b,i,j) * wm_interm_28_triplet_pt2(b,a,j,i)
term(482) = term(482) + wm_interm_105_triplet_pt2(a,b,i,j) * wm_interm_28_triplet_pt2(b,a,j,i)
term(483) = term(483) + wm_interm_114_triplet_pt2(a,b,i,j) * wm_interm_25_triplet_pt2(b,a,j,i)
term(484) = term(484) + wm_interm_114_triplet_pt2(a,b,i,j) * wm_interm_28_triplet_pt2(b,a,j,i)
term(485) = term(485) + wm_interm_118_triplet_pt2(a,b,i,j) * wm_interm_25_triplet_pt2(b,a,j,i)
term(486) = term(486) + wm_interm_104_triplet_pt2(a,b,i,j) * wm_interm_25_triplet_pt2(b,a,j,i)
term(487) = term(487) + wm_interm_118_triplet_pt2(a,b,i,j) * wm_interm_28_triplet_pt2(b,a,j,i)
term(488) = term(488) + wm_interm_104_triplet_pt2(a,b,i,j) * wm_interm_28_triplet_pt2(b,a,j,i)
term(489) = term(489) + wm_interm_100_triplet_pt2(a,b,i,j) * wm_interm_25_triplet_pt2(b,a,j,i)
term(490) = term(490) + wm_interm_100_triplet_pt2(a,b,i,j) * wm_interm_28_triplet_pt2(b,a,j,i)
term(491) = term(491) + wm_interm_118_triplet_pt2(a,b,i,j) * wm_interm_27_triplet_pt2(b,a,j,i)
term(492) = term(492) + wm_interm_104_triplet_pt2(a,b,i,j) * wm_interm_27_triplet_pt2(b,a,j,i)
term(493) = term(493) + wm_interm_100_triplet_pt2(a,b,i,j) * wm_interm_27_triplet_pt2(b,a,j,i)
term(494) = term(494) + wm_interm_104_triplet_pt2(a,b,i,j) * wm_interm_26_triplet_pt2(b,a,j,i)
term(495) = term(495) + wm_interm_26_triplet_pt2(a,b,i,j) * wm_interm_99_triplet_pt2(b,a,j,i)
term(496) = term(496) + wm_interm_100_triplet_pt2(a,b,i,j) * wm_interm_26_triplet_pt2(b,a,j,i)
term(497) = term(497) + wm_interm_102_triplet_pt2(a,b,i,j) * wm_interm_26_triplet_pt2(b,a,j,i)
term(498) = term(498) + wm_interm_103_triplet_pt2(a,b,i,j) * wm_interm_26_triplet_pt2(b,a,j,i)
term(499) = term(499) + wm_interm_102_triplet_pt2(a,b,i,j) * wm_interm_27_triplet_pt2(b,a,j,i)
term(500) = term(500) + wm_interm_103_triplet_pt2(a,b,i,j) * wm_interm_27_triplet_pt2(b,a,j,i)
term(501) = term(501) + wm_interm_101_triplet_pt2(a,b,i,j) * wm_interm_26_triplet_pt2(b,a,j,i)
term(502) = term(502) + wm_interm_101_triplet_pt2(a,b,i,j) * wm_interm_27_triplet_pt2(b,a,j,i)
term(503) = term(503) + wm_interm_106_triplet_pt2(a,b,i,j) * wm_interm_27_triplet_pt2(b,a,j,i)
term(504) = term(504) + wm_interm_105_triplet_pt2(a,b,i,j) * wm_interm_27_triplet_pt2(b,a,j,i)
term(505) = term(505) + wm_interm_105_triplet_pt2(a,b,i,j) * wm_interm_26_triplet_pt2(b,a,j,i)
term(506) = term(506) + wm_interm_106_triplet_pt2(a,b,i,j) * wm_interm_26_triplet_pt2(b,a,j,i)
term(507) = term(507) + wm_interm_107_triplet_pt2(a,b,i,j) * wm_interm_26_triplet_pt2(b,a,j,i)
term(508) = term(508) + wm_interm_114_triplet_pt2(a,b,i,j) * wm_interm_27_triplet_pt2(b,a,j,i)
term(509) = term(509) + wm_interm_117_triplet_pt2(a,b,i,j) * wm_interm_26_triplet_pt2(b,a,j,i)
term(510) = term(510) + wm_interm_115_triplet_pt2(a,b,i,j) * wm_interm_26_triplet_pt2(b,a,j,i)
term(511) = term(511) + wm_interm_116_triplet_pt2(a,b,i,j) * wm_interm_26_triplet_pt2(b,a,j,i)
term(512) = term(512) + wm_interm_127_triplet_pt2(a,b,i,j) * wm_interm_25_triplet_pt2(b,a,j,i)
term(513) = term(513) + wm_interm_128_triplet_pt2(a,b,i,j) * wm_interm_25_triplet_pt2(b,a,j,i)
term(514) = term(514) + wm_interm_127_triplet_pt2(a,b,i,j) * wm_interm_28_triplet_pt2(b,a,j,i)
term(515) = term(515) + wm_interm_128_triplet_pt2(a,b,i,j) * wm_interm_28_triplet_pt2(b,a,j,i)
term(516) = term(516) + wm_interm_130_triplet_pt2(a,b,i,j) * wm_interm_25_triplet_pt2(b,a,j,i)
term(517) = term(517) + wm_interm_129_triplet_pt2(a,b,i,j) * wm_interm_25_triplet_pt2(b,a,j,i)
term(518) = term(518) + wm_interm_130_triplet_pt2(a,b,i,j) * wm_interm_28_triplet_pt2(b,a,j,i)
term(519) = term(519) + wm_interm_129_triplet_pt2(a,b,i,j) * wm_interm_28_triplet_pt2(b,a,j,i)
term(520) = term(520) + wm_interm_126_triplet_pt2(a,b,i,j) * wm_interm_25_triplet_pt2(b,a,j,i)
term(521) = term(521) + wm_interm_125_triplet_pt2(a,b,i,j) * wm_interm_25_triplet_pt2(b,a,j,i)
term(522) = term(522) + wm_interm_126_triplet_pt2(a,b,i,j) * wm_interm_28_triplet_pt2(b,a,j,i)
term(523) = term(523) + wm_interm_125_triplet_pt2(a,b,i,j) * wm_interm_28_triplet_pt2(b,a,j,i)
term(524) = term(524) + wm_interm_126_triplet_pt2(a,b,i,j) * wm_interm_27_triplet_pt2(b,a,j,i)
term(525) = term(525) + wm_interm_125_triplet_pt2(a,b,i,j) * wm_interm_27_triplet_pt2(b,a,j,i)
term(526) = term(526) + wm_interm_125_triplet_pt2(a,b,i,j) * wm_interm_26_triplet_pt2(b,a,j,i)
term(527) = term(527) + wm_interm_126_triplet_pt2(a,b,i,j) * wm_interm_26_triplet_pt2(b,a,j,i)
term(528) = term(528) + wm_interm_127_triplet_pt2(a,b,i,j) * wm_interm_26_triplet_pt2(b,a,j,i)
term(529) = term(529) + wm_interm_128_triplet_pt2(a,b,i,j) * wm_interm_26_triplet_pt2(b,a,j,i)
term(530) = term(530) + wm_interm_127_triplet_pt2(a,b,i,j) * wm_interm_27_triplet_pt2(b,a,j,i)
term(531) = term(531) + wm_interm_128_triplet_pt2(a,b,i,j) * wm_interm_27_triplet_pt2(b,a,j,i)
term(532) = term(532) + wm_interm_130_triplet_pt2(a,b,i,j) * wm_interm_27_triplet_pt2(b,a,j,i)
term(533) = term(533) + wm_interm_129_triplet_pt2(a,b,i,j) * wm_interm_27_triplet_pt2(b,a,j,i)
term(534) = term(534) + wm_interm_129_triplet_pt2(a,b,i,j) * wm_interm_26_triplet_pt2(b,a,j,i)
term(535) = term(535) + wm_interm_130_triplet_pt2(a,b,i,j) * wm_interm_26_triplet_pt2(b,a,j,i)
term(536) = term(536) + wm_interm_135_triplet_pt2(a,b,i,j) * wm_interm_26_triplet_pt2(b,a,j,i)
term(537) = term(537) + wm_interm_136_triplet_pt2(a,b,i,j) * wm_interm_26_triplet_pt2(b,a,j,i)
term(538) = term(538) + wm_interm_152_triplet_pt2(a,b,i,j) * wm_interm_25_triplet_pt2(b,a,j,i)
term(539) = term(539) + wm_interm_153_triplet_pt2(a,b,i,j) * wm_interm_25_triplet_pt2(b,a,j,i)
term(540) = term(540) + wm_interm_152_triplet_pt2(a,b,i,j) * wm_interm_28_triplet_pt2(b,a,j,i)
term(541) = term(541) + wm_interm_153_triplet_pt2(a,b,i,j) * wm_interm_28_triplet_pt2(b,a,j,i)
term(542) = term(542) + wm_interm_151_triplet_pt2(a,b,i,j) * wm_interm_25_triplet_pt2(b,a,j,i)
term(543) = term(543) + wm_interm_151_triplet_pt2(a,b,i,j) * wm_interm_28_triplet_pt2(b,a,j,i)
term(544) = term(544) + wm_interm_155_triplet_pt2(a,b,i,j) * wm_interm_25_triplet_pt2(b,a,j,i)
term(545) = term(545) + wm_interm_154_triplet_pt2(a,b,i,j) * wm_interm_25_triplet_pt2(b,a,j,i)
term(546) = term(546) + wm_interm_155_triplet_pt2(a,b,i,j) * wm_interm_28_triplet_pt2(b,a,j,i)
term(547) = term(547) + wm_interm_154_triplet_pt2(a,b,i,j) * wm_interm_28_triplet_pt2(b,a,j,i)
term(548) = term(548) + wm_interm_150_triplet_pt2(a,b,i,j) * wm_interm_25_triplet_pt2(b,a,j,i)
term(549) = term(549) + wm_interm_150_triplet_pt2(a,b,i,j) * wm_interm_28_triplet_pt2(b,a,j,i)
term(550) = term(550) + wm_interm_155_triplet_pt2(a,b,i,j) * wm_interm_27_triplet_pt2(b,a,j,i)
term(551) = term(551) + wm_interm_154_triplet_pt2(a,b,i,j) * wm_interm_27_triplet_pt2(b,a,j,i)
term(552) = term(552) + wm_interm_150_triplet_pt2(a,b,i,j) * wm_interm_27_triplet_pt2(b,a,j,i)
term(553) = term(553) + wm_interm_154_triplet_pt2(a,b,i,j) * wm_interm_26_triplet_pt2(b,a,j,i)
term(554) = term(554) + wm_interm_149_triplet_pt2(a,b,i,j) * wm_interm_26_triplet_pt2(b,a,j,i)
term(555) = term(555) + wm_interm_150_triplet_pt2(a,b,i,j) * wm_interm_26_triplet_pt2(b,a,j,i)
term(556) = term(556) + wm_interm_152_triplet_pt2(a,b,i,j) * wm_interm_26_triplet_pt2(b,a,j,i)
term(557) = term(557) + wm_interm_153_triplet_pt2(a,b,i,j) * wm_interm_26_triplet_pt2(b,a,j,i)
term(558) = term(558) + wm_interm_152_triplet_pt2(a,b,i,j) * wm_interm_27_triplet_pt2(b,a,j,i)
term(559) = term(559) + wm_interm_153_triplet_pt2(a,b,i,j) * wm_interm_27_triplet_pt2(b,a,j,i)
term(560) = term(560) + wm_interm_151_triplet_pt2(a,b,i,j) * wm_interm_26_triplet_pt2(b,a,j,i)
term(561) = term(561) + wm_interm_151_triplet_pt2(a,b,i,j) * wm_interm_27_triplet_pt2(b,a,j,i)
term(562) = term(562) + wm_interm_155_triplet_pt2(a,b,i,j) * wm_interm_26_triplet_pt2(b,a,j,i)
term(563) = term(563) + wm_interm_162_triplet_pt2(a,b,i,j) * wm_interm_26_triplet_pt2(b,a,j,i)
term(564) = term(564) + wm_interm_171_triplet_pt2(a,b,i,j) * wm_interm_25_triplet_pt2(b,a,j,i)
term(565) = term(565) + wm_interm_172_triplet_pt2(a,b,i,j) * wm_interm_25_triplet_pt2(b,a,j,i)
term(566) = term(566) + wm_interm_171_triplet_pt2(a,b,i,j) * wm_interm_28_triplet_pt2(b,a,j,i)
term(567) = term(567) + wm_interm_172_triplet_pt2(a,b,i,j) * wm_interm_28_triplet_pt2(b,a,j,i)
term(568) = term(568) + wm_interm_170_triplet_pt2(a,b,i,j) * wm_interm_25_triplet_pt2(b,a,j,i)
term(569) = term(569) + wm_interm_169_triplet_pt2(a,b,i,j) * wm_interm_25_triplet_pt2(b,a,j,i)
term(570) = term(570) + wm_interm_170_triplet_pt2(a,b,i,j) * wm_interm_28_triplet_pt2(b,a,j,i)
term(571) = term(571) + wm_interm_169_triplet_pt2(a,b,i,j) * wm_interm_28_triplet_pt2(b,a,j,i)
term(572) = term(572) + wm_interm_170_triplet_pt2(a,b,i,j) * wm_interm_27_triplet_pt2(b,a,j,i)
term(573) = term(573) + wm_interm_169_triplet_pt2(a,b,i,j) * wm_interm_27_triplet_pt2(b,a,j,i)
term(574) = term(574) + wm_interm_169_triplet_pt2(a,b,i,j) * wm_interm_26_triplet_pt2(b,a,j,i)
term(575) = term(575) + wm_interm_170_triplet_pt2(a,b,i,j) * wm_interm_26_triplet_pt2(b,a,j,i)
term(576) = term(576) + wm_interm_171_triplet_pt2(a,b,i,j) * wm_interm_26_triplet_pt2(b,a,j,i)
term(577) = term(577) + wm_interm_172_triplet_pt2(a,b,i,j) * wm_interm_26_triplet_pt2(b,a,j,i)
term(578) = term(578) + wm_interm_171_triplet_pt2(a,b,i,j) * wm_interm_27_triplet_pt2(b,a,j,i)
term(579) = term(579) + wm_interm_172_triplet_pt2(a,b,i,j) * wm_interm_27_triplet_pt2(b,a,j,i)
end do 
end do 
end do 
end do 

term(473) = term(473) * (2.0d+0) 
term(474) = term(474) * (-4.0d+0) 
term(475) = term(475) * (-4.0d+0) 
term(476) = term(476) * (8.0d+0) 
term(477) = term(477) * (2.0d+0) 
term(478) = term(478) * (-4.0d+0) 
term(479) = term(479) * (-1.0d+0) 
term(480) = term(480) * (2.0d+0) 
term(481) = term(481) * (2.0d+0) 
term(482) = term(482) * (-4.0d+0) 
term(483) = term(483) * (-1.0d+0) 
term(484) = term(484) * (2.0d+0) 
term(485) = term(485) * (-1.0d+0) 
term(486) = term(486) * (2.0d+0) 
term(487) = term(487) * (2.0d+0) 
term(488) = term(488) * (-4.0d+0) 
term(489) = term(489) * (-1.0d+0) 
term(490) = term(490) * (2.0d+0) 
term(491) = term(491) * (-1.0d+0) 
term(492) = term(492) * (2.0d+0) 
term(493) = term(493) * (-1.0d+0) 
term(494) = term(494) * (-1.0d+0) 
term(495) = term(495) * (-1.0d+0) 
term(496) = term(496) * (2.0d+0) 
term(497) = term(497) * (-1.0d+0) 
term(498) = term(498) * (2.0d+0) 
term(499) = term(499) * (2.0d+0) 
term(500) = term(500) * (-4.0d+0) 
term(501) = term(501) * (-1.0d+0) 
term(502) = term(502) * (2.0d+0) 
term(503) = term(503) * (-1.0d+0) 
term(504) = term(504) * (2.0d+0) 
term(505) = term(505) * (-1.0d+0) 
term(506) = term(506) * (2.0d+0) 
term(507) = term(507) * (-1.0d+0) 
term(508) = term(508) * (-1.0d+0) 
term(509) = term(509) * (-1.0d+0) 
term(510) = term(510) * (-1.0d+0) 
term(511) = term(511) * (2.0d+0) 
term(512) = term(512) * (8.0d+0) 
term(513) = term(513) * (-8.0d+0) 
term(514) = term(514) * (-16.0d+0) 
term(515) = term(515) * (16.0d+0) 
term(516) = term(516) * (-4.0d+0) 
term(517) = term(517) * (4.0d+0) 
term(518) = term(518) * (8.0d+0) 
term(519) = term(519) * (-8.0d+0) 
term(520) = term(520) * (-4.0d+0) 
term(521) = term(521) * (4.0d+0) 
term(522) = term(522) * (8.0d+0) 
term(523) = term(523) * (-8.0d+0) 
term(524) = term(524) * (-4.0d+0) 
term(525) = term(525) * (4.0d+0) 
term(526) = term(526) * (-4.0d+0) 
term(527) = term(527) * (4.0d+0) 
term(528) = term(528) * (-4.0d+0) 
term(529) = term(529) * (4.0d+0) 
term(530) = term(530) * (8.0d+0) 
term(531) = term(531) * (-8.0d+0) 
term(532) = term(532) * (-4.0d+0) 
term(533) = term(533) * (4.0d+0) 
term(534) = term(534) * (-4.0d+0) 
term(535) = term(535) * (4.0d+0) 
term(536) = term(536) * (-4.0d+0) 
term(537) = term(537) * (4.0d+0) 
term(538) = term(538) * (4.0d+0) 
term(539) = term(539) * (-8.0d+0) 
term(540) = term(540) * (-8.0d+0) 
term(541) = term(541) * (16.0d+0) 
term(542) = term(542) * (4.0d+0) 
term(543) = term(543) * (-8.0d+0) 
term(544) = term(544) * (-4.0d+0) 
term(545) = term(545) * (8.0d+0) 
term(546) = term(546) * (8.0d+0) 
term(547) = term(547) * (-16.0d+0) 
term(548) = term(548) * (-4.0d+0) 
term(549) = term(549) * (8.0d+0) 
term(550) = term(550) * (-4.0d+0) 
term(551) = term(551) * (8.0d+0) 
term(552) = term(552) * (-4.0d+0) 
term(553) = term(553) * (-4.0d+0) 
term(554) = term(554) * (-4.0d+0) 
term(555) = term(555) * (4.0d+0) 
term(556) = term(556) * (-4.0d+0) 
term(557) = term(557) * (4.0d+0) 
term(558) = term(558) * (4.0d+0) 
term(559) = term(559) * (-8.0d+0) 
term(560) = term(560) * (-4.0d+0) 
term(561) = term(561) * (4.0d+0) 
term(562) = term(562) * (4.0d+0) 
term(563) = term(563) * (4.0d+0) 
term(564) = term(564) * (16.0d+0) 
term(565) = term(565) * (-16.0d+0) 
term(566) = term(566) * (-32.0d+0) 
term(567) = term(567) * (32.0d+0) 
term(568) = term(568) * (-16.0d+0) 
term(569) = term(569) * (16.0d+0) 
term(570) = term(570) * (32.0d+0) 
term(571) = term(571) * (-32.0d+0) 
term(572) = term(572) * (-16.0d+0) 
term(573) = term(573) * (16.0d+0) 
term(574) = term(574) * (-16.0d+0) 
term(575) = term(575) * (16.0d+0) 
term(576) = term(576) * (-16.0d+0) 
term(577) = term(577) * (16.0d+0) 
term(578) = term(578) * (16.0d+0) 
term(579) = term(579) * (-16.0d+0) 

do a = nocc + 1, nactive 
term(580) = term(580) + r1(vrdav_Rl, a,q) * wm_interm_78_triplet_pt2(a,p)
term(581) = term(581) + r1(vrdav_Rl, a,q) * wm_interm_79_triplet_pt2(a,p)
term(582) = term(582) + r1(vrdav_Rl, a,q) * wm_interm_80_triplet_pt2(a,p)
term(583) = term(583) + r1(vrdav_Rl, a,q) * wm_interm_81_triplet_pt2(a,p)
term(584) = term(584) + r1(vrdav_Rl, a,q) * wm_interm_82_triplet_pt2(a,p)
term(585) = term(585) + s1(a,q) * wm_interm_13_triplet_pt2(a,p)
term(586) = term(586) + s1(a,q) * wm_interm_21_triplet_pt2(a,p)
term(587) = term(587) + s1(a,q) * wm_interm_22_triplet_pt2(a,p)
term(588) = term(588) + s1(a,q) * wm_interm_33_triplet_pt2(a,p)
term(589) = term(589) + s1(a,q) * wm_interm_38_triplet_pt2(a,p)
term(590) = term(590) + r1(vrdav_Rr, a,p) * wm_interm_83_triplet_pt2(a,q)
term(591) = term(591) + r1(vrdav_Rr, a,p) * wm_interm_84_triplet_pt2(a,q)
term(592) = term(592) + r1(vrdav_Rr, a,p) * wm_interm_85_triplet_pt2(a,q)
term(593) = term(593) + t1(a,q) * wm_interm_44_triplet_pt2(a,p)
term(594) = term(594) + t1(a,q) * wm_interm_49_triplet_pt2(a,p)
term(595) = term(595) + t1(a,q) * wm_interm_46_triplet_pt2(a,p)
term(596) = term(596) + r1(vrdav_Rr, a,p) * wm_interm_86_triplet_pt2(a,q)
term(597) = term(597) + r1(vrdav_Rr, a,p) * wm_interm_87_triplet_pt2(a,q)
term(598) = term(598) + t1(a,q) * wm_interm_65_triplet_pt2(a,p)
term(599) = term(599) + t1(a,q) * wm_interm_67_triplet_pt2(a,p)
term(600) = term(600) + wm_interm_54_triplet_pt2(a,p) * wm_interm_6_triplet_pt2(a,q)
end do 

term(580) = term(580) * (-2.0d+0) 
term(583) = term(583) * (-4.0d+0) 
term(584) = term(584) * (4.0d+0) 
term(585) = term(585) * (-1.0d+0) 
term(586) = term(586) * (-1.0d+0) 
term(587) = term(587) * (2.0d+0) 
term(588) = term(588) * (-4.0d+0) 
term(589) = term(589) * (4.0d+0) 
term(590) = term(590) * (2.0d+0) 
term(591) = term(591) * (-1.0d+0) 
term(592) = term(592) * (-1.0d+0) 
term(593) = term(593) * (-1.0d+0) 
term(594) = term(594) * (-1.0d+0) 
term(595) = term(595) * (2.0d+0) 
term(596) = term(596) * (4.0d+0) 
term(597) = term(597) * (-4.0d+0) 
term(598) = term(598) * (-4.0d+0) 
term(599) = term(599) * (4.0d+0) 
term(600) = term(600) * (-2.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(601) = term(601) + s1(a,i) * wm_interm_22_triplet_pt2(a,i)
term(602) = term(602) + s1(a,i) * wm_interm_21_triplet_pt2(a,i)
term(603) = term(603) + s1(a,i) * wm_interm_13_triplet_pt2(a,i)
term(604) = term(604) + s1(a,i) * wm_interm_38_triplet_pt2(a,i)
term(605) = term(605) + s1(a,i) * wm_interm_33_triplet_pt2(a,i)
term(606) = term(606) + t1(a,i) * wm_interm_46_triplet_pt2(a,i)
term(607) = term(607) + t1(a,i) * wm_interm_49_triplet_pt2(a,i)
term(608) = term(608) + t1(a,i) * wm_interm_44_triplet_pt2(a,i)
term(609) = term(609) + t1(a,i) * wm_interm_67_triplet_pt2(a,i)
term(610) = term(610) + t1(a,i) * wm_interm_65_triplet_pt2(a,i)
term(611) = term(611) + wm_interm_48_triplet_pt2(a,p,i,q) * wm_interm_6_triplet_pt2(a,i)
term(612) = term(612) + wm_interm_54_triplet_pt2(a,i) * wm_interm_6_triplet_pt2(a,i)
term(613) = term(613) + wm_interm_14_triplet_pt2(a,p,i,q) * wm_interm_54_triplet_pt2(a,i)
end do 
end do 

term(601) = term(601) * (4.0d+0) 
term(602) = term(602) * (-2.0d+0) 
term(603) = term(603) * (-2.0d+0) 
term(604) = term(604) * (8.0d+0) 
term(605) = term(605) * (-8.0d+0) 
term(606) = term(606) * (-4.0d+0) 
term(607) = term(607) * (2.0d+0) 
term(608) = term(608) * (2.0d+0) 
term(609) = term(609) * (-8.0d+0) 
term(610) = term(610) * (8.0d+0) 
term(611) = term(611) * (-2.0d+0) 
term(612) = term(612) * (4.0d+0) 
term(613) = term(613) * (-2.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(614) = term(614) + wm_interm_16_triplet_pt2(i,q,j,k) * wm_interm_94_triplet_pt2(j,k,p,i)
term(615) = term(615) + wm_interm_16_triplet_pt2(q,i,j,k) * wm_interm_94_triplet_pt2(j,k,p,i)
term(616) = term(616) + wm_interm_16_triplet_pt2(q,i,j,k) * wm_interm_95_triplet_pt2(j,k,p,i)
end do 
end do 
end do 

term(614) = term(614) * (0.5d+0) 
term(615) = term(615) * (-1.0d+0) 
term(616) = term(616) * (0.5d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(617) = term(617) + wm_interm_16_triplet_pt2(i,q,j,k) * wm_interm_94_triplet_pt2(j,k,i,p)
term(618) = term(618) + wm_interm_16_triplet_pt2(q,i,j,k) * wm_interm_94_triplet_pt2(j,k,i,p)
term(619) = term(619) + wm_interm_16_triplet_pt2(i,q,j,k) * wm_interm_95_triplet_pt2(j,k,i,p)
term(620) = term(620) + wm_interm_29_triplet_pt2(i,p,j,k) * wm_interm_58_triplet_pt2(i,q,k,j)
term(621) = term(621) + wm_interm_29_triplet_pt2(p,i,j,k) * wm_interm_58_triplet_pt2(i,q,j,k)
term(622) = term(622) + wm_interm_29_triplet_pt2(i,p,j,k) * wm_interm_58_triplet_pt2(i,q,j,k)
term(623) = term(623) + wm_interm_29_triplet_pt2(p,i,j,k) * wm_interm_58_triplet_pt2(q,i,k,j)
term(624) = term(624) + wm_interm_29_triplet_pt2(i,p,j,k) * wm_interm_58_triplet_pt2(q,i,j,k)
term(625) = term(625) + wm_interm_29_triplet_pt2(p,i,j,k) * wm_interm_58_triplet_pt2(q,i,j,k)
term(626) = term(626) + wm_interm_122_triplet_pt2(i,j,p,k) * wm_interm_16_triplet_pt2(k,q,i,j)
term(627) = term(627) + wm_interm_122_triplet_pt2(i,j,p,k) * wm_interm_16_triplet_pt2(q,k,i,j)
term(628) = term(628) + wm_interm_40_triplet_pt2(i,p,j,k) * wm_interm_58_triplet_pt2(i,q,k,j)
term(629) = term(629) + wm_interm_40_triplet_pt2(p,i,j,k) * wm_interm_58_triplet_pt2(i,q,j,k)
term(630) = term(630) + wm_interm_40_triplet_pt2(i,p,j,k) * wm_interm_58_triplet_pt2(i,q,j,k)
term(631) = term(631) + wm_interm_40_triplet_pt2(p,i,j,k) * wm_interm_58_triplet_pt2(q,i,k,j)
term(632) = term(632) + wm_interm_40_triplet_pt2(i,p,j,k) * wm_interm_58_triplet_pt2(q,i,j,k)
term(633) = term(633) + wm_interm_40_triplet_pt2(p,i,j,k) * wm_interm_58_triplet_pt2(q,i,j,k)
term(634) = term(634) + wm_interm_29_triplet_pt2(i,j,k,p) * wm_interm_58_triplet_pt2(j,i,k,q)
term(635) = term(635) + wm_interm_29_triplet_pt2(i,j,k,p) * wm_interm_58_triplet_pt2(i,j,k,q)
term(636) = term(636) + wm_interm_29_triplet_pt2(i,j,k,p) * wm_interm_58_triplet_pt2(i,j,q,k)
term(637) = term(637) + wm_interm_29_triplet_pt2(i,j,p,k) * wm_interm_58_triplet_pt2(j,i,q,k)
term(638) = term(638) + wm_interm_29_triplet_pt2(i,j,p,k) * wm_interm_58_triplet_pt2(i,j,q,k)
term(639) = term(639) + wm_interm_29_triplet_pt2(i,j,p,k) * wm_interm_58_triplet_pt2(i,j,k,q)
term(640) = term(640) + wm_interm_122_triplet_pt2(i,p,j,k) * wm_interm_16_triplet_pt2(k,j,i,q)
term(641) = term(641) + wm_interm_122_triplet_pt2(i,p,j,k) * wm_interm_16_triplet_pt2(j,k,i,q)
term(642) = term(642) + wm_interm_122_triplet_pt2(p,i,j,k) * wm_interm_16_triplet_pt2(j,k,i,q)
term(643) = term(643) + wm_interm_122_triplet_pt2(p,i,j,k) * wm_interm_16_triplet_pt2(k,j,i,q)
term(644) = term(644) + wm_interm_40_triplet_pt2(i,j,k,p) * wm_interm_58_triplet_pt2(j,i,k,q)
term(645) = term(645) + wm_interm_40_triplet_pt2(i,j,k,p) * wm_interm_58_triplet_pt2(i,j,k,q)
term(646) = term(646) + wm_interm_40_triplet_pt2(i,j,k,p) * wm_interm_58_triplet_pt2(i,j,q,k)
term(647) = term(647) + wm_interm_40_triplet_pt2(i,j,p,k) * wm_interm_58_triplet_pt2(j,i,q,k)
term(648) = term(648) + wm_interm_40_triplet_pt2(i,j,p,k) * wm_interm_58_triplet_pt2(i,j,q,k)
term(649) = term(649) + wm_interm_40_triplet_pt2(i,j,p,k) * wm_interm_58_triplet_pt2(i,j,k,q)
term(650) = term(650) + wm_interm_144_triplet_pt2(i,j,p,k) * wm_interm_16_triplet_pt2(k,q,i,j)
term(651) = term(651) + wm_interm_144_triplet_pt2(i,j,p,k) * wm_interm_16_triplet_pt2(q,k,i,j)
term(652) = term(652) + wm_interm_145_triplet_pt2(i,j,p,k) * wm_interm_16_triplet_pt2(q,k,i,j)
term(653) = term(653) + wm_interm_29_triplet_pt2(i,p,j,k) * wm_interm_75_triplet_pt2(i,q,k,j)
term(654) = term(654) + wm_interm_29_triplet_pt2(p,i,j,k) * wm_interm_75_triplet_pt2(i,q,j,k)
term(655) = term(655) + wm_interm_29_triplet_pt2(i,p,j,k) * wm_interm_75_triplet_pt2(i,q,j,k)
term(656) = term(656) + wm_interm_29_triplet_pt2(p,i,j,k) * wm_interm_75_triplet_pt2(q,i,k,j)
term(657) = term(657) + wm_interm_29_triplet_pt2(i,p,j,k) * wm_interm_75_triplet_pt2(q,i,j,k)
term(658) = term(658) + wm_interm_29_triplet_pt2(p,i,j,k) * wm_interm_75_triplet_pt2(q,i,j,k)
term(659) = term(659) + wm_interm_166_triplet_pt2(i,j,p,k) * wm_interm_16_triplet_pt2(k,q,i,j)
term(660) = term(660) + wm_interm_166_triplet_pt2(i,j,p,k) * wm_interm_16_triplet_pt2(q,k,i,j)
term(661) = term(661) + wm_interm_40_triplet_pt2(i,p,j,k) * wm_interm_75_triplet_pt2(i,q,k,j)
term(662) = term(662) + wm_interm_40_triplet_pt2(p,i,j,k) * wm_interm_75_triplet_pt2(i,q,j,k)
term(663) = term(663) + wm_interm_40_triplet_pt2(i,p,j,k) * wm_interm_75_triplet_pt2(i,q,j,k)
term(664) = term(664) + wm_interm_40_triplet_pt2(p,i,j,k) * wm_interm_75_triplet_pt2(q,i,k,j)
term(665) = term(665) + wm_interm_40_triplet_pt2(i,p,j,k) * wm_interm_75_triplet_pt2(q,i,j,k)
term(666) = term(666) + wm_interm_40_triplet_pt2(p,i,j,k) * wm_interm_75_triplet_pt2(q,i,j,k)
term(667) = term(667) + wm_interm_144_triplet_pt2(i,p,j,k) * wm_interm_16_triplet_pt2(k,j,i,q)
term(668) = term(668) + wm_interm_144_triplet_pt2(i,p,j,k) * wm_interm_16_triplet_pt2(j,k,i,q)
term(669) = term(669) + wm_interm_145_triplet_pt2(i,p,j,k) * wm_interm_16_triplet_pt2(j,k,i,q)
term(670) = term(670) + wm_interm_144_triplet_pt2(p,i,j,k) * wm_interm_16_triplet_pt2(j,k,i,q)
term(671) = term(671) + wm_interm_144_triplet_pt2(p,i,j,k) * wm_interm_16_triplet_pt2(k,j,i,q)
term(672) = term(672) + wm_interm_145_triplet_pt2(p,i,j,k) * wm_interm_16_triplet_pt2(k,j,i,q)
term(673) = term(673) + wm_interm_29_triplet_pt2(i,j,k,p) * wm_interm_75_triplet_pt2(j,i,k,q)
term(674) = term(674) + wm_interm_29_triplet_pt2(i,j,k,p) * wm_interm_75_triplet_pt2(i,j,k,q)
term(675) = term(675) + wm_interm_29_triplet_pt2(i,j,k,p) * wm_interm_75_triplet_pt2(i,j,q,k)
term(676) = term(676) + wm_interm_29_triplet_pt2(i,j,p,k) * wm_interm_75_triplet_pt2(j,i,q,k)
term(677) = term(677) + wm_interm_29_triplet_pt2(i,j,p,k) * wm_interm_75_triplet_pt2(i,j,q,k)
term(678) = term(678) + wm_interm_29_triplet_pt2(i,j,p,k) * wm_interm_75_triplet_pt2(i,j,k,q)
term(679) = term(679) + wm_interm_166_triplet_pt2(i,p,j,k) * wm_interm_16_triplet_pt2(k,j,i,q)
term(680) = term(680) + wm_interm_166_triplet_pt2(i,p,j,k) * wm_interm_16_triplet_pt2(j,k,i,q)
term(681) = term(681) + wm_interm_166_triplet_pt2(p,i,j,k) * wm_interm_16_triplet_pt2(j,k,i,q)
term(682) = term(682) + wm_interm_166_triplet_pt2(p,i,j,k) * wm_interm_16_triplet_pt2(k,j,i,q)
term(683) = term(683) + wm_interm_40_triplet_pt2(i,j,k,p) * wm_interm_75_triplet_pt2(j,i,k,q)
term(684) = term(684) + wm_interm_40_triplet_pt2(i,j,k,p) * wm_interm_75_triplet_pt2(i,j,k,q)
term(685) = term(685) + wm_interm_40_triplet_pt2(i,j,k,p) * wm_interm_75_triplet_pt2(i,j,q,k)
term(686) = term(686) + wm_interm_40_triplet_pt2(i,j,p,k) * wm_interm_75_triplet_pt2(j,i,q,k)
term(687) = term(687) + wm_interm_40_triplet_pt2(i,j,p,k) * wm_interm_75_triplet_pt2(i,j,q,k)
term(688) = term(688) + wm_interm_40_triplet_pt2(i,j,p,k) * wm_interm_75_triplet_pt2(i,j,k,q)
end do 
end do 
end do 

term(617) = term(617) * (-1.0d+0) 
term(618) = term(618) * (0.5d+0) 
term(619) = term(619) * (0.5d+0) 
term(620) = term(620) * (0.5d+0) 
term(621) = term(621) * (0.5d+0) 
term(622) = term(622) * (-1.0d+0) 
term(623) = term(623) * (0.5d+0) 
term(624) = term(624) * (0.5d+0) 
term(625) = term(625) * (-1.0d+0) 
term(626) = term(626) * (2.0d+0) 
term(627) = term(627) * (-2.0d+0) 
term(630) = term(630) * (-2.0d+0) 
term(633) = term(633) * (-2.0d+0) 
term(634) = term(634) * (0.5d+0) 
term(635) = term(635) * (-1.0d+0) 
term(636) = term(636) * (0.5d+0) 
term(637) = term(637) * (0.5d+0) 
term(638) = term(638) * (-1.0d+0) 
term(639) = term(639) * (0.5d+0) 
term(640) = term(640) * (2.0d+0) 
term(641) = term(641) * (-2.0d+0) 
term(642) = term(642) * (2.0d+0) 
term(643) = term(643) * (-2.0d+0) 
term(645) = term(645) * (-2.0d+0) 
term(648) = term(648) * (-2.0d+0) 
term(651) = term(651) * (-2.0d+0) 
term(655) = term(655) * (-2.0d+0) 
term(658) = term(658) * (-2.0d+0) 
term(659) = term(659) * (4.0d+0) 
term(660) = term(660) * (-4.0d+0) 
term(661) = term(661) * (2.0d+0) 
term(662) = term(662) * (2.0d+0) 
term(663) = term(663) * (-4.0d+0) 
term(664) = term(664) * (2.0d+0) 
term(665) = term(665) * (2.0d+0) 
term(666) = term(666) * (-4.0d+0) 
term(668) = term(668) * (-2.0d+0) 
term(671) = term(671) * (-2.0d+0) 
term(674) = term(674) * (-2.0d+0) 
term(677) = term(677) * (-2.0d+0) 
term(679) = term(679) * (4.0d+0) 
term(680) = term(680) * (-4.0d+0) 
term(681) = term(681) * (4.0d+0) 
term(682) = term(682) * (-4.0d+0) 
term(683) = term(683) * (2.0d+0) 
term(684) = term(684) * (-4.0d+0) 
term(685) = term(685) * (2.0d+0) 
term(686) = term(686) * (2.0d+0) 
term(687) = term(687) * (-4.0d+0) 
term(688) = term(688) * (2.0d+0) 

term(689) = term(689) + wm_interm_1_triplet_pt2(p,q) * wm_interm_89_triplet_pt2
term(690) = term(690) + wm_interm_2_triplet_pt2(p,q) * wm_interm_89_triplet_pt2
term(691) = term(691) + wm_interm_139_triplet_pt2 * wm_interm_1_triplet_pt2(p,q)
term(692) = term(692) + wm_interm_140_triplet_pt2 * wm_interm_1_triplet_pt2(p,q)
term(693) = term(693) + wm_interm_141_triplet_pt2 * wm_interm_1_triplet_pt2(p,q)
term(694) = term(694) + wm_interm_139_triplet_pt2 * wm_interm_2_triplet_pt2(p,q)
term(695) = term(695) + wm_interm_140_triplet_pt2 * wm_interm_2_triplet_pt2(p,q)
term(696) = term(696) + wm_interm_141_triplet_pt2 * wm_interm_2_triplet_pt2(p,q)
term(697) = term(697) + wm_interm_142_triplet_pt2 * wm_interm_1_triplet_pt2(p,q)
term(698) = term(698) + wm_interm_143_triplet_pt2 * wm_interm_1_triplet_pt2(p,q)
term(699) = term(699) + wm_interm_142_triplet_pt2 * wm_interm_2_triplet_pt2(p,q)
term(700) = term(700) + wm_interm_143_triplet_pt2 * wm_interm_2_triplet_pt2(p,q)
term(701) = term(701) + wm_interm_179_triplet_pt2 * wm_interm_1_triplet_pt2(p,q)
term(702) = term(702) + wm_interm_180_triplet_pt2 * wm_interm_1_triplet_pt2(p,q)
term(703) = term(703) + wm_interm_181_triplet_pt2 * wm_interm_1_triplet_pt2(p,q)
term(704) = term(704) + wm_interm_179_triplet_pt2 * wm_interm_2_triplet_pt2(p,q)
term(705) = term(705) + wm_interm_180_triplet_pt2 * wm_interm_2_triplet_pt2(p,q)
term(706) = term(706) + wm_interm_181_triplet_pt2 * wm_interm_2_triplet_pt2(p,q)
term(707) = term(707) + wm_interm_182_triplet_pt2 * wm_interm_1_triplet_pt2(p,q)
term(708) = term(708) + wm_interm_183_triplet_pt2 * wm_interm_1_triplet_pt2(p,q)
term(709) = term(709) + wm_interm_182_triplet_pt2 * wm_interm_2_triplet_pt2(p,q)
term(710) = term(710) + wm_interm_183_triplet_pt2 * wm_interm_2_triplet_pt2(p,q)

term(689) = term(689) * (4.0d+0) 
term(690) = term(690) * (-8.0d+0) 
term(691) = term(691) * (-1.0d+0) 
term(692) = term(692) * (2.0d+0) 
term(693) = term(693) * (-1.0d+0) 
term(694) = term(694) * (2.0d+0) 
term(695) = term(695) * (-4.0d+0) 
term(696) = term(696) * (2.0d+0) 
term(697) = term(697) * (-4.0d+0) 
term(698) = term(698) * (4.0d+0) 
term(699) = term(699) * (8.0d+0) 
term(700) = term(700) * (-8.0d+0) 
term(701) = term(701) * (-2.0d+0) 
term(702) = term(702) * (4.0d+0) 
term(703) = term(703) * (-2.0d+0) 
term(704) = term(704) * (4.0d+0) 
term(705) = term(705) * (-8.0d+0) 
term(706) = term(706) * (4.0d+0) 
term(707) = term(707) * (-8.0d+0) 
term(708) = term(708) * (8.0d+0) 
term(709) = term(709) * (16.0d+0) 
term(710) = term(710) * (-16.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
term(711) = term(711) + wm_interm_1_triplet_pt2(i,j) * wm_interm_88_triplet_pt2(j,i)
term(712) = term(712) + wm_interm_2_triplet_pt2(i,j) * wm_interm_88_triplet_pt2(j,i)
term(713) = term(713) + wm_interm_16_triplet_pt2(p,i,j,q) * wm_interm_88_triplet_pt2(j,i)
term(714) = term(714) + wm_interm_16_triplet_pt2(i,p,j,q) * wm_interm_88_triplet_pt2(j,i)
term(715) = term(715) + wm_interm_1_triplet_pt2(i,j) * wm_interm_96_triplet_pt2(j,i)
term(716) = term(716) + wm_interm_1_triplet_pt2(i,j) * wm_interm_97_triplet_pt2(j,i)
term(717) = term(717) + wm_interm_2_triplet_pt2(i,j) * wm_interm_96_triplet_pt2(j,i)
term(718) = term(718) + wm_interm_2_triplet_pt2(i,j) * wm_interm_97_triplet_pt2(j,i)
term(719) = term(719) + wm_interm_1_triplet_pt2(i,j) * wm_interm_98_triplet_pt2(j,i)
term(720) = term(720) + wm_interm_2_triplet_pt2(i,j) * wm_interm_98_triplet_pt2(j,i)
term(721) = term(721) + wm_interm_111_triplet_pt2(i,j) * wm_interm_1_triplet_pt2(j,i)
term(722) = term(722) + wm_interm_111_triplet_pt2(i,j) * wm_interm_2_triplet_pt2(j,i)
term(723) = term(723) + wm_interm_112_triplet_pt2(i,j) * wm_interm_1_triplet_pt2(j,i)
term(724) = term(724) + wm_interm_112_triplet_pt2(i,j) * wm_interm_2_triplet_pt2(j,i)
term(725) = term(725) + wm_interm_113_triplet_pt2(i,j) * wm_interm_1_triplet_pt2(j,i)
term(726) = term(726) + wm_interm_113_triplet_pt2(i,j) * wm_interm_2_triplet_pt2(j,i)
term(727) = term(727) + wm_interm_29_triplet_pt2(p,i,j,q) * wm_interm_57_triplet_pt2(i,j)
term(728) = term(728) + wm_interm_29_triplet_pt2(i,p,j,q) * wm_interm_57_triplet_pt2(i,j)
term(729) = term(729) + wm_interm_30_triplet_pt2(i,j) * wm_interm_57_triplet_pt2(i,j)
term(730) = term(730) + wm_interm_31_triplet_pt2(i,j) * wm_interm_57_triplet_pt2(i,j)
term(731) = term(731) + wm_interm_29_triplet_pt2(i,p,q,j) * wm_interm_57_triplet_pt2(i,j)
term(732) = term(732) + wm_interm_32_triplet_pt2(i,j) * wm_interm_57_triplet_pt2(i,j)
term(733) = term(733) + wm_interm_29_triplet_pt2(p,i,j,q) * wm_interm_61_triplet_pt2(i,j)
term(734) = term(734) + wm_interm_29_triplet_pt2(i,p,j,q) * wm_interm_61_triplet_pt2(i,j)
term(735) = term(735) + wm_interm_30_triplet_pt2(i,j) * wm_interm_61_triplet_pt2(i,j)
term(736) = term(736) + wm_interm_31_triplet_pt2(i,j) * wm_interm_61_triplet_pt2(i,j)
term(737) = term(737) + wm_interm_29_triplet_pt2(i,p,q,j) * wm_interm_61_triplet_pt2(i,j)
term(738) = term(738) + wm_interm_32_triplet_pt2(i,j) * wm_interm_61_triplet_pt2(i,j)
term(739) = term(739) + wm_interm_29_triplet_pt2(p,i,j,q) * wm_interm_64_triplet_pt2(i,j)
term(740) = term(740) + wm_interm_29_triplet_pt2(i,p,j,q) * wm_interm_64_triplet_pt2(i,j)
term(741) = term(741) + wm_interm_30_triplet_pt2(i,j) * wm_interm_64_triplet_pt2(i,j)
term(742) = term(742) + wm_interm_31_triplet_pt2(i,j) * wm_interm_64_triplet_pt2(i,j)
term(743) = term(743) + wm_interm_29_triplet_pt2(i,p,q,j) * wm_interm_64_triplet_pt2(i,j)
term(744) = term(744) + wm_interm_32_triplet_pt2(i,j) * wm_interm_64_triplet_pt2(i,j)
term(745) = term(745) + wm_interm_122_triplet_pt2(q,i,j,p) * wm_interm_1_triplet_pt2(j,i)
term(746) = term(746) + wm_interm_122_triplet_pt2(q,i,p,j) * wm_interm_1_triplet_pt2(j,i)
term(747) = term(747) + wm_interm_122_triplet_pt2(q,i,j,p) * wm_interm_2_triplet_pt2(j,i)
term(748) = term(748) + wm_interm_122_triplet_pt2(q,i,p,j) * wm_interm_2_triplet_pt2(j,i)
term(749) = term(749) + wm_interm_123_triplet_pt2(i,j) * wm_interm_1_triplet_pt2(j,i)
term(750) = term(750) + wm_interm_124_triplet_pt2(i,j) * wm_interm_1_triplet_pt2(j,i)
term(751) = term(751) + wm_interm_123_triplet_pt2(i,j) * wm_interm_2_triplet_pt2(j,i)
term(752) = term(752) + wm_interm_124_triplet_pt2(i,j) * wm_interm_2_triplet_pt2(j,i)
term(753) = term(753) + wm_interm_122_triplet_pt2(i,q,p,j) * wm_interm_1_triplet_pt2(j,i)
term(754) = term(754) + wm_interm_122_triplet_pt2(i,q,p,j) * wm_interm_2_triplet_pt2(j,i)
term(755) = term(755) + wm_interm_122_triplet_pt2(i,q,j,p) * wm_interm_1_triplet_pt2(j,i)
term(756) = term(756) + wm_interm_122_triplet_pt2(i,q,j,p) * wm_interm_2_triplet_pt2(j,i)
term(757) = term(757) + wm_interm_133_triplet_pt2(i,j) * wm_interm_1_triplet_pt2(j,i)
term(758) = term(758) + wm_interm_133_triplet_pt2(i,j) * wm_interm_2_triplet_pt2(j,i)
term(759) = term(759) + wm_interm_134_triplet_pt2(i,j) * wm_interm_1_triplet_pt2(j,i)
term(760) = term(760) + wm_interm_134_triplet_pt2(i,j) * wm_interm_2_triplet_pt2(j,i)
term(761) = term(761) + wm_interm_40_triplet_pt2(p,i,j,q) * wm_interm_57_triplet_pt2(i,j)
term(762) = term(762) + wm_interm_40_triplet_pt2(i,p,j,q) * wm_interm_57_triplet_pt2(i,j)
term(763) = term(763) + wm_interm_41_triplet_pt2(i,j) * wm_interm_57_triplet_pt2(i,j)
term(764) = term(764) + wm_interm_42_triplet_pt2(i,j) * wm_interm_57_triplet_pt2(i,j)
term(765) = term(765) + wm_interm_40_triplet_pt2(i,p,q,j) * wm_interm_57_triplet_pt2(i,j)
term(766) = term(766) + wm_interm_43_triplet_pt2(i,j) * wm_interm_57_triplet_pt2(i,j)
term(767) = term(767) + wm_interm_40_triplet_pt2(p,i,j,q) * wm_interm_61_triplet_pt2(i,j)
term(768) = term(768) + wm_interm_40_triplet_pt2(i,p,j,q) * wm_interm_61_triplet_pt2(i,j)
term(769) = term(769) + wm_interm_41_triplet_pt2(i,j) * wm_interm_61_triplet_pt2(i,j)
term(770) = term(770) + wm_interm_42_triplet_pt2(i,j) * wm_interm_61_triplet_pt2(i,j)
term(771) = term(771) + wm_interm_40_triplet_pt2(i,p,q,j) * wm_interm_61_triplet_pt2(i,j)
term(772) = term(772) + wm_interm_43_triplet_pt2(i,j) * wm_interm_61_triplet_pt2(i,j)
term(773) = term(773) + wm_interm_40_triplet_pt2(p,i,j,q) * wm_interm_64_triplet_pt2(i,j)
term(774) = term(774) + wm_interm_40_triplet_pt2(i,p,j,q) * wm_interm_64_triplet_pt2(i,j)
term(775) = term(775) + wm_interm_41_triplet_pt2(i,j) * wm_interm_64_triplet_pt2(i,j)
term(776) = term(776) + wm_interm_42_triplet_pt2(i,j) * wm_interm_64_triplet_pt2(i,j)
term(777) = term(777) + wm_interm_40_triplet_pt2(i,p,q,j) * wm_interm_64_triplet_pt2(i,j)
term(778) = term(778) + wm_interm_43_triplet_pt2(i,j) * wm_interm_64_triplet_pt2(i,j)
term(779) = term(779) + wm_interm_16_triplet_pt2(p,i,j,q) * wm_interm_96_triplet_pt2(j,i)
term(780) = term(780) + wm_interm_16_triplet_pt2(p,i,j,q) * wm_interm_97_triplet_pt2(j,i)
term(781) = term(781) + wm_interm_16_triplet_pt2(i,p,j,q) * wm_interm_96_triplet_pt2(j,i)
term(782) = term(782) + wm_interm_16_triplet_pt2(i,p,j,q) * wm_interm_97_triplet_pt2(j,i)
term(783) = term(783) + wm_interm_16_triplet_pt2(p,i,j,q) * wm_interm_98_triplet_pt2(j,i)
term(784) = term(784) + wm_interm_16_triplet_pt2(i,p,j,q) * wm_interm_98_triplet_pt2(j,i)
term(785) = term(785) + wm_interm_30_triplet_pt2(i,j) * wm_interm_58_triplet_pt2(i,p,q,j)
term(786) = term(786) + wm_interm_30_triplet_pt2(i,j) * wm_interm_58_triplet_pt2(p,i,j,q)
term(787) = term(787) + wm_interm_30_triplet_pt2(i,j) * wm_interm_58_triplet_pt2(i,p,j,q)
term(788) = term(788) + wm_interm_31_triplet_pt2(i,j) * wm_interm_58_triplet_pt2(i,p,q,j)
term(789) = term(789) + wm_interm_31_triplet_pt2(i,j) * wm_interm_58_triplet_pt2(p,i,j,q)
term(790) = term(790) + wm_interm_31_triplet_pt2(i,j) * wm_interm_58_triplet_pt2(i,p,j,q)
term(791) = term(791) + wm_interm_32_triplet_pt2(i,j) * wm_interm_58_triplet_pt2(i,p,q,j)
term(792) = term(792) + wm_interm_32_triplet_pt2(i,j) * wm_interm_58_triplet_pt2(p,i,j,q)
term(793) = term(793) + wm_interm_32_triplet_pt2(i,j) * wm_interm_58_triplet_pt2(i,p,j,q)
term(794) = term(794) + wm_interm_41_triplet_pt2(i,j) * wm_interm_58_triplet_pt2(i,p,q,j)
term(795) = term(795) + wm_interm_41_triplet_pt2(i,j) * wm_interm_58_triplet_pt2(p,i,j,q)
term(796) = term(796) + wm_interm_41_triplet_pt2(i,j) * wm_interm_58_triplet_pt2(i,p,j,q)
term(797) = term(797) + wm_interm_42_triplet_pt2(i,j) * wm_interm_58_triplet_pt2(i,p,q,j)
term(798) = term(798) + wm_interm_42_triplet_pt2(i,j) * wm_interm_58_triplet_pt2(p,i,j,q)
term(799) = term(799) + wm_interm_42_triplet_pt2(i,j) * wm_interm_58_triplet_pt2(i,p,j,q)
term(800) = term(800) + wm_interm_43_triplet_pt2(i,j) * wm_interm_58_triplet_pt2(i,p,q,j)
term(801) = term(801) + wm_interm_43_triplet_pt2(i,j) * wm_interm_58_triplet_pt2(p,i,j,q)
term(802) = term(802) + wm_interm_43_triplet_pt2(i,j) * wm_interm_58_triplet_pt2(i,p,j,q)
term(803) = term(803) + wm_interm_144_triplet_pt2(q,i,j,p) * wm_interm_1_triplet_pt2(j,i)
term(804) = term(804) + wm_interm_144_triplet_pt2(q,i,p,j) * wm_interm_1_triplet_pt2(j,i)
term(805) = term(805) + wm_interm_144_triplet_pt2(q,i,j,p) * wm_interm_2_triplet_pt2(j,i)
term(806) = term(806) + wm_interm_144_triplet_pt2(q,i,p,j) * wm_interm_2_triplet_pt2(j,i)
term(807) = term(807) + wm_interm_145_triplet_pt2(q,i,p,j) * wm_interm_1_triplet_pt2(j,i)
term(808) = term(808) + wm_interm_145_triplet_pt2(q,i,p,j) * wm_interm_2_triplet_pt2(j,i)
term(809) = term(809) + wm_interm_146_triplet_pt2(i,j) * wm_interm_1_triplet_pt2(j,i)
term(810) = term(810) + wm_interm_147_triplet_pt2(i,j) * wm_interm_1_triplet_pt2(j,i)
term(811) = term(811) + wm_interm_146_triplet_pt2(i,j) * wm_interm_2_triplet_pt2(j,i)
term(812) = term(812) + wm_interm_147_triplet_pt2(i,j) * wm_interm_2_triplet_pt2(j,i)
term(813) = term(813) + wm_interm_148_triplet_pt2(i,j) * wm_interm_1_triplet_pt2(j,i)
term(814) = term(814) + wm_interm_148_triplet_pt2(i,j) * wm_interm_2_triplet_pt2(j,i)
term(815) = term(815) + wm_interm_144_triplet_pt2(i,q,p,j) * wm_interm_1_triplet_pt2(j,i)
term(816) = term(816) + wm_interm_144_triplet_pt2(i,q,p,j) * wm_interm_2_triplet_pt2(j,i)
term(817) = term(817) + wm_interm_144_triplet_pt2(i,q,j,p) * wm_interm_1_triplet_pt2(j,i)
term(818) = term(818) + wm_interm_144_triplet_pt2(i,q,j,p) * wm_interm_2_triplet_pt2(j,i)
term(819) = term(819) + wm_interm_159_triplet_pt2(i,j) * wm_interm_1_triplet_pt2(j,i)
term(820) = term(820) + wm_interm_159_triplet_pt2(i,j) * wm_interm_2_triplet_pt2(j,i)
term(821) = term(821) + wm_interm_160_triplet_pt2(i,j) * wm_interm_1_triplet_pt2(j,i)
term(822) = term(822) + wm_interm_160_triplet_pt2(i,j) * wm_interm_2_triplet_pt2(j,i)
term(823) = term(823) + wm_interm_145_triplet_pt2(i,q,j,p) * wm_interm_1_triplet_pt2(j,i)
term(824) = term(824) + wm_interm_145_triplet_pt2(i,q,j,p) * wm_interm_2_triplet_pt2(j,i)
term(825) = term(825) + wm_interm_161_triplet_pt2(i,j) * wm_interm_1_triplet_pt2(j,i)
term(826) = term(826) + wm_interm_161_triplet_pt2(i,j) * wm_interm_2_triplet_pt2(j,i)
term(827) = term(827) + wm_interm_29_triplet_pt2(p,i,j,q) * wm_interm_74_triplet_pt2(i,j)
term(828) = term(828) + wm_interm_29_triplet_pt2(i,p,j,q) * wm_interm_74_triplet_pt2(i,j)
term(829) = term(829) + wm_interm_30_triplet_pt2(i,j) * wm_interm_74_triplet_pt2(i,j)
term(830) = term(830) + wm_interm_31_triplet_pt2(i,j) * wm_interm_74_triplet_pt2(i,j)
term(831) = term(831) + wm_interm_29_triplet_pt2(i,p,q,j) * wm_interm_74_triplet_pt2(i,j)
term(832) = term(832) + wm_interm_32_triplet_pt2(i,j) * wm_interm_74_triplet_pt2(i,j)
term(833) = term(833) + wm_interm_29_triplet_pt2(p,i,j,q) * wm_interm_77_triplet_pt2(i,j)
term(834) = term(834) + wm_interm_29_triplet_pt2(i,p,j,q) * wm_interm_77_triplet_pt2(i,j)
term(835) = term(835) + wm_interm_30_triplet_pt2(i,j) * wm_interm_77_triplet_pt2(i,j)
term(836) = term(836) + wm_interm_31_triplet_pt2(i,j) * wm_interm_77_triplet_pt2(i,j)
term(837) = term(837) + wm_interm_29_triplet_pt2(i,p,q,j) * wm_interm_77_triplet_pt2(i,j)
term(838) = term(838) + wm_interm_32_triplet_pt2(i,j) * wm_interm_77_triplet_pt2(i,j)
term(839) = term(839) + wm_interm_166_triplet_pt2(q,i,j,p) * wm_interm_1_triplet_pt2(j,i)
term(840) = term(840) + wm_interm_166_triplet_pt2(q,i,p,j) * wm_interm_1_triplet_pt2(j,i)
term(841) = term(841) + wm_interm_166_triplet_pt2(q,i,j,p) * wm_interm_2_triplet_pt2(j,i)
term(842) = term(842) + wm_interm_166_triplet_pt2(q,i,p,j) * wm_interm_2_triplet_pt2(j,i)
term(843) = term(843) + wm_interm_167_triplet_pt2(i,j) * wm_interm_1_triplet_pt2(j,i)
term(844) = term(844) + wm_interm_168_triplet_pt2(i,j) * wm_interm_1_triplet_pt2(j,i)
term(845) = term(845) + wm_interm_167_triplet_pt2(i,j) * wm_interm_2_triplet_pt2(j,i)
term(846) = term(846) + wm_interm_168_triplet_pt2(i,j) * wm_interm_2_triplet_pt2(j,i)
term(847) = term(847) + wm_interm_166_triplet_pt2(i,q,p,j) * wm_interm_1_triplet_pt2(j,i)
term(848) = term(848) + wm_interm_166_triplet_pt2(i,q,p,j) * wm_interm_2_triplet_pt2(j,i)
term(849) = term(849) + wm_interm_166_triplet_pt2(i,q,j,p) * wm_interm_1_triplet_pt2(j,i)
term(850) = term(850) + wm_interm_166_triplet_pt2(i,q,j,p) * wm_interm_2_triplet_pt2(j,i)
term(851) = term(851) + wm_interm_175_triplet_pt2(i,j) * wm_interm_1_triplet_pt2(j,i)
term(852) = term(852) + wm_interm_175_triplet_pt2(i,j) * wm_interm_2_triplet_pt2(j,i)
term(853) = term(853) + wm_interm_176_triplet_pt2(i,j) * wm_interm_1_triplet_pt2(j,i)
term(854) = term(854) + wm_interm_176_triplet_pt2(i,j) * wm_interm_2_triplet_pt2(j,i)
term(855) = term(855) + wm_interm_40_triplet_pt2(p,i,j,q) * wm_interm_74_triplet_pt2(i,j)
term(856) = term(856) + wm_interm_40_triplet_pt2(i,p,j,q) * wm_interm_74_triplet_pt2(i,j)
term(857) = term(857) + wm_interm_41_triplet_pt2(i,j) * wm_interm_74_triplet_pt2(i,j)
term(858) = term(858) + wm_interm_42_triplet_pt2(i,j) * wm_interm_74_triplet_pt2(i,j)
term(859) = term(859) + wm_interm_40_triplet_pt2(i,p,q,j) * wm_interm_74_triplet_pt2(i,j)
term(860) = term(860) + wm_interm_43_triplet_pt2(i,j) * wm_interm_74_triplet_pt2(i,j)
term(861) = term(861) + wm_interm_40_triplet_pt2(p,i,j,q) * wm_interm_77_triplet_pt2(i,j)
term(862) = term(862) + wm_interm_40_triplet_pt2(i,p,j,q) * wm_interm_77_triplet_pt2(i,j)
term(863) = term(863) + wm_interm_41_triplet_pt2(i,j) * wm_interm_77_triplet_pt2(i,j)
term(864) = term(864) + wm_interm_42_triplet_pt2(i,j) * wm_interm_77_triplet_pt2(i,j)
term(865) = term(865) + wm_interm_40_triplet_pt2(i,p,q,j) * wm_interm_77_triplet_pt2(i,j)
term(866) = term(866) + wm_interm_43_triplet_pt2(i,j) * wm_interm_77_triplet_pt2(i,j)
term(867) = term(867) + wm_interm_30_triplet_pt2(i,j) * wm_interm_75_triplet_pt2(i,p,q,j)
term(868) = term(868) + wm_interm_30_triplet_pt2(i,j) * wm_interm_75_triplet_pt2(p,i,j,q)
term(869) = term(869) + wm_interm_30_triplet_pt2(i,j) * wm_interm_75_triplet_pt2(i,p,j,q)
term(870) = term(870) + wm_interm_31_triplet_pt2(i,j) * wm_interm_75_triplet_pt2(i,p,q,j)
term(871) = term(871) + wm_interm_31_triplet_pt2(i,j) * wm_interm_75_triplet_pt2(p,i,j,q)
term(872) = term(872) + wm_interm_31_triplet_pt2(i,j) * wm_interm_75_triplet_pt2(i,p,j,q)
term(873) = term(873) + wm_interm_32_triplet_pt2(i,j) * wm_interm_75_triplet_pt2(i,p,q,j)
term(874) = term(874) + wm_interm_32_triplet_pt2(i,j) * wm_interm_75_triplet_pt2(p,i,j,q)
term(875) = term(875) + wm_interm_32_triplet_pt2(i,j) * wm_interm_75_triplet_pt2(i,p,j,q)
term(876) = term(876) + wm_interm_16_triplet_pt2(p,i,j,q) * wm_interm_175_triplet_pt2(j,i)
term(877) = term(877) + wm_interm_16_triplet_pt2(p,i,j,q) * wm_interm_176_triplet_pt2(j,i)
term(878) = term(878) + wm_interm_16_triplet_pt2(i,p,j,q) * wm_interm_175_triplet_pt2(j,i)
term(879) = term(879) + wm_interm_16_triplet_pt2(i,p,j,q) * wm_interm_176_triplet_pt2(j,i)
term(880) = term(880) + wm_interm_41_triplet_pt2(i,j) * wm_interm_75_triplet_pt2(i,p,q,j)
term(881) = term(881) + wm_interm_41_triplet_pt2(i,j) * wm_interm_75_triplet_pt2(p,i,j,q)
term(882) = term(882) + wm_interm_41_triplet_pt2(i,j) * wm_interm_75_triplet_pt2(i,p,j,q)
term(883) = term(883) + wm_interm_42_triplet_pt2(i,j) * wm_interm_75_triplet_pt2(i,p,q,j)
term(884) = term(884) + wm_interm_42_triplet_pt2(i,j) * wm_interm_75_triplet_pt2(p,i,j,q)
term(885) = term(885) + wm_interm_42_triplet_pt2(i,j) * wm_interm_75_triplet_pt2(i,p,j,q)
term(886) = term(886) + wm_interm_43_triplet_pt2(i,j) * wm_interm_75_triplet_pt2(i,p,q,j)
term(887) = term(887) + wm_interm_43_triplet_pt2(i,j) * wm_interm_75_triplet_pt2(p,i,j,q)
term(888) = term(888) + wm_interm_43_triplet_pt2(i,j) * wm_interm_75_triplet_pt2(i,p,j,q)
end do 
end do 

term(711) = term(711) * (4.0d+0) 
term(712) = term(712) * (-8.0d+0) 
term(713) = term(713) * (-2.0d+0) 
term(714) = term(714) * (4.0d+0) 
term(715) = term(715) * (-1.0d+0) 
term(716) = term(716) * (2.0d+0) 
term(717) = term(717) * (2.0d+0) 
term(718) = term(718) * (-4.0d+0) 
term(719) = term(719) * (-1.0d+0) 
term(720) = term(720) * (2.0d+0) 
term(721) = term(721) * (-1.0d+0) 
term(722) = term(722) * (2.0d+0) 
term(723) = term(723) * (2.0d+0) 
term(724) = term(724) * (-4.0d+0) 
term(725) = term(725) * (-1.0d+0) 
term(726) = term(726) * (2.0d+0) 
term(727) = term(727) * (0.5d+0) 
term(728) = term(728) * (-1.0d+0) 
term(729) = term(729) * (-1.0d+0) 
term(730) = term(730) * (2.0d+0) 
term(731) = term(731) * (0.5d+0) 
term(732) = term(732) * (-1.0d+0) 
term(733) = term(733) * (-1.0d+0) 
term(734) = term(734) * (2.0d+0) 
term(735) = term(735) * (2.0d+0) 
term(736) = term(736) * (-4.0d+0) 
term(737) = term(737) * (-1.0d+0) 
term(738) = term(738) * (2.0d+0) 
term(739) = term(739) * (0.5d+0) 
term(740) = term(740) * (-1.0d+0) 
term(741) = term(741) * (-1.0d+0) 
term(742) = term(742) * (2.0d+0) 
term(743) = term(743) * (0.5d+0) 
term(744) = term(744) * (-1.0d+0) 
term(745) = term(745) * (2.0d+0) 
term(746) = term(746) * (-2.0d+0) 
term(747) = term(747) * (-4.0d+0) 
term(748) = term(748) * (4.0d+0) 
term(749) = term(749) * (-4.0d+0) 
term(750) = term(750) * (4.0d+0) 
term(751) = term(751) * (8.0d+0) 
term(752) = term(752) * (-8.0d+0) 
term(753) = term(753) * (2.0d+0) 
term(754) = term(754) * (-4.0d+0) 
term(755) = term(755) * (-2.0d+0) 
term(756) = term(756) * (4.0d+0) 
term(757) = term(757) * (-4.0d+0) 
term(758) = term(758) * (8.0d+0) 
term(759) = term(759) * (4.0d+0) 
term(760) = term(760) * (-8.0d+0) 
term(762) = term(762) * (-2.0d+0) 
term(763) = term(763) * (-2.0d+0) 
term(764) = term(764) * (4.0d+0) 
term(766) = term(766) * (-2.0d+0) 
term(767) = term(767) * (-2.0d+0) 
term(768) = term(768) * (4.0d+0) 
term(769) = term(769) * (4.0d+0) 
term(770) = term(770) * (-8.0d+0) 
term(771) = term(771) * (-2.0d+0) 
term(772) = term(772) * (4.0d+0) 
term(774) = term(774) * (-2.0d+0) 
term(775) = term(775) * (-2.0d+0) 
term(776) = term(776) * (4.0d+0) 
term(778) = term(778) * (-2.0d+0) 
term(779) = term(779) * (0.5d+0) 
term(780) = term(780) * (-1.0d+0) 
term(781) = term(781) * (-1.0d+0) 
term(782) = term(782) * (2.0d+0) 
term(783) = term(783) * (0.5d+0) 
term(784) = term(784) * (-1.0d+0) 
term(785) = term(785) * (0.5d+0) 
term(786) = term(786) * (0.5d+0) 
term(787) = term(787) * (-1.0d+0) 
term(788) = term(788) * (-1.0d+0) 
term(789) = term(789) * (-1.0d+0) 
term(790) = term(790) * (2.0d+0) 
term(791) = term(791) * (0.5d+0) 
term(792) = term(792) * (0.5d+0) 
term(793) = term(793) * (-1.0d+0) 
term(796) = term(796) * (-2.0d+0) 
term(797) = term(797) * (-2.0d+0) 
term(798) = term(798) * (-2.0d+0) 
term(799) = term(799) * (4.0d+0) 
term(802) = term(802) * (-2.0d+0) 
term(804) = term(804) * (-2.0d+0) 
term(805) = term(805) * (-2.0d+0) 
term(806) = term(806) * (4.0d+0) 
term(808) = term(808) * (-2.0d+0) 
term(809) = term(809) * (-2.0d+0) 
term(810) = term(810) * (4.0d+0) 
term(811) = term(811) * (4.0d+0) 
term(812) = term(812) * (-8.0d+0) 
term(813) = term(813) * (-2.0d+0) 
term(814) = term(814) * (4.0d+0) 
term(816) = term(816) * (-2.0d+0) 
term(817) = term(817) * (-2.0d+0) 
term(818) = term(818) * (4.0d+0) 
term(819) = term(819) * (-2.0d+0) 
term(820) = term(820) * (4.0d+0) 
term(821) = term(821) * (4.0d+0) 
term(822) = term(822) * (-8.0d+0) 
term(824) = term(824) * (-2.0d+0) 
term(825) = term(825) * (-2.0d+0) 
term(826) = term(826) * (4.0d+0) 
term(827) = term(827) * (2.0d+0) 
term(828) = term(828) * (-4.0d+0) 
term(829) = term(829) * (-4.0d+0) 
term(830) = term(830) * (8.0d+0) 
term(831) = term(831) * (2.0d+0) 
term(832) = term(832) * (-4.0d+0) 
term(833) = term(833) * (-2.0d+0) 
term(834) = term(834) * (4.0d+0) 
term(835) = term(835) * (4.0d+0) 
term(836) = term(836) * (-8.0d+0) 
term(837) = term(837) * (-2.0d+0) 
term(838) = term(838) * (4.0d+0) 
term(839) = term(839) * (4.0d+0) 
term(840) = term(840) * (-4.0d+0) 
term(841) = term(841) * (-8.0d+0) 
term(842) = term(842) * (8.0d+0) 
term(843) = term(843) * (-8.0d+0) 
term(844) = term(844) * (8.0d+0) 
term(845) = term(845) * (16.0d+0) 
term(846) = term(846) * (-16.0d+0) 
term(847) = term(847) * (4.0d+0) 
term(848) = term(848) * (-8.0d+0) 
term(849) = term(849) * (-4.0d+0) 
term(850) = term(850) * (8.0d+0) 
term(851) = term(851) * (-8.0d+0) 
term(852) = term(852) * (16.0d+0) 
term(853) = term(853) * (8.0d+0) 
term(854) = term(854) * (-16.0d+0) 
term(855) = term(855) * (4.0d+0) 
term(856) = term(856) * (-8.0d+0) 
term(857) = term(857) * (-8.0d+0) 
term(858) = term(858) * (16.0d+0) 
term(859) = term(859) * (4.0d+0) 
term(860) = term(860) * (-8.0d+0) 
term(861) = term(861) * (-4.0d+0) 
term(862) = term(862) * (8.0d+0) 
term(863) = term(863) * (8.0d+0) 
term(864) = term(864) * (-16.0d+0) 
term(865) = term(865) * (-4.0d+0) 
term(866) = term(866) * (8.0d+0) 
term(869) = term(869) * (-2.0d+0) 
term(870) = term(870) * (-2.0d+0) 
term(871) = term(871) * (-2.0d+0) 
term(872) = term(872) * (4.0d+0) 
term(875) = term(875) * (-2.0d+0) 
term(876) = term(876) * (4.0d+0) 
term(877) = term(877) * (-4.0d+0) 
term(878) = term(878) * (-8.0d+0) 
term(879) = term(879) * (8.0d+0) 
term(880) = term(880) * (2.0d+0) 
term(881) = term(881) * (2.0d+0) 
term(882) = term(882) * (-4.0d+0) 
term(883) = term(883) * (-4.0d+0) 
term(884) = term(884) * (-4.0d+0) 
term(885) = term(885) * (8.0d+0) 
term(886) = term(886) * (2.0d+0) 
term(887) = term(887) * (2.0d+0) 
term(888) = term(888) * (-4.0d+0) 

do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(889) = term(889) + wm_interm_4_triplet_pt2(a,b) * wm_interm_92_triplet_pt2(b,a,q,p)
term(890) = term(890) + wm_interm_4_triplet_pt2(a,b) * wm_interm_93_triplet_pt2(b,a)
term(891) = term(891) + wm_interm_5_triplet_pt2(a,b) * wm_interm_92_triplet_pt2(b,a,q,p)
term(892) = term(892) + wm_interm_5_triplet_pt2(a,b) * wm_interm_93_triplet_pt2(b,a)
term(893) = term(893) + wm_interm_25_triplet_pt2(a,b,p,q) * wm_interm_93_triplet_pt2(b,a)
term(894) = term(894) + wm_interm_28_triplet_pt2(a,b,p,q) * wm_interm_93_triplet_pt2(b,a)
term(895) = term(895) + wm_interm_27_triplet_pt2(a,b,p,q) * wm_interm_93_triplet_pt2(b,a)
term(896) = term(896) + wm_interm_26_triplet_pt2(a,b,p,q) * wm_interm_93_triplet_pt2(b,a)
term(897) = term(897) + wm_interm_105_triplet_pt2(a,b,q,p) * wm_interm_4_triplet_pt2(b,a)
term(898) = term(898) + wm_interm_106_triplet_pt2(a,b,q,p) * wm_interm_4_triplet_pt2(b,a)
term(899) = term(899) + wm_interm_105_triplet_pt2(a,b,q,p) * wm_interm_5_triplet_pt2(b,a)
term(900) = term(900) + wm_interm_106_triplet_pt2(a,b,q,p) * wm_interm_5_triplet_pt2(b,a)
term(901) = term(901) + wm_interm_107_triplet_pt2(a,b,q,p) * wm_interm_4_triplet_pt2(b,a)
term(902) = term(902) + wm_interm_107_triplet_pt2(a,b,q,p) * wm_interm_5_triplet_pt2(b,a)
term(903) = term(903) + wm_interm_102_triplet_pt2(a,b,q,p) * wm_interm_4_triplet_pt2(b,a)
term(904) = term(904) + wm_interm_103_triplet_pt2(a,b,q,p) * wm_interm_4_triplet_pt2(b,a)
term(905) = term(905) + wm_interm_108_triplet_pt2(a,b) * wm_interm_4_triplet_pt2(b,a)
term(906) = term(906) + wm_interm_109_triplet_pt2(a,b) * wm_interm_4_triplet_pt2(b,a)
term(907) = term(907) + wm_interm_102_triplet_pt2(a,b,q,p) * wm_interm_5_triplet_pt2(b,a)
term(908) = term(908) + wm_interm_103_triplet_pt2(a,b,q,p) * wm_interm_5_triplet_pt2(b,a)
term(909) = term(909) + wm_interm_108_triplet_pt2(a,b) * wm_interm_5_triplet_pt2(b,a)
term(910) = term(910) + wm_interm_109_triplet_pt2(a,b) * wm_interm_5_triplet_pt2(b,a)
term(911) = term(911) + wm_interm_101_triplet_pt2(a,b,q,p) * wm_interm_4_triplet_pt2(b,a)
term(912) = term(912) + wm_interm_110_triplet_pt2(a,b) * wm_interm_4_triplet_pt2(b,a)
term(913) = term(913) + wm_interm_101_triplet_pt2(a,b,q,p) * wm_interm_5_triplet_pt2(b,a)
term(914) = term(914) + wm_interm_110_triplet_pt2(a,b) * wm_interm_5_triplet_pt2(b,a)
term(915) = term(915) + wm_interm_104_triplet_pt2(a,b,q,p) * wm_interm_4_triplet_pt2(b,a)
term(916) = term(916) + wm_interm_104_triplet_pt2(a,b,q,p) * wm_interm_5_triplet_pt2(b,a)
term(917) = term(917) + wm_interm_4_triplet_pt2(a,b) * wm_interm_99_triplet_pt2(b,a,q,p)
term(918) = term(918) + wm_interm_5_triplet_pt2(a,b) * wm_interm_99_triplet_pt2(b,a,q,p)
term(919) = term(919) + wm_interm_100_triplet_pt2(a,b,q,p) * wm_interm_4_triplet_pt2(b,a)
term(920) = term(920) + wm_interm_100_triplet_pt2(a,b,q,p) * wm_interm_5_triplet_pt2(b,a)
term(921) = term(921) + wm_interm_117_triplet_pt2(a,b,q,p) * wm_interm_4_triplet_pt2(b,a)
term(922) = term(922) + wm_interm_119_triplet_pt2(a,b) * wm_interm_4_triplet_pt2(b,a)
term(923) = term(923) + wm_interm_117_triplet_pt2(a,b,q,p) * wm_interm_5_triplet_pt2(b,a)
term(924) = term(924) + wm_interm_119_triplet_pt2(a,b) * wm_interm_5_triplet_pt2(b,a)
term(925) = term(925) + wm_interm_115_triplet_pt2(a,b,q,p) * wm_interm_4_triplet_pt2(b,a)
term(926) = term(926) + wm_interm_120_triplet_pt2(a,b) * wm_interm_4_triplet_pt2(b,a)
term(927) = term(927) + wm_interm_115_triplet_pt2(a,b,q,p) * wm_interm_5_triplet_pt2(b,a)
term(928) = term(928) + wm_interm_120_triplet_pt2(a,b) * wm_interm_5_triplet_pt2(b,a)
term(929) = term(929) + wm_interm_116_triplet_pt2(a,b,q,p) * wm_interm_4_triplet_pt2(b,a)
term(930) = term(930) + wm_interm_121_triplet_pt2(a,b) * wm_interm_4_triplet_pt2(b,a)
term(931) = term(931) + wm_interm_116_triplet_pt2(a,b,q,p) * wm_interm_5_triplet_pt2(b,a)
term(932) = term(932) + wm_interm_121_triplet_pt2(a,b) * wm_interm_5_triplet_pt2(b,a)
term(933) = term(933) + wm_interm_18_triplet_pt2(a,b,p,q) * wm_interm_55_triplet_pt2(a,b)
term(934) = term(934) + wm_interm_20_triplet_pt2(a,b,p,q) * wm_interm_55_triplet_pt2(a,b)
term(935) = term(935) + wm_interm_15_triplet_pt2(a,b,p,q) * wm_interm_55_triplet_pt2(a,b)
term(936) = term(936) + wm_interm_55_triplet_pt2(a,b) * wm_interm_7_triplet_pt2(a,b,p,q)
term(937) = term(937) + wm_interm_17_triplet_pt2(a,b) * wm_interm_55_triplet_pt2(a,b)
term(938) = term(938) + wm_interm_19_triplet_pt2(a,b) * wm_interm_55_triplet_pt2(a,b)
term(939) = term(939) + wm_interm_24_triplet_pt2(a,b,p,q) * wm_interm_55_triplet_pt2(a,b)
term(940) = term(940) + wm_interm_55_triplet_pt2(a,b) * wm_interm_9_triplet_pt2(a,b,p,q)
term(941) = term(941) + wm_interm_23_triplet_pt2(a,b) * wm_interm_55_triplet_pt2(a,b)
term(942) = term(942) + wm_interm_18_triplet_pt2(a,b,p,q) * wm_interm_60_triplet_pt2(a,b)
term(943) = term(943) + wm_interm_20_triplet_pt2(a,b,p,q) * wm_interm_60_triplet_pt2(a,b)
term(944) = term(944) + wm_interm_24_triplet_pt2(a,b,p,q) * wm_interm_60_triplet_pt2(a,b)
term(945) = term(945) + wm_interm_15_triplet_pt2(a,b,p,q) * wm_interm_60_triplet_pt2(a,b)
term(946) = term(946) + wm_interm_60_triplet_pt2(a,b) * wm_interm_7_triplet_pt2(a,b,p,q)
term(947) = term(947) + wm_interm_17_triplet_pt2(a,b) * wm_interm_60_triplet_pt2(a,b)
term(948) = term(948) + wm_interm_19_triplet_pt2(a,b) * wm_interm_60_triplet_pt2(a,b)
term(949) = term(949) + wm_interm_60_triplet_pt2(a,b) * wm_interm_9_triplet_pt2(a,b,p,q)
term(950) = term(950) + wm_interm_23_triplet_pt2(a,b) * wm_interm_60_triplet_pt2(a,b)
term(951) = term(951) + wm_interm_18_triplet_pt2(a,b,p,q) * wm_interm_62_triplet_pt2(a,b)
term(952) = term(952) + wm_interm_20_triplet_pt2(a,b,p,q) * wm_interm_62_triplet_pt2(a,b)
term(953) = term(953) + wm_interm_24_triplet_pt2(a,b,p,q) * wm_interm_62_triplet_pt2(a,b)
term(954) = term(954) + wm_interm_15_triplet_pt2(a,b,p,q) * wm_interm_62_triplet_pt2(a,b)
term(955) = term(955) + wm_interm_62_triplet_pt2(a,b) * wm_interm_7_triplet_pt2(a,b,p,q)
term(956) = term(956) + wm_interm_17_triplet_pt2(a,b) * wm_interm_62_triplet_pt2(a,b)
term(957) = term(957) + wm_interm_19_triplet_pt2(a,b) * wm_interm_62_triplet_pt2(a,b)
term(958) = term(958) + wm_interm_62_triplet_pt2(a,b) * wm_interm_9_triplet_pt2(a,b,p,q)
term(959) = term(959) + wm_interm_23_triplet_pt2(a,b) * wm_interm_62_triplet_pt2(a,b)
term(960) = term(960) + wm_interm_129_triplet_pt2(a,b,q,p) * wm_interm_4_triplet_pt2(b,a)
term(961) = term(961) + wm_interm_130_triplet_pt2(a,b,q,p) * wm_interm_4_triplet_pt2(b,a)
term(962) = term(962) + wm_interm_129_triplet_pt2(a,b,q,p) * wm_interm_5_triplet_pt2(b,a)
term(963) = term(963) + wm_interm_130_triplet_pt2(a,b,q,p) * wm_interm_5_triplet_pt2(b,a)
term(964) = term(964) + wm_interm_127_triplet_pt2(a,b,q,p) * wm_interm_4_triplet_pt2(b,a)
term(965) = term(965) + wm_interm_128_triplet_pt2(a,b,q,p) * wm_interm_4_triplet_pt2(b,a)
term(966) = term(966) + wm_interm_131_triplet_pt2(a,b) * wm_interm_4_triplet_pt2(b,a)
term(967) = term(967) + wm_interm_132_triplet_pt2(a,b) * wm_interm_4_triplet_pt2(b,a)
term(968) = term(968) + wm_interm_127_triplet_pt2(a,b,q,p) * wm_interm_5_triplet_pt2(b,a)
term(969) = term(969) + wm_interm_128_triplet_pt2(a,b,q,p) * wm_interm_5_triplet_pt2(b,a)
term(970) = term(970) + wm_interm_131_triplet_pt2(a,b) * wm_interm_5_triplet_pt2(b,a)
term(971) = term(971) + wm_interm_132_triplet_pt2(a,b) * wm_interm_5_triplet_pt2(b,a)
term(972) = term(972) + wm_interm_125_triplet_pt2(a,b,q,p) * wm_interm_4_triplet_pt2(b,a)
term(973) = term(973) + wm_interm_125_triplet_pt2(a,b,q,p) * wm_interm_5_triplet_pt2(b,a)
term(974) = term(974) + wm_interm_126_triplet_pt2(a,b,q,p) * wm_interm_4_triplet_pt2(b,a)
term(975) = term(975) + wm_interm_126_triplet_pt2(a,b,q,p) * wm_interm_5_triplet_pt2(b,a)
term(976) = term(976) + wm_interm_135_triplet_pt2(a,b,q,p) * wm_interm_4_triplet_pt2(b,a)
term(977) = term(977) + wm_interm_137_triplet_pt2(a,b) * wm_interm_4_triplet_pt2(b,a)
term(978) = term(978) + wm_interm_135_triplet_pt2(a,b,q,p) * wm_interm_5_triplet_pt2(b,a)
term(979) = term(979) + wm_interm_137_triplet_pt2(a,b) * wm_interm_5_triplet_pt2(b,a)
term(980) = term(980) + wm_interm_136_triplet_pt2(a,b,q,p) * wm_interm_4_triplet_pt2(b,a)
term(981) = term(981) + wm_interm_138_triplet_pt2(a,b) * wm_interm_4_triplet_pt2(b,a)
term(982) = term(982) + wm_interm_136_triplet_pt2(a,b,q,p) * wm_interm_5_triplet_pt2(b,a)
term(983) = term(983) + wm_interm_138_triplet_pt2(a,b) * wm_interm_5_triplet_pt2(b,a)
term(984) = term(984) + wm_interm_35_triplet_pt2(a,b,p,q) * wm_interm_55_triplet_pt2(a,b)
term(985) = term(985) + wm_interm_37_triplet_pt2(a,b,p,q) * wm_interm_55_triplet_pt2(a,b)
term(986) = term(986) + wm_interm_12_triplet_pt2(a,b,p,q) * wm_interm_55_triplet_pt2(a,b)
term(987) = term(987) + wm_interm_11_triplet_pt2(a,b,p,q) * wm_interm_55_triplet_pt2(a,b)
term(988) = term(988) + wm_interm_34_triplet_pt2(a,b) * wm_interm_55_triplet_pt2(a,b)
term(989) = term(989) + wm_interm_36_triplet_pt2(a,b) * wm_interm_55_triplet_pt2(a,b)
term(990) = term(990) + wm_interm_35_triplet_pt2(a,b,p,q) * wm_interm_60_triplet_pt2(a,b)
term(991) = term(991) + wm_interm_37_triplet_pt2(a,b,p,q) * wm_interm_60_triplet_pt2(a,b)
term(992) = term(992) + wm_interm_12_triplet_pt2(a,b,p,q) * wm_interm_60_triplet_pt2(a,b)
term(993) = term(993) + wm_interm_11_triplet_pt2(a,b,p,q) * wm_interm_60_triplet_pt2(a,b)
term(994) = term(994) + wm_interm_34_triplet_pt2(a,b) * wm_interm_60_triplet_pt2(a,b)
term(995) = term(995) + wm_interm_36_triplet_pt2(a,b) * wm_interm_60_triplet_pt2(a,b)
term(996) = term(996) + wm_interm_35_triplet_pt2(a,b,p,q) * wm_interm_62_triplet_pt2(a,b)
term(997) = term(997) + wm_interm_37_triplet_pt2(a,b,p,q) * wm_interm_62_triplet_pt2(a,b)
term(998) = term(998) + wm_interm_12_triplet_pt2(a,b,p,q) * wm_interm_62_triplet_pt2(a,b)
term(999) = term(999) + wm_interm_11_triplet_pt2(a,b,p,q) * wm_interm_62_triplet_pt2(a,b)
term(1000) = term(1000) + wm_interm_34_triplet_pt2(a,b) * wm_interm_62_triplet_pt2(a,b)
term(1001) = term(1001) + wm_interm_36_triplet_pt2(a,b) * wm_interm_62_triplet_pt2(a,b)
term(1002) = term(1002) + wm_interm_120_triplet_pt2(a,b) * wm_interm_25_triplet_pt2(b,a,p,q)
term(1003) = term(1003) + wm_interm_121_triplet_pt2(a,b) * wm_interm_25_triplet_pt2(b,a,p,q)
term(1004) = term(1004) + wm_interm_110_triplet_pt2(a,b) * wm_interm_25_triplet_pt2(b,a,p,q)
term(1005) = term(1005) + wm_interm_108_triplet_pt2(a,b) * wm_interm_25_triplet_pt2(b,a,p,q)
term(1006) = term(1006) + wm_interm_109_triplet_pt2(a,b) * wm_interm_25_triplet_pt2(b,a,p,q)
term(1007) = term(1007) + wm_interm_119_triplet_pt2(a,b) * wm_interm_25_triplet_pt2(b,a,p,q)
term(1008) = term(1008) + wm_interm_120_triplet_pt2(a,b) * wm_interm_28_triplet_pt2(b,a,p,q)
term(1009) = term(1009) + wm_interm_121_triplet_pt2(a,b) * wm_interm_28_triplet_pt2(b,a,p,q)
term(1010) = term(1010) + wm_interm_110_triplet_pt2(a,b) * wm_interm_28_triplet_pt2(b,a,p,q)
term(1011) = term(1011) + wm_interm_108_triplet_pt2(a,b) * wm_interm_28_triplet_pt2(b,a,p,q)
term(1012) = term(1012) + wm_interm_109_triplet_pt2(a,b) * wm_interm_28_triplet_pt2(b,a,p,q)
term(1013) = term(1013) + wm_interm_119_triplet_pt2(a,b) * wm_interm_28_triplet_pt2(b,a,p,q)
term(1014) = term(1014) + wm_interm_23_triplet_pt2(a,b) * wm_interm_63_triplet_pt2(a,b,p,q)
term(1015) = term(1015) + wm_interm_23_triplet_pt2(a,b) * wm_interm_59_triplet_pt2(a,b,p,q)
term(1016) = term(1016) + wm_interm_23_triplet_pt2(a,b) * wm_interm_56_triplet_pt2(a,b,p,q)
term(1017) = term(1017) + wm_interm_17_triplet_pt2(a,b) * wm_interm_63_triplet_pt2(a,b,p,q)
term(1018) = term(1018) + wm_interm_17_triplet_pt2(a,b) * wm_interm_59_triplet_pt2(a,b,p,q)
term(1019) = term(1019) + wm_interm_17_triplet_pt2(a,b) * wm_interm_56_triplet_pt2(a,b,p,q)
term(1020) = term(1020) + wm_interm_19_triplet_pt2(a,b) * wm_interm_63_triplet_pt2(a,b,p,q)
term(1021) = term(1021) + wm_interm_19_triplet_pt2(a,b) * wm_interm_59_triplet_pt2(a,b,p,q)
term(1022) = term(1022) + wm_interm_19_triplet_pt2(a,b) * wm_interm_56_triplet_pt2(a,b,p,q)
term(1023) = term(1023) + wm_interm_120_triplet_pt2(a,b) * wm_interm_27_triplet_pt2(b,a,p,q)
term(1024) = term(1024) + wm_interm_121_triplet_pt2(a,b) * wm_interm_27_triplet_pt2(b,a,p,q)
term(1025) = term(1025) + wm_interm_110_triplet_pt2(a,b) * wm_interm_27_triplet_pt2(b,a,p,q)
term(1026) = term(1026) + wm_interm_108_triplet_pt2(a,b) * wm_interm_27_triplet_pt2(b,a,p,q)
term(1027) = term(1027) + wm_interm_109_triplet_pt2(a,b) * wm_interm_27_triplet_pt2(b,a,p,q)
term(1028) = term(1028) + wm_interm_119_triplet_pt2(a,b) * wm_interm_27_triplet_pt2(b,a,p,q)
term(1029) = term(1029) + wm_interm_120_triplet_pt2(a,b) * wm_interm_26_triplet_pt2(b,a,p,q)
term(1030) = term(1030) + wm_interm_121_triplet_pt2(a,b) * wm_interm_26_triplet_pt2(b,a,p,q)
term(1031) = term(1031) + wm_interm_110_triplet_pt2(a,b) * wm_interm_26_triplet_pt2(b,a,p,q)
term(1032) = term(1032) + wm_interm_108_triplet_pt2(a,b) * wm_interm_26_triplet_pt2(b,a,p,q)
term(1033) = term(1033) + wm_interm_109_triplet_pt2(a,b) * wm_interm_26_triplet_pt2(b,a,p,q)
term(1034) = term(1034) + wm_interm_119_triplet_pt2(a,b) * wm_interm_26_triplet_pt2(b,a,p,q)
term(1035) = term(1035) + wm_interm_23_triplet_pt2(a,b) * wm_interm_52_triplet_pt2(a,b,p,q)
term(1036) = term(1036) + wm_interm_23_triplet_pt2(a,b) * wm_interm_50_triplet_pt2(a,b,p,q)
term(1037) = term(1037) + wm_interm_23_triplet_pt2(a,b) * wm_interm_47_triplet_pt2(a,b,p,q)
term(1038) = term(1038) + wm_interm_17_triplet_pt2(a,b) * wm_interm_52_triplet_pt2(a,b,p,q)
term(1039) = term(1039) + wm_interm_17_triplet_pt2(a,b) * wm_interm_50_triplet_pt2(a,b,p,q)
term(1040) = term(1040) + wm_interm_17_triplet_pt2(a,b) * wm_interm_47_triplet_pt2(a,b,p,q)
term(1041) = term(1041) + wm_interm_19_triplet_pt2(a,b) * wm_interm_52_triplet_pt2(a,b,p,q)
term(1042) = term(1042) + wm_interm_19_triplet_pt2(a,b) * wm_interm_50_triplet_pt2(a,b,p,q)
term(1043) = term(1043) + wm_interm_19_triplet_pt2(a,b) * wm_interm_47_triplet_pt2(a,b,p,q)
term(1044) = term(1044) + wm_interm_137_triplet_pt2(a,b) * wm_interm_25_triplet_pt2(b,a,p,q)
term(1045) = term(1045) + wm_interm_138_triplet_pt2(a,b) * wm_interm_25_triplet_pt2(b,a,p,q)
term(1046) = term(1046) + wm_interm_131_triplet_pt2(a,b) * wm_interm_25_triplet_pt2(b,a,p,q)
term(1047) = term(1047) + wm_interm_132_triplet_pt2(a,b) * wm_interm_25_triplet_pt2(b,a,p,q)
term(1048) = term(1048) + wm_interm_137_triplet_pt2(a,b) * wm_interm_28_triplet_pt2(b,a,p,q)
term(1049) = term(1049) + wm_interm_138_triplet_pt2(a,b) * wm_interm_28_triplet_pt2(b,a,p,q)
term(1050) = term(1050) + wm_interm_131_triplet_pt2(a,b) * wm_interm_28_triplet_pt2(b,a,p,q)
term(1051) = term(1051) + wm_interm_132_triplet_pt2(a,b) * wm_interm_28_triplet_pt2(b,a,p,q)
term(1052) = term(1052) + wm_interm_34_triplet_pt2(a,b) * wm_interm_63_triplet_pt2(a,b,p,q)
term(1053) = term(1053) + wm_interm_34_triplet_pt2(a,b) * wm_interm_59_triplet_pt2(a,b,p,q)
term(1054) = term(1054) + wm_interm_34_triplet_pt2(a,b) * wm_interm_56_triplet_pt2(a,b,p,q)
term(1055) = term(1055) + wm_interm_36_triplet_pt2(a,b) * wm_interm_63_triplet_pt2(a,b,p,q)
term(1056) = term(1056) + wm_interm_36_triplet_pt2(a,b) * wm_interm_59_triplet_pt2(a,b,p,q)
term(1057) = term(1057) + wm_interm_36_triplet_pt2(a,b) * wm_interm_56_triplet_pt2(a,b,p,q)
term(1058) = term(1058) + wm_interm_137_triplet_pt2(a,b) * wm_interm_27_triplet_pt2(b,a,p,q)
term(1059) = term(1059) + wm_interm_138_triplet_pt2(a,b) * wm_interm_27_triplet_pt2(b,a,p,q)
term(1060) = term(1060) + wm_interm_131_triplet_pt2(a,b) * wm_interm_27_triplet_pt2(b,a,p,q)
term(1061) = term(1061) + wm_interm_132_triplet_pt2(a,b) * wm_interm_27_triplet_pt2(b,a,p,q)
term(1062) = term(1062) + wm_interm_137_triplet_pt2(a,b) * wm_interm_26_triplet_pt2(b,a,p,q)
term(1063) = term(1063) + wm_interm_138_triplet_pt2(a,b) * wm_interm_26_triplet_pt2(b,a,p,q)
term(1064) = term(1064) + wm_interm_131_triplet_pt2(a,b) * wm_interm_26_triplet_pt2(b,a,p,q)
term(1065) = term(1065) + wm_interm_132_triplet_pt2(a,b) * wm_interm_26_triplet_pt2(b,a,p,q)
term(1066) = term(1066) + wm_interm_34_triplet_pt2(a,b) * wm_interm_52_triplet_pt2(a,b,p,q)
term(1067) = term(1067) + wm_interm_34_triplet_pt2(a,b) * wm_interm_50_triplet_pt2(a,b,p,q)
term(1068) = term(1068) + wm_interm_34_triplet_pt2(a,b) * wm_interm_47_triplet_pt2(a,b,p,q)
term(1069) = term(1069) + wm_interm_36_triplet_pt2(a,b) * wm_interm_52_triplet_pt2(a,b,p,q)
term(1070) = term(1070) + wm_interm_36_triplet_pt2(a,b) * wm_interm_50_triplet_pt2(a,b,p,q)
term(1071) = term(1071) + wm_interm_36_triplet_pt2(a,b) * wm_interm_47_triplet_pt2(a,b,p,q)
term(1072) = term(1072) + wm_interm_154_triplet_pt2(a,b,q,p) * wm_interm_4_triplet_pt2(b,a)
term(1073) = term(1073) + wm_interm_155_triplet_pt2(a,b,q,p) * wm_interm_4_triplet_pt2(b,a)
term(1074) = term(1074) + wm_interm_154_triplet_pt2(a,b,q,p) * wm_interm_5_triplet_pt2(b,a)
term(1075) = term(1075) + wm_interm_155_triplet_pt2(a,b,q,p) * wm_interm_5_triplet_pt2(b,a)
term(1076) = term(1076) + wm_interm_149_triplet_pt2(a,b,q,p) * wm_interm_4_triplet_pt2(b,a)
term(1077) = term(1077) + wm_interm_149_triplet_pt2(a,b,q,p) * wm_interm_5_triplet_pt2(b,a)
term(1078) = term(1078) + wm_interm_152_triplet_pt2(a,b,q,p) * wm_interm_4_triplet_pt2(b,a)
term(1079) = term(1079) + wm_interm_153_triplet_pt2(a,b,q,p) * wm_interm_4_triplet_pt2(b,a)
term(1080) = term(1080) + wm_interm_156_triplet_pt2(a,b) * wm_interm_4_triplet_pt2(b,a)
term(1081) = term(1081) + wm_interm_157_triplet_pt2(a,b) * wm_interm_4_triplet_pt2(b,a)
term(1082) = term(1082) + wm_interm_152_triplet_pt2(a,b,q,p) * wm_interm_5_triplet_pt2(b,a)
term(1083) = term(1083) + wm_interm_153_triplet_pt2(a,b,q,p) * wm_interm_5_triplet_pt2(b,a)
term(1084) = term(1084) + wm_interm_156_triplet_pt2(a,b) * wm_interm_5_triplet_pt2(b,a)
term(1085) = term(1085) + wm_interm_157_triplet_pt2(a,b) * wm_interm_5_triplet_pt2(b,a)
term(1086) = term(1086) + wm_interm_151_triplet_pt2(a,b,q,p) * wm_interm_4_triplet_pt2(b,a)
term(1087) = term(1087) + wm_interm_158_triplet_pt2(a,b) * wm_interm_4_triplet_pt2(b,a)
term(1088) = term(1088) + wm_interm_151_triplet_pt2(a,b,q,p) * wm_interm_5_triplet_pt2(b,a)
term(1089) = term(1089) + wm_interm_158_triplet_pt2(a,b) * wm_interm_5_triplet_pt2(b,a)
term(1090) = term(1090) + wm_interm_150_triplet_pt2(a,b,q,p) * wm_interm_4_triplet_pt2(b,a)
term(1091) = term(1091) + wm_interm_150_triplet_pt2(a,b,q,p) * wm_interm_5_triplet_pt2(b,a)
term(1092) = term(1092) + wm_interm_163_triplet_pt2(a,b) * wm_interm_4_triplet_pt2(b,a)
term(1093) = term(1093) + wm_interm_163_triplet_pt2(a,b) * wm_interm_5_triplet_pt2(b,a)
term(1094) = term(1094) + wm_interm_164_triplet_pt2(a,b) * wm_interm_4_triplet_pt2(b,a)
term(1095) = term(1095) + wm_interm_164_triplet_pt2(a,b) * wm_interm_5_triplet_pt2(b,a)
term(1096) = term(1096) + wm_interm_162_triplet_pt2(a,b,q,p) * wm_interm_4_triplet_pt2(b,a)
term(1097) = term(1097) + wm_interm_165_triplet_pt2(a,b) * wm_interm_4_triplet_pt2(b,a)
term(1098) = term(1098) + wm_interm_162_triplet_pt2(a,b,q,p) * wm_interm_5_triplet_pt2(b,a)
term(1099) = term(1099) + wm_interm_165_triplet_pt2(a,b) * wm_interm_5_triplet_pt2(b,a)
term(1100) = term(1100) + wm_interm_18_triplet_pt2(a,b,p,q) * wm_interm_72_triplet_pt2(a,b)
term(1101) = term(1101) + wm_interm_20_triplet_pt2(a,b,p,q) * wm_interm_72_triplet_pt2(a,b)
term(1102) = term(1102) + wm_interm_15_triplet_pt2(a,b,p,q) * wm_interm_72_triplet_pt2(a,b)
term(1103) = term(1103) + wm_interm_72_triplet_pt2(a,b) * wm_interm_7_triplet_pt2(a,b,p,q)
term(1104) = term(1104) + wm_interm_17_triplet_pt2(a,b) * wm_interm_72_triplet_pt2(a,b)
term(1105) = term(1105) + wm_interm_19_triplet_pt2(a,b) * wm_interm_72_triplet_pt2(a,b)
term(1106) = term(1106) + wm_interm_24_triplet_pt2(a,b,p,q) * wm_interm_72_triplet_pt2(a,b)
term(1107) = term(1107) + wm_interm_72_triplet_pt2(a,b) * wm_interm_9_triplet_pt2(a,b,p,q)
term(1108) = term(1108) + wm_interm_23_triplet_pt2(a,b) * wm_interm_72_triplet_pt2(a,b)
term(1109) = term(1109) + wm_interm_18_triplet_pt2(a,b,p,q) * wm_interm_76_triplet_pt2(a,b)
term(1110) = term(1110) + wm_interm_20_triplet_pt2(a,b,p,q) * wm_interm_76_triplet_pt2(a,b)
term(1111) = term(1111) + wm_interm_24_triplet_pt2(a,b,p,q) * wm_interm_76_triplet_pt2(a,b)
term(1112) = term(1112) + wm_interm_15_triplet_pt2(a,b,p,q) * wm_interm_76_triplet_pt2(a,b)
term(1113) = term(1113) + wm_interm_76_triplet_pt2(a,b) * wm_interm_7_triplet_pt2(a,b,p,q)
term(1114) = term(1114) + wm_interm_17_triplet_pt2(a,b) * wm_interm_76_triplet_pt2(a,b)
term(1115) = term(1115) + wm_interm_19_triplet_pt2(a,b) * wm_interm_76_triplet_pt2(a,b)
term(1116) = term(1116) + wm_interm_76_triplet_pt2(a,b) * wm_interm_9_triplet_pt2(a,b,p,q)
term(1117) = term(1117) + wm_interm_23_triplet_pt2(a,b) * wm_interm_76_triplet_pt2(a,b)
term(1118) = term(1118) + wm_interm_169_triplet_pt2(a,b,q,p) * wm_interm_4_triplet_pt2(b,a)
term(1119) = term(1119) + wm_interm_170_triplet_pt2(a,b,q,p) * wm_interm_4_triplet_pt2(b,a)
term(1120) = term(1120) + wm_interm_169_triplet_pt2(a,b,q,p) * wm_interm_5_triplet_pt2(b,a)
term(1121) = term(1121) + wm_interm_170_triplet_pt2(a,b,q,p) * wm_interm_5_triplet_pt2(b,a)
term(1122) = term(1122) + wm_interm_171_triplet_pt2(a,b,q,p) * wm_interm_4_triplet_pt2(b,a)
term(1123) = term(1123) + wm_interm_172_triplet_pt2(a,b,q,p) * wm_interm_4_triplet_pt2(b,a)
term(1124) = term(1124) + wm_interm_173_triplet_pt2(a,b) * wm_interm_4_triplet_pt2(b,a)
term(1125) = term(1125) + wm_interm_174_triplet_pt2(a,b) * wm_interm_4_triplet_pt2(b,a)
term(1126) = term(1126) + wm_interm_171_triplet_pt2(a,b,q,p) * wm_interm_5_triplet_pt2(b,a)
term(1127) = term(1127) + wm_interm_172_triplet_pt2(a,b,q,p) * wm_interm_5_triplet_pt2(b,a)
term(1128) = term(1128) + wm_interm_173_triplet_pt2(a,b) * wm_interm_5_triplet_pt2(b,a)
term(1129) = term(1129) + wm_interm_174_triplet_pt2(a,b) * wm_interm_5_triplet_pt2(b,a)
term(1130) = term(1130) + wm_interm_177_triplet_pt2(a,b) * wm_interm_4_triplet_pt2(b,a)
term(1131) = term(1131) + wm_interm_177_triplet_pt2(a,b) * wm_interm_5_triplet_pt2(b,a)
term(1132) = term(1132) + wm_interm_178_triplet_pt2(a,b) * wm_interm_4_triplet_pt2(b,a)
term(1133) = term(1133) + wm_interm_178_triplet_pt2(a,b) * wm_interm_5_triplet_pt2(b,a)
term(1134) = term(1134) + wm_interm_35_triplet_pt2(a,b,p,q) * wm_interm_72_triplet_pt2(a,b)
term(1135) = term(1135) + wm_interm_37_triplet_pt2(a,b,p,q) * wm_interm_72_triplet_pt2(a,b)
term(1136) = term(1136) + wm_interm_12_triplet_pt2(a,b,p,q) * wm_interm_72_triplet_pt2(a,b)
term(1137) = term(1137) + wm_interm_11_triplet_pt2(a,b,p,q) * wm_interm_72_triplet_pt2(a,b)
term(1138) = term(1138) + wm_interm_34_triplet_pt2(a,b) * wm_interm_72_triplet_pt2(a,b)
term(1139) = term(1139) + wm_interm_36_triplet_pt2(a,b) * wm_interm_72_triplet_pt2(a,b)
term(1140) = term(1140) + wm_interm_35_triplet_pt2(a,b,p,q) * wm_interm_76_triplet_pt2(a,b)
term(1141) = term(1141) + wm_interm_37_triplet_pt2(a,b,p,q) * wm_interm_76_triplet_pt2(a,b)
term(1142) = term(1142) + wm_interm_12_triplet_pt2(a,b,p,q) * wm_interm_76_triplet_pt2(a,b)
term(1143) = term(1143) + wm_interm_11_triplet_pt2(a,b,p,q) * wm_interm_76_triplet_pt2(a,b)
term(1144) = term(1144) + wm_interm_34_triplet_pt2(a,b) * wm_interm_76_triplet_pt2(a,b)
term(1145) = term(1145) + wm_interm_36_triplet_pt2(a,b) * wm_interm_76_triplet_pt2(a,b)
term(1146) = term(1146) + wm_interm_164_triplet_pt2(a,b) * wm_interm_25_triplet_pt2(b,a,p,q)
term(1147) = term(1147) + wm_interm_165_triplet_pt2(a,b) * wm_interm_25_triplet_pt2(b,a,p,q)
term(1148) = term(1148) + wm_interm_158_triplet_pt2(a,b) * wm_interm_25_triplet_pt2(b,a,p,q)
term(1149) = term(1149) + wm_interm_156_triplet_pt2(a,b) * wm_interm_25_triplet_pt2(b,a,p,q)
term(1150) = term(1150) + wm_interm_157_triplet_pt2(a,b) * wm_interm_25_triplet_pt2(b,a,p,q)
term(1151) = term(1151) + wm_interm_163_triplet_pt2(a,b) * wm_interm_25_triplet_pt2(b,a,p,q)
term(1152) = term(1152) + wm_interm_164_triplet_pt2(a,b) * wm_interm_28_triplet_pt2(b,a,p,q)
term(1153) = term(1153) + wm_interm_165_triplet_pt2(a,b) * wm_interm_28_triplet_pt2(b,a,p,q)
term(1154) = term(1154) + wm_interm_158_triplet_pt2(a,b) * wm_interm_28_triplet_pt2(b,a,p,q)
term(1155) = term(1155) + wm_interm_156_triplet_pt2(a,b) * wm_interm_28_triplet_pt2(b,a,p,q)
term(1156) = term(1156) + wm_interm_157_triplet_pt2(a,b) * wm_interm_28_triplet_pt2(b,a,p,q)
term(1157) = term(1157) + wm_interm_163_triplet_pt2(a,b) * wm_interm_28_triplet_pt2(b,a,p,q)
term(1158) = term(1158) + wm_interm_23_triplet_pt2(a,b) * wm_interm_73_triplet_pt2(a,b,p,q)
term(1159) = term(1159) + wm_interm_23_triplet_pt2(a,b) * wm_interm_71_triplet_pt2(a,b,p,q)
term(1160) = term(1160) + wm_interm_17_triplet_pt2(a,b) * wm_interm_73_triplet_pt2(a,b,p,q)
term(1161) = term(1161) + wm_interm_17_triplet_pt2(a,b) * wm_interm_71_triplet_pt2(a,b,p,q)
term(1162) = term(1162) + wm_interm_19_triplet_pt2(a,b) * wm_interm_73_triplet_pt2(a,b,p,q)
term(1163) = term(1163) + wm_interm_19_triplet_pt2(a,b) * wm_interm_71_triplet_pt2(a,b,p,q)
term(1164) = term(1164) + wm_interm_164_triplet_pt2(a,b) * wm_interm_27_triplet_pt2(b,a,p,q)
term(1165) = term(1165) + wm_interm_165_triplet_pt2(a,b) * wm_interm_27_triplet_pt2(b,a,p,q)
term(1166) = term(1166) + wm_interm_158_triplet_pt2(a,b) * wm_interm_27_triplet_pt2(b,a,p,q)
term(1167) = term(1167) + wm_interm_156_triplet_pt2(a,b) * wm_interm_27_triplet_pt2(b,a,p,q)
term(1168) = term(1168) + wm_interm_157_triplet_pt2(a,b) * wm_interm_27_triplet_pt2(b,a,p,q)
term(1169) = term(1169) + wm_interm_163_triplet_pt2(a,b) * wm_interm_27_triplet_pt2(b,a,p,q)
term(1170) = term(1170) + wm_interm_164_triplet_pt2(a,b) * wm_interm_26_triplet_pt2(b,a,p,q)
term(1171) = term(1171) + wm_interm_165_triplet_pt2(a,b) * wm_interm_26_triplet_pt2(b,a,p,q)
term(1172) = term(1172) + wm_interm_158_triplet_pt2(a,b) * wm_interm_26_triplet_pt2(b,a,p,q)
term(1173) = term(1173) + wm_interm_156_triplet_pt2(a,b) * wm_interm_26_triplet_pt2(b,a,p,q)
term(1174) = term(1174) + wm_interm_157_triplet_pt2(a,b) * wm_interm_26_triplet_pt2(b,a,p,q)
term(1175) = term(1175) + wm_interm_163_triplet_pt2(a,b) * wm_interm_26_triplet_pt2(b,a,p,q)
term(1176) = term(1176) + wm_interm_23_triplet_pt2(a,b) * wm_interm_69_triplet_pt2(a,b,p,q)
term(1177) = term(1177) + wm_interm_23_triplet_pt2(a,b) * wm_interm_68_triplet_pt2(a,b,p,q)
term(1178) = term(1178) + wm_interm_17_triplet_pt2(a,b) * wm_interm_69_triplet_pt2(a,b,p,q)
term(1179) = term(1179) + wm_interm_17_triplet_pt2(a,b) * wm_interm_68_triplet_pt2(a,b,p,q)
term(1180) = term(1180) + wm_interm_19_triplet_pt2(a,b) * wm_interm_69_triplet_pt2(a,b,p,q)
term(1181) = term(1181) + wm_interm_19_triplet_pt2(a,b) * wm_interm_68_triplet_pt2(a,b,p,q)
term(1182) = term(1182) + wm_interm_177_triplet_pt2(a,b) * wm_interm_25_triplet_pt2(b,a,p,q)
term(1183) = term(1183) + wm_interm_178_triplet_pt2(a,b) * wm_interm_25_triplet_pt2(b,a,p,q)
term(1184) = term(1184) + wm_interm_173_triplet_pt2(a,b) * wm_interm_25_triplet_pt2(b,a,p,q)
term(1185) = term(1185) + wm_interm_174_triplet_pt2(a,b) * wm_interm_25_triplet_pt2(b,a,p,q)
term(1186) = term(1186) + wm_interm_177_triplet_pt2(a,b) * wm_interm_28_triplet_pt2(b,a,p,q)
term(1187) = term(1187) + wm_interm_178_triplet_pt2(a,b) * wm_interm_28_triplet_pt2(b,a,p,q)
term(1188) = term(1188) + wm_interm_173_triplet_pt2(a,b) * wm_interm_28_triplet_pt2(b,a,p,q)
term(1189) = term(1189) + wm_interm_174_triplet_pt2(a,b) * wm_interm_28_triplet_pt2(b,a,p,q)
term(1190) = term(1190) + wm_interm_34_triplet_pt2(a,b) * wm_interm_73_triplet_pt2(a,b,p,q)
term(1191) = term(1191) + wm_interm_34_triplet_pt2(a,b) * wm_interm_71_triplet_pt2(a,b,p,q)
term(1192) = term(1192) + wm_interm_36_triplet_pt2(a,b) * wm_interm_73_triplet_pt2(a,b,p,q)
term(1193) = term(1193) + wm_interm_36_triplet_pt2(a,b) * wm_interm_71_triplet_pt2(a,b,p,q)
term(1194) = term(1194) + wm_interm_177_triplet_pt2(a,b) * wm_interm_27_triplet_pt2(b,a,p,q)
term(1195) = term(1195) + wm_interm_178_triplet_pt2(a,b) * wm_interm_27_triplet_pt2(b,a,p,q)
term(1196) = term(1196) + wm_interm_173_triplet_pt2(a,b) * wm_interm_27_triplet_pt2(b,a,p,q)
term(1197) = term(1197) + wm_interm_174_triplet_pt2(a,b) * wm_interm_27_triplet_pt2(b,a,p,q)
term(1198) = term(1198) + wm_interm_177_triplet_pt2(a,b) * wm_interm_26_triplet_pt2(b,a,p,q)
term(1199) = term(1199) + wm_interm_178_triplet_pt2(a,b) * wm_interm_26_triplet_pt2(b,a,p,q)
term(1200) = term(1200) + wm_interm_173_triplet_pt2(a,b) * wm_interm_26_triplet_pt2(b,a,p,q)
term(1201) = term(1201) + wm_interm_174_triplet_pt2(a,b) * wm_interm_26_triplet_pt2(b,a,p,q)
term(1202) = term(1202) + wm_interm_34_triplet_pt2(a,b) * wm_interm_69_triplet_pt2(a,b,p,q)
term(1203) = term(1203) + wm_interm_34_triplet_pt2(a,b) * wm_interm_68_triplet_pt2(a,b,p,q)
term(1204) = term(1204) + wm_interm_36_triplet_pt2(a,b) * wm_interm_69_triplet_pt2(a,b,p,q)
term(1205) = term(1205) + wm_interm_36_triplet_pt2(a,b) * wm_interm_68_triplet_pt2(a,b,p,q)
end do 
end do 

term(889) = term(889) * (-2.0d+0) 
term(890) = term(890) * (4.0d+0) 
term(891) = term(891) * (4.0d+0) 
term(892) = term(892) * (-8.0d+0) 
term(893) = term(893) * (-2.0d+0) 
term(894) = term(894) * (4.0d+0) 
term(895) = term(895) * (-2.0d+0) 
term(896) = term(896) * (4.0d+0) 
term(897) = term(897) * (0.5d+0) 
term(898) = term(898) * (-1.0d+0) 
term(899) = term(899) * (-1.0d+0) 
term(900) = term(900) * (2.0d+0) 
term(901) = term(901) * (0.5d+0) 
term(902) = term(902) * (-1.0d+0) 
term(903) = term(903) * (0.5d+0) 
term(904) = term(904) * (-1.0d+0) 
term(905) = term(905) * (-1.0d+0) 
term(906) = term(906) * (2.0d+0) 
term(907) = term(907) * (-1.0d+0) 
term(908) = term(908) * (2.0d+0) 
term(909) = term(909) * (2.0d+0) 
term(910) = term(910) * (-4.0d+0) 
term(911) = term(911) * (0.5d+0) 
term(912) = term(912) * (-1.0d+0) 
term(913) = term(913) * (-1.0d+0) 
term(914) = term(914) * (2.0d+0) 
term(915) = term(915) * (0.5d+0) 
term(916) = term(916) * (-1.0d+0) 
term(917) = term(917) * (0.5d+0) 
term(918) = term(918) * (-1.0d+0) 
term(919) = term(919) * (-1.0d+0) 
term(920) = term(920) * (2.0d+0) 
term(921) = term(921) * (0.5d+0) 
term(922) = term(922) * (-1.0d+0) 
term(923) = term(923) * (-1.0d+0) 
term(924) = term(924) * (2.0d+0) 
term(925) = term(925) * (0.5d+0) 
term(926) = term(926) * (-1.0d+0) 
term(927) = term(927) * (-1.0d+0) 
term(928) = term(928) * (2.0d+0) 
term(929) = term(929) * (-1.0d+0) 
term(930) = term(930) * (2.0d+0) 
term(931) = term(931) * (2.0d+0) 
term(932) = term(932) * (-4.0d+0) 
term(933) = term(933) * (0.5d+0) 
term(934) = term(934) * (-1.0d+0) 
term(935) = term(935) * (0.5d+0) 
term(936) = term(936) * (-1.0d+0) 
term(937) = term(937) * (-1.0d+0) 
term(938) = term(938) * (2.0d+0) 
term(939) = term(939) * (0.5d+0) 
term(940) = term(940) * (0.5d+0) 
term(941) = term(941) * (-1.0d+0) 
term(942) = term(942) * (-1.0d+0) 
term(943) = term(943) * (2.0d+0) 
term(944) = term(944) * (-1.0d+0) 
term(945) = term(945) * (-1.0d+0) 
term(946) = term(946) * (2.0d+0) 
term(947) = term(947) * (2.0d+0) 
term(948) = term(948) * (-4.0d+0) 
term(949) = term(949) * (-1.0d+0) 
term(950) = term(950) * (2.0d+0) 
term(951) = term(951) * (0.5d+0) 
term(952) = term(952) * (-1.0d+0) 
term(953) = term(953) * (0.5d+0) 
term(954) = term(954) * (0.5d+0) 
term(955) = term(955) * (-1.0d+0) 
term(956) = term(956) * (-1.0d+0) 
term(957) = term(957) * (2.0d+0) 
term(958) = term(958) * (0.5d+0) 
term(959) = term(959) * (-1.0d+0) 
term(960) = term(960) * (2.0d+0) 
term(961) = term(961) * (-2.0d+0) 
term(962) = term(962) * (-4.0d+0) 
term(963) = term(963) * (4.0d+0) 
term(964) = term(964) * (2.0d+0) 
term(965) = term(965) * (-2.0d+0) 
term(966) = term(966) * (-4.0d+0) 
term(967) = term(967) * (4.0d+0) 
term(968) = term(968) * (-4.0d+0) 
term(969) = term(969) * (4.0d+0) 
term(970) = term(970) * (8.0d+0) 
term(971) = term(971) * (-8.0d+0) 
term(972) = term(972) * (2.0d+0) 
term(973) = term(973) * (-4.0d+0) 
term(974) = term(974) * (-2.0d+0) 
term(975) = term(975) * (4.0d+0) 
term(976) = term(976) * (2.0d+0) 
term(977) = term(977) * (-4.0d+0) 
term(978) = term(978) * (-4.0d+0) 
term(979) = term(979) * (8.0d+0) 
term(980) = term(980) * (-2.0d+0) 
term(981) = term(981) * (4.0d+0) 
term(982) = term(982) * (4.0d+0) 
term(983) = term(983) * (-8.0d+0) 
term(984) = term(984) * (2.0d+0) 
term(985) = term(985) * (-2.0d+0) 
term(986) = term(986) * (2.0d+0) 
term(987) = term(987) * (-2.0d+0) 
term(988) = term(988) * (-4.0d+0) 
term(989) = term(989) * (4.0d+0) 
term(990) = term(990) * (-4.0d+0) 
term(991) = term(991) * (4.0d+0) 
term(992) = term(992) * (-4.0d+0) 
term(993) = term(993) * (4.0d+0) 
term(994) = term(994) * (8.0d+0) 
term(995) = term(995) * (-8.0d+0) 
term(996) = term(996) * (2.0d+0) 
term(997) = term(997) * (-2.0d+0) 
term(998) = term(998) * (2.0d+0) 
term(999) = term(999) * (-2.0d+0) 
term(1000) = term(1000) * (-4.0d+0) 
term(1001) = term(1001) * (4.0d+0) 
term(1002) = term(1002) * (0.5d+0) 
term(1003) = term(1003) * (-1.0d+0) 
term(1004) = term(1004) * (0.5d+0) 
term(1005) = term(1005) * (0.5d+0) 
term(1006) = term(1006) * (-1.0d+0) 
term(1007) = term(1007) * (0.5d+0) 
term(1008) = term(1008) * (-1.0d+0) 
term(1009) = term(1009) * (2.0d+0) 
term(1010) = term(1010) * (-1.0d+0) 
term(1011) = term(1011) * (-1.0d+0) 
term(1012) = term(1012) * (2.0d+0) 
term(1013) = term(1013) * (-1.0d+0) 
term(1014) = term(1014) * (0.5d+0) 
term(1015) = term(1015) * (-1.0d+0) 
term(1016) = term(1016) * (0.5d+0) 
term(1017) = term(1017) * (0.5d+0) 
term(1018) = term(1018) * (-1.0d+0) 
term(1019) = term(1019) * (0.5d+0) 
term(1020) = term(1020) * (-1.0d+0) 
term(1021) = term(1021) * (2.0d+0) 
term(1022) = term(1022) * (-1.0d+0) 
term(1023) = term(1023) * (0.5d+0) 
term(1024) = term(1024) * (-1.0d+0) 
term(1025) = term(1025) * (0.5d+0) 
term(1026) = term(1026) * (0.5d+0) 
term(1027) = term(1027) * (-1.0d+0) 
term(1028) = term(1028) * (0.5d+0) 
term(1029) = term(1029) * (-1.0d+0) 
term(1030) = term(1030) * (2.0d+0) 
term(1031) = term(1031) * (-1.0d+0) 
term(1032) = term(1032) * (-1.0d+0) 
term(1033) = term(1033) * (2.0d+0) 
term(1034) = term(1034) * (-1.0d+0) 
term(1035) = term(1035) * (0.5d+0) 
term(1036) = term(1036) * (0.5d+0) 
term(1037) = term(1037) * (-1.0d+0) 
term(1038) = term(1038) * (0.5d+0) 
term(1039) = term(1039) * (0.5d+0) 
term(1040) = term(1040) * (-1.0d+0) 
term(1041) = term(1041) * (-1.0d+0) 
term(1042) = term(1042) * (-1.0d+0) 
term(1043) = term(1043) * (2.0d+0) 
term(1044) = term(1044) * (2.0d+0) 
term(1045) = term(1045) * (-2.0d+0) 
term(1046) = term(1046) * (2.0d+0) 
term(1047) = term(1047) * (-2.0d+0) 
term(1048) = term(1048) * (-4.0d+0) 
term(1049) = term(1049) * (4.0d+0) 
term(1050) = term(1050) * (-4.0d+0) 
term(1051) = term(1051) * (4.0d+0) 
term(1052) = term(1052) * (2.0d+0) 
term(1053) = term(1053) * (-4.0d+0) 
term(1054) = term(1054) * (2.0d+0) 
term(1055) = term(1055) * (-2.0d+0) 
term(1056) = term(1056) * (4.0d+0) 
term(1057) = term(1057) * (-2.0d+0) 
term(1058) = term(1058) * (2.0d+0) 
term(1059) = term(1059) * (-2.0d+0) 
term(1060) = term(1060) * (2.0d+0) 
term(1061) = term(1061) * (-2.0d+0) 
term(1062) = term(1062) * (-4.0d+0) 
term(1063) = term(1063) * (4.0d+0) 
term(1064) = term(1064) * (-4.0d+0) 
term(1065) = term(1065) * (4.0d+0) 
term(1066) = term(1066) * (2.0d+0) 
term(1067) = term(1067) * (2.0d+0) 
term(1068) = term(1068) * (-4.0d+0) 
term(1069) = term(1069) * (-2.0d+0) 
term(1070) = term(1070) * (-2.0d+0) 
term(1071) = term(1071) * (4.0d+0) 
term(1072) = term(1072) * (2.0d+0) 
term(1073) = term(1073) * (-2.0d+0) 
term(1074) = term(1074) * (-4.0d+0) 
term(1075) = term(1075) * (4.0d+0) 
term(1076) = term(1076) * (2.0d+0) 
term(1077) = term(1077) * (-4.0d+0) 
term(1078) = term(1078) * (2.0d+0) 
term(1079) = term(1079) * (-2.0d+0) 
term(1080) = term(1080) * (-2.0d+0) 
term(1081) = term(1081) * (4.0d+0) 
term(1082) = term(1082) * (-4.0d+0) 
term(1083) = term(1083) * (4.0d+0) 
term(1084) = term(1084) * (4.0d+0) 
term(1085) = term(1085) * (-8.0d+0) 
term(1086) = term(1086) * (2.0d+0) 
term(1087) = term(1087) * (-2.0d+0) 
term(1088) = term(1088) * (-4.0d+0) 
term(1089) = term(1089) * (4.0d+0) 
term(1090) = term(1090) * (-2.0d+0) 
term(1091) = term(1091) * (4.0d+0) 
term(1092) = term(1092) * (-2.0d+0) 
term(1093) = term(1093) * (4.0d+0) 
term(1094) = term(1094) * (-2.0d+0) 
term(1095) = term(1095) * (4.0d+0) 
term(1096) = term(1096) * (-2.0d+0) 
term(1097) = term(1097) * (4.0d+0) 
term(1098) = term(1098) * (4.0d+0) 
term(1099) = term(1099) * (-8.0d+0) 
term(1100) = term(1100) * (2.0d+0) 
term(1101) = term(1101) * (-4.0d+0) 
term(1102) = term(1102) * (2.0d+0) 
term(1103) = term(1103) * (-4.0d+0) 
term(1104) = term(1104) * (-4.0d+0) 
term(1105) = term(1105) * (8.0d+0) 
term(1106) = term(1106) * (2.0d+0) 
term(1107) = term(1107) * (2.0d+0) 
term(1108) = term(1108) * (-4.0d+0) 
term(1109) = term(1109) * (-2.0d+0) 
term(1110) = term(1110) * (4.0d+0) 
term(1111) = term(1111) * (-2.0d+0) 
term(1112) = term(1112) * (-2.0d+0) 
term(1113) = term(1113) * (4.0d+0) 
term(1114) = term(1114) * (4.0d+0) 
term(1115) = term(1115) * (-8.0d+0) 
term(1116) = term(1116) * (-2.0d+0) 
term(1117) = term(1117) * (4.0d+0) 
term(1118) = term(1118) * (8.0d+0) 
term(1119) = term(1119) * (-8.0d+0) 
term(1120) = term(1120) * (-16.0d+0) 
term(1121) = term(1121) * (16.0d+0) 
term(1122) = term(1122) * (8.0d+0) 
term(1123) = term(1123) * (-8.0d+0) 
term(1124) = term(1124) * (-8.0d+0) 
term(1125) = term(1125) * (8.0d+0) 
term(1126) = term(1126) * (-16.0d+0) 
term(1127) = term(1127) * (16.0d+0) 
term(1128) = term(1128) * (16.0d+0) 
term(1129) = term(1129) * (-16.0d+0) 
term(1130) = term(1130) * (-8.0d+0) 
term(1131) = term(1131) * (16.0d+0) 
term(1132) = term(1132) * (8.0d+0) 
term(1133) = term(1133) * (-16.0d+0) 
term(1134) = term(1134) * (8.0d+0) 
term(1135) = term(1135) * (-8.0d+0) 
term(1136) = term(1136) * (8.0d+0) 
term(1137) = term(1137) * (-8.0d+0) 
term(1138) = term(1138) * (-16.0d+0) 
term(1139) = term(1139) * (16.0d+0) 
term(1140) = term(1140) * (-8.0d+0) 
term(1141) = term(1141) * (8.0d+0) 
term(1142) = term(1142) * (-8.0d+0) 
term(1143) = term(1143) * (8.0d+0) 
term(1144) = term(1144) * (16.0d+0) 
term(1145) = term(1145) * (-16.0d+0) 
term(1147) = term(1147) * (-2.0d+0) 
term(1150) = term(1150) * (-2.0d+0) 
term(1152) = term(1152) * (-2.0d+0) 
term(1153) = term(1153) * (4.0d+0) 
term(1154) = term(1154) * (-2.0d+0) 
term(1155) = term(1155) * (-2.0d+0) 
term(1156) = term(1156) * (4.0d+0) 
term(1157) = term(1157) * (-2.0d+0) 
term(1158) = term(1158) * (2.0d+0) 
term(1159) = term(1159) * (-2.0d+0) 
term(1160) = term(1160) * (2.0d+0) 
term(1161) = term(1161) * (-2.0d+0) 
term(1162) = term(1162) * (-4.0d+0) 
term(1163) = term(1163) * (4.0d+0) 
term(1165) = term(1165) * (-2.0d+0) 
term(1168) = term(1168) * (-2.0d+0) 
term(1170) = term(1170) * (-2.0d+0) 
term(1171) = term(1171) * (4.0d+0) 
term(1172) = term(1172) * (-2.0d+0) 
term(1173) = term(1173) * (-2.0d+0) 
term(1174) = term(1174) * (4.0d+0) 
term(1175) = term(1175) * (-2.0d+0) 
term(1176) = term(1176) * (2.0d+0) 
term(1177) = term(1177) * (-2.0d+0) 
term(1178) = term(1178) * (2.0d+0) 
term(1179) = term(1179) * (-2.0d+0) 
term(1180) = term(1180) * (-4.0d+0) 
term(1181) = term(1181) * (4.0d+0) 
term(1182) = term(1182) * (4.0d+0) 
term(1183) = term(1183) * (-4.0d+0) 
term(1184) = term(1184) * (4.0d+0) 
term(1185) = term(1185) * (-4.0d+0) 
term(1186) = term(1186) * (-8.0d+0) 
term(1187) = term(1187) * (8.0d+0) 
term(1188) = term(1188) * (-8.0d+0) 
term(1189) = term(1189) * (8.0d+0) 
term(1190) = term(1190) * (8.0d+0) 
term(1191) = term(1191) * (-8.0d+0) 
term(1192) = term(1192) * (-8.0d+0) 
term(1193) = term(1193) * (8.0d+0) 
term(1194) = term(1194) * (4.0d+0) 
term(1195) = term(1195) * (-4.0d+0) 
term(1196) = term(1196) * (4.0d+0) 
term(1197) = term(1197) * (-4.0d+0) 
term(1198) = term(1198) * (-8.0d+0) 
term(1199) = term(1199) * (8.0d+0) 
term(1200) = term(1200) * (-8.0d+0) 
term(1201) = term(1201) * (8.0d+0) 
term(1202) = term(1202) * (8.0d+0) 
term(1203) = term(1203) * (-8.0d+0) 
term(1204) = term(1204) * (-8.0d+0) 
term(1205) = term(1205) * (8.0d+0) 

do i = 1, nocc 
term(1206) = term(1206) + wm_interm_1_triplet_pt2(q,i) * wm_interm_88_triplet_pt2(i,p)
term(1207) = term(1207) + wm_interm_2_triplet_pt2(q,i) * wm_interm_88_triplet_pt2(i,p)
term(1208) = term(1208) + wm_interm_1_triplet_pt2(i,q) * wm_interm_88_triplet_pt2(p,i)
term(1209) = term(1209) + wm_interm_2_triplet_pt2(i,q) * wm_interm_88_triplet_pt2(p,i)
term(1210) = term(1210) + wm_interm_1_triplet_pt2(q,i) * wm_interm_96_triplet_pt2(i,p)
term(1211) = term(1211) + wm_interm_1_triplet_pt2(q,i) * wm_interm_97_triplet_pt2(i,p)
term(1212) = term(1212) + wm_interm_2_triplet_pt2(q,i) * wm_interm_96_triplet_pt2(i,p)
term(1213) = term(1213) + wm_interm_2_triplet_pt2(q,i) * wm_interm_97_triplet_pt2(i,p)
term(1214) = term(1214) + wm_interm_1_triplet_pt2(q,i) * wm_interm_98_triplet_pt2(i,p)
term(1215) = term(1215) + wm_interm_2_triplet_pt2(q,i) * wm_interm_98_triplet_pt2(i,p)
term(1216) = term(1216) + wm_interm_111_triplet_pt2(i,p) * wm_interm_1_triplet_pt2(q,i)
term(1217) = term(1217) + wm_interm_111_triplet_pt2(i,p) * wm_interm_2_triplet_pt2(q,i)
term(1218) = term(1218) + wm_interm_112_triplet_pt2(i,p) * wm_interm_1_triplet_pt2(q,i)
term(1219) = term(1219) + wm_interm_112_triplet_pt2(i,p) * wm_interm_2_triplet_pt2(q,i)
term(1220) = term(1220) + wm_interm_113_triplet_pt2(i,p) * wm_interm_1_triplet_pt2(q,i)
term(1221) = term(1221) + wm_interm_113_triplet_pt2(i,p) * wm_interm_2_triplet_pt2(q,i)
term(1222) = term(1222) + wm_interm_30_triplet_pt2(p,i) * wm_interm_57_triplet_pt2(q,i)
term(1223) = term(1223) + wm_interm_31_triplet_pt2(p,i) * wm_interm_57_triplet_pt2(q,i)
term(1224) = term(1224) + wm_interm_32_triplet_pt2(p,i) * wm_interm_57_triplet_pt2(q,i)
term(1225) = term(1225) + wm_interm_30_triplet_pt2(p,i) * wm_interm_61_triplet_pt2(q,i)
term(1226) = term(1226) + wm_interm_31_triplet_pt2(p,i) * wm_interm_61_triplet_pt2(q,i)
term(1227) = term(1227) + wm_interm_32_triplet_pt2(p,i) * wm_interm_61_triplet_pt2(q,i)
term(1228) = term(1228) + wm_interm_30_triplet_pt2(p,i) * wm_interm_64_triplet_pt2(q,i)
term(1229) = term(1229) + wm_interm_31_triplet_pt2(p,i) * wm_interm_64_triplet_pt2(q,i)
term(1230) = term(1230) + wm_interm_32_triplet_pt2(p,i) * wm_interm_64_triplet_pt2(q,i)
term(1231) = term(1231) + wm_interm_123_triplet_pt2(i,p) * wm_interm_1_triplet_pt2(q,i)
term(1232) = term(1232) + wm_interm_124_triplet_pt2(i,p) * wm_interm_1_triplet_pt2(q,i)
term(1233) = term(1233) + wm_interm_123_triplet_pt2(i,p) * wm_interm_2_triplet_pt2(q,i)
term(1234) = term(1234) + wm_interm_124_triplet_pt2(i,p) * wm_interm_2_triplet_pt2(q,i)
term(1235) = term(1235) + wm_interm_133_triplet_pt2(i,p) * wm_interm_1_triplet_pt2(q,i)
term(1236) = term(1236) + wm_interm_133_triplet_pt2(i,p) * wm_interm_2_triplet_pt2(q,i)
term(1237) = term(1237) + wm_interm_134_triplet_pt2(i,p) * wm_interm_1_triplet_pt2(q,i)
term(1238) = term(1238) + wm_interm_134_triplet_pt2(i,p) * wm_interm_2_triplet_pt2(q,i)
term(1239) = term(1239) + wm_interm_41_triplet_pt2(p,i) * wm_interm_57_triplet_pt2(q,i)
term(1240) = term(1240) + wm_interm_42_triplet_pt2(p,i) * wm_interm_57_triplet_pt2(q,i)
term(1241) = term(1241) + wm_interm_43_triplet_pt2(p,i) * wm_interm_57_triplet_pt2(q,i)
term(1242) = term(1242) + wm_interm_41_triplet_pt2(p,i) * wm_interm_61_triplet_pt2(q,i)
term(1243) = term(1243) + wm_interm_42_triplet_pt2(p,i) * wm_interm_61_triplet_pt2(q,i)
term(1244) = term(1244) + wm_interm_43_triplet_pt2(p,i) * wm_interm_61_triplet_pt2(q,i)
term(1245) = term(1245) + wm_interm_41_triplet_pt2(p,i) * wm_interm_64_triplet_pt2(q,i)
term(1246) = term(1246) + wm_interm_42_triplet_pt2(p,i) * wm_interm_64_triplet_pt2(q,i)
term(1247) = term(1247) + wm_interm_43_triplet_pt2(p,i) * wm_interm_64_triplet_pt2(q,i)
term(1248) = term(1248) + wm_interm_111_triplet_pt2(p,i) * wm_interm_1_triplet_pt2(i,q)
term(1249) = term(1249) + wm_interm_112_triplet_pt2(p,i) * wm_interm_1_triplet_pt2(i,q)
term(1250) = term(1250) + wm_interm_113_triplet_pt2(p,i) * wm_interm_1_triplet_pt2(i,q)
term(1251) = term(1251) + wm_interm_1_triplet_pt2(i,q) * wm_interm_96_triplet_pt2(p,i)
term(1252) = term(1252) + wm_interm_1_triplet_pt2(i,q) * wm_interm_97_triplet_pt2(p,i)
term(1253) = term(1253) + wm_interm_1_triplet_pt2(i,q) * wm_interm_98_triplet_pt2(p,i)
term(1254) = term(1254) + wm_interm_111_triplet_pt2(p,i) * wm_interm_2_triplet_pt2(i,q)
term(1255) = term(1255) + wm_interm_112_triplet_pt2(p,i) * wm_interm_2_triplet_pt2(i,q)
term(1256) = term(1256) + wm_interm_113_triplet_pt2(p,i) * wm_interm_2_triplet_pt2(i,q)
term(1257) = term(1257) + wm_interm_2_triplet_pt2(i,q) * wm_interm_96_triplet_pt2(p,i)
term(1258) = term(1258) + wm_interm_2_triplet_pt2(i,q) * wm_interm_97_triplet_pt2(p,i)
term(1259) = term(1259) + wm_interm_2_triplet_pt2(i,q) * wm_interm_98_triplet_pt2(p,i)
term(1260) = term(1260) + wm_interm_30_triplet_pt2(i,p) * wm_interm_57_triplet_pt2(i,q)
term(1261) = term(1261) + wm_interm_30_triplet_pt2(i,p) * wm_interm_64_triplet_pt2(i,q)
term(1262) = term(1262) + wm_interm_30_triplet_pt2(i,p) * wm_interm_61_triplet_pt2(i,q)
term(1263) = term(1263) + wm_interm_31_triplet_pt2(i,p) * wm_interm_57_triplet_pt2(i,q)
term(1264) = term(1264) + wm_interm_31_triplet_pt2(i,p) * wm_interm_64_triplet_pt2(i,q)
term(1265) = term(1265) + wm_interm_31_triplet_pt2(i,p) * wm_interm_61_triplet_pt2(i,q)
term(1266) = term(1266) + wm_interm_32_triplet_pt2(i,p) * wm_interm_57_triplet_pt2(i,q)
term(1267) = term(1267) + wm_interm_32_triplet_pt2(i,p) * wm_interm_64_triplet_pt2(i,q)
term(1268) = term(1268) + wm_interm_32_triplet_pt2(i,p) * wm_interm_61_triplet_pt2(i,q)
term(1269) = term(1269) + wm_interm_133_triplet_pt2(p,i) * wm_interm_1_triplet_pt2(i,q)
term(1270) = term(1270) + wm_interm_134_triplet_pt2(p,i) * wm_interm_1_triplet_pt2(i,q)
term(1271) = term(1271) + wm_interm_123_triplet_pt2(p,i) * wm_interm_1_triplet_pt2(i,q)
term(1272) = term(1272) + wm_interm_124_triplet_pt2(p,i) * wm_interm_1_triplet_pt2(i,q)
term(1273) = term(1273) + wm_interm_133_triplet_pt2(p,i) * wm_interm_2_triplet_pt2(i,q)
term(1274) = term(1274) + wm_interm_134_triplet_pt2(p,i) * wm_interm_2_triplet_pt2(i,q)
term(1275) = term(1275) + wm_interm_123_triplet_pt2(p,i) * wm_interm_2_triplet_pt2(i,q)
term(1276) = term(1276) + wm_interm_124_triplet_pt2(p,i) * wm_interm_2_triplet_pt2(i,q)
term(1277) = term(1277) + wm_interm_41_triplet_pt2(i,p) * wm_interm_57_triplet_pt2(i,q)
term(1278) = term(1278) + wm_interm_41_triplet_pt2(i,p) * wm_interm_64_triplet_pt2(i,q)
term(1279) = term(1279) + wm_interm_41_triplet_pt2(i,p) * wm_interm_61_triplet_pt2(i,q)
term(1280) = term(1280) + wm_interm_42_triplet_pt2(i,p) * wm_interm_57_triplet_pt2(i,q)
term(1281) = term(1281) + wm_interm_42_triplet_pt2(i,p) * wm_interm_64_triplet_pt2(i,q)
term(1282) = term(1282) + wm_interm_42_triplet_pt2(i,p) * wm_interm_61_triplet_pt2(i,q)
term(1283) = term(1283) + wm_interm_43_triplet_pt2(i,p) * wm_interm_57_triplet_pt2(i,q)
term(1284) = term(1284) + wm_interm_43_triplet_pt2(i,p) * wm_interm_64_triplet_pt2(i,q)
term(1285) = term(1285) + wm_interm_43_triplet_pt2(i,p) * wm_interm_61_triplet_pt2(i,q)
term(1286) = term(1286) + wm_interm_146_triplet_pt2(i,p) * wm_interm_1_triplet_pt2(q,i)
term(1287) = term(1287) + wm_interm_147_triplet_pt2(i,p) * wm_interm_1_triplet_pt2(q,i)
term(1288) = term(1288) + wm_interm_146_triplet_pt2(i,p) * wm_interm_2_triplet_pt2(q,i)
term(1289) = term(1289) + wm_interm_147_triplet_pt2(i,p) * wm_interm_2_triplet_pt2(q,i)
term(1290) = term(1290) + wm_interm_148_triplet_pt2(i,p) * wm_interm_1_triplet_pt2(q,i)
term(1291) = term(1291) + wm_interm_148_triplet_pt2(i,p) * wm_interm_2_triplet_pt2(q,i)
term(1292) = term(1292) + wm_interm_159_triplet_pt2(i,p) * wm_interm_1_triplet_pt2(q,i)
term(1293) = term(1293) + wm_interm_159_triplet_pt2(i,p) * wm_interm_2_triplet_pt2(q,i)
term(1294) = term(1294) + wm_interm_160_triplet_pt2(i,p) * wm_interm_1_triplet_pt2(q,i)
term(1295) = term(1295) + wm_interm_160_triplet_pt2(i,p) * wm_interm_2_triplet_pt2(q,i)
term(1296) = term(1296) + wm_interm_161_triplet_pt2(i,p) * wm_interm_1_triplet_pt2(q,i)
term(1297) = term(1297) + wm_interm_161_triplet_pt2(i,p) * wm_interm_2_triplet_pt2(q,i)
term(1298) = term(1298) + wm_interm_30_triplet_pt2(p,i) * wm_interm_74_triplet_pt2(q,i)
term(1299) = term(1299) + wm_interm_31_triplet_pt2(p,i) * wm_interm_74_triplet_pt2(q,i)
term(1300) = term(1300) + wm_interm_32_triplet_pt2(p,i) * wm_interm_74_triplet_pt2(q,i)
term(1301) = term(1301) + wm_interm_30_triplet_pt2(p,i) * wm_interm_77_triplet_pt2(q,i)
term(1302) = term(1302) + wm_interm_31_triplet_pt2(p,i) * wm_interm_77_triplet_pt2(q,i)
term(1303) = term(1303) + wm_interm_32_triplet_pt2(p,i) * wm_interm_77_triplet_pt2(q,i)
term(1304) = term(1304) + wm_interm_167_triplet_pt2(i,p) * wm_interm_1_triplet_pt2(q,i)
term(1305) = term(1305) + wm_interm_168_triplet_pt2(i,p) * wm_interm_1_triplet_pt2(q,i)
term(1306) = term(1306) + wm_interm_167_triplet_pt2(i,p) * wm_interm_2_triplet_pt2(q,i)
term(1307) = term(1307) + wm_interm_168_triplet_pt2(i,p) * wm_interm_2_triplet_pt2(q,i)
term(1308) = term(1308) + wm_interm_175_triplet_pt2(i,p) * wm_interm_1_triplet_pt2(q,i)
term(1309) = term(1309) + wm_interm_175_triplet_pt2(i,p) * wm_interm_2_triplet_pt2(q,i)
term(1310) = term(1310) + wm_interm_176_triplet_pt2(i,p) * wm_interm_1_triplet_pt2(q,i)
term(1311) = term(1311) + wm_interm_176_triplet_pt2(i,p) * wm_interm_2_triplet_pt2(q,i)
term(1312) = term(1312) + wm_interm_41_triplet_pt2(p,i) * wm_interm_74_triplet_pt2(q,i)
term(1313) = term(1313) + wm_interm_42_triplet_pt2(p,i) * wm_interm_74_triplet_pt2(q,i)
term(1314) = term(1314) + wm_interm_43_triplet_pt2(p,i) * wm_interm_74_triplet_pt2(q,i)
term(1315) = term(1315) + wm_interm_41_triplet_pt2(p,i) * wm_interm_77_triplet_pt2(q,i)
term(1316) = term(1316) + wm_interm_42_triplet_pt2(p,i) * wm_interm_77_triplet_pt2(q,i)
term(1317) = term(1317) + wm_interm_43_triplet_pt2(p,i) * wm_interm_77_triplet_pt2(q,i)
term(1318) = term(1318) + wm_interm_159_triplet_pt2(p,i) * wm_interm_1_triplet_pt2(i,q)
term(1319) = term(1319) + wm_interm_160_triplet_pt2(p,i) * wm_interm_1_triplet_pt2(i,q)
term(1320) = term(1320) + wm_interm_161_triplet_pt2(p,i) * wm_interm_1_triplet_pt2(i,q)
term(1321) = term(1321) + wm_interm_146_triplet_pt2(p,i) * wm_interm_1_triplet_pt2(i,q)
term(1322) = term(1322) + wm_interm_147_triplet_pt2(p,i) * wm_interm_1_triplet_pt2(i,q)
term(1323) = term(1323) + wm_interm_148_triplet_pt2(p,i) * wm_interm_1_triplet_pt2(i,q)
term(1324) = term(1324) + wm_interm_159_triplet_pt2(p,i) * wm_interm_2_triplet_pt2(i,q)
term(1325) = term(1325) + wm_interm_160_triplet_pt2(p,i) * wm_interm_2_triplet_pt2(i,q)
term(1326) = term(1326) + wm_interm_161_triplet_pt2(p,i) * wm_interm_2_triplet_pt2(i,q)
term(1327) = term(1327) + wm_interm_146_triplet_pt2(p,i) * wm_interm_2_triplet_pt2(i,q)
term(1328) = term(1328) + wm_interm_147_triplet_pt2(p,i) * wm_interm_2_triplet_pt2(i,q)
term(1329) = term(1329) + wm_interm_148_triplet_pt2(p,i) * wm_interm_2_triplet_pt2(i,q)
term(1330) = term(1330) + wm_interm_30_triplet_pt2(i,p) * wm_interm_74_triplet_pt2(i,q)
term(1331) = term(1331) + wm_interm_30_triplet_pt2(i,p) * wm_interm_77_triplet_pt2(i,q)
term(1332) = term(1332) + wm_interm_31_triplet_pt2(i,p) * wm_interm_74_triplet_pt2(i,q)
term(1333) = term(1333) + wm_interm_31_triplet_pt2(i,p) * wm_interm_77_triplet_pt2(i,q)
term(1334) = term(1334) + wm_interm_32_triplet_pt2(i,p) * wm_interm_74_triplet_pt2(i,q)
term(1335) = term(1335) + wm_interm_32_triplet_pt2(i,p) * wm_interm_77_triplet_pt2(i,q)
term(1336) = term(1336) + wm_interm_175_triplet_pt2(p,i) * wm_interm_1_triplet_pt2(i,q)
term(1337) = term(1337) + wm_interm_176_triplet_pt2(p,i) * wm_interm_1_triplet_pt2(i,q)
term(1338) = term(1338) + wm_interm_167_triplet_pt2(p,i) * wm_interm_1_triplet_pt2(i,q)
term(1339) = term(1339) + wm_interm_168_triplet_pt2(p,i) * wm_interm_1_triplet_pt2(i,q)
term(1340) = term(1340) + wm_interm_175_triplet_pt2(p,i) * wm_interm_2_triplet_pt2(i,q)
term(1341) = term(1341) + wm_interm_176_triplet_pt2(p,i) * wm_interm_2_triplet_pt2(i,q)
term(1342) = term(1342) + wm_interm_167_triplet_pt2(p,i) * wm_interm_2_triplet_pt2(i,q)
term(1343) = term(1343) + wm_interm_168_triplet_pt2(p,i) * wm_interm_2_triplet_pt2(i,q)
term(1344) = term(1344) + wm_interm_41_triplet_pt2(i,p) * wm_interm_74_triplet_pt2(i,q)
term(1345) = term(1345) + wm_interm_41_triplet_pt2(i,p) * wm_interm_77_triplet_pt2(i,q)
term(1346) = term(1346) + wm_interm_42_triplet_pt2(i,p) * wm_interm_74_triplet_pt2(i,q)
term(1347) = term(1347) + wm_interm_42_triplet_pt2(i,p) * wm_interm_77_triplet_pt2(i,q)
term(1348) = term(1348) + wm_interm_43_triplet_pt2(i,p) * wm_interm_74_triplet_pt2(i,q)
term(1349) = term(1349) + wm_interm_43_triplet_pt2(i,p) * wm_interm_77_triplet_pt2(i,q)
end do 

term(1206) = term(1206) * (-2.0d+0) 
term(1207) = term(1207) * (4.0d+0) 
term(1208) = term(1208) * (-2.0d+0) 
term(1209) = term(1209) * (4.0d+0) 
term(1210) = term(1210) * (0.5d+0) 
term(1211) = term(1211) * (-1.0d+0) 
term(1212) = term(1212) * (-1.0d+0) 
term(1213) = term(1213) * (2.0d+0) 
term(1214) = term(1214) * (0.5d+0) 
term(1215) = term(1215) * (-1.0d+0) 
term(1216) = term(1216) * (0.5d+0) 
term(1217) = term(1217) * (-1.0d+0) 
term(1218) = term(1218) * (-1.0d+0) 
term(1219) = term(1219) * (2.0d+0) 
term(1220) = term(1220) * (0.5d+0) 
term(1221) = term(1221) * (-1.0d+0) 
term(1222) = term(1222) * (0.5d+0) 
term(1223) = term(1223) * (-1.0d+0) 
term(1224) = term(1224) * (0.5d+0) 
term(1225) = term(1225) * (-1.0d+0) 
term(1226) = term(1226) * (2.0d+0) 
term(1227) = term(1227) * (-1.0d+0) 
term(1228) = term(1228) * (0.5d+0) 
term(1229) = term(1229) * (-1.0d+0) 
term(1230) = term(1230) * (0.5d+0) 
term(1231) = term(1231) * (2.0d+0) 
term(1232) = term(1232) * (-2.0d+0) 
term(1233) = term(1233) * (-4.0d+0) 
term(1234) = term(1234) * (4.0d+0) 
term(1235) = term(1235) * (2.0d+0) 
term(1236) = term(1236) * (-4.0d+0) 
term(1237) = term(1237) * (-2.0d+0) 
term(1238) = term(1238) * (4.0d+0) 
term(1240) = term(1240) * (-2.0d+0) 
term(1242) = term(1242) * (-2.0d+0) 
term(1243) = term(1243) * (4.0d+0) 
term(1244) = term(1244) * (-2.0d+0) 
term(1246) = term(1246) * (-2.0d+0) 
term(1248) = term(1248) * (0.5d+0) 
term(1249) = term(1249) * (-1.0d+0) 
term(1250) = term(1250) * (0.5d+0) 
term(1251) = term(1251) * (0.5d+0) 
term(1252) = term(1252) * (-1.0d+0) 
term(1253) = term(1253) * (0.5d+0) 
term(1254) = term(1254) * (-1.0d+0) 
term(1255) = term(1255) * (2.0d+0) 
term(1256) = term(1256) * (-1.0d+0) 
term(1257) = term(1257) * (-1.0d+0) 
term(1258) = term(1258) * (2.0d+0) 
term(1259) = term(1259) * (-1.0d+0) 
term(1260) = term(1260) * (0.5d+0) 
term(1261) = term(1261) * (0.5d+0) 
term(1262) = term(1262) * (-1.0d+0) 
term(1263) = term(1263) * (-1.0d+0) 
term(1264) = term(1264) * (-1.0d+0) 
term(1265) = term(1265) * (2.0d+0) 
term(1266) = term(1266) * (0.5d+0) 
term(1267) = term(1267) * (0.5d+0) 
term(1268) = term(1268) * (-1.0d+0) 
term(1269) = term(1269) * (2.0d+0) 
term(1270) = term(1270) * (-2.0d+0) 
term(1271) = term(1271) * (2.0d+0) 
term(1272) = term(1272) * (-2.0d+0) 
term(1273) = term(1273) * (-4.0d+0) 
term(1274) = term(1274) * (4.0d+0) 
term(1275) = term(1275) * (-4.0d+0) 
term(1276) = term(1276) * (4.0d+0) 
term(1279) = term(1279) * (-2.0d+0) 
term(1280) = term(1280) * (-2.0d+0) 
term(1281) = term(1281) * (-2.0d+0) 
term(1282) = term(1282) * (4.0d+0) 
term(1285) = term(1285) * (-2.0d+0) 
term(1287) = term(1287) * (-2.0d+0) 
term(1288) = term(1288) * (-2.0d+0) 
term(1289) = term(1289) * (4.0d+0) 
term(1291) = term(1291) * (-2.0d+0) 
term(1293) = term(1293) * (-2.0d+0) 
term(1294) = term(1294) * (-2.0d+0) 
term(1295) = term(1295) * (4.0d+0) 
term(1297) = term(1297) * (-2.0d+0) 
term(1298) = term(1298) * (2.0d+0) 
term(1299) = term(1299) * (-4.0d+0) 
term(1300) = term(1300) * (2.0d+0) 
term(1301) = term(1301) * (-2.0d+0) 
term(1302) = term(1302) * (4.0d+0) 
term(1303) = term(1303) * (-2.0d+0) 
term(1304) = term(1304) * (4.0d+0) 
term(1305) = term(1305) * (-4.0d+0) 
term(1306) = term(1306) * (-8.0d+0) 
term(1307) = term(1307) * (8.0d+0) 
term(1308) = term(1308) * (4.0d+0) 
term(1309) = term(1309) * (-8.0d+0) 
term(1310) = term(1310) * (-4.0d+0) 
term(1311) = term(1311) * (8.0d+0) 
term(1312) = term(1312) * (4.0d+0) 
term(1313) = term(1313) * (-8.0d+0) 
term(1314) = term(1314) * (4.0d+0) 
term(1315) = term(1315) * (-4.0d+0) 
term(1316) = term(1316) * (8.0d+0) 
term(1317) = term(1317) * (-4.0d+0) 
term(1319) = term(1319) * (-2.0d+0) 
term(1322) = term(1322) * (-2.0d+0) 
term(1324) = term(1324) * (-2.0d+0) 
term(1325) = term(1325) * (4.0d+0) 
term(1326) = term(1326) * (-2.0d+0) 
term(1327) = term(1327) * (-2.0d+0) 
term(1328) = term(1328) * (4.0d+0) 
term(1329) = term(1329) * (-2.0d+0) 
term(1330) = term(1330) * (2.0d+0) 
term(1331) = term(1331) * (-2.0d+0) 
term(1332) = term(1332) * (-4.0d+0) 
term(1333) = term(1333) * (4.0d+0) 
term(1334) = term(1334) * (2.0d+0) 
term(1335) = term(1335) * (-2.0d+0) 
term(1336) = term(1336) * (4.0d+0) 
term(1337) = term(1337) * (-4.0d+0) 
term(1338) = term(1338) * (4.0d+0) 
term(1339) = term(1339) * (-4.0d+0) 
term(1340) = term(1340) * (-8.0d+0) 
term(1341) = term(1341) * (8.0d+0) 
term(1342) = term(1342) * (-8.0d+0) 
term(1343) = term(1343) * (8.0d+0) 
term(1344) = term(1344) * (4.0d+0) 
term(1345) = term(1345) * (-4.0d+0) 
term(1346) = term(1346) * (-8.0d+0) 
term(1347) = term(1347) * (8.0d+0) 
term(1348) = term(1348) * (4.0d+0) 
term(1349) = term(1349) * (-4.0d+0) 

do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
term(1350) = term(1350) + wm_interm_122_triplet_pt2(i,j,k,p) * wm_interm_16_triplet_pt2(k,q,i,j)
term(1351) = term(1351) + wm_interm_122_triplet_pt2(i,j,k,p) * wm_interm_16_triplet_pt2(q,k,i,j)
term(1352) = term(1352) + wm_interm_16_triplet_pt2(i,j,k,q) * wm_interm_94_triplet_pt2(k,p,i,j)
term(1353) = term(1353) + wm_interm_16_triplet_pt2(i,j,k,q) * wm_interm_95_triplet_pt2(k,p,i,j)
term(1354) = term(1354) + wm_interm_16_triplet_pt2(i,j,k,q) * wm_interm_94_triplet_pt2(p,k,i,j)
term(1355) = term(1355) + wm_interm_144_triplet_pt2(i,j,k,p) * wm_interm_16_triplet_pt2(k,q,i,j)
term(1356) = term(1356) + wm_interm_144_triplet_pt2(i,j,k,p) * wm_interm_16_triplet_pt2(q,k,i,j)
term(1357) = term(1357) + wm_interm_145_triplet_pt2(i,j,k,p) * wm_interm_16_triplet_pt2(k,q,i,j)
term(1358) = term(1358) + wm_interm_166_triplet_pt2(i,j,k,p) * wm_interm_16_triplet_pt2(k,q,i,j)
term(1359) = term(1359) + wm_interm_166_triplet_pt2(i,j,k,p) * wm_interm_16_triplet_pt2(q,k,i,j)
end do 
end do 
end do 

term(1350) = term(1350) * (-2.0d+0) 
term(1351) = term(1351) * (2.0d+0) 
term(1352) = term(1352) * (-1.0d+0) 
term(1353) = term(1353) * (0.5d+0) 
term(1354) = term(1354) * (0.5d+0) 
term(1355) = term(1355) * (-2.0d+0) 
term(1358) = term(1358) * (-4.0d+0) 
term(1359) = term(1359) * (4.0d+0) 

do l = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
term(1360) = term(1360) + wm_interm_16_triplet_pt2(i,j,k,l) * wm_interm_94_triplet_pt2(k,l,j,i)
end do 
end do 
end do 
end do 

term(1360) = term(1360) * (-1.0d+0) 

do l = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
term(1361) = term(1361) + wm_interm_16_triplet_pt2(i,j,k,l) * wm_interm_94_triplet_pt2(k,l,i,j)
term(1362) = term(1362) + wm_interm_16_triplet_pt2(i,j,k,l) * wm_interm_95_triplet_pt2(k,l,i,j)
term(1363) = term(1363) + wm_interm_122_triplet_pt2(i,j,k,l) * wm_interm_16_triplet_pt2(l,k,i,j)
term(1364) = term(1364) + wm_interm_122_triplet_pt2(i,j,k,l) * wm_interm_16_triplet_pt2(k,l,i,j)
term(1365) = term(1365) + wm_interm_144_triplet_pt2(i,j,k,l) * wm_interm_16_triplet_pt2(l,k,i,j)
term(1366) = term(1366) + wm_interm_144_triplet_pt2(i,j,k,l) * wm_interm_16_triplet_pt2(k,l,i,j)
term(1367) = term(1367) + wm_interm_145_triplet_pt2(i,j,k,l) * wm_interm_16_triplet_pt2(k,l,i,j)
term(1368) = term(1368) + wm_interm_166_triplet_pt2(i,j,k,l) * wm_interm_16_triplet_pt2(l,k,i,j)
term(1369) = term(1369) + wm_interm_166_triplet_pt2(i,j,k,l) * wm_interm_16_triplet_pt2(k,l,i,j)
end do 
end do 
end do 
end do 

term(1361) = term(1361) * (2.0d+0) 
term(1362) = term(1362) * (-1.0d+0) 
term(1363) = term(1363) * (-4.0d+0) 
term(1364) = term(1364) * (4.0d+0) 
term(1365) = term(1365) * (-2.0d+0) 
term(1366) = term(1366) * (4.0d+0) 
term(1367) = term(1367) * (-2.0d+0) 
term(1368) = term(1368) * (-8.0d+0) 
term(1369) = term(1369) * (8.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(1370) = term(1370) + wm_interm_29_triplet_pt2(i,j,k,l) * wm_interm_58_triplet_pt2(i,j,l,k)
term(1371) = term(1371) + wm_interm_29_triplet_pt2(i,j,k,l) * wm_interm_58_triplet_pt2(j,i,k,l)
term(1372) = term(1372) + wm_interm_29_triplet_pt2(i,j,k,l) * wm_interm_58_triplet_pt2(i,j,k,l)
term(1373) = term(1373) + wm_interm_40_triplet_pt2(i,j,k,l) * wm_interm_58_triplet_pt2(i,j,l,k)
term(1374) = term(1374) + wm_interm_40_triplet_pt2(i,j,k,l) * wm_interm_58_triplet_pt2(j,i,k,l)
term(1375) = term(1375) + wm_interm_40_triplet_pt2(i,j,k,l) * wm_interm_58_triplet_pt2(i,j,k,l)
term(1376) = term(1376) + wm_interm_29_triplet_pt2(i,j,k,l) * wm_interm_75_triplet_pt2(i,j,l,k)
term(1377) = term(1377) + wm_interm_29_triplet_pt2(i,j,k,l) * wm_interm_75_triplet_pt2(j,i,k,l)
term(1378) = term(1378) + wm_interm_29_triplet_pt2(i,j,k,l) * wm_interm_75_triplet_pt2(i,j,k,l)
term(1379) = term(1379) + wm_interm_40_triplet_pt2(i,j,k,l) * wm_interm_75_triplet_pt2(i,j,l,k)
term(1380) = term(1380) + wm_interm_40_triplet_pt2(i,j,k,l) * wm_interm_75_triplet_pt2(j,i,k,l)
term(1381) = term(1381) + wm_interm_40_triplet_pt2(i,j,k,l) * wm_interm_75_triplet_pt2(i,j,k,l)
end do 
end do 
end do 
end do 

term(1370) = term(1370) * (-1.0d+0) 
term(1371) = term(1371) * (-1.0d+0) 
term(1372) = term(1372) * (2.0d+0) 
term(1373) = term(1373) * (-2.0d+0) 
term(1374) = term(1374) * (-2.0d+0) 
term(1375) = term(1375) * (4.0d+0) 
term(1376) = term(1376) * (-2.0d+0) 
term(1377) = term(1377) * (-2.0d+0) 
term(1378) = term(1378) * (4.0d+0) 
term(1379) = term(1379) * (-4.0d+0) 
term(1380) = term(1380) * (-4.0d+0) 
term(1381) = term(1381) * (8.0d+0) 


    calc_D_oo_wm_triplet_pt2 = zero
    do s = 0, 1381
    calc_D_oo_wm_triplet_pt2 = calc_D_oo_wm_triplet_pt2 + term(s)
    end do

    end function calc_D_oo_wm_triplet_pt2
    
    function calc_D_ov_wm_triplet_pt2(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_ov_wm_triplet_pt2
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
    real(F64), dimension(0:124) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + wm_interm_0_triplet_pt2(a,i,p,j) * wm_interm_25_triplet_pt2(a,q,j,i)
term(1) = term(1) + wm_interm_25_triplet_pt2(a,q,i,j) * wm_interm_3_triplet_pt2(a,j,i,p)
term(2) = term(2) + wm_interm_25_triplet_pt2(a,q,i,j) * wm_interm_3_triplet_pt2(a,j,p,i)
term(3) = term(3) + wm_interm_26_triplet_pt2(a,q,i,j) * wm_interm_3_triplet_pt2(a,j,p,i)
term(4) = term(4) + wm_interm_26_triplet_pt2(a,q,i,j) * wm_interm_3_triplet_pt2(a,j,i,p)
term(5) = term(5) + wm_interm_0_triplet_pt2(a,i,p,j) * wm_interm_27_triplet_pt2(a,q,j,i)
term(6) = term(6) + wm_interm_0_triplet_pt2(a,i,p,j) * wm_interm_28_triplet_pt2(a,q,j,i)
term(7) = term(7) + wm_interm_27_triplet_pt2(a,q,i,j) * wm_interm_3_triplet_pt2(a,j,i,p)
term(8) = term(8) + wm_interm_28_triplet_pt2(a,q,i,j) * wm_interm_3_triplet_pt2(a,j,i,p)
term(9) = term(9) + wm_interm_27_triplet_pt2(a,q,i,j) * wm_interm_3_triplet_pt2(a,j,p,i)
term(10) = term(10) + wm_interm_28_triplet_pt2(a,q,i,j) * wm_interm_3_triplet_pt2(a,j,p,i)
term(11) = term(11) + wm_interm_10_triplet_pt2(a,i,p,j) * wm_interm_25_triplet_pt2(a,q,j,i)
term(12) = term(12) + wm_interm_10_triplet_pt2(a,i,p,j) * wm_interm_26_triplet_pt2(a,q,j,i)
term(13) = term(13) + wm_interm_10_triplet_pt2(a,i,p,j) * wm_interm_27_triplet_pt2(a,q,j,i)
term(14) = term(14) + wm_interm_10_triplet_pt2(a,i,p,j) * wm_interm_28_triplet_pt2(a,q,j,i)
term(15) = term(15) + wm_interm_47_triplet_pt2(q,a,i,j) * wm_interm_48_triplet_pt2(a,i,j,p)
term(16) = term(16) + wm_interm_47_triplet_pt2(q,a,i,j) * wm_interm_48_triplet_pt2(a,i,p,j)
term(17) = term(17) + wm_interm_48_triplet_pt2(a,i,j,p) * wm_interm_50_triplet_pt2(q,a,i,j)
term(18) = term(18) + wm_interm_48_triplet_pt2(a,i,p,j) * wm_interm_50_triplet_pt2(q,a,i,j)
term(19) = term(19) + wm_interm_48_triplet_pt2(a,i,j,p) * wm_interm_52_triplet_pt2(q,a,i,j)
term(20) = term(20) + wm_interm_48_triplet_pt2(a,i,p,j) * wm_interm_52_triplet_pt2(q,a,i,j)
term(21) = term(21) + wm_interm_48_triplet_pt2(a,i,p,j) * wm_interm_53_triplet_pt2(q,a,i,j)
term(22) = term(22) + wm_interm_48_triplet_pt2(a,i,j,p) * wm_interm_53_triplet_pt2(q,a,i,j)
term(23) = term(23) + wm_interm_48_triplet_pt2(a,i,j,p) * wm_interm_56_triplet_pt2(q,a,i,j)
term(24) = term(24) + wm_interm_48_triplet_pt2(a,i,p,j) * wm_interm_56_triplet_pt2(q,a,i,j)
term(25) = term(25) + wm_interm_48_triplet_pt2(a,i,p,j) * wm_interm_59_triplet_pt2(q,a,i,j)
term(26) = term(26) + wm_interm_48_triplet_pt2(a,i,j,p) * wm_interm_63_triplet_pt2(q,a,i,j)
term(27) = term(27) + wm_interm_48_triplet_pt2(a,i,j,p) * wm_interm_68_triplet_pt2(q,a,i,j)
term(28) = term(28) + wm_interm_48_triplet_pt2(a,i,p,j) * wm_interm_68_triplet_pt2(q,a,i,j)
term(29) = term(29) + wm_interm_48_triplet_pt2(a,i,j,p) * wm_interm_69_triplet_pt2(q,a,i,j)
term(30) = term(30) + wm_interm_48_triplet_pt2(a,i,p,j) * wm_interm_69_triplet_pt2(q,a,i,j)
term(31) = term(31) + wm_interm_48_triplet_pt2(a,i,p,j) * wm_interm_71_triplet_pt2(q,a,i,j)
term(32) = term(32) + wm_interm_48_triplet_pt2(a,i,j,p) * wm_interm_71_triplet_pt2(q,a,i,j)
term(33) = term(33) + wm_interm_48_triplet_pt2(a,i,j,p) * wm_interm_73_triplet_pt2(q,a,i,j)
term(34) = term(34) + wm_interm_48_triplet_pt2(a,i,p,j) * wm_interm_73_triplet_pt2(q,a,i,j)
end do 
end do 
end do 

term(0) = term(0) * (-1.0d+0) 
term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (2.0d+0) 
term(3) = term(3) * (-1.0d+0) 
term(4) = term(4) * (2.0d+0) 
term(5) = term(5) * (-1.0d+0) 
term(6) = term(6) * (2.0d+0) 
term(7) = term(7) * (-1.0d+0) 
term(8) = term(8) * (2.0d+0) 
term(9) = term(9) * (2.0d+0) 
term(10) = term(10) * (-4.0d+0) 
term(11) = term(11) * (4.0d+0) 
term(12) = term(12) * (-4.0d+0) 
term(13) = term(13) * (4.0d+0) 
term(14) = term(14) * (-8.0d+0) 
term(15) = term(15) * (2.0d+0) 
term(16) = term(16) * (-4.0d+0) 
term(17) = term(17) * (-1.0d+0) 
term(18) = term(18) * (2.0d+0) 
term(19) = term(19) * (-1.0d+0) 
term(20) = term(20) * (2.0d+0) 
term(21) = term(21) * (-1.0d+0) 
term(22) = term(22) * (2.0d+0) 
term(23) = term(23) * (-1.0d+0) 
term(24) = term(24) * (2.0d+0) 
term(25) = term(25) * (-1.0d+0) 
term(26) = term(26) * (-1.0d+0) 
term(27) = term(27) * (4.0d+0) 
term(28) = term(28) * (-8.0d+0) 
term(29) = term(29) * (-4.0d+0) 
term(30) = term(30) * (8.0d+0) 
term(31) = term(31) * (-4.0d+0) 
term(32) = term(32) * (4.0d+0) 
term(33) = term(33) * (-4.0d+0) 
term(34) = term(34) * (4.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(35) = term(35) + wm_interm_16_triplet_pt2(i,p,j,k) * wm_interm_51_triplet_pt2(q,j,k,i)
term(36) = term(36) + wm_interm_16_triplet_pt2(p,i,j,k) * wm_interm_45_triplet_pt2(q,j,k,i)
term(37) = term(37) + wm_interm_16_triplet_pt2(i,p,j,k) * wm_interm_45_triplet_pt2(q,j,k,i)
term(38) = term(38) + wm_interm_16_triplet_pt2(i,p,j,k) * wm_interm_70_triplet_pt2(q,j,k,i)
term(39) = term(39) + wm_interm_16_triplet_pt2(p,i,j,k) * wm_interm_66_triplet_pt2(q,j,k,i)
term(40) = term(40) + wm_interm_16_triplet_pt2(i,p,j,k) * wm_interm_66_triplet_pt2(q,j,k,i)
end do 
end do 
end do 

term(35) = term(35) * (-1.0d+0) 
term(36) = term(36) * (-1.0d+0) 
term(37) = term(37) * (2.0d+0) 
term(38) = term(38) * (-2.0d+0) 
term(39) = term(39) * (-2.0d+0) 
term(40) = term(40) * (4.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(41) = term(41) + wm_interm_0_triplet_pt2(a,i,j,p) * wm_interm_26_triplet_pt2(a,q,j,i)
term(42) = term(42) + wm_interm_10_triplet_pt2(a,i,j,p) * wm_interm_25_triplet_pt2(a,q,j,i)
term(43) = term(43) + wm_interm_10_triplet_pt2(a,i,j,p) * wm_interm_26_triplet_pt2(a,q,j,i)
term(44) = term(44) + wm_interm_10_triplet_pt2(a,i,j,p) * wm_interm_27_triplet_pt2(a,q,j,i)
term(45) = term(45) + wm_interm_10_triplet_pt2(a,i,j,p) * wm_interm_28_triplet_pt2(a,q,j,i)
end do 
end do 
end do 

term(41) = term(41) * (-1.0d+0) 
term(42) = term(42) * (-4.0d+0) 
term(43) = term(43) * (4.0d+0) 
term(44) = term(44) * (-4.0d+0) 
term(45) = term(45) * (8.0d+0) 

do a = nocc + 1, nactive 
term(46) = term(46) + wm_interm_13_triplet_pt2(a,p) * wm_interm_4_triplet_pt2(a,q)
term(47) = term(47) + wm_interm_21_triplet_pt2(a,p) * wm_interm_4_triplet_pt2(a,q)
term(48) = term(48) + wm_interm_22_triplet_pt2(a,p) * wm_interm_4_triplet_pt2(a,q)
term(49) = term(49) + wm_interm_13_triplet_pt2(a,p) * wm_interm_5_triplet_pt2(a,q)
term(50) = term(50) + wm_interm_21_triplet_pt2(a,p) * wm_interm_5_triplet_pt2(a,q)
term(51) = term(51) + wm_interm_22_triplet_pt2(a,p) * wm_interm_5_triplet_pt2(a,q)
term(52) = term(52) + wm_interm_33_triplet_pt2(a,p) * wm_interm_4_triplet_pt2(a,q)
term(53) = term(53) + wm_interm_38_triplet_pt2(a,p) * wm_interm_4_triplet_pt2(a,q)
term(54) = term(54) + wm_interm_33_triplet_pt2(a,p) * wm_interm_5_triplet_pt2(a,q)
term(55) = term(55) + wm_interm_38_triplet_pt2(a,p) * wm_interm_5_triplet_pt2(a,q)
term(56) = term(56) + wm_interm_54_triplet_pt2(a,p) * wm_interm_55_triplet_pt2(q,a)
term(57) = term(57) + wm_interm_54_triplet_pt2(a,p) * wm_interm_60_triplet_pt2(q,a)
term(58) = term(58) + wm_interm_54_triplet_pt2(a,p) * wm_interm_62_triplet_pt2(q,a)
term(59) = term(59) + wm_interm_54_triplet_pt2(a,p) * wm_interm_72_triplet_pt2(q,a)
term(60) = term(60) + wm_interm_54_triplet_pt2(a,p) * wm_interm_76_triplet_pt2(q,a)
end do 

term(46) = term(46) * (-1.0d+0) 
term(47) = term(47) * (-1.0d+0) 
term(48) = term(48) * (2.0d+0) 
term(49) = term(49) * (2.0d+0) 
term(50) = term(50) * (2.0d+0) 
term(51) = term(51) * (-4.0d+0) 
term(52) = term(52) * (-4.0d+0) 
term(53) = term(53) * (4.0d+0) 
term(54) = term(54) * (8.0d+0) 
term(55) = term(55) * (-8.0d+0) 
term(56) = term(56) * (-1.0d+0) 
term(57) = term(57) * (2.0d+0) 
term(58) = term(58) * (-1.0d+0) 
term(59) = term(59) * (-4.0d+0) 
term(60) = term(60) * (4.0d+0) 

do i = 1, nocc 
term(61) = term(61) + wm_interm_30_triplet_pt2(p,i) * wm_interm_6_triplet_pt2(q,i)
term(62) = term(62) + wm_interm_31_triplet_pt2(p,i) * wm_interm_6_triplet_pt2(q,i)
term(63) = term(63) + wm_interm_32_triplet_pt2(p,i) * wm_interm_6_triplet_pt2(q,i)
term(64) = term(64) + wm_interm_41_triplet_pt2(p,i) * wm_interm_6_triplet_pt2(q,i)
term(65) = term(65) + wm_interm_42_triplet_pt2(p,i) * wm_interm_6_triplet_pt2(q,i)
term(66) = term(66) + wm_interm_43_triplet_pt2(p,i) * wm_interm_6_triplet_pt2(q,i)
term(67) = term(67) + wm_interm_1_triplet_pt2(p,i) * wm_interm_44_triplet_pt2(q,i)
term(68) = term(68) + wm_interm_2_triplet_pt2(p,i) * wm_interm_44_triplet_pt2(q,i)
term(69) = term(69) + wm_interm_1_triplet_pt2(p,i) * wm_interm_46_triplet_pt2(q,i)
term(70) = term(70) + wm_interm_2_triplet_pt2(p,i) * wm_interm_46_triplet_pt2(q,i)
term(71) = term(71) + wm_interm_1_triplet_pt2(p,i) * wm_interm_49_triplet_pt2(q,i)
term(72) = term(72) + wm_interm_2_triplet_pt2(p,i) * wm_interm_49_triplet_pt2(q,i)
term(73) = term(73) + wm_interm_1_triplet_pt2(p,i) * wm_interm_65_triplet_pt2(q,i)
term(74) = term(74) + wm_interm_2_triplet_pt2(p,i) * wm_interm_65_triplet_pt2(q,i)
term(75) = term(75) + wm_interm_1_triplet_pt2(p,i) * wm_interm_67_triplet_pt2(q,i)
term(76) = term(76) + wm_interm_2_triplet_pt2(p,i) * wm_interm_67_triplet_pt2(q,i)
term(77) = term(77) + s1(q,i) * wm_interm_88_triplet_pt2(p,i)
term(78) = term(78) + t1(q,i) * wm_interm_88_triplet_pt2(i,p)
term(79) = term(79) + s1(q,i) * wm_interm_111_triplet_pt2(p,i)
term(80) = term(80) + s1(q,i) * wm_interm_112_triplet_pt2(p,i)
term(81) = term(81) + s1(q,i) * wm_interm_113_triplet_pt2(p,i)
term(82) = term(82) + s1(q,i) * wm_interm_96_triplet_pt2(p,i)
term(83) = term(83) + s1(q,i) * wm_interm_97_triplet_pt2(p,i)
term(84) = term(84) + s1(q,i) * wm_interm_98_triplet_pt2(p,i)
term(85) = term(85) + s1(q,i) * wm_interm_133_triplet_pt2(p,i)
term(86) = term(86) + s1(q,i) * wm_interm_134_triplet_pt2(p,i)
term(87) = term(87) + s1(q,i) * wm_interm_123_triplet_pt2(p,i)
term(88) = term(88) + s1(q,i) * wm_interm_124_triplet_pt2(p,i)
term(89) = term(89) + t1(q,i) * wm_interm_111_triplet_pt2(i,p)
term(90) = term(90) + t1(q,i) * wm_interm_112_triplet_pt2(i,p)
term(91) = term(91) + t1(q,i) * wm_interm_113_triplet_pt2(i,p)
term(92) = term(92) + t1(q,i) * wm_interm_96_triplet_pt2(i,p)
term(93) = term(93) + t1(q,i) * wm_interm_97_triplet_pt2(i,p)
term(94) = term(94) + t1(q,i) * wm_interm_98_triplet_pt2(i,p)
term(95) = term(95) + t1(q,i) * wm_interm_133_triplet_pt2(i,p)
term(96) = term(96) + t1(q,i) * wm_interm_134_triplet_pt2(i,p)
term(97) = term(97) + t1(q,i) * wm_interm_123_triplet_pt2(i,p)
term(98) = term(98) + t1(q,i) * wm_interm_124_triplet_pt2(i,p)
term(99) = term(99) + s1(q,i) * wm_interm_159_triplet_pt2(p,i)
term(100) = term(100) + s1(q,i) * wm_interm_160_triplet_pt2(p,i)
term(101) = term(101) + s1(q,i) * wm_interm_161_triplet_pt2(p,i)
term(102) = term(102) + s1(q,i) * wm_interm_146_triplet_pt2(p,i)
term(103) = term(103) + s1(q,i) * wm_interm_147_triplet_pt2(p,i)
term(104) = term(104) + s1(q,i) * wm_interm_148_triplet_pt2(p,i)
term(105) = term(105) + s1(q,i) * wm_interm_175_triplet_pt2(p,i)
term(106) = term(106) + s1(q,i) * wm_interm_176_triplet_pt2(p,i)
term(107) = term(107) + s1(q,i) * wm_interm_167_triplet_pt2(p,i)
term(108) = term(108) + s1(q,i) * wm_interm_168_triplet_pt2(p,i)
term(109) = term(109) + t1(q,i) * wm_interm_159_triplet_pt2(i,p)
term(110) = term(110) + t1(q,i) * wm_interm_160_triplet_pt2(i,p)
term(111) = term(111) + t1(q,i) * wm_interm_161_triplet_pt2(i,p)
term(112) = term(112) + t1(q,i) * wm_interm_146_triplet_pt2(i,p)
term(113) = term(113) + t1(q,i) * wm_interm_147_triplet_pt2(i,p)
term(114) = term(114) + t1(q,i) * wm_interm_148_triplet_pt2(i,p)
term(115) = term(115) + t1(q,i) * wm_interm_175_triplet_pt2(i,p)
term(116) = term(116) + t1(q,i) * wm_interm_176_triplet_pt2(i,p)
term(117) = term(117) + t1(q,i) * wm_interm_167_triplet_pt2(i,p)
term(118) = term(118) + t1(q,i) * wm_interm_168_triplet_pt2(i,p)
end do 

term(61) = term(61) * (-1.0d+0) 
term(62) = term(62) * (2.0d+0) 
term(63) = term(63) * (-1.0d+0) 
term(64) = term(64) * (-2.0d+0) 
term(65) = term(65) * (4.0d+0) 
term(66) = term(66) * (-2.0d+0) 
term(67) = term(67) * (-1.0d+0) 
term(68) = term(68) * (2.0d+0) 
term(69) = term(69) * (2.0d+0) 
term(70) = term(70) * (-4.0d+0) 
term(71) = term(71) * (-1.0d+0) 
term(72) = term(72) * (2.0d+0) 
term(73) = term(73) * (-4.0d+0) 
term(74) = term(74) * (8.0d+0) 
term(75) = term(75) * (4.0d+0) 
term(76) = term(76) * (-8.0d+0) 
term(77) = term(77) * (2.0d+0) 
term(78) = term(78) * (2.0d+0) 
term(79) = term(79) * (-0.5d+0) 
term(81) = term(81) * (-0.5d+0) 
term(82) = term(82) * (-0.5d+0) 
term(84) = term(84) * (-0.5d+0) 
term(85) = term(85) * (-2.0d+0) 
term(86) = term(86) * (2.0d+0) 
term(87) = term(87) * (-2.0d+0) 
term(88) = term(88) * (2.0d+0) 
term(89) = term(89) * (-0.5d+0) 
term(91) = term(91) * (-0.5d+0) 
term(92) = term(92) * (-0.5d+0) 
term(94) = term(94) * (-0.5d+0) 
term(95) = term(95) * (-2.0d+0) 
term(96) = term(96) * (2.0d+0) 
term(97) = term(97) * (-2.0d+0) 
term(98) = term(98) * (2.0d+0) 
term(99) = term(99) * (-1.0d+0) 
term(100) = term(100) * (2.0d+0) 
term(101) = term(101) * (-1.0d+0) 
term(102) = term(102) * (-1.0d+0) 
term(103) = term(103) * (2.0d+0) 
term(104) = term(104) * (-1.0d+0) 
term(105) = term(105) * (-4.0d+0) 
term(106) = term(106) * (4.0d+0) 
term(107) = term(107) * (-4.0d+0) 
term(108) = term(108) * (4.0d+0) 
term(109) = term(109) * (-1.0d+0) 
term(110) = term(110) * (2.0d+0) 
term(111) = term(111) * (-1.0d+0) 
term(112) = term(112) * (-1.0d+0) 
term(113) = term(113) * (2.0d+0) 
term(114) = term(114) * (-1.0d+0) 
term(115) = term(115) * (-4.0d+0) 
term(116) = term(116) * (4.0d+0) 
term(117) = term(117) * (-4.0d+0) 
term(118) = term(118) * (4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(119) = term(119) + wm_interm_14_triplet_pt2(q,i,j,k) * wm_interm_29_triplet_pt2(p,i,j,k)
term(120) = term(120) + wm_interm_14_triplet_pt2(q,i,j,k) * wm_interm_29_triplet_pt2(i,p,j,k)
term(121) = term(121) + wm_interm_14_triplet_pt2(q,i,j,k) * wm_interm_29_triplet_pt2(i,p,k,j)
term(122) = term(122) + wm_interm_14_triplet_pt2(q,i,j,k) * wm_interm_40_triplet_pt2(p,i,j,k)
term(123) = term(123) + wm_interm_14_triplet_pt2(q,i,j,k) * wm_interm_40_triplet_pt2(i,p,j,k)
term(124) = term(124) + wm_interm_14_triplet_pt2(q,i,j,k) * wm_interm_40_triplet_pt2(i,p,k,j)
end do 
end do 
end do 

term(119) = term(119) * (-1.0d+0) 
term(120) = term(120) * (2.0d+0) 
term(121) = term(121) * (-1.0d+0) 
term(122) = term(122) * (-2.0d+0) 
term(123) = term(123) * (4.0d+0) 
term(124) = term(124) * (-2.0d+0) 


    calc_D_ov_wm_triplet_pt2 = zero
    do s = 0, 124
    calc_D_ov_wm_triplet_pt2 = calc_D_ov_wm_triplet_pt2 + term(s)
    end do

    end function calc_D_ov_wm_triplet_pt2
    
    function calc_D_vo_wm_triplet_pt2(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vo_wm_triplet_pt2
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
    real(F64), dimension(0:333) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + wm_interm_14_triplet_pt2(a,i,q,j) * wm_interm_7_triplet_pt2(p,a,i,j)
term(1) = term(1) + wm_interm_14_triplet_pt2(a,i,q,j) * wm_interm_15_triplet_pt2(p,a,i,j)
term(2) = term(2) + wm_interm_14_triplet_pt2(a,i,q,j) * wm_interm_18_triplet_pt2(p,a,i,j)
term(3) = term(3) + wm_interm_14_triplet_pt2(a,i,q,j) * wm_interm_20_triplet_pt2(p,a,i,j)
term(4) = term(4) + wm_interm_14_triplet_pt2(a,i,q,j) * wm_interm_8_triplet_pt2(p,a,i,j)
term(5) = term(5) + wm_interm_14_triplet_pt2(a,i,q,j) * wm_interm_24_triplet_pt2(p,a,i,j)
term(6) = term(6) + wm_interm_14_triplet_pt2(a,i,j,q) * wm_interm_18_triplet_pt2(p,a,i,j)
term(7) = term(7) + wm_interm_14_triplet_pt2(a,i,j,q) * wm_interm_20_triplet_pt2(p,a,i,j)
term(8) = term(8) + wm_interm_14_triplet_pt2(a,i,j,q) * wm_interm_15_triplet_pt2(p,a,i,j)
term(9) = term(9) + wm_interm_14_triplet_pt2(a,i,j,q) * wm_interm_9_triplet_pt2(p,a,i,j)
term(10) = term(10) + wm_interm_14_triplet_pt2(a,i,j,q) * wm_interm_8_triplet_pt2(p,a,i,j)
term(11) = term(11) + wm_interm_14_triplet_pt2(a,i,j,q) * wm_interm_24_triplet_pt2(p,a,i,j)
term(12) = term(12) + wm_interm_11_triplet_pt2(p,a,i,j) * wm_interm_14_triplet_pt2(a,i,q,j)
term(13) = term(13) + wm_interm_12_triplet_pt2(p,a,i,j) * wm_interm_14_triplet_pt2(a,i,q,j)
term(14) = term(14) + wm_interm_14_triplet_pt2(a,i,q,j) * wm_interm_35_triplet_pt2(p,a,i,j)
term(15) = term(15) + wm_interm_14_triplet_pt2(a,i,q,j) * wm_interm_37_triplet_pt2(p,a,i,j)
term(16) = term(16) + wm_interm_14_triplet_pt2(a,i,j,q) * wm_interm_35_triplet_pt2(p,a,i,j)
term(17) = term(17) + wm_interm_14_triplet_pt2(a,i,j,q) * wm_interm_37_triplet_pt2(p,a,i,j)
term(18) = term(18) + wm_interm_12_triplet_pt2(p,a,i,j) * wm_interm_14_triplet_pt2(a,i,j,q)
term(19) = term(19) + wm_interm_11_triplet_pt2(p,a,i,j) * wm_interm_14_triplet_pt2(a,i,j,q)
term(20) = term(20) + wm_interm_25_triplet_pt2(p,a,i,j) * wm_interm_45_triplet_pt2(a,q,j,i)
term(21) = term(21) + wm_interm_28_triplet_pt2(p,a,i,j) * wm_interm_45_triplet_pt2(a,q,j,i)
term(22) = term(22) + wm_interm_25_triplet_pt2(p,a,i,j) * wm_interm_45_triplet_pt2(a,j,q,i)
term(23) = term(23) + wm_interm_28_triplet_pt2(p,a,i,j) * wm_interm_45_triplet_pt2(a,j,q,i)
term(24) = term(24) + wm_interm_25_triplet_pt2(p,a,i,j) * wm_interm_51_triplet_pt2(a,q,j,i)
term(25) = term(25) + wm_interm_28_triplet_pt2(p,a,i,j) * wm_interm_51_triplet_pt2(a,q,j,i)
term(26) = term(26) + wm_interm_27_triplet_pt2(p,a,i,j) * wm_interm_51_triplet_pt2(a,q,j,i)
term(27) = term(27) + wm_interm_26_triplet_pt2(p,a,i,j) * wm_interm_45_triplet_pt2(a,q,j,i)
term(28) = term(28) + wm_interm_27_triplet_pt2(p,a,i,j) * wm_interm_45_triplet_pt2(a,q,j,i)
term(29) = term(29) + wm_interm_26_triplet_pt2(p,a,i,j) * wm_interm_45_triplet_pt2(a,j,q,i)
term(30) = term(30) + wm_interm_27_triplet_pt2(p,a,i,j) * wm_interm_45_triplet_pt2(a,j,q,i)
term(31) = term(31) + wm_interm_26_triplet_pt2(p,a,i,j) * wm_interm_51_triplet_pt2(a,j,q,i)
term(32) = term(32) + wm_interm_25_triplet_pt2(p,a,i,j) * wm_interm_66_triplet_pt2(a,q,j,i)
term(33) = term(33) + wm_interm_28_triplet_pt2(p,a,i,j) * wm_interm_66_triplet_pt2(a,q,j,i)
term(34) = term(34) + wm_interm_25_triplet_pt2(p,a,i,j) * wm_interm_66_triplet_pt2(a,j,q,i)
term(35) = term(35) + wm_interm_28_triplet_pt2(p,a,i,j) * wm_interm_66_triplet_pt2(a,j,q,i)
term(36) = term(36) + wm_interm_25_triplet_pt2(p,a,i,j) * wm_interm_70_triplet_pt2(a,q,j,i)
term(37) = term(37) + wm_interm_28_triplet_pt2(p,a,i,j) * wm_interm_70_triplet_pt2(a,q,j,i)
term(38) = term(38) + wm_interm_27_triplet_pt2(p,a,i,j) * wm_interm_70_triplet_pt2(a,q,j,i)
term(39) = term(39) + wm_interm_26_triplet_pt2(p,a,i,j) * wm_interm_66_triplet_pt2(a,q,j,i)
term(40) = term(40) + wm_interm_27_triplet_pt2(p,a,i,j) * wm_interm_66_triplet_pt2(a,q,j,i)
term(41) = term(41) + wm_interm_26_triplet_pt2(p,a,i,j) * wm_interm_66_triplet_pt2(a,j,q,i)
term(42) = term(42) + wm_interm_27_triplet_pt2(p,a,i,j) * wm_interm_66_triplet_pt2(a,j,q,i)
term(43) = term(43) + wm_interm_26_triplet_pt2(p,a,i,j) * wm_interm_70_triplet_pt2(a,j,q,i)
term(44) = term(44) + r2p(vrdav_Rl, a,i,p,j) * wm_interm_184_triplet_pt2(a,i,j,q)
term(45) = term(45) + r2p(vrdav_Rl, a,i,p,j) * wm_interm_185_triplet_pt2(a,j,i,q)
term(46) = term(46) + r2p(vrdav_Rl, a,i,p,j) * wm_interm_185_triplet_pt2(a,i,j,q)
term(47) = term(47) + r2p(vrdav_Rl, a,i,p,j) * wm_interm_186_triplet_pt2(a,j,i,q)
term(48) = term(48) + r2p(vrdav_Rl, a,i,p,j) * wm_interm_186_triplet_pt2(a,i,j,q)
term(49) = term(49) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_188_triplet_pt2(a,j,i,q)
term(50) = term(50) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_188_triplet_pt2(a,i,j,q)
term(51) = term(51) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_187_triplet_pt2(a,i,j,q)
term(52) = term(52) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_187_triplet_pt2(a,i,j,q)
term(53) = term(53) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_187_triplet_pt2(a,j,i,q)
term(54) = term(54) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_188_triplet_pt2(a,j,i,q)
term(55) = term(55) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_188_triplet_pt2(a,i,j,q)
term(56) = term(56) + r2m(vrdav_Rl, a,i,p,j) * wm_interm_184_triplet_pt2(a,i,j,q)
term(57) = term(57) + r2m(vrdav_Rl, a,i,p,j) * wm_interm_185_triplet_pt2(a,j,i,q)
term(58) = term(58) + r2m(vrdav_Rl, a,i,p,j) * wm_interm_185_triplet_pt2(a,i,j,q)
term(59) = term(59) + r2m(vrdav_Rl, a,i,p,j) * wm_interm_186_triplet_pt2(a,j,i,q)
term(60) = term(60) + r2m(vrdav_Rl, a,i,p,j) * wm_interm_186_triplet_pt2(a,i,j,q)
term(61) = term(61) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_190_triplet_pt2(a,j,i,q)
term(62) = term(62) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_190_triplet_pt2(a,i,j,q)
term(63) = term(63) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_189_triplet_pt2(a,i,j,q)
term(64) = term(64) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_189_triplet_pt2(a,i,j,q)
term(65) = term(65) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_189_triplet_pt2(a,j,i,q)
term(66) = term(66) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_190_triplet_pt2(a,j,i,q)
term(67) = term(67) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_190_triplet_pt2(a,i,j,q)
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
term(9) = term(9) * (-1.0d+0) 
term(10) = term(10) * (2.0d+0) 
term(11) = term(11) * (-1.0d+0) 
term(12) = term(12) * (-4.0d+0) 
term(13) = term(13) * (4.0d+0) 
term(14) = term(14) * (8.0d+0) 
term(15) = term(15) * (-8.0d+0) 
term(16) = term(16) * (-4.0d+0) 
term(17) = term(17) * (4.0d+0) 
term(18) = term(18) * (-4.0d+0) 
term(19) = term(19) * (4.0d+0) 
term(20) = term(20) * (2.0d+0) 
term(21) = term(21) * (-4.0d+0) 
term(22) = term(22) * (-1.0d+0) 
term(23) = term(23) * (2.0d+0) 
term(24) = term(24) * (-1.0d+0) 
term(25) = term(25) * (2.0d+0) 
term(26) = term(26) * (-1.0d+0) 
term(27) = term(27) * (-1.0d+0) 
term(28) = term(28) * (2.0d+0) 
term(29) = term(29) * (2.0d+0) 
term(30) = term(30) * (-1.0d+0) 
term(31) = term(31) * (-1.0d+0) 
term(32) = term(32) * (4.0d+0) 
term(33) = term(33) * (-8.0d+0) 
term(34) = term(34) * (-2.0d+0) 
term(35) = term(35) * (4.0d+0) 
term(36) = term(36) * (-2.0d+0) 
term(37) = term(37) * (4.0d+0) 
term(38) = term(38) * (-2.0d+0) 
term(39) = term(39) * (-2.0d+0) 
term(40) = term(40) * (4.0d+0) 
term(41) = term(41) * (4.0d+0) 
term(42) = term(42) * (-2.0d+0) 
term(43) = term(43) * (-2.0d+0) 
term(44) = term(44) * (-0.5d+0) 
term(45) = term(45) * (-0.5d+0) 
term(47) = term(47) * (-2.0d+0) 
term(48) = term(48) * (2.0d+0) 
term(49) = term(49) * (-0.5d+0) 
term(51) = term(51) * (-0.5d+0) 
term(52) = term(52) * (-2.0d+0) 
term(53) = term(53) * (2.0d+0) 
term(54) = term(54) * (-2.0d+0) 
term(55) = term(55) * (2.0d+0) 
term(56) = term(56) * (-1.0d+0) 
term(57) = term(57) * (-1.0d+0) 
term(58) = term(58) * (2.0d+0) 
term(59) = term(59) * (-4.0d+0) 
term(60) = term(60) * (4.0d+0) 
term(61) = term(61) * (-1.0d+0) 
term(62) = term(62) * (2.0d+0) 
term(63) = term(63) * (-1.0d+0) 
term(64) = term(64) * (-4.0d+0) 
term(65) = term(65) * (4.0d+0) 
term(66) = term(66) * (-4.0d+0) 
term(67) = term(67) * (4.0d+0) 

do a = nocc + 1, nactive 
term(68) = term(68) + wm_interm_17_triplet_pt2(p,a) * wm_interm_6_triplet_pt2(a,q)
term(69) = term(69) + wm_interm_19_triplet_pt2(p,a) * wm_interm_6_triplet_pt2(a,q)
term(70) = term(70) + wm_interm_23_triplet_pt2(p,a) * wm_interm_6_triplet_pt2(a,q)
term(71) = term(71) + wm_interm_34_triplet_pt2(p,a) * wm_interm_6_triplet_pt2(a,q)
term(72) = term(72) + wm_interm_36_triplet_pt2(p,a) * wm_interm_6_triplet_pt2(a,q)
term(73) = term(73) + wm_interm_39_triplet_pt2(p,a) * wm_interm_6_triplet_pt2(a,q)
term(74) = term(74) + wm_interm_49_triplet_pt2(a,q) * wm_interm_4_triplet_pt2(p,a)
term(75) = term(75) + wm_interm_49_triplet_pt2(a,q) * wm_interm_5_triplet_pt2(p,a)
term(76) = term(76) + wm_interm_46_triplet_pt2(a,q) * wm_interm_4_triplet_pt2(p,a)
term(77) = term(77) + wm_interm_46_triplet_pt2(a,q) * wm_interm_5_triplet_pt2(p,a)
term(78) = term(78) + wm_interm_44_triplet_pt2(a,q) * wm_interm_4_triplet_pt2(p,a)
term(79) = term(79) + wm_interm_44_triplet_pt2(a,q) * wm_interm_5_triplet_pt2(p,a)
term(80) = term(80) + wm_interm_4_triplet_pt2(p,a) * wm_interm_65_triplet_pt2(a,q)
term(81) = term(81) + wm_interm_5_triplet_pt2(p,a) * wm_interm_65_triplet_pt2(a,q)
term(82) = term(82) + wm_interm_4_triplet_pt2(p,a) * wm_interm_67_triplet_pt2(a,q)
term(83) = term(83) + wm_interm_5_triplet_pt2(p,a) * wm_interm_67_triplet_pt2(a,q)
end do 

term(68) = term(68) * (-1.0d+0) 
term(69) = term(69) * (2.0d+0) 
term(70) = term(70) * (-1.0d+0) 
term(71) = term(71) * (-2.0d+0) 
term(72) = term(72) * (4.0d+0) 
term(73) = term(73) * (-2.0d+0) 
term(74) = term(74) * (-1.0d+0) 
term(75) = term(75) * (2.0d+0) 
term(76) = term(76) * (2.0d+0) 
term(77) = term(77) * (-4.0d+0) 
term(78) = term(78) * (-1.0d+0) 
term(79) = term(79) * (2.0d+0) 
term(80) = term(80) * (-4.0d+0) 
term(81) = term(81) * (8.0d+0) 
term(82) = term(82) * (4.0d+0) 
term(83) = term(83) * (-8.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(84) = term(84) + wm_interm_0_triplet_pt2(p,i,j,k) * wm_interm_16_triplet_pt2(j,k,i,q)
term(85) = term(85) + wm_interm_10_triplet_pt2(p,i,j,k) * wm_interm_16_triplet_pt2(k,j,i,q)
term(86) = term(86) + wm_interm_10_triplet_pt2(p,i,j,k) * wm_interm_16_triplet_pt2(j,k,i,q)
term(87) = term(87) + wm_interm_48_triplet_pt2(p,i,j,k) * wm_interm_58_triplet_pt2(q,i,j,k)
term(88) = term(88) + wm_interm_48_triplet_pt2(p,i,j,k) * wm_interm_58_triplet_pt2(i,q,j,k)
term(89) = term(89) + wm_interm_48_triplet_pt2(p,i,j,k) * wm_interm_58_triplet_pt2(i,q,k,j)
term(90) = term(90) + wm_interm_48_triplet_pt2(p,i,j,k) * wm_interm_75_triplet_pt2(q,i,j,k)
term(91) = term(91) + wm_interm_48_triplet_pt2(p,i,j,k) * wm_interm_75_triplet_pt2(i,q,j,k)
term(92) = term(92) + wm_interm_48_triplet_pt2(p,i,j,k) * wm_interm_75_triplet_pt2(i,q,k,j)
end do 
end do 
end do 

term(84) = term(84) * (-1.0d+0) 
term(85) = term(85) * (-4.0d+0) 
term(86) = term(86) * (4.0d+0) 
term(87) = term(87) * (-1.0d+0) 
term(88) = term(88) * (2.0d+0) 
term(89) = term(89) * (-1.0d+0) 
term(90) = term(90) * (-2.0d+0) 
term(91) = term(91) * (4.0d+0) 
term(92) = term(92) * (-2.0d+0) 

do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(93) = term(93) + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, p,i,b,q) * wm_interm_4_triplet_pt2(b,a)
term(94) = term(94) + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, p,i,b,q) * wm_interm_5_triplet_pt2(b,a)
end do 
end do 
end do 

term(93) = term(93) * (-1.0d+0) 
term(94) = term(94) * (2.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(95) = term(95) + wm_interm_6_triplet_pt2(a,i) * wm_interm_7_triplet_pt2(p,a,q,i)
term(96) = term(96) + wm_interm_6_triplet_pt2(a,i) * wm_interm_8_triplet_pt2(p,a,q,i)
term(97) = term(97) + wm_interm_6_triplet_pt2(a,i) * wm_interm_9_triplet_pt2(p,a,q,i)
term(98) = term(98) + wm_interm_11_triplet_pt2(p,a,q,i) * wm_interm_6_triplet_pt2(a,i)
term(99) = term(99) + wm_interm_12_triplet_pt2(p,a,q,i) * wm_interm_6_triplet_pt2(a,i)
term(100) = term(100) + wm_interm_13_triplet_pt2(a,i) * wm_interm_25_triplet_pt2(a,p,i,q)
term(101) = term(101) + wm_interm_21_triplet_pt2(a,i) * wm_interm_25_triplet_pt2(a,p,i,q)
term(102) = term(102) + wm_interm_22_triplet_pt2(a,i) * wm_interm_25_triplet_pt2(a,p,i,q)
term(103) = term(103) + wm_interm_13_triplet_pt2(a,i) * wm_interm_28_triplet_pt2(a,p,i,q)
term(104) = term(104) + wm_interm_21_triplet_pt2(a,i) * wm_interm_28_triplet_pt2(a,p,i,q)
term(105) = term(105) + wm_interm_22_triplet_pt2(a,i) * wm_interm_28_triplet_pt2(a,p,i,q)
term(106) = term(106) + wm_interm_13_triplet_pt2(a,i) * wm_interm_26_triplet_pt2(a,p,i,q)
term(107) = term(107) + wm_interm_21_triplet_pt2(a,i) * wm_interm_26_triplet_pt2(a,p,i,q)
term(108) = term(108) + wm_interm_22_triplet_pt2(a,i) * wm_interm_26_triplet_pt2(a,p,i,q)
term(109) = term(109) + wm_interm_13_triplet_pt2(a,i) * wm_interm_27_triplet_pt2(a,p,i,q)
term(110) = term(110) + wm_interm_21_triplet_pt2(a,i) * wm_interm_27_triplet_pt2(a,p,i,q)
term(111) = term(111) + wm_interm_22_triplet_pt2(a,i) * wm_interm_27_triplet_pt2(a,p,i,q)
term(112) = term(112) + wm_interm_25_triplet_pt2(a,p,i,q) * wm_interm_33_triplet_pt2(a,i)
term(113) = term(113) + wm_interm_25_triplet_pt2(a,p,i,q) * wm_interm_38_triplet_pt2(a,i)
term(114) = term(114) + wm_interm_28_triplet_pt2(a,p,i,q) * wm_interm_33_triplet_pt2(a,i)
term(115) = term(115) + wm_interm_28_triplet_pt2(a,p,i,q) * wm_interm_38_triplet_pt2(a,i)
term(116) = term(116) + wm_interm_26_triplet_pt2(a,p,i,q) * wm_interm_33_triplet_pt2(a,i)
term(117) = term(117) + wm_interm_26_triplet_pt2(a,p,i,q) * wm_interm_38_triplet_pt2(a,i)
term(118) = term(118) + wm_interm_27_triplet_pt2(a,p,i,q) * wm_interm_33_triplet_pt2(a,i)
term(119) = term(119) + wm_interm_27_triplet_pt2(a,p,i,q) * wm_interm_38_triplet_pt2(a,i)
term(120) = term(120) + wm_interm_25_triplet_pt2(p,a,q,i) * wm_interm_46_triplet_pt2(a,i)
term(121) = term(121) + wm_interm_28_triplet_pt2(p,a,q,i) * wm_interm_46_triplet_pt2(a,i)
term(122) = term(122) + wm_interm_25_triplet_pt2(p,a,q,i) * wm_interm_49_triplet_pt2(a,i)
term(123) = term(123) + wm_interm_28_triplet_pt2(p,a,q,i) * wm_interm_49_triplet_pt2(a,i)
term(124) = term(124) + wm_interm_25_triplet_pt2(p,a,q,i) * wm_interm_44_triplet_pt2(a,i)
term(125) = term(125) + wm_interm_28_triplet_pt2(p,a,q,i) * wm_interm_44_triplet_pt2(a,i)
term(126) = term(126) + wm_interm_27_triplet_pt2(p,a,q,i) * wm_interm_44_triplet_pt2(a,i)
term(127) = term(127) + wm_interm_53_triplet_pt2(p,a,q,i) * wm_interm_54_triplet_pt2(a,i)
term(128) = term(128) + wm_interm_26_triplet_pt2(p,a,q,i) * wm_interm_44_triplet_pt2(a,i)
term(129) = term(129) + wm_interm_26_triplet_pt2(p,a,q,i) * wm_interm_46_triplet_pt2(a,i)
term(130) = term(130) + wm_interm_27_triplet_pt2(p,a,q,i) * wm_interm_46_triplet_pt2(a,i)
term(131) = term(131) + wm_interm_54_triplet_pt2(a,i) * wm_interm_59_triplet_pt2(p,a,q,i)
term(132) = term(132) + wm_interm_26_triplet_pt2(p,a,q,i) * wm_interm_49_triplet_pt2(a,i)
term(133) = term(133) + wm_interm_27_triplet_pt2(p,a,q,i) * wm_interm_49_triplet_pt2(a,i)
term(134) = term(134) + wm_interm_54_triplet_pt2(a,i) * wm_interm_63_triplet_pt2(p,a,q,i)
term(135) = term(135) + wm_interm_25_triplet_pt2(p,a,q,i) * wm_interm_67_triplet_pt2(a,i)
term(136) = term(136) + wm_interm_28_triplet_pt2(p,a,q,i) * wm_interm_67_triplet_pt2(a,i)
term(137) = term(137) + wm_interm_25_triplet_pt2(p,a,q,i) * wm_interm_65_triplet_pt2(a,i)
term(138) = term(138) + wm_interm_28_triplet_pt2(p,a,q,i) * wm_interm_65_triplet_pt2(a,i)
term(139) = term(139) + wm_interm_27_triplet_pt2(p,a,q,i) * wm_interm_65_triplet_pt2(a,i)
term(140) = term(140) + wm_interm_54_triplet_pt2(a,i) * wm_interm_71_triplet_pt2(p,a,q,i)
term(141) = term(141) + wm_interm_26_triplet_pt2(p,a,q,i) * wm_interm_65_triplet_pt2(a,i)
term(142) = term(142) + wm_interm_26_triplet_pt2(p,a,q,i) * wm_interm_67_triplet_pt2(a,i)
term(143) = term(143) + wm_interm_27_triplet_pt2(p,a,q,i) * wm_interm_67_triplet_pt2(a,i)
term(144) = term(144) + wm_interm_54_triplet_pt2(a,i) * wm_interm_73_triplet_pt2(p,a,q,i)
term(145) = term(145) + r2p(vrdav_Rl, a,q,p,i) * wm_interm_78_triplet_pt2(a,i)
term(146) = term(146) + r2p(vrdav_Rl, a,i,p,q) * wm_interm_78_triplet_pt2(a,i)
term(147) = term(147) + r2p(vrdav_Rl, a,q,p,i) * wm_interm_79_triplet_pt2(a,i)
term(148) = term(148) + r2p(vrdav_Rl, a,i,p,q) * wm_interm_79_triplet_pt2(a,i)
term(149) = term(149) + r2p(vrdav_Rl, a,q,p,i) * wm_interm_80_triplet_pt2(a,i)
term(150) = term(150) + r2p(vrdav_Rl, a,i,p,q) * wm_interm_80_triplet_pt2(a,i)
term(151) = term(151) + r2p(vrdav_Rl, a,q,p,i) * wm_interm_81_triplet_pt2(a,i)
term(152) = term(152) + r2p(vrdav_Rl, a,i,p,q) * wm_interm_81_triplet_pt2(a,i)
term(153) = term(153) + r2p(vrdav_Rl, a,q,p,i) * wm_interm_82_triplet_pt2(a,i)
term(154) = term(154) + r2p(vrdav_Rl, a,i,p,q) * wm_interm_82_triplet_pt2(a,i)
term(155) = term(155) + r2p(vrdav_Rr, a,q,p,i) * wm_interm_83_triplet_pt2(a,i)
term(156) = term(156) + r2p(vrdav_Rr, a,i,p,q) * wm_interm_83_triplet_pt2(a,i)
term(157) = term(157) + r2p(vrdav_Rr, a,q,p,i) * wm_interm_84_triplet_pt2(a,i)
term(158) = term(158) + r2p(vrdav_Rr, a,i,p,q) * wm_interm_84_triplet_pt2(a,i)
term(159) = term(159) + r2p(vrdav_Rr, a,q,p,i) * wm_interm_85_triplet_pt2(a,i)
term(160) = term(160) + r2p(vrdav_Rr, a,i,p,q) * wm_interm_85_triplet_pt2(a,i)
term(161) = term(161) + r2m(vrdav_Rr, a,q,p,i) * wm_interm_83_triplet_pt2(a,i)
term(162) = term(162) + r2m(vrdav_Rr, a,i,p,q) * wm_interm_83_triplet_pt2(a,i)
term(163) = term(163) + r2m(vrdav_Rr, a,q,p,i) * wm_interm_84_triplet_pt2(a,i)
term(164) = term(164) + r2m(vrdav_Rr, a,i,p,q) * wm_interm_84_triplet_pt2(a,i)
term(165) = term(165) + r2m(vrdav_Rr, a,q,p,i) * wm_interm_85_triplet_pt2(a,i)
term(166) = term(166) + r2m(vrdav_Rr, a,i,p,q) * wm_interm_85_triplet_pt2(a,i)
term(167) = term(167) + r2m(vrdav_Rl, a,q,p,i) * wm_interm_78_triplet_pt2(a,i)
term(168) = term(168) + r2m(vrdav_Rl, a,i,p,q) * wm_interm_78_triplet_pt2(a,i)
term(169) = term(169) + r2m(vrdav_Rl, a,q,p,i) * wm_interm_79_triplet_pt2(a,i)
term(170) = term(170) + r2m(vrdav_Rl, a,i,p,q) * wm_interm_79_triplet_pt2(a,i)
term(171) = term(171) + r2m(vrdav_Rl, a,q,p,i) * wm_interm_80_triplet_pt2(a,i)
term(172) = term(172) + r2m(vrdav_Rl, a,i,p,q) * wm_interm_80_triplet_pt2(a,i)
term(173) = term(173) + r2m(vrdav_Rl, a,q,p,i) * wm_interm_81_triplet_pt2(a,i)
term(174) = term(174) + r2m(vrdav_Rl, a,i,p,q) * wm_interm_81_triplet_pt2(a,i)
term(175) = term(175) + r2m(vrdav_Rl, a,q,p,i) * wm_interm_82_triplet_pt2(a,i)
term(176) = term(176) + r2m(vrdav_Rl, a,i,p,q) * wm_interm_82_triplet_pt2(a,i)
term(177) = term(177) + r2p(vrdav_Rr, a,q,p,i) * wm_interm_86_triplet_pt2(a,i)
term(178) = term(178) + r2p(vrdav_Rr, a,i,p,q) * wm_interm_86_triplet_pt2(a,i)
term(179) = term(179) + r2p(vrdav_Rr, a,q,p,i) * wm_interm_87_triplet_pt2(a,i)
term(180) = term(180) + r2p(vrdav_Rr, a,i,p,q) * wm_interm_87_triplet_pt2(a,i)
term(181) = term(181) + r2m(vrdav_Rr, a,q,p,i) * wm_interm_86_triplet_pt2(a,i)
term(182) = term(182) + r2m(vrdav_Rr, a,i,p,q) * wm_interm_86_triplet_pt2(a,i)
term(183) = term(183) + r2m(vrdav_Rr, a,q,p,i) * wm_interm_87_triplet_pt2(a,i)
term(184) = term(184) + r2m(vrdav_Rr, a,i,p,q) * wm_interm_87_triplet_pt2(a,i)
end do 
end do 

term(95) = term(95) * (-1.0d+0) 
term(96) = term(96) * (-1.0d+0) 
term(97) = term(97) * (2.0d+0) 
term(98) = term(98) * (-4.0d+0) 
term(99) = term(99) * (4.0d+0) 
term(100) = term(100) * (2.0d+0) 
term(101) = term(101) * (2.0d+0) 
term(102) = term(102) * (-4.0d+0) 
term(103) = term(103) * (-4.0d+0) 
term(104) = term(104) * (-4.0d+0) 
term(105) = term(105) * (8.0d+0) 
term(106) = term(106) * (-1.0d+0) 
term(107) = term(107) * (-1.0d+0) 
term(108) = term(108) * (2.0d+0) 
term(109) = term(109) * (2.0d+0) 
term(110) = term(110) * (2.0d+0) 
term(111) = term(111) * (-4.0d+0) 
term(112) = term(112) * (8.0d+0) 
term(113) = term(113) * (-8.0d+0) 
term(114) = term(114) * (-16.0d+0) 
term(115) = term(115) * (16.0d+0) 
term(116) = term(116) * (-4.0d+0) 
term(117) = term(117) * (4.0d+0) 
term(118) = term(118) * (8.0d+0) 
term(119) = term(119) * (-8.0d+0) 
term(120) = term(120) * (-4.0d+0) 
term(121) = term(121) * (8.0d+0) 
term(122) = term(122) * (2.0d+0) 
term(123) = term(123) * (-4.0d+0) 
term(124) = term(124) * (2.0d+0) 
term(125) = term(125) * (-4.0d+0) 
term(126) = term(126) * (2.0d+0) 
term(127) = term(127) * (-1.0d+0) 
term(128) = term(128) * (-1.0d+0) 
term(129) = term(129) * (2.0d+0) 
term(130) = term(130) * (-4.0d+0) 
term(131) = term(131) * (-1.0d+0) 
term(132) = term(132) * (-1.0d+0) 
term(133) = term(133) * (2.0d+0) 
term(134) = term(134) * (2.0d+0) 
term(135) = term(135) * (-8.0d+0) 
term(136) = term(136) * (16.0d+0) 
term(137) = term(137) * (8.0d+0) 
term(138) = term(138) * (-16.0d+0) 
term(139) = term(139) * (8.0d+0) 
term(140) = term(140) * (-4.0d+0) 
term(141) = term(141) * (-4.0d+0) 
term(142) = term(142) * (4.0d+0) 
term(143) = term(143) * (-8.0d+0) 
term(144) = term(144) * (4.0d+0) 
term(145) = term(145) * (-1.0d+0) 
term(146) = term(146) * (2.0d+0) 
term(147) = term(147) * (0.5d+0) 
term(148) = term(148) * (-1.0d+0) 
term(149) = term(149) * (0.5d+0) 
term(150) = term(150) * (-1.0d+0) 
term(151) = term(151) * (-2.0d+0) 
term(152) = term(152) * (4.0d+0) 
term(153) = term(153) * (2.0d+0) 
term(154) = term(154) * (-4.0d+0) 
term(156) = term(156) * (-2.0d+0) 
term(157) = term(157) * (-0.5d+0) 
term(159) = term(159) * (-0.5d+0) 
term(161) = term(161) * (4.0d+0) 
term(162) = term(162) * (-4.0d+0) 
term(163) = term(163) * (-2.0d+0) 
term(164) = term(164) * (2.0d+0) 
term(165) = term(165) * (-2.0d+0) 
term(166) = term(166) * (2.0d+0) 
term(167) = term(167) * (-4.0d+0) 
term(168) = term(168) * (4.0d+0) 
term(169) = term(169) * (2.0d+0) 
term(170) = term(170) * (-2.0d+0) 
term(171) = term(171) * (2.0d+0) 
term(172) = term(172) * (-2.0d+0) 
term(173) = term(173) * (-8.0d+0) 
term(174) = term(174) * (8.0d+0) 
term(175) = term(175) * (8.0d+0) 
term(176) = term(176) * (-8.0d+0) 
term(177) = term(177) * (2.0d+0) 
term(178) = term(178) * (-4.0d+0) 
term(179) = term(179) * (-2.0d+0) 
term(180) = term(180) * (4.0d+0) 
term(181) = term(181) * (8.0d+0) 
term(182) = term(182) * (-8.0d+0) 
term(183) = term(183) * (-8.0d+0) 
term(184) = term(184) * (8.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
term(185) = term(185) + wm_interm_16_triplet_pt2(i,j,k,q) * wm_interm_3_triplet_pt2(p,k,j,i)
end do 
end do 
end do 

term(185) = term(185) * (-1.0d+0) 

do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
term(186) = term(186) + wm_interm_16_triplet_pt2(i,j,k,q) * wm_interm_3_triplet_pt2(p,k,i,j)
end do 
end do 
end do 

term(186) = term(186) * (2.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
term(187) = term(187) + wm_interm_0_triplet_pt2(p,i,j,q) * wm_interm_1_triplet_pt2(j,i)
term(188) = term(188) + wm_interm_0_triplet_pt2(p,i,j,q) * wm_interm_2_triplet_pt2(j,i)
term(189) = term(189) + wm_interm_10_triplet_pt2(p,i,q,j) * wm_interm_1_triplet_pt2(j,i)
term(190) = term(190) + wm_interm_10_triplet_pt2(p,i,q,j) * wm_interm_2_triplet_pt2(j,i)
term(191) = term(191) + wm_interm_10_triplet_pt2(p,i,j,q) * wm_interm_1_triplet_pt2(j,i)
term(192) = term(192) + wm_interm_10_triplet_pt2(p,i,j,q) * wm_interm_2_triplet_pt2(j,i)
term(193) = term(193) + wm_interm_14_triplet_pt2(p,i,j,q) * wm_interm_30_triplet_pt2(i,j)
term(194) = term(194) + wm_interm_14_triplet_pt2(p,i,j,q) * wm_interm_31_triplet_pt2(i,j)
term(195) = term(195) + wm_interm_14_triplet_pt2(p,i,j,q) * wm_interm_32_triplet_pt2(i,j)
term(196) = term(196) + wm_interm_14_triplet_pt2(p,i,q,j) * wm_interm_30_triplet_pt2(i,j)
term(197) = term(197) + wm_interm_14_triplet_pt2(p,i,q,j) * wm_interm_31_triplet_pt2(i,j)
term(198) = term(198) + wm_interm_14_triplet_pt2(p,i,q,j) * wm_interm_32_triplet_pt2(i,j)
term(199) = term(199) + wm_interm_14_triplet_pt2(p,i,j,q) * wm_interm_41_triplet_pt2(i,j)
term(200) = term(200) + wm_interm_14_triplet_pt2(p,i,j,q) * wm_interm_42_triplet_pt2(i,j)
term(201) = term(201) + wm_interm_14_triplet_pt2(p,i,j,q) * wm_interm_43_triplet_pt2(i,j)
term(202) = term(202) + wm_interm_14_triplet_pt2(p,i,q,j) * wm_interm_41_triplet_pt2(i,j)
term(203) = term(203) + wm_interm_14_triplet_pt2(p,i,q,j) * wm_interm_42_triplet_pt2(i,j)
term(204) = term(204) + wm_interm_14_triplet_pt2(p,i,q,j) * wm_interm_43_triplet_pt2(i,j)
term(205) = term(205) + wm_interm_48_triplet_pt2(p,i,q,j) * wm_interm_57_triplet_pt2(i,j)
term(206) = term(206) + wm_interm_48_triplet_pt2(p,i,j,q) * wm_interm_57_triplet_pt2(i,j)
term(207) = term(207) + wm_interm_48_triplet_pt2(p,i,q,j) * wm_interm_61_triplet_pt2(i,j)
term(208) = term(208) + wm_interm_48_triplet_pt2(p,i,j,q) * wm_interm_61_triplet_pt2(i,j)
term(209) = term(209) + wm_interm_48_triplet_pt2(p,i,q,j) * wm_interm_64_triplet_pt2(i,j)
term(210) = term(210) + wm_interm_48_triplet_pt2(p,i,j,q) * wm_interm_64_triplet_pt2(i,j)
term(211) = term(211) + wm_interm_48_triplet_pt2(p,i,q,j) * wm_interm_74_triplet_pt2(i,j)
term(212) = term(212) + wm_interm_48_triplet_pt2(p,i,j,q) * wm_interm_74_triplet_pt2(i,j)
term(213) = term(213) + wm_interm_48_triplet_pt2(p,i,q,j) * wm_interm_77_triplet_pt2(i,j)
term(214) = term(214) + wm_interm_48_triplet_pt2(p,i,j,q) * wm_interm_77_triplet_pt2(i,j)
end do 
end do 

term(187) = term(187) * (-1.0d+0) 
term(188) = term(188) * (2.0d+0) 
term(189) = term(189) * (-4.0d+0) 
term(190) = term(190) * (8.0d+0) 
term(191) = term(191) * (4.0d+0) 
term(192) = term(192) * (-8.0d+0) 
term(193) = term(193) * (2.0d+0) 
term(194) = term(194) * (-4.0d+0) 
term(195) = term(195) * (2.0d+0) 
term(196) = term(196) * (-1.0d+0) 
term(197) = term(197) * (2.0d+0) 
term(198) = term(198) * (-1.0d+0) 
term(199) = term(199) * (4.0d+0) 
term(200) = term(200) * (-8.0d+0) 
term(201) = term(201) * (4.0d+0) 
term(202) = term(202) * (-2.0d+0) 
term(203) = term(203) * (4.0d+0) 
term(204) = term(204) * (-2.0d+0) 
term(205) = term(205) * (-1.0d+0) 
term(206) = term(206) * (2.0d+0) 
term(207) = term(207) * (2.0d+0) 
term(208) = term(208) * (-4.0d+0) 
term(209) = term(209) * (-1.0d+0) 
term(210) = term(210) * (2.0d+0) 
term(211) = term(211) * (-4.0d+0) 
term(212) = term(212) * (8.0d+0) 
term(213) = term(213) * (4.0d+0) 
term(214) = term(214) * (-8.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(215) = term(215) + r2m(vrdav_Rl, a,j,p,i) * wm_interm_184_triplet_pt2(a,j,i,q)
term(216) = term(216) + r2m(vrdav_Rl, a,j,p,i) * wm_interm_184_triplet_pt2(a,i,j,q)
term(217) = term(217) + r2m(vrdav_Rl, a,j,p,i) * wm_interm_185_triplet_pt2(a,i,j,q)
term(218) = term(218) + r2m(vrdav_Rl, a,j,p,i) * wm_interm_186_triplet_pt2(a,i,j,q)
term(219) = term(219) + r2m(vrdav_Rl, a,j,p,i) * wm_interm_186_triplet_pt2(a,j,i,q)
end do 
end do 
end do 

term(215) = term(215) * (-1.0d+0) 
term(216) = term(216) * (2.0d+0) 
term(217) = term(217) * (-1.0d+0) 
term(218) = term(218) * (-4.0d+0) 
term(219) = term(219) * (4.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
term(220) = term(220) + wm_interm_1_triplet_pt2(i,j) * wm_interm_3_triplet_pt2(p,j,q,i)
term(221) = term(221) + wm_interm_2_triplet_pt2(i,j) * wm_interm_3_triplet_pt2(p,j,q,i)
term(222) = term(222) + wm_interm_1_triplet_pt2(i,j) * wm_interm_3_triplet_pt2(p,j,i,q)
term(223) = term(223) + wm_interm_2_triplet_pt2(i,j) * wm_interm_3_triplet_pt2(p,j,i,q)
term(224) = term(224) + wm_interm_1_triplet_pt2(i,j) * wm_interm_45_triplet_pt2(p,q,j,i)
term(225) = term(225) + wm_interm_2_triplet_pt2(i,j) * wm_interm_45_triplet_pt2(p,q,j,i)
term(226) = term(226) + wm_interm_1_triplet_pt2(i,j) * wm_interm_51_triplet_pt2(p,j,q,i)
term(227) = term(227) + wm_interm_2_triplet_pt2(i,j) * wm_interm_51_triplet_pt2(p,j,q,i)
term(228) = term(228) + wm_interm_1_triplet_pt2(i,j) * wm_interm_45_triplet_pt2(p,j,q,i)
term(229) = term(229) + wm_interm_2_triplet_pt2(i,j) * wm_interm_45_triplet_pt2(p,j,q,i)
term(230) = term(230) + wm_interm_1_triplet_pt2(i,j) * wm_interm_66_triplet_pt2(p,q,j,i)
term(231) = term(231) + wm_interm_2_triplet_pt2(i,j) * wm_interm_66_triplet_pt2(p,q,j,i)
term(232) = term(232) + wm_interm_1_triplet_pt2(i,j) * wm_interm_70_triplet_pt2(p,j,q,i)
term(233) = term(233) + wm_interm_2_triplet_pt2(i,j) * wm_interm_70_triplet_pt2(p,j,q,i)
term(234) = term(234) + wm_interm_1_triplet_pt2(i,j) * wm_interm_66_triplet_pt2(p,j,q,i)
term(235) = term(235) + wm_interm_2_triplet_pt2(i,j) * wm_interm_66_triplet_pt2(p,j,q,i)
end do 
end do 

term(220) = term(220) * (-1.0d+0) 
term(221) = term(221) * (2.0d+0) 
term(222) = term(222) * (2.0d+0) 
term(223) = term(223) * (-4.0d+0) 
term(224) = term(224) * (-1.0d+0) 
term(225) = term(225) * (2.0d+0) 
term(226) = term(226) * (-1.0d+0) 
term(227) = term(227) * (2.0d+0) 
term(228) = term(228) * (2.0d+0) 
term(229) = term(229) * (-4.0d+0) 
term(230) = term(230) * (-2.0d+0) 
term(231) = term(231) * (4.0d+0) 
term(232) = term(232) * (-2.0d+0) 
term(233) = term(233) * (4.0d+0) 
term(234) = term(234) * (4.0d+0) 
term(235) = term(235) * (-8.0d+0) 

do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(236) = term(236) + r1(vrdav_Rl, a,i) * t2(b,p,i,q) * wm_interm_23_triplet_pt2(a,b)
term(237) = term(237) + r1(vrdav_Rl, a,i) * t2(b,p,i,q) * wm_interm_17_triplet_pt2(a,b)
term(238) = term(238) + r1(vrdav_Rl, a,i) * t2(b,p,i,q) * wm_interm_19_triplet_pt2(a,b)
term(239) = term(239) + r1(vrdav_Rl, a,i) * t2(b,p,i,q) * wm_interm_34_triplet_pt2(a,b)
term(240) = term(240) + r1(vrdav_Rl, a,i) * t2(b,p,i,q) * wm_interm_36_triplet_pt2(a,b)
term(241) = term(241) + r1(vrdav_Rr, a,i) * s2(b,p,i,q) * wm_interm_55_triplet_pt2(a,b)
term(242) = term(242) + r1(vrdav_Rr, a,i) * s2(b,p,i,q) * wm_interm_60_triplet_pt2(a,b)
term(243) = term(243) + r1(vrdav_Rr, a,i) * s2(b,p,i,q) * wm_interm_62_triplet_pt2(a,b)
term(244) = term(244) + r1(vrdav_Rr, a,i) * s2(b,p,i,q) * wm_interm_72_triplet_pt2(a,b)
term(245) = term(245) + r1(vrdav_Rr, a,i) * s2(b,p,i,q) * wm_interm_76_triplet_pt2(a,b)
end do 
end do 
end do 

term(236) = term(236) * (2.0d+0) 
term(237) = term(237) * (2.0d+0) 
term(238) = term(238) * (-4.0d+0) 
term(239) = term(239) * (8.0d+0) 
term(240) = term(240) * (-8.0d+0) 
term(241) = term(241) * (2.0d+0) 
term(242) = term(242) * (-4.0d+0) 
term(243) = term(243) * (2.0d+0) 
term(244) = term(244) * (8.0d+0) 
term(245) = term(245) * (-8.0d+0) 

term(246) = term(246) + s1(p,q) * wm_interm_89_triplet_pt2
term(247) = term(247) + t1(p,q) * wm_interm_89_triplet_pt2
term(248) = term(248) + s1(p,q) * wm_interm_139_triplet_pt2
term(249) = term(249) + s1(p,q) * wm_interm_140_triplet_pt2
term(250) = term(250) + s1(p,q) * wm_interm_141_triplet_pt2
term(251) = term(251) + s1(p,q) * wm_interm_142_triplet_pt2
term(252) = term(252) + s1(p,q) * wm_interm_143_triplet_pt2
term(253) = term(253) + t1(p,q) * wm_interm_139_triplet_pt2
term(254) = term(254) + t1(p,q) * wm_interm_140_triplet_pt2
term(255) = term(255) + t1(p,q) * wm_interm_141_triplet_pt2
term(256) = term(256) + t1(p,q) * wm_interm_142_triplet_pt2
term(257) = term(257) + t1(p,q) * wm_interm_143_triplet_pt2
term(258) = term(258) + s1(p,q) * wm_interm_179_triplet_pt2
term(259) = term(259) + s1(p,q) * wm_interm_180_triplet_pt2
term(260) = term(260) + s1(p,q) * wm_interm_181_triplet_pt2
term(261) = term(261) + s1(p,q) * wm_interm_182_triplet_pt2
term(262) = term(262) + s1(p,q) * wm_interm_183_triplet_pt2
term(263) = term(263) + t1(p,q) * wm_interm_179_triplet_pt2
term(264) = term(264) + t1(p,q) * wm_interm_180_triplet_pt2
term(265) = term(265) + t1(p,q) * wm_interm_181_triplet_pt2
term(266) = term(266) + t1(p,q) * wm_interm_182_triplet_pt2
term(267) = term(267) + t1(p,q) * wm_interm_183_triplet_pt2

term(246) = term(246) * (-4.0d+0) 
term(247) = term(247) * (-4.0d+0) 
term(249) = term(249) * (-2.0d+0) 
term(251) = term(251) * (4.0d+0) 
term(252) = term(252) * (-4.0d+0) 
term(254) = term(254) * (-2.0d+0) 
term(256) = term(256) * (4.0d+0) 
term(257) = term(257) * (-4.0d+0) 
term(258) = term(258) * (2.0d+0) 
term(259) = term(259) * (-4.0d+0) 
term(260) = term(260) * (2.0d+0) 
term(261) = term(261) * (8.0d+0) 
term(262) = term(262) * (-8.0d+0) 
term(263) = term(263) * (2.0d+0) 
term(264) = term(264) * (-4.0d+0) 
term(265) = term(265) * (2.0d+0) 
term(266) = term(266) * (8.0d+0) 
term(267) = term(267) * (-8.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(268) = term(268) + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, b,q,p,i) * wm_interm_4_triplet_pt2(b,a)
term(269) = term(269) + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, b,q,p,i) * wm_interm_5_triplet_pt2(b,a)
term(270) = term(270) + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, b,q,p,i) * wm_interm_4_triplet_pt2(b,a)
term(271) = term(271) + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, b,q,p,i) * wm_interm_5_triplet_pt2(b,a)
term(272) = term(272) + r1(vrdav_Rl, b,i) * t2(a,p,q,i) * wm_interm_23_triplet_pt2(b,a)
term(273) = term(273) + r1(vrdav_Rl, b,i) * t2(a,p,q,i) * wm_interm_17_triplet_pt2(b,a)
term(274) = term(274) + r1(vrdav_Rl, b,i) * t2(a,p,q,i) * wm_interm_19_triplet_pt2(b,a)
term(275) = term(275) + r1(vrdav_Rl, b,i) * t2(a,p,q,i) * wm_interm_34_triplet_pt2(b,a)
term(276) = term(276) + r1(vrdav_Rl, b,i) * t2(a,p,q,i) * wm_interm_36_triplet_pt2(b,a)
term(277) = term(277) + r2p(vrdav_Rl, a,q,p,i) * r1(vrdav_Rr, b,i) * wm_interm_4_triplet_pt2(b,a)
term(278) = term(278) + r2p(vrdav_Rl, a,q,p,i) * r1(vrdav_Rr, b,i) * wm_interm_5_triplet_pt2(b,a)
term(279) = term(279) + r2m(vrdav_Rl, a,q,p,i) * r1(vrdav_Rr, b,i) * wm_interm_4_triplet_pt2(b,a)
term(280) = term(280) + r2m(vrdav_Rl, a,q,p,i) * r1(vrdav_Rr, b,i) * wm_interm_5_triplet_pt2(b,a)
end do 
end do 
end do 

term(268) = term(268) * (-1.0d+0) 
term(269) = term(269) * (2.0d+0) 
term(270) = term(270) * (-4.0d+0) 
term(271) = term(271) * (8.0d+0) 
term(272) = term(272) * (-1.0d+0) 
term(273) = term(273) * (-1.0d+0) 
term(274) = term(274) * (2.0d+0) 
term(275) = term(275) * (-4.0d+0) 
term(276) = term(276) * (4.0d+0) 
term(277) = term(277) * (-1.0d+0) 
term(278) = term(278) * (2.0d+0) 
term(279) = term(279) * (-4.0d+0) 
term(280) = term(280) * (8.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(281) = term(281) + r1(vrdav_Rr, a,i) * s2(b,p,q,i) * wm_interm_55_triplet_pt2(a,b)
term(282) = term(282) + r1(vrdav_Rr, a,i) * s2(b,p,q,i) * wm_interm_60_triplet_pt2(a,b)
term(283) = term(283) + r1(vrdav_Rr, a,i) * s2(b,p,q,i) * wm_interm_62_triplet_pt2(a,b)
term(284) = term(284) + r1(vrdav_Rr, a,i) * s2(b,p,q,i) * wm_interm_72_triplet_pt2(a,b)
term(285) = term(285) + r1(vrdav_Rr, a,i) * s2(b,p,q,i) * wm_interm_76_triplet_pt2(a,b)
end do 
end do 
end do 

term(281) = term(281) * (-1.0d+0) 
term(282) = term(282) * (2.0d+0) 
term(283) = term(283) * (-1.0d+0) 
term(284) = term(284) * (-4.0d+0) 
term(285) = term(285) * (4.0d+0) 

do j = 1, nocc 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(286) = term(286) + r2p(vrdav_Rl, p,i,a,j) * wm_interm_184_triplet_pt2(a,j,i,q)
term(287) = term(287) + r2p(vrdav_Rl, p,i,a,j) * wm_interm_184_triplet_pt2(a,i,j,q)
term(288) = term(288) + r2p(vrdav_Rl, p,i,a,j) * wm_interm_185_triplet_pt2(a,i,j,q)
term(289) = term(289) + r2p(vrdav_Rl, p,i,a,j) * wm_interm_186_triplet_pt2(a,i,j,q)
term(290) = term(290) + r2p(vrdav_Rl, p,i,a,j) * wm_interm_186_triplet_pt2(a,j,i,q)
term(291) = term(291) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_187_triplet_pt2(a,j,i,q)
term(292) = term(292) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_187_triplet_pt2(a,i,j,q)
term(293) = term(293) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_188_triplet_pt2(a,i,j,q)
term(294) = term(294) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_189_triplet_pt2(a,j,i,q)
term(295) = term(295) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_189_triplet_pt2(a,i,j,q)
term(296) = term(296) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_190_triplet_pt2(a,i,j,q)
end do 
end do 
end do 

term(286) = term(286) * (-0.5d+0) 
term(288) = term(288) * (-0.5d+0) 
term(289) = term(289) * (-2.0d+0) 
term(290) = term(290) * (2.0d+0) 
term(291) = term(291) * (-0.5d+0) 
term(293) = term(293) * (-0.5d+0) 
term(294) = term(294) * (-1.0d+0) 
term(295) = term(295) * (2.0d+0) 
term(296) = term(296) * (-1.0d+0) 

do a = nocc + 1, nactive 
do i = 1, nocc 
do b = nocc + 1, nactive 
term(297) = term(297) + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, b,i,p,q) * wm_interm_4_triplet_pt2(b,a)
term(298) = term(298) + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, b,i,p,q) * wm_interm_5_triplet_pt2(b,a)
term(299) = term(299) + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, b,i,p,q) * wm_interm_4_triplet_pt2(b,a)
term(300) = term(300) + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, b,i,p,q) * wm_interm_5_triplet_pt2(b,a)
term(301) = term(301) + r2p(vrdav_Rl, p,i,a,q) * r1(vrdav_Rr, b,i) * wm_interm_4_triplet_pt2(b,a)
term(302) = term(302) + r2p(vrdav_Rl, p,i,a,q) * r1(vrdav_Rr, b,i) * wm_interm_5_triplet_pt2(b,a)
term(303) = term(303) + r2p(vrdav_Rl, a,i,p,q) * r1(vrdav_Rr, b,i) * wm_interm_4_triplet_pt2(b,a)
term(304) = term(304) + r2p(vrdav_Rl, a,i,p,q) * r1(vrdav_Rr, b,i) * wm_interm_5_triplet_pt2(b,a)
term(305) = term(305) + r2m(vrdav_Rl, a,i,p,q) * r1(vrdav_Rr, b,i) * wm_interm_4_triplet_pt2(b,a)
term(306) = term(306) + r2m(vrdav_Rl, a,i,p,q) * r1(vrdav_Rr, b,i) * wm_interm_5_triplet_pt2(b,a)
end do 
end do 
end do 

term(297) = term(297) * (2.0d+0) 
term(298) = term(298) * (-4.0d+0) 
term(299) = term(299) * (4.0d+0) 
term(300) = term(300) * (-8.0d+0) 
term(301) = term(301) * (-1.0d+0) 
term(302) = term(302) * (2.0d+0) 
term(303) = term(303) * (2.0d+0) 
term(304) = term(304) * (-4.0d+0) 
term(305) = term(305) * (4.0d+0) 
term(306) = term(306) * (-8.0d+0) 

do a = nocc + 1, nactive 
do i = 1, nocc 
term(307) = term(307) + r2p(vrdav_Rl, p,i,a,q) * wm_interm_78_triplet_pt2(a,i)
term(308) = term(308) + r2p(vrdav_Rl, p,i,a,q) * wm_interm_79_triplet_pt2(a,i)
term(309) = term(309) + r2p(vrdav_Rl, p,i,a,q) * wm_interm_80_triplet_pt2(a,i)
term(310) = term(310) + r2p(vrdav_Rl, p,i,a,q) * wm_interm_81_triplet_pt2(a,i)
term(311) = term(311) + r2p(vrdav_Rl, p,i,a,q) * wm_interm_82_triplet_pt2(a,i)
term(312) = term(312) + r2p(vrdav_Rr, p,i,a,q) * wm_interm_83_triplet_pt2(a,i)
term(313) = term(313) + r2p(vrdav_Rr, p,i,a,q) * wm_interm_84_triplet_pt2(a,i)
term(314) = term(314) + r2p(vrdav_Rr, p,i,a,q) * wm_interm_85_triplet_pt2(a,i)
term(315) = term(315) + r2p(vrdav_Rr, p,i,a,q) * wm_interm_86_triplet_pt2(a,i)
term(316) = term(316) + r2p(vrdav_Rr, p,i,a,q) * wm_interm_87_triplet_pt2(a,i)
end do 
end do 

term(307) = term(307) * (-1.0d+0) 
term(308) = term(308) * (0.5d+0) 
term(309) = term(309) * (0.5d+0) 
term(310) = term(310) * (-2.0d+0) 
term(311) = term(311) * (2.0d+0) 
term(313) = term(313) * (-0.5d+0) 
term(314) = term(314) * (-0.5d+0) 
term(315) = term(315) * (2.0d+0) 
term(316) = term(316) * (-2.0d+0) 

do i = 1, nocc 
term(317) = term(317) + wm_interm_13_triplet_pt2(p,i) * wm_interm_1_triplet_pt2(i,q)
term(318) = term(318) + wm_interm_13_triplet_pt2(p,i) * wm_interm_2_triplet_pt2(i,q)
term(319) = term(319) + wm_interm_1_triplet_pt2(i,q) * wm_interm_21_triplet_pt2(p,i)
term(320) = term(320) + wm_interm_1_triplet_pt2(i,q) * wm_interm_22_triplet_pt2(p,i)
term(321) = term(321) + wm_interm_21_triplet_pt2(p,i) * wm_interm_2_triplet_pt2(i,q)
term(322) = term(322) + wm_interm_22_triplet_pt2(p,i) * wm_interm_2_triplet_pt2(i,q)
term(323) = term(323) + wm_interm_1_triplet_pt2(i,q) * wm_interm_33_triplet_pt2(p,i)
term(324) = term(324) + wm_interm_2_triplet_pt2(i,q) * wm_interm_33_triplet_pt2(p,i)
term(325) = term(325) + wm_interm_1_triplet_pt2(i,q) * wm_interm_38_triplet_pt2(p,i)
term(326) = term(326) + wm_interm_2_triplet_pt2(i,q) * wm_interm_38_triplet_pt2(p,i)
term(327) = term(327) + wm_interm_54_triplet_pt2(p,i) * wm_interm_57_triplet_pt2(q,i)
term(328) = term(328) + wm_interm_54_triplet_pt2(p,i) * wm_interm_61_triplet_pt2(q,i)
term(329) = term(329) + wm_interm_54_triplet_pt2(p,i) * wm_interm_64_triplet_pt2(q,i)
term(330) = term(330) + wm_interm_54_triplet_pt2(p,i) * wm_interm_74_triplet_pt2(q,i)
term(331) = term(331) + wm_interm_54_triplet_pt2(p,i) * wm_interm_77_triplet_pt2(q,i)
term(332) = term(332) + r1(vrdav_Rl, p,i) * wm_interm_90_triplet_pt2(i,q)
term(333) = term(333) + r1(vrdav_Rr, p,i) * wm_interm_91_triplet_pt2(i,q)
end do 

term(317) = term(317) * (-1.0d+0) 
term(318) = term(318) * (2.0d+0) 
term(319) = term(319) * (-1.0d+0) 
term(320) = term(320) * (2.0d+0) 
term(321) = term(321) * (2.0d+0) 
term(322) = term(322) * (-4.0d+0) 
term(323) = term(323) * (-4.0d+0) 
term(324) = term(324) * (8.0d+0) 
term(325) = term(325) * (4.0d+0) 
term(326) = term(326) * (-8.0d+0) 
term(327) = term(327) * (-1.0d+0) 
term(328) = term(328) * (2.0d+0) 
term(329) = term(329) * (-1.0d+0) 
term(330) = term(330) * (-4.0d+0) 
term(331) = term(331) * (4.0d+0) 
term(332) = term(332) * (2.0d+0) 
term(333) = term(333) * (2.0d+0) 


    calc_D_vo_wm_triplet_pt2 = zero
    do s = 0, 333
    calc_D_vo_wm_triplet_pt2 = calc_D_vo_wm_triplet_pt2 + term(s)
    end do

    end function calc_D_vo_wm_triplet_pt2
    
    function calc_D_vv_wm_triplet_pt2(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vv_wm_triplet_pt2
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
    real(F64), dimension(0:1086) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + wm_interm_26_triplet_pt2(q,a,i,j) * wm_interm_92_triplet_pt2(a,p,j,i)
term(1) = term(1) + wm_interm_26_triplet_pt2(a,q,i,j) * wm_interm_92_triplet_pt2(p,a,j,i)
term(2) = term(2) + wm_interm_47_triplet_pt2(q,a,i,j) * wm_interm_7_triplet_pt2(p,a,i,j)
term(3) = term(3) + wm_interm_15_triplet_pt2(p,a,i,j) * wm_interm_47_triplet_pt2(q,a,i,j)
term(4) = term(4) + wm_interm_18_triplet_pt2(p,a,i,j) * wm_interm_47_triplet_pt2(q,a,i,j)
term(5) = term(5) + wm_interm_20_triplet_pt2(p,a,i,j) * wm_interm_47_triplet_pt2(q,a,i,j)
term(6) = term(6) + wm_interm_101_triplet_pt2(a,p,i,j) * wm_interm_25_triplet_pt2(q,a,j,i)
term(7) = term(7) + wm_interm_101_triplet_pt2(a,p,i,j) * wm_interm_28_triplet_pt2(q,a,j,i)
term(8) = term(8) + wm_interm_102_triplet_pt2(a,p,i,j) * wm_interm_25_triplet_pt2(q,a,j,i)
term(9) = term(9) + wm_interm_103_triplet_pt2(a,p,i,j) * wm_interm_25_triplet_pt2(q,a,j,i)
term(10) = term(10) + wm_interm_102_triplet_pt2(a,p,i,j) * wm_interm_28_triplet_pt2(q,a,j,i)
term(11) = term(11) + wm_interm_103_triplet_pt2(a,p,i,j) * wm_interm_28_triplet_pt2(q,a,j,i)
term(12) = term(12) + wm_interm_47_triplet_pt2(q,a,i,j) * wm_interm_8_triplet_pt2(p,a,i,j)
term(13) = term(13) + wm_interm_24_triplet_pt2(p,a,i,j) * wm_interm_47_triplet_pt2(q,a,i,j)
term(14) = term(14) + wm_interm_50_triplet_pt2(q,a,i,j) * wm_interm_7_triplet_pt2(p,a,i,j)
term(15) = term(15) + wm_interm_15_triplet_pt2(p,a,i,j) * wm_interm_50_triplet_pt2(q,a,i,j)
term(16) = term(16) + wm_interm_18_triplet_pt2(p,a,i,j) * wm_interm_50_triplet_pt2(q,a,i,j)
term(17) = term(17) + wm_interm_20_triplet_pt2(p,a,i,j) * wm_interm_50_triplet_pt2(q,a,i,j)
term(18) = term(18) + wm_interm_114_triplet_pt2(a,p,i,j) * wm_interm_25_triplet_pt2(q,a,j,i)
term(19) = term(19) + wm_interm_114_triplet_pt2(a,p,i,j) * wm_interm_28_triplet_pt2(q,a,j,i)
term(20) = term(20) + wm_interm_106_triplet_pt2(a,p,i,j) * wm_interm_25_triplet_pt2(q,a,j,i)
term(21) = term(21) + wm_interm_105_triplet_pt2(a,p,i,j) * wm_interm_25_triplet_pt2(q,a,j,i)
term(22) = term(22) + wm_interm_106_triplet_pt2(a,p,i,j) * wm_interm_28_triplet_pt2(q,a,j,i)
term(23) = term(23) + wm_interm_105_triplet_pt2(a,p,i,j) * wm_interm_28_triplet_pt2(q,a,j,i)
term(24) = term(24) + wm_interm_50_triplet_pt2(q,a,i,j) * wm_interm_8_triplet_pt2(p,a,i,j)
term(25) = term(25) + wm_interm_24_triplet_pt2(p,a,i,j) * wm_interm_50_triplet_pt2(q,a,i,j)
term(26) = term(26) + wm_interm_52_triplet_pt2(q,a,i,j) * wm_interm_7_triplet_pt2(p,a,i,j)
term(27) = term(27) + wm_interm_15_triplet_pt2(p,a,i,j) * wm_interm_52_triplet_pt2(q,a,i,j)
term(28) = term(28) + wm_interm_18_triplet_pt2(p,a,i,j) * wm_interm_52_triplet_pt2(q,a,i,j)
term(29) = term(29) + wm_interm_20_triplet_pt2(p,a,i,j) * wm_interm_52_triplet_pt2(q,a,i,j)
term(30) = term(30) + wm_interm_100_triplet_pt2(a,p,i,j) * wm_interm_25_triplet_pt2(q,a,j,i)
term(31) = term(31) + wm_interm_100_triplet_pt2(a,p,i,j) * wm_interm_28_triplet_pt2(q,a,j,i)
term(32) = term(32) + wm_interm_118_triplet_pt2(a,p,i,j) * wm_interm_25_triplet_pt2(q,a,j,i)
term(33) = term(33) + wm_interm_104_triplet_pt2(a,p,i,j) * wm_interm_25_triplet_pt2(q,a,j,i)
term(34) = term(34) + wm_interm_118_triplet_pt2(a,p,i,j) * wm_interm_28_triplet_pt2(q,a,j,i)
term(35) = term(35) + wm_interm_104_triplet_pt2(a,p,i,j) * wm_interm_28_triplet_pt2(q,a,j,i)
term(36) = term(36) + wm_interm_52_triplet_pt2(q,a,i,j) * wm_interm_8_triplet_pt2(p,a,i,j)
term(37) = term(37) + wm_interm_24_triplet_pt2(p,a,i,j) * wm_interm_52_triplet_pt2(q,a,i,j)
term(38) = term(38) + wm_interm_18_triplet_pt2(p,a,i,j) * wm_interm_53_triplet_pt2(q,a,i,j)
term(39) = term(39) + wm_interm_20_triplet_pt2(p,a,i,j) * wm_interm_53_triplet_pt2(q,a,i,j)
term(40) = term(40) + wm_interm_100_triplet_pt2(a,p,i,j) * wm_interm_27_triplet_pt2(q,a,j,i)
term(41) = term(41) + wm_interm_15_triplet_pt2(p,a,i,j) * wm_interm_53_triplet_pt2(q,a,i,j)
term(42) = term(42) + wm_interm_26_triplet_pt2(q,a,i,j) * wm_interm_99_triplet_pt2(a,p,j,i)
term(43) = term(43) + wm_interm_100_triplet_pt2(a,p,i,j) * wm_interm_26_triplet_pt2(q,a,j,i)
term(44) = term(44) + wm_interm_56_triplet_pt2(q,a,i,j) * wm_interm_7_triplet_pt2(p,a,i,j)
term(45) = term(45) + wm_interm_15_triplet_pt2(p,a,i,j) * wm_interm_56_triplet_pt2(q,a,i,j)
term(46) = term(46) + wm_interm_18_triplet_pt2(p,a,i,j) * wm_interm_56_triplet_pt2(q,a,i,j)
term(47) = term(47) + wm_interm_20_triplet_pt2(p,a,i,j) * wm_interm_56_triplet_pt2(q,a,i,j)
term(48) = term(48) + wm_interm_101_triplet_pt2(a,p,i,j) * wm_interm_26_triplet_pt2(q,a,j,i)
term(49) = term(49) + wm_interm_101_triplet_pt2(a,p,i,j) * wm_interm_27_triplet_pt2(q,a,j,i)
term(50) = term(50) + wm_interm_118_triplet_pt2(a,p,i,j) * wm_interm_27_triplet_pt2(q,a,j,i)
term(51) = term(51) + wm_interm_104_triplet_pt2(a,p,i,j) * wm_interm_27_triplet_pt2(q,a,j,i)
term(52) = term(52) + wm_interm_24_triplet_pt2(p,a,i,j) * wm_interm_53_triplet_pt2(q,a,i,j)
term(53) = term(53) + wm_interm_104_triplet_pt2(a,p,i,j) * wm_interm_26_triplet_pt2(q,a,j,i)
term(54) = term(54) + wm_interm_53_triplet_pt2(q,a,i,j) * wm_interm_9_triplet_pt2(p,a,i,j)
term(55) = term(55) + wm_interm_53_triplet_pt2(q,a,i,j) * wm_interm_8_triplet_pt2(p,a,i,j)
term(56) = term(56) + wm_interm_102_triplet_pt2(a,p,i,j) * wm_interm_26_triplet_pt2(q,a,j,i)
term(57) = term(57) + wm_interm_103_triplet_pt2(a,p,i,j) * wm_interm_26_triplet_pt2(q,a,j,i)
term(58) = term(58) + wm_interm_102_triplet_pt2(a,p,i,j) * wm_interm_27_triplet_pt2(q,a,j,i)
term(59) = term(59) + wm_interm_103_triplet_pt2(a,p,i,j) * wm_interm_27_triplet_pt2(q,a,j,i)
term(60) = term(60) + wm_interm_56_triplet_pt2(q,a,i,j) * wm_interm_8_triplet_pt2(p,a,i,j)
term(61) = term(61) + wm_interm_24_triplet_pt2(p,a,i,j) * wm_interm_56_triplet_pt2(q,a,i,j)
term(62) = term(62) + wm_interm_18_triplet_pt2(p,a,i,j) * wm_interm_59_triplet_pt2(q,a,i,j)
term(63) = term(63) + wm_interm_20_triplet_pt2(p,a,i,j) * wm_interm_59_triplet_pt2(q,a,i,j)
term(64) = term(64) + wm_interm_15_triplet_pt2(p,a,i,j) * wm_interm_59_triplet_pt2(q,a,i,j)
term(65) = term(65) + wm_interm_59_triplet_pt2(q,a,i,j) * wm_interm_7_triplet_pt2(p,a,i,j)
term(66) = term(66) + wm_interm_107_triplet_pt2(a,p,i,j) * wm_interm_26_triplet_pt2(q,a,j,i)
term(67) = term(67) + wm_interm_114_triplet_pt2(a,p,i,j) * wm_interm_27_triplet_pt2(q,a,j,i)
term(68) = term(68) + wm_interm_106_triplet_pt2(a,p,i,j) * wm_interm_27_triplet_pt2(q,a,j,i)
term(69) = term(69) + wm_interm_105_triplet_pt2(a,p,i,j) * wm_interm_27_triplet_pt2(q,a,j,i)
term(70) = term(70) + wm_interm_105_triplet_pt2(a,p,i,j) * wm_interm_26_triplet_pt2(q,a,j,i)
term(71) = term(71) + wm_interm_106_triplet_pt2(a,p,i,j) * wm_interm_26_triplet_pt2(q,a,j,i)
term(72) = term(72) + wm_interm_59_triplet_pt2(q,a,i,j) * wm_interm_9_triplet_pt2(p,a,i,j)
term(73) = term(73) + wm_interm_24_triplet_pt2(p,a,i,j) * wm_interm_59_triplet_pt2(q,a,i,j)
term(74) = term(74) + wm_interm_63_triplet_pt2(q,a,i,j) * wm_interm_7_triplet_pt2(p,a,i,j)
term(75) = term(75) + wm_interm_115_triplet_pt2(a,p,i,j) * wm_interm_26_triplet_pt2(q,a,j,i)
term(76) = term(76) + wm_interm_116_triplet_pt2(a,p,i,j) * wm_interm_26_triplet_pt2(q,a,j,i)
term(77) = term(77) + wm_interm_117_triplet_pt2(a,p,i,j) * wm_interm_26_triplet_pt2(q,a,j,i)
term(78) = term(78) + wm_interm_63_triplet_pt2(q,a,i,j) * wm_interm_8_triplet_pt2(p,a,i,j)
term(79) = term(79) + wm_interm_63_triplet_pt2(q,a,i,j) * wm_interm_9_triplet_pt2(p,a,i,j)
term(80) = term(80) + wm_interm_11_triplet_pt2(p,a,i,j) * wm_interm_47_triplet_pt2(q,a,i,j)
term(81) = term(81) + wm_interm_12_triplet_pt2(p,a,i,j) * wm_interm_47_triplet_pt2(q,a,i,j)
term(82) = term(82) + wm_interm_35_triplet_pt2(p,a,i,j) * wm_interm_47_triplet_pt2(q,a,i,j)
term(83) = term(83) + wm_interm_37_triplet_pt2(p,a,i,j) * wm_interm_47_triplet_pt2(q,a,i,j)
term(84) = term(84) + wm_interm_127_triplet_pt2(a,p,i,j) * wm_interm_25_triplet_pt2(q,a,j,i)
term(85) = term(85) + wm_interm_127_triplet_pt2(a,p,i,j) * wm_interm_28_triplet_pt2(q,a,j,i)
term(86) = term(86) + wm_interm_128_triplet_pt2(a,p,i,j) * wm_interm_25_triplet_pt2(q,a,j,i)
term(87) = term(87) + wm_interm_128_triplet_pt2(a,p,i,j) * wm_interm_28_triplet_pt2(q,a,j,i)
term(88) = term(88) + wm_interm_11_triplet_pt2(p,a,i,j) * wm_interm_50_triplet_pt2(q,a,i,j)
term(89) = term(89) + wm_interm_12_triplet_pt2(p,a,i,j) * wm_interm_50_triplet_pt2(q,a,i,j)
term(90) = term(90) + wm_interm_35_triplet_pt2(p,a,i,j) * wm_interm_50_triplet_pt2(q,a,i,j)
term(91) = term(91) + wm_interm_37_triplet_pt2(p,a,i,j) * wm_interm_50_triplet_pt2(q,a,i,j)
term(92) = term(92) + wm_interm_130_triplet_pt2(a,p,i,j) * wm_interm_25_triplet_pt2(q,a,j,i)
term(93) = term(93) + wm_interm_130_triplet_pt2(a,p,i,j) * wm_interm_28_triplet_pt2(q,a,j,i)
term(94) = term(94) + wm_interm_129_triplet_pt2(a,p,i,j) * wm_interm_25_triplet_pt2(q,a,j,i)
term(95) = term(95) + wm_interm_129_triplet_pt2(a,p,i,j) * wm_interm_28_triplet_pt2(q,a,j,i)
term(96) = term(96) + wm_interm_11_triplet_pt2(p,a,i,j) * wm_interm_52_triplet_pt2(q,a,i,j)
term(97) = term(97) + wm_interm_12_triplet_pt2(p,a,i,j) * wm_interm_52_triplet_pt2(q,a,i,j)
term(98) = term(98) + wm_interm_35_triplet_pt2(p,a,i,j) * wm_interm_52_triplet_pt2(q,a,i,j)
term(99) = term(99) + wm_interm_37_triplet_pt2(p,a,i,j) * wm_interm_52_triplet_pt2(q,a,i,j)
term(100) = term(100) + wm_interm_126_triplet_pt2(a,p,i,j) * wm_interm_25_triplet_pt2(q,a,j,i)
term(101) = term(101) + wm_interm_126_triplet_pt2(a,p,i,j) * wm_interm_28_triplet_pt2(q,a,j,i)
term(102) = term(102) + wm_interm_125_triplet_pt2(a,p,i,j) * wm_interm_25_triplet_pt2(q,a,j,i)
term(103) = term(103) + wm_interm_125_triplet_pt2(a,p,i,j) * wm_interm_28_triplet_pt2(q,a,j,i)
term(104) = term(104) + wm_interm_35_triplet_pt2(p,a,i,j) * wm_interm_53_triplet_pt2(q,a,i,j)
term(105) = term(105) + wm_interm_37_triplet_pt2(p,a,i,j) * wm_interm_53_triplet_pt2(q,a,i,j)
term(106) = term(106) + wm_interm_126_triplet_pt2(a,p,i,j) * wm_interm_27_triplet_pt2(q,a,j,i)
term(107) = term(107) + wm_interm_12_triplet_pt2(p,a,i,j) * wm_interm_53_triplet_pt2(q,a,i,j)
term(108) = term(108) + wm_interm_125_triplet_pt2(a,p,i,j) * wm_interm_26_triplet_pt2(q,a,j,i)
term(109) = term(109) + wm_interm_126_triplet_pt2(a,p,i,j) * wm_interm_26_triplet_pt2(q,a,j,i)
term(110) = term(110) + wm_interm_11_triplet_pt2(p,a,i,j) * wm_interm_56_triplet_pt2(q,a,i,j)
term(111) = term(111) + wm_interm_12_triplet_pt2(p,a,i,j) * wm_interm_56_triplet_pt2(q,a,i,j)
term(112) = term(112) + wm_interm_35_triplet_pt2(p,a,i,j) * wm_interm_56_triplet_pt2(q,a,i,j)
term(113) = term(113) + wm_interm_37_triplet_pt2(p,a,i,j) * wm_interm_56_triplet_pt2(q,a,i,j)
term(114) = term(114) + wm_interm_127_triplet_pt2(a,p,i,j) * wm_interm_26_triplet_pt2(q,a,j,i)
term(115) = term(115) + wm_interm_127_triplet_pt2(a,p,i,j) * wm_interm_27_triplet_pt2(q,a,j,i)
term(116) = term(116) + wm_interm_125_triplet_pt2(a,p,i,j) * wm_interm_27_triplet_pt2(q,a,j,i)
term(117) = term(117) + wm_interm_11_triplet_pt2(p,a,i,j) * wm_interm_53_triplet_pt2(q,a,i,j)
term(118) = term(118) + wm_interm_128_triplet_pt2(a,p,i,j) * wm_interm_26_triplet_pt2(q,a,j,i)
term(119) = term(119) + wm_interm_128_triplet_pt2(a,p,i,j) * wm_interm_27_triplet_pt2(q,a,j,i)
term(120) = term(120) + wm_interm_35_triplet_pt2(p,a,i,j) * wm_interm_59_triplet_pt2(q,a,i,j)
term(121) = term(121) + wm_interm_37_triplet_pt2(p,a,i,j) * wm_interm_59_triplet_pt2(q,a,i,j)
term(122) = term(122) + wm_interm_12_triplet_pt2(p,a,i,j) * wm_interm_59_triplet_pt2(q,a,i,j)
term(123) = term(123) + wm_interm_11_triplet_pt2(p,a,i,j) * wm_interm_59_triplet_pt2(q,a,i,j)
term(124) = term(124) + wm_interm_129_triplet_pt2(a,p,i,j) * wm_interm_26_triplet_pt2(q,a,j,i)
term(125) = term(125) + wm_interm_130_triplet_pt2(a,p,i,j) * wm_interm_27_triplet_pt2(q,a,j,i)
term(126) = term(126) + wm_interm_129_triplet_pt2(a,p,i,j) * wm_interm_27_triplet_pt2(q,a,j,i)
term(127) = term(127) + wm_interm_130_triplet_pt2(a,p,i,j) * wm_interm_26_triplet_pt2(q,a,j,i)
term(128) = term(128) + wm_interm_11_triplet_pt2(p,a,i,j) * wm_interm_63_triplet_pt2(q,a,i,j)
term(129) = term(129) + wm_interm_135_triplet_pt2(a,p,i,j) * wm_interm_26_triplet_pt2(q,a,j,i)
term(130) = term(130) + wm_interm_136_triplet_pt2(a,p,i,j) * wm_interm_26_triplet_pt2(q,a,j,i)
term(131) = term(131) + wm_interm_12_triplet_pt2(p,a,i,j) * wm_interm_63_triplet_pt2(q,a,i,j)
term(132) = term(132) + wm_interm_100_triplet_pt2(p,a,i,j) * wm_interm_25_triplet_pt2(a,q,j,i)
term(133) = term(133) + wm_interm_114_triplet_pt2(p,a,i,j) * wm_interm_25_triplet_pt2(a,q,j,i)
term(134) = term(134) + wm_interm_101_triplet_pt2(p,a,i,j) * wm_interm_25_triplet_pt2(a,q,j,i)
term(135) = term(135) + wm_interm_118_triplet_pt2(p,a,i,j) * wm_interm_25_triplet_pt2(a,q,j,i)
term(136) = term(136) + wm_interm_106_triplet_pt2(p,a,i,j) * wm_interm_25_triplet_pt2(a,q,j,i)
term(137) = term(137) + wm_interm_102_triplet_pt2(p,a,i,j) * wm_interm_25_triplet_pt2(a,q,j,i)
term(138) = term(138) + wm_interm_104_triplet_pt2(p,a,i,j) * wm_interm_25_triplet_pt2(a,q,j,i)
term(139) = term(139) + wm_interm_105_triplet_pt2(p,a,i,j) * wm_interm_25_triplet_pt2(a,q,j,i)
term(140) = term(140) + wm_interm_103_triplet_pt2(p,a,i,j) * wm_interm_25_triplet_pt2(a,q,j,i)
term(141) = term(141) + wm_interm_115_triplet_pt2(p,a,i,j) * wm_interm_26_triplet_pt2(a,q,j,i)
term(142) = term(142) + wm_interm_116_triplet_pt2(p,a,i,j) * wm_interm_26_triplet_pt2(a,q,j,i)
term(143) = term(143) + wm_interm_107_triplet_pt2(p,a,i,j) * wm_interm_26_triplet_pt2(a,q,j,i)
term(144) = term(144) + wm_interm_26_triplet_pt2(a,q,i,j) * wm_interm_99_triplet_pt2(p,a,j,i)
term(145) = term(145) + wm_interm_100_triplet_pt2(p,a,i,j) * wm_interm_26_triplet_pt2(a,q,j,i)
term(146) = term(146) + wm_interm_101_triplet_pt2(p,a,i,j) * wm_interm_26_triplet_pt2(a,q,j,i)
term(147) = term(147) + wm_interm_105_triplet_pt2(p,a,i,j) * wm_interm_26_triplet_pt2(a,q,j,i)
term(148) = term(148) + wm_interm_106_triplet_pt2(p,a,i,j) * wm_interm_26_triplet_pt2(a,q,j,i)
term(149) = term(149) + wm_interm_117_triplet_pt2(p,a,i,j) * wm_interm_26_triplet_pt2(a,q,j,i)
term(150) = term(150) + wm_interm_102_triplet_pt2(p,a,i,j) * wm_interm_26_triplet_pt2(a,q,j,i)
term(151) = term(151) + wm_interm_103_triplet_pt2(p,a,i,j) * wm_interm_26_triplet_pt2(a,q,j,i)
term(152) = term(152) + wm_interm_104_triplet_pt2(p,a,i,j) * wm_interm_26_triplet_pt2(a,q,j,i)
term(153) = term(153) + wm_interm_114_triplet_pt2(p,a,i,j) * wm_interm_27_triplet_pt2(a,q,j,i)
term(154) = term(154) + wm_interm_100_triplet_pt2(p,a,i,j) * wm_interm_27_triplet_pt2(a,q,j,i)
term(155) = term(155) + wm_interm_100_triplet_pt2(p,a,i,j) * wm_interm_28_triplet_pt2(a,q,j,i)
term(156) = term(156) + wm_interm_114_triplet_pt2(p,a,i,j) * wm_interm_28_triplet_pt2(a,q,j,i)
term(157) = term(157) + wm_interm_101_triplet_pt2(p,a,i,j) * wm_interm_27_triplet_pt2(a,q,j,i)
term(158) = term(158) + wm_interm_101_triplet_pt2(p,a,i,j) * wm_interm_28_triplet_pt2(a,q,j,i)
term(159) = term(159) + wm_interm_106_triplet_pt2(p,a,i,j) * wm_interm_27_triplet_pt2(a,q,j,i)
term(160) = term(160) + wm_interm_118_triplet_pt2(p,a,i,j) * wm_interm_27_triplet_pt2(a,q,j,i)
term(161) = term(161) + wm_interm_118_triplet_pt2(p,a,i,j) * wm_interm_28_triplet_pt2(a,q,j,i)
term(162) = term(162) + wm_interm_106_triplet_pt2(p,a,i,j) * wm_interm_28_triplet_pt2(a,q,j,i)
term(163) = term(163) + wm_interm_102_triplet_pt2(p,a,i,j) * wm_interm_27_triplet_pt2(a,q,j,i)
term(164) = term(164) + wm_interm_102_triplet_pt2(p,a,i,j) * wm_interm_28_triplet_pt2(a,q,j,i)
term(165) = term(165) + wm_interm_105_triplet_pt2(p,a,i,j) * wm_interm_27_triplet_pt2(a,q,j,i)
term(166) = term(166) + wm_interm_104_triplet_pt2(p,a,i,j) * wm_interm_27_triplet_pt2(a,q,j,i)
term(167) = term(167) + wm_interm_104_triplet_pt2(p,a,i,j) * wm_interm_28_triplet_pt2(a,q,j,i)
term(168) = term(168) + wm_interm_105_triplet_pt2(p,a,i,j) * wm_interm_28_triplet_pt2(a,q,j,i)
term(169) = term(169) + wm_interm_103_triplet_pt2(p,a,i,j) * wm_interm_27_triplet_pt2(a,q,j,i)
term(170) = term(170) + wm_interm_103_triplet_pt2(p,a,i,j) * wm_interm_28_triplet_pt2(a,q,j,i)
term(171) = term(171) + wm_interm_24_triplet_pt2(a,p,i,j) * wm_interm_53_triplet_pt2(a,q,i,j)
term(172) = term(172) + wm_interm_24_triplet_pt2(a,p,i,j) * wm_interm_59_triplet_pt2(a,q,i,j)
term(173) = term(173) + wm_interm_24_triplet_pt2(a,p,i,j) * wm_interm_56_triplet_pt2(a,q,i,j)
term(174) = term(174) + wm_interm_18_triplet_pt2(a,p,i,j) * wm_interm_53_triplet_pt2(a,q,i,j)
term(175) = term(175) + wm_interm_18_triplet_pt2(a,p,i,j) * wm_interm_59_triplet_pt2(a,q,i,j)
term(176) = term(176) + wm_interm_18_triplet_pt2(a,p,i,j) * wm_interm_56_triplet_pt2(a,q,i,j)
term(177) = term(177) + wm_interm_20_triplet_pt2(a,p,i,j) * wm_interm_53_triplet_pt2(a,q,i,j)
term(178) = term(178) + wm_interm_20_triplet_pt2(a,p,i,j) * wm_interm_59_triplet_pt2(a,q,i,j)
term(179) = term(179) + wm_interm_20_triplet_pt2(a,p,i,j) * wm_interm_56_triplet_pt2(a,q,i,j)
term(180) = term(180) + wm_interm_63_triplet_pt2(a,q,i,j) * wm_interm_8_triplet_pt2(a,p,i,j)
term(181) = term(181) + wm_interm_63_triplet_pt2(a,q,i,j) * wm_interm_9_triplet_pt2(a,p,i,j)
term(182) = term(182) + wm_interm_59_triplet_pt2(a,q,i,j) * wm_interm_9_triplet_pt2(a,p,i,j)
term(183) = term(183) + wm_interm_53_triplet_pt2(a,q,i,j) * wm_interm_9_triplet_pt2(a,p,i,j)
term(184) = term(184) + wm_interm_53_triplet_pt2(a,q,i,j) * wm_interm_8_triplet_pt2(a,p,i,j)
term(185) = term(185) + wm_interm_56_triplet_pt2(a,q,i,j) * wm_interm_8_triplet_pt2(a,p,i,j)
term(186) = term(186) + wm_interm_15_triplet_pt2(a,p,i,j) * wm_interm_59_triplet_pt2(a,q,i,j)
term(187) = term(187) + wm_interm_59_triplet_pt2(a,q,i,j) * wm_interm_7_triplet_pt2(a,p,i,j)
term(188) = term(188) + wm_interm_63_triplet_pt2(a,q,i,j) * wm_interm_7_triplet_pt2(a,p,i,j)
term(189) = term(189) + wm_interm_56_triplet_pt2(a,q,i,j) * wm_interm_7_triplet_pt2(a,p,i,j)
term(190) = term(190) + wm_interm_15_triplet_pt2(a,p,i,j) * wm_interm_56_triplet_pt2(a,q,i,j)
term(191) = term(191) + wm_interm_15_triplet_pt2(a,p,i,j) * wm_interm_53_triplet_pt2(a,q,i,j)
term(192) = term(192) + wm_interm_52_triplet_pt2(a,q,i,j) * wm_interm_8_triplet_pt2(a,p,i,j)
term(193) = term(193) + wm_interm_50_triplet_pt2(a,q,i,j) * wm_interm_8_triplet_pt2(a,p,i,j)
term(194) = term(194) + wm_interm_47_triplet_pt2(a,q,i,j) * wm_interm_8_triplet_pt2(a,p,i,j)
term(195) = term(195) + wm_interm_52_triplet_pt2(a,q,i,j) * wm_interm_7_triplet_pt2(a,p,i,j)
term(196) = term(196) + wm_interm_50_triplet_pt2(a,q,i,j) * wm_interm_7_triplet_pt2(a,p,i,j)
term(197) = term(197) + wm_interm_47_triplet_pt2(a,q,i,j) * wm_interm_7_triplet_pt2(a,p,i,j)
term(198) = term(198) + wm_interm_15_triplet_pt2(a,p,i,j) * wm_interm_52_triplet_pt2(a,q,i,j)
term(199) = term(199) + wm_interm_15_triplet_pt2(a,p,i,j) * wm_interm_50_triplet_pt2(a,q,i,j)
term(200) = term(200) + wm_interm_15_triplet_pt2(a,p,i,j) * wm_interm_47_triplet_pt2(a,q,i,j)
term(201) = term(201) + wm_interm_24_triplet_pt2(a,p,i,j) * wm_interm_52_triplet_pt2(a,q,i,j)
term(202) = term(202) + wm_interm_24_triplet_pt2(a,p,i,j) * wm_interm_50_triplet_pt2(a,q,i,j)
term(203) = term(203) + wm_interm_24_triplet_pt2(a,p,i,j) * wm_interm_47_triplet_pt2(a,q,i,j)
term(204) = term(204) + wm_interm_18_triplet_pt2(a,p,i,j) * wm_interm_52_triplet_pt2(a,q,i,j)
term(205) = term(205) + wm_interm_18_triplet_pt2(a,p,i,j) * wm_interm_50_triplet_pt2(a,q,i,j)
term(206) = term(206) + wm_interm_18_triplet_pt2(a,p,i,j) * wm_interm_47_triplet_pt2(a,q,i,j)
term(207) = term(207) + wm_interm_20_triplet_pt2(a,p,i,j) * wm_interm_52_triplet_pt2(a,q,i,j)
term(208) = term(208) + wm_interm_20_triplet_pt2(a,p,i,j) * wm_interm_50_triplet_pt2(a,q,i,j)
term(209) = term(209) + wm_interm_20_triplet_pt2(a,p,i,j) * wm_interm_47_triplet_pt2(a,q,i,j)
term(210) = term(210) + wm_interm_126_triplet_pt2(p,a,i,j) * wm_interm_25_triplet_pt2(a,q,j,i)
term(211) = term(211) + wm_interm_130_triplet_pt2(p,a,i,j) * wm_interm_25_triplet_pt2(a,q,j,i)
term(212) = term(212) + wm_interm_127_triplet_pt2(p,a,i,j) * wm_interm_25_triplet_pt2(a,q,j,i)
term(213) = term(213) + wm_interm_125_triplet_pt2(p,a,i,j) * wm_interm_25_triplet_pt2(a,q,j,i)
term(214) = term(214) + wm_interm_129_triplet_pt2(p,a,i,j) * wm_interm_25_triplet_pt2(a,q,j,i)
term(215) = term(215) + wm_interm_128_triplet_pt2(p,a,i,j) * wm_interm_25_triplet_pt2(a,q,j,i)
term(216) = term(216) + wm_interm_135_triplet_pt2(p,a,i,j) * wm_interm_26_triplet_pt2(a,q,j,i)
term(217) = term(217) + wm_interm_136_triplet_pt2(p,a,i,j) * wm_interm_26_triplet_pt2(a,q,j,i)
term(218) = term(218) + wm_interm_129_triplet_pt2(p,a,i,j) * wm_interm_26_triplet_pt2(a,q,j,i)
term(219) = term(219) + wm_interm_125_triplet_pt2(p,a,i,j) * wm_interm_26_triplet_pt2(a,q,j,i)
term(220) = term(220) + wm_interm_126_triplet_pt2(p,a,i,j) * wm_interm_26_triplet_pt2(a,q,j,i)
term(221) = term(221) + wm_interm_127_triplet_pt2(p,a,i,j) * wm_interm_26_triplet_pt2(a,q,j,i)
term(222) = term(222) + wm_interm_130_triplet_pt2(p,a,i,j) * wm_interm_26_triplet_pt2(a,q,j,i)
term(223) = term(223) + wm_interm_128_triplet_pt2(p,a,i,j) * wm_interm_26_triplet_pt2(a,q,j,i)
term(224) = term(224) + wm_interm_130_triplet_pt2(p,a,i,j) * wm_interm_27_triplet_pt2(a,q,j,i)
term(225) = term(225) + wm_interm_126_triplet_pt2(p,a,i,j) * wm_interm_27_triplet_pt2(a,q,j,i)
term(226) = term(226) + wm_interm_126_triplet_pt2(p,a,i,j) * wm_interm_28_triplet_pt2(a,q,j,i)
term(227) = term(227) + wm_interm_130_triplet_pt2(p,a,i,j) * wm_interm_28_triplet_pt2(a,q,j,i)
term(228) = term(228) + wm_interm_127_triplet_pt2(p,a,i,j) * wm_interm_27_triplet_pt2(a,q,j,i)
term(229) = term(229) + wm_interm_127_triplet_pt2(p,a,i,j) * wm_interm_28_triplet_pt2(a,q,j,i)
term(230) = term(230) + wm_interm_129_triplet_pt2(p,a,i,j) * wm_interm_27_triplet_pt2(a,q,j,i)
term(231) = term(231) + wm_interm_125_triplet_pt2(p,a,i,j) * wm_interm_27_triplet_pt2(a,q,j,i)
term(232) = term(232) + wm_interm_125_triplet_pt2(p,a,i,j) * wm_interm_28_triplet_pt2(a,q,j,i)
term(233) = term(233) + wm_interm_129_triplet_pt2(p,a,i,j) * wm_interm_28_triplet_pt2(a,q,j,i)
term(234) = term(234) + wm_interm_128_triplet_pt2(p,a,i,j) * wm_interm_27_triplet_pt2(a,q,j,i)
term(235) = term(235) + wm_interm_128_triplet_pt2(p,a,i,j) * wm_interm_28_triplet_pt2(a,q,j,i)
term(236) = term(236) + wm_interm_35_triplet_pt2(a,p,i,j) * wm_interm_53_triplet_pt2(a,q,i,j)
term(237) = term(237) + wm_interm_35_triplet_pt2(a,p,i,j) * wm_interm_59_triplet_pt2(a,q,i,j)
term(238) = term(238) + wm_interm_35_triplet_pt2(a,p,i,j) * wm_interm_56_triplet_pt2(a,q,i,j)
term(239) = term(239) + wm_interm_37_triplet_pt2(a,p,i,j) * wm_interm_53_triplet_pt2(a,q,i,j)
term(240) = term(240) + wm_interm_37_triplet_pt2(a,p,i,j) * wm_interm_59_triplet_pt2(a,q,i,j)
term(241) = term(241) + wm_interm_37_triplet_pt2(a,p,i,j) * wm_interm_56_triplet_pt2(a,q,i,j)
term(242) = term(242) + wm_interm_11_triplet_pt2(a,p,i,j) * wm_interm_63_triplet_pt2(a,q,i,j)
term(243) = term(243) + wm_interm_12_triplet_pt2(a,p,i,j) * wm_interm_63_triplet_pt2(a,q,i,j)
term(244) = term(244) + wm_interm_12_triplet_pt2(a,p,i,j) * wm_interm_59_triplet_pt2(a,q,i,j)
term(245) = term(245) + wm_interm_12_triplet_pt2(a,p,i,j) * wm_interm_53_triplet_pt2(a,q,i,j)
term(246) = term(246) + wm_interm_11_triplet_pt2(a,p,i,j) * wm_interm_53_triplet_pt2(a,q,i,j)
term(247) = term(247) + wm_interm_11_triplet_pt2(a,p,i,j) * wm_interm_56_triplet_pt2(a,q,i,j)
term(248) = term(248) + wm_interm_11_triplet_pt2(a,p,i,j) * wm_interm_59_triplet_pt2(a,q,i,j)
term(249) = term(249) + wm_interm_12_triplet_pt2(a,p,i,j) * wm_interm_56_triplet_pt2(a,q,i,j)
term(250) = term(250) + wm_interm_11_triplet_pt2(a,p,i,j) * wm_interm_52_triplet_pt2(a,q,i,j)
term(251) = term(251) + wm_interm_11_triplet_pt2(a,p,i,j) * wm_interm_50_triplet_pt2(a,q,i,j)
term(252) = term(252) + wm_interm_11_triplet_pt2(a,p,i,j) * wm_interm_47_triplet_pt2(a,q,i,j)
term(253) = term(253) + wm_interm_12_triplet_pt2(a,p,i,j) * wm_interm_52_triplet_pt2(a,q,i,j)
term(254) = term(254) + wm_interm_12_triplet_pt2(a,p,i,j) * wm_interm_50_triplet_pt2(a,q,i,j)
term(255) = term(255) + wm_interm_12_triplet_pt2(a,p,i,j) * wm_interm_47_triplet_pt2(a,q,i,j)
term(256) = term(256) + wm_interm_35_triplet_pt2(a,p,i,j) * wm_interm_52_triplet_pt2(a,q,i,j)
term(257) = term(257) + wm_interm_35_triplet_pt2(a,p,i,j) * wm_interm_50_triplet_pt2(a,q,i,j)
term(258) = term(258) + wm_interm_35_triplet_pt2(a,p,i,j) * wm_interm_47_triplet_pt2(a,q,i,j)
term(259) = term(259) + wm_interm_37_triplet_pt2(a,p,i,j) * wm_interm_52_triplet_pt2(a,q,i,j)
term(260) = term(260) + wm_interm_37_triplet_pt2(a,p,i,j) * wm_interm_50_triplet_pt2(a,q,i,j)
term(261) = term(261) + wm_interm_37_triplet_pt2(a,p,i,j) * wm_interm_47_triplet_pt2(a,q,i,j)
term(262) = term(262) + wm_interm_68_triplet_pt2(q,a,i,j) * wm_interm_7_triplet_pt2(p,a,i,j)
term(263) = term(263) + wm_interm_15_triplet_pt2(p,a,i,j) * wm_interm_68_triplet_pt2(q,a,i,j)
term(264) = term(264) + wm_interm_18_triplet_pt2(p,a,i,j) * wm_interm_68_triplet_pt2(q,a,i,j)
term(265) = term(265) + wm_interm_20_triplet_pt2(p,a,i,j) * wm_interm_68_triplet_pt2(q,a,i,j)
term(266) = term(266) + wm_interm_151_triplet_pt2(a,p,i,j) * wm_interm_25_triplet_pt2(q,a,j,i)
term(267) = term(267) + wm_interm_151_triplet_pt2(a,p,i,j) * wm_interm_28_triplet_pt2(q,a,j,i)
term(268) = term(268) + wm_interm_152_triplet_pt2(a,p,i,j) * wm_interm_25_triplet_pt2(q,a,j,i)
term(269) = term(269) + wm_interm_153_triplet_pt2(a,p,i,j) * wm_interm_25_triplet_pt2(q,a,j,i)
term(270) = term(270) + wm_interm_152_triplet_pt2(a,p,i,j) * wm_interm_28_triplet_pt2(q,a,j,i)
term(271) = term(271) + wm_interm_153_triplet_pt2(a,p,i,j) * wm_interm_28_triplet_pt2(q,a,j,i)
term(272) = term(272) + wm_interm_68_triplet_pt2(q,a,i,j) * wm_interm_8_triplet_pt2(p,a,i,j)
term(273) = term(273) + wm_interm_24_triplet_pt2(p,a,i,j) * wm_interm_68_triplet_pt2(q,a,i,j)
term(274) = term(274) + wm_interm_69_triplet_pt2(q,a,i,j) * wm_interm_7_triplet_pt2(p,a,i,j)
term(275) = term(275) + wm_interm_15_triplet_pt2(p,a,i,j) * wm_interm_69_triplet_pt2(q,a,i,j)
term(276) = term(276) + wm_interm_18_triplet_pt2(p,a,i,j) * wm_interm_69_triplet_pt2(q,a,i,j)
term(277) = term(277) + wm_interm_20_triplet_pt2(p,a,i,j) * wm_interm_69_triplet_pt2(q,a,i,j)
term(278) = term(278) + wm_interm_150_triplet_pt2(a,p,i,j) * wm_interm_25_triplet_pt2(q,a,j,i)
term(279) = term(279) + wm_interm_150_triplet_pt2(a,p,i,j) * wm_interm_28_triplet_pt2(q,a,j,i)
term(280) = term(280) + wm_interm_155_triplet_pt2(a,p,i,j) * wm_interm_25_triplet_pt2(q,a,j,i)
term(281) = term(281) + wm_interm_154_triplet_pt2(a,p,i,j) * wm_interm_25_triplet_pt2(q,a,j,i)
term(282) = term(282) + wm_interm_155_triplet_pt2(a,p,i,j) * wm_interm_28_triplet_pt2(q,a,j,i)
term(283) = term(283) + wm_interm_154_triplet_pt2(a,p,i,j) * wm_interm_28_triplet_pt2(q,a,j,i)
term(284) = term(284) + wm_interm_69_triplet_pt2(q,a,i,j) * wm_interm_8_triplet_pt2(p,a,i,j)
term(285) = term(285) + wm_interm_24_triplet_pt2(p,a,i,j) * wm_interm_69_triplet_pt2(q,a,i,j)
term(286) = term(286) + wm_interm_18_triplet_pt2(p,a,i,j) * wm_interm_71_triplet_pt2(q,a,i,j)
term(287) = term(287) + wm_interm_20_triplet_pt2(p,a,i,j) * wm_interm_71_triplet_pt2(q,a,i,j)
term(288) = term(288) + wm_interm_150_triplet_pt2(a,p,i,j) * wm_interm_27_triplet_pt2(q,a,j,i)
term(289) = term(289) + wm_interm_15_triplet_pt2(p,a,i,j) * wm_interm_71_triplet_pt2(q,a,i,j)
term(290) = term(290) + wm_interm_149_triplet_pt2(a,p,i,j) * wm_interm_26_triplet_pt2(q,a,j,i)
term(291) = term(291) + wm_interm_150_triplet_pt2(a,p,i,j) * wm_interm_26_triplet_pt2(q,a,j,i)
term(292) = term(292) + wm_interm_73_triplet_pt2(q,a,i,j) * wm_interm_7_triplet_pt2(p,a,i,j)
term(293) = term(293) + wm_interm_15_triplet_pt2(p,a,i,j) * wm_interm_73_triplet_pt2(q,a,i,j)
term(294) = term(294) + wm_interm_18_triplet_pt2(p,a,i,j) * wm_interm_73_triplet_pt2(q,a,i,j)
term(295) = term(295) + wm_interm_20_triplet_pt2(p,a,i,j) * wm_interm_73_triplet_pt2(q,a,i,j)
term(296) = term(296) + wm_interm_151_triplet_pt2(a,p,i,j) * wm_interm_26_triplet_pt2(q,a,j,i)
term(297) = term(297) + wm_interm_151_triplet_pt2(a,p,i,j) * wm_interm_27_triplet_pt2(q,a,j,i)
term(298) = term(298) + wm_interm_155_triplet_pt2(a,p,i,j) * wm_interm_27_triplet_pt2(q,a,j,i)
term(299) = term(299) + wm_interm_154_triplet_pt2(a,p,i,j) * wm_interm_27_triplet_pt2(q,a,j,i)
term(300) = term(300) + wm_interm_24_triplet_pt2(p,a,i,j) * wm_interm_71_triplet_pt2(q,a,i,j)
term(301) = term(301) + wm_interm_154_triplet_pt2(a,p,i,j) * wm_interm_26_triplet_pt2(q,a,j,i)
term(302) = term(302) + wm_interm_71_triplet_pt2(q,a,i,j) * wm_interm_9_triplet_pt2(p,a,i,j)
term(303) = term(303) + wm_interm_71_triplet_pt2(q,a,i,j) * wm_interm_8_triplet_pt2(p,a,i,j)
term(304) = term(304) + wm_interm_152_triplet_pt2(a,p,i,j) * wm_interm_26_triplet_pt2(q,a,j,i)
term(305) = term(305) + wm_interm_153_triplet_pt2(a,p,i,j) * wm_interm_26_triplet_pt2(q,a,j,i)
term(306) = term(306) + wm_interm_152_triplet_pt2(a,p,i,j) * wm_interm_27_triplet_pt2(q,a,j,i)
term(307) = term(307) + wm_interm_153_triplet_pt2(a,p,i,j) * wm_interm_27_triplet_pt2(q,a,j,i)
term(308) = term(308) + wm_interm_73_triplet_pt2(q,a,i,j) * wm_interm_8_triplet_pt2(p,a,i,j)
term(309) = term(309) + wm_interm_24_triplet_pt2(p,a,i,j) * wm_interm_73_triplet_pt2(q,a,i,j)
term(310) = term(310) + wm_interm_71_triplet_pt2(q,a,i,j) * wm_interm_7_triplet_pt2(p,a,i,j)
term(311) = term(311) + wm_interm_155_triplet_pt2(a,p,i,j) * wm_interm_26_triplet_pt2(q,a,j,i)
term(312) = term(312) + wm_interm_162_triplet_pt2(a,p,i,j) * wm_interm_26_triplet_pt2(q,a,j,i)
term(313) = term(313) + wm_interm_73_triplet_pt2(q,a,i,j) * wm_interm_9_triplet_pt2(p,a,i,j)
term(314) = term(314) + wm_interm_11_triplet_pt2(p,a,i,j) * wm_interm_68_triplet_pt2(q,a,i,j)
term(315) = term(315) + wm_interm_12_triplet_pt2(p,a,i,j) * wm_interm_68_triplet_pt2(q,a,i,j)
term(316) = term(316) + wm_interm_35_triplet_pt2(p,a,i,j) * wm_interm_68_triplet_pt2(q,a,i,j)
term(317) = term(317) + wm_interm_37_triplet_pt2(p,a,i,j) * wm_interm_68_triplet_pt2(q,a,i,j)
term(318) = term(318) + wm_interm_171_triplet_pt2(a,p,i,j) * wm_interm_25_triplet_pt2(q,a,j,i)
term(319) = term(319) + wm_interm_171_triplet_pt2(a,p,i,j) * wm_interm_28_triplet_pt2(q,a,j,i)
term(320) = term(320) + wm_interm_172_triplet_pt2(a,p,i,j) * wm_interm_25_triplet_pt2(q,a,j,i)
term(321) = term(321) + wm_interm_172_triplet_pt2(a,p,i,j) * wm_interm_28_triplet_pt2(q,a,j,i)
term(322) = term(322) + wm_interm_11_triplet_pt2(p,a,i,j) * wm_interm_69_triplet_pt2(q,a,i,j)
term(323) = term(323) + wm_interm_12_triplet_pt2(p,a,i,j) * wm_interm_69_triplet_pt2(q,a,i,j)
term(324) = term(324) + wm_interm_35_triplet_pt2(p,a,i,j) * wm_interm_69_triplet_pt2(q,a,i,j)
term(325) = term(325) + wm_interm_37_triplet_pt2(p,a,i,j) * wm_interm_69_triplet_pt2(q,a,i,j)
term(326) = term(326) + wm_interm_170_triplet_pt2(a,p,i,j) * wm_interm_25_triplet_pt2(q,a,j,i)
term(327) = term(327) + wm_interm_170_triplet_pt2(a,p,i,j) * wm_interm_28_triplet_pt2(q,a,j,i)
term(328) = term(328) + wm_interm_169_triplet_pt2(a,p,i,j) * wm_interm_25_triplet_pt2(q,a,j,i)
term(329) = term(329) + wm_interm_169_triplet_pt2(a,p,i,j) * wm_interm_28_triplet_pt2(q,a,j,i)
term(330) = term(330) + wm_interm_35_triplet_pt2(p,a,i,j) * wm_interm_71_triplet_pt2(q,a,i,j)
term(331) = term(331) + wm_interm_37_triplet_pt2(p,a,i,j) * wm_interm_71_triplet_pt2(q,a,i,j)
term(332) = term(332) + wm_interm_170_triplet_pt2(a,p,i,j) * wm_interm_27_triplet_pt2(q,a,j,i)
term(333) = term(333) + wm_interm_12_triplet_pt2(p,a,i,j) * wm_interm_71_triplet_pt2(q,a,i,j)
term(334) = term(334) + wm_interm_169_triplet_pt2(a,p,i,j) * wm_interm_26_triplet_pt2(q,a,j,i)
term(335) = term(335) + wm_interm_170_triplet_pt2(a,p,i,j) * wm_interm_26_triplet_pt2(q,a,j,i)
term(336) = term(336) + wm_interm_11_triplet_pt2(p,a,i,j) * wm_interm_73_triplet_pt2(q,a,i,j)
term(337) = term(337) + wm_interm_12_triplet_pt2(p,a,i,j) * wm_interm_73_triplet_pt2(q,a,i,j)
term(338) = term(338) + wm_interm_35_triplet_pt2(p,a,i,j) * wm_interm_73_triplet_pt2(q,a,i,j)
term(339) = term(339) + wm_interm_37_triplet_pt2(p,a,i,j) * wm_interm_73_triplet_pt2(q,a,i,j)
term(340) = term(340) + wm_interm_171_triplet_pt2(a,p,i,j) * wm_interm_26_triplet_pt2(q,a,j,i)
term(341) = term(341) + wm_interm_171_triplet_pt2(a,p,i,j) * wm_interm_27_triplet_pt2(q,a,j,i)
term(342) = term(342) + wm_interm_169_triplet_pt2(a,p,i,j) * wm_interm_27_triplet_pt2(q,a,j,i)
term(343) = term(343) + wm_interm_11_triplet_pt2(p,a,i,j) * wm_interm_71_triplet_pt2(q,a,i,j)
term(344) = term(344) + wm_interm_172_triplet_pt2(a,p,i,j) * wm_interm_26_triplet_pt2(q,a,j,i)
term(345) = term(345) + wm_interm_172_triplet_pt2(a,p,i,j) * wm_interm_27_triplet_pt2(q,a,j,i)
term(346) = term(346) + wm_interm_150_triplet_pt2(p,a,i,j) * wm_interm_25_triplet_pt2(a,q,j,i)
term(347) = term(347) + wm_interm_151_triplet_pt2(p,a,i,j) * wm_interm_25_triplet_pt2(a,q,j,i)
term(348) = term(348) + wm_interm_155_triplet_pt2(p,a,i,j) * wm_interm_25_triplet_pt2(a,q,j,i)
term(349) = term(349) + wm_interm_152_triplet_pt2(p,a,i,j) * wm_interm_25_triplet_pt2(a,q,j,i)
term(350) = term(350) + wm_interm_154_triplet_pt2(p,a,i,j) * wm_interm_25_triplet_pt2(a,q,j,i)
term(351) = term(351) + wm_interm_153_triplet_pt2(p,a,i,j) * wm_interm_25_triplet_pt2(a,q,j,i)
term(352) = term(352) + wm_interm_151_triplet_pt2(p,a,i,j) * wm_interm_26_triplet_pt2(a,q,j,i)
term(353) = term(353) + wm_interm_162_triplet_pt2(p,a,i,j) * wm_interm_26_triplet_pt2(a,q,j,i)
term(354) = term(354) + wm_interm_149_triplet_pt2(p,a,i,j) * wm_interm_26_triplet_pt2(a,q,j,i)
term(355) = term(355) + wm_interm_150_triplet_pt2(p,a,i,j) * wm_interm_26_triplet_pt2(a,q,j,i)
term(356) = term(356) + wm_interm_154_triplet_pt2(p,a,i,j) * wm_interm_26_triplet_pt2(a,q,j,i)
term(357) = term(357) + wm_interm_155_triplet_pt2(p,a,i,j) * wm_interm_26_triplet_pt2(a,q,j,i)
term(358) = term(358) + wm_interm_152_triplet_pt2(p,a,i,j) * wm_interm_26_triplet_pt2(a,q,j,i)
term(359) = term(359) + wm_interm_153_triplet_pt2(p,a,i,j) * wm_interm_26_triplet_pt2(a,q,j,i)
term(360) = term(360) + wm_interm_150_triplet_pt2(p,a,i,j) * wm_interm_27_triplet_pt2(a,q,j,i)
term(361) = term(361) + wm_interm_150_triplet_pt2(p,a,i,j) * wm_interm_28_triplet_pt2(a,q,j,i)
term(362) = term(362) + wm_interm_151_triplet_pt2(p,a,i,j) * wm_interm_27_triplet_pt2(a,q,j,i)
term(363) = term(363) + wm_interm_151_triplet_pt2(p,a,i,j) * wm_interm_28_triplet_pt2(a,q,j,i)
term(364) = term(364) + wm_interm_155_triplet_pt2(p,a,i,j) * wm_interm_27_triplet_pt2(a,q,j,i)
term(365) = term(365) + wm_interm_155_triplet_pt2(p,a,i,j) * wm_interm_28_triplet_pt2(a,q,j,i)
term(366) = term(366) + wm_interm_152_triplet_pt2(p,a,i,j) * wm_interm_27_triplet_pt2(a,q,j,i)
term(367) = term(367) + wm_interm_152_triplet_pt2(p,a,i,j) * wm_interm_28_triplet_pt2(a,q,j,i)
term(368) = term(368) + wm_interm_154_triplet_pt2(p,a,i,j) * wm_interm_27_triplet_pt2(a,q,j,i)
term(369) = term(369) + wm_interm_154_triplet_pt2(p,a,i,j) * wm_interm_28_triplet_pt2(a,q,j,i)
term(370) = term(370) + wm_interm_153_triplet_pt2(p,a,i,j) * wm_interm_27_triplet_pt2(a,q,j,i)
term(371) = term(371) + wm_interm_153_triplet_pt2(p,a,i,j) * wm_interm_28_triplet_pt2(a,q,j,i)
term(372) = term(372) + wm_interm_24_triplet_pt2(a,p,i,j) * wm_interm_71_triplet_pt2(a,q,i,j)
term(373) = term(373) + wm_interm_24_triplet_pt2(a,p,i,j) * wm_interm_73_triplet_pt2(a,q,i,j)
term(374) = term(374) + wm_interm_18_triplet_pt2(a,p,i,j) * wm_interm_71_triplet_pt2(a,q,i,j)
term(375) = term(375) + wm_interm_18_triplet_pt2(a,p,i,j) * wm_interm_73_triplet_pt2(a,q,i,j)
term(376) = term(376) + wm_interm_20_triplet_pt2(a,p,i,j) * wm_interm_71_triplet_pt2(a,q,i,j)
term(377) = term(377) + wm_interm_20_triplet_pt2(a,p,i,j) * wm_interm_73_triplet_pt2(a,q,i,j)
term(378) = term(378) + wm_interm_73_triplet_pt2(a,q,i,j) * wm_interm_8_triplet_pt2(a,p,i,j)
term(379) = term(379) + wm_interm_73_triplet_pt2(a,q,i,j) * wm_interm_9_triplet_pt2(a,p,i,j)
term(380) = term(380) + wm_interm_71_triplet_pt2(a,q,i,j) * wm_interm_9_triplet_pt2(a,p,i,j)
term(381) = term(381) + wm_interm_71_triplet_pt2(a,q,i,j) * wm_interm_8_triplet_pt2(a,p,i,j)
term(382) = term(382) + wm_interm_15_triplet_pt2(a,p,i,j) * wm_interm_71_triplet_pt2(a,q,i,j)
term(383) = term(383) + wm_interm_71_triplet_pt2(a,q,i,j) * wm_interm_7_triplet_pt2(a,p,i,j)
term(384) = term(384) + wm_interm_73_triplet_pt2(a,q,i,j) * wm_interm_7_triplet_pt2(a,p,i,j)
term(385) = term(385) + wm_interm_15_triplet_pt2(a,p,i,j) * wm_interm_73_triplet_pt2(a,q,i,j)
term(386) = term(386) + wm_interm_69_triplet_pt2(a,q,i,j) * wm_interm_8_triplet_pt2(a,p,i,j)
term(387) = term(387) + wm_interm_68_triplet_pt2(a,q,i,j) * wm_interm_8_triplet_pt2(a,p,i,j)
term(388) = term(388) + wm_interm_69_triplet_pt2(a,q,i,j) * wm_interm_7_triplet_pt2(a,p,i,j)
term(389) = term(389) + wm_interm_68_triplet_pt2(a,q,i,j) * wm_interm_7_triplet_pt2(a,p,i,j)
term(390) = term(390) + wm_interm_15_triplet_pt2(a,p,i,j) * wm_interm_69_triplet_pt2(a,q,i,j)
term(391) = term(391) + wm_interm_15_triplet_pt2(a,p,i,j) * wm_interm_68_triplet_pt2(a,q,i,j)
term(392) = term(392) + wm_interm_24_triplet_pt2(a,p,i,j) * wm_interm_69_triplet_pt2(a,q,i,j)
term(393) = term(393) + wm_interm_24_triplet_pt2(a,p,i,j) * wm_interm_68_triplet_pt2(a,q,i,j)
term(394) = term(394) + wm_interm_18_triplet_pt2(a,p,i,j) * wm_interm_69_triplet_pt2(a,q,i,j)
term(395) = term(395) + wm_interm_18_triplet_pt2(a,p,i,j) * wm_interm_68_triplet_pt2(a,q,i,j)
term(396) = term(396) + wm_interm_20_triplet_pt2(a,p,i,j) * wm_interm_69_triplet_pt2(a,q,i,j)
term(397) = term(397) + wm_interm_20_triplet_pt2(a,p,i,j) * wm_interm_68_triplet_pt2(a,q,i,j)
term(398) = term(398) + wm_interm_170_triplet_pt2(p,a,i,j) * wm_interm_25_triplet_pt2(a,q,j,i)
term(399) = term(399) + wm_interm_171_triplet_pt2(p,a,i,j) * wm_interm_25_triplet_pt2(a,q,j,i)
term(400) = term(400) + wm_interm_169_triplet_pt2(p,a,i,j) * wm_interm_25_triplet_pt2(a,q,j,i)
term(401) = term(401) + wm_interm_172_triplet_pt2(p,a,i,j) * wm_interm_25_triplet_pt2(a,q,j,i)
term(402) = term(402) + wm_interm_171_triplet_pt2(p,a,i,j) * wm_interm_26_triplet_pt2(a,q,j,i)
term(403) = term(403) + wm_interm_172_triplet_pt2(p,a,i,j) * wm_interm_26_triplet_pt2(a,q,j,i)
term(404) = term(404) + wm_interm_169_triplet_pt2(p,a,i,j) * wm_interm_26_triplet_pt2(a,q,j,i)
term(405) = term(405) + wm_interm_170_triplet_pt2(p,a,i,j) * wm_interm_26_triplet_pt2(a,q,j,i)
term(406) = term(406) + wm_interm_170_triplet_pt2(p,a,i,j) * wm_interm_27_triplet_pt2(a,q,j,i)
term(407) = term(407) + wm_interm_170_triplet_pt2(p,a,i,j) * wm_interm_28_triplet_pt2(a,q,j,i)
term(408) = term(408) + wm_interm_171_triplet_pt2(p,a,i,j) * wm_interm_27_triplet_pt2(a,q,j,i)
term(409) = term(409) + wm_interm_171_triplet_pt2(p,a,i,j) * wm_interm_28_triplet_pt2(a,q,j,i)
term(410) = term(410) + wm_interm_169_triplet_pt2(p,a,i,j) * wm_interm_27_triplet_pt2(a,q,j,i)
term(411) = term(411) + wm_interm_169_triplet_pt2(p,a,i,j) * wm_interm_28_triplet_pt2(a,q,j,i)
term(412) = term(412) + wm_interm_172_triplet_pt2(p,a,i,j) * wm_interm_27_triplet_pt2(a,q,j,i)
term(413) = term(413) + wm_interm_172_triplet_pt2(p,a,i,j) * wm_interm_28_triplet_pt2(a,q,j,i)
term(414) = term(414) + wm_interm_35_triplet_pt2(a,p,i,j) * wm_interm_71_triplet_pt2(a,q,i,j)
term(415) = term(415) + wm_interm_35_triplet_pt2(a,p,i,j) * wm_interm_73_triplet_pt2(a,q,i,j)
term(416) = term(416) + wm_interm_37_triplet_pt2(a,p,i,j) * wm_interm_71_triplet_pt2(a,q,i,j)
term(417) = term(417) + wm_interm_37_triplet_pt2(a,p,i,j) * wm_interm_73_triplet_pt2(a,q,i,j)
term(418) = term(418) + wm_interm_11_triplet_pt2(a,p,i,j) * wm_interm_73_triplet_pt2(a,q,i,j)
term(419) = term(419) + wm_interm_12_triplet_pt2(a,p,i,j) * wm_interm_73_triplet_pt2(a,q,i,j)
term(420) = term(420) + wm_interm_12_triplet_pt2(a,p,i,j) * wm_interm_71_triplet_pt2(a,q,i,j)
term(421) = term(421) + wm_interm_11_triplet_pt2(a,p,i,j) * wm_interm_71_triplet_pt2(a,q,i,j)
term(422) = term(422) + wm_interm_11_triplet_pt2(a,p,i,j) * wm_interm_69_triplet_pt2(a,q,i,j)
term(423) = term(423) + wm_interm_11_triplet_pt2(a,p,i,j) * wm_interm_68_triplet_pt2(a,q,i,j)
term(424) = term(424) + wm_interm_12_triplet_pt2(a,p,i,j) * wm_interm_69_triplet_pt2(a,q,i,j)
term(425) = term(425) + wm_interm_12_triplet_pt2(a,p,i,j) * wm_interm_68_triplet_pt2(a,q,i,j)
term(426) = term(426) + wm_interm_35_triplet_pt2(a,p,i,j) * wm_interm_69_triplet_pt2(a,q,i,j)
term(427) = term(427) + wm_interm_35_triplet_pt2(a,p,i,j) * wm_interm_68_triplet_pt2(a,q,i,j)
term(428) = term(428) + wm_interm_37_triplet_pt2(a,p,i,j) * wm_interm_69_triplet_pt2(a,q,i,j)
term(429) = term(429) + wm_interm_37_triplet_pt2(a,p,i,j) * wm_interm_68_triplet_pt2(a,q,i,j)
end do 
end do 
end do 

term(0) = term(0) * (2.0d+0) 
term(1) = term(1) * (2.0d+0) 
term(3) = term(3) * (-2.0d+0) 
term(4) = term(4) * (-2.0d+0) 
term(5) = term(5) * (4.0d+0) 
term(7) = term(7) * (-2.0d+0) 
term(9) = term(9) * (-2.0d+0) 
term(10) = term(10) * (-2.0d+0) 
term(11) = term(11) * (4.0d+0) 
term(13) = term(13) * (-2.0d+0) 
term(14) = term(14) * (-0.5d+0) 
term(17) = term(17) * (-2.0d+0) 
term(18) = term(18) * (-0.5d+0) 
term(20) = term(20) * (-0.5d+0) 
term(23) = term(23) * (-2.0d+0) 
term(24) = term(24) * (-0.5d+0) 
term(26) = term(26) * (-0.5d+0) 
term(29) = term(29) * (-2.0d+0) 
term(30) = term(30) * (-0.5d+0) 
term(32) = term(32) * (-0.5d+0) 
term(35) = term(35) * (-2.0d+0) 
term(36) = term(36) * (-0.5d+0) 
term(38) = term(38) * (-0.5d+0) 
term(40) = term(40) * (-0.5d+0) 
term(41) = term(41) * (-0.5d+0) 
term(42) = term(42) * (-0.5d+0) 
term(44) = term(44) * (-0.5d+0) 
term(47) = term(47) * (-2.0d+0) 
term(48) = term(48) * (-0.5d+0) 
term(50) = term(50) * (-0.5d+0) 
term(52) = term(52) * (-0.5d+0) 
term(53) = term(53) * (-0.5d+0) 
term(54) = term(54) * (-0.5d+0) 
term(56) = term(56) * (-0.5d+0) 
term(59) = term(59) * (-2.0d+0) 
term(60) = term(60) * (-0.5d+0) 
term(62) = term(62) * (-0.5d+0) 
term(64) = term(64) * (-0.5d+0) 
term(66) = term(66) * (-0.5d+0) 
term(67) = term(67) * (-0.5d+0) 
term(68) = term(68) * (-0.5d+0) 
term(70) = term(70) * (-0.5d+0) 
term(72) = term(72) * (-0.5d+0) 
term(73) = term(73) * (-0.5d+0) 
term(74) = term(74) * (-0.5d+0) 
term(75) = term(75) * (-0.5d+0) 
term(77) = term(77) * (-0.5d+0) 
term(78) = term(78) * (-0.5d+0) 
term(80) = term(80) * (4.0d+0) 
term(81) = term(81) * (-4.0d+0) 
term(82) = term(82) * (-8.0d+0) 
term(83) = term(83) * (8.0d+0) 
term(84) = term(84) * (4.0d+0) 
term(85) = term(85) * (-8.0d+0) 
term(86) = term(86) * (-4.0d+0) 
term(87) = term(87) * (8.0d+0) 
term(88) = term(88) * (-2.0d+0) 
term(89) = term(89) * (2.0d+0) 
term(90) = term(90) * (4.0d+0) 
term(91) = term(91) * (-4.0d+0) 
term(92) = term(92) * (-2.0d+0) 
term(93) = term(93) * (4.0d+0) 
term(94) = term(94) * (2.0d+0) 
term(95) = term(95) * (-4.0d+0) 
term(96) = term(96) * (-2.0d+0) 
term(97) = term(97) * (2.0d+0) 
term(98) = term(98) * (4.0d+0) 
term(99) = term(99) * (-4.0d+0) 
term(100) = term(100) * (-2.0d+0) 
term(101) = term(101) * (4.0d+0) 
term(102) = term(102) * (2.0d+0) 
term(103) = term(103) * (-4.0d+0) 
term(104) = term(104) * (-2.0d+0) 
term(105) = term(105) * (2.0d+0) 
term(106) = term(106) * (-2.0d+0) 
term(107) = term(107) * (-2.0d+0) 
term(108) = term(108) * (-2.0d+0) 
term(109) = term(109) * (2.0d+0) 
term(110) = term(110) * (-2.0d+0) 
term(111) = term(111) * (2.0d+0) 
term(112) = term(112) * (4.0d+0) 
term(113) = term(113) * (-4.0d+0) 
term(114) = term(114) * (-2.0d+0) 
term(115) = term(115) * (4.0d+0) 
term(116) = term(116) * (2.0d+0) 
term(117) = term(117) * (2.0d+0) 
term(118) = term(118) * (2.0d+0) 
term(119) = term(119) * (-4.0d+0) 
term(120) = term(120) * (-2.0d+0) 
term(121) = term(121) * (2.0d+0) 
term(122) = term(122) * (-2.0d+0) 
term(123) = term(123) * (2.0d+0) 
term(124) = term(124) * (-2.0d+0) 
term(125) = term(125) * (-2.0d+0) 
term(126) = term(126) * (2.0d+0) 
term(127) = term(127) * (2.0d+0) 
term(128) = term(128) * (-2.0d+0) 
term(129) = term(129) * (-2.0d+0) 
term(130) = term(130) * (2.0d+0) 
term(131) = term(131) * (2.0d+0) 
term(132) = term(132) * (-0.5d+0) 
term(133) = term(133) * (-0.5d+0) 
term(135) = term(135) * (-0.5d+0) 
term(136) = term(136) * (-0.5d+0) 
term(140) = term(140) * (-2.0d+0) 
term(141) = term(141) * (-0.5d+0) 
term(143) = term(143) * (-0.5d+0) 
term(144) = term(144) * (-0.5d+0) 
term(146) = term(146) * (-0.5d+0) 
term(147) = term(147) * (-0.5d+0) 
term(149) = term(149) * (-0.5d+0) 
term(150) = term(150) * (-0.5d+0) 
term(152) = term(152) * (-0.5d+0) 
term(153) = term(153) * (-0.5d+0) 
term(154) = term(154) * (-0.5d+0) 
term(158) = term(158) * (-2.0d+0) 
term(159) = term(159) * (-0.5d+0) 
term(160) = term(160) * (-0.5d+0) 
term(164) = term(164) * (-2.0d+0) 
term(167) = term(167) * (-2.0d+0) 
term(168) = term(168) * (-2.0d+0) 
term(169) = term(169) * (-2.0d+0) 
term(170) = term(170) * (4.0d+0) 
term(171) = term(171) * (-0.5d+0) 
term(172) = term(172) * (-0.5d+0) 
term(174) = term(174) * (-0.5d+0) 
term(175) = term(175) * (-0.5d+0) 
term(179) = term(179) * (-2.0d+0) 
term(180) = term(180) * (-0.5d+0) 
term(182) = term(182) * (-0.5d+0) 
term(183) = term(183) * (-0.5d+0) 
term(185) = term(185) * (-0.5d+0) 
term(186) = term(186) * (-0.5d+0) 
term(188) = term(188) * (-0.5d+0) 
term(189) = term(189) * (-0.5d+0) 
term(191) = term(191) * (-0.5d+0) 
term(192) = term(192) * (-0.5d+0) 
term(193) = term(193) * (-0.5d+0) 
term(195) = term(195) * (-0.5d+0) 
term(196) = term(196) * (-0.5d+0) 
term(200) = term(200) * (-2.0d+0) 
term(203) = term(203) * (-2.0d+0) 
term(206) = term(206) * (-2.0d+0) 
term(207) = term(207) * (-2.0d+0) 
term(208) = term(208) * (-2.0d+0) 
term(209) = term(209) * (4.0d+0) 
term(210) = term(210) * (-2.0d+0) 
term(211) = term(211) * (-2.0d+0) 
term(212) = term(212) * (4.0d+0) 
term(213) = term(213) * (2.0d+0) 
term(214) = term(214) * (2.0d+0) 
term(215) = term(215) * (-4.0d+0) 
term(216) = term(216) * (-2.0d+0) 
term(217) = term(217) * (2.0d+0) 
term(218) = term(218) * (-2.0d+0) 
term(219) = term(219) * (-2.0d+0) 
term(220) = term(220) * (2.0d+0) 
term(221) = term(221) * (-2.0d+0) 
term(222) = term(222) * (2.0d+0) 
term(223) = term(223) * (2.0d+0) 
term(224) = term(224) * (-2.0d+0) 
term(225) = term(225) * (-2.0d+0) 
term(226) = term(226) * (4.0d+0) 
term(227) = term(227) * (4.0d+0) 
term(228) = term(228) * (4.0d+0) 
term(229) = term(229) * (-8.0d+0) 
term(230) = term(230) * (2.0d+0) 
term(231) = term(231) * (2.0d+0) 
term(232) = term(232) * (-4.0d+0) 
term(233) = term(233) * (-4.0d+0) 
term(234) = term(234) * (-4.0d+0) 
term(235) = term(235) * (8.0d+0) 
term(236) = term(236) * (-2.0d+0) 
term(237) = term(237) * (-2.0d+0) 
term(238) = term(238) * (4.0d+0) 
term(239) = term(239) * (2.0d+0) 
term(240) = term(240) * (2.0d+0) 
term(241) = term(241) * (-4.0d+0) 
term(242) = term(242) * (-2.0d+0) 
term(243) = term(243) * (2.0d+0) 
term(244) = term(244) * (-2.0d+0) 
term(245) = term(245) * (-2.0d+0) 
term(246) = term(246) * (2.0d+0) 
term(247) = term(247) * (-2.0d+0) 
term(248) = term(248) * (2.0d+0) 
term(249) = term(249) * (2.0d+0) 
term(250) = term(250) * (-2.0d+0) 
term(251) = term(251) * (-2.0d+0) 
term(252) = term(252) * (4.0d+0) 
term(253) = term(253) * (2.0d+0) 
term(254) = term(254) * (2.0d+0) 
term(255) = term(255) * (-4.0d+0) 
term(256) = term(256) * (4.0d+0) 
term(257) = term(257) * (4.0d+0) 
term(258) = term(258) * (-8.0d+0) 
term(259) = term(259) * (-4.0d+0) 
term(260) = term(260) * (-4.0d+0) 
term(261) = term(261) * (8.0d+0) 
term(262) = term(262) * (2.0d+0) 
term(263) = term(263) * (-4.0d+0) 
term(264) = term(264) * (-4.0d+0) 
term(265) = term(265) * (8.0d+0) 
term(266) = term(266) * (2.0d+0) 
term(267) = term(267) * (-4.0d+0) 
term(268) = term(268) * (2.0d+0) 
term(269) = term(269) * (-4.0d+0) 
term(270) = term(270) * (-4.0d+0) 
term(271) = term(271) * (8.0d+0) 
term(272) = term(272) * (2.0d+0) 
term(273) = term(273) * (-4.0d+0) 
term(274) = term(274) * (-2.0d+0) 
term(275) = term(275) * (4.0d+0) 
term(276) = term(276) * (4.0d+0) 
term(277) = term(277) * (-8.0d+0) 
term(278) = term(278) * (-2.0d+0) 
term(279) = term(279) * (4.0d+0) 
term(280) = term(280) * (-2.0d+0) 
term(281) = term(281) * (4.0d+0) 
term(282) = term(282) * (4.0d+0) 
term(283) = term(283) * (-8.0d+0) 
term(284) = term(284) * (-2.0d+0) 
term(285) = term(285) * (4.0d+0) 
term(286) = term(286) * (-2.0d+0) 
term(287) = term(287) * (4.0d+0) 
term(288) = term(288) * (-2.0d+0) 
term(289) = term(289) * (-2.0d+0) 
term(290) = term(290) * (-2.0d+0) 
term(291) = term(291) * (2.0d+0) 
term(292) = term(292) * (-2.0d+0) 
term(293) = term(293) * (2.0d+0) 
term(294) = term(294) * (2.0d+0) 
term(295) = term(295) * (-4.0d+0) 
term(296) = term(296) * (-2.0d+0) 
term(297) = term(297) * (2.0d+0) 
term(298) = term(298) * (-2.0d+0) 
term(299) = term(299) * (4.0d+0) 
term(300) = term(300) * (-2.0d+0) 
term(301) = term(301) * (-2.0d+0) 
term(302) = term(302) * (-2.0d+0) 
term(303) = term(303) * (2.0d+0) 
term(304) = term(304) * (-2.0d+0) 
term(305) = term(305) * (2.0d+0) 
term(306) = term(306) * (2.0d+0) 
term(307) = term(307) * (-4.0d+0) 
term(308) = term(308) * (-2.0d+0) 
term(309) = term(309) * (2.0d+0) 
term(310) = term(310) * (2.0d+0) 
term(311) = term(311) * (2.0d+0) 
term(312) = term(312) * (2.0d+0) 
term(313) = term(313) * (2.0d+0) 
term(314) = term(314) * (8.0d+0) 
term(315) = term(315) * (-8.0d+0) 
term(316) = term(316) * (-16.0d+0) 
term(317) = term(317) * (16.0d+0) 
term(318) = term(318) * (8.0d+0) 
term(319) = term(319) * (-16.0d+0) 
term(320) = term(320) * (-8.0d+0) 
term(321) = term(321) * (16.0d+0) 
term(322) = term(322) * (-8.0d+0) 
term(323) = term(323) * (8.0d+0) 
term(324) = term(324) * (16.0d+0) 
term(325) = term(325) * (-16.0d+0) 
term(326) = term(326) * (-8.0d+0) 
term(327) = term(327) * (16.0d+0) 
term(328) = term(328) * (8.0d+0) 
term(329) = term(329) * (-16.0d+0) 
term(330) = term(330) * (-8.0d+0) 
term(331) = term(331) * (8.0d+0) 
term(332) = term(332) * (-8.0d+0) 
term(333) = term(333) * (-8.0d+0) 
term(334) = term(334) * (-8.0d+0) 
term(335) = term(335) * (8.0d+0) 
term(336) = term(336) * (-8.0d+0) 
term(337) = term(337) * (8.0d+0) 
term(338) = term(338) * (8.0d+0) 
term(339) = term(339) * (-8.0d+0) 
term(340) = term(340) * (-8.0d+0) 
term(341) = term(341) * (8.0d+0) 
term(342) = term(342) * (8.0d+0) 
term(343) = term(343) * (8.0d+0) 
term(344) = term(344) * (8.0d+0) 
term(345) = term(345) * (-8.0d+0) 
term(346) = term(346) * (-2.0d+0) 
term(347) = term(347) * (2.0d+0) 
term(348) = term(348) * (-2.0d+0) 
term(349) = term(349) * (2.0d+0) 
term(350) = term(350) * (4.0d+0) 
term(351) = term(351) * (-4.0d+0) 
term(352) = term(352) * (-2.0d+0) 
term(353) = term(353) * (2.0d+0) 
term(354) = term(354) * (-2.0d+0) 
term(355) = term(355) * (2.0d+0) 
term(356) = term(356) * (-2.0d+0) 
term(357) = term(357) * (2.0d+0) 
term(358) = term(358) * (-2.0d+0) 
term(359) = term(359) * (2.0d+0) 
term(360) = term(360) * (-2.0d+0) 
term(361) = term(361) * (4.0d+0) 
term(362) = term(362) * (2.0d+0) 
term(363) = term(363) * (-4.0d+0) 
term(364) = term(364) * (-2.0d+0) 
term(365) = term(365) * (4.0d+0) 
term(366) = term(366) * (2.0d+0) 
term(367) = term(367) * (-4.0d+0) 
term(368) = term(368) * (4.0d+0) 
term(369) = term(369) * (-8.0d+0) 
term(370) = term(370) * (-4.0d+0) 
term(371) = term(371) * (8.0d+0) 
term(372) = term(372) * (-2.0d+0) 
term(373) = term(373) * (2.0d+0) 
term(374) = term(374) * (-2.0d+0) 
term(375) = term(375) * (2.0d+0) 
term(376) = term(376) * (4.0d+0) 
term(377) = term(377) * (-4.0d+0) 
term(378) = term(378) * (-2.0d+0) 
term(379) = term(379) * (2.0d+0) 
term(380) = term(380) * (-2.0d+0) 
term(381) = term(381) * (2.0d+0) 
term(382) = term(382) * (-2.0d+0) 
term(383) = term(383) * (2.0d+0) 
term(384) = term(384) * (-2.0d+0) 
term(385) = term(385) * (2.0d+0) 
term(386) = term(386) * (-2.0d+0) 
term(387) = term(387) * (2.0d+0) 
term(388) = term(388) * (-2.0d+0) 
term(389) = term(389) * (2.0d+0) 
term(390) = term(390) * (4.0d+0) 
term(391) = term(391) * (-4.0d+0) 
term(392) = term(392) * (4.0d+0) 
term(393) = term(393) * (-4.0d+0) 
term(394) = term(394) * (4.0d+0) 
term(395) = term(395) * (-4.0d+0) 
term(396) = term(396) * (-8.0d+0) 
term(397) = term(397) * (8.0d+0) 
term(398) = term(398) * (-8.0d+0) 
term(399) = term(399) * (8.0d+0) 
term(400) = term(400) * (8.0d+0) 
term(401) = term(401) * (-8.0d+0) 
term(402) = term(402) * (-8.0d+0) 
term(403) = term(403) * (8.0d+0) 
term(404) = term(404) * (-8.0d+0) 
term(405) = term(405) * (8.0d+0) 
term(406) = term(406) * (-8.0d+0) 
term(407) = term(407) * (16.0d+0) 
term(408) = term(408) * (8.0d+0) 
term(409) = term(409) * (-16.0d+0) 
term(410) = term(410) * (8.0d+0) 
term(411) = term(411) * (-16.0d+0) 
term(412) = term(412) * (-8.0d+0) 
term(413) = term(413) * (16.0d+0) 
term(414) = term(414) * (-8.0d+0) 
term(415) = term(415) * (8.0d+0) 
term(416) = term(416) * (8.0d+0) 
term(417) = term(417) * (-8.0d+0) 
term(418) = term(418) * (-8.0d+0) 
term(419) = term(419) * (8.0d+0) 
term(420) = term(420) * (-8.0d+0) 
term(421) = term(421) * (8.0d+0) 
term(422) = term(422) * (-8.0d+0) 
term(423) = term(423) * (8.0d+0) 
term(424) = term(424) * (8.0d+0) 
term(425) = term(425) * (-8.0d+0) 
term(426) = term(426) * (16.0d+0) 
term(427) = term(427) * (-16.0d+0) 
term(428) = term(428) * (-16.0d+0) 
term(429) = term(429) * (16.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(430) = term(430) + wm_interm_14_triplet_pt2(q,i,j,k) * wm_interm_48_triplet_pt2(p,i,k,j)
term(431) = term(431) + wm_interm_14_triplet_pt2(q,i,j,k) * wm_interm_48_triplet_pt2(p,i,j,k)
end do 
end do 
end do 

term(430) = term(430) * (2.0d+0) 
term(431) = term(431) * (-4.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
do j = 1, nocc 
term(432) = term(432) + r2p(vrdav_Rl, p,j,a,k) * t2(a,q,l,i) * wm_interm_29_triplet_pt2(k,j,i,l)
term(433) = term(433) + r2p(vrdav_Rl, p,j,a,k) * t2(a,q,l,i) * wm_interm_29_triplet_pt2(j,k,i,l)
term(434) = term(434) + r2p(vrdav_Rl, p,j,a,k) * t2(a,q,l,i) * wm_interm_40_triplet_pt2(k,j,i,l)
term(435) = term(435) + r2p(vrdav_Rl, p,j,a,k) * t2(a,q,l,i) * wm_interm_40_triplet_pt2(j,k,i,l)
end do 
end do 
end do 
end do 
end do 

term(432) = term(432) * (-0.5d+0) 
term(434) = term(434) * (-1.0d+0) 
term(435) = term(435) * (2.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(436) = term(436) + r2p(vrdav_Rr, a,i,p,j) * s2(a,q,l,k) * wm_interm_58_triplet_pt2(i,j,k,l)
term(437) = term(437) + r2m(vrdav_Rr, a,i,p,j) * s2(a,q,l,k) * wm_interm_58_triplet_pt2(i,j,k,l)
term(438) = term(438) + r2p(vrdav_Rr, a,i,p,j) * s2(a,q,l,k) * wm_interm_75_triplet_pt2(i,j,k,l)
term(439) = term(439) + r2m(vrdav_Rr, a,i,p,j) * s2(a,q,l,k) * wm_interm_75_triplet_pt2(i,j,k,l)
end do 
end do 
end do 
end do 
end do 

term(436) = term(436) * (-0.5d+0) 
term(437) = term(437) * (-1.0d+0) 
term(438) = term(438) * (-1.0d+0) 
term(439) = term(439) * (-2.0d+0) 

do i = 1, nocc 
term(440) = term(440) + r1(vrdav_Rl, q,i) * wm_interm_78_triplet_pt2(p,i)
term(441) = term(441) + r1(vrdav_Rl, q,i) * wm_interm_79_triplet_pt2(p,i)
term(442) = term(442) + r1(vrdav_Rl, q,i) * wm_interm_80_triplet_pt2(p,i)
term(443) = term(443) + r1(vrdav_Rl, q,i) * wm_interm_81_triplet_pt2(p,i)
term(444) = term(444) + r1(vrdav_Rl, q,i) * wm_interm_82_triplet_pt2(p,i)
term(445) = term(445) + s1(q,i) * wm_interm_13_triplet_pt2(p,i)
term(446) = term(446) + s1(q,i) * wm_interm_21_triplet_pt2(p,i)
term(447) = term(447) + s1(q,i) * wm_interm_22_triplet_pt2(p,i)
term(448) = term(448) + s1(q,i) * wm_interm_33_triplet_pt2(p,i)
term(449) = term(449) + s1(q,i) * wm_interm_38_triplet_pt2(p,i)
term(450) = term(450) + r1(vrdav_Rr, p,i) * wm_interm_83_triplet_pt2(q,i)
term(451) = term(451) + r1(vrdav_Rr, p,i) * wm_interm_84_triplet_pt2(q,i)
term(452) = term(452) + r1(vrdav_Rr, p,i) * wm_interm_85_triplet_pt2(q,i)
term(453) = term(453) + t1(q,i) * wm_interm_44_triplet_pt2(p,i)
term(454) = term(454) + t1(q,i) * wm_interm_49_triplet_pt2(p,i)
term(455) = term(455) + t1(q,i) * wm_interm_46_triplet_pt2(p,i)
term(456) = term(456) + r1(vrdav_Rr, p,i) * wm_interm_86_triplet_pt2(q,i)
term(457) = term(457) + r1(vrdav_Rr, p,i) * wm_interm_87_triplet_pt2(q,i)
term(458) = term(458) + t1(q,i) * wm_interm_65_triplet_pt2(p,i)
term(459) = term(459) + t1(q,i) * wm_interm_67_triplet_pt2(p,i)
term(460) = term(460) + wm_interm_54_triplet_pt2(p,i) * wm_interm_6_triplet_pt2(q,i)
end do 

term(440) = term(440) * (2.0d+0) 
term(441) = term(441) * (-1.0d+0) 
term(442) = term(442) * (-1.0d+0) 
term(443) = term(443) * (4.0d+0) 
term(444) = term(444) * (-4.0d+0) 
term(447) = term(447) * (-2.0d+0) 
term(448) = term(448) * (4.0d+0) 
term(449) = term(449) * (-4.0d+0) 
term(450) = term(450) * (-2.0d+0) 
term(455) = term(455) * (-2.0d+0) 
term(456) = term(456) * (-4.0d+0) 
term(457) = term(457) * (4.0d+0) 
term(458) = term(458) * (4.0d+0) 
term(459) = term(459) * (-4.0d+0) 
term(460) = term(460) * (2.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
do j = 1, nocc 
term(461) = term(461) + r2p(vrdav_Rl, p,j,a,i) * t2(b,q,j,i) * wm_interm_23_triplet_pt2(a,b)
term(462) = term(462) + r2p(vrdav_Rl, p,j,a,i) * t2(b,q,j,i) * wm_interm_17_triplet_pt2(a,b)
term(463) = term(463) + r2p(vrdav_Rl, p,j,a,i) * t2(b,q,j,i) * wm_interm_19_triplet_pt2(a,b)
term(464) = term(464) + r2p(vrdav_Rl, p,j,a,i) * t2(b,q,j,i) * wm_interm_34_triplet_pt2(a,b)
term(465) = term(465) + r2p(vrdav_Rl, p,j,a,i) * t2(b,q,j,i) * wm_interm_36_triplet_pt2(a,b)
end do 
end do 
end do 
end do 

term(461) = term(461) * (-0.5d+0) 
term(462) = term(462) * (-0.5d+0) 
term(464) = term(464) * (-2.0d+0) 
term(465) = term(465) * (2.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(466) = term(466) + r2p(vrdav_Rl, a,j,p,i) * t2(b,q,j,i) * wm_interm_23_triplet_pt2(a,b)
term(467) = term(467) + r2p(vrdav_Rl, a,j,p,i) * t2(b,q,j,i) * wm_interm_17_triplet_pt2(a,b)
term(468) = term(468) + r2p(vrdav_Rl, a,j,p,i) * t2(b,q,j,i) * wm_interm_19_triplet_pt2(a,b)
term(469) = term(469) + r2p(vrdav_Rl, a,j,p,i) * t2(b,q,j,i) * wm_interm_34_triplet_pt2(a,b)
term(470) = term(470) + r2p(vrdav_Rl, a,j,p,i) * t2(b,q,j,i) * wm_interm_36_triplet_pt2(a,b)
term(471) = term(471) + r2m(vrdav_Rl, a,j,p,i) * t2(b,q,j,i) * wm_interm_23_triplet_pt2(a,b)
term(472) = term(472) + r2m(vrdav_Rl, a,j,p,i) * t2(b,q,j,i) * wm_interm_17_triplet_pt2(a,b)
term(473) = term(473) + r2m(vrdav_Rl, a,j,p,i) * t2(b,q,j,i) * wm_interm_19_triplet_pt2(a,b)
term(474) = term(474) + r2m(vrdav_Rl, a,j,p,i) * t2(b,q,j,i) * wm_interm_34_triplet_pt2(a,b)
term(475) = term(475) + r2m(vrdav_Rl, a,j,p,i) * t2(b,q,j,i) * wm_interm_36_triplet_pt2(a,b)
end do 
end do 
end do 
end do 

term(468) = term(468) * (-2.0d+0) 
term(469) = term(469) * (4.0d+0) 
term(470) = term(470) * (-4.0d+0) 
term(471) = term(471) * (2.0d+0) 
term(472) = term(472) * (2.0d+0) 
term(473) = term(473) * (-4.0d+0) 
term(474) = term(474) * (8.0d+0) 
term(475) = term(475) * (-8.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
do l = 1, nocc 
do i = 1, nocc 
term(476) = term(476) + r2p(vrdav_Rr, p,i,a,j) * s2(a,q,l,k) * wm_interm_58_triplet_pt2(i,j,l,k)
term(477) = term(477) + r2p(vrdav_Rr, p,i,a,j) * s2(a,q,l,k) * wm_interm_75_triplet_pt2(i,j,l,k)
end do 
end do 
end do 
end do 
end do 

term(476) = term(476) * (-0.5d+0) 
term(477) = term(477) * (-1.0d+0) 

do l = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
do k = 1, nocc 
term(478) = term(478) + r2m(vrdav_Rl, a,j,q,i) * r2p(vrdav_Rr, p,k,a,l) * wm_interm_16_triplet_pt2(l,k,i,j)
term(479) = term(479) + r2m(vrdav_Rl, a,j,q,i) * r2p(vrdav_Rr, p,k,a,l) * wm_interm_16_triplet_pt2(k,l,i,j)
end do 
end do 
end do 
end do 
end do 

term(478) = term(478) * (-1.0d+0) 
term(479) = term(479) * (2.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
do j = 1, nocc 
do b = nocc + 1, nactive 
term(480) = term(480) + s2(a,p,j,i) * t2(b,q,j,i) * wm_interm_120_triplet_pt2(b,a)
term(481) = term(481) + s2(a,p,j,i) * t2(b,q,j,i) * wm_interm_121_triplet_pt2(b,a)
term(482) = term(482) + s2(a,p,j,i) * t2(b,q,j,i) * wm_interm_110_triplet_pt2(b,a)
term(483) = term(483) + s2(a,p,j,i) * t2(b,q,j,i) * wm_interm_108_triplet_pt2(b,a)
term(484) = term(484) + s2(a,p,j,i) * t2(b,q,j,i) * wm_interm_109_triplet_pt2(b,a)
term(485) = term(485) + s2(a,p,j,i) * t2(b,q,j,i) * wm_interm_119_triplet_pt2(b,a)
term(486) = term(486) + s2(a,p,j,i) * t2(b,q,j,i) * wm_interm_137_triplet_pt2(b,a)
term(487) = term(487) + s2(a,p,j,i) * t2(b,q,j,i) * wm_interm_138_triplet_pt2(b,a)
term(488) = term(488) + s2(a,p,j,i) * t2(b,q,j,i) * wm_interm_131_triplet_pt2(b,a)
term(489) = term(489) + s2(a,p,j,i) * t2(b,q,j,i) * wm_interm_132_triplet_pt2(b,a)
term(490) = term(490) + r2m(vrdav_Rl, a,j,q,i) * r2m(vrdav_Rr, b,j,p,i) * wm_interm_4_triplet_pt2(b,a)
term(491) = term(491) + r2m(vrdav_Rl, a,j,q,i) * r2m(vrdav_Rr, b,j,p,i) * wm_interm_5_triplet_pt2(b,a)
term(492) = term(492) + s2(a,p,j,i) * t2(b,q,j,i) * wm_interm_164_triplet_pt2(b,a)
term(493) = term(493) + s2(a,p,j,i) * t2(b,q,j,i) * wm_interm_165_triplet_pt2(b,a)
term(494) = term(494) + s2(a,p,j,i) * t2(b,q,j,i) * wm_interm_158_triplet_pt2(b,a)
term(495) = term(495) + s2(a,p,j,i) * t2(b,q,j,i) * wm_interm_156_triplet_pt2(b,a)
term(496) = term(496) + s2(a,p,j,i) * t2(b,q,j,i) * wm_interm_157_triplet_pt2(b,a)
term(497) = term(497) + s2(a,p,j,i) * t2(b,q,j,i) * wm_interm_163_triplet_pt2(b,a)
term(498) = term(498) + s2(a,p,j,i) * t2(b,q,j,i) * wm_interm_177_triplet_pt2(b,a)
term(499) = term(499) + s2(a,p,j,i) * t2(b,q,j,i) * wm_interm_178_triplet_pt2(b,a)
term(500) = term(500) + s2(a,p,j,i) * t2(b,q,j,i) * wm_interm_173_triplet_pt2(b,a)
term(501) = term(501) + s2(a,p,j,i) * t2(b,q,j,i) * wm_interm_174_triplet_pt2(b,a)
end do 
end do 
end do 
end do 

term(481) = term(481) * (-2.0d+0) 
term(484) = term(484) * (-2.0d+0) 
term(486) = term(486) * (4.0d+0) 
term(487) = term(487) * (-4.0d+0) 
term(488) = term(488) * (4.0d+0) 
term(489) = term(489) * (-4.0d+0) 
term(490) = term(490) * (4.0d+0) 
term(491) = term(491) * (-8.0d+0) 
term(492) = term(492) * (2.0d+0) 
term(493) = term(493) * (-4.0d+0) 
term(494) = term(494) * (2.0d+0) 
term(495) = term(495) * (2.0d+0) 
term(496) = term(496) * (-4.0d+0) 
term(497) = term(497) * (2.0d+0) 
term(498) = term(498) * (8.0d+0) 
term(499) = term(499) * (-8.0d+0) 
term(500) = term(500) * (8.0d+0) 
term(501) = term(501) * (-8.0d+0) 

do a = nocc + 1, nactive 
term(502) = term(502) + wm_interm_4_triplet_pt2(q,a) * wm_interm_93_triplet_pt2(a,p)
term(503) = term(503) + wm_interm_5_triplet_pt2(q,a) * wm_interm_93_triplet_pt2(a,p)
term(504) = term(504) + wm_interm_4_triplet_pt2(a,q) * wm_interm_93_triplet_pt2(p,a)
term(505) = term(505) + wm_interm_5_triplet_pt2(a,q) * wm_interm_93_triplet_pt2(p,a)
term(506) = term(506) + wm_interm_110_triplet_pt2(a,p) * wm_interm_4_triplet_pt2(q,a)
term(507) = term(507) + wm_interm_110_triplet_pt2(a,p) * wm_interm_5_triplet_pt2(q,a)
term(508) = term(508) + wm_interm_108_triplet_pt2(a,p) * wm_interm_4_triplet_pt2(q,a)
term(509) = term(509) + wm_interm_109_triplet_pt2(a,p) * wm_interm_4_triplet_pt2(q,a)
term(510) = term(510) + wm_interm_108_triplet_pt2(a,p) * wm_interm_5_triplet_pt2(q,a)
term(511) = term(511) + wm_interm_109_triplet_pt2(a,p) * wm_interm_5_triplet_pt2(q,a)
term(512) = term(512) + wm_interm_120_triplet_pt2(a,p) * wm_interm_4_triplet_pt2(q,a)
term(513) = term(513) + wm_interm_120_triplet_pt2(a,p) * wm_interm_5_triplet_pt2(q,a)
term(514) = term(514) + wm_interm_121_triplet_pt2(a,p) * wm_interm_4_triplet_pt2(q,a)
term(515) = term(515) + wm_interm_121_triplet_pt2(a,p) * wm_interm_5_triplet_pt2(q,a)
term(516) = term(516) + wm_interm_119_triplet_pt2(a,p) * wm_interm_4_triplet_pt2(q,a)
term(517) = term(517) + wm_interm_119_triplet_pt2(a,p) * wm_interm_5_triplet_pt2(q,a)
term(518) = term(518) + wm_interm_17_triplet_pt2(p,a) * wm_interm_55_triplet_pt2(q,a)
term(519) = term(519) + wm_interm_19_triplet_pt2(p,a) * wm_interm_55_triplet_pt2(q,a)
term(520) = term(520) + wm_interm_23_triplet_pt2(p,a) * wm_interm_55_triplet_pt2(q,a)
term(521) = term(521) + wm_interm_17_triplet_pt2(p,a) * wm_interm_60_triplet_pt2(q,a)
term(522) = term(522) + wm_interm_19_triplet_pt2(p,a) * wm_interm_60_triplet_pt2(q,a)
term(523) = term(523) + wm_interm_23_triplet_pt2(p,a) * wm_interm_60_triplet_pt2(q,a)
term(524) = term(524) + wm_interm_17_triplet_pt2(p,a) * wm_interm_62_triplet_pt2(q,a)
term(525) = term(525) + wm_interm_19_triplet_pt2(p,a) * wm_interm_62_triplet_pt2(q,a)
term(526) = term(526) + wm_interm_23_triplet_pt2(p,a) * wm_interm_62_triplet_pt2(q,a)
term(527) = term(527) + wm_interm_131_triplet_pt2(a,p) * wm_interm_4_triplet_pt2(q,a)
term(528) = term(528) + wm_interm_131_triplet_pt2(a,p) * wm_interm_5_triplet_pt2(q,a)
term(529) = term(529) + wm_interm_132_triplet_pt2(a,p) * wm_interm_4_triplet_pt2(q,a)
term(530) = term(530) + wm_interm_132_triplet_pt2(a,p) * wm_interm_5_triplet_pt2(q,a)
term(531) = term(531) + wm_interm_137_triplet_pt2(a,p) * wm_interm_4_triplet_pt2(q,a)
term(532) = term(532) + wm_interm_137_triplet_pt2(a,p) * wm_interm_5_triplet_pt2(q,a)
term(533) = term(533) + wm_interm_138_triplet_pt2(a,p) * wm_interm_4_triplet_pt2(q,a)
term(534) = term(534) + wm_interm_138_triplet_pt2(a,p) * wm_interm_5_triplet_pt2(q,a)
term(535) = term(535) + wm_interm_34_triplet_pt2(p,a) * wm_interm_55_triplet_pt2(q,a)
term(536) = term(536) + wm_interm_36_triplet_pt2(p,a) * wm_interm_55_triplet_pt2(q,a)
term(537) = term(537) + wm_interm_34_triplet_pt2(p,a) * wm_interm_60_triplet_pt2(q,a)
term(538) = term(538) + wm_interm_36_triplet_pt2(p,a) * wm_interm_60_triplet_pt2(q,a)
term(539) = term(539) + wm_interm_34_triplet_pt2(p,a) * wm_interm_62_triplet_pt2(q,a)
term(540) = term(540) + wm_interm_36_triplet_pt2(p,a) * wm_interm_62_triplet_pt2(q,a)
term(541) = term(541) + wm_interm_120_triplet_pt2(p,a) * wm_interm_4_triplet_pt2(a,q)
term(542) = term(542) + wm_interm_121_triplet_pt2(p,a) * wm_interm_4_triplet_pt2(a,q)
term(543) = term(543) + wm_interm_110_triplet_pt2(p,a) * wm_interm_4_triplet_pt2(a,q)
term(544) = term(544) + wm_interm_108_triplet_pt2(p,a) * wm_interm_4_triplet_pt2(a,q)
term(545) = term(545) + wm_interm_109_triplet_pt2(p,a) * wm_interm_4_triplet_pt2(a,q)
term(546) = term(546) + wm_interm_119_triplet_pt2(p,a) * wm_interm_4_triplet_pt2(a,q)
term(547) = term(547) + wm_interm_120_triplet_pt2(p,a) * wm_interm_5_triplet_pt2(a,q)
term(548) = term(548) + wm_interm_121_triplet_pt2(p,a) * wm_interm_5_triplet_pt2(a,q)
term(549) = term(549) + wm_interm_110_triplet_pt2(p,a) * wm_interm_5_triplet_pt2(a,q)
term(550) = term(550) + wm_interm_108_triplet_pt2(p,a) * wm_interm_5_triplet_pt2(a,q)
term(551) = term(551) + wm_interm_109_triplet_pt2(p,a) * wm_interm_5_triplet_pt2(a,q)
term(552) = term(552) + wm_interm_119_triplet_pt2(p,a) * wm_interm_5_triplet_pt2(a,q)
term(553) = term(553) + wm_interm_23_triplet_pt2(a,p) * wm_interm_62_triplet_pt2(a,q)
term(554) = term(554) + wm_interm_23_triplet_pt2(a,p) * wm_interm_55_triplet_pt2(a,q)
term(555) = term(555) + wm_interm_23_triplet_pt2(a,p) * wm_interm_60_triplet_pt2(a,q)
term(556) = term(556) + wm_interm_17_triplet_pt2(a,p) * wm_interm_62_triplet_pt2(a,q)
term(557) = term(557) + wm_interm_17_triplet_pt2(a,p) * wm_interm_55_triplet_pt2(a,q)
term(558) = term(558) + wm_interm_17_triplet_pt2(a,p) * wm_interm_60_triplet_pt2(a,q)
term(559) = term(559) + wm_interm_19_triplet_pt2(a,p) * wm_interm_62_triplet_pt2(a,q)
term(560) = term(560) + wm_interm_19_triplet_pt2(a,p) * wm_interm_55_triplet_pt2(a,q)
term(561) = term(561) + wm_interm_19_triplet_pt2(a,p) * wm_interm_60_triplet_pt2(a,q)
term(562) = term(562) + wm_interm_137_triplet_pt2(p,a) * wm_interm_4_triplet_pt2(a,q)
term(563) = term(563) + wm_interm_138_triplet_pt2(p,a) * wm_interm_4_triplet_pt2(a,q)
term(564) = term(564) + wm_interm_131_triplet_pt2(p,a) * wm_interm_4_triplet_pt2(a,q)
term(565) = term(565) + wm_interm_132_triplet_pt2(p,a) * wm_interm_4_triplet_pt2(a,q)
term(566) = term(566) + wm_interm_137_triplet_pt2(p,a) * wm_interm_5_triplet_pt2(a,q)
term(567) = term(567) + wm_interm_138_triplet_pt2(p,a) * wm_interm_5_triplet_pt2(a,q)
term(568) = term(568) + wm_interm_131_triplet_pt2(p,a) * wm_interm_5_triplet_pt2(a,q)
term(569) = term(569) + wm_interm_132_triplet_pt2(p,a) * wm_interm_5_triplet_pt2(a,q)
term(570) = term(570) + wm_interm_34_triplet_pt2(a,p) * wm_interm_62_triplet_pt2(a,q)
term(571) = term(571) + wm_interm_34_triplet_pt2(a,p) * wm_interm_55_triplet_pt2(a,q)
term(572) = term(572) + wm_interm_34_triplet_pt2(a,p) * wm_interm_60_triplet_pt2(a,q)
term(573) = term(573) + wm_interm_36_triplet_pt2(a,p) * wm_interm_62_triplet_pt2(a,q)
term(574) = term(574) + wm_interm_36_triplet_pt2(a,p) * wm_interm_55_triplet_pt2(a,q)
term(575) = term(575) + wm_interm_36_triplet_pt2(a,p) * wm_interm_60_triplet_pt2(a,q)
term(576) = term(576) + wm_interm_158_triplet_pt2(a,p) * wm_interm_4_triplet_pt2(q,a)
term(577) = term(577) + wm_interm_158_triplet_pt2(a,p) * wm_interm_5_triplet_pt2(q,a)
term(578) = term(578) + wm_interm_156_triplet_pt2(a,p) * wm_interm_4_triplet_pt2(q,a)
term(579) = term(579) + wm_interm_157_triplet_pt2(a,p) * wm_interm_4_triplet_pt2(q,a)
term(580) = term(580) + wm_interm_156_triplet_pt2(a,p) * wm_interm_5_triplet_pt2(q,a)
term(581) = term(581) + wm_interm_157_triplet_pt2(a,p) * wm_interm_5_triplet_pt2(q,a)
term(582) = term(582) + wm_interm_164_triplet_pt2(a,p) * wm_interm_4_triplet_pt2(q,a)
term(583) = term(583) + wm_interm_164_triplet_pt2(a,p) * wm_interm_5_triplet_pt2(q,a)
term(584) = term(584) + wm_interm_165_triplet_pt2(a,p) * wm_interm_4_triplet_pt2(q,a)
term(585) = term(585) + wm_interm_165_triplet_pt2(a,p) * wm_interm_5_triplet_pt2(q,a)
term(586) = term(586) + wm_interm_163_triplet_pt2(a,p) * wm_interm_4_triplet_pt2(q,a)
term(587) = term(587) + wm_interm_163_triplet_pt2(a,p) * wm_interm_5_triplet_pt2(q,a)
term(588) = term(588) + wm_interm_17_triplet_pt2(p,a) * wm_interm_72_triplet_pt2(q,a)
term(589) = term(589) + wm_interm_19_triplet_pt2(p,a) * wm_interm_72_triplet_pt2(q,a)
term(590) = term(590) + wm_interm_23_triplet_pt2(p,a) * wm_interm_72_triplet_pt2(q,a)
term(591) = term(591) + wm_interm_17_triplet_pt2(p,a) * wm_interm_76_triplet_pt2(q,a)
term(592) = term(592) + wm_interm_19_triplet_pt2(p,a) * wm_interm_76_triplet_pt2(q,a)
term(593) = term(593) + wm_interm_23_triplet_pt2(p,a) * wm_interm_76_triplet_pt2(q,a)
term(594) = term(594) + wm_interm_173_triplet_pt2(a,p) * wm_interm_4_triplet_pt2(q,a)
term(595) = term(595) + wm_interm_173_triplet_pt2(a,p) * wm_interm_5_triplet_pt2(q,a)
term(596) = term(596) + wm_interm_174_triplet_pt2(a,p) * wm_interm_4_triplet_pt2(q,a)
term(597) = term(597) + wm_interm_174_triplet_pt2(a,p) * wm_interm_5_triplet_pt2(q,a)
term(598) = term(598) + wm_interm_177_triplet_pt2(a,p) * wm_interm_4_triplet_pt2(q,a)
term(599) = term(599) + wm_interm_177_triplet_pt2(a,p) * wm_interm_5_triplet_pt2(q,a)
term(600) = term(600) + wm_interm_178_triplet_pt2(a,p) * wm_interm_4_triplet_pt2(q,a)
term(601) = term(601) + wm_interm_178_triplet_pt2(a,p) * wm_interm_5_triplet_pt2(q,a)
term(602) = term(602) + wm_interm_34_triplet_pt2(p,a) * wm_interm_72_triplet_pt2(q,a)
term(603) = term(603) + wm_interm_36_triplet_pt2(p,a) * wm_interm_72_triplet_pt2(q,a)
term(604) = term(604) + wm_interm_34_triplet_pt2(p,a) * wm_interm_76_triplet_pt2(q,a)
term(605) = term(605) + wm_interm_36_triplet_pt2(p,a) * wm_interm_76_triplet_pt2(q,a)
term(606) = term(606) + wm_interm_164_triplet_pt2(p,a) * wm_interm_4_triplet_pt2(a,q)
term(607) = term(607) + wm_interm_165_triplet_pt2(p,a) * wm_interm_4_triplet_pt2(a,q)
term(608) = term(608) + wm_interm_158_triplet_pt2(p,a) * wm_interm_4_triplet_pt2(a,q)
term(609) = term(609) + wm_interm_156_triplet_pt2(p,a) * wm_interm_4_triplet_pt2(a,q)
term(610) = term(610) + wm_interm_157_triplet_pt2(p,a) * wm_interm_4_triplet_pt2(a,q)
term(611) = term(611) + wm_interm_163_triplet_pt2(p,a) * wm_interm_4_triplet_pt2(a,q)
term(612) = term(612) + wm_interm_164_triplet_pt2(p,a) * wm_interm_5_triplet_pt2(a,q)
term(613) = term(613) + wm_interm_165_triplet_pt2(p,a) * wm_interm_5_triplet_pt2(a,q)
term(614) = term(614) + wm_interm_158_triplet_pt2(p,a) * wm_interm_5_triplet_pt2(a,q)
term(615) = term(615) + wm_interm_156_triplet_pt2(p,a) * wm_interm_5_triplet_pt2(a,q)
term(616) = term(616) + wm_interm_157_triplet_pt2(p,a) * wm_interm_5_triplet_pt2(a,q)
term(617) = term(617) + wm_interm_163_triplet_pt2(p,a) * wm_interm_5_triplet_pt2(a,q)
term(618) = term(618) + wm_interm_23_triplet_pt2(a,p) * wm_interm_72_triplet_pt2(a,q)
term(619) = term(619) + wm_interm_23_triplet_pt2(a,p) * wm_interm_76_triplet_pt2(a,q)
term(620) = term(620) + wm_interm_17_triplet_pt2(a,p) * wm_interm_72_triplet_pt2(a,q)
term(621) = term(621) + wm_interm_17_triplet_pt2(a,p) * wm_interm_76_triplet_pt2(a,q)
term(622) = term(622) + wm_interm_19_triplet_pt2(a,p) * wm_interm_72_triplet_pt2(a,q)
term(623) = term(623) + wm_interm_19_triplet_pt2(a,p) * wm_interm_76_triplet_pt2(a,q)
term(624) = term(624) + wm_interm_177_triplet_pt2(p,a) * wm_interm_4_triplet_pt2(a,q)
term(625) = term(625) + wm_interm_178_triplet_pt2(p,a) * wm_interm_4_triplet_pt2(a,q)
term(626) = term(626) + wm_interm_173_triplet_pt2(p,a) * wm_interm_4_triplet_pt2(a,q)
term(627) = term(627) + wm_interm_174_triplet_pt2(p,a) * wm_interm_4_triplet_pt2(a,q)
term(628) = term(628) + wm_interm_177_triplet_pt2(p,a) * wm_interm_5_triplet_pt2(a,q)
term(629) = term(629) + wm_interm_178_triplet_pt2(p,a) * wm_interm_5_triplet_pt2(a,q)
term(630) = term(630) + wm_interm_173_triplet_pt2(p,a) * wm_interm_5_triplet_pt2(a,q)
term(631) = term(631) + wm_interm_174_triplet_pt2(p,a) * wm_interm_5_triplet_pt2(a,q)
term(632) = term(632) + wm_interm_34_triplet_pt2(a,p) * wm_interm_72_triplet_pt2(a,q)
term(633) = term(633) + wm_interm_34_triplet_pt2(a,p) * wm_interm_76_triplet_pt2(a,q)
term(634) = term(634) + wm_interm_36_triplet_pt2(a,p) * wm_interm_72_triplet_pt2(a,q)
term(635) = term(635) + wm_interm_36_triplet_pt2(a,p) * wm_interm_76_triplet_pt2(a,q)
end do 

term(502) = term(502) * (2.0d+0) 
term(503) = term(503) * (-4.0d+0) 
term(504) = term(504) * (2.0d+0) 
term(505) = term(505) * (-4.0d+0) 
term(506) = term(506) * (-0.5d+0) 
term(508) = term(508) * (-0.5d+0) 
term(511) = term(511) * (-2.0d+0) 
term(512) = term(512) * (-0.5d+0) 
term(515) = term(515) * (-2.0d+0) 
term(516) = term(516) * (-0.5d+0) 
term(518) = term(518) * (-0.5d+0) 
term(520) = term(520) * (-0.5d+0) 
term(522) = term(522) * (-2.0d+0) 
term(524) = term(524) * (-0.5d+0) 
term(526) = term(526) * (-0.5d+0) 
term(527) = term(527) * (-2.0d+0) 
term(528) = term(528) * (4.0d+0) 
term(529) = term(529) * (2.0d+0) 
term(530) = term(530) * (-4.0d+0) 
term(531) = term(531) * (-2.0d+0) 
term(532) = term(532) * (4.0d+0) 
term(533) = term(533) * (2.0d+0) 
term(534) = term(534) * (-4.0d+0) 
term(535) = term(535) * (-2.0d+0) 
term(536) = term(536) * (2.0d+0) 
term(537) = term(537) * (4.0d+0) 
term(538) = term(538) * (-4.0d+0) 
term(539) = term(539) * (-2.0d+0) 
term(540) = term(540) * (2.0d+0) 
term(541) = term(541) * (-0.5d+0) 
term(543) = term(543) * (-0.5d+0) 
term(544) = term(544) * (-0.5d+0) 
term(546) = term(546) * (-0.5d+0) 
term(548) = term(548) * (-2.0d+0) 
term(551) = term(551) * (-2.0d+0) 
term(553) = term(553) * (-0.5d+0) 
term(554) = term(554) * (-0.5d+0) 
term(556) = term(556) * (-0.5d+0) 
term(557) = term(557) * (-0.5d+0) 
term(561) = term(561) * (-2.0d+0) 
term(562) = term(562) * (-2.0d+0) 
term(563) = term(563) * (2.0d+0) 
term(564) = term(564) * (-2.0d+0) 
term(565) = term(565) * (2.0d+0) 
term(566) = term(566) * (4.0d+0) 
term(567) = term(567) * (-4.0d+0) 
term(568) = term(568) * (4.0d+0) 
term(569) = term(569) * (-4.0d+0) 
term(570) = term(570) * (-2.0d+0) 
term(571) = term(571) * (-2.0d+0) 
term(572) = term(572) * (4.0d+0) 
term(573) = term(573) * (2.0d+0) 
term(574) = term(574) * (2.0d+0) 
term(575) = term(575) * (-4.0d+0) 
term(576) = term(576) * (-1.0d+0) 
term(577) = term(577) * (2.0d+0) 
term(578) = term(578) * (-1.0d+0) 
term(579) = term(579) * (2.0d+0) 
term(580) = term(580) * (2.0d+0) 
term(581) = term(581) * (-4.0d+0) 
term(582) = term(582) * (-1.0d+0) 
term(583) = term(583) * (2.0d+0) 
term(584) = term(584) * (2.0d+0) 
term(585) = term(585) * (-4.0d+0) 
term(586) = term(586) * (-1.0d+0) 
term(587) = term(587) * (2.0d+0) 
term(588) = term(588) * (-2.0d+0) 
term(589) = term(589) * (4.0d+0) 
term(590) = term(590) * (-2.0d+0) 
term(591) = term(591) * (2.0d+0) 
term(592) = term(592) * (-4.0d+0) 
term(593) = term(593) * (2.0d+0) 
term(594) = term(594) * (-4.0d+0) 
term(595) = term(595) * (8.0d+0) 
term(596) = term(596) * (4.0d+0) 
term(597) = term(597) * (-8.0d+0) 
term(598) = term(598) * (-4.0d+0) 
term(599) = term(599) * (8.0d+0) 
term(600) = term(600) * (4.0d+0) 
term(601) = term(601) * (-8.0d+0) 
term(602) = term(602) * (-8.0d+0) 
term(603) = term(603) * (8.0d+0) 
term(604) = term(604) * (8.0d+0) 
term(605) = term(605) * (-8.0d+0) 
term(606) = term(606) * (-1.0d+0) 
term(607) = term(607) * (2.0d+0) 
term(608) = term(608) * (-1.0d+0) 
term(609) = term(609) * (-1.0d+0) 
term(610) = term(610) * (2.0d+0) 
term(611) = term(611) * (-1.0d+0) 
term(612) = term(612) * (2.0d+0) 
term(613) = term(613) * (-4.0d+0) 
term(614) = term(614) * (2.0d+0) 
term(615) = term(615) * (2.0d+0) 
term(616) = term(616) * (-4.0d+0) 
term(617) = term(617) * (2.0d+0) 
term(618) = term(618) * (-2.0d+0) 
term(619) = term(619) * (2.0d+0) 
term(620) = term(620) * (-2.0d+0) 
term(621) = term(621) * (2.0d+0) 
term(622) = term(622) * (4.0d+0) 
term(623) = term(623) * (-4.0d+0) 
term(624) = term(624) * (-4.0d+0) 
term(625) = term(625) * (4.0d+0) 
term(626) = term(626) * (-4.0d+0) 
term(627) = term(627) * (4.0d+0) 
term(628) = term(628) * (8.0d+0) 
term(629) = term(629) * (-8.0d+0) 
term(630) = term(630) * (8.0d+0) 
term(631) = term(631) * (-8.0d+0) 
term(632) = term(632) * (-8.0d+0) 
term(633) = term(633) * (8.0d+0) 
term(634) = term(634) * (8.0d+0) 
term(635) = term(635) * (-8.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
do l = 1, nocc 
do j = 1, nocc 
term(636) = term(636) + r2p(vrdav_Rl, p,j,a,k) * t2(a,q,l,i) * wm_interm_29_triplet_pt2(j,k,l,i)
term(637) = term(637) + r2p(vrdav_Rl, p,j,a,k) * t2(a,q,l,i) * wm_interm_40_triplet_pt2(j,k,l,i)
end do 
end do 
end do 
end do 
end do 

term(636) = term(636) * (-0.5d+0) 
term(637) = term(637) * (-1.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
term(638) = term(638) + wm_interm_1_triplet_pt2(i,j) * wm_interm_92_triplet_pt2(q,p,j,i)
term(639) = term(639) + wm_interm_2_triplet_pt2(i,j) * wm_interm_92_triplet_pt2(q,p,j,i)
term(640) = term(640) + wm_interm_1_triplet_pt2(i,j) * wm_interm_99_triplet_pt2(q,p,j,i)
term(641) = term(641) + wm_interm_2_triplet_pt2(i,j) * wm_interm_99_triplet_pt2(q,p,j,i)
term(642) = term(642) + wm_interm_111_triplet_pt2(i,j) * wm_interm_25_triplet_pt2(p,q,j,i)
term(643) = term(643) + wm_interm_112_triplet_pt2(i,j) * wm_interm_25_triplet_pt2(p,q,j,i)
term(644) = term(644) + wm_interm_113_triplet_pt2(i,j) * wm_interm_25_triplet_pt2(p,q,j,i)
term(645) = term(645) + wm_interm_111_triplet_pt2(i,j) * wm_interm_27_triplet_pt2(p,q,j,i)
term(646) = term(646) + wm_interm_112_triplet_pt2(i,j) * wm_interm_27_triplet_pt2(p,q,j,i)
term(647) = term(647) + wm_interm_113_triplet_pt2(i,j) * wm_interm_27_triplet_pt2(p,q,j,i)
term(648) = term(648) + wm_interm_111_triplet_pt2(i,j) * wm_interm_26_triplet_pt2(p,q,j,i)
term(649) = term(649) + wm_interm_112_triplet_pt2(i,j) * wm_interm_26_triplet_pt2(p,q,j,i)
term(650) = term(650) + wm_interm_113_triplet_pt2(i,j) * wm_interm_26_triplet_pt2(p,q,j,i)
term(651) = term(651) + wm_interm_111_triplet_pt2(i,j) * wm_interm_28_triplet_pt2(p,q,j,i)
term(652) = term(652) + wm_interm_112_triplet_pt2(i,j) * wm_interm_28_triplet_pt2(p,q,j,i)
term(653) = term(653) + wm_interm_113_triplet_pt2(i,j) * wm_interm_28_triplet_pt2(p,q,j,i)
term(654) = term(654) + wm_interm_133_triplet_pt2(i,j) * wm_interm_25_triplet_pt2(p,q,j,i)
term(655) = term(655) + wm_interm_134_triplet_pt2(i,j) * wm_interm_25_triplet_pt2(p,q,j,i)
term(656) = term(656) + wm_interm_123_triplet_pt2(i,j) * wm_interm_25_triplet_pt2(p,q,j,i)
term(657) = term(657) + wm_interm_124_triplet_pt2(i,j) * wm_interm_25_triplet_pt2(p,q,j,i)
term(658) = term(658) + wm_interm_133_triplet_pt2(i,j) * wm_interm_27_triplet_pt2(p,q,j,i)
term(659) = term(659) + wm_interm_134_triplet_pt2(i,j) * wm_interm_27_triplet_pt2(p,q,j,i)
term(660) = term(660) + wm_interm_133_triplet_pt2(i,j) * wm_interm_26_triplet_pt2(p,q,j,i)
term(661) = term(661) + wm_interm_134_triplet_pt2(i,j) * wm_interm_26_triplet_pt2(p,q,j,i)
term(662) = term(662) + wm_interm_133_triplet_pt2(i,j) * wm_interm_28_triplet_pt2(p,q,j,i)
term(663) = term(663) + wm_interm_134_triplet_pt2(i,j) * wm_interm_28_triplet_pt2(p,q,j,i)
term(664) = term(664) + wm_interm_123_triplet_pt2(i,j) * wm_interm_27_triplet_pt2(p,q,j,i)
term(665) = term(665) + wm_interm_124_triplet_pt2(i,j) * wm_interm_27_triplet_pt2(p,q,j,i)
term(666) = term(666) + wm_interm_123_triplet_pt2(i,j) * wm_interm_26_triplet_pt2(p,q,j,i)
term(667) = term(667) + wm_interm_124_triplet_pt2(i,j) * wm_interm_26_triplet_pt2(p,q,j,i)
term(668) = term(668) + wm_interm_123_triplet_pt2(i,j) * wm_interm_28_triplet_pt2(p,q,j,i)
term(669) = term(669) + wm_interm_124_triplet_pt2(i,j) * wm_interm_28_triplet_pt2(p,q,j,i)
term(670) = term(670) + wm_interm_159_triplet_pt2(i,j) * wm_interm_25_triplet_pt2(p,q,j,i)
term(671) = term(671) + wm_interm_160_triplet_pt2(i,j) * wm_interm_25_triplet_pt2(p,q,j,i)
term(672) = term(672) + wm_interm_161_triplet_pt2(i,j) * wm_interm_25_triplet_pt2(p,q,j,i)
term(673) = term(673) + wm_interm_146_triplet_pt2(i,j) * wm_interm_25_triplet_pt2(p,q,j,i)
term(674) = term(674) + wm_interm_147_triplet_pt2(i,j) * wm_interm_25_triplet_pt2(p,q,j,i)
term(675) = term(675) + wm_interm_148_triplet_pt2(i,j) * wm_interm_25_triplet_pt2(p,q,j,i)
term(676) = term(676) + wm_interm_159_triplet_pt2(i,j) * wm_interm_27_triplet_pt2(p,q,j,i)
term(677) = term(677) + wm_interm_160_triplet_pt2(i,j) * wm_interm_27_triplet_pt2(p,q,j,i)
term(678) = term(678) + wm_interm_161_triplet_pt2(i,j) * wm_interm_27_triplet_pt2(p,q,j,i)
term(679) = term(679) + wm_interm_159_triplet_pt2(i,j) * wm_interm_26_triplet_pt2(p,q,j,i)
term(680) = term(680) + wm_interm_160_triplet_pt2(i,j) * wm_interm_26_triplet_pt2(p,q,j,i)
term(681) = term(681) + wm_interm_161_triplet_pt2(i,j) * wm_interm_26_triplet_pt2(p,q,j,i)
term(682) = term(682) + wm_interm_159_triplet_pt2(i,j) * wm_interm_28_triplet_pt2(p,q,j,i)
term(683) = term(683) + wm_interm_160_triplet_pt2(i,j) * wm_interm_28_triplet_pt2(p,q,j,i)
term(684) = term(684) + wm_interm_161_triplet_pt2(i,j) * wm_interm_28_triplet_pt2(p,q,j,i)
term(685) = term(685) + wm_interm_146_triplet_pt2(i,j) * wm_interm_27_triplet_pt2(p,q,j,i)
term(686) = term(686) + wm_interm_147_triplet_pt2(i,j) * wm_interm_27_triplet_pt2(p,q,j,i)
term(687) = term(687) + wm_interm_146_triplet_pt2(i,j) * wm_interm_26_triplet_pt2(p,q,j,i)
term(688) = term(688) + wm_interm_147_triplet_pt2(i,j) * wm_interm_26_triplet_pt2(p,q,j,i)
term(689) = term(689) + wm_interm_146_triplet_pt2(i,j) * wm_interm_28_triplet_pt2(p,q,j,i)
term(690) = term(690) + wm_interm_147_triplet_pt2(i,j) * wm_interm_28_triplet_pt2(p,q,j,i)
term(691) = term(691) + wm_interm_148_triplet_pt2(i,j) * wm_interm_27_triplet_pt2(p,q,j,i)
term(692) = term(692) + wm_interm_148_triplet_pt2(i,j) * wm_interm_26_triplet_pt2(p,q,j,i)
term(693) = term(693) + wm_interm_148_triplet_pt2(i,j) * wm_interm_28_triplet_pt2(p,q,j,i)
term(694) = term(694) + wm_interm_175_triplet_pt2(i,j) * wm_interm_25_triplet_pt2(p,q,j,i)
term(695) = term(695) + wm_interm_176_triplet_pt2(i,j) * wm_interm_25_triplet_pt2(p,q,j,i)
term(696) = term(696) + wm_interm_167_triplet_pt2(i,j) * wm_interm_25_triplet_pt2(p,q,j,i)
term(697) = term(697) + wm_interm_168_triplet_pt2(i,j) * wm_interm_25_triplet_pt2(p,q,j,i)
term(698) = term(698) + wm_interm_175_triplet_pt2(i,j) * wm_interm_27_triplet_pt2(p,q,j,i)
term(699) = term(699) + wm_interm_176_triplet_pt2(i,j) * wm_interm_27_triplet_pt2(p,q,j,i)
term(700) = term(700) + wm_interm_175_triplet_pt2(i,j) * wm_interm_26_triplet_pt2(p,q,j,i)
term(701) = term(701) + wm_interm_176_triplet_pt2(i,j) * wm_interm_26_triplet_pt2(p,q,j,i)
term(702) = term(702) + wm_interm_175_triplet_pt2(i,j) * wm_interm_28_triplet_pt2(p,q,j,i)
term(703) = term(703) + wm_interm_176_triplet_pt2(i,j) * wm_interm_28_triplet_pt2(p,q,j,i)
term(704) = term(704) + wm_interm_167_triplet_pt2(i,j) * wm_interm_27_triplet_pt2(p,q,j,i)
term(705) = term(705) + wm_interm_168_triplet_pt2(i,j) * wm_interm_27_triplet_pt2(p,q,j,i)
term(706) = term(706) + wm_interm_167_triplet_pt2(i,j) * wm_interm_26_triplet_pt2(p,q,j,i)
term(707) = term(707) + wm_interm_168_triplet_pt2(i,j) * wm_interm_26_triplet_pt2(p,q,j,i)
term(708) = term(708) + wm_interm_167_triplet_pt2(i,j) * wm_interm_28_triplet_pt2(p,q,j,i)
term(709) = term(709) + wm_interm_168_triplet_pt2(i,j) * wm_interm_28_triplet_pt2(p,q,j,i)
end do 
end do 

term(638) = term(638) * (2.0d+0) 
term(639) = term(639) * (-4.0d+0) 
term(640) = term(640) * (-0.5d+0) 
term(642) = term(642) * (-0.5d+0) 
term(644) = term(644) * (-0.5d+0) 
term(645) = term(645) * (-0.5d+0) 
term(647) = term(647) * (-0.5d+0) 
term(649) = term(649) * (-2.0d+0) 
term(652) = term(652) * (-2.0d+0) 
term(654) = term(654) * (-2.0d+0) 
term(655) = term(655) * (2.0d+0) 
term(656) = term(656) * (-2.0d+0) 
term(657) = term(657) * (2.0d+0) 
term(658) = term(658) * (-2.0d+0) 
term(659) = term(659) * (2.0d+0) 
term(660) = term(660) * (4.0d+0) 
term(661) = term(661) * (-4.0d+0) 
term(662) = term(662) * (4.0d+0) 
term(663) = term(663) * (-4.0d+0) 
term(664) = term(664) * (-2.0d+0) 
term(665) = term(665) * (2.0d+0) 
term(666) = term(666) * (4.0d+0) 
term(667) = term(667) * (-4.0d+0) 
term(668) = term(668) * (4.0d+0) 
term(669) = term(669) * (-4.0d+0) 
term(670) = term(670) * (-1.0d+0) 
term(671) = term(671) * (2.0d+0) 
term(672) = term(672) * (-1.0d+0) 
term(673) = term(673) * (-1.0d+0) 
term(674) = term(674) * (2.0d+0) 
term(675) = term(675) * (-1.0d+0) 
term(676) = term(676) * (-1.0d+0) 
term(677) = term(677) * (2.0d+0) 
term(678) = term(678) * (-1.0d+0) 
term(679) = term(679) * (2.0d+0) 
term(680) = term(680) * (-4.0d+0) 
term(681) = term(681) * (2.0d+0) 
term(682) = term(682) * (2.0d+0) 
term(683) = term(683) * (-4.0d+0) 
term(684) = term(684) * (2.0d+0) 
term(685) = term(685) * (-1.0d+0) 
term(686) = term(686) * (2.0d+0) 
term(687) = term(687) * (2.0d+0) 
term(688) = term(688) * (-4.0d+0) 
term(689) = term(689) * (2.0d+0) 
term(690) = term(690) * (-4.0d+0) 
term(691) = term(691) * (-1.0d+0) 
term(692) = term(692) * (2.0d+0) 
term(693) = term(693) * (2.0d+0) 
term(694) = term(694) * (-4.0d+0) 
term(695) = term(695) * (4.0d+0) 
term(696) = term(696) * (-4.0d+0) 
term(697) = term(697) * (4.0d+0) 
term(698) = term(698) * (-4.0d+0) 
term(699) = term(699) * (4.0d+0) 
term(700) = term(700) * (8.0d+0) 
term(701) = term(701) * (-8.0d+0) 
term(702) = term(702) * (8.0d+0) 
term(703) = term(703) * (-8.0d+0) 
term(704) = term(704) * (-4.0d+0) 
term(705) = term(705) * (4.0d+0) 
term(706) = term(706) * (8.0d+0) 
term(707) = term(707) * (-8.0d+0) 
term(708) = term(708) * (8.0d+0) 
term(709) = term(709) * (-8.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
term(710) = term(710) + wm_interm_25_triplet_pt2(p,q,i,j) * wm_interm_88_triplet_pt2(j,i)
term(711) = term(711) + wm_interm_27_triplet_pt2(p,q,i,j) * wm_interm_88_triplet_pt2(j,i)
term(712) = term(712) + wm_interm_26_triplet_pt2(p,q,i,j) * wm_interm_88_triplet_pt2(j,i)
term(713) = term(713) + wm_interm_28_triplet_pt2(p,q,i,j) * wm_interm_88_triplet_pt2(j,i)
term(714) = term(714) + wm_interm_100_triplet_pt2(q,p,i,j) * wm_interm_1_triplet_pt2(j,i)
term(715) = term(715) + wm_interm_100_triplet_pt2(q,p,i,j) * wm_interm_2_triplet_pt2(j,i)
term(716) = term(716) + wm_interm_101_triplet_pt2(q,p,i,j) * wm_interm_1_triplet_pt2(j,i)
term(717) = term(717) + wm_interm_101_triplet_pt2(q,p,i,j) * wm_interm_2_triplet_pt2(j,i)
term(718) = term(718) + wm_interm_102_triplet_pt2(q,p,i,j) * wm_interm_1_triplet_pt2(j,i)
term(719) = term(719) + wm_interm_103_triplet_pt2(q,p,i,j) * wm_interm_1_triplet_pt2(j,i)
term(720) = term(720) + wm_interm_102_triplet_pt2(q,p,i,j) * wm_interm_2_triplet_pt2(j,i)
term(721) = term(721) + wm_interm_103_triplet_pt2(q,p,i,j) * wm_interm_2_triplet_pt2(j,i)
term(722) = term(722) + wm_interm_104_triplet_pt2(q,p,i,j) * wm_interm_1_triplet_pt2(j,i)
term(723) = term(723) + wm_interm_104_triplet_pt2(q,p,i,j) * wm_interm_2_triplet_pt2(j,i)
term(724) = term(724) + wm_interm_115_triplet_pt2(q,p,i,j) * wm_interm_1_triplet_pt2(j,i)
term(725) = term(725) + wm_interm_115_triplet_pt2(q,p,i,j) * wm_interm_2_triplet_pt2(j,i)
term(726) = term(726) + wm_interm_116_triplet_pt2(q,p,i,j) * wm_interm_1_triplet_pt2(j,i)
term(727) = term(727) + wm_interm_116_triplet_pt2(q,p,i,j) * wm_interm_2_triplet_pt2(j,i)
term(728) = term(728) + wm_interm_107_triplet_pt2(q,p,i,j) * wm_interm_1_triplet_pt2(j,i)
term(729) = term(729) + wm_interm_107_triplet_pt2(q,p,i,j) * wm_interm_2_triplet_pt2(j,i)
term(730) = term(730) + wm_interm_105_triplet_pt2(q,p,i,j) * wm_interm_1_triplet_pt2(j,i)
term(731) = term(731) + wm_interm_105_triplet_pt2(q,p,i,j) * wm_interm_2_triplet_pt2(j,i)
term(732) = term(732) + wm_interm_106_triplet_pt2(q,p,i,j) * wm_interm_1_triplet_pt2(j,i)
term(733) = term(733) + wm_interm_106_triplet_pt2(q,p,i,j) * wm_interm_2_triplet_pt2(j,i)
term(734) = term(734) + wm_interm_117_triplet_pt2(q,p,i,j) * wm_interm_1_triplet_pt2(j,i)
term(735) = term(735) + wm_interm_117_triplet_pt2(q,p,i,j) * wm_interm_2_triplet_pt2(j,i)
term(736) = term(736) + wm_interm_18_triplet_pt2(p,q,i,j) * wm_interm_57_triplet_pt2(i,j)
term(737) = term(737) + wm_interm_20_triplet_pt2(p,q,i,j) * wm_interm_57_triplet_pt2(i,j)
term(738) = term(738) + wm_interm_15_triplet_pt2(p,q,i,j) * wm_interm_57_triplet_pt2(i,j)
term(739) = term(739) + wm_interm_57_triplet_pt2(i,j) * wm_interm_9_triplet_pt2(p,q,i,j)
term(740) = term(740) + wm_interm_57_triplet_pt2(i,j) * wm_interm_8_triplet_pt2(p,q,i,j)
term(741) = term(741) + wm_interm_24_triplet_pt2(p,q,i,j) * wm_interm_57_triplet_pt2(i,j)
term(742) = term(742) + wm_interm_18_triplet_pt2(p,q,i,j) * wm_interm_61_triplet_pt2(i,j)
term(743) = term(743) + wm_interm_20_triplet_pt2(p,q,i,j) * wm_interm_61_triplet_pt2(i,j)
term(744) = term(744) + wm_interm_15_triplet_pt2(p,q,i,j) * wm_interm_61_triplet_pt2(i,j)
term(745) = term(745) + wm_interm_61_triplet_pt2(i,j) * wm_interm_9_triplet_pt2(p,q,i,j)
term(746) = term(746) + wm_interm_61_triplet_pt2(i,j) * wm_interm_8_triplet_pt2(p,q,i,j)
term(747) = term(747) + wm_interm_24_triplet_pt2(p,q,i,j) * wm_interm_61_triplet_pt2(i,j)
term(748) = term(748) + wm_interm_18_triplet_pt2(p,q,i,j) * wm_interm_64_triplet_pt2(i,j)
term(749) = term(749) + wm_interm_20_triplet_pt2(p,q,i,j) * wm_interm_64_triplet_pt2(i,j)
term(750) = term(750) + wm_interm_15_triplet_pt2(p,q,i,j) * wm_interm_64_triplet_pt2(i,j)
term(751) = term(751) + wm_interm_64_triplet_pt2(i,j) * wm_interm_9_triplet_pt2(p,q,i,j)
term(752) = term(752) + wm_interm_64_triplet_pt2(i,j) * wm_interm_8_triplet_pt2(p,q,i,j)
term(753) = term(753) + wm_interm_24_triplet_pt2(p,q,i,j) * wm_interm_64_triplet_pt2(i,j)
term(754) = term(754) + wm_interm_125_triplet_pt2(q,p,i,j) * wm_interm_1_triplet_pt2(j,i)
term(755) = term(755) + wm_interm_126_triplet_pt2(q,p,i,j) * wm_interm_1_triplet_pt2(j,i)
term(756) = term(756) + wm_interm_125_triplet_pt2(q,p,i,j) * wm_interm_2_triplet_pt2(j,i)
term(757) = term(757) + wm_interm_126_triplet_pt2(q,p,i,j) * wm_interm_2_triplet_pt2(j,i)
term(758) = term(758) + wm_interm_127_triplet_pt2(q,p,i,j) * wm_interm_1_triplet_pt2(j,i)
term(759) = term(759) + wm_interm_127_triplet_pt2(q,p,i,j) * wm_interm_2_triplet_pt2(j,i)
term(760) = term(760) + wm_interm_128_triplet_pt2(q,p,i,j) * wm_interm_1_triplet_pt2(j,i)
term(761) = term(761) + wm_interm_128_triplet_pt2(q,p,i,j) * wm_interm_2_triplet_pt2(j,i)
term(762) = term(762) + wm_interm_135_triplet_pt2(q,p,i,j) * wm_interm_1_triplet_pt2(j,i)
term(763) = term(763) + wm_interm_135_triplet_pt2(q,p,i,j) * wm_interm_2_triplet_pt2(j,i)
term(764) = term(764) + wm_interm_136_triplet_pt2(q,p,i,j) * wm_interm_1_triplet_pt2(j,i)
term(765) = term(765) + wm_interm_136_triplet_pt2(q,p,i,j) * wm_interm_2_triplet_pt2(j,i)
term(766) = term(766) + wm_interm_129_triplet_pt2(q,p,i,j) * wm_interm_1_triplet_pt2(j,i)
term(767) = term(767) + wm_interm_129_triplet_pt2(q,p,i,j) * wm_interm_2_triplet_pt2(j,i)
term(768) = term(768) + wm_interm_130_triplet_pt2(q,p,i,j) * wm_interm_1_triplet_pt2(j,i)
term(769) = term(769) + wm_interm_130_triplet_pt2(q,p,i,j) * wm_interm_2_triplet_pt2(j,i)
term(770) = term(770) + wm_interm_35_triplet_pt2(p,q,i,j) * wm_interm_57_triplet_pt2(i,j)
term(771) = term(771) + wm_interm_37_triplet_pt2(p,q,i,j) * wm_interm_57_triplet_pt2(i,j)
term(772) = term(772) + wm_interm_12_triplet_pt2(p,q,i,j) * wm_interm_57_triplet_pt2(i,j)
term(773) = term(773) + wm_interm_11_triplet_pt2(p,q,i,j) * wm_interm_57_triplet_pt2(i,j)
term(774) = term(774) + wm_interm_35_triplet_pt2(p,q,i,j) * wm_interm_61_triplet_pt2(i,j)
term(775) = term(775) + wm_interm_37_triplet_pt2(p,q,i,j) * wm_interm_61_triplet_pt2(i,j)
term(776) = term(776) + wm_interm_12_triplet_pt2(p,q,i,j) * wm_interm_61_triplet_pt2(i,j)
term(777) = term(777) + wm_interm_11_triplet_pt2(p,q,i,j) * wm_interm_61_triplet_pt2(i,j)
term(778) = term(778) + wm_interm_35_triplet_pt2(p,q,i,j) * wm_interm_64_triplet_pt2(i,j)
term(779) = term(779) + wm_interm_37_triplet_pt2(p,q,i,j) * wm_interm_64_triplet_pt2(i,j)
term(780) = term(780) + wm_interm_12_triplet_pt2(p,q,i,j) * wm_interm_64_triplet_pt2(i,j)
term(781) = term(781) + wm_interm_11_triplet_pt2(p,q,i,j) * wm_interm_64_triplet_pt2(i,j)
term(782) = term(782) + wm_interm_25_triplet_pt2(p,q,i,j) * wm_interm_96_triplet_pt2(j,i)
term(783) = term(783) + wm_interm_25_triplet_pt2(p,q,i,j) * wm_interm_97_triplet_pt2(j,i)
term(784) = term(784) + wm_interm_25_triplet_pt2(p,q,i,j) * wm_interm_98_triplet_pt2(j,i)
term(785) = term(785) + wm_interm_27_triplet_pt2(p,q,i,j) * wm_interm_96_triplet_pt2(j,i)
term(786) = term(786) + wm_interm_27_triplet_pt2(p,q,i,j) * wm_interm_97_triplet_pt2(j,i)
term(787) = term(787) + wm_interm_26_triplet_pt2(p,q,i,j) * wm_interm_96_triplet_pt2(j,i)
term(788) = term(788) + wm_interm_26_triplet_pt2(p,q,i,j) * wm_interm_97_triplet_pt2(j,i)
term(789) = term(789) + wm_interm_28_triplet_pt2(p,q,i,j) * wm_interm_96_triplet_pt2(j,i)
term(790) = term(790) + wm_interm_28_triplet_pt2(p,q,i,j) * wm_interm_97_triplet_pt2(j,i)
term(791) = term(791) + wm_interm_27_triplet_pt2(p,q,i,j) * wm_interm_98_triplet_pt2(j,i)
term(792) = term(792) + wm_interm_26_triplet_pt2(p,q,i,j) * wm_interm_98_triplet_pt2(j,i)
term(793) = term(793) + wm_interm_28_triplet_pt2(p,q,i,j) * wm_interm_98_triplet_pt2(j,i)
term(794) = term(794) + wm_interm_30_triplet_pt2(i,j) * wm_interm_56_triplet_pt2(p,q,i,j)
term(795) = term(795) + wm_interm_30_triplet_pt2(i,j) * wm_interm_63_triplet_pt2(p,q,i,j)
term(796) = term(796) + wm_interm_30_triplet_pt2(i,j) * wm_interm_53_triplet_pt2(p,q,i,j)
term(797) = term(797) + wm_interm_31_triplet_pt2(i,j) * wm_interm_56_triplet_pt2(p,q,i,j)
term(798) = term(798) + wm_interm_31_triplet_pt2(i,j) * wm_interm_63_triplet_pt2(p,q,i,j)
term(799) = term(799) + wm_interm_31_triplet_pt2(i,j) * wm_interm_53_triplet_pt2(p,q,i,j)
term(800) = term(800) + wm_interm_32_triplet_pt2(i,j) * wm_interm_56_triplet_pt2(p,q,i,j)
term(801) = term(801) + wm_interm_32_triplet_pt2(i,j) * wm_interm_63_triplet_pt2(p,q,i,j)
term(802) = term(802) + wm_interm_32_triplet_pt2(i,j) * wm_interm_53_triplet_pt2(p,q,i,j)
term(803) = term(803) + wm_interm_30_triplet_pt2(i,j) * wm_interm_52_triplet_pt2(p,q,i,j)
term(804) = term(804) + wm_interm_30_triplet_pt2(i,j) * wm_interm_50_triplet_pt2(p,q,i,j)
term(805) = term(805) + wm_interm_30_triplet_pt2(i,j) * wm_interm_47_triplet_pt2(p,q,i,j)
term(806) = term(806) + wm_interm_31_triplet_pt2(i,j) * wm_interm_52_triplet_pt2(p,q,i,j)
term(807) = term(807) + wm_interm_31_triplet_pt2(i,j) * wm_interm_50_triplet_pt2(p,q,i,j)
term(808) = term(808) + wm_interm_31_triplet_pt2(i,j) * wm_interm_47_triplet_pt2(p,q,i,j)
term(809) = term(809) + wm_interm_32_triplet_pt2(i,j) * wm_interm_52_triplet_pt2(p,q,i,j)
term(810) = term(810) + wm_interm_32_triplet_pt2(i,j) * wm_interm_50_triplet_pt2(p,q,i,j)
term(811) = term(811) + wm_interm_32_triplet_pt2(i,j) * wm_interm_47_triplet_pt2(p,q,i,j)
term(812) = term(812) + wm_interm_41_triplet_pt2(i,j) * wm_interm_56_triplet_pt2(p,q,i,j)
term(813) = term(813) + wm_interm_41_triplet_pt2(i,j) * wm_interm_63_triplet_pt2(p,q,i,j)
term(814) = term(814) + wm_interm_41_triplet_pt2(i,j) * wm_interm_53_triplet_pt2(p,q,i,j)
term(815) = term(815) + wm_interm_42_triplet_pt2(i,j) * wm_interm_56_triplet_pt2(p,q,i,j)
term(816) = term(816) + wm_interm_42_triplet_pt2(i,j) * wm_interm_63_triplet_pt2(p,q,i,j)
term(817) = term(817) + wm_interm_42_triplet_pt2(i,j) * wm_interm_53_triplet_pt2(p,q,i,j)
term(818) = term(818) + wm_interm_43_triplet_pt2(i,j) * wm_interm_56_triplet_pt2(p,q,i,j)
term(819) = term(819) + wm_interm_43_triplet_pt2(i,j) * wm_interm_63_triplet_pt2(p,q,i,j)
term(820) = term(820) + wm_interm_43_triplet_pt2(i,j) * wm_interm_53_triplet_pt2(p,q,i,j)
term(821) = term(821) + wm_interm_41_triplet_pt2(i,j) * wm_interm_52_triplet_pt2(p,q,i,j)
term(822) = term(822) + wm_interm_41_triplet_pt2(i,j) * wm_interm_50_triplet_pt2(p,q,i,j)
term(823) = term(823) + wm_interm_41_triplet_pt2(i,j) * wm_interm_47_triplet_pt2(p,q,i,j)
term(824) = term(824) + wm_interm_42_triplet_pt2(i,j) * wm_interm_52_triplet_pt2(p,q,i,j)
term(825) = term(825) + wm_interm_42_triplet_pt2(i,j) * wm_interm_50_triplet_pt2(p,q,i,j)
term(826) = term(826) + wm_interm_42_triplet_pt2(i,j) * wm_interm_47_triplet_pt2(p,q,i,j)
term(827) = term(827) + wm_interm_43_triplet_pt2(i,j) * wm_interm_52_triplet_pt2(p,q,i,j)
term(828) = term(828) + wm_interm_43_triplet_pt2(i,j) * wm_interm_50_triplet_pt2(p,q,i,j)
term(829) = term(829) + wm_interm_43_triplet_pt2(i,j) * wm_interm_47_triplet_pt2(p,q,i,j)
term(830) = term(830) + wm_interm_149_triplet_pt2(q,p,i,j) * wm_interm_1_triplet_pt2(j,i)
term(831) = term(831) + wm_interm_150_triplet_pt2(q,p,i,j) * wm_interm_1_triplet_pt2(j,i)
term(832) = term(832) + wm_interm_149_triplet_pt2(q,p,i,j) * wm_interm_2_triplet_pt2(j,i)
term(833) = term(833) + wm_interm_150_triplet_pt2(q,p,i,j) * wm_interm_2_triplet_pt2(j,i)
term(834) = term(834) + wm_interm_151_triplet_pt2(q,p,i,j) * wm_interm_1_triplet_pt2(j,i)
term(835) = term(835) + wm_interm_151_triplet_pt2(q,p,i,j) * wm_interm_2_triplet_pt2(j,i)
term(836) = term(836) + wm_interm_152_triplet_pt2(q,p,i,j) * wm_interm_1_triplet_pt2(j,i)
term(837) = term(837) + wm_interm_153_triplet_pt2(q,p,i,j) * wm_interm_1_triplet_pt2(j,i)
term(838) = term(838) + wm_interm_152_triplet_pt2(q,p,i,j) * wm_interm_2_triplet_pt2(j,i)
term(839) = term(839) + wm_interm_153_triplet_pt2(q,p,i,j) * wm_interm_2_triplet_pt2(j,i)
term(840) = term(840) + wm_interm_154_triplet_pt2(q,p,i,j) * wm_interm_1_triplet_pt2(j,i)
term(841) = term(841) + wm_interm_154_triplet_pt2(q,p,i,j) * wm_interm_2_triplet_pt2(j,i)
term(842) = term(842) + wm_interm_162_triplet_pt2(q,p,i,j) * wm_interm_1_triplet_pt2(j,i)
term(843) = term(843) + wm_interm_162_triplet_pt2(q,p,i,j) * wm_interm_2_triplet_pt2(j,i)
term(844) = term(844) + wm_interm_155_triplet_pt2(q,p,i,j) * wm_interm_1_triplet_pt2(j,i)
term(845) = term(845) + wm_interm_155_triplet_pt2(q,p,i,j) * wm_interm_2_triplet_pt2(j,i)
term(846) = term(846) + wm_interm_18_triplet_pt2(p,q,i,j) * wm_interm_74_triplet_pt2(i,j)
term(847) = term(847) + wm_interm_20_triplet_pt2(p,q,i,j) * wm_interm_74_triplet_pt2(i,j)
term(848) = term(848) + wm_interm_15_triplet_pt2(p,q,i,j) * wm_interm_74_triplet_pt2(i,j)
term(849) = term(849) + wm_interm_74_triplet_pt2(i,j) * wm_interm_9_triplet_pt2(p,q,i,j)
term(850) = term(850) + wm_interm_74_triplet_pt2(i,j) * wm_interm_8_triplet_pt2(p,q,i,j)
term(851) = term(851) + wm_interm_24_triplet_pt2(p,q,i,j) * wm_interm_74_triplet_pt2(i,j)
term(852) = term(852) + wm_interm_18_triplet_pt2(p,q,i,j) * wm_interm_77_triplet_pt2(i,j)
term(853) = term(853) + wm_interm_20_triplet_pt2(p,q,i,j) * wm_interm_77_triplet_pt2(i,j)
term(854) = term(854) + wm_interm_15_triplet_pt2(p,q,i,j) * wm_interm_77_triplet_pt2(i,j)
term(855) = term(855) + wm_interm_77_triplet_pt2(i,j) * wm_interm_9_triplet_pt2(p,q,i,j)
term(856) = term(856) + wm_interm_77_triplet_pt2(i,j) * wm_interm_8_triplet_pt2(p,q,i,j)
term(857) = term(857) + wm_interm_24_triplet_pt2(p,q,i,j) * wm_interm_77_triplet_pt2(i,j)
term(858) = term(858) + wm_interm_169_triplet_pt2(q,p,i,j) * wm_interm_1_triplet_pt2(j,i)
term(859) = term(859) + wm_interm_170_triplet_pt2(q,p,i,j) * wm_interm_1_triplet_pt2(j,i)
term(860) = term(860) + wm_interm_169_triplet_pt2(q,p,i,j) * wm_interm_2_triplet_pt2(j,i)
term(861) = term(861) + wm_interm_170_triplet_pt2(q,p,i,j) * wm_interm_2_triplet_pt2(j,i)
term(862) = term(862) + wm_interm_171_triplet_pt2(q,p,i,j) * wm_interm_1_triplet_pt2(j,i)
term(863) = term(863) + wm_interm_171_triplet_pt2(q,p,i,j) * wm_interm_2_triplet_pt2(j,i)
term(864) = term(864) + wm_interm_172_triplet_pt2(q,p,i,j) * wm_interm_1_triplet_pt2(j,i)
term(865) = term(865) + wm_interm_172_triplet_pt2(q,p,i,j) * wm_interm_2_triplet_pt2(j,i)
term(866) = term(866) + wm_interm_35_triplet_pt2(p,q,i,j) * wm_interm_74_triplet_pt2(i,j)
term(867) = term(867) + wm_interm_37_triplet_pt2(p,q,i,j) * wm_interm_74_triplet_pt2(i,j)
term(868) = term(868) + wm_interm_12_triplet_pt2(p,q,i,j) * wm_interm_74_triplet_pt2(i,j)
term(869) = term(869) + wm_interm_11_triplet_pt2(p,q,i,j) * wm_interm_74_triplet_pt2(i,j)
term(870) = term(870) + wm_interm_35_triplet_pt2(p,q,i,j) * wm_interm_77_triplet_pt2(i,j)
term(871) = term(871) + wm_interm_37_triplet_pt2(p,q,i,j) * wm_interm_77_triplet_pt2(i,j)
term(872) = term(872) + wm_interm_12_triplet_pt2(p,q,i,j) * wm_interm_77_triplet_pt2(i,j)
term(873) = term(873) + wm_interm_11_triplet_pt2(p,q,i,j) * wm_interm_77_triplet_pt2(i,j)
term(874) = term(874) + wm_interm_30_triplet_pt2(i,j) * wm_interm_73_triplet_pt2(p,q,i,j)
term(875) = term(875) + wm_interm_30_triplet_pt2(i,j) * wm_interm_71_triplet_pt2(p,q,i,j)
term(876) = term(876) + wm_interm_31_triplet_pt2(i,j) * wm_interm_73_triplet_pt2(p,q,i,j)
term(877) = term(877) + wm_interm_31_triplet_pt2(i,j) * wm_interm_71_triplet_pt2(p,q,i,j)
term(878) = term(878) + wm_interm_32_triplet_pt2(i,j) * wm_interm_73_triplet_pt2(p,q,i,j)
term(879) = term(879) + wm_interm_32_triplet_pt2(i,j) * wm_interm_71_triplet_pt2(p,q,i,j)
term(880) = term(880) + wm_interm_30_triplet_pt2(i,j) * wm_interm_69_triplet_pt2(p,q,i,j)
term(881) = term(881) + wm_interm_30_triplet_pt2(i,j) * wm_interm_68_triplet_pt2(p,q,i,j)
term(882) = term(882) + wm_interm_31_triplet_pt2(i,j) * wm_interm_69_triplet_pt2(p,q,i,j)
term(883) = term(883) + wm_interm_31_triplet_pt2(i,j) * wm_interm_68_triplet_pt2(p,q,i,j)
term(884) = term(884) + wm_interm_32_triplet_pt2(i,j) * wm_interm_69_triplet_pt2(p,q,i,j)
term(885) = term(885) + wm_interm_32_triplet_pt2(i,j) * wm_interm_68_triplet_pt2(p,q,i,j)
term(886) = term(886) + wm_interm_41_triplet_pt2(i,j) * wm_interm_73_triplet_pt2(p,q,i,j)
term(887) = term(887) + wm_interm_41_triplet_pt2(i,j) * wm_interm_71_triplet_pt2(p,q,i,j)
term(888) = term(888) + wm_interm_42_triplet_pt2(i,j) * wm_interm_73_triplet_pt2(p,q,i,j)
term(889) = term(889) + wm_interm_42_triplet_pt2(i,j) * wm_interm_71_triplet_pt2(p,q,i,j)
term(890) = term(890) + wm_interm_43_triplet_pt2(i,j) * wm_interm_73_triplet_pt2(p,q,i,j)
term(891) = term(891) + wm_interm_43_triplet_pt2(i,j) * wm_interm_71_triplet_pt2(p,q,i,j)
term(892) = term(892) + wm_interm_41_triplet_pt2(i,j) * wm_interm_69_triplet_pt2(p,q,i,j)
term(893) = term(893) + wm_interm_41_triplet_pt2(i,j) * wm_interm_68_triplet_pt2(p,q,i,j)
term(894) = term(894) + wm_interm_42_triplet_pt2(i,j) * wm_interm_69_triplet_pt2(p,q,i,j)
term(895) = term(895) + wm_interm_42_triplet_pt2(i,j) * wm_interm_68_triplet_pt2(p,q,i,j)
term(896) = term(896) + wm_interm_43_triplet_pt2(i,j) * wm_interm_69_triplet_pt2(p,q,i,j)
term(897) = term(897) + wm_interm_43_triplet_pt2(i,j) * wm_interm_68_triplet_pt2(p,q,i,j)
end do 
end do 

term(710) = term(710) * (2.0d+0) 
term(711) = term(711) * (2.0d+0) 
term(712) = term(712) * (-4.0d+0) 
term(713) = term(713) * (-4.0d+0) 
term(715) = term(715) * (-2.0d+0) 
term(716) = term(716) * (-0.5d+0) 
term(718) = term(718) * (-0.5d+0) 
term(721) = term(721) * (-2.0d+0) 
term(722) = term(722) * (-0.5d+0) 
term(724) = term(724) * (-0.5d+0) 
term(727) = term(727) * (-2.0d+0) 
term(728) = term(728) * (-0.5d+0) 
term(730) = term(730) * (-0.5d+0) 
term(733) = term(733) * (-2.0d+0) 
term(734) = term(734) * (-0.5d+0) 
term(736) = term(736) * (-0.5d+0) 
term(738) = term(738) * (-0.5d+0) 
term(739) = term(739) * (-0.5d+0) 
term(741) = term(741) * (-0.5d+0) 
term(743) = term(743) * (-2.0d+0) 
term(746) = term(746) * (-2.0d+0) 
term(748) = term(748) * (-0.5d+0) 
term(750) = term(750) * (-0.5d+0) 
term(751) = term(751) * (-0.5d+0) 
term(753) = term(753) * (-0.5d+0) 
term(754) = term(754) * (-2.0d+0) 
term(755) = term(755) * (2.0d+0) 
term(756) = term(756) * (4.0d+0) 
term(757) = term(757) * (-4.0d+0) 
term(758) = term(758) * (-2.0d+0) 
term(759) = term(759) * (4.0d+0) 
term(760) = term(760) * (2.0d+0) 
term(761) = term(761) * (-4.0d+0) 
term(762) = term(762) * (-2.0d+0) 
term(763) = term(763) * (4.0d+0) 
term(764) = term(764) * (2.0d+0) 
term(765) = term(765) * (-4.0d+0) 
term(766) = term(766) * (-2.0d+0) 
term(767) = term(767) * (4.0d+0) 
term(768) = term(768) * (2.0d+0) 
term(769) = term(769) * (-4.0d+0) 
term(770) = term(770) * (-2.0d+0) 
term(771) = term(771) * (2.0d+0) 
term(772) = term(772) * (-2.0d+0) 
term(773) = term(773) * (2.0d+0) 
term(774) = term(774) * (4.0d+0) 
term(775) = term(775) * (-4.0d+0) 
term(776) = term(776) * (4.0d+0) 
term(777) = term(777) * (-4.0d+0) 
term(778) = term(778) * (-2.0d+0) 
term(779) = term(779) * (2.0d+0) 
term(780) = term(780) * (-2.0d+0) 
term(781) = term(781) * (2.0d+0) 
term(782) = term(782) * (-0.5d+0) 
term(784) = term(784) * (-0.5d+0) 
term(785) = term(785) * (-0.5d+0) 
term(788) = term(788) * (-2.0d+0) 
term(790) = term(790) * (-2.0d+0) 
term(791) = term(791) * (-0.5d+0) 
term(794) = term(794) * (-0.5d+0) 
term(795) = term(795) * (-0.5d+0) 
term(799) = term(799) * (-2.0d+0) 
term(800) = term(800) * (-0.5d+0) 
term(801) = term(801) * (-0.5d+0) 
term(803) = term(803) * (-0.5d+0) 
term(804) = term(804) * (-0.5d+0) 
term(808) = term(808) * (-2.0d+0) 
term(809) = term(809) * (-0.5d+0) 
term(810) = term(810) * (-0.5d+0) 
term(812) = term(812) * (-1.0d+0) 
term(813) = term(813) * (-1.0d+0) 
term(814) = term(814) * (2.0d+0) 
term(815) = term(815) * (2.0d+0) 
term(816) = term(816) * (2.0d+0) 
term(817) = term(817) * (-4.0d+0) 
term(818) = term(818) * (-1.0d+0) 
term(819) = term(819) * (-1.0d+0) 
term(820) = term(820) * (2.0d+0) 
term(821) = term(821) * (-1.0d+0) 
term(822) = term(822) * (-1.0d+0) 
term(823) = term(823) * (2.0d+0) 
term(824) = term(824) * (2.0d+0) 
term(825) = term(825) * (2.0d+0) 
term(826) = term(826) * (-4.0d+0) 
term(827) = term(827) * (-1.0d+0) 
term(828) = term(828) * (-1.0d+0) 
term(829) = term(829) * (2.0d+0) 
term(830) = term(830) * (-2.0d+0) 
term(831) = term(831) * (2.0d+0) 
term(832) = term(832) * (4.0d+0) 
term(833) = term(833) * (-4.0d+0) 
term(834) = term(834) * (-2.0d+0) 
term(835) = term(835) * (4.0d+0) 
term(836) = term(836) * (-2.0d+0) 
term(837) = term(837) * (2.0d+0) 
term(838) = term(838) * (4.0d+0) 
term(839) = term(839) * (-4.0d+0) 
term(840) = term(840) * (-2.0d+0) 
term(841) = term(841) * (4.0d+0) 
term(842) = term(842) * (2.0d+0) 
term(843) = term(843) * (-4.0d+0) 
term(844) = term(844) * (2.0d+0) 
term(845) = term(845) * (-4.0d+0) 
term(846) = term(846) * (-2.0d+0) 
term(847) = term(847) * (4.0d+0) 
term(848) = term(848) * (-2.0d+0) 
term(849) = term(849) * (-2.0d+0) 
term(850) = term(850) * (4.0d+0) 
term(851) = term(851) * (-2.0d+0) 
term(852) = term(852) * (2.0d+0) 
term(853) = term(853) * (-4.0d+0) 
term(854) = term(854) * (2.0d+0) 
term(855) = term(855) * (2.0d+0) 
term(856) = term(856) * (-4.0d+0) 
term(857) = term(857) * (2.0d+0) 
term(858) = term(858) * (-8.0d+0) 
term(859) = term(859) * (8.0d+0) 
term(860) = term(860) * (16.0d+0) 
term(861) = term(861) * (-16.0d+0) 
term(862) = term(862) * (-8.0d+0) 
term(863) = term(863) * (16.0d+0) 
term(864) = term(864) * (8.0d+0) 
term(865) = term(865) * (-16.0d+0) 
term(866) = term(866) * (-8.0d+0) 
term(867) = term(867) * (8.0d+0) 
term(868) = term(868) * (-8.0d+0) 
term(869) = term(869) * (8.0d+0) 
term(870) = term(870) * (8.0d+0) 
term(871) = term(871) * (-8.0d+0) 
term(872) = term(872) * (8.0d+0) 
term(873) = term(873) * (-8.0d+0) 
term(874) = term(874) * (-2.0d+0) 
term(875) = term(875) * (2.0d+0) 
term(876) = term(876) * (4.0d+0) 
term(877) = term(877) * (-4.0d+0) 
term(878) = term(878) * (-2.0d+0) 
term(879) = term(879) * (2.0d+0) 
term(880) = term(880) * (-2.0d+0) 
term(881) = term(881) * (2.0d+0) 
term(882) = term(882) * (4.0d+0) 
term(883) = term(883) * (-4.0d+0) 
term(884) = term(884) * (-2.0d+0) 
term(885) = term(885) * (2.0d+0) 
term(886) = term(886) * (-4.0d+0) 
term(887) = term(887) * (4.0d+0) 
term(888) = term(888) * (8.0d+0) 
term(889) = term(889) * (-8.0d+0) 
term(890) = term(890) * (-4.0d+0) 
term(891) = term(891) * (4.0d+0) 
term(892) = term(892) * (-4.0d+0) 
term(893) = term(893) * (4.0d+0) 
term(894) = term(894) * (8.0d+0) 
term(895) = term(895) * (-8.0d+0) 
term(896) = term(896) * (-4.0d+0) 
term(897) = term(897) * (4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(898) = term(898) + r2p(vrdav_Rl, q,i,a,j) * r2p(vrdav_Rr, p,j,b,i) * wm_interm_4_triplet_pt2(b,a)
term(899) = term(899) + r2p(vrdav_Rl, q,i,a,j) * r2p(vrdav_Rr, p,j,b,i) * wm_interm_5_triplet_pt2(b,a)
term(900) = term(900) + r2p(vrdav_Rr, a,i,p,j) * s2(b,q,j,i) * wm_interm_55_triplet_pt2(a,b)
term(901) = term(901) + r2p(vrdav_Rr, a,i,p,j) * s2(b,q,j,i) * wm_interm_60_triplet_pt2(a,b)
term(902) = term(902) + r2p(vrdav_Rr, a,i,p,j) * s2(b,q,j,i) * wm_interm_62_triplet_pt2(a,b)
term(903) = term(903) + r2m(vrdav_Rr, a,i,p,j) * s2(b,q,j,i) * wm_interm_55_triplet_pt2(a,b)
term(904) = term(904) + r2m(vrdav_Rr, a,i,p,j) * s2(b,q,j,i) * wm_interm_60_triplet_pt2(a,b)
term(905) = term(905) + r2m(vrdav_Rr, a,i,p,j) * s2(b,q,j,i) * wm_interm_62_triplet_pt2(a,b)
term(906) = term(906) + r2p(vrdav_Rl, a,i,p,j) * t2(b,q,j,i) * wm_interm_23_triplet_pt2(a,b)
term(907) = term(907) + r2p(vrdav_Rl, a,i,p,j) * t2(b,q,j,i) * wm_interm_17_triplet_pt2(a,b)
term(908) = term(908) + r2p(vrdav_Rl, a,i,p,j) * t2(b,q,j,i) * wm_interm_19_triplet_pt2(a,b)
term(909) = term(909) + r2p(vrdav_Rl, a,i,p,j) * t2(b,q,j,i) * wm_interm_34_triplet_pt2(a,b)
term(910) = term(910) + r2p(vrdav_Rl, a,i,p,j) * t2(b,q,j,i) * wm_interm_36_triplet_pt2(a,b)
term(911) = term(911) + r2m(vrdav_Rl, a,j,q,i) * r2p(vrdav_Rr, p,i,b,j) * wm_interm_4_triplet_pt2(b,a)
term(912) = term(912) + r2m(vrdav_Rl, a,j,q,i) * r2p(vrdav_Rr, p,i,b,j) * wm_interm_5_triplet_pt2(b,a)
term(913) = term(913) + r2p(vrdav_Rr, a,i,p,j) * s2(b,q,j,i) * wm_interm_72_triplet_pt2(a,b)
term(914) = term(914) + r2p(vrdav_Rr, a,i,p,j) * s2(b,q,j,i) * wm_interm_76_triplet_pt2(a,b)
term(915) = term(915) + r2m(vrdav_Rr, a,i,p,j) * s2(b,q,j,i) * wm_interm_72_triplet_pt2(a,b)
term(916) = term(916) + r2m(vrdav_Rr, a,i,p,j) * s2(b,q,j,i) * wm_interm_76_triplet_pt2(a,b)
term(917) = term(917) + r2m(vrdav_Rl, a,i,p,j) * t2(b,q,j,i) * wm_interm_23_triplet_pt2(a,b)
term(918) = term(918) + r2m(vrdav_Rl, a,i,p,j) * t2(b,q,j,i) * wm_interm_17_triplet_pt2(a,b)
term(919) = term(919) + r2m(vrdav_Rl, a,i,p,j) * t2(b,q,j,i) * wm_interm_19_triplet_pt2(a,b)
term(920) = term(920) + r2m(vrdav_Rl, a,i,p,j) * t2(b,q,j,i) * wm_interm_34_triplet_pt2(a,b)
term(921) = term(921) + r2m(vrdav_Rl, a,i,p,j) * t2(b,q,j,i) * wm_interm_36_triplet_pt2(a,b)
end do 
end do 
end do 
end do 

term(898) = term(898) * (-0.5d+0) 
term(900) = term(900) * (-0.5d+0) 
term(902) = term(902) * (-0.5d+0) 
term(903) = term(903) * (-2.0d+0) 
term(904) = term(904) * (4.0d+0) 
term(905) = term(905) * (-2.0d+0) 
term(906) = term(906) * (-0.5d+0) 
term(907) = term(907) * (-0.5d+0) 
term(909) = term(909) * (-2.0d+0) 
term(910) = term(910) * (2.0d+0) 
term(911) = term(911) * (2.0d+0) 
term(912) = term(912) * (-4.0d+0) 
term(913) = term(913) * (-2.0d+0) 
term(914) = term(914) * (2.0d+0) 
term(915) = term(915) * (-8.0d+0) 
term(916) = term(916) * (8.0d+0) 
term(917) = term(917) * (-2.0d+0) 
term(918) = term(918) * (-2.0d+0) 
term(919) = term(919) * (4.0d+0) 
term(920) = term(920) * (-8.0d+0) 
term(921) = term(921) * (8.0d+0) 

do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(922) = term(922) + r2p(vrdav_Rl, q,i,a,j) * r2p(vrdav_Rr, p,i,b,j) * wm_interm_4_triplet_pt2(b,a)
term(923) = term(923) + r2p(vrdav_Rl, q,i,a,j) * r2p(vrdav_Rr, p,i,b,j) * wm_interm_5_triplet_pt2(b,a)
term(924) = term(924) + r2p(vrdav_Rl, a,i,q,j) * r2p(vrdav_Rr, p,i,b,j) * wm_interm_4_triplet_pt2(b,a)
term(925) = term(925) + r2p(vrdav_Rl, a,i,q,j) * r2p(vrdav_Rr, p,i,b,j) * wm_interm_5_triplet_pt2(b,a)
term(926) = term(926) + r2m(vrdav_Rl, a,i,q,j) * r2p(vrdav_Rr, p,i,b,j) * wm_interm_4_triplet_pt2(b,a)
term(927) = term(927) + r2m(vrdav_Rl, a,i,q,j) * r2p(vrdav_Rr, p,i,b,j) * wm_interm_5_triplet_pt2(b,a)
end do 
end do 
end do 
end do 

term(923) = term(923) * (-2.0d+0) 
term(924) = term(924) * (-0.5d+0) 
term(926) = term(926) * (-1.0d+0) 
term(927) = term(927) * (2.0d+0) 

do l = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
do i = 1, nocc 
do k = 1, nocc 
term(928) = term(928) + r2p(vrdav_Rl, q,i,a,j) * r2p(vrdav_Rr, p,k,a,l) * wm_interm_16_triplet_pt2(l,k,i,j)
term(929) = term(929) + r2p(vrdav_Rl, q,i,a,j) * r2p(vrdav_Rr, p,k,a,l) * wm_interm_16_triplet_pt2(k,l,i,j)
term(930) = term(930) + r2p(vrdav_Rl, a,i,q,j) * r2p(vrdav_Rr, p,k,a,l) * wm_interm_16_triplet_pt2(k,l,i,j)
term(931) = term(931) + r2p(vrdav_Rl, q,i,a,j) * r2p(vrdav_Rr, a,k,p,l) * wm_interm_16_triplet_pt2(k,l,i,j)
term(932) = term(932) + r2p(vrdav_Rl, q,i,a,j) * r2m(vrdav_Rr, a,k,p,l) * wm_interm_16_triplet_pt2(k,l,i,j)
term(933) = term(933) + r2p(vrdav_Rl, q,i,a,j) * r2m(vrdav_Rr, a,k,p,l) * wm_interm_16_triplet_pt2(l,k,i,j)
term(934) = term(934) + r2m(vrdav_Rl, a,i,q,j) * r2p(vrdav_Rr, p,k,a,l) * wm_interm_16_triplet_pt2(k,l,i,j)
end do 
end do 
end do 
end do 
end do 

term(928) = term(928) * (-0.5d+0) 
term(930) = term(930) * (-0.5d+0) 
term(931) = term(931) * (-0.5d+0) 
term(932) = term(932) * (-2.0d+0) 
term(933) = term(933) * (2.0d+0) 
term(934) = term(934) * (-1.0d+0) 

do j = 1, nocc 
do a = nocc + 1, nactive 
do i = 1, nocc 
do b = nocc + 1, nactive 
term(935) = term(935) + r2p(vrdav_Rl, q,i,a,j) * r2p(vrdav_Rr, b,i,p,j) * wm_interm_4_triplet_pt2(b,a)
term(936) = term(936) + r2p(vrdav_Rl, q,i,a,j) * r2p(vrdav_Rr, b,i,p,j) * wm_interm_5_triplet_pt2(b,a)
term(937) = term(937) + r2p(vrdav_Rl, a,i,q,j) * r2p(vrdav_Rr, b,i,p,j) * wm_interm_4_triplet_pt2(b,a)
term(938) = term(938) + r2p(vrdav_Rl, a,i,q,j) * r2p(vrdav_Rr, b,i,p,j) * wm_interm_5_triplet_pt2(b,a)
term(939) = term(939) + r2p(vrdav_Rl, q,i,a,j) * r2m(vrdav_Rr, b,i,p,j) * wm_interm_4_triplet_pt2(b,a)
term(940) = term(940) + r2p(vrdav_Rl, q,i,a,j) * r2m(vrdav_Rr, b,i,p,j) * wm_interm_5_triplet_pt2(b,a)
term(941) = term(941) + r2p(vrdav_Rl, a,i,q,j) * r2m(vrdav_Rr, b,i,p,j) * wm_interm_4_triplet_pt2(b,a)
term(942) = term(942) + r2p(vrdav_Rl, a,i,q,j) * r2m(vrdav_Rr, b,i,p,j) * wm_interm_5_triplet_pt2(b,a)
term(943) = term(943) + r2m(vrdav_Rl, a,i,q,j) * r2p(vrdav_Rr, b,i,p,j) * wm_interm_4_triplet_pt2(b,a)
term(944) = term(944) + r2m(vrdav_Rl, a,i,q,j) * r2p(vrdav_Rr, b,i,p,j) * wm_interm_5_triplet_pt2(b,a)
term(945) = term(945) + r2m(vrdav_Rl, a,i,q,j) * r2m(vrdav_Rr, b,i,p,j) * wm_interm_4_triplet_pt2(b,a)
term(946) = term(946) + r2m(vrdav_Rl, a,i,q,j) * r2m(vrdav_Rr, b,i,p,j) * wm_interm_5_triplet_pt2(b,a)
end do 
end do 
end do 
end do 

term(935) = term(935) * (-0.5d+0) 
term(938) = term(938) * (-2.0d+0) 
term(939) = term(939) * (-2.0d+0) 
term(940) = term(940) * (4.0d+0) 
term(941) = term(941) * (2.0d+0) 
term(942) = term(942) * (-4.0d+0) 
term(943) = term(943) * (2.0d+0) 
term(944) = term(944) * (-4.0d+0) 
term(945) = term(945) * (4.0d+0) 
term(946) = term(946) * (-8.0d+0) 

do l = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(947) = term(947) + r2p(vrdav_Rl, a,i,q,j) * r2p(vrdav_Rr, a,k,p,l) * wm_interm_16_triplet_pt2(l,k,i,j)
term(948) = term(948) + r2p(vrdav_Rl, a,i,q,j) * r2p(vrdav_Rr, a,k,p,l) * wm_interm_16_triplet_pt2(k,l,i,j)
term(949) = term(949) + r2p(vrdav_Rl, a,i,q,j) * r2m(vrdav_Rr, a,k,p,l) * wm_interm_16_triplet_pt2(l,k,i,j)
term(950) = term(950) + r2p(vrdav_Rl, a,i,q,j) * r2m(vrdav_Rr, a,k,p,l) * wm_interm_16_triplet_pt2(k,l,i,j)
term(951) = term(951) + r2m(vrdav_Rl, a,i,q,j) * r2p(vrdav_Rr, a,k,p,l) * wm_interm_16_triplet_pt2(l,k,i,j)
term(952) = term(952) + r2m(vrdav_Rl, a,i,q,j) * r2p(vrdav_Rr, a,k,p,l) * wm_interm_16_triplet_pt2(k,l,i,j)
term(953) = term(953) + r2m(vrdav_Rl, a,j,q,i) * r2p(vrdav_Rr, a,k,p,l) * wm_interm_16_triplet_pt2(k,l,i,j)
term(954) = term(954) + r2m(vrdav_Rl, a,j,q,i) * r2m(vrdav_Rr, a,k,p,l) * wm_interm_16_triplet_pt2(k,l,i,j)
term(955) = term(955) + r2m(vrdav_Rl, a,j,q,i) * r2m(vrdav_Rr, a,k,p,l) * wm_interm_16_triplet_pt2(l,k,i,j)
term(956) = term(956) + r2m(vrdav_Rl, a,i,q,j) * r2m(vrdav_Rr, a,k,p,l) * wm_interm_16_triplet_pt2(l,k,i,j)
term(957) = term(957) + r2m(vrdav_Rl, a,i,q,j) * r2m(vrdav_Rr, a,k,p,l) * wm_interm_16_triplet_pt2(k,l,i,j)
term(958) = term(958) + r2m(vrdav_Rl, a,k,p,j) * t2(a,q,l,i) * wm_interm_29_triplet_pt2(k,j,i,l)
term(959) = term(959) + r2m(vrdav_Rl, a,k,p,j) * t2(a,q,l,i) * wm_interm_29_triplet_pt2(j,k,i,l)
term(960) = term(960) + r2m(vrdav_Rl, a,k,p,j) * t2(a,q,l,i) * wm_interm_40_triplet_pt2(k,j,i,l)
term(961) = term(961) + r2m(vrdav_Rl, a,k,p,j) * t2(a,q,l,i) * wm_interm_40_triplet_pt2(j,k,i,l)
end do 
end do 
end do 
end do 
end do 

term(947) = term(947) * (-0.5d+0) 
term(949) = term(949) * (-2.0d+0) 
term(950) = term(950) * (2.0d+0) 
term(951) = term(951) * (-1.0d+0) 
term(952) = term(952) * (2.0d+0) 
term(953) = term(953) * (-1.0d+0) 
term(954) = term(954) * (-4.0d+0) 
term(955) = term(955) * (4.0d+0) 
term(956) = term(956) * (-4.0d+0) 
term(957) = term(957) * (4.0d+0) 
term(958) = term(958) * (-1.0d+0) 
term(959) = term(959) * (2.0d+0) 
term(960) = term(960) * (-2.0d+0) 
term(961) = term(961) * (4.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do j = 1, nocc 
term(962) = term(962) + r2m(vrdav_Rl, a,j,q,i) * r2p(vrdav_Rr, p,j,b,i) * wm_interm_4_triplet_pt2(b,a)
term(963) = term(963) + r2m(vrdav_Rl, a,j,q,i) * r2p(vrdav_Rr, p,j,b,i) * wm_interm_5_triplet_pt2(b,a)
end do 
end do 
end do 
end do 

term(962) = term(962) * (-1.0d+0) 
term(963) = term(963) * (2.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(964) = term(964) + r2p(vrdav_Rl, a,i,q,j) * r2p(vrdav_Rr, b,j,p,i) * wm_interm_4_triplet_pt2(b,a)
term(965) = term(965) + r2p(vrdav_Rl, a,i,q,j) * r2p(vrdav_Rr, b,j,p,i) * wm_interm_5_triplet_pt2(b,a)
term(966) = term(966) + r2p(vrdav_Rl, q,i,a,j) * r2m(vrdav_Rr, b,j,p,i) * wm_interm_4_triplet_pt2(b,a)
term(967) = term(967) + r2p(vrdav_Rl, q,i,a,j) * r2m(vrdav_Rr, b,j,p,i) * wm_interm_5_triplet_pt2(b,a)
term(968) = term(968) + r2p(vrdav_Rl, a,i,q,j) * r2m(vrdav_Rr, b,j,p,i) * wm_interm_4_triplet_pt2(b,a)
term(969) = term(969) + r2p(vrdav_Rl, a,i,q,j) * r2m(vrdav_Rr, b,j,p,i) * wm_interm_5_triplet_pt2(b,a)
term(970) = term(970) + s2(a,p,i,j) * t2(b,q,j,i) * wm_interm_120_triplet_pt2(b,a)
term(971) = term(971) + s2(a,p,i,j) * t2(b,q,j,i) * wm_interm_121_triplet_pt2(b,a)
term(972) = term(972) + s2(a,p,i,j) * t2(b,q,j,i) * wm_interm_110_triplet_pt2(b,a)
term(973) = term(973) + s2(a,p,i,j) * t2(b,q,j,i) * wm_interm_108_triplet_pt2(b,a)
term(974) = term(974) + s2(a,p,i,j) * t2(b,q,j,i) * wm_interm_109_triplet_pt2(b,a)
term(975) = term(975) + s2(a,p,i,j) * t2(b,q,j,i) * wm_interm_119_triplet_pt2(b,a)
term(976) = term(976) + s2(a,p,i,j) * t2(b,q,j,i) * wm_interm_137_triplet_pt2(b,a)
term(977) = term(977) + s2(a,p,i,j) * t2(b,q,j,i) * wm_interm_138_triplet_pt2(b,a)
term(978) = term(978) + s2(a,p,i,j) * t2(b,q,j,i) * wm_interm_131_triplet_pt2(b,a)
term(979) = term(979) + s2(a,p,i,j) * t2(b,q,j,i) * wm_interm_132_triplet_pt2(b,a)
term(980) = term(980) + r2m(vrdav_Rl, a,j,q,i) * r2p(vrdav_Rr, b,i,p,j) * wm_interm_4_triplet_pt2(b,a)
term(981) = term(981) + r2m(vrdav_Rl, a,j,q,i) * r2p(vrdav_Rr, b,i,p,j) * wm_interm_5_triplet_pt2(b,a)
term(982) = term(982) + r2m(vrdav_Rl, a,i,q,j) * r2p(vrdav_Rr, b,j,p,i) * wm_interm_4_triplet_pt2(b,a)
term(983) = term(983) + r2m(vrdav_Rl, a,i,q,j) * r2p(vrdav_Rr, b,j,p,i) * wm_interm_5_triplet_pt2(b,a)
term(984) = term(984) + r2m(vrdav_Rl, a,j,q,i) * r2m(vrdav_Rr, b,i,p,j) * wm_interm_4_triplet_pt2(b,a)
term(985) = term(985) + r2m(vrdav_Rl, a,j,q,i) * r2m(vrdav_Rr, b,i,p,j) * wm_interm_5_triplet_pt2(b,a)
term(986) = term(986) + r2m(vrdav_Rl, a,i,q,j) * r2m(vrdav_Rr, b,j,p,i) * wm_interm_4_triplet_pt2(b,a)
term(987) = term(987) + r2m(vrdav_Rl, a,i,q,j) * r2m(vrdav_Rr, b,j,p,i) * wm_interm_5_triplet_pt2(b,a)
term(988) = term(988) + s2(a,p,i,j) * t2(b,q,j,i) * wm_interm_164_triplet_pt2(b,a)
term(989) = term(989) + s2(a,p,i,j) * t2(b,q,j,i) * wm_interm_165_triplet_pt2(b,a)
term(990) = term(990) + s2(a,p,i,j) * t2(b,q,j,i) * wm_interm_158_triplet_pt2(b,a)
term(991) = term(991) + s2(a,p,i,j) * t2(b,q,j,i) * wm_interm_156_triplet_pt2(b,a)
term(992) = term(992) + s2(a,p,i,j) * t2(b,q,j,i) * wm_interm_157_triplet_pt2(b,a)
term(993) = term(993) + s2(a,p,i,j) * t2(b,q,j,i) * wm_interm_163_triplet_pt2(b,a)
term(994) = term(994) + s2(a,p,i,j) * t2(b,q,j,i) * wm_interm_177_triplet_pt2(b,a)
term(995) = term(995) + s2(a,p,i,j) * t2(b,q,j,i) * wm_interm_178_triplet_pt2(b,a)
term(996) = term(996) + s2(a,p,i,j) * t2(b,q,j,i) * wm_interm_173_triplet_pt2(b,a)
term(997) = term(997) + s2(a,p,i,j) * t2(b,q,j,i) * wm_interm_174_triplet_pt2(b,a)
end do 
end do 
end do 
end do 

term(964) = term(964) * (-0.5d+0) 
term(966) = term(966) * (2.0d+0) 
term(967) = term(967) * (-4.0d+0) 
term(968) = term(968) * (-2.0d+0) 
term(969) = term(969) * (4.0d+0) 
term(970) = term(970) * (-0.5d+0) 
term(972) = term(972) * (-0.5d+0) 
term(973) = term(973) * (-0.5d+0) 
term(975) = term(975) * (-0.5d+0) 
term(976) = term(976) * (-2.0d+0) 
term(977) = term(977) * (2.0d+0) 
term(978) = term(978) * (-2.0d+0) 
term(979) = term(979) * (2.0d+0) 
term(980) = term(980) * (-1.0d+0) 
term(981) = term(981) * (2.0d+0) 
term(982) = term(982) * (-1.0d+0) 
term(983) = term(983) * (2.0d+0) 
term(984) = term(984) * (-4.0d+0) 
term(985) = term(985) * (8.0d+0) 
term(986) = term(986) * (-4.0d+0) 
term(987) = term(987) * (8.0d+0) 
term(988) = term(988) * (-1.0d+0) 
term(989) = term(989) * (2.0d+0) 
term(990) = term(990) * (-1.0d+0) 
term(991) = term(991) * (-1.0d+0) 
term(992) = term(992) * (2.0d+0) 
term(993) = term(993) * (-1.0d+0) 
term(994) = term(994) * (-4.0d+0) 
term(995) = term(995) * (4.0d+0) 
term(996) = term(996) * (-4.0d+0) 
term(997) = term(997) * (4.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(998) = term(998) + r2p(vrdav_Rr, p,i,a,j) * s2(a,q,l,k) * wm_interm_58_triplet_pt2(j,i,k,l)
term(999) = term(999) + r2p(vrdav_Rr, p,i,a,j) * s2(a,q,l,k) * wm_interm_58_triplet_pt2(i,j,k,l)
term(1000) = term(1000) + r2p(vrdav_Rr, p,i,a,j) * s2(a,q,l,k) * wm_interm_75_triplet_pt2(j,i,k,l)
term(1001) = term(1001) + r2p(vrdav_Rr, p,i,a,j) * s2(a,q,l,k) * wm_interm_75_triplet_pt2(i,j,k,l)
end do 
end do 
end do 
end do 
end do 

term(998) = term(998) * (-0.5d+0) 
term(1000) = term(1000) * (-1.0d+0) 
term(1001) = term(1001) * (2.0d+0) 

term(1002) = term(1002) + wm_interm_4_triplet_pt2(p,q) * wm_interm_89_triplet_pt2
term(1003) = term(1003) + wm_interm_5_triplet_pt2(p,q) * wm_interm_89_triplet_pt2
term(1004) = term(1004) + wm_interm_139_triplet_pt2 * wm_interm_4_triplet_pt2(p,q)
term(1005) = term(1005) + wm_interm_140_triplet_pt2 * wm_interm_4_triplet_pt2(p,q)
term(1006) = term(1006) + wm_interm_141_triplet_pt2 * wm_interm_4_triplet_pt2(p,q)
term(1007) = term(1007) + wm_interm_139_triplet_pt2 * wm_interm_5_triplet_pt2(p,q)
term(1008) = term(1008) + wm_interm_140_triplet_pt2 * wm_interm_5_triplet_pt2(p,q)
term(1009) = term(1009) + wm_interm_141_triplet_pt2 * wm_interm_5_triplet_pt2(p,q)
term(1010) = term(1010) + wm_interm_142_triplet_pt2 * wm_interm_4_triplet_pt2(p,q)
term(1011) = term(1011) + wm_interm_143_triplet_pt2 * wm_interm_4_triplet_pt2(p,q)
term(1012) = term(1012) + wm_interm_142_triplet_pt2 * wm_interm_5_triplet_pt2(p,q)
term(1013) = term(1013) + wm_interm_143_triplet_pt2 * wm_interm_5_triplet_pt2(p,q)
term(1014) = term(1014) + wm_interm_179_triplet_pt2 * wm_interm_4_triplet_pt2(p,q)
term(1015) = term(1015) + wm_interm_180_triplet_pt2 * wm_interm_4_triplet_pt2(p,q)
term(1016) = term(1016) + wm_interm_181_triplet_pt2 * wm_interm_4_triplet_pt2(p,q)
term(1017) = term(1017) + wm_interm_179_triplet_pt2 * wm_interm_5_triplet_pt2(p,q)
term(1018) = term(1018) + wm_interm_180_triplet_pt2 * wm_interm_5_triplet_pt2(p,q)
term(1019) = term(1019) + wm_interm_181_triplet_pt2 * wm_interm_5_triplet_pt2(p,q)
term(1020) = term(1020) + wm_interm_182_triplet_pt2 * wm_interm_4_triplet_pt2(p,q)
term(1021) = term(1021) + wm_interm_183_triplet_pt2 * wm_interm_4_triplet_pt2(p,q)
term(1022) = term(1022) + wm_interm_182_triplet_pt2 * wm_interm_5_triplet_pt2(p,q)
term(1023) = term(1023) + wm_interm_183_triplet_pt2 * wm_interm_5_triplet_pt2(p,q)

term(1002) = term(1002) * (-4.0d+0) 
term(1003) = term(1003) * (8.0d+0) 
term(1005) = term(1005) * (-2.0d+0) 
term(1007) = term(1007) * (-2.0d+0) 
term(1008) = term(1008) * (4.0d+0) 
term(1009) = term(1009) * (-2.0d+0) 
term(1010) = term(1010) * (4.0d+0) 
term(1011) = term(1011) * (-4.0d+0) 
term(1012) = term(1012) * (-8.0d+0) 
term(1013) = term(1013) * (8.0d+0) 
term(1014) = term(1014) * (2.0d+0) 
term(1015) = term(1015) * (-4.0d+0) 
term(1016) = term(1016) * (2.0d+0) 
term(1017) = term(1017) * (-4.0d+0) 
term(1018) = term(1018) * (8.0d+0) 
term(1019) = term(1019) * (-4.0d+0) 
term(1020) = term(1020) * (8.0d+0) 
term(1021) = term(1021) * (-8.0d+0) 
term(1022) = term(1022) * (-16.0d+0) 
term(1023) = term(1023) * (16.0d+0) 

do j = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
do i = 1, nocc 
term(1024) = term(1024) + r2p(vrdav_Rr, p,i,a,j) * s2(b,q,i,j) * wm_interm_55_triplet_pt2(a,b)
term(1025) = term(1025) + r2p(vrdav_Rr, p,i,a,j) * s2(b,q,i,j) * wm_interm_60_triplet_pt2(a,b)
term(1026) = term(1026) + r2p(vrdav_Rr, p,i,a,j) * s2(b,q,i,j) * wm_interm_62_triplet_pt2(a,b)
term(1027) = term(1027) + r2p(vrdav_Rr, p,i,a,j) * s2(b,q,i,j) * wm_interm_72_triplet_pt2(a,b)
term(1028) = term(1028) + r2p(vrdav_Rr, p,i,a,j) * s2(b,q,i,j) * wm_interm_76_triplet_pt2(a,b)
end do 
end do 
end do 
end do 

term(1024) = term(1024) * (-0.5d+0) 
term(1026) = term(1026) * (-0.5d+0) 
term(1027) = term(1027) * (-2.0d+0) 
term(1028) = term(1028) * (2.0d+0) 

do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(1029) = term(1029) + r2p(vrdav_Rr, a,i,p,j) * s2(b,q,i,j) * wm_interm_55_triplet_pt2(a,b)
term(1030) = term(1030) + r2p(vrdav_Rr, a,i,p,j) * s2(b,q,i,j) * wm_interm_60_triplet_pt2(a,b)
term(1031) = term(1031) + r2p(vrdav_Rr, a,i,p,j) * s2(b,q,i,j) * wm_interm_62_triplet_pt2(a,b)
term(1032) = term(1032) + r2m(vrdav_Rr, a,i,p,j) * s2(b,q,i,j) * wm_interm_55_triplet_pt2(a,b)
term(1033) = term(1033) + r2m(vrdav_Rr, a,i,p,j) * s2(b,q,i,j) * wm_interm_60_triplet_pt2(a,b)
term(1034) = term(1034) + r2m(vrdav_Rr, a,i,p,j) * s2(b,q,i,j) * wm_interm_62_triplet_pt2(a,b)
term(1035) = term(1035) + r2p(vrdav_Rr, a,i,p,j) * s2(b,q,i,j) * wm_interm_72_triplet_pt2(a,b)
term(1036) = term(1036) + r2p(vrdav_Rr, a,i,p,j) * s2(b,q,i,j) * wm_interm_76_triplet_pt2(a,b)
term(1037) = term(1037) + r2m(vrdav_Rr, a,i,p,j) * s2(b,q,i,j) * wm_interm_72_triplet_pt2(a,b)
term(1038) = term(1038) + r2m(vrdav_Rr, a,i,p,j) * s2(b,q,i,j) * wm_interm_76_triplet_pt2(a,b)
end do 
end do 
end do 
end do 

term(1030) = term(1030) * (-2.0d+0) 
term(1032) = term(1032) * (2.0d+0) 
term(1033) = term(1033) * (-4.0d+0) 
term(1034) = term(1034) * (2.0d+0) 
term(1035) = term(1035) * (4.0d+0) 
term(1036) = term(1036) * (-4.0d+0) 
term(1037) = term(1037) * (8.0d+0) 
term(1038) = term(1038) * (-8.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do l = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(1039) = term(1039) + r2p(vrdav_Rr, a,i,p,j) * s2(a,q,l,k) * wm_interm_58_triplet_pt2(j,i,l,k)
term(1040) = term(1040) + r2p(vrdav_Rr, a,i,p,j) * s2(a,q,l,k) * wm_interm_58_triplet_pt2(i,j,l,k)
term(1041) = term(1041) + r2m(vrdav_Rr, a,i,p,j) * s2(a,q,l,k) * wm_interm_58_triplet_pt2(j,i,l,k)
term(1042) = term(1042) + r2m(vrdav_Rr, a,i,p,j) * s2(a,q,l,k) * wm_interm_58_triplet_pt2(i,j,l,k)
term(1043) = term(1043) + r2p(vrdav_Rr, a,i,p,j) * s2(a,q,l,k) * wm_interm_75_triplet_pt2(j,i,l,k)
term(1044) = term(1044) + r2p(vrdav_Rr, a,i,p,j) * s2(a,q,l,k) * wm_interm_75_triplet_pt2(i,j,l,k)
term(1045) = term(1045) + r2m(vrdav_Rr, a,i,p,j) * s2(a,q,l,k) * wm_interm_75_triplet_pt2(j,i,l,k)
term(1046) = term(1046) + r2m(vrdav_Rr, a,i,p,j) * s2(a,q,l,k) * wm_interm_75_triplet_pt2(i,j,l,k)
end do 
end do 
end do 
end do 
end do 

term(1039) = term(1039) * (-0.5d+0) 
term(1041) = term(1041) * (-1.0d+0) 
term(1042) = term(1042) * (2.0d+0) 
term(1043) = term(1043) * (-1.0d+0) 
term(1044) = term(1044) * (2.0d+0) 
term(1045) = term(1045) * (-2.0d+0) 
term(1046) = term(1046) * (4.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(1047) = term(1047) + r2m(vrdav_Rr, a,j,p,i) * s2(a,q,l,k) * wm_interm_58_triplet_pt2(i,j,l,k)
term(1048) = term(1048) + r2p(vrdav_Rl, a,j,p,k) * t2(a,q,l,i) * wm_interm_29_triplet_pt2(k,j,l,i)
term(1049) = term(1049) + r2p(vrdav_Rl, a,j,p,k) * t2(a,q,l,i) * wm_interm_29_triplet_pt2(j,k,l,i)
term(1050) = term(1050) + r2p(vrdav_Rl, a,j,p,k) * t2(a,q,l,i) * wm_interm_40_triplet_pt2(k,j,l,i)
term(1051) = term(1051) + r2p(vrdav_Rl, a,j,p,k) * t2(a,q,l,i) * wm_interm_40_triplet_pt2(j,k,l,i)
term(1052) = term(1052) + r2m(vrdav_Rr, a,j,p,i) * s2(a,q,l,k) * wm_interm_75_triplet_pt2(i,j,l,k)
term(1053) = term(1053) + r2m(vrdav_Rl, a,j,p,k) * t2(a,q,l,i) * wm_interm_29_triplet_pt2(k,j,l,i)
term(1054) = term(1054) + r2m(vrdav_Rl, a,j,p,k) * t2(a,q,l,i) * wm_interm_29_triplet_pt2(j,k,l,i)
term(1055) = term(1055) + r2m(vrdav_Rl, a,j,p,k) * t2(a,q,l,i) * wm_interm_40_triplet_pt2(k,j,l,i)
term(1056) = term(1056) + r2m(vrdav_Rl, a,j,p,k) * t2(a,q,l,i) * wm_interm_40_triplet_pt2(j,k,l,i)
end do 
end do 
end do 
end do 
end do 

term(1047) = term(1047) * (-1.0d+0) 
term(1048) = term(1048) * (-0.5d+0) 
term(1050) = term(1050) * (-1.0d+0) 
term(1051) = term(1051) * (2.0d+0) 
term(1052) = term(1052) * (-2.0d+0) 
term(1053) = term(1053) * (-1.0d+0) 
term(1054) = term(1054) * (2.0d+0) 
term(1055) = term(1055) * (-2.0d+0) 
term(1056) = term(1056) * (4.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(1057) = term(1057) + r2m(vrdav_Rr, a,j,p,i) * s2(a,q,l,k) * wm_interm_58_triplet_pt2(j,i,k,l)
term(1058) = term(1058) + r2m(vrdav_Rr, a,j,p,i) * s2(a,q,l,k) * wm_interm_58_triplet_pt2(i,j,k,l)
term(1059) = term(1059) + r2p(vrdav_Rl, a,j,p,k) * t2(a,q,l,i) * wm_interm_29_triplet_pt2(j,k,i,l)
term(1060) = term(1060) + r2p(vrdav_Rl, a,j,p,k) * t2(a,q,l,i) * wm_interm_40_triplet_pt2(j,k,i,l)
term(1061) = term(1061) + r2m(vrdav_Rr, a,j,p,i) * s2(a,q,l,k) * wm_interm_75_triplet_pt2(j,i,k,l)
term(1062) = term(1062) + r2m(vrdav_Rr, a,j,p,i) * s2(a,q,l,k) * wm_interm_75_triplet_pt2(i,j,k,l)
term(1063) = term(1063) + r2m(vrdav_Rl, a,j,p,k) * t2(a,q,l,i) * wm_interm_29_triplet_pt2(j,k,i,l)
term(1064) = term(1064) + r2m(vrdav_Rl, a,j,p,k) * t2(a,q,l,i) * wm_interm_40_triplet_pt2(j,k,i,l)
end do 
end do 
end do 
end do 
end do 

term(1057) = term(1057) * (-1.0d+0) 
term(1058) = term(1058) * (2.0d+0) 
term(1059) = term(1059) * (-0.5d+0) 
term(1060) = term(1060) * (-1.0d+0) 
term(1061) = term(1061) * (-2.0d+0) 
term(1062) = term(1062) * (4.0d+0) 
term(1063) = term(1063) * (-1.0d+0) 
term(1064) = term(1064) * (-2.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(1065) = term(1065) + r2m(vrdav_Rl, a,k,p,j) * t2(a,q,l,i) * wm_interm_29_triplet_pt2(j,k,l,i)
term(1066) = term(1066) + r2m(vrdav_Rl, a,k,p,j) * t2(a,q,l,i) * wm_interm_40_triplet_pt2(j,k,l,i)
end do 
end do 
end do 
end do 
end do 

term(1065) = term(1065) * (-1.0d+0) 
term(1066) = term(1066) * (-2.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do a = nocc + 1, nactive 
term(1067) = term(1067) + s2(a,p,k,j) * t2(a,q,l,i) * wm_interm_94_triplet_pt2(l,i,j,k)
term(1068) = term(1068) + s2(a,p,k,j) * t2(a,q,l,i) * wm_interm_94_triplet_pt2(i,l,j,k)
term(1069) = term(1069) + s2(a,p,k,j) * t2(a,q,l,i) * wm_interm_95_triplet_pt2(i,l,j,k)
term(1070) = term(1070) + s2(a,p,k,j) * t2(a,q,l,i) * wm_interm_122_triplet_pt2(l,i,j,k)
term(1071) = term(1071) + s2(a,p,k,j) * t2(a,q,l,i) * wm_interm_122_triplet_pt2(i,l,j,k)
term(1072) = term(1072) + s2(a,p,k,j) * t2(a,q,l,i) * wm_interm_144_triplet_pt2(l,i,j,k)
term(1073) = term(1073) + s2(a,p,k,j) * t2(a,q,l,i) * wm_interm_144_triplet_pt2(i,l,j,k)
term(1074) = term(1074) + s2(a,p,k,j) * t2(a,q,l,i) * wm_interm_145_triplet_pt2(i,l,j,k)
term(1075) = term(1075) + s2(a,p,k,j) * t2(a,q,l,i) * wm_interm_166_triplet_pt2(l,i,j,k)
term(1076) = term(1076) + s2(a,p,k,j) * t2(a,q,l,i) * wm_interm_166_triplet_pt2(i,l,j,k)
end do 
end do 
end do 
end do 
end do 

term(1067) = term(1067) * (-0.5d+0) 
term(1069) = term(1069) * (-0.5d+0) 
term(1070) = term(1070) * (-2.0d+0) 
term(1071) = term(1071) * (2.0d+0) 
term(1072) = term(1072) * (-1.0d+0) 
term(1073) = term(1073) * (2.0d+0) 
term(1074) = term(1074) * (-1.0d+0) 
term(1075) = term(1075) * (-4.0d+0) 
term(1076) = term(1076) * (4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
do a = nocc + 1, nactive 
term(1077) = term(1077) + s2(a,p,k,j) * t2(a,q,l,i) * wm_interm_94_triplet_pt2(l,i,k,j)
term(1078) = term(1078) + s2(a,p,k,j) * t2(a,q,l,i) * wm_interm_95_triplet_pt2(l,i,k,j)
term(1079) = term(1079) + s2(a,p,k,j) * t2(a,q,l,i) * wm_interm_94_triplet_pt2(i,l,k,j)
term(1080) = term(1080) + s2(a,p,k,j) * t2(a,q,l,i) * wm_interm_122_triplet_pt2(l,i,k,j)
term(1081) = term(1081) + s2(a,p,k,j) * t2(a,q,l,i) * wm_interm_122_triplet_pt2(i,l,k,j)
term(1082) = term(1082) + s2(a,p,k,j) * t2(a,q,l,i) * wm_interm_144_triplet_pt2(l,i,k,j)
term(1083) = term(1083) + s2(a,p,k,j) * t2(a,q,l,i) * wm_interm_145_triplet_pt2(l,i,k,j)
term(1084) = term(1084) + s2(a,p,k,j) * t2(a,q,l,i) * wm_interm_144_triplet_pt2(i,l,k,j)
term(1085) = term(1085) + s2(a,p,k,j) * t2(a,q,l,i) * wm_interm_166_triplet_pt2(l,i,k,j)
term(1086) = term(1086) + s2(a,p,k,j) * t2(a,q,l,i) * wm_interm_166_triplet_pt2(i,l,k,j)
end do 
end do 
end do 
end do 
end do 

term(1078) = term(1078) * (-0.5d+0) 
term(1079) = term(1079) * (-0.5d+0) 
term(1080) = term(1080) * (2.0d+0) 
term(1081) = term(1081) * (-2.0d+0) 
term(1082) = term(1082) * (2.0d+0) 
term(1083) = term(1083) * (-1.0d+0) 
term(1084) = term(1084) * (-1.0d+0) 
term(1085) = term(1085) * (4.0d+0) 
term(1086) = term(1086) * (-4.0d+0) 


    calc_D_vv_wm_triplet_pt2 = zero
    do s = 0, 1086
    calc_D_vv_wm_triplet_pt2 = calc_D_vv_wm_triplet_pt2 + term(s)
    end do

    end function calc_D_vv_wm_triplet_pt2
    
        

  end module tt_ccsd_pt012
