module density_exc_exc_functions_cc3_triplet_pt4
      

    use ccsd_transformed_integrals
    use cc3_intermediates
    use arithmetic
    use s_gen
    use basis
    use eom_vectors
    
    implicit none
    !
    ! File generated automatically on 2016-08-16 09:02:10
    !
    real(F64), dimension(:, :), allocatable :: wm_interm_cc3_0_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_1_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_2_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_3_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_4_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_5_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_6_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_7_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_8_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_9_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_10_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_11_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_12_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_13_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_14_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_15_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_16_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_17_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_18_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_19_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_20_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_21_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_22_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_23_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_24_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_25_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_26_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_27_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_28_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_29_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_30_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_31_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_32_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_33_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_34_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_35_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_36_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_37_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_38_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_39_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_40_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_41_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_42_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_43_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_44_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_45_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_46_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_47_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_48_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_49_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_50_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_51_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_52_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_53_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_54_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_55_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_56_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_57_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_58_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_59_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_60_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_61_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_62_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_63_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_64_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_65_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_66_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_67_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_68_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_69_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_70_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_71_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_72_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_73_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_74_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_75_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_76_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_77_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_78_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_79_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_80_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_81_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_82_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_83_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_84_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_85_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_86_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_87_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_88_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_89_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_90_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_91_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_92_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_93_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_94_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_95_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_96_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_97_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_98_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_99_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_100_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_101_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_102_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_103_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_104_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_105_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_106_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_107_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_108_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_109_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_110_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_111_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_112_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_113_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_114_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_115_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_116_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_117_triplet_pt4 

    contains
    
    subroutine wm_triplet_intermediates_cc3_init_pt4(nocc, nactive)
    integer, intent(in) :: nocc
    integer, intent(in) :: nactive
    allocate(wm_interm_cc3_0_triplet_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_cc3_1_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_2_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_3_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_4_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_5_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_6_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_7_triplet_pt4(1: nocc, 1: nocc))
allocate(wm_interm_cc3_8_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_9_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_10_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_11_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_12_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_13_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_14_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_15_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_16_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_17_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_18_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_19_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_20_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_21_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_22_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_23_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_24_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_25_triplet_pt4(1: nocc, 1: nocc))
allocate(wm_interm_cc3_26_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_27_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_28_triplet_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_cc3_29_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_30_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_31_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_32_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_33_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_34_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_35_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_36_triplet_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_cc3_37_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_38_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_39_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_40_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_41_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_42_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_43_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_44_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_45_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_46_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_47_triplet_pt4(1: nocc, 1: nocc))
allocate(wm_interm_cc3_48_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_49_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_50_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_51_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_52_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_53_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_54_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_55_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_56_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_57_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_58_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_59_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_60_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_61_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_62_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_63_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_64_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_65_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_66_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_67_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_68_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_69_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_70_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_71_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_72_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_73_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_74_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_75_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_76_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_77_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_78_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_79_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_80_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_81_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_82_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_83_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_84_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_85_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_86_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_87_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_88_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_89_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_90_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_91_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_92_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_93_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_94_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_95_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_96_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_97_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_98_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_99_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_100_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_101_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_102_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_103_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_104_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_105_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_106_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_107_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_108_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_109_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_110_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_111_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_112_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_113_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_114_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_115_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_116_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_117_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
wm_interm_cc3_0_triplet_pt4 = zero 
wm_interm_cc3_1_triplet_pt4 = zero 
wm_interm_cc3_2_triplet_pt4 = zero 
wm_interm_cc3_3_triplet_pt4 = zero 
wm_interm_cc3_4_triplet_pt4 = zero 
wm_interm_cc3_5_triplet_pt4 = zero 
wm_interm_cc3_6_triplet_pt4 = zero 
wm_interm_cc3_7_triplet_pt4 = zero 
wm_interm_cc3_8_triplet_pt4 = zero 
wm_interm_cc3_9_triplet_pt4 = zero 
wm_interm_cc3_10_triplet_pt4 = zero 
wm_interm_cc3_11_triplet_pt4 = zero 
wm_interm_cc3_12_triplet_pt4 = zero 
wm_interm_cc3_13_triplet_pt4 = zero 
wm_interm_cc3_14_triplet_pt4 = zero 
wm_interm_cc3_15_triplet_pt4 = zero 
wm_interm_cc3_16_triplet_pt4 = zero 
wm_interm_cc3_17_triplet_pt4 = zero 
wm_interm_cc3_18_triplet_pt4 = zero 
wm_interm_cc3_19_triplet_pt4 = zero 
wm_interm_cc3_20_triplet_pt4 = zero 
wm_interm_cc3_21_triplet_pt4 = zero 
wm_interm_cc3_22_triplet_pt4 = zero 
wm_interm_cc3_23_triplet_pt4 = zero 
wm_interm_cc3_24_triplet_pt4 = zero 
wm_interm_cc3_25_triplet_pt4 = zero 
wm_interm_cc3_26_triplet_pt4 = zero 
wm_interm_cc3_27_triplet_pt4 = zero 
wm_interm_cc3_28_triplet_pt4 = zero 
wm_interm_cc3_29_triplet_pt4 = zero 
wm_interm_cc3_30_triplet_pt4 = zero 
wm_interm_cc3_31_triplet_pt4 = zero 
wm_interm_cc3_32_triplet_pt4 = zero 
wm_interm_cc3_33_triplet_pt4 = zero 
wm_interm_cc3_34_triplet_pt4 = zero 
wm_interm_cc3_35_triplet_pt4 = zero 
wm_interm_cc3_36_triplet_pt4 = zero 
wm_interm_cc3_37_triplet_pt4 = zero 
wm_interm_cc3_38_triplet_pt4 = zero 
wm_interm_cc3_39_triplet_pt4 = zero 
wm_interm_cc3_40_triplet_pt4 = zero 
wm_interm_cc3_41_triplet_pt4 = zero 
wm_interm_cc3_42_triplet_pt4 = zero 
wm_interm_cc3_43_triplet_pt4 = zero 
wm_interm_cc3_44_triplet_pt4 = zero 
wm_interm_cc3_45_triplet_pt4 = zero 
wm_interm_cc3_46_triplet_pt4 = zero 
wm_interm_cc3_47_triplet_pt4 = zero 
wm_interm_cc3_48_triplet_pt4 = zero 
wm_interm_cc3_49_triplet_pt4 = zero 
wm_interm_cc3_50_triplet_pt4 = zero 
wm_interm_cc3_51_triplet_pt4 = zero 
wm_interm_cc3_52_triplet_pt4 = zero 
wm_interm_cc3_53_triplet_pt4 = zero 
wm_interm_cc3_54_triplet_pt4 = zero 
wm_interm_cc3_55_triplet_pt4 = zero 
wm_interm_cc3_56_triplet_pt4 = zero 
wm_interm_cc3_57_triplet_pt4 = zero 
wm_interm_cc3_58_triplet_pt4 = zero 
wm_interm_cc3_59_triplet_pt4 = zero 
wm_interm_cc3_60_triplet_pt4 = zero 
wm_interm_cc3_61_triplet_pt4 = zero 
wm_interm_cc3_62_triplet_pt4 = zero 
wm_interm_cc3_63_triplet_pt4 = zero 
wm_interm_cc3_64_triplet_pt4 = zero 
wm_interm_cc3_65_triplet_pt4 = zero 
wm_interm_cc3_66_triplet_pt4 = zero 
wm_interm_cc3_67_triplet_pt4 = zero 
wm_interm_cc3_68_triplet_pt4 = zero 
wm_interm_cc3_69_triplet_pt4 = zero 
wm_interm_cc3_70_triplet_pt4 = zero 
wm_interm_cc3_71_triplet_pt4 = zero 
wm_interm_cc3_72_triplet_pt4 = zero 
wm_interm_cc3_73_triplet_pt4 = zero 
wm_interm_cc3_74_triplet_pt4 = zero 
wm_interm_cc3_75_triplet_pt4 = zero 
wm_interm_cc3_76_triplet_pt4 = zero 
wm_interm_cc3_77_triplet_pt4 = zero 
wm_interm_cc3_78_triplet_pt4 = zero 
wm_interm_cc3_79_triplet_pt4 = zero 
wm_interm_cc3_80_triplet_pt4 = zero 
wm_interm_cc3_81_triplet_pt4 = zero 
wm_interm_cc3_82_triplet_pt4 = zero 
wm_interm_cc3_83_triplet_pt4 = zero 
wm_interm_cc3_84_triplet_pt4 = zero 
wm_interm_cc3_85_triplet_pt4 = zero 
wm_interm_cc3_86_triplet_pt4 = zero 
wm_interm_cc3_87_triplet_pt4 = zero 
wm_interm_cc3_88_triplet_pt4 = zero 
wm_interm_cc3_89_triplet_pt4 = zero 
wm_interm_cc3_90_triplet_pt4 = zero 
wm_interm_cc3_91_triplet_pt4 = zero 
wm_interm_cc3_92_triplet_pt4 = zero 
wm_interm_cc3_93_triplet_pt4 = zero 
wm_interm_cc3_94_triplet_pt4 = zero 
wm_interm_cc3_95_triplet_pt4 = zero 
wm_interm_cc3_96_triplet_pt4 = zero 
wm_interm_cc3_97_triplet_pt4 = zero 
wm_interm_cc3_98_triplet_pt4 = zero 
wm_interm_cc3_99_triplet_pt4 = zero 
wm_interm_cc3_100_triplet_pt4 = zero 
wm_interm_cc3_101_triplet_pt4 = zero 
wm_interm_cc3_102_triplet_pt4 = zero 
wm_interm_cc3_103_triplet_pt4 = zero 
wm_interm_cc3_104_triplet_pt4 = zero 
wm_interm_cc3_105_triplet_pt4 = zero 
wm_interm_cc3_106_triplet_pt4 = zero 
wm_interm_cc3_107_triplet_pt4 = zero 
wm_interm_cc3_108_triplet_pt4 = zero 
wm_interm_cc3_109_triplet_pt4 = zero 
wm_interm_cc3_110_triplet_pt4 = zero 
wm_interm_cc3_111_triplet_pt4 = zero 
wm_interm_cc3_112_triplet_pt4 = zero 
wm_interm_cc3_113_triplet_pt4 = zero 
wm_interm_cc3_114_triplet_pt4 = zero 
wm_interm_cc3_115_triplet_pt4 = zero 
wm_interm_cc3_116_triplet_pt4 = zero 
wm_interm_cc3_117_triplet_pt4 = zero 

    end subroutine wm_triplet_intermediates_cc3_init_pt4
    
    subroutine wm_triplet_intermediates_cc3_free_pt4
    deallocate(wm_interm_cc3_0_triplet_pt4)
deallocate(wm_interm_cc3_1_triplet_pt4)
deallocate(wm_interm_cc3_2_triplet_pt4)
deallocate(wm_interm_cc3_3_triplet_pt4)
deallocate(wm_interm_cc3_4_triplet_pt4)
deallocate(wm_interm_cc3_5_triplet_pt4)
deallocate(wm_interm_cc3_6_triplet_pt4)
deallocate(wm_interm_cc3_7_triplet_pt4)
deallocate(wm_interm_cc3_8_triplet_pt4)
deallocate(wm_interm_cc3_9_triplet_pt4)
deallocate(wm_interm_cc3_10_triplet_pt4)
deallocate(wm_interm_cc3_11_triplet_pt4)
deallocate(wm_interm_cc3_12_triplet_pt4)
deallocate(wm_interm_cc3_13_triplet_pt4)
deallocate(wm_interm_cc3_14_triplet_pt4)
deallocate(wm_interm_cc3_15_triplet_pt4)
deallocate(wm_interm_cc3_16_triplet_pt4)
deallocate(wm_interm_cc3_17_triplet_pt4)
deallocate(wm_interm_cc3_18_triplet_pt4)
deallocate(wm_interm_cc3_19_triplet_pt4)
deallocate(wm_interm_cc3_20_triplet_pt4)
deallocate(wm_interm_cc3_21_triplet_pt4)
deallocate(wm_interm_cc3_22_triplet_pt4)
deallocate(wm_interm_cc3_23_triplet_pt4)
deallocate(wm_interm_cc3_24_triplet_pt4)
deallocate(wm_interm_cc3_25_triplet_pt4)
deallocate(wm_interm_cc3_26_triplet_pt4)
deallocate(wm_interm_cc3_27_triplet_pt4)
deallocate(wm_interm_cc3_28_triplet_pt4)
deallocate(wm_interm_cc3_29_triplet_pt4)
deallocate(wm_interm_cc3_30_triplet_pt4)
deallocate(wm_interm_cc3_31_triplet_pt4)
deallocate(wm_interm_cc3_32_triplet_pt4)
deallocate(wm_interm_cc3_33_triplet_pt4)
deallocate(wm_interm_cc3_34_triplet_pt4)
deallocate(wm_interm_cc3_35_triplet_pt4)
deallocate(wm_interm_cc3_36_triplet_pt4)
deallocate(wm_interm_cc3_37_triplet_pt4)
deallocate(wm_interm_cc3_38_triplet_pt4)
deallocate(wm_interm_cc3_39_triplet_pt4)
deallocate(wm_interm_cc3_40_triplet_pt4)
deallocate(wm_interm_cc3_41_triplet_pt4)
deallocate(wm_interm_cc3_42_triplet_pt4)
deallocate(wm_interm_cc3_43_triplet_pt4)
deallocate(wm_interm_cc3_44_triplet_pt4)
deallocate(wm_interm_cc3_45_triplet_pt4)
deallocate(wm_interm_cc3_46_triplet_pt4)
deallocate(wm_interm_cc3_47_triplet_pt4)
deallocate(wm_interm_cc3_48_triplet_pt4)
deallocate(wm_interm_cc3_49_triplet_pt4)
deallocate(wm_interm_cc3_50_triplet_pt4)
deallocate(wm_interm_cc3_51_triplet_pt4)
deallocate(wm_interm_cc3_52_triplet_pt4)
deallocate(wm_interm_cc3_53_triplet_pt4)
deallocate(wm_interm_cc3_54_triplet_pt4)
deallocate(wm_interm_cc3_55_triplet_pt4)
deallocate(wm_interm_cc3_56_triplet_pt4)
deallocate(wm_interm_cc3_57_triplet_pt4)
deallocate(wm_interm_cc3_58_triplet_pt4)
deallocate(wm_interm_cc3_59_triplet_pt4)
deallocate(wm_interm_cc3_60_triplet_pt4)
deallocate(wm_interm_cc3_61_triplet_pt4)
deallocate(wm_interm_cc3_62_triplet_pt4)
deallocate(wm_interm_cc3_63_triplet_pt4)
deallocate(wm_interm_cc3_64_triplet_pt4)
deallocate(wm_interm_cc3_65_triplet_pt4)
deallocate(wm_interm_cc3_66_triplet_pt4)
deallocate(wm_interm_cc3_67_triplet_pt4)
deallocate(wm_interm_cc3_68_triplet_pt4)
deallocate(wm_interm_cc3_69_triplet_pt4)
deallocate(wm_interm_cc3_70_triplet_pt4)
deallocate(wm_interm_cc3_71_triplet_pt4)
deallocate(wm_interm_cc3_72_triplet_pt4)
deallocate(wm_interm_cc3_73_triplet_pt4)
deallocate(wm_interm_cc3_74_triplet_pt4)
deallocate(wm_interm_cc3_75_triplet_pt4)
deallocate(wm_interm_cc3_76_triplet_pt4)
deallocate(wm_interm_cc3_77_triplet_pt4)
deallocate(wm_interm_cc3_78_triplet_pt4)
deallocate(wm_interm_cc3_79_triplet_pt4)
deallocate(wm_interm_cc3_80_triplet_pt4)
deallocate(wm_interm_cc3_81_triplet_pt4)
deallocate(wm_interm_cc3_82_triplet_pt4)
deallocate(wm_interm_cc3_83_triplet_pt4)
deallocate(wm_interm_cc3_84_triplet_pt4)
deallocate(wm_interm_cc3_85_triplet_pt4)
deallocate(wm_interm_cc3_86_triplet_pt4)
deallocate(wm_interm_cc3_87_triplet_pt4)
deallocate(wm_interm_cc3_88_triplet_pt4)
deallocate(wm_interm_cc3_89_triplet_pt4)
deallocate(wm_interm_cc3_90_triplet_pt4)
deallocate(wm_interm_cc3_91_triplet_pt4)
deallocate(wm_interm_cc3_92_triplet_pt4)
deallocate(wm_interm_cc3_93_triplet_pt4)
deallocate(wm_interm_cc3_94_triplet_pt4)
deallocate(wm_interm_cc3_95_triplet_pt4)
deallocate(wm_interm_cc3_96_triplet_pt4)
deallocate(wm_interm_cc3_97_triplet_pt4)
deallocate(wm_interm_cc3_98_triplet_pt4)
deallocate(wm_interm_cc3_99_triplet_pt4)
deallocate(wm_interm_cc3_100_triplet_pt4)
deallocate(wm_interm_cc3_101_triplet_pt4)
deallocate(wm_interm_cc3_102_triplet_pt4)
deallocate(wm_interm_cc3_103_triplet_pt4)
deallocate(wm_interm_cc3_104_triplet_pt4)
deallocate(wm_interm_cc3_105_triplet_pt4)
deallocate(wm_interm_cc3_106_triplet_pt4)
deallocate(wm_interm_cc3_107_triplet_pt4)
deallocate(wm_interm_cc3_108_triplet_pt4)
deallocate(wm_interm_cc3_109_triplet_pt4)
deallocate(wm_interm_cc3_110_triplet_pt4)
deallocate(wm_interm_cc3_111_triplet_pt4)
deallocate(wm_interm_cc3_112_triplet_pt4)
deallocate(wm_interm_cc3_113_triplet_pt4)
deallocate(wm_interm_cc3_114_triplet_pt4)
deallocate(wm_interm_cc3_115_triplet_pt4)
deallocate(wm_interm_cc3_116_triplet_pt4)
deallocate(wm_interm_cc3_117_triplet_pt4)

    end subroutine wm_triplet_intermediates_cc3_free_pt4
    
    subroutine wm_triplet_intermediates_cc3_pt4(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr)
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    double precision, dimension(:), intent(in) :: vrdav_Rl
    double precision, dimension(:), intent(in) :: vrdav_Rr
    real(F64) :: sum
    integer :: i, a, b, j, c, k, l 

    !$omp parallel private(i, a, b, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
sum = zero 
do i = 1, nocc 
sum = sum + r1(vrdav_Rl, a,i) * t1(b,i)
end do 
wm_interm_cc3_0_triplet_pt4(a, b) = wm_interm_cc3_0_triplet_pt4(a, b) + sum 
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
wm_interm_cc3_1_triplet_pt4(c, k) = wm_interm_cc3_1_triplet_pt4(c, k) + sum 
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
wm_interm_cc3_2_triplet_pt4(c, k) = wm_interm_cc3_2_triplet_pt4(c, k) + sum 
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
sum = sum + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,i,j,k)
end do 
end do 
end do 
end do 
wm_interm_cc3_3_triplet_pt4(c, k) = wm_interm_cc3_3_triplet_pt4(c, k) + sum 
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
sum = sum + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,j,i,k)
end do 
end do 
end do 
end do 
wm_interm_cc3_4_triplet_pt4(c, k) = wm_interm_cc3_4_triplet_pt4(c, k) + sum 
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
wm_interm_cc3_5_triplet_pt4(c, k) = wm_interm_cc3_5_triplet_pt4(c, k) + sum 
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
sum = sum + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,i,k,j)
end do 
end do 
end do 
end do 
wm_interm_cc3_6_triplet_pt4(c, k) = wm_interm_cc3_6_triplet_pt4(c, k) + sum 
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
wm_interm_cc3_7_triplet_pt4(i, j) = wm_interm_cc3_7_triplet_pt4(i, j) + sum 
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
wm_interm_cc3_8_triplet_pt4(c, j, k, l) = wm_interm_cc3_8_triplet_pt4(c, j, k, l) + sum 
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
wm_interm_cc3_9_triplet_pt4(c, j, k, l) = wm_interm_cc3_9_triplet_pt4(c, j, k, l) + sum 
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
wm_interm_cc3_10_triplet_pt4(c, j, k, l) = wm_interm_cc3_10_triplet_pt4(c, j, k, l) + sum 
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
sum = sum + r2m(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,k,i,j)
end do 
end do 
end do 
end do 
wm_interm_cc3_11_triplet_pt4(c, k) = wm_interm_cc3_11_triplet_pt4(c, k) + sum 
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
wm_interm_cc3_12_triplet_pt4(c, k) = wm_interm_cc3_12_triplet_pt4(c, k) + sum 
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
wm_interm_cc3_13_triplet_pt4(c, k) = wm_interm_cc3_13_triplet_pt4(c, k) + sum 
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
sum = sum + r2m(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,i,k,j)
end do 
end do 
end do 
end do 
wm_interm_cc3_14_triplet_pt4(c, k) = wm_interm_cc3_14_triplet_pt4(c, k) + sum 
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
sum = sum + r2m(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,j,i,k)
end do 
end do 
end do 
end do 
wm_interm_cc3_15_triplet_pt4(c, k) = wm_interm_cc3_15_triplet_pt4(c, k) + sum 
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
sum = sum + r2m(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,i,j,k)
end do 
end do 
end do 
end do 
wm_interm_cc3_16_triplet_pt4(c, k) = wm_interm_cc3_16_triplet_pt4(c, k) + sum 
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
wm_interm_cc3_17_triplet_pt4(c, j, k, l) = wm_interm_cc3_17_triplet_pt4(c, j, k, l) + sum 
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
wm_interm_cc3_18_triplet_pt4(c, j, k, l) = wm_interm_cc3_18_triplet_pt4(c, j, k, l) + sum 
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
wm_interm_cc3_19_triplet_pt4(c, j, k, l) = wm_interm_cc3_19_triplet_pt4(c, j, k, l) + sum 
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
sum = sum + r2m(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,i,k,l)
end do 
end do 
end do 
wm_interm_cc3_20_triplet_pt4(c, j, k, l) = wm_interm_cc3_20_triplet_pt4(c, j, k, l) + sum 
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
sum = sum + r2m(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,k,i,l)
end do 
end do 
end do 
wm_interm_cc3_21_triplet_pt4(c, j, k, l) = wm_interm_cc3_21_triplet_pt4(c, j, k, l) + sum 
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
wm_interm_cc3_22_triplet_pt4(c, j, k, l) = wm_interm_cc3_22_triplet_pt4(c, j, k, l) + sum 
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
wm_interm_cc3_23_triplet_pt4(b, j) = wm_interm_cc3_23_triplet_pt4(b, j) + sum 
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
wm_interm_cc3_24_triplet_pt4(c, j, k, l) = wm_interm_cc3_24_triplet_pt4(c, j, k, l) + sum 
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
wm_interm_cc3_25_triplet_pt4(i, j) = wm_interm_cc3_25_triplet_pt4(i, j) + sum 
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
wm_interm_cc3_26_triplet_pt4(c, j, k, l) = wm_interm_cc3_26_triplet_pt4(c, j, k, l) + sum 
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
wm_interm_cc3_27_triplet_pt4(c, j, k, l) = wm_interm_cc3_27_triplet_pt4(c, j, k, l) + sum 
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
wm_interm_cc3_28_triplet_pt4(a, b) = wm_interm_cc3_28_triplet_pt4(a, b) + sum 
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
wm_interm_cc3_29_triplet_pt4(c, k) = wm_interm_cc3_29_triplet_pt4(c, k) + sum 
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
wm_interm_cc3_30_triplet_pt4(c, k) = wm_interm_cc3_30_triplet_pt4(c, k) + sum 
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
wm_interm_cc3_31_triplet_pt4(c, k) = wm_interm_cc3_31_triplet_pt4(c, k) + sum 
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
wm_interm_cc3_32_triplet_pt4(c, k) = wm_interm_cc3_32_triplet_pt4(c, k) + sum 
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
wm_interm_cc3_33_triplet_pt4(c, k) = wm_interm_cc3_33_triplet_pt4(c, k) + sum 
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
wm_interm_cc3_34_triplet_pt4(c, k) = wm_interm_cc3_34_triplet_pt4(c, k) + sum 
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
wm_interm_cc3_35_triplet_pt4(b, i, j, k) = wm_interm_cc3_35_triplet_pt4(b, i, j, k) + sum 
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
sum = sum + r1(vrdav_Rr, a,i) * s1(b,i)
end do 
wm_interm_cc3_36_triplet_pt4(a, b) = wm_interm_cc3_36_triplet_pt4(a, b) + sum 
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
sum = sum + r2p(vrdav_Rl, b,j,a,i) * r1(vrdav_Rr, a,i)
end do 
end do 
wm_interm_cc3_37_triplet_pt4(b, j) = wm_interm_cc3_37_triplet_pt4(b, j) + sum 
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
sum = sum + r2p(vrdav_Rl, b,j,a,i) * t3(nocc, nactive, a,b,c,k,i,l)
end do 
end do 
end do 
wm_interm_cc3_38_triplet_pt4(c, j, k, l) = wm_interm_cc3_38_triplet_pt4(c, j, k, l) + sum 
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
sum = sum + r2p(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, a,b,c,k,i,l)
end do 
end do 
end do 
wm_interm_cc3_39_triplet_pt4(c, j, k, l) = wm_interm_cc3_39_triplet_pt4(c, j, k, l) + sum 
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
sum = sum + r2p(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,b,c,j,i,k)
end do 
end do 
end do 
end do 
wm_interm_cc3_40_triplet_pt4(c, k) = wm_interm_cc3_40_triplet_pt4(c, k) + sum 
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
sum = sum + r2p(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,b,c,i,j,k)
end do 
end do 
end do 
end do 
wm_interm_cc3_41_triplet_pt4(c, k) = wm_interm_cc3_41_triplet_pt4(c, k) + sum 
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
sum = sum + r2p(vrdav_Rl, b,i,a,j) * t3(nocc, nactive, a,b,c,k,j,i)
end do 
end do 
end do 
end do 
wm_interm_cc3_42_triplet_pt4(c, k) = wm_interm_cc3_42_triplet_pt4(c, k) + sum 
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
sum = sum + r2p(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, a,b,c,k,l,i)
end do 
end do 
end do 
wm_interm_cc3_43_triplet_pt4(c, j, k, l) = wm_interm_cc3_43_triplet_pt4(c, j, k, l) + sum 
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
sum = sum + r2p(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,b,c,k,j,i)
end do 
end do 
end do 
end do 
wm_interm_cc3_44_triplet_pt4(c, k) = wm_interm_cc3_44_triplet_pt4(c, k) + sum 
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
do a = nocc + 1, nactive 
do j = 1, nocc 
sum = sum + r2p(vrdav_Rl, b,j,a,i) * t3(nocc, nactive, a,b,c,k,j,i)
end do 
end do 
end do 
end do 
wm_interm_cc3_45_triplet_pt4(c, k) = wm_interm_cc3_45_triplet_pt4(c, k) + sum 
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
sum = sum + r2p(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, a,b,c,k,j,i)
end do 
end do 
end do 
end do 
wm_interm_cc3_46_triplet_pt4(c, k) = wm_interm_cc3_46_triplet_pt4(c, k) + sum 
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
wm_interm_cc3_47_triplet_pt4(i, j) = wm_interm_cc3_47_triplet_pt4(i, j) + sum 
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
wm_interm_cc3_48_triplet_pt4(b, i, j, k) = wm_interm_cc3_48_triplet_pt4(b, i, j, k) + sum 
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
wm_interm_cc3_49_triplet_pt4(b, j) = wm_interm_cc3_49_triplet_pt4(b, j) + sum 
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
wm_interm_cc3_50_triplet_pt4(b, j) = wm_interm_cc3_50_triplet_pt4(b, j) + sum 
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
wm_interm_cc3_51_triplet_pt4(c, j, k, l) = wm_interm_cc3_51_triplet_pt4(c, j, k, l) + sum 
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
wm_interm_cc3_52_triplet_pt4(c, j, k, l) = wm_interm_cc3_52_triplet_pt4(c, j, k, l) + sum 
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
wm_interm_cc3_53_triplet_pt4(c, j, k, l) = wm_interm_cc3_53_triplet_pt4(c, j, k, l) + sum 
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
wm_interm_cc3_54_triplet_pt4(c, j, k, l) = wm_interm_cc3_54_triplet_pt4(c, j, k, l) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,j,b,i,c,k)
end do 
end do 
end do 
end do 
wm_interm_cc3_55_triplet_pt4(c, k) = wm_interm_cc3_55_triplet_pt4(c, k) + sum 
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
wm_interm_cc3_56_triplet_pt4(c, k) = wm_interm_cc3_56_triplet_pt4(c, k) + sum 
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
wm_interm_cc3_57_triplet_pt4(c, k) = wm_interm_cc3_57_triplet_pt4(c, k) + sum 
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
wm_interm_cc3_58_triplet_pt4(c, k) = wm_interm_cc3_58_triplet_pt4(c, k) + sum 
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
wm_interm_cc3_59_triplet_pt4(b, i, j, k) = wm_interm_cc3_59_triplet_pt4(b, i, j, k) + sum 
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
wm_interm_cc3_60_triplet_pt4(b, j, i, k) = wm_interm_cc3_60_triplet_pt4(b, j, i, k) + sum 
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
wm_interm_cc3_61_triplet_pt4(b, j) = wm_interm_cc3_61_triplet_pt4(b, j) + sum 
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
wm_interm_cc3_62_triplet_pt4(b, j) = wm_interm_cc3_62_triplet_pt4(b, j) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,b,c,k,i,l)
end do 
end do 
end do 
wm_interm_cc3_63_triplet_pt4(c, j, k, l) = wm_interm_cc3_63_triplet_pt4(c, j, k, l) + sum 
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
sum = sum + r2m(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, a,b,c,k,i,l)
end do 
end do 
end do 
wm_interm_cc3_64_triplet_pt4(c, j, k, l) = wm_interm_cc3_64_triplet_pt4(c, j, k, l) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,b,c,j,i,k)
end do 
end do 
end do 
end do 
wm_interm_cc3_65_triplet_pt4(c, k) = wm_interm_cc3_65_triplet_pt4(c, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,b,c,i,j,k)
end do 
end do 
end do 
end do 
wm_interm_cc3_66_triplet_pt4(c, k) = wm_interm_cc3_66_triplet_pt4(c, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, a,b,c,k,j,i)
end do 
end do 
end do 
end do 
wm_interm_cc3_67_triplet_pt4(c, k) = wm_interm_cc3_67_triplet_pt4(c, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,b,c,k,l,i)
end do 
end do 
end do 
wm_interm_cc3_68_triplet_pt4(c, j, k, l) = wm_interm_cc3_68_triplet_pt4(c, j, k, l) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,b,c,k,j,i)
end do 
end do 
end do 
end do 
wm_interm_cc3_69_triplet_pt4(c, k) = wm_interm_cc3_69_triplet_pt4(c, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, a,b,c,k,l,i)
end do 
end do 
end do 
wm_interm_cc3_70_triplet_pt4(c, j, k, l) = wm_interm_cc3_70_triplet_pt4(c, j, k, l) + sum 
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
wm_interm_cc3_71_triplet_pt4(b, j, i, k) = wm_interm_cc3_71_triplet_pt4(b, j, i, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * r1(vrdav_Rr, a,k)
end do 
wm_interm_cc3_72_triplet_pt4(b, i, j, k) = wm_interm_cc3_72_triplet_pt4(b, i, j, k) + sum 
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
wm_interm_cc3_73_triplet_pt4(b, j) = wm_interm_cc3_73_triplet_pt4(b, j) + sum 
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
wm_interm_cc3_74_triplet_pt4(b, j) = wm_interm_cc3_74_triplet_pt4(b, j) + sum 
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
wm_interm_cc3_75_triplet_pt4(b, j) = wm_interm_cc3_75_triplet_pt4(b, j) + sum 
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
wm_interm_cc3_76_triplet_pt4(c, j, k, l) = wm_interm_cc3_76_triplet_pt4(c, j, k, l) + sum 
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
wm_interm_cc3_77_triplet_pt4(c, k) = wm_interm_cc3_77_triplet_pt4(c, k) + sum 
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
wm_interm_cc3_78_triplet_pt4(c, k, j, l) = wm_interm_cc3_78_triplet_pt4(c, k, j, l) + sum 
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
wm_interm_cc3_79_triplet_pt4(c, j, k, l) = wm_interm_cc3_79_triplet_pt4(c, j, k, l) + sum 
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
wm_interm_cc3_80_triplet_pt4(c, k) = wm_interm_cc3_80_triplet_pt4(c, k) + sum 
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
wm_interm_cc3_81_triplet_pt4(c, j, k, l) = wm_interm_cc3_81_triplet_pt4(c, j, k, l) + sum 
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
wm_interm_cc3_82_triplet_pt4(c, k) = wm_interm_cc3_82_triplet_pt4(c, k) + sum 
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
wm_interm_cc3_83_triplet_pt4(c, k) = wm_interm_cc3_83_triplet_pt4(c, k) + sum 
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
wm_interm_cc3_84_triplet_pt4(c, k) = wm_interm_cc3_84_triplet_pt4(c, k) + sum 
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
wm_interm_cc3_85_triplet_pt4(c, k, j, l) = wm_interm_cc3_85_triplet_pt4(c, k, j, l) + sum 
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
wm_interm_cc3_86_triplet_pt4(c, j, k, l) = wm_interm_cc3_86_triplet_pt4(c, j, k, l) + sum 
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
wm_interm_cc3_87_triplet_pt4(c, j, k, l) = wm_interm_cc3_87_triplet_pt4(c, j, k, l) + sum 
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
wm_interm_cc3_88_triplet_pt4(c, k) = wm_interm_cc3_88_triplet_pt4(c, k) + sum 
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
wm_interm_cc3_89_triplet_pt4(c, k) = wm_interm_cc3_89_triplet_pt4(c, k) + sum 
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
wm_interm_cc3_90_triplet_pt4(c, k, j, l) = wm_interm_cc3_90_triplet_pt4(c, k, j, l) + sum 
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
wm_interm_cc3_91_triplet_pt4(c, k, j, l) = wm_interm_cc3_91_triplet_pt4(c, k, j, l) + sum 
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
wm_interm_cc3_92_triplet_pt4(c, j, k, l) = wm_interm_cc3_92_triplet_pt4(c, j, k, l) + sum 
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
wm_interm_cc3_93_triplet_pt4(c, j, k, l) = wm_interm_cc3_93_triplet_pt4(c, j, k, l) + sum 
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
wm_interm_cc3_94_triplet_pt4(c, k) = wm_interm_cc3_94_triplet_pt4(c, k) + sum 
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
wm_interm_cc3_95_triplet_pt4(c, k) = wm_interm_cc3_95_triplet_pt4(c, k) + sum 
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
wm_interm_cc3_96_triplet_pt4(b, i, j, k) = wm_interm_cc3_96_triplet_pt4(b, i, j, k) + sum 
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
wm_interm_cc3_97_triplet_pt4(b, j, i, k) = wm_interm_cc3_97_triplet_pt4(b, j, i, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_cc3_98_triplet_pt4(b, c, j, k) = wm_interm_cc3_98_triplet_pt4(b, c, j, k) + sum 
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
wm_interm_cc3_99_triplet_pt4(b, c, k, j) = wm_interm_cc3_99_triplet_pt4(b, c, k, j) + sum 
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
wm_interm_cc3_100_triplet_pt4(b, c, k, j) = wm_interm_cc3_100_triplet_pt4(b, c, k, j) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_cc3_101_triplet_pt4(b, c, j, k) = wm_interm_cc3_101_triplet_pt4(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_cc3_102_triplet_pt4(b, c, j, k) = wm_interm_cc3_102_triplet_pt4(b, c, j, k) + sum 
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
wm_interm_cc3_103_triplet_pt4(b, c, k, j) = wm_interm_cc3_103_triplet_pt4(b, c, k, j) + sum 
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
wm_interm_cc3_104_triplet_pt4(b, c, k, j) = wm_interm_cc3_104_triplet_pt4(b, c, k, j) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_cc3_105_triplet_pt4(b, c, j, k) = wm_interm_cc3_105_triplet_pt4(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_cc3_106_triplet_pt4(b, c, j, k) = wm_interm_cc3_106_triplet_pt4(b, c, j, k) + sum 
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
wm_interm_cc3_107_triplet_pt4(a, b, i, j) = wm_interm_cc3_107_triplet_pt4(a, b, i, j) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_cc3_108_triplet_pt4(b, c, j, k) = wm_interm_cc3_108_triplet_pt4(b, c, j, k) + sum 
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
sum = sum + s1(a,i) * s1(b,j)
wm_interm_cc3_109_triplet_pt4(a, b, i, j) = wm_interm_cc3_109_triplet_pt4(a, b, i, j) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_cc3_110_triplet_pt4(b, c, j, k) = wm_interm_cc3_110_triplet_pt4(b, c, j, k) + sum 
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
sum = sum + r1(vrdav_Rr, a,i) * t1(b,j)
wm_interm_cc3_111_triplet_pt4(a, b, i, j) = wm_interm_cc3_111_triplet_pt4(a, b, i, j) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_cc3_112_triplet_pt4(b, c, j, k) = wm_interm_cc3_112_triplet_pt4(b, c, j, k) + sum 
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
wm_interm_cc3_113_triplet_pt4(a, b, i, j) = wm_interm_cc3_113_triplet_pt4(a, b, i, j) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_cc3_114_triplet_pt4(b, c, j, k) = wm_interm_cc3_114_triplet_pt4(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_cc3_115_triplet_pt4(b, c, j, k) = wm_interm_cc3_115_triplet_pt4(b, c, j, k) + sum 
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
wm_interm_cc3_116_triplet_pt4(b, c, k, j) = wm_interm_cc3_116_triplet_pt4(b, c, k, j) + sum 
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
wm_interm_cc3_117_triplet_pt4(b, c, k, j) = wm_interm_cc3_117_triplet_pt4(b, c, k, j) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 



    end subroutine wm_triplet_intermediates_cc3_pt4
    

    

    
    function calc_D_oo_wm_triplet_cc3_pt4(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_oo_wm_triplet_cc3_pt4
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    double precision, dimension(:), intent(in) :: vrdav_Rl
    double precision, dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , i, b, a, j 
    real(F64), dimension(0:61) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(0) = term(0) + wm_interm_cc3_107_triplet_pt4(a, b, q, i) * wm_interm_cc3_98_triplet_pt4(a, b, p, i)
term(1) = term(1) + wm_interm_cc3_107_triplet_pt4(a, b, q, i) * wm_interm_cc3_98_triplet_pt4(a, b, i, p)
term(2) = term(2) + wm_interm_cc3_107_triplet_pt4(a, b, q, i) * wm_interm_cc3_99_triplet_pt4(a, b, p, i)
term(3) = term(3) + wm_interm_cc3_107_triplet_pt4(a, b, q, i) * wm_interm_cc3_99_triplet_pt4(a, b, i, p)
term(4) = term(4) + wm_interm_cc3_100_triplet_pt4(a, b, p, i) * wm_interm_cc3_107_triplet_pt4(a, b, q, i)
term(5) = term(5) + wm_interm_cc3_100_triplet_pt4(a, b, i, p) * wm_interm_cc3_107_triplet_pt4(a, b, q, i)
term(6) = term(6) + wm_interm_cc3_109_triplet_pt4(a, b, q, i) * wm_interm_cc3_110_triplet_pt4(a, b, p, i)
term(7) = term(7) + wm_interm_cc3_108_triplet_pt4(a, b, p, i) * wm_interm_cc3_109_triplet_pt4(a, b, q, i)
term(8) = term(8) + wm_interm_cc3_109_triplet_pt4(a, b, q, i) * wm_interm_cc3_110_triplet_pt4(a, b, i, p)
term(9) = term(9) + wm_interm_cc3_109_triplet_pt4(a, b, i, q) * wm_interm_cc3_110_triplet_pt4(a, b, i, p)
term(10) = term(10) + wm_interm_cc3_109_triplet_pt4(a, b, i, q) * wm_interm_cc3_110_triplet_pt4(a, b, p, i)
term(11) = term(11) + wm_interm_cc3_108_triplet_pt4(a, b, i, p) * wm_interm_cc3_109_triplet_pt4(a, b, q, i)
term(12) = term(12) + wm_interm_cc3_108_triplet_pt4(a, b, p, i) * wm_interm_cc3_109_triplet_pt4(a, b, i, q)
term(13) = term(13) + wm_interm_cc3_108_triplet_pt4(a, b, i, p) * wm_interm_cc3_109_triplet_pt4(a, b, i, q)
term(14) = term(14) + wm_interm_cc3_102_triplet_pt4(a, b, q, i) * wm_interm_cc3_111_triplet_pt4(a, b, p, i)
term(15) = term(15) + wm_interm_cc3_105_triplet_pt4(a, b, q, i) * wm_interm_cc3_111_triplet_pt4(a, b, p, i)
term(16) = term(16) + wm_interm_cc3_103_triplet_pt4(a, b, q, i) * wm_interm_cc3_111_triplet_pt4(a, b, p, i)
term(17) = term(17) + wm_interm_cc3_105_triplet_pt4(a, b, i, q) * wm_interm_cc3_111_triplet_pt4(a, b, p, i)
term(18) = term(18) + wm_interm_cc3_106_triplet_pt4(a, b, q, i) * wm_interm_cc3_111_triplet_pt4(a, b, p, i)
term(19) = term(19) + wm_interm_cc3_102_triplet_pt4(a, b, i, q) * wm_interm_cc3_111_triplet_pt4(a, b, p, i)
term(20) = term(20) + wm_interm_cc3_103_triplet_pt4(a, b, i, q) * wm_interm_cc3_111_triplet_pt4(a, b, p, i)
term(21) = term(21) + wm_interm_cc3_105_triplet_pt4(a, b, q, i) * wm_interm_cc3_111_triplet_pt4(b, a, p, i)
term(22) = term(22) + wm_interm_cc3_102_triplet_pt4(a, b, q, i) * wm_interm_cc3_111_triplet_pt4(b, a, p, i)
term(23) = term(23) + wm_interm_cc3_106_triplet_pt4(a, b, q, i) * wm_interm_cc3_111_triplet_pt4(b, a, p, i)
term(24) = term(24) + wm_interm_cc3_102_triplet_pt4(a, b, i, q) * wm_interm_cc3_111_triplet_pt4(b, a, p, i)
term(25) = term(25) + wm_interm_cc3_106_triplet_pt4(a, b, i, q) * wm_interm_cc3_111_triplet_pt4(b, a, p, i)
term(26) = term(26) + wm_interm_cc3_105_triplet_pt4(a, b, i, q) * wm_interm_cc3_111_triplet_pt4(b, a, p, i)
term(27) = term(27) + wm_interm_cc3_106_triplet_pt4(a, b, i, q) * wm_interm_cc3_111_triplet_pt4(a, b, p, i)
term(28) = term(28) + wm_interm_cc3_113_triplet_pt4(a, b, q, i) * wm_interm_cc3_114_triplet_pt4(a, b, p, i)
term(29) = term(29) + wm_interm_cc3_112_triplet_pt4(a, b, p, i) * wm_interm_cc3_113_triplet_pt4(a, b, q, i)
term(30) = term(30) + wm_interm_cc3_113_triplet_pt4(a, b, q, i) * wm_interm_cc3_114_triplet_pt4(a, b, i, p)
term(31) = term(31) + wm_interm_cc3_113_triplet_pt4(a, b, q, i) * wm_interm_cc3_116_triplet_pt4(a, b, p, i)
term(32) = term(32) + wm_interm_cc3_113_triplet_pt4(a, b, q, i) * wm_interm_cc3_115_triplet_pt4(a, b, p, i)
term(33) = term(33) + wm_interm_cc3_112_triplet_pt4(a, b, i, p) * wm_interm_cc3_113_triplet_pt4(a, b, q, i)
term(34) = term(34) + wm_interm_cc3_113_triplet_pt4(a, b, q, i) * wm_interm_cc3_117_triplet_pt4(a, b, p, i)
term(35) = term(35) + wm_interm_cc3_113_triplet_pt4(a, b, q, i) * wm_interm_cc3_117_triplet_pt4(a, b, i, p)
term(36) = term(36) + wm_interm_cc3_113_triplet_pt4(a, b, i, q) * wm_interm_cc3_114_triplet_pt4(a, b, p, i)
term(37) = term(37) + wm_interm_cc3_113_triplet_pt4(a, b, i, q) * wm_interm_cc3_115_triplet_pt4(a, b, p, i)
term(38) = term(38) + wm_interm_cc3_112_triplet_pt4(a, b, p, i) * wm_interm_cc3_113_triplet_pt4(a, b, i, q)
term(39) = term(39) + wm_interm_cc3_112_triplet_pt4(a, b, i, p) * wm_interm_cc3_113_triplet_pt4(a, b, i, q)
term(40) = term(40) + wm_interm_cc3_113_triplet_pt4(a, b, q, i) * wm_interm_cc3_116_triplet_pt4(a, b, i, p)
term(41) = term(41) + wm_interm_cc3_113_triplet_pt4(a, b, q, i) * wm_interm_cc3_115_triplet_pt4(a, b, i, p)
term(42) = term(42) + wm_interm_cc3_113_triplet_pt4(a, b, i, q) * wm_interm_cc3_115_triplet_pt4(a, b, i, p)
term(43) = term(43) + wm_interm_cc3_113_triplet_pt4(a, b, i, q) * wm_interm_cc3_114_triplet_pt4(a, b, i, p)
term(44) = term(44) + wm_interm_cc3_113_triplet_pt4(a, b, i, q) * wm_interm_cc3_117_triplet_pt4(a, b, i, p)
term(45) = term(45) + wm_interm_cc3_113_triplet_pt4(a, b, i, q) * wm_interm_cc3_116_triplet_pt4(a, b, p, i)
term(46) = term(46) + wm_interm_cc3_113_triplet_pt4(a, b, i, q) * wm_interm_cc3_117_triplet_pt4(a, b, p, i)
term(47) = term(47) + wm_interm_cc3_113_triplet_pt4(a, b, i, q) * wm_interm_cc3_116_triplet_pt4(a, b, i, p)
end do 
end do 
end do 

term(0) = term(0) * 24.0d+0 
term(1) = term(1) * (-12.0d+0) 
term(2) = term(2) * (-12.0d+0) 
term(3) = term(3) * 6.0d+0 
term(4) = term(4) * 6.0d+0 
term(5) = term(5) * (-12.0d+0) 
term(6) = term(6) * (-12.0d+0) 
term(7) = term(7) * 12.0d+0 
term(8) = term(8) * 6.0d+0 
term(9) = term(9) * (-12.0d+0) 
term(10) = term(10) * 6.0d+0 
term(11) = term(11) * (-6.0d+0) 
term(12) = term(12) * (-6.0d+0) 
term(13) = term(13) * 12.0d+0 
term(14) = term(14) * (-16.0d+0) 
term(15) = term(15) * 8.0d+0 
term(16) = term(16) * 4.0d+0 
term(17) = term(17) * (-4.0d+0) 
term(18) = term(18) * (-2.0d+0) 
term(19) = term(19) * 8.0d+0 
term(20) = term(20) * (-2.0d+0) 
term(21) = term(21) * (-2.0d+0) 
term(22) = term(22) * 4.0d+0 
term(23) = term(23) * 4.0d+0 
term(24) = term(24) * (-8.0d+0) 
term(25) = term(25) * (-2.0d+0) 
term(26) = term(26) * 4.0d+0 
term(27) = term(27) * 4.0d+0 
term(28) = term(28) * 4.0d+0 
term(29) = term(29) * (-12.0d+0) 
term(30) = term(30) * (-2.0d+0) 
term(31) = term(31) * 4.0d+0 
term(32) = -term(32) 
term(33) = term(33) * 6.0d+0 
term(34) = -term(34) 
term(35) = term(35) * 2.0d+0 
term(36) = term(36) * (-2.0d+0) 
term(37) = term(37) * 2.0d+0 
term(38) = term(38) * 6.0d+0 
term(39) = term(39) * (-12.0d+0) 
term(40) = term(40) * (-2.0d+0) 
term(41) = term(41) * 2.0d+0 
term(42) = -term(42) 
term(43) = term(43) * 4.0d+0 
term(44) = -term(44) 
term(45) = term(45) * (-2.0d+0) 
term(46) = term(46) * 2.0d+0 
term(47) = term(47) * 4.0d+0 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(48) = term(48) + wm_interm_cc3_108_triplet_pt4(a, b, i, j) * wm_interm_cc3_109_triplet_pt4(a, b, i, j)
term(49) = term(49) + wm_interm_cc3_109_triplet_pt4(a, b, i, j) * wm_interm_cc3_110_triplet_pt4(a, b, i, j)
term(50) = term(50) + wm_interm_cc3_108_triplet_pt4(a, b, i, j) * wm_interm_cc3_109_triplet_pt4(a, b, j, i)
term(51) = term(51) + wm_interm_cc3_109_triplet_pt4(a, b, i, j) * wm_interm_cc3_110_triplet_pt4(a, b, j, i)
term(52) = term(52) + wm_interm_cc3_112_triplet_pt4(a, b, i, j) * wm_interm_cc3_113_triplet_pt4(a, b, i, j)
term(53) = term(53) + wm_interm_cc3_113_triplet_pt4(a, b, i, j) * wm_interm_cc3_114_triplet_pt4(a, b, i, j)
term(54) = term(54) + wm_interm_cc3_112_triplet_pt4(a, b, i, j) * wm_interm_cc3_113_triplet_pt4(a, b, j, i)
term(55) = term(55) + wm_interm_cc3_113_triplet_pt4(a, b, i, j) * wm_interm_cc3_115_triplet_pt4(a, b, i, j)
term(56) = term(56) + wm_interm_cc3_113_triplet_pt4(a, b, i, j) * wm_interm_cc3_114_triplet_pt4(a, b, j, i)
term(57) = term(57) + wm_interm_cc3_113_triplet_pt4(a, b, i, j) * wm_interm_cc3_116_triplet_pt4(a, b, i, j)
term(58) = term(58) + wm_interm_cc3_113_triplet_pt4(a, b, i, j) * wm_interm_cc3_115_triplet_pt4(a, b, j, i)
term(59) = term(59) + wm_interm_cc3_113_triplet_pt4(a, b, i, j) * wm_interm_cc3_117_triplet_pt4(a, b, i, j)
term(60) = term(60) + wm_interm_cc3_113_triplet_pt4(a, b, i, j) * wm_interm_cc3_117_triplet_pt4(a, b, j, i)
term(61) = term(61) + wm_interm_cc3_113_triplet_pt4(a, b, i, j) * wm_interm_cc3_116_triplet_pt4(a, b, j, i)
end do 
end do 
end do 
end do 

term(48) = term(48) * (-48.0d+0) 
term(49) = term(49) * 48.0d+0 
term(50) = term(50) * 24.0d+0 
term(51) = term(51) * (-24.0d+0) 
term(52) = term(52) * 48.0d+0 
term(53) = term(53) * (-16.0d+0) 
term(54) = term(54) * (-24.0d+0) 
term(55) = term(55) * 4.0d+0 
term(56) = term(56) * 8.0d+0 
term(57) = term(57) * (-16.0d+0) 
term(58) = term(58) * (-8.0d+0) 
term(59) = term(59) * 4.0d+0 
term(60) = term(60) * (-8.0d+0) 
term(61) = term(61) * 8.0d+0 


    calc_D_oo_wm_triplet_cc3_pt4 = zero
    do s = 0, 61
    calc_D_oo_wm_triplet_cc3_pt4 = calc_D_oo_wm_triplet_cc3_pt4 + term(s)
    end do

    end function calc_D_oo_wm_triplet_cc3_pt4
    
    function calc_D_ov_wm_triplet_cc3_pt4(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_ov_wm_triplet_cc3_pt4
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    double precision, dimension(:), intent(in) :: vrdav_Rl
    double precision, dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , a, i, k, b, j 
    real(F64), dimension(0:130) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do a = nocc + 1, nactive 
term(0) = term(0) + wm_interm_cc3_0_triplet_pt4(q, a) * wm_interm_cc3_1_triplet_pt4(a, p)
term(1) = term(1) + wm_interm_cc3_0_triplet_pt4(q, a) * wm_interm_cc3_2_triplet_pt4(a, p)
term(2) = term(2) + wm_interm_cc3_0_triplet_pt4(q, a) * wm_interm_cc3_3_triplet_pt4(a, p)
term(3) = term(3) + wm_interm_cc3_0_triplet_pt4(q, a) * wm_interm_cc3_4_triplet_pt4(a, p)
term(4) = term(4) + wm_interm_cc3_0_triplet_pt4(q, a) * wm_interm_cc3_5_triplet_pt4(a, p)
term(5) = term(5) + wm_interm_cc3_0_triplet_pt4(q, a) * wm_interm_cc3_6_triplet_pt4(a, p)
term(6) = term(6) + wm_interm_cc3_0_triplet_pt4(q, a) * wm_interm_cc3_11_triplet_pt4(a, p)
term(7) = term(7) + wm_interm_cc3_0_triplet_pt4(q, a) * wm_interm_cc3_12_triplet_pt4(a, p)
term(8) = term(8) + wm_interm_cc3_0_triplet_pt4(q, a) * wm_interm_cc3_13_triplet_pt4(a, p)
term(9) = term(9) + wm_interm_cc3_0_triplet_pt4(q, a) * wm_interm_cc3_14_triplet_pt4(a, p)
term(10) = term(10) + wm_interm_cc3_0_triplet_pt4(q, a) * wm_interm_cc3_15_triplet_pt4(a, p)
term(11) = term(11) + wm_interm_cc3_0_triplet_pt4(q, a) * wm_interm_cc3_16_triplet_pt4(a, p)
term(12) = term(12) + wm_interm_cc3_28_triplet_pt4(a, q) * wm_interm_cc3_31_triplet_pt4(a, p)
term(13) = term(13) + wm_interm_cc3_28_triplet_pt4(a, q) * wm_interm_cc3_34_triplet_pt4(a, p)
term(14) = term(14) + wm_interm_cc3_28_triplet_pt4(a, q) * wm_interm_cc3_33_triplet_pt4(a, p)
term(15) = term(15) + wm_interm_cc3_28_triplet_pt4(a, q) * wm_interm_cc3_32_triplet_pt4(a, p)
term(16) = term(16) + wm_interm_cc3_28_triplet_pt4(a, q) * wm_interm_cc3_29_triplet_pt4(a, p)
term(17) = term(17) + wm_interm_cc3_28_triplet_pt4(a, q) * wm_interm_cc3_30_triplet_pt4(a, p)
term(18) = term(18) + wm_interm_cc3_28_triplet_pt4(a, q) * wm_interm_cc3_57_triplet_pt4(a, p)
term(19) = term(19) + wm_interm_cc3_28_triplet_pt4(a, q) * wm_interm_cc3_58_triplet_pt4(a, p)
term(20) = term(20) + wm_interm_cc3_28_triplet_pt4(a, q) * wm_interm_cc3_55_triplet_pt4(a, p)
term(21) = term(21) + wm_interm_cc3_28_triplet_pt4(a, q) * wm_interm_cc3_56_triplet_pt4(a, p)
end do 

term(0) = term(0) * 1.5d+0 
term(1) = term(1) * (-1.5d+0) 
term(2) = term(2) * (-3.0d+0) 
term(3) = term(3) * 3.0d+0 
term(4) = term(4) * (-1.5d+0) 
term(5) = term(5) * 1.5d+0 
term(6) = term(6) * (-2.0d+0) 
term(7) = term(7) * 4.0d+0 
term(8) = term(8) * (-2.0d+0) 
term(9) = term(9) * 4.0d+0 
term(10) = term(10) * 4.0d+0 
term(11) = term(11) * (-8.0d+0) 
term(12) = term(12) * (-3.0d+0) 
term(13) = term(13) * 6.0d+0 
term(14) = term(14) * (-3.0d+0) 
term(15) = term(15) * 6.0d+0 
term(16) = term(16) * 6.0d+0 
term(17) = term(17) * (-12.0d+0) 
term(18) = term(18) * (-12.0d+0) 
term(19) = term(19) * 24.0d+0 
term(20) = term(20) * 12.0d+0 
term(21) = term(21) * (-24.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(22) = term(22) + s1(a,i) * t3(nocc, nactive, a,b,q,k,j,i) * wm_interm_cc3_48_triplet_pt4(b, k, j, p)
term(23) = term(23) + s1(a,i) * t3(nocc, nactive, a,b,q,k,j,i) * wm_interm_cc3_48_triplet_pt4(b, j, k, p)
term(24) = term(24) + s1(a,j) * t3(nocc, nactive, a,b,q,k,j,i) * wm_interm_cc3_48_triplet_pt4(b, i, k, p)
term(25) = term(25) + s1(a,j) * t3(nocc, nactive, a,b,q,k,j,i) * wm_interm_cc3_48_triplet_pt4(b, k, i, p)
term(26) = term(26) + s1(a,i) * t3(nocc, nactive, a,b,q,k,j,i) * wm_interm_cc3_71_triplet_pt4(b, k, j, p)
term(27) = term(27) + s1(a,i) * t3(nocc, nactive, a,b,q,k,j,i) * wm_interm_cc3_72_triplet_pt4(b, k, j, p)
term(28) = term(28) + s1(a,i) * t3(nocc, nactive, a,b,q,k,j,i) * wm_interm_cc3_71_triplet_pt4(b, j, k, p)
term(29) = term(29) + s1(a,i) * t3(nocc, nactive, a,b,q,k,j,i) * wm_interm_cc3_72_triplet_pt4(b, j, k, p)
term(30) = term(30) + s1(a,j) * t3(nocc, nactive, a,b,q,k,j,i) * wm_interm_cc3_71_triplet_pt4(b, i, k, p)
term(31) = term(31) + s1(a,j) * t3(nocc, nactive, a,b,q,k,j,i) * wm_interm_cc3_72_triplet_pt4(b, i, k, p)
term(32) = term(32) + s1(a,j) * t3(nocc, nactive, a,b,q,k,j,i) * wm_interm_cc3_71_triplet_pt4(b, k, i, p)
term(33) = term(33) + s1(a,j) * t3(nocc, nactive, a,b,q,k,j,i) * wm_interm_cc3_72_triplet_pt4(b, k, i, p)
term(34) = term(34) + r3(vrdav_Rl, a,j,b,k,q,i) * t1(b,k) * wm_interm_cc3_96_triplet_pt4(a, i, j, p)
term(35) = term(35) + r3(vrdav_Rl, a,j,b,k,q,i) * t1(b,k) * wm_interm_cc3_96_triplet_pt4(a, j, i, p)
term(36) = term(36) + r3(vrdav_Rl, a,j,b,k,q,i) * t1(b,j) * wm_interm_cc3_96_triplet_pt4(a, i, k, p)
term(37) = term(37) + r3(vrdav_Rl, a,j,b,k,q,i) * t1(b,i) * wm_interm_cc3_96_triplet_pt4(a, j, k, p)
term(38) = term(38) + r3(vrdav_Rl, a,j,b,k,q,i) * t1(a,k) * wm_interm_cc3_96_triplet_pt4(b, i, j, p)
term(39) = term(39) + r3(vrdav_Rl, a,j,b,k,q,i) * t1(a,j) * wm_interm_cc3_96_triplet_pt4(b, i, k, p)
term(40) = term(40) + r3(vrdav_Rl, a,j,b,k,q,i) * t1(a,i) * wm_interm_cc3_96_triplet_pt4(b, j, k, p)
term(41) = term(41) + r3(vrdav_Rl, a,j,b,k,q,i) * t1(b,j) * wm_interm_cc3_96_triplet_pt4(a, k, i, p)
term(42) = term(42) + r3(vrdav_Rl, a,j,b,k,q,i) * t1(a,k) * wm_interm_cc3_96_triplet_pt4(b, j, i, p)
term(43) = term(43) + r3(vrdav_Rl, a,j,b,k,q,i) * t1(a,j) * wm_interm_cc3_96_triplet_pt4(b, k, i, p)
term(44) = term(44) + r3(vrdav_Rl, a,j,b,k,q,i) * t1(b,k) * wm_interm_cc3_97_triplet_pt4(a, i, j, p)
term(45) = term(45) + r3(vrdav_Rl, a,j,b,k,q,i) * t1(b,k) * wm_interm_cc3_97_triplet_pt4(a, j, i, p)
term(46) = term(46) + r3(vrdav_Rl, a,j,b,k,q,i) * t1(b,j) * wm_interm_cc3_97_triplet_pt4(a, i, k, p)
term(47) = term(47) + r3(vrdav_Rl, a,j,b,k,q,i) * t1(b,j) * wm_interm_cc3_97_triplet_pt4(a, k, i, p)
term(48) = term(48) + r3(vrdav_Rl, a,j,b,k,q,i) * t1(b,i) * wm_interm_cc3_97_triplet_pt4(a, k, j, p)
term(49) = term(49) + r3(vrdav_Rl, a,j,b,k,q,i) * t1(b,i) * wm_interm_cc3_97_triplet_pt4(a, j, k, p)
term(50) = term(50) + r3(vrdav_Rl, a,j,b,k,q,i) * t1(a,k) * wm_interm_cc3_97_triplet_pt4(b, i, j, p)
term(51) = term(51) + r3(vrdav_Rl, a,j,b,k,q,i) * t1(a,k) * wm_interm_cc3_97_triplet_pt4(b, j, i, p)
term(52) = term(52) + r3(vrdav_Rl, a,j,b,k,q,i) * t1(a,j) * wm_interm_cc3_97_triplet_pt4(b, i, k, p)
term(53) = term(53) + r3(vrdav_Rl, a,j,b,k,q,i) * t1(a,j) * wm_interm_cc3_97_triplet_pt4(b, k, i, p)
term(54) = term(54) + r3(vrdav_Rl, a,j,b,k,q,i) * t1(a,i) * wm_interm_cc3_97_triplet_pt4(b, j, k, p)
term(55) = term(55) + r3(vrdav_Rl, a,j,b,k,q,i) * t1(a,i) * wm_interm_cc3_97_triplet_pt4(b, k, j, p)
end do 
end do 
end do 
end do 
end do 

term(22) = term(22) * 2.9999999999999996d+0 
term(23) = term(23) * (-2.9999999999999996d+0) 
term(24) = term(24) * 2.9999999999999996d+0 
term(25) = term(25) * (-2.9999999999999996d+0) 
term(26) = term(26) * (-1.9999999999999998d+0) 
term(27) = term(27) * 3.9999999999999996d+0 
term(28) = term(28) * 3.9999999999999996d+0 
term(29) = term(29) * (-1.9999999999999998d+0) 
term(30) = term(30) * (-1.9999999999999998d+0) 
term(31) = term(31) * 3.9999999999999996d+0 
term(32) = term(32) * 3.9999999999999996d+0 
term(33) = term(33) * (-1.9999999999999998d+0) 
term(34) = term(34) * 6.0d+0 
term(35) = term(35) * (-12.0d+0) 
term(36) = term(36) * (-6.0d+0) 
term(37) = term(37) * 9.0d+0 
term(38) = term(38) * (-3.0d+0) 
term(39) = term(39) * 12.0d+0 
term(40) = term(40) * (-9.0d+0) 
term(41) = term(41) * 3.0d+0 
term(42) = term(42) * 6.0d+0 
term(43) = term(43) * (-6.0d+0) 
term(44) = term(44) * 12.0d+0 
term(45) = term(45) * (-24.0d+0) 
term(46) = term(46) * (-6.0d+0) 
term(47) = term(47) * 12.0d+0 
term(48) = term(48) * (-6.0d+0) 
term(49) = term(49) * 12.0d+0 
term(50) = term(50) * (-6.0d+0) 
term(51) = term(51) * 12.0d+0 
term(52) = term(52) * 12.0d+0 
term(53) = term(53) * (-24.0d+0) 
term(54) = term(54) * (-6.0d+0) 
term(55) = term(55) * 12.0d+0 

do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(56) = term(56) + s1(a,j) * t3(nocc, nactive, a,b,q,j,k,i) * wm_interm_cc3_48_triplet_pt4(b, i, k, p)
term(57) = term(57) + s1(a,j) * t3(nocc, nactive, a,b,q,j,k,i) * wm_interm_cc3_48_triplet_pt4(b, k, i, p)
term(58) = term(58) + s1(a,j) * t3(nocc, nactive, a,b,q,j,k,i) * wm_interm_cc3_71_triplet_pt4(b, i, k, p)
term(59) = term(59) + s1(a,j) * t3(nocc, nactive, a,b,q,j,k,i) * wm_interm_cc3_72_triplet_pt4(b, i, k, p)
term(60) = term(60) + s1(a,j) * t3(nocc, nactive, a,b,q,j,k,i) * wm_interm_cc3_71_triplet_pt4(b, k, i, p)
term(61) = term(61) + s1(a,j) * t3(nocc, nactive, a,b,q,j,k,i) * wm_interm_cc3_72_triplet_pt4(b, k, i, p)
end do 
end do 
end do 
end do 
end do 

term(56) = term(56) * (-5.999999999999999d+0) 
term(57) = term(57) * 5.999999999999999d+0 
term(58) = term(58) * 3.9999999999999996d+0 
term(59) = term(59) * (-7.999999999999999d+0) 
term(60) = term(60) * (-7.999999999999999d+0) 
term(61) = term(61) * 3.9999999999999996d+0 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(62) = term(62) + s1(a,j) * s1(q,i) * wm_interm_cc3_27_triplet_pt4(a, p, i, j)
term(63) = term(63) + s1(a,j) * s1(q,i) * wm_interm_cc3_26_triplet_pt4(a, p, i, j)
term(64) = term(64) + s1(a,j) * s1(q,i) * wm_interm_cc3_24_triplet_pt4(a, p, i, j)
term(65) = term(65) + s1(a,j) * t1(q,i) * wm_interm_cc3_27_triplet_pt4(a, i, p, j)
term(66) = term(66) + s1(a,j) * t1(q,i) * wm_interm_cc3_26_triplet_pt4(a, i, p, j)
term(67) = term(67) + s1(a,j) * t1(q,i) * wm_interm_cc3_27_triplet_pt4(a, i, j, p)
term(68) = term(68) + s1(a,j) * t1(q,i) * wm_interm_cc3_24_triplet_pt4(a, i, p, j)
term(69) = term(69) + s1(a,j) * t1(q,i) * wm_interm_cc3_24_triplet_pt4(a, i, j, p)
term(70) = term(70) + s1(a,j) * t1(q,i) * wm_interm_cc3_26_triplet_pt4(a, i, j, p)
term(71) = term(71) + s1(a,j) * s1(q,i) * wm_interm_cc3_52_triplet_pt4(a, p, i, j)
term(72) = term(72) + s1(a,j) * s1(q,i) * wm_interm_cc3_53_triplet_pt4(a, p, i, j)
term(73) = term(73) + s1(a,j) * s1(q,i) * wm_interm_cc3_54_triplet_pt4(a, p, i, j)
term(74) = term(74) + s1(a,j) * s1(q,i) * wm_interm_cc3_51_triplet_pt4(a, p, i, j)
term(75) = term(75) + s1(a,j) * t1(q,i) * wm_interm_cc3_52_triplet_pt4(a, i, p, j)
term(76) = term(76) + s1(a,j) * t1(q,i) * wm_interm_cc3_53_triplet_pt4(a, i, p, j)
term(77) = term(77) + s1(a,j) * t1(q,i) * wm_interm_cc3_52_triplet_pt4(a, i, j, p)
term(78) = term(78) + s1(a,j) * t1(q,i) * wm_interm_cc3_54_triplet_pt4(a, i, p, j)
term(79) = term(79) + s1(a,j) * t1(q,i) * wm_interm_cc3_54_triplet_pt4(a, i, j, p)
term(80) = term(80) + s1(a,j) * t1(q,i) * wm_interm_cc3_53_triplet_pt4(a, i, j, p)
term(81) = term(81) + s1(a,j) * t1(q,i) * wm_interm_cc3_51_triplet_pt4(a, i, p, j)
term(82) = term(82) + s1(a,j) * t1(q,i) * wm_interm_cc3_51_triplet_pt4(a, i, j, p)
term(83) = term(83) + t1(a,j) * t1(q,i) * wm_interm_cc3_79_triplet_pt4(a, i, j, p)
term(84) = term(84) + t1(a,j) * t1(q,i) * wm_interm_cc3_81_triplet_pt4(a, i, j, p)
term(85) = term(85) + t1(a,j) * t1(q,i) * wm_interm_cc3_76_triplet_pt4(a, i, j, p)
term(86) = term(86) + t1(a,j) * t1(q,i) * wm_interm_cc3_78_triplet_pt4(a, i, j, p)
term(87) = term(87) + t1(a,j) * t1(q,i) * wm_interm_cc3_85_triplet_pt4(a, i, j, p)
term(88) = term(88) + t1(a,j) * t1(q,i) * wm_interm_cc3_92_triplet_pt4(a, i, j, p)
term(89) = term(89) + t1(a,j) * t1(q,i) * wm_interm_cc3_93_triplet_pt4(a, i, j, p)
term(90) = term(90) + t1(a,j) * t1(q,i) * wm_interm_cc3_87_triplet_pt4(a, i, j, p)
term(91) = term(91) + t1(a,j) * t1(q,i) * wm_interm_cc3_86_triplet_pt4(a, i, j, p)
term(92) = term(92) + t1(a,j) * t1(q,i) * wm_interm_cc3_90_triplet_pt4(a, i, j, p)
term(93) = term(93) + t1(a,j) * t1(q,i) * wm_interm_cc3_91_triplet_pt4(a, i, j, p)
end do 
end do 
end do 

term(62) = term(62) * 18.0d+0 
term(63) = term(63) * (-18.0d+0) 
term(64) = term(64) * 9.0d+0 
term(65) = term(65) * 18.0d+0 
term(66) = term(66) * (-18.0d+0) 
term(67) = term(67) * (-9.0d+0) 
term(68) = term(68) * 9.0d+0 
term(69) = term(69) * (-9.0d+0) 
term(70) = term(70) * 9.0d+0 
term(71) = term(71) * (-24.0d+0) 
term(72) = term(72) * 48.0d+0 
term(73) = term(73) * (-12.0d+0) 
term(74) = term(74) * 6.0d+0 
term(75) = term(75) * (-24.0d+0) 
term(76) = term(76) * 48.0d+0 
term(77) = term(77) * 12.0d+0 
term(78) = term(78) * (-12.0d+0) 
term(79) = term(79) * 6.0d+0 
term(80) = term(80) * (-24.0d+0) 
term(81) = term(81) * 6.0d+0 
term(82) = term(82) * (-12.0d+0) 
term(83) = term(83) * 12.0d+0 
term(84) = term(84) * (-6.0d+0) 
term(85) = term(85) * (-9.0d+0) 
term(86) = term(86) * 6.0d+0 
term(87) = term(87) * (-3.0d+0) 
term(88) = term(88) * 12.0d+0 
term(89) = term(89) * (-24.0d+0) 
term(90) = term(90) * (-6.0d+0) 
term(91) = term(91) * 12.0d+0 
term(92) = term(92) * (-6.0d+0) 
term(93) = term(93) * 12.0d+0 

do i = 1, nocc 
term(94) = term(94) + wm_interm_cc3_40_triplet_pt4(q, i) * wm_interm_cc3_47_triplet_pt4(p, i)
term(95) = term(95) + wm_interm_cc3_41_triplet_pt4(q, i) * wm_interm_cc3_47_triplet_pt4(p, i)
term(96) = term(96) + wm_interm_cc3_42_triplet_pt4(q, i) * wm_interm_cc3_47_triplet_pt4(p, i)
term(97) = term(97) + wm_interm_cc3_44_triplet_pt4(q, i) * wm_interm_cc3_47_triplet_pt4(p, i)
term(98) = term(98) + wm_interm_cc3_45_triplet_pt4(q, i) * wm_interm_cc3_47_triplet_pt4(p, i)
term(99) = term(99) + wm_interm_cc3_46_triplet_pt4(q, i) * wm_interm_cc3_47_triplet_pt4(p, i)
term(100) = term(100) + wm_interm_cc3_47_triplet_pt4(p, i) * wm_interm_cc3_65_triplet_pt4(q, i)
term(101) = term(101) + wm_interm_cc3_47_triplet_pt4(p, i) * wm_interm_cc3_66_triplet_pt4(q, i)
term(102) = term(102) + wm_interm_cc3_47_triplet_pt4(p, i) * wm_interm_cc3_67_triplet_pt4(q, i)
term(103) = term(103) + wm_interm_cc3_47_triplet_pt4(p, i) * wm_interm_cc3_69_triplet_pt4(q, i)
term(104) = term(104) + wm_interm_cc3_25_triplet_pt4(p, i) * wm_interm_cc3_77_triplet_pt4(q, i)
term(105) = term(105) + wm_interm_cc3_25_triplet_pt4(p, i) * wm_interm_cc3_80_triplet_pt4(q, i)
term(106) = term(106) + wm_interm_cc3_25_triplet_pt4(p, i) * wm_interm_cc3_82_triplet_pt4(q, i)
term(107) = term(107) + wm_interm_cc3_25_triplet_pt4(p, i) * wm_interm_cc3_84_triplet_pt4(q, i)
term(108) = term(108) + wm_interm_cc3_25_triplet_pt4(p, i) * wm_interm_cc3_83_triplet_pt4(q, i)
term(109) = term(109) + wm_interm_cc3_25_triplet_pt4(p, i) * wm_interm_cc3_88_triplet_pt4(q, i)
term(110) = term(110) + wm_interm_cc3_25_triplet_pt4(p, i) * wm_interm_cc3_89_triplet_pt4(q, i)
term(111) = term(111) + wm_interm_cc3_25_triplet_pt4(p, i) * wm_interm_cc3_94_triplet_pt4(q, i)
term(112) = term(112) + wm_interm_cc3_25_triplet_pt4(p, i) * wm_interm_cc3_95_triplet_pt4(q, i)
end do 

term(94) = term(94) * 2.0d+0 
term(95) = term(95) * (-4.0d+0) 
term(96) = -term(96) 
term(97) = term(97) * 1.9999999999999998d+0 
term(98) = term(98) * 1.9999999999999998d+0 
term(99) = -term(99) 
term(100) = term(100) * 4.0d+0 
term(101) = term(101) * (-8.0d+0) 
term(102) = term(102) * (-3.9999999999999996d+0) 
term(103) = term(103) * 7.999999999999999d+0 
term(104) = term(104) * (-3.0d+0) 
term(105) = term(105) * 6.0d+0 
term(106) = term(106) * (-18.0d+0) 
term(107) = term(107) * 6.0d+0 
term(108) = term(108) * (-3.0d+0) 
term(109) = term(109) * (-12.0d+0) 
term(110) = term(110) * 24.0d+0 
term(111) = term(111) * 12.0d+0 
term(112) = term(112) * (-24.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(113) = term(113) + s1(a,j) * s1(q,i) * wm_interm_cc3_27_triplet_pt4(a, p, j, i)
term(114) = term(114) + s1(a,j) * s1(q,i) * wm_interm_cc3_24_triplet_pt4(a, p, j, i)
term(115) = term(115) + s1(a,j) * s1(q,i) * wm_interm_cc3_26_triplet_pt4(a, p, j, i)
term(116) = term(116) + s1(a,j) * s1(q,i) * wm_interm_cc3_52_triplet_pt4(a, p, j, i)
term(117) = term(117) + s1(a,j) * s1(q,i) * wm_interm_cc3_54_triplet_pt4(a, p, j, i)
term(118) = term(118) + s1(a,j) * s1(q,i) * wm_interm_cc3_53_triplet_pt4(a, p, j, i)
term(119) = term(119) + s1(a,j) * s1(q,i) * wm_interm_cc3_51_triplet_pt4(a, p, j, i)
term(120) = term(120) + t1(a,j) * t1(q,i) * wm_interm_cc3_81_triplet_pt4(a, j, i, p)
term(121) = term(121) + t1(a,j) * t1(q,i) * wm_interm_cc3_78_triplet_pt4(a, j, i, p)
term(122) = term(122) + t1(a,j) * t1(q,i) * wm_interm_cc3_79_triplet_pt4(a, j, i, p)
term(123) = term(123) + t1(a,j) * t1(q,i) * wm_interm_cc3_76_triplet_pt4(a, j, i, p)
term(124) = term(124) + t1(a,j) * t1(q,i) * wm_interm_cc3_85_triplet_pt4(a, j, i, p)
term(125) = term(125) + t1(a,j) * t1(q,i) * wm_interm_cc3_92_triplet_pt4(a, j, i, p)
term(126) = term(126) + t1(a,j) * t1(q,i) * wm_interm_cc3_93_triplet_pt4(a, j, i, p)
term(127) = term(127) + t1(a,j) * t1(q,i) * wm_interm_cc3_90_triplet_pt4(a, j, i, p)
term(128) = term(128) + t1(a,j) * t1(q,i) * wm_interm_cc3_91_triplet_pt4(a, j, i, p)
term(129) = term(129) + t1(a,j) * t1(q,i) * wm_interm_cc3_86_triplet_pt4(a, j, i, p)
term(130) = term(130) + t1(a,j) * t1(q,i) * wm_interm_cc3_87_triplet_pt4(a, j, i, p)
end do 
end do 
end do 

term(113) = term(113) * (-9.0d+0) 
term(114) = term(114) * (-9.0d+0) 
term(115) = term(115) * 9.0d+0 
term(116) = term(116) * 12.0d+0 
term(117) = term(117) * 6.0d+0 
term(118) = term(118) * (-24.0d+0) 
term(119) = term(119) * (-12.0d+0) 
term(120) = term(120) * 3.0d+0 
term(121) = term(121) * (-12.0d+0) 
term(122) = term(122) * (-6.0d+0) 
term(123) = term(123) * 9.0d+0 
term(124) = term(124) * 6.0d+0 
term(125) = term(125) * (-6.0d+0) 
term(126) = term(126) * 12.0d+0 
term(127) = term(127) * 12.0d+0 
term(128) = term(128) * (-24.0d+0) 
term(129) = term(129) * (-6.0d+0) 
term(130) = term(130) * 12.0d+0 


    calc_D_ov_wm_triplet_cc3_pt4 = zero
    do s = 0, 130
    calc_D_ov_wm_triplet_cc3_pt4 = calc_D_ov_wm_triplet_cc3_pt4 + term(s)
    end do

    end function calc_D_ov_wm_triplet_cc3_pt4
    
    function calc_D_vo_wm_triplet_cc3_pt4(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vo_wm_triplet_cc3_pt4
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    double precision, dimension(:), intent(in) :: vrdav_Rl
    double precision, dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , i, j, b, a, c, k 
    real(F64), dimension(0:439) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(0) = term(0) + r3(vrdav_Rr, a,q,b,j,p,i) * s1(b,j) * wm_interm_cc3_23_triplet_pt4(a, i)
term(1) = term(1) + r3(vrdav_Rr, a,q,b,j,p,i) * s1(a,j) * wm_interm_cc3_23_triplet_pt4(b, i)
term(2) = term(2) + r3(vrdav_Rr, a,q,b,j,p,i) * s1(b,i) * wm_interm_cc3_23_triplet_pt4(a, j)
term(3) = term(3) + r3(vrdav_Rr, a,q,b,j,p,i) * s1(a,i) * wm_interm_cc3_23_triplet_pt4(b, j)
term(4) = term(4) + r3(vrdav_Rr, a,q,b,j,p,i) * s1(b,j) * wm_interm_cc3_49_triplet_pt4(a, i)
term(5) = term(5) + r3(vrdav_Rr, a,q,b,j,p,i) * s1(a,j) * wm_interm_cc3_49_triplet_pt4(b, i)
term(6) = term(6) + r3(vrdav_Rr, a,q,b,j,p,i) * s1(b,i) * wm_interm_cc3_49_triplet_pt4(a, j)
term(7) = term(7) + r3(vrdav_Rr, a,q,b,j,p,i) * s1(a,i) * wm_interm_cc3_49_triplet_pt4(b, j)
term(8) = term(8) + r3(vrdav_Rr, a,q,b,j,p,i) * s1(b,j) * wm_interm_cc3_50_triplet_pt4(a, i)
term(9) = term(9) + r3(vrdav_Rr, a,q,b,j,p,i) * s1(a,j) * wm_interm_cc3_50_triplet_pt4(b, i)
term(10) = term(10) + r3(vrdav_Rr, a,q,b,j,p,i) * s1(b,i) * wm_interm_cc3_50_triplet_pt4(a, j)
term(11) = term(11) + r3(vrdav_Rr, a,q,b,j,p,i) * s1(a,i) * wm_interm_cc3_50_triplet_pt4(b, j)
term(12) = term(12) + r3(vrdav_Rl, a,q,b,j,p,i) * t1(b,j) * wm_interm_cc3_73_triplet_pt4(a, i)
term(13) = term(13) + r3(vrdav_Rl, a,q,b,j,p,i) * t1(a,j) * wm_interm_cc3_73_triplet_pt4(b, i)
term(14) = term(14) + r3(vrdav_Rl, a,q,b,j,p,i) * t1(b,i) * wm_interm_cc3_73_triplet_pt4(a, j)
term(15) = term(15) + r3(vrdav_Rl, a,q,b,j,p,i) * t1(a,i) * wm_interm_cc3_73_triplet_pt4(b, j)
term(16) = term(16) + r3(vrdav_Rl, a,q,b,j,p,i) * t1(b,j) * wm_interm_cc3_74_triplet_pt4(a, i)
term(17) = term(17) + r3(vrdav_Rl, a,q,b,j,p,i) * t1(b,j) * wm_interm_cc3_75_triplet_pt4(a, i)
term(18) = term(18) + r3(vrdav_Rl, a,q,b,j,p,i) * t1(a,j) * wm_interm_cc3_74_triplet_pt4(b, i)
term(19) = term(19) + r3(vrdav_Rl, a,q,b,j,p,i) * t1(b,i) * wm_interm_cc3_74_triplet_pt4(a, j)
term(20) = term(20) + r3(vrdav_Rl, a,q,b,j,p,i) * t1(a,i) * wm_interm_cc3_74_triplet_pt4(b, j)
term(21) = term(21) + r3(vrdav_Rl, a,q,b,j,p,i) * t1(a,j) * wm_interm_cc3_75_triplet_pt4(b, i)
term(22) = term(22) + r3(vrdav_Rl, a,q,b,j,p,i) * t1(b,i) * wm_interm_cc3_75_triplet_pt4(a, j)
term(23) = term(23) + r3(vrdav_Rl, a,q,b,j,p,i) * t1(a,i) * wm_interm_cc3_75_triplet_pt4(b, j)
term(24) = term(24) + r2m(vrdav_Rl, a,q,p,i) * s1(b,j) * wm_interm_cc3_98_triplet_pt4(a, b, j, i)
term(25) = term(25) + r2m(vrdav_Rl, a,i,p,q) * s1(b,j) * wm_interm_cc3_98_triplet_pt4(a, b, j, i)
term(26) = term(26) + r2m(vrdav_Rr, a,q,p,i) * t1(b,j) * wm_interm_cc3_102_triplet_pt4(a, b, j, i)
term(27) = term(27) + r2m(vrdav_Rr, a,i,p,q) * t1(b,j) * wm_interm_cc3_102_triplet_pt4(a, b, j, i)
term(28) = term(28) + r2m(vrdav_Rr, a,q,p,i) * t1(b,j) * wm_interm_cc3_103_triplet_pt4(a, b, j, i)
term(29) = term(29) + r2m(vrdav_Rr, a,i,p,q) * t1(b,j) * wm_interm_cc3_103_triplet_pt4(a, b, j, i)
term(30) = term(30) + r2m(vrdav_Rr, a,q,p,i) * t1(b,j) * wm_interm_cc3_104_triplet_pt4(a, b, j, i)
term(31) = term(31) + r2m(vrdav_Rr, a,i,p,q) * t1(b,j) * wm_interm_cc3_104_triplet_pt4(a, b, j, i)
end do 
end do 
end do 
end do 

term(0) = term(0) * 36.0d+0 
term(1) = term(1) * (-18.0d+0) 
term(2) = term(2) * (-18.0d+0) 
term(3) = term(3) * 36.0d+0 
term(4) = term(4) * 48.0d+0 
term(5) = term(5) * (-24.0d+0) 
term(6) = term(6) * (-24.0d+0) 
term(7) = term(7) * 48.0d+0 
term(8) = term(8) * (-24.0d+0) 
term(9) = term(9) * 12.0d+0 
term(10) = term(10) * 12.0d+0 
term(11) = term(11) * (-24.0d+0) 
term(12) = term(12) * 36.0d+0 
term(13) = term(13) * (-18.0d+0) 
term(14) = term(14) * (-18.0d+0) 
term(15) = term(15) * 36.0d+0 
term(16) = term(16) * 48.0d+0 
term(17) = term(17) * (-24.0d+0) 
term(18) = term(18) * (-24.0d+0) 
term(19) = term(19) * (-24.0d+0) 
term(20) = term(20) * 48.0d+0 
term(21) = term(21) * 12.0d+0 
term(22) = term(22) * 12.0d+0 
term(23) = term(23) * (-24.0d+0) 
term(24) = term(24) * (-48.0d+0) 
term(25) = term(25) * 96.0d+0 
term(26) = term(26) * 8.0d+0 
term(27) = term(27) * (-16.0d+0) 
term(28) = term(28) * (-4.0d+0) 
term(29) = term(29) * 8.0d+0 
term(30) = term(30) * 8.0d+0 
term(31) = term(31) * (-16.0d+0) 

do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(32) = term(32) + r3(vrdav_Rr, a,i,b,j,p,q) * s1(b,j) * wm_interm_cc3_23_triplet_pt4(a, i)
term(33) = term(33) + r3(vrdav_Rr, a,i,b,j,p,q) * s1(b,i) * wm_interm_cc3_23_triplet_pt4(a, j)
term(34) = term(34) + s1(a,i) * t3(nocc, nactive, a,b,p,j,i,q) * wm_interm_cc3_37_triplet_pt4(b, j)
term(35) = term(35) + r3(vrdav_Rr, a,i,b,j,p,q) * s1(b,j) * wm_interm_cc3_49_triplet_pt4(a, i)
term(36) = term(36) + r3(vrdav_Rr, a,i,b,j,p,q) * s1(b,i) * wm_interm_cc3_49_triplet_pt4(a, j)
term(37) = term(37) + r3(vrdav_Rr, a,i,b,j,p,q) * s1(b,j) * wm_interm_cc3_50_triplet_pt4(a, i)
term(38) = term(38) + r3(vrdav_Rr, a,i,b,j,p,q) * s1(b,i) * wm_interm_cc3_50_triplet_pt4(a, j)
term(39) = term(39) + s1(a,i) * t3(nocc, nactive, a,b,p,j,i,q) * wm_interm_cc3_61_triplet_pt4(b, j)
term(40) = term(40) + s1(a,i) * t3(nocc, nactive, a,b,p,j,i,q) * wm_interm_cc3_62_triplet_pt4(b, j)
term(41) = term(41) + r3(vrdav_Rl, a,i,b,j,p,q) * t1(b,j) * wm_interm_cc3_73_triplet_pt4(a, i)
term(42) = term(42) + r3(vrdav_Rl, a,i,b,j,p,q) * t1(b,i) * wm_interm_cc3_73_triplet_pt4(a, j)
term(43) = term(43) + r3(vrdav_Rl, a,i,b,j,p,q) * t1(a,j) * wm_interm_cc3_73_triplet_pt4(b, i)
term(44) = term(44) + r3(vrdav_Rl, a,i,b,j,p,q) * t1(a,i) * wm_interm_cc3_73_triplet_pt4(b, j)
term(45) = term(45) + r3(vrdav_Rl, a,i,b,j,p,q) * t1(b,j) * wm_interm_cc3_74_triplet_pt4(a, i)
term(46) = term(46) + r3(vrdav_Rl, a,i,b,j,p,q) * t1(b,j) * wm_interm_cc3_75_triplet_pt4(a, i)
term(47) = term(47) + r3(vrdav_Rl, a,i,b,j,p,q) * t1(b,i) * wm_interm_cc3_74_triplet_pt4(a, j)
term(48) = term(48) + r3(vrdav_Rl, a,i,b,j,p,q) * t1(b,i) * wm_interm_cc3_75_triplet_pt4(a, j)
term(49) = term(49) + r3(vrdav_Rl, a,i,b,j,p,q) * t1(a,j) * wm_interm_cc3_74_triplet_pt4(b, i)
term(50) = term(50) + r3(vrdav_Rl, a,i,b,j,p,q) * t1(a,i) * wm_interm_cc3_74_triplet_pt4(b, j)
term(51) = term(51) + r3(vrdav_Rl, a,i,b,j,p,q) * t1(a,j) * wm_interm_cc3_75_triplet_pt4(b, i)
term(52) = term(52) + r3(vrdav_Rl, a,i,b,j,p,q) * t1(a,i) * wm_interm_cc3_75_triplet_pt4(b, j)
term(53) = term(53) + r2p(vrdav_Rl, p,i,b,j) * s1(a,q) * wm_interm_cc3_98_triplet_pt4(b, a, i, j)
term(54) = term(54) + r2p(vrdav_Rl, p,i,b,j) * s1(a,q) * wm_interm_cc3_101_triplet_pt4(b, a, i, j)
term(55) = term(55) + r2p(vrdav_Rl, p,i,b,j) * s1(a,q) * wm_interm_cc3_100_triplet_pt4(b, a, i, j)
term(56) = term(56) + r2p(vrdav_Rl, p,i,b,j) * s1(a,q) * wm_interm_cc3_99_triplet_pt4(b, a, i, j)
term(57) = term(57) + r2p(vrdav_Rr, p,i,b,j) * t1(a,q) * wm_interm_cc3_102_triplet_pt4(a, b, i, j)
term(58) = term(58) + r2p(vrdav_Rr, p,i,b,j) * t1(a,q) * wm_interm_cc3_102_triplet_pt4(b, a, i, j)
term(59) = term(59) + r2p(vrdav_Rr, p,i,b,j) * t1(a,q) * wm_interm_cc3_103_triplet_pt4(a, b, i, j)
term(60) = term(60) + r2p(vrdav_Rr, p,i,b,j) * t1(a,q) * wm_interm_cc3_103_triplet_pt4(b, a, i, j)
term(61) = term(61) + r2p(vrdav_Rr, p,i,b,j) * t1(a,q) * wm_interm_cc3_105_triplet_pt4(a, b, i, j)
term(62) = term(62) + r2p(vrdav_Rr, p,i,b,j) * t1(a,q) * wm_interm_cc3_104_triplet_pt4(a, b, i, j)
term(63) = term(63) + r2p(vrdav_Rr, p,i,b,j) * t1(a,q) * wm_interm_cc3_104_triplet_pt4(b, a, i, j)
term(64) = term(64) + r2p(vrdav_Rr, p,i,b,j) * t1(a,q) * wm_interm_cc3_105_triplet_pt4(b, a, i, j)
term(65) = term(65) + r2p(vrdav_Rr, p,i,b,j) * t1(a,q) * wm_interm_cc3_106_triplet_pt4(a, b, i, j)
term(66) = term(66) + r2p(vrdav_Rr, p,i,b,j) * t1(a,q) * wm_interm_cc3_106_triplet_pt4(b, a, i, j)
end do 
end do 
end do 
end do 

term(32) = term(32) * (-72.0d+0) 
term(33) = term(33) * 36.0d+0 
term(34) = term(34) * (-11.999999999999998d+0) 
term(35) = term(35) * (-96.0d+0) 
term(36) = term(36) * 48.0d+0 
term(37) = term(37) * 48.0d+0 
term(38) = term(38) * (-24.0d+0) 
term(39) = term(39) * (-15.999999999999998d+0) 
term(40) = term(40) * 7.999999999999999d+0 
term(41) = term(41) * (-36.0d+0) 
term(42) = term(42) * 18.0d+0 
term(43) = term(43) * 18.0d+0 
term(44) = term(44) * (-36.0d+0) 
term(45) = term(45) * (-48.0d+0) 
term(46) = term(46) * 24.0d+0 
term(47) = term(47) * 24.0d+0 
term(48) = term(48) * (-12.0d+0) 
term(49) = term(49) * 24.0d+0 
term(50) = term(50) * (-48.0d+0) 
term(51) = term(51) * (-12.0d+0) 
term(52) = term(52) * 24.0d+0 
term(53) = term(53) * (-18.0d+0) 
term(54) = term(54) * 9.0d+0 
term(55) = term(55) * (-9.0d+0) 
term(56) = term(56) * 9.0d+0 
term(57) = term(57) * (-18.0d+0) 
term(58) = term(58) * 18.0d+0 
term(59) = term(59) * 6.0d+0 
term(60) = term(60) * (-6.0d+0) 
term(61) = term(61) * 6.0d+0 
term(62) = term(62) * (-3.0d+0) 
term(63) = term(63) * 3.0d+0 
term(64) = term(64) * (-6.0d+0) 
term(65) = term(65) * (-3.0d+0) 
term(66) = term(66) * 3.0d+0 

do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(67) = term(67) + s1(a,j) * t3(nocc, nactive, a,b,p,q,j,i) * wm_interm_cc3_37_triplet_pt4(b, i)
term(68) = term(68) + s1(a,i) * t3(nocc, nactive, a,b,p,q,j,i) * wm_interm_cc3_37_triplet_pt4(b, j)
term(69) = term(69) + s1(b,i) * t3(nocc, nactive, a,b,p,q,j,i) * wm_interm_cc3_37_triplet_pt4(a, j)
term(70) = term(70) + s1(b,j) * t3(nocc, nactive, a,b,p,q,j,i) * wm_interm_cc3_37_triplet_pt4(a, i)
term(71) = term(71) + s1(a,i) * t3(nocc, nactive, a,b,p,i,j,q) * wm_interm_cc3_37_triplet_pt4(b, j)
term(72) = term(72) + s1(a,j) * t3(nocc, nactive, a,b,p,q,j,i) * wm_interm_cc3_61_triplet_pt4(b, i)
term(73) = term(73) + s1(a,j) * t3(nocc, nactive, a,b,p,q,j,i) * wm_interm_cc3_62_triplet_pt4(b, i)
term(74) = term(74) + s1(a,i) * t3(nocc, nactive, a,b,p,q,j,i) * wm_interm_cc3_62_triplet_pt4(b, j)
term(75) = term(75) + s1(a,i) * t3(nocc, nactive, a,b,p,q,j,i) * wm_interm_cc3_61_triplet_pt4(b, j)
term(76) = term(76) + s1(b,i) * t3(nocc, nactive, a,b,p,q,j,i) * wm_interm_cc3_61_triplet_pt4(a, j)
term(77) = term(77) + s1(b,i) * t3(nocc, nactive, a,b,p,q,j,i) * wm_interm_cc3_62_triplet_pt4(a, j)
term(78) = term(78) + s1(b,j) * t3(nocc, nactive, a,b,p,q,j,i) * wm_interm_cc3_62_triplet_pt4(a, i)
term(79) = term(79) + s1(b,j) * t3(nocc, nactive, a,b,p,q,j,i) * wm_interm_cc3_61_triplet_pt4(a, i)
term(80) = term(80) + s1(a,i) * t3(nocc, nactive, a,b,p,i,j,q) * wm_interm_cc3_62_triplet_pt4(b, j)
term(81) = term(81) + s1(a,i) * t3(nocc, nactive, a,b,p,i,j,q) * wm_interm_cc3_61_triplet_pt4(b, j)
end do 
end do 
end do 
end do 

term(67) = term(67) * 5.999999999999999d+0 
term(68) = term(68) * (-11.999999999999998d+0) 
term(69) = term(69) * 5.999999999999999d+0 
term(70) = term(70) * (-11.999999999999998d+0) 
term(71) = term(71) * 23.999999999999996d+0 
term(72) = term(72) * 7.999999999999999d+0 
term(73) = term(73) * (-3.9999999999999996d+0) 
term(74) = term(74) * 7.999999999999999d+0 
term(75) = term(75) * (-15.999999999999998d+0) 
term(76) = term(76) * 7.999999999999999d+0 
term(77) = term(77) * (-3.9999999999999996d+0) 
term(78) = term(78) * 7.999999999999999d+0 
term(79) = term(79) * (-15.999999999999998d+0) 
term(80) = term(80) * (-15.999999999999998d+0) 
term(81) = term(81) * 31.999999999999996d+0 

do a = nocc + 1, nactive 
term(82) = term(82) + wm_interm_cc3_36_triplet_pt4(p, a) * wm_interm_cc3_40_triplet_pt4(a, q)
term(83) = term(83) + wm_interm_cc3_36_triplet_pt4(p, a) * wm_interm_cc3_41_triplet_pt4(a, q)
term(84) = term(84) + wm_interm_cc3_36_triplet_pt4(p, a) * wm_interm_cc3_42_triplet_pt4(a, q)
term(85) = term(85) + wm_interm_cc3_36_triplet_pt4(p, a) * wm_interm_cc3_44_triplet_pt4(a, q)
term(86) = term(86) + wm_interm_cc3_36_triplet_pt4(p, a) * wm_interm_cc3_45_triplet_pt4(a, q)
term(87) = term(87) + wm_interm_cc3_36_triplet_pt4(p, a) * wm_interm_cc3_46_triplet_pt4(a, q)
term(88) = term(88) + wm_interm_cc3_36_triplet_pt4(p, a) * wm_interm_cc3_65_triplet_pt4(a, q)
term(89) = term(89) + wm_interm_cc3_36_triplet_pt4(p, a) * wm_interm_cc3_66_triplet_pt4(a, q)
term(90) = term(90) + wm_interm_cc3_36_triplet_pt4(p, a) * wm_interm_cc3_67_triplet_pt4(a, q)
term(91) = term(91) + wm_interm_cc3_36_triplet_pt4(p, a) * wm_interm_cc3_69_triplet_pt4(a, q)
term(92) = term(92) + wm_interm_cc3_28_triplet_pt4(p, a) * wm_interm_cc3_83_triplet_pt4(a, q)
term(93) = term(93) + wm_interm_cc3_28_triplet_pt4(p, a) * wm_interm_cc3_80_triplet_pt4(a, q)
term(94) = term(94) + wm_interm_cc3_28_triplet_pt4(p, a) * wm_interm_cc3_82_triplet_pt4(a, q)
term(95) = term(95) + wm_interm_cc3_28_triplet_pt4(p, a) * wm_interm_cc3_84_triplet_pt4(a, q)
term(96) = term(96) + wm_interm_cc3_28_triplet_pt4(p, a) * wm_interm_cc3_77_triplet_pt4(a, q)
term(97) = term(97) + wm_interm_cc3_28_triplet_pt4(p, a) * wm_interm_cc3_88_triplet_pt4(a, q)
term(98) = term(98) + wm_interm_cc3_28_triplet_pt4(p, a) * wm_interm_cc3_89_triplet_pt4(a, q)
term(99) = term(99) + wm_interm_cc3_28_triplet_pt4(p, a) * wm_interm_cc3_94_triplet_pt4(a, q)
term(100) = term(100) + wm_interm_cc3_28_triplet_pt4(p, a) * wm_interm_cc3_95_triplet_pt4(a, q)
end do 

term(82) = term(82) * 2.0d+0 
term(83) = term(83) * (-4.0d+0) 
term(84) = -term(84) 
term(85) = term(85) * 1.9999999999999998d+0 
term(86) = term(86) * 1.9999999999999998d+0 
term(87) = -term(87) 
term(88) = term(88) * 4.0d+0 
term(89) = term(89) * (-8.0d+0) 
term(90) = term(90) * (-3.9999999999999996d+0) 
term(91) = term(91) * 7.999999999999999d+0 
term(92) = term(92) * (-3.0d+0) 
term(93) = term(93) * 6.0d+0 
term(94) = term(94) * (-18.0d+0) 
term(95) = term(95) * 6.0d+0 
term(96) = term(96) * (-3.0d+0) 
term(97) = term(97) * (-12.0d+0) 
term(98) = term(98) * 24.0d+0 
term(99) = term(99) * 12.0d+0 
term(100) = term(100) * (-24.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(101) = term(101) + r1(vrdav_Rr, p,i) * s1(a,j) * wm_interm_cc3_38_triplet_pt4(a, i, j, q)
term(102) = term(102) + r1(vrdav_Rr, p,i) * s1(a,j) * wm_interm_cc3_39_triplet_pt4(a, i, j, q)
term(103) = term(103) + r1(vrdav_Rr, p,i) * s1(a,j) * wm_interm_cc3_43_triplet_pt4(a, i, j, q)
term(104) = term(104) + r1(vrdav_Rr, p,i) * s1(a,j) * wm_interm_cc3_43_triplet_pt4(a, i, q, j)
term(105) = term(105) + r1(vrdav_Rr, p,i) * s1(a,j) * wm_interm_cc3_38_triplet_pt4(a, i, q, j)
term(106) = term(106) + r1(vrdav_Rr, p,i) * s1(a,j) * wm_interm_cc3_39_triplet_pt4(a, i, q, j)
term(107) = term(107) + r1(vrdav_Rr, p,i) * s1(a,j) * wm_interm_cc3_63_triplet_pt4(a, i, j, q)
term(108) = term(108) + r1(vrdav_Rr, p,i) * s1(a,j) * wm_interm_cc3_64_triplet_pt4(a, i, j, q)
term(109) = term(109) + r1(vrdav_Rr, p,i) * s1(a,j) * wm_interm_cc3_68_triplet_pt4(a, i, j, q)
term(110) = term(110) + r1(vrdav_Rr, p,i) * s1(a,j) * wm_interm_cc3_68_triplet_pt4(a, i, q, j)
term(111) = term(111) + r1(vrdav_Rr, p,i) * s1(a,j) * wm_interm_cc3_70_triplet_pt4(a, i, j, q)
term(112) = term(112) + r1(vrdav_Rr, p,i) * s1(a,j) * wm_interm_cc3_63_triplet_pt4(a, i, q, j)
term(113) = term(113) + r1(vrdav_Rr, p,i) * s1(a,j) * wm_interm_cc3_64_triplet_pt4(a, i, q, j)
term(114) = term(114) + r1(vrdav_Rr, p,i) * s1(a,j) * wm_interm_cc3_70_triplet_pt4(a, i, q, j)
end do 
end do 
end do 

term(101) = term(101) * (-2.9999999999999996d+0) 
term(102) = term(102) * 2.9999999999999996d+0 
term(103) = term(103) * (-2.9999999999999996d+0) 
term(104) = term(104) * 2.9999999999999996d+0 
term(105) = term(105) * 5.999999999999999d+0 
term(106) = term(106) * (-5.999999999999999d+0) 
term(107) = term(107) * (-3.9999999999999996d+0) 
term(108) = term(108) * 7.999999999999999d+0 
term(109) = term(109) * 3.9999999999999996d+0 
term(110) = term(110) * (-1.9999999999999998d+0) 
term(111) = term(111) * (-1.9999999999999998d+0) 
term(112) = term(112) * 7.999999999999999d+0 
term(113) = term(113) * (-15.999999999999998d+0) 
term(114) = term(114) * 3.9999999999999996d+0 

do j = 1, nocc 
do i = 1, nocc 
term(115) = term(115) + wm_interm_cc3_7_triplet_pt4(i, j) * wm_interm_cc3_8_triplet_pt4(p, i, q, j)
term(116) = term(116) + wm_interm_cc3_7_triplet_pt4(i, j) * wm_interm_cc3_8_triplet_pt4(p, i, j, q)
term(117) = term(117) + wm_interm_cc3_7_triplet_pt4(i, j) * wm_interm_cc3_9_triplet_pt4(p, i, j, q)
term(118) = term(118) + wm_interm_cc3_10_triplet_pt4(p, i, j, q) * wm_interm_cc3_7_triplet_pt4(i, j)
term(119) = term(119) + wm_interm_cc3_10_triplet_pt4(p, i, q, j) * wm_interm_cc3_7_triplet_pt4(i, j)
term(120) = term(120) + wm_interm_cc3_7_triplet_pt4(i, j) * wm_interm_cc3_9_triplet_pt4(p, i, q, j)
term(121) = term(121) + wm_interm_cc3_17_triplet_pt4(p, i, q, j) * wm_interm_cc3_7_triplet_pt4(i, j)
term(122) = term(122) + wm_interm_cc3_18_triplet_pt4(p, i, q, j) * wm_interm_cc3_7_triplet_pt4(i, j)
term(123) = term(123) + wm_interm_cc3_18_triplet_pt4(p, i, j, q) * wm_interm_cc3_7_triplet_pt4(i, j)
term(124) = term(124) + wm_interm_cc3_17_triplet_pt4(p, i, j, q) * wm_interm_cc3_7_triplet_pt4(i, j)
term(125) = term(125) + wm_interm_cc3_19_triplet_pt4(p, i, j, q) * wm_interm_cc3_7_triplet_pt4(i, j)
term(126) = term(126) + wm_interm_cc3_20_triplet_pt4(p, i, j, q) * wm_interm_cc3_7_triplet_pt4(i, j)
term(127) = term(127) + wm_interm_cc3_21_triplet_pt4(p, i, j, q) * wm_interm_cc3_7_triplet_pt4(i, j)
term(128) = term(128) + wm_interm_cc3_22_triplet_pt4(p, i, j, q) * wm_interm_cc3_7_triplet_pt4(i, j)
term(129) = term(129) + wm_interm_cc3_21_triplet_pt4(p, i, q, j) * wm_interm_cc3_7_triplet_pt4(i, j)
term(130) = term(130) + wm_interm_cc3_22_triplet_pt4(p, i, q, j) * wm_interm_cc3_7_triplet_pt4(i, j)
term(131) = term(131) + wm_interm_cc3_19_triplet_pt4(p, i, q, j) * wm_interm_cc3_7_triplet_pt4(i, j)
term(132) = term(132) + wm_interm_cc3_20_triplet_pt4(p, i, q, j) * wm_interm_cc3_7_triplet_pt4(i, j)
term(133) = term(133) + wm_interm_cc3_24_triplet_pt4(p, i, j, q) * wm_interm_cc3_25_triplet_pt4(j, i)
term(134) = term(134) + wm_interm_cc3_24_triplet_pt4(p, i, q, j) * wm_interm_cc3_25_triplet_pt4(j, i)
term(135) = term(135) + wm_interm_cc3_43_triplet_pt4(p, i, q, j) * wm_interm_cc3_47_triplet_pt4(i, j)
term(136) = term(136) + wm_interm_cc3_43_triplet_pt4(p, i, j, q) * wm_interm_cc3_47_triplet_pt4(i, j)
term(137) = term(137) + wm_interm_cc3_38_triplet_pt4(p, i, q, j) * wm_interm_cc3_47_triplet_pt4(i, j)
term(138) = term(138) + wm_interm_cc3_39_triplet_pt4(p, i, q, j) * wm_interm_cc3_47_triplet_pt4(i, j)
term(139) = term(139) + wm_interm_cc3_38_triplet_pt4(p, i, j, q) * wm_interm_cc3_47_triplet_pt4(i, j)
term(140) = term(140) + wm_interm_cc3_39_triplet_pt4(p, i, j, q) * wm_interm_cc3_47_triplet_pt4(i, j)
term(141) = term(141) + wm_interm_cc3_47_triplet_pt4(i, j) * wm_interm_cc3_70_triplet_pt4(p, i, q, j)
term(142) = term(142) + wm_interm_cc3_47_triplet_pt4(i, j) * wm_interm_cc3_68_triplet_pt4(p, i, q, j)
term(143) = term(143) + wm_interm_cc3_47_triplet_pt4(i, j) * wm_interm_cc3_68_triplet_pt4(p, i, j, q)
term(144) = term(144) + wm_interm_cc3_47_triplet_pt4(i, j) * wm_interm_cc3_70_triplet_pt4(p, i, j, q)
term(145) = term(145) + wm_interm_cc3_47_triplet_pt4(i, j) * wm_interm_cc3_63_triplet_pt4(p, i, q, j)
term(146) = term(146) + wm_interm_cc3_47_triplet_pt4(i, j) * wm_interm_cc3_64_triplet_pt4(p, i, q, j)
term(147) = term(147) + wm_interm_cc3_47_triplet_pt4(i, j) * wm_interm_cc3_63_triplet_pt4(p, i, j, q)
term(148) = term(148) + wm_interm_cc3_47_triplet_pt4(i, j) * wm_interm_cc3_64_triplet_pt4(p, i, j, q)
end do 
end do 

term(115) = term(115) * (-3.0d+0) 
term(116) = term(116) * 3.0d+0 
term(117) = term(117) * 6.0d+0 
term(118) = term(118) * (-6.0d+0) 
term(119) = term(119) * 3.0d+0 
term(120) = term(120) * (-3.0d+0) 
term(121) = term(121) * (-2.0d+0) 
term(122) = term(122) * 4.0d+0 
term(123) = term(123) * (-2.0d+0) 
term(124) = term(124) * 4.0d+0 
term(125) = term(125) * 4.0d+0 
term(126) = term(126) * (-8.0d+0) 
term(127) = term(127) * 4.0d+0 
term(128) = term(128) * (-8.0d+0) 
term(129) = term(129) * (-2.0d+0) 
term(130) = term(130) * 4.0d+0 
term(131) = term(131) * (-2.0d+0) 
term(132) = term(132) * 4.0d+0 
term(133) = term(133) * (-9.0d+0) 
term(134) = term(134) * 9.0d+0 
term(135) = term(135) * (-2.9999999999999996d+0) 
term(136) = term(136) * 2.9999999999999996d+0 
term(137) = term(137) * (-2.9999999999999996d+0) 
term(138) = term(138) * 2.9999999999999996d+0 
term(139) = term(139) * 5.999999999999999d+0 
term(140) = term(140) * (-5.999999999999999d+0) 
term(141) = term(141) * (-1.9999999999999998d+0) 
term(142) = term(142) * 3.9999999999999996d+0 
term(143) = term(143) * (-1.9999999999999998d+0) 
term(144) = term(144) * 3.9999999999999996d+0 
term(145) = term(145) * (-3.9999999999999996d+0) 
term(146) = term(146) * 7.999999999999999d+0 
term(147) = term(147) * 7.999999999999999d+0 
term(148) = term(148) * (-15.999999999999998d+0) 

do j = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(149) = term(149) + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,p,q,i,j) * wm_interm_cc3_0_triplet_pt4(a, c)
term(150) = term(150) + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,p,j,i,q) * wm_interm_cc3_0_triplet_pt4(a, c)
term(151) = term(151) + r2m(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,p,q,i,j) * wm_interm_cc3_0_triplet_pt4(a, c)
term(152) = term(152) + r2m(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,p,j,i,q) * wm_interm_cc3_0_triplet_pt4(a, c)
term(153) = term(153) + r2p(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, b,c,p,j,i,q) * wm_interm_cc3_36_triplet_pt4(a, c)
term(154) = term(154) + r2m(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,i,c,j,p,q) * wm_interm_cc3_28_triplet_pt4(c, b)
term(155) = term(155) + r2m(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,c,p,j,i,q) * wm_interm_cc3_36_triplet_pt4(b, c)
term(156) = term(156) + r2m(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, b,c,p,j,i,q) * wm_interm_cc3_36_triplet_pt4(a, c)
term(157) = term(157) + r3(vrdav_Rl, a,i,b,j,p,q) * r2m(vrdav_Rr, a,i,c,j) * wm_interm_cc3_28_triplet_pt4(c, b)
end do 
end do 
end do 
end do 
end do 

term(149) = term(149) * 3.0d+0 
term(150) = term(150) * (-6.0d+0) 
term(151) = term(151) * 7.999999999999999d+0 
term(152) = term(152) * (-15.999999999999998d+0) 
term(153) = term(153) * (-5.999999999999999d+0) 
term(154) = term(154) * (-24.0d+0) 
term(155) = term(155) * 3.9999999999999996d+0 
term(156) = term(156) * (-7.999999999999999d+0) 
term(157) = term(157) * (-24.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(158) = term(158) + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,p,j,q,i) * wm_interm_cc3_0_triplet_pt4(a, c)
term(159) = term(159) + r2m(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,p,j,q,i) * wm_interm_cc3_0_triplet_pt4(a, c)
term(160) = term(160) + r2p(vrdav_Rl, b,i,c,j) * r3(vrdav_Rr, a,q,c,j,p,i) * wm_interm_cc3_28_triplet_pt4(a, b)
term(161) = term(161) + r2p(vrdav_Rl, b,j,c,i) * r3(vrdav_Rr, a,q,c,j,p,i) * wm_interm_cc3_28_triplet_pt4(a, b)
term(162) = term(162) + r2m(vrdav_Rl, b,i,c,j) * r3(vrdav_Rr, a,q,b,j,p,i) * wm_interm_cc3_28_triplet_pt4(a, c)
term(163) = term(163) + r2m(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,q,c,j,p,i) * wm_interm_cc3_28_triplet_pt4(c, b)
term(164) = term(164) + r2m(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,q,c,j,p,i) * wm_interm_cc3_28_triplet_pt4(c, b)
term(165) = term(165) + r2m(vrdav_Rl, b,j,c,i) * r3(vrdav_Rr, a,q,b,j,p,i) * wm_interm_cc3_28_triplet_pt4(a, c)
term(166) = term(166) + r2m(vrdav_Rl, b,i,c,j) * r3(vrdav_Rr, a,q,c,j,p,i) * wm_interm_cc3_28_triplet_pt4(a, b)
term(167) = term(167) + r2m(vrdav_Rl, b,j,c,i) * r3(vrdav_Rr, a,q,c,j,p,i) * wm_interm_cc3_28_triplet_pt4(a, b)
term(168) = term(168) + r3(vrdav_Rl, a,q,b,j,p,i) * r2m(vrdav_Rr, a,j,c,i) * wm_interm_cc3_28_triplet_pt4(c, b)
term(169) = term(169) + r3(vrdav_Rl, a,q,b,j,p,i) * r2m(vrdav_Rr, a,i,c,j) * wm_interm_cc3_28_triplet_pt4(c, b)
term(170) = term(170) + r3(vrdav_Rl, a,q,b,j,p,i) * r2m(vrdav_Rr, b,i,c,j) * wm_interm_cc3_28_triplet_pt4(c, a)
term(171) = term(171) + r3(vrdav_Rl, a,q,b,j,p,i) * r2m(vrdav_Rr, b,j,c,i) * wm_interm_cc3_28_triplet_pt4(c, a)
end do 
end do 
end do 
end do 
end do 

term(158) = term(158) * 3.0d+0 
term(159) = term(159) * 7.999999999999999d+0 
term(160) = term(160) * 9.0d+0 
term(161) = term(161) * (-9.0d+0) 
term(162) = term(162) * (-6.0d+0) 
term(163) = term(163) * 24.0d+0 
term(164) = term(164) * (-12.0d+0) 
term(165) = term(165) * 12.0d+0 
term(166) = term(166) * 12.0d+0 
term(167) = term(167) * (-6.0d+0) 
term(168) = term(168) * (-12.0d+0) 
term(169) = term(169) * 24.0d+0 
term(170) = term(170) * (-12.0d+0) 
term(171) = term(171) * 24.0d+0 

do j = 1, nocc 
do i = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(172) = term(172) + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,p,i,q,j) * wm_interm_cc3_0_triplet_pt4(a, c)
term(173) = term(173) + r2m(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,p,i,q,j) * wm_interm_cc3_0_triplet_pt4(a, c)
term(174) = term(174) + r2p(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, b,j,c,i,p,q) * wm_interm_cc3_28_triplet_pt4(c, a)
term(175) = term(175) + r2m(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,j,c,i,p,q) * wm_interm_cc3_28_triplet_pt4(c, b)
term(176) = term(176) + r2m(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, b,j,c,i,p,q) * wm_interm_cc3_28_triplet_pt4(c, a)
term(177) = term(177) + r3(vrdav_Rl, a,i,b,j,p,q) * r2m(vrdav_Rr, a,j,c,i) * wm_interm_cc3_28_triplet_pt4(c, b)
term(178) = term(178) + r3(vrdav_Rl, a,i,b,j,p,q) * r2m(vrdav_Rr, b,j,c,i) * wm_interm_cc3_28_triplet_pt4(c, a)
end do 
end do 
end do 
end do 
end do 

term(172) = term(172) * (-3.0d+0) 
term(173) = term(173) * (-3.9999999999999996d+0) 
term(174) = term(174) * (-18.0d+0) 
term(175) = term(175) * 12.0d+0 
term(176) = term(176) * (-24.0d+0) 
term(177) = term(177) * 12.0d+0 
term(178) = term(178) * (-24.0d+0) 

do i = 1, nocc 
do c = nocc + 1, nactive 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(179) = term(179) + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,p,i,j,q) * wm_interm_cc3_0_triplet_pt4(a, c)
term(180) = term(180) + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,p,q,j,i) * wm_interm_cc3_0_triplet_pt4(a, c)
term(181) = term(181) + r2m(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,p,q,j,i) * wm_interm_cc3_0_triplet_pt4(a, c)
term(182) = term(182) + r2m(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,p,i,j,q) * wm_interm_cc3_0_triplet_pt4(a, c)
term(183) = term(183) + r2p(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, b,c,p,i,j,q) * wm_interm_cc3_36_triplet_pt4(a, c)
term(184) = term(184) + r2m(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_cc3_36_triplet_pt4(b, c)
term(185) = term(185) + r2m(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,c,p,i,j,q) * wm_interm_cc3_36_triplet_pt4(b, c)
term(186) = term(186) + r2m(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, b,c,p,i,j,q) * wm_interm_cc3_36_triplet_pt4(a, c)
end do 
end do 
end do 
end do 
end do 

term(179) = term(179) * 6.0d+0 
term(180) = term(180) * (-3.0d+0) 
term(181) = term(181) * (-3.9999999999999996d+0) 
term(182) = term(182) * 7.999999999999999d+0 
term(183) = term(183) * 5.999999999999999d+0 
term(184) = term(184) * 7.999999999999999d+0 
term(185) = term(185) * (-7.999999999999999d+0) 
term(186) = term(186) * 3.9999999999999996d+0 

do i = 1, nocc 
do j = 1, nocc 
term(187) = term(187) + wm_interm_cc3_25_triplet_pt4(i, j) * wm_interm_cc3_26_triplet_pt4(p, j, q, i)
term(188) = term(188) + wm_interm_cc3_25_triplet_pt4(i, j) * wm_interm_cc3_27_triplet_pt4(p, j, q, i)
term(189) = term(189) + wm_interm_cc3_25_triplet_pt4(i, j) * wm_interm_cc3_26_triplet_pt4(p, j, i, q)
term(190) = term(190) + wm_interm_cc3_25_triplet_pt4(i, j) * wm_interm_cc3_27_triplet_pt4(p, j, i, q)
term(191) = term(191) + wm_interm_cc3_25_triplet_pt4(i, j) * wm_interm_cc3_51_triplet_pt4(p, j, i, q)
term(192) = term(192) + wm_interm_cc3_25_triplet_pt4(i, j) * wm_interm_cc3_51_triplet_pt4(p, j, q, i)
term(193) = term(193) + wm_interm_cc3_25_triplet_pt4(i, j) * wm_interm_cc3_52_triplet_pt4(p, j, q, i)
term(194) = term(194) + wm_interm_cc3_25_triplet_pt4(i, j) * wm_interm_cc3_53_triplet_pt4(p, j, q, i)
term(195) = term(195) + wm_interm_cc3_25_triplet_pt4(i, j) * wm_interm_cc3_52_triplet_pt4(p, j, i, q)
term(196) = term(196) + wm_interm_cc3_25_triplet_pt4(i, j) * wm_interm_cc3_53_triplet_pt4(p, j, i, q)
term(197) = term(197) + wm_interm_cc3_25_triplet_pt4(i, j) * wm_interm_cc3_54_triplet_pt4(p, j, q, i)
term(198) = term(198) + wm_interm_cc3_25_triplet_pt4(i, j) * wm_interm_cc3_54_triplet_pt4(p, j, i, q)
term(199) = term(199) + wm_interm_cc3_25_triplet_pt4(i, j) * wm_interm_cc3_76_triplet_pt4(p, q, j, i)
term(200) = term(200) + wm_interm_cc3_25_triplet_pt4(i, j) * wm_interm_cc3_78_triplet_pt4(p, q, j, i)
term(201) = term(201) + wm_interm_cc3_25_triplet_pt4(i, j) * wm_interm_cc3_79_triplet_pt4(p, q, j, i)
term(202) = term(202) + wm_interm_cc3_25_triplet_pt4(i, j) * wm_interm_cc3_81_triplet_pt4(p, q, j, i)
term(203) = term(203) + wm_interm_cc3_25_triplet_pt4(i, j) * wm_interm_cc3_76_triplet_pt4(p, j, q, i)
term(204) = term(204) + wm_interm_cc3_25_triplet_pt4(i, j) * wm_interm_cc3_85_triplet_pt4(p, q, j, i)
term(205) = term(205) + wm_interm_cc3_25_triplet_pt4(i, j) * wm_interm_cc3_78_triplet_pt4(p, j, q, i)
term(206) = term(206) + wm_interm_cc3_25_triplet_pt4(i, j) * wm_interm_cc3_81_triplet_pt4(p, j, q, i)
term(207) = term(207) + wm_interm_cc3_25_triplet_pt4(i, j) * wm_interm_cc3_85_triplet_pt4(p, j, q, i)
term(208) = term(208) + wm_interm_cc3_25_triplet_pt4(i, j) * wm_interm_cc3_79_triplet_pt4(p, j, q, i)
term(209) = term(209) + wm_interm_cc3_25_triplet_pt4(i, j) * wm_interm_cc3_86_triplet_pt4(p, q, j, i)
term(210) = term(210) + wm_interm_cc3_25_triplet_pt4(i, j) * wm_interm_cc3_87_triplet_pt4(p, q, j, i)
term(211) = term(211) + wm_interm_cc3_25_triplet_pt4(i, j) * wm_interm_cc3_90_triplet_pt4(p, q, j, i)
term(212) = term(212) + wm_interm_cc3_25_triplet_pt4(i, j) * wm_interm_cc3_91_triplet_pt4(p, q, j, i)
term(213) = term(213) + wm_interm_cc3_25_triplet_pt4(i, j) * wm_interm_cc3_92_triplet_pt4(p, q, j, i)
term(214) = term(214) + wm_interm_cc3_25_triplet_pt4(i, j) * wm_interm_cc3_93_triplet_pt4(p, q, j, i)
term(215) = term(215) + wm_interm_cc3_25_triplet_pt4(i, j) * wm_interm_cc3_87_triplet_pt4(p, j, q, i)
term(216) = term(216) + wm_interm_cc3_25_triplet_pt4(i, j) * wm_interm_cc3_86_triplet_pt4(p, j, q, i)
term(217) = term(217) + wm_interm_cc3_25_triplet_pt4(i, j) * wm_interm_cc3_90_triplet_pt4(p, j, q, i)
term(218) = term(218) + wm_interm_cc3_25_triplet_pt4(i, j) * wm_interm_cc3_91_triplet_pt4(p, j, q, i)
term(219) = term(219) + wm_interm_cc3_25_triplet_pt4(i, j) * wm_interm_cc3_92_triplet_pt4(p, j, q, i)
term(220) = term(220) + wm_interm_cc3_25_triplet_pt4(i, j) * wm_interm_cc3_93_triplet_pt4(p, j, q, i)
end do 
end do 

term(187) = term(187) * (-9.0d+0) 
term(188) = term(188) * 9.0d+0 
term(189) = term(189) * 18.0d+0 
term(190) = term(190) * (-18.0d+0) 
term(191) = term(191) * (-6.0d+0) 
term(192) = term(192) * 12.0d+0 
term(193) = term(193) * (-12.0d+0) 
term(194) = term(194) * 24.0d+0 
term(195) = term(195) * 24.0d+0 
term(196) = term(196) * (-48.0d+0) 
term(197) = term(197) * (-6.0d+0) 
term(198) = term(198) * 12.0d+0 
term(199) = term(199) * 9.0d+0 
term(200) = term(200) * (-12.0d+0) 
term(201) = term(201) * (-6.0d+0) 
term(202) = term(202) * 3.0d+0 
term(203) = term(203) * (-9.0d+0) 
term(204) = term(204) * 6.0d+0 
term(205) = term(205) * 6.0d+0 
term(206) = term(206) * (-6.0d+0) 
term(207) = term(207) * (-3.0d+0) 
term(208) = term(208) * 12.0d+0 
term(209) = term(209) * (-6.0d+0) 
term(210) = term(210) * 12.0d+0 
term(211) = term(211) * 12.0d+0 
term(212) = term(212) * (-24.0d+0) 
term(213) = term(213) * (-6.0d+0) 
term(214) = term(214) * 12.0d+0 
term(215) = term(215) * (-6.0d+0) 
term(216) = term(216) * 12.0d+0 
term(217) = term(217) * (-6.0d+0) 
term(218) = term(218) * 12.0d+0 
term(219) = term(219) * 12.0d+0 
term(220) = term(220) * (-24.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do c = nocc + 1, nactive 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(221) = term(221) + r2p(vrdav_Rl, b,i,a,j) * r3(vrdav_Rr, a,q,c,j,p,i) * wm_interm_cc3_28_triplet_pt4(c, b)
term(222) = term(222) + r2p(vrdav_Rl, b,j,a,i) * r3(vrdav_Rr, a,q,c,j,p,i) * wm_interm_cc3_28_triplet_pt4(c, b)
end do 
end do 
end do 
end do 
end do 

term(221) = term(221) * (-9.0d+0) 
term(222) = term(222) * 9.0d+0 

do j = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(223) = term(223) + r2p(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, b,i,c,j,p,q) * wm_interm_cc3_28_triplet_pt4(c, a)
term(224) = term(224) + r2m(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, b,i,c,j,p,q) * wm_interm_cc3_28_triplet_pt4(c, a)
term(225) = term(225) + r3(vrdav_Rl, a,i,b,j,p,q) * r2m(vrdav_Rr, b,i,c,j) * wm_interm_cc3_28_triplet_pt4(c, a)
end do 
end do 
end do 
end do 
end do 

term(223) = term(223) * 18.0d+0 
term(224) = term(224) * 12.0d+0 
term(225) = term(225) * 12.0d+0 

do i = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(226) = term(226) + r3(vrdav_Rr, a,j,b,k,p,i) * s1(b,k) * wm_interm_cc3_35_triplet_pt4(a, j, i, q)
term(227) = term(227) + r3(vrdav_Rr, a,j,b,k,p,i) * s1(b,k) * wm_interm_cc3_35_triplet_pt4(a, i, j, q)
term(228) = term(228) + r3(vrdav_Rr, a,j,b,k,p,i) * s1(b,j) * wm_interm_cc3_35_triplet_pt4(a, k, i, q)
term(229) = term(229) + r3(vrdav_Rr, a,j,b,k,p,i) * s1(b,j) * wm_interm_cc3_35_triplet_pt4(a, i, k, q)
term(230) = term(230) + r3(vrdav_Rr, a,j,b,k,p,i) * s1(b,i) * wm_interm_cc3_35_triplet_pt4(a, k, j, q)
term(231) = term(231) + r3(vrdav_Rr, a,j,b,k,p,i) * s1(b,i) * wm_interm_cc3_35_triplet_pt4(a, j, k, q)
term(232) = term(232) + r3(vrdav_Rr, a,j,b,k,p,i) * s1(b,k) * wm_interm_cc3_59_triplet_pt4(a, j, i, q)
term(233) = term(233) + r3(vrdav_Rr, a,j,b,k,p,i) * s1(b,k) * wm_interm_cc3_59_triplet_pt4(a, i, j, q)
term(234) = term(234) + r3(vrdav_Rr, a,j,b,k,p,i) * s1(b,k) * wm_interm_cc3_60_triplet_pt4(a, i, j, q)
term(235) = term(235) + r3(vrdav_Rr, a,j,b,k,p,i) * s1(b,k) * wm_interm_cc3_60_triplet_pt4(a, j, i, q)
term(236) = term(236) + r3(vrdav_Rr, a,j,b,k,p,i) * s1(b,j) * wm_interm_cc3_59_triplet_pt4(a, k, i, q)
term(237) = term(237) + r3(vrdav_Rr, a,j,b,k,p,i) * s1(b,j) * wm_interm_cc3_59_triplet_pt4(a, i, k, q)
term(238) = term(238) + r3(vrdav_Rr, a,j,b,k,p,i) * s1(b,j) * wm_interm_cc3_60_triplet_pt4(a, i, k, q)
term(239) = term(239) + r3(vrdav_Rr, a,j,b,k,p,i) * s1(b,j) * wm_interm_cc3_60_triplet_pt4(a, k, i, q)
term(240) = term(240) + r3(vrdav_Rr, a,j,b,k,p,i) * s1(b,i) * wm_interm_cc3_60_triplet_pt4(a, k, j, q)
term(241) = term(241) + r3(vrdav_Rr, a,j,b,k,p,i) * s1(b,i) * wm_interm_cc3_60_triplet_pt4(a, j, k, q)
term(242) = term(242) + r3(vrdav_Rr, a,j,b,k,p,i) * s1(b,i) * wm_interm_cc3_59_triplet_pt4(a, j, k, q)
term(243) = term(243) + r3(vrdav_Rr, a,j,b,k,p,i) * s1(b,i) * wm_interm_cc3_59_triplet_pt4(a, k, j, q)
end do 
end do 
end do 
end do 
end do 

term(226) = term(226) * 18.0d+0 
term(227) = term(227) * (-18.0d+0) 
term(228) = term(228) * (-9.0d+0) 
term(229) = term(229) * 9.0d+0 
term(230) = term(230) * 9.0d+0 
term(231) = term(231) * (-9.0d+0) 
term(232) = term(232) * (-12.0d+0) 
term(233) = term(233) * 24.0d+0 
term(234) = term(234) * (-12.0d+0) 
term(235) = term(235) * 24.0d+0 
term(236) = term(236) * 6.0d+0 
term(237) = term(237) * (-12.0d+0) 
term(238) = term(238) * 6.0d+0 
term(239) = term(239) * (-12.0d+0) 
term(240) = term(240) * 6.0d+0 
term(241) = term(241) * (-12.0d+0) 
term(242) = term(242) * 6.0d+0 
term(243) = term(243) * (-12.0d+0) 

do i = 1, nocc 
do c = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(244) = term(244) + r2p(vrdav_Rl, b,i,c,j) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_cc3_36_triplet_pt4(b, a)
term(245) = term(245) + r2p(vrdav_Rl, b,j,c,i) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_cc3_36_triplet_pt4(b, a)
term(246) = term(246) + r2p(vrdav_Rl, b,i,a,j) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_cc3_36_triplet_pt4(b, c)
term(247) = term(247) + r2m(vrdav_Rl, b,i,c,j) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_cc3_36_triplet_pt4(b, a)
term(248) = term(248) + r2m(vrdav_Rl, b,j,c,i) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_cc3_36_triplet_pt4(b, a)
end do 
end do 
end do 
end do 
end do 

term(244) = term(244) * 2.9999999999999996d+0 
term(245) = term(245) * (-2.9999999999999996d+0) 
term(246) = term(246) * (-2.9999999999999996d+0) 
term(247) = term(247) * 3.9999999999999996d+0 
term(248) = term(248) * (-1.9999999999999998d+0) 

do i = 1, nocc 
do c = nocc + 1, nactive 
do a = nocc + 1, nactive 
do j = 1, nocc 
do b = nocc + 1, nactive 
term(249) = term(249) + r2p(vrdav_Rl, b,j,a,i) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_cc3_36_triplet_pt4(b, c)
end do 
end do 
end do 
end do 
end do 

term(249) = term(249) * 2.9999999999999996d+0 

do i = 1, nocc 
do a = nocc + 1, nactive 
do j = 1, nocc 
do b = nocc + 1, nactive 
term(250) = term(250) + r2p(vrdav_Rl, p,q,a,i) * s1(b,j) * wm_interm_cc3_98_triplet_pt4(a, b, j, i)
term(251) = term(251) + r2p(vrdav_Rr, p,q,a,i) * t1(b,j) * wm_interm_cc3_102_triplet_pt4(a, b, j, i)
term(252) = term(252) + r2p(vrdav_Rr, p,q,a,i) * t1(b,j) * wm_interm_cc3_102_triplet_pt4(b, a, j, i)
term(253) = term(253) + r2p(vrdav_Rr, p,q,a,i) * t1(b,j) * wm_interm_cc3_103_triplet_pt4(a, b, j, i)
term(254) = term(254) + r2p(vrdav_Rr, p,q,a,i) * t1(b,j) * wm_interm_cc3_103_triplet_pt4(b, a, j, i)
term(255) = term(255) + r2p(vrdav_Rr, p,q,a,i) * t1(b,j) * wm_interm_cc3_104_triplet_pt4(a, b, j, i)
term(256) = term(256) + r2p(vrdav_Rr, p,q,a,i) * t1(b,j) * wm_interm_cc3_105_triplet_pt4(b, a, j, i)
term(257) = term(257) + r2p(vrdav_Rr, p,q,a,i) * t1(b,j) * wm_interm_cc3_104_triplet_pt4(b, a, j, i)
end do 
end do 
end do 
end do 

term(250) = term(250) * 72.0d+0 
term(251) = term(251) * (-12.0d+0) 
term(252) = term(252) * 48.0d+0 
term(253) = term(253) * 6.0d+0 
term(254) = term(254) * (-24.0d+0) 
term(255) = term(255) * (-12.0d+0) 
term(256) = term(256) * (-12.0d+0) 
term(257) = term(257) * 6.0d+0 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(258) = term(258) + r2p(vrdav_Rl, p,i,b,j) * s1(a,q) * wm_interm_cc3_98_triplet_pt4(b, a, j, i)
term(259) = term(259) + r2p(vrdav_Rl, p,i,b,j) * s1(a,q) * wm_interm_cc3_99_triplet_pt4(b, a, j, i)
term(260) = term(260) + r2m(vrdav_Rl, a,q,p,i) * s1(b,j) * wm_interm_cc3_98_triplet_pt4(a, b, i, j)
term(261) = term(261) + r2m(vrdav_Rl, a,i,p,q) * s1(b,j) * wm_interm_cc3_98_triplet_pt4(a, b, i, j)
term(262) = term(262) + r2m(vrdav_Rl, a,q,p,i) * s1(b,j) * wm_interm_cc3_99_triplet_pt4(a, b, i, j)
term(263) = term(263) + r2m(vrdav_Rl, a,i,p,q) * s1(b,j) * wm_interm_cc3_99_triplet_pt4(a, b, i, j)
term(264) = term(264) + r2m(vrdav_Rl, a,q,p,i) * s1(b,j) * wm_interm_cc3_100_triplet_pt4(a, b, i, j)
term(265) = term(265) + r2m(vrdav_Rl, a,i,p,q) * s1(b,j) * wm_interm_cc3_100_triplet_pt4(a, b, i, j)
term(266) = term(266) + r2m(vrdav_Rr, a,q,p,i) * t1(b,j) * wm_interm_cc3_102_triplet_pt4(a, b, i, j)
term(267) = term(267) + r2m(vrdav_Rr, a,i,p,q) * t1(b,j) * wm_interm_cc3_102_triplet_pt4(a, b, i, j)
term(268) = term(268) + r2m(vrdav_Rr, a,q,p,i) * t1(b,j) * wm_interm_cc3_103_triplet_pt4(a, b, i, j)
term(269) = term(269) + r2m(vrdav_Rr, a,i,p,q) * t1(b,j) * wm_interm_cc3_103_triplet_pt4(a, b, i, j)
term(270) = term(270) + r2m(vrdav_Rr, a,q,p,i) * t1(b,j) * wm_interm_cc3_104_triplet_pt4(a, b, i, j)
term(271) = term(271) + r2m(vrdav_Rr, a,i,p,q) * t1(b,j) * wm_interm_cc3_104_triplet_pt4(a, b, i, j)
term(272) = term(272) + r2m(vrdav_Rr, b,i,p,j) * t1(a,q) * wm_interm_cc3_102_triplet_pt4(a, b, i, j)
term(273) = term(273) + r2m(vrdav_Rr, b,i,p,j) * t1(a,q) * wm_interm_cc3_102_triplet_pt4(a, b, j, i)
term(274) = term(274) + r2m(vrdav_Rr, b,i,p,j) * t1(a,q) * wm_interm_cc3_103_triplet_pt4(a, b, i, j)
term(275) = term(275) + r2m(vrdav_Rr, b,i,p,j) * t1(a,q) * wm_interm_cc3_103_triplet_pt4(a, b, j, i)
term(276) = term(276) + r2m(vrdav_Rr, b,i,p,j) * t1(a,q) * wm_interm_cc3_105_triplet_pt4(a, b, i, j)
term(277) = term(277) + r2m(vrdav_Rr, b,i,p,j) * t1(a,q) * wm_interm_cc3_105_triplet_pt4(a, b, j, i)
term(278) = term(278) + r2m(vrdav_Rr, b,i,p,j) * t1(a,q) * wm_interm_cc3_104_triplet_pt4(a, b, j, i)
term(279) = term(279) + r2m(vrdav_Rr, b,i,p,j) * t1(a,q) * wm_interm_cc3_104_triplet_pt4(a, b, i, j)
term(280) = term(280) + r2m(vrdav_Rr, b,i,p,j) * t1(a,q) * wm_interm_cc3_106_triplet_pt4(a, b, j, i)
term(281) = term(281) + r2m(vrdav_Rr, b,i,p,j) * t1(a,q) * wm_interm_cc3_106_triplet_pt4(a, b, i, j)
end do 
end do 
end do 
end do 

term(258) = term(258) * 18.0d+0 
term(259) = term(259) * (-9.0d+0) 
term(260) = term(260) * 48.0d+0 
term(261) = term(261) * (-96.0d+0) 
term(262) = term(262) * (-24.0d+0) 
term(263) = term(263) * 48.0d+0 
term(264) = term(264) * 24.0d+0 
term(265) = term(265) * (-48.0d+0) 
term(266) = term(266) * (-16.0d+0) 
term(267) = term(267) * 32.0d+0 
term(268) = term(268) * 8.0d+0 
term(269) = term(269) * (-16.0d+0) 
term(270) = term(270) * (-4.0d+0) 
term(271) = term(271) * 8.0d+0 
term(272) = term(272) * 12.0d+0 
term(273) = term(273) * (-24.0d+0) 
term(274) = term(274) * (-4.0d+0) 
term(275) = term(275) * 8.0d+0 
term(276) = term(276) * (-4.0d+0) 
term(277) = term(277) * 8.0d+0 
term(278) = term(278) * (-2.0d+0) 
term(279) = term(279) * 4.0d+0 
term(280) = term(280) * (-2.0d+0) 
term(281) = term(281) * 4.0d+0 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(282) = term(282) + s1(p,i) * t1(a,j) * wm_interm_cc3_79_triplet_pt4(a, q, j, i)
term(283) = term(283) + s1(p,i) * t1(a,j) * wm_interm_cc3_81_triplet_pt4(a, q, j, i)
term(284) = term(284) + s1(p,i) * t1(a,j) * wm_interm_cc3_76_triplet_pt4(a, q, j, i)
term(285) = term(285) + s1(p,i) * t1(a,j) * wm_interm_cc3_81_triplet_pt4(a, j, q, i)
term(286) = term(286) + s1(p,i) * t1(a,j) * wm_interm_cc3_78_triplet_pt4(a, q, j, i)
term(287) = term(287) + s1(p,i) * t1(a,j) * wm_interm_cc3_78_triplet_pt4(a, j, q, i)
term(288) = term(288) + s1(p,i) * t1(a,j) * wm_interm_cc3_79_triplet_pt4(a, j, q, i)
term(289) = term(289) + s1(p,i) * t1(a,j) * wm_interm_cc3_76_triplet_pt4(a, j, q, i)
term(290) = term(290) + s1(p,i) * t1(a,j) * wm_interm_cc3_85_triplet_pt4(a, q, j, i)
term(291) = term(291) + s1(p,i) * t1(a,j) * wm_interm_cc3_85_triplet_pt4(a, j, q, i)
term(292) = term(292) + s1(p,i) * t1(a,j) * wm_interm_cc3_92_triplet_pt4(a, q, j, i)
term(293) = term(293) + s1(p,i) * t1(a,j) * wm_interm_cc3_93_triplet_pt4(a, q, j, i)
term(294) = term(294) + s1(p,i) * t1(a,j) * wm_interm_cc3_87_triplet_pt4(a, q, j, i)
term(295) = term(295) + s1(p,i) * t1(a,j) * wm_interm_cc3_86_triplet_pt4(a, q, j, i)
term(296) = term(296) + s1(p,i) * t1(a,j) * wm_interm_cc3_92_triplet_pt4(a, j, q, i)
term(297) = term(297) + s1(p,i) * t1(a,j) * wm_interm_cc3_93_triplet_pt4(a, j, q, i)
term(298) = term(298) + s1(p,i) * t1(a,j) * wm_interm_cc3_90_triplet_pt4(a, q, j, i)
term(299) = term(299) + s1(p,i) * t1(a,j) * wm_interm_cc3_91_triplet_pt4(a, q, j, i)
term(300) = term(300) + s1(p,i) * t1(a,j) * wm_interm_cc3_90_triplet_pt4(a, j, q, i)
term(301) = term(301) + s1(p,i) * t1(a,j) * wm_interm_cc3_91_triplet_pt4(a, j, q, i)
term(302) = term(302) + s1(p,i) * t1(a,j) * wm_interm_cc3_86_triplet_pt4(a, j, q, i)
term(303) = term(303) + s1(p,i) * t1(a,j) * wm_interm_cc3_87_triplet_pt4(a, j, q, i)
end do 
end do 
end do 

term(282) = term(282) * 12.0d+0 
term(283) = term(283) * (-6.0d+0) 
term(284) = term(284) * (-9.0d+0) 
term(285) = term(285) * 3.0d+0 
term(286) = term(286) * 6.0d+0 
term(287) = term(287) * (-12.0d+0) 
term(288) = term(288) * (-6.0d+0) 
term(289) = term(289) * 9.0d+0 
term(290) = term(290) * (-3.0d+0) 
term(291) = term(291) * 6.0d+0 
term(292) = term(292) * 12.0d+0 
term(293) = term(293) * (-24.0d+0) 
term(294) = term(294) * (-6.0d+0) 
term(295) = term(295) * 12.0d+0 
term(296) = term(296) * (-6.0d+0) 
term(297) = term(297) * 12.0d+0 
term(298) = term(298) * (-6.0d+0) 
term(299) = term(299) * 12.0d+0 
term(300) = term(300) * 12.0d+0 
term(301) = term(301) * (-24.0d+0) 
term(302) = term(302) * (-6.0d+0) 
term(303) = term(303) * 12.0d+0 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(304) = term(304) + r2m(vrdav_Rl, b,j,p,i) * s1(a,q) * wm_interm_cc3_98_triplet_pt4(b, a, j, i)
term(305) = term(305) + r2m(vrdav_Rl, b,j,p,i) * s1(a,q) * wm_interm_cc3_99_triplet_pt4(b, a, j, i)
term(306) = term(306) + r2m(vrdav_Rr, a,q,p,i) * t1(b,j) * wm_interm_cc3_102_triplet_pt4(b, a, j, i)
term(307) = term(307) + r2m(vrdav_Rr, a,i,p,q) * t1(b,j) * wm_interm_cc3_102_triplet_pt4(b, a, j, i)
term(308) = term(308) + r2m(vrdav_Rr, a,q,p,i) * t1(b,j) * wm_interm_cc3_103_triplet_pt4(b, a, j, i)
term(309) = term(309) + r2m(vrdav_Rr, a,i,p,q) * t1(b,j) * wm_interm_cc3_103_triplet_pt4(b, a, j, i)
term(310) = term(310) + r2m(vrdav_Rr, a,q,p,i) * t1(b,j) * wm_interm_cc3_105_triplet_pt4(b, a, j, i)
term(311) = term(311) + r2m(vrdav_Rr, a,i,p,q) * t1(b,j) * wm_interm_cc3_105_triplet_pt4(b, a, j, i)
term(312) = term(312) + r2m(vrdav_Rr, a,q,p,i) * t1(b,j) * wm_interm_cc3_104_triplet_pt4(b, a, j, i)
term(313) = term(313) + r2m(vrdav_Rr, a,i,p,q) * t1(b,j) * wm_interm_cc3_104_triplet_pt4(b, a, j, i)
end do 
end do 
end do 
end do 

term(304) = term(304) * 24.0d+0 
term(305) = term(305) * (-12.0d+0) 
term(306) = term(306) * (-32.0d+0) 
term(307) = term(307) * 64.0d+0 
term(308) = term(308) * 16.0d+0 
term(309) = term(309) * (-32.0d+0) 
term(310) = term(310) * 8.0d+0 
term(311) = term(311) * (-16.0d+0) 
term(312) = term(312) * (-4.0d+0) 
term(313) = term(313) * 8.0d+0 

do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do c = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(314) = term(314) + r2m(vrdav_Rl, b,i,c,j) * t3(nocc, nactive, a,b,p,q,j,i) * wm_interm_cc3_36_triplet_pt4(c, a)
end do 
end do 
end do 
end do 
end do 

term(314) = term(314) * (-1.9999999999999998d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(315) = term(315) + r2m(vrdav_Rl, b,j,c,i) * t3(nocc, nactive, a,b,p,q,j,i) * wm_interm_cc3_36_triplet_pt4(c, a)
end do 
end do 
end do 
end do 
end do 

term(315) = term(315) * 3.9999999999999996d+0 

do i = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(316) = term(316) + r2m(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_cc3_36_triplet_pt4(b, c)
end do 
end do 
end do 
end do 
end do 

term(316) = term(316) * (-3.9999999999999996d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(317) = term(317) + s1(a,i) * s1(p,q) * wm_interm_cc3_29_triplet_pt4(a, i)
term(318) = term(318) + s1(a,i) * s1(p,q) * wm_interm_cc3_30_triplet_pt4(a, i)
term(319) = term(319) + s1(a,i) * s1(p,q) * wm_interm_cc3_31_triplet_pt4(a, i)
term(320) = term(320) + s1(a,i) * s1(p,q) * wm_interm_cc3_32_triplet_pt4(a, i)
term(321) = term(321) + s1(a,i) * s1(p,q) * wm_interm_cc3_33_triplet_pt4(a, i)
term(322) = term(322) + s1(a,i) * s1(p,q) * wm_interm_cc3_34_triplet_pt4(a, i)
term(323) = term(323) + s1(p,i) * s1(a,q) * wm_interm_cc3_31_triplet_pt4(a, i)
term(324) = term(324) + s1(p,i) * s1(a,q) * wm_interm_cc3_34_triplet_pt4(a, i)
term(325) = term(325) + s1(p,i) * s1(a,q) * wm_interm_cc3_33_triplet_pt4(a, i)
term(326) = term(326) + s1(p,i) * s1(a,q) * wm_interm_cc3_32_triplet_pt4(a, i)
term(327) = term(327) + s1(p,i) * s1(a,q) * wm_interm_cc3_29_triplet_pt4(a, i)
term(328) = term(328) + s1(p,i) * s1(a,q) * wm_interm_cc3_30_triplet_pt4(a, i)
term(329) = term(329) + s1(a,i) * t1(p,q) * wm_interm_cc3_29_triplet_pt4(a, i)
term(330) = term(330) + s1(a,i) * t1(p,q) * wm_interm_cc3_30_triplet_pt4(a, i)
term(331) = term(331) + s1(a,i) * t1(p,q) * wm_interm_cc3_31_triplet_pt4(a, i)
term(332) = term(332) + s1(a,i) * t1(p,q) * wm_interm_cc3_32_triplet_pt4(a, i)
term(333) = term(333) + s1(a,i) * t1(p,q) * wm_interm_cc3_33_triplet_pt4(a, i)
term(334) = term(334) + s1(a,i) * t1(p,q) * wm_interm_cc3_34_triplet_pt4(a, i)
term(335) = term(335) + s1(a,i) * s1(p,q) * wm_interm_cc3_55_triplet_pt4(a, i)
term(336) = term(336) + s1(a,i) * s1(p,q) * wm_interm_cc3_56_triplet_pt4(a, i)
term(337) = term(337) + s1(a,i) * s1(p,q) * wm_interm_cc3_57_triplet_pt4(a, i)
term(338) = term(338) + s1(a,i) * s1(p,q) * wm_interm_cc3_58_triplet_pt4(a, i)
term(339) = term(339) + s1(p,i) * s1(a,q) * wm_interm_cc3_57_triplet_pt4(a, i)
term(340) = term(340) + s1(p,i) * s1(a,q) * wm_interm_cc3_58_triplet_pt4(a, i)
term(341) = term(341) + s1(p,i) * s1(a,q) * wm_interm_cc3_55_triplet_pt4(a, i)
term(342) = term(342) + s1(p,i) * s1(a,q) * wm_interm_cc3_56_triplet_pt4(a, i)
term(343) = term(343) + s1(a,i) * t1(p,q) * wm_interm_cc3_55_triplet_pt4(a, i)
term(344) = term(344) + s1(a,i) * t1(p,q) * wm_interm_cc3_56_triplet_pt4(a, i)
term(345) = term(345) + s1(a,i) * t1(p,q) * wm_interm_cc3_57_triplet_pt4(a, i)
term(346) = term(346) + s1(a,i) * t1(p,q) * wm_interm_cc3_58_triplet_pt4(a, i)
term(347) = term(347) + s1(p,q) * t1(a,i) * wm_interm_cc3_82_triplet_pt4(a, i)
term(348) = term(348) + s1(p,q) * t1(a,i) * wm_interm_cc3_80_triplet_pt4(a, i)
term(349) = term(349) + s1(p,q) * t1(a,i) * wm_interm_cc3_84_triplet_pt4(a, i)
term(350) = term(350) + s1(p,q) * t1(a,i) * wm_interm_cc3_83_triplet_pt4(a, i)
term(351) = term(351) + s1(p,q) * t1(a,i) * wm_interm_cc3_77_triplet_pt4(a, i)
term(352) = term(352) + s1(p,q) * t1(a,i) * wm_interm_cc3_94_triplet_pt4(a, i)
term(353) = term(353) + s1(p,q) * t1(a,i) * wm_interm_cc3_95_triplet_pt4(a, i)
term(354) = term(354) + s1(p,q) * t1(a,i) * wm_interm_cc3_88_triplet_pt4(a, i)
term(355) = term(355) + s1(p,q) * t1(a,i) * wm_interm_cc3_89_triplet_pt4(a, i)
term(356) = term(356) + t1(a,i) * t1(p,q) * wm_interm_cc3_82_triplet_pt4(a, i)
term(357) = term(357) + t1(a,i) * t1(p,q) * wm_interm_cc3_80_triplet_pt4(a, i)
term(358) = term(358) + t1(a,i) * t1(p,q) * wm_interm_cc3_84_triplet_pt4(a, i)
term(359) = term(359) + t1(a,i) * t1(p,q) * wm_interm_cc3_83_triplet_pt4(a, i)
term(360) = term(360) + t1(a,i) * t1(p,q) * wm_interm_cc3_77_triplet_pt4(a, i)
term(361) = term(361) + t1(a,i) * t1(p,q) * wm_interm_cc3_94_triplet_pt4(a, i)
term(362) = term(362) + t1(a,i) * t1(p,q) * wm_interm_cc3_95_triplet_pt4(a, i)
term(363) = term(363) + t1(a,i) * t1(p,q) * wm_interm_cc3_88_triplet_pt4(a, i)
term(364) = term(364) + t1(a,i) * t1(p,q) * wm_interm_cc3_89_triplet_pt4(a, i)
term(365) = term(365) + t1(p,i) * t1(a,q) * wm_interm_cc3_84_triplet_pt4(a, i)
term(366) = term(366) + t1(p,i) * t1(a,q) * wm_interm_cc3_77_triplet_pt4(a, i)
term(367) = term(367) + t1(p,i) * t1(a,q) * wm_interm_cc3_82_triplet_pt4(a, i)
term(368) = term(368) + t1(p,i) * t1(a,q) * wm_interm_cc3_83_triplet_pt4(a, i)
term(369) = term(369) + t1(p,i) * t1(a,q) * wm_interm_cc3_80_triplet_pt4(a, i)
term(370) = term(370) + t1(p,i) * t1(a,q) * wm_interm_cc3_88_triplet_pt4(a, i)
term(371) = term(371) + t1(p,i) * t1(a,q) * wm_interm_cc3_89_triplet_pt4(a, i)
term(372) = term(372) + t1(p,i) * t1(a,q) * wm_interm_cc3_94_triplet_pt4(a, i)
term(373) = term(373) + t1(p,i) * t1(a,q) * wm_interm_cc3_95_triplet_pt4(a, i)
end do 
end do 

term(317) = term(317) * 12.0d+0 
term(318) = term(318) * (-24.0d+0) 
term(319) = term(319) * (-6.0d+0) 
term(320) = term(320) * 12.0d+0 
term(321) = term(321) * (-6.0d+0) 
term(322) = term(322) * 12.0d+0 
term(323) = term(323) * 6.0d+0 
term(324) = term(324) * (-12.0d+0) 
term(325) = term(325) * 6.0d+0 
term(326) = term(326) * (-12.0d+0) 
term(327) = term(327) * (-12.0d+0) 
term(328) = term(328) * 24.0d+0 
term(329) = term(329) * 12.0d+0 
term(330) = term(330) * (-24.0d+0) 
term(331) = term(331) * (-6.0d+0) 
term(332) = term(332) * 12.0d+0 
term(333) = term(333) * (-6.0d+0) 
term(334) = term(334) * 12.0d+0 
term(335) = term(335) * 24.0d+0 
term(336) = term(336) * (-48.0d+0) 
term(337) = term(337) * (-24.0d+0) 
term(338) = term(338) * 48.0d+0 
term(339) = term(339) * 24.0d+0 
term(340) = term(340) * (-48.0d+0) 
term(341) = term(341) * (-24.0d+0) 
term(342) = term(342) * 48.0d+0 
term(343) = term(343) * 24.0d+0 
term(344) = term(344) * (-48.0d+0) 
term(345) = term(345) * (-24.0d+0) 
term(346) = term(346) * 48.0d+0 
term(347) = term(347) * 36.0d+0 
term(348) = term(348) * (-12.0d+0) 
term(349) = term(349) * (-12.0d+0) 
term(350) = term(350) * 6.0d+0 
term(351) = term(351) * 6.0d+0 
term(352) = term(352) * (-24.0d+0) 
term(353) = term(353) * 48.0d+0 
term(354) = term(354) * 24.0d+0 
term(355) = term(355) * (-48.0d+0) 
term(356) = term(356) * 36.0d+0 
term(357) = term(357) * (-12.0d+0) 
term(358) = term(358) * (-12.0d+0) 
term(359) = term(359) * 6.0d+0 
term(360) = term(360) * 6.0d+0 
term(361) = term(361) * (-24.0d+0) 
term(362) = term(362) * 48.0d+0 
term(363) = term(363) * 24.0d+0 
term(364) = term(364) * (-48.0d+0) 
term(365) = term(365) * 12.0d+0 
term(366) = term(366) * (-6.0d+0) 
term(367) = term(367) * (-36.0d+0) 
term(368) = term(368) * (-6.0d+0) 
term(369) = term(369) * 12.0d+0 
term(370) = term(370) * (-24.0d+0) 
term(371) = term(371) * 48.0d+0 
term(372) = term(372) * 24.0d+0 
term(373) = term(373) * (-48.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do c = nocc + 1, nactive 
term(374) = term(374) + r3(vrdav_Rl, a,q,b,j,p,i) * r2p(vrdav_Rr, c,i,a,j) * wm_interm_cc3_28_triplet_pt4(c, b)
term(375) = term(375) + r3(vrdav_Rl, a,q,b,j,p,i) * r2p(vrdav_Rr, c,i,b,j) * wm_interm_cc3_28_triplet_pt4(c, a)
term(376) = term(376) + r3(vrdav_Rl, a,q,b,j,p,i) * r2p(vrdav_Rr, c,j,b,i) * wm_interm_cc3_28_triplet_pt4(c, a)
term(377) = term(377) + r3(vrdav_Rl, a,q,b,j,p,i) * r2p(vrdav_Rr, c,j,a,i) * wm_interm_cc3_28_triplet_pt4(c, b)
end do 
end do 
end do 
end do 
end do 

term(374) = term(374) * (-9.0d+0) 
term(375) = term(375) * 9.0d+0 
term(376) = term(376) * (-9.0d+0) 
term(377) = term(377) * 9.0d+0 

do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do i = 1, nocc 
do c = nocc + 1, nactive 
term(378) = term(378) + r3(vrdav_Rl, a,i,b,j,p,q) * r2p(vrdav_Rr, c,i,a,j) * wm_interm_cc3_28_triplet_pt4(c, b)
term(379) = term(379) + r3(vrdav_Rl, a,i,b,j,p,q) * r2p(vrdav_Rr, c,i,b,j) * wm_interm_cc3_28_triplet_pt4(c, a)
end do 
end do 
end do 
end do 
end do 

term(378) = term(378) * 18.0d+0 
term(379) = term(379) * (-18.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(380) = term(380) + r2p(vrdav_Rl, p,q,a,i) * s1(b,j) * wm_interm_cc3_98_triplet_pt4(a, b, i, j)
term(381) = term(381) + r2p(vrdav_Rl, p,q,a,i) * s1(b,j) * wm_interm_cc3_99_triplet_pt4(a, b, i, j)
term(382) = term(382) + r2p(vrdav_Rl, p,q,a,i) * s1(b,j) * wm_interm_cc3_100_triplet_pt4(a, b, i, j)
term(383) = term(383) + r2m(vrdav_Rl, b,j,p,i) * s1(a,q) * wm_interm_cc3_98_triplet_pt4(b, a, i, j)
term(384) = term(384) + r2m(vrdav_Rl, b,i,p,j) * s1(a,q) * wm_interm_cc3_98_triplet_pt4(b, a, j, i)
term(385) = term(385) + r2m(vrdav_Rl, b,i,p,j) * s1(a,q) * wm_interm_cc3_98_triplet_pt4(b, a, i, j)
term(386) = term(386) + r2m(vrdav_Rl, b,j,p,i) * s1(a,q) * wm_interm_cc3_101_triplet_pt4(b, a, i, j)
term(387) = term(387) + r2m(vrdav_Rl, b,j,p,i) * s1(a,q) * wm_interm_cc3_100_triplet_pt4(b, a, i, j)
term(388) = term(388) + r2m(vrdav_Rl, b,i,p,j) * s1(a,q) * wm_interm_cc3_100_triplet_pt4(b, a, i, j)
term(389) = term(389) + r2m(vrdav_Rl, b,i,p,j) * s1(a,q) * wm_interm_cc3_101_triplet_pt4(b, a, i, j)
term(390) = term(390) + r2m(vrdav_Rl, b,i,p,j) * s1(a,q) * wm_interm_cc3_99_triplet_pt4(b, a, j, i)
term(391) = term(391) + r2m(vrdav_Rl, b,i,p,j) * s1(a,q) * wm_interm_cc3_99_triplet_pt4(b, a, i, j)
term(392) = term(392) + r2m(vrdav_Rl, b,j,p,i) * s1(a,q) * wm_interm_cc3_99_triplet_pt4(b, a, i, j)
term(393) = term(393) + r2p(vrdav_Rr, p,q,a,i) * t1(b,j) * wm_interm_cc3_102_triplet_pt4(a, b, i, j)
term(394) = term(394) + r2p(vrdav_Rr, p,q,a,i) * t1(b,j) * wm_interm_cc3_102_triplet_pt4(b, a, i, j)
term(395) = term(395) + r2p(vrdav_Rr, p,q,a,i) * t1(b,j) * wm_interm_cc3_103_triplet_pt4(a, b, i, j)
term(396) = term(396) + r2p(vrdav_Rr, p,q,a,i) * t1(b,j) * wm_interm_cc3_103_triplet_pt4(b, a, i, j)
term(397) = term(397) + r2p(vrdav_Rr, p,q,a,i) * t1(b,j) * wm_interm_cc3_104_triplet_pt4(a, b, i, j)
term(398) = term(398) + r2p(vrdav_Rr, p,q,a,i) * t1(b,j) * wm_interm_cc3_105_triplet_pt4(b, a, i, j)
term(399) = term(399) + r2p(vrdav_Rr, p,q,a,i) * t1(b,j) * wm_interm_cc3_104_triplet_pt4(b, a, i, j)
term(400) = term(400) + r2m(vrdav_Rr, a,q,p,i) * t1(b,j) * wm_interm_cc3_102_triplet_pt4(b, a, i, j)
term(401) = term(401) + r2m(vrdav_Rr, a,i,p,q) * t1(b,j) * wm_interm_cc3_102_triplet_pt4(b, a, i, j)
term(402) = term(402) + r2m(vrdav_Rr, a,q,p,i) * t1(b,j) * wm_interm_cc3_103_triplet_pt4(b, a, i, j)
term(403) = term(403) + r2m(vrdav_Rr, a,i,p,q) * t1(b,j) * wm_interm_cc3_103_triplet_pt4(b, a, i, j)
term(404) = term(404) + r2m(vrdav_Rr, a,q,p,i) * t1(b,j) * wm_interm_cc3_105_triplet_pt4(b, a, i, j)
term(405) = term(405) + r2m(vrdav_Rr, a,i,p,q) * t1(b,j) * wm_interm_cc3_105_triplet_pt4(b, a, i, j)
term(406) = term(406) + r2m(vrdav_Rr, a,q,p,i) * t1(b,j) * wm_interm_cc3_104_triplet_pt4(b, a, i, j)
term(407) = term(407) + r2m(vrdav_Rr, a,i,p,q) * t1(b,j) * wm_interm_cc3_104_triplet_pt4(b, a, i, j)
term(408) = term(408) + r2m(vrdav_Rr, b,i,p,j) * t1(a,q) * wm_interm_cc3_102_triplet_pt4(b, a, j, i)
term(409) = term(409) + r2m(vrdav_Rr, b,i,p,j) * t1(a,q) * wm_interm_cc3_102_triplet_pt4(b, a, i, j)
term(410) = term(410) + r2m(vrdav_Rr, b,i,p,j) * t1(a,q) * wm_interm_cc3_103_triplet_pt4(b, a, j, i)
term(411) = term(411) + r2m(vrdav_Rr, b,i,p,j) * t1(a,q) * wm_interm_cc3_103_triplet_pt4(b, a, i, j)
term(412) = term(412) + r2m(vrdav_Rr, b,i,p,j) * t1(a,q) * wm_interm_cc3_104_triplet_pt4(b, a, i, j)
term(413) = term(413) + r2m(vrdav_Rr, b,i,p,j) * t1(a,q) * wm_interm_cc3_104_triplet_pt4(b, a, j, i)
term(414) = term(414) + r2m(vrdav_Rr, b,i,p,j) * t1(a,q) * wm_interm_cc3_105_triplet_pt4(b, a, j, i)
term(415) = term(415) + r2m(vrdav_Rr, b,i,p,j) * t1(a,q) * wm_interm_cc3_105_triplet_pt4(b, a, i, j)
term(416) = term(416) + r2m(vrdav_Rr, b,i,p,j) * t1(a,q) * wm_interm_cc3_106_triplet_pt4(b, a, i, j)
term(417) = term(417) + r2m(vrdav_Rr, b,i,p,j) * t1(a,q) * wm_interm_cc3_106_triplet_pt4(b, a, j, i)
end do 
end do 
end do 
end do 

term(380) = term(380) * (-72.0d+0) 
term(381) = term(381) * 36.0d+0 
term(382) = term(382) * (-36.0d+0) 
term(383) = term(383) * (-12.0d+0) 
term(384) = term(384) * (-12.0d+0) 
term(385) = term(385) * 24.0d+0 
term(386) = term(386) * 6.0d+0 
term(387) = term(387) * (-12.0d+0) 
term(388) = term(388) * 6.0d+0 
term(389) = term(389) * (-12.0d+0) 
term(390) = term(390) * 6.0d+0 
term(391) = term(391) * (-12.0d+0) 
term(392) = term(392) * 6.0d+0 
term(393) = term(393) * 24.0d+0 
term(394) = term(394) * (-24.0d+0) 
term(395) = term(395) * (-12.0d+0) 
term(396) = term(396) * 12.0d+0 
term(397) = term(397) * 6.0d+0 
term(398) = term(398) * 6.0d+0 
term(399) = term(399) * (-12.0d+0) 
term(400) = term(400) * 16.0d+0 
term(401) = term(401) * (-32.0d+0) 
term(402) = term(402) * (-8.0d+0) 
term(403) = term(403) * 16.0d+0 
term(404) = term(404) * (-4.0d+0) 
term(405) = term(405) * 8.0d+0 
term(406) = term(406) * 8.0d+0 
term(407) = term(407) * (-16.0d+0) 
term(408) = term(408) * 12.0d+0 
term(409) = term(409) * (-24.0d+0) 
term(410) = term(410) * (-4.0d+0) 
term(411) = term(411) * 8.0d+0 
term(412) = term(412) * (-2.0d+0) 
term(413) = term(413) * 4.0d+0 
term(414) = term(414) * (-4.0d+0) 
term(415) = term(415) * 8.0d+0 
term(416) = term(416) * (-2.0d+0) 
term(417) = term(417) * 4.0d+0 

do i = 1, nocc 
term(418) = term(418) + wm_interm_cc3_2_triplet_pt4(p, i) * wm_interm_cc3_7_triplet_pt4(q, i)
term(419) = term(419) + wm_interm_cc3_1_triplet_pt4(p, i) * wm_interm_cc3_7_triplet_pt4(q, i)
term(420) = term(420) + wm_interm_cc3_6_triplet_pt4(p, i) * wm_interm_cc3_7_triplet_pt4(q, i)
term(421) = term(421) + wm_interm_cc3_3_triplet_pt4(p, i) * wm_interm_cc3_7_triplet_pt4(q, i)
term(422) = term(422) + wm_interm_cc3_5_triplet_pt4(p, i) * wm_interm_cc3_7_triplet_pt4(q, i)
term(423) = term(423) + wm_interm_cc3_4_triplet_pt4(p, i) * wm_interm_cc3_7_triplet_pt4(q, i)
term(424) = term(424) + wm_interm_cc3_13_triplet_pt4(p, i) * wm_interm_cc3_7_triplet_pt4(q, i)
term(425) = term(425) + wm_interm_cc3_14_triplet_pt4(p, i) * wm_interm_cc3_7_triplet_pt4(q, i)
term(426) = term(426) + wm_interm_cc3_11_triplet_pt4(p, i) * wm_interm_cc3_7_triplet_pt4(q, i)
term(427) = term(427) + wm_interm_cc3_12_triplet_pt4(p, i) * wm_interm_cc3_7_triplet_pt4(q, i)
term(428) = term(428) + wm_interm_cc3_15_triplet_pt4(p, i) * wm_interm_cc3_7_triplet_pt4(q, i)
term(429) = term(429) + wm_interm_cc3_16_triplet_pt4(p, i) * wm_interm_cc3_7_triplet_pt4(q, i)
term(430) = term(430) + wm_interm_cc3_25_triplet_pt4(i, q) * wm_interm_cc3_31_triplet_pt4(p, i)
term(431) = term(431) + wm_interm_cc3_25_triplet_pt4(i, q) * wm_interm_cc3_34_triplet_pt4(p, i)
term(432) = term(432) + wm_interm_cc3_25_triplet_pt4(i, q) * wm_interm_cc3_33_triplet_pt4(p, i)
term(433) = term(433) + wm_interm_cc3_25_triplet_pt4(i, q) * wm_interm_cc3_32_triplet_pt4(p, i)
term(434) = term(434) + wm_interm_cc3_25_triplet_pt4(i, q) * wm_interm_cc3_29_triplet_pt4(p, i)
term(435) = term(435) + wm_interm_cc3_25_triplet_pt4(i, q) * wm_interm_cc3_30_triplet_pt4(p, i)
term(436) = term(436) + wm_interm_cc3_25_triplet_pt4(i, q) * wm_interm_cc3_57_triplet_pt4(p, i)
term(437) = term(437) + wm_interm_cc3_25_triplet_pt4(i, q) * wm_interm_cc3_58_triplet_pt4(p, i)
term(438) = term(438) + wm_interm_cc3_25_triplet_pt4(i, q) * wm_interm_cc3_55_triplet_pt4(p, i)
term(439) = term(439) + wm_interm_cc3_25_triplet_pt4(i, q) * wm_interm_cc3_56_triplet_pt4(p, i)
end do 

term(418) = term(418) * (-1.5d+0) 
term(419) = term(419) * 1.5d+0 
term(420) = term(420) * 1.5d+0 
term(421) = term(421) * (-3.0d+0) 
term(422) = term(422) * (-1.5d+0) 
term(423) = term(423) * 3.0d+0 
term(424) = term(424) * (-2.0d+0) 
term(425) = term(425) * 4.0d+0 
term(426) = term(426) * (-2.0d+0) 
term(427) = term(427) * 4.0d+0 
term(428) = term(428) * 4.0d+0 
term(429) = term(429) * (-8.0d+0) 
term(430) = term(430) * (-3.0d+0) 
term(431) = term(431) * 6.0d+0 
term(432) = term(432) * (-3.0d+0) 
term(433) = term(433) * 6.0d+0 
term(434) = term(434) * 6.0d+0 
term(435) = term(435) * (-12.0d+0) 
term(436) = term(436) * (-12.0d+0) 
term(437) = term(437) * 24.0d+0 
term(438) = term(438) * 12.0d+0 
term(439) = term(439) * (-24.0d+0) 


    calc_D_vo_wm_triplet_cc3_pt4 = zero
    do s = 0, 439
    calc_D_vo_wm_triplet_cc3_pt4 = calc_D_vo_wm_triplet_cc3_pt4 + term(s)
    end do

    end function calc_D_vo_wm_triplet_cc3_pt4
    
    function calc_D_vv_wm_triplet_cc3_pt4(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vv_wm_triplet_cc3_pt4
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    double precision, dimension(:), intent(in) :: vrdav_Rl
    double precision, dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , j, i, a, b 
    real(F64), dimension(0:45) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + wm_interm_cc3_107_triplet_pt4(q, a, i, j) * wm_interm_cc3_98_triplet_pt4(p, a, i, j)
term(1) = term(1) + wm_interm_cc3_107_triplet_pt4(q, a, i, j) * wm_interm_cc3_98_triplet_pt4(p, a, j, i)
term(2) = term(2) + wm_interm_cc3_107_triplet_pt4(q, a, i, j) * wm_interm_cc3_99_triplet_pt4(p, a, i, j)
term(3) = term(3) + wm_interm_cc3_100_triplet_pt4(p, a, i, j) * wm_interm_cc3_107_triplet_pt4(q, a, i, j)
term(4) = term(4) + wm_interm_cc3_109_triplet_pt4(a, q, i, j) * wm_interm_cc3_110_triplet_pt4(p, a, j, i)
term(5) = term(5) + wm_interm_cc3_108_triplet_pt4(p, a, i, j) * wm_interm_cc3_109_triplet_pt4(a, q, j, i)
term(6) = term(6) + wm_interm_cc3_109_triplet_pt4(a, q, i, j) * wm_interm_cc3_110_triplet_pt4(p, a, i, j)
term(7) = term(7) + wm_interm_cc3_109_triplet_pt4(a, q, i, j) * wm_interm_cc3_110_triplet_pt4(a, p, i, j)
term(8) = term(8) + wm_interm_cc3_109_triplet_pt4(a, q, i, j) * wm_interm_cc3_110_triplet_pt4(a, p, j, i)
term(9) = term(9) + wm_interm_cc3_108_triplet_pt4(p, a, i, j) * wm_interm_cc3_109_triplet_pt4(a, q, i, j)
term(10) = term(10) + wm_interm_cc3_108_triplet_pt4(a, p, i, j) * wm_interm_cc3_109_triplet_pt4(a, q, j, i)
term(11) = term(11) + wm_interm_cc3_108_triplet_pt4(a, p, i, j) * wm_interm_cc3_109_triplet_pt4(a, q, i, j)
term(12) = term(12) + wm_interm_cc3_102_triplet_pt4(q, a, i, j) * wm_interm_cc3_111_triplet_pt4(p, a, i, j)
term(13) = term(13) + wm_interm_cc3_105_triplet_pt4(q, a, i, j) * wm_interm_cc3_111_triplet_pt4(p, a, i, j)
term(14) = term(14) + wm_interm_cc3_103_triplet_pt4(q, a, i, j) * wm_interm_cc3_111_triplet_pt4(p, a, i, j)
term(15) = term(15) + wm_interm_cc3_105_triplet_pt4(q, a, i, j) * wm_interm_cc3_111_triplet_pt4(p, a, j, i)
term(16) = term(16) + wm_interm_cc3_106_triplet_pt4(q, a, i, j) * wm_interm_cc3_111_triplet_pt4(p, a, i, j)
term(17) = term(17) + wm_interm_cc3_102_triplet_pt4(q, a, i, j) * wm_interm_cc3_111_triplet_pt4(p, a, j, i)
term(18) = term(18) + wm_interm_cc3_103_triplet_pt4(q, a, i, j) * wm_interm_cc3_111_triplet_pt4(p, a, j, i)
term(19) = term(19) + wm_interm_cc3_105_triplet_pt4(a, q, i, j) * wm_interm_cc3_111_triplet_pt4(p, a, i, j)
term(20) = term(20) + wm_interm_cc3_102_triplet_pt4(a, q, i, j) * wm_interm_cc3_111_triplet_pt4(p, a, i, j)
term(21) = term(21) + wm_interm_cc3_106_triplet_pt4(a, q, i, j) * wm_interm_cc3_111_triplet_pt4(p, a, i, j)
term(22) = term(22) + wm_interm_cc3_102_triplet_pt4(a, q, i, j) * wm_interm_cc3_111_triplet_pt4(p, a, j, i)
term(23) = term(23) + wm_interm_cc3_106_triplet_pt4(a, q, i, j) * wm_interm_cc3_111_triplet_pt4(p, a, j, i)
term(24) = term(24) + wm_interm_cc3_105_triplet_pt4(a, q, i, j) * wm_interm_cc3_111_triplet_pt4(p, a, j, i)
term(25) = term(25) + wm_interm_cc3_106_triplet_pt4(q, a, i, j) * wm_interm_cc3_111_triplet_pt4(p, a, j, i)
term(26) = term(26) + wm_interm_cc3_113_triplet_pt4(a, q, i, j) * wm_interm_cc3_114_triplet_pt4(p, a, j, i)
term(27) = term(27) + wm_interm_cc3_112_triplet_pt4(p, a, i, j) * wm_interm_cc3_113_triplet_pt4(a, q, j, i)
term(28) = term(28) + wm_interm_cc3_113_triplet_pt4(a, q, i, j) * wm_interm_cc3_114_triplet_pt4(p, a, i, j)
term(29) = term(29) + wm_interm_cc3_113_triplet_pt4(a, q, i, j) * wm_interm_cc3_116_triplet_pt4(p, a, j, i)
term(30) = term(30) + wm_interm_cc3_113_triplet_pt4(a, q, i, j) * wm_interm_cc3_115_triplet_pt4(p, a, j, i)
term(31) = term(31) + wm_interm_cc3_112_triplet_pt4(p, a, i, j) * wm_interm_cc3_113_triplet_pt4(a, q, i, j)
term(32) = term(32) + wm_interm_cc3_113_triplet_pt4(a, q, i, j) * wm_interm_cc3_117_triplet_pt4(p, a, j, i)
term(33) = term(33) + wm_interm_cc3_113_triplet_pt4(a, q, i, j) * wm_interm_cc3_117_triplet_pt4(p, a, i, j)
term(34) = term(34) + wm_interm_cc3_113_triplet_pt4(a, q, i, j) * wm_interm_cc3_114_triplet_pt4(a, p, j, i)
term(35) = term(35) + wm_interm_cc3_113_triplet_pt4(a, q, i, j) * wm_interm_cc3_115_triplet_pt4(a, p, j, i)
term(36) = term(36) + wm_interm_cc3_112_triplet_pt4(a, p, i, j) * wm_interm_cc3_113_triplet_pt4(a, q, j, i)
term(37) = term(37) + wm_interm_cc3_112_triplet_pt4(a, p, i, j) * wm_interm_cc3_113_triplet_pt4(a, q, i, j)
term(38) = term(38) + wm_interm_cc3_113_triplet_pt4(a, q, i, j) * wm_interm_cc3_116_triplet_pt4(p, a, i, j)
term(39) = term(39) + wm_interm_cc3_113_triplet_pt4(a, q, i, j) * wm_interm_cc3_115_triplet_pt4(p, a, i, j)
term(40) = term(40) + wm_interm_cc3_113_triplet_pt4(a, q, i, j) * wm_interm_cc3_115_triplet_pt4(a, p, i, j)
term(41) = term(41) + wm_interm_cc3_113_triplet_pt4(a, q, i, j) * wm_interm_cc3_114_triplet_pt4(a, p, i, j)
term(42) = term(42) + wm_interm_cc3_113_triplet_pt4(a, q, i, j) * wm_interm_cc3_117_triplet_pt4(a, p, i, j)
term(43) = term(43) + wm_interm_cc3_113_triplet_pt4(a, q, i, j) * wm_interm_cc3_116_triplet_pt4(a, p, j, i)
term(44) = term(44) + wm_interm_cc3_113_triplet_pt4(a, q, i, j) * wm_interm_cc3_117_triplet_pt4(a, p, j, i)
term(45) = term(45) + wm_interm_cc3_113_triplet_pt4(a, q, i, j) * wm_interm_cc3_116_triplet_pt4(a, p, i, j)
end do 
end do 
end do 

term(0) = term(0) * (-24.0d+0) 
term(1) = term(1) * 24.0d+0 
term(2) = term(2) * 12.0d+0 
term(3) = term(3) * (-12.0d+0) 
term(4) = term(4) * 12.0d+0 
term(5) = term(5) * (-12.0d+0) 
term(6) = term(6) * (-6.0d+0) 
term(7) = term(7) * 12.0d+0 
term(8) = term(8) * (-6.0d+0) 
term(9) = term(9) * 6.0d+0 
term(10) = term(10) * 6.0d+0 
term(11) = term(11) * (-12.0d+0) 
term(12) = term(12) * 16.0d+0 
term(13) = term(13) * (-8.0d+0) 
term(14) = term(14) * (-4.0d+0) 
term(15) = term(15) * 4.0d+0 
term(16) = term(16) * 2.0d+0 
term(17) = term(17) * (-8.0d+0) 
term(18) = term(18) * 2.0d+0 
term(19) = term(19) * 2.0d+0 
term(20) = term(20) * (-4.0d+0) 
term(21) = term(21) * (-4.0d+0) 
term(22) = term(22) * 8.0d+0 
term(23) = term(23) * 2.0d+0 
term(24) = term(24) * (-4.0d+0) 
term(25) = term(25) * (-4.0d+0) 
term(26) = term(26) * (-4.0d+0) 
term(27) = term(27) * 12.0d+0 
term(28) = term(28) * 2.0d+0 
term(29) = term(29) * (-4.0d+0) 
term(31) = term(31) * (-6.0d+0) 
term(33) = term(33) * (-2.0d+0) 
term(34) = term(34) * 2.0d+0 
term(35) = term(35) * (-2.0d+0) 
term(36) = term(36) * (-6.0d+0) 
term(37) = term(37) * 12.0d+0 
term(38) = term(38) * 2.0d+0 
term(39) = term(39) * (-2.0d+0) 
term(41) = term(41) * (-4.0d+0) 
term(43) = term(43) * 2.0d+0 
term(44) = term(44) * (-2.0d+0) 
term(45) = term(45) * (-4.0d+0) 


    calc_D_vv_wm_triplet_cc3_pt4 = zero
    do s = 0, 45
    calc_D_vv_wm_triplet_cc3_pt4 = calc_D_vv_wm_triplet_cc3_pt4 + term(s)
    end do

    end function calc_D_vv_wm_triplet_cc3_pt4
    

    

end module density_exc_exc_functions_cc3_triplet_pt4
