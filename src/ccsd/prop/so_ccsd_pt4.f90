module so_ccsd_pt4
 use ccsd_transformed_integrals
    use cc3_intermediates
    use arithmetic
    use s_gen
    use basis
    use eom_vectors

    implicit none
    !
    ! File generated automatically on 2018-04-18 13:05:52
    !
        real(F64), dimension(:, :), allocatable :: wm_interm_0_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_1_so_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_2_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_3_so_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_4_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_5_so_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_6_so_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_7_so_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_8_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_9_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_10_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_11_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_12_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_13_so_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_14_so_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_15_so_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_16_so_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_17_so_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_18_so_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_19_so_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_20_so_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_21_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_22_so_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_23_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_24_so_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_25_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_26_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_27_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_28_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_29_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_30_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_31_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_32_so_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_33_so_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_34_so_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_35_so_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_36_so_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_37_so_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_38_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_39_so_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_40_so_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_41_so_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_42_so_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_43_so_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_44_so_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_45_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_46_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_47_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_48_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_49_so_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_50_so_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_51_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_52_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_53_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_54_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_55_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_56_so_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_57_so_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_58_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_59_so_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_60_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_61_so_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_62_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_63_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_64_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_65_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_66_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_67_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_68_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_69_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_70_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_71_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_72_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_73_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_74_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_75_so_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_76_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_77_so_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_78_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_79_so_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_80_so_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_81_so_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_82_so_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_83_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_84_so_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_85_so_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_86_so_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_87_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_88_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_89_so_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_90_so_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_91_so_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_92_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_93_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_94_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_95_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_96_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_97_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_98_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_99_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_100_so_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_101_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_102_so_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_103_so_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_104_so_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_105_so_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_106_so_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_107_so_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_108_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_109_so_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_110_so_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_111_so_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_112_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_113_so_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_114_so_pt4 

    contains
    
    subroutine wm_so_intermediates_ccsd_init_pt4(nocc, nactive)
    integer, intent(in) :: nocc
    integer, intent(in) :: nactive
    allocate(wm_interm_0_so_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_1_so_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_2_so_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_3_so_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_4_so_pt4(1: nocc, 1: nocc))
allocate(wm_interm_5_so_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_6_so_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_7_so_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_8_so_pt4(1: nocc, 1: nocc))
allocate(wm_interm_9_so_pt4(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_10_so_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_11_so_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_12_so_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_13_so_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_14_so_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_15_so_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_16_so_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_17_so_pt4(1: nocc, 1: nocc))
allocate(wm_interm_18_so_pt4(1: nocc, 1: nocc))
allocate(wm_interm_19_so_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_20_so_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_21_so_pt4(1: nocc, 1: nocc))
allocate(wm_interm_22_so_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_23_so_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_24_so_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_25_so_pt4(1: nocc, 1: nocc))
allocate(wm_interm_26_so_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_27_so_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_28_so_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_29_so_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_30_so_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_31_so_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_32_so_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_33_so_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_34_so_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_35_so_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_36_so_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_37_so_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_38_so_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_39_so_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_40_so_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_41_so_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_42_so_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_43_so_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_44_so_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_45_so_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_46_so_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_47_so_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_48_so_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_49_so_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_50_so_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_51_so_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_52_so_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_53_so_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_54_so_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_55_so_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_56_so_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_57_so_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_58_so_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_59_so_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_60_so_pt4(1: nocc, 1: nocc))
allocate(wm_interm_61_so_pt4(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_62_so_pt4(1: nocc, 1: nocc))
allocate(wm_interm_63_so_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_64_so_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_65_so_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_66_so_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_67_so_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_68_so_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_69_so_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_70_so_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_71_so_pt4(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_72_so_pt4(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_73_so_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_74_so_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_75_so_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_76_so_pt4(1: nocc, 1: nocc))
allocate(wm_interm_77_so_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_78_so_pt4(1: nocc, 1: nocc))
allocate(wm_interm_79_so_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_80_so_pt4(1: nocc, 1: nocc))
allocate(wm_interm_81_so_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_82_so_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_83_so_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_84_so_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_85_so_pt4(1: nocc, 1: nocc))
allocate(wm_interm_86_so_pt4(1: nocc, 1: nocc))
allocate(wm_interm_87_so_pt4(1: nocc, 1: nocc))
allocate(wm_interm_88_so_pt4(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_89_so_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_90_so_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_91_so_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_92_so_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_93_so_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_94_so_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_95_so_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_96_so_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_97_so_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_98_so_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_99_so_pt4(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_100_so_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_101_so_pt4(1: nocc, 1: nocc))
allocate(wm_interm_102_so_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_103_so_pt4(1: nocc, 1: nocc))
allocate(wm_interm_104_so_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_105_so_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_106_so_pt4(1: nocc, 1: nocc))
allocate(wm_interm_107_so_pt4(1: nocc, 1: nocc))
allocate(wm_interm_108_so_pt4(1: nocc, 1: nocc))
allocate(wm_interm_109_so_pt4(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_110_so_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_111_so_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_112_so_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_113_so_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_114_so_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
wm_interm_0_so_pt4 = zero 
wm_interm_1_so_pt4 = zero 
wm_interm_2_so_pt4 = zero 
wm_interm_3_so_pt4 = zero 
wm_interm_4_so_pt4 = zero 
wm_interm_5_so_pt4 = zero 
wm_interm_6_so_pt4 = zero 
wm_interm_7_so_pt4 = zero 
wm_interm_8_so_pt4 = zero 
wm_interm_9_so_pt4 = zero 
wm_interm_10_so_pt4 = zero 
wm_interm_11_so_pt4 = zero 
wm_interm_12_so_pt4 = zero 
wm_interm_13_so_pt4 = zero 
wm_interm_14_so_pt4 = zero 
wm_interm_15_so_pt4 = zero 
wm_interm_16_so_pt4 = zero 
wm_interm_17_so_pt4 = zero 
wm_interm_18_so_pt4 = zero 
wm_interm_19_so_pt4 = zero 
wm_interm_20_so_pt4 = zero 
wm_interm_21_so_pt4 = zero 
wm_interm_22_so_pt4 = zero 
wm_interm_23_so_pt4 = zero 
wm_interm_24_so_pt4 = zero 
wm_interm_25_so_pt4 = zero 
wm_interm_26_so_pt4 = zero 
wm_interm_27_so_pt4 = zero 
wm_interm_28_so_pt4 = zero 
wm_interm_29_so_pt4 = zero 
wm_interm_30_so_pt4 = zero 
wm_interm_31_so_pt4 = zero 
wm_interm_32_so_pt4 = zero 
wm_interm_33_so_pt4 = zero 
wm_interm_34_so_pt4 = zero 
wm_interm_35_so_pt4 = zero 
wm_interm_36_so_pt4 = zero 
wm_interm_37_so_pt4 = zero 
wm_interm_38_so_pt4 = zero 
wm_interm_39_so_pt4 = zero 
wm_interm_40_so_pt4 = zero 
wm_interm_41_so_pt4 = zero 
wm_interm_42_so_pt4 = zero 
wm_interm_43_so_pt4 = zero 
wm_interm_44_so_pt4 = zero 
wm_interm_45_so_pt4 = zero 
wm_interm_46_so_pt4 = zero 
wm_interm_47_so_pt4 = zero 
wm_interm_48_so_pt4 = zero 
wm_interm_49_so_pt4 = zero 
wm_interm_50_so_pt4 = zero 
wm_interm_51_so_pt4 = zero 
wm_interm_52_so_pt4 = zero 
wm_interm_53_so_pt4 = zero 
wm_interm_54_so_pt4 = zero 
wm_interm_55_so_pt4 = zero 
wm_interm_56_so_pt4 = zero 
wm_interm_57_so_pt4 = zero 
wm_interm_58_so_pt4 = zero 
wm_interm_59_so_pt4 = zero 
wm_interm_60_so_pt4 = zero 
wm_interm_61_so_pt4 = zero 
wm_interm_62_so_pt4 = zero 
wm_interm_63_so_pt4 = zero 
wm_interm_64_so_pt4 = zero 
wm_interm_65_so_pt4 = zero 
wm_interm_66_so_pt4 = zero 
wm_interm_67_so_pt4 = zero 
wm_interm_68_so_pt4 = zero 
wm_interm_69_so_pt4 = zero 
wm_interm_70_so_pt4 = zero 
wm_interm_71_so_pt4 = zero 
wm_interm_72_so_pt4 = zero 
wm_interm_73_so_pt4 = zero 
wm_interm_74_so_pt4 = zero 
wm_interm_75_so_pt4 = zero 
wm_interm_76_so_pt4 = zero 
wm_interm_77_so_pt4 = zero 
wm_interm_78_so_pt4 = zero 
wm_interm_79_so_pt4 = zero 
wm_interm_80_so_pt4 = zero 
wm_interm_81_so_pt4 = zero 
wm_interm_82_so_pt4 = zero 
wm_interm_83_so_pt4 = zero 
wm_interm_84_so_pt4 = zero 
wm_interm_85_so_pt4 = zero 
wm_interm_86_so_pt4 = zero 
wm_interm_87_so_pt4 = zero 
wm_interm_88_so_pt4 = zero 
wm_interm_89_so_pt4 = zero 
wm_interm_90_so_pt4 = zero 
wm_interm_91_so_pt4 = zero 
wm_interm_92_so_pt4 = zero 
wm_interm_93_so_pt4 = zero 
wm_interm_94_so_pt4 = zero 
wm_interm_95_so_pt4 = zero 
wm_interm_96_so_pt4 = zero 
wm_interm_97_so_pt4 = zero 
wm_interm_98_so_pt4 = zero 
wm_interm_99_so_pt4 = zero 
wm_interm_100_so_pt4 = zero 
wm_interm_101_so_pt4 = zero 
wm_interm_102_so_pt4 = zero 
wm_interm_103_so_pt4 = zero 
wm_interm_104_so_pt4 = zero 
wm_interm_105_so_pt4 = zero 
wm_interm_106_so_pt4 = zero 
wm_interm_107_so_pt4 = zero 
wm_interm_108_so_pt4 = zero 
wm_interm_109_so_pt4 = zero 
wm_interm_110_so_pt4 = zero 
wm_interm_111_so_pt4 = zero 
wm_interm_112_so_pt4 = zero 
wm_interm_113_so_pt4 = zero 
wm_interm_114_so_pt4 = zero 

    end subroutine wm_so_intermediates_ccsd_init_pt4
    
    subroutine wm_so_intermediates_ccsd_free_pt4
    deallocate(wm_interm_0_so_pt4)
deallocate(wm_interm_1_so_pt4)
deallocate(wm_interm_2_so_pt4)
deallocate(wm_interm_3_so_pt4)
deallocate(wm_interm_4_so_pt4)
deallocate(wm_interm_5_so_pt4)
deallocate(wm_interm_6_so_pt4)
deallocate(wm_interm_7_so_pt4)
deallocate(wm_interm_8_so_pt4)
deallocate(wm_interm_9_so_pt4)
deallocate(wm_interm_10_so_pt4)
deallocate(wm_interm_11_so_pt4)
deallocate(wm_interm_12_so_pt4)
deallocate(wm_interm_13_so_pt4)
deallocate(wm_interm_14_so_pt4)
deallocate(wm_interm_15_so_pt4)
deallocate(wm_interm_16_so_pt4)
deallocate(wm_interm_17_so_pt4)
deallocate(wm_interm_18_so_pt4)
deallocate(wm_interm_19_so_pt4)
deallocate(wm_interm_20_so_pt4)
deallocate(wm_interm_21_so_pt4)
deallocate(wm_interm_22_so_pt4)
deallocate(wm_interm_23_so_pt4)
deallocate(wm_interm_24_so_pt4)
deallocate(wm_interm_25_so_pt4)
deallocate(wm_interm_26_so_pt4)
deallocate(wm_interm_27_so_pt4)
deallocate(wm_interm_28_so_pt4)
deallocate(wm_interm_29_so_pt4)
deallocate(wm_interm_30_so_pt4)
deallocate(wm_interm_31_so_pt4)
deallocate(wm_interm_32_so_pt4)
deallocate(wm_interm_33_so_pt4)
deallocate(wm_interm_34_so_pt4)
deallocate(wm_interm_35_so_pt4)
deallocate(wm_interm_36_so_pt4)
deallocate(wm_interm_37_so_pt4)
deallocate(wm_interm_38_so_pt4)
deallocate(wm_interm_39_so_pt4)
deallocate(wm_interm_40_so_pt4)
deallocate(wm_interm_41_so_pt4)
deallocate(wm_interm_42_so_pt4)
deallocate(wm_interm_43_so_pt4)
deallocate(wm_interm_44_so_pt4)
deallocate(wm_interm_45_so_pt4)
deallocate(wm_interm_46_so_pt4)
deallocate(wm_interm_47_so_pt4)
deallocate(wm_interm_48_so_pt4)
deallocate(wm_interm_49_so_pt4)
deallocate(wm_interm_50_so_pt4)
deallocate(wm_interm_51_so_pt4)
deallocate(wm_interm_52_so_pt4)
deallocate(wm_interm_53_so_pt4)
deallocate(wm_interm_54_so_pt4)
deallocate(wm_interm_55_so_pt4)
deallocate(wm_interm_56_so_pt4)
deallocate(wm_interm_57_so_pt4)
deallocate(wm_interm_58_so_pt4)
deallocate(wm_interm_59_so_pt4)
deallocate(wm_interm_60_so_pt4)
deallocate(wm_interm_61_so_pt4)
deallocate(wm_interm_62_so_pt4)
deallocate(wm_interm_63_so_pt4)
deallocate(wm_interm_64_so_pt4)
deallocate(wm_interm_65_so_pt4)
deallocate(wm_interm_66_so_pt4)
deallocate(wm_interm_67_so_pt4)
deallocate(wm_interm_68_so_pt4)
deallocate(wm_interm_69_so_pt4)
deallocate(wm_interm_70_so_pt4)
deallocate(wm_interm_71_so_pt4)
deallocate(wm_interm_72_so_pt4)
deallocate(wm_interm_73_so_pt4)
deallocate(wm_interm_74_so_pt4)
deallocate(wm_interm_75_so_pt4)
deallocate(wm_interm_76_so_pt4)
deallocate(wm_interm_77_so_pt4)
deallocate(wm_interm_78_so_pt4)
deallocate(wm_interm_79_so_pt4)
deallocate(wm_interm_80_so_pt4)
deallocate(wm_interm_81_so_pt4)
deallocate(wm_interm_82_so_pt4)
deallocate(wm_interm_83_so_pt4)
deallocate(wm_interm_84_so_pt4)
deallocate(wm_interm_85_so_pt4)
deallocate(wm_interm_86_so_pt4)
deallocate(wm_interm_87_so_pt4)
deallocate(wm_interm_88_so_pt4)
deallocate(wm_interm_89_so_pt4)
deallocate(wm_interm_90_so_pt4)
deallocate(wm_interm_91_so_pt4)
deallocate(wm_interm_92_so_pt4)
deallocate(wm_interm_93_so_pt4)
deallocate(wm_interm_94_so_pt4)
deallocate(wm_interm_95_so_pt4)
deallocate(wm_interm_96_so_pt4)
deallocate(wm_interm_97_so_pt4)
deallocate(wm_interm_98_so_pt4)
deallocate(wm_interm_99_so_pt4)
deallocate(wm_interm_100_so_pt4)
deallocate(wm_interm_101_so_pt4)
deallocate(wm_interm_102_so_pt4)
deallocate(wm_interm_103_so_pt4)
deallocate(wm_interm_104_so_pt4)
deallocate(wm_interm_105_so_pt4)
deallocate(wm_interm_106_so_pt4)
deallocate(wm_interm_107_so_pt4)
deallocate(wm_interm_108_so_pt4)
deallocate(wm_interm_109_so_pt4)
deallocate(wm_interm_110_so_pt4)
deallocate(wm_interm_111_so_pt4)
deallocate(wm_interm_112_so_pt4)
deallocate(wm_interm_113_so_pt4)
deallocate(wm_interm_114_so_pt4)

    end subroutine wm_so_intermediates_ccsd_free_pt4
    
    subroutine wm_so_intermediates_ccsd_pt4(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr)
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
wm_interm_0_so_pt4(a, b) = wm_interm_0_so_pt4(a, b) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_1_so_pt4(b, c, j, k) = wm_interm_1_so_pt4(b, c, j, k) + sum 
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
wm_interm_2_so_pt4(b, j) = wm_interm_2_so_pt4(b, j) + sum 
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
wm_interm_3_so_pt4(b, i, j, k) = wm_interm_3_so_pt4(b, i, j, k) + sum 
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
wm_interm_4_so_pt4(i, j) = wm_interm_4_so_pt4(i, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_5_so_pt4(b, c, j, k) = wm_interm_5_so_pt4(b, c, j, k) + sum 
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
wm_interm_6_so_pt4(b, j) = wm_interm_6_so_pt4(b, j) + sum 
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
wm_interm_7_so_pt4(b, j) = wm_interm_7_so_pt4(b, j) + sum 
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
wm_interm_8_so_pt4(i, j) = wm_interm_8_so_pt4(i, j) + sum 
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
wm_interm_9_so_pt4(i, j, k, l) = wm_interm_9_so_pt4(i, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_10_so_pt4(b, c, j, k) = wm_interm_10_so_pt4(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_11_so_pt4(b, c, j, k) = wm_interm_11_so_pt4(b, c, j, k) + sum 
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
wm_interm_12_so_pt4(b, i, j, k) = wm_interm_12_so_pt4(b, i, j, k) + sum 
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
wm_interm_13_so_pt4(b, i, j, k) = wm_interm_13_so_pt4(b, i, j, k) + sum 
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
wm_interm_14_so_pt4(b, j) = wm_interm_14_so_pt4(b, j) + sum 
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
wm_interm_15_so_pt4(a, b) = wm_interm_15_so_pt4(a, b) + sum 
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
wm_interm_16_so_pt4(b, j) = wm_interm_16_so_pt4(b, j) + sum 
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
wm_interm_17_so_pt4(j, k) = wm_interm_17_so_pt4(j, k) + sum 
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
wm_interm_18_so_pt4(j, k) = wm_interm_18_so_pt4(j, k) + sum 
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
wm_interm_19_so_pt4(b, c) = wm_interm_19_so_pt4(b, c) + sum 
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
wm_interm_20_so_pt4(b, c) = wm_interm_20_so_pt4(b, c) + sum 
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
wm_interm_21_so_pt4(i, j) = wm_interm_21_so_pt4(i, j) + sum 
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
wm_interm_22_so_pt4(b, j, i, k) = wm_interm_22_so_pt4(b, j, i, k) + sum 
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
wm_interm_23_so_pt4(a, b) = wm_interm_23_so_pt4(a, b) + sum 
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
wm_interm_24_so_pt4(b, i, j, k) = wm_interm_24_so_pt4(b, i, j, k) + sum 
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
wm_interm_25_so_pt4(i, j) = wm_interm_25_so_pt4(i, j) + sum 
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
wm_interm_26_so_pt4(b, i, j, k) = wm_interm_26_so_pt4(b, i, j, k) + sum 
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
wm_interm_27_so_pt4(b, i, j, k) = wm_interm_27_so_pt4(b, i, j, k) + sum 
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
wm_interm_28_so_pt4(b, i, j, k) = wm_interm_28_so_pt4(b, i, j, k) + sum 
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
wm_interm_29_so_pt4(b, i, j, k) = wm_interm_29_so_pt4(b, i, j, k) + sum 
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
wm_interm_30_so_pt4(b, j, i, k) = wm_interm_30_so_pt4(b, j, i, k) + sum 
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
wm_interm_31_so_pt4(b, i, j, k) = wm_interm_31_so_pt4(b, i, j, k) + sum 
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
wm_interm_32_so_pt4(a, b, i, j) = wm_interm_32_so_pt4(a, b, i, j) + sum 
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
wm_interm_33_so_pt4(b, j) = wm_interm_33_so_pt4(b, j) + sum 
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
wm_interm_34_so_pt4(b, j) = wm_interm_34_so_pt4(b, j) + sum 
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
wm_interm_35_so_pt4(b, j) = wm_interm_35_so_pt4(b, j) + sum 
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
wm_interm_36_so_pt4(b, j) = wm_interm_36_so_pt4(b, j) + sum 
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
wm_interm_37_so_pt4(b, j) = wm_interm_37_so_pt4(b, j) + sum 
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
wm_interm_38_so_pt4(b, j) = wm_interm_38_so_pt4(b, j) + sum 
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
wm_interm_39_so_pt4(a, b, i, j) = wm_interm_39_so_pt4(a, b, i, j) + sum 
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
sum = sum + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, b,j,a,i)
end do 
end do 
wm_interm_40_so_pt4(b, j) = wm_interm_40_so_pt4(b, j) + sum 
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
wm_interm_41_so_pt4(b, j) = wm_interm_41_so_pt4(b, j) + sum 
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
wm_interm_42_so_pt4(b, j) = wm_interm_42_so_pt4(b, j) + sum 
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
wm_interm_43_so_pt4(b, j) = wm_interm_43_so_pt4(b, j) + sum 
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
wm_interm_44_so_pt4(a, b) = wm_interm_44_so_pt4(a, b) + sum 
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
wm_interm_45_so_pt4(b, j) = wm_interm_45_so_pt4(b, j) + sum 
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
wm_interm_46_so_pt4(b, i, j, k) = wm_interm_46_so_pt4(b, i, j, k) + sum 
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
wm_interm_47_so_pt4(b, j, i, k) = wm_interm_47_so_pt4(b, j, i, k) + sum 
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
wm_interm_48_so_pt4(b, j, i, k) = wm_interm_48_so_pt4(b, j, i, k) + sum 
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
wm_interm_49_so_pt4(b, i, j, k) = wm_interm_49_so_pt4(b, i, j, k) + sum 
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
wm_interm_50_so_pt4(b, j) = wm_interm_50_so_pt4(b, j) + sum 
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
wm_interm_51_so_pt4(b, j) = wm_interm_51_so_pt4(b, j) + sum 
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
wm_interm_52_so_pt4(a, b, i, j) = wm_interm_52_so_pt4(a, b, i, j) + sum 
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
wm_interm_53_so_pt4(a, b, i, j) = wm_interm_53_so_pt4(a, b, i, j) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_54_so_pt4(b, c, j, k) = wm_interm_54_so_pt4(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_55_so_pt4(b, c, j, k) = wm_interm_55_so_pt4(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_56_so_pt4(b, c, j, k) = wm_interm_56_so_pt4(b, c, j, k) + sum 
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
wm_interm_57_so_pt4(b, c) = wm_interm_57_so_pt4(b, c) + sum 
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
wm_interm_58_so_pt4(b, c) = wm_interm_58_so_pt4(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_59_so_pt4(b, c, j, k) = wm_interm_59_so_pt4(b, c, j, k) + sum 
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
wm_interm_60_so_pt4(j, k) = wm_interm_60_so_pt4(j, k) + sum 
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
wm_interm_61_so_pt4(i, j, k, l) = wm_interm_61_so_pt4(i, j, k, l) + sum 
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
wm_interm_62_so_pt4(j, k) = wm_interm_62_so_pt4(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_63_so_pt4(b, c, j, k) = wm_interm_63_so_pt4(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_64_so_pt4(b, c, j, k) = wm_interm_64_so_pt4(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_65_so_pt4(b, c, j, k) = wm_interm_65_so_pt4(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_66_so_pt4(b, c, j, k) = wm_interm_66_so_pt4(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
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
wm_interm_67_so_pt4(b, c, j, k) = wm_interm_67_so_pt4(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_68_so_pt4(b, c, j, k) = wm_interm_68_so_pt4(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_69_so_pt4(b, c, j, k) = wm_interm_69_so_pt4(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_70_so_pt4(b, c, j, k) = wm_interm_70_so_pt4(b, c, j, k) + sum 
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
wm_interm_71_so_pt4(i, j, k, l) = wm_interm_71_so_pt4(i, j, k, l) + sum 
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
wm_interm_72_so_pt4(i, j, k, l) = wm_interm_72_so_pt4(i, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_73_so_pt4(b, c, j, k) = wm_interm_73_so_pt4(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_74_so_pt4(b, c, j, k) = wm_interm_74_so_pt4(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_75_so_pt4(b, c, j, k) = wm_interm_75_so_pt4(b, c, j, k) + sum 
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
wm_interm_76_so_pt4(j, k) = wm_interm_76_so_pt4(j, k) + sum 
end do 
end do 
!$omp end do nowait 
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
wm_interm_77_so_pt4(b, c, j, k) = wm_interm_77_so_pt4(b, c, j, k) + sum 
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
wm_interm_78_so_pt4(j, k) = wm_interm_78_so_pt4(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_79_so_pt4(b, c, j, k) = wm_interm_79_so_pt4(b, c, j, k) + sum 
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
wm_interm_80_so_pt4(j, k) = wm_interm_80_so_pt4(j, k) + sum 
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
wm_interm_81_so_pt4(b, c) = wm_interm_81_so_pt4(b, c) + sum 
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
wm_interm_82_so_pt4(b, c) = wm_interm_82_so_pt4(b, c) + sum 
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
wm_interm_83_so_pt4(b, c) = wm_interm_83_so_pt4(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_84_so_pt4(b, c, j, k) = wm_interm_84_so_pt4(b, c, j, k) + sum 
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
wm_interm_85_so_pt4(j, k) = wm_interm_85_so_pt4(j, k) + sum 
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
wm_interm_86_so_pt4(j, k) = wm_interm_86_so_pt4(j, k) + sum 
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
wm_interm_87_so_pt4(j, k) = wm_interm_87_so_pt4(j, k) + sum 
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
wm_interm_88_so_pt4(i, j, k, l) = wm_interm_88_so_pt4(i, j, k, l) + sum 
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
wm_interm_89_so_pt4(b, i, k, j) = wm_interm_89_so_pt4(b, i, k, j) + sum 
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
wm_interm_90_so_pt4(b, c) = wm_interm_90_so_pt4(b, c) + sum 
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
wm_interm_91_so_pt4(b, c) = wm_interm_91_so_pt4(b, c) + sum 
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
wm_interm_92_so_pt4(b, c) = wm_interm_92_so_pt4(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_93_so_pt4(b, c, j, k) = wm_interm_93_so_pt4(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_94_so_pt4(b, c, j, k) = wm_interm_94_so_pt4(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_95_so_pt4(b, c, j, k) = wm_interm_95_so_pt4(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_96_so_pt4(b, c, j, k) = wm_interm_96_so_pt4(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_97_so_pt4(b, c, j, k) = wm_interm_97_so_pt4(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_98_so_pt4(b, c, j, k) = wm_interm_98_so_pt4(b, c, j, k) + sum 
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
wm_interm_99_so_pt4(i, j, k, l) = wm_interm_99_so_pt4(i, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_100_so_pt4(b, c, j, k) = wm_interm_100_so_pt4(b, c, j, k) + sum 
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
wm_interm_101_so_pt4(j, k) = wm_interm_101_so_pt4(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_102_so_pt4(b, c, j, k) = wm_interm_102_so_pt4(b, c, j, k) + sum 
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
wm_interm_103_so_pt4(j, k) = wm_interm_103_so_pt4(j, k) + sum 
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
wm_interm_104_so_pt4(b, c) = wm_interm_104_so_pt4(b, c) + sum 
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
wm_interm_105_so_pt4(b, c) = wm_interm_105_so_pt4(b, c) + sum 
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
wm_interm_106_so_pt4(j, k) = wm_interm_106_so_pt4(j, k) + sum 
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
wm_interm_107_so_pt4(j, k) = wm_interm_107_so_pt4(j, k) + sum 
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
wm_interm_108_so_pt4(j, k) = wm_interm_108_so_pt4(j, k) + sum 
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
wm_interm_109_so_pt4(i, j, k, l) = wm_interm_109_so_pt4(i, j, k, l) + sum 
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
wm_interm_110_so_pt4(b, c) = wm_interm_110_so_pt4(b, c) + sum 
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
wm_interm_111_so_pt4(b, c) = wm_interm_111_so_pt4(b, c) + sum 
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
wm_interm_112_so_pt4(b, c) = wm_interm_112_so_pt4(b, c) + sum 
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
wm_interm_113_so_pt4(b, i, j, k) = wm_interm_113_so_pt4(b, i, j, k) + sum 
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
sum = sum + s1(a,i) * t1(b,j)
wm_interm_114_so_pt4(a, b, i, j) = wm_interm_114_so_pt4(a, b, i, j) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 



    end subroutine wm_so_intermediates_ccsd_pt4
    
    
    function calc_D_oo_wm_so_pt4(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_oo_wm_so_pt4
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , a, i, j, b, k 
    real(F64), dimension(0:811) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do a = nocc + 1, nactive 
term(0) = term(0) + wm_interm_33_so_pt4(a,q) * wm_interm_50_so_pt4(a,p)
term(1) = term(1) + wm_interm_34_so_pt4(a,q) * wm_interm_50_so_pt4(a,p)
term(2) = term(2) + wm_interm_35_so_pt4(a,q) * wm_interm_50_so_pt4(a,p)
term(3) = term(3) + wm_interm_33_so_pt4(a,q) * wm_interm_51_so_pt4(a,p)
term(4) = term(4) + wm_interm_34_so_pt4(a,q) * wm_interm_51_so_pt4(a,p)
term(5) = term(5) + wm_interm_35_so_pt4(a,q) * wm_interm_51_so_pt4(a,p)
term(6) = term(6) + wm_interm_36_so_pt4(a,q) * wm_interm_50_so_pt4(a,p)
term(7) = term(7) + wm_interm_37_so_pt4(a,q) * wm_interm_50_so_pt4(a,p)
term(8) = term(8) + wm_interm_36_so_pt4(a,q) * wm_interm_51_so_pt4(a,p)
term(9) = term(9) + wm_interm_37_so_pt4(a,q) * wm_interm_51_so_pt4(a,p)
end do 

term(0) = term(0) * (4.0d+0) 
term(1) = term(1) * (-2.0d+0) 
term(2) = term(2) * (-2.0d+0) 
term(3) = term(3) * (-2.0d+0) 
term(6) = term(6) * (8.0d+0) 
term(7) = term(7) * (-8.0d+0) 
term(8) = term(8) * (-4.0d+0) 
term(9) = term(9) * (4.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(10) = term(10) + s1(a,i) * wm_interm_13_so_pt4(b,j,q,i) * wm_interm_67_so_pt4(a,b,j,p)
term(11) = term(11) + s1(a,i) * wm_interm_13_so_pt4(b,j,q,i) * wm_interm_68_so_pt4(a,b,j,p)
term(12) = term(12) + s1(a,i) * wm_interm_13_so_pt4(b,j,q,i) * wm_interm_74_so_pt4(a,b,j,p)
term(13) = term(13) + s1(a,i) * wm_interm_13_so_pt4(b,j,q,i) * wm_interm_69_so_pt4(a,b,j,p)
term(14) = term(14) + s1(a,i) * wm_interm_13_so_pt4(b,j,q,i) * wm_interm_73_so_pt4(a,b,j,p)
term(15) = term(15) + s1(a,i) * wm_interm_13_so_pt4(b,j,q,i) * wm_interm_84_so_pt4(a,b,j,p)
term(16) = term(16) + r1(vrdav_Rl, a,i) * wm_interm_3_so_pt4(b,j,q,i) * wm_interm_73_so_pt4(a,b,j,p)
term(17) = term(17) + r1(vrdav_Rl, a,i) * wm_interm_3_so_pt4(b,j,q,i) * wm_interm_84_so_pt4(a,b,j,p)
term(18) = term(18) + r1(vrdav_Rl, a,i) * wm_interm_3_so_pt4(b,j,q,i) * wm_interm_69_so_pt4(a,b,j,p)
term(19) = term(19) + r1(vrdav_Rl, a,i) * wm_interm_3_so_pt4(b,j,q,i) * wm_interm_74_so_pt4(a,b,j,p)
term(20) = term(20) + r1(vrdav_Rl, a,i) * wm_interm_3_so_pt4(b,j,q,i) * wm_interm_67_so_pt4(a,b,j,p)
term(21) = term(21) + r1(vrdav_Rl, a,i) * wm_interm_3_so_pt4(b,j,q,i) * wm_interm_68_so_pt4(a,b,j,p)
term(22) = term(22) + s1(a,i) * wm_interm_13_so_pt4(b,j,q,i) * wm_interm_95_so_pt4(a,b,j,p)
term(23) = term(23) + s1(a,i) * wm_interm_13_so_pt4(b,j,q,i) * wm_interm_96_so_pt4(a,b,j,p)
term(24) = term(24) + s1(a,i) * wm_interm_13_so_pt4(b,j,q,i) * wm_interm_97_so_pt4(a,b,j,p)
term(25) = term(25) + s1(a,i) * wm_interm_13_so_pt4(b,j,q,i) * wm_interm_98_so_pt4(a,b,j,p)
term(26) = term(26) + r1(vrdav_Rl, a,i) * wm_interm_3_so_pt4(b,j,q,i) * wm_interm_97_so_pt4(a,b,j,p)
term(27) = term(27) + r1(vrdav_Rl, a,i) * wm_interm_3_so_pt4(b,j,q,i) * wm_interm_98_so_pt4(a,b,j,p)
term(28) = term(28) + r1(vrdav_Rl, a,i) * wm_interm_3_so_pt4(b,j,q,i) * wm_interm_95_so_pt4(a,b,j,p)
term(29) = term(29) + r1(vrdav_Rl, a,i) * wm_interm_3_so_pt4(b,j,q,i) * wm_interm_96_so_pt4(a,b,j,p)
term(30) = term(30) + t1(a,i) * wm_interm_12_so_pt4(b,j,p,i) * wm_interm_55_so_pt4(a,b,j,q)
term(31) = term(31) + t1(a,i) * wm_interm_12_so_pt4(b,j,p,i) * wm_interm_54_so_pt4(a,b,j,q)
term(32) = term(32) + t1(a,i) * wm_interm_12_so_pt4(b,j,p,i) * wm_interm_59_so_pt4(a,b,j,q)
term(33) = term(33) + t1(a,i) * wm_interm_12_so_pt4(b,j,p,i) * wm_interm_56_so_pt4(a,b,j,q)
end do 
end do 
end do 
end do 

term(10) = term(10) * (2.0d+0) 
term(11) = term(11) * (-4.0d+0) 
term(12) = term(12) * (2.0d+0) 
term(13) = term(13) * (2.0d+0) 
term(14) = term(14) * (2.0d+0) 
term(15) = term(15) * (-4.0d+0) 
term(16) = term(16) * (2.0d+0) 
term(17) = term(17) * (-4.0d+0) 
term(18) = term(18) * (2.0d+0) 
term(19) = term(19) * (2.0d+0) 
term(20) = term(20) * (2.0d+0) 
term(21) = term(21) * (-4.0d+0) 
term(22) = term(22) * (8.0d+0) 
term(23) = term(23) * (-8.0d+0) 
term(24) = term(24) * (8.0d+0) 
term(25) = term(25) * (-8.0d+0) 
term(26) = term(26) * (8.0d+0) 
term(27) = term(27) * (-8.0d+0) 
term(28) = term(28) * (8.0d+0) 
term(29) = term(29) * (-8.0d+0) 
term(30) = term(30) * (-2.0d+0) 
term(31) = term(31) * (4.0d+0) 
term(32) = term(32) * (-2.0d+0) 
term(33) = term(33) * (4.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(34) = term(34) + s1(a,p) * wm_interm_17_so_pt4(i,j) * wm_interm_24_so_pt4(a,j,i,q)
term(35) = term(35) + s1(a,p) * wm_interm_17_so_pt4(i,j) * wm_interm_24_so_pt4(a,j,q,i)
term(36) = term(36) + s1(a,p) * wm_interm_18_so_pt4(i,j) * wm_interm_24_so_pt4(a,j,i,q)
term(37) = term(37) + s1(a,p) * wm_interm_18_so_pt4(i,j) * wm_interm_24_so_pt4(a,j,q,i)
term(38) = term(38) + s1(a,p) * wm_interm_17_so_pt4(i,j) * wm_interm_26_so_pt4(a,j,q,i)
term(39) = term(39) + s1(a,p) * wm_interm_18_so_pt4(i,j) * wm_interm_26_so_pt4(a,j,q,i)
term(40) = term(40) + s1(a,p) * wm_interm_17_so_pt4(i,j) * wm_interm_29_so_pt4(a,j,q,i)
term(41) = term(41) + s1(a,p) * wm_interm_17_so_pt4(i,j) * wm_interm_29_so_pt4(a,j,i,q)
term(42) = term(42) + s1(a,p) * wm_interm_18_so_pt4(i,j) * wm_interm_29_so_pt4(a,j,q,i)
term(43) = term(43) + s1(a,p) * wm_interm_18_so_pt4(i,j) * wm_interm_29_so_pt4(a,j,i,q)
term(44) = term(44) + r1(vrdav_Rl, a,i) * wm_interm_33_so_pt4(a,j) * wm_interm_9_so_pt4(p,j,i,q)
term(45) = term(45) + r1(vrdav_Rl, a,i) * wm_interm_34_so_pt4(a,j) * wm_interm_9_so_pt4(p,j,i,q)
term(46) = term(46) + r1(vrdav_Rl, a,i) * wm_interm_35_so_pt4(a,j) * wm_interm_9_so_pt4(p,j,i,q)
term(47) = term(47) + r1(vrdav_Rl, a,i) * wm_interm_36_so_pt4(a,j) * wm_interm_9_so_pt4(p,j,i,q)
term(48) = term(48) + r1(vrdav_Rl, a,i) * wm_interm_37_so_pt4(a,j) * wm_interm_9_so_pt4(p,j,i,q)
term(49) = term(49) + s1(a,i) * wm_interm_38_so_pt4(a,j) * wm_interm_9_so_pt4(j,p,q,i)
term(50) = term(50) + s1(a,i) * wm_interm_40_so_pt4(a,j) * wm_interm_9_so_pt4(j,p,q,i)
term(51) = term(51) + s1(a,i) * wm_interm_41_so_pt4(a,j) * wm_interm_9_so_pt4(j,p,q,i)
term(52) = term(52) + s1(a,i) * wm_interm_38_so_pt4(a,j) * wm_interm_9_so_pt4(p,j,i,q)
term(53) = term(53) + s1(a,i) * wm_interm_40_so_pt4(a,j) * wm_interm_9_so_pt4(p,j,i,q)
term(54) = term(54) + s1(a,i) * wm_interm_41_so_pt4(a,j) * wm_interm_9_so_pt4(p,j,i,q)
term(55) = term(55) + s1(a,i) * wm_interm_42_so_pt4(a,j) * wm_interm_9_so_pt4(j,p,q,i)
term(56) = term(56) + s1(a,i) * wm_interm_43_so_pt4(a,j) * wm_interm_9_so_pt4(j,p,q,i)
term(57) = term(57) + s1(a,i) * wm_interm_42_so_pt4(a,j) * wm_interm_9_so_pt4(p,j,i,q)
term(58) = term(58) + s1(a,i) * wm_interm_43_so_pt4(a,j) * wm_interm_9_so_pt4(p,j,i,q)
term(59) = term(59) + t1(a,q) * wm_interm_17_so_pt4(i,j) * wm_interm_46_so_pt4(a,p,j,i)
term(60) = term(60) + t1(a,q) * wm_interm_18_so_pt4(i,j) * wm_interm_46_so_pt4(a,p,j,i)
term(61) = term(61) + t1(a,q) * wm_interm_17_so_pt4(i,j) * wm_interm_48_so_pt4(a,j,p,i)
term(62) = term(62) + t1(a,q) * wm_interm_18_so_pt4(i,j) * wm_interm_48_so_pt4(a,j,p,i)
end do 
end do 
end do 

term(34) = term(34) * (-1.0d+0) 
term(35) = term(35) * (2.0d+0) 
term(36) = term(36) * (2.0d+0) 
term(37) = term(37) * (-4.0d+0) 
term(38) = term(38) * (-1.0d+0) 
term(39) = term(39) * (2.0d+0) 
term(40) = term(40) * (-4.0d+0) 
term(41) = term(41) * (4.0d+0) 
term(42) = term(42) * (8.0d+0) 
term(43) = term(43) * (-8.0d+0) 
term(44) = term(44) * (-2.0d+0) 
term(47) = term(47) * (-4.0d+0) 
term(48) = term(48) * (4.0d+0) 
term(49) = term(49) * (0.5d+0) 
term(50) = term(50) * (-1.0d+0) 
term(51) = term(51) * (0.5d+0) 
term(52) = term(52) * (1.5d+0) 
term(53) = term(53) * (-3.0d+0) 
term(54) = term(54) * (1.5d+0) 
term(55) = term(55) * (2.0d+0) 
term(56) = term(56) * (-2.0d+0) 
term(57) = term(57) * (6.0d+0) 
term(58) = term(58) * (-6.0d+0) 
term(59) = term(59) * (-1.0d+0) 
term(60) = term(60) * (2.0d+0) 
term(61) = term(61) * (-1.0d+0) 
term(62) = term(62) * (2.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(63) = term(63) + s1(a,p) * wm_interm_1_so_pt4(b,a,i,j) * wm_interm_26_so_pt4(b,j,i,q)
term(64) = term(64) + s1(a,p) * wm_interm_1_so_pt4(b,a,i,j) * wm_interm_26_so_pt4(b,j,q,i)
term(65) = term(65) + s1(a,p) * wm_interm_26_so_pt4(b,i,q,j) * wm_interm_5_so_pt4(b,a,j,i)
term(66) = term(66) + s1(a,i) * wm_interm_1_so_pt4(b,a,p,j) * wm_interm_26_so_pt4(b,j,q,i)
term(67) = term(67) + s1(a,i) * wm_interm_26_so_pt4(b,j,i,q) * wm_interm_5_so_pt4(b,a,p,j)
term(68) = term(68) + s1(a,i) * wm_interm_26_so_pt4(b,j,q,i) * wm_interm_5_so_pt4(b,a,p,j)
term(69) = term(69) + s1(a,p) * wm_interm_24_so_pt4(b,i,q,j) * wm_interm_5_so_pt4(b,a,j,i)
term(70) = term(70) + s1(a,p) * wm_interm_1_so_pt4(b,a,i,j) * wm_interm_24_so_pt4(b,j,q,i)
term(71) = term(71) + s1(a,i) * wm_interm_24_so_pt4(b,j,q,i) * wm_interm_5_so_pt4(b,a,p,j)
term(72) = term(72) + s1(a,i) * wm_interm_1_so_pt4(b,a,p,j) * wm_interm_24_so_pt4(b,j,i,q)
term(73) = term(73) + s1(a,i) * wm_interm_1_so_pt4(b,a,p,j) * wm_interm_24_so_pt4(b,j,q,i)
term(74) = term(74) + s1(a,p) * wm_interm_10_so_pt4(b,a,i,j) * wm_interm_26_so_pt4(b,j,i,q)
term(75) = term(75) + s1(a,p) * wm_interm_10_so_pt4(b,a,i,j) * wm_interm_26_so_pt4(b,j,q,i)
term(76) = term(76) + s1(a,p) * wm_interm_11_so_pt4(b,a,i,j) * wm_interm_26_so_pt4(b,j,i,q)
term(77) = term(77) + s1(a,p) * wm_interm_11_so_pt4(b,a,i,j) * wm_interm_26_so_pt4(b,j,q,i)
term(78) = term(78) + s1(a,p) * wm_interm_10_so_pt4(b,a,i,j) * wm_interm_24_so_pt4(b,j,q,i)
term(79) = term(79) + s1(a,p) * wm_interm_11_so_pt4(b,a,i,j) * wm_interm_24_so_pt4(b,j,q,i)
term(80) = term(80) + s1(a,i) * wm_interm_10_so_pt4(b,a,p,j) * wm_interm_26_so_pt4(b,j,q,i)
term(81) = term(81) + s1(a,i) * wm_interm_11_so_pt4(b,a,p,j) * wm_interm_26_so_pt4(b,j,q,i)
term(82) = term(82) + s1(a,i) * wm_interm_10_so_pt4(b,a,p,j) * wm_interm_24_so_pt4(b,j,i,q)
term(83) = term(83) + s1(a,i) * wm_interm_11_so_pt4(b,a,p,j) * wm_interm_24_so_pt4(b,j,i,q)
term(84) = term(84) + s1(a,i) * wm_interm_10_so_pt4(b,a,p,j) * wm_interm_24_so_pt4(b,j,q,i)
term(85) = term(85) + s1(a,i) * wm_interm_11_so_pt4(b,a,p,j) * wm_interm_24_so_pt4(b,j,q,i)
term(86) = term(86) + r1(vrdav_Rl, a,i) * wm_interm_1_so_pt4(b,a,p,j) * wm_interm_28_so_pt4(b,q,i,j)
term(87) = term(87) + r1(vrdav_Rl, a,i) * wm_interm_1_so_pt4(b,a,p,j) * wm_interm_27_so_pt4(b,q,i,j)
term(88) = term(88) + r1(vrdav_Rl, a,i) * wm_interm_28_so_pt4(b,q,i,j) * wm_interm_5_so_pt4(b,a,p,j)
term(89) = term(89) + r1(vrdav_Rl, a,i) * wm_interm_27_so_pt4(b,q,i,j) * wm_interm_5_so_pt4(b,a,p,j)
term(90) = term(90) + r1(vrdav_Rl, a,i) * wm_interm_10_so_pt4(b,a,p,j) * wm_interm_28_so_pt4(b,q,i,j)
term(91) = term(91) + r1(vrdav_Rl, a,i) * wm_interm_11_so_pt4(b,a,p,j) * wm_interm_28_so_pt4(b,q,i,j)
term(92) = term(92) + r1(vrdav_Rl, a,i) * wm_interm_10_so_pt4(b,a,p,j) * wm_interm_27_so_pt4(b,q,i,j)
term(93) = term(93) + r1(vrdav_Rl, a,i) * wm_interm_11_so_pt4(b,a,p,j) * wm_interm_27_so_pt4(b,q,i,j)
term(94) = term(94) + s1(a,p) * wm_interm_1_so_pt4(b,a,i,j) * wm_interm_29_so_pt4(b,j,i,q)
term(95) = term(95) + s1(a,p) * wm_interm_1_so_pt4(b,a,i,j) * wm_interm_29_so_pt4(b,j,q,i)
term(96) = term(96) + s1(a,p) * wm_interm_29_so_pt4(b,i,q,j) * wm_interm_5_so_pt4(b,a,j,i)
term(97) = term(97) + s1(a,i) * wm_interm_1_so_pt4(b,a,p,j) * wm_interm_29_so_pt4(b,j,q,i)
term(98) = term(98) + s1(a,i) * wm_interm_29_so_pt4(b,j,i,q) * wm_interm_5_so_pt4(b,a,p,j)
term(99) = term(99) + s1(a,i) * wm_interm_29_so_pt4(b,j,q,i) * wm_interm_5_so_pt4(b,a,p,j)
term(100) = term(100) + s1(a,i) * wm_interm_1_so_pt4(b,a,p,j) * wm_interm_29_so_pt4(b,j,i,q)
term(101) = term(101) + s1(a,p) * wm_interm_10_so_pt4(b,a,i,j) * wm_interm_29_so_pt4(b,j,i,q)
term(102) = term(102) + s1(a,p) * wm_interm_10_so_pt4(b,a,i,j) * wm_interm_29_so_pt4(b,j,q,i)
term(103) = term(103) + s1(a,p) * wm_interm_11_so_pt4(b,a,i,j) * wm_interm_29_so_pt4(b,j,i,q)
term(104) = term(104) + s1(a,p) * wm_interm_11_so_pt4(b,a,i,j) * wm_interm_29_so_pt4(b,j,q,i)
term(105) = term(105) + s1(a,i) * wm_interm_10_so_pt4(b,a,p,j) * wm_interm_29_so_pt4(b,j,q,i)
term(106) = term(106) + s1(a,i) * wm_interm_11_so_pt4(b,a,p,j) * wm_interm_29_so_pt4(b,j,q,i)
term(107) = term(107) + s1(a,i) * wm_interm_10_so_pt4(b,a,p,j) * wm_interm_29_so_pt4(b,j,i,q)
term(108) = term(108) + s1(a,i) * wm_interm_11_so_pt4(b,a,p,j) * wm_interm_29_so_pt4(b,j,i,q)
term(109) = term(109) + r1(vrdav_Rl, a,i) * wm_interm_1_so_pt4(b,a,p,j) * wm_interm_31_so_pt4(b,q,i,j)
term(110) = term(110) + r1(vrdav_Rl, a,i) * wm_interm_1_so_pt4(b,a,p,j) * wm_interm_30_so_pt4(b,q,i,j)
term(111) = term(111) + r1(vrdav_Rl, a,i) * wm_interm_31_so_pt4(b,q,i,j) * wm_interm_5_so_pt4(b,a,p,j)
term(112) = term(112) + r1(vrdav_Rl, a,i) * wm_interm_30_so_pt4(b,q,i,j) * wm_interm_5_so_pt4(b,a,p,j)
term(113) = term(113) + r1(vrdav_Rl, a,i) * wm_interm_10_so_pt4(b,a,p,j) * wm_interm_31_so_pt4(b,q,i,j)
term(114) = term(114) + r1(vrdav_Rl, a,i) * wm_interm_11_so_pt4(b,a,p,j) * wm_interm_31_so_pt4(b,q,i,j)
term(115) = term(115) + r1(vrdav_Rl, a,i) * wm_interm_10_so_pt4(b,a,p,j) * wm_interm_30_so_pt4(b,q,i,j)
term(116) = term(116) + r1(vrdav_Rl, a,i) * wm_interm_11_so_pt4(b,a,p,j) * wm_interm_30_so_pt4(b,q,i,j)
term(117) = term(117) + s1(a,i) * wm_interm_1_so_pt4(b,a,j,q) * wm_interm_24_so_pt4(b,p,i,j)
term(118) = term(118) + s1(a,i) * wm_interm_1_so_pt4(b,a,j,q) * wm_interm_26_so_pt4(b,p,i,j)
term(119) = term(119) + s1(a,i) * wm_interm_26_so_pt4(b,p,i,j) * wm_interm_5_so_pt4(b,a,j,q)
term(120) = term(120) + s1(a,i) * wm_interm_10_so_pt4(b,a,j,q) * wm_interm_24_so_pt4(b,p,i,j)
term(121) = term(121) + s1(a,i) * wm_interm_10_so_pt4(b,a,j,q) * wm_interm_26_so_pt4(b,p,i,j)
term(122) = term(122) + s1(a,i) * wm_interm_11_so_pt4(b,a,j,q) * wm_interm_24_so_pt4(b,p,i,j)
term(123) = term(123) + s1(a,i) * wm_interm_11_so_pt4(b,a,j,q) * wm_interm_26_so_pt4(b,p,i,j)
term(124) = term(124) + s1(a,i) * wm_interm_1_so_pt4(b,a,j,q) * wm_interm_29_so_pt4(b,p,i,j)
term(125) = term(125) + s1(a,i) * wm_interm_29_so_pt4(b,p,i,j) * wm_interm_5_so_pt4(b,a,j,q)
term(126) = term(126) + s1(a,i) * wm_interm_10_so_pt4(b,a,j,q) * wm_interm_29_so_pt4(b,p,i,j)
term(127) = term(127) + s1(a,i) * wm_interm_11_so_pt4(b,a,j,q) * wm_interm_29_so_pt4(b,p,i,j)
end do 
end do 
end do 
end do 

term(63) = term(63) * (2.0d+0) 
term(64) = term(64) * (-4.0d+0) 
term(65) = term(65) * (2.0d+0) 
term(66) = term(66) * (2.0d+0) 
term(67) = term(67) * (2.0d+0) 
term(68) = term(68) * (-4.0d+0) 
term(69) = term(69) * (-4.0d+0) 
term(70) = term(70) * (2.0d+0) 
term(71) = term(71) * (2.0d+0) 
term(72) = term(72) * (2.0d+0) 
term(73) = term(73) * (-4.0d+0) 
term(74) = term(74) * (2.0d+0) 
term(75) = term(75) * (-4.0d+0) 
term(76) = term(76) * (-4.0d+0) 
term(77) = term(77) * (8.0d+0) 
term(78) = term(78) * (2.0d+0) 
term(79) = term(79) * (-4.0d+0) 
term(80) = term(80) * (2.0d+0) 
term(81) = term(81) * (-4.0d+0) 
term(82) = term(82) * (2.0d+0) 
term(83) = term(83) * (-4.0d+0) 
term(84) = term(84) * (-4.0d+0) 
term(85) = term(85) * (8.0d+0) 
term(86) = term(86) * (2.0d+0) 
term(87) = term(87) * (-4.0d+0) 
term(88) = term(88) * (-4.0d+0) 
term(89) = term(89) * (2.0d+0) 
term(90) = term(90) * (2.0d+0) 
term(91) = term(91) * (-4.0d+0) 
term(92) = term(92) * (-4.0d+0) 
term(93) = term(93) * (8.0d+0) 
term(94) = term(94) * (8.0d+0) 
term(95) = term(95) * (-8.0d+0) 
term(96) = term(96) * (8.0d+0) 
term(97) = term(97) * (8.0d+0) 
term(98) = term(98) * (8.0d+0) 
term(99) = term(99) * (-8.0d+0) 
term(100) = term(100) * (-8.0d+0) 
term(101) = term(101) * (8.0d+0) 
term(102) = term(102) * (-8.0d+0) 
term(103) = term(103) * (-16.0d+0) 
term(104) = term(104) * (16.0d+0) 
term(105) = term(105) * (8.0d+0) 
term(106) = term(106) * (-16.0d+0) 
term(107) = term(107) * (-8.0d+0) 
term(108) = term(108) * (16.0d+0) 
term(109) = term(109) * (4.0d+0) 
term(110) = term(110) * (-8.0d+0) 
term(111) = term(111) * (-8.0d+0) 
term(112) = term(112) * (4.0d+0) 
term(113) = term(113) * (4.0d+0) 
term(114) = term(114) * (-8.0d+0) 
term(115) = term(115) * (-8.0d+0) 
term(116) = term(116) * (16.0d+0) 
term(117) = term(117) * (2.0d+0) 
term(118) = term(118) * (-4.0d+0) 
term(119) = term(119) * (2.0d+0) 
term(120) = term(120) * (2.0d+0) 
term(121) = term(121) * (-4.0d+0) 
term(122) = term(122) * (-4.0d+0) 
term(123) = term(123) * (8.0d+0) 
term(124) = term(124) * (-8.0d+0) 
term(125) = term(125) * (8.0d+0) 
term(126) = term(126) * (-8.0d+0) 
term(127) = term(127) * (16.0d+0) 

do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(128) = term(128) + s2(a,b,i,p) * wm_interm_14_so_pt4(b,i) * wm_interm_33_so_pt4(a,q)
term(129) = term(129) + s2(a,b,i,p) * wm_interm_14_so_pt4(b,i) * wm_interm_34_so_pt4(a,q)
term(130) = term(130) + s2(a,b,i,p) * wm_interm_14_so_pt4(b,i) * wm_interm_35_so_pt4(a,q)
term(131) = term(131) + s2(a,b,i,p) * wm_interm_16_so_pt4(b,i) * wm_interm_33_so_pt4(a,q)
term(132) = term(132) + s2(a,b,i,p) * wm_interm_16_so_pt4(b,i) * wm_interm_34_so_pt4(a,q)
term(133) = term(133) + s2(a,b,i,p) * wm_interm_16_so_pt4(b,i) * wm_interm_35_so_pt4(a,q)
term(134) = term(134) + s2(a,b,i,p) * wm_interm_14_so_pt4(b,i) * wm_interm_36_so_pt4(a,q)
term(135) = term(135) + s2(a,b,i,p) * wm_interm_14_so_pt4(b,i) * wm_interm_37_so_pt4(a,q)
term(136) = term(136) + s2(a,b,i,p) * wm_interm_16_so_pt4(b,i) * wm_interm_36_so_pt4(a,q)
term(137) = term(137) + s2(a,b,i,p) * wm_interm_16_so_pt4(b,i) * wm_interm_37_so_pt4(a,q)
term(138) = term(138) + r1(vrdav_Rl, a,i) * wm_interm_67_so_pt4(a,b,q,p) * wm_interm_6_so_pt4(b,i)
term(139) = term(139) + r1(vrdav_Rl, a,i) * wm_interm_68_so_pt4(a,b,q,p) * wm_interm_6_so_pt4(b,i)
term(140) = term(140) + r1(vrdav_Rl, a,i) * wm_interm_6_so_pt4(b,i) * wm_interm_74_so_pt4(a,b,q,p)
term(141) = term(141) + r1(vrdav_Rl, a,i) * wm_interm_69_so_pt4(a,b,q,p) * wm_interm_6_so_pt4(b,i)
term(142) = term(142) + r1(vrdav_Rl, a,i) * wm_interm_6_so_pt4(b,i) * wm_interm_73_so_pt4(a,b,q,p)
term(143) = term(143) + r1(vrdav_Rl, a,i) * wm_interm_6_so_pt4(b,i) * wm_interm_84_so_pt4(a,b,q,p)
term(144) = term(144) + r1(vrdav_Rl, a,i) * wm_interm_67_so_pt4(a,b,q,p) * wm_interm_7_so_pt4(b,i)
term(145) = term(145) + r1(vrdav_Rl, a,i) * wm_interm_68_so_pt4(a,b,q,p) * wm_interm_7_so_pt4(b,i)
term(146) = term(146) + r1(vrdav_Rl, a,i) * wm_interm_69_so_pt4(a,b,q,p) * wm_interm_7_so_pt4(b,i)
term(147) = term(147) + r1(vrdav_Rl, a,i) * wm_interm_74_so_pt4(a,b,q,p) * wm_interm_7_so_pt4(b,i)
term(148) = term(148) + r1(vrdav_Rl, a,i) * wm_interm_73_so_pt4(a,b,q,p) * wm_interm_7_so_pt4(b,i)
term(149) = term(149) + r1(vrdav_Rl, a,i) * wm_interm_7_so_pt4(b,i) * wm_interm_84_so_pt4(a,b,q,p)
term(150) = term(150) + s1(a,i) * wm_interm_14_so_pt4(b,i) * wm_interm_67_so_pt4(a,b,q,p)
term(151) = term(151) + s1(a,i) * wm_interm_14_so_pt4(b,i) * wm_interm_68_so_pt4(a,b,q,p)
term(152) = term(152) + s1(a,i) * wm_interm_14_so_pt4(b,i) * wm_interm_74_so_pt4(a,b,q,p)
term(153) = term(153) + s1(a,i) * wm_interm_14_so_pt4(b,i) * wm_interm_69_so_pt4(a,b,q,p)
term(154) = term(154) + s1(a,i) * wm_interm_14_so_pt4(b,i) * wm_interm_73_so_pt4(a,b,q,p)
term(155) = term(155) + s1(a,i) * wm_interm_14_so_pt4(b,i) * wm_interm_84_so_pt4(a,b,q,p)
term(156) = term(156) + s1(a,i) * wm_interm_16_so_pt4(b,i) * wm_interm_67_so_pt4(a,b,q,p)
term(157) = term(157) + s1(a,i) * wm_interm_16_so_pt4(b,i) * wm_interm_68_so_pt4(a,b,q,p)
term(158) = term(158) + s1(a,i) * wm_interm_16_so_pt4(b,i) * wm_interm_69_so_pt4(a,b,q,p)
term(159) = term(159) + s1(a,i) * wm_interm_16_so_pt4(b,i) * wm_interm_74_so_pt4(a,b,q,p)
term(160) = term(160) + s1(a,i) * wm_interm_16_so_pt4(b,i) * wm_interm_73_so_pt4(a,b,q,p)
term(161) = term(161) + s1(a,i) * wm_interm_16_so_pt4(b,i) * wm_interm_84_so_pt4(a,b,q,p)
term(162) = term(162) + r1(vrdav_Rl, a,i) * wm_interm_6_so_pt4(b,i) * wm_interm_95_so_pt4(a,b,q,p)
term(163) = term(163) + r1(vrdav_Rl, a,i) * wm_interm_6_so_pt4(b,i) * wm_interm_96_so_pt4(a,b,q,p)
term(164) = term(164) + r1(vrdav_Rl, a,i) * wm_interm_6_so_pt4(b,i) * wm_interm_97_so_pt4(a,b,q,p)
term(165) = term(165) + r1(vrdav_Rl, a,i) * wm_interm_6_so_pt4(b,i) * wm_interm_98_so_pt4(a,b,q,p)
term(166) = term(166) + r1(vrdav_Rl, a,i) * wm_interm_7_so_pt4(b,i) * wm_interm_95_so_pt4(a,b,q,p)
term(167) = term(167) + r1(vrdav_Rl, a,i) * wm_interm_7_so_pt4(b,i) * wm_interm_96_so_pt4(a,b,q,p)
term(168) = term(168) + r1(vrdav_Rl, a,i) * wm_interm_7_so_pt4(b,i) * wm_interm_97_so_pt4(a,b,q,p)
term(169) = term(169) + r1(vrdav_Rl, a,i) * wm_interm_7_so_pt4(b,i) * wm_interm_98_so_pt4(a,b,q,p)
term(170) = term(170) + s1(a,i) * wm_interm_14_so_pt4(b,i) * wm_interm_95_so_pt4(a,b,q,p)
term(171) = term(171) + s1(a,i) * wm_interm_14_so_pt4(b,i) * wm_interm_96_so_pt4(a,b,q,p)
term(172) = term(172) + s1(a,i) * wm_interm_14_so_pt4(b,i) * wm_interm_97_so_pt4(a,b,q,p)
term(173) = term(173) + s1(a,i) * wm_interm_14_so_pt4(b,i) * wm_interm_98_so_pt4(a,b,q,p)
term(174) = term(174) + s1(a,i) * wm_interm_16_so_pt4(b,i) * wm_interm_95_so_pt4(a,b,q,p)
term(175) = term(175) + s1(a,i) * wm_interm_16_so_pt4(b,i) * wm_interm_96_so_pt4(a,b,q,p)
term(176) = term(176) + s1(a,i) * wm_interm_16_so_pt4(b,i) * wm_interm_97_so_pt4(a,b,q,p)
term(177) = term(177) + s1(a,i) * wm_interm_16_so_pt4(b,i) * wm_interm_98_so_pt4(a,b,q,p)
term(178) = term(178) + s2(a,b,i,p) * wm_interm_16_so_pt4(a,q) * wm_interm_33_so_pt4(b,i)
term(179) = term(179) + s2(a,b,i,p) * wm_interm_16_so_pt4(a,q) * wm_interm_34_so_pt4(b,i)
term(180) = term(180) + s2(a,b,i,p) * wm_interm_16_so_pt4(a,q) * wm_interm_35_so_pt4(b,i)
term(181) = term(181) + s2(a,b,i,p) * wm_interm_14_so_pt4(a,q) * wm_interm_33_so_pt4(b,i)
term(182) = term(182) + s2(a,b,i,p) * wm_interm_14_so_pt4(a,q) * wm_interm_34_so_pt4(b,i)
term(183) = term(183) + s2(a,b,i,p) * wm_interm_14_so_pt4(a,q) * wm_interm_35_so_pt4(b,i)
term(184) = term(184) + s2(a,b,i,p) * wm_interm_16_so_pt4(a,q) * wm_interm_36_so_pt4(b,i)
term(185) = term(185) + s2(a,b,i,p) * wm_interm_16_so_pt4(a,q) * wm_interm_37_so_pt4(b,i)
term(186) = term(186) + s2(a,b,i,p) * wm_interm_14_so_pt4(a,q) * wm_interm_36_so_pt4(b,i)
term(187) = term(187) + s2(a,b,i,p) * wm_interm_14_so_pt4(a,q) * wm_interm_37_so_pt4(b,i)
term(188) = term(188) + s2(a,b,i,p) * wm_interm_38_so_pt4(b,i) * wm_interm_6_so_pt4(a,q)
term(189) = term(189) + s2(a,b,i,p) * wm_interm_40_so_pt4(b,i) * wm_interm_6_so_pt4(a,q)
term(190) = term(190) + s2(a,b,i,p) * wm_interm_41_so_pt4(b,i) * wm_interm_6_so_pt4(a,q)
term(191) = term(191) + s2(a,b,i,p) * wm_interm_38_so_pt4(b,i) * wm_interm_7_so_pt4(a,q)
term(192) = term(192) + s2(a,b,i,p) * wm_interm_40_so_pt4(b,i) * wm_interm_7_so_pt4(a,q)
term(193) = term(193) + s2(a,b,i,p) * wm_interm_41_so_pt4(b,i) * wm_interm_7_so_pt4(a,q)
term(194) = term(194) + s2(a,b,i,p) * wm_interm_42_so_pt4(b,i) * wm_interm_6_so_pt4(a,q)
term(195) = term(195) + s2(a,b,i,p) * wm_interm_43_so_pt4(b,i) * wm_interm_6_so_pt4(a,q)
term(196) = term(196) + s2(a,b,i,p) * wm_interm_42_so_pt4(b,i) * wm_interm_7_so_pt4(a,q)
term(197) = term(197) + s2(a,b,i,p) * wm_interm_43_so_pt4(b,i) * wm_interm_7_so_pt4(a,q)
term(198) = term(198) + t1(a,i) * wm_interm_19_so_pt4(a,b) * wm_interm_46_so_pt4(b,i,p,q)
term(199) = term(199) + t1(a,i) * wm_interm_20_so_pt4(a,b) * wm_interm_46_so_pt4(b,i,p,q)
term(200) = term(200) + t1(a,i) * wm_interm_19_so_pt4(a,b) * wm_interm_48_so_pt4(b,i,p,q)
term(201) = term(201) + t1(a,i) * wm_interm_20_so_pt4(a,b) * wm_interm_48_so_pt4(b,i,p,q)
term(202) = term(202) + r1(vrdav_Rr, a,i) * wm_interm_19_so_pt4(a,b) * wm_interm_49_so_pt4(b,i,p,q)
term(203) = term(203) + r1(vrdav_Rr, a,i) * wm_interm_20_so_pt4(a,b) * wm_interm_49_so_pt4(b,i,p,q)
term(204) = term(204) + t1(a,i) * wm_interm_1_so_pt4(a,b,p,q) * wm_interm_45_so_pt4(b,i)
term(205) = term(205) + t1(a,i) * wm_interm_10_so_pt4(a,b,p,q) * wm_interm_45_so_pt4(b,i)
term(206) = term(206) + t1(a,i) * wm_interm_11_so_pt4(a,b,p,q) * wm_interm_45_so_pt4(b,i)
term(207) = term(207) + t1(a,i) * wm_interm_2_so_pt4(b,i) * wm_interm_55_so_pt4(a,b,p,q)
term(208) = term(208) + t1(a,i) * wm_interm_2_so_pt4(b,i) * wm_interm_54_so_pt4(a,b,p,q)
term(209) = term(209) + t1(a,i) * wm_interm_2_so_pt4(b,i) * wm_interm_59_so_pt4(a,b,p,q)
term(210) = term(210) + r1(vrdav_Rr, a,i) * wm_interm_10_so_pt4(a,b,p,q) * wm_interm_50_so_pt4(b,i)
term(211) = term(211) + t2(a,b,i,q) * wm_interm_2_so_pt4(a,p) * wm_interm_50_so_pt4(b,i)
term(212) = term(212) + r1(vrdav_Rr, a,i) * wm_interm_11_so_pt4(a,b,p,q) * wm_interm_50_so_pt4(b,i)
term(213) = term(213) + r1(vrdav_Rr, a,i) * wm_interm_10_so_pt4(a,b,p,q) * wm_interm_51_so_pt4(b,i)
term(214) = term(214) + t2(a,b,i,q) * wm_interm_2_so_pt4(a,p) * wm_interm_51_so_pt4(b,i)
term(215) = term(215) + r1(vrdav_Rr, a,i) * wm_interm_11_so_pt4(a,b,p,q) * wm_interm_51_so_pt4(b,i)
term(216) = term(216) + r1(vrdav_Rr, a,i) * wm_interm_1_so_pt4(a,b,p,q) * wm_interm_50_so_pt4(b,i)
term(217) = term(217) + t2(a,b,i,q) * wm_interm_2_so_pt4(b,i) * wm_interm_50_so_pt4(a,p)
term(218) = term(218) + r1(vrdav_Rr, a,i) * wm_interm_1_so_pt4(a,b,p,q) * wm_interm_51_so_pt4(b,i)
term(219) = term(219) + t2(a,b,i,q) * wm_interm_2_so_pt4(b,i) * wm_interm_51_so_pt4(a,p)
end do 
end do 
end do 

term(128) = term(128) * (4.0d+0) 
term(129) = term(129) * (-2.0d+0) 
term(130) = term(130) * (-2.0d+0) 
term(131) = term(131) * (-2.0d+0) 
term(134) = term(134) * (8.0d+0) 
term(135) = term(135) * (-8.0d+0) 
term(136) = term(136) * (-4.0d+0) 
term(137) = term(137) * (4.0d+0) 
term(138) = term(138) * (-4.0d+0) 
term(139) = term(139) * (8.0d+0) 
term(140) = term(140) * (-4.0d+0) 
term(141) = term(141) * (-4.0d+0) 
term(142) = term(142) * (-4.0d+0) 
term(143) = term(143) * (8.0d+0) 
term(144) = term(144) * (2.0d+0) 
term(145) = term(145) * (-4.0d+0) 
term(146) = term(146) * (2.0d+0) 
term(147) = term(147) * (2.0d+0) 
term(148) = term(148) * (2.0d+0) 
term(149) = term(149) * (-4.0d+0) 
term(150) = term(150) * (-4.0d+0) 
term(151) = term(151) * (8.0d+0) 
term(152) = term(152) * (-4.0d+0) 
term(153) = term(153) * (-4.0d+0) 
term(154) = term(154) * (-4.0d+0) 
term(155) = term(155) * (8.0d+0) 
term(156) = term(156) * (2.0d+0) 
term(157) = term(157) * (-4.0d+0) 
term(158) = term(158) * (2.0d+0) 
term(159) = term(159) * (2.0d+0) 
term(160) = term(160) * (2.0d+0) 
term(161) = term(161) * (-4.0d+0) 
term(162) = term(162) * (-16.0d+0) 
term(163) = term(163) * (16.0d+0) 
term(164) = term(164) * (-16.0d+0) 
term(165) = term(165) * (16.0d+0) 
term(166) = term(166) * (8.0d+0) 
term(167) = term(167) * (-8.0d+0) 
term(168) = term(168) * (8.0d+0) 
term(169) = term(169) * (-8.0d+0) 
term(170) = term(170) * (-16.0d+0) 
term(171) = term(171) * (16.0d+0) 
term(172) = term(172) * (-16.0d+0) 
term(173) = term(173) * (16.0d+0) 
term(174) = term(174) * (8.0d+0) 
term(175) = term(175) * (-8.0d+0) 
term(176) = term(176) * (8.0d+0) 
term(177) = term(177) * (-8.0d+0) 
term(178) = term(178) * (-2.0d+0) 
term(181) = term(181) * (4.0d+0) 
term(182) = term(182) * (-2.0d+0) 
term(183) = term(183) * (-2.0d+0) 
term(184) = term(184) * (-4.0d+0) 
term(185) = term(185) * (4.0d+0) 
term(186) = term(186) * (8.0d+0) 
term(187) = term(187) * (-8.0d+0) 
term(188) = term(188) * (-4.0d+0) 
term(189) = term(189) * (8.0d+0) 
term(190) = term(190) * (-4.0d+0) 
term(191) = term(191) * (2.0d+0) 
term(192) = term(192) * (-4.0d+0) 
term(193) = term(193) * (2.0d+0) 
term(194) = term(194) * (-16.0d+0) 
term(195) = term(195) * (16.0d+0) 
term(196) = term(196) * (8.0d+0) 
term(197) = term(197) * (-8.0d+0) 
term(198) = term(198) * (-2.0d+0) 
term(199) = term(199) * (4.0d+0) 
term(200) = term(200) * (4.0d+0) 
term(201) = term(201) * (-8.0d+0) 
term(202) = term(202) * (-1.0d+0) 
term(203) = term(203) * (2.0d+0) 
term(204) = term(204) * (-2.0d+0) 
term(205) = term(205) * (-2.0d+0) 
term(206) = term(206) * (4.0d+0) 
term(207) = term(207) * (-2.0d+0) 
term(208) = term(208) * (4.0d+0) 
term(209) = term(209) * (-2.0d+0) 
term(210) = term(210) * (4.0d+0) 
term(211) = term(211) * (4.0d+0) 
term(212) = term(212) * (-8.0d+0) 
term(213) = term(213) * (-2.0d+0) 
term(214) = term(214) * (-2.0d+0) 
term(215) = term(215) * (4.0d+0) 
term(216) = term(216) * (4.0d+0) 
term(217) = term(217) * (4.0d+0) 
term(218) = term(218) * (-2.0d+0) 
term(219) = term(219) * (-2.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(220) = term(220) + s2(a,b,p,i) * wm_interm_14_so_pt4(b,i) * wm_interm_33_so_pt4(a,q)
term(221) = term(221) + s2(a,b,p,i) * wm_interm_14_so_pt4(b,i) * wm_interm_34_so_pt4(a,q)
term(222) = term(222) + s2(a,b,p,i) * wm_interm_14_so_pt4(b,i) * wm_interm_35_so_pt4(a,q)
term(223) = term(223) + s2(a,b,p,i) * wm_interm_16_so_pt4(b,i) * wm_interm_33_so_pt4(a,q)
term(224) = term(224) + s2(a,b,p,i) * wm_interm_16_so_pt4(b,i) * wm_interm_34_so_pt4(a,q)
term(225) = term(225) + s2(a,b,p,i) * wm_interm_16_so_pt4(b,i) * wm_interm_35_so_pt4(a,q)
term(226) = term(226) + s2(a,b,p,i) * wm_interm_14_so_pt4(b,i) * wm_interm_36_so_pt4(a,q)
term(227) = term(227) + s2(a,b,p,i) * wm_interm_14_so_pt4(b,i) * wm_interm_37_so_pt4(a,q)
term(228) = term(228) + s2(a,b,p,i) * wm_interm_16_so_pt4(b,i) * wm_interm_36_so_pt4(a,q)
term(229) = term(229) + s2(a,b,p,i) * wm_interm_16_so_pt4(b,i) * wm_interm_37_so_pt4(a,q)
term(230) = term(230) + s1(a,p) * wm_interm_14_so_pt4(b,i) * wm_interm_70_so_pt4(a,b,q,i)
term(231) = term(231) + s1(a,p) * wm_interm_14_so_pt4(b,i) * wm_interm_69_so_pt4(a,b,q,i)
term(232) = term(232) + s1(a,p) * wm_interm_14_so_pt4(b,i) * wm_interm_67_so_pt4(a,b,q,i)
term(233) = term(233) + s1(a,p) * wm_interm_14_so_pt4(b,i) * wm_interm_68_so_pt4(a,b,q,i)
term(234) = term(234) + s1(a,p) * wm_interm_14_so_pt4(b,i) * wm_interm_84_so_pt4(a,b,q,i)
term(235) = term(235) + s1(a,p) * wm_interm_14_so_pt4(b,i) * wm_interm_74_so_pt4(a,b,q,i)
term(236) = term(236) + s1(a,p) * wm_interm_16_so_pt4(b,i) * wm_interm_70_so_pt4(a,b,q,i)
term(237) = term(237) + s1(a,p) * wm_interm_16_so_pt4(b,i) * wm_interm_69_so_pt4(a,b,q,i)
term(238) = term(238) + s1(a,p) * wm_interm_16_so_pt4(b,i) * wm_interm_67_so_pt4(a,b,q,i)
term(239) = term(239) + s1(a,p) * wm_interm_16_so_pt4(b,i) * wm_interm_68_so_pt4(a,b,q,i)
term(240) = term(240) + s1(a,p) * wm_interm_16_so_pt4(b,i) * wm_interm_84_so_pt4(a,b,q,i)
term(241) = term(241) + s1(a,p) * wm_interm_16_so_pt4(b,i) * wm_interm_74_so_pt4(a,b,q,i)
term(242) = term(242) + s1(a,p) * wm_interm_14_so_pt4(b,i) * wm_interm_98_so_pt4(a,b,q,i)
term(243) = term(243) + s1(a,p) * wm_interm_14_so_pt4(b,i) * wm_interm_97_so_pt4(a,b,q,i)
term(244) = term(244) + s1(a,p) * wm_interm_14_so_pt4(b,i) * wm_interm_95_so_pt4(a,b,q,i)
term(245) = term(245) + s1(a,p) * wm_interm_14_so_pt4(b,i) * wm_interm_96_so_pt4(a,b,q,i)
term(246) = term(246) + s1(a,p) * wm_interm_16_so_pt4(b,i) * wm_interm_98_so_pt4(a,b,q,i)
term(247) = term(247) + s1(a,p) * wm_interm_16_so_pt4(b,i) * wm_interm_97_so_pt4(a,b,q,i)
term(248) = term(248) + s1(a,p) * wm_interm_16_so_pt4(b,i) * wm_interm_95_so_pt4(a,b,q,i)
term(249) = term(249) + s1(a,p) * wm_interm_16_so_pt4(b,i) * wm_interm_96_so_pt4(a,b,q,i)
term(250) = term(250) + r1(vrdav_Rl, a,i) * wm_interm_3_so_pt4(b,p,i,q) * wm_interm_83_so_pt4(a,b)
term(251) = term(251) + r1(vrdav_Rl, a,i) * wm_interm_3_so_pt4(b,p,i,q) * wm_interm_81_so_pt4(a,b)
term(252) = term(252) + r1(vrdav_Rl, a,i) * wm_interm_3_so_pt4(b,p,i,q) * wm_interm_82_so_pt4(a,b)
term(253) = term(253) + s1(a,i) * wm_interm_13_so_pt4(b,p,i,q) * wm_interm_81_so_pt4(a,b)
term(254) = term(254) + s1(a,i) * wm_interm_13_so_pt4(b,p,i,q) * wm_interm_82_so_pt4(a,b)
term(255) = term(255) + s1(a,i) * wm_interm_13_so_pt4(b,p,i,q) * wm_interm_83_so_pt4(a,b)
term(256) = term(256) + r1(vrdav_Rl, a,i) * wm_interm_104_so_pt4(a,b) * wm_interm_3_so_pt4(b,p,i,q)
term(257) = term(257) + r1(vrdav_Rl, a,i) * wm_interm_105_so_pt4(a,b) * wm_interm_3_so_pt4(b,p,i,q)
term(258) = term(258) + s1(a,i) * wm_interm_104_so_pt4(a,b) * wm_interm_13_so_pt4(b,p,i,q)
term(259) = term(259) + s1(a,i) * wm_interm_105_so_pt4(a,b) * wm_interm_13_so_pt4(b,p,i,q)
term(260) = term(260) + s1(a,i) * wm_interm_112_so_pt4(a,b) * wm_interm_13_so_pt4(b,p,i,q)
term(261) = term(261) + t1(a,i) * wm_interm_19_so_pt4(a,b) * wm_interm_46_so_pt4(b,p,i,q)
term(262) = term(262) + t1(a,i) * wm_interm_20_so_pt4(a,b) * wm_interm_46_so_pt4(b,p,i,q)
term(263) = term(263) + t1(a,i) * wm_interm_19_so_pt4(a,b) * wm_interm_48_so_pt4(b,p,i,q)
term(264) = term(264) + t1(a,i) * wm_interm_20_so_pt4(a,b) * wm_interm_48_so_pt4(b,p,i,q)
term(265) = term(265) + r1(vrdav_Rr, a,q) * wm_interm_10_so_pt4(a,b,p,i) * wm_interm_50_so_pt4(b,i)
term(266) = term(266) + r1(vrdav_Rr, a,q) * wm_interm_11_so_pt4(a,b,p,i) * wm_interm_50_so_pt4(b,i)
term(267) = term(267) + t1(a,i) * wm_interm_12_so_pt4(b,q,i,p) * wm_interm_57_so_pt4(a,b)
term(268) = term(268) + t1(a,i) * wm_interm_12_so_pt4(b,q,p,i) * wm_interm_57_so_pt4(a,b)
term(269) = term(269) + t1(a,i) * wm_interm_12_so_pt4(b,q,p,i) * wm_interm_58_so_pt4(a,b)
term(270) = term(270) + t1(a,i) * wm_interm_12_so_pt4(b,q,i,p) * wm_interm_58_so_pt4(a,b)
term(271) = term(271) + r1(vrdav_Rr, a,q) * wm_interm_50_so_pt4(b,i) * wm_interm_5_so_pt4(a,b,p,i)
term(272) = term(272) + r1(vrdav_Rr, a,q) * wm_interm_1_so_pt4(a,b,p,i) * wm_interm_50_so_pt4(b,i)
term(273) = term(273) + r1(vrdav_Rr, a,q) * wm_interm_10_so_pt4(a,b,p,i) * wm_interm_51_so_pt4(b,i)
term(274) = term(274) + r1(vrdav_Rr, a,q) * wm_interm_11_so_pt4(a,b,p,i) * wm_interm_51_so_pt4(b,i)
term(275) = term(275) + r1(vrdav_Rr, a,q) * wm_interm_51_so_pt4(b,i) * wm_interm_5_so_pt4(a,b,p,i)
term(276) = term(276) + r1(vrdav_Rr, a,q) * wm_interm_1_so_pt4(a,b,p,i) * wm_interm_51_so_pt4(b,i)
term(277) = term(277) + r1(vrdav_Rr, a,i) * wm_interm_19_so_pt4(a,b) * wm_interm_47_so_pt4(b,p,i,q)
term(278) = term(278) + r1(vrdav_Rr, a,i) * wm_interm_20_so_pt4(a,b) * wm_interm_47_so_pt4(b,p,i,q)
term(279) = term(279) + t1(a,q) * wm_interm_2_so_pt4(b,i) * wm_interm_56_so_pt4(a,b,p,i)
term(280) = term(280) + t1(a,q) * wm_interm_45_so_pt4(b,i) * wm_interm_5_so_pt4(a,b,p,i)
term(281) = term(281) + r1(vrdav_Rr, a,i) * wm_interm_22_so_pt4(b,p,i,q) * wm_interm_57_so_pt4(a,b)
term(282) = term(282) + r1(vrdav_Rr, a,i) * wm_interm_22_so_pt4(b,p,i,q) * wm_interm_58_so_pt4(a,b)
term(283) = term(283) + t2(a,b,q,i) * wm_interm_2_so_pt4(a,p) * wm_interm_50_so_pt4(b,i)
term(284) = term(284) + t2(a,b,q,i) * wm_interm_2_so_pt4(a,p) * wm_interm_51_so_pt4(b,i)
end do 
end do 
end do 

term(220) = term(220) * (-8.0d+0) 
term(221) = term(221) * (4.0d+0) 
term(222) = term(222) * (4.0d+0) 
term(223) = term(223) * (4.0d+0) 
term(224) = term(224) * (-2.0d+0) 
term(225) = term(225) * (-2.0d+0) 
term(226) = term(226) * (-16.0d+0) 
term(227) = term(227) * (16.0d+0) 
term(228) = term(228) * (8.0d+0) 
term(229) = term(229) * (-8.0d+0) 
term(230) = term(230) * (2.0d+0) 
term(231) = term(231) * (-4.0d+0) 
term(232) = term(232) * (-4.0d+0) 
term(233) = term(233) * (8.0d+0) 
term(234) = term(234) * (2.0d+0) 
term(235) = term(235) * (-4.0d+0) 
term(236) = term(236) * (-1.0d+0) 
term(237) = term(237) * (2.0d+0) 
term(238) = term(238) * (2.0d+0) 
term(239) = term(239) * (-4.0d+0) 
term(240) = term(240) * (-1.0d+0) 
term(241) = term(241) * (2.0d+0) 
term(242) = term(242) * (8.0d+0) 
term(243) = term(243) * (-8.0d+0) 
term(244) = term(244) * (-16.0d+0) 
term(245) = term(245) * (16.0d+0) 
term(246) = term(246) * (-4.0d+0) 
term(247) = term(247) * (4.0d+0) 
term(248) = term(248) * (8.0d+0) 
term(249) = term(249) * (-8.0d+0) 
term(250) = term(250) * (2.0d+0) 
term(251) = term(251) * (2.0d+0) 
term(252) = term(252) * (-4.0d+0) 
term(253) = term(253) * (2.0d+0) 
term(254) = term(254) * (-4.0d+0) 
term(255) = term(255) * (2.0d+0) 
term(256) = term(256) * (8.0d+0) 
term(257) = term(257) * (-8.0d+0) 
term(258) = term(258) * (4.0d+0) 
term(259) = term(259) * (-8.0d+0) 
term(260) = term(260) * (4.0d+0) 
term(261) = term(261) * (4.0d+0) 
term(262) = term(262) * (-8.0d+0) 
term(263) = term(263) * (-2.0d+0) 
term(264) = term(264) * (4.0d+0) 
term(265) = term(265) * (-16.0d+0) 
term(266) = term(266) * (32.0d+0) 
term(267) = term(267) * (8.0d+0) 
term(268) = term(268) * (-16.0d+0) 
term(269) = term(269) * (8.0d+0) 
term(270) = term(270) * (-4.0d+0) 
term(271) = term(271) * (8.0d+0) 
term(272) = term(272) * (-16.0d+0) 
term(273) = term(273) * (8.0d+0) 
term(274) = term(274) * (-16.0d+0) 
term(275) = term(275) * (-4.0d+0) 
term(276) = term(276) * (8.0d+0) 
term(277) = term(277) * (-1.0d+0) 
term(278) = term(278) * (2.0d+0) 
term(279) = term(279) * (-2.0d+0) 
term(280) = term(280) * (-2.0d+0) 
term(281) = term(281) * (4.0d+0) 
term(282) = term(282) * (-2.0d+0) 
term(283) = term(283) * (-8.0d+0) 
term(284) = term(284) * (4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(285) = term(285) + s1(a,p) * wm_interm_13_so_pt4(a,i,j,k) * wm_interm_88_so_pt4(i,q,k,j)
term(286) = term(286) + s1(a,p) * wm_interm_13_so_pt4(a,i,j,k) * wm_interm_88_so_pt4(q,i,k,j)
term(287) = term(287) + s1(a,p) * wm_interm_13_so_pt4(a,i,j,k) * wm_interm_88_so_pt4(q,i,j,k)
term(288) = term(288) + s1(a,i) * wm_interm_13_so_pt4(a,p,j,k) * wm_interm_88_so_pt4(q,i,k,j)
term(289) = term(289) + s1(a,i) * wm_interm_13_so_pt4(a,p,j,k) * wm_interm_88_so_pt4(i,q,j,k)
term(290) = term(290) + s1(a,i) * wm_interm_13_so_pt4(a,p,j,k) * wm_interm_88_so_pt4(q,i,j,k)
term(291) = term(291) + s1(a,p) * wm_interm_109_so_pt4(i,q,j,k) * wm_interm_13_so_pt4(a,i,k,j)
term(292) = term(292) + s1(a,p) * wm_interm_109_so_pt4(q,i,j,k) * wm_interm_13_so_pt4(a,i,k,j)
term(293) = term(293) + s1(a,p) * wm_interm_109_so_pt4(q,i,j,k) * wm_interm_13_so_pt4(a,i,j,k)
term(294) = term(294) + s1(a,i) * wm_interm_109_so_pt4(q,i,j,k) * wm_interm_13_so_pt4(a,p,k,j)
term(295) = term(295) + s1(a,i) * wm_interm_109_so_pt4(i,q,j,k) * wm_interm_13_so_pt4(a,p,j,k)
term(296) = term(296) + s1(a,i) * wm_interm_109_so_pt4(q,i,j,k) * wm_interm_13_so_pt4(a,p,j,k)
term(297) = term(297) + s1(a,i) * wm_interm_13_so_pt4(a,j,q,k) * wm_interm_88_so_pt4(j,i,k,p)
term(298) = term(298) + s1(a,i) * wm_interm_13_so_pt4(a,j,q,k) * wm_interm_88_so_pt4(i,j,p,k)
term(299) = term(299) + s1(a,i) * wm_interm_13_so_pt4(a,j,q,k) * wm_interm_88_so_pt4(j,i,p,k)
term(300) = term(300) + s1(a,i) * wm_interm_24_so_pt4(a,p,j,k) * wm_interm_9_so_pt4(k,j,i,q)
term(301) = term(301) + s1(a,i) * wm_interm_26_so_pt4(a,p,j,k) * wm_interm_9_so_pt4(j,k,i,q)
term(302) = term(302) + s1(a,i) * wm_interm_26_so_pt4(a,p,j,k) * wm_interm_9_so_pt4(k,j,i,q)
term(303) = term(303) + s1(a,i) * wm_interm_13_so_pt4(a,j,k,q) * wm_interm_88_so_pt4(i,j,k,p)
term(304) = term(304) + s1(a,i) * wm_interm_13_so_pt4(a,j,k,q) * wm_interm_88_so_pt4(j,i,p,k)
term(305) = term(305) + s1(a,i) * wm_interm_13_so_pt4(a,j,k,q) * wm_interm_88_so_pt4(i,j,p,k)
term(306) = term(306) + s1(a,p) * wm_interm_24_so_pt4(a,i,j,k) * wm_interm_9_so_pt4(j,k,i,q)
term(307) = term(307) + s1(a,p) * wm_interm_24_so_pt4(a,i,j,k) * wm_interm_9_so_pt4(k,j,i,q)
term(308) = term(308) + s1(a,p) * wm_interm_26_so_pt4(a,i,j,k) * wm_interm_9_so_pt4(k,j,i,q)
term(309) = term(309) + s1(a,i) * wm_interm_109_so_pt4(j,i,k,p) * wm_interm_13_so_pt4(a,j,q,k)
term(310) = term(310) + s1(a,i) * wm_interm_109_so_pt4(i,j,p,k) * wm_interm_13_so_pt4(a,j,q,k)
term(311) = term(311) + s1(a,i) * wm_interm_109_so_pt4(j,i,p,k) * wm_interm_13_so_pt4(a,j,q,k)
term(312) = term(312) + s1(a,i) * wm_interm_29_so_pt4(a,p,j,k) * wm_interm_9_so_pt4(j,k,i,q)
term(313) = term(313) + s1(a,i) * wm_interm_29_so_pt4(a,p,j,k) * wm_interm_9_so_pt4(k,j,i,q)
term(314) = term(314) + s1(a,i) * wm_interm_109_so_pt4(i,j,k,p) * wm_interm_13_so_pt4(a,j,k,q)
term(315) = term(315) + s1(a,i) * wm_interm_109_so_pt4(j,i,p,k) * wm_interm_13_so_pt4(a,j,k,q)
term(316) = term(316) + s1(a,i) * wm_interm_109_so_pt4(i,j,p,k) * wm_interm_13_so_pt4(a,j,k,q)
term(317) = term(317) + s1(a,p) * wm_interm_29_so_pt4(a,i,j,k) * wm_interm_9_so_pt4(k,j,i,q)
term(318) = term(318) + s1(a,p) * wm_interm_29_so_pt4(a,i,j,k) * wm_interm_9_so_pt4(j,k,i,q)
term(319) = term(319) + t1(a,i) * wm_interm_12_so_pt4(a,q,j,k) * wm_interm_61_so_pt4(p,i,k,j)
term(320) = term(320) + t1(a,i) * wm_interm_12_so_pt4(a,q,j,k) * wm_interm_61_so_pt4(p,i,j,k)
term(321) = term(321) + t1(a,i) * wm_interm_46_so_pt4(a,j,k,q) * wm_interm_9_so_pt4(i,p,j,k)
term(322) = term(322) + t1(a,i) * wm_interm_46_so_pt4(a,j,k,q) * wm_interm_9_so_pt4(p,i,j,k)
term(323) = term(323) + t1(a,i) * wm_interm_12_so_pt4(a,q,j,k) * wm_interm_61_so_pt4(i,p,j,k)
term(324) = term(324) + t1(a,i) * wm_interm_12_so_pt4(a,q,j,k) * wm_interm_61_so_pt4(i,p,k,j)
term(325) = term(325) + t1(a,i) * wm_interm_48_so_pt4(a,j,k,q) * wm_interm_9_so_pt4(p,i,j,k)
term(326) = term(326) + t1(a,i) * wm_interm_48_so_pt4(a,j,k,q) * wm_interm_9_so_pt4(i,p,j,k)
term(327) = term(327) + t1(a,q) * wm_interm_48_so_pt4(a,i,j,k) * wm_interm_9_so_pt4(k,p,i,j)
term(328) = term(328) + t1(a,q) * wm_interm_46_so_pt4(a,i,j,k) * wm_interm_9_so_pt4(p,k,i,j)
term(329) = term(329) + t1(a,q) * wm_interm_12_so_pt4(a,i,j,k) * wm_interm_61_so_pt4(p,i,j,k)
term(330) = term(330) + t1(a,q) * wm_interm_12_so_pt4(a,i,j,k) * wm_interm_61_so_pt4(i,p,k,j)
term(331) = term(331) + t1(a,i) * wm_interm_12_so_pt4(a,j,p,k) * wm_interm_61_so_pt4(j,i,k,q)
term(332) = term(332) + t1(a,i) * wm_interm_12_so_pt4(a,j,k,p) * wm_interm_61_so_pt4(j,i,q,k)
term(333) = term(333) + t1(a,i) * wm_interm_12_so_pt4(a,j,p,k) * wm_interm_61_so_pt4(j,i,q,k)
term(334) = term(334) + t1(a,i) * wm_interm_46_so_pt4(a,j,p,k) * wm_interm_9_so_pt4(i,k,j,q)
term(335) = term(335) + t1(a,i) * wm_interm_46_so_pt4(a,p,j,k) * wm_interm_9_so_pt4(i,k,j,q)
term(336) = term(336) + t1(a,i) * wm_interm_46_so_pt4(a,p,j,k) * wm_interm_9_so_pt4(k,i,j,q)
term(337) = term(337) + t1(a,i) * wm_interm_12_so_pt4(a,j,k,p) * wm_interm_61_so_pt4(i,j,k,q)
term(338) = term(338) + t1(a,i) * wm_interm_12_so_pt4(a,j,p,k) * wm_interm_61_so_pt4(i,j,k,q)
term(339) = term(339) + t1(a,i) * wm_interm_12_so_pt4(a,j,p,k) * wm_interm_61_so_pt4(i,j,q,k)
term(340) = term(340) + t1(a,i) * wm_interm_48_so_pt4(a,p,j,k) * wm_interm_9_so_pt4(i,k,j,q)
term(341) = term(341) + t1(a,i) * wm_interm_48_so_pt4(a,j,p,k) * wm_interm_9_so_pt4(i,k,j,q)
term(342) = term(342) + t1(a,i) * wm_interm_48_so_pt4(a,j,p,k) * wm_interm_9_so_pt4(k,i,j,q)
end do 
end do 
end do 
end do 

term(285) = term(285) * (2.0d+0) 
term(286) = term(286) * (-4.0d+0) 
term(287) = term(287) * (2.0d+0) 
term(288) = term(288) * (2.0d+0) 
term(289) = term(289) * (2.0d+0) 
term(290) = term(290) * (-4.0d+0) 
term(291) = term(291) * (4.0d+0) 
term(292) = term(292) * (-8.0d+0) 
term(293) = term(293) * (4.0d+0) 
term(294) = term(294) * (4.0d+0) 
term(295) = term(295) * (4.0d+0) 
term(296) = term(296) * (-8.0d+0) 
term(297) = term(297) * (2.0d+0) 
term(298) = term(298) * (2.0d+0) 
term(299) = term(299) * (-4.0d+0) 
term(302) = term(302) * (-2.0d+0) 
term(303) = term(303) * (2.0d+0) 
term(304) = term(304) * (2.0d+0) 
term(305) = term(305) * (-4.0d+0) 
term(306) = term(306) * (2.0d+0) 
term(307) = term(307) * (-4.0d+0) 
term(308) = term(308) * (2.0d+0) 
term(309) = term(309) * (4.0d+0) 
term(310) = term(310) * (4.0d+0) 
term(311) = term(311) * (-8.0d+0) 
term(312) = term(312) * (4.0d+0) 
term(313) = term(313) * (-4.0d+0) 
term(314) = term(314) * (4.0d+0) 
term(315) = term(315) * (4.0d+0) 
term(316) = term(316) * (-8.0d+0) 
term(317) = term(317) * (8.0d+0) 
term(318) = term(318) * (-8.0d+0) 
term(319) = term(319) * (-2.0d+0) 
term(320) = term(320) * (4.0d+0) 
term(321) = term(321) * (-2.0d+0) 
term(322) = term(322) * (4.0d+0) 
term(323) = term(323) * (-2.0d+0) 
term(324) = term(324) * (4.0d+0) 
term(325) = term(325) * (-2.0d+0) 
term(326) = term(326) * (4.0d+0) 
term(327) = term(327) * (-1.0d+0) 
term(328) = term(328) * (-1.0d+0) 
term(329) = term(329) * (-1.0d+0) 
term(330) = term(330) * (-1.0d+0) 
term(331) = term(331) * (-1.0d+0) 
term(332) = term(332) * (-1.0d+0) 
term(333) = term(333) * (2.0d+0) 
term(334) = term(334) * (-1.0d+0) 
term(335) = term(335) * (2.0d+0) 
term(336) = term(336) * (-1.0d+0) 
term(337) = term(337) * (-1.0d+0) 
term(338) = term(338) * (2.0d+0) 
term(339) = term(339) * (-1.0d+0) 
term(340) = term(340) * (-1.0d+0) 
term(341) = term(341) * (2.0d+0) 
term(342) = term(342) * (-1.0d+0) 

do i = 1, nocc 
term(343) = term(343) + wm_interm_25_so_pt4(p,i) * wm_interm_76_so_pt4(i,q)
term(344) = term(344) + wm_interm_25_so_pt4(p,i) * wm_interm_78_so_pt4(i,q)
term(345) = term(345) + wm_interm_25_so_pt4(p,i) * wm_interm_80_so_pt4(i,q)
term(346) = term(346) + wm_interm_101_so_pt4(i,q) * wm_interm_25_so_pt4(p,i)
term(347) = term(347) + wm_interm_103_so_pt4(i,q) * wm_interm_25_so_pt4(p,i)
term(348) = term(348) + wm_interm_25_so_pt4(i,q) * wm_interm_78_so_pt4(p,i)
term(349) = term(349) + wm_interm_25_so_pt4(i,q) * wm_interm_80_so_pt4(p,i)
term(350) = term(350) + wm_interm_25_so_pt4(i,q) * wm_interm_76_so_pt4(p,i)
term(351) = term(351) + wm_interm_101_so_pt4(p,i) * wm_interm_25_so_pt4(i,q)
term(352) = term(352) + wm_interm_103_so_pt4(p,i) * wm_interm_25_so_pt4(i,q)
term(353) = term(353) + wm_interm_25_so_pt4(p,i) * wm_interm_4_so_pt4(i,q)
term(354) = term(354) + wm_interm_21_so_pt4(p,i) * wm_interm_8_so_pt4(q,i)
term(355) = term(355) + wm_interm_25_so_pt4(i,q) * wm_interm_4_so_pt4(p,i)
term(356) = term(356) + wm_interm_21_so_pt4(i,q) * wm_interm_8_so_pt4(i,p)
end do 

term(343) = term(343) * (-1.0d+0) 
term(344) = term(344) * (-1.0d+0) 
term(345) = term(345) * (2.0d+0) 
term(346) = term(346) * (-4.0d+0) 
term(347) = term(347) * (4.0d+0) 
term(348) = term(348) * (-1.0d+0) 
term(349) = term(349) * (2.0d+0) 
term(350) = term(350) * (-1.0d+0) 
term(351) = term(351) * (-4.0d+0) 
term(352) = term(352) * (4.0d+0) 
term(353) = term(353) * (2.0d+0) 
term(354) = term(354) * (2.0d+0) 
term(355) = term(355) * (2.0d+0) 
term(356) = term(356) * (2.0d+0) 

do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(357) = term(357) + r1(vrdav_Rl, a,p) * wm_interm_19_so_pt4(b,a) * wm_interm_33_so_pt4(b,q)
term(358) = term(358) + r1(vrdav_Rl, a,p) * wm_interm_20_so_pt4(b,a) * wm_interm_33_so_pt4(b,q)
term(359) = term(359) + r1(vrdav_Rl, a,p) * wm_interm_19_so_pt4(b,a) * wm_interm_34_so_pt4(b,q)
term(360) = term(360) + r1(vrdav_Rl, a,p) * wm_interm_20_so_pt4(b,a) * wm_interm_34_so_pt4(b,q)
term(361) = term(361) + r1(vrdav_Rl, a,p) * wm_interm_19_so_pt4(b,a) * wm_interm_35_so_pt4(b,q)
term(362) = term(362) + r1(vrdav_Rl, a,p) * wm_interm_20_so_pt4(b,a) * wm_interm_35_so_pt4(b,q)
term(363) = term(363) + r1(vrdav_Rl, a,p) * wm_interm_19_so_pt4(b,a) * wm_interm_36_so_pt4(b,q)
term(364) = term(364) + r1(vrdav_Rl, a,p) * wm_interm_20_so_pt4(b,a) * wm_interm_36_so_pt4(b,q)
term(365) = term(365) + r1(vrdav_Rl, a,p) * wm_interm_19_so_pt4(b,a) * wm_interm_37_so_pt4(b,q)
term(366) = term(366) + r1(vrdav_Rl, a,p) * wm_interm_20_so_pt4(b,a) * wm_interm_37_so_pt4(b,q)
term(367) = term(367) + r1(vrdav_Rr, b,q) * wm_interm_19_so_pt4(b,a) * wm_interm_50_so_pt4(a,p)
term(368) = term(368) + r1(vrdav_Rr, b,q) * wm_interm_20_so_pt4(b,a) * wm_interm_50_so_pt4(a,p)
term(369) = term(369) + r1(vrdav_Rr, b,q) * wm_interm_19_so_pt4(b,a) * wm_interm_51_so_pt4(a,p)
term(370) = term(370) + r1(vrdav_Rr, b,q) * wm_interm_20_so_pt4(b,a) * wm_interm_51_so_pt4(a,p)
term(371) = term(371) + t1(b,q) * wm_interm_19_so_pt4(b,a) * wm_interm_45_so_pt4(a,p)
term(372) = term(372) + t1(b,q) * wm_interm_20_so_pt4(b,a) * wm_interm_45_so_pt4(a,p)
term(373) = term(373) + t1(b,q) * wm_interm_2_so_pt4(a,p) * wm_interm_57_so_pt4(b,a)
term(374) = term(374) + t1(b,q) * wm_interm_2_so_pt4(a,p) * wm_interm_58_so_pt4(b,a)
end do 
end do 

term(357) = term(357) * (-2.0d+0) 
term(358) = term(358) * (4.0d+0) 
term(360) = term(360) * (-2.0d+0) 
term(362) = term(362) * (-2.0d+0) 
term(363) = term(363) * (-4.0d+0) 
term(364) = term(364) * (8.0d+0) 
term(365) = term(365) * (4.0d+0) 
term(366) = term(366) * (-8.0d+0) 
term(367) = term(367) * (8.0d+0) 
term(368) = term(368) * (-16.0d+0) 
term(369) = term(369) * (-4.0d+0) 
term(370) = term(370) * (8.0d+0) 
term(371) = term(371) * (-2.0d+0) 
term(372) = term(372) * (4.0d+0) 
term(373) = term(373) * (4.0d+0) 
term(374) = term(374) * (-2.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(375) = term(375) + wm_interm_28_so_pt4(a,i,q,p) * wm_interm_50_so_pt4(a,i)
term(376) = term(376) + wm_interm_28_so_pt4(a,q,i,p) * wm_interm_50_so_pt4(a,i)
term(377) = term(377) + wm_interm_27_so_pt4(a,q,i,p) * wm_interm_50_so_pt4(a,i)
term(378) = term(378) + wm_interm_28_so_pt4(a,i,q,p) * wm_interm_51_so_pt4(a,i)
term(379) = term(379) + wm_interm_28_so_pt4(a,q,i,p) * wm_interm_51_so_pt4(a,i)
term(380) = term(380) + wm_interm_27_so_pt4(a,q,i,p) * wm_interm_51_so_pt4(a,i)
term(381) = term(381) + wm_interm_30_so_pt4(a,q,i,p) * wm_interm_50_so_pt4(a,i)
term(382) = term(382) + wm_interm_30_so_pt4(a,i,q,p) * wm_interm_50_so_pt4(a,i)
term(383) = term(383) + wm_interm_30_so_pt4(a,q,i,p) * wm_interm_51_so_pt4(a,i)
term(384) = term(384) + wm_interm_30_so_pt4(a,i,q,p) * wm_interm_51_so_pt4(a,i)
term(385) = term(385) + wm_interm_33_so_pt4(a,i) * wm_interm_49_so_pt4(a,i,p,q)
term(386) = term(386) + wm_interm_33_so_pt4(a,i) * wm_interm_47_so_pt4(a,p,i,q)
term(387) = term(387) + wm_interm_34_so_pt4(a,i) * wm_interm_49_so_pt4(a,i,p,q)
term(388) = term(388) + wm_interm_34_so_pt4(a,i) * wm_interm_47_so_pt4(a,p,i,q)
term(389) = term(389) + wm_interm_35_so_pt4(a,i) * wm_interm_49_so_pt4(a,i,p,q)
term(390) = term(390) + wm_interm_35_so_pt4(a,i) * wm_interm_47_so_pt4(a,p,i,q)
term(391) = term(391) + wm_interm_36_so_pt4(a,i) * wm_interm_49_so_pt4(a,i,p,q)
term(392) = term(392) + wm_interm_36_so_pt4(a,i) * wm_interm_47_so_pt4(a,p,i,q)
term(393) = term(393) + wm_interm_37_so_pt4(a,i) * wm_interm_49_so_pt4(a,i,p,q)
term(394) = term(394) + wm_interm_37_so_pt4(a,i) * wm_interm_47_so_pt4(a,p,i,q)
term(395) = term(395) + r1(vrdav_Rl, a,i) * wm_interm_17_so_pt4(p,i) * wm_interm_33_so_pt4(a,q)
term(396) = term(396) + r1(vrdav_Rl, a,i) * wm_interm_18_so_pt4(p,i) * wm_interm_33_so_pt4(a,q)
term(397) = term(397) + r1(vrdav_Rl, a,i) * wm_interm_17_so_pt4(p,i) * wm_interm_34_so_pt4(a,q)
term(398) = term(398) + r1(vrdav_Rl, a,i) * wm_interm_18_so_pt4(p,i) * wm_interm_34_so_pt4(a,q)
term(399) = term(399) + r1(vrdav_Rl, a,i) * wm_interm_17_so_pt4(p,i) * wm_interm_35_so_pt4(a,q)
term(400) = term(400) + r1(vrdav_Rl, a,i) * wm_interm_18_so_pt4(p,i) * wm_interm_35_so_pt4(a,q)
term(401) = term(401) + r1(vrdav_Rl, a,i) * wm_interm_17_so_pt4(p,i) * wm_interm_36_so_pt4(a,q)
term(402) = term(402) + r1(vrdav_Rl, a,i) * wm_interm_18_so_pt4(p,i) * wm_interm_36_so_pt4(a,q)
term(403) = term(403) + r1(vrdav_Rl, a,i) * wm_interm_17_so_pt4(p,i) * wm_interm_37_so_pt4(a,q)
term(404) = term(404) + r1(vrdav_Rl, a,i) * wm_interm_18_so_pt4(p,i) * wm_interm_37_so_pt4(a,q)
term(405) = term(405) + r1(vrdav_Rl, a,p) * wm_interm_7_so_pt4(a,i) * wm_interm_85_so_pt4(q,i)
term(406) = term(406) + r1(vrdav_Rl, a,p) * wm_interm_7_so_pt4(a,i) * wm_interm_86_so_pt4(q,i)
term(407) = term(407) + r1(vrdav_Rl, a,p) * wm_interm_7_so_pt4(a,i) * wm_interm_87_so_pt4(q,i)
term(408) = term(408) + s1(a,p) * wm_interm_14_so_pt4(a,i) * wm_interm_85_so_pt4(q,i)
term(409) = term(409) + s1(a,p) * wm_interm_14_so_pt4(a,i) * wm_interm_86_so_pt4(q,i)
term(410) = term(410) + s1(a,p) * wm_interm_14_so_pt4(a,i) * wm_interm_87_so_pt4(q,i)
term(411) = term(411) + r1(vrdav_Rl, a,p) * wm_interm_6_so_pt4(a,i) * wm_interm_85_so_pt4(q,i)
term(412) = term(412) + r1(vrdav_Rl, a,p) * wm_interm_6_so_pt4(a,i) * wm_interm_86_so_pt4(q,i)
term(413) = term(413) + r1(vrdav_Rl, a,p) * wm_interm_6_so_pt4(a,i) * wm_interm_87_so_pt4(q,i)
term(414) = term(414) + s1(a,p) * wm_interm_16_so_pt4(a,i) * wm_interm_85_so_pt4(q,i)
term(415) = term(415) + s1(a,p) * wm_interm_16_so_pt4(a,i) * wm_interm_86_so_pt4(q,i)
term(416) = term(416) + s1(a,p) * wm_interm_16_so_pt4(a,i) * wm_interm_87_so_pt4(q,i)
term(417) = term(417) + r1(vrdav_Rl, a,p) * wm_interm_106_so_pt4(q,i) * wm_interm_7_so_pt4(a,i)
term(418) = term(418) + r1(vrdav_Rl, a,p) * wm_interm_107_so_pt4(q,i) * wm_interm_7_so_pt4(a,i)
term(419) = term(419) + r1(vrdav_Rl, a,p) * wm_interm_108_so_pt4(q,i) * wm_interm_7_so_pt4(a,i)
term(420) = term(420) + s1(a,p) * wm_interm_106_so_pt4(q,i) * wm_interm_14_so_pt4(a,i)
term(421) = term(421) + s1(a,p) * wm_interm_107_so_pt4(q,i) * wm_interm_14_so_pt4(a,i)
term(422) = term(422) + s1(a,p) * wm_interm_108_so_pt4(q,i) * wm_interm_14_so_pt4(a,i)
term(423) = term(423) + r1(vrdav_Rl, a,p) * wm_interm_106_so_pt4(q,i) * wm_interm_6_so_pt4(a,i)
term(424) = term(424) + r1(vrdav_Rl, a,p) * wm_interm_107_so_pt4(q,i) * wm_interm_6_so_pt4(a,i)
term(425) = term(425) + r1(vrdav_Rl, a,p) * wm_interm_108_so_pt4(q,i) * wm_interm_6_so_pt4(a,i)
term(426) = term(426) + s1(a,p) * wm_interm_106_so_pt4(q,i) * wm_interm_16_so_pt4(a,i)
term(427) = term(427) + s1(a,p) * wm_interm_107_so_pt4(q,i) * wm_interm_16_so_pt4(a,i)
term(428) = term(428) + s1(a,p) * wm_interm_108_so_pt4(q,i) * wm_interm_16_so_pt4(a,i)
term(429) = term(429) + r1(vrdav_Rl, a,p) * wm_interm_17_so_pt4(i,q) * wm_interm_33_so_pt4(a,i)
term(430) = term(430) + r1(vrdav_Rl, a,p) * wm_interm_18_so_pt4(i,q) * wm_interm_33_so_pt4(a,i)
term(431) = term(431) + r1(vrdav_Rl, a,p) * wm_interm_17_so_pt4(i,q) * wm_interm_34_so_pt4(a,i)
term(432) = term(432) + r1(vrdav_Rl, a,p) * wm_interm_18_so_pt4(i,q) * wm_interm_34_so_pt4(a,i)
term(433) = term(433) + r1(vrdav_Rl, a,p) * wm_interm_17_so_pt4(i,q) * wm_interm_35_so_pt4(a,i)
term(434) = term(434) + r1(vrdav_Rl, a,p) * wm_interm_18_so_pt4(i,q) * wm_interm_35_so_pt4(a,i)
term(435) = term(435) + r1(vrdav_Rl, a,p) * wm_interm_17_so_pt4(i,q) * wm_interm_36_so_pt4(a,i)
term(436) = term(436) + r1(vrdav_Rl, a,p) * wm_interm_18_so_pt4(i,q) * wm_interm_36_so_pt4(a,i)
term(437) = term(437) + r1(vrdav_Rl, a,p) * wm_interm_17_so_pt4(i,q) * wm_interm_37_so_pt4(a,i)
term(438) = term(438) + r1(vrdav_Rl, a,p) * wm_interm_18_so_pt4(i,q) * wm_interm_37_so_pt4(a,i)
term(439) = term(439) + s1(a,i) * wm_interm_16_so_pt4(a,q) * wm_interm_85_so_pt4(i,p)
term(440) = term(440) + r1(vrdav_Rl, a,i) * wm_interm_6_so_pt4(a,q) * wm_interm_85_so_pt4(i,p)
term(441) = term(441) + s1(a,i) * wm_interm_16_so_pt4(a,q) * wm_interm_86_so_pt4(i,p)
term(442) = term(442) + r1(vrdav_Rl, a,i) * wm_interm_6_so_pt4(a,q) * wm_interm_86_so_pt4(i,p)
term(443) = term(443) + s1(a,i) * wm_interm_16_so_pt4(a,q) * wm_interm_87_so_pt4(i,p)
term(444) = term(444) + r1(vrdav_Rl, a,i) * wm_interm_6_so_pt4(a,q) * wm_interm_87_so_pt4(i,p)
term(445) = term(445) + r1(vrdav_Rl, a,i) * wm_interm_7_so_pt4(a,q) * wm_interm_85_so_pt4(i,p)
term(446) = term(446) + r1(vrdav_Rl, a,i) * wm_interm_7_so_pt4(a,q) * wm_interm_86_so_pt4(i,p)
term(447) = term(447) + r1(vrdav_Rl, a,i) * wm_interm_7_so_pt4(a,q) * wm_interm_87_so_pt4(i,p)
term(448) = term(448) + s1(a,i) * wm_interm_14_so_pt4(a,q) * wm_interm_85_so_pt4(i,p)
term(449) = term(449) + s1(a,i) * wm_interm_14_so_pt4(a,q) * wm_interm_86_so_pt4(i,p)
term(450) = term(450) + s1(a,i) * wm_interm_14_so_pt4(a,q) * wm_interm_87_so_pt4(i,p)
term(451) = term(451) + s1(a,p) * wm_interm_17_so_pt4(i,q) * wm_interm_38_so_pt4(a,i)
term(452) = term(452) + s1(a,p) * wm_interm_17_so_pt4(i,q) * wm_interm_40_so_pt4(a,i)
term(453) = term(453) + s1(a,p) * wm_interm_18_so_pt4(i,q) * wm_interm_38_so_pt4(a,i)
term(454) = term(454) + s1(a,p) * wm_interm_18_so_pt4(i,q) * wm_interm_40_so_pt4(a,i)
term(455) = term(455) + s1(a,p) * wm_interm_17_so_pt4(i,q) * wm_interm_41_so_pt4(a,i)
term(456) = term(456) + s1(a,p) * wm_interm_18_so_pt4(i,q) * wm_interm_41_so_pt4(a,i)
term(457) = term(457) + s1(a,i) * wm_interm_106_so_pt4(i,p) * wm_interm_16_so_pt4(a,q)
term(458) = term(458) + r1(vrdav_Rl, a,i) * wm_interm_106_so_pt4(i,p) * wm_interm_6_so_pt4(a,q)
term(459) = term(459) + s1(a,i) * wm_interm_107_so_pt4(i,p) * wm_interm_16_so_pt4(a,q)
term(460) = term(460) + r1(vrdav_Rl, a,i) * wm_interm_107_so_pt4(i,p) * wm_interm_6_so_pt4(a,q)
term(461) = term(461) + s1(a,i) * wm_interm_108_so_pt4(i,p) * wm_interm_16_so_pt4(a,q)
term(462) = term(462) + r1(vrdav_Rl, a,i) * wm_interm_108_so_pt4(i,p) * wm_interm_6_so_pt4(a,q)
term(463) = term(463) + r1(vrdav_Rl, a,i) * wm_interm_106_so_pt4(i,p) * wm_interm_7_so_pt4(a,q)
term(464) = term(464) + r1(vrdav_Rl, a,i) * wm_interm_107_so_pt4(i,p) * wm_interm_7_so_pt4(a,q)
term(465) = term(465) + r1(vrdav_Rl, a,i) * wm_interm_108_so_pt4(i,p) * wm_interm_7_so_pt4(a,q)
term(466) = term(466) + s1(a,i) * wm_interm_106_so_pt4(i,p) * wm_interm_14_so_pt4(a,q)
term(467) = term(467) + s1(a,i) * wm_interm_107_so_pt4(i,p) * wm_interm_14_so_pt4(a,q)
term(468) = term(468) + s1(a,i) * wm_interm_108_so_pt4(i,p) * wm_interm_14_so_pt4(a,q)
term(469) = term(469) + s1(a,p) * wm_interm_17_so_pt4(i,q) * wm_interm_42_so_pt4(a,i)
term(470) = term(470) + s1(a,p) * wm_interm_17_so_pt4(i,q) * wm_interm_43_so_pt4(a,i)
term(471) = term(471) + s1(a,p) * wm_interm_18_so_pt4(i,q) * wm_interm_42_so_pt4(a,i)
term(472) = term(472) + s1(a,p) * wm_interm_18_so_pt4(i,q) * wm_interm_43_so_pt4(a,i)
term(473) = term(473) + r1(vrdav_Rr, a,q) * wm_interm_17_so_pt4(p,i) * wm_interm_50_so_pt4(a,i)
term(474) = term(474) + r1(vrdav_Rr, a,q) * wm_interm_18_so_pt4(p,i) * wm_interm_50_so_pt4(a,i)
term(475) = term(475) + r1(vrdav_Rr, a,q) * wm_interm_17_so_pt4(p,i) * wm_interm_51_so_pt4(a,i)
term(476) = term(476) + r1(vrdav_Rr, a,q) * wm_interm_18_so_pt4(p,i) * wm_interm_51_so_pt4(a,i)
term(477) = term(477) + t1(a,q) * wm_interm_17_so_pt4(p,i) * wm_interm_45_so_pt4(a,i)
term(478) = term(478) + t1(a,q) * wm_interm_18_so_pt4(p,i) * wm_interm_45_so_pt4(a,i)
term(479) = term(479) + t1(a,q) * wm_interm_2_so_pt4(a,i) * wm_interm_60_so_pt4(p,i)
term(480) = term(480) + t1(a,q) * wm_interm_2_so_pt4(a,i) * wm_interm_62_so_pt4(p,i)
term(481) = term(481) + t1(a,i) * wm_interm_2_so_pt4(a,p) * wm_interm_62_so_pt4(i,q)
term(482) = term(482) + t1(a,i) * wm_interm_2_so_pt4(a,p) * wm_interm_60_so_pt4(i,q)
term(483) = term(483) + t1(a,i) * wm_interm_17_so_pt4(i,q) * wm_interm_45_so_pt4(a,p)
term(484) = term(484) + t1(a,i) * wm_interm_18_so_pt4(i,q) * wm_interm_45_so_pt4(a,p)
term(485) = term(485) + r1(vrdav_Rr, a,i) * wm_interm_17_so_pt4(i,q) * wm_interm_50_so_pt4(a,p)
term(486) = term(486) + r1(vrdav_Rr, a,i) * wm_interm_18_so_pt4(i,q) * wm_interm_50_so_pt4(a,p)
term(487) = term(487) + r1(vrdav_Rr, a,i) * wm_interm_17_so_pt4(i,q) * wm_interm_51_so_pt4(a,p)
term(488) = term(488) + r1(vrdav_Rr, a,i) * wm_interm_18_so_pt4(i,q) * wm_interm_51_so_pt4(a,p)
end do 
end do 

term(375) = term(375) * (2.0d+0) 
term(376) = term(376) * (-4.0d+0) 
term(377) = term(377) * (2.0d+0) 
term(378) = term(378) * (-1.0d+0) 
term(379) = term(379) * (2.0d+0) 
term(380) = term(380) * (-1.0d+0) 
term(381) = term(381) * (8.0d+0) 
term(382) = term(382) * (-8.0d+0) 
term(383) = term(383) * (-4.0d+0) 
term(384) = term(384) * (4.0d+0) 
term(385) = term(385) * (-1.0d+0) 
term(386) = term(386) * (-1.0d+0) 
term(387) = term(387) * (0.5d+0) 
term(388) = term(388) * (0.5d+0) 
term(389) = term(389) * (0.5d+0) 
term(390) = term(390) * (0.5d+0) 
term(391) = term(391) * (-2.0d+0) 
term(392) = term(392) * (-2.0d+0) 
term(393) = term(393) * (2.0d+0) 
term(394) = term(394) * (2.0d+0) 
term(395) = term(395) * (-2.0d+0) 
term(396) = term(396) * (4.0d+0) 
term(398) = term(398) * (-2.0d+0) 
term(400) = term(400) * (-2.0d+0) 
term(401) = term(401) * (-4.0d+0) 
term(402) = term(402) * (8.0d+0) 
term(403) = term(403) * (4.0d+0) 
term(404) = term(404) * (-8.0d+0) 
term(405) = term(405) * (2.0d+0) 
term(406) = term(406) * (-4.0d+0) 
term(407) = term(407) * (2.0d+0) 
term(408) = term(408) * (-4.0d+0) 
term(409) = term(409) * (8.0d+0) 
term(410) = term(410) * (-4.0d+0) 
term(411) = term(411) * (-4.0d+0) 
term(412) = term(412) * (8.0d+0) 
term(413) = term(413) * (-4.0d+0) 
term(414) = term(414) * (2.0d+0) 
term(415) = term(415) * (-4.0d+0) 
term(416) = term(416) * (2.0d+0) 
term(417) = term(417) * (4.0d+0) 
term(418) = term(418) * (-8.0d+0) 
term(419) = term(419) * (4.0d+0) 
term(420) = term(420) * (-8.0d+0) 
term(421) = term(421) * (16.0d+0) 
term(422) = term(422) * (-8.0d+0) 
term(423) = term(423) * (-8.0d+0) 
term(424) = term(424) * (16.0d+0) 
term(425) = term(425) * (-8.0d+0) 
term(426) = term(426) * (4.0d+0) 
term(427) = term(427) * (-8.0d+0) 
term(428) = term(428) * (4.0d+0) 
term(429) = term(429) * (-2.0d+0) 
term(430) = term(430) * (4.0d+0) 
term(432) = term(432) * (-2.0d+0) 
term(434) = term(434) * (-2.0d+0) 
term(435) = term(435) * (-4.0d+0) 
term(436) = term(436) * (8.0d+0) 
term(437) = term(437) * (4.0d+0) 
term(438) = term(438) * (-8.0d+0) 
term(439) = term(439) * (2.0d+0) 
term(440) = term(440) * (-4.0d+0) 
term(441) = term(441) * (-4.0d+0) 
term(442) = term(442) * (8.0d+0) 
term(443) = term(443) * (2.0d+0) 
term(444) = term(444) * (-4.0d+0) 
term(445) = term(445) * (2.0d+0) 
term(446) = term(446) * (-4.0d+0) 
term(447) = term(447) * (2.0d+0) 
term(448) = term(448) * (-4.0d+0) 
term(449) = term(449) * (8.0d+0) 
term(450) = term(450) * (-4.0d+0) 
term(451) = term(451) * (2.0d+0) 
term(452) = term(452) * (-4.0d+0) 
term(453) = term(453) * (-4.0d+0) 
term(454) = term(454) * (8.0d+0) 
term(455) = term(455) * (2.0d+0) 
term(456) = term(456) * (-4.0d+0) 
term(457) = term(457) * (4.0d+0) 
term(458) = term(458) * (-8.0d+0) 
term(459) = term(459) * (-8.0d+0) 
term(460) = term(460) * (16.0d+0) 
term(461) = term(461) * (4.0d+0) 
term(462) = term(462) * (-8.0d+0) 
term(463) = term(463) * (4.0d+0) 
term(464) = term(464) * (-8.0d+0) 
term(465) = term(465) * (4.0d+0) 
term(466) = term(466) * (-8.0d+0) 
term(467) = term(467) * (16.0d+0) 
term(468) = term(468) * (-8.0d+0) 
term(469) = term(469) * (8.0d+0) 
term(470) = term(470) * (-8.0d+0) 
term(471) = term(471) * (-16.0d+0) 
term(472) = term(472) * (16.0d+0) 
term(473) = term(473) * (8.0d+0) 
term(474) = term(474) * (-16.0d+0) 
term(475) = term(475) * (-4.0d+0) 
term(476) = term(476) * (8.0d+0) 
term(477) = term(477) * (-2.0d+0) 
term(478) = term(478) * (4.0d+0) 
term(479) = term(479) * (-2.0d+0) 
term(480) = term(480) * (4.0d+0) 
term(481) = term(481) * (4.0d+0) 
term(482) = term(482) * (-2.0d+0) 
term(483) = term(483) * (-2.0d+0) 
term(484) = term(484) * (4.0d+0) 
term(485) = term(485) * (4.0d+0) 
term(486) = term(486) * (-8.0d+0) 
term(487) = term(487) * (-2.0d+0) 
term(488) = term(488) * (4.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(489) = term(489) + r1(vrdav_Rl, a,i) * wm_interm_19_so_pt4(b,a) * wm_interm_28_so_pt4(b,q,i,p)
term(490) = term(490) + r1(vrdav_Rl, a,i) * wm_interm_20_so_pt4(b,a) * wm_interm_28_so_pt4(b,q,i,p)
term(491) = term(491) + r1(vrdav_Rl, a,i) * wm_interm_19_so_pt4(b,a) * wm_interm_27_so_pt4(b,q,i,p)
term(492) = term(492) + r1(vrdav_Rl, a,i) * wm_interm_20_so_pt4(b,a) * wm_interm_27_so_pt4(b,q,i,p)
term(493) = term(493) + r1(vrdav_Rl, a,i) * wm_interm_19_so_pt4(b,a) * wm_interm_31_so_pt4(b,q,i,p)
term(494) = term(494) + r1(vrdav_Rl, a,i) * wm_interm_20_so_pt4(b,a) * wm_interm_31_so_pt4(b,q,i,p)
term(495) = term(495) + r1(vrdav_Rl, a,i) * wm_interm_19_so_pt4(b,a) * wm_interm_30_so_pt4(b,q,i,p)
term(496) = term(496) + r1(vrdav_Rl, a,i) * wm_interm_20_so_pt4(b,a) * wm_interm_30_so_pt4(b,q,i,p)
term(497) = term(497) + r1(vrdav_Rl, a,p) * wm_interm_33_so_pt4(b,i) * wm_interm_5_so_pt4(b,a,i,q)
term(498) = term(498) + r1(vrdav_Rl, a,p) * wm_interm_34_so_pt4(b,i) * wm_interm_5_so_pt4(b,a,i,q)
term(499) = term(499) + r1(vrdav_Rl, a,p) * wm_interm_35_so_pt4(b,i) * wm_interm_5_so_pt4(b,a,i,q)
term(500) = term(500) + r1(vrdav_Rl, a,p) * wm_interm_36_so_pt4(b,i) * wm_interm_5_so_pt4(b,a,i,q)
term(501) = term(501) + r1(vrdav_Rl, a,p) * wm_interm_37_so_pt4(b,i) * wm_interm_5_so_pt4(b,a,i,q)
term(502) = term(502) + s1(a,p) * wm_interm_38_so_pt4(b,i) * wm_interm_5_so_pt4(b,a,i,q)
term(503) = term(503) + s1(a,p) * wm_interm_40_so_pt4(b,i) * wm_interm_5_so_pt4(b,a,i,q)
term(504) = term(504) + s1(a,p) * wm_interm_41_so_pt4(b,i) * wm_interm_5_so_pt4(b,a,i,q)
term(505) = term(505) + s1(a,p) * wm_interm_42_so_pt4(b,i) * wm_interm_5_so_pt4(b,a,i,q)
term(506) = term(506) + s1(a,p) * wm_interm_43_so_pt4(b,i) * wm_interm_5_so_pt4(b,a,i,q)
end do 
end do 
end do 

term(489) = term(489) * (2.0d+0) 
term(490) = term(490) * (-4.0d+0) 
term(491) = term(491) * (-1.0d+0) 
term(492) = term(492) * (2.0d+0) 
term(493) = term(493) * (4.0d+0) 
term(494) = term(494) * (-8.0d+0) 
term(495) = term(495) * (-2.0d+0) 
term(496) = term(496) * (4.0d+0) 
term(497) = term(497) * (-2.0d+0) 
term(500) = term(500) * (-4.0d+0) 
term(501) = term(501) * (4.0d+0) 
term(502) = term(502) * (2.0d+0) 
term(503) = term(503) * (-4.0d+0) 
term(504) = term(504) * (2.0d+0) 
term(505) = term(505) * (8.0d+0) 
term(506) = term(506) * (-8.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(507) = term(507) + r1(vrdav_Rl, a,i) * wm_interm_7_so_pt4(a,j) * wm_interm_88_so_pt4(q,i,j,p)
term(508) = term(508) + r1(vrdav_Rl, a,i) * wm_interm_7_so_pt4(a,j) * wm_interm_88_so_pt4(i,q,p,j)
term(509) = term(509) + r1(vrdav_Rl, a,i) * wm_interm_7_so_pt4(a,j) * wm_interm_88_so_pt4(q,i,p,j)
term(510) = term(510) + s1(a,i) * wm_interm_14_so_pt4(a,j) * wm_interm_88_so_pt4(q,i,j,p)
term(511) = term(511) + s1(a,i) * wm_interm_14_so_pt4(a,j) * wm_interm_88_so_pt4(i,q,p,j)
term(512) = term(512) + s1(a,i) * wm_interm_14_so_pt4(a,j) * wm_interm_88_so_pt4(q,i,p,j)
term(513) = term(513) + r1(vrdav_Rl, a,i) * wm_interm_6_so_pt4(a,j) * wm_interm_88_so_pt4(q,i,j,p)
term(514) = term(514) + r1(vrdav_Rl, a,i) * wm_interm_6_so_pt4(a,j) * wm_interm_88_so_pt4(i,q,p,j)
term(515) = term(515) + r1(vrdav_Rl, a,i) * wm_interm_6_so_pt4(a,j) * wm_interm_88_so_pt4(q,i,p,j)
term(516) = term(516) + s1(a,i) * wm_interm_16_so_pt4(a,j) * wm_interm_88_so_pt4(q,i,j,p)
term(517) = term(517) + s1(a,i) * wm_interm_16_so_pt4(a,j) * wm_interm_88_so_pt4(i,q,p,j)
term(518) = term(518) + s1(a,i) * wm_interm_16_so_pt4(a,j) * wm_interm_88_so_pt4(q,i,p,j)
term(519) = term(519) + r1(vrdav_Rl, a,i) * wm_interm_109_so_pt4(q,i,j,p) * wm_interm_7_so_pt4(a,j)
term(520) = term(520) + r1(vrdav_Rl, a,i) * wm_interm_109_so_pt4(i,q,p,j) * wm_interm_7_so_pt4(a,j)
term(521) = term(521) + r1(vrdav_Rl, a,i) * wm_interm_109_so_pt4(q,i,p,j) * wm_interm_7_so_pt4(a,j)
term(522) = term(522) + s1(a,i) * wm_interm_109_so_pt4(q,i,j,p) * wm_interm_14_so_pt4(a,j)
term(523) = term(523) + s1(a,i) * wm_interm_109_so_pt4(i,q,p,j) * wm_interm_14_so_pt4(a,j)
term(524) = term(524) + s1(a,i) * wm_interm_109_so_pt4(q,i,p,j) * wm_interm_14_so_pt4(a,j)
term(525) = term(525) + r1(vrdav_Rl, a,i) * wm_interm_109_so_pt4(q,i,j,p) * wm_interm_6_so_pt4(a,j)
term(526) = term(526) + r1(vrdav_Rl, a,i) * wm_interm_109_so_pt4(i,q,p,j) * wm_interm_6_so_pt4(a,j)
term(527) = term(527) + r1(vrdav_Rl, a,i) * wm_interm_109_so_pt4(q,i,p,j) * wm_interm_6_so_pt4(a,j)
term(528) = term(528) + s1(a,i) * wm_interm_109_so_pt4(q,i,j,p) * wm_interm_16_so_pt4(a,j)
term(529) = term(529) + s1(a,i) * wm_interm_109_so_pt4(i,q,p,j) * wm_interm_16_so_pt4(a,j)
term(530) = term(530) + s1(a,i) * wm_interm_109_so_pt4(q,i,p,j) * wm_interm_16_so_pt4(a,j)
term(531) = term(531) + s1(a,p) * wm_interm_13_so_pt4(a,i,q,j) * wm_interm_85_so_pt4(i,j)
term(532) = term(532) + s1(a,p) * wm_interm_13_so_pt4(a,i,q,j) * wm_interm_86_so_pt4(i,j)
term(533) = term(533) + s1(a,p) * wm_interm_13_so_pt4(a,i,q,j) * wm_interm_87_so_pt4(i,j)
term(534) = term(534) + s1(a,i) * wm_interm_13_so_pt4(a,p,j,q) * wm_interm_85_so_pt4(i,j)
term(535) = term(535) + s1(a,i) * wm_interm_13_so_pt4(a,p,j,q) * wm_interm_86_so_pt4(i,j)
term(536) = term(536) + s1(a,i) * wm_interm_13_so_pt4(a,p,j,q) * wm_interm_87_so_pt4(i,j)
term(537) = term(537) + s1(a,p) * wm_interm_106_so_pt4(i,j) * wm_interm_13_so_pt4(a,i,q,j)
term(538) = term(538) + s1(a,p) * wm_interm_107_so_pt4(i,j) * wm_interm_13_so_pt4(a,i,q,j)
term(539) = term(539) + s1(a,p) * wm_interm_108_so_pt4(i,j) * wm_interm_13_so_pt4(a,i,q,j)
term(540) = term(540) + s1(a,i) * wm_interm_106_so_pt4(i,j) * wm_interm_13_so_pt4(a,p,j,q)
term(541) = term(541) + s1(a,i) * wm_interm_107_so_pt4(i,j) * wm_interm_13_so_pt4(a,p,j,q)
term(542) = term(542) + s1(a,i) * wm_interm_108_so_pt4(i,j) * wm_interm_13_so_pt4(a,p,j,q)
term(543) = term(543) + t1(a,i) * wm_interm_12_so_pt4(a,q,j,p) * wm_interm_62_so_pt4(i,j)
term(544) = term(544) + t1(a,i) * wm_interm_12_so_pt4(a,q,p,j) * wm_interm_62_so_pt4(i,j)
term(545) = term(545) + t1(a,i) * wm_interm_12_so_pt4(a,q,j,p) * wm_interm_60_so_pt4(i,j)
term(546) = term(546) + t1(a,i) * wm_interm_12_so_pt4(a,q,p,j) * wm_interm_60_so_pt4(i,j)
term(547) = term(547) + t1(a,i) * wm_interm_17_so_pt4(i,j) * wm_interm_46_so_pt4(a,p,j,q)
term(548) = term(548) + t1(a,i) * wm_interm_18_so_pt4(i,j) * wm_interm_46_so_pt4(a,p,j,q)
term(549) = term(549) + t1(a,i) * wm_interm_17_so_pt4(i,j) * wm_interm_46_so_pt4(a,j,p,q)
term(550) = term(550) + t1(a,i) * wm_interm_18_so_pt4(i,j) * wm_interm_46_so_pt4(a,j,p,q)
term(551) = term(551) + t1(a,i) * wm_interm_17_so_pt4(i,j) * wm_interm_48_so_pt4(a,p,j,q)
term(552) = term(552) + t1(a,i) * wm_interm_18_so_pt4(i,j) * wm_interm_48_so_pt4(a,p,j,q)
term(553) = term(553) + t1(a,i) * wm_interm_17_so_pt4(i,j) * wm_interm_48_so_pt4(a,j,p,q)
term(554) = term(554) + t1(a,i) * wm_interm_18_so_pt4(i,j) * wm_interm_48_so_pt4(a,j,p,q)
term(555) = term(555) + t1(a,q) * wm_interm_12_so_pt4(a,i,p,j) * wm_interm_60_so_pt4(i,j)
term(556) = term(556) + t1(a,q) * wm_interm_12_so_pt4(a,i,p,j) * wm_interm_62_so_pt4(i,j)
term(557) = term(557) + t1(a,i) * wm_interm_2_so_pt4(a,j) * wm_interm_61_so_pt4(p,i,j,q)
term(558) = term(558) + t1(a,i) * wm_interm_45_so_pt4(a,j) * wm_interm_9_so_pt4(p,i,j,q)
term(559) = term(559) + t1(a,i) * wm_interm_2_so_pt4(a,j) * wm_interm_61_so_pt4(i,p,q,j)
term(560) = term(560) + r1(vrdav_Rr, a,i) * wm_interm_50_so_pt4(a,j) * wm_interm_9_so_pt4(p,i,j,q)
term(561) = term(561) + r1(vrdav_Rr, a,i) * wm_interm_51_so_pt4(a,j) * wm_interm_9_so_pt4(p,i,j,q)
end do 
end do 
end do 

term(507) = term(507) * (2.0d+0) 
term(508) = term(508) * (2.0d+0) 
term(509) = term(509) * (-4.0d+0) 
term(510) = term(510) * (-4.0d+0) 
term(511) = term(511) * (-4.0d+0) 
term(512) = term(512) * (8.0d+0) 
term(513) = term(513) * (-4.0d+0) 
term(514) = term(514) * (-4.0d+0) 
term(515) = term(515) * (8.0d+0) 
term(516) = term(516) * (2.0d+0) 
term(517) = term(517) * (2.0d+0) 
term(518) = term(518) * (-4.0d+0) 
term(519) = term(519) * (4.0d+0) 
term(520) = term(520) * (4.0d+0) 
term(521) = term(521) * (-8.0d+0) 
term(522) = term(522) * (-8.0d+0) 
term(523) = term(523) * (-8.0d+0) 
term(524) = term(524) * (16.0d+0) 
term(525) = term(525) * (-8.0d+0) 
term(526) = term(526) * (-8.0d+0) 
term(527) = term(527) * (16.0d+0) 
term(528) = term(528) * (4.0d+0) 
term(529) = term(529) * (4.0d+0) 
term(530) = term(530) * (-8.0d+0) 
term(531) = term(531) * (2.0d+0) 
term(532) = term(532) * (-4.0d+0) 
term(533) = term(533) * (2.0d+0) 
term(534) = term(534) * (2.0d+0) 
term(535) = term(535) * (-4.0d+0) 
term(536) = term(536) * (2.0d+0) 
term(537) = term(537) * (4.0d+0) 
term(538) = term(538) * (-8.0d+0) 
term(539) = term(539) * (4.0d+0) 
term(540) = term(540) * (4.0d+0) 
term(541) = term(541) * (-8.0d+0) 
term(542) = term(542) * (4.0d+0) 
term(543) = term(543) * (8.0d+0) 
term(544) = term(544) * (-16.0d+0) 
term(545) = term(545) * (-4.0d+0) 
term(546) = term(546) * (8.0d+0) 
term(547) = term(547) * (4.0d+0) 
term(548) = term(548) * (-8.0d+0) 
term(549) = term(549) * (-2.0d+0) 
term(550) = term(550) * (4.0d+0) 
term(551) = term(551) * (-2.0d+0) 
term(552) = term(552) * (4.0d+0) 
term(553) = term(553) * (4.0d+0) 
term(554) = term(554) * (-8.0d+0) 
term(555) = term(555) * (-2.0d+0) 
term(556) = term(556) * (4.0d+0) 
term(557) = term(557) * (-1.0d+0) 
term(558) = term(558) * (-2.0d+0) 
term(559) = term(559) * (-1.0d+0) 
term(560) = term(560) * (4.0d+0) 
term(561) = term(561) * (-2.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(562) = term(562) + s1(a,i) * wm_interm_13_so_pt4(b,p,j,i) * wm_interm_67_so_pt4(a,b,q,j)
term(563) = term(563) + s1(a,i) * wm_interm_13_so_pt4(b,p,j,i) * wm_interm_68_so_pt4(a,b,q,j)
term(564) = term(564) + s1(a,i) * wm_interm_13_so_pt4(b,p,j,i) * wm_interm_74_so_pt4(a,b,q,j)
term(565) = term(565) + s1(a,i) * wm_interm_13_so_pt4(b,p,j,i) * wm_interm_69_so_pt4(a,b,q,j)
term(566) = term(566) + s1(a,i) * wm_interm_13_so_pt4(b,p,j,i) * wm_interm_73_so_pt4(a,b,q,j)
term(567) = term(567) + s1(a,i) * wm_interm_13_so_pt4(b,p,j,i) * wm_interm_84_so_pt4(a,b,q,j)
term(568) = term(568) + s1(a,i) * wm_interm_13_so_pt4(b,p,i,j) * wm_interm_70_so_pt4(a,b,q,j)
term(569) = term(569) + s1(a,i) * wm_interm_13_so_pt4(b,p,i,j) * wm_interm_69_so_pt4(a,b,q,j)
term(570) = term(570) + s1(a,i) * wm_interm_13_so_pt4(b,p,i,j) * wm_interm_67_so_pt4(a,b,q,j)
term(571) = term(571) + s1(a,i) * wm_interm_13_so_pt4(b,p,i,j) * wm_interm_68_so_pt4(a,b,q,j)
term(572) = term(572) + s1(a,i) * wm_interm_13_so_pt4(b,p,i,j) * wm_interm_84_so_pt4(a,b,q,j)
term(573) = term(573) + s1(a,i) * wm_interm_13_so_pt4(b,p,i,j) * wm_interm_74_so_pt4(a,b,q,j)
term(574) = term(574) + s1(a,i) * wm_interm_13_so_pt4(b,p,j,i) * wm_interm_95_so_pt4(a,b,q,j)
term(575) = term(575) + s1(a,i) * wm_interm_13_so_pt4(b,p,j,i) * wm_interm_96_so_pt4(a,b,q,j)
term(576) = term(576) + s1(a,i) * wm_interm_13_so_pt4(b,p,j,i) * wm_interm_97_so_pt4(a,b,q,j)
term(577) = term(577) + s1(a,i) * wm_interm_13_so_pt4(b,p,j,i) * wm_interm_98_so_pt4(a,b,q,j)
term(578) = term(578) + s1(a,i) * wm_interm_13_so_pt4(b,p,i,j) * wm_interm_98_so_pt4(a,b,q,j)
term(579) = term(579) + s1(a,i) * wm_interm_13_so_pt4(b,p,i,j) * wm_interm_97_so_pt4(a,b,q,j)
term(580) = term(580) + s1(a,i) * wm_interm_13_so_pt4(b,p,i,j) * wm_interm_95_so_pt4(a,b,q,j)
term(581) = term(581) + s1(a,i) * wm_interm_13_so_pt4(b,p,i,j) * wm_interm_96_so_pt4(a,b,q,j)
term(582) = term(582) + r1(vrdav_Rl, a,i) * wm_interm_3_so_pt4(b,j,i,q) * wm_interm_84_so_pt4(a,b,j,p)
term(583) = term(583) + r1(vrdav_Rl, a,i) * wm_interm_3_so_pt4(b,j,i,q) * wm_interm_73_so_pt4(a,b,j,p)
term(584) = term(584) + r1(vrdav_Rl, a,i) * wm_interm_3_so_pt4(b,j,i,q) * wm_interm_70_so_pt4(a,b,j,p)
term(585) = term(585) + s1(a,i) * wm_interm_13_so_pt4(b,j,i,q) * wm_interm_70_so_pt4(a,b,j,p)
term(586) = term(586) + s1(a,i) * wm_interm_13_so_pt4(b,j,i,q) * wm_interm_84_so_pt4(a,b,j,p)
term(587) = term(587) + s1(a,i) * wm_interm_13_so_pt4(b,j,i,q) * wm_interm_73_so_pt4(a,b,j,p)
term(588) = term(588) + s1(a,p) * wm_interm_13_so_pt4(b,i,q,j) * wm_interm_70_so_pt4(a,b,i,j)
term(589) = term(589) + s1(a,p) * wm_interm_13_so_pt4(b,i,q,j) * wm_interm_69_so_pt4(a,b,i,j)
term(590) = term(590) + s1(a,p) * wm_interm_13_so_pt4(b,i,q,j) * wm_interm_67_so_pt4(a,b,i,j)
term(591) = term(591) + s1(a,p) * wm_interm_13_so_pt4(b,i,q,j) * wm_interm_68_so_pt4(a,b,i,j)
term(592) = term(592) + s1(a,p) * wm_interm_13_so_pt4(b,i,q,j) * wm_interm_84_so_pt4(a,b,i,j)
term(593) = term(593) + s1(a,p) * wm_interm_13_so_pt4(b,i,q,j) * wm_interm_74_so_pt4(a,b,i,j)
term(594) = term(594) + s1(a,p) * wm_interm_13_so_pt4(b,i,j,q) * wm_interm_67_so_pt4(a,b,i,j)
term(595) = term(595) + s1(a,p) * wm_interm_13_so_pt4(b,i,j,q) * wm_interm_68_so_pt4(a,b,i,j)
term(596) = term(596) + s1(a,p) * wm_interm_13_so_pt4(b,i,j,q) * wm_interm_69_so_pt4(a,b,i,j)
term(597) = term(597) + s1(a,p) * wm_interm_13_so_pt4(b,i,j,q) * wm_interm_70_so_pt4(a,b,i,j)
term(598) = term(598) + s1(a,p) * wm_interm_13_so_pt4(b,i,j,q) * wm_interm_73_so_pt4(a,b,i,j)
term(599) = term(599) + s1(a,p) * wm_interm_13_so_pt4(b,i,j,q) * wm_interm_74_so_pt4(a,b,i,j)
term(600) = term(600) + r1(vrdav_Rl, a,i) * wm_interm_3_so_pt4(b,j,i,q) * wm_interm_98_so_pt4(a,b,j,p)
term(601) = term(601) + r1(vrdav_Rl, a,i) * wm_interm_3_so_pt4(b,j,i,q) * wm_interm_97_so_pt4(a,b,j,p)
term(602) = term(602) + s1(a,i) * wm_interm_13_so_pt4(b,j,i,q) * wm_interm_98_so_pt4(a,b,j,p)
term(603) = term(603) + s1(a,i) * wm_interm_13_so_pt4(b,j,i,q) * wm_interm_97_so_pt4(a,b,j,p)
term(604) = term(604) + s1(a,p) * wm_interm_13_so_pt4(b,i,q,j) * wm_interm_98_so_pt4(a,b,i,j)
term(605) = term(605) + s1(a,p) * wm_interm_13_so_pt4(b,i,q,j) * wm_interm_97_so_pt4(a,b,i,j)
term(606) = term(606) + s1(a,p) * wm_interm_13_so_pt4(b,i,q,j) * wm_interm_95_so_pt4(a,b,i,j)
term(607) = term(607) + s1(a,p) * wm_interm_13_so_pt4(b,i,q,j) * wm_interm_96_so_pt4(a,b,i,j)
term(608) = term(608) + s1(a,p) * wm_interm_13_so_pt4(b,i,j,q) * wm_interm_95_so_pt4(a,b,i,j)
term(609) = term(609) + s1(a,p) * wm_interm_13_so_pt4(b,i,j,q) * wm_interm_96_so_pt4(a,b,i,j)
term(610) = term(610) + s1(a,p) * wm_interm_13_so_pt4(b,i,j,q) * wm_interm_97_so_pt4(a,b,i,j)
term(611) = term(611) + s1(a,p) * wm_interm_13_so_pt4(b,i,j,q) * wm_interm_98_so_pt4(a,b,i,j)
term(612) = term(612) + t1(a,i) * wm_interm_10_so_pt4(a,b,p,j) * wm_interm_46_so_pt4(b,j,i,q)
term(613) = term(613) + t1(a,i) * wm_interm_11_so_pt4(a,b,p,j) * wm_interm_46_so_pt4(b,j,i,q)
term(614) = term(614) + t1(a,i) * wm_interm_10_so_pt4(a,b,p,j) * wm_interm_48_so_pt4(b,j,i,q)
term(615) = term(615) + t1(a,i) * wm_interm_11_so_pt4(a,b,p,j) * wm_interm_48_so_pt4(b,j,i,q)
term(616) = term(616) + t1(a,i) * wm_interm_1_so_pt4(a,b,p,j) * wm_interm_46_so_pt4(b,j,i,q)
term(617) = term(617) + t1(a,i) * wm_interm_46_so_pt4(b,j,i,q) * wm_interm_5_so_pt4(a,b,p,j)
term(618) = term(618) + t1(a,i) * wm_interm_48_so_pt4(b,j,i,q) * wm_interm_5_so_pt4(a,b,p,j)
term(619) = term(619) + t1(a,i) * wm_interm_1_so_pt4(a,b,p,j) * wm_interm_48_so_pt4(b,j,i,q)
term(620) = term(620) + r1(vrdav_Rr, a,q) * wm_interm_10_so_pt4(a,b,i,j) * wm_interm_47_so_pt4(b,p,j,i)
term(621) = term(621) + r1(vrdav_Rr, a,q) * wm_interm_10_so_pt4(a,b,i,j) * wm_interm_47_so_pt4(b,j,p,i)
term(622) = term(622) + r1(vrdav_Rr, a,q) * wm_interm_11_so_pt4(a,b,i,j) * wm_interm_47_so_pt4(b,p,j,i)
term(623) = term(623) + r1(vrdav_Rr, a,q) * wm_interm_11_so_pt4(a,b,i,j) * wm_interm_47_so_pt4(b,j,p,i)
term(624) = term(624) + r1(vrdav_Rr, a,q) * wm_interm_1_so_pt4(a,b,i,j) * wm_interm_47_so_pt4(b,p,j,i)
term(625) = term(625) + r1(vrdav_Rr, a,q) * wm_interm_47_so_pt4(b,p,i,j) * wm_interm_5_so_pt4(a,b,j,i)
term(626) = term(626) + r1(vrdav_Rr, a,q) * wm_interm_47_so_pt4(b,i,p,j) * wm_interm_5_so_pt4(a,b,j,i)
term(627) = term(627) + r1(vrdav_Rr, a,q) * wm_interm_1_so_pt4(a,b,i,j) * wm_interm_47_so_pt4(b,j,p,i)
term(628) = term(628) + r1(vrdav_Rr, a,q) * wm_interm_10_so_pt4(a,b,i,j) * wm_interm_49_so_pt4(b,p,j,i)
term(629) = term(629) + r1(vrdav_Rr, a,q) * wm_interm_11_so_pt4(a,b,i,j) * wm_interm_49_so_pt4(b,p,j,i)
term(630) = term(630) + r1(vrdav_Rr, a,q) * wm_interm_10_so_pt4(a,b,i,j) * wm_interm_49_so_pt4(b,j,p,i)
term(631) = term(631) + r1(vrdav_Rr, a,q) * wm_interm_11_so_pt4(a,b,i,j) * wm_interm_49_so_pt4(b,j,p,i)
term(632) = term(632) + r1(vrdav_Rr, a,q) * wm_interm_49_so_pt4(b,p,i,j) * wm_interm_5_so_pt4(a,b,j,i)
term(633) = term(633) + r1(vrdav_Rr, a,q) * wm_interm_1_so_pt4(a,b,i,j) * wm_interm_49_so_pt4(b,p,j,i)
term(634) = term(634) + r1(vrdav_Rr, a,q) * wm_interm_1_so_pt4(a,b,i,j) * wm_interm_49_so_pt4(b,j,p,i)
term(635) = term(635) + r1(vrdav_Rr, a,q) * wm_interm_49_so_pt4(b,i,p,j) * wm_interm_5_so_pt4(a,b,j,i)
term(636) = term(636) + t1(a,q) * wm_interm_10_so_pt4(a,b,i,j) * wm_interm_46_so_pt4(b,p,j,i)
term(637) = term(637) + t1(a,q) * wm_interm_11_so_pt4(a,b,i,j) * wm_interm_46_so_pt4(b,p,j,i)
term(638) = term(638) + r1(vrdav_Rr, a,i) * wm_interm_10_so_pt4(a,b,p,j) * wm_interm_49_so_pt4(b,j,i,q)
term(639) = term(639) + r1(vrdav_Rr, a,i) * wm_interm_11_so_pt4(a,b,p,j) * wm_interm_49_so_pt4(b,j,i,q)
term(640) = term(640) + t1(a,q) * wm_interm_10_so_pt4(a,b,i,j) * wm_interm_46_so_pt4(b,j,p,i)
term(641) = term(641) + t1(a,q) * wm_interm_11_so_pt4(a,b,i,j) * wm_interm_46_so_pt4(b,j,p,i)
term(642) = term(642) + t1(a,q) * wm_interm_10_so_pt4(a,b,i,j) * wm_interm_48_so_pt4(b,p,j,i)
term(643) = term(643) + t1(a,q) * wm_interm_11_so_pt4(a,b,i,j) * wm_interm_48_so_pt4(b,p,j,i)
term(644) = term(644) + r1(vrdav_Rr, a,i) * wm_interm_10_so_pt4(a,b,p,j) * wm_interm_47_so_pt4(b,j,i,q)
term(645) = term(645) + r1(vrdav_Rr, a,i) * wm_interm_11_so_pt4(a,b,p,j) * wm_interm_47_so_pt4(b,j,i,q)
term(646) = term(646) + t1(a,q) * wm_interm_10_so_pt4(a,b,i,j) * wm_interm_48_so_pt4(b,j,p,i)
term(647) = term(647) + t1(a,q) * wm_interm_11_so_pt4(a,b,i,j) * wm_interm_48_so_pt4(b,j,p,i)
term(648) = term(648) + t1(a,q) * wm_interm_1_so_pt4(a,b,i,j) * wm_interm_48_so_pt4(b,p,j,i)
term(649) = term(649) + t1(a,q) * wm_interm_46_so_pt4(b,p,i,j) * wm_interm_5_so_pt4(a,b,j,i)
term(650) = term(650) + t1(a,q) * wm_interm_1_so_pt4(a,b,i,j) * wm_interm_46_so_pt4(b,p,j,i)
term(651) = term(651) + r1(vrdav_Rr, a,i) * wm_interm_1_so_pt4(a,b,p,j) * wm_interm_49_so_pt4(b,j,i,q)
term(652) = term(652) + t1(a,q) * wm_interm_1_so_pt4(a,b,i,j) * wm_interm_46_so_pt4(b,j,p,i)
term(653) = term(653) + r1(vrdav_Rr, a,i) * wm_interm_47_so_pt4(b,j,i,q) * wm_interm_5_so_pt4(a,b,p,j)
term(654) = term(654) + r1(vrdav_Rr, a,i) * wm_interm_1_so_pt4(a,b,p,j) * wm_interm_47_so_pt4(b,j,i,q)
term(655) = term(655) + t1(a,q) * wm_interm_48_so_pt4(b,i,p,j) * wm_interm_5_so_pt4(a,b,j,i)
term(656) = term(656) + t1(a,q) * wm_interm_1_so_pt4(a,b,i,j) * wm_interm_48_so_pt4(b,j,p,i)
term(657) = term(657) + t1(a,i) * wm_interm_48_so_pt4(b,p,i,j) * wm_interm_5_so_pt4(a,b,j,q)
term(658) = term(658) + r1(vrdav_Rr, a,i) * wm_interm_113_so_pt4(b,p,i,j) * wm_interm_54_so_pt4(a,b,j,q)
term(659) = term(659) + r1(vrdav_Rr, a,i) * wm_interm_22_so_pt4(b,p,i,j) * wm_interm_56_so_pt4(a,b,j,q)
term(660) = term(660) + r1(vrdav_Rr, a,i) * wm_interm_113_so_pt4(b,p,i,j) * wm_interm_55_so_pt4(a,b,j,q)
term(661) = term(661) + t1(a,i) * wm_interm_1_so_pt4(a,b,j,q) * wm_interm_46_so_pt4(b,p,i,j)
term(662) = term(662) + t1(a,i) * wm_interm_46_so_pt4(b,p,i,j) * wm_interm_5_so_pt4(a,b,j,q)
term(663) = term(663) + t1(a,i) * wm_interm_10_so_pt4(a,b,j,q) * wm_interm_46_so_pt4(b,p,i,j)
term(664) = term(664) + t1(a,i) * wm_interm_11_so_pt4(a,b,j,q) * wm_interm_46_so_pt4(b,p,i,j)
term(665) = term(665) + r1(vrdav_Rr, a,i) * wm_interm_47_so_pt4(b,p,i,j) * wm_interm_5_so_pt4(a,b,j,q)
term(666) = term(666) + t1(a,i) * wm_interm_12_so_pt4(b,j,i,p) * wm_interm_56_so_pt4(a,b,j,q)
term(667) = term(667) + r1(vrdav_Rr, a,i) * wm_interm_113_so_pt4(b,p,i,j) * wm_interm_59_so_pt4(a,b,j,q)
term(668) = term(668) + r1(vrdav_Rr, a,i) * wm_interm_10_so_pt4(a,b,j,q) * wm_interm_49_so_pt4(b,p,i,j)
term(669) = term(669) + r1(vrdav_Rr, a,i) * wm_interm_1_so_pt4(a,b,j,q) * wm_interm_49_so_pt4(b,p,i,j)
term(670) = term(670) + r1(vrdav_Rr, a,i) * wm_interm_11_so_pt4(a,b,j,q) * wm_interm_49_so_pt4(b,p,i,j)
end do 
end do 
end do 
end do 

term(562) = term(562) * (2.0d+0) 
term(563) = term(563) * (-4.0d+0) 
term(564) = term(564) * (2.0d+0) 
term(565) = term(565) * (2.0d+0) 
term(566) = term(566) * (2.0d+0) 
term(567) = term(567) * (-4.0d+0) 
term(568) = term(568) * (2.0d+0) 
term(569) = term(569) * (-4.0d+0) 
term(570) = term(570) * (-4.0d+0) 
term(571) = term(571) * (8.0d+0) 
term(572) = term(572) * (2.0d+0) 
term(573) = term(573) * (-4.0d+0) 
term(574) = term(574) * (8.0d+0) 
term(575) = term(575) * (-8.0d+0) 
term(576) = term(576) * (8.0d+0) 
term(577) = term(577) * (-8.0d+0) 
term(578) = term(578) * (8.0d+0) 
term(579) = term(579) * (-8.0d+0) 
term(580) = term(580) * (-16.0d+0) 
term(581) = term(581) * (16.0d+0) 
term(582) = term(582) * (2.0d+0) 
term(583) = term(583) * (-4.0d+0) 
term(584) = term(584) * (2.0d+0) 
term(585) = term(585) * (2.0d+0) 
term(586) = term(586) * (2.0d+0) 
term(587) = term(587) * (-4.0d+0) 
term(588) = term(588) * (2.0d+0) 
term(589) = term(589) * (-4.0d+0) 
term(590) = term(590) * (-4.0d+0) 
term(591) = term(591) * (8.0d+0) 
term(592) = term(592) * (2.0d+0) 
term(593) = term(593) * (-4.0d+0) 
term(594) = term(594) * (2.0d+0) 
term(595) = term(595) * (-4.0d+0) 
term(596) = term(596) * (2.0d+0) 
term(597) = term(597) * (-4.0d+0) 
term(598) = term(598) * (2.0d+0) 
term(599) = term(599) * (2.0d+0) 
term(600) = term(600) * (8.0d+0) 
term(601) = term(601) * (-8.0d+0) 
term(602) = term(602) * (8.0d+0) 
term(603) = term(603) * (-8.0d+0) 
term(604) = term(604) * (8.0d+0) 
term(605) = term(605) * (-8.0d+0) 
term(606) = term(606) * (-16.0d+0) 
term(607) = term(607) * (16.0d+0) 
term(608) = term(608) * (8.0d+0) 
term(609) = term(609) * (-8.0d+0) 
term(610) = term(610) * (8.0d+0) 
term(611) = term(611) * (-8.0d+0) 
term(612) = term(612) * (-2.0d+0) 
term(613) = term(613) * (4.0d+0) 
term(614) = term(614) * (4.0d+0) 
term(615) = term(615) * (-8.0d+0) 
term(616) = term(616) * (-2.0d+0) 
term(617) = term(617) * (4.0d+0) 
term(618) = term(618) * (-2.0d+0) 
term(619) = term(619) * (4.0d+0) 
term(620) = term(620) * (-2.0d+0) 
term(621) = term(621) * (4.0d+0) 
term(622) = term(622) * (4.0d+0) 
term(623) = term(623) * (-8.0d+0) 
term(624) = term(624) * (-2.0d+0) 
term(625) = term(625) * (4.0d+0) 
term(626) = term(626) * (-2.0d+0) 
term(627) = term(627) * (4.0d+0) 
term(628) = term(628) * (4.0d+0) 
term(629) = term(629) * (-8.0d+0) 
term(630) = term(630) * (-2.0d+0) 
term(631) = term(631) * (4.0d+0) 
term(632) = term(632) * (-2.0d+0) 
term(633) = term(633) * (4.0d+0) 
term(634) = term(634) * (-2.0d+0) 
term(635) = term(635) * (4.0d+0) 
term(636) = term(636) * (2.0d+0) 
term(637) = term(637) * (-4.0d+0) 
term(638) = term(638) * (-1.0d+0) 
term(639) = term(639) * (2.0d+0) 
term(640) = term(640) * (-1.0d+0) 
term(641) = term(641) * (2.0d+0) 
term(642) = term(642) * (-1.0d+0) 
term(643) = term(643) * (2.0d+0) 
term(644) = term(644) * (2.0d+0) 
term(645) = term(645) * (-4.0d+0) 
term(646) = term(646) * (2.0d+0) 
term(647) = term(647) * (-4.0d+0) 
term(648) = term(648) * (-1.0d+0) 
term(649) = term(649) * (-1.0d+0) 
term(650) = term(650) * (2.0d+0) 
term(651) = term(651) * (-1.0d+0) 
term(652) = term(652) * (-1.0d+0) 
term(653) = term(653) * (-1.0d+0) 
term(654) = term(654) * (2.0d+0) 
term(655) = term(655) * (-1.0d+0) 
term(656) = term(656) * (2.0d+0) 
term(657) = term(657) * (-1.0d+0) 
term(658) = term(658) * (4.0d+0) 
term(659) = term(659) * (-1.0d+0) 
term(660) = term(660) * (-2.0d+0) 
term(661) = term(661) * (-1.0d+0) 
term(662) = term(662) * (2.0d+0) 
term(663) = term(663) * (-1.0d+0) 
term(664) = term(664) * (2.0d+0) 
term(665) = term(665) * (-1.0d+0) 
term(666) = term(666) * (-2.0d+0) 
term(667) = term(667) * (-1.0d+0) 
term(668) = term(668) * (-1.0d+0) 
term(669) = term(669) * (-1.0d+0) 
term(670) = term(670) * (2.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(671) = term(671) + s1(a,p) * wm_interm_24_so_pt4(b,i,j,q) * wm_interm_5_so_pt4(b,a,j,i)
term(672) = term(672) + s1(a,p) * wm_interm_29_so_pt4(b,i,j,q) * wm_interm_5_so_pt4(b,a,j,i)
term(673) = term(673) + s1(a,i) * wm_interm_1_so_pt4(b,a,j,q) * wm_interm_26_so_pt4(b,p,j,i)
term(674) = term(674) + s1(a,i) * wm_interm_24_so_pt4(b,p,j,i) * wm_interm_5_so_pt4(b,a,j,q)
term(675) = term(675) + s1(a,i) * wm_interm_26_so_pt4(b,p,j,i) * wm_interm_5_so_pt4(b,a,j,q)
term(676) = term(676) + s1(a,i) * wm_interm_10_so_pt4(b,a,j,q) * wm_interm_26_so_pt4(b,p,j,i)
term(677) = term(677) + s1(a,i) * wm_interm_11_so_pt4(b,a,j,q) * wm_interm_26_so_pt4(b,p,j,i)
term(678) = term(678) + s1(a,i) * wm_interm_1_so_pt4(b,a,j,q) * wm_interm_29_so_pt4(b,p,j,i)
term(679) = term(679) + s1(a,i) * wm_interm_29_so_pt4(b,p,j,i) * wm_interm_5_so_pt4(b,a,j,q)
term(680) = term(680) + s1(a,i) * wm_interm_10_so_pt4(b,a,j,q) * wm_interm_29_so_pt4(b,p,j,i)
term(681) = term(681) + s1(a,i) * wm_interm_11_so_pt4(b,a,j,q) * wm_interm_29_so_pt4(b,p,j,i)
end do 
end do 
end do 
end do 

term(671) = term(671) * (2.0d+0) 
term(672) = term(672) * (-8.0d+0) 
term(673) = term(673) * (2.0d+0) 
term(674) = term(674) * (2.0d+0) 
term(675) = term(675) * (-4.0d+0) 
term(676) = term(676) * (2.0d+0) 
term(677) = term(677) * (-4.0d+0) 
term(678) = term(678) * (8.0d+0) 
term(679) = term(679) * (-8.0d+0) 
term(680) = term(680) * (8.0d+0) 
term(681) = term(681) * (-16.0d+0) 

do j = 1, nocc 
do a = nocc + 1, nactive 
do i = 1, nocc 
do b = nocc + 1, nactive 
term(682) = term(682) + r1(vrdav_Rl, a,i) * wm_interm_1_so_pt4(b,a,p,j) * wm_interm_27_so_pt4(b,i,q,j)
term(683) = term(683) + r1(vrdav_Rl, a,i) * wm_interm_28_so_pt4(b,i,q,j) * wm_interm_5_so_pt4(b,a,p,j)
term(684) = term(684) + r1(vrdav_Rl, a,i) * wm_interm_10_so_pt4(b,a,p,j) * wm_interm_27_so_pt4(b,i,q,j)
term(685) = term(685) + r1(vrdav_Rl, a,i) * wm_interm_11_so_pt4(b,a,p,j) * wm_interm_27_so_pt4(b,i,q,j)
term(686) = term(686) + r1(vrdav_Rl, a,i) * wm_interm_1_so_pt4(b,a,p,j) * wm_interm_30_so_pt4(b,i,q,j)
term(687) = term(687) + r1(vrdav_Rl, a,i) * wm_interm_31_so_pt4(b,i,q,j) * wm_interm_5_so_pt4(b,a,p,j)
term(688) = term(688) + r1(vrdav_Rl, a,i) * wm_interm_10_so_pt4(b,a,p,j) * wm_interm_30_so_pt4(b,i,q,j)
term(689) = term(689) + r1(vrdav_Rl, a,i) * wm_interm_11_so_pt4(b,a,p,j) * wm_interm_30_so_pt4(b,i,q,j)
end do 
end do 
end do 
end do 

term(682) = term(682) * (2.0d+0) 
term(683) = term(683) * (2.0d+0) 
term(684) = term(684) * (2.0d+0) 
term(685) = term(685) * (-4.0d+0) 
term(686) = term(686) * (4.0d+0) 
term(687) = term(687) * (4.0d+0) 
term(688) = term(688) * (4.0d+0) 
term(689) = term(689) * (-8.0d+0) 

do a = nocc + 1, nactive 
do i = 1, nocc 
do b = nocc + 1, nactive 
term(690) = term(690) + r1(vrdav_Rl, a,i) * wm_interm_19_so_pt4(b,a) * wm_interm_28_so_pt4(b,i,q,p)
term(691) = term(691) + r1(vrdav_Rl, a,i) * wm_interm_20_so_pt4(b,a) * wm_interm_28_so_pt4(b,i,q,p)
term(692) = term(692) + r1(vrdav_Rl, a,i) * wm_interm_19_so_pt4(b,a) * wm_interm_31_so_pt4(b,i,q,p)
term(693) = term(693) + r1(vrdav_Rl, a,i) * wm_interm_20_so_pt4(b,a) * wm_interm_31_so_pt4(b,i,q,p)
term(694) = term(694) + r1(vrdav_Rl, a,i) * wm_interm_10_so_pt4(b,a,p,q) * wm_interm_33_so_pt4(b,i)
term(695) = term(695) + r1(vrdav_Rl, a,i) * wm_interm_11_so_pt4(b,a,p,q) * wm_interm_33_so_pt4(b,i)
term(696) = term(696) + r1(vrdav_Rl, a,i) * wm_interm_10_so_pt4(b,a,p,q) * wm_interm_34_so_pt4(b,i)
term(697) = term(697) + r1(vrdav_Rl, a,i) * wm_interm_11_so_pt4(b,a,p,q) * wm_interm_34_so_pt4(b,i)
term(698) = term(698) + r1(vrdav_Rl, a,i) * wm_interm_10_so_pt4(b,a,p,q) * wm_interm_35_so_pt4(b,i)
term(699) = term(699) + r1(vrdav_Rl, a,i) * wm_interm_11_so_pt4(b,a,p,q) * wm_interm_35_so_pt4(b,i)
term(700) = term(700) + r1(vrdav_Rl, a,i) * wm_interm_1_so_pt4(b,a,p,q) * wm_interm_33_so_pt4(b,i)
term(701) = term(701) + r1(vrdav_Rl, a,i) * wm_interm_1_so_pt4(b,a,p,q) * wm_interm_34_so_pt4(b,i)
term(702) = term(702) + r1(vrdav_Rl, a,i) * wm_interm_1_so_pt4(b,a,p,q) * wm_interm_35_so_pt4(b,i)
term(703) = term(703) + r1(vrdav_Rl, a,i) * wm_interm_10_so_pt4(b,a,p,q) * wm_interm_36_so_pt4(b,i)
term(704) = term(704) + r1(vrdav_Rl, a,i) * wm_interm_11_so_pt4(b,a,p,q) * wm_interm_36_so_pt4(b,i)
term(705) = term(705) + r1(vrdav_Rl, a,i) * wm_interm_10_so_pt4(b,a,p,q) * wm_interm_37_so_pt4(b,i)
term(706) = term(706) + r1(vrdav_Rl, a,i) * wm_interm_11_so_pt4(b,a,p,q) * wm_interm_37_so_pt4(b,i)
term(707) = term(707) + r1(vrdav_Rl, a,i) * wm_interm_1_so_pt4(b,a,p,q) * wm_interm_36_so_pt4(b,i)
term(708) = term(708) + r1(vrdav_Rl, a,i) * wm_interm_1_so_pt4(b,a,p,q) * wm_interm_37_so_pt4(b,i)
term(709) = term(709) + s1(a,i) * wm_interm_1_so_pt4(b,a,p,q) * wm_interm_38_so_pt4(b,i)
term(710) = term(710) + s1(a,i) * wm_interm_1_so_pt4(b,a,p,q) * wm_interm_40_so_pt4(b,i)
term(711) = term(711) + s1(a,i) * wm_interm_1_so_pt4(b,a,p,q) * wm_interm_41_so_pt4(b,i)
term(712) = term(712) + s1(a,i) * wm_interm_10_so_pt4(b,a,p,q) * wm_interm_38_so_pt4(b,i)
term(713) = term(713) + s1(a,i) * wm_interm_10_so_pt4(b,a,p,q) * wm_interm_40_so_pt4(b,i)
term(714) = term(714) + s1(a,i) * wm_interm_10_so_pt4(b,a,p,q) * wm_interm_41_so_pt4(b,i)
term(715) = term(715) + s1(a,i) * wm_interm_11_so_pt4(b,a,p,q) * wm_interm_38_so_pt4(b,i)
term(716) = term(716) + s1(a,i) * wm_interm_11_so_pt4(b,a,p,q) * wm_interm_40_so_pt4(b,i)
term(717) = term(717) + s1(a,i) * wm_interm_11_so_pt4(b,a,p,q) * wm_interm_41_so_pt4(b,i)
term(718) = term(718) + s1(a,i) * wm_interm_1_so_pt4(b,a,p,q) * wm_interm_42_so_pt4(b,i)
term(719) = term(719) + s1(a,i) * wm_interm_1_so_pt4(b,a,p,q) * wm_interm_43_so_pt4(b,i)
term(720) = term(720) + s1(a,i) * wm_interm_10_so_pt4(b,a,p,q) * wm_interm_42_so_pt4(b,i)
term(721) = term(721) + s1(a,i) * wm_interm_10_so_pt4(b,a,p,q) * wm_interm_43_so_pt4(b,i)
term(722) = term(722) + s1(a,i) * wm_interm_11_so_pt4(b,a,p,q) * wm_interm_42_so_pt4(b,i)
term(723) = term(723) + s1(a,i) * wm_interm_11_so_pt4(b,a,p,q) * wm_interm_43_so_pt4(b,i)
end do 
end do 
end do 

term(690) = term(690) * (-1.0d+0) 
term(691) = term(691) * (2.0d+0) 
term(692) = term(692) * (-2.0d+0) 
term(693) = term(693) * (4.0d+0) 
term(694) = term(694) * (-2.0d+0) 
term(695) = term(695) * (4.0d+0) 
term(697) = term(697) * (-2.0d+0) 
term(699) = term(699) * (-2.0d+0) 
term(700) = term(700) * (-2.0d+0) 
term(703) = term(703) * (-4.0d+0) 
term(704) = term(704) * (8.0d+0) 
term(705) = term(705) * (4.0d+0) 
term(706) = term(706) * (-8.0d+0) 
term(707) = term(707) * (-4.0d+0) 
term(708) = term(708) * (4.0d+0) 
term(709) = term(709) * (2.0d+0) 
term(710) = term(710) * (-4.0d+0) 
term(711) = term(711) * (2.0d+0) 
term(712) = term(712) * (2.0d+0) 
term(713) = term(713) * (-4.0d+0) 
term(714) = term(714) * (2.0d+0) 
term(715) = term(715) * (-4.0d+0) 
term(716) = term(716) * (8.0d+0) 
term(717) = term(717) * (-4.0d+0) 
term(718) = term(718) * (8.0d+0) 
term(719) = term(719) * (-8.0d+0) 
term(720) = term(720) * (8.0d+0) 
term(721) = term(721) * (-8.0d+0) 
term(722) = term(722) * (-16.0d+0) 
term(723) = term(723) * (16.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(724) = term(724) + s1(a,i) * wm_interm_26_so_pt4(a,j,q,k) * wm_interm_9_so_pt4(k,p,j,i)
term(725) = term(725) + s1(a,i) * wm_interm_26_so_pt4(a,j,q,k) * wm_interm_9_so_pt4(p,k,j,i)
term(726) = term(726) + s1(a,i) * wm_interm_24_so_pt4(a,j,q,k) * wm_interm_9_so_pt4(k,p,j,i)
term(727) = term(727) + s1(a,i) * wm_interm_24_so_pt4(a,j,q,k) * wm_interm_9_so_pt4(p,k,j,i)
term(728) = term(728) + s1(a,i) * wm_interm_29_so_pt4(a,j,q,k) * wm_interm_9_so_pt4(k,p,j,i)
term(729) = term(729) + s1(a,i) * wm_interm_29_so_pt4(a,j,q,k) * wm_interm_9_so_pt4(p,k,j,i)
term(730) = term(730) + s1(a,i) * wm_interm_24_so_pt4(a,p,j,k) * wm_interm_9_so_pt4(j,k,q,i)
term(731) = term(731) + s1(a,i) * wm_interm_26_so_pt4(a,p,j,k) * wm_interm_9_so_pt4(k,j,q,i)
term(732) = term(732) + s1(a,i) * wm_interm_26_so_pt4(a,p,j,k) * wm_interm_9_so_pt4(j,k,q,i)
term(733) = term(733) + s1(a,i) * wm_interm_29_so_pt4(a,p,j,k) * wm_interm_9_so_pt4(k,j,q,i)
term(734) = term(734) + s1(a,i) * wm_interm_29_so_pt4(a,p,j,k) * wm_interm_9_so_pt4(j,k,q,i)
end do 
end do 
end do 
end do 

term(724) = term(724) * (2.0d+0) 
term(725) = term(725) * (-4.0d+0) 
term(726) = term(726) * (-4.0d+0) 
term(727) = term(727) * (2.0d+0) 
term(728) = term(728) * (8.0d+0) 
term(729) = term(729) * (-8.0d+0) 
term(732) = term(732) * (-2.0d+0) 
term(733) = term(733) * (4.0d+0) 
term(734) = term(734) * (-4.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
term(735) = term(735) + wm_interm_25_so_pt4(i,j) * wm_interm_71_so_pt4(p,j,i,q)
term(736) = term(736) + wm_interm_25_so_pt4(i,j) * wm_interm_71_so_pt4(p,j,q,i)
term(737) = term(737) + wm_interm_25_so_pt4(i,j) * wm_interm_72_so_pt4(p,j,q,i)
term(738) = term(738) + wm_interm_25_so_pt4(i,j) * wm_interm_71_so_pt4(j,p,q,i)
term(739) = term(739) + wm_interm_25_so_pt4(i,j) * wm_interm_72_so_pt4(j,p,i,q)
term(740) = term(740) + wm_interm_25_so_pt4(i,j) * wm_interm_72_so_pt4(j,p,q,i)
term(741) = term(741) + wm_interm_25_so_pt4(i,j) * wm_interm_99_so_pt4(p,j,i,q)
term(742) = term(742) + wm_interm_25_so_pt4(i,j) * wm_interm_99_so_pt4(p,j,q,i)
term(743) = term(743) + wm_interm_25_so_pt4(i,j) * wm_interm_99_so_pt4(j,p,q,i)
term(744) = term(744) + wm_interm_25_so_pt4(i,j) * wm_interm_99_so_pt4(j,p,i,q)
end do 
end do 

term(735) = term(735) * (-0.5d+0) 
term(737) = term(737) * (-0.5d+0) 
term(738) = term(738) * (-0.5d+0) 
term(739) = term(739) * (-0.5d+0) 
term(741) = term(741) * (-2.0d+0) 
term(742) = term(742) * (2.0d+0) 
term(743) = term(743) * (-2.0d+0) 
term(744) = term(744) * (2.0d+0) 

do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(745) = term(745) + wm_interm_44_so_pt4(a,b) * wm_interm_64_so_pt4(b,a,p,q)
term(746) = term(746) + wm_interm_44_so_pt4(a,b) * wm_interm_63_so_pt4(b,a,p,q)
term(747) = term(747) + wm_interm_44_so_pt4(a,b) * wm_interm_65_so_pt4(b,a,p,q)
term(748) = term(748) + wm_interm_44_so_pt4(a,b) * wm_interm_75_so_pt4(b,a,p,q)
term(749) = term(749) + wm_interm_44_so_pt4(a,b) * wm_interm_77_so_pt4(b,a,p,q)
term(750) = term(750) + wm_interm_44_so_pt4(a,b) * wm_interm_79_so_pt4(b,a,p,q)
term(751) = term(751) + wm_interm_44_so_pt4(a,b) * wm_interm_94_so_pt4(b,a,p,q)
term(752) = term(752) + wm_interm_44_so_pt4(a,b) * wm_interm_93_so_pt4(b,a,p,q)
term(753) = term(753) + wm_interm_100_so_pt4(a,b,p,q) * wm_interm_44_so_pt4(b,a)
term(754) = term(754) + wm_interm_102_so_pt4(a,b,p,q) * wm_interm_44_so_pt4(b,a)
term(755) = term(755) + wm_interm_114_so_pt4(a,b,p,q) * wm_interm_91_so_pt4(b,a)
term(756) = term(756) + wm_interm_114_so_pt4(a,b,p,q) * wm_interm_92_so_pt4(b,a)
term(757) = term(757) + wm_interm_114_so_pt4(a,b,p,q) * wm_interm_90_so_pt4(b,a)
term(758) = term(758) + wm_interm_110_so_pt4(a,b) * wm_interm_114_so_pt4(b,a,p,q)
term(759) = term(759) + wm_interm_111_so_pt4(a,b) * wm_interm_114_so_pt4(b,a,p,q)
term(760) = term(760) + r1(vrdav_Rl, a,p) * wm_interm_6_so_pt4(b,q) * wm_interm_83_so_pt4(a,b)
term(761) = term(761) + r1(vrdav_Rl, a,p) * wm_interm_6_so_pt4(b,q) * wm_interm_81_so_pt4(a,b)
term(762) = term(762) + r1(vrdav_Rl, a,p) * wm_interm_6_so_pt4(b,q) * wm_interm_82_so_pt4(a,b)
term(763) = term(763) + r1(vrdav_Rl, a,p) * wm_interm_7_so_pt4(b,q) * wm_interm_83_so_pt4(a,b)
term(764) = term(764) + r1(vrdav_Rl, a,p) * wm_interm_7_so_pt4(b,q) * wm_interm_81_so_pt4(a,b)
term(765) = term(765) + r1(vrdav_Rl, a,p) * wm_interm_7_so_pt4(b,q) * wm_interm_82_so_pt4(a,b)
term(766) = term(766) + s1(a,p) * wm_interm_16_so_pt4(b,q) * wm_interm_81_so_pt4(a,b)
term(767) = term(767) + s1(a,p) * wm_interm_16_so_pt4(b,q) * wm_interm_82_so_pt4(a,b)
term(768) = term(768) + s1(a,p) * wm_interm_16_so_pt4(b,q) * wm_interm_83_so_pt4(a,b)
term(769) = term(769) + s1(a,p) * wm_interm_14_so_pt4(b,q) * wm_interm_81_so_pt4(a,b)
term(770) = term(770) + s1(a,p) * wm_interm_14_so_pt4(b,q) * wm_interm_82_so_pt4(a,b)
term(771) = term(771) + s1(a,p) * wm_interm_14_so_pt4(b,q) * wm_interm_83_so_pt4(a,b)
term(772) = term(772) + r1(vrdav_Rl, a,p) * wm_interm_104_so_pt4(a,b) * wm_interm_6_so_pt4(b,q)
term(773) = term(773) + r1(vrdav_Rl, a,p) * wm_interm_105_so_pt4(a,b) * wm_interm_6_so_pt4(b,q)
term(774) = term(774) + r1(vrdav_Rl, a,p) * wm_interm_104_so_pt4(a,b) * wm_interm_7_so_pt4(b,q)
term(775) = term(775) + r1(vrdav_Rl, a,p) * wm_interm_105_so_pt4(a,b) * wm_interm_7_so_pt4(b,q)
term(776) = term(776) + s1(a,p) * wm_interm_104_so_pt4(a,b) * wm_interm_16_so_pt4(b,q)
term(777) = term(777) + s1(a,p) * wm_interm_105_so_pt4(a,b) * wm_interm_16_so_pt4(b,q)
term(778) = term(778) + s1(a,p) * wm_interm_112_so_pt4(a,b) * wm_interm_16_so_pt4(b,q)
term(779) = term(779) + s1(a,p) * wm_interm_104_so_pt4(a,b) * wm_interm_14_so_pt4(b,q)
term(780) = term(780) + s1(a,p) * wm_interm_105_so_pt4(a,b) * wm_interm_14_so_pt4(b,q)
term(781) = term(781) + s1(a,p) * wm_interm_112_so_pt4(a,b) * wm_interm_14_so_pt4(b,q)
end do 
end do 

term(745) = term(745) * (-1.0d+0) 
term(746) = term(746) * (2.0d+0) 
term(747) = term(747) * (-1.0d+0) 
term(748) = term(748) * (-1.0d+0) 
term(749) = term(749) * (-1.0d+0) 
term(750) = term(750) * (2.0d+0) 
term(751) = term(751) * (-4.0d+0) 
term(752) = term(752) * (4.0d+0) 
term(753) = term(753) * (-4.0d+0) 
term(754) = term(754) * (4.0d+0) 
term(755) = term(755) * (-1.0d+0) 
term(756) = term(756) * (2.0d+0) 
term(757) = term(757) * (-1.0d+0) 
term(758) = term(758) * (-4.0d+0) 
term(759) = term(759) * (4.0d+0) 
term(760) = term(760) * (-4.0d+0) 
term(761) = term(761) * (-4.0d+0) 
term(762) = term(762) * (8.0d+0) 
term(763) = term(763) * (2.0d+0) 
term(764) = term(764) * (2.0d+0) 
term(765) = term(765) * (-4.0d+0) 
term(766) = term(766) * (2.0d+0) 
term(767) = term(767) * (-4.0d+0) 
term(768) = term(768) * (2.0d+0) 
term(769) = term(769) * (-4.0d+0) 
term(770) = term(770) * (8.0d+0) 
term(771) = term(771) * (-4.0d+0) 
term(772) = term(772) * (-16.0d+0) 
term(773) = term(773) * (16.0d+0) 
term(774) = term(774) * (8.0d+0) 
term(775) = term(775) * (-8.0d+0) 
term(776) = term(776) * (4.0d+0) 
term(777) = term(777) * (-8.0d+0) 
term(778) = term(778) * (4.0d+0) 
term(779) = term(779) * (-8.0d+0) 
term(780) = term(780) * (16.0d+0) 
term(781) = term(781) * (-8.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(782) = term(782) + s1(a,i) * wm_interm_26_so_pt4(a,j,k,q) * wm_interm_9_so_pt4(p,k,j,i)
term(783) = term(783) + s1(a,i) * wm_interm_24_so_pt4(a,j,k,q) * wm_interm_9_so_pt4(k,p,j,i)
term(784) = term(784) + s1(a,i) * wm_interm_29_so_pt4(a,j,k,q) * wm_interm_9_so_pt4(p,k,j,i)
term(785) = term(785) + s1(a,i) * wm_interm_29_so_pt4(a,j,k,q) * wm_interm_9_so_pt4(k,p,j,i)
end do 
end do 
end do 
end do 

term(782) = term(782) * (2.0d+0) 
term(783) = term(783) * (2.0d+0) 
term(784) = term(784) * (8.0d+0) 
term(785) = term(785) * (-8.0d+0) 

do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(786) = term(786) + t1(a,i) * wm_interm_10_so_pt4(a,b,p,j) * wm_interm_46_so_pt4(b,i,j,q)
term(787) = term(787) + t1(a,i) * wm_interm_11_so_pt4(a,b,p,j) * wm_interm_46_so_pt4(b,i,j,q)
term(788) = term(788) + t1(a,i) * wm_interm_10_so_pt4(a,b,p,j) * wm_interm_48_so_pt4(b,i,j,q)
term(789) = term(789) + t1(a,i) * wm_interm_11_so_pt4(a,b,p,j) * wm_interm_48_so_pt4(b,i,j,q)
term(790) = term(790) + t1(a,i) * wm_interm_1_so_pt4(a,b,p,j) * wm_interm_48_so_pt4(b,i,j,q)
term(791) = term(791) + t1(a,i) * wm_interm_48_so_pt4(b,i,j,q) * wm_interm_5_so_pt4(a,b,p,j)
term(792) = term(792) + t1(a,i) * wm_interm_46_so_pt4(b,i,j,q) * wm_interm_5_so_pt4(a,b,p,j)
term(793) = term(793) + t1(a,i) * wm_interm_1_so_pt4(a,b,p,j) * wm_interm_46_so_pt4(b,i,j,q)
term(794) = term(794) + r1(vrdav_Rr, a,i) * wm_interm_10_so_pt4(a,b,p,j) * wm_interm_49_so_pt4(b,i,j,q)
term(795) = term(795) + r1(vrdav_Rr, a,i) * wm_interm_11_so_pt4(a,b,p,j) * wm_interm_49_so_pt4(b,i,j,q)
term(796) = term(796) + r1(vrdav_Rr, a,i) * wm_interm_10_so_pt4(a,b,p,j) * wm_interm_47_so_pt4(b,i,j,q)
term(797) = term(797) + r1(vrdav_Rr, a,i) * wm_interm_11_so_pt4(a,b,p,j) * wm_interm_47_so_pt4(b,i,j,q)
term(798) = term(798) + r1(vrdav_Rr, a,i) * wm_interm_1_so_pt4(a,b,p,j) * wm_interm_47_so_pt4(b,i,j,q)
term(799) = term(799) + r1(vrdav_Rr, a,i) * wm_interm_49_so_pt4(b,i,j,q) * wm_interm_5_so_pt4(a,b,p,j)
term(800) = term(800) + r1(vrdav_Rr, a,i) * wm_interm_1_so_pt4(a,b,p,j) * wm_interm_49_so_pt4(b,i,j,q)
term(801) = term(801) + r1(vrdav_Rr, a,i) * wm_interm_22_so_pt4(b,i,p,j) * wm_interm_59_so_pt4(a,b,j,q)
term(802) = term(802) + r1(vrdav_Rr, a,i) * wm_interm_1_so_pt4(a,b,j,q) * wm_interm_47_so_pt4(b,i,p,j)
term(803) = term(803) + r1(vrdav_Rr, a,i) * wm_interm_10_so_pt4(a,b,j,q) * wm_interm_47_so_pt4(b,i,p,j)
term(804) = term(804) + r1(vrdav_Rr, a,i) * wm_interm_11_so_pt4(a,b,j,q) * wm_interm_47_so_pt4(b,i,p,j)
term(805) = term(805) + t1(a,i) * wm_interm_1_so_pt4(a,b,j,q) * wm_interm_48_so_pt4(b,i,p,j)
term(806) = term(806) + t1(a,i) * wm_interm_48_so_pt4(b,i,p,j) * wm_interm_5_so_pt4(a,b,j,q)
term(807) = term(807) + t1(a,i) * wm_interm_10_so_pt4(a,b,j,q) * wm_interm_48_so_pt4(b,i,p,j)
term(808) = term(808) + t1(a,i) * wm_interm_46_so_pt4(b,i,p,j) * wm_interm_5_so_pt4(a,b,j,q)
term(809) = term(809) + r1(vrdav_Rr, a,i) * wm_interm_113_so_pt4(b,i,p,j) * wm_interm_56_so_pt4(a,b,j,q)
term(810) = term(810) + t1(a,i) * wm_interm_11_so_pt4(a,b,j,q) * wm_interm_48_so_pt4(b,i,p,j)
term(811) = term(811) + r1(vrdav_Rr, a,i) * wm_interm_49_so_pt4(b,i,p,j) * wm_interm_5_so_pt4(a,b,j,q)
end do 
end do 
end do 
end do 

term(786) = term(786) * (4.0d+0) 
term(787) = term(787) * (-8.0d+0) 
term(788) = term(788) * (-2.0d+0) 
term(789) = term(789) * (4.0d+0) 
term(790) = term(790) * (-2.0d+0) 
term(791) = term(791) * (4.0d+0) 
term(792) = term(792) * (-2.0d+0) 
term(793) = term(793) * (4.0d+0) 
term(794) = term(794) * (2.0d+0) 
term(795) = term(795) * (-4.0d+0) 
term(796) = term(796) * (-1.0d+0) 
term(797) = term(797) * (2.0d+0) 
term(798) = term(798) * (-1.0d+0) 
term(799) = term(799) * (-1.0d+0) 
term(800) = term(800) * (2.0d+0) 
term(801) = term(801) * (-1.0d+0) 
term(802) = term(802) * (-1.0d+0) 
term(803) = term(803) * (-1.0d+0) 
term(804) = term(804) * (2.0d+0) 
term(805) = term(805) * (-1.0d+0) 
term(806) = term(806) * (2.0d+0) 
term(807) = term(807) * (-1.0d+0) 
term(808) = term(808) * (-1.0d+0) 
term(809) = term(809) * (-1.0d+0) 
term(810) = term(810) * (2.0d+0) 
term(811) = term(811) * (-1.0d+0) 


    calc_D_oo_wm_so_pt4 = zero
    do s = 0, 811
    calc_D_oo_wm_so_pt4 = calc_D_oo_wm_so_pt4 + term(s)
    end do

    end function calc_D_oo_wm_so_pt4
    
    function calc_D_ov_wm_so_pt4(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_ov_wm_so_pt4
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , k, i, j, b, a, l 
    real(F64), dimension(0:1592) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(0) = term(0) + r2(vrdav_Rl, a,p,q,i) * wm_interm_3_so_pt4(b,j,k,i) * wm_interm_67_so_pt4(a,b,j,k)
term(1) = term(1) + r2(vrdav_Rl, a,p,q,i) * wm_interm_3_so_pt4(b,j,k,i) * wm_interm_68_so_pt4(a,b,j,k)
term(2) = term(2) + r2(vrdav_Rl, a,p,q,i) * wm_interm_3_so_pt4(b,j,k,i) * wm_interm_69_so_pt4(a,b,j,k)
term(3) = term(3) + r2(vrdav_Rl, a,p,q,i) * wm_interm_3_so_pt4(b,j,k,i) * wm_interm_70_so_pt4(a,b,j,k)
term(4) = term(4) + r2(vrdav_Rl, a,p,q,i) * wm_interm_3_so_pt4(b,j,k,i) * wm_interm_73_so_pt4(a,b,j,k)
term(5) = term(5) + r2(vrdav_Rl, a,p,q,i) * wm_interm_3_so_pt4(b,j,k,i) * wm_interm_74_so_pt4(a,b,j,k)
term(6) = term(6) + r2(vrdav_Rl, a,p,q,i) * wm_interm_3_so_pt4(b,j,i,k) * wm_interm_70_so_pt4(a,b,j,k)
term(7) = term(7) + r2(vrdav_Rl, a,p,q,i) * wm_interm_3_so_pt4(b,j,i,k) * wm_interm_69_so_pt4(a,b,j,k)
term(8) = term(8) + r2(vrdav_Rl, a,p,q,i) * wm_interm_3_so_pt4(b,j,i,k) * wm_interm_67_so_pt4(a,b,j,k)
term(9) = term(9) + r2(vrdav_Rl, a,p,q,i) * wm_interm_3_so_pt4(b,j,i,k) * wm_interm_68_so_pt4(a,b,j,k)
term(10) = term(10) + r2(vrdav_Rl, a,p,q,i) * wm_interm_3_so_pt4(b,j,i,k) * wm_interm_84_so_pt4(a,b,j,k)
term(11) = term(11) + r2(vrdav_Rl, a,p,q,i) * wm_interm_3_so_pt4(b,j,i,k) * wm_interm_74_so_pt4(a,b,j,k)
term(12) = term(12) + r2(vrdav_Rl, a,p,q,i) * wm_interm_3_so_pt4(b,j,k,i) * wm_interm_95_so_pt4(a,b,j,k)
term(13) = term(13) + r2(vrdav_Rl, a,p,q,i) * wm_interm_3_so_pt4(b,j,k,i) * wm_interm_96_so_pt4(a,b,j,k)
term(14) = term(14) + r2(vrdav_Rl, a,p,q,i) * wm_interm_3_so_pt4(b,j,k,i) * wm_interm_97_so_pt4(a,b,j,k)
term(15) = term(15) + r2(vrdav_Rl, a,p,q,i) * wm_interm_3_so_pt4(b,j,k,i) * wm_interm_98_so_pt4(a,b,j,k)
term(16) = term(16) + r2(vrdav_Rl, a,p,q,i) * wm_interm_3_so_pt4(b,j,i,k) * wm_interm_98_so_pt4(a,b,j,k)
term(17) = term(17) + r2(vrdav_Rl, a,p,q,i) * wm_interm_3_so_pt4(b,j,i,k) * wm_interm_97_so_pt4(a,b,j,k)
term(18) = term(18) + r2(vrdav_Rl, a,p,q,i) * wm_interm_3_so_pt4(b,j,i,k) * wm_interm_95_so_pt4(a,b,j,k)
term(19) = term(19) + r2(vrdav_Rl, a,p,q,i) * wm_interm_3_so_pt4(b,j,i,k) * wm_interm_96_so_pt4(a,b,j,k)
end do 
end do 
end do 
end do 
end do 

term(0) = term(0) * (2.0d+0) 
term(1) = term(1) * (-4.0d+0) 
term(2) = term(2) * (2.0d+0) 
term(3) = term(3) * (-4.0d+0) 
term(4) = term(4) * (2.0d+0) 
term(5) = term(5) * (2.0d+0) 
term(6) = term(6) * (2.0d+0) 
term(7) = term(7) * (-4.0d+0) 
term(8) = term(8) * (-4.0d+0) 
term(9) = term(9) * (8.0d+0) 
term(10) = term(10) * (2.0d+0) 
term(11) = term(11) * (-4.0d+0) 
term(12) = term(12) * (8.0d+0) 
term(13) = term(13) * (-8.0d+0) 
term(14) = term(14) * (8.0d+0) 
term(15) = term(15) * (-8.0d+0) 
term(16) = term(16) * (8.0d+0) 
term(17) = term(17) * (-8.0d+0) 
term(18) = term(18) * (-16.0d+0) 
term(19) = term(19) * (16.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(20) = term(20) + s2(a,b,i,j) * wm_interm_33_so_pt4(a,j) * wm_interm_56_so_pt4(q,b,p,i)
term(21) = term(21) + s2(a,b,i,j) * wm_interm_34_so_pt4(a,j) * wm_interm_56_so_pt4(q,b,p,i)
term(22) = term(22) + s2(a,b,i,j) * wm_interm_35_so_pt4(a,j) * wm_interm_56_so_pt4(q,b,p,i)
term(23) = term(23) + s2(a,b,i,j) * wm_interm_36_so_pt4(a,j) * wm_interm_56_so_pt4(q,b,p,i)
term(24) = term(24) + s2(a,b,i,j) * wm_interm_37_so_pt4(a,j) * wm_interm_56_so_pt4(q,b,p,i)
term(25) = term(25) + s1(a,p) * wm_interm_1_so_pt4(b,a,i,j) * wm_interm_63_so_pt4(q,b,j,i)
term(26) = term(26) + s1(a,p) * wm_interm_1_so_pt4(b,a,i,j) * wm_interm_64_so_pt4(q,b,j,i)
term(27) = term(27) + s1(a,p) * wm_interm_5_so_pt4(b,a,i,j) * wm_interm_64_so_pt4(q,b,j,i)
term(28) = term(28) + s1(a,i) * wm_interm_1_so_pt4(b,a,p,j) * wm_interm_64_so_pt4(q,b,j,i)
term(29) = term(29) + s1(a,i) * wm_interm_5_so_pt4(b,a,p,j) * wm_interm_63_so_pt4(q,b,j,i)
term(30) = term(30) + s1(a,i) * wm_interm_5_so_pt4(b,a,p,j) * wm_interm_64_so_pt4(q,b,j,i)
term(31) = term(31) + s1(a,p) * wm_interm_5_so_pt4(b,a,i,j) * wm_interm_65_so_pt4(q,b,j,i)
term(32) = term(32) + s1(a,p) * wm_interm_5_so_pt4(b,a,i,j) * wm_interm_66_so_pt4(q,b,j,i)
term(33) = term(33) + s1(a,p) * wm_interm_1_so_pt4(b,a,i,j) * wm_interm_66_so_pt4(q,b,j,i)
term(34) = term(34) + s1(a,i) * wm_interm_5_so_pt4(b,a,p,j) * wm_interm_66_so_pt4(q,b,j,i)
term(35) = term(35) + s1(a,i) * wm_interm_1_so_pt4(b,a,p,j) * wm_interm_65_so_pt4(q,b,j,i)
term(36) = term(36) + s1(a,i) * wm_interm_1_so_pt4(b,a,p,j) * wm_interm_66_so_pt4(q,b,j,i)
term(37) = term(37) + s1(a,i) * wm_interm_1_so_pt4(b,a,p,j) * wm_interm_75_so_pt4(q,b,j,i)
term(38) = term(38) + s1(a,p) * wm_interm_5_so_pt4(b,a,i,j) * wm_interm_75_so_pt4(q,b,j,i)
term(39) = term(39) + s1(a,i) * wm_interm_1_so_pt4(b,a,p,j) * wm_interm_77_so_pt4(q,b,j,i)
term(40) = term(40) + s1(a,p) * wm_interm_5_so_pt4(b,a,i,j) * wm_interm_77_so_pt4(q,b,j,i)
term(41) = term(41) + s1(a,i) * wm_interm_1_so_pt4(b,a,p,j) * wm_interm_79_so_pt4(q,b,j,i)
term(42) = term(42) + s1(a,p) * wm_interm_5_so_pt4(b,a,i,j) * wm_interm_79_so_pt4(q,b,j,i)
term(43) = term(43) + s1(a,p) * wm_interm_10_so_pt4(b,a,i,j) * wm_interm_63_so_pt4(q,b,j,i)
term(44) = term(44) + s1(a,p) * wm_interm_10_so_pt4(b,a,i,j) * wm_interm_64_so_pt4(q,b,j,i)
term(45) = term(45) + s1(a,p) * wm_interm_11_so_pt4(b,a,i,j) * wm_interm_63_so_pt4(q,b,j,i)
term(46) = term(46) + s1(a,p) * wm_interm_11_so_pt4(b,a,i,j) * wm_interm_64_so_pt4(q,b,j,i)
term(47) = term(47) + s1(a,p) * wm_interm_10_so_pt4(b,a,i,j) * wm_interm_66_so_pt4(q,b,j,i)
term(48) = term(48) + s1(a,p) * wm_interm_11_so_pt4(b,a,i,j) * wm_interm_66_so_pt4(q,b,j,i)
term(49) = term(49) + s1(a,i) * wm_interm_10_so_pt4(b,a,p,j) * wm_interm_64_so_pt4(q,b,j,i)
term(50) = term(50) + s1(a,i) * wm_interm_11_so_pt4(b,a,p,j) * wm_interm_64_so_pt4(q,b,j,i)
term(51) = term(51) + s1(a,i) * wm_interm_10_so_pt4(b,a,p,j) * wm_interm_65_so_pt4(q,b,j,i)
term(52) = term(52) + s1(a,i) * wm_interm_11_so_pt4(b,a,p,j) * wm_interm_65_so_pt4(q,b,j,i)
term(53) = term(53) + s1(a,i) * wm_interm_10_so_pt4(b,a,p,j) * wm_interm_66_so_pt4(q,b,j,i)
term(54) = term(54) + s1(a,i) * wm_interm_11_so_pt4(b,a,p,j) * wm_interm_66_so_pt4(q,b,j,i)
term(55) = term(55) + s1(a,i) * wm_interm_10_so_pt4(b,a,p,j) * wm_interm_75_so_pt4(q,b,j,i)
term(56) = term(56) + s1(a,i) * wm_interm_11_so_pt4(b,a,p,j) * wm_interm_75_so_pt4(q,b,j,i)
term(57) = term(57) + s1(a,i) * wm_interm_10_so_pt4(b,a,p,j) * wm_interm_77_so_pt4(q,b,j,i)
term(58) = term(58) + s1(a,i) * wm_interm_11_so_pt4(b,a,p,j) * wm_interm_77_so_pt4(q,b,j,i)
term(59) = term(59) + s1(a,i) * wm_interm_10_so_pt4(b,a,p,j) * wm_interm_79_so_pt4(q,b,j,i)
term(60) = term(60) + s1(a,i) * wm_interm_11_so_pt4(b,a,p,j) * wm_interm_79_so_pt4(q,b,j,i)
term(61) = term(61) + s1(q,i) * wm_interm_1_so_pt4(a,b,j,i) * wm_interm_63_so_pt4(b,a,p,j)
term(62) = term(62) + s1(q,i) * wm_interm_1_so_pt4(a,b,j,i) * wm_interm_64_so_pt4(b,a,p,j)
term(63) = term(63) + s1(a,i) * wm_interm_1_so_pt4(q,b,j,i) * wm_interm_64_so_pt4(b,a,p,j)
term(64) = term(64) + s1(a,i) * wm_interm_1_so_pt4(q,b,j,i) * wm_interm_63_so_pt4(b,a,p,j)
term(65) = term(65) + s1(q,i) * wm_interm_5_so_pt4(a,b,j,i) * wm_interm_64_so_pt4(b,a,p,j)
term(66) = term(66) + s1(q,i) * wm_interm_5_so_pt4(a,b,j,i) * wm_interm_63_so_pt4(b,a,p,j)
term(67) = term(67) + s1(a,i) * wm_interm_5_so_pt4(q,b,j,i) * wm_interm_63_so_pt4(b,a,p,j)
term(68) = term(68) + s1(a,i) * wm_interm_5_so_pt4(q,b,j,i) * wm_interm_64_so_pt4(b,a,p,j)
term(69) = term(69) + s1(a,i) * wm_interm_1_so_pt4(q,b,j,i) * wm_interm_65_so_pt4(b,a,p,j)
term(70) = term(70) + s1(q,i) * wm_interm_1_so_pt4(a,b,j,i) * wm_interm_66_so_pt4(b,a,p,j)
term(71) = term(71) + s1(a,i) * wm_interm_5_so_pt4(q,b,j,i) * wm_interm_66_so_pt4(b,a,p,j)
term(72) = term(72) + s1(q,i) * wm_interm_5_so_pt4(a,b,j,i) * wm_interm_65_so_pt4(b,a,p,j)
term(73) = term(73) + s1(a,i) * wm_interm_1_so_pt4(q,b,j,i) * wm_interm_75_so_pt4(b,a,p,j)
term(74) = term(74) + s1(q,i) * wm_interm_5_so_pt4(a,b,j,i) * wm_interm_75_so_pt4(b,a,p,j)
term(75) = term(75) + s1(a,i) * wm_interm_1_so_pt4(q,b,j,i) * wm_interm_77_so_pt4(b,a,p,j)
term(76) = term(76) + s1(q,i) * wm_interm_5_so_pt4(a,b,j,i) * wm_interm_77_so_pt4(b,a,p,j)
term(77) = term(77) + s1(a,i) * wm_interm_1_so_pt4(q,b,j,i) * wm_interm_79_so_pt4(b,a,p,j)
term(78) = term(78) + s1(q,i) * wm_interm_5_so_pt4(a,b,j,i) * wm_interm_79_so_pt4(b,a,p,j)
term(79) = term(79) + s1(q,i) * wm_interm_10_so_pt4(a,b,j,i) * wm_interm_63_so_pt4(b,a,p,j)
term(80) = term(80) + s1(q,i) * wm_interm_10_so_pt4(a,b,j,i) * wm_interm_64_so_pt4(b,a,p,j)
term(81) = term(81) + s1(q,i) * wm_interm_11_so_pt4(a,b,j,i) * wm_interm_63_so_pt4(b,a,p,j)
term(82) = term(82) + s1(q,i) * wm_interm_11_so_pt4(a,b,j,i) * wm_interm_64_so_pt4(b,a,p,j)
term(83) = term(83) + s1(a,i) * wm_interm_10_so_pt4(q,b,j,i) * wm_interm_64_so_pt4(b,a,p,j)
term(84) = term(84) + s1(a,i) * wm_interm_11_so_pt4(q,b,j,i) * wm_interm_64_so_pt4(b,a,p,j)
term(85) = term(85) + s1(a,i) * wm_interm_10_so_pt4(q,b,j,i) * wm_interm_63_so_pt4(b,a,p,j)
term(86) = term(86) + s1(a,i) * wm_interm_11_so_pt4(q,b,j,i) * wm_interm_63_so_pt4(b,a,p,j)
term(87) = term(87) + s1(a,i) * wm_interm_10_so_pt4(q,b,j,i) * wm_interm_65_so_pt4(b,a,p,j)
term(88) = term(88) + s1(a,i) * wm_interm_11_so_pt4(q,b,j,i) * wm_interm_65_so_pt4(b,a,p,j)
term(89) = term(89) + s1(q,i) * wm_interm_10_so_pt4(a,b,j,i) * wm_interm_66_so_pt4(b,a,p,j)
term(90) = term(90) + s1(q,i) * wm_interm_11_so_pt4(a,b,j,i) * wm_interm_66_so_pt4(b,a,p,j)
term(91) = term(91) + s1(a,i) * wm_interm_10_so_pt4(q,b,j,i) * wm_interm_75_so_pt4(b,a,p,j)
term(92) = term(92) + s1(a,i) * wm_interm_11_so_pt4(q,b,j,i) * wm_interm_75_so_pt4(b,a,p,j)
term(93) = term(93) + s1(a,i) * wm_interm_10_so_pt4(q,b,j,i) * wm_interm_77_so_pt4(b,a,p,j)
term(94) = term(94) + s1(a,i) * wm_interm_11_so_pt4(q,b,j,i) * wm_interm_77_so_pt4(b,a,p,j)
term(95) = term(95) + s1(a,i) * wm_interm_10_so_pt4(q,b,j,i) * wm_interm_79_so_pt4(b,a,p,j)
term(96) = term(96) + s1(a,i) * wm_interm_11_so_pt4(q,b,j,i) * wm_interm_79_so_pt4(b,a,p,j)
term(97) = term(97) + s1(a,p) * wm_interm_1_so_pt4(b,a,i,j) * wm_interm_93_so_pt4(q,b,j,i)
term(98) = term(98) + s1(a,p) * wm_interm_1_so_pt4(b,a,i,j) * wm_interm_94_so_pt4(q,b,j,i)
term(99) = term(99) + s1(a,p) * wm_interm_5_so_pt4(b,a,i,j) * wm_interm_94_so_pt4(q,b,j,i)
term(100) = term(100) + s1(a,i) * wm_interm_1_so_pt4(b,a,p,j) * wm_interm_94_so_pt4(q,b,j,i)
term(101) = term(101) + s1(a,i) * wm_interm_5_so_pt4(b,a,p,j) * wm_interm_93_so_pt4(q,b,j,i)
term(102) = term(102) + s1(a,i) * wm_interm_5_so_pt4(b,a,p,j) * wm_interm_94_so_pt4(q,b,j,i)
term(103) = term(103) + s1(a,p) * wm_interm_5_so_pt4(b,a,i,j) * wm_interm_93_so_pt4(q,b,j,i)
term(104) = term(104) + s1(a,i) * wm_interm_1_so_pt4(b,a,p,j) * wm_interm_93_so_pt4(q,b,j,i)
term(105) = term(105) + s1(a,i) * wm_interm_100_so_pt4(q,b,j,i) * wm_interm_1_so_pt4(b,a,p,j)
term(106) = term(106) + s1(a,p) * wm_interm_100_so_pt4(q,b,i,j) * wm_interm_5_so_pt4(b,a,j,i)
term(107) = term(107) + s1(a,i) * wm_interm_102_so_pt4(q,b,j,i) * wm_interm_1_so_pt4(b,a,p,j)
term(108) = term(108) + s1(a,p) * wm_interm_102_so_pt4(q,b,i,j) * wm_interm_5_so_pt4(b,a,j,i)
term(109) = term(109) + s1(a,p) * wm_interm_10_so_pt4(b,a,i,j) * wm_interm_93_so_pt4(q,b,j,i)
term(110) = term(110) + s1(a,p) * wm_interm_10_so_pt4(b,a,i,j) * wm_interm_94_so_pt4(q,b,j,i)
term(111) = term(111) + s1(a,p) * wm_interm_11_so_pt4(b,a,i,j) * wm_interm_93_so_pt4(q,b,j,i)
term(112) = term(112) + s1(a,p) * wm_interm_11_so_pt4(b,a,i,j) * wm_interm_94_so_pt4(q,b,j,i)
term(113) = term(113) + s1(a,i) * wm_interm_10_so_pt4(b,a,p,j) * wm_interm_94_so_pt4(q,b,j,i)
term(114) = term(114) + s1(a,i) * wm_interm_11_so_pt4(b,a,p,j) * wm_interm_94_so_pt4(q,b,j,i)
term(115) = term(115) + s1(a,i) * wm_interm_10_so_pt4(b,a,p,j) * wm_interm_93_so_pt4(q,b,j,i)
term(116) = term(116) + s1(a,i) * wm_interm_11_so_pt4(b,a,p,j) * wm_interm_93_so_pt4(q,b,j,i)
term(117) = term(117) + s1(a,i) * wm_interm_100_so_pt4(q,b,j,i) * wm_interm_10_so_pt4(b,a,p,j)
term(118) = term(118) + s1(a,i) * wm_interm_100_so_pt4(q,b,j,i) * wm_interm_11_so_pt4(b,a,p,j)
term(119) = term(119) + s1(a,i) * wm_interm_102_so_pt4(q,b,j,i) * wm_interm_10_so_pt4(b,a,p,j)
term(120) = term(120) + s1(a,i) * wm_interm_102_so_pt4(q,b,j,i) * wm_interm_11_so_pt4(b,a,p,j)
term(121) = term(121) + s1(q,i) * wm_interm_1_so_pt4(a,b,j,i) * wm_interm_93_so_pt4(b,a,p,j)
term(122) = term(122) + s1(q,i) * wm_interm_1_so_pt4(a,b,j,i) * wm_interm_94_so_pt4(b,a,p,j)
term(123) = term(123) + s1(a,i) * wm_interm_1_so_pt4(q,b,j,i) * wm_interm_94_so_pt4(b,a,p,j)
term(124) = term(124) + s1(a,i) * wm_interm_1_so_pt4(q,b,j,i) * wm_interm_93_so_pt4(b,a,p,j)
term(125) = term(125) + s1(q,i) * wm_interm_5_so_pt4(a,b,j,i) * wm_interm_94_so_pt4(b,a,p,j)
term(126) = term(126) + s1(q,i) * wm_interm_5_so_pt4(a,b,j,i) * wm_interm_93_so_pt4(b,a,p,j)
term(127) = term(127) + s1(a,i) * wm_interm_5_so_pt4(q,b,j,i) * wm_interm_93_so_pt4(b,a,p,j)
term(128) = term(128) + s1(a,i) * wm_interm_5_so_pt4(q,b,j,i) * wm_interm_94_so_pt4(b,a,p,j)
term(129) = term(129) + s1(a,i) * wm_interm_100_so_pt4(b,a,p,j) * wm_interm_1_so_pt4(q,b,j,i)
term(130) = term(130) + s1(q,i) * wm_interm_100_so_pt4(a,b,p,j) * wm_interm_5_so_pt4(b,a,j,i)
term(131) = term(131) + s1(a,i) * wm_interm_102_so_pt4(b,a,p,j) * wm_interm_1_so_pt4(q,b,j,i)
term(132) = term(132) + s1(q,i) * wm_interm_102_so_pt4(a,b,p,j) * wm_interm_5_so_pt4(b,a,j,i)
term(133) = term(133) + s1(q,i) * wm_interm_10_so_pt4(a,b,j,i) * wm_interm_93_so_pt4(b,a,p,j)
term(134) = term(134) + s1(q,i) * wm_interm_10_so_pt4(a,b,j,i) * wm_interm_94_so_pt4(b,a,p,j)
term(135) = term(135) + s1(q,i) * wm_interm_11_so_pt4(a,b,j,i) * wm_interm_93_so_pt4(b,a,p,j)
term(136) = term(136) + s1(q,i) * wm_interm_11_so_pt4(a,b,j,i) * wm_interm_94_so_pt4(b,a,p,j)
term(137) = term(137) + s1(a,i) * wm_interm_10_so_pt4(q,b,j,i) * wm_interm_94_so_pt4(b,a,p,j)
term(138) = term(138) + s1(a,i) * wm_interm_11_so_pt4(q,b,j,i) * wm_interm_94_so_pt4(b,a,p,j)
term(139) = term(139) + s1(a,i) * wm_interm_10_so_pt4(q,b,j,i) * wm_interm_93_so_pt4(b,a,p,j)
term(140) = term(140) + s1(a,i) * wm_interm_11_so_pt4(q,b,j,i) * wm_interm_93_so_pt4(b,a,p,j)
term(141) = term(141) + s1(a,i) * wm_interm_100_so_pt4(b,a,p,j) * wm_interm_10_so_pt4(q,b,j,i)
term(142) = term(142) + s1(a,i) * wm_interm_100_so_pt4(b,a,p,j) * wm_interm_11_so_pt4(q,b,j,i)
term(143) = term(143) + s1(a,i) * wm_interm_102_so_pt4(b,a,p,j) * wm_interm_10_so_pt4(q,b,j,i)
term(144) = term(144) + s1(a,i) * wm_interm_102_so_pt4(b,a,p,j) * wm_interm_11_so_pt4(q,b,j,i)
term(145) = term(145) + s1(q,i) * wm_interm_54_so_pt4(a,b,p,j) * wm_interm_70_so_pt4(a,b,i,j)
term(146) = term(146) + s1(q,i) * wm_interm_54_so_pt4(a,b,p,j) * wm_interm_69_so_pt4(a,b,i,j)
term(147) = term(147) + s1(q,i) * wm_interm_54_so_pt4(a,b,p,j) * wm_interm_67_so_pt4(a,b,i,j)
term(148) = term(148) + s1(q,i) * wm_interm_54_so_pt4(a,b,p,j) * wm_interm_68_so_pt4(a,b,i,j)
term(149) = term(149) + s1(q,i) * wm_interm_54_so_pt4(a,b,p,j) * wm_interm_84_so_pt4(a,b,i,j)
term(150) = term(150) + s1(q,i) * wm_interm_54_so_pt4(a,b,p,j) * wm_interm_74_so_pt4(a,b,i,j)
term(151) = term(151) + s1(q,i) * wm_interm_10_so_pt4(a,b,p,j) * wm_interm_75_so_pt4(b,a,j,i)
term(152) = term(152) + s1(q,i) * wm_interm_11_so_pt4(a,b,p,j) * wm_interm_75_so_pt4(b,a,j,i)
term(153) = term(153) + s1(q,i) * wm_interm_10_so_pt4(a,b,p,j) * wm_interm_77_so_pt4(b,a,j,i)
term(154) = term(154) + s1(q,i) * wm_interm_11_so_pt4(a,b,p,j) * wm_interm_77_so_pt4(b,a,j,i)
term(155) = term(155) + s1(q,i) * wm_interm_10_so_pt4(a,b,p,j) * wm_interm_79_so_pt4(b,a,j,i)
term(156) = term(156) + s1(q,i) * wm_interm_11_so_pt4(a,b,p,j) * wm_interm_79_so_pt4(b,a,j,i)
term(157) = term(157) + s1(q,i) * wm_interm_55_so_pt4(a,b,p,j) * wm_interm_70_so_pt4(a,b,i,j)
term(158) = term(158) + s1(q,i) * wm_interm_55_so_pt4(a,b,p,j) * wm_interm_69_so_pt4(a,b,i,j)
term(159) = term(159) + s1(q,i) * wm_interm_55_so_pt4(a,b,p,j) * wm_interm_67_so_pt4(a,b,i,j)
term(160) = term(160) + s1(q,i) * wm_interm_55_so_pt4(a,b,p,j) * wm_interm_68_so_pt4(a,b,i,j)
term(161) = term(161) + s1(q,i) * wm_interm_10_so_pt4(a,b,p,j) * wm_interm_63_so_pt4(b,a,j,i)
term(162) = term(162) + s1(q,i) * wm_interm_11_so_pt4(a,b,p,j) * wm_interm_63_so_pt4(b,a,j,i)
term(163) = term(163) + s1(q,i) * wm_interm_55_so_pt4(a,b,p,j) * wm_interm_84_so_pt4(a,b,i,j)
term(164) = term(164) + s1(q,i) * wm_interm_55_so_pt4(a,b,p,j) * wm_interm_74_so_pt4(a,b,i,j)
term(165) = term(165) + s1(q,i) * wm_interm_10_so_pt4(a,b,p,j) * wm_interm_66_so_pt4(b,a,j,i)
term(166) = term(166) + s1(q,i) * wm_interm_11_so_pt4(a,b,p,j) * wm_interm_66_so_pt4(b,a,j,i)
term(167) = term(167) + s1(q,i) * wm_interm_10_so_pt4(a,b,p,j) * wm_interm_65_so_pt4(b,a,j,i)
term(168) = term(168) + s1(q,i) * wm_interm_11_so_pt4(a,b,p,j) * wm_interm_65_so_pt4(b,a,j,i)
term(169) = term(169) + s1(q,i) * wm_interm_56_so_pt4(a,b,p,j) * wm_interm_67_so_pt4(a,b,i,j)
term(170) = term(170) + s1(q,i) * wm_interm_56_so_pt4(a,b,p,j) * wm_interm_68_so_pt4(a,b,i,j)
term(171) = term(171) + s1(q,i) * wm_interm_56_so_pt4(a,b,p,j) * wm_interm_74_so_pt4(a,b,i,j)
term(172) = term(172) + s1(q,i) * wm_interm_1_so_pt4(a,b,p,j) * wm_interm_63_so_pt4(b,a,j,i)
term(173) = term(173) + s1(q,i) * wm_interm_1_so_pt4(a,b,p,j) * wm_interm_66_so_pt4(b,a,j,i)
term(174) = term(174) + s1(q,i) * wm_interm_1_so_pt4(a,b,p,j) * wm_interm_65_so_pt4(b,a,j,i)
term(175) = term(175) + s1(q,i) * wm_interm_5_so_pt4(a,b,p,j) * wm_interm_64_so_pt4(b,a,j,i)
term(176) = term(176) + s1(q,i) * wm_interm_5_so_pt4(a,b,p,j) * wm_interm_63_so_pt4(b,a,j,i)
term(177) = term(177) + s1(q,i) * wm_interm_5_so_pt4(a,b,p,j) * wm_interm_65_so_pt4(b,a,j,i)
term(178) = term(178) + s1(q,i) * wm_interm_56_so_pt4(a,b,p,j) * wm_interm_69_so_pt4(a,b,i,j)
term(179) = term(179) + s1(q,i) * wm_interm_56_so_pt4(a,b,p,j) * wm_interm_73_so_pt4(a,b,i,j)
term(180) = term(180) + s1(q,i) * wm_interm_56_so_pt4(a,b,p,j) * wm_interm_84_so_pt4(a,b,i,j)
term(181) = term(181) + s1(q,i) * wm_interm_59_so_pt4(a,b,p,j) * wm_interm_70_so_pt4(a,b,i,j)
term(182) = term(182) + s1(q,i) * wm_interm_59_so_pt4(a,b,p,j) * wm_interm_69_so_pt4(a,b,i,j)
term(183) = term(183) + s1(q,i) * wm_interm_59_so_pt4(a,b,p,j) * wm_interm_67_so_pt4(a,b,i,j)
term(184) = term(184) + s1(q,i) * wm_interm_59_so_pt4(a,b,p,j) * wm_interm_68_so_pt4(a,b,i,j)
term(185) = term(185) + s1(q,i) * wm_interm_59_so_pt4(a,b,p,j) * wm_interm_84_so_pt4(a,b,i,j)
term(186) = term(186) + s1(q,i) * wm_interm_59_so_pt4(a,b,p,j) * wm_interm_74_so_pt4(a,b,i,j)
term(187) = term(187) + s1(q,i) * wm_interm_5_so_pt4(a,b,p,j) * wm_interm_75_so_pt4(b,a,j,i)
term(188) = term(188) + s1(q,i) * wm_interm_1_so_pt4(a,b,p,j) * wm_interm_75_so_pt4(b,a,j,i)
term(189) = term(189) + s1(q,i) * wm_interm_5_so_pt4(a,b,p,j) * wm_interm_77_so_pt4(b,a,j,i)
term(190) = term(190) + s1(q,i) * wm_interm_1_so_pt4(a,b,p,j) * wm_interm_77_so_pt4(b,a,j,i)
term(191) = term(191) + s1(q,i) * wm_interm_5_so_pt4(a,b,p,j) * wm_interm_79_so_pt4(b,a,j,i)
term(192) = term(192) + s1(q,i) * wm_interm_1_so_pt4(a,b,p,j) * wm_interm_79_so_pt4(b,a,j,i)
term(193) = term(193) + s1(a,p) * wm_interm_54_so_pt4(q,b,i,j) * wm_interm_70_so_pt4(a,b,i,j)
term(194) = term(194) + s1(a,p) * wm_interm_54_so_pt4(q,b,i,j) * wm_interm_69_so_pt4(a,b,i,j)
term(195) = term(195) + s1(a,p) * wm_interm_54_so_pt4(q,b,i,j) * wm_interm_67_so_pt4(a,b,i,j)
term(196) = term(196) + s1(a,p) * wm_interm_54_so_pt4(q,b,i,j) * wm_interm_68_so_pt4(a,b,i,j)
term(197) = term(197) + s1(a,p) * wm_interm_10_so_pt4(q,b,i,j) * wm_interm_77_so_pt4(b,a,j,i)
term(198) = term(198) + s1(a,p) * wm_interm_11_so_pt4(q,b,i,j) * wm_interm_77_so_pt4(b,a,j,i)
term(199) = term(199) + s1(a,p) * wm_interm_10_so_pt4(q,b,i,j) * wm_interm_79_so_pt4(b,a,j,i)
term(200) = term(200) + s1(a,p) * wm_interm_11_so_pt4(q,b,i,j) * wm_interm_79_so_pt4(b,a,j,i)
term(201) = term(201) + s1(a,p) * wm_interm_10_so_pt4(q,b,i,j) * wm_interm_75_so_pt4(b,a,j,i)
term(202) = term(202) + s1(a,p) * wm_interm_11_so_pt4(q,b,i,j) * wm_interm_75_so_pt4(b,a,j,i)
term(203) = term(203) + s1(a,p) * wm_interm_54_so_pt4(q,b,i,j) * wm_interm_84_so_pt4(a,b,i,j)
term(204) = term(204) + s1(a,p) * wm_interm_54_so_pt4(q,b,i,j) * wm_interm_74_so_pt4(a,b,i,j)
term(205) = term(205) + s1(a,p) * wm_interm_55_so_pt4(q,b,i,j) * wm_interm_70_so_pt4(a,b,i,j)
term(206) = term(206) + s1(a,p) * wm_interm_55_so_pt4(q,b,i,j) * wm_interm_69_so_pt4(a,b,i,j)
term(207) = term(207) + s1(a,p) * wm_interm_55_so_pt4(q,b,i,j) * wm_interm_67_so_pt4(a,b,i,j)
term(208) = term(208) + s1(a,p) * wm_interm_55_so_pt4(q,b,i,j) * wm_interm_68_so_pt4(a,b,i,j)
term(209) = term(209) + s1(a,p) * wm_interm_10_so_pt4(q,b,i,j) * wm_interm_66_so_pt4(b,a,j,i)
term(210) = term(210) + s1(a,p) * wm_interm_11_so_pt4(q,b,i,j) * wm_interm_66_so_pt4(b,a,j,i)
term(211) = term(211) + s1(a,p) * wm_interm_10_so_pt4(q,b,i,j) * wm_interm_65_so_pt4(b,a,j,i)
term(212) = term(212) + s1(a,p) * wm_interm_11_so_pt4(q,b,i,j) * wm_interm_65_so_pt4(b,a,j,i)
term(213) = term(213) + s1(a,p) * wm_interm_10_so_pt4(q,b,i,j) * wm_interm_63_so_pt4(b,a,j,i)
term(214) = term(214) + s1(a,p) * wm_interm_11_so_pt4(q,b,i,j) * wm_interm_63_so_pt4(b,a,j,i)
term(215) = term(215) + s1(a,p) * wm_interm_55_so_pt4(q,b,i,j) * wm_interm_84_so_pt4(a,b,i,j)
term(216) = term(216) + s1(a,p) * wm_interm_55_so_pt4(q,b,i,j) * wm_interm_74_so_pt4(a,b,i,j)
term(217) = term(217) + s1(a,p) * wm_interm_56_so_pt4(q,b,i,j) * wm_interm_67_so_pt4(a,b,i,j)
term(218) = term(218) + s1(a,p) * wm_interm_56_so_pt4(q,b,i,j) * wm_interm_68_so_pt4(a,b,i,j)
term(219) = term(219) + s1(a,p) * wm_interm_1_so_pt4(q,b,i,j) * wm_interm_66_so_pt4(b,a,j,i)
term(220) = term(220) + s1(a,p) * wm_interm_1_so_pt4(q,b,i,j) * wm_interm_65_so_pt4(b,a,j,i)
term(221) = term(221) + s1(a,p) * wm_interm_56_so_pt4(q,b,i,j) * wm_interm_69_so_pt4(a,b,i,j)
term(222) = term(222) + s1(a,p) * wm_interm_56_so_pt4(q,b,i,j) * wm_interm_70_so_pt4(a,b,i,j)
term(223) = term(223) + s1(a,p) * wm_interm_5_so_pt4(q,b,i,j) * wm_interm_65_so_pt4(b,a,j,i)
term(224) = term(224) + s1(a,p) * wm_interm_5_so_pt4(q,b,i,j) * wm_interm_66_so_pt4(b,a,j,i)
term(225) = term(225) + s1(a,p) * wm_interm_59_so_pt4(q,b,i,j) * wm_interm_70_so_pt4(a,b,i,j)
term(226) = term(226) + s1(a,p) * wm_interm_59_so_pt4(q,b,i,j) * wm_interm_69_so_pt4(a,b,i,j)
term(227) = term(227) + s1(a,p) * wm_interm_59_so_pt4(q,b,i,j) * wm_interm_67_so_pt4(a,b,i,j)
term(228) = term(228) + s1(a,p) * wm_interm_59_so_pt4(q,b,i,j) * wm_interm_68_so_pt4(a,b,i,j)
term(229) = term(229) + s1(a,p) * wm_interm_5_so_pt4(q,b,i,j) * wm_interm_77_so_pt4(b,a,j,i)
term(230) = term(230) + s1(a,p) * wm_interm_1_so_pt4(q,b,i,j) * wm_interm_77_so_pt4(b,a,j,i)
term(231) = term(231) + s1(a,p) * wm_interm_5_so_pt4(q,b,i,j) * wm_interm_79_so_pt4(b,a,j,i)
term(232) = term(232) + s1(a,p) * wm_interm_1_so_pt4(q,b,i,j) * wm_interm_79_so_pt4(b,a,j,i)
term(233) = term(233) + s1(a,p) * wm_interm_1_so_pt4(q,b,i,j) * wm_interm_63_so_pt4(b,a,j,i)
term(234) = term(234) + s1(a,p) * wm_interm_56_so_pt4(q,b,i,j) * wm_interm_74_so_pt4(a,b,i,j)
term(235) = term(235) + s1(a,p) * wm_interm_5_so_pt4(q,b,i,j) * wm_interm_64_so_pt4(b,a,j,i)
term(236) = term(236) + s1(a,p) * wm_interm_56_so_pt4(q,b,i,j) * wm_interm_73_so_pt4(a,b,i,j)
term(237) = term(237) + s1(a,p) * wm_interm_5_so_pt4(q,b,i,j) * wm_interm_75_so_pt4(b,a,j,i)
term(238) = term(238) + s1(a,p) * wm_interm_1_so_pt4(q,b,i,j) * wm_interm_75_so_pt4(b,a,j,i)
term(239) = term(239) + s1(a,p) * wm_interm_59_so_pt4(q,b,i,j) * wm_interm_84_so_pt4(a,b,i,j)
term(240) = term(240) + s1(a,p) * wm_interm_59_so_pt4(q,b,i,j) * wm_interm_74_so_pt4(a,b,i,j)
term(241) = term(241) + s1(q,i) * wm_interm_54_so_pt4(a,b,p,j) * wm_interm_98_so_pt4(a,b,i,j)
term(242) = term(242) + s1(q,i) * wm_interm_54_so_pt4(a,b,p,j) * wm_interm_97_so_pt4(a,b,i,j)
term(243) = term(243) + s1(q,i) * wm_interm_54_so_pt4(a,b,p,j) * wm_interm_95_so_pt4(a,b,i,j)
term(244) = term(244) + s1(q,i) * wm_interm_54_so_pt4(a,b,p,j) * wm_interm_96_so_pt4(a,b,i,j)
term(245) = term(245) + s1(q,i) * wm_interm_100_so_pt4(a,b,j,i) * wm_interm_10_so_pt4(b,a,p,j)
term(246) = term(246) + s1(q,i) * wm_interm_100_so_pt4(a,b,j,i) * wm_interm_11_so_pt4(b,a,p,j)
term(247) = term(247) + s1(q,i) * wm_interm_102_so_pt4(a,b,j,i) * wm_interm_10_so_pt4(b,a,p,j)
term(248) = term(248) + s1(q,i) * wm_interm_102_so_pt4(a,b,j,i) * wm_interm_11_so_pt4(b,a,p,j)
term(249) = term(249) + s1(q,i) * wm_interm_55_so_pt4(a,b,p,j) * wm_interm_98_so_pt4(a,b,i,j)
term(250) = term(250) + s1(q,i) * wm_interm_55_so_pt4(a,b,p,j) * wm_interm_97_so_pt4(a,b,i,j)
term(251) = term(251) + s1(q,i) * wm_interm_55_so_pt4(a,b,p,j) * wm_interm_95_so_pt4(a,b,i,j)
term(252) = term(252) + s1(q,i) * wm_interm_55_so_pt4(a,b,p,j) * wm_interm_96_so_pt4(a,b,i,j)
term(253) = term(253) + s1(q,i) * wm_interm_10_so_pt4(a,b,p,j) * wm_interm_93_so_pt4(b,a,j,i)
term(254) = term(254) + s1(q,i) * wm_interm_11_so_pt4(a,b,p,j) * wm_interm_93_so_pt4(b,a,j,i)
term(255) = term(255) + s1(q,i) * wm_interm_10_so_pt4(a,b,p,j) * wm_interm_94_so_pt4(b,a,j,i)
term(256) = term(256) + s1(q,i) * wm_interm_11_so_pt4(a,b,p,j) * wm_interm_94_so_pt4(b,a,j,i)
term(257) = term(257) + s1(q,i) * wm_interm_56_so_pt4(a,b,p,j) * wm_interm_95_so_pt4(a,b,i,j)
term(258) = term(258) + s1(q,i) * wm_interm_56_so_pt4(a,b,p,j) * wm_interm_96_so_pt4(a,b,i,j)
term(259) = term(259) + s1(q,i) * wm_interm_1_so_pt4(a,b,p,j) * wm_interm_93_so_pt4(b,a,j,i)
term(260) = term(260) + s1(q,i) * wm_interm_1_so_pt4(a,b,p,j) * wm_interm_94_so_pt4(b,a,j,i)
term(261) = term(261) + s1(q,i) * wm_interm_5_so_pt4(a,b,p,j) * wm_interm_94_so_pt4(b,a,j,i)
term(262) = term(262) + s1(q,i) * wm_interm_5_so_pt4(a,b,p,j) * wm_interm_93_so_pt4(b,a,j,i)
term(263) = term(263) + s1(q,i) * wm_interm_56_so_pt4(a,b,p,j) * wm_interm_97_so_pt4(a,b,i,j)
term(264) = term(264) + s1(q,i) * wm_interm_56_so_pt4(a,b,p,j) * wm_interm_98_so_pt4(a,b,i,j)
term(265) = term(265) + s1(q,i) * wm_interm_59_so_pt4(a,b,p,j) * wm_interm_98_so_pt4(a,b,i,j)
term(266) = term(266) + s1(q,i) * wm_interm_59_so_pt4(a,b,p,j) * wm_interm_97_so_pt4(a,b,i,j)
term(267) = term(267) + s1(q,i) * wm_interm_59_so_pt4(a,b,p,j) * wm_interm_95_so_pt4(a,b,i,j)
term(268) = term(268) + s1(q,i) * wm_interm_59_so_pt4(a,b,p,j) * wm_interm_96_so_pt4(a,b,i,j)
term(269) = term(269) + s1(q,i) * wm_interm_100_so_pt4(a,b,j,i) * wm_interm_5_so_pt4(b,a,p,j)
term(270) = term(270) + s1(q,i) * wm_interm_100_so_pt4(a,b,j,i) * wm_interm_1_so_pt4(b,a,p,j)
term(271) = term(271) + s1(q,i) * wm_interm_102_so_pt4(a,b,j,i) * wm_interm_5_so_pt4(b,a,p,j)
term(272) = term(272) + s1(q,i) * wm_interm_102_so_pt4(a,b,j,i) * wm_interm_1_so_pt4(b,a,p,j)
term(273) = term(273) + s1(a,p) * wm_interm_54_so_pt4(q,b,i,j) * wm_interm_98_so_pt4(a,b,i,j)
term(274) = term(274) + s1(a,p) * wm_interm_54_so_pt4(q,b,i,j) * wm_interm_97_so_pt4(a,b,i,j)
term(275) = term(275) + s1(a,p) * wm_interm_54_so_pt4(q,b,i,j) * wm_interm_95_so_pt4(a,b,i,j)
term(276) = term(276) + s1(a,p) * wm_interm_54_so_pt4(q,b,i,j) * wm_interm_96_so_pt4(a,b,i,j)
term(277) = term(277) + s1(a,p) * wm_interm_100_so_pt4(b,a,i,j) * wm_interm_10_so_pt4(q,b,j,i)
term(278) = term(278) + s1(a,p) * wm_interm_100_so_pt4(b,a,i,j) * wm_interm_11_so_pt4(q,b,j,i)
term(279) = term(279) + s1(a,p) * wm_interm_102_so_pt4(b,a,i,j) * wm_interm_10_so_pt4(q,b,j,i)
term(280) = term(280) + s1(a,p) * wm_interm_102_so_pt4(b,a,i,j) * wm_interm_11_so_pt4(q,b,j,i)
term(281) = term(281) + s1(a,p) * wm_interm_55_so_pt4(q,b,i,j) * wm_interm_98_so_pt4(a,b,i,j)
term(282) = term(282) + s1(a,p) * wm_interm_55_so_pt4(q,b,i,j) * wm_interm_97_so_pt4(a,b,i,j)
term(283) = term(283) + s1(a,p) * wm_interm_55_so_pt4(q,b,i,j) * wm_interm_95_so_pt4(a,b,i,j)
term(284) = term(284) + s1(a,p) * wm_interm_55_so_pt4(q,b,i,j) * wm_interm_96_so_pt4(a,b,i,j)
term(285) = term(285) + s1(a,p) * wm_interm_10_so_pt4(q,b,i,j) * wm_interm_93_so_pt4(b,a,j,i)
term(286) = term(286) + s1(a,p) * wm_interm_11_so_pt4(q,b,i,j) * wm_interm_93_so_pt4(b,a,j,i)
term(287) = term(287) + s1(a,p) * wm_interm_10_so_pt4(q,b,i,j) * wm_interm_94_so_pt4(b,a,j,i)
term(288) = term(288) + s1(a,p) * wm_interm_11_so_pt4(q,b,i,j) * wm_interm_94_so_pt4(b,a,j,i)
term(289) = term(289) + s1(a,p) * wm_interm_56_so_pt4(q,b,i,j) * wm_interm_95_so_pt4(a,b,i,j)
term(290) = term(290) + s1(a,p) * wm_interm_56_so_pt4(q,b,i,j) * wm_interm_96_so_pt4(a,b,i,j)
term(291) = term(291) + s1(a,p) * wm_interm_1_so_pt4(q,b,i,j) * wm_interm_93_so_pt4(b,a,j,i)
term(292) = term(292) + s1(a,p) * wm_interm_1_so_pt4(q,b,i,j) * wm_interm_94_so_pt4(b,a,j,i)
term(293) = term(293) + s1(a,p) * wm_interm_56_so_pt4(q,b,i,j) * wm_interm_97_so_pt4(a,b,i,j)
term(294) = term(294) + s1(a,p) * wm_interm_56_so_pt4(q,b,i,j) * wm_interm_98_so_pt4(a,b,i,j)
term(295) = term(295) + s1(a,p) * wm_interm_5_so_pt4(q,b,i,j) * wm_interm_94_so_pt4(b,a,j,i)
term(296) = term(296) + s1(a,p) * wm_interm_5_so_pt4(q,b,i,j) * wm_interm_93_so_pt4(b,a,j,i)
term(297) = term(297) + s1(a,p) * wm_interm_59_so_pt4(q,b,i,j) * wm_interm_98_so_pt4(a,b,i,j)
term(298) = term(298) + s1(a,p) * wm_interm_59_so_pt4(q,b,i,j) * wm_interm_97_so_pt4(a,b,i,j)
term(299) = term(299) + s1(a,p) * wm_interm_59_so_pt4(q,b,i,j) * wm_interm_95_so_pt4(a,b,i,j)
term(300) = term(300) + s1(a,p) * wm_interm_59_so_pt4(q,b,i,j) * wm_interm_96_so_pt4(a,b,i,j)
term(301) = term(301) + s1(a,p) * wm_interm_100_so_pt4(b,a,i,j) * wm_interm_5_so_pt4(q,b,j,i)
term(302) = term(302) + s1(a,p) * wm_interm_100_so_pt4(b,a,i,j) * wm_interm_1_so_pt4(q,b,j,i)
term(303) = term(303) + s1(a,p) * wm_interm_102_so_pt4(b,a,i,j) * wm_interm_5_so_pt4(q,b,j,i)
term(304) = term(304) + s1(a,p) * wm_interm_102_so_pt4(b,a,i,j) * wm_interm_1_so_pt4(q,b,j,i)
end do 
end do 
end do 
end do 

term(20) = term(20) * (-2.0d+0) 
term(23) = term(23) * (-4.0d+0) 
term(24) = term(24) * (4.0d+0) 
term(25) = term(25) * (2.0d+0) 
term(26) = term(26) * (-4.0d+0) 
term(27) = term(27) * (2.0d+0) 
term(28) = term(28) * (2.0d+0) 
term(29) = term(29) * (2.0d+0) 
term(30) = term(30) * (-4.0d+0) 
term(31) = term(31) * (2.0d+0) 
term(32) = term(32) * (-4.0d+0) 
term(33) = term(33) * (2.0d+0) 
term(34) = term(34) * (2.0d+0) 
term(35) = term(35) * (2.0d+0) 
term(36) = term(36) * (-4.0d+0) 
term(37) = term(37) * (2.0d+0) 
term(38) = term(38) * (2.0d+0) 
term(39) = term(39) * (2.0d+0) 
term(40) = term(40) * (2.0d+0) 
term(41) = term(41) * (-4.0d+0) 
term(42) = term(42) * (-4.0d+0) 
term(43) = term(43) * (2.0d+0) 
term(44) = term(44) * (-4.0d+0) 
term(45) = term(45) * (-4.0d+0) 
term(46) = term(46) * (8.0d+0) 
term(47) = term(47) * (2.0d+0) 
term(48) = term(48) * (-4.0d+0) 
term(49) = term(49) * (2.0d+0) 
term(50) = term(50) * (-4.0d+0) 
term(51) = term(51) * (2.0d+0) 
term(52) = term(52) * (-4.0d+0) 
term(53) = term(53) * (-4.0d+0) 
term(54) = term(54) * (8.0d+0) 
term(55) = term(55) * (2.0d+0) 
term(56) = term(56) * (-4.0d+0) 
term(57) = term(57) * (2.0d+0) 
term(58) = term(58) * (-4.0d+0) 
term(59) = term(59) * (-4.0d+0) 
term(60) = term(60) * (8.0d+0) 
term(61) = term(61) * (2.0d+0) 
term(62) = term(62) * (-4.0d+0) 
term(63) = term(63) * (2.0d+0) 
term(64) = term(64) * (-4.0d+0) 
term(65) = term(65) * (2.0d+0) 
term(66) = term(66) * (-4.0d+0) 
term(67) = term(67) * (2.0d+0) 
term(68) = term(68) * (-4.0d+0) 
term(69) = term(69) * (2.0d+0) 
term(70) = term(70) * (2.0d+0) 
term(71) = term(71) * (2.0d+0) 
term(72) = term(72) * (2.0d+0) 
term(73) = term(73) * (2.0d+0) 
term(74) = term(74) * (2.0d+0) 
term(75) = term(75) * (2.0d+0) 
term(76) = term(76) * (2.0d+0) 
term(77) = term(77) * (-4.0d+0) 
term(78) = term(78) * (-4.0d+0) 
term(79) = term(79) * (2.0d+0) 
term(80) = term(80) * (-4.0d+0) 
term(81) = term(81) * (-4.0d+0) 
term(82) = term(82) * (8.0d+0) 
term(83) = term(83) * (2.0d+0) 
term(84) = term(84) * (-4.0d+0) 
term(85) = term(85) * (-4.0d+0) 
term(86) = term(86) * (8.0d+0) 
term(87) = term(87) * (2.0d+0) 
term(88) = term(88) * (-4.0d+0) 
term(89) = term(89) * (2.0d+0) 
term(90) = term(90) * (-4.0d+0) 
term(91) = term(91) * (2.0d+0) 
term(92) = term(92) * (-4.0d+0) 
term(93) = term(93) * (2.0d+0) 
term(94) = term(94) * (-4.0d+0) 
term(95) = term(95) * (-4.0d+0) 
term(96) = term(96) * (8.0d+0) 
term(97) = term(97) * (8.0d+0) 
term(98) = term(98) * (-8.0d+0) 
term(99) = term(99) * (8.0d+0) 
term(100) = term(100) * (8.0d+0) 
term(101) = term(101) * (8.0d+0) 
term(102) = term(102) * (-8.0d+0) 
term(103) = term(103) * (-8.0d+0) 
term(104) = term(104) * (-8.0d+0) 
term(105) = term(105) * (8.0d+0) 
term(106) = term(106) * (8.0d+0) 
term(107) = term(107) * (-8.0d+0) 
term(108) = term(108) * (-8.0d+0) 
term(109) = term(109) * (8.0d+0) 
term(110) = term(110) * (-8.0d+0) 
term(111) = term(111) * (-16.0d+0) 
term(112) = term(112) * (16.0d+0) 
term(113) = term(113) * (8.0d+0) 
term(114) = term(114) * (-16.0d+0) 
term(115) = term(115) * (-8.0d+0) 
term(116) = term(116) * (16.0d+0) 
term(117) = term(117) * (8.0d+0) 
term(118) = term(118) * (-16.0d+0) 
term(119) = term(119) * (-8.0d+0) 
term(120) = term(120) * (16.0d+0) 
term(121) = term(121) * (8.0d+0) 
term(122) = term(122) * (-8.0d+0) 
term(123) = term(123) * (8.0d+0) 
term(124) = term(124) * (-8.0d+0) 
term(125) = term(125) * (8.0d+0) 
term(126) = term(126) * (-8.0d+0) 
term(127) = term(127) * (8.0d+0) 
term(128) = term(128) * (-8.0d+0) 
term(129) = term(129) * (8.0d+0) 
term(130) = term(130) * (8.0d+0) 
term(131) = term(131) * (-8.0d+0) 
term(132) = term(132) * (-8.0d+0) 
term(133) = term(133) * (8.0d+0) 
term(134) = term(134) * (-8.0d+0) 
term(135) = term(135) * (-16.0d+0) 
term(136) = term(136) * (16.0d+0) 
term(137) = term(137) * (8.0d+0) 
term(138) = term(138) * (-16.0d+0) 
term(139) = term(139) * (-8.0d+0) 
term(140) = term(140) * (16.0d+0) 
term(141) = term(141) * (8.0d+0) 
term(142) = term(142) * (-16.0d+0) 
term(143) = term(143) * (-8.0d+0) 
term(144) = term(144) * (16.0d+0) 
term(145) = term(145) * (2.0d+0) 
term(146) = term(146) * (-4.0d+0) 
term(147) = term(147) * (-4.0d+0) 
term(148) = term(148) * (8.0d+0) 
term(149) = term(149) * (2.0d+0) 
term(150) = term(150) * (-4.0d+0) 
term(151) = term(151) * (2.0d+0) 
term(152) = term(152) * (-4.0d+0) 
term(153) = term(153) * (2.0d+0) 
term(154) = term(154) * (-4.0d+0) 
term(155) = term(155) * (-4.0d+0) 
term(156) = term(156) * (8.0d+0) 
term(157) = term(157) * (-1.0d+0) 
term(158) = term(158) * (2.0d+0) 
term(159) = term(159) * (2.0d+0) 
term(160) = term(160) * (-4.0d+0) 
term(161) = term(161) * (-1.0d+0) 
term(162) = term(162) * (2.0d+0) 
term(163) = term(163) * (-1.0d+0) 
term(164) = term(164) * (2.0d+0) 
term(165) = term(165) * (-1.0d+0) 
term(166) = term(166) * (2.0d+0) 
term(167) = term(167) * (2.0d+0) 
term(168) = term(168) * (-4.0d+0) 
term(169) = term(169) * (-1.0d+0) 
term(170) = term(170) * (2.0d+0) 
term(171) = term(171) * (-1.0d+0) 
term(172) = term(172) * (-1.0d+0) 
term(173) = term(173) * (-1.0d+0) 
term(174) = term(174) * (2.0d+0) 
term(175) = term(175) * (-1.0d+0) 
term(176) = term(176) * (2.0d+0) 
term(177) = term(177) * (-1.0d+0) 
term(178) = term(178) * (-1.0d+0) 
term(179) = term(179) * (-1.0d+0) 
term(180) = term(180) * (2.0d+0) 
term(181) = term(181) * (-1.0d+0) 
term(182) = term(182) * (2.0d+0) 
term(183) = term(183) * (2.0d+0) 
term(184) = term(184) * (-4.0d+0) 
term(185) = term(185) * (-1.0d+0) 
term(186) = term(186) * (2.0d+0) 
term(187) = term(187) * (-1.0d+0) 
term(188) = term(188) * (2.0d+0) 
term(189) = term(189) * (-1.0d+0) 
term(190) = term(190) * (2.0d+0) 
term(191) = term(191) * (2.0d+0) 
term(192) = term(192) * (-4.0d+0) 
term(193) = term(193) * (2.0d+0) 
term(194) = term(194) * (-4.0d+0) 
term(195) = term(195) * (-4.0d+0) 
term(196) = term(196) * (8.0d+0) 
term(197) = term(197) * (2.0d+0) 
term(198) = term(198) * (-4.0d+0) 
term(199) = term(199) * (-4.0d+0) 
term(200) = term(200) * (8.0d+0) 
term(201) = term(201) * (2.0d+0) 
term(202) = term(202) * (-4.0d+0) 
term(203) = term(203) * (2.0d+0) 
term(204) = term(204) * (-4.0d+0) 
term(205) = term(205) * (-1.0d+0) 
term(206) = term(206) * (2.0d+0) 
term(207) = term(207) * (2.0d+0) 
term(208) = term(208) * (-4.0d+0) 
term(209) = term(209) * (-1.0d+0) 
term(210) = term(210) * (2.0d+0) 
term(211) = term(211) * (2.0d+0) 
term(212) = term(212) * (-4.0d+0) 
term(213) = term(213) * (-1.0d+0) 
term(214) = term(214) * (2.0d+0) 
term(215) = term(215) * (-1.0d+0) 
term(216) = term(216) * (2.0d+0) 
term(217) = term(217) * (-1.0d+0) 
term(218) = term(218) * (2.0d+0) 
term(219) = term(219) * (-1.0d+0) 
term(220) = term(220) * (2.0d+0) 
term(221) = term(221) * (-1.0d+0) 
term(222) = term(222) * (2.0d+0) 
term(223) = term(223) * (-1.0d+0) 
term(224) = term(224) * (2.0d+0) 
term(225) = term(225) * (-1.0d+0) 
term(226) = term(226) * (2.0d+0) 
term(227) = term(227) * (2.0d+0) 
term(228) = term(228) * (-4.0d+0) 
term(229) = term(229) * (-1.0d+0) 
term(230) = term(230) * (2.0d+0) 
term(231) = term(231) * (2.0d+0) 
term(232) = term(232) * (-4.0d+0) 
term(233) = term(233) * (-1.0d+0) 
term(234) = term(234) * (-1.0d+0) 
term(235) = term(235) * (-1.0d+0) 
term(236) = term(236) * (-1.0d+0) 
term(237) = term(237) * (-1.0d+0) 
term(238) = term(238) * (2.0d+0) 
term(239) = term(239) * (-1.0d+0) 
term(240) = term(240) * (2.0d+0) 
term(241) = term(241) * (8.0d+0) 
term(242) = term(242) * (-8.0d+0) 
term(243) = term(243) * (-16.0d+0) 
term(244) = term(244) * (16.0d+0) 
term(245) = term(245) * (8.0d+0) 
term(246) = term(246) * (-16.0d+0) 
term(247) = term(247) * (-8.0d+0) 
term(248) = term(248) * (16.0d+0) 
term(249) = term(249) * (-4.0d+0) 
term(250) = term(250) * (4.0d+0) 
term(251) = term(251) * (8.0d+0) 
term(252) = term(252) * (-8.0d+0) 
term(253) = term(253) * (-4.0d+0) 
term(254) = term(254) * (8.0d+0) 
term(255) = term(255) * (4.0d+0) 
term(256) = term(256) * (-8.0d+0) 
term(257) = term(257) * (-4.0d+0) 
term(258) = term(258) * (4.0d+0) 
term(259) = term(259) * (-4.0d+0) 
term(260) = term(260) * (4.0d+0) 
term(261) = term(261) * (-4.0d+0) 
term(262) = term(262) * (4.0d+0) 
term(263) = term(263) * (-4.0d+0) 
term(264) = term(264) * (4.0d+0) 
term(265) = term(265) * (-4.0d+0) 
term(266) = term(266) * (4.0d+0) 
term(267) = term(267) * (8.0d+0) 
term(268) = term(268) * (-8.0d+0) 
term(269) = term(269) * (-4.0d+0) 
term(270) = term(270) * (8.0d+0) 
term(271) = term(271) * (4.0d+0) 
term(272) = term(272) * (-8.0d+0) 
term(273) = term(273) * (8.0d+0) 
term(274) = term(274) * (-8.0d+0) 
term(275) = term(275) * (-16.0d+0) 
term(276) = term(276) * (16.0d+0) 
term(277) = term(277) * (8.0d+0) 
term(278) = term(278) * (-16.0d+0) 
term(279) = term(279) * (-8.0d+0) 
term(280) = term(280) * (16.0d+0) 
term(281) = term(281) * (-4.0d+0) 
term(282) = term(282) * (4.0d+0) 
term(283) = term(283) * (8.0d+0) 
term(284) = term(284) * (-8.0d+0) 
term(285) = term(285) * (-4.0d+0) 
term(286) = term(286) * (8.0d+0) 
term(287) = term(287) * (4.0d+0) 
term(288) = term(288) * (-8.0d+0) 
term(289) = term(289) * (-4.0d+0) 
term(290) = term(290) * (4.0d+0) 
term(291) = term(291) * (-4.0d+0) 
term(292) = term(292) * (4.0d+0) 
term(293) = term(293) * (-4.0d+0) 
term(294) = term(294) * (4.0d+0) 
term(295) = term(295) * (-4.0d+0) 
term(296) = term(296) * (4.0d+0) 
term(297) = term(297) * (-4.0d+0) 
term(298) = term(298) * (4.0d+0) 
term(299) = term(299) * (8.0d+0) 
term(300) = term(300) * (-8.0d+0) 
term(301) = term(301) * (-4.0d+0) 
term(302) = term(302) * (8.0d+0) 
term(303) = term(303) * (4.0d+0) 
term(304) = term(304) * (-8.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(305) = term(305) + s2(a,q,p,i) * wm_interm_33_so_pt4(b,j) * wm_interm_56_so_pt4(b,a,j,i)
term(306) = term(306) + s2(a,q,p,i) * wm_interm_34_so_pt4(b,j) * wm_interm_56_so_pt4(b,a,j,i)
term(307) = term(307) + s2(a,q,p,i) * wm_interm_35_so_pt4(b,j) * wm_interm_56_so_pt4(b,a,j,i)
term(308) = term(308) + s2(a,q,p,i) * wm_interm_36_so_pt4(b,j) * wm_interm_56_so_pt4(b,a,j,i)
term(309) = term(309) + s2(a,q,p,i) * wm_interm_37_so_pt4(b,j) * wm_interm_56_so_pt4(b,a,j,i)
term(310) = term(310) + r2(vrdav_Rl, a,j,q,i) * wm_interm_19_so_pt4(b,a) * wm_interm_28_so_pt4(b,i,j,p)
term(311) = term(311) + r2(vrdav_Rl, a,j,q,i) * wm_interm_20_so_pt4(b,a) * wm_interm_28_so_pt4(b,i,j,p)
term(312) = term(312) + r2(vrdav_Rl, a,j,q,i) * wm_interm_19_so_pt4(b,a) * wm_interm_27_so_pt4(b,i,j,p)
term(313) = term(313) + r2(vrdav_Rl, a,j,q,i) * wm_interm_20_so_pt4(b,a) * wm_interm_27_so_pt4(b,i,j,p)
term(314) = term(314) + s2(a,q,j,i) * wm_interm_28_so_pt4(b,i,j,p) * wm_interm_57_so_pt4(b,a)
term(315) = term(315) + s2(a,q,j,i) * wm_interm_28_so_pt4(b,i,j,p) * wm_interm_58_so_pt4(b,a)
term(316) = term(316) + s2(a,q,j,i) * wm_interm_27_so_pt4(b,i,j,p) * wm_interm_57_so_pt4(b,a)
term(317) = term(317) + s2(a,q,j,i) * wm_interm_27_so_pt4(b,i,j,p) * wm_interm_58_so_pt4(b,a)
term(318) = term(318) + r2(vrdav_Rl, a,j,q,i) * wm_interm_19_so_pt4(b,a) * wm_interm_31_so_pt4(b,i,j,p)
term(319) = term(319) + r2(vrdav_Rl, a,j,q,i) * wm_interm_20_so_pt4(b,a) * wm_interm_31_so_pt4(b,i,j,p)
term(320) = term(320) + r2(vrdav_Rl, a,j,q,i) * wm_interm_19_so_pt4(b,a) * wm_interm_30_so_pt4(b,i,j,p)
term(321) = term(321) + r2(vrdav_Rl, a,j,q,i) * wm_interm_20_so_pt4(b,a) * wm_interm_30_so_pt4(b,i,j,p)
term(322) = term(322) + s2(a,q,j,i) * wm_interm_30_so_pt4(b,i,j,p) * wm_interm_57_so_pt4(b,a)
term(323) = term(323) + s2(a,q,j,i) * wm_interm_30_so_pt4(b,i,j,p) * wm_interm_58_so_pt4(b,a)
term(324) = term(324) + s2(a,q,p,i) * wm_interm_6_so_pt4(b,j) * wm_interm_77_so_pt4(b,a,j,i)
term(325) = term(325) + s2(a,q,p,i) * wm_interm_6_so_pt4(b,j) * wm_interm_79_so_pt4(b,a,j,i)
term(326) = term(326) + s2(a,q,p,i) * wm_interm_6_so_pt4(b,j) * wm_interm_75_so_pt4(b,a,j,i)
term(327) = term(327) + s2(a,q,p,i) * wm_interm_66_so_pt4(b,a,j,i) * wm_interm_6_so_pt4(b,j)
term(328) = term(328) + s2(a,q,p,i) * wm_interm_65_so_pt4(b,a,j,i) * wm_interm_6_so_pt4(b,j)
term(329) = term(329) + s2(a,q,p,i) * wm_interm_63_so_pt4(b,a,j,i) * wm_interm_6_so_pt4(b,j)
term(330) = term(330) + s2(a,q,p,i) * wm_interm_66_so_pt4(b,a,j,i) * wm_interm_7_so_pt4(b,j)
term(331) = term(331) + s2(a,q,p,i) * wm_interm_65_so_pt4(b,a,j,i) * wm_interm_7_so_pt4(b,j)
term(332) = term(332) + s2(a,q,p,i) * wm_interm_77_so_pt4(b,a,j,i) * wm_interm_7_so_pt4(b,j)
term(333) = term(333) + s2(a,q,p,i) * wm_interm_79_so_pt4(b,a,j,i) * wm_interm_7_so_pt4(b,j)
term(334) = term(334) + s2(a,q,p,i) * wm_interm_63_so_pt4(b,a,j,i) * wm_interm_7_so_pt4(b,j)
term(335) = term(335) + s2(a,q,p,i) * wm_interm_75_so_pt4(b,a,j,i) * wm_interm_7_so_pt4(b,j)
term(336) = term(336) + s2(a,q,p,i) * wm_interm_100_so_pt4(b,a,j,i) * wm_interm_6_so_pt4(b,j)
term(337) = term(337) + s2(a,q,p,i) * wm_interm_102_so_pt4(b,a,j,i) * wm_interm_6_so_pt4(b,j)
term(338) = term(338) + s2(a,q,p,i) * wm_interm_6_so_pt4(b,j) * wm_interm_93_so_pt4(b,a,j,i)
term(339) = term(339) + s2(a,q,p,i) * wm_interm_6_so_pt4(b,j) * wm_interm_94_so_pt4(b,a,j,i)
term(340) = term(340) + s2(a,q,p,i) * wm_interm_7_so_pt4(b,j) * wm_interm_93_so_pt4(b,a,j,i)
term(341) = term(341) + s2(a,q,p,i) * wm_interm_7_so_pt4(b,j) * wm_interm_94_so_pt4(b,a,j,i)
term(342) = term(342) + s2(a,q,p,i) * wm_interm_100_so_pt4(b,a,j,i) * wm_interm_7_so_pt4(b,j)
term(343) = term(343) + s2(a,q,p,i) * wm_interm_102_so_pt4(b,a,j,i) * wm_interm_7_so_pt4(b,j)
end do 
end do 
end do 
end do 

term(305) = term(305) * (-2.0d+0) 
term(308) = term(308) * (-4.0d+0) 
term(309) = term(309) * (4.0d+0) 
term(310) = term(310) * (2.0d+0) 
term(311) = term(311) * (-4.0d+0) 
term(312) = term(312) * (-1.0d+0) 
term(313) = term(313) * (2.0d+0) 
term(314) = term(314) * (-4.0d+0) 
term(315) = term(315) * (2.0d+0) 
term(316) = term(316) * (2.0d+0) 
term(317) = term(317) * (-1.0d+0) 
term(318) = term(318) * (4.0d+0) 
term(319) = term(319) * (-8.0d+0) 
term(320) = term(320) * (-2.0d+0) 
term(321) = term(321) * (4.0d+0) 
term(322) = term(322) * (8.0d+0) 
term(323) = term(323) * (-4.0d+0) 
term(324) = term(324) * (-4.0d+0) 
term(325) = term(325) * (8.0d+0) 
term(326) = term(326) * (-4.0d+0) 
term(327) = term(327) * (2.0d+0) 
term(328) = term(328) * (-4.0d+0) 
term(329) = term(329) * (2.0d+0) 
term(330) = term(330) * (-1.0d+0) 
term(331) = term(331) * (2.0d+0) 
term(332) = term(332) * (2.0d+0) 
term(333) = term(333) * (-4.0d+0) 
term(334) = term(334) * (-1.0d+0) 
term(335) = term(335) * (2.0d+0) 
term(336) = term(336) * (-16.0d+0) 
term(337) = term(337) * (16.0d+0) 
term(338) = term(338) * (8.0d+0) 
term(339) = term(339) * (-8.0d+0) 
term(340) = term(340) * (-4.0d+0) 
term(341) = term(341) * (4.0d+0) 
term(342) = term(342) * (8.0d+0) 
term(343) = term(343) * (-8.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(344) = term(344) + r2(vrdav_Rl, a,j,q,i) * wm_interm_33_so_pt4(a,k) * wm_interm_9_so_pt4(k,p,i,j)
term(345) = term(345) + r2(vrdav_Rl, a,j,q,i) * wm_interm_34_so_pt4(a,k) * wm_interm_9_so_pt4(k,p,i,j)
term(346) = term(346) + r2(vrdav_Rl, a,j,q,i) * wm_interm_35_so_pt4(a,k) * wm_interm_9_so_pt4(k,p,i,j)
term(347) = term(347) + s2(a,q,j,i) * wm_interm_33_so_pt4(a,k) * wm_interm_61_so_pt4(k,p,i,j)
term(348) = term(348) + s2(a,q,j,i) * wm_interm_34_so_pt4(a,k) * wm_interm_61_so_pt4(k,p,i,j)
term(349) = term(349) + s2(a,q,j,i) * wm_interm_35_so_pt4(a,k) * wm_interm_61_so_pt4(k,p,i,j)
term(350) = term(350) + r2(vrdav_Rl, a,j,q,i) * wm_interm_36_so_pt4(a,k) * wm_interm_9_so_pt4(k,p,i,j)
term(351) = term(351) + r2(vrdav_Rl, a,j,q,i) * wm_interm_37_so_pt4(a,k) * wm_interm_9_so_pt4(k,p,i,j)
term(352) = term(352) + s2(a,q,j,i) * wm_interm_36_so_pt4(a,k) * wm_interm_61_so_pt4(k,p,i,j)
term(353) = term(353) + s2(a,q,j,i) * wm_interm_37_so_pt4(a,k) * wm_interm_61_so_pt4(k,p,i,j)
term(354) = term(354) + s2(a,q,j,i) * wm_interm_6_so_pt4(a,k) * wm_interm_71_so_pt4(p,k,i,j)
term(355) = term(355) + s2(a,q,j,i) * wm_interm_6_so_pt4(a,k) * wm_interm_72_so_pt4(p,k,i,j)
term(356) = term(356) + s2(a,q,j,i) * wm_interm_71_so_pt4(p,k,i,j) * wm_interm_7_so_pt4(a,k)
term(357) = term(357) + s2(a,q,j,i) * wm_interm_72_so_pt4(p,k,i,j) * wm_interm_7_so_pt4(a,k)
term(358) = term(358) + s2(a,q,j,i) * wm_interm_6_so_pt4(a,k) * wm_interm_71_so_pt4(k,p,i,j)
term(359) = term(359) + s2(a,q,j,i) * wm_interm_6_so_pt4(a,k) * wm_interm_72_so_pt4(k,p,i,j)
term(360) = term(360) + s2(a,q,j,i) * wm_interm_71_so_pt4(k,p,i,j) * wm_interm_7_so_pt4(a,k)
term(361) = term(361) + s2(a,q,j,i) * wm_interm_72_so_pt4(k,p,i,j) * wm_interm_7_so_pt4(a,k)
term(362) = term(362) + s2(a,q,j,i) * wm_interm_6_so_pt4(a,k) * wm_interm_99_so_pt4(p,k,i,j)
term(363) = term(363) + s2(a,q,j,i) * wm_interm_7_so_pt4(a,k) * wm_interm_99_so_pt4(p,k,i,j)
term(364) = term(364) + s2(a,q,j,i) * wm_interm_6_so_pt4(a,k) * wm_interm_99_so_pt4(k,p,i,j)
term(365) = term(365) + s2(a,q,j,i) * wm_interm_7_so_pt4(a,k) * wm_interm_99_so_pt4(k,p,i,j)
end do 
end do 
end do 
end do 

term(344) = term(344) * (-1.0d+0) 
term(345) = term(345) * (0.5d+0) 
term(346) = term(346) * (0.5d+0) 
term(347) = term(347) * (-1.0d+0) 
term(348) = term(348) * (0.5d+0) 
term(349) = term(349) * (0.5d+0) 
term(350) = term(350) * (-2.0d+0) 
term(351) = term(351) * (2.0d+0) 
term(352) = term(352) * (-2.0d+0) 
term(353) = term(353) * (2.0d+0) 
term(354) = term(354) * (4.0d+0) 
term(355) = term(355) * (-2.0d+0) 
term(356) = term(356) * (-2.0d+0) 
term(358) = term(358) * (-2.0d+0) 
term(359) = term(359) * (4.0d+0) 
term(361) = term(361) * (-2.0d+0) 
term(362) = term(362) * (8.0d+0) 
term(363) = term(363) * (-4.0d+0) 
term(364) = term(364) * (-8.0d+0) 
term(365) = term(365) * (4.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(366) = term(366) + r2(vrdav_Rl, a,j,q,i) * wm_interm_33_so_pt4(a,k) * wm_interm_9_so_pt4(p,k,j,i)
term(367) = term(367) + r2(vrdav_Rl, a,j,q,i) * wm_interm_34_so_pt4(a,k) * wm_interm_9_so_pt4(p,k,j,i)
term(368) = term(368) + r2(vrdav_Rl, a,j,q,i) * wm_interm_35_so_pt4(a,k) * wm_interm_9_so_pt4(p,k,j,i)
term(369) = term(369) + s2(a,q,j,i) * wm_interm_33_so_pt4(a,k) * wm_interm_61_so_pt4(p,k,j,i)
term(370) = term(370) + s2(a,q,j,i) * wm_interm_34_so_pt4(a,k) * wm_interm_61_so_pt4(p,k,j,i)
term(371) = term(371) + s2(a,q,j,i) * wm_interm_35_so_pt4(a,k) * wm_interm_61_so_pt4(p,k,j,i)
term(372) = term(372) + r2(vrdav_Rl, a,j,q,i) * wm_interm_36_so_pt4(a,k) * wm_interm_9_so_pt4(p,k,j,i)
term(373) = term(373) + r2(vrdav_Rl, a,j,q,i) * wm_interm_37_so_pt4(a,k) * wm_interm_9_so_pt4(p,k,j,i)
term(374) = term(374) + s2(a,q,j,i) * wm_interm_36_so_pt4(a,k) * wm_interm_61_so_pt4(p,k,j,i)
term(375) = term(375) + s2(a,q,j,i) * wm_interm_37_so_pt4(a,k) * wm_interm_61_so_pt4(p,k,j,i)
term(376) = term(376) + s2(a,q,j,i) * wm_interm_6_so_pt4(a,k) * wm_interm_71_so_pt4(p,k,j,i)
term(377) = term(377) + s2(a,q,j,i) * wm_interm_71_so_pt4(p,k,j,i) * wm_interm_7_so_pt4(a,k)
term(378) = term(378) + s2(a,q,j,i) * wm_interm_6_so_pt4(a,k) * wm_interm_72_so_pt4(k,p,j,i)
term(379) = term(379) + s2(a,q,j,i) * wm_interm_72_so_pt4(k,p,j,i) * wm_interm_7_so_pt4(a,k)
term(380) = term(380) + s2(a,q,j,i) * wm_interm_6_so_pt4(a,k) * wm_interm_99_so_pt4(p,k,j,i)
term(381) = term(381) + s2(a,q,j,i) * wm_interm_7_so_pt4(a,k) * wm_interm_99_so_pt4(p,k,j,i)
term(382) = term(382) + s2(a,q,j,i) * wm_interm_6_so_pt4(a,k) * wm_interm_99_so_pt4(k,p,j,i)
term(383) = term(383) + s2(a,q,j,i) * wm_interm_7_so_pt4(a,k) * wm_interm_99_so_pt4(k,p,j,i)
end do 
end do 
end do 
end do 

term(366) = term(366) * (-1.0d+0) 
term(367) = term(367) * (0.5d+0) 
term(368) = term(368) * (0.5d+0) 
term(369) = term(369) * (-1.0d+0) 
term(370) = term(370) * (0.5d+0) 
term(371) = term(371) * (0.5d+0) 
term(372) = term(372) * (-2.0d+0) 
term(373) = term(373) * (2.0d+0) 
term(374) = term(374) * (-2.0d+0) 
term(375) = term(375) * (2.0d+0) 
term(376) = term(376) * (-2.0d+0) 
term(378) = term(378) * (-2.0d+0) 
term(380) = term(380) * (-8.0d+0) 
term(381) = term(381) * (4.0d+0) 
term(382) = term(382) * (8.0d+0) 
term(383) = term(383) * (-4.0d+0) 

do l = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(384) = term(384) + r2(vrdav_Rl, a,p,q,i) * wm_interm_27_so_pt4(a,j,k,l) * wm_interm_9_so_pt4(k,j,i,l)
term(385) = term(385) + r2(vrdav_Rl, a,p,q,i) * wm_interm_27_so_pt4(a,j,k,l) * wm_interm_9_so_pt4(j,k,i,l)
term(386) = term(386) + r2(vrdav_Rl, a,p,q,i) * wm_interm_28_so_pt4(a,j,k,l) * wm_interm_9_so_pt4(j,k,i,l)
term(387) = term(387) + r2(vrdav_Rl, a,j,q,i) * wm_interm_88_so_pt4(i,j,k,l) * wm_interm_89_so_pt4(a,p,k,l)
term(388) = term(388) + r2(vrdav_Rl, a,p,q,i) * wm_interm_88_so_pt4(j,i,k,l) * wm_interm_89_so_pt4(a,j,k,l)
term(389) = term(389) + r2(vrdav_Rl, a,p,q,i) * wm_interm_88_so_pt4(i,j,k,l) * wm_interm_89_so_pt4(a,j,k,l)
term(390) = term(390) + s2(a,q,p,i) * wm_interm_3_so_pt4(a,j,k,l) * wm_interm_71_so_pt4(l,k,j,i)
term(391) = term(391) + s2(a,q,j,i) * wm_interm_3_so_pt4(a,p,k,l) * wm_interm_72_so_pt4(l,k,j,i)
term(392) = term(392) + s2(a,q,j,i) * wm_interm_3_so_pt4(a,p,k,l) * wm_interm_71_so_pt4(k,l,j,i)
term(393) = term(393) + s2(a,q,p,i) * wm_interm_3_so_pt4(a,j,k,l) * wm_interm_72_so_pt4(k,l,j,i)
term(394) = term(394) + s2(a,q,p,i) * wm_interm_28_so_pt4(a,j,k,l) * wm_interm_61_so_pt4(j,k,i,l)
term(395) = term(395) + s2(a,q,p,i) * wm_interm_27_so_pt4(a,j,k,l) * wm_interm_61_so_pt4(k,j,i,l)
term(396) = term(396) + s2(a,q,p,i) * wm_interm_27_so_pt4(a,j,k,l) * wm_interm_61_so_pt4(j,k,i,l)
term(397) = term(397) + s2(a,q,p,i) * wm_interm_28_so_pt4(a,j,k,l) * wm_interm_61_so_pt4(k,j,l,i)
term(398) = term(398) + s2(a,q,p,i) * wm_interm_27_so_pt4(a,j,k,l) * wm_interm_61_so_pt4(j,k,l,i)
term(399) = term(399) + s2(a,q,p,i) * wm_interm_27_so_pt4(a,j,k,l) * wm_interm_61_so_pt4(k,j,l,i)
term(400) = term(400) + r2(vrdav_Rl, a,p,q,i) * wm_interm_30_so_pt4(a,j,k,l) * wm_interm_9_so_pt4(k,j,i,l)
term(401) = term(401) + r2(vrdav_Rl, a,p,q,i) * wm_interm_30_so_pt4(a,j,k,l) * wm_interm_9_so_pt4(j,k,i,l)
term(402) = term(402) + r2(vrdav_Rl, a,j,q,i) * wm_interm_109_so_pt4(i,j,k,l) * wm_interm_89_so_pt4(a,p,k,l)
term(403) = term(403) + r2(vrdav_Rl, a,p,q,i) * wm_interm_109_so_pt4(j,i,k,l) * wm_interm_89_so_pt4(a,j,k,l)
term(404) = term(404) + r2(vrdav_Rl, a,p,q,i) * wm_interm_109_so_pt4(i,j,k,l) * wm_interm_89_so_pt4(a,j,k,l)
term(405) = term(405) + s2(a,q,p,i) * wm_interm_3_so_pt4(a,j,k,l) * wm_interm_99_so_pt4(l,k,j,i)
term(406) = term(406) + s2(a,q,j,i) * wm_interm_3_so_pt4(a,p,k,l) * wm_interm_99_so_pt4(l,k,j,i)
term(407) = term(407) + s2(a,q,j,i) * wm_interm_3_so_pt4(a,p,k,l) * wm_interm_99_so_pt4(k,l,j,i)
term(408) = term(408) + s2(a,q,p,i) * wm_interm_3_so_pt4(a,j,k,l) * wm_interm_99_so_pt4(k,l,j,i)
term(409) = term(409) + s2(a,q,p,i) * wm_interm_31_so_pt4(a,j,k,l) * wm_interm_61_so_pt4(j,k,i,l)
term(410) = term(410) + s2(a,q,p,i) * wm_interm_30_so_pt4(a,j,k,l) * wm_interm_61_so_pt4(k,j,i,l)
term(411) = term(411) + s2(a,q,p,i) * wm_interm_30_so_pt4(a,j,k,l) * wm_interm_61_so_pt4(j,k,i,l)
term(412) = term(412) + s2(a,q,p,i) * wm_interm_31_so_pt4(a,j,k,l) * wm_interm_61_so_pt4(k,j,l,i)
term(413) = term(413) + s2(a,q,p,i) * wm_interm_30_so_pt4(a,j,k,l) * wm_interm_61_so_pt4(j,k,l,i)
term(414) = term(414) + s2(a,q,p,i) * wm_interm_30_so_pt4(a,j,k,l) * wm_interm_61_so_pt4(k,j,l,i)
end do 
end do 
end do 
end do 
end do 

term(384) = term(384) * (2.0d+0) 
term(385) = term(385) * (-4.0d+0) 
term(386) = term(386) * (2.0d+0) 
term(387) = term(387) * (2.0d+0) 
term(388) = term(388) * (2.0d+0) 
term(389) = term(389) * (-4.0d+0) 
term(396) = term(396) * (-2.0d+0) 
term(399) = term(399) * (-2.0d+0) 
term(400) = term(400) * (8.0d+0) 
term(401) = term(401) * (-8.0d+0) 
term(402) = term(402) * (4.0d+0) 
term(403) = term(403) * (4.0d+0) 
term(404) = term(404) * (-8.0d+0) 
term(405) = term(405) * (4.0d+0) 
term(406) = term(406) * (-4.0d+0) 
term(407) = term(407) * (4.0d+0) 
term(408) = term(408) * (-4.0d+0) 
term(409) = term(409) * (2.0d+0) 
term(410) = term(410) * (2.0d+0) 
term(411) = term(411) * (-4.0d+0) 
term(412) = term(412) * (2.0d+0) 
term(413) = term(413) * (2.0d+0) 
term(414) = term(414) * (-4.0d+0) 

do i = 1, nocc 
term(415) = term(415) + wm_interm_25_so_pt4(p,i) * wm_interm_45_so_pt4(q,i)
term(416) = term(416) + wm_interm_50_so_pt4(q,i) * wm_interm_8_so_pt4(i,p)
term(417) = term(417) + wm_interm_51_so_pt4(q,i) * wm_interm_8_so_pt4(i,p)
end do 

term(415) = term(415) * (2.0d+0) 
term(416) = term(416) * (-4.0d+0) 
term(417) = term(417) * (2.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(418) = term(418) + r1(vrdav_Rl, q,i) * wm_interm_2_so_pt4(a,j) * wm_interm_3_so_pt4(a,p,j,i)
term(419) = term(419) + s2(a,q,p,i) * wm_interm_4_so_pt4(j,i) * wm_interm_6_so_pt4(a,j)
term(420) = term(420) + s2(a,q,p,i) * wm_interm_4_so_pt4(j,i) * wm_interm_7_so_pt4(a,j)
term(421) = term(421) + s1(q,i) * wm_interm_13_so_pt4(a,p,j,i) * wm_interm_2_so_pt4(a,j)
term(422) = term(422) + s2(a,q,p,i) * wm_interm_14_so_pt4(a,j) * wm_interm_8_so_pt4(i,j)
term(423) = term(423) + s2(a,q,p,i) * wm_interm_16_so_pt4(a,j) * wm_interm_8_so_pt4(i,j)
term(424) = term(424) + s2(a,q,j,i) * wm_interm_14_so_pt4(a,j) * wm_interm_8_so_pt4(i,p)
term(425) = term(425) + s2(a,q,j,i) * wm_interm_16_so_pt4(a,j) * wm_interm_8_so_pt4(i,p)
term(426) = term(426) + r2(vrdav_Rl, a,p,q,i) * wm_interm_17_so_pt4(j,i) * wm_interm_33_so_pt4(a,j)
term(427) = term(427) + r2(vrdav_Rl, a,p,q,i) * wm_interm_18_so_pt4(j,i) * wm_interm_33_so_pt4(a,j)
term(428) = term(428) + r2(vrdav_Rl, a,p,q,i) * wm_interm_17_so_pt4(j,i) * wm_interm_34_so_pt4(a,j)
term(429) = term(429) + r2(vrdav_Rl, a,p,q,i) * wm_interm_18_so_pt4(j,i) * wm_interm_34_so_pt4(a,j)
term(430) = term(430) + r2(vrdav_Rl, a,p,q,i) * wm_interm_17_so_pt4(j,i) * wm_interm_35_so_pt4(a,j)
term(431) = term(431) + r2(vrdav_Rl, a,p,q,i) * wm_interm_18_so_pt4(j,i) * wm_interm_35_so_pt4(a,j)
term(432) = term(432) + s2(a,q,p,i) * wm_interm_33_so_pt4(a,j) * wm_interm_60_so_pt4(j,i)
term(433) = term(433) + s2(a,q,p,i) * wm_interm_33_so_pt4(a,j) * wm_interm_62_so_pt4(j,i)
term(434) = term(434) + s2(a,q,p,i) * wm_interm_34_so_pt4(a,j) * wm_interm_60_so_pt4(j,i)
term(435) = term(435) + s2(a,q,p,i) * wm_interm_34_so_pt4(a,j) * wm_interm_62_so_pt4(j,i)
term(436) = term(436) + s2(a,q,p,i) * wm_interm_35_so_pt4(a,j) * wm_interm_60_so_pt4(j,i)
term(437) = term(437) + s2(a,q,p,i) * wm_interm_35_so_pt4(a,j) * wm_interm_62_so_pt4(j,i)
term(438) = term(438) + r2(vrdav_Rl, a,p,q,i) * wm_interm_17_so_pt4(j,i) * wm_interm_36_so_pt4(a,j)
term(439) = term(439) + r2(vrdav_Rl, a,p,q,i) * wm_interm_18_so_pt4(j,i) * wm_interm_36_so_pt4(a,j)
term(440) = term(440) + r2(vrdav_Rl, a,p,q,i) * wm_interm_17_so_pt4(j,i) * wm_interm_37_so_pt4(a,j)
term(441) = term(441) + r2(vrdav_Rl, a,p,q,i) * wm_interm_18_so_pt4(j,i) * wm_interm_37_so_pt4(a,j)
term(442) = term(442) + s2(a,q,p,i) * wm_interm_36_so_pt4(a,j) * wm_interm_60_so_pt4(j,i)
term(443) = term(443) + s2(a,q,p,i) * wm_interm_36_so_pt4(a,j) * wm_interm_62_so_pt4(j,i)
term(444) = term(444) + s2(a,q,p,i) * wm_interm_37_so_pt4(a,j) * wm_interm_60_so_pt4(j,i)
term(445) = term(445) + s2(a,q,p,i) * wm_interm_37_so_pt4(a,j) * wm_interm_62_so_pt4(j,i)
term(446) = term(446) + s2(a,q,p,i) * wm_interm_6_so_pt4(a,j) * wm_interm_76_so_pt4(j,i)
term(447) = term(447) + s2(a,q,p,i) * wm_interm_6_so_pt4(a,j) * wm_interm_78_so_pt4(j,i)
term(448) = term(448) + s2(a,q,p,i) * wm_interm_6_so_pt4(a,j) * wm_interm_80_so_pt4(j,i)
term(449) = term(449) + s2(a,q,p,i) * wm_interm_76_so_pt4(j,i) * wm_interm_7_so_pt4(a,j)
term(450) = term(450) + s2(a,q,p,i) * wm_interm_78_so_pt4(j,i) * wm_interm_7_so_pt4(a,j)
term(451) = term(451) + s2(a,q,p,i) * wm_interm_7_so_pt4(a,j) * wm_interm_80_so_pt4(j,i)
term(452) = term(452) + r2(vrdav_Rl, a,p,q,i) * wm_interm_7_so_pt4(a,j) * wm_interm_85_so_pt4(i,j)
term(453) = term(453) + r2(vrdav_Rl, a,p,q,i) * wm_interm_7_so_pt4(a,j) * wm_interm_86_so_pt4(i,j)
term(454) = term(454) + r2(vrdav_Rl, a,p,q,i) * wm_interm_7_so_pt4(a,j) * wm_interm_87_so_pt4(i,j)
term(455) = term(455) + r2(vrdav_Rl, a,p,q,i) * wm_interm_6_so_pt4(a,j) * wm_interm_85_so_pt4(i,j)
term(456) = term(456) + r2(vrdav_Rl, a,p,q,i) * wm_interm_6_so_pt4(a,j) * wm_interm_86_so_pt4(i,j)
term(457) = term(457) + r2(vrdav_Rl, a,p,q,i) * wm_interm_6_so_pt4(a,j) * wm_interm_87_so_pt4(i,j)
term(458) = term(458) + s1(a,p) * wm_interm_101_so_pt4(i,j) * wm_interm_1_so_pt4(q,a,j,i)
term(459) = term(459) + s1(a,p) * wm_interm_103_so_pt4(i,j) * wm_interm_1_so_pt4(q,a,j,i)
term(460) = term(460) + s2(a,q,p,i) * wm_interm_101_so_pt4(j,i) * wm_interm_6_so_pt4(a,j)
term(461) = term(461) + s2(a,q,p,i) * wm_interm_103_so_pt4(j,i) * wm_interm_6_so_pt4(a,j)
term(462) = term(462) + s2(a,q,p,i) * wm_interm_101_so_pt4(j,i) * wm_interm_7_so_pt4(a,j)
term(463) = term(463) + s2(a,q,p,i) * wm_interm_103_so_pt4(j,i) * wm_interm_7_so_pt4(a,j)
term(464) = term(464) + s1(a,p) * wm_interm_101_so_pt4(i,j) * wm_interm_10_so_pt4(q,a,j,i)
term(465) = term(465) + s1(a,p) * wm_interm_101_so_pt4(i,j) * wm_interm_11_so_pt4(q,a,j,i)
term(466) = term(466) + s1(a,p) * wm_interm_103_so_pt4(i,j) * wm_interm_10_so_pt4(q,a,j,i)
term(467) = term(467) + s1(a,p) * wm_interm_103_so_pt4(i,j) * wm_interm_11_so_pt4(q,a,j,i)
term(468) = term(468) + r2(vrdav_Rl, a,p,q,i) * wm_interm_106_so_pt4(i,j) * wm_interm_7_so_pt4(a,j)
term(469) = term(469) + r2(vrdav_Rl, a,p,q,i) * wm_interm_107_so_pt4(i,j) * wm_interm_7_so_pt4(a,j)
term(470) = term(470) + r2(vrdav_Rl, a,p,q,i) * wm_interm_108_so_pt4(i,j) * wm_interm_7_so_pt4(a,j)
term(471) = term(471) + r2(vrdav_Rl, a,p,q,i) * wm_interm_106_so_pt4(i,j) * wm_interm_6_so_pt4(a,j)
term(472) = term(472) + r2(vrdav_Rl, a,p,q,i) * wm_interm_107_so_pt4(i,j) * wm_interm_6_so_pt4(a,j)
term(473) = term(473) + r2(vrdav_Rl, a,p,q,i) * wm_interm_108_so_pt4(i,j) * wm_interm_6_so_pt4(a,j)
term(474) = term(474) + s1(a,p) * wm_interm_17_so_pt4(i,j) * wm_interm_65_so_pt4(q,a,j,i)
term(475) = term(475) + s1(a,p) * wm_interm_17_so_pt4(i,j) * wm_interm_66_so_pt4(q,a,j,i)
term(476) = term(476) + s1(a,p) * wm_interm_18_so_pt4(i,j) * wm_interm_65_so_pt4(q,a,j,i)
term(477) = term(477) + s1(a,p) * wm_interm_18_so_pt4(i,j) * wm_interm_66_so_pt4(q,a,j,i)
term(478) = term(478) + s1(a,p) * wm_interm_17_so_pt4(i,j) * wm_interm_77_so_pt4(q,a,j,i)
term(479) = term(479) + s1(a,p) * wm_interm_18_so_pt4(i,j) * wm_interm_77_so_pt4(q,a,j,i)
term(480) = term(480) + s1(a,p) * wm_interm_17_so_pt4(i,j) * wm_interm_79_so_pt4(q,a,j,i)
term(481) = term(481) + s1(a,p) * wm_interm_18_so_pt4(i,j) * wm_interm_79_so_pt4(q,a,j,i)
term(482) = term(482) + s1(a,p) * wm_interm_17_so_pt4(i,j) * wm_interm_75_so_pt4(q,a,j,i)
term(483) = term(483) + s1(a,p) * wm_interm_18_so_pt4(i,j) * wm_interm_75_so_pt4(q,a,j,i)
term(484) = term(484) + s1(a,p) * wm_interm_17_so_pt4(i,j) * wm_interm_64_so_pt4(q,a,j,i)
term(485) = term(485) + s1(a,p) * wm_interm_18_so_pt4(i,j) * wm_interm_64_so_pt4(q,a,j,i)
term(486) = term(486) + s1(a,p) * wm_interm_17_so_pt4(i,j) * wm_interm_94_so_pt4(q,a,j,i)
term(487) = term(487) + s1(a,p) * wm_interm_17_so_pt4(i,j) * wm_interm_93_so_pt4(q,a,j,i)
term(488) = term(488) + s1(a,p) * wm_interm_18_so_pt4(i,j) * wm_interm_94_so_pt4(q,a,j,i)
term(489) = term(489) + s1(a,p) * wm_interm_18_so_pt4(i,j) * wm_interm_93_so_pt4(q,a,j,i)
term(490) = term(490) + r2(vrdav_Rl, a,j,q,i) * wm_interm_6_so_pt4(a,j) * wm_interm_85_so_pt4(i,p)
term(491) = term(491) + r2(vrdav_Rl, a,j,q,i) * wm_interm_6_so_pt4(a,j) * wm_interm_86_so_pt4(i,p)
term(492) = term(492) + r2(vrdav_Rl, a,j,q,i) * wm_interm_6_so_pt4(a,j) * wm_interm_87_so_pt4(i,p)
term(493) = term(493) + r2(vrdav_Rl, a,j,q,i) * wm_interm_7_so_pt4(a,j) * wm_interm_85_so_pt4(i,p)
term(494) = term(494) + r2(vrdav_Rl, a,j,q,i) * wm_interm_7_so_pt4(a,j) * wm_interm_86_so_pt4(i,p)
term(495) = term(495) + r2(vrdav_Rl, a,j,q,i) * wm_interm_7_so_pt4(a,j) * wm_interm_87_so_pt4(i,p)
term(496) = term(496) + r2(vrdav_Rl, a,j,q,i) * wm_interm_106_so_pt4(i,p) * wm_interm_6_so_pt4(a,j)
term(497) = term(497) + r2(vrdav_Rl, a,j,q,i) * wm_interm_107_so_pt4(i,p) * wm_interm_6_so_pt4(a,j)
term(498) = term(498) + r2(vrdav_Rl, a,j,q,i) * wm_interm_108_so_pt4(i,p) * wm_interm_6_so_pt4(a,j)
term(499) = term(499) + r2(vrdav_Rl, a,j,q,i) * wm_interm_106_so_pt4(i,p) * wm_interm_7_so_pt4(a,j)
term(500) = term(500) + r2(vrdav_Rl, a,j,q,i) * wm_interm_107_so_pt4(i,p) * wm_interm_7_so_pt4(a,j)
term(501) = term(501) + r2(vrdav_Rl, a,j,q,i) * wm_interm_108_so_pt4(i,p) * wm_interm_7_so_pt4(a,j)
end do 
end do 
end do 

term(418) = term(418) * (-4.0d+0) 
term(419) = term(419) * (8.0d+0) 
term(420) = term(420) * (-4.0d+0) 
term(421) = term(421) * (-4.0d+0) 
term(422) = term(422) * (8.0d+0) 
term(423) = term(423) * (-4.0d+0) 
term(424) = term(424) * (8.0d+0) 
term(425) = term(425) * (-4.0d+0) 
term(426) = term(426) * (-2.0d+0) 
term(427) = term(427) * (4.0d+0) 
term(429) = term(429) * (-2.0d+0) 
term(431) = term(431) * (-2.0d+0) 
term(432) = term(432) * (-2.0d+0) 
term(433) = term(433) * (4.0d+0) 
term(435) = term(435) * (-2.0d+0) 
term(437) = term(437) * (-2.0d+0) 
term(438) = term(438) * (-4.0d+0) 
term(439) = term(439) * (8.0d+0) 
term(440) = term(440) * (4.0d+0) 
term(441) = term(441) * (-8.0d+0) 
term(442) = term(442) * (-4.0d+0) 
term(443) = term(443) * (8.0d+0) 
term(444) = term(444) * (4.0d+0) 
term(445) = term(445) * (-8.0d+0) 
term(446) = term(446) * (-4.0d+0) 
term(447) = term(447) * (-4.0d+0) 
term(448) = term(448) * (8.0d+0) 
term(449) = term(449) * (2.0d+0) 
term(450) = term(450) * (2.0d+0) 
term(451) = term(451) * (-4.0d+0) 
term(452) = term(452) * (2.0d+0) 
term(453) = term(453) * (-4.0d+0) 
term(454) = term(454) * (2.0d+0) 
term(455) = term(455) * (-4.0d+0) 
term(456) = term(456) * (8.0d+0) 
term(457) = term(457) * (-4.0d+0) 
term(458) = term(458) * (8.0d+0) 
term(459) = term(459) * (-8.0d+0) 
term(460) = term(460) * (-16.0d+0) 
term(461) = term(461) * (16.0d+0) 
term(462) = term(462) * (8.0d+0) 
term(463) = term(463) * (-8.0d+0) 
term(464) = term(464) * (8.0d+0) 
term(465) = term(465) * (-16.0d+0) 
term(466) = term(466) * (-8.0d+0) 
term(467) = term(467) * (16.0d+0) 
term(468) = term(468) * (4.0d+0) 
term(469) = term(469) * (-8.0d+0) 
term(470) = term(470) * (4.0d+0) 
term(471) = term(471) * (-8.0d+0) 
term(472) = term(472) * (16.0d+0) 
term(473) = term(473) * (-8.0d+0) 
term(474) = term(474) * (-1.0d+0) 
term(475) = term(475) * (2.0d+0) 
term(476) = term(476) * (2.0d+0) 
term(477) = term(477) * (-4.0d+0) 
term(478) = term(478) * (-1.0d+0) 
term(479) = term(479) * (2.0d+0) 
term(480) = term(480) * (2.0d+0) 
term(481) = term(481) * (-4.0d+0) 
term(482) = term(482) * (-1.0d+0) 
term(483) = term(483) * (2.0d+0) 
term(484) = term(484) * (-1.0d+0) 
term(485) = term(485) * (2.0d+0) 
term(486) = term(486) * (-4.0d+0) 
term(487) = term(487) * (4.0d+0) 
term(488) = term(488) * (8.0d+0) 
term(489) = term(489) * (-8.0d+0) 
term(490) = term(490) * (-4.0d+0) 
term(491) = term(491) * (8.0d+0) 
term(492) = term(492) * (-4.0d+0) 
term(493) = term(493) * (2.0d+0) 
term(494) = term(494) * (-4.0d+0) 
term(495) = term(495) * (2.0d+0) 
term(496) = term(496) * (-8.0d+0) 
term(497) = term(497) * (16.0d+0) 
term(498) = term(498) * (-8.0d+0) 
term(499) = term(499) * (4.0d+0) 
term(500) = term(500) * (-8.0d+0) 
term(501) = term(501) * (4.0d+0) 

do j = 1, nocc 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(502) = term(502) + s1(a,i) * wm_interm_4_so_pt4(j,i) * wm_interm_5_so_pt4(q,a,p,j)
term(503) = term(503) + r1(vrdav_Rl, a,i) * wm_interm_5_so_pt4(q,a,p,j) * wm_interm_8_so_pt4(i,j)
term(504) = term(504) + s1(a,i) * wm_interm_5_so_pt4(q,a,p,j) * wm_interm_76_so_pt4(j,i)
term(505) = term(505) + s1(a,i) * wm_interm_5_so_pt4(q,a,p,j) * wm_interm_78_so_pt4(j,i)
term(506) = term(506) + s1(a,i) * wm_interm_5_so_pt4(q,a,p,j) * wm_interm_80_so_pt4(j,i)
term(507) = term(507) + s1(a,i) * wm_interm_56_so_pt4(q,a,p,j) * wm_interm_85_so_pt4(i,j)
term(508) = term(508) + s1(a,i) * wm_interm_56_so_pt4(q,a,p,j) * wm_interm_86_so_pt4(i,j)
term(509) = term(509) + s1(a,i) * wm_interm_56_so_pt4(q,a,p,j) * wm_interm_87_so_pt4(i,j)
term(510) = term(510) + s1(a,i) * wm_interm_101_so_pt4(j,i) * wm_interm_5_so_pt4(q,a,p,j)
term(511) = term(511) + s1(a,i) * wm_interm_103_so_pt4(j,i) * wm_interm_5_so_pt4(q,a,p,j)
term(512) = term(512) + s1(a,i) * wm_interm_106_so_pt4(i,j) * wm_interm_56_so_pt4(q,a,p,j)
term(513) = term(513) + s1(a,i) * wm_interm_107_so_pt4(i,j) * wm_interm_56_so_pt4(q,a,p,j)
term(514) = term(514) + s1(a,i) * wm_interm_108_so_pt4(i,j) * wm_interm_56_so_pt4(q,a,p,j)
end do 
end do 
end do 

term(502) = term(502) * (-4.0d+0) 
term(503) = term(503) * (-4.0d+0) 
term(504) = term(504) * (2.0d+0) 
term(505) = term(505) * (2.0d+0) 
term(506) = term(506) * (-4.0d+0) 
term(507) = term(507) * (2.0d+0) 
term(508) = term(508) * (-4.0d+0) 
term(509) = term(509) * (2.0d+0) 
term(510) = term(510) * (8.0d+0) 
term(511) = term(511) * (-8.0d+0) 
term(512) = term(512) * (4.0d+0) 
term(513) = term(513) * (-8.0d+0) 
term(514) = term(514) * (4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(515) = term(515) + s1(a,p) * wm_interm_1_so_pt4(q,a,i,j) * wm_interm_4_so_pt4(j,i)
term(516) = term(516) + s1(a,p) * wm_interm_10_so_pt4(q,a,i,j) * wm_interm_4_so_pt4(j,i)
term(517) = term(517) + s1(a,p) * wm_interm_11_so_pt4(q,a,i,j) * wm_interm_4_so_pt4(j,i)
term(518) = term(518) + r1(vrdav_Rl, a,i) * wm_interm_12_so_pt4(q,i,p,j) * wm_interm_7_so_pt4(a,j)
term(519) = term(519) + s1(a,i) * wm_interm_12_so_pt4(q,i,p,j) * wm_interm_14_so_pt4(a,j)
term(520) = term(520) + r1(vrdav_Rl, a,i) * wm_interm_12_so_pt4(q,i,p,j) * wm_interm_6_so_pt4(a,j)
term(521) = term(521) + r1(vrdav_Rl, a,p) * wm_interm_1_so_pt4(q,a,i,j) * wm_interm_8_so_pt4(i,j)
term(522) = term(522) + r1(vrdav_Rl, a,p) * wm_interm_10_so_pt4(q,a,i,j) * wm_interm_8_so_pt4(i,j)
term(523) = term(523) + r1(vrdav_Rl, a,p) * wm_interm_11_so_pt4(q,a,i,j) * wm_interm_8_so_pt4(i,j)
term(524) = term(524) + s1(a,i) * wm_interm_12_so_pt4(q,i,p,j) * wm_interm_16_so_pt4(a,j)
term(525) = term(525) + s1(q,i) * wm_interm_12_so_pt4(a,i,j,p) * wm_interm_14_so_pt4(a,j)
term(526) = term(526) + s1(q,i) * wm_interm_12_so_pt4(a,i,p,j) * wm_interm_14_so_pt4(a,j)
term(527) = term(527) + s1(q,i) * wm_interm_12_so_pt4(a,i,j,p) * wm_interm_16_so_pt4(a,j)
term(528) = term(528) + s1(q,i) * wm_interm_12_so_pt4(a,i,p,j) * wm_interm_16_so_pt4(a,j)
term(529) = term(529) + s2(a,q,i,j) * wm_interm_14_so_pt4(a,j) * wm_interm_8_so_pt4(i,p)
term(530) = term(530) + s2(a,q,i,j) * wm_interm_16_so_pt4(a,j) * wm_interm_8_so_pt4(i,p)
term(531) = term(531) + r2(vrdav_Rl, a,i,q,j) * wm_interm_17_so_pt4(p,i) * wm_interm_33_so_pt4(a,j)
term(532) = term(532) + r2(vrdav_Rl, a,i,q,j) * wm_interm_18_so_pt4(p,i) * wm_interm_33_so_pt4(a,j)
term(533) = term(533) + r2(vrdav_Rl, a,i,q,j) * wm_interm_17_so_pt4(p,i) * wm_interm_34_so_pt4(a,j)
term(534) = term(534) + r2(vrdav_Rl, a,i,q,j) * wm_interm_18_so_pt4(p,i) * wm_interm_34_so_pt4(a,j)
term(535) = term(535) + r2(vrdav_Rl, a,i,q,j) * wm_interm_17_so_pt4(p,i) * wm_interm_35_so_pt4(a,j)
term(536) = term(536) + r2(vrdav_Rl, a,i,q,j) * wm_interm_18_so_pt4(p,i) * wm_interm_35_so_pt4(a,j)
term(537) = term(537) + s2(a,q,i,j) * wm_interm_33_so_pt4(a,j) * wm_interm_60_so_pt4(p,i)
term(538) = term(538) + s2(a,q,i,j) * wm_interm_33_so_pt4(a,j) * wm_interm_62_so_pt4(p,i)
term(539) = term(539) + s2(a,q,i,j) * wm_interm_34_so_pt4(a,j) * wm_interm_60_so_pt4(p,i)
term(540) = term(540) + s2(a,q,i,j) * wm_interm_34_so_pt4(a,j) * wm_interm_62_so_pt4(p,i)
term(541) = term(541) + s2(a,q,i,j) * wm_interm_35_so_pt4(a,j) * wm_interm_60_so_pt4(p,i)
term(542) = term(542) + s2(a,q,i,j) * wm_interm_35_so_pt4(a,j) * wm_interm_62_so_pt4(p,i)
term(543) = term(543) + r2(vrdav_Rl, a,i,q,j) * wm_interm_17_so_pt4(p,i) * wm_interm_36_so_pt4(a,j)
term(544) = term(544) + r2(vrdav_Rl, a,i,q,j) * wm_interm_18_so_pt4(p,i) * wm_interm_36_so_pt4(a,j)
term(545) = term(545) + r2(vrdav_Rl, a,i,q,j) * wm_interm_17_so_pt4(p,i) * wm_interm_37_so_pt4(a,j)
term(546) = term(546) + r2(vrdav_Rl, a,i,q,j) * wm_interm_18_so_pt4(p,i) * wm_interm_37_so_pt4(a,j)
term(547) = term(547) + s2(a,q,i,j) * wm_interm_36_so_pt4(a,j) * wm_interm_60_so_pt4(p,i)
term(548) = term(548) + s2(a,q,i,j) * wm_interm_36_so_pt4(a,j) * wm_interm_62_so_pt4(p,i)
term(549) = term(549) + s2(a,q,i,j) * wm_interm_37_so_pt4(a,j) * wm_interm_60_so_pt4(p,i)
term(550) = term(550) + s2(a,q,i,j) * wm_interm_37_so_pt4(a,j) * wm_interm_62_so_pt4(p,i)
term(551) = term(551) + s1(a,p) * wm_interm_1_so_pt4(q,a,i,j) * wm_interm_76_so_pt4(j,i)
term(552) = term(552) + s1(a,p) * wm_interm_1_so_pt4(q,a,i,j) * wm_interm_78_so_pt4(j,i)
term(553) = term(553) + s1(a,p) * wm_interm_1_so_pt4(q,a,i,j) * wm_interm_80_so_pt4(j,i)
term(554) = term(554) + s1(a,p) * wm_interm_10_so_pt4(q,a,i,j) * wm_interm_76_so_pt4(j,i)
term(555) = term(555) + s1(a,p) * wm_interm_11_so_pt4(q,a,i,j) * wm_interm_76_so_pt4(j,i)
term(556) = term(556) + s1(a,p) * wm_interm_10_so_pt4(q,a,i,j) * wm_interm_78_so_pt4(j,i)
term(557) = term(557) + s1(a,p) * wm_interm_11_so_pt4(q,a,i,j) * wm_interm_78_so_pt4(j,i)
term(558) = term(558) + s1(a,p) * wm_interm_10_so_pt4(q,a,i,j) * wm_interm_80_so_pt4(j,i)
term(559) = term(559) + s1(a,p) * wm_interm_11_so_pt4(q,a,i,j) * wm_interm_80_so_pt4(j,i)
term(560) = term(560) + s1(a,p) * wm_interm_54_so_pt4(q,a,i,j) * wm_interm_85_so_pt4(i,j)
term(561) = term(561) + s1(a,p) * wm_interm_54_so_pt4(q,a,i,j) * wm_interm_86_so_pt4(i,j)
term(562) = term(562) + s1(a,p) * wm_interm_54_so_pt4(q,a,i,j) * wm_interm_87_so_pt4(i,j)
term(563) = term(563) + s1(a,p) * wm_interm_55_so_pt4(q,a,i,j) * wm_interm_85_so_pt4(i,j)
term(564) = term(564) + s1(a,p) * wm_interm_55_so_pt4(q,a,i,j) * wm_interm_86_so_pt4(i,j)
term(565) = term(565) + s1(a,p) * wm_interm_55_so_pt4(q,a,i,j) * wm_interm_87_so_pt4(i,j)
term(566) = term(566) + s1(a,p) * wm_interm_59_so_pt4(q,a,i,j) * wm_interm_85_so_pt4(i,j)
term(567) = term(567) + s1(a,p) * wm_interm_59_so_pt4(q,a,i,j) * wm_interm_86_so_pt4(i,j)
term(568) = term(568) + s1(a,p) * wm_interm_59_so_pt4(q,a,i,j) * wm_interm_87_so_pt4(i,j)
term(569) = term(569) + s1(a,p) * wm_interm_106_so_pt4(i,j) * wm_interm_54_so_pt4(q,a,i,j)
term(570) = term(570) + s1(a,p) * wm_interm_107_so_pt4(i,j) * wm_interm_54_so_pt4(q,a,i,j)
term(571) = term(571) + s1(a,p) * wm_interm_108_so_pt4(i,j) * wm_interm_54_so_pt4(q,a,i,j)
term(572) = term(572) + s1(a,p) * wm_interm_106_so_pt4(i,j) * wm_interm_55_so_pt4(q,a,i,j)
term(573) = term(573) + s1(a,p) * wm_interm_107_so_pt4(i,j) * wm_interm_55_so_pt4(q,a,i,j)
term(574) = term(574) + s1(a,p) * wm_interm_108_so_pt4(i,j) * wm_interm_55_so_pt4(q,a,i,j)
term(575) = term(575) + s1(a,p) * wm_interm_106_so_pt4(i,j) * wm_interm_59_so_pt4(q,a,i,j)
term(576) = term(576) + s1(a,p) * wm_interm_107_so_pt4(i,j) * wm_interm_59_so_pt4(q,a,i,j)
term(577) = term(577) + s1(a,p) * wm_interm_108_so_pt4(i,j) * wm_interm_59_so_pt4(q,a,i,j)
term(578) = term(578) + s1(a,p) * wm_interm_60_so_pt4(i,j) * wm_interm_67_so_pt4(a,q,i,j)
term(579) = term(579) + s1(a,p) * wm_interm_60_so_pt4(i,j) * wm_interm_68_so_pt4(a,q,i,j)
term(580) = term(580) + s1(a,p) * wm_interm_60_so_pt4(i,j) * wm_interm_69_so_pt4(a,q,i,j)
term(581) = term(581) + s1(a,p) * wm_interm_60_so_pt4(i,j) * wm_interm_70_so_pt4(a,q,i,j)
term(582) = term(582) + s1(a,p) * wm_interm_62_so_pt4(i,j) * wm_interm_67_so_pt4(a,q,i,j)
term(583) = term(583) + s1(a,p) * wm_interm_62_so_pt4(i,j) * wm_interm_68_so_pt4(a,q,i,j)
term(584) = term(584) + s1(a,p) * wm_interm_62_so_pt4(i,j) * wm_interm_69_so_pt4(a,q,i,j)
term(585) = term(585) + s1(a,p) * wm_interm_62_so_pt4(i,j) * wm_interm_70_so_pt4(a,q,i,j)
term(586) = term(586) + s1(a,p) * wm_interm_60_so_pt4(i,j) * wm_interm_73_so_pt4(a,q,i,j)
term(587) = term(587) + s1(a,p) * wm_interm_60_so_pt4(i,j) * wm_interm_74_so_pt4(a,q,i,j)
term(588) = term(588) + s1(a,p) * wm_interm_62_so_pt4(i,j) * wm_interm_73_so_pt4(a,q,i,j)
term(589) = term(589) + s1(a,p) * wm_interm_62_so_pt4(i,j) * wm_interm_74_so_pt4(a,q,i,j)
term(590) = term(590) + s1(a,p) * wm_interm_100_so_pt4(q,a,i,j) * wm_interm_17_so_pt4(j,i)
term(591) = term(591) + s1(a,p) * wm_interm_100_so_pt4(q,a,i,j) * wm_interm_18_so_pt4(j,i)
term(592) = term(592) + s1(a,p) * wm_interm_102_so_pt4(q,a,i,j) * wm_interm_17_so_pt4(j,i)
term(593) = term(593) + s1(a,p) * wm_interm_102_so_pt4(q,a,i,j) * wm_interm_18_so_pt4(j,i)
term(594) = term(594) + s1(a,p) * wm_interm_60_so_pt4(i,j) * wm_interm_95_so_pt4(a,q,i,j)
term(595) = term(595) + s1(a,p) * wm_interm_60_so_pt4(i,j) * wm_interm_96_so_pt4(a,q,i,j)
term(596) = term(596) + s1(a,p) * wm_interm_60_so_pt4(i,j) * wm_interm_97_so_pt4(a,q,i,j)
term(597) = term(597) + s1(a,p) * wm_interm_60_so_pt4(i,j) * wm_interm_98_so_pt4(a,q,i,j)
term(598) = term(598) + s1(a,p) * wm_interm_62_so_pt4(i,j) * wm_interm_95_so_pt4(a,q,i,j)
term(599) = term(599) + s1(a,p) * wm_interm_62_so_pt4(i,j) * wm_interm_96_so_pt4(a,q,i,j)
term(600) = term(600) + s1(a,p) * wm_interm_62_so_pt4(i,j) * wm_interm_97_so_pt4(a,q,i,j)
term(601) = term(601) + s1(a,p) * wm_interm_62_so_pt4(i,j) * wm_interm_98_so_pt4(a,q,i,j)
term(602) = term(602) + s1(a,i) * wm_interm_60_so_pt4(j,i) * wm_interm_84_so_pt4(a,q,j,p)
term(603) = term(603) + s1(a,i) * wm_interm_60_so_pt4(j,i) * wm_interm_73_so_pt4(a,q,j,p)
term(604) = term(604) + s1(a,i) * wm_interm_62_so_pt4(j,i) * wm_interm_84_so_pt4(a,q,j,p)
term(605) = term(605) + s1(a,i) * wm_interm_62_so_pt4(j,i) * wm_interm_73_so_pt4(a,q,j,p)
term(606) = term(606) + s1(a,i) * wm_interm_60_so_pt4(j,i) * wm_interm_70_so_pt4(a,q,j,p)
term(607) = term(607) + s1(a,i) * wm_interm_62_so_pt4(j,i) * wm_interm_70_so_pt4(a,q,j,p)
term(608) = term(608) + r2(vrdav_Rl, a,i,q,j) * wm_interm_6_so_pt4(a,j) * wm_interm_85_so_pt4(i,p)
term(609) = term(609) + r2(vrdav_Rl, a,i,q,j) * wm_interm_6_so_pt4(a,j) * wm_interm_86_so_pt4(i,p)
term(610) = term(610) + r2(vrdav_Rl, a,i,q,j) * wm_interm_6_so_pt4(a,j) * wm_interm_87_so_pt4(i,p)
term(611) = term(611) + r2(vrdav_Rl, a,i,q,j) * wm_interm_7_so_pt4(a,j) * wm_interm_85_so_pt4(i,p)
term(612) = term(612) + r2(vrdav_Rl, a,i,q,j) * wm_interm_7_so_pt4(a,j) * wm_interm_86_so_pt4(i,p)
term(613) = term(613) + r2(vrdav_Rl, a,i,q,j) * wm_interm_7_so_pt4(a,j) * wm_interm_87_so_pt4(i,p)
term(614) = term(614) + s1(a,i) * wm_interm_60_so_pt4(j,i) * wm_interm_98_so_pt4(a,q,j,p)
term(615) = term(615) + s1(a,i) * wm_interm_60_so_pt4(j,i) * wm_interm_97_so_pt4(a,q,j,p)
term(616) = term(616) + s1(a,i) * wm_interm_62_so_pt4(j,i) * wm_interm_98_so_pt4(a,q,j,p)
term(617) = term(617) + s1(a,i) * wm_interm_62_so_pt4(j,i) * wm_interm_97_so_pt4(a,q,j,p)
term(618) = term(618) + r2(vrdav_Rl, a,i,q,j) * wm_interm_106_so_pt4(i,p) * wm_interm_6_so_pt4(a,j)
term(619) = term(619) + r2(vrdav_Rl, a,i,q,j) * wm_interm_107_so_pt4(i,p) * wm_interm_6_so_pt4(a,j)
term(620) = term(620) + r2(vrdav_Rl, a,i,q,j) * wm_interm_108_so_pt4(i,p) * wm_interm_6_so_pt4(a,j)
term(621) = term(621) + r2(vrdav_Rl, a,i,q,j) * wm_interm_106_so_pt4(i,p) * wm_interm_7_so_pt4(a,j)
term(622) = term(622) + r2(vrdav_Rl, a,i,q,j) * wm_interm_107_so_pt4(i,p) * wm_interm_7_so_pt4(a,j)
term(623) = term(623) + r2(vrdav_Rl, a,i,q,j) * wm_interm_108_so_pt4(i,p) * wm_interm_7_so_pt4(a,j)
end do 
end do 
end do 

term(515) = term(515) * (-4.0d+0) 
term(516) = term(516) * (-4.0d+0) 
term(517) = term(517) * (8.0d+0) 
term(518) = term(518) * (-4.0d+0) 
term(519) = term(519) * (8.0d+0) 
term(520) = term(520) * (8.0d+0) 
term(521) = term(521) * (-4.0d+0) 
term(522) = term(522) * (-4.0d+0) 
term(523) = term(523) * (8.0d+0) 
term(524) = term(524) * (-4.0d+0) 
term(525) = term(525) * (-4.0d+0) 
term(526) = term(526) * (8.0d+0) 
term(527) = term(527) * (2.0d+0) 
term(528) = term(528) * (-4.0d+0) 
term(529) = term(529) * (-4.0d+0) 
term(530) = term(530) * (2.0d+0) 
term(531) = term(531) * (-2.0d+0) 
term(532) = term(532) * (4.0d+0) 
term(534) = term(534) * (-2.0d+0) 
term(536) = term(536) * (-2.0d+0) 
term(537) = term(537) * (-2.0d+0) 
term(538) = term(538) * (4.0d+0) 
term(540) = term(540) * (-2.0d+0) 
term(542) = term(542) * (-2.0d+0) 
term(543) = term(543) * (-4.0d+0) 
term(544) = term(544) * (8.0d+0) 
term(545) = term(545) * (4.0d+0) 
term(546) = term(546) * (-8.0d+0) 
term(547) = term(547) * (-4.0d+0) 
term(548) = term(548) * (8.0d+0) 
term(549) = term(549) * (4.0d+0) 
term(550) = term(550) * (-8.0d+0) 
term(551) = term(551) * (2.0d+0) 
term(552) = term(552) * (2.0d+0) 
term(553) = term(553) * (-4.0d+0) 
term(554) = term(554) * (2.0d+0) 
term(555) = term(555) * (-4.0d+0) 
term(556) = term(556) * (2.0d+0) 
term(557) = term(557) * (-4.0d+0) 
term(558) = term(558) * (-4.0d+0) 
term(559) = term(559) * (8.0d+0) 
term(560) = term(560) * (-4.0d+0) 
term(561) = term(561) * (8.0d+0) 
term(562) = term(562) * (-4.0d+0) 
term(563) = term(563) * (2.0d+0) 
term(564) = term(564) * (-4.0d+0) 
term(565) = term(565) * (2.0d+0) 
term(566) = term(566) * (2.0d+0) 
term(567) = term(567) * (-4.0d+0) 
term(568) = term(568) * (2.0d+0) 
term(569) = term(569) * (-8.0d+0) 
term(570) = term(570) * (16.0d+0) 
term(571) = term(571) * (-8.0d+0) 
term(572) = term(572) * (4.0d+0) 
term(573) = term(573) * (-8.0d+0) 
term(574) = term(574) * (4.0d+0) 
term(575) = term(575) * (4.0d+0) 
term(576) = term(576) * (-8.0d+0) 
term(577) = term(577) * (4.0d+0) 
term(578) = term(578) * (-1.0d+0) 
term(579) = term(579) * (2.0d+0) 
term(580) = term(580) * (-1.0d+0) 
term(581) = term(581) * (2.0d+0) 
term(582) = term(582) * (2.0d+0) 
term(583) = term(583) * (-4.0d+0) 
term(584) = term(584) * (2.0d+0) 
term(585) = term(585) * (-4.0d+0) 
term(586) = term(586) * (-1.0d+0) 
term(587) = term(587) * (-1.0d+0) 
term(588) = term(588) * (2.0d+0) 
term(589) = term(589) * (2.0d+0) 
term(590) = term(590) * (-4.0d+0) 
term(591) = term(591) * (8.0d+0) 
term(592) = term(592) * (4.0d+0) 
term(593) = term(593) * (-8.0d+0) 
term(594) = term(594) * (-4.0d+0) 
term(595) = term(595) * (4.0d+0) 
term(596) = term(596) * (-4.0d+0) 
term(597) = term(597) * (4.0d+0) 
term(598) = term(598) * (8.0d+0) 
term(599) = term(599) * (-8.0d+0) 
term(600) = term(600) * (8.0d+0) 
term(601) = term(601) * (-8.0d+0) 
term(602) = term(602) * (-1.0d+0) 
term(603) = term(603) * (2.0d+0) 
term(604) = term(604) * (2.0d+0) 
term(605) = term(605) * (-4.0d+0) 
term(606) = term(606) * (-1.0d+0) 
term(607) = term(607) * (2.0d+0) 
term(608) = term(608) * (2.0d+0) 
term(609) = term(609) * (-4.0d+0) 
term(610) = term(610) * (2.0d+0) 
term(611) = term(611) * (-1.0d+0) 
term(612) = term(612) * (2.0d+0) 
term(613) = term(613) * (-1.0d+0) 
term(614) = term(614) * (-4.0d+0) 
term(615) = term(615) * (4.0d+0) 
term(616) = term(616) * (8.0d+0) 
term(617) = term(617) * (-8.0d+0) 
term(618) = term(618) * (4.0d+0) 
term(619) = term(619) * (-8.0d+0) 
term(620) = term(620) * (4.0d+0) 
term(621) = term(621) * (-2.0d+0) 
term(622) = term(622) * (4.0d+0) 
term(623) = term(623) * (-2.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(624) = term(624) + r1(vrdav_Rl, q,i) * wm_interm_8_so_pt4(j,k) * wm_interm_9_so_pt4(j,p,i,k)
term(625) = term(625) + s1(q,i) * wm_interm_61_so_pt4(j,p,i,k) * wm_interm_85_so_pt4(j,k)
term(626) = term(626) + s1(q,i) * wm_interm_61_so_pt4(j,p,i,k) * wm_interm_86_so_pt4(j,k)
term(627) = term(627) + s1(q,i) * wm_interm_61_so_pt4(j,p,i,k) * wm_interm_87_so_pt4(j,k)
term(628) = term(628) + s1(q,i) * wm_interm_106_so_pt4(j,k) * wm_interm_61_so_pt4(j,p,i,k)
term(629) = term(629) + s1(q,i) * wm_interm_107_so_pt4(j,k) * wm_interm_61_so_pt4(j,p,i,k)
term(630) = term(630) + s1(q,i) * wm_interm_108_so_pt4(j,k) * wm_interm_61_so_pt4(j,p,i,k)
term(631) = term(631) + s1(q,i) * wm_interm_60_so_pt4(j,k) * wm_interm_88_so_pt4(j,i,p,k)
term(632) = term(632) + s1(q,i) * wm_interm_62_so_pt4(j,k) * wm_interm_88_so_pt4(j,i,p,k)
term(633) = term(633) + s1(q,i) * wm_interm_109_so_pt4(j,i,p,k) * wm_interm_60_so_pt4(j,k)
term(634) = term(634) + s1(q,i) * wm_interm_109_so_pt4(j,i,p,k) * wm_interm_62_so_pt4(j,k)
end do 
end do 
end do 

term(624) = term(624) * (-4.0d+0) 
term(626) = term(626) * (-2.0d+0) 
term(628) = term(628) * (2.0d+0) 
term(629) = term(629) * (-4.0d+0) 
term(630) = term(630) * (2.0d+0) 
term(631) = term(631) * (-1.0d+0) 
term(632) = term(632) * (2.0d+0) 
term(633) = term(633) * (-2.0d+0) 
term(634) = term(634) * (4.0d+0) 

do a = nocc + 1, nactive 
term(635) = term(635) + wm_interm_44_so_pt4(q,a) * wm_interm_45_so_pt4(a,p)
term(636) = term(636) + wm_interm_0_so_pt4(a,q) * wm_interm_50_so_pt4(a,p)
term(637) = term(637) + wm_interm_0_so_pt4(a,q) * wm_interm_51_so_pt4(a,p)
end do 

term(635) = term(635) * (2.0d+0) 
term(636) = term(636) * (-4.0d+0) 
term(637) = term(637) * (2.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(638) = term(638) + r2(vrdav_Rl, a,j,q,i) * wm_interm_88_so_pt4(j,i,k,l) * wm_interm_89_so_pt4(a,p,l,k)
term(639) = term(639) + r2(vrdav_Rl, a,j,q,i) * wm_interm_88_so_pt4(i,j,k,l) * wm_interm_89_so_pt4(a,p,l,k)
term(640) = term(640) + r2(vrdav_Rl, a,p,q,i) * wm_interm_88_so_pt4(i,j,k,l) * wm_interm_89_so_pt4(a,j,l,k)
term(641) = term(641) + r2(vrdav_Rl, a,j,q,i) * wm_interm_109_so_pt4(j,i,k,l) * wm_interm_89_so_pt4(a,p,l,k)
term(642) = term(642) + r2(vrdav_Rl, a,j,q,i) * wm_interm_109_so_pt4(i,j,k,l) * wm_interm_89_so_pt4(a,p,l,k)
term(643) = term(643) + r2(vrdav_Rl, a,p,q,i) * wm_interm_109_so_pt4(i,j,k,l) * wm_interm_89_so_pt4(a,j,l,k)
end do 
end do 
end do 
end do 
end do 

term(638) = term(638) * (2.0d+0) 
term(639) = term(639) * (-4.0d+0) 
term(640) = term(640) * (2.0d+0) 
term(641) = term(641) * (4.0d+0) 
term(642) = term(642) * (-8.0d+0) 
term(643) = term(643) * (4.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
term(644) = term(644) + wm_interm_25_so_pt4(i,j) * wm_interm_46_so_pt4(q,p,j,i)
term(645) = term(645) + wm_interm_25_so_pt4(i,j) * wm_interm_48_so_pt4(q,j,p,i)
end do 
end do 


do j = 1, nocc 
do i = 1, nocc 
term(646) = term(646) + wm_interm_47_so_pt4(q,i,p,j) * wm_interm_8_so_pt4(i,j)
term(647) = term(647) + wm_interm_49_so_pt4(q,p,i,j) * wm_interm_8_so_pt4(i,j)
term(648) = term(648) + s1(q,j) * wm_interm_17_so_pt4(p,i) * wm_interm_4_so_pt4(i,j)
term(649) = term(649) + s1(q,j) * wm_interm_18_so_pt4(p,i) * wm_interm_4_so_pt4(i,j)
term(650) = term(650) + r1(vrdav_Rl, q,j) * wm_interm_17_so_pt4(i,j) * wm_interm_8_so_pt4(i,p)
term(651) = term(651) + r1(vrdav_Rl, q,j) * wm_interm_18_so_pt4(i,j) * wm_interm_8_so_pt4(i,p)
term(652) = term(652) + s1(q,j) * wm_interm_17_so_pt4(p,i) * wm_interm_76_so_pt4(i,j)
term(653) = term(653) + s1(q,j) * wm_interm_18_so_pt4(p,i) * wm_interm_76_so_pt4(i,j)
term(654) = term(654) + s1(q,j) * wm_interm_17_so_pt4(p,i) * wm_interm_78_so_pt4(i,j)
term(655) = term(655) + s1(q,j) * wm_interm_18_so_pt4(p,i) * wm_interm_78_so_pt4(i,j)
term(656) = term(656) + s1(q,j) * wm_interm_17_so_pt4(p,i) * wm_interm_80_so_pt4(i,j)
term(657) = term(657) + s1(q,j) * wm_interm_18_so_pt4(p,i) * wm_interm_80_so_pt4(i,j)
term(658) = term(658) + s1(q,j) * wm_interm_60_so_pt4(p,i) * wm_interm_85_so_pt4(j,i)
term(659) = term(659) + s1(q,j) * wm_interm_60_so_pt4(p,i) * wm_interm_86_so_pt4(j,i)
term(660) = term(660) + s1(q,j) * wm_interm_60_so_pt4(p,i) * wm_interm_87_so_pt4(j,i)
term(661) = term(661) + s1(q,j) * wm_interm_62_so_pt4(p,i) * wm_interm_85_so_pt4(j,i)
term(662) = term(662) + s1(q,j) * wm_interm_62_so_pt4(p,i) * wm_interm_86_so_pt4(j,i)
term(663) = term(663) + s1(q,j) * wm_interm_62_so_pt4(p,i) * wm_interm_87_so_pt4(j,i)
term(664) = term(664) + s1(q,j) * wm_interm_101_so_pt4(i,j) * wm_interm_17_so_pt4(p,i)
term(665) = term(665) + s1(q,j) * wm_interm_101_so_pt4(i,j) * wm_interm_18_so_pt4(p,i)
term(666) = term(666) + s1(q,j) * wm_interm_103_so_pt4(i,j) * wm_interm_17_so_pt4(p,i)
term(667) = term(667) + s1(q,j) * wm_interm_103_so_pt4(i,j) * wm_interm_18_so_pt4(p,i)
term(668) = term(668) + s1(q,j) * wm_interm_106_so_pt4(j,i) * wm_interm_60_so_pt4(p,i)
term(669) = term(669) + s1(q,j) * wm_interm_107_so_pt4(j,i) * wm_interm_60_so_pt4(p,i)
term(670) = term(670) + s1(q,j) * wm_interm_108_so_pt4(j,i) * wm_interm_60_so_pt4(p,i)
term(671) = term(671) + s1(q,j) * wm_interm_106_so_pt4(j,i) * wm_interm_62_so_pt4(p,i)
term(672) = term(672) + s1(q,j) * wm_interm_107_so_pt4(j,i) * wm_interm_62_so_pt4(p,i)
term(673) = term(673) + s1(q,j) * wm_interm_108_so_pt4(j,i) * wm_interm_62_so_pt4(p,i)
term(674) = term(674) + s1(q,j) * wm_interm_60_so_pt4(i,j) * wm_interm_85_so_pt4(i,p)
term(675) = term(675) + s1(q,j) * wm_interm_62_so_pt4(i,j) * wm_interm_85_so_pt4(i,p)
term(676) = term(676) + s1(q,j) * wm_interm_60_so_pt4(i,j) * wm_interm_86_so_pt4(i,p)
term(677) = term(677) + s1(q,j) * wm_interm_62_so_pt4(i,j) * wm_interm_86_so_pt4(i,p)
term(678) = term(678) + s1(q,j) * wm_interm_60_so_pt4(i,j) * wm_interm_87_so_pt4(i,p)
term(679) = term(679) + s1(q,j) * wm_interm_62_so_pt4(i,j) * wm_interm_87_so_pt4(i,p)
term(680) = term(680) + s1(q,j) * wm_interm_106_so_pt4(i,p) * wm_interm_60_so_pt4(i,j)
term(681) = term(681) + s1(q,j) * wm_interm_106_so_pt4(i,p) * wm_interm_62_so_pt4(i,j)
term(682) = term(682) + s1(q,j) * wm_interm_107_so_pt4(i,p) * wm_interm_60_so_pt4(i,j)
term(683) = term(683) + s1(q,j) * wm_interm_107_so_pt4(i,p) * wm_interm_62_so_pt4(i,j)
term(684) = term(684) + s1(q,j) * wm_interm_108_so_pt4(i,p) * wm_interm_60_so_pt4(i,j)
term(685) = term(685) + s1(q,j) * wm_interm_108_so_pt4(i,p) * wm_interm_62_so_pt4(i,j)
end do 
end do 

term(648) = term(648) * (2.0d+0) 
term(649) = term(649) * (-4.0d+0) 
term(650) = term(650) * (2.0d+0) 
term(651) = term(651) * (-4.0d+0) 
term(652) = term(652) * (-1.0d+0) 
term(653) = term(653) * (2.0d+0) 
term(654) = term(654) * (-1.0d+0) 
term(655) = term(655) * (2.0d+0) 
term(656) = term(656) * (2.0d+0) 
term(657) = term(657) * (-4.0d+0) 
term(658) = term(658) * (-1.0d+0) 
term(659) = term(659) * (2.0d+0) 
term(660) = term(660) * (-1.0d+0) 
term(661) = term(661) * (2.0d+0) 
term(662) = term(662) * (-4.0d+0) 
term(663) = term(663) * (2.0d+0) 
term(664) = term(664) * (-4.0d+0) 
term(665) = term(665) * (8.0d+0) 
term(666) = term(666) * (4.0d+0) 
term(667) = term(667) * (-8.0d+0) 
term(668) = term(668) * (-2.0d+0) 
term(669) = term(669) * (4.0d+0) 
term(670) = term(670) * (-2.0d+0) 
term(671) = term(671) * (4.0d+0) 
term(672) = term(672) * (-8.0d+0) 
term(673) = term(673) * (4.0d+0) 
term(674) = term(674) * (-1.0d+0) 
term(675) = term(675) * (2.0d+0) 
term(676) = term(676) * (2.0d+0) 
term(677) = term(677) * (-4.0d+0) 
term(678) = term(678) * (-1.0d+0) 
term(679) = term(679) * (2.0d+0) 
term(680) = term(680) * (-2.0d+0) 
term(681) = term(681) * (4.0d+0) 
term(682) = term(682) * (4.0d+0) 
term(683) = term(683) * (-8.0d+0) 
term(684) = term(684) * (-2.0d+0) 
term(685) = term(685) * (4.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(686) = term(686) + s2(a,q,j,i) * wm_interm_64_so_pt4(b,a,p,k) * wm_interm_89_so_pt4(b,k,j,i)
term(687) = term(687) + s2(a,q,j,i) * wm_interm_63_so_pt4(b,a,p,k) * wm_interm_89_so_pt4(b,k,j,i)
term(688) = term(688) + s2(a,q,j,i) * wm_interm_65_so_pt4(b,a,p,k) * wm_interm_89_so_pt4(b,k,j,i)
term(689) = term(689) + s2(a,q,j,i) * wm_interm_75_so_pt4(b,a,p,k) * wm_interm_89_so_pt4(b,k,j,i)
term(690) = term(690) + s2(a,q,p,i) * wm_interm_75_so_pt4(b,a,j,k) * wm_interm_89_so_pt4(b,k,j,i)
term(691) = term(691) + s2(a,q,j,i) * wm_interm_77_so_pt4(b,a,p,k) * wm_interm_89_so_pt4(b,k,j,i)
term(692) = term(692) + s2(a,q,p,i) * wm_interm_77_so_pt4(b,a,j,k) * wm_interm_89_so_pt4(b,k,j,i)
term(693) = term(693) + s2(a,q,j,i) * wm_interm_79_so_pt4(b,a,p,k) * wm_interm_89_so_pt4(b,k,j,i)
term(694) = term(694) + s2(a,q,p,i) * wm_interm_79_so_pt4(b,a,j,k) * wm_interm_89_so_pt4(b,k,j,i)
term(695) = term(695) + r2(vrdav_Rl, a,p,q,i) * wm_interm_1_so_pt4(b,a,j,k) * wm_interm_28_so_pt4(b,j,i,k)
term(696) = term(696) + r2(vrdav_Rl, a,p,q,i) * wm_interm_1_so_pt4(b,a,j,k) * wm_interm_28_so_pt4(b,i,j,k)
term(697) = term(697) + r2(vrdav_Rl, a,p,q,i) * wm_interm_1_so_pt4(b,a,j,k) * wm_interm_27_so_pt4(b,i,j,k)
term(698) = term(698) + r2(vrdav_Rl, a,j,q,i) * wm_interm_1_so_pt4(b,a,p,k) * wm_interm_28_so_pt4(b,i,j,k)
term(699) = term(699) + r2(vrdav_Rl, a,j,q,i) * wm_interm_1_so_pt4(b,a,p,k) * wm_interm_27_so_pt4(b,i,j,k)
term(700) = term(700) + r2(vrdav_Rl, a,j,q,i) * wm_interm_28_so_pt4(b,i,j,k) * wm_interm_5_so_pt4(b,a,p,k)
term(701) = term(701) + r2(vrdav_Rl, a,j,q,i) * wm_interm_27_so_pt4(b,i,j,k) * wm_interm_5_so_pt4(b,a,p,k)
term(702) = term(702) + r2(vrdav_Rl, a,p,q,i) * wm_interm_28_so_pt4(b,i,j,k) * wm_interm_5_so_pt4(b,a,j,k)
term(703) = term(703) + r2(vrdav_Rl, a,p,q,i) * wm_interm_27_so_pt4(b,j,i,k) * wm_interm_5_so_pt4(b,a,j,k)
term(704) = term(704) + r2(vrdav_Rl, a,p,q,i) * wm_interm_27_so_pt4(b,i,j,k) * wm_interm_5_so_pt4(b,a,j,k)
term(705) = term(705) + s2(a,q,j,i) * wm_interm_27_so_pt4(b,i,j,k) * wm_interm_54_so_pt4(b,a,p,k)
term(706) = term(706) + s2(a,q,j,i) * wm_interm_28_so_pt4(b,i,j,k) * wm_interm_54_so_pt4(b,a,p,k)
term(707) = term(707) + s2(a,q,p,i) * wm_interm_27_so_pt4(b,i,j,k) * wm_interm_54_so_pt4(b,a,j,k)
term(708) = term(708) + s2(a,q,p,i) * wm_interm_28_so_pt4(b,j,i,k) * wm_interm_54_so_pt4(b,a,j,k)
term(709) = term(709) + s2(a,q,p,i) * wm_interm_28_so_pt4(b,i,j,k) * wm_interm_54_so_pt4(b,a,j,k)
term(710) = term(710) + r2(vrdav_Rl, a,p,q,i) * wm_interm_10_so_pt4(b,a,j,k) * wm_interm_28_so_pt4(b,j,i,k)
term(711) = term(711) + r2(vrdav_Rl, a,p,q,i) * wm_interm_10_so_pt4(b,a,j,k) * wm_interm_28_so_pt4(b,i,j,k)
term(712) = term(712) + r2(vrdav_Rl, a,p,q,i) * wm_interm_11_so_pt4(b,a,j,k) * wm_interm_28_so_pt4(b,j,i,k)
term(713) = term(713) + r2(vrdav_Rl, a,p,q,i) * wm_interm_11_so_pt4(b,a,j,k) * wm_interm_28_so_pt4(b,i,j,k)
term(714) = term(714) + r2(vrdav_Rl, a,p,q,i) * wm_interm_10_so_pt4(b,a,j,k) * wm_interm_27_so_pt4(b,i,j,k)
term(715) = term(715) + r2(vrdav_Rl, a,p,q,i) * wm_interm_11_so_pt4(b,a,j,k) * wm_interm_27_so_pt4(b,i,j,k)
term(716) = term(716) + r2(vrdav_Rl, a,j,q,i) * wm_interm_10_so_pt4(b,a,p,k) * wm_interm_28_so_pt4(b,i,j,k)
term(717) = term(717) + r2(vrdav_Rl, a,j,q,i) * wm_interm_11_so_pt4(b,a,p,k) * wm_interm_28_so_pt4(b,i,j,k)
term(718) = term(718) + r2(vrdav_Rl, a,j,q,i) * wm_interm_10_so_pt4(b,a,p,k) * wm_interm_27_so_pt4(b,i,j,k)
term(719) = term(719) + r2(vrdav_Rl, a,j,q,i) * wm_interm_11_so_pt4(b,a,p,k) * wm_interm_27_so_pt4(b,i,j,k)
term(720) = term(720) + s2(a,q,p,i) * wm_interm_63_so_pt4(b,a,j,k) * wm_interm_89_so_pt4(b,k,j,i)
term(721) = term(721) + s2(a,q,p,i) * wm_interm_66_so_pt4(b,a,j,k) * wm_interm_89_so_pt4(b,k,j,i)
term(722) = term(722) + s2(a,q,p,i) * wm_interm_65_so_pt4(b,a,j,k) * wm_interm_89_so_pt4(b,k,j,i)
term(723) = term(723) + s2(a,q,j,i) * wm_interm_27_so_pt4(b,i,j,k) * wm_interm_55_so_pt4(b,a,p,k)
term(724) = term(724) + s2(a,q,p,i) * wm_interm_27_so_pt4(b,i,j,k) * wm_interm_55_so_pt4(b,a,j,k)
term(725) = term(725) + s2(a,q,j,i) * wm_interm_28_so_pt4(b,i,j,k) * wm_interm_55_so_pt4(b,a,p,k)
term(726) = term(726) + s2(a,q,p,i) * wm_interm_28_so_pt4(b,j,i,k) * wm_interm_55_so_pt4(b,a,j,k)
term(727) = term(727) + s2(a,q,p,i) * wm_interm_28_so_pt4(b,i,j,k) * wm_interm_55_so_pt4(b,a,j,k)
term(728) = term(728) + s2(a,q,j,i) * wm_interm_28_so_pt4(b,i,j,k) * wm_interm_56_so_pt4(b,a,p,k)
term(729) = term(729) + s2(a,q,j,i) * wm_interm_28_so_pt4(b,i,j,k) * wm_interm_59_so_pt4(b,a,p,k)
term(730) = term(730) + s2(a,q,p,i) * wm_interm_28_so_pt4(b,i,j,k) * wm_interm_56_so_pt4(b,a,j,k)
term(731) = term(731) + s2(a,q,p,i) * wm_interm_28_so_pt4(b,j,i,k) * wm_interm_59_so_pt4(b,a,j,k)
term(732) = term(732) + s2(a,q,p,i) * wm_interm_28_so_pt4(b,i,j,k) * wm_interm_59_so_pt4(b,a,j,k)
term(733) = term(733) + s2(a,q,j,i) * wm_interm_27_so_pt4(b,i,j,k) * wm_interm_59_so_pt4(b,a,p,k)
term(734) = term(734) + s2(a,q,j,i) * wm_interm_27_so_pt4(b,i,j,k) * wm_interm_56_so_pt4(b,a,p,k)
term(735) = term(735) + s2(a,q,p,i) * wm_interm_27_so_pt4(b,i,j,k) * wm_interm_59_so_pt4(b,a,j,k)
term(736) = term(736) + s2(a,q,p,i) * wm_interm_27_so_pt4(b,j,i,k) * wm_interm_56_so_pt4(b,a,j,k)
term(737) = term(737) + s2(a,q,p,i) * wm_interm_27_so_pt4(b,i,j,k) * wm_interm_56_so_pt4(b,a,j,k)
term(738) = term(738) + s2(a,q,j,i) * wm_interm_89_so_pt4(b,k,j,i) * wm_interm_94_so_pt4(b,a,p,k)
term(739) = term(739) + s2(a,q,j,i) * wm_interm_89_so_pt4(b,k,j,i) * wm_interm_93_so_pt4(b,a,p,k)
term(740) = term(740) + s2(a,q,j,i) * wm_interm_100_so_pt4(b,a,p,k) * wm_interm_89_so_pt4(b,k,j,i)
term(741) = term(741) + s2(a,q,p,i) * wm_interm_100_so_pt4(b,a,j,k) * wm_interm_89_so_pt4(b,k,j,i)
term(742) = term(742) + s2(a,q,j,i) * wm_interm_102_so_pt4(b,a,p,k) * wm_interm_89_so_pt4(b,k,j,i)
term(743) = term(743) + s2(a,q,p,i) * wm_interm_102_so_pt4(b,a,j,k) * wm_interm_89_so_pt4(b,k,j,i)
term(744) = term(744) + r2(vrdav_Rl, a,p,q,i) * wm_interm_1_so_pt4(b,a,j,k) * wm_interm_31_so_pt4(b,j,i,k)
term(745) = term(745) + r2(vrdav_Rl, a,p,q,i) * wm_interm_1_so_pt4(b,a,j,k) * wm_interm_31_so_pt4(b,i,j,k)
term(746) = term(746) + r2(vrdav_Rl, a,p,q,i) * wm_interm_1_so_pt4(b,a,j,k) * wm_interm_30_so_pt4(b,i,j,k)
term(747) = term(747) + r2(vrdav_Rl, a,j,q,i) * wm_interm_1_so_pt4(b,a,p,k) * wm_interm_31_so_pt4(b,i,j,k)
term(748) = term(748) + r2(vrdav_Rl, a,j,q,i) * wm_interm_1_so_pt4(b,a,p,k) * wm_interm_30_so_pt4(b,i,j,k)
term(749) = term(749) + r2(vrdav_Rl, a,j,q,i) * wm_interm_31_so_pt4(b,i,j,k) * wm_interm_5_so_pt4(b,a,p,k)
term(750) = term(750) + r2(vrdav_Rl, a,j,q,i) * wm_interm_30_so_pt4(b,i,j,k) * wm_interm_5_so_pt4(b,a,p,k)
term(751) = term(751) + r2(vrdav_Rl, a,p,q,i) * wm_interm_31_so_pt4(b,i,j,k) * wm_interm_5_so_pt4(b,a,j,k)
term(752) = term(752) + r2(vrdav_Rl, a,p,q,i) * wm_interm_30_so_pt4(b,j,i,k) * wm_interm_5_so_pt4(b,a,j,k)
term(753) = term(753) + r2(vrdav_Rl, a,p,q,i) * wm_interm_30_so_pt4(b,i,j,k) * wm_interm_5_so_pt4(b,a,j,k)
term(754) = term(754) + s2(a,q,j,i) * wm_interm_30_so_pt4(b,i,j,k) * wm_interm_54_so_pt4(b,a,p,k)
term(755) = term(755) + s2(a,q,p,i) * wm_interm_30_so_pt4(b,i,j,k) * wm_interm_54_so_pt4(b,a,j,k)
term(756) = term(756) + s2(a,q,p,i) * wm_interm_30_so_pt4(b,j,i,k) * wm_interm_54_so_pt4(b,a,j,k)
term(757) = term(757) + r2(vrdav_Rl, a,p,q,i) * wm_interm_10_so_pt4(b,a,j,k) * wm_interm_31_so_pt4(b,j,i,k)
term(758) = term(758) + r2(vrdav_Rl, a,p,q,i) * wm_interm_10_so_pt4(b,a,j,k) * wm_interm_31_so_pt4(b,i,j,k)
term(759) = term(759) + r2(vrdav_Rl, a,p,q,i) * wm_interm_11_so_pt4(b,a,j,k) * wm_interm_31_so_pt4(b,j,i,k)
term(760) = term(760) + r2(vrdav_Rl, a,p,q,i) * wm_interm_11_so_pt4(b,a,j,k) * wm_interm_31_so_pt4(b,i,j,k)
term(761) = term(761) + r2(vrdav_Rl, a,p,q,i) * wm_interm_10_so_pt4(b,a,j,k) * wm_interm_30_so_pt4(b,i,j,k)
term(762) = term(762) + r2(vrdav_Rl, a,p,q,i) * wm_interm_11_so_pt4(b,a,j,k) * wm_interm_30_so_pt4(b,i,j,k)
term(763) = term(763) + r2(vrdav_Rl, a,j,q,i) * wm_interm_10_so_pt4(b,a,p,k) * wm_interm_31_so_pt4(b,i,j,k)
term(764) = term(764) + r2(vrdav_Rl, a,j,q,i) * wm_interm_11_so_pt4(b,a,p,k) * wm_interm_31_so_pt4(b,i,j,k)
term(765) = term(765) + r2(vrdav_Rl, a,j,q,i) * wm_interm_10_so_pt4(b,a,p,k) * wm_interm_30_so_pt4(b,i,j,k)
term(766) = term(766) + r2(vrdav_Rl, a,j,q,i) * wm_interm_11_so_pt4(b,a,p,k) * wm_interm_30_so_pt4(b,i,j,k)
term(767) = term(767) + s2(a,q,j,i) * wm_interm_30_so_pt4(b,i,j,k) * wm_interm_55_so_pt4(b,a,p,k)
term(768) = term(768) + s2(a,q,p,i) * wm_interm_30_so_pt4(b,i,j,k) * wm_interm_55_so_pt4(b,a,j,k)
term(769) = term(769) + s2(a,q,p,i) * wm_interm_30_so_pt4(b,j,i,k) * wm_interm_55_so_pt4(b,a,j,k)
term(770) = term(770) + s2(a,q,j,i) * wm_interm_30_so_pt4(b,i,j,k) * wm_interm_56_so_pt4(b,a,p,k)
term(771) = term(771) + s2(a,q,p,i) * wm_interm_30_so_pt4(b,j,i,k) * wm_interm_56_so_pt4(b,a,j,k)
term(772) = term(772) + s2(a,q,p,i) * wm_interm_30_so_pt4(b,i,j,k) * wm_interm_59_so_pt4(b,a,j,k)
term(773) = term(773) + s2(a,q,p,i) * wm_interm_30_so_pt4(b,j,i,k) * wm_interm_59_so_pt4(b,a,j,k)
term(774) = term(774) + s2(a,q,j,i) * wm_interm_30_so_pt4(b,i,j,k) * wm_interm_59_so_pt4(b,a,p,k)
term(775) = term(775) + s2(a,q,p,i) * wm_interm_30_so_pt4(b,i,j,k) * wm_interm_56_so_pt4(b,a,j,k)
end do 
end do 
end do 
end do 
end do 

term(686) = term(686) * (2.0d+0) 
term(687) = term(687) * (-4.0d+0) 
term(688) = term(688) * (2.0d+0) 
term(689) = term(689) * (2.0d+0) 
term(690) = term(690) * (-4.0d+0) 
term(691) = term(691) * (2.0d+0) 
term(692) = term(692) * (-4.0d+0) 
term(693) = term(693) * (-4.0d+0) 
term(694) = term(694) * (8.0d+0) 
term(695) = term(695) * (2.0d+0) 
term(696) = term(696) * (-4.0d+0) 
term(697) = term(697) * (2.0d+0) 
term(698) = term(698) * (2.0d+0) 
term(699) = term(699) * (-4.0d+0) 
term(700) = term(700) * (-4.0d+0) 
term(701) = term(701) * (2.0d+0) 
term(702) = term(702) * (2.0d+0) 
term(703) = term(703) * (2.0d+0) 
term(704) = term(704) * (-4.0d+0) 
term(705) = term(705) * (8.0d+0) 
term(706) = term(706) * (-4.0d+0) 
term(707) = term(707) * (-4.0d+0) 
term(708) = term(708) * (-4.0d+0) 
term(709) = term(709) * (8.0d+0) 
term(710) = term(710) * (2.0d+0) 
term(711) = term(711) * (-4.0d+0) 
term(712) = term(712) * (-4.0d+0) 
term(713) = term(713) * (8.0d+0) 
term(714) = term(714) * (2.0d+0) 
term(715) = term(715) * (-4.0d+0) 
term(716) = term(716) * (2.0d+0) 
term(717) = term(717) * (-4.0d+0) 
term(718) = term(718) * (-4.0d+0) 
term(719) = term(719) * (8.0d+0) 
term(720) = term(720) * (2.0d+0) 
term(721) = term(721) * (2.0d+0) 
term(722) = term(722) * (-4.0d+0) 
term(723) = term(723) * (-4.0d+0) 
term(724) = term(724) * (2.0d+0) 
term(725) = term(725) * (2.0d+0) 
term(726) = term(726) * (2.0d+0) 
term(727) = term(727) * (-4.0d+0) 
term(728) = term(728) * (-4.0d+0) 
term(729) = term(729) * (2.0d+0) 
term(730) = term(730) * (2.0d+0) 
term(731) = term(731) * (2.0d+0) 
term(732) = term(732) * (-4.0d+0) 
term(733) = term(733) * (-4.0d+0) 
term(734) = term(734) * (2.0d+0) 
term(735) = term(735) * (2.0d+0) 
term(736) = term(736) * (2.0d+0) 
term(737) = term(737) * (-4.0d+0) 
term(738) = term(738) * (8.0d+0) 
term(739) = term(739) * (-8.0d+0) 
term(740) = term(740) * (8.0d+0) 
term(741) = term(741) * (-16.0d+0) 
term(742) = term(742) * (-8.0d+0) 
term(743) = term(743) * (16.0d+0) 
term(744) = term(744) * (4.0d+0) 
term(745) = term(745) * (-8.0d+0) 
term(746) = term(746) * (4.0d+0) 
term(747) = term(747) * (4.0d+0) 
term(748) = term(748) * (-8.0d+0) 
term(749) = term(749) * (-8.0d+0) 
term(750) = term(750) * (4.0d+0) 
term(751) = term(751) * (4.0d+0) 
term(752) = term(752) * (4.0d+0) 
term(753) = term(753) * (-8.0d+0) 
term(754) = term(754) * (16.0d+0) 
term(755) = term(755) * (-16.0d+0) 
term(756) = term(756) * (16.0d+0) 
term(757) = term(757) * (4.0d+0) 
term(758) = term(758) * (-8.0d+0) 
term(759) = term(759) * (-8.0d+0) 
term(760) = term(760) * (16.0d+0) 
term(761) = term(761) * (4.0d+0) 
term(762) = term(762) * (-8.0d+0) 
term(763) = term(763) * (4.0d+0) 
term(764) = term(764) * (-8.0d+0) 
term(765) = term(765) * (-8.0d+0) 
term(766) = term(766) * (16.0d+0) 
term(767) = term(767) * (-8.0d+0) 
term(768) = term(768) * (8.0d+0) 
term(769) = term(769) * (-8.0d+0) 
term(770) = term(770) * (8.0d+0) 
term(771) = term(771) * (8.0d+0) 
term(772) = term(772) * (8.0d+0) 
term(773) = term(773) * (-8.0d+0) 
term(774) = term(774) * (-8.0d+0) 
term(775) = term(775) * (-8.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(776) = term(776) + s2(a,q,j,i) * wm_interm_63_so_pt4(b,a,p,k) * wm_interm_89_so_pt4(b,k,i,j)
term(777) = term(777) + s2(a,q,j,i) * wm_interm_64_so_pt4(b,a,p,k) * wm_interm_89_so_pt4(b,k,i,j)
term(778) = term(778) + s2(a,q,j,i) * wm_interm_66_so_pt4(b,a,p,k) * wm_interm_89_so_pt4(b,k,i,j)
term(779) = term(779) + s2(a,q,p,i) * wm_interm_64_so_pt4(b,a,j,k) * wm_interm_89_so_pt4(b,k,i,j)
term(780) = term(780) + s2(a,q,p,i) * wm_interm_65_so_pt4(b,a,j,k) * wm_interm_89_so_pt4(b,k,i,j)
term(781) = term(781) + s2(a,q,p,i) * wm_interm_66_so_pt4(b,a,j,k) * wm_interm_89_so_pt4(b,k,i,j)
term(782) = term(782) + s2(a,q,p,i) * wm_interm_75_so_pt4(b,a,j,k) * wm_interm_89_so_pt4(b,k,i,j)
term(783) = term(783) + s2(a,q,p,i) * wm_interm_77_so_pt4(b,a,j,k) * wm_interm_89_so_pt4(b,k,i,j)
term(784) = term(784) + s2(a,q,p,i) * wm_interm_79_so_pt4(b,a,j,k) * wm_interm_89_so_pt4(b,k,i,j)
term(785) = term(785) + s2(a,q,j,i) * wm_interm_89_so_pt4(b,k,i,j) * wm_interm_93_so_pt4(b,a,p,k)
term(786) = term(786) + s2(a,q,j,i) * wm_interm_89_so_pt4(b,k,i,j) * wm_interm_94_so_pt4(b,a,p,k)
term(787) = term(787) + s2(a,q,p,i) * wm_interm_89_so_pt4(b,j,i,k) * wm_interm_94_so_pt4(b,a,k,j)
term(788) = term(788) + s2(a,q,p,i) * wm_interm_89_so_pt4(b,j,i,k) * wm_interm_93_so_pt4(b,a,k,j)
term(789) = term(789) + s2(a,q,p,i) * wm_interm_100_so_pt4(b,a,j,k) * wm_interm_89_so_pt4(b,k,i,j)
term(790) = term(790) + s2(a,q,p,i) * wm_interm_102_so_pt4(b,a,j,k) * wm_interm_89_so_pt4(b,k,i,j)
end do 
end do 
end do 
end do 
end do 

term(776) = term(776) * (2.0d+0) 
term(777) = term(777) * (-4.0d+0) 
term(778) = term(778) * (2.0d+0) 
term(779) = term(779) * (2.0d+0) 
term(780) = term(780) * (2.0d+0) 
term(781) = term(781) * (-4.0d+0) 
term(782) = term(782) * (2.0d+0) 
term(783) = term(783) * (2.0d+0) 
term(784) = term(784) * (-4.0d+0) 
term(785) = term(785) * (8.0d+0) 
term(786) = term(786) * (-8.0d+0) 
term(787) = term(787) * (8.0d+0) 
term(788) = term(788) * (-8.0d+0) 
term(789) = term(789) * (8.0d+0) 
term(790) = term(790) * (-8.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(791) = term(791) + wm_interm_32_so_pt4(a,q,p,i) * wm_interm_33_so_pt4(a,i)
term(792) = term(792) + wm_interm_32_so_pt4(a,q,p,i) * wm_interm_34_so_pt4(a,i)
term(793) = term(793) + wm_interm_32_so_pt4(a,q,p,i) * wm_interm_35_so_pt4(a,i)
term(794) = term(794) + wm_interm_32_so_pt4(q,a,i,p) * wm_interm_33_so_pt4(a,i)
term(795) = term(795) + wm_interm_32_so_pt4(q,a,i,p) * wm_interm_34_so_pt4(a,i)
term(796) = term(796) + wm_interm_32_so_pt4(q,a,i,p) * wm_interm_35_so_pt4(a,i)
term(797) = term(797) + wm_interm_32_so_pt4(a,q,p,i) * wm_interm_36_so_pt4(a,i)
term(798) = term(798) + wm_interm_32_so_pt4(a,q,p,i) * wm_interm_37_so_pt4(a,i)
term(799) = term(799) + wm_interm_32_so_pt4(q,a,i,p) * wm_interm_36_so_pt4(a,i)
term(800) = term(800) + wm_interm_32_so_pt4(q,a,i,p) * wm_interm_37_so_pt4(a,i)
term(801) = term(801) + wm_interm_38_so_pt4(a,i) * wm_interm_39_so_pt4(a,q,p,i)
term(802) = term(802) + wm_interm_39_so_pt4(a,q,p,i) * wm_interm_40_so_pt4(a,i)
term(803) = term(803) + wm_interm_39_so_pt4(a,q,p,i) * wm_interm_41_so_pt4(a,i)
term(804) = term(804) + wm_interm_39_so_pt4(a,q,p,i) * wm_interm_42_so_pt4(a,i)
term(805) = term(805) + wm_interm_39_so_pt4(a,q,p,i) * wm_interm_43_so_pt4(a,i)
term(806) = term(806) + r1(vrdav_Rl, q,i) * wm_interm_2_so_pt4(a,p) * wm_interm_6_so_pt4(a,i)
term(807) = term(807) + r1(vrdav_Rl, q,i) * wm_interm_2_so_pt4(a,p) * wm_interm_7_so_pt4(a,i)
term(808) = term(808) + r1(vrdav_Rl, a,p) * wm_interm_2_so_pt4(q,i) * wm_interm_7_so_pt4(a,i)
term(809) = term(809) + s1(a,p) * wm_interm_14_so_pt4(a,i) * wm_interm_2_so_pt4(q,i)
term(810) = term(810) + r1(vrdav_Rl, a,p) * wm_interm_2_so_pt4(q,i) * wm_interm_6_so_pt4(a,i)
term(811) = term(811) + s1(q,i) * wm_interm_14_so_pt4(a,i) * wm_interm_2_so_pt4(a,p)
term(812) = term(812) + s1(a,p) * wm_interm_16_so_pt4(a,i) * wm_interm_2_so_pt4(q,i)
term(813) = term(813) + s1(q,i) * wm_interm_16_so_pt4(a,i) * wm_interm_2_so_pt4(a,p)
term(814) = term(814) + s1(a,i) * wm_interm_60_so_pt4(p,i) * wm_interm_83_so_pt4(a,q)
term(815) = term(815) + s1(a,i) * wm_interm_62_so_pt4(p,i) * wm_interm_83_so_pt4(a,q)
term(816) = term(816) + s1(a,i) * wm_interm_60_so_pt4(p,i) * wm_interm_81_so_pt4(a,q)
term(817) = term(817) + s1(a,i) * wm_interm_62_so_pt4(p,i) * wm_interm_81_so_pt4(a,q)
term(818) = term(818) + s1(a,i) * wm_interm_60_so_pt4(p,i) * wm_interm_82_so_pt4(a,q)
term(819) = term(819) + s1(a,i) * wm_interm_62_so_pt4(p,i) * wm_interm_82_so_pt4(a,q)
term(820) = term(820) + s1(a,i) * wm_interm_112_so_pt4(a,q) * wm_interm_60_so_pt4(p,i)
term(821) = term(821) + s1(a,i) * wm_interm_112_so_pt4(a,q) * wm_interm_62_so_pt4(p,i)
term(822) = term(822) + s1(a,i) * wm_interm_104_so_pt4(a,q) * wm_interm_60_so_pt4(p,i)
term(823) = term(823) + s1(a,i) * wm_interm_104_so_pt4(a,q) * wm_interm_62_so_pt4(p,i)
term(824) = term(824) + s1(a,i) * wm_interm_105_so_pt4(a,q) * wm_interm_60_so_pt4(p,i)
term(825) = term(825) + s1(a,i) * wm_interm_105_so_pt4(a,q) * wm_interm_62_so_pt4(p,i)
end do 
end do 

term(791) = term(791) * (2.0d+0) 
term(792) = term(792) * (-1.0d+0) 
term(793) = term(793) * (-1.0d+0) 
term(794) = term(794) * (2.0d+0) 
term(795) = term(795) * (-1.0d+0) 
term(796) = term(796) * (-1.0d+0) 
term(797) = term(797) * (4.0d+0) 
term(798) = term(798) * (-4.0d+0) 
term(799) = term(799) * (4.0d+0) 
term(800) = term(800) * (-4.0d+0) 
term(801) = term(801) * (-2.0d+0) 
term(802) = term(802) * (4.0d+0) 
term(803) = term(803) * (-2.0d+0) 
term(804) = term(804) * (-8.0d+0) 
term(805) = term(805) * (8.0d+0) 
term(806) = term(806) * (8.0d+0) 
term(807) = term(807) * (-4.0d+0) 
term(808) = term(808) * (-4.0d+0) 
term(809) = term(809) * (8.0d+0) 
term(810) = term(810) * (8.0d+0) 
term(811) = term(811) * (8.0d+0) 
term(812) = term(812) * (-4.0d+0) 
term(813) = term(813) * (-4.0d+0) 
term(814) = term(814) * (-1.0d+0) 
term(815) = term(815) * (2.0d+0) 
term(816) = term(816) * (-1.0d+0) 
term(817) = term(817) * (2.0d+0) 
term(818) = term(818) * (2.0d+0) 
term(819) = term(819) * (-4.0d+0) 
term(820) = term(820) * (-2.0d+0) 
term(821) = term(821) * (4.0d+0) 
term(822) = term(822) * (-2.0d+0) 
term(823) = term(823) * (4.0d+0) 
term(824) = term(824) * (4.0d+0) 
term(825) = term(825) * (-8.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(826) = term(826) + r2(vrdav_Rl, a,j,q,i) * wm_interm_7_so_pt4(a,k) * wm_interm_88_so_pt4(i,j,k,p)
term(827) = term(827) + r2(vrdav_Rl, a,j,q,i) * wm_interm_6_so_pt4(a,k) * wm_interm_88_so_pt4(i,j,k,p)
term(828) = term(828) + r2(vrdav_Rl, a,j,q,i) * wm_interm_109_so_pt4(i,j,k,p) * wm_interm_7_so_pt4(a,k)
term(829) = term(829) + r2(vrdav_Rl, a,j,q,i) * wm_interm_109_so_pt4(i,j,k,p) * wm_interm_6_so_pt4(a,k)
end do 
end do 
end do 
end do 

term(826) = term(826) * (2.0d+0) 
term(827) = term(827) * (-4.0d+0) 
term(828) = term(828) * (4.0d+0) 
term(829) = term(829) * (-8.0d+0) 

do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(830) = term(830) + r1(vrdav_Rl, a,p) * wm_interm_0_so_pt4(b,q) * wm_interm_19_so_pt4(b,a)
term(831) = term(831) + r1(vrdav_Rl, a,p) * wm_interm_0_so_pt4(b,q) * wm_interm_20_so_pt4(b,a)
term(832) = term(832) + s1(a,p) * wm_interm_57_so_pt4(b,a) * wm_interm_83_so_pt4(b,q)
term(833) = term(833) + s1(a,p) * wm_interm_58_so_pt4(b,a) * wm_interm_83_so_pt4(b,q)
term(834) = term(834) + s1(a,p) * wm_interm_57_so_pt4(b,a) * wm_interm_81_so_pt4(b,q)
term(835) = term(835) + s1(a,p) * wm_interm_58_so_pt4(b,a) * wm_interm_81_so_pt4(b,q)
term(836) = term(836) + s1(a,p) * wm_interm_57_so_pt4(b,a) * wm_interm_82_so_pt4(b,q)
term(837) = term(837) + s1(a,p) * wm_interm_58_so_pt4(b,a) * wm_interm_82_so_pt4(b,q)
term(838) = term(838) + s1(a,p) * wm_interm_104_so_pt4(b,q) * wm_interm_57_so_pt4(b,a)
term(839) = term(839) + s1(a,p) * wm_interm_104_so_pt4(b,q) * wm_interm_58_so_pt4(b,a)
term(840) = term(840) + s1(a,p) * wm_interm_105_so_pt4(b,q) * wm_interm_57_so_pt4(b,a)
term(841) = term(841) + s1(a,p) * wm_interm_105_so_pt4(b,q) * wm_interm_58_so_pt4(b,a)
end do 
end do 

term(830) = term(830) * (2.0d+0) 
term(831) = term(831) * (-4.0d+0) 
term(832) = term(832) * (2.0d+0) 
term(833) = term(833) * (-1.0d+0) 
term(834) = term(834) * (2.0d+0) 
term(835) = term(835) * (-1.0d+0) 
term(836) = term(836) * (-4.0d+0) 
term(837) = term(837) * (2.0d+0) 
term(838) = term(838) * (8.0d+0) 
term(839) = term(839) * (-4.0d+0) 
term(840) = term(840) * (-8.0d+0) 
term(841) = term(841) * (4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(842) = term(842) + s1(a,i) * wm_interm_54_so_pt4(q,a,j,k) * wm_interm_88_so_pt4(j,i,k,p)
term(843) = term(843) + s1(a,i) * wm_interm_54_so_pt4(q,a,j,k) * wm_interm_88_so_pt4(i,j,p,k)
term(844) = term(844) + s1(a,i) * wm_interm_54_so_pt4(q,a,j,k) * wm_interm_88_so_pt4(j,i,p,k)
term(845) = term(845) + s1(a,i) * wm_interm_55_so_pt4(q,a,j,k) * wm_interm_88_so_pt4(j,i,k,p)
term(846) = term(846) + s1(a,i) * wm_interm_55_so_pt4(q,a,j,k) * wm_interm_88_so_pt4(i,j,p,k)
term(847) = term(847) + s1(a,i) * wm_interm_55_so_pt4(q,a,j,k) * wm_interm_88_so_pt4(j,i,p,k)
term(848) = term(848) + s1(a,i) * wm_interm_59_so_pt4(q,a,j,k) * wm_interm_88_so_pt4(j,i,k,p)
term(849) = term(849) + s1(a,i) * wm_interm_56_so_pt4(q,a,j,k) * wm_interm_88_so_pt4(i,j,k,p)
term(850) = term(850) + s1(a,i) * wm_interm_59_so_pt4(q,a,j,k) * wm_interm_88_so_pt4(i,j,p,k)
term(851) = term(851) + s1(a,i) * wm_interm_59_so_pt4(q,a,j,k) * wm_interm_88_so_pt4(j,i,p,k)
term(852) = term(852) + s1(a,i) * wm_interm_56_so_pt4(q,a,j,k) * wm_interm_88_so_pt4(j,i,p,k)
term(853) = term(853) + s1(a,i) * wm_interm_56_so_pt4(q,a,j,k) * wm_interm_88_so_pt4(i,j,p,k)
term(854) = term(854) + s1(a,i) * wm_interm_109_so_pt4(j,i,k,p) * wm_interm_54_so_pt4(q,a,j,k)
term(855) = term(855) + s1(a,i) * wm_interm_109_so_pt4(i,j,p,k) * wm_interm_54_so_pt4(q,a,j,k)
term(856) = term(856) + s1(a,i) * wm_interm_109_so_pt4(j,i,p,k) * wm_interm_54_so_pt4(q,a,j,k)
term(857) = term(857) + s1(a,i) * wm_interm_109_so_pt4(j,i,k,p) * wm_interm_55_so_pt4(q,a,j,k)
term(858) = term(858) + s1(a,i) * wm_interm_109_so_pt4(i,j,p,k) * wm_interm_55_so_pt4(q,a,j,k)
term(859) = term(859) + s1(a,i) * wm_interm_109_so_pt4(j,i,p,k) * wm_interm_55_so_pt4(q,a,j,k)
term(860) = term(860) + s1(a,i) * wm_interm_109_so_pt4(j,i,k,p) * wm_interm_59_so_pt4(q,a,j,k)
term(861) = term(861) + s1(a,i) * wm_interm_109_so_pt4(i,j,k,p) * wm_interm_56_so_pt4(q,a,j,k)
term(862) = term(862) + s1(a,i) * wm_interm_109_so_pt4(i,j,p,k) * wm_interm_59_so_pt4(q,a,j,k)
term(863) = term(863) + s1(a,i) * wm_interm_109_so_pt4(j,i,p,k) * wm_interm_59_so_pt4(q,a,j,k)
term(864) = term(864) + s1(a,i) * wm_interm_109_so_pt4(j,i,p,k) * wm_interm_56_so_pt4(q,a,j,k)
term(865) = term(865) + s1(a,i) * wm_interm_109_so_pt4(i,j,p,k) * wm_interm_56_so_pt4(q,a,j,k)
end do 
end do 
end do 
end do 

term(842) = term(842) * (-4.0d+0) 
term(843) = term(843) * (-4.0d+0) 
term(844) = term(844) * (8.0d+0) 
term(845) = term(845) * (2.0d+0) 
term(846) = term(846) * (2.0d+0) 
term(847) = term(847) * (-4.0d+0) 
term(848) = term(848) * (2.0d+0) 
term(849) = term(849) * (2.0d+0) 
term(850) = term(850) * (2.0d+0) 
term(851) = term(851) * (-4.0d+0) 
term(852) = term(852) * (2.0d+0) 
term(853) = term(853) * (-4.0d+0) 
term(854) = term(854) * (-8.0d+0) 
term(855) = term(855) * (-8.0d+0) 
term(856) = term(856) * (16.0d+0) 
term(857) = term(857) * (4.0d+0) 
term(858) = term(858) * (4.0d+0) 
term(859) = term(859) * (-8.0d+0) 
term(860) = term(860) * (4.0d+0) 
term(861) = term(861) * (4.0d+0) 
term(862) = term(862) * (4.0d+0) 
term(863) = term(863) * (-8.0d+0) 
term(864) = term(864) * (4.0d+0) 
term(865) = term(865) * (-8.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
do j = 1, nocc 
do b = nocc + 1, nactive 
term(866) = term(866) + r2(vrdav_Rl, a,j,q,i) * wm_interm_1_so_pt4(b,a,p,k) * wm_interm_27_so_pt4(b,j,i,k)
term(867) = term(867) + r2(vrdav_Rl, a,j,q,i) * wm_interm_28_so_pt4(b,j,i,k) * wm_interm_5_so_pt4(b,a,p,k)
term(868) = term(868) + s2(a,q,j,i) * wm_interm_27_so_pt4(b,j,i,k) * wm_interm_54_so_pt4(b,a,p,k)
term(869) = term(869) + r2(vrdav_Rl, a,j,q,i) * wm_interm_10_so_pt4(b,a,p,k) * wm_interm_27_so_pt4(b,j,i,k)
term(870) = term(870) + r2(vrdav_Rl, a,j,q,i) * wm_interm_11_so_pt4(b,a,p,k) * wm_interm_27_so_pt4(b,j,i,k)
term(871) = term(871) + s2(a,q,j,i) * wm_interm_27_so_pt4(b,j,i,k) * wm_interm_55_so_pt4(b,a,p,k)
term(872) = term(872) + s2(a,q,j,i) * wm_interm_28_so_pt4(b,j,i,k) * wm_interm_56_so_pt4(b,a,p,k)
term(873) = term(873) + s2(a,q,j,i) * wm_interm_27_so_pt4(b,j,i,k) * wm_interm_59_so_pt4(b,a,p,k)
term(874) = term(874) + r2(vrdav_Rl, a,j,q,i) * wm_interm_1_so_pt4(b,a,p,k) * wm_interm_30_so_pt4(b,j,i,k)
term(875) = term(875) + r2(vrdav_Rl, a,j,q,i) * wm_interm_31_so_pt4(b,j,i,k) * wm_interm_5_so_pt4(b,a,p,k)
term(876) = term(876) + s2(a,q,j,i) * wm_interm_30_so_pt4(b,j,i,k) * wm_interm_54_so_pt4(b,a,p,k)
term(877) = term(877) + r2(vrdav_Rl, a,j,q,i) * wm_interm_10_so_pt4(b,a,p,k) * wm_interm_30_so_pt4(b,j,i,k)
term(878) = term(878) + r2(vrdav_Rl, a,j,q,i) * wm_interm_11_so_pt4(b,a,p,k) * wm_interm_30_so_pt4(b,j,i,k)
term(879) = term(879) + s2(a,q,j,i) * wm_interm_30_so_pt4(b,j,i,k) * wm_interm_55_so_pt4(b,a,p,k)
term(880) = term(880) + s2(a,q,j,i) * wm_interm_30_so_pt4(b,j,i,k) * wm_interm_56_so_pt4(b,a,p,k)
term(881) = term(881) + s2(a,q,j,i) * wm_interm_30_so_pt4(b,j,i,k) * wm_interm_59_so_pt4(b,a,p,k)
end do 
end do 
end do 
end do 
end do 

term(866) = term(866) * (2.0d+0) 
term(867) = term(867) * (2.0d+0) 
term(868) = term(868) * (-4.0d+0) 
term(869) = term(869) * (2.0d+0) 
term(870) = term(870) * (-4.0d+0) 
term(871) = term(871) * (2.0d+0) 
term(872) = term(872) * (2.0d+0) 
term(873) = term(873) * (2.0d+0) 
term(874) = term(874) * (4.0d+0) 
term(875) = term(875) * (4.0d+0) 
term(876) = term(876) * (-16.0d+0) 
term(877) = term(877) * (4.0d+0) 
term(878) = term(878) * (-8.0d+0) 
term(879) = term(879) * (8.0d+0) 
term(880) = term(880) * (-8.0d+0) 
term(881) = term(881) * (8.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(882) = term(882) + s2(a,q,p,i) * wm_interm_89_so_pt4(b,j,k,i) * wm_interm_93_so_pt4(b,a,k,j)
term(883) = term(883) + s2(a,q,p,i) * wm_interm_89_so_pt4(b,j,k,i) * wm_interm_94_so_pt4(b,a,k,j)
end do 
end do 
end do 
end do 
end do 

term(882) = term(882) * (8.0d+0) 
term(883) = term(883) * (-8.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(884) = term(884) + r1(vrdav_Rl, q,i) * wm_interm_0_so_pt4(a,b) * wm_interm_1_so_pt4(a,b,p,i)
term(885) = term(885) + r1(vrdav_Rl, q,i) * wm_interm_0_so_pt4(a,b) * wm_interm_10_so_pt4(a,b,p,i)
term(886) = term(886) + r1(vrdav_Rl, q,i) * wm_interm_0_so_pt4(a,b) * wm_interm_11_so_pt4(a,b,p,i)
term(887) = term(887) + s1(q,i) * wm_interm_15_so_pt4(a,b) * wm_interm_1_so_pt4(b,a,p,i)
term(888) = term(888) + s1(q,i) * wm_interm_10_so_pt4(a,b,p,i) * wm_interm_15_so_pt4(b,a)
term(889) = term(889) + s1(q,i) * wm_interm_11_so_pt4(a,b,p,i) * wm_interm_15_so_pt4(b,a)
term(890) = term(890) + r2(vrdav_Rl, a,p,b,i) * wm_interm_19_so_pt4(q,b) * wm_interm_33_so_pt4(a,i)
term(891) = term(891) + r2(vrdav_Rl, a,p,b,i) * wm_interm_20_so_pt4(q,b) * wm_interm_33_so_pt4(a,i)
term(892) = term(892) + r2(vrdav_Rl, a,p,b,i) * wm_interm_19_so_pt4(q,b) * wm_interm_34_so_pt4(a,i)
term(893) = term(893) + r2(vrdav_Rl, a,p,b,i) * wm_interm_20_so_pt4(q,b) * wm_interm_34_so_pt4(a,i)
term(894) = term(894) + r2(vrdav_Rl, a,p,b,i) * wm_interm_19_so_pt4(q,b) * wm_interm_35_so_pt4(a,i)
term(895) = term(895) + r2(vrdav_Rl, a,p,b,i) * wm_interm_20_so_pt4(q,b) * wm_interm_35_so_pt4(a,i)
term(896) = term(896) + r2(vrdav_Rl, a,p,b,i) * wm_interm_19_so_pt4(q,b) * wm_interm_36_so_pt4(a,i)
term(897) = term(897) + r2(vrdav_Rl, a,p,b,i) * wm_interm_20_so_pt4(q,b) * wm_interm_36_so_pt4(a,i)
term(898) = term(898) + r2(vrdav_Rl, a,p,b,i) * wm_interm_19_so_pt4(q,b) * wm_interm_37_so_pt4(a,i)
term(899) = term(899) + r2(vrdav_Rl, a,p,b,i) * wm_interm_20_so_pt4(q,b) * wm_interm_37_so_pt4(a,i)
term(900) = term(900) + r2(vrdav_Rl, a,p,q,i) * wm_interm_6_so_pt4(b,i) * wm_interm_81_so_pt4(a,b)
term(901) = term(901) + r2(vrdav_Rl, a,p,q,i) * wm_interm_6_so_pt4(b,i) * wm_interm_82_so_pt4(a,b)
term(902) = term(902) + r2(vrdav_Rl, a,p,q,i) * wm_interm_6_so_pt4(b,i) * wm_interm_83_so_pt4(a,b)
term(903) = term(903) + r2(vrdav_Rl, a,p,q,i) * wm_interm_7_so_pt4(b,i) * wm_interm_81_so_pt4(a,b)
term(904) = term(904) + r2(vrdav_Rl, a,p,q,i) * wm_interm_7_so_pt4(b,i) * wm_interm_82_so_pt4(a,b)
term(905) = term(905) + r2(vrdav_Rl, a,p,q,i) * wm_interm_7_so_pt4(b,i) * wm_interm_83_so_pt4(a,b)
term(906) = term(906) + s1(q,i) * wm_interm_54_so_pt4(a,b,p,i) * wm_interm_81_so_pt4(a,b)
term(907) = term(907) + s1(q,i) * wm_interm_54_so_pt4(a,b,p,i) * wm_interm_82_so_pt4(a,b)
term(908) = term(908) + s1(q,i) * wm_interm_54_so_pt4(a,b,p,i) * wm_interm_83_so_pt4(a,b)
term(909) = term(909) + s1(q,i) * wm_interm_1_so_pt4(a,b,p,i) * wm_interm_90_so_pt4(b,a)
term(910) = term(910) + s1(a,i) * wm_interm_5_so_pt4(q,b,p,i) * wm_interm_90_so_pt4(b,a)
term(911) = term(911) + s1(q,i) * wm_interm_1_so_pt4(a,b,p,i) * wm_interm_91_so_pt4(b,a)
term(912) = term(912) + s1(a,i) * wm_interm_5_so_pt4(q,b,p,i) * wm_interm_91_so_pt4(b,a)
term(913) = term(913) + s1(q,i) * wm_interm_1_so_pt4(a,b,p,i) * wm_interm_92_so_pt4(b,a)
term(914) = term(914) + s1(a,i) * wm_interm_5_so_pt4(q,b,p,i) * wm_interm_92_so_pt4(b,a)
term(915) = term(915) + s1(q,i) * wm_interm_10_so_pt4(a,b,p,i) * wm_interm_90_so_pt4(b,a)
term(916) = term(916) + s1(q,i) * wm_interm_11_so_pt4(a,b,p,i) * wm_interm_90_so_pt4(b,a)
term(917) = term(917) + s1(q,i) * wm_interm_10_so_pt4(a,b,p,i) * wm_interm_91_so_pt4(b,a)
term(918) = term(918) + s1(q,i) * wm_interm_11_so_pt4(a,b,p,i) * wm_interm_91_so_pt4(b,a)
term(919) = term(919) + s1(q,i) * wm_interm_10_so_pt4(a,b,p,i) * wm_interm_92_so_pt4(b,a)
term(920) = term(920) + s1(q,i) * wm_interm_11_so_pt4(a,b,p,i) * wm_interm_92_so_pt4(b,a)
term(921) = term(921) + s1(q,i) * wm_interm_55_so_pt4(a,b,p,i) * wm_interm_81_so_pt4(a,b)
term(922) = term(922) + s1(q,i) * wm_interm_55_so_pt4(a,b,p,i) * wm_interm_82_so_pt4(a,b)
term(923) = term(923) + s1(q,i) * wm_interm_55_so_pt4(a,b,p,i) * wm_interm_83_so_pt4(a,b)
term(924) = term(924) + s1(q,i) * wm_interm_59_so_pt4(a,b,p,i) * wm_interm_81_so_pt4(a,b)
term(925) = term(925) + s1(q,i) * wm_interm_59_so_pt4(a,b,p,i) * wm_interm_82_so_pt4(a,b)
term(926) = term(926) + s1(q,i) * wm_interm_59_so_pt4(a,b,p,i) * wm_interm_83_so_pt4(a,b)
term(927) = term(927) + s1(a,i) * wm_interm_56_so_pt4(q,b,p,i) * wm_interm_81_so_pt4(a,b)
term(928) = term(928) + s1(a,i) * wm_interm_56_so_pt4(q,b,p,i) * wm_interm_82_so_pt4(a,b)
term(929) = term(929) + s1(a,i) * wm_interm_56_so_pt4(q,b,p,i) * wm_interm_83_so_pt4(a,b)
term(930) = term(930) + r2(vrdav_Rl, a,p,q,i) * wm_interm_104_so_pt4(a,b) * wm_interm_6_so_pt4(b,i)
term(931) = term(931) + r2(vrdav_Rl, a,p,q,i) * wm_interm_105_so_pt4(a,b) * wm_interm_6_so_pt4(b,i)
term(932) = term(932) + r2(vrdav_Rl, a,p,q,i) * wm_interm_104_so_pt4(a,b) * wm_interm_7_so_pt4(b,i)
term(933) = term(933) + r2(vrdav_Rl, a,p,q,i) * wm_interm_105_so_pt4(a,b) * wm_interm_7_so_pt4(b,i)
term(934) = term(934) + s1(q,i) * wm_interm_104_so_pt4(a,b) * wm_interm_54_so_pt4(a,b,p,i)
term(935) = term(935) + s1(q,i) * wm_interm_105_so_pt4(a,b) * wm_interm_54_so_pt4(a,b,p,i)
term(936) = term(936) + s1(q,i) * wm_interm_110_so_pt4(a,b) * wm_interm_1_so_pt4(b,a,p,i)
term(937) = term(937) + s1(a,i) * wm_interm_110_so_pt4(b,a) * wm_interm_5_so_pt4(q,b,p,i)
term(938) = term(938) + s1(q,i) * wm_interm_111_so_pt4(a,b) * wm_interm_1_so_pt4(b,a,p,i)
term(939) = term(939) + s1(a,i) * wm_interm_111_so_pt4(b,a) * wm_interm_5_so_pt4(q,b,p,i)
term(940) = term(940) + s1(q,i) * wm_interm_10_so_pt4(a,b,p,i) * wm_interm_110_so_pt4(b,a)
term(941) = term(941) + s1(q,i) * wm_interm_110_so_pt4(a,b) * wm_interm_11_so_pt4(b,a,p,i)
term(942) = term(942) + s1(q,i) * wm_interm_10_so_pt4(a,b,p,i) * wm_interm_111_so_pt4(b,a)
term(943) = term(943) + s1(q,i) * wm_interm_111_so_pt4(a,b) * wm_interm_11_so_pt4(b,a,p,i)
term(944) = term(944) + s1(q,i) * wm_interm_104_so_pt4(a,b) * wm_interm_55_so_pt4(a,b,p,i)
term(945) = term(945) + s1(q,i) * wm_interm_105_so_pt4(a,b) * wm_interm_55_so_pt4(a,b,p,i)
term(946) = term(946) + s1(q,i) * wm_interm_104_so_pt4(a,b) * wm_interm_59_so_pt4(a,b,p,i)
term(947) = term(947) + s1(q,i) * wm_interm_105_so_pt4(a,b) * wm_interm_59_so_pt4(a,b,p,i)
term(948) = term(948) + s1(a,i) * wm_interm_104_so_pt4(a,b) * wm_interm_56_so_pt4(q,b,p,i)
term(949) = term(949) + s1(a,i) * wm_interm_105_so_pt4(a,b) * wm_interm_56_so_pt4(q,b,p,i)
term(950) = term(950) + s1(a,i) * wm_interm_112_so_pt4(a,b) * wm_interm_56_so_pt4(q,b,p,i)
term(951) = term(951) + s1(q,i) * wm_interm_19_so_pt4(a,b) * wm_interm_64_so_pt4(b,a,p,i)
term(952) = term(952) + s1(q,i) * wm_interm_19_so_pt4(a,b) * wm_interm_63_so_pt4(b,a,p,i)
term(953) = term(953) + s1(q,i) * wm_interm_20_so_pt4(a,b) * wm_interm_64_so_pt4(b,a,p,i)
term(954) = term(954) + s1(q,i) * wm_interm_20_so_pt4(a,b) * wm_interm_63_so_pt4(b,a,p,i)
term(955) = term(955) + s1(q,i) * wm_interm_19_so_pt4(a,b) * wm_interm_65_so_pt4(b,a,p,i)
term(956) = term(956) + s1(q,i) * wm_interm_20_so_pt4(a,b) * wm_interm_65_so_pt4(b,a,p,i)
term(957) = term(957) + s1(q,i) * wm_interm_19_so_pt4(a,b) * wm_interm_75_so_pt4(b,a,p,i)
term(958) = term(958) + s1(q,i) * wm_interm_20_so_pt4(a,b) * wm_interm_75_so_pt4(b,a,p,i)
term(959) = term(959) + s1(q,i) * wm_interm_19_so_pt4(a,b) * wm_interm_77_so_pt4(b,a,p,i)
term(960) = term(960) + s1(q,i) * wm_interm_20_so_pt4(a,b) * wm_interm_77_so_pt4(b,a,p,i)
term(961) = term(961) + s1(q,i) * wm_interm_19_so_pt4(a,b) * wm_interm_79_so_pt4(b,a,p,i)
term(962) = term(962) + s1(q,i) * wm_interm_20_so_pt4(a,b) * wm_interm_79_so_pt4(b,a,p,i)
term(963) = term(963) + s1(q,i) * wm_interm_57_so_pt4(a,b) * wm_interm_67_so_pt4(a,b,i,p)
term(964) = term(964) + s1(q,i) * wm_interm_57_so_pt4(a,b) * wm_interm_68_so_pt4(a,b,i,p)
term(965) = term(965) + s1(q,i) * wm_interm_57_so_pt4(a,b) * wm_interm_74_so_pt4(a,b,i,p)
term(966) = term(966) + s1(q,i) * wm_interm_57_so_pt4(a,b) * wm_interm_69_so_pt4(a,b,i,p)
term(967) = term(967) + s1(q,i) * wm_interm_57_so_pt4(a,b) * wm_interm_73_so_pt4(a,b,i,p)
term(968) = term(968) + s1(q,i) * wm_interm_57_so_pt4(a,b) * wm_interm_84_so_pt4(a,b,i,p)
term(969) = term(969) + s1(q,i) * wm_interm_58_so_pt4(a,b) * wm_interm_67_so_pt4(a,b,i,p)
term(970) = term(970) + s1(q,i) * wm_interm_58_so_pt4(a,b) * wm_interm_68_so_pt4(a,b,i,p)
term(971) = term(971) + s1(q,i) * wm_interm_58_so_pt4(a,b) * wm_interm_69_so_pt4(a,b,i,p)
term(972) = term(972) + s1(q,i) * wm_interm_58_so_pt4(a,b) * wm_interm_74_so_pt4(a,b,i,p)
term(973) = term(973) + s1(q,i) * wm_interm_58_so_pt4(a,b) * wm_interm_73_so_pt4(a,b,i,p)
term(974) = term(974) + s1(q,i) * wm_interm_58_so_pt4(a,b) * wm_interm_84_so_pt4(a,b,i,p)
term(975) = term(975) + s1(q,i) * wm_interm_19_so_pt4(a,b) * wm_interm_94_so_pt4(b,a,p,i)
term(976) = term(976) + s1(q,i) * wm_interm_19_so_pt4(a,b) * wm_interm_93_so_pt4(b,a,p,i)
term(977) = term(977) + s1(q,i) * wm_interm_20_so_pt4(a,b) * wm_interm_94_so_pt4(b,a,p,i)
term(978) = term(978) + s1(q,i) * wm_interm_20_so_pt4(a,b) * wm_interm_93_so_pt4(b,a,p,i)
term(979) = term(979) + s1(q,i) * wm_interm_100_so_pt4(a,b,p,i) * wm_interm_19_so_pt4(b,a)
term(980) = term(980) + s1(q,i) * wm_interm_100_so_pt4(a,b,p,i) * wm_interm_20_so_pt4(b,a)
term(981) = term(981) + s1(q,i) * wm_interm_102_so_pt4(a,b,p,i) * wm_interm_19_so_pt4(b,a)
term(982) = term(982) + s1(q,i) * wm_interm_102_so_pt4(a,b,p,i) * wm_interm_20_so_pt4(b,a)
term(983) = term(983) + s1(q,i) * wm_interm_57_so_pt4(a,b) * wm_interm_95_so_pt4(a,b,i,p)
term(984) = term(984) + s1(q,i) * wm_interm_57_so_pt4(a,b) * wm_interm_96_so_pt4(a,b,i,p)
term(985) = term(985) + s1(q,i) * wm_interm_57_so_pt4(a,b) * wm_interm_97_so_pt4(a,b,i,p)
term(986) = term(986) + s1(q,i) * wm_interm_57_so_pt4(a,b) * wm_interm_98_so_pt4(a,b,i,p)
term(987) = term(987) + s1(q,i) * wm_interm_58_so_pt4(a,b) * wm_interm_95_so_pt4(a,b,i,p)
term(988) = term(988) + s1(q,i) * wm_interm_58_so_pt4(a,b) * wm_interm_96_so_pt4(a,b,i,p)
term(989) = term(989) + s1(q,i) * wm_interm_58_so_pt4(a,b) * wm_interm_97_so_pt4(a,b,i,p)
term(990) = term(990) + s1(q,i) * wm_interm_58_so_pt4(a,b) * wm_interm_98_so_pt4(a,b,i,p)
term(991) = term(991) + r2(vrdav_Rl, a,p,b,i) * wm_interm_6_so_pt4(b,i) * wm_interm_83_so_pt4(a,q)
term(992) = term(992) + r2(vrdav_Rl, a,p,b,i) * wm_interm_6_so_pt4(b,i) * wm_interm_81_so_pt4(a,q)
term(993) = term(993) + r2(vrdav_Rl, a,p,b,i) * wm_interm_6_so_pt4(b,i) * wm_interm_82_so_pt4(a,q)
term(994) = term(994) + r2(vrdav_Rl, a,p,b,i) * wm_interm_6_so_pt4(a,i) * wm_interm_83_so_pt4(b,q)
term(995) = term(995) + r2(vrdav_Rl, a,p,b,i) * wm_interm_6_so_pt4(a,i) * wm_interm_81_so_pt4(b,q)
term(996) = term(996) + r2(vrdav_Rl, a,p,b,i) * wm_interm_6_so_pt4(a,i) * wm_interm_82_so_pt4(b,q)
term(997) = term(997) + r2(vrdav_Rl, a,p,b,i) * wm_interm_7_so_pt4(a,i) * wm_interm_83_so_pt4(b,q)
term(998) = term(998) + r2(vrdav_Rl, a,p,b,i) * wm_interm_7_so_pt4(b,i) * wm_interm_83_so_pt4(a,q)
term(999) = term(999) + r2(vrdav_Rl, a,p,b,i) * wm_interm_7_so_pt4(a,i) * wm_interm_81_so_pt4(b,q)
term(1000) = term(1000) + r2(vrdav_Rl, a,p,b,i) * wm_interm_7_so_pt4(b,i) * wm_interm_81_so_pt4(a,q)
term(1001) = term(1001) + r2(vrdav_Rl, a,p,b,i) * wm_interm_7_so_pt4(a,i) * wm_interm_82_so_pt4(b,q)
term(1002) = term(1002) + r2(vrdav_Rl, a,p,b,i) * wm_interm_7_so_pt4(b,i) * wm_interm_82_so_pt4(a,q)
term(1003) = term(1003) + r2(vrdav_Rl, a,p,b,i) * wm_interm_104_so_pt4(a,q) * wm_interm_6_so_pt4(b,i)
term(1004) = term(1004) + r2(vrdav_Rl, a,p,b,i) * wm_interm_105_so_pt4(a,q) * wm_interm_6_so_pt4(b,i)
term(1005) = term(1005) + r2(vrdav_Rl, a,p,b,i) * wm_interm_104_so_pt4(b,q) * wm_interm_6_so_pt4(a,i)
term(1006) = term(1006) + r2(vrdav_Rl, a,p,b,i) * wm_interm_105_so_pt4(b,q) * wm_interm_6_so_pt4(a,i)
term(1007) = term(1007) + r2(vrdav_Rl, a,p,b,i) * wm_interm_104_so_pt4(b,q) * wm_interm_7_so_pt4(a,i)
term(1008) = term(1008) + r2(vrdav_Rl, a,p,b,i) * wm_interm_104_so_pt4(a,q) * wm_interm_7_so_pt4(b,i)
term(1009) = term(1009) + r2(vrdav_Rl, a,p,b,i) * wm_interm_105_so_pt4(b,q) * wm_interm_7_so_pt4(a,i)
term(1010) = term(1010) + r2(vrdav_Rl, a,p,b,i) * wm_interm_105_so_pt4(a,q) * wm_interm_7_so_pt4(b,i)
end do 
end do 
end do 

term(884) = term(884) * (-4.0d+0) 
term(885) = term(885) * (-4.0d+0) 
term(886) = term(886) * (8.0d+0) 
term(887) = term(887) * (-4.0d+0) 
term(888) = term(888) * (-4.0d+0) 
term(889) = term(889) * (8.0d+0) 
term(890) = term(890) * (-2.0d+0) 
term(891) = term(891) * (4.0d+0) 
term(893) = term(893) * (-2.0d+0) 
term(895) = term(895) * (-2.0d+0) 
term(896) = term(896) * (-4.0d+0) 
term(897) = term(897) * (8.0d+0) 
term(898) = term(898) * (4.0d+0) 
term(899) = term(899) * (-8.0d+0) 
term(900) = term(900) * (-4.0d+0) 
term(901) = term(901) * (8.0d+0) 
term(902) = term(902) * (-4.0d+0) 
term(903) = term(903) * (2.0d+0) 
term(904) = term(904) * (-4.0d+0) 
term(905) = term(905) * (2.0d+0) 
term(906) = term(906) * (-4.0d+0) 
term(907) = term(907) * (8.0d+0) 
term(908) = term(908) * (-4.0d+0) 
term(909) = term(909) * (2.0d+0) 
term(910) = term(910) * (2.0d+0) 
term(911) = term(911) * (2.0d+0) 
term(912) = term(912) * (2.0d+0) 
term(913) = term(913) * (-4.0d+0) 
term(914) = term(914) * (-4.0d+0) 
term(915) = term(915) * (2.0d+0) 
term(916) = term(916) * (-4.0d+0) 
term(917) = term(917) * (2.0d+0) 
term(918) = term(918) * (-4.0d+0) 
term(919) = term(919) * (-4.0d+0) 
term(920) = term(920) * (8.0d+0) 
term(921) = term(921) * (2.0d+0) 
term(922) = term(922) * (-4.0d+0) 
term(923) = term(923) * (2.0d+0) 
term(924) = term(924) * (2.0d+0) 
term(925) = term(925) * (-4.0d+0) 
term(926) = term(926) * (2.0d+0) 
term(927) = term(927) * (2.0d+0) 
term(928) = term(928) * (-4.0d+0) 
term(929) = term(929) * (2.0d+0) 
term(930) = term(930) * (-16.0d+0) 
term(931) = term(931) * (16.0d+0) 
term(932) = term(932) * (8.0d+0) 
term(933) = term(933) * (-8.0d+0) 
term(934) = term(934) * (-16.0d+0) 
term(935) = term(935) * (16.0d+0) 
term(936) = term(936) * (8.0d+0) 
term(937) = term(937) * (8.0d+0) 
term(938) = term(938) * (-8.0d+0) 
term(939) = term(939) * (-8.0d+0) 
term(940) = term(940) * (8.0d+0) 
term(941) = term(941) * (-16.0d+0) 
term(942) = term(942) * (-8.0d+0) 
term(943) = term(943) * (16.0d+0) 
term(944) = term(944) * (8.0d+0) 
term(945) = term(945) * (-8.0d+0) 
term(946) = term(946) * (8.0d+0) 
term(947) = term(947) * (-8.0d+0) 
term(948) = term(948) * (4.0d+0) 
term(949) = term(949) * (-8.0d+0) 
term(950) = term(950) * (4.0d+0) 
term(951) = term(951) * (-1.0d+0) 
term(952) = term(952) * (2.0d+0) 
term(953) = term(953) * (2.0d+0) 
term(954) = term(954) * (-4.0d+0) 
term(955) = term(955) * (-1.0d+0) 
term(956) = term(956) * (2.0d+0) 
term(957) = term(957) * (-1.0d+0) 
term(958) = term(958) * (2.0d+0) 
term(959) = term(959) * (-1.0d+0) 
term(960) = term(960) * (2.0d+0) 
term(961) = term(961) * (2.0d+0) 
term(962) = term(962) * (-4.0d+0) 
term(963) = term(963) * (2.0d+0) 
term(964) = term(964) * (-4.0d+0) 
term(965) = term(965) * (2.0d+0) 
term(966) = term(966) * (2.0d+0) 
term(967) = term(967) * (2.0d+0) 
term(968) = term(968) * (-4.0d+0) 
term(969) = term(969) * (-1.0d+0) 
term(970) = term(970) * (2.0d+0) 
term(971) = term(971) * (-1.0d+0) 
term(972) = term(972) * (-1.0d+0) 
term(973) = term(973) * (-1.0d+0) 
term(974) = term(974) * (2.0d+0) 
term(975) = term(975) * (-4.0d+0) 
term(976) = term(976) * (4.0d+0) 
term(977) = term(977) * (8.0d+0) 
term(978) = term(978) * (-8.0d+0) 
term(979) = term(979) * (-4.0d+0) 
term(980) = term(980) * (8.0d+0) 
term(981) = term(981) * (4.0d+0) 
term(982) = term(982) * (-8.0d+0) 
term(983) = term(983) * (8.0d+0) 
term(984) = term(984) * (-8.0d+0) 
term(985) = term(985) * (8.0d+0) 
term(986) = term(986) * (-8.0d+0) 
term(987) = term(987) * (-4.0d+0) 
term(988) = term(988) * (4.0d+0) 
term(989) = term(989) * (-4.0d+0) 
term(990) = term(990) * (4.0d+0) 
term(991) = term(991) * (-4.0d+0) 
term(992) = term(992) * (-4.0d+0) 
term(993) = term(993) * (8.0d+0) 
term(994) = term(994) * (2.0d+0) 
term(995) = term(995) * (2.0d+0) 
term(996) = term(996) * (-4.0d+0) 
term(997) = term(997) * (-1.0d+0) 
term(998) = term(998) * (2.0d+0) 
term(999) = term(999) * (-1.0d+0) 
term(1000) = term(1000) * (2.0d+0) 
term(1001) = term(1001) * (2.0d+0) 
term(1002) = term(1002) * (-4.0d+0) 
term(1003) = term(1003) * (-16.0d+0) 
term(1004) = term(1004) * (16.0d+0) 
term(1005) = term(1005) * (8.0d+0) 
term(1006) = term(1006) * (-8.0d+0) 
term(1007) = term(1007) * (-4.0d+0) 
term(1008) = term(1008) * (8.0d+0) 
term(1009) = term(1009) * (4.0d+0) 
term(1010) = term(1010) * (-8.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(1011) = term(1011) + s1(q,i) * wm_interm_60_so_pt4(j,k) * wm_interm_88_so_pt4(i,j,k,p)
term(1012) = term(1012) + s1(q,i) * wm_interm_60_so_pt4(j,k) * wm_interm_88_so_pt4(i,j,p,k)
term(1013) = term(1013) + s1(q,i) * wm_interm_62_so_pt4(j,k) * wm_interm_88_so_pt4(i,j,k,p)
term(1014) = term(1014) + s1(q,i) * wm_interm_62_so_pt4(j,k) * wm_interm_88_so_pt4(i,j,p,k)
term(1015) = term(1015) + s1(q,i) * wm_interm_109_so_pt4(i,j,k,p) * wm_interm_60_so_pt4(j,k)
term(1016) = term(1016) + s1(q,i) * wm_interm_109_so_pt4(i,j,p,k) * wm_interm_60_so_pt4(j,k)
term(1017) = term(1017) + s1(q,i) * wm_interm_109_so_pt4(i,j,k,p) * wm_interm_62_so_pt4(j,k)
term(1018) = term(1018) + s1(q,i) * wm_interm_109_so_pt4(i,j,p,k) * wm_interm_62_so_pt4(j,k)
end do 
end do 
end do 

term(1011) = term(1011) * (-1.0d+0) 
term(1012) = term(1012) * (2.0d+0) 
term(1013) = term(1013) * (2.0d+0) 
term(1014) = term(1014) * (-4.0d+0) 
term(1015) = term(1015) * (-2.0d+0) 
term(1016) = term(1016) * (4.0d+0) 
term(1017) = term(1017) * (4.0d+0) 
term(1018) = term(1018) * (-8.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
term(1019) = term(1019) + s1(q,i) * wm_interm_4_so_pt4(j,k) * wm_interm_9_so_pt4(p,k,j,i)
term(1020) = term(1020) + s1(q,i) * wm_interm_76_so_pt4(j,k) * wm_interm_9_so_pt4(p,k,j,i)
term(1021) = term(1021) + s1(q,i) * wm_interm_78_so_pt4(j,k) * wm_interm_9_so_pt4(p,k,j,i)
term(1022) = term(1022) + s1(q,i) * wm_interm_80_so_pt4(j,k) * wm_interm_9_so_pt4(p,k,j,i)
term(1023) = term(1023) + s1(q,i) * wm_interm_101_so_pt4(j,k) * wm_interm_9_so_pt4(p,k,j,i)
term(1024) = term(1024) + s1(q,i) * wm_interm_103_so_pt4(j,k) * wm_interm_9_so_pt4(p,k,j,i)
term(1025) = term(1025) + s1(q,i) * wm_interm_17_so_pt4(j,k) * wm_interm_71_so_pt4(p,k,j,i)
term(1026) = term(1026) + s1(q,i) * wm_interm_18_so_pt4(j,k) * wm_interm_71_so_pt4(p,k,j,i)
term(1027) = term(1027) + s1(q,i) * wm_interm_17_so_pt4(j,k) * wm_interm_72_so_pt4(k,p,j,i)
term(1028) = term(1028) + s1(q,i) * wm_interm_18_so_pt4(j,k) * wm_interm_72_so_pt4(k,p,j,i)
term(1029) = term(1029) + s1(q,i) * wm_interm_17_so_pt4(j,k) * wm_interm_99_so_pt4(p,k,j,i)
term(1030) = term(1030) + s1(q,i) * wm_interm_18_so_pt4(j,k) * wm_interm_99_so_pt4(p,k,j,i)
term(1031) = term(1031) + s1(q,i) * wm_interm_17_so_pt4(j,k) * wm_interm_99_so_pt4(k,p,j,i)
term(1032) = term(1032) + s1(q,i) * wm_interm_18_so_pt4(j,k) * wm_interm_99_so_pt4(k,p,j,i)
end do 
end do 
end do 

term(1019) = term(1019) * (-4.0d+0) 
term(1020) = term(1020) * (2.0d+0) 
term(1021) = term(1021) * (2.0d+0) 
term(1022) = term(1022) * (-4.0d+0) 
term(1023) = term(1023) * (8.0d+0) 
term(1024) = term(1024) * (-8.0d+0) 
term(1025) = term(1025) * (-0.5d+0) 
term(1027) = term(1027) * (-0.5d+0) 
term(1029) = term(1029) * (-2.0d+0) 
term(1030) = term(1030) * (4.0d+0) 
term(1031) = term(1031) * (2.0d+0) 
term(1032) = term(1032) * (-4.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(1033) = term(1033) + s1(q,i) * wm_interm_71_so_pt4(j,k,i,l) * wm_interm_9_so_pt4(l,p,j,k)
term(1034) = term(1034) + s1(q,i) * wm_interm_71_so_pt4(j,k,i,l) * wm_interm_9_so_pt4(p,l,j,k)
term(1035) = term(1035) + s1(q,i) * wm_interm_72_so_pt4(j,k,i,l) * wm_interm_9_so_pt4(l,p,j,k)
term(1036) = term(1036) + s1(q,i) * wm_interm_72_so_pt4(j,k,i,l) * wm_interm_9_so_pt4(p,l,j,k)
term(1037) = term(1037) + s1(q,i) * wm_interm_61_so_pt4(j,p,k,l) * wm_interm_88_so_pt4(i,j,l,k)
term(1038) = term(1038) + s1(q,i) * wm_interm_61_so_pt4(j,p,k,l) * wm_interm_88_so_pt4(i,j,k,l)
term(1039) = term(1039) + s1(q,i) * wm_interm_61_so_pt4(p,j,k,l) * wm_interm_88_so_pt4(i,j,l,k)
term(1040) = term(1040) + s1(q,i) * wm_interm_61_so_pt4(p,j,k,l) * wm_interm_88_so_pt4(j,i,k,l)
term(1041) = term(1041) + s1(q,i) * wm_interm_61_so_pt4(p,j,k,l) * wm_interm_88_so_pt4(i,j,k,l)
term(1042) = term(1042) + s1(q,i) * wm_interm_99_so_pt4(j,k,i,l) * wm_interm_9_so_pt4(l,p,j,k)
term(1043) = term(1043) + s1(q,i) * wm_interm_99_so_pt4(j,k,i,l) * wm_interm_9_so_pt4(p,l,j,k)
term(1044) = term(1044) + s1(q,i) * wm_interm_109_so_pt4(i,j,k,l) * wm_interm_61_so_pt4(j,p,l,k)
term(1045) = term(1045) + s1(q,i) * wm_interm_109_so_pt4(i,j,k,l) * wm_interm_61_so_pt4(j,p,k,l)
term(1046) = term(1046) + s1(q,i) * wm_interm_109_so_pt4(i,j,k,l) * wm_interm_61_so_pt4(p,j,l,k)
term(1047) = term(1047) + s1(q,i) * wm_interm_109_so_pt4(j,i,k,l) * wm_interm_61_so_pt4(p,j,k,l)
term(1048) = term(1048) + s1(q,i) * wm_interm_109_so_pt4(i,j,k,l) * wm_interm_61_so_pt4(p,j,k,l)
end do 
end do 
end do 
end do 

term(1033) = term(1033) * (-0.5d+0) 
term(1036) = term(1036) * (-0.5d+0) 
term(1038) = term(1038) * (-0.5d+0) 
term(1039) = term(1039) * (-0.5d+0) 
term(1040) = term(1040) * (-0.5d+0) 
term(1042) = term(1042) * (-2.0d+0) 
term(1043) = term(1043) * (2.0d+0) 
term(1044) = term(1044) * (2.0d+0) 
term(1045) = term(1045) * (-1.0d+0) 
term(1046) = term(1046) * (-1.0d+0) 
term(1047) = term(1047) * (-1.0d+0) 
term(1048) = term(1048) * (2.0d+0) 

do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(1049) = term(1049) + s1(a,p) * wm_interm_19_so_pt4(q,b) * wm_interm_91_so_pt4(b,a)
term(1050) = term(1050) + s1(a,p) * wm_interm_20_so_pt4(q,b) * wm_interm_91_so_pt4(b,a)
term(1051) = term(1051) + s1(a,p) * wm_interm_19_so_pt4(q,b) * wm_interm_92_so_pt4(b,a)
term(1052) = term(1052) + s1(a,p) * wm_interm_20_so_pt4(q,b) * wm_interm_92_so_pt4(b,a)
term(1053) = term(1053) + s1(a,p) * wm_interm_19_so_pt4(q,b) * wm_interm_90_so_pt4(b,a)
term(1054) = term(1054) + s1(a,p) * wm_interm_20_so_pt4(q,b) * wm_interm_90_so_pt4(b,a)
term(1055) = term(1055) + s1(a,p) * wm_interm_57_so_pt4(q,b) * wm_interm_81_so_pt4(a,b)
term(1056) = term(1056) + s1(a,p) * wm_interm_57_so_pt4(q,b) * wm_interm_82_so_pt4(a,b)
term(1057) = term(1057) + s1(a,p) * wm_interm_58_so_pt4(q,b) * wm_interm_81_so_pt4(a,b)
term(1058) = term(1058) + s1(a,p) * wm_interm_58_so_pt4(q,b) * wm_interm_82_so_pt4(a,b)
term(1059) = term(1059) + s1(a,p) * wm_interm_57_so_pt4(q,b) * wm_interm_83_so_pt4(a,b)
term(1060) = term(1060) + s1(a,p) * wm_interm_58_so_pt4(q,b) * wm_interm_83_so_pt4(a,b)
term(1061) = term(1061) + s1(a,p) * wm_interm_110_so_pt4(b,a) * wm_interm_19_so_pt4(q,b)
term(1062) = term(1062) + s1(a,p) * wm_interm_110_so_pt4(b,a) * wm_interm_20_so_pt4(q,b)
term(1063) = term(1063) + s1(a,p) * wm_interm_111_so_pt4(b,a) * wm_interm_19_so_pt4(q,b)
term(1064) = term(1064) + s1(a,p) * wm_interm_111_so_pt4(b,a) * wm_interm_20_so_pt4(q,b)
term(1065) = term(1065) + s1(a,p) * wm_interm_104_so_pt4(a,b) * wm_interm_57_so_pt4(q,b)
term(1066) = term(1066) + s1(a,p) * wm_interm_105_so_pt4(a,b) * wm_interm_57_so_pt4(q,b)
term(1067) = term(1067) + s1(a,p) * wm_interm_104_so_pt4(a,b) * wm_interm_58_so_pt4(q,b)
term(1068) = term(1068) + s1(a,p) * wm_interm_105_so_pt4(a,b) * wm_interm_58_so_pt4(q,b)
term(1069) = term(1069) + s1(a,p) * wm_interm_112_so_pt4(a,b) * wm_interm_57_so_pt4(q,b)
term(1070) = term(1070) + s1(a,p) * wm_interm_112_so_pt4(a,b) * wm_interm_58_so_pt4(q,b)
end do 
end do 

term(1049) = term(1049) * (-1.0d+0) 
term(1050) = term(1050) * (2.0d+0) 
term(1051) = term(1051) * (2.0d+0) 
term(1052) = term(1052) * (-4.0d+0) 
term(1053) = term(1053) * (-1.0d+0) 
term(1054) = term(1054) * (2.0d+0) 
term(1055) = term(1055) * (2.0d+0) 
term(1056) = term(1056) * (-4.0d+0) 
term(1057) = term(1057) * (-1.0d+0) 
term(1058) = term(1058) * (2.0d+0) 
term(1059) = term(1059) * (2.0d+0) 
term(1060) = term(1060) * (-1.0d+0) 
term(1061) = term(1061) * (-4.0d+0) 
term(1062) = term(1062) * (8.0d+0) 
term(1063) = term(1063) * (4.0d+0) 
term(1064) = term(1064) * (-8.0d+0) 
term(1065) = term(1065) * (4.0d+0) 
term(1066) = term(1066) * (-8.0d+0) 
term(1067) = term(1067) * (-2.0d+0) 
term(1068) = term(1068) * (4.0d+0) 
term(1069) = term(1069) * (4.0d+0) 
term(1070) = term(1070) * (-2.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(1071) = term(1071) + r2(vrdav_Rl, a,p,q,i) * wm_interm_19_so_pt4(b,a) * wm_interm_33_so_pt4(b,i)
term(1072) = term(1072) + r2(vrdav_Rl, a,p,q,i) * wm_interm_20_so_pt4(b,a) * wm_interm_33_so_pt4(b,i)
term(1073) = term(1073) + r2(vrdav_Rl, a,p,q,i) * wm_interm_19_so_pt4(b,a) * wm_interm_34_so_pt4(b,i)
term(1074) = term(1074) + r2(vrdav_Rl, a,p,q,i) * wm_interm_20_so_pt4(b,a) * wm_interm_34_so_pt4(b,i)
term(1075) = term(1075) + r2(vrdav_Rl, a,p,q,i) * wm_interm_19_so_pt4(b,a) * wm_interm_35_so_pt4(b,i)
term(1076) = term(1076) + r2(vrdav_Rl, a,p,q,i) * wm_interm_20_so_pt4(b,a) * wm_interm_35_so_pt4(b,i)
term(1077) = term(1077) + s2(a,q,p,i) * wm_interm_33_so_pt4(b,i) * wm_interm_57_so_pt4(b,a)
term(1078) = term(1078) + s2(a,q,p,i) * wm_interm_33_so_pt4(b,i) * wm_interm_58_so_pt4(b,a)
term(1079) = term(1079) + s2(a,q,p,i) * wm_interm_34_so_pt4(b,i) * wm_interm_57_so_pt4(b,a)
term(1080) = term(1080) + s2(a,q,p,i) * wm_interm_34_so_pt4(b,i) * wm_interm_58_so_pt4(b,a)
term(1081) = term(1081) + s2(a,q,p,i) * wm_interm_35_so_pt4(b,i) * wm_interm_57_so_pt4(b,a)
term(1082) = term(1082) + s2(a,q,p,i) * wm_interm_35_so_pt4(b,i) * wm_interm_58_so_pt4(b,a)
term(1083) = term(1083) + r2(vrdav_Rl, a,p,q,i) * wm_interm_19_so_pt4(b,a) * wm_interm_36_so_pt4(b,i)
term(1084) = term(1084) + r2(vrdav_Rl, a,p,q,i) * wm_interm_20_so_pt4(b,a) * wm_interm_36_so_pt4(b,i)
term(1085) = term(1085) + r2(vrdav_Rl, a,p,q,i) * wm_interm_19_so_pt4(b,a) * wm_interm_37_so_pt4(b,i)
term(1086) = term(1086) + r2(vrdav_Rl, a,p,q,i) * wm_interm_20_so_pt4(b,a) * wm_interm_37_so_pt4(b,i)
term(1087) = term(1087) + s2(a,q,p,i) * wm_interm_36_so_pt4(b,i) * wm_interm_57_so_pt4(b,a)
term(1088) = term(1088) + s2(a,q,p,i) * wm_interm_36_so_pt4(b,i) * wm_interm_58_so_pt4(b,a)
term(1089) = term(1089) + s2(a,q,p,i) * wm_interm_37_so_pt4(b,i) * wm_interm_57_so_pt4(b,a)
term(1090) = term(1090) + s2(a,q,p,i) * wm_interm_37_so_pt4(b,i) * wm_interm_58_so_pt4(b,a)
term(1091) = term(1091) + s2(a,q,p,i) * wm_interm_7_so_pt4(b,i) * wm_interm_90_so_pt4(b,a)
term(1092) = term(1092) + s2(a,q,p,i) * wm_interm_7_so_pt4(b,i) * wm_interm_91_so_pt4(b,a)
term(1093) = term(1093) + s2(a,q,p,i) * wm_interm_7_so_pt4(b,i) * wm_interm_92_so_pt4(b,a)
term(1094) = term(1094) + s2(a,q,p,i) * wm_interm_6_so_pt4(b,i) * wm_interm_90_so_pt4(b,a)
term(1095) = term(1095) + s2(a,q,p,i) * wm_interm_6_so_pt4(b,i) * wm_interm_91_so_pt4(b,a)
term(1096) = term(1096) + s2(a,q,p,i) * wm_interm_6_so_pt4(b,i) * wm_interm_92_so_pt4(b,a)
term(1097) = term(1097) + s2(a,q,p,i) * wm_interm_110_so_pt4(b,a) * wm_interm_7_so_pt4(b,i)
term(1098) = term(1098) + s2(a,q,p,i) * wm_interm_111_so_pt4(b,a) * wm_interm_7_so_pt4(b,i)
term(1099) = term(1099) + s2(a,q,p,i) * wm_interm_110_so_pt4(b,a) * wm_interm_6_so_pt4(b,i)
term(1100) = term(1100) + s2(a,q,p,i) * wm_interm_111_so_pt4(b,a) * wm_interm_6_so_pt4(b,i)
term(1101) = term(1101) + s1(a,i) * wm_interm_57_so_pt4(b,a) * wm_interm_84_so_pt4(b,q,i,p)
term(1102) = term(1102) + s1(a,i) * wm_interm_57_so_pt4(b,a) * wm_interm_73_so_pt4(b,q,i,p)
term(1103) = term(1103) + s1(a,i) * wm_interm_58_so_pt4(b,a) * wm_interm_84_so_pt4(b,q,i,p)
term(1104) = term(1104) + s1(a,i) * wm_interm_58_so_pt4(b,a) * wm_interm_73_so_pt4(b,q,i,p)
term(1105) = term(1105) + s1(a,i) * wm_interm_57_so_pt4(b,a) * wm_interm_70_so_pt4(b,q,i,p)
term(1106) = term(1106) + s1(a,i) * wm_interm_58_so_pt4(b,a) * wm_interm_70_so_pt4(b,q,i,p)
term(1107) = term(1107) + s1(a,i) * wm_interm_57_so_pt4(b,a) * wm_interm_98_so_pt4(b,q,i,p)
term(1108) = term(1108) + s1(a,i) * wm_interm_57_so_pt4(b,a) * wm_interm_97_so_pt4(b,q,i,p)
term(1109) = term(1109) + s1(a,i) * wm_interm_58_so_pt4(b,a) * wm_interm_98_so_pt4(b,q,i,p)
term(1110) = term(1110) + s1(a,i) * wm_interm_58_so_pt4(b,a) * wm_interm_97_so_pt4(b,q,i,p)
end do 
end do 
end do 

term(1071) = term(1071) * (-2.0d+0) 
term(1072) = term(1072) * (4.0d+0) 
term(1074) = term(1074) * (-2.0d+0) 
term(1076) = term(1076) * (-2.0d+0) 
term(1077) = term(1077) * (4.0d+0) 
term(1078) = term(1078) * (-2.0d+0) 
term(1079) = term(1079) * (-2.0d+0) 
term(1081) = term(1081) * (-2.0d+0) 
term(1083) = term(1083) * (-4.0d+0) 
term(1084) = term(1084) * (8.0d+0) 
term(1085) = term(1085) * (4.0d+0) 
term(1086) = term(1086) * (-8.0d+0) 
term(1087) = term(1087) * (8.0d+0) 
term(1088) = term(1088) * (-4.0d+0) 
term(1089) = term(1089) * (-8.0d+0) 
term(1090) = term(1090) * (4.0d+0) 
term(1091) = term(1091) * (2.0d+0) 
term(1092) = term(1092) * (2.0d+0) 
term(1093) = term(1093) * (-4.0d+0) 
term(1094) = term(1094) * (-4.0d+0) 
term(1095) = term(1095) * (-4.0d+0) 
term(1096) = term(1096) * (8.0d+0) 
term(1097) = term(1097) * (8.0d+0) 
term(1098) = term(1098) * (-8.0d+0) 
term(1099) = term(1099) * (-16.0d+0) 
term(1100) = term(1100) * (16.0d+0) 
term(1101) = term(1101) * (2.0d+0) 
term(1102) = term(1102) * (-4.0d+0) 
term(1103) = term(1103) * (-1.0d+0) 
term(1104) = term(1104) * (2.0d+0) 
term(1105) = term(1105) * (2.0d+0) 
term(1106) = term(1106) * (-1.0d+0) 
term(1107) = term(1107) * (8.0d+0) 
term(1108) = term(1108) * (-8.0d+0) 
term(1109) = term(1109) * (-4.0d+0) 
term(1110) = term(1110) * (4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
term(1111) = term(1111) + s1(q,i) * wm_interm_17_so_pt4(j,k) * wm_interm_71_so_pt4(p,k,i,j)
term(1112) = term(1112) + s1(q,i) * wm_interm_18_so_pt4(j,k) * wm_interm_71_so_pt4(p,k,i,j)
term(1113) = term(1113) + s1(q,i) * wm_interm_17_so_pt4(j,k) * wm_interm_72_so_pt4(p,k,i,j)
term(1114) = term(1114) + s1(q,i) * wm_interm_18_so_pt4(j,k) * wm_interm_72_so_pt4(p,k,i,j)
term(1115) = term(1115) + s1(q,i) * wm_interm_17_so_pt4(j,k) * wm_interm_71_so_pt4(k,p,i,j)
term(1116) = term(1116) + s1(q,i) * wm_interm_18_so_pt4(j,k) * wm_interm_71_so_pt4(k,p,i,j)
term(1117) = term(1117) + s1(q,i) * wm_interm_17_so_pt4(j,k) * wm_interm_72_so_pt4(k,p,i,j)
term(1118) = term(1118) + s1(q,i) * wm_interm_18_so_pt4(j,k) * wm_interm_72_so_pt4(k,p,i,j)
term(1119) = term(1119) + s1(q,i) * wm_interm_17_so_pt4(j,k) * wm_interm_99_so_pt4(p,k,i,j)
term(1120) = term(1120) + s1(q,i) * wm_interm_18_so_pt4(j,k) * wm_interm_99_so_pt4(p,k,i,j)
term(1121) = term(1121) + s1(q,i) * wm_interm_17_so_pt4(j,k) * wm_interm_99_so_pt4(k,p,i,j)
term(1122) = term(1122) + s1(q,i) * wm_interm_18_so_pt4(j,k) * wm_interm_99_so_pt4(k,p,i,j)
end do 
end do 
end do 

term(1112) = term(1112) * (-2.0d+0) 
term(1113) = term(1113) * (-0.5d+0) 
term(1115) = term(1115) * (-0.5d+0) 
term(1118) = term(1118) * (-2.0d+0) 
term(1119) = term(1119) * (2.0d+0) 
term(1120) = term(1120) * (-4.0d+0) 
term(1121) = term(1121) * (-2.0d+0) 
term(1122) = term(1122) * (4.0d+0) 

do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(1123) = term(1123) + s2(a,b,i,p) * wm_interm_33_so_pt4(b,i) * wm_interm_57_so_pt4(q,a)
term(1124) = term(1124) + s2(a,b,i,p) * wm_interm_33_so_pt4(b,i) * wm_interm_58_so_pt4(q,a)
term(1125) = term(1125) + s2(a,b,i,p) * wm_interm_34_so_pt4(b,i) * wm_interm_57_so_pt4(q,a)
term(1126) = term(1126) + s2(a,b,i,p) * wm_interm_34_so_pt4(b,i) * wm_interm_58_so_pt4(q,a)
term(1127) = term(1127) + s2(a,b,i,p) * wm_interm_35_so_pt4(b,i) * wm_interm_57_so_pt4(q,a)
term(1128) = term(1128) + s2(a,b,i,p) * wm_interm_35_so_pt4(b,i) * wm_interm_58_so_pt4(q,a)
term(1129) = term(1129) + s2(a,b,i,p) * wm_interm_36_so_pt4(b,i) * wm_interm_57_so_pt4(q,a)
term(1130) = term(1130) + s2(a,b,i,p) * wm_interm_36_so_pt4(b,i) * wm_interm_58_so_pt4(q,a)
term(1131) = term(1131) + s2(a,b,i,p) * wm_interm_37_so_pt4(b,i) * wm_interm_57_so_pt4(q,a)
term(1132) = term(1132) + s2(a,b,i,p) * wm_interm_37_so_pt4(b,i) * wm_interm_58_so_pt4(q,a)
end do 
end do 
end do 

term(1123) = term(1123) * (4.0d+0) 
term(1124) = term(1124) * (-2.0d+0) 
term(1125) = term(1125) * (-2.0d+0) 
term(1127) = term(1127) * (-2.0d+0) 
term(1129) = term(1129) * (8.0d+0) 
term(1130) = term(1130) * (-4.0d+0) 
term(1131) = term(1131) * (-8.0d+0) 
term(1132) = term(1132) * (4.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
term(1133) = term(1133) + s1(q,i) * wm_interm_61_so_pt4(p,j,k,i) * wm_interm_85_so_pt4(j,k)
term(1134) = term(1134) + s1(q,i) * wm_interm_61_so_pt4(p,j,k,i) * wm_interm_86_so_pt4(j,k)
term(1135) = term(1135) + s1(q,i) * wm_interm_61_so_pt4(p,j,k,i) * wm_interm_87_so_pt4(j,k)
term(1136) = term(1136) + s1(q,i) * wm_interm_106_so_pt4(j,k) * wm_interm_61_so_pt4(p,j,k,i)
term(1137) = term(1137) + s1(q,i) * wm_interm_107_so_pt4(j,k) * wm_interm_61_so_pt4(p,j,k,i)
term(1138) = term(1138) + s1(q,i) * wm_interm_108_so_pt4(j,k) * wm_interm_61_so_pt4(p,j,k,i)
end do 
end do 
end do 

term(1134) = term(1134) * (-2.0d+0) 
term(1136) = term(1136) * (2.0d+0) 
term(1137) = term(1137) * (-4.0d+0) 
term(1138) = term(1138) * (2.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(1139) = term(1139) + s1(a,i) * wm_interm_54_so_pt4(q,b,j,i) * wm_interm_67_so_pt4(a,b,j,p)
term(1140) = term(1140) + s1(a,i) * wm_interm_54_so_pt4(q,b,j,i) * wm_interm_68_so_pt4(a,b,j,p)
term(1141) = term(1141) + s1(a,i) * wm_interm_54_so_pt4(q,b,j,i) * wm_interm_74_so_pt4(a,b,j,p)
term(1142) = term(1142) + s1(a,i) * wm_interm_54_so_pt4(q,b,j,i) * wm_interm_69_so_pt4(a,b,j,p)
term(1143) = term(1143) + s1(a,i) * wm_interm_54_so_pt4(q,b,j,i) * wm_interm_73_so_pt4(a,b,j,p)
term(1144) = term(1144) + s1(a,i) * wm_interm_54_so_pt4(q,b,j,i) * wm_interm_84_so_pt4(a,b,j,p)
term(1145) = term(1145) + s1(q,i) * wm_interm_54_so_pt4(a,b,j,i) * wm_interm_70_so_pt4(a,b,j,p)
term(1146) = term(1146) + s1(q,i) * wm_interm_54_so_pt4(a,b,j,i) * wm_interm_84_so_pt4(a,b,j,p)
term(1147) = term(1147) + s1(q,i) * wm_interm_54_so_pt4(a,b,j,i) * wm_interm_73_so_pt4(a,b,j,p)
term(1148) = term(1148) + s1(a,i) * wm_interm_55_so_pt4(q,b,j,i) * wm_interm_67_so_pt4(a,b,j,p)
term(1149) = term(1149) + s1(a,i) * wm_interm_55_so_pt4(q,b,j,i) * wm_interm_68_so_pt4(a,b,j,p)
term(1150) = term(1150) + s1(a,i) * wm_interm_55_so_pt4(q,b,j,i) * wm_interm_74_so_pt4(a,b,j,p)
term(1151) = term(1151) + s1(a,i) * wm_interm_55_so_pt4(q,b,j,i) * wm_interm_69_so_pt4(a,b,j,p)
term(1152) = term(1152) + s1(a,i) * wm_interm_55_so_pt4(q,b,j,i) * wm_interm_73_so_pt4(a,b,j,p)
term(1153) = term(1153) + s1(a,i) * wm_interm_55_so_pt4(q,b,j,i) * wm_interm_84_so_pt4(a,b,j,p)
term(1154) = term(1154) + s1(q,i) * wm_interm_55_so_pt4(a,b,j,i) * wm_interm_70_so_pt4(a,b,j,p)
term(1155) = term(1155) + s1(q,i) * wm_interm_55_so_pt4(a,b,j,i) * wm_interm_84_so_pt4(a,b,j,p)
term(1156) = term(1156) + s1(q,i) * wm_interm_55_so_pt4(a,b,j,i) * wm_interm_73_so_pt4(a,b,j,p)
term(1157) = term(1157) + r2(vrdav_Rl, a,j,q,i) * wm_interm_3_so_pt4(b,p,j,i) * wm_interm_81_so_pt4(a,b)
term(1158) = term(1158) + r2(vrdav_Rl, a,j,q,i) * wm_interm_3_so_pt4(b,p,j,i) * wm_interm_82_so_pt4(a,b)
term(1159) = term(1159) + r2(vrdav_Rl, a,j,q,i) * wm_interm_3_so_pt4(b,p,j,i) * wm_interm_83_so_pt4(a,b)
term(1160) = term(1160) + s1(q,i) * wm_interm_56_so_pt4(a,b,j,i) * wm_interm_67_so_pt4(a,b,j,p)
term(1161) = term(1161) + s1(q,i) * wm_interm_56_so_pt4(a,b,j,i) * wm_interm_68_so_pt4(a,b,j,p)
term(1162) = term(1162) + s1(q,i) * wm_interm_56_so_pt4(a,b,j,i) * wm_interm_74_so_pt4(a,b,j,p)
term(1163) = term(1163) + s1(q,i) * wm_interm_56_so_pt4(a,b,j,i) * wm_interm_69_so_pt4(a,b,j,p)
term(1164) = term(1164) + s1(q,i) * wm_interm_56_so_pt4(a,b,j,i) * wm_interm_73_so_pt4(a,b,j,p)
term(1165) = term(1165) + s1(q,i) * wm_interm_56_so_pt4(a,b,j,i) * wm_interm_84_so_pt4(a,b,j,p)
term(1166) = term(1166) + s1(q,i) * wm_interm_59_so_pt4(a,b,j,i) * wm_interm_70_so_pt4(a,b,j,p)
term(1167) = term(1167) + s1(q,i) * wm_interm_59_so_pt4(a,b,j,i) * wm_interm_84_so_pt4(a,b,j,p)
term(1168) = term(1168) + s1(q,i) * wm_interm_59_so_pt4(a,b,j,i) * wm_interm_73_so_pt4(a,b,j,p)
term(1169) = term(1169) + s1(a,i) * wm_interm_59_so_pt4(q,b,j,i) * wm_interm_67_so_pt4(a,b,j,p)
term(1170) = term(1170) + s1(a,i) * wm_interm_59_so_pt4(q,b,j,i) * wm_interm_68_so_pt4(a,b,j,p)
term(1171) = term(1171) + s1(a,i) * wm_interm_59_so_pt4(q,b,j,i) * wm_interm_69_so_pt4(a,b,j,p)
term(1172) = term(1172) + s1(a,i) * wm_interm_56_so_pt4(q,b,j,i) * wm_interm_70_so_pt4(a,b,j,p)
term(1173) = term(1173) + s1(a,i) * wm_interm_59_so_pt4(q,b,j,i) * wm_interm_74_so_pt4(a,b,j,p)
term(1174) = term(1174) + s1(a,i) * wm_interm_59_so_pt4(q,b,j,i) * wm_interm_73_so_pt4(a,b,j,p)
term(1175) = term(1175) + s1(a,i) * wm_interm_59_so_pt4(q,b,j,i) * wm_interm_84_so_pt4(a,b,j,p)
term(1176) = term(1176) + s1(a,i) * wm_interm_56_so_pt4(q,b,j,i) * wm_interm_84_so_pt4(a,b,j,p)
term(1177) = term(1177) + s1(a,i) * wm_interm_56_so_pt4(q,b,j,i) * wm_interm_73_so_pt4(a,b,j,p)
term(1178) = term(1178) + s1(a,i) * wm_interm_54_so_pt4(q,b,j,i) * wm_interm_95_so_pt4(a,b,j,p)
term(1179) = term(1179) + s1(a,i) * wm_interm_54_so_pt4(q,b,j,i) * wm_interm_96_so_pt4(a,b,j,p)
term(1180) = term(1180) + s1(a,i) * wm_interm_54_so_pt4(q,b,j,i) * wm_interm_97_so_pt4(a,b,j,p)
term(1181) = term(1181) + s1(a,i) * wm_interm_54_so_pt4(q,b,j,i) * wm_interm_98_so_pt4(a,b,j,p)
term(1182) = term(1182) + s1(q,i) * wm_interm_54_so_pt4(a,b,j,i) * wm_interm_98_so_pt4(a,b,j,p)
term(1183) = term(1183) + s1(q,i) * wm_interm_54_so_pt4(a,b,j,i) * wm_interm_97_so_pt4(a,b,j,p)
term(1184) = term(1184) + s1(a,i) * wm_interm_55_so_pt4(q,b,j,i) * wm_interm_95_so_pt4(a,b,j,p)
term(1185) = term(1185) + s1(a,i) * wm_interm_55_so_pt4(q,b,j,i) * wm_interm_96_so_pt4(a,b,j,p)
term(1186) = term(1186) + s1(a,i) * wm_interm_55_so_pt4(q,b,j,i) * wm_interm_97_so_pt4(a,b,j,p)
term(1187) = term(1187) + s1(a,i) * wm_interm_55_so_pt4(q,b,j,i) * wm_interm_98_so_pt4(a,b,j,p)
term(1188) = term(1188) + s1(q,i) * wm_interm_55_so_pt4(a,b,j,i) * wm_interm_98_so_pt4(a,b,j,p)
term(1189) = term(1189) + s1(q,i) * wm_interm_55_so_pt4(a,b,j,i) * wm_interm_97_so_pt4(a,b,j,p)
term(1190) = term(1190) + r2(vrdav_Rl, a,j,q,i) * wm_interm_104_so_pt4(a,b) * wm_interm_3_so_pt4(b,p,j,i)
term(1191) = term(1191) + r2(vrdav_Rl, a,j,q,i) * wm_interm_105_so_pt4(a,b) * wm_interm_3_so_pt4(b,p,j,i)
term(1192) = term(1192) + s1(q,i) * wm_interm_56_so_pt4(a,b,j,i) * wm_interm_95_so_pt4(a,b,j,p)
term(1193) = term(1193) + s1(q,i) * wm_interm_56_so_pt4(a,b,j,i) * wm_interm_96_so_pt4(a,b,j,p)
term(1194) = term(1194) + s1(q,i) * wm_interm_56_so_pt4(a,b,j,i) * wm_interm_97_so_pt4(a,b,j,p)
term(1195) = term(1195) + s1(q,i) * wm_interm_56_so_pt4(a,b,j,i) * wm_interm_98_so_pt4(a,b,j,p)
term(1196) = term(1196) + s1(q,i) * wm_interm_59_so_pt4(a,b,j,i) * wm_interm_98_so_pt4(a,b,j,p)
term(1197) = term(1197) + s1(q,i) * wm_interm_59_so_pt4(a,b,j,i) * wm_interm_97_so_pt4(a,b,j,p)
term(1198) = term(1198) + s1(a,i) * wm_interm_59_so_pt4(q,b,j,i) * wm_interm_95_so_pt4(a,b,j,p)
term(1199) = term(1199) + s1(a,i) * wm_interm_59_so_pt4(q,b,j,i) * wm_interm_96_so_pt4(a,b,j,p)
term(1200) = term(1200) + s1(a,i) * wm_interm_59_so_pt4(q,b,j,i) * wm_interm_97_so_pt4(a,b,j,p)
term(1201) = term(1201) + s1(a,i) * wm_interm_56_so_pt4(q,b,j,i) * wm_interm_98_so_pt4(a,b,j,p)
term(1202) = term(1202) + s1(a,i) * wm_interm_59_so_pt4(q,b,j,i) * wm_interm_98_so_pt4(a,b,j,p)
term(1203) = term(1203) + s1(a,i) * wm_interm_56_so_pt4(q,b,j,i) * wm_interm_97_so_pt4(a,b,j,p)
end do 
end do 
end do 
end do 

term(1139) = term(1139) * (-4.0d+0) 
term(1140) = term(1140) * (8.0d+0) 
term(1141) = term(1141) * (-4.0d+0) 
term(1142) = term(1142) * (-4.0d+0) 
term(1143) = term(1143) * (-4.0d+0) 
term(1144) = term(1144) * (8.0d+0) 
term(1145) = term(1145) * (-4.0d+0) 
term(1146) = term(1146) * (-4.0d+0) 
term(1147) = term(1147) * (8.0d+0) 
term(1148) = term(1148) * (2.0d+0) 
term(1149) = term(1149) * (-4.0d+0) 
term(1150) = term(1150) * (2.0d+0) 
term(1151) = term(1151) * (2.0d+0) 
term(1152) = term(1152) * (2.0d+0) 
term(1153) = term(1153) * (-4.0d+0) 
term(1154) = term(1154) * (2.0d+0) 
term(1155) = term(1155) * (2.0d+0) 
term(1156) = term(1156) * (-4.0d+0) 
term(1157) = term(1157) * (2.0d+0) 
term(1158) = term(1158) * (-4.0d+0) 
term(1159) = term(1159) * (2.0d+0) 
term(1160) = term(1160) * (2.0d+0) 
term(1161) = term(1161) * (-4.0d+0) 
term(1162) = term(1162) * (2.0d+0) 
term(1163) = term(1163) * (2.0d+0) 
term(1164) = term(1164) * (2.0d+0) 
term(1165) = term(1165) * (-4.0d+0) 
term(1166) = term(1166) * (2.0d+0) 
term(1167) = term(1167) * (2.0d+0) 
term(1168) = term(1168) * (-4.0d+0) 
term(1169) = term(1169) * (2.0d+0) 
term(1170) = term(1170) * (-4.0d+0) 
term(1171) = term(1171) * (2.0d+0) 
term(1172) = term(1172) * (2.0d+0) 
term(1173) = term(1173) * (2.0d+0) 
term(1174) = term(1174) * (2.0d+0) 
term(1175) = term(1175) * (-4.0d+0) 
term(1176) = term(1176) * (2.0d+0) 
term(1177) = term(1177) * (-4.0d+0) 
term(1178) = term(1178) * (-16.0d+0) 
term(1179) = term(1179) * (16.0d+0) 
term(1180) = term(1180) * (-16.0d+0) 
term(1181) = term(1181) * (16.0d+0) 
term(1182) = term(1182) * (-16.0d+0) 
term(1183) = term(1183) * (16.0d+0) 
term(1184) = term(1184) * (8.0d+0) 
term(1185) = term(1185) * (-8.0d+0) 
term(1186) = term(1186) * (8.0d+0) 
term(1187) = term(1187) * (-8.0d+0) 
term(1188) = term(1188) * (8.0d+0) 
term(1189) = term(1189) * (-8.0d+0) 
term(1190) = term(1190) * (8.0d+0) 
term(1191) = term(1191) * (-8.0d+0) 
term(1192) = term(1192) * (8.0d+0) 
term(1193) = term(1193) * (-8.0d+0) 
term(1194) = term(1194) * (8.0d+0) 
term(1195) = term(1195) * (-8.0d+0) 
term(1196) = term(1196) * (8.0d+0) 
term(1197) = term(1197) * (-8.0d+0) 
term(1198) = term(1198) * (8.0d+0) 
term(1199) = term(1199) * (-8.0d+0) 
term(1200) = term(1200) * (8.0d+0) 
term(1201) = term(1201) * (8.0d+0) 
term(1202) = term(1202) * (-8.0d+0) 
term(1203) = term(1203) * (-8.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(1204) = term(1204) + r2(vrdav_Rl, a,j,q,i) * wm_interm_3_so_pt4(b,k,j,i) * wm_interm_70_so_pt4(a,b,k,p)
term(1205) = term(1205) + r2(vrdav_Rl, a,j,q,i) * wm_interm_3_so_pt4(b,k,j,i) * wm_interm_84_so_pt4(a,b,k,p)
term(1206) = term(1206) + r2(vrdav_Rl, a,j,q,i) * wm_interm_3_so_pt4(b,k,j,i) * wm_interm_73_so_pt4(a,b,k,p)
term(1207) = term(1207) + r2(vrdav_Rl, a,j,q,i) * wm_interm_3_so_pt4(b,k,j,i) * wm_interm_98_so_pt4(a,b,k,p)
term(1208) = term(1208) + r2(vrdav_Rl, a,j,q,i) * wm_interm_3_so_pt4(b,k,j,i) * wm_interm_97_so_pt4(a,b,k,p)
end do 
end do 
end do 
end do 
end do 

term(1204) = term(1204) * (2.0d+0) 
term(1205) = term(1205) * (2.0d+0) 
term(1206) = term(1206) * (-4.0d+0) 
term(1207) = term(1207) * (8.0d+0) 
term(1208) = term(1208) * (-8.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do j = 1, nocc 
term(1209) = term(1209) + s1(q,i) * wm_interm_71_so_pt4(j,k,l,i) * wm_interm_9_so_pt4(p,l,j,k)
term(1210) = term(1210) + s1(q,i) * wm_interm_72_so_pt4(j,k,l,i) * wm_interm_9_so_pt4(l,p,j,k)
term(1211) = term(1211) + s1(q,i) * wm_interm_99_so_pt4(j,k,l,i) * wm_interm_9_so_pt4(p,l,j,k)
term(1212) = term(1212) + s1(q,i) * wm_interm_99_so_pt4(j,k,l,i) * wm_interm_9_so_pt4(l,p,j,k)
end do 
end do 
end do 
end do 

term(1209) = term(1209) * (-0.5d+0) 
term(1210) = term(1210) * (-0.5d+0) 
term(1211) = term(1211) * (-2.0d+0) 
term(1212) = term(1212) * (2.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(1213) = term(1213) + r2(vrdav_Rl, a,j,q,i) * wm_interm_3_so_pt4(b,k,i,j) * wm_interm_67_so_pt4(a,b,k,p)
term(1214) = term(1214) + r2(vrdav_Rl, a,j,q,i) * wm_interm_3_so_pt4(b,k,i,j) * wm_interm_68_so_pt4(a,b,k,p)
term(1215) = term(1215) + r2(vrdav_Rl, a,j,q,i) * wm_interm_3_so_pt4(b,k,i,j) * wm_interm_74_so_pt4(a,b,k,p)
term(1216) = term(1216) + r2(vrdav_Rl, a,j,q,i) * wm_interm_3_so_pt4(b,k,i,j) * wm_interm_69_so_pt4(a,b,k,p)
term(1217) = term(1217) + r2(vrdav_Rl, a,j,q,i) * wm_interm_3_so_pt4(b,k,i,j) * wm_interm_73_so_pt4(a,b,k,p)
term(1218) = term(1218) + r2(vrdav_Rl, a,j,q,i) * wm_interm_3_so_pt4(b,k,i,j) * wm_interm_84_so_pt4(a,b,k,p)
term(1219) = term(1219) + r2(vrdav_Rl, a,j,q,i) * wm_interm_3_so_pt4(b,k,i,j) * wm_interm_95_so_pt4(a,b,k,p)
term(1220) = term(1220) + r2(vrdav_Rl, a,j,q,i) * wm_interm_3_so_pt4(b,k,i,j) * wm_interm_96_so_pt4(a,b,k,p)
term(1221) = term(1221) + r2(vrdav_Rl, a,j,q,i) * wm_interm_3_so_pt4(b,k,i,j) * wm_interm_97_so_pt4(a,b,k,p)
term(1222) = term(1222) + r2(vrdav_Rl, a,j,q,i) * wm_interm_3_so_pt4(b,k,i,j) * wm_interm_98_so_pt4(a,b,k,p)
end do 
end do 
end do 
end do 
end do 

term(1213) = term(1213) * (2.0d+0) 
term(1214) = term(1214) * (-4.0d+0) 
term(1215) = term(1215) * (2.0d+0) 
term(1216) = term(1216) * (2.0d+0) 
term(1217) = term(1217) * (2.0d+0) 
term(1218) = term(1218) * (-4.0d+0) 
term(1219) = term(1219) * (8.0d+0) 
term(1220) = term(1220) * (-8.0d+0) 
term(1221) = term(1221) * (8.0d+0) 
term(1222) = term(1222) * (-8.0d+0) 

do l = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(1223) = term(1223) + s2(a,q,p,i) * wm_interm_3_so_pt4(a,j,k,l) * wm_interm_71_so_pt4(l,k,i,j)
term(1224) = term(1224) + s2(a,q,p,i) * wm_interm_3_so_pt4(a,j,k,l) * wm_interm_72_so_pt4(l,k,i,j)
term(1225) = term(1225) + s2(a,q,j,i) * wm_interm_3_so_pt4(a,p,k,l) * wm_interm_71_so_pt4(l,k,i,j)
term(1226) = term(1226) + s2(a,q,j,i) * wm_interm_3_so_pt4(a,p,k,l) * wm_interm_72_so_pt4(l,k,i,j)
term(1227) = term(1227) + s2(a,q,j,i) * wm_interm_3_so_pt4(a,p,k,l) * wm_interm_71_so_pt4(k,l,i,j)
term(1228) = term(1228) + s2(a,q,j,i) * wm_interm_3_so_pt4(a,p,k,l) * wm_interm_72_so_pt4(k,l,i,j)
term(1229) = term(1229) + s2(a,q,p,i) * wm_interm_3_so_pt4(a,j,k,l) * wm_interm_71_so_pt4(k,l,i,j)
term(1230) = term(1230) + s2(a,q,p,i) * wm_interm_3_so_pt4(a,j,k,l) * wm_interm_72_so_pt4(k,l,i,j)
term(1231) = term(1231) + s2(a,q,p,i) * wm_interm_3_so_pt4(a,j,k,l) * wm_interm_99_so_pt4(l,k,i,j)
term(1232) = term(1232) + s2(a,q,j,i) * wm_interm_3_so_pt4(a,p,k,l) * wm_interm_99_so_pt4(l,k,i,j)
term(1233) = term(1233) + s2(a,q,j,i) * wm_interm_3_so_pt4(a,p,k,l) * wm_interm_99_so_pt4(k,l,i,j)
term(1234) = term(1234) + s2(a,q,p,i) * wm_interm_3_so_pt4(a,j,k,l) * wm_interm_99_so_pt4(k,l,i,j)
end do 
end do 
end do 
end do 
end do 

term(1223) = term(1223) * (-2.0d+0) 
term(1226) = term(1226) * (-2.0d+0) 
term(1227) = term(1227) * (-2.0d+0) 
term(1230) = term(1230) * (-2.0d+0) 
term(1231) = term(1231) * (-4.0d+0) 
term(1232) = term(1232) * (4.0d+0) 
term(1233) = term(1233) * (-4.0d+0) 
term(1234) = term(1234) * (4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(1235) = term(1235) + r2(vrdav_Rl, a,j,q,i) * wm_interm_28_so_pt4(a,k,l,p) * wm_interm_9_so_pt4(l,k,i,j)
term(1236) = term(1236) + r2(vrdav_Rl, a,j,q,i) * wm_interm_28_so_pt4(a,k,l,p) * wm_interm_9_so_pt4(k,l,i,j)
term(1237) = term(1237) + r2(vrdav_Rl, a,j,q,i) * wm_interm_27_so_pt4(a,k,l,p) * wm_interm_9_so_pt4(k,l,i,j)
term(1238) = term(1238) + s2(a,q,j,i) * wm_interm_28_so_pt4(a,k,l,p) * wm_interm_61_so_pt4(l,k,i,j)
term(1239) = term(1239) + s2(a,q,j,i) * wm_interm_28_so_pt4(a,k,l,p) * wm_interm_61_so_pt4(k,l,i,j)
term(1240) = term(1240) + s2(a,q,j,i) * wm_interm_27_so_pt4(a,k,l,p) * wm_interm_61_so_pt4(k,l,i,j)
term(1241) = term(1241) + r2(vrdav_Rl, a,j,q,i) * wm_interm_30_so_pt4(a,k,l,p) * wm_interm_9_so_pt4(k,l,i,j)
term(1242) = term(1242) + r2(vrdav_Rl, a,j,q,i) * wm_interm_30_so_pt4(a,k,l,p) * wm_interm_9_so_pt4(l,k,i,j)
term(1243) = term(1243) + s2(a,q,j,i) * wm_interm_31_so_pt4(a,k,l,p) * wm_interm_61_so_pt4(l,k,i,j)
term(1244) = term(1244) + s2(a,q,j,i) * wm_interm_31_so_pt4(a,k,l,p) * wm_interm_61_so_pt4(k,l,i,j)
term(1245) = term(1245) + s2(a,q,j,i) * wm_interm_30_so_pt4(a,k,l,p) * wm_interm_61_so_pt4(k,l,i,j)
end do 
end do 
end do 
end do 
end do 

term(1235) = term(1235) * (-0.5d+0) 
term(1237) = term(1237) * (-0.5d+0) 
term(1238) = term(1238) * (-0.5d+0) 
term(1240) = term(1240) * (-0.5d+0) 
term(1241) = term(1241) * (-2.0d+0) 
term(1242) = term(1242) * (2.0d+0) 
term(1243) = term(1243) * (-1.0d+0) 
term(1244) = term(1244) * (2.0d+0) 
term(1245) = term(1245) * (-1.0d+0) 

do i = 1, nocc 
do l = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(1246) = term(1246) + r2(vrdav_Rl, a,j,q,i) * wm_interm_28_so_pt4(a,k,l,p) * wm_interm_9_so_pt4(k,l,j,i)
term(1247) = term(1247) + r2(vrdav_Rl, a,j,q,i) * wm_interm_28_so_pt4(a,k,l,p) * wm_interm_9_so_pt4(l,k,j,i)
term(1248) = term(1248) + r2(vrdav_Rl, a,j,q,i) * wm_interm_27_so_pt4(a,k,l,p) * wm_interm_9_so_pt4(l,k,j,i)
term(1249) = term(1249) + s2(a,q,j,i) * wm_interm_28_so_pt4(a,k,l,p) * wm_interm_61_so_pt4(k,l,j,i)
term(1250) = term(1250) + s2(a,q,j,i) * wm_interm_28_so_pt4(a,k,l,p) * wm_interm_61_so_pt4(l,k,j,i)
term(1251) = term(1251) + s2(a,q,j,i) * wm_interm_27_so_pt4(a,k,l,p) * wm_interm_61_so_pt4(l,k,j,i)
term(1252) = term(1252) + r2(vrdav_Rl, a,j,q,i) * wm_interm_30_so_pt4(a,k,l,p) * wm_interm_9_so_pt4(l,k,j,i)
term(1253) = term(1253) + r2(vrdav_Rl, a,j,q,i) * wm_interm_30_so_pt4(a,k,l,p) * wm_interm_9_so_pt4(k,l,j,i)
term(1254) = term(1254) + s2(a,q,j,i) * wm_interm_31_so_pt4(a,k,l,p) * wm_interm_61_so_pt4(k,l,j,i)
term(1255) = term(1255) + s2(a,q,j,i) * wm_interm_31_so_pt4(a,k,l,p) * wm_interm_61_so_pt4(l,k,j,i)
term(1256) = term(1256) + s2(a,q,j,i) * wm_interm_30_so_pt4(a,k,l,p) * wm_interm_61_so_pt4(l,k,j,i)
end do 
end do 
end do 
end do 
end do 

term(1246) = term(1246) * (-0.5d+0) 
term(1248) = term(1248) * (-0.5d+0) 
term(1249) = term(1249) * (-0.5d+0) 
term(1251) = term(1251) * (-0.5d+0) 
term(1252) = term(1252) * (-2.0d+0) 
term(1253) = term(1253) * (2.0d+0) 
term(1254) = term(1254) * (-1.0d+0) 
term(1255) = term(1255) * (2.0d+0) 
term(1256) = term(1256) * (-1.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(1257) = term(1257) + s1(a,i) * wm_interm_5_so_pt4(q,a,j,k) * wm_interm_71_so_pt4(p,k,j,i)
term(1258) = term(1258) + s1(a,i) * wm_interm_1_so_pt4(q,a,j,k) * wm_interm_71_so_pt4(p,k,j,i)
term(1259) = term(1259) + s1(a,i) * wm_interm_1_so_pt4(q,a,j,k) * wm_interm_72_so_pt4(p,k,j,i)
term(1260) = term(1260) + s1(a,i) * wm_interm_64_so_pt4(q,a,j,k) * wm_interm_9_so_pt4(k,p,j,i)
term(1261) = term(1261) + s1(a,i) * wm_interm_63_so_pt4(q,a,j,k) * wm_interm_9_so_pt4(p,k,j,i)
term(1262) = term(1262) + s1(a,i) * wm_interm_64_so_pt4(q,a,j,k) * wm_interm_9_so_pt4(p,k,j,i)
term(1263) = term(1263) + s1(a,i) * wm_interm_65_so_pt4(q,a,j,k) * wm_interm_9_so_pt4(k,p,j,i)
term(1264) = term(1264) + s1(a,i) * wm_interm_66_so_pt4(q,a,j,k) * wm_interm_9_so_pt4(k,p,j,i)
term(1265) = term(1265) + s1(a,i) * wm_interm_66_so_pt4(q,a,j,k) * wm_interm_9_so_pt4(p,k,j,i)
term(1266) = term(1266) + s1(a,i) * wm_interm_75_so_pt4(q,a,j,k) * wm_interm_9_so_pt4(k,p,j,i)
term(1267) = term(1267) + s1(a,i) * wm_interm_77_so_pt4(q,a,j,k) * wm_interm_9_so_pt4(k,p,j,i)
term(1268) = term(1268) + s1(a,i) * wm_interm_79_so_pt4(q,a,j,k) * wm_interm_9_so_pt4(k,p,j,i)
term(1269) = term(1269) + s1(a,i) * wm_interm_10_so_pt4(q,a,j,k) * wm_interm_71_so_pt4(p,k,j,i)
term(1270) = term(1270) + s1(a,i) * wm_interm_11_so_pt4(q,a,j,k) * wm_interm_71_so_pt4(p,k,j,i)
term(1271) = term(1271) + s1(a,i) * wm_interm_10_so_pt4(q,a,j,k) * wm_interm_72_so_pt4(p,k,j,i)
term(1272) = term(1272) + s1(a,i) * wm_interm_11_so_pt4(q,a,j,k) * wm_interm_72_so_pt4(p,k,j,i)
term(1273) = term(1273) + r2(vrdav_Rl, a,j,q,i) * wm_interm_7_so_pt4(a,k) * wm_interm_88_so_pt4(j,i,p,k)
term(1274) = term(1274) + r2(vrdav_Rl, a,j,q,i) * wm_interm_7_so_pt4(a,k) * wm_interm_88_so_pt4(i,j,p,k)
term(1275) = term(1275) + r2(vrdav_Rl, a,j,q,i) * wm_interm_6_so_pt4(a,k) * wm_interm_88_so_pt4(j,i,p,k)
term(1276) = term(1276) + r2(vrdav_Rl, a,j,q,i) * wm_interm_6_so_pt4(a,k) * wm_interm_88_so_pt4(i,j,p,k)
term(1277) = term(1277) + s1(a,i) * wm_interm_1_so_pt4(q,a,j,k) * wm_interm_71_so_pt4(k,p,j,i)
term(1278) = term(1278) + s1(a,i) * wm_interm_5_so_pt4(q,a,j,k) * wm_interm_72_so_pt4(k,p,j,i)
term(1279) = term(1279) + s1(a,i) * wm_interm_1_so_pt4(q,a,j,k) * wm_interm_72_so_pt4(k,p,j,i)
term(1280) = term(1280) + s1(a,i) * wm_interm_10_so_pt4(q,a,j,k) * wm_interm_71_so_pt4(k,p,j,i)
term(1281) = term(1281) + s1(a,i) * wm_interm_11_so_pt4(q,a,j,k) * wm_interm_71_so_pt4(k,p,j,i)
term(1282) = term(1282) + s1(a,i) * wm_interm_10_so_pt4(q,a,j,k) * wm_interm_72_so_pt4(k,p,j,i)
term(1283) = term(1283) + s1(a,i) * wm_interm_11_so_pt4(q,a,j,k) * wm_interm_72_so_pt4(k,p,j,i)
term(1284) = term(1284) + s1(a,i) * wm_interm_61_so_pt4(j,p,k,i) * wm_interm_67_so_pt4(a,q,j,k)
term(1285) = term(1285) + s1(a,i) * wm_interm_61_so_pt4(j,p,k,i) * wm_interm_68_so_pt4(a,q,j,k)
term(1286) = term(1286) + s1(a,i) * wm_interm_61_so_pt4(j,p,k,i) * wm_interm_74_so_pt4(a,q,j,k)
term(1287) = term(1287) + s1(a,i) * wm_interm_61_so_pt4(p,j,k,i) * wm_interm_84_so_pt4(a,q,j,k)
term(1288) = term(1288) + s1(a,i) * wm_interm_61_so_pt4(p,j,k,i) * wm_interm_73_so_pt4(a,q,j,k)
term(1289) = term(1289) + s1(a,i) * wm_interm_61_so_pt4(j,p,k,i) * wm_interm_73_so_pt4(a,q,j,k)
term(1290) = term(1290) + s1(a,i) * wm_interm_61_so_pt4(p,j,k,i) * wm_interm_70_so_pt4(a,q,j,k)
term(1291) = term(1291) + s1(a,i) * wm_interm_61_so_pt4(j,p,k,i) * wm_interm_69_so_pt4(a,q,j,k)
term(1292) = term(1292) + s1(a,i) * wm_interm_61_so_pt4(j,p,k,i) * wm_interm_70_so_pt4(a,q,j,k)
term(1293) = term(1293) + s1(a,i) * wm_interm_5_so_pt4(q,a,j,k) * wm_interm_99_so_pt4(p,k,j,i)
term(1294) = term(1294) + s1(a,i) * wm_interm_1_so_pt4(q,a,j,k) * wm_interm_99_so_pt4(p,k,j,i)
term(1295) = term(1295) + s1(a,i) * wm_interm_94_so_pt4(q,a,j,k) * wm_interm_9_so_pt4(k,p,j,i)
term(1296) = term(1296) + s1(a,i) * wm_interm_93_so_pt4(q,a,j,k) * wm_interm_9_so_pt4(p,k,j,i)
term(1297) = term(1297) + s1(a,i) * wm_interm_94_so_pt4(q,a,j,k) * wm_interm_9_so_pt4(p,k,j,i)
term(1298) = term(1298) + s1(a,i) * wm_interm_93_so_pt4(q,a,j,k) * wm_interm_9_so_pt4(k,p,j,i)
term(1299) = term(1299) + s1(a,i) * wm_interm_100_so_pt4(q,a,j,k) * wm_interm_9_so_pt4(k,p,j,i)
term(1300) = term(1300) + s1(a,i) * wm_interm_102_so_pt4(q,a,j,k) * wm_interm_9_so_pt4(k,p,j,i)
term(1301) = term(1301) + s1(a,i) * wm_interm_10_so_pt4(q,a,j,k) * wm_interm_99_so_pt4(p,k,j,i)
term(1302) = term(1302) + s1(a,i) * wm_interm_11_so_pt4(q,a,j,k) * wm_interm_99_so_pt4(p,k,j,i)
term(1303) = term(1303) + r2(vrdav_Rl, a,j,q,i) * wm_interm_109_so_pt4(j,i,p,k) * wm_interm_7_so_pt4(a,k)
term(1304) = term(1304) + r2(vrdav_Rl, a,j,q,i) * wm_interm_109_so_pt4(i,j,p,k) * wm_interm_7_so_pt4(a,k)
term(1305) = term(1305) + r2(vrdav_Rl, a,j,q,i) * wm_interm_109_so_pt4(j,i,p,k) * wm_interm_6_so_pt4(a,k)
term(1306) = term(1306) + r2(vrdav_Rl, a,j,q,i) * wm_interm_109_so_pt4(i,j,p,k) * wm_interm_6_so_pt4(a,k)
term(1307) = term(1307) + s1(a,i) * wm_interm_1_so_pt4(q,a,j,k) * wm_interm_99_so_pt4(k,p,j,i)
term(1308) = term(1308) + s1(a,i) * wm_interm_5_so_pt4(q,a,j,k) * wm_interm_99_so_pt4(k,p,j,i)
term(1309) = term(1309) + s1(a,i) * wm_interm_10_so_pt4(q,a,j,k) * wm_interm_99_so_pt4(k,p,j,i)
term(1310) = term(1310) + s1(a,i) * wm_interm_11_so_pt4(q,a,j,k) * wm_interm_99_so_pt4(k,p,j,i)
term(1311) = term(1311) + s1(a,i) * wm_interm_61_so_pt4(j,p,k,i) * wm_interm_95_so_pt4(a,q,j,k)
term(1312) = term(1312) + s1(a,i) * wm_interm_61_so_pt4(j,p,k,i) * wm_interm_96_so_pt4(a,q,j,k)
term(1313) = term(1313) + s1(a,i) * wm_interm_61_so_pt4(p,j,k,i) * wm_interm_98_so_pt4(a,q,j,k)
term(1314) = term(1314) + s1(a,i) * wm_interm_61_so_pt4(p,j,k,i) * wm_interm_97_so_pt4(a,q,j,k)
term(1315) = term(1315) + s1(a,i) * wm_interm_61_so_pt4(j,p,k,i) * wm_interm_97_so_pt4(a,q,j,k)
term(1316) = term(1316) + s1(a,i) * wm_interm_61_so_pt4(j,p,k,i) * wm_interm_98_so_pt4(a,q,j,k)
end do 
end do 
end do 
end do 

term(1258) = term(1258) * (-2.0d+0) 
term(1260) = term(1260) * (2.0d+0) 
term(1261) = term(1261) * (2.0d+0) 
term(1262) = term(1262) * (-4.0d+0) 
term(1263) = term(1263) * (2.0d+0) 
term(1264) = term(1264) * (-4.0d+0) 
term(1265) = term(1265) * (2.0d+0) 
term(1266) = term(1266) * (2.0d+0) 
term(1267) = term(1267) * (2.0d+0) 
term(1268) = term(1268) * (-4.0d+0) 
term(1269) = term(1269) * (-2.0d+0) 
term(1270) = term(1270) * (4.0d+0) 
term(1272) = term(1272) * (-2.0d+0) 
term(1273) = term(1273) * (2.0d+0) 
term(1274) = term(1274) * (-4.0d+0) 
term(1275) = term(1275) * (-4.0d+0) 
term(1276) = term(1276) * (8.0d+0) 
term(1279) = term(1279) * (-2.0d+0) 
term(1281) = term(1281) * (-2.0d+0) 
term(1282) = term(1282) * (-2.0d+0) 
term(1283) = term(1283) * (4.0d+0) 
term(1285) = term(1285) * (-2.0d+0) 
term(1288) = term(1288) * (-2.0d+0) 
term(1292) = term(1292) * (-2.0d+0) 
term(1293) = term(1293) * (4.0d+0) 
term(1294) = term(1294) * (-4.0d+0) 
term(1295) = term(1295) * (8.0d+0) 
term(1296) = term(1296) * (8.0d+0) 
term(1297) = term(1297) * (-8.0d+0) 
term(1298) = term(1298) * (-8.0d+0) 
term(1299) = term(1299) * (8.0d+0) 
term(1300) = term(1300) * (-8.0d+0) 
term(1301) = term(1301) * (-4.0d+0) 
term(1302) = term(1302) * (8.0d+0) 
term(1303) = term(1303) * (4.0d+0) 
term(1304) = term(1304) * (-8.0d+0) 
term(1305) = term(1305) * (-8.0d+0) 
term(1306) = term(1306) * (16.0d+0) 
term(1307) = term(1307) * (4.0d+0) 
term(1308) = term(1308) * (-4.0d+0) 
term(1309) = term(1309) * (4.0d+0) 
term(1310) = term(1310) * (-8.0d+0) 
term(1311) = term(1311) * (4.0d+0) 
term(1312) = term(1312) * (-4.0d+0) 
term(1313) = term(1313) * (4.0d+0) 
term(1314) = term(1314) * (-4.0d+0) 
term(1315) = term(1315) * (4.0d+0) 
term(1316) = term(1316) * (-4.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(1317) = term(1317) + s1(q,i) * wm_interm_61_so_pt4(j,p,k,l) * wm_interm_88_so_pt4(j,i,l,k)
term(1318) = term(1318) + s1(q,i) * wm_interm_109_so_pt4(j,i,k,l) * wm_interm_61_so_pt4(j,p,l,k)
end do 
end do 
end do 
end do 

term(1317) = term(1317) * (-0.5d+0) 
term(1318) = term(1318) * (-1.0d+0) 

do i = 1, nocc 
do l = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
term(1319) = term(1319) + s1(q,i) * wm_interm_61_so_pt4(j,k,l,i) * wm_interm_88_so_pt4(k,j,l,p)
term(1320) = term(1320) + s1(q,i) * wm_interm_109_so_pt4(j,k,l,p) * wm_interm_61_so_pt4(k,j,l,i)
end do 
end do 
end do 
end do 

term(1320) = term(1320) * (2.0d+0) 

do a = nocc + 1, nactive 
do i = 1, nocc 
term(1321) = term(1321) + r1(vrdav_Rl, a,i) * wm_interm_19_so_pt4(q,a) * wm_interm_8_so_pt4(i,p)
term(1322) = term(1322) + r1(vrdav_Rl, a,i) * wm_interm_20_so_pt4(q,a) * wm_interm_8_so_pt4(i,p)
term(1323) = term(1323) + s1(a,i) * wm_interm_57_so_pt4(q,a) * wm_interm_85_so_pt4(i,p)
term(1324) = term(1324) + s1(a,i) * wm_interm_58_so_pt4(q,a) * wm_interm_85_so_pt4(i,p)
term(1325) = term(1325) + s1(a,i) * wm_interm_57_so_pt4(q,a) * wm_interm_86_so_pt4(i,p)
term(1326) = term(1326) + s1(a,i) * wm_interm_58_so_pt4(q,a) * wm_interm_86_so_pt4(i,p)
term(1327) = term(1327) + s1(a,i) * wm_interm_57_so_pt4(q,a) * wm_interm_87_so_pt4(i,p)
term(1328) = term(1328) + s1(a,i) * wm_interm_58_so_pt4(q,a) * wm_interm_87_so_pt4(i,p)
term(1329) = term(1329) + s1(a,i) * wm_interm_106_so_pt4(i,p) * wm_interm_57_so_pt4(q,a)
term(1330) = term(1330) + s1(a,i) * wm_interm_106_so_pt4(i,p) * wm_interm_58_so_pt4(q,a)
term(1331) = term(1331) + s1(a,i) * wm_interm_107_so_pt4(i,p) * wm_interm_57_so_pt4(q,a)
term(1332) = term(1332) + s1(a,i) * wm_interm_107_so_pt4(i,p) * wm_interm_58_so_pt4(q,a)
term(1333) = term(1333) + s1(a,i) * wm_interm_108_so_pt4(i,p) * wm_interm_57_so_pt4(q,a)
term(1334) = term(1334) + s1(a,i) * wm_interm_108_so_pt4(i,p) * wm_interm_58_so_pt4(q,a)
end do 
end do 

term(1321) = term(1321) * (2.0d+0) 
term(1322) = term(1322) * (-4.0d+0) 
term(1323) = term(1323) * (2.0d+0) 
term(1324) = term(1324) * (-1.0d+0) 
term(1325) = term(1325) * (-4.0d+0) 
term(1326) = term(1326) * (2.0d+0) 
term(1327) = term(1327) * (2.0d+0) 
term(1328) = term(1328) * (-1.0d+0) 
term(1329) = term(1329) * (4.0d+0) 
term(1330) = term(1330) * (-2.0d+0) 
term(1331) = term(1331) * (-8.0d+0) 
term(1332) = term(1332) * (4.0d+0) 
term(1333) = term(1333) * (4.0d+0) 
term(1334) = term(1334) * (-2.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(1335) = term(1335) + s1(a,i) * wm_interm_12_so_pt4(q,i,j,k) * wm_interm_13_so_pt4(a,p,j,k)
term(1336) = term(1336) + s1(a,p) * wm_interm_12_so_pt4(q,i,j,k) * wm_interm_13_so_pt4(a,i,k,j)
term(1337) = term(1337) + s1(a,i) * wm_interm_5_so_pt4(q,a,j,k) * wm_interm_71_so_pt4(p,k,i,j)
term(1338) = term(1338) + s1(a,i) * wm_interm_1_so_pt4(q,a,j,k) * wm_interm_71_so_pt4(p,k,i,j)
term(1339) = term(1339) + s1(a,i) * wm_interm_5_so_pt4(q,a,j,k) * wm_interm_72_so_pt4(p,k,i,j)
term(1340) = term(1340) + s1(a,i) * wm_interm_10_so_pt4(q,a,j,k) * wm_interm_71_so_pt4(p,k,i,j)
term(1341) = term(1341) + s1(a,i) * wm_interm_11_so_pt4(q,a,j,k) * wm_interm_71_so_pt4(p,k,i,j)
term(1342) = term(1342) + s1(a,i) * wm_interm_5_so_pt4(q,a,j,k) * wm_interm_71_so_pt4(k,p,i,j)
term(1343) = term(1343) + s1(a,i) * wm_interm_5_so_pt4(q,a,j,k) * wm_interm_72_so_pt4(k,p,i,j)
term(1344) = term(1344) + s1(a,i) * wm_interm_1_so_pt4(q,a,j,k) * wm_interm_72_so_pt4(k,p,i,j)
term(1345) = term(1345) + s1(a,i) * wm_interm_10_so_pt4(q,a,j,k) * wm_interm_72_so_pt4(k,p,i,j)
term(1346) = term(1346) + s1(a,i) * wm_interm_11_so_pt4(q,a,j,k) * wm_interm_72_so_pt4(k,p,i,j)
term(1347) = term(1347) + s1(a,i) * wm_interm_61_so_pt4(p,j,i,k) * wm_interm_67_so_pt4(a,q,j,k)
term(1348) = term(1348) + s1(a,i) * wm_interm_61_so_pt4(p,j,i,k) * wm_interm_68_so_pt4(a,q,j,k)
term(1349) = term(1349) + s1(a,i) * wm_interm_61_so_pt4(p,j,i,k) * wm_interm_69_so_pt4(a,q,j,k)
term(1350) = term(1350) + s1(a,i) * wm_interm_61_so_pt4(p,j,i,k) * wm_interm_70_so_pt4(a,q,j,k)
term(1351) = term(1351) + s1(a,i) * wm_interm_61_so_pt4(j,p,i,k) * wm_interm_70_so_pt4(a,q,j,k)
term(1352) = term(1352) + s1(a,i) * wm_interm_61_so_pt4(p,j,i,k) * wm_interm_73_so_pt4(a,q,j,k)
term(1353) = term(1353) + s1(a,i) * wm_interm_61_so_pt4(p,j,i,k) * wm_interm_74_so_pt4(a,q,j,k)
term(1354) = term(1354) + s1(a,i) * wm_interm_61_so_pt4(j,p,i,k) * wm_interm_84_so_pt4(a,q,j,k)
term(1355) = term(1355) + s1(a,i) * wm_interm_61_so_pt4(j,p,i,k) * wm_interm_73_so_pt4(a,q,j,k)
term(1356) = term(1356) + s1(a,i) * wm_interm_5_so_pt4(q,a,j,k) * wm_interm_99_so_pt4(p,k,i,j)
term(1357) = term(1357) + s1(a,i) * wm_interm_1_so_pt4(q,a,j,k) * wm_interm_99_so_pt4(p,k,i,j)
term(1358) = term(1358) + s1(a,i) * wm_interm_10_so_pt4(q,a,j,k) * wm_interm_99_so_pt4(p,k,i,j)
term(1359) = term(1359) + s1(a,i) * wm_interm_11_so_pt4(q,a,j,k) * wm_interm_99_so_pt4(p,k,i,j)
term(1360) = term(1360) + s1(a,i) * wm_interm_5_so_pt4(q,a,j,k) * wm_interm_99_so_pt4(k,p,i,j)
term(1361) = term(1361) + s1(a,i) * wm_interm_1_so_pt4(q,a,j,k) * wm_interm_99_so_pt4(k,p,i,j)
term(1362) = term(1362) + s1(a,i) * wm_interm_10_so_pt4(q,a,j,k) * wm_interm_99_so_pt4(k,p,i,j)
term(1363) = term(1363) + s1(a,i) * wm_interm_11_so_pt4(q,a,j,k) * wm_interm_99_so_pt4(k,p,i,j)
term(1364) = term(1364) + s1(a,i) * wm_interm_61_so_pt4(p,j,i,k) * wm_interm_95_so_pt4(a,q,j,k)
term(1365) = term(1365) + s1(a,i) * wm_interm_61_so_pt4(p,j,i,k) * wm_interm_96_so_pt4(a,q,j,k)
term(1366) = term(1366) + s1(a,i) * wm_interm_61_so_pt4(p,j,i,k) * wm_interm_97_so_pt4(a,q,j,k)
term(1367) = term(1367) + s1(a,i) * wm_interm_61_so_pt4(p,j,i,k) * wm_interm_98_so_pt4(a,q,j,k)
term(1368) = term(1368) + s1(a,i) * wm_interm_61_so_pt4(j,p,i,k) * wm_interm_98_so_pt4(a,q,j,k)
term(1369) = term(1369) + s1(a,i) * wm_interm_61_so_pt4(j,p,i,k) * wm_interm_97_so_pt4(a,q,j,k)
end do 
end do 
end do 
end do 

term(1335) = term(1335) * (-4.0d+0) 
term(1336) = term(1336) * (-4.0d+0) 
term(1337) = term(1337) * (-2.0d+0) 
term(1341) = term(1341) * (-2.0d+0) 
term(1343) = term(1343) * (-2.0d+0) 
term(1346) = term(1346) * (-2.0d+0) 
term(1348) = term(1348) * (-2.0d+0) 
term(1350) = term(1350) * (-2.0d+0) 
term(1355) = term(1355) * (-2.0d+0) 
term(1356) = term(1356) * (-4.0d+0) 
term(1357) = term(1357) * (4.0d+0) 
term(1358) = term(1358) * (4.0d+0) 
term(1359) = term(1359) * (-8.0d+0) 
term(1360) = term(1360) * (4.0d+0) 
term(1361) = term(1361) * (-4.0d+0) 
term(1362) = term(1362) * (-4.0d+0) 
term(1363) = term(1363) * (8.0d+0) 
term(1364) = term(1364) * (4.0d+0) 
term(1365) = term(1365) * (-4.0d+0) 
term(1366) = term(1366) * (4.0d+0) 
term(1367) = term(1367) * (-4.0d+0) 
term(1368) = term(1368) * (4.0d+0) 
term(1369) = term(1369) * (-4.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(1370) = term(1370) + s2(a,b,i,p) * wm_interm_33_so_pt4(a,j) * wm_interm_54_so_pt4(q,b,j,i)
term(1371) = term(1371) + s2(a,b,p,i) * wm_interm_33_so_pt4(a,j) * wm_interm_54_so_pt4(q,b,j,i)
term(1372) = term(1372) + s2(a,b,i,p) * wm_interm_34_so_pt4(a,j) * wm_interm_54_so_pt4(q,b,j,i)
term(1373) = term(1373) + s2(a,b,p,i) * wm_interm_34_so_pt4(a,j) * wm_interm_54_so_pt4(q,b,j,i)
term(1374) = term(1374) + s2(a,b,i,p) * wm_interm_35_so_pt4(a,j) * wm_interm_54_so_pt4(q,b,j,i)
term(1375) = term(1375) + s2(a,b,p,i) * wm_interm_35_so_pt4(a,j) * wm_interm_54_so_pt4(q,b,j,i)
term(1376) = term(1376) + s2(a,b,i,p) * wm_interm_33_so_pt4(a,j) * wm_interm_55_so_pt4(q,b,j,i)
term(1377) = term(1377) + s2(a,b,p,i) * wm_interm_33_so_pt4(a,j) * wm_interm_55_so_pt4(q,b,j,i)
term(1378) = term(1378) + s2(a,b,i,p) * wm_interm_34_so_pt4(a,j) * wm_interm_55_so_pt4(q,b,j,i)
term(1379) = term(1379) + s2(a,b,p,i) * wm_interm_34_so_pt4(a,j) * wm_interm_55_so_pt4(q,b,j,i)
term(1380) = term(1380) + s2(a,b,i,p) * wm_interm_35_so_pt4(a,j) * wm_interm_55_so_pt4(q,b,j,i)
term(1381) = term(1381) + s2(a,b,p,i) * wm_interm_35_so_pt4(a,j) * wm_interm_55_so_pt4(q,b,j,i)
term(1382) = term(1382) + s2(a,b,p,i) * wm_interm_33_so_pt4(a,j) * wm_interm_56_so_pt4(q,b,j,i)
term(1383) = term(1383) + s2(a,b,i,p) * wm_interm_33_so_pt4(a,j) * wm_interm_59_so_pt4(q,b,j,i)
term(1384) = term(1384) + s2(a,b,p,i) * wm_interm_33_so_pt4(a,j) * wm_interm_59_so_pt4(q,b,j,i)
term(1385) = term(1385) + s2(a,b,p,i) * wm_interm_34_so_pt4(a,j) * wm_interm_56_so_pt4(q,b,j,i)
term(1386) = term(1386) + s2(a,b,i,p) * wm_interm_34_so_pt4(a,j) * wm_interm_59_so_pt4(q,b,j,i)
term(1387) = term(1387) + s2(a,b,p,i) * wm_interm_34_so_pt4(a,j) * wm_interm_59_so_pt4(q,b,j,i)
term(1388) = term(1388) + s2(a,b,p,i) * wm_interm_35_so_pt4(a,j) * wm_interm_56_so_pt4(q,b,j,i)
term(1389) = term(1389) + s2(a,b,i,p) * wm_interm_35_so_pt4(a,j) * wm_interm_59_so_pt4(q,b,j,i)
term(1390) = term(1390) + s2(a,b,p,i) * wm_interm_35_so_pt4(a,j) * wm_interm_59_so_pt4(q,b,j,i)
term(1391) = term(1391) + s2(a,b,i,p) * wm_interm_36_so_pt4(a,j) * wm_interm_54_so_pt4(q,b,j,i)
term(1392) = term(1392) + s2(a,b,p,i) * wm_interm_36_so_pt4(a,j) * wm_interm_54_so_pt4(q,b,j,i)
term(1393) = term(1393) + s2(a,b,i,p) * wm_interm_37_so_pt4(a,j) * wm_interm_54_so_pt4(q,b,j,i)
term(1394) = term(1394) + s2(a,b,p,i) * wm_interm_37_so_pt4(a,j) * wm_interm_54_so_pt4(q,b,j,i)
term(1395) = term(1395) + s2(a,b,i,p) * wm_interm_36_so_pt4(a,j) * wm_interm_55_so_pt4(q,b,j,i)
term(1396) = term(1396) + s2(a,b,p,i) * wm_interm_36_so_pt4(a,j) * wm_interm_55_so_pt4(q,b,j,i)
term(1397) = term(1397) + s2(a,b,i,p) * wm_interm_37_so_pt4(a,j) * wm_interm_55_so_pt4(q,b,j,i)
term(1398) = term(1398) + s2(a,b,p,i) * wm_interm_37_so_pt4(a,j) * wm_interm_55_so_pt4(q,b,j,i)
term(1399) = term(1399) + s2(a,b,p,i) * wm_interm_36_so_pt4(a,j) * wm_interm_56_so_pt4(q,b,j,i)
term(1400) = term(1400) + s2(a,b,i,p) * wm_interm_36_so_pt4(a,j) * wm_interm_59_so_pt4(q,b,j,i)
term(1401) = term(1401) + s2(a,b,p,i) * wm_interm_36_so_pt4(a,j) * wm_interm_59_so_pt4(q,b,j,i)
term(1402) = term(1402) + s2(a,b,p,i) * wm_interm_37_so_pt4(a,j) * wm_interm_56_so_pt4(q,b,j,i)
term(1403) = term(1403) + s2(a,b,i,p) * wm_interm_37_so_pt4(a,j) * wm_interm_59_so_pt4(q,b,j,i)
term(1404) = term(1404) + s2(a,b,p,i) * wm_interm_37_so_pt4(a,j) * wm_interm_59_so_pt4(q,b,j,i)
term(1405) = term(1405) + s2(a,b,p,i) * wm_interm_63_so_pt4(q,b,j,i) * wm_interm_6_so_pt4(a,j)
term(1406) = term(1406) + s2(a,b,p,i) * wm_interm_64_so_pt4(q,b,j,i) * wm_interm_6_so_pt4(a,j)
term(1407) = term(1407) + s2(a,b,p,i) * wm_interm_66_so_pt4(q,b,j,i) * wm_interm_6_so_pt4(a,j)
term(1408) = term(1408) + s2(a,b,i,p) * wm_interm_64_so_pt4(q,b,j,i) * wm_interm_6_so_pt4(a,j)
term(1409) = term(1409) + s2(a,b,i,p) * wm_interm_65_so_pt4(q,b,j,i) * wm_interm_6_so_pt4(a,j)
term(1410) = term(1410) + s2(a,b,i,p) * wm_interm_66_so_pt4(q,b,j,i) * wm_interm_6_so_pt4(a,j)
term(1411) = term(1411) + s2(a,b,i,p) * wm_interm_6_so_pt4(a,j) * wm_interm_75_so_pt4(q,b,j,i)
term(1412) = term(1412) + s2(a,b,i,p) * wm_interm_6_so_pt4(a,j) * wm_interm_77_so_pt4(q,b,j,i)
term(1413) = term(1413) + s2(a,b,i,p) * wm_interm_6_so_pt4(a,j) * wm_interm_79_so_pt4(q,b,j,i)
term(1414) = term(1414) + s2(a,b,p,i) * wm_interm_63_so_pt4(q,b,j,i) * wm_interm_7_so_pt4(a,j)
term(1415) = term(1415) + s2(a,b,p,i) * wm_interm_64_so_pt4(q,b,j,i) * wm_interm_7_so_pt4(a,j)
term(1416) = term(1416) + s2(a,b,i,p) * wm_interm_64_so_pt4(q,b,j,i) * wm_interm_7_so_pt4(a,j)
term(1417) = term(1417) + s2(a,b,p,i) * wm_interm_66_so_pt4(q,b,j,i) * wm_interm_7_so_pt4(a,j)
term(1418) = term(1418) + s2(a,b,i,p) * wm_interm_65_so_pt4(q,b,j,i) * wm_interm_7_so_pt4(a,j)
term(1419) = term(1419) + s2(a,b,i,p) * wm_interm_66_so_pt4(q,b,j,i) * wm_interm_7_so_pt4(a,j)
term(1420) = term(1420) + s2(a,b,i,p) * wm_interm_75_so_pt4(q,b,j,i) * wm_interm_7_so_pt4(a,j)
term(1421) = term(1421) + s2(a,b,i,p) * wm_interm_77_so_pt4(q,b,j,i) * wm_interm_7_so_pt4(a,j)
term(1422) = term(1422) + s2(a,b,i,p) * wm_interm_79_so_pt4(q,b,j,i) * wm_interm_7_so_pt4(a,j)
term(1423) = term(1423) + s2(a,b,p,i) * wm_interm_6_so_pt4(a,j) * wm_interm_93_so_pt4(q,b,j,i)
term(1424) = term(1424) + s2(a,b,p,i) * wm_interm_6_so_pt4(a,j) * wm_interm_94_so_pt4(q,b,j,i)
term(1425) = term(1425) + s2(a,b,i,p) * wm_interm_6_so_pt4(a,j) * wm_interm_94_so_pt4(q,b,j,i)
term(1426) = term(1426) + s2(a,b,i,p) * wm_interm_6_so_pt4(a,j) * wm_interm_93_so_pt4(q,b,j,i)
term(1427) = term(1427) + s2(a,b,i,p) * wm_interm_100_so_pt4(q,b,j,i) * wm_interm_6_so_pt4(a,j)
term(1428) = term(1428) + s2(a,b,i,p) * wm_interm_102_so_pt4(q,b,j,i) * wm_interm_6_so_pt4(a,j)
term(1429) = term(1429) + s2(a,b,p,i) * wm_interm_7_so_pt4(a,j) * wm_interm_93_so_pt4(q,b,j,i)
term(1430) = term(1430) + s2(a,b,p,i) * wm_interm_7_so_pt4(a,j) * wm_interm_94_so_pt4(q,b,j,i)
term(1431) = term(1431) + s2(a,b,i,p) * wm_interm_7_so_pt4(a,j) * wm_interm_94_so_pt4(q,b,j,i)
term(1432) = term(1432) + s2(a,b,i,p) * wm_interm_7_so_pt4(a,j) * wm_interm_93_so_pt4(q,b,j,i)
term(1433) = term(1433) + s2(a,b,i,p) * wm_interm_100_so_pt4(q,b,j,i) * wm_interm_7_so_pt4(a,j)
term(1434) = term(1434) + s2(a,b,i,p) * wm_interm_102_so_pt4(q,b,j,i) * wm_interm_7_so_pt4(a,j)
end do 
end do 
end do 
end do 

term(1370) = term(1370) * (4.0d+0) 
term(1371) = term(1371) * (-8.0d+0) 
term(1372) = term(1372) * (-2.0d+0) 
term(1373) = term(1373) * (4.0d+0) 
term(1374) = term(1374) * (-2.0d+0) 
term(1375) = term(1375) * (4.0d+0) 
term(1376) = term(1376) * (-2.0d+0) 
term(1377) = term(1377) * (4.0d+0) 
term(1379) = term(1379) * (-2.0d+0) 
term(1381) = term(1381) * (-2.0d+0) 
term(1382) = term(1382) * (-2.0d+0) 
term(1383) = term(1383) * (-2.0d+0) 
term(1384) = term(1384) * (4.0d+0) 
term(1387) = term(1387) * (-2.0d+0) 
term(1390) = term(1390) * (-2.0d+0) 
term(1391) = term(1391) * (8.0d+0) 
term(1392) = term(1392) * (-16.0d+0) 
term(1393) = term(1393) * (-8.0d+0) 
term(1394) = term(1394) * (16.0d+0) 
term(1395) = term(1395) * (-4.0d+0) 
term(1396) = term(1396) * (8.0d+0) 
term(1397) = term(1397) * (4.0d+0) 
term(1398) = term(1398) * (-8.0d+0) 
term(1399) = term(1399) * (-4.0d+0) 
term(1400) = term(1400) * (-4.0d+0) 
term(1401) = term(1401) * (8.0d+0) 
term(1402) = term(1402) * (4.0d+0) 
term(1403) = term(1403) * (4.0d+0) 
term(1404) = term(1404) * (-8.0d+0) 
term(1405) = term(1405) * (-4.0d+0) 
term(1406) = term(1406) * (8.0d+0) 
term(1407) = term(1407) * (-4.0d+0) 
term(1408) = term(1408) * (-4.0d+0) 
term(1409) = term(1409) * (-4.0d+0) 
term(1410) = term(1410) * (8.0d+0) 
term(1411) = term(1411) * (-4.0d+0) 
term(1412) = term(1412) * (-4.0d+0) 
term(1413) = term(1413) * (8.0d+0) 
term(1414) = term(1414) * (2.0d+0) 
term(1415) = term(1415) * (-4.0d+0) 
term(1416) = term(1416) * (2.0d+0) 
term(1417) = term(1417) * (2.0d+0) 
term(1418) = term(1418) * (2.0d+0) 
term(1419) = term(1419) * (-4.0d+0) 
term(1420) = term(1420) * (2.0d+0) 
term(1421) = term(1421) * (2.0d+0) 
term(1422) = term(1422) * (-4.0d+0) 
term(1423) = term(1423) * (-16.0d+0) 
term(1424) = term(1424) * (16.0d+0) 
term(1425) = term(1425) * (-16.0d+0) 
term(1426) = term(1426) * (16.0d+0) 
term(1427) = term(1427) * (-16.0d+0) 
term(1428) = term(1428) * (16.0d+0) 
term(1429) = term(1429) * (8.0d+0) 
term(1430) = term(1430) * (-8.0d+0) 
term(1431) = term(1431) * (8.0d+0) 
term(1432) = term(1432) * (-8.0d+0) 
term(1433) = term(1433) * (8.0d+0) 
term(1434) = term(1434) * (-8.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
do j = 1, nocc 
do b = nocc + 1, nactive 
term(1435) = term(1435) + s2(a,q,j,i) * wm_interm_33_so_pt4(b,j) * wm_interm_54_so_pt4(b,a,p,i)
term(1436) = term(1436) + s2(a,q,j,i) * wm_interm_34_so_pt4(b,j) * wm_interm_54_so_pt4(b,a,p,i)
term(1437) = term(1437) + s2(a,q,j,i) * wm_interm_35_so_pt4(b,j) * wm_interm_54_so_pt4(b,a,p,i)
term(1438) = term(1438) + s2(a,q,j,i) * wm_interm_33_so_pt4(b,j) * wm_interm_55_so_pt4(b,a,p,i)
term(1439) = term(1439) + s2(a,q,j,i) * wm_interm_34_so_pt4(b,j) * wm_interm_55_so_pt4(b,a,p,i)
term(1440) = term(1440) + s2(a,q,j,i) * wm_interm_35_so_pt4(b,j) * wm_interm_55_so_pt4(b,a,p,i)
term(1441) = term(1441) + s2(a,q,j,i) * wm_interm_33_so_pt4(b,j) * wm_interm_59_so_pt4(b,a,p,i)
term(1442) = term(1442) + s2(a,q,j,i) * wm_interm_34_so_pt4(b,j) * wm_interm_59_so_pt4(b,a,p,i)
term(1443) = term(1443) + s2(a,q,j,i) * wm_interm_35_so_pt4(b,j) * wm_interm_59_so_pt4(b,a,p,i)
term(1444) = term(1444) + s2(a,q,j,i) * wm_interm_36_so_pt4(b,j) * wm_interm_54_so_pt4(b,a,p,i)
term(1445) = term(1445) + s2(a,q,j,i) * wm_interm_37_so_pt4(b,j) * wm_interm_54_so_pt4(b,a,p,i)
term(1446) = term(1446) + s2(a,q,j,i) * wm_interm_36_so_pt4(b,j) * wm_interm_55_so_pt4(b,a,p,i)
term(1447) = term(1447) + s2(a,q,j,i) * wm_interm_37_so_pt4(b,j) * wm_interm_55_so_pt4(b,a,p,i)
term(1448) = term(1448) + s2(a,q,j,i) * wm_interm_36_so_pt4(b,j) * wm_interm_59_so_pt4(b,a,p,i)
term(1449) = term(1449) + s2(a,q,j,i) * wm_interm_37_so_pt4(b,j) * wm_interm_59_so_pt4(b,a,p,i)
term(1450) = term(1450) + s2(a,q,j,i) * wm_interm_64_so_pt4(b,a,p,i) * wm_interm_7_so_pt4(b,j)
term(1451) = term(1451) + s2(a,q,j,i) * wm_interm_63_so_pt4(b,a,p,i) * wm_interm_7_so_pt4(b,j)
term(1452) = term(1452) + s2(a,q,j,i) * wm_interm_65_so_pt4(b,a,p,i) * wm_interm_7_so_pt4(b,j)
term(1453) = term(1453) + s2(a,q,j,i) * wm_interm_75_so_pt4(b,a,p,i) * wm_interm_7_so_pt4(b,j)
term(1454) = term(1454) + s2(a,q,j,i) * wm_interm_77_so_pt4(b,a,p,i) * wm_interm_7_so_pt4(b,j)
term(1455) = term(1455) + s2(a,q,j,i) * wm_interm_79_so_pt4(b,a,p,i) * wm_interm_7_so_pt4(b,j)
term(1456) = term(1456) + s2(a,q,j,i) * wm_interm_64_so_pt4(b,a,p,i) * wm_interm_6_so_pt4(b,j)
term(1457) = term(1457) + s2(a,q,j,i) * wm_interm_63_so_pt4(b,a,p,i) * wm_interm_6_so_pt4(b,j)
term(1458) = term(1458) + s2(a,q,j,i) * wm_interm_65_so_pt4(b,a,p,i) * wm_interm_6_so_pt4(b,j)
term(1459) = term(1459) + s2(a,q,j,i) * wm_interm_6_so_pt4(b,j) * wm_interm_75_so_pt4(b,a,p,i)
term(1460) = term(1460) + s2(a,q,j,i) * wm_interm_6_so_pt4(b,j) * wm_interm_77_so_pt4(b,a,p,i)
term(1461) = term(1461) + s2(a,q,j,i) * wm_interm_6_so_pt4(b,j) * wm_interm_79_so_pt4(b,a,p,i)
term(1462) = term(1462) + s2(a,q,j,i) * wm_interm_7_so_pt4(b,j) * wm_interm_94_so_pt4(b,a,p,i)
term(1463) = term(1463) + s2(a,q,j,i) * wm_interm_7_so_pt4(b,j) * wm_interm_93_so_pt4(b,a,p,i)
term(1464) = term(1464) + s2(a,q,j,i) * wm_interm_100_so_pt4(b,a,p,i) * wm_interm_7_so_pt4(b,j)
term(1465) = term(1465) + s2(a,q,j,i) * wm_interm_102_so_pt4(b,a,p,i) * wm_interm_7_so_pt4(b,j)
term(1466) = term(1466) + s2(a,q,j,i) * wm_interm_6_so_pt4(b,j) * wm_interm_94_so_pt4(b,a,p,i)
term(1467) = term(1467) + s2(a,q,j,i) * wm_interm_6_so_pt4(b,j) * wm_interm_93_so_pt4(b,a,p,i)
term(1468) = term(1468) + s2(a,q,j,i) * wm_interm_100_so_pt4(b,a,p,i) * wm_interm_6_so_pt4(b,j)
term(1469) = term(1469) + s2(a,q,j,i) * wm_interm_102_so_pt4(b,a,p,i) * wm_interm_6_so_pt4(b,j)
term(1470) = term(1470) + r2(vrdav_Rl, a,j,q,i) * wm_interm_19_so_pt4(b,a) * wm_interm_28_so_pt4(b,j,i,p)
term(1471) = term(1471) + r2(vrdav_Rl, a,j,q,i) * wm_interm_20_so_pt4(b,a) * wm_interm_28_so_pt4(b,j,i,p)
term(1472) = term(1472) + s2(a,q,j,i) * wm_interm_28_so_pt4(b,j,i,p) * wm_interm_57_so_pt4(b,a)
term(1473) = term(1473) + s2(a,q,j,i) * wm_interm_28_so_pt4(b,j,i,p) * wm_interm_58_so_pt4(b,a)
term(1474) = term(1474) + r2(vrdav_Rl, a,j,q,i) * wm_interm_19_so_pt4(b,a) * wm_interm_31_so_pt4(b,j,i,p)
term(1475) = term(1475) + r2(vrdav_Rl, a,j,q,i) * wm_interm_20_so_pt4(b,a) * wm_interm_31_so_pt4(b,j,i,p)
term(1476) = term(1476) + s2(a,q,j,i) * wm_interm_30_so_pt4(b,j,i,p) * wm_interm_57_so_pt4(b,a)
term(1477) = term(1477) + s2(a,q,j,i) * wm_interm_30_so_pt4(b,j,i,p) * wm_interm_58_so_pt4(b,a)
end do 
end do 
end do 
end do 

term(1435) = term(1435) * (4.0d+0) 
term(1436) = term(1436) * (-2.0d+0) 
term(1437) = term(1437) * (-2.0d+0) 
term(1438) = term(1438) * (-2.0d+0) 
term(1441) = term(1441) * (-2.0d+0) 
term(1444) = term(1444) * (8.0d+0) 
term(1445) = term(1445) * (-8.0d+0) 
term(1446) = term(1446) * (-4.0d+0) 
term(1447) = term(1447) * (4.0d+0) 
term(1448) = term(1448) * (-4.0d+0) 
term(1449) = term(1449) * (4.0d+0) 
term(1450) = term(1450) * (2.0d+0) 
term(1451) = term(1451) * (-4.0d+0) 
term(1452) = term(1452) * (2.0d+0) 
term(1453) = term(1453) * (2.0d+0) 
term(1454) = term(1454) * (2.0d+0) 
term(1455) = term(1455) * (-4.0d+0) 
term(1456) = term(1456) * (-4.0d+0) 
term(1457) = term(1457) * (8.0d+0) 
term(1458) = term(1458) * (-4.0d+0) 
term(1459) = term(1459) * (-4.0d+0) 
term(1460) = term(1460) * (-4.0d+0) 
term(1461) = term(1461) * (8.0d+0) 
term(1462) = term(1462) * (8.0d+0) 
term(1463) = term(1463) * (-8.0d+0) 
term(1464) = term(1464) * (8.0d+0) 
term(1465) = term(1465) * (-8.0d+0) 
term(1466) = term(1466) * (-16.0d+0) 
term(1467) = term(1467) * (16.0d+0) 
term(1468) = term(1468) * (-16.0d+0) 
term(1469) = term(1469) * (16.0d+0) 
term(1470) = term(1470) * (-1.0d+0) 
term(1471) = term(1471) * (2.0d+0) 
term(1472) = term(1472) * (2.0d+0) 
term(1473) = term(1473) * (-1.0d+0) 
term(1474) = term(1474) * (-2.0d+0) 
term(1475) = term(1475) * (4.0d+0) 
term(1476) = term(1476) * (-8.0d+0) 
term(1477) = term(1477) * (4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(1478) = term(1478) + s2(a,q,j,i) * wm_interm_33_so_pt4(b,i) * wm_interm_54_so_pt4(b,a,p,j)
term(1479) = term(1479) + s2(a,q,j,i) * wm_interm_34_so_pt4(b,i) * wm_interm_54_so_pt4(b,a,p,j)
term(1480) = term(1480) + s2(a,q,j,i) * wm_interm_35_so_pt4(b,i) * wm_interm_54_so_pt4(b,a,p,j)
term(1481) = term(1481) + s2(a,q,j,i) * wm_interm_33_so_pt4(b,i) * wm_interm_55_so_pt4(b,a,p,j)
term(1482) = term(1482) + s2(a,q,j,i) * wm_interm_34_so_pt4(b,i) * wm_interm_55_so_pt4(b,a,p,j)
term(1483) = term(1483) + s2(a,q,j,i) * wm_interm_35_so_pt4(b,i) * wm_interm_55_so_pt4(b,a,p,j)
term(1484) = term(1484) + s2(a,q,j,i) * wm_interm_33_so_pt4(b,i) * wm_interm_56_so_pt4(b,a,p,j)
term(1485) = term(1485) + s2(a,q,j,i) * wm_interm_33_so_pt4(b,i) * wm_interm_59_so_pt4(b,a,p,j)
term(1486) = term(1486) + s2(a,q,j,i) * wm_interm_34_so_pt4(b,i) * wm_interm_56_so_pt4(b,a,p,j)
term(1487) = term(1487) + s2(a,q,j,i) * wm_interm_34_so_pt4(b,i) * wm_interm_59_so_pt4(b,a,p,j)
term(1488) = term(1488) + s2(a,q,j,i) * wm_interm_35_so_pt4(b,i) * wm_interm_56_so_pt4(b,a,p,j)
term(1489) = term(1489) + s2(a,q,j,i) * wm_interm_35_so_pt4(b,i) * wm_interm_59_so_pt4(b,a,p,j)
term(1490) = term(1490) + s2(a,q,j,i) * wm_interm_36_so_pt4(b,i) * wm_interm_54_so_pt4(b,a,p,j)
term(1491) = term(1491) + s2(a,q,j,i) * wm_interm_37_so_pt4(b,i) * wm_interm_54_so_pt4(b,a,p,j)
term(1492) = term(1492) + s2(a,q,j,i) * wm_interm_36_so_pt4(b,i) * wm_interm_55_so_pt4(b,a,p,j)
term(1493) = term(1493) + s2(a,q,j,i) * wm_interm_37_so_pt4(b,i) * wm_interm_55_so_pt4(b,a,p,j)
term(1494) = term(1494) + s2(a,q,j,i) * wm_interm_36_so_pt4(b,i) * wm_interm_56_so_pt4(b,a,p,j)
term(1495) = term(1495) + s2(a,q,j,i) * wm_interm_36_so_pt4(b,i) * wm_interm_59_so_pt4(b,a,p,j)
term(1496) = term(1496) + s2(a,q,j,i) * wm_interm_37_so_pt4(b,i) * wm_interm_56_so_pt4(b,a,p,j)
term(1497) = term(1497) + s2(a,q,j,i) * wm_interm_37_so_pt4(b,i) * wm_interm_59_so_pt4(b,a,p,j)
term(1498) = term(1498) + s2(a,q,j,i) * wm_interm_63_so_pt4(b,a,p,j) * wm_interm_7_so_pt4(b,i)
term(1499) = term(1499) + s2(a,q,j,i) * wm_interm_64_so_pt4(b,a,p,j) * wm_interm_7_so_pt4(b,i)
term(1500) = term(1500) + s1(a,i) * wm_interm_54_so_pt4(b,a,p,j) * wm_interm_67_so_pt4(b,q,i,j)
term(1501) = term(1501) + s1(a,i) * wm_interm_54_so_pt4(b,a,p,j) * wm_interm_68_so_pt4(b,q,i,j)
term(1502) = term(1502) + s1(a,i) * wm_interm_54_so_pt4(b,a,p,j) * wm_interm_69_so_pt4(b,q,i,j)
term(1503) = term(1503) + s1(a,i) * wm_interm_54_so_pt4(b,a,p,j) * wm_interm_70_so_pt4(b,q,i,j)
term(1504) = term(1504) + s2(a,q,j,i) * wm_interm_66_so_pt4(b,a,p,j) * wm_interm_7_so_pt4(b,i)
term(1505) = term(1505) + s1(a,i) * wm_interm_54_so_pt4(b,a,p,j) * wm_interm_73_so_pt4(b,q,i,j)
term(1506) = term(1506) + s1(a,i) * wm_interm_54_so_pt4(b,a,p,j) * wm_interm_74_so_pt4(b,q,i,j)
term(1507) = term(1507) + s2(a,q,j,i) * wm_interm_89_so_pt4(b,p,i,j) * wm_interm_90_so_pt4(b,a)
term(1508) = term(1508) + s1(a,p) * wm_interm_54_so_pt4(b,a,i,j) * wm_interm_70_so_pt4(b,q,i,j)
term(1509) = term(1509) + s2(a,q,j,i) * wm_interm_89_so_pt4(b,p,i,j) * wm_interm_91_so_pt4(b,a)
term(1510) = term(1510) + s1(a,p) * wm_interm_54_so_pt4(b,a,i,j) * wm_interm_84_so_pt4(b,q,i,j)
term(1511) = term(1511) + s2(a,q,j,i) * wm_interm_89_so_pt4(b,p,i,j) * wm_interm_92_so_pt4(b,a)
term(1512) = term(1512) + s1(a,p) * wm_interm_54_so_pt4(b,a,i,j) * wm_interm_73_so_pt4(b,q,i,j)
term(1513) = term(1513) + s2(a,q,j,i) * wm_interm_63_so_pt4(b,a,p,j) * wm_interm_6_so_pt4(b,i)
term(1514) = term(1514) + s2(a,q,j,i) * wm_interm_64_so_pt4(b,a,p,j) * wm_interm_6_so_pt4(b,i)
term(1515) = term(1515) + s2(a,q,j,i) * wm_interm_66_so_pt4(b,a,p,j) * wm_interm_6_so_pt4(b,i)
term(1516) = term(1516) + s1(a,i) * wm_interm_55_so_pt4(b,a,p,j) * wm_interm_67_so_pt4(b,q,i,j)
term(1517) = term(1517) + s1(a,i) * wm_interm_55_so_pt4(b,a,p,j) * wm_interm_68_so_pt4(b,q,i,j)
term(1518) = term(1518) + s1(a,i) * wm_interm_55_so_pt4(b,a,p,j) * wm_interm_69_so_pt4(b,q,i,j)
term(1519) = term(1519) + s1(a,i) * wm_interm_55_so_pt4(b,a,p,j) * wm_interm_70_so_pt4(b,q,i,j)
term(1520) = term(1520) + s1(a,p) * wm_interm_55_so_pt4(b,a,i,j) * wm_interm_70_so_pt4(b,q,i,j)
term(1521) = term(1521) + s1(a,i) * wm_interm_55_so_pt4(b,a,p,j) * wm_interm_73_so_pt4(b,q,i,j)
term(1522) = term(1522) + s1(a,i) * wm_interm_55_so_pt4(b,a,p,j) * wm_interm_74_so_pt4(b,q,i,j)
term(1523) = term(1523) + s1(a,p) * wm_interm_55_so_pt4(b,a,i,j) * wm_interm_84_so_pt4(b,q,i,j)
term(1524) = term(1524) + s1(a,p) * wm_interm_55_so_pt4(b,a,i,j) * wm_interm_73_so_pt4(b,q,i,j)
term(1525) = term(1525) + s1(a,p) * wm_interm_56_so_pt4(b,a,i,j) * wm_interm_67_so_pt4(b,q,i,j)
term(1526) = term(1526) + s1(a,p) * wm_interm_56_so_pt4(b,a,i,j) * wm_interm_68_so_pt4(b,q,i,j)
term(1527) = term(1527) + s1(a,p) * wm_interm_56_so_pt4(b,a,i,j) * wm_interm_74_so_pt4(b,q,i,j)
term(1528) = term(1528) + s1(a,i) * wm_interm_59_so_pt4(b,a,p,j) * wm_interm_67_so_pt4(b,q,i,j)
term(1529) = term(1529) + s1(a,i) * wm_interm_59_so_pt4(b,a,p,j) * wm_interm_68_so_pt4(b,q,i,j)
term(1530) = term(1530) + s1(a,i) * wm_interm_59_so_pt4(b,a,p,j) * wm_interm_74_so_pt4(b,q,i,j)
term(1531) = term(1531) + s1(a,i) * wm_interm_56_so_pt4(b,a,p,j) * wm_interm_84_so_pt4(b,q,i,j)
term(1532) = term(1532) + s1(a,i) * wm_interm_56_so_pt4(b,a,p,j) * wm_interm_73_so_pt4(b,q,i,j)
term(1533) = term(1533) + s1(a,i) * wm_interm_59_so_pt4(b,a,p,j) * wm_interm_73_so_pt4(b,q,i,j)
term(1534) = term(1534) + s1(a,p) * wm_interm_56_so_pt4(b,a,i,j) * wm_interm_73_so_pt4(b,q,i,j)
term(1535) = term(1535) + s1(a,p) * wm_interm_59_so_pt4(b,a,i,j) * wm_interm_84_so_pt4(b,q,i,j)
term(1536) = term(1536) + s1(a,p) * wm_interm_59_so_pt4(b,a,i,j) * wm_interm_73_so_pt4(b,q,i,j)
term(1537) = term(1537) + s1(a,i) * wm_interm_59_so_pt4(b,a,p,j) * wm_interm_69_so_pt4(b,q,i,j)
term(1538) = term(1538) + s1(a,i) * wm_interm_59_so_pt4(b,a,p,j) * wm_interm_70_so_pt4(b,q,i,j)
term(1539) = term(1539) + s1(a,i) * wm_interm_56_so_pt4(b,a,p,j) * wm_interm_70_so_pt4(b,q,i,j)
term(1540) = term(1540) + s1(a,p) * wm_interm_59_so_pt4(b,a,i,j) * wm_interm_70_so_pt4(b,q,i,j)
term(1541) = term(1541) + s1(a,p) * wm_interm_56_so_pt4(b,a,i,j) * wm_interm_69_so_pt4(b,q,i,j)
term(1542) = term(1542) + s1(a,p) * wm_interm_56_so_pt4(b,a,i,j) * wm_interm_70_so_pt4(b,q,i,j)
term(1543) = term(1543) + s2(a,q,j,i) * wm_interm_7_so_pt4(b,i) * wm_interm_93_so_pt4(b,a,p,j)
term(1544) = term(1544) + s2(a,q,j,i) * wm_interm_7_so_pt4(b,i) * wm_interm_94_so_pt4(b,a,p,j)
term(1545) = term(1545) + s1(a,i) * wm_interm_54_so_pt4(b,a,p,j) * wm_interm_95_so_pt4(b,q,i,j)
term(1546) = term(1546) + s1(a,i) * wm_interm_54_so_pt4(b,a,p,j) * wm_interm_96_so_pt4(b,q,i,j)
term(1547) = term(1547) + s1(a,i) * wm_interm_54_so_pt4(b,a,p,j) * wm_interm_97_so_pt4(b,q,i,j)
term(1548) = term(1548) + s1(a,i) * wm_interm_54_so_pt4(b,a,p,j) * wm_interm_98_so_pt4(b,q,i,j)
term(1549) = term(1549) + s2(a,q,j,i) * wm_interm_110_so_pt4(b,a) * wm_interm_89_so_pt4(b,p,i,j)
term(1550) = term(1550) + s1(a,p) * wm_interm_54_so_pt4(b,a,i,j) * wm_interm_98_so_pt4(b,q,i,j)
term(1551) = term(1551) + s2(a,q,j,i) * wm_interm_111_so_pt4(b,a) * wm_interm_89_so_pt4(b,p,i,j)
term(1552) = term(1552) + s1(a,p) * wm_interm_54_so_pt4(b,a,i,j) * wm_interm_97_so_pt4(b,q,i,j)
term(1553) = term(1553) + s2(a,q,j,i) * wm_interm_6_so_pt4(b,i) * wm_interm_93_so_pt4(b,a,p,j)
term(1554) = term(1554) + s2(a,q,j,i) * wm_interm_6_so_pt4(b,i) * wm_interm_94_so_pt4(b,a,p,j)
term(1555) = term(1555) + s1(a,i) * wm_interm_55_so_pt4(b,a,p,j) * wm_interm_95_so_pt4(b,q,i,j)
term(1556) = term(1556) + s1(a,i) * wm_interm_55_so_pt4(b,a,p,j) * wm_interm_96_so_pt4(b,q,i,j)
term(1557) = term(1557) + s1(a,i) * wm_interm_55_so_pt4(b,a,p,j) * wm_interm_97_so_pt4(b,q,i,j)
term(1558) = term(1558) + s1(a,i) * wm_interm_55_so_pt4(b,a,p,j) * wm_interm_98_so_pt4(b,q,i,j)
term(1559) = term(1559) + s1(a,p) * wm_interm_55_so_pt4(b,a,i,j) * wm_interm_98_so_pt4(b,q,i,j)
term(1560) = term(1560) + s1(a,p) * wm_interm_55_so_pt4(b,a,i,j) * wm_interm_97_so_pt4(b,q,i,j)
term(1561) = term(1561) + s1(a,p) * wm_interm_56_so_pt4(b,a,i,j) * wm_interm_95_so_pt4(b,q,i,j)
term(1562) = term(1562) + s1(a,p) * wm_interm_56_so_pt4(b,a,i,j) * wm_interm_96_so_pt4(b,q,i,j)
term(1563) = term(1563) + s1(a,i) * wm_interm_59_so_pt4(b,a,p,j) * wm_interm_95_so_pt4(b,q,i,j)
term(1564) = term(1564) + s1(a,i) * wm_interm_59_so_pt4(b,a,p,j) * wm_interm_96_so_pt4(b,q,i,j)
term(1565) = term(1565) + s1(a,i) * wm_interm_56_so_pt4(b,a,p,j) * wm_interm_98_so_pt4(b,q,i,j)
term(1566) = term(1566) + s1(a,i) * wm_interm_56_so_pt4(b,a,p,j) * wm_interm_97_so_pt4(b,q,i,j)
term(1567) = term(1567) + s1(a,i) * wm_interm_59_so_pt4(b,a,p,j) * wm_interm_97_so_pt4(b,q,i,j)
term(1568) = term(1568) + s1(a,p) * wm_interm_56_so_pt4(b,a,i,j) * wm_interm_97_so_pt4(b,q,i,j)
term(1569) = term(1569) + s1(a,p) * wm_interm_59_so_pt4(b,a,i,j) * wm_interm_98_so_pt4(b,q,i,j)
term(1570) = term(1570) + s1(a,p) * wm_interm_59_so_pt4(b,a,i,j) * wm_interm_97_so_pt4(b,q,i,j)
term(1571) = term(1571) + s1(a,i) * wm_interm_59_so_pt4(b,a,p,j) * wm_interm_98_so_pt4(b,q,i,j)
term(1572) = term(1572) + s1(a,p) * wm_interm_56_so_pt4(b,a,i,j) * wm_interm_98_so_pt4(b,q,i,j)
end do 
end do 
end do 
end do 

term(1478) = term(1478) * (-8.0d+0) 
term(1479) = term(1479) * (4.0d+0) 
term(1480) = term(1480) * (4.0d+0) 
term(1481) = term(1481) * (4.0d+0) 
term(1482) = term(1482) * (-2.0d+0) 
term(1483) = term(1483) * (-2.0d+0) 
term(1484) = term(1484) * (-2.0d+0) 
term(1485) = term(1485) * (4.0d+0) 
term(1487) = term(1487) * (-2.0d+0) 
term(1489) = term(1489) * (-2.0d+0) 
term(1490) = term(1490) * (-16.0d+0) 
term(1491) = term(1491) * (16.0d+0) 
term(1492) = term(1492) * (8.0d+0) 
term(1493) = term(1493) * (-8.0d+0) 
term(1494) = term(1494) * (-4.0d+0) 
term(1495) = term(1495) * (8.0d+0) 
term(1496) = term(1496) * (4.0d+0) 
term(1497) = term(1497) * (-8.0d+0) 
term(1498) = term(1498) * (2.0d+0) 
term(1499) = term(1499) * (-4.0d+0) 
term(1500) = term(1500) * (-4.0d+0) 
term(1501) = term(1501) * (8.0d+0) 
term(1502) = term(1502) * (-4.0d+0) 
term(1503) = term(1503) * (8.0d+0) 
term(1504) = term(1504) * (2.0d+0) 
term(1505) = term(1505) * (-4.0d+0) 
term(1506) = term(1506) * (-4.0d+0) 
term(1507) = term(1507) * (2.0d+0) 
term(1508) = term(1508) * (-4.0d+0) 
term(1509) = term(1509) * (2.0d+0) 
term(1510) = term(1510) * (-4.0d+0) 
term(1511) = term(1511) * (-4.0d+0) 
term(1512) = term(1512) * (8.0d+0) 
term(1513) = term(1513) * (-4.0d+0) 
term(1514) = term(1514) * (8.0d+0) 
term(1515) = term(1515) * (-4.0d+0) 
term(1516) = term(1516) * (2.0d+0) 
term(1517) = term(1517) * (-4.0d+0) 
term(1518) = term(1518) * (2.0d+0) 
term(1519) = term(1519) * (-4.0d+0) 
term(1520) = term(1520) * (2.0d+0) 
term(1521) = term(1521) * (2.0d+0) 
term(1522) = term(1522) * (2.0d+0) 
term(1523) = term(1523) * (2.0d+0) 
term(1524) = term(1524) * (-4.0d+0) 
term(1525) = term(1525) * (2.0d+0) 
term(1526) = term(1526) * (-4.0d+0) 
term(1527) = term(1527) * (2.0d+0) 
term(1528) = term(1528) * (2.0d+0) 
term(1529) = term(1529) * (-4.0d+0) 
term(1530) = term(1530) * (2.0d+0) 
term(1531) = term(1531) * (2.0d+0) 
term(1532) = term(1532) * (-4.0d+0) 
term(1533) = term(1533) * (2.0d+0) 
term(1534) = term(1534) * (2.0d+0) 
term(1535) = term(1535) * (2.0d+0) 
term(1536) = term(1536) * (-4.0d+0) 
term(1537) = term(1537) * (2.0d+0) 
term(1538) = term(1538) * (-4.0d+0) 
term(1539) = term(1539) * (2.0d+0) 
term(1540) = term(1540) * (2.0d+0) 
term(1541) = term(1541) * (2.0d+0) 
term(1542) = term(1542) * (-4.0d+0) 
term(1543) = term(1543) * (8.0d+0) 
term(1544) = term(1544) * (-8.0d+0) 
term(1545) = term(1545) * (-16.0d+0) 
term(1546) = term(1546) * (16.0d+0) 
term(1547) = term(1547) * (-16.0d+0) 
term(1548) = term(1548) * (16.0d+0) 
term(1549) = term(1549) * (8.0d+0) 
term(1550) = term(1550) * (-16.0d+0) 
term(1551) = term(1551) * (-8.0d+0) 
term(1552) = term(1552) * (16.0d+0) 
term(1553) = term(1553) * (-16.0d+0) 
term(1554) = term(1554) * (16.0d+0) 
term(1555) = term(1555) * (8.0d+0) 
term(1556) = term(1556) * (-8.0d+0) 
term(1557) = term(1557) * (8.0d+0) 
term(1558) = term(1558) * (-8.0d+0) 
term(1559) = term(1559) * (8.0d+0) 
term(1560) = term(1560) * (-8.0d+0) 
term(1561) = term(1561) * (8.0d+0) 
term(1562) = term(1562) * (-8.0d+0) 
term(1563) = term(1563) * (8.0d+0) 
term(1564) = term(1564) * (-8.0d+0) 
term(1565) = term(1565) * (8.0d+0) 
term(1566) = term(1566) * (-8.0d+0) 
term(1567) = term(1567) * (8.0d+0) 
term(1568) = term(1568) * (8.0d+0) 
term(1569) = term(1569) * (8.0d+0) 
term(1570) = term(1570) * (-8.0d+0) 
term(1571) = term(1571) * (-8.0d+0) 
term(1572) = term(1572) * (-8.0d+0) 

do l = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
term(1573) = term(1573) + s1(q,i) * wm_interm_71_so_pt4(p,j,k,l) * wm_interm_9_so_pt4(k,l,j,i)
term(1574) = term(1574) + s1(q,i) * wm_interm_71_so_pt4(p,j,k,l) * wm_interm_9_so_pt4(l,k,j,i)
term(1575) = term(1575) + s1(q,i) * wm_interm_72_so_pt4(p,j,k,l) * wm_interm_9_so_pt4(l,k,j,i)
term(1576) = term(1576) + s1(q,i) * wm_interm_71_so_pt4(j,p,k,l) * wm_interm_9_so_pt4(l,k,j,i)
term(1577) = term(1577) + s1(q,i) * wm_interm_72_so_pt4(j,p,k,l) * wm_interm_9_so_pt4(k,l,j,i)
term(1578) = term(1578) + s1(q,i) * wm_interm_72_so_pt4(j,p,k,l) * wm_interm_9_so_pt4(l,k,j,i)
term(1579) = term(1579) + s1(q,i) * wm_interm_61_so_pt4(j,k,l,i) * wm_interm_88_so_pt4(j,k,p,l)
term(1580) = term(1580) + s1(q,i) * wm_interm_61_so_pt4(j,k,l,i) * wm_interm_88_so_pt4(k,j,p,l)
term(1581) = term(1581) + s1(q,i) * wm_interm_61_so_pt4(j,k,i,l) * wm_interm_88_so_pt4(j,k,l,p)
term(1582) = term(1582) + s1(q,i) * wm_interm_61_so_pt4(j,k,i,l) * wm_interm_88_so_pt4(k,j,p,l)
term(1583) = term(1583) + s1(q,i) * wm_interm_61_so_pt4(j,k,i,l) * wm_interm_88_so_pt4(j,k,p,l)
term(1584) = term(1584) + s1(q,i) * wm_interm_99_so_pt4(p,j,k,l) * wm_interm_9_so_pt4(k,l,j,i)
term(1585) = term(1585) + s1(q,i) * wm_interm_99_so_pt4(p,j,k,l) * wm_interm_9_so_pt4(l,k,j,i)
term(1586) = term(1586) + s1(q,i) * wm_interm_99_so_pt4(j,p,k,l) * wm_interm_9_so_pt4(l,k,j,i)
term(1587) = term(1587) + s1(q,i) * wm_interm_99_so_pt4(j,p,k,l) * wm_interm_9_so_pt4(k,l,j,i)
term(1588) = term(1588) + s1(q,i) * wm_interm_109_so_pt4(j,k,p,l) * wm_interm_61_so_pt4(j,k,l,i)
term(1589) = term(1589) + s1(q,i) * wm_interm_109_so_pt4(j,k,p,l) * wm_interm_61_so_pt4(k,j,l,i)
term(1590) = term(1590) + s1(q,i) * wm_interm_109_so_pt4(j,k,l,p) * wm_interm_61_so_pt4(j,k,i,l)
term(1591) = term(1591) + s1(q,i) * wm_interm_109_so_pt4(j,k,p,l) * wm_interm_61_so_pt4(k,j,i,l)
term(1592) = term(1592) + s1(q,i) * wm_interm_109_so_pt4(j,k,p,l) * wm_interm_61_so_pt4(j,k,i,l)
end do 
end do 
end do 
end do 

term(1574) = term(1574) * (-2.0d+0) 
term(1578) = term(1578) * (-2.0d+0) 
term(1580) = term(1580) * (-2.0d+0) 
term(1583) = term(1583) * (-2.0d+0) 
term(1584) = term(1584) * (4.0d+0) 
term(1585) = term(1585) * (-4.0d+0) 
term(1586) = term(1586) * (4.0d+0) 
term(1587) = term(1587) * (-4.0d+0) 
term(1588) = term(1588) * (2.0d+0) 
term(1589) = term(1589) * (-4.0d+0) 
term(1590) = term(1590) * (2.0d+0) 
term(1591) = term(1591) * (2.0d+0) 
term(1592) = term(1592) * (-4.0d+0) 


    calc_D_ov_wm_so_pt4 = zero
    do s = 0, 1592
    calc_D_ov_wm_so_pt4 = calc_D_ov_wm_so_pt4 + term(s)
    end do

    end function calc_D_ov_wm_so_pt4
    
    function calc_D_vo_wm_so_pt4(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vo_wm_so_pt4
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , k, j, a, i, b, l 
    real(F64), dimension(0:1880) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
do i = 1, nocc 
do b = nocc + 1, nactive 
term(0) = term(0) + r2p(vrdav_Rr, p,i,a,q) * wm_interm_113_so_pt4(b,j,i,k) * wm_interm_55_so_pt4(a,b,k,j)
term(1) = term(1) + r2p(vrdav_Rr, p,i,a,q) * wm_interm_113_so_pt4(b,j,i,k) * wm_interm_54_so_pt4(a,b,k,j)
term(2) = term(2) + r2p(vrdav_Rr, p,i,a,q) * wm_interm_22_so_pt4(b,j,i,k) * wm_interm_56_so_pt4(a,b,k,j)
term(3) = term(3) + r2p(vrdav_Rr, p,i,a,q) * wm_interm_22_so_pt4(b,j,i,k) * wm_interm_59_so_pt4(a,b,k,j)
term(4) = term(4) + r2p(vrdav_Rr, p,i,a,q) * wm_interm_10_so_pt4(a,b,j,k) * wm_interm_47_so_pt4(b,k,i,j)
term(5) = term(5) + r2p(vrdav_Rr, p,i,a,q) * wm_interm_11_so_pt4(a,b,j,k) * wm_interm_47_so_pt4(b,k,i,j)
term(6) = term(6) + r2p(vrdav_Rr, p,i,a,q) * wm_interm_47_so_pt4(b,j,i,k) * wm_interm_5_so_pt4(a,b,k,j)
term(7) = term(7) + r2p(vrdav_Rr, p,i,a,q) * wm_interm_1_so_pt4(a,b,j,k) * wm_interm_47_so_pt4(b,k,i,j)
term(8) = term(8) + r2p(vrdav_Rr, p,i,a,q) * wm_interm_113_so_pt4(b,j,i,k) * wm_interm_56_so_pt4(a,b,k,j)
term(9) = term(9) + r2p(vrdav_Rr, p,i,a,q) * wm_interm_113_so_pt4(b,j,i,k) * wm_interm_59_so_pt4(a,b,k,j)
term(10) = term(10) + r2p(vrdav_Rr, p,i,a,q) * wm_interm_10_so_pt4(a,b,j,k) * wm_interm_49_so_pt4(b,k,i,j)
term(11) = term(11) + r2p(vrdav_Rr, p,i,a,q) * wm_interm_11_so_pt4(a,b,j,k) * wm_interm_49_so_pt4(b,k,i,j)
term(12) = term(12) + r2p(vrdav_Rr, p,i,a,q) * wm_interm_1_so_pt4(a,b,j,k) * wm_interm_49_so_pt4(b,k,i,j)
term(13) = term(13) + r2p(vrdav_Rr, p,i,a,q) * wm_interm_49_so_pt4(b,j,i,k) * wm_interm_5_so_pt4(a,b,k,j)
term(14) = term(14) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_113_so_pt4(b,j,i,k) * wm_interm_54_so_pt4(a,b,k,q)
term(15) = term(15) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_22_so_pt4(b,j,i,k) * wm_interm_59_so_pt4(a,b,k,q)
term(16) = term(16) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_22_so_pt4(b,j,i,k) * wm_interm_56_so_pt4(a,b,k,q)
term(17) = term(17) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_113_so_pt4(b,j,i,k) * wm_interm_55_so_pt4(a,b,k,q)
term(18) = term(18) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_47_so_pt4(b,j,i,k) * wm_interm_5_so_pt4(a,b,k,q)
term(19) = term(19) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_1_so_pt4(a,b,k,q) * wm_interm_47_so_pt4(b,j,i,k)
term(20) = term(20) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_10_so_pt4(a,b,k,q) * wm_interm_47_so_pt4(b,j,i,k)
term(21) = term(21) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_11_so_pt4(a,b,k,q) * wm_interm_47_so_pt4(b,j,i,k)
term(22) = term(22) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_113_so_pt4(b,j,i,k) * wm_interm_56_so_pt4(a,b,k,q)
term(23) = term(23) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_113_so_pt4(b,j,i,k) * wm_interm_59_so_pt4(a,b,k,q)
term(24) = term(24) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_10_so_pt4(a,b,k,q) * wm_interm_49_so_pt4(b,j,i,k)
term(25) = term(25) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_49_so_pt4(b,j,i,k) * wm_interm_5_so_pt4(a,b,k,q)
term(26) = term(26) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_1_so_pt4(a,b,k,q) * wm_interm_49_so_pt4(b,j,i,k)
term(27) = term(27) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_11_so_pt4(a,b,k,q) * wm_interm_49_so_pt4(b,j,i,k)
end do 
end do 
end do 
end do 
end do 

term(0) = term(0) * (-2.0d+0) 
term(1) = term(1) * (4.0d+0) 
term(2) = term(2) * (-1.0d+0) 
term(3) = term(3) * (2.0d+0) 
term(4) = term(4) * (2.0d+0) 
term(5) = term(5) * (-4.0d+0) 
term(6) = term(6) * (-1.0d+0) 
term(7) = term(7) * (2.0d+0) 
term(8) = term(8) * (2.0d+0) 
term(9) = term(9) * (-1.0d+0) 
term(10) = term(10) * (-1.0d+0) 
term(11) = term(11) * (2.0d+0) 
term(12) = term(12) * (-1.0d+0) 
term(13) = term(13) * (2.0d+0) 
term(14) = term(14) * (-4.0d+0) 
term(15) = term(15) * (-0.5d+0) 
term(17) = term(17) * (2.0d+0) 
term(19) = term(19) * (-0.5d+0) 
term(20) = term(20) * (-0.5d+0) 
term(22) = term(22) * (-0.5d+0) 
term(25) = term(25) * (-0.5d+0) 
term(27) = term(27) * (-2.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(28) = term(28) + r2p(vrdav_Rr, p,q,a,i) * wm_interm_113_so_pt4(b,j,i,k) * wm_interm_55_so_pt4(a,b,k,j)
term(29) = term(29) + r2p(vrdav_Rr, p,q,a,i) * wm_interm_113_so_pt4(b,i,j,k) * wm_interm_55_so_pt4(a,b,k,j)
term(30) = term(30) + r2p(vrdav_Rr, p,q,a,i) * wm_interm_113_so_pt4(b,j,i,k) * wm_interm_54_so_pt4(a,b,k,j)
term(31) = term(31) + r2p(vrdav_Rr, p,q,a,i) * wm_interm_113_so_pt4(b,i,j,k) * wm_interm_54_so_pt4(a,b,k,j)
term(32) = term(32) + r2p(vrdav_Rr, p,q,a,i) * wm_interm_22_so_pt4(b,j,i,k) * wm_interm_56_so_pt4(a,b,k,j)
term(33) = term(33) + r2p(vrdav_Rr, p,q,a,i) * wm_interm_22_so_pt4(b,i,j,k) * wm_interm_56_so_pt4(a,b,k,j)
term(34) = term(34) + r2p(vrdav_Rr, p,q,a,i) * wm_interm_22_so_pt4(b,i,j,k) * wm_interm_59_so_pt4(a,b,k,j)
term(35) = term(35) + r2p(vrdav_Rr, p,q,a,i) * wm_interm_22_so_pt4(b,j,i,k) * wm_interm_59_so_pt4(a,b,k,j)
term(36) = term(36) + r2p(vrdav_Rr, p,q,a,i) * wm_interm_10_so_pt4(a,b,j,k) * wm_interm_47_so_pt4(b,i,k,j)
term(37) = term(37) + r2p(vrdav_Rr, p,q,a,i) * wm_interm_10_so_pt4(a,b,j,k) * wm_interm_47_so_pt4(b,k,i,j)
term(38) = term(38) + r2p(vrdav_Rr, p,q,a,i) * wm_interm_11_so_pt4(a,b,j,k) * wm_interm_47_so_pt4(b,i,k,j)
term(39) = term(39) + r2p(vrdav_Rr, p,q,a,i) * wm_interm_11_so_pt4(a,b,j,k) * wm_interm_47_so_pt4(b,k,i,j)
term(40) = term(40) + r2p(vrdav_Rr, p,q,a,i) * wm_interm_1_so_pt4(a,b,j,k) * wm_interm_47_so_pt4(b,i,k,j)
term(41) = term(41) + r2p(vrdav_Rr, p,q,a,i) * wm_interm_47_so_pt4(b,i,j,k) * wm_interm_5_so_pt4(a,b,k,j)
term(42) = term(42) + r2p(vrdav_Rr, p,q,a,i) * wm_interm_47_so_pt4(b,j,i,k) * wm_interm_5_so_pt4(a,b,k,j)
term(43) = term(43) + r2p(vrdav_Rr, p,q,a,i) * wm_interm_1_so_pt4(a,b,j,k) * wm_interm_47_so_pt4(b,k,i,j)
term(44) = term(44) + r2p(vrdav_Rr, p,q,a,i) * wm_interm_113_so_pt4(b,i,j,k) * wm_interm_56_so_pt4(a,b,k,j)
term(45) = term(45) + r2p(vrdav_Rr, p,q,a,i) * wm_interm_113_so_pt4(b,j,i,k) * wm_interm_56_so_pt4(a,b,k,j)
term(46) = term(46) + r2p(vrdav_Rr, p,q,a,i) * wm_interm_113_so_pt4(b,j,i,k) * wm_interm_59_so_pt4(a,b,k,j)
term(47) = term(47) + r2p(vrdav_Rr, p,q,a,i) * wm_interm_113_so_pt4(b,i,j,k) * wm_interm_59_so_pt4(a,b,k,j)
term(48) = term(48) + r2p(vrdav_Rr, p,q,a,i) * wm_interm_10_so_pt4(a,b,j,k) * wm_interm_49_so_pt4(b,i,k,j)
term(49) = term(49) + r2p(vrdav_Rr, p,q,a,i) * wm_interm_11_so_pt4(a,b,j,k) * wm_interm_49_so_pt4(b,i,k,j)
term(50) = term(50) + r2p(vrdav_Rr, p,q,a,i) * wm_interm_10_so_pt4(a,b,j,k) * wm_interm_49_so_pt4(b,k,i,j)
term(51) = term(51) + r2p(vrdav_Rr, p,q,a,i) * wm_interm_11_so_pt4(a,b,j,k) * wm_interm_49_so_pt4(b,k,i,j)
term(52) = term(52) + r2p(vrdav_Rr, p,q,a,i) * wm_interm_49_so_pt4(b,i,j,k) * wm_interm_5_so_pt4(a,b,k,j)
term(53) = term(53) + r2p(vrdav_Rr, p,q,a,i) * wm_interm_1_so_pt4(a,b,j,k) * wm_interm_49_so_pt4(b,i,k,j)
term(54) = term(54) + r2p(vrdav_Rr, p,q,a,i) * wm_interm_1_so_pt4(a,b,j,k) * wm_interm_49_so_pt4(b,k,i,j)
term(55) = term(55) + r2p(vrdav_Rr, p,q,a,i) * wm_interm_49_so_pt4(b,j,i,k) * wm_interm_5_so_pt4(a,b,k,j)
end do 
end do 
end do 
end do 
end do 

term(28) = term(28) * (4.0d+0) 
term(29) = term(29) * (-8.0d+0) 
term(30) = term(30) * (-8.0d+0) 
term(31) = term(31) * (16.0d+0) 
term(32) = term(32) * (2.0d+0) 
term(33) = term(33) * (-4.0d+0) 
term(34) = term(34) * (2.0d+0) 
term(35) = term(35) * (-4.0d+0) 
term(36) = term(36) * (2.0d+0) 
term(37) = term(37) * (-4.0d+0) 
term(38) = term(38) * (-4.0d+0) 
term(39) = term(39) * (8.0d+0) 
term(40) = term(40) * (2.0d+0) 
term(41) = term(41) * (-4.0d+0) 
term(42) = term(42) * (2.0d+0) 
term(43) = term(43) * (-4.0d+0) 
term(44) = term(44) * (2.0d+0) 
term(45) = term(45) * (-4.0d+0) 
term(46) = term(46) * (2.0d+0) 
term(47) = term(47) * (-4.0d+0) 
term(48) = term(48) * (-4.0d+0) 
term(49) = term(49) * (8.0d+0) 
term(50) = term(50) * (2.0d+0) 
term(51) = term(51) * (-4.0d+0) 
term(52) = term(52) * (2.0d+0) 
term(53) = term(53) * (-4.0d+0) 
term(54) = term(54) * (2.0d+0) 
term(55) = term(55) * (-4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
do i = 1, nocc 
term(56) = term(56) + r2p(vrdav_Rr, p,i,a,q) * wm_interm_113_so_pt4(b,i,j,k) * wm_interm_55_so_pt4(a,b,k,j)
term(57) = term(57) + r2p(vrdav_Rr, p,i,a,q) * wm_interm_113_so_pt4(b,i,j,k) * wm_interm_54_so_pt4(a,b,k,j)
term(58) = term(58) + r2p(vrdav_Rr, p,i,a,q) * wm_interm_22_so_pt4(b,i,j,k) * wm_interm_56_so_pt4(a,b,k,j)
term(59) = term(59) + r2p(vrdav_Rr, p,i,a,q) * wm_interm_22_so_pt4(b,i,j,k) * wm_interm_59_so_pt4(a,b,k,j)
term(60) = term(60) + r2p(vrdav_Rr, p,i,a,q) * wm_interm_10_so_pt4(a,b,j,k) * wm_interm_47_so_pt4(b,i,k,j)
term(61) = term(61) + r2p(vrdav_Rr, p,i,a,q) * wm_interm_11_so_pt4(a,b,j,k) * wm_interm_47_so_pt4(b,i,k,j)
term(62) = term(62) + r2p(vrdav_Rr, p,i,a,q) * wm_interm_1_so_pt4(a,b,j,k) * wm_interm_47_so_pt4(b,i,k,j)
term(63) = term(63) + r2p(vrdav_Rr, p,i,a,q) * wm_interm_47_so_pt4(b,i,j,k) * wm_interm_5_so_pt4(a,b,k,j)
term(64) = term(64) + r2p(vrdav_Rr, p,i,a,q) * wm_interm_113_so_pt4(b,i,j,k) * wm_interm_56_so_pt4(a,b,k,j)
term(65) = term(65) + r2p(vrdav_Rr, p,i,a,q) * wm_interm_113_so_pt4(b,i,j,k) * wm_interm_59_so_pt4(a,b,k,j)
term(66) = term(66) + r2p(vrdav_Rr, p,i,a,q) * wm_interm_10_so_pt4(a,b,j,k) * wm_interm_49_so_pt4(b,i,k,j)
term(67) = term(67) + r2p(vrdav_Rr, p,i,a,q) * wm_interm_11_so_pt4(a,b,j,k) * wm_interm_49_so_pt4(b,i,k,j)
term(68) = term(68) + r2p(vrdav_Rr, p,i,a,q) * wm_interm_49_so_pt4(b,i,j,k) * wm_interm_5_so_pt4(a,b,k,j)
term(69) = term(69) + r2p(vrdav_Rr, p,i,a,q) * wm_interm_1_so_pt4(a,b,j,k) * wm_interm_49_so_pt4(b,i,k,j)
term(70) = term(70) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_113_so_pt4(b,i,j,k) * wm_interm_54_so_pt4(a,b,k,q)
term(71) = term(71) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_22_so_pt4(b,i,j,k) * wm_interm_59_so_pt4(a,b,k,q)
term(72) = term(72) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_22_so_pt4(b,i,j,k) * wm_interm_56_so_pt4(a,b,k,q)
term(73) = term(73) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_113_so_pt4(b,i,j,k) * wm_interm_55_so_pt4(a,b,k,q)
term(74) = term(74) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_47_so_pt4(b,i,j,k) * wm_interm_5_so_pt4(a,b,k,q)
term(75) = term(75) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_1_so_pt4(a,b,k,q) * wm_interm_47_so_pt4(b,i,j,k)
term(76) = term(76) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_10_so_pt4(a,b,k,q) * wm_interm_47_so_pt4(b,i,j,k)
term(77) = term(77) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_11_so_pt4(a,b,k,q) * wm_interm_47_so_pt4(b,i,j,k)
term(78) = term(78) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_113_so_pt4(b,i,j,k) * wm_interm_56_so_pt4(a,b,k,q)
term(79) = term(79) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_113_so_pt4(b,i,j,k) * wm_interm_59_so_pt4(a,b,k,q)
term(80) = term(80) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_10_so_pt4(a,b,k,q) * wm_interm_49_so_pt4(b,i,j,k)
term(81) = term(81) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_49_so_pt4(b,i,j,k) * wm_interm_5_so_pt4(a,b,k,q)
term(82) = term(82) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_1_so_pt4(a,b,k,q) * wm_interm_49_so_pt4(b,i,j,k)
term(83) = term(83) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_11_so_pt4(a,b,k,q) * wm_interm_49_so_pt4(b,i,j,k)
end do 
end do 
end do 
end do 
end do 

term(56) = term(56) * (4.0d+0) 
term(57) = term(57) * (-8.0d+0) 
term(58) = term(58) * (2.0d+0) 
term(59) = term(59) * (-1.0d+0) 
term(60) = term(60) * (-1.0d+0) 
term(61) = term(61) * (2.0d+0) 
term(62) = term(62) * (-1.0d+0) 
term(63) = term(63) * (2.0d+0) 
term(64) = term(64) * (-1.0d+0) 
term(65) = term(65) * (2.0d+0) 
term(66) = term(66) * (2.0d+0) 
term(67) = term(67) * (-4.0d+0) 
term(68) = term(68) * (-1.0d+0) 
term(69) = term(69) * (2.0d+0) 
term(70) = term(70) * (2.0d+0) 
term(72) = term(72) * (-0.5d+0) 
term(73) = term(73) * (-1.0d+0) 
term(74) = term(74) * (-0.5d+0) 
term(77) = term(77) * (-2.0d+0) 
term(79) = term(79) * (-0.5d+0) 
term(80) = term(80) * (-0.5d+0) 
term(82) = term(82) * (-0.5d+0) 

do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(84) = term(84) + t2(a,p,j,i) * wm_interm_50_so_pt4(a,k) * wm_interm_88_so_pt4(k,q,i,j)
term(85) = term(85) + t2(a,p,j,i) * wm_interm_50_so_pt4(a,k) * wm_interm_88_so_pt4(q,k,i,j)
term(86) = term(86) + t2(a,p,j,i) * wm_interm_51_so_pt4(a,k) * wm_interm_88_so_pt4(k,q,i,j)
term(87) = term(87) + t2(a,p,j,i) * wm_interm_51_so_pt4(a,k) * wm_interm_88_so_pt4(q,k,i,j)
term(88) = term(88) + t2(a,p,j,i) * wm_interm_109_so_pt4(k,q,i,j) * wm_interm_50_so_pt4(a,k)
term(89) = term(89) + t2(a,p,j,i) * wm_interm_109_so_pt4(q,k,i,j) * wm_interm_50_so_pt4(a,k)
term(90) = term(90) + t2(a,p,j,i) * wm_interm_109_so_pt4(k,q,i,j) * wm_interm_51_so_pt4(a,k)
term(91) = term(91) + t2(a,p,j,i) * wm_interm_109_so_pt4(q,k,i,j) * wm_interm_51_so_pt4(a,k)
end do 
end do 
end do 
end do 

term(84) = term(84) * (2.0d+0) 
term(85) = term(85) * (-4.0d+0) 
term(86) = term(86) * (-1.0d+0) 
term(87) = term(87) * (2.0d+0) 
term(88) = term(88) * (4.0d+0) 
term(89) = term(89) * (-8.0d+0) 
term(90) = term(90) * (-2.0d+0) 
term(91) = term(91) * (4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(92) = term(92) + r2p(vrdav_Rr, a,q,p,i) * wm_interm_113_so_pt4(b,j,i,k) * wm_interm_55_so_pt4(a,b,k,j)
term(93) = term(93) + r2p(vrdav_Rr, a,q,p,i) * wm_interm_113_so_pt4(b,i,j,k) * wm_interm_55_so_pt4(a,b,k,j)
term(94) = term(94) + r2p(vrdav_Rr, a,q,p,i) * wm_interm_113_so_pt4(b,j,i,k) * wm_interm_54_so_pt4(a,b,k,j)
term(95) = term(95) + r2p(vrdav_Rr, a,q,p,i) * wm_interm_113_so_pt4(b,i,j,k) * wm_interm_54_so_pt4(a,b,k,j)
term(96) = term(96) + r2p(vrdav_Rr, a,q,p,i) * wm_interm_22_so_pt4(b,j,i,k) * wm_interm_56_so_pt4(a,b,k,j)
term(97) = term(97) + r2p(vrdav_Rr, a,q,p,i) * wm_interm_22_so_pt4(b,i,j,k) * wm_interm_56_so_pt4(a,b,k,j)
term(98) = term(98) + r2p(vrdav_Rr, a,q,p,i) * wm_interm_22_so_pt4(b,i,j,k) * wm_interm_59_so_pt4(a,b,k,j)
term(99) = term(99) + r2p(vrdav_Rr, a,q,p,i) * wm_interm_22_so_pt4(b,j,i,k) * wm_interm_59_so_pt4(a,b,k,j)
term(100) = term(100) + r2p(vrdav_Rr, a,q,p,i) * wm_interm_10_so_pt4(a,b,j,k) * wm_interm_47_so_pt4(b,i,k,j)
term(101) = term(101) + r2p(vrdav_Rr, a,q,p,i) * wm_interm_10_so_pt4(a,b,j,k) * wm_interm_47_so_pt4(b,k,i,j)
term(102) = term(102) + r2p(vrdav_Rr, a,q,p,i) * wm_interm_11_so_pt4(a,b,j,k) * wm_interm_47_so_pt4(b,i,k,j)
term(103) = term(103) + r2p(vrdav_Rr, a,q,p,i) * wm_interm_11_so_pt4(a,b,j,k) * wm_interm_47_so_pt4(b,k,i,j)
term(104) = term(104) + r2p(vrdav_Rr, a,q,p,i) * wm_interm_1_so_pt4(a,b,j,k) * wm_interm_47_so_pt4(b,i,k,j)
term(105) = term(105) + r2p(vrdav_Rr, a,q,p,i) * wm_interm_47_so_pt4(b,i,j,k) * wm_interm_5_so_pt4(a,b,k,j)
term(106) = term(106) + r2p(vrdav_Rr, a,q,p,i) * wm_interm_47_so_pt4(b,j,i,k) * wm_interm_5_so_pt4(a,b,k,j)
term(107) = term(107) + r2p(vrdav_Rr, a,q,p,i) * wm_interm_1_so_pt4(a,b,j,k) * wm_interm_47_so_pt4(b,k,i,j)
term(108) = term(108) + r2p(vrdav_Rr, a,q,p,i) * wm_interm_113_so_pt4(b,i,j,k) * wm_interm_56_so_pt4(a,b,k,j)
term(109) = term(109) + r2p(vrdav_Rr, a,q,p,i) * wm_interm_113_so_pt4(b,j,i,k) * wm_interm_56_so_pt4(a,b,k,j)
term(110) = term(110) + r2p(vrdav_Rr, a,q,p,i) * wm_interm_113_so_pt4(b,j,i,k) * wm_interm_59_so_pt4(a,b,k,j)
term(111) = term(111) + r2p(vrdav_Rr, a,q,p,i) * wm_interm_113_so_pt4(b,i,j,k) * wm_interm_59_so_pt4(a,b,k,j)
term(112) = term(112) + r2p(vrdav_Rr, a,q,p,i) * wm_interm_10_so_pt4(a,b,j,k) * wm_interm_49_so_pt4(b,i,k,j)
term(113) = term(113) + r2p(vrdav_Rr, a,q,p,i) * wm_interm_11_so_pt4(a,b,j,k) * wm_interm_49_so_pt4(b,i,k,j)
term(114) = term(114) + r2p(vrdav_Rr, a,q,p,i) * wm_interm_10_so_pt4(a,b,j,k) * wm_interm_49_so_pt4(b,k,i,j)
term(115) = term(115) + r2p(vrdav_Rr, a,q,p,i) * wm_interm_11_so_pt4(a,b,j,k) * wm_interm_49_so_pt4(b,k,i,j)
term(116) = term(116) + r2p(vrdav_Rr, a,q,p,i) * wm_interm_49_so_pt4(b,i,j,k) * wm_interm_5_so_pt4(a,b,k,j)
term(117) = term(117) + r2p(vrdav_Rr, a,q,p,i) * wm_interm_1_so_pt4(a,b,j,k) * wm_interm_49_so_pt4(b,i,k,j)
term(118) = term(118) + r2p(vrdav_Rr, a,q,p,i) * wm_interm_1_so_pt4(a,b,j,k) * wm_interm_49_so_pt4(b,k,i,j)
term(119) = term(119) + r2p(vrdav_Rr, a,q,p,i) * wm_interm_49_so_pt4(b,j,i,k) * wm_interm_5_so_pt4(a,b,k,j)
term(120) = term(120) + r2m(vrdav_Rr, a,q,p,i) * wm_interm_113_so_pt4(b,j,i,k) * wm_interm_55_so_pt4(a,b,k,j)
term(121) = term(121) + r2m(vrdav_Rr, a,i,p,q) * wm_interm_113_so_pt4(b,j,i,k) * wm_interm_55_so_pt4(a,b,k,j)
term(122) = term(122) + r2m(vrdav_Rr, a,q,p,i) * wm_interm_113_so_pt4(b,i,j,k) * wm_interm_55_so_pt4(a,b,k,j)
term(123) = term(123) + r2m(vrdav_Rr, a,q,p,i) * wm_interm_113_so_pt4(b,j,i,k) * wm_interm_54_so_pt4(a,b,k,j)
term(124) = term(124) + r2m(vrdav_Rr, a,i,p,q) * wm_interm_113_so_pt4(b,j,i,k) * wm_interm_54_so_pt4(a,b,k,j)
term(125) = term(125) + r2m(vrdav_Rr, a,q,p,i) * wm_interm_113_so_pt4(b,i,j,k) * wm_interm_54_so_pt4(a,b,k,j)
term(126) = term(126) + r2m(vrdav_Rr, a,q,p,i) * wm_interm_22_so_pt4(b,j,i,k) * wm_interm_56_so_pt4(a,b,k,j)
term(127) = term(127) + r2m(vrdav_Rr, a,i,p,q) * wm_interm_22_so_pt4(b,j,i,k) * wm_interm_56_so_pt4(a,b,k,j)
term(128) = term(128) + r2m(vrdav_Rr, a,q,p,i) * wm_interm_22_so_pt4(b,i,j,k) * wm_interm_56_so_pt4(a,b,k,j)
term(129) = term(129) + r2m(vrdav_Rr, a,q,p,i) * wm_interm_22_so_pt4(b,i,j,k) * wm_interm_59_so_pt4(a,b,k,j)
term(130) = term(130) + r2m(vrdav_Rr, a,q,p,i) * wm_interm_22_so_pt4(b,j,i,k) * wm_interm_59_so_pt4(a,b,k,j)
term(131) = term(131) + r2m(vrdav_Rr, a,i,p,q) * wm_interm_22_so_pt4(b,j,i,k) * wm_interm_59_so_pt4(a,b,k,j)
term(132) = term(132) + r2m(vrdav_Rr, a,q,p,i) * wm_interm_10_so_pt4(a,b,j,k) * wm_interm_47_so_pt4(b,i,k,j)
term(133) = term(133) + r2m(vrdav_Rr, a,q,p,i) * wm_interm_10_so_pt4(a,b,j,k) * wm_interm_47_so_pt4(b,k,i,j)
term(134) = term(134) + r2m(vrdav_Rr, a,i,p,q) * wm_interm_10_so_pt4(a,b,j,k) * wm_interm_47_so_pt4(b,k,i,j)
term(135) = term(135) + r2m(vrdav_Rr, a,q,p,i) * wm_interm_11_so_pt4(a,b,j,k) * wm_interm_47_so_pt4(b,i,k,j)
term(136) = term(136) + r2m(vrdav_Rr, a,q,p,i) * wm_interm_11_so_pt4(a,b,j,k) * wm_interm_47_so_pt4(b,k,i,j)
term(137) = term(137) + r2m(vrdav_Rr, a,i,p,q) * wm_interm_11_so_pt4(a,b,j,k) * wm_interm_47_so_pt4(b,k,i,j)
term(138) = term(138) + r2m(vrdav_Rr, a,q,p,i) * wm_interm_1_so_pt4(a,b,j,k) * wm_interm_47_so_pt4(b,i,k,j)
term(139) = term(139) + r2m(vrdav_Rr, a,q,p,i) * wm_interm_47_so_pt4(b,i,j,k) * wm_interm_5_so_pt4(a,b,k,j)
term(140) = term(140) + r2m(vrdav_Rr, a,q,p,i) * wm_interm_47_so_pt4(b,j,i,k) * wm_interm_5_so_pt4(a,b,k,j)
term(141) = term(141) + r2m(vrdav_Rr, a,i,p,q) * wm_interm_47_so_pt4(b,j,i,k) * wm_interm_5_so_pt4(a,b,k,j)
term(142) = term(142) + r2m(vrdav_Rr, a,q,p,i) * wm_interm_1_so_pt4(a,b,j,k) * wm_interm_47_so_pt4(b,k,i,j)
term(143) = term(143) + r2m(vrdav_Rr, a,i,p,q) * wm_interm_1_so_pt4(a,b,j,k) * wm_interm_47_so_pt4(b,k,i,j)
term(144) = term(144) + r2m(vrdav_Rr, a,q,p,i) * wm_interm_113_so_pt4(b,i,j,k) * wm_interm_56_so_pt4(a,b,k,j)
term(145) = term(145) + r2m(vrdav_Rr, a,q,p,i) * wm_interm_113_so_pt4(b,j,i,k) * wm_interm_56_so_pt4(a,b,k,j)
term(146) = term(146) + r2m(vrdav_Rr, a,i,p,q) * wm_interm_113_so_pt4(b,j,i,k) * wm_interm_56_so_pt4(a,b,k,j)
term(147) = term(147) + r2m(vrdav_Rr, a,q,p,i) * wm_interm_113_so_pt4(b,j,i,k) * wm_interm_59_so_pt4(a,b,k,j)
term(148) = term(148) + r2m(vrdav_Rr, a,i,p,q) * wm_interm_113_so_pt4(b,j,i,k) * wm_interm_59_so_pt4(a,b,k,j)
term(149) = term(149) + r2m(vrdav_Rr, a,q,p,i) * wm_interm_113_so_pt4(b,i,j,k) * wm_interm_59_so_pt4(a,b,k,j)
term(150) = term(150) + r2m(vrdav_Rr, a,q,p,i) * wm_interm_10_so_pt4(a,b,j,k) * wm_interm_49_so_pt4(b,i,k,j)
term(151) = term(151) + r2m(vrdav_Rr, a,q,p,i) * wm_interm_11_so_pt4(a,b,j,k) * wm_interm_49_so_pt4(b,i,k,j)
term(152) = term(152) + r2m(vrdav_Rr, a,q,p,i) * wm_interm_10_so_pt4(a,b,j,k) * wm_interm_49_so_pt4(b,k,i,j)
term(153) = term(153) + r2m(vrdav_Rr, a,i,p,q) * wm_interm_10_so_pt4(a,b,j,k) * wm_interm_49_so_pt4(b,k,i,j)
term(154) = term(154) + r2m(vrdav_Rr, a,q,p,i) * wm_interm_11_so_pt4(a,b,j,k) * wm_interm_49_so_pt4(b,k,i,j)
term(155) = term(155) + r2m(vrdav_Rr, a,i,p,q) * wm_interm_11_so_pt4(a,b,j,k) * wm_interm_49_so_pt4(b,k,i,j)
term(156) = term(156) + r2m(vrdav_Rr, a,q,p,i) * wm_interm_49_so_pt4(b,i,j,k) * wm_interm_5_so_pt4(a,b,k,j)
term(157) = term(157) + r2m(vrdav_Rr, a,q,p,i) * wm_interm_1_so_pt4(a,b,j,k) * wm_interm_49_so_pt4(b,i,k,j)
term(158) = term(158) + r2m(vrdav_Rr, a,q,p,i) * wm_interm_1_so_pt4(a,b,j,k) * wm_interm_49_so_pt4(b,k,i,j)
term(159) = term(159) + r2m(vrdav_Rr, a,i,p,q) * wm_interm_1_so_pt4(a,b,j,k) * wm_interm_49_so_pt4(b,k,i,j)
term(160) = term(160) + r2m(vrdav_Rr, a,q,p,i) * wm_interm_49_so_pt4(b,j,i,k) * wm_interm_5_so_pt4(a,b,k,j)
term(161) = term(161) + r2m(vrdav_Rr, a,i,p,q) * wm_interm_49_so_pt4(b,j,i,k) * wm_interm_5_so_pt4(a,b,k,j)
term(162) = term(162) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_113_so_pt4(b,j,i,k) * wm_interm_54_so_pt4(a,b,k,q)
term(163) = term(163) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_113_so_pt4(b,j,i,k) * wm_interm_55_so_pt4(a,b,k,q)
term(164) = term(164) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_22_so_pt4(b,j,i,k) * wm_interm_56_so_pt4(a,b,k,q)
term(165) = term(165) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_47_so_pt4(b,j,i,k) * wm_interm_5_so_pt4(a,b,k,q)
term(166) = term(166) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_113_so_pt4(b,j,i,k) * wm_interm_59_so_pt4(a,b,k,q)
term(167) = term(167) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_10_so_pt4(a,b,k,q) * wm_interm_49_so_pt4(b,j,i,k)
term(168) = term(168) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_1_so_pt4(a,b,k,q) * wm_interm_49_so_pt4(b,j,i,k)
term(169) = term(169) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_11_so_pt4(a,b,k,q) * wm_interm_49_so_pt4(b,j,i,k)
term(170) = term(170) + t2(a,p,q,i) * wm_interm_22_so_pt4(b,j,i,k) * wm_interm_63_so_pt4(a,b,k,j)
term(171) = term(171) + t2(a,p,q,i) * wm_interm_22_so_pt4(b,j,i,k) * wm_interm_64_so_pt4(a,b,k,j)
term(172) = term(172) + t2(a,p,q,i) * wm_interm_22_so_pt4(b,j,i,k) * wm_interm_66_so_pt4(a,b,k,j)
term(173) = term(173) + t2(a,p,q,i) * wm_interm_22_so_pt4(b,i,j,k) * wm_interm_64_so_pt4(a,b,k,j)
term(174) = term(174) + t2(a,p,q,i) * wm_interm_22_so_pt4(b,i,j,k) * wm_interm_65_so_pt4(a,b,k,j)
term(175) = term(175) + t2(a,p,q,i) * wm_interm_22_so_pt4(b,i,j,k) * wm_interm_66_so_pt4(a,b,k,j)
term(176) = term(176) + t2(a,p,q,i) * wm_interm_22_so_pt4(b,i,j,k) * wm_interm_75_so_pt4(a,b,k,j)
term(177) = term(177) + t2(a,p,q,i) * wm_interm_22_so_pt4(b,i,j,k) * wm_interm_77_so_pt4(a,b,k,j)
term(178) = term(178) + t2(a,p,q,i) * wm_interm_22_so_pt4(b,i,j,k) * wm_interm_79_so_pt4(a,b,k,j)
term(179) = term(179) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_113_so_pt4(b,j,i,k) * wm_interm_54_so_pt4(a,b,k,q)
term(180) = term(180) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_22_so_pt4(b,j,i,k) * wm_interm_59_so_pt4(a,b,k,q)
term(181) = term(181) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_22_so_pt4(b,j,i,k) * wm_interm_56_so_pt4(a,b,k,q)
term(182) = term(182) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_113_so_pt4(b,j,i,k) * wm_interm_55_so_pt4(a,b,k,q)
term(183) = term(183) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_47_so_pt4(b,j,i,k) * wm_interm_5_so_pt4(a,b,k,q)
term(184) = term(184) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_113_so_pt4(b,j,i,k) * wm_interm_56_so_pt4(a,b,k,q)
term(185) = term(185) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_113_so_pt4(b,j,i,k) * wm_interm_59_so_pt4(a,b,k,q)
term(186) = term(186) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_10_so_pt4(a,b,k,q) * wm_interm_49_so_pt4(b,j,i,k)
term(187) = term(187) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_1_so_pt4(a,b,k,q) * wm_interm_49_so_pt4(b,j,i,k)
term(188) = term(188) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_11_so_pt4(a,b,k,q) * wm_interm_49_so_pt4(b,j,i,k)
term(189) = term(189) + t2(a,p,q,i) * wm_interm_22_so_pt4(b,j,i,k) * wm_interm_93_so_pt4(a,b,k,j)
term(190) = term(190) + t2(a,p,q,i) * wm_interm_22_so_pt4(b,j,i,k) * wm_interm_94_so_pt4(a,b,k,j)
term(191) = term(191) + t2(a,p,q,i) * wm_interm_22_so_pt4(b,i,j,k) * wm_interm_94_so_pt4(a,b,k,j)
term(192) = term(192) + t2(a,p,q,i) * wm_interm_22_so_pt4(b,i,j,k) * wm_interm_93_so_pt4(a,b,k,j)
term(193) = term(193) + t2(a,p,q,i) * wm_interm_100_so_pt4(a,b,j,k) * wm_interm_22_so_pt4(b,i,k,j)
term(194) = term(194) + t2(a,p,q,i) * wm_interm_102_so_pt4(a,b,j,k) * wm_interm_22_so_pt4(b,i,k,j)
end do 
end do 
end do 
end do 
end do 

term(92) = term(92) * (-2.0d+0) 
term(93) = term(93) * (4.0d+0) 
term(94) = term(94) * (4.0d+0) 
term(95) = term(95) * (-8.0d+0) 
term(96) = term(96) * (-1.0d+0) 
term(97) = term(97) * (2.0d+0) 
term(98) = term(98) * (-1.0d+0) 
term(99) = term(99) * (2.0d+0) 
term(100) = term(100) * (-1.0d+0) 
term(101) = term(101) * (2.0d+0) 
term(102) = term(102) * (2.0d+0) 
term(103) = term(103) * (-4.0d+0) 
term(104) = term(104) * (-1.0d+0) 
term(105) = term(105) * (2.0d+0) 
term(106) = term(106) * (-1.0d+0) 
term(107) = term(107) * (2.0d+0) 
term(108) = term(108) * (-1.0d+0) 
term(109) = term(109) * (2.0d+0) 
term(110) = term(110) * (-1.0d+0) 
term(111) = term(111) * (2.0d+0) 
term(112) = term(112) * (2.0d+0) 
term(113) = term(113) * (-4.0d+0) 
term(114) = term(114) * (-1.0d+0) 
term(115) = term(115) * (2.0d+0) 
term(116) = term(116) * (-1.0d+0) 
term(117) = term(117) * (2.0d+0) 
term(118) = term(118) * (-1.0d+0) 
term(119) = term(119) * (2.0d+0) 
term(120) = term(120) * (-8.0d+0) 
term(121) = term(121) * (8.0d+0) 
term(122) = term(122) * (16.0d+0) 
term(123) = term(123) * (16.0d+0) 
term(124) = term(124) * (-16.0d+0) 
term(125) = term(125) * (-32.0d+0) 
term(126) = term(126) * (-4.0d+0) 
term(127) = term(127) * (4.0d+0) 
term(128) = term(128) * (8.0d+0) 
term(129) = term(129) * (-4.0d+0) 
term(130) = term(130) * (8.0d+0) 
term(131) = term(131) * (-8.0d+0) 
term(132) = term(132) * (-4.0d+0) 
term(133) = term(133) * (8.0d+0) 
term(134) = term(134) * (-8.0d+0) 
term(135) = term(135) * (8.0d+0) 
term(136) = term(136) * (-16.0d+0) 
term(137) = term(137) * (16.0d+0) 
term(138) = term(138) * (-4.0d+0) 
term(139) = term(139) * (8.0d+0) 
term(140) = term(140) * (-4.0d+0) 
term(141) = term(141) * (4.0d+0) 
term(142) = term(142) * (8.0d+0) 
term(143) = term(143) * (-8.0d+0) 
term(144) = term(144) * (-4.0d+0) 
term(145) = term(145) * (8.0d+0) 
term(146) = term(146) * (-8.0d+0) 
term(147) = term(147) * (-4.0d+0) 
term(148) = term(148) * (4.0d+0) 
term(149) = term(149) * (8.0d+0) 
term(150) = term(150) * (8.0d+0) 
term(151) = term(151) * (-16.0d+0) 
term(152) = term(152) * (-4.0d+0) 
term(153) = term(153) * (4.0d+0) 
term(154) = term(154) * (8.0d+0) 
term(155) = term(155) * (-8.0d+0) 
term(156) = term(156) * (-4.0d+0) 
term(157) = term(157) * (8.0d+0) 
term(158) = term(158) * (-4.0d+0) 
term(159) = term(159) * (4.0d+0) 
term(160) = term(160) * (8.0d+0) 
term(161) = term(161) * (-8.0d+0) 
term(162) = term(162) * (2.0d+0) 
term(163) = term(163) * (-1.0d+0) 
term(164) = term(164) * (-0.5d+0) 
term(165) = term(165) * (-0.5d+0) 
term(166) = term(166) * (-0.5d+0) 
term(167) = term(167) * (-0.5d+0) 
term(168) = term(168) * (-0.5d+0) 
term(170) = term(170) * (-1.0d+0) 
term(171) = term(171) * (2.0d+0) 
term(172) = term(172) * (-1.0d+0) 
term(173) = term(173) * (-1.0d+0) 
term(174) = term(174) * (-1.0d+0) 
term(175) = term(175) * (2.0d+0) 
term(176) = term(176) * (-1.0d+0) 
term(177) = term(177) * (-1.0d+0) 
term(178) = term(178) * (2.0d+0) 
term(179) = term(179) * (8.0d+0) 
term(180) = term(180) * (2.0d+0) 
term(181) = term(181) * (-2.0d+0) 
term(182) = term(182) * (-4.0d+0) 
term(183) = term(183) * (-1.0d+0) 
term(184) = term(184) * (2.0d+0) 
term(185) = term(185) * (-2.0d+0) 
term(186) = term(186) * (-1.0d+0) 
term(187) = term(187) * (-1.0d+0) 
term(188) = term(188) * (2.0d+0) 
term(189) = term(189) * (-4.0d+0) 
term(190) = term(190) * (4.0d+0) 
term(191) = term(191) * (-4.0d+0) 
term(192) = term(192) * (4.0d+0) 
term(193) = term(193) * (-4.0d+0) 
term(194) = term(194) * (4.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(195) = term(195) + t2(a,p,j,i) * wm_interm_47_so_pt4(b,i,j,k) * wm_interm_67_so_pt4(b,a,q,k)
term(196) = term(196) + t2(a,p,j,i) * wm_interm_47_so_pt4(b,i,j,k) * wm_interm_68_so_pt4(b,a,q,k)
term(197) = term(197) + t2(a,p,j,i) * wm_interm_47_so_pt4(b,i,j,k) * wm_interm_70_so_pt4(b,a,q,k)
term(198) = term(198) + t2(a,p,j,i) * wm_interm_47_so_pt4(b,i,j,k) * wm_interm_69_so_pt4(b,a,q,k)
term(199) = term(199) + t2(a,p,j,i) * wm_interm_47_so_pt4(b,i,j,k) * wm_interm_74_so_pt4(b,a,q,k)
term(200) = term(200) + t2(a,p,j,i) * wm_interm_47_so_pt4(b,i,j,k) * wm_interm_84_so_pt4(b,a,q,k)
term(201) = term(201) + t2(a,p,j,i) * wm_interm_49_so_pt4(b,i,j,k) * wm_interm_67_so_pt4(b,a,q,k)
term(202) = term(202) + t2(a,p,j,i) * wm_interm_49_so_pt4(b,i,j,k) * wm_interm_68_so_pt4(b,a,q,k)
term(203) = term(203) + t2(a,p,j,i) * wm_interm_49_so_pt4(b,i,j,k) * wm_interm_74_so_pt4(b,a,q,k)
term(204) = term(204) + t2(a,p,j,i) * wm_interm_49_so_pt4(b,i,j,k) * wm_interm_73_so_pt4(b,a,q,k)
term(205) = term(205) + t2(a,p,j,i) * wm_interm_49_so_pt4(b,i,j,k) * wm_interm_84_so_pt4(b,a,q,k)
term(206) = term(206) + t2(a,p,j,i) * wm_interm_49_so_pt4(b,i,j,k) * wm_interm_69_so_pt4(b,a,q,k)
term(207) = term(207) + t2(a,p,q,i) * wm_interm_47_so_pt4(b,i,j,k) * wm_interm_67_so_pt4(b,a,j,k)
term(208) = term(208) + t2(a,p,q,i) * wm_interm_47_so_pt4(b,i,j,k) * wm_interm_68_so_pt4(b,a,j,k)
term(209) = term(209) + t2(a,p,q,i) * wm_interm_47_so_pt4(b,i,j,k) * wm_interm_69_so_pt4(b,a,j,k)
term(210) = term(210) + t2(a,p,q,i) * wm_interm_47_so_pt4(b,i,j,k) * wm_interm_70_so_pt4(b,a,j,k)
term(211) = term(211) + t2(a,p,q,i) * wm_interm_47_so_pt4(b,i,j,k) * wm_interm_73_so_pt4(b,a,j,k)
term(212) = term(212) + t2(a,p,q,i) * wm_interm_47_so_pt4(b,i,j,k) * wm_interm_74_so_pt4(b,a,j,k)
term(213) = term(213) + t2(a,p,q,i) * wm_interm_47_so_pt4(b,j,i,k) * wm_interm_70_so_pt4(b,a,j,k)
term(214) = term(214) + t2(a,p,q,i) * wm_interm_47_so_pt4(b,j,i,k) * wm_interm_84_so_pt4(b,a,j,k)
term(215) = term(215) + t2(a,p,q,i) * wm_interm_47_so_pt4(b,j,i,k) * wm_interm_73_so_pt4(b,a,j,k)
term(216) = term(216) + t2(a,p,q,i) * wm_interm_49_so_pt4(b,j,i,k) * wm_interm_67_so_pt4(b,a,j,k)
term(217) = term(217) + t2(a,p,q,i) * wm_interm_49_so_pt4(b,j,i,k) * wm_interm_68_so_pt4(b,a,j,k)
term(218) = term(218) + t2(a,p,q,i) * wm_interm_49_so_pt4(b,j,i,k) * wm_interm_74_so_pt4(b,a,j,k)
term(219) = term(219) + t2(a,p,q,i) * wm_interm_49_so_pt4(b,i,j,k) * wm_interm_84_so_pt4(b,a,j,k)
term(220) = term(220) + t2(a,p,q,i) * wm_interm_49_so_pt4(b,i,j,k) * wm_interm_73_so_pt4(b,a,j,k)
term(221) = term(221) + t2(a,p,q,i) * wm_interm_49_so_pt4(b,i,j,k) * wm_interm_70_so_pt4(b,a,j,k)
term(222) = term(222) + t2(a,p,q,i) * wm_interm_49_so_pt4(b,j,i,k) * wm_interm_73_so_pt4(b,a,j,k)
term(223) = term(223) + t2(a,p,q,i) * wm_interm_49_so_pt4(b,j,i,k) * wm_interm_69_so_pt4(b,a,j,k)
term(224) = term(224) + t2(a,p,q,i) * wm_interm_49_so_pt4(b,j,i,k) * wm_interm_70_so_pt4(b,a,j,k)
term(225) = term(225) + t2(a,p,j,i) * wm_interm_47_so_pt4(b,i,j,k) * wm_interm_95_so_pt4(b,a,q,k)
term(226) = term(226) + t2(a,p,j,i) * wm_interm_47_so_pt4(b,i,j,k) * wm_interm_96_so_pt4(b,a,q,k)
term(227) = term(227) + t2(a,p,j,i) * wm_interm_47_so_pt4(b,i,j,k) * wm_interm_98_so_pt4(b,a,q,k)
term(228) = term(228) + t2(a,p,j,i) * wm_interm_47_so_pt4(b,i,j,k) * wm_interm_97_so_pt4(b,a,q,k)
term(229) = term(229) + t2(a,p,j,i) * wm_interm_49_so_pt4(b,i,j,k) * wm_interm_95_so_pt4(b,a,q,k)
term(230) = term(230) + t2(a,p,j,i) * wm_interm_49_so_pt4(b,i,j,k) * wm_interm_96_so_pt4(b,a,q,k)
term(231) = term(231) + t2(a,p,j,i) * wm_interm_49_so_pt4(b,i,j,k) * wm_interm_97_so_pt4(b,a,q,k)
term(232) = term(232) + t2(a,p,j,i) * wm_interm_49_so_pt4(b,i,j,k) * wm_interm_98_so_pt4(b,a,q,k)
term(233) = term(233) + t2(a,p,q,i) * wm_interm_47_so_pt4(b,i,j,k) * wm_interm_95_so_pt4(b,a,j,k)
term(234) = term(234) + t2(a,p,q,i) * wm_interm_47_so_pt4(b,i,j,k) * wm_interm_96_so_pt4(b,a,j,k)
term(235) = term(235) + t2(a,p,q,i) * wm_interm_47_so_pt4(b,i,j,k) * wm_interm_97_so_pt4(b,a,j,k)
term(236) = term(236) + t2(a,p,q,i) * wm_interm_47_so_pt4(b,i,j,k) * wm_interm_98_so_pt4(b,a,j,k)
term(237) = term(237) + t2(a,p,q,i) * wm_interm_47_so_pt4(b,j,i,k) * wm_interm_98_so_pt4(b,a,j,k)
term(238) = term(238) + t2(a,p,q,i) * wm_interm_47_so_pt4(b,j,i,k) * wm_interm_97_so_pt4(b,a,j,k)
term(239) = term(239) + t2(a,p,q,i) * wm_interm_49_so_pt4(b,j,i,k) * wm_interm_95_so_pt4(b,a,j,k)
term(240) = term(240) + t2(a,p,q,i) * wm_interm_49_so_pt4(b,j,i,k) * wm_interm_96_so_pt4(b,a,j,k)
term(241) = term(241) + t2(a,p,q,i) * wm_interm_49_so_pt4(b,i,j,k) * wm_interm_98_so_pt4(b,a,j,k)
term(242) = term(242) + t2(a,p,q,i) * wm_interm_49_so_pt4(b,i,j,k) * wm_interm_97_so_pt4(b,a,j,k)
term(243) = term(243) + t2(a,p,q,i) * wm_interm_49_so_pt4(b,j,i,k) * wm_interm_97_so_pt4(b,a,j,k)
term(244) = term(244) + t2(a,p,q,i) * wm_interm_49_so_pt4(b,j,i,k) * wm_interm_98_so_pt4(b,a,j,k)
end do 
end do 
end do 
end do 
end do 

term(196) = term(196) * (-2.0d+0) 
term(197) = term(197) * (-0.5d+0) 
term(200) = term(200) * (-0.5d+0) 
term(201) = term(201) * (-0.5d+0) 
term(203) = term(203) * (-0.5d+0) 
term(204) = term(204) * (-0.5d+0) 
term(206) = term(206) * (-0.5d+0) 
term(207) = term(207) * (-0.5d+0) 
term(209) = term(209) * (-0.5d+0) 
term(211) = term(211) * (-0.5d+0) 
term(212) = term(212) * (-0.5d+0) 
term(213) = term(213) * (-0.5d+0) 
term(214) = term(214) * (-0.5d+0) 
term(216) = term(216) * (-0.5d+0) 
term(218) = term(218) * (-0.5d+0) 
term(219) = term(219) * (-0.5d+0) 
term(221) = term(221) * (-0.5d+0) 
term(222) = term(222) * (-0.5d+0) 
term(223) = term(223) * (-0.5d+0) 
term(225) = term(225) * (4.0d+0) 
term(226) = term(226) * (-4.0d+0) 
term(227) = term(227) * (-2.0d+0) 
term(228) = term(228) * (2.0d+0) 
term(229) = term(229) * (-2.0d+0) 
term(230) = term(230) * (2.0d+0) 
term(231) = term(231) * (-2.0d+0) 
term(232) = term(232) * (2.0d+0) 
term(233) = term(233) * (-2.0d+0) 
term(234) = term(234) * (2.0d+0) 
term(235) = term(235) * (-2.0d+0) 
term(236) = term(236) * (2.0d+0) 
term(237) = term(237) * (-2.0d+0) 
term(238) = term(238) * (2.0d+0) 
term(239) = term(239) * (-2.0d+0) 
term(240) = term(240) * (2.0d+0) 
term(241) = term(241) * (-2.0d+0) 
term(242) = term(242) * (2.0d+0) 
term(243) = term(243) * (-2.0d+0) 
term(244) = term(244) * (2.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(245) = term(245) + r1(vrdav_Rr, p,i) * wm_interm_1_so_pt4(a,b,i,q) * wm_interm_23_so_pt4(b,a)
term(246) = term(246) + r1(vrdav_Rr, p,i) * wm_interm_23_so_pt4(a,b) * wm_interm_5_so_pt4(b,a,i,q)
term(247) = term(247) + r1(vrdav_Rr, p,i) * wm_interm_10_so_pt4(a,b,i,q) * wm_interm_23_so_pt4(b,a)
term(248) = term(248) + r1(vrdav_Rr, p,i) * wm_interm_11_so_pt4(a,b,i,q) * wm_interm_23_so_pt4(b,a)
term(249) = term(249) + t1(p,i) * wm_interm_10_so_pt4(a,b,i,q) * wm_interm_15_so_pt4(b,a)
term(250) = term(250) + t1(p,i) * wm_interm_11_so_pt4(a,b,i,q) * wm_interm_15_so_pt4(b,a)
term(251) = term(251) + t1(p,i) * wm_interm_15_so_pt4(a,b) * wm_interm_1_so_pt4(b,a,i,q)
term(252) = term(252) + t1(a,i) * wm_interm_19_so_pt4(a,b) * wm_interm_77_so_pt4(b,p,i,q)
term(253) = term(253) + t1(a,i) * wm_interm_20_so_pt4(a,b) * wm_interm_77_so_pt4(b,p,i,q)
term(254) = term(254) + t1(a,i) * wm_interm_19_so_pt4(a,b) * wm_interm_79_so_pt4(b,p,i,q)
term(255) = term(255) + t1(a,i) * wm_interm_20_so_pt4(a,b) * wm_interm_79_so_pt4(b,p,i,q)
term(256) = term(256) + t1(a,i) * wm_interm_19_so_pt4(a,b) * wm_interm_75_so_pt4(b,p,i,q)
term(257) = term(257) + t1(a,i) * wm_interm_20_so_pt4(a,b) * wm_interm_75_so_pt4(b,p,i,q)
term(258) = term(258) + t1(a,i) * wm_interm_19_so_pt4(a,b) * wm_interm_66_so_pt4(b,p,i,q)
term(259) = term(259) + t1(a,i) * wm_interm_19_so_pt4(a,b) * wm_interm_65_so_pt4(b,p,i,q)
term(260) = term(260) + t1(a,i) * wm_interm_20_so_pt4(a,b) * wm_interm_66_so_pt4(b,p,i,q)
term(261) = term(261) + t1(a,i) * wm_interm_20_so_pt4(a,b) * wm_interm_65_so_pt4(b,p,i,q)
term(262) = term(262) + t1(a,i) * wm_interm_19_so_pt4(a,b) * wm_interm_63_so_pt4(b,p,i,q)
term(263) = term(263) + t1(a,i) * wm_interm_20_so_pt4(a,b) * wm_interm_63_so_pt4(b,p,i,q)
term(264) = term(264) + r2p(vrdav_Rr, a,q,p,i) * wm_interm_19_so_pt4(a,b) * wm_interm_50_so_pt4(b,i)
term(265) = term(265) + r2p(vrdav_Rr, a,q,p,i) * wm_interm_20_so_pt4(a,b) * wm_interm_50_so_pt4(b,i)
term(266) = term(266) + t1(a,i) * wm_interm_57_so_pt4(a,b) * wm_interm_70_so_pt4(p,b,q,i)
term(267) = term(267) + t1(a,i) * wm_interm_57_so_pt4(a,b) * wm_interm_69_so_pt4(p,b,q,i)
term(268) = term(268) + t1(a,i) * wm_interm_57_so_pt4(a,b) * wm_interm_67_so_pt4(p,b,q,i)
term(269) = term(269) + t1(a,i) * wm_interm_57_so_pt4(a,b) * wm_interm_68_so_pt4(p,b,q,i)
term(270) = term(270) + t1(a,i) * wm_interm_57_so_pt4(a,b) * wm_interm_84_so_pt4(p,b,q,i)
term(271) = term(271) + t1(a,i) * wm_interm_57_so_pt4(a,b) * wm_interm_74_so_pt4(p,b,q,i)
term(272) = term(272) + t1(a,i) * wm_interm_58_so_pt4(a,b) * wm_interm_67_so_pt4(p,b,q,i)
term(273) = term(273) + t1(a,i) * wm_interm_58_so_pt4(a,b) * wm_interm_68_so_pt4(p,b,q,i)
term(274) = term(274) + t1(a,i) * wm_interm_58_so_pt4(a,b) * wm_interm_70_so_pt4(p,b,q,i)
term(275) = term(275) + t1(a,i) * wm_interm_58_so_pt4(a,b) * wm_interm_69_so_pt4(p,b,q,i)
term(276) = term(276) + t1(a,i) * wm_interm_58_so_pt4(a,b) * wm_interm_74_so_pt4(p,b,q,i)
term(277) = term(277) + t1(a,i) * wm_interm_58_so_pt4(a,b) * wm_interm_84_so_pt4(p,b,q,i)
term(278) = term(278) + r2p(vrdav_Rr, a,q,p,i) * wm_interm_19_so_pt4(a,b) * wm_interm_51_so_pt4(b,i)
term(279) = term(279) + r2p(vrdav_Rr, a,q,p,i) * wm_interm_20_so_pt4(a,b) * wm_interm_51_so_pt4(b,i)
term(280) = term(280) + t1(a,i) * wm_interm_100_so_pt4(b,p,i,q) * wm_interm_19_so_pt4(a,b)
term(281) = term(281) + t1(a,i) * wm_interm_100_so_pt4(b,p,i,q) * wm_interm_20_so_pt4(a,b)
term(282) = term(282) + t1(a,i) * wm_interm_102_so_pt4(b,p,i,q) * wm_interm_19_so_pt4(a,b)
term(283) = term(283) + t1(a,i) * wm_interm_102_so_pt4(b,p,i,q) * wm_interm_20_so_pt4(a,b)
term(284) = term(284) + t1(a,i) * wm_interm_19_so_pt4(a,b) * wm_interm_93_so_pt4(b,p,i,q)
term(285) = term(285) + t1(a,i) * wm_interm_19_so_pt4(a,b) * wm_interm_94_so_pt4(b,p,i,q)
term(286) = term(286) + t1(a,i) * wm_interm_20_so_pt4(a,b) * wm_interm_93_so_pt4(b,p,i,q)
term(287) = term(287) + t1(a,i) * wm_interm_20_so_pt4(a,b) * wm_interm_94_so_pt4(b,p,i,q)
term(288) = term(288) + r2m(vrdav_Rr, a,q,p,i) * wm_interm_19_so_pt4(a,b) * wm_interm_50_so_pt4(b,i)
term(289) = term(289) + r2m(vrdav_Rr, a,q,p,i) * wm_interm_20_so_pt4(a,b) * wm_interm_50_so_pt4(b,i)
term(290) = term(290) + t1(a,i) * wm_interm_57_so_pt4(a,b) * wm_interm_98_so_pt4(p,b,q,i)
term(291) = term(291) + t1(a,i) * wm_interm_57_so_pt4(a,b) * wm_interm_97_so_pt4(p,b,q,i)
term(292) = term(292) + t1(a,i) * wm_interm_57_so_pt4(a,b) * wm_interm_95_so_pt4(p,b,q,i)
term(293) = term(293) + t1(a,i) * wm_interm_57_so_pt4(a,b) * wm_interm_96_so_pt4(p,b,q,i)
term(294) = term(294) + t1(a,i) * wm_interm_58_so_pt4(a,b) * wm_interm_95_so_pt4(p,b,q,i)
term(295) = term(295) + t1(a,i) * wm_interm_58_so_pt4(a,b) * wm_interm_96_so_pt4(p,b,q,i)
term(296) = term(296) + t1(a,i) * wm_interm_58_so_pt4(a,b) * wm_interm_98_so_pt4(p,b,q,i)
term(297) = term(297) + t1(a,i) * wm_interm_58_so_pt4(a,b) * wm_interm_97_so_pt4(p,b,q,i)
term(298) = term(298) + r2m(vrdav_Rr, a,q,p,i) * wm_interm_19_so_pt4(a,b) * wm_interm_51_so_pt4(b,i)
term(299) = term(299) + r2m(vrdav_Rr, a,q,p,i) * wm_interm_20_so_pt4(a,b) * wm_interm_51_so_pt4(b,i)
term(300) = term(300) + t1(p,i) * wm_interm_19_so_pt4(a,b) * wm_interm_64_so_pt4(b,a,i,q)
term(301) = term(301) + t1(p,i) * wm_interm_19_so_pt4(a,b) * wm_interm_63_so_pt4(b,a,i,q)
term(302) = term(302) + t1(p,i) * wm_interm_20_so_pt4(a,b) * wm_interm_64_so_pt4(b,a,i,q)
term(303) = term(303) + t1(p,i) * wm_interm_20_so_pt4(a,b) * wm_interm_63_so_pt4(b,a,i,q)
term(304) = term(304) + t1(p,i) * wm_interm_19_so_pt4(a,b) * wm_interm_65_so_pt4(b,a,i,q)
term(305) = term(305) + t1(p,i) * wm_interm_20_so_pt4(a,b) * wm_interm_65_so_pt4(b,a,i,q)
term(306) = term(306) + t1(p,i) * wm_interm_19_so_pt4(a,b) * wm_interm_75_so_pt4(b,a,i,q)
term(307) = term(307) + t1(p,i) * wm_interm_20_so_pt4(a,b) * wm_interm_75_so_pt4(b,a,i,q)
term(308) = term(308) + t1(p,i) * wm_interm_19_so_pt4(a,b) * wm_interm_77_so_pt4(b,a,i,q)
term(309) = term(309) + t1(p,i) * wm_interm_20_so_pt4(a,b) * wm_interm_77_so_pt4(b,a,i,q)
term(310) = term(310) + t1(p,i) * wm_interm_19_so_pt4(a,b) * wm_interm_79_so_pt4(b,a,i,q)
term(311) = term(311) + t1(p,i) * wm_interm_20_so_pt4(a,b) * wm_interm_79_so_pt4(b,a,i,q)
term(312) = term(312) + t1(p,i) * wm_interm_57_so_pt4(a,b) * wm_interm_67_so_pt4(a,b,q,i)
term(313) = term(313) + t1(p,i) * wm_interm_57_so_pt4(a,b) * wm_interm_68_so_pt4(a,b,q,i)
term(314) = term(314) + t1(p,i) * wm_interm_57_so_pt4(a,b) * wm_interm_74_so_pt4(a,b,q,i)
term(315) = term(315) + t1(p,i) * wm_interm_57_so_pt4(a,b) * wm_interm_69_so_pt4(a,b,q,i)
term(316) = term(316) + t1(p,i) * wm_interm_57_so_pt4(a,b) * wm_interm_73_so_pt4(a,b,q,i)
term(317) = term(317) + t1(p,i) * wm_interm_57_so_pt4(a,b) * wm_interm_84_so_pt4(a,b,q,i)
term(318) = term(318) + t1(p,i) * wm_interm_58_so_pt4(a,b) * wm_interm_67_so_pt4(a,b,q,i)
term(319) = term(319) + t1(p,i) * wm_interm_58_so_pt4(a,b) * wm_interm_68_so_pt4(a,b,q,i)
term(320) = term(320) + t1(p,i) * wm_interm_58_so_pt4(a,b) * wm_interm_69_so_pt4(a,b,q,i)
term(321) = term(321) + t1(p,i) * wm_interm_58_so_pt4(a,b) * wm_interm_74_so_pt4(a,b,q,i)
term(322) = term(322) + t1(p,i) * wm_interm_58_so_pt4(a,b) * wm_interm_73_so_pt4(a,b,q,i)
term(323) = term(323) + t1(p,i) * wm_interm_58_so_pt4(a,b) * wm_interm_84_so_pt4(a,b,q,i)
term(324) = term(324) + t1(p,i) * wm_interm_19_so_pt4(a,b) * wm_interm_94_so_pt4(b,a,i,q)
term(325) = term(325) + t1(p,i) * wm_interm_19_so_pt4(a,b) * wm_interm_93_so_pt4(b,a,i,q)
term(326) = term(326) + t1(p,i) * wm_interm_20_so_pt4(a,b) * wm_interm_94_so_pt4(b,a,i,q)
term(327) = term(327) + t1(p,i) * wm_interm_20_so_pt4(a,b) * wm_interm_93_so_pt4(b,a,i,q)
term(328) = term(328) + t1(p,i) * wm_interm_100_so_pt4(a,b,i,q) * wm_interm_19_so_pt4(b,a)
term(329) = term(329) + t1(p,i) * wm_interm_100_so_pt4(a,b,i,q) * wm_interm_20_so_pt4(b,a)
term(330) = term(330) + t1(p,i) * wm_interm_102_so_pt4(a,b,i,q) * wm_interm_19_so_pt4(b,a)
term(331) = term(331) + t1(p,i) * wm_interm_102_so_pt4(a,b,i,q) * wm_interm_20_so_pt4(b,a)
term(332) = term(332) + t1(p,i) * wm_interm_57_so_pt4(a,b) * wm_interm_95_so_pt4(a,b,q,i)
term(333) = term(333) + t1(p,i) * wm_interm_57_so_pt4(a,b) * wm_interm_96_so_pt4(a,b,q,i)
term(334) = term(334) + t1(p,i) * wm_interm_57_so_pt4(a,b) * wm_interm_97_so_pt4(a,b,q,i)
term(335) = term(335) + t1(p,i) * wm_interm_57_so_pt4(a,b) * wm_interm_98_so_pt4(a,b,q,i)
term(336) = term(336) + t1(p,i) * wm_interm_58_so_pt4(a,b) * wm_interm_95_so_pt4(a,b,q,i)
term(337) = term(337) + t1(p,i) * wm_interm_58_so_pt4(a,b) * wm_interm_96_so_pt4(a,b,q,i)
term(338) = term(338) + t1(p,i) * wm_interm_58_so_pt4(a,b) * wm_interm_97_so_pt4(a,b,q,i)
term(339) = term(339) + t1(p,i) * wm_interm_58_so_pt4(a,b) * wm_interm_98_so_pt4(a,b,q,i)
term(340) = term(340) + t1(a,i) * wm_interm_5_so_pt4(b,p,i,q) * wm_interm_90_so_pt4(a,b)
term(341) = term(341) + t1(a,i) * wm_interm_5_so_pt4(b,p,i,q) * wm_interm_91_so_pt4(a,b)
term(342) = term(342) + t1(a,i) * wm_interm_5_so_pt4(b,p,i,q) * wm_interm_92_so_pt4(a,b)
term(343) = term(343) + t1(a,i) * wm_interm_110_so_pt4(a,b) * wm_interm_5_so_pt4(b,p,i,q)
term(344) = term(344) + t1(a,i) * wm_interm_111_so_pt4(a,b) * wm_interm_5_so_pt4(b,p,i,q)
term(345) = term(345) + t2(a,b,q,i) * wm_interm_50_so_pt4(b,i) * wm_interm_81_so_pt4(p,a)
term(346) = term(346) + t2(a,b,q,i) * wm_interm_50_so_pt4(b,i) * wm_interm_82_so_pt4(p,a)
term(347) = term(347) + t2(a,b,q,i) * wm_interm_50_so_pt4(b,i) * wm_interm_83_so_pt4(p,a)
term(348) = term(348) + t2(a,b,q,i) * wm_interm_51_so_pt4(b,i) * wm_interm_81_so_pt4(p,a)
term(349) = term(349) + t2(a,b,q,i) * wm_interm_51_so_pt4(b,i) * wm_interm_82_so_pt4(p,a)
term(350) = term(350) + t2(a,b,q,i) * wm_interm_51_so_pt4(b,i) * wm_interm_83_so_pt4(p,a)
term(351) = term(351) + r2p(vrdav_Rr, a,q,b,i) * wm_interm_19_so_pt4(a,p) * wm_interm_50_so_pt4(b,i)
term(352) = term(352) + r2p(vrdav_Rr, a,q,b,i) * wm_interm_20_so_pt4(a,p) * wm_interm_50_so_pt4(b,i)
term(353) = term(353) + r2p(vrdav_Rr, a,q,b,i) * wm_interm_19_so_pt4(a,p) * wm_interm_51_so_pt4(b,i)
term(354) = term(354) + r2p(vrdav_Rr, a,q,b,i) * wm_interm_20_so_pt4(a,p) * wm_interm_51_so_pt4(b,i)
term(355) = term(355) + t2(a,b,q,i) * wm_interm_104_so_pt4(p,a) * wm_interm_50_so_pt4(b,i)
term(356) = term(356) + t2(a,b,q,i) * wm_interm_105_so_pt4(p,a) * wm_interm_50_so_pt4(b,i)
term(357) = term(357) + t2(a,b,q,i) * wm_interm_112_so_pt4(p,a) * wm_interm_50_so_pt4(b,i)
term(358) = term(358) + t2(a,b,q,i) * wm_interm_104_so_pt4(p,a) * wm_interm_51_so_pt4(b,i)
term(359) = term(359) + t2(a,b,q,i) * wm_interm_105_so_pt4(p,a) * wm_interm_51_so_pt4(b,i)
term(360) = term(360) + t2(a,b,q,i) * wm_interm_112_so_pt4(p,a) * wm_interm_51_so_pt4(b,i)
term(361) = term(361) + r2m(vrdav_Rr, a,q,b,i) * wm_interm_19_so_pt4(a,p) * wm_interm_50_so_pt4(b,i)
term(362) = term(362) + r2m(vrdav_Rr, a,q,b,i) * wm_interm_20_so_pt4(a,p) * wm_interm_50_so_pt4(b,i)
term(363) = term(363) + r2m(vrdav_Rr, a,q,b,i) * wm_interm_19_so_pt4(a,p) * wm_interm_51_so_pt4(b,i)
term(364) = term(364) + r2m(vrdav_Rr, a,q,b,i) * wm_interm_20_so_pt4(a,p) * wm_interm_51_so_pt4(b,i)
term(365) = term(365) + t1(p,i) * wm_interm_10_so_pt4(a,b,i,q) * wm_interm_91_so_pt4(b,a)
term(366) = term(366) + t1(p,i) * wm_interm_10_so_pt4(a,b,i,q) * wm_interm_92_so_pt4(b,a)
term(367) = term(367) + t1(p,i) * wm_interm_10_so_pt4(a,b,i,q) * wm_interm_90_so_pt4(b,a)
term(368) = term(368) + t1(p,i) * wm_interm_11_so_pt4(a,b,i,q) * wm_interm_91_so_pt4(b,a)
term(369) = term(369) + t1(p,i) * wm_interm_11_so_pt4(a,b,i,q) * wm_interm_92_so_pt4(b,a)
term(370) = term(370) + t1(p,i) * wm_interm_11_so_pt4(a,b,i,q) * wm_interm_90_so_pt4(b,a)
term(371) = term(371) + t1(p,i) * wm_interm_59_so_pt4(a,b,i,q) * wm_interm_83_so_pt4(a,b)
term(372) = term(372) + t1(p,i) * wm_interm_59_so_pt4(a,b,i,q) * wm_interm_81_so_pt4(a,b)
term(373) = term(373) + t1(p,i) * wm_interm_59_so_pt4(a,b,i,q) * wm_interm_82_so_pt4(a,b)
term(374) = term(374) + t1(p,i) * wm_interm_1_so_pt4(a,b,i,q) * wm_interm_91_so_pt4(b,a)
term(375) = term(375) + t1(p,i) * wm_interm_1_so_pt4(a,b,i,q) * wm_interm_92_so_pt4(b,a)
term(376) = term(376) + t1(p,i) * wm_interm_1_so_pt4(a,b,i,q) * wm_interm_90_so_pt4(b,a)
term(377) = term(377) + t1(p,i) * wm_interm_55_so_pt4(a,b,i,q) * wm_interm_83_so_pt4(a,b)
term(378) = term(378) + t1(p,i) * wm_interm_54_so_pt4(a,b,i,q) * wm_interm_83_so_pt4(a,b)
term(379) = term(379) + t1(p,i) * wm_interm_55_so_pt4(a,b,i,q) * wm_interm_81_so_pt4(a,b)
term(380) = term(380) + t1(p,i) * wm_interm_54_so_pt4(a,b,i,q) * wm_interm_81_so_pt4(a,b)
term(381) = term(381) + t1(p,i) * wm_interm_55_so_pt4(a,b,i,q) * wm_interm_82_so_pt4(a,b)
term(382) = term(382) + t1(p,i) * wm_interm_54_so_pt4(a,b,i,q) * wm_interm_82_so_pt4(a,b)
term(383) = term(383) + t1(p,i) * wm_interm_10_so_pt4(a,b,i,q) * wm_interm_110_so_pt4(b,a)
term(384) = term(384) + t1(p,i) * wm_interm_10_so_pt4(a,b,i,q) * wm_interm_111_so_pt4(b,a)
term(385) = term(385) + t1(p,i) * wm_interm_110_so_pt4(a,b) * wm_interm_11_so_pt4(b,a,i,q)
term(386) = term(386) + t1(p,i) * wm_interm_111_so_pt4(a,b) * wm_interm_11_so_pt4(b,a,i,q)
term(387) = term(387) + t1(p,i) * wm_interm_104_so_pt4(a,b) * wm_interm_59_so_pt4(a,b,i,q)
term(388) = term(388) + t1(p,i) * wm_interm_105_so_pt4(a,b) * wm_interm_59_so_pt4(a,b,i,q)
term(389) = term(389) + t1(p,i) * wm_interm_110_so_pt4(a,b) * wm_interm_1_so_pt4(b,a,i,q)
term(390) = term(390) + t1(p,i) * wm_interm_111_so_pt4(a,b) * wm_interm_1_so_pt4(b,a,i,q)
term(391) = term(391) + t1(p,i) * wm_interm_104_so_pt4(a,b) * wm_interm_55_so_pt4(a,b,i,q)
term(392) = term(392) + t1(p,i) * wm_interm_104_so_pt4(a,b) * wm_interm_54_so_pt4(a,b,i,q)
term(393) = term(393) + t1(p,i) * wm_interm_105_so_pt4(a,b) * wm_interm_55_so_pt4(a,b,i,q)
term(394) = term(394) + t1(p,i) * wm_interm_105_so_pt4(a,b) * wm_interm_54_so_pt4(a,b,i,q)
end do 
end do 
end do 

term(245) = term(245) * (2.0d+0) 
term(246) = term(246) * (-4.0d+0) 
term(247) = term(247) * (2.0d+0) 
term(248) = term(248) * (-4.0d+0) 
term(249) = term(249) * (4.0d+0) 
term(250) = term(250) * (-8.0d+0) 
term(251) = term(251) * (4.0d+0) 
term(252) = term(252) * (4.0d+0) 
term(253) = term(253) * (-8.0d+0) 
term(254) = term(254) * (-8.0d+0) 
term(255) = term(255) * (16.0d+0) 
term(256) = term(256) * (4.0d+0) 
term(257) = term(257) * (-8.0d+0) 
term(258) = term(258) * (-2.0d+0) 
term(259) = term(259) * (4.0d+0) 
term(260) = term(260) * (4.0d+0) 
term(261) = term(261) * (-8.0d+0) 
term(262) = term(262) * (-2.0d+0) 
term(263) = term(263) * (4.0d+0) 
term(264) = term(264) * (4.0d+0) 
term(265) = term(265) * (-8.0d+0) 
term(266) = term(266) * (4.0d+0) 
term(267) = term(267) * (-8.0d+0) 
term(268) = term(268) * (-8.0d+0) 
term(269) = term(269) * (16.0d+0) 
term(270) = term(270) * (4.0d+0) 
term(271) = term(271) * (-8.0d+0) 
term(272) = term(272) * (4.0d+0) 
term(273) = term(273) * (-8.0d+0) 
term(274) = term(274) * (-2.0d+0) 
term(275) = term(275) * (4.0d+0) 
term(276) = term(276) * (4.0d+0) 
term(277) = term(277) * (-2.0d+0) 
term(278) = term(278) * (-2.0d+0) 
term(279) = term(279) * (4.0d+0) 
term(280) = term(280) * (16.0d+0) 
term(281) = term(281) * (-32.0d+0) 
term(282) = term(282) * (-16.0d+0) 
term(283) = term(283) * (32.0d+0) 
term(284) = term(284) * (-8.0d+0) 
term(285) = term(285) * (8.0d+0) 
term(286) = term(286) * (16.0d+0) 
term(287) = term(287) * (-16.0d+0) 
term(288) = term(288) * (16.0d+0) 
term(289) = term(289) * (-32.0d+0) 
term(290) = term(290) * (16.0d+0) 
term(291) = term(291) * (-16.0d+0) 
term(292) = term(292) * (-32.0d+0) 
term(293) = term(293) * (32.0d+0) 
term(294) = term(294) * (16.0d+0) 
term(295) = term(295) * (-16.0d+0) 
term(296) = term(296) * (-8.0d+0) 
term(297) = term(297) * (8.0d+0) 
term(298) = term(298) * (-8.0d+0) 
term(299) = term(299) * (16.0d+0) 
term(300) = term(300) * (-1.0d+0) 
term(301) = term(301) * (2.0d+0) 
term(302) = term(302) * (2.0d+0) 
term(303) = term(303) * (-4.0d+0) 
term(304) = term(304) * (-1.0d+0) 
term(305) = term(305) * (2.0d+0) 
term(306) = term(306) * (-1.0d+0) 
term(307) = term(307) * (2.0d+0) 
term(308) = term(308) * (-1.0d+0) 
term(309) = term(309) * (2.0d+0) 
term(310) = term(310) * (2.0d+0) 
term(311) = term(311) * (-4.0d+0) 
term(312) = term(312) * (2.0d+0) 
term(313) = term(313) * (-4.0d+0) 
term(314) = term(314) * (2.0d+0) 
term(315) = term(315) * (2.0d+0) 
term(316) = term(316) * (2.0d+0) 
term(317) = term(317) * (-4.0d+0) 
term(318) = term(318) * (-1.0d+0) 
term(319) = term(319) * (2.0d+0) 
term(320) = term(320) * (-1.0d+0) 
term(321) = term(321) * (-1.0d+0) 
term(322) = term(322) * (-1.0d+0) 
term(323) = term(323) * (2.0d+0) 
term(324) = term(324) * (-4.0d+0) 
term(325) = term(325) * (4.0d+0) 
term(326) = term(326) * (8.0d+0) 
term(327) = term(327) * (-8.0d+0) 
term(328) = term(328) * (-4.0d+0) 
term(329) = term(329) * (8.0d+0) 
term(330) = term(330) * (4.0d+0) 
term(331) = term(331) * (-8.0d+0) 
term(332) = term(332) * (8.0d+0) 
term(333) = term(333) * (-8.0d+0) 
term(334) = term(334) * (8.0d+0) 
term(335) = term(335) * (-8.0d+0) 
term(336) = term(336) * (-4.0d+0) 
term(337) = term(337) * (4.0d+0) 
term(338) = term(338) * (-4.0d+0) 
term(339) = term(339) * (4.0d+0) 
term(340) = term(340) * (-1.0d+0) 
term(341) = term(341) * (-1.0d+0) 
term(342) = term(342) * (2.0d+0) 
term(343) = term(343) * (-4.0d+0) 
term(344) = term(344) * (4.0d+0) 
term(345) = term(345) * (-4.0d+0) 
term(346) = term(346) * (8.0d+0) 
term(347) = term(347) * (-4.0d+0) 
term(348) = term(348) * (2.0d+0) 
term(349) = term(349) * (-4.0d+0) 
term(350) = term(350) * (2.0d+0) 
term(351) = term(351) * (-4.0d+0) 
term(352) = term(352) * (8.0d+0) 
term(353) = term(353) * (2.0d+0) 
term(354) = term(354) * (-4.0d+0) 
term(355) = term(355) * (-8.0d+0) 
term(356) = term(356) * (16.0d+0) 
term(357) = term(357) * (-8.0d+0) 
term(358) = term(358) * (4.0d+0) 
term(359) = term(359) * (-8.0d+0) 
term(360) = term(360) * (4.0d+0) 
term(361) = term(361) * (-8.0d+0) 
term(362) = term(362) * (16.0d+0) 
term(363) = term(363) * (4.0d+0) 
term(364) = term(364) * (-8.0d+0) 
term(365) = term(365) * (-2.0d+0) 
term(366) = term(366) * (4.0d+0) 
term(367) = term(367) * (-2.0d+0) 
term(368) = term(368) * (4.0d+0) 
term(369) = term(369) * (-8.0d+0) 
term(370) = term(370) * (4.0d+0) 
term(371) = term(371) * (-2.0d+0) 
term(372) = term(372) * (-2.0d+0) 
term(373) = term(373) * (4.0d+0) 
term(374) = term(374) * (-2.0d+0) 
term(375) = term(375) * (4.0d+0) 
term(376) = term(376) * (-2.0d+0) 
term(377) = term(377) * (-2.0d+0) 
term(378) = term(378) * (4.0d+0) 
term(379) = term(379) * (-2.0d+0) 
term(380) = term(380) * (4.0d+0) 
term(381) = term(381) * (4.0d+0) 
term(382) = term(382) * (-8.0d+0) 
term(383) = term(383) * (-8.0d+0) 
term(384) = term(384) * (8.0d+0) 
term(385) = term(385) * (16.0d+0) 
term(386) = term(386) * (-16.0d+0) 
term(387) = term(387) * (-8.0d+0) 
term(388) = term(388) * (8.0d+0) 
term(389) = term(389) * (-8.0d+0) 
term(390) = term(390) * (8.0d+0) 
term(391) = term(391) * (-8.0d+0) 
term(392) = term(392) * (16.0d+0) 
term(393) = term(393) * (8.0d+0) 
term(394) = term(394) * (-16.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(395) = term(395) + t2(a,p,q,i) * wm_interm_50_so_pt4(b,j) * wm_interm_84_so_pt4(b,a,j,i)
term(396) = term(396) + t2(a,p,q,i) * wm_interm_50_so_pt4(b,j) * wm_interm_73_so_pt4(b,a,j,i)
term(397) = term(397) + t2(a,p,q,i) * wm_interm_50_so_pt4(b,j) * wm_interm_70_so_pt4(b,a,j,i)
term(398) = term(398) + t2(a,p,q,i) * wm_interm_51_so_pt4(b,j) * wm_interm_84_so_pt4(b,a,j,i)
term(399) = term(399) + t2(a,p,q,i) * wm_interm_51_so_pt4(b,j) * wm_interm_73_so_pt4(b,a,j,i)
term(400) = term(400) + t2(a,p,q,i) * wm_interm_51_so_pt4(b,j) * wm_interm_70_so_pt4(b,a,j,i)
term(401) = term(401) + t2(a,p,q,i) * wm_interm_50_so_pt4(b,j) * wm_interm_98_so_pt4(b,a,j,i)
term(402) = term(402) + t2(a,p,q,i) * wm_interm_50_so_pt4(b,j) * wm_interm_97_so_pt4(b,a,j,i)
term(403) = term(403) + t2(a,p,q,i) * wm_interm_51_so_pt4(b,j) * wm_interm_98_so_pt4(b,a,j,i)
term(404) = term(404) + t2(a,p,q,i) * wm_interm_51_so_pt4(b,j) * wm_interm_97_so_pt4(b,a,j,i)
term(405) = term(405) + t2(a,p,j,i) * wm_interm_47_so_pt4(b,i,j,q) * wm_interm_83_so_pt4(b,a)
term(406) = term(406) + t2(a,p,j,i) * wm_interm_47_so_pt4(b,i,j,q) * wm_interm_81_so_pt4(b,a)
term(407) = term(407) + t2(a,p,j,i) * wm_interm_47_so_pt4(b,i,j,q) * wm_interm_82_so_pt4(b,a)
term(408) = term(408) + t2(a,p,j,i) * wm_interm_104_so_pt4(b,a) * wm_interm_47_so_pt4(b,i,j,q)
term(409) = term(409) + t2(a,p,j,i) * wm_interm_105_so_pt4(b,a) * wm_interm_47_so_pt4(b,i,j,q)
end do 
end do 
end do 
end do 

term(395) = term(395) * (2.0d+0) 
term(396) = term(396) * (-4.0d+0) 
term(397) = term(397) * (2.0d+0) 
term(398) = term(398) * (-1.0d+0) 
term(399) = term(399) * (2.0d+0) 
term(400) = term(400) * (-1.0d+0) 
term(401) = term(401) * (8.0d+0) 
term(402) = term(402) * (-8.0d+0) 
term(403) = term(403) * (-4.0d+0) 
term(404) = term(404) * (4.0d+0) 
term(405) = term(405) * (-1.0d+0) 
term(406) = term(406) * (-1.0d+0) 
term(407) = term(407) * (2.0d+0) 
term(408) = term(408) * (-4.0d+0) 
term(409) = term(409) * (4.0d+0) 

do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(410) = term(410) + t1(b,q) * wm_interm_15_so_pt4(a,p) * wm_interm_19_so_pt4(b,a)
term(411) = term(411) + t1(b,q) * wm_interm_15_so_pt4(a,p) * wm_interm_20_so_pt4(b,a)
term(412) = term(412) + r1(vrdav_Rr, b,q) * wm_interm_19_so_pt4(b,a) * wm_interm_23_so_pt4(a,p)
term(413) = term(413) + r1(vrdav_Rr, b,q) * wm_interm_20_so_pt4(b,a) * wm_interm_23_so_pt4(a,p)
term(414) = term(414) + t1(b,q) * wm_interm_19_so_pt4(b,a) * wm_interm_91_so_pt4(a,p)
term(415) = term(415) + t1(b,q) * wm_interm_20_so_pt4(b,a) * wm_interm_91_so_pt4(a,p)
term(416) = term(416) + t1(b,q) * wm_interm_19_so_pt4(b,a) * wm_interm_92_so_pt4(a,p)
term(417) = term(417) + t1(b,q) * wm_interm_20_so_pt4(b,a) * wm_interm_92_so_pt4(a,p)
term(418) = term(418) + t1(b,q) * wm_interm_19_so_pt4(b,a) * wm_interm_90_so_pt4(a,p)
term(419) = term(419) + t1(b,q) * wm_interm_20_so_pt4(b,a) * wm_interm_90_so_pt4(a,p)
term(420) = term(420) + t1(b,q) * wm_interm_57_so_pt4(b,a) * wm_interm_81_so_pt4(p,a)
term(421) = term(421) + t1(b,q) * wm_interm_57_so_pt4(b,a) * wm_interm_82_so_pt4(p,a)
term(422) = term(422) + t1(b,q) * wm_interm_58_so_pt4(b,a) * wm_interm_81_so_pt4(p,a)
term(423) = term(423) + t1(b,q) * wm_interm_58_so_pt4(b,a) * wm_interm_82_so_pt4(p,a)
term(424) = term(424) + t1(b,q) * wm_interm_57_so_pt4(b,a) * wm_interm_83_so_pt4(p,a)
term(425) = term(425) + t1(b,q) * wm_interm_58_so_pt4(b,a) * wm_interm_83_so_pt4(p,a)
term(426) = term(426) + t1(b,q) * wm_interm_110_so_pt4(a,p) * wm_interm_19_so_pt4(b,a)
term(427) = term(427) + t1(b,q) * wm_interm_110_so_pt4(a,p) * wm_interm_20_so_pt4(b,a)
term(428) = term(428) + t1(b,q) * wm_interm_111_so_pt4(a,p) * wm_interm_19_so_pt4(b,a)
term(429) = term(429) + t1(b,q) * wm_interm_111_so_pt4(a,p) * wm_interm_20_so_pt4(b,a)
term(430) = term(430) + t1(b,q) * wm_interm_104_so_pt4(p,a) * wm_interm_57_so_pt4(b,a)
term(431) = term(431) + t1(b,q) * wm_interm_105_so_pt4(p,a) * wm_interm_57_so_pt4(b,a)
term(432) = term(432) + t1(b,q) * wm_interm_104_so_pt4(p,a) * wm_interm_58_so_pt4(b,a)
term(433) = term(433) + t1(b,q) * wm_interm_105_so_pt4(p,a) * wm_interm_58_so_pt4(b,a)
term(434) = term(434) + t1(b,q) * wm_interm_112_so_pt4(p,a) * wm_interm_57_so_pt4(b,a)
term(435) = term(435) + t1(b,q) * wm_interm_112_so_pt4(p,a) * wm_interm_58_so_pt4(b,a)
term(436) = term(436) + t1(b,q) * wm_interm_19_so_pt4(a,p) * wm_interm_91_so_pt4(b,a)
term(437) = term(437) + t1(b,q) * wm_interm_19_so_pt4(a,p) * wm_interm_92_so_pt4(b,a)
term(438) = term(438) + t1(b,q) * wm_interm_19_so_pt4(a,p) * wm_interm_90_so_pt4(b,a)
term(439) = term(439) + t1(b,q) * wm_interm_20_so_pt4(a,p) * wm_interm_91_so_pt4(b,a)
term(440) = term(440) + t1(b,q) * wm_interm_20_so_pt4(a,p) * wm_interm_92_so_pt4(b,a)
term(441) = term(441) + t1(b,q) * wm_interm_20_so_pt4(a,p) * wm_interm_90_so_pt4(b,a)
term(442) = term(442) + t1(b,q) * wm_interm_110_so_pt4(b,a) * wm_interm_19_so_pt4(a,p)
term(443) = term(443) + t1(b,q) * wm_interm_111_so_pt4(b,a) * wm_interm_19_so_pt4(a,p)
term(444) = term(444) + t1(b,q) * wm_interm_110_so_pt4(b,a) * wm_interm_20_so_pt4(a,p)
term(445) = term(445) + t1(b,q) * wm_interm_111_so_pt4(b,a) * wm_interm_20_so_pt4(a,p)
end do 
end do 

term(410) = term(410) * (2.0d+0) 
term(411) = term(411) * (-4.0d+0) 
term(412) = term(412) * (2.0d+0) 
term(413) = term(413) * (-4.0d+0) 
term(414) = term(414) * (-1.0d+0) 
term(415) = term(415) * (2.0d+0) 
term(416) = term(416) * (2.0d+0) 
term(417) = term(417) * (-4.0d+0) 
term(418) = term(418) * (-1.0d+0) 
term(419) = term(419) * (2.0d+0) 
term(420) = term(420) * (2.0d+0) 
term(421) = term(421) * (-4.0d+0) 
term(422) = term(422) * (-1.0d+0) 
term(423) = term(423) * (2.0d+0) 
term(424) = term(424) * (2.0d+0) 
term(425) = term(425) * (-1.0d+0) 
term(426) = term(426) * (-4.0d+0) 
term(427) = term(427) * (8.0d+0) 
term(428) = term(428) * (4.0d+0) 
term(429) = term(429) * (-8.0d+0) 
term(430) = term(430) * (4.0d+0) 
term(431) = term(431) * (-8.0d+0) 
term(432) = term(432) * (-2.0d+0) 
term(433) = term(433) * (4.0d+0) 
term(434) = term(434) * (4.0d+0) 
term(435) = term(435) * (-2.0d+0) 
term(436) = term(436) * (-2.0d+0) 
term(437) = term(437) * (4.0d+0) 
term(438) = term(438) * (-2.0d+0) 
term(439) = term(439) * (4.0d+0) 
term(440) = term(440) * (-8.0d+0) 
term(441) = term(441) * (4.0d+0) 
term(442) = term(442) * (-8.0d+0) 
term(443) = term(443) * (8.0d+0) 
term(444) = term(444) * (16.0d+0) 
term(445) = term(445) * (-16.0d+0) 

do l = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
do k = 1, nocc 
do i = 1, nocc 
term(446) = term(446) + r2p(vrdav_Rr, p,i,a,q) * wm_interm_22_so_pt4(a,j,k,l) * wm_interm_61_so_pt4(i,l,k,j)
term(447) = term(447) + r2p(vrdav_Rr, p,i,a,q) * wm_interm_22_so_pt4(a,j,k,l) * wm_interm_61_so_pt4(l,i,k,j)
term(448) = term(448) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_49_so_pt4(a,k,l,q) * wm_interm_9_so_pt4(j,i,k,l)
term(449) = term(449) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_49_so_pt4(a,k,l,q) * wm_interm_9_so_pt4(i,j,k,l)
term(450) = term(450) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_47_so_pt4(a,k,l,q) * wm_interm_9_so_pt4(i,j,k,l)
term(451) = term(451) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_47_so_pt4(a,k,l,q) * wm_interm_9_so_pt4(j,i,k,l)
term(452) = term(452) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_22_so_pt4(a,k,l,q) * wm_interm_61_so_pt4(i,j,k,l)
term(453) = term(453) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_22_so_pt4(a,k,l,q) * wm_interm_61_so_pt4(j,i,k,l)
end do 
end do 
end do 
end do 
end do 

term(446) = term(446) * (-1.0d+0) 
term(447) = term(447) * (2.0d+0) 
term(448) = term(448) * (-0.5d+0) 
term(450) = term(450) * (-0.5d+0) 
term(452) = term(452) * (-0.5d+0) 

do l = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
do k = 1, nocc 
term(454) = term(454) + r2p(vrdav_Rr, p,q,a,i) * wm_interm_22_so_pt4(a,j,k,l) * wm_interm_61_so_pt4(i,l,k,j)
term(455) = term(455) + r2p(vrdav_Rr, p,q,a,i) * wm_interm_22_so_pt4(a,j,k,l) * wm_interm_61_so_pt4(l,i,k,j)
end do 
end do 
end do 
end do 
end do 

term(454) = term(454) * (2.0d+0) 
term(455) = term(455) * (-4.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
do j = 1, nocc 
do b = nocc + 1, nactive 
term(456) = term(456) + t2(a,p,j,i) * wm_interm_47_so_pt4(b,j,i,k) * wm_interm_67_so_pt4(b,a,q,k)
term(457) = term(457) + t2(a,p,j,i) * wm_interm_47_so_pt4(b,j,i,k) * wm_interm_68_so_pt4(b,a,q,k)
term(458) = term(458) + t2(a,p,j,i) * wm_interm_47_so_pt4(b,j,i,k) * wm_interm_69_so_pt4(b,a,q,k)
term(459) = term(459) + t2(a,p,j,i) * wm_interm_47_so_pt4(b,j,i,k) * wm_interm_73_so_pt4(b,a,q,k)
term(460) = term(460) + t2(a,p,j,i) * wm_interm_47_so_pt4(b,j,i,k) * wm_interm_84_so_pt4(b,a,q,k)
term(461) = term(461) + t2(a,p,j,i) * wm_interm_47_so_pt4(b,j,i,k) * wm_interm_74_so_pt4(b,a,q,k)
term(462) = term(462) + t2(a,p,j,i) * wm_interm_49_so_pt4(b,j,i,k) * wm_interm_67_so_pt4(b,a,q,k)
term(463) = term(463) + t2(a,p,j,i) * wm_interm_49_so_pt4(b,j,i,k) * wm_interm_68_so_pt4(b,a,q,k)
term(464) = term(464) + t2(a,p,j,i) * wm_interm_49_so_pt4(b,j,i,k) * wm_interm_74_so_pt4(b,a,q,k)
term(465) = term(465) + t2(a,p,j,i) * wm_interm_49_so_pt4(b,j,i,k) * wm_interm_84_so_pt4(b,a,q,k)
term(466) = term(466) + t2(a,p,j,i) * wm_interm_49_so_pt4(b,j,i,k) * wm_interm_70_so_pt4(b,a,q,k)
term(467) = term(467) + t2(a,p,j,i) * wm_interm_49_so_pt4(b,j,i,k) * wm_interm_69_so_pt4(b,a,q,k)
term(468) = term(468) + t2(a,p,j,i) * wm_interm_47_so_pt4(b,j,i,k) * wm_interm_95_so_pt4(b,a,q,k)
term(469) = term(469) + t2(a,p,j,i) * wm_interm_47_so_pt4(b,j,i,k) * wm_interm_96_so_pt4(b,a,q,k)
term(470) = term(470) + t2(a,p,j,i) * wm_interm_47_so_pt4(b,j,i,k) * wm_interm_97_so_pt4(b,a,q,k)
term(471) = term(471) + t2(a,p,j,i) * wm_interm_47_so_pt4(b,j,i,k) * wm_interm_98_so_pt4(b,a,q,k)
term(472) = term(472) + t2(a,p,j,i) * wm_interm_49_so_pt4(b,j,i,k) * wm_interm_95_so_pt4(b,a,q,k)
term(473) = term(473) + t2(a,p,j,i) * wm_interm_49_so_pt4(b,j,i,k) * wm_interm_96_so_pt4(b,a,q,k)
term(474) = term(474) + t2(a,p,j,i) * wm_interm_49_so_pt4(b,j,i,k) * wm_interm_98_so_pt4(b,a,q,k)
term(475) = term(475) + t2(a,p,j,i) * wm_interm_49_so_pt4(b,j,i,k) * wm_interm_97_so_pt4(b,a,q,k)
end do 
end do 
end do 
end do 
end do 

term(456) = term(456) * (-0.5d+0) 
term(458) = term(458) * (-0.5d+0) 
term(459) = term(459) * (-0.5d+0) 
term(461) = term(461) * (-0.5d+0) 
term(463) = term(463) * (-2.0d+0) 
term(465) = term(465) * (-0.5d+0) 
term(466) = term(466) * (-0.5d+0) 
term(468) = term(468) * (-2.0d+0) 
term(469) = term(469) * (2.0d+0) 
term(470) = term(470) * (-2.0d+0) 
term(471) = term(471) * (2.0d+0) 
term(472) = term(472) * (4.0d+0) 
term(473) = term(473) * (-4.0d+0) 
term(474) = term(474) * (-2.0d+0) 
term(475) = term(475) * (2.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
do j = 1, nocc 
term(476) = term(476) + r2p(vrdav_Rr, p,q,a,i) * wm_interm_22_so_pt4(a,j,k,l) * wm_interm_61_so_pt4(i,l,j,k)
term(477) = term(477) + r2p(vrdav_Rr, p,q,a,i) * wm_interm_47_so_pt4(a,j,k,l) * wm_interm_9_so_pt4(l,i,j,k)
term(478) = term(478) + r2p(vrdav_Rr, p,q,a,i) * wm_interm_47_so_pt4(a,j,k,l) * wm_interm_9_so_pt4(i,l,j,k)
term(479) = term(479) + r2p(vrdav_Rr, p,q,a,i) * wm_interm_22_so_pt4(a,j,k,l) * wm_interm_61_so_pt4(l,i,j,k)
term(480) = term(480) + r2p(vrdav_Rr, p,q,a,i) * wm_interm_49_so_pt4(a,j,k,l) * wm_interm_9_so_pt4(i,l,j,k)
term(481) = term(481) + r2p(vrdav_Rr, p,q,a,i) * wm_interm_49_so_pt4(a,j,k,l) * wm_interm_9_so_pt4(l,i,j,k)
end do 
end do 
end do 
end do 
end do 

term(476) = term(476) * (-4.0d+0) 
term(477) = term(477) * (2.0d+0) 
term(478) = term(478) * (-4.0d+0) 
term(479) = term(479) * (2.0d+0) 
term(480) = term(480) * (2.0d+0) 
term(481) = term(481) * (-4.0d+0) 

do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(482) = term(482) + r2m(vrdav_Rr, a,i,p,q) * wm_interm_19_so_pt4(a,b) * wm_interm_50_so_pt4(b,i)
term(483) = term(483) + r2m(vrdav_Rr, a,i,p,q) * wm_interm_20_so_pt4(a,b) * wm_interm_50_so_pt4(b,i)
term(484) = term(484) + r2m(vrdav_Rr, a,i,p,q) * wm_interm_19_so_pt4(a,b) * wm_interm_51_so_pt4(b,i)
term(485) = term(485) + r2m(vrdav_Rr, a,i,p,q) * wm_interm_20_so_pt4(a,b) * wm_interm_51_so_pt4(b,i)
term(486) = term(486) + r2p(vrdav_Rr, a,i,b,q) * wm_interm_19_so_pt4(a,p) * wm_interm_50_so_pt4(b,i)
term(487) = term(487) + r2p(vrdav_Rr, a,i,b,q) * wm_interm_20_so_pt4(a,p) * wm_interm_50_so_pt4(b,i)
term(488) = term(488) + r2p(vrdav_Rr, a,i,b,q) * wm_interm_19_so_pt4(a,p) * wm_interm_51_so_pt4(b,i)
term(489) = term(489) + r2p(vrdav_Rr, a,i,b,q) * wm_interm_20_so_pt4(a,p) * wm_interm_51_so_pt4(b,i)
term(490) = term(490) + r2m(vrdav_Rr, a,i,b,q) * wm_interm_19_so_pt4(a,p) * wm_interm_50_so_pt4(b,i)
term(491) = term(491) + r2m(vrdav_Rr, a,i,b,q) * wm_interm_20_so_pt4(a,p) * wm_interm_50_so_pt4(b,i)
term(492) = term(492) + r2m(vrdav_Rr, a,i,b,q) * wm_interm_19_so_pt4(a,p) * wm_interm_51_so_pt4(b,i)
term(493) = term(493) + r2m(vrdav_Rr, a,i,b,q) * wm_interm_20_so_pt4(a,p) * wm_interm_51_so_pt4(b,i)
end do 
end do 
end do 

term(482) = term(482) * (-16.0d+0) 
term(483) = term(483) * (32.0d+0) 
term(484) = term(484) * (8.0d+0) 
term(485) = term(485) * (-16.0d+0) 
term(486) = term(486) * (2.0d+0) 
term(487) = term(487) * (-4.0d+0) 
term(488) = term(488) * (-1.0d+0) 
term(489) = term(489) * (2.0d+0) 
term(490) = term(490) * (8.0d+0) 
term(491) = term(491) * (-16.0d+0) 
term(492) = term(492) * (-4.0d+0) 
term(493) = term(493) * (8.0d+0) 

do l = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(494) = term(494) + r2p(vrdav_Rr, a,q,p,i) * wm_interm_22_so_pt4(a,j,k,l) * wm_interm_61_so_pt4(i,l,k,j)
term(495) = term(495) + r2p(vrdav_Rr, a,q,p,i) * wm_interm_22_so_pt4(a,j,k,l) * wm_interm_61_so_pt4(l,i,k,j)
term(496) = term(496) + r2m(vrdav_Rr, a,q,p,i) * wm_interm_22_so_pt4(a,j,k,l) * wm_interm_61_so_pt4(i,l,k,j)
term(497) = term(497) + r2m(vrdav_Rr, a,q,p,i) * wm_interm_22_so_pt4(a,j,k,l) * wm_interm_61_so_pt4(l,i,k,j)
term(498) = term(498) + t2(a,p,q,i) * wm_interm_113_so_pt4(a,j,k,l) * wm_interm_71_so_pt4(i,l,k,j)
term(499) = term(499) + t2(a,p,q,i) * wm_interm_113_so_pt4(a,j,k,l) * wm_interm_72_so_pt4(i,l,k,j)
term(500) = term(500) + t2(a,p,q,i) * wm_interm_113_so_pt4(a,j,k,l) * wm_interm_71_so_pt4(l,i,k,j)
term(501) = term(501) + t2(a,p,q,i) * wm_interm_113_so_pt4(a,j,k,l) * wm_interm_72_so_pt4(l,i,k,j)
term(502) = term(502) + t2(a,p,q,i) * wm_interm_113_so_pt4(a,j,k,l) * wm_interm_99_so_pt4(i,l,k,j)
term(503) = term(503) + t2(a,p,q,i) * wm_interm_113_so_pt4(a,j,k,l) * wm_interm_99_so_pt4(l,i,k,j)
end do 
end do 
end do 
end do 
end do 

term(494) = term(494) * (-1.0d+0) 
term(495) = term(495) * (2.0d+0) 
term(496) = term(496) * (-4.0d+0) 
term(497) = term(497) * (8.0d+0) 
term(499) = term(499) * (-0.5d+0) 
term(500) = term(500) * (-0.5d+0) 
term(502) = term(502) * (2.0d+0) 
term(503) = term(503) * (-2.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(504) = term(504) + r2p(vrdav_Rr, a,q,p,i) * wm_interm_22_so_pt4(a,j,k,l) * wm_interm_61_so_pt4(i,l,j,k)
term(505) = term(505) + r2p(vrdav_Rr, a,q,p,i) * wm_interm_47_so_pt4(a,j,k,l) * wm_interm_9_so_pt4(l,i,j,k)
term(506) = term(506) + r2p(vrdav_Rr, a,q,p,i) * wm_interm_47_so_pt4(a,j,k,l) * wm_interm_9_so_pt4(i,l,j,k)
term(507) = term(507) + r2p(vrdav_Rr, a,q,p,i) * wm_interm_22_so_pt4(a,j,k,l) * wm_interm_61_so_pt4(l,i,j,k)
term(508) = term(508) + r2p(vrdav_Rr, a,q,p,i) * wm_interm_49_so_pt4(a,j,k,l) * wm_interm_9_so_pt4(i,l,j,k)
term(509) = term(509) + r2p(vrdav_Rr, a,q,p,i) * wm_interm_49_so_pt4(a,j,k,l) * wm_interm_9_so_pt4(l,i,j,k)
term(510) = term(510) + r2m(vrdav_Rr, a,q,p,i) * wm_interm_22_so_pt4(a,j,k,l) * wm_interm_61_so_pt4(i,l,j,k)
term(511) = term(511) + r2m(vrdav_Rr, a,q,p,i) * wm_interm_47_so_pt4(a,j,k,l) * wm_interm_9_so_pt4(l,i,j,k)
term(512) = term(512) + r2m(vrdav_Rr, a,q,p,i) * wm_interm_47_so_pt4(a,j,k,l) * wm_interm_9_so_pt4(i,l,j,k)
term(513) = term(513) + r2m(vrdav_Rr, a,q,p,i) * wm_interm_22_so_pt4(a,j,k,l) * wm_interm_61_so_pt4(l,i,j,k)
term(514) = term(514) + r2m(vrdav_Rr, a,q,p,i) * wm_interm_49_so_pt4(a,j,k,l) * wm_interm_9_so_pt4(i,l,j,k)
term(515) = term(515) + r2m(vrdav_Rr, a,q,p,i) * wm_interm_49_so_pt4(a,j,k,l) * wm_interm_9_so_pt4(l,i,j,k)
term(516) = term(516) + t2(a,p,q,i) * wm_interm_113_so_pt4(a,j,k,l) * wm_interm_71_so_pt4(i,l,j,k)
term(517) = term(517) + t2(a,p,q,i) * wm_interm_113_so_pt4(a,j,k,l) * wm_interm_72_so_pt4(l,i,j,k)
term(518) = term(518) + t2(a,p,q,i) * wm_interm_113_so_pt4(a,j,k,l) * wm_interm_99_so_pt4(i,l,j,k)
term(519) = term(519) + t2(a,p,q,i) * wm_interm_113_so_pt4(a,j,k,l) * wm_interm_99_so_pt4(l,i,j,k)
end do 
end do 
end do 
end do 
end do 

term(504) = term(504) * (2.0d+0) 
term(505) = term(505) * (-1.0d+0) 
term(506) = term(506) * (2.0d+0) 
term(507) = term(507) * (-1.0d+0) 
term(508) = term(508) * (-1.0d+0) 
term(509) = term(509) * (2.0d+0) 
term(510) = term(510) * (8.0d+0) 
term(511) = term(511) * (-4.0d+0) 
term(512) = term(512) * (8.0d+0) 
term(513) = term(513) * (-4.0d+0) 
term(514) = term(514) * (-4.0d+0) 
term(515) = term(515) * (8.0d+0) 
term(516) = term(516) * (-0.5d+0) 
term(517) = term(517) * (-0.5d+0) 
term(518) = term(518) * (-2.0d+0) 
term(519) = term(519) * (2.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(520) = term(520) + t1(a,i) * wm_interm_59_so_pt4(b,p,i,j) * wm_interm_67_so_pt4(b,a,q,j)
term(521) = term(521) + t1(a,i) * wm_interm_59_so_pt4(b,p,i,j) * wm_interm_68_so_pt4(b,a,q,j)
term(522) = term(522) + t1(a,i) * wm_interm_59_so_pt4(b,p,i,j) * wm_interm_74_so_pt4(b,a,q,j)
term(523) = term(523) + t1(a,i) * wm_interm_54_so_pt4(b,p,i,j) * wm_interm_67_so_pt4(b,a,q,j)
term(524) = term(524) + t1(a,i) * wm_interm_54_so_pt4(b,p,i,j) * wm_interm_68_so_pt4(b,a,q,j)
term(525) = term(525) + t1(a,i) * wm_interm_54_so_pt4(b,p,i,j) * wm_interm_74_so_pt4(b,a,q,j)
term(526) = term(526) + t1(a,i) * wm_interm_59_so_pt4(b,p,i,j) * wm_interm_69_so_pt4(b,a,q,j)
term(527) = term(527) + t1(a,i) * wm_interm_54_so_pt4(b,p,i,j) * wm_interm_69_so_pt4(b,a,q,j)
term(528) = term(528) + t1(a,i) * wm_interm_59_so_pt4(b,p,i,j) * wm_interm_73_so_pt4(b,a,q,j)
term(529) = term(529) + t1(a,i) * wm_interm_54_so_pt4(b,p,i,j) * wm_interm_73_so_pt4(b,a,q,j)
term(530) = term(530) + t1(a,i) * wm_interm_59_so_pt4(b,p,i,j) * wm_interm_84_so_pt4(b,a,q,j)
term(531) = term(531) + t1(a,i) * wm_interm_54_so_pt4(b,p,i,j) * wm_interm_84_so_pt4(b,a,q,j)
term(532) = term(532) + t1(a,i) * wm_interm_55_so_pt4(b,p,i,j) * wm_interm_67_so_pt4(b,a,q,j)
term(533) = term(533) + t1(a,i) * wm_interm_55_so_pt4(b,p,i,j) * wm_interm_68_so_pt4(b,a,q,j)
term(534) = term(534) + t1(a,i) * wm_interm_55_so_pt4(b,p,i,j) * wm_interm_69_so_pt4(b,a,q,j)
term(535) = term(535) + t1(a,i) * wm_interm_55_so_pt4(b,p,i,j) * wm_interm_74_so_pt4(b,a,q,j)
term(536) = term(536) + t1(a,i) * wm_interm_55_so_pt4(b,p,i,j) * wm_interm_73_so_pt4(b,a,q,j)
term(537) = term(537) + t1(a,i) * wm_interm_55_so_pt4(b,p,i,j) * wm_interm_84_so_pt4(b,a,q,j)
term(538) = term(538) + t1(a,i) * wm_interm_56_so_pt4(b,p,i,j) * wm_interm_70_so_pt4(b,a,q,j)
term(539) = term(539) + t1(a,i) * wm_interm_56_so_pt4(b,p,i,j) * wm_interm_69_so_pt4(b,a,q,j)
term(540) = term(540) + t1(a,i) * wm_interm_56_so_pt4(b,p,i,j) * wm_interm_67_so_pt4(b,a,q,j)
term(541) = term(541) + t1(a,i) * wm_interm_56_so_pt4(b,p,i,j) * wm_interm_68_so_pt4(b,a,q,j)
term(542) = term(542) + t1(a,i) * wm_interm_56_so_pt4(b,p,i,j) * wm_interm_84_so_pt4(b,a,q,j)
term(543) = term(543) + t1(a,i) * wm_interm_56_so_pt4(b,p,i,j) * wm_interm_74_so_pt4(b,a,q,j)
term(544) = term(544) + t1(a,i) * wm_interm_59_so_pt4(b,p,i,j) * wm_interm_95_so_pt4(b,a,q,j)
term(545) = term(545) + t1(a,i) * wm_interm_59_so_pt4(b,p,i,j) * wm_interm_96_so_pt4(b,a,q,j)
term(546) = term(546) + t1(a,i) * wm_interm_54_so_pt4(b,p,i,j) * wm_interm_95_so_pt4(b,a,q,j)
term(547) = term(547) + t1(a,i) * wm_interm_54_so_pt4(b,p,i,j) * wm_interm_96_so_pt4(b,a,q,j)
term(548) = term(548) + t1(a,i) * wm_interm_59_so_pt4(b,p,i,j) * wm_interm_97_so_pt4(b,a,q,j)
term(549) = term(549) + t1(a,i) * wm_interm_54_so_pt4(b,p,i,j) * wm_interm_97_so_pt4(b,a,q,j)
term(550) = term(550) + t1(a,i) * wm_interm_59_so_pt4(b,p,i,j) * wm_interm_98_so_pt4(b,a,q,j)
term(551) = term(551) + t1(a,i) * wm_interm_54_so_pt4(b,p,i,j) * wm_interm_98_so_pt4(b,a,q,j)
term(552) = term(552) + t1(a,i) * wm_interm_55_so_pt4(b,p,i,j) * wm_interm_95_so_pt4(b,a,q,j)
term(553) = term(553) + t1(a,i) * wm_interm_55_so_pt4(b,p,i,j) * wm_interm_96_so_pt4(b,a,q,j)
term(554) = term(554) + t1(a,i) * wm_interm_55_so_pt4(b,p,i,j) * wm_interm_97_so_pt4(b,a,q,j)
term(555) = term(555) + t1(a,i) * wm_interm_55_so_pt4(b,p,i,j) * wm_interm_98_so_pt4(b,a,q,j)
term(556) = term(556) + t1(a,i) * wm_interm_56_so_pt4(b,p,i,j) * wm_interm_98_so_pt4(b,a,q,j)
term(557) = term(557) + t1(a,i) * wm_interm_56_so_pt4(b,p,i,j) * wm_interm_97_so_pt4(b,a,q,j)
term(558) = term(558) + t1(a,i) * wm_interm_56_so_pt4(b,p,i,j) * wm_interm_95_so_pt4(b,a,q,j)
term(559) = term(559) + t1(a,i) * wm_interm_56_so_pt4(b,p,i,j) * wm_interm_96_so_pt4(b,a,q,j)
term(560) = term(560) + t2(a,p,j,i) * wm_interm_50_so_pt4(b,i) * wm_interm_84_so_pt4(b,a,q,j)
term(561) = term(561) + t2(a,p,j,i) * wm_interm_50_so_pt4(b,i) * wm_interm_70_so_pt4(b,a,q,j)
term(562) = term(562) + t2(a,p,j,i) * wm_interm_50_so_pt4(b,i) * wm_interm_69_so_pt4(b,a,q,j)
term(563) = term(563) + t2(a,p,j,i) * wm_interm_50_so_pt4(b,i) * wm_interm_74_so_pt4(b,a,q,j)
term(564) = term(564) + t2(a,p,j,i) * wm_interm_50_so_pt4(b,i) * wm_interm_67_so_pt4(b,a,q,j)
term(565) = term(565) + t2(a,p,j,i) * wm_interm_50_so_pt4(b,i) * wm_interm_68_so_pt4(b,a,q,j)
term(566) = term(566) + t2(a,p,j,i) * wm_interm_51_so_pt4(b,i) * wm_interm_84_so_pt4(b,a,q,j)
term(567) = term(567) + t2(a,p,j,i) * wm_interm_51_so_pt4(b,i) * wm_interm_70_so_pt4(b,a,q,j)
term(568) = term(568) + t2(a,p,j,i) * wm_interm_51_so_pt4(b,i) * wm_interm_69_so_pt4(b,a,q,j)
term(569) = term(569) + t2(a,p,j,i) * wm_interm_51_so_pt4(b,i) * wm_interm_74_so_pt4(b,a,q,j)
term(570) = term(570) + t2(a,p,j,i) * wm_interm_51_so_pt4(b,i) * wm_interm_67_so_pt4(b,a,q,j)
term(571) = term(571) + t2(a,p,j,i) * wm_interm_51_so_pt4(b,i) * wm_interm_68_so_pt4(b,a,q,j)
term(572) = term(572) + t2(a,p,j,i) * wm_interm_50_so_pt4(b,i) * wm_interm_98_so_pt4(b,a,q,j)
term(573) = term(573) + t2(a,p,j,i) * wm_interm_50_so_pt4(b,i) * wm_interm_97_so_pt4(b,a,q,j)
term(574) = term(574) + t2(a,p,j,i) * wm_interm_50_so_pt4(b,i) * wm_interm_95_so_pt4(b,a,q,j)
term(575) = term(575) + t2(a,p,j,i) * wm_interm_50_so_pt4(b,i) * wm_interm_96_so_pt4(b,a,q,j)
term(576) = term(576) + t2(a,p,j,i) * wm_interm_51_so_pt4(b,i) * wm_interm_98_so_pt4(b,a,q,j)
term(577) = term(577) + t2(a,p,j,i) * wm_interm_51_so_pt4(b,i) * wm_interm_97_so_pt4(b,a,q,j)
term(578) = term(578) + t2(a,p,j,i) * wm_interm_51_so_pt4(b,i) * wm_interm_95_so_pt4(b,a,q,j)
term(579) = term(579) + t2(a,p,j,i) * wm_interm_51_so_pt4(b,i) * wm_interm_96_so_pt4(b,a,q,j)
term(580) = term(580) + t1(a,q) * wm_interm_56_so_pt4(b,p,i,j) * wm_interm_74_so_pt4(b,a,i,j)
term(581) = term(581) + t1(a,q) * wm_interm_56_so_pt4(b,p,i,j) * wm_interm_67_so_pt4(b,a,i,j)
term(582) = term(582) + t1(a,q) * wm_interm_56_so_pt4(b,p,i,j) * wm_interm_68_so_pt4(b,a,i,j)
term(583) = term(583) + t1(a,q) * wm_interm_59_so_pt4(b,p,i,j) * wm_interm_84_so_pt4(b,a,i,j)
term(584) = term(584) + t1(a,q) * wm_interm_59_so_pt4(b,p,i,j) * wm_interm_73_so_pt4(b,a,i,j)
term(585) = term(585) + t1(a,q) * wm_interm_56_so_pt4(b,p,i,j) * wm_interm_73_so_pt4(b,a,i,j)
term(586) = term(586) + t1(a,q) * wm_interm_56_so_pt4(b,p,i,j) * wm_interm_69_so_pt4(b,a,i,j)
term(587) = term(587) + t1(a,q) * wm_interm_56_so_pt4(b,p,i,j) * wm_interm_70_so_pt4(b,a,i,j)
term(588) = term(588) + t1(a,q) * wm_interm_59_so_pt4(b,p,i,j) * wm_interm_70_so_pt4(b,a,i,j)
term(589) = term(589) + t1(a,q) * wm_interm_55_so_pt4(b,p,i,j) * wm_interm_84_so_pt4(b,a,i,j)
term(590) = term(590) + t1(a,q) * wm_interm_55_so_pt4(b,p,i,j) * wm_interm_73_so_pt4(b,a,i,j)
term(591) = term(591) + t1(a,q) * wm_interm_54_so_pt4(b,p,i,j) * wm_interm_84_so_pt4(b,a,i,j)
term(592) = term(592) + t1(a,q) * wm_interm_54_so_pt4(b,p,i,j) * wm_interm_73_so_pt4(b,a,i,j)
term(593) = term(593) + t1(a,q) * wm_interm_55_so_pt4(b,p,i,j) * wm_interm_70_so_pt4(b,a,i,j)
term(594) = term(594) + t1(a,q) * wm_interm_54_so_pt4(b,p,i,j) * wm_interm_70_so_pt4(b,a,i,j)
term(595) = term(595) + t1(a,q) * wm_interm_56_so_pt4(b,p,i,j) * wm_interm_95_so_pt4(b,a,i,j)
term(596) = term(596) + t1(a,q) * wm_interm_56_so_pt4(b,p,i,j) * wm_interm_96_so_pt4(b,a,i,j)
term(597) = term(597) + t1(a,q) * wm_interm_59_so_pt4(b,p,i,j) * wm_interm_98_so_pt4(b,a,i,j)
term(598) = term(598) + t1(a,q) * wm_interm_59_so_pt4(b,p,i,j) * wm_interm_97_so_pt4(b,a,i,j)
term(599) = term(599) + t1(a,q) * wm_interm_56_so_pt4(b,p,i,j) * wm_interm_97_so_pt4(b,a,i,j)
term(600) = term(600) + t1(a,q) * wm_interm_56_so_pt4(b,p,i,j) * wm_interm_98_so_pt4(b,a,i,j)
term(601) = term(601) + t1(a,q) * wm_interm_55_so_pt4(b,p,i,j) * wm_interm_98_so_pt4(b,a,i,j)
term(602) = term(602) + t1(a,q) * wm_interm_55_so_pt4(b,p,i,j) * wm_interm_97_so_pt4(b,a,i,j)
term(603) = term(603) + t1(a,q) * wm_interm_54_so_pt4(b,p,i,j) * wm_interm_98_so_pt4(b,a,i,j)
term(604) = term(604) + t1(a,q) * wm_interm_54_so_pt4(b,p,i,j) * wm_interm_97_so_pt4(b,a,i,j)
end do 
end do 
end do 
end do 

term(520) = term(520) * (-1.0d+0) 
term(521) = term(521) * (2.0d+0) 
term(522) = term(522) * (-1.0d+0) 
term(523) = term(523) * (2.0d+0) 
term(524) = term(524) * (-4.0d+0) 
term(525) = term(525) * (2.0d+0) 
term(526) = term(526) * (-1.0d+0) 
term(527) = term(527) * (2.0d+0) 
term(528) = term(528) * (-1.0d+0) 
term(529) = term(529) * (2.0d+0) 
term(530) = term(530) * (2.0d+0) 
term(531) = term(531) * (-4.0d+0) 
term(532) = term(532) * (-1.0d+0) 
term(533) = term(533) * (2.0d+0) 
term(534) = term(534) * (-1.0d+0) 
term(535) = term(535) * (-1.0d+0) 
term(536) = term(536) * (-1.0d+0) 
term(537) = term(537) * (2.0d+0) 
term(538) = term(538) * (-1.0d+0) 
term(539) = term(539) * (2.0d+0) 
term(540) = term(540) * (2.0d+0) 
term(541) = term(541) * (-4.0d+0) 
term(542) = term(542) * (-1.0d+0) 
term(543) = term(543) * (2.0d+0) 
term(544) = term(544) * (-4.0d+0) 
term(545) = term(545) * (4.0d+0) 
term(546) = term(546) * (8.0d+0) 
term(547) = term(547) * (-8.0d+0) 
term(548) = term(548) * (-4.0d+0) 
term(549) = term(549) * (8.0d+0) 
term(550) = term(550) * (4.0d+0) 
term(551) = term(551) * (-8.0d+0) 
term(552) = term(552) * (-4.0d+0) 
term(553) = term(553) * (4.0d+0) 
term(554) = term(554) * (-4.0d+0) 
term(555) = term(555) * (4.0d+0) 
term(556) = term(556) * (-4.0d+0) 
term(557) = term(557) * (4.0d+0) 
term(558) = term(558) * (8.0d+0) 
term(559) = term(559) * (-8.0d+0) 
term(560) = term(560) * (2.0d+0) 
term(561) = term(561) * (2.0d+0) 
term(562) = term(562) * (-4.0d+0) 
term(563) = term(563) * (-4.0d+0) 
term(564) = term(564) * (-4.0d+0) 
term(565) = term(565) * (8.0d+0) 
term(566) = term(566) * (-1.0d+0) 
term(567) = term(567) * (-1.0d+0) 
term(568) = term(568) * (2.0d+0) 
term(569) = term(569) * (2.0d+0) 
term(570) = term(570) * (2.0d+0) 
term(571) = term(571) * (-4.0d+0) 
term(572) = term(572) * (8.0d+0) 
term(573) = term(573) * (-8.0d+0) 
term(574) = term(574) * (-16.0d+0) 
term(575) = term(575) * (16.0d+0) 
term(576) = term(576) * (-4.0d+0) 
term(577) = term(577) * (4.0d+0) 
term(578) = term(578) * (8.0d+0) 
term(579) = term(579) * (-8.0d+0) 
term(580) = term(580) * (-2.0d+0) 
term(581) = term(581) * (-2.0d+0) 
term(582) = term(582) * (4.0d+0) 
term(583) = term(583) * (-2.0d+0) 
term(584) = term(584) * (4.0d+0) 
term(585) = term(585) * (-2.0d+0) 
term(586) = term(586) * (-2.0d+0) 
term(587) = term(587) * (4.0d+0) 
term(588) = term(588) * (-2.0d+0) 
term(589) = term(589) * (-2.0d+0) 
term(590) = term(590) * (4.0d+0) 
term(591) = term(591) * (4.0d+0) 
term(592) = term(592) * (-8.0d+0) 
term(593) = term(593) * (-2.0d+0) 
term(594) = term(594) * (4.0d+0) 
term(595) = term(595) * (-8.0d+0) 
term(596) = term(596) * (8.0d+0) 
term(597) = term(597) * (-8.0d+0) 
term(598) = term(598) * (8.0d+0) 
term(599) = term(599) * (-8.0d+0) 
term(600) = term(600) * (8.0d+0) 
term(601) = term(601) * (-8.0d+0) 
term(602) = term(602) * (8.0d+0) 
term(603) = term(603) * (16.0d+0) 
term(604) = term(604) * (-16.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
do j = 1, nocc 
term(605) = term(605) + r2p(vrdav_Rr, p,q,a,i) * wm_interm_17_so_pt4(i,j) * wm_interm_50_so_pt4(a,j)
term(606) = term(606) + r2p(vrdav_Rr, p,q,a,i) * wm_interm_18_so_pt4(i,j) * wm_interm_50_so_pt4(a,j)
term(607) = term(607) + r2p(vrdav_Rr, p,q,a,i) * wm_interm_17_so_pt4(i,j) * wm_interm_51_so_pt4(a,j)
term(608) = term(608) + r2p(vrdav_Rr, p,q,a,i) * wm_interm_18_so_pt4(i,j) * wm_interm_51_so_pt4(a,j)
term(609) = term(609) + r2p(vrdav_Rr, p,j,a,i) * wm_interm_17_so_pt4(i,q) * wm_interm_50_so_pt4(a,j)
term(610) = term(610) + r2p(vrdav_Rr, p,j,a,i) * wm_interm_18_so_pt4(i,q) * wm_interm_50_so_pt4(a,j)
term(611) = term(611) + r2p(vrdav_Rr, p,j,a,i) * wm_interm_17_so_pt4(i,q) * wm_interm_51_so_pt4(a,j)
term(612) = term(612) + r2p(vrdav_Rr, p,j,a,i) * wm_interm_18_so_pt4(i,q) * wm_interm_51_so_pt4(a,j)
end do 
end do 
end do 

term(605) = term(605) * (-8.0d+0) 
term(606) = term(606) * (16.0d+0) 
term(607) = term(607) * (4.0d+0) 
term(608) = term(608) * (-8.0d+0) 
term(609) = term(609) * (2.0d+0) 
term(610) = term(610) * (-4.0d+0) 
term(611) = term(611) * (-1.0d+0) 
term(612) = term(612) * (2.0d+0) 

do l = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(613) = term(613) + t2(a,p,q,i) * wm_interm_109_so_pt4(j,k,i,l) * wm_interm_47_so_pt4(a,k,j,l)
term(614) = term(614) + t2(a,p,q,i) * wm_interm_109_so_pt4(j,k,l,i) * wm_interm_49_so_pt4(a,k,j,l)
term(615) = term(615) + t2(a,p,q,i) * wm_interm_109_so_pt4(j,k,i,l) * wm_interm_49_so_pt4(a,k,j,l)
end do 
end do 
end do 
end do 
end do 

term(613) = term(613) * (-1.0d+0) 
term(614) = term(614) * (-1.0d+0) 
term(615) = term(615) * (2.0d+0) 

do i = 1, nocc 
do l = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(616) = term(616) + t2(a,p,j,i) * wm_interm_47_so_pt4(a,k,l,q) * wm_interm_88_so_pt4(l,k,j,i)
term(617) = term(617) + t2(a,p,j,i) * wm_interm_49_so_pt4(a,k,l,q) * wm_interm_88_so_pt4(k,l,j,i)
term(618) = term(618) + t2(a,p,j,i) * wm_interm_109_so_pt4(k,l,j,i) * wm_interm_49_so_pt4(a,k,l,q)
end do 
end do 
end do 
end do 
end do 

term(616) = term(616) * (-1.0d+0) 
term(617) = term(617) * (-1.0d+0) 
term(618) = term(618) * (-2.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(619) = term(619) + t2(a,p,j,i) * wm_interm_49_so_pt4(a,k,l,q) * wm_interm_88_so_pt4(l,k,i,j)
term(620) = term(620) + t2(a,p,j,i) * wm_interm_49_so_pt4(a,k,l,q) * wm_interm_88_so_pt4(k,l,i,j)
term(621) = term(621) + t2(a,p,j,i) * wm_interm_47_so_pt4(a,k,l,q) * wm_interm_88_so_pt4(k,l,i,j)
term(622) = term(622) + t2(a,p,j,i) * wm_interm_47_so_pt4(a,k,l,q) * wm_interm_88_so_pt4(l,k,i,j)
term(623) = term(623) + t2(a,p,j,i) * wm_interm_109_so_pt4(k,l,i,j) * wm_interm_49_so_pt4(a,k,l,q)
term(624) = term(624) + t2(a,p,j,i) * wm_interm_109_so_pt4(k,l,i,j) * wm_interm_47_so_pt4(a,k,l,q)
end do 
end do 
end do 
end do 
end do 

term(619) = term(619) * (-1.0d+0) 
term(620) = term(620) * (2.0d+0) 
term(621) = term(621) * (-1.0d+0) 
term(622) = term(622) * (2.0d+0) 
term(623) = term(623) * (4.0d+0) 
term(624) = term(624) * (-2.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(625) = term(625) + t2(a,b,i,j) * wm_interm_50_so_pt4(b,j) * wm_interm_70_so_pt4(p,a,q,i)
term(626) = term(626) + t2(a,b,i,j) * wm_interm_50_so_pt4(b,j) * wm_interm_69_so_pt4(p,a,q,i)
term(627) = term(627) + t2(a,b,i,j) * wm_interm_50_so_pt4(b,j) * wm_interm_67_so_pt4(p,a,q,i)
term(628) = term(628) + t2(a,b,i,j) * wm_interm_50_so_pt4(b,j) * wm_interm_68_so_pt4(p,a,q,i)
term(629) = term(629) + t2(a,b,i,j) * wm_interm_50_so_pt4(b,j) * wm_interm_84_so_pt4(p,a,q,i)
term(630) = term(630) + t2(a,b,i,j) * wm_interm_50_so_pt4(b,j) * wm_interm_74_so_pt4(p,a,q,i)
term(631) = term(631) + t2(a,b,i,j) * wm_interm_51_so_pt4(b,j) * wm_interm_70_so_pt4(p,a,q,i)
term(632) = term(632) + t2(a,b,i,j) * wm_interm_51_so_pt4(b,j) * wm_interm_69_so_pt4(p,a,q,i)
term(633) = term(633) + t2(a,b,i,j) * wm_interm_51_so_pt4(b,j) * wm_interm_67_so_pt4(p,a,q,i)
term(634) = term(634) + t2(a,b,i,j) * wm_interm_51_so_pt4(b,j) * wm_interm_68_so_pt4(p,a,q,i)
term(635) = term(635) + t2(a,b,i,j) * wm_interm_51_so_pt4(b,j) * wm_interm_84_so_pt4(p,a,q,i)
term(636) = term(636) + t2(a,b,i,j) * wm_interm_51_so_pt4(b,j) * wm_interm_74_so_pt4(p,a,q,i)
term(637) = term(637) + t2(a,b,i,j) * wm_interm_50_so_pt4(b,j) * wm_interm_98_so_pt4(p,a,q,i)
term(638) = term(638) + t2(a,b,i,j) * wm_interm_50_so_pt4(b,j) * wm_interm_97_so_pt4(p,a,q,i)
term(639) = term(639) + t2(a,b,i,j) * wm_interm_50_so_pt4(b,j) * wm_interm_95_so_pt4(p,a,q,i)
term(640) = term(640) + t2(a,b,i,j) * wm_interm_50_so_pt4(b,j) * wm_interm_96_so_pt4(p,a,q,i)
term(641) = term(641) + t2(a,b,i,j) * wm_interm_51_so_pt4(b,j) * wm_interm_98_so_pt4(p,a,q,i)
term(642) = term(642) + t2(a,b,i,j) * wm_interm_51_so_pt4(b,j) * wm_interm_97_so_pt4(p,a,q,i)
term(643) = term(643) + t2(a,b,i,j) * wm_interm_51_so_pt4(b,j) * wm_interm_95_so_pt4(p,a,q,i)
term(644) = term(644) + t2(a,b,i,j) * wm_interm_51_so_pt4(b,j) * wm_interm_96_so_pt4(p,a,q,i)
term(645) = term(645) + t1(a,q) * wm_interm_54_so_pt4(a,b,i,j) * wm_interm_70_so_pt4(p,b,i,j)
term(646) = term(646) + t1(a,q) * wm_interm_54_so_pt4(a,b,i,j) * wm_interm_69_so_pt4(p,b,i,j)
term(647) = term(647) + t1(a,q) * wm_interm_54_so_pt4(a,b,i,j) * wm_interm_67_so_pt4(p,b,i,j)
term(648) = term(648) + t1(a,q) * wm_interm_54_so_pt4(a,b,i,j) * wm_interm_68_so_pt4(p,b,i,j)
term(649) = term(649) + t1(a,q) * wm_interm_10_so_pt4(a,b,i,j) * wm_interm_77_so_pt4(b,p,j,i)
term(650) = term(650) + t1(a,q) * wm_interm_11_so_pt4(a,b,i,j) * wm_interm_77_so_pt4(b,p,j,i)
term(651) = term(651) + t1(a,q) * wm_interm_10_so_pt4(a,b,i,j) * wm_interm_79_so_pt4(b,p,j,i)
term(652) = term(652) + t1(a,q) * wm_interm_11_so_pt4(a,b,i,j) * wm_interm_79_so_pt4(b,p,j,i)
term(653) = term(653) + t1(a,q) * wm_interm_10_so_pt4(a,b,i,j) * wm_interm_75_so_pt4(b,p,j,i)
term(654) = term(654) + t1(a,q) * wm_interm_11_so_pt4(a,b,i,j) * wm_interm_75_so_pt4(b,p,j,i)
term(655) = term(655) + t1(a,q) * wm_interm_54_so_pt4(a,b,i,j) * wm_interm_84_so_pt4(p,b,i,j)
term(656) = term(656) + t1(a,q) * wm_interm_54_so_pt4(a,b,i,j) * wm_interm_74_so_pt4(p,b,i,j)
term(657) = term(657) + t1(a,q) * wm_interm_55_so_pt4(a,b,i,j) * wm_interm_70_so_pt4(p,b,i,j)
term(658) = term(658) + t1(a,q) * wm_interm_55_so_pt4(a,b,i,j) * wm_interm_69_so_pt4(p,b,i,j)
term(659) = term(659) + t1(a,q) * wm_interm_55_so_pt4(a,b,i,j) * wm_interm_67_so_pt4(p,b,i,j)
term(660) = term(660) + t1(a,q) * wm_interm_55_so_pt4(a,b,i,j) * wm_interm_68_so_pt4(p,b,i,j)
term(661) = term(661) + t1(a,q) * wm_interm_10_so_pt4(a,b,i,j) * wm_interm_66_so_pt4(b,p,j,i)
term(662) = term(662) + t1(a,q) * wm_interm_11_so_pt4(a,b,i,j) * wm_interm_66_so_pt4(b,p,j,i)
term(663) = term(663) + t1(a,q) * wm_interm_10_so_pt4(a,b,i,j) * wm_interm_65_so_pt4(b,p,j,i)
term(664) = term(664) + t1(a,q) * wm_interm_11_so_pt4(a,b,i,j) * wm_interm_65_so_pt4(b,p,j,i)
term(665) = term(665) + t1(a,q) * wm_interm_10_so_pt4(a,b,i,j) * wm_interm_63_so_pt4(b,p,j,i)
term(666) = term(666) + t1(a,q) * wm_interm_11_so_pt4(a,b,i,j) * wm_interm_63_so_pt4(b,p,j,i)
term(667) = term(667) + t1(a,q) * wm_interm_55_so_pt4(a,b,i,j) * wm_interm_84_so_pt4(p,b,i,j)
term(668) = term(668) + t1(a,q) * wm_interm_55_so_pt4(a,b,i,j) * wm_interm_74_so_pt4(p,b,i,j)
term(669) = term(669) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_19_so_pt4(a,b) * wm_interm_47_so_pt4(b,j,i,q)
term(670) = term(670) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_20_so_pt4(a,b) * wm_interm_47_so_pt4(b,j,i,q)
term(671) = term(671) + t1(a,q) * wm_interm_56_so_pt4(a,b,i,j) * wm_interm_67_so_pt4(p,b,i,j)
term(672) = term(672) + t1(a,q) * wm_interm_56_so_pt4(a,b,i,j) * wm_interm_68_so_pt4(p,b,i,j)
term(673) = term(673) + t1(a,q) * wm_interm_1_so_pt4(a,b,i,j) * wm_interm_66_so_pt4(b,p,j,i)
term(674) = term(674) + t1(a,q) * wm_interm_1_so_pt4(a,b,i,j) * wm_interm_65_so_pt4(b,p,j,i)
term(675) = term(675) + t1(a,q) * wm_interm_56_so_pt4(a,b,i,j) * wm_interm_69_so_pt4(p,b,i,j)
term(676) = term(676) + t1(a,q) * wm_interm_56_so_pt4(a,b,i,j) * wm_interm_70_so_pt4(p,b,i,j)
term(677) = term(677) + t1(a,q) * wm_interm_5_so_pt4(a,b,i,j) * wm_interm_65_so_pt4(b,p,j,i)
term(678) = term(678) + t1(a,q) * wm_interm_5_so_pt4(a,b,i,j) * wm_interm_66_so_pt4(b,p,j,i)
term(679) = term(679) + t1(a,q) * wm_interm_59_so_pt4(a,b,i,j) * wm_interm_70_so_pt4(p,b,i,j)
term(680) = term(680) + t1(a,q) * wm_interm_59_so_pt4(a,b,i,j) * wm_interm_69_so_pt4(p,b,i,j)
term(681) = term(681) + t1(a,q) * wm_interm_59_so_pt4(a,b,i,j) * wm_interm_67_so_pt4(p,b,i,j)
term(682) = term(682) + t1(a,q) * wm_interm_59_so_pt4(a,b,i,j) * wm_interm_68_so_pt4(p,b,i,j)
term(683) = term(683) + t1(a,q) * wm_interm_5_so_pt4(a,b,i,j) * wm_interm_77_so_pt4(b,p,j,i)
term(684) = term(684) + t1(a,q) * wm_interm_1_so_pt4(a,b,i,j) * wm_interm_77_so_pt4(b,p,j,i)
term(685) = term(685) + t1(a,q) * wm_interm_5_so_pt4(a,b,i,j) * wm_interm_79_so_pt4(b,p,j,i)
term(686) = term(686) + t1(a,q) * wm_interm_1_so_pt4(a,b,i,j) * wm_interm_79_so_pt4(b,p,j,i)
term(687) = term(687) + t1(a,q) * wm_interm_1_so_pt4(a,b,i,j) * wm_interm_63_so_pt4(b,p,j,i)
term(688) = term(688) + t1(a,q) * wm_interm_56_so_pt4(a,b,i,j) * wm_interm_74_so_pt4(p,b,i,j)
term(689) = term(689) + t1(a,q) * wm_interm_5_so_pt4(a,b,i,j) * wm_interm_64_so_pt4(b,p,j,i)
term(690) = term(690) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_22_so_pt4(b,j,i,q) * wm_interm_57_so_pt4(a,b)
term(691) = term(691) + t1(a,q) * wm_interm_56_so_pt4(a,b,i,j) * wm_interm_73_so_pt4(p,b,i,j)
term(692) = term(692) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_22_so_pt4(b,j,i,q) * wm_interm_58_so_pt4(a,b)
term(693) = term(693) + t1(a,q) * wm_interm_5_so_pt4(a,b,i,j) * wm_interm_75_so_pt4(b,p,j,i)
term(694) = term(694) + t1(a,q) * wm_interm_1_so_pt4(a,b,i,j) * wm_interm_75_so_pt4(b,p,j,i)
term(695) = term(695) + t1(a,q) * wm_interm_59_so_pt4(a,b,i,j) * wm_interm_84_so_pt4(p,b,i,j)
term(696) = term(696) + t1(a,q) * wm_interm_59_so_pt4(a,b,i,j) * wm_interm_74_so_pt4(p,b,i,j)
term(697) = term(697) + t1(p,i) * wm_interm_54_so_pt4(a,b,i,j) * wm_interm_70_so_pt4(a,b,q,j)
term(698) = term(698) + t1(p,i) * wm_interm_54_so_pt4(a,b,i,j) * wm_interm_69_so_pt4(a,b,q,j)
term(699) = term(699) + t1(p,i) * wm_interm_54_so_pt4(a,b,i,j) * wm_interm_67_so_pt4(a,b,q,j)
term(700) = term(700) + t1(p,i) * wm_interm_54_so_pt4(a,b,i,j) * wm_interm_68_so_pt4(a,b,q,j)
term(701) = term(701) + t1(p,i) * wm_interm_54_so_pt4(a,b,i,j) * wm_interm_84_so_pt4(a,b,q,j)
term(702) = term(702) + t1(p,i) * wm_interm_54_so_pt4(a,b,i,j) * wm_interm_74_so_pt4(a,b,q,j)
term(703) = term(703) + t1(p,i) * wm_interm_10_so_pt4(a,b,i,j) * wm_interm_75_so_pt4(b,a,j,q)
term(704) = term(704) + t1(p,i) * wm_interm_11_so_pt4(a,b,i,j) * wm_interm_75_so_pt4(b,a,j,q)
term(705) = term(705) + t1(p,i) * wm_interm_10_so_pt4(a,b,i,j) * wm_interm_77_so_pt4(b,a,j,q)
term(706) = term(706) + t1(p,i) * wm_interm_11_so_pt4(a,b,i,j) * wm_interm_77_so_pt4(b,a,j,q)
term(707) = term(707) + t1(p,i) * wm_interm_10_so_pt4(a,b,i,j) * wm_interm_79_so_pt4(b,a,j,q)
term(708) = term(708) + t1(p,i) * wm_interm_11_so_pt4(a,b,i,j) * wm_interm_79_so_pt4(b,a,j,q)
term(709) = term(709) + t1(p,i) * wm_interm_55_so_pt4(a,b,i,j) * wm_interm_70_so_pt4(a,b,q,j)
term(710) = term(710) + t1(p,i) * wm_interm_55_so_pt4(a,b,i,j) * wm_interm_69_so_pt4(a,b,q,j)
term(711) = term(711) + t1(p,i) * wm_interm_55_so_pt4(a,b,i,j) * wm_interm_67_so_pt4(a,b,q,j)
term(712) = term(712) + t1(p,i) * wm_interm_55_so_pt4(a,b,i,j) * wm_interm_68_so_pt4(a,b,q,j)
term(713) = term(713) + t1(p,i) * wm_interm_10_so_pt4(a,b,i,j) * wm_interm_63_so_pt4(b,a,j,q)
term(714) = term(714) + t1(p,i) * wm_interm_11_so_pt4(a,b,i,j) * wm_interm_63_so_pt4(b,a,j,q)
term(715) = term(715) + t1(p,i) * wm_interm_55_so_pt4(a,b,i,j) * wm_interm_84_so_pt4(a,b,q,j)
term(716) = term(716) + t1(p,i) * wm_interm_55_so_pt4(a,b,i,j) * wm_interm_74_so_pt4(a,b,q,j)
term(717) = term(717) + t1(p,i) * wm_interm_10_so_pt4(a,b,i,j) * wm_interm_66_so_pt4(b,a,j,q)
term(718) = term(718) + t1(p,i) * wm_interm_11_so_pt4(a,b,i,j) * wm_interm_66_so_pt4(b,a,j,q)
term(719) = term(719) + t1(p,i) * wm_interm_10_so_pt4(a,b,i,j) * wm_interm_65_so_pt4(b,a,j,q)
term(720) = term(720) + t1(p,i) * wm_interm_11_so_pt4(a,b,i,j) * wm_interm_65_so_pt4(b,a,j,q)
term(721) = term(721) + t1(p,i) * wm_interm_56_so_pt4(a,b,i,j) * wm_interm_67_so_pt4(a,b,q,j)
term(722) = term(722) + t1(p,i) * wm_interm_56_so_pt4(a,b,i,j) * wm_interm_68_so_pt4(a,b,q,j)
term(723) = term(723) + t1(p,i) * wm_interm_56_so_pt4(a,b,i,j) * wm_interm_74_so_pt4(a,b,q,j)
term(724) = term(724) + t1(p,i) * wm_interm_1_so_pt4(a,b,i,j) * wm_interm_63_so_pt4(b,a,j,q)
term(725) = term(725) + t1(p,i) * wm_interm_1_so_pt4(a,b,i,j) * wm_interm_66_so_pt4(b,a,j,q)
term(726) = term(726) + t1(p,i) * wm_interm_1_so_pt4(a,b,i,j) * wm_interm_65_so_pt4(b,a,j,q)
term(727) = term(727) + t1(p,i) * wm_interm_5_so_pt4(a,b,i,j) * wm_interm_64_so_pt4(b,a,j,q)
term(728) = term(728) + t1(p,i) * wm_interm_5_so_pt4(a,b,i,j) * wm_interm_63_so_pt4(b,a,j,q)
term(729) = term(729) + t1(p,i) * wm_interm_5_so_pt4(a,b,i,j) * wm_interm_65_so_pt4(b,a,j,q)
term(730) = term(730) + t1(p,i) * wm_interm_56_so_pt4(a,b,i,j) * wm_interm_69_so_pt4(a,b,q,j)
term(731) = term(731) + t1(p,i) * wm_interm_56_so_pt4(a,b,i,j) * wm_interm_73_so_pt4(a,b,q,j)
term(732) = term(732) + t1(p,i) * wm_interm_56_so_pt4(a,b,i,j) * wm_interm_84_so_pt4(a,b,q,j)
term(733) = term(733) + t1(p,i) * wm_interm_59_so_pt4(a,b,i,j) * wm_interm_70_so_pt4(a,b,q,j)
term(734) = term(734) + t1(p,i) * wm_interm_59_so_pt4(a,b,i,j) * wm_interm_69_so_pt4(a,b,q,j)
term(735) = term(735) + t1(p,i) * wm_interm_59_so_pt4(a,b,i,j) * wm_interm_67_so_pt4(a,b,q,j)
term(736) = term(736) + t1(p,i) * wm_interm_59_so_pt4(a,b,i,j) * wm_interm_68_so_pt4(a,b,q,j)
term(737) = term(737) + t1(p,i) * wm_interm_59_so_pt4(a,b,i,j) * wm_interm_84_so_pt4(a,b,q,j)
term(738) = term(738) + t1(p,i) * wm_interm_59_so_pt4(a,b,i,j) * wm_interm_74_so_pt4(a,b,q,j)
term(739) = term(739) + t1(p,i) * wm_interm_5_so_pt4(a,b,i,j) * wm_interm_75_so_pt4(b,a,j,q)
term(740) = term(740) + t1(p,i) * wm_interm_1_so_pt4(a,b,i,j) * wm_interm_75_so_pt4(b,a,j,q)
term(741) = term(741) + t1(p,i) * wm_interm_5_so_pt4(a,b,i,j) * wm_interm_77_so_pt4(b,a,j,q)
term(742) = term(742) + t1(p,i) * wm_interm_1_so_pt4(a,b,i,j) * wm_interm_77_so_pt4(b,a,j,q)
term(743) = term(743) + t1(p,i) * wm_interm_5_so_pt4(a,b,i,j) * wm_interm_79_so_pt4(b,a,j,q)
term(744) = term(744) + t1(p,i) * wm_interm_1_so_pt4(a,b,i,j) * wm_interm_79_so_pt4(b,a,j,q)
term(745) = term(745) + t1(a,q) * wm_interm_54_so_pt4(a,b,i,j) * wm_interm_98_so_pt4(p,b,i,j)
term(746) = term(746) + t1(a,q) * wm_interm_54_so_pt4(a,b,i,j) * wm_interm_97_so_pt4(p,b,i,j)
term(747) = term(747) + t1(a,q) * wm_interm_54_so_pt4(a,b,i,j) * wm_interm_95_so_pt4(p,b,i,j)
term(748) = term(748) + t1(a,q) * wm_interm_54_so_pt4(a,b,i,j) * wm_interm_96_so_pt4(p,b,i,j)
term(749) = term(749) + t1(a,q) * wm_interm_100_so_pt4(b,p,i,j) * wm_interm_10_so_pt4(a,b,j,i)
term(750) = term(750) + t1(a,q) * wm_interm_100_so_pt4(b,p,i,j) * wm_interm_11_so_pt4(a,b,j,i)
term(751) = term(751) + t1(a,q) * wm_interm_102_so_pt4(b,p,i,j) * wm_interm_10_so_pt4(a,b,j,i)
term(752) = term(752) + t1(a,q) * wm_interm_102_so_pt4(b,p,i,j) * wm_interm_11_so_pt4(a,b,j,i)
term(753) = term(753) + t1(a,q) * wm_interm_55_so_pt4(a,b,i,j) * wm_interm_98_so_pt4(p,b,i,j)
term(754) = term(754) + t1(a,q) * wm_interm_55_so_pt4(a,b,i,j) * wm_interm_97_so_pt4(p,b,i,j)
term(755) = term(755) + t1(a,q) * wm_interm_55_so_pt4(a,b,i,j) * wm_interm_95_so_pt4(p,b,i,j)
term(756) = term(756) + t1(a,q) * wm_interm_55_so_pt4(a,b,i,j) * wm_interm_96_so_pt4(p,b,i,j)
term(757) = term(757) + t1(a,q) * wm_interm_10_so_pt4(a,b,i,j) * wm_interm_93_so_pt4(b,p,j,i)
term(758) = term(758) + t1(a,q) * wm_interm_11_so_pt4(a,b,i,j) * wm_interm_93_so_pt4(b,p,j,i)
term(759) = term(759) + t1(a,q) * wm_interm_10_so_pt4(a,b,i,j) * wm_interm_94_so_pt4(b,p,j,i)
term(760) = term(760) + t1(a,q) * wm_interm_11_so_pt4(a,b,i,j) * wm_interm_94_so_pt4(b,p,j,i)
term(761) = term(761) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_19_so_pt4(a,b) * wm_interm_47_so_pt4(b,j,i,q)
term(762) = term(762) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_20_so_pt4(a,b) * wm_interm_47_so_pt4(b,j,i,q)
term(763) = term(763) + t1(a,q) * wm_interm_56_so_pt4(a,b,i,j) * wm_interm_95_so_pt4(p,b,i,j)
term(764) = term(764) + t1(a,q) * wm_interm_56_so_pt4(a,b,i,j) * wm_interm_96_so_pt4(p,b,i,j)
term(765) = term(765) + t1(a,q) * wm_interm_1_so_pt4(a,b,i,j) * wm_interm_93_so_pt4(b,p,j,i)
term(766) = term(766) + t1(a,q) * wm_interm_1_so_pt4(a,b,i,j) * wm_interm_94_so_pt4(b,p,j,i)
term(767) = term(767) + t1(a,q) * wm_interm_56_so_pt4(a,b,i,j) * wm_interm_97_so_pt4(p,b,i,j)
term(768) = term(768) + t1(a,q) * wm_interm_56_so_pt4(a,b,i,j) * wm_interm_98_so_pt4(p,b,i,j)
term(769) = term(769) + t1(a,q) * wm_interm_5_so_pt4(a,b,i,j) * wm_interm_94_so_pt4(b,p,j,i)
term(770) = term(770) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_22_so_pt4(b,j,i,q) * wm_interm_57_so_pt4(a,b)
term(771) = term(771) + t1(a,q) * wm_interm_5_so_pt4(a,b,i,j) * wm_interm_93_so_pt4(b,p,j,i)
term(772) = term(772) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_22_so_pt4(b,j,i,q) * wm_interm_58_so_pt4(a,b)
term(773) = term(773) + t1(a,q) * wm_interm_59_so_pt4(a,b,i,j) * wm_interm_98_so_pt4(p,b,i,j)
term(774) = term(774) + t1(a,q) * wm_interm_59_so_pt4(a,b,i,j) * wm_interm_97_so_pt4(p,b,i,j)
term(775) = term(775) + t1(a,q) * wm_interm_59_so_pt4(a,b,i,j) * wm_interm_95_so_pt4(p,b,i,j)
term(776) = term(776) + t1(a,q) * wm_interm_59_so_pt4(a,b,i,j) * wm_interm_96_so_pt4(p,b,i,j)
term(777) = term(777) + t1(a,q) * wm_interm_100_so_pt4(b,p,i,j) * wm_interm_5_so_pt4(a,b,j,i)
term(778) = term(778) + t1(a,q) * wm_interm_100_so_pt4(b,p,i,j) * wm_interm_1_so_pt4(a,b,j,i)
term(779) = term(779) + t1(a,q) * wm_interm_102_so_pt4(b,p,i,j) * wm_interm_5_so_pt4(a,b,j,i)
term(780) = term(780) + t1(a,q) * wm_interm_102_so_pt4(b,p,i,j) * wm_interm_1_so_pt4(a,b,j,i)
term(781) = term(781) + t1(p,i) * wm_interm_54_so_pt4(a,b,i,j) * wm_interm_98_so_pt4(a,b,q,j)
term(782) = term(782) + t1(p,i) * wm_interm_54_so_pt4(a,b,i,j) * wm_interm_97_so_pt4(a,b,q,j)
term(783) = term(783) + t1(p,i) * wm_interm_54_so_pt4(a,b,i,j) * wm_interm_95_so_pt4(a,b,q,j)
term(784) = term(784) + t1(p,i) * wm_interm_54_so_pt4(a,b,i,j) * wm_interm_96_so_pt4(a,b,q,j)
term(785) = term(785) + t1(p,i) * wm_interm_100_so_pt4(a,b,j,q) * wm_interm_10_so_pt4(b,a,i,j)
term(786) = term(786) + t1(p,i) * wm_interm_100_so_pt4(a,b,j,q) * wm_interm_11_so_pt4(b,a,i,j)
term(787) = term(787) + t1(p,i) * wm_interm_102_so_pt4(a,b,j,q) * wm_interm_10_so_pt4(b,a,i,j)
term(788) = term(788) + t1(p,i) * wm_interm_102_so_pt4(a,b,j,q) * wm_interm_11_so_pt4(b,a,i,j)
term(789) = term(789) + t1(p,i) * wm_interm_55_so_pt4(a,b,i,j) * wm_interm_98_so_pt4(a,b,q,j)
term(790) = term(790) + t1(p,i) * wm_interm_55_so_pt4(a,b,i,j) * wm_interm_97_so_pt4(a,b,q,j)
term(791) = term(791) + t1(p,i) * wm_interm_55_so_pt4(a,b,i,j) * wm_interm_95_so_pt4(a,b,q,j)
term(792) = term(792) + t1(p,i) * wm_interm_55_so_pt4(a,b,i,j) * wm_interm_96_so_pt4(a,b,q,j)
term(793) = term(793) + t1(p,i) * wm_interm_10_so_pt4(a,b,i,j) * wm_interm_93_so_pt4(b,a,j,q)
term(794) = term(794) + t1(p,i) * wm_interm_11_so_pt4(a,b,i,j) * wm_interm_93_so_pt4(b,a,j,q)
term(795) = term(795) + t1(p,i) * wm_interm_10_so_pt4(a,b,i,j) * wm_interm_94_so_pt4(b,a,j,q)
term(796) = term(796) + t1(p,i) * wm_interm_11_so_pt4(a,b,i,j) * wm_interm_94_so_pt4(b,a,j,q)
term(797) = term(797) + t1(p,i) * wm_interm_56_so_pt4(a,b,i,j) * wm_interm_95_so_pt4(a,b,q,j)
term(798) = term(798) + t1(p,i) * wm_interm_56_so_pt4(a,b,i,j) * wm_interm_96_so_pt4(a,b,q,j)
term(799) = term(799) + t1(p,i) * wm_interm_1_so_pt4(a,b,i,j) * wm_interm_93_so_pt4(b,a,j,q)
term(800) = term(800) + t1(p,i) * wm_interm_1_so_pt4(a,b,i,j) * wm_interm_94_so_pt4(b,a,j,q)
term(801) = term(801) + t1(p,i) * wm_interm_5_so_pt4(a,b,i,j) * wm_interm_94_so_pt4(b,a,j,q)
term(802) = term(802) + t1(p,i) * wm_interm_5_so_pt4(a,b,i,j) * wm_interm_93_so_pt4(b,a,j,q)
term(803) = term(803) + t1(p,i) * wm_interm_56_so_pt4(a,b,i,j) * wm_interm_97_so_pt4(a,b,q,j)
term(804) = term(804) + t1(p,i) * wm_interm_56_so_pt4(a,b,i,j) * wm_interm_98_so_pt4(a,b,q,j)
term(805) = term(805) + t1(p,i) * wm_interm_59_so_pt4(a,b,i,j) * wm_interm_98_so_pt4(a,b,q,j)
term(806) = term(806) + t1(p,i) * wm_interm_59_so_pt4(a,b,i,j) * wm_interm_97_so_pt4(a,b,q,j)
term(807) = term(807) + t1(p,i) * wm_interm_59_so_pt4(a,b,i,j) * wm_interm_95_so_pt4(a,b,q,j)
term(808) = term(808) + t1(p,i) * wm_interm_59_so_pt4(a,b,i,j) * wm_interm_96_so_pt4(a,b,q,j)
term(809) = term(809) + t1(p,i) * wm_interm_100_so_pt4(a,b,j,q) * wm_interm_5_so_pt4(b,a,i,j)
term(810) = term(810) + t1(p,i) * wm_interm_100_so_pt4(a,b,j,q) * wm_interm_1_so_pt4(b,a,i,j)
term(811) = term(811) + t1(p,i) * wm_interm_102_so_pt4(a,b,j,q) * wm_interm_5_so_pt4(b,a,i,j)
term(812) = term(812) + t1(p,i) * wm_interm_102_so_pt4(a,b,j,q) * wm_interm_1_so_pt4(b,a,i,j)
term(813) = term(813) + t1(a,i) * wm_interm_5_so_pt4(a,b,j,q) * wm_interm_66_so_pt4(b,p,i,j)
term(814) = term(814) + t1(a,i) * wm_interm_5_so_pt4(a,b,j,q) * wm_interm_65_so_pt4(b,p,i,j)
term(815) = term(815) + t1(a,i) * wm_interm_1_so_pt4(a,b,j,q) * wm_interm_65_so_pt4(b,p,i,j)
term(816) = term(816) + t1(a,i) * wm_interm_1_so_pt4(a,b,j,q) * wm_interm_66_so_pt4(b,p,i,j)
term(817) = term(817) + t1(a,i) * wm_interm_10_so_pt4(a,b,j,q) * wm_interm_65_so_pt4(b,p,i,j)
term(818) = term(818) + t1(a,i) * wm_interm_10_so_pt4(a,b,j,q) * wm_interm_66_so_pt4(b,p,i,j)
term(819) = term(819) + t1(a,i) * wm_interm_11_so_pt4(a,b,j,q) * wm_interm_65_so_pt4(b,p,i,j)
term(820) = term(820) + t1(a,i) * wm_interm_11_so_pt4(a,b,j,q) * wm_interm_66_so_pt4(b,p,i,j)
term(821) = term(821) + t1(a,i) * wm_interm_1_so_pt4(a,b,j,q) * wm_interm_77_so_pt4(b,p,i,j)
term(822) = term(822) + t1(a,i) * wm_interm_5_so_pt4(a,b,j,q) * wm_interm_77_so_pt4(b,p,i,j)
term(823) = term(823) + t1(a,i) * wm_interm_10_so_pt4(a,b,j,q) * wm_interm_77_so_pt4(b,p,i,j)
term(824) = term(824) + t1(a,i) * wm_interm_11_so_pt4(a,b,j,q) * wm_interm_77_so_pt4(b,p,i,j)
term(825) = term(825) + t1(a,i) * wm_interm_1_so_pt4(a,b,j,q) * wm_interm_79_so_pt4(b,p,i,j)
term(826) = term(826) + t1(a,i) * wm_interm_5_so_pt4(a,b,j,q) * wm_interm_79_so_pt4(b,p,i,j)
term(827) = term(827) + t1(a,i) * wm_interm_10_so_pt4(a,b,j,q) * wm_interm_79_so_pt4(b,p,i,j)
term(828) = term(828) + t1(a,i) * wm_interm_11_so_pt4(a,b,j,q) * wm_interm_79_so_pt4(b,p,i,j)
term(829) = term(829) + t1(a,i) * wm_interm_5_so_pt4(a,b,j,q) * wm_interm_63_so_pt4(b,p,i,j)
term(830) = term(830) + t1(a,i) * wm_interm_1_so_pt4(a,b,j,q) * wm_interm_64_so_pt4(b,p,i,j)
term(831) = term(831) + t1(a,i) * wm_interm_1_so_pt4(a,b,j,q) * wm_interm_75_so_pt4(b,p,i,j)
term(832) = term(832) + t1(a,i) * wm_interm_5_so_pt4(a,b,j,q) * wm_interm_75_so_pt4(b,p,i,j)
term(833) = term(833) + t1(a,i) * wm_interm_10_so_pt4(a,b,j,q) * wm_interm_75_so_pt4(b,p,i,j)
term(834) = term(834) + t1(a,i) * wm_interm_11_so_pt4(a,b,j,q) * wm_interm_75_so_pt4(b,p,i,j)
term(835) = term(835) + t1(a,i) * wm_interm_10_so_pt4(a,b,j,q) * wm_interm_64_so_pt4(b,p,i,j)
term(836) = term(836) + t1(a,i) * wm_interm_11_so_pt4(a,b,j,q) * wm_interm_64_so_pt4(b,p,i,j)
term(837) = term(837) + t1(a,i) * wm_interm_5_so_pt4(b,p,i,j) * wm_interm_63_so_pt4(a,b,j,q)
term(838) = term(838) + t1(a,i) * wm_interm_1_so_pt4(b,p,i,j) * wm_interm_64_so_pt4(a,b,j,q)
term(839) = term(839) + t1(a,i) * wm_interm_1_so_pt4(b,p,i,j) * wm_interm_63_so_pt4(a,b,j,q)
term(840) = term(840) + t1(a,i) * wm_interm_1_so_pt4(b,p,i,j) * wm_interm_65_so_pt4(a,b,j,q)
term(841) = term(841) + t1(a,i) * wm_interm_5_so_pt4(b,p,i,j) * wm_interm_66_so_pt4(a,b,j,q)
term(842) = term(842) + t1(a,i) * wm_interm_5_so_pt4(b,p,i,j) * wm_interm_65_so_pt4(a,b,j,q)
term(843) = term(843) + t1(a,i) * wm_interm_10_so_pt4(b,p,i,j) * wm_interm_64_so_pt4(a,b,j,q)
term(844) = term(844) + t1(a,i) * wm_interm_10_so_pt4(b,p,i,j) * wm_interm_63_so_pt4(a,b,j,q)
term(845) = term(845) + t1(a,i) * wm_interm_11_so_pt4(b,p,i,j) * wm_interm_64_so_pt4(a,b,j,q)
term(846) = term(846) + t1(a,i) * wm_interm_11_so_pt4(b,p,i,j) * wm_interm_63_so_pt4(a,b,j,q)
term(847) = term(847) + t1(a,i) * wm_interm_10_so_pt4(b,p,i,j) * wm_interm_65_so_pt4(a,b,j,q)
term(848) = term(848) + t1(a,i) * wm_interm_11_so_pt4(b,p,i,j) * wm_interm_65_so_pt4(a,b,j,q)
term(849) = term(849) + t1(a,i) * wm_interm_10_so_pt4(b,p,i,j) * wm_interm_75_so_pt4(a,b,j,q)
term(850) = term(850) + t1(a,i) * wm_interm_5_so_pt4(b,p,i,j) * wm_interm_75_so_pt4(a,b,j,q)
term(851) = term(851) + t1(a,i) * wm_interm_1_so_pt4(b,p,i,j) * wm_interm_75_so_pt4(a,b,j,q)
term(852) = term(852) + t1(a,i) * wm_interm_11_so_pt4(b,p,i,j) * wm_interm_75_so_pt4(a,b,j,q)
term(853) = term(853) + t1(a,i) * wm_interm_10_so_pt4(b,p,i,j) * wm_interm_77_so_pt4(a,b,j,q)
term(854) = term(854) + t1(a,i) * wm_interm_5_so_pt4(b,p,i,j) * wm_interm_77_so_pt4(a,b,j,q)
term(855) = term(855) + t1(a,i) * wm_interm_1_so_pt4(b,p,i,j) * wm_interm_77_so_pt4(a,b,j,q)
term(856) = term(856) + t1(a,i) * wm_interm_11_so_pt4(b,p,i,j) * wm_interm_77_so_pt4(a,b,j,q)
term(857) = term(857) + t1(a,i) * wm_interm_10_so_pt4(b,p,i,j) * wm_interm_79_so_pt4(a,b,j,q)
term(858) = term(858) + t1(a,i) * wm_interm_5_so_pt4(b,p,i,j) * wm_interm_79_so_pt4(a,b,j,q)
term(859) = term(859) + t1(a,i) * wm_interm_1_so_pt4(b,p,i,j) * wm_interm_79_so_pt4(a,b,j,q)
term(860) = term(860) + t1(a,i) * wm_interm_11_so_pt4(b,p,i,j) * wm_interm_79_so_pt4(a,b,j,q)
term(861) = term(861) + t1(a,i) * wm_interm_5_so_pt4(a,b,j,q) * wm_interm_93_so_pt4(b,p,i,j)
term(862) = term(862) + t1(a,i) * wm_interm_5_so_pt4(a,b,j,q) * wm_interm_94_so_pt4(b,p,i,j)
term(863) = term(863) + t1(a,i) * wm_interm_1_so_pt4(a,b,j,q) * wm_interm_94_so_pt4(b,p,i,j)
term(864) = term(864) + t1(a,i) * wm_interm_1_so_pt4(a,b,j,q) * wm_interm_93_so_pt4(b,p,i,j)
term(865) = term(865) + t1(a,i) * wm_interm_10_so_pt4(a,b,j,q) * wm_interm_94_so_pt4(b,p,i,j)
term(866) = term(866) + t1(a,i) * wm_interm_10_so_pt4(a,b,j,q) * wm_interm_93_so_pt4(b,p,i,j)
term(867) = term(867) + t1(a,i) * wm_interm_11_so_pt4(a,b,j,q) * wm_interm_94_so_pt4(b,p,i,j)
term(868) = term(868) + t1(a,i) * wm_interm_11_so_pt4(a,b,j,q) * wm_interm_93_so_pt4(b,p,i,j)
term(869) = term(869) + t1(a,i) * wm_interm_100_so_pt4(b,p,i,j) * wm_interm_1_so_pt4(a,b,j,q)
term(870) = term(870) + t1(a,i) * wm_interm_100_so_pt4(b,p,i,j) * wm_interm_5_so_pt4(a,b,j,q)
term(871) = term(871) + t1(a,i) * wm_interm_100_so_pt4(b,p,i,j) * wm_interm_10_so_pt4(a,b,j,q)
term(872) = term(872) + t1(a,i) * wm_interm_100_so_pt4(b,p,i,j) * wm_interm_11_so_pt4(a,b,j,q)
term(873) = term(873) + t1(a,i) * wm_interm_102_so_pt4(b,p,i,j) * wm_interm_1_so_pt4(a,b,j,q)
term(874) = term(874) + t1(a,i) * wm_interm_102_so_pt4(b,p,i,j) * wm_interm_5_so_pt4(a,b,j,q)
term(875) = term(875) + t1(a,i) * wm_interm_102_so_pt4(b,p,i,j) * wm_interm_10_so_pt4(a,b,j,q)
term(876) = term(876) + t1(a,i) * wm_interm_102_so_pt4(b,p,i,j) * wm_interm_11_so_pt4(a,b,j,q)
term(877) = term(877) + t1(a,i) * wm_interm_5_so_pt4(b,p,i,j) * wm_interm_93_so_pt4(a,b,j,q)
term(878) = term(878) + t1(a,i) * wm_interm_1_so_pt4(b,p,i,j) * wm_interm_94_so_pt4(a,b,j,q)
term(879) = term(879) + t1(a,i) * wm_interm_1_so_pt4(b,p,i,j) * wm_interm_93_so_pt4(a,b,j,q)
term(880) = term(880) + t1(a,i) * wm_interm_5_so_pt4(b,p,i,j) * wm_interm_94_so_pt4(a,b,j,q)
term(881) = term(881) + t1(a,i) * wm_interm_10_so_pt4(b,p,i,j) * wm_interm_94_so_pt4(a,b,j,q)
term(882) = term(882) + t1(a,i) * wm_interm_10_so_pt4(b,p,i,j) * wm_interm_93_so_pt4(a,b,j,q)
term(883) = term(883) + t1(a,i) * wm_interm_11_so_pt4(b,p,i,j) * wm_interm_94_so_pt4(a,b,j,q)
term(884) = term(884) + t1(a,i) * wm_interm_11_so_pt4(b,p,i,j) * wm_interm_93_so_pt4(a,b,j,q)
term(885) = term(885) + t1(a,i) * wm_interm_100_so_pt4(a,b,j,q) * wm_interm_10_so_pt4(b,p,i,j)
term(886) = term(886) + t1(a,i) * wm_interm_100_so_pt4(a,b,j,q) * wm_interm_5_so_pt4(b,p,i,j)
term(887) = term(887) + t1(a,i) * wm_interm_100_so_pt4(a,b,j,q) * wm_interm_1_so_pt4(b,p,i,j)
term(888) = term(888) + t1(a,i) * wm_interm_100_so_pt4(a,b,j,q) * wm_interm_11_so_pt4(b,p,i,j)
term(889) = term(889) + t1(a,i) * wm_interm_102_so_pt4(a,b,j,q) * wm_interm_10_so_pt4(b,p,i,j)
term(890) = term(890) + t1(a,i) * wm_interm_102_so_pt4(a,b,j,q) * wm_interm_5_so_pt4(b,p,i,j)
term(891) = term(891) + t1(a,i) * wm_interm_102_so_pt4(a,b,j,q) * wm_interm_1_so_pt4(b,p,i,j)
term(892) = term(892) + t1(a,i) * wm_interm_102_so_pt4(a,b,j,q) * wm_interm_11_so_pt4(b,p,i,j)
term(893) = term(893) + t1(a,q) * wm_interm_10_so_pt4(b,p,i,j) * wm_interm_66_so_pt4(a,b,j,i)
term(894) = term(894) + t1(a,q) * wm_interm_10_so_pt4(b,p,i,j) * wm_interm_63_so_pt4(a,b,j,i)
term(895) = term(895) + t1(a,q) * wm_interm_10_so_pt4(b,p,i,j) * wm_interm_64_so_pt4(a,b,j,i)
term(896) = term(896) + t1(a,q) * wm_interm_5_so_pt4(b,p,i,j) * wm_interm_77_so_pt4(a,b,j,i)
term(897) = term(897) + t1(a,q) * wm_interm_5_so_pt4(b,p,i,j) * wm_interm_79_so_pt4(a,b,j,i)
term(898) = term(898) + t1(a,q) * wm_interm_5_so_pt4(b,p,i,j) * wm_interm_65_so_pt4(a,b,j,i)
term(899) = term(899) + t1(a,q) * wm_interm_5_so_pt4(b,p,i,j) * wm_interm_66_so_pt4(a,b,j,i)
term(900) = term(900) + t1(a,q) * wm_interm_5_so_pt4(b,p,i,j) * wm_interm_64_so_pt4(a,b,j,i)
term(901) = term(901) + t1(a,q) * wm_interm_5_so_pt4(b,p,i,j) * wm_interm_75_so_pt4(a,b,j,i)
term(902) = term(902) + t1(a,q) * wm_interm_1_so_pt4(b,p,i,j) * wm_interm_66_so_pt4(a,b,j,i)
term(903) = term(903) + t1(a,q) * wm_interm_11_so_pt4(b,p,i,j) * wm_interm_66_so_pt4(a,b,j,i)
term(904) = term(904) + t1(a,q) * wm_interm_1_so_pt4(b,p,i,j) * wm_interm_63_so_pt4(a,b,j,i)
term(905) = term(905) + t1(a,q) * wm_interm_11_so_pt4(b,p,i,j) * wm_interm_63_so_pt4(a,b,j,i)
term(906) = term(906) + t1(a,q) * wm_interm_1_so_pt4(b,p,i,j) * wm_interm_64_so_pt4(a,b,j,i)
term(907) = term(907) + t1(a,q) * wm_interm_11_so_pt4(b,p,i,j) * wm_interm_64_so_pt4(a,b,j,i)
term(908) = term(908) + t1(p,i) * wm_interm_10_so_pt4(a,b,j,q) * wm_interm_66_so_pt4(b,a,i,j)
term(909) = term(909) + t1(p,i) * wm_interm_10_so_pt4(a,b,j,q) * wm_interm_63_so_pt4(b,a,i,j)
term(910) = term(910) + t1(p,i) * wm_interm_10_so_pt4(a,b,j,q) * wm_interm_64_so_pt4(b,a,i,j)
term(911) = term(911) + t1(p,i) * wm_interm_11_so_pt4(a,b,j,q) * wm_interm_66_so_pt4(b,a,i,j)
term(912) = term(912) + t1(p,i) * wm_interm_11_so_pt4(a,b,j,q) * wm_interm_63_so_pt4(b,a,i,j)
term(913) = term(913) + t1(p,i) * wm_interm_11_so_pt4(a,b,j,q) * wm_interm_64_so_pt4(b,a,i,j)
term(914) = term(914) + t1(p,i) * wm_interm_1_so_pt4(a,b,j,q) * wm_interm_66_so_pt4(b,a,i,j)
term(915) = term(915) + t1(p,i) * wm_interm_1_so_pt4(a,b,j,q) * wm_interm_63_so_pt4(b,a,i,j)
term(916) = term(916) + t1(p,i) * wm_interm_1_so_pt4(a,b,j,q) * wm_interm_64_so_pt4(b,a,i,j)
term(917) = term(917) + t1(p,i) * wm_interm_5_so_pt4(a,b,j,q) * wm_interm_77_so_pt4(b,a,i,j)
term(918) = term(918) + t1(p,i) * wm_interm_5_so_pt4(a,b,j,q) * wm_interm_79_so_pt4(b,a,i,j)
term(919) = term(919) + t1(p,i) * wm_interm_5_so_pt4(a,b,j,q) * wm_interm_65_so_pt4(b,a,i,j)
term(920) = term(920) + t1(p,i) * wm_interm_5_so_pt4(a,b,j,q) * wm_interm_64_so_pt4(b,a,i,j)
term(921) = term(921) + t1(p,i) * wm_interm_5_so_pt4(a,b,j,q) * wm_interm_63_so_pt4(b,a,i,j)
term(922) = term(922) + t1(p,i) * wm_interm_5_so_pt4(a,b,j,q) * wm_interm_75_so_pt4(b,a,i,j)
term(923) = term(923) + t1(a,q) * wm_interm_10_so_pt4(b,p,i,j) * wm_interm_93_so_pt4(a,b,j,i)
term(924) = term(924) + t1(a,q) * wm_interm_10_so_pt4(b,p,i,j) * wm_interm_94_so_pt4(a,b,j,i)
term(925) = term(925) + t1(a,q) * wm_interm_100_so_pt4(a,b,i,j) * wm_interm_5_so_pt4(b,p,j,i)
term(926) = term(926) + t1(a,q) * wm_interm_102_so_pt4(a,b,i,j) * wm_interm_5_so_pt4(b,p,j,i)
term(927) = term(927) + t1(a,q) * wm_interm_5_so_pt4(b,p,i,j) * wm_interm_94_so_pt4(a,b,j,i)
term(928) = term(928) + t1(a,q) * wm_interm_5_so_pt4(b,p,i,j) * wm_interm_93_so_pt4(a,b,j,i)
term(929) = term(929) + t1(a,q) * wm_interm_1_so_pt4(b,p,i,j) * wm_interm_93_so_pt4(a,b,j,i)
term(930) = term(930) + t1(a,q) * wm_interm_11_so_pt4(b,p,i,j) * wm_interm_93_so_pt4(a,b,j,i)
term(931) = term(931) + t1(a,q) * wm_interm_1_so_pt4(b,p,i,j) * wm_interm_94_so_pt4(a,b,j,i)
term(932) = term(932) + t1(a,q) * wm_interm_11_so_pt4(b,p,i,j) * wm_interm_94_so_pt4(a,b,j,i)
term(933) = term(933) + t1(p,i) * wm_interm_10_so_pt4(a,b,j,q) * wm_interm_93_so_pt4(b,a,i,j)
term(934) = term(934) + t1(p,i) * wm_interm_10_so_pt4(a,b,j,q) * wm_interm_94_so_pt4(b,a,i,j)
term(935) = term(935) + t1(p,i) * wm_interm_11_so_pt4(a,b,j,q) * wm_interm_93_so_pt4(b,a,i,j)
term(936) = term(936) + t1(p,i) * wm_interm_11_so_pt4(a,b,j,q) * wm_interm_94_so_pt4(b,a,i,j)
term(937) = term(937) + t1(p,i) * wm_interm_1_so_pt4(a,b,j,q) * wm_interm_93_so_pt4(b,a,i,j)
term(938) = term(938) + t1(p,i) * wm_interm_1_so_pt4(a,b,j,q) * wm_interm_94_so_pt4(b,a,i,j)
term(939) = term(939) + t1(p,i) * wm_interm_100_so_pt4(a,b,i,j) * wm_interm_5_so_pt4(b,a,j,q)
term(940) = term(940) + t1(p,i) * wm_interm_102_so_pt4(a,b,i,j) * wm_interm_5_so_pt4(b,a,j,q)
term(941) = term(941) + t1(p,i) * wm_interm_5_so_pt4(a,b,j,q) * wm_interm_94_so_pt4(b,a,i,j)
term(942) = term(942) + t1(p,i) * wm_interm_5_so_pt4(a,b,j,q) * wm_interm_93_so_pt4(b,a,i,j)
end do 
end do 
end do 
end do 

term(625) = term(625) * (-8.0d+0) 
term(626) = term(626) * (16.0d+0) 
term(627) = term(627) * (16.0d+0) 
term(628) = term(628) * (-32.0d+0) 
term(629) = term(629) * (-8.0d+0) 
term(630) = term(630) * (16.0d+0) 
term(631) = term(631) * (4.0d+0) 
term(632) = term(632) * (-8.0d+0) 
term(633) = term(633) * (-8.0d+0) 
term(634) = term(634) * (16.0d+0) 
term(635) = term(635) * (4.0d+0) 
term(636) = term(636) * (-8.0d+0) 
term(637) = term(637) * (-32.0d+0) 
term(638) = term(638) * (32.0d+0) 
term(639) = term(639) * (64.0d+0) 
term(640) = term(640) * (-64.0d+0) 
term(641) = term(641) * (16.0d+0) 
term(642) = term(642) * (-16.0d+0) 
term(643) = term(643) * (-32.0d+0) 
term(644) = term(644) * (32.0d+0) 
term(645) = term(645) * (2.0d+0) 
term(646) = term(646) * (-4.0d+0) 
term(647) = term(647) * (-4.0d+0) 
term(648) = term(648) * (8.0d+0) 
term(649) = term(649) * (2.0d+0) 
term(650) = term(650) * (-4.0d+0) 
term(651) = term(651) * (-4.0d+0) 
term(652) = term(652) * (8.0d+0) 
term(653) = term(653) * (2.0d+0) 
term(654) = term(654) * (-4.0d+0) 
term(655) = term(655) * (2.0d+0) 
term(656) = term(656) * (-4.0d+0) 
term(657) = term(657) * (-1.0d+0) 
term(658) = term(658) * (2.0d+0) 
term(659) = term(659) * (2.0d+0) 
term(660) = term(660) * (-4.0d+0) 
term(661) = term(661) * (-1.0d+0) 
term(662) = term(662) * (2.0d+0) 
term(663) = term(663) * (2.0d+0) 
term(664) = term(664) * (-4.0d+0) 
term(665) = term(665) * (-1.0d+0) 
term(666) = term(666) * (2.0d+0) 
term(667) = term(667) * (-1.0d+0) 
term(668) = term(668) * (2.0d+0) 
term(669) = term(669) * (-0.5d+0) 
term(671) = term(671) * (-1.0d+0) 
term(672) = term(672) * (2.0d+0) 
term(673) = term(673) * (-1.0d+0) 
term(674) = term(674) * (2.0d+0) 
term(675) = term(675) * (-1.0d+0) 
term(676) = term(676) * (2.0d+0) 
term(677) = term(677) * (-1.0d+0) 
term(678) = term(678) * (2.0d+0) 
term(679) = term(679) * (-1.0d+0) 
term(680) = term(680) * (2.0d+0) 
term(681) = term(681) * (2.0d+0) 
term(682) = term(682) * (-4.0d+0) 
term(683) = term(683) * (-1.0d+0) 
term(684) = term(684) * (2.0d+0) 
term(685) = term(685) * (2.0d+0) 
term(686) = term(686) * (-4.0d+0) 
term(687) = term(687) * (-1.0d+0) 
term(688) = term(688) * (-1.0d+0) 
term(689) = term(689) * (-1.0d+0) 
term(690) = term(690) * (2.0d+0) 
term(691) = term(691) * (-1.0d+0) 
term(692) = term(692) * (-1.0d+0) 
term(693) = term(693) * (-1.0d+0) 
term(694) = term(694) * (2.0d+0) 
term(695) = term(695) * (-1.0d+0) 
term(696) = term(696) * (2.0d+0) 
term(697) = term(697) * (2.0d+0) 
term(698) = term(698) * (-4.0d+0) 
term(699) = term(699) * (-4.0d+0) 
term(700) = term(700) * (8.0d+0) 
term(701) = term(701) * (2.0d+0) 
term(702) = term(702) * (-4.0d+0) 
term(703) = term(703) * (2.0d+0) 
term(704) = term(704) * (-4.0d+0) 
term(705) = term(705) * (2.0d+0) 
term(706) = term(706) * (-4.0d+0) 
term(707) = term(707) * (-4.0d+0) 
term(708) = term(708) * (8.0d+0) 
term(709) = term(709) * (-1.0d+0) 
term(710) = term(710) * (2.0d+0) 
term(711) = term(711) * (2.0d+0) 
term(712) = term(712) * (-4.0d+0) 
term(713) = term(713) * (-1.0d+0) 
term(714) = term(714) * (2.0d+0) 
term(715) = term(715) * (-1.0d+0) 
term(716) = term(716) * (2.0d+0) 
term(717) = term(717) * (-1.0d+0) 
term(718) = term(718) * (2.0d+0) 
term(719) = term(719) * (2.0d+0) 
term(720) = term(720) * (-4.0d+0) 
term(721) = term(721) * (-1.0d+0) 
term(722) = term(722) * (2.0d+0) 
term(723) = term(723) * (-1.0d+0) 
term(724) = term(724) * (-1.0d+0) 
term(725) = term(725) * (-1.0d+0) 
term(726) = term(726) * (2.0d+0) 
term(727) = term(727) * (-1.0d+0) 
term(728) = term(728) * (2.0d+0) 
term(729) = term(729) * (-1.0d+0) 
term(730) = term(730) * (-1.0d+0) 
term(731) = term(731) * (-1.0d+0) 
term(732) = term(732) * (2.0d+0) 
term(733) = term(733) * (-1.0d+0) 
term(734) = term(734) * (2.0d+0) 
term(735) = term(735) * (2.0d+0) 
term(736) = term(736) * (-4.0d+0) 
term(737) = term(737) * (-1.0d+0) 
term(738) = term(738) * (2.0d+0) 
term(739) = term(739) * (-1.0d+0) 
term(740) = term(740) * (2.0d+0) 
term(741) = term(741) * (-1.0d+0) 
term(742) = term(742) * (2.0d+0) 
term(743) = term(743) * (2.0d+0) 
term(744) = term(744) * (-4.0d+0) 
term(745) = term(745) * (8.0d+0) 
term(746) = term(746) * (-8.0d+0) 
term(747) = term(747) * (-16.0d+0) 
term(748) = term(748) * (16.0d+0) 
term(749) = term(749) * (8.0d+0) 
term(750) = term(750) * (-16.0d+0) 
term(751) = term(751) * (-8.0d+0) 
term(752) = term(752) * (16.0d+0) 
term(753) = term(753) * (-4.0d+0) 
term(754) = term(754) * (4.0d+0) 
term(755) = term(755) * (8.0d+0) 
term(756) = term(756) * (-8.0d+0) 
term(757) = term(757) * (-4.0d+0) 
term(758) = term(758) * (8.0d+0) 
term(759) = term(759) * (4.0d+0) 
term(760) = term(760) * (-8.0d+0) 
term(761) = term(761) * (-1.0d+0) 
term(762) = term(762) * (2.0d+0) 
term(763) = term(763) * (-4.0d+0) 
term(764) = term(764) * (4.0d+0) 
term(765) = term(765) * (-4.0d+0) 
term(766) = term(766) * (4.0d+0) 
term(767) = term(767) * (-4.0d+0) 
term(768) = term(768) * (4.0d+0) 
term(769) = term(769) * (-4.0d+0) 
term(770) = term(770) * (8.0d+0) 
term(771) = term(771) * (4.0d+0) 
term(772) = term(772) * (-4.0d+0) 
term(773) = term(773) * (-4.0d+0) 
term(774) = term(774) * (4.0d+0) 
term(775) = term(775) * (8.0d+0) 
term(776) = term(776) * (-8.0d+0) 
term(777) = term(777) * (-4.0d+0) 
term(778) = term(778) * (8.0d+0) 
term(779) = term(779) * (4.0d+0) 
term(780) = term(780) * (-8.0d+0) 
term(781) = term(781) * (8.0d+0) 
term(782) = term(782) * (-8.0d+0) 
term(783) = term(783) * (-16.0d+0) 
term(784) = term(784) * (16.0d+0) 
term(785) = term(785) * (8.0d+0) 
term(786) = term(786) * (-16.0d+0) 
term(787) = term(787) * (-8.0d+0) 
term(788) = term(788) * (16.0d+0) 
term(789) = term(789) * (-4.0d+0) 
term(790) = term(790) * (4.0d+0) 
term(791) = term(791) * (8.0d+0) 
term(792) = term(792) * (-8.0d+0) 
term(793) = term(793) * (-4.0d+0) 
term(794) = term(794) * (8.0d+0) 
term(795) = term(795) * (4.0d+0) 
term(796) = term(796) * (-8.0d+0) 
term(797) = term(797) * (-4.0d+0) 
term(798) = term(798) * (4.0d+0) 
term(799) = term(799) * (-4.0d+0) 
term(800) = term(800) * (4.0d+0) 
term(801) = term(801) * (-4.0d+0) 
term(802) = term(802) * (4.0d+0) 
term(803) = term(803) * (-4.0d+0) 
term(804) = term(804) * (4.0d+0) 
term(805) = term(805) * (-4.0d+0) 
term(806) = term(806) * (4.0d+0) 
term(807) = term(807) * (8.0d+0) 
term(808) = term(808) * (-8.0d+0) 
term(809) = term(809) * (-4.0d+0) 
term(810) = term(810) * (8.0d+0) 
term(811) = term(811) * (4.0d+0) 
term(812) = term(812) * (-8.0d+0) 
term(813) = term(813) * (-1.0d+0) 
term(814) = term(814) * (2.0d+0) 
term(815) = term(815) * (-1.0d+0) 
term(816) = term(816) * (2.0d+0) 
term(817) = term(817) * (-1.0d+0) 
term(818) = term(818) * (2.0d+0) 
term(819) = term(819) * (2.0d+0) 
term(820) = term(820) * (-4.0d+0) 
term(821) = term(821) * (-1.0d+0) 
term(822) = term(822) * (2.0d+0) 
term(823) = term(823) * (-1.0d+0) 
term(824) = term(824) * (2.0d+0) 
term(825) = term(825) * (2.0d+0) 
term(826) = term(826) * (-4.0d+0) 
term(827) = term(827) * (2.0d+0) 
term(828) = term(828) * (-4.0d+0) 
term(829) = term(829) * (-1.0d+0) 
term(830) = term(830) * (-1.0d+0) 
term(831) = term(831) * (-1.0d+0) 
term(832) = term(832) * (2.0d+0) 
term(833) = term(833) * (-1.0d+0) 
term(834) = term(834) * (2.0d+0) 
term(835) = term(835) * (-1.0d+0) 
term(836) = term(836) * (2.0d+0) 
term(837) = term(837) * (-1.0d+0) 
term(838) = term(838) * (-1.0d+0) 
term(839) = term(839) * (2.0d+0) 
term(840) = term(840) * (-1.0d+0) 
term(841) = term(841) * (-1.0d+0) 
term(842) = term(842) * (2.0d+0) 
term(843) = term(843) * (-1.0d+0) 
term(844) = term(844) * (2.0d+0) 
term(845) = term(845) * (2.0d+0) 
term(846) = term(846) * (-4.0d+0) 
term(847) = term(847) * (-1.0d+0) 
term(848) = term(848) * (2.0d+0) 
term(849) = term(849) * (-1.0d+0) 
term(850) = term(850) * (2.0d+0) 
term(851) = term(851) * (-1.0d+0) 
term(852) = term(852) * (2.0d+0) 
term(853) = term(853) * (-1.0d+0) 
term(854) = term(854) * (2.0d+0) 
term(855) = term(855) * (-1.0d+0) 
term(856) = term(856) * (2.0d+0) 
term(857) = term(857) * (2.0d+0) 
term(858) = term(858) * (-4.0d+0) 
term(859) = term(859) * (2.0d+0) 
term(860) = term(860) * (-4.0d+0) 
term(861) = term(861) * (-4.0d+0) 
term(862) = term(862) * (4.0d+0) 
term(863) = term(863) * (-4.0d+0) 
term(864) = term(864) * (4.0d+0) 
term(865) = term(865) * (-4.0d+0) 
term(866) = term(866) * (4.0d+0) 
term(867) = term(867) * (8.0d+0) 
term(868) = term(868) * (-8.0d+0) 
term(869) = term(869) * (-4.0d+0) 
term(870) = term(870) * (8.0d+0) 
term(871) = term(871) * (-4.0d+0) 
term(872) = term(872) * (8.0d+0) 
term(873) = term(873) * (4.0d+0) 
term(874) = term(874) * (-8.0d+0) 
term(875) = term(875) * (4.0d+0) 
term(876) = term(876) * (-8.0d+0) 
term(877) = term(877) * (-4.0d+0) 
term(878) = term(878) * (-4.0d+0) 
term(879) = term(879) * (4.0d+0) 
term(880) = term(880) * (4.0d+0) 
term(881) = term(881) * (-4.0d+0) 
term(882) = term(882) * (4.0d+0) 
term(883) = term(883) * (8.0d+0) 
term(884) = term(884) * (-8.0d+0) 
term(885) = term(885) * (-4.0d+0) 
term(886) = term(886) * (8.0d+0) 
term(887) = term(887) * (-4.0d+0) 
term(888) = term(888) * (8.0d+0) 
term(889) = term(889) * (4.0d+0) 
term(890) = term(890) * (-8.0d+0) 
term(891) = term(891) * (4.0d+0) 
term(892) = term(892) * (-8.0d+0) 
term(893) = term(893) * (-2.0d+0) 
term(894) = term(894) * (-2.0d+0) 
term(895) = term(895) * (4.0d+0) 
term(896) = term(896) * (-2.0d+0) 
term(897) = term(897) * (4.0d+0) 
term(898) = term(898) * (-2.0d+0) 
term(899) = term(899) * (4.0d+0) 
term(900) = term(900) * (-2.0d+0) 
term(901) = term(901) * (-2.0d+0) 
term(902) = term(902) * (-2.0d+0) 
term(903) = term(903) * (4.0d+0) 
term(904) = term(904) * (-2.0d+0) 
term(905) = term(905) * (4.0d+0) 
term(906) = term(906) * (4.0d+0) 
term(907) = term(907) * (-8.0d+0) 
term(908) = term(908) * (-2.0d+0) 
term(909) = term(909) * (-2.0d+0) 
term(910) = term(910) * (4.0d+0) 
term(911) = term(911) * (4.0d+0) 
term(912) = term(912) * (4.0d+0) 
term(913) = term(913) * (-8.0d+0) 
term(914) = term(914) * (-2.0d+0) 
term(915) = term(915) * (-2.0d+0) 
term(916) = term(916) * (4.0d+0) 
term(917) = term(917) * (-2.0d+0) 
term(918) = term(918) * (4.0d+0) 
term(919) = term(919) * (-2.0d+0) 
term(920) = term(920) * (-2.0d+0) 
term(921) = term(921) * (4.0d+0) 
term(922) = term(922) * (-2.0d+0) 
term(923) = term(923) * (-8.0d+0) 
term(924) = term(924) * (8.0d+0) 
term(925) = term(925) * (-8.0d+0) 
term(926) = term(926) * (8.0d+0) 
term(927) = term(927) * (-8.0d+0) 
term(928) = term(928) * (8.0d+0) 
term(929) = term(929) * (-8.0d+0) 
term(930) = term(930) * (16.0d+0) 
term(931) = term(931) * (8.0d+0) 
term(932) = term(932) * (-16.0d+0) 
term(933) = term(933) * (-8.0d+0) 
term(934) = term(934) * (8.0d+0) 
term(935) = term(935) * (16.0d+0) 
term(936) = term(936) * (-16.0d+0) 
term(937) = term(937) * (-8.0d+0) 
term(938) = term(938) * (8.0d+0) 
term(939) = term(939) * (-8.0d+0) 
term(940) = term(940) * (8.0d+0) 
term(941) = term(941) * (-8.0d+0) 
term(942) = term(942) * (8.0d+0) 

do a = nocc + 1, nactive 
do i = 1, nocc 
term(943) = term(943) + t1(a,i) * wm_interm_62_so_pt4(i,q) * wm_interm_81_so_pt4(p,a)
term(944) = term(944) + t1(a,i) * wm_interm_62_so_pt4(i,q) * wm_interm_82_so_pt4(p,a)
term(945) = term(945) + t1(a,i) * wm_interm_60_so_pt4(i,q) * wm_interm_81_so_pt4(p,a)
term(946) = term(946) + t1(a,i) * wm_interm_60_so_pt4(i,q) * wm_interm_82_so_pt4(p,a)
term(947) = term(947) + t1(a,i) * wm_interm_62_so_pt4(i,q) * wm_interm_83_so_pt4(p,a)
term(948) = term(948) + t1(a,i) * wm_interm_60_so_pt4(i,q) * wm_interm_83_so_pt4(p,a)
term(949) = term(949) + t1(a,i) * wm_interm_104_so_pt4(p,a) * wm_interm_62_so_pt4(i,q)
term(950) = term(950) + t1(a,i) * wm_interm_105_so_pt4(p,a) * wm_interm_62_so_pt4(i,q)
term(951) = term(951) + t1(a,i) * wm_interm_104_so_pt4(p,a) * wm_interm_60_so_pt4(i,q)
term(952) = term(952) + t1(a,i) * wm_interm_105_so_pt4(p,a) * wm_interm_60_so_pt4(i,q)
term(953) = term(953) + t1(a,i) * wm_interm_112_so_pt4(p,a) * wm_interm_62_so_pt4(i,q)
term(954) = term(954) + t1(a,i) * wm_interm_112_so_pt4(p,a) * wm_interm_60_so_pt4(i,q)
end do 
end do 

term(943) = term(943) * (2.0d+0) 
term(944) = term(944) * (-4.0d+0) 
term(945) = term(945) * (-1.0d+0) 
term(946) = term(946) * (2.0d+0) 
term(947) = term(947) * (2.0d+0) 
term(948) = term(948) * (-1.0d+0) 
term(949) = term(949) * (4.0d+0) 
term(950) = term(950) * (-8.0d+0) 
term(951) = term(951) * (-2.0d+0) 
term(952) = term(952) * (4.0d+0) 
term(953) = term(953) * (4.0d+0) 
term(954) = term(954) * (-2.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do l = 1, nocc 
do a = nocc + 1, nactive 
term(955) = term(955) + t2(a,p,j,i) * wm_interm_109_so_pt4(k,l,j,i) * wm_interm_47_so_pt4(a,l,k,q)
end do 
end do 
end do 
end do 
end do 

term(955) = term(955) * (-2.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(956) = term(956) + t1(a,i) * wm_interm_55_so_pt4(a,p,j,k) * wm_interm_88_so_pt4(j,q,k,i)
term(957) = term(957) + t1(a,i) * wm_interm_55_so_pt4(a,p,j,k) * wm_interm_88_so_pt4(q,j,k,i)
term(958) = term(958) + t1(a,i) * wm_interm_56_so_pt4(a,p,j,k) * wm_interm_88_so_pt4(q,j,k,i)
term(959) = term(959) + t1(a,i) * wm_interm_59_so_pt4(a,p,j,k) * wm_interm_88_so_pt4(j,q,k,i)
term(960) = term(960) + t1(a,i) * wm_interm_54_so_pt4(a,p,j,k) * wm_interm_88_so_pt4(j,q,k,i)
term(961) = term(961) + t1(a,i) * wm_interm_59_so_pt4(a,p,j,k) * wm_interm_88_so_pt4(q,j,k,i)
term(962) = term(962) + t1(a,i) * wm_interm_54_so_pt4(a,p,j,k) * wm_interm_88_so_pt4(q,j,k,i)
term(963) = term(963) + t1(a,i) * wm_interm_109_so_pt4(j,q,k,i) * wm_interm_55_so_pt4(a,p,j,k)
term(964) = term(964) + t1(a,i) * wm_interm_109_so_pt4(q,j,k,i) * wm_interm_55_so_pt4(a,p,j,k)
term(965) = term(965) + t1(a,i) * wm_interm_109_so_pt4(q,j,k,i) * wm_interm_56_so_pt4(a,p,j,k)
term(966) = term(966) + t1(a,i) * wm_interm_109_so_pt4(j,q,k,i) * wm_interm_59_so_pt4(a,p,j,k)
term(967) = term(967) + t1(a,i) * wm_interm_109_so_pt4(j,q,k,i) * wm_interm_54_so_pt4(a,p,j,k)
term(968) = term(968) + t1(a,i) * wm_interm_109_so_pt4(q,j,k,i) * wm_interm_59_so_pt4(a,p,j,k)
term(969) = term(969) + t1(a,i) * wm_interm_109_so_pt4(q,j,k,i) * wm_interm_54_so_pt4(a,p,j,k)
end do 
end do 
end do 
end do 

term(956) = term(956) * (-1.0d+0) 
term(957) = term(957) * (2.0d+0) 
term(958) = term(958) * (-1.0d+0) 
term(959) = term(959) * (-1.0d+0) 
term(960) = term(960) * (2.0d+0) 
term(961) = term(961) * (2.0d+0) 
term(962) = term(962) * (-4.0d+0) 
term(963) = term(963) * (-2.0d+0) 
term(964) = term(964) * (4.0d+0) 
term(965) = term(965) * (-2.0d+0) 
term(966) = term(966) * (-2.0d+0) 
term(967) = term(967) * (4.0d+0) 
term(968) = term(968) * (4.0d+0) 
term(969) = term(969) * (-8.0d+0) 

do j = 1, nocc 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(970) = term(970) + t1(a,i) * wm_interm_62_so_pt4(i,j) * wm_interm_70_so_pt4(p,a,q,j)
term(971) = term(971) + t1(a,i) * wm_interm_62_so_pt4(i,j) * wm_interm_69_so_pt4(p,a,q,j)
term(972) = term(972) + t1(a,i) * wm_interm_62_so_pt4(i,j) * wm_interm_67_so_pt4(p,a,q,j)
term(973) = term(973) + t1(a,i) * wm_interm_62_so_pt4(i,j) * wm_interm_68_so_pt4(p,a,q,j)
term(974) = term(974) + t1(a,i) * wm_interm_62_so_pt4(i,j) * wm_interm_84_so_pt4(p,a,q,j)
term(975) = term(975) + t1(a,i) * wm_interm_62_so_pt4(i,j) * wm_interm_74_so_pt4(p,a,q,j)
term(976) = term(976) + t1(a,i) * wm_interm_60_so_pt4(i,j) * wm_interm_70_so_pt4(p,a,q,j)
term(977) = term(977) + t1(a,i) * wm_interm_60_so_pt4(i,j) * wm_interm_69_so_pt4(p,a,q,j)
term(978) = term(978) + t1(a,i) * wm_interm_60_so_pt4(i,j) * wm_interm_67_so_pt4(p,a,q,j)
term(979) = term(979) + t1(a,i) * wm_interm_60_so_pt4(i,j) * wm_interm_68_so_pt4(p,a,q,j)
term(980) = term(980) + t1(a,i) * wm_interm_60_so_pt4(i,j) * wm_interm_84_so_pt4(p,a,q,j)
term(981) = term(981) + t1(a,i) * wm_interm_60_so_pt4(i,j) * wm_interm_74_so_pt4(p,a,q,j)
term(982) = term(982) + t1(a,i) * wm_interm_62_so_pt4(i,j) * wm_interm_98_so_pt4(p,a,q,j)
term(983) = term(983) + t1(a,i) * wm_interm_62_so_pt4(i,j) * wm_interm_97_so_pt4(p,a,q,j)
term(984) = term(984) + t1(a,i) * wm_interm_62_so_pt4(i,j) * wm_interm_95_so_pt4(p,a,q,j)
term(985) = term(985) + t1(a,i) * wm_interm_62_so_pt4(i,j) * wm_interm_96_so_pt4(p,a,q,j)
term(986) = term(986) + t1(a,i) * wm_interm_60_so_pt4(i,j) * wm_interm_98_so_pt4(p,a,q,j)
term(987) = term(987) + t1(a,i) * wm_interm_60_so_pt4(i,j) * wm_interm_97_so_pt4(p,a,q,j)
term(988) = term(988) + t1(a,i) * wm_interm_60_so_pt4(i,j) * wm_interm_95_so_pt4(p,a,q,j)
term(989) = term(989) + t1(a,i) * wm_interm_60_so_pt4(i,j) * wm_interm_96_so_pt4(p,a,q,j)
term(990) = term(990) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_17_so_pt4(i,q) * wm_interm_50_so_pt4(a,j)
term(991) = term(991) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_18_so_pt4(i,q) * wm_interm_50_so_pt4(a,j)
term(992) = term(992) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_17_so_pt4(i,q) * wm_interm_51_so_pt4(a,j)
term(993) = term(993) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_18_so_pt4(i,q) * wm_interm_51_so_pt4(a,j)
end do 
end do 
end do 

term(970) = term(970) * (4.0d+0) 
term(971) = term(971) * (-8.0d+0) 
term(972) = term(972) * (-8.0d+0) 
term(973) = term(973) * (16.0d+0) 
term(974) = term(974) * (4.0d+0) 
term(975) = term(975) * (-8.0d+0) 
term(976) = term(976) * (-2.0d+0) 
term(977) = term(977) * (4.0d+0) 
term(978) = term(978) * (4.0d+0) 
term(979) = term(979) * (-8.0d+0) 
term(980) = term(980) * (-2.0d+0) 
term(981) = term(981) * (4.0d+0) 
term(982) = term(982) * (16.0d+0) 
term(983) = term(983) * (-16.0d+0) 
term(984) = term(984) * (-32.0d+0) 
term(985) = term(985) * (32.0d+0) 
term(986) = term(986) * (-8.0d+0) 
term(987) = term(987) * (8.0d+0) 
term(988) = term(988) * (16.0d+0) 
term(989) = term(989) * (-16.0d+0) 
term(990) = term(990) * (-4.0d+0) 
term(991) = term(991) * (8.0d+0) 
term(992) = term(992) * (2.0d+0) 
term(993) = term(993) * (-4.0d+0) 

do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(994) = term(994) + t1(b,q) * wm_interm_58_so_pt4(a,p) * wm_interm_83_so_pt4(a,b)
term(995) = term(995) + t1(b,q) * wm_interm_57_so_pt4(a,p) * wm_interm_83_so_pt4(a,b)
term(996) = term(996) + t1(b,q) * wm_interm_58_so_pt4(a,p) * wm_interm_81_so_pt4(a,b)
term(997) = term(997) + t1(b,q) * wm_interm_57_so_pt4(a,p) * wm_interm_81_so_pt4(a,b)
term(998) = term(998) + t1(b,q) * wm_interm_58_so_pt4(a,p) * wm_interm_82_so_pt4(a,b)
term(999) = term(999) + t1(b,q) * wm_interm_57_so_pt4(a,p) * wm_interm_82_so_pt4(a,b)
term(1000) = term(1000) + t1(b,q) * wm_interm_104_so_pt4(a,b) * wm_interm_58_so_pt4(a,p)
term(1001) = term(1001) + t1(b,q) * wm_interm_104_so_pt4(a,b) * wm_interm_57_so_pt4(a,p)
term(1002) = term(1002) + t1(b,q) * wm_interm_105_so_pt4(a,b) * wm_interm_58_so_pt4(a,p)
term(1003) = term(1003) + t1(b,q) * wm_interm_105_so_pt4(a,b) * wm_interm_57_so_pt4(a,p)
end do 
end do 

term(994) = term(994) * (-2.0d+0) 
term(995) = term(995) * (4.0d+0) 
term(996) = term(996) * (-2.0d+0) 
term(997) = term(997) * (4.0d+0) 
term(998) = term(998) * (4.0d+0) 
term(999) = term(999) * (-8.0d+0) 
term(1000) = term(1000) * (-8.0d+0) 
term(1001) = term(1001) * (16.0d+0) 
term(1002) = term(1002) * (8.0d+0) 
term(1003) = term(1003) * (-16.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(1004) = term(1004) + t1(p,i) * wm_interm_61_so_pt4(i,j,k,q) * wm_interm_85_so_pt4(j,k)
term(1005) = term(1005) + t1(p,i) * wm_interm_61_so_pt4(i,j,k,q) * wm_interm_86_so_pt4(j,k)
term(1006) = term(1006) + t1(p,i) * wm_interm_61_so_pt4(i,j,k,q) * wm_interm_87_so_pt4(j,k)
term(1007) = term(1007) + t1(p,i) * wm_interm_106_so_pt4(j,k) * wm_interm_61_so_pt4(i,j,k,q)
term(1008) = term(1008) + t1(p,i) * wm_interm_107_so_pt4(j,k) * wm_interm_61_so_pt4(i,j,k,q)
term(1009) = term(1009) + t1(p,i) * wm_interm_108_so_pt4(j,k) * wm_interm_61_so_pt4(i,j,k,q)
end do 
end do 
end do 

term(1004) = term(1004) * (-1.0d+0) 
term(1005) = term(1005) * (2.0d+0) 
term(1006) = term(1006) * (-1.0d+0) 
term(1007) = term(1007) * (-2.0d+0) 
term(1008) = term(1008) * (4.0d+0) 
term(1009) = term(1009) * (-2.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(1010) = term(1010) + t1(a,i) * wm_interm_12_so_pt4(a,q,j,k) * wm_interm_13_so_pt4(p,i,j,k)
term(1011) = term(1011) + t1(a,i) * wm_interm_12_so_pt4(a,q,j,k) * wm_interm_13_so_pt4(p,i,k,j)
term(1012) = term(1012) + t1(a,q) * wm_interm_12_so_pt4(a,i,j,k) * wm_interm_13_so_pt4(p,i,k,j)
term(1013) = term(1013) + t1(a,i) * wm_interm_65_so_pt4(a,p,j,k) * wm_interm_9_so_pt4(k,i,j,q)
term(1014) = term(1014) + t1(a,i) * wm_interm_66_so_pt4(a,p,j,k) * wm_interm_9_so_pt4(k,i,j,q)
term(1015) = term(1015) + t1(a,i) * wm_interm_66_so_pt4(a,p,j,k) * wm_interm_9_so_pt4(i,k,j,q)
term(1016) = term(1016) + t1(a,i) * wm_interm_65_so_pt4(a,p,j,k) * wm_interm_9_so_pt4(i,k,j,q)
term(1017) = term(1017) + t1(a,i) * wm_interm_77_so_pt4(a,p,j,k) * wm_interm_9_so_pt4(i,k,j,q)
term(1018) = term(1018) + t1(a,i) * wm_interm_77_so_pt4(a,p,j,k) * wm_interm_9_so_pt4(k,i,j,q)
term(1019) = term(1019) + t1(a,i) * wm_interm_79_so_pt4(a,p,j,k) * wm_interm_9_so_pt4(i,k,j,q)
term(1020) = term(1020) + t1(a,i) * wm_interm_79_so_pt4(a,p,j,k) * wm_interm_9_so_pt4(k,i,j,q)
term(1021) = term(1021) + t1(a,i) * wm_interm_63_so_pt4(a,p,j,k) * wm_interm_9_so_pt4(i,k,j,q)
term(1022) = term(1022) + t1(a,i) * wm_interm_64_so_pt4(a,p,j,k) * wm_interm_9_so_pt4(k,i,j,q)
term(1023) = term(1023) + t1(a,i) * wm_interm_75_so_pt4(a,p,j,k) * wm_interm_9_so_pt4(i,k,j,q)
term(1024) = term(1024) + t1(a,i) * wm_interm_75_so_pt4(a,p,j,k) * wm_interm_9_so_pt4(k,i,j,q)
term(1025) = term(1025) + t1(a,i) * wm_interm_5_so_pt4(a,p,j,k) * wm_interm_71_so_pt4(k,i,q,j)
term(1026) = term(1026) + t1(a,i) * wm_interm_1_so_pt4(a,p,j,k) * wm_interm_71_so_pt4(k,i,j,q)
term(1027) = term(1027) + t1(a,i) * wm_interm_1_so_pt4(a,p,j,k) * wm_interm_71_so_pt4(k,i,q,j)
term(1028) = term(1028) + t1(a,i) * wm_interm_5_so_pt4(a,p,j,k) * wm_interm_72_so_pt4(k,i,j,q)
term(1029) = term(1029) + t1(a,i) * wm_interm_5_so_pt4(a,p,j,k) * wm_interm_72_so_pt4(k,i,q,j)
term(1030) = term(1030) + t1(a,i) * wm_interm_1_so_pt4(a,p,j,k) * wm_interm_72_so_pt4(k,i,q,j)
term(1031) = term(1031) + t1(a,i) * wm_interm_10_so_pt4(a,p,j,k) * wm_interm_71_so_pt4(k,i,j,q)
term(1032) = term(1032) + t1(a,i) * wm_interm_10_so_pt4(a,p,j,k) * wm_interm_71_so_pt4(k,i,q,j)
term(1033) = term(1033) + t1(a,i) * wm_interm_11_so_pt4(a,p,j,k) * wm_interm_71_so_pt4(k,i,j,q)
term(1034) = term(1034) + t1(a,i) * wm_interm_11_so_pt4(a,p,j,k) * wm_interm_71_so_pt4(k,i,q,j)
term(1035) = term(1035) + t1(a,i) * wm_interm_10_so_pt4(a,p,j,k) * wm_interm_72_so_pt4(k,i,q,j)
term(1036) = term(1036) + t1(a,i) * wm_interm_11_so_pt4(a,p,j,k) * wm_interm_72_so_pt4(k,i,q,j)
term(1037) = term(1037) + t1(a,i) * wm_interm_55_so_pt4(a,p,j,k) * wm_interm_88_so_pt4(q,j,i,k)
term(1038) = term(1038) + t1(a,i) * wm_interm_56_so_pt4(a,p,j,k) * wm_interm_88_so_pt4(j,q,i,k)
term(1039) = term(1039) + t1(a,i) * wm_interm_56_so_pt4(a,p,j,k) * wm_interm_88_so_pt4(q,j,i,k)
term(1040) = term(1040) + t1(a,i) * wm_interm_59_so_pt4(a,p,j,k) * wm_interm_88_so_pt4(q,j,i,k)
term(1041) = term(1041) + t1(a,i) * wm_interm_54_so_pt4(a,p,j,k) * wm_interm_88_so_pt4(q,j,i,k)
term(1042) = term(1042) + t1(a,i) * wm_interm_10_so_pt4(a,p,j,k) * wm_interm_71_so_pt4(i,k,q,j)
term(1043) = term(1043) + t1(a,i) * wm_interm_10_so_pt4(a,p,j,k) * wm_interm_72_so_pt4(i,k,j,q)
term(1044) = term(1044) + t1(a,i) * wm_interm_10_so_pt4(a,p,j,k) * wm_interm_72_so_pt4(i,k,q,j)
term(1045) = term(1045) + t1(a,i) * wm_interm_5_so_pt4(a,p,j,k) * wm_interm_71_so_pt4(i,k,j,q)
term(1046) = term(1046) + t1(a,i) * wm_interm_5_so_pt4(a,p,j,k) * wm_interm_71_so_pt4(i,k,q,j)
term(1047) = term(1047) + t1(a,i) * wm_interm_5_so_pt4(a,p,j,k) * wm_interm_72_so_pt4(i,k,q,j)
term(1048) = term(1048) + t1(a,i) * wm_interm_1_so_pt4(a,p,j,k) * wm_interm_71_so_pt4(i,k,q,j)
term(1049) = term(1049) + t1(a,i) * wm_interm_11_so_pt4(a,p,j,k) * wm_interm_71_so_pt4(i,k,q,j)
term(1050) = term(1050) + t1(a,i) * wm_interm_1_so_pt4(a,p,j,k) * wm_interm_72_so_pt4(i,k,j,q)
term(1051) = term(1051) + t1(a,i) * wm_interm_11_so_pt4(a,p,j,k) * wm_interm_72_so_pt4(i,k,j,q)
term(1052) = term(1052) + t1(a,i) * wm_interm_1_so_pt4(a,p,j,k) * wm_interm_72_so_pt4(i,k,q,j)
term(1053) = term(1053) + t1(a,i) * wm_interm_11_so_pt4(a,p,j,k) * wm_interm_72_so_pt4(i,k,q,j)
term(1054) = term(1054) + t1(a,i) * wm_interm_94_so_pt4(a,p,j,k) * wm_interm_9_so_pt4(k,i,j,q)
term(1055) = term(1055) + t1(a,i) * wm_interm_93_so_pt4(a,p,j,k) * wm_interm_9_so_pt4(k,i,j,q)
term(1056) = term(1056) + t1(a,i) * wm_interm_93_so_pt4(a,p,j,k) * wm_interm_9_so_pt4(i,k,j,q)
term(1057) = term(1057) + t1(a,i) * wm_interm_94_so_pt4(a,p,j,k) * wm_interm_9_so_pt4(i,k,j,q)
term(1058) = term(1058) + t1(a,i) * wm_interm_100_so_pt4(a,p,j,k) * wm_interm_9_so_pt4(i,k,j,q)
term(1059) = term(1059) + t1(a,i) * wm_interm_100_so_pt4(a,p,j,k) * wm_interm_9_so_pt4(k,i,j,q)
term(1060) = term(1060) + t1(a,i) * wm_interm_102_so_pt4(a,p,j,k) * wm_interm_9_so_pt4(i,k,j,q)
term(1061) = term(1061) + t1(a,i) * wm_interm_102_so_pt4(a,p,j,k) * wm_interm_9_so_pt4(k,i,j,q)
term(1062) = term(1062) + t1(a,i) * wm_interm_5_so_pt4(a,p,j,k) * wm_interm_99_so_pt4(k,i,q,j)
term(1063) = term(1063) + t1(a,i) * wm_interm_1_so_pt4(a,p,j,k) * wm_interm_99_so_pt4(k,i,j,q)
term(1064) = term(1064) + t1(a,i) * wm_interm_1_so_pt4(a,p,j,k) * wm_interm_99_so_pt4(k,i,q,j)
term(1065) = term(1065) + t1(a,i) * wm_interm_5_so_pt4(a,p,j,k) * wm_interm_99_so_pt4(k,i,j,q)
term(1066) = term(1066) + t1(a,i) * wm_interm_10_so_pt4(a,p,j,k) * wm_interm_99_so_pt4(k,i,j,q)
term(1067) = term(1067) + t1(a,i) * wm_interm_10_so_pt4(a,p,j,k) * wm_interm_99_so_pt4(k,i,q,j)
term(1068) = term(1068) + t1(a,i) * wm_interm_11_so_pt4(a,p,j,k) * wm_interm_99_so_pt4(k,i,j,q)
term(1069) = term(1069) + t1(a,i) * wm_interm_11_so_pt4(a,p,j,k) * wm_interm_99_so_pt4(k,i,q,j)
term(1070) = term(1070) + t1(a,i) * wm_interm_109_so_pt4(q,j,i,k) * wm_interm_55_so_pt4(a,p,j,k)
term(1071) = term(1071) + t1(a,i) * wm_interm_109_so_pt4(j,q,i,k) * wm_interm_56_so_pt4(a,p,j,k)
term(1072) = term(1072) + t1(a,i) * wm_interm_109_so_pt4(q,j,i,k) * wm_interm_56_so_pt4(a,p,j,k)
term(1073) = term(1073) + t1(a,i) * wm_interm_109_so_pt4(q,j,i,k) * wm_interm_59_so_pt4(a,p,j,k)
term(1074) = term(1074) + t1(a,i) * wm_interm_109_so_pt4(q,j,i,k) * wm_interm_54_so_pt4(a,p,j,k)
term(1075) = term(1075) + t1(a,i) * wm_interm_10_so_pt4(a,p,j,k) * wm_interm_99_so_pt4(i,k,q,j)
term(1076) = term(1076) + t1(a,i) * wm_interm_10_so_pt4(a,p,j,k) * wm_interm_99_so_pt4(i,k,j,q)
term(1077) = term(1077) + t1(a,i) * wm_interm_5_so_pt4(a,p,j,k) * wm_interm_99_so_pt4(i,k,j,q)
term(1078) = term(1078) + t1(a,i) * wm_interm_5_so_pt4(a,p,j,k) * wm_interm_99_so_pt4(i,k,q,j)
term(1079) = term(1079) + t1(a,i) * wm_interm_1_so_pt4(a,p,j,k) * wm_interm_99_so_pt4(i,k,q,j)
term(1080) = term(1080) + t1(a,i) * wm_interm_11_so_pt4(a,p,j,k) * wm_interm_99_so_pt4(i,k,q,j)
term(1081) = term(1081) + t1(a,i) * wm_interm_1_so_pt4(a,p,j,k) * wm_interm_99_so_pt4(i,k,j,q)
term(1082) = term(1082) + t1(a,i) * wm_interm_11_so_pt4(a,p,j,k) * wm_interm_99_so_pt4(i,k,j,q)
end do 
end do 
end do 
end do 

term(1010) = term(1010) * (2.0d+0) 
term(1011) = term(1011) * (-4.0d+0) 
term(1012) = term(1012) * (4.0d+0) 
term(1013) = term(1013) * (-1.0d+0) 
term(1014) = term(1014) * (2.0d+0) 
term(1015) = term(1015) * (-1.0d+0) 
term(1016) = term(1016) * (2.0d+0) 
term(1017) = term(1017) * (2.0d+0) 
term(1018) = term(1018) * (-1.0d+0) 
term(1019) = term(1019) * (-4.0d+0) 
term(1020) = term(1020) * (2.0d+0) 
term(1021) = term(1021) * (-1.0d+0) 
term(1022) = term(1022) * (-1.0d+0) 
term(1023) = term(1023) * (2.0d+0) 
term(1024) = term(1024) * (-1.0d+0) 
term(1025) = term(1025) * (-0.5d+0) 
term(1026) = term(1026) * (-0.5d+0) 
term(1028) = term(1028) * (-0.5d+0) 
term(1030) = term(1030) * (-0.5d+0) 
term(1031) = term(1031) * (-0.5d+0) 
term(1034) = term(1034) * (-2.0d+0) 
term(1035) = term(1035) * (-0.5d+0) 
term(1037) = term(1037) * (-1.0d+0) 
term(1038) = term(1038) * (-1.0d+0) 
term(1039) = term(1039) * (2.0d+0) 
term(1040) = term(1040) * (-1.0d+0) 
term(1041) = term(1041) * (2.0d+0) 
term(1042) = term(1042) * (-0.5d+0) 
term(1043) = term(1043) * (-0.5d+0) 
term(1045) = term(1045) * (-0.5d+0) 
term(1047) = term(1047) * (-0.5d+0) 
term(1048) = term(1048) * (-0.5d+0) 
term(1050) = term(1050) * (-0.5d+0) 
term(1053) = term(1053) * (-2.0d+0) 
term(1054) = term(1054) * (-4.0d+0) 
term(1055) = term(1055) * (4.0d+0) 
term(1056) = term(1056) * (-4.0d+0) 
term(1057) = term(1057) * (4.0d+0) 
term(1058) = term(1058) * (8.0d+0) 
term(1059) = term(1059) * (-4.0d+0) 
term(1060) = term(1060) * (-8.0d+0) 
term(1061) = term(1061) * (4.0d+0) 
term(1062) = term(1062) * (-2.0d+0) 
term(1063) = term(1063) * (-2.0d+0) 
term(1064) = term(1064) * (2.0d+0) 
term(1065) = term(1065) * (2.0d+0) 
term(1066) = term(1066) * (-2.0d+0) 
term(1067) = term(1067) * (2.0d+0) 
term(1068) = term(1068) * (4.0d+0) 
term(1069) = term(1069) * (-4.0d+0) 
term(1070) = term(1070) * (-2.0d+0) 
term(1071) = term(1071) * (-2.0d+0) 
term(1072) = term(1072) * (4.0d+0) 
term(1073) = term(1073) * (-2.0d+0) 
term(1074) = term(1074) * (4.0d+0) 
term(1075) = term(1075) * (-2.0d+0) 
term(1076) = term(1076) * (2.0d+0) 
term(1077) = term(1077) * (-2.0d+0) 
term(1078) = term(1078) * (2.0d+0) 
term(1079) = term(1079) * (-2.0d+0) 
term(1080) = term(1080) * (4.0d+0) 
term(1081) = term(1081) * (2.0d+0) 
term(1082) = term(1082) * (-4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(1083) = term(1083) + t1(a,i) * wm_interm_61_so_pt4(j,i,k,q) * wm_interm_67_so_pt4(p,a,j,k)
term(1084) = term(1084) + t1(a,i) * wm_interm_61_so_pt4(j,i,k,q) * wm_interm_68_so_pt4(p,a,j,k)
term(1085) = term(1085) + t1(a,i) * wm_interm_61_so_pt4(j,i,k,q) * wm_interm_69_so_pt4(p,a,j,k)
term(1086) = term(1086) + t1(a,i) * wm_interm_61_so_pt4(j,i,k,q) * wm_interm_70_so_pt4(p,a,j,k)
term(1087) = term(1087) + t1(a,i) * wm_interm_61_so_pt4(j,i,q,k) * wm_interm_70_so_pt4(p,a,j,k)
term(1088) = term(1088) + t1(a,i) * wm_interm_61_so_pt4(j,i,q,k) * wm_interm_69_so_pt4(p,a,j,k)
term(1089) = term(1089) + t1(a,i) * wm_interm_61_so_pt4(j,i,q,k) * wm_interm_67_so_pt4(p,a,j,k)
term(1090) = term(1090) + t1(a,i) * wm_interm_61_so_pt4(j,i,q,k) * wm_interm_68_so_pt4(p,a,j,k)
term(1091) = term(1091) + t1(a,i) * wm_interm_61_so_pt4(j,i,k,q) * wm_interm_74_so_pt4(p,a,j,k)
term(1092) = term(1092) + t1(a,i) * wm_interm_61_so_pt4(j,i,k,q) * wm_interm_73_so_pt4(p,a,j,k)
term(1093) = term(1093) + t1(a,i) * wm_interm_61_so_pt4(j,i,q,k) * wm_interm_84_so_pt4(p,a,j,k)
term(1094) = term(1094) + t1(a,i) * wm_interm_61_so_pt4(j,i,q,k) * wm_interm_74_so_pt4(p,a,j,k)
term(1095) = term(1095) + t1(a,i) * wm_interm_61_so_pt4(i,j,k,q) * wm_interm_70_so_pt4(p,a,j,k)
term(1096) = term(1096) + t1(a,i) * wm_interm_61_so_pt4(i,j,k,q) * wm_interm_69_so_pt4(p,a,j,k)
term(1097) = term(1097) + t1(a,i) * wm_interm_61_so_pt4(i,j,k,q) * wm_interm_67_so_pt4(p,a,j,k)
term(1098) = term(1098) + t1(a,i) * wm_interm_61_so_pt4(i,j,k,q) * wm_interm_68_so_pt4(p,a,j,k)
term(1099) = term(1099) + t1(a,i) * wm_interm_61_so_pt4(i,j,q,k) * wm_interm_67_so_pt4(p,a,j,k)
term(1100) = term(1100) + t1(a,i) * wm_interm_61_so_pt4(i,j,q,k) * wm_interm_68_so_pt4(p,a,j,k)
term(1101) = term(1101) + t1(a,i) * wm_interm_61_so_pt4(i,j,q,k) * wm_interm_69_so_pt4(p,a,j,k)
term(1102) = term(1102) + t1(a,i) * wm_interm_61_so_pt4(i,j,q,k) * wm_interm_70_so_pt4(p,a,j,k)
term(1103) = term(1103) + t1(a,i) * wm_interm_61_so_pt4(i,j,k,q) * wm_interm_84_so_pt4(p,a,j,k)
term(1104) = term(1104) + t1(a,i) * wm_interm_61_so_pt4(i,j,k,q) * wm_interm_74_so_pt4(p,a,j,k)
term(1105) = term(1105) + t1(a,i) * wm_interm_61_so_pt4(i,j,q,k) * wm_interm_74_so_pt4(p,a,j,k)
term(1106) = term(1106) + t1(a,i) * wm_interm_61_so_pt4(i,j,q,k) * wm_interm_73_so_pt4(p,a,j,k)
term(1107) = term(1107) + t1(a,i) * wm_interm_61_so_pt4(j,i,k,q) * wm_interm_95_so_pt4(p,a,j,k)
term(1108) = term(1108) + t1(a,i) * wm_interm_61_so_pt4(j,i,k,q) * wm_interm_96_so_pt4(p,a,j,k)
term(1109) = term(1109) + t1(a,i) * wm_interm_61_so_pt4(j,i,k,q) * wm_interm_97_so_pt4(p,a,j,k)
term(1110) = term(1110) + t1(a,i) * wm_interm_61_so_pt4(j,i,k,q) * wm_interm_98_so_pt4(p,a,j,k)
term(1111) = term(1111) + t1(a,i) * wm_interm_61_so_pt4(j,i,q,k) * wm_interm_98_so_pt4(p,a,j,k)
term(1112) = term(1112) + t1(a,i) * wm_interm_61_so_pt4(j,i,q,k) * wm_interm_97_so_pt4(p,a,j,k)
term(1113) = term(1113) + t1(a,i) * wm_interm_61_so_pt4(j,i,q,k) * wm_interm_95_so_pt4(p,a,j,k)
term(1114) = term(1114) + t1(a,i) * wm_interm_61_so_pt4(j,i,q,k) * wm_interm_96_so_pt4(p,a,j,k)
term(1115) = term(1115) + t1(a,i) * wm_interm_61_so_pt4(i,j,k,q) * wm_interm_98_so_pt4(p,a,j,k)
term(1116) = term(1116) + t1(a,i) * wm_interm_61_so_pt4(i,j,k,q) * wm_interm_97_so_pt4(p,a,j,k)
term(1117) = term(1117) + t1(a,i) * wm_interm_61_so_pt4(i,j,k,q) * wm_interm_95_so_pt4(p,a,j,k)
term(1118) = term(1118) + t1(a,i) * wm_interm_61_so_pt4(i,j,k,q) * wm_interm_96_so_pt4(p,a,j,k)
term(1119) = term(1119) + t1(a,i) * wm_interm_61_so_pt4(i,j,q,k) * wm_interm_95_so_pt4(p,a,j,k)
term(1120) = term(1120) + t1(a,i) * wm_interm_61_so_pt4(i,j,q,k) * wm_interm_96_so_pt4(p,a,j,k)
term(1121) = term(1121) + t1(a,i) * wm_interm_61_so_pt4(i,j,q,k) * wm_interm_97_so_pt4(p,a,j,k)
term(1122) = term(1122) + t1(a,i) * wm_interm_61_so_pt4(i,j,q,k) * wm_interm_98_so_pt4(p,a,j,k)
end do 
end do 
end do 
end do 

term(1083) = term(1083) * (-0.5d+0) 
term(1085) = term(1085) * (-0.5d+0) 
term(1087) = term(1087) * (-0.5d+0) 
term(1090) = term(1090) * (-2.0d+0) 
term(1091) = term(1091) * (-0.5d+0) 
term(1092) = term(1092) * (-0.5d+0) 
term(1093) = term(1093) * (-0.5d+0) 
term(1095) = term(1095) * (-0.5d+0) 
term(1098) = term(1098) * (-2.0d+0) 
term(1099) = term(1099) * (-0.5d+0) 
term(1101) = term(1101) * (-0.5d+0) 
term(1103) = term(1103) * (-0.5d+0) 
term(1105) = term(1105) * (-0.5d+0) 
term(1106) = term(1106) * (-0.5d+0) 
term(1107) = term(1107) * (-2.0d+0) 
term(1108) = term(1108) * (2.0d+0) 
term(1109) = term(1109) * (-2.0d+0) 
term(1110) = term(1110) * (2.0d+0) 
term(1111) = term(1111) * (-2.0d+0) 
term(1112) = term(1112) * (2.0d+0) 
term(1113) = term(1113) * (4.0d+0) 
term(1114) = term(1114) * (-4.0d+0) 
term(1115) = term(1115) * (-2.0d+0) 
term(1116) = term(1116) * (2.0d+0) 
term(1117) = term(1117) * (4.0d+0) 
term(1118) = term(1118) * (-4.0d+0) 
term(1119) = term(1119) * (-2.0d+0) 
term(1120) = term(1120) * (2.0d+0) 
term(1121) = term(1121) * (-2.0d+0) 
term(1122) = term(1122) * (2.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(1123) = term(1123) + r1(vrdav_Rr, p,i) * wm_interm_14_so_pt4(a,j) * wm_interm_22_so_pt4(a,j,i,q)
term(1124) = term(1124) + r1(vrdav_Rr, p,i) * wm_interm_16_so_pt4(a,j) * wm_interm_22_so_pt4(a,j,i,q)
term(1125) = term(1125) + t1(p,i) * wm_interm_12_so_pt4(a,q,j,i) * wm_interm_14_so_pt4(a,j)
term(1126) = term(1126) + t1(p,i) * wm_interm_12_so_pt4(a,q,j,i) * wm_interm_16_so_pt4(a,j)
term(1127) = term(1127) + r1(vrdav_Rr, a,q) * wm_interm_21_so_pt4(i,j) * wm_interm_5_so_pt4(a,p,j,i)
term(1128) = term(1128) + t2(a,p,q,i) * wm_interm_21_so_pt4(i,j) * wm_interm_2_so_pt4(a,j)
term(1129) = term(1129) + t2(a,p,j,i) * wm_interm_21_so_pt4(j,q) * wm_interm_2_so_pt4(a,i)
term(1130) = term(1130) + r2p(vrdav_Rr, a,q,p,i) * wm_interm_17_so_pt4(i,j) * wm_interm_50_so_pt4(a,j)
term(1131) = term(1131) + r2p(vrdav_Rr, a,q,p,i) * wm_interm_18_so_pt4(i,j) * wm_interm_50_so_pt4(a,j)
term(1132) = term(1132) + r2p(vrdav_Rr, a,q,p,i) * wm_interm_17_so_pt4(i,j) * wm_interm_51_so_pt4(a,j)
term(1133) = term(1133) + r2p(vrdav_Rr, a,q,p,i) * wm_interm_18_so_pt4(i,j) * wm_interm_51_so_pt4(a,j)
term(1134) = term(1134) + r2m(vrdav_Rr, a,q,p,i) * wm_interm_17_so_pt4(i,j) * wm_interm_50_so_pt4(a,j)
term(1135) = term(1135) + r2m(vrdav_Rr, a,q,p,i) * wm_interm_18_so_pt4(i,j) * wm_interm_50_so_pt4(a,j)
term(1136) = term(1136) + r2m(vrdav_Rr, a,q,p,i) * wm_interm_17_so_pt4(i,j) * wm_interm_51_so_pt4(a,j)
term(1137) = term(1137) + r2m(vrdav_Rr, a,q,p,i) * wm_interm_18_so_pt4(i,j) * wm_interm_51_so_pt4(a,j)
term(1138) = term(1138) + t1(a,q) * wm_interm_17_so_pt4(i,j) * wm_interm_65_so_pt4(a,p,j,i)
term(1139) = term(1139) + t1(a,q) * wm_interm_17_so_pt4(i,j) * wm_interm_66_so_pt4(a,p,j,i)
term(1140) = term(1140) + t1(a,q) * wm_interm_18_so_pt4(i,j) * wm_interm_65_so_pt4(a,p,j,i)
term(1141) = term(1141) + t1(a,q) * wm_interm_18_so_pt4(i,j) * wm_interm_66_so_pt4(a,p,j,i)
term(1142) = term(1142) + t1(a,q) * wm_interm_17_so_pt4(i,j) * wm_interm_77_so_pt4(a,p,j,i)
term(1143) = term(1143) + t1(a,q) * wm_interm_18_so_pt4(i,j) * wm_interm_77_so_pt4(a,p,j,i)
term(1144) = term(1144) + t1(a,q) * wm_interm_17_so_pt4(i,j) * wm_interm_79_so_pt4(a,p,j,i)
term(1145) = term(1145) + t1(a,q) * wm_interm_18_so_pt4(i,j) * wm_interm_79_so_pt4(a,p,j,i)
term(1146) = term(1146) + t1(a,q) * wm_interm_17_so_pt4(i,j) * wm_interm_75_so_pt4(a,p,j,i)
term(1147) = term(1147) + t1(a,q) * wm_interm_18_so_pt4(i,j) * wm_interm_75_so_pt4(a,p,j,i)
term(1148) = term(1148) + t1(a,q) * wm_interm_17_so_pt4(i,j) * wm_interm_64_so_pt4(a,p,j,i)
term(1149) = term(1149) + t1(a,q) * wm_interm_18_so_pt4(i,j) * wm_interm_64_so_pt4(a,p,j,i)
term(1150) = term(1150) + t1(a,q) * wm_interm_17_so_pt4(i,j) * wm_interm_94_so_pt4(a,p,j,i)
term(1151) = term(1151) + t1(a,q) * wm_interm_17_so_pt4(i,j) * wm_interm_93_so_pt4(a,p,j,i)
term(1152) = term(1152) + t1(a,q) * wm_interm_18_so_pt4(i,j) * wm_interm_94_so_pt4(a,p,j,i)
term(1153) = term(1153) + t1(a,q) * wm_interm_18_so_pt4(i,j) * wm_interm_93_so_pt4(a,p,j,i)
term(1154) = term(1154) + t2(a,p,j,i) * wm_interm_50_so_pt4(a,i) * wm_interm_85_so_pt4(q,j)
term(1155) = term(1155) + t2(a,p,j,i) * wm_interm_50_so_pt4(a,i) * wm_interm_86_so_pt4(q,j)
term(1156) = term(1156) + t2(a,p,j,i) * wm_interm_50_so_pt4(a,i) * wm_interm_87_so_pt4(q,j)
term(1157) = term(1157) + t2(a,p,j,i) * wm_interm_51_so_pt4(a,i) * wm_interm_85_so_pt4(q,j)
term(1158) = term(1158) + t2(a,p,j,i) * wm_interm_51_so_pt4(a,i) * wm_interm_86_so_pt4(q,j)
term(1159) = term(1159) + t2(a,p,j,i) * wm_interm_51_so_pt4(a,i) * wm_interm_87_so_pt4(q,j)
term(1160) = term(1160) + t2(a,p,j,i) * wm_interm_50_so_pt4(a,j) * wm_interm_85_so_pt4(q,i)
term(1161) = term(1161) + t2(a,p,j,i) * wm_interm_50_so_pt4(a,j) * wm_interm_86_so_pt4(q,i)
term(1162) = term(1162) + t2(a,p,j,i) * wm_interm_50_so_pt4(a,j) * wm_interm_87_so_pt4(q,i)
term(1163) = term(1163) + t2(a,p,j,i) * wm_interm_51_so_pt4(a,j) * wm_interm_85_so_pt4(q,i)
term(1164) = term(1164) + t2(a,p,j,i) * wm_interm_51_so_pt4(a,j) * wm_interm_86_so_pt4(q,i)
term(1165) = term(1165) + t2(a,p,j,i) * wm_interm_51_so_pt4(a,j) * wm_interm_87_so_pt4(q,i)
term(1166) = term(1166) + t2(a,p,q,i) * wm_interm_50_so_pt4(a,j) * wm_interm_85_so_pt4(j,i)
term(1167) = term(1167) + t2(a,p,q,i) * wm_interm_50_so_pt4(a,j) * wm_interm_86_so_pt4(j,i)
term(1168) = term(1168) + t2(a,p,q,i) * wm_interm_50_so_pt4(a,j) * wm_interm_87_so_pt4(j,i)
term(1169) = term(1169) + t2(a,p,q,i) * wm_interm_51_so_pt4(a,j) * wm_interm_85_so_pt4(j,i)
term(1170) = term(1170) + t2(a,p,q,i) * wm_interm_51_so_pt4(a,j) * wm_interm_86_so_pt4(j,i)
term(1171) = term(1171) + t2(a,p,q,i) * wm_interm_51_so_pt4(a,j) * wm_interm_87_so_pt4(j,i)
term(1172) = term(1172) + r2m(vrdav_Rr, a,j,p,i) * wm_interm_17_so_pt4(i,q) * wm_interm_50_so_pt4(a,j)
term(1173) = term(1173) + r2m(vrdav_Rr, a,j,p,i) * wm_interm_18_so_pt4(i,q) * wm_interm_50_so_pt4(a,j)
term(1174) = term(1174) + r2m(vrdav_Rr, a,j,p,i) * wm_interm_17_so_pt4(i,q) * wm_interm_51_so_pt4(a,j)
term(1175) = term(1175) + r2m(vrdav_Rr, a,j,p,i) * wm_interm_18_so_pt4(i,q) * wm_interm_51_so_pt4(a,j)
term(1176) = term(1176) + t2(a,p,j,i) * wm_interm_106_so_pt4(q,j) * wm_interm_50_so_pt4(a,i)
term(1177) = term(1177) + t2(a,p,j,i) * wm_interm_107_so_pt4(q,j) * wm_interm_50_so_pt4(a,i)
term(1178) = term(1178) + t2(a,p,j,i) * wm_interm_108_so_pt4(q,j) * wm_interm_50_so_pt4(a,i)
term(1179) = term(1179) + t2(a,p,j,i) * wm_interm_106_so_pt4(q,j) * wm_interm_51_so_pt4(a,i)
term(1180) = term(1180) + t2(a,p,j,i) * wm_interm_107_so_pt4(q,j) * wm_interm_51_so_pt4(a,i)
term(1181) = term(1181) + t2(a,p,j,i) * wm_interm_108_so_pt4(q,j) * wm_interm_51_so_pt4(a,i)
term(1182) = term(1182) + t2(a,p,j,i) * wm_interm_106_so_pt4(q,i) * wm_interm_50_so_pt4(a,j)
term(1183) = term(1183) + t2(a,p,j,i) * wm_interm_107_so_pt4(q,i) * wm_interm_50_so_pt4(a,j)
term(1184) = term(1184) + t2(a,p,j,i) * wm_interm_108_so_pt4(q,i) * wm_interm_50_so_pt4(a,j)
term(1185) = term(1185) + t2(a,p,j,i) * wm_interm_106_so_pt4(q,i) * wm_interm_51_so_pt4(a,j)
term(1186) = term(1186) + t2(a,p,j,i) * wm_interm_107_so_pt4(q,i) * wm_interm_51_so_pt4(a,j)
term(1187) = term(1187) + t2(a,p,j,i) * wm_interm_108_so_pt4(q,i) * wm_interm_51_so_pt4(a,j)
term(1188) = term(1188) + t2(a,p,q,i) * wm_interm_106_so_pt4(j,i) * wm_interm_50_so_pt4(a,j)
term(1189) = term(1189) + t2(a,p,q,i) * wm_interm_107_so_pt4(j,i) * wm_interm_50_so_pt4(a,j)
term(1190) = term(1190) + t2(a,p,q,i) * wm_interm_108_so_pt4(j,i) * wm_interm_50_so_pt4(a,j)
term(1191) = term(1191) + t2(a,p,q,i) * wm_interm_106_so_pt4(j,i) * wm_interm_51_so_pt4(a,j)
term(1192) = term(1192) + t2(a,p,q,i) * wm_interm_107_so_pt4(j,i) * wm_interm_51_so_pt4(a,j)
term(1193) = term(1193) + t2(a,p,q,i) * wm_interm_108_so_pt4(j,i) * wm_interm_51_so_pt4(a,j)
term(1194) = term(1194) + t1(a,q) * wm_interm_101_so_pt4(i,j) * wm_interm_10_so_pt4(a,p,j,i)
term(1195) = term(1195) + t1(a,q) * wm_interm_103_so_pt4(i,j) * wm_interm_10_so_pt4(a,p,j,i)
term(1196) = term(1196) + t1(a,q) * wm_interm_101_so_pt4(i,j) * wm_interm_1_so_pt4(a,p,j,i)
term(1197) = term(1197) + t1(a,q) * wm_interm_103_so_pt4(i,j) * wm_interm_1_so_pt4(a,p,j,i)
term(1198) = term(1198) + t1(a,q) * wm_interm_101_so_pt4(i,j) * wm_interm_11_so_pt4(a,p,j,i)
term(1199) = term(1199) + t1(a,q) * wm_interm_103_so_pt4(i,j) * wm_interm_11_so_pt4(a,p,j,i)
end do 
end do 
end do 

term(1123) = term(1123) * (8.0d+0) 
term(1124) = term(1124) * (-4.0d+0) 
term(1125) = term(1125) * (-4.0d+0) 
term(1126) = term(1126) * (2.0d+0) 
term(1127) = term(1127) * (-4.0d+0) 
term(1128) = term(1128) * (2.0d+0) 
term(1129) = term(1129) * (4.0d+0) 
term(1130) = term(1130) * (4.0d+0) 
term(1131) = term(1131) * (-8.0d+0) 
term(1132) = term(1132) * (-2.0d+0) 
term(1133) = term(1133) * (4.0d+0) 
term(1134) = term(1134) * (16.0d+0) 
term(1135) = term(1135) * (-32.0d+0) 
term(1136) = term(1136) * (-8.0d+0) 
term(1137) = term(1137) * (16.0d+0) 
term(1138) = term(1138) * (-1.0d+0) 
term(1139) = term(1139) * (2.0d+0) 
term(1140) = term(1140) * (2.0d+0) 
term(1141) = term(1141) * (-4.0d+0) 
term(1142) = term(1142) * (-1.0d+0) 
term(1143) = term(1143) * (2.0d+0) 
term(1144) = term(1144) * (2.0d+0) 
term(1145) = term(1145) * (-4.0d+0) 
term(1146) = term(1146) * (-1.0d+0) 
term(1147) = term(1147) * (2.0d+0) 
term(1148) = term(1148) * (-1.0d+0) 
term(1149) = term(1149) * (2.0d+0) 
term(1150) = term(1150) * (-4.0d+0) 
term(1151) = term(1151) * (4.0d+0) 
term(1152) = term(1152) * (8.0d+0) 
term(1153) = term(1153) * (-8.0d+0) 
term(1154) = term(1154) * (2.0d+0) 
term(1155) = term(1155) * (-4.0d+0) 
term(1156) = term(1156) * (2.0d+0) 
term(1157) = term(1157) * (-1.0d+0) 
term(1158) = term(1158) * (2.0d+0) 
term(1159) = term(1159) * (-1.0d+0) 
term(1160) = term(1160) * (-4.0d+0) 
term(1161) = term(1161) * (8.0d+0) 
term(1162) = term(1162) * (-4.0d+0) 
term(1163) = term(1163) * (2.0d+0) 
term(1164) = term(1164) * (-4.0d+0) 
term(1165) = term(1165) * (2.0d+0) 
term(1166) = term(1166) * (2.0d+0) 
term(1167) = term(1167) * (-4.0d+0) 
term(1168) = term(1168) * (2.0d+0) 
term(1169) = term(1169) * (-1.0d+0) 
term(1170) = term(1170) * (2.0d+0) 
term(1171) = term(1171) * (-1.0d+0) 
term(1172) = term(1172) * (-8.0d+0) 
term(1173) = term(1173) * (16.0d+0) 
term(1174) = term(1174) * (4.0d+0) 
term(1175) = term(1175) * (-8.0d+0) 
term(1176) = term(1176) * (4.0d+0) 
term(1177) = term(1177) * (-8.0d+0) 
term(1178) = term(1178) * (4.0d+0) 
term(1179) = term(1179) * (-2.0d+0) 
term(1180) = term(1180) * (4.0d+0) 
term(1181) = term(1181) * (-2.0d+0) 
term(1182) = term(1182) * (-8.0d+0) 
term(1183) = term(1183) * (16.0d+0) 
term(1184) = term(1184) * (-8.0d+0) 
term(1185) = term(1185) * (4.0d+0) 
term(1186) = term(1186) * (-8.0d+0) 
term(1187) = term(1187) * (4.0d+0) 
term(1188) = term(1188) * (4.0d+0) 
term(1189) = term(1189) * (-8.0d+0) 
term(1190) = term(1190) * (4.0d+0) 
term(1191) = term(1191) * (-2.0d+0) 
term(1192) = term(1192) * (4.0d+0) 
term(1193) = term(1193) * (-2.0d+0) 
term(1194) = term(1194) * (-8.0d+0) 
term(1195) = term(1195) * (8.0d+0) 
term(1196) = term(1196) * (-8.0d+0) 
term(1197) = term(1197) * (8.0d+0) 
term(1198) = term(1198) * (16.0d+0) 
term(1199) = term(1199) * (-16.0d+0) 

do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(1200) = term(1200) + t2(a,b,i,j) * wm_interm_50_so_pt4(b,i) * wm_interm_70_so_pt4(p,a,q,j)
term(1201) = term(1201) + t2(a,b,i,j) * wm_interm_50_so_pt4(b,i) * wm_interm_69_so_pt4(p,a,q,j)
term(1202) = term(1202) + t2(a,b,i,j) * wm_interm_50_so_pt4(b,i) * wm_interm_67_so_pt4(p,a,q,j)
term(1203) = term(1203) + t2(a,b,i,j) * wm_interm_50_so_pt4(b,i) * wm_interm_68_so_pt4(p,a,q,j)
term(1204) = term(1204) + t2(a,b,i,j) * wm_interm_50_so_pt4(b,i) * wm_interm_84_so_pt4(p,a,q,j)
term(1205) = term(1205) + t2(a,b,i,j) * wm_interm_50_so_pt4(b,i) * wm_interm_74_so_pt4(p,a,q,j)
term(1206) = term(1206) + t2(a,b,i,j) * wm_interm_51_so_pt4(b,i) * wm_interm_70_so_pt4(p,a,q,j)
term(1207) = term(1207) + t2(a,b,i,j) * wm_interm_51_so_pt4(b,i) * wm_interm_69_so_pt4(p,a,q,j)
term(1208) = term(1208) + t2(a,b,i,j) * wm_interm_51_so_pt4(b,i) * wm_interm_67_so_pt4(p,a,q,j)
term(1209) = term(1209) + t2(a,b,i,j) * wm_interm_51_so_pt4(b,i) * wm_interm_68_so_pt4(p,a,q,j)
term(1210) = term(1210) + t2(a,b,i,j) * wm_interm_51_so_pt4(b,i) * wm_interm_84_so_pt4(p,a,q,j)
term(1211) = term(1211) + t2(a,b,i,j) * wm_interm_51_so_pt4(b,i) * wm_interm_74_so_pt4(p,a,q,j)
term(1212) = term(1212) + t2(a,b,i,j) * wm_interm_50_so_pt4(b,i) * wm_interm_98_so_pt4(p,a,q,j)
term(1213) = term(1213) + t2(a,b,i,j) * wm_interm_50_so_pt4(b,i) * wm_interm_97_so_pt4(p,a,q,j)
term(1214) = term(1214) + t2(a,b,i,j) * wm_interm_50_so_pt4(b,i) * wm_interm_95_so_pt4(p,a,q,j)
term(1215) = term(1215) + t2(a,b,i,j) * wm_interm_50_so_pt4(b,i) * wm_interm_96_so_pt4(p,a,q,j)
term(1216) = term(1216) + t2(a,b,i,j) * wm_interm_51_so_pt4(b,i) * wm_interm_98_so_pt4(p,a,q,j)
term(1217) = term(1217) + t2(a,b,i,j) * wm_interm_51_so_pt4(b,i) * wm_interm_97_so_pt4(p,a,q,j)
term(1218) = term(1218) + t2(a,b,i,j) * wm_interm_51_so_pt4(b,i) * wm_interm_95_so_pt4(p,a,q,j)
term(1219) = term(1219) + t2(a,b,i,j) * wm_interm_51_so_pt4(b,i) * wm_interm_96_so_pt4(p,a,q,j)
end do 
end do 
end do 
end do 

term(1200) = term(1200) * (4.0d+0) 
term(1201) = term(1201) * (-8.0d+0) 
term(1202) = term(1202) * (-8.0d+0) 
term(1203) = term(1203) * (16.0d+0) 
term(1204) = term(1204) * (4.0d+0) 
term(1205) = term(1205) * (-8.0d+0) 
term(1206) = term(1206) * (-2.0d+0) 
term(1207) = term(1207) * (4.0d+0) 
term(1208) = term(1208) * (4.0d+0) 
term(1209) = term(1209) * (-8.0d+0) 
term(1210) = term(1210) * (-2.0d+0) 
term(1211) = term(1211) * (4.0d+0) 
term(1212) = term(1212) * (16.0d+0) 
term(1213) = term(1213) * (-16.0d+0) 
term(1214) = term(1214) * (-32.0d+0) 
term(1215) = term(1215) * (32.0d+0) 
term(1216) = term(1216) * (-8.0d+0) 
term(1217) = term(1217) * (8.0d+0) 
term(1218) = term(1218) * (16.0d+0) 
term(1219) = term(1219) * (-16.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(1220) = term(1220) + wm_interm_50_so_pt4(a,i) * wm_interm_52_so_pt4(p,a,i,q)
term(1221) = term(1221) + wm_interm_51_so_pt4(a,i) * wm_interm_52_so_pt4(p,a,i,q)
term(1222) = term(1222) + wm_interm_50_so_pt4(a,i) * wm_interm_52_so_pt4(a,p,q,i)
term(1223) = term(1223) + wm_interm_51_so_pt4(a,i) * wm_interm_52_so_pt4(a,p,q,i)
term(1224) = term(1224) + wm_interm_45_so_pt4(a,i) * wm_interm_53_so_pt4(a,p,q,i)
term(1225) = term(1225) + t1(a,i) * wm_interm_19_so_pt4(a,p) * wm_interm_4_so_pt4(i,q)
term(1226) = term(1226) + t1(a,i) * wm_interm_20_so_pt4(a,p) * wm_interm_4_so_pt4(i,q)
term(1227) = term(1227) + r1(vrdav_Rr, a,i) * wm_interm_19_so_pt4(a,p) * wm_interm_21_so_pt4(i,q)
term(1228) = term(1228) + r1(vrdav_Rr, a,i) * wm_interm_20_so_pt4(a,p) * wm_interm_21_so_pt4(i,q)
term(1229) = term(1229) + t1(a,q) * wm_interm_16_so_pt4(p,i) * wm_interm_2_so_pt4(a,i)
term(1230) = term(1230) + t1(a,q) * wm_interm_14_so_pt4(p,i) * wm_interm_2_so_pt4(a,i)
term(1231) = term(1231) + t1(p,i) * wm_interm_16_so_pt4(a,q) * wm_interm_2_so_pt4(a,i)
term(1232) = term(1232) + t1(p,i) * wm_interm_14_so_pt4(a,q) * wm_interm_2_so_pt4(a,i)
term(1233) = term(1233) + t1(a,i) * wm_interm_17_so_pt4(i,q) * wm_interm_91_so_pt4(a,p)
term(1234) = term(1234) + t1(a,i) * wm_interm_18_so_pt4(i,q) * wm_interm_91_so_pt4(a,p)
term(1235) = term(1235) + t1(a,i) * wm_interm_17_so_pt4(i,q) * wm_interm_92_so_pt4(a,p)
term(1236) = term(1236) + t1(a,i) * wm_interm_18_so_pt4(i,q) * wm_interm_92_so_pt4(a,p)
term(1237) = term(1237) + t1(a,i) * wm_interm_17_so_pt4(i,q) * wm_interm_90_so_pt4(a,p)
term(1238) = term(1238) + t1(a,i) * wm_interm_18_so_pt4(i,q) * wm_interm_90_so_pt4(a,p)
term(1239) = term(1239) + t1(a,i) * wm_interm_19_so_pt4(a,p) * wm_interm_76_so_pt4(i,q)
term(1240) = term(1240) + t1(a,i) * wm_interm_20_so_pt4(a,p) * wm_interm_76_so_pt4(i,q)
term(1241) = term(1241) + t1(a,i) * wm_interm_19_so_pt4(a,p) * wm_interm_78_so_pt4(i,q)
term(1242) = term(1242) + t1(a,i) * wm_interm_20_so_pt4(a,p) * wm_interm_78_so_pt4(i,q)
term(1243) = term(1243) + t1(a,i) * wm_interm_19_so_pt4(a,p) * wm_interm_80_so_pt4(i,q)
term(1244) = term(1244) + t1(a,i) * wm_interm_20_so_pt4(a,p) * wm_interm_80_so_pt4(i,q)
term(1245) = term(1245) + t1(a,i) * wm_interm_58_so_pt4(a,p) * wm_interm_85_so_pt4(q,i)
term(1246) = term(1246) + t1(a,i) * wm_interm_58_so_pt4(a,p) * wm_interm_86_so_pt4(q,i)
term(1247) = term(1247) + t1(a,i) * wm_interm_58_so_pt4(a,p) * wm_interm_87_so_pt4(q,i)
term(1248) = term(1248) + t1(a,i) * wm_interm_57_so_pt4(a,p) * wm_interm_85_so_pt4(q,i)
term(1249) = term(1249) + t1(a,i) * wm_interm_57_so_pt4(a,p) * wm_interm_86_so_pt4(q,i)
term(1250) = term(1250) + t1(a,i) * wm_interm_57_so_pt4(a,p) * wm_interm_87_so_pt4(q,i)
term(1251) = term(1251) + t1(a,i) * wm_interm_110_so_pt4(a,p) * wm_interm_17_so_pt4(i,q)
term(1252) = term(1252) + t1(a,i) * wm_interm_110_so_pt4(a,p) * wm_interm_18_so_pt4(i,q)
term(1253) = term(1253) + t1(a,i) * wm_interm_111_so_pt4(a,p) * wm_interm_17_so_pt4(i,q)
term(1254) = term(1254) + t1(a,i) * wm_interm_111_so_pt4(a,p) * wm_interm_18_so_pt4(i,q)
term(1255) = term(1255) + t1(a,i) * wm_interm_101_so_pt4(i,q) * wm_interm_19_so_pt4(a,p)
term(1256) = term(1256) + t1(a,i) * wm_interm_101_so_pt4(i,q) * wm_interm_20_so_pt4(a,p)
term(1257) = term(1257) + t1(a,i) * wm_interm_103_so_pt4(i,q) * wm_interm_19_so_pt4(a,p)
term(1258) = term(1258) + t1(a,i) * wm_interm_103_so_pt4(i,q) * wm_interm_20_so_pt4(a,p)
term(1259) = term(1259) + t1(a,i) * wm_interm_106_so_pt4(q,i) * wm_interm_58_so_pt4(a,p)
term(1260) = term(1260) + t1(a,i) * wm_interm_107_so_pt4(q,i) * wm_interm_58_so_pt4(a,p)
term(1261) = term(1261) + t1(a,i) * wm_interm_108_so_pt4(q,i) * wm_interm_58_so_pt4(a,p)
term(1262) = term(1262) + t1(a,i) * wm_interm_106_so_pt4(q,i) * wm_interm_57_so_pt4(a,p)
term(1263) = term(1263) + t1(a,i) * wm_interm_107_so_pt4(q,i) * wm_interm_57_so_pt4(a,p)
term(1264) = term(1264) + t1(a,i) * wm_interm_108_so_pt4(q,i) * wm_interm_57_so_pt4(a,p)
end do 
end do 

term(1220) = term(1220) * (-4.0d+0) 
term(1221) = term(1221) * (2.0d+0) 
term(1222) = term(1222) * (-4.0d+0) 
term(1223) = term(1223) * (2.0d+0) 
term(1224) = term(1224) * (4.0d+0) 
term(1225) = term(1225) * (2.0d+0) 
term(1226) = term(1226) * (-4.0d+0) 
term(1227) = term(1227) * (4.0d+0) 
term(1228) = term(1228) * (-8.0d+0) 
term(1229) = term(1229) * (4.0d+0) 
term(1230) = term(1230) * (-8.0d+0) 
term(1231) = term(1231) * (4.0d+0) 
term(1232) = term(1232) * (-8.0d+0) 
term(1233) = term(1233) * (-1.0d+0) 
term(1234) = term(1234) * (2.0d+0) 
term(1235) = term(1235) * (2.0d+0) 
term(1236) = term(1236) * (-4.0d+0) 
term(1237) = term(1237) * (-1.0d+0) 
term(1238) = term(1238) * (2.0d+0) 
term(1239) = term(1239) * (-1.0d+0) 
term(1240) = term(1240) * (2.0d+0) 
term(1241) = term(1241) * (-1.0d+0) 
term(1242) = term(1242) * (2.0d+0) 
term(1243) = term(1243) * (2.0d+0) 
term(1244) = term(1244) * (-4.0d+0) 
term(1245) = term(1245) * (-1.0d+0) 
term(1246) = term(1246) * (2.0d+0) 
term(1247) = term(1247) * (-1.0d+0) 
term(1248) = term(1248) * (2.0d+0) 
term(1249) = term(1249) * (-4.0d+0) 
term(1250) = term(1250) * (2.0d+0) 
term(1251) = term(1251) * (-4.0d+0) 
term(1252) = term(1252) * (8.0d+0) 
term(1253) = term(1253) * (4.0d+0) 
term(1254) = term(1254) * (-8.0d+0) 
term(1255) = term(1255) * (-4.0d+0) 
term(1256) = term(1256) * (8.0d+0) 
term(1257) = term(1257) * (4.0d+0) 
term(1258) = term(1258) * (-8.0d+0) 
term(1259) = term(1259) * (-2.0d+0) 
term(1260) = term(1260) * (4.0d+0) 
term(1261) = term(1261) * (-2.0d+0) 
term(1262) = term(1262) * (4.0d+0) 
term(1263) = term(1263) * (-8.0d+0) 
term(1264) = term(1264) * (4.0d+0) 

do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(1265) = term(1265) + t2(a,b,i,q) * wm_interm_50_so_pt4(b,i) * wm_interm_81_so_pt4(p,a)
term(1266) = term(1266) + t2(a,b,i,q) * wm_interm_50_so_pt4(b,i) * wm_interm_82_so_pt4(p,a)
term(1267) = term(1267) + t2(a,b,i,q) * wm_interm_50_so_pt4(b,i) * wm_interm_83_so_pt4(p,a)
term(1268) = term(1268) + t2(a,b,i,q) * wm_interm_51_so_pt4(b,i) * wm_interm_81_so_pt4(p,a)
term(1269) = term(1269) + t2(a,b,i,q) * wm_interm_51_so_pt4(b,i) * wm_interm_82_so_pt4(p,a)
term(1270) = term(1270) + t2(a,b,i,q) * wm_interm_51_so_pt4(b,i) * wm_interm_83_so_pt4(p,a)
term(1271) = term(1271) + t2(a,b,i,q) * wm_interm_104_so_pt4(p,a) * wm_interm_50_so_pt4(b,i)
term(1272) = term(1272) + t2(a,b,i,q) * wm_interm_105_so_pt4(p,a) * wm_interm_50_so_pt4(b,i)
term(1273) = term(1273) + t2(a,b,i,q) * wm_interm_112_so_pt4(p,a) * wm_interm_50_so_pt4(b,i)
term(1274) = term(1274) + t2(a,b,i,q) * wm_interm_104_so_pt4(p,a) * wm_interm_51_so_pt4(b,i)
term(1275) = term(1275) + t2(a,b,i,q) * wm_interm_105_so_pt4(p,a) * wm_interm_51_so_pt4(b,i)
term(1276) = term(1276) + t2(a,b,i,q) * wm_interm_112_so_pt4(p,a) * wm_interm_51_so_pt4(b,i)
end do 
end do 
end do 

term(1265) = term(1265) * (2.0d+0) 
term(1266) = term(1266) * (-4.0d+0) 
term(1267) = term(1267) * (2.0d+0) 
term(1268) = term(1268) * (-1.0d+0) 
term(1269) = term(1269) * (2.0d+0) 
term(1270) = term(1270) * (-1.0d+0) 
term(1271) = term(1271) * (4.0d+0) 
term(1272) = term(1272) * (-8.0d+0) 
term(1273) = term(1273) * (4.0d+0) 
term(1274) = term(1274) * (-2.0d+0) 
term(1275) = term(1275) * (4.0d+0) 
term(1276) = term(1276) * (-2.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
do l = 1, nocc 
do i = 1, nocc 
term(1277) = term(1277) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_22_so_pt4(a,k,l,q) * wm_interm_61_so_pt4(j,i,l,k)
term(1278) = term(1278) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_22_so_pt4(a,k,l,q) * wm_interm_61_so_pt4(i,j,l,k)
end do 
end do 
end do 
end do 
end do 

term(1277) = term(1277) * (-0.5d+0) 

do l = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(1279) = term(1279) + t1(p,i) * wm_interm_71_so_pt4(j,k,q,l) * wm_interm_9_so_pt4(l,i,j,k)
term(1280) = term(1280) + t1(p,i) * wm_interm_71_so_pt4(j,k,q,l) * wm_interm_9_so_pt4(i,l,j,k)
term(1281) = term(1281) + t1(p,i) * wm_interm_72_so_pt4(j,k,q,l) * wm_interm_9_so_pt4(l,i,j,k)
term(1282) = term(1282) + t1(p,i) * wm_interm_72_so_pt4(j,k,q,l) * wm_interm_9_so_pt4(i,l,j,k)
term(1283) = term(1283) + t1(p,i) * wm_interm_61_so_pt4(j,i,k,l) * wm_interm_88_so_pt4(q,j,l,k)
term(1284) = term(1284) + t1(p,i) * wm_interm_61_so_pt4(j,i,k,l) * wm_interm_88_so_pt4(q,j,k,l)
term(1285) = term(1285) + t1(p,i) * wm_interm_61_so_pt4(i,j,k,l) * wm_interm_88_so_pt4(q,j,l,k)
term(1286) = term(1286) + t1(p,i) * wm_interm_61_so_pt4(i,j,k,l) * wm_interm_88_so_pt4(j,q,k,l)
term(1287) = term(1287) + t1(p,i) * wm_interm_61_so_pt4(i,j,k,l) * wm_interm_88_so_pt4(q,j,k,l)
term(1288) = term(1288) + t1(p,i) * wm_interm_99_so_pt4(j,k,q,l) * wm_interm_9_so_pt4(l,i,j,k)
term(1289) = term(1289) + t1(p,i) * wm_interm_99_so_pt4(j,k,q,l) * wm_interm_9_so_pt4(i,l,j,k)
term(1290) = term(1290) + t1(p,i) * wm_interm_109_so_pt4(q,j,k,l) * wm_interm_61_so_pt4(j,i,l,k)
term(1291) = term(1291) + t1(p,i) * wm_interm_109_so_pt4(q,j,k,l) * wm_interm_61_so_pt4(j,i,k,l)
term(1292) = term(1292) + t1(p,i) * wm_interm_109_so_pt4(q,j,k,l) * wm_interm_61_so_pt4(i,j,l,k)
term(1293) = term(1293) + t1(p,i) * wm_interm_109_so_pt4(j,q,k,l) * wm_interm_61_so_pt4(i,j,k,l)
term(1294) = term(1294) + t1(p,i) * wm_interm_109_so_pt4(q,j,k,l) * wm_interm_61_so_pt4(i,j,k,l)
term(1295) = term(1295) + t1(p,i) * wm_interm_71_so_pt4(j,i,k,l) * wm_interm_9_so_pt4(l,k,j,q)
term(1296) = term(1296) + t1(p,i) * wm_interm_72_so_pt4(j,i,k,l) * wm_interm_9_so_pt4(k,l,j,q)
term(1297) = term(1297) + t1(p,i) * wm_interm_72_so_pt4(j,i,k,l) * wm_interm_9_so_pt4(l,k,j,q)
term(1298) = term(1298) + t1(p,i) * wm_interm_71_so_pt4(i,j,k,l) * wm_interm_9_so_pt4(k,l,j,q)
term(1299) = term(1299) + t1(p,i) * wm_interm_71_so_pt4(i,j,k,l) * wm_interm_9_so_pt4(l,k,j,q)
term(1300) = term(1300) + t1(p,i) * wm_interm_72_so_pt4(i,j,k,l) * wm_interm_9_so_pt4(l,k,j,q)
term(1301) = term(1301) + t1(p,i) * wm_interm_99_so_pt4(j,i,k,l) * wm_interm_9_so_pt4(l,k,j,q)
term(1302) = term(1302) + t1(p,i) * wm_interm_99_so_pt4(j,i,k,l) * wm_interm_9_so_pt4(k,l,j,q)
term(1303) = term(1303) + t1(p,i) * wm_interm_99_so_pt4(i,j,k,l) * wm_interm_9_so_pt4(k,l,j,q)
term(1304) = term(1304) + t1(p,i) * wm_interm_99_so_pt4(i,j,k,l) * wm_interm_9_so_pt4(l,k,j,q)
end do 
end do 
end do 
end do 

term(1279) = term(1279) * (-0.5d+0) 
term(1282) = term(1282) * (-0.5d+0) 
term(1284) = term(1284) * (-0.5d+0) 
term(1285) = term(1285) * (-0.5d+0) 
term(1286) = term(1286) * (-0.5d+0) 
term(1288) = term(1288) * (-2.0d+0) 
term(1289) = term(1289) * (2.0d+0) 
term(1290) = term(1290) * (2.0d+0) 
term(1291) = term(1291) * (-1.0d+0) 
term(1292) = term(1292) * (-1.0d+0) 
term(1293) = term(1293) * (-1.0d+0) 
term(1294) = term(1294) * (2.0d+0) 
term(1295) = term(1295) * (-1.0d+0) 
term(1296) = term(1296) * (-1.0d+0) 
term(1297) = term(1297) * (2.0d+0) 
term(1298) = term(1298) * (-1.0d+0) 
term(1299) = term(1299) * (2.0d+0) 
term(1300) = term(1300) * (-1.0d+0) 
term(1301) = term(1301) * (-4.0d+0) 
term(1302) = term(1302) * (4.0d+0) 
term(1303) = term(1303) * (-4.0d+0) 
term(1304) = term(1304) * (4.0d+0) 

do k = 1, nocc 
do l = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(1305) = term(1305) + t1(p,i) * wm_interm_71_so_pt4(j,k,l,q) * wm_interm_9_so_pt4(i,l,j,k)
term(1306) = term(1306) + t1(p,i) * wm_interm_72_so_pt4(j,k,l,q) * wm_interm_9_so_pt4(l,i,j,k)
term(1307) = term(1307) + t1(p,i) * wm_interm_99_so_pt4(j,k,l,q) * wm_interm_9_so_pt4(i,l,j,k)
term(1308) = term(1308) + t1(p,i) * wm_interm_99_so_pt4(j,k,l,q) * wm_interm_9_so_pt4(l,i,j,k)
end do 
end do 
end do 
end do 

term(1305) = term(1305) * (-0.5d+0) 
term(1306) = term(1306) * (-0.5d+0) 
term(1307) = term(1307) * (-2.0d+0) 
term(1308) = term(1308) * (2.0d+0) 

do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
term(1309) = term(1309) + r1(vrdav_Rr, p,i) * wm_interm_21_so_pt4(j,k) * wm_interm_9_so_pt4(k,i,j,q)
term(1310) = term(1310) + r1(vrdav_Rr, p,i) * wm_interm_21_so_pt4(j,k) * wm_interm_9_so_pt4(i,k,j,q)
term(1311) = term(1311) + t1(p,i) * wm_interm_4_so_pt4(j,k) * wm_interm_9_so_pt4(i,k,j,q)
term(1312) = term(1312) + t1(p,i) * wm_interm_17_so_pt4(j,k) * wm_interm_71_so_pt4(i,k,j,q)
term(1313) = term(1313) + t1(p,i) * wm_interm_17_so_pt4(j,k) * wm_interm_71_so_pt4(i,k,q,j)
term(1314) = term(1314) + t1(p,i) * wm_interm_18_so_pt4(j,k) * wm_interm_71_so_pt4(i,k,j,q)
term(1315) = term(1315) + t1(p,i) * wm_interm_18_so_pt4(j,k) * wm_interm_71_so_pt4(i,k,q,j)
term(1316) = term(1316) + t1(p,i) * wm_interm_17_so_pt4(j,k) * wm_interm_72_so_pt4(i,k,q,j)
term(1317) = term(1317) + t1(p,i) * wm_interm_18_so_pt4(j,k) * wm_interm_72_so_pt4(i,k,q,j)
term(1318) = term(1318) + t1(p,i) * wm_interm_17_so_pt4(j,k) * wm_interm_71_so_pt4(k,i,q,j)
term(1319) = term(1319) + t1(p,i) * wm_interm_18_so_pt4(j,k) * wm_interm_71_so_pt4(k,i,q,j)
term(1320) = term(1320) + t1(p,i) * wm_interm_17_so_pt4(j,k) * wm_interm_72_so_pt4(k,i,j,q)
term(1321) = term(1321) + t1(p,i) * wm_interm_17_so_pt4(j,k) * wm_interm_72_so_pt4(k,i,q,j)
term(1322) = term(1322) + t1(p,i) * wm_interm_18_so_pt4(j,k) * wm_interm_72_so_pt4(k,i,j,q)
term(1323) = term(1323) + t1(p,i) * wm_interm_18_so_pt4(j,k) * wm_interm_72_so_pt4(k,i,q,j)
term(1324) = term(1324) + t1(p,i) * wm_interm_17_so_pt4(j,k) * wm_interm_99_so_pt4(i,k,j,q)
term(1325) = term(1325) + t1(p,i) * wm_interm_17_so_pt4(j,k) * wm_interm_99_so_pt4(i,k,q,j)
term(1326) = term(1326) + t1(p,i) * wm_interm_18_so_pt4(j,k) * wm_interm_99_so_pt4(i,k,j,q)
term(1327) = term(1327) + t1(p,i) * wm_interm_18_so_pt4(j,k) * wm_interm_99_so_pt4(i,k,q,j)
term(1328) = term(1328) + t1(p,i) * wm_interm_17_so_pt4(j,k) * wm_interm_99_so_pt4(k,i,q,j)
term(1329) = term(1329) + t1(p,i) * wm_interm_18_so_pt4(j,k) * wm_interm_99_so_pt4(k,i,q,j)
term(1330) = term(1330) + t1(p,i) * wm_interm_17_so_pt4(j,k) * wm_interm_99_so_pt4(k,i,j,q)
term(1331) = term(1331) + t1(p,i) * wm_interm_18_so_pt4(j,k) * wm_interm_99_so_pt4(k,i,j,q)
term(1332) = term(1332) + t1(p,i) * wm_interm_78_so_pt4(j,k) * wm_interm_9_so_pt4(i,k,j,q)
term(1333) = term(1333) + t1(p,i) * wm_interm_80_so_pt4(j,k) * wm_interm_9_so_pt4(i,k,j,q)
term(1334) = term(1334) + t1(p,i) * wm_interm_76_so_pt4(j,k) * wm_interm_9_so_pt4(i,k,j,q)
term(1335) = term(1335) + t1(p,i) * wm_interm_101_so_pt4(j,k) * wm_interm_9_so_pt4(i,k,j,q)
term(1336) = term(1336) + t1(p,i) * wm_interm_103_so_pt4(j,k) * wm_interm_9_so_pt4(i,k,j,q)
end do 
end do 
end do 

term(1309) = term(1309) * (-4.0d+0) 
term(1310) = term(1310) * (2.0d+0) 
term(1311) = term(1311) * (4.0d+0) 
term(1312) = term(1312) * (-0.5d+0) 
term(1315) = term(1315) * (-2.0d+0) 
term(1316) = term(1316) * (-0.5d+0) 
term(1318) = term(1318) * (-0.5d+0) 
term(1320) = term(1320) * (-0.5d+0) 
term(1323) = term(1323) * (-2.0d+0) 
term(1324) = term(1324) * (-2.0d+0) 
term(1325) = term(1325) * (2.0d+0) 
term(1326) = term(1326) * (4.0d+0) 
term(1327) = term(1327) * (-4.0d+0) 
term(1328) = term(1328) * (-2.0d+0) 
term(1329) = term(1329) * (4.0d+0) 
term(1330) = term(1330) * (2.0d+0) 
term(1331) = term(1331) * (-4.0d+0) 
term(1332) = term(1332) * (-2.0d+0) 
term(1333) = term(1333) * (4.0d+0) 
term(1334) = term(1334) * (-2.0d+0) 
term(1335) = term(1335) * (-8.0d+0) 
term(1336) = term(1336) * (8.0d+0) 

do l = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(1337) = term(1337) + r2m(vrdav_Rr, a,j,p,i) * wm_interm_22_so_pt4(a,k,l,q) * wm_interm_61_so_pt4(i,j,k,l)
term(1338) = term(1338) + r2m(vrdav_Rr, a,j,p,i) * wm_interm_22_so_pt4(a,k,l,q) * wm_interm_61_so_pt4(j,i,k,l)
term(1339) = term(1339) + t2(a,p,q,i) * wm_interm_47_so_pt4(a,j,k,l) * wm_interm_88_so_pt4(j,k,l,i)
term(1340) = term(1340) + t2(a,p,q,i) * wm_interm_47_so_pt4(a,j,k,l) * wm_interm_88_so_pt4(k,j,i,l)
term(1341) = term(1341) + t2(a,p,q,i) * wm_interm_47_so_pt4(a,j,k,l) * wm_interm_88_so_pt4(j,k,i,l)
term(1342) = term(1342) + t2(a,p,q,i) * wm_interm_49_so_pt4(a,j,k,l) * wm_interm_88_so_pt4(k,j,l,i)
term(1343) = term(1343) + t2(a,p,q,i) * wm_interm_49_so_pt4(a,j,k,l) * wm_interm_88_so_pt4(j,k,i,l)
term(1344) = term(1344) + t2(a,p,q,i) * wm_interm_49_so_pt4(a,j,k,l) * wm_interm_88_so_pt4(k,j,i,l)
term(1345) = term(1345) + t2(a,p,q,i) * wm_interm_109_so_pt4(j,k,l,i) * wm_interm_47_so_pt4(a,j,k,l)
term(1346) = term(1346) + t2(a,p,q,i) * wm_interm_109_so_pt4(j,k,i,l) * wm_interm_47_so_pt4(a,j,k,l)
term(1347) = term(1347) + t2(a,p,q,i) * wm_interm_109_so_pt4(j,k,i,l) * wm_interm_49_so_pt4(a,j,k,l)
term(1348) = term(1348) + t2(a,p,j,i) * wm_interm_22_so_pt4(a,k,l,q) * wm_interm_72_so_pt4(j,i,k,l)
term(1349) = term(1349) + t2(a,p,j,i) * wm_interm_22_so_pt4(a,k,l,q) * wm_interm_71_so_pt4(i,j,k,l)
term(1350) = term(1350) + t2(a,p,j,i) * wm_interm_22_so_pt4(a,k,l,q) * wm_interm_99_so_pt4(j,i,k,l)
term(1351) = term(1351) + t2(a,p,j,i) * wm_interm_22_so_pt4(a,k,l,q) * wm_interm_99_so_pt4(i,j,k,l)
end do 
end do 
end do 
end do 
end do 

term(1337) = term(1337) * (-1.0d+0) 
term(1338) = term(1338) * (2.0d+0) 
term(1339) = term(1339) * (-0.5d+0) 
term(1340) = term(1340) * (-0.5d+0) 
term(1342) = term(1342) * (-0.5d+0) 
term(1343) = term(1343) * (-0.5d+0) 
term(1345) = term(1345) * (-1.0d+0) 
term(1346) = term(1346) * (2.0d+0) 
term(1347) = term(1347) * (-1.0d+0) 
term(1348) = term(1348) * (-1.0d+0) 
term(1349) = term(1349) * (-1.0d+0) 
term(1350) = term(1350) * (4.0d+0) 
term(1351) = term(1351) * (-4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
term(1352) = term(1352) + wm_interm_24_so_pt4(p,i,j,q) * wm_interm_25_so_pt4(j,i)
term(1353) = term(1353) + wm_interm_24_so_pt4(p,i,q,j) * wm_interm_25_so_pt4(j,i)
term(1354) = term(1354) + wm_interm_21_so_pt4(i,j) * wm_interm_27_so_pt4(p,i,q,j)
term(1355) = term(1355) + wm_interm_21_so_pt4(i,j) * wm_interm_27_so_pt4(p,q,i,j)
term(1356) = term(1356) + wm_interm_21_so_pt4(i,j) * wm_interm_28_so_pt4(p,q,i,j)
term(1357) = term(1357) + wm_interm_21_so_pt4(i,j) * wm_interm_30_so_pt4(p,i,q,j)
term(1358) = term(1358) + wm_interm_21_so_pt4(i,j) * wm_interm_30_so_pt4(p,q,i,j)
term(1359) = term(1359) + wm_interm_21_so_pt4(i,j) * wm_interm_31_so_pt4(p,q,i,j)
term(1360) = term(1360) + r1(vrdav_Rr, p,i) * wm_interm_17_so_pt4(i,j) * wm_interm_21_so_pt4(j,q)
term(1361) = term(1361) + r1(vrdav_Rr, p,i) * wm_interm_18_so_pt4(i,j) * wm_interm_21_so_pt4(j,q)
term(1362) = term(1362) + t1(p,i) * wm_interm_17_so_pt4(i,j) * wm_interm_4_so_pt4(j,q)
term(1363) = term(1363) + t1(p,i) * wm_interm_18_so_pt4(i,j) * wm_interm_4_so_pt4(j,q)
term(1364) = term(1364) + r1(vrdav_Rr, p,i) * wm_interm_17_so_pt4(j,q) * wm_interm_21_so_pt4(i,j)
term(1365) = term(1365) + r1(vrdav_Rr, p,i) * wm_interm_18_so_pt4(j,q) * wm_interm_21_so_pt4(i,j)
term(1366) = term(1366) + t1(p,i) * wm_interm_17_so_pt4(j,q) * wm_interm_4_so_pt4(i,j)
term(1367) = term(1367) + t1(p,i) * wm_interm_18_so_pt4(j,q) * wm_interm_4_so_pt4(i,j)
term(1368) = term(1368) + t1(p,i) * wm_interm_17_so_pt4(i,j) * wm_interm_76_so_pt4(j,q)
term(1369) = term(1369) + t1(p,i) * wm_interm_18_so_pt4(i,j) * wm_interm_76_so_pt4(j,q)
term(1370) = term(1370) + t1(p,i) * wm_interm_17_so_pt4(i,j) * wm_interm_78_so_pt4(j,q)
term(1371) = term(1371) + t1(p,i) * wm_interm_18_so_pt4(i,j) * wm_interm_78_so_pt4(j,q)
term(1372) = term(1372) + t1(p,i) * wm_interm_17_so_pt4(i,j) * wm_interm_80_so_pt4(j,q)
term(1373) = term(1373) + t1(p,i) * wm_interm_18_so_pt4(i,j) * wm_interm_80_so_pt4(j,q)
term(1374) = term(1374) + t1(p,i) * wm_interm_60_so_pt4(i,j) * wm_interm_85_so_pt4(q,j)
term(1375) = term(1375) + t1(p,i) * wm_interm_60_so_pt4(i,j) * wm_interm_86_so_pt4(q,j)
term(1376) = term(1376) + t1(p,i) * wm_interm_60_so_pt4(i,j) * wm_interm_87_so_pt4(q,j)
term(1377) = term(1377) + t1(p,i) * wm_interm_62_so_pt4(i,j) * wm_interm_85_so_pt4(q,j)
term(1378) = term(1378) + t1(p,i) * wm_interm_62_so_pt4(i,j) * wm_interm_86_so_pt4(q,j)
term(1379) = term(1379) + t1(p,i) * wm_interm_62_so_pt4(i,j) * wm_interm_87_so_pt4(q,j)
term(1380) = term(1380) + t1(p,i) * wm_interm_101_so_pt4(j,q) * wm_interm_17_so_pt4(i,j)
term(1381) = term(1381) + t1(p,i) * wm_interm_101_so_pt4(j,q) * wm_interm_18_so_pt4(i,j)
term(1382) = term(1382) + t1(p,i) * wm_interm_103_so_pt4(j,q) * wm_interm_17_so_pt4(i,j)
term(1383) = term(1383) + t1(p,i) * wm_interm_103_so_pt4(j,q) * wm_interm_18_so_pt4(i,j)
term(1384) = term(1384) + t1(p,i) * wm_interm_106_so_pt4(q,j) * wm_interm_60_so_pt4(i,j)
term(1385) = term(1385) + t1(p,i) * wm_interm_107_so_pt4(q,j) * wm_interm_60_so_pt4(i,j)
term(1386) = term(1386) + t1(p,i) * wm_interm_108_so_pt4(q,j) * wm_interm_60_so_pt4(i,j)
term(1387) = term(1387) + t1(p,i) * wm_interm_106_so_pt4(q,j) * wm_interm_62_so_pt4(i,j)
term(1388) = term(1388) + t1(p,i) * wm_interm_107_so_pt4(q,j) * wm_interm_62_so_pt4(i,j)
term(1389) = term(1389) + t1(p,i) * wm_interm_108_so_pt4(q,j) * wm_interm_62_so_pt4(i,j)
term(1390) = term(1390) + t1(p,i) * wm_interm_17_so_pt4(j,q) * wm_interm_78_so_pt4(i,j)
term(1391) = term(1391) + t1(p,i) * wm_interm_17_so_pt4(j,q) * wm_interm_80_so_pt4(i,j)
term(1392) = term(1392) + t1(p,i) * wm_interm_17_so_pt4(j,q) * wm_interm_76_so_pt4(i,j)
term(1393) = term(1393) + t1(p,i) * wm_interm_18_so_pt4(j,q) * wm_interm_78_so_pt4(i,j)
term(1394) = term(1394) + t1(p,i) * wm_interm_18_so_pt4(j,q) * wm_interm_80_so_pt4(i,j)
term(1395) = term(1395) + t1(p,i) * wm_interm_18_so_pt4(j,q) * wm_interm_76_so_pt4(i,j)
term(1396) = term(1396) + t1(p,i) * wm_interm_101_so_pt4(i,j) * wm_interm_17_so_pt4(j,q)
term(1397) = term(1397) + t1(p,i) * wm_interm_103_so_pt4(i,j) * wm_interm_17_so_pt4(j,q)
term(1398) = term(1398) + t1(p,i) * wm_interm_101_so_pt4(i,j) * wm_interm_18_so_pt4(j,q)
term(1399) = term(1399) + t1(p,i) * wm_interm_103_so_pt4(i,j) * wm_interm_18_so_pt4(j,q)
end do 
end do 

term(1353) = term(1353) * (-2.0d+0) 
term(1355) = term(1355) * (-2.0d+0) 
term(1357) = term(1357) * (2.0d+0) 
term(1358) = term(1358) * (-4.0d+0) 
term(1359) = term(1359) * (2.0d+0) 
term(1360) = term(1360) * (2.0d+0) 
term(1361) = term(1361) * (-4.0d+0) 
term(1362) = term(1362) * (2.0d+0) 
term(1363) = term(1363) * (-4.0d+0) 
term(1364) = term(1364) * (2.0d+0) 
term(1365) = term(1365) * (-4.0d+0) 
term(1366) = term(1366) * (4.0d+0) 
term(1367) = term(1367) * (-8.0d+0) 
term(1368) = term(1368) * (-1.0d+0) 
term(1369) = term(1369) * (2.0d+0) 
term(1370) = term(1370) * (-1.0d+0) 
term(1371) = term(1371) * (2.0d+0) 
term(1372) = term(1372) * (2.0d+0) 
term(1373) = term(1373) * (-4.0d+0) 
term(1374) = term(1374) * (-1.0d+0) 
term(1375) = term(1375) * (2.0d+0) 
term(1376) = term(1376) * (-1.0d+0) 
term(1377) = term(1377) * (2.0d+0) 
term(1378) = term(1378) * (-4.0d+0) 
term(1379) = term(1379) * (2.0d+0) 
term(1380) = term(1380) * (-4.0d+0) 
term(1381) = term(1381) * (8.0d+0) 
term(1382) = term(1382) * (4.0d+0) 
term(1383) = term(1383) * (-8.0d+0) 
term(1384) = term(1384) * (-2.0d+0) 
term(1385) = term(1385) * (4.0d+0) 
term(1386) = term(1386) * (-2.0d+0) 
term(1387) = term(1387) * (4.0d+0) 
term(1388) = term(1388) * (-8.0d+0) 
term(1389) = term(1389) * (4.0d+0) 
term(1390) = term(1390) * (-2.0d+0) 
term(1391) = term(1391) * (4.0d+0) 
term(1392) = term(1392) * (-2.0d+0) 
term(1393) = term(1393) * (4.0d+0) 
term(1394) = term(1394) * (-8.0d+0) 
term(1395) = term(1395) * (4.0d+0) 
term(1396) = term(1396) * (-8.0d+0) 
term(1397) = term(1397) * (8.0d+0) 
term(1398) = term(1398) * (16.0d+0) 
term(1399) = term(1399) * (-16.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(1400) = term(1400) + r2m(vrdav_Rr, a,j,p,i) * wm_interm_22_so_pt4(a,k,l,q) * wm_interm_61_so_pt4(j,i,l,k)
term(1401) = term(1401) + r2m(vrdav_Rr, a,j,p,i) * wm_interm_22_so_pt4(a,k,l,q) * wm_interm_61_so_pt4(i,j,l,k)
term(1402) = term(1402) + t2(a,p,j,i) * wm_interm_22_so_pt4(a,k,l,q) * wm_interm_71_so_pt4(j,i,l,k)
term(1403) = term(1403) + t2(a,p,j,i) * wm_interm_22_so_pt4(a,k,l,q) * wm_interm_72_so_pt4(j,i,l,k)
term(1404) = term(1404) + t2(a,p,j,i) * wm_interm_22_so_pt4(a,k,l,q) * wm_interm_71_so_pt4(i,j,l,k)
term(1405) = term(1405) + t2(a,p,j,i) * wm_interm_22_so_pt4(a,k,l,q) * wm_interm_72_so_pt4(i,j,l,k)
term(1406) = term(1406) + t2(a,p,j,i) * wm_interm_22_so_pt4(a,k,l,q) * wm_interm_99_so_pt4(j,i,l,k)
term(1407) = term(1407) + t2(a,p,j,i) * wm_interm_22_so_pt4(a,k,l,q) * wm_interm_99_so_pt4(i,j,l,k)
end do 
end do 
end do 
end do 
end do 

term(1400) = term(1400) * (-1.0d+0) 
term(1401) = term(1401) * (2.0d+0) 
term(1402) = term(1402) * (-1.0d+0) 
term(1403) = term(1403) * (2.0d+0) 
term(1404) = term(1404) * (2.0d+0) 
term(1405) = term(1405) * (-1.0d+0) 
term(1406) = term(1406) * (-4.0d+0) 
term(1407) = term(1407) * (4.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
term(1408) = term(1408) + wm_interm_25_so_pt4(i,j) * wm_interm_26_so_pt4(p,j,q,i)
term(1409) = term(1409) + wm_interm_25_so_pt4(i,j) * wm_interm_29_so_pt4(p,j,q,i)
term(1410) = term(1410) + wm_interm_25_so_pt4(i,j) * wm_interm_29_so_pt4(p,j,i,q)
term(1411) = term(1411) + t1(p,i) * wm_interm_60_so_pt4(j,q) * wm_interm_85_so_pt4(j,i)
term(1412) = term(1412) + t1(p,i) * wm_interm_62_so_pt4(j,q) * wm_interm_85_so_pt4(j,i)
term(1413) = term(1413) + t1(p,i) * wm_interm_60_so_pt4(j,q) * wm_interm_86_so_pt4(j,i)
term(1414) = term(1414) + t1(p,i) * wm_interm_62_so_pt4(j,q) * wm_interm_86_so_pt4(j,i)
term(1415) = term(1415) + t1(p,i) * wm_interm_60_so_pt4(j,q) * wm_interm_87_so_pt4(j,i)
term(1416) = term(1416) + t1(p,i) * wm_interm_62_so_pt4(j,q) * wm_interm_87_so_pt4(j,i)
term(1417) = term(1417) + t1(p,i) * wm_interm_106_so_pt4(j,i) * wm_interm_60_so_pt4(j,q)
term(1418) = term(1418) + t1(p,i) * wm_interm_106_so_pt4(j,i) * wm_interm_62_so_pt4(j,q)
term(1419) = term(1419) + t1(p,i) * wm_interm_107_so_pt4(j,i) * wm_interm_60_so_pt4(j,q)
term(1420) = term(1420) + t1(p,i) * wm_interm_107_so_pt4(j,i) * wm_interm_62_so_pt4(j,q)
term(1421) = term(1421) + t1(p,i) * wm_interm_108_so_pt4(j,i) * wm_interm_60_so_pt4(j,q)
term(1422) = term(1422) + t1(p,i) * wm_interm_108_so_pt4(j,i) * wm_interm_62_so_pt4(j,q)
end do 
end do 

term(1409) = term(1409) * (4.0d+0) 
term(1410) = term(1410) * (-4.0d+0) 
term(1411) = term(1411) * (-2.0d+0) 
term(1412) = term(1412) * (4.0d+0) 
term(1413) = term(1413) * (4.0d+0) 
term(1414) = term(1414) * (-8.0d+0) 
term(1415) = term(1415) * (-2.0d+0) 
term(1416) = term(1416) * (4.0d+0) 
term(1417) = term(1417) * (-4.0d+0) 
term(1418) = term(1418) * (8.0d+0) 
term(1419) = term(1419) * (8.0d+0) 
term(1420) = term(1420) * (-16.0d+0) 
term(1421) = term(1421) * (-4.0d+0) 
term(1422) = term(1422) * (8.0d+0) 

do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
do i = 1, nocc 
term(1423) = term(1423) + r2p(vrdav_Rr, p,i,a,q) * wm_interm_19_so_pt4(a,b) * wm_interm_50_so_pt4(b,i)
term(1424) = term(1424) + r2p(vrdav_Rr, p,i,a,q) * wm_interm_20_so_pt4(a,b) * wm_interm_50_so_pt4(b,i)
term(1425) = term(1425) + r2p(vrdav_Rr, p,i,a,q) * wm_interm_19_so_pt4(a,b) * wm_interm_51_so_pt4(b,i)
term(1426) = term(1426) + r2p(vrdav_Rr, p,i,a,q) * wm_interm_20_so_pt4(a,b) * wm_interm_51_so_pt4(b,i)
end do 
end do 
end do 

term(1423) = term(1423) * (4.0d+0) 
term(1424) = term(1424) * (-8.0d+0) 
term(1425) = term(1425) * (-2.0d+0) 
term(1426) = term(1426) * (4.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(1427) = term(1427) + r2p(vrdav_Rr, p,q,a,i) * wm_interm_19_so_pt4(a,b) * wm_interm_50_so_pt4(b,i)
term(1428) = term(1428) + r2p(vrdav_Rr, p,q,a,i) * wm_interm_20_so_pt4(a,b) * wm_interm_50_so_pt4(b,i)
term(1429) = term(1429) + r2p(vrdav_Rr, p,q,a,i) * wm_interm_19_so_pt4(a,b) * wm_interm_51_so_pt4(b,i)
term(1430) = term(1430) + r2p(vrdav_Rr, p,q,a,i) * wm_interm_20_so_pt4(a,b) * wm_interm_51_so_pt4(b,i)
term(1431) = term(1431) + t1(a,i) * wm_interm_56_so_pt4(b,p,i,q) * wm_interm_81_so_pt4(b,a)
term(1432) = term(1432) + t1(a,i) * wm_interm_56_so_pt4(b,p,i,q) * wm_interm_82_so_pt4(b,a)
term(1433) = term(1433) + t1(a,i) * wm_interm_56_so_pt4(b,p,i,q) * wm_interm_83_so_pt4(b,a)
term(1434) = term(1434) + t1(a,i) * wm_interm_104_so_pt4(b,a) * wm_interm_56_so_pt4(b,p,i,q)
term(1435) = term(1435) + t1(a,i) * wm_interm_105_so_pt4(b,a) * wm_interm_56_so_pt4(b,p,i,q)
term(1436) = term(1436) + r2p(vrdav_Rr, b,q,a,i) * wm_interm_19_so_pt4(a,p) * wm_interm_50_so_pt4(b,i)
term(1437) = term(1437) + r2p(vrdav_Rr, b,q,a,i) * wm_interm_20_so_pt4(a,p) * wm_interm_50_so_pt4(b,i)
term(1438) = term(1438) + r2p(vrdav_Rr, b,q,a,i) * wm_interm_19_so_pt4(a,p) * wm_interm_51_so_pt4(b,i)
term(1439) = term(1439) + r2p(vrdav_Rr, b,q,a,i) * wm_interm_20_so_pt4(a,p) * wm_interm_51_so_pt4(b,i)
term(1440) = term(1440) + t2(a,p,q,i) * wm_interm_50_so_pt4(b,i) * wm_interm_83_so_pt4(b,a)
term(1441) = term(1441) + t2(a,p,q,i) * wm_interm_50_so_pt4(b,i) * wm_interm_81_so_pt4(b,a)
term(1442) = term(1442) + t2(a,p,q,i) * wm_interm_50_so_pt4(b,i) * wm_interm_82_so_pt4(b,a)
term(1443) = term(1443) + t2(a,p,q,i) * wm_interm_51_so_pt4(b,i) * wm_interm_83_so_pt4(b,a)
term(1444) = term(1444) + t2(a,p,q,i) * wm_interm_51_so_pt4(b,i) * wm_interm_81_so_pt4(b,a)
term(1445) = term(1445) + t2(a,p,q,i) * wm_interm_51_so_pt4(b,i) * wm_interm_82_so_pt4(b,a)
term(1446) = term(1446) + t2(a,p,q,i) * wm_interm_104_so_pt4(b,a) * wm_interm_50_so_pt4(b,i)
term(1447) = term(1447) + t2(a,p,q,i) * wm_interm_105_so_pt4(b,a) * wm_interm_50_so_pt4(b,i)
term(1448) = term(1448) + t2(a,p,q,i) * wm_interm_104_so_pt4(b,a) * wm_interm_51_so_pt4(b,i)
term(1449) = term(1449) + t2(a,p,q,i) * wm_interm_105_so_pt4(b,a) * wm_interm_51_so_pt4(b,i)
end do 
end do 
end do 

term(1427) = term(1427) * (-8.0d+0) 
term(1428) = term(1428) * (16.0d+0) 
term(1429) = term(1429) * (4.0d+0) 
term(1430) = term(1430) * (-8.0d+0) 
term(1431) = term(1431) * (-1.0d+0) 
term(1432) = term(1432) * (2.0d+0) 
term(1433) = term(1433) * (-1.0d+0) 
term(1434) = term(1434) * (-4.0d+0) 
term(1435) = term(1435) * (4.0d+0) 
term(1436) = term(1436) * (2.0d+0) 
term(1437) = term(1437) * (-4.0d+0) 
term(1438) = term(1438) * (-1.0d+0) 
term(1439) = term(1439) * (2.0d+0) 
term(1440) = term(1440) * (2.0d+0) 
term(1441) = term(1441) * (2.0d+0) 
term(1442) = term(1442) * (-4.0d+0) 
term(1443) = term(1443) * (-1.0d+0) 
term(1444) = term(1444) * (-1.0d+0) 
term(1445) = term(1445) * (2.0d+0) 
term(1446) = term(1446) * (8.0d+0) 
term(1447) = term(1447) * (-8.0d+0) 
term(1448) = term(1448) * (-4.0d+0) 
term(1449) = term(1449) * (4.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(1450) = term(1450) + t2(a,p,j,i) * wm_interm_22_so_pt4(b,j,i,k) * wm_interm_64_so_pt4(a,b,k,q)
term(1451) = term(1451) + t2(a,p,j,i) * wm_interm_22_so_pt4(b,j,i,k) * wm_interm_63_so_pt4(a,b,k,q)
term(1452) = term(1452) + t2(a,p,j,i) * wm_interm_22_so_pt4(b,j,i,k) * wm_interm_65_so_pt4(a,b,k,q)
term(1453) = term(1453) + t2(a,p,j,i) * wm_interm_22_so_pt4(b,j,i,k) * wm_interm_75_so_pt4(a,b,k,q)
term(1454) = term(1454) + t2(a,p,j,i) * wm_interm_22_so_pt4(b,j,i,k) * wm_interm_77_so_pt4(a,b,k,q)
term(1455) = term(1455) + t2(a,p,j,i) * wm_interm_22_so_pt4(b,j,i,k) * wm_interm_79_so_pt4(a,b,k,q)
term(1456) = term(1456) + r2m(vrdav_Rr, a,j,p,i) * wm_interm_47_so_pt4(b,j,i,k) * wm_interm_5_so_pt4(a,b,k,q)
term(1457) = term(1457) + r2m(vrdav_Rr, a,j,p,i) * wm_interm_1_so_pt4(a,b,k,q) * wm_interm_47_so_pt4(b,j,i,k)
term(1458) = term(1458) + r2m(vrdav_Rr, a,j,p,i) * wm_interm_10_so_pt4(a,b,k,q) * wm_interm_47_so_pt4(b,j,i,k)
term(1459) = term(1459) + r2m(vrdav_Rr, a,j,p,i) * wm_interm_11_so_pt4(a,b,k,q) * wm_interm_47_so_pt4(b,j,i,k)
term(1460) = term(1460) + r2m(vrdav_Rr, a,j,p,i) * wm_interm_10_so_pt4(a,b,k,q) * wm_interm_49_so_pt4(b,j,i,k)
term(1461) = term(1461) + r2m(vrdav_Rr, a,j,p,i) * wm_interm_49_so_pt4(b,j,i,k) * wm_interm_5_so_pt4(a,b,k,q)
term(1462) = term(1462) + r2m(vrdav_Rr, a,j,p,i) * wm_interm_1_so_pt4(a,b,k,q) * wm_interm_49_so_pt4(b,j,i,k)
term(1463) = term(1463) + r2m(vrdav_Rr, a,j,p,i) * wm_interm_11_so_pt4(a,b,k,q) * wm_interm_49_so_pt4(b,j,i,k)
term(1464) = term(1464) + t2(a,p,j,i) * wm_interm_22_so_pt4(b,j,i,k) * wm_interm_94_so_pt4(a,b,k,q)
term(1465) = term(1465) + t2(a,p,j,i) * wm_interm_22_so_pt4(b,j,i,k) * wm_interm_93_so_pt4(a,b,k,q)
term(1466) = term(1466) + t2(a,p,j,i) * wm_interm_100_so_pt4(a,b,k,q) * wm_interm_22_so_pt4(b,j,i,k)
term(1467) = term(1467) + t2(a,p,j,i) * wm_interm_102_so_pt4(a,b,k,q) * wm_interm_22_so_pt4(b,j,i,k)
end do 
end do 
end do 
end do 
end do 

term(1450) = term(1450) * (-1.0d+0) 
term(1451) = term(1451) * (2.0d+0) 
term(1452) = term(1452) * (-1.0d+0) 
term(1453) = term(1453) * (-1.0d+0) 
term(1454) = term(1454) * (-1.0d+0) 
term(1455) = term(1455) * (2.0d+0) 
term(1456) = term(1456) * (2.0d+0) 
term(1457) = term(1457) * (-1.0d+0) 
term(1458) = term(1458) * (-1.0d+0) 
term(1459) = term(1459) * (2.0d+0) 
term(1460) = term(1460) * (2.0d+0) 
term(1461) = term(1461) * (-1.0d+0) 
term(1462) = term(1462) * (2.0d+0) 
term(1463) = term(1463) * (-4.0d+0) 
term(1464) = term(1464) * (-4.0d+0) 
term(1465) = term(1465) * (4.0d+0) 
term(1466) = term(1466) * (-4.0d+0) 
term(1467) = term(1467) * (4.0d+0) 

do j = 1, nocc 
do a = nocc + 1, nactive 
do k = 1, nocc 
do i = 1, nocc 
term(1468) = term(1468) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_50_so_pt4(a,k) * wm_interm_9_so_pt4(i,j,k,q)
term(1469) = term(1469) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_50_so_pt4(a,k) * wm_interm_9_so_pt4(j,i,k,q)
term(1470) = term(1470) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_51_so_pt4(a,k) * wm_interm_9_so_pt4(i,j,k,q)
term(1471) = term(1471) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_51_so_pt4(a,k) * wm_interm_9_so_pt4(j,i,k,q)
end do 
end do 
end do 
end do 

term(1468) = term(1468) * (2.0d+0) 
term(1469) = term(1469) * (-4.0d+0) 
term(1470) = term(1470) * (-1.0d+0) 
term(1471) = term(1471) * (2.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do l = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(1472) = term(1472) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_22_so_pt4(a,k,l,q) * wm_interm_61_so_pt4(i,j,l,k)
term(1473) = term(1473) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_22_so_pt4(a,k,l,q) * wm_interm_61_so_pt4(i,j,l,k)
end do 
end do 
end do 
end do 
end do 

term(1472) = term(1472) * (-0.5d+0) 
term(1473) = term(1473) * (-1.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(1474) = term(1474) + t1(p,i) * wm_interm_61_so_pt4(j,i,k,l) * wm_interm_88_so_pt4(j,q,l,k)
term(1475) = term(1475) + t1(p,i) * wm_interm_109_so_pt4(j,q,k,l) * wm_interm_61_so_pt4(j,i,l,k)
end do 
end do 
end do 
end do 

term(1474) = term(1474) * (-0.5d+0) 
term(1475) = term(1475) * (-1.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
term(1476) = term(1476) + t1(p,i) * wm_interm_60_so_pt4(j,k) * wm_interm_88_so_pt4(q,j,k,i)
term(1477) = term(1477) + t1(p,i) * wm_interm_62_so_pt4(j,k) * wm_interm_88_so_pt4(q,j,k,i)
term(1478) = term(1478) + t1(p,i) * wm_interm_109_so_pt4(q,j,k,i) * wm_interm_60_so_pt4(j,k)
term(1479) = term(1479) + t1(p,i) * wm_interm_109_so_pt4(q,j,k,i) * wm_interm_62_so_pt4(j,k)
end do 
end do 
end do 

term(1476) = term(1476) * (-1.0d+0) 
term(1477) = term(1477) * (2.0d+0) 
term(1478) = term(1478) * (-2.0d+0) 
term(1479) = term(1479) * (4.0d+0) 

do a = nocc + 1, nactive 
do j = 1, nocc 
do i = 1, nocc 
term(1480) = term(1480) + r2p(vrdav_Rr, p,i,a,q) * wm_interm_17_so_pt4(i,j) * wm_interm_50_so_pt4(a,j)
term(1481) = term(1481) + r2p(vrdav_Rr, p,i,a,q) * wm_interm_18_so_pt4(i,j) * wm_interm_50_so_pt4(a,j)
term(1482) = term(1482) + r2p(vrdav_Rr, p,i,a,q) * wm_interm_17_so_pt4(i,j) * wm_interm_51_so_pt4(a,j)
term(1483) = term(1483) + r2p(vrdav_Rr, p,i,a,q) * wm_interm_18_so_pt4(i,j) * wm_interm_51_so_pt4(a,j)
end do 
end do 
end do 

term(1480) = term(1480) * (4.0d+0) 
term(1481) = term(1481) * (-8.0d+0) 
term(1482) = term(1482) * (-2.0d+0) 
term(1483) = term(1483) * (4.0d+0) 

do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(1484) = term(1484) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_50_so_pt4(a,k) * wm_interm_9_so_pt4(j,i,k,q)
term(1485) = term(1485) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_51_so_pt4(a,k) * wm_interm_9_so_pt4(j,i,k,q)
term(1486) = term(1486) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_50_so_pt4(a,k) * wm_interm_9_so_pt4(j,i,k,q)
term(1487) = term(1487) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_50_so_pt4(a,k) * wm_interm_9_so_pt4(i,j,k,q)
term(1488) = term(1488) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_51_so_pt4(a,k) * wm_interm_9_so_pt4(j,i,k,q)
term(1489) = term(1489) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_51_so_pt4(a,k) * wm_interm_9_so_pt4(i,j,k,q)
end do 
end do 
end do 
end do 

term(1484) = term(1484) * (2.0d+0) 
term(1485) = term(1485) * (-1.0d+0) 
term(1486) = term(1486) * (8.0d+0) 
term(1487) = term(1487) * (-8.0d+0) 
term(1488) = term(1488) * (-4.0d+0) 
term(1489) = term(1489) * (4.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
do j = 1, nocc 
do i = 1, nocc 
term(1490) = term(1490) + r2p(vrdav_Rr, p,i,a,q) * wm_interm_22_so_pt4(a,j,k,l) * wm_interm_61_so_pt4(i,l,j,k)
term(1491) = term(1491) + r2p(vrdav_Rr, p,i,a,q) * wm_interm_47_so_pt4(a,j,k,l) * wm_interm_9_so_pt4(l,i,j,k)
term(1492) = term(1492) + r2p(vrdav_Rr, p,i,a,q) * wm_interm_47_so_pt4(a,j,k,l) * wm_interm_9_so_pt4(i,l,j,k)
term(1493) = term(1493) + r2p(vrdav_Rr, p,i,a,q) * wm_interm_22_so_pt4(a,j,k,l) * wm_interm_61_so_pt4(l,i,j,k)
term(1494) = term(1494) + r2p(vrdav_Rr, p,i,a,q) * wm_interm_49_so_pt4(a,j,k,l) * wm_interm_9_so_pt4(i,l,j,k)
term(1495) = term(1495) + r2p(vrdav_Rr, p,i,a,q) * wm_interm_49_so_pt4(a,j,k,l) * wm_interm_9_so_pt4(l,i,j,k)
end do 
end do 
end do 
end do 
end do 

term(1490) = term(1490) * (2.0d+0) 
term(1491) = term(1491) * (-1.0d+0) 
term(1492) = term(1492) * (2.0d+0) 
term(1493) = term(1493) * (-1.0d+0) 
term(1494) = term(1494) * (-1.0d+0) 
term(1495) = term(1495) * (2.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(1496) = term(1496) + r2m(vrdav_Rr, a,j,p,i) * wm_interm_19_so_pt4(a,b) * wm_interm_49_so_pt4(b,j,i,q)
term(1497) = term(1497) + r2m(vrdav_Rr, a,j,p,i) * wm_interm_20_so_pt4(a,b) * wm_interm_49_so_pt4(b,j,i,q)
term(1498) = term(1498) + r2m(vrdav_Rr, a,j,p,i) * wm_interm_19_so_pt4(a,b) * wm_interm_47_so_pt4(b,j,i,q)
term(1499) = term(1499) + r2m(vrdav_Rr, a,j,p,i) * wm_interm_20_so_pt4(a,b) * wm_interm_47_so_pt4(b,j,i,q)
term(1500) = term(1500) + t2(a,b,q,i) * wm_interm_50_so_pt4(a,j) * wm_interm_70_so_pt4(p,b,j,i)
term(1501) = term(1501) + t2(a,b,q,i) * wm_interm_50_so_pt4(a,j) * wm_interm_69_so_pt4(p,b,j,i)
term(1502) = term(1502) + t2(a,b,q,i) * wm_interm_50_so_pt4(a,j) * wm_interm_67_so_pt4(p,b,j,i)
term(1503) = term(1503) + t2(a,b,q,i) * wm_interm_50_so_pt4(a,j) * wm_interm_68_so_pt4(p,b,j,i)
term(1504) = term(1504) + t2(a,b,q,i) * wm_interm_50_so_pt4(a,j) * wm_interm_84_so_pt4(p,b,j,i)
term(1505) = term(1505) + t2(a,b,q,i) * wm_interm_50_so_pt4(a,j) * wm_interm_74_so_pt4(p,b,j,i)
term(1506) = term(1506) + t2(a,b,q,i) * wm_interm_51_so_pt4(a,j) * wm_interm_70_so_pt4(p,b,j,i)
term(1507) = term(1507) + t2(a,b,q,i) * wm_interm_51_so_pt4(a,j) * wm_interm_69_so_pt4(p,b,j,i)
term(1508) = term(1508) + t2(a,b,q,i) * wm_interm_51_so_pt4(a,j) * wm_interm_67_so_pt4(p,b,j,i)
term(1509) = term(1509) + t2(a,b,q,i) * wm_interm_51_so_pt4(a,j) * wm_interm_68_so_pt4(p,b,j,i)
term(1510) = term(1510) + t2(a,b,q,i) * wm_interm_51_so_pt4(a,j) * wm_interm_84_so_pt4(p,b,j,i)
term(1511) = term(1511) + t2(a,b,q,i) * wm_interm_51_so_pt4(a,j) * wm_interm_74_so_pt4(p,b,j,i)
term(1512) = term(1512) + t2(a,b,q,i) * wm_interm_50_so_pt4(b,j) * wm_interm_67_so_pt4(p,a,j,i)
term(1513) = term(1513) + t2(a,b,q,i) * wm_interm_50_so_pt4(b,j) * wm_interm_68_so_pt4(p,a,j,i)
term(1514) = term(1514) + t2(a,b,q,i) * wm_interm_50_so_pt4(b,j) * wm_interm_69_so_pt4(p,a,j,i)
term(1515) = term(1515) + t2(a,b,q,i) * wm_interm_50_so_pt4(b,j) * wm_interm_70_so_pt4(p,a,j,i)
term(1516) = term(1516) + t2(a,b,q,i) * wm_interm_50_so_pt4(b,j) * wm_interm_73_so_pt4(p,a,j,i)
term(1517) = term(1517) + t2(a,b,q,i) * wm_interm_50_so_pt4(b,j) * wm_interm_74_so_pt4(p,a,j,i)
term(1518) = term(1518) + t2(a,b,q,i) * wm_interm_51_so_pt4(b,j) * wm_interm_67_so_pt4(p,a,j,i)
term(1519) = term(1519) + t2(a,b,q,i) * wm_interm_51_so_pt4(b,j) * wm_interm_68_so_pt4(p,a,j,i)
term(1520) = term(1520) + t2(a,b,q,i) * wm_interm_51_so_pt4(b,j) * wm_interm_69_so_pt4(p,a,j,i)
term(1521) = term(1521) + t2(a,b,q,i) * wm_interm_51_so_pt4(b,j) * wm_interm_70_so_pt4(p,a,j,i)
term(1522) = term(1522) + t2(a,b,q,i) * wm_interm_51_so_pt4(b,j) * wm_interm_73_so_pt4(p,a,j,i)
term(1523) = term(1523) + t2(a,b,q,i) * wm_interm_51_so_pt4(b,j) * wm_interm_74_so_pt4(p,a,j,i)
term(1524) = term(1524) + t2(a,b,q,i) * wm_interm_50_so_pt4(a,j) * wm_interm_98_so_pt4(p,b,j,i)
term(1525) = term(1525) + t2(a,b,q,i) * wm_interm_50_so_pt4(a,j) * wm_interm_97_so_pt4(p,b,j,i)
term(1526) = term(1526) + t2(a,b,q,i) * wm_interm_50_so_pt4(a,j) * wm_interm_95_so_pt4(p,b,j,i)
term(1527) = term(1527) + t2(a,b,q,i) * wm_interm_50_so_pt4(a,j) * wm_interm_96_so_pt4(p,b,j,i)
term(1528) = term(1528) + t2(a,b,q,i) * wm_interm_51_so_pt4(a,j) * wm_interm_98_so_pt4(p,b,j,i)
term(1529) = term(1529) + t2(a,b,q,i) * wm_interm_51_so_pt4(a,j) * wm_interm_97_so_pt4(p,b,j,i)
term(1530) = term(1530) + t2(a,b,q,i) * wm_interm_51_so_pt4(a,j) * wm_interm_95_so_pt4(p,b,j,i)
term(1531) = term(1531) + t2(a,b,q,i) * wm_interm_51_so_pt4(a,j) * wm_interm_96_so_pt4(p,b,j,i)
term(1532) = term(1532) + t2(a,b,q,i) * wm_interm_50_so_pt4(b,j) * wm_interm_95_so_pt4(p,a,j,i)
term(1533) = term(1533) + t2(a,b,q,i) * wm_interm_50_so_pt4(b,j) * wm_interm_96_so_pt4(p,a,j,i)
term(1534) = term(1534) + t2(a,b,q,i) * wm_interm_50_so_pt4(b,j) * wm_interm_97_so_pt4(p,a,j,i)
term(1535) = term(1535) + t2(a,b,q,i) * wm_interm_50_so_pt4(b,j) * wm_interm_98_so_pt4(p,a,j,i)
term(1536) = term(1536) + t2(a,b,q,i) * wm_interm_51_so_pt4(b,j) * wm_interm_95_so_pt4(p,a,j,i)
term(1537) = term(1537) + t2(a,b,q,i) * wm_interm_51_so_pt4(b,j) * wm_interm_96_so_pt4(p,a,j,i)
term(1538) = term(1538) + t2(a,b,q,i) * wm_interm_51_so_pt4(b,j) * wm_interm_97_so_pt4(p,a,j,i)
term(1539) = term(1539) + t2(a,b,q,i) * wm_interm_51_so_pt4(b,j) * wm_interm_98_so_pt4(p,a,j,i)
end do 
end do 
end do 
end do 

term(1496) = term(1496) * (-1.0d+0) 
term(1497) = term(1497) * (2.0d+0) 
term(1498) = term(1498) * (2.0d+0) 
term(1499) = term(1499) * (-4.0d+0) 
term(1500) = term(1500) * (2.0d+0) 
term(1501) = term(1501) * (-4.0d+0) 
term(1502) = term(1502) * (-4.0d+0) 
term(1503) = term(1503) * (8.0d+0) 
term(1504) = term(1504) * (2.0d+0) 
term(1505) = term(1505) * (-4.0d+0) 
term(1506) = term(1506) * (-1.0d+0) 
term(1507) = term(1507) * (2.0d+0) 
term(1508) = term(1508) * (2.0d+0) 
term(1509) = term(1509) * (-4.0d+0) 
term(1510) = term(1510) * (-1.0d+0) 
term(1511) = term(1511) * (2.0d+0) 
term(1512) = term(1512) * (2.0d+0) 
term(1513) = term(1513) * (-4.0d+0) 
term(1514) = term(1514) * (2.0d+0) 
term(1515) = term(1515) * (-4.0d+0) 
term(1516) = term(1516) * (2.0d+0) 
term(1517) = term(1517) * (2.0d+0) 
term(1518) = term(1518) * (-1.0d+0) 
term(1519) = term(1519) * (2.0d+0) 
term(1520) = term(1520) * (-1.0d+0) 
term(1521) = term(1521) * (2.0d+0) 
term(1522) = term(1522) * (-1.0d+0) 
term(1523) = term(1523) * (-1.0d+0) 
term(1524) = term(1524) * (8.0d+0) 
term(1525) = term(1525) * (-8.0d+0) 
term(1526) = term(1526) * (-16.0d+0) 
term(1527) = term(1527) * (16.0d+0) 
term(1528) = term(1528) * (-4.0d+0) 
term(1529) = term(1529) * (4.0d+0) 
term(1530) = term(1530) * (8.0d+0) 
term(1531) = term(1531) * (-8.0d+0) 
term(1532) = term(1532) * (8.0d+0) 
term(1533) = term(1533) * (-8.0d+0) 
term(1534) = term(1534) * (8.0d+0) 
term(1535) = term(1535) * (-8.0d+0) 
term(1536) = term(1536) * (-4.0d+0) 
term(1537) = term(1537) * (4.0d+0) 
term(1538) = term(1538) * (-4.0d+0) 
term(1539) = term(1539) * (4.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(1540) = term(1540) + r2m(vrdav_Rr, a,j,p,i) * wm_interm_19_so_pt4(a,b) * wm_interm_49_so_pt4(b,i,j,q)
term(1541) = term(1541) + r2m(vrdav_Rr, a,j,p,i) * wm_interm_20_so_pt4(a,b) * wm_interm_49_so_pt4(b,i,j,q)
term(1542) = term(1542) + r2m(vrdav_Rr, a,j,p,i) * wm_interm_19_so_pt4(a,b) * wm_interm_47_so_pt4(b,i,j,q)
term(1543) = term(1543) + r2m(vrdav_Rr, a,j,p,i) * wm_interm_20_so_pt4(a,b) * wm_interm_47_so_pt4(b,i,j,q)
term(1544) = term(1544) + t1(a,i) * wm_interm_55_so_pt4(a,b,j,q) * wm_interm_67_so_pt4(p,b,j,i)
term(1545) = term(1545) + t1(a,i) * wm_interm_55_so_pt4(a,b,j,q) * wm_interm_68_so_pt4(p,b,j,i)
term(1546) = term(1546) + t1(a,i) * wm_interm_55_so_pt4(a,b,j,q) * wm_interm_69_so_pt4(p,b,j,i)
term(1547) = term(1547) + t1(a,i) * wm_interm_55_so_pt4(a,b,j,q) * wm_interm_70_so_pt4(p,b,j,i)
term(1548) = term(1548) + t1(a,i) * wm_interm_54_so_pt4(a,b,j,q) * wm_interm_67_so_pt4(p,b,j,i)
term(1549) = term(1549) + t1(a,i) * wm_interm_54_so_pt4(a,b,j,q) * wm_interm_68_so_pt4(p,b,j,i)
term(1550) = term(1550) + t1(a,i) * wm_interm_54_so_pt4(a,b,j,q) * wm_interm_69_so_pt4(p,b,j,i)
term(1551) = term(1551) + t1(a,i) * wm_interm_54_so_pt4(a,b,j,q) * wm_interm_70_so_pt4(p,b,j,i)
term(1552) = term(1552) + t1(a,i) * wm_interm_59_so_pt4(a,b,j,q) * wm_interm_67_so_pt4(p,b,j,i)
term(1553) = term(1553) + t1(a,i) * wm_interm_59_so_pt4(a,b,j,q) * wm_interm_68_so_pt4(p,b,j,i)
term(1554) = term(1554) + t1(a,i) * wm_interm_59_so_pt4(a,b,j,q) * wm_interm_69_so_pt4(p,b,j,i)
term(1555) = term(1555) + t1(a,i) * wm_interm_59_so_pt4(a,b,j,q) * wm_interm_70_so_pt4(p,b,j,i)
term(1556) = term(1556) + t1(a,i) * wm_interm_56_so_pt4(a,b,j,q) * wm_interm_67_so_pt4(p,b,j,i)
term(1557) = term(1557) + t1(a,i) * wm_interm_56_so_pt4(a,b,j,q) * wm_interm_68_so_pt4(p,b,j,i)
term(1558) = term(1558) + t1(a,i) * wm_interm_56_so_pt4(a,b,j,q) * wm_interm_70_so_pt4(p,b,j,i)
term(1559) = term(1559) + t1(a,i) * wm_interm_56_so_pt4(a,b,j,q) * wm_interm_69_so_pt4(p,b,j,i)
term(1560) = term(1560) + t1(a,i) * wm_interm_55_so_pt4(a,b,j,q) * wm_interm_73_so_pt4(p,b,j,i)
term(1561) = term(1561) + t1(a,i) * wm_interm_55_so_pt4(a,b,j,q) * wm_interm_74_so_pt4(p,b,j,i)
term(1562) = term(1562) + t1(a,i) * wm_interm_54_so_pt4(a,b,j,q) * wm_interm_73_so_pt4(p,b,j,i)
term(1563) = term(1563) + t1(a,i) * wm_interm_54_so_pt4(a,b,j,q) * wm_interm_74_so_pt4(p,b,j,i)
term(1564) = term(1564) + t1(a,i) * wm_interm_59_so_pt4(a,b,j,q) * wm_interm_73_so_pt4(p,b,j,i)
term(1565) = term(1565) + t1(a,i) * wm_interm_59_so_pt4(a,b,j,q) * wm_interm_74_so_pt4(p,b,j,i)
term(1566) = term(1566) + t1(a,i) * wm_interm_56_so_pt4(a,b,j,q) * wm_interm_74_so_pt4(p,b,j,i)
term(1567) = term(1567) + t1(a,i) * wm_interm_56_so_pt4(a,b,j,q) * wm_interm_84_so_pt4(p,b,j,i)
term(1568) = term(1568) + t1(a,i) * wm_interm_55_so_pt4(a,b,j,q) * wm_interm_95_so_pt4(p,b,j,i)
term(1569) = term(1569) + t1(a,i) * wm_interm_55_so_pt4(a,b,j,q) * wm_interm_96_so_pt4(p,b,j,i)
term(1570) = term(1570) + t1(a,i) * wm_interm_55_so_pt4(a,b,j,q) * wm_interm_97_so_pt4(p,b,j,i)
term(1571) = term(1571) + t1(a,i) * wm_interm_55_so_pt4(a,b,j,q) * wm_interm_98_so_pt4(p,b,j,i)
term(1572) = term(1572) + t1(a,i) * wm_interm_54_so_pt4(a,b,j,q) * wm_interm_95_so_pt4(p,b,j,i)
term(1573) = term(1573) + t1(a,i) * wm_interm_54_so_pt4(a,b,j,q) * wm_interm_96_so_pt4(p,b,j,i)
term(1574) = term(1574) + t1(a,i) * wm_interm_54_so_pt4(a,b,j,q) * wm_interm_97_so_pt4(p,b,j,i)
term(1575) = term(1575) + t1(a,i) * wm_interm_54_so_pt4(a,b,j,q) * wm_interm_98_so_pt4(p,b,j,i)
term(1576) = term(1576) + t1(a,i) * wm_interm_59_so_pt4(a,b,j,q) * wm_interm_95_so_pt4(p,b,j,i)
term(1577) = term(1577) + t1(a,i) * wm_interm_59_so_pt4(a,b,j,q) * wm_interm_96_so_pt4(p,b,j,i)
term(1578) = term(1578) + t1(a,i) * wm_interm_59_so_pt4(a,b,j,q) * wm_interm_97_so_pt4(p,b,j,i)
term(1579) = term(1579) + t1(a,i) * wm_interm_59_so_pt4(a,b,j,q) * wm_interm_98_so_pt4(p,b,j,i)
term(1580) = term(1580) + t1(a,i) * wm_interm_56_so_pt4(a,b,j,q) * wm_interm_95_so_pt4(p,b,j,i)
term(1581) = term(1581) + t1(a,i) * wm_interm_56_so_pt4(a,b,j,q) * wm_interm_96_so_pt4(p,b,j,i)
term(1582) = term(1582) + t1(a,i) * wm_interm_56_so_pt4(a,b,j,q) * wm_interm_98_so_pt4(p,b,j,i)
term(1583) = term(1583) + t1(a,i) * wm_interm_56_so_pt4(a,b,j,q) * wm_interm_97_so_pt4(p,b,j,i)
term(1584) = term(1584) + t2(a,p,j,i) * wm_interm_22_so_pt4(b,i,j,q) * wm_interm_91_so_pt4(a,b)
term(1585) = term(1585) + t2(a,p,j,i) * wm_interm_22_so_pt4(b,i,j,q) * wm_interm_92_so_pt4(a,b)
term(1586) = term(1586) + t2(a,p,j,i) * wm_interm_22_so_pt4(b,i,j,q) * wm_interm_90_so_pt4(a,b)
term(1587) = term(1587) + t1(p,i) * wm_interm_59_so_pt4(a,b,j,q) * wm_interm_84_so_pt4(a,b,j,i)
term(1588) = term(1588) + t1(p,i) * wm_interm_59_so_pt4(a,b,j,q) * wm_interm_73_so_pt4(a,b,j,i)
term(1589) = term(1589) + t1(p,i) * wm_interm_56_so_pt4(a,b,j,q) * wm_interm_73_so_pt4(a,b,j,i)
term(1590) = term(1590) + t1(p,i) * wm_interm_56_so_pt4(a,b,j,q) * wm_interm_84_so_pt4(a,b,j,i)
term(1591) = term(1591) + t1(p,i) * wm_interm_56_so_pt4(a,b,j,q) * wm_interm_69_so_pt4(a,b,j,i)
term(1592) = term(1592) + t1(p,i) * wm_interm_59_so_pt4(a,b,j,q) * wm_interm_70_so_pt4(a,b,j,i)
term(1593) = term(1593) + t1(p,i) * wm_interm_56_so_pt4(a,b,j,q) * wm_interm_74_so_pt4(a,b,j,i)
term(1594) = term(1594) + t1(p,i) * wm_interm_56_so_pt4(a,b,j,q) * wm_interm_67_so_pt4(a,b,j,i)
term(1595) = term(1595) + t1(p,i) * wm_interm_56_so_pt4(a,b,j,q) * wm_interm_68_so_pt4(a,b,j,i)
term(1596) = term(1596) + t1(p,i) * wm_interm_55_so_pt4(a,b,j,q) * wm_interm_84_so_pt4(a,b,j,i)
term(1597) = term(1597) + t1(p,i) * wm_interm_55_so_pt4(a,b,j,q) * wm_interm_73_so_pt4(a,b,j,i)
term(1598) = term(1598) + t1(p,i) * wm_interm_54_so_pt4(a,b,j,q) * wm_interm_84_so_pt4(a,b,j,i)
term(1599) = term(1599) + t1(p,i) * wm_interm_54_so_pt4(a,b,j,q) * wm_interm_73_so_pt4(a,b,j,i)
term(1600) = term(1600) + t1(p,i) * wm_interm_55_so_pt4(a,b,j,q) * wm_interm_70_so_pt4(a,b,j,i)
term(1601) = term(1601) + t1(p,i) * wm_interm_54_so_pt4(a,b,j,q) * wm_interm_70_so_pt4(a,b,j,i)
term(1602) = term(1602) + t2(a,p,j,i) * wm_interm_110_so_pt4(a,b) * wm_interm_22_so_pt4(b,i,j,q)
term(1603) = term(1603) + t2(a,p,j,i) * wm_interm_111_so_pt4(a,b) * wm_interm_22_so_pt4(b,i,j,q)
term(1604) = term(1604) + t1(p,i) * wm_interm_59_so_pt4(a,b,j,q) * wm_interm_98_so_pt4(a,b,j,i)
term(1605) = term(1605) + t1(p,i) * wm_interm_59_so_pt4(a,b,j,q) * wm_interm_97_so_pt4(a,b,j,i)
term(1606) = term(1606) + t1(p,i) * wm_interm_56_so_pt4(a,b,j,q) * wm_interm_97_so_pt4(a,b,j,i)
term(1607) = term(1607) + t1(p,i) * wm_interm_56_so_pt4(a,b,j,q) * wm_interm_98_so_pt4(a,b,j,i)
term(1608) = term(1608) + t1(p,i) * wm_interm_56_so_pt4(a,b,j,q) * wm_interm_95_so_pt4(a,b,j,i)
term(1609) = term(1609) + t1(p,i) * wm_interm_56_so_pt4(a,b,j,q) * wm_interm_96_so_pt4(a,b,j,i)
term(1610) = term(1610) + t1(p,i) * wm_interm_55_so_pt4(a,b,j,q) * wm_interm_98_so_pt4(a,b,j,i)
term(1611) = term(1611) + t1(p,i) * wm_interm_55_so_pt4(a,b,j,q) * wm_interm_97_so_pt4(a,b,j,i)
term(1612) = term(1612) + t1(p,i) * wm_interm_54_so_pt4(a,b,j,q) * wm_interm_98_so_pt4(a,b,j,i)
term(1613) = term(1613) + t1(p,i) * wm_interm_54_so_pt4(a,b,j,q) * wm_interm_97_so_pt4(a,b,j,i)
end do 
end do 
end do 
end do 

term(1540) = term(1540) * (2.0d+0) 
term(1541) = term(1541) * (-4.0d+0) 
term(1542) = term(1542) * (-1.0d+0) 
term(1543) = term(1543) * (2.0d+0) 
term(1544) = term(1544) * (-1.0d+0) 
term(1545) = term(1545) * (2.0d+0) 
term(1546) = term(1546) * (-1.0d+0) 
term(1547) = term(1547) * (2.0d+0) 
term(1548) = term(1548) * (2.0d+0) 
term(1549) = term(1549) * (-4.0d+0) 
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
term(1560) = term(1560) * (-1.0d+0) 
term(1561) = term(1561) * (-1.0d+0) 
term(1562) = term(1562) * (2.0d+0) 
term(1563) = term(1563) * (2.0d+0) 
term(1564) = term(1564) * (-1.0d+0) 
term(1565) = term(1565) * (-1.0d+0) 
term(1566) = term(1566) * (2.0d+0) 
term(1567) = term(1567) * (-1.0d+0) 
term(1568) = term(1568) * (-4.0d+0) 
term(1569) = term(1569) * (4.0d+0) 
term(1570) = term(1570) * (-4.0d+0) 
term(1571) = term(1571) * (4.0d+0) 
term(1572) = term(1572) * (8.0d+0) 
term(1573) = term(1573) * (-8.0d+0) 
term(1574) = term(1574) * (8.0d+0) 
term(1575) = term(1575) * (-8.0d+0) 
term(1576) = term(1576) * (-4.0d+0) 
term(1577) = term(1577) * (4.0d+0) 
term(1578) = term(1578) * (-4.0d+0) 
term(1579) = term(1579) * (4.0d+0) 
term(1580) = term(1580) * (8.0d+0) 
term(1581) = term(1581) * (-8.0d+0) 
term(1582) = term(1582) * (-4.0d+0) 
term(1583) = term(1583) * (4.0d+0) 
term(1584) = term(1584) * (-2.0d+0) 
term(1585) = term(1585) * (4.0d+0) 
term(1586) = term(1586) * (-2.0d+0) 
term(1587) = term(1587) * (-2.0d+0) 
term(1588) = term(1588) * (4.0d+0) 
term(1589) = term(1589) * (-2.0d+0) 
term(1590) = term(1590) * (4.0d+0) 
term(1591) = term(1591) * (-2.0d+0) 
term(1592) = term(1592) * (-2.0d+0) 
term(1593) = term(1593) * (-2.0d+0) 
term(1594) = term(1594) * (-2.0d+0) 
term(1595) = term(1595) * (4.0d+0) 
term(1596) = term(1596) * (-2.0d+0) 
term(1597) = term(1597) * (4.0d+0) 
term(1598) = term(1598) * (4.0d+0) 
term(1599) = term(1599) * (-8.0d+0) 
term(1600) = term(1600) * (-2.0d+0) 
term(1601) = term(1601) * (4.0d+0) 
term(1602) = term(1602) * (-8.0d+0) 
term(1603) = term(1603) * (8.0d+0) 
term(1604) = term(1604) * (-8.0d+0) 
term(1605) = term(1605) * (8.0d+0) 
term(1606) = term(1606) * (-8.0d+0) 
term(1607) = term(1607) * (8.0d+0) 
term(1608) = term(1608) * (-8.0d+0) 
term(1609) = term(1609) * (8.0d+0) 
term(1610) = term(1610) * (-8.0d+0) 
term(1611) = term(1611) * (8.0d+0) 
term(1612) = term(1612) * (16.0d+0) 
term(1613) = term(1613) * (-16.0d+0) 

do j = 1, nocc 
do a = nocc + 1, nactive 
do i = 1, nocc 
do b = nocc + 1, nactive 
term(1614) = term(1614) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_19_so_pt4(a,b) * wm_interm_49_so_pt4(b,j,i,q)
term(1615) = term(1615) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_20_so_pt4(a,b) * wm_interm_49_so_pt4(b,j,i,q)
term(1616) = term(1616) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_19_so_pt4(a,b) * wm_interm_47_so_pt4(b,j,i,q)
term(1617) = term(1617) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_20_so_pt4(a,b) * wm_interm_47_so_pt4(b,j,i,q)
term(1618) = term(1618) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_22_so_pt4(b,j,i,q) * wm_interm_57_so_pt4(a,b)
term(1619) = term(1619) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_22_so_pt4(b,j,i,q) * wm_interm_58_so_pt4(a,b)
end do 
end do 
end do 
end do 

term(1614) = term(1614) * (-0.5d+0) 
term(1617) = term(1617) * (-2.0d+0) 
term(1618) = term(1618) * (-4.0d+0) 
term(1619) = term(1619) * (2.0d+0) 

do j = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
do i = 1, nocc 
term(1620) = term(1620) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_19_so_pt4(a,b) * wm_interm_49_so_pt4(b,i,j,q)
term(1621) = term(1621) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_20_so_pt4(a,b) * wm_interm_49_so_pt4(b,i,j,q)
term(1622) = term(1622) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_19_so_pt4(a,b) * wm_interm_47_so_pt4(b,i,j,q)
term(1623) = term(1623) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_20_so_pt4(a,b) * wm_interm_47_so_pt4(b,i,j,q)
term(1624) = term(1624) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_22_so_pt4(b,i,j,q) * wm_interm_57_so_pt4(a,b)
term(1625) = term(1625) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_22_so_pt4(b,i,j,q) * wm_interm_58_so_pt4(a,b)
end do 
end do 
end do 
end do 

term(1621) = term(1621) * (-2.0d+0) 
term(1622) = term(1622) * (-0.5d+0) 
term(1624) = term(1624) * (2.0d+0) 
term(1625) = term(1625) * (-1.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
do a = nocc + 1, nactive 
term(1626) = term(1626) + t2(a,p,j,i) * wm_interm_109_so_pt4(k,l,i,j) * wm_interm_49_so_pt4(a,l,k,q)
term(1627) = term(1627) + t2(a,p,j,i) * wm_interm_109_so_pt4(k,l,i,j) * wm_interm_47_so_pt4(a,l,k,q)
end do 
end do 
end do 
end do 
end do 

term(1626) = term(1626) * (-2.0d+0) 
term(1627) = term(1627) * (4.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(1628) = term(1628) + t2(a,p,j,i) * wm_interm_50_so_pt4(a,k) * wm_interm_88_so_pt4(q,k,j,i)
term(1629) = term(1629) + t2(a,p,j,i) * wm_interm_51_so_pt4(a,k) * wm_interm_88_so_pt4(q,k,j,i)
term(1630) = term(1630) + t2(a,p,j,i) * wm_interm_109_so_pt4(q,k,j,i) * wm_interm_50_so_pt4(a,k)
term(1631) = term(1631) + t2(a,p,j,i) * wm_interm_109_so_pt4(q,k,j,i) * wm_interm_51_so_pt4(a,k)
end do 
end do 
end do 
end do 

term(1628) = term(1628) * (2.0d+0) 
term(1629) = term(1629) * (-1.0d+0) 
term(1630) = term(1630) * (4.0d+0) 
term(1631) = term(1631) * (-2.0d+0) 

do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(1632) = term(1632) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_19_so_pt4(a,b) * wm_interm_49_so_pt4(b,i,j,q)
term(1633) = term(1633) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_20_so_pt4(a,b) * wm_interm_49_so_pt4(b,i,j,q)
term(1634) = term(1634) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_19_so_pt4(a,b) * wm_interm_49_so_pt4(b,i,j,q)
term(1635) = term(1635) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_20_so_pt4(a,b) * wm_interm_49_so_pt4(b,i,j,q)
term(1636) = term(1636) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_22_so_pt4(b,i,j,q) * wm_interm_57_so_pt4(a,b)
term(1637) = term(1637) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_22_so_pt4(b,i,j,q) * wm_interm_58_so_pt4(a,b)
end do 
end do 
end do 
end do 

term(1632) = term(1632) * (-0.5d+0) 
term(1634) = term(1634) * (-1.0d+0) 
term(1635) = term(1635) * (2.0d+0) 
term(1636) = term(1636) * (-8.0d+0) 
term(1637) = term(1637) * (4.0d+0) 

do i = 1, nocc 
term(1638) = term(1638) + wm_interm_21_so_pt4(i,q) * wm_interm_33_so_pt4(p,i)
term(1639) = term(1639) + wm_interm_21_so_pt4(i,q) * wm_interm_34_so_pt4(p,i)
term(1640) = term(1640) + wm_interm_21_so_pt4(i,q) * wm_interm_35_so_pt4(p,i)
term(1641) = term(1641) + wm_interm_21_so_pt4(i,q) * wm_interm_36_so_pt4(p,i)
term(1642) = term(1642) + wm_interm_21_so_pt4(i,q) * wm_interm_37_so_pt4(p,i)
term(1643) = term(1643) + wm_interm_25_so_pt4(i,q) * wm_interm_38_so_pt4(p,i)
term(1644) = term(1644) + wm_interm_25_so_pt4(i,q) * wm_interm_40_so_pt4(p,i)
term(1645) = term(1645) + wm_interm_25_so_pt4(i,q) * wm_interm_41_so_pt4(p,i)
term(1646) = term(1646) + wm_interm_25_so_pt4(i,q) * wm_interm_42_so_pt4(p,i)
term(1647) = term(1647) + wm_interm_25_so_pt4(i,q) * wm_interm_43_so_pt4(p,i)
end do 

term(1638) = term(1638) * (2.0d+0) 
term(1639) = term(1639) * (-1.0d+0) 
term(1640) = term(1640) * (-1.0d+0) 
term(1641) = term(1641) * (4.0d+0) 
term(1642) = term(1642) * (-4.0d+0) 
term(1644) = term(1644) * (-2.0d+0) 
term(1646) = term(1646) * (4.0d+0) 
term(1647) = term(1647) * (-4.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
do j = 1, nocc 
do b = nocc + 1, nactive 
term(1648) = term(1648) + t2(a,p,j,i) * wm_interm_50_so_pt4(b,j) * wm_interm_73_so_pt4(b,a,q,i)
term(1649) = term(1649) + t2(a,p,j,i) * wm_interm_50_so_pt4(b,j) * wm_interm_84_so_pt4(b,a,q,i)
term(1650) = term(1650) + t2(a,p,j,i) * wm_interm_50_so_pt4(b,j) * wm_interm_69_so_pt4(b,a,q,i)
term(1651) = term(1651) + t2(a,p,j,i) * wm_interm_50_so_pt4(b,j) * wm_interm_74_so_pt4(b,a,q,i)
term(1652) = term(1652) + t2(a,p,j,i) * wm_interm_50_so_pt4(b,j) * wm_interm_67_so_pt4(b,a,q,i)
term(1653) = term(1653) + t2(a,p,j,i) * wm_interm_50_so_pt4(b,j) * wm_interm_68_so_pt4(b,a,q,i)
term(1654) = term(1654) + t2(a,p,j,i) * wm_interm_51_so_pt4(b,j) * wm_interm_73_so_pt4(b,a,q,i)
term(1655) = term(1655) + t2(a,p,j,i) * wm_interm_51_so_pt4(b,j) * wm_interm_84_so_pt4(b,a,q,i)
term(1656) = term(1656) + t2(a,p,j,i) * wm_interm_51_so_pt4(b,j) * wm_interm_69_so_pt4(b,a,q,i)
term(1657) = term(1657) + t2(a,p,j,i) * wm_interm_51_so_pt4(b,j) * wm_interm_74_so_pt4(b,a,q,i)
term(1658) = term(1658) + t2(a,p,j,i) * wm_interm_51_so_pt4(b,j) * wm_interm_67_so_pt4(b,a,q,i)
term(1659) = term(1659) + t2(a,p,j,i) * wm_interm_51_so_pt4(b,j) * wm_interm_68_so_pt4(b,a,q,i)
term(1660) = term(1660) + t2(a,p,j,i) * wm_interm_50_so_pt4(b,j) * wm_interm_97_so_pt4(b,a,q,i)
term(1661) = term(1661) + t2(a,p,j,i) * wm_interm_50_so_pt4(b,j) * wm_interm_98_so_pt4(b,a,q,i)
term(1662) = term(1662) + t2(a,p,j,i) * wm_interm_50_so_pt4(b,j) * wm_interm_95_so_pt4(b,a,q,i)
term(1663) = term(1663) + t2(a,p,j,i) * wm_interm_50_so_pt4(b,j) * wm_interm_96_so_pt4(b,a,q,i)
term(1664) = term(1664) + t2(a,p,j,i) * wm_interm_51_so_pt4(b,j) * wm_interm_97_so_pt4(b,a,q,i)
term(1665) = term(1665) + t2(a,p,j,i) * wm_interm_51_so_pt4(b,j) * wm_interm_98_so_pt4(b,a,q,i)
term(1666) = term(1666) + t2(a,p,j,i) * wm_interm_51_so_pt4(b,j) * wm_interm_95_so_pt4(b,a,q,i)
term(1667) = term(1667) + t2(a,p,j,i) * wm_interm_51_so_pt4(b,j) * wm_interm_96_so_pt4(b,a,q,i)
term(1668) = term(1668) + t2(a,p,j,i) * wm_interm_49_so_pt4(b,j,i,q) * wm_interm_83_so_pt4(b,a)
term(1669) = term(1669) + t2(a,p,j,i) * wm_interm_49_so_pt4(b,j,i,q) * wm_interm_81_so_pt4(b,a)
term(1670) = term(1670) + t2(a,p,j,i) * wm_interm_49_so_pt4(b,j,i,q) * wm_interm_82_so_pt4(b,a)
term(1671) = term(1671) + t2(a,p,j,i) * wm_interm_104_so_pt4(b,a) * wm_interm_49_so_pt4(b,j,i,q)
term(1672) = term(1672) + t2(a,p,j,i) * wm_interm_105_so_pt4(b,a) * wm_interm_49_so_pt4(b,j,i,q)
end do 
end do 
end do 
end do 

term(1648) = term(1648) * (2.0d+0) 
term(1649) = term(1649) * (-4.0d+0) 
term(1650) = term(1650) * (2.0d+0) 
term(1651) = term(1651) * (2.0d+0) 
term(1652) = term(1652) * (2.0d+0) 
term(1653) = term(1653) * (-4.0d+0) 
term(1654) = term(1654) * (-1.0d+0) 
term(1655) = term(1655) * (2.0d+0) 
term(1656) = term(1656) * (-1.0d+0) 
term(1657) = term(1657) * (-1.0d+0) 
term(1658) = term(1658) * (-1.0d+0) 
term(1659) = term(1659) * (2.0d+0) 
term(1660) = term(1660) * (8.0d+0) 
term(1661) = term(1661) * (-8.0d+0) 
term(1662) = term(1662) * (8.0d+0) 
term(1663) = term(1663) * (-8.0d+0) 
term(1664) = term(1664) * (-4.0d+0) 
term(1665) = term(1665) * (4.0d+0) 
term(1666) = term(1666) * (-4.0d+0) 
term(1667) = term(1667) * (4.0d+0) 
term(1668) = term(1668) * (-1.0d+0) 
term(1669) = term(1669) * (-1.0d+0) 
term(1670) = term(1670) * (2.0d+0) 
term(1671) = term(1671) * (-4.0d+0) 
term(1672) = term(1672) * (4.0d+0) 

do a = nocc + 1, nactive 
term(1673) = term(1673) + wm_interm_23_so_pt4(a,p) * wm_interm_33_so_pt4(a,q)
term(1674) = term(1674) + wm_interm_23_so_pt4(a,p) * wm_interm_34_so_pt4(a,q)
term(1675) = term(1675) + wm_interm_23_so_pt4(a,p) * wm_interm_35_so_pt4(a,q)
term(1676) = term(1676) + wm_interm_23_so_pt4(a,p) * wm_interm_36_so_pt4(a,q)
term(1677) = term(1677) + wm_interm_23_so_pt4(a,p) * wm_interm_37_so_pt4(a,q)
term(1678) = term(1678) + wm_interm_38_so_pt4(a,q) * wm_interm_44_so_pt4(a,p)
term(1679) = term(1679) + wm_interm_40_so_pt4(a,q) * wm_interm_44_so_pt4(a,p)
term(1680) = term(1680) + wm_interm_41_so_pt4(a,q) * wm_interm_44_so_pt4(a,p)
term(1681) = term(1681) + wm_interm_42_so_pt4(a,q) * wm_interm_44_so_pt4(a,p)
term(1682) = term(1682) + wm_interm_43_so_pt4(a,q) * wm_interm_44_so_pt4(a,p)
end do 

term(1673) = term(1673) * (2.0d+0) 
term(1674) = term(1674) * (-1.0d+0) 
term(1675) = term(1675) * (-1.0d+0) 
term(1676) = term(1676) * (4.0d+0) 
term(1677) = term(1677) * (-4.0d+0) 
term(1679) = term(1679) * (-2.0d+0) 
term(1681) = term(1681) * (4.0d+0) 
term(1682) = term(1682) * (-4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(1683) = term(1683) + r2m(vrdav_Rr, a,i,p,q) * wm_interm_113_so_pt4(b,i,j,k) * wm_interm_55_so_pt4(a,b,k,j)
term(1684) = term(1684) + r2m(vrdav_Rr, a,i,p,q) * wm_interm_113_so_pt4(b,i,j,k) * wm_interm_54_so_pt4(a,b,k,j)
term(1685) = term(1685) + r2m(vrdav_Rr, a,i,p,q) * wm_interm_22_so_pt4(b,i,j,k) * wm_interm_56_so_pt4(a,b,k,j)
term(1686) = term(1686) + r2m(vrdav_Rr, a,i,p,q) * wm_interm_22_so_pt4(b,i,j,k) * wm_interm_59_so_pt4(a,b,k,j)
term(1687) = term(1687) + r2m(vrdav_Rr, a,i,p,q) * wm_interm_10_so_pt4(a,b,j,k) * wm_interm_47_so_pt4(b,i,k,j)
term(1688) = term(1688) + r2m(vrdav_Rr, a,i,p,q) * wm_interm_11_so_pt4(a,b,j,k) * wm_interm_47_so_pt4(b,i,k,j)
term(1689) = term(1689) + r2m(vrdav_Rr, a,i,p,q) * wm_interm_1_so_pt4(a,b,j,k) * wm_interm_47_so_pt4(b,i,k,j)
term(1690) = term(1690) + r2m(vrdav_Rr, a,i,p,q) * wm_interm_47_so_pt4(b,i,j,k) * wm_interm_5_so_pt4(a,b,k,j)
term(1691) = term(1691) + r2m(vrdav_Rr, a,i,p,q) * wm_interm_113_so_pt4(b,i,j,k) * wm_interm_56_so_pt4(a,b,k,j)
term(1692) = term(1692) + r2m(vrdav_Rr, a,i,p,q) * wm_interm_113_so_pt4(b,i,j,k) * wm_interm_59_so_pt4(a,b,k,j)
term(1693) = term(1693) + r2m(vrdav_Rr, a,i,p,q) * wm_interm_10_so_pt4(a,b,j,k) * wm_interm_49_so_pt4(b,i,k,j)
term(1694) = term(1694) + r2m(vrdav_Rr, a,i,p,q) * wm_interm_11_so_pt4(a,b,j,k) * wm_interm_49_so_pt4(b,i,k,j)
term(1695) = term(1695) + r2m(vrdav_Rr, a,i,p,q) * wm_interm_49_so_pt4(b,i,j,k) * wm_interm_5_so_pt4(a,b,k,j)
term(1696) = term(1696) + r2m(vrdav_Rr, a,i,p,q) * wm_interm_1_so_pt4(a,b,j,k) * wm_interm_49_so_pt4(b,i,k,j)
term(1697) = term(1697) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_22_so_pt4(b,i,j,k) * wm_interm_59_so_pt4(a,b,k,q)
term(1698) = term(1698) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_1_so_pt4(a,b,k,q) * wm_interm_47_so_pt4(b,i,j,k)
term(1699) = term(1699) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_10_so_pt4(a,b,k,q) * wm_interm_47_so_pt4(b,i,j,k)
term(1700) = term(1700) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_11_so_pt4(a,b,k,q) * wm_interm_47_so_pt4(b,i,j,k)
term(1701) = term(1701) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_113_so_pt4(b,i,j,k) * wm_interm_56_so_pt4(a,b,k,q)
term(1702) = term(1702) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_49_so_pt4(b,i,j,k) * wm_interm_5_so_pt4(a,b,k,q)
term(1703) = term(1703) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_113_so_pt4(b,i,j,k) * wm_interm_54_so_pt4(a,b,k,q)
term(1704) = term(1704) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_22_so_pt4(b,i,j,k) * wm_interm_59_so_pt4(a,b,k,q)
term(1705) = term(1705) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_22_so_pt4(b,i,j,k) * wm_interm_56_so_pt4(a,b,k,q)
term(1706) = term(1706) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_113_so_pt4(b,i,j,k) * wm_interm_55_so_pt4(a,b,k,q)
term(1707) = term(1707) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_1_so_pt4(a,b,k,q) * wm_interm_47_so_pt4(b,i,j,k)
term(1708) = term(1708) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_10_so_pt4(a,b,k,q) * wm_interm_47_so_pt4(b,i,j,k)
term(1709) = term(1709) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_11_so_pt4(a,b,k,q) * wm_interm_47_so_pt4(b,i,j,k)
term(1710) = term(1710) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_113_so_pt4(b,i,j,k) * wm_interm_56_so_pt4(a,b,k,q)
term(1711) = term(1711) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_113_so_pt4(b,i,j,k) * wm_interm_59_so_pt4(a,b,k,q)
term(1712) = term(1712) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_49_so_pt4(b,i,j,k) * wm_interm_5_so_pt4(a,b,k,q)
end do 
end do 
end do 
end do 
end do 

term(1683) = term(1683) * (-16.0d+0) 
term(1684) = term(1684) * (32.0d+0) 
term(1685) = term(1685) * (-8.0d+0) 
term(1686) = term(1686) * (4.0d+0) 
term(1687) = term(1687) * (4.0d+0) 
term(1688) = term(1688) * (-8.0d+0) 
term(1689) = term(1689) * (4.0d+0) 
term(1690) = term(1690) * (-8.0d+0) 
term(1691) = term(1691) * (4.0d+0) 
term(1692) = term(1692) * (-8.0d+0) 
term(1693) = term(1693) * (-8.0d+0) 
term(1694) = term(1694) * (16.0d+0) 
term(1695) = term(1695) * (4.0d+0) 
term(1696) = term(1696) * (-8.0d+0) 
term(1697) = term(1697) * (-0.5d+0) 
term(1698) = term(1698) * (-0.5d+0) 
term(1699) = term(1699) * (-0.5d+0) 
term(1701) = term(1701) * (-0.5d+0) 
term(1702) = term(1702) * (-0.5d+0) 
term(1703) = term(1703) * (-8.0d+0) 
term(1704) = term(1704) * (-2.0d+0) 
term(1705) = term(1705) * (2.0d+0) 
term(1706) = term(1706) * (4.0d+0) 
term(1707) = term(1707) * (-1.0d+0) 
term(1708) = term(1708) * (-1.0d+0) 
term(1709) = term(1709) * (2.0d+0) 
term(1710) = term(1710) * (-2.0d+0) 
term(1711) = term(1711) * (2.0d+0) 
term(1712) = term(1712) * (-1.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(1713) = term(1713) + t1(p,i) * wm_interm_60_so_pt4(j,k) * wm_interm_88_so_pt4(j,q,i,k)
term(1714) = term(1714) + t1(p,i) * wm_interm_60_so_pt4(j,k) * wm_interm_88_so_pt4(q,j,i,k)
term(1715) = term(1715) + t1(p,i) * wm_interm_62_so_pt4(j,k) * wm_interm_88_so_pt4(j,q,i,k)
term(1716) = term(1716) + t1(p,i) * wm_interm_62_so_pt4(j,k) * wm_interm_88_so_pt4(q,j,i,k)
term(1717) = term(1717) + t1(p,i) * wm_interm_109_so_pt4(j,q,i,k) * wm_interm_60_so_pt4(j,k)
term(1718) = term(1718) + t1(p,i) * wm_interm_109_so_pt4(q,j,i,k) * wm_interm_60_so_pt4(j,k)
term(1719) = term(1719) + t1(p,i) * wm_interm_109_so_pt4(j,q,i,k) * wm_interm_62_so_pt4(j,k)
term(1720) = term(1720) + t1(p,i) * wm_interm_109_so_pt4(q,j,i,k) * wm_interm_62_so_pt4(j,k)
term(1721) = term(1721) + t1(p,i) * wm_interm_61_so_pt4(j,i,q,k) * wm_interm_85_so_pt4(j,k)
term(1722) = term(1722) + t1(p,i) * wm_interm_61_so_pt4(j,i,q,k) * wm_interm_86_so_pt4(j,k)
term(1723) = term(1723) + t1(p,i) * wm_interm_61_so_pt4(j,i,q,k) * wm_interm_87_so_pt4(j,k)
term(1724) = term(1724) + t1(p,i) * wm_interm_106_so_pt4(j,k) * wm_interm_61_so_pt4(j,i,q,k)
term(1725) = term(1725) + t1(p,i) * wm_interm_107_so_pt4(j,k) * wm_interm_61_so_pt4(j,i,q,k)
term(1726) = term(1726) + t1(p,i) * wm_interm_108_so_pt4(j,k) * wm_interm_61_so_pt4(j,i,q,k)
end do 
end do 
end do 

term(1713) = term(1713) * (-1.0d+0) 
term(1714) = term(1714) * (2.0d+0) 
term(1715) = term(1715) * (2.0d+0) 
term(1716) = term(1716) * (-4.0d+0) 
term(1717) = term(1717) * (-2.0d+0) 
term(1718) = term(1718) * (4.0d+0) 
term(1719) = term(1719) * (4.0d+0) 
term(1720) = term(1720) * (-8.0d+0) 
term(1721) = term(1721) * (-1.0d+0) 
term(1722) = term(1722) * (2.0d+0) 
term(1723) = term(1723) * (-1.0d+0) 
term(1724) = term(1724) * (-2.0d+0) 
term(1725) = term(1725) * (4.0d+0) 
term(1726) = term(1726) * (-2.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(1727) = term(1727) + t2(a,p,j,i) * wm_interm_22_so_pt4(b,i,j,k) * wm_interm_63_so_pt4(a,b,k,q)
term(1728) = term(1728) + t2(a,p,j,i) * wm_interm_22_so_pt4(b,i,j,k) * wm_interm_66_so_pt4(a,b,k,q)
term(1729) = term(1729) + t2(a,p,j,i) * wm_interm_22_so_pt4(b,i,j,k) * wm_interm_65_so_pt4(a,b,k,q)
term(1730) = term(1730) + t2(a,p,j,i) * wm_interm_22_so_pt4(b,i,j,k) * wm_interm_75_so_pt4(a,b,k,q)
term(1731) = term(1731) + t2(a,p,j,i) * wm_interm_22_so_pt4(b,i,j,k) * wm_interm_77_so_pt4(a,b,k,q)
term(1732) = term(1732) + t2(a,p,j,i) * wm_interm_22_so_pt4(b,i,j,k) * wm_interm_79_so_pt4(a,b,k,q)
term(1733) = term(1733) + r2m(vrdav_Rr, a,j,p,i) * wm_interm_47_so_pt4(b,i,j,k) * wm_interm_5_so_pt4(a,b,k,q)
term(1734) = term(1734) + r2m(vrdav_Rr, a,j,p,i) * wm_interm_1_so_pt4(a,b,k,q) * wm_interm_47_so_pt4(b,i,j,k)
term(1735) = term(1735) + r2m(vrdav_Rr, a,j,p,i) * wm_interm_10_so_pt4(a,b,k,q) * wm_interm_47_so_pt4(b,i,j,k)
term(1736) = term(1736) + r2m(vrdav_Rr, a,j,p,i) * wm_interm_11_so_pt4(a,b,k,q) * wm_interm_47_so_pt4(b,i,j,k)
term(1737) = term(1737) + r2m(vrdav_Rr, a,j,p,i) * wm_interm_10_so_pt4(a,b,k,q) * wm_interm_49_so_pt4(b,i,j,k)
term(1738) = term(1738) + r2m(vrdav_Rr, a,j,p,i) * wm_interm_49_so_pt4(b,i,j,k) * wm_interm_5_so_pt4(a,b,k,q)
term(1739) = term(1739) + r2m(vrdav_Rr, a,j,p,i) * wm_interm_1_so_pt4(a,b,k,q) * wm_interm_49_so_pt4(b,i,j,k)
term(1740) = term(1740) + r2m(vrdav_Rr, a,j,p,i) * wm_interm_11_so_pt4(a,b,k,q) * wm_interm_49_so_pt4(b,i,j,k)
term(1741) = term(1741) + t2(a,p,j,i) * wm_interm_22_so_pt4(b,i,j,k) * wm_interm_93_so_pt4(a,b,k,q)
term(1742) = term(1742) + t2(a,p,j,i) * wm_interm_22_so_pt4(b,i,j,k) * wm_interm_94_so_pt4(a,b,k,q)
term(1743) = term(1743) + t2(a,p,j,i) * wm_interm_100_so_pt4(a,b,k,q) * wm_interm_22_so_pt4(b,i,j,k)
term(1744) = term(1744) + t2(a,p,j,i) * wm_interm_102_so_pt4(a,b,k,q) * wm_interm_22_so_pt4(b,i,j,k)
end do 
end do 
end do 
end do 
end do 

term(1727) = term(1727) * (-1.0d+0) 
term(1728) = term(1728) * (-1.0d+0) 
term(1729) = term(1729) * (2.0d+0) 
term(1730) = term(1730) * (2.0d+0) 
term(1731) = term(1731) * (2.0d+0) 
term(1732) = term(1732) * (-4.0d+0) 
term(1733) = term(1733) * (-1.0d+0) 
term(1734) = term(1734) * (2.0d+0) 
term(1735) = term(1735) * (2.0d+0) 
term(1736) = term(1736) * (-4.0d+0) 
term(1737) = term(1737) * (-1.0d+0) 
term(1738) = term(1738) * (2.0d+0) 
term(1739) = term(1739) * (-1.0d+0) 
term(1740) = term(1740) * (2.0d+0) 
term(1741) = term(1741) * (-4.0d+0) 
term(1742) = term(1742) * (4.0d+0) 
term(1743) = term(1743) * (8.0d+0) 
term(1744) = term(1744) * (-8.0d+0) 

do l = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(1745) = term(1745) + r2m(vrdav_Rr, a,i,p,q) * wm_interm_22_so_pt4(a,j,k,l) * wm_interm_61_so_pt4(i,l,k,j)
term(1746) = term(1746) + r2m(vrdav_Rr, a,i,p,q) * wm_interm_22_so_pt4(a,j,k,l) * wm_interm_61_so_pt4(l,i,k,j)
term(1747) = term(1747) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_47_so_pt4(a,k,l,q) * wm_interm_9_so_pt4(j,i,k,l)
term(1748) = term(1748) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_49_so_pt4(a,k,l,q) * wm_interm_9_so_pt4(i,j,k,l)
term(1749) = term(1749) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_22_so_pt4(a,k,l,q) * wm_interm_61_so_pt4(j,i,k,l)
term(1750) = term(1750) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_49_so_pt4(a,k,l,q) * wm_interm_9_so_pt4(i,j,k,l)
term(1751) = term(1751) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_49_so_pt4(a,k,l,q) * wm_interm_9_so_pt4(j,i,k,l)
term(1752) = term(1752) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_47_so_pt4(a,k,l,q) * wm_interm_9_so_pt4(j,i,k,l)
term(1753) = term(1753) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_47_so_pt4(a,k,l,q) * wm_interm_9_so_pt4(i,j,k,l)
term(1754) = term(1754) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_22_so_pt4(a,k,l,q) * wm_interm_61_so_pt4(j,i,k,l)
end do 
end do 
end do 
end do 
end do 

term(1745) = term(1745) * (4.0d+0) 
term(1746) = term(1746) * (-8.0d+0) 
term(1747) = term(1747) * (-0.5d+0) 
term(1748) = term(1748) * (-0.5d+0) 
term(1749) = term(1749) * (-0.5d+0) 
term(1750) = term(1750) * (-2.0d+0) 
term(1751) = term(1751) * (2.0d+0) 
term(1752) = term(1752) * (-2.0d+0) 
term(1753) = term(1753) * (2.0d+0) 
term(1754) = term(1754) * (-1.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(1755) = term(1755) + r2m(vrdav_Rr, a,i,p,q) * wm_interm_22_so_pt4(a,j,k,l) * wm_interm_61_so_pt4(i,l,j,k)
term(1756) = term(1756) + r2m(vrdav_Rr, a,i,p,q) * wm_interm_47_so_pt4(a,j,k,l) * wm_interm_9_so_pt4(l,i,j,k)
term(1757) = term(1757) + r2m(vrdav_Rr, a,i,p,q) * wm_interm_47_so_pt4(a,j,k,l) * wm_interm_9_so_pt4(i,l,j,k)
term(1758) = term(1758) + r2m(vrdav_Rr, a,i,p,q) * wm_interm_22_so_pt4(a,j,k,l) * wm_interm_61_so_pt4(l,i,j,k)
term(1759) = term(1759) + r2m(vrdav_Rr, a,i,p,q) * wm_interm_49_so_pt4(a,j,k,l) * wm_interm_9_so_pt4(i,l,j,k)
term(1760) = term(1760) + r2m(vrdav_Rr, a,i,p,q) * wm_interm_49_so_pt4(a,j,k,l) * wm_interm_9_so_pt4(l,i,j,k)
end do 
end do 
end do 
end do 
end do 

term(1755) = term(1755) * (-8.0d+0) 
term(1756) = term(1756) * (4.0d+0) 
term(1757) = term(1757) * (-8.0d+0) 
term(1758) = term(1758) * (4.0d+0) 
term(1759) = term(1759) * (4.0d+0) 
term(1760) = term(1760) * (-8.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(1761) = term(1761) + r1(vrdav_Rr, p,i) * wm_interm_14_so_pt4(a,j) * wm_interm_22_so_pt4(a,i,j,q)
term(1762) = term(1762) + r1(vrdav_Rr, p,i) * wm_interm_16_so_pt4(a,j) * wm_interm_22_so_pt4(a,i,j,q)
term(1763) = term(1763) + t1(p,i) * wm_interm_12_so_pt4(a,q,i,j) * wm_interm_14_so_pt4(a,j)
term(1764) = term(1764) + t1(p,i) * wm_interm_12_so_pt4(a,q,i,j) * wm_interm_16_so_pt4(a,j)
term(1765) = term(1765) + r1(vrdav_Rr, a,q) * wm_interm_10_so_pt4(a,p,i,j) * wm_interm_21_so_pt4(j,i)
term(1766) = term(1766) + r1(vrdav_Rr, a,q) * wm_interm_1_so_pt4(a,p,i,j) * wm_interm_21_so_pt4(j,i)
term(1767) = term(1767) + r1(vrdav_Rr, a,q) * wm_interm_11_so_pt4(a,p,i,j) * wm_interm_21_so_pt4(j,i)
term(1768) = term(1768) + t1(a,i) * wm_interm_13_so_pt4(p,i,q,j) * wm_interm_2_so_pt4(a,j)
term(1769) = term(1769) + t1(a,i) * wm_interm_4_so_pt4(i,j) * wm_interm_5_so_pt4(a,p,j,q)
term(1770) = term(1770) + r1(vrdav_Rr, a,i) * wm_interm_21_so_pt4(i,j) * wm_interm_5_so_pt4(a,p,j,q)
term(1771) = term(1771) + t1(a,q) * wm_interm_10_so_pt4(a,p,i,j) * wm_interm_4_so_pt4(j,i)
term(1772) = term(1772) + t1(a,q) * wm_interm_1_so_pt4(a,p,i,j) * wm_interm_4_so_pt4(j,i)
term(1773) = term(1773) + t1(a,q) * wm_interm_11_so_pt4(a,p,i,j) * wm_interm_4_so_pt4(j,i)
term(1774) = term(1774) + t1(p,i) * wm_interm_13_so_pt4(a,i,j,q) * wm_interm_2_so_pt4(a,j)
term(1775) = term(1775) + t1(a,i) * wm_interm_17_so_pt4(i,j) * wm_interm_77_so_pt4(a,p,j,q)
term(1776) = term(1776) + t1(a,i) * wm_interm_18_so_pt4(i,j) * wm_interm_77_so_pt4(a,p,j,q)
term(1777) = term(1777) + t1(a,i) * wm_interm_17_so_pt4(i,j) * wm_interm_79_so_pt4(a,p,j,q)
term(1778) = term(1778) + t1(a,i) * wm_interm_18_so_pt4(i,j) * wm_interm_79_so_pt4(a,p,j,q)
term(1779) = term(1779) + t1(a,i) * wm_interm_17_so_pt4(i,j) * wm_interm_75_so_pt4(a,p,j,q)
term(1780) = term(1780) + t1(a,i) * wm_interm_18_so_pt4(i,j) * wm_interm_75_so_pt4(a,p,j,q)
term(1781) = term(1781) + t1(a,i) * wm_interm_17_so_pt4(i,j) * wm_interm_66_so_pt4(a,p,j,q)
term(1782) = term(1782) + t1(a,i) * wm_interm_17_so_pt4(i,j) * wm_interm_65_so_pt4(a,p,j,q)
term(1783) = term(1783) + t1(a,i) * wm_interm_18_so_pt4(i,j) * wm_interm_66_so_pt4(a,p,j,q)
term(1784) = term(1784) + t1(a,i) * wm_interm_18_so_pt4(i,j) * wm_interm_65_so_pt4(a,p,j,q)
term(1785) = term(1785) + t1(a,i) * wm_interm_17_so_pt4(i,j) * wm_interm_63_so_pt4(a,p,j,q)
term(1786) = term(1786) + t1(a,i) * wm_interm_18_so_pt4(i,j) * wm_interm_63_so_pt4(a,p,j,q)
term(1787) = term(1787) + t1(a,i) * wm_interm_100_so_pt4(a,p,j,q) * wm_interm_17_so_pt4(i,j)
term(1788) = term(1788) + t1(a,i) * wm_interm_100_so_pt4(a,p,j,q) * wm_interm_18_so_pt4(i,j)
term(1789) = term(1789) + t1(a,i) * wm_interm_102_so_pt4(a,p,j,q) * wm_interm_17_so_pt4(i,j)
term(1790) = term(1790) + t1(a,i) * wm_interm_102_so_pt4(a,p,j,q) * wm_interm_18_so_pt4(i,j)
term(1791) = term(1791) + t1(a,i) * wm_interm_17_so_pt4(i,j) * wm_interm_93_so_pt4(a,p,j,q)
term(1792) = term(1792) + t1(a,i) * wm_interm_17_so_pt4(i,j) * wm_interm_94_so_pt4(a,p,j,q)
term(1793) = term(1793) + r2m(vrdav_Rr, a,i,p,q) * wm_interm_17_so_pt4(i,j) * wm_interm_50_so_pt4(a,j)
term(1794) = term(1794) + t1(a,i) * wm_interm_18_so_pt4(i,j) * wm_interm_93_so_pt4(a,p,j,q)
term(1795) = term(1795) + t1(a,i) * wm_interm_18_so_pt4(i,j) * wm_interm_94_so_pt4(a,p,j,q)
term(1796) = term(1796) + r2m(vrdav_Rr, a,i,p,q) * wm_interm_18_so_pt4(i,j) * wm_interm_50_so_pt4(a,j)
term(1797) = term(1797) + r2m(vrdav_Rr, a,i,p,q) * wm_interm_17_so_pt4(i,j) * wm_interm_51_so_pt4(a,j)
term(1798) = term(1798) + r2m(vrdav_Rr, a,i,p,q) * wm_interm_18_so_pt4(i,j) * wm_interm_51_so_pt4(a,j)
term(1799) = term(1799) + t1(a,q) * wm_interm_60_so_pt4(i,j) * wm_interm_67_so_pt4(p,a,i,j)
term(1800) = term(1800) + t1(a,q) * wm_interm_60_so_pt4(i,j) * wm_interm_68_so_pt4(p,a,i,j)
term(1801) = term(1801) + t1(a,q) * wm_interm_60_so_pt4(i,j) * wm_interm_69_so_pt4(p,a,i,j)
term(1802) = term(1802) + t1(a,q) * wm_interm_60_so_pt4(i,j) * wm_interm_70_so_pt4(p,a,i,j)
term(1803) = term(1803) + t1(a,q) * wm_interm_62_so_pt4(i,j) * wm_interm_67_so_pt4(p,a,i,j)
term(1804) = term(1804) + t1(a,q) * wm_interm_62_so_pt4(i,j) * wm_interm_68_so_pt4(p,a,i,j)
term(1805) = term(1805) + t1(a,q) * wm_interm_62_so_pt4(i,j) * wm_interm_69_so_pt4(p,a,i,j)
term(1806) = term(1806) + t1(a,q) * wm_interm_62_so_pt4(i,j) * wm_interm_70_so_pt4(p,a,i,j)
term(1807) = term(1807) + t1(a,q) * wm_interm_60_so_pt4(i,j) * wm_interm_73_so_pt4(p,a,i,j)
term(1808) = term(1808) + t1(a,q) * wm_interm_60_so_pt4(i,j) * wm_interm_74_so_pt4(p,a,i,j)
term(1809) = term(1809) + t1(a,q) * wm_interm_62_so_pt4(i,j) * wm_interm_73_so_pt4(p,a,i,j)
term(1810) = term(1810) + t1(a,q) * wm_interm_62_so_pt4(i,j) * wm_interm_74_so_pt4(p,a,i,j)
term(1811) = term(1811) + t1(a,q) * wm_interm_100_so_pt4(a,p,i,j) * wm_interm_17_so_pt4(j,i)
term(1812) = term(1812) + t1(a,q) * wm_interm_100_so_pt4(a,p,i,j) * wm_interm_18_so_pt4(j,i)
term(1813) = term(1813) + t1(a,q) * wm_interm_102_so_pt4(a,p,i,j) * wm_interm_17_so_pt4(j,i)
term(1814) = term(1814) + t1(a,q) * wm_interm_102_so_pt4(a,p,i,j) * wm_interm_18_so_pt4(j,i)
term(1815) = term(1815) + t1(a,q) * wm_interm_60_so_pt4(i,j) * wm_interm_95_so_pt4(p,a,i,j)
term(1816) = term(1816) + t1(a,q) * wm_interm_60_so_pt4(i,j) * wm_interm_96_so_pt4(p,a,i,j)
term(1817) = term(1817) + t1(a,q) * wm_interm_60_so_pt4(i,j) * wm_interm_97_so_pt4(p,a,i,j)
term(1818) = term(1818) + t1(a,q) * wm_interm_60_so_pt4(i,j) * wm_interm_98_so_pt4(p,a,i,j)
term(1819) = term(1819) + t1(a,q) * wm_interm_62_so_pt4(i,j) * wm_interm_95_so_pt4(p,a,i,j)
term(1820) = term(1820) + t1(a,q) * wm_interm_62_so_pt4(i,j) * wm_interm_96_so_pt4(p,a,i,j)
term(1821) = term(1821) + t1(a,q) * wm_interm_62_so_pt4(i,j) * wm_interm_97_so_pt4(p,a,i,j)
term(1822) = term(1822) + t1(a,q) * wm_interm_62_so_pt4(i,j) * wm_interm_98_so_pt4(p,a,i,j)
term(1823) = term(1823) + t1(a,i) * wm_interm_5_so_pt4(a,p,j,q) * wm_interm_76_so_pt4(i,j)
term(1824) = term(1824) + t1(a,i) * wm_interm_5_so_pt4(a,p,j,q) * wm_interm_78_so_pt4(i,j)
term(1825) = term(1825) + t1(a,i) * wm_interm_5_so_pt4(a,p,j,q) * wm_interm_80_so_pt4(i,j)
term(1826) = term(1826) + t1(a,i) * wm_interm_56_so_pt4(a,p,j,q) * wm_interm_85_so_pt4(j,i)
term(1827) = term(1827) + t1(a,i) * wm_interm_56_so_pt4(a,p,j,q) * wm_interm_86_so_pt4(j,i)
term(1828) = term(1828) + t1(a,i) * wm_interm_56_so_pt4(a,p,j,q) * wm_interm_87_so_pt4(j,i)
term(1829) = term(1829) + t1(a,i) * wm_interm_101_so_pt4(i,j) * wm_interm_5_so_pt4(a,p,j,q)
term(1830) = term(1830) + t1(a,i) * wm_interm_103_so_pt4(i,j) * wm_interm_5_so_pt4(a,p,j,q)
term(1831) = term(1831) + t1(a,i) * wm_interm_106_so_pt4(j,i) * wm_interm_56_so_pt4(a,p,j,q)
term(1832) = term(1832) + t1(a,i) * wm_interm_107_so_pt4(j,i) * wm_interm_56_so_pt4(a,p,j,q)
term(1833) = term(1833) + t1(a,i) * wm_interm_108_so_pt4(j,i) * wm_interm_56_so_pt4(a,p,j,q)
term(1834) = term(1834) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_17_so_pt4(i,q) * wm_interm_50_so_pt4(a,j)
term(1835) = term(1835) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_18_so_pt4(i,q) * wm_interm_50_so_pt4(a,j)
term(1836) = term(1836) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_17_so_pt4(i,q) * wm_interm_51_so_pt4(a,j)
term(1837) = term(1837) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_18_so_pt4(i,q) * wm_interm_51_so_pt4(a,j)
term(1838) = term(1838) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_17_so_pt4(i,q) * wm_interm_50_so_pt4(a,j)
term(1839) = term(1839) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_18_so_pt4(i,q) * wm_interm_50_so_pt4(a,j)
term(1840) = term(1840) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_17_so_pt4(i,q) * wm_interm_51_so_pt4(a,j)
term(1841) = term(1841) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_18_so_pt4(i,q) * wm_interm_51_so_pt4(a,j)
term(1842) = term(1842) + t1(a,q) * wm_interm_10_so_pt4(a,p,i,j) * wm_interm_78_so_pt4(j,i)
term(1843) = term(1843) + t1(a,q) * wm_interm_10_so_pt4(a,p,i,j) * wm_interm_80_so_pt4(j,i)
term(1844) = term(1844) + t1(a,q) * wm_interm_10_so_pt4(a,p,i,j) * wm_interm_76_so_pt4(j,i)
term(1845) = term(1845) + t1(a,q) * wm_interm_1_so_pt4(a,p,i,j) * wm_interm_78_so_pt4(j,i)
term(1846) = term(1846) + t1(a,q) * wm_interm_1_so_pt4(a,p,i,j) * wm_interm_80_so_pt4(j,i)
term(1847) = term(1847) + t1(a,q) * wm_interm_1_so_pt4(a,p,i,j) * wm_interm_76_so_pt4(j,i)
term(1848) = term(1848) + t1(a,q) * wm_interm_11_so_pt4(a,p,i,j) * wm_interm_78_so_pt4(j,i)
term(1849) = term(1849) + t1(a,q) * wm_interm_11_so_pt4(a,p,i,j) * wm_interm_80_so_pt4(j,i)
term(1850) = term(1850) + t1(a,q) * wm_interm_11_so_pt4(a,p,i,j) * wm_interm_76_so_pt4(j,i)
term(1851) = term(1851) + t1(a,q) * wm_interm_59_so_pt4(a,p,i,j) * wm_interm_85_so_pt4(i,j)
term(1852) = term(1852) + t1(a,q) * wm_interm_59_so_pt4(a,p,i,j) * wm_interm_86_so_pt4(i,j)
term(1853) = term(1853) + t1(a,q) * wm_interm_59_so_pt4(a,p,i,j) * wm_interm_87_so_pt4(i,j)
term(1854) = term(1854) + t1(a,q) * wm_interm_55_so_pt4(a,p,i,j) * wm_interm_85_so_pt4(i,j)
term(1855) = term(1855) + t1(a,q) * wm_interm_54_so_pt4(a,p,i,j) * wm_interm_85_so_pt4(i,j)
term(1856) = term(1856) + t1(a,q) * wm_interm_55_so_pt4(a,p,i,j) * wm_interm_86_so_pt4(i,j)
term(1857) = term(1857) + t1(a,q) * wm_interm_54_so_pt4(a,p,i,j) * wm_interm_86_so_pt4(i,j)
term(1858) = term(1858) + t1(a,q) * wm_interm_55_so_pt4(a,p,i,j) * wm_interm_87_so_pt4(i,j)
term(1859) = term(1859) + t1(a,q) * wm_interm_54_so_pt4(a,p,i,j) * wm_interm_87_so_pt4(i,j)
term(1860) = term(1860) + t1(a,q) * wm_interm_106_so_pt4(i,j) * wm_interm_59_so_pt4(a,p,i,j)
term(1861) = term(1861) + t1(a,q) * wm_interm_107_so_pt4(i,j) * wm_interm_59_so_pt4(a,p,i,j)
term(1862) = term(1862) + t1(a,q) * wm_interm_108_so_pt4(i,j) * wm_interm_59_so_pt4(a,p,i,j)
term(1863) = term(1863) + t1(a,q) * wm_interm_106_so_pt4(i,j) * wm_interm_55_so_pt4(a,p,i,j)
term(1864) = term(1864) + t1(a,q) * wm_interm_106_so_pt4(i,j) * wm_interm_54_so_pt4(a,p,i,j)
term(1865) = term(1865) + t1(a,q) * wm_interm_107_so_pt4(i,j) * wm_interm_55_so_pt4(a,p,i,j)
term(1866) = term(1866) + t1(a,q) * wm_interm_107_so_pt4(i,j) * wm_interm_54_so_pt4(a,p,i,j)
term(1867) = term(1867) + t1(a,q) * wm_interm_108_so_pt4(i,j) * wm_interm_55_so_pt4(a,p,i,j)
term(1868) = term(1868) + t1(a,q) * wm_interm_108_so_pt4(i,j) * wm_interm_54_so_pt4(a,p,i,j)
end do 
end do 
end do 

term(1761) = term(1761) * (-4.0d+0) 
term(1762) = term(1762) * (2.0d+0) 
term(1763) = term(1763) * (8.0d+0) 
term(1764) = term(1764) * (-4.0d+0) 
term(1765) = term(1765) * (2.0d+0) 
term(1766) = term(1766) * (2.0d+0) 
term(1767) = term(1767) * (-4.0d+0) 
term(1768) = term(1768) * (2.0d+0) 
term(1769) = term(1769) * (2.0d+0) 
term(1770) = term(1770) * (2.0d+0) 
term(1771) = term(1771) * (4.0d+0) 
term(1772) = term(1772) * (4.0d+0) 
term(1773) = term(1773) * (-8.0d+0) 
term(1774) = term(1774) * (4.0d+0) 
term(1775) = term(1775) * (4.0d+0) 
term(1776) = term(1776) * (-8.0d+0) 
term(1777) = term(1777) * (-8.0d+0) 
term(1778) = term(1778) * (16.0d+0) 
term(1779) = term(1779) * (4.0d+0) 
term(1780) = term(1780) * (-8.0d+0) 
term(1781) = term(1781) * (-2.0d+0) 
term(1782) = term(1782) * (4.0d+0) 
term(1783) = term(1783) * (4.0d+0) 
term(1784) = term(1784) * (-8.0d+0) 
term(1785) = term(1785) * (-2.0d+0) 
term(1786) = term(1786) * (4.0d+0) 
term(1787) = term(1787) * (16.0d+0) 
term(1788) = term(1788) * (-32.0d+0) 
term(1789) = term(1789) * (-16.0d+0) 
term(1790) = term(1790) * (32.0d+0) 
term(1791) = term(1791) * (-8.0d+0) 
term(1792) = term(1792) * (8.0d+0) 
term(1793) = term(1793) * (-16.0d+0) 
term(1794) = term(1794) * (16.0d+0) 
term(1795) = term(1795) * (-16.0d+0) 
term(1796) = term(1796) * (32.0d+0) 
term(1797) = term(1797) * (8.0d+0) 
term(1798) = term(1798) * (-16.0d+0) 
term(1799) = term(1799) * (-1.0d+0) 
term(1800) = term(1800) * (2.0d+0) 
term(1801) = term(1801) * (-1.0d+0) 
term(1802) = term(1802) * (2.0d+0) 
term(1803) = term(1803) * (2.0d+0) 
term(1804) = term(1804) * (-4.0d+0) 
term(1805) = term(1805) * (2.0d+0) 
term(1806) = term(1806) * (-4.0d+0) 
term(1807) = term(1807) * (-1.0d+0) 
term(1808) = term(1808) * (-1.0d+0) 
term(1809) = term(1809) * (2.0d+0) 
term(1810) = term(1810) * (2.0d+0) 
term(1811) = term(1811) * (-4.0d+0) 
term(1812) = term(1812) * (8.0d+0) 
term(1813) = term(1813) * (4.0d+0) 
term(1814) = term(1814) * (-8.0d+0) 
term(1815) = term(1815) * (-4.0d+0) 
term(1816) = term(1816) * (4.0d+0) 
term(1817) = term(1817) * (-4.0d+0) 
term(1818) = term(1818) * (4.0d+0) 
term(1819) = term(1819) * (8.0d+0) 
term(1820) = term(1820) * (-8.0d+0) 
term(1821) = term(1821) * (8.0d+0) 
term(1822) = term(1822) * (-8.0d+0) 
term(1823) = term(1823) * (-1.0d+0) 
term(1824) = term(1824) * (-1.0d+0) 
term(1825) = term(1825) * (2.0d+0) 
term(1826) = term(1826) * (-1.0d+0) 
term(1827) = term(1827) * (2.0d+0) 
term(1828) = term(1828) * (-1.0d+0) 
term(1829) = term(1829) * (-4.0d+0) 
term(1830) = term(1830) * (4.0d+0) 
term(1831) = term(1831) * (-2.0d+0) 
term(1832) = term(1832) * (4.0d+0) 
term(1833) = term(1833) * (-2.0d+0) 
term(1834) = term(1834) * (2.0d+0) 
term(1835) = term(1835) * (-4.0d+0) 
term(1836) = term(1836) * (-1.0d+0) 
term(1837) = term(1837) * (2.0d+0) 
term(1838) = term(1838) * (8.0d+0) 
term(1839) = term(1839) * (-16.0d+0) 
term(1840) = term(1840) * (-4.0d+0) 
term(1841) = term(1841) * (8.0d+0) 
term(1842) = term(1842) * (-2.0d+0) 
term(1843) = term(1843) * (4.0d+0) 
term(1844) = term(1844) * (-2.0d+0) 
term(1845) = term(1845) * (-2.0d+0) 
term(1846) = term(1846) * (4.0d+0) 
term(1847) = term(1847) * (-2.0d+0) 
term(1848) = term(1848) * (4.0d+0) 
term(1849) = term(1849) * (-8.0d+0) 
term(1850) = term(1850) * (4.0d+0) 
term(1851) = term(1851) * (-2.0d+0) 
term(1852) = term(1852) * (4.0d+0) 
term(1853) = term(1853) * (-2.0d+0) 
term(1854) = term(1854) * (-2.0d+0) 
term(1855) = term(1855) * (4.0d+0) 
term(1856) = term(1856) * (4.0d+0) 
term(1857) = term(1857) * (-8.0d+0) 
term(1858) = term(1858) * (-2.0d+0) 
term(1859) = term(1859) * (4.0d+0) 
term(1860) = term(1860) * (-4.0d+0) 
term(1861) = term(1861) * (8.0d+0) 
term(1862) = term(1862) * (-4.0d+0) 
term(1863) = term(1863) * (-4.0d+0) 
term(1864) = term(1864) * (8.0d+0) 
term(1865) = term(1865) * (8.0d+0) 
term(1866) = term(1866) * (-16.0d+0) 
term(1867) = term(1867) * (-4.0d+0) 
term(1868) = term(1868) * (8.0d+0) 

do i = 1, nocc 
do l = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
term(1869) = term(1869) + t1(p,i) * wm_interm_61_so_pt4(j,k,l,q) * wm_interm_88_so_pt4(k,j,l,i)
term(1870) = term(1870) + t1(p,i) * wm_interm_109_so_pt4(j,k,l,i) * wm_interm_61_so_pt4(k,j,l,q)
end do 
end do 
end do 
end do 

term(1869) = term(1869) * (-1.0d+0) 
term(1870) = term(1870) * (-2.0d+0) 

do l = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
term(1871) = term(1871) + t1(p,i) * wm_interm_61_so_pt4(j,k,q,l) * wm_interm_88_so_pt4(j,k,l,i)
term(1872) = term(1872) + t1(p,i) * wm_interm_61_so_pt4(j,k,q,l) * wm_interm_88_so_pt4(k,j,i,l)
term(1873) = term(1873) + t1(p,i) * wm_interm_61_so_pt4(j,k,q,l) * wm_interm_88_so_pt4(j,k,i,l)
term(1874) = term(1874) + t1(p,i) * wm_interm_61_so_pt4(j,k,l,q) * wm_interm_88_so_pt4(j,k,i,l)
term(1875) = term(1875) + t1(p,i) * wm_interm_61_so_pt4(j,k,l,q) * wm_interm_88_so_pt4(k,j,i,l)
term(1876) = term(1876) + t1(p,i) * wm_interm_109_so_pt4(j,k,l,i) * wm_interm_61_so_pt4(j,k,q,l)
term(1877) = term(1877) + t1(p,i) * wm_interm_109_so_pt4(j,k,i,l) * wm_interm_61_so_pt4(k,j,q,l)
term(1878) = term(1878) + t1(p,i) * wm_interm_109_so_pt4(j,k,i,l) * wm_interm_61_so_pt4(j,k,q,l)
term(1879) = term(1879) + t1(p,i) * wm_interm_109_so_pt4(j,k,i,l) * wm_interm_61_so_pt4(j,k,l,q)
term(1880) = term(1880) + t1(p,i) * wm_interm_109_so_pt4(j,k,i,l) * wm_interm_61_so_pt4(k,j,l,q)
end do 
end do 
end do 
end do 

term(1871) = term(1871) * (-1.0d+0) 
term(1872) = term(1872) * (-1.0d+0) 
term(1873) = term(1873) * (2.0d+0) 
term(1874) = term(1874) * (-1.0d+0) 
term(1875) = term(1875) * (2.0d+0) 
term(1876) = term(1876) * (-2.0d+0) 
term(1877) = term(1877) * (-2.0d+0) 
term(1878) = term(1878) * (4.0d+0) 
term(1879) = term(1879) * (-2.0d+0) 
term(1880) = term(1880) * (4.0d+0) 


    calc_D_vo_wm_so_pt4 = zero
    do s = 0, 1880
    calc_D_vo_wm_so_pt4 = calc_D_vo_wm_so_pt4 + term(s)
    end do

    end function calc_D_vo_wm_so_pt4
    
    function calc_D_vv_wm_so_pt4(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vv_wm_so_pt4
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
    real(F64), dimension(0:774) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(0) = term(0) + wm_interm_27_so_pt4(p,i,j,k) * wm_interm_47_so_pt4(q,j,i,k)
term(1) = term(1) + wm_interm_27_so_pt4(p,i,j,k) * wm_interm_47_so_pt4(q,i,j,k)
term(2) = term(2) + wm_interm_28_so_pt4(p,i,j,k) * wm_interm_47_so_pt4(q,i,j,k)
term(3) = term(3) + wm_interm_27_so_pt4(p,i,j,k) * wm_interm_49_so_pt4(q,i,j,k)
term(4) = term(4) + wm_interm_27_so_pt4(p,i,j,k) * wm_interm_49_so_pt4(q,j,i,k)
term(5) = term(5) + wm_interm_28_so_pt4(p,i,j,k) * wm_interm_49_so_pt4(q,j,i,k)
term(6) = term(6) + wm_interm_30_so_pt4(p,i,j,k) * wm_interm_47_so_pt4(q,j,i,k)
term(7) = term(7) + wm_interm_30_so_pt4(p,i,j,k) * wm_interm_47_so_pt4(q,i,j,k)
term(8) = term(8) + wm_interm_30_so_pt4(p,i,j,k) * wm_interm_49_so_pt4(q,i,j,k)
term(9) = term(9) + wm_interm_30_so_pt4(p,i,j,k) * wm_interm_49_so_pt4(q,j,i,k)
term(10) = term(10) + r1(vrdav_Rr, p,i) * wm_interm_22_so_pt4(q,i,j,k) * wm_interm_62_so_pt4(k,j)
term(11) = term(11) + r1(vrdav_Rr, p,i) * wm_interm_22_so_pt4(q,i,j,k) * wm_interm_60_so_pt4(k,j)
end do 
end do 
end do 

term(0) = term(0) * (0.5d+0) 
term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (0.5d+0) 
term(3) = term(3) * (0.5d+0) 
term(4) = term(4) * (-1.0d+0) 
term(5) = term(5) * (0.5d+0) 
term(6) = term(6) * (2.0d+0) 
term(7) = term(7) * (-2.0d+0) 
term(8) = term(8) * (2.0d+0) 
term(9) = term(9) * (-2.0d+0) 
term(10) = term(10) * (16.0d+0) 
term(11) = term(11) * (-8.0d+0) 

do a = nocc + 1, nactive 
term(12) = term(12) + wm_interm_44_so_pt4(q,a) * wm_interm_91_so_pt4(a,p)
term(13) = term(13) + wm_interm_44_so_pt4(q,a) * wm_interm_92_so_pt4(a,p)
term(14) = term(14) + wm_interm_44_so_pt4(q,a) * wm_interm_90_so_pt4(a,p)
term(15) = term(15) + wm_interm_110_so_pt4(a,p) * wm_interm_44_so_pt4(q,a)
term(16) = term(16) + wm_interm_111_so_pt4(a,p) * wm_interm_44_so_pt4(q,a)
term(17) = term(17) + wm_interm_44_so_pt4(a,p) * wm_interm_91_so_pt4(q,a)
term(18) = term(18) + wm_interm_44_so_pt4(a,p) * wm_interm_92_so_pt4(q,a)
term(19) = term(19) + wm_interm_44_so_pt4(a,p) * wm_interm_90_so_pt4(q,a)
term(20) = term(20) + wm_interm_110_so_pt4(q,a) * wm_interm_44_so_pt4(a,p)
term(21) = term(21) + wm_interm_111_so_pt4(q,a) * wm_interm_44_so_pt4(a,p)
term(22) = term(22) + wm_interm_0_so_pt4(p,a) * wm_interm_23_so_pt4(q,a)
term(23) = term(23) + wm_interm_15_so_pt4(a,p) * wm_interm_44_so_pt4(q,a)
term(24) = term(24) + wm_interm_0_so_pt4(a,q) * wm_interm_23_so_pt4(a,p)
term(25) = term(25) + wm_interm_15_so_pt4(q,a) * wm_interm_44_so_pt4(a,p)
end do 

term(13) = term(13) * (-2.0d+0) 
term(15) = term(15) * (4.0d+0) 
term(16) = term(16) * (-4.0d+0) 
term(18) = term(18) * (-2.0d+0) 
term(20) = term(20) * (4.0d+0) 
term(21) = term(21) * (-4.0d+0) 
term(22) = term(22) * (-2.0d+0) 
term(23) = term(23) * (-2.0d+0) 
term(24) = term(24) * (-2.0d+0) 
term(25) = term(25) * (-2.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(26) = term(26) + s2(a,q,j,i) * wm_interm_14_so_pt4(a,j) * wm_interm_33_so_pt4(p,i)
term(27) = term(27) + s2(a,q,j,i) * wm_interm_14_so_pt4(a,j) * wm_interm_34_so_pt4(p,i)
term(28) = term(28) + s2(a,q,j,i) * wm_interm_14_so_pt4(a,j) * wm_interm_35_so_pt4(p,i)
term(29) = term(29) + s2(a,q,j,i) * wm_interm_16_so_pt4(a,j) * wm_interm_33_so_pt4(p,i)
term(30) = term(30) + s2(a,q,j,i) * wm_interm_16_so_pt4(a,j) * wm_interm_34_so_pt4(p,i)
term(31) = term(31) + s2(a,q,j,i) * wm_interm_16_so_pt4(a,j) * wm_interm_35_so_pt4(p,i)
term(32) = term(32) + s2(a,q,j,i) * wm_interm_14_so_pt4(a,j) * wm_interm_36_so_pt4(p,i)
term(33) = term(33) + s2(a,q,j,i) * wm_interm_14_so_pt4(a,j) * wm_interm_37_so_pt4(p,i)
term(34) = term(34) + s2(a,q,j,i) * wm_interm_16_so_pt4(a,j) * wm_interm_36_so_pt4(p,i)
term(35) = term(35) + s2(a,q,j,i) * wm_interm_16_so_pt4(a,j) * wm_interm_37_so_pt4(p,i)
term(36) = term(36) + r1(vrdav_Rl, a,i) * wm_interm_10_so_pt4(q,p,j,i) * wm_interm_33_so_pt4(a,j)
term(37) = term(37) + r1(vrdav_Rl, a,i) * wm_interm_1_so_pt4(q,p,j,i) * wm_interm_33_so_pt4(a,j)
term(38) = term(38) + r1(vrdav_Rl, a,i) * wm_interm_11_so_pt4(q,p,j,i) * wm_interm_33_so_pt4(a,j)
term(39) = term(39) + r1(vrdav_Rl, q,i) * wm_interm_33_so_pt4(a,j) * wm_interm_5_so_pt4(a,p,j,i)
term(40) = term(40) + r1(vrdav_Rl, a,i) * wm_interm_10_so_pt4(q,p,j,i) * wm_interm_34_so_pt4(a,j)
term(41) = term(41) + r1(vrdav_Rl, a,i) * wm_interm_1_so_pt4(q,p,j,i) * wm_interm_34_so_pt4(a,j)
term(42) = term(42) + r1(vrdav_Rl, a,i) * wm_interm_11_so_pt4(q,p,j,i) * wm_interm_34_so_pt4(a,j)
term(43) = term(43) + r1(vrdav_Rl, q,i) * wm_interm_34_so_pt4(a,j) * wm_interm_5_so_pt4(a,p,j,i)
term(44) = term(44) + r1(vrdav_Rl, a,i) * wm_interm_10_so_pt4(q,p,j,i) * wm_interm_35_so_pt4(a,j)
term(45) = term(45) + r1(vrdav_Rl, a,i) * wm_interm_1_so_pt4(q,p,j,i) * wm_interm_35_so_pt4(a,j)
term(46) = term(46) + r1(vrdav_Rl, a,i) * wm_interm_11_so_pt4(q,p,j,i) * wm_interm_35_so_pt4(a,j)
term(47) = term(47) + r1(vrdav_Rl, q,i) * wm_interm_35_so_pt4(a,j) * wm_interm_5_so_pt4(a,p,j,i)
term(48) = term(48) + r1(vrdav_Rl, a,i) * wm_interm_10_so_pt4(q,p,j,i) * wm_interm_36_so_pt4(a,j)
term(49) = term(49) + r1(vrdav_Rl, a,i) * wm_interm_1_so_pt4(q,p,j,i) * wm_interm_36_so_pt4(a,j)
term(50) = term(50) + r1(vrdav_Rl, a,i) * wm_interm_11_so_pt4(q,p,j,i) * wm_interm_36_so_pt4(a,j)
term(51) = term(51) + r1(vrdav_Rl, q,i) * wm_interm_36_so_pt4(a,j) * wm_interm_5_so_pt4(a,p,j,i)
term(52) = term(52) + r1(vrdav_Rl, a,i) * wm_interm_10_so_pt4(q,p,j,i) * wm_interm_37_so_pt4(a,j)
term(53) = term(53) + r1(vrdav_Rl, a,i) * wm_interm_1_so_pt4(q,p,j,i) * wm_interm_37_so_pt4(a,j)
term(54) = term(54) + r1(vrdav_Rl, a,i) * wm_interm_11_so_pt4(q,p,j,i) * wm_interm_37_so_pt4(a,j)
term(55) = term(55) + r1(vrdav_Rl, q,i) * wm_interm_37_so_pt4(a,j) * wm_interm_5_so_pt4(a,p,j,i)
term(56) = term(56) + s1(q,i) * wm_interm_38_so_pt4(a,j) * wm_interm_5_so_pt4(a,p,j,i)
term(57) = term(57) + s1(q,i) * wm_interm_40_so_pt4(a,j) * wm_interm_5_so_pt4(a,p,j,i)
term(58) = term(58) + s1(q,i) * wm_interm_41_so_pt4(a,j) * wm_interm_5_so_pt4(a,p,j,i)
term(59) = term(59) + s1(a,i) * wm_interm_1_so_pt4(q,p,j,i) * wm_interm_38_so_pt4(a,j)
term(60) = term(60) + s1(a,i) * wm_interm_1_so_pt4(q,p,j,i) * wm_interm_40_so_pt4(a,j)
term(61) = term(61) + s1(a,i) * wm_interm_1_so_pt4(q,p,j,i) * wm_interm_41_so_pt4(a,j)
term(62) = term(62) + s1(a,i) * wm_interm_10_so_pt4(q,p,j,i) * wm_interm_38_so_pt4(a,j)
term(63) = term(63) + s1(a,i) * wm_interm_10_so_pt4(q,p,j,i) * wm_interm_40_so_pt4(a,j)
term(64) = term(64) + s1(a,i) * wm_interm_11_so_pt4(q,p,j,i) * wm_interm_38_so_pt4(a,j)
term(65) = term(65) + s1(a,i) * wm_interm_11_so_pt4(q,p,j,i) * wm_interm_40_so_pt4(a,j)
term(66) = term(66) + s1(a,i) * wm_interm_10_so_pt4(q,p,j,i) * wm_interm_41_so_pt4(a,j)
term(67) = term(67) + s1(a,i) * wm_interm_11_so_pt4(q,p,j,i) * wm_interm_41_so_pt4(a,j)
term(68) = term(68) + s1(q,i) * wm_interm_42_so_pt4(a,j) * wm_interm_5_so_pt4(a,p,j,i)
term(69) = term(69) + s1(q,i) * wm_interm_43_so_pt4(a,j) * wm_interm_5_so_pt4(a,p,j,i)
term(70) = term(70) + s1(a,i) * wm_interm_1_so_pt4(q,p,j,i) * wm_interm_42_so_pt4(a,j)
term(71) = term(71) + s1(a,i) * wm_interm_1_so_pt4(q,p,j,i) * wm_interm_43_so_pt4(a,j)
term(72) = term(72) + s1(a,i) * wm_interm_10_so_pt4(q,p,j,i) * wm_interm_42_so_pt4(a,j)
term(73) = term(73) + s1(a,i) * wm_interm_10_so_pt4(q,p,j,i) * wm_interm_43_so_pt4(a,j)
term(74) = term(74) + s1(a,i) * wm_interm_11_so_pt4(q,p,j,i) * wm_interm_42_so_pt4(a,j)
term(75) = term(75) + s1(a,i) * wm_interm_11_so_pt4(q,p,j,i) * wm_interm_43_so_pt4(a,j)
term(76) = term(76) + t2(a,p,j,i) * wm_interm_2_so_pt4(a,i) * wm_interm_50_so_pt4(q,j)
term(77) = term(77) + t2(a,p,j,i) * wm_interm_2_so_pt4(a,i) * wm_interm_51_so_pt4(q,j)
term(78) = term(78) + t2(a,p,j,i) * wm_interm_2_so_pt4(q,j) * wm_interm_50_so_pt4(a,i)
term(79) = term(79) + t2(a,p,j,i) * wm_interm_2_so_pt4(q,j) * wm_interm_51_so_pt4(a,i)
term(80) = term(80) + t2(a,p,j,i) * wm_interm_2_so_pt4(q,i) * wm_interm_50_so_pt4(a,j)
term(81) = term(81) + t2(a,p,j,i) * wm_interm_2_so_pt4(q,i) * wm_interm_51_so_pt4(a,j)
end do 
end do 
end do 

term(26) = term(26) * (8.0d+0) 
term(27) = term(27) * (-4.0d+0) 
term(28) = term(28) * (-4.0d+0) 
term(29) = term(29) * (-4.0d+0) 
term(30) = term(30) * (2.0d+0) 
term(31) = term(31) * (2.0d+0) 
term(32) = term(32) * (16.0d+0) 
term(33) = term(33) * (-16.0d+0) 
term(34) = term(34) * (-8.0d+0) 
term(35) = term(35) * (8.0d+0) 
term(36) = term(36) * (2.0d+0) 
term(37) = term(37) * (2.0d+0) 
term(38) = term(38) * (-4.0d+0) 
term(39) = term(39) * (2.0d+0) 
term(40) = term(40) * (-1.0d+0) 
term(41) = term(41) * (-1.0d+0) 
term(42) = term(42) * (2.0d+0) 
term(43) = term(43) * (-1.0d+0) 
term(44) = term(44) * (-1.0d+0) 
term(45) = term(45) * (-1.0d+0) 
term(46) = term(46) * (2.0d+0) 
term(47) = term(47) * (-1.0d+0) 
term(48) = term(48) * (4.0d+0) 
term(49) = term(49) * (4.0d+0) 
term(50) = term(50) * (-8.0d+0) 
term(51) = term(51) * (4.0d+0) 
term(52) = term(52) * (-4.0d+0) 
term(53) = term(53) * (-4.0d+0) 
term(54) = term(54) * (8.0d+0) 
term(55) = term(55) * (-4.0d+0) 
term(56) = term(56) * (-2.0d+0) 
term(57) = term(57) * (4.0d+0) 
term(58) = term(58) * (-2.0d+0) 
term(59) = term(59) * (-2.0d+0) 
term(60) = term(60) * (4.0d+0) 
term(61) = term(61) * (-2.0d+0) 
term(62) = term(62) * (-2.0d+0) 
term(63) = term(63) * (4.0d+0) 
term(64) = term(64) * (4.0d+0) 
term(65) = term(65) * (-8.0d+0) 
term(66) = term(66) * (-2.0d+0) 
term(67) = term(67) * (4.0d+0) 
term(68) = term(68) * (-8.0d+0) 
term(69) = term(69) * (8.0d+0) 
term(70) = term(70) * (-8.0d+0) 
term(71) = term(71) * (8.0d+0) 
term(72) = term(72) * (-8.0d+0) 
term(73) = term(73) * (8.0d+0) 
term(74) = term(74) * (16.0d+0) 
term(75) = term(75) * (-16.0d+0) 
term(76) = term(76) * (-4.0d+0) 
term(77) = term(77) * (2.0d+0) 
term(78) = term(78) * (-4.0d+0) 
term(79) = term(79) * (2.0d+0) 
term(80) = term(80) * (8.0d+0) 
term(81) = term(81) * (-4.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(82) = term(82) + r1(vrdav_Rl, q,i) * wm_interm_19_so_pt4(a,p) * wm_interm_33_so_pt4(a,i)
term(83) = term(83) + r1(vrdav_Rl, q,i) * wm_interm_20_so_pt4(a,p) * wm_interm_33_so_pt4(a,i)
term(84) = term(84) + r1(vrdav_Rl, q,i) * wm_interm_19_so_pt4(a,p) * wm_interm_34_so_pt4(a,i)
term(85) = term(85) + r1(vrdav_Rl, q,i) * wm_interm_20_so_pt4(a,p) * wm_interm_34_so_pt4(a,i)
term(86) = term(86) + r1(vrdav_Rl, q,i) * wm_interm_19_so_pt4(a,p) * wm_interm_35_so_pt4(a,i)
term(87) = term(87) + r1(vrdav_Rl, q,i) * wm_interm_20_so_pt4(a,p) * wm_interm_35_so_pt4(a,i)
term(88) = term(88) + r1(vrdav_Rl, q,i) * wm_interm_19_so_pt4(a,p) * wm_interm_36_so_pt4(a,i)
term(89) = term(89) + r1(vrdav_Rl, q,i) * wm_interm_20_so_pt4(a,p) * wm_interm_36_so_pt4(a,i)
term(90) = term(90) + r1(vrdav_Rl, q,i) * wm_interm_19_so_pt4(a,p) * wm_interm_37_so_pt4(a,i)
term(91) = term(91) + r1(vrdav_Rl, q,i) * wm_interm_20_so_pt4(a,p) * wm_interm_37_so_pt4(a,i)
term(92) = term(92) + r1(vrdav_Rl, a,i) * wm_interm_7_so_pt4(p,i) * wm_interm_83_so_pt4(a,q)
term(93) = term(93) + r1(vrdav_Rl, a,i) * wm_interm_6_so_pt4(p,i) * wm_interm_83_so_pt4(a,q)
term(94) = term(94) + r1(vrdav_Rl, a,i) * wm_interm_7_so_pt4(p,i) * wm_interm_81_so_pt4(a,q)
term(95) = term(95) + r1(vrdav_Rl, a,i) * wm_interm_6_so_pt4(p,i) * wm_interm_81_so_pt4(a,q)
term(96) = term(96) + r1(vrdav_Rl, a,i) * wm_interm_7_so_pt4(p,i) * wm_interm_82_so_pt4(a,q)
term(97) = term(97) + r1(vrdav_Rl, a,i) * wm_interm_6_so_pt4(p,i) * wm_interm_82_so_pt4(a,q)
term(98) = term(98) + s1(a,i) * wm_interm_16_so_pt4(p,i) * wm_interm_81_so_pt4(a,q)
term(99) = term(99) + s1(a,i) * wm_interm_16_so_pt4(p,i) * wm_interm_82_so_pt4(a,q)
term(100) = term(100) + s1(a,i) * wm_interm_16_so_pt4(p,i) * wm_interm_83_so_pt4(a,q)
term(101) = term(101) + s1(q,i) * wm_interm_19_so_pt4(a,p) * wm_interm_38_so_pt4(a,i)
term(102) = term(102) + s1(q,i) * wm_interm_19_so_pt4(a,p) * wm_interm_40_so_pt4(a,i)
term(103) = term(103) + s1(q,i) * wm_interm_19_so_pt4(a,p) * wm_interm_41_so_pt4(a,i)
term(104) = term(104) + s1(q,i) * wm_interm_20_so_pt4(a,p) * wm_interm_38_so_pt4(a,i)
term(105) = term(105) + s1(q,i) * wm_interm_20_so_pt4(a,p) * wm_interm_40_so_pt4(a,i)
term(106) = term(106) + s1(q,i) * wm_interm_20_so_pt4(a,p) * wm_interm_41_so_pt4(a,i)
term(107) = term(107) + s1(a,i) * wm_interm_14_so_pt4(p,i) * wm_interm_81_so_pt4(a,q)
term(108) = term(108) + s1(a,i) * wm_interm_14_so_pt4(p,i) * wm_interm_82_so_pt4(a,q)
term(109) = term(109) + s1(a,i) * wm_interm_14_so_pt4(p,i) * wm_interm_83_so_pt4(a,q)
term(110) = term(110) + r1(vrdav_Rl, a,i) * wm_interm_104_so_pt4(a,q) * wm_interm_7_so_pt4(p,i)
term(111) = term(111) + r1(vrdav_Rl, a,i) * wm_interm_104_so_pt4(a,q) * wm_interm_6_so_pt4(p,i)
term(112) = term(112) + r1(vrdav_Rl, a,i) * wm_interm_105_so_pt4(a,q) * wm_interm_7_so_pt4(p,i)
term(113) = term(113) + r1(vrdav_Rl, a,i) * wm_interm_105_so_pt4(a,q) * wm_interm_6_so_pt4(p,i)
term(114) = term(114) + s1(a,i) * wm_interm_104_so_pt4(a,q) * wm_interm_16_so_pt4(p,i)
term(115) = term(115) + s1(a,i) * wm_interm_105_so_pt4(a,q) * wm_interm_16_so_pt4(p,i)
term(116) = term(116) + s1(a,i) * wm_interm_112_so_pt4(a,q) * wm_interm_16_so_pt4(p,i)
term(117) = term(117) + s1(q,i) * wm_interm_19_so_pt4(a,p) * wm_interm_42_so_pt4(a,i)
term(118) = term(118) + s1(q,i) * wm_interm_19_so_pt4(a,p) * wm_interm_43_so_pt4(a,i)
term(119) = term(119) + s1(q,i) * wm_interm_20_so_pt4(a,p) * wm_interm_42_so_pt4(a,i)
term(120) = term(120) + s1(q,i) * wm_interm_20_so_pt4(a,p) * wm_interm_43_so_pt4(a,i)
term(121) = term(121) + s1(a,i) * wm_interm_104_so_pt4(a,q) * wm_interm_14_so_pt4(p,i)
term(122) = term(122) + s1(a,i) * wm_interm_105_so_pt4(a,q) * wm_interm_14_so_pt4(p,i)
term(123) = term(123) + s1(a,i) * wm_interm_112_so_pt4(a,q) * wm_interm_14_so_pt4(p,i)
term(124) = term(124) + t1(a,i) * wm_interm_19_so_pt4(a,p) * wm_interm_45_so_pt4(q,i)
term(125) = term(125) + t1(a,i) * wm_interm_20_so_pt4(a,p) * wm_interm_45_so_pt4(q,i)
term(126) = term(126) + t1(a,i) * wm_interm_2_so_pt4(q,i) * wm_interm_58_so_pt4(a,p)
term(127) = term(127) + t1(a,i) * wm_interm_2_so_pt4(q,i) * wm_interm_57_so_pt4(a,p)
term(128) = term(128) + r1(vrdav_Rr, a,i) * wm_interm_19_so_pt4(a,p) * wm_interm_50_so_pt4(q,i)
term(129) = term(129) + r1(vrdav_Rr, a,i) * wm_interm_20_so_pt4(a,p) * wm_interm_50_so_pt4(q,i)
term(130) = term(130) + r1(vrdav_Rr, a,i) * wm_interm_19_so_pt4(a,p) * wm_interm_51_so_pt4(q,i)
term(131) = term(131) + r1(vrdav_Rr, a,i) * wm_interm_20_so_pt4(a,p) * wm_interm_51_so_pt4(q,i)
end do 
end do 

term(82) = term(82) * (2.0d+0) 
term(83) = term(83) * (-4.0d+0) 
term(84) = term(84) * (-1.0d+0) 
term(85) = term(85) * (2.0d+0) 
term(86) = term(86) * (-1.0d+0) 
term(87) = term(87) * (2.0d+0) 
term(88) = term(88) * (4.0d+0) 
term(89) = term(89) * (-8.0d+0) 
term(90) = term(90) * (-4.0d+0) 
term(91) = term(91) * (8.0d+0) 
term(92) = term(92) * (-2.0d+0) 
term(93) = term(93) * (4.0d+0) 
term(94) = term(94) * (-2.0d+0) 
term(95) = term(95) * (4.0d+0) 
term(96) = term(96) * (4.0d+0) 
term(97) = term(97) * (-8.0d+0) 
term(98) = term(98) * (-2.0d+0) 
term(99) = term(99) * (4.0d+0) 
term(100) = term(100) * (-2.0d+0) 
term(101) = term(101) * (-2.0d+0) 
term(102) = term(102) * (4.0d+0) 
term(103) = term(103) * (-2.0d+0) 
term(104) = term(104) * (4.0d+0) 
term(105) = term(105) * (-8.0d+0) 
term(106) = term(106) * (4.0d+0) 
term(107) = term(107) * (4.0d+0) 
term(108) = term(108) * (-8.0d+0) 
term(109) = term(109) * (4.0d+0) 
term(110) = term(110) * (-8.0d+0) 
term(111) = term(111) * (16.0d+0) 
term(112) = term(112) * (8.0d+0) 
term(113) = term(113) * (-16.0d+0) 
term(114) = term(114) * (-4.0d+0) 
term(115) = term(115) * (8.0d+0) 
term(116) = term(116) * (-4.0d+0) 
term(117) = term(117) * (-8.0d+0) 
term(118) = term(118) * (8.0d+0) 
term(119) = term(119) * (16.0d+0) 
term(120) = term(120) * (-16.0d+0) 
term(121) = term(121) * (8.0d+0) 
term(122) = term(122) * (-16.0d+0) 
term(123) = term(123) * (8.0d+0) 
term(124) = term(124) * (2.0d+0) 
term(125) = term(125) * (-4.0d+0) 
term(126) = term(126) * (2.0d+0) 
term(127) = term(127) * (-4.0d+0) 
term(128) = term(128) * (-4.0d+0) 
term(129) = term(129) * (8.0d+0) 
term(130) = term(130) * (2.0d+0) 
term(131) = term(131) * (-4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(132) = term(132) + wm_interm_114_so_pt4(q,a,i,j) * wm_interm_77_so_pt4(a,p,j,i)
term(133) = term(133) + wm_interm_114_so_pt4(q,a,i,j) * wm_interm_79_so_pt4(a,p,j,i)
term(134) = term(134) + wm_interm_114_so_pt4(q,a,i,j) * wm_interm_75_so_pt4(a,p,j,i)
term(135) = term(135) + wm_interm_114_so_pt4(q,a,i,j) * wm_interm_66_so_pt4(a,p,j,i)
term(136) = term(136) + wm_interm_114_so_pt4(q,a,i,j) * wm_interm_65_so_pt4(a,p,j,i)
term(137) = term(137) + wm_interm_114_so_pt4(q,a,i,j) * wm_interm_63_so_pt4(a,p,j,i)
term(138) = term(138) + wm_interm_100_so_pt4(a,p,i,j) * wm_interm_114_so_pt4(q,a,j,i)
term(139) = term(139) + wm_interm_102_so_pt4(a,p,i,j) * wm_interm_114_so_pt4(q,a,j,i)
term(140) = term(140) + wm_interm_114_so_pt4(q,a,i,j) * wm_interm_93_so_pt4(a,p,j,i)
term(141) = term(141) + wm_interm_114_so_pt4(q,a,i,j) * wm_interm_94_so_pt4(a,p,j,i)
term(142) = term(142) + wm_interm_114_so_pt4(a,p,i,j) * wm_interm_64_so_pt4(q,a,j,i)
term(143) = term(143) + wm_interm_114_so_pt4(a,p,i,j) * wm_interm_63_so_pt4(q,a,j,i)
term(144) = term(144) + wm_interm_114_so_pt4(a,p,i,j) * wm_interm_66_so_pt4(q,a,j,i)
term(145) = term(145) + wm_interm_114_so_pt4(a,p,i,j) * wm_interm_94_so_pt4(q,a,j,i)
term(146) = term(146) + wm_interm_114_so_pt4(a,p,i,j) * wm_interm_93_so_pt4(q,a,j,i)
term(147) = term(147) + s2(a,q,i,j) * wm_interm_14_so_pt4(a,j) * wm_interm_33_so_pt4(p,i)
term(148) = term(148) + s2(a,q,i,j) * wm_interm_14_so_pt4(a,j) * wm_interm_34_so_pt4(p,i)
term(149) = term(149) + s2(a,q,i,j) * wm_interm_14_so_pt4(a,j) * wm_interm_35_so_pt4(p,i)
term(150) = term(150) + s2(a,q,i,j) * wm_interm_16_so_pt4(a,j) * wm_interm_33_so_pt4(p,i)
term(151) = term(151) + s2(a,q,i,j) * wm_interm_16_so_pt4(a,j) * wm_interm_34_so_pt4(p,i)
term(152) = term(152) + s2(a,q,i,j) * wm_interm_16_so_pt4(a,j) * wm_interm_35_so_pt4(p,i)
term(153) = term(153) + s2(a,q,i,j) * wm_interm_14_so_pt4(a,j) * wm_interm_36_so_pt4(p,i)
term(154) = term(154) + s2(a,q,i,j) * wm_interm_14_so_pt4(a,j) * wm_interm_37_so_pt4(p,i)
term(155) = term(155) + s2(a,q,i,j) * wm_interm_16_so_pt4(a,j) * wm_interm_36_so_pt4(p,i)
term(156) = term(156) + s2(a,q,i,j) * wm_interm_16_so_pt4(a,j) * wm_interm_37_so_pt4(p,i)
term(157) = term(157) + r1(vrdav_Rl, a,i) * wm_interm_67_so_pt4(p,q,i,j) * wm_interm_7_so_pt4(a,j)
term(158) = term(158) + r1(vrdav_Rl, a,i) * wm_interm_68_so_pt4(p,q,i,j) * wm_interm_7_so_pt4(a,j)
term(159) = term(159) + r1(vrdav_Rl, a,i) * wm_interm_69_so_pt4(p,q,i,j) * wm_interm_7_so_pt4(a,j)
term(160) = term(160) + r1(vrdav_Rl, a,i) * wm_interm_70_so_pt4(p,q,i,j) * wm_interm_7_so_pt4(a,j)
term(161) = term(161) + s1(a,i) * wm_interm_14_so_pt4(a,j) * wm_interm_67_so_pt4(p,q,i,j)
term(162) = term(162) + s1(a,i) * wm_interm_14_so_pt4(a,j) * wm_interm_68_so_pt4(p,q,i,j)
term(163) = term(163) + s1(a,i) * wm_interm_14_so_pt4(a,j) * wm_interm_69_so_pt4(p,q,i,j)
term(164) = term(164) + s1(a,i) * wm_interm_14_so_pt4(a,j) * wm_interm_70_so_pt4(p,q,i,j)
term(165) = term(165) + r1(vrdav_Rl, a,i) * wm_interm_73_so_pt4(p,q,i,j) * wm_interm_7_so_pt4(a,j)
term(166) = term(166) + r1(vrdav_Rl, a,i) * wm_interm_74_so_pt4(p,q,i,j) * wm_interm_7_so_pt4(a,j)
term(167) = term(167) + s1(a,i) * wm_interm_14_so_pt4(a,j) * wm_interm_73_so_pt4(p,q,i,j)
term(168) = term(168) + s1(a,i) * wm_interm_14_so_pt4(a,j) * wm_interm_74_so_pt4(p,q,i,j)
term(169) = term(169) + r1(vrdav_Rl, a,i) * wm_interm_67_so_pt4(p,q,i,j) * wm_interm_6_so_pt4(a,j)
term(170) = term(170) + r1(vrdav_Rl, a,i) * wm_interm_68_so_pt4(p,q,i,j) * wm_interm_6_so_pt4(a,j)
term(171) = term(171) + r1(vrdav_Rl, a,i) * wm_interm_69_so_pt4(p,q,i,j) * wm_interm_6_so_pt4(a,j)
term(172) = term(172) + r1(vrdav_Rl, a,i) * wm_interm_6_so_pt4(a,j) * wm_interm_70_so_pt4(p,q,i,j)
term(173) = term(173) + r1(vrdav_Rl, a,i) * wm_interm_6_so_pt4(a,j) * wm_interm_73_so_pt4(p,q,i,j)
term(174) = term(174) + r1(vrdav_Rl, a,i) * wm_interm_6_so_pt4(a,j) * wm_interm_74_so_pt4(p,q,i,j)
term(175) = term(175) + s1(a,i) * wm_interm_16_so_pt4(a,j) * wm_interm_67_so_pt4(p,q,i,j)
term(176) = term(176) + s1(a,i) * wm_interm_16_so_pt4(a,j) * wm_interm_68_so_pt4(p,q,i,j)
term(177) = term(177) + s1(a,i) * wm_interm_16_so_pt4(a,j) * wm_interm_69_so_pt4(p,q,i,j)
term(178) = term(178) + s1(a,i) * wm_interm_16_so_pt4(a,j) * wm_interm_70_so_pt4(p,q,i,j)
term(179) = term(179) + s1(a,i) * wm_interm_16_so_pt4(a,j) * wm_interm_73_so_pt4(p,q,i,j)
term(180) = term(180) + s1(a,i) * wm_interm_16_so_pt4(a,j) * wm_interm_74_so_pt4(p,q,i,j)
term(181) = term(181) + r1(vrdav_Rl, a,i) * wm_interm_7_so_pt4(a,j) * wm_interm_95_so_pt4(p,q,i,j)
term(182) = term(182) + r1(vrdav_Rl, a,i) * wm_interm_7_so_pt4(a,j) * wm_interm_96_so_pt4(p,q,i,j)
term(183) = term(183) + r1(vrdav_Rl, a,i) * wm_interm_7_so_pt4(a,j) * wm_interm_97_so_pt4(p,q,i,j)
term(184) = term(184) + r1(vrdav_Rl, a,i) * wm_interm_7_so_pt4(a,j) * wm_interm_98_so_pt4(p,q,i,j)
term(185) = term(185) + s1(a,i) * wm_interm_14_so_pt4(a,j) * wm_interm_95_so_pt4(p,q,i,j)
term(186) = term(186) + s1(a,i) * wm_interm_14_so_pt4(a,j) * wm_interm_96_so_pt4(p,q,i,j)
term(187) = term(187) + s1(a,i) * wm_interm_14_so_pt4(a,j) * wm_interm_97_so_pt4(p,q,i,j)
term(188) = term(188) + s1(a,i) * wm_interm_14_so_pt4(a,j) * wm_interm_98_so_pt4(p,q,i,j)
term(189) = term(189) + r1(vrdav_Rl, a,i) * wm_interm_6_so_pt4(a,j) * wm_interm_95_so_pt4(p,q,i,j)
term(190) = term(190) + r1(vrdav_Rl, a,i) * wm_interm_6_so_pt4(a,j) * wm_interm_96_so_pt4(p,q,i,j)
term(191) = term(191) + r1(vrdav_Rl, a,i) * wm_interm_6_so_pt4(a,j) * wm_interm_97_so_pt4(p,q,i,j)
term(192) = term(192) + r1(vrdav_Rl, a,i) * wm_interm_6_so_pt4(a,j) * wm_interm_98_so_pt4(p,q,i,j)
term(193) = term(193) + s1(a,i) * wm_interm_16_so_pt4(a,j) * wm_interm_95_so_pt4(p,q,i,j)
term(194) = term(194) + s1(a,i) * wm_interm_16_so_pt4(a,j) * wm_interm_96_so_pt4(p,q,i,j)
term(195) = term(195) + s1(a,i) * wm_interm_16_so_pt4(a,j) * wm_interm_97_so_pt4(p,q,i,j)
term(196) = term(196) + s1(a,i) * wm_interm_16_so_pt4(a,j) * wm_interm_98_so_pt4(p,q,i,j)
term(197) = term(197) + s1(q,i) * wm_interm_14_so_pt4(a,j) * wm_interm_70_so_pt4(p,a,i,j)
term(198) = term(198) + s1(q,i) * wm_interm_14_so_pt4(a,j) * wm_interm_69_so_pt4(p,a,i,j)
term(199) = term(199) + s1(q,i) * wm_interm_14_so_pt4(a,j) * wm_interm_67_so_pt4(p,a,i,j)
term(200) = term(200) + s1(q,i) * wm_interm_14_so_pt4(a,j) * wm_interm_68_so_pt4(p,a,i,j)
term(201) = term(201) + s1(q,i) * wm_interm_14_so_pt4(a,j) * wm_interm_84_so_pt4(p,a,i,j)
term(202) = term(202) + s1(q,i) * wm_interm_14_so_pt4(a,j) * wm_interm_74_so_pt4(p,a,i,j)
term(203) = term(203) + s1(q,i) * wm_interm_16_so_pt4(a,j) * wm_interm_70_so_pt4(p,a,i,j)
term(204) = term(204) + s1(q,i) * wm_interm_16_so_pt4(a,j) * wm_interm_69_so_pt4(p,a,i,j)
term(205) = term(205) + s1(q,i) * wm_interm_16_so_pt4(a,j) * wm_interm_67_so_pt4(p,a,i,j)
term(206) = term(206) + s1(q,i) * wm_interm_16_so_pt4(a,j) * wm_interm_68_so_pt4(p,a,i,j)
term(207) = term(207) + s1(q,i) * wm_interm_16_so_pt4(a,j) * wm_interm_84_so_pt4(p,a,i,j)
term(208) = term(208) + s1(q,i) * wm_interm_16_so_pt4(a,j) * wm_interm_74_so_pt4(p,a,i,j)
term(209) = term(209) + s1(q,i) * wm_interm_14_so_pt4(a,j) * wm_interm_98_so_pt4(p,a,i,j)
term(210) = term(210) + s1(q,i) * wm_interm_14_so_pt4(a,j) * wm_interm_97_so_pt4(p,a,i,j)
term(211) = term(211) + s1(q,i) * wm_interm_14_so_pt4(a,j) * wm_interm_95_so_pt4(p,a,i,j)
term(212) = term(212) + s1(q,i) * wm_interm_14_so_pt4(a,j) * wm_interm_96_so_pt4(p,a,i,j)
term(213) = term(213) + s1(q,i) * wm_interm_16_so_pt4(a,j) * wm_interm_98_so_pt4(p,a,i,j)
term(214) = term(214) + s1(q,i) * wm_interm_16_so_pt4(a,j) * wm_interm_97_so_pt4(p,a,i,j)
term(215) = term(215) + s1(q,i) * wm_interm_16_so_pt4(a,j) * wm_interm_95_so_pt4(p,a,i,j)
term(216) = term(216) + s1(q,i) * wm_interm_16_so_pt4(a,j) * wm_interm_96_so_pt4(p,a,i,j)
term(217) = term(217) + s2(a,q,i,j) * wm_interm_16_so_pt4(p,i) * wm_interm_33_so_pt4(a,j)
term(218) = term(218) + s2(a,q,i,j) * wm_interm_16_so_pt4(p,i) * wm_interm_34_so_pt4(a,j)
term(219) = term(219) + s2(a,q,i,j) * wm_interm_16_so_pt4(p,i) * wm_interm_35_so_pt4(a,j)
term(220) = term(220) + s2(a,q,i,j) * wm_interm_14_so_pt4(p,i) * wm_interm_33_so_pt4(a,j)
term(221) = term(221) + s2(a,q,i,j) * wm_interm_14_so_pt4(p,i) * wm_interm_34_so_pt4(a,j)
term(222) = term(222) + s2(a,q,i,j) * wm_interm_14_so_pt4(p,i) * wm_interm_35_so_pt4(a,j)
term(223) = term(223) + s2(a,q,i,j) * wm_interm_16_so_pt4(p,i) * wm_interm_36_so_pt4(a,j)
term(224) = term(224) + s2(a,q,i,j) * wm_interm_16_so_pt4(p,i) * wm_interm_37_so_pt4(a,j)
term(225) = term(225) + s2(a,q,i,j) * wm_interm_14_so_pt4(p,i) * wm_interm_36_so_pt4(a,j)
term(226) = term(226) + s2(a,q,i,j) * wm_interm_14_so_pt4(p,i) * wm_interm_37_so_pt4(a,j)
term(227) = term(227) + s2(a,q,i,j) * wm_interm_38_so_pt4(a,j) * wm_interm_7_so_pt4(p,i)
term(228) = term(228) + s2(a,q,i,j) * wm_interm_40_so_pt4(a,j) * wm_interm_7_so_pt4(p,i)
term(229) = term(229) + s2(a,q,i,j) * wm_interm_41_so_pt4(a,j) * wm_interm_7_so_pt4(p,i)
term(230) = term(230) + s2(a,q,i,j) * wm_interm_38_so_pt4(a,j) * wm_interm_6_so_pt4(p,i)
term(231) = term(231) + s2(a,q,i,j) * wm_interm_40_so_pt4(a,j) * wm_interm_6_so_pt4(p,i)
term(232) = term(232) + s2(a,q,i,j) * wm_interm_41_so_pt4(a,j) * wm_interm_6_so_pt4(p,i)
term(233) = term(233) + s2(a,q,i,j) * wm_interm_42_so_pt4(a,j) * wm_interm_7_so_pt4(p,i)
term(234) = term(234) + s2(a,q,i,j) * wm_interm_43_so_pt4(a,j) * wm_interm_7_so_pt4(p,i)
term(235) = term(235) + s2(a,q,i,j) * wm_interm_42_so_pt4(a,j) * wm_interm_6_so_pt4(p,i)
term(236) = term(236) + s2(a,q,i,j) * wm_interm_43_so_pt4(a,j) * wm_interm_6_so_pt4(p,i)
term(237) = term(237) + r1(vrdav_Rr, p,i) * wm_interm_10_so_pt4(q,a,i,j) * wm_interm_50_so_pt4(a,j)
term(238) = term(238) + r1(vrdav_Rr, p,i) * wm_interm_11_so_pt4(q,a,i,j) * wm_interm_50_so_pt4(a,j)
term(239) = term(239) + r1(vrdav_Rr, p,i) * wm_interm_50_so_pt4(a,j) * wm_interm_5_so_pt4(q,a,i,j)
term(240) = term(240) + r1(vrdav_Rr, p,i) * wm_interm_1_so_pt4(q,a,i,j) * wm_interm_50_so_pt4(a,j)
term(241) = term(241) + r1(vrdav_Rr, p,i) * wm_interm_10_so_pt4(q,a,i,j) * wm_interm_51_so_pt4(a,j)
term(242) = term(242) + r1(vrdav_Rr, p,i) * wm_interm_11_so_pt4(q,a,i,j) * wm_interm_51_so_pt4(a,j)
term(243) = term(243) + r1(vrdav_Rr, p,i) * wm_interm_51_so_pt4(a,j) * wm_interm_5_so_pt4(q,a,i,j)
term(244) = term(244) + r1(vrdav_Rr, p,i) * wm_interm_1_so_pt4(q,a,i,j) * wm_interm_51_so_pt4(a,j)
term(245) = term(245) + t1(p,i) * wm_interm_2_so_pt4(a,j) * wm_interm_56_so_pt4(q,a,i,j)
term(246) = term(246) + t1(p,i) * wm_interm_45_so_pt4(a,j) * wm_interm_5_so_pt4(q,a,i,j)
term(247) = term(247) + t1(a,i) * wm_interm_2_so_pt4(a,j) * wm_interm_59_so_pt4(q,p,i,j)
term(248) = term(248) + t1(a,i) * wm_interm_2_so_pt4(a,j) * wm_interm_54_so_pt4(q,p,i,j)
term(249) = term(249) + t1(a,i) * wm_interm_2_so_pt4(a,j) * wm_interm_55_so_pt4(q,p,i,j)
term(250) = term(250) + t1(a,i) * wm_interm_1_so_pt4(q,p,i,j) * wm_interm_45_so_pt4(a,j)
term(251) = term(251) + t1(a,i) * wm_interm_10_so_pt4(q,p,i,j) * wm_interm_45_so_pt4(a,j)
term(252) = term(252) + t1(a,i) * wm_interm_11_so_pt4(q,p,i,j) * wm_interm_45_so_pt4(a,j)
term(253) = term(253) + r1(vrdav_Rr, a,i) * wm_interm_10_so_pt4(q,p,i,j) * wm_interm_50_so_pt4(a,j)
term(254) = term(254) + r1(vrdav_Rr, a,i) * wm_interm_1_so_pt4(q,p,i,j) * wm_interm_50_so_pt4(a,j)
term(255) = term(255) + r1(vrdav_Rr, a,i) * wm_interm_11_so_pt4(q,p,i,j) * wm_interm_50_so_pt4(a,j)
term(256) = term(256) + r1(vrdav_Rr, a,i) * wm_interm_10_so_pt4(q,p,i,j) * wm_interm_51_so_pt4(a,j)
term(257) = term(257) + r1(vrdav_Rr, a,i) * wm_interm_1_so_pt4(q,p,i,j) * wm_interm_51_so_pt4(a,j)
term(258) = term(258) + r1(vrdav_Rr, a,i) * wm_interm_11_so_pt4(q,p,i,j) * wm_interm_51_so_pt4(a,j)
end do 
end do 
end do 

term(132) = term(132) * (-2.0d+0) 
term(133) = term(133) * (4.0d+0) 
term(134) = term(134) * (-2.0d+0) 
term(136) = term(136) * (-2.0d+0) 
term(138) = term(138) * (-8.0d+0) 
term(139) = term(139) * (8.0d+0) 
term(140) = term(140) * (4.0d+0) 
term(141) = term(141) * (-4.0d+0) 
term(142) = term(142) * (2.0d+0) 
term(143) = term(143) * (-1.0d+0) 
term(144) = term(144) * (-1.0d+0) 
term(145) = term(145) * (4.0d+0) 
term(146) = term(146) * (-4.0d+0) 
term(147) = term(147) * (-4.0d+0) 
term(148) = term(148) * (2.0d+0) 
term(149) = term(149) * (2.0d+0) 
term(150) = term(150) * (2.0d+0) 
term(151) = term(151) * (-1.0d+0) 
term(152) = term(152) * (-1.0d+0) 
term(153) = term(153) * (-8.0d+0) 
term(154) = term(154) * (8.0d+0) 
term(155) = term(155) * (4.0d+0) 
term(156) = term(156) * (-4.0d+0) 
term(157) = term(157) * (-2.0d+0) 
term(158) = term(158) * (4.0d+0) 
term(159) = term(159) * (-2.0d+0) 
term(160) = term(160) * (4.0d+0) 
term(161) = term(161) * (4.0d+0) 
term(162) = term(162) * (-8.0d+0) 
term(163) = term(163) * (4.0d+0) 
term(164) = term(164) * (-8.0d+0) 
term(165) = term(165) * (-2.0d+0) 
term(166) = term(166) * (-2.0d+0) 
term(167) = term(167) * (4.0d+0) 
term(168) = term(168) * (4.0d+0) 
term(169) = term(169) * (4.0d+0) 
term(170) = term(170) * (-8.0d+0) 
term(171) = term(171) * (4.0d+0) 
term(172) = term(172) * (-8.0d+0) 
term(173) = term(173) * (4.0d+0) 
term(174) = term(174) * (4.0d+0) 
term(175) = term(175) * (-2.0d+0) 
term(176) = term(176) * (4.0d+0) 
term(177) = term(177) * (-2.0d+0) 
term(178) = term(178) * (4.0d+0) 
term(179) = term(179) * (-2.0d+0) 
term(180) = term(180) * (-2.0d+0) 
term(181) = term(181) * (-8.0d+0) 
term(182) = term(182) * (8.0d+0) 
term(183) = term(183) * (-8.0d+0) 
term(184) = term(184) * (8.0d+0) 
term(185) = term(185) * (16.0d+0) 
term(186) = term(186) * (-16.0d+0) 
term(187) = term(187) * (16.0d+0) 
term(188) = term(188) * (-16.0d+0) 
term(189) = term(189) * (16.0d+0) 
term(190) = term(190) * (-16.0d+0) 
term(191) = term(191) * (16.0d+0) 
term(192) = term(192) * (-16.0d+0) 
term(193) = term(193) * (-8.0d+0) 
term(194) = term(194) * (8.0d+0) 
term(195) = term(195) * (-8.0d+0) 
term(196) = term(196) * (8.0d+0) 
term(197) = term(197) * (-2.0d+0) 
term(198) = term(198) * (4.0d+0) 
term(199) = term(199) * (4.0d+0) 
term(200) = term(200) * (-8.0d+0) 
term(201) = term(201) * (-2.0d+0) 
term(202) = term(202) * (4.0d+0) 
term(204) = term(204) * (-2.0d+0) 
term(205) = term(205) * (-2.0d+0) 
term(206) = term(206) * (4.0d+0) 
term(208) = term(208) * (-2.0d+0) 
term(209) = term(209) * (-8.0d+0) 
term(210) = term(210) * (8.0d+0) 
term(211) = term(211) * (16.0d+0) 
term(212) = term(212) * (-16.0d+0) 
term(213) = term(213) * (4.0d+0) 
term(214) = term(214) * (-4.0d+0) 
term(215) = term(215) * (-8.0d+0) 
term(216) = term(216) * (8.0d+0) 
term(217) = term(217) * (2.0d+0) 
term(218) = term(218) * (-1.0d+0) 
term(219) = term(219) * (-1.0d+0) 
term(220) = term(220) * (-4.0d+0) 
term(221) = term(221) * (2.0d+0) 
term(222) = term(222) * (2.0d+0) 
term(223) = term(223) * (4.0d+0) 
term(224) = term(224) * (-4.0d+0) 
term(225) = term(225) * (-8.0d+0) 
term(226) = term(226) * (8.0d+0) 
term(227) = term(227) * (-2.0d+0) 
term(228) = term(228) * (4.0d+0) 
term(229) = term(229) * (-2.0d+0) 
term(230) = term(230) * (4.0d+0) 
term(231) = term(231) * (-8.0d+0) 
term(232) = term(232) * (4.0d+0) 
term(233) = term(233) * (-8.0d+0) 
term(234) = term(234) * (8.0d+0) 
term(235) = term(235) * (16.0d+0) 
term(236) = term(236) * (-16.0d+0) 
term(237) = term(237) * (16.0d+0) 
term(238) = term(238) * (-32.0d+0) 
term(239) = term(239) * (-8.0d+0) 
term(240) = term(240) * (16.0d+0) 
term(241) = term(241) * (-8.0d+0) 
term(242) = term(242) * (16.0d+0) 
term(243) = term(243) * (4.0d+0) 
term(244) = term(244) * (-8.0d+0) 
term(245) = term(245) * (2.0d+0) 
term(246) = term(246) * (2.0d+0) 
term(247) = term(247) * (2.0d+0) 
term(248) = term(248) * (-4.0d+0) 
term(249) = term(249) * (2.0d+0) 
term(250) = term(250) * (2.0d+0) 
term(251) = term(251) * (2.0d+0) 
term(252) = term(252) * (-4.0d+0) 
term(253) = term(253) * (-4.0d+0) 
term(254) = term(254) * (-4.0d+0) 
term(255) = term(255) * (8.0d+0) 
term(256) = term(256) * (2.0d+0) 
term(257) = term(257) * (2.0d+0) 
term(258) = term(258) * (-4.0d+0) 

do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
term(259) = term(259) + r1(vrdav_Rr, p,i) * wm_interm_17_so_pt4(j,k) * wm_interm_47_so_pt4(q,i,k,j)
term(260) = term(260) + r1(vrdav_Rr, p,i) * wm_interm_18_so_pt4(j,k) * wm_interm_47_so_pt4(q,i,k,j)
term(261) = term(261) + r1(vrdav_Rr, p,i) * wm_interm_17_so_pt4(j,k) * wm_interm_49_so_pt4(q,i,k,j)
term(262) = term(262) + r1(vrdav_Rr, p,i) * wm_interm_18_so_pt4(j,k) * wm_interm_49_so_pt4(q,i,k,j)
term(263) = term(263) + t1(p,i) * wm_interm_17_so_pt4(j,k) * wm_interm_46_so_pt4(q,i,k,j)
term(264) = term(264) + t1(p,i) * wm_interm_18_so_pt4(j,k) * wm_interm_46_so_pt4(q,i,k,j)
end do 
end do 
end do 

term(259) = term(259) * (-4.0d+0) 
term(260) = term(260) * (8.0d+0) 
term(261) = term(261) * (2.0d+0) 
term(262) = term(262) * (-4.0d+0) 
term(264) = term(264) * (-2.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(265) = term(265) + s2(a,q,j,i) * wm_interm_24_so_pt4(p,k,j,i) * wm_interm_6_so_pt4(a,k)
term(266) = term(266) + s2(a,q,j,i) * wm_interm_24_so_pt4(p,k,j,i) * wm_interm_7_so_pt4(a,k)
term(267) = term(267) + s2(a,q,j,i) * wm_interm_29_so_pt4(p,k,j,i) * wm_interm_6_so_pt4(a,k)
term(268) = term(268) + s2(a,q,j,i) * wm_interm_29_so_pt4(p,k,j,i) * wm_interm_7_so_pt4(a,k)
end do 
end do 
end do 
end do 

term(265) = term(265) * (4.0d+0) 
term(266) = term(266) * (-2.0d+0) 
term(267) = term(267) * (-16.0d+0) 
term(268) = term(268) * (8.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
term(269) = term(269) + wm_interm_25_so_pt4(i,j) * wm_interm_65_so_pt4(q,p,j,i)
term(270) = term(270) + wm_interm_25_so_pt4(i,j) * wm_interm_66_so_pt4(q,p,j,i)
term(271) = term(271) + wm_interm_25_so_pt4(i,j) * wm_interm_77_so_pt4(q,p,j,i)
term(272) = term(272) + wm_interm_25_so_pt4(i,j) * wm_interm_79_so_pt4(q,p,j,i)
term(273) = term(273) + wm_interm_25_so_pt4(i,j) * wm_interm_75_so_pt4(q,p,j,i)
term(274) = term(274) + wm_interm_25_so_pt4(i,j) * wm_interm_64_so_pt4(q,p,j,i)
term(275) = term(275) + wm_interm_25_so_pt4(i,j) * wm_interm_94_so_pt4(q,p,j,i)
term(276) = term(276) + wm_interm_25_so_pt4(i,j) * wm_interm_93_so_pt4(q,p,j,i)
term(277) = term(277) + wm_interm_101_so_pt4(i,j) * wm_interm_114_so_pt4(q,p,j,i)
term(278) = term(278) + wm_interm_103_so_pt4(i,j) * wm_interm_114_so_pt4(q,p,j,i)
end do 
end do 

term(270) = term(270) * (-2.0d+0) 
term(272) = term(272) * (-2.0d+0) 
term(275) = term(275) * (4.0d+0) 
term(276) = term(276) * (-4.0d+0) 
term(277) = term(277) * (4.0d+0) 
term(278) = term(278) * (-4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(279) = term(279) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_19_so_pt4(a,b) * wm_interm_32_so_pt4(b,q,j,i)
term(280) = term(280) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_20_so_pt4(a,b) * wm_interm_32_so_pt4(b,q,j,i)
term(281) = term(281) + t2(a,p,j,i) * wm_interm_32_so_pt4(b,q,i,j) * wm_interm_83_so_pt4(b,a)
term(282) = term(282) + t2(a,p,j,i) * wm_interm_32_so_pt4(b,q,i,j) * wm_interm_81_so_pt4(b,a)
term(283) = term(283) + t2(a,p,j,i) * wm_interm_32_so_pt4(b,q,i,j) * wm_interm_82_so_pt4(b,a)
term(284) = term(284) + t2(a,p,j,i) * wm_interm_104_so_pt4(b,a) * wm_interm_32_so_pt4(b,q,i,j)
term(285) = term(285) + t2(a,p,j,i) * wm_interm_105_so_pt4(b,a) * wm_interm_32_so_pt4(b,q,i,j)
term(286) = term(286) + r2(vrdav_Rl, a,j,q,i) * wm_interm_19_so_pt4(b,a) * wm_interm_52_so_pt4(b,p,i,j)
term(287) = term(287) + r2(vrdav_Rl, a,j,q,i) * wm_interm_20_so_pt4(b,a) * wm_interm_52_so_pt4(b,p,i,j)
term(288) = term(288) + s2(a,q,j,i) * wm_interm_52_so_pt4(b,p,i,j) * wm_interm_57_so_pt4(b,a)
term(289) = term(289) + s2(a,q,j,i) * wm_interm_52_so_pt4(b,p,i,j) * wm_interm_58_so_pt4(b,a)
end do 
end do 
end do 
end do 

term(279) = term(279) * (-2.0d+0) 
term(280) = term(280) * (4.0d+0) 
term(281) = term(281) * (-2.0d+0) 
term(282) = term(282) * (-2.0d+0) 
term(283) = term(283) * (4.0d+0) 
term(284) = term(284) * (-8.0d+0) 
term(285) = term(285) * (8.0d+0) 
term(286) = term(286) * (2.0d+0) 
term(287) = term(287) * (-4.0d+0) 
term(288) = term(288) * (-4.0d+0) 
term(289) = term(289) * (2.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(290) = term(290) + r1(vrdav_Rl, q,i) * wm_interm_3_so_pt4(p,j,k,l) * wm_interm_88_so_pt4(i,j,k,l)
term(291) = term(291) + r1(vrdav_Rl, q,i) * wm_interm_3_so_pt4(p,j,k,l) * wm_interm_88_so_pt4(j,i,l,k)
term(292) = term(292) + r1(vrdav_Rl, q,i) * wm_interm_3_so_pt4(p,j,k,l) * wm_interm_88_so_pt4(i,j,l,k)
term(293) = term(293) + s1(q,i) * wm_interm_13_so_pt4(p,j,k,l) * wm_interm_88_so_pt4(i,j,k,l)
term(294) = term(294) + s1(q,i) * wm_interm_13_so_pt4(p,j,k,l) * wm_interm_88_so_pt4(j,i,l,k)
term(295) = term(295) + s1(q,i) * wm_interm_13_so_pt4(p,j,k,l) * wm_interm_88_so_pt4(i,j,l,k)
term(296) = term(296) + r1(vrdav_Rl, q,i) * wm_interm_109_so_pt4(i,j,k,l) * wm_interm_3_so_pt4(p,j,k,l)
term(297) = term(297) + r1(vrdav_Rl, q,i) * wm_interm_109_so_pt4(j,i,k,l) * wm_interm_3_so_pt4(p,j,l,k)
term(298) = term(298) + r1(vrdav_Rl, q,i) * wm_interm_109_so_pt4(i,j,k,l) * wm_interm_3_so_pt4(p,j,l,k)
term(299) = term(299) + s1(q,i) * wm_interm_109_so_pt4(i,j,k,l) * wm_interm_13_so_pt4(p,j,k,l)
term(300) = term(300) + s1(q,i) * wm_interm_109_so_pt4(j,i,k,l) * wm_interm_13_so_pt4(p,j,l,k)
term(301) = term(301) + s1(q,i) * wm_interm_109_so_pt4(i,j,k,l) * wm_interm_13_so_pt4(p,j,l,k)
term(302) = term(302) + r1(vrdav_Rr, p,i) * wm_interm_22_so_pt4(q,j,k,l) * wm_interm_61_so_pt4(i,l,j,k)
term(303) = term(303) + r1(vrdav_Rr, p,i) * wm_interm_47_so_pt4(q,j,k,l) * wm_interm_9_so_pt4(l,i,j,k)
term(304) = term(304) + r1(vrdav_Rr, p,i) * wm_interm_47_so_pt4(q,j,k,l) * wm_interm_9_so_pt4(i,l,j,k)
term(305) = term(305) + r1(vrdav_Rr, p,i) * wm_interm_22_so_pt4(q,j,k,l) * wm_interm_61_so_pt4(l,i,j,k)
term(306) = term(306) + r1(vrdav_Rr, p,i) * wm_interm_49_so_pt4(q,j,k,l) * wm_interm_9_so_pt4(i,l,j,k)
term(307) = term(307) + r1(vrdav_Rr, p,i) * wm_interm_49_so_pt4(q,j,k,l) * wm_interm_9_so_pt4(l,i,j,k)
term(308) = term(308) + t1(p,i) * wm_interm_48_so_pt4(q,j,k,l) * wm_interm_9_so_pt4(l,i,j,k)
term(309) = term(309) + t1(p,i) * wm_interm_46_so_pt4(q,j,k,l) * wm_interm_9_so_pt4(i,l,j,k)
term(310) = term(310) + t1(p,i) * wm_interm_12_so_pt4(q,j,k,l) * wm_interm_61_so_pt4(i,j,k,l)
term(311) = term(311) + t1(p,i) * wm_interm_12_so_pt4(q,j,k,l) * wm_interm_61_so_pt4(j,i,l,k)
end do 
end do 
end do 
end do 

term(290) = term(290) * (-2.0d+0) 
term(291) = term(291) * (-2.0d+0) 
term(292) = term(292) * (4.0d+0) 
term(293) = term(293) * (-2.0d+0) 
term(294) = term(294) * (-2.0d+0) 
term(295) = term(295) * (4.0d+0) 
term(296) = term(296) * (-4.0d+0) 
term(297) = term(297) * (-4.0d+0) 
term(298) = term(298) * (8.0d+0) 
term(299) = term(299) * (-4.0d+0) 
term(300) = term(300) * (-4.0d+0) 
term(301) = term(301) * (8.0d+0) 
term(302) = term(302) * (-4.0d+0) 
term(303) = term(303) * (2.0d+0) 
term(304) = term(304) * (-4.0d+0) 
term(305) = term(305) * (2.0d+0) 
term(306) = term(306) * (2.0d+0) 
term(307) = term(307) * (-4.0d+0) 

do l = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
term(312) = term(312) + r1(vrdav_Rr, p,i) * wm_interm_22_so_pt4(q,j,k,l) * wm_interm_61_so_pt4(i,l,k,j)
term(313) = term(313) + r1(vrdav_Rr, p,i) * wm_interm_22_so_pt4(q,j,k,l) * wm_interm_61_so_pt4(l,i,k,j)
end do 
end do 
end do 
end do 

term(312) = term(312) * (2.0d+0) 
term(313) = term(313) * (-4.0d+0) 

do j = 1, nocc 
do a = nocc + 1, nactive 
do i = 1, nocc 
do b = nocc + 1, nactive 
term(314) = term(314) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_19_so_pt4(a,b) * wm_interm_32_so_pt4(b,q,i,j)
term(315) = term(315) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_20_so_pt4(a,b) * wm_interm_32_so_pt4(b,q,i,j)
end do 
end do 
end do 
end do 

term(315) = term(315) * (-2.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
term(316) = term(316) + wm_interm_100_so_pt4(q,p,i,j) * wm_interm_25_so_pt4(j,i)
term(317) = term(317) + wm_interm_102_so_pt4(q,p,i,j) * wm_interm_25_so_pt4(j,i)
term(318) = term(318) + wm_interm_114_so_pt4(q,p,i,j) * wm_interm_78_so_pt4(j,i)
term(319) = term(319) + wm_interm_114_so_pt4(q,p,i,j) * wm_interm_80_so_pt4(j,i)
term(320) = term(320) + wm_interm_114_so_pt4(q,p,i,j) * wm_interm_76_so_pt4(j,i)
term(321) = term(321) + r1(vrdav_Rl, q,j) * wm_interm_17_so_pt4(i,j) * wm_interm_33_so_pt4(p,i)
term(322) = term(322) + r1(vrdav_Rl, q,j) * wm_interm_18_so_pt4(i,j) * wm_interm_33_so_pt4(p,i)
term(323) = term(323) + r1(vrdav_Rl, q,j) * wm_interm_17_so_pt4(i,j) * wm_interm_34_so_pt4(p,i)
term(324) = term(324) + r1(vrdav_Rl, q,j) * wm_interm_18_so_pt4(i,j) * wm_interm_34_so_pt4(p,i)
term(325) = term(325) + r1(vrdav_Rl, q,j) * wm_interm_17_so_pt4(i,j) * wm_interm_35_so_pt4(p,i)
term(326) = term(326) + r1(vrdav_Rl, q,j) * wm_interm_18_so_pt4(i,j) * wm_interm_35_so_pt4(p,i)
term(327) = term(327) + r1(vrdav_Rl, q,j) * wm_interm_17_so_pt4(i,j) * wm_interm_36_so_pt4(p,i)
term(328) = term(328) + r1(vrdav_Rl, q,j) * wm_interm_18_so_pt4(i,j) * wm_interm_36_so_pt4(p,i)
term(329) = term(329) + r1(vrdav_Rl, q,j) * wm_interm_17_so_pt4(i,j) * wm_interm_37_so_pt4(p,i)
term(330) = term(330) + r1(vrdav_Rl, q,j) * wm_interm_18_so_pt4(i,j) * wm_interm_37_so_pt4(p,i)
term(331) = term(331) + r1(vrdav_Rl, q,j) * wm_interm_7_so_pt4(p,i) * wm_interm_85_so_pt4(j,i)
term(332) = term(332) + r1(vrdav_Rl, q,j) * wm_interm_7_so_pt4(p,i) * wm_interm_86_so_pt4(j,i)
term(333) = term(333) + r1(vrdav_Rl, q,j) * wm_interm_7_so_pt4(p,i) * wm_interm_87_so_pt4(j,i)
term(334) = term(334) + r1(vrdav_Rl, q,j) * wm_interm_6_so_pt4(p,i) * wm_interm_85_so_pt4(j,i)
term(335) = term(335) + r1(vrdav_Rl, q,j) * wm_interm_6_so_pt4(p,i) * wm_interm_86_so_pt4(j,i)
term(336) = term(336) + r1(vrdav_Rl, q,j) * wm_interm_6_so_pt4(p,i) * wm_interm_87_so_pt4(j,i)
term(337) = term(337) + s1(q,j) * wm_interm_16_so_pt4(p,i) * wm_interm_85_so_pt4(j,i)
term(338) = term(338) + s1(q,j) * wm_interm_16_so_pt4(p,i) * wm_interm_86_so_pt4(j,i)
term(339) = term(339) + s1(q,j) * wm_interm_16_so_pt4(p,i) * wm_interm_87_so_pt4(j,i)
term(340) = term(340) + s1(q,j) * wm_interm_14_so_pt4(p,i) * wm_interm_85_so_pt4(j,i)
term(341) = term(341) + s1(q,j) * wm_interm_14_so_pt4(p,i) * wm_interm_86_so_pt4(j,i)
term(342) = term(342) + s1(q,j) * wm_interm_14_so_pt4(p,i) * wm_interm_87_so_pt4(j,i)
term(343) = term(343) + r1(vrdav_Rl, q,j) * wm_interm_106_so_pt4(j,i) * wm_interm_7_so_pt4(p,i)
term(344) = term(344) + r1(vrdav_Rl, q,j) * wm_interm_107_so_pt4(j,i) * wm_interm_7_so_pt4(p,i)
term(345) = term(345) + r1(vrdav_Rl, q,j) * wm_interm_108_so_pt4(j,i) * wm_interm_7_so_pt4(p,i)
term(346) = term(346) + r1(vrdav_Rl, q,j) * wm_interm_106_so_pt4(j,i) * wm_interm_6_so_pt4(p,i)
term(347) = term(347) + r1(vrdav_Rl, q,j) * wm_interm_107_so_pt4(j,i) * wm_interm_6_so_pt4(p,i)
term(348) = term(348) + r1(vrdav_Rl, q,j) * wm_interm_108_so_pt4(j,i) * wm_interm_6_so_pt4(p,i)
term(349) = term(349) + s1(q,j) * wm_interm_106_so_pt4(j,i) * wm_interm_16_so_pt4(p,i)
term(350) = term(350) + s1(q,j) * wm_interm_107_so_pt4(j,i) * wm_interm_16_so_pt4(p,i)
term(351) = term(351) + s1(q,j) * wm_interm_108_so_pt4(j,i) * wm_interm_16_so_pt4(p,i)
term(352) = term(352) + s1(q,j) * wm_interm_106_so_pt4(j,i) * wm_interm_14_so_pt4(p,i)
term(353) = term(353) + s1(q,j) * wm_interm_107_so_pt4(j,i) * wm_interm_14_so_pt4(p,i)
term(354) = term(354) + s1(q,j) * wm_interm_108_so_pt4(j,i) * wm_interm_14_so_pt4(p,i)
term(355) = term(355) + r1(vrdav_Rr, p,i) * wm_interm_17_so_pt4(i,j) * wm_interm_50_so_pt4(q,j)
term(356) = term(356) + r1(vrdav_Rr, p,i) * wm_interm_18_so_pt4(i,j) * wm_interm_50_so_pt4(q,j)
term(357) = term(357) + r1(vrdav_Rr, p,i) * wm_interm_17_so_pt4(i,j) * wm_interm_51_so_pt4(q,j)
term(358) = term(358) + r1(vrdav_Rr, p,i) * wm_interm_18_so_pt4(i,j) * wm_interm_51_so_pt4(q,j)
term(359) = term(359) + t1(p,i) * wm_interm_17_so_pt4(i,j) * wm_interm_45_so_pt4(q,j)
term(360) = term(360) + t1(p,i) * wm_interm_18_so_pt4(i,j) * wm_interm_45_so_pt4(q,j)
term(361) = term(361) + t1(p,i) * wm_interm_2_so_pt4(q,j) * wm_interm_60_so_pt4(i,j)
term(362) = term(362) + t1(p,i) * wm_interm_2_so_pt4(q,j) * wm_interm_62_so_pt4(i,j)
end do 
end do 

term(316) = term(316) * (4.0d+0) 
term(317) = term(317) * (-4.0d+0) 
term(319) = term(319) * (-2.0d+0) 
term(321) = term(321) * (2.0d+0) 
term(322) = term(322) * (-4.0d+0) 
term(323) = term(323) * (-1.0d+0) 
term(324) = term(324) * (2.0d+0) 
term(325) = term(325) * (-1.0d+0) 
term(326) = term(326) * (2.0d+0) 
term(327) = term(327) * (4.0d+0) 
term(328) = term(328) * (-8.0d+0) 
term(329) = term(329) * (-4.0d+0) 
term(330) = term(330) * (8.0d+0) 
term(331) = term(331) * (-2.0d+0) 
term(332) = term(332) * (4.0d+0) 
term(333) = term(333) * (-2.0d+0) 
term(334) = term(334) * (4.0d+0) 
term(335) = term(335) * (-8.0d+0) 
term(336) = term(336) * (4.0d+0) 
term(337) = term(337) * (-2.0d+0) 
term(338) = term(338) * (4.0d+0) 
term(339) = term(339) * (-2.0d+0) 
term(340) = term(340) * (4.0d+0) 
term(341) = term(341) * (-8.0d+0) 
term(342) = term(342) * (4.0d+0) 
term(343) = term(343) * (-4.0d+0) 
term(344) = term(344) * (8.0d+0) 
term(345) = term(345) * (-4.0d+0) 
term(346) = term(346) * (8.0d+0) 
term(347) = term(347) * (-16.0d+0) 
term(348) = term(348) * (8.0d+0) 
term(349) = term(349) * (-4.0d+0) 
term(350) = term(350) * (8.0d+0) 
term(351) = term(351) * (-4.0d+0) 
term(352) = term(352) * (8.0d+0) 
term(353) = term(353) * (-16.0d+0) 
term(354) = term(354) * (8.0d+0) 
term(355) = term(355) * (-8.0d+0) 
term(356) = term(356) * (16.0d+0) 
term(357) = term(357) * (4.0d+0) 
term(358) = term(358) * (-8.0d+0) 
term(359) = term(359) * (2.0d+0) 
term(360) = term(360) * (-4.0d+0) 
term(361) = term(361) * (2.0d+0) 
term(362) = term(362) * (-4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
do l = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
term(363) = term(363) + r2p(vrdav_Rr, p,i,a,j) * s1(b,j) * s2(a,q,l,k) * wm_interm_13_so_pt4(b,i,l,k)
term(364) = term(364) + r2p(vrdav_Rr, p,i,a,j) * s1(b,i) * s2(a,q,l,k) * wm_interm_13_so_pt4(b,j,l,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(363) = term(363) * (-2.0d+0) 
term(364) = term(364) * (4.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
do i = 1, nocc 
do b = nocc + 1, nactive 
term(365) = term(365) + r2p(vrdav_Rr, p,i,a,j) * s1(b,i) * s2(a,q,l,k) * wm_interm_13_so_pt4(b,j,k,l)
term(366) = term(366) + r2p(vrdav_Rr, p,i,a,j) * s1(b,j) * s2(a,q,l,k) * wm_interm_13_so_pt4(b,i,k,l)
end do 
end do 
end do 
end do 
end do 
end do 

term(365) = term(365) * (-2.0d+0) 
term(366) = term(366) * (4.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(367) = term(367) + r2p(vrdav_Rr, a,i,p,j) * s1(b,j) * s2(a,q,l,k) * wm_interm_13_so_pt4(b,i,k,l)
term(368) = term(368) + r2m(vrdav_Rr, a,i,p,j) * s1(b,j) * s2(a,q,l,k) * wm_interm_13_so_pt4(b,i,k,l)
end do 
end do 
end do 
end do 
end do 
end do 

term(367) = term(367) * (-2.0d+0) 
term(368) = term(368) * (-4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do l = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(369) = term(369) + r2p(vrdav_Rr, a,i,p,j) * s1(b,i) * s2(a,q,l,k) * wm_interm_13_so_pt4(b,j,l,k)
term(370) = term(370) + r2m(vrdav_Rr, a,i,p,j) * s1(b,i) * s2(a,q,l,k) * wm_interm_13_so_pt4(b,j,l,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(369) = term(369) * (-2.0d+0) 
term(370) = term(370) * (-4.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(371) = term(371) + r2m(vrdav_Rr, a,j,p,i) * s1(b,j) * s2(a,q,l,k) * wm_interm_13_so_pt4(b,i,l,k)
term(372) = term(372) + r2m(vrdav_Rr, a,j,p,i) * s1(b,i) * s2(a,q,l,k) * wm_interm_13_so_pt4(b,j,l,k)
term(373) = term(373) + s1(a,j) * s2(b,q,l,k) * t2(b,p,j,i) * wm_interm_26_so_pt4(a,i,l,k)
term(374) = term(374) + s1(a,j) * s2(b,q,l,k) * t2(b,p,j,i) * wm_interm_29_so_pt4(a,i,l,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(371) = term(371) * (-4.0d+0) 
term(372) = term(372) * (8.0d+0) 
term(373) = term(373) * (-2.0d+0) 
term(374) = term(374) * (-8.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(375) = term(375) + r2m(vrdav_Rr, a,j,p,i) * s1(b,i) * s2(a,q,l,k) * wm_interm_13_so_pt4(b,j,k,l)
term(376) = term(376) + r2m(vrdav_Rr, a,j,p,i) * s1(b,j) * s2(a,q,l,k) * wm_interm_13_so_pt4(b,i,k,l)
term(377) = term(377) + s1(a,j) * s2(b,q,l,k) * t2(b,p,j,i) * wm_interm_24_so_pt4(a,i,k,l)
term(378) = term(378) + s1(a,j) * s2(b,q,l,k) * t2(b,p,j,i) * wm_interm_26_so_pt4(a,i,k,l)
term(379) = term(379) + s1(a,j) * s2(b,q,l,k) * t2(b,p,j,i) * wm_interm_29_so_pt4(a,i,k,l)
end do 
end do 
end do 
end do 
end do 
end do 

term(375) = term(375) * (-4.0d+0) 
term(376) = term(376) * (8.0d+0) 
term(377) = term(377) * (-2.0d+0) 
term(378) = term(378) * (4.0d+0) 
term(379) = term(379) * (8.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(380) = term(380) + r1(vrdav_Rl, q,i) * wm_interm_3_so_pt4(a,j,k,i) * wm_interm_67_so_pt4(p,a,j,k)
term(381) = term(381) + r1(vrdav_Rl, q,i) * wm_interm_3_so_pt4(a,j,k,i) * wm_interm_68_so_pt4(p,a,j,k)
term(382) = term(382) + r1(vrdav_Rl, q,i) * wm_interm_3_so_pt4(a,j,k,i) * wm_interm_69_so_pt4(p,a,j,k)
term(383) = term(383) + r1(vrdav_Rl, q,i) * wm_interm_3_so_pt4(a,j,k,i) * wm_interm_70_so_pt4(p,a,j,k)
term(384) = term(384) + s1(a,i) * wm_interm_1_so_pt4(q,a,j,k) * wm_interm_24_so_pt4(p,k,j,i)
term(385) = term(385) + s1(a,i) * wm_interm_1_so_pt4(q,a,j,k) * wm_interm_26_so_pt4(p,k,j,i)
term(386) = term(386) + r1(vrdav_Rl, q,i) * wm_interm_3_so_pt4(a,j,k,i) * wm_interm_73_so_pt4(p,a,j,k)
term(387) = term(387) + r1(vrdav_Rl, q,i) * wm_interm_3_so_pt4(a,j,k,i) * wm_interm_74_so_pt4(p,a,j,k)
term(388) = term(388) + s1(a,i) * wm_interm_10_so_pt4(q,a,j,k) * wm_interm_24_so_pt4(p,k,j,i)
term(389) = term(389) + s1(a,i) * wm_interm_11_so_pt4(q,a,j,k) * wm_interm_24_so_pt4(p,k,j,i)
term(390) = term(390) + s1(a,i) * wm_interm_10_so_pt4(q,a,j,k) * wm_interm_26_so_pt4(p,k,j,i)
term(391) = term(391) + s1(a,i) * wm_interm_11_so_pt4(q,a,j,k) * wm_interm_26_so_pt4(p,k,j,i)
term(392) = term(392) + s1(q,i) * wm_interm_13_so_pt4(a,j,k,i) * wm_interm_67_so_pt4(p,a,j,k)
term(393) = term(393) + s1(q,i) * wm_interm_13_so_pt4(a,j,k,i) * wm_interm_68_so_pt4(p,a,j,k)
term(394) = term(394) + s1(q,i) * wm_interm_13_so_pt4(a,j,k,i) * wm_interm_69_so_pt4(p,a,j,k)
term(395) = term(395) + s1(q,i) * wm_interm_13_so_pt4(a,j,k,i) * wm_interm_70_so_pt4(p,a,j,k)
term(396) = term(396) + s2(a,q,j,i) * wm_interm_14_so_pt4(a,k) * wm_interm_27_so_pt4(p,j,i,k)
term(397) = term(397) + s2(a,q,j,i) * wm_interm_14_so_pt4(a,k) * wm_interm_27_so_pt4(p,i,j,k)
term(398) = term(398) + s1(q,i) * wm_interm_13_so_pt4(a,j,k,i) * wm_interm_74_so_pt4(p,a,j,k)
term(399) = term(399) + s2(a,q,j,i) * wm_interm_14_so_pt4(a,k) * wm_interm_28_so_pt4(p,i,j,k)
term(400) = term(400) + s1(q,i) * wm_interm_13_so_pt4(a,j,k,i) * wm_interm_73_so_pt4(p,a,j,k)
term(401) = term(401) + s2(a,q,j,i) * wm_interm_16_so_pt4(a,k) * wm_interm_27_so_pt4(p,j,i,k)
term(402) = term(402) + s2(a,q,j,i) * wm_interm_16_so_pt4(a,k) * wm_interm_27_so_pt4(p,i,j,k)
term(403) = term(403) + s2(a,q,j,i) * wm_interm_16_so_pt4(a,k) * wm_interm_28_so_pt4(p,i,j,k)
term(404) = term(404) + r1(vrdav_Rl, q,i) * wm_interm_3_so_pt4(a,j,k,i) * wm_interm_95_so_pt4(p,a,j,k)
term(405) = term(405) + r1(vrdav_Rl, q,i) * wm_interm_3_so_pt4(a,j,k,i) * wm_interm_96_so_pt4(p,a,j,k)
term(406) = term(406) + r1(vrdav_Rl, q,i) * wm_interm_3_so_pt4(a,j,k,i) * wm_interm_97_so_pt4(p,a,j,k)
term(407) = term(407) + r1(vrdav_Rl, q,i) * wm_interm_3_so_pt4(a,j,k,i) * wm_interm_98_so_pt4(p,a,j,k)
term(408) = term(408) + s1(a,i) * wm_interm_1_so_pt4(q,a,j,k) * wm_interm_29_so_pt4(p,k,j,i)
term(409) = term(409) + s1(a,i) * wm_interm_10_so_pt4(q,a,j,k) * wm_interm_29_so_pt4(p,k,j,i)
term(410) = term(410) + s1(a,i) * wm_interm_11_so_pt4(q,a,j,k) * wm_interm_29_so_pt4(p,k,j,i)
term(411) = term(411) + s1(q,i) * wm_interm_13_so_pt4(a,j,k,i) * wm_interm_95_so_pt4(p,a,j,k)
term(412) = term(412) + s1(q,i) * wm_interm_13_so_pt4(a,j,k,i) * wm_interm_96_so_pt4(p,a,j,k)
term(413) = term(413) + s1(q,i) * wm_interm_13_so_pt4(a,j,k,i) * wm_interm_97_so_pt4(p,a,j,k)
term(414) = term(414) + s1(q,i) * wm_interm_13_so_pt4(a,j,k,i) * wm_interm_98_so_pt4(p,a,j,k)
term(415) = term(415) + s2(a,q,j,i) * wm_interm_14_so_pt4(a,k) * wm_interm_30_so_pt4(p,j,i,k)
term(416) = term(416) + s2(a,q,j,i) * wm_interm_14_so_pt4(a,k) * wm_interm_30_so_pt4(p,i,j,k)
term(417) = term(417) + s2(a,q,j,i) * wm_interm_14_so_pt4(a,k) * wm_interm_31_so_pt4(p,i,j,k)
term(418) = term(418) + s2(a,q,j,i) * wm_interm_16_so_pt4(a,k) * wm_interm_30_so_pt4(p,j,i,k)
term(419) = term(419) + s2(a,q,j,i) * wm_interm_16_so_pt4(a,k) * wm_interm_30_so_pt4(p,i,j,k)
term(420) = term(420) + s2(a,q,j,i) * wm_interm_16_so_pt4(a,k) * wm_interm_31_so_pt4(p,i,j,k)
term(421) = term(421) + r1(vrdav_Rl, a,i) * wm_interm_3_so_pt4(p,j,k,i) * wm_interm_74_so_pt4(a,q,j,k)
term(422) = term(422) + r1(vrdav_Rl, a,i) * wm_interm_3_so_pt4(p,j,k,i) * wm_interm_67_so_pt4(a,q,j,k)
term(423) = term(423) + r1(vrdav_Rl, a,i) * wm_interm_3_so_pt4(p,j,k,i) * wm_interm_68_so_pt4(a,q,j,k)
term(424) = term(424) + r1(vrdav_Rl, a,i) * wm_interm_3_so_pt4(p,j,k,i) * wm_interm_73_so_pt4(a,q,j,k)
term(425) = term(425) + r1(vrdav_Rl, a,i) * wm_interm_3_so_pt4(p,j,k,i) * wm_interm_69_so_pt4(a,q,j,k)
term(426) = term(426) + r1(vrdav_Rl, a,i) * wm_interm_3_so_pt4(p,j,k,i) * wm_interm_70_so_pt4(a,q,j,k)
term(427) = term(427) + s1(q,i) * wm_interm_10_so_pt4(a,p,j,k) * wm_interm_26_so_pt4(a,k,j,i)
term(428) = term(428) + s1(q,i) * wm_interm_1_so_pt4(a,p,j,k) * wm_interm_26_so_pt4(a,k,j,i)
term(429) = term(429) + s1(q,i) * wm_interm_11_so_pt4(a,p,j,k) * wm_interm_26_so_pt4(a,k,j,i)
term(430) = term(430) + s1(a,i) * wm_interm_13_so_pt4(p,j,k,i) * wm_interm_67_so_pt4(a,q,j,k)
term(431) = term(431) + s1(a,i) * wm_interm_13_so_pt4(p,j,k,i) * wm_interm_68_so_pt4(a,q,j,k)
term(432) = term(432) + s1(a,i) * wm_interm_13_so_pt4(p,j,k,i) * wm_interm_69_so_pt4(a,q,j,k)
term(433) = term(433) + s1(a,i) * wm_interm_13_so_pt4(p,j,k,i) * wm_interm_70_so_pt4(a,q,j,k)
term(434) = term(434) + s1(a,i) * wm_interm_13_so_pt4(p,j,k,i) * wm_interm_73_so_pt4(a,q,j,k)
term(435) = term(435) + s1(a,i) * wm_interm_13_so_pt4(p,j,k,i) * wm_interm_74_so_pt4(a,q,j,k)
term(436) = term(436) + r1(vrdav_Rl, a,i) * wm_interm_3_so_pt4(p,j,k,i) * wm_interm_95_so_pt4(a,q,j,k)
term(437) = term(437) + r1(vrdav_Rl, a,i) * wm_interm_3_so_pt4(p,j,k,i) * wm_interm_96_so_pt4(a,q,j,k)
term(438) = term(438) + r1(vrdav_Rl, a,i) * wm_interm_3_so_pt4(p,j,k,i) * wm_interm_97_so_pt4(a,q,j,k)
term(439) = term(439) + r1(vrdav_Rl, a,i) * wm_interm_3_so_pt4(p,j,k,i) * wm_interm_98_so_pt4(a,q,j,k)
term(440) = term(440) + s1(q,i) * wm_interm_10_so_pt4(a,p,j,k) * wm_interm_29_so_pt4(a,k,j,i)
term(441) = term(441) + s1(q,i) * wm_interm_1_so_pt4(a,p,j,k) * wm_interm_29_so_pt4(a,k,j,i)
term(442) = term(442) + s1(q,i) * wm_interm_11_so_pt4(a,p,j,k) * wm_interm_29_so_pt4(a,k,j,i)
term(443) = term(443) + s1(a,i) * wm_interm_13_so_pt4(p,j,k,i) * wm_interm_95_so_pt4(a,q,j,k)
term(444) = term(444) + s1(a,i) * wm_interm_13_so_pt4(p,j,k,i) * wm_interm_96_so_pt4(a,q,j,k)
term(445) = term(445) + s1(a,i) * wm_interm_13_so_pt4(p,j,k,i) * wm_interm_97_so_pt4(a,q,j,k)
term(446) = term(446) + s1(a,i) * wm_interm_13_so_pt4(p,j,k,i) * wm_interm_98_so_pt4(a,q,j,k)
term(447) = term(447) + t1(p,i) * wm_interm_12_so_pt4(a,j,k,i) * wm_interm_54_so_pt4(q,a,j,k)
term(448) = term(448) + t1(p,i) * wm_interm_12_so_pt4(a,j,k,i) * wm_interm_55_so_pt4(q,a,j,k)
term(449) = term(449) + t1(p,i) * wm_interm_12_so_pt4(a,j,k,i) * wm_interm_59_so_pt4(q,a,j,k)
term(450) = term(450) + t2(a,p,j,i) * wm_interm_22_so_pt4(q,j,i,k) * wm_interm_45_so_pt4(a,k)
term(451) = term(451) + t1(a,i) * wm_interm_12_so_pt4(q,j,k,i) * wm_interm_55_so_pt4(a,p,j,k)
term(452) = term(452) + t1(a,i) * wm_interm_12_so_pt4(q,j,k,i) * wm_interm_59_so_pt4(a,p,j,k)
term(453) = term(453) + t1(a,i) * wm_interm_12_so_pt4(q,j,k,i) * wm_interm_54_so_pt4(a,p,j,k)
term(454) = term(454) + t2(a,p,j,i) * wm_interm_2_so_pt4(a,k) * wm_interm_47_so_pt4(q,j,i,k)
term(455) = term(455) + t2(a,p,j,i) * wm_interm_2_so_pt4(a,k) * wm_interm_49_so_pt4(q,i,j,k)
end do 
end do 
end do 
end do 

term(380) = term(380) * (-2.0d+0) 
term(381) = term(381) * (4.0d+0) 
term(382) = term(382) * (-2.0d+0) 
term(383) = term(383) * (4.0d+0) 
term(384) = term(384) * (4.0d+0) 
term(385) = term(385) * (-2.0d+0) 
term(386) = term(386) * (-2.0d+0) 
term(387) = term(387) * (-2.0d+0) 
term(388) = term(388) * (4.0d+0) 
term(389) = term(389) * (-8.0d+0) 
term(390) = term(390) * (-2.0d+0) 
term(391) = term(391) * (4.0d+0) 
term(392) = term(392) * (-2.0d+0) 
term(393) = term(393) * (4.0d+0) 
term(394) = term(394) * (-2.0d+0) 
term(395) = term(395) * (4.0d+0) 
term(396) = term(396) * (4.0d+0) 
term(397) = term(397) * (-8.0d+0) 
term(398) = term(398) * (-2.0d+0) 
term(399) = term(399) * (4.0d+0) 
term(400) = term(400) * (-2.0d+0) 
term(401) = term(401) * (-2.0d+0) 
term(402) = term(402) * (4.0d+0) 
term(403) = term(403) * (-2.0d+0) 
term(404) = term(404) * (-8.0d+0) 
term(405) = term(405) * (8.0d+0) 
term(406) = term(406) * (-8.0d+0) 
term(407) = term(407) * (8.0d+0) 
term(408) = term(408) * (-8.0d+0) 
term(409) = term(409) * (-8.0d+0) 
term(410) = term(410) * (16.0d+0) 
term(411) = term(411) * (-8.0d+0) 
term(412) = term(412) * (8.0d+0) 
term(413) = term(413) * (-8.0d+0) 
term(414) = term(414) * (8.0d+0) 
term(415) = term(415) * (8.0d+0) 
term(416) = term(416) * (-16.0d+0) 
term(417) = term(417) * (8.0d+0) 
term(418) = term(418) * (-4.0d+0) 
term(419) = term(419) * (8.0d+0) 
term(420) = term(420) * (-4.0d+0) 
term(421) = term(421) * (-2.0d+0) 
term(422) = term(422) * (-2.0d+0) 
term(423) = term(423) * (4.0d+0) 
term(424) = term(424) * (-2.0d+0) 
term(425) = term(425) * (-2.0d+0) 
term(426) = term(426) * (4.0d+0) 
term(427) = term(427) * (-2.0d+0) 
term(428) = term(428) * (-2.0d+0) 
term(429) = term(429) * (4.0d+0) 
term(430) = term(430) * (-2.0d+0) 
term(431) = term(431) * (4.0d+0) 
term(432) = term(432) * (-2.0d+0) 
term(433) = term(433) * (4.0d+0) 
term(434) = term(434) * (-2.0d+0) 
term(435) = term(435) * (-2.0d+0) 
term(436) = term(436) * (-8.0d+0) 
term(437) = term(437) * (8.0d+0) 
term(438) = term(438) * (-8.0d+0) 
term(439) = term(439) * (8.0d+0) 
term(440) = term(440) * (-8.0d+0) 
term(441) = term(441) * (-8.0d+0) 
term(442) = term(442) * (16.0d+0) 
term(443) = term(443) * (-8.0d+0) 
term(444) = term(444) * (8.0d+0) 
term(445) = term(445) * (-8.0d+0) 
term(446) = term(446) * (8.0d+0) 
term(447) = term(447) * (-4.0d+0) 
term(448) = term(448) * (2.0d+0) 
term(449) = term(449) * (2.0d+0) 
term(450) = term(450) * (2.0d+0) 
term(451) = term(451) * (2.0d+0) 
term(452) = term(452) * (2.0d+0) 
term(453) = term(453) * (-4.0d+0) 

do a = nocc + 1, nactive 
do i = 1, nocc 
term(456) = term(456) + r1(vrdav_Rl, a,i) * wm_interm_19_so_pt4(q,a) * wm_interm_33_so_pt4(p,i)
term(457) = term(457) + r1(vrdav_Rl, a,i) * wm_interm_20_so_pt4(q,a) * wm_interm_33_so_pt4(p,i)
term(458) = term(458) + r1(vrdav_Rl, a,i) * wm_interm_19_so_pt4(q,a) * wm_interm_34_so_pt4(p,i)
term(459) = term(459) + r1(vrdav_Rl, a,i) * wm_interm_20_so_pt4(q,a) * wm_interm_34_so_pt4(p,i)
term(460) = term(460) + r1(vrdav_Rl, a,i) * wm_interm_19_so_pt4(q,a) * wm_interm_35_so_pt4(p,i)
term(461) = term(461) + r1(vrdav_Rl, a,i) * wm_interm_20_so_pt4(q,a) * wm_interm_35_so_pt4(p,i)
term(462) = term(462) + r1(vrdav_Rl, a,i) * wm_interm_19_so_pt4(q,a) * wm_interm_36_so_pt4(p,i)
term(463) = term(463) + r1(vrdav_Rl, a,i) * wm_interm_20_so_pt4(q,a) * wm_interm_36_so_pt4(p,i)
term(464) = term(464) + r1(vrdav_Rl, a,i) * wm_interm_19_so_pt4(q,a) * wm_interm_37_so_pt4(p,i)
term(465) = term(465) + r1(vrdav_Rl, a,i) * wm_interm_20_so_pt4(q,a) * wm_interm_37_so_pt4(p,i)
term(466) = term(466) + r1(vrdav_Rl, q,i) * wm_interm_6_so_pt4(a,i) * wm_interm_81_so_pt4(p,a)
term(467) = term(467) + r1(vrdav_Rl, q,i) * wm_interm_6_so_pt4(a,i) * wm_interm_82_so_pt4(p,a)
term(468) = term(468) + r1(vrdav_Rl, q,i) * wm_interm_6_so_pt4(a,i) * wm_interm_83_so_pt4(p,a)
term(469) = term(469) + r1(vrdav_Rl, q,i) * wm_interm_7_so_pt4(a,i) * wm_interm_81_so_pt4(p,a)
term(470) = term(470) + r1(vrdav_Rl, q,i) * wm_interm_7_so_pt4(a,i) * wm_interm_82_so_pt4(p,a)
term(471) = term(471) + r1(vrdav_Rl, q,i) * wm_interm_7_so_pt4(a,i) * wm_interm_83_so_pt4(p,a)
term(472) = term(472) + s1(q,i) * wm_interm_14_so_pt4(a,i) * wm_interm_81_so_pt4(p,a)
term(473) = term(473) + s1(q,i) * wm_interm_14_so_pt4(a,i) * wm_interm_82_so_pt4(p,a)
term(474) = term(474) + s1(q,i) * wm_interm_14_so_pt4(a,i) * wm_interm_83_so_pt4(p,a)
term(475) = term(475) + s1(q,i) * wm_interm_16_so_pt4(a,i) * wm_interm_81_so_pt4(p,a)
term(476) = term(476) + s1(q,i) * wm_interm_16_so_pt4(a,i) * wm_interm_82_so_pt4(p,a)
term(477) = term(477) + s1(q,i) * wm_interm_16_so_pt4(a,i) * wm_interm_83_so_pt4(p,a)
term(478) = term(478) + r1(vrdav_Rl, q,i) * wm_interm_104_so_pt4(p,a) * wm_interm_6_so_pt4(a,i)
term(479) = term(479) + r1(vrdav_Rl, q,i) * wm_interm_105_so_pt4(p,a) * wm_interm_6_so_pt4(a,i)
term(480) = term(480) + r1(vrdav_Rl, q,i) * wm_interm_104_so_pt4(p,a) * wm_interm_7_so_pt4(a,i)
term(481) = term(481) + r1(vrdav_Rl, q,i) * wm_interm_105_so_pt4(p,a) * wm_interm_7_so_pt4(a,i)
term(482) = term(482) + s1(q,i) * wm_interm_104_so_pt4(p,a) * wm_interm_14_so_pt4(a,i)
term(483) = term(483) + s1(q,i) * wm_interm_105_so_pt4(p,a) * wm_interm_14_so_pt4(a,i)
term(484) = term(484) + s1(q,i) * wm_interm_112_so_pt4(p,a) * wm_interm_14_so_pt4(a,i)
term(485) = term(485) + s1(q,i) * wm_interm_104_so_pt4(p,a) * wm_interm_16_so_pt4(a,i)
term(486) = term(486) + s1(q,i) * wm_interm_105_so_pt4(p,a) * wm_interm_16_so_pt4(a,i)
term(487) = term(487) + s1(q,i) * wm_interm_112_so_pt4(p,a) * wm_interm_16_so_pt4(a,i)
term(488) = term(488) + r1(vrdav_Rr, p,i) * wm_interm_19_so_pt4(q,a) * wm_interm_50_so_pt4(a,i)
term(489) = term(489) + r1(vrdav_Rr, p,i) * wm_interm_20_so_pt4(q,a) * wm_interm_50_so_pt4(a,i)
term(490) = term(490) + r1(vrdav_Rr, p,i) * wm_interm_19_so_pt4(q,a) * wm_interm_51_so_pt4(a,i)
term(491) = term(491) + r1(vrdav_Rr, p,i) * wm_interm_20_so_pt4(q,a) * wm_interm_51_so_pt4(a,i)
term(492) = term(492) + t1(p,i) * wm_interm_19_so_pt4(q,a) * wm_interm_45_so_pt4(a,i)
term(493) = term(493) + t1(p,i) * wm_interm_20_so_pt4(q,a) * wm_interm_45_so_pt4(a,i)
term(494) = term(494) + t1(p,i) * wm_interm_2_so_pt4(a,i) * wm_interm_57_so_pt4(q,a)
term(495) = term(495) + t1(p,i) * wm_interm_2_so_pt4(a,i) * wm_interm_58_so_pt4(q,a)
end do 
end do 

term(456) = term(456) * (2.0d+0) 
term(457) = term(457) * (-4.0d+0) 
term(458) = term(458) * (-1.0d+0) 
term(459) = term(459) * (2.0d+0) 
term(460) = term(460) * (-1.0d+0) 
term(461) = term(461) * (2.0d+0) 
term(462) = term(462) * (4.0d+0) 
term(463) = term(463) * (-8.0d+0) 
term(464) = term(464) * (-4.0d+0) 
term(465) = term(465) * (8.0d+0) 
term(466) = term(466) * (4.0d+0) 
term(467) = term(467) * (-8.0d+0) 
term(468) = term(468) * (4.0d+0) 
term(469) = term(469) * (-2.0d+0) 
term(470) = term(470) * (4.0d+0) 
term(471) = term(471) * (-2.0d+0) 
term(472) = term(472) * (4.0d+0) 
term(473) = term(473) * (-8.0d+0) 
term(474) = term(474) * (4.0d+0) 
term(475) = term(475) * (-2.0d+0) 
term(476) = term(476) * (4.0d+0) 
term(477) = term(477) * (-2.0d+0) 
term(478) = term(478) * (16.0d+0) 
term(479) = term(479) * (-16.0d+0) 
term(480) = term(480) * (-8.0d+0) 
term(481) = term(481) * (8.0d+0) 
term(482) = term(482) * (8.0d+0) 
term(483) = term(483) * (-16.0d+0) 
term(484) = term(484) * (8.0d+0) 
term(485) = term(485) * (-4.0d+0) 
term(486) = term(486) * (8.0d+0) 
term(487) = term(487) * (-4.0d+0) 
term(488) = term(488) * (-8.0d+0) 
term(489) = term(489) * (16.0d+0) 
term(490) = term(490) * (4.0d+0) 
term(491) = term(491) * (-8.0d+0) 
term(492) = term(492) * (2.0d+0) 
term(493) = term(493) * (-4.0d+0) 
term(494) = term(494) * (-4.0d+0) 
term(495) = term(495) * (2.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(496) = term(496) + s1(a,i) * wm_interm_24_so_pt4(p,j,k,i) * wm_interm_5_so_pt4(q,a,k,j)
term(497) = term(497) + s2(a,q,j,i) * wm_interm_24_so_pt4(p,k,i,j) * wm_interm_6_so_pt4(a,k)
term(498) = term(498) + s2(a,q,j,i) * wm_interm_26_so_pt4(p,k,i,j) * wm_interm_6_so_pt4(a,k)
term(499) = term(499) + s2(a,q,j,i) * wm_interm_24_so_pt4(p,k,i,j) * wm_interm_7_so_pt4(a,k)
term(500) = term(500) + s2(a,q,j,i) * wm_interm_26_so_pt4(p,k,i,j) * wm_interm_7_so_pt4(a,k)
term(501) = term(501) + s1(a,i) * wm_interm_29_so_pt4(p,j,k,i) * wm_interm_5_so_pt4(q,a,k,j)
term(502) = term(502) + s2(a,q,j,i) * wm_interm_29_so_pt4(p,k,i,j) * wm_interm_6_so_pt4(a,k)
term(503) = term(503) + s2(a,q,j,i) * wm_interm_29_so_pt4(p,k,i,j) * wm_interm_7_so_pt4(a,k)
term(504) = term(504) + s2(a,q,j,i) * wm_interm_13_so_pt4(p,k,i,j) * wm_interm_33_so_pt4(a,k)
term(505) = term(505) + s2(a,q,j,i) * wm_interm_13_so_pt4(p,k,i,j) * wm_interm_34_so_pt4(a,k)
term(506) = term(506) + s2(a,q,j,i) * wm_interm_13_so_pt4(p,k,i,j) * wm_interm_35_so_pt4(a,k)
term(507) = term(507) + s2(a,q,j,i) * wm_interm_13_so_pt4(p,k,i,j) * wm_interm_36_so_pt4(a,k)
term(508) = term(508) + s2(a,q,j,i) * wm_interm_13_so_pt4(p,k,i,j) * wm_interm_37_so_pt4(a,k)
term(509) = term(509) + s2(a,q,j,i) * wm_interm_38_so_pt4(a,k) * wm_interm_3_so_pt4(p,k,i,j)
term(510) = term(510) + s2(a,q,j,i) * wm_interm_3_so_pt4(p,k,i,j) * wm_interm_40_so_pt4(a,k)
term(511) = term(511) + s2(a,q,j,i) * wm_interm_3_so_pt4(p,k,i,j) * wm_interm_41_so_pt4(a,k)
term(512) = term(512) + s1(q,i) * wm_interm_24_so_pt4(a,j,k,i) * wm_interm_5_so_pt4(a,p,k,j)
term(513) = term(513) + s2(a,q,j,i) * wm_interm_3_so_pt4(p,k,i,j) * wm_interm_42_so_pt4(a,k)
term(514) = term(514) + s2(a,q,j,i) * wm_interm_3_so_pt4(p,k,i,j) * wm_interm_43_so_pt4(a,k)
term(515) = term(515) + s1(q,i) * wm_interm_29_so_pt4(a,j,k,i) * wm_interm_5_so_pt4(a,p,k,j)
term(516) = term(516) + t2(a,p,j,i) * wm_interm_12_so_pt4(q,k,i,j) * wm_interm_50_so_pt4(a,k)
term(517) = term(517) + t2(a,p,j,i) * wm_interm_12_so_pt4(q,k,i,j) * wm_interm_51_so_pt4(a,k)
end do 
end do 
end do 
end do 

term(496) = term(496) * (-2.0d+0) 
term(497) = term(497) * (-8.0d+0) 
term(498) = term(498) * (4.0d+0) 
term(499) = term(499) * (4.0d+0) 
term(500) = term(500) * (-2.0d+0) 
term(501) = term(501) * (8.0d+0) 
term(502) = term(502) * (16.0d+0) 
term(503) = term(503) * (-8.0d+0) 
term(504) = term(504) * (2.0d+0) 
term(505) = term(505) * (-1.0d+0) 
term(506) = term(506) * (-1.0d+0) 
term(507) = term(507) * (4.0d+0) 
term(508) = term(508) * (-4.0d+0) 
term(509) = term(509) * (-2.0d+0) 
term(510) = term(510) * (4.0d+0) 
term(511) = term(511) * (-2.0d+0) 
term(512) = term(512) * (-2.0d+0) 
term(513) = term(513) * (-8.0d+0) 
term(514) = term(514) * (8.0d+0) 
term(515) = term(515) * (8.0d+0) 
term(516) = term(516) * (-4.0d+0) 
term(517) = term(517) * (2.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(518) = term(518) + s1(a,i) * s2(b,q,k,j) * t2(b,p,l,i) * wm_interm_24_so_pt4(a,l,k,j)
term(519) = term(519) + s1(a,i) * s2(b,q,k,j) * t2(b,p,l,i) * wm_interm_29_so_pt4(a,l,k,j)
end do 
end do 
end do 
end do 
end do 
end do 

term(518) = term(518) * (-2.0d+0) 
term(519) = term(519) * (8.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(520) = term(520) + s1(a,i) * s2(b,q,k,j) * t2(b,p,l,i) * wm_interm_24_so_pt4(a,l,j,k)
term(521) = term(521) + s1(a,i) * s2(b,q,k,j) * t2(b,p,l,i) * wm_interm_26_so_pt4(a,l,j,k)
term(522) = term(522) + s1(a,i) * s2(b,q,k,j) * t2(b,p,l,i) * wm_interm_29_so_pt4(a,l,j,k)
term(523) = term(523) + s2(a,q,k,j) * t1(b,j) * t2(a,p,l,i) * wm_interm_46_so_pt4(b,i,l,k)
term(524) = term(524) + s2(a,q,k,j) * t1(b,j) * t2(a,p,l,i) * wm_interm_48_so_pt4(b,l,i,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(520) = term(520) * (4.0d+0) 
term(521) = term(521) * (-2.0d+0) 
term(522) = term(522) * (-8.0d+0) 

do l = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(525) = term(525) + r2(vrdav_Rl, a,k,q,j) * t1(b,j) * t2(a,p,l,i) * wm_interm_12_so_pt4(b,k,i,l)
term(526) = term(526) + r2(vrdav_Rl, a,k,q,j) * t1(b,k) * t2(a,p,l,i) * wm_interm_12_so_pt4(b,j,i,l)
end do 
end do 
end do 
end do 
end do 
end do 

term(525) = term(525) * (2.0d+0) 
term(526) = term(526) * (-4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(527) = term(527) + r2(vrdav_Rl, a,k,q,j) * t1(b,k) * t2(a,p,l,i) * wm_interm_12_so_pt4(b,j,l,i)
term(528) = term(528) + s2(a,q,k,j) * t1(b,k) * t2(a,p,l,i) * wm_interm_46_so_pt4(b,l,i,j)
term(529) = term(529) + s2(a,q,k,j) * t1(b,k) * t2(a,p,l,i) * wm_interm_46_so_pt4(b,i,l,j)
term(530) = term(530) + s2(a,q,k,j) * t1(b,k) * t2(a,p,l,i) * wm_interm_48_so_pt4(b,i,l,j)
term(531) = term(531) + s2(a,q,k,j) * t1(b,k) * t2(a,p,l,i) * wm_interm_48_so_pt4(b,l,i,j)
end do 
end do 
end do 
end do 
end do 
end do 

term(527) = term(527) * (2.0d+0) 
term(529) = term(529) * (-2.0d+0) 
term(531) = term(531) * (-2.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(532) = term(532) + s1(a,i) * wm_interm_24_so_pt4(p,j,i,k) * wm_interm_5_so_pt4(q,a,k,j)
term(533) = term(533) + s1(a,i) * wm_interm_1_so_pt4(q,a,j,k) * wm_interm_24_so_pt4(p,k,i,j)
term(534) = term(534) + s1(a,i) * wm_interm_26_so_pt4(p,j,i,k) * wm_interm_5_so_pt4(q,a,k,j)
term(535) = term(535) + r1(vrdav_Rl, q,i) * wm_interm_3_so_pt4(a,j,i,k) * wm_interm_70_so_pt4(p,a,j,k)
term(536) = term(536) + r1(vrdav_Rl, q,i) * wm_interm_3_so_pt4(a,j,i,k) * wm_interm_69_so_pt4(p,a,j,k)
term(537) = term(537) + r1(vrdav_Rl, q,i) * wm_interm_3_so_pt4(a,j,i,k) * wm_interm_67_so_pt4(p,a,j,k)
term(538) = term(538) + r1(vrdav_Rl, q,i) * wm_interm_3_so_pt4(a,j,i,k) * wm_interm_68_so_pt4(p,a,j,k)
term(539) = term(539) + s1(a,i) * wm_interm_10_so_pt4(q,a,j,k) * wm_interm_24_so_pt4(p,k,i,j)
term(540) = term(540) + s1(a,i) * wm_interm_11_so_pt4(q,a,j,k) * wm_interm_24_so_pt4(p,k,i,j)
term(541) = term(541) + r1(vrdav_Rl, q,i) * wm_interm_3_so_pt4(a,j,i,k) * wm_interm_84_so_pt4(p,a,j,k)
term(542) = term(542) + r1(vrdav_Rl, q,i) * wm_interm_3_so_pt4(a,j,i,k) * wm_interm_74_so_pt4(p,a,j,k)
term(543) = term(543) + r1(vrdav_Rl, a,i) * wm_interm_1_so_pt4(q,a,j,k) * wm_interm_27_so_pt4(p,j,i,k)
term(544) = term(544) + r1(vrdav_Rl, a,i) * wm_interm_27_so_pt4(p,j,i,k) * wm_interm_5_so_pt4(q,a,j,k)
term(545) = term(545) + r1(vrdav_Rl, a,i) * wm_interm_1_so_pt4(q,a,j,k) * wm_interm_28_so_pt4(p,j,i,k)
term(546) = term(546) + s1(q,i) * wm_interm_13_so_pt4(a,j,i,k) * wm_interm_70_so_pt4(p,a,j,k)
term(547) = term(547) + s1(q,i) * wm_interm_13_so_pt4(a,j,i,k) * wm_interm_69_so_pt4(p,a,j,k)
term(548) = term(548) + s1(q,i) * wm_interm_13_so_pt4(a,j,i,k) * wm_interm_67_so_pt4(p,a,j,k)
term(549) = term(549) + s1(q,i) * wm_interm_13_so_pt4(a,j,i,k) * wm_interm_68_so_pt4(p,a,j,k)
term(550) = term(550) + r1(vrdav_Rl, a,i) * wm_interm_10_so_pt4(q,a,j,k) * wm_interm_27_so_pt4(p,j,i,k)
term(551) = term(551) + r1(vrdav_Rl, a,i) * wm_interm_11_so_pt4(q,a,j,k) * wm_interm_27_so_pt4(p,j,i,k)
term(552) = term(552) + r1(vrdav_Rl, a,i) * wm_interm_10_so_pt4(q,a,j,k) * wm_interm_28_so_pt4(p,j,i,k)
term(553) = term(553) + r1(vrdav_Rl, a,i) * wm_interm_11_so_pt4(q,a,j,k) * wm_interm_28_so_pt4(p,j,i,k)
term(554) = term(554) + s1(q,i) * wm_interm_13_so_pt4(a,j,i,k) * wm_interm_84_so_pt4(p,a,j,k)
term(555) = term(555) + s1(q,i) * wm_interm_13_so_pt4(a,j,i,k) * wm_interm_74_so_pt4(p,a,j,k)
term(556) = term(556) + s1(a,i) * wm_interm_29_so_pt4(p,j,i,k) * wm_interm_5_so_pt4(q,a,k,j)
term(557) = term(557) + s1(a,i) * wm_interm_1_so_pt4(q,a,j,k) * wm_interm_29_so_pt4(p,k,i,j)
term(558) = term(558) + r1(vrdav_Rl, q,i) * wm_interm_3_so_pt4(a,j,i,k) * wm_interm_98_so_pt4(p,a,j,k)
term(559) = term(559) + r1(vrdav_Rl, q,i) * wm_interm_3_so_pt4(a,j,i,k) * wm_interm_97_so_pt4(p,a,j,k)
term(560) = term(560) + r1(vrdav_Rl, q,i) * wm_interm_3_so_pt4(a,j,i,k) * wm_interm_95_so_pt4(p,a,j,k)
term(561) = term(561) + r1(vrdav_Rl, q,i) * wm_interm_3_so_pt4(a,j,i,k) * wm_interm_96_so_pt4(p,a,j,k)
term(562) = term(562) + s1(a,i) * wm_interm_10_so_pt4(q,a,j,k) * wm_interm_29_so_pt4(p,k,i,j)
term(563) = term(563) + s1(a,i) * wm_interm_11_so_pt4(q,a,j,k) * wm_interm_29_so_pt4(p,k,i,j)
term(564) = term(564) + r1(vrdav_Rl, a,i) * wm_interm_1_so_pt4(q,a,j,k) * wm_interm_30_so_pt4(p,j,i,k)
term(565) = term(565) + r1(vrdav_Rl, a,i) * wm_interm_30_so_pt4(p,j,i,k) * wm_interm_5_so_pt4(q,a,j,k)
term(566) = term(566) + r1(vrdav_Rl, a,i) * wm_interm_1_so_pt4(q,a,j,k) * wm_interm_31_so_pt4(p,j,i,k)
term(567) = term(567) + s1(q,i) * wm_interm_13_so_pt4(a,j,i,k) * wm_interm_98_so_pt4(p,a,j,k)
term(568) = term(568) + s1(q,i) * wm_interm_13_so_pt4(a,j,i,k) * wm_interm_97_so_pt4(p,a,j,k)
term(569) = term(569) + s1(q,i) * wm_interm_13_so_pt4(a,j,i,k) * wm_interm_95_so_pt4(p,a,j,k)
term(570) = term(570) + s1(q,i) * wm_interm_13_so_pt4(a,j,i,k) * wm_interm_96_so_pt4(p,a,j,k)
term(571) = term(571) + r1(vrdav_Rl, a,i) * wm_interm_10_so_pt4(q,a,j,k) * wm_interm_30_so_pt4(p,j,i,k)
term(572) = term(572) + r1(vrdav_Rl, a,i) * wm_interm_11_so_pt4(q,a,j,k) * wm_interm_30_so_pt4(p,j,i,k)
term(573) = term(573) + r1(vrdav_Rl, a,i) * wm_interm_10_so_pt4(q,a,j,k) * wm_interm_31_so_pt4(p,j,i,k)
term(574) = term(574) + r1(vrdav_Rl, a,i) * wm_interm_11_so_pt4(q,a,j,k) * wm_interm_31_so_pt4(p,j,i,k)
term(575) = term(575) + r1(vrdav_Rl, a,i) * wm_interm_3_so_pt4(p,j,i,k) * wm_interm_84_so_pt4(a,q,j,k)
term(576) = term(576) + r1(vrdav_Rl, a,i) * wm_interm_3_so_pt4(p,j,i,k) * wm_interm_73_so_pt4(a,q,j,k)
term(577) = term(577) + r1(vrdav_Rl, a,i) * wm_interm_3_so_pt4(p,j,i,k) * wm_interm_70_so_pt4(a,q,j,k)
term(578) = term(578) + r1(vrdav_Rl, q,i) * wm_interm_1_so_pt4(a,p,j,k) * wm_interm_28_so_pt4(a,j,i,k)
term(579) = term(579) + r1(vrdav_Rl, q,i) * wm_interm_1_so_pt4(a,p,j,k) * wm_interm_28_so_pt4(a,i,j,k)
term(580) = term(580) + r1(vrdav_Rl, q,i) * wm_interm_1_so_pt4(a,p,j,k) * wm_interm_27_so_pt4(a,i,j,k)
term(581) = term(581) + r1(vrdav_Rl, q,i) * wm_interm_28_so_pt4(a,i,j,k) * wm_interm_5_so_pt4(a,p,j,k)
term(582) = term(582) + r1(vrdav_Rl, q,i) * wm_interm_27_so_pt4(a,j,i,k) * wm_interm_5_so_pt4(a,p,j,k)
term(583) = term(583) + r1(vrdav_Rl, q,i) * wm_interm_27_so_pt4(a,i,j,k) * wm_interm_5_so_pt4(a,p,j,k)
term(584) = term(584) + s1(a,i) * wm_interm_13_so_pt4(p,j,i,k) * wm_interm_70_so_pt4(a,q,j,k)
term(585) = term(585) + s1(a,i) * wm_interm_13_so_pt4(p,j,i,k) * wm_interm_84_so_pt4(a,q,j,k)
term(586) = term(586) + s1(a,i) * wm_interm_13_so_pt4(p,j,i,k) * wm_interm_73_so_pt4(a,q,j,k)
term(587) = term(587) + s1(q,i) * wm_interm_10_so_pt4(a,p,j,k) * wm_interm_24_so_pt4(a,k,i,j)
term(588) = term(588) + s1(q,i) * wm_interm_10_so_pt4(a,p,j,k) * wm_interm_26_so_pt4(a,k,i,j)
term(589) = term(589) + s1(q,i) * wm_interm_24_so_pt4(a,j,i,k) * wm_interm_5_so_pt4(a,p,k,j)
term(590) = term(590) + s1(q,i) * wm_interm_26_so_pt4(a,j,i,k) * wm_interm_5_so_pt4(a,p,k,j)
term(591) = term(591) + s1(q,i) * wm_interm_1_so_pt4(a,p,j,k) * wm_interm_24_so_pt4(a,k,i,j)
term(592) = term(592) + s1(q,i) * wm_interm_11_so_pt4(a,p,j,k) * wm_interm_24_so_pt4(a,k,i,j)
term(593) = term(593) + s1(q,i) * wm_interm_1_so_pt4(a,p,j,k) * wm_interm_26_so_pt4(a,k,i,j)
term(594) = term(594) + s1(q,i) * wm_interm_11_so_pt4(a,p,j,k) * wm_interm_26_so_pt4(a,k,i,j)
term(595) = term(595) + r1(vrdav_Rl, q,i) * wm_interm_10_so_pt4(a,p,j,k) * wm_interm_28_so_pt4(a,j,i,k)
term(596) = term(596) + r1(vrdav_Rl, q,i) * wm_interm_10_so_pt4(a,p,j,k) * wm_interm_28_so_pt4(a,i,j,k)
term(597) = term(597) + r1(vrdav_Rl, q,i) * wm_interm_11_so_pt4(a,p,j,k) * wm_interm_28_so_pt4(a,j,i,k)
term(598) = term(598) + r1(vrdav_Rl, q,i) * wm_interm_11_so_pt4(a,p,j,k) * wm_interm_28_so_pt4(a,i,j,k)
term(599) = term(599) + r1(vrdav_Rl, q,i) * wm_interm_10_so_pt4(a,p,j,k) * wm_interm_27_so_pt4(a,i,j,k)
term(600) = term(600) + r1(vrdav_Rl, q,i) * wm_interm_11_so_pt4(a,p,j,k) * wm_interm_27_so_pt4(a,i,j,k)
term(601) = term(601) + r1(vrdav_Rl, a,i) * wm_interm_3_so_pt4(p,j,i,k) * wm_interm_98_so_pt4(a,q,j,k)
term(602) = term(602) + r1(vrdav_Rl, a,i) * wm_interm_3_so_pt4(p,j,i,k) * wm_interm_97_so_pt4(a,q,j,k)
term(603) = term(603) + r1(vrdav_Rl, q,i) * wm_interm_1_so_pt4(a,p,j,k) * wm_interm_31_so_pt4(a,j,i,k)
term(604) = term(604) + r1(vrdav_Rl, q,i) * wm_interm_1_so_pt4(a,p,j,k) * wm_interm_31_so_pt4(a,i,j,k)
term(605) = term(605) + r1(vrdav_Rl, q,i) * wm_interm_1_so_pt4(a,p,j,k) * wm_interm_30_so_pt4(a,i,j,k)
term(606) = term(606) + r1(vrdav_Rl, q,i) * wm_interm_31_so_pt4(a,i,j,k) * wm_interm_5_so_pt4(a,p,j,k)
term(607) = term(607) + r1(vrdav_Rl, q,i) * wm_interm_30_so_pt4(a,j,i,k) * wm_interm_5_so_pt4(a,p,j,k)
term(608) = term(608) + r1(vrdav_Rl, q,i) * wm_interm_30_so_pt4(a,i,j,k) * wm_interm_5_so_pt4(a,p,j,k)
term(609) = term(609) + s1(a,i) * wm_interm_13_so_pt4(p,j,i,k) * wm_interm_98_so_pt4(a,q,j,k)
term(610) = term(610) + s1(a,i) * wm_interm_13_so_pt4(p,j,i,k) * wm_interm_97_so_pt4(a,q,j,k)
term(611) = term(611) + s1(q,i) * wm_interm_10_so_pt4(a,p,j,k) * wm_interm_29_so_pt4(a,k,i,j)
term(612) = term(612) + s1(q,i) * wm_interm_29_so_pt4(a,j,i,k) * wm_interm_5_so_pt4(a,p,k,j)
term(613) = term(613) + s1(q,i) * wm_interm_1_so_pt4(a,p,j,k) * wm_interm_29_so_pt4(a,k,i,j)
term(614) = term(614) + s1(q,i) * wm_interm_11_so_pt4(a,p,j,k) * wm_interm_29_so_pt4(a,k,i,j)
term(615) = term(615) + r1(vrdav_Rl, q,i) * wm_interm_10_so_pt4(a,p,j,k) * wm_interm_31_so_pt4(a,j,i,k)
term(616) = term(616) + r1(vrdav_Rl, q,i) * wm_interm_10_so_pt4(a,p,j,k) * wm_interm_31_so_pt4(a,i,j,k)
term(617) = term(617) + r1(vrdav_Rl, q,i) * wm_interm_11_so_pt4(a,p,j,k) * wm_interm_31_so_pt4(a,j,i,k)
term(618) = term(618) + r1(vrdav_Rl, q,i) * wm_interm_11_so_pt4(a,p,j,k) * wm_interm_31_so_pt4(a,i,j,k)
term(619) = term(619) + r1(vrdav_Rl, q,i) * wm_interm_10_so_pt4(a,p,j,k) * wm_interm_30_so_pt4(a,i,j,k)
term(620) = term(620) + r1(vrdav_Rl, q,i) * wm_interm_11_so_pt4(a,p,j,k) * wm_interm_30_so_pt4(a,i,j,k)
term(621) = term(621) + r1(vrdav_Rr, p,i) * wm_interm_113_so_pt4(a,j,i,k) * wm_interm_55_so_pt4(q,a,k,j)
term(622) = term(622) + r1(vrdav_Rr, p,i) * wm_interm_113_so_pt4(a,j,i,k) * wm_interm_54_so_pt4(q,a,k,j)
term(623) = term(623) + r1(vrdav_Rr, p,i) * wm_interm_22_so_pt4(a,j,i,k) * wm_interm_56_so_pt4(q,a,k,j)
term(624) = term(624) + r1(vrdav_Rr, p,i) * wm_interm_22_so_pt4(a,j,i,k) * wm_interm_59_so_pt4(q,a,k,j)
term(625) = term(625) + r1(vrdav_Rr, p,i) * wm_interm_10_so_pt4(q,a,j,k) * wm_interm_47_so_pt4(a,k,i,j)
term(626) = term(626) + r1(vrdav_Rr, p,i) * wm_interm_11_so_pt4(q,a,j,k) * wm_interm_47_so_pt4(a,k,i,j)
term(627) = term(627) + r1(vrdav_Rr, p,i) * wm_interm_47_so_pt4(a,j,i,k) * wm_interm_5_so_pt4(q,a,k,j)
term(628) = term(628) + r1(vrdav_Rr, p,i) * wm_interm_1_so_pt4(q,a,j,k) * wm_interm_47_so_pt4(a,k,i,j)
term(629) = term(629) + r1(vrdav_Rr, p,i) * wm_interm_113_so_pt4(a,j,i,k) * wm_interm_56_so_pt4(q,a,k,j)
term(630) = term(630) + r1(vrdav_Rr, p,i) * wm_interm_113_so_pt4(a,j,i,k) * wm_interm_59_so_pt4(q,a,k,j)
term(631) = term(631) + r1(vrdav_Rr, p,i) * wm_interm_10_so_pt4(q,a,j,k) * wm_interm_49_so_pt4(a,k,i,j)
term(632) = term(632) + r1(vrdav_Rr, p,i) * wm_interm_11_so_pt4(q,a,j,k) * wm_interm_49_so_pt4(a,k,i,j)
term(633) = term(633) + r1(vrdav_Rr, p,i) * wm_interm_1_so_pt4(q,a,j,k) * wm_interm_49_so_pt4(a,k,i,j)
term(634) = term(634) + r1(vrdav_Rr, p,i) * wm_interm_49_so_pt4(a,j,i,k) * wm_interm_5_so_pt4(q,a,k,j)
term(635) = term(635) + t1(p,i) * wm_interm_12_so_pt4(a,j,i,k) * wm_interm_54_so_pt4(q,a,j,k)
term(636) = term(636) + t1(p,i) * wm_interm_12_so_pt4(a,j,i,k) * wm_interm_55_so_pt4(q,a,j,k)
term(637) = term(637) + t1(p,i) * wm_interm_10_so_pt4(q,a,j,k) * wm_interm_46_so_pt4(a,k,i,j)
term(638) = term(638) + t1(p,i) * wm_interm_11_so_pt4(q,a,j,k) * wm_interm_46_so_pt4(a,k,i,j)
term(639) = term(639) + t1(p,i) * wm_interm_10_so_pt4(q,a,j,k) * wm_interm_48_so_pt4(a,k,i,j)
term(640) = term(640) + t1(p,i) * wm_interm_11_so_pt4(q,a,j,k) * wm_interm_48_so_pt4(a,k,i,j)
term(641) = term(641) + t1(p,i) * wm_interm_12_so_pt4(a,j,i,k) * wm_interm_56_so_pt4(q,a,j,k)
term(642) = term(642) + t1(p,i) * wm_interm_12_so_pt4(a,j,i,k) * wm_interm_59_so_pt4(q,a,j,k)
term(643) = term(643) + t1(p,i) * wm_interm_1_so_pt4(q,a,j,k) * wm_interm_46_so_pt4(a,k,i,j)
term(644) = term(644) + t1(p,i) * wm_interm_48_so_pt4(a,j,i,k) * wm_interm_5_so_pt4(q,a,k,j)
term(645) = term(645) + t1(p,i) * wm_interm_1_so_pt4(q,a,j,k) * wm_interm_48_so_pt4(a,k,i,j)
term(646) = term(646) + r1(vrdav_Rr, a,i) * wm_interm_22_so_pt4(q,i,j,k) * wm_interm_59_so_pt4(a,p,k,j)
term(647) = term(647) + r1(vrdav_Rr, a,i) * wm_interm_22_so_pt4(q,i,j,k) * wm_interm_54_so_pt4(a,p,k,j)
term(648) = term(648) + t1(a,i) * wm_interm_48_so_pt4(q,j,i,k) * wm_interm_5_so_pt4(a,p,k,j)
term(649) = term(649) + t1(a,i) * wm_interm_1_so_pt4(a,p,j,k) * wm_interm_46_so_pt4(q,k,i,j)
term(650) = term(650) + r1(vrdav_Rr, a,i) * wm_interm_22_so_pt4(q,i,j,k) * wm_interm_55_so_pt4(a,p,k,j)
term(651) = term(651) + r1(vrdav_Rr, a,i) * wm_interm_22_so_pt4(q,j,i,k) * wm_interm_56_so_pt4(a,p,k,j)
term(652) = term(652) + r1(vrdav_Rr, a,i) * wm_interm_22_so_pt4(q,i,j,k) * wm_interm_56_so_pt4(a,p,k,j)
term(653) = term(653) + t1(a,i) * wm_interm_10_so_pt4(a,p,j,k) * wm_interm_46_so_pt4(q,k,i,j)
term(654) = term(654) + t1(a,i) * wm_interm_11_so_pt4(a,p,j,k) * wm_interm_46_so_pt4(q,k,i,j)
term(655) = term(655) + t1(a,i) * wm_interm_12_so_pt4(q,j,i,k) * wm_interm_56_so_pt4(a,p,j,k)
term(656) = term(656) + r1(vrdav_Rr, a,i) * wm_interm_47_so_pt4(q,j,i,k) * wm_interm_5_so_pt4(a,p,k,j)
term(657) = term(657) + r1(vrdav_Rr, a,i) * wm_interm_10_so_pt4(a,p,j,k) * wm_interm_47_so_pt4(q,i,k,j)
term(658) = term(658) + r1(vrdav_Rr, a,i) * wm_interm_47_so_pt4(q,i,j,k) * wm_interm_5_so_pt4(a,p,k,j)
term(659) = term(659) + r1(vrdav_Rr, a,i) * wm_interm_1_so_pt4(a,p,j,k) * wm_interm_47_so_pt4(q,i,k,j)
term(660) = term(660) + r1(vrdav_Rr, a,i) * wm_interm_11_so_pt4(a,p,j,k) * wm_interm_47_so_pt4(q,i,k,j)
term(661) = term(661) + t1(a,i) * wm_interm_10_so_pt4(a,p,j,k) * wm_interm_48_so_pt4(q,i,k,j)
term(662) = term(662) + t1(a,i) * wm_interm_1_so_pt4(a,p,j,k) * wm_interm_48_so_pt4(q,i,k,j)
term(663) = term(663) + t1(a,i) * wm_interm_11_so_pt4(a,p,j,k) * wm_interm_48_so_pt4(q,i,k,j)
term(664) = term(664) + t1(a,i) * wm_interm_46_so_pt4(q,i,j,k) * wm_interm_5_so_pt4(a,p,k,j)
term(665) = term(665) + r1(vrdav_Rr, a,i) * wm_interm_49_so_pt4(q,i,j,k) * wm_interm_5_so_pt4(a,p,k,j)
term(666) = term(666) + r1(vrdav_Rr, a,i) * wm_interm_10_so_pt4(a,p,j,k) * wm_interm_49_so_pt4(q,k,i,j)
term(667) = term(667) + r1(vrdav_Rr, a,i) * wm_interm_49_so_pt4(q,j,i,k) * wm_interm_5_so_pt4(a,p,k,j)
term(668) = term(668) + r1(vrdav_Rr, a,i) * wm_interm_1_so_pt4(a,p,j,k) * wm_interm_49_so_pt4(q,k,i,j)
term(669) = term(669) + r1(vrdav_Rr, a,i) * wm_interm_11_so_pt4(a,p,j,k) * wm_interm_49_so_pt4(q,k,i,j)
end do 
end do 
end do 
end do 

term(532) = term(532) * (4.0d+0) 
term(533) = term(533) * (-2.0d+0) 
term(534) = term(534) * (-2.0d+0) 
term(535) = term(535) * (-2.0d+0) 
term(536) = term(536) * (4.0d+0) 
term(537) = term(537) * (4.0d+0) 
term(538) = term(538) * (-8.0d+0) 
term(539) = term(539) * (-2.0d+0) 
term(540) = term(540) * (4.0d+0) 
term(541) = term(541) * (-2.0d+0) 
term(542) = term(542) * (4.0d+0) 
term(543) = term(543) * (4.0d+0) 
term(544) = term(544) * (-2.0d+0) 
term(545) = term(545) * (-2.0d+0) 
term(546) = term(546) * (-2.0d+0) 
term(547) = term(547) * (4.0d+0) 
term(548) = term(548) * (4.0d+0) 
term(549) = term(549) * (-8.0d+0) 
term(550) = term(550) * (4.0d+0) 
term(551) = term(551) * (-8.0d+0) 
term(552) = term(552) * (-2.0d+0) 
term(553) = term(553) * (4.0d+0) 
term(554) = term(554) * (-2.0d+0) 
term(555) = term(555) * (4.0d+0) 
term(556) = term(556) * (-8.0d+0) 
term(557) = term(557) * (8.0d+0) 
term(558) = term(558) * (-8.0d+0) 
term(559) = term(559) * (8.0d+0) 
term(560) = term(560) * (16.0d+0) 
term(561) = term(561) * (-16.0d+0) 
term(562) = term(562) * (8.0d+0) 
term(563) = term(563) * (-16.0d+0) 
term(564) = term(564) * (8.0d+0) 
term(565) = term(565) * (-4.0d+0) 
term(566) = term(566) * (-4.0d+0) 
term(567) = term(567) * (-8.0d+0) 
term(568) = term(568) * (8.0d+0) 
term(569) = term(569) * (16.0d+0) 
term(570) = term(570) * (-16.0d+0) 
term(571) = term(571) * (8.0d+0) 
term(572) = term(572) * (-16.0d+0) 
term(573) = term(573) * (-4.0d+0) 
term(574) = term(574) * (8.0d+0) 
term(575) = term(575) * (-2.0d+0) 
term(576) = term(576) * (4.0d+0) 
term(577) = term(577) * (-2.0d+0) 
term(578) = term(578) * (-2.0d+0) 
term(579) = term(579) * (4.0d+0) 
term(580) = term(580) * (-2.0d+0) 
term(581) = term(581) * (-2.0d+0) 
term(582) = term(582) * (-2.0d+0) 
term(583) = term(583) * (4.0d+0) 
term(584) = term(584) * (-2.0d+0) 
term(585) = term(585) * (-2.0d+0) 
term(586) = term(586) * (4.0d+0) 
term(587) = term(587) * (-2.0d+0) 
term(588) = term(588) * (4.0d+0) 
term(589) = term(589) * (4.0d+0) 
term(590) = term(590) * (-2.0d+0) 
term(591) = term(591) * (-2.0d+0) 
term(592) = term(592) * (4.0d+0) 
term(593) = term(593) * (4.0d+0) 
term(594) = term(594) * (-8.0d+0) 
term(595) = term(595) * (-2.0d+0) 
term(596) = term(596) * (4.0d+0) 
term(597) = term(597) * (4.0d+0) 
term(598) = term(598) * (-8.0d+0) 
term(599) = term(599) * (-2.0d+0) 
term(600) = term(600) * (4.0d+0) 
term(601) = term(601) * (-8.0d+0) 
term(602) = term(602) * (8.0d+0) 
term(603) = term(603) * (-4.0d+0) 
term(604) = term(604) * (8.0d+0) 
term(605) = term(605) * (-4.0d+0) 
term(606) = term(606) * (-4.0d+0) 
term(607) = term(607) * (-4.0d+0) 
term(608) = term(608) * (8.0d+0) 
term(609) = term(609) * (-8.0d+0) 
term(610) = term(610) * (8.0d+0) 
term(611) = term(611) * (8.0d+0) 
term(612) = term(612) * (-8.0d+0) 
term(613) = term(613) * (8.0d+0) 
term(614) = term(614) * (-16.0d+0) 
term(615) = term(615) * (-4.0d+0) 
term(616) = term(616) * (8.0d+0) 
term(617) = term(617) * (8.0d+0) 
term(618) = term(618) * (-16.0d+0) 
term(619) = term(619) * (-4.0d+0) 
term(620) = term(620) * (8.0d+0) 
term(621) = term(621) * (4.0d+0) 
term(622) = term(622) * (-8.0d+0) 
term(623) = term(623) * (2.0d+0) 
term(624) = term(624) * (-4.0d+0) 
term(625) = term(625) * (-4.0d+0) 
term(626) = term(626) * (8.0d+0) 
term(627) = term(627) * (2.0d+0) 
term(628) = term(628) * (-4.0d+0) 
term(629) = term(629) * (-4.0d+0) 
term(630) = term(630) * (2.0d+0) 
term(631) = term(631) * (2.0d+0) 
term(632) = term(632) * (-4.0d+0) 
term(633) = term(633) * (2.0d+0) 
term(634) = term(634) * (-4.0d+0) 
term(635) = term(635) * (8.0d+0) 
term(636) = term(636) * (-4.0d+0) 
term(638) = term(638) * (-2.0d+0) 
term(639) = term(639) * (-2.0d+0) 
term(640) = term(640) * (4.0d+0) 
term(641) = term(641) * (2.0d+0) 
term(642) = term(642) * (-4.0d+0) 
term(645) = term(645) * (-2.0d+0) 
term(646) = term(646) * (2.0d+0) 
term(647) = term(647) * (-4.0d+0) 
term(650) = term(650) * (2.0d+0) 
term(651) = term(651) * (2.0d+0) 
term(652) = term(652) * (-4.0d+0) 
term(654) = term(654) * (-2.0d+0) 
term(655) = term(655) * (2.0d+0) 
term(658) = term(658) * (-2.0d+0) 
term(660) = term(660) * (-2.0d+0) 
term(663) = term(663) * (-2.0d+0) 
term(667) = term(667) * (-2.0d+0) 
term(669) = term(669) * (-2.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(670) = term(670) + r1(vrdav_Rl, q,i) * wm_interm_27_so_pt4(p,j,k,l) * wm_interm_9_so_pt4(k,j,i,l)
term(671) = term(671) + r1(vrdav_Rl, q,i) * wm_interm_27_so_pt4(p,j,k,l) * wm_interm_9_so_pt4(j,k,i,l)
term(672) = term(672) + r1(vrdav_Rl, q,i) * wm_interm_28_so_pt4(p,j,k,l) * wm_interm_9_so_pt4(j,k,i,l)
term(673) = term(673) + r1(vrdav_Rl, q,i) * wm_interm_30_so_pt4(p,j,k,l) * wm_interm_9_so_pt4(k,j,i,l)
term(674) = term(674) + r1(vrdav_Rl, q,i) * wm_interm_30_so_pt4(p,j,k,l) * wm_interm_9_so_pt4(j,k,i,l)
end do 
end do 
end do 
end do 

term(670) = term(670) * (-2.0d+0) 
term(671) = term(671) * (4.0d+0) 
term(672) = term(672) * (-2.0d+0) 
term(673) = term(673) * (-8.0d+0) 
term(674) = term(674) * (8.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
term(675) = term(675) + s1(q,i) * wm_interm_17_so_pt4(j,k) * wm_interm_24_so_pt4(p,k,j,i)
term(676) = term(676) + s1(q,i) * wm_interm_18_so_pt4(j,k) * wm_interm_24_so_pt4(p,k,j,i)
term(677) = term(677) + s1(q,i) * wm_interm_17_so_pt4(j,k) * wm_interm_29_so_pt4(p,k,j,i)
term(678) = term(678) + s1(q,i) * wm_interm_18_so_pt4(j,k) * wm_interm_29_so_pt4(p,k,j,i)
end do 
end do 
end do 

term(676) = term(676) * (-2.0d+0) 
term(677) = term(677) * (-4.0d+0) 
term(678) = term(678) * (8.0d+0) 

do l = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
term(679) = term(679) + s1(q,i) * wm_interm_24_so_pt4(p,j,k,l) * wm_interm_9_so_pt4(k,l,j,i)
term(680) = term(680) + s1(q,i) * wm_interm_24_so_pt4(p,j,k,l) * wm_interm_9_so_pt4(l,k,j,i)
term(681) = term(681) + s1(q,i) * wm_interm_26_so_pt4(p,j,k,l) * wm_interm_9_so_pt4(l,k,j,i)
term(682) = term(682) + s1(q,i) * wm_interm_29_so_pt4(p,j,k,l) * wm_interm_9_so_pt4(l,k,j,i)
term(683) = term(683) + s1(q,i) * wm_interm_29_so_pt4(p,j,k,l) * wm_interm_9_so_pt4(k,l,j,i)
end do 
end do 
end do 
end do 

term(679) = term(679) * (-2.0d+0) 
term(680) = term(680) * (4.0d+0) 
term(681) = term(681) * (-2.0d+0) 
term(682) = term(682) * (-8.0d+0) 
term(683) = term(683) * (8.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(684) = term(684) + r2m(vrdav_Rr, a,j,p,i) * wm_interm_19_so_pt4(a,b) * wm_interm_32_so_pt4(b,q,j,i)
term(685) = term(685) + r2m(vrdav_Rr, a,j,p,i) * wm_interm_20_so_pt4(a,b) * wm_interm_32_so_pt4(b,q,j,i)
term(686) = term(686) + t2(a,p,j,i) * wm_interm_32_so_pt4(q,b,j,i) * wm_interm_81_so_pt4(b,a)
term(687) = term(687) + t2(a,p,j,i) * wm_interm_32_so_pt4(q,b,j,i) * wm_interm_82_so_pt4(b,a)
term(688) = term(688) + t2(a,p,j,i) * wm_interm_32_so_pt4(q,b,j,i) * wm_interm_83_so_pt4(b,a)
term(689) = term(689) + t2(a,p,j,i) * wm_interm_104_so_pt4(b,a) * wm_interm_32_so_pt4(q,b,j,i)
term(690) = term(690) + t2(a,p,j,i) * wm_interm_105_so_pt4(b,a) * wm_interm_32_so_pt4(q,b,j,i)
term(691) = term(691) + t2(a,p,j,i) * wm_interm_112_so_pt4(b,a) * wm_interm_32_so_pt4(q,b,j,i)
term(692) = term(692) + r2(vrdav_Rl, a,j,q,i) * wm_interm_19_so_pt4(b,a) * wm_interm_52_so_pt4(p,b,j,i)
term(693) = term(693) + r2(vrdav_Rl, a,j,q,i) * wm_interm_20_so_pt4(b,a) * wm_interm_52_so_pt4(p,b,j,i)
term(694) = term(694) + s2(a,q,j,i) * wm_interm_52_so_pt4(p,b,j,i) * wm_interm_57_so_pt4(b,a)
term(695) = term(695) + s2(a,q,j,i) * wm_interm_52_so_pt4(p,b,j,i) * wm_interm_58_so_pt4(b,a)
end do 
end do 
end do 
end do 

term(684) = term(684) * (-4.0d+0) 
term(685) = term(685) * (8.0d+0) 
term(686) = term(686) * (-2.0d+0) 
term(687) = term(687) * (4.0d+0) 
term(688) = term(688) * (-2.0d+0) 
term(689) = term(689) * (-4.0d+0) 
term(690) = term(690) * (8.0d+0) 
term(691) = term(691) * (-4.0d+0) 
term(692) = term(692) * (4.0d+0) 
term(693) = term(693) * (-8.0d+0) 
term(694) = term(694) * (-8.0d+0) 
term(695) = term(695) * (4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(696) = term(696) + r1(vrdav_Rl, a,i) * wm_interm_1_so_pt4(q,a,j,k) * wm_interm_27_so_pt4(p,i,j,k)
term(697) = term(697) + r1(vrdav_Rl, a,i) * wm_interm_27_so_pt4(p,i,j,k) * wm_interm_5_so_pt4(q,a,j,k)
term(698) = term(698) + r1(vrdav_Rl, a,i) * wm_interm_28_so_pt4(p,i,j,k) * wm_interm_5_so_pt4(q,a,j,k)
term(699) = term(699) + r1(vrdav_Rl, a,i) * wm_interm_10_so_pt4(q,a,j,k) * wm_interm_27_so_pt4(p,i,j,k)
term(700) = term(700) + r1(vrdav_Rl, a,i) * wm_interm_11_so_pt4(q,a,j,k) * wm_interm_27_so_pt4(p,i,j,k)
term(701) = term(701) + r1(vrdav_Rl, a,i) * wm_interm_1_so_pt4(q,a,j,k) * wm_interm_30_so_pt4(p,i,j,k)
term(702) = term(702) + r1(vrdav_Rl, a,i) * wm_interm_30_so_pt4(p,i,j,k) * wm_interm_5_so_pt4(q,a,j,k)
term(703) = term(703) + r1(vrdav_Rl, a,i) * wm_interm_31_so_pt4(p,i,j,k) * wm_interm_5_so_pt4(q,a,j,k)
term(704) = term(704) + r1(vrdav_Rl, a,i) * wm_interm_10_so_pt4(q,a,j,k) * wm_interm_30_so_pt4(p,i,j,k)
term(705) = term(705) + r1(vrdav_Rl, a,i) * wm_interm_11_so_pt4(q,a,j,k) * wm_interm_30_so_pt4(p,i,j,k)
term(706) = term(706) + r1(vrdav_Rr, p,i) * wm_interm_113_so_pt4(a,i,j,k) * wm_interm_55_so_pt4(q,a,k,j)
term(707) = term(707) + r1(vrdav_Rr, p,i) * wm_interm_113_so_pt4(a,i,j,k) * wm_interm_54_so_pt4(q,a,k,j)
term(708) = term(708) + r1(vrdav_Rr, p,i) * wm_interm_22_so_pt4(a,i,j,k) * wm_interm_56_so_pt4(q,a,k,j)
term(709) = term(709) + r1(vrdav_Rr, p,i) * wm_interm_22_so_pt4(a,i,j,k) * wm_interm_59_so_pt4(q,a,k,j)
term(710) = term(710) + r1(vrdav_Rr, p,i) * wm_interm_10_so_pt4(q,a,j,k) * wm_interm_47_so_pt4(a,i,k,j)
term(711) = term(711) + r1(vrdav_Rr, p,i) * wm_interm_11_so_pt4(q,a,j,k) * wm_interm_47_so_pt4(a,i,k,j)
term(712) = term(712) + r1(vrdav_Rr, p,i) * wm_interm_1_so_pt4(q,a,j,k) * wm_interm_47_so_pt4(a,i,k,j)
term(713) = term(713) + r1(vrdav_Rr, p,i) * wm_interm_47_so_pt4(a,i,j,k) * wm_interm_5_so_pt4(q,a,k,j)
term(714) = term(714) + r1(vrdav_Rr, p,i) * wm_interm_113_so_pt4(a,i,j,k) * wm_interm_56_so_pt4(q,a,k,j)
term(715) = term(715) + r1(vrdav_Rr, p,i) * wm_interm_113_so_pt4(a,i,j,k) * wm_interm_59_so_pt4(q,a,k,j)
term(716) = term(716) + r1(vrdav_Rr, p,i) * wm_interm_10_so_pt4(q,a,j,k) * wm_interm_49_so_pt4(a,i,k,j)
term(717) = term(717) + r1(vrdav_Rr, p,i) * wm_interm_11_so_pt4(q,a,j,k) * wm_interm_49_so_pt4(a,i,k,j)
term(718) = term(718) + r1(vrdav_Rr, p,i) * wm_interm_49_so_pt4(a,i,j,k) * wm_interm_5_so_pt4(q,a,k,j)
term(719) = term(719) + r1(vrdav_Rr, p,i) * wm_interm_1_so_pt4(q,a,j,k) * wm_interm_49_so_pt4(a,i,k,j)
term(720) = term(720) + t1(p,i) * wm_interm_10_so_pt4(q,a,j,k) * wm_interm_46_so_pt4(a,i,k,j)
term(721) = term(721) + t1(p,i) * wm_interm_11_so_pt4(q,a,j,k) * wm_interm_46_so_pt4(a,i,k,j)
term(722) = term(722) + t1(p,i) * wm_interm_10_so_pt4(q,a,j,k) * wm_interm_48_so_pt4(a,i,k,j)
term(723) = term(723) + t1(p,i) * wm_interm_11_so_pt4(q,a,j,k) * wm_interm_48_so_pt4(a,i,k,j)
term(724) = term(724) + t1(p,i) * wm_interm_1_so_pt4(q,a,j,k) * wm_interm_48_so_pt4(a,i,k,j)
term(725) = term(725) + t1(p,i) * wm_interm_46_so_pt4(a,i,j,k) * wm_interm_5_so_pt4(q,a,k,j)
term(726) = term(726) + t1(p,i) * wm_interm_1_so_pt4(q,a,j,k) * wm_interm_46_so_pt4(a,i,k,j)
end do 
end do 
end do 
end do 

term(696) = term(696) * (-2.0d+0) 
term(697) = term(697) * (4.0d+0) 
term(698) = term(698) * (-2.0d+0) 
term(699) = term(699) * (-2.0d+0) 
term(700) = term(700) * (4.0d+0) 
term(701) = term(701) * (-4.0d+0) 
term(702) = term(702) * (8.0d+0) 
term(703) = term(703) * (-4.0d+0) 
term(704) = term(704) * (-4.0d+0) 
term(705) = term(705) * (8.0d+0) 
term(706) = term(706) * (-8.0d+0) 
term(707) = term(707) * (16.0d+0) 
term(708) = term(708) * (-4.0d+0) 
term(709) = term(709) * (2.0d+0) 
term(710) = term(710) * (2.0d+0) 
term(711) = term(711) * (-4.0d+0) 
term(712) = term(712) * (2.0d+0) 
term(713) = term(713) * (-4.0d+0) 
term(714) = term(714) * (2.0d+0) 
term(715) = term(715) * (-4.0d+0) 
term(716) = term(716) * (-4.0d+0) 
term(717) = term(717) * (8.0d+0) 
term(718) = term(718) * (2.0d+0) 
term(719) = term(719) * (-4.0d+0) 
term(720) = term(720) * (-2.0d+0) 
term(721) = term(721) * (4.0d+0) 
term(723) = term(723) * (-2.0d+0) 
term(726) = term(726) * (-2.0d+0) 

do i = 1, nocc 
term(727) = term(727) + wm_interm_33_so_pt4(p,i) * wm_interm_50_so_pt4(q,i)
term(728) = term(728) + wm_interm_34_so_pt4(p,i) * wm_interm_50_so_pt4(q,i)
term(729) = term(729) + wm_interm_35_so_pt4(p,i) * wm_interm_50_so_pt4(q,i)
term(730) = term(730) + wm_interm_33_so_pt4(p,i) * wm_interm_51_so_pt4(q,i)
term(731) = term(731) + wm_interm_34_so_pt4(p,i) * wm_interm_51_so_pt4(q,i)
term(732) = term(732) + wm_interm_35_so_pt4(p,i) * wm_interm_51_so_pt4(q,i)
term(733) = term(733) + wm_interm_36_so_pt4(p,i) * wm_interm_50_so_pt4(q,i)
term(734) = term(734) + wm_interm_37_so_pt4(p,i) * wm_interm_50_so_pt4(q,i)
term(735) = term(735) + wm_interm_36_so_pt4(p,i) * wm_interm_51_so_pt4(q,i)
term(736) = term(736) + wm_interm_37_so_pt4(p,i) * wm_interm_51_so_pt4(q,i)
end do 

term(727) = term(727) * (-4.0d+0) 
term(728) = term(728) * (2.0d+0) 
term(729) = term(729) * (2.0d+0) 
term(730) = term(730) * (2.0d+0) 
term(731) = term(731) * (-1.0d+0) 
term(732) = term(732) * (-1.0d+0) 
term(733) = term(733) * (-8.0d+0) 
term(734) = term(734) * (8.0d+0) 
term(735) = term(735) * (4.0d+0) 
term(736) = term(736) * (-4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
term(737) = term(737) + s1(q,i) * wm_interm_17_so_pt4(j,k) * wm_interm_24_so_pt4(p,k,i,j)
term(738) = term(738) + s1(q,i) * wm_interm_18_so_pt4(j,k) * wm_interm_24_so_pt4(p,k,i,j)
term(739) = term(739) + s1(q,i) * wm_interm_17_so_pt4(j,k) * wm_interm_26_so_pt4(p,k,i,j)
term(740) = term(740) + s1(q,i) * wm_interm_18_so_pt4(j,k) * wm_interm_26_so_pt4(p,k,i,j)
term(741) = term(741) + s1(q,i) * wm_interm_17_so_pt4(j,k) * wm_interm_29_so_pt4(p,k,i,j)
term(742) = term(742) + s1(q,i) * wm_interm_18_so_pt4(j,k) * wm_interm_29_so_pt4(p,k,i,j)
term(743) = term(743) + r1(vrdav_Rr, p,i) * wm_interm_17_so_pt4(j,k) * wm_interm_47_so_pt4(q,k,i,j)
term(744) = term(744) + r1(vrdav_Rr, p,i) * wm_interm_18_so_pt4(j,k) * wm_interm_47_so_pt4(q,k,i,j)
term(745) = term(745) + r1(vrdav_Rr, p,i) * wm_interm_17_so_pt4(j,k) * wm_interm_49_so_pt4(q,k,i,j)
term(746) = term(746) + r1(vrdav_Rr, p,i) * wm_interm_18_so_pt4(j,k) * wm_interm_49_so_pt4(q,k,i,j)
term(747) = term(747) + t1(p,i) * wm_interm_17_so_pt4(j,k) * wm_interm_48_so_pt4(q,k,i,j)
term(748) = term(748) + t1(p,i) * wm_interm_18_so_pt4(j,k) * wm_interm_48_so_pt4(q,k,i,j)
end do 
end do 
end do 

term(737) = term(737) * (-2.0d+0) 
term(738) = term(738) * (4.0d+0) 
term(740) = term(740) * (-2.0d+0) 
term(741) = term(741) * (4.0d+0) 
term(742) = term(742) * (-8.0d+0) 
term(743) = term(743) * (2.0d+0) 
term(744) = term(744) * (-4.0d+0) 
term(745) = term(745) * (-4.0d+0) 
term(746) = term(746) * (8.0d+0) 
term(748) = term(748) * (-2.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(749) = term(749) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_19_so_pt4(a,b) * wm_interm_32_so_pt4(b,q,j,i)
term(750) = term(750) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_20_so_pt4(a,b) * wm_interm_32_so_pt4(b,q,j,i)
term(751) = term(751) + r2m(vrdav_Rr, a,j,p,i) * wm_interm_19_so_pt4(a,b) * wm_interm_32_so_pt4(b,q,i,j)
term(752) = term(752) + r2m(vrdav_Rr, a,j,p,i) * wm_interm_20_so_pt4(a,b) * wm_interm_32_so_pt4(b,q,i,j)
term(753) = term(753) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_19_so_pt4(a,b) * wm_interm_32_so_pt4(b,q,j,i)
term(754) = term(754) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_20_so_pt4(a,b) * wm_interm_32_so_pt4(b,q,j,i)
term(755) = term(755) + r2(vrdav_Rl, a,j,q,i) * wm_interm_19_so_pt4(b,a) * wm_interm_52_so_pt4(p,b,i,j)
term(756) = term(756) + r2(vrdav_Rl, a,j,q,i) * wm_interm_20_so_pt4(b,a) * wm_interm_52_so_pt4(p,b,i,j)
term(757) = term(757) + s2(a,q,j,i) * wm_interm_52_so_pt4(p,b,i,j) * wm_interm_57_so_pt4(b,a)
term(758) = term(758) + s2(a,q,j,i) * wm_interm_52_so_pt4(p,b,i,j) * wm_interm_58_so_pt4(b,a)
end do 
end do 
end do 
end do 

term(750) = term(750) * (-2.0d+0) 
term(751) = term(751) * (2.0d+0) 
term(752) = term(752) * (-4.0d+0) 
term(753) = term(753) * (2.0d+0) 
term(754) = term(754) * (-4.0d+0) 
term(755) = term(755) * (-8.0d+0) 
term(756) = term(756) * (16.0d+0) 
term(757) = term(757) * (16.0d+0) 
term(758) = term(758) * (-8.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(759) = term(759) + r1(vrdav_Rl, q,i) * wm_interm_3_so_pt4(p,j,i,k) * wm_interm_85_so_pt4(j,k)
term(760) = term(760) + r1(vrdav_Rl, q,i) * wm_interm_3_so_pt4(p,j,i,k) * wm_interm_86_so_pt4(j,k)
term(761) = term(761) + r1(vrdav_Rl, q,i) * wm_interm_3_so_pt4(p,j,i,k) * wm_interm_87_so_pt4(j,k)
term(762) = term(762) + s1(q,i) * wm_interm_13_so_pt4(p,j,i,k) * wm_interm_85_so_pt4(j,k)
term(763) = term(763) + s1(q,i) * wm_interm_13_so_pt4(p,j,i,k) * wm_interm_86_so_pt4(j,k)
term(764) = term(764) + s1(q,i) * wm_interm_13_so_pt4(p,j,i,k) * wm_interm_87_so_pt4(j,k)
term(765) = term(765) + r1(vrdav_Rl, q,i) * wm_interm_106_so_pt4(j,k) * wm_interm_3_so_pt4(p,j,i,k)
term(766) = term(766) + r1(vrdav_Rl, q,i) * wm_interm_107_so_pt4(j,k) * wm_interm_3_so_pt4(p,j,i,k)
term(767) = term(767) + r1(vrdav_Rl, q,i) * wm_interm_108_so_pt4(j,k) * wm_interm_3_so_pt4(p,j,i,k)
term(768) = term(768) + s1(q,i) * wm_interm_106_so_pt4(j,k) * wm_interm_13_so_pt4(p,j,i,k)
term(769) = term(769) + s1(q,i) * wm_interm_107_so_pt4(j,k) * wm_interm_13_so_pt4(p,j,i,k)
term(770) = term(770) + s1(q,i) * wm_interm_108_so_pt4(j,k) * wm_interm_13_so_pt4(p,j,i,k)
term(771) = term(771) + r1(vrdav_Rr, p,i) * wm_interm_22_so_pt4(q,j,i,k) * wm_interm_62_so_pt4(k,j)
term(772) = term(772) + r1(vrdav_Rr, p,i) * wm_interm_22_so_pt4(q,j,i,k) * wm_interm_60_so_pt4(k,j)
term(773) = term(773) + t1(p,i) * wm_interm_12_so_pt4(q,j,i,k) * wm_interm_60_so_pt4(j,k)
term(774) = term(774) + t1(p,i) * wm_interm_12_so_pt4(q,j,i,k) * wm_interm_62_so_pt4(j,k)
end do 
end do 
end do 

term(759) = term(759) * (-2.0d+0) 
term(760) = term(760) * (4.0d+0) 
term(761) = term(761) * (-2.0d+0) 
term(762) = term(762) * (-2.0d+0) 
term(763) = term(763) * (4.0d+0) 
term(764) = term(764) * (-2.0d+0) 
term(765) = term(765) * (-4.0d+0) 
term(766) = term(766) * (8.0d+0) 
term(767) = term(767) * (-4.0d+0) 
term(768) = term(768) * (-4.0d+0) 
term(769) = term(769) * (8.0d+0) 
term(770) = term(770) * (-4.0d+0) 
term(771) = term(771) * (-8.0d+0) 
term(772) = term(772) * (4.0d+0) 
term(773) = term(773) * (2.0d+0) 
term(774) = term(774) * (-4.0d+0) 


    calc_D_vv_wm_so_pt4 = zero
    do s = 0, 774
    calc_D_vv_wm_so_pt4 = calc_D_vv_wm_so_pt4 + term(s)
    end do

    end function calc_D_vv_wm_so_pt4
    
    
  end module so_ccsd_pt4
