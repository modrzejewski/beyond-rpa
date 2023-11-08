module so_ccsd_pt3
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
        real(F64), dimension(:, :, :, :), allocatable :: wm_interm_0_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_1_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_2_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_3_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_4_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_5_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_6_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_7_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_8_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_9_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_10_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_11_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_12_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_13_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_14_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_15_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_16_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_17_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_18_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_19_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_20_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_21_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_22_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_23_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_24_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_25_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_26_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_27_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_28_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_29_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_30_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_31_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_32_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_33_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_34_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_35_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_36_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_37_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_38_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_39_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_40_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_41_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_42_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_43_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_44_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_45_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_46_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_47_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_48_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_49_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_50_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_51_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_52_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_53_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_54_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_55_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_56_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_57_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_58_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_59_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_60_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_61_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_62_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_63_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_64_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_65_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_66_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_67_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_68_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_69_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_70_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_71_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_72_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_73_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_74_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_75_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_76_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_77_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_78_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_79_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_80_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_81_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_82_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_83_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_84_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_85_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_86_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_87_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_88_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_89_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_90_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_91_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_92_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_93_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_94_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_95_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_96_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_97_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_98_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_99_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_100_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_101_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_102_so_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_103_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_104_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_105_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_106_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_107_so_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_108_so_pt3 

    contains
    
    subroutine wm_so_intermediates_ccsd_init_pt3(nocc, nactive)
    integer, intent(in) :: nocc
    integer, intent(in) :: nactive
    allocate(wm_interm_0_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_1_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_2_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_3_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_4_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_5_so_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_6_so_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_7_so_pt3(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_8_so_pt3(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_9_so_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_10_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_11_so_pt3(1: nocc, 1: nocc))
allocate(wm_interm_12_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_13_so_pt3(1: nocc, 1: nocc))
allocate(wm_interm_14_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_15_so_pt3(1: nocc, 1: nocc))
allocate(wm_interm_16_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_17_so_pt3(1: nocc, 1: nocc))
allocate(wm_interm_18_so_pt3(1: nocc, 1: nocc))
allocate(wm_interm_19_so_pt3(1: nocc, 1: nocc))
allocate(wm_interm_20_so_pt3(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_21_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_22_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_23_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_24_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_25_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_26_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_27_so_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_28_so_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_29_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_30_so_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_31_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_32_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_33_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_34_so_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_35_so_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_36_so_pt3(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_37_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_38_so_pt3(1: nocc, 1: nocc))
allocate(wm_interm_39_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_40_so_pt3(1: nocc, 1: nocc))
allocate(wm_interm_41_so_pt3(1: nocc, 1: nocc))
allocate(wm_interm_42_so_pt3(1: nocc, 1: nocc))
allocate(wm_interm_43_so_pt3(1: nocc, 1: nocc))
allocate(wm_interm_44_so_pt3(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_45_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_46_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_47_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_48_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_49_so_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_50_so_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_51_so_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_52_so_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_53_so_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_54_so_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_55_so_pt3(1: nocc, 1: nocc))
allocate(wm_interm_56_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_57_so_pt3(1: nocc, 1: nocc))
allocate(wm_interm_58_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_59_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_60_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_61_so_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_62_so_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_63_so_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_64_so_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_65_so_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_66_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_67_so_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_68_so_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_69_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_70_so_pt3(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_71_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_72_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_73_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_74_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_75_so_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_76_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_77_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_78_so_pt3(1: nocc, 1: nocc))
allocate(wm_interm_79_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_80_so_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_81_so_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_82_so_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_83_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_84_so_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_85_so_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_86_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_87_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_88_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_89_so_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_90_so_pt3(1: nocc, 1: nocc))
allocate(wm_interm_91_so_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_92_so_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_93_so_pt3(1: nocc, 1: nocc))
allocate(wm_interm_94_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_95_so_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_96_so_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_97_so_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_98_so_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_99_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_100_so_pt3(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_101_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_102_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_103_so_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_104_so_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_105_so_pt3(1: nocc, 1: nocc))
allocate(wm_interm_106_so_pt3(1: nocc, 1: nocc))
allocate(wm_interm_107_so_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_108_so_pt3(nocc+1: nactive, nocc+1: nactive))
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

    end subroutine wm_so_intermediates_ccsd_init_pt3
    
    subroutine wm_so_intermediates_ccsd_free_pt3
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

    end subroutine wm_so_intermediates_ccsd_free_pt3
    
    subroutine wm_so_intermediates_ccsd_pt3(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr)
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
sum = sum + r2(vrdav_Rl, a,j,b,i) * r2p(vrdav_Rr, a,k,c,i)
end do 
end do 
wm_interm_0_so_pt3(b, c, j, k) = wm_interm_0_so_pt3(b, c, j, k) + sum 
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
wm_interm_1_so_pt3(b, j, i, k) = wm_interm_1_so_pt3(b, j, i, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_2_so_pt3(b, c, j, k) = wm_interm_2_so_pt3(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_3_so_pt3(b, c, j, k) = wm_interm_3_so_pt3(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_4_so_pt3(b, c, j, k) = wm_interm_4_so_pt3(b, c, j, k) + sum 
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
wm_interm_5_so_pt3(b, c) = wm_interm_5_so_pt3(b, c) + sum 
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
wm_interm_6_so_pt3(b, c) = wm_interm_6_so_pt3(b, c) + sum 
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
wm_interm_7_so_pt3(i, j, k, l) = wm_interm_7_so_pt3(i, j, k, l) + sum 
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
wm_interm_8_so_pt3(i, j, k, l) = wm_interm_8_so_pt3(i, j, k, l) + sum 
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
wm_interm_9_so_pt3(b, c) = wm_interm_9_so_pt3(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_10_so_pt3(b, c, j, k) = wm_interm_10_so_pt3(b, c, j, k) + sum 
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
wm_interm_11_so_pt3(j, k) = wm_interm_11_so_pt3(j, k) + sum 
end do 
end do 
!$omp end do nowait 
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
wm_interm_12_so_pt3(b, c, j, k) = wm_interm_12_so_pt3(b, c, j, k) + sum 
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
wm_interm_13_so_pt3(j, k) = wm_interm_13_so_pt3(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_14_so_pt3(b, c, j, k) = wm_interm_14_so_pt3(b, c, j, k) + sum 
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
wm_interm_15_so_pt3(j, k) = wm_interm_15_so_pt3(j, k) + sum 
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
wm_interm_16_so_pt3(b, j, i, k) = wm_interm_16_so_pt3(b, j, i, k) + sum 
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
wm_interm_17_so_pt3(j, k) = wm_interm_17_so_pt3(j, k) + sum 
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
wm_interm_18_so_pt3(j, k) = wm_interm_18_so_pt3(j, k) + sum 
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
wm_interm_19_so_pt3(j, k) = wm_interm_19_so_pt3(j, k) + sum 
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
wm_interm_20_so_pt3(i, j, k, l) = wm_interm_20_so_pt3(i, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
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
wm_interm_21_so_pt3(b, c, j, k) = wm_interm_21_so_pt3(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_22_so_pt3(b, c, j, k) = wm_interm_22_so_pt3(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
sum = sum + r2p(vrdav_Rr, b,i,a,j) * s2(a,c,k,i)
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
sum = sum + r2p(vrdav_Rr, a,i,b,j) * s2(a,c,k,i)
end do 
end do 
wm_interm_25_so_pt3(b, c, j, k) = wm_interm_25_so_pt3(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_26_so_pt3(b, c, j, k) = wm_interm_26_so_pt3(b, c, j, k) + sum 
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
wm_interm_27_so_pt3(b, c) = wm_interm_27_so_pt3(b, c) + sum 
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
wm_interm_28_so_pt3(b, c) = wm_interm_28_so_pt3(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_29_so_pt3(b, c, j, k) = wm_interm_29_so_pt3(b, c, j, k) + sum 
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
sum = sum + r2(vrdav_Rl, a,j,b,i) * r2p(vrdav_Rr, c,i,a,j)
end do 
end do 
end do 
wm_interm_30_so_pt3(b, c) = wm_interm_30_so_pt3(b, c) + sum 
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
wm_interm_31_so_pt3(b, i, j, k) = wm_interm_31_so_pt3(b, i, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_32_so_pt3(b, c, j, k) = wm_interm_32_so_pt3(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_33_so_pt3(b, c, j, k) = wm_interm_33_so_pt3(b, c, j, k) + sum 
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
wm_interm_34_so_pt3(b, c) = wm_interm_34_so_pt3(b, c) + sum 
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
wm_interm_35_so_pt3(b, c) = wm_interm_35_so_pt3(b, c) + sum 
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
wm_interm_36_so_pt3(i, j, k, l) = wm_interm_36_so_pt3(i, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_37_so_pt3(b, c, j, k) = wm_interm_37_so_pt3(b, c, j, k) + sum 
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
wm_interm_38_so_pt3(j, k) = wm_interm_38_so_pt3(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_39_so_pt3(b, c, j, k) = wm_interm_39_so_pt3(b, c, j, k) + sum 
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
wm_interm_40_so_pt3(j, k) = wm_interm_40_so_pt3(j, k) + sum 
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
wm_interm_41_so_pt3(j, k) = wm_interm_41_so_pt3(j, k) + sum 
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
wm_interm_42_so_pt3(j, k) = wm_interm_42_so_pt3(j, k) + sum 
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
wm_interm_43_so_pt3(j, k) = wm_interm_43_so_pt3(j, k) + sum 
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
wm_interm_44_so_pt3(i, j, k, l) = wm_interm_44_so_pt3(i, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_45_so_pt3(b, c, j, k) = wm_interm_45_so_pt3(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_46_so_pt3(b, c, j, k) = wm_interm_46_so_pt3(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_47_so_pt3(b, c, j, k) = wm_interm_47_so_pt3(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_48_so_pt3(b, c, j, k) = wm_interm_48_so_pt3(b, c, j, k) + sum 
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
wm_interm_49_so_pt3(b, c) = wm_interm_49_so_pt3(b, c) + sum 
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
wm_interm_50_so_pt3(b, c) = wm_interm_50_so_pt3(b, c) + sum 
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
wm_interm_51_so_pt3(b, j) = wm_interm_51_so_pt3(b, j) + sum 
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
wm_interm_52_so_pt3(b, j) = wm_interm_52_so_pt3(b, j) + sum 
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
wm_interm_53_so_pt3(b, c) = wm_interm_53_so_pt3(b, c) + sum 
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
wm_interm_54_so_pt3(b, c) = wm_interm_54_so_pt3(b, c) + sum 
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
wm_interm_55_so_pt3(j, k) = wm_interm_55_so_pt3(j, k) + sum 
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
wm_interm_56_so_pt3(b, i, j, k) = wm_interm_56_so_pt3(b, i, j, k) + sum 
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
wm_interm_57_so_pt3(j, k) = wm_interm_57_so_pt3(j, k) + sum 
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
wm_interm_58_so_pt3(b, i, j, k) = wm_interm_58_so_pt3(b, i, j, k) + sum 
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
wm_interm_59_so_pt3(b, j, i, k) = wm_interm_59_so_pt3(b, j, i, k) + sum 
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
wm_interm_60_so_pt3(b, i, j, k) = wm_interm_60_so_pt3(b, i, j, k) + sum 
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
wm_interm_61_so_pt3(b, j) = wm_interm_61_so_pt3(b, j) + sum 
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
wm_interm_62_so_pt3(b, j) = wm_interm_62_so_pt3(b, j) + sum 
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
wm_interm_63_so_pt3(b, j) = wm_interm_63_so_pt3(b, j) + sum 
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
wm_interm_64_so_pt3(b, j) = wm_interm_64_so_pt3(b, j) + sum 
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
wm_interm_65_so_pt3(b, j) = wm_interm_65_so_pt3(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_66_so_pt3(b, c, j, k) = wm_interm_66_so_pt3(b, c, j, k) + sum 
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
wm_interm_67_so_pt3(b, j) = wm_interm_67_so_pt3(b, j) + sum 
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
wm_interm_68_so_pt3(b, j) = wm_interm_68_so_pt3(b, j) + sum 
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
wm_interm_69_so_pt3(b, i, k, j) = wm_interm_69_so_pt3(b, i, k, j) + sum 
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
sum = sum + r2(vrdav_Rl, a,i,b,j) * t2(a,b,k,l)
end do 
end do 
wm_interm_70_so_pt3(i, j, k, l) = wm_interm_70_so_pt3(i, j, k, l) + sum 
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
wm_interm_71_so_pt3(b, i, j, k) = wm_interm_71_so_pt3(b, i, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
sum = sum + r2(vrdav_Rl, a,j,b,i) * t2(a,c,i,k)
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
sum = sum + r2(vrdav_Rl, a,i,b,j) * t2(a,c,i,k)
end do 
end do 
wm_interm_74_so_pt3(b, c, j, k) = wm_interm_74_so_pt3(b, c, j, k) + sum 
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
wm_interm_75_so_pt3(a, b) = wm_interm_75_so_pt3(a, b) + sum 
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
wm_interm_76_so_pt3(b, i, j, k) = wm_interm_76_so_pt3(b, i, j, k) + sum 
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
wm_interm_77_so_pt3(b, i, j, k) = wm_interm_77_so_pt3(b, i, j, k) + sum 
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
wm_interm_78_so_pt3(i, j) = wm_interm_78_so_pt3(i, j) + sum 
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
wm_interm_79_so_pt3(b, i, j, k) = wm_interm_79_so_pt3(b, i, j, k) + sum 
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
wm_interm_80_so_pt3(b, j) = wm_interm_80_so_pt3(b, j) + sum 
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
wm_interm_81_so_pt3(b, j) = wm_interm_81_so_pt3(b, j) + sum 
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
wm_interm_82_so_pt3(b, j) = wm_interm_82_so_pt3(b, j) + sum 
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
wm_interm_83_so_pt3(a, b, i, j) = wm_interm_83_so_pt3(a, b, i, j) + sum 
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
wm_interm_84_so_pt3(b, j) = wm_interm_84_so_pt3(b, j) + sum 
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
wm_interm_85_so_pt3(b, j) = wm_interm_85_so_pt3(b, j) + sum 
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
wm_interm_86_so_pt3(b, i, j, k) = wm_interm_86_so_pt3(b, i, j, k) + sum 
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
sum = sum + r1(vrdav_Rr, a,i) * s1(b,j)
wm_interm_87_so_pt3(a, b, i, j) = wm_interm_87_so_pt3(a, b, i, j) + sum 
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
wm_interm_88_so_pt3(b, j, i, k) = wm_interm_88_so_pt3(b, j, i, k) + sum 
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
wm_interm_89_so_pt3(a, b) = wm_interm_89_so_pt3(a, b) + sum 
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
wm_interm_90_so_pt3(i, j) = wm_interm_90_so_pt3(i, j) + sum 
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
wm_interm_91_so_pt3(b, j) = wm_interm_91_so_pt3(b, j) + sum 
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
wm_interm_92_so_pt3(b, j) = wm_interm_92_so_pt3(b, j) + sum 
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
wm_interm_93_so_pt3(i, j) = wm_interm_93_so_pt3(i, j) + sum 
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
wm_interm_94_so_pt3(b, i, j, k) = wm_interm_94_so_pt3(b, i, j, k) + sum 
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
wm_interm_95_so_pt3(b, j) = wm_interm_95_so_pt3(b, j) + sum 
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
wm_interm_96_so_pt3(a, b) = wm_interm_96_so_pt3(a, b) + sum 
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
wm_interm_97_so_pt3(b, j) = wm_interm_97_so_pt3(b, j) + sum 
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
wm_interm_98_so_pt3(b, i, j, k) = wm_interm_98_so_pt3(b, i, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_99_so_pt3(b, c, j, k) = wm_interm_99_so_pt3(b, c, j, k) + sum 
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
wm_interm_100_so_pt3(i, j, k, l) = wm_interm_100_so_pt3(i, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_101_so_pt3(b, c, j, k) = wm_interm_101_so_pt3(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_102_so_pt3(b, c, j, k) = wm_interm_102_so_pt3(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wm_interm_103_so_pt3(b, c, j, k) = wm_interm_103_so_pt3(b, c, j, k) + sum 
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
wm_interm_104_so_pt3(b, c) = wm_interm_104_so_pt3(b, c) + sum 
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
wm_interm_105_so_pt3(j, k) = wm_interm_105_so_pt3(j, k) + sum 
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
wm_interm_106_so_pt3(j, k) = wm_interm_106_so_pt3(j, k) + sum 
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
wm_interm_107_so_pt3(b, c) = wm_interm_107_so_pt3(b, c) + sum 
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
wm_interm_108_so_pt3(b, c) = wm_interm_108_so_pt3(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 



    end subroutine wm_so_intermediates_ccsd_pt3

        
    function calc_D_oo_wm_so_pt3(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_oo_wm_so_pt3
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
    real(F64), dimension(0:59) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do i = 1, nocc 
term(0) = term(0) + wm_interm_17_so_pt3(q,i) * wm_interm_78_so_pt3(p,i)
term(1) = term(1) + wm_interm_18_so_pt3(q,i) * wm_interm_78_so_pt3(p,i)
term(2) = term(2) + wm_interm_19_so_pt3(q,i) * wm_interm_78_so_pt3(p,i)
term(3) = term(3) + wm_interm_41_so_pt3(q,i) * wm_interm_78_so_pt3(p,i)
term(4) = term(4) + wm_interm_42_so_pt3(q,i) * wm_interm_78_so_pt3(p,i)
term(5) = term(5) + wm_interm_43_so_pt3(q,i) * wm_interm_78_so_pt3(p,i)
term(6) = term(6) + wm_interm_17_so_pt3(i,p) * wm_interm_78_so_pt3(i,q)
term(7) = term(7) + wm_interm_18_so_pt3(i,p) * wm_interm_78_so_pt3(i,q)
term(8) = term(8) + wm_interm_19_so_pt3(i,p) * wm_interm_78_so_pt3(i,q)
term(9) = term(9) + wm_interm_41_so_pt3(i,p) * wm_interm_78_so_pt3(i,q)
term(10) = term(10) + wm_interm_42_so_pt3(i,p) * wm_interm_78_so_pt3(i,q)
term(11) = term(11) + wm_interm_43_so_pt3(i,p) * wm_interm_78_so_pt3(i,q)
term(12) = term(12) + wm_interm_55_so_pt3(p,i) * wm_interm_90_so_pt3(q,i)
term(13) = term(13) + wm_interm_57_so_pt3(p,i) * wm_interm_90_so_pt3(q,i)
term(14) = term(14) + wm_interm_55_so_pt3(i,q) * wm_interm_90_so_pt3(i,p)
term(15) = term(15) + wm_interm_57_so_pt3(i,q) * wm_interm_90_so_pt3(i,p)
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
term(12) = term(12) * (-2.0d+0) 
term(13) = term(13) * (4.0d+0) 
term(14) = term(14) * (-2.0d+0) 
term(15) = term(15) * (4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
term(16) = term(16) + wm_interm_20_so_pt3(q,i,j,p) * wm_interm_78_so_pt3(i,j)
term(17) = term(17) + wm_interm_20_so_pt3(i,q,p,j) * wm_interm_78_so_pt3(i,j)
term(18) = term(18) + wm_interm_20_so_pt3(q,i,p,j) * wm_interm_78_so_pt3(i,j)
term(19) = term(19) + wm_interm_44_so_pt3(q,i,j,p) * wm_interm_78_so_pt3(i,j)
term(20) = term(20) + wm_interm_44_so_pt3(i,q,p,j) * wm_interm_78_so_pt3(i,j)
term(21) = term(21) + wm_interm_44_so_pt3(q,i,p,j) * wm_interm_78_so_pt3(i,j)
term(22) = term(22) + wm_interm_70_so_pt3(i,p,q,j) * wm_interm_90_so_pt3(i,j)
term(23) = term(23) + wm_interm_70_so_pt3(p,i,j,q) * wm_interm_90_so_pt3(i,j)
end do 
end do 

term(16) = term(16) * (-1.0d+0) 
term(17) = term(17) * (-1.0d+0) 
term(18) = term(18) * (2.0d+0) 
term(19) = term(19) * (-2.0d+0) 
term(20) = term(20) * (-2.0d+0) 
term(21) = term(21) * (4.0d+0) 
term(22) = term(22) * (-1.0d+0) 
term(23) = term(23) * (-1.0d+0) 

do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(24) = term(24) + wm_interm_21_so_pt3(a,b,q,p) * wm_interm_75_so_pt3(a,b)
term(25) = term(25) + wm_interm_22_so_pt3(a,b,q,p) * wm_interm_75_so_pt3(a,b)
term(26) = term(26) + wm_interm_26_so_pt3(a,b,q,p) * wm_interm_75_so_pt3(a,b)
term(27) = term(27) + wm_interm_23_so_pt3(a,b,q,p) * wm_interm_75_so_pt3(a,b)
term(28) = term(28) + wm_interm_25_so_pt3(a,b,q,p) * wm_interm_75_so_pt3(a,b)
term(29) = term(29) + wm_interm_29_so_pt3(a,b,q,p) * wm_interm_75_so_pt3(a,b)
term(30) = term(30) + wm_interm_45_so_pt3(a,b,q,p) * wm_interm_75_so_pt3(a,b)
term(31) = term(31) + wm_interm_46_so_pt3(a,b,q,p) * wm_interm_75_so_pt3(a,b)
term(32) = term(32) + wm_interm_47_so_pt3(a,b,q,p) * wm_interm_75_so_pt3(a,b)
term(33) = term(33) + wm_interm_48_so_pt3(a,b,q,p) * wm_interm_75_so_pt3(a,b)
term(34) = term(34) + wm_interm_83_so_pt3(a,b,p,q) * wm_interm_9_so_pt3(a,b)
term(35) = term(35) + wm_interm_5_so_pt3(a,b) * wm_interm_83_so_pt3(a,b,p,q)
term(36) = term(36) + wm_interm_6_so_pt3(a,b) * wm_interm_83_so_pt3(a,b,p,q)
term(37) = term(37) + wm_interm_34_so_pt3(a,b) * wm_interm_83_so_pt3(a,b,p,q)
term(38) = term(38) + wm_interm_35_so_pt3(a,b) * wm_interm_83_so_pt3(a,b,p,q)
term(39) = term(39) + wm_interm_53_so_pt3(a,b) * wm_interm_87_so_pt3(a,b,q,p)
term(40) = term(40) + wm_interm_54_so_pt3(a,b) * wm_interm_87_so_pt3(a,b,q,p)
term(41) = term(41) + wm_interm_72_so_pt3(a,b,p,q) * wm_interm_89_so_pt3(a,b)
term(42) = term(42) + wm_interm_73_so_pt3(a,b,p,q) * wm_interm_89_so_pt3(a,b)
term(43) = term(43) + wm_interm_74_so_pt3(a,b,p,q) * wm_interm_89_so_pt3(a,b)
end do 
end do 

term(24) = term(24) * (-1.0d+0) 
term(25) = term(25) * (2.0d+0) 
term(26) = term(26) * (-1.0d+0) 
term(27) = term(27) * (-1.0d+0) 
term(28) = term(28) * (-1.0d+0) 
term(29) = term(29) * (2.0d+0) 
term(30) = term(30) * (-4.0d+0) 
term(31) = term(31) * (4.0d+0) 
term(32) = term(32) * (-4.0d+0) 
term(33) = term(33) * (4.0d+0) 
term(34) = term(34) * (-1.0d+0) 
term(35) = term(35) * (-1.0d+0) 
term(36) = term(36) * (2.0d+0) 
term(37) = term(37) * (-4.0d+0) 
term(38) = term(38) * (4.0d+0) 
term(39) = term(39) * (4.0d+0) 
term(40) = term(40) * (-2.0d+0) 
term(41) = term(41) * (-2.0d+0) 
term(42) = term(42) * (-2.0d+0) 
term(43) = term(43) * (4.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(44) = term(44) + wm_interm_1_so_pt3(a,p,i,q) * wm_interm_80_so_pt3(a,i)
term(45) = term(45) + wm_interm_1_so_pt3(a,p,i,q) * wm_interm_81_so_pt3(a,i)
term(46) = term(46) + wm_interm_1_so_pt3(a,p,i,q) * wm_interm_82_so_pt3(a,i)
term(47) = term(47) + wm_interm_1_so_pt3(a,p,i,q) * wm_interm_84_so_pt3(a,i)
term(48) = term(48) + wm_interm_1_so_pt3(a,p,i,q) * wm_interm_85_so_pt3(a,i)
term(49) = term(49) + wm_interm_51_so_pt3(a,i) * wm_interm_86_so_pt3(a,p,i,q)
term(50) = term(50) + wm_interm_51_so_pt3(a,i) * wm_interm_86_so_pt3(a,i,p,q)
term(51) = term(51) + wm_interm_51_so_pt3(a,i) * wm_interm_88_so_pt3(a,p,i,q)
term(52) = term(52) + wm_interm_51_so_pt3(a,i) * wm_interm_88_so_pt3(a,i,p,q)
term(53) = term(53) + wm_interm_52_so_pt3(a,i) * wm_interm_88_so_pt3(a,p,i,q)
term(54) = term(54) + wm_interm_52_so_pt3(a,i) * wm_interm_86_so_pt3(a,p,i,q)
term(55) = term(55) + wm_interm_52_so_pt3(a,i) * wm_interm_86_so_pt3(a,i,p,q)
term(56) = term(56) + wm_interm_52_so_pt3(a,i) * wm_interm_88_so_pt3(a,i,p,q)
term(57) = term(57) + wm_interm_69_so_pt3(a,p,q,i) * wm_interm_91_so_pt3(a,i)
end do 
end do 

term(44) = term(44) * (-1.0d+0) 
term(45) = term(45) * (2.0d+0) 
term(46) = term(46) * (-1.0d+0) 
term(47) = term(47) * (-4.0d+0) 
term(48) = term(48) * (4.0d+0) 
term(49) = term(49) * (-4.0d+0) 
term(50) = term(50) * (2.0d+0) 
term(51) = term(51) * (2.0d+0) 
term(52) = term(52) * (-4.0d+0) 
term(53) = term(53) * (-1.0d+0) 
term(54) = term(54) * (2.0d+0) 
term(55) = term(55) * (-1.0d+0) 
term(56) = term(56) * (2.0d+0) 
term(57) = term(57) * (-2.0d+0) 

do a = nocc + 1, nactive 
term(58) = term(58) + wm_interm_51_so_pt3(a,q) * wm_interm_91_so_pt3(a,p)
term(59) = term(59) + wm_interm_52_so_pt3(a,q) * wm_interm_91_so_pt3(a,p)
end do 

term(58) = term(58) * (4.0d+0) 
term(59) = term(59) * (-2.0d+0) 


    calc_D_oo_wm_so_pt3 = zero
    do s = 0, 59
    calc_D_oo_wm_so_pt3 = calc_D_oo_wm_so_pt3 + term(s)
    end do

    end function calc_D_oo_wm_so_pt3
    
    function calc_D_ov_wm_so_pt3(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_ov_wm_so_pt3
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
    real(F64), dimension(0:645) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + wm_interm_0_so_pt3(q,a,i,j) * wm_interm_1_so_pt3(a,j,p,i)
term(1) = term(1) + wm_interm_1_so_pt3(a,i,p,j) * wm_interm_2_so_pt3(q,a,j,i)
term(2) = term(2) + wm_interm_1_so_pt3(a,i,p,j) * wm_interm_3_so_pt3(q,a,j,i)
term(3) = term(3) + wm_interm_1_so_pt3(a,p,i,j) * wm_interm_2_so_pt3(q,a,j,i)
term(4) = term(4) + wm_interm_1_so_pt3(a,p,i,j) * wm_interm_4_so_pt3(q,a,j,i)
term(5) = term(5) + wm_interm_1_so_pt3(a,p,i,j) * wm_interm_3_so_pt3(q,a,j,i)
term(6) = term(6) + wm_interm_10_so_pt3(q,a,i,j) * wm_interm_1_so_pt3(a,p,j,i)
term(7) = term(7) + wm_interm_12_so_pt3(q,a,i,j) * wm_interm_1_so_pt3(a,p,j,i)
term(8) = term(8) + wm_interm_14_so_pt3(q,a,i,j) * wm_interm_1_so_pt3(a,p,j,i)
term(9) = term(9) + wm_interm_16_so_pt3(a,p,i,j) * wm_interm_21_so_pt3(a,q,i,j)
term(10) = term(10) + wm_interm_16_so_pt3(a,p,i,j) * wm_interm_22_so_pt3(a,q,i,j)
term(11) = term(11) + wm_interm_16_so_pt3(a,p,i,j) * wm_interm_23_so_pt3(a,q,i,j)
term(12) = term(12) + wm_interm_16_so_pt3(a,p,i,j) * wm_interm_24_so_pt3(a,q,i,j)
term(13) = term(13) + wm_interm_16_so_pt3(a,p,i,j) * wm_interm_25_so_pt3(a,q,i,j)
term(14) = term(14) + wm_interm_16_so_pt3(a,p,i,j) * wm_interm_26_so_pt3(a,q,i,j)
term(15) = term(15) + wm_interm_16_so_pt3(a,i,p,j) * wm_interm_24_so_pt3(a,q,i,j)
term(16) = term(16) + wm_interm_16_so_pt3(a,i,p,j) * wm_interm_29_so_pt3(a,q,i,j)
term(17) = term(17) + wm_interm_16_so_pt3(a,i,p,j) * wm_interm_25_so_pt3(a,q,i,j)
term(18) = term(18) + wm_interm_21_so_pt3(a,q,i,j) * wm_interm_31_so_pt3(a,i,p,j)
term(19) = term(19) + wm_interm_22_so_pt3(a,q,i,j) * wm_interm_31_so_pt3(a,i,p,j)
term(20) = term(20) + wm_interm_26_so_pt3(a,q,i,j) * wm_interm_31_so_pt3(a,i,p,j)
term(21) = term(21) + wm_interm_29_so_pt3(a,q,i,j) * wm_interm_31_so_pt3(a,p,i,j)
term(22) = term(22) + wm_interm_25_so_pt3(a,q,i,j) * wm_interm_31_so_pt3(a,p,i,j)
term(23) = term(23) + wm_interm_24_so_pt3(a,q,i,j) * wm_interm_31_so_pt3(a,p,i,j)
term(24) = term(24) + wm_interm_25_so_pt3(a,q,i,j) * wm_interm_31_so_pt3(a,i,p,j)
term(25) = term(25) + wm_interm_23_so_pt3(a,q,i,j) * wm_interm_31_so_pt3(a,i,p,j)
term(26) = term(26) + wm_interm_24_so_pt3(a,q,i,j) * wm_interm_31_so_pt3(a,i,p,j)
term(27) = term(27) + wm_interm_1_so_pt3(a,i,p,j) * wm_interm_32_so_pt3(q,a,j,i)
term(28) = term(28) + wm_interm_1_so_pt3(a,i,p,j) * wm_interm_33_so_pt3(q,a,j,i)
term(29) = term(29) + wm_interm_1_so_pt3(a,p,i,j) * wm_interm_33_so_pt3(q,a,j,i)
term(30) = term(30) + wm_interm_1_so_pt3(a,p,i,j) * wm_interm_32_so_pt3(q,a,j,i)
term(31) = term(31) + wm_interm_1_so_pt3(a,p,i,j) * wm_interm_37_so_pt3(q,a,j,i)
term(32) = term(32) + wm_interm_1_so_pt3(a,p,i,j) * wm_interm_39_so_pt3(q,a,j,i)
term(33) = term(33) + wm_interm_16_so_pt3(a,p,i,j) * wm_interm_45_so_pt3(a,q,i,j)
term(34) = term(34) + wm_interm_16_so_pt3(a,p,i,j) * wm_interm_46_so_pt3(a,q,i,j)
term(35) = term(35) + wm_interm_16_so_pt3(a,p,i,j) * wm_interm_47_so_pt3(a,q,i,j)
term(36) = term(36) + wm_interm_16_so_pt3(a,p,i,j) * wm_interm_48_so_pt3(a,q,i,j)
term(37) = term(37) + wm_interm_16_so_pt3(a,i,p,j) * wm_interm_48_so_pt3(a,q,i,j)
term(38) = term(38) + wm_interm_16_so_pt3(a,i,p,j) * wm_interm_47_so_pt3(a,q,i,j)
term(39) = term(39) + wm_interm_31_so_pt3(a,i,p,j) * wm_interm_45_so_pt3(a,q,i,j)
term(40) = term(40) + wm_interm_31_so_pt3(a,i,p,j) * wm_interm_46_so_pt3(a,q,i,j)
term(41) = term(41) + wm_interm_31_so_pt3(a,p,i,j) * wm_interm_48_so_pt3(a,q,i,j)
term(42) = term(42) + wm_interm_31_so_pt3(a,p,i,j) * wm_interm_47_so_pt3(a,q,i,j)
term(43) = term(43) + wm_interm_31_so_pt3(a,i,p,j) * wm_interm_47_so_pt3(a,q,i,j)
term(44) = term(44) + wm_interm_31_so_pt3(a,i,p,j) * wm_interm_48_so_pt3(a,q,i,j)
term(45) = term(45) + r1(vrdav_Rl, a,p) * wm_interm_17_so_pt3(i,j) * wm_interm_99_so_pt3(q,a,i,j)
term(46) = term(46) + r1(vrdav_Rl, a,p) * wm_interm_18_so_pt3(i,j) * wm_interm_99_so_pt3(q,a,i,j)
term(47) = term(47) + r1(vrdav_Rl, a,p) * wm_interm_19_so_pt3(i,j) * wm_interm_99_so_pt3(q,a,i,j)
term(48) = term(48) + r1(vrdav_Rl, a,p) * wm_interm_102_so_pt3(q,a,i,j) * wm_interm_17_so_pt3(i,j)
term(49) = term(49) + r1(vrdav_Rl, a,p) * wm_interm_102_so_pt3(q,a,i,j) * wm_interm_18_so_pt3(i,j)
term(50) = term(50) + r1(vrdav_Rl, a,p) * wm_interm_103_so_pt3(q,a,i,j) * wm_interm_17_so_pt3(i,j)
term(51) = term(51) + r1(vrdav_Rl, a,p) * wm_interm_103_so_pt3(q,a,i,j) * wm_interm_18_so_pt3(i,j)
term(52) = term(52) + r1(vrdav_Rl, a,p) * wm_interm_102_so_pt3(q,a,i,j) * wm_interm_19_so_pt3(i,j)
term(53) = term(53) + r1(vrdav_Rl, a,p) * wm_interm_103_so_pt3(q,a,i,j) * wm_interm_19_so_pt3(i,j)
term(54) = term(54) + r1(vrdav_Rl, a,p) * wm_interm_41_so_pt3(i,j) * wm_interm_99_so_pt3(q,a,i,j)
term(55) = term(55) + r1(vrdav_Rl, a,p) * wm_interm_42_so_pt3(i,j) * wm_interm_99_so_pt3(q,a,i,j)
term(56) = term(56) + r1(vrdav_Rl, a,p) * wm_interm_43_so_pt3(i,j) * wm_interm_99_so_pt3(q,a,i,j)
term(57) = term(57) + r1(vrdav_Rl, a,p) * wm_interm_102_so_pt3(q,a,i,j) * wm_interm_41_so_pt3(i,j)
term(58) = term(58) + r1(vrdav_Rl, a,p) * wm_interm_102_so_pt3(q,a,i,j) * wm_interm_42_so_pt3(i,j)
term(59) = term(59) + r1(vrdav_Rl, a,p) * wm_interm_103_so_pt3(q,a,i,j) * wm_interm_41_so_pt3(i,j)
term(60) = term(60) + r1(vrdav_Rl, a,p) * wm_interm_103_so_pt3(q,a,i,j) * wm_interm_42_so_pt3(i,j)
term(61) = term(61) + r1(vrdav_Rl, a,p) * wm_interm_102_so_pt3(q,a,i,j) * wm_interm_43_so_pt3(i,j)
term(62) = term(62) + r1(vrdav_Rl, a,p) * wm_interm_103_so_pt3(q,a,i,j) * wm_interm_43_so_pt3(i,j)
term(63) = term(63) + r1(vrdav_Rl, a,i) * wm_interm_105_so_pt3(j,i) * wm_interm_29_so_pt3(a,q,j,p)
term(64) = term(64) + r1(vrdav_Rl, a,i) * wm_interm_105_so_pt3(j,i) * wm_interm_25_so_pt3(a,q,j,p)
term(65) = term(65) + r1(vrdav_Rl, a,i) * wm_interm_106_so_pt3(j,i) * wm_interm_29_so_pt3(a,q,j,p)
term(66) = term(66) + r1(vrdav_Rl, a,i) * wm_interm_106_so_pt3(j,i) * wm_interm_25_so_pt3(a,q,j,p)
term(67) = term(67) + r1(vrdav_Rl, a,i) * wm_interm_105_so_pt3(j,i) * wm_interm_24_so_pt3(a,q,j,p)
term(68) = term(68) + r1(vrdav_Rl, a,i) * wm_interm_106_so_pt3(j,i) * wm_interm_24_so_pt3(a,q,j,p)
term(69) = term(69) + s2(a,q,i,j) * wm_interm_17_so_pt3(i,p) * wm_interm_97_so_pt3(a,j)
term(70) = term(70) + s2(a,q,i,j) * wm_interm_18_so_pt3(i,p) * wm_interm_97_so_pt3(a,j)
term(71) = term(71) + s2(a,q,i,j) * wm_interm_19_so_pt3(i,p) * wm_interm_97_so_pt3(a,j)
term(72) = term(72) + s2(a,q,i,j) * wm_interm_17_so_pt3(i,p) * wm_interm_95_so_pt3(a,j)
term(73) = term(73) + s2(a,q,i,j) * wm_interm_18_so_pt3(i,p) * wm_interm_95_so_pt3(a,j)
term(74) = term(74) + s2(a,q,i,j) * wm_interm_19_so_pt3(i,p) * wm_interm_95_so_pt3(a,j)
term(75) = term(75) + r1(vrdav_Rl, a,i) * wm_interm_105_so_pt3(j,i) * wm_interm_48_so_pt3(a,q,j,p)
term(76) = term(76) + r1(vrdav_Rl, a,i) * wm_interm_105_so_pt3(j,i) * wm_interm_47_so_pt3(a,q,j,p)
term(77) = term(77) + r1(vrdav_Rl, a,i) * wm_interm_106_so_pt3(j,i) * wm_interm_48_so_pt3(a,q,j,p)
term(78) = term(78) + r1(vrdav_Rl, a,i) * wm_interm_106_so_pt3(j,i) * wm_interm_47_so_pt3(a,q,j,p)
term(79) = term(79) + s2(a,q,i,j) * wm_interm_41_so_pt3(i,p) * wm_interm_97_so_pt3(a,j)
term(80) = term(80) + s2(a,q,i,j) * wm_interm_42_so_pt3(i,p) * wm_interm_97_so_pt3(a,j)
term(81) = term(81) + s2(a,q,i,j) * wm_interm_43_so_pt3(i,p) * wm_interm_97_so_pt3(a,j)
term(82) = term(82) + s2(a,q,i,j) * wm_interm_41_so_pt3(i,p) * wm_interm_95_so_pt3(a,j)
term(83) = term(83) + s2(a,q,i,j) * wm_interm_42_so_pt3(i,p) * wm_interm_95_so_pt3(a,j)
term(84) = term(84) + s2(a,q,i,j) * wm_interm_43_so_pt3(i,p) * wm_interm_95_so_pt3(a,j)
end do 
end do 
end do 

term(0) = term(0) * (-1.0d+0) 
term(1) = term(1) * (2.0d+0) 
term(2) = term(2) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 
term(4) = term(4) * (-1.0d+0) 
term(5) = term(5) * (2.0d+0) 
term(6) = term(6) * (-1.0d+0) 
term(7) = term(7) * (-1.0d+0) 
term(8) = term(8) * (2.0d+0) 
term(9) = term(9) * (-0.5d+0) 
term(11) = term(11) * (-0.5d+0) 
term(13) = term(13) * (-0.5d+0) 
term(14) = term(14) * (-0.5d+0) 
term(15) = term(15) * (-0.5d+0) 
term(16) = term(16) * (-0.5d+0) 
term(18) = term(18) * (-0.5d+0) 
term(20) = term(20) * (-0.5d+0) 
term(21) = term(21) * (-0.5d+0) 
term(23) = term(23) * (-0.5d+0) 
term(24) = term(24) * (-0.5d+0) 
term(25) = term(25) * (-0.5d+0) 
term(27) = term(27) * (-4.0d+0) 
term(28) = term(28) * (4.0d+0) 
term(29) = term(29) * (-4.0d+0) 
term(30) = term(30) * (4.0d+0) 
term(31) = term(31) * (-4.0d+0) 
term(32) = term(32) * (4.0d+0) 
term(33) = term(33) * (-2.0d+0) 
term(34) = term(34) * (2.0d+0) 
term(35) = term(35) * (-2.0d+0) 
term(36) = term(36) * (2.0d+0) 
term(37) = term(37) * (-2.0d+0) 
term(38) = term(38) * (2.0d+0) 
term(39) = term(39) * (-2.0d+0) 
term(40) = term(40) * (2.0d+0) 
term(41) = term(41) * (-2.0d+0) 
term(42) = term(42) * (2.0d+0) 
term(43) = term(43) * (-2.0d+0) 
term(44) = term(44) * (2.0d+0) 
term(45) = term(45) * (2.0d+0) 
term(46) = term(46) * (-4.0d+0) 
term(47) = term(47) * (2.0d+0) 
term(48) = term(48) * (2.0d+0) 
term(49) = term(49) * (-4.0d+0) 
term(50) = term(50) * (-4.0d+0) 
term(51) = term(51) * (8.0d+0) 
term(52) = term(52) * (2.0d+0) 
term(53) = term(53) * (-4.0d+0) 
term(54) = term(54) * (4.0d+0) 
term(55) = term(55) * (-8.0d+0) 
term(56) = term(56) * (4.0d+0) 
term(57) = term(57) * (4.0d+0) 
term(58) = term(58) * (-8.0d+0) 
term(59) = term(59) * (-8.0d+0) 
term(60) = term(60) * (16.0d+0) 
term(61) = term(61) * (4.0d+0) 
term(62) = term(62) * (-8.0d+0) 
term(63) = term(63) * (-1.0d+0) 
term(64) = term(64) * (2.0d+0) 
term(65) = term(65) * (2.0d+0) 
term(66) = term(66) * (-4.0d+0) 
term(67) = term(67) * (-1.0d+0) 
term(68) = term(68) * (2.0d+0) 
term(69) = term(69) * (2.0d+0) 
term(70) = term(70) * (-4.0d+0) 
term(71) = term(71) * (2.0d+0) 
term(72) = term(72) * (-1.0d+0) 
term(73) = term(73) * (2.0d+0) 
term(74) = term(74) * (-1.0d+0) 
term(75) = term(75) * (-4.0d+0) 
term(76) = term(76) * (4.0d+0) 
term(77) = term(77) * (8.0d+0) 
term(78) = term(78) * (-8.0d+0) 
term(79) = term(79) * (4.0d+0) 
term(80) = term(80) * (-8.0d+0) 
term(81) = term(81) * (4.0d+0) 
term(82) = term(82) * (-2.0d+0) 
term(83) = term(83) * (4.0d+0) 
term(84) = term(84) * (-2.0d+0) 

do l = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
term(85) = term(85) + r1(vrdav_Rl, q,i) * wm_interm_100_so_pt3(j,k,i,l) * wm_interm_20_so_pt3(j,k,l,p)
term(86) = term(86) + r1(vrdav_Rl, q,i) * wm_interm_100_so_pt3(j,k,i,l) * wm_interm_20_so_pt3(k,j,p,l)
term(87) = term(87) + r1(vrdav_Rl, q,i) * wm_interm_100_so_pt3(j,k,i,l) * wm_interm_20_so_pt3(j,k,p,l)
term(88) = term(88) + r1(vrdav_Rl, q,i) * wm_interm_100_so_pt3(j,k,i,l) * wm_interm_44_so_pt3(j,k,l,p)
term(89) = term(89) + r1(vrdav_Rl, q,i) * wm_interm_100_so_pt3(j,k,i,l) * wm_interm_44_so_pt3(k,j,p,l)
term(90) = term(90) + r1(vrdav_Rl, q,i) * wm_interm_100_so_pt3(j,k,i,l) * wm_interm_44_so_pt3(j,k,p,l)
end do 
end do 
end do 
end do 

term(85) = term(85) * (2.0d+0) 
term(86) = term(86) * (2.0d+0) 
term(87) = term(87) * (-4.0d+0) 
term(88) = term(88) * (4.0d+0) 
term(89) = term(89) * (4.0d+0) 
term(90) = term(90) * (-8.0d+0) 

do a = nocc + 1, nactive 
term(91) = term(91) + wm_interm_61_so_pt3(a,p) * wm_interm_9_so_pt3(a,q)
term(92) = term(92) + wm_interm_5_so_pt3(a,q) * wm_interm_61_so_pt3(a,p)
term(93) = term(93) + wm_interm_61_so_pt3(a,p) * wm_interm_6_so_pt3(a,q)
term(94) = term(94) + wm_interm_62_so_pt3(a,p) * wm_interm_9_so_pt3(a,q)
term(95) = term(95) + wm_interm_5_so_pt3(a,q) * wm_interm_62_so_pt3(a,p)
term(96) = term(96) + wm_interm_62_so_pt3(a,p) * wm_interm_6_so_pt3(a,q)
term(97) = term(97) + wm_interm_34_so_pt3(a,q) * wm_interm_61_so_pt3(a,p)
term(98) = term(98) + wm_interm_35_so_pt3(a,q) * wm_interm_61_so_pt3(a,p)
term(99) = term(99) + wm_interm_34_so_pt3(a,q) * wm_interm_62_so_pt3(a,p)
term(100) = term(100) + wm_interm_35_so_pt3(a,q) * wm_interm_62_so_pt3(a,p)
term(101) = term(101) + wm_interm_75_so_pt3(q,a) * wm_interm_92_so_pt3(a,p)
end do 

term(91) = term(91) * (2.0d+0) 
term(92) = term(92) * (2.0d+0) 
term(93) = term(93) * (-4.0d+0) 
term(94) = term(94) * (-1.0d+0) 
term(95) = term(95) * (-1.0d+0) 
term(96) = term(96) * (2.0d+0) 
term(97) = term(97) * (8.0d+0) 
term(98) = term(98) * (-8.0d+0) 
term(99) = term(99) * (-4.0d+0) 
term(100) = term(100) * (4.0d+0) 
term(101) = term(101) * (2.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(102) = term(102) + s2(a,q,p,i) * wm_interm_100_so_pt3(j,k,l,i) * wm_interm_76_so_pt3(a,l,j,k)
term(103) = term(103) + s2(a,q,p,i) * wm_interm_100_so_pt3(j,k,l,i) * wm_interm_79_so_pt3(a,l,j,k)
end do 
end do 
end do 
end do 
end do 

term(102) = term(102) * (2.0d+0) 
term(103) = term(103) * (-8.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(104) = term(104) + wm_interm_1_so_pt3(q,i,j,k) * wm_interm_7_so_pt3(p,k,j,i)
term(105) = term(105) + wm_interm_1_so_pt3(q,i,j,k) * wm_interm_8_so_pt3(k,p,j,i)
term(106) = term(106) + wm_interm_20_so_pt3(i,j,k,p) * wm_interm_31_so_pt3(q,j,i,k)
term(107) = term(107) + wm_interm_20_so_pt3(i,j,p,k) * wm_interm_31_so_pt3(q,j,i,k)
term(108) = term(108) + wm_interm_1_so_pt3(q,i,j,k) * wm_interm_36_so_pt3(p,k,j,i)
term(109) = term(109) + wm_interm_1_so_pt3(q,i,j,k) * wm_interm_36_so_pt3(k,p,j,i)
term(110) = term(110) + r1(vrdav_Rl, q,i) * wm_interm_100_so_pt3(j,p,i,k) * wm_interm_17_so_pt3(j,k)
term(111) = term(111) + r1(vrdav_Rl, q,i) * wm_interm_100_so_pt3(j,p,i,k) * wm_interm_18_so_pt3(j,k)
term(112) = term(112) + r1(vrdav_Rl, q,i) * wm_interm_100_so_pt3(j,p,i,k) * wm_interm_19_so_pt3(j,k)
term(113) = term(113) + r1(vrdav_Rl, q,i) * wm_interm_100_so_pt3(j,p,i,k) * wm_interm_41_so_pt3(j,k)
term(114) = term(114) + r1(vrdav_Rl, q,i) * wm_interm_100_so_pt3(j,p,i,k) * wm_interm_42_so_pt3(j,k)
term(115) = term(115) + r1(vrdav_Rl, q,i) * wm_interm_100_so_pt3(j,p,i,k) * wm_interm_43_so_pt3(j,k)
end do 
end do 
end do 

term(104) = term(104) * (-0.5d+0) 
term(105) = term(105) * (-0.5d+0) 
term(106) = term(106) * (-0.5d+0) 
term(108) = term(108) * (-2.0d+0) 
term(109) = term(109) * (2.0d+0) 
term(110) = term(110) * (2.0d+0) 
term(111) = term(111) * (-4.0d+0) 
term(112) = term(112) * (2.0d+0) 
term(113) = term(113) * (4.0d+0) 
term(114) = term(114) * (-8.0d+0) 
term(115) = term(115) * (4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(116) = term(116) + wm_interm_1_so_pt3(q,i,j,k) * wm_interm_7_so_pt3(p,k,i,j)
term(117) = term(117) + wm_interm_1_so_pt3(q,i,j,k) * wm_interm_8_so_pt3(p,k,i,j)
term(118) = term(118) + wm_interm_16_so_pt3(q,i,j,k) * wm_interm_20_so_pt3(i,j,k,p)
term(119) = term(119) + wm_interm_16_so_pt3(q,i,j,k) * wm_interm_20_so_pt3(j,i,p,k)
term(120) = term(120) + wm_interm_16_so_pt3(q,i,j,k) * wm_interm_20_so_pt3(i,j,p,k)
term(121) = term(121) + wm_interm_1_so_pt3(q,i,j,k) * wm_interm_7_so_pt3(k,p,i,j)
term(122) = term(122) + wm_interm_1_so_pt3(q,i,j,k) * wm_interm_8_so_pt3(k,p,i,j)
term(123) = term(123) + wm_interm_20_so_pt3(i,j,p,k) * wm_interm_31_so_pt3(q,i,j,k)
term(124) = term(124) + wm_interm_1_so_pt3(q,i,j,k) * wm_interm_36_so_pt3(p,k,i,j)
term(125) = term(125) + wm_interm_16_so_pt3(q,i,j,k) * wm_interm_44_so_pt3(i,j,k,p)
term(126) = term(126) + wm_interm_16_so_pt3(q,i,j,k) * wm_interm_44_so_pt3(j,i,p,k)
term(127) = term(127) + wm_interm_16_so_pt3(q,i,j,k) * wm_interm_44_so_pt3(i,j,p,k)
term(128) = term(128) + wm_interm_1_so_pt3(q,i,j,k) * wm_interm_36_so_pt3(k,p,i,j)
term(129) = term(129) + wm_interm_31_so_pt3(q,i,j,k) * wm_interm_44_so_pt3(j,i,k,p)
term(130) = term(130) + wm_interm_31_so_pt3(q,i,j,k) * wm_interm_44_so_pt3(i,j,p,k)
term(131) = term(131) + wm_interm_31_so_pt3(q,i,j,k) * wm_interm_44_so_pt3(j,i,p,k)
end do 
end do 
end do 

term(117) = term(117) * (-0.5d+0) 
term(118) = term(118) * (-0.5d+0) 
term(119) = term(119) * (-0.5d+0) 
term(121) = term(121) * (-0.5d+0) 
term(123) = term(123) * (-0.5d+0) 
term(124) = term(124) * (2.0d+0) 
term(125) = term(125) * (-1.0d+0) 
term(126) = term(126) * (-1.0d+0) 
term(127) = term(127) * (2.0d+0) 
term(128) = term(128) * (-2.0d+0) 
term(129) = term(129) * (-1.0d+0) 
term(130) = term(130) * (-1.0d+0) 
term(131) = term(131) * (2.0d+0) 

do i = 1, nocc 
term(132) = term(132) + wm_interm_17_so_pt3(i,p) * wm_interm_61_so_pt3(q,i)
term(133) = term(133) + wm_interm_18_so_pt3(i,p) * wm_interm_61_so_pt3(q,i)
term(134) = term(134) + wm_interm_19_so_pt3(i,p) * wm_interm_61_so_pt3(q,i)
term(135) = term(135) + wm_interm_17_so_pt3(i,p) * wm_interm_62_so_pt3(q,i)
term(136) = term(136) + wm_interm_18_so_pt3(i,p) * wm_interm_62_so_pt3(q,i)
term(137) = term(137) + wm_interm_19_so_pt3(i,p) * wm_interm_62_so_pt3(q,i)
term(138) = term(138) + wm_interm_41_so_pt3(i,p) * wm_interm_61_so_pt3(q,i)
term(139) = term(139) + wm_interm_42_so_pt3(i,p) * wm_interm_61_so_pt3(q,i)
term(140) = term(140) + wm_interm_43_so_pt3(i,p) * wm_interm_61_so_pt3(q,i)
term(141) = term(141) + wm_interm_41_so_pt3(i,p) * wm_interm_62_so_pt3(q,i)
term(142) = term(142) + wm_interm_42_so_pt3(i,p) * wm_interm_62_so_pt3(q,i)
term(143) = term(143) + wm_interm_43_so_pt3(i,p) * wm_interm_62_so_pt3(q,i)
term(144) = term(144) + wm_interm_78_so_pt3(p,i) * wm_interm_92_so_pt3(q,i)
end do 

term(132) = term(132) * (2.0d+0) 
term(133) = term(133) * (-4.0d+0) 
term(134) = term(134) * (2.0d+0) 
term(135) = term(135) * (-1.0d+0) 
term(136) = term(136) * (2.0d+0) 
term(137) = term(137) * (-1.0d+0) 
term(138) = term(138) * (4.0d+0) 
term(139) = term(139) * (-8.0d+0) 
term(140) = term(140) * (4.0d+0) 
term(141) = term(141) * (-2.0d+0) 
term(142) = term(142) * (4.0d+0) 
term(143) = term(143) * (-2.0d+0) 
term(144) = term(144) * (2.0d+0) 

do a = nocc + 1, nactive 
do i = 1, nocc 
term(145) = term(145) + r1(vrdav_Rl, a,i) * wm_interm_107_so_pt3(q,a) * wm_interm_17_so_pt3(i,p)
term(146) = term(146) + r1(vrdav_Rl, a,i) * wm_interm_108_so_pt3(q,a) * wm_interm_17_so_pt3(i,p)
term(147) = term(147) + r1(vrdav_Rl, a,i) * wm_interm_107_so_pt3(q,a) * wm_interm_18_so_pt3(i,p)
term(148) = term(148) + r1(vrdav_Rl, a,i) * wm_interm_108_so_pt3(q,a) * wm_interm_18_so_pt3(i,p)
term(149) = term(149) + r1(vrdav_Rl, a,i) * wm_interm_107_so_pt3(q,a) * wm_interm_19_so_pt3(i,p)
term(150) = term(150) + r1(vrdav_Rl, a,i) * wm_interm_108_so_pt3(q,a) * wm_interm_19_so_pt3(i,p)
term(151) = term(151) + r1(vrdav_Rl, a,i) * wm_interm_107_so_pt3(q,a) * wm_interm_41_so_pt3(i,p)
term(152) = term(152) + r1(vrdav_Rl, a,i) * wm_interm_108_so_pt3(q,a) * wm_interm_41_so_pt3(i,p)
term(153) = term(153) + r1(vrdav_Rl, a,i) * wm_interm_107_so_pt3(q,a) * wm_interm_42_so_pt3(i,p)
term(154) = term(154) + r1(vrdav_Rl, a,i) * wm_interm_108_so_pt3(q,a) * wm_interm_42_so_pt3(i,p)
term(155) = term(155) + r1(vrdav_Rl, a,i) * wm_interm_107_so_pt3(q,a) * wm_interm_43_so_pt3(i,p)
term(156) = term(156) + r1(vrdav_Rl, a,i) * wm_interm_108_so_pt3(q,a) * wm_interm_43_so_pt3(i,p)
end do 
end do 

term(145) = term(145) * (-1.0d+0) 
term(146) = term(146) * (2.0d+0) 
term(147) = term(147) * (2.0d+0) 
term(148) = term(148) * (-4.0d+0) 
term(149) = term(149) * (-1.0d+0) 
term(150) = term(150) * (2.0d+0) 
term(151) = term(151) * (-2.0d+0) 
term(152) = term(152) * (4.0d+0) 
term(153) = term(153) * (4.0d+0) 
term(154) = term(154) * (-8.0d+0) 
term(155) = term(155) * (-2.0d+0) 
term(156) = term(156) * (4.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(157) = term(157) + s2(a,q,p,i) * wm_interm_17_so_pt3(i,j) * wm_interm_97_so_pt3(a,j)
term(158) = term(158) + s2(a,q,p,i) * wm_interm_18_so_pt3(i,j) * wm_interm_97_so_pt3(a,j)
term(159) = term(159) + s2(a,q,p,i) * wm_interm_19_so_pt3(i,j) * wm_interm_97_so_pt3(a,j)
term(160) = term(160) + s2(a,q,p,i) * wm_interm_17_so_pt3(i,j) * wm_interm_95_so_pt3(a,j)
term(161) = term(161) + s2(a,q,p,i) * wm_interm_18_so_pt3(i,j) * wm_interm_95_so_pt3(a,j)
term(162) = term(162) + s2(a,q,p,i) * wm_interm_19_so_pt3(i,j) * wm_interm_95_so_pt3(a,j)
term(163) = term(163) + s2(a,q,p,i) * wm_interm_41_so_pt3(i,j) * wm_interm_97_so_pt3(a,j)
term(164) = term(164) + s2(a,q,p,i) * wm_interm_42_so_pt3(i,j) * wm_interm_97_so_pt3(a,j)
term(165) = term(165) + s2(a,q,p,i) * wm_interm_43_so_pt3(i,j) * wm_interm_97_so_pt3(a,j)
term(166) = term(166) + s2(a,q,p,i) * wm_interm_41_so_pt3(i,j) * wm_interm_95_so_pt3(a,j)
term(167) = term(167) + s2(a,q,p,i) * wm_interm_42_so_pt3(i,j) * wm_interm_95_so_pt3(a,j)
term(168) = term(168) + s2(a,q,p,i) * wm_interm_43_so_pt3(i,j) * wm_interm_95_so_pt3(a,j)
term(169) = term(169) + s2(a,q,j,i) * wm_interm_17_so_pt3(i,p) * wm_interm_97_so_pt3(a,j)
term(170) = term(170) + s2(a,q,j,i) * wm_interm_18_so_pt3(i,p) * wm_interm_97_so_pt3(a,j)
term(171) = term(171) + s2(a,q,j,i) * wm_interm_19_so_pt3(i,p) * wm_interm_97_so_pt3(a,j)
term(172) = term(172) + s2(a,q,j,i) * wm_interm_17_so_pt3(i,p) * wm_interm_95_so_pt3(a,j)
term(173) = term(173) + s2(a,q,j,i) * wm_interm_18_so_pt3(i,p) * wm_interm_95_so_pt3(a,j)
term(174) = term(174) + s2(a,q,j,i) * wm_interm_19_so_pt3(i,p) * wm_interm_95_so_pt3(a,j)
term(175) = term(175) + s2(a,q,j,i) * wm_interm_41_so_pt3(i,p) * wm_interm_97_so_pt3(a,j)
term(176) = term(176) + s2(a,q,j,i) * wm_interm_42_so_pt3(i,p) * wm_interm_97_so_pt3(a,j)
term(177) = term(177) + s2(a,q,j,i) * wm_interm_43_so_pt3(i,p) * wm_interm_97_so_pt3(a,j)
term(178) = term(178) + s2(a,q,j,i) * wm_interm_41_so_pt3(i,p) * wm_interm_95_so_pt3(a,j)
term(179) = term(179) + s2(a,q,j,i) * wm_interm_42_so_pt3(i,p) * wm_interm_95_so_pt3(a,j)
term(180) = term(180) + s2(a,q,j,i) * wm_interm_43_so_pt3(i,p) * wm_interm_95_so_pt3(a,j)
end do 
end do 
end do 

term(157) = term(157) * (-4.0d+0) 
term(158) = term(158) * (8.0d+0) 
term(159) = term(159) * (-4.0d+0) 
term(160) = term(160) * (2.0d+0) 
term(161) = term(161) * (-4.0d+0) 
term(162) = term(162) * (2.0d+0) 
term(163) = term(163) * (-8.0d+0) 
term(164) = term(164) * (16.0d+0) 
term(165) = term(165) * (-8.0d+0) 
term(166) = term(166) * (4.0d+0) 
term(167) = term(167) * (-8.0d+0) 
term(168) = term(168) * (4.0d+0) 
term(169) = term(169) * (-4.0d+0) 
term(170) = term(170) * (8.0d+0) 
term(171) = term(171) * (-4.0d+0) 
term(172) = term(172) * (2.0d+0) 
term(173) = term(173) * (-4.0d+0) 
term(174) = term(174) * (2.0d+0) 
term(175) = term(175) * (-8.0d+0) 
term(176) = term(176) * (16.0d+0) 
term(177) = term(177) * (-8.0d+0) 
term(178) = term(178) * (4.0d+0) 
term(179) = term(179) * (-8.0d+0) 
term(180) = term(180) * (4.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(181) = term(181) + s2(a,q,p,i) * wm_interm_20_so_pt3(j,i,k,l) * wm_interm_98_so_pt3(a,j,l,k)
term(182) = term(182) + s2(a,q,p,i) * wm_interm_20_so_pt3(i,j,k,l) * wm_interm_98_so_pt3(a,j,l,k)
term(183) = term(183) + s2(a,q,j,i) * wm_interm_20_so_pt3(i,j,k,l) * wm_interm_98_so_pt3(a,p,l,k)
term(184) = term(184) + s2(a,q,p,i) * wm_interm_44_so_pt3(j,i,k,l) * wm_interm_98_so_pt3(a,j,l,k)
term(185) = term(185) + s2(a,q,p,i) * wm_interm_44_so_pt3(i,j,k,l) * wm_interm_98_so_pt3(a,j,l,k)
term(186) = term(186) + s2(a,q,j,i) * wm_interm_44_so_pt3(i,j,k,l) * wm_interm_98_so_pt3(a,p,l,k)
end do 
end do 
end do 
end do 
end do 

term(181) = term(181) * (2.0d+0) 
term(182) = term(182) * (-4.0d+0) 
term(183) = term(183) * (2.0d+0) 
term(184) = term(184) * (4.0d+0) 
term(185) = term(185) * (-8.0d+0) 
term(186) = term(186) * (4.0d+0) 

do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(187) = term(187) + s2(a,b,i,p) * wm_interm_97_so_pt3(b,i) * wm_interm_9_so_pt3(a,q)
term(188) = term(188) + s2(a,b,i,p) * wm_interm_5_so_pt3(a,q) * wm_interm_97_so_pt3(b,i)
term(189) = term(189) + s2(a,b,i,p) * wm_interm_6_so_pt3(a,q) * wm_interm_97_so_pt3(b,i)
term(190) = term(190) + s2(a,b,i,p) * wm_interm_95_so_pt3(b,i) * wm_interm_9_so_pt3(a,q)
term(191) = term(191) + s2(a,b,i,p) * wm_interm_5_so_pt3(a,q) * wm_interm_95_so_pt3(b,i)
term(192) = term(192) + s2(a,b,i,p) * wm_interm_6_so_pt3(a,q) * wm_interm_95_so_pt3(b,i)
term(193) = term(193) + s2(a,b,i,p) * wm_interm_104_so_pt3(a,q) * wm_interm_97_so_pt3(b,i)
term(194) = term(194) + s2(a,b,i,p) * wm_interm_34_so_pt3(a,q) * wm_interm_97_so_pt3(b,i)
term(195) = term(195) + s2(a,b,i,p) * wm_interm_35_so_pt3(a,q) * wm_interm_97_so_pt3(b,i)
term(196) = term(196) + s2(a,b,i,p) * wm_interm_104_so_pt3(a,q) * wm_interm_95_so_pt3(b,i)
term(197) = term(197) + s2(a,b,i,p) * wm_interm_34_so_pt3(a,q) * wm_interm_95_so_pt3(b,i)
term(198) = term(198) + s2(a,b,i,p) * wm_interm_35_so_pt3(a,q) * wm_interm_95_so_pt3(b,i)
end do 
end do 
end do 

term(187) = term(187) * (2.0d+0) 
term(188) = term(188) * (2.0d+0) 
term(189) = term(189) * (-4.0d+0) 
term(190) = term(190) * (-1.0d+0) 
term(191) = term(191) * (-1.0d+0) 
term(192) = term(192) * (2.0d+0) 
term(193) = term(193) * (4.0d+0) 
term(194) = term(194) * (4.0d+0) 
term(195) = term(195) * (-8.0d+0) 
term(196) = term(196) * (-2.0d+0) 
term(197) = term(197) * (-2.0d+0) 
term(198) = term(198) * (4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(199) = term(199) + s2(a,q,p,i) * wm_interm_100_so_pt3(j,k,l,i) * wm_interm_77_so_pt3(a,l,k,j)
term(200) = term(200) + s2(a,q,p,i) * wm_interm_100_so_pt3(j,k,l,i) * wm_interm_76_so_pt3(a,l,k,j)
term(201) = term(201) + s2(a,q,p,i) * wm_interm_100_so_pt3(j,k,l,i) * wm_interm_79_so_pt3(a,l,k,j)
end do 
end do 
end do 
end do 
end do 

term(199) = term(199) * (2.0d+0) 
term(200) = term(200) * (-4.0d+0) 
term(201) = term(201) * (8.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(202) = term(202) + s2(a,q,p,i) * wm_interm_77_so_pt3(b,j,k,i) * wm_interm_99_so_pt3(b,a,k,j)
term(203) = term(203) + s2(a,q,p,i) * wm_interm_79_so_pt3(b,j,k,i) * wm_interm_99_so_pt3(b,a,k,j)
end do 
end do 
end do 
end do 
end do 

term(202) = term(202) * (2.0d+0) 
term(203) = term(203) * (8.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(204) = term(204) + s2(a,q,p,i) * wm_interm_77_so_pt3(b,j,i,k) * wm_interm_99_so_pt3(b,a,k,j)
term(205) = term(205) + s2(a,q,p,i) * wm_interm_101_so_pt3(b,a,j,k) * wm_interm_77_so_pt3(b,k,i,j)
term(206) = term(206) + s2(a,q,j,i) * wm_interm_77_so_pt3(b,k,i,j) * wm_interm_99_so_pt3(b,a,p,k)
term(207) = term(207) + s2(a,q,j,i) * wm_interm_101_so_pt3(b,a,p,k) * wm_interm_77_so_pt3(b,k,i,j)
term(208) = term(208) + s2(a,q,p,i) * wm_interm_101_so_pt3(b,a,j,k) * wm_interm_76_so_pt3(b,k,i,j)
term(209) = term(209) + s2(a,q,p,i) * wm_interm_76_so_pt3(b,j,i,k) * wm_interm_99_so_pt3(b,a,k,j)
term(210) = term(210) + s2(a,q,j,i) * wm_interm_101_so_pt3(b,a,p,k) * wm_interm_76_so_pt3(b,k,i,j)
term(211) = term(211) + s2(a,q,j,i) * wm_interm_76_so_pt3(b,k,i,j) * wm_interm_99_so_pt3(b,a,p,k)
term(212) = term(212) + s2(a,q,p,i) * wm_interm_102_so_pt3(b,a,j,k) * wm_interm_77_so_pt3(b,k,i,j)
term(213) = term(213) + s2(a,q,p,i) * wm_interm_103_so_pt3(b,a,j,k) * wm_interm_77_so_pt3(b,k,i,j)
term(214) = term(214) + s2(a,q,p,i) * wm_interm_102_so_pt3(b,a,j,k) * wm_interm_76_so_pt3(b,k,i,j)
term(215) = term(215) + s2(a,q,p,i) * wm_interm_103_so_pt3(b,a,j,k) * wm_interm_76_so_pt3(b,k,i,j)
term(216) = term(216) + s2(a,q,j,i) * wm_interm_102_so_pt3(b,a,p,k) * wm_interm_77_so_pt3(b,k,i,j)
term(217) = term(217) + s2(a,q,j,i) * wm_interm_103_so_pt3(b,a,p,k) * wm_interm_77_so_pt3(b,k,i,j)
term(218) = term(218) + s2(a,q,j,i) * wm_interm_102_so_pt3(b,a,p,k) * wm_interm_76_so_pt3(b,k,i,j)
term(219) = term(219) + s2(a,q,j,i) * wm_interm_103_so_pt3(b,a,p,k) * wm_interm_76_so_pt3(b,k,i,j)
term(220) = term(220) + s2(a,q,p,i) * wm_interm_79_so_pt3(b,j,i,k) * wm_interm_99_so_pt3(b,a,k,j)
term(221) = term(221) + s2(a,q,p,i) * wm_interm_101_so_pt3(b,a,j,k) * wm_interm_79_so_pt3(b,k,i,j)
term(222) = term(222) + s2(a,q,j,i) * wm_interm_79_so_pt3(b,k,i,j) * wm_interm_99_so_pt3(b,a,p,k)
term(223) = term(223) + s2(a,q,j,i) * wm_interm_101_so_pt3(b,a,p,k) * wm_interm_79_so_pt3(b,k,i,j)
term(224) = term(224) + s2(a,q,p,i) * wm_interm_102_so_pt3(b,a,j,k) * wm_interm_79_so_pt3(b,k,i,j)
term(225) = term(225) + s2(a,q,p,i) * wm_interm_103_so_pt3(b,a,j,k) * wm_interm_79_so_pt3(b,k,i,j)
term(226) = term(226) + s2(a,q,j,i) * wm_interm_102_so_pt3(b,a,p,k) * wm_interm_79_so_pt3(b,k,i,j)
term(227) = term(227) + s2(a,q,j,i) * wm_interm_103_so_pt3(b,a,p,k) * wm_interm_79_so_pt3(b,k,i,j)
end do 
end do 
end do 
end do 
end do 

term(204) = term(204) * (-4.0d+0) 
term(205) = term(205) * (2.0d+0) 
term(206) = term(206) * (2.0d+0) 
term(207) = term(207) * (-4.0d+0) 
term(208) = term(208) * (-4.0d+0) 
term(209) = term(209) * (2.0d+0) 
term(210) = term(210) * (2.0d+0) 
term(211) = term(211) * (-4.0d+0) 
term(212) = term(212) * (-4.0d+0) 
term(213) = term(213) * (8.0d+0) 
term(214) = term(214) * (2.0d+0) 
term(215) = term(215) * (-4.0d+0) 
term(216) = term(216) * (2.0d+0) 
term(217) = term(217) * (-4.0d+0) 
term(218) = term(218) * (-4.0d+0) 
term(219) = term(219) * (8.0d+0) 
term(220) = term(220) * (-8.0d+0) 
term(221) = term(221) * (8.0d+0) 
term(222) = term(222) * (8.0d+0) 
term(223) = term(223) * (-8.0d+0) 
term(224) = term(224) * (-8.0d+0) 
term(225) = term(225) * (16.0d+0) 
term(226) = term(226) * (8.0d+0) 
term(227) = term(227) * (-16.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(228) = term(228) + s2(a,q,j,i) * wm_interm_101_so_pt3(b,a,p,k) * wm_interm_77_so_pt3(b,k,j,i)
term(229) = term(229) + s2(a,q,p,i) * wm_interm_101_so_pt3(b,a,j,k) * wm_interm_76_so_pt3(b,k,j,i)
term(230) = term(230) + s2(a,q,j,i) * wm_interm_76_so_pt3(b,k,j,i) * wm_interm_99_so_pt3(b,a,p,k)
term(231) = term(231) + s2(a,q,p,i) * wm_interm_102_so_pt3(b,a,j,k) * wm_interm_77_so_pt3(b,k,j,i)
term(232) = term(232) + s2(a,q,p,i) * wm_interm_103_so_pt3(b,a,j,k) * wm_interm_77_so_pt3(b,k,j,i)
term(233) = term(233) + s2(a,q,j,i) * wm_interm_102_so_pt3(b,a,p,k) * wm_interm_76_so_pt3(b,k,j,i)
term(234) = term(234) + s2(a,q,j,i) * wm_interm_103_so_pt3(b,a,p,k) * wm_interm_76_so_pt3(b,k,j,i)
term(235) = term(235) + s2(a,q,j,i) * wm_interm_101_so_pt3(b,a,p,k) * wm_interm_79_so_pt3(b,k,j,i)
term(236) = term(236) + s2(a,q,p,i) * wm_interm_101_so_pt3(b,a,j,k) * wm_interm_79_so_pt3(b,k,j,i)
term(237) = term(237) + s2(a,q,j,i) * wm_interm_79_so_pt3(b,k,j,i) * wm_interm_99_so_pt3(b,a,p,k)
term(238) = term(238) + s2(a,q,p,i) * wm_interm_102_so_pt3(b,a,j,k) * wm_interm_79_so_pt3(b,k,j,i)
term(239) = term(239) + s2(a,q,p,i) * wm_interm_103_so_pt3(b,a,j,k) * wm_interm_79_so_pt3(b,k,j,i)
term(240) = term(240) + s2(a,q,j,i) * wm_interm_102_so_pt3(b,a,p,k) * wm_interm_79_so_pt3(b,k,j,i)
term(241) = term(241) + s2(a,q,j,i) * wm_interm_103_so_pt3(b,a,p,k) * wm_interm_79_so_pt3(b,k,j,i)
end do 
end do 
end do 
end do 
end do 

term(228) = term(228) * (2.0d+0) 
term(229) = term(229) * (2.0d+0) 
term(230) = term(230) * (2.0d+0) 
term(231) = term(231) * (2.0d+0) 
term(232) = term(232) * (-4.0d+0) 
term(233) = term(233) * (2.0d+0) 
term(234) = term(234) * (-4.0d+0) 
term(235) = term(235) * (8.0d+0) 
term(236) = term(236) * (-8.0d+0) 
term(237) = term(237) * (-8.0d+0) 
term(238) = term(238) * (8.0d+0) 
term(239) = term(239) * (-16.0d+0) 
term(240) = term(240) * (-8.0d+0) 
term(241) = term(241) * (16.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(242) = term(242) + r1(vrdav_Rl, a,i) * wm_interm_21_so_pt3(b,q,i,j) * wm_interm_99_so_pt3(b,a,p,j)
term(243) = term(243) + r1(vrdav_Rl, a,i) * wm_interm_22_so_pt3(b,q,i,j) * wm_interm_99_so_pt3(b,a,p,j)
term(244) = term(244) + r1(vrdav_Rl, a,i) * wm_interm_26_so_pt3(b,q,i,j) * wm_interm_99_so_pt3(b,a,p,j)
term(245) = term(245) + r1(vrdav_Rl, a,p) * wm_interm_29_so_pt3(b,q,i,j) * wm_interm_99_so_pt3(b,a,i,j)
term(246) = term(246) + r1(vrdav_Rl, a,p) * wm_interm_25_so_pt3(b,q,i,j) * wm_interm_99_so_pt3(b,a,i,j)
term(247) = term(247) + r1(vrdav_Rl, a,p) * wm_interm_24_so_pt3(b,q,i,j) * wm_interm_99_so_pt3(b,a,i,j)
term(248) = term(248) + r1(vrdav_Rl, a,i) * wm_interm_25_so_pt3(b,q,i,j) * wm_interm_99_so_pt3(b,a,p,j)
term(249) = term(249) + r1(vrdav_Rl, a,i) * wm_interm_23_so_pt3(b,q,i,j) * wm_interm_99_so_pt3(b,a,p,j)
term(250) = term(250) + r1(vrdav_Rl, a,i) * wm_interm_24_so_pt3(b,q,i,j) * wm_interm_99_so_pt3(b,a,p,j)
term(251) = term(251) + r1(vrdav_Rl, a,p) * wm_interm_101_so_pt3(b,a,i,j) * wm_interm_21_so_pt3(b,q,i,j)
term(252) = term(252) + r1(vrdav_Rl, a,p) * wm_interm_101_so_pt3(b,a,i,j) * wm_interm_22_so_pt3(b,q,i,j)
term(253) = term(253) + r1(vrdav_Rl, a,p) * wm_interm_101_so_pt3(b,a,i,j) * wm_interm_26_so_pt3(b,q,i,j)
term(254) = term(254) + r1(vrdav_Rl, a,i) * wm_interm_101_so_pt3(b,a,p,j) * wm_interm_29_so_pt3(b,q,i,j)
term(255) = term(255) + r1(vrdav_Rl, a,i) * wm_interm_101_so_pt3(b,a,p,j) * wm_interm_25_so_pt3(b,q,i,j)
term(256) = term(256) + r1(vrdav_Rl, a,i) * wm_interm_101_so_pt3(b,a,p,j) * wm_interm_24_so_pt3(b,q,i,j)
term(257) = term(257) + r1(vrdav_Rl, a,p) * wm_interm_101_so_pt3(b,a,i,j) * wm_interm_25_so_pt3(b,q,i,j)
term(258) = term(258) + r1(vrdav_Rl, a,p) * wm_interm_101_so_pt3(b,a,i,j) * wm_interm_23_so_pt3(b,q,i,j)
term(259) = term(259) + r1(vrdav_Rl, a,p) * wm_interm_101_so_pt3(b,a,i,j) * wm_interm_24_so_pt3(b,q,i,j)
term(260) = term(260) + r1(vrdav_Rl, a,i) * wm_interm_102_so_pt3(b,a,p,j) * wm_interm_21_so_pt3(b,q,i,j)
term(261) = term(261) + r1(vrdav_Rl, a,i) * wm_interm_102_so_pt3(b,a,p,j) * wm_interm_22_so_pt3(b,q,i,j)
term(262) = term(262) + r1(vrdav_Rl, a,i) * wm_interm_102_so_pt3(b,a,p,j) * wm_interm_26_so_pt3(b,q,i,j)
term(263) = term(263) + r1(vrdav_Rl, a,i) * wm_interm_103_so_pt3(b,a,p,j) * wm_interm_21_so_pt3(b,q,i,j)
term(264) = term(264) + r1(vrdav_Rl, a,i) * wm_interm_103_so_pt3(b,a,p,j) * wm_interm_22_so_pt3(b,q,i,j)
term(265) = term(265) + r1(vrdav_Rl, a,i) * wm_interm_103_so_pt3(b,a,p,j) * wm_interm_26_so_pt3(b,q,i,j)
term(266) = term(266) + r1(vrdav_Rl, a,p) * wm_interm_102_so_pt3(b,a,i,j) * wm_interm_29_so_pt3(b,q,i,j)
term(267) = term(267) + r1(vrdav_Rl, a,p) * wm_interm_102_so_pt3(b,a,i,j) * wm_interm_25_so_pt3(b,q,i,j)
term(268) = term(268) + r1(vrdav_Rl, a,p) * wm_interm_103_so_pt3(b,a,i,j) * wm_interm_29_so_pt3(b,q,i,j)
term(269) = term(269) + r1(vrdav_Rl, a,p) * wm_interm_103_so_pt3(b,a,i,j) * wm_interm_25_so_pt3(b,q,i,j)
term(270) = term(270) + r1(vrdav_Rl, a,p) * wm_interm_102_so_pt3(b,a,i,j) * wm_interm_24_so_pt3(b,q,i,j)
term(271) = term(271) + r1(vrdav_Rl, a,p) * wm_interm_103_so_pt3(b,a,i,j) * wm_interm_24_so_pt3(b,q,i,j)
term(272) = term(272) + r1(vrdav_Rl, a,i) * wm_interm_102_so_pt3(b,a,p,j) * wm_interm_25_so_pt3(b,q,i,j)
term(273) = term(273) + r1(vrdav_Rl, a,i) * wm_interm_103_so_pt3(b,a,p,j) * wm_interm_25_so_pt3(b,q,i,j)
term(274) = term(274) + r1(vrdav_Rl, a,i) * wm_interm_102_so_pt3(b,a,p,j) * wm_interm_23_so_pt3(b,q,i,j)
term(275) = term(275) + r1(vrdav_Rl, a,i) * wm_interm_103_so_pt3(b,a,p,j) * wm_interm_23_so_pt3(b,q,i,j)
term(276) = term(276) + r1(vrdav_Rl, a,i) * wm_interm_102_so_pt3(b,a,p,j) * wm_interm_24_so_pt3(b,q,i,j)
term(277) = term(277) + r1(vrdav_Rl, a,i) * wm_interm_103_so_pt3(b,a,p,j) * wm_interm_24_so_pt3(b,q,i,j)
term(278) = term(278) + r1(vrdav_Rl, a,i) * wm_interm_45_so_pt3(b,q,i,j) * wm_interm_99_so_pt3(b,a,p,j)
term(279) = term(279) + r1(vrdav_Rl, a,i) * wm_interm_46_so_pt3(b,q,i,j) * wm_interm_99_so_pt3(b,a,p,j)
term(280) = term(280) + r1(vrdav_Rl, a,p) * wm_interm_48_so_pt3(b,q,i,j) * wm_interm_99_so_pt3(b,a,i,j)
term(281) = term(281) + r1(vrdav_Rl, a,p) * wm_interm_47_so_pt3(b,q,i,j) * wm_interm_99_so_pt3(b,a,i,j)
term(282) = term(282) + r1(vrdav_Rl, a,i) * wm_interm_47_so_pt3(b,q,i,j) * wm_interm_99_so_pt3(b,a,p,j)
term(283) = term(283) + r1(vrdav_Rl, a,i) * wm_interm_48_so_pt3(b,q,i,j) * wm_interm_99_so_pt3(b,a,p,j)
term(284) = term(284) + r1(vrdav_Rl, a,p) * wm_interm_101_so_pt3(b,a,i,j) * wm_interm_45_so_pt3(b,q,i,j)
term(285) = term(285) + r1(vrdav_Rl, a,p) * wm_interm_101_so_pt3(b,a,i,j) * wm_interm_46_so_pt3(b,q,i,j)
term(286) = term(286) + r1(vrdav_Rl, a,i) * wm_interm_101_so_pt3(b,a,p,j) * wm_interm_48_so_pt3(b,q,i,j)
term(287) = term(287) + r1(vrdav_Rl, a,i) * wm_interm_101_so_pt3(b,a,p,j) * wm_interm_47_so_pt3(b,q,i,j)
term(288) = term(288) + r1(vrdav_Rl, a,p) * wm_interm_101_so_pt3(b,a,i,j) * wm_interm_47_so_pt3(b,q,i,j)
term(289) = term(289) + r1(vrdav_Rl, a,p) * wm_interm_101_so_pt3(b,a,i,j) * wm_interm_48_so_pt3(b,q,i,j)
term(290) = term(290) + r1(vrdav_Rl, a,i) * wm_interm_102_so_pt3(b,a,p,j) * wm_interm_45_so_pt3(b,q,i,j)
term(291) = term(291) + r1(vrdav_Rl, a,i) * wm_interm_102_so_pt3(b,a,p,j) * wm_interm_46_so_pt3(b,q,i,j)
term(292) = term(292) + r1(vrdav_Rl, a,i) * wm_interm_103_so_pt3(b,a,p,j) * wm_interm_45_so_pt3(b,q,i,j)
term(293) = term(293) + r1(vrdav_Rl, a,i) * wm_interm_103_so_pt3(b,a,p,j) * wm_interm_46_so_pt3(b,q,i,j)
term(294) = term(294) + r1(vrdav_Rl, a,p) * wm_interm_102_so_pt3(b,a,i,j) * wm_interm_48_so_pt3(b,q,i,j)
term(295) = term(295) + r1(vrdav_Rl, a,p) * wm_interm_102_so_pt3(b,a,i,j) * wm_interm_47_so_pt3(b,q,i,j)
term(296) = term(296) + r1(vrdav_Rl, a,p) * wm_interm_103_so_pt3(b,a,i,j) * wm_interm_48_so_pt3(b,q,i,j)
term(297) = term(297) + r1(vrdav_Rl, a,p) * wm_interm_103_so_pt3(b,a,i,j) * wm_interm_47_so_pt3(b,q,i,j)
term(298) = term(298) + r1(vrdav_Rl, a,i) * wm_interm_102_so_pt3(b,a,p,j) * wm_interm_47_so_pt3(b,q,i,j)
term(299) = term(299) + r1(vrdav_Rl, a,i) * wm_interm_103_so_pt3(b,a,p,j) * wm_interm_47_so_pt3(b,q,i,j)
term(300) = term(300) + r1(vrdav_Rl, a,i) * wm_interm_102_so_pt3(b,a,p,j) * wm_interm_48_so_pt3(b,q,i,j)
term(301) = term(301) + r1(vrdav_Rl, a,i) * wm_interm_103_so_pt3(b,a,p,j) * wm_interm_48_so_pt3(b,q,i,j)
end do 
end do 
end do 
end do 

term(242) = term(242) * (2.0d+0) 
term(243) = term(243) * (-4.0d+0) 
term(244) = term(244) * (2.0d+0) 
term(245) = term(245) * (2.0d+0) 
term(246) = term(246) * (-4.0d+0) 
term(247) = term(247) * (2.0d+0) 
term(248) = term(248) * (2.0d+0) 
term(249) = term(249) * (2.0d+0) 
term(250) = term(250) * (-4.0d+0) 
term(251) = term(251) * (2.0d+0) 
term(252) = term(252) * (-4.0d+0) 
term(253) = term(253) * (2.0d+0) 
term(254) = term(254) * (2.0d+0) 
term(255) = term(255) * (-4.0d+0) 
term(256) = term(256) * (2.0d+0) 
term(257) = term(257) * (2.0d+0) 
term(258) = term(258) * (2.0d+0) 
term(259) = term(259) * (-4.0d+0) 
term(260) = term(260) * (2.0d+0) 
term(261) = term(261) * (-4.0d+0) 
term(262) = term(262) * (2.0d+0) 
term(263) = term(263) * (-4.0d+0) 
term(264) = term(264) * (8.0d+0) 
term(265) = term(265) * (-4.0d+0) 
term(266) = term(266) * (2.0d+0) 
term(267) = term(267) * (-4.0d+0) 
term(268) = term(268) * (-4.0d+0) 
term(269) = term(269) * (8.0d+0) 
term(270) = term(270) * (2.0d+0) 
term(271) = term(271) * (-4.0d+0) 
term(272) = term(272) * (2.0d+0) 
term(273) = term(273) * (-4.0d+0) 
term(274) = term(274) * (2.0d+0) 
term(275) = term(275) * (-4.0d+0) 
term(276) = term(276) * (-4.0d+0) 
term(277) = term(277) * (8.0d+0) 
term(278) = term(278) * (8.0d+0) 
term(279) = term(279) * (-8.0d+0) 
term(280) = term(280) * (8.0d+0) 
term(281) = term(281) * (-8.0d+0) 
term(282) = term(282) * (8.0d+0) 
term(283) = term(283) * (-8.0d+0) 
term(284) = term(284) * (8.0d+0) 
term(285) = term(285) * (-8.0d+0) 
term(286) = term(286) * (8.0d+0) 
term(287) = term(287) * (-8.0d+0) 
term(288) = term(288) * (8.0d+0) 
term(289) = term(289) * (-8.0d+0) 
term(290) = term(290) * (8.0d+0) 
term(291) = term(291) * (-8.0d+0) 
term(292) = term(292) * (-16.0d+0) 
term(293) = term(293) * (16.0d+0) 
term(294) = term(294) * (8.0d+0) 
term(295) = term(295) * (-8.0d+0) 
term(296) = term(296) * (-16.0d+0) 
term(297) = term(297) * (16.0d+0) 
term(298) = term(298) * (8.0d+0) 
term(299) = term(299) * (-16.0d+0) 
term(300) = term(300) * (-8.0d+0) 
term(301) = term(301) * (16.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
term(302) = term(302) + wm_interm_11_so_pt3(i,j) * wm_interm_1_so_pt3(q,j,p,i)
term(303) = term(303) + wm_interm_13_so_pt3(i,j) * wm_interm_1_so_pt3(q,j,p,i)
term(304) = term(304) + wm_interm_15_so_pt3(i,j) * wm_interm_1_so_pt3(q,j,p,i)
end do 
end do 

term(302) = term(302) * (-1.0d+0) 
term(303) = term(303) * (-1.0d+0) 
term(304) = term(304) * (2.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(305) = term(305) + r1(vrdav_Rl, a,i) * wm_interm_20_so_pt3(j,i,k,p) * wm_interm_99_so_pt3(q,a,j,k)
term(306) = term(306) + r1(vrdav_Rl, a,i) * wm_interm_20_so_pt3(i,j,p,k) * wm_interm_99_so_pt3(q,a,j,k)
term(307) = term(307) + r1(vrdav_Rl, a,i) * wm_interm_20_so_pt3(j,i,p,k) * wm_interm_99_so_pt3(q,a,j,k)
term(308) = term(308) + r1(vrdav_Rl, a,i) * wm_interm_101_so_pt3(q,a,j,k) * wm_interm_20_so_pt3(i,j,k,p)
term(309) = term(309) + r1(vrdav_Rl, a,i) * wm_interm_101_so_pt3(q,a,j,k) * wm_interm_20_so_pt3(j,i,p,k)
term(310) = term(310) + r1(vrdav_Rl, a,i) * wm_interm_101_so_pt3(q,a,j,k) * wm_interm_20_so_pt3(i,j,p,k)
term(311) = term(311) + r1(vrdav_Rl, a,i) * wm_interm_102_so_pt3(q,a,j,k) * wm_interm_20_so_pt3(j,i,k,p)
term(312) = term(312) + r1(vrdav_Rl, a,i) * wm_interm_103_so_pt3(q,a,j,k) * wm_interm_20_so_pt3(j,i,k,p)
term(313) = term(313) + r1(vrdav_Rl, a,i) * wm_interm_102_so_pt3(q,a,j,k) * wm_interm_20_so_pt3(i,j,p,k)
term(314) = term(314) + r1(vrdav_Rl, a,i) * wm_interm_103_so_pt3(q,a,j,k) * wm_interm_20_so_pt3(i,j,p,k)
term(315) = term(315) + r1(vrdav_Rl, a,i) * wm_interm_102_so_pt3(q,a,j,k) * wm_interm_20_so_pt3(j,i,p,k)
term(316) = term(316) + r1(vrdav_Rl, a,i) * wm_interm_103_so_pt3(q,a,j,k) * wm_interm_20_so_pt3(j,i,p,k)
term(317) = term(317) + r1(vrdav_Rl, a,i) * wm_interm_44_so_pt3(j,i,k,p) * wm_interm_99_so_pt3(q,a,j,k)
term(318) = term(318) + r1(vrdav_Rl, a,i) * wm_interm_44_so_pt3(i,j,p,k) * wm_interm_99_so_pt3(q,a,j,k)
term(319) = term(319) + r1(vrdav_Rl, a,i) * wm_interm_44_so_pt3(j,i,p,k) * wm_interm_99_so_pt3(q,a,j,k)
term(320) = term(320) + r1(vrdav_Rl, a,i) * wm_interm_101_so_pt3(q,a,j,k) * wm_interm_44_so_pt3(i,j,k,p)
term(321) = term(321) + r1(vrdav_Rl, a,i) * wm_interm_101_so_pt3(q,a,j,k) * wm_interm_44_so_pt3(j,i,p,k)
term(322) = term(322) + r1(vrdav_Rl, a,i) * wm_interm_101_so_pt3(q,a,j,k) * wm_interm_44_so_pt3(i,j,p,k)
term(323) = term(323) + r1(vrdav_Rl, a,i) * wm_interm_102_so_pt3(q,a,j,k) * wm_interm_44_so_pt3(j,i,k,p)
term(324) = term(324) + r1(vrdav_Rl, a,i) * wm_interm_103_so_pt3(q,a,j,k) * wm_interm_44_so_pt3(j,i,k,p)
term(325) = term(325) + r1(vrdav_Rl, a,i) * wm_interm_102_so_pt3(q,a,j,k) * wm_interm_44_so_pt3(i,j,p,k)
term(326) = term(326) + r1(vrdav_Rl, a,i) * wm_interm_103_so_pt3(q,a,j,k) * wm_interm_44_so_pt3(i,j,p,k)
term(327) = term(327) + r1(vrdav_Rl, a,i) * wm_interm_102_so_pt3(q,a,j,k) * wm_interm_44_so_pt3(j,i,p,k)
term(328) = term(328) + r1(vrdav_Rl, a,i) * wm_interm_103_so_pt3(q,a,j,k) * wm_interm_44_so_pt3(j,i,p,k)
end do 
end do 
end do 
end do 

term(305) = term(305) * (2.0d+0) 
term(306) = term(306) * (2.0d+0) 
term(307) = term(307) * (-4.0d+0) 
term(308) = term(308) * (2.0d+0) 
term(309) = term(309) * (2.0d+0) 
term(310) = term(310) * (-4.0d+0) 
term(311) = term(311) * (2.0d+0) 
term(312) = term(312) * (-4.0d+0) 
term(313) = term(313) * (2.0d+0) 
term(314) = term(314) * (-4.0d+0) 
term(315) = term(315) * (-4.0d+0) 
term(316) = term(316) * (8.0d+0) 
term(317) = term(317) * (4.0d+0) 
term(318) = term(318) * (4.0d+0) 
term(319) = term(319) * (-8.0d+0) 
term(320) = term(320) * (4.0d+0) 
term(321) = term(321) * (4.0d+0) 
term(322) = term(322) * (-8.0d+0) 
term(323) = term(323) * (4.0d+0) 
term(324) = term(324) * (-8.0d+0) 
term(325) = term(325) * (4.0d+0) 
term(326) = term(326) * (-8.0d+0) 
term(327) = term(327) * (-8.0d+0) 
term(328) = term(328) * (16.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
term(329) = term(329) + wm_interm_16_so_pt3(q,i,p,j) * wm_interm_17_so_pt3(i,j)
term(330) = term(330) + wm_interm_16_so_pt3(q,i,p,j) * wm_interm_18_so_pt3(i,j)
term(331) = term(331) + wm_interm_16_so_pt3(q,i,p,j) * wm_interm_19_so_pt3(i,j)
term(332) = term(332) + wm_interm_17_so_pt3(i,j) * wm_interm_31_so_pt3(q,p,i,j)
term(333) = term(333) + wm_interm_18_so_pt3(i,j) * wm_interm_31_so_pt3(q,p,i,j)
term(334) = term(334) + wm_interm_19_so_pt3(i,j) * wm_interm_31_so_pt3(q,p,i,j)
term(335) = term(335) + wm_interm_1_so_pt3(q,i,p,j) * wm_interm_38_so_pt3(j,i)
term(336) = term(336) + wm_interm_1_so_pt3(q,i,p,j) * wm_interm_40_so_pt3(j,i)
term(337) = term(337) + wm_interm_16_so_pt3(q,i,p,j) * wm_interm_41_so_pt3(i,j)
term(338) = term(338) + wm_interm_16_so_pt3(q,i,p,j) * wm_interm_42_so_pt3(i,j)
term(339) = term(339) + wm_interm_16_so_pt3(q,i,p,j) * wm_interm_43_so_pt3(i,j)
term(340) = term(340) + wm_interm_31_so_pt3(q,p,i,j) * wm_interm_41_so_pt3(i,j)
term(341) = term(341) + wm_interm_31_so_pt3(q,p,i,j) * wm_interm_42_so_pt3(i,j)
term(342) = term(342) + wm_interm_31_so_pt3(q,p,i,j) * wm_interm_43_so_pt3(i,j)
term(343) = term(343) + wm_interm_1_so_pt3(q,i,p,j) * wm_interm_93_so_pt3(j,i)
term(344) = term(344) + wm_interm_78_so_pt3(i,j) * wm_interm_94_so_pt3(q,i,p,j)
term(345) = term(345) + r1(vrdav_Rl, q,j) * wm_interm_105_so_pt3(i,j) * wm_interm_17_so_pt3(i,p)
term(346) = term(346) + r1(vrdav_Rl, q,j) * wm_interm_106_so_pt3(i,j) * wm_interm_17_so_pt3(i,p)
term(347) = term(347) + r1(vrdav_Rl, q,j) * wm_interm_105_so_pt3(i,j) * wm_interm_18_so_pt3(i,p)
term(348) = term(348) + r1(vrdav_Rl, q,j) * wm_interm_106_so_pt3(i,j) * wm_interm_18_so_pt3(i,p)
term(349) = term(349) + r1(vrdav_Rl, q,j) * wm_interm_105_so_pt3(i,j) * wm_interm_19_so_pt3(i,p)
term(350) = term(350) + r1(vrdav_Rl, q,j) * wm_interm_106_so_pt3(i,j) * wm_interm_19_so_pt3(i,p)
term(351) = term(351) + r1(vrdav_Rl, q,j) * wm_interm_105_so_pt3(i,j) * wm_interm_41_so_pt3(i,p)
term(352) = term(352) + r1(vrdav_Rl, q,j) * wm_interm_106_so_pt3(i,j) * wm_interm_41_so_pt3(i,p)
term(353) = term(353) + r1(vrdav_Rl, q,j) * wm_interm_105_so_pt3(i,j) * wm_interm_42_so_pt3(i,p)
term(354) = term(354) + r1(vrdav_Rl, q,j) * wm_interm_106_so_pt3(i,j) * wm_interm_42_so_pt3(i,p)
term(355) = term(355) + r1(vrdav_Rl, q,j) * wm_interm_105_so_pt3(i,j) * wm_interm_43_so_pt3(i,p)
term(356) = term(356) + r1(vrdav_Rl, q,j) * wm_interm_106_so_pt3(i,j) * wm_interm_43_so_pt3(i,p)
end do 
end do 

term(329) = term(329) * (-0.5d+0) 
term(331) = term(331) * (-0.5d+0) 
term(332) = term(332) * (-0.5d+0) 
term(334) = term(334) * (-0.5d+0) 
term(335) = term(335) * (-4.0d+0) 
term(336) = term(336) * (4.0d+0) 
term(337) = term(337) * (-1.0d+0) 
term(338) = term(338) * (2.0d+0) 
term(339) = term(339) * (-1.0d+0) 
term(340) = term(340) * (-1.0d+0) 
term(341) = term(341) * (2.0d+0) 
term(342) = term(342) * (-1.0d+0) 
term(343) = term(343) * (2.0d+0) 
term(344) = term(344) * (2.0d+0) 
term(345) = term(345) * (-1.0d+0) 
term(346) = term(346) * (2.0d+0) 
term(347) = term(347) * (2.0d+0) 
term(348) = term(348) * (-4.0d+0) 
term(349) = term(349) * (-1.0d+0) 
term(350) = term(350) * (2.0d+0) 
term(351) = term(351) * (-2.0d+0) 
term(352) = term(352) * (4.0d+0) 
term(353) = term(353) * (4.0d+0) 
term(354) = term(354) * (-8.0d+0) 
term(355) = term(355) * (-2.0d+0) 
term(356) = term(356) * (4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(357) = term(357) + s2(a,b,p,i) * wm_interm_21_so_pt3(a,q,i,j) * wm_interm_97_so_pt3(b,j)
term(358) = term(358) + s2(a,b,p,i) * wm_interm_22_so_pt3(a,q,i,j) * wm_interm_97_so_pt3(b,j)
term(359) = term(359) + s2(a,b,p,i) * wm_interm_26_so_pt3(a,q,i,j) * wm_interm_97_so_pt3(b,j)
term(360) = term(360) + s2(a,b,p,i) * wm_interm_25_so_pt3(a,q,i,j) * wm_interm_97_so_pt3(b,j)
term(361) = term(361) + s2(a,b,p,i) * wm_interm_23_so_pt3(a,q,i,j) * wm_interm_97_so_pt3(b,j)
term(362) = term(362) + s2(a,b,p,i) * wm_interm_24_so_pt3(a,q,i,j) * wm_interm_97_so_pt3(b,j)
term(363) = term(363) + s2(a,b,p,i) * wm_interm_21_so_pt3(a,q,i,j) * wm_interm_95_so_pt3(b,j)
term(364) = term(364) + s2(a,b,p,i) * wm_interm_22_so_pt3(a,q,i,j) * wm_interm_95_so_pt3(b,j)
term(365) = term(365) + s2(a,b,p,i) * wm_interm_26_so_pt3(a,q,i,j) * wm_interm_95_so_pt3(b,j)
term(366) = term(366) + s2(a,b,p,i) * wm_interm_25_so_pt3(a,q,i,j) * wm_interm_95_so_pt3(b,j)
term(367) = term(367) + s2(a,b,p,i) * wm_interm_23_so_pt3(a,q,i,j) * wm_interm_95_so_pt3(b,j)
term(368) = term(368) + s2(a,b,p,i) * wm_interm_24_so_pt3(a,q,i,j) * wm_interm_95_so_pt3(b,j)
term(369) = term(369) + s2(a,b,p,i) * wm_interm_45_so_pt3(a,q,i,j) * wm_interm_97_so_pt3(b,j)
term(370) = term(370) + s2(a,b,p,i) * wm_interm_46_so_pt3(a,q,i,j) * wm_interm_97_so_pt3(b,j)
term(371) = term(371) + s2(a,b,p,i) * wm_interm_47_so_pt3(a,q,i,j) * wm_interm_97_so_pt3(b,j)
term(372) = term(372) + s2(a,b,p,i) * wm_interm_48_so_pt3(a,q,i,j) * wm_interm_97_so_pt3(b,j)
term(373) = term(373) + s2(a,b,p,i) * wm_interm_45_so_pt3(a,q,i,j) * wm_interm_95_so_pt3(b,j)
term(374) = term(374) + s2(a,b,p,i) * wm_interm_46_so_pt3(a,q,i,j) * wm_interm_95_so_pt3(b,j)
term(375) = term(375) + s2(a,b,p,i) * wm_interm_47_so_pt3(a,q,i,j) * wm_interm_95_so_pt3(b,j)
term(376) = term(376) + s2(a,b,p,i) * wm_interm_48_so_pt3(a,q,i,j) * wm_interm_95_so_pt3(b,j)
end do 
end do 
end do 
end do 

term(357) = term(357) * (-4.0d+0) 
term(358) = term(358) * (8.0d+0) 
term(359) = term(359) * (-4.0d+0) 
term(360) = term(360) * (-4.0d+0) 
term(361) = term(361) * (-4.0d+0) 
term(362) = term(362) * (8.0d+0) 
term(363) = term(363) * (2.0d+0) 
term(364) = term(364) * (-4.0d+0) 
term(365) = term(365) * (2.0d+0) 
term(366) = term(366) * (2.0d+0) 
term(367) = term(367) * (2.0d+0) 
term(368) = term(368) * (-4.0d+0) 
term(369) = term(369) * (-16.0d+0) 
term(370) = term(370) * (16.0d+0) 
term(371) = term(371) * (-16.0d+0) 
term(372) = term(372) * (16.0d+0) 
term(373) = term(373) * (8.0d+0) 
term(374) = term(374) * (-8.0d+0) 
term(375) = term(375) * (8.0d+0) 
term(376) = term(376) * (-8.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(377) = term(377) + r1(vrdav_Rl, q,i) * wm_interm_29_so_pt3(a,b,j,p) * wm_interm_99_so_pt3(a,b,j,i)
term(378) = term(378) + r1(vrdav_Rl, q,i) * wm_interm_25_so_pt3(a,b,j,p) * wm_interm_99_so_pt3(a,b,j,i)
term(379) = term(379) + r1(vrdav_Rl, q,i) * wm_interm_24_so_pt3(a,b,j,p) * wm_interm_99_so_pt3(a,b,j,i)
term(380) = term(380) + r1(vrdav_Rl, q,i) * wm_interm_101_so_pt3(a,b,j,i) * wm_interm_21_so_pt3(a,b,j,p)
term(381) = term(381) + r1(vrdav_Rl, q,i) * wm_interm_101_so_pt3(a,b,j,i) * wm_interm_22_so_pt3(a,b,j,p)
term(382) = term(382) + r1(vrdav_Rl, q,i) * wm_interm_101_so_pt3(a,b,j,i) * wm_interm_26_so_pt3(a,b,j,p)
term(383) = term(383) + r1(vrdav_Rl, q,i) * wm_interm_101_so_pt3(a,b,j,i) * wm_interm_23_so_pt3(a,b,j,p)
term(384) = term(384) + r1(vrdav_Rl, q,i) * wm_interm_101_so_pt3(a,b,j,i) * wm_interm_25_so_pt3(a,b,j,p)
term(385) = term(385) + r1(vrdav_Rl, q,i) * wm_interm_101_so_pt3(a,b,j,i) * wm_interm_29_so_pt3(a,b,j,p)
term(386) = term(386) + r1(vrdav_Rl, a,i) * wm_interm_101_so_pt3(q,b,j,i) * wm_interm_24_so_pt3(a,b,j,p)
term(387) = term(387) + r1(vrdav_Rl, a,i) * wm_interm_21_so_pt3(a,b,j,p) * wm_interm_99_so_pt3(q,b,j,i)
term(388) = term(388) + r1(vrdav_Rl, a,i) * wm_interm_22_so_pt3(a,b,j,p) * wm_interm_99_so_pt3(q,b,j,i)
term(389) = term(389) + r1(vrdav_Rl, a,i) * wm_interm_23_so_pt3(a,b,j,p) * wm_interm_99_so_pt3(q,b,j,i)
term(390) = term(390) + r1(vrdav_Rl, a,i) * wm_interm_101_so_pt3(q,b,j,i) * wm_interm_29_so_pt3(a,b,j,p)
term(391) = term(391) + r1(vrdav_Rl, a,i) * wm_interm_101_so_pt3(q,b,j,i) * wm_interm_25_so_pt3(a,b,j,p)
term(392) = term(392) + r1(vrdav_Rl, a,i) * wm_interm_26_so_pt3(a,b,j,p) * wm_interm_99_so_pt3(q,b,j,i)
term(393) = term(393) + r1(vrdav_Rl, a,i) * wm_interm_25_so_pt3(a,b,j,p) * wm_interm_99_so_pt3(q,b,j,i)
term(394) = term(394) + r1(vrdav_Rl, a,i) * wm_interm_29_so_pt3(a,b,j,p) * wm_interm_99_so_pt3(q,b,j,i)
term(395) = term(395) + r1(vrdav_Rl, q,i) * wm_interm_102_so_pt3(a,b,j,i) * wm_interm_24_so_pt3(a,b,j,p)
term(396) = term(396) + r1(vrdav_Rl, q,i) * wm_interm_102_so_pt3(a,b,j,i) * wm_interm_29_so_pt3(a,b,j,p)
term(397) = term(397) + r1(vrdav_Rl, q,i) * wm_interm_102_so_pt3(a,b,j,i) * wm_interm_25_so_pt3(a,b,j,p)
term(398) = term(398) + r1(vrdav_Rl, q,i) * wm_interm_103_so_pt3(a,b,j,i) * wm_interm_24_so_pt3(a,b,j,p)
term(399) = term(399) + r1(vrdav_Rl, q,i) * wm_interm_103_so_pt3(a,b,j,i) * wm_interm_29_so_pt3(a,b,j,p)
term(400) = term(400) + r1(vrdav_Rl, q,i) * wm_interm_103_so_pt3(a,b,j,i) * wm_interm_25_so_pt3(a,b,j,p)
term(401) = term(401) + r1(vrdav_Rl, a,i) * wm_interm_102_so_pt3(q,b,j,i) * wm_interm_21_so_pt3(a,b,j,p)
term(402) = term(402) + r1(vrdav_Rl, a,i) * wm_interm_102_so_pt3(q,b,j,i) * wm_interm_22_so_pt3(a,b,j,p)
term(403) = term(403) + r1(vrdav_Rl, a,i) * wm_interm_103_so_pt3(q,b,j,i) * wm_interm_21_so_pt3(a,b,j,p)
term(404) = term(404) + r1(vrdav_Rl, a,i) * wm_interm_103_so_pt3(q,b,j,i) * wm_interm_22_so_pt3(a,b,j,p)
term(405) = term(405) + r1(vrdav_Rl, a,i) * wm_interm_102_so_pt3(q,b,j,i) * wm_interm_26_so_pt3(a,b,j,p)
term(406) = term(406) + r1(vrdav_Rl, a,i) * wm_interm_103_so_pt3(q,b,j,i) * wm_interm_26_so_pt3(a,b,j,p)
term(407) = term(407) + r1(vrdav_Rl, a,i) * wm_interm_102_so_pt3(q,b,j,i) * wm_interm_23_so_pt3(a,b,j,p)
term(408) = term(408) + r1(vrdav_Rl, a,i) * wm_interm_103_so_pt3(q,b,j,i) * wm_interm_23_so_pt3(a,b,j,p)
term(409) = term(409) + r1(vrdav_Rl, a,i) * wm_interm_102_so_pt3(q,b,j,i) * wm_interm_25_so_pt3(a,b,j,p)
term(410) = term(410) + r1(vrdav_Rl, a,i) * wm_interm_103_so_pt3(q,b,j,i) * wm_interm_25_so_pt3(a,b,j,p)
term(411) = term(411) + r1(vrdav_Rl, a,i) * wm_interm_102_so_pt3(q,b,j,i) * wm_interm_29_so_pt3(a,b,j,p)
term(412) = term(412) + r1(vrdav_Rl, a,i) * wm_interm_103_so_pt3(q,b,j,i) * wm_interm_29_so_pt3(a,b,j,p)
term(413) = term(413) + s2(a,q,j,i) * wm_interm_98_so_pt3(b,p,j,i) * wm_interm_9_so_pt3(a,b)
term(414) = term(414) + s2(a,q,j,i) * wm_interm_5_so_pt3(a,b) * wm_interm_98_so_pt3(b,p,j,i)
term(415) = term(415) + s2(a,q,j,i) * wm_interm_6_so_pt3(a,b) * wm_interm_98_so_pt3(b,p,j,i)
term(416) = term(416) + r1(vrdav_Rl, q,i) * wm_interm_48_so_pt3(a,b,j,p) * wm_interm_99_so_pt3(a,b,j,i)
term(417) = term(417) + r1(vrdav_Rl, q,i) * wm_interm_47_so_pt3(a,b,j,p) * wm_interm_99_so_pt3(a,b,j,i)
term(418) = term(418) + r1(vrdav_Rl, q,i) * wm_interm_101_so_pt3(a,b,j,i) * wm_interm_45_so_pt3(a,b,j,p)
term(419) = term(419) + r1(vrdav_Rl, q,i) * wm_interm_101_so_pt3(a,b,j,i) * wm_interm_46_so_pt3(a,b,j,p)
term(420) = term(420) + r1(vrdav_Rl, q,i) * wm_interm_101_so_pt3(a,b,j,i) * wm_interm_47_so_pt3(a,b,j,p)
term(421) = term(421) + r1(vrdav_Rl, q,i) * wm_interm_101_so_pt3(a,b,j,i) * wm_interm_48_so_pt3(a,b,j,p)
term(422) = term(422) + r1(vrdav_Rl, a,i) * wm_interm_101_so_pt3(q,b,j,i) * wm_interm_48_so_pt3(a,b,j,p)
term(423) = term(423) + r1(vrdav_Rl, a,i) * wm_interm_45_so_pt3(a,b,j,p) * wm_interm_99_so_pt3(q,b,j,i)
term(424) = term(424) + r1(vrdav_Rl, a,i) * wm_interm_46_so_pt3(a,b,j,p) * wm_interm_99_so_pt3(q,b,j,i)
term(425) = term(425) + r1(vrdav_Rl, a,i) * wm_interm_47_so_pt3(a,b,j,p) * wm_interm_99_so_pt3(q,b,j,i)
term(426) = term(426) + r1(vrdav_Rl, a,i) * wm_interm_101_so_pt3(q,b,j,i) * wm_interm_47_so_pt3(a,b,j,p)
term(427) = term(427) + r1(vrdav_Rl, a,i) * wm_interm_48_so_pt3(a,b,j,p) * wm_interm_99_so_pt3(q,b,j,i)
term(428) = term(428) + r1(vrdav_Rl, q,i) * wm_interm_102_so_pt3(a,b,j,i) * wm_interm_48_so_pt3(a,b,j,p)
term(429) = term(429) + r1(vrdav_Rl, q,i) * wm_interm_102_so_pt3(a,b,j,i) * wm_interm_47_so_pt3(a,b,j,p)
term(430) = term(430) + r1(vrdav_Rl, q,i) * wm_interm_103_so_pt3(a,b,j,i) * wm_interm_48_so_pt3(a,b,j,p)
term(431) = term(431) + r1(vrdav_Rl, q,i) * wm_interm_103_so_pt3(a,b,j,i) * wm_interm_47_so_pt3(a,b,j,p)
term(432) = term(432) + r1(vrdav_Rl, a,i) * wm_interm_102_so_pt3(q,b,j,i) * wm_interm_45_so_pt3(a,b,j,p)
term(433) = term(433) + r1(vrdav_Rl, a,i) * wm_interm_102_so_pt3(q,b,j,i) * wm_interm_46_so_pt3(a,b,j,p)
term(434) = term(434) + r1(vrdav_Rl, a,i) * wm_interm_103_so_pt3(q,b,j,i) * wm_interm_45_so_pt3(a,b,j,p)
term(435) = term(435) + r1(vrdav_Rl, a,i) * wm_interm_103_so_pt3(q,b,j,i) * wm_interm_46_so_pt3(a,b,j,p)
term(436) = term(436) + r1(vrdav_Rl, a,i) * wm_interm_102_so_pt3(q,b,j,i) * wm_interm_47_so_pt3(a,b,j,p)
term(437) = term(437) + r1(vrdav_Rl, a,i) * wm_interm_103_so_pt3(q,b,j,i) * wm_interm_47_so_pt3(a,b,j,p)
term(438) = term(438) + r1(vrdav_Rl, a,i) * wm_interm_102_so_pt3(q,b,j,i) * wm_interm_48_so_pt3(a,b,j,p)
term(439) = term(439) + r1(vrdav_Rl, a,i) * wm_interm_103_so_pt3(q,b,j,i) * wm_interm_48_so_pt3(a,b,j,p)
term(440) = term(440) + s2(a,q,j,i) * wm_interm_104_so_pt3(a,b) * wm_interm_98_so_pt3(b,p,j,i)
term(441) = term(441) + s2(a,q,j,i) * wm_interm_34_so_pt3(a,b) * wm_interm_98_so_pt3(b,p,j,i)
term(442) = term(442) + s2(a,q,j,i) * wm_interm_35_so_pt3(a,b) * wm_interm_98_so_pt3(b,p,j,i)
end do 
end do 
end do 
end do 

term(377) = term(377) * (2.0d+0) 
term(378) = term(378) * (-4.0d+0) 
term(379) = term(379) * (2.0d+0) 
term(380) = term(380) * (2.0d+0) 
term(381) = term(381) * (-4.0d+0) 
term(382) = term(382) * (2.0d+0) 
term(383) = term(383) * (2.0d+0) 
term(384) = term(384) * (2.0d+0) 
term(385) = term(385) * (-4.0d+0) 
term(386) = term(386) * (2.0d+0) 
term(387) = term(387) * (2.0d+0) 
term(388) = term(388) * (-4.0d+0) 
term(389) = term(389) * (2.0d+0) 
term(390) = term(390) * (2.0d+0) 
term(391) = term(391) * (-4.0d+0) 
term(392) = term(392) * (2.0d+0) 
term(393) = term(393) * (2.0d+0) 
term(394) = term(394) * (-4.0d+0) 
term(395) = term(395) * (2.0d+0) 
term(396) = term(396) * (2.0d+0) 
term(397) = term(397) * (-4.0d+0) 
term(398) = term(398) * (-4.0d+0) 
term(399) = term(399) * (-4.0d+0) 
term(400) = term(400) * (8.0d+0) 
term(401) = term(401) * (2.0d+0) 
term(402) = term(402) * (-4.0d+0) 
term(403) = term(403) * (-4.0d+0) 
term(404) = term(404) * (8.0d+0) 
term(405) = term(405) * (2.0d+0) 
term(406) = term(406) * (-4.0d+0) 
term(407) = term(407) * (2.0d+0) 
term(408) = term(408) * (-4.0d+0) 
term(409) = term(409) * (2.0d+0) 
term(410) = term(410) * (-4.0d+0) 
term(411) = term(411) * (-4.0d+0) 
term(412) = term(412) * (8.0d+0) 
term(413) = term(413) * (2.0d+0) 
term(414) = term(414) * (2.0d+0) 
term(415) = term(415) * (-4.0d+0) 
term(416) = term(416) * (8.0d+0) 
term(417) = term(417) * (-8.0d+0) 
term(418) = term(418) * (8.0d+0) 
term(419) = term(419) * (-8.0d+0) 
term(420) = term(420) * (8.0d+0) 
term(421) = term(421) * (-8.0d+0) 
term(422) = term(422) * (8.0d+0) 
term(423) = term(423) * (8.0d+0) 
term(424) = term(424) * (-8.0d+0) 
term(425) = term(425) * (8.0d+0) 
term(426) = term(426) * (-8.0d+0) 
term(427) = term(427) * (-8.0d+0) 
term(428) = term(428) * (8.0d+0) 
term(429) = term(429) * (-8.0d+0) 
term(430) = term(430) * (-16.0d+0) 
term(431) = term(431) * (16.0d+0) 
term(432) = term(432) * (8.0d+0) 
term(433) = term(433) * (-8.0d+0) 
term(434) = term(434) * (-16.0d+0) 
term(435) = term(435) * (16.0d+0) 
term(436) = term(436) * (8.0d+0) 
term(437) = term(437) * (-16.0d+0) 
term(438) = term(438) * (-8.0d+0) 
term(439) = term(439) * (16.0d+0) 
term(440) = term(440) * (4.0d+0) 
term(441) = term(441) * (4.0d+0) 
term(442) = term(442) * (-8.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(443) = term(443) + s2(a,q,p,i) * wm_interm_26_so_pt3(a,b,j,k) * wm_interm_98_so_pt3(b,j,k,i)
term(444) = term(444) + s2(a,q,p,i) * wm_interm_21_so_pt3(a,b,j,k) * wm_interm_98_so_pt3(b,j,k,i)
term(445) = term(445) + s2(a,q,p,i) * wm_interm_22_so_pt3(a,b,j,k) * wm_interm_98_so_pt3(b,j,k,i)
term(446) = term(446) + s2(a,q,p,i) * wm_interm_25_so_pt3(a,b,j,k) * wm_interm_98_so_pt3(b,j,k,i)
term(447) = term(447) + s2(a,q,p,i) * wm_interm_23_so_pt3(a,b,j,k) * wm_interm_98_so_pt3(b,j,k,i)
term(448) = term(448) + s2(a,q,p,i) * wm_interm_24_so_pt3(a,b,j,k) * wm_interm_98_so_pt3(b,j,k,i)
term(449) = term(449) + s2(a,q,p,i) * wm_interm_29_so_pt3(a,b,j,k) * wm_interm_98_so_pt3(b,j,i,k)
term(450) = term(450) + s2(a,q,p,i) * wm_interm_24_so_pt3(a,b,j,k) * wm_interm_98_so_pt3(b,j,i,k)
term(451) = term(451) + s2(a,q,p,i) * wm_interm_23_so_pt3(a,b,j,k) * wm_interm_98_so_pt3(b,j,i,k)
term(452) = term(452) + s2(a,q,p,i) * wm_interm_26_so_pt3(a,b,j,k) * wm_interm_98_so_pt3(b,j,i,k)
term(453) = term(453) + s2(a,q,p,i) * wm_interm_21_so_pt3(a,b,j,k) * wm_interm_98_so_pt3(b,j,i,k)
term(454) = term(454) + s2(a,q,p,i) * wm_interm_22_so_pt3(a,b,j,k) * wm_interm_98_so_pt3(b,j,i,k)
term(455) = term(455) + s2(a,q,p,i) * wm_interm_45_so_pt3(a,b,j,k) * wm_interm_98_so_pt3(b,j,k,i)
term(456) = term(456) + s2(a,q,p,i) * wm_interm_46_so_pt3(a,b,j,k) * wm_interm_98_so_pt3(b,j,k,i)
term(457) = term(457) + s2(a,q,p,i) * wm_interm_47_so_pt3(a,b,j,k) * wm_interm_98_so_pt3(b,j,k,i)
term(458) = term(458) + s2(a,q,p,i) * wm_interm_48_so_pt3(a,b,j,k) * wm_interm_98_so_pt3(b,j,k,i)
term(459) = term(459) + s2(a,q,p,i) * wm_interm_48_so_pt3(a,b,j,k) * wm_interm_98_so_pt3(b,j,i,k)
term(460) = term(460) + s2(a,q,p,i) * wm_interm_47_so_pt3(a,b,j,k) * wm_interm_98_so_pt3(b,j,i,k)
term(461) = term(461) + s2(a,q,p,i) * wm_interm_45_so_pt3(a,b,j,k) * wm_interm_98_so_pt3(b,j,i,k)
term(462) = term(462) + s2(a,q,p,i) * wm_interm_46_so_pt3(a,b,j,k) * wm_interm_98_so_pt3(b,j,i,k)
end do 
end do 
end do 
end do 
end do 

term(443) = term(443) * (2.0d+0) 
term(444) = term(444) * (2.0d+0) 
term(445) = term(445) * (-4.0d+0) 
term(446) = term(446) * (2.0d+0) 
term(447) = term(447) * (2.0d+0) 
term(448) = term(448) * (-4.0d+0) 
term(449) = term(449) * (2.0d+0) 
term(450) = term(450) * (2.0d+0) 
term(451) = term(451) * (-4.0d+0) 
term(452) = term(452) * (-4.0d+0) 
term(453) = term(453) * (-4.0d+0) 
term(454) = term(454) * (8.0d+0) 
term(455) = term(455) * (8.0d+0) 
term(456) = term(456) * (-8.0d+0) 
term(457) = term(457) * (8.0d+0) 
term(458) = term(458) * (-8.0d+0) 
term(459) = term(459) * (8.0d+0) 
term(460) = term(460) * (-8.0d+0) 
term(461) = term(461) * (-16.0d+0) 
term(462) = term(462) * (16.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(463) = term(463) + r2(vrdav_Rl, a,p,q,i) * t1(b,i) * wm_interm_5_so_pt3(a,b)
term(464) = term(464) + r2(vrdav_Rl, a,p,q,i) * t1(b,i) * wm_interm_6_so_pt3(a,b)
term(465) = term(465) + r2(vrdav_Rl, a,p,q,i) * t1(b,i) * wm_interm_9_so_pt3(a,b)
term(466) = term(466) + r2(vrdav_Rl, a,p,q,i) * t1(b,i) * wm_interm_34_so_pt3(a,b)
term(467) = term(467) + r2(vrdav_Rl, a,p,q,i) * t1(b,i) * wm_interm_35_so_pt3(a,b)
term(468) = term(468) + r1(vrdav_Rl, q,i) * wm_interm_99_so_pt3(a,b,p,i) * wm_interm_9_so_pt3(a,b)
term(469) = term(469) + r1(vrdav_Rl, q,i) * wm_interm_5_so_pt3(a,b) * wm_interm_99_so_pt3(a,b,p,i)
term(470) = term(470) + r1(vrdav_Rl, q,i) * wm_interm_6_so_pt3(a,b) * wm_interm_99_so_pt3(a,b,p,i)
term(471) = term(471) + r1(vrdav_Rl, a,i) * wm_interm_101_so_pt3(q,b,p,i) * wm_interm_5_so_pt3(a,b)
term(472) = term(472) + r1(vrdav_Rl, a,i) * wm_interm_101_so_pt3(q,b,p,i) * wm_interm_6_so_pt3(a,b)
term(473) = term(473) + r1(vrdav_Rl, a,i) * wm_interm_101_so_pt3(q,b,p,i) * wm_interm_9_so_pt3(a,b)
term(474) = term(474) + r1(vrdav_Rl, q,i) * wm_interm_102_so_pt3(a,b,p,i) * wm_interm_5_so_pt3(a,b)
term(475) = term(475) + r1(vrdav_Rl, q,i) * wm_interm_102_so_pt3(a,b,p,i) * wm_interm_6_so_pt3(a,b)
term(476) = term(476) + r1(vrdav_Rl, q,i) * wm_interm_102_so_pt3(a,b,p,i) * wm_interm_9_so_pt3(a,b)
term(477) = term(477) + r1(vrdav_Rl, q,i) * wm_interm_103_so_pt3(a,b,p,i) * wm_interm_5_so_pt3(a,b)
term(478) = term(478) + r1(vrdav_Rl, q,i) * wm_interm_103_so_pt3(a,b,p,i) * wm_interm_6_so_pt3(a,b)
term(479) = term(479) + r1(vrdav_Rl, q,i) * wm_interm_103_so_pt3(a,b,p,i) * wm_interm_9_so_pt3(a,b)
term(480) = term(480) + s2(a,q,p,i) * wm_interm_97_so_pt3(b,i) * wm_interm_9_so_pt3(a,b)
term(481) = term(481) + s2(a,q,p,i) * wm_interm_5_so_pt3(a,b) * wm_interm_97_so_pt3(b,i)
term(482) = term(482) + s2(a,q,p,i) * wm_interm_6_so_pt3(a,b) * wm_interm_97_so_pt3(b,i)
term(483) = term(483) + s2(a,q,p,i) * wm_interm_95_so_pt3(b,i) * wm_interm_9_so_pt3(a,b)
term(484) = term(484) + s2(a,q,p,i) * wm_interm_5_so_pt3(a,b) * wm_interm_95_so_pt3(b,i)
term(485) = term(485) + s2(a,q,p,i) * wm_interm_6_so_pt3(a,b) * wm_interm_95_so_pt3(b,i)
term(486) = term(486) + r1(vrdav_Rl, q,i) * wm_interm_104_so_pt3(a,b) * wm_interm_99_so_pt3(a,b,p,i)
term(487) = term(487) + r1(vrdav_Rl, q,i) * wm_interm_34_so_pt3(a,b) * wm_interm_99_so_pt3(a,b,p,i)
term(488) = term(488) + r1(vrdav_Rl, q,i) * wm_interm_35_so_pt3(a,b) * wm_interm_99_so_pt3(a,b,p,i)
term(489) = term(489) + r1(vrdav_Rl, a,i) * wm_interm_101_so_pt3(q,b,p,i) * wm_interm_34_so_pt3(a,b)
term(490) = term(490) + r1(vrdav_Rl, a,i) * wm_interm_101_so_pt3(q,b,p,i) * wm_interm_35_so_pt3(a,b)
term(491) = term(491) + r1(vrdav_Rl, q,i) * wm_interm_102_so_pt3(a,b,p,i) * wm_interm_34_so_pt3(a,b)
term(492) = term(492) + r1(vrdav_Rl, q,i) * wm_interm_102_so_pt3(a,b,p,i) * wm_interm_35_so_pt3(a,b)
term(493) = term(493) + r1(vrdav_Rl, q,i) * wm_interm_102_so_pt3(a,b,p,i) * wm_interm_104_so_pt3(a,b)
term(494) = term(494) + r1(vrdav_Rl, q,i) * wm_interm_103_so_pt3(a,b,p,i) * wm_interm_34_so_pt3(a,b)
term(495) = term(495) + r1(vrdav_Rl, q,i) * wm_interm_103_so_pt3(a,b,p,i) * wm_interm_35_so_pt3(a,b)
term(496) = term(496) + r1(vrdav_Rl, q,i) * wm_interm_103_so_pt3(a,b,p,i) * wm_interm_104_so_pt3(a,b)
term(497) = term(497) + s2(a,q,p,i) * wm_interm_104_so_pt3(a,b) * wm_interm_97_so_pt3(b,i)
term(498) = term(498) + s2(a,q,p,i) * wm_interm_34_so_pt3(a,b) * wm_interm_97_so_pt3(b,i)
term(499) = term(499) + s2(a,q,p,i) * wm_interm_35_so_pt3(a,b) * wm_interm_97_so_pt3(b,i)
term(500) = term(500) + s2(a,q,p,i) * wm_interm_104_so_pt3(a,b) * wm_interm_95_so_pt3(b,i)
term(501) = term(501) + s2(a,q,p,i) * wm_interm_34_so_pt3(a,b) * wm_interm_95_so_pt3(b,i)
term(502) = term(502) + s2(a,q,p,i) * wm_interm_35_so_pt3(a,b) * wm_interm_95_so_pt3(b,i)
term(503) = term(503) + s2(a,b,p,i) * wm_interm_97_so_pt3(b,i) * wm_interm_9_so_pt3(a,q)
term(504) = term(504) + s2(a,b,p,i) * wm_interm_5_so_pt3(a,q) * wm_interm_97_so_pt3(b,i)
term(505) = term(505) + s2(a,b,p,i) * wm_interm_6_so_pt3(a,q) * wm_interm_97_so_pt3(b,i)
term(506) = term(506) + s2(a,b,p,i) * wm_interm_95_so_pt3(b,i) * wm_interm_9_so_pt3(a,q)
term(507) = term(507) + s2(a,b,p,i) * wm_interm_5_so_pt3(a,q) * wm_interm_95_so_pt3(b,i)
term(508) = term(508) + s2(a,b,p,i) * wm_interm_6_so_pt3(a,q) * wm_interm_95_so_pt3(b,i)
term(509) = term(509) + s2(a,b,p,i) * wm_interm_104_so_pt3(a,q) * wm_interm_97_so_pt3(b,i)
term(510) = term(510) + s2(a,b,p,i) * wm_interm_34_so_pt3(a,q) * wm_interm_97_so_pt3(b,i)
term(511) = term(511) + s2(a,b,p,i) * wm_interm_35_so_pt3(a,q) * wm_interm_97_so_pt3(b,i)
term(512) = term(512) + s2(a,b,p,i) * wm_interm_104_so_pt3(a,q) * wm_interm_95_so_pt3(b,i)
term(513) = term(513) + s2(a,b,p,i) * wm_interm_34_so_pt3(a,q) * wm_interm_95_so_pt3(b,i)
term(514) = term(514) + s2(a,b,p,i) * wm_interm_35_so_pt3(a,q) * wm_interm_95_so_pt3(b,i)
end do 
end do 
end do 

term(463) = term(463) * (-1.0d+0) 
term(464) = term(464) * (2.0d+0) 
term(465) = term(465) * (-1.0d+0) 
term(466) = term(466) * (-4.0d+0) 
term(467) = term(467) * (4.0d+0) 
term(468) = term(468) * (2.0d+0) 
term(469) = term(469) * (2.0d+0) 
term(470) = term(470) * (-4.0d+0) 
term(471) = term(471) * (2.0d+0) 
term(472) = term(472) * (-4.0d+0) 
term(473) = term(473) * (2.0d+0) 
term(474) = term(474) * (2.0d+0) 
term(475) = term(475) * (-4.0d+0) 
term(476) = term(476) * (2.0d+0) 
term(477) = term(477) * (-4.0d+0) 
term(478) = term(478) * (8.0d+0) 
term(479) = term(479) * (-4.0d+0) 
term(480) = term(480) * (-4.0d+0) 
term(481) = term(481) * (-4.0d+0) 
term(482) = term(482) * (8.0d+0) 
term(483) = term(483) * (2.0d+0) 
term(484) = term(484) * (2.0d+0) 
term(485) = term(485) * (-4.0d+0) 
term(486) = term(486) * (4.0d+0) 
term(487) = term(487) * (4.0d+0) 
term(488) = term(488) * (-8.0d+0) 
term(489) = term(489) * (8.0d+0) 
term(490) = term(490) * (-8.0d+0) 
term(491) = term(491) * (4.0d+0) 
term(492) = term(492) * (-8.0d+0) 
term(493) = term(493) * (4.0d+0) 
term(494) = term(494) * (-8.0d+0) 
term(495) = term(495) * (16.0d+0) 
term(496) = term(496) * (-8.0d+0) 
term(497) = term(497) * (-8.0d+0) 
term(498) = term(498) * (-8.0d+0) 
term(499) = term(499) * (16.0d+0) 
term(500) = term(500) * (4.0d+0) 
term(501) = term(501) * (4.0d+0) 
term(502) = term(502) * (-8.0d+0) 
term(503) = term(503) * (-4.0d+0) 
term(504) = term(504) * (-4.0d+0) 
term(505) = term(505) * (8.0d+0) 
term(506) = term(506) * (2.0d+0) 
term(507) = term(507) * (2.0d+0) 
term(508) = term(508) * (-4.0d+0) 
term(509) = term(509) * (-8.0d+0) 
term(510) = term(510) * (-8.0d+0) 
term(511) = term(511) * (16.0d+0) 
term(512) = term(512) * (4.0d+0) 
term(513) = term(513) * (4.0d+0) 
term(514) = term(514) * (-8.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(515) = term(515) + wm_interm_29_so_pt3(a,q,i,p) * wm_interm_61_so_pt3(a,i)
term(516) = term(516) + wm_interm_25_so_pt3(a,q,i,p) * wm_interm_61_so_pt3(a,i)
term(517) = term(517) + wm_interm_24_so_pt3(a,q,i,p) * wm_interm_61_so_pt3(a,i)
term(518) = term(518) + wm_interm_29_so_pt3(a,q,i,p) * wm_interm_62_so_pt3(a,i)
term(519) = term(519) + wm_interm_25_so_pt3(a,q,i,p) * wm_interm_62_so_pt3(a,i)
term(520) = term(520) + wm_interm_24_so_pt3(a,q,i,p) * wm_interm_62_so_pt3(a,i)
term(521) = term(521) + wm_interm_48_so_pt3(a,q,i,p) * wm_interm_61_so_pt3(a,i)
term(522) = term(522) + wm_interm_47_so_pt3(a,q,i,p) * wm_interm_61_so_pt3(a,i)
term(523) = term(523) + wm_interm_48_so_pt3(a,q,i,p) * wm_interm_62_so_pt3(a,i)
term(524) = term(524) + wm_interm_47_so_pt3(a,q,i,p) * wm_interm_62_so_pt3(a,i)
term(525) = term(525) + r1(vrdav_Rl, a,i) * wm_interm_105_so_pt3(p,i) * wm_interm_9_so_pt3(a,q)
term(526) = term(526) + r1(vrdav_Rl, a,i) * wm_interm_106_so_pt3(p,i) * wm_interm_9_so_pt3(a,q)
term(527) = term(527) + r1(vrdav_Rl, a,i) * wm_interm_105_so_pt3(p,i) * wm_interm_5_so_pt3(a,q)
term(528) = term(528) + r1(vrdav_Rl, a,i) * wm_interm_106_so_pt3(p,i) * wm_interm_5_so_pt3(a,q)
term(529) = term(529) + r1(vrdav_Rl, a,i) * wm_interm_105_so_pt3(p,i) * wm_interm_6_so_pt3(a,q)
term(530) = term(530) + r1(vrdav_Rl, a,i) * wm_interm_106_so_pt3(p,i) * wm_interm_6_so_pt3(a,q)
term(531) = term(531) + r1(vrdav_Rl, a,i) * wm_interm_105_so_pt3(p,i) * wm_interm_34_so_pt3(a,q)
term(532) = term(532) + r1(vrdav_Rl, a,i) * wm_interm_106_so_pt3(p,i) * wm_interm_34_so_pt3(a,q)
term(533) = term(533) + r1(vrdav_Rl, a,i) * wm_interm_105_so_pt3(p,i) * wm_interm_35_so_pt3(a,q)
term(534) = term(534) + r1(vrdav_Rl, a,i) * wm_interm_106_so_pt3(p,i) * wm_interm_35_so_pt3(a,q)
end do 
end do 

term(515) = term(515) * (2.0d+0) 
term(516) = term(516) * (-4.0d+0) 
term(517) = term(517) * (2.0d+0) 
term(518) = term(518) * (-1.0d+0) 
term(519) = term(519) * (2.0d+0) 
term(520) = term(520) * (-1.0d+0) 
term(521) = term(521) * (8.0d+0) 
term(522) = term(522) * (-8.0d+0) 
term(523) = term(523) * (-4.0d+0) 
term(524) = term(524) * (4.0d+0) 
term(525) = term(525) * (-1.0d+0) 
term(526) = term(526) * (2.0d+0) 
term(527) = term(527) * (-1.0d+0) 
term(528) = term(528) * (2.0d+0) 
term(529) = term(529) * (2.0d+0) 
term(530) = term(530) * (-4.0d+0) 
term(531) = term(531) * (-4.0d+0) 
term(532) = term(532) * (8.0d+0) 
term(533) = term(533) * (4.0d+0) 
term(534) = term(534) * (-8.0d+0) 

do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(535) = term(535) + s2(a,b,i,p) * wm_interm_29_so_pt3(a,q,i,j) * wm_interm_97_so_pt3(b,j)
term(536) = term(536) + s2(a,b,i,p) * wm_interm_25_so_pt3(a,q,i,j) * wm_interm_97_so_pt3(b,j)
term(537) = term(537) + s2(a,b,i,p) * wm_interm_24_so_pt3(a,q,i,j) * wm_interm_97_so_pt3(b,j)
term(538) = term(538) + s2(a,b,i,p) * wm_interm_29_so_pt3(a,q,i,j) * wm_interm_95_so_pt3(b,j)
term(539) = term(539) + s2(a,b,i,p) * wm_interm_25_so_pt3(a,q,i,j) * wm_interm_95_so_pt3(b,j)
term(540) = term(540) + s2(a,b,i,p) * wm_interm_24_so_pt3(a,q,i,j) * wm_interm_95_so_pt3(b,j)
term(541) = term(541) + s2(a,b,i,p) * wm_interm_48_so_pt3(a,q,i,j) * wm_interm_97_so_pt3(b,j)
term(542) = term(542) + s2(a,b,i,p) * wm_interm_47_so_pt3(a,q,i,j) * wm_interm_97_so_pt3(b,j)
term(543) = term(543) + s2(a,b,i,p) * wm_interm_48_so_pt3(a,q,i,j) * wm_interm_95_so_pt3(b,j)
term(544) = term(544) + s2(a,b,i,p) * wm_interm_47_so_pt3(a,q,i,j) * wm_interm_95_so_pt3(b,j)
term(545) = term(545) + s2(a,b,i,j) * wm_interm_29_so_pt3(a,q,j,p) * wm_interm_97_so_pt3(b,i)
term(546) = term(546) + s2(a,b,i,j) * wm_interm_25_so_pt3(a,q,j,p) * wm_interm_97_so_pt3(b,i)
term(547) = term(547) + s2(a,b,i,j) * wm_interm_29_so_pt3(a,q,i,p) * wm_interm_97_so_pt3(b,j)
term(548) = term(548) + s2(a,b,i,j) * wm_interm_25_so_pt3(a,q,i,p) * wm_interm_97_so_pt3(b,j)
term(549) = term(549) + s2(a,b,i,j) * wm_interm_24_so_pt3(a,q,j,p) * wm_interm_97_so_pt3(b,i)
term(550) = term(550) + s2(a,b,i,j) * wm_interm_24_so_pt3(a,q,i,p) * wm_interm_97_so_pt3(b,j)
term(551) = term(551) + s2(a,b,i,j) * wm_interm_29_so_pt3(a,q,j,p) * wm_interm_95_so_pt3(b,i)
term(552) = term(552) + s2(a,b,i,j) * wm_interm_25_so_pt3(a,q,j,p) * wm_interm_95_so_pt3(b,i)
term(553) = term(553) + s2(a,b,i,j) * wm_interm_29_so_pt3(a,q,i,p) * wm_interm_95_so_pt3(b,j)
term(554) = term(554) + s2(a,b,i,j) * wm_interm_25_so_pt3(a,q,i,p) * wm_interm_95_so_pt3(b,j)
term(555) = term(555) + s2(a,b,i,j) * wm_interm_24_so_pt3(a,q,j,p) * wm_interm_95_so_pt3(b,i)
term(556) = term(556) + s2(a,b,i,j) * wm_interm_24_so_pt3(a,q,i,p) * wm_interm_95_so_pt3(b,j)
term(557) = term(557) + s2(a,b,i,j) * wm_interm_48_so_pt3(a,q,j,p) * wm_interm_97_so_pt3(b,i)
term(558) = term(558) + s2(a,b,i,j) * wm_interm_47_so_pt3(a,q,j,p) * wm_interm_97_so_pt3(b,i)
term(559) = term(559) + s2(a,b,i,j) * wm_interm_48_so_pt3(a,q,i,p) * wm_interm_97_so_pt3(b,j)
term(560) = term(560) + s2(a,b,i,j) * wm_interm_47_so_pt3(a,q,i,p) * wm_interm_97_so_pt3(b,j)
term(561) = term(561) + s2(a,b,i,j) * wm_interm_48_so_pt3(a,q,j,p) * wm_interm_95_so_pt3(b,i)
term(562) = term(562) + s2(a,b,i,j) * wm_interm_47_so_pt3(a,q,j,p) * wm_interm_95_so_pt3(b,i)
term(563) = term(563) + s2(a,b,i,j) * wm_interm_48_so_pt3(a,q,i,p) * wm_interm_95_so_pt3(b,j)
term(564) = term(564) + s2(a,b,i,j) * wm_interm_47_so_pt3(a,q,i,p) * wm_interm_95_so_pt3(b,j)
end do 
end do 
end do 
end do 

term(535) = term(535) * (-4.0d+0) 
term(536) = term(536) * (8.0d+0) 
term(537) = term(537) * (-4.0d+0) 
term(538) = term(538) * (2.0d+0) 
term(539) = term(539) * (-4.0d+0) 
term(540) = term(540) * (2.0d+0) 
term(541) = term(541) * (-16.0d+0) 
term(542) = term(542) * (16.0d+0) 
term(543) = term(543) * (8.0d+0) 
term(544) = term(544) * (-8.0d+0) 
term(545) = term(545) * (2.0d+0) 
term(546) = term(546) * (-4.0d+0) 
term(547) = term(547) * (-4.0d+0) 
term(548) = term(548) * (8.0d+0) 
term(549) = term(549) * (2.0d+0) 
term(550) = term(550) * (-4.0d+0) 
term(551) = term(551) * (-1.0d+0) 
term(552) = term(552) * (2.0d+0) 
term(553) = term(553) * (2.0d+0) 
term(554) = term(554) * (-4.0d+0) 
term(555) = term(555) * (-1.0d+0) 
term(556) = term(556) * (2.0d+0) 
term(557) = term(557) * (8.0d+0) 
term(558) = term(558) * (-8.0d+0) 
term(559) = term(559) * (-16.0d+0) 
term(560) = term(560) * (16.0d+0) 
term(561) = term(561) * (-4.0d+0) 
term(562) = term(562) * (4.0d+0) 
term(563) = term(563) * (8.0d+0) 
term(564) = term(564) * (-8.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(565) = term(565) + s2(a,q,p,i) * t1(b,i) * wm_interm_27_so_pt3(b,a)
term(566) = term(566) + s2(a,q,p,i) * t1(b,i) * wm_interm_28_so_pt3(b,a)
term(567) = term(567) + s2(a,q,p,i) * t1(b,i) * wm_interm_30_so_pt3(b,a)
term(568) = term(568) + s2(a,q,p,i) * t1(b,i) * wm_interm_49_so_pt3(b,a)
term(569) = term(569) + s2(a,q,p,i) * t1(b,i) * wm_interm_50_so_pt3(b,a)
term(570) = term(570) + r1(vrdav_Rl, a,i) * wm_interm_107_so_pt3(b,a) * wm_interm_29_so_pt3(b,q,i,p)
term(571) = term(571) + r1(vrdav_Rl, a,i) * wm_interm_107_so_pt3(b,a) * wm_interm_25_so_pt3(b,q,i,p)
term(572) = term(572) + r1(vrdav_Rl, a,i) * wm_interm_108_so_pt3(b,a) * wm_interm_29_so_pt3(b,q,i,p)
term(573) = term(573) + r1(vrdav_Rl, a,i) * wm_interm_108_so_pt3(b,a) * wm_interm_25_so_pt3(b,q,i,p)
term(574) = term(574) + r1(vrdav_Rl, a,i) * wm_interm_107_so_pt3(b,a) * wm_interm_24_so_pt3(b,q,i,p)
term(575) = term(575) + r1(vrdav_Rl, a,i) * wm_interm_108_so_pt3(b,a) * wm_interm_24_so_pt3(b,q,i,p)
term(576) = term(576) + r1(vrdav_Rl, a,i) * wm_interm_107_so_pt3(b,a) * wm_interm_48_so_pt3(b,q,i,p)
term(577) = term(577) + r1(vrdav_Rl, a,i) * wm_interm_107_so_pt3(b,a) * wm_interm_47_so_pt3(b,q,i,p)
term(578) = term(578) + r1(vrdav_Rl, a,i) * wm_interm_108_so_pt3(b,a) * wm_interm_48_so_pt3(b,q,i,p)
term(579) = term(579) + r1(vrdav_Rl, a,i) * wm_interm_108_so_pt3(b,a) * wm_interm_47_so_pt3(b,q,i,p)
end do 
end do 
end do 

term(565) = term(565) * (-1.0d+0) 
term(566) = term(566) * (-1.0d+0) 
term(567) = term(567) * (2.0d+0) 
term(568) = term(568) * (-4.0d+0) 
term(569) = term(569) * (4.0d+0) 
term(570) = term(570) * (-1.0d+0) 
term(571) = term(571) * (2.0d+0) 
term(572) = term(572) * (2.0d+0) 
term(573) = term(573) * (-4.0d+0) 
term(574) = term(574) * (-1.0d+0) 
term(575) = term(575) * (2.0d+0) 
term(576) = term(576) * (-4.0d+0) 
term(577) = term(577) * (4.0d+0) 
term(578) = term(578) * (8.0d+0) 
term(579) = term(579) * (-8.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(580) = term(580) + s2(a,q,j,i) * wm_interm_29_so_pt3(a,b,k,p) * wm_interm_98_so_pt3(b,k,j,i)
term(581) = term(581) + s2(a,q,j,i) * wm_interm_25_so_pt3(a,b,k,p) * wm_interm_98_so_pt3(b,k,j,i)
term(582) = term(582) + s2(a,q,j,i) * wm_interm_24_so_pt3(a,b,k,p) * wm_interm_98_so_pt3(b,k,j,i)
term(583) = term(583) + s2(a,q,j,i) * wm_interm_48_so_pt3(a,b,k,p) * wm_interm_98_so_pt3(b,k,j,i)
term(584) = term(584) + s2(a,q,j,i) * wm_interm_47_so_pt3(a,b,k,p) * wm_interm_98_so_pt3(b,k,j,i)
end do 
end do 
end do 
end do 
end do 

term(580) = term(580) * (2.0d+0) 
term(581) = term(581) * (-4.0d+0) 
term(582) = term(582) * (2.0d+0) 
term(583) = term(583) * (8.0d+0) 
term(584) = term(584) * (-8.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(585) = term(585) + r1(vrdav_Rl, a,i) * wm_interm_100_so_pt3(p,j,i,k) * wm_interm_21_so_pt3(a,q,j,k)
term(586) = term(586) + r1(vrdav_Rl, a,i) * wm_interm_100_so_pt3(p,j,i,k) * wm_interm_22_so_pt3(a,q,j,k)
term(587) = term(587) + r1(vrdav_Rl, a,i) * wm_interm_100_so_pt3(j,p,i,k) * wm_interm_24_so_pt3(a,q,j,k)
term(588) = term(588) + r1(vrdav_Rl, a,i) * wm_interm_100_so_pt3(p,j,i,k) * wm_interm_23_so_pt3(a,q,j,k)
term(589) = term(589) + r1(vrdav_Rl, a,i) * wm_interm_100_so_pt3(p,j,i,k) * wm_interm_24_so_pt3(a,q,j,k)
term(590) = term(590) + r1(vrdav_Rl, a,i) * wm_interm_100_so_pt3(p,j,i,k) * wm_interm_26_so_pt3(a,q,j,k)
term(591) = term(591) + r1(vrdav_Rl, a,i) * wm_interm_100_so_pt3(j,p,i,k) * wm_interm_29_so_pt3(a,q,j,k)
term(592) = term(592) + r1(vrdav_Rl, a,i) * wm_interm_100_so_pt3(j,p,i,k) * wm_interm_25_so_pt3(a,q,j,k)
term(593) = term(593) + r1(vrdav_Rl, a,i) * wm_interm_100_so_pt3(p,j,i,k) * wm_interm_25_so_pt3(a,q,j,k)
term(594) = term(594) + r1(vrdav_Rl, a,i) * wm_interm_100_so_pt3(p,j,i,k) * wm_interm_45_so_pt3(a,q,j,k)
term(595) = term(595) + r1(vrdav_Rl, a,i) * wm_interm_100_so_pt3(p,j,i,k) * wm_interm_46_so_pt3(a,q,j,k)
term(596) = term(596) + r1(vrdav_Rl, a,i) * wm_interm_100_so_pt3(j,p,i,k) * wm_interm_48_so_pt3(a,q,j,k)
term(597) = term(597) + r1(vrdav_Rl, a,i) * wm_interm_100_so_pt3(p,j,i,k) * wm_interm_47_so_pt3(a,q,j,k)
term(598) = term(598) + r1(vrdav_Rl, a,i) * wm_interm_100_so_pt3(p,j,i,k) * wm_interm_48_so_pt3(a,q,j,k)
term(599) = term(599) + r1(vrdav_Rl, a,i) * wm_interm_100_so_pt3(j,p,i,k) * wm_interm_47_so_pt3(a,q,j,k)
end do 
end do 
end do 
end do 

term(585) = term(585) * (2.0d+0) 
term(586) = term(586) * (-4.0d+0) 
term(587) = term(587) * (2.0d+0) 
term(588) = term(588) * (2.0d+0) 
term(589) = term(589) * (-4.0d+0) 
term(590) = term(590) * (2.0d+0) 
term(591) = term(591) * (2.0d+0) 
term(592) = term(592) * (-4.0d+0) 
term(593) = term(593) * (2.0d+0) 
term(594) = term(594) * (8.0d+0) 
term(595) = term(595) * (-8.0d+0) 
term(596) = term(596) * (8.0d+0) 
term(597) = term(597) * (8.0d+0) 
term(598) = term(598) * (-8.0d+0) 
term(599) = term(599) * (-8.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(600) = term(600) + s2(a,q,j,i) * wm_interm_20_so_pt3(i,j,k,p) * wm_interm_97_so_pt3(a,k)
term(601) = term(601) + s2(a,q,j,i) * wm_interm_20_so_pt3(i,j,k,p) * wm_interm_95_so_pt3(a,k)
term(602) = term(602) + s2(a,q,j,i) * wm_interm_44_so_pt3(i,j,k,p) * wm_interm_97_so_pt3(a,k)
term(603) = term(603) + s2(a,q,j,i) * wm_interm_44_so_pt3(i,j,k,p) * wm_interm_95_so_pt3(a,k)
end do 
end do 
end do 
end do 

term(600) = term(600) * (-4.0d+0) 
term(601) = term(601) * (2.0d+0) 
term(602) = term(602) * (-8.0d+0) 
term(603) = term(603) * (4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(604) = term(604) + s2(a,q,j,i) * wm_interm_21_so_pt3(a,b,k,p) * wm_interm_98_so_pt3(b,k,i,j)
term(605) = term(605) + s2(a,q,j,i) * wm_interm_22_so_pt3(a,b,k,p) * wm_interm_98_so_pt3(b,k,i,j)
term(606) = term(606) + s2(a,q,j,i) * wm_interm_26_so_pt3(a,b,k,p) * wm_interm_98_so_pt3(b,k,i,j)
term(607) = term(607) + s2(a,q,j,i) * wm_interm_23_so_pt3(a,b,k,p) * wm_interm_98_so_pt3(b,k,i,j)
term(608) = term(608) + s2(a,q,j,i) * wm_interm_25_so_pt3(a,b,k,p) * wm_interm_98_so_pt3(b,k,i,j)
term(609) = term(609) + s2(a,q,j,i) * wm_interm_29_so_pt3(a,b,k,p) * wm_interm_98_so_pt3(b,k,i,j)
term(610) = term(610) + s2(a,q,j,i) * wm_interm_45_so_pt3(a,b,k,p) * wm_interm_98_so_pt3(b,k,i,j)
term(611) = term(611) + s2(a,q,j,i) * wm_interm_46_so_pt3(a,b,k,p) * wm_interm_98_so_pt3(b,k,i,j)
term(612) = term(612) + s2(a,q,j,i) * wm_interm_47_so_pt3(a,b,k,p) * wm_interm_98_so_pt3(b,k,i,j)
term(613) = term(613) + s2(a,q,j,i) * wm_interm_48_so_pt3(a,b,k,p) * wm_interm_98_so_pt3(b,k,i,j)
end do 
end do 
end do 
end do 
end do 

term(604) = term(604) * (2.0d+0) 
term(605) = term(605) * (-4.0d+0) 
term(606) = term(606) * (2.0d+0) 
term(607) = term(607) * (2.0d+0) 
term(608) = term(608) * (2.0d+0) 
term(609) = term(609) * (-4.0d+0) 
term(610) = term(610) * (8.0d+0) 
term(611) = term(611) * (-8.0d+0) 
term(612) = term(612) * (8.0d+0) 
term(613) = term(613) * (-8.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(614) = term(614) + s2(a,q,j,i) * wm_interm_20_so_pt3(j,i,p,k) * wm_interm_97_so_pt3(a,k)
term(615) = term(615) + s2(a,q,j,i) * wm_interm_20_so_pt3(i,j,p,k) * wm_interm_97_so_pt3(a,k)
term(616) = term(616) + s2(a,q,j,i) * wm_interm_20_so_pt3(j,i,p,k) * wm_interm_95_so_pt3(a,k)
term(617) = term(617) + s2(a,q,j,i) * wm_interm_20_so_pt3(i,j,p,k) * wm_interm_95_so_pt3(a,k)
term(618) = term(618) + s2(a,q,j,i) * wm_interm_44_so_pt3(j,i,p,k) * wm_interm_97_so_pt3(a,k)
term(619) = term(619) + s2(a,q,j,i) * wm_interm_44_so_pt3(i,j,p,k) * wm_interm_97_so_pt3(a,k)
term(620) = term(620) + s2(a,q,j,i) * wm_interm_44_so_pt3(j,i,p,k) * wm_interm_95_so_pt3(a,k)
term(621) = term(621) + s2(a,q,j,i) * wm_interm_44_so_pt3(i,j,p,k) * wm_interm_95_so_pt3(a,k)
end do 
end do 
end do 
end do 

term(614) = term(614) * (-4.0d+0) 
term(615) = term(615) * (8.0d+0) 
term(616) = term(616) * (2.0d+0) 
term(617) = term(617) * (-4.0d+0) 
term(618) = term(618) * (-8.0d+0) 
term(619) = term(619) * (16.0d+0) 
term(620) = term(620) * (4.0d+0) 
term(621) = term(621) * (-8.0d+0) 

do l = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(622) = term(622) + s2(a,q,p,i) * wm_interm_20_so_pt3(i,j,k,l) * wm_interm_98_so_pt3(a,j,k,l)
term(623) = term(623) + s2(a,q,j,i) * wm_interm_20_so_pt3(j,i,k,l) * wm_interm_98_so_pt3(a,p,k,l)
term(624) = term(624) + s2(a,q,j,i) * wm_interm_20_so_pt3(i,j,k,l) * wm_interm_98_so_pt3(a,p,k,l)
term(625) = term(625) + s2(a,q,p,i) * wm_interm_44_so_pt3(i,j,k,l) * wm_interm_98_so_pt3(a,j,k,l)
term(626) = term(626) + s2(a,q,j,i) * wm_interm_44_so_pt3(j,i,k,l) * wm_interm_98_so_pt3(a,p,k,l)
term(627) = term(627) + s2(a,q,j,i) * wm_interm_44_so_pt3(i,j,k,l) * wm_interm_98_so_pt3(a,p,k,l)
end do 
end do 
end do 
end do 
end do 

term(622) = term(622) * (2.0d+0) 
term(623) = term(623) * (2.0d+0) 
term(624) = term(624) * (-4.0d+0) 
term(625) = term(625) * (4.0d+0) 
term(626) = term(626) * (4.0d+0) 
term(627) = term(627) * (-8.0d+0) 

do j = 1, nocc 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(628) = term(628) + r1(vrdav_Rl, a,i) * wm_interm_101_so_pt3(q,a,p,j) * wm_interm_17_so_pt3(i,j)
term(629) = term(629) + r1(vrdav_Rl, a,i) * wm_interm_101_so_pt3(q,a,p,j) * wm_interm_18_so_pt3(i,j)
term(630) = term(630) + r1(vrdav_Rl, a,i) * wm_interm_101_so_pt3(q,a,p,j) * wm_interm_19_so_pt3(i,j)
term(631) = term(631) + r1(vrdav_Rl, a,i) * wm_interm_101_so_pt3(q,a,p,j) * wm_interm_41_so_pt3(i,j)
term(632) = term(632) + r1(vrdav_Rl, a,i) * wm_interm_101_so_pt3(q,a,p,j) * wm_interm_42_so_pt3(i,j)
term(633) = term(633) + r1(vrdav_Rl, a,i) * wm_interm_101_so_pt3(q,a,p,j) * wm_interm_43_so_pt3(i,j)
end do 
end do 
end do 

term(628) = term(628) * (2.0d+0) 
term(629) = term(629) * (-4.0d+0) 
term(630) = term(630) * (2.0d+0) 
term(631) = term(631) * (4.0d+0) 
term(632) = term(632) * (-8.0d+0) 
term(633) = term(633) * (4.0d+0) 

do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(634) = term(634) + r1(vrdav_Rl, a,p) * wm_interm_107_so_pt3(b,a) * wm_interm_9_so_pt3(b,q)
term(635) = term(635) + r1(vrdav_Rl, a,p) * wm_interm_108_so_pt3(b,a) * wm_interm_9_so_pt3(b,q)
term(636) = term(636) + r1(vrdav_Rl, a,p) * wm_interm_107_so_pt3(b,a) * wm_interm_5_so_pt3(b,q)
term(637) = term(637) + r1(vrdav_Rl, a,p) * wm_interm_108_so_pt3(b,a) * wm_interm_5_so_pt3(b,q)
term(638) = term(638) + r1(vrdav_Rl, a,p) * wm_interm_107_so_pt3(b,a) * wm_interm_6_so_pt3(b,q)
term(639) = term(639) + r1(vrdav_Rl, a,p) * wm_interm_108_so_pt3(b,a) * wm_interm_6_so_pt3(b,q)
term(640) = term(640) + r1(vrdav_Rl, a,p) * wm_interm_104_so_pt3(b,q) * wm_interm_107_so_pt3(b,a)
term(641) = term(641) + r1(vrdav_Rl, a,p) * wm_interm_104_so_pt3(b,q) * wm_interm_108_so_pt3(b,a)
term(642) = term(642) + r1(vrdav_Rl, a,p) * wm_interm_107_so_pt3(b,a) * wm_interm_34_so_pt3(b,q)
term(643) = term(643) + r1(vrdav_Rl, a,p) * wm_interm_108_so_pt3(b,a) * wm_interm_34_so_pt3(b,q)
term(644) = term(644) + r1(vrdav_Rl, a,p) * wm_interm_107_so_pt3(b,a) * wm_interm_35_so_pt3(b,q)
term(645) = term(645) + r1(vrdav_Rl, a,p) * wm_interm_108_so_pt3(b,a) * wm_interm_35_so_pt3(b,q)
end do 
end do 

term(634) = term(634) * (-1.0d+0) 
term(635) = term(635) * (2.0d+0) 
term(636) = term(636) * (-1.0d+0) 
term(637) = term(637) * (2.0d+0) 
term(638) = term(638) * (2.0d+0) 
term(639) = term(639) * (-4.0d+0) 
term(640) = term(640) * (-2.0d+0) 
term(641) = term(641) * (4.0d+0) 
term(642) = term(642) * (-2.0d+0) 
term(643) = term(643) * (4.0d+0) 
term(644) = term(644) * (4.0d+0) 
term(645) = term(645) * (-8.0d+0) 


    calc_D_ov_wm_so_pt3 = zero
    do s = 0, 645
    calc_D_ov_wm_so_pt3 = calc_D_ov_wm_so_pt3 + term(s)
    end do

    end function calc_D_ov_wm_so_pt3
    
    function calc_D_vo_wm_so_pt3(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vo_wm_so_pt3
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
    real(F64), dimension(0:454) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + wm_interm_12_so_pt3(a,p,i,j) * wm_interm_69_so_pt3(a,j,q,i)
term(1) = term(1) + wm_interm_14_so_pt3(a,p,i,j) * wm_interm_69_so_pt3(a,j,q,i)
term(2) = term(2) + wm_interm_4_so_pt3(a,p,i,j) * wm_interm_69_so_pt3(a,j,q,i)
term(3) = term(3) + wm_interm_3_so_pt3(a,p,i,j) * wm_interm_69_so_pt3(a,j,q,i)
term(4) = term(4) + wm_interm_2_so_pt3(a,p,i,j) * wm_interm_69_so_pt3(a,j,q,i)
term(5) = term(5) + wm_interm_10_so_pt3(a,p,i,j) * wm_interm_69_so_pt3(a,j,q,i)
term(6) = term(6) + wm_interm_3_so_pt3(a,p,i,j) * wm_interm_69_so_pt3(a,j,i,q)
term(7) = term(7) + wm_interm_4_so_pt3(a,p,i,j) * wm_interm_69_so_pt3(a,j,i,q)
term(8) = term(8) + wm_interm_12_so_pt3(a,p,i,j) * wm_interm_69_so_pt3(a,j,i,q)
term(9) = term(9) + wm_interm_14_so_pt3(a,p,i,j) * wm_interm_69_so_pt3(a,j,i,q)
term(10) = term(10) + wm_interm_0_so_pt3(a,p,i,j) * wm_interm_69_so_pt3(a,j,i,q)
term(11) = term(11) + wm_interm_10_so_pt3(a,p,i,j) * wm_interm_69_so_pt3(a,j,i,q)
term(12) = term(12) + wm_interm_58_so_pt3(a,i,q,j) * wm_interm_72_so_pt3(a,p,i,j)
term(13) = term(13) + wm_interm_58_so_pt3(a,q,i,j) * wm_interm_72_so_pt3(a,p,i,j)
term(14) = term(14) + wm_interm_58_so_pt3(a,q,i,j) * wm_interm_66_so_pt3(a,p,i,j)
term(15) = term(15) + wm_interm_56_so_pt3(a,i,q,j) * wm_interm_66_so_pt3(a,p,i,j)
term(16) = term(16) + wm_interm_56_so_pt3(a,q,i,j) * wm_interm_66_so_pt3(a,p,i,j)
term(17) = term(17) + wm_interm_56_so_pt3(a,q,i,j) * wm_interm_72_so_pt3(a,p,i,j)
term(18) = term(18) + wm_interm_58_so_pt3(a,i,q,j) * wm_interm_73_so_pt3(a,p,i,j)
term(19) = term(19) + wm_interm_58_so_pt3(a,q,i,j) * wm_interm_73_so_pt3(a,p,i,j)
term(20) = term(20) + wm_interm_58_so_pt3(a,i,q,j) * wm_interm_74_so_pt3(a,p,i,j)
term(21) = term(21) + wm_interm_58_so_pt3(a,q,i,j) * wm_interm_74_so_pt3(a,p,i,j)
term(22) = term(22) + wm_interm_56_so_pt3(a,q,i,j) * wm_interm_73_so_pt3(a,p,i,j)
term(23) = term(23) + wm_interm_56_so_pt3(a,q,i,j) * wm_interm_74_so_pt3(a,p,i,j)
term(24) = term(24) + wm_interm_37_so_pt3(a,p,i,j) * wm_interm_69_so_pt3(a,j,q,i)
term(25) = term(25) + wm_interm_39_so_pt3(a,p,i,j) * wm_interm_69_so_pt3(a,j,q,i)
term(26) = term(26) + wm_interm_33_so_pt3(a,p,i,j) * wm_interm_69_so_pt3(a,j,q,i)
term(27) = term(27) + wm_interm_32_so_pt3(a,p,i,j) * wm_interm_69_so_pt3(a,j,q,i)
term(28) = term(28) + wm_interm_32_so_pt3(a,p,i,j) * wm_interm_69_so_pt3(a,j,i,q)
term(29) = term(29) + wm_interm_33_so_pt3(a,p,i,j) * wm_interm_69_so_pt3(a,j,i,q)
term(30) = term(30) + wm_interm_37_so_pt3(a,p,i,j) * wm_interm_69_so_pt3(a,j,i,q)
term(31) = term(31) + wm_interm_39_so_pt3(a,p,i,j) * wm_interm_69_so_pt3(a,j,i,q)
term(32) = term(32) + wm_interm_59_so_pt3(a,q,i,j) * wm_interm_72_so_pt3(a,p,i,j)
term(33) = term(33) + wm_interm_59_so_pt3(a,i,q,j) * wm_interm_72_so_pt3(a,p,i,j)
term(34) = term(34) + wm_interm_59_so_pt3(a,i,q,j) * wm_interm_66_so_pt3(a,p,i,j)
term(35) = term(35) + wm_interm_59_so_pt3(a,q,i,j) * wm_interm_66_so_pt3(a,p,i,j)
term(36) = term(36) + wm_interm_59_so_pt3(a,q,i,j) * wm_interm_73_so_pt3(a,p,i,j)
term(37) = term(37) + wm_interm_59_so_pt3(a,i,q,j) * wm_interm_73_so_pt3(a,p,i,j)
term(38) = term(38) + wm_interm_59_so_pt3(a,q,i,j) * wm_interm_74_so_pt3(a,p,i,j)
term(39) = term(39) + wm_interm_59_so_pt3(a,i,q,j) * wm_interm_74_so_pt3(a,p,i,j)
term(40) = term(40) + r1(vrdav_Rr, a,q) * wm_interm_102_so_pt3(a,p,i,j) * wm_interm_55_so_pt3(j,i)
term(41) = term(41) + r1(vrdav_Rr, a,q) * wm_interm_101_so_pt3(a,p,i,j) * wm_interm_55_so_pt3(j,i)
term(42) = term(42) + r1(vrdav_Rr, a,q) * wm_interm_103_so_pt3(a,p,i,j) * wm_interm_55_so_pt3(j,i)
term(43) = term(43) + r1(vrdav_Rr, a,q) * wm_interm_102_so_pt3(a,p,i,j) * wm_interm_57_so_pt3(j,i)
term(44) = term(44) + r1(vrdav_Rr, a,q) * wm_interm_101_so_pt3(a,p,i,j) * wm_interm_57_so_pt3(j,i)
term(45) = term(45) + r1(vrdav_Rr, a,q) * wm_interm_103_so_pt3(a,p,i,j) * wm_interm_57_so_pt3(j,i)
term(46) = term(46) + r1(vrdav_Rr, a,i) * wm_interm_105_so_pt3(i,j) * wm_interm_66_so_pt3(a,p,j,q)
term(47) = term(47) + r1(vrdav_Rr, a,i) * wm_interm_106_so_pt3(i,j) * wm_interm_66_so_pt3(a,p,j,q)
term(48) = term(48) + r1(vrdav_Rr, a,i) * wm_interm_101_so_pt3(a,p,j,q) * wm_interm_55_so_pt3(i,j)
term(49) = term(49) + r1(vrdav_Rr, a,i) * wm_interm_101_so_pt3(a,p,j,q) * wm_interm_57_so_pt3(i,j)
end do 
end do 
end do 

term(0) = term(0) * (-1.0d+0) 
term(1) = term(1) * (2.0d+0) 
term(2) = term(2) * (-1.0d+0) 
term(3) = term(3) * (2.0d+0) 
term(4) = term(4) * (-1.0d+0) 
term(5) = term(5) * (-1.0d+0) 
term(6) = term(6) * (-1.0d+0) 
term(7) = term(7) * (2.0d+0) 
term(8) = term(8) * (2.0d+0) 
term(9) = term(9) * (-4.0d+0) 
term(10) = term(10) * (-1.0d+0) 
term(11) = term(11) * (2.0d+0) 
term(12) = term(12) * (-1.0d+0) 
term(13) = term(13) * (2.0d+0) 
term(14) = term(14) * (-1.0d+0) 
term(15) = term(15) * (-1.0d+0) 
term(16) = term(16) * (2.0d+0) 
term(17) = term(17) * (-1.0d+0) 
term(18) = term(18) * (-1.0d+0) 
term(19) = term(19) * (2.0d+0) 
term(20) = term(20) * (2.0d+0) 
term(21) = term(21) * (-4.0d+0) 
term(22) = term(22) * (-1.0d+0) 
term(23) = term(23) * (2.0d+0) 
term(24) = term(24) * (-4.0d+0) 
term(25) = term(25) * (4.0d+0) 
term(26) = term(26) * (-4.0d+0) 
term(27) = term(27) * (4.0d+0) 
term(28) = term(28) * (-4.0d+0) 
term(29) = term(29) * (4.0d+0) 
term(30) = term(30) * (8.0d+0) 
term(31) = term(31) * (-8.0d+0) 
term(32) = term(32) * (-4.0d+0) 
term(33) = term(33) * (4.0d+0) 
term(34) = term(34) * (-4.0d+0) 
term(35) = term(35) * (4.0d+0) 
term(36) = term(36) * (-4.0d+0) 
term(37) = term(37) * (4.0d+0) 
term(38) = term(38) * (8.0d+0) 
term(39) = term(39) * (-8.0d+0) 
term(40) = term(40) * (-2.0d+0) 
term(41) = term(41) * (4.0d+0) 
term(42) = term(42) * (4.0d+0) 
term(43) = term(43) * (4.0d+0) 
term(44) = term(44) * (-8.0d+0) 
term(45) = term(45) * (-8.0d+0) 
term(46) = term(46) * (-2.0d+0) 
term(47) = term(47) * (4.0d+0) 
term(48) = term(48) * (-2.0d+0) 
term(49) = term(49) * (4.0d+0) 

do a = nocc + 1, nactive 
term(50) = term(50) + wm_interm_54_so_pt3(a,p) * wm_interm_63_so_pt3(a,q)
term(51) = term(51) + wm_interm_53_so_pt3(a,p) * wm_interm_63_so_pt3(a,q)
term(52) = term(52) + wm_interm_54_so_pt3(a,p) * wm_interm_64_so_pt3(a,q)
term(53) = term(53) + wm_interm_53_so_pt3(a,p) * wm_interm_64_so_pt3(a,q)
term(54) = term(54) + wm_interm_54_so_pt3(a,p) * wm_interm_65_so_pt3(a,q)
term(55) = term(55) + wm_interm_53_so_pt3(a,p) * wm_interm_65_so_pt3(a,q)
term(56) = term(56) + wm_interm_54_so_pt3(a,p) * wm_interm_67_so_pt3(a,q)
term(57) = term(57) + wm_interm_53_so_pt3(a,p) * wm_interm_67_so_pt3(a,q)
term(58) = term(58) + wm_interm_54_so_pt3(a,p) * wm_interm_68_so_pt3(a,q)
term(59) = term(59) + wm_interm_53_so_pt3(a,p) * wm_interm_68_so_pt3(a,q)
term(60) = term(60) + wm_interm_28_so_pt3(a,p) * wm_interm_51_so_pt3(a,q)
term(61) = term(61) + wm_interm_30_so_pt3(a,p) * wm_interm_51_so_pt3(a,q)
term(62) = term(62) + wm_interm_27_so_pt3(a,p) * wm_interm_51_so_pt3(a,q)
term(63) = term(63) + wm_interm_28_so_pt3(a,p) * wm_interm_52_so_pt3(a,q)
term(64) = term(64) + wm_interm_30_so_pt3(a,p) * wm_interm_52_so_pt3(a,q)
term(65) = term(65) + wm_interm_27_so_pt3(a,p) * wm_interm_52_so_pt3(a,q)
term(66) = term(66) + wm_interm_49_so_pt3(a,p) * wm_interm_51_so_pt3(a,q)
term(67) = term(67) + wm_interm_50_so_pt3(a,p) * wm_interm_51_so_pt3(a,q)
term(68) = term(68) + wm_interm_49_so_pt3(a,p) * wm_interm_52_so_pt3(a,q)
term(69) = term(69) + wm_interm_50_so_pt3(a,p) * wm_interm_52_so_pt3(a,q)
term(70) = term(70) + wm_interm_89_so_pt3(p,a) * wm_interm_95_so_pt3(a,q)
term(71) = term(71) + wm_interm_51_so_pt3(a,q) * wm_interm_96_so_pt3(a,p)
term(72) = term(72) + wm_interm_52_so_pt3(a,q) * wm_interm_96_so_pt3(a,p)
term(73) = term(73) + wm_interm_89_so_pt3(p,a) * wm_interm_97_so_pt3(a,q)
end do 

term(50) = term(50) * (-2.0d+0) 
term(51) = term(51) * (4.0d+0) 
term(53) = term(53) * (-2.0d+0) 
term(55) = term(55) * (-2.0d+0) 
term(56) = term(56) * (-4.0d+0) 
term(57) = term(57) * (8.0d+0) 
term(58) = term(58) * (4.0d+0) 
term(59) = term(59) * (-8.0d+0) 
term(60) = term(60) * (2.0d+0) 
term(61) = term(61) * (-4.0d+0) 
term(62) = term(62) * (2.0d+0) 
term(63) = term(63) * (-1.0d+0) 
term(64) = term(64) * (2.0d+0) 
term(65) = term(65) * (-1.0d+0) 
term(66) = term(66) * (8.0d+0) 
term(67) = term(67) * (-8.0d+0) 
term(68) = term(68) * (-4.0d+0) 
term(69) = term(69) * (4.0d+0) 
term(70) = term(70) * (2.0d+0) 
term(71) = term(71) * (-4.0d+0) 
term(72) = term(72) * (2.0d+0) 
term(73) = term(73) * (-4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(74) = term(74) + wm_interm_56_so_pt3(p,i,j,k) * wm_interm_70_so_pt3(j,i,q,k)
term(75) = term(75) + wm_interm_56_so_pt3(p,i,j,k) * wm_interm_70_so_pt3(i,j,q,k)
term(76) = term(76) + wm_interm_56_so_pt3(p,i,j,k) * wm_interm_70_so_pt3(i,j,k,q)
term(77) = term(77) + wm_interm_56_so_pt3(p,i,j,k) * wm_interm_70_so_pt3(j,i,k,q)
term(78) = term(78) + wm_interm_58_so_pt3(p,i,j,k) * wm_interm_70_so_pt3(j,i,k,q)
term(79) = term(79) + wm_interm_58_so_pt3(p,i,j,k) * wm_interm_70_so_pt3(i,j,q,k)
term(80) = term(80) + wm_interm_71_so_pt3(p,i,j,k) * wm_interm_8_so_pt3(j,k,i,q)
term(81) = term(81) + wm_interm_71_so_pt3(p,i,j,k) * wm_interm_7_so_pt3(k,j,i,q)
term(82) = term(82) + wm_interm_59_so_pt3(p,i,j,k) * wm_interm_70_so_pt3(j,i,q,k)
term(83) = term(83) + wm_interm_59_so_pt3(p,i,j,k) * wm_interm_70_so_pt3(i,j,q,k)
term(84) = term(84) + wm_interm_59_so_pt3(p,i,j,k) * wm_interm_70_so_pt3(i,j,k,q)
term(85) = term(85) + wm_interm_59_so_pt3(p,i,j,k) * wm_interm_70_so_pt3(j,i,k,q)
term(86) = term(86) + wm_interm_60_so_pt3(p,i,j,k) * wm_interm_70_so_pt3(j,i,k,q)
term(87) = term(87) + wm_interm_60_so_pt3(p,i,j,k) * wm_interm_70_so_pt3(i,j,q,k)
term(88) = term(88) + wm_interm_36_so_pt3(i,j,q,k) * wm_interm_71_so_pt3(p,k,i,j)
term(89) = term(89) + r1(vrdav_Rr, p,i) * wm_interm_100_so_pt3(j,i,k,q) * wm_interm_55_so_pt3(k,j)
term(90) = term(90) + r1(vrdav_Rr, p,i) * wm_interm_100_so_pt3(i,j,k,q) * wm_interm_55_so_pt3(k,j)
term(91) = term(91) + r1(vrdav_Rr, p,i) * wm_interm_100_so_pt3(j,i,k,q) * wm_interm_57_so_pt3(k,j)
term(92) = term(92) + r1(vrdav_Rr, p,i) * wm_interm_100_so_pt3(i,j,k,q) * wm_interm_57_so_pt3(k,j)
end do 
end do 
end do 

term(74) = term(74) * (-0.5d+0) 
term(76) = term(76) * (-0.5d+0) 
term(78) = term(78) * (-0.5d+0) 
term(79) = term(79) * (-0.5d+0) 
term(80) = term(80) * (-0.5d+0) 
term(81) = term(81) * (-0.5d+0) 
term(82) = term(82) * (-1.0d+0) 
term(83) = term(83) * (2.0d+0) 
term(84) = term(84) * (-1.0d+0) 
term(85) = term(85) * (2.0d+0) 
term(86) = term(86) * (-1.0d+0) 
term(87) = term(87) * (-1.0d+0) 
term(88) = term(88) * (-2.0d+0) 
term(89) = term(89) * (4.0d+0) 
term(90) = term(90) * (-2.0d+0) 
term(91) = term(91) * (-8.0d+0) 
term(92) = term(92) * (4.0d+0) 

do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(93) = term(93) + t2(a,b,i,j) * wm_interm_66_so_pt3(b,p,i,q) * wm_interm_92_so_pt3(a,j)
end do 
end do 
end do 
end do 

term(93) = term(93) * (-1.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(94) = term(94) + r1(vrdav_Rr, a,i) * wm_interm_102_so_pt3(a,p,j,k) * wm_interm_70_so_pt3(i,k,q,j)
term(95) = term(95) + r1(vrdav_Rr, a,i) * wm_interm_102_so_pt3(a,p,j,k) * wm_interm_70_so_pt3(k,i,j,q)
term(96) = term(96) + r1(vrdav_Rr, a,i) * wm_interm_70_so_pt3(i,j,q,k) * wm_interm_99_so_pt3(a,p,k,j)
term(97) = term(97) + r1(vrdav_Rr, a,i) * wm_interm_103_so_pt3(a,p,j,k) * wm_interm_70_so_pt3(i,k,q,j)
term(98) = term(98) + r1(vrdav_Rr, a,i) * wm_interm_103_so_pt3(a,p,j,k) * wm_interm_70_so_pt3(k,i,j,q)
term(99) = term(99) + r1(vrdav_Rr, a,i) * wm_interm_101_so_pt3(a,p,j,k) * wm_interm_70_so_pt3(k,i,j,q)
term(100) = term(100) + r1(vrdav_Rr, a,i) * wm_interm_101_so_pt3(a,p,j,k) * wm_interm_70_so_pt3(k,i,q,j)
term(101) = term(101) + r1(vrdav_Rr, a,i) * wm_interm_101_so_pt3(a,p,j,k) * wm_interm_70_so_pt3(i,k,j,q)
term(102) = term(102) + r1(vrdav_Rr, a,i) * wm_interm_101_so_pt3(a,p,j,k) * wm_interm_70_so_pt3(i,k,q,j)
end do 
end do 
end do 
end do 

term(94) = term(94) * (-2.0d+0) 
term(95) = term(95) * (-2.0d+0) 
term(96) = term(96) * (-2.0d+0) 
term(97) = term(97) * (4.0d+0) 
term(98) = term(98) * (4.0d+0) 
term(99) = term(99) * (4.0d+0) 
term(100) = term(100) * (-2.0d+0) 
term(101) = term(101) * (-2.0d+0) 
term(102) = term(102) * (4.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(103) = term(103) + r1(vrdav_Rr, p,i) * wm_interm_100_so_pt3(i,j,k,l) * wm_interm_70_so_pt3(k,l,j,q)
term(104) = term(104) + r1(vrdav_Rr, p,i) * wm_interm_100_so_pt3(j,i,k,l) * wm_interm_70_so_pt3(k,l,j,q)
end do 
end do 
end do 
end do 

term(103) = term(103) * (-1.0d+0) 
term(104) = term(104) * (2.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(105) = term(105) + t2(a,p,j,i) * wm_interm_66_so_pt3(a,b,k,q) * wm_interm_94_so_pt3(b,k,j,i)
end do 
end do 
end do 
end do 
end do 

term(105) = term(105) * (-4.0d+0) 

do l = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
term(106) = term(106) + r1(vrdav_Rr, p,i) * wm_interm_100_so_pt3(j,i,k,l) * wm_interm_70_so_pt3(k,l,q,j)
term(107) = term(107) + r1(vrdav_Rr, p,i) * wm_interm_100_so_pt3(i,j,k,l) * wm_interm_70_so_pt3(k,l,q,j)
end do 
end do 
end do 
end do 

term(106) = term(106) * (-1.0d+0) 
term(107) = term(107) * (2.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(108) = term(108) + t2(a,p,j,i) * wm_interm_107_so_pt3(a,b) * wm_interm_86_so_pt3(b,i,j,q)
term(109) = term(109) + t2(a,p,j,i) * wm_interm_108_so_pt3(a,b) * wm_interm_86_so_pt3(b,i,j,q)
term(110) = term(110) + t2(a,p,j,i) * wm_interm_107_so_pt3(a,b) * wm_interm_88_so_pt3(b,i,j,q)
term(111) = term(111) + t2(a,p,j,i) * wm_interm_108_so_pt3(a,b) * wm_interm_88_so_pt3(b,i,j,q)
term(112) = term(112) + t2(a,p,j,i) * wm_interm_53_so_pt3(a,b) * wm_interm_94_so_pt3(b,q,j,i)
term(113) = term(113) + t2(a,p,j,i) * wm_interm_54_so_pt3(a,b) * wm_interm_94_so_pt3(b,q,j,i)
term(114) = term(114) + t2(a,p,j,i) * wm_interm_102_so_pt3(a,b,j,q) * wm_interm_91_so_pt3(b,i)
term(115) = term(115) + t2(a,p,j,i) * wm_interm_101_so_pt3(a,b,j,q) * wm_interm_91_so_pt3(b,i)
term(116) = term(116) + t2(a,p,j,i) * wm_interm_103_so_pt3(a,b,j,q) * wm_interm_91_so_pt3(b,i)
term(117) = term(117) + t2(a,p,j,i) * wm_interm_91_so_pt3(b,i) * wm_interm_99_so_pt3(a,b,j,q)
term(118) = term(118) + t2(a,p,j,i) * wm_interm_66_so_pt3(a,b,j,q) * wm_interm_92_so_pt3(b,i)
term(119) = term(119) + t2(a,p,j,i) * wm_interm_72_so_pt3(a,b,j,q) * wm_interm_92_so_pt3(b,i)
term(120) = term(120) + t2(a,p,j,i) * wm_interm_73_so_pt3(a,b,j,q) * wm_interm_92_so_pt3(b,i)
term(121) = term(121) + t2(a,p,j,i) * wm_interm_74_so_pt3(a,b,j,q) * wm_interm_92_so_pt3(b,i)
end do 
end do 
end do 
end do 

term(108) = term(108) * (2.0d+0) 
term(109) = term(109) * (-4.0d+0) 
term(110) = term(110) * (-1.0d+0) 
term(111) = term(111) * (2.0d+0) 
term(112) = term(112) * (4.0d+0) 
term(113) = term(113) * (-2.0d+0) 
term(114) = term(114) * (8.0d+0) 
term(115) = term(115) * (-4.0d+0) 
term(116) = term(116) * (-16.0d+0) 
term(117) = term(117) * (8.0d+0) 
term(118) = term(118) * (-2.0d+0) 
term(119) = term(119) * (4.0d+0) 
term(120) = term(120) * (4.0d+0) 
term(121) = term(121) * (-8.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(122) = term(122) + t2(a,p,q,i) * wm_interm_102_so_pt3(a,b,j,k) * wm_interm_86_so_pt3(b,i,k,j)
term(123) = term(123) + t2(a,p,q,i) * wm_interm_103_so_pt3(a,b,j,k) * wm_interm_86_so_pt3(b,i,k,j)
term(124) = term(124) + t2(a,p,q,i) * wm_interm_102_so_pt3(a,b,j,k) * wm_interm_86_so_pt3(b,k,i,j)
term(125) = term(125) + t2(a,p,q,i) * wm_interm_103_so_pt3(a,b,j,k) * wm_interm_86_so_pt3(b,k,i,j)
term(126) = term(126) + t2(a,p,q,i) * wm_interm_102_so_pt3(a,b,j,k) * wm_interm_88_so_pt3(b,i,k,j)
term(127) = term(127) + t2(a,p,q,i) * wm_interm_103_so_pt3(a,b,j,k) * wm_interm_88_so_pt3(b,i,k,j)
term(128) = term(128) + t2(a,p,q,i) * wm_interm_102_so_pt3(a,b,j,k) * wm_interm_88_so_pt3(b,k,i,j)
term(129) = term(129) + t2(a,p,q,i) * wm_interm_103_so_pt3(a,b,j,k) * wm_interm_88_so_pt3(b,k,i,j)
term(130) = term(130) + t2(a,p,q,i) * wm_interm_88_so_pt3(b,i,j,k) * wm_interm_99_so_pt3(a,b,k,j)
term(131) = term(131) + t2(a,p,q,i) * wm_interm_101_so_pt3(a,b,j,k) * wm_interm_86_so_pt3(b,i,k,j)
term(132) = term(132) + t2(a,p,q,i) * wm_interm_86_so_pt3(b,i,j,k) * wm_interm_99_so_pt3(a,b,k,j)
term(133) = term(133) + t2(a,p,q,i) * wm_interm_86_so_pt3(b,j,i,k) * wm_interm_99_so_pt3(a,b,k,j)
term(134) = term(134) + t2(a,p,q,i) * wm_interm_101_so_pt3(a,b,j,k) * wm_interm_88_so_pt3(b,k,i,j)
term(135) = term(135) + t2(a,p,q,i) * wm_interm_88_so_pt3(b,j,i,k) * wm_interm_99_so_pt3(a,b,k,j)
end do 
end do 
end do 
end do 
end do 

term(122) = term(122) * (2.0d+0) 
term(123) = term(123) * (-4.0d+0) 
term(124) = term(124) * (-1.0d+0) 
term(125) = term(125) * (2.0d+0) 
term(126) = term(126) * (-1.0d+0) 
term(127) = term(127) * (2.0d+0) 
term(128) = term(128) * (2.0d+0) 
term(129) = term(129) * (-4.0d+0) 
term(130) = term(130) * (-1.0d+0) 
term(131) = term(131) * (-1.0d+0) 
term(132) = term(132) * (2.0d+0) 
term(133) = term(133) * (-1.0d+0) 
term(134) = term(134) * (-1.0d+0) 
term(135) = term(135) * (2.0d+0) 

do k = 1, nocc 
do l = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(136) = term(136) + r1(vrdav_Rr, p,i) * wm_interm_100_so_pt3(j,k,l,q) * wm_interm_70_so_pt3(i,l,j,k)
term(137) = term(137) + r1(vrdav_Rr, p,i) * wm_interm_100_so_pt3(j,k,l,q) * wm_interm_70_so_pt3(l,i,j,k)
end do 
end do 
end do 
end do 

term(136) = term(136) * (-1.0d+0) 
term(137) = term(137) * (2.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(138) = term(138) + wm_interm_71_so_pt3(p,i,j,k) * wm_interm_7_so_pt3(j,k,q,i)
term(139) = term(139) + wm_interm_71_so_pt3(p,i,j,k) * wm_interm_8_so_pt3(j,k,q,i)
term(140) = term(140) + wm_interm_71_so_pt3(p,i,j,k) * wm_interm_7_so_pt3(k,j,q,i)
term(141) = term(141) + wm_interm_71_so_pt3(p,i,j,k) * wm_interm_8_so_pt3(k,j,q,i)
term(142) = term(142) + wm_interm_36_so_pt3(i,j,q,k) * wm_interm_71_so_pt3(p,k,j,i)
end do 
end do 
end do 

term(138) = term(138) * (-0.5d+0) 
term(141) = term(141) * (-0.5d+0) 
term(142) = term(142) * (2.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
term(143) = term(143) + wm_interm_13_so_pt3(i,j) * wm_interm_71_so_pt3(p,j,q,i)
term(144) = term(144) + wm_interm_15_so_pt3(i,j) * wm_interm_71_so_pt3(p,j,q,i)
term(145) = term(145) + wm_interm_11_so_pt3(i,j) * wm_interm_71_so_pt3(p,j,q,i)
term(146) = term(146) + wm_interm_38_so_pt3(i,j) * wm_interm_71_so_pt3(p,j,q,i)
term(147) = term(147) + wm_interm_40_so_pt3(i,j) * wm_interm_71_so_pt3(p,j,q,i)
end do 
end do 

term(143) = term(143) * (-1.0d+0) 
term(144) = term(144) * (2.0d+0) 
term(145) = term(145) * (-1.0d+0) 
term(146) = term(146) * (-4.0d+0) 
term(147) = term(147) * (4.0d+0) 

do j = 1, nocc 
do l = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
term(148) = term(148) + r1(vrdav_Rr, p,i) * wm_interm_100_so_pt3(j,k,l,q) * wm_interm_70_so_pt3(l,i,k,j)
term(149) = term(149) + r1(vrdav_Rr, p,i) * wm_interm_100_so_pt3(j,k,l,q) * wm_interm_70_so_pt3(i,l,k,j)
end do 
end do 
end do 
end do 

term(148) = term(148) * (-1.0d+0) 
term(149) = term(149) * (2.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(150) = term(150) + t2(a,p,j,i) * wm_interm_70_so_pt3(j,i,q,k) * wm_interm_92_so_pt3(a,k)
end do 
end do 
end do 
end do 

term(150) = term(150) * (-2.0d+0) 

do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(151) = term(151) + r1(vrdav_Rr, a,i) * wm_interm_70_so_pt3(j,i,k,q) * wm_interm_99_so_pt3(a,p,k,j)
term(152) = term(152) + r1(vrdav_Rr, a,i) * wm_interm_100_so_pt3(j,i,k,q) * wm_interm_72_so_pt3(a,p,k,j)
term(153) = term(153) + r1(vrdav_Rr, a,i) * wm_interm_100_so_pt3(j,i,k,q) * wm_interm_66_so_pt3(a,p,k,j)
term(154) = term(154) + r1(vrdav_Rr, a,i) * wm_interm_100_so_pt3(i,j,k,q) * wm_interm_66_so_pt3(a,p,k,j)
term(155) = term(155) + r1(vrdav_Rr, a,i) * wm_interm_100_so_pt3(j,i,k,q) * wm_interm_73_so_pt3(a,p,k,j)
term(156) = term(156) + r1(vrdav_Rr, a,i) * wm_interm_100_so_pt3(j,i,k,q) * wm_interm_74_so_pt3(a,p,k,j)
end do 
end do 
end do 
end do 

term(151) = term(151) * (-2.0d+0) 
term(152) = term(152) * (-4.0d+0) 
term(153) = term(153) * (8.0d+0) 
term(154) = term(154) * (-4.0d+0) 
term(155) = term(155) * (-4.0d+0) 
term(156) = term(156) * (8.0d+0) 

do l = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(157) = term(157) + t2(a,p,q,i) * wm_interm_100_so_pt3(j,i,k,l) * wm_interm_88_so_pt3(a,k,l,j)
term(158) = term(158) + t2(a,p,q,i) * wm_interm_100_so_pt3(i,j,k,l) * wm_interm_86_so_pt3(a,k,l,j)
end do 
end do 
end do 
end do 
end do 

term(157) = term(157) * (-1.0d+0) 
term(158) = term(158) * (-1.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(159) = term(159) + t2(a,p,j,i) * wm_interm_70_so_pt3(i,j,k,q) * wm_interm_92_so_pt3(a,k)
term(160) = term(160) + t2(a,p,j,i) * wm_interm_100_so_pt3(i,j,k,q) * wm_interm_91_so_pt3(a,k)
end do 
end do 
end do 
end do 

term(159) = term(159) * (-2.0d+0) 
term(160) = term(160) * (-4.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(161) = term(161) + t2(a,p,j,i) * wm_interm_107_so_pt3(a,b) * wm_interm_86_so_pt3(b,j,i,q)
term(162) = term(162) + t2(a,p,j,i) * wm_interm_108_so_pt3(a,b) * wm_interm_86_so_pt3(b,j,i,q)
term(163) = term(163) + t2(a,p,j,i) * wm_interm_107_so_pt3(a,b) * wm_interm_88_so_pt3(b,j,i,q)
term(164) = term(164) + t2(a,p,j,i) * wm_interm_108_so_pt3(a,b) * wm_interm_88_so_pt3(b,j,i,q)
term(165) = term(165) + t2(a,p,j,i) * wm_interm_102_so_pt3(a,b,i,q) * wm_interm_91_so_pt3(b,j)
term(166) = term(166) + t2(a,p,j,i) * wm_interm_103_so_pt3(a,b,i,q) * wm_interm_91_so_pt3(b,j)
term(167) = term(167) + t2(a,p,j,i) * wm_interm_91_so_pt3(b,j) * wm_interm_99_so_pt3(a,b,i,q)
term(168) = term(168) + t2(a,p,j,i) * wm_interm_72_so_pt3(a,b,i,q) * wm_interm_92_so_pt3(b,j)
term(169) = term(169) + t2(a,p,j,i) * wm_interm_73_so_pt3(a,b,i,q) * wm_interm_92_so_pt3(b,j)
term(170) = term(170) + t2(a,p,j,i) * wm_interm_74_so_pt3(a,b,i,q) * wm_interm_92_so_pt3(b,j)
end do 
end do 
end do 
end do 

term(161) = term(161) * (-1.0d+0) 
term(162) = term(162) * (2.0d+0) 
term(163) = term(163) * (2.0d+0) 
term(164) = term(164) * (-4.0d+0) 
term(165) = term(165) * (-4.0d+0) 
term(166) = term(166) * (8.0d+0) 
term(167) = term(167) * (-4.0d+0) 
term(168) = term(168) * (-2.0d+0) 
term(169) = term(169) * (-2.0d+0) 
term(170) = term(170) * (4.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(171) = term(171) + r1(vrdav_Rr, a,q) * wm_interm_105_so_pt3(i,j) * wm_interm_72_so_pt3(a,p,j,i)
term(172) = term(172) + r1(vrdav_Rr, a,q) * wm_interm_105_so_pt3(i,j) * wm_interm_74_so_pt3(a,p,j,i)
term(173) = term(173) + r1(vrdav_Rr, a,q) * wm_interm_106_so_pt3(i,j) * wm_interm_72_so_pt3(a,p,j,i)
term(174) = term(174) + r1(vrdav_Rr, a,q) * wm_interm_106_so_pt3(i,j) * wm_interm_74_so_pt3(a,p,j,i)
term(175) = term(175) + r1(vrdav_Rr, a,q) * wm_interm_105_so_pt3(i,j) * wm_interm_73_so_pt3(a,p,j,i)
term(176) = term(176) + r1(vrdav_Rr, a,q) * wm_interm_106_so_pt3(i,j) * wm_interm_73_so_pt3(a,p,j,i)
term(177) = term(177) + r1(vrdav_Rr, a,q) * wm_interm_105_so_pt3(i,j) * wm_interm_66_so_pt3(a,p,j,i)
term(178) = term(178) + r1(vrdav_Rr, a,q) * wm_interm_106_so_pt3(i,j) * wm_interm_66_so_pt3(a,p,j,i)
term(179) = term(179) + r1(vrdav_Rr, a,q) * wm_interm_55_so_pt3(i,j) * wm_interm_99_so_pt3(a,p,j,i)
term(180) = term(180) + r1(vrdav_Rr, a,q) * wm_interm_57_so_pt3(i,j) * wm_interm_99_so_pt3(a,p,j,i)
term(181) = term(181) + t2(a,p,q,i) * wm_interm_105_so_pt3(i,j) * wm_interm_91_so_pt3(a,j)
term(182) = term(182) + t2(a,p,q,i) * wm_interm_106_so_pt3(i,j) * wm_interm_91_so_pt3(a,j)
term(183) = term(183) + t2(a,p,q,i) * wm_interm_55_so_pt3(i,j) * wm_interm_92_so_pt3(a,j)
term(184) = term(184) + t2(a,p,q,i) * wm_interm_57_so_pt3(i,j) * wm_interm_92_so_pt3(a,j)
term(185) = term(185) + t2(a,p,j,i) * wm_interm_55_so_pt3(j,q) * wm_interm_92_so_pt3(a,i)
term(186) = term(186) + t2(a,p,j,i) * wm_interm_57_so_pt3(j,q) * wm_interm_92_so_pt3(a,i)
term(187) = term(187) + t2(a,p,j,i) * wm_interm_105_so_pt3(j,q) * wm_interm_91_so_pt3(a,i)
term(188) = term(188) + t2(a,p,j,i) * wm_interm_106_so_pt3(j,q) * wm_interm_91_so_pt3(a,i)
end do 
end do 
end do 

term(171) = term(171) * (-2.0d+0) 
term(172) = term(172) * (4.0d+0) 
term(173) = term(173) * (4.0d+0) 
term(174) = term(174) * (-8.0d+0) 
term(175) = term(175) * (-2.0d+0) 
term(176) = term(176) * (4.0d+0) 
term(177) = term(177) * (4.0d+0) 
term(178) = term(178) * (-8.0d+0) 
term(179) = term(179) * (-2.0d+0) 
term(180) = term(180) * (4.0d+0) 
term(181) = term(181) * (-2.0d+0) 
term(182) = term(182) * (4.0d+0) 
term(183) = term(183) * (-2.0d+0) 
term(184) = term(184) * (4.0d+0) 
term(185) = term(185) * (-4.0d+0) 
term(186) = term(186) * (8.0d+0) 
term(187) = term(187) * (-4.0d+0) 
term(188) = term(188) * (8.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(189) = term(189) + r2p(vrdav_Rr, a,q,p,i) * s1(b,i) * wm_interm_53_so_pt3(a,b)
term(190) = term(190) + r2p(vrdav_Rr, a,q,p,i) * s1(b,i) * wm_interm_54_so_pt3(a,b)
term(191) = term(191) + r2m(vrdav_Rr, a,q,p,i) * s1(b,i) * wm_interm_53_so_pt3(a,b)
term(192) = term(192) + r2m(vrdav_Rr, a,q,p,i) * s1(b,i) * wm_interm_54_so_pt3(a,b)
term(193) = term(193) + s1(b,i) * t2(a,p,q,i) * wm_interm_28_so_pt3(a,b)
term(194) = term(194) + s1(b,i) * t2(a,p,q,i) * wm_interm_30_so_pt3(a,b)
term(195) = term(195) + s1(b,i) * t2(a,p,q,i) * wm_interm_27_so_pt3(a,b)
term(196) = term(196) + s1(b,i) * t2(a,p,q,i) * wm_interm_49_so_pt3(a,b)
term(197) = term(197) + s1(b,i) * t2(a,p,q,i) * wm_interm_50_so_pt3(a,b)
term(198) = term(198) + r1(vrdav_Rr, p,i) * wm_interm_107_so_pt3(a,b) * wm_interm_73_so_pt3(b,a,i,q)
term(199) = term(199) + r1(vrdav_Rr, p,i) * wm_interm_107_so_pt3(a,b) * wm_interm_74_so_pt3(b,a,i,q)
term(200) = term(200) + r1(vrdav_Rr, p,i) * wm_interm_108_so_pt3(a,b) * wm_interm_73_so_pt3(b,a,i,q)
term(201) = term(201) + r1(vrdav_Rr, p,i) * wm_interm_108_so_pt3(a,b) * wm_interm_74_so_pt3(b,a,i,q)
term(202) = term(202) + r1(vrdav_Rr, p,i) * wm_interm_107_so_pt3(a,b) * wm_interm_72_so_pt3(b,a,i,q)
term(203) = term(203) + r1(vrdav_Rr, p,i) * wm_interm_108_so_pt3(a,b) * wm_interm_72_so_pt3(b,a,i,q)
term(204) = term(204) + r1(vrdav_Rr, p,i) * wm_interm_107_so_pt3(a,b) * wm_interm_66_so_pt3(b,a,i,q)
term(205) = term(205) + r1(vrdav_Rr, p,i) * wm_interm_108_so_pt3(a,b) * wm_interm_66_so_pt3(b,a,i,q)
term(206) = term(206) + r1(vrdav_Rr, p,i) * wm_interm_53_so_pt3(a,b) * wm_interm_99_so_pt3(b,a,i,q)
term(207) = term(207) + r1(vrdav_Rr, p,i) * wm_interm_101_so_pt3(a,b,i,q) * wm_interm_53_so_pt3(b,a)
term(208) = term(208) + r1(vrdav_Rr, p,i) * wm_interm_102_so_pt3(a,b,i,q) * wm_interm_53_so_pt3(b,a)
term(209) = term(209) + r1(vrdav_Rr, p,i) * wm_interm_103_so_pt3(a,b,i,q) * wm_interm_53_so_pt3(b,a)
term(210) = term(210) + r1(vrdav_Rr, p,i) * wm_interm_54_so_pt3(a,b) * wm_interm_99_so_pt3(b,a,i,q)
term(211) = term(211) + r1(vrdav_Rr, p,i) * wm_interm_101_so_pt3(a,b,i,q) * wm_interm_54_so_pt3(b,a)
term(212) = term(212) + r1(vrdav_Rr, p,i) * wm_interm_102_so_pt3(a,b,i,q) * wm_interm_54_so_pt3(b,a)
term(213) = term(213) + r1(vrdav_Rr, p,i) * wm_interm_103_so_pt3(a,b,i,q) * wm_interm_54_so_pt3(b,a)
term(214) = term(214) + r1(vrdav_Rr, a,i) * wm_interm_107_so_pt3(a,b) * wm_interm_66_so_pt3(b,p,i,q)
term(215) = term(215) + r1(vrdav_Rr, a,i) * wm_interm_108_so_pt3(a,b) * wm_interm_66_so_pt3(b,p,i,q)
term(216) = term(216) + t2(a,p,q,i) * wm_interm_107_so_pt3(a,b) * wm_interm_91_so_pt3(b,i)
term(217) = term(217) + t2(a,p,q,i) * wm_interm_108_so_pt3(a,b) * wm_interm_91_so_pt3(b,i)
term(218) = term(218) + t2(a,p,q,i) * wm_interm_53_so_pt3(a,b) * wm_interm_92_so_pt3(b,i)
term(219) = term(219) + r1(vrdav_Rr, a,i) * wm_interm_101_so_pt3(b,p,i,q) * wm_interm_53_so_pt3(a,b)
term(220) = term(220) + t2(a,p,q,i) * wm_interm_54_so_pt3(a,b) * wm_interm_92_so_pt3(b,i)
term(221) = term(221) + r1(vrdav_Rr, a,i) * wm_interm_101_so_pt3(b,p,i,q) * wm_interm_54_so_pt3(a,b)
end do 
end do 
end do 

term(189) = term(189) * (2.0d+0) 
term(190) = term(190) * (-1.0d+0) 
term(191) = term(191) * (8.0d+0) 
term(192) = term(192) * (-4.0d+0) 
term(193) = term(193) * (-1.0d+0) 
term(194) = term(194) * (2.0d+0) 
term(195) = term(195) * (-1.0d+0) 
term(196) = term(196) * (-4.0d+0) 
term(197) = term(197) * (4.0d+0) 
term(198) = term(198) * (-2.0d+0) 
term(199) = term(199) * (4.0d+0) 
term(200) = term(200) * (4.0d+0) 
term(201) = term(201) * (-8.0d+0) 
term(202) = term(202) * (-2.0d+0) 
term(203) = term(203) * (4.0d+0) 
term(204) = term(204) * (4.0d+0) 
term(205) = term(205) * (-8.0d+0) 
term(206) = term(206) * (4.0d+0) 
term(207) = term(207) * (-8.0d+0) 
term(208) = term(208) * (4.0d+0) 
term(209) = term(209) * (-8.0d+0) 
term(210) = term(210) * (-2.0d+0) 
term(211) = term(211) * (4.0d+0) 
term(212) = term(212) * (-2.0d+0) 
term(213) = term(213) * (4.0d+0) 
term(214) = term(214) * (-2.0d+0) 
term(215) = term(215) * (4.0d+0) 
term(216) = term(216) * (-2.0d+0) 
term(217) = term(217) * (4.0d+0) 
term(218) = term(218) * (4.0d+0) 
term(219) = term(219) * (4.0d+0) 
term(220) = term(220) * (-2.0d+0) 
term(221) = term(221) * (-2.0d+0) 

do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(222) = term(222) + r1(vrdav_Rr, b,q) * wm_interm_107_so_pt3(b,a) * wm_interm_54_so_pt3(a,p)
term(223) = term(223) + r1(vrdav_Rr, b,q) * wm_interm_107_so_pt3(b,a) * wm_interm_53_so_pt3(a,p)
term(224) = term(224) + r1(vrdav_Rr, b,q) * wm_interm_108_so_pt3(b,a) * wm_interm_54_so_pt3(a,p)
term(225) = term(225) + r1(vrdav_Rr, b,q) * wm_interm_108_so_pt3(b,a) * wm_interm_53_so_pt3(a,p)
term(226) = term(226) + r1(vrdav_Rr, b,q) * wm_interm_107_so_pt3(a,p) * wm_interm_53_so_pt3(b,a)
term(227) = term(227) + r1(vrdav_Rr, b,q) * wm_interm_108_so_pt3(a,p) * wm_interm_53_so_pt3(b,a)
term(228) = term(228) + r1(vrdav_Rr, b,q) * wm_interm_107_so_pt3(a,p) * wm_interm_54_so_pt3(b,a)
term(229) = term(229) + r1(vrdav_Rr, b,q) * wm_interm_108_so_pt3(a,p) * wm_interm_54_so_pt3(b,a)
end do 
end do 

term(222) = term(222) * (-2.0d+0) 
term(223) = term(223) * (4.0d+0) 
term(224) = term(224) * (4.0d+0) 
term(225) = term(225) * (-8.0d+0) 
term(226) = term(226) * (4.0d+0) 
term(227) = term(227) * (-8.0d+0) 
term(228) = term(228) * (-2.0d+0) 
term(229) = term(229) * (4.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(230) = term(230) + t2(a,p,j,i) * wm_interm_102_so_pt3(a,b,k,q) * wm_interm_88_so_pt3(b,j,i,k)
term(231) = term(231) + t2(a,p,j,i) * wm_interm_103_so_pt3(a,b,k,q) * wm_interm_88_so_pt3(b,j,i,k)
term(232) = term(232) + t2(a,p,j,i) * wm_interm_88_so_pt3(b,j,i,k) * wm_interm_99_so_pt3(a,b,k,q)
term(233) = term(233) + t2(a,p,j,i) * wm_interm_101_so_pt3(a,b,k,q) * wm_interm_88_so_pt3(b,j,i,k)
term(234) = term(234) + t2(a,p,j,i) * wm_interm_101_so_pt3(a,b,k,q) * wm_interm_86_so_pt3(b,j,i,k)
end do 
end do 
end do 
end do 
end do 

term(230) = term(230) * (-2.0d+0) 
term(231) = term(231) * (4.0d+0) 
term(232) = term(232) * (-2.0d+0) 
term(233) = term(233) * (4.0d+0) 
term(234) = term(234) * (-2.0d+0) 

do l = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(235) = term(235) + t2(a,p,j,i) * wm_interm_100_so_pt3(i,j,k,l) * wm_interm_88_so_pt3(a,k,l,q)
term(236) = term(236) + t2(a,p,j,i) * wm_interm_100_so_pt3(j,i,k,l) * wm_interm_88_so_pt3(a,k,l,q)
term(237) = term(237) + t2(a,p,j,i) * wm_interm_100_so_pt3(j,i,k,l) * wm_interm_86_so_pt3(a,k,l,q)
term(238) = term(238) + t2(a,p,j,i) * wm_interm_100_so_pt3(i,j,k,l) * wm_interm_86_so_pt3(a,k,l,q)
term(239) = term(239) + t2(a,p,j,i) * wm_interm_70_so_pt3(j,i,k,l) * wm_interm_94_so_pt3(a,q,k,l)
term(240) = term(240) + t2(a,p,j,i) * wm_interm_70_so_pt3(i,j,k,l) * wm_interm_94_so_pt3(a,q,k,l)
term(241) = term(241) + t2(a,p,q,i) * wm_interm_70_so_pt3(i,j,k,l) * wm_interm_94_so_pt3(a,j,k,l)
end do 
end do 
end do 
end do 
end do 

term(235) = term(235) * (-1.0d+0) 
term(236) = term(236) * (2.0d+0) 
term(237) = term(237) * (-1.0d+0) 
term(238) = term(238) * (2.0d+0) 
term(239) = term(239) * (-1.0d+0) 
term(240) = term(240) * (2.0d+0) 
term(241) = term(241) * (-1.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(242) = term(242) + wm_interm_12_so_pt3(a,p,i,q) * wm_interm_51_so_pt3(a,i)
term(243) = term(243) + wm_interm_14_so_pt3(a,p,i,q) * wm_interm_51_so_pt3(a,i)
term(244) = term(244) + wm_interm_10_so_pt3(a,p,i,q) * wm_interm_51_so_pt3(a,i)
term(245) = term(245) + wm_interm_3_so_pt3(a,p,i,q) * wm_interm_51_so_pt3(a,i)
term(246) = term(246) + wm_interm_4_so_pt3(a,p,i,q) * wm_interm_51_so_pt3(a,i)
term(247) = term(247) + wm_interm_0_so_pt3(a,p,i,q) * wm_interm_51_so_pt3(a,i)
term(248) = term(248) + wm_interm_3_so_pt3(a,p,i,q) * wm_interm_52_so_pt3(a,i)
term(249) = term(249) + wm_interm_4_so_pt3(a,p,i,q) * wm_interm_52_so_pt3(a,i)
term(250) = term(250) + wm_interm_12_so_pt3(a,p,i,q) * wm_interm_52_so_pt3(a,i)
term(251) = term(251) + wm_interm_14_so_pt3(a,p,i,q) * wm_interm_52_so_pt3(a,i)
term(252) = term(252) + wm_interm_0_so_pt3(a,p,i,q) * wm_interm_52_so_pt3(a,i)
term(253) = term(253) + wm_interm_10_so_pt3(a,p,i,q) * wm_interm_52_so_pt3(a,i)
term(254) = term(254) + wm_interm_37_so_pt3(a,p,i,q) * wm_interm_51_so_pt3(a,i)
term(255) = term(255) + wm_interm_39_so_pt3(a,p,i,q) * wm_interm_51_so_pt3(a,i)
term(256) = term(256) + wm_interm_32_so_pt3(a,p,i,q) * wm_interm_51_so_pt3(a,i)
term(257) = term(257) + wm_interm_33_so_pt3(a,p,i,q) * wm_interm_51_so_pt3(a,i)
term(258) = term(258) + wm_interm_32_so_pt3(a,p,i,q) * wm_interm_52_so_pt3(a,i)
term(259) = term(259) + wm_interm_33_so_pt3(a,p,i,q) * wm_interm_52_so_pt3(a,i)
term(260) = term(260) + wm_interm_37_so_pt3(a,p,i,q) * wm_interm_52_so_pt3(a,i)
term(261) = term(261) + wm_interm_39_so_pt3(a,p,i,q) * wm_interm_52_so_pt3(a,i)
term(262) = term(262) + wm_interm_63_so_pt3(a,i) * wm_interm_66_so_pt3(a,p,i,q)
term(263) = term(263) + wm_interm_64_so_pt3(a,i) * wm_interm_66_so_pt3(a,p,i,q)
term(264) = term(264) + wm_interm_65_so_pt3(a,i) * wm_interm_66_so_pt3(a,p,i,q)
term(265) = term(265) + wm_interm_66_so_pt3(a,p,i,q) * wm_interm_67_so_pt3(a,i)
term(266) = term(266) + wm_interm_66_so_pt3(a,p,i,q) * wm_interm_68_so_pt3(a,i)
term(267) = term(267) + r1(vrdav_Rr, a,i) * wm_interm_107_so_pt3(a,p) * wm_interm_55_so_pt3(i,q)
term(268) = term(268) + r1(vrdav_Rr, a,i) * wm_interm_107_so_pt3(a,p) * wm_interm_57_so_pt3(i,q)
term(269) = term(269) + r1(vrdav_Rr, a,i) * wm_interm_108_so_pt3(a,p) * wm_interm_55_so_pt3(i,q)
term(270) = term(270) + r1(vrdav_Rr, a,i) * wm_interm_108_so_pt3(a,p) * wm_interm_57_so_pt3(i,q)
term(271) = term(271) + r1(vrdav_Rr, a,i) * wm_interm_105_so_pt3(i,q) * wm_interm_54_so_pt3(a,p)
term(272) = term(272) + r1(vrdav_Rr, a,i) * wm_interm_105_so_pt3(i,q) * wm_interm_53_so_pt3(a,p)
term(273) = term(273) + r1(vrdav_Rr, a,i) * wm_interm_106_so_pt3(i,q) * wm_interm_54_so_pt3(a,p)
term(274) = term(274) + r1(vrdav_Rr, a,i) * wm_interm_106_so_pt3(i,q) * wm_interm_53_so_pt3(a,p)
end do 
end do 

term(242) = term(242) * (-4.0d+0) 
term(243) = term(243) * (8.0d+0) 
term(244) = term(244) * (-4.0d+0) 
term(245) = term(245) * (2.0d+0) 
term(246) = term(246) * (-4.0d+0) 
term(247) = term(247) * (2.0d+0) 
term(248) = term(248) * (-1.0d+0) 
term(249) = term(249) * (2.0d+0) 
term(250) = term(250) * (2.0d+0) 
term(251) = term(251) * (-4.0d+0) 
term(252) = term(252) * (-1.0d+0) 
term(253) = term(253) * (2.0d+0) 
term(254) = term(254) * (-16.0d+0) 
term(255) = term(255) * (16.0d+0) 
term(256) = term(256) * (8.0d+0) 
term(257) = term(257) * (-8.0d+0) 
term(258) = term(258) * (-4.0d+0) 
term(259) = term(259) * (4.0d+0) 
term(260) = term(260) * (8.0d+0) 
term(261) = term(261) * (-8.0d+0) 
term(262) = term(262) * (-2.0d+0) 
term(265) = term(265) * (-4.0d+0) 
term(266) = term(266) * (4.0d+0) 
term(267) = term(267) * (-4.0d+0) 
term(268) = term(268) * (8.0d+0) 
term(269) = term(269) * (8.0d+0) 
term(270) = term(270) * (-16.0d+0) 
term(271) = term(271) * (-4.0d+0) 
term(272) = term(272) * (8.0d+0) 
term(273) = term(273) * (8.0d+0) 
term(274) = term(274) * (-16.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(275) = term(275) + t2(a,p,j,i) * wm_interm_72_so_pt3(a,b,k,q) * wm_interm_94_so_pt3(b,k,i,j)
term(276) = term(276) + t2(a,p,j,i) * wm_interm_66_so_pt3(a,b,k,q) * wm_interm_94_so_pt3(b,k,i,j)
term(277) = term(277) + t2(a,p,j,i) * wm_interm_73_so_pt3(a,b,k,q) * wm_interm_94_so_pt3(b,k,i,j)
term(278) = term(278) + t2(a,p,j,i) * wm_interm_74_so_pt3(a,b,k,q) * wm_interm_94_so_pt3(b,k,i,j)
end do 
end do 
end do 
end do 
end do 

term(275) = term(275) * (-4.0d+0) 
term(276) = term(276) * (8.0d+0) 
term(277) = term(277) * (-4.0d+0) 
term(278) = term(278) * (8.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(279) = term(279) + r1(vrdav_Rr, p,i) * wm_interm_101_so_pt3(a,b,j,q) * wm_interm_74_so_pt3(b,a,i,j)
term(280) = term(280) + r1(vrdav_Rr, p,i) * wm_interm_102_so_pt3(a,b,i,j) * wm_interm_74_so_pt3(b,a,j,q)
term(281) = term(281) + r1(vrdav_Rr, p,i) * wm_interm_74_so_pt3(a,b,i,j) * wm_interm_99_so_pt3(b,a,j,q)
term(282) = term(282) + r1(vrdav_Rr, p,i) * wm_interm_103_so_pt3(a,b,i,j) * wm_interm_74_so_pt3(b,a,j,q)
term(283) = term(283) + r1(vrdav_Rr, p,i) * wm_interm_102_so_pt3(a,b,i,j) * wm_interm_72_so_pt3(b,a,j,q)
term(284) = term(284) + r1(vrdav_Rr, p,i) * wm_interm_103_so_pt3(a,b,i,j) * wm_interm_72_so_pt3(b,a,j,q)
term(285) = term(285) + r1(vrdav_Rr, p,i) * wm_interm_102_so_pt3(a,b,j,q) * wm_interm_74_so_pt3(b,a,i,j)
term(286) = term(286) + r1(vrdav_Rr, p,i) * wm_interm_103_so_pt3(a,b,j,q) * wm_interm_74_so_pt3(b,a,i,j)
term(287) = term(287) + r1(vrdav_Rr, p,i) * wm_interm_101_so_pt3(a,b,j,q) * wm_interm_73_so_pt3(b,a,i,j)
term(288) = term(288) + r1(vrdav_Rr, p,i) * wm_interm_102_so_pt3(a,b,i,j) * wm_interm_73_so_pt3(b,a,j,q)
term(289) = term(289) + r1(vrdav_Rr, p,i) * wm_interm_73_so_pt3(a,b,i,j) * wm_interm_99_so_pt3(b,a,j,q)
term(290) = term(290) + r1(vrdav_Rr, p,i) * wm_interm_103_so_pt3(a,b,i,j) * wm_interm_73_so_pt3(b,a,j,q)
term(291) = term(291) + r1(vrdav_Rr, p,i) * wm_interm_102_so_pt3(a,b,i,j) * wm_interm_66_so_pt3(b,a,j,q)
term(292) = term(292) + r1(vrdav_Rr, p,i) * wm_interm_103_so_pt3(a,b,i,j) * wm_interm_66_so_pt3(b,a,j,q)
term(293) = term(293) + r1(vrdav_Rr, p,i) * wm_interm_102_so_pt3(a,b,j,q) * wm_interm_73_so_pt3(b,a,i,j)
term(294) = term(294) + r1(vrdav_Rr, p,i) * wm_interm_103_so_pt3(a,b,j,q) * wm_interm_73_so_pt3(b,a,i,j)
term(295) = term(295) + r1(vrdav_Rr, p,i) * wm_interm_66_so_pt3(a,b,i,j) * wm_interm_99_so_pt3(b,a,j,q)
term(296) = term(296) + r1(vrdav_Rr, p,i) * wm_interm_73_so_pt3(a,b,j,q) * wm_interm_99_so_pt3(b,a,i,j)
term(297) = term(297) + r1(vrdav_Rr, p,i) * wm_interm_66_so_pt3(a,b,j,q) * wm_interm_99_so_pt3(b,a,i,j)
term(298) = term(298) + r1(vrdav_Rr, p,i) * wm_interm_103_so_pt3(a,b,j,q) * wm_interm_66_so_pt3(b,a,i,j)
term(299) = term(299) + r1(vrdav_Rr, p,i) * wm_interm_101_so_pt3(a,b,i,j) * wm_interm_73_so_pt3(b,a,j,q)
term(300) = term(300) + r1(vrdav_Rr, p,i) * wm_interm_101_so_pt3(a,b,j,q) * wm_interm_66_so_pt3(b,a,i,j)
term(301) = term(301) + r1(vrdav_Rr, p,i) * wm_interm_102_so_pt3(a,b,j,q) * wm_interm_66_so_pt3(b,a,i,j)
term(302) = term(302) + r1(vrdav_Rr, p,i) * wm_interm_101_so_pt3(a,b,i,j) * wm_interm_66_so_pt3(b,a,j,q)
term(303) = term(303) + r1(vrdav_Rr, p,i) * wm_interm_101_so_pt3(a,b,j,q) * wm_interm_72_so_pt3(b,a,i,j)
term(304) = term(304) + r1(vrdav_Rr, p,i) * wm_interm_101_so_pt3(a,b,i,j) * wm_interm_74_so_pt3(b,a,j,q)
term(305) = term(305) + r1(vrdav_Rr, p,i) * wm_interm_72_so_pt3(a,b,i,j) * wm_interm_99_so_pt3(b,a,j,q)
term(306) = term(306) + r1(vrdav_Rr, p,i) * wm_interm_74_so_pt3(a,b,j,q) * wm_interm_99_so_pt3(b,a,i,j)
term(307) = term(307) + r1(vrdav_Rr, p,i) * wm_interm_101_so_pt3(a,b,i,j) * wm_interm_72_so_pt3(b,a,j,q)
term(308) = term(308) + r1(vrdav_Rr, p,i) * wm_interm_72_so_pt3(a,b,j,q) * wm_interm_99_so_pt3(b,a,i,j)
term(309) = term(309) + r1(vrdav_Rr, p,i) * wm_interm_102_so_pt3(a,b,j,q) * wm_interm_72_so_pt3(b,a,i,j)
term(310) = term(310) + r1(vrdav_Rr, p,i) * wm_interm_103_so_pt3(a,b,j,q) * wm_interm_72_so_pt3(b,a,i,j)
term(311) = term(311) + r1(vrdav_Rr, a,q) * wm_interm_102_so_pt3(a,b,i,j) * wm_interm_72_so_pt3(b,p,j,i)
term(312) = term(312) + r1(vrdav_Rr, a,q) * wm_interm_102_so_pt3(a,b,i,j) * wm_interm_74_so_pt3(b,p,j,i)
term(313) = term(313) + r1(vrdav_Rr, a,q) * wm_interm_103_so_pt3(a,b,i,j) * wm_interm_72_so_pt3(b,p,j,i)
term(314) = term(314) + r1(vrdav_Rr, a,q) * wm_interm_103_so_pt3(a,b,i,j) * wm_interm_74_so_pt3(b,p,j,i)
term(315) = term(315) + r1(vrdav_Rr, a,q) * wm_interm_101_so_pt3(b,p,i,j) * wm_interm_74_so_pt3(a,b,j,i)
term(316) = term(316) + r1(vrdav_Rr, a,q) * wm_interm_74_so_pt3(a,b,i,j) * wm_interm_99_so_pt3(b,p,j,i)
term(317) = term(317) + r1(vrdav_Rr, a,q) * wm_interm_102_so_pt3(b,p,i,j) * wm_interm_74_so_pt3(a,b,j,i)
term(318) = term(318) + r1(vrdav_Rr, a,q) * wm_interm_103_so_pt3(b,p,i,j) * wm_interm_74_so_pt3(a,b,j,i)
term(319) = term(319) + r1(vrdav_Rr, a,q) * wm_interm_102_so_pt3(a,b,i,j) * wm_interm_66_so_pt3(b,p,j,i)
term(320) = term(320) + r1(vrdav_Rr, a,q) * wm_interm_102_so_pt3(a,b,i,j) * wm_interm_73_so_pt3(b,p,j,i)
term(321) = term(321) + r1(vrdav_Rr, a,q) * wm_interm_103_so_pt3(a,b,i,j) * wm_interm_66_so_pt3(b,p,j,i)
term(322) = term(322) + r1(vrdav_Rr, a,q) * wm_interm_103_so_pt3(a,b,i,j) * wm_interm_73_so_pt3(b,p,j,i)
term(323) = term(323) + r1(vrdav_Rr, a,q) * wm_interm_101_so_pt3(b,p,i,j) * wm_interm_73_so_pt3(a,b,j,i)
term(324) = term(324) + r1(vrdav_Rr, a,q) * wm_interm_73_so_pt3(a,b,i,j) * wm_interm_99_so_pt3(b,p,j,i)
term(325) = term(325) + r1(vrdav_Rr, a,q) * wm_interm_102_so_pt3(b,p,i,j) * wm_interm_73_so_pt3(a,b,j,i)
term(326) = term(326) + r1(vrdav_Rr, a,q) * wm_interm_103_so_pt3(b,p,i,j) * wm_interm_73_so_pt3(a,b,j,i)
term(327) = term(327) + r1(vrdav_Rr, a,q) * wm_interm_66_so_pt3(b,p,i,j) * wm_interm_99_so_pt3(a,b,j,i)
term(328) = term(328) + r1(vrdav_Rr, a,q) * wm_interm_73_so_pt3(b,p,i,j) * wm_interm_99_so_pt3(a,b,j,i)
term(329) = term(329) + r1(vrdav_Rr, a,q) * wm_interm_66_so_pt3(a,b,i,j) * wm_interm_99_so_pt3(b,p,j,i)
term(330) = term(330) + r1(vrdav_Rr, a,q) * wm_interm_103_so_pt3(b,p,i,j) * wm_interm_66_so_pt3(a,b,j,i)
term(331) = term(331) + r1(vrdav_Rr, a,q) * wm_interm_101_so_pt3(a,b,i,j) * wm_interm_73_so_pt3(b,p,j,i)
term(332) = term(332) + r1(vrdav_Rr, a,q) * wm_interm_101_so_pt3(a,b,i,j) * wm_interm_66_so_pt3(b,p,j,i)
term(333) = term(333) + t2(a,p,j,i) * wm_interm_53_so_pt3(a,b) * wm_interm_94_so_pt3(b,q,i,j)
term(334) = term(334) + r1(vrdav_Rr, a,q) * wm_interm_102_so_pt3(b,p,i,j) * wm_interm_66_so_pt3(a,b,j,i)
term(335) = term(335) + r1(vrdav_Rr, a,q) * wm_interm_101_so_pt3(b,p,i,j) * wm_interm_66_so_pt3(a,b,j,i)
term(336) = term(336) + t2(a,p,j,i) * wm_interm_54_so_pt3(a,b) * wm_interm_94_so_pt3(b,q,i,j)
term(337) = term(337) + r1(vrdav_Rr, a,q) * wm_interm_101_so_pt3(a,b,i,j) * wm_interm_72_so_pt3(b,p,j,i)
term(338) = term(338) + r1(vrdav_Rr, a,q) * wm_interm_101_so_pt3(a,b,i,j) * wm_interm_74_so_pt3(b,p,j,i)
term(339) = term(339) + r1(vrdav_Rr, a,q) * wm_interm_72_so_pt3(b,p,i,j) * wm_interm_99_so_pt3(a,b,j,i)
term(340) = term(340) + r1(vrdav_Rr, a,q) * wm_interm_74_so_pt3(b,p,i,j) * wm_interm_99_so_pt3(a,b,j,i)
term(341) = term(341) + r1(vrdav_Rr, a,q) * wm_interm_101_so_pt3(b,p,i,j) * wm_interm_72_so_pt3(a,b,j,i)
term(342) = term(342) + r1(vrdav_Rr, a,q) * wm_interm_72_so_pt3(a,b,i,j) * wm_interm_99_so_pt3(b,p,j,i)
term(343) = term(343) + r1(vrdav_Rr, a,q) * wm_interm_102_so_pt3(b,p,i,j) * wm_interm_72_so_pt3(a,b,j,i)
term(344) = term(344) + r1(vrdav_Rr, a,q) * wm_interm_103_so_pt3(b,p,i,j) * wm_interm_72_so_pt3(a,b,j,i)
term(345) = term(345) + t2(a,p,q,i) * wm_interm_66_so_pt3(a,b,i,j) * wm_interm_92_so_pt3(b,j)
term(346) = term(346) + t2(a,p,q,i) * wm_interm_101_so_pt3(a,b,i,j) * wm_interm_91_so_pt3(b,j)
term(347) = term(347) + r1(vrdav_Rr, a,i) * wm_interm_102_so_pt3(b,p,i,j) * wm_interm_72_so_pt3(a,b,j,q)
term(348) = term(348) + r1(vrdav_Rr, a,i) * wm_interm_102_so_pt3(b,p,i,j) * wm_interm_73_so_pt3(a,b,j,q)
term(349) = term(349) + r1(vrdav_Rr, a,i) * wm_interm_102_so_pt3(b,p,i,j) * wm_interm_74_so_pt3(a,b,j,q)
term(350) = term(350) + r1(vrdav_Rr, a,i) * wm_interm_101_so_pt3(b,p,i,j) * wm_interm_66_so_pt3(a,b,j,q)
term(351) = term(351) + r1(vrdav_Rr, a,i) * wm_interm_72_so_pt3(a,b,j,q) * wm_interm_99_so_pt3(b,p,i,j)
term(352) = term(352) + r1(vrdav_Rr, a,i) * wm_interm_103_so_pt3(b,p,i,j) * wm_interm_72_so_pt3(a,b,j,q)
term(353) = term(353) + r1(vrdav_Rr, a,i) * wm_interm_73_so_pt3(a,b,j,q) * wm_interm_99_so_pt3(b,p,i,j)
term(354) = term(354) + r1(vrdav_Rr, a,i) * wm_interm_74_so_pt3(a,b,j,q) * wm_interm_99_so_pt3(b,p,i,j)
term(355) = term(355) + r1(vrdav_Rr, a,i) * wm_interm_103_so_pt3(b,p,i,j) * wm_interm_73_so_pt3(a,b,j,q)
term(356) = term(356) + r1(vrdav_Rr, a,i) * wm_interm_103_so_pt3(b,p,i,j) * wm_interm_74_so_pt3(a,b,j,q)
term(357) = term(357) + r1(vrdav_Rr, a,i) * wm_interm_102_so_pt3(a,b,j,q) * wm_interm_72_so_pt3(b,p,i,j)
term(358) = term(358) + r1(vrdav_Rr, a,i) * wm_interm_101_so_pt3(a,b,j,q) * wm_interm_66_so_pt3(b,p,i,j)
term(359) = term(359) + r1(vrdav_Rr, a,i) * wm_interm_103_so_pt3(a,b,j,q) * wm_interm_72_so_pt3(b,p,i,j)
term(360) = term(360) + t2(a,b,q,i) * wm_interm_72_so_pt3(a,p,i,j) * wm_interm_92_so_pt3(b,j)
term(361) = term(361) + r1(vrdav_Rr, a,i) * wm_interm_72_so_pt3(b,p,i,j) * wm_interm_99_so_pt3(a,b,j,q)
term(362) = term(362) + t2(a,b,q,i) * wm_interm_66_so_pt3(b,p,i,j) * wm_interm_92_so_pt3(a,j)
term(363) = term(363) + t2(a,b,q,i) * wm_interm_72_so_pt3(b,p,i,j) * wm_interm_92_so_pt3(a,j)
term(364) = term(364) + r1(vrdav_Rr, a,i) * wm_interm_102_so_pt3(a,b,j,q) * wm_interm_73_so_pt3(b,p,i,j)
term(365) = term(365) + r1(vrdav_Rr, a,i) * wm_interm_102_so_pt3(a,b,j,q) * wm_interm_74_so_pt3(b,p,i,j)
term(366) = term(366) + t2(a,b,q,i) * wm_interm_73_so_pt3(a,p,i,j) * wm_interm_92_so_pt3(b,j)
term(367) = term(367) + r1(vrdav_Rr, a,i) * wm_interm_103_so_pt3(a,b,j,q) * wm_interm_73_so_pt3(b,p,i,j)
term(368) = term(368) + t2(a,b,q,i) * wm_interm_74_so_pt3(a,p,i,j) * wm_interm_92_so_pt3(b,j)
term(369) = term(369) + r1(vrdav_Rr, a,i) * wm_interm_103_so_pt3(a,b,j,q) * wm_interm_74_so_pt3(b,p,i,j)
term(370) = term(370) + r1(vrdav_Rr, a,i) * wm_interm_73_so_pt3(b,p,i,j) * wm_interm_99_so_pt3(a,b,j,q)
term(371) = term(371) + r1(vrdav_Rr, a,i) * wm_interm_74_so_pt3(b,p,i,j) * wm_interm_99_so_pt3(a,b,j,q)
term(372) = term(372) + t2(a,b,q,i) * wm_interm_73_so_pt3(b,p,i,j) * wm_interm_92_so_pt3(a,j)
term(373) = term(373) + t2(a,b,q,i) * wm_interm_74_so_pt3(b,p,i,j) * wm_interm_92_so_pt3(a,j)
end do 
end do 
end do 
end do 

term(279) = term(279) * (4.0d+0) 
term(280) = term(280) * (-8.0d+0) 
term(281) = term(281) * (-8.0d+0) 
term(282) = term(282) * (16.0d+0) 
term(283) = term(283) * (4.0d+0) 
term(284) = term(284) * (-8.0d+0) 
term(285) = term(285) * (-8.0d+0) 
term(286) = term(286) * (16.0d+0) 
term(287) = term(287) * (-2.0d+0) 
term(288) = term(288) * (4.0d+0) 
term(289) = term(289) * (4.0d+0) 
term(290) = term(290) * (-8.0d+0) 
term(291) = term(291) * (-2.0d+0) 
term(292) = term(292) * (4.0d+0) 
term(293) = term(293) * (4.0d+0) 
term(294) = term(294) * (-8.0d+0) 
term(295) = term(295) * (-2.0d+0) 
term(296) = term(296) * (4.0d+0) 
term(297) = term(297) * (-2.0d+0) 
term(298) = term(298) * (4.0d+0) 
term(299) = term(299) * (-2.0d+0) 
term(300) = term(300) * (4.0d+0) 
term(301) = term(301) * (-2.0d+0) 
term(302) = term(302) * (4.0d+0) 
term(303) = term(303) * (-2.0d+0) 
term(304) = term(304) * (4.0d+0) 
term(305) = term(305) * (4.0d+0) 
term(306) = term(306) * (-8.0d+0) 
term(307) = term(307) * (-2.0d+0) 
term(308) = term(308) * (4.0d+0) 
term(309) = term(309) * (4.0d+0) 
term(310) = term(310) * (-8.0d+0) 
term(311) = term(311) * (4.0d+0) 
term(312) = term(312) * (-8.0d+0) 
term(313) = term(313) * (-8.0d+0) 
term(314) = term(314) * (16.0d+0) 
term(315) = term(315) * (4.0d+0) 
term(316) = term(316) * (-8.0d+0) 
term(317) = term(317) * (-8.0d+0) 
term(318) = term(318) * (16.0d+0) 
term(319) = term(319) * (-2.0d+0) 
term(320) = term(320) * (4.0d+0) 
term(321) = term(321) * (4.0d+0) 
term(322) = term(322) * (-8.0d+0) 
term(323) = term(323) * (-2.0d+0) 
term(324) = term(324) * (4.0d+0) 
term(325) = term(325) * (4.0d+0) 
term(326) = term(326) * (-8.0d+0) 
term(327) = term(327) * (-2.0d+0) 
term(328) = term(328) * (4.0d+0) 
term(329) = term(329) * (-2.0d+0) 
term(330) = term(330) * (4.0d+0) 
term(331) = term(331) * (-2.0d+0) 
term(332) = term(332) * (4.0d+0) 
term(333) = term(333) * (-8.0d+0) 
term(334) = term(334) * (-2.0d+0) 
term(335) = term(335) * (4.0d+0) 
term(336) = term(336) * (4.0d+0) 
term(337) = term(337) * (-2.0d+0) 
term(338) = term(338) * (4.0d+0) 
term(339) = term(339) * (4.0d+0) 
term(340) = term(340) * (-8.0d+0) 
term(341) = term(341) * (-2.0d+0) 
term(342) = term(342) * (4.0d+0) 
term(343) = term(343) * (4.0d+0) 
term(344) = term(344) * (-8.0d+0) 
term(345) = term(345) * (-1.0d+0) 
term(346) = term(346) * (-2.0d+0) 
term(347) = term(347) * (-4.0d+0) 
term(348) = term(348) * (-4.0d+0) 
term(349) = term(349) * (8.0d+0) 
term(350) = term(350) * (-4.0d+0) 
term(351) = term(351) * (-4.0d+0) 
term(352) = term(352) * (8.0d+0) 
term(353) = term(353) * (-4.0d+0) 
term(354) = term(354) * (8.0d+0) 
term(355) = term(355) * (8.0d+0) 
term(356) = term(356) * (-16.0d+0) 
term(357) = term(357) * (-4.0d+0) 
term(358) = term(358) * (-4.0d+0) 
term(359) = term(359) * (8.0d+0) 
term(360) = term(360) * (-2.0d+0) 
term(361) = term(361) * (-4.0d+0) 
term(362) = term(362) * (-2.0d+0) 
term(363) = term(363) * (4.0d+0) 
term(364) = term(364) * (-4.0d+0) 
term(365) = term(365) * (8.0d+0) 
term(366) = term(366) * (-2.0d+0) 
term(367) = term(367) * (8.0d+0) 
term(368) = term(368) * (4.0d+0) 
term(369) = term(369) * (-16.0d+0) 
term(370) = term(370) * (-4.0d+0) 
term(371) = term(371) * (8.0d+0) 
term(372) = term(372) * (4.0d+0) 
term(373) = term(373) * (-8.0d+0) 

do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
term(374) = term(374) + wm_interm_36_so_pt3(i,j,k,q) * wm_interm_71_so_pt3(p,k,i,j)
term(375) = term(375) + r1(vrdav_Rr, p,i) * wm_interm_105_so_pt3(j,k) * wm_interm_70_so_pt3(i,k,j,q)
term(376) = term(376) + r1(vrdav_Rr, p,i) * wm_interm_106_so_pt3(j,k) * wm_interm_70_so_pt3(i,k,j,q)
term(377) = term(377) + r1(vrdav_Rr, p,i) * wm_interm_105_so_pt3(j,k) * wm_interm_70_so_pt3(i,k,q,j)
term(378) = term(378) + r1(vrdav_Rr, p,i) * wm_interm_106_so_pt3(j,k) * wm_interm_70_so_pt3(i,k,q,j)
term(379) = term(379) + r1(vrdav_Rr, p,i) * wm_interm_105_so_pt3(j,k) * wm_interm_70_so_pt3(k,i,j,q)
term(380) = term(380) + r1(vrdav_Rr, p,i) * wm_interm_106_so_pt3(j,k) * wm_interm_70_so_pt3(k,i,j,q)
term(381) = term(381) + r1(vrdav_Rr, p,i) * wm_interm_105_so_pt3(j,k) * wm_interm_70_so_pt3(k,i,q,j)
term(382) = term(382) + r1(vrdav_Rr, p,i) * wm_interm_106_so_pt3(j,k) * wm_interm_70_so_pt3(k,i,q,j)
end do 
end do 
end do 

term(374) = term(374) * (2.0d+0) 
term(375) = term(375) * (-1.0d+0) 
term(376) = term(376) * (2.0d+0) 
term(377) = term(377) * (2.0d+0) 
term(378) = term(378) * (-4.0d+0) 
term(379) = term(379) * (2.0d+0) 
term(380) = term(380) * (-4.0d+0) 
term(381) = term(381) * (-1.0d+0) 
term(382) = term(382) * (2.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
term(383) = term(383) + wm_interm_36_so_pt3(i,j,k,q) * wm_interm_71_so_pt3(p,k,j,i)
end do 
end do 
end do 

term(383) = term(383) * (-2.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(384) = term(384) + r2p(vrdav_Rr, p,q,a,i) * s1(b,i) * wm_interm_53_so_pt3(a,b)
term(385) = term(385) + r2p(vrdav_Rr, p,q,a,i) * s1(b,i) * wm_interm_54_so_pt3(a,b)
end do 
end do 
end do 

term(384) = term(384) * (-4.0d+0) 
term(385) = term(385) * (2.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(386) = term(386) + t2(a,p,q,i) * wm_interm_74_so_pt3(a,b,j,k) * wm_interm_94_so_pt3(b,j,k,i)
term(387) = term(387) + t2(a,p,q,i) * wm_interm_74_so_pt3(a,b,j,k) * wm_interm_94_so_pt3(b,j,i,k)
term(388) = term(388) + t2(a,p,q,i) * wm_interm_73_so_pt3(a,b,j,k) * wm_interm_94_so_pt3(b,j,k,i)
term(389) = term(389) + t2(a,p,q,i) * wm_interm_73_so_pt3(a,b,j,k) * wm_interm_94_so_pt3(b,j,i,k)
term(390) = term(390) + t2(a,p,q,i) * wm_interm_66_so_pt3(a,b,j,k) * wm_interm_94_so_pt3(b,j,i,k)
term(391) = term(391) + t2(a,p,q,i) * wm_interm_72_so_pt3(a,b,j,k) * wm_interm_94_so_pt3(b,j,k,i)
term(392) = term(392) + t2(a,p,q,i) * wm_interm_72_so_pt3(a,b,j,k) * wm_interm_94_so_pt3(b,j,i,k)
term(393) = term(393) + t2(a,p,j,i) * wm_interm_102_so_pt3(a,b,k,q) * wm_interm_86_so_pt3(b,i,j,k)
term(394) = term(394) + t2(a,p,j,i) * wm_interm_103_so_pt3(a,b,k,q) * wm_interm_86_so_pt3(b,i,j,k)
term(395) = term(395) + t2(a,p,j,i) * wm_interm_86_so_pt3(b,i,j,k) * wm_interm_99_so_pt3(a,b,k,q)
term(396) = term(396) + t2(a,p,j,i) * wm_interm_101_so_pt3(a,b,k,q) * wm_interm_88_so_pt3(b,i,j,k)
term(397) = term(397) + t2(a,p,j,i) * wm_interm_101_so_pt3(a,b,k,q) * wm_interm_86_so_pt3(b,i,j,k)
end do 
end do 
end do 
end do 
end do 

term(386) = term(386) * (4.0d+0) 
term(387) = term(387) * (-8.0d+0) 
term(388) = term(388) * (-2.0d+0) 
term(389) = term(389) * (4.0d+0) 
term(390) = term(390) * (-2.0d+0) 
term(391) = term(391) * (-2.0d+0) 
term(392) = term(392) * (4.0d+0) 
term(393) = term(393) * (-2.0d+0) 
term(394) = term(394) * (4.0d+0) 
term(395) = term(395) * (-2.0d+0) 
term(396) = term(396) * (-2.0d+0) 
term(397) = term(397) * (4.0d+0) 

do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
do i = 1, nocc 
term(398) = term(398) + r2p(vrdav_Rr, p,i,a,q) * s1(b,i) * wm_interm_53_so_pt3(a,b)
term(399) = term(399) + r2p(vrdav_Rr, p,i,a,q) * s1(b,i) * wm_interm_54_so_pt3(a,b)
end do 
end do 
end do 

term(398) = term(398) * (2.0d+0) 
term(399) = term(399) * (-1.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
term(400) = term(400) + wm_interm_55_so_pt3(i,j) * wm_interm_56_so_pt3(p,i,q,j)
term(401) = term(401) + wm_interm_55_so_pt3(i,j) * wm_interm_56_so_pt3(p,q,i,j)
term(402) = term(402) + wm_interm_56_so_pt3(p,i,q,j) * wm_interm_57_so_pt3(i,j)
term(403) = term(403) + wm_interm_56_so_pt3(p,q,i,j) * wm_interm_57_so_pt3(i,j)
term(404) = term(404) + wm_interm_55_so_pt3(i,j) * wm_interm_58_so_pt3(p,q,i,j)
term(405) = term(405) + wm_interm_57_so_pt3(i,j) * wm_interm_58_so_pt3(p,q,i,j)
term(406) = term(406) + wm_interm_55_so_pt3(i,j) * wm_interm_59_so_pt3(p,i,q,j)
term(407) = term(407) + wm_interm_55_so_pt3(i,j) * wm_interm_59_so_pt3(p,q,i,j)
term(408) = term(408) + wm_interm_57_so_pt3(i,j) * wm_interm_59_so_pt3(p,i,q,j)
term(409) = term(409) + wm_interm_57_so_pt3(i,j) * wm_interm_59_so_pt3(p,q,i,j)
term(410) = term(410) + wm_interm_55_so_pt3(i,j) * wm_interm_60_so_pt3(p,q,i,j)
term(411) = term(411) + wm_interm_57_so_pt3(i,j) * wm_interm_60_so_pt3(p,q,i,j)
term(412) = term(412) + wm_interm_71_so_pt3(p,i,q,j) * wm_interm_93_so_pt3(j,i)
term(413) = term(413) + wm_interm_90_so_pt3(i,j) * wm_interm_98_so_pt3(p,i,q,j)
term(414) = term(414) + r1(vrdav_Rr, p,i) * wm_interm_105_so_pt3(i,j) * wm_interm_57_so_pt3(j,q)
term(415) = term(415) + r1(vrdav_Rr, p,i) * wm_interm_106_so_pt3(i,j) * wm_interm_57_so_pt3(j,q)
term(416) = term(416) + r1(vrdav_Rr, p,i) * wm_interm_105_so_pt3(i,j) * wm_interm_55_so_pt3(j,q)
term(417) = term(417) + r1(vrdav_Rr, p,i) * wm_interm_106_so_pt3(i,j) * wm_interm_55_so_pt3(j,q)
term(418) = term(418) + r1(vrdav_Rr, p,i) * wm_interm_105_so_pt3(j,q) * wm_interm_55_so_pt3(i,j)
term(419) = term(419) + r1(vrdav_Rr, p,i) * wm_interm_106_so_pt3(j,q) * wm_interm_55_so_pt3(i,j)
term(420) = term(420) + r1(vrdav_Rr, p,i) * wm_interm_105_so_pt3(j,q) * wm_interm_57_so_pt3(i,j)
term(421) = term(421) + r1(vrdav_Rr, p,i) * wm_interm_106_so_pt3(j,q) * wm_interm_57_so_pt3(i,j)
end do 
end do 

term(400) = term(400) * (-1.0d+0) 
term(401) = term(401) * (2.0d+0) 
term(402) = term(402) * (2.0d+0) 
term(403) = term(403) * (-4.0d+0) 
term(404) = term(404) * (-1.0d+0) 
term(405) = term(405) * (2.0d+0) 
term(406) = term(406) * (-2.0d+0) 
term(407) = term(407) * (4.0d+0) 
term(408) = term(408) * (4.0d+0) 
term(409) = term(409) * (-8.0d+0) 
term(410) = term(410) * (-2.0d+0) 
term(411) = term(411) * (4.0d+0) 
term(412) = term(412) * (2.0d+0) 
term(413) = term(413) * (2.0d+0) 
term(414) = term(414) * (4.0d+0) 
term(415) = term(415) * (-8.0d+0) 
term(416) = term(416) * (-2.0d+0) 
term(417) = term(417) * (4.0d+0) 
term(418) = term(418) * (-2.0d+0) 
term(419) = term(419) * (4.0d+0) 
term(420) = term(420) * (4.0d+0) 
term(421) = term(421) * (-8.0d+0) 

do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(422) = term(422) + r2m(vrdav_Rr, a,i,p,q) * s1(b,i) * wm_interm_53_so_pt3(a,b)
term(423) = term(423) + r2m(vrdav_Rr, a,i,p,q) * s1(b,i) * wm_interm_54_so_pt3(a,b)
term(424) = term(424) + t2(a,b,i,q) * wm_interm_107_so_pt3(a,p) * wm_interm_91_so_pt3(b,i)
term(425) = term(425) + t2(a,b,i,q) * wm_interm_108_so_pt3(a,p) * wm_interm_91_so_pt3(b,i)
term(426) = term(426) + t2(a,b,i,q) * wm_interm_54_so_pt3(a,p) * wm_interm_92_so_pt3(b,i)
term(427) = term(427) + t2(a,b,i,q) * wm_interm_53_so_pt3(a,p) * wm_interm_92_so_pt3(b,i)
end do 
end do 
end do 

term(422) = term(422) * (-8.0d+0) 
term(423) = term(423) * (4.0d+0) 
term(424) = term(424) * (-4.0d+0) 
term(425) = term(425) * (8.0d+0) 
term(426) = term(426) * (-4.0d+0) 
term(427) = term(427) * (8.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(428) = term(428) + t2(a,p,j,i) * wm_interm_70_so_pt3(i,j,k,l) * wm_interm_94_so_pt3(a,q,l,k)
term(429) = term(429) + t2(a,p,j,i) * wm_interm_70_so_pt3(j,i,k,l) * wm_interm_94_so_pt3(a,q,l,k)
term(430) = term(430) + t2(a,p,q,i) * wm_interm_70_so_pt3(j,i,k,l) * wm_interm_94_so_pt3(a,j,l,k)
end do 
end do 
end do 
end do 
end do 

term(428) = term(428) * (-1.0d+0) 
term(429) = term(429) * (2.0d+0) 
term(430) = term(430) * (-1.0d+0) 

do i = 1, nocc 
term(431) = term(431) + wm_interm_55_so_pt3(i,q) * wm_interm_63_so_pt3(p,i)
term(432) = term(432) + wm_interm_57_so_pt3(i,q) * wm_interm_63_so_pt3(p,i)
term(433) = term(433) + wm_interm_55_so_pt3(i,q) * wm_interm_64_so_pt3(p,i)
term(434) = term(434) + wm_interm_57_so_pt3(i,q) * wm_interm_64_so_pt3(p,i)
term(435) = term(435) + wm_interm_55_so_pt3(i,q) * wm_interm_65_so_pt3(p,i)
term(436) = term(436) + wm_interm_57_so_pt3(i,q) * wm_interm_65_so_pt3(p,i)
term(437) = term(437) + wm_interm_55_so_pt3(i,q) * wm_interm_67_so_pt3(p,i)
term(438) = term(438) + wm_interm_57_so_pt3(i,q) * wm_interm_67_so_pt3(p,i)
term(439) = term(439) + wm_interm_55_so_pt3(i,q) * wm_interm_68_so_pt3(p,i)
term(440) = term(440) + wm_interm_57_so_pt3(i,q) * wm_interm_68_so_pt3(p,i)
term(441) = term(441) + wm_interm_13_so_pt3(i,q) * wm_interm_52_so_pt3(p,i)
term(442) = term(442) + wm_interm_15_so_pt3(i,q) * wm_interm_52_so_pt3(p,i)
term(443) = term(443) + wm_interm_11_so_pt3(i,q) * wm_interm_52_so_pt3(p,i)
term(444) = term(444) + wm_interm_13_so_pt3(i,q) * wm_interm_51_so_pt3(p,i)
term(445) = term(445) + wm_interm_15_so_pt3(i,q) * wm_interm_51_so_pt3(p,i)
term(446) = term(446) + wm_interm_11_so_pt3(i,q) * wm_interm_51_so_pt3(p,i)
term(447) = term(447) + wm_interm_38_so_pt3(i,q) * wm_interm_52_so_pt3(p,i)
term(448) = term(448) + wm_interm_40_so_pt3(i,q) * wm_interm_52_so_pt3(p,i)
term(449) = term(449) + wm_interm_38_so_pt3(i,q) * wm_interm_51_so_pt3(p,i)
term(450) = term(450) + wm_interm_40_so_pt3(i,q) * wm_interm_51_so_pt3(p,i)
term(451) = term(451) + wm_interm_52_so_pt3(p,i) * wm_interm_93_so_pt3(i,q)
term(452) = term(452) + wm_interm_51_so_pt3(p,i) * wm_interm_93_so_pt3(i,q)
term(453) = term(453) + wm_interm_90_so_pt3(q,i) * wm_interm_95_so_pt3(p,i)
term(454) = term(454) + wm_interm_90_so_pt3(q,i) * wm_interm_97_so_pt3(p,i)
end do 

term(431) = term(431) * (-2.0d+0) 
term(432) = term(432) * (4.0d+0) 
term(434) = term(434) * (-2.0d+0) 
term(436) = term(436) * (-2.0d+0) 
term(437) = term(437) * (-4.0d+0) 
term(438) = term(438) * (8.0d+0) 
term(439) = term(439) * (4.0d+0) 
term(440) = term(440) * (-8.0d+0) 
term(441) = term(441) * (-1.0d+0) 
term(442) = term(442) * (2.0d+0) 
term(443) = term(443) * (-1.0d+0) 
term(444) = term(444) * (2.0d+0) 
term(445) = term(445) * (-4.0d+0) 
term(446) = term(446) * (2.0d+0) 
term(447) = term(447) * (-4.0d+0) 
term(448) = term(448) * (4.0d+0) 
term(449) = term(449) * (8.0d+0) 
term(450) = term(450) * (-8.0d+0) 
term(451) = term(451) * (2.0d+0) 
term(452) = term(452) * (-4.0d+0) 
term(453) = term(453) * (2.0d+0) 
term(454) = term(454) * (-4.0d+0) 


    calc_D_vo_wm_so_pt3 = zero
    do s = 0, 454
    calc_D_vo_wm_so_pt3 = calc_D_vo_wm_so_pt3 + term(s)
    end do

    end function calc_D_vo_wm_so_pt3
    
    function calc_D_vv_wm_so_pt3(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vv_wm_so_pt3
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
    real(F64), dimension(0:53) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(0) = term(0) + wm_interm_1_so_pt3(q,i,j,k) * wm_interm_76_so_pt3(p,k,j,i)
term(1) = term(1) + wm_interm_1_so_pt3(q,i,j,k) * wm_interm_79_so_pt3(p,k,j,i)
term(2) = term(2) + wm_interm_71_so_pt3(p,i,j,k) * wm_interm_88_so_pt3(q,j,k,i)
term(3) = term(3) + wm_interm_71_so_pt3(p,i,j,k) * wm_interm_86_so_pt3(q,k,j,i)
end do 
end do 
end do 

term(1) = term(1) * (-4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(4) = term(4) + wm_interm_1_so_pt3(q,i,j,k) * wm_interm_76_so_pt3(p,k,i,j)
term(5) = term(5) + wm_interm_1_so_pt3(q,i,j,k) * wm_interm_77_so_pt3(p,k,i,j)
term(6) = term(6) + wm_interm_1_so_pt3(q,i,j,k) * wm_interm_79_so_pt3(p,k,i,j)
end do 
end do 
end do 

term(4) = term(4) * (-2.0d+0) 
term(6) = term(6) * (4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(7) = term(7) + wm_interm_29_so_pt3(a,q,i,j) * wm_interm_83_so_pt3(a,p,i,j)
term(8) = term(8) + wm_interm_25_so_pt3(a,q,i,j) * wm_interm_83_so_pt3(a,p,i,j)
term(9) = term(9) + wm_interm_24_so_pt3(a,q,i,j) * wm_interm_83_so_pt3(a,p,i,j)
term(10) = term(10) + wm_interm_48_so_pt3(a,q,i,j) * wm_interm_83_so_pt3(a,p,i,j)
term(11) = term(11) + wm_interm_47_so_pt3(a,q,i,j) * wm_interm_83_so_pt3(a,p,i,j)
term(12) = term(12) + wm_interm_74_so_pt3(q,a,i,j) * wm_interm_87_so_pt3(p,a,i,j)
term(13) = term(13) + wm_interm_73_so_pt3(q,a,i,j) * wm_interm_87_so_pt3(p,a,i,j)
term(14) = term(14) + wm_interm_66_so_pt3(q,a,i,j) * wm_interm_87_so_pt3(p,a,i,j)
term(15) = term(15) + wm_interm_72_so_pt3(q,a,i,j) * wm_interm_87_so_pt3(p,a,i,j)
term(16) = term(16) + wm_interm_66_so_pt3(a,p,i,j) * wm_interm_87_so_pt3(a,q,i,j)
end do 
end do 
end do 

term(8) = term(8) * (-2.0d+0) 
term(10) = term(10) * (4.0d+0) 
term(11) = term(11) * (-4.0d+0) 
term(12) = term(12) * (8.0d+0) 
term(13) = term(13) * (-4.0d+0) 
term(14) = term(14) * (2.0d+0) 
term(15) = term(15) * (-4.0d+0) 
term(16) = term(16) * (2.0d+0) 

do a = nocc + 1, nactive 
term(17) = term(17) + wm_interm_5_so_pt3(p,a) * wm_interm_75_so_pt3(q,a)
term(18) = term(18) + wm_interm_6_so_pt3(p,a) * wm_interm_75_so_pt3(q,a)
term(19) = term(19) + wm_interm_75_so_pt3(q,a) * wm_interm_9_so_pt3(p,a)
term(20) = term(20) + wm_interm_34_so_pt3(p,a) * wm_interm_75_so_pt3(q,a)
term(21) = term(21) + wm_interm_35_so_pt3(p,a) * wm_interm_75_so_pt3(q,a)
term(22) = term(22) + wm_interm_75_so_pt3(a,p) * wm_interm_9_so_pt3(a,q)
term(23) = term(23) + wm_interm_5_so_pt3(a,q) * wm_interm_75_so_pt3(a,p)
term(24) = term(24) + wm_interm_6_so_pt3(a,q) * wm_interm_75_so_pt3(a,p)
term(25) = term(25) + wm_interm_34_so_pt3(a,q) * wm_interm_75_so_pt3(a,p)
term(26) = term(26) + wm_interm_35_so_pt3(a,q) * wm_interm_75_so_pt3(a,p)
term(27) = term(27) + wm_interm_53_so_pt3(q,a) * wm_interm_89_so_pt3(p,a)
term(28) = term(28) + wm_interm_54_so_pt3(q,a) * wm_interm_89_so_pt3(p,a)
term(29) = term(29) + wm_interm_54_so_pt3(a,p) * wm_interm_89_so_pt3(a,q)
term(30) = term(30) + wm_interm_53_so_pt3(a,p) * wm_interm_89_so_pt3(a,q)
end do 

term(18) = term(18) * (-2.0d+0) 
term(20) = term(20) * (4.0d+0) 
term(21) = term(21) * (-4.0d+0) 
term(24) = term(24) * (-2.0d+0) 
term(25) = term(25) * (4.0d+0) 
term(26) = term(26) * (-4.0d+0) 
term(27) = term(27) * (-4.0d+0) 
term(28) = term(28) * (2.0d+0) 
term(29) = term(29) * (2.0d+0) 
term(30) = term(30) * (-4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
term(31) = term(31) + wm_interm_21_so_pt3(p,q,i,j) * wm_interm_78_so_pt3(i,j)
term(32) = term(32) + wm_interm_22_so_pt3(p,q,i,j) * wm_interm_78_so_pt3(i,j)
term(33) = term(33) + wm_interm_23_so_pt3(p,q,i,j) * wm_interm_78_so_pt3(i,j)
term(34) = term(34) + wm_interm_24_so_pt3(p,q,i,j) * wm_interm_78_so_pt3(i,j)
term(35) = term(35) + wm_interm_25_so_pt3(p,q,i,j) * wm_interm_78_so_pt3(i,j)
term(36) = term(36) + wm_interm_26_so_pt3(p,q,i,j) * wm_interm_78_so_pt3(i,j)
term(37) = term(37) + wm_interm_45_so_pt3(p,q,i,j) * wm_interm_78_so_pt3(i,j)
term(38) = term(38) + wm_interm_46_so_pt3(p,q,i,j) * wm_interm_78_so_pt3(i,j)
term(39) = term(39) + wm_interm_47_so_pt3(p,q,i,j) * wm_interm_78_so_pt3(i,j)
term(40) = term(40) + wm_interm_48_so_pt3(p,q,i,j) * wm_interm_78_so_pt3(i,j)
term(41) = term(41) + wm_interm_17_so_pt3(i,j) * wm_interm_83_so_pt3(q,p,i,j)
term(42) = term(42) + wm_interm_18_so_pt3(i,j) * wm_interm_83_so_pt3(q,p,i,j)
term(43) = term(43) + wm_interm_19_so_pt3(i,j) * wm_interm_83_so_pt3(q,p,i,j)
term(44) = term(44) + wm_interm_41_so_pt3(i,j) * wm_interm_83_so_pt3(q,p,i,j)
term(45) = term(45) + wm_interm_42_so_pt3(i,j) * wm_interm_83_so_pt3(q,p,i,j)
term(46) = term(46) + wm_interm_43_so_pt3(i,j) * wm_interm_83_so_pt3(q,p,i,j)
term(47) = term(47) + wm_interm_55_so_pt3(i,j) * wm_interm_87_so_pt3(p,q,i,j)
term(48) = term(48) + wm_interm_57_so_pt3(i,j) * wm_interm_87_so_pt3(p,q,i,j)
term(49) = term(49) + wm_interm_72_so_pt3(q,p,i,j) * wm_interm_90_so_pt3(i,j)
term(50) = term(50) + wm_interm_73_so_pt3(q,p,i,j) * wm_interm_90_so_pt3(i,j)
term(51) = term(51) + wm_interm_74_so_pt3(q,p,i,j) * wm_interm_90_so_pt3(i,j)
end do 
end do 

term(32) = term(32) * (-2.0d+0) 
term(34) = term(34) * (-2.0d+0) 
term(37) = term(37) * (4.0d+0) 
term(38) = term(38) * (-4.0d+0) 
term(39) = term(39) * (4.0d+0) 
term(40) = term(40) * (-4.0d+0) 
term(42) = term(42) * (-2.0d+0) 
term(44) = term(44) * (2.0d+0) 
term(45) = term(45) * (-4.0d+0) 
term(46) = term(46) * (2.0d+0) 
term(47) = term(47) * (2.0d+0) 
term(48) = term(48) * (-4.0d+0) 
term(49) = term(49) * (2.0d+0) 
term(50) = term(50) * (2.0d+0) 
term(51) = term(51) * (-4.0d+0) 

do i = 1, nocc 
term(52) = term(52) + wm_interm_52_so_pt3(p,i) * wm_interm_91_so_pt3(q,i)
term(53) = term(53) + wm_interm_51_so_pt3(p,i) * wm_interm_91_so_pt3(q,i)
end do 

term(52) = term(52) * (2.0d+0) 
term(53) = term(53) * (-4.0d+0) 


    calc_D_vv_wm_so_pt3 = zero
    do s = 0, 53
    calc_D_vv_wm_so_pt3 = calc_D_vv_wm_so_pt3 + term(s)
    end do

    end function calc_D_vv_wm_so_pt3
    
    
  end module so_ccsd_pt3
