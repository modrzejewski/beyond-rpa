module density_exc_exc_functions_triplet_pt4
      

    use ccsd_transformed_integrals
    use cc3_intermediates
    use arithmetic
    use s_gen
    use basis
    use eom_vectors
    
    implicit none
    !
    ! File generated automatically on 2016-08-16 08:17:16
    !
    real(F64), dimension(:, :), allocatable :: wm_interm_0_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_1_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_2_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_3_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_4_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_5_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_6_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_7_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_8_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_9_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_10_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_11_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_12_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_13_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_14_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_15_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_16_triplet_pt4 
real(F64) :: wm_interm_17_triplet_pt4 
real(F64) :: wm_interm_18_triplet_pt4 
real(F64) :: wm_interm_19_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_20_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_21_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_22_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_23_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_24_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_25_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_26_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_27_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_28_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_29_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_30_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_31_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_32_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_33_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_34_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_35_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_36_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_37_triplet_pt4 
real(F64) :: wm_interm_38_triplet_pt4 
real(F64) :: wm_interm_39_triplet_pt4 
real(F64) :: wm_interm_40_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_41_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_42_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_43_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_44_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_45_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_46_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_47_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_48_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_49_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_50_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_51_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_52_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_53_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_54_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_55_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_56_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_57_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_58_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_59_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_60_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_61_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_62_triplet_pt4 
real(F64) :: wm_interm_63_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_64_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_65_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_66_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_67_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_68_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_69_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_70_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_71_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_72_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_73_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_74_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_75_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_76_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_77_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_78_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_79_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_80_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_81_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_82_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_83_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_84_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_85_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_86_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_87_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_88_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_89_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_90_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_91_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_92_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_93_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_94_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_95_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_96_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_97_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_98_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_99_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_100_triplet_pt4 

    contains
    
    subroutine wm_triplet_intermediates_ccsd_init_pt4(nocc, nactive)
    integer, intent(in) :: nocc
    integer, intent(in) :: nactive
    allocate(wm_interm_0_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_1_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_2_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_3_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_4_triplet_pt4(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_5_triplet_pt4(1: nocc, 1: nocc))
allocate(wm_interm_6_triplet_pt4(1: nocc, 1: nocc))
allocate(wm_interm_7_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_8_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_9_triplet_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_10_triplet_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_11_triplet_pt4(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_12_triplet_pt4(1: nocc, 1: nocc))
allocate(wm_interm_13_triplet_pt4(1: nocc, 1: nocc))
allocate(wm_interm_14_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_15_triplet_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_16_triplet_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_20_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_21_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_22_triplet_pt4(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_23_triplet_pt4(1: nocc, 1: nocc))
allocate(wm_interm_24_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_25_triplet_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_26_triplet_pt4(1: nocc, 1: nocc))
allocate(wm_interm_27_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_28_triplet_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_29_triplet_pt4(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_30_triplet_pt4(1: nocc, 1: nocc))
allocate(wm_interm_31_triplet_pt4(1: nocc, 1: nocc))
allocate(wm_interm_32_triplet_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_33_triplet_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_34_triplet_pt4(1: nocc, 1: nocc))
allocate(wm_interm_35_triplet_pt4(1: nocc, 1: nocc))
allocate(wm_interm_36_triplet_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_37_triplet_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_41_triplet_pt4(1: nocc, 1: nocc))
allocate(wm_interm_42_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_43_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_44_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_45_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_46_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_47_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_48_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_49_triplet_pt4(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_50_triplet_pt4(1: nocc, 1: nocc))
allocate(wm_interm_51_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_52_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_53_triplet_pt4(1: nocc, 1: nocc))
allocate(wm_interm_54_triplet_pt4(1: nocc, 1: nocc))
allocate(wm_interm_55_triplet_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_56_triplet_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_57_triplet_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_58_triplet_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_59_triplet_pt4(1: nocc, 1: nocc))
allocate(wm_interm_60_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_61_triplet_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_62_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_64_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_65_triplet_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_66_triplet_pt4(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_67_triplet_pt4(1: nocc, 1: nocc))
allocate(wm_interm_68_triplet_pt4(1: nocc, 1: nocc))
allocate(wm_interm_69_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_70_triplet_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_71_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_72_triplet_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_73_triplet_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_74_triplet_pt4(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_75_triplet_pt4(1: nocc, 1: nocc))
allocate(wm_interm_76_triplet_pt4(1: nocc, 1: nocc))
allocate(wm_interm_77_triplet_pt4(1: nocc, 1: nocc))
allocate(wm_interm_78_triplet_pt4(1: nocc, 1: nocc))
allocate(wm_interm_79_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_80_triplet_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_81_triplet_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_82_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_83_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_84_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_85_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_86_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_87_triplet_pt4(1: nocc, 1: nocc))
allocate(wm_interm_88_triplet_pt4(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_89_triplet_pt4(1: nocc, 1: nocc))
allocate(wm_interm_90_triplet_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_91_triplet_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_92_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_93_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_94_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_95_triplet_pt4(1: nocc, 1: nocc))
allocate(wm_interm_96_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_97_triplet_pt4(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_98_triplet_pt4(1: nocc, 1: nocc))
allocate(wm_interm_99_triplet_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_100_triplet_pt4(nocc+1: nactive, nocc+1: nactive))
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
deallocate(wm_interm_20_triplet_pt4)
deallocate(wm_interm_21_triplet_pt4)
deallocate(wm_interm_22_triplet_pt4)
deallocate(wm_interm_23_triplet_pt4)
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

    end subroutine wm_triplet_intermediates_ccsd_free_pt4
    
    subroutine wm_triplet_intermediates_ccsd_pt4(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr)
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    double precision, dimension(:), intent(in) :: vrdav_Rl
    double precision, dimension(:), intent(in) :: vrdav_Rr
    real(F64) :: sum
    integer :: a, i, b, j, k, l, c 

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
wm_interm_0_triplet_pt4(b, j) = wm_interm_0_triplet_pt4(b, j) + sum 
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
wm_interm_1_triplet_pt4(b, j) = wm_interm_1_triplet_pt4(b, j) + sum 
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
wm_interm_2_triplet_pt4(b, j) = wm_interm_2_triplet_pt4(b, j) + sum 
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
wm_interm_3_triplet_pt4(b, j) = wm_interm_3_triplet_pt4(b, j) + sum 
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
wm_interm_4_triplet_pt4(i, j, k, l) = wm_interm_4_triplet_pt4(i, j, k, l) + sum 
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
wm_interm_5_triplet_pt4(i, j) = wm_interm_5_triplet_pt4(i, j) + sum 
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
wm_interm_6_triplet_pt4(j, k) = wm_interm_6_triplet_pt4(j, k) + sum 
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
wm_interm_7_triplet_pt4(b, i, j, k) = wm_interm_7_triplet_pt4(b, i, j, k) + sum 
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
wm_interm_8_triplet_pt4(b, i, j, k) = wm_interm_8_triplet_pt4(b, i, j, k) + sum 
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
sum = sum + r2p(vrdav_Rl, b,i,a,j) * r2p(vrdav_Rr, c,i,a,j)
end do 
end do 
end do 
wm_interm_9_triplet_pt4(b, c) = wm_interm_9_triplet_pt4(b, c) + sum 
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
wm_interm_10_triplet_pt4(a, b) = wm_interm_10_triplet_pt4(a, b) + sum 
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
wm_interm_11_triplet_pt4(i, j, k, l) = wm_interm_11_triplet_pt4(i, j, k, l) + sum 
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
sum = sum + r2p(vrdav_Rl, a,j,b,i) * r2m(vrdav_Rr, a,k,b,i)
end do 
end do 
end do 
wm_interm_12_triplet_pt4(j, k) = wm_interm_12_triplet_pt4(j, k) + sum 
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
wm_interm_13_triplet_pt4(j, k) = wm_interm_13_triplet_pt4(j, k) + sum 
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
wm_interm_14_triplet_pt4(b, j, i, k) = wm_interm_14_triplet_pt4(b, j, i, k) + sum 
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
sum = sum + r2p(vrdav_Rl, b,i,a,j) * r2m(vrdav_Rr, a,j,c,i)
end do 
end do 
end do 
wm_interm_15_triplet_pt4(b, c) = wm_interm_15_triplet_pt4(b, c) + sum 
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
wm_interm_16_triplet_pt4(b, c) = wm_interm_16_triplet_pt4(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

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
wm_interm_17_triplet_pt4 = wm_interm_17_triplet_pt4 + sum 
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
wm_interm_18_triplet_pt4 = wm_interm_18_triplet_pt4 + sum 
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
wm_interm_19_triplet_pt4 = wm_interm_19_triplet_pt4 + sum 
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
wm_interm_20_triplet_pt4(b, j) = wm_interm_20_triplet_pt4(b, j) + sum 
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
wm_interm_21_triplet_pt4(b, j) = wm_interm_21_triplet_pt4(b, j) + sum 
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
wm_interm_22_triplet_pt4(i, j, k, l) = wm_interm_22_triplet_pt4(i, j, k, l) + sum 
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
wm_interm_23_triplet_pt4(j, k) = wm_interm_23_triplet_pt4(j, k) + sum 
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
wm_interm_24_triplet_pt4(b, j, i, k) = wm_interm_24_triplet_pt4(b, j, i, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, c,i,a,j)
end do 
end do 
end do 
wm_interm_25_triplet_pt4(b, c) = wm_interm_25_triplet_pt4(b, c) + sum 
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
wm_interm_26_triplet_pt4(j, k) = wm_interm_26_triplet_pt4(j, k) + sum 
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
wm_interm_27_triplet_pt4(b, i, j, k) = wm_interm_27_triplet_pt4(b, i, j, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,j,b,i) * r2p(vrdav_Rr, c,i,a,j)
end do 
end do 
end do 
wm_interm_28_triplet_pt4(b, c) = wm_interm_28_triplet_pt4(b, c) + sum 
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
wm_interm_29_triplet_pt4(i, j, k, l) = wm_interm_29_triplet_pt4(i, j, k, l) + sum 
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
wm_interm_30_triplet_pt4(j, k) = wm_interm_30_triplet_pt4(j, k) + sum 
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
wm_interm_31_triplet_pt4(j, k) = wm_interm_31_triplet_pt4(j, k) + sum 
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
wm_interm_32_triplet_pt4(b, c) = wm_interm_32_triplet_pt4(b, c) + sum 
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
wm_interm_33_triplet_pt4(b, c) = wm_interm_33_triplet_pt4(b, c) + sum 
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
wm_interm_34_triplet_pt4(j, k) = wm_interm_34_triplet_pt4(j, k) + sum 
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
wm_interm_35_triplet_pt4(j, k) = wm_interm_35_triplet_pt4(j, k) + sum 
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
wm_interm_36_triplet_pt4(b, c) = wm_interm_36_triplet_pt4(b, c) + sum 
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
wm_interm_37_triplet_pt4(b, c) = wm_interm_37_triplet_pt4(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

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
wm_interm_38_triplet_pt4 = wm_interm_38_triplet_pt4 + sum 
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
wm_interm_39_triplet_pt4 = wm_interm_39_triplet_pt4 + sum 
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
wm_interm_40_triplet_pt4 = wm_interm_40_triplet_pt4 + sum 
!$omp parallel private(a, i, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do i = 1, nocc 
do j = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * r1(vrdav_Rr, a,j)
end do 
wm_interm_41_triplet_pt4(i, j) = wm_interm_41_triplet_pt4(i, j) + sum 
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
wm_interm_42_triplet_pt4(b, j) = wm_interm_42_triplet_pt4(b, j) + sum 
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
wm_interm_43_triplet_pt4(b, i, j, k) = wm_interm_43_triplet_pt4(b, i, j, k) + sum 
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
wm_interm_44_triplet_pt4(b, j) = wm_interm_44_triplet_pt4(b, j) + sum 
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
wm_interm_45_triplet_pt4(b, i, j, k) = wm_interm_45_triplet_pt4(b, i, j, k) + sum 
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
wm_interm_46_triplet_pt4(b, j) = wm_interm_46_triplet_pt4(b, j) + sum 
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
wm_interm_47_triplet_pt4(b, j) = wm_interm_47_triplet_pt4(b, j) + sum 
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
wm_interm_48_triplet_pt4(b, j) = wm_interm_48_triplet_pt4(b, j) + sum 
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
wm_interm_49_triplet_pt4(i, j, k, l) = wm_interm_49_triplet_pt4(i, j, k, l) + sum 
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
wm_interm_50_triplet_pt4(i, j) = wm_interm_50_triplet_pt4(i, j) + sum 
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
wm_interm_51_triplet_pt4(b, i, j, k) = wm_interm_51_triplet_pt4(b, i, j, k) + sum 
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
wm_interm_52_triplet_pt4(b, j) = wm_interm_52_triplet_pt4(b, j) + sum 
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
wm_interm_53_triplet_pt4(j, k) = wm_interm_53_triplet_pt4(j, k) + sum 
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
wm_interm_54_triplet_pt4(j, k) = wm_interm_54_triplet_pt4(j, k) + sum 
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
wm_interm_55_triplet_pt4(a, b) = wm_interm_55_triplet_pt4(a, b) + sum 
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
wm_interm_56_triplet_pt4(b, c) = wm_interm_56_triplet_pt4(b, c) + sum 
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
wm_interm_57_triplet_pt4(a, b) = wm_interm_57_triplet_pt4(a, b) + sum 
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
wm_interm_58_triplet_pt4(b, c) = wm_interm_58_triplet_pt4(b, c) + sum 
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
wm_interm_59_triplet_pt4(i, j) = wm_interm_59_triplet_pt4(i, j) + sum 
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
wm_interm_60_triplet_pt4(b, j, i, k) = wm_interm_60_triplet_pt4(b, j, i, k) + sum 
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
wm_interm_61_triplet_pt4(a, b) = wm_interm_61_triplet_pt4(a, b) + sum 
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
wm_interm_62_triplet_pt4(b, i, j, k) = wm_interm_62_triplet_pt4(b, i, j, k) + sum 
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
wm_interm_63_triplet_pt4 = wm_interm_63_triplet_pt4 + sum 
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
wm_interm_64_triplet_pt4(b, i, j, k) = wm_interm_64_triplet_pt4(b, i, j, k) + sum 
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
wm_interm_65_triplet_pt4(b, c) = wm_interm_65_triplet_pt4(b, c) + sum 
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
wm_interm_66_triplet_pt4(i, j, k, l) = wm_interm_66_triplet_pt4(i, j, k, l) + sum 
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
sum = sum + r2p(vrdav_Rr, a,j,b,i) * s2(a,b,i,k)
end do 
end do 
end do 
wm_interm_67_triplet_pt4(j, k) = wm_interm_67_triplet_pt4(j, k) + sum 
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
wm_interm_68_triplet_pt4(j, k) = wm_interm_68_triplet_pt4(j, k) + sum 
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
wm_interm_69_triplet_pt4(b, i, k, j) = wm_interm_69_triplet_pt4(b, i, k, j) + sum 
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
wm_interm_70_triplet_pt4(b, c) = wm_interm_70_triplet_pt4(b, c) + sum 
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
wm_interm_71_triplet_pt4(b, i, j, k) = wm_interm_71_triplet_pt4(b, i, j, k) + sum 
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
wm_interm_72_triplet_pt4(b, c) = wm_interm_72_triplet_pt4(b, c) + sum 
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
wm_interm_73_triplet_pt4(b, c) = wm_interm_73_triplet_pt4(b, c) + sum 
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
wm_interm_74_triplet_pt4(i, j, k, l) = wm_interm_74_triplet_pt4(i, j, k, l) + sum 
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
sum = sum + r2m(vrdav_Rr, a,j,b,i) * s2(a,b,i,k)
end do 
end do 
end do 
wm_interm_75_triplet_pt4(j, k) = wm_interm_75_triplet_pt4(j, k) + sum 
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
sum = sum + r2m(vrdav_Rr, a,i,b,j) * s2(a,b,i,k)
end do 
end do 
end do 
wm_interm_76_triplet_pt4(j, k) = wm_interm_76_triplet_pt4(j, k) + sum 
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
wm_interm_77_triplet_pt4(j, k) = wm_interm_77_triplet_pt4(j, k) + sum 
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
wm_interm_78_triplet_pt4(j, k) = wm_interm_78_triplet_pt4(j, k) + sum 
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
wm_interm_79_triplet_pt4(b, i, j, k) = wm_interm_79_triplet_pt4(b, i, j, k) + sum 
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
wm_interm_80_triplet_pt4(b, c) = wm_interm_80_triplet_pt4(b, c) + sum 
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
sum = sum + r2m(vrdav_Rr, a,i,b,j) * s2(a,c,i,j)
end do 
end do 
end do 
wm_interm_81_triplet_pt4(b, c) = wm_interm_81_triplet_pt4(b, c) + sum 
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
wm_interm_82_triplet_pt4(b, j) = wm_interm_82_triplet_pt4(b, j) + sum 
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
wm_interm_83_triplet_pt4(b, j) = wm_interm_83_triplet_pt4(b, j) + sum 
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
wm_interm_84_triplet_pt4(b, j) = wm_interm_84_triplet_pt4(b, j) + sum 
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
wm_interm_85_triplet_pt4(b, i, j, k) = wm_interm_85_triplet_pt4(b, i, j, k) + sum 
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
sum = sum + r2p(vrdav_Rl, b,j,a,i) * r1(vrdav_Rr, a,i)
end do 
end do 
wm_interm_86_triplet_pt4(b, j) = wm_interm_86_triplet_pt4(b, j) + sum 
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
wm_interm_87_triplet_pt4(j, k) = wm_interm_87_triplet_pt4(j, k) + sum 
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
wm_interm_88_triplet_pt4(i, j, k, l) = wm_interm_88_triplet_pt4(i, j, k, l) + sum 
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
wm_interm_89_triplet_pt4(j, k) = wm_interm_89_triplet_pt4(j, k) + sum 
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
wm_interm_90_triplet_pt4(b, c) = wm_interm_90_triplet_pt4(b, c) + sum 
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
wm_interm_91_triplet_pt4(b, c) = wm_interm_91_triplet_pt4(b, c) + sum 
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
wm_interm_92_triplet_pt4(b, i, j, k) = wm_interm_92_triplet_pt4(b, i, j, k) + sum 
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
wm_interm_93_triplet_pt4(b, j) = wm_interm_93_triplet_pt4(b, j) + sum 
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
wm_interm_94_triplet_pt4(b, j, i, k) = wm_interm_94_triplet_pt4(b, j, i, k) + sum 
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
wm_interm_95_triplet_pt4(j, k) = wm_interm_95_triplet_pt4(j, k) + sum 
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
wm_interm_96_triplet_pt4(b, j) = wm_interm_96_triplet_pt4(b, j) + sum 
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
wm_interm_97_triplet_pt4(i, j, k, l) = wm_interm_97_triplet_pt4(i, j, k, l) + sum 
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
sum = sum + r2m(vrdav_Rl, a,j,b,i) * t2(a,b,k,i)
end do 
end do 
end do 
wm_interm_98_triplet_pt4(j, k) = wm_interm_98_triplet_pt4(j, k) + sum 
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
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rl, a,j,b,i) * t2(a,c,j,i)
end do 
end do 
end do 
wm_interm_100_triplet_pt4(b, c) = wm_interm_100_triplet_pt4(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 



    end subroutine wm_triplet_intermediates_ccsd_pt4
    

    
    
    function calc_D_oo_wm_triplet_pt4(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_oo_wm_triplet_pt4
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    double precision, dimension(:), intent(in) :: vrdav_Rl
    double precision, dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , j, i, a, b, k, l 
    real(F64), dimension(0:1231) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + wm_interm_7_triplet_pt4(a, i, q, j) * wm_interm_8_triplet_pt4(a, i, p, j)
term(1) = term(1) + wm_interm_14_triplet_pt4(a, i, p, j) * wm_interm_7_triplet_pt4(a, i, q, j)
term(2) = term(2) + wm_interm_14_triplet_pt4(a, p, i, j) * wm_interm_7_triplet_pt4(a, i, q, j)
term(3) = term(3) + wm_interm_7_triplet_pt4(a, i, j, q) * wm_interm_8_triplet_pt4(a, i, j, p)
term(4) = term(4) + wm_interm_14_triplet_pt4(a, i, j, p) * wm_interm_7_triplet_pt4(a, i, j, q)
term(5) = term(5) + wm_interm_14_triplet_pt4(a, i, j, p) * wm_interm_7_triplet_pt4(a, j, i, q)
term(6) = term(6) + wm_interm_24_triplet_pt4(a, q, i, j) * wm_interm_8_triplet_pt4(a, i, p, j)
term(7) = term(7) + wm_interm_24_triplet_pt4(a, i, q, j) * wm_interm_8_triplet_pt4(a, i, p, j)
term(8) = term(8) + wm_interm_27_triplet_pt4(a, q, i, j) * wm_interm_8_triplet_pt4(a, i, p, j)
term(9) = term(9) + wm_interm_27_triplet_pt4(a, i, q, j) * wm_interm_8_triplet_pt4(a, i, p, j)
term(10) = term(10) + wm_interm_14_triplet_pt4(a, i, p, j) * wm_interm_24_triplet_pt4(a, q, i, j)
term(11) = term(11) + wm_interm_14_triplet_pt4(a, p, i, j) * wm_interm_24_triplet_pt4(a, q, i, j)
term(12) = term(12) + wm_interm_14_triplet_pt4(a, p, i, j) * wm_interm_24_triplet_pt4(a, i, q, j)
term(13) = term(13) + wm_interm_14_triplet_pt4(a, i, p, j) * wm_interm_24_triplet_pt4(a, i, q, j)
term(14) = term(14) + wm_interm_14_triplet_pt4(a, p, i, j) * wm_interm_27_triplet_pt4(a, q, i, j)
term(15) = term(15) + wm_interm_14_triplet_pt4(a, i, p, j) * wm_interm_27_triplet_pt4(a, q, i, j)
term(16) = term(16) + wm_interm_14_triplet_pt4(a, i, p, j) * wm_interm_27_triplet_pt4(a, i, q, j)
term(17) = term(17) + wm_interm_14_triplet_pt4(a, p, i, j) * wm_interm_27_triplet_pt4(a, i, q, j)
term(18) = term(18) + wm_interm_27_triplet_pt4(a, i, j, q) * wm_interm_8_triplet_pt4(a, i, j, p)
term(19) = term(19) + wm_interm_24_triplet_pt4(a, i, j, q) * wm_interm_8_triplet_pt4(a, i, j, p)
term(20) = term(20) + wm_interm_14_triplet_pt4(a, i, j, p) * wm_interm_27_triplet_pt4(a, i, j, q)
term(21) = term(21) + wm_interm_14_triplet_pt4(a, i, j, p) * wm_interm_27_triplet_pt4(a, j, i, q)
term(22) = term(22) + wm_interm_14_triplet_pt4(a, i, j, p) * wm_interm_24_triplet_pt4(a, j, i, q)
term(23) = term(23) + wm_interm_14_triplet_pt4(a, i, j, p) * wm_interm_24_triplet_pt4(a, i, j, q)
term(24) = term(24) + r1(vrdav_Rl, a,i) * wm_interm_1_triplet_pt4(a, j) * wm_interm_53_triplet_pt4(j, i)
term(25) = term(25) + r1(vrdav_Rl, a,i) * wm_interm_1_triplet_pt4(a, j) * wm_interm_54_triplet_pt4(j, i)
term(26) = term(26) + r1(vrdav_Rl, a,i) * wm_interm_2_triplet_pt4(a, j) * wm_interm_53_triplet_pt4(j, i)
term(27) = term(27) + r1(vrdav_Rl, a,i) * wm_interm_2_triplet_pt4(a, j) * wm_interm_54_triplet_pt4(j, i)
term(28) = term(28) + r1(vrdav_Rl, a,i) * wm_interm_3_triplet_pt4(a, j) * wm_interm_53_triplet_pt4(j, i)
term(29) = term(29) + r1(vrdav_Rl, a,i) * wm_interm_3_triplet_pt4(a, j) * wm_interm_54_triplet_pt4(j, i)
term(30) = term(30) + s1(a,i) * wm_interm_47_triplet_pt4(a, j) * wm_interm_66_triplet_pt4(p, i, q, j)
term(31) = term(31) + s1(a,i) * wm_interm_47_triplet_pt4(a, j) * wm_interm_66_triplet_pt4(p, i, j, q)
term(32) = term(32) + s1(a,i) * wm_interm_47_triplet_pt4(a, j) * wm_interm_67_triplet_pt4(i, j)
term(33) = term(33) + s1(a,i) * wm_interm_47_triplet_pt4(a, j) * wm_interm_68_triplet_pt4(i, j)
term(34) = term(34) + r1(vrdav_Rl, a,i) * wm_interm_42_triplet_pt4(a, j) * wm_interm_66_triplet_pt4(i, p, q, j)
term(35) = term(35) + r1(vrdav_Rl, a,i) * wm_interm_42_triplet_pt4(a, j) * wm_interm_66_triplet_pt4(i, p, j, q)
term(36) = term(36) + r1(vrdav_Rl, a,i) * wm_interm_42_triplet_pt4(a, j) * wm_interm_67_triplet_pt4(i, j)
term(37) = term(37) + r1(vrdav_Rl, a,i) * wm_interm_42_triplet_pt4(a, j) * wm_interm_68_triplet_pt4(i, j)
term(38) = term(38) + r1(vrdav_Rl, a,i) * wm_interm_48_triplet_pt4(a, j) * wm_interm_66_triplet_pt4(i, p, q, j)
term(39) = term(39) + r1(vrdav_Rl, a,i) * wm_interm_48_triplet_pt4(a, j) * wm_interm_66_triplet_pt4(i, p, j, q)
term(40) = term(40) + r1(vrdav_Rl, a,i) * wm_interm_48_triplet_pt4(a, j) * wm_interm_67_triplet_pt4(i, j)
term(41) = term(41) + r1(vrdav_Rl, a,i) * wm_interm_48_triplet_pt4(a, j) * wm_interm_68_triplet_pt4(i, j)
term(42) = term(42) + s1(a,i) * wm_interm_52_triplet_pt4(a, j) * wm_interm_66_triplet_pt4(p, i, q, j)
term(43) = term(43) + s1(a,i) * wm_interm_52_triplet_pt4(a, j) * wm_interm_66_triplet_pt4(p, i, j, q)
term(44) = term(44) + s1(a,i) * wm_interm_52_triplet_pt4(a, j) * wm_interm_67_triplet_pt4(i, j)
term(45) = term(45) + s1(a,i) * wm_interm_52_triplet_pt4(a, j) * wm_interm_68_triplet_pt4(i, j)
term(46) = term(46) + s1(a,i) * wm_interm_47_triplet_pt4(a, j) * wm_interm_74_triplet_pt4(i, p, q, j)
term(47) = term(47) + s1(a,i) * wm_interm_47_triplet_pt4(a, j) * wm_interm_74_triplet_pt4(p, i, q, j)
term(48) = term(48) + s1(a,i) * wm_interm_47_triplet_pt4(a, j) * wm_interm_74_triplet_pt4(p, i, j, q)
term(49) = term(49) + s1(a,i) * wm_interm_47_triplet_pt4(a, j) * wm_interm_74_triplet_pt4(i, p, j, q)
term(50) = term(50) + s1(a,i) * wm_interm_47_triplet_pt4(a, j) * wm_interm_75_triplet_pt4(i, j)
term(51) = term(51) + s1(a,i) * wm_interm_47_triplet_pt4(a, j) * wm_interm_76_triplet_pt4(i, j)
term(52) = term(52) + s1(a,i) * wm_interm_47_triplet_pt4(a, j) * wm_interm_77_triplet_pt4(i, j)
term(53) = term(53) + s1(a,i) * wm_interm_47_triplet_pt4(a, j) * wm_interm_78_triplet_pt4(i, j)
term(54) = term(54) + r1(vrdav_Rl, a,i) * wm_interm_42_triplet_pt4(a, j) * wm_interm_74_triplet_pt4(i, p, q, j)
term(55) = term(55) + r1(vrdav_Rl, a,i) * wm_interm_42_triplet_pt4(a, j) * wm_interm_74_triplet_pt4(p, i, q, j)
term(56) = term(56) + r1(vrdav_Rl, a,i) * wm_interm_42_triplet_pt4(a, j) * wm_interm_74_triplet_pt4(p, i, j, q)
term(57) = term(57) + r1(vrdav_Rl, a,i) * wm_interm_42_triplet_pt4(a, j) * wm_interm_74_triplet_pt4(i, p, j, q)
term(58) = term(58) + r1(vrdav_Rl, a,i) * wm_interm_42_triplet_pt4(a, j) * wm_interm_75_triplet_pt4(i, j)
term(59) = term(59) + r1(vrdav_Rl, a,i) * wm_interm_42_triplet_pt4(a, j) * wm_interm_76_triplet_pt4(i, j)
term(60) = term(60) + r1(vrdav_Rl, a,i) * wm_interm_42_triplet_pt4(a, j) * wm_interm_77_triplet_pt4(i, j)
term(61) = term(61) + r1(vrdav_Rl, a,i) * wm_interm_42_triplet_pt4(a, j) * wm_interm_78_triplet_pt4(i, j)
term(62) = term(62) + r1(vrdav_Rl, a,i) * wm_interm_48_triplet_pt4(a, j) * wm_interm_74_triplet_pt4(i, p, q, j)
term(63) = term(63) + r1(vrdav_Rl, a,i) * wm_interm_48_triplet_pt4(a, j) * wm_interm_74_triplet_pt4(p, i, q, j)
term(64) = term(64) + r1(vrdav_Rl, a,i) * wm_interm_48_triplet_pt4(a, j) * wm_interm_74_triplet_pt4(p, i, j, q)
term(65) = term(65) + r1(vrdav_Rl, a,i) * wm_interm_48_triplet_pt4(a, j) * wm_interm_74_triplet_pt4(i, p, j, q)
term(66) = term(66) + r1(vrdav_Rl, a,i) * wm_interm_48_triplet_pt4(a, j) * wm_interm_75_triplet_pt4(i, j)
term(67) = term(67) + r1(vrdav_Rl, a,i) * wm_interm_48_triplet_pt4(a, j) * wm_interm_76_triplet_pt4(i, j)
term(68) = term(68) + r1(vrdav_Rl, a,i) * wm_interm_48_triplet_pt4(a, j) * wm_interm_77_triplet_pt4(i, j)
term(69) = term(69) + r1(vrdav_Rl, a,i) * wm_interm_48_triplet_pt4(a, j) * wm_interm_78_triplet_pt4(i, j)
term(70) = term(70) + s1(a,i) * wm_interm_52_triplet_pt4(a, j) * wm_interm_74_triplet_pt4(i, p, q, j)
term(71) = term(71) + s1(a,i) * wm_interm_52_triplet_pt4(a, j) * wm_interm_74_triplet_pt4(p, i, q, j)
term(72) = term(72) + s1(a,i) * wm_interm_52_triplet_pt4(a, j) * wm_interm_74_triplet_pt4(p, i, j, q)
term(73) = term(73) + s1(a,i) * wm_interm_52_triplet_pt4(a, j) * wm_interm_74_triplet_pt4(i, p, j, q)
term(74) = term(74) + s1(a,i) * wm_interm_52_triplet_pt4(a, j) * wm_interm_75_triplet_pt4(i, j)
term(75) = term(75) + s1(a,i) * wm_interm_52_triplet_pt4(a, j) * wm_interm_76_triplet_pt4(i, j)
term(76) = term(76) + s1(a,i) * wm_interm_52_triplet_pt4(a, j) * wm_interm_77_triplet_pt4(i, j)
term(77) = term(77) + s1(a,i) * wm_interm_52_triplet_pt4(a, j) * wm_interm_78_triplet_pt4(i, j)
term(78) = term(78) + s1(a,i) * wm_interm_51_triplet_pt4(a, p, j, q) * wm_interm_67_triplet_pt4(i, j)
term(79) = term(79) + s1(a,p) * wm_interm_51_triplet_pt4(a, i, j, q) * wm_interm_67_triplet_pt4(i, j)
term(80) = term(80) + s1(a,i) * wm_interm_51_triplet_pt4(a, p, j, q) * wm_interm_68_triplet_pt4(i, j)
term(81) = term(81) + s1(a,p) * wm_interm_51_triplet_pt4(a, i, j, q) * wm_interm_68_triplet_pt4(i, j)
term(82) = term(82) + s1(a,p) * wm_interm_51_triplet_pt4(a, i, q, j) * wm_interm_67_triplet_pt4(i, j)
term(83) = term(83) + s1(a,p) * wm_interm_51_triplet_pt4(a, i, q, j) * wm_interm_68_triplet_pt4(i, j)
term(84) = term(84) + s1(a,i) * wm_interm_51_triplet_pt4(a, p, q, j) * wm_interm_67_triplet_pt4(i, j)
term(85) = term(85) + s1(a,i) * wm_interm_51_triplet_pt4(a, p, q, j) * wm_interm_68_triplet_pt4(i, j)
term(86) = term(86) + s1(a,i) * wm_interm_51_triplet_pt4(a, p, j, q) * wm_interm_75_triplet_pt4(i, j)
term(87) = term(87) + s1(a,i) * wm_interm_51_triplet_pt4(a, p, j, q) * wm_interm_76_triplet_pt4(i, j)
term(88) = term(88) + s1(a,p) * wm_interm_51_triplet_pt4(a, i, j, q) * wm_interm_75_triplet_pt4(i, j)
term(89) = term(89) + s1(a,p) * wm_interm_51_triplet_pt4(a, i, j, q) * wm_interm_76_triplet_pt4(i, j)
term(90) = term(90) + s1(a,i) * wm_interm_51_triplet_pt4(a, p, j, q) * wm_interm_77_triplet_pt4(i, j)
term(91) = term(91) + s1(a,p) * wm_interm_51_triplet_pt4(a, i, j, q) * wm_interm_77_triplet_pt4(i, j)
term(92) = term(92) + s1(a,i) * wm_interm_51_triplet_pt4(a, p, j, q) * wm_interm_78_triplet_pt4(i, j)
term(93) = term(93) + s1(a,p) * wm_interm_51_triplet_pt4(a, i, j, q) * wm_interm_78_triplet_pt4(i, j)
term(94) = term(94) + s1(a,p) * wm_interm_51_triplet_pt4(a, i, q, j) * wm_interm_75_triplet_pt4(i, j)
term(95) = term(95) + s1(a,p) * wm_interm_51_triplet_pt4(a, i, q, j) * wm_interm_76_triplet_pt4(i, j)
term(96) = term(96) + s1(a,p) * wm_interm_51_triplet_pt4(a, i, q, j) * wm_interm_77_triplet_pt4(i, j)
term(97) = term(97) + s1(a,p) * wm_interm_51_triplet_pt4(a, i, q, j) * wm_interm_78_triplet_pt4(i, j)
term(98) = term(98) + s1(a,i) * wm_interm_51_triplet_pt4(a, p, q, j) * wm_interm_75_triplet_pt4(i, j)
term(99) = term(99) + s1(a,i) * wm_interm_51_triplet_pt4(a, p, q, j) * wm_interm_76_triplet_pt4(i, j)
term(100) = term(100) + s1(a,i) * wm_interm_51_triplet_pt4(a, p, q, j) * wm_interm_77_triplet_pt4(i, j)
term(101) = term(101) + s1(a,i) * wm_interm_51_triplet_pt4(a, p, q, j) * wm_interm_78_triplet_pt4(i, j)
term(102) = term(102) + t1(a,i) * wm_interm_45_triplet_pt4(a, p, j, q) * wm_interm_87_triplet_pt4(i, j)
term(103) = term(103) + t1(a,i) * wm_interm_44_triplet_pt4(a, j) * wm_interm_87_triplet_pt4(i, j)
term(104) = term(104) + t1(a,i) * wm_interm_45_triplet_pt4(a, p, q, j) * wm_interm_87_triplet_pt4(i, j)
term(105) = term(105) + t1(a,i) * wm_interm_46_triplet_pt4(a, j) * wm_interm_87_triplet_pt4(i, j)
term(106) = term(106) + t1(a,i) * wm_interm_45_triplet_pt4(a, p, j, q) * wm_interm_89_triplet_pt4(i, j)
term(107) = term(107) + t1(a,i) * wm_interm_44_triplet_pt4(a, j) * wm_interm_89_triplet_pt4(i, j)
term(108) = term(108) + t1(a,i) * wm_interm_45_triplet_pt4(a, p, q, j) * wm_interm_89_triplet_pt4(i, j)
term(109) = term(109) + t1(a,i) * wm_interm_46_triplet_pt4(a, j) * wm_interm_89_triplet_pt4(i, j)
term(110) = term(110) + t1(a,i) * wm_interm_54_triplet_pt4(i, j) * wm_interm_85_triplet_pt4(a, j, q, p)
term(111) = term(111) + t1(a,i) * wm_interm_54_triplet_pt4(i, j) * wm_interm_86_triplet_pt4(a, j)
term(112) = term(112) + t1(a,i) * wm_interm_53_triplet_pt4(i, j) * wm_interm_85_triplet_pt4(a, j, q, p)
term(113) = term(113) + t1(a,i) * wm_interm_53_triplet_pt4(i, j) * wm_interm_86_triplet_pt4(a, j)
term(114) = term(114) + t1(a,q) * wm_interm_45_triplet_pt4(a, i, j, p) * wm_interm_87_triplet_pt4(i, j)
term(115) = term(115) + t1(a,q) * wm_interm_45_triplet_pt4(a, i, p, j) * wm_interm_87_triplet_pt4(i, j)
term(116) = term(116) + t1(a,q) * wm_interm_45_triplet_pt4(a, i, j, p) * wm_interm_89_triplet_pt4(i, j)
term(117) = term(117) + t1(a,q) * wm_interm_45_triplet_pt4(a, i, p, j) * wm_interm_89_triplet_pt4(i, j)
term(118) = term(118) + t1(a,i) * wm_interm_44_triplet_pt4(a, j) * wm_interm_88_triplet_pt4(i, p, q, j)
term(119) = term(119) + t1(a,i) * wm_interm_46_triplet_pt4(a, j) * wm_interm_88_triplet_pt4(i, p, q, j)
term(120) = term(120) + t1(a,i) * wm_interm_44_triplet_pt4(a, j) * wm_interm_88_triplet_pt4(i, p, j, q)
term(121) = term(121) + t1(a,i) * wm_interm_46_triplet_pt4(a, j) * wm_interm_88_triplet_pt4(i, p, j, q)
term(122) = term(122) + t1(a,i) * wm_interm_49_triplet_pt4(i, p, j, q) * wm_interm_86_triplet_pt4(a, j)
term(123) = term(123) + t1(a,i) * wm_interm_49_triplet_pt4(p, i, j, q) * wm_interm_86_triplet_pt4(a, j)
term(124) = term(124) + t1(a,i) * wm_interm_45_triplet_pt4(a, p, j, q) * wm_interm_95_triplet_pt4(i, j)
term(125) = term(125) + t1(a,i) * wm_interm_44_triplet_pt4(a, j) * wm_interm_95_triplet_pt4(i, j)
term(126) = term(126) + t1(a,i) * wm_interm_45_triplet_pt4(a, p, q, j) * wm_interm_95_triplet_pt4(i, j)
term(127) = term(127) + t1(a,i) * wm_interm_46_triplet_pt4(a, j) * wm_interm_95_triplet_pt4(i, j)
term(128) = term(128) + t1(a,i) * wm_interm_45_triplet_pt4(a, p, j, q) * wm_interm_98_triplet_pt4(i, j)
term(129) = term(129) + t1(a,i) * wm_interm_44_triplet_pt4(a, j) * wm_interm_98_triplet_pt4(i, j)
term(130) = term(130) + t1(a,i) * wm_interm_45_triplet_pt4(a, p, q, j) * wm_interm_98_triplet_pt4(i, j)
term(131) = term(131) + t1(a,i) * wm_interm_46_triplet_pt4(a, j) * wm_interm_98_triplet_pt4(i, j)
term(132) = term(132) + t1(a,i) * wm_interm_54_triplet_pt4(i, j) * wm_interm_92_triplet_pt4(a, q, j, p)
term(133) = term(133) + t1(a,i) * wm_interm_54_triplet_pt4(i, j) * wm_interm_93_triplet_pt4(a, j)
term(134) = term(134) + t1(a,i) * wm_interm_53_triplet_pt4(i, j) * wm_interm_92_triplet_pt4(a, q, j, p)
term(135) = term(135) + t1(a,i) * wm_interm_53_triplet_pt4(i, j) * wm_interm_93_triplet_pt4(a, j)
term(136) = term(136) + t1(a,i) * wm_interm_54_triplet_pt4(i, j) * wm_interm_92_triplet_pt4(a, j, q, p)
term(137) = term(137) + t1(a,i) * wm_interm_54_triplet_pt4(i, j) * wm_interm_96_triplet_pt4(a, j)
term(138) = term(138) + t1(a,i) * wm_interm_53_triplet_pt4(i, j) * wm_interm_92_triplet_pt4(a, j, q, p)
term(139) = term(139) + t1(a,i) * wm_interm_53_triplet_pt4(i, j) * wm_interm_96_triplet_pt4(a, j)
term(140) = term(140) + r1(vrdav_Rr, a,i) * wm_interm_20_triplet_pt4(a, j) * wm_interm_53_triplet_pt4(i, j)
term(141) = term(141) + r1(vrdav_Rr, a,i) * wm_interm_20_triplet_pt4(a, j) * wm_interm_54_triplet_pt4(i, j)
term(142) = term(142) + r1(vrdav_Rr, a,i) * wm_interm_21_triplet_pt4(a, j) * wm_interm_53_triplet_pt4(i, j)
term(143) = term(143) + r1(vrdav_Rr, a,i) * wm_interm_21_triplet_pt4(a, j) * wm_interm_54_triplet_pt4(i, j)
term(144) = term(144) + t1(a,i) * wm_interm_53_triplet_pt4(i, j) * wm_interm_94_triplet_pt4(a, q, j, p)
term(145) = term(145) + t1(a,i) * wm_interm_54_triplet_pt4(i, j) * wm_interm_94_triplet_pt4(a, q, j, p)
term(146) = term(146) + t1(a,i) * wm_interm_54_triplet_pt4(i, j) * wm_interm_94_triplet_pt4(a, j, q, p)
term(147) = term(147) + t1(a,i) * wm_interm_53_triplet_pt4(i, j) * wm_interm_94_triplet_pt4(a, j, q, p)
term(148) = term(148) + t1(a,q) * wm_interm_45_triplet_pt4(a, i, j, p) * wm_interm_95_triplet_pt4(i, j)
term(149) = term(149) + t1(a,q) * wm_interm_45_triplet_pt4(a, i, p, j) * wm_interm_95_triplet_pt4(i, j)
term(150) = term(150) + t1(a,q) * wm_interm_45_triplet_pt4(a, i, j, p) * wm_interm_98_triplet_pt4(i, j)
term(151) = term(151) + t1(a,q) * wm_interm_45_triplet_pt4(a, i, p, j) * wm_interm_98_triplet_pt4(i, j)
term(152) = term(152) + t1(a,i) * wm_interm_44_triplet_pt4(a, j) * wm_interm_97_triplet_pt4(p, i, q, j)
term(153) = term(153) + t1(a,i) * wm_interm_46_triplet_pt4(a, j) * wm_interm_97_triplet_pt4(p, i, q, j)
term(154) = term(154) + t1(a,i) * wm_interm_44_triplet_pt4(a, j) * wm_interm_97_triplet_pt4(p, i, j, q)
term(155) = term(155) + t1(a,i) * wm_interm_46_triplet_pt4(a, j) * wm_interm_97_triplet_pt4(p, i, j, q)
term(156) = term(156) + t1(a,i) * wm_interm_49_triplet_pt4(i, p, j, q) * wm_interm_96_triplet_pt4(a, j)
term(157) = term(157) + t1(a,i) * wm_interm_49_triplet_pt4(p, i, j, q) * wm_interm_96_triplet_pt4(a, j)
term(158) = term(158) + t1(a,i) * wm_interm_49_triplet_pt4(i, p, j, q) * wm_interm_93_triplet_pt4(a, j)
term(159) = term(159) + t1(a,i) * wm_interm_49_triplet_pt4(p, i, j, q) * wm_interm_93_triplet_pt4(a, j)
term(160) = term(160) + t1(a,i) * wm_interm_44_triplet_pt4(a, j) * wm_interm_97_triplet_pt4(i, p, q, j)
term(161) = term(161) + t1(a,i) * wm_interm_44_triplet_pt4(a, j) * wm_interm_97_triplet_pt4(i, p, j, q)
term(162) = term(162) + t1(a,i) * wm_interm_46_triplet_pt4(a, j) * wm_interm_97_triplet_pt4(i, p, q, j)
term(163) = term(163) + t1(a,i) * wm_interm_46_triplet_pt4(a, j) * wm_interm_97_triplet_pt4(i, p, j, q)
term(164) = term(164) + r1(vrdav_Rr, a,i) * wm_interm_20_triplet_pt4(a, j) * wm_interm_49_triplet_pt4(i, p, j, q)
term(165) = term(165) + r1(vrdav_Rr, a,i) * wm_interm_21_triplet_pt4(a, j) * wm_interm_49_triplet_pt4(i, p, j, q)
term(166) = term(166) + r1(vrdav_Rr, a,i) * wm_interm_20_triplet_pt4(a, j) * wm_interm_49_triplet_pt4(p, i, j, q)
term(167) = term(167) + r1(vrdav_Rr, a,i) * wm_interm_21_triplet_pt4(a, j) * wm_interm_49_triplet_pt4(p, i, j, q)
end do 
end do 
end do 

term(0) = term(0) * 12.0d+0 
term(1) = term(1) * 12.0d+0 
term(2) = term(2) * (-12.0d+0) 
term(3) = term(3) * 6.0d+0 
term(4) = term(4) * 6.0d+0 
term(5) = term(5) * (-6.0d+0) 
term(6) = term(6) * (-6.0d+0) 
term(7) = term(7) * 6.0d+0 
term(8) = term(8) * 6.0d+0 
term(9) = term(9) * (-6.0d+0) 
term(10) = term(10) * (-4.0d+0) 
term(11) = term(11) * 8.0d+0 
term(12) = term(12) * (-4.0d+0) 
term(13) = term(13) * 8.0d+0 
term(14) = term(14) * (-4.0d+0) 
term(15) = term(15) * 8.0d+0 
term(16) = term(16) * (-4.0d+0) 
term(17) = term(17) * 8.0d+0 
term(18) = term(18) * (-6.0d+0) 
term(19) = term(19) * 6.0d+0 
term(20) = term(20) * (-4.0d+0) 
term(21) = term(21) * 8.0d+0 
term(22) = term(22) * (-4.0d+0) 
term(23) = term(23) * 8.0d+0 
term(24) = term(24) * 12.0d+0 
term(25) = term(25) * (-24.0d+0) 
term(26) = term(26) * 16.0d+0 
term(27) = term(27) * (-32.0d+0) 
term(28) = term(28) * (-8.0d+0) 
term(29) = term(29) * 16.0d+0 
term(30) = term(30) * (-6.0d+0) 
term(31) = term(31) * 6.0d+0 
term(32) = term(32) * 6.0d+0 
term(33) = term(33) * (-6.0d+0) 
term(34) = term(34) * 6.0d+0 
term(35) = term(35) * (-6.0d+0) 
term(36) = term(36) * (-12.0d+0) 
term(37) = term(37) * 12.0d+0 
term(38) = term(38) * (-12.0d+0) 
term(39) = term(39) * 12.0d+0 
term(40) = term(40) * 24.0d+0 
term(41) = term(41) * (-24.0d+0) 
term(42) = term(42) * 12.0d+0 
term(43) = term(43) * (-12.0d+0) 
term(44) = term(44) * (-12.0d+0) 
term(45) = term(45) * 12.0d+0 
term(46) = term(46) * 4.0d+0 
term(47) = term(47) * (-8.0d+0) 
term(48) = term(48) * 4.0d+0 
term(49) = term(49) * (-8.0d+0) 
term(50) = term(50) * (-8.0d+0) 
term(51) = term(51) * 16.0d+0 
term(52) = term(52) * (-8.0d+0) 
term(53) = term(53) * 16.0d+0 
term(54) = term(54) * 4.0d+0 
term(55) = term(55) * (-8.0d+0) 
term(56) = term(56) * 4.0d+0 
term(57) = term(57) * (-8.0d+0) 
term(58) = term(58) * (-8.0d+0) 
term(59) = term(59) * 16.0d+0 
term(60) = term(60) * (-8.0d+0) 
term(61) = term(61) * 16.0d+0 
term(62) = term(62) * (-8.0d+0) 
term(63) = term(63) * 16.0d+0 
term(64) = term(64) * (-8.0d+0) 
term(65) = term(65) * 16.0d+0 
term(66) = term(66) * 16.0d+0 
term(67) = term(67) * (-32.0d+0) 
term(68) = term(68) * 16.0d+0 
term(69) = term(69) * (-32.0d+0) 
term(70) = term(70) * (-8.0d+0) 
term(71) = term(71) * 16.0d+0 
term(72) = term(72) * (-8.0d+0) 
term(73) = term(73) * 16.0d+0 
term(74) = term(74) * 16.0d+0 
term(75) = term(75) * (-32.0d+0) 
term(76) = term(76) * 16.0d+0 
term(77) = term(77) * (-32.0d+0) 
term(78) = term(78) * (-3.0d+0) 
term(79) = term(79) * (-12.0d+0) 
term(80) = term(80) * 3.0d+0 
term(81) = term(81) * 12.0d+0 
term(82) = term(82) * 6.0d+0 
term(83) = term(83) * (-6.0d+0) 
term(84) = term(84) * 6.0d+0 
term(85) = term(85) * (-6.0d+0) 
term(86) = term(86) * 4.0d+0 
term(87) = term(87) * (-8.0d+0) 
term(88) = term(88) * (-8.0d+0) 
term(89) = term(89) * 16.0d+0 
term(90) = term(90) * 4.0d+0 
term(91) = term(91) * (-8.0d+0) 
term(92) = term(92) * (-8.0d+0) 
term(93) = term(93) * 16.0d+0 
term(94) = term(94) * 4.0d+0 
term(95) = term(95) * (-8.0d+0) 
term(96) = term(96) * 4.0d+0 
term(97) = term(97) * (-8.0d+0) 
term(98) = term(98) * (-8.0d+0) 
term(99) = term(99) * 16.0d+0 
term(100) = term(100) * (-8.0d+0) 
term(101) = term(101) * 16.0d+0 
term(102) = term(102) * 2.0d+0 
term(103) = term(103) * (-4.0d+0) 
term(104) = term(104) * (-4.0d+0) 
term(105) = term(105) * 8.0d+0 
term(106) = term(106) * 2.0d+0 
term(107) = term(107) * (-4.0d+0) 
term(108) = term(108) * (-4.0d+0) 
term(109) = term(109) * 8.0d+0 
term(110) = term(110) * 24.0d+0 
term(111) = term(111) * 48.0d+0 
term(112) = term(112) * (-12.0d+0) 
term(113) = term(113) * (-24.0d+0) 
term(114) = term(114) * (-6.0d+0) 
term(115) = term(115) * 3.0d+0 
term(116) = term(116) * 6.0d+0 
term(117) = term(117) * (-3.0d+0) 
term(118) = term(118) * (-3.0d+0) 
term(119) = term(119) * 6.0d+0 
term(120) = term(120) * 3.0d+0 
term(121) = term(121) * (-6.0d+0) 
term(122) = term(122) * (-12.0d+0) 
term(123) = term(123) * 6.0d+0 
term(124) = term(124) * (-8.0d+0) 
term(125) = term(125) * 16.0d+0 
term(126) = term(126) * 16.0d+0 
term(127) = term(127) * (-32.0d+0) 
term(128) = term(128) * 16.0d+0 
term(129) = term(129) * (-32.0d+0) 
term(130) = term(130) * (-32.0d+0) 
term(131) = term(131) * 64.0d+0 
term(132) = term(132) * (-16.0d+0) 
term(133) = term(133) * 64.0d+0 
term(134) = term(134) * 8.0d+0 
term(135) = term(135) * (-32.0d+0) 
term(136) = term(136) * 8.0d+0 
term(137) = term(137) * (-32.0d+0) 
term(138) = term(138) * (-4.0d+0) 
term(139) = term(139) * 16.0d+0 
term(140) = term(140) * (-32.0d+0) 
term(141) = term(141) * 64.0d+0 
term(142) = term(142) * 16.0d+0 
term(143) = term(143) * (-32.0d+0) 
term(144) = term(144) * (-4.0d+0) 
term(145) = term(145) * 8.0d+0 
term(146) = term(146) * (-16.0d+0) 
term(147) = term(147) * 8.0d+0 
term(148) = term(148) * 8.0d+0 
term(149) = term(149) * (-4.0d+0) 
term(150) = term(150) * (-16.0d+0) 
term(151) = term(151) * 8.0d+0 
term(152) = term(152) * 4.0d+0 
term(153) = term(153) * (-8.0d+0) 
term(154) = term(154) * (-2.0d+0) 
term(155) = term(155) * 4.0d+0 
term(156) = term(156) * 8.0d+0 
term(157) = term(157) * (-4.0d+0) 
term(158) = term(158) * (-16.0d+0) 
term(159) = term(159) * 8.0d+0 
term(160) = term(160) * (-2.0d+0) 
term(161) = term(161) * 4.0d+0 
term(162) = term(162) * 4.0d+0 
term(163) = term(163) * (-8.0d+0) 
term(164) = term(164) * (-16.0d+0) 
term(165) = term(165) * 8.0d+0 
term(166) = term(166) * 8.0d+0 
term(167) = term(167) * (-4.0d+0) 

do b = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(168) = term(168) + s2(a,b,i,q) * wm_interm_42_triplet_pt4(a, j) * wm_interm_71_triplet_pt4(b, j, i, p)
term(169) = term(169) + s2(a,b,i,q) * wm_interm_48_triplet_pt4(a, j) * wm_interm_71_triplet_pt4(b, j, i, p)
term(170) = term(170) + s2(a,b,i,p) * wm_interm_2_triplet_pt4(a, j) * wm_interm_51_triplet_pt4(b, j, i, q)
term(171) = term(171) + s2(a,b,i,p) * wm_interm_3_triplet_pt4(a, j) * wm_interm_51_triplet_pt4(b, j, i, q)
term(172) = term(172) + s2(a,b,i,p) * wm_interm_43_triplet_pt4(a, j, i, q) * wm_interm_82_triplet_pt4(b, j)
term(173) = term(173) + s2(a,b,i,p) * wm_interm_43_triplet_pt4(a, j, i, q) * wm_interm_84_triplet_pt4(b, j)
term(174) = term(174) + s2(a,b,i,p) * wm_interm_43_triplet_pt4(a, j, i, q) * wm_interm_83_triplet_pt4(b, j)
end do 
end do 
end do 
end do 

term(168) = term(168) * (-16.0d+0) 
term(169) = term(169) * 32.0d+0 
term(170) = term(170) * 16.0d+0 
term(171) = term(171) * (-8.0d+0) 
term(172) = term(172) * (-12.0d+0) 
term(173) = term(173) * 8.0d+0 
term(174) = term(174) * (-16.0d+0) 

term(175) = term(175) + wm_interm_17_triplet_pt4 * wm_interm_5_triplet_pt4(p, q)
term(176) = term(176) + wm_interm_18_triplet_pt4 * wm_interm_5_triplet_pt4(p, q)
term(177) = term(177) + wm_interm_19_triplet_pt4 * wm_interm_5_triplet_pt4(p, q)
term(178) = term(178) + wm_interm_38_triplet_pt4 * wm_interm_5_triplet_pt4(p, q)
term(179) = term(179) + wm_interm_39_triplet_pt4 * wm_interm_5_triplet_pt4(p, q)
term(180) = term(180) + wm_interm_40_triplet_pt4 * wm_interm_5_triplet_pt4(p, q)
term(181) = term(181) + wm_interm_5_triplet_pt4(p, q) * wm_interm_63_triplet_pt4

term(175) = term(175) * (-6.0d+0) 
term(176) = term(176) * 4.0d+0 
term(177) = term(177) * (-8.0d+0) 
term(178) = term(178) * (-12.0d+0) 
term(179) = term(179) * 8.0d+0 
term(180) = term(180) * (-16.0d+0) 
term(181) = term(181) * (-4.0d+0) 

do a = nocc + 1, nactive 
do i = 1, nocc 
do b = nocc + 1, nactive 
term(182) = term(182) + r1(vrdav_Rl, a,i) * wm_interm_2_triplet_pt4(b, i) * wm_interm_56_triplet_pt4(b, a)
term(183) = term(183) + r1(vrdav_Rl, a,i) * wm_interm_2_triplet_pt4(b, i) * wm_interm_58_triplet_pt4(b, a)
term(184) = term(184) + r1(vrdav_Rl, a,i) * wm_interm_3_triplet_pt4(b, i) * wm_interm_56_triplet_pt4(b, a)
term(185) = term(185) + r1(vrdav_Rl, a,i) * wm_interm_3_triplet_pt4(b, i) * wm_interm_58_triplet_pt4(b, a)
term(186) = term(186) + r1(vrdav_Rl, a,i) * wm_interm_56_triplet_pt4(b, a) * wm_interm_79_triplet_pt4(b, i, p, q)
term(187) = term(187) + r1(vrdav_Rl, a,i) * wm_interm_58_triplet_pt4(b, a) * wm_interm_79_triplet_pt4(b, i, p, q)
term(188) = term(188) + r1(vrdav_Rl, a,i) * wm_interm_14_triplet_pt4(b, i, p, q) * wm_interm_56_triplet_pt4(b, a)
term(189) = term(189) + r1(vrdav_Rl, a,i) * wm_interm_14_triplet_pt4(b, i, p, q) * wm_interm_58_triplet_pt4(b, a)
end do 
end do 
end do 

term(182) = term(182) * (-32.0d+0) 
term(183) = term(183) * 16.0d+0 
term(184) = term(184) * 16.0d+0 
term(185) = term(185) * (-8.0d+0) 
term(186) = term(186) * 4.0d+0 
term(187) = term(187) * (-2.0d+0) 
term(188) = term(188) * (-8.0d+0) 
term(189) = term(189) * 4.0d+0 

do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(190) = term(190) + s2(a,b,i,q) * wm_interm_2_triplet_pt4(a, p) * wm_interm_47_triplet_pt4(b, i)
term(191) = term(191) + s2(a,b,i,q) * wm_interm_3_triplet_pt4(a, p) * wm_interm_47_triplet_pt4(b, i)
term(192) = term(192) + s2(a,b,i,q) * wm_interm_2_triplet_pt4(a, p) * wm_interm_52_triplet_pt4(b, i)
term(193) = term(193) + s2(a,b,i,q) * wm_interm_3_triplet_pt4(a, p) * wm_interm_52_triplet_pt4(b, i)
term(194) = term(194) + r1(vrdav_Rl, a,i) * wm_interm_42_triplet_pt4(b, i) * wm_interm_65_triplet_pt4(a, b)
term(195) = term(195) + r1(vrdav_Rl, a,i) * wm_interm_42_triplet_pt4(b, i) * wm_interm_70_triplet_pt4(a, b)
term(196) = term(196) + r1(vrdav_Rl, a,i) * wm_interm_48_triplet_pt4(b, i) * wm_interm_65_triplet_pt4(a, b)
term(197) = term(197) + r1(vrdav_Rl, a,i) * wm_interm_48_triplet_pt4(b, i) * wm_interm_70_triplet_pt4(a, b)
term(198) = term(198) + r1(vrdav_Rl, a,i) * wm_interm_42_triplet_pt4(b, i) * wm_interm_72_triplet_pt4(a, b)
term(199) = term(199) + r1(vrdav_Rl, a,i) * wm_interm_42_triplet_pt4(b, i) * wm_interm_73_triplet_pt4(a, b)
term(200) = term(200) + r1(vrdav_Rl, a,i) * wm_interm_48_triplet_pt4(b, i) * wm_interm_72_triplet_pt4(a, b)
term(201) = term(201) + r1(vrdav_Rl, a,i) * wm_interm_48_triplet_pt4(b, i) * wm_interm_73_triplet_pt4(a, b)
term(202) = term(202) + s1(a,i) * wm_interm_47_triplet_pt4(b, i) * wm_interm_72_triplet_pt4(a, b)
term(203) = term(203) + s1(a,i) * wm_interm_47_triplet_pt4(b, i) * wm_interm_73_triplet_pt4(a, b)
term(204) = term(204) + s1(a,i) * wm_interm_47_triplet_pt4(b, i) * wm_interm_80_triplet_pt4(a, b)
term(205) = term(205) + s1(a,i) * wm_interm_47_triplet_pt4(b, i) * wm_interm_81_triplet_pt4(a, b)
term(206) = term(206) + s1(a,i) * wm_interm_52_triplet_pt4(b, i) * wm_interm_80_triplet_pt4(a, b)
term(207) = term(207) + s1(a,i) * wm_interm_52_triplet_pt4(b, i) * wm_interm_81_triplet_pt4(a, b)
term(208) = term(208) + s1(a,i) * wm_interm_52_triplet_pt4(b, i) * wm_interm_72_triplet_pt4(a, b)
term(209) = term(209) + s1(a,i) * wm_interm_52_triplet_pt4(b, i) * wm_interm_73_triplet_pt4(a, b)
term(210) = term(210) + s2(a,b,i,p) * wm_interm_2_triplet_pt4(b, i) * wm_interm_47_triplet_pt4(a, q)
term(211) = term(211) + s2(a,b,i,p) * wm_interm_3_triplet_pt4(b, i) * wm_interm_47_triplet_pt4(a, q)
term(212) = term(212) + s2(a,b,i,p) * wm_interm_2_triplet_pt4(b, i) * wm_interm_52_triplet_pt4(a, q)
term(213) = term(213) + s2(a,b,i,p) * wm_interm_3_triplet_pt4(b, i) * wm_interm_52_triplet_pt4(a, q)
term(214) = term(214) + s2(a,b,i,p) * wm_interm_42_triplet_pt4(a, q) * wm_interm_82_triplet_pt4(b, i)
term(215) = term(215) + s2(a,b,i,p) * wm_interm_48_triplet_pt4(a, q) * wm_interm_82_triplet_pt4(b, i)
term(216) = term(216) + s2(a,b,i,p) * wm_interm_42_triplet_pt4(a, q) * wm_interm_84_triplet_pt4(b, i)
term(217) = term(217) + s2(a,b,i,p) * wm_interm_42_triplet_pt4(a, q) * wm_interm_83_triplet_pt4(b, i)
term(218) = term(218) + s2(a,b,i,p) * wm_interm_48_triplet_pt4(a, q) * wm_interm_84_triplet_pt4(b, i)
term(219) = term(219) + s2(a,b,i,p) * wm_interm_48_triplet_pt4(a, q) * wm_interm_83_triplet_pt4(b, i)
term(220) = term(220) + t1(a,i) * wm_interm_56_triplet_pt4(a, b) * wm_interm_85_triplet_pt4(b, i, q, p)
term(221) = term(221) + t1(a,i) * wm_interm_56_triplet_pt4(a, b) * wm_interm_86_triplet_pt4(b, i)
term(222) = term(222) + t1(a,i) * wm_interm_58_triplet_pt4(a, b) * wm_interm_85_triplet_pt4(b, i, q, p)
term(223) = term(223) + t1(a,i) * wm_interm_58_triplet_pt4(a, b) * wm_interm_86_triplet_pt4(b, i)
term(224) = term(224) + t1(a,i) * wm_interm_44_triplet_pt4(b, i) * wm_interm_90_triplet_pt4(a, b)
term(225) = term(225) + t1(a,i) * wm_interm_46_triplet_pt4(b, i) * wm_interm_90_triplet_pt4(a, b)
term(226) = term(226) + t1(a,i) * wm_interm_44_triplet_pt4(b, i) * wm_interm_91_triplet_pt4(a, b)
term(227) = term(227) + t1(a,i) * wm_interm_46_triplet_pt4(b, i) * wm_interm_91_triplet_pt4(a, b)
term(228) = term(228) + r1(vrdav_Rr, a,i) * wm_interm_60_triplet_pt4(b, i, p, q) * wm_interm_90_triplet_pt4(a, b)
term(229) = term(229) + r1(vrdav_Rr, a,i) * wm_interm_60_triplet_pt4(b, i, p, q) * wm_interm_91_triplet_pt4(a, b)
term(230) = term(230) + t2(a,b,i,q) * wm_interm_0_triplet_pt4(b, i) * wm_interm_44_triplet_pt4(a, p)
term(231) = term(231) + t2(a,b,i,q) * wm_interm_0_triplet_pt4(b, i) * wm_interm_46_triplet_pt4(a, p)
term(232) = term(232) + t1(a,i) * wm_interm_56_triplet_pt4(a, b) * wm_interm_93_triplet_pt4(b, i)
term(233) = term(233) + t1(a,i) * wm_interm_58_triplet_pt4(a, b) * wm_interm_93_triplet_pt4(b, i)
term(234) = term(234) + t1(a,i) * wm_interm_58_triplet_pt4(a, b) * wm_interm_96_triplet_pt4(b, i)
term(235) = term(235) + t1(a,i) * wm_interm_56_triplet_pt4(a, b) * wm_interm_96_triplet_pt4(b, i)
term(236) = term(236) + t1(a,i) * wm_interm_44_triplet_pt4(b, i) * wm_interm_99_triplet_pt4(a, b)
term(237) = term(237) + t1(a,i) * wm_interm_46_triplet_pt4(b, i) * wm_interm_99_triplet_pt4(a, b)
term(238) = term(238) + t1(a,i) * wm_interm_100_triplet_pt4(a, b) * wm_interm_44_triplet_pt4(b, i)
term(239) = term(239) + t1(a,i) * wm_interm_100_triplet_pt4(a, b) * wm_interm_46_triplet_pt4(b, i)
term(240) = term(240) + r1(vrdav_Rr, a,i) * wm_interm_20_triplet_pt4(b, i) * wm_interm_56_triplet_pt4(a, b)
term(241) = term(241) + r1(vrdav_Rr, a,i) * wm_interm_20_triplet_pt4(b, i) * wm_interm_58_triplet_pt4(a, b)
term(242) = term(242) + t1(a,i) * wm_interm_56_triplet_pt4(a, b) * wm_interm_92_triplet_pt4(b, i, q, p)
term(243) = term(243) + t1(a,i) * wm_interm_58_triplet_pt4(a, b) * wm_interm_92_triplet_pt4(b, i, q, p)
term(244) = term(244) + t1(a,i) * wm_interm_56_triplet_pt4(a, b) * wm_interm_94_triplet_pt4(b, i, q, p)
term(245) = term(245) + r1(vrdav_Rr, a,i) * wm_interm_21_triplet_pt4(b, i) * wm_interm_56_triplet_pt4(a, b)
term(246) = term(246) + r1(vrdav_Rr, a,i) * wm_interm_21_triplet_pt4(b, i) * wm_interm_58_triplet_pt4(a, b)
term(247) = term(247) + t1(a,i) * wm_interm_58_triplet_pt4(a, b) * wm_interm_94_triplet_pt4(b, i, q, p)
term(248) = term(248) + r1(vrdav_Rr, a,i) * wm_interm_27_triplet_pt4(b, i, p, q) * wm_interm_56_triplet_pt4(a, b)
term(249) = term(249) + r1(vrdav_Rr, a,i) * wm_interm_27_triplet_pt4(b, i, p, q) * wm_interm_58_triplet_pt4(a, b)
term(250) = term(250) + r1(vrdav_Rr, a,i) * wm_interm_60_triplet_pt4(b, i, p, q) * wm_interm_99_triplet_pt4(a, b)
term(251) = term(251) + r1(vrdav_Rr, a,i) * wm_interm_100_triplet_pt4(a, b) * wm_interm_60_triplet_pt4(b, i, p, q)
term(252) = term(252) + r1(vrdav_Rr, a,i) * wm_interm_24_triplet_pt4(b, i, p, q) * wm_interm_56_triplet_pt4(a, b)
term(253) = term(253) + r1(vrdav_Rr, a,i) * wm_interm_24_triplet_pt4(b, i, p, q) * wm_interm_58_triplet_pt4(a, b)
term(254) = term(254) + t2(a,b,i,q) * wm_interm_20_triplet_pt4(a, p) * wm_interm_44_triplet_pt4(b, i)
term(255) = term(255) + t2(a,b,i,q) * wm_interm_20_triplet_pt4(a, p) * wm_interm_46_triplet_pt4(b, i)
term(256) = term(256) + t2(a,b,i,q) * wm_interm_21_triplet_pt4(a, p) * wm_interm_44_triplet_pt4(b, i)
term(257) = term(257) + t2(a,b,i,q) * wm_interm_21_triplet_pt4(a, p) * wm_interm_46_triplet_pt4(b, i)
term(258) = term(258) + t2(a,b,i,q) * wm_interm_20_triplet_pt4(b, i) * wm_interm_44_triplet_pt4(a, p)
term(259) = term(259) + t2(a,b,i,q) * wm_interm_20_triplet_pt4(b, i) * wm_interm_46_triplet_pt4(a, p)
term(260) = term(260) + t2(a,b,i,q) * wm_interm_21_triplet_pt4(b, i) * wm_interm_44_triplet_pt4(a, p)
term(261) = term(261) + t2(a,b,i,q) * wm_interm_21_triplet_pt4(b, i) * wm_interm_46_triplet_pt4(a, p)
end do 
end do 
end do 

term(190) = term(190) * (-8.0d+0) 
term(191) = term(191) * 4.0d+0 
term(192) = term(192) * 16.0d+0 
term(193) = term(193) * (-8.0d+0) 
term(194) = term(194) * 12.0d+0 
term(195) = term(195) * (-12.0d+0) 
term(196) = term(196) * (-24.0d+0) 
term(197) = term(197) * 24.0d+0 
term(198) = term(198) * (-16.0d+0) 
term(199) = term(199) * 32.0d+0 
term(200) = term(200) * 32.0d+0 
term(201) = term(201) * (-64.0d+0) 
term(202) = term(202) * (-8.0d+0) 
term(203) = term(203) * 16.0d+0 
term(204) = term(204) * (-8.0d+0) 
term(205) = term(205) * 16.0d+0 
term(206) = term(206) * 16.0d+0 
term(207) = term(207) * (-32.0d+0) 
term(208) = term(208) * 16.0d+0 
term(209) = term(209) * (-32.0d+0) 
term(210) = term(210) * (-8.0d+0) 
term(211) = term(211) * 4.0d+0 
term(212) = term(212) * 16.0d+0 
term(213) = term(213) * (-8.0d+0) 
term(214) = term(214) * (-12.0d+0) 
term(215) = term(215) * 24.0d+0 
term(216) = term(216) * 8.0d+0 
term(217) = term(217) * (-16.0d+0) 
term(218) = term(218) * (-16.0d+0) 
term(219) = term(219) * 32.0d+0 
term(220) = term(220) * 24.0d+0 
term(221) = term(221) * 48.0d+0 
term(222) = term(222) * (-12.0d+0) 
term(223) = term(223) * (-24.0d+0) 
term(224) = term(224) * (-4.0d+0) 
term(225) = term(225) * 8.0d+0 
term(226) = term(226) * (-4.0d+0) 
term(227) = term(227) * 8.0d+0 
term(228) = term(228) * (-6.0d+0) 
term(229) = term(229) * 6.0d+0 
term(230) = term(230) * 2.0d+0 
term(231) = term(231) * (-4.0d+0) 
term(232) = term(232) * 64.0d+0 
term(233) = term(233) * (-32.0d+0) 
term(234) = term(234) * 16.0d+0 
term(235) = term(235) * (-32.0d+0) 
term(236) = term(236) * 16.0d+0 
term(237) = term(237) * (-32.0d+0) 
term(238) = term(238) * (-32.0d+0) 
term(239) = term(239) * 64.0d+0 
term(240) = term(240) * 64.0d+0 
term(241) = term(241) * (-32.0d+0) 
term(242) = term(242) * 8.0d+0 
term(243) = term(243) * (-4.0d+0) 
term(244) = term(244) * (-16.0d+0) 
term(245) = term(245) * (-32.0d+0) 
term(246) = term(246) * 16.0d+0 
term(247) = term(247) * 8.0d+0 
term(248) = term(248) * 4.0d+0 
term(249) = term(249) * (-2.0d+0) 
term(250) = term(250) * 8.0d+0 
term(251) = term(251) * (-16.0d+0) 
term(252) = term(252) * (-8.0d+0) 
term(253) = term(253) * 4.0d+0 
term(254) = term(254) * 8.0d+0 
term(255) = term(255) * (-16.0d+0) 
term(256) = term(256) * (-4.0d+0) 
term(257) = term(257) * 8.0d+0 
term(258) = term(258) * 8.0d+0 
term(259) = term(259) * (-16.0d+0) 
term(260) = term(260) * (-4.0d+0) 
term(261) = term(261) * 8.0d+0 

do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(262) = term(262) + s2(a,b,q,i) * wm_interm_2_triplet_pt4(a, p) * wm_interm_47_triplet_pt4(b, i)
term(263) = term(263) + s2(a,b,q,i) * wm_interm_3_triplet_pt4(a, p) * wm_interm_47_triplet_pt4(b, i)
term(264) = term(264) + s2(a,b,q,i) * wm_interm_2_triplet_pt4(a, p) * wm_interm_52_triplet_pt4(b, i)
term(265) = term(265) + s2(a,b,q,i) * wm_interm_3_triplet_pt4(a, p) * wm_interm_52_triplet_pt4(b, i)
term(266) = term(266) + s2(a,b,p,i) * wm_interm_2_triplet_pt4(b, i) * wm_interm_52_triplet_pt4(a, q)
term(267) = term(267) + s2(a,b,p,i) * wm_interm_3_triplet_pt4(b, i) * wm_interm_52_triplet_pt4(a, q)
term(268) = term(268) + s2(a,b,p,i) * wm_interm_2_triplet_pt4(b, i) * wm_interm_47_triplet_pt4(a, q)
term(269) = term(269) + s2(a,b,p,i) * wm_interm_3_triplet_pt4(b, i) * wm_interm_47_triplet_pt4(a, q)
term(270) = term(270) + s2(a,b,p,i) * wm_interm_42_triplet_pt4(a, q) * wm_interm_82_triplet_pt4(b, i)
term(271) = term(271) + r1(vrdav_Rl, a,i) * wm_interm_43_triplet_pt4(b, p, q, i) * wm_interm_65_triplet_pt4(a, b)
term(272) = term(272) + r1(vrdav_Rl, a,i) * wm_interm_43_triplet_pt4(b, p, q, i) * wm_interm_70_triplet_pt4(a, b)
term(273) = term(273) + r1(vrdav_Rl, a,i) * wm_interm_43_triplet_pt4(b, p, i, q) * wm_interm_65_triplet_pt4(a, b)
term(274) = term(274) + r1(vrdav_Rl, a,i) * wm_interm_43_triplet_pt4(b, p, i, q) * wm_interm_70_triplet_pt4(a, b)
term(275) = term(275) + s2(a,b,p,i) * wm_interm_48_triplet_pt4(a, q) * wm_interm_82_triplet_pt4(b, i)
term(276) = term(276) + s2(a,b,p,i) * wm_interm_42_triplet_pt4(a, q) * wm_interm_84_triplet_pt4(b, i)
term(277) = term(277) + s2(a,b,p,i) * wm_interm_42_triplet_pt4(a, q) * wm_interm_83_triplet_pt4(b, i)
term(278) = term(278) + r1(vrdav_Rl, a,i) * wm_interm_43_triplet_pt4(b, p, q, i) * wm_interm_72_triplet_pt4(a, b)
term(279) = term(279) + r1(vrdav_Rl, a,i) * wm_interm_43_triplet_pt4(b, p, q, i) * wm_interm_73_triplet_pt4(a, b)
term(280) = term(280) + r1(vrdav_Rl, a,i) * wm_interm_43_triplet_pt4(b, p, i, q) * wm_interm_72_triplet_pt4(a, b)
term(281) = term(281) + r1(vrdav_Rl, a,i) * wm_interm_43_triplet_pt4(b, p, i, q) * wm_interm_73_triplet_pt4(a, b)
term(282) = term(282) + s2(a,b,p,i) * wm_interm_48_triplet_pt4(a, q) * wm_interm_84_triplet_pt4(b, i)
term(283) = term(283) + s2(a,b,p,i) * wm_interm_48_triplet_pt4(a, q) * wm_interm_83_triplet_pt4(b, i)
term(284) = term(284) + s1(a,i) * wm_interm_51_triplet_pt4(b, p, i, q) * wm_interm_72_triplet_pt4(a, b)
term(285) = term(285) + s1(a,i) * wm_interm_51_triplet_pt4(b, p, i, q) * wm_interm_73_triplet_pt4(a, b)
term(286) = term(286) + s1(a,i) * wm_interm_51_triplet_pt4(b, p, i, q) * wm_interm_80_triplet_pt4(a, b)
term(287) = term(287) + s1(a,i) * wm_interm_51_triplet_pt4(b, p, i, q) * wm_interm_81_triplet_pt4(a, b)
term(288) = term(288) + s1(a,i) * wm_interm_51_triplet_pt4(b, p, q, i) * wm_interm_80_triplet_pt4(a, b)
term(289) = term(289) + s1(a,i) * wm_interm_51_triplet_pt4(b, p, q, i) * wm_interm_81_triplet_pt4(a, b)
term(290) = term(290) + s1(a,i) * wm_interm_51_triplet_pt4(b, p, q, i) * wm_interm_72_triplet_pt4(a, b)
term(291) = term(291) + s1(a,i) * wm_interm_51_triplet_pt4(b, p, q, i) * wm_interm_73_triplet_pt4(a, b)
term(292) = term(292) + t1(a,i) * wm_interm_45_triplet_pt4(b, p, i, q) * wm_interm_90_triplet_pt4(a, b)
term(293) = term(293) + t1(a,i) * wm_interm_45_triplet_pt4(b, p, q, i) * wm_interm_90_triplet_pt4(a, b)
term(294) = term(294) + t1(a,i) * wm_interm_45_triplet_pt4(b, p, i, q) * wm_interm_91_triplet_pt4(a, b)
term(295) = term(295) + t1(a,i) * wm_interm_45_triplet_pt4(b, p, q, i) * wm_interm_91_triplet_pt4(a, b)
term(296) = term(296) + r1(vrdav_Rr, a,i) * wm_interm_60_triplet_pt4(b, p, i, q) * wm_interm_90_triplet_pt4(a, b)
term(297) = term(297) + r1(vrdav_Rr, a,i) * wm_interm_60_triplet_pt4(b, p, i, q) * wm_interm_91_triplet_pt4(a, b)
term(298) = term(298) + t2(a,b,q,i) * wm_interm_0_triplet_pt4(b, i) * wm_interm_46_triplet_pt4(a, p)
term(299) = term(299) + t2(a,b,q,i) * wm_interm_0_triplet_pt4(b, i) * wm_interm_44_triplet_pt4(a, p)
term(300) = term(300) + t1(a,i) * wm_interm_56_triplet_pt4(a, b) * wm_interm_92_triplet_pt4(b, q, i, p)
term(301) = term(301) + t1(a,i) * wm_interm_58_triplet_pt4(a, b) * wm_interm_92_triplet_pt4(b, q, i, p)
term(302) = term(302) + t1(a,i) * wm_interm_58_triplet_pt4(a, b) * wm_interm_94_triplet_pt4(b, q, i, p)
term(303) = term(303) + t1(a,i) * wm_interm_56_triplet_pt4(a, b) * wm_interm_94_triplet_pt4(b, q, i, p)
term(304) = term(304) + t1(a,i) * wm_interm_45_triplet_pt4(b, p, i, q) * wm_interm_99_triplet_pt4(a, b)
term(305) = term(305) + t1(a,i) * wm_interm_45_triplet_pt4(b, p, q, i) * wm_interm_99_triplet_pt4(a, b)
term(306) = term(306) + t1(a,i) * wm_interm_100_triplet_pt4(a, b) * wm_interm_45_triplet_pt4(b, p, i, q)
term(307) = term(307) + t1(a,i) * wm_interm_100_triplet_pt4(a, b) * wm_interm_45_triplet_pt4(b, p, q, i)
term(308) = term(308) + r1(vrdav_Rr, a,i) * wm_interm_27_triplet_pt4(b, p, i, q) * wm_interm_56_triplet_pt4(a, b)
term(309) = term(309) + r1(vrdav_Rr, a,i) * wm_interm_27_triplet_pt4(b, p, i, q) * wm_interm_58_triplet_pt4(a, b)
term(310) = term(310) + r1(vrdav_Rr, a,i) * wm_interm_60_triplet_pt4(b, p, i, q) * wm_interm_99_triplet_pt4(a, b)
term(311) = term(311) + r1(vrdav_Rr, a,i) * wm_interm_100_triplet_pt4(a, b) * wm_interm_60_triplet_pt4(b, p, i, q)
term(312) = term(312) + r1(vrdav_Rr, a,i) * wm_interm_24_triplet_pt4(b, p, i, q) * wm_interm_56_triplet_pt4(a, b)
term(313) = term(313) + r1(vrdav_Rr, a,i) * wm_interm_24_triplet_pt4(b, p, i, q) * wm_interm_58_triplet_pt4(a, b)
term(314) = term(314) + t2(a,b,q,i) * wm_interm_20_triplet_pt4(b, i) * wm_interm_46_triplet_pt4(a, p)
term(315) = term(315) + t2(a,b,q,i) * wm_interm_21_triplet_pt4(b, i) * wm_interm_46_triplet_pt4(a, p)
term(316) = term(316) + t2(a,b,q,i) * wm_interm_20_triplet_pt4(b, i) * wm_interm_44_triplet_pt4(a, p)
term(317) = term(317) + t2(a,b,q,i) * wm_interm_21_triplet_pt4(b, i) * wm_interm_44_triplet_pt4(a, p)
term(318) = term(318) + t2(a,b,q,i) * wm_interm_20_triplet_pt4(a, p) * wm_interm_46_triplet_pt4(b, i)
term(319) = term(319) + t2(a,b,q,i) * wm_interm_21_triplet_pt4(a, p) * wm_interm_46_triplet_pt4(b, i)
term(320) = term(320) + t2(a,b,q,i) * wm_interm_20_triplet_pt4(a, p) * wm_interm_44_triplet_pt4(b, i)
term(321) = term(321) + t2(a,b,q,i) * wm_interm_21_triplet_pt4(a, p) * wm_interm_44_triplet_pt4(b, i)
end do 
end do 
end do 

term(262) = term(262) * 16.0d+0 
term(263) = term(263) * (-8.0d+0) 
term(264) = term(264) * (-32.0d+0) 
term(265) = term(265) * 16.0d+0 
term(266) = term(266) * (-32.0d+0) 
term(267) = term(267) * 16.0d+0 
term(268) = term(268) * 16.0d+0 
term(269) = term(269) * (-8.0d+0) 
term(270) = term(270) * 24.0d+0 
term(271) = term(271) * 12.0d+0 
term(272) = term(272) * (-12.0d+0) 
term(273) = term(273) * (-6.0d+0) 
term(274) = term(274) * 6.0d+0 
term(275) = term(275) * (-48.0d+0) 
term(276) = term(276) * (-16.0d+0) 
term(277) = term(277) * 32.0d+0 
term(278) = term(278) * (-16.0d+0) 
term(279) = term(279) * 32.0d+0 
term(280) = term(280) * 8.0d+0 
term(281) = term(281) * (-16.0d+0) 
term(282) = term(282) * 32.0d+0 
term(283) = term(283) * (-64.0d+0) 
term(284) = term(284) * 4.0d+0 
term(285) = term(285) * (-8.0d+0) 
term(286) = term(286) * 4.0d+0 
term(287) = term(287) * (-8.0d+0) 
term(288) = term(288) * (-8.0d+0) 
term(289) = term(289) * 16.0d+0 
term(290) = term(290) * (-8.0d+0) 
term(291) = term(291) * 16.0d+0 
term(292) = term(292) * 2.0d+0 
term(293) = term(293) * (-4.0d+0) 
term(294) = term(294) * 2.0d+0 
term(295) = term(295) * (-4.0d+0) 
term(296) = term(296) * 3.0d+0 
term(297) = term(297) * (-3.0d+0) 
term(298) = term(298) * 8.0d+0 
term(299) = term(299) * (-4.0d+0) 
term(300) = term(300) * (-16.0d+0) 
term(301) = term(301) * 8.0d+0 
term(302) = term(302) * (-4.0d+0) 
term(303) = term(303) * 8.0d+0 
term(304) = term(304) * (-8.0d+0) 
term(305) = term(305) * 16.0d+0 
term(306) = term(306) * 16.0d+0 
term(307) = term(307) * (-32.0d+0) 
term(308) = term(308) * (-8.0d+0) 
term(309) = term(309) * 4.0d+0 
term(310) = term(310) * (-4.0d+0) 
term(311) = term(311) * 8.0d+0 
term(312) = term(312) * 4.0d+0 
term(313) = term(313) * (-2.0d+0) 
term(314) = term(314) * 32.0d+0 
term(315) = term(315) * (-16.0d+0) 
term(316) = term(316) * (-16.0d+0) 
term(317) = term(317) * 8.0d+0 
term(318) = term(318) * 32.0d+0 
term(319) = term(319) * (-16.0d+0) 
term(320) = term(320) * (-16.0d+0) 
term(321) = term(321) * 8.0d+0 

do a = nocc + 1, nactive 
term(322) = term(322) + wm_interm_0_triplet_pt4(a, q) * wm_interm_1_triplet_pt4(a, p)
term(323) = term(323) + wm_interm_0_triplet_pt4(a, q) * wm_interm_2_triplet_pt4(a, p)
term(324) = term(324) + wm_interm_0_triplet_pt4(a, q) * wm_interm_3_triplet_pt4(a, p)
term(325) = term(325) + wm_interm_1_triplet_pt4(a, p) * wm_interm_20_triplet_pt4(a, q)
term(326) = term(326) + wm_interm_1_triplet_pt4(a, p) * wm_interm_21_triplet_pt4(a, q)
term(327) = term(327) + wm_interm_20_triplet_pt4(a, q) * wm_interm_2_triplet_pt4(a, p)
term(328) = term(328) + wm_interm_20_triplet_pt4(a, q) * wm_interm_3_triplet_pt4(a, p)
term(329) = term(329) + wm_interm_21_triplet_pt4(a, q) * wm_interm_2_triplet_pt4(a, p)
term(330) = term(330) + wm_interm_21_triplet_pt4(a, q) * wm_interm_3_triplet_pt4(a, p)
end do 

term(322) = term(322) * (-18.0d+0) 
term(323) = term(323) * (-24.0d+0) 
term(324) = term(324) * 12.0d+0 
term(325) = term(325) * 24.0d+0 
term(326) = term(326) * (-12.0d+0) 
term(327) = term(327) * 32.0d+0 
term(328) = term(328) * (-16.0d+0) 
term(329) = term(329) * (-16.0d+0) 
term(330) = term(330) * 8.0d+0 

do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(331) = term(331) + s2(a,b,i,j) * wm_interm_2_triplet_pt4(a, j) * wm_interm_47_triplet_pt4(b, i)
term(332) = term(332) + s2(a,b,i,j) * wm_interm_2_triplet_pt4(a, i) * wm_interm_47_triplet_pt4(b, j)
term(333) = term(333) + s2(a,b,i,j) * wm_interm_3_triplet_pt4(a, j) * wm_interm_47_triplet_pt4(b, i)
term(334) = term(334) + s2(a,b,i,j) * wm_interm_3_triplet_pt4(a, i) * wm_interm_47_triplet_pt4(b, j)
term(335) = term(335) + s2(a,b,i,j) * wm_interm_2_triplet_pt4(a, i) * wm_interm_52_triplet_pt4(b, j)
term(336) = term(336) + s2(a,b,i,j) * wm_interm_3_triplet_pt4(a, i) * wm_interm_52_triplet_pt4(b, j)
term(337) = term(337) + s2(a,b,i,j) * wm_interm_2_triplet_pt4(a, j) * wm_interm_52_triplet_pt4(b, i)
term(338) = term(338) + s2(a,b,i,j) * wm_interm_3_triplet_pt4(a, j) * wm_interm_52_triplet_pt4(b, i)
term(339) = term(339) + s2(a,b,i,q) * wm_interm_47_triplet_pt4(b, j) * wm_interm_79_triplet_pt4(a, i, p, j)
term(340) = term(340) + s2(a,b,i,q) * wm_interm_47_triplet_pt4(b, j) * wm_interm_79_triplet_pt4(a, p, i, j)
term(341) = term(341) + s2(a,b,i,q) * wm_interm_14_triplet_pt4(a, p, i, j) * wm_interm_47_triplet_pt4(b, j)
term(342) = term(342) + s2(a,b,i,q) * wm_interm_14_triplet_pt4(a, i, p, j) * wm_interm_47_triplet_pt4(b, j)
term(343) = term(343) + s2(a,b,i,q) * wm_interm_14_triplet_pt4(a, p, i, j) * wm_interm_52_triplet_pt4(b, j)
term(344) = term(344) + s2(a,b,i,q) * wm_interm_14_triplet_pt4(a, i, p, j) * wm_interm_52_triplet_pt4(b, j)
term(345) = term(345) + s2(a,b,i,q) * wm_interm_52_triplet_pt4(b, j) * wm_interm_79_triplet_pt4(a, i, p, j)
term(346) = term(346) + s2(a,b,i,q) * wm_interm_52_triplet_pt4(b, j) * wm_interm_79_triplet_pt4(a, p, i, j)
term(347) = term(347) + s2(a,b,i,j) * wm_interm_47_triplet_pt4(b, i) * wm_interm_79_triplet_pt4(a, j, p, q)
term(348) = term(348) + s2(a,b,i,j) * wm_interm_47_triplet_pt4(b, i) * wm_interm_79_triplet_pt4(a, p, j, q)
term(349) = term(349) + s2(a,b,i,j) * wm_interm_47_triplet_pt4(b, j) * wm_interm_79_triplet_pt4(a, i, p, q)
term(350) = term(350) + s2(a,b,i,j) * wm_interm_47_triplet_pt4(b, j) * wm_interm_79_triplet_pt4(a, p, i, q)
term(351) = term(351) + s2(a,b,i,j) * wm_interm_14_triplet_pt4(a, p, j, q) * wm_interm_47_triplet_pt4(b, i)
term(352) = term(352) + s2(a,b,i,j) * wm_interm_14_triplet_pt4(a, p, i, q) * wm_interm_47_triplet_pt4(b, j)
term(353) = term(353) + s2(a,b,i,j) * wm_interm_14_triplet_pt4(a, j, p, q) * wm_interm_47_triplet_pt4(b, i)
term(354) = term(354) + s2(a,b,i,j) * wm_interm_14_triplet_pt4(a, i, p, q) * wm_interm_47_triplet_pt4(b, j)
term(355) = term(355) + s2(a,b,i,j) * wm_interm_52_triplet_pt4(b, j) * wm_interm_79_triplet_pt4(a, i, p, q)
term(356) = term(356) + s2(a,b,i,j) * wm_interm_52_triplet_pt4(b, j) * wm_interm_79_triplet_pt4(a, p, i, q)
term(357) = term(357) + s2(a,b,i,j) * wm_interm_14_triplet_pt4(a, p, i, q) * wm_interm_52_triplet_pt4(b, j)
term(358) = term(358) + s2(a,b,i,j) * wm_interm_14_triplet_pt4(a, i, p, q) * wm_interm_52_triplet_pt4(b, j)
term(359) = term(359) + s2(a,b,i,j) * wm_interm_52_triplet_pt4(b, i) * wm_interm_79_triplet_pt4(a, j, p, q)
term(360) = term(360) + s2(a,b,i,j) * wm_interm_52_triplet_pt4(b, i) * wm_interm_79_triplet_pt4(a, p, j, q)
term(361) = term(361) + s2(a,b,i,j) * wm_interm_14_triplet_pt4(a, p, j, q) * wm_interm_52_triplet_pt4(b, i)
term(362) = term(362) + s2(a,b,i,j) * wm_interm_14_triplet_pt4(a, j, p, q) * wm_interm_52_triplet_pt4(b, i)
term(363) = term(363) + s2(a,b,i,j) * wm_interm_2_triplet_pt4(a, j) * wm_interm_51_triplet_pt4(b, p, i, q)
term(364) = term(364) + s2(a,b,i,j) * wm_interm_2_triplet_pt4(a, i) * wm_interm_51_triplet_pt4(b, p, j, q)
term(365) = term(365) + s2(a,b,i,j) * wm_interm_3_triplet_pt4(a, j) * wm_interm_51_triplet_pt4(b, p, i, q)
term(366) = term(366) + s2(a,b,i,j) * wm_interm_3_triplet_pt4(a, i) * wm_interm_51_triplet_pt4(b, p, j, q)
term(367) = term(367) + s2(a,b,i,j) * wm_interm_2_triplet_pt4(a, i) * wm_interm_51_triplet_pt4(b, p, q, j)
term(368) = term(368) + s2(a,b,i,j) * wm_interm_3_triplet_pt4(a, i) * wm_interm_51_triplet_pt4(b, p, q, j)
term(369) = term(369) + s2(a,b,i,j) * wm_interm_43_triplet_pt4(a, p, q, j) * wm_interm_82_triplet_pt4(b, i)
term(370) = term(370) + s2(a,b,i,j) * wm_interm_43_triplet_pt4(a, p, j, q) * wm_interm_82_triplet_pt4(b, i)
term(371) = term(371) + s2(a,b,i,j) * wm_interm_43_triplet_pt4(a, p, i, q) * wm_interm_82_triplet_pt4(b, j)
term(372) = term(372) + s2(a,b,i,j) * wm_interm_43_triplet_pt4(a, p, q, j) * wm_interm_84_triplet_pt4(b, i)
term(373) = term(373) + s2(a,b,i,j) * wm_interm_43_triplet_pt4(a, p, q, j) * wm_interm_83_triplet_pt4(b, i)
term(374) = term(374) + s2(a,b,i,j) * wm_interm_43_triplet_pt4(a, p, j, q) * wm_interm_84_triplet_pt4(b, i)
term(375) = term(375) + s2(a,b,i,j) * wm_interm_43_triplet_pt4(a, p, j, q) * wm_interm_83_triplet_pt4(b, i)
term(376) = term(376) + s2(a,b,i,j) * wm_interm_43_triplet_pt4(a, p, i, q) * wm_interm_84_triplet_pt4(b, j)
term(377) = term(377) + s2(a,b,i,j) * wm_interm_43_triplet_pt4(a, p, i, q) * wm_interm_83_triplet_pt4(b, j)
term(378) = term(378) + t2(a,b,i,j) * wm_interm_0_triplet_pt4(b, i) * wm_interm_45_triplet_pt4(a, p, j, q)
term(379) = term(379) + t2(a,b,i,j) * wm_interm_0_triplet_pt4(b, i) * wm_interm_44_triplet_pt4(a, j)
term(380) = term(380) + t2(a,b,i,j) * wm_interm_0_triplet_pt4(b, i) * wm_interm_45_triplet_pt4(a, p, q, j)
term(381) = term(381) + t2(a,b,i,j) * wm_interm_0_triplet_pt4(b, i) * wm_interm_46_triplet_pt4(a, j)
term(382) = term(382) + t2(a,b,i,j) * wm_interm_0_triplet_pt4(b, j) * wm_interm_46_triplet_pt4(a, i)
term(383) = term(383) + t2(a,b,i,j) * wm_interm_0_triplet_pt4(b, j) * wm_interm_45_triplet_pt4(a, p, i, q)
term(384) = term(384) + t2(a,b,i,j) * wm_interm_0_triplet_pt4(b, j) * wm_interm_44_triplet_pt4(a, i)
term(385) = term(385) + t2(a,b,i,j) * wm_interm_60_triplet_pt4(a, p, j, q) * wm_interm_86_triplet_pt4(b, i)
term(386) = term(386) + t2(a,b,i,j) * wm_interm_60_triplet_pt4(a, j, p, q) * wm_interm_86_triplet_pt4(b, i)
term(387) = term(387) + t2(a,b,i,j) * wm_interm_60_triplet_pt4(a, i, p, q) * wm_interm_86_triplet_pt4(b, j)
term(388) = term(388) + t2(a,b,i,j) * wm_interm_60_triplet_pt4(a, p, i, q) * wm_interm_86_triplet_pt4(b, j)
term(389) = term(389) + t2(a,b,i,j) * wm_interm_20_triplet_pt4(b, i) * wm_interm_45_triplet_pt4(a, p, j, q)
term(390) = term(390) + t2(a,b,i,j) * wm_interm_20_triplet_pt4(b, i) * wm_interm_44_triplet_pt4(a, j)
term(391) = term(391) + t2(a,b,i,j) * wm_interm_20_triplet_pt4(b, i) * wm_interm_45_triplet_pt4(a, p, q, j)
term(392) = term(392) + t2(a,b,i,j) * wm_interm_20_triplet_pt4(b, i) * wm_interm_46_triplet_pt4(a, j)
term(393) = term(393) + t2(a,b,i,j) * wm_interm_20_triplet_pt4(b, j) * wm_interm_46_triplet_pt4(a, i)
term(394) = term(394) + t2(a,b,i,j) * wm_interm_20_triplet_pt4(b, j) * wm_interm_45_triplet_pt4(a, p, i, q)
term(395) = term(395) + t2(a,b,i,j) * wm_interm_20_triplet_pt4(b, j) * wm_interm_44_triplet_pt4(a, i)
term(396) = term(396) + t2(a,b,i,j) * wm_interm_21_triplet_pt4(b, i) * wm_interm_45_triplet_pt4(a, p, j, q)
term(397) = term(397) + t2(a,b,i,j) * wm_interm_21_triplet_pt4(b, i) * wm_interm_44_triplet_pt4(a, j)
term(398) = term(398) + t2(a,b,i,j) * wm_interm_21_triplet_pt4(b, i) * wm_interm_45_triplet_pt4(a, p, q, j)
term(399) = term(399) + t2(a,b,i,j) * wm_interm_21_triplet_pt4(b, i) * wm_interm_46_triplet_pt4(a, j)
term(400) = term(400) + t2(a,b,i,j) * wm_interm_21_triplet_pt4(b, j) * wm_interm_46_triplet_pt4(a, i)
term(401) = term(401) + t2(a,b,i,j) * wm_interm_21_triplet_pt4(b, j) * wm_interm_45_triplet_pt4(a, p, i, q)
term(402) = term(402) + t2(a,b,i,j) * wm_interm_21_triplet_pt4(b, j) * wm_interm_44_triplet_pt4(a, i)
term(403) = term(403) + t2(a,b,i,j) * wm_interm_27_triplet_pt4(b, p, i, q) * wm_interm_44_triplet_pt4(a, j)
term(404) = term(404) + t2(a,b,i,j) * wm_interm_27_triplet_pt4(b, p, i, q) * wm_interm_46_triplet_pt4(a, j)
term(405) = term(405) + t2(a,b,i,j) * wm_interm_60_triplet_pt4(a, p, j, q) * wm_interm_93_triplet_pt4(b, i)
term(406) = term(406) + t2(a,b,i,j) * wm_interm_60_triplet_pt4(a, j, p, q) * wm_interm_93_triplet_pt4(b, i)
term(407) = term(407) + t2(a,b,i,j) * wm_interm_27_triplet_pt4(b, p, j, q) * wm_interm_46_triplet_pt4(a, i)
term(408) = term(408) + t2(a,b,i,j) * wm_interm_60_triplet_pt4(a, i, p, q) * wm_interm_93_triplet_pt4(b, j)
term(409) = term(409) + t2(a,b,i,j) * wm_interm_27_triplet_pt4(b, p, j, q) * wm_interm_44_triplet_pt4(a, i)
term(410) = term(410) + t2(a,b,i,j) * wm_interm_60_triplet_pt4(a, p, i, q) * wm_interm_93_triplet_pt4(b, j)
term(411) = term(411) + t2(a,b,i,j) * wm_interm_60_triplet_pt4(a, p, j, q) * wm_interm_96_triplet_pt4(b, i)
term(412) = term(412) + t2(a,b,i,j) * wm_interm_27_triplet_pt4(b, i, p, q) * wm_interm_44_triplet_pt4(a, j)
term(413) = term(413) + t2(a,b,i,j) * wm_interm_24_triplet_pt4(b, i, p, q) * wm_interm_44_triplet_pt4(a, j)
term(414) = term(414) + t2(a,b,i,j) * wm_interm_60_triplet_pt4(a, j, p, q) * wm_interm_96_triplet_pt4(b, i)
term(415) = term(415) + t2(a,b,i,j) * wm_interm_24_triplet_pt4(b, i, p, q) * wm_interm_46_triplet_pt4(a, j)
term(416) = term(416) + t2(a,b,i,j) * wm_interm_27_triplet_pt4(b, i, p, q) * wm_interm_46_triplet_pt4(a, j)
term(417) = term(417) + t2(a,b,i,j) * wm_interm_60_triplet_pt4(a, i, p, q) * wm_interm_96_triplet_pt4(b, j)
term(418) = term(418) + t2(a,b,i,j) * wm_interm_27_triplet_pt4(b, j, p, q) * wm_interm_46_triplet_pt4(a, i)
term(419) = term(419) + t2(a,b,i,j) * wm_interm_60_triplet_pt4(a, p, i, q) * wm_interm_96_triplet_pt4(b, j)
term(420) = term(420) + t2(a,b,i,j) * wm_interm_27_triplet_pt4(b, j, p, q) * wm_interm_44_triplet_pt4(a, i)
term(421) = term(421) + t2(a,b,i,j) * wm_interm_24_triplet_pt4(b, p, i, q) * wm_interm_44_triplet_pt4(a, j)
term(422) = term(422) + t2(a,b,i,j) * wm_interm_24_triplet_pt4(b, p, i, q) * wm_interm_46_triplet_pt4(a, j)
term(423) = term(423) + t2(a,b,i,j) * wm_interm_24_triplet_pt4(b, p, j, q) * wm_interm_46_triplet_pt4(a, i)
term(424) = term(424) + t2(a,b,i,j) * wm_interm_24_triplet_pt4(b, j, p, q) * wm_interm_46_triplet_pt4(a, i)
term(425) = term(425) + t2(a,b,i,j) * wm_interm_24_triplet_pt4(b, p, j, q) * wm_interm_44_triplet_pt4(a, i)
term(426) = term(426) + t2(a,b,i,j) * wm_interm_24_triplet_pt4(b, j, p, q) * wm_interm_44_triplet_pt4(a, i)
end do 
end do 
end do 
end do 

term(331) = term(331) * 16.0d+0 
term(332) = term(332) * (-32.0d+0) 
term(333) = term(333) * (-8.0d+0) 
term(334) = term(334) * 16.0d+0 
term(335) = term(335) * 64.0d+0 
term(336) = term(336) * (-32.0d+0) 
term(337) = term(337) * (-32.0d+0) 
term(338) = term(338) * 16.0d+0 
term(339) = term(339) * 4.0d+0 
term(340) = term(340) * (-8.0d+0) 
term(341) = term(341) * 4.0d+0 
term(342) = term(342) * (-8.0d+0) 
term(343) = term(343) * (-8.0d+0) 
term(344) = term(344) * 16.0d+0 
term(345) = term(345) * (-8.0d+0) 
term(346) = term(346) * 16.0d+0 
term(347) = term(347) * (-2.0d+0) 
term(348) = term(348) * 4.0d+0 
term(349) = term(349) * 4.0d+0 
term(350) = term(350) * (-8.0d+0) 
term(351) = term(351) * (-2.0d+0) 
term(352) = term(352) * 4.0d+0 
term(353) = term(353) * 4.0d+0 
term(354) = term(354) * (-8.0d+0) 
term(355) = term(355) * (-8.0d+0) 
term(356) = term(356) * 16.0d+0 
term(357) = term(357) * (-8.0d+0) 
term(358) = term(358) * 16.0d+0 
term(359) = term(359) * 4.0d+0 
term(360) = term(360) * (-8.0d+0) 
term(361) = term(361) * 4.0d+0 
term(362) = term(362) * (-8.0d+0) 
term(363) = term(363) * (-8.0d+0) 
term(364) = term(364) * 16.0d+0 
term(365) = term(365) * 4.0d+0 
term(366) = term(366) * (-8.0d+0) 
term(367) = term(367) * (-32.0d+0) 
term(368) = term(368) * 16.0d+0 
term(369) = term(369) * 24.0d+0 
term(370) = term(370) * (-12.0d+0) 
term(371) = term(371) * 24.0d+0 
term(372) = term(372) * (-16.0d+0) 
term(373) = term(373) * 32.0d+0 
term(374) = term(374) * 8.0d+0 
term(375) = term(375) * (-16.0d+0) 
term(376) = term(376) * (-16.0d+0) 
term(377) = term(377) * 32.0d+0 
term(378) = term(378) * 4.0d+0 
term(379) = term(379) * (-8.0d+0) 
term(380) = term(380) * (-8.0d+0) 
term(381) = term(381) * 16.0d+0 
term(382) = term(382) * (-32.0d+0) 
term(383) = term(383) * (-8.0d+0) 
term(384) = term(384) * 16.0d+0 
term(385) = term(385) * 6.0d+0 
term(386) = term(386) * (-12.0d+0) 
term(387) = term(387) * 24.0d+0 
term(388) = term(388) * (-12.0d+0) 
term(389) = term(389) * 16.0d+0 
term(390) = term(390) * (-32.0d+0) 
term(391) = term(391) * (-32.0d+0) 
term(392) = term(392) * 64.0d+0 
term(393) = term(393) * (-128.0d+0) 
term(394) = term(394) * (-32.0d+0) 
term(395) = term(395) * 64.0d+0 
term(396) = term(396) * (-8.0d+0) 
term(397) = term(397) * 16.0d+0 
term(398) = term(398) * 16.0d+0 
term(399) = term(399) * (-32.0d+0) 
term(400) = term(400) * 64.0d+0 
term(401) = term(401) * 16.0d+0 
term(402) = term(402) * (-32.0d+0) 
term(403) = term(403) * 4.0d+0 
term(404) = term(404) * (-8.0d+0) 
term(405) = term(405) * 8.0d+0 
term(406) = term(406) * (-16.0d+0) 
term(407) = term(407) * 16.0d+0 
term(408) = term(408) * 32.0d+0 
term(409) = term(409) * (-8.0d+0) 
term(410) = term(410) * (-16.0d+0) 
term(411) = term(411) * (-4.0d+0) 
term(412) = term(412) * (-2.0d+0) 
term(413) = term(413) * 4.0d+0 
term(414) = term(414) * 8.0d+0 
term(415) = term(415) * (-8.0d+0) 
term(416) = term(416) * 4.0d+0 
term(417) = term(417) * (-16.0d+0) 
term(418) = term(418) * (-8.0d+0) 
term(419) = term(419) * 8.0d+0 
term(420) = term(420) * 4.0d+0 
term(421) = term(421) * (-2.0d+0) 
term(422) = term(422) * 4.0d+0 
term(423) = term(423) * (-8.0d+0) 
term(424) = term(424) * 16.0d+0 
term(425) = term(425) * 4.0d+0 
term(426) = term(426) * (-8.0d+0) 

do i = 1, nocc 
term(427) = term(427) + wm_interm_5_triplet_pt4(q, i) * wm_interm_6_triplet_pt4(i, p)
term(428) = term(428) + wm_interm_12_triplet_pt4(i, p) * wm_interm_5_triplet_pt4(q, i)
term(429) = term(429) + wm_interm_13_triplet_pt4(i, p) * wm_interm_5_triplet_pt4(q, i)
term(430) = term(430) + wm_interm_5_triplet_pt4(i, q) * wm_interm_6_triplet_pt4(p, i)
term(431) = term(431) + wm_interm_13_triplet_pt4(p, i) * wm_interm_5_triplet_pt4(i, q)
term(432) = term(432) + wm_interm_12_triplet_pt4(p, i) * wm_interm_5_triplet_pt4(i, q)
term(433) = term(433) + wm_interm_23_triplet_pt4(i, p) * wm_interm_5_triplet_pt4(q, i)
term(434) = term(434) + wm_interm_26_triplet_pt4(i, p) * wm_interm_5_triplet_pt4(q, i)
term(435) = term(435) + wm_interm_30_triplet_pt4(i, p) * wm_interm_5_triplet_pt4(q, i)
term(436) = term(436) + wm_interm_31_triplet_pt4(i, p) * wm_interm_5_triplet_pt4(q, i)
term(437) = term(437) + wm_interm_34_triplet_pt4(i, p) * wm_interm_5_triplet_pt4(q, i)
term(438) = term(438) + wm_interm_35_triplet_pt4(i, p) * wm_interm_5_triplet_pt4(q, i)
term(439) = term(439) + wm_interm_26_triplet_pt4(p, i) * wm_interm_5_triplet_pt4(i, q)
term(440) = term(440) + wm_interm_23_triplet_pt4(p, i) * wm_interm_5_triplet_pt4(i, q)
term(441) = term(441) + wm_interm_34_triplet_pt4(p, i) * wm_interm_5_triplet_pt4(i, q)
term(442) = term(442) + wm_interm_35_triplet_pt4(p, i) * wm_interm_5_triplet_pt4(i, q)
term(443) = term(443) + wm_interm_30_triplet_pt4(p, i) * wm_interm_5_triplet_pt4(i, q)
term(444) = term(444) + wm_interm_31_triplet_pt4(p, i) * wm_interm_5_triplet_pt4(i, q)
term(445) = term(445) + wm_interm_41_triplet_pt4(i, p) * wm_interm_5_triplet_pt4(q, i)
term(446) = term(446) + wm_interm_50_triplet_pt4(p, i) * wm_interm_59_triplet_pt4(q, i)
term(447) = term(447) + wm_interm_41_triplet_pt4(p, i) * wm_interm_5_triplet_pt4(i, q)
term(448) = term(448) + wm_interm_50_triplet_pt4(i, p) * wm_interm_59_triplet_pt4(i, q)
end do 

term(427) = term(427) * 6.0d+0 
term(428) = term(428) * (-6.0d+0) 
term(429) = term(429) * 6.0d+0 
term(430) = term(430) * 6.0d+0 
term(431) = term(431) * 6.0d+0 
term(432) = term(432) * (-6.0d+0) 
term(433) = term(433) * 6.0d+0 
term(434) = term(434) * (-6.0d+0) 
term(435) = term(435) * (-4.0d+0) 
term(436) = term(436) * 8.0d+0 
term(437) = term(437) * (-4.0d+0) 
term(438) = term(438) * 8.0d+0 
term(439) = term(439) * (-6.0d+0) 
term(440) = term(440) * 6.0d+0 
term(441) = term(441) * (-4.0d+0) 
term(442) = term(442) * 8.0d+0 
term(443) = term(443) * (-4.0d+0) 
term(444) = term(444) * 8.0d+0 
term(445) = term(445) * 2.0d+0 
term(446) = term(446) * 2.0d+0 
term(447) = term(447) * 2.0d+0 
term(448) = term(448) * 2.0d+0 

do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(449) = term(449) + s2(a,b,i,q) * wm_interm_42_triplet_pt4(a, j) * wm_interm_64_triplet_pt4(b, j, p, i)
term(450) = term(450) + s2(a,b,q,i) * wm_interm_42_triplet_pt4(a, j) * wm_interm_64_triplet_pt4(b, j, p, i)
term(451) = term(451) + s2(a,b,i,q) * wm_interm_48_triplet_pt4(a, j) * wm_interm_64_triplet_pt4(b, j, p, i)
term(452) = term(452) + s2(a,b,q,i) * wm_interm_48_triplet_pt4(a, j) * wm_interm_64_triplet_pt4(b, j, p, i)
term(453) = term(453) + s2(a,b,i,q) * wm_interm_42_triplet_pt4(a, j) * wm_interm_71_triplet_pt4(b, j, p, i)
term(454) = term(454) + s2(a,b,q,i) * wm_interm_42_triplet_pt4(a, j) * wm_interm_71_triplet_pt4(b, j, i, p)
term(455) = term(455) + s2(a,b,q,i) * wm_interm_42_triplet_pt4(a, j) * wm_interm_71_triplet_pt4(b, j, p, i)
term(456) = term(456) + s2(a,b,i,q) * wm_interm_48_triplet_pt4(a, j) * wm_interm_71_triplet_pt4(b, j, p, i)
term(457) = term(457) + s2(a,b,q,i) * wm_interm_48_triplet_pt4(a, j) * wm_interm_71_triplet_pt4(b, j, i, p)
term(458) = term(458) + s2(a,b,q,i) * wm_interm_48_triplet_pt4(a, j) * wm_interm_71_triplet_pt4(b, j, p, i)
term(459) = term(459) + s2(a,b,p,i) * wm_interm_2_triplet_pt4(a, j) * wm_interm_51_triplet_pt4(b, j, q, i)
term(460) = term(460) + s2(a,b,i,p) * wm_interm_2_triplet_pt4(a, j) * wm_interm_51_triplet_pt4(b, j, q, i)
term(461) = term(461) + s2(a,b,p,i) * wm_interm_3_triplet_pt4(a, j) * wm_interm_51_triplet_pt4(b, j, q, i)
term(462) = term(462) + s2(a,b,i,p) * wm_interm_3_triplet_pt4(a, j) * wm_interm_51_triplet_pt4(b, j, q, i)
term(463) = term(463) + s2(a,b,p,i) * wm_interm_2_triplet_pt4(a, j) * wm_interm_51_triplet_pt4(b, j, i, q)
term(464) = term(464) + s2(a,b,p,i) * wm_interm_3_triplet_pt4(a, j) * wm_interm_51_triplet_pt4(b, j, i, q)
term(465) = term(465) + s2(a,b,i,p) * wm_interm_43_triplet_pt4(a, j, q, i) * wm_interm_82_triplet_pt4(b, j)
term(466) = term(466) + s2(a,b,p,i) * wm_interm_43_triplet_pt4(a, j, q, i) * wm_interm_82_triplet_pt4(b, j)
term(467) = term(467) + s2(a,b,p,i) * wm_interm_43_triplet_pt4(a, j, i, q) * wm_interm_82_triplet_pt4(b, j)
term(468) = term(468) + s2(a,b,i,p) * wm_interm_43_triplet_pt4(a, j, q, i) * wm_interm_84_triplet_pt4(b, j)
term(469) = term(469) + s2(a,b,i,p) * wm_interm_43_triplet_pt4(a, j, q, i) * wm_interm_83_triplet_pt4(b, j)
term(470) = term(470) + s2(a,b,p,i) * wm_interm_43_triplet_pt4(a, j, q, i) * wm_interm_84_triplet_pt4(b, j)
term(471) = term(471) + s2(a,b,p,i) * wm_interm_43_triplet_pt4(a, j, q, i) * wm_interm_83_triplet_pt4(b, j)
term(472) = term(472) + s2(a,b,p,i) * wm_interm_43_triplet_pt4(a, j, i, q) * wm_interm_84_triplet_pt4(b, j)
term(473) = term(473) + s2(a,b,p,i) * wm_interm_43_triplet_pt4(a, j, i, q) * wm_interm_83_triplet_pt4(b, j)
term(474) = term(474) + t2(a,b,q,i) * wm_interm_20_triplet_pt4(b, j) * wm_interm_45_triplet_pt4(a, j, i, p)
term(475) = term(475) + t2(a,b,q,i) * wm_interm_21_triplet_pt4(b, j) * wm_interm_45_triplet_pt4(a, j, i, p)
term(476) = term(476) + t2(a,b,q,i) * wm_interm_20_triplet_pt4(a, j) * wm_interm_45_triplet_pt4(b, j, p, i)
term(477) = term(477) + t2(a,b,q,i) * wm_interm_20_triplet_pt4(a, j) * wm_interm_45_triplet_pt4(b, j, i, p)
term(478) = term(478) + t2(a,b,q,i) * wm_interm_21_triplet_pt4(a, j) * wm_interm_45_triplet_pt4(b, j, p, i)
term(479) = term(479) + t2(a,b,q,i) * wm_interm_21_triplet_pt4(a, j) * wm_interm_45_triplet_pt4(b, j, i, p)
term(480) = term(480) + t2(a,b,q,i) * wm_interm_20_triplet_pt4(b, j) * wm_interm_45_triplet_pt4(a, j, p, i)
term(481) = term(481) + t2(a,b,q,i) * wm_interm_21_triplet_pt4(b, j) * wm_interm_45_triplet_pt4(a, j, p, i)
end do 
end do 
end do 
end do 

term(449) = term(449) * 12.0d+0 
term(450) = term(450) * (-12.0d+0) 
term(451) = term(451) * (-24.0d+0) 
term(452) = term(452) * 24.0d+0 
term(453) = term(453) * 8.0d+0 
term(454) = term(454) * 8.0d+0 
term(455) = term(455) * (-16.0d+0) 
term(456) = term(456) * (-16.0d+0) 
term(457) = term(457) * (-16.0d+0) 
term(458) = term(458) * 32.0d+0 
term(459) = term(459) * 16.0d+0 
term(460) = term(460) * (-8.0d+0) 
term(461) = term(461) * (-8.0d+0) 
term(462) = term(462) * 4.0d+0 
term(463) = term(463) * (-8.0d+0) 
term(464) = term(464) * 4.0d+0 
term(465) = term(465) * 24.0d+0 
term(466) = term(466) * (-12.0d+0) 
term(467) = term(467) * 24.0d+0 
term(468) = term(468) * (-16.0d+0) 
term(469) = term(469) * 32.0d+0 
term(470) = term(470) * 8.0d+0 
term(471) = term(471) * (-16.0d+0) 
term(472) = term(472) * (-16.0d+0) 
term(473) = term(473) * 32.0d+0 
term(474) = term(474) * (-16.0d+0) 
term(475) = term(475) * 8.0d+0 
term(476) = term(476) * (-16.0d+0) 
term(477) = term(477) * 8.0d+0 
term(478) = term(478) * 8.0d+0 
term(479) = term(479) * (-4.0d+0) 
term(480) = term(480) * 8.0d+0 
term(481) = term(481) * (-4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(482) = term(482) + s2(a,b,i,q) * wm_interm_43_triplet_pt4(a, j, k, i) * wm_interm_64_triplet_pt4(b, k, p, j)
term(483) = term(483) + s2(a,b,i,j) * wm_interm_43_triplet_pt4(a, q, k, i) * wm_interm_64_triplet_pt4(b, k, p, j)
term(484) = term(484) + s2(a,b,q,i) * wm_interm_43_triplet_pt4(a, j, k, i) * wm_interm_64_triplet_pt4(b, k, p, j)
term(485) = term(485) + s2(a,b,i,j) * wm_interm_43_triplet_pt4(a, q, k, j) * wm_interm_64_triplet_pt4(b, k, p, i)
term(486) = term(486) + s2(a,b,i,q) * wm_interm_43_triplet_pt4(a, j, k, i) * wm_interm_71_triplet_pt4(b, k, p, j)
term(487) = term(487) + s2(a,b,i,j) * wm_interm_43_triplet_pt4(a, q, k, i) * wm_interm_71_triplet_pt4(b, k, j, p)
term(488) = term(488) + s2(a,b,i,j) * wm_interm_43_triplet_pt4(a, q, k, i) * wm_interm_71_triplet_pt4(b, k, p, j)
term(489) = term(489) + s2(a,b,q,i) * wm_interm_43_triplet_pt4(a, j, k, i) * wm_interm_71_triplet_pt4(b, k, p, j)
term(490) = term(490) + s2(a,b,i,j) * wm_interm_43_triplet_pt4(a, q, k, j) * wm_interm_71_triplet_pt4(b, k, p, i)
term(491) = term(491) + s2(a,b,p,i) * wm_interm_43_triplet_pt4(a, j, k, q) * wm_interm_64_triplet_pt4(b, k, i, j)
term(492) = term(492) + s2(a,b,i,j) * wm_interm_43_triplet_pt4(a, k, j, q) * wm_interm_64_triplet_pt4(b, p, k, i)
term(493) = term(493) + s2(a,b,i,j) * wm_interm_43_triplet_pt4(a, k, q, j) * wm_interm_64_triplet_pt4(b, p, k, i)
term(494) = term(494) + s2(a,b,i,j) * wm_interm_43_triplet_pt4(a, k, q, i) * wm_interm_64_triplet_pt4(b, p, k, j)
term(495) = term(495) + s2(a,b,i,j) * wm_interm_43_triplet_pt4(a, k, q, i) * wm_interm_71_triplet_pt4(b, p, k, j)
term(496) = term(496) + s2(a,b,i,j) * wm_interm_43_triplet_pt4(a, k, q, j) * wm_interm_71_triplet_pt4(b, p, k, i)
term(497) = term(497) + s2(a,b,i,j) * wm_interm_14_triplet_pt4(a, k, j, p) * wm_interm_51_triplet_pt4(b, k, q, i)
term(498) = term(498) + s2(a,b,i,j) * wm_interm_14_triplet_pt4(a, j, k, p) * wm_interm_51_triplet_pt4(b, k, q, i)
term(499) = term(499) + s2(a,b,i,j) * wm_interm_51_triplet_pt4(b, k, q, i) * wm_interm_79_triplet_pt4(a, j, k, p)
term(500) = term(500) + s2(a,b,i,j) * wm_interm_51_triplet_pt4(b, k, q, i) * wm_interm_79_triplet_pt4(a, k, j, p)
term(501) = term(501) + s2(a,b,i,j) * wm_interm_43_triplet_pt4(a, k, j, q) * wm_interm_71_triplet_pt4(b, p, k, i)
term(502) = term(502) + s2(a,b,p,i) * wm_interm_43_triplet_pt4(a, j, k, q) * wm_interm_71_triplet_pt4(b, k, i, j)
term(503) = term(503) + t2(a,b,i,j) * wm_interm_27_triplet_pt4(b, k, j, q) * wm_interm_45_triplet_pt4(a, k, p, i)
term(504) = term(504) + t2(a,b,i,j) * wm_interm_27_triplet_pt4(b, j, k, q) * wm_interm_45_triplet_pt4(a, k, p, i)
term(505) = term(505) + t2(a,b,i,j) * wm_interm_24_triplet_pt4(b, k, j, q) * wm_interm_45_triplet_pt4(a, k, p, i)
term(506) = term(506) + t2(a,b,i,j) * wm_interm_24_triplet_pt4(b, j, k, q) * wm_interm_45_triplet_pt4(a, k, p, i)
end do 
end do 
end do 
end do 
end do 

term(482) = term(482) * (-24.0d+0) 
term(483) = term(483) * 24.0d+0 
term(484) = term(484) * 12.0d+0 
term(485) = term(485) * (-12.0d+0) 
term(486) = term(486) * (-16.0d+0) 
term(487) = term(487) * (-16.0d+0) 
term(488) = term(488) * 32.0d+0 
term(489) = term(489) * 8.0d+0 
term(490) = term(490) * (-16.0d+0) 
term(491) = term(491) * (-18.0d+0) 
term(492) = term(492) * 6.0d+0 
term(493) = term(493) * (-6.0d+0) 
term(494) = term(494) * 6.0d+0 
term(495) = term(495) * 32.0d+0 
term(496) = term(496) * (-16.0d+0) 
term(497) = term(497) * 4.0d+0 
term(498) = term(498) * (-8.0d+0) 
term(499) = term(499) * 4.0d+0 
term(500) = term(500) * (-8.0d+0) 
term(501) = term(501) * 8.0d+0 
term(502) = term(502) * (-16.0d+0) 
term(503) = term(503) * (-8.0d+0) 
term(504) = term(504) * 4.0d+0 
term(505) = term(505) * 4.0d+0 
term(506) = term(506) * (-8.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(507) = term(507) + s1(a,i) * wm_interm_49_triplet_pt4(j, q, k, i) * wm_interm_64_triplet_pt4(a, k, p, j)
term(508) = term(508) + s1(a,i) * wm_interm_49_triplet_pt4(q, j, k, i) * wm_interm_64_triplet_pt4(a, k, p, j)
term(509) = term(509) + s1(a,i) * wm_interm_49_triplet_pt4(j, q, k, i) * wm_interm_71_triplet_pt4(a, k, p, j)
term(510) = term(510) + s1(a,i) * wm_interm_49_triplet_pt4(q, j, k, i) * wm_interm_71_triplet_pt4(a, k, p, j)
term(511) = term(511) + s1(a,i) * wm_interm_49_triplet_pt4(j, k, q, i) * wm_interm_71_triplet_pt4(a, p, k, j)
end do 
end do 
end do 
end do 

term(507) = term(507) * 12.0d+0 
term(508) = term(508) * (-12.0d+0) 
term(509) = term(509) * 8.0d+0 
term(510) = term(510) * (-16.0d+0) 
term(511) = term(511) * 4.0d+0 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(512) = term(512) + s2(a,b,i,j) * wm_interm_43_triplet_pt4(a, q, j, k) * wm_interm_64_triplet_pt4(b, k, p, i)
term(513) = term(513) + s2(a,b,q,i) * wm_interm_43_triplet_pt4(a, j, i, k) * wm_interm_64_triplet_pt4(b, k, p, j)
term(514) = term(514) + s2(a,b,i,j) * wm_interm_43_triplet_pt4(a, q, j, k) * wm_interm_71_triplet_pt4(b, k, p, i)
term(515) = term(515) + s2(a,b,q,i) * wm_interm_43_triplet_pt4(a, j, i, k) * wm_interm_71_triplet_pt4(b, k, p, j)
term(516) = term(516) + s2(a,b,i,j) * wm_interm_14_triplet_pt4(a, p, j, k) * wm_interm_51_triplet_pt4(b, q, k, i)
term(517) = term(517) + s2(a,b,i,j) * wm_interm_14_triplet_pt4(a, j, p, k) * wm_interm_51_triplet_pt4(b, q, k, i)
term(518) = term(518) + s2(a,b,i,j) * wm_interm_51_triplet_pt4(b, q, k, i) * wm_interm_79_triplet_pt4(a, j, p, k)
term(519) = term(519) + s2(a,b,i,j) * wm_interm_51_triplet_pt4(b, q, k, i) * wm_interm_79_triplet_pt4(a, p, j, k)
term(520) = term(520) + s2(a,b,p,i) * wm_interm_43_triplet_pt4(a, j, q, k) * wm_interm_64_triplet_pt4(b, k, i, j)
term(521) = term(521) + s2(a,b,i,j) * wm_interm_43_triplet_pt4(a, k, q, i) * wm_interm_64_triplet_pt4(b, p, j, k)
term(522) = term(522) + s2(a,b,p,i) * wm_interm_43_triplet_pt4(a, j, q, k) * wm_interm_71_triplet_pt4(b, k, i, j)
term(523) = term(523) + s2(a,b,i,j) * wm_interm_43_triplet_pt4(a, k, q, i) * wm_interm_71_triplet_pt4(b, p, j, k)
term(524) = term(524) + t2(a,b,q,i) * wm_interm_60_triplet_pt4(b, p, j, k) * wm_interm_85_triplet_pt4(a, i, k, j)
term(525) = term(525) + t2(a,b,q,i) * wm_interm_60_triplet_pt4(b, j, p, k) * wm_interm_85_triplet_pt4(a, i, k, j)
term(526) = term(526) + t2(a,b,q,i) * wm_interm_62_triplet_pt4(b, i, j, k) * wm_interm_85_triplet_pt4(a, k, p, j)
term(527) = term(527) + t2(a,b,q,i) * wm_interm_62_triplet_pt4(b, j, i, k) * wm_interm_85_triplet_pt4(a, k, p, j)
term(528) = term(528) + t2(a,b,q,i) * wm_interm_62_triplet_pt4(a, i, j, k) * wm_interm_85_triplet_pt4(b, k, p, j)
term(529) = term(529) + t2(a,b,q,i) * wm_interm_62_triplet_pt4(a, j, i, k) * wm_interm_85_triplet_pt4(b, k, p, j)
term(530) = term(530) + t2(a,b,q,i) * wm_interm_62_triplet_pt4(a, p, j, k) * wm_interm_85_triplet_pt4(b, i, k, j)
term(531) = term(531) + t2(a,b,q,i) * wm_interm_62_triplet_pt4(a, j, p, k) * wm_interm_85_triplet_pt4(b, i, k, j)
term(532) = term(532) + t2(a,b,q,i) * wm_interm_62_triplet_pt4(a, p, j, k) * wm_interm_85_triplet_pt4(b, k, i, j)
term(533) = term(533) + t2(a,b,q,i) * wm_interm_62_triplet_pt4(a, j, p, k) * wm_interm_85_triplet_pt4(b, k, i, j)
term(534) = term(534) + t2(a,b,q,i) * wm_interm_62_triplet_pt4(b, j, p, k) * wm_interm_85_triplet_pt4(a, k, i, j)
term(535) = term(535) + t2(a,b,q,i) * wm_interm_62_triplet_pt4(b, p, j, k) * wm_interm_85_triplet_pt4(a, k, i, j)
term(536) = term(536) + t2(a,b,i,j) * wm_interm_24_triplet_pt4(b, q, j, k) * wm_interm_45_triplet_pt4(a, p, k, i)
term(537) = term(537) + t2(a,b,i,j) * wm_interm_24_triplet_pt4(b, j, q, k) * wm_interm_45_triplet_pt4(a, p, k, i)
term(538) = term(538) + t2(a,b,i,j) * wm_interm_27_triplet_pt4(b, q, j, k) * wm_interm_45_triplet_pt4(a, p, k, i)
term(539) = term(539) + t2(a,b,i,j) * wm_interm_27_triplet_pt4(b, j, q, k) * wm_interm_45_triplet_pt4(a, p, k, i)
term(540) = term(540) + t2(a,b,q,i) * wm_interm_60_triplet_pt4(b, p, j, k) * wm_interm_94_triplet_pt4(a, i, k, j)
term(541) = term(541) + t2(a,b,q,i) * wm_interm_60_triplet_pt4(b, j, p, k) * wm_interm_94_triplet_pt4(a, i, k, j)
term(542) = term(542) + t2(a,b,q,i) * wm_interm_62_triplet_pt4(b, i, j, k) * wm_interm_94_triplet_pt4(a, p, k, j)
term(543) = term(543) + t2(a,b,q,i) * wm_interm_62_triplet_pt4(b, j, i, k) * wm_interm_94_triplet_pt4(a, p, k, j)
term(544) = term(544) + t2(a,b,q,i) * wm_interm_62_triplet_pt4(a, i, j, k) * wm_interm_94_triplet_pt4(b, p, k, j)
term(545) = term(545) + t2(a,b,q,i) * wm_interm_62_triplet_pt4(a, j, i, k) * wm_interm_94_triplet_pt4(b, p, k, j)
term(546) = term(546) + t2(a,b,q,i) * wm_interm_62_triplet_pt4(b, i, j, k) * wm_interm_92_triplet_pt4(a, p, k, j)
term(547) = term(547) + t2(a,b,q,i) * wm_interm_62_triplet_pt4(b, j, i, k) * wm_interm_92_triplet_pt4(a, p, k, j)
term(548) = term(548) + t2(a,b,q,i) * wm_interm_62_triplet_pt4(a, j, i, k) * wm_interm_92_triplet_pt4(b, p, k, j)
term(549) = term(549) + t2(a,b,q,i) * wm_interm_62_triplet_pt4(a, i, j, k) * wm_interm_92_triplet_pt4(b, p, k, j)
term(550) = term(550) + t2(a,b,q,i) * wm_interm_62_triplet_pt4(a, p, j, k) * wm_interm_94_triplet_pt4(b, i, k, j)
term(551) = term(551) + t2(a,b,q,i) * wm_interm_62_triplet_pt4(a, j, p, k) * wm_interm_94_triplet_pt4(b, i, k, j)
term(552) = term(552) + t2(a,b,q,i) * wm_interm_62_triplet_pt4(a, p, j, k) * wm_interm_92_triplet_pt4(b, i, k, j)
term(553) = term(553) + t2(a,b,q,i) * wm_interm_62_triplet_pt4(a, j, p, k) * wm_interm_92_triplet_pt4(b, i, k, j)
term(554) = term(554) + t2(a,b,q,i) * wm_interm_60_triplet_pt4(b, p, j, k) * wm_interm_92_triplet_pt4(a, i, k, j)
term(555) = term(555) + t2(a,b,q,i) * wm_interm_60_triplet_pt4(b, j, p, k) * wm_interm_92_triplet_pt4(a, i, k, j)
term(556) = term(556) + t2(a,b,q,i) * wm_interm_62_triplet_pt4(a, p, j, k) * wm_interm_94_triplet_pt4(b, k, i, j)
term(557) = term(557) + t2(a,b,q,i) * wm_interm_62_triplet_pt4(a, p, j, k) * wm_interm_92_triplet_pt4(b, k, i, j)
term(558) = term(558) + t2(a,b,q,i) * wm_interm_62_triplet_pt4(a, j, i, k) * wm_interm_94_triplet_pt4(b, k, p, j)
term(559) = term(559) + t2(a,b,q,i) * wm_interm_62_triplet_pt4(a, i, j, k) * wm_interm_94_triplet_pt4(b, k, p, j)
term(560) = term(560) + t2(a,b,q,i) * wm_interm_62_triplet_pt4(a, j, p, k) * wm_interm_94_triplet_pt4(b, k, i, j)
term(561) = term(561) + t2(a,b,q,i) * wm_interm_62_triplet_pt4(a, i, j, k) * wm_interm_92_triplet_pt4(b, k, p, j)
term(562) = term(562) + t2(a,b,q,i) * wm_interm_62_triplet_pt4(a, j, i, k) * wm_interm_92_triplet_pt4(b, k, p, j)
term(563) = term(563) + t2(a,b,q,i) * wm_interm_62_triplet_pt4(a, j, p, k) * wm_interm_92_triplet_pt4(b, k, i, j)
term(564) = term(564) + t2(a,b,q,i) * wm_interm_62_triplet_pt4(b, j, p, k) * wm_interm_94_triplet_pt4(a, k, i, j)
term(565) = term(565) + t2(a,b,q,i) * wm_interm_62_triplet_pt4(b, j, p, k) * wm_interm_92_triplet_pt4(a, k, i, j)
term(566) = term(566) + t2(a,b,q,i) * wm_interm_62_triplet_pt4(b, i, j, k) * wm_interm_94_triplet_pt4(a, k, p, j)
term(567) = term(567) + t2(a,b,q,i) * wm_interm_62_triplet_pt4(b, j, i, k) * wm_interm_94_triplet_pt4(a, k, p, j)
term(568) = term(568) + t2(a,b,q,i) * wm_interm_62_triplet_pt4(b, p, j, k) * wm_interm_94_triplet_pt4(a, k, i, j)
term(569) = term(569) + t2(a,b,q,i) * wm_interm_62_triplet_pt4(b, p, j, k) * wm_interm_92_triplet_pt4(a, k, i, j)
term(570) = term(570) + t2(a,b,q,i) * wm_interm_62_triplet_pt4(b, i, j, k) * wm_interm_92_triplet_pt4(a, k, p, j)
term(571) = term(571) + t2(a,b,q,i) * wm_interm_62_triplet_pt4(b, j, i, k) * wm_interm_92_triplet_pt4(a, k, p, j)
end do 
end do 
end do 
end do 
end do 

term(512) = term(512) * 12.0d+0 
term(513) = term(513) * (-12.0d+0) 
term(514) = term(514) * 8.0d+0 
term(515) = term(515) * (-16.0d+0) 
term(516) = term(516) * 4.0d+0 
term(517) = term(517) * (-8.0d+0) 
term(518) = term(518) * 4.0d+0 
term(519) = term(519) * (-8.0d+0) 
term(520) = term(520) * 6.0d+0 
term(521) = term(521) * (-18.0d+0) 
term(522) = term(522) * 8.0d+0 
term(523) = term(523) * (-16.0d+0) 
term(524) = term(524) * (-3.0d+0) 
term(525) = term(525) * 3.0d+0 
term(526) = term(526) * 6.0d+0 
term(527) = term(527) * (-12.0d+0) 
term(528) = term(528) * (-6.0d+0) 
term(529) = term(529) * 6.0d+0 
term(530) = term(530) * (-3.0d+0) 
term(531) = term(531) * 6.0d+0 
term(532) = term(532) * 3.0d+0 
term(533) = term(533) * (-6.0d+0) 
term(534) = term(534) * 3.0d+0 
term(535) = term(535) * (-3.0d+0) 
term(536) = term(536) * 8.0d+0 
term(537) = term(537) * (-16.0d+0) 
term(538) = term(538) * (-16.0d+0) 
term(539) = term(539) * 8.0d+0 
term(540) = term(540) * 4.0d+0 
term(541) = term(541) * (-2.0d+0) 
term(542) = term(542) * 4.0d+0 
term(543) = term(543) * (-8.0d+0) 
term(544) = term(544) * (-2.0d+0) 
term(545) = term(545) * 4.0d+0 
term(546) = term(546) * (-2.0d+0) 
term(547) = term(547) * 4.0d+0 
term(548) = term(548) * (-2.0d+0) 
term(549) = term(549) * 4.0d+0 
term(550) = term(550) * 4.0d+0 
term(551) = term(551) * (-8.0d+0) 
term(552) = term(552) * (-2.0d+0) 
term(553) = term(553) * 4.0d+0 
term(554) = term(554) * (-2.0d+0) 
term(555) = term(555) * 4.0d+0 
term(556) = term(556) * (-2.0d+0) 
term(557) = term(557) * 4.0d+0 
term(558) = term(558) * (-2.0d+0) 
term(559) = term(559) * 4.0d+0 
term(560) = term(560) * 4.0d+0 
term(561) = term(561) * (-2.0d+0) 
term(562) = term(562) * 4.0d+0 
term(563) = term(563) * (-8.0d+0) 
term(564) = term(564) * (-2.0d+0) 
term(565) = term(565) * 4.0d+0 
term(566) = term(566) * (-2.0d+0) 
term(567) = term(567) * 4.0d+0 
term(568) = term(568) * 4.0d+0 
term(569) = term(569) * (-2.0d+0) 
term(570) = term(570) * 4.0d+0 
term(571) = term(571) * (-8.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(572) = term(572) + s2(a,b,i,q) * wm_interm_43_triplet_pt4(a, j, i, k) * wm_interm_64_triplet_pt4(b, k, p, j)
term(573) = term(573) + s2(a,b,i,j) * wm_interm_43_triplet_pt4(a, q, i, k) * wm_interm_64_triplet_pt4(b, k, p, j)
term(574) = term(574) + s2(a,b,i,j) * wm_interm_43_triplet_pt4(a, q, j, k) * wm_interm_71_triplet_pt4(b, k, i, p)
term(575) = term(575) + s2(a,b,i,q) * wm_interm_43_triplet_pt4(a, j, i, k) * wm_interm_71_triplet_pt4(b, k, p, j)
term(576) = term(576) + s2(a,b,i,j) * wm_interm_43_triplet_pt4(a, q, i, k) * wm_interm_71_triplet_pt4(b, k, j, p)
term(577) = term(577) + s2(a,b,i,j) * wm_interm_43_triplet_pt4(a, q, i, k) * wm_interm_71_triplet_pt4(b, k, p, j)
term(578) = term(578) + s2(a,b,i,j) * wm_interm_14_triplet_pt4(a, p, i, k) * wm_interm_51_triplet_pt4(b, q, k, j)
term(579) = term(579) + s2(a,b,i,j) * wm_interm_14_triplet_pt4(a, i, p, k) * wm_interm_51_triplet_pt4(b, q, k, j)
term(580) = term(580) + s2(a,b,i,j) * wm_interm_51_triplet_pt4(b, q, k, j) * wm_interm_79_triplet_pt4(a, i, p, k)
term(581) = term(581) + s2(a,b,i,j) * wm_interm_51_triplet_pt4(b, q, k, j) * wm_interm_79_triplet_pt4(a, p, i, k)
term(582) = term(582) + s2(a,b,i,j) * wm_interm_51_triplet_pt4(b, q, i, k) * wm_interm_79_triplet_pt4(a, p, j, k)
term(583) = term(583) + s2(a,b,i,j) * wm_interm_51_triplet_pt4(b, q, i, k) * wm_interm_79_triplet_pt4(a, j, p, k)
term(584) = term(584) + s2(a,b,i,j) * wm_interm_51_triplet_pt4(b, q, j, k) * wm_interm_79_triplet_pt4(a, i, p, k)
term(585) = term(585) + s2(a,b,i,j) * wm_interm_51_triplet_pt4(b, q, j, k) * wm_interm_79_triplet_pt4(a, p, i, k)
term(586) = term(586) + s2(a,b,i,j) * wm_interm_14_triplet_pt4(a, p, i, k) * wm_interm_51_triplet_pt4(b, q, j, k)
term(587) = term(587) + s2(a,b,i,j) * wm_interm_14_triplet_pt4(a, i, p, k) * wm_interm_51_triplet_pt4(b, q, j, k)
term(588) = term(588) + s2(a,b,i,j) * wm_interm_14_triplet_pt4(a, j, p, k) * wm_interm_51_triplet_pt4(b, q, i, k)
term(589) = term(589) + s2(a,b,i,j) * wm_interm_14_triplet_pt4(a, p, j, k) * wm_interm_51_triplet_pt4(b, q, i, k)
term(590) = term(590) + s2(a,b,i,p) * wm_interm_43_triplet_pt4(a, j, q, k) * wm_interm_64_triplet_pt4(b, k, i, j)
term(591) = term(591) + s2(a,b,i,j) * wm_interm_43_triplet_pt4(a, k, q, j) * wm_interm_64_triplet_pt4(b, p, i, k)
term(592) = term(592) + s2(a,b,i,j) * wm_interm_43_triplet_pt4(a, k, j, q) * wm_interm_64_triplet_pt4(b, p, i, k)
term(593) = term(593) + s2(a,b,i,j) * wm_interm_43_triplet_pt4(a, k, i, q) * wm_interm_64_triplet_pt4(b, p, j, k)
term(594) = term(594) + s2(a,b,i,p) * wm_interm_43_triplet_pt4(a, j, q, k) * wm_interm_71_triplet_pt4(b, k, i, j)
term(595) = term(595) + s2(a,b,i,j) * wm_interm_43_triplet_pt4(a, k, q, j) * wm_interm_71_triplet_pt4(b, p, i, k)
term(596) = term(596) + s2(a,b,i,j) * wm_interm_43_triplet_pt4(a, k, j, q) * wm_interm_71_triplet_pt4(b, p, i, k)
term(597) = term(597) + s2(a,b,i,j) * wm_interm_43_triplet_pt4(a, k, i, q) * wm_interm_71_triplet_pt4(b, p, j, k)
term(598) = term(598) + t2(a,b,i,j) * wm_interm_60_triplet_pt4(a, q, j, k) * wm_interm_85_triplet_pt4(b, i, k, p)
term(599) = term(599) + t2(a,b,i,j) * wm_interm_60_triplet_pt4(a, j, q, k) * wm_interm_85_triplet_pt4(b, i, k, p)
term(600) = term(600) + t2(a,b,i,j) * wm_interm_62_triplet_pt4(a, i, q, k) * wm_interm_85_triplet_pt4(b, j, k, p)
term(601) = term(601) + t2(a,b,i,j) * wm_interm_62_triplet_pt4(a, q, i, k) * wm_interm_85_triplet_pt4(b, j, k, p)
term(602) = term(602) + t2(a,b,i,j) * wm_interm_62_triplet_pt4(a, j, q, k) * wm_interm_85_triplet_pt4(b, k, i, p)
term(603) = term(603) + t2(a,b,i,j) * wm_interm_62_triplet_pt4(a, q, j, k) * wm_interm_85_triplet_pt4(b, k, i, p)
term(604) = term(604) + t2(a,b,i,j) * wm_interm_62_triplet_pt4(a, q, i, k) * wm_interm_85_triplet_pt4(b, k, j, p)
term(605) = term(605) + t2(a,b,i,j) * wm_interm_62_triplet_pt4(a, i, q, k) * wm_interm_85_triplet_pt4(b, k, j, p)
term(606) = term(606) + t2(a,b,i,j) * wm_interm_60_triplet_pt4(a, k, j, q) * wm_interm_85_triplet_pt4(b, i, p, k)
term(607) = term(607) + t2(a,b,i,j) * wm_interm_60_triplet_pt4(a, j, k, q) * wm_interm_85_triplet_pt4(b, i, p, k)
term(608) = term(608) + t2(a,b,i,j) * wm_interm_60_triplet_pt4(a, i, k, q) * wm_interm_85_triplet_pt4(b, j, p, k)
term(609) = term(609) + t2(a,b,i,j) * wm_interm_60_triplet_pt4(a, k, i, q) * wm_interm_85_triplet_pt4(b, j, p, k)
term(610) = term(610) + t2(a,b,i,j) * wm_interm_60_triplet_pt4(a, q, j, k) * wm_interm_94_triplet_pt4(b, i, k, p)
term(611) = term(611) + t2(a,b,i,j) * wm_interm_60_triplet_pt4(a, j, q, k) * wm_interm_94_triplet_pt4(b, i, k, p)
term(612) = term(612) + t2(a,b,i,j) * wm_interm_62_triplet_pt4(a, i, q, k) * wm_interm_92_triplet_pt4(b, j, k, p)
term(613) = term(613) + t2(a,b,i,j) * wm_interm_62_triplet_pt4(a, q, i, k) * wm_interm_92_triplet_pt4(b, j, k, p)
term(614) = term(614) + t2(a,b,i,j) * wm_interm_60_triplet_pt4(a, j, q, k) * wm_interm_92_triplet_pt4(b, i, k, p)
term(615) = term(615) + t2(a,b,i,j) * wm_interm_60_triplet_pt4(a, q, j, k) * wm_interm_92_triplet_pt4(b, i, k, p)
term(616) = term(616) + t2(a,b,i,j) * wm_interm_62_triplet_pt4(a, i, q, k) * wm_interm_94_triplet_pt4(b, j, k, p)
term(617) = term(617) + t2(a,b,i,j) * wm_interm_62_triplet_pt4(a, q, i, k) * wm_interm_94_triplet_pt4(b, j, k, p)
term(618) = term(618) + t2(a,b,i,j) * wm_interm_24_triplet_pt4(b, i, q, k) * wm_interm_45_triplet_pt4(a, p, k, j)
term(619) = term(619) + t2(a,b,i,j) * wm_interm_24_triplet_pt4(b, i, q, k) * wm_interm_45_triplet_pt4(a, p, j, k)
term(620) = term(620) + t2(a,b,i,j) * wm_interm_24_triplet_pt4(b, q, i, k) * wm_interm_45_triplet_pt4(a, p, k, j)
term(621) = term(621) + t2(a,b,i,j) * wm_interm_24_triplet_pt4(b, q, i, k) * wm_interm_45_triplet_pt4(a, p, j, k)
term(622) = term(622) + t2(a,b,i,j) * wm_interm_24_triplet_pt4(b, q, j, k) * wm_interm_45_triplet_pt4(a, p, i, k)
term(623) = term(623) + t2(a,b,i,j) * wm_interm_24_triplet_pt4(b, j, q, k) * wm_interm_45_triplet_pt4(a, p, i, k)
term(624) = term(624) + t2(a,b,i,j) * wm_interm_62_triplet_pt4(a, j, q, k) * wm_interm_92_triplet_pt4(b, k, i, p)
term(625) = term(625) + t2(a,b,i,j) * wm_interm_62_triplet_pt4(a, q, j, k) * wm_interm_92_triplet_pt4(b, k, i, p)
term(626) = term(626) + t2(a,b,i,j) * wm_interm_62_triplet_pt4(a, q, i, k) * wm_interm_92_triplet_pt4(b, k, j, p)
term(627) = term(627) + t2(a,b,i,j) * wm_interm_62_triplet_pt4(a, i, q, k) * wm_interm_92_triplet_pt4(b, k, j, p)
term(628) = term(628) + t2(a,b,i,j) * wm_interm_62_triplet_pt4(a, i, q, k) * wm_interm_94_triplet_pt4(b, k, j, p)
term(629) = term(629) + t2(a,b,i,j) * wm_interm_62_triplet_pt4(a, q, i, k) * wm_interm_94_triplet_pt4(b, k, j, p)
term(630) = term(630) + t2(a,b,i,j) * wm_interm_62_triplet_pt4(a, j, q, k) * wm_interm_94_triplet_pt4(b, k, i, p)
term(631) = term(631) + t2(a,b,i,j) * wm_interm_62_triplet_pt4(a, q, j, k) * wm_interm_94_triplet_pt4(b, k, i, p)
term(632) = term(632) + t2(a,b,i,j) * wm_interm_27_triplet_pt4(b, q, i, k) * wm_interm_45_triplet_pt4(a, p, j, k)
term(633) = term(633) + t2(a,b,i,j) * wm_interm_27_triplet_pt4(b, q, i, k) * wm_interm_45_triplet_pt4(a, p, k, j)
term(634) = term(634) + t2(a,b,i,j) * wm_interm_27_triplet_pt4(b, q, j, k) * wm_interm_45_triplet_pt4(a, p, i, k)
term(635) = term(635) + t2(a,b,i,j) * wm_interm_27_triplet_pt4(b, i, q, k) * wm_interm_45_triplet_pt4(a, p, j, k)
term(636) = term(636) + t2(a,b,i,j) * wm_interm_27_triplet_pt4(b, j, q, k) * wm_interm_45_triplet_pt4(a, p, i, k)
term(637) = term(637) + t2(a,b,i,j) * wm_interm_27_triplet_pt4(b, i, q, k) * wm_interm_45_triplet_pt4(a, p, k, j)
term(638) = term(638) + t2(a,b,i,j) * wm_interm_60_triplet_pt4(a, k, j, q) * wm_interm_92_triplet_pt4(b, p, i, k)
term(639) = term(639) + t2(a,b,i,j) * wm_interm_60_triplet_pt4(a, j, k, q) * wm_interm_92_triplet_pt4(b, p, i, k)
term(640) = term(640) + t2(a,b,i,j) * wm_interm_60_triplet_pt4(a, i, k, q) * wm_interm_92_triplet_pt4(b, p, j, k)
term(641) = term(641) + t2(a,b,i,j) * wm_interm_60_triplet_pt4(a, k, i, q) * wm_interm_92_triplet_pt4(b, p, j, k)
term(642) = term(642) + t2(a,b,i,j) * wm_interm_60_triplet_pt4(a, k, j, q) * wm_interm_94_triplet_pt4(b, i, p, k)
term(643) = term(643) + t2(a,b,i,j) * wm_interm_60_triplet_pt4(a, k, j, q) * wm_interm_92_triplet_pt4(b, i, p, k)
term(644) = term(644) + t2(a,b,i,j) * wm_interm_60_triplet_pt4(a, j, k, q) * wm_interm_94_triplet_pt4(b, i, p, k)
term(645) = term(645) + t2(a,b,i,j) * wm_interm_60_triplet_pt4(a, j, k, q) * wm_interm_92_triplet_pt4(b, i, p, k)
term(646) = term(646) + t2(a,b,i,j) * wm_interm_60_triplet_pt4(a, i, k, q) * wm_interm_92_triplet_pt4(b, j, p, k)
term(647) = term(647) + t2(a,b,i,j) * wm_interm_60_triplet_pt4(a, k, i, q) * wm_interm_92_triplet_pt4(b, j, p, k)
term(648) = term(648) + t2(a,b,i,j) * wm_interm_60_triplet_pt4(a, k, j, q) * wm_interm_94_triplet_pt4(b, p, i, k)
term(649) = term(649) + t2(a,b,i,j) * wm_interm_60_triplet_pt4(a, j, k, q) * wm_interm_94_triplet_pt4(b, p, i, k)
term(650) = term(650) + t2(a,b,i,j) * wm_interm_60_triplet_pt4(a, i, k, q) * wm_interm_94_triplet_pt4(b, p, j, k)
term(651) = term(651) + t2(a,b,i,j) * wm_interm_60_triplet_pt4(a, i, k, q) * wm_interm_94_triplet_pt4(b, j, p, k)
term(652) = term(652) + t2(a,b,i,j) * wm_interm_60_triplet_pt4(a, k, i, q) * wm_interm_94_triplet_pt4(b, p, j, k)
term(653) = term(653) + t2(a,b,i,j) * wm_interm_60_triplet_pt4(a, k, i, q) * wm_interm_94_triplet_pt4(b, j, p, k)
end do 
end do 
end do 
end do 
end do 

term(572) = term(572) * 12.0d+0 
term(573) = term(573) * (-12.0d+0) 
term(574) = term(574) * (-16.0d+0) 
term(575) = term(575) * 8.0d+0 
term(576) = term(576) * 8.0d+0 
term(577) = term(577) * (-16.0d+0) 
term(578) = term(578) * (-8.0d+0) 
term(579) = term(579) * 16.0d+0 
term(580) = term(580) * (-8.0d+0) 
term(581) = term(581) * 16.0d+0 
term(582) = term(582) * 4.0d+0 
term(583) = term(583) * (-8.0d+0) 
term(584) = term(584) * 4.0d+0 
term(585) = term(585) * (-8.0d+0) 
term(586) = term(586) * 4.0d+0 
term(587) = term(587) * (-8.0d+0) 
term(588) = term(588) * 4.0d+0 
term(589) = term(589) * (-8.0d+0) 
term(590) = term(590) * (-6.0d+0) 
term(591) = term(591) * 6.0d+0 
term(592) = term(592) * (-6.0d+0) 
term(593) = term(593) * 6.0d+0 
term(594) = term(594) * (-16.0d+0) 
term(595) = term(595) * 8.0d+0 
term(596) = term(596) * (-16.0d+0) 
term(597) = term(597) * 8.0d+0 
term(598) = term(598) * 6.0d+0 
term(599) = term(599) * (-6.0d+0) 
term(600) = term(600) * (-6.0d+0) 
term(601) = term(601) * 12.0d+0 
term(602) = term(602) * (-6.0d+0) 
term(603) = term(603) * 6.0d+0 
term(604) = term(604) * (-12.0d+0) 
term(605) = term(605) * 6.0d+0 
term(606) = term(606) * 6.0d+0 
term(607) = term(607) * (-6.0d+0) 
term(608) = term(608) * 12.0d+0 
term(609) = term(609) * (-6.0d+0) 
term(610) = term(610) * (-4.0d+0) 
term(611) = term(611) * 8.0d+0 
term(612) = term(612) * (-4.0d+0) 
term(613) = term(613) * 8.0d+0 
term(614) = term(614) * (-4.0d+0) 
term(615) = term(615) * 8.0d+0 
term(616) = term(616) * 8.0d+0 
term(617) = term(617) * (-16.0d+0) 
term(618) = term(618) * 8.0d+0 
term(619) = term(619) * (-4.0d+0) 
term(620) = term(620) * (-4.0d+0) 
term(621) = term(621) * 8.0d+0 
term(622) = term(622) * (-4.0d+0) 
term(623) = term(623) * 8.0d+0 
term(624) = term(624) * (-4.0d+0) 
term(625) = term(625) * 8.0d+0 
term(626) = term(626) * (-16.0d+0) 
term(627) = term(627) * 8.0d+0 
term(628) = term(628) * (-4.0d+0) 
term(629) = term(629) * 8.0d+0 
term(630) = term(630) * 8.0d+0 
term(631) = term(631) * (-4.0d+0) 
term(632) = term(632) * (-4.0d+0) 
term(633) = term(633) * 8.0d+0 
term(634) = term(634) * 8.0d+0 
term(635) = term(635) * 8.0d+0 
term(636) = term(636) * (-4.0d+0) 
term(637) = term(637) * (-4.0d+0) 
term(638) = term(638) * (-2.0d+0) 
term(639) = term(639) * 4.0d+0 
term(640) = term(640) * (-8.0d+0) 
term(641) = term(641) * 4.0d+0 
term(642) = term(642) * (-2.0d+0) 
term(643) = term(643) * 4.0d+0 
term(644) = term(644) * 4.0d+0 
term(645) = term(645) * (-2.0d+0) 
term(646) = term(646) * 4.0d+0 
term(647) = term(647) * (-2.0d+0) 
term(648) = term(648) * 4.0d+0 
term(649) = term(649) * (-2.0d+0) 
term(650) = term(650) * 4.0d+0 
term(651) = term(651) * (-8.0d+0) 
term(652) = term(652) * (-2.0d+0) 
term(653) = term(653) * 4.0d+0 

do k = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(654) = term(654) + s1(a,i) * wm_interm_49_triplet_pt4(j, k, l, i) * wm_interm_64_triplet_pt4(a, l, j, k)
term(655) = term(655) + s1(a,i) * wm_interm_49_triplet_pt4(j, k, l, i) * wm_interm_71_triplet_pt4(a, l, j, k)
end do 
end do 
end do 
end do 
end do 

term(654) = term(654) * 12.0d+0 
term(655) = term(655) * 32.0d+0 

do j = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(656) = term(656) + s1(a,i) * wm_interm_49_triplet_pt4(j, k, l, i) * wm_interm_64_triplet_pt4(a, l, k, j)
term(657) = term(657) + s1(a,i) * wm_interm_49_triplet_pt4(j, k, l, i) * wm_interm_71_triplet_pt4(a, l, k, j)
end do 
end do 
end do 
end do 
end do 

term(656) = term(656) * (-12.0d+0) 
term(657) = term(657) * (-16.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(658) = term(658) + s1(a,i) * wm_interm_51_triplet_pt4(a, j, k, l) * wm_interm_66_triplet_pt4(j, i, l, k)
term(659) = term(659) + s1(a,i) * wm_interm_51_triplet_pt4(a, j, k, l) * wm_interm_66_triplet_pt4(j, i, k, l)
term(660) = term(660) + s1(a,i) * wm_interm_51_triplet_pt4(a, j, k, l) * wm_interm_74_triplet_pt4(j, i, l, k)
term(661) = term(661) + s1(a,i) * wm_interm_51_triplet_pt4(a, j, k, l) * wm_interm_74_triplet_pt4(i, j, l, k)
term(662) = term(662) + s1(a,i) * wm_interm_51_triplet_pt4(a, j, k, l) * wm_interm_74_triplet_pt4(i, j, k, l)
term(663) = term(663) + s1(a,i) * wm_interm_51_triplet_pt4(a, j, k, l) * wm_interm_74_triplet_pt4(j, i, k, l)
term(664) = term(664) + t1(a,i) * wm_interm_45_triplet_pt4(a, j, k, l) * wm_interm_88_triplet_pt4(j, i, k, l)
term(665) = term(665) + t1(a,i) * wm_interm_45_triplet_pt4(a, j, k, l) * wm_interm_88_triplet_pt4(j, i, l, k)
term(666) = term(666) + t1(a,i) * wm_interm_45_triplet_pt4(a, j, k, l) * wm_interm_97_triplet_pt4(j, i, k, l)
term(667) = term(667) + t1(a,i) * wm_interm_45_triplet_pt4(a, j, k, l) * wm_interm_97_triplet_pt4(j, i, l, k)
term(668) = term(668) + t1(a,i) * wm_interm_45_triplet_pt4(a, j, k, l) * wm_interm_97_triplet_pt4(i, j, l, k)
term(669) = term(669) + t1(a,i) * wm_interm_45_triplet_pt4(a, j, k, l) * wm_interm_97_triplet_pt4(i, j, k, l)
end do 
end do 
end do 
end do 
end do 

term(658) = term(658) * (-12.0d+0) 
term(659) = term(659) * 12.0d+0 
term(660) = term(660) * (-8.0d+0) 
term(661) = term(661) * 16.0d+0 
term(662) = term(662) * (-8.0d+0) 
term(663) = term(663) * 16.0d+0 
term(664) = term(664) * (-12.0d+0) 
term(665) = term(665) * 12.0d+0 
term(666) = term(666) * (-16.0d+0) 
term(667) = term(667) * 8.0d+0 
term(668) = term(668) * (-16.0d+0) 
term(669) = term(669) * 8.0d+0 

do i = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(670) = term(670) + s2(a,b,i,q) * wm_interm_43_triplet_pt4(a, j, k, i) * wm_interm_71_triplet_pt4(b, k, j, p)
term(671) = term(671) + s2(a,b,q,i) * wm_interm_43_triplet_pt4(a, j, k, i) * wm_interm_71_triplet_pt4(b, k, j, p)
term(672) = term(672) + s2(a,b,i,p) * wm_interm_43_triplet_pt4(a, j, k, q) * wm_interm_64_triplet_pt4(b, k, j, i)
term(673) = term(673) + s2(a,b,p,i) * wm_interm_43_triplet_pt4(a, j, k, q) * wm_interm_64_triplet_pt4(b, k, j, i)
term(674) = term(674) + s2(a,b,p,i) * wm_interm_43_triplet_pt4(a, j, k, q) * wm_interm_71_triplet_pt4(b, k, j, i)
term(675) = term(675) + s2(a,b,i,p) * wm_interm_43_triplet_pt4(a, j, k, q) * wm_interm_71_triplet_pt4(b, k, j, i)
end do 
end do 
end do 
end do 
end do 

term(670) = term(670) * 32.0d+0 
term(671) = term(671) * (-16.0d+0) 
term(672) = term(672) * (-6.0d+0) 
term(673) = term(673) * 6.0d+0 
term(674) = term(674) * 32.0d+0 
term(675) = term(675) * (-16.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(676) = term(676) + s1(a,i) * wm_interm_49_triplet_pt4(j, q, k, i) * wm_interm_71_triplet_pt4(a, k, j, p)
term(677) = term(677) + s1(a,i) * wm_interm_49_triplet_pt4(q, j, k, i) * wm_interm_71_triplet_pt4(a, k, j, p)
term(678) = term(678) + s1(a,p) * wm_interm_49_triplet_pt4(i, j, k, q) * wm_interm_64_triplet_pt4(a, k, j, i)
term(679) = term(679) + s1(a,p) * wm_interm_49_triplet_pt4(i, j, k, q) * wm_interm_71_triplet_pt4(a, k, j, i)
end do 
end do 
end do 
end do 

term(676) = term(676) * (-16.0d+0) 
term(677) = term(677) * 8.0d+0 
term(678) = term(678) * 6.0d+0 
term(679) = term(679) * 8.0d+0 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(680) = term(680) + s2(a,b,q,i) * wm_interm_47_triplet_pt4(b, j) * wm_interm_79_triplet_pt4(a, p, i, j)
term(681) = term(681) + s2(a,b,q,i) * wm_interm_47_triplet_pt4(b, j) * wm_interm_79_triplet_pt4(a, i, p, j)
term(682) = term(682) + s2(a,b,q,i) * wm_interm_14_triplet_pt4(a, i, p, j) * wm_interm_47_triplet_pt4(b, j)
term(683) = term(683) + s2(a,b,q,i) * wm_interm_14_triplet_pt4(a, p, i, j) * wm_interm_47_triplet_pt4(b, j)
term(684) = term(684) + s2(a,b,q,i) * wm_interm_14_triplet_pt4(a, i, p, j) * wm_interm_52_triplet_pt4(b, j)
term(685) = term(685) + s2(a,b,q,i) * wm_interm_14_triplet_pt4(a, p, i, j) * wm_interm_52_triplet_pt4(b, j)
term(686) = term(686) + s2(a,b,q,i) * wm_interm_52_triplet_pt4(b, j) * wm_interm_79_triplet_pt4(a, p, i, j)
term(687) = term(687) + s2(a,b,q,i) * wm_interm_52_triplet_pt4(b, j) * wm_interm_79_triplet_pt4(a, i, p, j)
term(688) = term(688) + s2(a,b,i,j) * wm_interm_2_triplet_pt4(a, j) * wm_interm_51_triplet_pt4(b, p, q, i)
term(689) = term(689) + s2(a,b,i,j) * wm_interm_3_triplet_pt4(a, j) * wm_interm_51_triplet_pt4(b, p, q, i)
term(690) = term(690) + s2(a,b,i,j) * wm_interm_43_triplet_pt4(a, p, q, i) * wm_interm_82_triplet_pt4(b, j)
term(691) = term(691) + s2(a,b,i,j) * wm_interm_43_triplet_pt4(a, p, q, i) * wm_interm_84_triplet_pt4(b, j)
term(692) = term(692) + s2(a,b,i,j) * wm_interm_43_triplet_pt4(a, p, q, i) * wm_interm_83_triplet_pt4(b, j)
term(693) = term(693) + t2(a,b,i,j) * wm_interm_0_triplet_pt4(b, j) * wm_interm_45_triplet_pt4(a, p, q, i)
term(694) = term(694) + t2(a,b,q,i) * wm_interm_62_triplet_pt4(b, i, p, j) * wm_interm_86_triplet_pt4(a, j)
term(695) = term(695) + t2(a,b,q,i) * wm_interm_62_triplet_pt4(b, p, i, j) * wm_interm_86_triplet_pt4(a, j)
term(696) = term(696) + t2(a,b,q,i) * wm_interm_62_triplet_pt4(a, p, i, j) * wm_interm_86_triplet_pt4(b, j)
term(697) = term(697) + t2(a,b,q,i) * wm_interm_62_triplet_pt4(a, i, p, j) * wm_interm_86_triplet_pt4(b, j)
term(698) = term(698) + t2(a,b,i,j) * wm_interm_20_triplet_pt4(b, j) * wm_interm_45_triplet_pt4(a, p, q, i)
term(699) = term(699) + t2(a,b,i,j) * wm_interm_21_triplet_pt4(b, j) * wm_interm_45_triplet_pt4(a, p, q, i)
term(700) = term(700) + t2(a,b,q,i) * wm_interm_62_triplet_pt4(b, i, p, j) * wm_interm_96_triplet_pt4(a, j)
term(701) = term(701) + t2(a,b,q,i) * wm_interm_62_triplet_pt4(b, p, i, j) * wm_interm_96_triplet_pt4(a, j)
term(702) = term(702) + t2(a,b,q,i) * wm_interm_62_triplet_pt4(a, p, i, j) * wm_interm_96_triplet_pt4(b, j)
term(703) = term(703) + t2(a,b,q,i) * wm_interm_62_triplet_pt4(a, i, p, j) * wm_interm_96_triplet_pt4(b, j)
term(704) = term(704) + t2(a,b,q,i) * wm_interm_62_triplet_pt4(a, p, i, j) * wm_interm_93_triplet_pt4(b, j)
term(705) = term(705) + t2(a,b,q,i) * wm_interm_62_triplet_pt4(a, i, p, j) * wm_interm_93_triplet_pt4(b, j)
term(706) = term(706) + t2(a,b,q,i) * wm_interm_62_triplet_pt4(b, i, p, j) * wm_interm_93_triplet_pt4(a, j)
term(707) = term(707) + t2(a,b,q,i) * wm_interm_62_triplet_pt4(b, p, i, j) * wm_interm_93_triplet_pt4(a, j)
term(708) = term(708) + t2(a,b,q,i) * wm_interm_24_triplet_pt4(a, i, p, j) * wm_interm_44_triplet_pt4(b, j)
term(709) = term(709) + t2(a,b,q,i) * wm_interm_24_triplet_pt4(a, p, i, j) * wm_interm_44_triplet_pt4(b, j)
term(710) = term(710) + t2(a,b,q,i) * wm_interm_24_triplet_pt4(a, i, p, j) * wm_interm_46_triplet_pt4(b, j)
term(711) = term(711) + t2(a,b,q,i) * wm_interm_24_triplet_pt4(a, p, i, j) * wm_interm_46_triplet_pt4(b, j)
term(712) = term(712) + t2(a,b,q,i) * wm_interm_24_triplet_pt4(b, p, i, j) * wm_interm_44_triplet_pt4(a, j)
term(713) = term(713) + t2(a,b,q,i) * wm_interm_24_triplet_pt4(b, p, i, j) * wm_interm_46_triplet_pt4(a, j)
term(714) = term(714) + t2(a,b,q,i) * wm_interm_24_triplet_pt4(b, i, p, j) * wm_interm_44_triplet_pt4(a, j)
term(715) = term(715) + t2(a,b,q,i) * wm_interm_24_triplet_pt4(b, i, p, j) * wm_interm_46_triplet_pt4(a, j)
term(716) = term(716) + t2(a,b,q,i) * wm_interm_27_triplet_pt4(b, p, i, j) * wm_interm_44_triplet_pt4(a, j)
term(717) = term(717) + t2(a,b,q,i) * wm_interm_27_triplet_pt4(b, i, p, j) * wm_interm_44_triplet_pt4(a, j)
term(718) = term(718) + t2(a,b,q,i) * wm_interm_27_triplet_pt4(a, p, i, j) * wm_interm_44_triplet_pt4(b, j)
term(719) = term(719) + t2(a,b,q,i) * wm_interm_27_triplet_pt4(a, i, p, j) * wm_interm_44_triplet_pt4(b, j)
term(720) = term(720) + t2(a,b,q,i) * wm_interm_27_triplet_pt4(b, i, p, j) * wm_interm_46_triplet_pt4(a, j)
term(721) = term(721) + t2(a,b,q,i) * wm_interm_27_triplet_pt4(b, p, i, j) * wm_interm_46_triplet_pt4(a, j)
term(722) = term(722) + t2(a,b,q,i) * wm_interm_27_triplet_pt4(a, p, i, j) * wm_interm_46_triplet_pt4(b, j)
term(723) = term(723) + t2(a,b,q,i) * wm_interm_27_triplet_pt4(a, i, p, j) * wm_interm_46_triplet_pt4(b, j)
end do 
end do 
end do 
end do 

term(680) = term(680) * 4.0d+0 
term(681) = term(681) * (-8.0d+0) 
term(682) = term(682) * 4.0d+0 
term(683) = term(683) * (-8.0d+0) 
term(684) = term(684) * (-8.0d+0) 
term(685) = term(685) * 16.0d+0 
term(686) = term(686) * (-8.0d+0) 
term(687) = term(687) * 16.0d+0 
term(688) = term(688) * 16.0d+0 
term(689) = term(689) * (-8.0d+0) 
term(690) = term(690) * (-48.0d+0) 
term(691) = term(691) * 32.0d+0 
term(692) = term(692) * (-64.0d+0) 
term(693) = term(693) * 16.0d+0 
term(694) = term(694) * 6.0d+0 
term(695) = term(695) * (-12.0d+0) 
term(696) = term(696) * 6.0d+0 
term(697) = term(697) * (-12.0d+0) 
term(698) = term(698) * 64.0d+0 
term(699) = term(699) * (-32.0d+0) 
term(700) = term(700) * (-4.0d+0) 
term(701) = term(701) * 8.0d+0 
term(702) = term(702) * (-4.0d+0) 
term(703) = term(703) * 8.0d+0 
term(704) = term(704) * 8.0d+0 
term(705) = term(705) * (-16.0d+0) 
term(706) = term(706) * 8.0d+0 
term(707) = term(707) * (-16.0d+0) 
term(708) = term(708) * (-2.0d+0) 
term(709) = term(709) * 4.0d+0 
term(710) = term(710) * 4.0d+0 
term(711) = term(711) * (-8.0d+0) 
term(712) = term(712) * (-2.0d+0) 
term(713) = term(713) * 4.0d+0 
term(714) = term(714) * 4.0d+0 
term(715) = term(715) * (-8.0d+0) 
term(716) = term(716) * 4.0d+0 
term(717) = term(717) * (-2.0d+0) 
term(718) = term(718) * (-2.0d+0) 
term(719) = term(719) * 4.0d+0 
term(720) = term(720) * 4.0d+0 
term(721) = term(721) * (-8.0d+0) 
term(722) = term(722) * 4.0d+0 
term(723) = term(723) * (-8.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(724) = term(724) + wm_interm_0_triplet_pt4(a, i) * wm_interm_1_triplet_pt4(a, i)
term(725) = term(725) + wm_interm_0_triplet_pt4(a, i) * wm_interm_2_triplet_pt4(a, i)
term(726) = term(726) + wm_interm_0_triplet_pt4(a, i) * wm_interm_3_triplet_pt4(a, i)
term(727) = term(727) + wm_interm_0_triplet_pt4(a, i) * wm_interm_8_triplet_pt4(a, i, p, q)
term(728) = term(728) + wm_interm_0_triplet_pt4(a, i) * wm_interm_14_triplet_pt4(a, p, i, q)
term(729) = term(729) + wm_interm_0_triplet_pt4(a, i) * wm_interm_14_triplet_pt4(a, i, p, q)
term(730) = term(730) + wm_interm_1_triplet_pt4(a, i) * wm_interm_7_triplet_pt4(a, i, p, q)
term(731) = term(731) + wm_interm_2_triplet_pt4(a, i) * wm_interm_7_triplet_pt4(a, i, p, q)
term(732) = term(732) + wm_interm_3_triplet_pt4(a, i) * wm_interm_7_triplet_pt4(a, i, p, q)
term(733) = term(733) + wm_interm_1_triplet_pt4(a, i) * wm_interm_20_triplet_pt4(a, i)
term(734) = term(734) + wm_interm_1_triplet_pt4(a, i) * wm_interm_21_triplet_pt4(a, i)
term(735) = term(735) + wm_interm_20_triplet_pt4(a, i) * wm_interm_2_triplet_pt4(a, i)
term(736) = term(736) + wm_interm_20_triplet_pt4(a, i) * wm_interm_3_triplet_pt4(a, i)
term(737) = term(737) + wm_interm_21_triplet_pt4(a, i) * wm_interm_2_triplet_pt4(a, i)
term(738) = term(738) + wm_interm_21_triplet_pt4(a, i) * wm_interm_3_triplet_pt4(a, i)
term(739) = term(739) + wm_interm_20_triplet_pt4(a, i) * wm_interm_8_triplet_pt4(a, i, p, q)
term(740) = term(740) + wm_interm_21_triplet_pt4(a, i) * wm_interm_8_triplet_pt4(a, i, p, q)
term(741) = term(741) + wm_interm_14_triplet_pt4(a, p, i, q) * wm_interm_20_triplet_pt4(a, i)
term(742) = term(742) + wm_interm_14_triplet_pt4(a, i, p, q) * wm_interm_20_triplet_pt4(a, i)
term(743) = term(743) + wm_interm_14_triplet_pt4(a, p, i, q) * wm_interm_21_triplet_pt4(a, i)
term(744) = term(744) + wm_interm_14_triplet_pt4(a, i, p, q) * wm_interm_21_triplet_pt4(a, i)
term(745) = term(745) + wm_interm_1_triplet_pt4(a, i) * wm_interm_27_triplet_pt4(a, i, p, q)
term(746) = term(746) + wm_interm_1_triplet_pt4(a, i) * wm_interm_27_triplet_pt4(a, p, i, q)
term(747) = term(747) + wm_interm_1_triplet_pt4(a, i) * wm_interm_24_triplet_pt4(a, p, i, q)
term(748) = term(748) + wm_interm_1_triplet_pt4(a, i) * wm_interm_24_triplet_pt4(a, i, p, q)
term(749) = term(749) + wm_interm_27_triplet_pt4(a, i, p, q) * wm_interm_2_triplet_pt4(a, i)
term(750) = term(750) + wm_interm_27_triplet_pt4(a, p, i, q) * wm_interm_2_triplet_pt4(a, i)
term(751) = term(751) + wm_interm_24_triplet_pt4(a, p, i, q) * wm_interm_2_triplet_pt4(a, i)
term(752) = term(752) + wm_interm_24_triplet_pt4(a, i, p, q) * wm_interm_2_triplet_pt4(a, i)
term(753) = term(753) + wm_interm_27_triplet_pt4(a, i, p, q) * wm_interm_3_triplet_pt4(a, i)
term(754) = term(754) + wm_interm_27_triplet_pt4(a, p, i, q) * wm_interm_3_triplet_pt4(a, i)
term(755) = term(755) + wm_interm_24_triplet_pt4(a, p, i, q) * wm_interm_3_triplet_pt4(a, i)
term(756) = term(756) + wm_interm_24_triplet_pt4(a, i, p, q) * wm_interm_3_triplet_pt4(a, i)
term(757) = term(757) + r1(vrdav_Rl, a,i) * wm_interm_1_triplet_pt4(a, p) * wm_interm_53_triplet_pt4(q, i)
term(758) = term(758) + r1(vrdav_Rl, a,i) * wm_interm_1_triplet_pt4(a, p) * wm_interm_54_triplet_pt4(q, i)
term(759) = term(759) + r1(vrdav_Rl, a,i) * wm_interm_2_triplet_pt4(a, p) * wm_interm_53_triplet_pt4(q, i)
term(760) = term(760) + r1(vrdav_Rl, a,i) * wm_interm_2_triplet_pt4(a, p) * wm_interm_54_triplet_pt4(q, i)
term(761) = term(761) + r1(vrdav_Rl, a,i) * wm_interm_3_triplet_pt4(a, p) * wm_interm_53_triplet_pt4(q, i)
term(762) = term(762) + r1(vrdav_Rl, a,i) * wm_interm_3_triplet_pt4(a, p) * wm_interm_54_triplet_pt4(q, i)
term(763) = term(763) + s1(a,q) * wm_interm_47_triplet_pt4(a, i) * wm_interm_67_triplet_pt4(p, i)
term(764) = term(764) + s1(a,q) * wm_interm_47_triplet_pt4(a, i) * wm_interm_68_triplet_pt4(p, i)
term(765) = term(765) + r1(vrdav_Rl, a,q) * wm_interm_42_triplet_pt4(a, i) * wm_interm_67_triplet_pt4(p, i)
term(766) = term(766) + r1(vrdav_Rl, a,q) * wm_interm_42_triplet_pt4(a, i) * wm_interm_68_triplet_pt4(p, i)
term(767) = term(767) + r1(vrdav_Rl, a,q) * wm_interm_48_triplet_pt4(a, i) * wm_interm_67_triplet_pt4(p, i)
term(768) = term(768) + r1(vrdav_Rl, a,q) * wm_interm_48_triplet_pt4(a, i) * wm_interm_68_triplet_pt4(p, i)
term(769) = term(769) + s1(a,q) * wm_interm_52_triplet_pt4(a, i) * wm_interm_67_triplet_pt4(p, i)
term(770) = term(770) + s1(a,q) * wm_interm_52_triplet_pt4(a, i) * wm_interm_68_triplet_pt4(p, i)
term(771) = term(771) + s1(a,q) * wm_interm_47_triplet_pt4(a, i) * wm_interm_75_triplet_pt4(p, i)
term(772) = term(772) + s1(a,q) * wm_interm_47_triplet_pt4(a, i) * wm_interm_76_triplet_pt4(p, i)
term(773) = term(773) + s1(a,q) * wm_interm_47_triplet_pt4(a, i) * wm_interm_77_triplet_pt4(p, i)
term(774) = term(774) + s1(a,q) * wm_interm_47_triplet_pt4(a, i) * wm_interm_78_triplet_pt4(p, i)
term(775) = term(775) + r1(vrdav_Rl, a,q) * wm_interm_42_triplet_pt4(a, i) * wm_interm_75_triplet_pt4(p, i)
term(776) = term(776) + r1(vrdav_Rl, a,q) * wm_interm_42_triplet_pt4(a, i) * wm_interm_76_triplet_pt4(p, i)
term(777) = term(777) + r1(vrdav_Rl, a,q) * wm_interm_42_triplet_pt4(a, i) * wm_interm_77_triplet_pt4(p, i)
term(778) = term(778) + r1(vrdav_Rl, a,q) * wm_interm_42_triplet_pt4(a, i) * wm_interm_78_triplet_pt4(p, i)
term(779) = term(779) + r1(vrdav_Rl, a,q) * wm_interm_48_triplet_pt4(a, i) * wm_interm_75_triplet_pt4(p, i)
term(780) = term(780) + r1(vrdav_Rl, a,q) * wm_interm_48_triplet_pt4(a, i) * wm_interm_76_triplet_pt4(p, i)
term(781) = term(781) + r1(vrdav_Rl, a,q) * wm_interm_48_triplet_pt4(a, i) * wm_interm_77_triplet_pt4(p, i)
term(782) = term(782) + r1(vrdav_Rl, a,q) * wm_interm_48_triplet_pt4(a, i) * wm_interm_78_triplet_pt4(p, i)
term(783) = term(783) + s1(a,q) * wm_interm_52_triplet_pt4(a, i) * wm_interm_75_triplet_pt4(p, i)
term(784) = term(784) + s1(a,q) * wm_interm_52_triplet_pt4(a, i) * wm_interm_76_triplet_pt4(p, i)
term(785) = term(785) + s1(a,q) * wm_interm_52_triplet_pt4(a, i) * wm_interm_77_triplet_pt4(p, i)
term(786) = term(786) + s1(a,q) * wm_interm_52_triplet_pt4(a, i) * wm_interm_78_triplet_pt4(p, i)
term(787) = term(787) + r1(vrdav_Rl, a,p) * wm_interm_1_triplet_pt4(a, i) * wm_interm_53_triplet_pt4(i, q)
term(788) = term(788) + r1(vrdav_Rl, a,p) * wm_interm_1_triplet_pt4(a, i) * wm_interm_54_triplet_pt4(i, q)
term(789) = term(789) + s1(a,i) * wm_interm_54_triplet_pt4(p, q) * wm_interm_82_triplet_pt4(a, i)
term(790) = term(790) + s1(a,i) * wm_interm_53_triplet_pt4(p, q) * wm_interm_82_triplet_pt4(a, i)
term(791) = term(791) + r1(vrdav_Rl, a,p) * wm_interm_2_triplet_pt4(a, i) * wm_interm_53_triplet_pt4(i, q)
term(792) = term(792) + r1(vrdav_Rl, a,p) * wm_interm_2_triplet_pt4(a, i) * wm_interm_54_triplet_pt4(i, q)
term(793) = term(793) + s1(a,i) * wm_interm_54_triplet_pt4(p, q) * wm_interm_83_triplet_pt4(a, i)
term(794) = term(794) + r1(vrdav_Rl, a,p) * wm_interm_3_triplet_pt4(a, i) * wm_interm_53_triplet_pt4(i, q)
term(795) = term(795) + r1(vrdav_Rl, a,p) * wm_interm_3_triplet_pt4(a, i) * wm_interm_54_triplet_pt4(i, q)
term(796) = term(796) + s1(a,i) * wm_interm_54_triplet_pt4(p, q) * wm_interm_84_triplet_pt4(a, i)
term(797) = term(797) + s1(a,i) * wm_interm_53_triplet_pt4(p, q) * wm_interm_83_triplet_pt4(a, i)
term(798) = term(798) + s1(a,i) * wm_interm_53_triplet_pt4(p, q) * wm_interm_84_triplet_pt4(a, i)
term(799) = term(799) + r1(vrdav_Rl, a,i) * wm_interm_42_triplet_pt4(a, q) * wm_interm_67_triplet_pt4(i, p)
term(800) = term(800) + s1(a,i) * wm_interm_52_triplet_pt4(a, q) * wm_interm_67_triplet_pt4(i, p)
term(801) = term(801) + r1(vrdav_Rl, a,i) * wm_interm_42_triplet_pt4(a, q) * wm_interm_68_triplet_pt4(i, p)
term(802) = term(802) + s1(a,i) * wm_interm_52_triplet_pt4(a, q) * wm_interm_68_triplet_pt4(i, p)
term(803) = term(803) + s1(a,p) * wm_interm_53_triplet_pt4(i, q) * wm_interm_82_triplet_pt4(a, i)
term(804) = term(804) + s1(a,p) * wm_interm_54_triplet_pt4(i, q) * wm_interm_82_triplet_pt4(a, i)
term(805) = term(805) + s1(a,i) * wm_interm_47_triplet_pt4(a, q) * wm_interm_67_triplet_pt4(i, p)
term(806) = term(806) + r1(vrdav_Rl, a,i) * wm_interm_48_triplet_pt4(a, q) * wm_interm_67_triplet_pt4(i, p)
term(807) = term(807) + s1(a,i) * wm_interm_47_triplet_pt4(a, q) * wm_interm_68_triplet_pt4(i, p)
term(808) = term(808) + r1(vrdav_Rl, a,i) * wm_interm_48_triplet_pt4(a, q) * wm_interm_68_triplet_pt4(i, p)
term(809) = term(809) + r1(vrdav_Rl, a,i) * wm_interm_42_triplet_pt4(a, q) * wm_interm_75_triplet_pt4(i, p)
term(810) = term(810) + r1(vrdav_Rl, a,i) * wm_interm_42_triplet_pt4(a, q) * wm_interm_76_triplet_pt4(i, p)
term(811) = term(811) + s1(a,i) * wm_interm_52_triplet_pt4(a, q) * wm_interm_75_triplet_pt4(i, p)
term(812) = term(812) + s1(a,i) * wm_interm_52_triplet_pt4(a, q) * wm_interm_76_triplet_pt4(i, p)
term(813) = term(813) + r1(vrdav_Rl, a,i) * wm_interm_42_triplet_pt4(a, q) * wm_interm_77_triplet_pt4(i, p)
term(814) = term(814) + s1(a,i) * wm_interm_52_triplet_pt4(a, q) * wm_interm_77_triplet_pt4(i, p)
term(815) = term(815) + r1(vrdav_Rl, a,i) * wm_interm_42_triplet_pt4(a, q) * wm_interm_78_triplet_pt4(i, p)
term(816) = term(816) + s1(a,i) * wm_interm_52_triplet_pt4(a, q) * wm_interm_78_triplet_pt4(i, p)
term(817) = term(817) + s1(a,p) * wm_interm_53_triplet_pt4(i, q) * wm_interm_84_triplet_pt4(a, i)
term(818) = term(818) + s1(a,p) * wm_interm_53_triplet_pt4(i, q) * wm_interm_83_triplet_pt4(a, i)
term(819) = term(819) + s1(a,p) * wm_interm_54_triplet_pt4(i, q) * wm_interm_84_triplet_pt4(a, i)
term(820) = term(820) + s1(a,p) * wm_interm_54_triplet_pt4(i, q) * wm_interm_83_triplet_pt4(a, i)
term(821) = term(821) + s1(a,i) * wm_interm_47_triplet_pt4(a, q) * wm_interm_75_triplet_pt4(i, p)
term(822) = term(822) + s1(a,i) * wm_interm_47_triplet_pt4(a, q) * wm_interm_76_triplet_pt4(i, p)
term(823) = term(823) + r1(vrdav_Rl, a,i) * wm_interm_48_triplet_pt4(a, q) * wm_interm_75_triplet_pt4(i, p)
term(824) = term(824) + r1(vrdav_Rl, a,i) * wm_interm_48_triplet_pt4(a, q) * wm_interm_76_triplet_pt4(i, p)
term(825) = term(825) + s1(a,i) * wm_interm_47_triplet_pt4(a, q) * wm_interm_77_triplet_pt4(i, p)
term(826) = term(826) + r1(vrdav_Rl, a,i) * wm_interm_48_triplet_pt4(a, q) * wm_interm_77_triplet_pt4(i, p)
term(827) = term(827) + s1(a,i) * wm_interm_47_triplet_pt4(a, q) * wm_interm_78_triplet_pt4(i, p)
term(828) = term(828) + r1(vrdav_Rl, a,i) * wm_interm_48_triplet_pt4(a, q) * wm_interm_78_triplet_pt4(i, p)
term(829) = term(829) + t1(a,q) * wm_interm_53_triplet_pt4(p, i) * wm_interm_86_triplet_pt4(a, i)
term(830) = term(830) + t1(a,q) * wm_interm_54_triplet_pt4(p, i) * wm_interm_86_triplet_pt4(a, i)
term(831) = term(831) + t1(a,q) * wm_interm_44_triplet_pt4(a, i) * wm_interm_87_triplet_pt4(p, i)
term(832) = term(832) + t1(a,q) * wm_interm_46_triplet_pt4(a, i) * wm_interm_87_triplet_pt4(p, i)
term(833) = term(833) + t1(a,q) * wm_interm_44_triplet_pt4(a, i) * wm_interm_89_triplet_pt4(p, i)
term(834) = term(834) + t1(a,q) * wm_interm_46_triplet_pt4(a, i) * wm_interm_89_triplet_pt4(p, i)
term(835) = term(835) + t1(a,i) * wm_interm_44_triplet_pt4(a, p) * wm_interm_87_triplet_pt4(i, q)
term(836) = term(836) + t1(a,i) * wm_interm_46_triplet_pt4(a, p) * wm_interm_87_triplet_pt4(i, q)
term(837) = term(837) + t1(a,i) * wm_interm_44_triplet_pt4(a, p) * wm_interm_89_triplet_pt4(i, q)
term(838) = term(838) + t1(a,i) * wm_interm_46_triplet_pt4(a, p) * wm_interm_89_triplet_pt4(i, q)
term(839) = term(839) + t1(a,i) * wm_interm_54_triplet_pt4(i, q) * wm_interm_86_triplet_pt4(a, p)
term(840) = term(840) + t1(a,i) * wm_interm_53_triplet_pt4(i, q) * wm_interm_86_triplet_pt4(a, p)
term(841) = term(841) + r1(vrdav_Rr, a,i) * wm_interm_0_triplet_pt4(a, p) * wm_interm_53_triplet_pt4(i, q)
term(842) = term(842) + r1(vrdav_Rr, a,i) * wm_interm_0_triplet_pt4(a, p) * wm_interm_54_triplet_pt4(i, q)
term(843) = term(843) + t1(a,i) * wm_interm_54_triplet_pt4(p, q) * wm_interm_86_triplet_pt4(a, i)
term(844) = term(844) + t1(a,i) * wm_interm_53_triplet_pt4(p, q) * wm_interm_86_triplet_pt4(a, i)
term(845) = term(845) + r1(vrdav_Rr, a,p) * wm_interm_20_triplet_pt4(a, i) * wm_interm_53_triplet_pt4(q, i)
term(846) = term(846) + r1(vrdav_Rr, a,p) * wm_interm_20_triplet_pt4(a, i) * wm_interm_54_triplet_pt4(q, i)
term(847) = term(847) + r1(vrdav_Rr, a,p) * wm_interm_21_triplet_pt4(a, i) * wm_interm_53_triplet_pt4(q, i)
term(848) = term(848) + r1(vrdav_Rr, a,p) * wm_interm_21_triplet_pt4(a, i) * wm_interm_54_triplet_pt4(q, i)
term(849) = term(849) + t1(a,q) * wm_interm_53_triplet_pt4(p, i) * wm_interm_96_triplet_pt4(a, i)
term(850) = term(850) + t1(a,q) * wm_interm_54_triplet_pt4(p, i) * wm_interm_96_triplet_pt4(a, i)
term(851) = term(851) + t1(a,q) * wm_interm_53_triplet_pt4(p, i) * wm_interm_93_triplet_pt4(a, i)
term(852) = term(852) + t1(a,q) * wm_interm_54_triplet_pt4(p, i) * wm_interm_93_triplet_pt4(a, i)
term(853) = term(853) + t1(a,q) * wm_interm_44_triplet_pt4(a, i) * wm_interm_95_triplet_pt4(p, i)
term(854) = term(854) + t1(a,q) * wm_interm_46_triplet_pt4(a, i) * wm_interm_95_triplet_pt4(p, i)
term(855) = term(855) + t1(a,q) * wm_interm_44_triplet_pt4(a, i) * wm_interm_98_triplet_pt4(p, i)
term(856) = term(856) + t1(a,q) * wm_interm_46_triplet_pt4(a, i) * wm_interm_98_triplet_pt4(p, i)
term(857) = term(857) + t1(a,i) * wm_interm_44_triplet_pt4(a, p) * wm_interm_95_triplet_pt4(i, q)
term(858) = term(858) + t1(a,i) * wm_interm_46_triplet_pt4(a, p) * wm_interm_95_triplet_pt4(i, q)
term(859) = term(859) + t1(a,i) * wm_interm_44_triplet_pt4(a, p) * wm_interm_98_triplet_pt4(i, q)
term(860) = term(860) + t1(a,i) * wm_interm_46_triplet_pt4(a, p) * wm_interm_98_triplet_pt4(i, q)
term(861) = term(861) + t1(a,i) * wm_interm_54_triplet_pt4(i, q) * wm_interm_96_triplet_pt4(a, p)
term(862) = term(862) + t1(a,i) * wm_interm_53_triplet_pt4(i, q) * wm_interm_96_triplet_pt4(a, p)
term(863) = term(863) + t1(a,i) * wm_interm_53_triplet_pt4(i, q) * wm_interm_93_triplet_pt4(a, p)
term(864) = term(864) + t1(a,i) * wm_interm_54_triplet_pt4(i, q) * wm_interm_93_triplet_pt4(a, p)
term(865) = term(865) + r1(vrdav_Rr, a,i) * wm_interm_20_triplet_pt4(a, p) * wm_interm_53_triplet_pt4(i, q)
term(866) = term(866) + r1(vrdav_Rr, a,i) * wm_interm_20_triplet_pt4(a, p) * wm_interm_54_triplet_pt4(i, q)
term(867) = term(867) + t1(a,i) * wm_interm_54_triplet_pt4(p, q) * wm_interm_93_triplet_pt4(a, i)
term(868) = term(868) + r1(vrdav_Rr, a,i) * wm_interm_21_triplet_pt4(a, p) * wm_interm_53_triplet_pt4(i, q)
term(869) = term(869) + r1(vrdav_Rr, a,i) * wm_interm_21_triplet_pt4(a, p) * wm_interm_54_triplet_pt4(i, q)
term(870) = term(870) + t1(a,i) * wm_interm_54_triplet_pt4(p, q) * wm_interm_96_triplet_pt4(a, i)
term(871) = term(871) + t1(a,i) * wm_interm_53_triplet_pt4(p, q) * wm_interm_93_triplet_pt4(a, i)
term(872) = term(872) + t1(a,i) * wm_interm_53_triplet_pt4(p, q) * wm_interm_96_triplet_pt4(a, i)
end do 
end do 

term(724) = term(724) * (-36.0d+0) 
term(725) = term(725) * (-48.0d+0) 
term(726) = term(726) * 24.0d+0 
term(727) = term(727) * (-18.0d+0) 
term(728) = term(728) * 12.0d+0 
term(729) = term(729) * (-24.0d+0) 
term(730) = term(730) * 18.0d+0 
term(731) = term(731) * 24.0d+0 
term(732) = term(732) * (-12.0d+0) 
term(733) = term(733) * (-48.0d+0) 
term(734) = term(734) * 24.0d+0 
term(735) = term(735) * (-64.0d+0) 
term(736) = term(736) * 32.0d+0 
term(737) = term(737) * 32.0d+0 
term(738) = term(738) * (-16.0d+0) 
term(739) = term(739) * (-24.0d+0) 
term(740) = term(740) * 12.0d+0 
term(741) = term(741) * 16.0d+0 
term(742) = term(742) * (-32.0d+0) 
term(743) = term(743) * (-8.0d+0) 
term(744) = term(744) * 16.0d+0 
term(745) = term(745) * (-6.0d+0) 
term(746) = term(746) * 12.0d+0 
term(747) = term(747) * (-6.0d+0) 
term(748) = term(748) * 12.0d+0 
term(749) = term(749) * (-8.0d+0) 
term(750) = term(750) * 16.0d+0 
term(751) = term(751) * (-8.0d+0) 
term(752) = term(752) * 16.0d+0 
term(753) = term(753) * 4.0d+0 
term(754) = term(754) * (-8.0d+0) 
term(755) = term(755) * 4.0d+0 
term(756) = term(756) * (-8.0d+0) 
term(757) = term(757) * (-6.0d+0) 
term(758) = term(758) * 12.0d+0 
term(759) = term(759) * (-8.0d+0) 
term(760) = term(760) * 16.0d+0 
term(761) = term(761) * 4.0d+0 
term(762) = term(762) * (-8.0d+0) 
term(763) = term(763) * 6.0d+0 
term(764) = term(764) * (-6.0d+0) 
term(765) = term(765) * 6.0d+0 
term(766) = term(766) * (-6.0d+0) 
term(767) = term(767) * (-12.0d+0) 
term(768) = term(768) * 12.0d+0 
term(769) = term(769) * (-12.0d+0) 
term(770) = term(770) * 12.0d+0 
term(771) = term(771) * 4.0d+0 
term(772) = term(772) * (-8.0d+0) 
term(773) = term(773) * 4.0d+0 
term(774) = term(774) * (-8.0d+0) 
term(775) = term(775) * 4.0d+0 
term(776) = term(776) * (-8.0d+0) 
term(777) = term(777) * 4.0d+0 
term(778) = term(778) * (-8.0d+0) 
term(779) = term(779) * (-8.0d+0) 
term(780) = term(780) * 16.0d+0 
term(781) = term(781) * (-8.0d+0) 
term(782) = term(782) * 16.0d+0 
term(783) = term(783) * (-8.0d+0) 
term(784) = term(784) * 16.0d+0 
term(785) = term(785) * (-8.0d+0) 
term(786) = term(786) * 16.0d+0 
term(787) = term(787) * (-6.0d+0) 
term(788) = term(788) * 12.0d+0 
term(789) = term(789) * (-24.0d+0) 
term(790) = term(790) * 12.0d+0 
term(791) = term(791) * (-8.0d+0) 
term(792) = term(792) * 16.0d+0 
term(793) = term(793) * (-32.0d+0) 
term(794) = term(794) * 4.0d+0 
term(795) = term(795) * (-8.0d+0) 
term(796) = term(796) * 16.0d+0 
term(797) = term(797) * 16.0d+0 
term(798) = term(798) * (-8.0d+0) 
term(799) = term(799) * 6.0d+0 
term(800) = term(800) * 6.0d+0 
term(801) = term(801) * (-6.0d+0) 
term(802) = term(802) * (-6.0d+0) 
term(803) = term(803) * (-12.0d+0) 
term(804) = term(804) * 24.0d+0 
term(805) = term(805) * (-3.0d+0) 
term(806) = term(806) * (-12.0d+0) 
term(807) = term(807) * 3.0d+0 
term(808) = term(808) * 12.0d+0 
term(809) = term(809) * 4.0d+0 
term(810) = term(810) * (-8.0d+0) 
term(811) = term(811) * (-8.0d+0) 
term(812) = term(812) * 16.0d+0 
term(813) = term(813) * 4.0d+0 
term(814) = term(814) * (-8.0d+0) 
term(815) = term(815) * (-8.0d+0) 
term(816) = term(816) * 16.0d+0 
term(817) = term(817) * 8.0d+0 
term(818) = term(818) * (-16.0d+0) 
term(819) = term(819) * (-16.0d+0) 
term(820) = term(820) * 32.0d+0 
term(821) = term(821) * 4.0d+0 
term(822) = term(822) * (-8.0d+0) 
term(823) = term(823) * (-8.0d+0) 
term(824) = term(824) * 16.0d+0 
term(825) = term(825) * 4.0d+0 
term(826) = term(826) * (-8.0d+0) 
term(827) = term(827) * (-8.0d+0) 
term(828) = term(828) * 16.0d+0 
term(829) = term(829) * 6.0d+0 
term(830) = term(830) * (-12.0d+0) 
term(831) = term(831) * (-3.0d+0) 
term(832) = term(832) * 6.0d+0 
term(833) = term(833) * 3.0d+0 
term(834) = term(834) * (-6.0d+0) 
term(836) = term(836) * (-2.0d+0) 
term(838) = term(838) * (-2.0d+0) 
term(839) = term(839) * (-12.0d+0) 
term(840) = term(840) * 6.0d+0 
term(841) = term(841) * (-6.0d+0) 
term(842) = term(842) * 12.0d+0 
term(843) = term(843) * 24.0d+0 
term(844) = term(844) * (-12.0d+0) 
term(845) = term(845) * 16.0d+0 
term(846) = term(846) * (-32.0d+0) 
term(847) = term(847) * (-8.0d+0) 
term(848) = term(848) * 16.0d+0 
term(849) = term(849) * (-4.0d+0) 
term(850) = term(850) * 8.0d+0 
term(851) = term(851) * 8.0d+0 
term(852) = term(852) * (-16.0d+0) 
term(853) = term(853) * (-4.0d+0) 
term(854) = term(854) * 8.0d+0 
term(855) = term(855) * 8.0d+0 
term(856) = term(856) * (-16.0d+0) 
term(857) = term(857) * (-4.0d+0) 
term(858) = term(858) * 8.0d+0 
term(859) = term(859) * 8.0d+0 
term(860) = term(860) * (-16.0d+0) 
term(861) = term(861) * 8.0d+0 
term(862) = term(862) * (-4.0d+0) 
term(863) = term(863) * 8.0d+0 
term(864) = term(864) * (-16.0d+0) 
term(865) = term(865) * 8.0d+0 
term(866) = term(866) * (-16.0d+0) 
term(867) = term(867) * 32.0d+0 
term(868) = term(868) * (-4.0d+0) 
term(869) = term(869) * 8.0d+0 
term(870) = term(870) * (-16.0d+0) 
term(871) = term(871) * (-16.0d+0) 
term(872) = term(872) * 8.0d+0 

do k = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(873) = term(873) + s2(a,b,q,i) * wm_interm_43_triplet_pt4(a, j, i, k) * wm_interm_71_triplet_pt4(b, k, j, p)
term(874) = term(874) + s2(a,b,q,i) * wm_interm_14_triplet_pt4(a, j, p, k) * wm_interm_51_triplet_pt4(b, j, k, i)
term(875) = term(875) + s2(a,b,q,i) * wm_interm_14_triplet_pt4(a, p, j, k) * wm_interm_51_triplet_pt4(b, j, k, i)
term(876) = term(876) + s2(a,b,q,i) * wm_interm_51_triplet_pt4(b, j, k, i) * wm_interm_79_triplet_pt4(a, p, j, k)
term(877) = term(877) + s2(a,b,q,i) * wm_interm_51_triplet_pt4(b, j, k, i) * wm_interm_79_triplet_pt4(a, j, p, k)
term(878) = term(878) + s2(a,b,i,q) * wm_interm_14_triplet_pt4(a, j, p, k) * wm_interm_51_triplet_pt4(b, j, k, i)
term(879) = term(879) + s2(a,b,i,q) * wm_interm_14_triplet_pt4(a, p, j, k) * wm_interm_51_triplet_pt4(b, j, k, i)
term(880) = term(880) + s2(a,b,i,q) * wm_interm_51_triplet_pt4(b, j, k, i) * wm_interm_79_triplet_pt4(a, p, j, k)
term(881) = term(881) + s2(a,b,i,q) * wm_interm_51_triplet_pt4(b, j, k, i) * wm_interm_79_triplet_pt4(a, j, p, k)
term(882) = term(882) + s2(a,b,q,i) * wm_interm_51_triplet_pt4(b, j, i, k) * wm_interm_79_triplet_pt4(a, p, j, k)
term(883) = term(883) + s2(a,b,q,i) * wm_interm_51_triplet_pt4(b, j, i, k) * wm_interm_79_triplet_pt4(a, j, p, k)
term(884) = term(884) + s2(a,b,q,i) * wm_interm_14_triplet_pt4(a, j, p, k) * wm_interm_51_triplet_pt4(b, j, i, k)
term(885) = term(885) + s2(a,b,q,i) * wm_interm_14_triplet_pt4(a, p, j, k) * wm_interm_51_triplet_pt4(b, j, i, k)
term(886) = term(886) + s2(a,b,p,i) * wm_interm_43_triplet_pt4(a, j, q, k) * wm_interm_64_triplet_pt4(b, k, j, i)
term(887) = term(887) + s2(a,b,i,p) * wm_interm_43_triplet_pt4(a, j, q, k) * wm_interm_64_triplet_pt4(b, k, j, i)
term(888) = term(888) + s2(a,b,i,p) * wm_interm_43_triplet_pt4(a, j, q, k) * wm_interm_71_triplet_pt4(b, k, j, i)
term(889) = term(889) + s2(a,b,p,i) * wm_interm_43_triplet_pt4(a, j, q, k) * wm_interm_71_triplet_pt4(b, k, j, i)
term(890) = term(890) + s2(a,b,p,i) * wm_interm_51_triplet_pt4(b, j, q, k) * wm_interm_79_triplet_pt4(a, j, i, k)
term(891) = term(891) + s2(a,b,p,i) * wm_interm_51_triplet_pt4(b, j, q, k) * wm_interm_79_triplet_pt4(a, i, j, k)
term(892) = term(892) + s2(a,b,p,i) * wm_interm_14_triplet_pt4(a, i, j, k) * wm_interm_51_triplet_pt4(b, j, q, k)
term(893) = term(893) + s2(a,b,p,i) * wm_interm_14_triplet_pt4(a, j, i, k) * wm_interm_51_triplet_pt4(b, j, q, k)
term(894) = term(894) + s2(a,b,p,i) * wm_interm_51_triplet_pt4(b, j, k, q) * wm_interm_79_triplet_pt4(a, i, j, k)
term(895) = term(895) + s2(a,b,p,i) * wm_interm_51_triplet_pt4(b, j, k, q) * wm_interm_79_triplet_pt4(a, j, i, k)
term(896) = term(896) + s2(a,b,p,i) * wm_interm_14_triplet_pt4(a, j, i, k) * wm_interm_51_triplet_pt4(b, j, k, q)
term(897) = term(897) + s2(a,b,p,i) * wm_interm_14_triplet_pt4(a, i, j, k) * wm_interm_51_triplet_pt4(b, j, k, q)
term(898) = term(898) + t2(a,b,q,i) * wm_interm_24_triplet_pt4(a, j, i, k) * wm_interm_45_triplet_pt4(b, j, k, p)
term(899) = term(899) + t2(a,b,q,i) * wm_interm_24_triplet_pt4(a, i, j, k) * wm_interm_45_triplet_pt4(b, j, k, p)
term(900) = term(900) + t2(a,b,q,i) * wm_interm_24_triplet_pt4(a, i, j, k) * wm_interm_45_triplet_pt4(b, j, p, k)
term(901) = term(901) + t2(a,b,q,i) * wm_interm_24_triplet_pt4(a, j, i, k) * wm_interm_45_triplet_pt4(b, j, p, k)
term(902) = term(902) + t2(a,b,q,i) * wm_interm_24_triplet_pt4(a, j, p, k) * wm_interm_45_triplet_pt4(b, j, k, i)
term(903) = term(903) + t2(a,b,q,i) * wm_interm_24_triplet_pt4(a, p, j, k) * wm_interm_45_triplet_pt4(b, j, k, i)
term(904) = term(904) + t2(a,b,q,i) * wm_interm_24_triplet_pt4(a, j, p, k) * wm_interm_45_triplet_pt4(b, j, i, k)
term(905) = term(905) + t2(a,b,q,i) * wm_interm_24_triplet_pt4(a, p, j, k) * wm_interm_45_triplet_pt4(b, j, i, k)
term(906) = term(906) + t2(a,b,q,i) * wm_interm_24_triplet_pt4(b, p, j, k) * wm_interm_45_triplet_pt4(a, j, k, i)
term(907) = term(907) + t2(a,b,q,i) * wm_interm_24_triplet_pt4(b, p, j, k) * wm_interm_45_triplet_pt4(a, j, i, k)
term(908) = term(908) + t2(a,b,q,i) * wm_interm_24_triplet_pt4(b, j, p, k) * wm_interm_45_triplet_pt4(a, j, k, i)
term(909) = term(909) + t2(a,b,q,i) * wm_interm_24_triplet_pt4(b, j, i, k) * wm_interm_45_triplet_pt4(a, j, k, p)
term(910) = term(910) + t2(a,b,q,i) * wm_interm_24_triplet_pt4(b, i, j, k) * wm_interm_45_triplet_pt4(a, j, k, p)
term(911) = term(911) + t2(a,b,q,i) * wm_interm_24_triplet_pt4(b, j, i, k) * wm_interm_45_triplet_pt4(a, j, p, k)
term(912) = term(912) + t2(a,b,q,i) * wm_interm_24_triplet_pt4(b, i, j, k) * wm_interm_45_triplet_pt4(a, j, p, k)
term(913) = term(913) + t2(a,b,q,i) * wm_interm_24_triplet_pt4(b, j, p, k) * wm_interm_45_triplet_pt4(a, j, i, k)
term(914) = term(914) + t2(a,b,q,i) * wm_interm_27_triplet_pt4(b, p, j, k) * wm_interm_45_triplet_pt4(a, j, k, i)
term(915) = term(915) + t2(a,b,q,i) * wm_interm_27_triplet_pt4(b, i, j, k) * wm_interm_45_triplet_pt4(a, j, k, p)
term(916) = term(916) + t2(a,b,q,i) * wm_interm_27_triplet_pt4(b, j, p, k) * wm_interm_45_triplet_pt4(a, j, k, i)
term(917) = term(917) + t2(a,b,q,i) * wm_interm_27_triplet_pt4(b, j, i, k) * wm_interm_45_triplet_pt4(a, j, k, p)
term(918) = term(918) + t2(a,b,q,i) * wm_interm_27_triplet_pt4(a, i, j, k) * wm_interm_45_triplet_pt4(b, j, k, p)
term(919) = term(919) + t2(a,b,q,i) * wm_interm_27_triplet_pt4(a, j, i, k) * wm_interm_45_triplet_pt4(b, j, k, p)
term(920) = term(920) + t2(a,b,q,i) * wm_interm_27_triplet_pt4(a, j, i, k) * wm_interm_45_triplet_pt4(b, j, p, k)
term(921) = term(921) + t2(a,b,q,i) * wm_interm_27_triplet_pt4(a, i, j, k) * wm_interm_45_triplet_pt4(b, j, p, k)
term(922) = term(922) + t2(a,b,q,i) * wm_interm_27_triplet_pt4(b, i, j, k) * wm_interm_45_triplet_pt4(a, j, p, k)
term(923) = term(923) + t2(a,b,q,i) * wm_interm_27_triplet_pt4(b, j, i, k) * wm_interm_45_triplet_pt4(a, j, p, k)
term(924) = term(924) + t2(a,b,q,i) * wm_interm_27_triplet_pt4(a, p, j, k) * wm_interm_45_triplet_pt4(b, j, k, i)
term(925) = term(925) + t2(a,b,q,i) * wm_interm_27_triplet_pt4(a, j, p, k) * wm_interm_45_triplet_pt4(b, j, k, i)
term(926) = term(926) + t2(a,b,q,i) * wm_interm_27_triplet_pt4(b, j, p, k) * wm_interm_45_triplet_pt4(a, j, i, k)
term(927) = term(927) + t2(a,b,q,i) * wm_interm_27_triplet_pt4(b, p, j, k) * wm_interm_45_triplet_pt4(a, j, i, k)
term(928) = term(928) + t2(a,b,q,i) * wm_interm_27_triplet_pt4(a, p, j, k) * wm_interm_45_triplet_pt4(b, j, i, k)
term(929) = term(929) + t2(a,b,q,i) * wm_interm_27_triplet_pt4(a, j, p, k) * wm_interm_45_triplet_pt4(b, j, i, k)
end do 
end do 
end do 
end do 
end do 

term(873) = term(873) * 8.0d+0 
term(874) = term(874) * (-8.0d+0) 
term(875) = term(875) * 16.0d+0 
term(876) = term(876) * (-8.0d+0) 
term(877) = term(877) * 16.0d+0 
term(878) = term(878) * 4.0d+0 
term(879) = term(879) * (-8.0d+0) 
term(880) = term(880) * 4.0d+0 
term(881) = term(881) * (-8.0d+0) 
term(882) = term(882) * 4.0d+0 
term(883) = term(883) * (-8.0d+0) 
term(884) = term(884) * 4.0d+0 
term(885) = term(885) * (-8.0d+0) 
term(886) = term(886) * (-6.0d+0) 
term(887) = term(887) * 6.0d+0 
term(888) = term(888) * 8.0d+0 
term(889) = term(889) * (-16.0d+0) 
term(890) = term(890) * 4.0d+0 
term(891) = term(891) * (-8.0d+0) 
term(892) = term(892) * 4.0d+0 
term(893) = term(893) * (-8.0d+0) 
term(894) = term(894) * 4.0d+0 
term(895) = term(895) * (-8.0d+0) 
term(896) = term(896) * 4.0d+0 
term(897) = term(897) * (-8.0d+0) 
term(898) = term(898) * (-2.0d+0) 
term(899) = term(899) * 4.0d+0 
term(900) = term(900) * (-2.0d+0) 
term(901) = term(901) * 4.0d+0 
term(902) = term(902) * 4.0d+0 
term(903) = term(903) * (-8.0d+0) 
term(904) = term(904) * (-2.0d+0) 
term(905) = term(905) * 4.0d+0 
term(906) = term(906) * 4.0d+0 
term(907) = term(907) * (-2.0d+0) 
term(908) = term(908) * (-2.0d+0) 
term(909) = term(909) * 4.0d+0 
term(910) = term(910) * (-8.0d+0) 
term(911) = term(911) * (-2.0d+0) 
term(912) = term(912) * 4.0d+0 
term(913) = term(913) * 4.0d+0 
term(914) = term(914) * (-2.0d+0) 
term(915) = term(915) * 4.0d+0 
term(916) = term(916) * 4.0d+0 
term(917) = term(917) * (-8.0d+0) 
term(918) = term(918) * (-2.0d+0) 
term(919) = term(919) * 4.0d+0 
term(920) = term(920) * (-2.0d+0) 
term(921) = term(921) * 4.0d+0 
term(922) = term(922) * (-2.0d+0) 
term(923) = term(923) * 4.0d+0 
term(924) = term(924) * 4.0d+0 
term(925) = term(925) * (-8.0d+0) 
term(926) = term(926) * (-2.0d+0) 
term(927) = term(927) * 4.0d+0 
term(928) = term(928) * (-2.0d+0) 
term(929) = term(929) * 4.0d+0 

do j = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(930) = term(930) + s2(a,b,i,j) * wm_interm_43_triplet_pt4(a, q, k, j) * wm_interm_71_triplet_pt4(b, k, i, p)
term(931) = term(931) + s2(a,b,i,j) * wm_interm_43_triplet_pt4(a, k, i, q) * wm_interm_64_triplet_pt4(b, p, k, j)
term(932) = term(932) + s2(a,b,i,p) * wm_interm_43_triplet_pt4(a, j, k, q) * wm_interm_64_triplet_pt4(b, k, i, j)
term(933) = term(933) + s2(a,b,i,j) * wm_interm_14_triplet_pt4(a, j, k, p) * wm_interm_51_triplet_pt4(b, k, i, q)
term(934) = term(934) + s2(a,b,i,j) * wm_interm_51_triplet_pt4(b, k, i, q) * wm_interm_79_triplet_pt4(a, j, k, p)
term(935) = term(935) + s2(a,b,i,j) * wm_interm_14_triplet_pt4(a, i, k, p) * wm_interm_51_triplet_pt4(b, k, j, q)
term(936) = term(936) + s2(a,b,i,j) * wm_interm_51_triplet_pt4(b, k, j, q) * wm_interm_79_triplet_pt4(a, i, k, p)
term(937) = term(937) + s2(a,b,i,j) * wm_interm_14_triplet_pt4(a, i, k, p) * wm_interm_51_triplet_pt4(b, k, q, j)
term(938) = term(938) + s2(a,b,i,j) * wm_interm_51_triplet_pt4(b, k, q, j) * wm_interm_79_triplet_pt4(a, i, k, p)
term(939) = term(939) + s2(a,b,i,j) * wm_interm_43_triplet_pt4(a, k, i, q) * wm_interm_71_triplet_pt4(b, p, k, j)
term(940) = term(940) + s2(a,b,i,p) * wm_interm_43_triplet_pt4(a, j, k, q) * wm_interm_71_triplet_pt4(b, k, i, j)
term(941) = term(941) + t2(a,b,i,j) * wm_interm_27_triplet_pt4(b, i, k, q) * wm_interm_45_triplet_pt4(a, k, j, p)
term(942) = term(942) + t2(a,b,i,j) * wm_interm_24_triplet_pt4(b, i, k, q) * wm_interm_45_triplet_pt4(a, k, j, p)
term(943) = term(943) + t2(a,b,i,j) * wm_interm_24_triplet_pt4(b, i, k, q) * wm_interm_45_triplet_pt4(a, k, p, j)
term(944) = term(944) + t2(a,b,i,j) * wm_interm_27_triplet_pt4(b, i, k, q) * wm_interm_45_triplet_pt4(a, k, p, j)
term(945) = term(945) + t2(a,b,i,j) * wm_interm_27_triplet_pt4(b, j, k, q) * wm_interm_45_triplet_pt4(a, k, i, p)
term(946) = term(946) + t2(a,b,i,j) * wm_interm_24_triplet_pt4(b, j, k, q) * wm_interm_45_triplet_pt4(a, k, i, p)
end do 
end do 
end do 
end do 
end do 

term(930) = term(930) * 8.0d+0 
term(931) = term(931) * (-6.0d+0) 
term(932) = term(932) * 6.0d+0 
term(933) = term(933) * 4.0d+0 
term(934) = term(934) * (-8.0d+0) 
term(935) = term(935) * (-8.0d+0) 
term(936) = term(936) * 4.0d+0 
term(937) = term(937) * 16.0d+0 
term(938) = term(938) * (-8.0d+0) 
term(939) = term(939) * (-16.0d+0) 
term(940) = term(940) * 8.0d+0 
term(941) = term(941) * 4.0d+0 
term(942) = term(942) * (-2.0d+0) 
term(943) = term(943) * 4.0d+0 
term(944) = term(944) * (-2.0d+0) 
term(945) = term(945) * (-2.0d+0) 
term(946) = term(946) * 4.0d+0 

do k = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(947) = term(947) + s2(a,b,i,q) * wm_interm_43_triplet_pt4(a, j, i, k) * wm_interm_71_triplet_pt4(b, k, j, p)
term(948) = term(948) + s2(a,b,i,q) * wm_interm_51_triplet_pt4(b, j, i, k) * wm_interm_79_triplet_pt4(a, p, j, k)
term(949) = term(949) + s2(a,b,i,q) * wm_interm_14_triplet_pt4(a, p, j, k) * wm_interm_51_triplet_pt4(b, j, i, k)
term(950) = term(950) + s2(a,b,i,p) * wm_interm_51_triplet_pt4(b, j, k, q) * wm_interm_79_triplet_pt4(a, i, j, k)
term(951) = term(951) + s2(a,b,i,p) * wm_interm_14_triplet_pt4(a, i, j, k) * wm_interm_51_triplet_pt4(b, j, k, q)
term(952) = term(952) + s2(a,b,i,p) * wm_interm_51_triplet_pt4(b, j, q, k) * wm_interm_79_triplet_pt4(a, i, j, k)
term(953) = term(953) + s2(a,b,i,p) * wm_interm_14_triplet_pt4(a, i, j, k) * wm_interm_51_triplet_pt4(b, j, q, k)
end do 
end do 
end do 
end do 
end do 

term(947) = term(947) * (-16.0d+0) 
term(948) = term(948) * (-8.0d+0) 
term(949) = term(949) * 4.0d+0 
term(950) = term(950) * (-8.0d+0) 
term(951) = term(951) * 16.0d+0 
term(952) = term(952) * 4.0d+0 
term(953) = term(953) * (-8.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do l = 1, nocc 
do a = nocc + 1, nactive 
term(954) = term(954) + s2(a,b,i,j) * wm_interm_43_triplet_pt4(a, k, l, i) * wm_interm_64_triplet_pt4(b, l, j, k)
term(955) = term(955) + s2(a,b,i,j) * wm_interm_43_triplet_pt4(a, k, l, i) * wm_interm_71_triplet_pt4(b, l, j, k)
end do 
end do 
end do 
end do 
end do 
end do 

term(954) = term(954) * 36.0d+0 
term(955) = term(955) * 32.0d+0 

do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do l = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(956) = term(956) + s2(a,b,i,j) * wm_interm_43_triplet_pt4(a, k, l, j) * wm_interm_64_triplet_pt4(b, l, i, k)
term(957) = term(957) + s2(a,b,i,j) * wm_interm_43_triplet_pt4(a, k, l, j) * wm_interm_71_triplet_pt4(b, l, i, k)
end do 
end do 
end do 
end do 
end do 
end do 

term(956) = term(956) * (-12.0d+0) 
term(957) = term(957) * (-16.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(958) = term(958) + s2(a,b,i,j) * wm_interm_43_triplet_pt4(a, k, j, l) * wm_interm_64_triplet_pt4(b, l, i, k)
term(959) = term(959) + s2(a,b,i,j) * wm_interm_43_triplet_pt4(a, k, i, l) * wm_interm_64_triplet_pt4(b, l, j, k)
term(960) = term(960) + s2(a,b,i,j) * wm_interm_43_triplet_pt4(a, k, j, l) * wm_interm_71_triplet_pt4(b, l, i, k)
term(961) = term(961) + s2(a,b,i,j) * wm_interm_43_triplet_pt4(a, k, i, l) * wm_interm_71_triplet_pt4(b, l, j, k)
term(962) = term(962) + t2(a,b,i,j) * wm_interm_60_triplet_pt4(a, k, j, l) * wm_interm_85_triplet_pt4(b, i, l, k)
term(963) = term(963) + t2(a,b,i,j) * wm_interm_60_triplet_pt4(a, j, k, l) * wm_interm_85_triplet_pt4(b, i, l, k)
term(964) = term(964) + t2(a,b,i,j) * wm_interm_62_triplet_pt4(a, i, k, l) * wm_interm_85_triplet_pt4(b, j, l, k)
term(965) = term(965) + t2(a,b,i,j) * wm_interm_62_triplet_pt4(a, k, i, l) * wm_interm_85_triplet_pt4(b, j, l, k)
term(966) = term(966) + t2(a,b,i,j) * wm_interm_62_triplet_pt4(a, j, k, l) * wm_interm_85_triplet_pt4(b, l, i, k)
term(967) = term(967) + t2(a,b,i,j) * wm_interm_62_triplet_pt4(a, k, j, l) * wm_interm_85_triplet_pt4(b, l, i, k)
term(968) = term(968) + t2(a,b,i,j) * wm_interm_62_triplet_pt4(a, k, i, l) * wm_interm_85_triplet_pt4(b, l, j, k)
term(969) = term(969) + t2(a,b,i,j) * wm_interm_62_triplet_pt4(a, i, k, l) * wm_interm_85_triplet_pt4(b, l, j, k)
term(970) = term(970) + t2(a,b,i,j) * wm_interm_60_triplet_pt4(a, k, j, l) * wm_interm_94_triplet_pt4(b, i, l, k)
term(971) = term(971) + t2(a,b,i,j) * wm_interm_60_triplet_pt4(a, j, k, l) * wm_interm_94_triplet_pt4(b, i, l, k)
term(972) = term(972) + t2(a,b,i,j) * wm_interm_62_triplet_pt4(a, i, k, l) * wm_interm_92_triplet_pt4(b, j, l, k)
term(973) = term(973) + t2(a,b,i,j) * wm_interm_62_triplet_pt4(a, k, i, l) * wm_interm_92_triplet_pt4(b, j, l, k)
term(974) = term(974) + t2(a,b,i,j) * wm_interm_60_triplet_pt4(a, j, k, l) * wm_interm_92_triplet_pt4(b, i, l, k)
term(975) = term(975) + t2(a,b,i,j) * wm_interm_60_triplet_pt4(a, k, j, l) * wm_interm_92_triplet_pt4(b, i, l, k)
term(976) = term(976) + t2(a,b,i,j) * wm_interm_62_triplet_pt4(a, i, k, l) * wm_interm_94_triplet_pt4(b, j, l, k)
term(977) = term(977) + t2(a,b,i,j) * wm_interm_62_triplet_pt4(a, k, i, l) * wm_interm_94_triplet_pt4(b, j, l, k)
term(978) = term(978) + t2(a,b,i,j) * wm_interm_62_triplet_pt4(a, j, k, l) * wm_interm_92_triplet_pt4(b, l, i, k)
term(979) = term(979) + t2(a,b,i,j) * wm_interm_62_triplet_pt4(a, k, j, l) * wm_interm_92_triplet_pt4(b, l, i, k)
term(980) = term(980) + t2(a,b,i,j) * wm_interm_62_triplet_pt4(a, k, i, l) * wm_interm_92_triplet_pt4(b, l, j, k)
term(981) = term(981) + t2(a,b,i,j) * wm_interm_62_triplet_pt4(a, i, k, l) * wm_interm_92_triplet_pt4(b, l, j, k)
term(982) = term(982) + t2(a,b,i,j) * wm_interm_62_triplet_pt4(a, i, k, l) * wm_interm_94_triplet_pt4(b, l, j, k)
term(983) = term(983) + t2(a,b,i,j) * wm_interm_62_triplet_pt4(a, k, i, l) * wm_interm_94_triplet_pt4(b, l, j, k)
term(984) = term(984) + t2(a,b,i,j) * wm_interm_62_triplet_pt4(a, j, k, l) * wm_interm_94_triplet_pt4(b, l, i, k)
term(985) = term(985) + t2(a,b,i,j) * wm_interm_62_triplet_pt4(a, k, j, l) * wm_interm_94_triplet_pt4(b, l, i, k)
end do 
end do 
end do 
end do 
end do 
end do 

term(958) = term(958) * 12.0d+0 
term(959) = term(959) * (-12.0d+0) 
term(960) = term(960) * 32.0d+0 
term(961) = term(961) * (-16.0d+0) 
term(962) = term(962) * (-12.0d+0) 
term(963) = term(963) * 12.0d+0 
term(964) = term(964) * 12.0d+0 
term(965) = term(965) * (-24.0d+0) 
term(966) = term(966) * 12.0d+0 
term(967) = term(967) * (-12.0d+0) 
term(968) = term(968) * 24.0d+0 
term(969) = term(969) * (-12.0d+0) 
term(970) = term(970) * 8.0d+0 
term(971) = term(971) * (-16.0d+0) 
term(972) = term(972) * 8.0d+0 
term(973) = term(973) * (-16.0d+0) 
term(974) = term(974) * 8.0d+0 
term(975) = term(975) * (-16.0d+0) 
term(976) = term(976) * (-16.0d+0) 
term(977) = term(977) * 32.0d+0 
term(978) = term(978) * 8.0d+0 
term(979) = term(979) * (-16.0d+0) 
term(980) = term(980) * 32.0d+0 
term(981) = term(981) * (-16.0d+0) 
term(982) = term(982) * 8.0d+0 
term(983) = term(983) * (-16.0d+0) 
term(984) = term(984) * (-16.0d+0) 
term(985) = term(985) * 8.0d+0 

do l = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(986) = term(986) + s2(a,b,i,j) * wm_interm_43_triplet_pt4(a, k, i, l) * wm_interm_64_triplet_pt4(b, l, k, j)
term(987) = term(987) + s2(a,b,i,j) * wm_interm_43_triplet_pt4(a, k, i, l) * wm_interm_71_triplet_pt4(b, l, k, j)
term(988) = term(988) + s2(a,b,i,j) * wm_interm_14_triplet_pt4(a, i, k, l) * wm_interm_51_triplet_pt4(b, k, l, j)
term(989) = term(989) + s2(a,b,i,j) * wm_interm_51_triplet_pt4(b, k, l, j) * wm_interm_79_triplet_pt4(a, i, k, l)
term(990) = term(990) + s2(a,b,i,j) * wm_interm_51_triplet_pt4(b, k, i, l) * wm_interm_79_triplet_pt4(a, j, k, l)
term(991) = term(991) + s2(a,b,i,j) * wm_interm_51_triplet_pt4(b, k, j, l) * wm_interm_79_triplet_pt4(a, i, k, l)
term(992) = term(992) + s2(a,b,i,j) * wm_interm_14_triplet_pt4(a, i, k, l) * wm_interm_51_triplet_pt4(b, k, j, l)
term(993) = term(993) + s2(a,b,i,j) * wm_interm_14_triplet_pt4(a, j, k, l) * wm_interm_51_triplet_pt4(b, k, i, l)
term(994) = term(994) + t2(a,b,i,j) * wm_interm_24_triplet_pt4(b, i, k, l) * wm_interm_45_triplet_pt4(a, k, l, j)
term(995) = term(995) + t2(a,b,i,j) * wm_interm_24_triplet_pt4(b, i, k, l) * wm_interm_45_triplet_pt4(a, k, j, l)
term(996) = term(996) + t2(a,b,i,j) * wm_interm_24_triplet_pt4(b, j, k, l) * wm_interm_45_triplet_pt4(a, k, i, l)
term(997) = term(997) + t2(a,b,i,j) * wm_interm_27_triplet_pt4(b, i, k, l) * wm_interm_45_triplet_pt4(a, k, j, l)
term(998) = term(998) + t2(a,b,i,j) * wm_interm_27_triplet_pt4(b, j, k, l) * wm_interm_45_triplet_pt4(a, k, i, l)
term(999) = term(999) + t2(a,b,i,j) * wm_interm_27_triplet_pt4(b, i, k, l) * wm_interm_45_triplet_pt4(a, k, l, j)
end do 
end do 
end do 
end do 
end do 
end do 

term(986) = term(986) * 12.0d+0 
term(987) = term(987) * 32.0d+0 
term(988) = term(988) * (-32.0d+0) 
term(989) = term(989) * 16.0d+0 
term(990) = term(990) * 16.0d+0 
term(991) = term(991) * (-8.0d+0) 
term(992) = term(992) * 16.0d+0 
term(993) = term(993) * (-8.0d+0) 
term(994) = term(994) * (-16.0d+0) 
term(995) = term(995) * 8.0d+0 
term(996) = term(996) * (-16.0d+0) 
term(997) = term(997) * (-16.0d+0) 
term(998) = term(998) * 8.0d+0 
term(999) = term(999) * 8.0d+0 

do l = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(1000) = term(1000) + s2(a,b,i,j) * wm_interm_43_triplet_pt4(a, k, j, l) * wm_interm_64_triplet_pt4(b, l, k, i)
term(1001) = term(1001) + s2(a,b,i,j) * wm_interm_43_triplet_pt4(a, k, j, l) * wm_interm_71_triplet_pt4(b, l, k, i)
term(1002) = term(1002) + s2(a,b,i,j) * wm_interm_14_triplet_pt4(a, k, j, l) * wm_interm_51_triplet_pt4(b, k, l, i)
term(1003) = term(1003) + s2(a,b,i,j) * wm_interm_14_triplet_pt4(a, j, k, l) * wm_interm_51_triplet_pt4(b, k, l, i)
term(1004) = term(1004) + s2(a,b,i,j) * wm_interm_51_triplet_pt4(b, k, l, i) * wm_interm_79_triplet_pt4(a, j, k, l)
term(1005) = term(1005) + s2(a,b,i,j) * wm_interm_51_triplet_pt4(b, k, l, i) * wm_interm_79_triplet_pt4(a, k, j, l)
term(1006) = term(1006) + t2(a,b,i,j) * wm_interm_24_triplet_pt4(b, k, j, l) * wm_interm_45_triplet_pt4(a, k, l, i)
term(1007) = term(1007) + t2(a,b,i,j) * wm_interm_24_triplet_pt4(b, j, k, l) * wm_interm_45_triplet_pt4(a, k, l, i)
term(1008) = term(1008) + t2(a,b,i,j) * wm_interm_27_triplet_pt4(b, k, j, l) * wm_interm_45_triplet_pt4(a, k, l, i)
term(1009) = term(1009) + t2(a,b,i,j) * wm_interm_27_triplet_pt4(b, j, k, l) * wm_interm_45_triplet_pt4(a, k, l, i)
end do 
end do 
end do 
end do 
end do 
end do 

term(1000) = term(1000) * (-12.0d+0) 
term(1001) = term(1001) * (-16.0d+0) 
term(1002) = term(1002) * (-8.0d+0) 
term(1003) = term(1003) * 16.0d+0 
term(1004) = term(1004) * (-8.0d+0) 
term(1005) = term(1005) * 16.0d+0 
term(1006) = term(1006) * (-16.0d+0) 
term(1007) = term(1007) * 32.0d+0 
term(1008) = term(1008) * 32.0d+0 
term(1009) = term(1009) * (-16.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do l = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(1010) = term(1010) + s2(a,b,i,j) * wm_interm_43_triplet_pt4(a, k, l, j) * wm_interm_64_triplet_pt4(b, l, k, i)
term(1011) = term(1011) + s2(a,b,i,j) * wm_interm_43_triplet_pt4(a, k, l, i) * wm_interm_64_triplet_pt4(b, l, k, j)
term(1012) = term(1012) + s2(a,b,i,j) * wm_interm_43_triplet_pt4(a, k, l, i) * wm_interm_71_triplet_pt4(b, l, k, j)
term(1013) = term(1013) + s2(a,b,i,j) * wm_interm_43_triplet_pt4(a, k, l, j) * wm_interm_71_triplet_pt4(b, l, k, i)
end do 
end do 
end do 
end do 
end do 
end do 

term(1010) = term(1010) * 12.0d+0 
term(1011) = term(1011) * (-12.0d+0) 
term(1012) = term(1012) * (-64.0d+0) 
term(1013) = term(1013) * 32.0d+0 

do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(1014) = term(1014) + t1(b,q) * wm_interm_56_triplet_pt4(b, a) * wm_interm_86_triplet_pt4(a, p)
term(1015) = term(1015) + t1(b,q) * wm_interm_58_triplet_pt4(b, a) * wm_interm_86_triplet_pt4(a, p)
term(1016) = term(1016) + t1(b,q) * wm_interm_44_triplet_pt4(a, p) * wm_interm_90_triplet_pt4(b, a)
term(1017) = term(1017) + t1(b,q) * wm_interm_46_triplet_pt4(a, p) * wm_interm_90_triplet_pt4(b, a)
term(1018) = term(1018) + t1(b,q) * wm_interm_44_triplet_pt4(a, p) * wm_interm_91_triplet_pt4(b, a)
term(1019) = term(1019) + t1(b,q) * wm_interm_46_triplet_pt4(a, p) * wm_interm_91_triplet_pt4(b, a)
term(1020) = term(1020) + t1(b,q) * wm_interm_56_triplet_pt4(b, a) * wm_interm_96_triplet_pt4(a, p)
term(1021) = term(1021) + t1(b,q) * wm_interm_58_triplet_pt4(b, a) * wm_interm_96_triplet_pt4(a, p)
term(1022) = term(1022) + t1(b,q) * wm_interm_56_triplet_pt4(b, a) * wm_interm_93_triplet_pt4(a, p)
term(1023) = term(1023) + t1(b,q) * wm_interm_58_triplet_pt4(b, a) * wm_interm_93_triplet_pt4(a, p)
term(1024) = term(1024) + t1(b,q) * wm_interm_44_triplet_pt4(a, p) * wm_interm_99_triplet_pt4(b, a)
term(1025) = term(1025) + t1(b,q) * wm_interm_100_triplet_pt4(b, a) * wm_interm_44_triplet_pt4(a, p)
term(1026) = term(1026) + t1(b,q) * wm_interm_46_triplet_pt4(a, p) * wm_interm_99_triplet_pt4(b, a)
term(1027) = term(1027) + t1(b,q) * wm_interm_100_triplet_pt4(b, a) * wm_interm_46_triplet_pt4(a, p)
end do 
end do 

term(1014) = term(1014) * (-12.0d+0) 
term(1015) = term(1015) * 6.0d+0 
term(1017) = term(1017) * (-2.0d+0) 
term(1019) = term(1019) * (-2.0d+0) 
term(1020) = term(1020) * 8.0d+0 
term(1021) = term(1021) * (-4.0d+0) 
term(1022) = term(1022) * (-16.0d+0) 
term(1023) = term(1023) * 8.0d+0 
term(1024) = term(1024) * (-4.0d+0) 
term(1025) = term(1025) * 8.0d+0 
term(1026) = term(1026) * 8.0d+0 
term(1027) = term(1027) * (-16.0d+0) 

do k = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(1028) = term(1028) + s2(a,b,i,q) * wm_interm_51_triplet_pt4(b, j, i, k) * wm_interm_79_triplet_pt4(a, j, p, k)
term(1029) = term(1029) + s2(a,b,i,q) * wm_interm_14_triplet_pt4(a, j, p, k) * wm_interm_51_triplet_pt4(b, j, i, k)
term(1030) = term(1030) + s2(a,b,i,p) * wm_interm_51_triplet_pt4(b, j, k, q) * wm_interm_79_triplet_pt4(a, j, i, k)
term(1031) = term(1031) + s2(a,b,i,p) * wm_interm_14_triplet_pt4(a, j, i, k) * wm_interm_51_triplet_pt4(b, j, k, q)
term(1032) = term(1032) + s2(a,b,i,p) * wm_interm_51_triplet_pt4(b, j, q, k) * wm_interm_79_triplet_pt4(a, j, i, k)
term(1033) = term(1033) + s2(a,b,i,p) * wm_interm_14_triplet_pt4(a, j, i, k) * wm_interm_51_triplet_pt4(b, j, q, k)
end do 
end do 
end do 
end do 
end do 

term(1028) = term(1028) * 4.0d+0 
term(1029) = term(1029) * (-8.0d+0) 
term(1030) = term(1030) * 16.0d+0 
term(1031) = term(1031) * (-8.0d+0) 
term(1032) = term(1032) * (-8.0d+0) 
term(1033) = term(1033) * 4.0d+0 

do j = 1, nocc 
do i = 1, nocc 
term(1034) = term(1034) + wm_interm_4_triplet_pt4(i, q, p, j) * wm_interm_5_triplet_pt4(j, i)
term(1035) = term(1035) + wm_interm_5_triplet_pt4(i, j) * wm_interm_6_triplet_pt4(j, i)
term(1036) = term(1036) + wm_interm_11_triplet_pt4(i, q, j, p) * wm_interm_5_triplet_pt4(j, i)
term(1037) = term(1037) + wm_interm_11_triplet_pt4(i, q, p, j) * wm_interm_5_triplet_pt4(j, i)
term(1038) = term(1038) + wm_interm_12_triplet_pt4(i, j) * wm_interm_5_triplet_pt4(j, i)
term(1039) = term(1039) + wm_interm_13_triplet_pt4(i, j) * wm_interm_5_triplet_pt4(j, i)
term(1040) = term(1040) + wm_interm_22_triplet_pt4(q, i, p, j) * wm_interm_5_triplet_pt4(j, i)
term(1041) = term(1041) + wm_interm_23_triplet_pt4(i, j) * wm_interm_5_triplet_pt4(j, i)
term(1042) = term(1042) + wm_interm_22_triplet_pt4(i, q, p, j) * wm_interm_5_triplet_pt4(j, i)
term(1043) = term(1043) + wm_interm_26_triplet_pt4(i, j) * wm_interm_5_triplet_pt4(j, i)
term(1044) = term(1044) + wm_interm_29_triplet_pt4(q, i, j, p) * wm_interm_5_triplet_pt4(j, i)
term(1045) = term(1045) + wm_interm_29_triplet_pt4(q, i, p, j) * wm_interm_5_triplet_pt4(j, i)
term(1046) = term(1046) + wm_interm_30_triplet_pt4(i, j) * wm_interm_5_triplet_pt4(j, i)
term(1047) = term(1047) + wm_interm_31_triplet_pt4(i, j) * wm_interm_5_triplet_pt4(j, i)
term(1048) = term(1048) + wm_interm_29_triplet_pt4(i, q, p, j) * wm_interm_5_triplet_pt4(j, i)
term(1049) = term(1049) + wm_interm_29_triplet_pt4(i, q, j, p) * wm_interm_5_triplet_pt4(j, i)
term(1050) = term(1050) + wm_interm_34_triplet_pt4(i, j) * wm_interm_5_triplet_pt4(j, i)
term(1051) = term(1051) + wm_interm_35_triplet_pt4(i, j) * wm_interm_5_triplet_pt4(j, i)
term(1052) = term(1052) + wm_interm_41_triplet_pt4(i, j) * wm_interm_5_triplet_pt4(j, i)
term(1053) = term(1053) + wm_interm_50_triplet_pt4(i, j) * wm_interm_59_triplet_pt4(i, j)
end do 
end do 

term(1034) = term(1034) * (-6.0d+0) 
term(1035) = term(1035) * (-12.0d+0) 
term(1036) = term(1036) * 6.0d+0 
term(1037) = term(1037) * (-6.0d+0) 
term(1038) = term(1038) * 12.0d+0 
term(1039) = term(1039) * (-12.0d+0) 
term(1040) = term(1040) * 6.0d+0 
term(1041) = term(1041) * (-12.0d+0) 
term(1042) = term(1042) * (-6.0d+0) 
term(1043) = term(1043) * 12.0d+0 
term(1044) = term(1044) * (-4.0d+0) 
term(1045) = term(1045) * 8.0d+0 
term(1046) = term(1046) * 8.0d+0 
term(1047) = term(1047) * (-16.0d+0) 
term(1048) = term(1048) * (-4.0d+0) 
term(1049) = term(1049) * 8.0d+0 
term(1050) = term(1050) * 8.0d+0 
term(1051) = term(1051) * (-16.0d+0) 
term(1052) = term(1052) * (-4.0d+0) 
term(1053) = term(1053) * (-4.0d+0) 

do l = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(1054) = term(1054) + s2(a,b,i,j) * wm_interm_14_triplet_pt4(a, k, i, l) * wm_interm_51_triplet_pt4(b, k, l, j)
term(1055) = term(1055) + s2(a,b,i,j) * wm_interm_51_triplet_pt4(b, k, l, j) * wm_interm_79_triplet_pt4(a, k, i, l)
term(1056) = term(1056) + s2(a,b,i,j) * wm_interm_51_triplet_pt4(b, k, i, l) * wm_interm_79_triplet_pt4(a, k, j, l)
term(1057) = term(1057) + s2(a,b,i,j) * wm_interm_51_triplet_pt4(b, k, j, l) * wm_interm_79_triplet_pt4(a, k, i, l)
term(1058) = term(1058) + s2(a,b,i,j) * wm_interm_14_triplet_pt4(a, k, i, l) * wm_interm_51_triplet_pt4(b, k, j, l)
term(1059) = term(1059) + s2(a,b,i,j) * wm_interm_14_triplet_pt4(a, k, j, l) * wm_interm_51_triplet_pt4(b, k, i, l)
term(1060) = term(1060) + t2(a,b,i,j) * wm_interm_24_triplet_pt4(b, k, i, l) * wm_interm_45_triplet_pt4(a, k, l, j)
term(1061) = term(1061) + t2(a,b,i,j) * wm_interm_24_triplet_pt4(b, k, i, l) * wm_interm_45_triplet_pt4(a, k, j, l)
term(1062) = term(1062) + t2(a,b,i,j) * wm_interm_24_triplet_pt4(b, k, j, l) * wm_interm_45_triplet_pt4(a, k, i, l)
term(1063) = term(1063) + t2(a,b,i,j) * wm_interm_27_triplet_pt4(b, k, i, l) * wm_interm_45_triplet_pt4(a, k, j, l)
term(1064) = term(1064) + t2(a,b,i,j) * wm_interm_27_triplet_pt4(b, k, i, l) * wm_interm_45_triplet_pt4(a, k, l, j)
term(1065) = term(1065) + t2(a,b,i,j) * wm_interm_27_triplet_pt4(b, k, j, l) * wm_interm_45_triplet_pt4(a, k, i, l)
end do 
end do 
end do 
end do 
end do 
end do 

term(1054) = term(1054) * 16.0d+0 
term(1055) = term(1055) * (-32.0d+0) 
term(1056) = term(1056) * (-8.0d+0) 
term(1057) = term(1057) * 16.0d+0 
term(1058) = term(1058) * (-8.0d+0) 
term(1059) = term(1059) * 16.0d+0 
term(1060) = term(1060) * 8.0d+0 
term(1061) = term(1061) * (-16.0d+0) 
term(1062) = term(1062) * 8.0d+0 
term(1063) = term(1063) * 8.0d+0 
term(1064) = term(1064) * (-16.0d+0) 
term(1065) = term(1065) * (-16.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(1066) = term(1066) + s1(a,q) * wm_interm_53_triplet_pt4(i, j) * wm_interm_64_triplet_pt4(a, j, p, i)
term(1067) = term(1067) + s1(a,q) * wm_interm_54_triplet_pt4(i, j) * wm_interm_64_triplet_pt4(a, j, p, i)
term(1068) = term(1068) + s1(a,q) * wm_interm_53_triplet_pt4(i, j) * wm_interm_71_triplet_pt4(a, j, p, i)
term(1069) = term(1069) + s1(a,q) * wm_interm_53_triplet_pt4(i, j) * wm_interm_71_triplet_pt4(a, j, i, p)
term(1070) = term(1070) + s1(a,q) * wm_interm_54_triplet_pt4(i, j) * wm_interm_71_triplet_pt4(a, j, p, i)
term(1071) = term(1071) + s1(a,q) * wm_interm_54_triplet_pt4(i, j) * wm_interm_71_triplet_pt4(a, j, i, p)
term(1072) = term(1072) + r1(vrdav_Rl, a,i) * wm_interm_1_triplet_pt4(a, j) * wm_interm_49_triplet_pt4(j, p, i, q)
term(1073) = term(1073) + r1(vrdav_Rl, a,i) * wm_interm_1_triplet_pt4(a, j) * wm_interm_49_triplet_pt4(p, j, i, q)
term(1074) = term(1074) + r1(vrdav_Rl, a,i) * wm_interm_2_triplet_pt4(a, j) * wm_interm_49_triplet_pt4(j, p, i, q)
term(1075) = term(1075) + r1(vrdav_Rl, a,i) * wm_interm_3_triplet_pt4(a, j) * wm_interm_49_triplet_pt4(j, p, i, q)
term(1076) = term(1076) + r1(vrdav_Rl, a,i) * wm_interm_2_triplet_pt4(a, j) * wm_interm_49_triplet_pt4(p, j, i, q)
term(1077) = term(1077) + r1(vrdav_Rl, a,i) * wm_interm_3_triplet_pt4(a, j) * wm_interm_49_triplet_pt4(p, j, i, q)
term(1078) = term(1078) + s1(a,i) * wm_interm_49_triplet_pt4(p, j, q, i) * wm_interm_82_triplet_pt4(a, j)
term(1079) = term(1079) + s1(a,i) * wm_interm_49_triplet_pt4(p, j, i, q) * wm_interm_82_triplet_pt4(a, j)
term(1080) = term(1080) + s1(a,i) * wm_interm_49_triplet_pt4(j, p, i, q) * wm_interm_82_triplet_pt4(a, j)
term(1081) = term(1081) + s1(a,i) * wm_interm_49_triplet_pt4(j, p, q, i) * wm_interm_82_triplet_pt4(a, j)
term(1082) = term(1082) + s1(a,i) * wm_interm_49_triplet_pt4(p, j, q, i) * wm_interm_84_triplet_pt4(a, j)
term(1083) = term(1083) + s1(a,i) * wm_interm_49_triplet_pt4(p, j, q, i) * wm_interm_83_triplet_pt4(a, j)
term(1084) = term(1084) + s1(a,i) * wm_interm_49_triplet_pt4(p, j, i, q) * wm_interm_84_triplet_pt4(a, j)
term(1085) = term(1085) + s1(a,i) * wm_interm_49_triplet_pt4(p, j, i, q) * wm_interm_83_triplet_pt4(a, j)
term(1086) = term(1086) + s1(a,i) * wm_interm_49_triplet_pt4(j, p, i, q) * wm_interm_84_triplet_pt4(a, j)
term(1087) = term(1087) + s1(a,i) * wm_interm_49_triplet_pt4(j, p, i, q) * wm_interm_83_triplet_pt4(a, j)
term(1088) = term(1088) + s1(a,i) * wm_interm_49_triplet_pt4(j, p, q, i) * wm_interm_84_triplet_pt4(a, j)
term(1089) = term(1089) + s1(a,i) * wm_interm_49_triplet_pt4(j, p, q, i) * wm_interm_83_triplet_pt4(a, j)
term(1090) = term(1090) + t1(a,q) * wm_interm_53_triplet_pt4(i, j) * wm_interm_85_triplet_pt4(a, j, p, i)
term(1091) = term(1091) + t1(a,q) * wm_interm_54_triplet_pt4(i, j) * wm_interm_85_triplet_pt4(a, j, p, i)
term(1092) = term(1092) + t1(a,q) * wm_interm_53_triplet_pt4(i, j) * wm_interm_94_triplet_pt4(a, p, j, i)
term(1093) = term(1093) + t1(a,q) * wm_interm_54_triplet_pt4(i, j) * wm_interm_94_triplet_pt4(a, p, j, i)
term(1094) = term(1094) + t1(a,q) * wm_interm_53_triplet_pt4(i, j) * wm_interm_92_triplet_pt4(a, p, j, i)
term(1095) = term(1095) + t1(a,q) * wm_interm_54_triplet_pt4(i, j) * wm_interm_92_triplet_pt4(a, p, j, i)
term(1096) = term(1096) + t1(a,q) * wm_interm_53_triplet_pt4(i, j) * wm_interm_94_triplet_pt4(a, j, p, i)
term(1097) = term(1097) + t1(a,q) * wm_interm_53_triplet_pt4(i, j) * wm_interm_92_triplet_pt4(a, j, p, i)
term(1098) = term(1098) + t1(a,q) * wm_interm_54_triplet_pt4(i, j) * wm_interm_94_triplet_pt4(a, j, p, i)
term(1099) = term(1099) + t1(a,q) * wm_interm_54_triplet_pt4(i, j) * wm_interm_92_triplet_pt4(a, j, p, i)
end do 
end do 
end do 

term(1066) = term(1066) * (-6.0d+0) 
term(1067) = term(1067) * 12.0d+0 
term(1068) = term(1068) * (-4.0d+0) 
term(1069) = term(1069) * 8.0d+0 
term(1070) = term(1070) * 8.0d+0 
term(1071) = term(1071) * (-16.0d+0) 
term(1072) = term(1072) * 12.0d+0 
term(1073) = term(1073) * (-6.0d+0) 
term(1074) = term(1074) * 16.0d+0 
term(1075) = term(1075) * (-8.0d+0) 
term(1076) = term(1076) * (-8.0d+0) 
term(1077) = term(1077) * 4.0d+0 
term(1078) = term(1078) * 12.0d+0 
term(1079) = term(1079) * (-6.0d+0) 
term(1080) = term(1080) * 12.0d+0 
term(1081) = term(1081) * (-6.0d+0) 
term(1082) = term(1082) * (-8.0d+0) 
term(1083) = term(1083) * 16.0d+0 
term(1084) = term(1084) * 4.0d+0 
term(1085) = term(1085) * (-8.0d+0) 
term(1086) = term(1086) * (-8.0d+0) 
term(1087) = term(1087) * 16.0d+0 
term(1088) = term(1088) * 4.0d+0 
term(1089) = term(1089) * (-8.0d+0) 
term(1090) = term(1090) * 6.0d+0 
term(1091) = term(1091) * (-12.0d+0) 
term(1092) = term(1092) * 4.0d+0 
term(1093) = term(1093) * (-8.0d+0) 
term(1094) = term(1094) * (-2.0d+0) 
term(1095) = term(1095) * 4.0d+0 
term(1096) = term(1096) * (-2.0d+0) 
term(1097) = term(1097) * 4.0d+0 
term(1098) = term(1098) * 4.0d+0 
term(1099) = term(1099) * (-8.0d+0) 

do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(1100) = term(1100) + wm_interm_10_triplet_pt4(a, b) * wm_interm_9_triplet_pt4(b, a)
term(1101) = term(1101) + wm_interm_10_triplet_pt4(a, b) * wm_interm_15_triplet_pt4(b, a)
term(1102) = term(1102) + wm_interm_10_triplet_pt4(a, b) * wm_interm_16_triplet_pt4(b, a)
term(1103) = term(1103) + wm_interm_10_triplet_pt4(a, b) * wm_interm_25_triplet_pt4(b, a)
term(1104) = term(1104) + wm_interm_10_triplet_pt4(a, b) * wm_interm_28_triplet_pt4(b, a)
term(1105) = term(1105) + wm_interm_10_triplet_pt4(a, b) * wm_interm_32_triplet_pt4(b, a)
term(1106) = term(1106) + wm_interm_10_triplet_pt4(a, b) * wm_interm_33_triplet_pt4(b, a)
term(1107) = term(1107) + wm_interm_10_triplet_pt4(a, b) * wm_interm_36_triplet_pt4(b, a)
term(1108) = term(1108) + wm_interm_10_triplet_pt4(a, b) * wm_interm_37_triplet_pt4(b, a)
term(1109) = term(1109) + r1(vrdav_Rl, b,q) * wm_interm_2_triplet_pt4(a, p) * wm_interm_56_triplet_pt4(a, b)
term(1110) = term(1110) + r1(vrdav_Rl, b,q) * wm_interm_2_triplet_pt4(a, p) * wm_interm_58_triplet_pt4(a, b)
term(1111) = term(1111) + r1(vrdav_Rl, b,q) * wm_interm_3_triplet_pt4(a, p) * wm_interm_56_triplet_pt4(a, b)
term(1112) = term(1112) + r1(vrdav_Rl, b,q) * wm_interm_3_triplet_pt4(a, p) * wm_interm_58_triplet_pt4(a, b)
term(1113) = term(1113) + r1(vrdav_Rl, a,p) * wm_interm_42_triplet_pt4(b, q) * wm_interm_65_triplet_pt4(a, b)
term(1114) = term(1114) + r1(vrdav_Rl, a,p) * wm_interm_42_triplet_pt4(b, q) * wm_interm_70_triplet_pt4(a, b)
term(1115) = term(1115) + r1(vrdav_Rl, a,p) * wm_interm_48_triplet_pt4(b, q) * wm_interm_65_triplet_pt4(a, b)
term(1116) = term(1116) + r1(vrdav_Rl, a,p) * wm_interm_48_triplet_pt4(b, q) * wm_interm_70_triplet_pt4(a, b)
term(1117) = term(1117) + r1(vrdav_Rl, a,p) * wm_interm_42_triplet_pt4(b, q) * wm_interm_72_triplet_pt4(a, b)
term(1118) = term(1118) + r1(vrdav_Rl, a,p) * wm_interm_42_triplet_pt4(b, q) * wm_interm_73_triplet_pt4(a, b)
term(1119) = term(1119) + s1(a,p) * wm_interm_52_triplet_pt4(b, q) * wm_interm_80_triplet_pt4(a, b)
term(1120) = term(1120) + s1(a,p) * wm_interm_52_triplet_pt4(b, q) * wm_interm_81_triplet_pt4(a, b)
term(1121) = term(1121) + s1(a,p) * wm_interm_52_triplet_pt4(b, q) * wm_interm_72_triplet_pt4(a, b)
term(1122) = term(1122) + s1(a,p) * wm_interm_52_triplet_pt4(b, q) * wm_interm_73_triplet_pt4(a, b)
term(1123) = term(1123) + s1(a,p) * wm_interm_47_triplet_pt4(b, q) * wm_interm_80_triplet_pt4(a, b)
term(1124) = term(1124) + s1(a,p) * wm_interm_47_triplet_pt4(b, q) * wm_interm_81_triplet_pt4(a, b)
term(1125) = term(1125) + s1(a,p) * wm_interm_47_triplet_pt4(b, q) * wm_interm_72_triplet_pt4(a, b)
term(1126) = term(1126) + s1(a,p) * wm_interm_47_triplet_pt4(b, q) * wm_interm_73_triplet_pt4(a, b)
term(1127) = term(1127) + r1(vrdav_Rl, a,p) * wm_interm_48_triplet_pt4(b, q) * wm_interm_72_triplet_pt4(a, b)
term(1128) = term(1128) + r1(vrdav_Rl, a,p) * wm_interm_48_triplet_pt4(b, q) * wm_interm_73_triplet_pt4(a, b)
term(1129) = term(1129) + r1(vrdav_Rr, a,p) * wm_interm_20_triplet_pt4(b, q) * wm_interm_56_triplet_pt4(a, b)
term(1130) = term(1130) + r1(vrdav_Rr, a,p) * wm_interm_20_triplet_pt4(b, q) * wm_interm_58_triplet_pt4(a, b)
term(1131) = term(1131) + r1(vrdav_Rr, a,p) * wm_interm_21_triplet_pt4(b, q) * wm_interm_56_triplet_pt4(a, b)
term(1132) = term(1132) + r1(vrdav_Rr, a,p) * wm_interm_21_triplet_pt4(b, q) * wm_interm_58_triplet_pt4(a, b)
end do 
end do 

term(1100) = term(1100) * (-12.0d+0) 
term(1101) = term(1101) * 12.0d+0 
term(1102) = term(1102) * (-12.0d+0) 
term(1103) = term(1103) * (-12.0d+0) 
term(1104) = term(1104) * 12.0d+0 
term(1105) = term(1105) * 8.0d+0 
term(1106) = term(1106) * (-16.0d+0) 
term(1107) = term(1107) * 8.0d+0 
term(1108) = term(1108) * (-16.0d+0) 
term(1109) = term(1109) * 16.0d+0 
term(1110) = term(1110) * (-8.0d+0) 
term(1111) = term(1111) * (-8.0d+0) 
term(1112) = term(1112) * 4.0d+0 
term(1113) = term(1113) * (-6.0d+0) 
term(1114) = term(1114) * 6.0d+0 
term(1115) = term(1115) * 12.0d+0 
term(1116) = term(1116) * (-12.0d+0) 
term(1117) = term(1117) * 8.0d+0 
term(1118) = term(1118) * (-16.0d+0) 
term(1119) = term(1119) * (-8.0d+0) 
term(1120) = term(1120) * 16.0d+0 
term(1121) = term(1121) * (-8.0d+0) 
term(1122) = term(1122) * 16.0d+0 
term(1123) = term(1123) * 4.0d+0 
term(1124) = term(1124) * (-8.0d+0) 
term(1125) = term(1125) * 4.0d+0 
term(1126) = term(1126) * (-8.0d+0) 
term(1127) = term(1127) * (-16.0d+0) 
term(1128) = term(1128) * 32.0d+0 
term(1129) = term(1129) * (-32.0d+0) 
term(1130) = term(1130) * 16.0d+0 
term(1131) = term(1131) * 16.0d+0 
term(1132) = term(1132) * (-8.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(1133) = term(1133) + s1(a,q) * wm_interm_51_triplet_pt4(a, i, j, k) * wm_interm_66_triplet_pt4(i, p, k, j)
term(1134) = term(1134) + s1(a,q) * wm_interm_51_triplet_pt4(a, i, j, k) * wm_interm_66_triplet_pt4(i, p, j, k)
term(1135) = term(1135) + s1(a,i) * wm_interm_51_triplet_pt4(a, q, j, k) * wm_interm_66_triplet_pt4(p, i, k, j)
term(1136) = term(1136) + s1(a,i) * wm_interm_51_triplet_pt4(a, q, j, k) * wm_interm_66_triplet_pt4(p, i, j, k)
term(1137) = term(1137) + s1(a,q) * wm_interm_51_triplet_pt4(a, i, j, k) * wm_interm_74_triplet_pt4(i, p, k, j)
term(1138) = term(1138) + s1(a,q) * wm_interm_51_triplet_pt4(a, i, j, k) * wm_interm_74_triplet_pt4(p, i, k, j)
term(1139) = term(1139) + s1(a,q) * wm_interm_51_triplet_pt4(a, i, j, k) * wm_interm_74_triplet_pt4(p, i, j, k)
term(1140) = term(1140) + s1(a,q) * wm_interm_51_triplet_pt4(a, i, j, k) * wm_interm_74_triplet_pt4(i, p, j, k)
term(1141) = term(1141) + s1(a,i) * wm_interm_51_triplet_pt4(a, q, j, k) * wm_interm_74_triplet_pt4(p, i, k, j)
term(1142) = term(1142) + s1(a,i) * wm_interm_51_triplet_pt4(a, q, j, k) * wm_interm_74_triplet_pt4(i, p, k, j)
term(1143) = term(1143) + s1(a,i) * wm_interm_51_triplet_pt4(a, q, j, k) * wm_interm_74_triplet_pt4(i, p, j, k)
term(1144) = term(1144) + s1(a,i) * wm_interm_51_triplet_pt4(a, q, j, k) * wm_interm_74_triplet_pt4(p, i, j, k)
term(1145) = term(1145) + s1(a,i) * wm_interm_51_triplet_pt4(a, j, k, q) * wm_interm_66_triplet_pt4(j, i, k, p)
term(1146) = term(1146) + s1(a,i) * wm_interm_51_triplet_pt4(a, j, k, q) * wm_interm_66_triplet_pt4(j, i, p, k)
term(1147) = term(1147) + s1(a,i) * wm_interm_51_triplet_pt4(a, j, q, k) * wm_interm_66_triplet_pt4(j, i, k, p)
term(1148) = term(1148) + s1(a,i) * wm_interm_51_triplet_pt4(a, j, q, k) * wm_interm_66_triplet_pt4(j, i, p, k)
term(1149) = term(1149) + s1(a,i) * wm_interm_49_triplet_pt4(j, k, i, q) * wm_interm_64_triplet_pt4(a, p, j, k)
term(1150) = term(1150) + s1(a,i) * wm_interm_51_triplet_pt4(a, j, k, q) * wm_interm_74_triplet_pt4(i, j, k, p)
term(1151) = term(1151) + s1(a,i) * wm_interm_51_triplet_pt4(a, j, k, q) * wm_interm_74_triplet_pt4(j, i, k, p)
term(1152) = term(1152) + s1(a,i) * wm_interm_51_triplet_pt4(a, j, k, q) * wm_interm_74_triplet_pt4(j, i, p, k)
term(1153) = term(1153) + s1(a,i) * wm_interm_51_triplet_pt4(a, j, k, q) * wm_interm_74_triplet_pt4(i, j, p, k)
term(1154) = term(1154) + s1(a,i) * wm_interm_51_triplet_pt4(a, j, q, k) * wm_interm_74_triplet_pt4(j, i, k, p)
term(1155) = term(1155) + s1(a,i) * wm_interm_51_triplet_pt4(a, j, q, k) * wm_interm_74_triplet_pt4(i, j, k, p)
term(1156) = term(1156) + s1(a,i) * wm_interm_51_triplet_pt4(a, j, q, k) * wm_interm_74_triplet_pt4(i, j, p, k)
term(1157) = term(1157) + s1(a,i) * wm_interm_51_triplet_pt4(a, j, q, k) * wm_interm_74_triplet_pt4(j, i, p, k)
term(1158) = term(1158) + s1(a,i) * wm_interm_49_triplet_pt4(j, k, i, q) * wm_interm_71_triplet_pt4(a, p, j, k)
term(1159) = term(1159) + t1(a,i) * wm_interm_45_triplet_pt4(a, p, j, k) * wm_interm_88_triplet_pt4(i, q, j, k)
term(1160) = term(1160) + t1(a,i) * wm_interm_45_triplet_pt4(a, p, j, k) * wm_interm_88_triplet_pt4(i, q, k, j)
term(1161) = term(1161) + t1(a,i) * wm_interm_49_triplet_pt4(i, q, j, k) * wm_interm_85_triplet_pt4(a, j, k, p)
term(1162) = term(1162) + t1(a,i) * wm_interm_49_triplet_pt4(q, i, j, k) * wm_interm_85_triplet_pt4(a, j, k, p)
term(1163) = term(1163) + t1(a,q) * wm_interm_45_triplet_pt4(a, i, j, k) * wm_interm_88_triplet_pt4(i, p, j, k)
term(1164) = term(1164) + t1(a,q) * wm_interm_45_triplet_pt4(a, i, j, k) * wm_interm_88_triplet_pt4(i, p, k, j)
term(1165) = term(1165) + t1(a,i) * wm_interm_45_triplet_pt4(a, j, p, k) * wm_interm_88_triplet_pt4(j, i, q, k)
term(1166) = term(1166) + t1(a,i) * wm_interm_45_triplet_pt4(a, j, k, p) * wm_interm_88_triplet_pt4(j, i, q, k)
term(1167) = term(1167) + t1(a,i) * wm_interm_45_triplet_pt4(a, j, p, k) * wm_interm_88_triplet_pt4(j, i, k, q)
term(1168) = term(1168) + t1(a,i) * wm_interm_45_triplet_pt4(a, j, k, p) * wm_interm_88_triplet_pt4(j, i, k, q)
term(1169) = term(1169) + t1(a,i) * wm_interm_45_triplet_pt4(a, p, j, k) * wm_interm_97_triplet_pt4(q, i, j, k)
term(1170) = term(1170) + t1(a,i) * wm_interm_45_triplet_pt4(a, p, j, k) * wm_interm_97_triplet_pt4(q, i, k, j)
term(1171) = term(1171) + t1(a,i) * wm_interm_49_triplet_pt4(i, q, j, k) * wm_interm_92_triplet_pt4(a, j, k, p)
term(1172) = term(1172) + t1(a,i) * wm_interm_49_triplet_pt4(q, i, j, k) * wm_interm_92_triplet_pt4(a, j, k, p)
term(1173) = term(1173) + t1(a,i) * wm_interm_45_triplet_pt4(a, p, j, k) * wm_interm_97_triplet_pt4(i, q, k, j)
term(1174) = term(1174) + t1(a,i) * wm_interm_45_triplet_pt4(a, p, j, k) * wm_interm_97_triplet_pt4(i, q, j, k)
term(1175) = term(1175) + t1(a,i) * wm_interm_49_triplet_pt4(i, q, j, k) * wm_interm_94_triplet_pt4(a, j, k, p)
term(1176) = term(1176) + t1(a,i) * wm_interm_49_triplet_pt4(q, i, j, k) * wm_interm_94_triplet_pt4(a, j, k, p)
term(1177) = term(1177) + t1(a,q) * wm_interm_45_triplet_pt4(a, i, j, k) * wm_interm_97_triplet_pt4(p, i, j, k)
term(1178) = term(1178) + t1(a,q) * wm_interm_45_triplet_pt4(a, i, j, k) * wm_interm_97_triplet_pt4(i, p, j, k)
term(1179) = term(1179) + t1(a,q) * wm_interm_45_triplet_pt4(a, i, j, k) * wm_interm_97_triplet_pt4(i, p, k, j)
term(1180) = term(1180) + t1(a,q) * wm_interm_45_triplet_pt4(a, i, j, k) * wm_interm_97_triplet_pt4(p, i, k, j)
term(1181) = term(1181) + t1(a,i) * wm_interm_45_triplet_pt4(a, j, p, k) * wm_interm_97_triplet_pt4(j, i, q, k)
term(1182) = term(1182) + t1(a,i) * wm_interm_45_triplet_pt4(a, j, k, p) * wm_interm_97_triplet_pt4(j, i, q, k)
term(1183) = term(1183) + t1(a,i) * wm_interm_45_triplet_pt4(a, j, p, k) * wm_interm_97_triplet_pt4(j, i, k, q)
term(1184) = term(1184) + t1(a,i) * wm_interm_45_triplet_pt4(a, j, k, p) * wm_interm_97_triplet_pt4(j, i, k, q)
term(1185) = term(1185) + t1(a,i) * wm_interm_45_triplet_pt4(a, j, p, k) * wm_interm_97_triplet_pt4(i, j, k, q)
term(1186) = term(1186) + t1(a,i) * wm_interm_45_triplet_pt4(a, j, p, k) * wm_interm_97_triplet_pt4(i, j, q, k)
term(1187) = term(1187) + t1(a,i) * wm_interm_45_triplet_pt4(a, j, k, p) * wm_interm_97_triplet_pt4(i, j, q, k)
term(1188) = term(1188) + t1(a,i) * wm_interm_45_triplet_pt4(a, j, k, p) * wm_interm_97_triplet_pt4(i, j, k, q)
end do 
end do 
end do 
end do 

term(1133) = term(1133) * 6.0d+0 
term(1134) = term(1134) * (-6.0d+0) 
term(1135) = term(1135) * 6.0d+0 
term(1136) = term(1136) * (-6.0d+0) 
term(1137) = term(1137) * 4.0d+0 
term(1138) = term(1138) * (-8.0d+0) 
term(1139) = term(1139) * 4.0d+0 
term(1140) = term(1140) * (-8.0d+0) 
term(1141) = term(1141) * 4.0d+0 
term(1142) = term(1142) * (-8.0d+0) 
term(1143) = term(1143) * 4.0d+0 
term(1144) = term(1144) * (-8.0d+0) 
term(1145) = term(1145) * (-6.0d+0) 
term(1146) = term(1146) * 6.0d+0 
term(1147) = term(1147) * 6.0d+0 
term(1148) = term(1148) * (-6.0d+0) 
term(1149) = term(1149) * 6.0d+0 
term(1150) = term(1150) * 4.0d+0 
term(1151) = term(1151) * (-8.0d+0) 
term(1152) = term(1152) * 4.0d+0 
term(1153) = term(1153) * (-8.0d+0) 
term(1154) = term(1154) * 4.0d+0 
term(1155) = term(1155) * (-8.0d+0) 
term(1156) = term(1156) * 4.0d+0 
term(1157) = term(1157) * (-8.0d+0) 
term(1158) = term(1158) * 4.0d+0 
term(1159) = term(1159) * (-6.0d+0) 
term(1160) = term(1160) * 6.0d+0 
term(1161) = term(1161) * (-6.0d+0) 
term(1162) = term(1162) * 6.0d+0 
term(1163) = term(1163) * 3.0d+0 
term(1164) = term(1164) * (-3.0d+0) 
term(1165) = term(1165) * 3.0d+0 
term(1166) = term(1166) * (-3.0d+0) 
term(1167) = term(1167) * (-3.0d+0) 
term(1168) = term(1168) * 3.0d+0 
term(1169) = term(1169) * 8.0d+0 
term(1170) = term(1170) * (-4.0d+0) 
term(1171) = term(1171) * (-4.0d+0) 
term(1172) = term(1172) * 8.0d+0 
term(1173) = term(1173) * 8.0d+0 
term(1174) = term(1174) * (-4.0d+0) 
term(1175) = term(1175) * 8.0d+0 
term(1176) = term(1176) * (-4.0d+0) 
term(1177) = term(1177) * (-2.0d+0) 
term(1178) = term(1178) * 4.0d+0 
term(1179) = term(1179) * (-2.0d+0) 
term(1180) = term(1180) * 4.0d+0 
term(1181) = term(1181) * 4.0d+0 
term(1182) = term(1182) * (-2.0d+0) 
term(1183) = term(1183) * (-2.0d+0) 
term(1184) = term(1184) * 4.0d+0 
term(1185) = term(1185) * 4.0d+0 
term(1186) = term(1186) * (-2.0d+0) 
term(1187) = term(1187) * 4.0d+0 
term(1188) = term(1188) * (-2.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(1189) = term(1189) + r1(vrdav_Rl, a,i) * wm_interm_56_triplet_pt4(b, a) * wm_interm_79_triplet_pt4(b, p, i, q)
term(1190) = term(1190) + r1(vrdav_Rl, a,i) * wm_interm_58_triplet_pt4(b, a) * wm_interm_79_triplet_pt4(b, p, i, q)
term(1191) = term(1191) + r1(vrdav_Rl, a,i) * wm_interm_14_triplet_pt4(b, p, i, q) * wm_interm_56_triplet_pt4(b, a)
term(1192) = term(1192) + r1(vrdav_Rl, a,i) * wm_interm_14_triplet_pt4(b, p, i, q) * wm_interm_58_triplet_pt4(b, a)
end do 
end do 
end do 

term(1189) = term(1189) * (-8.0d+0) 
term(1190) = term(1190) * 4.0d+0 
term(1191) = term(1191) * 4.0d+0 
term(1192) = term(1192) * (-2.0d+0) 

do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(1193) = term(1193) + s1(a,p) * wm_interm_49_triplet_pt4(i, j, k, q) * wm_interm_64_triplet_pt4(a, k, i, j)
term(1194) = term(1194) + s1(a,p) * wm_interm_49_triplet_pt4(i, j, k, q) * wm_interm_71_triplet_pt4(a, k, i, j)
term(1195) = term(1195) + s1(a,i) * wm_interm_49_triplet_pt4(j, k, i, q) * wm_interm_71_triplet_pt4(a, p, k, j)
term(1196) = term(1196) + t1(a,i) * wm_interm_49_triplet_pt4(j, i, k, q) * wm_interm_85_triplet_pt4(a, k, p, j)
term(1197) = term(1197) + t1(a,i) * wm_interm_49_triplet_pt4(i, j, k, q) * wm_interm_85_triplet_pt4(a, k, p, j)
term(1198) = term(1198) + t1(a,i) * wm_interm_49_triplet_pt4(j, i, k, q) * wm_interm_92_triplet_pt4(a, p, k, j)
term(1199) = term(1199) + t1(a,i) * wm_interm_49_triplet_pt4(i, j, k, q) * wm_interm_92_triplet_pt4(a, p, k, j)
term(1200) = term(1200) + t1(a,i) * wm_interm_49_triplet_pt4(i, j, k, q) * wm_interm_92_triplet_pt4(a, k, p, j)
term(1201) = term(1201) + t1(a,i) * wm_interm_49_triplet_pt4(j, i, k, q) * wm_interm_92_triplet_pt4(a, k, p, j)
term(1202) = term(1202) + t1(a,i) * wm_interm_49_triplet_pt4(i, j, k, q) * wm_interm_94_triplet_pt4(a, p, k, j)
term(1203) = term(1203) + t1(a,i) * wm_interm_49_triplet_pt4(i, j, k, q) * wm_interm_94_triplet_pt4(a, k, p, j)
term(1204) = term(1204) + t1(a,i) * wm_interm_49_triplet_pt4(j, i, k, q) * wm_interm_94_triplet_pt4(a, k, p, j)
term(1205) = term(1205) + t1(a,i) * wm_interm_49_triplet_pt4(j, i, k, q) * wm_interm_94_triplet_pt4(a, p, k, j)
end do 
end do 
end do 
end do 

term(1193) = term(1193) * (-6.0d+0) 
term(1194) = term(1194) * (-16.0d+0) 
term(1195) = term(1195) * (-8.0d+0) 
term(1196) = term(1196) * 6.0d+0 
term(1197) = term(1197) * (-6.0d+0) 
term(1198) = term(1198) * (-2.0d+0) 
term(1199) = term(1199) * 4.0d+0 
term(1200) = term(1200) * (-2.0d+0) 
term(1201) = term(1201) * 4.0d+0 
term(1202) = term(1202) * (-2.0d+0) 
term(1203) = term(1203) * 4.0d+0 
term(1204) = term(1204) * (-2.0d+0) 
term(1205) = term(1205) * 4.0d+0 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(1206) = term(1206) + s1(a,i) * wm_interm_49_triplet_pt4(j, k, q, i) * wm_interm_64_triplet_pt4(a, p, j, k)
term(1207) = term(1207) + s1(a,i) * wm_interm_49_triplet_pt4(j, k, q, i) * wm_interm_71_triplet_pt4(a, p, j, k)
term(1208) = term(1208) + t1(a,q) * wm_interm_49_triplet_pt4(i, p, j, k) * wm_interm_85_triplet_pt4(a, j, k, i)
term(1209) = term(1209) + t1(a,q) * wm_interm_49_triplet_pt4(p, i, j, k) * wm_interm_85_triplet_pt4(a, j, k, i)
term(1210) = term(1210) + t1(a,q) * wm_interm_49_triplet_pt4(i, p, j, k) * wm_interm_94_triplet_pt4(a, j, k, i)
term(1211) = term(1211) + t1(a,q) * wm_interm_49_triplet_pt4(p, i, j, k) * wm_interm_94_triplet_pt4(a, j, k, i)
term(1212) = term(1212) + t1(a,q) * wm_interm_49_triplet_pt4(p, i, j, k) * wm_interm_92_triplet_pt4(a, j, k, i)
term(1213) = term(1213) + t1(a,q) * wm_interm_49_triplet_pt4(i, p, j, k) * wm_interm_92_triplet_pt4(a, j, k, i)
end do 
end do 
end do 
end do 

term(1206) = term(1206) * (-6.0d+0) 
term(1207) = term(1207) * (-8.0d+0) 
term(1208) = term(1208) * 3.0d+0 
term(1209) = term(1209) * (-3.0d+0) 
term(1210) = term(1210) * (-2.0d+0) 
term(1211) = term(1211) * 4.0d+0 
term(1212) = term(1212) * (-2.0d+0) 
term(1213) = term(1213) * 4.0d+0 

do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(1214) = term(1214) + s2(a,b,i,j) * wm_interm_14_triplet_pt4(a, k, j, p) * wm_interm_51_triplet_pt4(b, k, i, q)
term(1215) = term(1215) + s2(a,b,i,j) * wm_interm_51_triplet_pt4(b, k, i, q) * wm_interm_79_triplet_pt4(a, k, j, p)
term(1216) = term(1216) + s2(a,b,i,j) * wm_interm_14_triplet_pt4(a, k, i, p) * wm_interm_51_triplet_pt4(b, k, j, q)
term(1217) = term(1217) + s2(a,b,i,j) * wm_interm_51_triplet_pt4(b, k, j, q) * wm_interm_79_triplet_pt4(a, k, i, p)
term(1218) = term(1218) + s2(a,b,i,j) * wm_interm_14_triplet_pt4(a, k, i, p) * wm_interm_51_triplet_pt4(b, k, q, j)
term(1219) = term(1219) + s2(a,b,i,j) * wm_interm_51_triplet_pt4(b, k, q, j) * wm_interm_79_triplet_pt4(a, k, i, p)
term(1220) = term(1220) + t2(a,b,i,j) * wm_interm_27_triplet_pt4(b, k, i, q) * wm_interm_45_triplet_pt4(a, k, j, p)
term(1221) = term(1221) + t2(a,b,i,j) * wm_interm_27_triplet_pt4(b, k, i, q) * wm_interm_45_triplet_pt4(a, k, p, j)
term(1222) = term(1222) + t2(a,b,i,j) * wm_interm_27_triplet_pt4(b, k, j, q) * wm_interm_45_triplet_pt4(a, k, i, p)
term(1223) = term(1223) + t2(a,b,i,j) * wm_interm_24_triplet_pt4(b, k, i, q) * wm_interm_45_triplet_pt4(a, k, j, p)
term(1224) = term(1224) + t2(a,b,i,j) * wm_interm_24_triplet_pt4(b, k, i, q) * wm_interm_45_triplet_pt4(a, k, p, j)
term(1225) = term(1225) + t2(a,b,i,j) * wm_interm_24_triplet_pt4(b, k, j, q) * wm_interm_45_triplet_pt4(a, k, i, p)
end do 
end do 
end do 
end do 
end do 

term(1214) = term(1214) * (-8.0d+0) 
term(1215) = term(1215) * 4.0d+0 
term(1216) = term(1216) * 4.0d+0 
term(1217) = term(1217) * (-8.0d+0) 
term(1218) = term(1218) * (-8.0d+0) 
term(1219) = term(1219) * 16.0d+0 
term(1220) = term(1220) * (-2.0d+0) 
term(1221) = term(1221) * 4.0d+0 
term(1222) = term(1222) * 4.0d+0 
term(1223) = term(1223) * 4.0d+0 
term(1224) = term(1224) * (-2.0d+0) 
term(1225) = term(1225) * (-2.0d+0) 

do l = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(1226) = term(1226) + t1(a,i) * wm_interm_49_triplet_pt4(i, j, k, l) * wm_interm_85_triplet_pt4(a, k, l, j)
term(1227) = term(1227) + t1(a,i) * wm_interm_49_triplet_pt4(j, i, k, l) * wm_interm_85_triplet_pt4(a, k, l, j)
term(1228) = term(1228) + t1(a,i) * wm_interm_49_triplet_pt4(i, j, k, l) * wm_interm_92_triplet_pt4(a, k, l, j)
term(1229) = term(1229) + t1(a,i) * wm_interm_49_triplet_pt4(j, i, k, l) * wm_interm_92_triplet_pt4(a, k, l, j)
term(1230) = term(1230) + t1(a,i) * wm_interm_49_triplet_pt4(i, j, k, l) * wm_interm_94_triplet_pt4(a, k, l, j)
term(1231) = term(1231) + t1(a,i) * wm_interm_49_triplet_pt4(j, i, k, l) * wm_interm_94_triplet_pt4(a, k, l, j)
end do 
end do 
end do 
end do 
end do 

term(1226) = term(1226) * 12.0d+0 
term(1227) = term(1227) * (-12.0d+0) 
term(1228) = term(1228) * 8.0d+0 
term(1229) = term(1229) * (-16.0d+0) 
term(1230) = term(1230) * (-16.0d+0) 
term(1231) = term(1231) * 8.0d+0 


    calc_D_oo_wm_triplet_pt4 = zero
    do s = 0, 1231
    calc_D_oo_wm_triplet_pt4 = calc_D_oo_wm_triplet_pt4 + term(s)
    end do

    end function calc_D_oo_wm_triplet_pt4
    
    function calc_D_ov_wm_triplet_pt4(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_ov_wm_triplet_pt4
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    double precision, dimension(:), intent(in) :: vrdav_Rl
    double precision, dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , k, i, j, a, b 
    real(F64), dimension(0:48) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + r1(vrdav_Rl, q,i) * wm_interm_43_triplet_pt4(a, j, i, k) * wm_interm_45_triplet_pt4(a, j, p, k)
term(1) = term(1) + r1(vrdav_Rl, q,i) * wm_interm_43_triplet_pt4(a, j, i, k) * wm_interm_45_triplet_pt4(a, j, k, p)
term(2) = term(2) + r1(vrdav_Rl, q,i) * wm_interm_43_triplet_pt4(a, j, k, i) * wm_interm_45_triplet_pt4(a, j, p, k)
term(3) = term(3) + t2(a,q,j,i) * wm_interm_45_triplet_pt4(a, p, i, k) * wm_interm_59_triplet_pt4(j, k)
term(4) = term(4) + t2(a,q,j,i) * wm_interm_45_triplet_pt4(a, p, j, k) * wm_interm_59_triplet_pt4(i, k)
end do 
end do 
end do 
end do 

term(0) = term(0) * 8.0d+0 
term(1) = term(1) * (-4.0d+0) 
term(2) = term(2) * (-4.0d+0) 
term(3) = term(3) * (-4.0d+0) 
term(4) = term(4) * 2.0d+0 

do i = 1, nocc 
term(5) = term(5) + wm_interm_5_triplet_pt4(p, i) * wm_interm_86_triplet_pt4(q, i)
term(6) = term(6) + wm_interm_0_triplet_pt4(q, i) * wm_interm_50_triplet_pt4(i, p)
term(7) = term(7) + wm_interm_5_triplet_pt4(p, i) * wm_interm_96_triplet_pt4(q, i)
term(8) = term(8) + wm_interm_5_triplet_pt4(p, i) * wm_interm_93_triplet_pt4(q, i)
term(9) = term(9) + wm_interm_20_triplet_pt4(q, i) * wm_interm_50_triplet_pt4(i, p)
term(10) = term(10) + wm_interm_21_triplet_pt4(q, i) * wm_interm_50_triplet_pt4(i, p)
end do 

term(5) = term(5) * (-6.0d+0) 
term(6) = term(6) * 6.0d+0 
term(7) = term(7) * 4.0d+0 
term(8) = term(8) * (-8.0d+0) 
term(9) = term(9) * (-8.0d+0) 
term(10) = term(10) * 4.0d+0 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(11) = term(11) + r1(vrdav_Rl, q,i) * wm_interm_43_triplet_pt4(a, j, k, i) * wm_interm_45_triplet_pt4(a, j, k, p)
term(12) = term(12) + t2(a,q,j,i) * wm_interm_45_triplet_pt4(a, p, k, i) * wm_interm_59_triplet_pt4(j, k)
end do 
end do 
end do 
end do 

term(11) = term(11) * 8.0d+0 
term(12) = term(12) * 2.0d+0 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(13) = term(13) + t1(a,i) * wm_interm_45_triplet_pt4(a, p, j, k) * wm_interm_51_triplet_pt4(q, i, k, j)
term(14) = term(14) + t1(a,i) * wm_interm_45_triplet_pt4(a, p, j, k) * wm_interm_51_triplet_pt4(q, i, j, k)
end do 
end do 
end do 
end do 

term(13) = term(13) * (-4.0d+0) 
term(14) = term(14) * 2.0d+0 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(15) = term(15) + r1(vrdav_Rl, q,i) * wm_interm_43_triplet_pt4(a, p, i, j) * wm_interm_46_triplet_pt4(a, j)
term(16) = term(16) + r1(vrdav_Rl, q,i) * wm_interm_43_triplet_pt4(a, p, i, j) * wm_interm_44_triplet_pt4(a, j)
term(17) = term(17) + t1(q,i) * wm_interm_45_triplet_pt4(a, p, i, j) * wm_interm_47_triplet_pt4(a, j)
term(18) = term(18) + t1(q,i) * wm_interm_45_triplet_pt4(a, p, i, j) * wm_interm_52_triplet_pt4(a, j)
end do 
end do 
end do 

term(15) = term(15) * (-16.0d+0) 
term(16) = term(16) * 8.0d+0 
term(17) = term(17) * (-4.0d+0) 
term(18) = term(18) * 8.0d+0 

do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(19) = term(19) + t2(a,q,j,i) * wm_interm_45_triplet_pt4(a, p, k, j) * wm_interm_59_triplet_pt4(i, k)
end do 
end do 
end do 
end do 

term(19) = term(19) * (-4.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(20) = term(20) + r1(vrdav_Rl, q,i) * wm_interm_43_triplet_pt4(a, p, j, i) * wm_interm_44_triplet_pt4(a, j)
term(21) = term(21) + r1(vrdav_Rl, q,i) * wm_interm_43_triplet_pt4(a, p, j, i) * wm_interm_46_triplet_pt4(a, j)
term(22) = term(22) + t1(q,i) * wm_interm_45_triplet_pt4(a, p, j, i) * wm_interm_47_triplet_pt4(a, j)
term(23) = term(23) + t1(q,i) * wm_interm_45_triplet_pt4(a, p, j, i) * wm_interm_52_triplet_pt4(a, j)
end do 
end do 
end do 

term(20) = term(20) * (-4.0d+0) 
term(21) = term(21) * 8.0d+0 
term(22) = term(22) * 2.0d+0 
term(23) = term(23) * (-4.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(24) = term(24) + r1(vrdav_Rl, q,i) * wm_interm_42_triplet_pt4(a, i) * wm_interm_44_triplet_pt4(a, p)
term(25) = term(25) + r1(vrdav_Rl, q,i) * wm_interm_42_triplet_pt4(a, i) * wm_interm_46_triplet_pt4(a, p)
term(26) = term(26) + r1(vrdav_Rl, q,i) * wm_interm_44_triplet_pt4(a, p) * wm_interm_48_triplet_pt4(a, i)
term(27) = term(27) + r1(vrdav_Rl, q,i) * wm_interm_46_triplet_pt4(a, p) * wm_interm_48_triplet_pt4(a, i)
term(28) = term(28) + t1(a,i) * wm_interm_41_triplet_pt4(i, p) * wm_interm_58_triplet_pt4(a, q)
term(29) = term(29) + t1(a,i) * wm_interm_41_triplet_pt4(i, p) * wm_interm_56_triplet_pt4(a, q)
term(30) = term(30) + r1(vrdav_Rl, a,p) * s1(q,i) * wm_interm_1_triplet_pt4(a, i)
term(31) = term(31) + r1(vrdav_Rl, a,p) * s1(q,i) * wm_interm_2_triplet_pt4(a, i)
term(32) = term(32) + r1(vrdav_Rl, a,p) * s1(q,i) * wm_interm_3_triplet_pt4(a, i)
term(33) = term(33) + r1(vrdav_Rr, a,p) * t1(q,i) * wm_interm_20_triplet_pt4(a, i)
term(34) = term(34) + r1(vrdav_Rr, a,p) * t1(q,i) * wm_interm_21_triplet_pt4(a, i)
end do 
end do 

term(24) = term(24) * (-4.0d+0) 
term(25) = term(25) * 8.0d+0 
term(26) = term(26) * 8.0d+0 
term(27) = term(27) * (-16.0d+0) 
term(28) = term(28) * 2.0d+0 
term(29) = term(29) * (-4.0d+0) 
term(30) = term(30) * 6.0d+0 
term(31) = term(31) * 8.0d+0 
term(32) = term(32) * (-4.0d+0) 
term(33) = term(33) * (-8.0d+0) 
term(34) = term(34) * 4.0d+0 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(35) = term(35) + r1(vrdav_Rl, q,i) * wm_interm_49_triplet_pt4(j, p, i, k) * wm_interm_50_triplet_pt4(j, k)
term(36) = term(36) + r1(vrdav_Rl, q,i) * wm_interm_49_triplet_pt4(p, j, i, k) * wm_interm_50_triplet_pt4(j, k)
end do 
end do 
end do 

term(35) = term(35) * (-4.0d+0) 
term(36) = term(36) * 8.0d+0 

do j = 1, nocc 
do i = 1, nocc 
term(37) = term(37) + r1(vrdav_Rl, q,j) * wm_interm_50_triplet_pt4(i, p) * wm_interm_53_triplet_pt4(i, j)
term(38) = term(38) + r1(vrdav_Rl, q,j) * wm_interm_50_triplet_pt4(i, p) * wm_interm_54_triplet_pt4(i, j)
term(39) = term(39) + t1(q,j) * wm_interm_41_triplet_pt4(i, p) * wm_interm_53_triplet_pt4(j, i)
term(40) = term(40) + t1(q,j) * wm_interm_41_triplet_pt4(i, p) * wm_interm_54_triplet_pt4(j, i)
end do 
end do 

term(37) = term(37) * 2.0d+0 
term(38) = term(38) * (-4.0d+0) 
term(39) = term(39) * 2.0d+0 
term(40) = term(40) * (-4.0d+0) 

do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(41) = term(41) + r1(vrdav_Rr, a,p) * wm_interm_56_triplet_pt4(a, b) * wm_interm_61_triplet_pt4(b, q)
term(42) = term(42) + r1(vrdav_Rr, a,p) * wm_interm_58_triplet_pt4(a, b) * wm_interm_61_triplet_pt4(b, q)
end do 
end do 

term(41) = term(41) * (-4.0d+0) 
term(42) = term(42) * 2.0d+0 

do a = nocc + 1, nactive 
term(43) = term(43) + wm_interm_1_triplet_pt4(a, p) * wm_interm_61_triplet_pt4(a, q)
term(44) = term(44) + wm_interm_2_triplet_pt4(a, p) * wm_interm_61_triplet_pt4(a, q)
term(45) = term(45) + wm_interm_3_triplet_pt4(a, p) * wm_interm_61_triplet_pt4(a, q)
term(46) = term(46) + wm_interm_10_triplet_pt4(a, q) * wm_interm_82_triplet_pt4(a, p)
term(47) = term(47) + wm_interm_10_triplet_pt4(a, q) * wm_interm_84_triplet_pt4(a, p)
term(48) = term(48) + wm_interm_10_triplet_pt4(a, q) * wm_interm_83_triplet_pt4(a, p)
end do 

term(43) = term(43) * 6.0d+0 
term(44) = term(44) * 8.0d+0 
term(45) = term(45) * (-4.0d+0) 
term(46) = term(46) * (-6.0d+0) 
term(47) = term(47) * 4.0d+0 
term(48) = term(48) * (-8.0d+0) 


    calc_D_ov_wm_triplet_pt4 = zero
    do s = 0, 48
    calc_D_ov_wm_triplet_pt4 = calc_D_ov_wm_triplet_pt4 + term(s)
    end do

    end function calc_D_ov_wm_triplet_pt4
    
    function calc_D_vo_wm_triplet_pt4(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vo_wm_triplet_pt4
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    double precision, dimension(:), intent(in) :: vrdav_Rl
    double precision, dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , j, i, k, a, b 
    real(F64), dimension(0:219) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + s2(a,p,j,i) * wm_interm_41_triplet_pt4(k, i) * wm_interm_43_triplet_pt4(a, q, k, j)
term(1) = term(1) + s2(a,p,q,i) * wm_interm_41_triplet_pt4(j, k) * wm_interm_43_triplet_pt4(a, k, i, j)
term(2) = term(2) + s2(a,p,i,q) * wm_interm_41_triplet_pt4(j, k) * wm_interm_43_triplet_pt4(a, k, i, j)
term(3) = term(3) + s2(a,p,j,i) * wm_interm_50_triplet_pt4(i, k) * wm_interm_51_triplet_pt4(a, q, k, j)
term(4) = term(4) + t2(a,p,q,i) * wm_interm_41_triplet_pt4(j, k) * wm_interm_62_triplet_pt4(a, i, k, j)
term(5) = term(5) + t2(a,p,q,i) * wm_interm_41_triplet_pt4(j, k) * wm_interm_62_triplet_pt4(a, k, i, j)
term(6) = term(6) + t2(a,p,i,q) * wm_interm_41_triplet_pt4(j, k) * wm_interm_62_triplet_pt4(a, k, i, j)
end do 
end do 
end do 
end do 

term(0) = term(0) * 8.0d+0 
term(1) = term(1) * (-4.0d+0) 
term(2) = term(2) * 8.0d+0 
term(3) = term(3) * 8.0d+0 
term(4) = term(4) * 2.0d+0 
term(5) = term(5) * (-4.0d+0) 
term(6) = term(6) * 8.0d+0 

do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(7) = term(7) + s2(a,p,i,q) * wm_interm_41_triplet_pt4(j, k) * wm_interm_43_triplet_pt4(a, k, j, i)
term(8) = term(8) + s2(a,p,q,i) * wm_interm_41_triplet_pt4(j, k) * wm_interm_43_triplet_pt4(a, k, j, i)
term(9) = term(9) + t2(a,p,j,i) * wm_interm_41_triplet_pt4(i, k) * wm_interm_62_triplet_pt4(a, k, j, q)
term(10) = term(10) + t2(a,p,j,i) * wm_interm_41_triplet_pt4(i, k) * wm_interm_60_triplet_pt4(a, k, j, q)
end do 
end do 
end do 
end do 

term(7) = term(7) * (-16.0d+0) 
term(8) = term(8) * 8.0d+0 
term(9) = term(9) * (-4.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(11) = term(11) + s2(a,p,j,i) * wm_interm_41_triplet_pt4(k, j) * wm_interm_43_triplet_pt4(a, q, k, i)
term(12) = term(12) + s2(a,p,i,q) * wm_interm_50_triplet_pt4(j, k) * wm_interm_51_triplet_pt4(a, j, k, i)
term(13) = term(13) + s2(a,p,j,i) * wm_interm_50_triplet_pt4(j, k) * wm_interm_51_triplet_pt4(a, q, k, i)
term(14) = term(14) + s2(a,p,q,i) * wm_interm_50_triplet_pt4(j, k) * wm_interm_51_triplet_pt4(a, j, k, i)
term(15) = term(15) + s1(p,i) * wm_interm_45_triplet_pt4(a, j, k, q) * wm_interm_51_triplet_pt4(a, j, k, i)
term(16) = term(16) + t2(a,p,q,i) * wm_interm_45_triplet_pt4(a, j, k, i) * wm_interm_59_triplet_pt4(j, k)
term(17) = term(17) + t2(a,p,i,q) * wm_interm_45_triplet_pt4(a, j, k, i) * wm_interm_59_triplet_pt4(j, k)
term(18) = term(18) + t2(a,p,j,i) * wm_interm_41_triplet_pt4(i, k) * wm_interm_62_triplet_pt4(a, j, k, q)
term(19) = term(19) + t2(a,p,j,i) * wm_interm_41_triplet_pt4(j, k) * wm_interm_60_triplet_pt4(a, k, i, q)
term(20) = term(20) + t2(a,p,j,i) * wm_interm_41_triplet_pt4(j, k) * wm_interm_60_triplet_pt4(a, i, k, q)
term(21) = term(21) + t2(a,p,j,i) * wm_interm_41_triplet_pt4(i, k) * wm_interm_60_triplet_pt4(a, j, k, q)
term(22) = term(22) + t1(p,i) * wm_interm_45_triplet_pt4(a, j, k, i) * wm_interm_51_triplet_pt4(a, j, k, q)
end do 
end do 
end do 
end do 

term(11) = term(11) * (-4.0d+0) 
term(12) = term(12) * (-16.0d+0) 
term(13) = term(13) * (-4.0d+0) 
term(14) = term(14) * 8.0d+0 
term(15) = term(15) * 8.0d+0 
term(16) = term(16) * (-4.0d+0) 
term(17) = term(17) * 8.0d+0 
term(18) = term(18) * 3.0d+0 
term(19) = term(19) * (-8.0d+0) 
term(20) = term(20) * 4.0d+0 
term(21) = term(21) * (-4.0d+0) 
term(22) = term(22) * (-8.0d+0) 

do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(23) = term(23) + r1(vrdav_Rl, b,q) * wm_interm_55_triplet_pt4(a, p) * wm_interm_56_triplet_pt4(a, b)
term(24) = term(24) + s1(p,q) * wm_interm_56_triplet_pt4(a, b) * wm_interm_57_triplet_pt4(b, a)
term(25) = term(25) + r1(vrdav_Rl, b,q) * wm_interm_55_triplet_pt4(a, p) * wm_interm_58_triplet_pt4(a, b)
term(26) = term(26) + s1(p,q) * wm_interm_57_triplet_pt4(a, b) * wm_interm_58_triplet_pt4(b, a)
term(27) = term(27) + t1(p,q) * wm_interm_56_triplet_pt4(a, b) * wm_interm_57_triplet_pt4(b, a)
term(28) = term(28) + t1(p,q) * wm_interm_57_triplet_pt4(a, b) * wm_interm_58_triplet_pt4(b, a)
end do 
end do 

term(23) = term(23) * (-4.0d+0) 
term(24) = term(24) * 8.0d+0 
term(25) = term(25) * 2.0d+0 
term(26) = term(26) * (-4.0d+0) 
term(27) = term(27) * 8.0d+0 
term(28) = term(28) * (-4.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(29) = term(29) + s2(a,p,j,i) * wm_interm_41_triplet_pt4(k, j) * wm_interm_43_triplet_pt4(a, q, i, k)
term(30) = term(30) + s2(a,p,j,i) * wm_interm_41_triplet_pt4(k, i) * wm_interm_43_triplet_pt4(a, q, j, k)
term(31) = term(31) + s1(p,i) * wm_interm_45_triplet_pt4(a, j, q, k) * wm_interm_51_triplet_pt4(a, j, i, k)
term(32) = term(32) + s1(p,i) * wm_interm_45_triplet_pt4(a, j, k, q) * wm_interm_51_triplet_pt4(a, j, i, k)
term(33) = term(33) + s1(p,i) * wm_interm_45_triplet_pt4(a, j, q, k) * wm_interm_51_triplet_pt4(a, j, k, i)
term(34) = term(34) + s2(a,p,j,i) * wm_interm_50_triplet_pt4(i, k) * wm_interm_51_triplet_pt4(a, q, j, k)
term(35) = term(35) + s2(a,p,i,q) * wm_interm_50_triplet_pt4(j, k) * wm_interm_51_triplet_pt4(a, j, i, k)
term(36) = term(36) + s2(a,p,q,i) * wm_interm_50_triplet_pt4(j, k) * wm_interm_51_triplet_pt4(a, j, i, k)
term(37) = term(37) + s2(a,p,j,i) * wm_interm_50_triplet_pt4(j, k) * wm_interm_51_triplet_pt4(a, q, i, k)
term(38) = term(38) + t2(a,p,q,i) * wm_interm_45_triplet_pt4(a, j, i, k) * wm_interm_59_triplet_pt4(j, k)
term(39) = term(39) + t2(a,p,i,q) * wm_interm_45_triplet_pt4(a, j, i, k) * wm_interm_59_triplet_pt4(j, k)
term(40) = term(40) + t1(p,i) * wm_interm_45_triplet_pt4(a, j, i, k) * wm_interm_51_triplet_pt4(a, j, q, k)
term(41) = term(41) + t1(p,i) * wm_interm_45_triplet_pt4(a, j, k, i) * wm_interm_51_triplet_pt4(a, j, q, k)
term(42) = term(42) + t1(p,i) * wm_interm_45_triplet_pt4(a, j, i, k) * wm_interm_51_triplet_pt4(a, j, k, q)
end do 
end do 
end do 
end do 

term(29) = term(29) * 8.0d+0 
term(30) = term(30) * (-4.0d+0) 
term(31) = term(31) * 8.0d+0 
term(32) = term(32) * (-4.0d+0) 
term(33) = term(33) * (-4.0d+0) 
term(34) = term(34) * (-4.0d+0) 
term(35) = term(35) * 8.0d+0 
term(36) = term(36) * (-4.0d+0) 
term(37) = term(37) * 8.0d+0 
term(38) = term(38) * 2.0d+0 
term(39) = term(39) * (-4.0d+0) 
term(40) = term(40) * (-8.0d+0) 
term(41) = term(41) * 4.0d+0 
term(42) = term(42) * 4.0d+0 

do j = 1, nocc 
do i = 1, nocc 
term(43) = term(43) + wm_interm_59_triplet_pt4(i, j) * wm_interm_8_triplet_pt4(p, i, q, j)
term(44) = term(44) + wm_interm_14_triplet_pt4(p, i, q, j) * wm_interm_59_triplet_pt4(i, j)
term(45) = term(45) + wm_interm_14_triplet_pt4(p, q, i, j) * wm_interm_59_triplet_pt4(i, j)
term(46) = term(46) + wm_interm_59_triplet_pt4(i, j) * wm_interm_79_triplet_pt4(p, q, i, j)
term(47) = term(47) + wm_interm_59_triplet_pt4(i, j) * wm_interm_79_triplet_pt4(p, i, q, j)
term(48) = term(48) + wm_interm_50_triplet_pt4(i, j) * wm_interm_7_triplet_pt4(p, i, q, j)
term(49) = term(49) + wm_interm_24_triplet_pt4(p, i, q, j) * wm_interm_50_triplet_pt4(i, j)
term(50) = term(50) + wm_interm_24_triplet_pt4(p, q, i, j) * wm_interm_50_triplet_pt4(i, j)
term(51) = term(51) + wm_interm_27_triplet_pt4(p, q, i, j) * wm_interm_50_triplet_pt4(i, j)
term(52) = term(52) + wm_interm_27_triplet_pt4(p, i, q, j) * wm_interm_50_triplet_pt4(i, j)
term(53) = term(53) + s1(p,i) * wm_interm_41_triplet_pt4(j, i) * wm_interm_53_triplet_pt4(q, j)
term(54) = term(54) + s1(p,q) * wm_interm_41_triplet_pt4(i, j) * wm_interm_53_triplet_pt4(j, i)
term(55) = term(55) + s1(p,i) * wm_interm_41_triplet_pt4(j, i) * wm_interm_54_triplet_pt4(q, j)
term(56) = term(56) + s1(p,q) * wm_interm_41_triplet_pt4(i, j) * wm_interm_54_triplet_pt4(j, i)
term(57) = term(57) + r1(vrdav_Rr, p,i) * wm_interm_53_triplet_pt4(i, j) * wm_interm_59_triplet_pt4(j, q)
term(58) = term(58) + r1(vrdav_Rr, p,i) * wm_interm_54_triplet_pt4(i, j) * wm_interm_59_triplet_pt4(j, q)
term(59) = term(59) + t1(p,q) * wm_interm_41_triplet_pt4(i, j) * wm_interm_53_triplet_pt4(j, i)
term(60) = term(60) + t1(p,q) * wm_interm_41_triplet_pt4(i, j) * wm_interm_54_triplet_pt4(j, i)
term(61) = term(61) + r1(vrdav_Rr, p,i) * wm_interm_54_triplet_pt4(j, q) * wm_interm_59_triplet_pt4(i, j)
term(62) = term(62) + r1(vrdav_Rr, p,i) * wm_interm_53_triplet_pt4(j, q) * wm_interm_59_triplet_pt4(i, j)
term(63) = term(63) + t1(p,i) * wm_interm_41_triplet_pt4(i, j) * wm_interm_53_triplet_pt4(j, q)
term(64) = term(64) + t1(p,i) * wm_interm_41_triplet_pt4(i, j) * wm_interm_54_triplet_pt4(j, q)
end do 
end do 

term(43) = term(43) * 6.0d+0 
term(44) = term(44) * 2.0d+0 
term(45) = term(45) * (-4.0d+0) 
term(46) = term(46) * 2.0d+0 
term(47) = term(47) * (-4.0d+0) 
term(48) = term(48) * (-6.0d+0) 
term(49) = term(49) * 2.0d+0 
term(50) = term(50) * (-4.0d+0) 
term(51) = term(51) * 2.0d+0 
term(52) = term(52) * (-4.0d+0) 
term(53) = term(53) * 2.0d+0 
term(54) = term(54) * (-4.0d+0) 
term(55) = term(55) * (-4.0d+0) 
term(56) = term(56) * 8.0d+0 
term(57) = term(57) * 2.0d+0 
term(58) = term(58) * (-4.0d+0) 
term(59) = term(59) * (-4.0d+0) 
term(60) = term(60) * 8.0d+0 
term(61) = term(61) * (-4.0d+0) 
term(62) = term(62) * 2.0d+0 
term(63) = term(63) * 4.0d+0 
term(64) = term(64) * (-8.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
term(65) = term(65) + wm_interm_5_triplet_pt4(i, j) * wm_interm_64_triplet_pt4(p, j, q, i)
term(66) = term(66) + wm_interm_5_triplet_pt4(i, j) * wm_interm_71_triplet_pt4(p, j, q, i)
term(67) = term(67) + wm_interm_5_triplet_pt4(i, j) * wm_interm_71_triplet_pt4(p, j, i, q)
term(68) = term(68) + wm_interm_5_triplet_pt4(i, j) * wm_interm_85_triplet_pt4(p, j, q, i)
term(69) = term(69) + wm_interm_5_triplet_pt4(i, j) * wm_interm_94_triplet_pt4(p, q, j, i)
term(70) = term(70) + wm_interm_5_triplet_pt4(i, j) * wm_interm_92_triplet_pt4(p, q, j, i)
term(71) = term(71) + wm_interm_5_triplet_pt4(i, j) * wm_interm_94_triplet_pt4(p, j, q, i)
term(72) = term(72) + wm_interm_5_triplet_pt4(i, j) * wm_interm_92_triplet_pt4(p, j, q, i)
end do 
end do 

term(65) = term(65) * 6.0d+0 
term(66) = term(66) * 4.0d+0 
term(67) = term(67) * (-8.0d+0) 
term(68) = term(68) * (-6.0d+0) 
term(69) = term(69) * (-4.0d+0) 
term(70) = term(70) * 2.0d+0 
term(71) = term(71) * 2.0d+0 
term(72) = term(72) * (-4.0d+0) 

do a = nocc + 1, nactive 
do i = 1, nocc 
term(73) = term(73) + r1(vrdav_Rl, a,i) * wm_interm_50_triplet_pt4(i, q) * wm_interm_56_triplet_pt4(p, a)
term(74) = term(74) + r1(vrdav_Rl, a,i) * wm_interm_50_triplet_pt4(i, q) * wm_interm_58_triplet_pt4(p, a)
end do 
end do 

term(73) = term(73) * (-4.0d+0) 
term(74) = term(74) * 2.0d+0 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(75) = term(75) + s1(a,q) * wm_interm_44_triplet_pt4(p, i) * wm_interm_47_triplet_pt4(a, i)
term(76) = term(76) + s1(a,q) * wm_interm_46_triplet_pt4(p, i) * wm_interm_47_triplet_pt4(a, i)
term(77) = term(77) + s1(p,i) * wm_interm_44_triplet_pt4(a, q) * wm_interm_47_triplet_pt4(a, i)
term(78) = term(78) + s1(p,i) * wm_interm_46_triplet_pt4(a, q) * wm_interm_47_triplet_pt4(a, i)
term(79) = term(79) + r1(vrdav_Rl, a,q) * wm_interm_42_triplet_pt4(a, i) * wm_interm_44_triplet_pt4(p, i)
term(80) = term(80) + r1(vrdav_Rl, a,q) * wm_interm_42_triplet_pt4(a, i) * wm_interm_46_triplet_pt4(p, i)
term(81) = term(81) + r1(vrdav_Rl, a,q) * wm_interm_44_triplet_pt4(p, i) * wm_interm_48_triplet_pt4(a, i)
term(82) = term(82) + r1(vrdav_Rl, a,q) * wm_interm_46_triplet_pt4(p, i) * wm_interm_48_triplet_pt4(a, i)
term(83) = term(83) + s1(a,q) * wm_interm_44_triplet_pt4(p, i) * wm_interm_52_triplet_pt4(a, i)
term(84) = term(84) + s1(a,q) * wm_interm_46_triplet_pt4(p, i) * wm_interm_52_triplet_pt4(a, i)
term(85) = term(85) + s1(p,i) * wm_interm_44_triplet_pt4(a, q) * wm_interm_52_triplet_pt4(a, i)
term(86) = term(86) + s1(p,i) * wm_interm_46_triplet_pt4(a, q) * wm_interm_52_triplet_pt4(a, i)
term(87) = term(87) + s1(p,q) * wm_interm_44_triplet_pt4(a, i) * wm_interm_47_triplet_pt4(a, i)
term(88) = term(88) + s1(p,q) * wm_interm_46_triplet_pt4(a, i) * wm_interm_47_triplet_pt4(a, i)
term(89) = term(89) + s1(p,q) * wm_interm_46_triplet_pt4(a, i) * wm_interm_52_triplet_pt4(a, i)
term(90) = term(90) + s1(p,q) * wm_interm_44_triplet_pt4(a, i) * wm_interm_52_triplet_pt4(a, i)
term(91) = term(91) + t1(p,q) * wm_interm_44_triplet_pt4(a, i) * wm_interm_47_triplet_pt4(a, i)
term(92) = term(92) + t1(p,q) * wm_interm_46_triplet_pt4(a, i) * wm_interm_47_triplet_pt4(a, i)
term(93) = term(93) + t1(p,q) * wm_interm_46_triplet_pt4(a, i) * wm_interm_52_triplet_pt4(a, i)
term(94) = term(94) + t1(p,q) * wm_interm_44_triplet_pt4(a, i) * wm_interm_52_triplet_pt4(a, i)
term(95) = term(95) + t1(a,q) * wm_interm_44_triplet_pt4(a, i) * wm_interm_47_triplet_pt4(p, i)
term(96) = term(96) + t1(a,q) * wm_interm_46_triplet_pt4(a, i) * wm_interm_47_triplet_pt4(p, i)
term(97) = term(97) + t1(a,q) * wm_interm_46_triplet_pt4(a, i) * wm_interm_52_triplet_pt4(p, i)
term(98) = term(98) + r1(vrdav_Rr, a,i) * wm_interm_56_triplet_pt4(a, p) * wm_interm_59_triplet_pt4(i, q)
term(99) = term(99) + r1(vrdav_Rr, a,i) * wm_interm_58_triplet_pt4(a, p) * wm_interm_59_triplet_pt4(i, q)
term(100) = term(100) + t1(a,q) * wm_interm_44_triplet_pt4(a, i) * wm_interm_52_triplet_pt4(p, i)
term(101) = term(101) + t1(p,i) * wm_interm_46_triplet_pt4(a, i) * wm_interm_52_triplet_pt4(a, q)
term(102) = term(102) + t1(p,i) * wm_interm_44_triplet_pt4(a, i) * wm_interm_47_triplet_pt4(a, q)
term(103) = term(103) + t1(p,i) * wm_interm_46_triplet_pt4(a, i) * wm_interm_47_triplet_pt4(a, q)
term(104) = term(104) + t1(p,i) * wm_interm_44_triplet_pt4(a, i) * wm_interm_52_triplet_pt4(a, q)
term(105) = term(105) + r1(vrdav_Rl, a,i) * s1(p,q) * wm_interm_1_triplet_pt4(a, i)
term(106) = term(106) + r1(vrdav_Rl, p,i) * s1(a,q) * wm_interm_1_triplet_pt4(a, i)
term(107) = term(107) + r1(vrdav_Rl, a,i) * s1(p,q) * wm_interm_2_triplet_pt4(a, i)
term(108) = term(108) + r1(vrdav_Rl, a,i) * s1(p,q) * wm_interm_3_triplet_pt4(a, i)
term(109) = term(109) + r1(vrdav_Rl, p,i) * s1(a,q) * wm_interm_2_triplet_pt4(a, i)
term(110) = term(110) + r1(vrdav_Rl, p,i) * s1(a,q) * wm_interm_3_triplet_pt4(a, i)
term(111) = term(111) + r1(vrdav_Rl, a,i) * t1(p,q) * wm_interm_1_triplet_pt4(a, i)
term(112) = term(112) + r1(vrdav_Rl, a,i) * t1(p,q) * wm_interm_2_triplet_pt4(a, i)
term(113) = term(113) + r1(vrdav_Rl, a,i) * t1(p,q) * wm_interm_3_triplet_pt4(a, i)
term(114) = term(114) + r1(vrdav_Rr, a,i) * s1(p,q) * wm_interm_0_triplet_pt4(a, i)
term(115) = term(115) + r1(vrdav_Rr, p,i) * t1(a,q) * wm_interm_0_triplet_pt4(a, i)
term(116) = term(116) + r1(vrdav_Rr, a,i) * t1(p,q) * wm_interm_0_triplet_pt4(a, i)
term(117) = term(117) + r1(vrdav_Rr, a,i) * s1(p,q) * wm_interm_20_triplet_pt4(a, i)
term(118) = term(118) + r1(vrdav_Rr, a,i) * s1(p,q) * wm_interm_21_triplet_pt4(a, i)
term(119) = term(119) + r1(vrdav_Rr, p,i) * t1(a,q) * wm_interm_20_triplet_pt4(a, i)
term(120) = term(120) + r1(vrdav_Rr, p,i) * t1(a,q) * wm_interm_21_triplet_pt4(a, i)
term(121) = term(121) + r1(vrdav_Rr, a,i) * t1(p,q) * wm_interm_20_triplet_pt4(a, i)
term(122) = term(122) + r1(vrdav_Rr, a,i) * t1(p,q) * wm_interm_21_triplet_pt4(a, i)
end do 
end do 

term(75) = term(75) * (-4.0d+0) 
term(76) = term(76) * 8.0d+0 
term(77) = term(77) * (-4.0d+0) 
term(78) = term(78) * 8.0d+0 
term(79) = term(79) * (-4.0d+0) 
term(80) = term(80) * 8.0d+0 
term(81) = term(81) * 8.0d+0 
term(82) = term(82) * (-16.0d+0) 
term(83) = term(83) * 8.0d+0 
term(84) = term(84) * (-16.0d+0) 
term(85) = term(85) * 8.0d+0 
term(86) = term(86) * (-16.0d+0) 
term(87) = term(87) * (-4.0d+0) 
term(88) = term(88) * 8.0d+0 
term(89) = term(89) * (-16.0d+0) 
term(90) = term(90) * 8.0d+0 
term(91) = term(91) * (-4.0d+0) 
term(92) = term(92) * 8.0d+0 
term(93) = term(93) * (-16.0d+0) 
term(94) = term(94) * 8.0d+0 
term(95) = term(95) * 4.0d+0 
term(96) = term(96) * (-8.0d+0) 
term(97) = term(97) * 16.0d+0 
term(98) = term(98) * (-8.0d+0) 
term(99) = term(99) * 4.0d+0 
term(100) = term(100) * (-8.0d+0) 
term(101) = term(101) * 16.0d+0 
term(102) = term(102) * 4.0d+0 
term(103) = term(103) * (-8.0d+0) 
term(104) = term(104) * (-8.0d+0) 
term(105) = term(105) * (-12.0d+0) 
term(106) = term(106) * 3.0d+0 
term(107) = term(107) * (-16.0d+0) 
term(108) = term(108) * 8.0d+0 
term(109) = term(109) * 4.0d+0 
term(110) = term(110) * (-2.0d+0) 
term(111) = term(111) * (-12.0d+0) 
term(112) = term(112) * (-16.0d+0) 
term(113) = term(113) * 8.0d+0 
term(114) = term(114) * 12.0d+0 
term(115) = term(115) * (-3.0d+0) 
term(116) = term(116) * 12.0d+0 
term(117) = term(117) * 16.0d+0 
term(118) = term(118) * (-8.0d+0) 
term(119) = term(119) * (-4.0d+0) 
term(120) = term(120) * 2.0d+0 
term(121) = term(121) * 16.0d+0 
term(122) = term(122) * (-8.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(123) = term(123) + s1(a,i) * wm_interm_45_triplet_pt4(p, i, j, k) * wm_interm_51_triplet_pt4(a, q, j, k)
term(124) = term(124) + s1(a,q) * wm_interm_45_triplet_pt4(p, i, j, k) * wm_interm_51_triplet_pt4(a, i, j, k)
term(125) = term(125) + s1(a,q) * wm_interm_45_triplet_pt4(p, i, j, k) * wm_interm_51_triplet_pt4(a, i, k, j)
term(126) = term(126) + s1(a,i) * wm_interm_45_triplet_pt4(p, i, j, k) * wm_interm_51_triplet_pt4(a, q, k, j)
term(127) = term(127) + r1(vrdav_Rr, p,i) * wm_interm_51_triplet_pt4(a, j, q, k) * wm_interm_62_triplet_pt4(a, k, i, j)
term(128) = term(128) + r1(vrdav_Rr, p,i) * wm_interm_51_triplet_pt4(a, j, q, k) * wm_interm_62_triplet_pt4(a, i, k, j)
term(129) = term(129) + t1(a,q) * wm_interm_45_triplet_pt4(a, i, j, k) * wm_interm_51_triplet_pt4(p, i, j, k)
term(130) = term(130) + t1(a,q) * wm_interm_45_triplet_pt4(a, i, j, k) * wm_interm_51_triplet_pt4(p, i, k, j)
end do 
end do 
end do 
end do 

term(123) = term(123) * (-4.0d+0) 
term(124) = term(124) * 8.0d+0 
term(125) = term(125) * (-4.0d+0) 
term(126) = term(126) * 8.0d+0 
term(127) = term(127) * 2.0d+0 
term(128) = term(128) * (-4.0d+0) 
term(129) = term(129) * (-8.0d+0) 
term(130) = term(130) * 4.0d+0 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(131) = term(131) + r1(vrdav_Rr, p,i) * wm_interm_49_triplet_pt4(i, j, k, q) * wm_interm_59_triplet_pt4(k, j)
term(132) = term(132) + r1(vrdav_Rr, p,i) * wm_interm_49_triplet_pt4(j, i, k, q) * wm_interm_59_triplet_pt4(k, j)
end do 
end do 
end do 

term(131) = term(131) * 2.0d+0 
term(132) = term(132) * (-4.0d+0) 

do a = nocc + 1, nactive 
term(133) = term(133) + t1(a,q) * wm_interm_56_triplet_pt4(a, p) * wm_interm_63_triplet_pt4
term(134) = term(134) + t1(a,q) * wm_interm_58_triplet_pt4(a, p) * wm_interm_63_triplet_pt4
term(135) = term(135) + wm_interm_10_triplet_pt4(p, a) * wm_interm_86_triplet_pt4(a, q)
term(136) = term(136) + wm_interm_0_triplet_pt4(a, q) * wm_interm_55_triplet_pt4(a, p)
term(137) = term(137) + wm_interm_10_triplet_pt4(p, a) * wm_interm_96_triplet_pt4(a, q)
term(138) = term(138) + wm_interm_10_triplet_pt4(p, a) * wm_interm_93_triplet_pt4(a, q)
term(139) = term(139) + wm_interm_20_triplet_pt4(a, q) * wm_interm_55_triplet_pt4(a, p)
term(140) = term(140) + wm_interm_21_triplet_pt4(a, q) * wm_interm_55_triplet_pt4(a, p)
end do 

term(133) = term(133) * 16.0d+0 
term(134) = term(134) * (-8.0d+0) 
term(135) = term(135) * (-6.0d+0) 
term(136) = term(136) * 6.0d+0 
term(137) = term(137) * 4.0d+0 
term(138) = term(138) * (-8.0d+0) 
term(139) = term(139) * (-8.0d+0) 
term(140) = term(140) * 4.0d+0 

do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
term(141) = term(141) + t1(p,i) * wm_interm_41_triplet_pt4(j, k) * wm_interm_49_triplet_pt4(k, i, j, q)
term(142) = term(142) + t1(p,i) * wm_interm_41_triplet_pt4(j, k) * wm_interm_49_triplet_pt4(i, k, j, q)
end do 
end do 
end do 

term(141) = term(141) * (-8.0d+0) 
term(142) = term(142) * 4.0d+0 

do i = 1, nocc 
term(143) = term(143) + t1(p,i) * wm_interm_54_triplet_pt4(i, q) * wm_interm_63_triplet_pt4
term(144) = term(144) + t1(p,i) * wm_interm_53_triplet_pt4(i, q) * wm_interm_63_triplet_pt4
term(145) = term(145) + wm_interm_1_triplet_pt4(p, i) * wm_interm_59_triplet_pt4(i, q)
term(146) = term(146) + wm_interm_2_triplet_pt4(p, i) * wm_interm_59_triplet_pt4(i, q)
term(147) = term(147) + wm_interm_3_triplet_pt4(p, i) * wm_interm_59_triplet_pt4(i, q)
term(148) = term(148) + wm_interm_5_triplet_pt4(i, q) * wm_interm_82_triplet_pt4(p, i)
term(149) = term(149) + wm_interm_5_triplet_pt4(i, q) * wm_interm_84_triplet_pt4(p, i)
term(150) = term(150) + wm_interm_5_triplet_pt4(i, q) * wm_interm_83_triplet_pt4(p, i)
end do 

term(143) = term(143) * 16.0d+0 
term(144) = term(144) * (-8.0d+0) 
term(145) = term(145) * 6.0d+0 
term(146) = term(146) * 8.0d+0 
term(147) = term(147) * (-4.0d+0) 
term(148) = term(148) * (-6.0d+0) 
term(149) = term(149) * 4.0d+0 
term(150) = term(150) * (-8.0d+0) 

do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(151) = term(151) + t1(b,q) * wm_interm_56_triplet_pt4(b, a) * wm_interm_57_triplet_pt4(a, p)
term(152) = term(152) + t1(b,q) * wm_interm_57_triplet_pt4(a, p) * wm_interm_58_triplet_pt4(b, a)
end do 
end do 

term(151) = term(151) * (-4.0d+0) 
term(152) = term(152) * 2.0d+0 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(153) = term(153) + s2(a,p,q,i) * wm_interm_41_triplet_pt4(j, i) * wm_interm_42_triplet_pt4(a, j)
term(154) = term(154) + s2(a,p,q,i) * wm_interm_41_triplet_pt4(j, i) * wm_interm_48_triplet_pt4(a, j)
term(155) = term(155) + s2(a,p,q,i) * wm_interm_47_triplet_pt4(a, j) * wm_interm_50_triplet_pt4(i, j)
term(156) = term(156) + s1(p,i) * wm_interm_44_triplet_pt4(a, j) * wm_interm_51_triplet_pt4(a, q, j, i)
term(157) = term(157) + s1(p,i) * wm_interm_46_triplet_pt4(a, j) * wm_interm_51_triplet_pt4(a, q, j, i)
term(158) = term(158) + s2(a,p,q,i) * wm_interm_50_triplet_pt4(i, j) * wm_interm_52_triplet_pt4(a, j)
term(159) = term(159) + s2(a,p,j,i) * wm_interm_47_triplet_pt4(a, j) * wm_interm_50_triplet_pt4(i, q)
term(160) = term(160) + s2(a,p,j,i) * wm_interm_50_triplet_pt4(i, q) * wm_interm_52_triplet_pt4(a, j)
term(161) = term(161) + r1(vrdav_Rr, p,i) * wm_interm_47_triplet_pt4(a, j) * wm_interm_60_triplet_pt4(a, j, i, q)
term(162) = term(162) + r1(vrdav_Rr, p,i) * wm_interm_52_triplet_pt4(a, j) * wm_interm_60_triplet_pt4(a, j, i, q)
term(163) = term(163) + t2(a,p,q,i) * wm_interm_44_triplet_pt4(a, j) * wm_interm_59_triplet_pt4(i, j)
term(164) = term(164) + t2(a,p,q,i) * wm_interm_46_triplet_pt4(a, j) * wm_interm_59_triplet_pt4(i, j)
term(165) = term(165) + t2(a,p,j,i) * wm_interm_44_triplet_pt4(a, i) * wm_interm_59_triplet_pt4(j, q)
term(166) = term(166) + t2(a,p,j,i) * wm_interm_46_triplet_pt4(a, i) * wm_interm_59_triplet_pt4(j, q)
term(167) = term(167) + t2(a,p,j,i) * wm_interm_46_triplet_pt4(a, j) * wm_interm_59_triplet_pt4(i, q)
term(168) = term(168) + t2(a,p,j,i) * wm_interm_44_triplet_pt4(a, j) * wm_interm_59_triplet_pt4(i, q)
term(169) = term(169) + r1(vrdav_Rl, p,i) * s1(a,j) * wm_interm_14_triplet_pt4(a, j, i, q)
term(170) = term(170) + r1(vrdav_Rl, a,j) * s1(p,i) * wm_interm_8_triplet_pt4(a, j, i, q)
term(171) = term(171) + r1(vrdav_Rl, a,j) * s1(p,i) * wm_interm_14_triplet_pt4(a, j, i, q)
term(172) = term(172) + r1(vrdav_Rr, a,j) * t1(p,i) * wm_interm_7_triplet_pt4(a, j, i, q)
term(173) = term(173) + r1(vrdav_Rr, p,i) * t1(a,j) * wm_interm_24_triplet_pt4(a, j, i, q)
term(174) = term(174) + r1(vrdav_Rr, a,j) * t1(p,i) * wm_interm_27_triplet_pt4(a, j, i, q)
term(175) = term(175) + r1(vrdav_Rr, a,j) * t1(p,i) * wm_interm_24_triplet_pt4(a, j, i, q)
end do 
end do 
end do 

term(153) = term(153) * (-4.0d+0) 
term(154) = term(154) * 8.0d+0 
term(155) = term(155) * (-4.0d+0) 
term(156) = term(156) * (-4.0d+0) 
term(157) = term(157) * 8.0d+0 
term(158) = term(158) * 8.0d+0 
term(159) = term(159) * (-4.0d+0) 
term(160) = term(160) * 8.0d+0 
term(161) = term(161) * (-4.0d+0) 
term(162) = term(162) * 8.0d+0 
term(163) = term(163) * 2.0d+0 
term(164) = term(164) * (-4.0d+0) 
term(165) = term(165) * 4.0d+0 
term(166) = term(166) * (-8.0d+0) 
term(167) = term(167) * 16.0d+0 
term(168) = term(168) * (-8.0d+0) 
term(169) = term(169) * 4.0d+0 
term(170) = term(170) * 12.0d+0 
term(171) = term(171) * 16.0d+0 
term(172) = term(172) * (-12.0d+0) 
term(173) = term(173) * (-4.0d+0) 
term(174) = term(174) * 4.0d+0 
term(175) = term(175) * (-8.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(176) = term(176) + s2(a,p,i,q) * wm_interm_41_triplet_pt4(j, i) * wm_interm_42_triplet_pt4(a, j)
term(177) = term(177) + s1(a,i) * wm_interm_45_triplet_pt4(p, i, j, q) * wm_interm_47_triplet_pt4(a, j)
term(178) = term(178) + s1(a,i) * wm_interm_45_triplet_pt4(p, i, q, j) * wm_interm_47_triplet_pt4(a, j)
term(179) = term(179) + s2(a,p,i,q) * wm_interm_41_triplet_pt4(j, i) * wm_interm_48_triplet_pt4(a, j)
term(180) = term(180) + s2(a,p,i,q) * wm_interm_47_triplet_pt4(a, j) * wm_interm_50_triplet_pt4(i, j)
term(181) = term(181) + s1(p,i) * wm_interm_46_triplet_pt4(a, j) * wm_interm_51_triplet_pt4(a, q, i, j)
term(182) = term(182) + r1(vrdav_Rl, a,i) * wm_interm_42_triplet_pt4(a, j) * wm_interm_45_triplet_pt4(p, i, j, q)
term(183) = term(183) + r1(vrdav_Rl, a,i) * wm_interm_42_triplet_pt4(a, j) * wm_interm_45_triplet_pt4(p, i, q, j)
term(184) = term(184) + s1(p,i) * wm_interm_44_triplet_pt4(a, j) * wm_interm_51_triplet_pt4(a, q, i, j)
term(185) = term(185) + r1(vrdav_Rl, a,i) * wm_interm_45_triplet_pt4(p, i, j, q) * wm_interm_48_triplet_pt4(a, j)
term(186) = term(186) + r1(vrdav_Rl, a,i) * wm_interm_45_triplet_pt4(p, i, q, j) * wm_interm_48_triplet_pt4(a, j)
term(187) = term(187) + s1(a,i) * wm_interm_45_triplet_pt4(p, i, j, q) * wm_interm_52_triplet_pt4(a, j)
term(188) = term(188) + s1(a,i) * wm_interm_45_triplet_pt4(p, i, q, j) * wm_interm_52_triplet_pt4(a, j)
term(189) = term(189) + s2(a,p,i,q) * wm_interm_50_triplet_pt4(i, j) * wm_interm_52_triplet_pt4(a, j)
term(190) = term(190) + s1(p,i) * wm_interm_45_triplet_pt4(a, i, j, q) * wm_interm_47_triplet_pt4(a, j)
term(191) = term(191) + s1(p,i) * wm_interm_45_triplet_pt4(a, i, q, j) * wm_interm_47_triplet_pt4(a, j)
term(192) = term(192) + s1(p,i) * wm_interm_45_triplet_pt4(a, i, q, j) * wm_interm_52_triplet_pt4(a, j)
term(193) = term(193) + s1(p,i) * wm_interm_45_triplet_pt4(a, i, j, q) * wm_interm_52_triplet_pt4(a, j)
term(194) = term(194) + s2(a,p,i,j) * wm_interm_47_triplet_pt4(a, j) * wm_interm_50_triplet_pt4(i, q)
term(195) = term(195) + s2(a,p,i,j) * wm_interm_50_triplet_pt4(i, q) * wm_interm_52_triplet_pt4(a, j)
term(196) = term(196) + r1(vrdav_Rr, p,i) * wm_interm_47_triplet_pt4(a, j) * wm_interm_60_triplet_pt4(a, i, j, q)
term(197) = term(197) + r1(vrdav_Rr, p,i) * wm_interm_52_triplet_pt4(a, j) * wm_interm_60_triplet_pt4(a, i, j, q)
term(198) = term(198) + t1(a,i) * wm_interm_44_triplet_pt4(a, j) * wm_interm_51_triplet_pt4(p, i, q, j)
term(199) = term(199) + t1(a,i) * wm_interm_46_triplet_pt4(a, j) * wm_interm_51_triplet_pt4(p, i, q, j)
term(200) = term(200) + t1(a,i) * wm_interm_44_triplet_pt4(a, j) * wm_interm_51_triplet_pt4(p, i, j, q)
term(201) = term(201) + t1(a,i) * wm_interm_46_triplet_pt4(a, j) * wm_interm_51_triplet_pt4(p, i, j, q)
term(202) = term(202) + t2(a,p,i,q) * wm_interm_44_triplet_pt4(a, j) * wm_interm_59_triplet_pt4(i, j)
term(203) = term(203) + t2(a,p,i,q) * wm_interm_46_triplet_pt4(a, j) * wm_interm_59_triplet_pt4(i, j)
term(204) = term(204) + t1(p,i) * wm_interm_44_triplet_pt4(a, j) * wm_interm_51_triplet_pt4(a, i, j, q)
term(205) = term(205) + t1(p,i) * wm_interm_46_triplet_pt4(a, j) * wm_interm_51_triplet_pt4(a, i, j, q)
term(206) = term(206) + t1(p,i) * wm_interm_46_triplet_pt4(a, j) * wm_interm_51_triplet_pt4(a, i, q, j)
term(207) = term(207) + t1(p,i) * wm_interm_44_triplet_pt4(a, j) * wm_interm_51_triplet_pt4(a, i, q, j)
term(208) = term(208) + r1(vrdav_Rl, p,i) * s1(a,j) * wm_interm_8_triplet_pt4(a, i, j, q)
term(209) = term(209) + r1(vrdav_Rl, p,i) * s1(a,j) * wm_interm_14_triplet_pt4(a, i, j, q)
term(210) = term(210) + r1(vrdav_Rl, a,j) * s1(p,i) * wm_interm_14_triplet_pt4(a, i, j, q)
term(211) = term(211) + r1(vrdav_Rr, p,i) * t1(a,j) * wm_interm_7_triplet_pt4(a, i, j, q)
term(212) = term(212) + r1(vrdav_Rr, p,i) * t1(a,j) * wm_interm_24_triplet_pt4(a, i, j, q)
term(213) = term(213) + r1(vrdav_Rr, a,j) * t1(p,i) * wm_interm_27_triplet_pt4(a, i, j, q)
term(214) = term(214) + r1(vrdav_Rr, a,j) * t1(p,i) * wm_interm_24_triplet_pt4(a, i, j, q)
end do 
end do 
end do 

term(176) = term(176) * 8.0d+0 
term(177) = term(177) * 8.0d+0 
term(178) = term(178) * (-4.0d+0) 
term(179) = term(179) * (-16.0d+0) 
term(180) = term(180) * 8.0d+0 
term(181) = term(181) * (-16.0d+0) 
term(182) = term(182) * 8.0d+0 
term(183) = term(183) * (-4.0d+0) 
term(184) = term(184) * 8.0d+0 
term(185) = term(185) * (-16.0d+0) 
term(186) = term(186) * 8.0d+0 
term(187) = term(187) * (-16.0d+0) 
term(188) = term(188) * 8.0d+0 
term(189) = term(189) * (-16.0d+0) 
term(190) = term(190) * 2.0d+0 
term(191) = term(191) * (-4.0d+0) 
term(192) = term(192) * 8.0d+0 
term(193) = term(193) * (-4.0d+0) 
term(194) = term(194) * 2.0d+0 
term(195) = term(195) * (-4.0d+0) 
term(196) = term(196) * 2.0d+0 
term(197) = term(197) * (-4.0d+0) 
term(198) = term(198) * 2.0d+0 
term(199) = term(199) * (-4.0d+0) 
term(200) = term(200) * (-4.0d+0) 
term(201) = term(201) * 8.0d+0 
term(202) = term(202) * (-4.0d+0) 
term(203) = term(203) * 8.0d+0 
term(204) = term(204) * 4.0d+0 
term(205) = term(205) * (-8.0d+0) 
term(206) = term(206) * 16.0d+0 
term(207) = term(207) * (-8.0d+0) 
term(208) = term(208) * (-3.0d+0) 
term(209) = term(209) * (-2.0d+0) 
term(210) = term(210) * (-8.0d+0) 
term(211) = term(211) * 3.0d+0 
term(212) = term(212) * 2.0d+0 
term(213) = term(213) * (-8.0d+0) 
term(214) = term(214) * 4.0d+0 

do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(215) = term(215) + r1(vrdav_Rr, p,i) * wm_interm_51_triplet_pt4(a, j, k, q) * wm_interm_62_triplet_pt4(a, i, k, j)
term(216) = term(216) + r1(vrdav_Rr, p,i) * wm_interm_51_triplet_pt4(a, j, k, q) * wm_interm_62_triplet_pt4(a, k, i, j)
term(217) = term(217) + t2(a,p,i,q) * wm_interm_41_triplet_pt4(j, k) * wm_interm_62_triplet_pt4(a, i, k, j)
end do 
end do 
end do 
end do 

term(215) = term(215) * 2.0d+0 
term(216) = term(216) * (-4.0d+0) 
term(217) = term(217) * (-4.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
term(218) = term(218) + s1(p,i) * wm_interm_41_triplet_pt4(j, k) * wm_interm_49_triplet_pt4(q, k, j, i)
term(219) = term(219) + s1(p,i) * wm_interm_41_triplet_pt4(j, k) * wm_interm_49_triplet_pt4(k, q, j, i)
end do 
end do 
end do 

term(218) = term(218) * (-4.0d+0) 
term(219) = term(219) * 8.0d+0 


    calc_D_vo_wm_triplet_pt4 = zero
    do s = 0, 219
    calc_D_vo_wm_triplet_pt4 = calc_D_vo_wm_triplet_pt4 + term(s)
    end do

    end function calc_D_vo_wm_triplet_pt4
    
    function calc_D_vv_wm_triplet_pt4(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vv_wm_triplet_pt4
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    double precision, dimension(:), intent(in) :: vrdav_Rl
    double precision, dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , k, j, i, b, a, l 
    real(F64), dimension(0:1010) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(0) = term(0) + wm_interm_7_triplet_pt4(q, i, j, k) * wm_interm_8_triplet_pt4(p, i, j, k)
term(1) = term(1) + wm_interm_14_triplet_pt4(p, i, j, k) * wm_interm_7_triplet_pt4(q, j, i, k)
term(2) = term(2) + wm_interm_14_triplet_pt4(p, i, j, k) * wm_interm_7_triplet_pt4(q, i, j, k)
term(3) = term(3) + wm_interm_24_triplet_pt4(q, i, j, k) * wm_interm_8_triplet_pt4(p, i, j, k)
term(4) = term(4) + wm_interm_27_triplet_pt4(q, i, j, k) * wm_interm_8_triplet_pt4(p, i, j, k)
term(5) = term(5) + wm_interm_14_triplet_pt4(p, i, j, k) * wm_interm_24_triplet_pt4(q, j, i, k)
term(6) = term(6) + wm_interm_14_triplet_pt4(p, i, j, k) * wm_interm_24_triplet_pt4(q, i, j, k)
term(7) = term(7) + wm_interm_14_triplet_pt4(p, i, j, k) * wm_interm_27_triplet_pt4(q, i, j, k)
term(8) = term(8) + wm_interm_14_triplet_pt4(p, i, j, k) * wm_interm_27_triplet_pt4(q, j, i, k)
term(9) = term(9) + r1(vrdav_Rr, p,i) * wm_interm_60_triplet_pt4(q, i, j, k) * wm_interm_87_triplet_pt4(k, j)
term(10) = term(10) + r1(vrdav_Rr, p,i) * wm_interm_60_triplet_pt4(q, i, j, k) * wm_interm_89_triplet_pt4(k, j)
term(11) = term(11) + r1(vrdav_Rr, p,i) * wm_interm_60_triplet_pt4(q, i, j, k) * wm_interm_95_triplet_pt4(k, j)
term(12) = term(12) + r1(vrdav_Rr, p,i) * wm_interm_60_triplet_pt4(q, i, j, k) * wm_interm_98_triplet_pt4(k, j)
term(13) = term(13) + r1(vrdav_Rr, p,i) * wm_interm_24_triplet_pt4(q, i, j, k) * wm_interm_54_triplet_pt4(k, j)
term(14) = term(14) + r1(vrdav_Rr, p,i) * wm_interm_24_triplet_pt4(q, i, j, k) * wm_interm_53_triplet_pt4(k, j)
term(15) = term(15) + r1(vrdav_Rr, p,i) * wm_interm_27_triplet_pt4(q, i, j, k) * wm_interm_53_triplet_pt4(k, j)
term(16) = term(16) + r1(vrdav_Rr, p,i) * wm_interm_27_triplet_pt4(q, i, j, k) * wm_interm_54_triplet_pt4(k, j)
end do 
end do 
end do 

term(0) = term(0) * 6.0d+0 
term(1) = term(1) * (-6.0d+0) 
term(2) = term(2) * 6.0d+0 
term(3) = term(3) * (-6.0d+0) 
term(4) = term(4) * 6.0d+0 
term(5) = term(5) * 4.0d+0 
term(6) = term(6) * (-8.0d+0) 
term(7) = term(7) * 4.0d+0 
term(8) = term(8) * (-8.0d+0) 
term(9) = term(9) * 4.0d+0 
term(10) = term(10) * 4.0d+0 
term(11) = term(11) * (-16.0d+0) 
term(12) = term(12) * 32.0d+0 
term(13) = term(13) * 16.0d+0 
term(14) = term(14) * (-8.0d+0) 
term(15) = term(15) * 4.0d+0 
term(16) = term(16) * (-8.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(17) = term(17) + s2(a,q,j,i) * wm_interm_43_triplet_pt4(a, k, l, i) * wm_interm_64_triplet_pt4(p, l, j, k)
term(18) = term(18) + s2(a,q,j,i) * wm_interm_43_triplet_pt4(a, k, l, i) * wm_interm_71_triplet_pt4(p, l, j, k)
term(19) = term(19) + s2(a,p,j,i) * wm_interm_43_triplet_pt4(q, k, l, i) * wm_interm_64_triplet_pt4(a, l, j, k)
term(20) = term(20) + s2(a,p,j,i) * wm_interm_43_triplet_pt4(q, k, l, i) * wm_interm_71_triplet_pt4(a, l, j, k)
end do 
end do 
end do 
end do 
end do 

term(17) = term(17) * (-6.0d+0) 
term(18) = term(18) * (-8.0d+0) 
term(19) = term(19) * 18.0d+0 
term(20) = term(20) * 16.0d+0 

do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(21) = term(21) + r1(vrdav_Rl, a,j) * r2p(vrdav_Rr, p,i,b,j) * s1(q,i) * wm_interm_56_triplet_pt4(b, a)
term(22) = term(22) + r1(vrdav_Rl, a,j) * r2p(vrdav_Rr, p,i,b,j) * s1(q,i) * wm_interm_58_triplet_pt4(b, a)
end do 
end do 
end do 
end do 

term(21) = term(21) * 12.0d+0 
term(22) = term(22) * (-6.0d+0) 

do j = 1, nocc 
do a = nocc + 1, nactive 
do i = 1, nocc 
do b = nocc + 1, nactive 
term(23) = term(23) + r1(vrdav_Rl, a,j) * r2m(vrdav_Rr, b,i,p,j) * s1(q,i) * wm_interm_56_triplet_pt4(b, a)
term(24) = term(24) + r1(vrdav_Rl, a,j) * r2m(vrdav_Rr, b,i,p,j) * s1(q,i) * wm_interm_58_triplet_pt4(b, a)
term(25) = term(25) + r1(vrdav_Rr, p,i) * s2(a,q,i,j) * t1(b,j) * wm_interm_90_triplet_pt4(b, a)
term(26) = term(26) + r1(vrdav_Rr, p,i) * s2(a,q,i,j) * t1(b,j) * wm_interm_91_triplet_pt4(b, a)
term(27) = term(27) + r2p(vrdav_Rl, p,i,a,j) * r1(vrdav_Rr, b,j) * t1(q,i) * wm_interm_56_triplet_pt4(b, a)
term(28) = term(28) + r2p(vrdav_Rl, p,i,a,j) * r1(vrdav_Rr, b,j) * t1(q,i) * wm_interm_58_triplet_pt4(b, a)
term(29) = term(29) + r2m(vrdav_Rl, a,i,q,j) * r1(vrdav_Rr, p,i) * t1(b,j) * wm_interm_58_triplet_pt4(b, a)
term(30) = term(30) + r2m(vrdav_Rl, a,i,q,j) * r1(vrdav_Rr, p,i) * t1(b,j) * wm_interm_56_triplet_pt4(b, a)
term(31) = term(31) + r1(vrdav_Rr, p,i) * s2(a,q,i,j) * t1(b,j) * wm_interm_99_triplet_pt4(b, a)
term(32) = term(32) + r1(vrdav_Rr, p,i) * s2(a,q,i,j) * t1(b,j) * wm_interm_100_triplet_pt4(b, a)
term(33) = term(33) + r2m(vrdav_Rl, a,i,p,j) * r1(vrdav_Rr, b,j) * t1(q,i) * wm_interm_56_triplet_pt4(b, a)
term(34) = term(34) + r2m(vrdav_Rl, a,i,p,j) * r1(vrdav_Rr, b,j) * t1(q,i) * wm_interm_58_triplet_pt4(b, a)
end do 
end do 
end do 
end do 

term(23) = term(23) * (-8.0d+0) 
term(24) = term(24) * 4.0d+0 
term(25) = term(25) * (-2.0d+0) 
term(26) = term(26) * (-2.0d+0) 
term(27) = term(27) * 12.0d+0 
term(28) = term(28) * (-6.0d+0) 
term(29) = term(29) * 8.0d+0 
term(30) = term(30) * (-16.0d+0) 
term(31) = term(31) * 8.0d+0 
term(32) = term(32) * (-16.0d+0) 
term(33) = term(33) * (-8.0d+0) 
term(34) = term(34) * 4.0d+0 

do i = 1, nocc 
do a = nocc + 1, nactive 
do j = 1, nocc 
do b = nocc + 1, nactive 
term(35) = term(35) + r1(vrdav_Rl, a,j) * r2m(vrdav_Rr, b,j,p,i) * s1(q,i) * wm_interm_56_triplet_pt4(b, a)
term(36) = term(36) + r1(vrdav_Rl, a,j) * r2m(vrdav_Rr, b,j,p,i) * s1(q,i) * wm_interm_58_triplet_pt4(b, a)
term(37) = term(37) + r2p(vrdav_Rl, q,j,a,i) * r1(vrdav_Rr, p,i) * t1(b,j) * wm_interm_56_triplet_pt4(b, a)
term(38) = term(38) + r2p(vrdav_Rl, q,j,a,i) * r1(vrdav_Rr, p,i) * t1(b,j) * wm_interm_58_triplet_pt4(b, a)
term(39) = term(39) + r1(vrdav_Rr, p,i) * s2(a,q,j,i) * t1(b,j) * wm_interm_90_triplet_pt4(b, a)
term(40) = term(40) + r1(vrdav_Rr, p,i) * s2(a,q,j,i) * t1(b,j) * wm_interm_91_triplet_pt4(b, a)
term(41) = term(41) + r2m(vrdav_Rl, a,j,q,i) * r1(vrdav_Rr, p,i) * t1(b,j) * wm_interm_56_triplet_pt4(b, a)
term(42) = term(42) + r2m(vrdav_Rl, a,j,q,i) * r1(vrdav_Rr, p,i) * t1(b,j) * wm_interm_58_triplet_pt4(b, a)
term(43) = term(43) + r1(vrdav_Rr, p,i) * s2(a,q,j,i) * t1(b,j) * wm_interm_99_triplet_pt4(b, a)
term(44) = term(44) + r1(vrdav_Rr, p,i) * s2(a,q,j,i) * t1(b,j) * wm_interm_100_triplet_pt4(b, a)
term(45) = term(45) + r2m(vrdav_Rl, a,j,p,i) * r1(vrdav_Rr, b,j) * t1(q,i) * wm_interm_56_triplet_pt4(b, a)
term(46) = term(46) + r2m(vrdav_Rl, a,j,p,i) * r1(vrdav_Rr, b,j) * t1(q,i) * wm_interm_58_triplet_pt4(b, a)
end do 
end do 
end do 
end do 

term(35) = term(35) * 16.0d+0 
term(36) = term(36) * (-8.0d+0) 
term(37) = term(37) * (-24.0d+0) 
term(38) = term(38) * 12.0d+0 
term(39) = term(39) * 4.0d+0 
term(40) = term(40) * 4.0d+0 
term(41) = term(41) * 32.0d+0 
term(42) = term(42) * (-16.0d+0) 
term(43) = term(43) * (-16.0d+0) 
term(44) = term(44) * 32.0d+0 
term(45) = term(45) * 16.0d+0 
term(46) = term(46) * (-8.0d+0) 

do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(47) = term(47) + r1(vrdav_Rl, a,j) * s1(p,i) * t2(b,q,i,j) * wm_interm_65_triplet_pt4(a, b)
term(48) = term(48) + r1(vrdav_Rl, a,j) * s1(p,i) * t2(b,q,i,j) * wm_interm_70_triplet_pt4(a, b)
term(49) = term(49) + r1(vrdav_Rl, a,j) * s1(p,i) * t2(b,q,i,j) * wm_interm_72_triplet_pt4(a, b)
term(50) = term(50) + r1(vrdav_Rl, a,j) * s1(p,i) * t2(b,q,i,j) * wm_interm_73_triplet_pt4(a, b)
term(51) = term(51) + r1(vrdav_Rl, p,i) * s1(a,j) * t2(b,q,i,j) * wm_interm_80_triplet_pt4(a, b)
term(52) = term(52) + r1(vrdav_Rl, p,i) * s1(a,j) * t2(b,q,i,j) * wm_interm_81_triplet_pt4(a, b)
term(53) = term(53) + r1(vrdav_Rl, p,i) * s1(a,j) * t2(b,q,i,j) * wm_interm_72_triplet_pt4(a, b)
term(54) = term(54) + r1(vrdav_Rl, p,i) * s1(a,j) * t2(b,q,i,j) * wm_interm_73_triplet_pt4(a, b)
term(55) = term(55) + r1(vrdav_Rr, a,j) * s2(b,p,i,j) * t1(q,i) * wm_interm_90_triplet_pt4(a, b)
term(56) = term(56) + r1(vrdav_Rr, a,j) * s2(b,p,i,j) * t1(q,i) * wm_interm_91_triplet_pt4(a, b)
term(57) = term(57) + r1(vrdav_Rr, a,j) * s2(b,p,i,j) * t1(q,i) * wm_interm_99_triplet_pt4(a, b)
term(58) = term(58) + r1(vrdav_Rr, a,j) * s2(b,p,i,j) * t1(q,i) * wm_interm_100_triplet_pt4(a, b)
end do 
end do 
end do 
end do 

term(47) = term(47) * 6.0d+0 
term(48) = term(48) * (-6.0d+0) 
term(49) = term(49) * (-8.0d+0) 
term(50) = term(50) * 16.0d+0 
term(51) = term(51) * (-4.0d+0) 
term(52) = term(52) * 8.0d+0 
term(53) = term(53) * (-4.0d+0) 
term(54) = term(54) * 8.0d+0 
term(55) = term(55) * (-3.0d+0) 
term(56) = term(56) * 3.0d+0 
term(57) = term(57) * 4.0d+0 
term(58) = term(58) * (-8.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(59) = term(59) + r1(vrdav_Rl, a,j) * s1(p,i) * t2(b,q,j,i) * wm_interm_65_triplet_pt4(a, b)
term(60) = term(60) + r1(vrdav_Rl, a,j) * s1(p,i) * t2(b,q,j,i) * wm_interm_70_triplet_pt4(a, b)
term(61) = term(61) + r1(vrdav_Rl, a,j) * s1(p,i) * t2(b,q,j,i) * wm_interm_72_triplet_pt4(a, b)
term(62) = term(62) + r1(vrdav_Rl, a,j) * s1(p,i) * t2(b,q,j,i) * wm_interm_73_triplet_pt4(a, b)
term(63) = term(63) + r1(vrdav_Rl, p,i) * s1(a,j) * t2(b,q,j,i) * wm_interm_80_triplet_pt4(a, b)
term(64) = term(64) + r1(vrdav_Rl, p,i) * s1(a,j) * t2(b,q,j,i) * wm_interm_81_triplet_pt4(a, b)
term(65) = term(65) + r1(vrdav_Rl, p,i) * s1(a,j) * t2(b,q,j,i) * wm_interm_72_triplet_pt4(a, b)
term(66) = term(66) + r1(vrdav_Rl, p,i) * s1(a,j) * t2(b,q,j,i) * wm_interm_73_triplet_pt4(a, b)
term(67) = term(67) + r1(vrdav_Rr, a,j) * s2(b,p,j,i) * t1(q,i) * wm_interm_90_triplet_pt4(a, b)
term(68) = term(68) + r1(vrdav_Rr, a,j) * s2(b,p,j,i) * t1(q,i) * wm_interm_91_triplet_pt4(a, b)
term(69) = term(69) + r1(vrdav_Rr, a,j) * s2(b,p,j,i) * t1(q,i) * wm_interm_99_triplet_pt4(a, b)
term(70) = term(70) + r1(vrdav_Rr, a,j) * s2(b,p,j,i) * t1(q,i) * wm_interm_100_triplet_pt4(a, b)
end do 
end do 
end do 
end do 

term(59) = term(59) * (-12.0d+0) 
term(60) = term(60) * 12.0d+0 
term(61) = term(61) * 16.0d+0 
term(62) = term(62) * (-32.0d+0) 
term(63) = term(63) * 8.0d+0 
term(64) = term(64) * (-16.0d+0) 
term(65) = term(65) * 8.0d+0 
term(66) = term(66) * (-16.0d+0) 
term(67) = term(67) * 6.0d+0 
term(68) = term(68) * (-6.0d+0) 
term(69) = term(69) * (-8.0d+0) 
term(70) = term(70) * 16.0d+0 

do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(71) = term(71) + r2m(vrdav_Rl, a,j,p,i) * r1(vrdav_Rr, b,k) * t2(a,q,l,i) * wm_interm_60_triplet_pt4(b, l, k, j)
term(72) = term(72) + r2m(vrdav_Rl, a,i,p,j) * r1(vrdav_Rr, b,k) * t2(a,q,l,i) * wm_interm_60_triplet_pt4(b, l, k, j)
end do 
end do 
end do 
end do 
end do 
end do 

term(71) = term(71) * (-8.0d+0) 
term(72) = term(72) * 4.0d+0 

do l = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(73) = term(73) + s2(a,q,j,i) * wm_interm_43_triplet_pt4(a, k, j, l) * wm_interm_64_triplet_pt4(p, l, k, i)
term(74) = term(74) + s2(a,q,j,i) * wm_interm_43_triplet_pt4(a, k, j, l) * wm_interm_71_triplet_pt4(p, l, k, i)
term(75) = term(75) + s2(a,q,j,i) * wm_interm_14_triplet_pt4(p, j, k, l) * wm_interm_51_triplet_pt4(a, k, l, i)
term(76) = term(76) + s2(a,q,j,i) * wm_interm_51_triplet_pt4(a, k, l, i) * wm_interm_79_triplet_pt4(p, j, k, l)
term(77) = term(77) + s2(a,q,j,i) * wm_interm_14_triplet_pt4(p, i, k, l) * wm_interm_51_triplet_pt4(a, k, j, l)
term(78) = term(78) + s2(a,q,j,i) * wm_interm_14_triplet_pt4(p, j, k, l) * wm_interm_51_triplet_pt4(a, k, i, l)
term(79) = term(79) + s2(a,q,j,i) * wm_interm_51_triplet_pt4(a, k, i, l) * wm_interm_79_triplet_pt4(p, j, k, l)
term(80) = term(80) + s2(a,q,j,i) * wm_interm_51_triplet_pt4(a, k, j, l) * wm_interm_79_triplet_pt4(p, i, k, l)
term(81) = term(81) + s2(a,p,j,i) * wm_interm_43_triplet_pt4(q, k, j, l) * wm_interm_64_triplet_pt4(a, l, k, i)
term(82) = term(82) + s2(a,p,j,i) * wm_interm_14_triplet_pt4(a, i, k, l) * wm_interm_51_triplet_pt4(q, k, j, l)
term(83) = term(83) + s2(a,p,j,i) * wm_interm_51_triplet_pt4(q, k, j, l) * wm_interm_79_triplet_pt4(a, i, k, l)
term(84) = term(84) + s2(a,p,j,i) * wm_interm_14_triplet_pt4(a, j, k, l) * wm_interm_51_triplet_pt4(q, k, i, l)
term(85) = term(85) + s2(a,p,j,i) * wm_interm_51_triplet_pt4(q, k, i, l) * wm_interm_79_triplet_pt4(a, j, k, l)
term(86) = term(86) + s2(a,p,j,i) * wm_interm_43_triplet_pt4(q, k, j, l) * wm_interm_71_triplet_pt4(a, l, k, i)
term(87) = term(87) + s2(a,p,j,i) * wm_interm_51_triplet_pt4(q, k, l, i) * wm_interm_79_triplet_pt4(a, j, k, l)
term(88) = term(88) + s2(a,p,j,i) * wm_interm_14_triplet_pt4(a, j, k, l) * wm_interm_51_triplet_pt4(q, k, l, i)
term(89) = term(89) + t2(a,q,j,i) * wm_interm_24_triplet_pt4(p, j, k, l) * wm_interm_45_triplet_pt4(a, k, l, i)
term(90) = term(90) + t2(a,q,j,i) * wm_interm_24_triplet_pt4(p, j, k, l) * wm_interm_45_triplet_pt4(a, k, i, l)
term(91) = term(91) + t2(a,q,j,i) * wm_interm_24_triplet_pt4(p, i, k, l) * wm_interm_45_triplet_pt4(a, k, j, l)
term(92) = term(92) + t2(a,q,j,i) * wm_interm_24_triplet_pt4(a, i, k, l) * wm_interm_45_triplet_pt4(p, k, j, l)
term(93) = term(93) + t2(a,q,j,i) * wm_interm_24_triplet_pt4(a, j, k, l) * wm_interm_45_triplet_pt4(p, k, l, i)
term(94) = term(94) + t2(a,q,j,i) * wm_interm_24_triplet_pt4(a, j, k, l) * wm_interm_45_triplet_pt4(p, k, i, l)
term(95) = term(95) + t2(a,q,j,i) * wm_interm_27_triplet_pt4(a, j, k, l) * wm_interm_45_triplet_pt4(p, k, l, i)
term(96) = term(96) + t2(a,q,j,i) * wm_interm_27_triplet_pt4(p, j, k, l) * wm_interm_45_triplet_pt4(a, k, l, i)
term(97) = term(97) + t2(a,q,j,i) * wm_interm_27_triplet_pt4(p, j, k, l) * wm_interm_45_triplet_pt4(a, k, i, l)
term(98) = term(98) + t2(a,q,j,i) * wm_interm_27_triplet_pt4(a, j, k, l) * wm_interm_45_triplet_pt4(p, k, i, l)
term(99) = term(99) + t2(a,q,j,i) * wm_interm_27_triplet_pt4(a, i, k, l) * wm_interm_45_triplet_pt4(p, k, j, l)
term(100) = term(100) + t2(a,q,j,i) * wm_interm_27_triplet_pt4(p, i, k, l) * wm_interm_45_triplet_pt4(a, k, j, l)
end do 
end do 
end do 
end do 
end do 

term(73) = term(73) * 6.0d+0 
term(74) = term(74) * 16.0d+0 
term(75) = term(75) * 8.0d+0 
term(76) = term(76) * (-4.0d+0) 
term(77) = term(77) * 8.0d+0 
term(78) = term(78) * (-4.0d+0) 
term(79) = term(79) * 8.0d+0 
term(80) = term(80) * (-4.0d+0) 
term(81) = term(81) * (-6.0d+0) 
term(82) = term(82) * (-4.0d+0) 
term(83) = term(83) * 8.0d+0 
term(84) = term(84) * 8.0d+0 
term(85) = term(85) * (-4.0d+0) 
term(86) = term(86) * (-8.0d+0) 
term(87) = term(87) * 8.0d+0 
term(88) = term(88) * (-16.0d+0) 
term(89) = term(89) * (-4.0d+0) 
term(90) = term(90) * 2.0d+0 
term(91) = term(91) * (-4.0d+0) 
term(92) = term(92) * 2.0d+0 
term(93) = term(93) * 8.0d+0 
term(94) = term(94) * (-4.0d+0) 
term(95) = term(95) * (-4.0d+0) 
term(96) = term(96) * 2.0d+0 
term(97) = term(97) * (-4.0d+0) 
term(98) = term(98) * 2.0d+0 
term(99) = term(99) * (-4.0d+0) 
term(100) = term(100) * 2.0d+0 

do j = 1, nocc 
do a = nocc + 1, nactive 
do k = 1, nocc 
do i = 1, nocc 
term(101) = term(101) + r2p(vrdav_Rl, q,i,a,j) * r2p(vrdav_Rr, p,k,a,j) * wm_interm_5_triplet_pt4(k, i)
term(102) = term(102) + r2p(vrdav_Rl, q,i,a,j) * r2m(vrdav_Rr, a,k,p,j) * wm_interm_5_triplet_pt4(k, i)
term(103) = term(103) + r2m(vrdav_Rl, a,i,q,j) * r2p(vrdav_Rr, p,k,a,j) * wm_interm_5_triplet_pt4(k, i)
end do 
end do 
end do 
end do 

term(101) = term(101) * (-12.0d+0) 
term(102) = term(102) * 12.0d+0 
term(103) = term(103) * 6.0d+0 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(104) = term(104) + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, p,j,b,i) * s2(b,q,k,j) * wm_interm_42_triplet_pt4(a, k)
term(105) = term(105) + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, p,j,b,i) * s2(b,q,k,j) * wm_interm_48_triplet_pt4(a, k)
term(106) = term(106) + r2m(vrdav_Rr, a,j,p,i) * s1(q,i) * s2(a,b,k,j) * wm_interm_47_triplet_pt4(b, k)
term(107) = term(107) + r2m(vrdav_Rr, a,j,p,i) * s1(q,i) * s2(a,b,k,j) * wm_interm_52_triplet_pt4(b, k)
term(108) = term(108) + r1(vrdav_Rr, p,i) * s2(a,q,j,i) * t2(a,b,k,j) * wm_interm_0_triplet_pt4(b, k)
term(109) = term(109) + s2(a,p,j,i) * t1(q,i) * t2(a,b,k,j) * wm_interm_86_triplet_pt4(b, k)
term(110) = term(110) + r1(vrdav_Rr, p,i) * s2(a,q,j,i) * t2(a,b,k,j) * wm_interm_20_triplet_pt4(b, k)
term(111) = term(111) + r1(vrdav_Rr, p,i) * s2(a,q,j,i) * t2(a,b,k,j) * wm_interm_21_triplet_pt4(b, k)
term(112) = term(112) + r2m(vrdav_Rl, a,j,p,i) * t1(q,i) * t2(a,b,k,j) * wm_interm_44_triplet_pt4(b, k)
term(113) = term(113) + r2m(vrdav_Rl, a,j,p,i) * t1(q,i) * t2(a,b,k,j) * wm_interm_46_triplet_pt4(b, k)
term(114) = term(114) + s2(a,p,j,i) * t1(q,i) * t2(a,b,k,j) * wm_interm_93_triplet_pt4(b, k)
term(115) = term(115) + s2(a,p,j,i) * t1(q,i) * t2(a,b,k,j) * wm_interm_96_triplet_pt4(b, k)
end do 
end do 
end do 
end do 
end do 

term(104) = term(104) * 12.0d+0 
term(105) = term(105) * (-24.0d+0) 
term(106) = term(106) * (-8.0d+0) 
term(107) = term(107) * 16.0d+0 
term(108) = term(108) * 8.0d+0 
term(109) = term(109) * 12.0d+0 
term(110) = term(110) * 32.0d+0 
term(111) = term(111) * (-16.0d+0) 
term(112) = term(112) * (-8.0d+0) 
term(113) = term(113) * 16.0d+0 
term(114) = term(114) * 16.0d+0 
term(115) = term(115) * (-8.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(116) = term(116) + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, p,j,b,i) * s2(b,q,j,k) * wm_interm_42_triplet_pt4(a, k)
term(117) = term(117) + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, p,j,b,i) * s2(b,q,j,k) * wm_interm_48_triplet_pt4(a, k)
term(118) = term(118) + r2m(vrdav_Rr, a,j,p,i) * s1(q,i) * s2(a,b,j,k) * wm_interm_47_triplet_pt4(b, k)
term(119) = term(119) + r2m(vrdav_Rr, a,j,p,i) * s1(q,i) * s2(a,b,j,k) * wm_interm_52_triplet_pt4(b, k)
term(120) = term(120) + r1(vrdav_Rl, p,i) * s2(a,b,j,k) * t2(b,q,k,i) * wm_interm_2_triplet_pt4(a, j)
term(121) = term(121) + r1(vrdav_Rl, p,i) * s2(a,b,j,k) * t2(b,q,k,i) * wm_interm_3_triplet_pt4(a, j)
term(122) = term(122) + r1(vrdav_Rl, p,i) * s2(a,b,j,k) * t2(b,q,j,i) * wm_interm_2_triplet_pt4(a, k)
term(123) = term(123) + r1(vrdav_Rl, p,i) * s2(a,b,j,k) * t2(b,q,j,i) * wm_interm_3_triplet_pt4(a, k)
term(124) = term(124) + s1(p,i) * s2(a,b,j,k) * t2(b,q,k,i) * wm_interm_82_triplet_pt4(a, j)
term(125) = term(125) + s1(p,i) * s2(a,b,j,k) * t2(b,q,j,i) * wm_interm_82_triplet_pt4(a, k)
term(126) = term(126) + s1(p,i) * s2(a,b,j,k) * t2(b,q,k,i) * wm_interm_84_triplet_pt4(a, j)
term(127) = term(127) + s1(p,i) * s2(a,b,j,k) * t2(b,q,k,i) * wm_interm_83_triplet_pt4(a, j)
term(128) = term(128) + s1(p,i) * s2(a,b,j,k) * t2(b,q,j,i) * wm_interm_84_triplet_pt4(a, k)
term(129) = term(129) + s1(p,i) * s2(a,b,j,k) * t2(b,q,j,i) * wm_interm_83_triplet_pt4(a, k)
term(130) = term(130) + r1(vrdav_Rr, p,i) * s2(a,q,j,i) * t2(a,b,j,k) * wm_interm_0_triplet_pt4(b, k)
term(131) = term(131) + s2(a,p,j,i) * t1(q,i) * t2(a,b,j,k) * wm_interm_86_triplet_pt4(b, k)
term(132) = term(132) + r1(vrdav_Rr, p,i) * s2(a,q,j,i) * t2(a,b,j,k) * wm_interm_20_triplet_pt4(b, k)
term(133) = term(133) + r1(vrdav_Rr, p,i) * s2(a,q,j,i) * t2(a,b,j,k) * wm_interm_21_triplet_pt4(b, k)
term(134) = term(134) + r2m(vrdav_Rl, a,j,p,i) * t1(q,i) * t2(a,b,j,k) * wm_interm_46_triplet_pt4(b, k)
term(135) = term(135) + s2(a,p,j,i) * t1(q,i) * t2(a,b,j,k) * wm_interm_93_triplet_pt4(b, k)
term(136) = term(136) + r2m(vrdav_Rl, a,j,p,i) * t1(q,i) * t2(a,b,j,k) * wm_interm_44_triplet_pt4(b, k)
term(137) = term(137) + s2(a,p,j,i) * t1(q,i) * t2(a,b,j,k) * wm_interm_96_triplet_pt4(b, k)
end do 
end do 
end do 
end do 
end do 

term(116) = term(116) * (-12.0d+0) 
term(117) = term(117) * 24.0d+0 
term(118) = term(118) * 16.0d+0 
term(119) = term(119) * (-32.0d+0) 
term(120) = term(120) * 32.0d+0 
term(121) = term(121) * (-16.0d+0) 
term(122) = term(122) * (-16.0d+0) 
term(123) = term(123) * 8.0d+0 
term(124) = term(124) * 48.0d+0 
term(125) = term(125) * (-24.0d+0) 
term(126) = term(126) * (-32.0d+0) 
term(127) = term(127) * 64.0d+0 
term(128) = term(128) * 16.0d+0 
term(129) = term(129) * (-32.0d+0) 
term(130) = term(130) * (-16.0d+0) 
term(131) = term(131) * (-24.0d+0) 
term(132) = term(132) * (-64.0d+0) 
term(133) = term(133) * 32.0d+0 
term(134) = term(134) * (-32.0d+0) 
term(135) = term(135) * (-32.0d+0) 
term(136) = term(136) * 16.0d+0 
term(137) = term(137) * 16.0d+0 

do i = 1, nocc 
do a = nocc + 1, nactive 
do k = 1, nocc 
do j = 1, nocc 
term(138) = term(138) + r2m(vrdav_Rl, a,j,q,i) * r2p(vrdav_Rr, p,k,a,i) * wm_interm_5_triplet_pt4(k, j)
end do 
end do 
end do 
end do 

term(138) = term(138) * 6.0d+0 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
do k = 1, nocc 
term(139) = term(139) + r2m(vrdav_Rl, a,i,q,j) * r2p(vrdav_Rr, p,k,a,i) * wm_interm_5_triplet_pt4(k, j)
term(140) = term(140) + r2m(vrdav_Rl, a,j,q,i) * r2p(vrdav_Rr, p,k,a,j) * wm_interm_5_triplet_pt4(k, i)
end do 
end do 
end do 
end do 

term(139) = term(139) * (-6.0d+0) 
term(140) = term(140) * (-6.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(141) = term(141) + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, b,j,p,i) * s2(b,q,k,j) * wm_interm_42_triplet_pt4(a, k)
term(142) = term(142) + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, b,j,p,i) * s2(b,q,k,j) * wm_interm_48_triplet_pt4(a, k)
term(143) = term(143) + r1(vrdav_Rl, a,i) * s2(b,p,k,j) * t2(b,q,j,i) * wm_interm_1_triplet_pt4(a, k)
term(144) = term(144) + r1(vrdav_Rl, a,i) * s2(b,p,k,j) * t2(b,q,k,i) * wm_interm_1_triplet_pt4(a, j)
term(145) = term(145) + r1(vrdav_Rl, a,i) * s2(b,p,k,j) * t2(b,q,j,i) * wm_interm_2_triplet_pt4(a, k)
term(146) = term(146) + r1(vrdav_Rl, a,i) * s2(b,p,k,j) * t2(b,q,k,i) * wm_interm_2_triplet_pt4(a, j)
term(147) = term(147) + r1(vrdav_Rl, a,i) * s2(b,p,k,j) * t2(b,q,j,i) * wm_interm_3_triplet_pt4(a, k)
term(148) = term(148) + r1(vrdav_Rl, a,i) * s2(b,p,k,j) * t2(b,q,k,i) * wm_interm_3_triplet_pt4(a, j)
term(149) = term(149) + r1(vrdav_Rr, a,j) * s2(b,p,k,j) * t2(b,q,k,i) * wm_interm_20_triplet_pt4(a, i)
term(150) = term(150) + r1(vrdav_Rr, a,j) * s2(b,p,k,j) * t2(b,q,k,i) * wm_interm_21_triplet_pt4(a, i)
term(151) = term(151) + r1(vrdav_Rr, a,j) * s2(b,p,i,j) * t2(b,q,k,i) * wm_interm_20_triplet_pt4(a, k)
term(152) = term(152) + r1(vrdav_Rr, a,j) * s2(b,p,i,j) * t2(b,q,k,i) * wm_interm_21_triplet_pt4(a, k)
end do 
end do 
end do 
end do 
end do 

term(141) = term(141) * (-8.0d+0) 
term(142) = term(142) * 16.0d+0 
term(143) = term(143) * 6.0d+0 
term(144) = term(144) * (-12.0d+0) 
term(145) = term(145) * 8.0d+0 
term(146) = term(146) * (-16.0d+0) 
term(147) = term(147) * (-4.0d+0) 
term(148) = term(148) * 8.0d+0 
term(149) = term(149) * 16.0d+0 
term(150) = term(150) * (-8.0d+0) 
term(151) = term(151) * (-8.0d+0) 
term(152) = term(152) * 4.0d+0 

do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(153) = term(153) + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, b,i,p,j) * s2(b,q,k,j) * wm_interm_42_triplet_pt4(a, k)
term(154) = term(154) + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, b,i,p,j) * s2(b,q,k,j) * wm_interm_48_triplet_pt4(a, k)
end do 
end do 
end do 
end do 
end do 

term(153) = term(153) * 16.0d+0 
term(154) = term(154) * (-32.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(155) = term(155) + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, b,i,p,j) * s2(b,q,j,k) * wm_interm_42_triplet_pt4(a, k)
term(156) = term(156) + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, b,i,p,j) * s2(b,q,j,k) * wm_interm_48_triplet_pt4(a, k)
end do 
end do 
end do 
end do 
end do 

term(155) = term(155) * (-8.0d+0) 
term(156) = term(156) * 16.0d+0 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(157) = term(157) + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, b,j,p,i) * s2(b,q,j,k) * wm_interm_42_triplet_pt4(a, k)
term(158) = term(158) + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, b,j,p,i) * s2(b,q,j,k) * wm_interm_48_triplet_pt4(a, k)
term(159) = term(159) + r1(vrdav_Rl, a,j) * s2(b,p,i,k) * t2(b,q,j,i) * wm_interm_1_triplet_pt4(a, k)
term(160) = term(160) + r1(vrdav_Rl, a,j) * s2(b,p,i,k) * t2(b,q,j,i) * wm_interm_2_triplet_pt4(a, k)
term(161) = term(161) + r1(vrdav_Rl, a,j) * s2(b,p,i,k) * t2(b,q,j,i) * wm_interm_3_triplet_pt4(a, k)
term(162) = term(162) + r1(vrdav_Rr, a,j) * s2(b,p,j,k) * t2(b,q,k,i) * wm_interm_20_triplet_pt4(a, i)
term(163) = term(163) + r1(vrdav_Rr, a,j) * s2(b,p,j,k) * t2(b,q,k,i) * wm_interm_21_triplet_pt4(a, i)
end do 
end do 
end do 
end do 
end do 

term(157) = term(157) * 16.0d+0 
term(158) = term(158) * (-32.0d+0) 
term(159) = term(159) * 6.0d+0 
term(160) = term(160) * 8.0d+0 
term(161) = term(161) * (-4.0d+0) 
term(162) = term(162) * (-8.0d+0) 
term(163) = term(163) * 4.0d+0 

do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(164) = term(164) + r2m(vrdav_Rl, a,i,q,j) * r2m(vrdav_Rr, a,k,p,i) * wm_interm_5_triplet_pt4(k, j)
term(165) = term(165) + r2m(vrdav_Rl, a,j,q,i) * r2m(vrdav_Rr, a,k,p,j) * wm_interm_5_triplet_pt4(k, i)
term(166) = term(166) + s2(a,q,j,i) * wm_interm_42_triplet_pt4(a, k) * wm_interm_64_triplet_pt4(p, k, i, j)
term(167) = term(167) + s2(a,q,j,i) * wm_interm_48_triplet_pt4(a, k) * wm_interm_64_triplet_pt4(p, k, i, j)
term(168) = term(168) + s2(a,q,j,i) * wm_interm_42_triplet_pt4(a, k) * wm_interm_71_triplet_pt4(p, k, i, j)
term(169) = term(169) + s2(a,q,j,i) * wm_interm_48_triplet_pt4(a, k) * wm_interm_71_triplet_pt4(p, k, i, j)
term(170) = term(170) + s2(a,p,j,i) * wm_interm_2_triplet_pt4(a, k) * wm_interm_51_triplet_pt4(q, k, i, j)
term(171) = term(171) + s2(a,p,j,i) * wm_interm_3_triplet_pt4(a, k) * wm_interm_51_triplet_pt4(q, k, i, j)
term(172) = term(172) + s2(a,p,j,i) * wm_interm_43_triplet_pt4(q, k, i, j) * wm_interm_82_triplet_pt4(a, k)
term(173) = term(173) + s2(a,p,j,i) * wm_interm_43_triplet_pt4(q, k, i, j) * wm_interm_84_triplet_pt4(a, k)
term(174) = term(174) + s2(a,p,j,i) * wm_interm_43_triplet_pt4(q, k, i, j) * wm_interm_83_triplet_pt4(a, k)
term(175) = term(175) + t2(a,q,j,i) * wm_interm_20_triplet_pt4(a, k) * wm_interm_45_triplet_pt4(p, k, i, j)
term(176) = term(176) + t2(a,q,j,i) * wm_interm_21_triplet_pt4(a, k) * wm_interm_45_triplet_pt4(p, k, i, j)
end do 
end do 
end do 
end do 

term(164) = term(164) * 4.0d+0 
term(165) = term(165) * 4.0d+0 
term(166) = term(166) * (-6.0d+0) 
term(167) = term(167) * 12.0d+0 
term(168) = term(168) * (-8.0d+0) 
term(169) = term(169) * 16.0d+0 
term(170) = term(170) * 8.0d+0 
term(171) = term(171) * (-4.0d+0) 
term(172) = term(172) * 12.0d+0 
term(173) = term(173) * (-8.0d+0) 
term(174) = term(174) * 16.0d+0 
term(175) = term(175) * (-8.0d+0) 
term(176) = term(176) * 4.0d+0 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(177) = term(177) + r2m(vrdav_Rl, a,i,q,j) * r2m(vrdav_Rr, a,i,p,k) * wm_interm_5_triplet_pt4(k, j)
term(178) = term(178) + r2m(vrdav_Rl, a,i,q,j) * r2m(vrdav_Rr, a,j,p,k) * wm_interm_5_triplet_pt4(k, i)
end do 
end do 
end do 
end do 

term(177) = term(177) * (-8.0d+0) 
term(178) = term(178) * 4.0d+0 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do k = 1, nocc 
term(179) = term(179) + r2p(vrdav_Rr, p,j,a,i) * s1(q,i) * s2(a,b,k,j) * wm_interm_47_triplet_pt4(b, k)
term(180) = term(180) + r2p(vrdav_Rr, p,j,a,i) * s1(q,i) * s2(a,b,k,j) * wm_interm_52_triplet_pt4(b, k)
term(181) = term(181) + r2p(vrdav_Rl, p,j,a,i) * t1(q,i) * t2(a,b,k,j) * wm_interm_44_triplet_pt4(b, k)
term(182) = term(182) + r2p(vrdav_Rl, p,j,a,i) * t1(q,i) * t2(a,b,k,j) * wm_interm_46_triplet_pt4(b, k)
end do 
end do 
end do 
end do 
end do 

term(179) = term(179) * 6.0d+0 
term(180) = term(180) * (-12.0d+0) 
term(181) = term(181) * 3.0d+0 
term(182) = term(182) * (-6.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do j = 1, nocc 
term(183) = term(183) + r2p(vrdav_Rr, p,j,a,i) * s1(q,i) * s2(a,b,j,k) * wm_interm_47_triplet_pt4(b, k)
term(184) = term(184) + r2p(vrdav_Rr, p,j,a,i) * s1(q,i) * s2(a,b,j,k) * wm_interm_52_triplet_pt4(b, k)
term(185) = term(185) + r2p(vrdav_Rl, p,j,a,i) * t1(q,i) * t2(a,b,j,k) * wm_interm_46_triplet_pt4(b, k)
term(186) = term(186) + r2p(vrdav_Rl, p,j,a,i) * t1(q,i) * t2(a,b,j,k) * wm_interm_44_triplet_pt4(b, k)
end do 
end do 
end do 
end do 
end do 

term(183) = term(183) * (-12.0d+0) 
term(184) = term(184) * 24.0d+0 
term(185) = term(185) * 12.0d+0 
term(186) = term(186) * (-6.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(187) = term(187) + s2(a,q,j,i) * wm_interm_42_triplet_pt4(a, k) * wm_interm_64_triplet_pt4(p, k, j, i)
term(188) = term(188) + s2(a,q,j,i) * wm_interm_48_triplet_pt4(a, k) * wm_interm_64_triplet_pt4(p, k, j, i)
term(189) = term(189) + s2(a,q,j,i) * wm_interm_42_triplet_pt4(a, k) * wm_interm_71_triplet_pt4(p, k, j, i)
term(190) = term(190) + s2(a,q,j,i) * wm_interm_48_triplet_pt4(a, k) * wm_interm_71_triplet_pt4(p, k, j, i)
term(191) = term(191) + s2(a,p,j,i) * wm_interm_2_triplet_pt4(a, k) * wm_interm_51_triplet_pt4(q, k, j, i)
term(192) = term(192) + s2(a,p,j,i) * wm_interm_3_triplet_pt4(a, k) * wm_interm_51_triplet_pt4(q, k, j, i)
term(193) = term(193) + s2(a,p,j,i) * wm_interm_43_triplet_pt4(q, k, j, i) * wm_interm_82_triplet_pt4(a, k)
term(194) = term(194) + s2(a,p,j,i) * wm_interm_43_triplet_pt4(q, k, j, i) * wm_interm_84_triplet_pt4(a, k)
term(195) = term(195) + s2(a,p,j,i) * wm_interm_43_triplet_pt4(q, k, j, i) * wm_interm_83_triplet_pt4(a, k)
term(196) = term(196) + t2(a,q,j,i) * wm_interm_20_triplet_pt4(a, k) * wm_interm_45_triplet_pt4(p, k, j, i)
term(197) = term(197) + t2(a,q,j,i) * wm_interm_21_triplet_pt4(a, k) * wm_interm_45_triplet_pt4(p, k, j, i)
end do 
end do 
end do 
end do 

term(187) = term(187) * 6.0d+0 
term(188) = term(188) * (-12.0d+0) 
term(189) = term(189) * 16.0d+0 
term(190) = term(190) * (-32.0d+0) 
term(191) = term(191) * (-16.0d+0) 
term(192) = term(192) * 8.0d+0 
term(193) = term(193) * (-24.0d+0) 
term(194) = term(194) * 16.0d+0 
term(195) = term(195) * (-32.0d+0) 
term(196) = term(196) * 16.0d+0 
term(197) = term(197) * (-8.0d+0) 

do j = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(198) = term(198) + r2m(vrdav_Rr, a,i,p,j) * s1(q,i) * s2(a,b,k,j) * wm_interm_47_triplet_pt4(b, k)
term(199) = term(199) + r2m(vrdav_Rr, a,i,p,j) * s1(q,i) * s2(a,b,k,j) * wm_interm_52_triplet_pt4(b, k)
term(200) = term(200) + r1(vrdav_Rl, p,i) * s2(a,b,k,j) * t2(b,q,i,j) * wm_interm_2_triplet_pt4(a, k)
term(201) = term(201) + r1(vrdav_Rl, p,i) * s2(a,b,k,j) * t2(b,q,i,j) * wm_interm_3_triplet_pt4(a, k)
term(202) = term(202) + s1(p,i) * s2(a,b,k,j) * t2(b,q,i,j) * wm_interm_82_triplet_pt4(a, k)
term(203) = term(203) + s1(p,i) * s2(a,b,k,j) * t2(b,q,i,j) * wm_interm_84_triplet_pt4(a, k)
term(204) = term(204) + s1(p,i) * s2(a,b,k,j) * t2(b,q,i,j) * wm_interm_83_triplet_pt4(a, k)
term(205) = term(205) + r1(vrdav_Rr, p,i) * s2(a,q,i,j) * t2(a,b,k,j) * wm_interm_0_triplet_pt4(b, k)
term(206) = term(206) + s2(a,p,i,j) * t1(q,i) * t2(a,b,k,j) * wm_interm_86_triplet_pt4(b, k)
term(207) = term(207) + r1(vrdav_Rr, p,i) * s2(a,q,i,j) * t2(a,b,k,j) * wm_interm_20_triplet_pt4(b, k)
term(208) = term(208) + r1(vrdav_Rr, p,i) * s2(a,q,i,j) * t2(a,b,k,j) * wm_interm_21_triplet_pt4(b, k)
term(209) = term(209) + s2(a,p,i,j) * t1(q,i) * t2(a,b,k,j) * wm_interm_93_triplet_pt4(b, k)
term(210) = term(210) + s2(a,p,i,j) * t1(q,i) * t2(a,b,k,j) * wm_interm_96_triplet_pt4(b, k)
term(211) = term(211) + r2m(vrdav_Rl, a,i,p,j) * t1(q,i) * t2(a,b,k,j) * wm_interm_44_triplet_pt4(b, k)
term(212) = term(212) + r2m(vrdav_Rl, a,i,p,j) * t1(q,i) * t2(a,b,k,j) * wm_interm_46_triplet_pt4(b, k)
end do 
end do 
end do 
end do 
end do 

term(198) = term(198) * 4.0d+0 
term(199) = term(199) * (-8.0d+0) 
term(200) = term(200) * (-16.0d+0) 
term(201) = term(201) * 8.0d+0 
term(202) = term(202) * (-24.0d+0) 
term(203) = term(203) * 16.0d+0 
term(204) = term(204) * (-32.0d+0) 
term(205) = term(205) * (-4.0d+0) 
term(206) = term(206) * (-6.0d+0) 
term(207) = term(207) * (-16.0d+0) 
term(208) = term(208) * 8.0d+0 
term(209) = term(209) * (-8.0d+0) 
term(210) = term(210) * 4.0d+0 
term(211) = term(211) * 4.0d+0 
term(212) = term(212) * (-8.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do j = 1, nocc 
term(213) = term(213) + s1(q,i) * wm_interm_49_triplet_pt4(j, k, l, i) * wm_interm_64_triplet_pt4(p, l, j, k)
term(214) = term(214) + s1(q,i) * wm_interm_49_triplet_pt4(j, k, l, i) * wm_interm_71_triplet_pt4(p, l, j, k)
end do 
end do 
end do 
end do 

term(213) = term(213) * 6.0d+0 
term(214) = term(214) * 16.0d+0 

do l = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(215) = term(215) + r1(vrdav_Rl, q,i) * wm_interm_49_triplet_pt4(j, k, i, l) * wm_interm_8_triplet_pt4(p, j, k, l)
term(216) = term(216) + r1(vrdav_Rl, q,i) * wm_interm_14_triplet_pt4(p, j, k, l) * wm_interm_49_triplet_pt4(k, j, i, l)
term(217) = term(217) + r1(vrdav_Rl, q,i) * wm_interm_14_triplet_pt4(p, j, k, l) * wm_interm_49_triplet_pt4(j, k, i, l)
end do 
end do 
end do 
end do 

term(215) = term(215) * 6.0d+0 
term(216) = term(216) * (-8.0d+0) 
term(217) = term(217) * 16.0d+0 

do j = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do k = 1, nocc 
term(218) = term(218) + s1(q,i) * wm_interm_49_triplet_pt4(j, k, l, i) * wm_interm_64_triplet_pt4(p, l, k, j)
term(219) = term(219) + s1(q,i) * wm_interm_49_triplet_pt4(j, k, l, i) * wm_interm_71_triplet_pt4(p, l, k, j)
end do 
end do 
end do 
end do 

term(218) = term(218) * (-6.0d+0) 
term(219) = term(219) * (-8.0d+0) 

do l = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
term(220) = term(220) + r1(vrdav_Rl, q,i) * wm_interm_49_triplet_pt4(j, k, i, l) * wm_interm_8_triplet_pt4(p, k, j, l)
end do 
end do 
end do 
end do 

term(220) = term(220) * (-6.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(221) = term(221) + r1(vrdav_Rl, a,j) * s2(b,p,k,i) * t2(b,q,j,i) * wm_interm_1_triplet_pt4(a, k)
term(222) = term(222) + r1(vrdav_Rl, a,j) * s2(b,p,k,i) * t2(b,q,j,i) * wm_interm_2_triplet_pt4(a, k)
term(223) = term(223) + r1(vrdav_Rl, a,j) * s2(b,p,k,i) * t2(b,q,j,i) * wm_interm_3_triplet_pt4(a, k)
term(224) = term(224) + r1(vrdav_Rr, a,j) * s2(b,p,j,i) * t2(b,q,k,i) * wm_interm_20_triplet_pt4(a, k)
term(225) = term(225) + r1(vrdav_Rr, a,j) * s2(b,p,j,i) * t2(b,q,k,i) * wm_interm_21_triplet_pt4(a, k)
end do 
end do 
end do 
end do 
end do 

term(221) = term(221) * (-12.0d+0) 
term(222) = term(222) * (-16.0d+0) 
term(223) = term(223) * 8.0d+0 
term(224) = term(224) * 16.0d+0 
term(225) = term(225) * (-8.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(226) = term(226) + s1(p,i) * wm_interm_51_triplet_pt4(q, j, k, l) * wm_interm_66_triplet_pt4(j, i, k, l)
term(227) = term(227) + s1(p,i) * wm_interm_51_triplet_pt4(q, j, k, l) * wm_interm_66_triplet_pt4(j, i, l, k)
term(228) = term(228) + r1(vrdav_Rl, p,i) * wm_interm_43_triplet_pt4(q, j, k, l) * wm_interm_66_triplet_pt4(i, j, k, l)
term(229) = term(229) + r1(vrdav_Rl, p,i) * wm_interm_43_triplet_pt4(q, j, k, l) * wm_interm_66_triplet_pt4(i, j, l, k)
term(230) = term(230) + s1(p,i) * wm_interm_51_triplet_pt4(q, j, k, l) * wm_interm_74_triplet_pt4(i, j, k, l)
term(231) = term(231) + s1(p,i) * wm_interm_51_triplet_pt4(q, j, k, l) * wm_interm_74_triplet_pt4(j, i, k, l)
term(232) = term(232) + s1(p,i) * wm_interm_51_triplet_pt4(q, j, k, l) * wm_interm_74_triplet_pt4(j, i, l, k)
term(233) = term(233) + s1(p,i) * wm_interm_51_triplet_pt4(q, j, k, l) * wm_interm_74_triplet_pt4(i, j, l, k)
term(234) = term(234) + r1(vrdav_Rl, p,i) * wm_interm_43_triplet_pt4(q, j, k, l) * wm_interm_74_triplet_pt4(i, j, k, l)
term(235) = term(235) + r1(vrdav_Rl, p,i) * wm_interm_43_triplet_pt4(q, j, k, l) * wm_interm_74_triplet_pt4(j, i, k, l)
term(236) = term(236) + r1(vrdav_Rl, p,i) * wm_interm_43_triplet_pt4(q, j, k, l) * wm_interm_74_triplet_pt4(j, i, l, k)
term(237) = term(237) + r1(vrdav_Rl, p,i) * wm_interm_43_triplet_pt4(q, j, k, l) * wm_interm_74_triplet_pt4(i, j, l, k)
term(238) = term(238) + r1(vrdav_Rr, p,i) * wm_interm_60_triplet_pt4(q, j, k, l) * wm_interm_88_triplet_pt4(i, l, j, k)
term(239) = term(239) + t1(q,i) * wm_interm_45_triplet_pt4(p, j, k, l) * wm_interm_88_triplet_pt4(j, i, k, l)
term(240) = term(240) + t1(q,i) * wm_interm_45_triplet_pt4(p, j, k, l) * wm_interm_88_triplet_pt4(j, i, l, k)
term(241) = term(241) + r1(vrdav_Rr, p,i) * wm_interm_60_triplet_pt4(q, j, k, l) * wm_interm_97_triplet_pt4(i, l, j, k)
term(242) = term(242) + r1(vrdav_Rr, p,i) * wm_interm_24_triplet_pt4(q, j, k, l) * wm_interm_49_triplet_pt4(l, i, j, k)
term(243) = term(243) + r1(vrdav_Rr, p,i) * wm_interm_24_triplet_pt4(q, j, k, l) * wm_interm_49_triplet_pt4(i, l, j, k)
term(244) = term(244) + r1(vrdav_Rr, p,i) * wm_interm_60_triplet_pt4(q, j, k, l) * wm_interm_97_triplet_pt4(l, i, j, k)
term(245) = term(245) + r1(vrdav_Rr, p,i) * wm_interm_27_triplet_pt4(q, j, k, l) * wm_interm_49_triplet_pt4(l, i, j, k)
term(246) = term(246) + r1(vrdav_Rr, p,i) * wm_interm_27_triplet_pt4(q, j, k, l) * wm_interm_49_triplet_pt4(i, l, j, k)
term(247) = term(247) + t1(q,i) * wm_interm_45_triplet_pt4(p, j, k, l) * wm_interm_97_triplet_pt4(i, j, k, l)
term(248) = term(248) + t1(q,i) * wm_interm_45_triplet_pt4(p, j, k, l) * wm_interm_97_triplet_pt4(j, i, k, l)
term(249) = term(249) + t1(q,i) * wm_interm_45_triplet_pt4(p, j, k, l) * wm_interm_97_triplet_pt4(j, i, l, k)
term(250) = term(250) + t1(q,i) * wm_interm_45_triplet_pt4(p, j, k, l) * wm_interm_97_triplet_pt4(i, j, l, k)
end do 
end do 
end do 
end do 

term(226) = term(226) * 6.0d+0 
term(227) = term(227) * (-6.0d+0) 
term(228) = term(228) * (-6.0d+0) 
term(229) = term(229) * 6.0d+0 
term(230) = term(230) * (-4.0d+0) 
term(231) = term(231) * 8.0d+0 
term(232) = term(232) * (-4.0d+0) 
term(233) = term(233) * 8.0d+0 
term(234) = term(234) * (-4.0d+0) 
term(235) = term(235) * 8.0d+0 
term(236) = term(236) * (-4.0d+0) 
term(237) = term(237) * 8.0d+0 
term(238) = term(238) * (-6.0d+0) 
term(239) = term(239) * (-3.0d+0) 
term(240) = term(240) * 3.0d+0 
term(241) = term(241) * (-8.0d+0) 
term(242) = term(242) * 4.0d+0 
term(243) = term(243) * (-8.0d+0) 
term(244) = term(244) * 4.0d+0 
term(245) = term(245) * (-8.0d+0) 
term(246) = term(246) * 4.0d+0 
term(247) = term(247) * 2.0d+0 
term(248) = term(248) * (-4.0d+0) 
term(249) = term(249) * 2.0d+0 
term(250) = term(250) * (-4.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
do j = 1, nocc 
term(251) = term(251) + r2p(vrdav_Rr, p,j,a,i) * s1(q,i) * wm_interm_0_triplet_pt4(a, j)
term(252) = term(252) + r2p(vrdav_Rl, p,j,a,i) * t1(q,i) * wm_interm_1_triplet_pt4(a, j)
term(253) = term(253) + r2p(vrdav_Rl, p,j,a,i) * t1(q,i) * wm_interm_2_triplet_pt4(a, j)
term(254) = term(254) + r2p(vrdav_Rl, p,j,a,i) * t1(q,i) * wm_interm_3_triplet_pt4(a, j)
term(255) = term(255) + r2p(vrdav_Rr, p,j,a,i) * s1(q,i) * wm_interm_20_triplet_pt4(a, j)
term(256) = term(256) + r2p(vrdav_Rr, p,j,a,i) * s1(q,i) * wm_interm_21_triplet_pt4(a, j)
end do 
end do 
end do 

term(251) = term(251) * (-18.0d+0) 
term(252) = term(252) * 18.0d+0 
term(253) = term(253) * 24.0d+0 
term(254) = term(254) * (-12.0d+0) 
term(255) = term(255) * (-24.0d+0) 
term(256) = term(256) * 12.0d+0 

do k = 1, nocc 
do j = 1, nocc 
do l = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(257) = term(257) + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, b,i,p,j) * s2(b,q,l,k) * wm_interm_69_triplet_pt4(a, j, l, k)
end do 
end do 
end do 
end do 
end do 
end do 

term(257) = term(257) * 16.0d+0 

do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do k = 1, nocc 
do i = 1, nocc 
term(258) = term(258) + r2p(vrdav_Rl, p,i,a,j) * t1(q,i) * t2(a,b,k,j) * wm_interm_44_triplet_pt4(b, k)
term(259) = term(259) + r2p(vrdav_Rl, p,i,a,j) * t1(q,i) * t2(a,b,k,j) * wm_interm_46_triplet_pt4(b, k)
end do 
end do 
end do 
end do 
end do 

term(258) = term(258) * (-3.0d+0) 
term(259) = term(259) * 6.0d+0 

do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(260) = term(260) + r2p(vrdav_Rl, p,i,a,j) * t1(q,i) * t2(a,b,j,k) * wm_interm_46_triplet_pt4(b, k)
term(261) = term(261) + r2p(vrdav_Rl, p,i,a,j) * t1(q,i) * t2(a,b,j,k) * wm_interm_44_triplet_pt4(b, k)
end do 
end do 
end do 
end do 
end do 

term(260) = term(260) * (-12.0d+0) 
term(261) = term(261) * 6.0d+0 

do l = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(262) = term(262) + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, b,i,p,j) * s2(b,q,l,k) * wm_interm_69_triplet_pt4(a, j, k, l)
term(263) = term(263) + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, b,k,p,j) * s2(b,q,l,k) * wm_interm_69_triplet_pt4(a, j, i, l)
term(264) = term(264) + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, b,j,p,k) * s2(b,q,l,j) * wm_interm_69_triplet_pt4(a, k, i, l)
end do 
end do 
end do 
end do 
end do 
end do 

term(262) = term(262) * (-8.0d+0) 
term(263) = term(263) * 8.0d+0 
term(264) = term(264) * 8.0d+0 

term(265) = term(265) + wm_interm_10_triplet_pt4(p, q) * wm_interm_17_triplet_pt4
term(266) = term(266) + wm_interm_10_triplet_pt4(p, q) * wm_interm_18_triplet_pt4
term(267) = term(267) + wm_interm_10_triplet_pt4(p, q) * wm_interm_19_triplet_pt4
term(268) = term(268) + wm_interm_10_triplet_pt4(p, q) * wm_interm_38_triplet_pt4
term(269) = term(269) + wm_interm_10_triplet_pt4(p, q) * wm_interm_39_triplet_pt4
term(270) = term(270) + wm_interm_10_triplet_pt4(p, q) * wm_interm_40_triplet_pt4
term(271) = term(271) + wm_interm_10_triplet_pt4(p, q) * wm_interm_63_triplet_pt4

term(265) = term(265) * 6.0d+0 
term(266) = term(266) * (-4.0d+0) 
term(267) = term(267) * 8.0d+0 
term(268) = term(268) * 12.0d+0 
term(269) = term(269) * (-8.0d+0) 
term(270) = term(270) * 16.0d+0 
term(271) = term(271) * 4.0d+0 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do a = nocc + 1, nactive 
term(272) = term(272) + s2(a,q,j,i) * wm_interm_43_triplet_pt4(a, k, l, j) * wm_interm_64_triplet_pt4(p, l, i, k)
term(273) = term(273) + s2(a,q,j,i) * wm_interm_43_triplet_pt4(a, k, l, j) * wm_interm_71_triplet_pt4(p, l, i, k)
term(274) = term(274) + s2(a,p,j,i) * wm_interm_43_triplet_pt4(q, k, l, j) * wm_interm_64_triplet_pt4(a, l, i, k)
term(275) = term(275) + s2(a,p,j,i) * wm_interm_43_triplet_pt4(q, k, l, j) * wm_interm_71_triplet_pt4(a, l, i, k)
end do 
end do 
end do 
end do 
end do 

term(272) = term(272) * 18.0d+0 
term(273) = term(273) * 16.0d+0 
term(274) = term(274) * (-6.0d+0) 
term(275) = term(275) * (-8.0d+0) 

do i = 1, nocc 
term(276) = term(276) + wm_interm_0_triplet_pt4(q, i) * wm_interm_1_triplet_pt4(p, i)
term(277) = term(277) + wm_interm_0_triplet_pt4(q, i) * wm_interm_2_triplet_pt4(p, i)
term(278) = term(278) + wm_interm_0_triplet_pt4(q, i) * wm_interm_3_triplet_pt4(p, i)
term(279) = term(279) + wm_interm_1_triplet_pt4(p, i) * wm_interm_20_triplet_pt4(q, i)
term(280) = term(280) + wm_interm_1_triplet_pt4(p, i) * wm_interm_21_triplet_pt4(q, i)
term(281) = term(281) + wm_interm_20_triplet_pt4(q, i) * wm_interm_2_triplet_pt4(p, i)
term(282) = term(282) + wm_interm_20_triplet_pt4(q, i) * wm_interm_3_triplet_pt4(p, i)
term(283) = term(283) + wm_interm_21_triplet_pt4(q, i) * wm_interm_2_triplet_pt4(p, i)
term(284) = term(284) + wm_interm_21_triplet_pt4(q, i) * wm_interm_3_triplet_pt4(p, i)
end do 

term(276) = term(276) * 18.0d+0 
term(277) = term(277) * 24.0d+0 
term(278) = term(278) * (-12.0d+0) 
term(279) = term(279) * (-24.0d+0) 
term(280) = term(280) * 12.0d+0 
term(281) = term(281) * (-32.0d+0) 
term(282) = term(282) * 16.0d+0 
term(283) = term(283) * 16.0d+0 
term(284) = term(284) * (-8.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(285) = term(285) + s2(a,q,j,i) * wm_interm_43_triplet_pt4(a, k, i, l) * wm_interm_64_triplet_pt4(p, l, j, k)
term(286) = term(286) + s2(a,q,j,i) * wm_interm_43_triplet_pt4(a, k, j, l) * wm_interm_64_triplet_pt4(p, l, i, k)
term(287) = term(287) + s2(a,q,j,i) * wm_interm_43_triplet_pt4(a, k, i, l) * wm_interm_71_triplet_pt4(p, l, j, k)
term(288) = term(288) + s2(a,q,j,i) * wm_interm_43_triplet_pt4(a, k, j, l) * wm_interm_71_triplet_pt4(p, l, i, k)
term(289) = term(289) + s2(a,p,j,i) * wm_interm_43_triplet_pt4(q, k, j, l) * wm_interm_64_triplet_pt4(a, l, i, k)
term(290) = term(290) + s2(a,p,j,i) * wm_interm_43_triplet_pt4(q, k, i, l) * wm_interm_64_triplet_pt4(a, l, j, k)
term(291) = term(291) + s2(a,p,j,i) * wm_interm_43_triplet_pt4(q, k, j, l) * wm_interm_71_triplet_pt4(a, l, i, k)
term(292) = term(292) + s2(a,p,j,i) * wm_interm_43_triplet_pt4(q, k, i, l) * wm_interm_71_triplet_pt4(a, l, j, k)
term(293) = term(293) + t2(a,q,j,i) * wm_interm_60_triplet_pt4(a, i, k, l) * wm_interm_85_triplet_pt4(p, j, l, k)
term(294) = term(294) + t2(a,q,j,i) * wm_interm_60_triplet_pt4(a, k, i, l) * wm_interm_85_triplet_pt4(p, j, l, k)
term(295) = term(295) + t2(a,q,j,i) * wm_interm_62_triplet_pt4(a, j, k, l) * wm_interm_85_triplet_pt4(p, i, l, k)
term(296) = term(296) + t2(a,q,j,i) * wm_interm_62_triplet_pt4(a, k, j, l) * wm_interm_85_triplet_pt4(p, i, l, k)
term(297) = term(297) + t2(a,q,j,i) * wm_interm_60_triplet_pt4(p, k, j, l) * wm_interm_85_triplet_pt4(a, i, l, k)
term(298) = term(298) + t2(a,q,j,i) * wm_interm_60_triplet_pt4(p, j, k, l) * wm_interm_85_triplet_pt4(a, i, l, k)
term(299) = term(299) + t2(a,q,j,i) * wm_interm_60_triplet_pt4(p, k, i, l) * wm_interm_85_triplet_pt4(a, j, l, k)
term(300) = term(300) + t2(a,q,j,i) * wm_interm_60_triplet_pt4(p, i, k, l) * wm_interm_85_triplet_pt4(a, j, l, k)
term(301) = term(301) + t2(a,q,j,i) * wm_interm_60_triplet_pt4(p, k, i, l) * wm_interm_85_triplet_pt4(a, l, j, k)
term(302) = term(302) + t2(a,q,j,i) * wm_interm_60_triplet_pt4(p, j, k, l) * wm_interm_85_triplet_pt4(a, l, i, k)
term(303) = term(303) + t2(a,q,j,i) * wm_interm_60_triplet_pt4(p, k, j, l) * wm_interm_85_triplet_pt4(a, l, i, k)
term(304) = term(304) + t2(a,q,j,i) * wm_interm_60_triplet_pt4(p, i, k, l) * wm_interm_85_triplet_pt4(a, l, j, k)
term(305) = term(305) + t2(a,q,j,i) * wm_interm_62_triplet_pt4(a, k, i, l) * wm_interm_85_triplet_pt4(p, l, j, k)
term(306) = term(306) + t2(a,q,j,i) * wm_interm_62_triplet_pt4(a, j, k, l) * wm_interm_85_triplet_pt4(p, l, i, k)
term(307) = term(307) + t2(a,q,j,i) * wm_interm_62_triplet_pt4(a, k, j, l) * wm_interm_85_triplet_pt4(p, l, i, k)
term(308) = term(308) + t2(a,q,j,i) * wm_interm_62_triplet_pt4(a, i, k, l) * wm_interm_85_triplet_pt4(p, l, j, k)
term(309) = term(309) + t2(a,q,j,i) * wm_interm_60_triplet_pt4(a, i, k, l) * wm_interm_94_triplet_pt4(p, j, l, k)
term(310) = term(310) + t2(a,q,j,i) * wm_interm_60_triplet_pt4(a, k, i, l) * wm_interm_94_triplet_pt4(p, j, l, k)
term(311) = term(311) + t2(a,q,j,i) * wm_interm_62_triplet_pt4(a, j, k, l) * wm_interm_94_triplet_pt4(p, i, l, k)
term(312) = term(312) + t2(a,q,j,i) * wm_interm_62_triplet_pt4(a, k, j, l) * wm_interm_94_triplet_pt4(p, i, l, k)
term(313) = term(313) + t2(a,q,j,i) * wm_interm_60_triplet_pt4(p, k, j, l) * wm_interm_94_triplet_pt4(a, i, l, k)
term(314) = term(314) + t2(a,q,j,i) * wm_interm_60_triplet_pt4(p, j, k, l) * wm_interm_94_triplet_pt4(a, i, l, k)
term(315) = term(315) + t2(a,q,j,i) * wm_interm_62_triplet_pt4(a, j, k, l) * wm_interm_92_triplet_pt4(p, i, l, k)
term(316) = term(316) + t2(a,q,j,i) * wm_interm_62_triplet_pt4(a, k, j, l) * wm_interm_92_triplet_pt4(p, i, l, k)
term(317) = term(317) + t2(a,q,j,i) * wm_interm_60_triplet_pt4(p, j, k, l) * wm_interm_92_triplet_pt4(a, i, l, k)
term(318) = term(318) + t2(a,q,j,i) * wm_interm_60_triplet_pt4(p, k, j, l) * wm_interm_92_triplet_pt4(a, i, l, k)
term(319) = term(319) + t2(a,q,j,i) * wm_interm_60_triplet_pt4(p, k, i, l) * wm_interm_94_triplet_pt4(a, j, l, k)
term(320) = term(320) + t2(a,q,j,i) * wm_interm_60_triplet_pt4(p, i, k, l) * wm_interm_94_triplet_pt4(a, j, l, k)
term(321) = term(321) + t2(a,q,j,i) * wm_interm_60_triplet_pt4(p, k, i, l) * wm_interm_92_triplet_pt4(a, j, l, k)
term(322) = term(322) + t2(a,q,j,i) * wm_interm_60_triplet_pt4(p, i, k, l) * wm_interm_92_triplet_pt4(a, j, l, k)
term(323) = term(323) + t2(a,q,j,i) * wm_interm_60_triplet_pt4(a, i, k, l) * wm_interm_92_triplet_pt4(p, j, l, k)
term(324) = term(324) + t2(a,q,j,i) * wm_interm_60_triplet_pt4(a, k, i, l) * wm_interm_92_triplet_pt4(p, j, l, k)
term(325) = term(325) + t2(a,q,j,i) * wm_interm_60_triplet_pt4(p, k, i, l) * wm_interm_94_triplet_pt4(a, l, j, k)
term(326) = term(326) + t2(a,q,j,i) * wm_interm_60_triplet_pt4(p, k, i, l) * wm_interm_92_triplet_pt4(a, l, j, k)
term(327) = term(327) + t2(a,q,j,i) * wm_interm_60_triplet_pt4(p, j, k, l) * wm_interm_94_triplet_pt4(a, l, i, k)
term(328) = term(328) + t2(a,q,j,i) * wm_interm_60_triplet_pt4(p, k, j, l) * wm_interm_94_triplet_pt4(a, l, i, k)
term(329) = term(329) + t2(a,q,j,i) * wm_interm_60_triplet_pt4(p, i, k, l) * wm_interm_94_triplet_pt4(a, l, j, k)
term(330) = term(330) + t2(a,q,j,i) * wm_interm_60_triplet_pt4(p, k, j, l) * wm_interm_92_triplet_pt4(a, l, i, k)
term(331) = term(331) + t2(a,q,j,i) * wm_interm_60_triplet_pt4(p, j, k, l) * wm_interm_92_triplet_pt4(a, l, i, k)
term(332) = term(332) + t2(a,q,j,i) * wm_interm_60_triplet_pt4(p, i, k, l) * wm_interm_92_triplet_pt4(a, l, j, k)
term(333) = term(333) + t2(a,q,j,i) * wm_interm_62_triplet_pt4(a, k, i, l) * wm_interm_94_triplet_pt4(p, l, j, k)
term(334) = term(334) + t2(a,q,j,i) * wm_interm_62_triplet_pt4(a, k, i, l) * wm_interm_92_triplet_pt4(p, l, j, k)
term(335) = term(335) + t2(a,q,j,i) * wm_interm_62_triplet_pt4(a, j, k, l) * wm_interm_94_triplet_pt4(p, l, i, k)
term(336) = term(336) + t2(a,q,j,i) * wm_interm_62_triplet_pt4(a, k, j, l) * wm_interm_94_triplet_pt4(p, l, i, k)
term(337) = term(337) + t2(a,q,j,i) * wm_interm_62_triplet_pt4(a, i, k, l) * wm_interm_94_triplet_pt4(p, l, j, k)
term(338) = term(338) + t2(a,q,j,i) * wm_interm_62_triplet_pt4(a, i, k, l) * wm_interm_92_triplet_pt4(p, l, j, k)
term(339) = term(339) + t2(a,q,j,i) * wm_interm_62_triplet_pt4(a, j, k, l) * wm_interm_92_triplet_pt4(p, l, i, k)
term(340) = term(340) + t2(a,q,j,i) * wm_interm_62_triplet_pt4(a, k, j, l) * wm_interm_92_triplet_pt4(p, l, i, k)
end do 
end do 
end do 
end do 
end do 

term(285) = term(285) * 6.0d+0 
term(286) = term(286) * (-6.0d+0) 
term(287) = term(287) * 16.0d+0 
term(288) = term(288) * (-8.0d+0) 
term(289) = term(289) * 6.0d+0 
term(290) = term(290) * (-6.0d+0) 
term(291) = term(291) * 16.0d+0 
term(292) = term(292) * (-8.0d+0) 
term(293) = term(293) * 3.0d+0 
term(294) = term(294) * (-3.0d+0) 
term(295) = term(295) * 3.0d+0 
term(296) = term(296) * (-6.0d+0) 
term(297) = term(297) * (-3.0d+0) 
term(298) = term(298) * 3.0d+0 
term(299) = term(299) * 3.0d+0 
term(300) = term(300) * (-6.0d+0) 
term(301) = term(301) * (-3.0d+0) 
term(302) = term(302) * (-3.0d+0) 
term(303) = term(303) * 3.0d+0 
term(304) = term(304) * 6.0d+0 
term(305) = term(305) * (-3.0d+0) 
term(306) = term(306) * (-3.0d+0) 
term(307) = term(307) * 6.0d+0 
term(308) = term(308) * 3.0d+0 
term(309) = term(309) * (-4.0d+0) 
term(310) = term(310) * 2.0d+0 
term(311) = term(311) * (-4.0d+0) 
term(312) = term(312) * 8.0d+0 
term(313) = term(313) * 2.0d+0 
term(314) = term(314) * (-4.0d+0) 
term(315) = term(315) * 2.0d+0 
term(316) = term(316) * (-4.0d+0) 
term(317) = term(317) * 2.0d+0 
term(318) = term(318) * (-4.0d+0) 
term(319) = term(319) * (-4.0d+0) 
term(320) = term(320) * 8.0d+0 
term(321) = term(321) * 2.0d+0 
term(322) = term(322) * (-4.0d+0) 
term(323) = term(323) * 2.0d+0 
term(324) = term(324) * (-4.0d+0) 
term(325) = term(325) * 2.0d+0 
term(326) = term(326) * (-4.0d+0) 
term(327) = term(327) * 2.0d+0 
term(328) = term(328) * (-4.0d+0) 
term(329) = term(329) * (-4.0d+0) 
term(330) = term(330) * 2.0d+0 
term(331) = term(331) * (-4.0d+0) 
term(332) = term(332) * 8.0d+0 
term(333) = term(333) * 2.0d+0 
term(334) = term(334) * (-4.0d+0) 
term(335) = term(335) * 2.0d+0 
term(336) = term(336) * (-4.0d+0) 
term(337) = term(337) * (-4.0d+0) 
term(338) = term(338) * 2.0d+0 
term(339) = term(339) * (-4.0d+0) 
term(340) = term(340) * 8.0d+0 

do a = nocc + 1, nactive 
term(341) = term(341) + wm_interm_10_triplet_pt4(q, a) * wm_interm_9_triplet_pt4(a, p)
term(342) = term(342) + wm_interm_10_triplet_pt4(q, a) * wm_interm_15_triplet_pt4(a, p)
term(343) = term(343) + wm_interm_10_triplet_pt4(q, a) * wm_interm_16_triplet_pt4(a, p)
term(344) = term(344) + wm_interm_10_triplet_pt4(a, q) * wm_interm_9_triplet_pt4(p, a)
term(345) = term(345) + wm_interm_10_triplet_pt4(a, q) * wm_interm_16_triplet_pt4(p, a)
term(346) = term(346) + wm_interm_10_triplet_pt4(a, q) * wm_interm_15_triplet_pt4(p, a)
term(347) = term(347) + wm_interm_10_triplet_pt4(q, a) * wm_interm_25_triplet_pt4(a, p)
term(348) = term(348) + wm_interm_10_triplet_pt4(q, a) * wm_interm_28_triplet_pt4(a, p)
term(349) = term(349) + wm_interm_10_triplet_pt4(q, a) * wm_interm_32_triplet_pt4(a, p)
term(350) = term(350) + wm_interm_10_triplet_pt4(q, a) * wm_interm_33_triplet_pt4(a, p)
term(351) = term(351) + wm_interm_10_triplet_pt4(q, a) * wm_interm_36_triplet_pt4(a, p)
term(352) = term(352) + wm_interm_10_triplet_pt4(q, a) * wm_interm_37_triplet_pt4(a, p)
term(353) = term(353) + wm_interm_10_triplet_pt4(a, q) * wm_interm_28_triplet_pt4(p, a)
term(354) = term(354) + wm_interm_10_triplet_pt4(a, q) * wm_interm_25_triplet_pt4(p, a)
term(355) = term(355) + wm_interm_10_triplet_pt4(a, q) * wm_interm_36_triplet_pt4(p, a)
term(356) = term(356) + wm_interm_10_triplet_pt4(a, q) * wm_interm_37_triplet_pt4(p, a)
term(357) = term(357) + wm_interm_10_triplet_pt4(a, q) * wm_interm_32_triplet_pt4(p, a)
term(358) = term(358) + wm_interm_10_triplet_pt4(a, q) * wm_interm_33_triplet_pt4(p, a)
term(359) = term(359) + wm_interm_55_triplet_pt4(p, a) * wm_interm_61_triplet_pt4(q, a)
term(360) = term(360) + wm_interm_10_triplet_pt4(q, a) * wm_interm_57_triplet_pt4(a, p)
term(361) = term(361) + wm_interm_55_triplet_pt4(a, p) * wm_interm_61_triplet_pt4(a, q)
term(362) = term(362) + wm_interm_10_triplet_pt4(a, q) * wm_interm_57_triplet_pt4(p, a)
end do 

term(341) = term(341) * (-6.0d+0) 
term(342) = term(342) * 6.0d+0 
term(343) = term(343) * (-6.0d+0) 
term(344) = term(344) * (-6.0d+0) 
term(345) = term(345) * (-6.0d+0) 
term(346) = term(346) * 6.0d+0 
term(347) = term(347) * (-6.0d+0) 
term(348) = term(348) * 6.0d+0 
term(349) = term(349) * 4.0d+0 
term(350) = term(350) * (-8.0d+0) 
term(351) = term(351) * 4.0d+0 
term(352) = term(352) * (-8.0d+0) 
term(353) = term(353) * 6.0d+0 
term(354) = term(354) * (-6.0d+0) 
term(355) = term(355) * 4.0d+0 
term(356) = term(356) * (-8.0d+0) 
term(357) = term(357) * 4.0d+0 
term(358) = term(358) * (-8.0d+0) 
term(359) = term(359) * (-2.0d+0) 
term(360) = term(360) * (-2.0d+0) 
term(361) = term(361) * (-2.0d+0) 
term(362) = term(362) * (-2.0d+0) 

do l = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(363) = term(363) + s2(a,q,j,i) * wm_interm_43_triplet_pt4(a, k, i, l) * wm_interm_64_triplet_pt4(p, l, k, j)
term(364) = term(364) + s2(a,q,j,i) * wm_interm_51_triplet_pt4(a, k, l, j) * wm_interm_8_triplet_pt4(p, k, i, l)
term(365) = term(365) + s2(a,q,j,i) * wm_interm_43_triplet_pt4(a, k, i, l) * wm_interm_71_triplet_pt4(p, l, k, j)
term(366) = term(366) + s2(a,q,j,i) * wm_interm_14_triplet_pt4(p, k, i, l) * wm_interm_51_triplet_pt4(a, k, l, j)
term(367) = term(367) + s2(a,q,j,i) * wm_interm_14_triplet_pt4(p, i, k, l) * wm_interm_51_triplet_pt4(a, k, l, j)
term(368) = term(368) + s2(a,q,j,i) * wm_interm_51_triplet_pt4(a, k, l, j) * wm_interm_79_triplet_pt4(p, i, k, l)
term(369) = term(369) + s2(a,q,j,i) * wm_interm_51_triplet_pt4(a, k, l, j) * wm_interm_79_triplet_pt4(p, k, i, l)
term(370) = term(370) + s2(a,p,j,i) * wm_interm_43_triplet_pt4(q, k, i, l) * wm_interm_64_triplet_pt4(a, l, k, j)
term(371) = term(371) + s2(a,p,j,i) * wm_interm_43_triplet_pt4(q, k, i, l) * wm_interm_71_triplet_pt4(a, l, k, j)
term(372) = term(372) + s2(a,p,j,i) * wm_interm_51_triplet_pt4(q, k, l, j) * wm_interm_79_triplet_pt4(a, i, k, l)
term(373) = term(373) + s2(a,p,j,i) * wm_interm_51_triplet_pt4(q, k, l, j) * wm_interm_79_triplet_pt4(a, k, i, l)
term(374) = term(374) + s2(a,p,j,i) * wm_interm_14_triplet_pt4(a, k, i, l) * wm_interm_51_triplet_pt4(q, k, l, j)
term(375) = term(375) + s2(a,p,j,i) * wm_interm_14_triplet_pt4(a, i, k, l) * wm_interm_51_triplet_pt4(q, k, l, j)
term(376) = term(376) + t2(a,q,j,i) * wm_interm_45_triplet_pt4(a, k, l, j) * wm_interm_7_triplet_pt4(p, k, i, l)
term(377) = term(377) + t2(a,q,j,i) * wm_interm_24_triplet_pt4(p, k, i, l) * wm_interm_45_triplet_pt4(a, k, l, j)
term(378) = term(378) + t2(a,q,j,i) * wm_interm_24_triplet_pt4(p, i, k, l) * wm_interm_45_triplet_pt4(a, k, l, j)
term(379) = term(379) + t2(a,q,j,i) * wm_interm_24_triplet_pt4(a, i, k, l) * wm_interm_45_triplet_pt4(p, k, l, j)
term(380) = term(380) + t2(a,q,j,i) * wm_interm_24_triplet_pt4(a, k, i, l) * wm_interm_45_triplet_pt4(p, k, l, j)
term(381) = term(381) + t2(a,q,j,i) * wm_interm_27_triplet_pt4(a, i, k, l) * wm_interm_45_triplet_pt4(p, k, l, j)
term(382) = term(382) + t2(a,q,j,i) * wm_interm_27_triplet_pt4(a, k, i, l) * wm_interm_45_triplet_pt4(p, k, l, j)
term(383) = term(383) + t2(a,q,j,i) * wm_interm_27_triplet_pt4(p, i, k, l) * wm_interm_45_triplet_pt4(a, k, l, j)
term(384) = term(384) + t2(a,q,j,i) * wm_interm_27_triplet_pt4(p, k, i, l) * wm_interm_45_triplet_pt4(a, k, l, j)
end do 
end do 
end do 
end do 
end do 

term(363) = term(363) * (-6.0d+0) 
term(364) = term(364) * 24.0d+0 
term(365) = term(365) * (-8.0d+0) 
term(366) = term(366) * 8.0d+0 
term(367) = term(367) * (-16.0d+0) 
term(368) = term(368) * 8.0d+0 
term(369) = term(369) * (-16.0d+0) 
term(370) = term(370) * 6.0d+0 
term(371) = term(371) * 16.0d+0 
term(372) = term(372) * (-4.0d+0) 
term(373) = term(373) * 8.0d+0 
term(374) = term(374) * (-4.0d+0) 
term(375) = term(375) * 8.0d+0 
term(376) = term(376) * 12.0d+0 
term(377) = term(377) * (-4.0d+0) 
term(378) = term(378) * 8.0d+0 
term(379) = term(379) * (-4.0d+0) 
term(380) = term(380) * 2.0d+0 
term(381) = term(381) * 2.0d+0 
term(382) = term(382) * (-4.0d+0) 
term(383) = term(383) * (-4.0d+0) 
term(384) = term(384) * 8.0d+0 

do l = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(385) = term(385) + s2(a,q,j,i) * wm_interm_51_triplet_pt4(a, k, l, i) * wm_interm_8_triplet_pt4(p, k, j, l)
term(386) = term(386) + s2(a,q,j,i) * wm_interm_51_triplet_pt4(a, k, j, l) * wm_interm_8_triplet_pt4(p, k, i, l)
term(387) = term(387) + s2(a,q,j,i) * wm_interm_51_triplet_pt4(a, k, i, l) * wm_interm_8_triplet_pt4(p, k, j, l)
term(388) = term(388) + s2(a,q,j,i) * wm_interm_14_triplet_pt4(p, k, j, l) * wm_interm_51_triplet_pt4(a, k, l, i)
term(389) = term(389) + s2(a,q,j,i) * wm_interm_51_triplet_pt4(a, k, l, i) * wm_interm_79_triplet_pt4(p, k, j, l)
term(390) = term(390) + s2(a,q,j,i) * wm_interm_14_triplet_pt4(p, k, i, l) * wm_interm_51_triplet_pt4(a, k, j, l)
term(391) = term(391) + s2(a,q,j,i) * wm_interm_14_triplet_pt4(p, k, j, l) * wm_interm_51_triplet_pt4(a, k, i, l)
term(392) = term(392) + s2(a,q,j,i) * wm_interm_51_triplet_pt4(a, k, i, l) * wm_interm_79_triplet_pt4(p, k, j, l)
term(393) = term(393) + s2(a,q,j,i) * wm_interm_51_triplet_pt4(a, k, j, l) * wm_interm_79_triplet_pt4(p, k, i, l)
term(394) = term(394) + s2(a,p,j,i) * wm_interm_14_triplet_pt4(a, k, i, l) * wm_interm_51_triplet_pt4(q, k, j, l)
term(395) = term(395) + s2(a,p,j,i) * wm_interm_51_triplet_pt4(q, k, j, l) * wm_interm_79_triplet_pt4(a, k, i, l)
term(396) = term(396) + s2(a,p,j,i) * wm_interm_14_triplet_pt4(a, k, j, l) * wm_interm_51_triplet_pt4(q, k, i, l)
term(397) = term(397) + s2(a,p,j,i) * wm_interm_51_triplet_pt4(q, k, i, l) * wm_interm_79_triplet_pt4(a, k, j, l)
term(398) = term(398) + s2(a,p,j,i) * wm_interm_51_triplet_pt4(q, k, l, i) * wm_interm_79_triplet_pt4(a, k, j, l)
term(399) = term(399) + s2(a,p,j,i) * wm_interm_14_triplet_pt4(a, k, j, l) * wm_interm_51_triplet_pt4(q, k, l, i)
term(400) = term(400) + t2(a,q,j,i) * wm_interm_45_triplet_pt4(a, k, l, i) * wm_interm_7_triplet_pt4(p, k, j, l)
term(401) = term(401) + t2(a,q,j,i) * wm_interm_45_triplet_pt4(a, k, i, l) * wm_interm_7_triplet_pt4(p, k, j, l)
term(402) = term(402) + t2(a,q,j,i) * wm_interm_45_triplet_pt4(a, k, j, l) * wm_interm_7_triplet_pt4(p, k, i, l)
term(403) = term(403) + t2(a,q,j,i) * wm_interm_24_triplet_pt4(p, k, j, l) * wm_interm_45_triplet_pt4(a, k, l, i)
term(404) = term(404) + t2(a,q,j,i) * wm_interm_24_triplet_pt4(p, k, j, l) * wm_interm_45_triplet_pt4(a, k, i, l)
term(405) = term(405) + t2(a,q,j,i) * wm_interm_24_triplet_pt4(p, k, i, l) * wm_interm_45_triplet_pt4(a, k, j, l)
term(406) = term(406) + t2(a,q,j,i) * wm_interm_24_triplet_pt4(a, k, j, l) * wm_interm_45_triplet_pt4(p, k, l, i)
term(407) = term(407) + t2(a,q,j,i) * wm_interm_24_triplet_pt4(a, k, j, l) * wm_interm_45_triplet_pt4(p, k, i, l)
term(408) = term(408) + t2(a,q,j,i) * wm_interm_24_triplet_pt4(a, k, i, l) * wm_interm_45_triplet_pt4(p, k, j, l)
term(409) = term(409) + t2(a,q,j,i) * wm_interm_27_triplet_pt4(a, k, j, l) * wm_interm_45_triplet_pt4(p, k, l, i)
term(410) = term(410) + t2(a,q,j,i) * wm_interm_27_triplet_pt4(p, k, j, l) * wm_interm_45_triplet_pt4(a, k, l, i)
term(411) = term(411) + t2(a,q,j,i) * wm_interm_27_triplet_pt4(p, k, j, l) * wm_interm_45_triplet_pt4(a, k, i, l)
term(412) = term(412) + t2(a,q,j,i) * wm_interm_27_triplet_pt4(a, k, j, l) * wm_interm_45_triplet_pt4(p, k, i, l)
term(413) = term(413) + t2(a,q,j,i) * wm_interm_27_triplet_pt4(a, k, i, l) * wm_interm_45_triplet_pt4(p, k, j, l)
term(414) = term(414) + t2(a,q,j,i) * wm_interm_27_triplet_pt4(p, k, i, l) * wm_interm_45_triplet_pt4(a, k, j, l)
end do 
end do 
end do 
end do 
end do 

term(385) = term(385) * (-12.0d+0) 
term(386) = term(386) * (-12.0d+0) 
term(387) = term(387) * 12.0d+0 
term(388) = term(388) * (-4.0d+0) 
term(389) = term(389) * 8.0d+0 
term(390) = term(390) * (-4.0d+0) 
term(391) = term(391) * 8.0d+0 
term(392) = term(392) * (-4.0d+0) 
term(393) = term(393) * 8.0d+0 
term(394) = term(394) * 8.0d+0 
term(395) = term(395) * (-4.0d+0) 
term(396) = term(396) * (-4.0d+0) 
term(397) = term(397) * 8.0d+0 
term(398) = term(398) * (-16.0d+0) 
term(399) = term(399) * 8.0d+0 
term(400) = term(400) * (-6.0d+0) 
term(401) = term(401) * 6.0d+0 
term(402) = term(402) * (-6.0d+0) 
term(403) = term(403) * 2.0d+0 
term(404) = term(404) * (-4.0d+0) 
term(405) = term(405) * 2.0d+0 
term(406) = term(406) * (-4.0d+0) 
term(407) = term(407) * 2.0d+0 
term(408) = term(408) * (-4.0d+0) 
term(409) = term(409) * 8.0d+0 
term(410) = term(410) * (-4.0d+0) 
term(411) = term(411) * 2.0d+0 
term(412) = term(412) * (-4.0d+0) 
term(413) = term(413) * 2.0d+0 
term(414) = term(414) * (-4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(415) = term(415) + s2(a,q,j,i) * wm_interm_43_triplet_pt4(a, k, l, i) * wm_interm_64_triplet_pt4(p, l, k, j)
term(416) = term(416) + s2(a,q,j,i) * wm_interm_43_triplet_pt4(a, k, l, j) * wm_interm_64_triplet_pt4(p, l, k, i)
term(417) = term(417) + s2(a,q,j,i) * wm_interm_43_triplet_pt4(a, k, l, j) * wm_interm_71_triplet_pt4(p, l, k, i)
term(418) = term(418) + s2(a,q,j,i) * wm_interm_43_triplet_pt4(a, k, l, i) * wm_interm_71_triplet_pt4(p, l, k, j)
term(419) = term(419) + s2(a,p,j,i) * wm_interm_43_triplet_pt4(q, k, l, j) * wm_interm_64_triplet_pt4(a, l, k, i)
term(420) = term(420) + s2(a,p,j,i) * wm_interm_43_triplet_pt4(q, k, l, i) * wm_interm_64_triplet_pt4(a, l, k, j)
term(421) = term(421) + s2(a,p,j,i) * wm_interm_43_triplet_pt4(q, k, l, i) * wm_interm_71_triplet_pt4(a, l, k, j)
term(422) = term(422) + s2(a,p,j,i) * wm_interm_43_triplet_pt4(q, k, l, j) * wm_interm_71_triplet_pt4(a, l, k, i)
end do 
end do 
end do 
end do 
end do 

term(415) = term(415) * 6.0d+0 
term(416) = term(416) * (-6.0d+0) 
term(417) = term(417) * (-32.0d+0) 
term(418) = term(418) * 16.0d+0 
term(419) = term(419) * 6.0d+0 
term(420) = term(420) * (-6.0d+0) 
term(421) = term(421) * (-32.0d+0) 
term(422) = term(422) * 16.0d+0 

do j = 1, nocc 
do i = 1, nocc 
term(423) = term(423) + s1(p,i) * t1(q,j) * wm_interm_6_triplet_pt4(j, i)
term(424) = term(424) + s1(p,i) * t1(q,j) * wm_interm_13_triplet_pt4(j, i)
term(425) = term(425) + s1(p,i) * t1(q,j) * wm_interm_12_triplet_pt4(j, i)
term(426) = term(426) + s1(p,i) * t1(q,j) * wm_interm_26_triplet_pt4(j, i)
term(427) = term(427) + s1(p,i) * t1(q,j) * wm_interm_23_triplet_pt4(j, i)
term(428) = term(428) + s1(p,i) * t1(q,j) * wm_interm_34_triplet_pt4(j, i)
term(429) = term(429) + s1(p,i) * t1(q,j) * wm_interm_35_triplet_pt4(j, i)
term(430) = term(430) + s1(p,i) * t1(q,j) * wm_interm_30_triplet_pt4(j, i)
term(431) = term(431) + s1(p,i) * t1(q,j) * wm_interm_31_triplet_pt4(j, i)
term(432) = term(432) + r1(vrdav_Rl, q,j) * wm_interm_1_triplet_pt4(p, i) * wm_interm_53_triplet_pt4(i, j)
term(433) = term(433) + r1(vrdav_Rl, q,j) * wm_interm_1_triplet_pt4(p, i) * wm_interm_54_triplet_pt4(i, j)
term(434) = term(434) + r1(vrdav_Rl, q,j) * wm_interm_2_triplet_pt4(p, i) * wm_interm_53_triplet_pt4(i, j)
term(435) = term(435) + r1(vrdav_Rl, q,j) * wm_interm_2_triplet_pt4(p, i) * wm_interm_54_triplet_pt4(i, j)
term(436) = term(436) + r1(vrdav_Rl, q,j) * wm_interm_3_triplet_pt4(p, i) * wm_interm_53_triplet_pt4(i, j)
term(437) = term(437) + r1(vrdav_Rl, q,j) * wm_interm_3_triplet_pt4(p, i) * wm_interm_54_triplet_pt4(i, j)
term(438) = term(438) + s1(p,i) * wm_interm_52_triplet_pt4(q, j) * wm_interm_67_triplet_pt4(i, j)
term(439) = term(439) + s1(p,i) * wm_interm_52_triplet_pt4(q, j) * wm_interm_68_triplet_pt4(i, j)
term(440) = term(440) + s1(p,i) * wm_interm_47_triplet_pt4(q, j) * wm_interm_67_triplet_pt4(i, j)
term(441) = term(441) + s1(p,i) * wm_interm_47_triplet_pt4(q, j) * wm_interm_68_triplet_pt4(i, j)
term(442) = term(442) + r1(vrdav_Rl, p,i) * wm_interm_42_triplet_pt4(q, j) * wm_interm_67_triplet_pt4(i, j)
term(443) = term(443) + r1(vrdav_Rl, p,i) * wm_interm_42_triplet_pt4(q, j) * wm_interm_68_triplet_pt4(i, j)
term(444) = term(444) + r1(vrdav_Rl, p,i) * wm_interm_48_triplet_pt4(q, j) * wm_interm_67_triplet_pt4(i, j)
term(445) = term(445) + r1(vrdav_Rl, p,i) * wm_interm_48_triplet_pt4(q, j) * wm_interm_68_triplet_pt4(i, j)
term(446) = term(446) + s1(p,i) * wm_interm_52_triplet_pt4(q, j) * wm_interm_75_triplet_pt4(i, j)
term(447) = term(447) + s1(p,i) * wm_interm_52_triplet_pt4(q, j) * wm_interm_76_triplet_pt4(i, j)
term(448) = term(448) + s1(p,i) * wm_interm_52_triplet_pt4(q, j) * wm_interm_77_triplet_pt4(i, j)
term(449) = term(449) + s1(p,i) * wm_interm_52_triplet_pt4(q, j) * wm_interm_78_triplet_pt4(i, j)
term(450) = term(450) + s1(p,i) * wm_interm_47_triplet_pt4(q, j) * wm_interm_75_triplet_pt4(i, j)
term(451) = term(451) + s1(p,i) * wm_interm_47_triplet_pt4(q, j) * wm_interm_76_triplet_pt4(i, j)
term(452) = term(452) + s1(p,i) * wm_interm_47_triplet_pt4(q, j) * wm_interm_77_triplet_pt4(i, j)
term(453) = term(453) + s1(p,i) * wm_interm_47_triplet_pt4(q, j) * wm_interm_78_triplet_pt4(i, j)
term(454) = term(454) + r1(vrdav_Rl, p,i) * wm_interm_42_triplet_pt4(q, j) * wm_interm_75_triplet_pt4(i, j)
term(455) = term(455) + r1(vrdav_Rl, p,i) * wm_interm_42_triplet_pt4(q, j) * wm_interm_76_triplet_pt4(i, j)
term(456) = term(456) + r1(vrdav_Rl, p,i) * wm_interm_42_triplet_pt4(q, j) * wm_interm_77_triplet_pt4(i, j)
term(457) = term(457) + r1(vrdav_Rl, p,i) * wm_interm_42_triplet_pt4(q, j) * wm_interm_78_triplet_pt4(i, j)
term(458) = term(458) + r1(vrdav_Rl, p,i) * wm_interm_48_triplet_pt4(q, j) * wm_interm_75_triplet_pt4(i, j)
term(459) = term(459) + r1(vrdav_Rl, p,i) * wm_interm_48_triplet_pt4(q, j) * wm_interm_76_triplet_pt4(i, j)
term(460) = term(460) + r1(vrdav_Rl, p,i) * wm_interm_48_triplet_pt4(q, j) * wm_interm_77_triplet_pt4(i, j)
term(461) = term(461) + r1(vrdav_Rl, p,i) * wm_interm_48_triplet_pt4(q, j) * wm_interm_78_triplet_pt4(i, j)
term(462) = term(462) + t1(q,j) * wm_interm_53_triplet_pt4(j, i) * wm_interm_86_triplet_pt4(p, i)
term(463) = term(463) + t1(q,j) * wm_interm_54_triplet_pt4(j, i) * wm_interm_86_triplet_pt4(p, i)
term(464) = term(464) + t1(q,j) * wm_interm_44_triplet_pt4(p, i) * wm_interm_87_triplet_pt4(j, i)
term(465) = term(465) + t1(q,j) * wm_interm_46_triplet_pt4(p, i) * wm_interm_87_triplet_pt4(j, i)
term(466) = term(466) + t1(q,j) * wm_interm_44_triplet_pt4(p, i) * wm_interm_89_triplet_pt4(j, i)
term(467) = term(467) + t1(q,j) * wm_interm_46_triplet_pt4(p, i) * wm_interm_89_triplet_pt4(j, i)
term(468) = term(468) + r1(vrdav_Rr, p,i) * wm_interm_20_triplet_pt4(q, j) * wm_interm_53_triplet_pt4(i, j)
term(469) = term(469) + r1(vrdav_Rr, p,i) * wm_interm_20_triplet_pt4(q, j) * wm_interm_54_triplet_pt4(i, j)
term(470) = term(470) + r1(vrdav_Rr, p,i) * wm_interm_21_triplet_pt4(q, j) * wm_interm_53_triplet_pt4(i, j)
term(471) = term(471) + r1(vrdav_Rr, p,i) * wm_interm_21_triplet_pt4(q, j) * wm_interm_54_triplet_pt4(i, j)
term(472) = term(472) + t1(q,j) * wm_interm_53_triplet_pt4(j, i) * wm_interm_96_triplet_pt4(p, i)
term(473) = term(473) + t1(q,j) * wm_interm_54_triplet_pt4(j, i) * wm_interm_96_triplet_pt4(p, i)
term(474) = term(474) + t1(q,j) * wm_interm_53_triplet_pt4(j, i) * wm_interm_93_triplet_pt4(p, i)
term(475) = term(475) + t1(q,j) * wm_interm_54_triplet_pt4(j, i) * wm_interm_93_triplet_pt4(p, i)
term(476) = term(476) + t1(q,j) * wm_interm_44_triplet_pt4(p, i) * wm_interm_95_triplet_pt4(j, i)
term(477) = term(477) + t1(q,j) * wm_interm_46_triplet_pt4(p, i) * wm_interm_95_triplet_pt4(j, i)
term(478) = term(478) + t1(q,j) * wm_interm_44_triplet_pt4(p, i) * wm_interm_98_triplet_pt4(j, i)
term(479) = term(479) + t1(q,j) * wm_interm_46_triplet_pt4(p, i) * wm_interm_98_triplet_pt4(j, i)
end do 
end do 

term(423) = term(423) * (-6.0d+0) 
term(424) = term(424) * (-6.0d+0) 
term(425) = term(425) * 6.0d+0 
term(426) = term(426) * 6.0d+0 
term(427) = term(427) * (-6.0d+0) 
term(428) = term(428) * 4.0d+0 
term(429) = term(429) * (-8.0d+0) 
term(430) = term(430) * 4.0d+0 
term(431) = term(431) * (-8.0d+0) 
term(432) = term(432) * 6.0d+0 
term(433) = term(433) * (-12.0d+0) 
term(434) = term(434) * 8.0d+0 
term(435) = term(435) * (-16.0d+0) 
term(436) = term(436) * (-4.0d+0) 
term(437) = term(437) * 8.0d+0 
term(438) = term(438) * (-6.0d+0) 
term(439) = term(439) * 6.0d+0 
term(440) = term(440) * 3.0d+0 
term(441) = term(441) * (-3.0d+0) 
term(442) = term(442) * (-6.0d+0) 
term(443) = term(443) * 6.0d+0 
term(444) = term(444) * 12.0d+0 
term(445) = term(445) * (-12.0d+0) 
term(446) = term(446) * 8.0d+0 
term(447) = term(447) * (-16.0d+0) 
term(448) = term(448) * 8.0d+0 
term(449) = term(449) * (-16.0d+0) 
term(450) = term(450) * (-4.0d+0) 
term(451) = term(451) * 8.0d+0 
term(452) = term(452) * (-4.0d+0) 
term(453) = term(453) * 8.0d+0 
term(454) = term(454) * (-4.0d+0) 
term(455) = term(455) * 8.0d+0 
term(456) = term(456) * (-4.0d+0) 
term(457) = term(457) * 8.0d+0 
term(458) = term(458) * 8.0d+0 
term(459) = term(459) * (-16.0d+0) 
term(460) = term(460) * 8.0d+0 
term(461) = term(461) * (-16.0d+0) 
term(462) = term(462) * (-6.0d+0) 
term(463) = term(463) * 12.0d+0 
term(464) = -term(464) 
term(465) = term(465) * 2.0d+0 
term(466) = -term(466) 
term(467) = term(467) * 2.0d+0 
term(468) = term(468) * (-16.0d+0) 
term(469) = term(469) * 32.0d+0 
term(470) = term(470) * 8.0d+0 
term(471) = term(471) * (-16.0d+0) 
term(472) = term(472) * 4.0d+0 
term(473) = term(473) * (-8.0d+0) 
term(474) = term(474) * (-8.0d+0) 
term(475) = term(475) * 16.0d+0 
term(476) = term(476) * 4.0d+0 
term(477) = term(477) * (-8.0d+0) 
term(478) = term(478) * (-8.0d+0) 
term(479) = term(479) * 16.0d+0 

do a = nocc + 1, nactive 
do i = 1, nocc 
term(480) = term(480) + r1(vrdav_Rl, a,i) * wm_interm_1_triplet_pt4(p, i) * wm_interm_56_triplet_pt4(q, a)
term(481) = term(481) + r1(vrdav_Rl, a,i) * wm_interm_1_triplet_pt4(p, i) * wm_interm_58_triplet_pt4(q, a)
term(482) = term(482) + r1(vrdav_Rl, a,i) * wm_interm_2_triplet_pt4(p, i) * wm_interm_56_triplet_pt4(q, a)
term(483) = term(483) + r1(vrdav_Rl, a,i) * wm_interm_2_triplet_pt4(p, i) * wm_interm_58_triplet_pt4(q, a)
term(484) = term(484) + r1(vrdav_Rl, a,i) * wm_interm_3_triplet_pt4(p, i) * wm_interm_56_triplet_pt4(q, a)
term(485) = term(485) + r1(vrdav_Rl, a,i) * wm_interm_3_triplet_pt4(p, i) * wm_interm_58_triplet_pt4(q, a)
term(486) = term(486) + r1(vrdav_Rl, q,i) * wm_interm_42_triplet_pt4(a, i) * wm_interm_65_triplet_pt4(p, a)
term(487) = term(487) + r1(vrdav_Rl, q,i) * wm_interm_42_triplet_pt4(a, i) * wm_interm_70_triplet_pt4(p, a)
term(488) = term(488) + r1(vrdav_Rl, q,i) * wm_interm_48_triplet_pt4(a, i) * wm_interm_65_triplet_pt4(p, a)
term(489) = term(489) + r1(vrdav_Rl, q,i) * wm_interm_48_triplet_pt4(a, i) * wm_interm_70_triplet_pt4(p, a)
term(490) = term(490) + s1(q,i) * wm_interm_47_triplet_pt4(a, i) * wm_interm_65_triplet_pt4(p, a)
term(491) = term(491) + s1(q,i) * wm_interm_47_triplet_pt4(a, i) * wm_interm_70_triplet_pt4(p, a)
term(492) = term(492) + s1(q,i) * wm_interm_52_triplet_pt4(a, i) * wm_interm_65_triplet_pt4(p, a)
term(493) = term(493) + s1(q,i) * wm_interm_52_triplet_pt4(a, i) * wm_interm_70_triplet_pt4(p, a)
term(494) = term(494) + r1(vrdav_Rl, q,i) * wm_interm_42_triplet_pt4(a, i) * wm_interm_72_triplet_pt4(p, a)
term(495) = term(495) + r1(vrdav_Rl, q,i) * wm_interm_42_triplet_pt4(a, i) * wm_interm_73_triplet_pt4(p, a)
term(496) = term(496) + r1(vrdav_Rl, q,i) * wm_interm_48_triplet_pt4(a, i) * wm_interm_72_triplet_pt4(p, a)
term(497) = term(497) + r1(vrdav_Rl, q,i) * wm_interm_48_triplet_pt4(a, i) * wm_interm_73_triplet_pt4(p, a)
term(498) = term(498) + s1(q,i) * wm_interm_47_triplet_pt4(a, i) * wm_interm_72_triplet_pt4(p, a)
term(499) = term(499) + s1(q,i) * wm_interm_47_triplet_pt4(a, i) * wm_interm_73_triplet_pt4(p, a)
term(500) = term(500) + s1(q,i) * wm_interm_47_triplet_pt4(a, i) * wm_interm_80_triplet_pt4(p, a)
term(501) = term(501) + s1(q,i) * wm_interm_47_triplet_pt4(a, i) * wm_interm_81_triplet_pt4(p, a)
term(502) = term(502) + s1(q,i) * wm_interm_52_triplet_pt4(a, i) * wm_interm_72_triplet_pt4(p, a)
term(503) = term(503) + s1(q,i) * wm_interm_52_triplet_pt4(a, i) * wm_interm_73_triplet_pt4(p, a)
term(504) = term(504) + s1(q,i) * wm_interm_52_triplet_pt4(a, i) * wm_interm_80_triplet_pt4(p, a)
term(505) = term(505) + s1(q,i) * wm_interm_52_triplet_pt4(a, i) * wm_interm_81_triplet_pt4(p, a)
term(506) = term(506) + t1(q,i) * wm_interm_56_triplet_pt4(p, a) * wm_interm_86_triplet_pt4(a, i)
term(507) = term(507) + t1(q,i) * wm_interm_58_triplet_pt4(p, a) * wm_interm_86_triplet_pt4(a, i)
term(508) = term(508) + t1(q,i) * wm_interm_44_triplet_pt4(a, i) * wm_interm_90_triplet_pt4(p, a)
term(509) = term(509) + t1(q,i) * wm_interm_46_triplet_pt4(a, i) * wm_interm_90_triplet_pt4(p, a)
term(510) = term(510) + t1(q,i) * wm_interm_44_triplet_pt4(a, i) * wm_interm_91_triplet_pt4(p, a)
term(511) = term(511) + t1(q,i) * wm_interm_46_triplet_pt4(a, i) * wm_interm_91_triplet_pt4(p, a)
term(512) = term(512) + r1(vrdav_Rr, p,i) * wm_interm_20_triplet_pt4(a, i) * wm_interm_56_triplet_pt4(q, a)
term(513) = term(513) + r1(vrdav_Rr, p,i) * wm_interm_20_triplet_pt4(a, i) * wm_interm_58_triplet_pt4(q, a)
term(514) = term(514) + r1(vrdav_Rr, p,i) * wm_interm_21_triplet_pt4(a, i) * wm_interm_56_triplet_pt4(q, a)
term(515) = term(515) + r1(vrdav_Rr, p,i) * wm_interm_21_triplet_pt4(a, i) * wm_interm_58_triplet_pt4(q, a)
term(516) = term(516) + t1(q,i) * wm_interm_56_triplet_pt4(p, a) * wm_interm_96_triplet_pt4(a, i)
term(517) = term(517) + t1(q,i) * wm_interm_58_triplet_pt4(p, a) * wm_interm_96_triplet_pt4(a, i)
term(518) = term(518) + t1(q,i) * wm_interm_56_triplet_pt4(p, a) * wm_interm_93_triplet_pt4(a, i)
term(519) = term(519) + t1(q,i) * wm_interm_58_triplet_pt4(p, a) * wm_interm_93_triplet_pt4(a, i)
term(520) = term(520) + t1(q,i) * wm_interm_44_triplet_pt4(a, i) * wm_interm_99_triplet_pt4(p, a)
term(521) = term(521) + t1(q,i) * wm_interm_100_triplet_pt4(p, a) * wm_interm_44_triplet_pt4(a, i)
term(522) = term(522) + t1(q,i) * wm_interm_46_triplet_pt4(a, i) * wm_interm_99_triplet_pt4(p, a)
term(523) = term(523) + t1(q,i) * wm_interm_100_triplet_pt4(p, a) * wm_interm_46_triplet_pt4(a, i)
end do 
end do 

term(480) = term(480) * (-12.0d+0) 
term(481) = term(481) * 6.0d+0 
term(482) = term(482) * (-16.0d+0) 
term(483) = term(483) * 8.0d+0 
term(484) = term(484) * 8.0d+0 
term(485) = term(485) * (-4.0d+0) 
term(486) = term(486) * 6.0d+0 
term(487) = term(487) * (-6.0d+0) 
term(488) = term(488) * (-12.0d+0) 
term(489) = term(489) * 12.0d+0 
term(490) = term(490) * 6.0d+0 
term(491) = term(491) * (-6.0d+0) 
term(492) = term(492) * (-12.0d+0) 
term(493) = term(493) * 12.0d+0 
term(494) = term(494) * (-8.0d+0) 
term(495) = term(495) * 16.0d+0 
term(496) = term(496) * 16.0d+0 
term(497) = term(497) * (-32.0d+0) 
term(498) = term(498) * (-4.0d+0) 
term(499) = term(499) * 8.0d+0 
term(500) = term(500) * (-4.0d+0) 
term(501) = term(501) * 8.0d+0 
term(502) = term(502) * 8.0d+0 
term(503) = term(503) * (-16.0d+0) 
term(504) = term(504) * 8.0d+0 
term(505) = term(505) * (-16.0d+0) 
term(506) = term(506) * 12.0d+0 
term(507) = term(507) * (-6.0d+0) 
term(508) = term(508) * 3.0d+0 
term(509) = term(509) * (-6.0d+0) 
term(510) = term(510) * (-3.0d+0) 
term(511) = term(511) * 6.0d+0 
term(512) = term(512) * 32.0d+0 
term(513) = term(513) * (-16.0d+0) 
term(514) = term(514) * (-16.0d+0) 
term(515) = term(515) * 8.0d+0 
term(516) = term(516) * (-8.0d+0) 
term(517) = term(517) * 4.0d+0 
term(518) = term(518) * 16.0d+0 
term(519) = term(519) * (-8.0d+0) 
term(520) = term(520) * 4.0d+0 
term(521) = term(521) * (-8.0d+0) 
term(522) = term(522) * (-8.0d+0) 
term(523) = term(523) * 16.0d+0 

do l = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
term(524) = term(524) + r1(vrdav_Rr, p,i) * wm_interm_60_triplet_pt4(q, j, k, l) * wm_interm_88_triplet_pt4(i, l, k, j)
term(525) = term(525) + r1(vrdav_Rr, p,i) * wm_interm_49_triplet_pt4(j, i, k, l) * wm_interm_7_triplet_pt4(q, k, l, j)
term(526) = term(526) + r1(vrdav_Rr, p,i) * wm_interm_49_triplet_pt4(i, j, k, l) * wm_interm_7_triplet_pt4(q, k, l, j)
term(527) = term(527) + t1(q,i) * wm_interm_49_triplet_pt4(j, i, k, l) * wm_interm_85_triplet_pt4(p, k, l, j)
term(528) = term(528) + t1(q,i) * wm_interm_49_triplet_pt4(i, j, k, l) * wm_interm_85_triplet_pt4(p, k, l, j)
term(529) = term(529) + r1(vrdav_Rr, p,i) * wm_interm_60_triplet_pt4(q, j, k, l) * wm_interm_97_triplet_pt4(i, l, k, j)
term(530) = term(530) + r1(vrdav_Rr, p,i) * wm_interm_60_triplet_pt4(q, j, k, l) * wm_interm_97_triplet_pt4(l, i, k, j)
term(531) = term(531) + t1(q,i) * wm_interm_49_triplet_pt4(j, i, k, l) * wm_interm_94_triplet_pt4(p, k, l, j)
term(532) = term(532) + t1(q,i) * wm_interm_49_triplet_pt4(i, j, k, l) * wm_interm_94_triplet_pt4(p, k, l, j)
term(533) = term(533) + t1(q,i) * wm_interm_49_triplet_pt4(i, j, k, l) * wm_interm_92_triplet_pt4(p, k, l, j)
term(534) = term(534) + t1(q,i) * wm_interm_49_triplet_pt4(j, i, k, l) * wm_interm_92_triplet_pt4(p, k, l, j)
end do 
end do 
end do 
end do 

term(524) = term(524) * 6.0d+0 
term(525) = term(525) * (-6.0d+0) 
term(526) = term(526) * 6.0d+0 
term(527) = term(527) * (-3.0d+0) 
term(528) = term(528) * 3.0d+0 
term(529) = term(529) * 4.0d+0 
term(530) = term(530) * (-8.0d+0) 
term(531) = term(531) * 2.0d+0 
term(532) = term(532) * (-4.0d+0) 
term(533) = term(533) * 2.0d+0 
term(534) = term(534) * (-4.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(535) = term(535) + s1(a,i) * wm_interm_56_triplet_pt4(p, q) * wm_interm_82_triplet_pt4(a, i)
term(536) = term(536) + s1(a,i) * wm_interm_58_triplet_pt4(p, q) * wm_interm_82_triplet_pt4(a, i)
term(537) = term(537) + s1(a,i) * wm_interm_56_triplet_pt4(p, q) * wm_interm_83_triplet_pt4(a, i)
term(538) = term(538) + s1(a,i) * wm_interm_56_triplet_pt4(p, q) * wm_interm_84_triplet_pt4(a, i)
term(539) = term(539) + r1(vrdav_Rl, p,i) * wm_interm_2_triplet_pt4(a, i) * wm_interm_56_triplet_pt4(a, q)
term(540) = term(540) + r1(vrdav_Rl, p,i) * wm_interm_2_triplet_pt4(a, i) * wm_interm_58_triplet_pt4(a, q)
term(541) = term(541) + r1(vrdav_Rl, p,i) * wm_interm_3_triplet_pt4(a, i) * wm_interm_56_triplet_pt4(a, q)
term(542) = term(542) + r1(vrdav_Rl, p,i) * wm_interm_3_triplet_pt4(a, i) * wm_interm_58_triplet_pt4(a, q)
term(543) = term(543) + s1(a,i) * wm_interm_58_triplet_pt4(p, q) * wm_interm_83_triplet_pt4(a, i)
term(544) = term(544) + s1(a,i) * wm_interm_58_triplet_pt4(p, q) * wm_interm_84_triplet_pt4(a, i)
term(545) = term(545) + s1(p,i) * wm_interm_56_triplet_pt4(a, q) * wm_interm_82_triplet_pt4(a, i)
term(546) = term(546) + s1(p,i) * wm_interm_58_triplet_pt4(a, q) * wm_interm_82_triplet_pt4(a, i)
term(547) = term(547) + r1(vrdav_Rl, a,i) * wm_interm_42_triplet_pt4(q, i) * wm_interm_65_triplet_pt4(a, p)
term(548) = term(548) + r1(vrdav_Rl, a,i) * wm_interm_42_triplet_pt4(q, i) * wm_interm_70_triplet_pt4(a, p)
term(549) = term(549) + r1(vrdav_Rl, a,i) * wm_interm_48_triplet_pt4(q, i) * wm_interm_65_triplet_pt4(a, p)
term(550) = term(550) + r1(vrdav_Rl, a,i) * wm_interm_48_triplet_pt4(q, i) * wm_interm_70_triplet_pt4(a, p)
term(551) = term(551) + s1(p,i) * wm_interm_56_triplet_pt4(a, q) * wm_interm_84_triplet_pt4(a, i)
term(552) = term(552) + s1(p,i) * wm_interm_56_triplet_pt4(a, q) * wm_interm_83_triplet_pt4(a, i)
term(553) = term(553) + s1(p,i) * wm_interm_58_triplet_pt4(a, q) * wm_interm_84_triplet_pt4(a, i)
term(554) = term(554) + s1(p,i) * wm_interm_58_triplet_pt4(a, q) * wm_interm_83_triplet_pt4(a, i)
term(555) = term(555) + r1(vrdav_Rl, a,i) * wm_interm_42_triplet_pt4(q, i) * wm_interm_72_triplet_pt4(a, p)
term(556) = term(556) + r1(vrdav_Rl, a,i) * wm_interm_42_triplet_pt4(q, i) * wm_interm_73_triplet_pt4(a, p)
term(557) = term(557) + s1(a,i) * wm_interm_52_triplet_pt4(q, i) * wm_interm_80_triplet_pt4(a, p)
term(558) = term(558) + s1(a,i) * wm_interm_52_triplet_pt4(q, i) * wm_interm_81_triplet_pt4(a, p)
term(559) = term(559) + s1(a,i) * wm_interm_52_triplet_pt4(q, i) * wm_interm_72_triplet_pt4(a, p)
term(560) = term(560) + s1(a,i) * wm_interm_52_triplet_pt4(q, i) * wm_interm_73_triplet_pt4(a, p)
term(561) = term(561) + r1(vrdav_Rl, a,i) * wm_interm_48_triplet_pt4(q, i) * wm_interm_72_triplet_pt4(a, p)
term(562) = term(562) + r1(vrdav_Rl, a,i) * wm_interm_48_triplet_pt4(q, i) * wm_interm_73_triplet_pt4(a, p)
term(563) = term(563) + s1(a,i) * wm_interm_47_triplet_pt4(q, i) * wm_interm_72_triplet_pt4(a, p)
term(564) = term(564) + s1(a,i) * wm_interm_47_triplet_pt4(q, i) * wm_interm_73_triplet_pt4(a, p)
term(565) = term(565) + s1(a,i) * wm_interm_47_triplet_pt4(q, i) * wm_interm_80_triplet_pt4(a, p)
term(566) = term(566) + s1(a,i) * wm_interm_47_triplet_pt4(q, i) * wm_interm_81_triplet_pt4(a, p)
term(567) = term(567) + t1(a,i) * wm_interm_58_triplet_pt4(a, q) * wm_interm_86_triplet_pt4(p, i)
term(568) = term(568) + t1(a,i) * wm_interm_56_triplet_pt4(a, q) * wm_interm_86_triplet_pt4(p, i)
term(569) = term(569) + t1(a,i) * wm_interm_44_triplet_pt4(p, i) * wm_interm_90_triplet_pt4(a, q)
term(570) = term(570) + t1(a,i) * wm_interm_46_triplet_pt4(p, i) * wm_interm_90_triplet_pt4(a, q)
term(571) = term(571) + t1(a,i) * wm_interm_44_triplet_pt4(p, i) * wm_interm_91_triplet_pt4(a, q)
term(572) = term(572) + t1(a,i) * wm_interm_46_triplet_pt4(p, i) * wm_interm_91_triplet_pt4(a, q)
term(573) = term(573) + t1(a,i) * wm_interm_56_triplet_pt4(p, q) * wm_interm_86_triplet_pt4(a, i)
term(574) = term(574) + r1(vrdav_Rr, a,i) * wm_interm_0_triplet_pt4(p, i) * wm_interm_56_triplet_pt4(a, q)
term(575) = term(575) + r1(vrdav_Rr, a,i) * wm_interm_0_triplet_pt4(p, i) * wm_interm_58_triplet_pt4(a, q)
term(576) = term(576) + t1(a,i) * wm_interm_58_triplet_pt4(p, q) * wm_interm_86_triplet_pt4(a, i)
term(577) = term(577) + t1(a,i) * wm_interm_58_triplet_pt4(a, q) * wm_interm_96_triplet_pt4(p, i)
term(578) = term(578) + t1(a,i) * wm_interm_56_triplet_pt4(a, q) * wm_interm_96_triplet_pt4(p, i)
term(579) = term(579) + t1(a,i) * wm_interm_58_triplet_pt4(a, q) * wm_interm_93_triplet_pt4(p, i)
term(580) = term(580) + t1(a,i) * wm_interm_56_triplet_pt4(a, q) * wm_interm_93_triplet_pt4(p, i)
term(581) = term(581) + t1(a,i) * wm_interm_44_triplet_pt4(p, i) * wm_interm_99_triplet_pt4(a, q)
term(582) = term(582) + t1(a,i) * wm_interm_46_triplet_pt4(p, i) * wm_interm_99_triplet_pt4(a, q)
term(583) = term(583) + t1(a,i) * wm_interm_100_triplet_pt4(a, q) * wm_interm_44_triplet_pt4(p, i)
term(584) = term(584) + t1(a,i) * wm_interm_100_triplet_pt4(a, q) * wm_interm_46_triplet_pt4(p, i)
term(585) = term(585) + t1(a,i) * wm_interm_56_triplet_pt4(p, q) * wm_interm_93_triplet_pt4(a, i)
term(586) = term(586) + t1(a,i) * wm_interm_56_triplet_pt4(p, q) * wm_interm_96_triplet_pt4(a, i)
term(587) = term(587) + r1(vrdav_Rr, a,i) * wm_interm_20_triplet_pt4(p, i) * wm_interm_56_triplet_pt4(a, q)
term(588) = term(588) + r1(vrdav_Rr, a,i) * wm_interm_20_triplet_pt4(p, i) * wm_interm_58_triplet_pt4(a, q)
term(589) = term(589) + r1(vrdav_Rr, a,i) * wm_interm_21_triplet_pt4(p, i) * wm_interm_56_triplet_pt4(a, q)
term(590) = term(590) + r1(vrdav_Rr, a,i) * wm_interm_21_triplet_pt4(p, i) * wm_interm_58_triplet_pt4(a, q)
term(591) = term(591) + t1(a,i) * wm_interm_58_triplet_pt4(p, q) * wm_interm_93_triplet_pt4(a, i)
term(592) = term(592) + t1(a,i) * wm_interm_58_triplet_pt4(p, q) * wm_interm_96_triplet_pt4(a, i)
end do 
end do 

term(535) = term(535) * 24.0d+0 
term(536) = term(536) * (-12.0d+0) 
term(537) = term(537) * 32.0d+0 
term(538) = term(538) * (-16.0d+0) 
term(539) = term(539) * (-16.0d+0) 
term(540) = term(540) * 8.0d+0 
term(541) = term(541) * 8.0d+0 
term(542) = term(542) * (-4.0d+0) 
term(543) = term(543) * (-16.0d+0) 
term(544) = term(544) * 8.0d+0 
term(545) = term(545) * (-24.0d+0) 
term(546) = term(546) * 12.0d+0 
term(547) = term(547) * 6.0d+0 
term(548) = term(548) * (-6.0d+0) 
term(549) = term(549) * (-12.0d+0) 
term(550) = term(550) * 12.0d+0 
term(551) = term(551) * 16.0d+0 
term(552) = term(552) * (-32.0d+0) 
term(553) = term(553) * (-8.0d+0) 
term(554) = term(554) * 16.0d+0 
term(555) = term(555) * (-8.0d+0) 
term(556) = term(556) * 16.0d+0 
term(557) = term(557) * 8.0d+0 
term(558) = term(558) * (-16.0d+0) 
term(559) = term(559) * 8.0d+0 
term(560) = term(560) * (-16.0d+0) 
term(561) = term(561) * 16.0d+0 
term(562) = term(562) * (-32.0d+0) 
term(563) = term(563) * (-4.0d+0) 
term(564) = term(564) * 8.0d+0 
term(565) = term(565) * (-4.0d+0) 
term(566) = term(566) * 8.0d+0 
term(567) = term(567) * (-6.0d+0) 
term(568) = term(568) * 12.0d+0 
term(569) = -term(569) 
term(570) = term(570) * 2.0d+0 
term(571) = -term(571) 
term(572) = term(572) * 2.0d+0 
term(573) = term(573) * (-24.0d+0) 
term(574) = term(574) * (-12.0d+0) 
term(575) = term(575) * 6.0d+0 
term(576) = term(576) * 12.0d+0 
term(577) = term(577) * 4.0d+0 
term(578) = term(578) * (-8.0d+0) 
term(579) = term(579) * (-8.0d+0) 
term(580) = term(580) * 16.0d+0 
term(581) = term(581) * 4.0d+0 
term(582) = term(582) * (-8.0d+0) 
term(583) = term(583) * (-8.0d+0) 
term(584) = term(584) * 16.0d+0 
term(585) = term(585) * (-32.0d+0) 
term(586) = term(586) * 16.0d+0 
term(587) = term(587) * 16.0d+0 
term(588) = term(588) * (-8.0d+0) 
term(589) = term(589) * (-8.0d+0) 
term(590) = term(590) * 4.0d+0 
term(591) = term(591) * 16.0d+0 
term(592) = term(592) * (-8.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
term(593) = term(593) + s1(q,i) * wm_interm_53_triplet_pt4(j, k) * wm_interm_64_triplet_pt4(p, k, j, i)
term(594) = term(594) + s1(q,i) * wm_interm_54_triplet_pt4(j, k) * wm_interm_64_triplet_pt4(p, k, j, i)
term(595) = term(595) + s1(q,i) * wm_interm_53_triplet_pt4(j, k) * wm_interm_71_triplet_pt4(p, k, j, i)
term(596) = term(596) + s1(q,i) * wm_interm_54_triplet_pt4(j, k) * wm_interm_71_triplet_pt4(p, k, j, i)
end do 
end do 
end do 

term(593) = term(593) * (-6.0d+0) 
term(594) = term(594) * 12.0d+0 
term(595) = term(595) * (-8.0d+0) 
term(596) = term(596) * 16.0d+0 

do l = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
term(597) = term(597) + r1(vrdav_Rl, q,i) * r2p(vrdav_Rr, p,j,a,k) * s2(a,b,k,l) * wm_interm_43_triplet_pt4(b, j, i, l)
term(598) = term(598) + r1(vrdav_Rl, q,i) * r2p(vrdav_Rr, p,j,a,k) * s2(a,b,l,k) * wm_interm_43_triplet_pt4(b, j, i, l)
term(599) = term(599) + r2p(vrdav_Rr, p,j,a,k) * s1(q,i) * s2(a,b,k,l) * wm_interm_51_triplet_pt4(b, j, i, l)
term(600) = term(600) + r2p(vrdav_Rr, p,j,a,k) * s1(q,i) * s2(a,b,l,k) * wm_interm_51_triplet_pt4(b, j, i, l)
term(601) = term(601) + r2p(vrdav_Rl, p,j,a,k) * t1(q,i) * t2(a,b,l,k) * wm_interm_45_triplet_pt4(b, j, i, l)
term(602) = term(602) + r2p(vrdav_Rl, p,j,a,k) * t1(q,i) * t2(a,b,k,l) * wm_interm_45_triplet_pt4(b, j, i, l)
end do 
end do 
end do 
end do 
end do 
end do 

term(597) = term(597) * (-24.0d+0) 
term(598) = term(598) * 12.0d+0 
term(599) = term(599) * (-24.0d+0) 
term(600) = term(600) * 12.0d+0 
term(601) = term(601) * (-6.0d+0) 
term(602) = term(602) * 12.0d+0 

do k = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do l = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(603) = term(603) + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, p,j,b,k) * s2(b,q,l,k) * wm_interm_69_triplet_pt4(a, j, l, i)
term(604) = term(604) + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, p,j,b,i) * s2(b,q,l,k) * wm_interm_69_triplet_pt4(a, j, l, k)
term(605) = term(605) + r2m(vrdav_Rr, a,j,p,k) * s1(q,i) * s2(a,b,l,k) * wm_interm_51_triplet_pt4(b, j, l, i)
term(606) = term(606) + r2m(vrdav_Rl, a,j,p,k) * t1(q,i) * t2(a,b,l,k) * wm_interm_45_triplet_pt4(b, j, l, i)
end do 
end do 
end do 
end do 
end do 
end do 

term(603) = term(603) * (-12.0d+0) 
term(604) = term(604) * 12.0d+0 
term(605) = term(605) * 8.0d+0 
term(606) = term(606) * (-4.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(607) = term(607) + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, p,j,b,k) * s2(b,q,k,l) * wm_interm_69_triplet_pt4(a, j, l, i)
term(608) = term(608) + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, p,j,b,i) * s2(b,q,l,k) * wm_interm_69_triplet_pt4(a, j, k, l)
term(609) = term(609) + r2m(vrdav_Rr, a,j,p,k) * s1(q,i) * s2(a,b,k,l) * wm_interm_51_triplet_pt4(b, j, l, i)
term(610) = term(610) + r2m(vrdav_Rr, a,j,p,k) * s1(q,i) * s2(a,b,j,l) * wm_interm_51_triplet_pt4(b, k, l, i)
term(611) = term(611) + r2m(vrdav_Rl, a,j,p,k) * t1(q,i) * t2(a,b,k,l) * wm_interm_45_triplet_pt4(b, j, l, i)
term(612) = term(612) + r2m(vrdav_Rl, a,j,p,k) * t1(q,i) * t2(a,b,j,l) * wm_interm_45_triplet_pt4(b, k, l, i)
end do 
end do 
end do 
end do 
end do 
end do 

term(607) = term(607) * 12.0d+0 
term(608) = term(608) * (-12.0d+0) 
term(609) = term(609) * (-4.0d+0) 
term(610) = term(610) * 8.0d+0 
term(611) = term(611) * 2.0d+0 
term(612) = term(612) * (-4.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do l = 1, nocc 
do j = 1, nocc 
term(613) = term(613) + r1(vrdav_Rl, q,i) * r2p(vrdav_Rr, p,j,a,k) * s2(a,b,l,k) * wm_interm_43_triplet_pt4(b, j, l, i)
term(614) = term(614) + r2p(vrdav_Rr, p,j,a,k) * s1(q,i) * s2(a,b,l,k) * wm_interm_51_triplet_pt4(b, j, l, i)
term(615) = term(615) + r2p(vrdav_Rl, p,j,a,k) * t1(q,i) * t2(a,b,l,k) * wm_interm_45_triplet_pt4(b, j, l, i)
end do 
end do 
end do 
end do 
end do 
end do 

term(613) = term(613) * (-12.0d+0) 
term(614) = term(614) * (-12.0d+0) 
term(615) = term(615) * 6.0d+0 

do l = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do j = 1, nocc 
term(616) = term(616) + r1(vrdav_Rl, q,i) * r2p(vrdav_Rr, p,j,a,k) * s2(a,b,k,l) * wm_interm_43_triplet_pt4(b, j, l, i)
term(617) = term(617) + r2p(vrdav_Rr, p,j,a,k) * s1(q,i) * s2(a,b,k,l) * wm_interm_51_triplet_pt4(b, j, l, i)
term(618) = term(618) + r2p(vrdav_Rl, p,j,a,k) * t1(q,i) * t2(a,b,k,l) * wm_interm_45_triplet_pt4(b, j, l, i)
end do 
end do 
end do 
end do 
end do 
end do 

term(616) = term(616) * 12.0d+0 
term(617) = term(617) * 12.0d+0 
term(618) = term(618) * (-6.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(619) = term(619) + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, p,j,b,k) * s2(b,q,l,k) * wm_interm_69_triplet_pt4(a, j, i, l)
term(620) = term(620) + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, p,j,b,k) * s2(b,q,k,l) * wm_interm_69_triplet_pt4(a, j, i, l)
term(621) = term(621) + r2m(vrdav_Rr, a,j,p,k) * s1(q,i) * s2(a,b,k,l) * wm_interm_51_triplet_pt4(b, j, i, l)
term(622) = term(622) + r2m(vrdav_Rr, a,j,p,k) * s1(q,i) * s2(a,b,j,l) * wm_interm_51_triplet_pt4(b, k, i, l)
term(623) = term(623) + r2m(vrdav_Rr, a,j,p,k) * s1(q,i) * s2(a,b,l,k) * wm_interm_51_triplet_pt4(b, j, i, l)
term(624) = term(624) + r2m(vrdav_Rl, a,j,q,k) * r1(vrdav_Rr, p,i) * t2(a,b,j,l) * wm_interm_62_triplet_pt4(b, l, i, k)
term(625) = term(625) + r2m(vrdav_Rl, a,j,p,k) * t1(q,i) * t2(a,b,l,k) * wm_interm_45_triplet_pt4(b, j, i, l)
term(626) = term(626) + r2m(vrdav_Rl, a,j,p,k) * t1(q,i) * t2(a,b,k,l) * wm_interm_45_triplet_pt4(b, j, i, l)
term(627) = term(627) + r2m(vrdav_Rl, a,j,p,k) * t1(q,i) * t2(a,b,j,l) * wm_interm_45_triplet_pt4(b, k, i, l)
end do 
end do 
end do 
end do 
end do 
end do 

term(619) = term(619) * 24.0d+0 
term(620) = term(620) * (-24.0d+0) 
term(621) = term(621) * 8.0d+0 
term(622) = term(622) * (-16.0d+0) 
term(623) = term(623) * (-4.0d+0) 
term(624) = term(624) * (-8.0d+0) 
term(625) = term(625) * 2.0d+0 
term(626) = term(626) * (-4.0d+0) 
term(627) = term(627) * 8.0d+0 

do l = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(628) = term(628) + r1(vrdav_Rl, q,i) * r2m(vrdav_Rr, a,k,p,j) * s2(a,b,j,l) * wm_interm_43_triplet_pt4(b, k, i, l)
term(629) = term(629) + r1(vrdav_Rl, q,i) * r2m(vrdav_Rr, a,k,p,j) * s2(a,b,k,l) * wm_interm_43_triplet_pt4(b, j, i, l)
term(630) = term(630) + r1(vrdav_Rl, q,i) * r2m(vrdav_Rr, a,k,p,j) * s2(a,b,l,j) * wm_interm_43_triplet_pt4(b, k, i, l)
term(631) = term(631) + r2m(vrdav_Rr, a,k,p,j) * s1(q,i) * s2(a,b,j,l) * wm_interm_51_triplet_pt4(b, k, i, l)
term(632) = term(632) + r2m(vrdav_Rr, a,k,p,j) * s1(q,i) * s2(a,b,k,l) * wm_interm_51_triplet_pt4(b, j, i, l)
term(633) = term(633) + r2m(vrdav_Rr, a,k,p,j) * s1(q,i) * s2(a,b,l,j) * wm_interm_51_triplet_pt4(b, k, i, l)
term(634) = term(634) + s1(p,i) * s2(a,b,k,l) * t2(b,q,k,j) * wm_interm_64_triplet_pt4(a, j, i, l)
term(635) = term(635) + s1(p,i) * s2(a,b,k,l) * t2(b,q,k,j) * wm_interm_71_triplet_pt4(a, j, i, l)
term(636) = term(636) + r1(vrdav_Rl, p,i) * s2(a,b,k,l) * t2(b,q,l,j) * wm_interm_14_triplet_pt4(a, k, i, j)
term(637) = term(637) + r1(vrdav_Rl, p,i) * s2(a,b,k,l) * t2(b,q,l,j) * wm_interm_79_triplet_pt4(a, k, i, j)
term(638) = term(638) + r1(vrdav_Rl, p,i) * s2(a,b,k,l) * t2(b,q,k,j) * wm_interm_14_triplet_pt4(a, l, i, j)
term(639) = term(639) + r1(vrdav_Rl, p,i) * s2(a,b,k,l) * t2(b,q,k,j) * wm_interm_79_triplet_pt4(a, l, i, j)
term(640) = term(640) + r1(vrdav_Rl, p,i) * s2(a,b,k,j) * t2(b,q,l,j) * wm_interm_79_triplet_pt4(a, k, i, l)
term(641) = term(641) + r1(vrdav_Rl, p,i) * s2(a,b,k,j) * t2(b,q,l,j) * wm_interm_14_triplet_pt4(a, k, i, l)
term(642) = term(642) + s2(a,p,k,j) * t1(q,i) * t2(a,b,k,l) * wm_interm_85_triplet_pt4(b, l, i, j)
term(643) = term(643) + r1(vrdav_Rr, p,i) * s2(a,q,k,j) * t2(a,b,k,l) * wm_interm_24_triplet_pt4(b, l, i, j)
term(644) = term(644) + r2m(vrdav_Rl, a,k,q,j) * r1(vrdav_Rr, p,i) * t2(a,b,k,l) * wm_interm_62_triplet_pt4(b, l, i, j)
term(645) = term(645) + r1(vrdav_Rr, p,i) * s2(a,q,k,j) * t2(a,b,k,l) * wm_interm_27_triplet_pt4(b, l, i, j)
term(646) = term(646) + r2m(vrdav_Rl, a,k,p,j) * t1(q,i) * t2(a,b,k,l) * wm_interm_45_triplet_pt4(b, j, i, l)
term(647) = term(647) + r2m(vrdav_Rl, a,k,p,j) * t1(q,i) * t2(a,b,l,j) * wm_interm_45_triplet_pt4(b, k, i, l)
term(648) = term(648) + s2(a,p,k,j) * t1(q,i) * t2(a,b,k,l) * wm_interm_92_triplet_pt4(b, l, i, j)
term(649) = term(649) + r2m(vrdav_Rl, a,k,p,j) * t1(q,i) * t2(a,b,j,l) * wm_interm_45_triplet_pt4(b, k, i, l)
term(650) = term(650) + s2(a,p,k,j) * t1(q,i) * t2(a,b,k,l) * wm_interm_94_triplet_pt4(b, l, i, j)
end do 
end do 
end do 
end do 
end do 
end do 

term(628) = term(628) * 16.0d+0 
term(629) = term(629) * (-32.0d+0) 
term(630) = term(630) * (-8.0d+0) 
term(631) = term(631) * 8.0d+0 
term(632) = term(632) * (-16.0d+0) 
term(633) = term(633) * (-4.0d+0) 
term(634) = term(634) * 6.0d+0 
term(635) = term(635) * 16.0d+0 
term(636) = term(636) * (-16.0d+0) 
term(637) = term(637) * 8.0d+0 
term(638) = term(638) * 8.0d+0 
term(639) = term(639) * (-4.0d+0) 
term(640) = term(640) * (-4.0d+0) 
term(641) = term(641) * 8.0d+0 
term(642) = term(642) * (-6.0d+0) 
term(643) = term(643) * 16.0d+0 
term(644) = term(644) * (-8.0d+0) 
term(645) = term(645) * (-8.0d+0) 
term(646) = term(646) * 8.0d+0 
term(647) = term(647) * 2.0d+0 
term(648) = term(648) * (-4.0d+0) 
term(649) = term(649) * (-4.0d+0) 
term(650) = term(650) * 8.0d+0 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(651) = term(651) + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, b,k,p,j) * s2(b,q,l,k) * wm_interm_69_triplet_pt4(a, j, l, i)
term(652) = term(652) + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, b,j,p,k) * s2(b,q,l,j) * wm_interm_69_triplet_pt4(a, k, l, i)
term(653) = term(653) + r1(vrdav_Rr, a,j) * s2(b,p,k,j) * t2(b,q,l,i) * wm_interm_7_triplet_pt4(a, i, l, k)
term(654) = term(654) + r1(vrdav_Rr, a,j) * s2(b,p,k,j) * t2(b,q,l,i) * wm_interm_7_triplet_pt4(a, l, i, k)
term(655) = term(655) + r2m(vrdav_Rl, a,k,p,j) * r1(vrdav_Rr, b,l) * t2(a,q,j,i) * wm_interm_60_triplet_pt4(b, i, l, k)
term(656) = term(656) + r2m(vrdav_Rl, a,k,p,j) * r1(vrdav_Rr, b,j) * t2(a,q,l,i) * wm_interm_60_triplet_pt4(b, i, l, k)
term(657) = term(657) + r2m(vrdav_Rl, a,k,p,j) * r1(vrdav_Rr, b,j) * t2(a,q,l,i) * wm_interm_60_triplet_pt4(b, l, i, k)
term(658) = term(658) + r2m(vrdav_Rl, a,k,p,j) * r1(vrdav_Rr, b,l) * t2(a,q,j,i) * wm_interm_60_triplet_pt4(b, l, i, k)
term(659) = term(659) + r1(vrdav_Rr, a,j) * s2(b,p,k,j) * t2(b,q,l,i) * wm_interm_24_triplet_pt4(a, i, l, k)
term(660) = term(660) + r1(vrdav_Rr, a,j) * s2(b,p,k,j) * t2(b,q,l,i) * wm_interm_24_triplet_pt4(a, l, i, k)
term(661) = term(661) + r2m(vrdav_Rl, a,j,p,k) * r1(vrdav_Rr, b,l) * t2(a,q,k,i) * wm_interm_60_triplet_pt4(b, i, l, j)
term(662) = term(662) + r2m(vrdav_Rl, a,j,p,k) * r1(vrdav_Rr, b,k) * t2(a,q,l,i) * wm_interm_60_triplet_pt4(b, i, l, j)
term(663) = term(663) + r2m(vrdav_Rl, a,j,p,k) * r1(vrdav_Rr, b,k) * t2(a,q,l,i) * wm_interm_60_triplet_pt4(b, l, i, j)
term(664) = term(664) + r2m(vrdav_Rl, a,j,p,k) * r1(vrdav_Rr, b,l) * t2(a,q,k,i) * wm_interm_60_triplet_pt4(b, l, i, j)
term(665) = term(665) + r1(vrdav_Rr, a,j) * s2(b,p,k,j) * t2(b,q,l,i) * wm_interm_27_triplet_pt4(a, i, l, k)
term(666) = term(666) + r1(vrdav_Rr, a,j) * s2(b,p,k,j) * t2(b,q,l,i) * wm_interm_27_triplet_pt4(a, l, i, k)
end do 
end do 
end do 
end do 
end do 
end do 

term(651) = term(651) * (-4.0d+0) 
term(652) = term(652) * (-4.0d+0) 
term(653) = term(653) * (-3.0d+0) 
term(654) = term(654) * 3.0d+0 
term(655) = term(655) * 2.0d+0 
term(656) = term(656) * (-4.0d+0) 
term(657) = term(657) * 2.0d+0 
term(658) = term(658) * (-4.0d+0) 
term(659) = term(659) * (-4.0d+0) 
term(660) = term(660) * 2.0d+0 
term(661) = term(661) * 2.0d+0 
term(662) = term(662) * (-4.0d+0) 
term(663) = term(663) * 2.0d+0 
term(664) = term(664) * (-4.0d+0) 
term(665) = term(665) * 2.0d+0 
term(666) = term(666) * (-4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(667) = term(667) + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, b,k,p,j) * s2(b,q,l,j) * wm_interm_69_triplet_pt4(a, k, l, i)
term(668) = term(668) + r2m(vrdav_Rl, a,j,p,i) * r1(vrdav_Rr, b,k) * t2(a,q,l,i) * wm_interm_60_triplet_pt4(b, k, l, j)
term(669) = term(669) + r2m(vrdav_Rl, a,i,p,j) * r1(vrdav_Rr, b,k) * t2(a,q,l,i) * wm_interm_60_triplet_pt4(b, k, l, j)
term(670) = term(670) + r2m(vrdav_Rl, a,k,p,j) * r1(vrdav_Rr, b,k) * t2(a,q,l,i) * wm_interm_60_triplet_pt4(b, i, l, j)
term(671) = term(671) + r2m(vrdav_Rl, a,k,p,j) * r1(vrdav_Rr, b,l) * t2(a,q,k,i) * wm_interm_60_triplet_pt4(b, i, l, j)
term(672) = term(672) + r2m(vrdav_Rl, a,k,p,j) * r1(vrdav_Rr, b,k) * t2(a,q,l,i) * wm_interm_60_triplet_pt4(b, l, i, j)
term(673) = term(673) + r2m(vrdav_Rl, a,k,p,j) * r1(vrdav_Rr, b,l) * t2(a,q,k,i) * wm_interm_60_triplet_pt4(b, l, i, j)
end do 
end do 
end do 
end do 
end do 
end do 

term(667) = term(667) * 8.0d+0 
term(668) = term(668) * 16.0d+0 
term(669) = term(669) * (-8.0d+0) 
term(670) = term(670) * 2.0d+0 
term(671) = term(671) * (-4.0d+0) 
term(672) = term(672) * (-4.0d+0) 
term(673) = term(673) * 8.0d+0 

do l = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(674) = term(674) + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, b,k,p,j) * s2(b,q,j,l) * wm_interm_69_triplet_pt4(a, k, l, i)
term(675) = term(675) + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, b,k,p,j) * s2(b,q,k,l) * wm_interm_69_triplet_pt4(a, j, l, i)
term(676) = term(676) + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, b,k,p,j) * s2(b,q,l,j) * wm_interm_69_triplet_pt4(a, k, i, l)
term(677) = term(677) + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, b,k,p,j) * s2(b,q,j,l) * wm_interm_69_triplet_pt4(a, k, i, l)
term(678) = term(678) + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, b,k,p,j) * s2(b,q,k,l) * wm_interm_69_triplet_pt4(a, j, i, l)
term(679) = term(679) + r1(vrdav_Rl, a,i) * s2(b,p,k,j) * t2(b,q,l,i) * wm_interm_8_triplet_pt4(a, j, k, l)
term(680) = term(680) + r1(vrdav_Rl, a,i) * s2(b,p,k,j) * t2(b,q,l,i) * wm_interm_8_triplet_pt4(a, k, j, l)
term(681) = term(681) + r1(vrdav_Rl, a,i) * s2(b,p,k,j) * t2(b,q,l,i) * wm_interm_14_triplet_pt4(a, k, j, l)
term(682) = term(682) + r1(vrdav_Rl, a,i) * s2(b,p,k,j) * t2(b,q,l,i) * wm_interm_14_triplet_pt4(a, j, k, l)
end do 
end do 
end do 
end do 
end do 
end do 

term(674) = term(674) * (-4.0d+0) 
term(675) = term(675) * 8.0d+0 
term(676) = term(676) * (-16.0d+0) 
term(677) = term(677) * 8.0d+0 
term(678) = term(678) * (-16.0d+0) 
term(679) = term(679) * 6.0d+0 
term(680) = term(680) * (-6.0d+0) 
term(681) = term(681) * (-8.0d+0) 
term(682) = term(682) * 16.0d+0 

do l = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(683) = term(683) + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, b,j,p,k) * s2(b,q,k,l) * wm_interm_69_triplet_pt4(a, j, l, i)
term(684) = term(684) + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, b,j,p,k) * s2(b,q,j,l) * wm_interm_69_triplet_pt4(a, k, l, i)
term(685) = term(685) + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, b,j,p,i) * s2(b,q,l,k) * wm_interm_69_triplet_pt4(a, j, k, l)
term(686) = term(686) + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, b,j,p,k) * s2(b,q,k,l) * wm_interm_69_triplet_pt4(a, j, i, l)
term(687) = term(687) + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, b,j,p,k) * s2(b,q,j,l) * wm_interm_69_triplet_pt4(a, k, i, l)
term(688) = term(688) + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, b,j,p,k) * s2(b,q,l,k) * wm_interm_69_triplet_pt4(a, j, i, l)
term(689) = term(689) + r1(vrdav_Rl, a,j) * s2(b,p,i,k) * t2(b,q,l,i) * wm_interm_8_triplet_pt4(a, j, k, l)
term(690) = term(690) + r1(vrdav_Rl, a,j) * s2(b,p,i,k) * t2(b,q,l,i) * wm_interm_14_triplet_pt4(a, k, j, l)
term(691) = term(691) + r1(vrdav_Rl, a,j) * s2(b,p,i,k) * t2(b,q,l,i) * wm_interm_14_triplet_pt4(a, j, k, l)
term(692) = term(692) + r1(vrdav_Rr, a,j) * s2(b,p,l,k) * t2(b,q,k,i) * wm_interm_7_triplet_pt4(a, j, i, l)
term(693) = term(693) + r1(vrdav_Rr, a,j) * s2(b,p,l,k) * t2(b,q,k,i) * wm_interm_24_triplet_pt4(a, i, j, l)
term(694) = term(694) + r1(vrdav_Rr, a,j) * s2(b,p,l,k) * t2(b,q,k,i) * wm_interm_24_triplet_pt4(a, j, i, l)
term(695) = term(695) + r1(vrdav_Rr, a,j) * s2(b,p,l,k) * t2(b,q,k,i) * wm_interm_27_triplet_pt4(a, i, j, l)
term(696) = term(696) + r1(vrdav_Rr, a,j) * s2(b,p,l,k) * t2(b,q,k,i) * wm_interm_27_triplet_pt4(a, j, i, l)
end do 
end do 
end do 
end do 
end do 
end do 

term(683) = term(683) * (-4.0d+0) 
term(684) = term(684) * 8.0d+0 
term(685) = term(685) * 16.0d+0 
term(686) = term(686) * 8.0d+0 
term(687) = term(687) * (-16.0d+0) 
term(688) = term(688) * (-16.0d+0) 
term(689) = term(689) * 12.0d+0 
term(690) = term(690) * (-8.0d+0) 
term(691) = term(691) * 16.0d+0 
term(692) = term(692) * (-6.0d+0) 
term(693) = term(693) * 2.0d+0 
term(694) = term(694) * (-4.0d+0) 
term(695) = term(695) * (-4.0d+0) 
term(696) = term(696) * 2.0d+0 

do k = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(697) = term(697) + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, b,j,p,k) * s2(b,q,l,k) * wm_interm_69_triplet_pt4(a, j, l, i)
term(698) = term(698) + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, b,j,p,i) * s2(b,q,l,k) * wm_interm_69_triplet_pt4(a, j, l, k)
term(699) = term(699) + r1(vrdav_Rl, a,j) * s2(b,p,l,k) * t2(b,q,k,i) * wm_interm_8_triplet_pt4(a, j, l, i)
term(700) = term(700) + r1(vrdav_Rl, a,j) * s2(b,p,l,k) * t2(b,q,j,i) * wm_interm_8_triplet_pt4(a, k, l, i)
term(701) = term(701) + r1(vrdav_Rl, a,j) * s2(b,p,l,k) * t2(b,q,l,i) * wm_interm_8_triplet_pt4(a, j, k, i)
term(702) = term(702) + r1(vrdav_Rl, a,j) * s2(b,p,l,k) * t2(b,q,j,i) * wm_interm_8_triplet_pt4(a, l, k, i)
term(703) = term(703) + r1(vrdav_Rl, a,j) * s2(b,p,l,k) * t2(b,q,k,i) * wm_interm_14_triplet_pt4(a, j, l, i)
term(704) = term(704) + r1(vrdav_Rl, a,j) * s2(b,p,l,k) * t2(b,q,j,i) * wm_interm_14_triplet_pt4(a, k, l, i)
term(705) = term(705) + r1(vrdav_Rl, a,j) * s2(b,p,l,k) * t2(b,q,j,i) * wm_interm_14_triplet_pt4(a, l, k, i)
term(706) = term(706) + r1(vrdav_Rl, a,j) * s2(b,p,l,k) * t2(b,q,l,i) * wm_interm_14_triplet_pt4(a, j, k, i)
term(707) = term(707) + r1(vrdav_Rr, a,j) * s2(b,p,j,k) * t2(b,q,l,i) * wm_interm_7_triplet_pt4(a, i, l, k)
term(708) = term(708) + r1(vrdav_Rr, a,j) * s2(b,p,l,k) * t2(b,q,l,i) * wm_interm_7_triplet_pt4(a, j, i, k)
term(709) = term(709) + r1(vrdav_Rr, a,j) * s2(b,p,k,i) * t2(b,q,l,i) * wm_interm_7_triplet_pt4(a, j, l, k)
term(710) = term(710) + r1(vrdav_Rr, a,j) * s2(b,p,i,k) * t2(b,q,l,i) * wm_interm_7_triplet_pt4(a, j, l, k)
term(711) = term(711) + r1(vrdav_Rr, a,j) * s2(b,p,j,k) * t2(b,q,l,i) * wm_interm_7_triplet_pt4(a, l, i, k)
term(712) = term(712) + r2m(vrdav_Rl, a,j,p,k) * r1(vrdav_Rr, b,j) * t2(a,q,l,i) * wm_interm_60_triplet_pt4(b, i, l, k)
term(713) = term(713) + r2m(vrdav_Rl, a,j,p,k) * r1(vrdav_Rr, b,l) * t2(a,q,j,i) * wm_interm_60_triplet_pt4(b, i, l, k)
term(714) = term(714) + r2m(vrdav_Rl, a,j,p,k) * r1(vrdav_Rr, b,j) * t2(a,q,l,i) * wm_interm_60_triplet_pt4(b, l, i, k)
term(715) = term(715) + r2m(vrdav_Rl, a,j,p,k) * r1(vrdav_Rr, b,l) * t2(a,q,j,i) * wm_interm_60_triplet_pt4(b, l, i, k)
term(716) = term(716) + r1(vrdav_Rr, a,j) * s2(b,p,j,k) * t2(b,q,l,i) * wm_interm_24_triplet_pt4(a, i, l, k)
term(717) = term(717) + r1(vrdav_Rr, a,j) * s2(b,p,j,k) * t2(b,q,l,i) * wm_interm_24_triplet_pt4(a, l, i, k)
term(718) = term(718) + r1(vrdav_Rr, a,j) * s2(b,p,k,i) * t2(b,q,l,i) * wm_interm_24_triplet_pt4(a, j, l, k)
term(719) = term(719) + r1(vrdav_Rr, a,j) * s2(b,p,i,k) * t2(b,q,l,i) * wm_interm_24_triplet_pt4(a, j, l, k)
term(720) = term(720) + r1(vrdav_Rr, a,j) * s2(b,p,l,k) * t2(b,q,l,i) * wm_interm_24_triplet_pt4(a, j, i, k)
term(721) = term(721) + r1(vrdav_Rr, a,j) * s2(b,p,k,i) * t2(b,q,l,i) * wm_interm_27_triplet_pt4(a, j, l, k)
term(722) = term(722) + r1(vrdav_Rr, a,j) * s2(b,p,i,k) * t2(b,q,l,i) * wm_interm_27_triplet_pt4(a, j, l, k)
term(723) = term(723) + r1(vrdav_Rr, a,j) * s2(b,p,j,k) * t2(b,q,l,i) * wm_interm_27_triplet_pt4(a, l, i, k)
term(724) = term(724) + r1(vrdav_Rr, a,j) * s2(b,p,l,k) * t2(b,q,l,i) * wm_interm_27_triplet_pt4(a, j, i, k)
term(725) = term(725) + r1(vrdav_Rr, a,j) * s2(b,p,j,k) * t2(b,q,l,i) * wm_interm_27_triplet_pt4(a, i, l, k)
end do 
end do 
end do 
end do 
end do 
end do 

term(697) = term(697) * 8.0d+0 
term(698) = term(698) * (-8.0d+0) 
term(699) = term(699) * 12.0d+0 
term(700) = term(700) * (-6.0d+0) 
term(701) = term(701) * (-24.0d+0) 
term(702) = term(702) * 6.0d+0 
term(703) = term(703) * 16.0d+0 
term(704) = term(704) * (-8.0d+0) 
term(705) = term(705) * 16.0d+0 
term(706) = term(706) * (-32.0d+0) 
term(707) = term(707) * 3.0d+0 
term(708) = term(708) * 12.0d+0 
term(709) = term(709) * 12.0d+0 
term(710) = term(710) * (-6.0d+0) 
term(711) = term(711) * (-3.0d+0) 
term(712) = term(712) * 2.0d+0 
term(713) = term(713) * (-4.0d+0) 
term(714) = term(714) * (-4.0d+0) 
term(715) = term(715) * 8.0d+0 
term(716) = term(716) * 2.0d+0 
term(717) = term(717) * (-4.0d+0) 
term(718) = term(718) * 8.0d+0 
term(719) = term(719) * (-4.0d+0) 
term(720) = term(720) * 8.0d+0 
term(721) = term(721) * (-4.0d+0) 
term(722) = term(722) * 2.0d+0 
term(723) = term(723) * 2.0d+0 
term(724) = term(724) * (-4.0d+0) 
term(725) = term(725) * (-4.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(726) = term(726) + r1(vrdav_Rl, q,i) * r2m(vrdav_Rr, a,k,p,j) * s2(a,b,l,k) * wm_interm_43_triplet_pt4(b, j, i, l)
term(727) = term(727) + r2m(vrdav_Rr, a,k,p,j) * s1(q,i) * s2(a,b,l,k) * wm_interm_51_triplet_pt4(b, j, i, l)
term(728) = term(728) + r2m(vrdav_Rr, a,j,p,k) * s1(q,i) * s2(a,b,l,j) * wm_interm_51_triplet_pt4(b, k, i, l)
term(729) = term(729) + s1(p,i) * s2(a,b,k,l) * t2(b,q,l,j) * wm_interm_64_triplet_pt4(a, j, i, k)
term(730) = term(730) + s1(p,i) * s2(a,b,k,l) * t2(b,q,l,j) * wm_interm_71_triplet_pt4(a, j, i, k)
term(731) = term(731) + r1(vrdav_Rl, p,i) * s2(a,b,j,k) * t2(b,q,l,j) * wm_interm_79_triplet_pt4(a, i, k, l)
term(732) = term(732) + r1(vrdav_Rl, p,i) * s2(a,b,j,k) * t2(b,q,l,j) * wm_interm_79_triplet_pt4(a, k, i, l)
term(733) = term(733) + r1(vrdav_Rl, p,i) * s2(a,b,j,k) * t2(b,q,l,j) * wm_interm_14_triplet_pt4(a, k, i, l)
term(734) = term(734) + r1(vrdav_Rl, p,i) * s2(a,b,j,k) * t2(b,q,l,j) * wm_interm_14_triplet_pt4(a, i, k, l)
term(735) = term(735) + s2(a,p,k,j) * t1(q,i) * t2(a,b,j,l) * wm_interm_85_triplet_pt4(b, i, l, k)
term(736) = term(736) + s2(a,p,k,j) * t1(q,i) * t2(a,b,j,l) * wm_interm_85_triplet_pt4(b, l, i, k)
term(737) = term(737) + r2m(vrdav_Rl, a,k,q,j) * r1(vrdav_Rr, p,i) * t2(a,b,j,l) * wm_interm_62_triplet_pt4(b, l, i, k)
term(738) = term(738) + r2m(vrdav_Rl, a,k,q,j) * r1(vrdav_Rr, p,i) * t2(a,b,j,l) * wm_interm_62_triplet_pt4(b, i, l, k)
term(739) = term(739) + r1(vrdav_Rr, p,i) * s2(a,q,k,j) * t2(a,b,j,l) * wm_interm_24_triplet_pt4(b, i, l, k)
term(740) = term(740) + r1(vrdav_Rr, p,i) * s2(a,q,k,j) * t2(a,b,j,l) * wm_interm_24_triplet_pt4(b, l, i, k)
term(741) = term(741) + r2m(vrdav_Rl, a,j,q,k) * r1(vrdav_Rr, p,i) * t2(a,b,k,l) * wm_interm_62_triplet_pt4(b, l, i, j)
term(742) = term(742) + r2m(vrdav_Rl, a,j,q,k) * r1(vrdav_Rr, p,i) * t2(a,b,k,l) * wm_interm_62_triplet_pt4(b, i, l, j)
term(743) = term(743) + r1(vrdav_Rr, p,i) * s2(a,q,k,j) * t2(a,b,j,l) * wm_interm_27_triplet_pt4(b, i, l, k)
term(744) = term(744) + r1(vrdav_Rr, p,i) * s2(a,q,k,j) * t2(a,b,j,l) * wm_interm_27_triplet_pt4(b, l, i, k)
term(745) = term(745) + r2m(vrdav_Rl, a,k,p,j) * t1(q,i) * t2(a,b,l,k) * wm_interm_45_triplet_pt4(b, j, i, l)
term(746) = term(746) + s2(a,p,k,j) * t1(q,i) * t2(a,b,j,l) * wm_interm_92_triplet_pt4(b, i, l, k)
term(747) = term(747) + r2m(vrdav_Rl, a,j,p,k) * t1(q,i) * t2(a,b,l,j) * wm_interm_45_triplet_pt4(b, k, i, l)
term(748) = term(748) + s2(a,p,k,j) * t1(q,i) * t2(a,b,j,l) * wm_interm_92_triplet_pt4(b, l, i, k)
term(749) = term(749) + s2(a,p,k,j) * t1(q,i) * t2(a,b,j,l) * wm_interm_94_triplet_pt4(b, i, l, k)
term(750) = term(750) + s2(a,p,k,j) * t1(q,i) * t2(a,b,j,l) * wm_interm_94_triplet_pt4(b, l, i, k)
end do 
end do 
end do 
end do 
end do 
end do 

term(726) = term(726) * 16.0d+0 
term(727) = term(727) * 8.0d+0 
term(728) = term(728) * 8.0d+0 
term(729) = term(729) * (-6.0d+0) 
term(730) = term(730) * (-32.0d+0) 
term(731) = term(731) * (-4.0d+0) 
term(732) = term(732) * 8.0d+0 
term(733) = term(733) * (-4.0d+0) 
term(734) = term(734) * 8.0d+0 
term(735) = term(735) * (-3.0d+0) 
term(736) = term(736) * 3.0d+0 
term(737) = term(737) * 4.0d+0 
term(738) = term(738) * (-8.0d+0) 
term(739) = term(739) * 4.0d+0 
term(740) = term(740) * (-8.0d+0) 
term(741) = term(741) * 4.0d+0 
term(742) = term(742) * (-8.0d+0) 
term(743) = term(743) * (-8.0d+0) 
term(744) = term(744) * 4.0d+0 
term(745) = term(745) * (-4.0d+0) 
term(746) = term(746) * (-4.0d+0) 
term(747) = term(747) * (-4.0d+0) 
term(748) = term(748) * 2.0d+0 
term(749) = term(749) * 2.0d+0 
term(750) = term(750) * (-4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do l = 1, nocc 
do a = nocc + 1, nactive 
term(751) = term(751) + r1(vrdav_Rl, q,i) * r2m(vrdav_Rr, a,k,p,j) * s2(a,b,l,k) * wm_interm_43_triplet_pt4(b, j, l, i)
term(752) = term(752) + r2m(vrdav_Rr, a,k,p,j) * s1(q,i) * s2(a,b,l,k) * wm_interm_51_triplet_pt4(b, j, l, i)
term(753) = term(753) + r2m(vrdav_Rr, a,j,p,k) * s1(q,i) * s2(a,b,l,j) * wm_interm_51_triplet_pt4(b, k, l, i)
term(754) = term(754) + s1(p,i) * s2(a,b,j,k) * t2(b,q,l,j) * wm_interm_64_triplet_pt4(a, l, k, i)
term(755) = term(755) + s1(p,i) * s2(a,b,j,k) * t2(b,q,l,j) * wm_interm_71_triplet_pt4(a, l, k, i)
term(756) = term(756) + r2m(vrdav_Rl, a,k,p,j) * t1(q,i) * t2(a,b,l,k) * wm_interm_45_triplet_pt4(b, j, l, i)
term(757) = term(757) + r2m(vrdav_Rl, a,j,p,k) * t1(q,i) * t2(a,b,l,j) * wm_interm_45_triplet_pt4(b, k, l, i)
end do 
end do 
end do 
end do 
end do 
end do 

term(751) = term(751) * (-8.0d+0) 
term(752) = term(752) * (-4.0d+0) 
term(753) = term(753) * (-4.0d+0) 
term(754) = term(754) * 6.0d+0 
term(755) = term(755) * 16.0d+0 
term(756) = term(756) * 2.0d+0 
term(757) = term(757) * 2.0d+0 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do l = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(758) = term(758) + r1(vrdav_Rl, q,i) * r2m(vrdav_Rr, a,k,p,j) * s2(a,b,l,j) * wm_interm_43_triplet_pt4(b, k, l, i)
term(759) = term(759) + r2m(vrdav_Rr, a,k,p,j) * s1(q,i) * s2(a,b,l,j) * wm_interm_51_triplet_pt4(b, k, l, i)
term(760) = term(760) + r2m(vrdav_Rl, a,k,p,j) * t1(q,i) * t2(a,b,l,j) * wm_interm_45_triplet_pt4(b, k, l, i)
end do 
end do 
end do 
end do 
end do 
end do 

term(758) = term(758) * 16.0d+0 
term(759) = term(759) * 8.0d+0 
term(760) = term(760) * (-4.0d+0) 

do l = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(761) = term(761) + r1(vrdav_Rl, q,i) * r2m(vrdav_Rr, a,k,p,j) * s2(a,b,j,l) * wm_interm_43_triplet_pt4(b, k, l, i)
term(762) = term(762) + r1(vrdav_Rl, q,i) * r2m(vrdav_Rr, a,k,p,j) * s2(a,b,k,l) * wm_interm_43_triplet_pt4(b, j, l, i)
term(763) = term(763) + r2m(vrdav_Rr, a,k,p,j) * s1(q,i) * s2(a,b,j,l) * wm_interm_51_triplet_pt4(b, k, l, i)
term(764) = term(764) + r2m(vrdav_Rr, a,k,p,j) * s1(q,i) * s2(a,b,k,l) * wm_interm_51_triplet_pt4(b, j, l, i)
term(765) = term(765) + s1(p,i) * s2(a,b,k,l) * t2(b,q,l,j) * wm_interm_64_triplet_pt4(a, j, k, i)
term(766) = term(766) + s1(p,i) * s2(a,b,k,l) * t2(b,q,k,j) * wm_interm_64_triplet_pt4(a, j, l, i)
term(767) = term(767) + s1(p,i) * s2(a,b,k,l) * t2(b,q,l,j) * wm_interm_71_triplet_pt4(a, j, k, i)
term(768) = term(768) + s1(p,i) * s2(a,b,k,l) * t2(b,q,k,j) * wm_interm_71_triplet_pt4(a, j, l, i)
term(769) = term(769) + r2m(vrdav_Rl, a,k,p,j) * t1(q,i) * t2(a,b,k,l) * wm_interm_45_triplet_pt4(b, j, l, i)
term(770) = term(770) + r2m(vrdav_Rl, a,k,p,j) * t1(q,i) * t2(a,b,j,l) * wm_interm_45_triplet_pt4(b, k, l, i)
end do 
end do 
end do 
end do 
end do 
end do 

term(761) = term(761) * (-8.0d+0) 
term(762) = term(762) * 16.0d+0 
term(763) = term(763) * (-4.0d+0) 
term(764) = term(764) * 8.0d+0 
term(765) = term(765) * 18.0d+0 
term(766) = term(766) * (-6.0d+0) 
term(767) = term(767) * 16.0d+0 
term(768) = term(768) * (-8.0d+0) 
term(769) = term(769) * (-4.0d+0) 
term(770) = term(770) * 2.0d+0 

do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(771) = term(771) + r2p(vrdav_Rl, q,i,a,j) * r2m(vrdav_Rr, a,j,p,k) * wm_interm_5_triplet_pt4(k, i)
end do 
end do 
end do 
end do 

term(771) = term(771) * (-12.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(772) = term(772) + r2m(vrdav_Rr, a,i,p,j) * s1(q,i) * wm_interm_0_triplet_pt4(a, j)
term(773) = term(773) + r2m(vrdav_Rr, a,i,p,j) * s1(q,i) * wm_interm_20_triplet_pt4(a, j)
term(774) = term(774) + r2m(vrdav_Rr, a,i,p,j) * s1(q,i) * wm_interm_21_triplet_pt4(a, j)
term(775) = term(775) + r2m(vrdav_Rl, a,i,p,j) * t1(q,i) * wm_interm_1_triplet_pt4(a, j)
term(776) = term(776) + r2m(vrdav_Rl, a,i,p,j) * t1(q,i) * wm_interm_2_triplet_pt4(a, j)
term(777) = term(777) + r2m(vrdav_Rl, a,i,p,j) * t1(q,i) * wm_interm_3_triplet_pt4(a, j)
term(778) = term(778) + s2(a,q,i,j) * wm_interm_1_triplet_pt4(p, i) * wm_interm_47_triplet_pt4(a, j)
term(779) = term(779) + s2(a,q,i,j) * wm_interm_1_triplet_pt4(p, i) * wm_interm_52_triplet_pt4(a, j)
term(780) = term(780) + s2(a,q,i,j) * wm_interm_2_triplet_pt4(p, i) * wm_interm_47_triplet_pt4(a, j)
term(781) = term(781) + s2(a,q,i,j) * wm_interm_3_triplet_pt4(p, i) * wm_interm_47_triplet_pt4(a, j)
term(782) = term(782) + s2(a,q,i,j) * wm_interm_2_triplet_pt4(p, i) * wm_interm_52_triplet_pt4(a, j)
term(783) = term(783) + s2(a,q,i,j) * wm_interm_3_triplet_pt4(p, i) * wm_interm_52_triplet_pt4(a, j)
term(784) = term(784) + s2(a,p,i,j) * wm_interm_2_triplet_pt4(a, j) * wm_interm_47_triplet_pt4(q, i)
term(785) = term(785) + s2(a,p,i,j) * wm_interm_3_triplet_pt4(a, j) * wm_interm_47_triplet_pt4(q, i)
term(786) = term(786) + s2(a,p,i,j) * wm_interm_2_triplet_pt4(a, j) * wm_interm_52_triplet_pt4(q, i)
term(787) = term(787) + s2(a,p,i,j) * wm_interm_3_triplet_pt4(a, j) * wm_interm_52_triplet_pt4(q, i)
term(788) = term(788) + s2(a,p,i,j) * wm_interm_42_triplet_pt4(q, i) * wm_interm_82_triplet_pt4(a, j)
term(789) = term(789) + s2(a,p,i,j) * wm_interm_48_triplet_pt4(q, i) * wm_interm_82_triplet_pt4(a, j)
term(790) = term(790) + s2(a,p,i,j) * wm_interm_42_triplet_pt4(q, i) * wm_interm_84_triplet_pt4(a, j)
term(791) = term(791) + s2(a,p,i,j) * wm_interm_42_triplet_pt4(q, i) * wm_interm_83_triplet_pt4(a, j)
term(792) = term(792) + s2(a,p,i,j) * wm_interm_48_triplet_pt4(q, i) * wm_interm_84_triplet_pt4(a, j)
term(793) = term(793) + s2(a,p,i,j) * wm_interm_48_triplet_pt4(q, i) * wm_interm_83_triplet_pt4(a, j)
term(794) = term(794) + t2(a,q,i,j) * wm_interm_0_triplet_pt4(a, j) * wm_interm_44_triplet_pt4(p, i)
term(795) = term(795) + t2(a,q,i,j) * wm_interm_0_triplet_pt4(a, j) * wm_interm_46_triplet_pt4(p, i)
term(796) = term(796) + t2(a,q,i,j) * wm_interm_20_triplet_pt4(a, j) * wm_interm_44_triplet_pt4(p, i)
term(797) = term(797) + t2(a,q,i,j) * wm_interm_20_triplet_pt4(a, j) * wm_interm_46_triplet_pt4(p, i)
term(798) = term(798) + t2(a,q,i,j) * wm_interm_21_triplet_pt4(a, j) * wm_interm_44_triplet_pt4(p, i)
term(799) = term(799) + t2(a,q,i,j) * wm_interm_21_triplet_pt4(a, j) * wm_interm_46_triplet_pt4(p, i)
term(800) = term(800) + t2(a,q,i,j) * wm_interm_20_triplet_pt4(p, i) * wm_interm_44_triplet_pt4(a, j)
term(801) = term(801) + t2(a,q,i,j) * wm_interm_20_triplet_pt4(p, i) * wm_interm_46_triplet_pt4(a, j)
term(802) = term(802) + t2(a,q,i,j) * wm_interm_21_triplet_pt4(p, i) * wm_interm_44_triplet_pt4(a, j)
term(803) = term(803) + t2(a,q,i,j) * wm_interm_21_triplet_pt4(p, i) * wm_interm_46_triplet_pt4(a, j)
end do 
end do 
end do 

term(772) = term(772) * (-12.0d+0) 
term(773) = term(773) * (-16.0d+0) 
term(774) = term(774) * 8.0d+0 
term(775) = term(775) * 12.0d+0 
term(776) = term(776) * 16.0d+0 
term(777) = term(777) * (-8.0d+0) 
term(778) = term(778) * 6.0d+0 
term(779) = term(779) * (-12.0d+0) 
term(780) = term(780) * 8.0d+0 
term(781) = term(781) * (-4.0d+0) 
term(782) = term(782) * (-16.0d+0) 
term(783) = term(783) * 8.0d+0 
term(784) = term(784) * 8.0d+0 
term(785) = term(785) * (-4.0d+0) 
term(786) = term(786) * (-16.0d+0) 
term(787) = term(787) * 8.0d+0 
term(788) = term(788) * 12.0d+0 
term(789) = term(789) * (-24.0d+0) 
term(790) = term(790) * (-8.0d+0) 
term(791) = term(791) * 16.0d+0 
term(792) = term(792) * 16.0d+0 
term(793) = term(793) * (-32.0d+0) 
term(794) = term(794) * (-2.0d+0) 
term(795) = term(795) * 4.0d+0 
term(796) = term(796) * (-8.0d+0) 
term(797) = term(797) * 16.0d+0 
term(798) = term(798) * 4.0d+0 
term(799) = term(799) * (-8.0d+0) 
term(800) = term(800) * (-8.0d+0) 
term(801) = term(801) * 16.0d+0 
term(802) = term(802) * 4.0d+0 
term(803) = term(803) * (-8.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(804) = term(804) + r2m(vrdav_Rr, a,j,p,i) * s1(q,i) * wm_interm_0_triplet_pt4(a, j)
term(805) = term(805) + r2m(vrdav_Rr, a,j,p,i) * s1(q,i) * wm_interm_20_triplet_pt4(a, j)
term(806) = term(806) + r2m(vrdav_Rr, a,j,p,i) * s1(q,i) * wm_interm_21_triplet_pt4(a, j)
term(807) = term(807) + r2m(vrdav_Rl, a,j,p,i) * t1(q,i) * wm_interm_1_triplet_pt4(a, j)
term(808) = term(808) + r2m(vrdav_Rl, a,j,p,i) * t1(q,i) * wm_interm_2_triplet_pt4(a, j)
term(809) = term(809) + r2m(vrdav_Rl, a,j,p,i) * t1(q,i) * wm_interm_3_triplet_pt4(a, j)
term(810) = term(810) + s2(a,q,j,i) * wm_interm_1_triplet_pt4(p, i) * wm_interm_47_triplet_pt4(a, j)
term(811) = term(811) + s2(a,q,j,i) * wm_interm_1_triplet_pt4(p, i) * wm_interm_52_triplet_pt4(a, j)
term(812) = term(812) + s2(a,q,j,i) * wm_interm_2_triplet_pt4(p, i) * wm_interm_47_triplet_pt4(a, j)
term(813) = term(813) + s2(a,q,j,i) * wm_interm_3_triplet_pt4(p, i) * wm_interm_47_triplet_pt4(a, j)
term(814) = term(814) + s2(a,q,j,i) * wm_interm_2_triplet_pt4(p, i) * wm_interm_52_triplet_pt4(a, j)
term(815) = term(815) + s2(a,q,j,i) * wm_interm_3_triplet_pt4(p, i) * wm_interm_52_triplet_pt4(a, j)
term(816) = term(816) + s2(a,p,j,i) * wm_interm_2_triplet_pt4(a, j) * wm_interm_47_triplet_pt4(q, i)
term(817) = term(817) + s2(a,p,j,i) * wm_interm_2_triplet_pt4(a, j) * wm_interm_52_triplet_pt4(q, i)
term(818) = term(818) + s2(a,p,j,i) * wm_interm_3_triplet_pt4(a, j) * wm_interm_47_triplet_pt4(q, i)
term(819) = term(819) + s2(a,p,j,i) * wm_interm_3_triplet_pt4(a, j) * wm_interm_52_triplet_pt4(q, i)
term(820) = term(820) + s2(a,p,j,i) * wm_interm_42_triplet_pt4(q, i) * wm_interm_82_triplet_pt4(a, j)
term(821) = term(821) + s2(a,p,j,i) * wm_interm_48_triplet_pt4(q, i) * wm_interm_82_triplet_pt4(a, j)
term(822) = term(822) + s2(a,p,j,i) * wm_interm_42_triplet_pt4(q, i) * wm_interm_84_triplet_pt4(a, j)
term(823) = term(823) + s2(a,p,j,i) * wm_interm_42_triplet_pt4(q, i) * wm_interm_83_triplet_pt4(a, j)
term(824) = term(824) + s2(a,p,j,i) * wm_interm_48_triplet_pt4(q, i) * wm_interm_84_triplet_pt4(a, j)
term(825) = term(825) + s2(a,p,j,i) * wm_interm_48_triplet_pt4(q, i) * wm_interm_83_triplet_pt4(a, j)
term(826) = term(826) + t2(a,q,j,i) * wm_interm_0_triplet_pt4(a, j) * wm_interm_46_triplet_pt4(p, i)
term(827) = term(827) + t2(a,q,j,i) * wm_interm_0_triplet_pt4(a, j) * wm_interm_44_triplet_pt4(p, i)
term(828) = term(828) + t2(a,q,j,i) * wm_interm_20_triplet_pt4(a, j) * wm_interm_46_triplet_pt4(p, i)
term(829) = term(829) + t2(a,q,j,i) * wm_interm_21_triplet_pt4(a, j) * wm_interm_46_triplet_pt4(p, i)
term(830) = term(830) + t2(a,q,j,i) * wm_interm_20_triplet_pt4(a, j) * wm_interm_44_triplet_pt4(p, i)
term(831) = term(831) + t2(a,q,j,i) * wm_interm_21_triplet_pt4(a, j) * wm_interm_44_triplet_pt4(p, i)
term(832) = term(832) + t2(a,q,j,i) * wm_interm_20_triplet_pt4(p, i) * wm_interm_46_triplet_pt4(a, j)
term(833) = term(833) + t2(a,q,j,i) * wm_interm_21_triplet_pt4(p, i) * wm_interm_46_triplet_pt4(a, j)
term(834) = term(834) + t2(a,q,j,i) * wm_interm_20_triplet_pt4(p, i) * wm_interm_44_triplet_pt4(a, j)
term(835) = term(835) + t2(a,q,j,i) * wm_interm_21_triplet_pt4(p, i) * wm_interm_44_triplet_pt4(a, j)
end do 
end do 
end do 

term(804) = term(804) * 24.0d+0 
term(805) = term(805) * 32.0d+0 
term(806) = term(806) * (-16.0d+0) 
term(807) = term(807) * (-24.0d+0) 
term(808) = term(808) * (-32.0d+0) 
term(809) = term(809) * 16.0d+0 
term(810) = term(810) * (-12.0d+0) 
term(811) = term(811) * 24.0d+0 
term(812) = term(812) * (-16.0d+0) 
term(813) = term(813) * 8.0d+0 
term(814) = term(814) * 32.0d+0 
term(815) = term(815) * (-16.0d+0) 
term(816) = term(816) * (-16.0d+0) 
term(817) = term(817) * 32.0d+0 
term(818) = term(818) * 8.0d+0 
term(819) = term(819) * (-16.0d+0) 
term(820) = term(820) * (-24.0d+0) 
term(821) = term(821) * 48.0d+0 
term(822) = term(822) * 16.0d+0 
term(823) = term(823) * (-32.0d+0) 
term(824) = term(824) * (-32.0d+0) 
term(825) = term(825) * 64.0d+0 
term(826) = term(826) * (-8.0d+0) 
term(827) = term(827) * 4.0d+0 
term(828) = term(828) * (-32.0d+0) 
term(829) = term(829) * 16.0d+0 
term(830) = term(830) * 16.0d+0 
term(831) = term(831) * (-8.0d+0) 
term(832) = term(832) * (-32.0d+0) 
term(833) = term(833) * 16.0d+0 
term(834) = term(834) * 16.0d+0 
term(835) = term(835) * (-8.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do l = 1, nocc 
do a = nocc + 1, nactive 
term(836) = term(836) + s1(p,i) * s2(a,b,k,j) * t2(b,q,l,j) * wm_interm_64_triplet_pt4(a, l, k, i)
term(837) = term(837) + s1(p,i) * s2(a,b,k,j) * t2(b,q,l,j) * wm_interm_71_triplet_pt4(a, l, k, i)
end do 
end do 
end do 
end do 
end do 
end do 

term(836) = term(836) * (-6.0d+0) 
term(837) = term(837) * (-8.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do l = 1, nocc 
do a = nocc + 1, nactive 
term(838) = term(838) + s1(p,i) * s2(a,b,k,j) * t2(b,q,l,j) * wm_interm_64_triplet_pt4(a, l, i, k)
term(839) = term(839) + s1(p,i) * s2(a,b,j,k) * t2(b,q,l,j) * wm_interm_64_triplet_pt4(a, l, i, k)
term(840) = term(840) + s1(p,i) * s2(a,b,j,k) * t2(b,q,l,j) * wm_interm_71_triplet_pt4(a, l, i, k)
term(841) = term(841) + s1(p,i) * s2(a,b,k,j) * t2(b,q,l,j) * wm_interm_71_triplet_pt4(a, l, i, k)
term(842) = term(842) + s2(a,p,k,j) * t1(q,i) * t2(a,b,l,j) * wm_interm_85_triplet_pt4(b, l, i, k)
term(843) = term(843) + s2(a,p,k,j) * t1(q,i) * t2(a,b,l,k) * wm_interm_85_triplet_pt4(b, l, i, j)
term(844) = term(844) + r2m(vrdav_Rl, a,j,q,k) * r1(vrdav_Rr, p,i) * t2(a,b,l,j) * wm_interm_60_triplet_pt4(b, l, i, k)
term(845) = term(845) + r2m(vrdav_Rl, a,k,q,j) * r1(vrdav_Rr, p,i) * t2(a,b,l,j) * wm_interm_60_triplet_pt4(b, l, i, k)
term(846) = term(846) + r1(vrdav_Rr, p,i) * s2(a,q,k,j) * t2(a,b,l,k) * wm_interm_24_triplet_pt4(b, l, i, j)
term(847) = term(847) + r1(vrdav_Rr, p,i) * s2(a,q,k,j) * t2(a,b,l,j) * wm_interm_24_triplet_pt4(b, l, i, k)
term(848) = term(848) + r2m(vrdav_Rl, a,k,q,j) * r1(vrdav_Rr, p,i) * t2(a,b,l,k) * wm_interm_62_triplet_pt4(b, l, i, j)
term(849) = term(849) + r2m(vrdav_Rl, a,j,q,k) * r1(vrdav_Rr, p,i) * t2(a,b,l,k) * wm_interm_62_triplet_pt4(b, l, i, j)
term(850) = term(850) + r1(vrdav_Rr, p,i) * s2(a,q,k,j) * t2(a,b,l,j) * wm_interm_27_triplet_pt4(b, l, i, k)
term(851) = term(851) + r1(vrdav_Rr, p,i) * s2(a,q,k,j) * t2(a,b,l,k) * wm_interm_27_triplet_pt4(b, l, i, j)
term(852) = term(852) + s2(a,p,k,j) * t1(q,i) * t2(a,b,l,j) * wm_interm_94_triplet_pt4(b, l, i, k)
term(853) = term(853) + s2(a,p,k,j) * t1(q,i) * t2(a,b,l,j) * wm_interm_92_triplet_pt4(b, l, i, k)
term(854) = term(854) + s2(a,p,k,j) * t1(q,i) * t2(a,b,l,k) * wm_interm_94_triplet_pt4(b, l, i, j)
term(855) = term(855) + s2(a,p,k,j) * t1(q,i) * t2(a,b,l,k) * wm_interm_92_triplet_pt4(b, l, i, j)
end do 
end do 
end do 
end do 
end do 
end do 

term(838) = term(838) * 6.0d+0 
term(839) = term(839) * (-6.0d+0) 
term(840) = term(840) * (-8.0d+0) 
term(841) = term(841) * 16.0d+0 
term(842) = term(842) * (-3.0d+0) 
term(843) = term(843) * 3.0d+0 
term(844) = term(844) * (-8.0d+0) 
term(845) = term(845) * 4.0d+0 
term(846) = term(846) * (-8.0d+0) 
term(847) = term(847) * 4.0d+0 
term(848) = term(848) * 4.0d+0 
term(849) = term(849) * (-8.0d+0) 
term(850) = term(850) * (-8.0d+0) 
term(851) = term(851) * 4.0d+0 
term(852) = term(852) * 2.0d+0 
term(853) = term(853) * (-4.0d+0) 
term(854) = term(854) * (-4.0d+0) 
term(855) = term(855) * 2.0d+0 

do l = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(856) = term(856) + r1(vrdav_Rl, a,j) * s2(b,p,k,i) * t2(b,q,l,i) * wm_interm_8_triplet_pt4(a, j, k, l)
term(857) = term(857) + r1(vrdav_Rl, a,j) * s2(b,p,k,i) * t2(b,q,l,i) * wm_interm_14_triplet_pt4(a, j, k, l)
end do 
end do 
end do 
end do 
end do 
end do 

term(856) = term(856) * (-24.0d+0) 
term(857) = term(857) * (-32.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
term(858) = term(858) + s1(q,i) * wm_interm_53_triplet_pt4(j, k) * wm_interm_71_triplet_pt4(p, k, i, j)
term(859) = term(859) + s1(q,i) * wm_interm_54_triplet_pt4(j, k) * wm_interm_71_triplet_pt4(p, k, i, j)
term(860) = term(860) + t1(q,i) * wm_interm_53_triplet_pt4(j, k) * wm_interm_85_triplet_pt4(p, k, i, j)
term(861) = term(861) + t1(q,i) * wm_interm_54_triplet_pt4(j, k) * wm_interm_85_triplet_pt4(p, k, i, j)
term(862) = term(862) + t1(q,i) * wm_interm_53_triplet_pt4(j, k) * wm_interm_94_triplet_pt4(p, k, i, j)
term(863) = term(863) + t1(q,i) * wm_interm_53_triplet_pt4(j, k) * wm_interm_92_triplet_pt4(p, k, i, j)
term(864) = term(864) + t1(q,i) * wm_interm_54_triplet_pt4(j, k) * wm_interm_94_triplet_pt4(p, k, i, j)
term(865) = term(865) + t1(q,i) * wm_interm_54_triplet_pt4(j, k) * wm_interm_92_triplet_pt4(p, k, i, j)
end do 
end do 
end do 

term(858) = term(858) * 4.0d+0 
term(859) = term(859) * (-8.0d+0) 
term(860) = term(860) * (-3.0d+0) 
term(861) = term(861) * 6.0d+0 
term(862) = term(862) * 2.0d+0 
term(863) = term(863) * (-4.0d+0) 
term(864) = term(864) * (-4.0d+0) 
term(865) = term(865) * 8.0d+0 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(866) = term(866) + r2m(vrdav_Rl, a,j,q,i) * r2m(vrdav_Rr, a,i,p,k) * wm_interm_5_triplet_pt4(k, j)
term(867) = term(867) + r2m(vrdav_Rl, a,j,q,i) * r2m(vrdav_Rr, a,j,p,k) * wm_interm_5_triplet_pt4(k, i)
term(868) = term(868) + s2(a,q,j,i) * wm_interm_47_triplet_pt4(a, k) * wm_interm_8_triplet_pt4(p, j, i, k)
term(869) = term(869) + s2(a,q,j,i) * wm_interm_47_triplet_pt4(a, k) * wm_interm_8_triplet_pt4(p, i, j, k)
term(870) = term(870) + s2(a,q,j,i) * wm_interm_52_triplet_pt4(a, k) * wm_interm_8_triplet_pt4(p, j, i, k)
term(871) = term(871) + s2(a,q,j,i) * wm_interm_52_triplet_pt4(a, k) * wm_interm_8_triplet_pt4(p, i, j, k)
term(872) = term(872) + s2(a,q,j,i) * wm_interm_14_triplet_pt4(p, j, i, k) * wm_interm_47_triplet_pt4(a, k)
term(873) = term(873) + s2(a,q,j,i) * wm_interm_14_triplet_pt4(p, i, j, k) * wm_interm_47_triplet_pt4(a, k)
term(874) = term(874) + s2(a,q,j,i) * wm_interm_47_triplet_pt4(a, k) * wm_interm_79_triplet_pt4(p, i, j, k)
term(875) = term(875) + s2(a,q,j,i) * wm_interm_47_triplet_pt4(a, k) * wm_interm_79_triplet_pt4(p, j, i, k)
term(876) = term(876) + s2(a,q,j,i) * wm_interm_14_triplet_pt4(p, j, i, k) * wm_interm_52_triplet_pt4(a, k)
term(877) = term(877) + s2(a,q,j,i) * wm_interm_14_triplet_pt4(p, i, j, k) * wm_interm_52_triplet_pt4(a, k)
term(878) = term(878) + s2(a,q,j,i) * wm_interm_52_triplet_pt4(a, k) * wm_interm_79_triplet_pt4(p, i, j, k)
term(879) = term(879) + s2(a,q,j,i) * wm_interm_52_triplet_pt4(a, k) * wm_interm_79_triplet_pt4(p, j, i, k)
term(880) = term(880) + t2(a,q,j,i) * wm_interm_60_triplet_pt4(p, j, i, k) * wm_interm_86_triplet_pt4(a, k)
term(881) = term(881) + t2(a,q,j,i) * wm_interm_60_triplet_pt4(p, i, j, k) * wm_interm_86_triplet_pt4(a, k)
term(882) = term(882) + t2(a,q,j,i) * wm_interm_44_triplet_pt4(a, k) * wm_interm_7_triplet_pt4(p, j, i, k)
term(883) = term(883) + t2(a,q,j,i) * wm_interm_44_triplet_pt4(a, k) * wm_interm_7_triplet_pt4(p, i, j, k)
term(884) = term(884) + t2(a,q,j,i) * wm_interm_46_triplet_pt4(a, k) * wm_interm_7_triplet_pt4(p, j, i, k)
term(885) = term(885) + t2(a,q,j,i) * wm_interm_46_triplet_pt4(a, k) * wm_interm_7_triplet_pt4(p, i, j, k)
term(886) = term(886) + t2(a,q,j,i) * wm_interm_60_triplet_pt4(p, j, i, k) * wm_interm_96_triplet_pt4(a, k)
term(887) = term(887) + t2(a,q,j,i) * wm_interm_60_triplet_pt4(p, i, j, k) * wm_interm_96_triplet_pt4(a, k)
term(888) = term(888) + t2(a,q,j,i) * wm_interm_60_triplet_pt4(p, j, i, k) * wm_interm_93_triplet_pt4(a, k)
term(889) = term(889) + t2(a,q,j,i) * wm_interm_60_triplet_pt4(p, i, j, k) * wm_interm_93_triplet_pt4(a, k)
term(890) = term(890) + t2(a,q,j,i) * wm_interm_24_triplet_pt4(p, j, i, k) * wm_interm_44_triplet_pt4(a, k)
term(891) = term(891) + t2(a,q,j,i) * wm_interm_24_triplet_pt4(p, i, j, k) * wm_interm_44_triplet_pt4(a, k)
term(892) = term(892) + t2(a,q,j,i) * wm_interm_24_triplet_pt4(p, j, i, k) * wm_interm_46_triplet_pt4(a, k)
term(893) = term(893) + t2(a,q,j,i) * wm_interm_24_triplet_pt4(p, i, j, k) * wm_interm_46_triplet_pt4(a, k)
term(894) = term(894) + t2(a,q,j,i) * wm_interm_27_triplet_pt4(p, i, j, k) * wm_interm_44_triplet_pt4(a, k)
term(895) = term(895) + t2(a,q,j,i) * wm_interm_27_triplet_pt4(p, j, i, k) * wm_interm_44_triplet_pt4(a, k)
term(896) = term(896) + t2(a,q,j,i) * wm_interm_27_triplet_pt4(p, i, j, k) * wm_interm_46_triplet_pt4(a, k)
term(897) = term(897) + t2(a,q,j,i) * wm_interm_27_triplet_pt4(p, j, i, k) * wm_interm_46_triplet_pt4(a, k)
end do 
end do 
end do 
end do 

term(866) = term(866) * 4.0d+0 
term(867) = term(867) * (-8.0d+0) 
term(868) = term(868) * (-6.0d+0) 
term(869) = term(869) * 6.0d+0 
term(870) = term(870) * 12.0d+0 
term(871) = term(871) * (-12.0d+0) 
term(872) = term(872) * (-4.0d+0) 
term(873) = term(873) * 8.0d+0 
term(874) = term(874) * (-4.0d+0) 
term(875) = term(875) * 8.0d+0 
term(876) = term(876) * 8.0d+0 
term(877) = term(877) * (-16.0d+0) 
term(878) = term(878) * 8.0d+0 
term(879) = term(879) * (-16.0d+0) 
term(880) = term(880) * (-6.0d+0) 
term(881) = term(881) * 12.0d+0 
term(882) = term(882) * (-3.0d+0) 
term(883) = term(883) * 3.0d+0 
term(884) = term(884) * 6.0d+0 
term(885) = term(885) * (-6.0d+0) 
term(886) = term(886) * 4.0d+0 
term(887) = term(887) * (-8.0d+0) 
term(888) = term(888) * (-8.0d+0) 
term(889) = term(889) * 16.0d+0 
term(890) = term(890) * 2.0d+0 
term(891) = term(891) * (-4.0d+0) 
term(892) = term(892) * (-4.0d+0) 
term(893) = term(893) * 8.0d+0 
term(894) = term(894) * 2.0d+0 
term(895) = term(895) * (-4.0d+0) 
term(896) = term(896) * (-4.0d+0) 
term(897) = term(897) * 8.0d+0 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(898) = term(898) + r2m(vrdav_Rl, a,j,q,i) * r2m(vrdav_Rr, a,k,p,i) * wm_interm_5_triplet_pt4(k, j)
end do 
end do 
end do 
end do 

term(898) = term(898) * (-8.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do l = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(899) = term(899) + r1(vrdav_Rl, a,j) * s2(b,p,l,k) * t2(b,q,k,i) * wm_interm_14_triplet_pt4(a, l, j, i)
term(900) = term(900) + r1(vrdav_Rl, a,j) * s2(b,p,l,k) * t2(b,q,l,i) * wm_interm_14_triplet_pt4(a, k, j, i)
term(901) = term(901) + r1(vrdav_Rr, a,j) * s2(b,p,l,k) * t2(b,q,l,i) * wm_interm_24_triplet_pt4(a, i, j, k)
term(902) = term(902) + r1(vrdav_Rr, a,j) * s2(b,p,k,i) * t2(b,q,l,i) * wm_interm_24_triplet_pt4(a, l, j, k)
term(903) = term(903) + r1(vrdav_Rr, a,j) * s2(b,p,i,k) * t2(b,q,l,i) * wm_interm_24_triplet_pt4(a, l, j, k)
term(904) = term(904) + r1(vrdav_Rr, a,j) * s2(b,p,k,i) * t2(b,q,l,i) * wm_interm_27_triplet_pt4(a, l, j, k)
term(905) = term(905) + r1(vrdav_Rr, a,j) * s2(b,p,i,k) * t2(b,q,l,i) * wm_interm_27_triplet_pt4(a, l, j, k)
term(906) = term(906) + r1(vrdav_Rr, a,j) * s2(b,p,l,k) * t2(b,q,l,i) * wm_interm_27_triplet_pt4(a, i, j, k)
end do 
end do 
end do 
end do 
end do 
end do 

term(899) = term(899) * (-8.0d+0) 
term(900) = term(900) * 16.0d+0 
term(901) = term(901) * (-4.0d+0) 
term(902) = term(902) * (-4.0d+0) 
term(903) = term(903) * 2.0d+0 
term(904) = term(904) * 8.0d+0 
term(905) = term(905) * (-4.0d+0) 
term(906) = term(906) * 8.0d+0 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(907) = term(907) + s1(p,i) * wm_interm_51_triplet_pt4(q, j, i, k) * wm_interm_67_triplet_pt4(j, k)
term(908) = term(908) + s1(p,i) * wm_interm_51_triplet_pt4(q, j, i, k) * wm_interm_68_triplet_pt4(j, k)
term(909) = term(909) + r1(vrdav_Rl, p,i) * wm_interm_43_triplet_pt4(q, j, i, k) * wm_interm_67_triplet_pt4(j, k)
term(910) = term(910) + r1(vrdav_Rl, p,i) * wm_interm_43_triplet_pt4(q, j, i, k) * wm_interm_68_triplet_pt4(j, k)
term(911) = term(911) + s1(p,i) * wm_interm_51_triplet_pt4(q, j, i, k) * wm_interm_75_triplet_pt4(j, k)
term(912) = term(912) + s1(p,i) * wm_interm_51_triplet_pt4(q, j, i, k) * wm_interm_76_triplet_pt4(j, k)
term(913) = term(913) + s1(p,i) * wm_interm_51_triplet_pt4(q, j, i, k) * wm_interm_77_triplet_pt4(j, k)
term(914) = term(914) + s1(p,i) * wm_interm_51_triplet_pt4(q, j, i, k) * wm_interm_78_triplet_pt4(j, k)
term(915) = term(915) + r1(vrdav_Rl, p,i) * wm_interm_43_triplet_pt4(q, j, i, k) * wm_interm_75_triplet_pt4(j, k)
term(916) = term(916) + r1(vrdav_Rl, p,i) * wm_interm_43_triplet_pt4(q, j, i, k) * wm_interm_76_triplet_pt4(j, k)
term(917) = term(917) + r1(vrdav_Rl, p,i) * wm_interm_43_triplet_pt4(q, j, i, k) * wm_interm_77_triplet_pt4(j, k)
term(918) = term(918) + r1(vrdav_Rl, p,i) * wm_interm_43_triplet_pt4(q, j, i, k) * wm_interm_78_triplet_pt4(j, k)
term(919) = term(919) + r1(vrdav_Rr, p,i) * wm_interm_60_triplet_pt4(q, j, i, k) * wm_interm_87_triplet_pt4(k, j)
term(920) = term(920) + r1(vrdav_Rr, p,i) * wm_interm_60_triplet_pt4(q, j, i, k) * wm_interm_89_triplet_pt4(k, j)
term(921) = term(921) + t1(q,i) * wm_interm_45_triplet_pt4(p, j, i, k) * wm_interm_87_triplet_pt4(j, k)
term(922) = term(922) + t1(q,i) * wm_interm_45_triplet_pt4(p, j, i, k) * wm_interm_89_triplet_pt4(j, k)
term(923) = term(923) + r1(vrdav_Rr, p,i) * wm_interm_60_triplet_pt4(q, j, i, k) * wm_interm_95_triplet_pt4(k, j)
term(924) = term(924) + r1(vrdav_Rr, p,i) * wm_interm_60_triplet_pt4(q, j, i, k) * wm_interm_98_triplet_pt4(k, j)
term(925) = term(925) + r1(vrdav_Rr, p,i) * wm_interm_24_triplet_pt4(q, j, i, k) * wm_interm_54_triplet_pt4(k, j)
term(926) = term(926) + r1(vrdav_Rr, p,i) * wm_interm_24_triplet_pt4(q, j, i, k) * wm_interm_53_triplet_pt4(k, j)
term(927) = term(927) + r1(vrdav_Rr, p,i) * wm_interm_27_triplet_pt4(q, j, i, k) * wm_interm_54_triplet_pt4(k, j)
term(928) = term(928) + r1(vrdav_Rr, p,i) * wm_interm_27_triplet_pt4(q, j, i, k) * wm_interm_53_triplet_pt4(k, j)
term(929) = term(929) + t1(q,i) * wm_interm_45_triplet_pt4(p, j, i, k) * wm_interm_95_triplet_pt4(j, k)
term(930) = term(930) + t1(q,i) * wm_interm_45_triplet_pt4(p, j, i, k) * wm_interm_98_triplet_pt4(j, k)
end do 
end do 
end do 

term(907) = term(907) * (-6.0d+0) 
term(908) = term(908) * 6.0d+0 
term(909) = term(909) * 3.0d+0 
term(910) = term(910) * (-3.0d+0) 
term(911) = term(911) * (-4.0d+0) 
term(912) = term(912) * 8.0d+0 
term(913) = term(913) * (-4.0d+0) 
term(914) = term(914) * 8.0d+0 
term(915) = term(915) * (-4.0d+0) 
term(916) = term(916) * 8.0d+0 
term(917) = term(917) * (-4.0d+0) 
term(918) = term(918) * 8.0d+0 
term(919) = term(919) * (-2.0d+0) 
term(920) = term(920) * (-2.0d+0) 
term(921) = term(921) * (-3.0d+0) 
term(922) = term(922) * 3.0d+0 
term(923) = term(923) * 8.0d+0 
term(924) = term(924) * (-16.0d+0) 
term(925) = term(925) * (-8.0d+0) 
term(926) = term(926) * 4.0d+0 
term(927) = term(927) * 16.0d+0 
term(928) = term(928) * (-8.0d+0) 
term(929) = term(929) * 4.0d+0 
term(930) = term(930) * (-8.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
term(931) = term(931) + s1(p,i) * wm_interm_51_triplet_pt4(q, j, k, i) * wm_interm_67_triplet_pt4(j, k)
term(932) = term(932) + s1(p,i) * wm_interm_51_triplet_pt4(q, j, k, i) * wm_interm_68_triplet_pt4(j, k)
term(933) = term(933) + r1(vrdav_Rl, p,i) * wm_interm_43_triplet_pt4(q, j, k, i) * wm_interm_67_triplet_pt4(j, k)
term(934) = term(934) + r1(vrdav_Rl, p,i) * wm_interm_43_triplet_pt4(q, j, k, i) * wm_interm_68_triplet_pt4(j, k)
term(935) = term(935) + s1(p,i) * wm_interm_51_triplet_pt4(q, j, k, i) * wm_interm_75_triplet_pt4(j, k)
term(936) = term(936) + s1(p,i) * wm_interm_51_triplet_pt4(q, j, k, i) * wm_interm_76_triplet_pt4(j, k)
term(937) = term(937) + s1(p,i) * wm_interm_51_triplet_pt4(q, j, k, i) * wm_interm_77_triplet_pt4(j, k)
term(938) = term(938) + s1(p,i) * wm_interm_51_triplet_pt4(q, j, k, i) * wm_interm_78_triplet_pt4(j, k)
term(939) = term(939) + r1(vrdav_Rl, p,i) * wm_interm_43_triplet_pt4(q, j, k, i) * wm_interm_75_triplet_pt4(j, k)
term(940) = term(940) + r1(vrdav_Rl, p,i) * wm_interm_43_triplet_pt4(q, j, k, i) * wm_interm_76_triplet_pt4(j, k)
term(941) = term(941) + r1(vrdav_Rl, p,i) * wm_interm_43_triplet_pt4(q, j, k, i) * wm_interm_77_triplet_pt4(j, k)
term(942) = term(942) + r1(vrdav_Rl, p,i) * wm_interm_43_triplet_pt4(q, j, k, i) * wm_interm_78_triplet_pt4(j, k)
term(943) = term(943) + t1(q,i) * wm_interm_45_triplet_pt4(p, j, k, i) * wm_interm_87_triplet_pt4(j, k)
term(944) = term(944) + t1(q,i) * wm_interm_45_triplet_pt4(p, j, k, i) * wm_interm_89_triplet_pt4(j, k)
term(945) = term(945) + t1(q,i) * wm_interm_45_triplet_pt4(p, j, k, i) * wm_interm_95_triplet_pt4(j, k)
term(946) = term(946) + t1(q,i) * wm_interm_45_triplet_pt4(p, j, k, i) * wm_interm_98_triplet_pt4(j, k)
end do 
end do 
end do 

term(931) = term(931) * 12.0d+0 
term(932) = term(932) * (-12.0d+0) 
term(933) = term(933) * (-6.0d+0) 
term(934) = term(934) * 6.0d+0 
term(935) = term(935) * 8.0d+0 
term(936) = term(936) * (-16.0d+0) 
term(937) = term(937) * 8.0d+0 
term(938) = term(938) * (-16.0d+0) 
term(939) = term(939) * 8.0d+0 
term(940) = term(940) * (-16.0d+0) 
term(941) = term(941) * 8.0d+0 
term(942) = term(942) * (-16.0d+0) 
term(943) = term(943) * 6.0d+0 
term(944) = term(944) * (-6.0d+0) 
term(945) = term(945) * (-8.0d+0) 
term(946) = term(946) * 16.0d+0 

do l = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(947) = term(947) + r1(vrdav_Rl, p,i) * s2(a,b,k,l) * t2(b,q,l,j) * wm_interm_14_triplet_pt4(a, i, k, j)
term(948) = term(948) + r1(vrdav_Rl, p,i) * s2(a,b,k,l) * t2(b,q,l,j) * wm_interm_79_triplet_pt4(a, i, k, j)
term(949) = term(949) + r1(vrdav_Rl, p,i) * s2(a,b,k,l) * t2(b,q,k,j) * wm_interm_14_triplet_pt4(a, i, l, j)
term(950) = term(950) + r1(vrdav_Rl, p,i) * s2(a,b,k,l) * t2(b,q,k,j) * wm_interm_79_triplet_pt4(a, i, l, j)
term(951) = term(951) + r1(vrdav_Rl, p,i) * s2(a,b,k,j) * t2(b,q,l,j) * wm_interm_79_triplet_pt4(a, i, k, l)
term(952) = term(952) + r1(vrdav_Rl, p,i) * s2(a,b,k,j) * t2(b,q,l,j) * wm_interm_14_triplet_pt4(a, i, k, l)
term(953) = term(953) + s2(a,p,k,j) * t1(q,i) * t2(a,b,k,l) * wm_interm_85_triplet_pt4(b, i, l, j)
term(954) = term(954) + r1(vrdav_Rr, p,i) * s2(a,q,k,j) * t2(a,b,k,l) * wm_interm_24_triplet_pt4(b, i, l, j)
term(955) = term(955) + r2m(vrdav_Rl, a,k,q,j) * r1(vrdav_Rr, p,i) * t2(a,b,k,l) * wm_interm_62_triplet_pt4(b, i, l, j)
term(956) = term(956) + r1(vrdav_Rr, p,i) * s2(a,q,k,j) * t2(a,b,k,l) * wm_interm_27_triplet_pt4(b, i, l, j)
term(957) = term(957) + s2(a,p,k,j) * t1(q,i) * t2(a,b,k,l) * wm_interm_92_triplet_pt4(b, i, l, j)
term(958) = term(958) + s2(a,p,k,j) * t1(q,i) * t2(a,b,k,l) * wm_interm_94_triplet_pt4(b, i, l, j)
end do 
end do 
end do 
end do 
end do 
end do 

term(947) = term(947) * 8.0d+0 
term(948) = term(948) * (-16.0d+0) 
term(949) = term(949) * (-4.0d+0) 
term(950) = term(950) * 8.0d+0 
term(951) = term(951) * 8.0d+0 
term(952) = term(952) * (-4.0d+0) 
term(953) = term(953) * 6.0d+0 
term(954) = term(954) * (-8.0d+0) 
term(955) = term(955) * 16.0d+0 
term(956) = term(956) * 16.0d+0 
term(957) = term(957) * 8.0d+0 
term(958) = term(958) * (-4.0d+0) 

do l = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(959) = term(959) + r1(vrdav_Rl, a,j) * s2(b,p,k,i) * t2(b,q,l,i) * wm_interm_14_triplet_pt4(a, k, j, l)
end do 
end do 
end do 
end do 
end do 
end do 

term(959) = term(959) * 16.0d+0 

do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do l = 1, nocc 
do i = 1, nocc 
term(960) = term(960) + r2p(vrdav_Rl, q,j,a,k) * r1(vrdav_Rr, p,i) * t2(a,b,l,k) * wm_interm_60_triplet_pt4(b, i, l, j)
term(961) = term(961) + r2p(vrdav_Rl, q,j,a,k) * r1(vrdav_Rr, p,i) * t2(a,b,l,k) * wm_interm_62_triplet_pt4(b, i, l, j)
end do 
end do 
end do 
end do 
end do 
end do 

term(960) = term(960) * 6.0d+0 
term(961) = term(961) * (-6.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do i = 1, nocc 
do l = 1, nocc 
term(962) = term(962) + r2p(vrdav_Rl, q,j,a,k) * r1(vrdav_Rr, p,i) * t2(a,b,l,k) * wm_interm_60_triplet_pt4(b, l, i, j)
term(963) = term(963) + r2p(vrdav_Rl, q,j,a,k) * r1(vrdav_Rr, p,i) * t2(a,b,l,k) * wm_interm_62_triplet_pt4(b, l, i, j)
end do 
end do 
end do 
end do 
end do 
end do 

term(962) = term(962) * (-6.0d+0) 
term(963) = term(963) * 6.0d+0 

do l = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(964) = term(964) + r2p(vrdav_Rl, q,j,a,k) * r1(vrdav_Rr, p,i) * t2(a,b,k,l) * wm_interm_62_triplet_pt4(b, l, i, j)
term(965) = term(965) + r2p(vrdav_Rl, q,j,a,k) * r1(vrdav_Rr, p,i) * t2(a,b,k,l) * wm_interm_62_triplet_pt4(b, i, l, j)
end do 
end do 
end do 
end do 
end do 
end do 

term(964) = term(964) * (-12.0d+0) 
term(965) = term(965) * 24.0d+0 

do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(966) = term(966) + r2m(vrdav_Rl, a,i,q,j) * r2m(vrdav_Rr, a,k,p,j) * wm_interm_5_triplet_pt4(k, i)
end do 
end do 
end do 
end do 

term(966) = term(966) * (-8.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do l = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(967) = term(967) + s2(a,p,k,j) * t1(q,i) * t2(a,b,l,j) * wm_interm_85_triplet_pt4(b, i, l, k)
term(968) = term(968) + s2(a,p,k,j) * t1(q,i) * t2(a,b,l,k) * wm_interm_85_triplet_pt4(b, i, l, j)
term(969) = term(969) + r2m(vrdav_Rl, a,j,q,k) * r1(vrdav_Rr, p,i) * t2(a,b,l,j) * wm_interm_60_triplet_pt4(b, i, l, k)
term(970) = term(970) + r2m(vrdav_Rl, a,k,q,j) * r1(vrdav_Rr, p,i) * t2(a,b,l,j) * wm_interm_60_triplet_pt4(b, i, l, k)
term(971) = term(971) + r1(vrdav_Rr, p,i) * s2(a,q,k,j) * t2(a,b,l,k) * wm_interm_24_triplet_pt4(b, i, l, j)
term(972) = term(972) + r1(vrdav_Rr, p,i) * s2(a,q,k,j) * t2(a,b,l,j) * wm_interm_24_triplet_pt4(b, i, l, k)
term(973) = term(973) + r2m(vrdav_Rl, a,k,q,j) * r1(vrdav_Rr, p,i) * t2(a,b,l,k) * wm_interm_62_triplet_pt4(b, i, l, j)
term(974) = term(974) + r2m(vrdav_Rl, a,j,q,k) * r1(vrdav_Rr, p,i) * t2(a,b,l,k) * wm_interm_62_triplet_pt4(b, i, l, j)
term(975) = term(975) + r1(vrdav_Rr, p,i) * s2(a,q,k,j) * t2(a,b,l,j) * wm_interm_27_triplet_pt4(b, i, l, k)
term(976) = term(976) + r1(vrdav_Rr, p,i) * s2(a,q,k,j) * t2(a,b,l,k) * wm_interm_27_triplet_pt4(b, i, l, j)
term(977) = term(977) + s2(a,p,k,j) * t1(q,i) * t2(a,b,l,j) * wm_interm_92_triplet_pt4(b, i, l, k)
term(978) = term(978) + s2(a,p,k,j) * t1(q,i) * t2(a,b,l,k) * wm_interm_92_triplet_pt4(b, i, l, j)
term(979) = term(979) + s2(a,p,k,j) * t1(q,i) * t2(a,b,l,j) * wm_interm_94_triplet_pt4(b, i, l, k)
term(980) = term(980) + s2(a,p,k,j) * t1(q,i) * t2(a,b,l,k) * wm_interm_94_triplet_pt4(b, i, l, j)
end do 
end do 
end do 
end do 
end do 
end do 

term(967) = term(967) * 3.0d+0 
term(968) = term(968) * (-3.0d+0) 
term(969) = term(969) * 4.0d+0 
term(970) = term(970) * (-8.0d+0) 
term(971) = term(971) * 4.0d+0 
term(972) = term(972) * (-8.0d+0) 
term(973) = term(973) * (-8.0d+0) 
term(974) = term(974) * 4.0d+0 
term(975) = term(975) * 4.0d+0 
term(976) = term(976) * (-8.0d+0) 
term(977) = term(977) * 2.0d+0 
term(978) = term(978) * (-4.0d+0) 
term(979) = term(979) * (-4.0d+0) 
term(980) = term(980) * 2.0d+0 

do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
term(981) = term(981) + r1(vrdav_Rr, p,i) * wm_interm_54_triplet_pt4(j, k) * wm_interm_7_triplet_pt4(q, i, k, j)
term(982) = term(982) + r1(vrdav_Rr, p,i) * wm_interm_53_triplet_pt4(j, k) * wm_interm_7_triplet_pt4(q, i, k, j)
term(983) = term(983) + t1(q,i) * wm_interm_53_triplet_pt4(j, k) * wm_interm_85_triplet_pt4(p, i, k, j)
term(984) = term(984) + t1(q,i) * wm_interm_54_triplet_pt4(j, k) * wm_interm_85_triplet_pt4(p, i, k, j)
term(985) = term(985) + t1(q,i) * wm_interm_53_triplet_pt4(j, k) * wm_interm_94_triplet_pt4(p, i, k, j)
term(986) = term(986) + t1(q,i) * wm_interm_54_triplet_pt4(j, k) * wm_interm_94_triplet_pt4(p, i, k, j)
term(987) = term(987) + t1(q,i) * wm_interm_53_triplet_pt4(j, k) * wm_interm_92_triplet_pt4(p, i, k, j)
term(988) = term(988) + t1(q,i) * wm_interm_54_triplet_pt4(j, k) * wm_interm_92_triplet_pt4(p, i, k, j)
end do 
end do 
end do 

term(981) = term(981) * (-24.0d+0) 
term(982) = term(982) * 12.0d+0 
term(983) = term(983) * 3.0d+0 
term(984) = term(984) * (-6.0d+0) 
term(985) = term(985) * (-4.0d+0) 
term(986) = term(986) * 8.0d+0 
term(987) = term(987) * 2.0d+0 
term(988) = term(988) * (-4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
do l = 1, nocc 
do b = nocc + 1, nactive 
term(989) = term(989) + r2p(vrdav_Rl, p,j,a,k) * r1(vrdav_Rr, b,l) * t2(a,q,k,i) * wm_interm_60_triplet_pt4(b, i, l, j)
term(990) = term(990) + r2p(vrdav_Rl, p,j,a,k) * r1(vrdav_Rr, b,k) * t2(a,q,l,i) * wm_interm_60_triplet_pt4(b, i, l, j)
term(991) = term(991) + r2p(vrdav_Rl, p,j,a,k) * r1(vrdav_Rr, b,k) * t2(a,q,l,i) * wm_interm_60_triplet_pt4(b, l, i, j)
term(992) = term(992) + r2p(vrdav_Rl, p,j,a,k) * r1(vrdav_Rr, b,l) * t2(a,q,k,i) * wm_interm_60_triplet_pt4(b, l, i, j)
end do 
end do 
end do 
end do 
end do 
end do 

term(989) = term(989) * (-6.0d+0) 
term(990) = term(990) * 6.0d+0 
term(991) = term(991) * (-6.0d+0) 
term(992) = term(992) * 12.0d+0 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
do k = 1, nocc 
do l = 1, nocc 
do b = nocc + 1, nactive 
term(993) = term(993) + r2p(vrdav_Rl, p,j,a,i) * r1(vrdav_Rr, b,k) * t2(a,q,l,i) * wm_interm_60_triplet_pt4(b, l, k, j)
end do 
end do 
end do 
end do 
end do 
end do 

term(993) = term(993) * 6.0d+0 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
do l = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
term(994) = term(994) + r2p(vrdav_Rl, p,j,a,i) * r1(vrdav_Rr, b,k) * t2(a,q,l,i) * wm_interm_60_triplet_pt4(b, k, l, j)
end do 
end do 
end do 
end do 
end do 
end do 

term(994) = term(994) * (-12.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(995) = term(995) + r2m(vrdav_Rr, a,i,p,j) * s1(q,i) * s2(a,b,j,k) * wm_interm_47_triplet_pt4(b, k)
term(996) = term(996) + r2m(vrdav_Rr, a,i,p,j) * s1(q,i) * s2(a,b,j,k) * wm_interm_52_triplet_pt4(b, k)
term(997) = term(997) + r1(vrdav_Rl, p,i) * s2(a,b,j,k) * t2(b,q,i,j) * wm_interm_2_triplet_pt4(a, k)
term(998) = term(998) + r1(vrdav_Rl, p,i) * s2(a,b,j,k) * t2(b,q,i,j) * wm_interm_3_triplet_pt4(a, k)
term(999) = term(999) + s1(p,i) * s2(a,b,j,k) * t2(b,q,i,j) * wm_interm_82_triplet_pt4(a, k)
term(1000) = term(1000) + s1(p,i) * s2(a,b,j,k) * t2(b,q,i,j) * wm_interm_84_triplet_pt4(a, k)
term(1001) = term(1001) + s1(p,i) * s2(a,b,j,k) * t2(b,q,i,j) * wm_interm_83_triplet_pt4(a, k)
term(1002) = term(1002) + r1(vrdav_Rr, p,i) * s2(a,q,i,j) * t2(a,b,j,k) * wm_interm_0_triplet_pt4(b, k)
term(1003) = term(1003) + s2(a,p,i,j) * t1(q,i) * t2(a,b,j,k) * wm_interm_86_triplet_pt4(b, k)
term(1004) = term(1004) + r1(vrdav_Rr, p,i) * s2(a,q,i,j) * t2(a,b,j,k) * wm_interm_20_triplet_pt4(b, k)
term(1005) = term(1005) + r1(vrdav_Rr, p,i) * s2(a,q,i,j) * t2(a,b,j,k) * wm_interm_21_triplet_pt4(b, k)
term(1006) = term(1006) + s2(a,p,i,j) * t1(q,i) * t2(a,b,j,k) * wm_interm_93_triplet_pt4(b, k)
term(1007) = term(1007) + r2m(vrdav_Rl, a,i,p,j) * t1(q,i) * t2(a,b,j,k) * wm_interm_46_triplet_pt4(b, k)
term(1008) = term(1008) + s2(a,p,i,j) * t1(q,i) * t2(a,b,j,k) * wm_interm_96_triplet_pt4(b, k)
term(1009) = term(1009) + r2m(vrdav_Rl, a,i,p,j) * t1(q,i) * t2(a,b,j,k) * wm_interm_44_triplet_pt4(b, k)
end do 
end do 
end do 
end do 
end do 

term(995) = term(995) * (-8.0d+0) 
term(996) = term(996) * 16.0d+0 
term(997) = term(997) * 8.0d+0 
term(998) = term(998) * (-4.0d+0) 
term(999) = term(999) * 12.0d+0 
term(1000) = term(1000) * (-8.0d+0) 
term(1001) = term(1001) * 16.0d+0 
term(1002) = term(1002) * 8.0d+0 
term(1003) = term(1003) * 12.0d+0 
term(1004) = term(1004) * 32.0d+0 
term(1005) = term(1005) * (-16.0d+0) 
term(1006) = term(1006) * 16.0d+0 
term(1007) = term(1007) * 16.0d+0 
term(1008) = term(1008) * (-8.0d+0) 
term(1009) = term(1009) * (-8.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(1010) = term(1010) + r2m(vrdav_Rl, a,j,q,k) * r1(vrdav_Rr, p,i) * t2(a,b,j,l) * wm_interm_62_triplet_pt4(b, i, l, k)
end do 
end do 
end do 
end do 
end do 
end do 

term(1010) = term(1010) * 16.0d+0 


    calc_D_vv_wm_triplet_pt4 = zero
    do s = 0, 1010
    calc_D_vv_wm_triplet_pt4 = calc_D_vv_wm_triplet_pt4 + term(s)
    end do

    end function calc_D_vv_wm_triplet_pt4
    

    

end module density_exc_exc_functions_triplet_pt4
