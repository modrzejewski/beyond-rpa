module density_exc_exc_functions_triplet_pt3
          use ccsd_transformed_integrals
    use cc3_intermediates
    use arithmetic
    use s_gen
    use basis
    use eom_vectors
    
    implicit none
    !
    ! File generated automatically on 2016-09-15 15:12:46
    !
    real(F64), dimension(:, :, :, :), allocatable :: wm_interm_0_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_1_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_2_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_3_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_4_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_5_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_6_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_7_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_8_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_9_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_10_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_11_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_12_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_13_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_14_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_15_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_16_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_17_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_18_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_19_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_20_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_21_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_22_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_23_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_24_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_25_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_26_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_27_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_28_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_29_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_30_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_31_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_32_triplet_pt3 
real(F64) :: wm_interm_33_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_34_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_35_triplet_pt3 
real(F64) :: wm_interm_36_triplet_pt3 
real(F64) :: wm_interm_37_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_38_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_39_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_40_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_41_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_42_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_43_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_44_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_45_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_46_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_47_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_48_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_49_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_50_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_51_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_52_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_53_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_54_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_55_triplet_pt3 
real(F64) :: wm_interm_56_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_57_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_58_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_59_triplet_pt3 
real(F64) :: wm_interm_60_triplet_pt3 
real(F64) :: wm_interm_61_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_62_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_63_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_64_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_65_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_66_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_67_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_68_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_69_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_70_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_71_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_72_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_73_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_74_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_75_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_76_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_77_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_78_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_79_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_80_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_81_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_82_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_83_triplet_pt3 
real(F64) :: wm_interm_84_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_85_triplet_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_86_triplet_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_87_triplet_pt3 

    contains
    
    subroutine wm_triplet_intermediates_ccsd_init_pt3(nocc, nactive)
    integer, intent(in) :: nocc
    integer, intent(in) :: nactive
    allocate(wm_interm_0_triplet_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_1_triplet_pt3(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_2_triplet_pt3(1: nocc, 1: nocc))
allocate(wm_interm_3_triplet_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_4_triplet_pt3(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_5_triplet_pt3(1: nocc, 1: nocc))
allocate(wm_interm_6_triplet_pt3(1: nocc, 1: nocc))
allocate(wm_interm_7_triplet_pt3(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_8_triplet_pt3(1: nocc, 1: nocc))
allocate(wm_interm_9_triplet_pt3(1: nocc, 1: nocc))
allocate(wm_interm_10_triplet_pt3(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_11_triplet_pt3(1: nocc, 1: nocc))
allocate(wm_interm_12_triplet_pt3(1: nocc, 1: nocc))
allocate(wm_interm_13_triplet_pt3(1: nocc, 1: nocc))
allocate(wm_interm_14_triplet_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_15_triplet_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_16_triplet_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_17_triplet_pt3(1: nocc, 1: nocc))
allocate(wm_interm_18_triplet_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_19_triplet_pt3(1: nocc, 1: nocc))
allocate(wm_interm_20_triplet_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_21_triplet_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_22_triplet_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_23_triplet_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_24_triplet_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_25_triplet_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_26_triplet_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_27_triplet_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_28_triplet_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_29_triplet_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_30_triplet_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_31_triplet_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_32_triplet_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_34_triplet_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_35_triplet_pt3(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_38_triplet_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_39_triplet_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_40_triplet_pt3(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_41_triplet_pt3(1: nocc, 1: nocc))
allocate(wm_interm_42_triplet_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_43_triplet_pt3(1: nocc, 1: nocc))
allocate(wm_interm_44_triplet_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_45_triplet_pt3(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_46_triplet_pt3(1: nocc, 1: nocc))
allocate(wm_interm_47_triplet_pt3(1: nocc, 1: nocc))
allocate(wm_interm_48_triplet_pt3(1: nocc, 1: nocc))
allocate(wm_interm_49_triplet_pt3(1: nocc, 1: nocc))
allocate(wm_interm_50_triplet_pt3(1: nocc, 1: nocc))
allocate(wm_interm_51_triplet_pt3(1: nocc, 1: nocc))
allocate(wm_interm_52_triplet_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_53_triplet_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_54_triplet_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_55_triplet_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_57_triplet_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_58_triplet_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_59_triplet_pt3(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_62_triplet_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_63_triplet_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_64_triplet_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_65_triplet_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_66_triplet_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_67_triplet_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_68_triplet_pt3(1: nocc, 1: nocc))
allocate(wm_interm_69_triplet_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_70_triplet_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_71_triplet_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_72_triplet_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_73_triplet_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_74_triplet_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_75_triplet_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_76_triplet_pt3(1: nocc, 1: nocc))
allocate(wm_interm_77_triplet_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_78_triplet_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_79_triplet_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_80_triplet_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_81_triplet_pt3(1: nocc, 1: nocc))
allocate(wm_interm_82_triplet_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_83_triplet_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_85_triplet_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_86_triplet_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_87_triplet_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
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
deallocate(wm_interm_34_triplet_pt3)
deallocate(wm_interm_35_triplet_pt3)
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
deallocate(wm_interm_57_triplet_pt3)
deallocate(wm_interm_58_triplet_pt3)
deallocate(wm_interm_59_triplet_pt3)
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
deallocate(wm_interm_85_triplet_pt3)
deallocate(wm_interm_86_triplet_pt3)
deallocate(wm_interm_87_triplet_pt3)

    end subroutine wm_triplet_intermediates_ccsd_free_pt3
    
    subroutine wm_triplet_intermediates_ccsd_pt3(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr)
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    double precision, dimension(:), intent(in) :: vrdav_Rl
    double precision, dimension(:), intent(in) :: vrdav_Rr
    real(F64) :: sum
    integer :: a, b, j, i, k, l, c 

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
wm_interm_0_triplet_pt3(b, j, i, k) = wm_interm_0_triplet_pt3(b, j, i, k) + sum 
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
wm_interm_1_triplet_pt3(i, j, k, l) = wm_interm_1_triplet_pt3(i, j, k, l) + sum 
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
sum = sum + r2p(vrdav_Rl, a,j,b,i) * r2p(vrdav_Rr, a,k,b,i)
end do 
end do 
end do 
wm_interm_2_triplet_pt3(j, k) = wm_interm_2_triplet_pt3(j, k) + sum 
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
wm_interm_3_triplet_pt3(b, i, j, k) = wm_interm_3_triplet_pt3(b, i, j, k) + sum 
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
wm_interm_4_triplet_pt3(i, j, k, l) = wm_interm_4_triplet_pt3(i, j, k, l) + sum 
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
wm_interm_5_triplet_pt3(j, k) = wm_interm_5_triplet_pt3(j, k) + sum 
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
wm_interm_6_triplet_pt3(j, k) = wm_interm_6_triplet_pt3(j, k) + sum 
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
wm_interm_7_triplet_pt3(i, j, k, l) = wm_interm_7_triplet_pt3(i, j, k, l) + sum 
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
wm_interm_8_triplet_pt3(j, k) = wm_interm_8_triplet_pt3(j, k) + sum 
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
wm_interm_9_triplet_pt3(j, k) = wm_interm_9_triplet_pt3(j, k) + sum 
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
wm_interm_10_triplet_pt3(i, j, k, l) = wm_interm_10_triplet_pt3(i, j, k, l) + sum 
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
wm_interm_11_triplet_pt3(j, k) = wm_interm_11_triplet_pt3(j, k) + sum 
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
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rr, a,j,b,i) * s2(a,b,k,i)
end do 
end do 
end do 
wm_interm_13_triplet_pt3(j, k) = wm_interm_13_triplet_pt3(j, k) + sum 
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
wm_interm_14_triplet_pt3(b, j) = wm_interm_14_triplet_pt3(b, j) + sum 
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
wm_interm_15_triplet_pt3(b, j) = wm_interm_15_triplet_pt3(b, j) + sum 
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
wm_interm_16_triplet_pt3(b, i, k, j) = wm_interm_16_triplet_pt3(b, i, k, j) + sum 
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
sum = sum + r2p(vrdav_Rl, a,j,b,i) * t2(a,b,k,i)
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
!$omp do collapse(4)
do b = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rr, b,i,a,j) * s1(a,k)
end do 
wm_interm_18_triplet_pt3(b, i, j, k) = wm_interm_18_triplet_pt3(b, i, j, k) + sum 
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
wm_interm_19_triplet_pt3(j, k) = wm_interm_19_triplet_pt3(j, k) + sum 
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
wm_interm_20_triplet_pt3(b, j, i, k) = wm_interm_20_triplet_pt3(b, j, i, k) + sum 
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
wm_interm_21_triplet_pt3(b, i, j, k) = wm_interm_21_triplet_pt3(b, i, j, k) + sum 
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
wm_interm_22_triplet_pt3(b, j) = wm_interm_22_triplet_pt3(b, j) + sum 
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
wm_interm_23_triplet_pt3(b, c) = wm_interm_23_triplet_pt3(b, c) + sum 
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
wm_interm_24_triplet_pt3(b, c) = wm_interm_24_triplet_pt3(b, c) + sum 
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
wm_interm_25_triplet_pt3(b, c) = wm_interm_25_triplet_pt3(b, c) + sum 
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
wm_interm_26_triplet_pt3(b, c) = wm_interm_26_triplet_pt3(b, c) + sum 
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
wm_interm_27_triplet_pt3(b, j) = wm_interm_27_triplet_pt3(b, j) + sum 
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
do i = 1, nocc 
do a = nocc + 1, nactive 
do j = 1, nocc 
sum = sum + r2p(vrdav_Rl, b,j,a,i) * t2(a,c,j,i)
end do 
end do 
end do 
wm_interm_29_triplet_pt3(b, c) = wm_interm_29_triplet_pt3(b, c) + sum 
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
wm_interm_30_triplet_pt3(b, j) = wm_interm_30_triplet_pt3(b, j) + sum 
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
wm_interm_31_triplet_pt3(b, j) = wm_interm_31_triplet_pt3(b, j) + sum 
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
wm_interm_32_triplet_pt3(b, i, j, k) = wm_interm_32_triplet_pt3(b, i, j, k) + sum 
end do 
end do 
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
wm_interm_33_triplet_pt3 = wm_interm_33_triplet_pt3 + sum 
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
wm_interm_34_triplet_pt3(b, c) = wm_interm_34_triplet_pt3(b, c) + sum 
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
wm_interm_35_triplet_pt3(i, j, k, l) = wm_interm_35_triplet_pt3(i, j, k, l) + sum 
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
wm_interm_36_triplet_pt3 = wm_interm_36_triplet_pt3 + sum 
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
wm_interm_37_triplet_pt3 = wm_interm_37_triplet_pt3 + sum 
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
wm_interm_38_triplet_pt3(b, c) = wm_interm_38_triplet_pt3(b, c) + sum 
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
wm_interm_39_triplet_pt3(b, c) = wm_interm_39_triplet_pt3(b, c) + sum 
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
wm_interm_40_triplet_pt3(i, j, k, l) = wm_interm_40_triplet_pt3(i, j, k, l) + sum 
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
wm_interm_41_triplet_pt3(j, k) = wm_interm_41_triplet_pt3(j, k) + sum 
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
wm_interm_42_triplet_pt3(b, j, i, k) = wm_interm_42_triplet_pt3(b, j, i, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,j,b,i) * r2p(vrdav_Rr, a,k,b,i)
end do 
end do 
end do 
wm_interm_43_triplet_pt3(j, k) = wm_interm_43_triplet_pt3(j, k) + sum 
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
wm_interm_44_triplet_pt3(b, i, j, k) = wm_interm_44_triplet_pt3(b, i, j, k) + sum 
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
wm_interm_45_triplet_pt3(i, j, k, l) = wm_interm_45_triplet_pt3(i, j, k, l) + sum 
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
wm_interm_46_triplet_pt3(j, k) = wm_interm_46_triplet_pt3(j, k) + sum 
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
wm_interm_47_triplet_pt3(j, k) = wm_interm_47_triplet_pt3(j, k) + sum 
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
wm_interm_48_triplet_pt3(j, k) = wm_interm_48_triplet_pt3(j, k) + sum 
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
wm_interm_49_triplet_pt3(j, k) = wm_interm_49_triplet_pt3(j, k) + sum 
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
wm_interm_50_triplet_pt3(j, k) = wm_interm_50_triplet_pt3(j, k) + sum 
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
wm_interm_51_triplet_pt3(j, k) = wm_interm_51_triplet_pt3(j, k) + sum 
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
wm_interm_52_triplet_pt3(b, j) = wm_interm_52_triplet_pt3(b, j) + sum 
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
wm_interm_53_triplet_pt3(b, j) = wm_interm_53_triplet_pt3(b, j) + sum 
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
wm_interm_54_triplet_pt3(b, c) = wm_interm_54_triplet_pt3(b, c) + sum 
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
wm_interm_55_triplet_pt3(b, c) = wm_interm_55_triplet_pt3(b, c) + sum 
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
wm_interm_56_triplet_pt3 = wm_interm_56_triplet_pt3 + sum 
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
wm_interm_57_triplet_pt3(b, c) = wm_interm_57_triplet_pt3(b, c) + sum 
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
wm_interm_58_triplet_pt3(b, c) = wm_interm_58_triplet_pt3(b, c) + sum 
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
wm_interm_59_triplet_pt3(i, j, k, l) = wm_interm_59_triplet_pt3(i, j, k, l) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,j,b,i)
end do 
end do 
end do 
end do 
wm_interm_60_triplet_pt3 = wm_interm_60_triplet_pt3 + sum 
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
wm_interm_61_triplet_pt3 = wm_interm_61_triplet_pt3 + sum 
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
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rl, a,j,b,i) * r2m(vrdav_Rr, a,j,c,i)
end do 
end do 
end do 
wm_interm_63_triplet_pt3(b, c) = wm_interm_63_triplet_pt3(b, c) + sum 
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
wm_interm_64_triplet_pt3(b, c) = wm_interm_64_triplet_pt3(b, c) + sum 
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
wm_interm_65_triplet_pt3(b, c) = wm_interm_65_triplet_pt3(b, c) + sum 
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
wm_interm_66_triplet_pt3(b, i, j, k) = wm_interm_66_triplet_pt3(b, i, j, k) + sum 
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
wm_interm_67_triplet_pt3(a, b) = wm_interm_67_triplet_pt3(a, b) + sum 
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
wm_interm_68_triplet_pt3(i, j) = wm_interm_68_triplet_pt3(i, j) + sum 
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
wm_interm_69_triplet_pt3(b, i, j, k) = wm_interm_69_triplet_pt3(b, i, j, k) + sum 
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
wm_interm_70_triplet_pt3(b, j) = wm_interm_70_triplet_pt3(b, j) + sum 
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
wm_interm_71_triplet_pt3(b, j) = wm_interm_71_triplet_pt3(b, j) + sum 
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
wm_interm_72_triplet_pt3(b, j) = wm_interm_72_triplet_pt3(b, j) + sum 
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
wm_interm_73_triplet_pt3(b, i, j, k) = wm_interm_73_triplet_pt3(b, i, j, k) + sum 
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
wm_interm_74_triplet_pt3(b, j) = wm_interm_74_triplet_pt3(b, j) + sum 
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
wm_interm_75_triplet_pt3(a, b) = wm_interm_75_triplet_pt3(a, b) + sum 
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
wm_interm_76_triplet_pt3(i, j) = wm_interm_76_triplet_pt3(i, j) + sum 
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
wm_interm_77_triplet_pt3(b, i, j, k) = wm_interm_77_triplet_pt3(b, i, j, k) + sum 
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
wm_interm_78_triplet_pt3(b, j) = wm_interm_78_triplet_pt3(b, j) + sum 
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
wm_interm_79_triplet_pt3(b, j, i, k) = wm_interm_79_triplet_pt3(b, j, i, k) + sum 
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
wm_interm_80_triplet_pt3(b, j) = wm_interm_80_triplet_pt3(b, j) + sum 
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
wm_interm_81_triplet_pt3(i, j) = wm_interm_81_triplet_pt3(i, j) + sum 
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
wm_interm_82_triplet_pt3(b, j) = wm_interm_82_triplet_pt3(b, j) + sum 
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
wm_interm_83_triplet_pt3(b, i, j, k) = wm_interm_83_triplet_pt3(b, i, j, k) + sum 
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
wm_interm_84_triplet_pt3 = wm_interm_84_triplet_pt3 + sum 
!$omp parallel private(i, a, b, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
sum = zero 
do i = 1, nocc 
sum = sum + r1(vrdav_Rl, a,i) * r1(vrdav_Rr, b,i)
end do 
wm_interm_85_triplet_pt3(a, b) = wm_interm_85_triplet_pt3(a, b) + sum 
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
wm_interm_86_triplet_pt3(b, j) = wm_interm_86_triplet_pt3(b, j) + sum 
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
wm_interm_87_triplet_pt3(b, i, j, k) = wm_interm_87_triplet_pt3(b, i, j, k) + sum 
end do 
end do 
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
    double precision, dimension(:), intent(in) :: vrdav_Rl
    double precision, dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , j, i, a, b 
    real(F64), dimension(0:99) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + wm_interm_0_triplet_pt3(a, q, i, j) * wm_interm_66_triplet_pt3(a, j, p, i)
term(1) = term(1) + wm_interm_0_triplet_pt3(a, i, q, j) * wm_interm_66_triplet_pt3(a, j, p, i)
term(2) = term(2) + wm_interm_0_triplet_pt3(a, q, i, j) * wm_interm_69_triplet_pt3(a, j, p, i)
term(3) = term(3) + wm_interm_0_triplet_pt3(a, q, i, j) * wm_interm_69_triplet_pt3(a, j, i, p)
term(4) = term(4) + wm_interm_0_triplet_pt3(a, i, q, j) * wm_interm_69_triplet_pt3(a, j, i, p)
term(5) = term(5) + wm_interm_0_triplet_pt3(a, i, q, j) * wm_interm_69_triplet_pt3(a, j, p, i)
term(6) = term(6) + wm_interm_0_triplet_pt3(a, i, j, q) * wm_interm_66_triplet_pt3(a, p, i, j)
term(7) = term(7) + wm_interm_0_triplet_pt3(a, i, j, q) * wm_interm_69_triplet_pt3(a, p, i, j)
term(8) = term(8) + wm_interm_16_triplet_pt3(a, q, i, j) * wm_interm_73_triplet_pt3(a, j, i, p)
term(9) = term(9) + wm_interm_16_triplet_pt3(a, q, i, j) * wm_interm_73_triplet_pt3(a, i, j, p)
term(10) = term(10) + wm_interm_16_triplet_pt3(a, i, q, j) * wm_interm_73_triplet_pt3(a, j, p, i)
term(11) = term(11) + wm_interm_16_triplet_pt3(a, q, i, j) * wm_interm_79_triplet_pt3(a, j, i, p)
term(12) = term(12) + wm_interm_16_triplet_pt3(a, q, i, j) * wm_interm_77_triplet_pt3(a, j, i, p)
term(13) = term(13) + wm_interm_16_triplet_pt3(a, q, i, j) * wm_interm_77_triplet_pt3(a, i, j, p)
term(14) = term(14) + wm_interm_16_triplet_pt3(a, i, q, j) * wm_interm_79_triplet_pt3(a, j, p, i)
term(15) = term(15) + wm_interm_16_triplet_pt3(a, i, q, j) * wm_interm_77_triplet_pt3(a, p, j, i)
term(16) = term(16) + wm_interm_16_triplet_pt3(a, i, q, j) * wm_interm_77_triplet_pt3(a, j, p, i)
end do 
end do 
end do 

term(0) = term(0) * (-4.0d+0) 
term(1) = term(1) * 4.0d+0 
term(2) = term(2) * (-4.0d+0) 
term(3) = term(3) * 4.0d+0 
term(4) = term(4) * (-4.0d+0) 
term(5) = term(5) * 4.0d+0 
term(6) = term(6) * (-2.0d+0) 
term(7) = term(7) * (-4.0d+0) 
term(8) = term(8) * 3.0d+0 
term(9) = -term(9) 
term(10) = term(10) * 4.0d+0 
term(11) = term(11) * (-2.0d+0) 
term(12) = term(12) * 4.0d+0 
term(13) = term(13) * (-2.0d+0) 
term(14) = term(14) * (-2.0d+0) 
term(15) = term(15) * (-2.0d+0) 
term(16) = term(16) * 4.0d+0 

do a = nocc + 1, nactive 
term(17) = term(17) + wm_interm_14_triplet_pt3(a, q) * wm_interm_74_triplet_pt3(a, p)
term(18) = term(18) + wm_interm_15_triplet_pt3(a, q) * wm_interm_74_triplet_pt3(a, p)
term(19) = term(19) + wm_interm_14_triplet_pt3(a, q) * wm_interm_80_triplet_pt3(a, p)
term(20) = term(20) + wm_interm_14_triplet_pt3(a, q) * wm_interm_78_triplet_pt3(a, p)
term(21) = term(21) + wm_interm_15_triplet_pt3(a, q) * wm_interm_80_triplet_pt3(a, p)
term(22) = term(22) + wm_interm_15_triplet_pt3(a, q) * wm_interm_78_triplet_pt3(a, p)
end do 

term(17) = term(17) * 4.0d+0 
term(18) = term(18) * (-8.0d+0) 
term(19) = term(19) * (-4.0d+0) 
term(20) = term(20) * 4.0d+0 
term(21) = term(21) * 8.0d+0 
term(22) = term(22) * (-8.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(23) = term(23) + wm_interm_0_triplet_pt3(a, i, j, q) * wm_interm_66_triplet_pt3(a, p, j, i)
term(24) = term(24) + wm_interm_0_triplet_pt3(a, i, j, q) * wm_interm_69_triplet_pt3(a, p, j, i)
term(25) = term(25) + wm_interm_16_triplet_pt3(a, i, j, q) * wm_interm_73_triplet_pt3(a, j, p, i)
term(26) = term(26) + wm_interm_16_triplet_pt3(a, i, j, q) * wm_interm_79_triplet_pt3(a, p, j, i)
term(27) = term(27) + wm_interm_16_triplet_pt3(a, i, j, q) * wm_interm_77_triplet_pt3(a, p, j, i)
term(28) = term(28) + wm_interm_16_triplet_pt3(a, i, j, q) * wm_interm_77_triplet_pt3(a, j, p, i)
end do 
end do 
end do 

term(23) = term(23) * 2.0d+0 
term(24) = term(24) * 4.0d+0 
term(25) = term(25) * (-4.0d+0) 
term(26) = term(26) * (-2.0d+0) 
term(27) = term(27) * 4.0d+0 
term(28) = term(28) * (-2.0d+0) 

do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(29) = term(29) + wm_interm_23_triplet_pt3(a, b) * wm_interm_67_triplet_pt3(a, b)
term(30) = term(30) + wm_interm_24_triplet_pt3(a, b) * wm_interm_67_triplet_pt3(a, b)
term(31) = term(31) + wm_interm_25_triplet_pt3(a, b) * wm_interm_67_triplet_pt3(a, b)
term(32) = term(32) + wm_interm_26_triplet_pt3(a, b) * wm_interm_67_triplet_pt3(a, b)
term(33) = term(33) + wm_interm_28_triplet_pt3(a, b) * wm_interm_75_triplet_pt3(a, b)
term(34) = term(34) + wm_interm_29_triplet_pt3(a, b) * wm_interm_75_triplet_pt3(a, b)
term(35) = term(35) + wm_interm_54_triplet_pt3(a, b) * wm_interm_75_triplet_pt3(a, b)
term(36) = term(36) + wm_interm_55_triplet_pt3(a, b) * wm_interm_75_triplet_pt3(a, b)
end do 
end do 

term(29) = term(29) * (-4.0d+0) 
term(30) = term(30) * 4.0d+0 
term(31) = term(31) * 8.0d+0 
term(32) = term(32) * (-8.0d+0) 
term(33) = term(33) * (-6.0d+0) 
term(34) = term(34) * 2.0d+0 
term(35) = term(35) * 8.0d+0 
term(36) = term(36) * (-8.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
term(37) = term(37) + wm_interm_4_triplet_pt3(i, p, q, j) * wm_interm_68_triplet_pt3(i, j)
term(38) = term(38) + wm_interm_4_triplet_pt3(i, p, j, q) * wm_interm_68_triplet_pt3(i, j)
term(39) = term(39) + wm_interm_5_triplet_pt3(i, j) * wm_interm_68_triplet_pt3(i, j)
term(40) = term(40) + wm_interm_68_triplet_pt3(i, j) * wm_interm_6_triplet_pt3(i, j)
term(41) = term(41) + wm_interm_10_triplet_pt3(i, p, q, j) * wm_interm_68_triplet_pt3(i, j)
term(42) = term(42) + wm_interm_10_triplet_pt3(p, i, j, q) * wm_interm_68_triplet_pt3(i, j)
term(43) = term(43) + wm_interm_10_triplet_pt3(i, p, j, q) * wm_interm_68_triplet_pt3(i, j)
term(44) = term(44) + wm_interm_11_triplet_pt3(i, j) * wm_interm_68_triplet_pt3(i, j)
term(45) = term(45) + wm_interm_12_triplet_pt3(i, j) * wm_interm_68_triplet_pt3(i, j)
term(46) = term(46) + wm_interm_13_triplet_pt3(i, j) * wm_interm_68_triplet_pt3(i, j)
term(47) = term(47) + wm_interm_17_triplet_pt3(i, j) * wm_interm_76_triplet_pt3(i, j)
term(48) = term(48) + wm_interm_19_triplet_pt3(i, j) * wm_interm_76_triplet_pt3(i, j)
term(49) = term(49) + wm_interm_35_triplet_pt3(i, p, q, j) * wm_interm_76_triplet_pt3(i, j)
term(50) = term(50) + wm_interm_35_triplet_pt3(i, p, j, q) * wm_interm_76_triplet_pt3(i, j)
term(51) = term(51) + wm_interm_50_triplet_pt3(i, j) * wm_interm_76_triplet_pt3(i, j)
term(52) = term(52) + wm_interm_51_triplet_pt3(i, j) * wm_interm_76_triplet_pt3(i, j)
term(53) = term(53) + wm_interm_59_triplet_pt3(i, p, q, j) * wm_interm_76_triplet_pt3(i, j)
term(54) = term(54) + wm_interm_59_triplet_pt3(p, i, j, q) * wm_interm_76_triplet_pt3(i, j)
term(55) = term(55) + wm_interm_59_triplet_pt3(i, p, j, q) * wm_interm_76_triplet_pt3(i, j)
end do 
end do 

term(37) = -term(37) 
term(38) = term(38) * 3.0d+0 
term(39) = term(39) * 2.0d+0 
term(40) = term(40) * (-6.0d+0) 
term(41) = term(41) * (-2.0d+0) 
term(42) = term(42) * (-2.0d+0) 
term(43) = term(43) * 4.0d+0 
term(44) = term(44) * 4.0d+0 
term(45) = term(45) * 4.0d+0 
term(46) = term(46) * (-8.0d+0) 
term(47) = term(47) * (-6.0d+0) 
term(48) = term(48) * 2.0d+0 
term(49) = -term(49) 
term(50) = term(50) * 3.0d+0 
term(51) = term(51) * 8.0d+0 
term(52) = term(52) * (-8.0d+0) 
term(53) = term(53) * (-2.0d+0) 
term(54) = term(54) * (-2.0d+0) 
term(55) = term(55) * 4.0d+0 

do i = 1, nocc 
term(56) = term(56) + wm_interm_5_triplet_pt3(p, i) * wm_interm_68_triplet_pt3(q, i)
term(57) = term(57) + wm_interm_68_triplet_pt3(q, i) * wm_interm_6_triplet_pt3(p, i)
term(58) = term(58) + wm_interm_11_triplet_pt3(p, i) * wm_interm_68_triplet_pt3(q, i)
term(59) = term(59) + wm_interm_12_triplet_pt3(p, i) * wm_interm_68_triplet_pt3(q, i)
term(60) = term(60) + wm_interm_13_triplet_pt3(p, i) * wm_interm_68_triplet_pt3(q, i)
term(61) = term(61) + wm_interm_5_triplet_pt3(i, p) * wm_interm_68_triplet_pt3(i, q)
term(62) = term(62) + wm_interm_68_triplet_pt3(i, q) * wm_interm_6_triplet_pt3(i, p)
term(63) = term(63) + wm_interm_11_triplet_pt3(i, p) * wm_interm_68_triplet_pt3(i, q)
term(64) = term(64) + wm_interm_12_triplet_pt3(i, p) * wm_interm_68_triplet_pt3(i, q)
term(65) = term(65) + wm_interm_13_triplet_pt3(i, p) * wm_interm_68_triplet_pt3(i, q)
term(66) = term(66) + wm_interm_17_triplet_pt3(q, i) * wm_interm_76_triplet_pt3(p, i)
term(67) = term(67) + wm_interm_19_triplet_pt3(q, i) * wm_interm_76_triplet_pt3(p, i)
term(68) = term(68) + wm_interm_19_triplet_pt3(i, q) * wm_interm_76_triplet_pt3(i, p)
term(69) = term(69) + wm_interm_17_triplet_pt3(i, q) * wm_interm_76_triplet_pt3(i, p)
term(70) = term(70) + wm_interm_50_triplet_pt3(q, i) * wm_interm_76_triplet_pt3(p, i)
term(71) = term(71) + wm_interm_51_triplet_pt3(q, i) * wm_interm_76_triplet_pt3(p, i)
term(72) = term(72) + wm_interm_50_triplet_pt3(i, q) * wm_interm_76_triplet_pt3(i, p)
term(73) = term(73) + wm_interm_51_triplet_pt3(i, q) * wm_interm_76_triplet_pt3(i, p)
end do 

term(56) = -term(56) 
term(57) = term(57) * 3.0d+0 
term(58) = term(58) * (-2.0d+0) 
term(59) = term(59) * (-2.0d+0) 
term(60) = term(60) * 4.0d+0 
term(61) = -term(61) 
term(62) = term(62) * 3.0d+0 
term(63) = term(63) * (-2.0d+0) 
term(64) = term(64) * (-2.0d+0) 
term(65) = term(65) * 4.0d+0 
term(66) = term(66) * (-3.0d+0) 
term(68) = -term(68) 
term(69) = term(69) * 3.0d+0 
term(70) = term(70) * (-4.0d+0) 
term(71) = term(71) * 4.0d+0 
term(72) = term(72) * (-4.0d+0) 
term(73) = term(73) * 4.0d+0 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(74) = term(74) + wm_interm_0_triplet_pt3(a, i, p, q) * wm_interm_70_triplet_pt3(a, i)
term(75) = term(75) + wm_interm_0_triplet_pt3(a, p, i, q) * wm_interm_70_triplet_pt3(a, i)
term(76) = term(76) + wm_interm_0_triplet_pt3(a, i, p, q) * wm_interm_71_triplet_pt3(a, i)
term(77) = term(77) + wm_interm_0_triplet_pt3(a, i, p, q) * wm_interm_72_triplet_pt3(a, i)
term(78) = term(78) + wm_interm_0_triplet_pt3(a, p, i, q) * wm_interm_71_triplet_pt3(a, i)
term(79) = term(79) + wm_interm_0_triplet_pt3(a, p, i, q) * wm_interm_72_triplet_pt3(a, i)
term(80) = term(80) + wm_interm_14_triplet_pt3(a, i) * wm_interm_73_triplet_pt3(a, i, q, p)
term(81) = term(81) + wm_interm_14_triplet_pt3(a, i) * wm_interm_74_triplet_pt3(a, i)
term(82) = term(82) + wm_interm_15_triplet_pt3(a, i) * wm_interm_73_triplet_pt3(a, i, q, p)
term(83) = term(83) + wm_interm_15_triplet_pt3(a, i) * wm_interm_74_triplet_pt3(a, i)
term(84) = term(84) + wm_interm_16_triplet_pt3(a, p, i, q) * wm_interm_74_triplet_pt3(a, i)
term(85) = term(85) + wm_interm_16_triplet_pt3(a, p, q, i) * wm_interm_74_triplet_pt3(a, i)
term(86) = term(86) + wm_interm_14_triplet_pt3(a, i) * wm_interm_77_triplet_pt3(a, q, i, p)
term(87) = term(87) + wm_interm_14_triplet_pt3(a, i) * wm_interm_78_triplet_pt3(a, i)
term(88) = term(88) + wm_interm_15_triplet_pt3(a, i) * wm_interm_77_triplet_pt3(a, q, i, p)
term(89) = term(89) + wm_interm_15_triplet_pt3(a, i) * wm_interm_78_triplet_pt3(a, i)
term(90) = term(90) + wm_interm_14_triplet_pt3(a, i) * wm_interm_77_triplet_pt3(a, i, q, p)
term(91) = term(91) + wm_interm_14_triplet_pt3(a, i) * wm_interm_80_triplet_pt3(a, i)
term(92) = term(92) + wm_interm_15_triplet_pt3(a, i) * wm_interm_77_triplet_pt3(a, i, q, p)
term(93) = term(93) + wm_interm_15_triplet_pt3(a, i) * wm_interm_80_triplet_pt3(a, i)
term(94) = term(94) + wm_interm_14_triplet_pt3(a, i) * wm_interm_79_triplet_pt3(a, q, i, p)
term(95) = term(95) + wm_interm_15_triplet_pt3(a, i) * wm_interm_79_triplet_pt3(a, q, i, p)
term(96) = term(96) + wm_interm_16_triplet_pt3(a, p, i, q) * wm_interm_80_triplet_pt3(a, i)
term(97) = term(97) + wm_interm_16_triplet_pt3(a, p, i, q) * wm_interm_78_triplet_pt3(a, i)
term(98) = term(98) + wm_interm_16_triplet_pt3(a, p, q, i) * wm_interm_80_triplet_pt3(a, i)
term(99) = term(99) + wm_interm_16_triplet_pt3(a, p, q, i) * wm_interm_78_triplet_pt3(a, i)
end do 
end do 

term(74) = term(74) * (-8.0d+0) 
term(75) = term(75) * 4.0d+0 
term(76) = term(76) * 8.0d+0 
term(77) = term(77) * (-8.0d+0) 
term(78) = term(78) * (-4.0d+0) 
term(79) = term(79) * 4.0d+0 
term(80) = term(80) * (-4.0d+0) 
term(81) = term(81) * (-8.0d+0) 
term(82) = term(82) * 8.0d+0 
term(83) = term(83) * 16.0d+0 
term(84) = term(84) * (-8.0d+0) 
term(85) = term(85) * 4.0d+0 
term(86) = term(86) * 4.0d+0 
term(87) = term(87) * (-8.0d+0) 
term(88) = term(88) * (-8.0d+0) 
term(89) = term(89) * 16.0d+0 
term(90) = term(90) * (-2.0d+0) 
term(91) = term(91) * 8.0d+0 
term(92) = term(92) * 4.0d+0 
term(93) = term(93) * (-16.0d+0) 
term(94) = term(94) * (-2.0d+0) 
term(95) = term(95) * 4.0d+0 
term(96) = term(96) * 8.0d+0 
term(97) = term(97) * (-8.0d+0) 
term(98) = term(98) * (-4.0d+0) 
term(99) = term(99) * 4.0d+0 


    calc_D_oo_wm_triplet_pt3 = zero
    do s = 0, 99
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
    double precision, dimension(:), intent(in) :: vrdav_Rl
    double precision, dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , k, j, i, a, b 
    real(F64), dimension(0:130) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(0) = term(0) + wm_interm_3_triplet_pt3(q, i, j, k) * wm_interm_4_triplet_pt3(i, j, p, k)
term(1) = term(1) + wm_interm_3_triplet_pt3(q, i, j, k) * wm_interm_4_triplet_pt3(i, j, k, p)
term(2) = term(2) + wm_interm_10_triplet_pt3(i, j, p, k) * wm_interm_3_triplet_pt3(q, i, j, k)
term(3) = term(3) + wm_interm_10_triplet_pt3(i, j, k, p) * wm_interm_3_triplet_pt3(q, i, j, k)
term(4) = term(4) + wm_interm_1_triplet_pt3(i, j, p, k) * wm_interm_32_triplet_pt3(q, k, i, j)
term(5) = term(5) + wm_interm_32_triplet_pt3(q, i, j, k) * wm_interm_7_triplet_pt3(k, j, i, p)
term(6) = term(6) + wm_interm_32_triplet_pt3(q, i, j, k) * wm_interm_7_triplet_pt3(j, k, i, p)
term(7) = term(7) + wm_interm_42_triplet_pt3(q, i, j, k) * wm_interm_4_triplet_pt3(i, j, p, k)
term(8) = term(8) + wm_interm_42_triplet_pt3(q, i, j, k) * wm_interm_4_triplet_pt3(i, j, k, p)
term(9) = term(9) + wm_interm_44_triplet_pt3(q, i, j, k) * wm_interm_4_triplet_pt3(i, j, p, k)
term(10) = term(10) + wm_interm_44_triplet_pt3(q, i, j, k) * wm_interm_4_triplet_pt3(i, j, k, p)
term(11) = term(11) + wm_interm_10_triplet_pt3(i, j, p, k) * wm_interm_42_triplet_pt3(q, i, j, k)
term(12) = term(12) + wm_interm_10_triplet_pt3(i, j, k, p) * wm_interm_42_triplet_pt3(q, i, j, k)
term(13) = term(13) + wm_interm_10_triplet_pt3(i, j, p, k) * wm_interm_44_triplet_pt3(q, i, j, k)
term(14) = term(14) + wm_interm_10_triplet_pt3(i, j, k, p) * wm_interm_44_triplet_pt3(q, i, j, k)
term(15) = term(15) + wm_interm_32_triplet_pt3(q, i, j, k) * wm_interm_45_triplet_pt3(k, j, i, p)
term(16) = term(16) + wm_interm_32_triplet_pt3(q, i, j, k) * wm_interm_45_triplet_pt3(j, k, i, p)
end do 
end do 
end do 

term(0) = term(0) * (-2.0d+0) 
term(1) = term(1) * 2.0d+0 
term(2) = term(2) * (-3.0d+0) 
term(3) = term(3) * 3.0d+0 
term(4) = term(4) * (-2.0d+0) 
term(5) = term(5) * (-2.0d+0) 
term(6) = term(6) * 2.0d+0 
term(7) = term(7) * 3.0d+0 
term(8) = -term(8) 
term(9) = -term(9) 
term(10) = term(10) * 3.0d+0 
term(11) = term(11) * 4.0d+0 
term(12) = term(12) * (-2.0d+0) 
term(13) = term(13) * (-2.0d+0) 
term(14) = term(14) * 4.0d+0 
term(15) = term(15) * (-4.0d+0) 
term(16) = term(16) * 4.0d+0 

do a = nocc + 1, nactive 
term(17) = term(17) + wm_interm_27_triplet_pt3(a, p) * wm_interm_28_triplet_pt3(a, q)
term(18) = term(18) + wm_interm_27_triplet_pt3(a, p) * wm_interm_29_triplet_pt3(a, q)
term(19) = term(19) + wm_interm_28_triplet_pt3(a, q) * wm_interm_30_triplet_pt3(a, p)
term(20) = term(20) + wm_interm_29_triplet_pt3(a, q) * wm_interm_30_triplet_pt3(a, p)
term(21) = term(21) + wm_interm_28_triplet_pt3(a, q) * wm_interm_31_triplet_pt3(a, p)
term(22) = term(22) + wm_interm_29_triplet_pt3(a, q) * wm_interm_31_triplet_pt3(a, p)
term(23) = term(23) + wm_interm_27_triplet_pt3(a, p) * wm_interm_54_triplet_pt3(a, q)
term(24) = term(24) + wm_interm_27_triplet_pt3(a, p) * wm_interm_55_triplet_pt3(a, q)
term(25) = term(25) + wm_interm_30_triplet_pt3(a, p) * wm_interm_54_triplet_pt3(a, q)
term(26) = term(26) + wm_interm_30_triplet_pt3(a, p) * wm_interm_55_triplet_pt3(a, q)
term(27) = term(27) + wm_interm_31_triplet_pt3(a, p) * wm_interm_54_triplet_pt3(a, q)
term(28) = term(28) + wm_interm_31_triplet_pt3(a, p) * wm_interm_55_triplet_pt3(a, q)
term(29) = term(29) + wm_interm_67_triplet_pt3(q, a) * wm_interm_82_triplet_pt3(a, p)
end do 

term(17) = term(17) * 6.0d+0 
term(18) = term(18) * (-2.0d+0) 
term(19) = term(19) * 6.0d+0 
term(20) = term(20) * (-2.0d+0) 
term(21) = term(21) * (-6.0d+0) 
term(22) = term(22) * 2.0d+0 
term(23) = term(23) * (-8.0d+0) 
term(24) = term(24) * 8.0d+0 
term(25) = term(25) * (-8.0d+0) 
term(26) = term(26) * 8.0d+0 
term(27) = term(27) * 8.0d+0 
term(28) = term(28) * (-8.0d+0) 
term(29) = term(29) * 2.0d+0 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(30) = term(30) + wm_interm_10_triplet_pt3(i, j, p, k) * wm_interm_3_triplet_pt3(q, j, i, k)
term(31) = term(31) + wm_interm_10_triplet_pt3(i, j, k, p) * wm_interm_3_triplet_pt3(q, j, i, k)
term(32) = term(32) + wm_interm_1_triplet_pt3(i, j, p, k) * wm_interm_32_triplet_pt3(q, k, j, i)
term(33) = term(33) + wm_interm_32_triplet_pt3(q, i, j, k) * wm_interm_7_triplet_pt3(k, j, p, i)
term(34) = term(34) + wm_interm_32_triplet_pt3(q, i, j, k) * wm_interm_7_triplet_pt3(j, k, p, i)
term(35) = term(35) + wm_interm_10_triplet_pt3(i, j, p, k) * wm_interm_42_triplet_pt3(q, j, i, k)
term(36) = term(36) + wm_interm_10_triplet_pt3(i, j, k, p) * wm_interm_44_triplet_pt3(q, j, i, k)
term(37) = term(37) + wm_interm_32_triplet_pt3(q, i, j, k) * wm_interm_40_triplet_pt3(k, j, p, i)
term(38) = term(38) + wm_interm_32_triplet_pt3(q, i, j, k) * wm_interm_40_triplet_pt3(j, k, p, i)
term(39) = term(39) + wm_interm_32_triplet_pt3(q, i, j, k) * wm_interm_45_triplet_pt3(k, j, p, i)
term(40) = term(40) + wm_interm_32_triplet_pt3(q, i, j, k) * wm_interm_45_triplet_pt3(j, k, p, i)
end do 
end do 
end do 

term(31) = -term(31) 
term(32) = term(32) * 2.0d+0 
term(33) = term(33) * 2.0d+0 
term(34) = term(34) * (-2.0d+0) 
term(35) = term(35) * (-2.0d+0) 
term(36) = term(36) * (-2.0d+0) 
term(37) = term(37) * 4.0d+0 
term(38) = term(38) * (-4.0d+0) 
term(39) = term(39) * 4.0d+0 
term(40) = term(40) * (-4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(41) = term(41) + r2m(vrdav_Rl, a,i,q,j) * r2m(vrdav_Rr, a,i,b,k) * wm_interm_0_triplet_pt3(b, p, k, j)
term(42) = term(42) + r2m(vrdav_Rl, a,i,q,j) * r2m(vrdav_Rr, a,i,b,k) * wm_interm_0_triplet_pt3(b, k, p, j)
end do 
end do 
end do 
end do 
end do 

term(41) = term(41) * 4.0d+0 
term(42) = term(42) * (-8.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(43) = term(43) + r2m(vrdav_Rl, a,j,q,i) * r2m(vrdav_Rr, a,k,b,i) * wm_interm_0_triplet_pt3(b, p, k, j)
term(44) = term(44) + r2m(vrdav_Rl, a,j,q,i) * r2m(vrdav_Rr, a,k,b,i) * wm_interm_0_triplet_pt3(b, k, p, j)
term(45) = term(45) + r2m(vrdav_Rl, a,i,q,j) * r2m(vrdav_Rr, a,k,b,i) * wm_interm_0_triplet_pt3(b, p, k, j)
term(46) = term(46) + r2m(vrdav_Rl, a,i,q,j) * r2m(vrdav_Rr, a,k,b,i) * wm_interm_0_triplet_pt3(b, k, p, j)
term(47) = term(47) + r2m(vrdav_Rl, a,j,q,i) * r2m(vrdav_Rr, a,k,b,j) * wm_interm_0_triplet_pt3(b, p, k, i)
term(48) = term(48) + r2m(vrdav_Rl, a,i,q,j) * r2m(vrdav_Rr, a,k,b,j) * wm_interm_0_triplet_pt3(b, p, k, i)
term(49) = term(49) + r2m(vrdav_Rl, a,i,q,j) * r2m(vrdav_Rr, a,k,b,j) * wm_interm_0_triplet_pt3(b, k, p, i)
end do 
end do 
end do 
end do 
end do 

term(43) = term(43) * 4.0d+0 
term(44) = term(44) * (-4.0d+0) 
term(45) = term(45) * (-4.0d+0) 
term(46) = term(46) * 8.0d+0 
term(47) = term(47) * (-4.0d+0) 
term(48) = term(48) * 4.0d+0 
term(49) = term(49) * (-4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
do k = 1, nocc 
do b = nocc + 1, nactive 
term(50) = term(50) + r2p(vrdav_Rl, q,i,a,j) * r2p(vrdav_Rr, b,k,a,j) * wm_interm_0_triplet_pt3(b, p, k, i)
term(51) = term(51) + r2p(vrdav_Rl, q,i,a,j) * r2p(vrdav_Rr, b,k,a,j) * wm_interm_0_triplet_pt3(b, k, p, i)
term(52) = term(52) + r2m(vrdav_Rl, a,j,q,i) * r2p(vrdav_Rr, b,k,a,i) * wm_interm_0_triplet_pt3(b, p, k, j)
term(53) = term(53) + r2m(vrdav_Rl, a,j,q,i) * r2p(vrdav_Rr, b,k,a,i) * wm_interm_0_triplet_pt3(b, k, p, j)
term(54) = term(54) + r2m(vrdav_Rl, a,i,q,j) * r2p(vrdav_Rr, b,k,a,i) * wm_interm_0_triplet_pt3(b, p, k, j)
term(55) = term(55) + r2m(vrdav_Rl, a,i,q,j) * r2p(vrdav_Rr, b,k,a,i) * wm_interm_0_triplet_pt3(b, k, p, j)
term(56) = term(56) + r2m(vrdav_Rl, a,j,q,i) * r2p(vrdav_Rr, b,k,a,j) * wm_interm_0_triplet_pt3(b, p, k, i)
term(57) = term(57) + r2m(vrdav_Rl, a,i,q,j) * r2p(vrdav_Rr, b,k,a,j) * wm_interm_0_triplet_pt3(b, p, k, i)
term(58) = term(58) + r2m(vrdav_Rl, a,i,q,j) * r2p(vrdav_Rr, b,k,a,j) * wm_interm_0_triplet_pt3(b, k, p, i)
end do 
end do 
end do 
end do 
end do 

term(50) = term(50) * 8.0d+0 
term(51) = term(51) * (-8.0d+0) 
term(52) = term(52) * (-4.0d+0) 
term(53) = term(53) * 4.0d+0 
term(54) = term(54) * 4.0d+0 
term(55) = term(55) * (-8.0d+0) 
term(56) = term(56) * 4.0d+0 
term(57) = term(57) * (-4.0d+0) 
term(58) = term(58) * 4.0d+0 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(59) = term(59) + r2p(vrdav_Rl, q,i,a,j) * r2m(vrdav_Rr, a,j,b,k) * wm_interm_0_triplet_pt3(b, p, k, i)
term(60) = term(60) + r2p(vrdav_Rl, q,i,a,j) * r2m(vrdav_Rr, a,j,b,k) * wm_interm_0_triplet_pt3(b, k, p, i)
term(61) = term(61) + r2m(vrdav_Rl, a,j,q,i) * r2m(vrdav_Rr, a,i,b,k) * wm_interm_0_triplet_pt3(b, p, k, j)
term(62) = term(62) + r2m(vrdav_Rl, a,j,q,i) * r2m(vrdav_Rr, a,i,b,k) * wm_interm_0_triplet_pt3(b, k, p, j)
term(63) = term(63) + r2m(vrdav_Rl, a,i,q,j) * r2m(vrdav_Rr, a,j,b,k) * wm_interm_0_triplet_pt3(b, p, k, i)
term(64) = term(64) + r2m(vrdav_Rl, a,i,q,j) * r2m(vrdav_Rr, a,j,b,k) * wm_interm_0_triplet_pt3(b, k, p, i)
term(65) = term(65) + r2m(vrdav_Rl, a,i,b,j) * t2(a,q,k,i) * wm_interm_18_triplet_pt3(b, j, p, k)
term(66) = term(66) + r2m(vrdav_Rl, a,i,b,j) * t2(b,q,k,i) * wm_interm_18_triplet_pt3(a, j, p, k)
term(67) = term(67) + r2m(vrdav_Rl, a,i,b,j) * t2(a,q,k,i) * wm_interm_20_triplet_pt3(b, p, j, k)
term(68) = term(68) + r2m(vrdav_Rl, a,i,b,j) * t2(a,q,k,i) * wm_interm_20_triplet_pt3(b, j, p, k)
term(69) = term(69) + r2m(vrdav_Rl, a,i,b,j) * t2(b,q,k,i) * wm_interm_20_triplet_pt3(a, j, p, k)
term(70) = term(70) + r2m(vrdav_Rl, a,i,b,j) * t2(b,q,k,i) * wm_interm_20_triplet_pt3(a, p, j, k)
end do 
end do 
end do 
end do 
end do 

term(59) = term(59) * 8.0d+0 
term(60) = term(60) * (-8.0d+0) 
term(61) = term(61) * (-4.0d+0) 
term(62) = term(62) * 4.0d+0 
term(63) = term(63) * (-4.0d+0) 
term(64) = term(64) * 4.0d+0 
term(65) = term(65) * 4.0d+0 
term(66) = term(66) * (-4.0d+0) 
term(67) = term(67) * (-4.0d+0) 
term(68) = term(68) * 4.0d+0 
term(69) = term(69) * (-4.0d+0) 
term(70) = term(70) * 4.0d+0 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do k = 1, nocc 
term(71) = term(71) + r2p(vrdav_Rl, q,i,a,j) * r2m(vrdav_Rr, a,k,b,j) * wm_interm_0_triplet_pt3(b, p, k, i)
term(72) = term(72) + r2p(vrdav_Rl, q,i,a,j) * r2m(vrdav_Rr, a,k,b,j) * wm_interm_0_triplet_pt3(b, k, p, i)
end do 
end do 
end do 
end do 
end do 

term(71) = term(71) * (-8.0d+0) 
term(72) = term(72) * 8.0d+0 

do k = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(73) = term(73) + r2p(vrdav_Rl, a,j,b,k) * t2(b,q,k,i) * wm_interm_18_triplet_pt3(a, j, p, i)
term(74) = term(74) + r2p(vrdav_Rl, a,j,b,i) * t2(b,q,k,i) * wm_interm_18_triplet_pt3(a, j, p, k)
term(75) = term(75) + r2p(vrdav_Rl, a,j,b,k) * t2(b,q,k,i) * wm_interm_20_triplet_pt3(a, p, j, i)
term(76) = term(76) + r2p(vrdav_Rl, a,j,b,k) * t2(b,q,k,i) * wm_interm_20_triplet_pt3(a, j, p, i)
term(77) = term(77) + r2p(vrdav_Rl, a,j,b,i) * t2(b,q,k,i) * wm_interm_20_triplet_pt3(a, p, j, k)
term(78) = term(78) + r2p(vrdav_Rl, a,j,b,i) * t2(b,q,k,i) * wm_interm_20_triplet_pt3(a, j, p, k)
term(79) = term(79) + r2m(vrdav_Rl, a,j,q,i) * r2m(vrdav_Rr, a,j,b,k) * wm_interm_0_triplet_pt3(b, p, k, i)
term(80) = term(80) + r2m(vrdav_Rl, a,j,b,k) * t2(a,q,k,i) * wm_interm_18_triplet_pt3(b, j, p, i)
term(81) = term(81) + r2m(vrdav_Rl, a,j,b,k) * t2(b,q,j,i) * wm_interm_18_triplet_pt3(a, k, p, i)
term(82) = term(82) + r2m(vrdav_Rl, a,j,b,k) * t2(b,q,k,i) * wm_interm_18_triplet_pt3(a, j, p, i)
term(83) = term(83) + r2m(vrdav_Rl, a,j,b,i) * t2(a,q,k,i) * wm_interm_18_triplet_pt3(b, j, p, k)
term(84) = term(84) + r2m(vrdav_Rl, a,j,b,i) * t2(b,q,k,i) * wm_interm_18_triplet_pt3(a, j, p, k)
term(85) = term(85) + r2m(vrdav_Rl, a,j,b,k) * t2(a,q,k,i) * wm_interm_20_triplet_pt3(b, p, j, i)
term(86) = term(86) + r2m(vrdav_Rl, a,j,b,k) * t2(b,q,j,i) * wm_interm_20_triplet_pt3(a, p, k, i)
term(87) = term(87) + r2m(vrdav_Rl, a,j,b,k) * t2(b,q,k,i) * wm_interm_20_triplet_pt3(a, p, j, i)
term(88) = term(88) + r2m(vrdav_Rl, a,j,b,k) * t2(a,q,k,i) * wm_interm_20_triplet_pt3(b, j, p, i)
term(89) = term(89) + r2m(vrdav_Rl, a,j,b,k) * t2(b,q,j,i) * wm_interm_20_triplet_pt3(a, k, p, i)
term(90) = term(90) + r2m(vrdav_Rl, a,j,b,k) * t2(b,q,k,i) * wm_interm_20_triplet_pt3(a, j, p, i)
term(91) = term(91) + r2m(vrdav_Rl, a,j,b,i) * t2(a,q,k,i) * wm_interm_20_triplet_pt3(b, j, p, k)
term(92) = term(92) + r2m(vrdav_Rl, a,j,b,i) * t2(a,q,k,i) * wm_interm_20_triplet_pt3(b, p, j, k)
term(93) = term(93) + r2m(vrdav_Rl, a,j,b,i) * t2(b,q,k,i) * wm_interm_20_triplet_pt3(a, p, j, k)
term(94) = term(94) + r2m(vrdav_Rl, a,j,b,i) * t2(b,q,k,i) * wm_interm_20_triplet_pt3(a, j, p, k)
end do 
end do 
end do 
end do 
end do 

term(73) = term(73) * (-8.0d+0) 
term(74) = term(74) * 8.0d+0 
term(75) = term(75) * 8.0d+0 
term(76) = term(76) * (-8.0d+0) 
term(77) = term(77) * (-8.0d+0) 
term(78) = term(78) * 8.0d+0 
term(79) = term(79) * 4.0d+0 
term(80) = term(80) * 4.0d+0 
term(81) = term(81) * 4.0d+0 
term(82) = term(82) * (-8.0d+0) 
term(83) = term(83) * (-4.0d+0) 
term(84) = term(84) * 4.0d+0 
term(85) = term(85) * (-4.0d+0) 
term(86) = term(86) * (-4.0d+0) 
term(87) = term(87) * 8.0d+0 
term(88) = term(88) * 4.0d+0 
term(89) = term(89) * 4.0d+0 
term(90) = term(90) * (-8.0d+0) 
term(91) = term(91) * (-4.0d+0) 
term(92) = term(92) * 4.0d+0 
term(93) = term(93) * (-4.0d+0) 
term(94) = term(94) * 4.0d+0 

do i = 1, nocc 
term(95) = term(95) + wm_interm_22_triplet_pt3(q, i) * wm_interm_5_triplet_pt3(i, p)
term(96) = term(96) + wm_interm_22_triplet_pt3(q, i) * wm_interm_6_triplet_pt3(i, p)
term(97) = term(97) + wm_interm_11_triplet_pt3(i, p) * wm_interm_22_triplet_pt3(q, i)
term(98) = term(98) + wm_interm_12_triplet_pt3(i, p) * wm_interm_22_triplet_pt3(q, i)
term(99) = term(99) + wm_interm_13_triplet_pt3(i, p) * wm_interm_22_triplet_pt3(q, i)
term(100) = term(100) + wm_interm_14_triplet_pt3(q, i) * wm_interm_2_triplet_pt3(i, p)
term(101) = term(101) + wm_interm_15_triplet_pt3(q, i) * wm_interm_2_triplet_pt3(i, p)
term(102) = term(102) + wm_interm_14_triplet_pt3(q, i) * wm_interm_9_triplet_pt3(i, p)
term(103) = term(103) + wm_interm_14_triplet_pt3(q, i) * wm_interm_8_triplet_pt3(i, p)
term(104) = term(104) + wm_interm_15_triplet_pt3(q, i) * wm_interm_9_triplet_pt3(i, p)
term(105) = term(105) + wm_interm_15_triplet_pt3(q, i) * wm_interm_8_triplet_pt3(i, p)
term(106) = term(106) + wm_interm_52_triplet_pt3(q, i) * wm_interm_5_triplet_pt3(i, p)
term(107) = term(107) + wm_interm_52_triplet_pt3(q, i) * wm_interm_6_triplet_pt3(i, p)
term(108) = term(108) + wm_interm_53_triplet_pt3(q, i) * wm_interm_5_triplet_pt3(i, p)
term(109) = term(109) + wm_interm_53_triplet_pt3(q, i) * wm_interm_6_triplet_pt3(i, p)
term(110) = term(110) + wm_interm_11_triplet_pt3(i, p) * wm_interm_52_triplet_pt3(q, i)
term(111) = term(111) + wm_interm_12_triplet_pt3(i, p) * wm_interm_52_triplet_pt3(q, i)
term(112) = term(112) + wm_interm_13_triplet_pt3(i, p) * wm_interm_52_triplet_pt3(q, i)
term(113) = term(113) + wm_interm_11_triplet_pt3(i, p) * wm_interm_53_triplet_pt3(q, i)
term(114) = term(114) + wm_interm_12_triplet_pt3(i, p) * wm_interm_53_triplet_pt3(q, i)
term(115) = term(115) + wm_interm_13_triplet_pt3(i, p) * wm_interm_53_triplet_pt3(q, i)
term(116) = term(116) + wm_interm_14_triplet_pt3(q, i) * wm_interm_43_triplet_pt3(i, p)
term(117) = term(117) + wm_interm_14_triplet_pt3(q, i) * wm_interm_41_triplet_pt3(i, p)
term(118) = term(118) + wm_interm_15_triplet_pt3(q, i) * wm_interm_43_triplet_pt3(i, p)
term(119) = term(119) + wm_interm_15_triplet_pt3(q, i) * wm_interm_41_triplet_pt3(i, p)
term(120) = term(120) + wm_interm_14_triplet_pt3(q, i) * wm_interm_48_triplet_pt3(i, p)
term(121) = term(121) + wm_interm_14_triplet_pt3(q, i) * wm_interm_49_triplet_pt3(i, p)
term(122) = term(122) + wm_interm_14_triplet_pt3(q, i) * wm_interm_46_triplet_pt3(i, p)
term(123) = term(123) + wm_interm_14_triplet_pt3(q, i) * wm_interm_47_triplet_pt3(i, p)
term(124) = term(124) + wm_interm_15_triplet_pt3(q, i) * wm_interm_48_triplet_pt3(i, p)
term(125) = term(125) + wm_interm_15_triplet_pt3(q, i) * wm_interm_49_triplet_pt3(i, p)
term(126) = term(126) + wm_interm_15_triplet_pt3(q, i) * wm_interm_46_triplet_pt3(i, p)
term(127) = term(127) + wm_interm_15_triplet_pt3(q, i) * wm_interm_47_triplet_pt3(i, p)
term(128) = term(128) + wm_interm_14_triplet_pt3(q, i) * wm_interm_81_triplet_pt3(i, p)
term(129) = term(129) + wm_interm_15_triplet_pt3(q, i) * wm_interm_81_triplet_pt3(i, p)
term(130) = term(130) + wm_interm_76_triplet_pt3(p, i) * wm_interm_86_triplet_pt3(q, i)
end do 

term(95) = term(95) * (-2.0d+0) 
term(96) = term(96) * 6.0d+0 
term(97) = term(97) * (-4.0d+0) 
term(98) = term(98) * (-4.0d+0) 
term(99) = term(99) * 8.0d+0 
term(100) = term(100) * 4.0d+0 
term(101) = term(101) * (-8.0d+0) 
term(102) = term(102) * 4.0d+0 
term(103) = term(103) * (-4.0d+0) 
term(104) = term(104) * (-8.0d+0) 
term(105) = term(105) * 8.0d+0 
term(106) = term(106) * 2.0d+0 
term(107) = term(107) * (-6.0d+0) 
term(108) = term(108) * (-2.0d+0) 
term(109) = term(109) * 6.0d+0 
term(110) = term(110) * 4.0d+0 
term(111) = term(111) * 4.0d+0 
term(112) = term(112) * (-8.0d+0) 
term(113) = term(113) * (-4.0d+0) 
term(114) = term(114) * (-4.0d+0) 
term(115) = term(115) * 8.0d+0 
term(116) = term(116) * (-4.0d+0) 
term(117) = term(117) * 4.0d+0 
term(118) = term(118) * 8.0d+0 
term(119) = term(119) * (-8.0d+0) 
term(120) = term(120) * (-4.0d+0) 
term(121) = term(121) * 4.0d+0 
term(122) = term(122) * (-4.0d+0) 
term(123) = term(123) * 4.0d+0 
term(124) = term(124) * 8.0d+0 
term(125) = term(125) * (-8.0d+0) 
term(126) = term(126) * 8.0d+0 
term(127) = term(127) * (-8.0d+0) 
term(128) = term(128) * 2.0d+0 
term(129) = term(129) * (-4.0d+0) 
term(130) = term(130) * 2.0d+0 


    calc_D_ov_wm_triplet_pt3 = zero
    do s = 0, 130
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
    double precision, dimension(:), intent(in) :: vrdav_Rl
    double precision, dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , k, i, j, b, a 
    real(F64), dimension(0:347) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(0) = term(0) + wm_interm_0_triplet_pt3(p, i, j, k) * wm_interm_1_triplet_pt3(k, q, j, i)
term(1) = term(1) + wm_interm_0_triplet_pt3(p, i, j, k) * wm_interm_7_triplet_pt3(k, q, j, i)
term(2) = term(2) + wm_interm_0_triplet_pt3(p, i, j, k) * wm_interm_40_triplet_pt3(q, k, j, i)
term(3) = term(3) + wm_interm_0_triplet_pt3(p, i, j, k) * wm_interm_40_triplet_pt3(k, q, j, i)
term(4) = term(4) + wm_interm_0_triplet_pt3(p, i, j, k) * wm_interm_45_triplet_pt3(q, k, j, i)
term(5) = term(5) + wm_interm_0_triplet_pt3(p, i, j, k) * wm_interm_45_triplet_pt3(k, q, j, i)
end do 
end do 
end do 

term(0) = term(0) * 2.0d+0 
term(1) = term(1) * 4.0d+0 
term(2) = term(2) * (-2.0d+0) 
term(3) = term(3) * 2.0d+0 
term(4) = term(4) * (-4.0d+0) 
term(5) = term(5) * 4.0d+0 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(6) = term(6) + wm_interm_0_triplet_pt3(p, i, j, k) * wm_interm_1_triplet_pt3(k, q, i, j)
term(7) = term(7) + wm_interm_0_triplet_pt3(p, i, j, k) * wm_interm_7_triplet_pt3(k, q, i, j)
term(8) = term(8) + wm_interm_18_triplet_pt3(p, i, j, k) * wm_interm_35_triplet_pt3(i, j, k, q)
term(9) = term(9) + wm_interm_18_triplet_pt3(p, i, j, k) * wm_interm_35_triplet_pt3(i, j, q, k)
term(10) = term(10) + wm_interm_21_triplet_pt3(p, i, j, k) * wm_interm_35_triplet_pt3(j, i, k, q)
term(11) = term(11) + wm_interm_21_triplet_pt3(p, i, j, k) * wm_interm_35_triplet_pt3(i, j, k, q)
term(12) = term(12) + wm_interm_21_triplet_pt3(p, i, j, k) * wm_interm_35_triplet_pt3(i, j, q, k)
term(13) = term(13) + wm_interm_20_triplet_pt3(p, i, j, k) * wm_interm_35_triplet_pt3(j, i, q, k)
term(14) = term(14) + wm_interm_20_triplet_pt3(p, i, j, k) * wm_interm_35_triplet_pt3(i, j, q, k)
term(15) = term(15) + wm_interm_20_triplet_pt3(p, i, j, k) * wm_interm_35_triplet_pt3(i, j, k, q)
term(16) = term(16) + wm_interm_0_triplet_pt3(p, i, j, k) * wm_interm_40_triplet_pt3(q, k, i, j)
term(17) = term(17) + wm_interm_0_triplet_pt3(p, i, j, k) * wm_interm_40_triplet_pt3(k, q, i, j)
term(18) = term(18) + wm_interm_0_triplet_pt3(p, i, j, k) * wm_interm_45_triplet_pt3(q, k, i, j)
term(19) = term(19) + wm_interm_0_triplet_pt3(p, i, j, k) * wm_interm_45_triplet_pt3(k, q, i, j)
term(20) = term(20) + wm_interm_18_triplet_pt3(p, i, j, k) * wm_interm_59_triplet_pt3(i, j, k, q)
term(21) = term(21) + wm_interm_18_triplet_pt3(p, i, j, k) * wm_interm_59_triplet_pt3(i, j, q, k)
term(22) = term(22) + wm_interm_21_triplet_pt3(p, i, j, k) * wm_interm_59_triplet_pt3(j, i, k, q)
term(23) = term(23) + wm_interm_21_triplet_pt3(p, i, j, k) * wm_interm_59_triplet_pt3(i, j, k, q)
term(24) = term(24) + wm_interm_21_triplet_pt3(p, i, j, k) * wm_interm_59_triplet_pt3(i, j, q, k)
term(25) = term(25) + wm_interm_20_triplet_pt3(p, i, j, k) * wm_interm_59_triplet_pt3(j, i, q, k)
term(26) = term(26) + wm_interm_20_triplet_pt3(p, i, j, k) * wm_interm_59_triplet_pt3(i, j, q, k)
term(27) = term(27) + wm_interm_20_triplet_pt3(p, i, j, k) * wm_interm_59_triplet_pt3(i, j, k, q)
end do 
end do 
end do 

term(6) = term(6) * (-2.0d+0) 
term(7) = term(7) * (-4.0d+0) 
term(8) = term(8) * (-2.0d+0) 
term(9) = term(9) * 2.0d+0 
term(10) = -term(10) 
term(11) = term(11) * 2.0d+0 
term(12) = -term(12) 
term(13) = -term(13) 
term(14) = term(14) * 2.0d+0 
term(15) = -term(15) 
term(16) = term(16) * 2.0d+0 
term(17) = term(17) * (-2.0d+0) 
term(18) = term(18) * 4.0d+0 
term(19) = term(19) * (-4.0d+0) 
term(20) = term(20) * (-4.0d+0) 
term(21) = term(21) * 4.0d+0 
term(22) = term(22) * (-2.0d+0) 
term(23) = term(23) * 4.0d+0 
term(24) = term(24) * (-2.0d+0) 
term(25) = term(25) * (-2.0d+0) 
term(26) = term(26) * 4.0d+0 
term(27) = term(27) * (-2.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(28) = term(28) + r2m(vrdav_Rl, a,q,p,i) * r2m(vrdav_Rr, a,j,b,k) * wm_interm_0_triplet_pt3(b, j, k, i)
term(29) = term(29) + r2m(vrdav_Rl, a,q,p,i) * r2m(vrdav_Rr, a,j,b,k) * wm_interm_0_triplet_pt3(b, k, j, i)
term(30) = term(30) + r2m(vrdav_Rl, a,i,p,q) * r2m(vrdav_Rr, a,j,b,k) * wm_interm_0_triplet_pt3(b, j, k, i)
term(31) = term(31) + r2m(vrdav_Rl, a,i,p,q) * r2m(vrdav_Rr, a,j,b,k) * wm_interm_0_triplet_pt3(b, k, j, i)
term(32) = term(32) + r2m(vrdav_Rl, a,j,b,k) * t2(a,p,q,i) * wm_interm_18_triplet_pt3(b, j, k, i)
term(33) = term(33) + r2m(vrdav_Rl, a,j,b,k) * t2(a,p,q,i) * wm_interm_20_triplet_pt3(b, j, k, i)
term(34) = term(34) + r2m(vrdav_Rl, a,j,b,k) * t2(a,p,q,i) * wm_interm_20_triplet_pt3(b, k, j, i)
end do 
end do 
end do 
end do 
end do 

term(28) = term(28) * (-8.0d+0) 
term(29) = term(29) * 8.0d+0 
term(30) = term(30) * 8.0d+0 
term(31) = term(31) * (-8.0d+0) 
term(32) = term(32) * (-4.0d+0) 
term(33) = term(33) * (-4.0d+0) 
term(34) = term(34) * 4.0d+0 

do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(35) = term(35) + r2p(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,q,b,j) * wm_interm_14_triplet_pt3(a, i)
term(36) = term(36) + r2p(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,q,b,j) * wm_interm_15_triplet_pt3(a, i)
term(37) = term(37) + r2p(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,q,p,j) * wm_interm_14_triplet_pt3(a, i)
term(38) = term(38) + r2p(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,j,p,q) * wm_interm_14_triplet_pt3(a, i)
term(39) = term(39) + r2p(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,q,p,j) * wm_interm_15_triplet_pt3(a, i)
term(40) = term(40) + r2p(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,j,p,q) * wm_interm_15_triplet_pt3(a, i)
term(41) = term(41) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,j,q) * wm_interm_22_triplet_pt3(a, i)
term(42) = term(42) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,q,j) * wm_interm_22_triplet_pt3(a, i)
term(43) = term(43) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,i,q) * wm_interm_22_triplet_pt3(a, j)
term(44) = term(44) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,j,q) * wm_interm_22_triplet_pt3(a, i)
term(45) = term(45) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,q,j) * wm_interm_22_triplet_pt3(a, i)
term(46) = term(46) + r2p(vrdav_Rl, a,i,b,j) * t2(b,p,j,q) * wm_interm_27_triplet_pt3(a, i)
term(47) = term(47) + r2p(vrdav_Rl, a,i,b,j) * t2(b,p,j,q) * wm_interm_30_triplet_pt3(a, i)
term(48) = term(48) + r2p(vrdav_Rl, a,i,b,j) * t2(b,p,j,q) * wm_interm_31_triplet_pt3(a, i)
term(49) = term(49) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,i,p,q) * wm_interm_14_triplet_pt3(b, j)
term(50) = term(50) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,i,p,q) * wm_interm_15_triplet_pt3(b, j)
term(51) = term(51) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,q,p,j) * wm_interm_14_triplet_pt3(b, i)
term(52) = term(52) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,j,p,q) * wm_interm_14_triplet_pt3(b, i)
term(53) = term(53) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,q,p,j) * wm_interm_15_triplet_pt3(b, i)
term(54) = term(54) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,j,p,q) * wm_interm_15_triplet_pt3(b, i)
term(55) = term(55) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,i,p,q) * wm_interm_14_triplet_pt3(a, j)
term(56) = term(56) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,i,p,q) * wm_interm_15_triplet_pt3(a, j)
term(57) = term(57) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,j,q) * wm_interm_52_triplet_pt3(a, i)
term(58) = term(58) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,j,q) * wm_interm_53_triplet_pt3(a, i)
term(59) = term(59) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,q,j) * wm_interm_52_triplet_pt3(a, i)
term(60) = term(60) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,q,j) * wm_interm_53_triplet_pt3(a, i)
term(61) = term(61) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,i,q) * wm_interm_52_triplet_pt3(a, j)
term(62) = term(62) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,j,q) * wm_interm_52_triplet_pt3(a, i)
term(63) = term(63) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,i,q) * wm_interm_53_triplet_pt3(a, j)
term(64) = term(64) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,j,q) * wm_interm_53_triplet_pt3(a, i)
term(65) = term(65) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,q,j) * wm_interm_52_triplet_pt3(a, i)
term(66) = term(66) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,q,j) * wm_interm_53_triplet_pt3(a, i)
term(67) = term(67) + r2m(vrdav_Rl, a,i,b,j) * t2(a,p,j,q) * wm_interm_27_triplet_pt3(b, i)
term(68) = term(68) + r2m(vrdav_Rl, a,i,b,j) * t2(b,p,i,q) * wm_interm_27_triplet_pt3(a, j)
term(69) = term(69) + r2m(vrdav_Rl, a,i,b,j) * t2(b,p,j,q) * wm_interm_27_triplet_pt3(a, i)
term(70) = term(70) + r2m(vrdav_Rl, a,i,b,j) * t2(a,p,j,q) * wm_interm_30_triplet_pt3(b, i)
term(71) = term(71) + r2m(vrdav_Rl, a,i,b,j) * t2(b,p,i,q) * wm_interm_30_triplet_pt3(a, j)
term(72) = term(72) + r2m(vrdav_Rl, a,i,b,j) * t2(b,p,j,q) * wm_interm_30_triplet_pt3(a, i)
term(73) = term(73) + r2m(vrdav_Rl, a,i,b,j) * t2(a,p,j,q) * wm_interm_31_triplet_pt3(b, i)
term(74) = term(74) + r2m(vrdav_Rl, a,i,b,j) * t2(b,p,i,q) * wm_interm_31_triplet_pt3(a, j)
term(75) = term(75) + r2m(vrdav_Rl, a,i,b,j) * t2(b,p,j,q) * wm_interm_31_triplet_pt3(a, i)
end do 
end do 
end do 
end do 

term(35) = term(35) * (-8.0d+0) 
term(36) = term(36) * 16.0d+0 
term(37) = term(37) * 8.0d+0 
term(38) = term(38) * (-8.0d+0) 
term(39) = term(39) * (-16.0d+0) 
term(40) = term(40) * 16.0d+0 
term(41) = term(41) * 16.0d+0 
term(42) = term(42) * (-8.0d+0) 
term(43) = term(43) * (-16.0d+0) 
term(44) = term(44) * 16.0d+0 
term(45) = term(45) * (-8.0d+0) 
term(46) = term(46) * (-16.0d+0) 
term(47) = term(47) * (-16.0d+0) 
term(48) = term(48) * 16.0d+0 
term(49) = term(49) * (-8.0d+0) 
term(50) = term(50) * 16.0d+0 
term(51) = term(51) * (-4.0d+0) 
term(52) = term(52) * 4.0d+0 
term(53) = term(53) * 8.0d+0 
term(54) = term(54) * (-8.0d+0) 
term(55) = term(55) * 4.0d+0 
term(56) = term(56) * (-8.0d+0) 
term(57) = term(57) * 16.0d+0 
term(58) = term(58) * (-16.0d+0) 
term(59) = term(59) * (-8.0d+0) 
term(60) = term(60) * 8.0d+0 
term(61) = term(61) * (-16.0d+0) 
term(62) = term(62) * 16.0d+0 
term(63) = term(63) * 16.0d+0 
term(64) = term(64) * (-16.0d+0) 
term(65) = term(65) * (-8.0d+0) 
term(66) = term(66) * 8.0d+0 
term(67) = term(67) * 8.0d+0 
term(68) = term(68) * 8.0d+0 
term(69) = term(69) * (-16.0d+0) 
term(70) = term(70) * 8.0d+0 
term(71) = term(71) * 8.0d+0 
term(72) = term(72) * (-16.0d+0) 
term(73) = term(73) * (-8.0d+0) 
term(74) = term(74) * (-8.0d+0) 
term(75) = term(75) * 16.0d+0 

do i = 1, nocc 
do j = 1, nocc 
term(76) = term(76) + wm_interm_2_triplet_pt3(i, j) * wm_interm_32_triplet_pt3(p, j, q, i)
term(77) = term(77) + wm_interm_2_triplet_pt3(i, j) * wm_interm_32_triplet_pt3(p, j, i, q)
end do 
end do 

term(76) = term(76) * 4.0d+0 
term(77) = term(77) * (-8.0d+0) 

do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(78) = term(78) + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,q,a,j) * wm_interm_14_triplet_pt3(b, i)
term(79) = term(79) + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,q,a,j) * wm_interm_15_triplet_pt3(b, i)
end do 
end do 
end do 
end do 

term(78) = term(78) * 4.0d+0 
term(79) = term(79) * (-8.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(80) = term(80) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,q,i) * wm_interm_22_triplet_pt3(a, j)
term(81) = term(81) + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,q,a,i) * wm_interm_14_triplet_pt3(b, j)
term(82) = term(82) + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,q,a,i) * wm_interm_15_triplet_pt3(b, j)
term(83) = term(83) + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,q,b,i) * wm_interm_14_triplet_pt3(a, j)
term(84) = term(84) + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,q,b,i) * wm_interm_15_triplet_pt3(a, j)
term(85) = term(85) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,q,p,i) * wm_interm_14_triplet_pt3(b, j)
term(86) = term(86) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,q,p,i) * wm_interm_15_triplet_pt3(b, j)
term(87) = term(87) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,q,p,i) * wm_interm_14_triplet_pt3(a, j)
term(88) = term(88) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,q,p,i) * wm_interm_15_triplet_pt3(a, j)
term(89) = term(89) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,q,i) * wm_interm_52_triplet_pt3(a, j)
term(90) = term(90) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,q,i) * wm_interm_53_triplet_pt3(a, j)
term(91) = term(91) + r2m(vrdav_Rl, a,i,b,j) * t2(a,p,q,i) * wm_interm_27_triplet_pt3(b, j)
term(92) = term(92) + r2m(vrdav_Rl, a,i,b,j) * t2(a,p,q,i) * wm_interm_30_triplet_pt3(b, j)
term(93) = term(93) + r2m(vrdav_Rl, a,i,b,j) * t2(a,p,q,i) * wm_interm_31_triplet_pt3(b, j)
end do 
end do 
end do 
end do 

term(80) = term(80) * 8.0d+0 
term(81) = term(81) * (-8.0d+0) 
term(82) = term(82) * 16.0d+0 
term(83) = term(83) * 4.0d+0 
term(84) = term(84) * (-8.0d+0) 
term(85) = term(85) * 8.0d+0 
term(86) = term(86) * (-16.0d+0) 
term(87) = term(87) * (-4.0d+0) 
term(88) = term(88) * 8.0d+0 
term(89) = term(89) * 8.0d+0 
term(90) = term(90) * (-8.0d+0) 
term(91) = term(91) * 8.0d+0 
term(92) = term(92) * 8.0d+0 
term(93) = term(93) * (-8.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
do j = 1, nocc 
do b = nocc + 1, nactive 
term(94) = term(94) + r2p(vrdav_Rl, b,j,a,i) * t2(a,p,q,i) * wm_interm_27_triplet_pt3(b, j)
term(95) = term(95) + r2p(vrdav_Rl, b,j,a,i) * t2(a,p,q,i) * wm_interm_30_triplet_pt3(b, j)
term(96) = term(96) + r2p(vrdav_Rl, b,j,a,i) * t2(a,p,q,i) * wm_interm_31_triplet_pt3(b, j)
end do 
end do 
end do 
end do 

term(94) = term(94) * 8.0d+0 
term(95) = term(95) * 8.0d+0 
term(96) = term(96) * (-8.0d+0) 

do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do i = 1, nocc 
do k = 1, nocc 
term(97) = term(97) + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,k,a,j) * wm_interm_16_triplet_pt3(b, k, i, q)
end do 
end do 
end do 
end do 
end do 

term(97) = term(97) * 4.0d+0 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do k = 1, nocc 
term(98) = term(98) + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,k,a,i) * wm_interm_16_triplet_pt3(b, k, j, q)
term(99) = term(99) + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,k,a,j) * wm_interm_16_triplet_pt3(b, k, q, i)
term(100) = term(100) + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,k,a,i) * wm_interm_16_triplet_pt3(b, k, q, j)
end do 
end do 
end do 
end do 
end do 

term(98) = term(98) * (-8.0d+0) 
term(99) = term(99) * (-4.0d+0) 
term(100) = term(100) * 4.0d+0 

do j = 1, nocc 
do i = 1, nocc 
term(101) = term(101) + wm_interm_0_triplet_pt3(p, i, q, j) * wm_interm_2_triplet_pt3(j, i)
term(102) = term(102) + wm_interm_0_triplet_pt3(p, q, i, j) * wm_interm_2_triplet_pt3(j, i)
term(103) = term(103) + wm_interm_3_triplet_pt3(p, i, q, j) * wm_interm_5_triplet_pt3(i, j)
term(104) = term(104) + wm_interm_3_triplet_pt3(p, i, q, j) * wm_interm_6_triplet_pt3(i, j)
term(105) = term(105) + wm_interm_0_triplet_pt3(p, i, q, j) * wm_interm_8_triplet_pt3(j, i)
term(106) = term(106) + wm_interm_0_triplet_pt3(p, i, q, j) * wm_interm_9_triplet_pt3(j, i)
term(107) = term(107) + wm_interm_0_triplet_pt3(p, q, i, j) * wm_interm_8_triplet_pt3(j, i)
term(108) = term(108) + wm_interm_0_triplet_pt3(p, q, i, j) * wm_interm_9_triplet_pt3(j, i)
term(109) = term(109) + wm_interm_11_triplet_pt3(i, j) * wm_interm_3_triplet_pt3(p, i, q, j)
term(110) = term(110) + wm_interm_12_triplet_pt3(i, j) * wm_interm_3_triplet_pt3(p, i, q, j)
term(111) = term(111) + wm_interm_13_triplet_pt3(i, j) * wm_interm_3_triplet_pt3(p, i, q, j)
term(112) = term(112) + wm_interm_17_triplet_pt3(i, j) * wm_interm_18_triplet_pt3(p, i, q, j)
term(113) = term(113) + wm_interm_18_triplet_pt3(p, i, q, j) * wm_interm_19_triplet_pt3(i, j)
term(114) = term(114) + wm_interm_17_triplet_pt3(i, j) * wm_interm_20_triplet_pt3(p, i, q, j)
term(115) = term(115) + wm_interm_17_triplet_pt3(i, j) * wm_interm_21_triplet_pt3(p, q, i, j)
term(116) = term(116) + wm_interm_17_triplet_pt3(i, j) * wm_interm_21_triplet_pt3(p, i, q, j)
term(117) = term(117) + wm_interm_19_triplet_pt3(i, j) * wm_interm_20_triplet_pt3(p, i, q, j)
term(118) = term(118) + wm_interm_19_triplet_pt3(i, j) * wm_interm_21_triplet_pt3(p, q, i, j)
term(119) = term(119) + wm_interm_19_triplet_pt3(i, j) * wm_interm_21_triplet_pt3(p, i, q, j)
term(120) = term(120) + wm_interm_32_triplet_pt3(p, i, q, j) * wm_interm_9_triplet_pt3(j, i)
term(121) = term(121) + wm_interm_32_triplet_pt3(p, i, q, j) * wm_interm_8_triplet_pt3(j, i)
term(122) = term(122) + wm_interm_32_triplet_pt3(p, i, j, q) * wm_interm_9_triplet_pt3(j, i)
term(123) = term(123) + wm_interm_32_triplet_pt3(p, i, j, q) * wm_interm_8_triplet_pt3(j, i)
term(124) = term(124) + wm_interm_0_triplet_pt3(p, i, q, j) * wm_interm_41_triplet_pt3(j, i)
term(125) = term(125) + wm_interm_0_triplet_pt3(p, q, i, j) * wm_interm_41_triplet_pt3(j, i)
term(126) = term(126) + wm_interm_42_triplet_pt3(p, i, q, j) * wm_interm_5_triplet_pt3(i, j)
term(127) = term(127) + wm_interm_42_triplet_pt3(p, i, q, j) * wm_interm_6_triplet_pt3(i, j)
term(128) = term(128) + wm_interm_0_triplet_pt3(p, i, q, j) * wm_interm_43_triplet_pt3(j, i)
term(129) = term(129) + wm_interm_0_triplet_pt3(p, q, i, j) * wm_interm_43_triplet_pt3(j, i)
term(130) = term(130) + wm_interm_44_triplet_pt3(p, q, i, j) * wm_interm_5_triplet_pt3(i, j)
term(131) = term(131) + wm_interm_44_triplet_pt3(p, i, q, j) * wm_interm_5_triplet_pt3(i, j)
term(132) = term(132) + wm_interm_44_triplet_pt3(p, q, i, j) * wm_interm_6_triplet_pt3(i, j)
term(133) = term(133) + wm_interm_44_triplet_pt3(p, i, q, j) * wm_interm_6_triplet_pt3(i, j)
term(134) = term(134) + wm_interm_0_triplet_pt3(p, i, q, j) * wm_interm_46_triplet_pt3(j, i)
term(135) = term(135) + wm_interm_0_triplet_pt3(p, i, q, j) * wm_interm_47_triplet_pt3(j, i)
term(136) = term(136) + wm_interm_0_triplet_pt3(p, q, i, j) * wm_interm_46_triplet_pt3(j, i)
term(137) = term(137) + wm_interm_0_triplet_pt3(p, q, i, j) * wm_interm_47_triplet_pt3(j, i)
term(138) = term(138) + wm_interm_11_triplet_pt3(i, j) * wm_interm_42_triplet_pt3(p, i, q, j)
term(139) = term(139) + wm_interm_12_triplet_pt3(i, j) * wm_interm_42_triplet_pt3(p, i, q, j)
term(140) = term(140) + wm_interm_13_triplet_pt3(i, j) * wm_interm_42_triplet_pt3(p, i, q, j)
term(141) = term(141) + wm_interm_0_triplet_pt3(p, i, q, j) * wm_interm_48_triplet_pt3(j, i)
term(142) = term(142) + wm_interm_0_triplet_pt3(p, i, q, j) * wm_interm_49_triplet_pt3(j, i)
term(143) = term(143) + wm_interm_0_triplet_pt3(p, q, i, j) * wm_interm_48_triplet_pt3(j, i)
term(144) = term(144) + wm_interm_0_triplet_pt3(p, q, i, j) * wm_interm_49_triplet_pt3(j, i)
term(145) = term(145) + wm_interm_11_triplet_pt3(i, j) * wm_interm_44_triplet_pt3(p, q, i, j)
term(146) = term(146) + wm_interm_11_triplet_pt3(i, j) * wm_interm_44_triplet_pt3(p, i, q, j)
term(147) = term(147) + wm_interm_12_triplet_pt3(i, j) * wm_interm_44_triplet_pt3(p, q, i, j)
term(148) = term(148) + wm_interm_12_triplet_pt3(i, j) * wm_interm_44_triplet_pt3(p, i, q, j)
term(149) = term(149) + wm_interm_13_triplet_pt3(i, j) * wm_interm_44_triplet_pt3(p, q, i, j)
term(150) = term(150) + wm_interm_13_triplet_pt3(i, j) * wm_interm_44_triplet_pt3(p, i, q, j)
term(151) = term(151) + wm_interm_18_triplet_pt3(p, i, q, j) * wm_interm_50_triplet_pt3(i, j)
term(152) = term(152) + wm_interm_18_triplet_pt3(p, i, q, j) * wm_interm_51_triplet_pt3(i, j)
term(153) = term(153) + wm_interm_20_triplet_pt3(p, i, q, j) * wm_interm_50_triplet_pt3(i, j)
term(154) = term(154) + wm_interm_21_triplet_pt3(p, q, i, j) * wm_interm_50_triplet_pt3(i, j)
term(155) = term(155) + wm_interm_21_triplet_pt3(p, i, q, j) * wm_interm_50_triplet_pt3(i, j)
term(156) = term(156) + wm_interm_20_triplet_pt3(p, i, q, j) * wm_interm_51_triplet_pt3(i, j)
term(157) = term(157) + wm_interm_21_triplet_pt3(p, q, i, j) * wm_interm_51_triplet_pt3(i, j)
term(158) = term(158) + wm_interm_21_triplet_pt3(p, i, q, j) * wm_interm_51_triplet_pt3(i, j)
term(159) = term(159) + wm_interm_32_triplet_pt3(p, i, q, j) * wm_interm_43_triplet_pt3(j, i)
term(160) = term(160) + wm_interm_32_triplet_pt3(p, i, q, j) * wm_interm_41_triplet_pt3(j, i)
term(161) = term(161) + wm_interm_32_triplet_pt3(p, i, j, q) * wm_interm_43_triplet_pt3(j, i)
term(162) = term(162) + wm_interm_32_triplet_pt3(p, i, j, q) * wm_interm_41_triplet_pt3(j, i)
term(163) = term(163) + wm_interm_32_triplet_pt3(p, i, q, j) * wm_interm_48_triplet_pt3(j, i)
term(164) = term(164) + wm_interm_32_triplet_pt3(p, i, q, j) * wm_interm_49_triplet_pt3(j, i)
term(165) = term(165) + wm_interm_32_triplet_pt3(p, i, q, j) * wm_interm_46_triplet_pt3(j, i)
term(166) = term(166) + wm_interm_32_triplet_pt3(p, i, q, j) * wm_interm_47_triplet_pt3(j, i)
term(167) = term(167) + wm_interm_32_triplet_pt3(p, i, j, q) * wm_interm_48_triplet_pt3(j, i)
term(168) = term(168) + wm_interm_32_triplet_pt3(p, i, j, q) * wm_interm_49_triplet_pt3(j, i)
term(169) = term(169) + wm_interm_32_triplet_pt3(p, i, j, q) * wm_interm_46_triplet_pt3(j, i)
term(170) = term(170) + wm_interm_32_triplet_pt3(p, i, j, q) * wm_interm_47_triplet_pt3(j, i)
term(171) = term(171) + wm_interm_0_triplet_pt3(p, i, q, j) * wm_interm_81_triplet_pt3(j, i)
term(172) = term(172) + wm_interm_0_triplet_pt3(p, q, i, j) * wm_interm_81_triplet_pt3(j, i)
term(173) = term(173) + wm_interm_68_triplet_pt3(i, j) * wm_interm_83_triplet_pt3(p, i, j, q)
term(174) = term(174) + wm_interm_68_triplet_pt3(i, j) * wm_interm_83_triplet_pt3(p, i, q, j)
term(175) = term(175) + wm_interm_32_triplet_pt3(p, i, q, j) * wm_interm_81_triplet_pt3(j, i)
term(176) = term(176) + wm_interm_76_triplet_pt3(i, j) * wm_interm_87_triplet_pt3(p, i, q, j)
term(177) = term(177) + wm_interm_32_triplet_pt3(p, i, j, q) * wm_interm_81_triplet_pt3(j, i)
term(178) = term(178) + wm_interm_76_triplet_pt3(i, j) * wm_interm_87_triplet_pt3(p, i, j, q)
end do 
end do 

term(101) = term(101) * 4.0d+0 
term(102) = term(102) * (-8.0d+0) 
term(103) = term(103) * 2.0d+0 
term(104) = term(104) * (-6.0d+0) 
term(105) = term(105) * (-4.0d+0) 
term(106) = term(106) * 4.0d+0 
term(107) = term(107) * 8.0d+0 
term(108) = term(108) * (-8.0d+0) 
term(109) = term(109) * 4.0d+0 
term(110) = term(110) * 4.0d+0 
term(111) = term(111) * (-8.0d+0) 
term(112) = term(112) * 6.0d+0 
term(113) = term(113) * (-2.0d+0) 
term(114) = term(114) * 3.0d+0 
term(115) = term(115) * 3.0d+0 
term(116) = term(116) * (-6.0d+0) 
term(117) = -term(117) 
term(118) = -term(118) 
term(119) = term(119) * 2.0d+0 
term(120) = term(120) * 4.0d+0 
term(121) = term(121) * (-4.0d+0) 
term(122) = term(122) * (-8.0d+0) 
term(123) = term(123) * 8.0d+0 
term(124) = term(124) * 4.0d+0 
term(125) = term(125) * (-8.0d+0) 
term(126) = -term(126) 
term(127) = term(127) * 3.0d+0 
term(128) = term(128) * (-4.0d+0) 
term(129) = term(129) * 8.0d+0 
term(130) = -term(130) 
term(131) = term(131) * 2.0d+0 
term(132) = term(132) * 3.0d+0 
term(133) = term(133) * (-6.0d+0) 
term(134) = term(134) * (-4.0d+0) 
term(135) = term(135) * 4.0d+0 
term(136) = term(136) * 8.0d+0 
term(137) = term(137) * (-8.0d+0) 
term(138) = term(138) * (-2.0d+0) 
term(139) = term(139) * (-2.0d+0) 
term(140) = term(140) * 4.0d+0 
term(141) = term(141) * (-4.0d+0) 
term(142) = term(142) * 4.0d+0 
term(143) = term(143) * 8.0d+0 
term(144) = term(144) * (-8.0d+0) 
term(145) = term(145) * (-2.0d+0) 
term(146) = term(146) * 4.0d+0 
term(147) = term(147) * (-2.0d+0) 
term(148) = term(148) * 4.0d+0 
term(149) = term(149) * 4.0d+0 
term(150) = term(150) * (-8.0d+0) 
term(151) = term(151) * (-8.0d+0) 
term(152) = term(152) * 8.0d+0 
term(153) = term(153) * (-4.0d+0) 
term(154) = term(154) * (-4.0d+0) 
term(155) = term(155) * 8.0d+0 
term(156) = term(156) * 4.0d+0 
term(157) = term(157) * 4.0d+0 
term(158) = term(158) * (-8.0d+0) 
term(159) = term(159) * (-4.0d+0) 
term(160) = term(160) * 4.0d+0 
term(161) = term(161) * 8.0d+0 
term(162) = term(162) * (-8.0d+0) 
term(163) = term(163) * (-4.0d+0) 
term(164) = term(164) * 4.0d+0 
term(165) = term(165) * (-4.0d+0) 
term(166) = term(166) * 4.0d+0 
term(167) = term(167) * 8.0d+0 
term(168) = term(168) * (-8.0d+0) 
term(169) = term(169) * 8.0d+0 
term(170) = term(170) * (-8.0d+0) 
term(171) = term(171) * 2.0d+0 
term(172) = term(172) * (-4.0d+0) 
term(173) = term(173) * (-4.0d+0) 
term(174) = term(174) * 2.0d+0 
term(175) = term(175) * 2.0d+0 
term(176) = term(176) * 2.0d+0 
term(177) = term(177) * (-4.0d+0) 
term(178) = term(178) * (-4.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(179) = term(179) + r2m(vrdav_Rl, a,j,b,i) * t2(a,p,q,i) * wm_interm_27_triplet_pt3(b, j)
term(180) = term(180) + r2m(vrdav_Rl, a,j,b,i) * t2(a,p,q,i) * wm_interm_30_triplet_pt3(b, j)
term(181) = term(181) + r2m(vrdav_Rl, a,j,b,i) * t2(a,p,q,i) * wm_interm_31_triplet_pt3(b, j)
end do 
end do 
end do 
end do 

term(179) = term(179) * (-8.0d+0) 
term(180) = term(180) * (-8.0d+0) 
term(181) = term(181) * 8.0d+0 

term(182) = term(182) + wm_interm_14_triplet_pt3(p, q) * wm_interm_33_triplet_pt3
term(183) = term(183) + wm_interm_15_triplet_pt3(p, q) * wm_interm_33_triplet_pt3
term(184) = term(184) + wm_interm_14_triplet_pt3(p, q) * wm_interm_36_triplet_pt3
term(185) = term(185) + wm_interm_14_triplet_pt3(p, q) * wm_interm_37_triplet_pt3
term(186) = term(186) + wm_interm_15_triplet_pt3(p, q) * wm_interm_36_triplet_pt3
term(187) = term(187) + wm_interm_15_triplet_pt3(p, q) * wm_interm_37_triplet_pt3
term(188) = term(188) + wm_interm_14_triplet_pt3(p, q) * wm_interm_56_triplet_pt3
term(189) = term(189) + wm_interm_15_triplet_pt3(p, q) * wm_interm_56_triplet_pt3
term(190) = term(190) + wm_interm_14_triplet_pt3(p, q) * wm_interm_60_triplet_pt3
term(191) = term(191) + wm_interm_14_triplet_pt3(p, q) * wm_interm_61_triplet_pt3
term(192) = term(192) + wm_interm_15_triplet_pt3(p, q) * wm_interm_60_triplet_pt3
term(193) = term(193) + wm_interm_15_triplet_pt3(p, q) * wm_interm_61_triplet_pt3
term(194) = term(194) + wm_interm_14_triplet_pt3(p, q) * wm_interm_84_triplet_pt3
term(195) = term(195) + wm_interm_15_triplet_pt3(p, q) * wm_interm_84_triplet_pt3

term(182) = term(182) * (-4.0d+0) 
term(183) = term(183) * 8.0d+0 
term(184) = term(184) * 4.0d+0 
term(185) = term(185) * (-4.0d+0) 
term(186) = term(186) * (-8.0d+0) 
term(187) = term(187) * 8.0d+0 
term(188) = term(188) * (-8.0d+0) 
term(189) = term(189) * 16.0d+0 
term(190) = term(190) * 8.0d+0 
term(191) = term(191) * (-8.0d+0) 
term(192) = term(192) * (-16.0d+0) 
term(193) = term(193) * 16.0d+0 
term(194) = term(194) * (-4.0d+0) 
term(195) = term(195) * 8.0d+0 

do k = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
do j = 1, nocc 
do b = nocc + 1, nactive 
term(196) = term(196) + r2p(vrdav_Rl, p,q,a,i) * r2p(vrdav_Rr, b,j,a,k) * wm_interm_0_triplet_pt3(b, k, j, i)
term(197) = term(197) + r2p(vrdav_Rl, p,q,a,i) * r2p(vrdav_Rr, b,j,a,k) * wm_interm_0_triplet_pt3(b, j, k, i)
term(198) = term(198) + r2p(vrdav_Rl, b,j,a,k) * t2(a,p,q,i) * wm_interm_18_triplet_pt3(b, j, k, i)
term(199) = term(199) + r2p(vrdav_Rl, b,j,a,k) * t2(a,p,q,i) * wm_interm_20_triplet_pt3(b, j, k, i)
term(200) = term(200) + r2p(vrdav_Rl, b,j,a,k) * t2(a,p,q,i) * wm_interm_20_triplet_pt3(b, k, j, i)
term(201) = term(201) + r2m(vrdav_Rl, a,q,p,i) * r2p(vrdav_Rr, b,j,a,k) * wm_interm_0_triplet_pt3(b, k, j, i)
term(202) = term(202) + r2m(vrdav_Rl, a,q,p,i) * r2p(vrdav_Rr, b,j,a,k) * wm_interm_0_triplet_pt3(b, j, k, i)
term(203) = term(203) + r2m(vrdav_Rl, a,i,p,q) * r2p(vrdav_Rr, b,j,a,k) * wm_interm_0_triplet_pt3(b, k, j, i)
term(204) = term(204) + r2m(vrdav_Rl, a,i,p,q) * r2p(vrdav_Rr, b,j,a,k) * wm_interm_0_triplet_pt3(b, j, k, i)
end do 
end do 
end do 
end do 
end do 

term(196) = term(196) * 4.0d+0 
term(197) = term(197) * (-4.0d+0) 
term(198) = term(198) * 4.0d+0 
term(199) = term(199) * 4.0d+0 
term(200) = term(200) * (-4.0d+0) 
term(201) = term(201) * (-4.0d+0) 
term(202) = term(202) * 4.0d+0 
term(203) = term(203) * 4.0d+0 
term(204) = term(204) * (-4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(205) = term(205) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,k,j) * wm_interm_3_triplet_pt3(a, i, q, k)
term(206) = term(206) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,j,k) * wm_interm_3_triplet_pt3(a, i, q, k)
term(207) = term(207) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,k,q) * wm_interm_3_triplet_pt3(a, i, j, k)
term(208) = term(208) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_3_triplet_pt3(a, i, j, k)
term(209) = term(209) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,k,j) * wm_interm_3_triplet_pt3(a, i, q, k)
term(210) = term(210) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,i,k) * wm_interm_3_triplet_pt3(a, j, q, k)
term(211) = term(211) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,j,k) * wm_interm_3_triplet_pt3(a, i, q, k)
term(212) = term(212) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,k,q) * wm_interm_3_triplet_pt3(a, j, i, k)
term(213) = term(213) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,k,q) * wm_interm_3_triplet_pt3(a, i, j, k)
term(214) = term(214) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_3_triplet_pt3(a, j, i, k)
term(215) = term(215) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_3_triplet_pt3(a, i, j, k)
term(216) = term(216) + r2p(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,q,b,k) * wm_interm_16_triplet_pt3(a, k, i, j)
term(217) = term(217) + r2p(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,q,p,k) * wm_interm_16_triplet_pt3(a, k, i, j)
term(218) = term(218) + r2p(vrdav_Rl, a,i,b,j) * t2(b,p,k,q) * wm_interm_18_triplet_pt3(a, i, j, k)
term(219) = term(219) + r2p(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,j,p,k) * wm_interm_16_triplet_pt3(a, k, i, q)
term(220) = term(220) + r2p(vrdav_Rl, a,i,b,j) * t2(b,p,k,q) * wm_interm_20_triplet_pt3(a, i, j, k)
term(221) = term(221) + r2p(vrdav_Rl, a,i,b,j) * t2(b,p,k,q) * wm_interm_20_triplet_pt3(a, j, i, k)
term(222) = term(222) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,k,j) * wm_interm_42_triplet_pt3(a, q, i, k)
term(223) = term(223) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,j,k) * wm_interm_42_triplet_pt3(a, q, i, k)
term(224) = term(224) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,k,j) * wm_interm_42_triplet_pt3(a, i, q, k)
term(225) = term(225) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,k,q) * wm_interm_42_triplet_pt3(a, i, j, k)
term(226) = term(226) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_42_triplet_pt3(a, i, j, k)
term(227) = term(227) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,j,k) * wm_interm_42_triplet_pt3(a, i, q, k)
term(228) = term(228) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,k,j) * wm_interm_44_triplet_pt3(a, q, i, k)
term(229) = term(229) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,k,j) * wm_interm_44_triplet_pt3(a, i, q, k)
term(230) = term(230) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,k,q) * wm_interm_44_triplet_pt3(a, i, j, k)
term(231) = term(231) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_44_triplet_pt3(a, i, j, k)
term(232) = term(232) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,j,k) * wm_interm_44_triplet_pt3(a, i, q, k)
term(233) = term(233) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,k,j) * wm_interm_42_triplet_pt3(a, q, i, k)
term(234) = term(234) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,i,k) * wm_interm_42_triplet_pt3(a, q, j, k)
term(235) = term(235) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,j,k) * wm_interm_42_triplet_pt3(a, q, i, k)
term(236) = term(236) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,k,j) * wm_interm_42_triplet_pt3(a, i, q, k)
term(237) = term(237) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,k,q) * wm_interm_42_triplet_pt3(a, j, i, k)
term(238) = term(238) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,k,q) * wm_interm_42_triplet_pt3(a, i, j, k)
term(239) = term(239) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_42_triplet_pt3(a, j, i, k)
term(240) = term(240) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_42_triplet_pt3(a, i, j, k)
term(241) = term(241) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,i,k) * wm_interm_42_triplet_pt3(a, j, q, k)
term(242) = term(242) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,j,k) * wm_interm_42_triplet_pt3(a, i, q, k)
term(243) = term(243) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,k,j) * wm_interm_44_triplet_pt3(a, q, i, k)
term(244) = term(244) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,k,j) * wm_interm_44_triplet_pt3(a, i, q, k)
term(245) = term(245) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,k,q) * wm_interm_44_triplet_pt3(a, i, j, k)
term(246) = term(246) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,k,q) * wm_interm_44_triplet_pt3(a, j, i, k)
term(247) = term(247) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_44_triplet_pt3(a, i, j, k)
term(248) = term(248) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_44_triplet_pt3(a, j, i, k)
term(249) = term(249) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,i,k) * wm_interm_44_triplet_pt3(a, j, q, k)
term(250) = term(250) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,j,k) * wm_interm_44_triplet_pt3(a, i, q, k)
term(251) = term(251) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,q,p,k) * wm_interm_16_triplet_pt3(b, k, i, j)
term(252) = term(252) + r2m(vrdav_Rl, a,i,b,j) * t2(a,p,k,q) * wm_interm_18_triplet_pt3(b, i, j, k)
term(253) = term(253) + r2m(vrdav_Rl, a,i,b,j) * t2(b,p,k,q) * wm_interm_18_triplet_pt3(a, i, j, k)
term(254) = term(254) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,i,p,k) * wm_interm_16_triplet_pt3(a, k, j, q)
term(255) = term(255) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,j,p,k) * wm_interm_16_triplet_pt3(b, k, i, q)
term(256) = term(256) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,i,p,k) * wm_interm_16_triplet_pt3(b, k, j, q)
term(257) = term(257) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,i,p,k) * wm_interm_16_triplet_pt3(a, k, q, j)
term(258) = term(258) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,i,p,k) * wm_interm_16_triplet_pt3(b, k, q, j)
term(259) = term(259) + r2m(vrdav_Rl, a,i,b,j) * t2(a,p,k,q) * wm_interm_20_triplet_pt3(b, i, j, k)
term(260) = term(260) + r2m(vrdav_Rl, a,i,b,j) * t2(a,p,k,q) * wm_interm_20_triplet_pt3(b, j, i, k)
term(261) = term(261) + r2m(vrdav_Rl, a,i,b,j) * t2(b,p,k,q) * wm_interm_20_triplet_pt3(a, j, i, k)
term(262) = term(262) + r2m(vrdav_Rl, a,i,b,j) * t2(b,p,k,q) * wm_interm_20_triplet_pt3(a, i, j, k)
end do 
end do 
end do 
end do 
end do 

term(205) = term(205) * 8.0d+0 
term(206) = term(206) * (-8.0d+0) 
term(207) = term(207) * (-8.0d+0) 
term(208) = term(208) * 4.0d+0 
term(209) = term(209) * 8.0d+0 
term(210) = term(210) * 8.0d+0 
term(211) = term(211) * (-8.0d+0) 
term(212) = term(212) * 8.0d+0 
term(213) = term(213) * (-8.0d+0) 
term(214) = term(214) * (-4.0d+0) 
term(215) = term(215) * 4.0d+0 
term(216) = term(216) * (-2.0d+0) 
term(217) = term(217) * 2.0d+0 
term(218) = term(218) * (-8.0d+0) 
term(219) = term(219) * (-8.0d+0) 
term(220) = term(220) * (-8.0d+0) 
term(221) = term(221) * 8.0d+0 
term(222) = term(222) * (-4.0d+0) 
term(223) = term(223) * 4.0d+0 
term(224) = term(224) * 4.0d+0 
term(225) = term(225) * (-8.0d+0) 
term(226) = term(226) * 4.0d+0 
term(227) = term(227) * (-8.0d+0) 
term(228) = term(228) * 4.0d+0 
term(229) = term(229) * (-4.0d+0) 
term(230) = term(230) * 8.0d+0 
term(231) = term(231) * (-4.0d+0) 
term(232) = term(232) * 4.0d+0 
term(233) = term(233) * (-4.0d+0) 
term(234) = term(234) * (-4.0d+0) 
term(235) = term(235) * 4.0d+0 
term(236) = term(236) * 4.0d+0 
term(237) = term(237) * 8.0d+0 
term(238) = term(238) * (-8.0d+0) 
term(239) = term(239) * (-4.0d+0) 
term(240) = term(240) * 4.0d+0 
term(241) = term(241) * 8.0d+0 
term(242) = term(242) * (-8.0d+0) 
term(243) = term(243) * 4.0d+0 
term(244) = term(244) * (-4.0d+0) 
term(245) = term(245) * 8.0d+0 
term(246) = term(246) * (-8.0d+0) 
term(247) = term(247) * (-4.0d+0) 
term(248) = term(248) * 4.0d+0 
term(249) = term(249) * (-4.0d+0) 
term(250) = term(250) * 4.0d+0 
term(251) = term(251) * (-4.0d+0) 
term(252) = term(252) * 8.0d+0 
term(253) = term(253) * (-8.0d+0) 
term(254) = term(254) * 4.0d+0 
term(255) = term(255) * 4.0d+0 
term(256) = term(256) * (-8.0d+0) 
term(257) = term(257) * (-4.0d+0) 
term(258) = term(258) * 4.0d+0 
term(259) = term(259) * 8.0d+0 
term(260) = term(260) * (-8.0d+0) 
term(261) = term(261) * 8.0d+0 
term(262) = term(262) * (-8.0d+0) 

do a = nocc + 1, nactive 
term(263) = term(263) + wm_interm_22_triplet_pt3(a, q) * wm_interm_23_triplet_pt3(a, p)
term(264) = term(264) + wm_interm_22_triplet_pt3(a, q) * wm_interm_24_triplet_pt3(a, p)
term(265) = term(265) + wm_interm_22_triplet_pt3(a, q) * wm_interm_25_triplet_pt3(a, p)
term(266) = term(266) + wm_interm_22_triplet_pt3(a, q) * wm_interm_26_triplet_pt3(a, p)
term(267) = term(267) + wm_interm_14_triplet_pt3(a, q) * wm_interm_34_triplet_pt3(a, p)
term(268) = term(268) + wm_interm_15_triplet_pt3(a, q) * wm_interm_34_triplet_pt3(a, p)
term(269) = term(269) + wm_interm_14_triplet_pt3(a, q) * wm_interm_38_triplet_pt3(a, p)
term(270) = term(270) + wm_interm_14_triplet_pt3(a, q) * wm_interm_39_triplet_pt3(a, p)
term(271) = term(271) + wm_interm_15_triplet_pt3(a, q) * wm_interm_38_triplet_pt3(a, p)
term(272) = term(272) + wm_interm_15_triplet_pt3(a, q) * wm_interm_39_triplet_pt3(a, p)
term(273) = term(273) + wm_interm_23_triplet_pt3(a, p) * wm_interm_52_triplet_pt3(a, q)
term(274) = term(274) + wm_interm_23_triplet_pt3(a, p) * wm_interm_53_triplet_pt3(a, q)
term(275) = term(275) + wm_interm_24_triplet_pt3(a, p) * wm_interm_52_triplet_pt3(a, q)
term(276) = term(276) + wm_interm_24_triplet_pt3(a, p) * wm_interm_53_triplet_pt3(a, q)
term(277) = term(277) + wm_interm_25_triplet_pt3(a, p) * wm_interm_52_triplet_pt3(a, q)
term(278) = term(278) + wm_interm_26_triplet_pt3(a, p) * wm_interm_52_triplet_pt3(a, q)
term(279) = term(279) + wm_interm_25_triplet_pt3(a, p) * wm_interm_53_triplet_pt3(a, q)
term(280) = term(280) + wm_interm_26_triplet_pt3(a, p) * wm_interm_53_triplet_pt3(a, q)
term(281) = term(281) + wm_interm_14_triplet_pt3(a, q) * wm_interm_57_triplet_pt3(a, p)
term(282) = term(282) + wm_interm_14_triplet_pt3(a, q) * wm_interm_58_triplet_pt3(a, p)
term(283) = term(283) + wm_interm_15_triplet_pt3(a, q) * wm_interm_57_triplet_pt3(a, p)
term(284) = term(284) + wm_interm_15_triplet_pt3(a, q) * wm_interm_58_triplet_pt3(a, p)
term(285) = term(285) + wm_interm_14_triplet_pt3(a, q) * wm_interm_62_triplet_pt3(a, p)
term(286) = term(286) + wm_interm_14_triplet_pt3(a, q) * wm_interm_63_triplet_pt3(a, p)
term(287) = term(287) + wm_interm_14_triplet_pt3(a, q) * wm_interm_64_triplet_pt3(a, p)
term(288) = term(288) + wm_interm_14_triplet_pt3(a, q) * wm_interm_65_triplet_pt3(a, p)
term(289) = term(289) + wm_interm_15_triplet_pt3(a, q) * wm_interm_62_triplet_pt3(a, p)
term(290) = term(290) + wm_interm_15_triplet_pt3(a, q) * wm_interm_63_triplet_pt3(a, p)
term(291) = term(291) + wm_interm_15_triplet_pt3(a, q) * wm_interm_64_triplet_pt3(a, p)
term(292) = term(292) + wm_interm_15_triplet_pt3(a, q) * wm_interm_65_triplet_pt3(a, p)
term(293) = term(293) + wm_interm_14_triplet_pt3(a, q) * wm_interm_85_triplet_pt3(a, p)
term(294) = term(294) + wm_interm_75_triplet_pt3(p, a) * wm_interm_86_triplet_pt3(a, q)
term(295) = term(295) + wm_interm_15_triplet_pt3(a, q) * wm_interm_85_triplet_pt3(a, p)
end do 

term(263) = term(263) * 4.0d+0 
term(264) = term(264) * (-4.0d+0) 
term(265) = term(265) * (-8.0d+0) 
term(266) = term(266) * 8.0d+0 
term(267) = term(267) * 4.0d+0 
term(268) = term(268) * (-8.0d+0) 
term(269) = term(269) * 4.0d+0 
term(270) = term(270) * (-4.0d+0) 
term(271) = term(271) * (-8.0d+0) 
term(272) = term(272) * 8.0d+0 
term(273) = term(273) * (-4.0d+0) 
term(274) = term(274) * 4.0d+0 
term(275) = term(275) * 4.0d+0 
term(276) = term(276) * (-4.0d+0) 
term(277) = term(277) * 8.0d+0 
term(278) = term(278) * (-8.0d+0) 
term(279) = term(279) * (-8.0d+0) 
term(280) = term(280) * 8.0d+0 
term(281) = term(281) * (-4.0d+0) 
term(282) = term(282) * 4.0d+0 
term(283) = term(283) * 8.0d+0 
term(284) = term(284) * (-8.0d+0) 
term(285) = term(285) * (-4.0d+0) 
term(286) = term(286) * 4.0d+0 
term(287) = term(287) * (-4.0d+0) 
term(288) = term(288) * 4.0d+0 
term(289) = term(289) * 8.0d+0 
term(290) = term(290) * (-8.0d+0) 
term(291) = term(291) * 8.0d+0 
term(292) = term(292) * (-8.0d+0) 
term(293) = term(293) * 2.0d+0 
term(294) = term(294) * 2.0d+0 
term(295) = term(295) * (-4.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do j = 1, nocc 
term(296) = term(296) + r2p(vrdav_Rl, p,q,a,i) * r2m(vrdav_Rr, a,j,b,k) * wm_interm_0_triplet_pt3(b, j, k, i)
term(297) = term(297) + r2p(vrdav_Rl, p,q,a,i) * r2m(vrdav_Rr, a,j,b,k) * wm_interm_0_triplet_pt3(b, k, j, i)
end do 
end do 
end do 
end do 
end do 

term(296) = term(296) * 8.0d+0 
term(297) = term(297) * (-8.0d+0) 

do i = 1, nocc 
term(298) = term(298) + wm_interm_19_triplet_pt3(i, q) * wm_interm_27_triplet_pt3(p, i)
term(299) = term(299) + wm_interm_17_triplet_pt3(i, q) * wm_interm_27_triplet_pt3(p, i)
term(300) = term(300) + wm_interm_19_triplet_pt3(i, q) * wm_interm_30_triplet_pt3(p, i)
term(301) = term(301) + wm_interm_17_triplet_pt3(i, q) * wm_interm_30_triplet_pt3(p, i)
term(302) = term(302) + wm_interm_19_triplet_pt3(i, q) * wm_interm_31_triplet_pt3(p, i)
term(303) = term(303) + wm_interm_17_triplet_pt3(i, q) * wm_interm_31_triplet_pt3(p, i)
term(304) = term(304) + wm_interm_27_triplet_pt3(p, i) * wm_interm_50_triplet_pt3(i, q)
term(305) = term(305) + wm_interm_27_triplet_pt3(p, i) * wm_interm_51_triplet_pt3(i, q)
term(306) = term(306) + wm_interm_30_triplet_pt3(p, i) * wm_interm_50_triplet_pt3(i, q)
term(307) = term(307) + wm_interm_30_triplet_pt3(p, i) * wm_interm_51_triplet_pt3(i, q)
term(308) = term(308) + wm_interm_31_triplet_pt3(p, i) * wm_interm_50_triplet_pt3(i, q)
term(309) = term(309) + wm_interm_31_triplet_pt3(p, i) * wm_interm_51_triplet_pt3(i, q)
term(310) = term(310) + wm_interm_68_triplet_pt3(q, i) * wm_interm_82_triplet_pt3(p, i)
end do 

term(298) = term(298) * (-2.0d+0) 
term(299) = term(299) * 6.0d+0 
term(300) = term(300) * (-2.0d+0) 
term(301) = term(301) * 6.0d+0 
term(302) = term(302) * 2.0d+0 
term(303) = term(303) * (-6.0d+0) 
term(304) = term(304) * (-8.0d+0) 
term(305) = term(305) * 8.0d+0 
term(306) = term(306) * (-8.0d+0) 
term(307) = term(307) * 8.0d+0 
term(308) = term(308) * 8.0d+0 
term(309) = term(309) * (-8.0d+0) 
term(310) = term(310) * 2.0d+0 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(311) = term(311) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,k,i) * wm_interm_3_triplet_pt3(a, j, q, k)
term(312) = term(312) + r2p(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,q,b,k) * wm_interm_16_triplet_pt3(a, k, j, i)
term(313) = term(313) + r2p(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,q,p,k) * wm_interm_16_triplet_pt3(a, k, j, i)
term(314) = term(314) + r2p(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,j,p,k) * wm_interm_16_triplet_pt3(a, k, q, i)
term(315) = term(315) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,k,i) * wm_interm_42_triplet_pt3(a, q, j, k)
term(316) = term(316) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,k,i) * wm_interm_42_triplet_pt3(a, j, q, k)
term(317) = term(317) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,k,i) * wm_interm_44_triplet_pt3(a, q, j, k)
term(318) = term(318) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,k,i) * wm_interm_44_triplet_pt3(a, j, q, k)
term(319) = term(319) + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,q,b,k) * wm_interm_16_triplet_pt3(a, k, j, i)
term(320) = term(320) + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,q,a,k) * wm_interm_16_triplet_pt3(b, k, j, i)
term(321) = term(321) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,q,p,k) * wm_interm_16_triplet_pt3(a, k, j, i)
term(322) = term(322) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,q,p,k) * wm_interm_16_triplet_pt3(b, k, j, i)
term(323) = term(323) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,j,p,k) * wm_interm_16_triplet_pt3(a, k, q, i)
term(324) = term(324) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,j,p,k) * wm_interm_16_triplet_pt3(b, k, q, i)
end do 
end do 
end do 
end do 
end do 

term(311) = term(311) * (-8.0d+0) 
term(312) = term(312) * 6.0d+0 
term(313) = term(313) * (-6.0d+0) 
term(314) = term(314) * 8.0d+0 
term(315) = term(315) * 4.0d+0 
term(316) = term(316) * (-4.0d+0) 
term(317) = term(317) * (-4.0d+0) 
term(318) = term(318) * 4.0d+0 
term(319) = term(319) * 4.0d+0 
term(320) = term(320) * (-8.0d+0) 
term(321) = term(321) * (-4.0d+0) 
term(322) = term(322) * 8.0d+0 
term(323) = term(323) * 4.0d+0 
term(324) = term(324) * (-4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(325) = term(325) + r2p(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,k,p,q) * wm_interm_16_triplet_pt3(a, k, j, i)
term(326) = term(326) + r2p(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,k,b,j) * wm_interm_16_triplet_pt3(a, k, q, i)
term(327) = term(327) + r2p(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,k,p,j) * wm_interm_16_triplet_pt3(a, k, q, i)
term(328) = term(328) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,k,p,q) * wm_interm_16_triplet_pt3(a, k, j, i)
term(329) = term(329) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,k,p,q) * wm_interm_16_triplet_pt3(b, k, j, i)
term(330) = term(330) + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,k,b,i) * wm_interm_16_triplet_pt3(a, k, j, q)
term(331) = term(331) + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,k,b,j) * wm_interm_16_triplet_pt3(a, k, q, i)
term(332) = term(332) + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,k,b,i) * wm_interm_16_triplet_pt3(a, k, q, j)
term(333) = term(333) + r2m(vrdav_Rl, a,k,b,j) * t2(a,p,q,i) * wm_interm_18_triplet_pt3(b, j, k, i)
term(334) = term(334) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,k,p,i) * wm_interm_16_triplet_pt3(a, k, j, q)
term(335) = term(335) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,k,p,i) * wm_interm_16_triplet_pt3(b, k, j, q)
term(336) = term(336) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,k,p,j) * wm_interm_16_triplet_pt3(a, k, q, i)
term(337) = term(337) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,k,p,i) * wm_interm_16_triplet_pt3(a, k, q, j)
term(338) = term(338) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,k,p,i) * wm_interm_16_triplet_pt3(b, k, q, j)
term(339) = term(339) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,k,p,j) * wm_interm_16_triplet_pt3(b, k, q, i)
term(340) = term(340) + r2m(vrdav_Rl, a,k,b,j) * t2(a,p,q,i) * wm_interm_20_triplet_pt3(b, k, j, i)
term(341) = term(341) + r2m(vrdav_Rl, a,k,b,j) * t2(a,p,q,i) * wm_interm_20_triplet_pt3(b, j, k, i)
end do 
end do 
end do 
end do 
end do 

term(325) = term(325) * 6.0d+0 
term(326) = term(326) * 8.0d+0 
term(327) = term(327) * (-8.0d+0) 
term(328) = term(328) * 4.0d+0 
term(329) = term(329) * (-8.0d+0) 
term(330) = term(330) * 4.0d+0 
term(331) = term(331) * 4.0d+0 
term(332) = term(332) * (-4.0d+0) 
term(333) = term(333) * 4.0d+0 
term(334) = term(334) * (-4.0d+0) 
term(335) = term(335) * 8.0d+0 
term(336) = term(336) * (-4.0d+0) 
term(337) = term(337) * 4.0d+0 
term(338) = term(338) * (-4.0d+0) 
term(339) = term(339) * 4.0d+0 
term(340) = term(340) * (-4.0d+0) 
term(341) = term(341) * 4.0d+0 

do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(342) = term(342) + r2p(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,k,p,q) * wm_interm_16_triplet_pt3(a, k, i, j)
term(343) = term(343) + r2p(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,k,b,j) * wm_interm_16_triplet_pt3(a, k, i, q)
term(344) = term(344) + r2p(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,k,p,j) * wm_interm_16_triplet_pt3(a, k, i, q)
term(345) = term(345) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,k,p,q) * wm_interm_16_triplet_pt3(b, k, i, j)
term(346) = term(346) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,k,p,j) * wm_interm_16_triplet_pt3(b, k, i, q)
end do 
end do 
end do 
end do 
end do 

term(342) = term(342) * (-2.0d+0) 
term(343) = term(343) * (-8.0d+0) 
term(344) = term(344) * 8.0d+0 
term(345) = term(345) * 4.0d+0 
term(346) = term(346) * (-4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(347) = term(347) + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,q,a,k) * wm_interm_16_triplet_pt3(b, k, i, j)
end do 
end do 
end do 
end do 
end do 

term(347) = term(347) * 4.0d+0 


    calc_D_vo_wm_triplet_pt3 = zero
    do s = 0, 347
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
    double precision, dimension(:), intent(in) :: vrdav_Rl
    double precision, dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , k, i, j, a, b 
    real(F64), dimension(0:72) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(0) = term(0) + wm_interm_0_triplet_pt3(q, i, j, k) * wm_interm_66_triplet_pt3(p, k, j, i)
term(1) = term(1) + wm_interm_0_triplet_pt3(q, i, j, k) * wm_interm_69_triplet_pt3(p, k, j, i)
term(2) = term(2) + wm_interm_32_triplet_pt3(q, i, j, k) * wm_interm_73_triplet_pt3(p, j, k, i)
term(3) = term(3) + wm_interm_32_triplet_pt3(q, i, j, k) * wm_interm_73_triplet_pt3(p, k, j, i)
term(4) = term(4) + wm_interm_32_triplet_pt3(q, i, j, k) * wm_interm_79_triplet_pt3(p, j, k, i)
term(5) = term(5) + wm_interm_32_triplet_pt3(q, i, j, k) * wm_interm_77_triplet_pt3(p, k, j, i)
term(6) = term(6) + wm_interm_32_triplet_pt3(q, i, j, k) * wm_interm_77_triplet_pt3(p, j, k, i)
end do 
end do 
end do 

term(0) = term(0) * (-2.0d+0) 
term(1) = term(1) * (-4.0d+0) 
term(2) = term(2) * (-3.0d+0) 
term(4) = term(4) * 2.0d+0 
term(5) = term(5) * 2.0d+0 
term(6) = term(6) * (-4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(7) = term(7) + wm_interm_0_triplet_pt3(q, i, j, k) * wm_interm_66_triplet_pt3(p, k, i, j)
term(8) = term(8) + wm_interm_0_triplet_pt3(q, i, j, k) * wm_interm_69_triplet_pt3(p, k, i, j)
end do 
end do 
end do 

term(7) = term(7) * 2.0d+0 
term(8) = term(8) * 4.0d+0 

do j = 1, nocc 
do a = nocc + 1, nactive 
do k = 1, nocc 
do i = 1, nocc 
term(9) = term(9) + r2p(vrdav_Rr, p,i,a,j) * s2(a,q,k,j) * wm_interm_68_triplet_pt3(i, k)
end do 
end do 
end do 
end do 

term(9) = term(9) * 4.0d+0 

do a = nocc + 1, nactive 
term(10) = term(10) + wm_interm_23_triplet_pt3(p, a) * wm_interm_67_triplet_pt3(q, a)
term(11) = term(11) + wm_interm_24_triplet_pt3(p, a) * wm_interm_67_triplet_pt3(q, a)
term(12) = term(12) + wm_interm_25_triplet_pt3(p, a) * wm_interm_67_triplet_pt3(q, a)
term(13) = term(13) + wm_interm_26_triplet_pt3(p, a) * wm_interm_67_triplet_pt3(q, a)
term(14) = term(14) + wm_interm_23_triplet_pt3(a, p) * wm_interm_67_triplet_pt3(a, q)
term(15) = term(15) + wm_interm_24_triplet_pt3(a, p) * wm_interm_67_triplet_pt3(a, q)
term(16) = term(16) + wm_interm_25_triplet_pt3(a, p) * wm_interm_67_triplet_pt3(a, q)
term(17) = term(17) + wm_interm_26_triplet_pt3(a, p) * wm_interm_67_triplet_pt3(a, q)
term(18) = term(18) + wm_interm_28_triplet_pt3(q, a) * wm_interm_75_triplet_pt3(p, a)
term(19) = term(19) + wm_interm_29_triplet_pt3(q, a) * wm_interm_75_triplet_pt3(p, a)
term(20) = term(20) + wm_interm_28_triplet_pt3(a, q) * wm_interm_75_triplet_pt3(a, p)
term(21) = term(21) + wm_interm_29_triplet_pt3(a, q) * wm_interm_75_triplet_pt3(a, p)
term(22) = term(22) + wm_interm_54_triplet_pt3(q, a) * wm_interm_75_triplet_pt3(p, a)
term(23) = term(23) + wm_interm_55_triplet_pt3(q, a) * wm_interm_75_triplet_pt3(p, a)
term(24) = term(24) + wm_interm_54_triplet_pt3(a, q) * wm_interm_75_triplet_pt3(a, p)
term(25) = term(25) + wm_interm_55_triplet_pt3(a, q) * wm_interm_75_triplet_pt3(a, p)
end do 

term(10) = term(10) * (-2.0d+0) 
term(11) = term(11) * 2.0d+0 
term(12) = term(12) * 4.0d+0 
term(13) = term(13) * (-4.0d+0) 
term(14) = term(14) * (-2.0d+0) 
term(15) = term(15) * 2.0d+0 
term(16) = term(16) * 4.0d+0 
term(17) = term(17) * (-4.0d+0) 
term(18) = term(18) * 3.0d+0 
term(19) = -term(19) 
term(20) = term(20) * (-3.0d+0) 
term(22) = term(22) * 4.0d+0 
term(23) = term(23) * (-4.0d+0) 
term(24) = term(24) * 4.0d+0 
term(25) = term(25) * (-4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(26) = term(26) + r2m(vrdav_Rr, a,j,p,i) * s2(a,q,k,j) * wm_interm_68_triplet_pt3(i, k)
term(27) = term(27) + r2m(vrdav_Rr, a,i,p,j) * s2(a,q,k,i) * wm_interm_68_triplet_pt3(j, k)
term(28) = term(28) + r2m(vrdav_Rl, a,k,p,j) * t2(a,q,j,i) * wm_interm_76_triplet_pt3(k, i)
term(29) = term(29) + r2m(vrdav_Rl, a,k,p,j) * t2(a,q,k,i) * wm_interm_76_triplet_pt3(j, i)
term(30) = term(30) + r2m(vrdav_Rl, a,i,p,j) * t2(a,q,k,i) * wm_interm_76_triplet_pt3(j, k)
end do 
end do 
end do 
end do 

term(26) = term(26) * 2.0d+0 
term(27) = term(27) * 2.0d+0 
term(28) = term(28) * 2.0d+0 
term(29) = term(29) * (-4.0d+0) 
term(30) = term(30) * 4.0d+0 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(31) = term(31) + r2m(vrdav_Rr, a,j,p,i) * s2(a,q,i,k) * wm_interm_68_triplet_pt3(j, k)
term(32) = term(32) + r2m(vrdav_Rr, a,j,p,i) * s2(a,q,j,k) * wm_interm_68_triplet_pt3(i, k)
term(33) = term(33) + r2m(vrdav_Rl, a,j,p,k) * t2(a,q,k,i) * wm_interm_76_triplet_pt3(j, i)
end do 
end do 
end do 
end do 

term(31) = term(31) * 2.0d+0 
term(32) = term(32) * (-4.0d+0) 
term(33) = term(33) * 2.0d+0 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(34) = term(34) + r2m(vrdav_Rr, a,i,p,j) * s2(a,q,j,k) * wm_interm_68_triplet_pt3(i, k)
end do 
end do 
end do 
end do 

term(34) = term(34) * 2.0d+0 

do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(35) = term(35) + r2m(vrdav_Rr, a,i,p,j) * s2(a,q,k,j) * wm_interm_68_triplet_pt3(i, k)
end do 
end do 
end do 
end do 

term(35) = term(35) * (-4.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
do j = 1, nocc 
term(36) = term(36) + r2p(vrdav_Rl, p,j,a,k) * t2(a,q,k,i) * wm_interm_76_triplet_pt3(j, i)
end do 
end do 
end do 
end do 

term(36) = term(36) * (-4.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
do k = 1, nocc 
do j = 1, nocc 
term(37) = term(37) + r2p(vrdav_Rl, p,j,a,i) * t2(a,q,k,i) * wm_interm_76_triplet_pt3(j, k)
end do 
end do 
end do 
end do 

term(37) = term(37) * 4.0d+0 

do i = 1, nocc 
term(38) = term(38) + wm_interm_14_triplet_pt3(q, i) * wm_interm_74_triplet_pt3(p, i)
term(39) = term(39) + wm_interm_15_triplet_pt3(q, i) * wm_interm_74_triplet_pt3(p, i)
term(40) = term(40) + wm_interm_14_triplet_pt3(q, i) * wm_interm_80_triplet_pt3(p, i)
term(41) = term(41) + wm_interm_14_triplet_pt3(q, i) * wm_interm_78_triplet_pt3(p, i)
term(42) = term(42) + wm_interm_15_triplet_pt3(q, i) * wm_interm_80_triplet_pt3(p, i)
term(43) = term(43) + wm_interm_15_triplet_pt3(q, i) * wm_interm_78_triplet_pt3(p, i)
end do 

term(38) = term(38) * (-4.0d+0) 
term(39) = term(39) * 8.0d+0 
term(40) = term(40) * 4.0d+0 
term(41) = term(41) * (-4.0d+0) 
term(42) = term(42) * (-8.0d+0) 
term(43) = term(43) * 8.0d+0 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(44) = term(44) + r2m(vrdav_Rl, a,j,p,i) * t2(a,q,k,i) * wm_interm_76_triplet_pt3(j, k)
end do 
end do 
end do 
end do 

term(44) = term(44) * (-4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(45) = term(45) + r2p(vrdav_Rr, p,i,a,j) * s2(a,q,j,k) * wm_interm_68_triplet_pt3(i, k)
end do 
end do 
end do 
end do 

term(45) = term(45) * (-4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(46) = term(46) + s2(a,p,i,j) * t1(q,i) * wm_interm_70_triplet_pt3(a, j)
term(47) = term(47) + s2(a,p,i,j) * t1(q,i) * wm_interm_71_triplet_pt3(a, j)
term(48) = term(48) + s2(a,p,i,j) * t1(q,i) * wm_interm_72_triplet_pt3(a, j)
term(49) = term(49) + s1(p,i) * t2(a,q,i,j) * wm_interm_74_triplet_pt3(a, j)
term(50) = term(50) + r2m(vrdav_Rl, a,i,q,j) * r1(vrdav_Rr, p,i) * wm_interm_14_triplet_pt3(a, j)
term(51) = term(51) + r2m(vrdav_Rl, a,i,q,j) * r1(vrdav_Rr, p,i) * wm_interm_15_triplet_pt3(a, j)
term(52) = term(52) + s1(p,i) * t2(a,q,i,j) * wm_interm_80_triplet_pt3(a, j)
term(53) = term(53) + s1(p,i) * t2(a,q,i,j) * wm_interm_78_triplet_pt3(a, j)
end do 
end do 
end do 

term(46) = term(46) * (-4.0d+0) 
term(47) = term(47) * 4.0d+0 
term(48) = term(48) * (-4.0d+0) 
term(49) = term(49) * (-4.0d+0) 
term(50) = term(50) * 4.0d+0 
term(51) = term(51) * (-8.0d+0) 
term(52) = term(52) * 4.0d+0 
term(53) = term(53) * (-4.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(54) = term(54) + s2(a,p,j,i) * t1(q,i) * wm_interm_70_triplet_pt3(a, j)
term(55) = term(55) + s2(a,p,j,i) * t1(q,i) * wm_interm_71_triplet_pt3(a, j)
term(56) = term(56) + s2(a,p,j,i) * t1(q,i) * wm_interm_72_triplet_pt3(a, j)
term(57) = term(57) + s1(p,i) * t2(a,q,j,i) * wm_interm_74_triplet_pt3(a, j)
term(58) = term(58) + r2m(vrdav_Rl, a,j,q,i) * r1(vrdav_Rr, p,i) * wm_interm_14_triplet_pt3(a, j)
term(59) = term(59) + r2m(vrdav_Rl, a,j,q,i) * r1(vrdav_Rr, p,i) * wm_interm_15_triplet_pt3(a, j)
term(60) = term(60) + s1(p,i) * t2(a,q,j,i) * wm_interm_80_triplet_pt3(a, j)
term(61) = term(61) + s1(p,i) * t2(a,q,j,i) * wm_interm_78_triplet_pt3(a, j)
end do 
end do 
end do 

term(54) = term(54) * 8.0d+0 
term(55) = term(55) * (-8.0d+0) 
term(56) = term(56) * 8.0d+0 
term(57) = term(57) * 8.0d+0 
term(58) = term(58) * (-4.0d+0) 
term(59) = term(59) * 8.0d+0 
term(60) = term(60) * (-8.0d+0) 
term(61) = term(61) * 8.0d+0 

do i = 1, nocc 
do a = nocc + 1, nactive 
do j = 1, nocc 
term(62) = term(62) + r2p(vrdav_Rl, q,j,a,i) * r1(vrdav_Rr, p,i) * wm_interm_14_triplet_pt3(a, j)
term(63) = term(63) + r2p(vrdav_Rl, q,j,a,i) * r1(vrdav_Rr, p,i) * wm_interm_15_triplet_pt3(a, j)
end do 
end do 
end do 

term(62) = term(62) * 4.0d+0 
term(63) = term(63) * (-8.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
term(64) = term(64) + r1(vrdav_Rl, p,i) * t1(q,j) * wm_interm_5_triplet_pt3(i, j)
term(65) = term(65) + r1(vrdav_Rl, p,i) * t1(q,j) * wm_interm_6_triplet_pt3(i, j)
term(66) = term(66) + r1(vrdav_Rl, p,i) * t1(q,j) * wm_interm_11_triplet_pt3(i, j)
term(67) = term(67) + r1(vrdav_Rl, p,i) * t1(q,j) * wm_interm_12_triplet_pt3(i, j)
term(68) = term(68) + r1(vrdav_Rl, p,i) * t1(q,j) * wm_interm_13_triplet_pt3(i, j)
term(69) = term(69) + r1(vrdav_Rr, p,i) * s1(q,j) * wm_interm_17_triplet_pt3(i, j)
term(70) = term(70) + r1(vrdav_Rr, p,i) * s1(q,j) * wm_interm_19_triplet_pt3(i, j)
term(71) = term(71) + r1(vrdav_Rr, p,i) * s1(q,j) * wm_interm_50_triplet_pt3(i, j)
term(72) = term(72) + r1(vrdav_Rr, p,i) * s1(q,j) * wm_interm_51_triplet_pt3(i, j)
end do 
end do 

term(65) = term(65) * (-3.0d+0) 
term(66) = term(66) * 2.0d+0 
term(67) = term(67) * 2.0d+0 
term(68) = term(68) * (-4.0d+0) 
term(69) = term(69) * (-3.0d+0) 
term(71) = term(71) * 4.0d+0 
term(72) = term(72) * (-4.0d+0) 


    calc_D_vv_wm_triplet_pt3 = zero
    do s = 0, 72
    calc_D_vv_wm_triplet_pt3 = calc_D_vv_wm_triplet_pt3 + term(s)
    end do

    end function calc_D_vv_wm_triplet_pt3
    

    
end module density_exc_exc_functions_triplet_pt3
