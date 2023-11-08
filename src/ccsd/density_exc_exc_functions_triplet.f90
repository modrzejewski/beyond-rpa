module density_exc_exc_functions_triplet

    use ccsd_transformed_integrals
    use cc3_intermediates
    use arithmetic
    use s_gen
    use basis
    use eom_vectors
    
    implicit none
    !
    ! File generated automatically on 2016-08-12 16:30:00
    !
!         real(F64), dimension(:, :), allocatable :: wm_interm_0_triplet 
! real(F64), dimension(:, :), allocatable :: wm_interm_1_triplet 
! real(F64), dimension(:, :), allocatable :: wm_interm_2_triplet 
! real(F64), dimension(:, :), allocatable :: wm_interm_3_triplet 
! real(F64), dimension(:, :), allocatable :: wm_interm_4_triplet 
! real(F64), dimension(:, :), allocatable :: wm_interm_5_triplet 
! real(F64), dimension(:, :), allocatable :: wm_interm_6_triplet 
! real(F64), dimension(:, :), allocatable :: wm_interm_7_triplet 
! real(F64), dimension(:, :), allocatable :: wm_interm_8_triplet 
! real(F64), dimension(:, :), allocatable :: wm_interm_9_triplet 
! real(F64), dimension(:, :), allocatable :: wm_interm_10_triplet 
! real(F64), dimension(:, :), allocatable :: wm_interm_11_triplet 
! real(F64), dimension(:, :), allocatable :: wm_interm_12_triplet 
! real(F64) :: wm_interm_13_triplet 
! real(F64), dimension(:, :), allocatable :: wm_interm_14_triplet 
! real(F64), dimension(:, :), allocatable :: wm_interm_15_triplet 
! real(F64), dimension(:, :), allocatable :: wm_interm_16_triplet 
! real(F64), dimension(:, :), allocatable :: wm_interm_17_triplet 
! real(F64), dimension(:, :), allocatable :: wm_interm_18_triplet 
! real(F64), dimension(:, :), allocatable :: wm_interm_19_triplet 
! real(F64), dimension(:, :), allocatable :: wm_interm_20_triplet 
! real(F64), dimension(:, :), allocatable :: wm_interm_21_triplet 
! real(F64), dimension(:, :), allocatable :: wm_interm_22_triplet 
! real(F64), dimension(:, :), allocatable :: wm_interm_23_triplet 
! real(F64), dimension(:, :), allocatable :: wm_interm_24_triplet 
! real(F64), dimension(:, :), allocatable :: wm_interm_25_triplet 
! real(F64), dimension(:, :), allocatable :: wm_interm_26_triplet 
! real(F64), dimension(:, :), allocatable :: wm_interm_27_triplet 
! real(F64) :: wm_interm_28_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wm_interm_29_triplet 
! real(F64), dimension(:, :), allocatable :: wm_interm_30_triplet 
! real(F64), dimension(:, :), allocatable :: wm_interm_31_triplet 
! real(F64) :: wm_interm_32_triplet 
! real(F64) :: wm_interm_33_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wm_interm_34_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wm_interm_35_triplet 
! real(F64), dimension(:, :), allocatable :: wm_interm_36_triplet 
! real(F64), dimension(:, :), allocatable :: wm_interm_37_triplet 
! real(F64), dimension(:, :), allocatable :: wm_interm_38_triplet 
! real(F64), dimension(:, :), allocatable :: wm_interm_39_triplet 
! real(F64), dimension(:, :), allocatable :: wm_interm_40_triplet 
! real(F64), dimension(:, :), allocatable :: wm_interm_41_triplet 
! real(F64) :: wm_interm_42_triplet 
! real(F64), dimension(:, :), allocatable :: wm_interm_43_triplet 
! real(F64), dimension(:, :), allocatable :: wm_interm_44_triplet 
! real(F64), dimension(:, :), allocatable :: wm_interm_45_triplet 
! real(F64), dimension(:, :), allocatable :: wm_interm_46_triplet 
! real(F64) :: wm_interm_47_triplet 
! real(F64) :: wm_interm_48_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wm_interm_49_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wm_interm_50_triplet 
! real(F64), dimension(:, :), allocatable :: wm_interm_51_triplet 
! real(F64), dimension(:, :), allocatable :: wm_interm_52_triplet 
! real(F64), dimension(:, :), allocatable :: wm_interm_53_triplet 
! real(F64), dimension(:, :), allocatable :: wm_interm_54_triplet 
! real(F64), dimension(:, :), allocatable :: wm_interm_55_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wm_interm_56_triplet 
! real(F64), dimension(:, :), allocatable :: wm_interm_57_triplet 
! real(F64), dimension(:, :), allocatable :: wm_interm_58_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wm_interm_59_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wm_interm_60_triplet 
! real(F64), dimension(:, :), allocatable :: wm_interm_61_triplet 
! real(F64), dimension(:, :), allocatable :: wm_interm_62_triplet 
! real(F64), dimension(:, :), allocatable :: wm_interm_63_triplet 
! real(F64), dimension(:, :), allocatable :: wm_interm_64_triplet 
! real(F64), dimension(:, :), allocatable :: wm_interm_65_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wm_interm_66_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wm_interm_67_triplet 
! real(F64), dimension(:, :), allocatable :: wm_interm_68_triplet 
! real(F64), dimension(:, :), allocatable :: wm_interm_69_triplet 
! real(F64), dimension(:, :), allocatable :: wm_interm_70_triplet 
! real(F64), dimension(:, :), allocatable :: wm_interm_71_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wm_interm_72_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wm_interm_73_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wm_interm_74_triplet 
! real(F64), dimension(:, :), allocatable :: wm_interm_75_triplet 
! real(F64), dimension(:, :), allocatable :: wm_interm_76_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wm_interm_77_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wm_interm_78_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wm_interm_79_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wm_interm_80_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wm_interm_81_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wm_interm_82_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wm_interm_83_triplet 
! real(F64), dimension(:, :), allocatable :: wm_interm_84_triplet 
! real(F64), dimension(:, :), allocatable :: wm_interm_85_triplet 
! real(F64), dimension(:, :), allocatable :: wm_interm_86_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wm_interm_87_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wm_interm_88_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wm_interm_89_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wm_interm_90_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wm_interm_91_triplet 
! real(F64), dimension(:, :), allocatable :: wm_interm_92_triplet 
! real(F64), dimension(:, :), allocatable :: wm_interm_93_triplet 
! real(F64), dimension(:, :), allocatable :: wm_interm_94_triplet 
! real(F64), dimension(:, :), allocatable :: wm_interm_95_triplet 
! real(F64), dimension(:, :), allocatable :: wm_interm_96_triplet 
    ! real(F64), dimension(:, :), allocatable :: wm_interm_97_triplet


        real(F64), dimension(:, :), allocatable :: wm_interm_0_triplet 
real(F64), dimension(:, :), allocatable :: wm_interm_1_triplet 
real(F64), dimension(:, :), allocatable :: wm_interm_2_triplet 
real(F64), dimension(:, :), allocatable :: wm_interm_3_triplet 
real(F64), dimension(:, :), allocatable :: wm_interm_4_triplet 
real(F64), dimension(:, :), allocatable :: wm_interm_5_triplet 
real(F64), dimension(:, :), allocatable :: wm_interm_6_triplet 
real(F64), dimension(:, :), allocatable :: wm_interm_7_triplet 
real(F64), dimension(:, :), allocatable :: wm_interm_8_triplet 
real(F64), dimension(:, :), allocatable :: wm_interm_9_triplet 
real(F64), dimension(:, :), allocatable :: wm_interm_10_triplet 
real(F64), dimension(:, :), allocatable :: wm_interm_11_triplet 
real(F64), dimension(:, :), allocatable :: wm_interm_12_triplet 
real(F64) :: wm_interm_13_triplet 
real(F64), dimension(:, :), allocatable :: wm_interm_14_triplet 
real(F64), dimension(:, :), allocatable :: wm_interm_15_triplet 
real(F64), dimension(:, :), allocatable :: wm_interm_16_triplet 
real(F64), dimension(:, :), allocatable :: wm_interm_17_triplet 
real(F64), dimension(:, :), allocatable :: wm_interm_18_triplet 
real(F64), dimension(:, :), allocatable :: wm_interm_19_triplet 
real(F64), dimension(:, :), allocatable :: wm_interm_20_triplet 
real(F64), dimension(:, :), allocatable :: wm_interm_21_triplet 
real(F64), dimension(:, :), allocatable :: wm_interm_22_triplet 
real(F64), dimension(:, :), allocatable :: wm_interm_23_triplet 
real(F64), dimension(:, :), allocatable :: wm_interm_24_triplet 
real(F64), dimension(:, :), allocatable :: wm_interm_25_triplet 
real(F64), dimension(:, :), allocatable :: wm_interm_26_triplet 
real(F64), dimension(:, :), allocatable :: wm_interm_27_triplet 
real(F64) :: wm_interm_28_triplet 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_29_triplet 
real(F64), dimension(:, :), allocatable :: wm_interm_30_triplet 
real(F64), dimension(:, :), allocatable :: wm_interm_31_triplet 
real(F64) :: wm_interm_32_triplet 
real(F64) :: wm_interm_33_triplet 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_34_triplet 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_35_triplet 
real(F64), dimension(:, :), allocatable :: wm_interm_36_triplet 
real(F64), dimension(:, :), allocatable :: wm_interm_37_triplet 
real(F64), dimension(:, :), allocatable :: wm_interm_38_triplet 
real(F64), dimension(:, :), allocatable :: wm_interm_39_triplet 
real(F64), dimension(:, :), allocatable :: wm_interm_40_triplet 
real(F64), dimension(:, :), allocatable :: wm_interm_41_triplet 
real(F64) :: wm_interm_42_triplet 
real(F64), dimension(:, :), allocatable :: wm_interm_43_triplet 
real(F64), dimension(:, :), allocatable :: wm_interm_44_triplet 
real(F64), dimension(:, :), allocatable :: wm_interm_45_triplet 
real(F64), dimension(:, :), allocatable :: wm_interm_46_triplet 
real(F64) :: wm_interm_47_triplet 
real(F64) :: wm_interm_48_triplet 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_49_triplet 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_50_triplet 
real(F64), dimension(:, :), allocatable :: wm_interm_51_triplet 
real(F64), dimension(:, :), allocatable :: wm_interm_52_triplet 
real(F64), dimension(:, :), allocatable :: wm_interm_53_triplet 
real(F64), dimension(:, :), allocatable :: wm_interm_54_triplet 
real(F64), dimension(:, :), allocatable :: wm_interm_55_triplet 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_56_triplet 
real(F64), dimension(:, :), allocatable :: wm_interm_57_triplet 
real(F64), dimension(:, :), allocatable :: wm_interm_58_triplet 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_59_triplet 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_60_triplet 
real(F64), dimension(:, :), allocatable :: wm_interm_61_triplet 
real(F64), dimension(:, :), allocatable :: wm_interm_62_triplet 
real(F64), dimension(:, :), allocatable :: wm_interm_63_triplet 
real(F64), dimension(:, :), allocatable :: wm_interm_64_triplet 
real(F64), dimension(:, :), allocatable :: wm_interm_65_triplet 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_66_triplet 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_67_triplet 
real(F64), dimension(:, :), allocatable :: wm_interm_68_triplet 
real(F64), dimension(:, :), allocatable :: wm_interm_69_triplet 
real(F64), dimension(:, :), allocatable :: wm_interm_70_triplet 
real(F64), dimension(:, :), allocatable :: wm_interm_71_triplet 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_72_triplet 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_73_triplet 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_74_triplet 
real(F64), dimension(:, :), allocatable :: wm_interm_75_triplet 
real(F64), dimension(:, :), allocatable :: wm_interm_76_triplet 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_77_triplet 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_78_triplet 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_79_triplet 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_80_triplet 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_81_triplet 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_82_triplet 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_83_triplet 
real(F64), dimension(:, :), allocatable :: wm_interm_84_triplet 
real(F64), dimension(:, :), allocatable :: wm_interm_85_triplet 
real(F64), dimension(:, :), allocatable :: wm_interm_86_triplet 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_87_triplet 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_88_triplet 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_89_triplet 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_90_triplet 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_91_triplet 
real(F64), dimension(:, :), allocatable :: wm_interm_92_triplet 
real(F64), dimension(:, :), allocatable :: wm_interm_93_triplet 
real(F64), dimension(:, :), allocatable :: wm_interm_94_triplet 
real(F64), dimension(:, :), allocatable :: wm_interm_95_triplet 
real(F64), dimension(:, :), allocatable :: wm_interm_96_triplet 
real(F64), dimension(:, :), allocatable :: wm_interm_97_triplet 

real(F64), dimension(:, :), allocatable :: wmo_interm_0_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_1_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_2_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_3_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_4_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_5_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_6_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_7_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_8_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_9_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_10_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_11_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_12_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_13_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_14_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_15_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_16_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_17_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_18_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_19_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_20_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_21_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_22_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_23_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_24_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_25_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_26_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_27_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_28_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_29_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_30_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_31_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_32_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_33_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_34_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_35_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_36_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_37_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_38_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_39_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_40_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_41_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_42_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_43_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_44_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_45_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_46_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_47_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_48_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_49_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_50_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_51_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_52_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_53_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_54_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_55_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_56_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_57_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_58_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_59_triplet 
real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_60_triplet 
real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_61_triplet 
real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_62_triplet 
real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_63_triplet 
real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_64_triplet 
real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_65_triplet 
real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_66_triplet 
real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_67_triplet 
real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_68_triplet 
real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_69_triplet 
real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_70_triplet 
real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_71_triplet 
real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_72_triplet 
real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_73_triplet 
real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_74_triplet 
real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_75_triplet 
real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_76_triplet 
real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_77_triplet 
real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_78_triplet 
real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_79_triplet 
real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_80_triplet 
real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_81_triplet 


contains

        subroutine overlap_ccsd_triplet_init(nocc, nactive)
            integer, intent(in) :: nocc
            integer, intent(in) :: nactive

    allocate(wmo_interm_0_triplet(1: nocc, 1: nocc))
allocate(wmo_interm_1_triplet(1: nocc, 1: nocc))
allocate(wmo_interm_2_triplet(1: nocc, 1: nocc))
allocate(wmo_interm_3_triplet(1: nocc, 1: nocc))
allocate(wmo_interm_4_triplet(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_5_triplet(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_6_triplet(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_7_triplet(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_8_triplet(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_9_triplet(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_10_triplet(1: nocc, 1: nocc))
allocate(wmo_interm_11_triplet(nocc+1: nactive, nocc+1: nactive))
allocate(wmo_interm_12_triplet(nocc+1: nactive, nocc+1: nactive))
allocate(wmo_interm_13_triplet(1: nocc, 1: nocc))
allocate(wmo_interm_14_triplet(1: nocc, 1: nocc))
allocate(wmo_interm_15_triplet(nocc+1: nactive, nocc+1: nactive))
allocate(wmo_interm_16_triplet(nocc+1: nactive, nocc+1: nactive))
allocate(wmo_interm_17_triplet(1: nocc, 1: nocc))
allocate(wmo_interm_18_triplet(nocc+1: nactive, nocc+1: nactive))
allocate(wmo_interm_19_triplet(1: nocc, 1: nocc))
allocate(wmo_interm_20_triplet(nocc+1: nactive, nocc+1: nactive))
allocate(wmo_interm_21_triplet(1: nocc, 1: nocc))
allocate(wmo_interm_22_triplet(1: nocc, 1: nocc))
allocate(wmo_interm_23_triplet(nocc+1: nactive, nocc+1: nactive))
allocate(wmo_interm_24_triplet(nocc+1: nactive, nocc+1: nactive))
allocate(wmo_interm_25_triplet(1: nocc, 1: nocc))
allocate(wmo_interm_26_triplet(1: nocc, 1: nocc))
allocate(wmo_interm_27_triplet(nocc+1: nactive, nocc+1: nactive))
allocate(wmo_interm_28_triplet(nocc+1: nactive, nocc+1: nactive))
allocate(wmo_interm_29_triplet(nocc+1: nactive, nocc+1: nactive))
allocate(wmo_interm_30_triplet(nocc+1: nactive, nocc+1: nactive))
allocate(wmo_interm_31_triplet(nocc+1: nactive, nocc+1: nactive))
allocate(wmo_interm_32_triplet(1: nocc, 1: nocc))
allocate(wmo_interm_33_triplet(1: nocc, 1: nocc))
allocate(wmo_interm_34_triplet(nocc+1: nactive, nocc+1: nactive))
allocate(wmo_interm_35_triplet(nocc+1: nactive, nocc+1: nactive))
allocate(wmo_interm_36_triplet(1: nocc, 1: nocc))
allocate(wmo_interm_37_triplet(1: nocc, 1: nocc))
allocate(wmo_interm_38_triplet(1: nocc, 1: nocc))
allocate(wmo_interm_39_triplet(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_40_triplet(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_41_triplet(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_42_triplet(nocc+1: nactive, nocc+1: nactive))
allocate(wmo_interm_43_triplet(nocc+1: nactive, nocc+1: nactive))
allocate(wmo_interm_44_triplet(nocc+1: nactive, nocc+1: nactive))
allocate(wmo_interm_45_triplet(1: nocc, 1: nocc))
allocate(wmo_interm_46_triplet(1: nocc, 1: nocc))
allocate(wmo_interm_47_triplet(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_48_triplet(nocc+1: nactive, nocc+1: nactive))
allocate(wmo_interm_49_triplet(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_50_triplet(nocc+1: nactive, nocc+1: nactive))
allocate(wmo_interm_51_triplet(1: nocc, 1: nocc))
allocate(wmo_interm_52_triplet(1: nocc, 1: nocc))
allocate(wmo_interm_53_triplet(1: nocc, 1: nocc))
allocate(wmo_interm_54_triplet(1: nocc, 1: nocc))
allocate(wmo_interm_55_triplet(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_56_triplet(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_57_triplet(nocc+1: nactive, nocc+1: nactive))
allocate(wmo_interm_58_triplet(nocc+1: nactive, nocc+1: nactive))
allocate(wmo_interm_59_triplet(nocc+1: nactive, nocc+1: nactive))
allocate(wmo_interm_60_triplet(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wmo_interm_61_triplet(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wmo_interm_62_triplet(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wmo_interm_63_triplet(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wmo_interm_64_triplet(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wmo_interm_65_triplet(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wmo_interm_66_triplet(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wmo_interm_67_triplet(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wmo_interm_68_triplet(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wmo_interm_69_triplet(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wmo_interm_70_triplet(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wmo_interm_71_triplet(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wmo_interm_72_triplet(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wmo_interm_73_triplet(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wmo_interm_74_triplet(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wmo_interm_75_triplet(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wmo_interm_76_triplet(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wmo_interm_77_triplet(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wmo_interm_78_triplet(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wmo_interm_79_triplet(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wmo_interm_80_triplet(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wmo_interm_81_triplet(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
wmo_interm_0_triplet = zero 
wmo_interm_1_triplet = zero 
wmo_interm_2_triplet = zero 
wmo_interm_3_triplet = zero 
wmo_interm_4_triplet = zero 
wmo_interm_5_triplet = zero 
wmo_interm_6_triplet = zero 
wmo_interm_7_triplet = zero 
wmo_interm_8_triplet = zero 
wmo_interm_9_triplet = zero 
wmo_interm_10_triplet = zero 
wmo_interm_11_triplet = zero 
wmo_interm_12_triplet = zero 
wmo_interm_13_triplet = zero 
wmo_interm_14_triplet = zero 
wmo_interm_15_triplet = zero 
wmo_interm_16_triplet = zero 
wmo_interm_17_triplet = zero 
wmo_interm_18_triplet = zero 
wmo_interm_19_triplet = zero 
wmo_interm_20_triplet = zero 
wmo_interm_21_triplet = zero 
wmo_interm_22_triplet = zero 
wmo_interm_23_triplet = zero 
wmo_interm_24_triplet = zero 
wmo_interm_25_triplet = zero 
wmo_interm_26_triplet = zero 
wmo_interm_27_triplet = zero 
wmo_interm_28_triplet = zero 
wmo_interm_29_triplet = zero 
wmo_interm_30_triplet = zero 
wmo_interm_31_triplet = zero 
wmo_interm_32_triplet = zero 
wmo_interm_33_triplet = zero 
wmo_interm_34_triplet = zero 
wmo_interm_35_triplet = zero 
wmo_interm_36_triplet = zero 
wmo_interm_37_triplet = zero 
wmo_interm_38_triplet = zero 
wmo_interm_39_triplet = zero 
wmo_interm_40_triplet = zero 
wmo_interm_41_triplet = zero 
wmo_interm_42_triplet = zero 
wmo_interm_43_triplet = zero 
wmo_interm_44_triplet = zero 
wmo_interm_45_triplet = zero 
wmo_interm_46_triplet = zero 
wmo_interm_47_triplet = zero 
wmo_interm_48_triplet = zero 
wmo_interm_49_triplet = zero 
wmo_interm_50_triplet = zero 
wmo_interm_51_triplet = zero 
wmo_interm_52_triplet = zero 
wmo_interm_53_triplet = zero 
wmo_interm_54_triplet = zero 
wmo_interm_55_triplet = zero 
wmo_interm_56_triplet = zero 
wmo_interm_57_triplet = zero 
wmo_interm_58_triplet = zero 
wmo_interm_59_triplet = zero 
wmo_interm_60_triplet = zero 
wmo_interm_61_triplet = zero 
wmo_interm_62_triplet = zero 
wmo_interm_63_triplet = zero 
wmo_interm_64_triplet = zero 
wmo_interm_65_triplet = zero 
wmo_interm_66_triplet = zero 
wmo_interm_67_triplet = zero 
wmo_interm_68_triplet = zero 
wmo_interm_69_triplet = zero 
wmo_interm_70_triplet = zero 
wmo_interm_71_triplet = zero 
wmo_interm_72_triplet = zero 
wmo_interm_73_triplet = zero 
wmo_interm_74_triplet = zero 
wmo_interm_75_triplet = zero 
wmo_interm_76_triplet = zero 
wmo_interm_77_triplet = zero 
wmo_interm_78_triplet = zero 
wmo_interm_79_triplet = zero 
wmo_interm_80_triplet = zero 
wmo_interm_81_triplet = zero

      end subroutine overlap_ccsd_triplet_init

     subroutine overlap_ccsd_triplet_free

deallocate(wmo_interm_0_triplet)
deallocate(wmo_interm_1_triplet)
deallocate(wmo_interm_2_triplet)
deallocate(wmo_interm_3_triplet)
deallocate(wmo_interm_4_triplet)
deallocate(wmo_interm_5_triplet)
deallocate(wmo_interm_6_triplet)
deallocate(wmo_interm_7_triplet)
deallocate(wmo_interm_8_triplet)
deallocate(wmo_interm_9_triplet)
deallocate(wmo_interm_10_triplet)
deallocate(wmo_interm_11_triplet)
deallocate(wmo_interm_12_triplet)
deallocate(wmo_interm_13_triplet)
deallocate(wmo_interm_14_triplet)
deallocate(wmo_interm_15_triplet)
deallocate(wmo_interm_16_triplet)
deallocate(wmo_interm_17_triplet)
deallocate(wmo_interm_18_triplet)
deallocate(wmo_interm_19_triplet)
deallocate(wmo_interm_20_triplet)
deallocate(wmo_interm_21_triplet)
deallocate(wmo_interm_22_triplet)
deallocate(wmo_interm_23_triplet)
deallocate(wmo_interm_24_triplet)
deallocate(wmo_interm_25_triplet)
deallocate(wmo_interm_26_triplet)
deallocate(wmo_interm_27_triplet)
deallocate(wmo_interm_28_triplet)
deallocate(wmo_interm_29_triplet)
deallocate(wmo_interm_30_triplet)
deallocate(wmo_interm_31_triplet)
deallocate(wmo_interm_32_triplet)
deallocate(wmo_interm_33_triplet)
deallocate(wmo_interm_34_triplet)
deallocate(wmo_interm_35_triplet)
deallocate(wmo_interm_36_triplet)
deallocate(wmo_interm_37_triplet)
deallocate(wmo_interm_38_triplet)
deallocate(wmo_interm_39_triplet)
deallocate(wmo_interm_40_triplet)
deallocate(wmo_interm_41_triplet)
deallocate(wmo_interm_42_triplet)
deallocate(wmo_interm_43_triplet)
deallocate(wmo_interm_44_triplet)
deallocate(wmo_interm_45_triplet)
deallocate(wmo_interm_46_triplet)
deallocate(wmo_interm_47_triplet)
deallocate(wmo_interm_48_triplet)
deallocate(wmo_interm_49_triplet)
deallocate(wmo_interm_50_triplet)
deallocate(wmo_interm_51_triplet)
deallocate(wmo_interm_52_triplet)
deallocate(wmo_interm_53_triplet)
deallocate(wmo_interm_54_triplet)
deallocate(wmo_interm_55_triplet)
deallocate(wmo_interm_56_triplet)
deallocate(wmo_interm_57_triplet)
deallocate(wmo_interm_58_triplet)
deallocate(wmo_interm_59_triplet)
deallocate(wmo_interm_60_triplet)
deallocate(wmo_interm_61_triplet)
deallocate(wmo_interm_62_triplet)
deallocate(wmo_interm_63_triplet)
deallocate(wmo_interm_64_triplet)
deallocate(wmo_interm_65_triplet)
deallocate(wmo_interm_66_triplet)
deallocate(wmo_interm_67_triplet)
deallocate(wmo_interm_68_triplet)
deallocate(wmo_interm_69_triplet)
deallocate(wmo_interm_70_triplet)
deallocate(wmo_interm_71_triplet)
deallocate(wmo_interm_72_triplet)
deallocate(wmo_interm_73_triplet)
deallocate(wmo_interm_74_triplet)
deallocate(wmo_interm_75_triplet)
deallocate(wmo_interm_76_triplet)
deallocate(wmo_interm_77_triplet)
deallocate(wmo_interm_78_triplet)
deallocate(wmo_interm_79_triplet)
deallocate(wmo_interm_80_triplet)
deallocate(wmo_interm_81_triplet)
     end subroutine overlap_ccsd_triplet_free

 subroutine overlap_intermediates_ccsd_triplet(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr) 

        integer, intent(in) :: nocc, nactive
        real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
        real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
        real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
        real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
        double precision, dimension(:), intent(in) ::vrdav_Rl
        double precision, dimension(:), intent(in) ::vrdav_Rr
        integer :: a, i, j, b, k, c, l 
        real(F64) :: sum


!$omp parallel private(a, i, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do i = 1, nocc 
do j = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * r1(vrdav_Rr, a,j)
end do 
wmo_interm_0_triplet(i, j) = wmo_interm_0_triplet(i, j) + sum 
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
wmo_interm_1_triplet(i, j) = wmo_interm_1_triplet(i, j) + sum 
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
wmo_interm_2_triplet(i, j) = wmo_interm_2_triplet(i, j) + sum 
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
wmo_interm_3_triplet(i, j) = wmo_interm_3_triplet(i, j) + sum 
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
wmo_interm_4_triplet(b, j) = wmo_interm_4_triplet(b, j) + sum 
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
wmo_interm_5_triplet(b, j) = wmo_interm_5_triplet(b, j) + sum 
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
wmo_interm_6_triplet(b, j) = wmo_interm_6_triplet(b, j) + sum 
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
wmo_interm_7_triplet(b, j) = wmo_interm_7_triplet(b, j) + sum 
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
wmo_interm_8_triplet(b, j) = wmo_interm_8_triplet(b, j) + sum 
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
wmo_interm_9_triplet(b, j) = wmo_interm_9_triplet(b, j) + sum 
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
wmo_interm_10_triplet(j, k) = wmo_interm_10_triplet(j, k) + sum 
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
wmo_interm_11_triplet(b, c) = wmo_interm_11_triplet(b, c) + sum 
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
wmo_interm_12_triplet(a, b) = wmo_interm_12_triplet(a, b) + sum 
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
wmo_interm_13_triplet(j, k) = wmo_interm_13_triplet(j, k) + sum 
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
wmo_interm_14_triplet(j, k) = wmo_interm_14_triplet(j, k) + sum 
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
wmo_interm_15_triplet(b, c) = wmo_interm_15_triplet(b, c) + sum 
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
wmo_interm_16_triplet(b, c) = wmo_interm_16_triplet(b, c) + sum 
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
wmo_interm_17_triplet(j, k) = wmo_interm_17_triplet(j, k) + sum 
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
wmo_interm_18_triplet(b, c) = wmo_interm_18_triplet(b, c) + sum 
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
wmo_interm_19_triplet(j, k) = wmo_interm_19_triplet(j, k) + sum 
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
wmo_interm_20_triplet(b, c) = wmo_interm_20_triplet(b, c) + sum 
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
wmo_interm_21_triplet(j, k) = wmo_interm_21_triplet(j, k) + sum 
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
wmo_interm_22_triplet(j, k) = wmo_interm_22_triplet(j, k) + sum 
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
wmo_interm_23_triplet(b, c) = wmo_interm_23_triplet(b, c) + sum 
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
wmo_interm_24_triplet(b, c) = wmo_interm_24_triplet(b, c) + sum 
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
wmo_interm_25_triplet(j, k) = wmo_interm_25_triplet(j, k) + sum 
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
wmo_interm_26_triplet(j, k) = wmo_interm_26_triplet(j, k) + sum 
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
wmo_interm_27_triplet(b, c) = wmo_interm_27_triplet(b, c) + sum 
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
wmo_interm_28_triplet(b, c) = wmo_interm_28_triplet(b, c) + sum 
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
wmo_interm_29_triplet(a, b) = wmo_interm_29_triplet(a, b) + sum 
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
wmo_interm_30_triplet(b, c) = wmo_interm_30_triplet(b, c) + sum 
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
wmo_interm_31_triplet(b, c) = wmo_interm_31_triplet(b, c) + sum 
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
wmo_interm_32_triplet(j, k) = wmo_interm_32_triplet(j, k) + sum 
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
wmo_interm_33_triplet(j, k) = wmo_interm_33_triplet(j, k) + sum 
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
wmo_interm_34_triplet(b, c) = wmo_interm_34_triplet(b, c) + sum 
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
wmo_interm_35_triplet(b, c) = wmo_interm_35_triplet(b, c) + sum 
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
wmo_interm_36_triplet(j, k) = wmo_interm_36_triplet(j, k) + sum 
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
wmo_interm_37_triplet(j, k) = wmo_interm_37_triplet(j, k) + sum 
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
wmo_interm_38_triplet(j, k) = wmo_interm_38_triplet(j, k) + sum 
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
wmo_interm_39_triplet(b, j) = wmo_interm_39_triplet(b, j) + sum 
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
wmo_interm_40_triplet(b, j) = wmo_interm_40_triplet(b, j) + sum 
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
wmo_interm_41_triplet(b, j) = wmo_interm_41_triplet(b, j) + sum 
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
wmo_interm_42_triplet(b, c) = wmo_interm_42_triplet(b, c) + sum 
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
wmo_interm_43_triplet(a, b) = wmo_interm_43_triplet(a, b) + sum 
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
wmo_interm_44_triplet(b, c) = wmo_interm_44_triplet(b, c) + sum 
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
wmo_interm_45_triplet(j, k) = wmo_interm_45_triplet(j, k) + sum 
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
wmo_interm_46_triplet(j, k) = wmo_interm_46_triplet(j, k) + sum 
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
wmo_interm_47_triplet(b, j) = wmo_interm_47_triplet(b, j) + sum 
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
wmo_interm_48_triplet(b, c) = wmo_interm_48_triplet(b, c) + sum 
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
wmo_interm_49_triplet(b, j) = wmo_interm_49_triplet(b, j) + sum 
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
wmo_interm_50_triplet(b, c) = wmo_interm_50_triplet(b, c) + sum 
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
wmo_interm_51_triplet(j, k) = wmo_interm_51_triplet(j, k) + sum 
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
wmo_interm_52_triplet(j, k) = wmo_interm_52_triplet(j, k) + sum 
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
wmo_interm_53_triplet(j, k) = wmo_interm_53_triplet(j, k) + sum 
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
wmo_interm_54_triplet(j, k) = wmo_interm_54_triplet(j, k) + sum 
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
wmo_interm_55_triplet(b, j) = wmo_interm_55_triplet(b, j) + sum 
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
wmo_interm_56_triplet(b, j) = wmo_interm_56_triplet(b, j) + sum 
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
wmo_interm_57_triplet(a, b) = wmo_interm_57_triplet(a, b) + sum 
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
wmo_interm_58_triplet(b, c) = wmo_interm_58_triplet(b, c) + sum 
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
wmo_interm_59_triplet(b, c) = wmo_interm_59_triplet(b, c) + sum 
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
wmo_interm_60_triplet(i, j, k, l) = wmo_interm_60_triplet(i, j, k, l) + sum 
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
wmo_interm_61_triplet(i, j, k, l) = wmo_interm_61_triplet(i, j, k, l) + sum 
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
wmo_interm_62_triplet(i, j, k, l) = wmo_interm_62_triplet(i, j, k, l) + sum 
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
wmo_interm_63_triplet(i, j, k, l) = wmo_interm_63_triplet(i, j, k, l) + sum 
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
wmo_interm_64_triplet(i, j, k, l) = wmo_interm_64_triplet(i, j, k, l) + sum 
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
sum = sum + r2m(vrdav_Rr, a,i,b,j) * s2(a,b,k,l)
end do 
end do 
wmo_interm_65_triplet(i, j, k, l) = wmo_interm_65_triplet(i, j, k, l) + sum 
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
wmo_interm_66_triplet(i, j, k, l) = wmo_interm_66_triplet(i, j, k, l) + sum 
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
wmo_interm_67_triplet(i, j, k, l) = wmo_interm_67_triplet(i, j, k, l) + sum 
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
wmo_interm_68_triplet(i, j, k, l) = wmo_interm_68_triplet(i, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wmo_interm_69_triplet(b, c, j, k) = wmo_interm_69_triplet(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wmo_interm_70_triplet(b, c, j, k) = wmo_interm_70_triplet(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wmo_interm_71_triplet(b, c, j, k) = wmo_interm_71_triplet(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wmo_interm_72_triplet(b, c, j, k) = wmo_interm_72_triplet(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wmo_interm_73_triplet(b, c, j, k) = wmo_interm_73_triplet(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wmo_interm_74_triplet(b, c, j, k) = wmo_interm_74_triplet(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wmo_interm_75_triplet(b, c, j, k) = wmo_interm_75_triplet(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wmo_interm_76_triplet(b, c, j, k) = wmo_interm_76_triplet(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wmo_interm_77_triplet(b, c, j, k) = wmo_interm_77_triplet(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wmo_interm_78_triplet(b, c, j, k) = wmo_interm_78_triplet(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wmo_interm_79_triplet(b, c, j, k) = wmo_interm_79_triplet(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wmo_interm_80_triplet(b, c, j, k) = wmo_interm_80_triplet(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wmo_interm_81_triplet(b, c, j, k) = wmo_interm_81_triplet(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 
end subroutine overlap_intermediates_ccsd_triplet

    function overlap_f_triplet(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr) 
    double precision :: overlap_f_triplet
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 

    double precision, dimension(:), intent(in) :: vrdav_Rl
    double precision, dimension(:), intent(in) :: vrdav_Rr
    integer :: s ,j,i,b,a,k,c,l,d 
    double precision, dimension(0:355) :: term 
    term = 0.d+0 
    do j = 1, nocc 
do i = 1, nocc 
term(0) = term(0) + wmo_interm_0_triplet(i, j) * wmo_interm_1_triplet(j, i)
term(1) = term(1) + wmo_interm_2_triplet(i, j) * wmo_interm_3_triplet(i, j)
term(2) = term(2) + wmo_interm_0_triplet(i, j) * wmo_interm_53_triplet(j, i)
term(3) = term(3) + wmo_interm_0_triplet(i, j) * wmo_interm_54_triplet(j, i)
term(4) = term(4) + wmo_interm_0_triplet(i, j) * wmo_interm_53_triplet(j, i)
term(5) = term(5) + wmo_interm_0_triplet(i, j) * wmo_interm_54_triplet(j, i)
end do 
end do 

term(0) = term(0) * (-2.0d+0) 
term(1) = term(1) * (-2.0d+0) 
term(3) = term(3) * (-2.0d+0) 
term(5) = term(5) * (-2.0d+0) 

do j = 1, nocc 
do b = nocc + 1, nactive 
term(6) = term(6) + wmo_interm_4_triplet(b, j) * wmo_interm_5_triplet(b, j)
term(7) = term(7) + wmo_interm_4_triplet(b, j) * wmo_interm_6_triplet(b, j)
term(8) = term(8) + wmo_interm_4_triplet(b, j) * wmo_interm_7_triplet(b, j)
term(9) = term(9) + wmo_interm_47_triplet(b, j) * wmo_interm_40_triplet(b, j)
term(10) = term(10) + wmo_interm_47_triplet(b, j) * wmo_interm_41_triplet(b, j)
end do 
end do 

term(6) = term(6) * (-8.0d+0) 
term(7) = term(7) * (-8.0d+0) 
term(8) = term(8) * 8.0d+0 
term(9) = term(9) * (-4.0d+0) 
term(10) = term(10) * 8.0d+0 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(11) = term(11) + wmo_interm_8_triplet(a, i) * wmo_interm_5_triplet(a, i)
term(12) = term(12) + wmo_interm_8_triplet(a, i) * wmo_interm_6_triplet(a, i)
term(13) = term(13) + wmo_interm_8_triplet(a, i) * wmo_interm_7_triplet(a, i)
term(14) = term(14) + wmo_interm_39_triplet(a, i) * wmo_interm_40_triplet(a, i)
term(15) = term(15) + wmo_interm_39_triplet(a, i) * wmo_interm_41_triplet(a, i)
term(16) = term(16) + r1(vrdav_Rl, a,i) * r1(vrdav_Rr, a,i)
end do 
end do 

term(11) = term(11) * (-8.0d+0) 
term(12) = term(12) * (-8.0d+0) 
term(13) = term(13) * 8.0d+0 
term(14) = term(14) * (-4.0d+0) 
term(15) = term(15) * 8.0d+0 
term(16) = term(16) * 2.0d+0 

do j = 1, nocc 
do a = nocc + 1, nactive 
term(17) = term(17) + wmo_interm_9_triplet(a, j) * wmo_interm_5_triplet(a, j)
term(18) = term(18) + wmo_interm_9_triplet(a, j) * wmo_interm_6_triplet(a, j)
term(19) = term(19) + wmo_interm_9_triplet(a, j) * wmo_interm_7_triplet(a, j)
term(20) = term(20) + wmo_interm_49_triplet(a, j) * wmo_interm_40_triplet(a, j)
term(21) = term(21) + wmo_interm_49_triplet(a, j) * wmo_interm_41_triplet(a, j)
end do 
end do 

term(17) = term(17) * 4.0d+0 
term(18) = term(18) * 4.0d+0 
term(19) = term(19) * (-4.0d+0) 
term(20) = term(20) * 2.0d+0 
term(21) = term(21) * (-4.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
term(22) = term(22) + wmo_interm_9_triplet(b, i) * wmo_interm_5_triplet(b, i)
term(23) = term(23) + wmo_interm_9_triplet(b, i) * wmo_interm_6_triplet(b, i)
term(24) = term(24) + wmo_interm_9_triplet(b, i) * wmo_interm_7_triplet(b, i)
term(25) = term(25) + wmo_interm_49_triplet(b, i) * wmo_interm_40_triplet(b, i)
term(26) = term(26) + wmo_interm_49_triplet(b, i) * wmo_interm_41_triplet(b, i)
end do 
end do 

term(22) = term(22) * 4.0d+0 
term(23) = term(23) * 4.0d+0 
term(24) = term(24) * (-4.0d+0) 
term(25) = term(25) * 2.0d+0 
term(26) = term(26) * (-4.0d+0) 

do j = 1, nocc 
do d = nocc + 1, nactive 
do l = 1, nocc 
do b = nocc + 1, nactive 
term(27) = term(27) + s2(b,d,l,j) * wmo_interm_55_triplet(d, l) * wmo_interm_5_triplet(b, j)
term(28) = term(28) + s2(b,d,l,j) * wmo_interm_55_triplet(d, l) * wmo_interm_6_triplet(b, j)
end do 
end do 
end do 
end do 

term(27) = term(27) * 3.0d+0 
term(28) = term(28) * 4.0d+0 

do j = 1, nocc 
do d = nocc + 1, nactive 
do l = 1, nocc 
do c = nocc + 1, nactive 
term(29) = term(29) + s2(c,d,l,j) * wmo_interm_55_triplet(d, l) * wmo_interm_5_triplet(c, j)
term(30) = term(30) + s2(c,d,l,j) * wmo_interm_55_triplet(d, l) * wmo_interm_7_triplet(c, j)
end do 
end do 
end do 
end do 

term(29) = -term(29) 
term(30) = term(30) * (-2.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
term(31) = term(31) + wmo_interm_10_triplet(i, k) * wmo_interm_1_triplet(k, i)
term(32) = term(32) + wmo_interm_13_triplet(i, k) * wmo_interm_1_triplet(k, i)
term(33) = term(33) + wmo_interm_14_triplet(i, k) * wmo_interm_1_triplet(k, i)
term(34) = term(34) + wmo_interm_19_triplet(i, k) * wmo_interm_1_triplet(k, i)
term(35) = term(35) + wmo_interm_25_triplet(i, k) * wmo_interm_1_triplet(k, i)
term(36) = term(36) + wmo_interm_26_triplet(i, k) * wmo_interm_1_triplet(k, i)
term(37) = term(37) + wmo_interm_2_triplet(i, k) * wmo_interm_32_triplet(i, k)
term(38) = term(38) + wmo_interm_2_triplet(i, k) * wmo_interm_33_triplet(i, k)
term(39) = term(39) + wmo_interm_2_triplet(i, k) * wmo_interm_36_triplet(i, k)
term(40) = term(40) + wmo_interm_2_triplet(i, k) * wmo_interm_37_triplet(i, k)
term(41) = term(41) + wmo_interm_2_triplet(i, k) * wmo_interm_38_triplet(i, k)
term(42) = term(42) + wmo_interm_52_triplet(i, k) * wmo_interm_3_triplet(i, k)
term(43) = term(43) + wmo_interm_51_triplet(i, k) * wmo_interm_3_triplet(i, k)
term(44) = term(44) + wmo_interm_10_triplet(i, k) * wmo_interm_53_triplet(k, i)
term(45) = term(45) + wmo_interm_10_triplet(i, k) * wmo_interm_54_triplet(k, i)
term(46) = term(46) + wmo_interm_10_triplet(i, k) * wmo_interm_53_triplet(k, i)
term(47) = term(47) + wmo_interm_10_triplet(i, k) * wmo_interm_54_triplet(k, i)
term(48) = term(48) + wmo_interm_13_triplet(i, k) * wmo_interm_53_triplet(k, i)
term(49) = term(49) + wmo_interm_14_triplet(i, k) * wmo_interm_53_triplet(k, i)
term(50) = term(50) + wmo_interm_13_triplet(i, k) * wmo_interm_54_triplet(k, i)
term(51) = term(51) + wmo_interm_14_triplet(i, k) * wmo_interm_54_triplet(k, i)
term(52) = term(52) + wmo_interm_13_triplet(i, k) * wmo_interm_53_triplet(k, i)
term(53) = term(53) + wmo_interm_14_triplet(i, k) * wmo_interm_53_triplet(k, i)
term(54) = term(54) + wmo_interm_13_triplet(i, k) * wmo_interm_54_triplet(k, i)
term(55) = term(55) + wmo_interm_14_triplet(i, k) * wmo_interm_54_triplet(k, i)
term(56) = term(56) + wmo_interm_19_triplet(i, k) * wmo_interm_53_triplet(k, i)
term(57) = term(57) + wmo_interm_19_triplet(i, k) * wmo_interm_54_triplet(k, i)
term(58) = term(58) + wmo_interm_19_triplet(i, k) * wmo_interm_54_triplet(k, i)
term(59) = term(59) + wmo_interm_19_triplet(i, k) * wmo_interm_53_triplet(k, i)
term(60) = term(60) + wmo_interm_25_triplet(i, k) * wmo_interm_53_triplet(k, i)
term(61) = term(61) + wmo_interm_26_triplet(i, k) * wmo_interm_53_triplet(k, i)
term(62) = term(62) + wmo_interm_25_triplet(i, k) * wmo_interm_54_triplet(k, i)
term(63) = term(63) + wmo_interm_26_triplet(i, k) * wmo_interm_54_triplet(k, i)
term(64) = term(64) + wmo_interm_25_triplet(i, k) * wmo_interm_54_triplet(k, i)
term(65) = term(65) + wmo_interm_26_triplet(i, k) * wmo_interm_54_triplet(k, i)
term(66) = term(66) + wmo_interm_25_triplet(i, k) * wmo_interm_53_triplet(k, i)
term(67) = term(67) + wmo_interm_26_triplet(i, k) * wmo_interm_53_triplet(k, i)
end do 
end do 

term(31) = term(31) * (-4.0d+0) 
term(32) = term(32) * (-4.0d+0) 
term(33) = term(33) * 4.0d+0 
term(34) = term(34) * (-4.0d+0) 
term(35) = term(35) * 4.0d+0 
term(36) = term(36) * (-4.0d+0) 
term(37) = -term(37) 
term(38) = term(38) * 3.0d+0 
term(39) = term(39) * 2.0d+0 
term(40) = term(40) * 2.0d+0 
term(41) = term(41) * (-4.0d+0) 
term(42) = term(42) * (-4.0d+0) 
term(43) = term(43) * 2.0d+0 
term(44) = term(44) * 2.0d+0 
term(45) = term(45) * (-4.0d+0) 
term(46) = term(46) * 2.0d+0 
term(47) = term(47) * (-4.0d+0) 
term(48) = term(48) * 2.0d+0 
term(49) = term(49) * (-2.0d+0) 
term(50) = term(50) * (-4.0d+0) 
term(51) = term(51) * 4.0d+0 
term(52) = term(52) * 2.0d+0 
term(53) = term(53) * (-2.0d+0) 
term(54) = term(54) * (-4.0d+0) 
term(55) = term(55) * 4.0d+0 
term(56) = term(56) * 2.0d+0 
term(57) = term(57) * (-4.0d+0) 
term(58) = term(58) * (-4.0d+0) 
term(59) = term(59) * 2.0d+0 
term(60) = term(60) * (-2.0d+0) 
term(61) = term(61) * 2.0d+0 
term(62) = term(62) * 4.0d+0 
term(63) = term(63) * (-4.0d+0) 
term(64) = term(64) * 4.0d+0 
term(65) = term(65) * (-4.0d+0) 
term(66) = term(66) * (-2.0d+0) 
term(67) = term(67) * 2.0d+0 

do c = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(68) = term(68) + wmo_interm_11_triplet(a, c) * wmo_interm_12_triplet(c, a)
term(69) = term(69) + wmo_interm_15_triplet(a, c) * wmo_interm_12_triplet(c, a)
term(70) = term(70) + wmo_interm_16_triplet(a, c) * wmo_interm_12_triplet(c, a)
term(71) = term(71) + wmo_interm_20_triplet(a, c) * wmo_interm_12_triplet(c, a)
term(72) = term(72) + wmo_interm_27_triplet(a, c) * wmo_interm_12_triplet(c, a)
term(73) = term(73) + wmo_interm_28_triplet(a, c) * wmo_interm_12_triplet(c, a)
term(74) = term(74) + wmo_interm_29_triplet(a, c) * wmo_interm_30_triplet(a, c)
term(75) = term(75) + wmo_interm_29_triplet(a, c) * wmo_interm_31_triplet(a, c)
term(76) = term(76) + wmo_interm_29_triplet(a, c) * wmo_interm_34_triplet(a, c)
term(77) = term(77) + wmo_interm_29_triplet(a, c) * wmo_interm_35_triplet(a, c)
term(78) = term(78) + wmo_interm_50_triplet(a, c) * wmo_interm_43_triplet(a, c)
term(79) = term(79) + wmo_interm_48_triplet(a, c) * wmo_interm_43_triplet(a, c)
term(80) = term(80) + wmo_interm_11_triplet(a, c) * wmo_interm_58_triplet(c, a)
term(81) = term(81) + wmo_interm_11_triplet(a, c) * wmo_interm_59_triplet(c, a)
term(82) = term(82) + wmo_interm_15_triplet(a, c) * wmo_interm_58_triplet(c, a)
term(83) = term(83) + wmo_interm_16_triplet(a, c) * wmo_interm_58_triplet(c, a)
term(84) = term(84) + wmo_interm_15_triplet(a, c) * wmo_interm_59_triplet(c, a)
term(85) = term(85) + wmo_interm_16_triplet(a, c) * wmo_interm_59_triplet(c, a)
term(86) = term(86) + wmo_interm_20_triplet(a, c) * wmo_interm_58_triplet(c, a)
term(87) = term(87) + wmo_interm_20_triplet(a, c) * wmo_interm_59_triplet(c, a)
term(88) = term(88) + wmo_interm_27_triplet(a, c) * wmo_interm_58_triplet(c, a)
term(89) = term(89) + wmo_interm_28_triplet(a, c) * wmo_interm_58_triplet(c, a)
term(90) = term(90) + wmo_interm_27_triplet(a, c) * wmo_interm_59_triplet(c, a)
term(91) = term(91) + wmo_interm_28_triplet(a, c) * wmo_interm_59_triplet(c, a)
end do 
end do 

term(68) = term(68) * (-4.0d+0) 
term(69) = term(69) * (-4.0d+0) 
term(70) = term(70) * 4.0d+0 
term(71) = term(71) * (-4.0d+0) 
term(72) = term(72) * 4.0d+0 
term(73) = term(73) * (-4.0d+0) 
term(74) = term(74) * 2.0d+0 
term(75) = term(75) * (-2.0d+0) 
term(76) = term(76) * 4.0d+0 
term(77) = term(77) * (-4.0d+0) 
term(78) = term(78) * (-4.0d+0) 
term(79) = term(79) * 2.0d+0 
term(80) = term(80) * (-8.0d+0) 
term(81) = term(81) * 4.0d+0 
term(82) = term(82) * (-8.0d+0) 
term(83) = term(83) * 8.0d+0 
term(84) = term(84) * 4.0d+0 
term(85) = term(85) * (-4.0d+0) 
term(86) = term(86) * (-8.0d+0) 
term(87) = term(87) * 4.0d+0 
term(88) = term(88) * 8.0d+0 
term(89) = term(89) * (-8.0d+0) 
term(90) = term(90) * (-4.0d+0) 
term(91) = term(91) * 4.0d+0 

do a = nocc + 1, nactive 
do i = 1, nocc 
do c = nocc + 1, nactive 
term(92) = term(92) + r1(vrdav_Rl, a,i) * wmo_interm_5_triplet(c, i) * wmo_interm_58_triplet(c, a)
term(93) = term(93) + r1(vrdav_Rl, a,i) * wmo_interm_5_triplet(c, i) * wmo_interm_59_triplet(c, a)
term(94) = term(94) + r1(vrdav_Rl, a,i) * wmo_interm_7_triplet(c, i) * wmo_interm_58_triplet(c, a)
term(95) = term(95) + r1(vrdav_Rl, a,i) * wmo_interm_7_triplet(c, i) * wmo_interm_59_triplet(c, a)
end do 
end do 
end do 

term(92) = term(92) * (-2.0d+0) 
term(94) = term(94) * 4.0d+0 
term(95) = term(95) * (-2.0d+0) 

do l = 1, nocc 
do i = 1, nocc 
term(96) = term(96) + wmo_interm_52_triplet(i, l) * wmo_interm_32_triplet(i, l)
term(97) = term(97) + wmo_interm_52_triplet(i, l) * wmo_interm_33_triplet(i, l)
term(98) = term(98) + wmo_interm_51_triplet(i, l) * wmo_interm_32_triplet(i, l)
term(99) = term(99) + wmo_interm_51_triplet(i, l) * wmo_interm_33_triplet(i, l)
term(100) = term(100) + wmo_interm_52_triplet(i, l) * wmo_interm_36_triplet(i, l)
term(101) = term(101) + wmo_interm_52_triplet(i, l) * wmo_interm_37_triplet(i, l)
term(102) = term(102) + wmo_interm_52_triplet(i, l) * wmo_interm_38_triplet(i, l)
term(103) = term(103) + wmo_interm_51_triplet(i, l) * wmo_interm_36_triplet(i, l)
term(104) = term(104) + wmo_interm_51_triplet(i, l) * wmo_interm_37_triplet(i, l)
term(105) = term(105) + wmo_interm_51_triplet(i, l) * wmo_interm_38_triplet(i, l)
end do 
end do 

term(96) = term(96) * (-2.0d+0) 
term(97) = term(97) * 6.0d+0 
term(99) = term(99) * (-3.0d+0) 
term(100) = term(100) * 4.0d+0 
term(101) = term(101) * 4.0d+0 
term(102) = term(102) * (-8.0d+0) 
term(103) = term(103) * (-2.0d+0) 
term(104) = term(104) * (-2.0d+0) 
term(105) = term(105) * 4.0d+0 

do k = 1, nocc 
do j = 1, nocc 
term(106) = term(106) + wmo_interm_17_triplet(j, k) * wmo_interm_1_triplet(k, j)
term(107) = term(107) + wmo_interm_21_triplet(j, k) * wmo_interm_1_triplet(k, j)
term(108) = term(108) + wmo_interm_22_triplet(j, k) * wmo_interm_1_triplet(k, j)
term(109) = term(109) + wmo_interm_45_triplet(j, k) * wmo_interm_3_triplet(j, k)
term(110) = term(110) + wmo_interm_46_triplet(j, k) * wmo_interm_3_triplet(j, k)
term(111) = term(111) + wmo_interm_51_triplet(j, k) * wmo_interm_3_triplet(j, k)
term(112) = term(112) + wmo_interm_17_triplet(j, k) * wmo_interm_53_triplet(k, j)
term(113) = term(113) + wmo_interm_17_triplet(j, k) * wmo_interm_54_triplet(k, j)
term(114) = term(114) + wmo_interm_17_triplet(j, k) * wmo_interm_53_triplet(k, j)
term(115) = term(115) + wmo_interm_17_triplet(j, k) * wmo_interm_54_triplet(k, j)
term(116) = term(116) + wmo_interm_21_triplet(j, k) * wmo_interm_53_triplet(k, j)
term(117) = term(117) + wmo_interm_22_triplet(j, k) * wmo_interm_53_triplet(k, j)
term(118) = term(118) + wmo_interm_21_triplet(j, k) * wmo_interm_54_triplet(k, j)
term(119) = term(119) + wmo_interm_22_triplet(j, k) * wmo_interm_54_triplet(k, j)
term(120) = term(120) + wmo_interm_21_triplet(j, k) * wmo_interm_53_triplet(k, j)
term(121) = term(121) + wmo_interm_22_triplet(j, k) * wmo_interm_53_triplet(k, j)
term(122) = term(122) + wmo_interm_21_triplet(j, k) * wmo_interm_54_triplet(k, j)
term(123) = term(123) + wmo_interm_22_triplet(j, k) * wmo_interm_54_triplet(k, j)
end do 
end do 

term(106) = term(106) * 4.0d+0 
term(107) = term(107) * 4.0d+0 
term(108) = term(108) * (-4.0d+0) 
term(109) = term(109) * (-2.0d+0) 
term(110) = term(110) * 2.0d+0 
term(111) = term(111) * 2.0d+0 
term(112) = term(112) * (-2.0d+0) 
term(113) = term(113) * 4.0d+0 
term(114) = term(114) * (-2.0d+0) 
term(115) = term(115) * 4.0d+0 
term(116) = term(116) * (-2.0d+0) 
term(117) = term(117) * 2.0d+0 
term(118) = term(118) * 4.0d+0 
term(119) = term(119) * (-4.0d+0) 
term(120) = term(120) * (-2.0d+0) 
term(121) = term(121) * 2.0d+0 
term(122) = term(122) * 4.0d+0 
term(123) = term(123) * (-4.0d+0) 

do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(124) = term(124) + wmo_interm_18_triplet(b, c) * wmo_interm_12_triplet(c, b)
term(125) = term(125) + wmo_interm_23_triplet(b, c) * wmo_interm_12_triplet(c, b)
term(126) = term(126) + wmo_interm_24_triplet(b, c) * wmo_interm_12_triplet(c, b)
term(127) = term(127) + wmo_interm_42_triplet(b, c) * wmo_interm_43_triplet(b, c)
term(128) = term(128) + wmo_interm_44_triplet(b, c) * wmo_interm_43_triplet(b, c)
term(129) = term(129) + wmo_interm_48_triplet(b, c) * wmo_interm_43_triplet(b, c)
term(130) = term(130) + wmo_interm_18_triplet(b, c) * wmo_interm_58_triplet(c, b)
term(131) = term(131) + wmo_interm_18_triplet(b, c) * wmo_interm_59_triplet(c, b)
term(132) = term(132) + wmo_interm_23_triplet(b, c) * wmo_interm_58_triplet(c, b)
term(133) = term(133) + wmo_interm_24_triplet(b, c) * wmo_interm_58_triplet(c, b)
term(134) = term(134) + wmo_interm_23_triplet(b, c) * wmo_interm_59_triplet(c, b)
term(135) = term(135) + wmo_interm_24_triplet(b, c) * wmo_interm_59_triplet(c, b)
end do 
end do 

term(124) = term(124) * 4.0d+0 
term(125) = term(125) * 4.0d+0 
term(126) = term(126) * (-4.0d+0) 
term(127) = term(127) * 3.0d+0 
term(128) = -term(128) 
term(129) = term(129) * 2.0d+0 
term(130) = term(130) * 8.0d+0 
term(131) = term(131) * (-4.0d+0) 
term(132) = term(132) * 8.0d+0 
term(133) = term(133) * (-8.0d+0) 
term(134) = term(134) * (-4.0d+0) 
term(135) = term(135) * 4.0d+0 

do k = 1, nocc 
do d = nocc + 1, nactive 
do l = 1, nocc 
do b = nocc + 1, nactive 
term(136) = term(136) + s2(b,d,l,k) * wmo_interm_55_triplet(d, l) * wmo_interm_7_triplet(b, k)
end do 
end do 
end do 
end do 

term(136) = term(136) * (-2.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do c = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(137) = term(137) + wmo_interm_69_triplet(a, c, i, k) * wmo_interm_70_triplet(c, a, k, i)
term(138) = term(138) + wmo_interm_69_triplet(a, c, i, k) * wmo_interm_71_triplet(c, a, k, i)
term(139) = term(139) + wmo_interm_69_triplet(a, c, i, k) * wmo_interm_72_triplet(c, a, k, i)
term(140) = term(140) + wmo_interm_69_triplet(a, c, i, k) * wmo_interm_73_triplet(c, a, k, i)
term(141) = term(141) + wmo_interm_74_triplet(a, c, i, k) * wmo_interm_70_triplet(c, a, k, i)
term(142) = term(142) + wmo_interm_75_triplet(a, c, i, k) * wmo_interm_70_triplet(c, a, k, i)
term(143) = term(143) + wmo_interm_74_triplet(a, c, i, k) * wmo_interm_71_triplet(c, a, k, i)
term(144) = term(144) + wmo_interm_75_triplet(a, c, i, k) * wmo_interm_71_triplet(c, a, k, i)
term(145) = term(145) + wmo_interm_74_triplet(a, c, i, k) * wmo_interm_72_triplet(c, a, k, i)
term(146) = term(146) + wmo_interm_75_triplet(a, c, i, k) * wmo_interm_72_triplet(c, a, k, i)
term(147) = term(147) + wmo_interm_74_triplet(a, c, i, k) * wmo_interm_73_triplet(c, a, k, i)
term(148) = term(148) + wmo_interm_75_triplet(a, c, i, k) * wmo_interm_73_triplet(c, a, k, i)
term(149) = term(149) + wmo_interm_76_triplet(a, c, i, k) * wmo_interm_70_triplet(c, a, k, i)
term(150) = term(150) + wmo_interm_78_triplet(a, c, i, k) * wmo_interm_70_triplet(c, a, k, i)
term(151) = term(151) + wmo_interm_79_triplet(a, c, i, k) * wmo_interm_70_triplet(c, a, k, i)
end do 
end do 
end do 
end do 

term(137) = term(137) * 8.0d+0 
term(138) = term(138) * (-8.0d+0) 
term(139) = term(139) * 16.0d+0 
term(140) = term(140) * (-8.0d+0) 
term(141) = term(141) * (-8.0d+0) 
term(142) = term(142) * 8.0d+0 
term(143) = term(143) * 8.0d+0 
term(144) = term(144) * (-8.0d+0) 
term(145) = term(145) * (-16.0d+0) 
term(146) = term(146) * 16.0d+0 
term(147) = term(147) * 8.0d+0 
term(148) = term(148) * (-8.0d+0) 
term(149) = term(149) * 4.0d+0 
term(150) = term(150) * (-4.0d+0) 
term(151) = term(151) * 4.0d+0 

do d = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(152) = term(152) + wmo_interm_50_triplet(a, d) * wmo_interm_30_triplet(a, d)
term(153) = term(153) + wmo_interm_50_triplet(a, d) * wmo_interm_31_triplet(a, d)
term(154) = term(154) + wmo_interm_48_triplet(a, d) * wmo_interm_30_triplet(a, d)
term(155) = term(155) + wmo_interm_48_triplet(a, d) * wmo_interm_31_triplet(a, d)
term(156) = term(156) + wmo_interm_50_triplet(a, d) * wmo_interm_34_triplet(a, d)
term(157) = term(157) + wmo_interm_50_triplet(a, d) * wmo_interm_35_triplet(a, d)
term(158) = term(158) + wmo_interm_48_triplet(a, d) * wmo_interm_34_triplet(a, d)
term(159) = term(159) + wmo_interm_48_triplet(a, d) * wmo_interm_35_triplet(a, d)
end do 
end do 

term(152) = term(152) * 4.0d+0 
term(153) = term(153) * (-4.0d+0) 
term(154) = term(154) * (-2.0d+0) 
term(155) = term(155) * 2.0d+0 
term(156) = term(156) * 8.0d+0 
term(157) = term(157) * (-8.0d+0) 
term(158) = term(158) * (-4.0d+0) 
term(159) = term(159) * 4.0d+0 

do i = 1, nocc 
do a = nocc + 1, nactive 
do j = 1, nocc 
do b = nocc + 1, nactive 
term(160) = term(160) + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, b,j,a,i) * s1(b,j)
end do 
end do 
end do 
end do 

term(160) = term(160) * 4.0d+0 

do k = 1, nocc 
do j = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(161) = term(161) + wmo_interm_76_triplet(b, c, j, k) * wmo_interm_70_triplet(c, b, k, j)
term(162) = term(162) + wmo_interm_76_triplet(b, c, j, k) * wmo_interm_71_triplet(c, b, k, j)
term(163) = term(163) + wmo_interm_76_triplet(b, c, j, k) * wmo_interm_72_triplet(c, b, k, j)
term(164) = term(164) + wmo_interm_76_triplet(b, c, j, k) * wmo_interm_73_triplet(c, b, k, j)
term(165) = term(165) + wmo_interm_78_triplet(b, c, j, k) * wmo_interm_70_triplet(c, b, k, j)
term(166) = term(166) + wmo_interm_79_triplet(b, c, j, k) * wmo_interm_70_triplet(c, b, k, j)
term(167) = term(167) + wmo_interm_78_triplet(b, c, j, k) * wmo_interm_71_triplet(c, b, k, j)
term(168) = term(168) + wmo_interm_79_triplet(b, c, j, k) * wmo_interm_71_triplet(c, b, k, j)
term(169) = term(169) + wmo_interm_78_triplet(b, c, j, k) * wmo_interm_72_triplet(c, b, k, j)
term(170) = term(170) + wmo_interm_79_triplet(b, c, j, k) * wmo_interm_72_triplet(c, b, k, j)
term(171) = term(171) + wmo_interm_78_triplet(b, c, j, k) * wmo_interm_73_triplet(c, b, k, j)
term(172) = term(172) + wmo_interm_79_triplet(b, c, j, k) * wmo_interm_73_triplet(c, b, k, j)
end do 
end do 
end do 
end do 

term(161) = term(161) * 4.0d+0 
term(162) = term(162) * (-8.0d+0) 
term(163) = term(163) * 16.0d+0 
term(164) = term(164) * (-8.0d+0) 
term(165) = term(165) * (-4.0d+0) 
term(166) = term(166) * 4.0d+0 
term(167) = term(167) * 8.0d+0 
term(168) = term(168) * (-8.0d+0) 
term(169) = term(169) * (-16.0d+0) 
term(170) = term(170) * 16.0d+0 
term(171) = term(171) * 8.0d+0 
term(172) = term(172) * (-8.0d+0) 

do a = nocc + 1, nactive 
do i = 1, nocc 
do b = nocc + 1, nactive 
term(173) = term(173) + r1(vrdav_Rl, a,i) * wmo_interm_5_triplet(b, i) * wmo_interm_58_triplet(b, a)
term(174) = term(174) + r1(vrdav_Rl, a,i) * wmo_interm_5_triplet(b, i) * wmo_interm_59_triplet(b, a)
term(175) = term(175) + r1(vrdav_Rl, a,i) * wmo_interm_6_triplet(b, i) * wmo_interm_58_triplet(b, a)
term(176) = term(176) + r1(vrdav_Rl, a,i) * wmo_interm_6_triplet(b, i) * wmo_interm_59_triplet(b, a)
term(177) = term(177) + r1(vrdav_Rl, a,i) * wmo_interm_7_triplet(b, i) * wmo_interm_58_triplet(b, a)
term(178) = term(178) + r1(vrdav_Rl, a,i) * wmo_interm_7_triplet(b, i) * wmo_interm_59_triplet(b, a)
end do 
end do 
end do 

term(173) = term(173) * 6.0d+0 
term(174) = term(174) * (-3.0d+0) 
term(175) = term(175) * (-8.0d+0) 
term(176) = term(176) * 4.0d+0 
term(177) = term(177) * 4.0d+0 
term(178) = term(178) * (-2.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(179) = term(179) + wmo_interm_77_triplet(b, c, i, k) * wmo_interm_70_triplet(c, b, k, i)
term(180) = term(180) + wmo_interm_77_triplet(b, c, i, k) * wmo_interm_71_triplet(c, b, k, i)
term(181) = term(181) + wmo_interm_77_triplet(b, c, i, k) * wmo_interm_72_triplet(c, b, k, i)
term(182) = term(182) + wmo_interm_77_triplet(b, c, i, k) * wmo_interm_73_triplet(c, b, k, i)
term(183) = term(183) + wmo_interm_80_triplet(b, c, i, k) * wmo_interm_70_triplet(c, b, k, i)
term(184) = term(184) + wmo_interm_81_triplet(b, c, i, k) * wmo_interm_70_triplet(c, b, k, i)
term(185) = term(185) + wmo_interm_81_triplet(b, c, i, k) * wmo_interm_71_triplet(c, b, k, i)
term(186) = term(186) + wmo_interm_80_triplet(b, c, i, k) * wmo_interm_71_triplet(c, b, k, i)
term(187) = term(187) + wmo_interm_81_triplet(b, c, i, k) * wmo_interm_72_triplet(c, b, k, i)
term(188) = term(188) + wmo_interm_80_triplet(b, c, i, k) * wmo_interm_72_triplet(c, b, k, i)
term(189) = term(189) + wmo_interm_81_triplet(b, c, i, k) * wmo_interm_73_triplet(c, b, k, i)
term(190) = term(190) + wmo_interm_80_triplet(b, c, i, k) * wmo_interm_73_triplet(c, b, k, i)
end do 
end do 
end do 
end do 

term(179) = term(179) * (-4.0d+0) 
term(180) = term(180) * 4.0d+0 
term(181) = term(181) * (-8.0d+0) 
term(182) = term(182) * 4.0d+0 
term(183) = term(183) * (-4.0d+0) 
term(184) = term(184) * 4.0d+0 
term(185) = term(185) * (-4.0d+0) 
term(186) = term(186) * 4.0d+0 
term(187) = term(187) * 8.0d+0 
term(188) = term(188) * (-8.0d+0) 
term(189) = term(189) * (-4.0d+0) 
term(190) = term(190) * 4.0d+0 

do k = 1, nocc 
do j = 1, nocc 
do c = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(191) = term(191) + wmo_interm_77_triplet(a, c, j, k) * wmo_interm_70_triplet(c, a, k, j)
term(192) = term(192) + wmo_interm_77_triplet(a, c, j, k) * wmo_interm_71_triplet(c, a, k, j)
term(193) = term(193) + wmo_interm_77_triplet(a, c, j, k) * wmo_interm_72_triplet(c, a, k, j)
term(194) = term(194) + wmo_interm_77_triplet(a, c, j, k) * wmo_interm_73_triplet(c, a, k, j)
term(195) = term(195) + wmo_interm_80_triplet(a, c, j, k) * wmo_interm_70_triplet(c, a, k, j)
term(196) = term(196) + wmo_interm_81_triplet(a, c, j, k) * wmo_interm_70_triplet(c, a, k, j)
term(197) = term(197) + wmo_interm_81_triplet(a, c, j, k) * wmo_interm_71_triplet(c, a, k, j)
term(198) = term(198) + wmo_interm_80_triplet(a, c, j, k) * wmo_interm_71_triplet(c, a, k, j)
term(199) = term(199) + wmo_interm_81_triplet(a, c, j, k) * wmo_interm_72_triplet(c, a, k, j)
term(200) = term(200) + wmo_interm_80_triplet(a, c, j, k) * wmo_interm_72_triplet(c, a, k, j)
term(201) = term(201) + wmo_interm_81_triplet(a, c, j, k) * wmo_interm_73_triplet(c, a, k, j)
term(202) = term(202) + wmo_interm_80_triplet(a, c, j, k) * wmo_interm_73_triplet(c, a, k, j)
end do 
end do 
end do 
end do 

term(191) = term(191) * (-4.0d+0) 
term(192) = term(192) * 4.0d+0 
term(193) = term(193) * (-8.0d+0) 
term(194) = term(194) * 4.0d+0 
term(195) = term(195) * (-4.0d+0) 
term(196) = term(196) * 4.0d+0 
term(197) = term(197) * (-4.0d+0) 
term(198) = term(198) * 4.0d+0 
term(199) = term(199) * 8.0d+0 
term(200) = term(200) * (-8.0d+0) 
term(201) = term(201) * (-4.0d+0) 
term(202) = term(202) * 4.0d+0 

do k = 1, nocc 
do c = nocc + 1, nactive 
term(203) = term(203) + wmo_interm_55_triplet(c, k) * wmo_interm_56_triplet(c, k)
end do 
end do 

term(203) = term(203) * 2.0d+0 

do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(204) = term(204) + wmo_interm_57_triplet(a, b) * wmo_interm_58_triplet(b, a)
term(205) = term(205) + wmo_interm_57_triplet(a, b) * wmo_interm_59_triplet(b, a)
end do 
end do 

term(204) = term(204) * (-4.0d+0) 
term(205) = term(205) * 2.0d+0 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(206) = term(206) + r1(vrdav_Rl, a,i) * wmo_interm_5_triplet(a, j) * wmo_interm_1_triplet(j, i)
term(207) = term(207) + r1(vrdav_Rl, a,i) * wmo_interm_6_triplet(a, j) * wmo_interm_1_triplet(j, i)
term(208) = term(208) + r1(vrdav_Rl, a,i) * wmo_interm_5_triplet(a, j) * wmo_interm_53_triplet(j, i)
term(209) = term(209) + r1(vrdav_Rl, a,i) * wmo_interm_5_triplet(a, j) * wmo_interm_54_triplet(j, i)
term(210) = term(210) + r1(vrdav_Rl, a,i) * wmo_interm_5_triplet(a, j) * wmo_interm_53_triplet(j, i)
term(211) = term(211) + r1(vrdav_Rl, a,i) * wmo_interm_5_triplet(a, j) * wmo_interm_54_triplet(j, i)
term(212) = term(212) + r1(vrdav_Rl, a,i) * wmo_interm_6_triplet(a, j) * wmo_interm_53_triplet(j, i)
term(213) = term(213) + r1(vrdav_Rl, a,i) * wmo_interm_6_triplet(a, j) * wmo_interm_54_triplet(j, i)
term(214) = term(214) + r1(vrdav_Rl, a,i) * wmo_interm_6_triplet(a, j) * wmo_interm_53_triplet(j, i)
term(215) = term(215) + r1(vrdav_Rl, a,i) * wmo_interm_6_triplet(a, j) * wmo_interm_54_triplet(j, i)
end do 
end do 
end do 

term(206) = term(206) * 4.0d+0 
term(207) = term(207) * (-4.0d+0) 
term(208) = term(208) * (-2.0d+0) 
term(209) = term(209) * 4.0d+0 
term(210) = term(210) * (-2.0d+0) 
term(211) = term(211) * 4.0d+0 
term(212) = term(212) * 2.0d+0 
term(213) = term(213) * (-4.0d+0) 
term(214) = term(214) * 2.0d+0 
term(215) = term(215) * (-4.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
term(216) = term(216) + s1(b,k) * wmo_interm_2_triplet(i, k) * wmo_interm_5_triplet(b, i)
term(217) = term(217) + s1(b,k) * wmo_interm_2_triplet(i, k) * wmo_interm_6_triplet(b, i)
term(218) = term(218) + s1(b,k) * wmo_interm_2_triplet(i, k) * wmo_interm_7_triplet(b, i)
end do 
end do 
end do 

term(216) = term(216) * 3.0d+0 
term(217) = term(217) * (-4.0d+0) 
term(218) = term(218) * 2.0d+0 

do k = 1, nocc 
do i = 1, nocc 
do c = nocc + 1, nactive 
term(219) = term(219) + s1(c,k) * wmo_interm_2_triplet(i, k) * wmo_interm_5_triplet(c, i)
term(220) = term(220) + s1(c,k) * wmo_interm_2_triplet(i, k) * wmo_interm_7_triplet(c, i)
end do 
end do 
end do 

term(219) = -term(219) 
term(220) = term(220) * 2.0d+0 

do k = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(221) = term(221) + r1(vrdav_Rl, a,i) * wmo_interm_7_triplet(a, k) * wmo_interm_1_triplet(k, i)
term(222) = term(222) + r1(vrdav_Rl, a,i) * wmo_interm_7_triplet(a, k) * wmo_interm_53_triplet(k, i)
term(223) = term(223) + r1(vrdav_Rl, a,i) * wmo_interm_7_triplet(a, k) * wmo_interm_54_triplet(k, i)
term(224) = term(224) + r1(vrdav_Rl, a,i) * wmo_interm_7_triplet(a, k) * wmo_interm_53_triplet(k, i)
term(225) = term(225) + r1(vrdav_Rl, a,i) * wmo_interm_7_triplet(a, k) * wmo_interm_54_triplet(k, i)
end do 
end do 
end do 

term(221) = term(221) * 4.0d+0 
term(222) = term(222) * (-2.0d+0) 
term(223) = term(223) * 4.0d+0 
term(224) = term(224) * (-2.0d+0) 
term(225) = term(225) * 4.0d+0 

do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(226) = term(226) + r2p(vrdav_Rl, a,i,b,j) * wmo_interm_5_triplet(b, j) * wmo_interm_40_triplet(a, i)
term(227) = term(227) + r2p(vrdav_Rl, a,i,b,j) * wmo_interm_5_triplet(b, j) * wmo_interm_41_triplet(a, i)
term(228) = term(228) + r2p(vrdav_Rl, a,i,b,j) * wmo_interm_6_triplet(b, j) * wmo_interm_40_triplet(a, i)
term(229) = term(229) + r2p(vrdav_Rl, a,i,b,j) * wmo_interm_7_triplet(b, j) * wmo_interm_40_triplet(a, i)
term(230) = term(230) + r2p(vrdav_Rl, a,i,b,j) * wmo_interm_6_triplet(b, j) * wmo_interm_41_triplet(a, i)
term(231) = term(231) + r2p(vrdav_Rl, a,i,b,j) * wmo_interm_7_triplet(b, j) * wmo_interm_41_triplet(a, i)
term(232) = term(232) + r2m(vrdav_Rl, a,i,b,j) * wmo_interm_5_triplet(a, i) * wmo_interm_40_triplet(b, j)
term(233) = term(233) + r2m(vrdav_Rl, a,i,b,j) * wmo_interm_5_triplet(a, i) * wmo_interm_41_triplet(b, j)
term(234) = term(234) + r2m(vrdav_Rl, a,i,b,j) * wmo_interm_5_triplet(a, j) * wmo_interm_40_triplet(b, i)
term(235) = term(235) + r2m(vrdav_Rl, a,i,b,j) * wmo_interm_5_triplet(a, j) * wmo_interm_41_triplet(b, i)
term(236) = term(236) + r2m(vrdav_Rl, a,i,b,j) * wmo_interm_5_triplet(b, i) * wmo_interm_40_triplet(a, j)
term(237) = term(237) + r2m(vrdav_Rl, a,i,b,j) * wmo_interm_5_triplet(b, i) * wmo_interm_41_triplet(a, j)
term(238) = term(238) + r2m(vrdav_Rl, a,i,b,j) * wmo_interm_6_triplet(a, i) * wmo_interm_40_triplet(b, j)
term(239) = term(239) + r2m(vrdav_Rl, a,i,b,j) * wmo_interm_7_triplet(a, i) * wmo_interm_40_triplet(b, j)
term(240) = term(240) + r2m(vrdav_Rl, a,i,b,j) * wmo_interm_6_triplet(a, i) * wmo_interm_41_triplet(b, j)
term(241) = term(241) + r2m(vrdav_Rl, a,i,b,j) * wmo_interm_7_triplet(a, i) * wmo_interm_41_triplet(b, j)
term(242) = term(242) + r2m(vrdav_Rl, a,i,b,j) * wmo_interm_6_triplet(a, j) * wmo_interm_40_triplet(b, i)
term(243) = term(243) + r2m(vrdav_Rl, a,i,b,j) * wmo_interm_7_triplet(a, j) * wmo_interm_40_triplet(b, i)
term(244) = term(244) + r2m(vrdav_Rl, a,i,b,j) * wmo_interm_6_triplet(a, j) * wmo_interm_41_triplet(b, i)
term(245) = term(245) + r2m(vrdav_Rl, a,i,b,j) * wmo_interm_7_triplet(a, j) * wmo_interm_41_triplet(b, i)
term(246) = term(246) + r2m(vrdav_Rl, a,i,b,j) * wmo_interm_6_triplet(b, i) * wmo_interm_40_triplet(a, j)
term(247) = term(247) + r2m(vrdav_Rl, a,i,b,j) * wmo_interm_7_triplet(b, i) * wmo_interm_40_triplet(a, j)
term(248) = term(248) + r2m(vrdav_Rl, a,i,b,j) * wmo_interm_6_triplet(b, i) * wmo_interm_41_triplet(a, j)
term(249) = term(249) + r2m(vrdav_Rl, a,i,b,j) * wmo_interm_7_triplet(b, i) * wmo_interm_41_triplet(a, j)
term(250) = term(250) + r2p(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, a,i,b,j)
term(251) = term(251) + r2p(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,i,b,j)
term(252) = term(252) + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, a,i,b,j)
term(253) = term(253) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,i,b,j)
term(254) = term(254) + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, a,i,b,j) * s1(b,j)
term(255) = term(255) + r2p(vrdav_Rl, a,i,b,j) * r1(vrdav_Rr, b,j) * t1(a,i)
term(256) = term(256) + r2m(vrdav_Rl, a,i,b,j) * r1(vrdav_Rr, a,i) * t1(b,j)
term(257) = term(257) + r2m(vrdav_Rl, a,i,b,j) * r1(vrdav_Rr, a,j) * t1(b,i)
term(258) = term(258) + r2m(vrdav_Rl, a,i,b,j) * r1(vrdav_Rr, b,i) * t1(a,j)
end do 
end do 
end do 
end do 

term(226) = term(226) * (-8.0d+0) 
term(227) = term(227) * 16.0d+0 
term(228) = term(228) * (-8.0d+0) 
term(229) = term(229) * 8.0d+0 
term(230) = term(230) * 16.0d+0 
term(231) = term(231) * (-16.0d+0) 
term(232) = term(232) * (-8.0d+0) 
term(233) = term(233) * 16.0d+0 
term(234) = term(234) * 4.0d+0 
term(235) = term(235) * (-8.0d+0) 
term(236) = term(236) * 4.0d+0 
term(237) = term(237) * (-8.0d+0) 
term(238) = term(238) * (-8.0d+0) 
term(239) = term(239) * 8.0d+0 
term(240) = term(240) * 16.0d+0 
term(241) = term(241) * (-16.0d+0) 
term(242) = term(242) * 4.0d+0 
term(243) = term(243) * (-4.0d+0) 
term(244) = term(244) * (-8.0d+0) 
term(245) = term(245) * 8.0d+0 
term(246) = term(246) * 4.0d+0 
term(247) = term(247) * (-4.0d+0) 
term(248) = term(248) * (-8.0d+0) 
term(249) = term(249) * 8.0d+0 
term(250) = term(250) * 2.0d+0 
term(251) = term(251) * 3.0d+0 
term(252) = term(252) * 4.0d+0 
term(253) = term(253) * 4.0d+0 
term(254) = term(254) * 4.0d+0 
term(255) = term(255) * (-4.0d+0) 
term(256) = term(256) * (-4.0d+0) 
term(257) = term(257) * 2.0d+0 
term(258) = term(258) * 2.0d+0 

do d = nocc + 1, nactive 
do k = 1, nocc 
do b = nocc + 1, nactive 
term(259) = term(259) + s1(d,k) * wmo_interm_42_triplet(b, d) * wmo_interm_5_triplet(b, k)
term(260) = term(260) + s1(d,k) * wmo_interm_44_triplet(b, d) * wmo_interm_5_triplet(b, k)
term(261) = term(261) + s1(d,k) * wmo_interm_42_triplet(b, d) * wmo_interm_6_triplet(b, k)
term(262) = term(262) + s1(d,k) * wmo_interm_44_triplet(b, d) * wmo_interm_6_triplet(b, k)
term(263) = term(263) + s1(d,k) * wmo_interm_48_triplet(b, d) * wmo_interm_5_triplet(b, k)
term(264) = term(264) + s1(d,k) * wmo_interm_48_triplet(b, d) * wmo_interm_6_triplet(b, k)
end do 
end do 
end do 

term(259) = term(259) * (-6.0d+0) 
term(260) = term(260) * 2.0d+0 
term(261) = term(261) * 6.0d+0 
term(262) = term(262) * (-2.0d+0) 
term(263) = term(263) * (-4.0d+0) 
term(264) = term(264) * 4.0d+0 

do l = 1, nocc 
do j = 1, nocc 
do c = nocc + 1, nactive 
term(265) = term(265) + s1(c,l) * wmo_interm_45_triplet(j, l) * wmo_interm_5_triplet(c, j)
term(266) = term(266) + s1(c,l) * wmo_interm_46_triplet(j, l) * wmo_interm_5_triplet(c, j)
term(267) = term(267) + s1(c,l) * wmo_interm_45_triplet(j, l) * wmo_interm_6_triplet(c, j)
term(268) = term(268) + s1(c,l) * wmo_interm_45_triplet(j, l) * wmo_interm_7_triplet(c, j)
term(269) = term(269) + s1(c,l) * wmo_interm_46_triplet(j, l) * wmo_interm_6_triplet(c, j)
term(270) = term(270) + s1(c,l) * wmo_interm_46_triplet(j, l) * wmo_interm_7_triplet(c, j)
term(271) = term(271) + s1(c,l) * wmo_interm_51_triplet(j, l) * wmo_interm_5_triplet(c, j)
term(272) = term(272) + s1(c,l) * wmo_interm_51_triplet(j, l) * wmo_interm_6_triplet(c, j)
term(273) = term(273) + s1(c,l) * wmo_interm_51_triplet(j, l) * wmo_interm_7_triplet(c, j)
end do 
end do 
end do 

term(265) = term(265) * 3.0d+0 
term(266) = term(266) * (-3.0d+0) 
term(267) = term(267) * (-4.0d+0) 
term(268) = term(268) * 2.0d+0 
term(269) = term(269) * 4.0d+0 
term(270) = term(270) * (-2.0d+0) 
term(271) = term(271) * (-3.0d+0) 
term(272) = term(272) * 4.0d+0 
term(273) = term(273) * (-2.0d+0) 

do l = 1, nocc 
do j = 1, nocc 
do d = nocc + 1, nactive 
term(274) = term(274) + s1(d,l) * wmo_interm_45_triplet(j, l) * wmo_interm_5_triplet(d, j)
term(275) = term(275) + s1(d,l) * wmo_interm_46_triplet(j, l) * wmo_interm_5_triplet(d, j)
term(276) = term(276) + s1(d,l) * wmo_interm_45_triplet(j, l) * wmo_interm_7_triplet(d, j)
term(277) = term(277) + s1(d,l) * wmo_interm_46_triplet(j, l) * wmo_interm_7_triplet(d, j)
term(278) = term(278) + s1(d,l) * wmo_interm_51_triplet(j, l) * wmo_interm_5_triplet(d, j)
term(279) = term(279) + s1(d,l) * wmo_interm_51_triplet(j, l) * wmo_interm_7_triplet(d, j)
end do 
end do 
end do 

term(274) = -term(274) 
term(276) = term(276) * 2.0d+0 
term(277) = term(277) * (-2.0d+0) 
term(279) = term(279) * (-2.0d+0) 

do l = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
term(280) = term(280) + wmo_interm_60_triplet(i, j, k, l) * wmo_interm_61_triplet(k, l, i, j)
term(281) = term(281) + wmo_interm_60_triplet(i, j, k, l) * wmo_interm_61_triplet(l, k, i, j)
term(282) = term(282) + wmo_interm_64_triplet(i, j, k, l) * wmo_interm_61_triplet(l, k, i, j)
term(283) = term(283) + wmo_interm_64_triplet(i, j, k, l) * wmo_interm_61_triplet(k, l, i, j)
term(284) = term(284) + wmo_interm_66_triplet(i, j, k, l) * wmo_interm_61_triplet(k, l, i, j)
term(285) = term(285) + wmo_interm_66_triplet(i, j, k, l) * wmo_interm_61_triplet(l, k, i, j)
term(286) = term(286) + wmo_interm_68_triplet(i, j, k, l) * wmo_interm_61_triplet(l, k, i, j)
term(287) = term(287) + wmo_interm_68_triplet(i, j, k, l) * wmo_interm_61_triplet(k, l, i, j)
end do 
end do 
end do 
end do 

term(280) = term(280) * 0.5d+0 
term(281) = term(281) * (-0.5d+0) 
term(282) = term(282) * (-0.5d+0) 
term(283) = term(283) * 1.5d+0 
term(285) = -term(285) 
term(286) = term(286) * (-2.0d+0) 
term(287) = term(287) * 2.0d+0 

do d = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(288) = term(288) + wmo_interm_42_triplet(b, d) * wmo_interm_30_triplet(b, d)
term(289) = term(289) + wmo_interm_42_triplet(b, d) * wmo_interm_31_triplet(b, d)
term(290) = term(290) + wmo_interm_44_triplet(b, d) * wmo_interm_30_triplet(b, d)
term(291) = term(291) + wmo_interm_44_triplet(b, d) * wmo_interm_31_triplet(b, d)
term(292) = term(292) + wmo_interm_42_triplet(b, d) * wmo_interm_34_triplet(b, d)
term(293) = term(293) + wmo_interm_42_triplet(b, d) * wmo_interm_35_triplet(b, d)
term(294) = term(294) + wmo_interm_44_triplet(b, d) * wmo_interm_34_triplet(b, d)
term(295) = term(295) + wmo_interm_44_triplet(b, d) * wmo_interm_35_triplet(b, d)
term(296) = term(296) + wmo_interm_48_triplet(b, d) * wmo_interm_30_triplet(b, d)
term(297) = term(297) + wmo_interm_48_triplet(b, d) * wmo_interm_31_triplet(b, d)
term(298) = term(298) + wmo_interm_48_triplet(b, d) * wmo_interm_34_triplet(b, d)
term(299) = term(299) + wmo_interm_48_triplet(b, d) * wmo_interm_35_triplet(b, d)
end do 
end do 

term(288) = term(288) * (-3.0d+0) 
term(289) = term(289) * 3.0d+0 
term(291) = -term(291) 
term(292) = term(292) * (-6.0d+0) 
term(293) = term(293) * 6.0d+0 
term(294) = term(294) * 2.0d+0 
term(295) = term(295) * (-2.0d+0) 
term(296) = term(296) * (-2.0d+0) 
term(297) = term(297) * 2.0d+0 
term(298) = term(298) * (-4.0d+0) 
term(299) = term(299) * 4.0d+0 

do l = 1, nocc 
do j = 1, nocc 
term(300) = term(300) + wmo_interm_45_triplet(j, l) * wmo_interm_32_triplet(j, l)
term(301) = term(301) + wmo_interm_45_triplet(j, l) * wmo_interm_33_triplet(j, l)
term(302) = term(302) + wmo_interm_46_triplet(j, l) * wmo_interm_32_triplet(j, l)
term(303) = term(303) + wmo_interm_46_triplet(j, l) * wmo_interm_33_triplet(j, l)
term(304) = term(304) + wmo_interm_45_triplet(j, l) * wmo_interm_36_triplet(j, l)
term(305) = term(305) + wmo_interm_45_triplet(j, l) * wmo_interm_37_triplet(j, l)
term(306) = term(306) + wmo_interm_45_triplet(j, l) * wmo_interm_38_triplet(j, l)
term(307) = term(307) + wmo_interm_46_triplet(j, l) * wmo_interm_36_triplet(j, l)
term(308) = term(308) + wmo_interm_46_triplet(j, l) * wmo_interm_37_triplet(j, l)
term(309) = term(309) + wmo_interm_46_triplet(j, l) * wmo_interm_38_triplet(j, l)
term(310) = term(310) + wmo_interm_51_triplet(j, l) * wmo_interm_32_triplet(j, l)
term(311) = term(311) + wmo_interm_51_triplet(j, l) * wmo_interm_33_triplet(j, l)
term(312) = term(312) + wmo_interm_51_triplet(j, l) * wmo_interm_36_triplet(j, l)
term(313) = term(313) + wmo_interm_51_triplet(j, l) * wmo_interm_37_triplet(j, l)
term(314) = term(314) + wmo_interm_51_triplet(j, l) * wmo_interm_38_triplet(j, l)
end do 
end do 

term(300) = -term(300) 
term(301) = term(301) * 3.0d+0 
term(303) = term(303) * (-3.0d+0) 
term(304) = term(304) * 2.0d+0 
term(305) = term(305) * 2.0d+0 
term(306) = term(306) * (-4.0d+0) 
term(307) = term(307) * (-2.0d+0) 
term(308) = term(308) * (-2.0d+0) 
term(309) = term(309) * 4.0d+0 
term(311) = term(311) * (-3.0d+0) 
term(312) = term(312) * (-2.0d+0) 
term(313) = term(313) * (-2.0d+0) 
term(314) = term(314) * 4.0d+0 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(315) = term(315) + r2p(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,j,b,i)
term(316) = term(316) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,j,b,i)
end do 
end do 
end do 
end do 

term(315) = -term(315) 
term(316) = term(316) * (-4.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(317) = term(317) + wmo_interm_62_triplet(i, j, l, k) * wmo_interm_63_triplet(i, j, k, l)
term(318) = term(318) + wmo_interm_62_triplet(i, j, k, l) * wmo_interm_63_triplet(i, j, k, l)
term(319) = term(319) + wmo_interm_62_triplet(i, j, l, k) * wmo_interm_65_triplet(i, j, k, l)
term(320) = term(320) + wmo_interm_62_triplet(i, j, k, l) * wmo_interm_65_triplet(j, i, k, l)
term(321) = term(321) + wmo_interm_62_triplet(i, j, k, l) * wmo_interm_65_triplet(i, j, k, l)
term(322) = term(322) + wmo_interm_62_triplet(i, j, l, k) * wmo_interm_65_triplet(j, i, k, l)
term(323) = term(323) + wmo_interm_67_triplet(i, j, l, k) * wmo_interm_63_triplet(i, j, k, l)
term(324) = term(324) + wmo_interm_67_triplet(i, j, k, l) * wmo_interm_63_triplet(i, j, k, l)
term(325) = term(325) + wmo_interm_67_triplet(i, j, l, k) * wmo_interm_65_triplet(i, j, k, l)
term(326) = term(326) + wmo_interm_67_triplet(i, j, k, l) * wmo_interm_65_triplet(j, i, k, l)
term(327) = term(327) + wmo_interm_67_triplet(i, j, k, l) * wmo_interm_65_triplet(i, j, k, l)
end do 
end do 
end do 
end do 

term(317) = -term(317) 
term(319) = term(319) * (-1.5d+0) 
term(320) = term(320) * (-0.5d+0) 
term(321) = term(321) * 1.5d+0 
term(322) = term(322) * 0.5d+0 
term(323) = -term(323) 
term(324) = term(324) * 3.0d+0 
term(325) = term(325) * (-2.0d+0) 
term(326) = term(326) * (-2.0d+0) 
term(327) = term(327) * 4.0d+0 

do l = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
term(328) = term(328) + wmo_interm_60_triplet(i, j, k, l) * wmo_interm_61_triplet(k, l, j, i)
term(329) = term(329) + wmo_interm_60_triplet(i, j, k, l) * wmo_interm_61_triplet(l, k, j, i)
term(330) = term(330) + wmo_interm_64_triplet(i, j, k, l) * wmo_interm_61_triplet(k, l, j, i)
term(331) = term(331) + wmo_interm_64_triplet(i, j, k, l) * wmo_interm_61_triplet(l, k, j, i)
term(332) = term(332) + wmo_interm_66_triplet(i, j, k, l) * wmo_interm_61_triplet(k, l, j, i)
term(333) = term(333) + wmo_interm_66_triplet(i, j, k, l) * wmo_interm_61_triplet(l, k, j, i)
term(334) = term(334) + wmo_interm_68_triplet(i, j, k, l) * wmo_interm_61_triplet(k, l, j, i)
term(335) = term(335) + wmo_interm_68_triplet(i, j, k, l) * wmo_interm_61_triplet(l, k, j, i)
end do 
end do 
end do 
end do 

term(328) = term(328) * (-0.5d+0) 
term(329) = term(329) * 0.5d+0 
term(330) = term(330) * (-0.5d+0) 
term(331) = term(331) * 1.5d+0 
term(332) = -term(332) 
term(334) = term(334) * (-2.0d+0) 
term(335) = term(335) * 2.0d+0 

do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(336) = term(336) + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, a,j,b,i) * s1(b,j)
end do 
end do 
end do 
end do 

term(336) = term(336) * (-4.0d+0) 

do d = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(337) = term(337) + s1(d,k) * wmo_interm_50_triplet(a, d) * wmo_interm_5_triplet(a, k)
term(338) = term(338) + s1(d,k) * wmo_interm_48_triplet(a, d) * wmo_interm_5_triplet(a, k)
term(339) = term(339) + s1(d,k) * wmo_interm_50_triplet(a, d) * wmo_interm_6_triplet(a, k)
term(340) = term(340) + s1(d,k) * wmo_interm_48_triplet(a, d) * wmo_interm_6_triplet(a, k)
end do 
end do 
end do 

term(337) = term(337) * 8.0d+0 
term(338) = term(338) * (-4.0d+0) 
term(339) = term(339) * (-8.0d+0) 
term(340) = term(340) * 4.0d+0 

do l = 1, nocc 
do i = 1, nocc 
do c = nocc + 1, nactive 
term(341) = term(341) + s1(c,l) * wmo_interm_52_triplet(i, l) * wmo_interm_5_triplet(c, i)
term(342) = term(342) + s1(c,l) * wmo_interm_51_triplet(i, l) * wmo_interm_5_triplet(c, i)
term(343) = term(343) + s1(c,l) * wmo_interm_52_triplet(i, l) * wmo_interm_6_triplet(c, i)
term(344) = term(344) + s1(c,l) * wmo_interm_52_triplet(i, l) * wmo_interm_7_triplet(c, i)
term(345) = term(345) + s1(c,l) * wmo_interm_51_triplet(i, l) * wmo_interm_6_triplet(c, i)
term(346) = term(346) + s1(c,l) * wmo_interm_51_triplet(i, l) * wmo_interm_7_triplet(c, i)
end do 
end do 
end do 

term(341) = term(341) * 6.0d+0 
term(342) = term(342) * (-3.0d+0) 
term(343) = term(343) * (-8.0d+0) 
term(344) = term(344) * 4.0d+0 
term(345) = term(345) * 4.0d+0 
term(346) = term(346) * (-2.0d+0) 

do l = 1, nocc 
do i = 1, nocc 
do d = nocc + 1, nactive 
term(347) = term(347) + s1(d,l) * wmo_interm_52_triplet(i, l) * wmo_interm_5_triplet(d, i)
term(348) = term(348) + s1(d,l) * wmo_interm_51_triplet(i, l) * wmo_interm_5_triplet(d, i)
term(349) = term(349) + s1(d,l) * wmo_interm_52_triplet(i, l) * wmo_interm_7_triplet(d, i)
term(350) = term(350) + s1(d,l) * wmo_interm_51_triplet(i, l) * wmo_interm_7_triplet(d, i)
end do 
end do 
end do 

term(347) = term(347) * (-2.0d+0) 
term(349) = term(349) * 4.0d+0 
term(350) = term(350) * (-2.0d+0) 

do d = nocc + 1, nactive 
do l = 1, nocc 
do a = nocc + 1, nactive 
term(351) = term(351) + s1(d,l) * wmo_interm_50_triplet(a, d) * wmo_interm_7_triplet(a, l)
term(352) = term(352) + s1(d,l) * wmo_interm_48_triplet(a, d) * wmo_interm_7_triplet(a, l)
end do 
end do 
end do 

term(351) = term(351) * 8.0d+0 
term(352) = term(352) * (-4.0d+0) 

do d = nocc + 1, nactive 
do l = 1, nocc 
do b = nocc + 1, nactive 
term(353) = term(353) + s1(d,l) * wmo_interm_42_triplet(b, d) * wmo_interm_7_triplet(b, l)
term(354) = term(354) + s1(d,l) * wmo_interm_44_triplet(b, d) * wmo_interm_7_triplet(b, l)
term(355) = term(355) + s1(d,l) * wmo_interm_48_triplet(b, d) * wmo_interm_7_triplet(b, l)
end do 
end do 
end do 

term(353) = term(353) * (-6.0d+0) 
term(354) = term(354) * 2.0d+0 
term(355) = term(355) * (-4.0d+0) 


    overlap_f_triplet = 0.d+0 
    do s = 0, 355
    overlap_f_triplet = overlap_f_triplet + term(s)
    end do

  end function overlap_f_triplet


      subroutine wm_triplet_intermediates_ccsd_init(nocc, nactive)
    integer, intent(in) :: nocc
    integer, intent(in) :: nactive
    allocate(wm_interm_0_triplet(nocc+1: nactive, 1: nocc))
allocate(wm_interm_1_triplet(nocc+1: nactive, 1: nocc))
allocate(wm_interm_2_triplet(nocc+1: nactive, 1: nocc))
allocate(wm_interm_3_triplet(nocc+1: nactive, 1: nocc))
allocate(wm_interm_4_triplet(nocc+1: nactive, 1: nocc))
allocate(wm_interm_5_triplet(nocc+1: nactive, 1: nocc))
allocate(wm_interm_6_triplet(nocc+1: nactive, 1: nocc))
allocate(wm_interm_7_triplet(nocc+1: nactive, 1: nocc))
allocate(wm_interm_8_triplet(nocc+1: nactive, 1: nocc))
allocate(wm_interm_9_triplet(nocc+1: nactive, 1: nocc))
allocate(wm_interm_10_triplet(nocc+1: nactive, 1: nocc))
allocate(wm_interm_11_triplet(nocc+1: nactive, 1: nocc))
allocate(wm_interm_12_triplet(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_14_triplet(1: nocc, 1: nocc))
allocate(wm_interm_15_triplet(1: nocc, 1: nocc))
allocate(wm_interm_16_triplet(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_17_triplet(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_18_triplet(1: nocc, 1: nocc))
allocate(wm_interm_19_triplet(1: nocc, 1: nocc))
allocate(wm_interm_20_triplet(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_21_triplet(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_22_triplet(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_23_triplet(1: nocc, 1: nocc))
allocate(wm_interm_24_triplet(1: nocc, 1: nocc))
allocate(wm_interm_25_triplet(1: nocc, 1: nocc))
allocate(wm_interm_26_triplet(1: nocc, 1: nocc))
allocate(wm_interm_27_triplet(1: nocc, 1: nocc))
allocate(wm_interm_29_triplet(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_30_triplet(1: nocc, 1: nocc))
allocate(wm_interm_31_triplet(1: nocc, 1: nocc))
allocate(wm_interm_34_triplet(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_35_triplet(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_36_triplet(1: nocc, 1: nocc))
allocate(wm_interm_37_triplet(1: nocc, 1: nocc))
allocate(wm_interm_38_triplet(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_39_triplet(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_40_triplet(1: nocc, 1: nocc))
allocate(wm_interm_41_triplet(1: nocc, 1: nocc))
allocate(wm_interm_43_triplet(1: nocc, 1: nocc))
allocate(wm_interm_44_triplet(1: nocc, 1: nocc))
allocate(wm_interm_45_triplet(1: nocc, 1: nocc))
allocate(wm_interm_46_triplet(1: nocc, 1: nocc))
allocate(wm_interm_49_triplet(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_50_triplet(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_51_triplet(1: nocc, 1: nocc))
allocate(wm_interm_52_triplet(1: nocc, 1: nocc))
allocate(wm_interm_53_triplet(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_54_triplet(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_55_triplet(1: nocc, 1: nocc))
allocate(wm_interm_56_triplet(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_57_triplet(nocc+1: nactive, 1: nocc))
allocate(wm_interm_58_triplet(nocc+1: nactive, 1: nocc))
allocate(wm_interm_59_triplet(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_60_triplet(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_61_triplet(nocc+1: nactive, 1: nocc))
allocate(wm_interm_62_triplet(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_63_triplet(nocc+1: nactive, 1: nocc))
allocate(wm_interm_64_triplet(nocc+1: nactive, 1: nocc))
allocate(wm_interm_65_triplet(nocc+1: nactive, 1: nocc))
allocate(wm_interm_66_triplet(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_67_triplet(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_68_triplet(1: nocc, 1: nocc))
allocate(wm_interm_69_triplet(1: nocc, 1: nocc))
allocate(wm_interm_70_triplet(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_71_triplet(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_72_triplet(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_73_triplet(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_74_triplet(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_75_triplet(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_76_triplet(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_77_triplet(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_78_triplet(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_79_triplet(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_80_triplet(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_81_triplet(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_82_triplet(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_83_triplet(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_84_triplet(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_85_triplet(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_86_triplet(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_87_triplet(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_88_triplet(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_89_triplet(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_90_triplet(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_91_triplet(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_92_triplet(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_93_triplet(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_94_triplet(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_95_triplet(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_96_triplet(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_97_triplet(nocc+1: nactive, nocc+1: nactive))
wm_interm_0_triplet = zero 
wm_interm_1_triplet = zero 
wm_interm_2_triplet = zero 
wm_interm_3_triplet = zero 
wm_interm_4_triplet = zero 
wm_interm_5_triplet = zero 
wm_interm_6_triplet = zero 
wm_interm_7_triplet = zero 
wm_interm_8_triplet = zero 
wm_interm_9_triplet = zero 
wm_interm_10_triplet = zero 
wm_interm_11_triplet = zero 
wm_interm_12_triplet = zero 
wm_interm_13_triplet = zero 
wm_interm_14_triplet = zero 
wm_interm_15_triplet = zero 
wm_interm_16_triplet = zero 
wm_interm_17_triplet = zero 
wm_interm_18_triplet = zero 
wm_interm_19_triplet = zero 
wm_interm_20_triplet = zero 
wm_interm_21_triplet = zero 
wm_interm_22_triplet = zero 
wm_interm_23_triplet = zero 
wm_interm_24_triplet = zero 
wm_interm_25_triplet = zero 
wm_interm_26_triplet = zero 
wm_interm_27_triplet = zero 
wm_interm_28_triplet = zero 
wm_interm_29_triplet = zero 
wm_interm_30_triplet = zero 
wm_interm_31_triplet = zero 
wm_interm_32_triplet = zero 
wm_interm_33_triplet = zero 
wm_interm_34_triplet = zero 
wm_interm_35_triplet = zero 
wm_interm_36_triplet = zero 
wm_interm_37_triplet = zero 
wm_interm_38_triplet = zero 
wm_interm_39_triplet = zero 
wm_interm_40_triplet = zero 
wm_interm_41_triplet = zero 
wm_interm_42_triplet = zero 
wm_interm_43_triplet = zero 
wm_interm_44_triplet = zero 
wm_interm_45_triplet = zero 
wm_interm_46_triplet = zero 
wm_interm_47_triplet = zero 
wm_interm_48_triplet = zero 
wm_interm_49_triplet = zero 
wm_interm_50_triplet = zero 
wm_interm_51_triplet = zero 
wm_interm_52_triplet = zero 
wm_interm_53_triplet = zero 
wm_interm_54_triplet = zero 
wm_interm_55_triplet = zero 
wm_interm_56_triplet = zero 
wm_interm_57_triplet = zero 
wm_interm_58_triplet = zero 
wm_interm_59_triplet = zero 
wm_interm_60_triplet = zero 
wm_interm_61_triplet = zero 
wm_interm_62_triplet = zero 
wm_interm_63_triplet = zero 
wm_interm_64_triplet = zero 
wm_interm_65_triplet = zero 
wm_interm_66_triplet = zero 
wm_interm_67_triplet = zero 
wm_interm_68_triplet = zero 
wm_interm_69_triplet = zero 
wm_interm_70_triplet = zero 
wm_interm_71_triplet = zero 
wm_interm_72_triplet = zero 
wm_interm_73_triplet = zero 
wm_interm_74_triplet = zero 
wm_interm_75_triplet = zero 
wm_interm_76_triplet = zero 
wm_interm_77_triplet = zero 
wm_interm_78_triplet = zero 
wm_interm_79_triplet = zero 
wm_interm_80_triplet = zero 
wm_interm_81_triplet = zero 
wm_interm_82_triplet = zero 
wm_interm_83_triplet = zero 
wm_interm_84_triplet = zero 
wm_interm_85_triplet = zero 
wm_interm_86_triplet = zero 
wm_interm_87_triplet = zero 
wm_interm_88_triplet = zero 
wm_interm_89_triplet = zero 
wm_interm_90_triplet = zero 
wm_interm_91_triplet = zero 
wm_interm_92_triplet = zero 
wm_interm_93_triplet = zero 
wm_interm_94_triplet = zero 
wm_interm_95_triplet = zero 
wm_interm_96_triplet = zero 
wm_interm_97_triplet = zero 

    end subroutine wm_triplet_intermediates_ccsd_init
    
    subroutine wm_triplet_intermediates_ccsd_free
    deallocate(wm_interm_0_triplet)
deallocate(wm_interm_1_triplet)
deallocate(wm_interm_2_triplet)
deallocate(wm_interm_3_triplet)
deallocate(wm_interm_4_triplet)
deallocate(wm_interm_5_triplet)
deallocate(wm_interm_6_triplet)
deallocate(wm_interm_7_triplet)
deallocate(wm_interm_8_triplet)
deallocate(wm_interm_9_triplet)
deallocate(wm_interm_10_triplet)
deallocate(wm_interm_11_triplet)
deallocate(wm_interm_12_triplet)
deallocate(wm_interm_14_triplet)
deallocate(wm_interm_15_triplet)
deallocate(wm_interm_16_triplet)
deallocate(wm_interm_17_triplet)
deallocate(wm_interm_18_triplet)
deallocate(wm_interm_19_triplet)
deallocate(wm_interm_20_triplet)
deallocate(wm_interm_21_triplet)
deallocate(wm_interm_22_triplet)
deallocate(wm_interm_23_triplet)
deallocate(wm_interm_24_triplet)
deallocate(wm_interm_25_triplet)
deallocate(wm_interm_26_triplet)
deallocate(wm_interm_27_triplet)
deallocate(wm_interm_29_triplet)
deallocate(wm_interm_30_triplet)
deallocate(wm_interm_31_triplet)
deallocate(wm_interm_34_triplet)
deallocate(wm_interm_35_triplet)
deallocate(wm_interm_36_triplet)
deallocate(wm_interm_37_triplet)
deallocate(wm_interm_38_triplet)
deallocate(wm_interm_39_triplet)
deallocate(wm_interm_40_triplet)
deallocate(wm_interm_41_triplet)
deallocate(wm_interm_43_triplet)
deallocate(wm_interm_44_triplet)
deallocate(wm_interm_45_triplet)
deallocate(wm_interm_46_triplet)
deallocate(wm_interm_49_triplet)
deallocate(wm_interm_50_triplet)
deallocate(wm_interm_51_triplet)
deallocate(wm_interm_52_triplet)
deallocate(wm_interm_53_triplet)
deallocate(wm_interm_54_triplet)
deallocate(wm_interm_55_triplet)
deallocate(wm_interm_56_triplet)
deallocate(wm_interm_57_triplet)
deallocate(wm_interm_58_triplet)
deallocate(wm_interm_59_triplet)
deallocate(wm_interm_60_triplet)
deallocate(wm_interm_61_triplet)
deallocate(wm_interm_62_triplet)
deallocate(wm_interm_63_triplet)
deallocate(wm_interm_64_triplet)
deallocate(wm_interm_65_triplet)
deallocate(wm_interm_66_triplet)
deallocate(wm_interm_67_triplet)
deallocate(wm_interm_68_triplet)
deallocate(wm_interm_69_triplet)
deallocate(wm_interm_70_triplet)
deallocate(wm_interm_71_triplet)
deallocate(wm_interm_72_triplet)
deallocate(wm_interm_73_triplet)
deallocate(wm_interm_74_triplet)
deallocate(wm_interm_75_triplet)
deallocate(wm_interm_76_triplet)
deallocate(wm_interm_77_triplet)
deallocate(wm_interm_78_triplet)
deallocate(wm_interm_79_triplet)
deallocate(wm_interm_80_triplet)
deallocate(wm_interm_81_triplet)
deallocate(wm_interm_82_triplet)
deallocate(wm_interm_83_triplet)
deallocate(wm_interm_84_triplet)
deallocate(wm_interm_85_triplet)
deallocate(wm_interm_86_triplet)
deallocate(wm_interm_87_triplet)
deallocate(wm_interm_88_triplet)
deallocate(wm_interm_89_triplet)
deallocate(wm_interm_90_triplet)
deallocate(wm_interm_91_triplet)
deallocate(wm_interm_92_triplet)
deallocate(wm_interm_93_triplet)
deallocate(wm_interm_94_triplet)
deallocate(wm_interm_95_triplet)
deallocate(wm_interm_96_triplet)
deallocate(wm_interm_97_triplet)

    end subroutine wm_triplet_intermediates_ccsd_free
    
    subroutine wm_triplet_intermediates_ccsd(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr)
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    double precision, dimension(:), intent(in) :: vrdav_Rl
    double precision, dimension(:), intent(in) :: vrdav_Rr
    real(F64) :: sum
    integer :: a, i, b, j, c, k, l 

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
wm_interm_0_triplet(b, j) = wm_interm_0_triplet(b, j) + sum 
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
wm_interm_1_triplet(b, j) = wm_interm_1_triplet(b, j) + sum 
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
wm_interm_2_triplet(b, j) = wm_interm_2_triplet(b, j) + sum 
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
wm_interm_3_triplet(b, j) = wm_interm_3_triplet(b, j) + sum 
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
wm_interm_4_triplet(b, j) = wm_interm_4_triplet(b, j) + sum 
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
wm_interm_5_triplet(b, j) = wm_interm_5_triplet(b, j) + sum 
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
wm_interm_6_triplet(b, j) = wm_interm_6_triplet(b, j) + sum 
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
wm_interm_7_triplet(b, j) = wm_interm_7_triplet(b, j) + sum 
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
wm_interm_8_triplet(b, j) = wm_interm_8_triplet(b, j) + sum 
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
wm_interm_9_triplet(b, j) = wm_interm_9_triplet(b, j) + sum 
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
wm_interm_10_triplet(b, j) = wm_interm_10_triplet(b, j) + sum 
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
wm_interm_11_triplet(b, j) = wm_interm_11_triplet(b, j) + sum 
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
wm_interm_12_triplet(a, b) = wm_interm_12_triplet(a, b) + sum 
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
wm_interm_13_triplet = wm_interm_13_triplet + sum 
!$omp parallel private(a, i, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do i = 1, nocc 
do j = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rr, a,i) * s1(a,j)
end do 
wm_interm_14_triplet(i, j) = wm_interm_14_triplet(i, j) + sum 
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
wm_interm_15_triplet(i, j) = wm_interm_15_triplet(i, j) + sum 
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
wm_interm_16_triplet(a, b) = wm_interm_16_triplet(a, b) + sum 
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
wm_interm_17_triplet(b, c) = wm_interm_17_triplet(b, c) + sum 
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
wm_interm_18_triplet(j, k) = wm_interm_18_triplet(j, k) + sum 
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
wm_interm_19_triplet(j, k) = wm_interm_19_triplet(j, k) + sum 
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
wm_interm_20_triplet(b, c) = wm_interm_20_triplet(b, c) + sum 
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
wm_interm_21_triplet(b, c) = wm_interm_21_triplet(b, c) + sum 
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
wm_interm_22_triplet(b, c) = wm_interm_22_triplet(b, c) + sum 
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
wm_interm_23_triplet(j, k) = wm_interm_23_triplet(j, k) + sum 
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
wm_interm_24_triplet(j, k) = wm_interm_24_triplet(j, k) + sum 
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
wm_interm_25_triplet(j, k) = wm_interm_25_triplet(j, k) + sum 
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
wm_interm_26_triplet(j, k) = wm_interm_26_triplet(j, k) + sum 
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
wm_interm_27_triplet(j, k) = wm_interm_27_triplet(j, k) + sum 
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
wm_interm_28_triplet = wm_interm_28_triplet + sum 
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
wm_interm_29_triplet(b, i, j, k) = wm_interm_29_triplet(b, i, j, k) + sum 
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
wm_interm_30_triplet(j, k) = wm_interm_30_triplet(j, k) + sum 
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
wm_interm_31_triplet(j, k) = wm_interm_31_triplet(j, k) + sum 
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
wm_interm_32_triplet = wm_interm_32_triplet + sum 
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
wm_interm_33_triplet = wm_interm_33_triplet + sum 
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
wm_interm_34_triplet(b, j, i, k) = wm_interm_34_triplet(b, j, i, k) + sum 
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
wm_interm_35_triplet(b, i, j, k) = wm_interm_35_triplet(b, i, j, k) + sum 
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
wm_interm_36_triplet(j, k) = wm_interm_36_triplet(j, k) + sum 
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
wm_interm_37_triplet(j, k) = wm_interm_37_triplet(j, k) + sum 
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
wm_interm_38_triplet(b, c) = wm_interm_38_triplet(b, c) + sum 
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
wm_interm_39_triplet(b, c) = wm_interm_39_triplet(b, c) + sum 
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
wm_interm_40_triplet(j, k) = wm_interm_40_triplet(j, k) + sum 
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
wm_interm_41_triplet(j, k) = wm_interm_41_triplet(j, k) + sum 
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
wm_interm_42_triplet = wm_interm_42_triplet + sum 
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
wm_interm_43_triplet(j, k) = wm_interm_43_triplet(j, k) + sum 
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
wm_interm_44_triplet(j, k) = wm_interm_44_triplet(j, k) + sum 
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
wm_interm_45_triplet(j, k) = wm_interm_45_triplet(j, k) + sum 
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
wm_interm_46_triplet(j, k) = wm_interm_46_triplet(j, k) + sum 
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
wm_interm_47_triplet = wm_interm_47_triplet + sum 
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
wm_interm_48_triplet = wm_interm_48_triplet + sum 
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
wm_interm_49_triplet(b, i, j, k) = wm_interm_49_triplet(b, i, j, k) + sum 
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
wm_interm_50_triplet(b, j, i, k) = wm_interm_50_triplet(b, j, i, k) + sum 
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
wm_interm_51_triplet(j, k) = wm_interm_51_triplet(j, k) + sum 
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
wm_interm_52_triplet(j, k) = wm_interm_52_triplet(j, k) + sum 
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
wm_interm_53_triplet(b, c) = wm_interm_53_triplet(b, c) + sum 
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
wm_interm_54_triplet(b, c) = wm_interm_54_triplet(b, c) + sum 
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
wm_interm_55_triplet(i, j) = wm_interm_55_triplet(i, j) + sum 
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
wm_interm_56_triplet(b, j, i, k) = wm_interm_56_triplet(b, j, i, k) + sum 
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
wm_interm_57_triplet(b, j) = wm_interm_57_triplet(b, j) + sum 
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
wm_interm_58_triplet(b, j) = wm_interm_58_triplet(b, j) + sum 
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
wm_interm_59_triplet(b, i, j, k) = wm_interm_59_triplet(b, i, j, k) + sum 
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
wm_interm_60_triplet(b, i, j, k) = wm_interm_60_triplet(b, i, j, k) + sum 
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
wm_interm_61_triplet(b, j) = wm_interm_61_triplet(b, j) + sum 
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
wm_interm_62_triplet(a, b) = wm_interm_62_triplet(a, b) + sum 
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
wm_interm_63_triplet(b, j) = wm_interm_63_triplet(b, j) + sum 
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
wm_interm_64_triplet(b, j) = wm_interm_64_triplet(b, j) + sum 
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
wm_interm_65_triplet(b, j) = wm_interm_65_triplet(b, j) + sum 
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
wm_interm_66_triplet(b, i, j, k) = wm_interm_66_triplet(b, i, j, k) + sum 
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
wm_interm_67_triplet(b, i, j, k) = wm_interm_67_triplet(b, i, j, k) + sum 
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
wm_interm_68_triplet(j, k) = wm_interm_68_triplet(j, k) + sum 
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
wm_interm_69_triplet(j, k) = wm_interm_69_triplet(j, k) + sum 
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
wm_interm_70_triplet(b, c) = wm_interm_70_triplet(b, c) + sum 
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
wm_interm_71_triplet(b, c) = wm_interm_71_triplet(b, c) + sum 
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
wm_interm_72_triplet(b, i, j, k) = wm_interm_72_triplet(b, i, j, k) + sum 
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
wm_interm_73_triplet(i, j, k, l) = wm_interm_73_triplet(i, j, k, l) + sum 
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
wm_interm_74_triplet(i, j, k, l) = wm_interm_74_triplet(i, j, k, l) + sum 
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
wm_interm_75_triplet(b, c) = wm_interm_75_triplet(b, c) + sum 
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
wm_interm_76_triplet(b, c) = wm_interm_76_triplet(b, c) + sum 
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
wm_interm_77_triplet(i, j, k, l) = wm_interm_77_triplet(i, j, k, l) + sum 
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
wm_interm_78_triplet(i, j, k, l) = wm_interm_78_triplet(i, j, k, l) + sum 
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
wm_interm_79_triplet(i, j, k, l) = wm_interm_79_triplet(i, j, k, l) + sum 
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
wm_interm_80_triplet(b, i, j, k) = wm_interm_80_triplet(b, i, j, k) + sum 
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
wm_interm_81_triplet(i, j, k, l) = wm_interm_81_triplet(i, j, k, l) + sum 
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
wm_interm_82_triplet(b, i, k, j) = wm_interm_82_triplet(b, i, k, j) + sum 
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
wm_interm_83_triplet(b, i, j, k) = wm_interm_83_triplet(b, i, j, k) + sum 
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
wm_interm_84_triplet(b, c) = wm_interm_84_triplet(b, c) + sum 
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
wm_interm_85_triplet(b, c) = wm_interm_85_triplet(b, c) + sum 
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
wm_interm_86_triplet(b, c) = wm_interm_86_triplet(b, c) + sum 
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
wm_interm_87_triplet(i, j, k, l) = wm_interm_87_triplet(i, j, k, l) + sum 
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
wm_interm_88_triplet(i, j, k, l) = wm_interm_88_triplet(i, j, k, l) + sum 
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
wm_interm_89_triplet(b, j, i, k) = wm_interm_89_triplet(b, j, i, k) + sum 
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
wm_interm_90_triplet(b, i, j, k) = wm_interm_90_triplet(b, i, j, k) + sum 
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
wm_interm_91_triplet(i, j, k, l) = wm_interm_91_triplet(i, j, k, l) + sum 
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
wm_interm_92_triplet(b, c) = wm_interm_92_triplet(b, c) + sum 
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
wm_interm_93_triplet(b, c) = wm_interm_93_triplet(b, c) + sum 
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
wm_interm_94_triplet(b, c) = wm_interm_94_triplet(b, c) + sum 
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
wm_interm_95_triplet(b, c) = wm_interm_95_triplet(b, c) + sum 
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
wm_interm_96_triplet(b, c) = wm_interm_96_triplet(b, c) + sum 
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
wm_interm_97_triplet(b, c) = wm_interm_97_triplet(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 



    end subroutine wm_triplet_intermediates_ccsd

        
    function calc_D_oo_wm_triplet(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_oo_wm_triplet
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
    real(F64), dimension(0:178) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + wm_interm_56_triplet(a, q, i, j) * wm_interm_67_triplet(a, j, p, i)
term(1) = term(1) + wm_interm_56_triplet(a, i, q, j) * wm_interm_67_triplet(a, j, p, i)
term(2) = term(2) + wm_interm_56_triplet(a, q, i, j) * wm_interm_72_triplet(a, j, p, i)
term(3) = term(3) + wm_interm_56_triplet(a, q, i, j) * wm_interm_72_triplet(a, j, i, p)
term(4) = term(4) + wm_interm_56_triplet(a, i, q, j) * wm_interm_72_triplet(a, j, i, p)
term(5) = term(5) + wm_interm_56_triplet(a, i, q, j) * wm_interm_72_triplet(a, j, p, i)
term(6) = term(6) + wm_interm_59_triplet(a, p, i, j) * wm_interm_66_triplet(a, q, i, j)
term(7) = term(7) + wm_interm_59_triplet(a, p, i, j) * wm_interm_66_triplet(a, q, j, i)
term(8) = term(8) + wm_interm_56_triplet(a, i, j, q) * wm_interm_67_triplet(a, p, i, j)
term(9) = term(9) + wm_interm_56_triplet(a, i, j, q) * wm_interm_72_triplet(a, p, i, j)
term(10) = term(10) + wm_interm_59_triplet(a, i, j, p) * wm_interm_66_triplet(a, i, j, q)
term(11) = term(11) + wm_interm_59_triplet(a, i, p, j) * wm_interm_66_triplet(a, i, q, j)
term(12) = term(12) + wm_interm_59_triplet(a, i, j, p) * wm_interm_66_triplet(a, i, q, j)
term(13) = term(13) + wm_interm_59_triplet(a, i, p, j) * wm_interm_66_triplet(a, i, j, q)
term(14) = term(14) + wm_interm_80_triplet(a, i, j, p) * wm_interm_82_triplet(a, q, i, j)
term(15) = term(15) + wm_interm_80_triplet(a, i, p, j) * wm_interm_82_triplet(a, j, i, q)
term(16) = term(16) + wm_interm_80_triplet(a, i, p, j) * wm_interm_82_triplet(a, j, q, i)
term(17) = term(17) + wm_interm_82_triplet(a, q, i, j) * wm_interm_89_triplet(a, j, i, p)
term(18) = term(18) + wm_interm_82_triplet(a, q, i, j) * wm_interm_90_triplet(a, j, i, p)
term(19) = term(19) + wm_interm_82_triplet(a, q, i, j) * wm_interm_90_triplet(a, i, j, p)
term(20) = term(20) + wm_interm_82_triplet(a, q, i, j) * wm_interm_89_triplet(a, i, j, p)
term(21) = term(21) + wm_interm_82_triplet(a, i, q, j) * wm_interm_89_triplet(a, j, p, i)
term(22) = term(22) + wm_interm_82_triplet(a, i, q, j) * wm_interm_89_triplet(a, p, j, i)
term(23) = term(23) + wm_interm_82_triplet(a, i, q, j) * wm_interm_90_triplet(a, p, j, i)
term(24) = term(24) + wm_interm_82_triplet(a, i, q, j) * wm_interm_90_triplet(a, j, p, i)
end do 
end do 
end do 

term(0) = term(0) * (-6.0d+0) 
term(1) = term(1) * 6.0d+0 
term(2) = term(2) * (-4.0d+0) 
term(3) = term(3) * 8.0d+0 
term(4) = term(4) * (-4.0d+0) 
term(5) = term(5) * 8.0d+0 
term(6) = term(6) * 4.0d+0 
term(7) = term(7) * (-2.0d+0) 
term(8) = term(8) * (-3.0d+0) 
term(9) = term(9) * (-4.0d+0) 
term(10) = term(10) * 4.0d+0 
term(11) = term(11) * 4.0d+0 
term(12) = term(12) * (-2.0d+0) 
term(13) = term(13) * (-2.0d+0) 
term(14) = term(14) * (-3.0d+0) 
term(15) = term(15) * (-6.0d+0) 
term(16) = term(16) * 6.0d+0 
term(17) = term(17) * (-2.0d+0) 
term(18) = term(18) * 4.0d+0 
term(19) = term(19) * (-2.0d+0) 
term(20) = term(20) * 4.0d+0 
term(21) = term(21) * (-2.0d+0) 
term(22) = term(22) * 4.0d+0 
term(23) = term(23) * (-2.0d+0) 
term(24) = term(24) * 4.0d+0 

do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(25) = term(25) + r2p(vrdav_Rl, a,p,b,i) * r2p(vrdav_Rr, a,q,b,i)
term(26) = term(26) + r2p(vrdav_Rl, a,p,b,i) * r2m(vrdav_Rr, a,i,b,q)
term(27) = term(27) + r2p(vrdav_Rl, a,p,b,i) * r2m(vrdav_Rr, a,q,b,i)
term(28) = term(28) + r2m(vrdav_Rl, a,p,b,i) * r2p(vrdav_Rr, a,q,b,i)
term(29) = term(29) + r2m(vrdav_Rl, a,i,b,p) * r2p(vrdav_Rr, a,q,b,i)
term(30) = term(30) + r2m(vrdav_Rl, a,p,b,i) * r2m(vrdav_Rr, a,i,b,q)
term(31) = term(31) + r2m(vrdav_Rl, a,p,b,i) * r2m(vrdav_Rr, a,q,b,i)
term(32) = term(32) + r2m(vrdav_Rl, a,i,b,p) * r2m(vrdav_Rr, a,q,b,i)
end do 
end do 
end do 

term(25) = term(25) * (-6.0d+0) 
term(26) = term(26) * 6.0d+0 
term(27) = term(27) * (-6.0d+0) 
term(28) = term(28) * (-6.0d+0) 
term(29) = term(29) * 6.0d+0 
term(30) = term(30) * 4.0d+0 
term(31) = term(31) * (-8.0d+0) 
term(32) = term(32) * 4.0d+0 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(33) = term(33) + wm_interm_56_triplet(a, i, j, q) * wm_interm_67_triplet(a, p, j, i)
term(34) = term(34) + wm_interm_56_triplet(a, i, j, q) * wm_interm_72_triplet(a, p, j, i)
term(35) = term(35) + wm_interm_80_triplet(a, i, j, p) * wm_interm_82_triplet(a, q, j, i)
term(36) = term(36) + wm_interm_82_triplet(a, i, j, q) * wm_interm_89_triplet(a, p, j, i)
term(37) = term(37) + wm_interm_82_triplet(a, i, j, q) * wm_interm_90_triplet(a, p, j, i)
term(38) = term(38) + wm_interm_82_triplet(a, i, j, q) * wm_interm_89_triplet(a, j, p, i)
term(39) = term(39) + wm_interm_82_triplet(a, i, j, q) * wm_interm_90_triplet(a, j, p, i)
end do 
end do 
end do 

term(33) = term(33) * 3.0d+0 
term(34) = term(34) * 8.0d+0 
term(35) = term(35) * 3.0d+0 
term(36) = term(36) * (-2.0d+0) 
term(37) = term(37) * 4.0d+0 
term(38) = term(38) * 4.0d+0 
term(39) = term(39) * (-2.0d+0) 

do a = nocc + 1, nactive 
term(40) = term(40) + r1(vrdav_Rl, a,q) * wm_interm_0_triplet(a, p)
term(41) = term(41) + r1(vrdav_Rl, a,q) * wm_interm_1_triplet(a, p)
term(42) = term(42) + r1(vrdav_Rl, a,q) * wm_interm_2_triplet(a, p)
term(43) = term(43) + s1(a,q) * wm_interm_3_triplet(a, p)
term(44) = term(44) + s1(a,q) * wm_interm_4_triplet(a, p)
term(45) = term(45) + s1(a,q) * wm_interm_5_triplet(a, p)
term(46) = term(46) + r1(vrdav_Rr, a,p) * wm_interm_6_triplet(a, q)
term(47) = term(47) + t1(a,q) * wm_interm_7_triplet(a, p)
term(48) = term(48) + r1(vrdav_Rr, a,p) * wm_interm_8_triplet(a, q)
term(49) = term(49) + r1(vrdav_Rr, a,p) * wm_interm_9_triplet(a, q)
term(50) = term(50) + t1(a,q) * wm_interm_10_triplet(a, p)
term(51) = term(51) + t1(a,q) * wm_interm_11_triplet(a, p)
term(52) = term(52) + wm_interm_58_triplet(a, p) * wm_interm_63_triplet(a, q)
term(53) = term(53) + wm_interm_57_triplet(a, p) * wm_interm_64_triplet(a, q)
term(54) = term(54) + wm_interm_58_triplet(a, p) * wm_interm_64_triplet(a, q)
term(55) = term(55) + wm_interm_57_triplet(a, p) * wm_interm_63_triplet(a, q)
term(56) = term(56) + wm_interm_61_triplet(a, q) * wm_interm_7_triplet(a, p)
term(57) = term(57) + wm_interm_65_triplet(a, q) * wm_interm_7_triplet(a, p)
term(58) = term(58) + wm_interm_11_triplet(a, p) * wm_interm_61_triplet(a, q)
term(59) = term(59) + wm_interm_10_triplet(a, p) * wm_interm_61_triplet(a, q)
term(60) = term(60) + wm_interm_10_triplet(a, p) * wm_interm_65_triplet(a, q)
term(61) = term(61) + wm_interm_11_triplet(a, p) * wm_interm_65_triplet(a, q)
term(62) = term(62) + r1(vrdav_Rl, a,p) * r1(vrdav_Rr, a,q)
end do 

term(40) = term(40) * (-6.0d+0) 
term(41) = term(41) * (-8.0d+0) 
term(42) = term(42) * 4.0d+0 
term(43) = term(43) * 6.0d+0 
term(44) = term(44) * (-4.0d+0) 
term(45) = term(45) * 8.0d+0 
term(46) = term(46) * (-6.0d+0) 
term(47) = term(47) * 6.0d+0 
term(48) = term(48) * 8.0d+0 
term(49) = term(49) * (-4.0d+0) 
term(50) = term(50) * (-4.0d+0) 
term(51) = term(51) * 8.0d+0 
term(52) = term(52) * (-8.0d+0) 
term(53) = term(53) * (-2.0d+0) 
term(54) = term(54) * 4.0d+0 
term(55) = term(55) * 4.0d+0 
term(56) = term(56) * 6.0d+0 
term(57) = term(57) * (-12.0d+0) 
term(58) = term(58) * 8.0d+0 
term(59) = term(59) * (-4.0d+0) 
term(60) = term(60) * 8.0d+0 
term(61) = term(61) * (-16.0d+0) 
term(62) = term(62) * (-2.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
term(63) = term(63) + wm_interm_55_triplet(i, j) * wm_interm_73_triplet(j, p, i, q)
term(64) = term(64) + wm_interm_55_triplet(i, j) * wm_interm_73_triplet(p, j, i, q)
end do 
end do 

term(63) = term(63) * 4.0d+0 
term(64) = term(64) * (-2.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
term(65) = term(65) + wm_interm_15_triplet(i, j) * wm_interm_74_triplet(i, p, q, j)
term(66) = term(66) + wm_interm_15_triplet(i, j) * wm_interm_74_triplet(i, p, j, q)
term(67) = term(67) + wm_interm_15_triplet(i, j) * wm_interm_18_triplet(i, j)
term(68) = term(68) + wm_interm_15_triplet(i, j) * wm_interm_19_triplet(i, j)
term(69) = term(69) + wm_interm_15_triplet(i, j) * wm_interm_77_triplet(i, p, q, j)
term(70) = term(70) + wm_interm_15_triplet(i, j) * wm_interm_77_triplet(p, i, q, j)
term(71) = term(71) + wm_interm_15_triplet(i, j) * wm_interm_77_triplet(p, i, j, q)
term(72) = term(72) + wm_interm_15_triplet(i, j) * wm_interm_77_triplet(i, p, j, q)
term(73) = term(73) + wm_interm_15_triplet(i, j) * wm_interm_23_triplet(i, j)
term(74) = term(74) + wm_interm_15_triplet(i, j) * wm_interm_24_triplet(i, j)
term(75) = term(75) + wm_interm_15_triplet(i, j) * wm_interm_25_triplet(i, j)
term(76) = term(76) + wm_interm_15_triplet(i, j) * wm_interm_26_triplet(i, j)
term(77) = term(77) + wm_interm_55_triplet(i, j) * wm_interm_68_triplet(j, i)
term(78) = term(78) + wm_interm_55_triplet(i, j) * wm_interm_69_triplet(j, i)
term(79) = term(79) + wm_interm_14_triplet(i, j) * wm_interm_37_triplet(i, j)
term(80) = term(80) + wm_interm_14_triplet(i, j) * wm_interm_36_triplet(i, j)
term(81) = term(81) + wm_interm_14_triplet(i, j) * wm_interm_81_triplet(i, p, q, j)
term(82) = term(82) + wm_interm_14_triplet(i, j) * wm_interm_81_triplet(i, p, j, q)
term(83) = term(83) + wm_interm_14_triplet(i, j) * wm_interm_51_triplet(i, j)
term(84) = term(84) + wm_interm_14_triplet(i, j) * wm_interm_52_triplet(i, j)
term(85) = term(85) + wm_interm_14_triplet(i, j) * wm_interm_91_triplet(i, p, q, j)
term(86) = term(86) + wm_interm_14_triplet(i, j) * wm_interm_91_triplet(p, i, q, j)
term(87) = term(87) + wm_interm_14_triplet(i, j) * wm_interm_91_triplet(p, i, j, q)
term(88) = term(88) + wm_interm_14_triplet(i, j) * wm_interm_91_triplet(i, p, j, q)
end do 
end do 

term(65) = term(65) * (-3.0d+0) 
term(66) = term(66) * 3.0d+0 
term(67) = term(67) * 6.0d+0 
term(68) = term(68) * (-6.0d+0) 
term(69) = term(69) * (-2.0d+0) 
term(70) = term(70) * 4.0d+0 
term(71) = term(71) * (-2.0d+0) 
term(72) = term(72) * 4.0d+0 
term(73) = term(73) * 4.0d+0 
term(74) = term(74) * (-8.0d+0) 
term(75) = term(75) * 4.0d+0 
term(76) = term(76) * (-8.0d+0) 
term(77) = term(77) * 4.0d+0 
term(78) = term(78) * (-8.0d+0) 
term(79) = term(79) * (-6.0d+0) 
term(80) = term(80) * 6.0d+0 
term(81) = term(81) * (-3.0d+0) 
term(82) = term(82) * 3.0d+0 
term(83) = term(83) * 8.0d+0 
term(84) = term(84) * (-16.0d+0) 
term(85) = term(85) * (-2.0d+0) 
term(86) = term(86) * 4.0d+0 
term(87) = term(87) * (-2.0d+0) 
term(88) = term(88) * 4.0d+0 

do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(89) = term(89) + r2m(vrdav_Rl, a,i,b,p) * r2m(vrdav_Rr, a,i,b,q)
end do 
end do 
end do 

term(89) = term(89) * (-8.0d+0) 

do i = 1, nocc 
term(90) = term(90) + wm_interm_15_triplet(q, i) * wm_interm_18_triplet(p, i)
term(91) = term(91) + wm_interm_15_triplet(q, i) * wm_interm_19_triplet(p, i)
term(92) = term(92) + wm_interm_15_triplet(q, i) * wm_interm_23_triplet(p, i)
term(93) = term(93) + wm_interm_15_triplet(q, i) * wm_interm_24_triplet(p, i)
term(94) = term(94) + wm_interm_15_triplet(q, i) * wm_interm_25_triplet(p, i)
term(95) = term(95) + wm_interm_15_triplet(q, i) * wm_interm_26_triplet(p, i)
term(96) = term(96) + wm_interm_55_triplet(i, p) * wm_interm_68_triplet(q, i)
term(97) = term(97) + wm_interm_55_triplet(i, p) * wm_interm_69_triplet(q, i)
term(98) = term(98) + wm_interm_15_triplet(i, q) * wm_interm_18_triplet(i, p)
term(99) = term(99) + wm_interm_15_triplet(i, q) * wm_interm_19_triplet(i, p)
term(100) = term(100) + wm_interm_15_triplet(i, q) * wm_interm_23_triplet(i, p)
term(101) = term(101) + wm_interm_15_triplet(i, q) * wm_interm_24_triplet(i, p)
term(102) = term(102) + wm_interm_15_triplet(i, q) * wm_interm_25_triplet(i, p)
term(103) = term(103) + wm_interm_15_triplet(i, q) * wm_interm_26_triplet(i, p)
term(104) = term(104) + wm_interm_55_triplet(p, i) * wm_interm_68_triplet(i, q)
term(105) = term(105) + wm_interm_55_triplet(p, i) * wm_interm_69_triplet(i, q)
term(106) = term(106) + wm_interm_14_triplet(p, i) * wm_interm_37_triplet(q, i)
term(107) = term(107) + wm_interm_14_triplet(p, i) * wm_interm_36_triplet(q, i)
term(108) = term(108) + wm_interm_14_triplet(i, p) * wm_interm_36_triplet(i, q)
term(109) = term(109) + wm_interm_14_triplet(i, p) * wm_interm_37_triplet(i, q)
term(110) = term(110) + wm_interm_14_triplet(p, i) * wm_interm_51_triplet(q, i)
term(111) = term(111) + wm_interm_14_triplet(p, i) * wm_interm_52_triplet(q, i)
term(112) = term(112) + wm_interm_14_triplet(i, p) * wm_interm_51_triplet(i, q)
term(113) = term(113) + wm_interm_14_triplet(i, p) * wm_interm_52_triplet(i, q)
end do 

term(90) = term(90) * (-3.0d+0) 
term(91) = term(91) * 3.0d+0 
term(92) = term(92) * (-2.0d+0) 
term(93) = term(93) * 4.0d+0 
term(94) = term(94) * (-2.0d+0) 
term(95) = term(95) * 4.0d+0 
term(96) = term(96) * (-2.0d+0) 
term(97) = term(97) * 4.0d+0 
term(98) = term(98) * (-3.0d+0) 
term(99) = term(99) * 3.0d+0 
term(100) = term(100) * (-2.0d+0) 
term(101) = term(101) * 4.0d+0 
term(102) = term(102) * (-2.0d+0) 
term(103) = term(103) * 4.0d+0 
term(104) = term(104) * (-2.0d+0) 
term(105) = term(105) * 4.0d+0 
term(106) = term(106) * (-3.0d+0) 
term(107) = term(107) * 3.0d+0 
term(108) = term(108) * (-3.0d+0) 
term(109) = term(109) * 3.0d+0 
term(110) = term(110) * (-4.0d+0) 
term(111) = term(111) * 8.0d+0 
term(112) = term(112) * (-4.0d+0) 
term(113) = term(113) * 8.0d+0 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(114) = term(114) + r1(vrdav_Rl, a,i) * wm_interm_0_triplet(a, i)
term(115) = term(115) + r1(vrdav_Rl, a,i) * wm_interm_1_triplet(a, i)
term(116) = term(116) + r1(vrdav_Rl, a,i) * wm_interm_2_triplet(a, i)
term(117) = term(117) + r1(vrdav_Rr, a,i) * wm_interm_6_triplet(a, i)
term(118) = term(118) + r1(vrdav_Rr, a,i) * wm_interm_8_triplet(a, i)
term(119) = term(119) + r1(vrdav_Rr, a,i) * wm_interm_9_triplet(a, i)
term(120) = term(120) + wm_interm_59_triplet(a, p, i, q) * wm_interm_64_triplet(a, i)
term(121) = term(121) + wm_interm_57_triplet(a, i) * wm_interm_64_triplet(a, i)
term(122) = term(122) + wm_interm_59_triplet(a, p, q, i) * wm_interm_64_triplet(a, i)
term(123) = term(123) + wm_interm_58_triplet(a, i) * wm_interm_64_triplet(a, i)
term(124) = term(124) + wm_interm_59_triplet(a, p, q, i) * wm_interm_63_triplet(a, i)
term(125) = term(125) + wm_interm_58_triplet(a, i) * wm_interm_63_triplet(a, i)
term(126) = term(126) + wm_interm_59_triplet(a, p, i, q) * wm_interm_63_triplet(a, i)
term(127) = term(127) + wm_interm_57_triplet(a, i) * wm_interm_63_triplet(a, i)
term(128) = term(128) + wm_interm_3_triplet(a, i) * wm_interm_56_triplet(a, i, p, q)
term(129) = term(129) + wm_interm_3_triplet(a, i) * wm_interm_56_triplet(a, p, i, q)
term(130) = term(130) + wm_interm_4_triplet(a, i) * wm_interm_56_triplet(a, i, p, q)
term(131) = term(131) + wm_interm_56_triplet(a, i, p, q) * wm_interm_5_triplet(a, i)
term(132) = term(132) + wm_interm_4_triplet(a, i) * wm_interm_56_triplet(a, p, i, q)
term(133) = term(133) + wm_interm_56_triplet(a, p, i, q) * wm_interm_5_triplet(a, i)
term(134) = term(134) + wm_interm_57_triplet(a, i) * wm_interm_66_triplet(a, p, i, q)
term(135) = term(135) + wm_interm_58_triplet(a, i) * wm_interm_66_triplet(a, p, i, q)
term(136) = term(136) + wm_interm_58_triplet(a, i) * wm_interm_66_triplet(a, p, q, i)
term(137) = term(137) + wm_interm_57_triplet(a, i) * wm_interm_66_triplet(a, p, q, i)
term(138) = term(138) + wm_interm_61_triplet(a, i) * wm_interm_80_triplet(a, i, q, p)
term(139) = term(139) + wm_interm_61_triplet(a, i) * wm_interm_7_triplet(a, i)
term(140) = term(140) + wm_interm_65_triplet(a, i) * wm_interm_80_triplet(a, i, q, p)
term(141) = term(141) + wm_interm_65_triplet(a, i) * wm_interm_7_triplet(a, i)
term(142) = term(142) + wm_interm_7_triplet(a, i) * wm_interm_82_triplet(a, p, i, q)
term(143) = term(143) + wm_interm_7_triplet(a, i) * wm_interm_82_triplet(a, p, q, i)
term(144) = term(144) + wm_interm_61_triplet(a, i) * wm_interm_90_triplet(a, q, i, p)
term(145) = term(145) + wm_interm_11_triplet(a, i) * wm_interm_61_triplet(a, i)
term(146) = term(146) + wm_interm_65_triplet(a, i) * wm_interm_90_triplet(a, q, i, p)
term(147) = term(147) + wm_interm_11_triplet(a, i) * wm_interm_65_triplet(a, i)
term(148) = term(148) + wm_interm_61_triplet(a, i) * wm_interm_89_triplet(a, i, q, p)
term(149) = term(149) + wm_interm_61_triplet(a, i) * wm_interm_90_triplet(a, i, q, p)
term(150) = term(150) + wm_interm_10_triplet(a, i) * wm_interm_61_triplet(a, i)
term(151) = term(151) + wm_interm_65_triplet(a, i) * wm_interm_90_triplet(a, i, q, p)
term(152) = term(152) + wm_interm_10_triplet(a, i) * wm_interm_65_triplet(a, i)
term(153) = term(153) + wm_interm_61_triplet(a, i) * wm_interm_89_triplet(a, q, i, p)
term(154) = term(154) + wm_interm_65_triplet(a, i) * wm_interm_89_triplet(a, q, i, p)
term(155) = term(155) + wm_interm_65_triplet(a, i) * wm_interm_89_triplet(a, i, q, p)
term(156) = term(156) + wm_interm_10_triplet(a, i) * wm_interm_82_triplet(a, p, i, q)
term(157) = term(157) + wm_interm_11_triplet(a, i) * wm_interm_82_triplet(a, p, i, q)
term(158) = term(158) + wm_interm_10_triplet(a, i) * wm_interm_82_triplet(a, p, q, i)
term(159) = term(159) + wm_interm_11_triplet(a, i) * wm_interm_82_triplet(a, p, q, i)
end do 
end do 

term(114) = term(114) * 12.0d+0 
term(115) = term(115) * 16.0d+0 
term(116) = term(116) * (-8.0d+0) 
term(117) = term(117) * (-12.0d+0) 
term(118) = term(118) * (-16.0d+0) 
term(119) = term(119) * 8.0d+0 
term(120) = term(120) * (-2.0d+0) 
term(121) = term(121) * 4.0d+0 
term(122) = term(122) * 4.0d+0 
term(123) = term(123) * (-8.0d+0) 
term(124) = term(124) * (-8.0d+0) 
term(125) = term(125) * 16.0d+0 
term(126) = term(126) * 4.0d+0 
term(127) = term(127) * (-8.0d+0) 
term(128) = term(128) * (-12.0d+0) 
term(129) = term(129) * 6.0d+0 
term(130) = term(130) * 8.0d+0 
term(131) = term(131) * (-16.0d+0) 
term(132) = term(132) * (-4.0d+0) 
term(133) = term(133) * 8.0d+0 
term(134) = term(134) * (-2.0d+0) 
term(135) = term(135) * 4.0d+0 
term(136) = term(136) * (-8.0d+0) 
term(137) = term(137) * 4.0d+0 
term(138) = term(138) * (-6.0d+0) 
term(139) = term(139) * (-12.0d+0) 
term(140) = term(140) * 12.0d+0 
term(141) = term(141) * 24.0d+0 
term(142) = term(142) * (-12.0d+0) 
term(143) = term(143) * 6.0d+0 
term(144) = term(144) * 4.0d+0 
term(145) = term(145) * (-16.0d+0) 
term(146) = term(146) * (-8.0d+0) 
term(147) = term(147) * 32.0d+0 
term(148) = term(148) * 4.0d+0 
term(149) = term(149) * (-2.0d+0) 
term(150) = term(150) * 8.0d+0 
term(151) = term(151) * 4.0d+0 
term(152) = term(152) * (-16.0d+0) 
term(153) = term(153) * (-2.0d+0) 
term(154) = term(154) * 4.0d+0 
term(155) = term(155) * (-8.0d+0) 
term(156) = term(156) * 8.0d+0 
term(157) = term(157) * (-16.0d+0) 
term(158) = term(158) * (-4.0d+0) 
term(159) = term(159) * 8.0d+0 

term(160) = term(160) + wm_interm_28_triplet
term(161) = term(161) + wm_interm_32_triplet
term(162) = term(162) + wm_interm_33_triplet
term(163) = term(163) + wm_interm_42_triplet
term(164) = term(164) + wm_interm_47_triplet
term(165) = term(165) + wm_interm_48_triplet
term(166) = term(166) + wm_interm_13_triplet
term(167) = term(167) + wm_interm_13_triplet * wm_interm_69_triplet(p, q)
term(168) = term(168) + wm_interm_13_triplet * wm_interm_68_triplet(p, q)

term(160) = term(160) * 6.0d+0 
term(161) = term(161) * (-4.0d+0) 
term(162) = term(162) * 8.0d+0 
term(163) = term(163) * 12.0d+0 
term(164) = term(164) * (-8.0d+0) 
term(165) = term(165) * 16.0d+0 
term(166) = term(166) * 4.0d+0 
term(167) = term(167) * (-8.0d+0) 
term(168) = term(168) * 4.0d+0 

do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(169) = term(169) + wm_interm_16_triplet(a, b) * wm_interm_17_triplet(a, b)
term(170) = term(170) + wm_interm_16_triplet(a, b) * wm_interm_20_triplet(a, b)
term(171) = term(171) + wm_interm_16_triplet(a, b) * wm_interm_21_triplet(a, b)
term(172) = term(172) + wm_interm_16_triplet(a, b) * wm_interm_22_triplet(a, b)
term(173) = term(173) + wm_interm_62_triplet(a, b) * wm_interm_70_triplet(b, a)
term(174) = term(174) + wm_interm_62_triplet(a, b) * wm_interm_71_triplet(b, a)
term(175) = term(175) + wm_interm_12_triplet(a, b) * wm_interm_38_triplet(a, b)
term(176) = term(176) + wm_interm_12_triplet(a, b) * wm_interm_39_triplet(a, b)
term(177) = term(177) + wm_interm_12_triplet(a, b) * wm_interm_53_triplet(a, b)
term(178) = term(178) + wm_interm_12_triplet(a, b) * wm_interm_54_triplet(a, b)
end do 
end do 

term(169) = term(169) * (-6.0d+0) 
term(170) = term(170) * 6.0d+0 
term(171) = term(171) * 8.0d+0 
term(172) = term(172) * (-16.0d+0) 
term(173) = term(173) * (-8.0d+0) 
term(174) = term(174) * 4.0d+0 
term(175) = term(175) * (-6.0d+0) 
term(176) = term(176) * 6.0d+0 
term(177) = term(177) * 8.0d+0 
term(178) = term(178) * (-16.0d+0) 


    calc_D_oo_wm_triplet = zero
    do s = 0, 178
    calc_D_oo_wm_triplet = calc_D_oo_wm_triplet + term(s)
    end do

    end function calc_D_oo_wm_triplet
    
    function calc_D_ov_wm_triplet(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_ov_wm_triplet
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
    real(F64), dimension(0:247) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(0) = term(0) + wm_interm_66_triplet(q, i, j, k) * wm_interm_74_triplet(i, p, j, k)
term(1) = term(1) + wm_interm_66_triplet(q, i, j, k) * wm_interm_74_triplet(i, p, k, j)
term(2) = term(2) + wm_interm_66_triplet(q, i, j, k) * wm_interm_77_triplet(p, i, j, k)
term(3) = term(3) + wm_interm_66_triplet(q, i, j, k) * wm_interm_77_triplet(i, p, j, k)
term(4) = term(4) + wm_interm_66_triplet(q, i, j, k) * wm_interm_77_triplet(i, p, k, j)
term(5) = term(5) + wm_interm_66_triplet(q, i, j, k) * wm_interm_77_triplet(p, i, k, j)
term(6) = term(6) + wm_interm_35_triplet(q, i, j, k) * wm_interm_74_triplet(i, j, p, k)
term(7) = term(7) + wm_interm_35_triplet(q, i, j, k) * wm_interm_74_triplet(i, j, k, p)
term(8) = term(8) + wm_interm_35_triplet(q, i, j, k) * wm_interm_77_triplet(j, i, p, k)
term(9) = term(9) + wm_interm_35_triplet(q, i, j, k) * wm_interm_77_triplet(i, j, p, k)
term(10) = term(10) + wm_interm_35_triplet(q, i, j, k) * wm_interm_77_triplet(i, j, k, p)
term(11) = term(11) + wm_interm_35_triplet(q, i, j, k) * wm_interm_77_triplet(j, i, k, p)
term(12) = term(12) + wm_interm_60_triplet(q, i, j, k) * wm_interm_79_triplet(k, j, i, p)
term(13) = term(13) + wm_interm_60_triplet(q, i, j, k) * wm_interm_79_triplet(j, k, i, p)
term(14) = term(14) + wm_interm_50_triplet(q, i, j, k) * wm_interm_74_triplet(i, j, p, k)
term(15) = term(15) + wm_interm_50_triplet(q, i, j, k) * wm_interm_74_triplet(i, j, k, p)
term(16) = term(16) + wm_interm_49_triplet(q, i, j, k) * wm_interm_74_triplet(i, j, p, k)
term(17) = term(17) + wm_interm_49_triplet(q, i, j, k) * wm_interm_74_triplet(i, j, k, p)
term(18) = term(18) + wm_interm_50_triplet(q, i, j, k) * wm_interm_77_triplet(j, i, p, k)
term(19) = term(19) + wm_interm_50_triplet(q, i, j, k) * wm_interm_77_triplet(i, j, p, k)
term(20) = term(20) + wm_interm_50_triplet(q, i, j, k) * wm_interm_77_triplet(i, j, k, p)
term(21) = term(21) + wm_interm_50_triplet(q, i, j, k) * wm_interm_77_triplet(j, i, k, p)
term(22) = term(22) + wm_interm_49_triplet(q, i, j, k) * wm_interm_77_triplet(i, j, p, k)
term(23) = term(23) + wm_interm_49_triplet(q, i, j, k) * wm_interm_77_triplet(j, i, p, k)
term(24) = term(24) + wm_interm_49_triplet(q, i, j, k) * wm_interm_77_triplet(j, i, k, p)
term(25) = term(25) + wm_interm_49_triplet(q, i, j, k) * wm_interm_77_triplet(i, j, k, p)
term(26) = term(26) + wm_interm_60_triplet(q, i, j, k) * wm_interm_88_triplet(k, j, i, p)
term(27) = term(27) + wm_interm_60_triplet(q, i, j, k) * wm_interm_88_triplet(j, k, i, p)
end do 
end do 
end do 

term(0) = term(0) * 3.0d+0 
term(1) = term(1) * (-3.0d+0) 
term(2) = term(2) * (-2.0d+0) 
term(3) = term(3) * 4.0d+0 
term(4) = term(4) * (-2.0d+0) 
term(5) = term(5) * 4.0d+0 
term(6) = term(6) * (-3.0d+0) 
term(7) = term(7) * 3.0d+0 
term(8) = term(8) * 3.0d+0 
term(9) = term(9) * (-3.0d+0) 
term(10) = term(10) * 3.0d+0 
term(11) = term(11) * (-3.0d+0) 
term(12) = term(12) * (-2.0d+0) 
term(13) = term(13) * 4.0d+0 
term(14) = term(14) * 3.0d+0 
term(15) = term(15) * (-3.0d+0) 
term(16) = term(16) * (-3.0d+0) 
term(17) = term(17) * 3.0d+0 
term(18) = term(18) * (-2.0d+0) 
term(19) = term(19) * 4.0d+0 
term(20) = term(20) * (-2.0d+0) 
term(21) = term(21) * 4.0d+0 
term(22) = term(22) * (-2.0d+0) 
term(23) = term(23) * 4.0d+0 
term(24) = term(24) * (-2.0d+0) 
term(25) = term(25) * 4.0d+0 
term(26) = term(26) * (-4.0d+0) 
term(27) = term(27) * 8.0d+0 

do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(28) = term(28) + s2(a,b,j,i) * t2(b,q,k,i) * wm_interm_72_triplet(a, k, j, p)
end do 
end do 
end do 
end do 
end do 

term(28) = term(28) * (-4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(29) = term(29) + r2m(vrdav_Rl, a,i,q,j) * r2m(vrdav_Rr, a,i,b,k) * wm_interm_56_triplet(b, p, k, j)
term(30) = term(30) + r2m(vrdav_Rl, a,i,q,j) * r2m(vrdav_Rr, a,i,b,k) * wm_interm_56_triplet(b, k, p, j)
term(31) = term(31) + r2m(vrdav_Rl, a,i,q,j) * t2(a,b,k,j) * wm_interm_59_triplet(b, i, p, k)
term(32) = term(32) + r2m(vrdav_Rl, a,i,q,j) * t2(a,b,j,k) * wm_interm_59_triplet(b, i, p, k)
term(33) = term(33) + r2m(vrdav_Rl, a,i,q,j) * t2(a,b,i,k) * wm_interm_59_triplet(b, j, p, k)
term(34) = term(34) + r2m(vrdav_Rl, a,i,q,j) * t2(a,b,j,k) * wm_interm_59_triplet(b, i, k, p)
term(35) = term(35) + r2m(vrdav_Rl, a,i,q,j) * t2(a,b,i,k) * wm_interm_59_triplet(b, j, k, p)
end do 
end do 
end do 
end do 
end do 

term(29) = term(29) * 8.0d+0 
term(30) = term(30) * (-16.0d+0) 
term(31) = term(31) * (-2.0d+0) 
term(32) = term(32) * 4.0d+0 
term(33) = term(33) * (-8.0d+0) 
term(34) = term(34) * (-2.0d+0) 
term(35) = term(35) * 4.0d+0 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
do k = 1, nocc 
do b = nocc + 1, nactive 
term(36) = term(36) + r2p(vrdav_Rl, q,i,a,j) * r2p(vrdav_Rr, b,k,a,j) * wm_interm_56_triplet(b, p, k, i)
term(37) = term(37) + r2p(vrdav_Rl, q,i,a,j) * r2p(vrdav_Rr, b,k,a,j) * wm_interm_56_triplet(b, k, p, i)
term(38) = term(38) + r2m(vrdav_Rl, a,j,q,i) * r2p(vrdav_Rr, b,k,a,i) * wm_interm_56_triplet(b, p, k, j)
term(39) = term(39) + r2m(vrdav_Rl, a,j,q,i) * r2p(vrdav_Rr, b,k,a,i) * wm_interm_56_triplet(b, k, p, j)
term(40) = term(40) + r2m(vrdav_Rl, a,i,q,j) * r2p(vrdav_Rr, b,k,a,i) * wm_interm_56_triplet(b, p, k, j)
term(41) = term(41) + r2m(vrdav_Rl, a,i,q,j) * r2p(vrdav_Rr, b,k,a,i) * wm_interm_56_triplet(b, k, p, j)
term(42) = term(42) + r2m(vrdav_Rl, a,j,q,i) * r2p(vrdav_Rr, b,k,a,j) * wm_interm_56_triplet(b, p, k, i)
term(43) = term(43) + r2m(vrdav_Rl, a,i,q,j) * r2p(vrdav_Rr, b,k,a,j) * wm_interm_56_triplet(b, p, k, i)
term(44) = term(44) + r2m(vrdav_Rl, a,j,q,i) * r2p(vrdav_Rr, b,k,a,j) * wm_interm_56_triplet(b, k, p, i)
term(45) = term(45) + r2m(vrdav_Rl, a,i,q,j) * r2p(vrdav_Rr, b,k,a,j) * wm_interm_56_triplet(b, k, p, i)
end do 
end do 
end do 
end do 
end do 

term(36) = term(36) * 12.0d+0 
term(37) = term(37) * (-18.0d+0) 
term(38) = term(38) * (-6.0d+0) 
term(39) = term(39) * 6.0d+0 
term(40) = term(40) * 6.0d+0 
term(41) = term(41) * (-12.0d+0) 
term(42) = term(42) * 6.0d+0 
term(43) = term(43) * (-6.0d+0) 
term(44) = term(44) * (-12.0d+0) 
term(45) = term(45) * 6.0d+0 

do a = nocc + 1, nactive 
term(46) = term(46) + r1(vrdav_Rl, a,p) * wm_interm_12_triplet(a, q)
term(47) = term(47) + r1(vrdav_Rr, a,p) * wm_interm_16_triplet(a, q)
term(48) = term(48) + r1(vrdav_Rl, a,p) * wm_interm_17_triplet(a, q)
term(49) = term(49) + r1(vrdav_Rl, a,p) * wm_interm_20_triplet(a, q)
term(50) = term(50) + r1(vrdav_Rl, a,p) * wm_interm_21_triplet(a, q)
term(51) = term(51) + r1(vrdav_Rl, a,p) * wm_interm_22_triplet(a, q)
term(52) = term(52) + r1(vrdav_Rr, a,p) * wm_interm_38_triplet(a, q)
term(53) = term(53) + r1(vrdav_Rr, a,p) * wm_interm_39_triplet(a, q)
term(54) = term(54) + r1(vrdav_Rr, a,p) * wm_interm_53_triplet(a, q)
term(55) = term(55) + r1(vrdav_Rr, a,p) * wm_interm_54_triplet(a, q)
term(56) = term(56) + wm_interm_16_triplet(q, a) * wm_interm_57_triplet(a, p)
term(57) = term(57) + wm_interm_16_triplet(q, a) * wm_interm_58_triplet(a, p)
term(58) = term(58) + wm_interm_3_triplet(a, p) * wm_interm_70_triplet(a, q)
term(59) = term(59) + wm_interm_3_triplet(a, p) * wm_interm_71_triplet(a, q)
term(60) = term(60) + wm_interm_4_triplet(a, p) * wm_interm_70_triplet(a, q)
term(61) = term(61) + wm_interm_5_triplet(a, p) * wm_interm_70_triplet(a, q)
term(62) = term(62) + wm_interm_4_triplet(a, p) * wm_interm_71_triplet(a, q)
term(63) = term(63) + wm_interm_5_triplet(a, p) * wm_interm_71_triplet(a, q)
term(64) = term(64) + wm_interm_38_triplet(q, a) * wm_interm_57_triplet(a, p)
term(65) = term(65) + wm_interm_38_triplet(q, a) * wm_interm_58_triplet(a, p)
term(66) = term(66) + wm_interm_39_triplet(q, a) * wm_interm_57_triplet(a, p)
term(67) = term(67) + wm_interm_39_triplet(q, a) * wm_interm_58_triplet(a, p)
term(68) = term(68) + wm_interm_0_triplet(a, p) * wm_interm_38_triplet(a, q)
term(69) = term(69) + wm_interm_0_triplet(a, p) * wm_interm_39_triplet(a, q)
term(70) = term(70) + wm_interm_1_triplet(a, p) * wm_interm_38_triplet(a, q)
term(71) = term(71) + wm_interm_1_triplet(a, p) * wm_interm_39_triplet(a, q)
term(72) = term(72) + wm_interm_2_triplet(a, p) * wm_interm_38_triplet(a, q)
term(73) = term(73) + wm_interm_2_triplet(a, p) * wm_interm_39_triplet(a, q)
term(74) = term(74) + wm_interm_53_triplet(q, a) * wm_interm_57_triplet(a, p)
term(75) = term(75) + wm_interm_54_triplet(q, a) * wm_interm_57_triplet(a, p)
term(76) = term(76) + wm_interm_53_triplet(q, a) * wm_interm_58_triplet(a, p)
term(77) = term(77) + wm_interm_54_triplet(q, a) * wm_interm_58_triplet(a, p)
term(78) = term(78) + wm_interm_0_triplet(a, p) * wm_interm_53_triplet(a, q)
term(79) = term(79) + wm_interm_0_triplet(a, p) * wm_interm_54_triplet(a, q)
term(80) = term(80) + wm_interm_1_triplet(a, p) * wm_interm_53_triplet(a, q)
term(81) = term(81) + wm_interm_1_triplet(a, p) * wm_interm_54_triplet(a, q)
term(82) = term(82) + wm_interm_2_triplet(a, p) * wm_interm_53_triplet(a, q)
term(83) = term(83) + wm_interm_2_triplet(a, p) * wm_interm_54_triplet(a, q)
end do 

term(46) = term(46) * 2.0d+0 
term(47) = term(47) * 2.0d+0 
term(48) = term(48) * 3.0d+0 
term(49) = term(49) * (-3.0d+0) 
term(50) = term(50) * (-4.0d+0) 
term(51) = term(51) * 8.0d+0 
term(52) = term(52) * 3.0d+0 
term(53) = term(53) * (-3.0d+0) 
term(54) = term(54) * (-4.0d+0) 
term(55) = term(55) * 8.0d+0 
term(56) = term(56) * 2.0d+0 
term(57) = term(57) * (-4.0d+0) 
term(58) = term(58) * (-12.0d+0) 
term(59) = term(59) * 6.0d+0 
term(60) = term(60) * 8.0d+0 
term(61) = term(61) * (-16.0d+0) 
term(62) = term(62) * (-4.0d+0) 
term(63) = term(63) * 8.0d+0 
term(64) = term(64) * (-3.0d+0) 
term(65) = term(65) * 6.0d+0 
term(66) = term(66) * 3.0d+0 
term(67) = term(67) * (-6.0d+0) 
term(68) = term(68) * 9.0d+0 
term(69) = term(69) * (-9.0d+0) 
term(70) = term(70) * 12.0d+0 
term(71) = term(71) * (-12.0d+0) 
term(72) = term(72) * (-6.0d+0) 
term(73) = term(73) * 6.0d+0 
term(74) = term(74) * (-4.0d+0) 
term(75) = term(75) * 8.0d+0 
term(76) = term(76) * 8.0d+0 
term(77) = term(77) * (-16.0d+0) 
term(78) = term(78) * (-12.0d+0) 
term(79) = term(79) * 24.0d+0 
term(80) = term(80) * (-16.0d+0) 
term(81) = term(81) * 32.0d+0 
term(82) = term(82) * 8.0d+0 
term(83) = term(83) * (-16.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do k = 1, nocc 
term(84) = term(84) + r2p(vrdav_Rl, q,i,a,j) * r2m(vrdav_Rr, a,k,b,j) * wm_interm_56_triplet(b, p, k, i)
term(85) = term(85) + r2p(vrdav_Rl, q,i,a,j) * r2m(vrdav_Rr, a,k,b,j) * wm_interm_56_triplet(b, k, p, i)
end do 
end do 
end do 
end do 
end do 

term(84) = term(84) * (-12.0d+0) 
term(85) = term(85) * 12.0d+0 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(86) = term(86) + wm_interm_73_triplet(i, p, j, k) * wm_interm_80_triplet(q, j, k, i)
term(87) = term(87) + wm_interm_73_triplet(p, i, j, k) * wm_interm_80_triplet(q, j, k, i)
term(88) = term(88) + wm_interm_60_triplet(q, i, j, k) * wm_interm_78_triplet(k, j, p, i)
term(89) = term(89) + wm_interm_60_triplet(q, i, j, k) * wm_interm_78_triplet(j, k, p, i)
term(90) = term(90) + wm_interm_60_triplet(q, i, j, k) * wm_interm_79_triplet(k, j, p, i)
term(91) = term(91) + wm_interm_60_triplet(q, i, j, k) * wm_interm_79_triplet(j, k, p, i)
term(92) = term(92) + wm_interm_73_triplet(i, p, j, k) * wm_interm_89_triplet(q, j, k, i)
term(93) = term(93) + wm_interm_73_triplet(p, i, j, k) * wm_interm_89_triplet(q, j, k, i)
term(94) = term(94) + wm_interm_73_triplet(p, i, j, k) * wm_interm_90_triplet(q, j, k, i)
term(95) = term(95) + wm_interm_73_triplet(i, p, j, k) * wm_interm_90_triplet(q, j, k, i)
term(96) = term(96) + wm_interm_60_triplet(q, i, j, k) * wm_interm_87_triplet(k, j, p, i)
term(97) = term(97) + wm_interm_60_triplet(q, i, j, k) * wm_interm_87_triplet(j, k, p, i)
term(98) = term(98) + wm_interm_60_triplet(q, i, j, k) * wm_interm_88_triplet(k, j, p, i)
term(99) = term(99) + wm_interm_60_triplet(q, i, j, k) * wm_interm_88_triplet(j, k, p, i)
end do 
end do 
end do 

term(86) = term(86) * 3.0d+0 
term(87) = term(87) * (-3.0d+0) 
term(88) = term(88) * 3.0d+0 
term(89) = term(89) * (-3.0d+0) 
term(90) = term(90) * 4.0d+0 
term(91) = term(91) * (-2.0d+0) 
term(92) = term(92) * (-2.0d+0) 
term(93) = term(93) * 4.0d+0 
term(94) = term(94) * (-2.0d+0) 
term(95) = term(95) * 4.0d+0 
term(96) = term(96) * 6.0d+0 
term(97) = term(97) * (-6.0d+0) 
term(98) = term(98) * 8.0d+0 
term(99) = term(99) * (-4.0d+0) 

do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do k = 1, nocc 
do i = 1, nocc 
term(100) = term(100) + r2p(vrdav_Rl, q,i,a,j) * t2(a,b,k,j) * wm_interm_59_triplet(b, i, k, p)
end do 
end do 
end do 
end do 
end do 

term(100) = term(100) * (-6.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(101) = term(101) + r2p(vrdav_Rl, q,i,a,j) * t2(a,b,k,j) * wm_interm_59_triplet(b, i, p, k)
term(102) = term(102) + r2p(vrdav_Rl, q,i,a,j) * t2(a,b,j,k) * wm_interm_59_triplet(b, i, p, k)
term(103) = term(103) + r2p(vrdav_Rl, q,i,a,j) * t2(a,b,j,k) * wm_interm_59_triplet(b, i, k, p)
end do 
end do 
end do 
end do 
end do 

term(101) = term(101) * 6.0d+0 
term(102) = term(102) * (-12.0d+0) 
term(103) = term(103) * 6.0d+0 

do i = 1, nocc 
term(104) = term(104) + s1(q,i) * wm_interm_27_triplet(p, i)
term(105) = term(105) + s1(q,i) * wm_interm_30_triplet(p, i)
term(106) = term(106) + s1(q,i) * wm_interm_31_triplet(p, i)
term(107) = term(107) + t1(q,i) * wm_interm_27_triplet(i, p)
term(108) = term(108) + t1(q,i) * wm_interm_30_triplet(i, p)
term(109) = term(109) + t1(q,i) * wm_interm_31_triplet(i, p)
term(110) = term(110) + s1(q,i) * wm_interm_40_triplet(p, i)
term(111) = term(111) + s1(q,i) * wm_interm_41_triplet(p, i)
term(112) = term(112) + s1(q,i) * wm_interm_43_triplet(p, i)
term(113) = term(113) + s1(q,i) * wm_interm_44_triplet(p, i)
term(114) = term(114) + s1(q,i) * wm_interm_45_triplet(p, i)
term(115) = term(115) + s1(q,i) * wm_interm_46_triplet(p, i)
term(116) = term(116) + t1(q,i) * wm_interm_40_triplet(i, p)
term(117) = term(117) + t1(q,i) * wm_interm_41_triplet(i, p)
term(118) = term(118) + t1(q,i) * wm_interm_43_triplet(i, p)
term(119) = term(119) + t1(q,i) * wm_interm_44_triplet(i, p)
term(120) = term(120) + t1(q,i) * wm_interm_45_triplet(i, p)
term(121) = term(121) + t1(q,i) * wm_interm_46_triplet(i, p)
term(122) = term(122) + wm_interm_55_triplet(i, p) * wm_interm_61_triplet(q, i)
term(123) = term(123) + wm_interm_14_triplet(p, i) * wm_interm_63_triplet(q, i)
term(124) = term(124) + wm_interm_55_triplet(i, p) * wm_interm_65_triplet(q, i)
term(125) = term(125) + wm_interm_14_triplet(p, i) * wm_interm_64_triplet(q, i)
term(126) = term(126) + wm_interm_18_triplet(p, i) * wm_interm_63_triplet(q, i)
term(127) = term(127) + wm_interm_19_triplet(p, i) * wm_interm_63_triplet(q, i)
term(128) = term(128) + wm_interm_18_triplet(p, i) * wm_interm_64_triplet(q, i)
term(129) = term(129) + wm_interm_19_triplet(p, i) * wm_interm_64_triplet(q, i)
term(130) = term(130) + wm_interm_23_triplet(p, i) * wm_interm_63_triplet(q, i)
term(131) = term(131) + wm_interm_24_triplet(p, i) * wm_interm_63_triplet(q, i)
term(132) = term(132) + wm_interm_25_triplet(p, i) * wm_interm_63_triplet(q, i)
term(133) = term(133) + wm_interm_26_triplet(p, i) * wm_interm_63_triplet(q, i)
term(134) = term(134) + wm_interm_23_triplet(p, i) * wm_interm_64_triplet(q, i)
term(135) = term(135) + wm_interm_24_triplet(p, i) * wm_interm_64_triplet(q, i)
term(136) = term(136) + wm_interm_25_triplet(p, i) * wm_interm_64_triplet(q, i)
term(137) = term(137) + wm_interm_26_triplet(p, i) * wm_interm_64_triplet(q, i)
term(138) = term(138) + wm_interm_68_triplet(p, i) * wm_interm_7_triplet(q, i)
term(139) = term(139) + wm_interm_69_triplet(p, i) * wm_interm_7_triplet(q, i)
term(140) = term(140) + wm_interm_18_triplet(i, p) * wm_interm_6_triplet(q, i)
term(141) = term(141) + wm_interm_19_triplet(i, p) * wm_interm_6_triplet(q, i)
term(142) = term(142) + wm_interm_23_triplet(i, p) * wm_interm_6_triplet(q, i)
term(143) = term(143) + wm_interm_24_triplet(i, p) * wm_interm_6_triplet(q, i)
term(144) = term(144) + wm_interm_25_triplet(i, p) * wm_interm_6_triplet(q, i)
term(145) = term(145) + wm_interm_26_triplet(i, p) * wm_interm_6_triplet(q, i)
term(146) = term(146) + wm_interm_27_triplet(i, p) * wm_interm_61_triplet(q, i)
term(147) = term(147) + wm_interm_27_triplet(i, p) * wm_interm_65_triplet(q, i)
term(148) = term(148) + wm_interm_30_triplet(i, p) * wm_interm_61_triplet(q, i)
term(149) = term(149) + wm_interm_31_triplet(i, p) * wm_interm_61_triplet(q, i)
term(150) = term(150) + wm_interm_30_triplet(i, p) * wm_interm_65_triplet(q, i)
term(151) = term(151) + wm_interm_31_triplet(i, p) * wm_interm_65_triplet(q, i)
term(152) = term(152) + wm_interm_10_triplet(q, i) * wm_interm_68_triplet(p, i)
term(153) = term(153) + wm_interm_10_triplet(q, i) * wm_interm_69_triplet(p, i)
term(154) = term(154) + wm_interm_11_triplet(q, i) * wm_interm_68_triplet(p, i)
term(155) = term(155) + wm_interm_11_triplet(q, i) * wm_interm_69_triplet(p, i)
term(156) = term(156) + wm_interm_18_triplet(i, p) * wm_interm_8_triplet(q, i)
term(157) = term(157) + wm_interm_19_triplet(i, p) * wm_interm_8_triplet(q, i)
term(158) = term(158) + wm_interm_18_triplet(i, p) * wm_interm_9_triplet(q, i)
term(159) = term(159) + wm_interm_19_triplet(i, p) * wm_interm_9_triplet(q, i)
term(160) = term(160) + wm_interm_23_triplet(i, p) * wm_interm_8_triplet(q, i)
term(161) = term(161) + wm_interm_24_triplet(i, p) * wm_interm_8_triplet(q, i)
term(162) = term(162) + wm_interm_25_triplet(i, p) * wm_interm_8_triplet(q, i)
term(163) = term(163) + wm_interm_26_triplet(i, p) * wm_interm_8_triplet(q, i)
term(164) = term(164) + wm_interm_23_triplet(i, p) * wm_interm_9_triplet(q, i)
term(165) = term(165) + wm_interm_24_triplet(i, p) * wm_interm_9_triplet(q, i)
term(166) = term(166) + wm_interm_25_triplet(i, p) * wm_interm_9_triplet(q, i)
term(167) = term(167) + wm_interm_26_triplet(i, p) * wm_interm_9_triplet(q, i)
term(168) = term(168) + wm_interm_40_triplet(i, p) * wm_interm_61_triplet(q, i)
term(169) = term(169) + wm_interm_41_triplet(i, p) * wm_interm_61_triplet(q, i)
term(170) = term(170) + wm_interm_40_triplet(i, p) * wm_interm_65_triplet(q, i)
term(171) = term(171) + wm_interm_41_triplet(i, p) * wm_interm_65_triplet(q, i)
term(172) = term(172) + wm_interm_43_triplet(i, p) * wm_interm_61_triplet(q, i)
term(173) = term(173) + wm_interm_44_triplet(i, p) * wm_interm_61_triplet(q, i)
term(174) = term(174) + wm_interm_45_triplet(i, p) * wm_interm_61_triplet(q, i)
term(175) = term(175) + wm_interm_46_triplet(i, p) * wm_interm_61_triplet(q, i)
term(176) = term(176) + wm_interm_43_triplet(i, p) * wm_interm_65_triplet(q, i)
term(177) = term(177) + wm_interm_44_triplet(i, p) * wm_interm_65_triplet(q, i)
term(178) = term(178) + wm_interm_45_triplet(i, p) * wm_interm_65_triplet(q, i)
term(179) = term(179) + wm_interm_46_triplet(i, p) * wm_interm_65_triplet(q, i)
end do 

term(104) = term(104) * 6.0d+0 
term(105) = term(105) * 6.0d+0 
term(106) = term(106) * (-6.0d+0) 
term(107) = term(107) * 6.0d+0 
term(108) = term(108) * 6.0d+0 
term(109) = term(109) * (-6.0d+0) 
term(110) = term(110) * (-6.0d+0) 
term(111) = term(111) * 6.0d+0 
term(112) = term(112) * (-4.0d+0) 
term(113) = term(113) * 8.0d+0 
term(114) = term(114) * (-4.0d+0) 
term(115) = term(115) * 8.0d+0 
term(116) = term(116) * (-6.0d+0) 
term(117) = term(117) * 6.0d+0 
term(118) = term(118) * (-4.0d+0) 
term(119) = term(119) * 8.0d+0 
term(120) = term(120) * (-4.0d+0) 
term(121) = term(121) * 8.0d+0 
term(122) = term(122) * 2.0d+0 
term(123) = term(123) * (-4.0d+0) 
term(124) = term(124) * (-4.0d+0) 
term(125) = term(125) * 2.0d+0 
term(126) = term(126) * 6.0d+0 
term(127) = term(127) * (-6.0d+0) 
term(128) = term(128) * (-3.0d+0) 
term(129) = term(129) * 3.0d+0 
term(130) = term(130) * 4.0d+0 
term(131) = term(131) * (-8.0d+0) 
term(132) = term(132) * 4.0d+0 
term(133) = term(133) * (-8.0d+0) 
term(134) = term(134) * (-2.0d+0) 
term(135) = term(135) * 4.0d+0 
term(136) = term(136) * (-2.0d+0) 
term(137) = term(137) * 4.0d+0 
term(138) = term(138) * 6.0d+0 
term(139) = term(139) * (-12.0d+0) 
term(140) = term(140) * (-9.0d+0) 
term(141) = term(141) * 9.0d+0 
term(142) = term(142) * (-6.0d+0) 
term(143) = term(143) * 12.0d+0 
term(144) = term(144) * (-6.0d+0) 
term(145) = term(145) * 12.0d+0 
term(146) = term(146) * 6.0d+0 
term(147) = term(147) * (-12.0d+0) 
term(148) = term(148) * 6.0d+0 
term(149) = term(149) * (-6.0d+0) 
term(150) = term(150) * (-12.0d+0) 
term(151) = term(151) * 12.0d+0 
term(152) = term(152) * (-4.0d+0) 
term(153) = term(153) * 8.0d+0 
term(154) = term(154) * 8.0d+0 
term(155) = term(155) * (-16.0d+0) 
term(156) = term(156) * 12.0d+0 
term(157) = term(157) * (-12.0d+0) 
term(158) = term(158) * (-6.0d+0) 
term(159) = term(159) * 6.0d+0 
term(160) = term(160) * 8.0d+0 
term(161) = term(161) * (-16.0d+0) 
term(162) = term(162) * 8.0d+0 
term(163) = term(163) * (-16.0d+0) 
term(164) = term(164) * (-4.0d+0) 
term(165) = term(165) * 8.0d+0 
term(166) = term(166) * (-4.0d+0) 
term(167) = term(167) * 8.0d+0 
term(168) = term(168) * (-6.0d+0) 
term(169) = term(169) * 6.0d+0 
term(170) = term(170) * 12.0d+0 
term(171) = term(171) * (-12.0d+0) 
term(172) = term(172) * (-4.0d+0) 
term(173) = term(173) * 8.0d+0 
term(174) = term(174) * (-4.0d+0) 
term(175) = term(175) * 8.0d+0 
term(176) = term(176) * 8.0d+0 
term(177) = term(177) * (-16.0d+0) 
term(178) = term(178) * 8.0d+0 
term(179) = term(179) * (-16.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(180) = term(180) + r2m(vrdav_Rl, a,j,q,i) * t2(a,b,k,i) * wm_interm_59_triplet(b, j, k, p)
end do 
end do 
end do 
end do 
end do 

term(180) = term(180) * 4.0d+0 

do j = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(181) = term(181) + r2m(vrdav_Rl, a,i,q,j) * t2(a,b,k,j) * wm_interm_59_triplet(b, i, k, p)
end do 
end do 
end do 
end do 
end do 

term(181) = term(181) * 4.0d+0 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(182) = term(182) + s2(a,b,i,j) * t2(b,q,k,i) * wm_interm_67_triplet(a, k, p, j)
term(183) = term(183) + s2(a,b,j,i) * t2(b,q,k,i) * wm_interm_67_triplet(a, k, p, j)
term(184) = term(184) + s2(a,b,i,j) * t2(b,q,k,i) * wm_interm_72_triplet(a, k, p, j)
term(185) = term(185) + s2(a,b,i,j) * t2(b,q,k,i) * wm_interm_72_triplet(a, k, j, p)
term(186) = term(186) + s2(a,b,j,i) * t2(b,q,k,i) * wm_interm_72_triplet(a, k, p, j)
term(187) = term(187) + r2m(vrdav_Rl, a,j,q,i) * r2m(vrdav_Rr, a,k,b,i) * wm_interm_56_triplet(b, p, k, j)
term(188) = term(188) + r2m(vrdav_Rl, a,j,q,i) * r2m(vrdav_Rr, a,k,b,i) * wm_interm_56_triplet(b, k, p, j)
term(189) = term(189) + r2m(vrdav_Rl, a,i,q,j) * r2m(vrdav_Rr, a,k,b,i) * wm_interm_56_triplet(b, p, k, j)
term(190) = term(190) + r2m(vrdav_Rl, a,i,q,j) * r2m(vrdav_Rr, a,k,b,i) * wm_interm_56_triplet(b, k, p, j)
term(191) = term(191) + r2m(vrdav_Rl, a,j,q,i) * r2m(vrdav_Rr, a,k,b,j) * wm_interm_56_triplet(b, p, k, i)
term(192) = term(192) + r2m(vrdav_Rl, a,i,q,j) * r2m(vrdav_Rr, a,k,b,j) * wm_interm_56_triplet(b, p, k, i)
term(193) = term(193) + r2m(vrdav_Rl, a,j,q,i) * r2m(vrdav_Rr, a,k,b,j) * wm_interm_56_triplet(b, k, p, i)
term(194) = term(194) + r2m(vrdav_Rl, a,i,q,j) * r2m(vrdav_Rr, a,k,b,j) * wm_interm_56_triplet(b, k, p, i)
term(195) = term(195) + r2m(vrdav_Rl, a,j,q,i) * t2(a,b,k,j) * wm_interm_59_triplet(b, i, k, p)
term(196) = term(196) + r2m(vrdav_Rl, a,i,q,j) * t2(a,b,k,i) * wm_interm_59_triplet(b, j, k, p)
end do 
end do 
end do 
end do 
end do 

term(182) = term(182) * (-6.0d+0) 
term(183) = term(183) * 6.0d+0 
term(184) = term(184) * (-4.0d+0) 
term(185) = term(185) * 8.0d+0 
term(186) = term(186) * 8.0d+0 
term(187) = term(187) * 8.0d+0 
term(188) = term(188) * (-4.0d+0) 
term(189) = term(189) * (-4.0d+0) 
term(190) = term(190) * 8.0d+0 
term(191) = term(191) * (-4.0d+0) 
term(192) = term(192) * 8.0d+0 
term(193) = term(193) * 8.0d+0 
term(194) = term(194) * (-4.0d+0) 
term(195) = term(195) * (-2.0d+0) 
term(196) = term(196) * (-2.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(197) = term(197) + s2(a,b,j,k) * t2(b,q,k,i) * wm_interm_67_triplet(a, i, p, j)
term(198) = term(198) + s2(a,b,j,k) * t2(b,q,k,i) * wm_interm_72_triplet(a, i, p, j)
term(199) = term(199) + r2p(vrdav_Rl, q,i,a,j) * r2m(vrdav_Rr, a,j,b,k) * wm_interm_56_triplet(b, p, k, i)
term(200) = term(200) + r2p(vrdav_Rl, q,i,a,j) * r2m(vrdav_Rr, a,j,b,k) * wm_interm_56_triplet(b, k, p, i)
term(201) = term(201) + r2m(vrdav_Rl, a,j,q,i) * r2m(vrdav_Rr, a,i,b,k) * wm_interm_56_triplet(b, p, k, j)
term(202) = term(202) + r2m(vrdav_Rl, a,j,q,i) * r2m(vrdav_Rr, a,i,b,k) * wm_interm_56_triplet(b, k, p, j)
term(203) = term(203) + r2m(vrdav_Rl, a,i,q,j) * r2m(vrdav_Rr, a,j,b,k) * wm_interm_56_triplet(b, p, k, i)
term(204) = term(204) + r2m(vrdav_Rl, a,i,q,j) * r2m(vrdav_Rr, a,j,b,k) * wm_interm_56_triplet(b, k, p, i)
term(205) = term(205) + r2m(vrdav_Rl, a,j,q,i) * t2(a,b,k,j) * wm_interm_59_triplet(b, i, p, k)
term(206) = term(206) + r2m(vrdav_Rl, a,i,q,j) * t2(a,b,k,i) * wm_interm_59_triplet(b, j, p, k)
term(207) = term(207) + r2m(vrdav_Rl, a,i,b,j) * t2(a,q,k,i) * wm_interm_29_triplet(b, j, p, k)
term(208) = term(208) + r2m(vrdav_Rl, a,i,b,j) * t2(b,q,k,i) * wm_interm_29_triplet(a, j, p, k)
term(209) = term(209) + r2m(vrdav_Rl, a,i,b,j) * t2(a,q,k,i) * wm_interm_34_triplet(b, p, j, k)
term(210) = term(210) + r2m(vrdav_Rl, a,i,b,j) * t2(a,q,k,i) * wm_interm_34_triplet(b, j, p, k)
term(211) = term(211) + r2m(vrdav_Rl, a,i,b,j) * t2(b,q,k,i) * wm_interm_34_triplet(a, j, p, k)
term(212) = term(212) + r2m(vrdav_Rl, a,i,b,j) * t2(b,q,k,i) * wm_interm_34_triplet(a, p, j, k)
end do 
end do 
end do 
end do 
end do 

term(197) = term(197) * (-12.0d+0) 
term(198) = term(198) * (-16.0d+0) 
term(199) = term(199) * 12.0d+0 
term(200) = term(200) * (-24.0d+0) 
term(201) = term(201) * (-4.0d+0) 
term(202) = term(202) * 8.0d+0 
term(203) = term(203) * (-4.0d+0) 
term(204) = term(204) * 8.0d+0 
term(205) = term(205) * 4.0d+0 
term(206) = term(206) * 4.0d+0 
term(207) = term(207) * 6.0d+0 
term(208) = term(208) * (-6.0d+0) 
term(209) = term(209) * (-4.0d+0) 
term(210) = term(210) * 8.0d+0 
term(211) = term(211) * (-4.0d+0) 
term(212) = term(212) * 8.0d+0 

do k = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(213) = term(213) + s2(a,b,j,k) * t2(b,q,j,i) * wm_interm_67_triplet(a, i, p, k)
term(214) = term(214) + s2(a,b,j,k) * t2(b,q,k,i) * wm_interm_72_triplet(a, i, j, p)
term(215) = term(215) + s2(a,b,j,k) * t2(b,q,j,i) * wm_interm_72_triplet(a, i, k, p)
term(216) = term(216) + s2(a,b,j,k) * t2(b,q,j,i) * wm_interm_72_triplet(a, i, p, k)
term(217) = term(217) + r2p(vrdav_Rl, a,j,b,k) * t2(b,q,k,i) * wm_interm_29_triplet(a, j, p, i)
term(218) = term(218) + r2p(vrdav_Rl, a,j,b,i) * t2(b,q,k,i) * wm_interm_29_triplet(a, j, p, k)
term(219) = term(219) + r2p(vrdav_Rl, a,j,b,k) * t2(b,q,k,i) * wm_interm_34_triplet(a, p, j, i)
term(220) = term(220) + r2p(vrdav_Rl, a,j,b,k) * t2(b,q,k,i) * wm_interm_34_triplet(a, j, p, i)
term(221) = term(221) + r2p(vrdav_Rl, a,j,b,i) * t2(b,q,k,i) * wm_interm_34_triplet(a, p, j, k)
term(222) = term(222) + r2p(vrdav_Rl, a,j,b,i) * t2(b,q,k,i) * wm_interm_34_triplet(a, j, p, k)
term(223) = term(223) + r2m(vrdav_Rl, a,j,q,i) * r2m(vrdav_Rr, a,j,b,k) * wm_interm_56_triplet(b, p, k, i)
term(224) = term(224) + r2m(vrdav_Rl, a,j,q,i) * r2m(vrdav_Rr, a,j,b,k) * wm_interm_56_triplet(b, k, p, i)
term(225) = term(225) + r2m(vrdav_Rl, a,j,q,i) * t2(a,b,j,k) * wm_interm_59_triplet(b, i, p, k)
term(226) = term(226) + r2m(vrdav_Rl, a,j,q,i) * t2(a,b,j,k) * wm_interm_59_triplet(b, i, k, p)
term(227) = term(227) + r2m(vrdav_Rl, a,j,q,i) * t2(a,b,k,i) * wm_interm_59_triplet(b, j, p, k)
term(228) = term(228) + r2m(vrdav_Rl, a,j,q,i) * t2(a,b,i,k) * wm_interm_59_triplet(b, j, p, k)
term(229) = term(229) + r2m(vrdav_Rl, a,j,q,i) * t2(a,b,i,k) * wm_interm_59_triplet(b, j, k, p)
term(230) = term(230) + r2m(vrdav_Rl, a,j,b,k) * t2(a,q,k,i) * wm_interm_29_triplet(b, j, p, i)
term(231) = term(231) + r2m(vrdav_Rl, a,j,b,k) * t2(a,q,j,i) * wm_interm_29_triplet(b, k, p, i)
term(232) = term(232) + r2m(vrdav_Rl, a,j,b,k) * t2(b,q,j,i) * wm_interm_29_triplet(a, k, p, i)
term(233) = term(233) + r2m(vrdav_Rl, a,j,b,k) * t2(b,q,k,i) * wm_interm_29_triplet(a, j, p, i)
term(234) = term(234) + r2m(vrdav_Rl, a,j,b,i) * t2(a,q,k,i) * wm_interm_29_triplet(b, j, p, k)
term(235) = term(235) + r2m(vrdav_Rl, a,j,b,i) * t2(b,q,k,i) * wm_interm_29_triplet(a, j, p, k)
term(236) = term(236) + r2m(vrdav_Rl, a,j,b,k) * t2(a,q,k,i) * wm_interm_34_triplet(b, p, j, i)
term(237) = term(237) + r2m(vrdav_Rl, a,j,b,k) * t2(a,q,k,i) * wm_interm_34_triplet(b, j, p, i)
term(238) = term(238) + r2m(vrdav_Rl, a,j,b,k) * t2(a,q,j,i) * wm_interm_34_triplet(b, p, k, i)
term(239) = term(239) + r2m(vrdav_Rl, a,j,b,k) * t2(a,q,j,i) * wm_interm_34_triplet(b, k, p, i)
term(240) = term(240) + r2m(vrdav_Rl, a,j,b,k) * t2(b,q,j,i) * wm_interm_34_triplet(a, p, k, i)
term(241) = term(241) + r2m(vrdav_Rl, a,j,b,k) * t2(b,q,j,i) * wm_interm_34_triplet(a, k, p, i)
term(242) = term(242) + r2m(vrdav_Rl, a,j,b,k) * t2(b,q,k,i) * wm_interm_34_triplet(a, p, j, i)
term(243) = term(243) + r2m(vrdav_Rl, a,j,b,k) * t2(b,q,k,i) * wm_interm_34_triplet(a, j, p, i)
term(244) = term(244) + r2m(vrdav_Rl, a,j,b,i) * t2(a,q,k,i) * wm_interm_34_triplet(b, j, p, k)
term(245) = term(245) + r2m(vrdav_Rl, a,j,b,i) * t2(a,q,k,i) * wm_interm_34_triplet(b, p, j, k)
term(246) = term(246) + r2m(vrdav_Rl, a,j,b,i) * t2(b,q,k,i) * wm_interm_34_triplet(a, p, j, k)
term(247) = term(247) + r2m(vrdav_Rl, a,j,b,i) * t2(b,q,k,i) * wm_interm_34_triplet(a, j, p, k)
end do 
end do 
end do 
end do 
end do 

term(213) = term(213) * 6.0d+0 
term(214) = term(214) * 8.0d+0 
term(215) = term(215) * (-4.0d+0) 
term(216) = term(216) * 8.0d+0 
term(217) = term(217) * (-18.0d+0) 
term(218) = term(218) * 12.0d+0 
term(219) = term(219) * 12.0d+0 
term(220) = term(220) * (-24.0d+0) 
term(221) = term(221) * (-12.0d+0) 
term(222) = term(222) * 12.0d+0 
term(223) = term(223) * 8.0d+0 
term(224) = term(224) * (-16.0d+0) 
term(225) = term(225) * (-8.0d+0) 
term(226) = term(226) * 4.0d+0 
term(227) = term(227) * (-2.0d+0) 
term(228) = term(228) * 4.0d+0 
term(229) = term(229) * (-2.0d+0) 
term(230) = term(230) * 6.0d+0 
term(231) = term(231) * (-12.0d+0) 
term(232) = term(232) * 6.0d+0 
term(233) = term(233) * (-12.0d+0) 
term(234) = term(234) * (-6.0d+0) 
term(235) = term(235) * 6.0d+0 
term(236) = term(236) * (-4.0d+0) 
term(237) = term(237) * 8.0d+0 
term(238) = term(238) * 8.0d+0 
term(239) = term(239) * (-16.0d+0) 
term(240) = term(240) * (-4.0d+0) 
term(241) = term(241) * 8.0d+0 
term(242) = term(242) * 8.0d+0 
term(243) = term(243) * (-16.0d+0) 
term(244) = term(244) * (-4.0d+0) 
term(245) = term(245) * 8.0d+0 
term(246) = term(246) * (-4.0d+0) 
term(247) = term(247) * 8.0d+0 


    calc_D_ov_wm_triplet = zero
    do s = 0, 247
    calc_D_ov_wm_triplet = calc_D_ov_wm_triplet + term(s)
    end do

    end function calc_D_ov_wm_triplet
    
    function calc_D_vo_wm_triplet(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vo_wm_triplet
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    double precision, dimension(:), intent(in) :: vrdav_Rl
    double precision, dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , j, a, i, k, b 
    real(F64), dimension(0:659) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do j = 1, nocc 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(0) = term(0) + r2p(vrdav_Rl, p,i,a,j) * wm_interm_29_triplet(a, i, j, q)
term(1) = term(1) + r2p(vrdav_Rl, p,i,a,j) * wm_interm_34_triplet(a, i, j, q)
term(2) = term(2) + r2p(vrdav_Rl, p,i,a,j) * wm_interm_34_triplet(a, j, i, q)
term(3) = term(3) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_35_triplet(a, i, j, q)
term(4) = term(4) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_49_triplet(a, i, j, q)
term(5) = term(5) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_50_triplet(a, i, j, q)
end do 
end do 
end do 

term(0) = term(0) * (-6.0d+0) 
term(1) = term(1) * (-6.0d+0) 
term(2) = term(2) * 6.0d+0 
term(3) = term(3) * (-6.0d+0) 
term(4) = term(4) * 6.0d+0 
term(5) = term(5) * (-6.0d+0) 

do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(6) = term(6) + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,q,a,j) * wm_interm_61_triplet(b, i)
term(7) = term(7) + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,q,a,j) * wm_interm_65_triplet(b, i)
end do 
end do 
end do 
end do 

term(6) = term(6) * 6.0d+0 
term(7) = term(7) * (-12.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(8) = term(8) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_35_triplet(a, i, j, q)
term(9) = term(9) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_35_triplet(a, j, i, q)
term(10) = term(10) + r2m(vrdav_Rl, a,i,p,j) * wm_interm_29_triplet(a, i, j, q)
term(11) = term(11) + r2m(vrdav_Rl, a,i,p,j) * wm_interm_34_triplet(a, j, i, q)
term(12) = term(12) + r2m(vrdav_Rl, a,i,p,j) * wm_interm_34_triplet(a, i, j, q)
term(13) = term(13) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_49_triplet(a, i, j, q)
term(14) = term(14) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_49_triplet(a, j, i, q)
term(15) = term(15) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_50_triplet(a, j, i, q)
term(16) = term(16) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_50_triplet(a, i, j, q)
end do 
end do 
end do 

term(8) = term(8) * 6.0d+0 
term(9) = term(9) * (-6.0d+0) 
term(10) = term(10) * 6.0d+0 
term(11) = term(11) * (-4.0d+0) 
term(12) = term(12) * 8.0d+0 
term(13) = term(13) * (-4.0d+0) 
term(14) = term(14) * 8.0d+0 
term(15) = term(15) * (-4.0d+0) 
term(16) = term(16) * 8.0d+0 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(17) = term(17) + r2m(vrdav_Rl, a,j,p,i) * wm_interm_29_triplet(a, i, j, q)
term(18) = term(18) + r2m(vrdav_Rl, a,j,p,i) * wm_interm_34_triplet(a, i, j, q)
term(19) = term(19) + r2m(vrdav_Rl, a,j,p,i) * wm_interm_34_triplet(a, j, i, q)
end do 
end do 
end do 

term(17) = term(17) * (-6.0d+0) 
term(18) = term(18) * (-4.0d+0) 
term(19) = term(19) * 8.0d+0 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(20) = term(20) + wm_interm_67_triplet(p, i, j, k) * wm_interm_73_triplet(k, j, i, q)
term(21) = term(21) + wm_interm_67_triplet(p, i, j, k) * wm_interm_73_triplet(j, k, i, q)
term(22) = term(22) + wm_interm_72_triplet(p, i, j, k) * wm_interm_73_triplet(k, j, i, q)
term(23) = term(23) + wm_interm_72_triplet(p, i, j, k) * wm_interm_73_triplet(j, k, i, q)
term(24) = term(24) + wm_interm_56_triplet(p, i, j, k) * wm_interm_78_triplet(k, q, i, j)
term(25) = term(25) + wm_interm_56_triplet(p, i, j, k) * wm_interm_79_triplet(k, q, i, j)
term(26) = term(26) + wm_interm_59_triplet(p, i, j, k) * wm_interm_81_triplet(i, q, j, k)
term(27) = term(27) + wm_interm_59_triplet(p, i, j, k) * wm_interm_81_triplet(i, q, k, j)
term(28) = term(28) + wm_interm_29_triplet(p, i, j, k) * wm_interm_81_triplet(i, j, k, q)
term(29) = term(29) + wm_interm_29_triplet(p, i, j, k) * wm_interm_81_triplet(i, j, q, k)
term(30) = term(30) + wm_interm_81_triplet(i, j, k, q) * wm_interm_83_triplet(p, i, j, k)
term(31) = term(31) + wm_interm_81_triplet(i, j, q, k) * wm_interm_83_triplet(p, i, j, k)
term(32) = term(32) + wm_interm_34_triplet(p, i, j, k) * wm_interm_81_triplet(j, i, q, k)
term(33) = term(33) + wm_interm_34_triplet(p, i, j, k) * wm_interm_81_triplet(i, j, q, k)
term(34) = term(34) + wm_interm_34_triplet(p, i, j, k) * wm_interm_81_triplet(i, j, k, q)
term(35) = term(35) + wm_interm_34_triplet(p, i, j, k) * wm_interm_81_triplet(j, i, k, q)
term(36) = term(36) + wm_interm_56_triplet(p, i, j, k) * wm_interm_87_triplet(q, k, i, j)
term(37) = term(37) + wm_interm_56_triplet(p, i, j, k) * wm_interm_87_triplet(k, q, i, j)
term(38) = term(38) + wm_interm_56_triplet(p, i, j, k) * wm_interm_88_triplet(q, k, i, j)
term(39) = term(39) + wm_interm_56_triplet(p, i, j, k) * wm_interm_88_triplet(k, q, i, j)
term(40) = term(40) + wm_interm_59_triplet(p, i, j, k) * wm_interm_91_triplet(q, i, j, k)
term(41) = term(41) + wm_interm_59_triplet(p, i, j, k) * wm_interm_91_triplet(i, q, j, k)
term(42) = term(42) + wm_interm_59_triplet(p, i, j, k) * wm_interm_91_triplet(i, q, k, j)
term(43) = term(43) + wm_interm_59_triplet(p, i, j, k) * wm_interm_91_triplet(q, i, k, j)
term(44) = term(44) + wm_interm_29_triplet(p, i, j, k) * wm_interm_91_triplet(i, j, k, q)
term(45) = term(45) + wm_interm_29_triplet(p, i, j, k) * wm_interm_91_triplet(i, j, q, k)
term(46) = term(46) + wm_interm_83_triplet(p, i, j, k) * wm_interm_91_triplet(j, i, k, q)
term(47) = term(47) + wm_interm_83_triplet(p, i, j, k) * wm_interm_91_triplet(i, j, k, q)
term(48) = term(48) + wm_interm_83_triplet(p, i, j, k) * wm_interm_91_triplet(i, j, q, k)
term(49) = term(49) + wm_interm_83_triplet(p, i, j, k) * wm_interm_91_triplet(j, i, q, k)
term(50) = term(50) + wm_interm_34_triplet(p, i, j, k) * wm_interm_91_triplet(j, i, q, k)
term(51) = term(51) + wm_interm_34_triplet(p, i, j, k) * wm_interm_91_triplet(i, j, q, k)
term(52) = term(52) + wm_interm_34_triplet(p, i, j, k) * wm_interm_91_triplet(i, j, k, q)
term(53) = term(53) + wm_interm_34_triplet(p, i, j, k) * wm_interm_91_triplet(j, i, k, q)
end do 
end do 
end do 

term(20) = term(20) * (-3.0d+0) 
term(21) = term(21) * 3.0d+0 
term(22) = term(22) * (-4.0d+0) 
term(23) = term(23) * 8.0d+0 
term(24) = term(24) * (-3.0d+0) 
term(25) = term(25) * (-6.0d+0) 
term(26) = term(26) * 3.0d+0 
term(27) = term(27) * (-3.0d+0) 
term(28) = term(28) * (-3.0d+0) 
term(29) = term(29) * 3.0d+0 
term(30) = term(30) * 2.0d+0 
term(31) = -term(31) 
term(32) = -term(32) 
term(33) = term(33) * 2.0d+0 
term(34) = -term(34) 
term(35) = term(35) * 2.0d+0 
term(36) = term(36) * 3.0d+0 
term(37) = term(37) * (-3.0d+0) 
term(38) = term(38) * 8.0d+0 
term(39) = term(39) * (-4.0d+0) 
term(40) = term(40) * (-2.0d+0) 
term(41) = term(41) * 4.0d+0 
term(42) = term(42) * (-2.0d+0) 
term(43) = term(43) * 4.0d+0 
term(44) = term(44) * (-6.0d+0) 
term(45) = term(45) * 6.0d+0 
term(46) = term(46) * (-2.0d+0) 
term(47) = term(47) * 4.0d+0 
term(48) = term(48) * (-2.0d+0) 
term(49) = term(49) * 4.0d+0 
term(50) = term(50) * (-2.0d+0) 
term(51) = term(51) * 4.0d+0 
term(52) = term(52) * (-2.0d+0) 
term(53) = term(53) * 4.0d+0 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(54) = term(54) + wm_interm_56_triplet(p, i, j, k) * wm_interm_78_triplet(k, q, j, i)
term(55) = term(55) + wm_interm_56_triplet(p, i, j, k) * wm_interm_79_triplet(k, q, j, i)
term(56) = term(56) + wm_interm_81_triplet(i, j, k, q) * wm_interm_83_triplet(p, j, i, k)
term(57) = term(57) + wm_interm_81_triplet(i, j, q, k) * wm_interm_83_triplet(p, j, i, k)
term(58) = term(58) + wm_interm_56_triplet(p, i, j, k) * wm_interm_87_triplet(q, k, j, i)
term(59) = term(59) + wm_interm_56_triplet(p, i, j, k) * wm_interm_87_triplet(k, q, j, i)
term(60) = term(60) + wm_interm_56_triplet(p, i, j, k) * wm_interm_88_triplet(q, k, j, i)
term(61) = term(61) + wm_interm_56_triplet(p, i, j, k) * wm_interm_88_triplet(k, q, j, i)
end do 
end do 
end do 

term(54) = term(54) * 3.0d+0 
term(55) = term(55) * 6.0d+0 
term(56) = -term(56) 
term(57) = term(57) * 2.0d+0 
term(58) = term(58) * (-3.0d+0) 
term(59) = term(59) * 3.0d+0 
term(60) = term(60) * (-4.0d+0) 
term(61) = term(61) * 8.0d+0 

do a = nocc + 1, nactive 
term(62) = term(62) + wm_interm_61_triplet(a, q) * wm_interm_62_triplet(a, p)
term(63) = term(63) + wm_interm_12_triplet(p, a) * wm_interm_63_triplet(a, q)
term(64) = term(64) + wm_interm_12_triplet(p, a) * wm_interm_64_triplet(a, q)
term(65) = term(65) + wm_interm_62_triplet(a, p) * wm_interm_65_triplet(a, q)
term(66) = term(66) + wm_interm_17_triplet(p, a) * wm_interm_63_triplet(a, q)
term(67) = term(67) + wm_interm_20_triplet(p, a) * wm_interm_63_triplet(a, q)
term(68) = term(68) + wm_interm_17_triplet(p, a) * wm_interm_64_triplet(a, q)
term(69) = term(69) + wm_interm_20_triplet(p, a) * wm_interm_64_triplet(a, q)
term(70) = term(70) + wm_interm_63_triplet(a, q) * wm_interm_75_triplet(p, a)
term(71) = term(71) + wm_interm_63_triplet(a, q) * wm_interm_76_triplet(p, a)
term(72) = term(72) + wm_interm_21_triplet(p, a) * wm_interm_63_triplet(a, q)
term(73) = term(73) + wm_interm_22_triplet(p, a) * wm_interm_63_triplet(a, q)
term(74) = term(74) + wm_interm_64_triplet(a, q) * wm_interm_75_triplet(p, a)
term(75) = term(75) + wm_interm_64_triplet(a, q) * wm_interm_76_triplet(p, a)
term(76) = term(76) + wm_interm_21_triplet(p, a) * wm_interm_64_triplet(a, q)
term(77) = term(77) + wm_interm_22_triplet(p, a) * wm_interm_64_triplet(a, q)
term(78) = term(78) + wm_interm_70_triplet(p, a) * wm_interm_7_triplet(a, q)
term(79) = term(79) + wm_interm_71_triplet(p, a) * wm_interm_7_triplet(a, q)
term(80) = term(80) + wm_interm_17_triplet(a, p) * wm_interm_6_triplet(a, q)
term(81) = term(81) + wm_interm_20_triplet(a, p) * wm_interm_6_triplet(a, q)
term(82) = term(82) + wm_interm_21_triplet(a, p) * wm_interm_6_triplet(a, q)
term(83) = term(83) + wm_interm_22_triplet(a, p) * wm_interm_6_triplet(a, q)
term(84) = term(84) + wm_interm_61_triplet(a, q) * wm_interm_84_triplet(a, p)
term(85) = term(85) + wm_interm_65_triplet(a, q) * wm_interm_84_triplet(a, p)
term(86) = term(86) + wm_interm_61_triplet(a, q) * wm_interm_85_triplet(a, p)
term(87) = term(87) + wm_interm_61_triplet(a, q) * wm_interm_86_triplet(a, p)
term(88) = term(88) + wm_interm_65_triplet(a, q) * wm_interm_85_triplet(a, p)
term(89) = term(89) + wm_interm_65_triplet(a, q) * wm_interm_86_triplet(a, p)
term(90) = term(90) + wm_interm_10_triplet(a, q) * wm_interm_70_triplet(p, a)
term(91) = term(91) + wm_interm_10_triplet(a, q) * wm_interm_71_triplet(p, a)
term(92) = term(92) + wm_interm_11_triplet(a, q) * wm_interm_70_triplet(p, a)
term(93) = term(93) + wm_interm_11_triplet(a, q) * wm_interm_71_triplet(p, a)
term(94) = term(94) + wm_interm_17_triplet(a, p) * wm_interm_8_triplet(a, q)
term(95) = term(95) + wm_interm_17_triplet(a, p) * wm_interm_9_triplet(a, q)
term(96) = term(96) + wm_interm_20_triplet(a, p) * wm_interm_8_triplet(a, q)
term(97) = term(97) + wm_interm_20_triplet(a, p) * wm_interm_9_triplet(a, q)
term(98) = term(98) + wm_interm_21_triplet(a, p) * wm_interm_8_triplet(a, q)
term(99) = term(99) + wm_interm_22_triplet(a, p) * wm_interm_8_triplet(a, q)
term(100) = term(100) + wm_interm_21_triplet(a, p) * wm_interm_9_triplet(a, q)
term(101) = term(101) + wm_interm_22_triplet(a, p) * wm_interm_9_triplet(a, q)
term(102) = term(102) + wm_interm_61_triplet(a, q) * wm_interm_92_triplet(a, p)
term(103) = term(103) + wm_interm_61_triplet(a, q) * wm_interm_93_triplet(a, p)
term(104) = term(104) + wm_interm_65_triplet(a, q) * wm_interm_92_triplet(a, p)
term(105) = term(105) + wm_interm_65_triplet(a, q) * wm_interm_93_triplet(a, p)
term(106) = term(106) + wm_interm_61_triplet(a, q) * wm_interm_94_triplet(a, p)
term(107) = term(107) + wm_interm_61_triplet(a, q) * wm_interm_95_triplet(a, p)
term(108) = term(108) + wm_interm_61_triplet(a, q) * wm_interm_96_triplet(a, p)
term(109) = term(109) + wm_interm_61_triplet(a, q) * wm_interm_97_triplet(a, p)
term(110) = term(110) + wm_interm_65_triplet(a, q) * wm_interm_94_triplet(a, p)
term(111) = term(111) + wm_interm_65_triplet(a, q) * wm_interm_95_triplet(a, p)
term(112) = term(112) + wm_interm_65_triplet(a, q) * wm_interm_96_triplet(a, p)
term(113) = term(113) + wm_interm_65_triplet(a, q) * wm_interm_97_triplet(a, p)
end do 

term(62) = term(62) * 2.0d+0 
term(63) = term(63) * (-4.0d+0) 
term(64) = term(64) * 2.0d+0 
term(65) = term(65) * (-4.0d+0) 
term(66) = term(66) * (-6.0d+0) 
term(67) = term(67) * 6.0d+0 
term(68) = term(68) * 3.0d+0 
term(69) = term(69) * (-3.0d+0) 
term(70) = term(70) * 4.0d+0 
term(71) = term(71) * (-8.0d+0) 
term(72) = term(72) * 4.0d+0 
term(73) = term(73) * (-8.0d+0) 
term(74) = term(74) * (-2.0d+0) 
term(75) = term(75) * 4.0d+0 
term(76) = term(76) * (-2.0d+0) 
term(77) = term(77) * 4.0d+0 
term(78) = term(78) * (-12.0d+0) 
term(79) = term(79) * 6.0d+0 
term(80) = term(80) * 9.0d+0 
term(81) = term(81) * (-9.0d+0) 
term(82) = term(82) * (-12.0d+0) 
term(83) = term(83) * 24.0d+0 
term(84) = term(84) * 6.0d+0 
term(85) = term(85) * (-12.0d+0) 
term(86) = term(86) * 6.0d+0 
term(87) = term(87) * (-6.0d+0) 
term(88) = term(88) * (-12.0d+0) 
term(89) = term(89) * 12.0d+0 
term(90) = term(90) * 8.0d+0 
term(91) = term(91) * (-4.0d+0) 
term(92) = term(92) * (-16.0d+0) 
term(93) = term(93) * 8.0d+0 
term(94) = term(94) * (-12.0d+0) 
term(95) = term(95) * 6.0d+0 
term(96) = term(96) * 12.0d+0 
term(97) = term(97) * (-6.0d+0) 
term(98) = term(98) * 16.0d+0 
term(99) = term(99) * (-32.0d+0) 
term(100) = term(100) * (-8.0d+0) 
term(101) = term(101) * 16.0d+0 
term(102) = term(102) * (-6.0d+0) 
term(103) = term(103) * 6.0d+0 
term(104) = term(104) * 12.0d+0 
term(105) = term(105) * (-12.0d+0) 
term(106) = term(106) * (-4.0d+0) 
term(107) = term(107) * 8.0d+0 
term(108) = term(108) * (-4.0d+0) 
term(109) = term(109) * 8.0d+0 
term(110) = term(110) * 8.0d+0 
term(111) = term(111) * (-16.0d+0) 
term(112) = term(112) * 8.0d+0 
term(113) = term(113) * (-16.0d+0) 

do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(114) = term(114) + r1(vrdav_Rl, a,i) * t2(b,p,i,q) * wm_interm_17_triplet(a, b)
term(115) = term(115) + r1(vrdav_Rl, a,i) * t2(b,p,i,q) * wm_interm_20_triplet(a, b)
term(116) = term(116) + r1(vrdav_Rl, a,i) * t2(b,p,i,q) * wm_interm_21_triplet(a, b)
term(117) = term(117) + r1(vrdav_Rl, a,i) * t2(b,p,i,q) * wm_interm_22_triplet(a, b)
term(118) = term(118) + r1(vrdav_Rr, a,i) * s2(b,p,i,q) * wm_interm_38_triplet(a, b)
term(119) = term(119) + r1(vrdav_Rr, a,i) * s2(b,p,i,q) * wm_interm_39_triplet(a, b)
term(120) = term(120) + r1(vrdav_Rr, a,i) * s2(b,p,i,q) * wm_interm_53_triplet(a, b)
term(121) = term(121) + r1(vrdav_Rr, a,i) * s2(b,p,i,q) * wm_interm_54_triplet(a, b)
end do 
end do 
end do 

term(114) = term(114) * (-6.0d+0) 
term(115) = term(115) * 6.0d+0 
term(116) = term(116) * 8.0d+0 
term(117) = term(117) * (-16.0d+0) 
term(118) = term(118) * (-6.0d+0) 
term(119) = term(119) * 6.0d+0 
term(120) = term(120) * 8.0d+0 
term(121) = term(121) * (-16.0d+0) 

do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do k = 1, nocc 
do i = 1, nocc 
term(122) = term(122) + r2p(vrdav_Rr, p,i,a,j) * s2(a,b,k,j) * wm_interm_66_triplet(b, i, k, q)
end do 
end do 
end do 
end do 
end do 

term(122) = term(122) * (-6.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(123) = term(123) + r2p(vrdav_Rr, p,i,a,j) * s2(a,b,j,k) * wm_interm_66_triplet(b, i, k, q)
term(124) = term(124) + r2p(vrdav_Rr, p,i,a,j) * s2(a,b,j,k) * wm_interm_66_triplet(b, i, q, k)
term(125) = term(125) + r2p(vrdav_Rr, p,i,a,j) * s2(a,b,k,j) * wm_interm_66_triplet(b, i, q, k)
term(126) = term(126) + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,q,a,k) * wm_interm_82_triplet(b, k, i, j)
end do 
end do 
end do 
end do 
end do 

term(123) = term(123) * 6.0d+0 
term(124) = term(124) * (-12.0d+0) 
term(125) = term(125) * 6.0d+0 
term(126) = term(126) * 6.0d+0 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(127) = term(127) + r2m(vrdav_Rr, a,i,p,j) * s2(a,b,k,i) * wm_interm_66_triplet(b, j, k, q)
term(128) = term(128) + r2m(vrdav_Rr, a,j,p,i) * s2(a,b,k,j) * wm_interm_66_triplet(b, i, k, q)
term(129) = term(129) + s2(a,p,j,i) * t2(a,b,k,i) * wm_interm_80_triplet(b, k, q, j)
term(130) = term(130) + s2(a,p,j,i) * t2(a,b,k,j) * wm_interm_80_triplet(b, k, q, i)
term(131) = term(131) + r2p(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,k,p,q) * wm_interm_82_triplet(a, k, j, i)
term(132) = term(132) + r2p(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,k,b,j) * wm_interm_82_triplet(a, k, q, i)
term(133) = term(133) + r2p(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,k,p,j) * wm_interm_82_triplet(a, k, q, i)
term(134) = term(134) + s2(a,p,j,i) * t2(a,b,k,i) * wm_interm_90_triplet(b, q, k, j)
term(135) = term(135) + s2(a,p,j,i) * t2(a,b,k,j) * wm_interm_90_triplet(b, q, k, i)
term(136) = term(136) + s2(a,p,j,i) * t2(a,b,k,i) * wm_interm_89_triplet(b, k, q, j)
term(137) = term(137) + s2(a,p,j,i) * t2(a,b,k,i) * wm_interm_90_triplet(b, k, q, j)
term(138) = term(138) + s2(a,p,j,i) * t2(a,b,k,j) * wm_interm_89_triplet(b, k, q, i)
term(139) = term(139) + s2(a,p,j,i) * t2(a,b,k,j) * wm_interm_90_triplet(b, k, q, i)
term(140) = term(140) + s2(a,p,j,i) * t2(a,b,k,i) * wm_interm_89_triplet(b, q, k, j)
term(141) = term(141) + s2(a,p,j,i) * t2(a,b,k,j) * wm_interm_89_triplet(b, q, k, i)
term(142) = term(142) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,k,p,q) * wm_interm_82_triplet(a, k, j, i)
term(143) = term(143) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,k,p,q) * wm_interm_82_triplet(b, k, j, i)
term(144) = term(144) + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,k,b,i) * wm_interm_82_triplet(a, k, j, q)
term(145) = term(145) + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,k,b,j) * wm_interm_82_triplet(a, k, q, i)
term(146) = term(146) + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,k,b,i) * wm_interm_82_triplet(a, k, q, j)
term(147) = term(147) + r2m(vrdav_Rl, a,k,b,j) * t2(a,p,q,i) * wm_interm_29_triplet(b, j, k, i)
term(148) = term(148) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,k,p,i) * wm_interm_82_triplet(a, k, j, q)
term(149) = term(149) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,k,p,i) * wm_interm_82_triplet(b, k, j, q)
term(150) = term(150) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,k,p,j) * wm_interm_82_triplet(a, k, q, i)
term(151) = term(151) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,k,p,j) * wm_interm_82_triplet(b, k, q, i)
term(152) = term(152) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,k,p,i) * wm_interm_82_triplet(a, k, q, j)
term(153) = term(153) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,k,p,i) * wm_interm_82_triplet(b, k, q, j)
term(154) = term(154) + r2m(vrdav_Rl, a,k,b,j) * t2(a,p,q,i) * wm_interm_34_triplet(b, k, j, i)
term(155) = term(155) + r2m(vrdav_Rl, a,k,b,j) * t2(a,p,q,i) * wm_interm_34_triplet(b, j, k, i)
end do 
end do 
end do 
end do 
end do 

term(127) = term(127) * (-2.0d+0) 
term(128) = term(128) * (-2.0d+0) 
term(129) = term(129) * 6.0d+0 
term(130) = term(130) * (-6.0d+0) 
term(131) = term(131) * 12.0d+0 
term(132) = term(132) * 12.0d+0 
term(133) = term(133) * (-12.0d+0) 
term(134) = term(134) * (-2.0d+0) 
term(135) = term(135) * 4.0d+0 
term(136) = term(136) * (-2.0d+0) 
term(137) = term(137) * 4.0d+0 
term(138) = term(138) * 4.0d+0 
term(139) = term(139) * (-2.0d+0) 
term(140) = term(140) * 4.0d+0 
term(141) = term(141) * (-2.0d+0) 
term(142) = term(142) * 8.0d+0 
term(143) = term(143) * (-16.0d+0) 
term(144) = term(144) * 6.0d+0 
term(145) = term(145) * 6.0d+0 
term(146) = term(146) * (-6.0d+0) 
term(147) = term(147) * 6.0d+0 
term(148) = term(148) * (-4.0d+0) 
term(149) = term(149) * 8.0d+0 
term(150) = term(150) * (-4.0d+0) 
term(151) = term(151) * 8.0d+0 
term(152) = term(152) * 8.0d+0 
term(153) = term(153) * (-4.0d+0) 
term(154) = term(154) * (-4.0d+0) 
term(155) = term(155) * 8.0d+0 

do j = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(156) = term(156) + r2m(vrdav_Rr, a,i,p,j) * s2(a,b,k,j) * wm_interm_66_triplet(b, i, k, q)
end do 
end do 
end do 
end do 
end do 

term(156) = term(156) * 4.0d+0 

do i = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(157) = term(157) + r2m(vrdav_Rr, a,j,p,i) * s2(a,b,k,i) * wm_interm_66_triplet(b, j, k, q)
end do 
end do 
end do 
end do 
end do 

term(157) = term(157) * 4.0d+0 

do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(158) = term(158) + r2m(vrdav_Rr, a,i,p,j) * s2(a,b,j,k) * wm_interm_66_triplet(b, i, k, q)
term(159) = term(159) + r2m(vrdav_Rr, a,i,p,j) * s2(a,b,i,k) * wm_interm_66_triplet(b, j, k, q)
term(160) = term(160) + r2m(vrdav_Rr, a,i,p,j) * s2(a,b,j,k) * wm_interm_66_triplet(b, i, q, k)
term(161) = term(161) + r2m(vrdav_Rr, a,i,p,j) * s2(a,b,i,k) * wm_interm_66_triplet(b, j, q, k)
term(162) = term(162) + r2m(vrdav_Rr, a,i,p,j) * s2(a,b,k,j) * wm_interm_66_triplet(b, i, q, k)
term(163) = term(163) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,k,j) * wm_interm_35_triplet(a, i, q, k)
term(164) = term(164) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,j,k) * wm_interm_35_triplet(a, i, q, k)
term(165) = term(165) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,k,q) * wm_interm_35_triplet(a, i, j, k)
term(166) = term(166) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_35_triplet(a, i, j, k)
term(167) = term(167) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,k,j) * wm_interm_35_triplet(a, i, q, k)
term(168) = term(168) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,i,k) * wm_interm_35_triplet(a, j, q, k)
term(169) = term(169) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,j,k) * wm_interm_35_triplet(a, i, q, k)
term(170) = term(170) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,k,q) * wm_interm_35_triplet(a, j, i, k)
term(171) = term(171) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,k,q) * wm_interm_35_triplet(a, i, j, k)
term(172) = term(172) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_35_triplet(a, j, i, k)
term(173) = term(173) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_35_triplet(a, i, j, k)
term(174) = term(174) + r2p(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,q,b,k) * wm_interm_82_triplet(a, k, i, j)
term(175) = term(175) + r2p(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,q,p,k) * wm_interm_82_triplet(a, k, i, j)
term(176) = term(176) + r2p(vrdav_Rl, a,i,b,j) * t2(b,p,k,q) * wm_interm_29_triplet(a, i, j, k)
term(177) = term(177) + r2p(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,j,p,k) * wm_interm_82_triplet(a, k, i, q)
term(178) = term(178) + r2p(vrdav_Rl, a,i,b,j) * t2(b,p,k,q) * wm_interm_34_triplet(a, i, j, k)
term(179) = term(179) + r2p(vrdav_Rl, a,i,b,j) * t2(b,p,k,q) * wm_interm_34_triplet(a, j, i, k)
term(180) = term(180) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,k,j) * wm_interm_50_triplet(a, q, i, k)
term(181) = term(181) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,j,k) * wm_interm_50_triplet(a, q, i, k)
term(182) = term(182) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,k,j) * wm_interm_50_triplet(a, i, q, k)
term(183) = term(183) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,k,q) * wm_interm_50_triplet(a, i, j, k)
term(184) = term(184) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_50_triplet(a, i, j, k)
term(185) = term(185) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,j,k) * wm_interm_50_triplet(a, i, q, k)
term(186) = term(186) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,k,j) * wm_interm_49_triplet(a, q, i, k)
term(187) = term(187) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,k,j) * wm_interm_49_triplet(a, i, q, k)
term(188) = term(188) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,k,q) * wm_interm_49_triplet(a, i, j, k)
term(189) = term(189) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_49_triplet(a, i, j, k)
term(190) = term(190) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,j,k) * wm_interm_49_triplet(a, i, q, k)
term(191) = term(191) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,j,k) * wm_interm_49_triplet(a, q, i, k)
term(192) = term(192) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,k,j) * wm_interm_50_triplet(a, q, i, k)
term(193) = term(193) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,i,k) * wm_interm_50_triplet(a, q, j, k)
term(194) = term(194) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,j,k) * wm_interm_50_triplet(a, q, i, k)
term(195) = term(195) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,k,j) * wm_interm_50_triplet(a, i, q, k)
term(196) = term(196) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,k,q) * wm_interm_50_triplet(a, j, i, k)
term(197) = term(197) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,k,q) * wm_interm_50_triplet(a, i, j, k)
term(198) = term(198) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_50_triplet(a, j, i, k)
term(199) = term(199) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_50_triplet(a, i, j, k)
term(200) = term(200) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,i,k) * wm_interm_50_triplet(a, j, q, k)
term(201) = term(201) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,j,k) * wm_interm_50_triplet(a, i, q, k)
term(202) = term(202) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,k,j) * wm_interm_49_triplet(a, q, i, k)
term(203) = term(203) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,k,j) * wm_interm_49_triplet(a, i, q, k)
term(204) = term(204) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,k,q) * wm_interm_49_triplet(a, i, j, k)
term(205) = term(205) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,k,q) * wm_interm_49_triplet(a, j, i, k)
term(206) = term(206) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_49_triplet(a, i, j, k)
term(207) = term(207) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_49_triplet(a, j, i, k)
term(208) = term(208) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,i,k) * wm_interm_49_triplet(a, j, q, k)
term(209) = term(209) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,j,k) * wm_interm_49_triplet(a, i, q, k)
term(210) = term(210) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,i,k) * wm_interm_49_triplet(a, q, j, k)
term(211) = term(211) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,j,k) * wm_interm_49_triplet(a, q, i, k)
term(212) = term(212) + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,q,b,k) * wm_interm_82_triplet(a, k, i, j)
term(213) = term(213) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,q,p,k) * wm_interm_82_triplet(b, k, i, j)
term(214) = term(214) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,q,p,k) * wm_interm_82_triplet(a, k, i, j)
term(215) = term(215) + r2m(vrdav_Rl, a,i,b,j) * t2(a,p,k,q) * wm_interm_29_triplet(b, i, j, k)
term(216) = term(216) + r2m(vrdav_Rl, a,i,b,j) * t2(b,p,k,q) * wm_interm_29_triplet(a, i, j, k)
term(217) = term(217) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,i,p,k) * wm_interm_82_triplet(a, k, j, q)
term(218) = term(218) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,j,p,k) * wm_interm_82_triplet(a, k, i, q)
term(219) = term(219) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,j,p,k) * wm_interm_82_triplet(b, k, i, q)
term(220) = term(220) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,i,p,k) * wm_interm_82_triplet(b, k, j, q)
term(221) = term(221) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,i,p,k) * wm_interm_82_triplet(a, k, q, j)
term(222) = term(222) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,i,p,k) * wm_interm_82_triplet(b, k, q, j)
term(223) = term(223) + r2m(vrdav_Rl, a,i,b,j) * t2(a,p,k,q) * wm_interm_34_triplet(b, i, j, k)
term(224) = term(224) + r2m(vrdav_Rl, a,i,b,j) * t2(a,p,k,q) * wm_interm_34_triplet(b, j, i, k)
term(225) = term(225) + r2m(vrdav_Rl, a,i,b,j) * t2(b,p,k,q) * wm_interm_34_triplet(a, j, i, k)
term(226) = term(226) + r2m(vrdav_Rl, a,i,b,j) * t2(b,p,k,q) * wm_interm_34_triplet(a, i, j, k)
end do 
end do 
end do 
end do 
end do 

term(158) = term(158) * (-2.0d+0) 
term(159) = term(159) * 4.0d+0 
term(160) = term(160) * 4.0d+0 
term(161) = term(161) * (-8.0d+0) 
term(162) = term(162) * (-2.0d+0) 
term(163) = term(163) * 12.0d+0 
term(164) = term(164) * (-18.0d+0) 
term(165) = term(165) * (-12.0d+0) 
term(166) = term(166) * 6.0d+0 
term(167) = term(167) * 12.0d+0 
term(168) = term(168) * 12.0d+0 
term(169) = term(169) * (-24.0d+0) 
term(170) = term(170) * 12.0d+0 
term(171) = term(171) * (-12.0d+0) 
term(172) = term(172) * (-6.0d+0) 
term(173) = term(173) * 6.0d+0 
term(174) = term(174) * (-9.0d+0) 
term(175) = term(175) * 6.0d+0 
term(176) = term(176) * (-12.0d+0) 
term(177) = term(177) * (-24.0d+0) 
term(178) = term(178) * (-12.0d+0) 
term(179) = term(179) * 12.0d+0 
term(180) = term(180) * (-6.0d+0) 
term(181) = term(181) * 6.0d+0 
term(182) = term(182) * 6.0d+0 
term(183) = term(183) * (-12.0d+0) 
term(184) = term(184) * 6.0d+0 
term(185) = term(185) * (-12.0d+0) 
term(186) = term(186) * 6.0d+0 
term(187) = term(187) * (-6.0d+0) 
term(188) = term(188) * 12.0d+0 
term(189) = term(189) * (-6.0d+0) 
term(190) = term(190) * 6.0d+0 
term(191) = term(191) * (-12.0d+0) 
term(192) = term(192) * (-4.0d+0) 
term(193) = term(193) * (-4.0d+0) 
term(194) = term(194) * 8.0d+0 
term(195) = term(195) * 8.0d+0 
term(196) = term(196) * 8.0d+0 
term(197) = term(197) * (-16.0d+0) 
term(198) = term(198) * (-4.0d+0) 
term(199) = term(199) * 8.0d+0 
term(200) = term(200) * 8.0d+0 
term(201) = term(201) * (-16.0d+0) 
term(202) = term(202) * 8.0d+0 
term(203) = term(203) * (-4.0d+0) 
term(204) = term(204) * 8.0d+0 
term(205) = term(205) * (-16.0d+0) 
term(206) = term(206) * (-4.0d+0) 
term(207) = term(207) * 8.0d+0 
term(208) = term(208) * (-4.0d+0) 
term(209) = term(209) * 8.0d+0 
term(210) = term(210) * 8.0d+0 
term(211) = term(211) * (-16.0d+0) 
term(212) = term(212) * (-12.0d+0) 
term(213) = term(213) * (-4.0d+0) 
term(214) = term(214) * 8.0d+0 
term(215) = term(215) * 12.0d+0 
term(216) = term(216) * (-12.0d+0) 
term(217) = term(217) * 8.0d+0 
term(218) = term(218) * (-16.0d+0) 
term(219) = term(219) * 8.0d+0 
term(220) = term(220) * (-16.0d+0) 
term(221) = term(221) * (-4.0d+0) 
term(222) = term(222) * 8.0d+0 
term(223) = term(223) * 8.0d+0 
term(224) = term(224) * (-16.0d+0) 
term(225) = term(225) * 8.0d+0 
term(226) = term(226) * (-16.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(227) = term(227) + r2p(vrdav_Rr, p,q,a,i) * s2(a,b,i,j) * wm_interm_64_triplet(b, j)
term(228) = term(228) + r2p(vrdav_Rr, p,q,a,i) * s2(a,b,i,j) * wm_interm_63_triplet(b, j)
term(229) = term(229) + r2m(vrdav_Rr, a,q,p,i) * s2(a,b,i,j) * wm_interm_64_triplet(b, j)
term(230) = term(230) + r2m(vrdav_Rr, a,q,p,i) * s2(a,b,i,j) * wm_interm_63_triplet(b, j)
term(231) = term(231) + s2(a,b,i,j) * t2(a,p,q,i) * wm_interm_3_triplet(b, j)
term(232) = term(232) + s2(a,b,i,j) * t2(a,p,q,i) * wm_interm_4_triplet(b, j)
term(233) = term(233) + s2(a,b,i,j) * t2(a,p,q,i) * wm_interm_5_triplet(b, j)
term(234) = term(234) + r2p(vrdav_Rl, p,q,a,i) * t2(a,b,i,j) * wm_interm_58_triplet(b, j)
term(235) = term(235) + r2p(vrdav_Rl, p,q,a,i) * t2(a,b,i,j) * wm_interm_57_triplet(b, j)
term(236) = term(236) + s2(a,p,q,i) * t2(a,b,i,j) * wm_interm_7_triplet(b, j)
term(237) = term(237) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,q,i) * wm_interm_6_triplet(a, j)
term(238) = term(238) + s2(a,p,q,i) * t2(a,b,i,j) * wm_interm_11_triplet(b, j)
term(239) = term(239) + r2m(vrdav_Rl, a,q,p,i) * t2(a,b,i,j) * wm_interm_58_triplet(b, j)
term(240) = term(240) + s2(a,p,q,i) * t2(a,b,i,j) * wm_interm_10_triplet(b, j)
term(241) = term(241) + r2m(vrdav_Rl, a,q,p,i) * t2(a,b,i,j) * wm_interm_57_triplet(b, j)
term(242) = term(242) + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,q,a,i) * wm_interm_61_triplet(b, j)
term(243) = term(243) + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,q,a,i) * wm_interm_65_triplet(b, j)
term(244) = term(244) + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,q,b,i) * wm_interm_61_triplet(a, j)
term(245) = term(245) + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,q,b,i) * wm_interm_65_triplet(a, j)
term(246) = term(246) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,q,p,i) * wm_interm_61_triplet(b, j)
term(247) = term(247) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,q,p,i) * wm_interm_65_triplet(b, j)
term(248) = term(248) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,q,p,i) * wm_interm_61_triplet(a, j)
term(249) = term(249) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,q,p,i) * wm_interm_65_triplet(a, j)
term(250) = term(250) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,q,i) * wm_interm_8_triplet(a, j)
term(251) = term(251) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,q,i) * wm_interm_9_triplet(a, j)
term(252) = term(252) + r2m(vrdav_Rl, a,i,b,j) * t2(a,p,q,i) * wm_interm_0_triplet(b, j)
term(253) = term(253) + r2m(vrdav_Rl, a,i,b,j) * t2(a,p,q,i) * wm_interm_1_triplet(b, j)
term(254) = term(254) + r2m(vrdav_Rl, a,i,b,j) * t2(a,p,q,i) * wm_interm_2_triplet(b, j)
end do 
end do 
end do 
end do 

term(227) = term(227) * (-12.0d+0) 
term(228) = term(228) * 24.0d+0 
term(229) = term(229) * 8.0d+0 
term(230) = term(230) * (-16.0d+0) 
term(231) = term(231) * (-12.0d+0) 
term(232) = term(232) * 8.0d+0 
term(233) = term(233) * (-16.0d+0) 
term(234) = term(234) * 24.0d+0 
term(235) = term(235) * (-12.0d+0) 
term(236) = term(236) * (-12.0d+0) 
term(237) = term(237) * 12.0d+0 
term(238) = term(238) * (-16.0d+0) 
term(239) = term(239) * (-16.0d+0) 
term(240) = term(240) * 8.0d+0 
term(241) = term(241) * 8.0d+0 
term(242) = term(242) * (-12.0d+0) 
term(243) = term(243) * 24.0d+0 
term(244) = term(244) * 6.0d+0 
term(245) = term(245) * (-12.0d+0) 
term(246) = term(246) * 8.0d+0 
term(247) = term(247) * (-16.0d+0) 
term(248) = term(248) * (-4.0d+0) 
term(249) = term(249) * 8.0d+0 
term(250) = term(250) * 16.0d+0 
term(251) = term(251) * (-8.0d+0) 
term(252) = term(252) * 24.0d+0 
term(253) = term(253) * 32.0d+0 
term(254) = term(254) * (-16.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(255) = term(255) + r2m(vrdav_Rr, a,j,p,i) * s2(a,b,i,k) * wm_interm_66_triplet(b, j, k, q)
term(256) = term(256) + r2m(vrdav_Rr, a,j,p,i) * s2(a,b,j,k) * wm_interm_66_triplet(b, i, k, q)
term(257) = term(257) + r2m(vrdav_Rr, a,j,p,i) * s2(a,b,i,k) * wm_interm_66_triplet(b, j, q, k)
term(258) = term(258) + r2m(vrdav_Rr, a,j,p,i) * s2(a,b,j,k) * wm_interm_66_triplet(b, i, q, k)
term(259) = term(259) + r2m(vrdav_Rr, a,j,p,i) * s2(a,b,k,i) * wm_interm_66_triplet(b, j, q, k)
term(260) = term(260) + s2(a,p,j,i) * t2(a,b,j,k) * wm_interm_80_triplet(b, k, q, i)
term(261) = term(261) + r2m(vrdav_Rl, a,i,p,q) * r2m(vrdav_Rr, a,j,b,k) * wm_interm_56_triplet(b, j, k, i)
term(262) = term(262) + r2m(vrdav_Rl, a,i,p,q) * r2m(vrdav_Rr, a,j,b,k) * wm_interm_56_triplet(b, k, j, i)
term(263) = term(263) + r2m(vrdav_Rl, a,q,p,i) * r2m(vrdav_Rr, a,j,b,k) * wm_interm_56_triplet(b, j, k, i)
term(264) = term(264) + r2m(vrdav_Rl, a,q,p,i) * r2m(vrdav_Rr, a,j,b,k) * wm_interm_56_triplet(b, k, j, i)
term(265) = term(265) + s2(a,p,j,i) * t2(a,b,j,k) * wm_interm_90_triplet(b, q, k, i)
term(266) = term(266) + s2(a,p,j,i) * t2(a,b,j,k) * wm_interm_90_triplet(b, k, q, i)
term(267) = term(267) + s2(a,p,j,i) * t2(a,b,j,k) * wm_interm_89_triplet(b, q, k, i)
term(268) = term(268) + s2(a,p,j,i) * t2(a,b,j,k) * wm_interm_89_triplet(b, k, q, i)
term(269) = term(269) + r2m(vrdav_Rl, a,j,b,k) * t2(a,p,q,i) * wm_interm_29_triplet(b, j, k, i)
term(270) = term(270) + r2m(vrdav_Rl, a,j,b,k) * t2(a,p,q,i) * wm_interm_34_triplet(b, j, k, i)
term(271) = term(271) + r2m(vrdav_Rl, a,j,b,k) * t2(a,p,q,i) * wm_interm_34_triplet(b, k, j, i)
end do 
end do 
end do 
end do 
end do 

term(255) = term(255) * (-2.0d+0) 
term(256) = term(256) * 4.0d+0 
term(257) = term(257) * 4.0d+0 
term(258) = term(258) * (-8.0d+0) 
term(259) = term(259) * (-2.0d+0) 
term(260) = term(260) * 12.0d+0 
term(261) = term(261) * 16.0d+0 
term(262) = term(262) * (-32.0d+0) 
term(263) = term(263) * (-8.0d+0) 
term(264) = term(264) * 16.0d+0 
term(265) = term(265) * (-8.0d+0) 
term(266) = term(266) * 4.0d+0 
term(267) = term(267) * 4.0d+0 
term(268) = term(268) * (-8.0d+0) 
term(269) = term(269) * (-6.0d+0) 
term(270) = term(270) * (-4.0d+0) 
term(271) = term(271) * 8.0d+0 

do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(272) = term(272) + r2m(vrdav_Rr, a,q,p,i) * s2(a,b,j,i) * wm_interm_64_triplet(b, j)
term(273) = term(273) + r2m(vrdav_Rr, a,i,p,q) * s2(a,b,j,i) * wm_interm_64_triplet(b, j)
term(274) = term(274) + r2m(vrdav_Rr, a,q,p,i) * s2(a,b,j,i) * wm_interm_63_triplet(b, j)
term(275) = term(275) + r2m(vrdav_Rr, a,i,p,q) * s2(a,b,j,i) * wm_interm_63_triplet(b, j)
term(276) = term(276) + s2(a,b,j,i) * t2(a,p,q,i) * wm_interm_3_triplet(b, j)
term(277) = term(277) + s2(a,b,j,i) * t2(a,p,q,i) * wm_interm_4_triplet(b, j)
term(278) = term(278) + s2(a,b,j,i) * t2(a,p,q,i) * wm_interm_5_triplet(b, j)
term(279) = term(279) + s2(a,p,q,i) * t2(a,b,j,i) * wm_interm_7_triplet(b, j)
term(280) = term(280) + s2(a,p,i,q) * t2(a,b,j,i) * wm_interm_7_triplet(b, j)
term(281) = term(281) + r2m(vrdav_Rl, a,i,p,q) * t2(a,b,j,i) * wm_interm_57_triplet(b, j)
term(282) = term(282) + r2m(vrdav_Rl, a,i,p,q) * t2(a,b,j,i) * wm_interm_58_triplet(b, j)
term(283) = term(283) + s2(a,p,q,i) * t2(a,b,j,i) * wm_interm_11_triplet(b, j)
term(284) = term(284) + s2(a,p,i,q) * t2(a,b,j,i) * wm_interm_11_triplet(b, j)
term(285) = term(285) + s2(a,p,q,i) * t2(a,b,j,i) * wm_interm_10_triplet(b, j)
term(286) = term(286) + r2m(vrdav_Rl, a,q,p,i) * t2(a,b,j,i) * wm_interm_57_triplet(b, j)
term(287) = term(287) + s2(a,p,i,q) * t2(a,b,j,i) * wm_interm_10_triplet(b, j)
term(288) = term(288) + r2m(vrdav_Rl, a,q,p,i) * t2(a,b,j,i) * wm_interm_58_triplet(b, j)
term(289) = term(289) + r2m(vrdav_Rl, a,j,b,i) * t2(a,p,q,i) * wm_interm_0_triplet(b, j)
term(290) = term(290) + r2m(vrdav_Rl, a,j,b,i) * t2(a,p,q,i) * wm_interm_1_triplet(b, j)
term(291) = term(291) + r2m(vrdav_Rl, a,j,b,i) * t2(a,p,q,i) * wm_interm_2_triplet(b, j)
end do 
end do 
end do 
end do 

term(272) = term(272) * (-4.0d+0) 
term(273) = term(273) * 8.0d+0 
term(274) = term(274) * 8.0d+0 
term(275) = term(275) * (-16.0d+0) 
term(276) = term(276) * 6.0d+0 
term(277) = term(277) * (-4.0d+0) 
term(278) = term(278) * 8.0d+0 
term(279) = term(279) * 6.0d+0 
term(280) = term(280) * (-12.0d+0) 
term(281) = term(281) * 8.0d+0 
term(282) = term(282) * (-16.0d+0) 
term(283) = term(283) * 8.0d+0 
term(284) = term(284) * (-16.0d+0) 
term(285) = term(285) * (-4.0d+0) 
term(286) = term(286) * (-4.0d+0) 
term(287) = term(287) * 8.0d+0 
term(288) = term(288) * 8.0d+0 
term(289) = term(289) * (-12.0d+0) 
term(290) = term(290) * (-16.0d+0) 
term(291) = term(291) * 8.0d+0 

do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(292) = term(292) + r2m(vrdav_Rr, a,i,p,q) * s2(a,b,i,j) * wm_interm_64_triplet(b, j)
term(293) = term(293) + r2m(vrdav_Rr, a,i,p,q) * s2(a,b,i,j) * wm_interm_63_triplet(b, j)
term(294) = term(294) + s2(a,b,i,j) * t2(b,p,j,q) * wm_interm_3_triplet(a, i)
term(295) = term(295) + s2(a,b,i,j) * t2(b,p,i,q) * wm_interm_3_triplet(a, j)
term(296) = term(296) + s2(a,b,i,j) * t2(b,p,j,q) * wm_interm_4_triplet(a, i)
term(297) = term(297) + s2(a,b,i,j) * t2(b,p,j,q) * wm_interm_5_triplet(a, i)
term(298) = term(298) + s2(a,b,i,j) * t2(b,p,i,q) * wm_interm_4_triplet(a, j)
term(299) = term(299) + s2(a,b,i,j) * t2(b,p,i,q) * wm_interm_5_triplet(a, j)
term(300) = term(300) + s2(a,p,i,q) * t2(a,b,i,j) * wm_interm_7_triplet(b, j)
term(301) = term(301) + r2p(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,q,b,j) * wm_interm_61_triplet(a, i)
term(302) = term(302) + r2p(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,q,b,j) * wm_interm_65_triplet(a, i)
term(303) = term(303) + r2p(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,q,p,j) * wm_interm_61_triplet(a, i)
term(304) = term(304) + r2p(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,j,p,q) * wm_interm_61_triplet(a, i)
term(305) = term(305) + r2p(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,q,p,j) * wm_interm_65_triplet(a, i)
term(306) = term(306) + r2p(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,j,p,q) * wm_interm_65_triplet(a, i)
term(307) = term(307) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,j,q) * wm_interm_6_triplet(a, i)
term(308) = term(308) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,q,j) * wm_interm_6_triplet(a, i)
term(309) = term(309) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,i,q) * wm_interm_6_triplet(a, j)
term(310) = term(310) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,j,q) * wm_interm_6_triplet(a, i)
term(311) = term(311) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,q,j) * wm_interm_6_triplet(a, i)
term(312) = term(312) + r2p(vrdav_Rl, a,i,b,j) * t2(b,p,j,q) * wm_interm_0_triplet(a, i)
term(313) = term(313) + r2p(vrdav_Rl, a,i,b,j) * t2(b,p,j,q) * wm_interm_1_triplet(a, i)
term(314) = term(314) + r2p(vrdav_Rl, a,i,b,j) * t2(b,p,j,q) * wm_interm_2_triplet(a, i)
term(315) = term(315) + r2m(vrdav_Rl, a,i,p,q) * t2(a,b,i,j) * wm_interm_58_triplet(b, j)
term(316) = term(316) + s2(a,p,i,q) * t2(a,b,i,j) * wm_interm_11_triplet(b, j)
term(317) = term(317) + r2m(vrdav_Rl, a,i,p,q) * t2(a,b,i,j) * wm_interm_57_triplet(b, j)
term(318) = term(318) + s2(a,p,i,q) * t2(a,b,i,j) * wm_interm_10_triplet(b, j)
term(319) = term(319) + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,q,b,j) * wm_interm_61_triplet(a, i)
term(320) = term(320) + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,q,b,j) * wm_interm_65_triplet(a, i)
term(321) = term(321) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,i,p,q) * wm_interm_61_triplet(b, j)
term(322) = term(322) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,i,p,q) * wm_interm_65_triplet(b, j)
term(323) = term(323) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,q,p,j) * wm_interm_61_triplet(a, i)
term(324) = term(324) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,j,p,q) * wm_interm_61_triplet(a, i)
term(325) = term(325) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,q,p,j) * wm_interm_61_triplet(b, i)
term(326) = term(326) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,j,p,q) * wm_interm_61_triplet(b, i)
term(327) = term(327) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,q,p,j) * wm_interm_65_triplet(b, i)
term(328) = term(328) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,j,p,q) * wm_interm_65_triplet(b, i)
term(329) = term(329) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,i,p,q) * wm_interm_61_triplet(a, j)
term(330) = term(330) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,i,p,q) * wm_interm_65_triplet(a, j)
term(331) = term(331) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,q,p,j) * wm_interm_65_triplet(a, i)
term(332) = term(332) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,j,p,q) * wm_interm_65_triplet(a, i)
term(333) = term(333) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,j,q) * wm_interm_8_triplet(a, i)
term(334) = term(334) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,j,q) * wm_interm_9_triplet(a, i)
term(335) = term(335) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,q,j) * wm_interm_8_triplet(a, i)
term(336) = term(336) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,q,j) * wm_interm_9_triplet(a, i)
term(337) = term(337) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,i,q) * wm_interm_8_triplet(a, j)
term(338) = term(338) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,j,q) * wm_interm_8_triplet(a, i)
term(339) = term(339) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,i,q) * wm_interm_9_triplet(a, j)
term(340) = term(340) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,j,q) * wm_interm_9_triplet(a, i)
term(341) = term(341) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,q,j) * wm_interm_8_triplet(a, i)
term(342) = term(342) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,q,j) * wm_interm_9_triplet(a, i)
term(343) = term(343) + r2m(vrdav_Rl, a,i,b,j) * t2(a,p,j,q) * wm_interm_0_triplet(b, i)
term(344) = term(344) + r2m(vrdav_Rl, a,i,b,j) * t2(a,p,i,q) * wm_interm_0_triplet(b, j)
term(345) = term(345) + r2m(vrdav_Rl, a,i,b,j) * t2(b,p,i,q) * wm_interm_0_triplet(a, j)
term(346) = term(346) + r2m(vrdav_Rl, a,i,b,j) * t2(b,p,j,q) * wm_interm_0_triplet(a, i)
term(347) = term(347) + r2m(vrdav_Rl, a,i,b,j) * t2(a,p,j,q) * wm_interm_1_triplet(b, i)
term(348) = term(348) + r2m(vrdav_Rl, a,i,b,j) * t2(a,p,i,q) * wm_interm_1_triplet(b, j)
term(349) = term(349) + r2m(vrdav_Rl, a,i,b,j) * t2(b,p,i,q) * wm_interm_1_triplet(a, j)
term(350) = term(350) + r2m(vrdav_Rl, a,i,b,j) * t2(b,p,j,q) * wm_interm_1_triplet(a, i)
term(351) = term(351) + r2m(vrdav_Rl, a,i,b,j) * t2(a,p,j,q) * wm_interm_2_triplet(b, i)
term(352) = term(352) + r2m(vrdav_Rl, a,i,b,j) * t2(a,p,i,q) * wm_interm_2_triplet(b, j)
term(353) = term(353) + r2m(vrdav_Rl, a,i,b,j) * t2(b,p,i,q) * wm_interm_2_triplet(a, j)
term(354) = term(354) + r2m(vrdav_Rl, a,i,b,j) * t2(b,p,j,q) * wm_interm_2_triplet(a, i)
end do 
end do 
end do 
end do 

term(292) = term(292) * (-16.0d+0) 
term(293) = term(293) * 32.0d+0 
term(294) = term(294) * 24.0d+0 
term(295) = term(295) * (-12.0d+0) 
term(296) = term(296) * (-16.0d+0) 
term(297) = term(297) * 32.0d+0 
term(298) = term(298) * 8.0d+0 
term(299) = term(299) * (-16.0d+0) 
term(300) = term(300) * 24.0d+0 
term(301) = term(301) * (-18.0d+0) 
term(302) = term(302) * 36.0d+0 
term(303) = term(303) * 12.0d+0 
term(304) = term(304) * (-24.0d+0) 
term(305) = term(305) * (-24.0d+0) 
term(306) = term(306) * 48.0d+0 
term(307) = term(307) * 36.0d+0 
term(308) = term(308) * (-18.0d+0) 
term(309) = term(309) * (-24.0d+0) 
term(310) = term(310) * 48.0d+0 
term(311) = term(311) * (-24.0d+0) 
term(312) = term(312) * (-36.0d+0) 
term(313) = term(313) * (-48.0d+0) 
term(314) = term(314) * 24.0d+0 
term(315) = term(315) * 32.0d+0 
term(316) = term(316) * 32.0d+0 
term(317) = term(317) * (-16.0d+0) 
term(318) = term(318) * (-16.0d+0) 
term(319) = term(319) * (-12.0d+0) 
term(320) = term(320) * 24.0d+0 
term(321) = term(321) * (-16.0d+0) 
term(322) = term(322) * 32.0d+0 
term(323) = term(323) * 8.0d+0 
term(324) = term(324) * (-16.0d+0) 
term(325) = term(325) * (-4.0d+0) 
term(326) = term(326) * 8.0d+0 
term(327) = term(327) * 8.0d+0 
term(328) = term(328) * (-16.0d+0) 
term(329) = term(329) * 8.0d+0 
term(330) = term(330) * (-16.0d+0) 
term(331) = term(331) * (-16.0d+0) 
term(332) = term(332) * 32.0d+0 
term(333) = term(333) * 48.0d+0 
term(334) = term(334) * (-24.0d+0) 
term(335) = term(335) * (-24.0d+0) 
term(336) = term(336) * 12.0d+0 
term(337) = term(337) * (-32.0d+0) 
term(338) = term(338) * 64.0d+0 
term(339) = term(339) * 16.0d+0 
term(340) = term(340) * (-32.0d+0) 
term(341) = term(341) * (-32.0d+0) 
term(342) = term(342) * 16.0d+0 
term(343) = term(343) * 12.0d+0 
term(344) = term(344) * (-24.0d+0) 
term(345) = term(345) * 12.0d+0 
term(346) = term(346) * (-24.0d+0) 
term(347) = term(347) * 16.0d+0 
term(348) = term(348) * (-32.0d+0) 
term(349) = term(349) * 16.0d+0 
term(350) = term(350) * (-32.0d+0) 
term(351) = term(351) * (-8.0d+0) 
term(352) = term(352) * 16.0d+0 
term(353) = term(353) * (-8.0d+0) 
term(354) = term(354) * 16.0d+0 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(355) = term(355) + r2m(vrdav_Rr, a,i,p,j) * s2(a,b,k,i) * wm_interm_66_triplet(b, j, q, k)
term(356) = term(356) + r2m(vrdav_Rr, a,j,p,i) * s2(a,b,k,j) * wm_interm_66_triplet(b, i, q, k)
term(357) = term(357) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,k,i) * wm_interm_35_triplet(a, j, q, k)
term(358) = term(358) + s2(a,p,j,i) * t2(a,b,i,k) * wm_interm_80_triplet(b, k, q, j)
term(359) = term(359) + r2p(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,q,b,k) * wm_interm_82_triplet(a, k, j, i)
term(360) = term(360) + r2p(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,q,p,k) * wm_interm_82_triplet(a, k, j, i)
term(361) = term(361) + r2p(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,j,p,k) * wm_interm_82_triplet(a, k, q, i)
term(362) = term(362) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,k,i) * wm_interm_50_triplet(a, q, j, k)
term(363) = term(363) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,k,i) * wm_interm_50_triplet(a, j, q, k)
term(364) = term(364) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,k,i) * wm_interm_49_triplet(a, q, j, k)
term(365) = term(365) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,k,i) * wm_interm_49_triplet(a, j, q, k)
term(366) = term(366) + s2(a,p,j,i) * t2(a,b,i,k) * wm_interm_90_triplet(b, q, k, j)
term(367) = term(367) + s2(a,p,j,i) * t2(a,b,i,k) * wm_interm_90_triplet(b, k, q, j)
term(368) = term(368) + s2(a,p,j,i) * t2(a,b,i,k) * wm_interm_89_triplet(b, q, k, j)
term(369) = term(369) + s2(a,p,j,i) * t2(a,b,i,k) * wm_interm_89_triplet(b, k, q, j)
term(370) = term(370) + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,q,b,k) * wm_interm_82_triplet(a, k, j, i)
term(371) = term(371) + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,q,a,k) * wm_interm_82_triplet(b, k, j, i)
term(372) = term(372) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,q,p,k) * wm_interm_82_triplet(a, k, j, i)
term(373) = term(373) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,q,p,k) * wm_interm_82_triplet(b, k, j, i)
term(374) = term(374) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,j,p,k) * wm_interm_82_triplet(a, k, q, i)
term(375) = term(375) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,j,p,k) * wm_interm_82_triplet(b, k, q, i)
end do 
end do 
end do 
end do 
end do 

term(355) = term(355) * 4.0d+0 
term(356) = term(356) * 4.0d+0 
term(357) = term(357) * (-12.0d+0) 
term(358) = term(358) * (-6.0d+0) 
term(359) = term(359) * 9.0d+0 
term(360) = term(360) * (-6.0d+0) 
term(361) = term(361) * 12.0d+0 
term(362) = term(362) * 8.0d+0 
term(363) = term(363) * (-4.0d+0) 
term(364) = term(364) * (-4.0d+0) 
term(365) = term(365) * 8.0d+0 
term(366) = term(366) * 4.0d+0 
term(367) = term(367) * (-2.0d+0) 
term(368) = term(368) * (-2.0d+0) 
term(369) = term(369) * 4.0d+0 
term(370) = term(370) * 6.0d+0 
term(371) = term(371) * (-12.0d+0) 
term(372) = term(372) * (-4.0d+0) 
term(373) = term(373) * 8.0d+0 
term(374) = term(374) * 8.0d+0 
term(375) = term(375) * (-4.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
do j = 1, nocc 
do b = nocc + 1, nactive 
term(376) = term(376) + r2p(vrdav_Rl, p,q,a,i) * r2p(vrdav_Rr, b,j,a,k) * wm_interm_56_triplet(b, k, j, i)
term(377) = term(377) + r2p(vrdav_Rl, p,q,a,i) * r2p(vrdav_Rr, b,j,a,k) * wm_interm_56_triplet(b, j, k, i)
term(378) = term(378) + r2p(vrdav_Rl, b,j,a,k) * t2(a,p,q,i) * wm_interm_29_triplet(b, j, k, i)
term(379) = term(379) + r2p(vrdav_Rl, b,j,a,k) * t2(a,p,q,i) * wm_interm_34_triplet(b, j, k, i)
term(380) = term(380) + r2p(vrdav_Rl, b,j,a,k) * t2(a,p,q,i) * wm_interm_34_triplet(b, k, j, i)
term(381) = term(381) + r2m(vrdav_Rl, a,i,p,q) * r2p(vrdav_Rr, b,j,a,k) * wm_interm_56_triplet(b, k, j, i)
term(382) = term(382) + r2m(vrdav_Rl, a,q,p,i) * r2p(vrdav_Rr, b,j,a,k) * wm_interm_56_triplet(b, k, j, i)
term(383) = term(383) + r2m(vrdav_Rl, a,i,p,q) * r2p(vrdav_Rr, b,j,a,k) * wm_interm_56_triplet(b, j, k, i)
term(384) = term(384) + r2m(vrdav_Rl, a,q,p,i) * r2p(vrdav_Rr, b,j,a,k) * wm_interm_56_triplet(b, j, k, i)
end do 
end do 
end do 
end do 
end do 

term(376) = term(376) * 9.0d+0 
term(377) = term(377) * (-9.0d+0) 
term(378) = term(378) * 6.0d+0 
term(379) = term(379) * 6.0d+0 
term(380) = term(380) * (-6.0d+0) 
term(381) = term(381) * 12.0d+0 
term(382) = term(382) * (-6.0d+0) 
term(383) = term(383) * (-12.0d+0) 
term(384) = term(384) * 6.0d+0 

do k = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do j = 1, nocc 
term(385) = term(385) + r2p(vrdav_Rl, p,q,a,i) * r2m(vrdav_Rr, a,j,b,k) * wm_interm_56_triplet(b, j, k, i)
term(386) = term(386) + r2p(vrdav_Rl, p,q,a,i) * r2m(vrdav_Rr, a,j,b,k) * wm_interm_56_triplet(b, k, j, i)
end do 
end do 
end do 
end do 
end do 

term(385) = term(385) * 12.0d+0 
term(386) = term(386) * (-24.0d+0) 

do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do i = 1, nocc 
do k = 1, nocc 
term(387) = term(387) + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,k,a,j) * wm_interm_82_triplet(b, k, i, q)
end do 
end do 
end do 
end do 
end do 

term(387) = term(387) * 6.0d+0 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(388) = term(388) + s2(a,p,i,q) * wm_interm_3_triplet(a, i)
term(389) = term(389) + s2(a,p,q,i) * wm_interm_3_triplet(a, i)
term(390) = term(390) + s2(a,p,i,q) * wm_interm_4_triplet(a, i)
term(391) = term(391) + s2(a,p,i,q) * wm_interm_5_triplet(a, i)
term(392) = term(392) + s2(a,p,q,i) * wm_interm_4_triplet(a, i)
term(393) = term(393) + s2(a,p,q,i) * wm_interm_5_triplet(a, i)
term(394) = term(394) + r2p(vrdav_Rl, p,q,a,i) * wm_interm_0_triplet(a, i)
term(395) = term(395) + r2p(vrdav_Rl, p,q,a,i) * wm_interm_1_triplet(a, i)
term(396) = term(396) + r2p(vrdav_Rl, p,q,a,i) * wm_interm_2_triplet(a, i)
term(397) = term(397) + r2p(vrdav_Rr, p,q,a,i) * wm_interm_6_triplet(a, i)
term(398) = term(398) + r2m(vrdav_Rr, a,q,p,i) * wm_interm_6_triplet(a, i)
term(399) = term(399) + r2m(vrdav_Rr, a,i,p,q) * wm_interm_6_triplet(a, i)
term(400) = term(400) + t2(a,p,q,i) * wm_interm_7_triplet(a, i)
term(401) = term(401) + t2(a,p,i,q) * wm_interm_7_triplet(a, i)
term(402) = term(402) + r2m(vrdav_Rl, a,q,p,i) * wm_interm_0_triplet(a, i)
term(403) = term(403) + r2m(vrdav_Rl, a,i,p,q) * wm_interm_0_triplet(a, i)
term(404) = term(404) + r2m(vrdav_Rl, a,q,p,i) * wm_interm_1_triplet(a, i)
term(405) = term(405) + r2m(vrdav_Rl, a,i,p,q) * wm_interm_1_triplet(a, i)
term(406) = term(406) + r2m(vrdav_Rl, a,q,p,i) * wm_interm_2_triplet(a, i)
term(407) = term(407) + r2m(vrdav_Rl, a,i,p,q) * wm_interm_2_triplet(a, i)
term(408) = term(408) + r2p(vrdav_Rr, p,q,a,i) * wm_interm_8_triplet(a, i)
term(409) = term(409) + r2p(vrdav_Rr, p,q,a,i) * wm_interm_9_triplet(a, i)
term(410) = term(410) + r2m(vrdav_Rr, a,q,p,i) * wm_interm_8_triplet(a, i)
term(411) = term(411) + r2m(vrdav_Rr, a,i,p,q) * wm_interm_8_triplet(a, i)
term(412) = term(412) + r2m(vrdav_Rr, a,q,p,i) * wm_interm_9_triplet(a, i)
term(413) = term(413) + r2m(vrdav_Rr, a,i,p,q) * wm_interm_9_triplet(a, i)
term(414) = term(414) + t2(a,p,q,i) * wm_interm_10_triplet(a, i)
term(415) = term(415) + t2(a,p,q,i) * wm_interm_11_triplet(a, i)
term(416) = term(416) + t2(a,p,i,q) * wm_interm_10_triplet(a, i)
term(417) = term(417) + t2(a,p,i,q) * wm_interm_11_triplet(a, i)
term(418) = term(418) + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, p,q,a,i)
term(419) = term(419) + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, a,q,p,i)
term(420) = term(420) + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, a,i,p,q)
term(421) = term(421) + r2p(vrdav_Rl, p,q,a,i) * r1(vrdav_Rr, a,i)
term(422) = term(422) + r2m(vrdav_Rl, a,q,p,i) * r1(vrdav_Rr, a,i)
term(423) = term(423) + r2m(vrdav_Rl, a,i,p,q) * r1(vrdav_Rr, a,i)
end do 
end do 

term(388) = term(388) * (-12.0d+0) 
term(389) = term(389) * 6.0d+0 
term(390) = term(390) * 8.0d+0 
term(391) = term(391) * (-16.0d+0) 
term(392) = term(392) * (-4.0d+0) 
term(393) = term(393) * 8.0d+0 
term(394) = term(394) * 18.0d+0 
term(395) = term(395) * 24.0d+0 
term(396) = term(396) * (-12.0d+0) 
term(397) = term(397) * (-18.0d+0) 
term(398) = term(398) * 12.0d+0 
term(399) = term(399) * (-24.0d+0) 
term(400) = term(400) * 6.0d+0 
term(401) = term(401) * (-12.0d+0) 
term(402) = term(402) * (-12.0d+0) 
term(403) = term(403) * 24.0d+0 
term(404) = term(404) * (-16.0d+0) 
term(405) = term(405) * 32.0d+0 
term(406) = term(406) * 8.0d+0 
term(407) = term(407) * (-16.0d+0) 
term(408) = term(408) * (-24.0d+0) 
term(409) = term(409) * 12.0d+0 
term(410) = term(410) * 16.0d+0 
term(411) = term(411) * (-32.0d+0) 
term(412) = term(412) * (-8.0d+0) 
term(413) = term(413) * 16.0d+0 
term(414) = term(414) * (-4.0d+0) 
term(415) = term(415) * 8.0d+0 
term(416) = term(416) * 8.0d+0 
term(417) = term(417) * (-16.0d+0) 
term(418) = term(418) * 6.0d+0 
term(419) = term(419) * (-4.0d+0) 
term(420) = term(420) * 8.0d+0 
term(421) = term(421) * 6.0d+0 
term(422) = term(422) * (-4.0d+0) 
term(423) = term(423) * 8.0d+0 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do k = 1, nocc 
term(424) = term(424) + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,k,a,i) * wm_interm_82_triplet(b, k, j, q)
term(425) = term(425) + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,k,a,j) * wm_interm_82_triplet(b, k, q, i)
term(426) = term(426) + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,k,a,i) * wm_interm_82_triplet(b, k, q, j)
end do 
end do 
end do 
end do 
end do 

term(424) = term(424) * (-12.0d+0) 
term(425) = term(425) * (-6.0d+0) 
term(426) = term(426) * 6.0d+0 

term(427) = term(427) + s1(p,q) * wm_interm_13_triplet
term(428) = term(428) + t1(p,q) * wm_interm_13_triplet
term(429) = term(429) + s1(p,q) * wm_interm_28_triplet
term(430) = term(430) + s1(p,q) * wm_interm_32_triplet
term(431) = term(431) + s1(p,q) * wm_interm_33_triplet
term(432) = term(432) + t1(p,q) * wm_interm_28_triplet
term(433) = term(433) + t1(p,q) * wm_interm_32_triplet
term(434) = term(434) + t1(p,q) * wm_interm_33_triplet
term(435) = term(435) + s1(p,q) * wm_interm_42_triplet
term(436) = term(436) + s1(p,q) * wm_interm_47_triplet
term(437) = term(437) + s1(p,q) * wm_interm_48_triplet
term(438) = term(438) + t1(p,q) * wm_interm_42_triplet
term(439) = term(439) + t1(p,q) * wm_interm_47_triplet
term(440) = term(440) + t1(p,q) * wm_interm_48_triplet
term(441) = term(441) + wm_interm_13_triplet * wm_interm_61_triplet(p, q)
term(442) = term(442) + wm_interm_13_triplet * wm_interm_65_triplet(p, q)
term(443) = term(443) + wm_interm_28_triplet * wm_interm_61_triplet(p, q)
term(444) = term(444) + wm_interm_28_triplet * wm_interm_65_triplet(p, q)
term(445) = term(445) + wm_interm_32_triplet * wm_interm_61_triplet(p, q)
term(446) = term(446) + wm_interm_33_triplet * wm_interm_61_triplet(p, q)
term(447) = term(447) + wm_interm_32_triplet * wm_interm_65_triplet(p, q)
term(448) = term(448) + wm_interm_33_triplet * wm_interm_65_triplet(p, q)
term(449) = term(449) + wm_interm_42_triplet * wm_interm_61_triplet(p, q)
term(450) = term(450) + wm_interm_42_triplet * wm_interm_65_triplet(p, q)
term(451) = term(451) + wm_interm_47_triplet * wm_interm_61_triplet(p, q)
term(452) = term(452) + wm_interm_48_triplet * wm_interm_61_triplet(p, q)
term(453) = term(453) + wm_interm_47_triplet * wm_interm_65_triplet(p, q)
term(454) = term(454) + wm_interm_48_triplet * wm_interm_65_triplet(p, q)

term(427) = term(427) * (-4.0d+0) 
term(428) = term(428) * (-4.0d+0) 
term(429) = term(429) * (-6.0d+0) 
term(430) = term(430) * 4.0d+0 
term(431) = term(431) * (-8.0d+0) 
term(432) = term(432) * (-6.0d+0) 
term(433) = term(433) * 4.0d+0 
term(434) = term(434) * (-8.0d+0) 
term(435) = term(435) * (-12.0d+0) 
term(436) = term(436) * 8.0d+0 
term(437) = term(437) * (-16.0d+0) 
term(438) = term(438) * (-12.0d+0) 
term(439) = term(439) * 8.0d+0 
term(440) = term(440) * (-16.0d+0) 
term(441) = term(441) * (-4.0d+0) 
term(442) = term(442) * 8.0d+0 
term(443) = term(443) * (-6.0d+0) 
term(444) = term(444) * 12.0d+0 
term(445) = term(445) * 4.0d+0 
term(446) = term(446) * (-8.0d+0) 
term(447) = term(447) * (-8.0d+0) 
term(448) = term(448) * 16.0d+0 
term(449) = term(449) * (-12.0d+0) 
term(450) = term(450) * 24.0d+0 
term(451) = term(451) * 8.0d+0 
term(452) = term(452) * (-16.0d+0) 
term(453) = term(453) * (-16.0d+0) 
term(454) = term(454) * 32.0d+0 

do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(455) = term(455) + r2p(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,k,p,q) * wm_interm_82_triplet(a, k, i, j)
term(456) = term(456) + r2p(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,k,b,j) * wm_interm_82_triplet(a, k, i, q)
term(457) = term(457) + r2p(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,k,p,j) * wm_interm_82_triplet(a, k, i, q)
term(458) = term(458) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,k,p,q) * wm_interm_82_triplet(b, k, i, j)
term(459) = term(459) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,k,p,q) * wm_interm_82_triplet(a, k, i, j)
term(460) = term(460) + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,k,b,j) * wm_interm_82_triplet(a, k, i, q)
term(461) = term(461) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,k,p,j) * wm_interm_82_triplet(a, k, i, q)
term(462) = term(462) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,k,p,j) * wm_interm_82_triplet(b, k, i, q)
end do 
end do 
end do 
end do 
end do 

term(455) = term(455) * (-12.0d+0) 
term(456) = term(456) * (-18.0d+0) 
term(457) = term(457) * 12.0d+0 
term(458) = term(458) * 8.0d+0 
term(459) = term(459) * (-16.0d+0) 
term(460) = term(460) * (-12.0d+0) 
term(461) = term(461) * 8.0d+0 
term(462) = term(462) * (-4.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
do j = 1, nocc 
do b = nocc + 1, nactive 
term(463) = term(463) + r2p(vrdav_Rl, b,j,a,i) * t2(a,p,q,i) * wm_interm_0_triplet(b, j)
term(464) = term(464) + r2p(vrdav_Rl, b,j,a,i) * t2(a,p,q,i) * wm_interm_1_triplet(b, j)
term(465) = term(465) + r2p(vrdav_Rl, b,j,a,i) * t2(a,p,q,i) * wm_interm_2_triplet(b, j)
end do 
end do 
end do 
end do 

term(463) = term(463) * 18.0d+0 
term(464) = term(464) * 24.0d+0 
term(465) = term(465) * (-12.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do j = 1, nocc 
term(466) = term(466) + r2p(vrdav_Rr, p,q,a,i) * s2(a,b,j,i) * wm_interm_64_triplet(b, j)
term(467) = term(467) + r2p(vrdav_Rr, p,q,a,i) * s2(a,b,j,i) * wm_interm_63_triplet(b, j)
term(468) = term(468) + r2p(vrdav_Rl, p,q,a,i) * t2(a,b,j,i) * wm_interm_57_triplet(b, j)
term(469) = term(469) + r2p(vrdav_Rl, p,q,a,i) * t2(a,b,j,i) * wm_interm_58_triplet(b, j)
end do 
end do 
end do 
end do 

term(466) = term(466) * 6.0d+0 
term(467) = term(467) * (-12.0d+0) 
term(468) = term(468) * 6.0d+0 
term(469) = term(469) * (-12.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
term(470) = term(470) + wm_interm_55_triplet(i, j) * wm_interm_56_triplet(p, j, q, i)
term(471) = term(471) + wm_interm_55_triplet(i, j) * wm_interm_56_triplet(p, q, j, i)
term(472) = term(472) + wm_interm_55_triplet(i, j) * wm_interm_60_triplet(p, j, q, i)
term(473) = term(473) + wm_interm_55_triplet(i, j) * wm_interm_60_triplet(p, j, i, q)
term(474) = term(474) + wm_interm_68_triplet(i, j) * wm_interm_72_triplet(p, j, q, i)
term(475) = term(475) + wm_interm_68_triplet(i, j) * wm_interm_72_triplet(p, j, i, q)
term(476) = term(476) + wm_interm_69_triplet(i, j) * wm_interm_72_triplet(p, j, q, i)
term(477) = term(477) + wm_interm_69_triplet(i, j) * wm_interm_72_triplet(p, j, i, q)
term(478) = term(478) + wm_interm_27_triplet(i, j) * wm_interm_56_triplet(p, j, q, i)
term(479) = term(479) + wm_interm_27_triplet(i, j) * wm_interm_56_triplet(p, q, j, i)
term(480) = term(480) + wm_interm_31_triplet(i, j) * wm_interm_56_triplet(p, j, q, i)
term(481) = term(481) + wm_interm_30_triplet(i, j) * wm_interm_56_triplet(p, j, q, i)
term(482) = term(482) + wm_interm_31_triplet(i, j) * wm_interm_56_triplet(p, q, j, i)
term(483) = term(483) + wm_interm_30_triplet(i, j) * wm_interm_56_triplet(p, q, j, i)
term(484) = term(484) + wm_interm_68_triplet(i, j) * wm_interm_80_triplet(p, j, q, i)
term(485) = term(485) + wm_interm_69_triplet(i, j) * wm_interm_80_triplet(p, j, q, i)
term(486) = term(486) + wm_interm_27_triplet(i, j) * wm_interm_60_triplet(p, j, q, i)
term(487) = term(487) + wm_interm_27_triplet(i, j) * wm_interm_60_triplet(p, j, i, q)
term(488) = term(488) + wm_interm_30_triplet(i, j) * wm_interm_60_triplet(p, j, q, i)
term(489) = term(489) + wm_interm_31_triplet(i, j) * wm_interm_60_triplet(p, j, q, i)
term(490) = term(490) + wm_interm_30_triplet(i, j) * wm_interm_60_triplet(p, j, i, q)
term(491) = term(491) + wm_interm_31_triplet(i, j) * wm_interm_60_triplet(p, j, i, q)
term(492) = term(492) + wm_interm_41_triplet(i, j) * wm_interm_56_triplet(p, j, q, i)
term(493) = term(493) + wm_interm_41_triplet(i, j) * wm_interm_56_triplet(p, q, j, i)
term(494) = term(494) + wm_interm_40_triplet(i, j) * wm_interm_56_triplet(p, j, q, i)
term(495) = term(495) + wm_interm_40_triplet(i, j) * wm_interm_56_triplet(p, q, j, i)
term(496) = term(496) + wm_interm_45_triplet(i, j) * wm_interm_56_triplet(p, j, q, i)
term(497) = term(497) + wm_interm_46_triplet(i, j) * wm_interm_56_triplet(p, j, q, i)
term(498) = term(498) + wm_interm_45_triplet(i, j) * wm_interm_56_triplet(p, q, j, i)
term(499) = term(499) + wm_interm_46_triplet(i, j) * wm_interm_56_triplet(p, q, j, i)
term(500) = term(500) + wm_interm_43_triplet(i, j) * wm_interm_56_triplet(p, j, q, i)
term(501) = term(501) + wm_interm_44_triplet(i, j) * wm_interm_56_triplet(p, j, q, i)
term(502) = term(502) + wm_interm_43_triplet(i, j) * wm_interm_56_triplet(p, q, j, i)
term(503) = term(503) + wm_interm_44_triplet(i, j) * wm_interm_56_triplet(p, q, j, i)
term(504) = term(504) + wm_interm_68_triplet(i, j) * wm_interm_89_triplet(p, q, j, i)
term(505) = term(505) + wm_interm_69_triplet(i, j) * wm_interm_89_triplet(p, q, j, i)
term(506) = term(506) + wm_interm_68_triplet(i, j) * wm_interm_90_triplet(p, q, j, i)
term(507) = term(507) + wm_interm_69_triplet(i, j) * wm_interm_90_triplet(p, q, j, i)
term(508) = term(508) + wm_interm_68_triplet(i, j) * wm_interm_89_triplet(p, j, q, i)
term(509) = term(509) + wm_interm_68_triplet(i, j) * wm_interm_90_triplet(p, j, q, i)
term(510) = term(510) + wm_interm_69_triplet(i, j) * wm_interm_89_triplet(p, j, q, i)
term(511) = term(511) + wm_interm_69_triplet(i, j) * wm_interm_90_triplet(p, j, q, i)
term(512) = term(512) + wm_interm_40_triplet(i, j) * wm_interm_60_triplet(p, j, q, i)
term(513) = term(513) + wm_interm_41_triplet(i, j) * wm_interm_60_triplet(p, j, q, i)
term(514) = term(514) + wm_interm_40_triplet(i, j) * wm_interm_60_triplet(p, j, i, q)
term(515) = term(515) + wm_interm_41_triplet(i, j) * wm_interm_60_triplet(p, j, i, q)
term(516) = term(516) + wm_interm_43_triplet(i, j) * wm_interm_60_triplet(p, j, q, i)
term(517) = term(517) + wm_interm_44_triplet(i, j) * wm_interm_60_triplet(p, j, q, i)
term(518) = term(518) + wm_interm_45_triplet(i, j) * wm_interm_60_triplet(p, j, q, i)
term(519) = term(519) + wm_interm_46_triplet(i, j) * wm_interm_60_triplet(p, j, q, i)
term(520) = term(520) + wm_interm_43_triplet(i, j) * wm_interm_60_triplet(p, j, i, q)
term(521) = term(521) + wm_interm_44_triplet(i, j) * wm_interm_60_triplet(p, j, i, q)
term(522) = term(522) + wm_interm_45_triplet(i, j) * wm_interm_60_triplet(p, j, i, q)
term(523) = term(523) + wm_interm_46_triplet(i, j) * wm_interm_60_triplet(p, j, i, q)
end do 
end do 

term(470) = term(470) * 2.0d+0 
term(471) = term(471) * (-4.0d+0) 
term(472) = term(472) * 2.0d+0 
term(473) = term(473) * (-4.0d+0) 
term(474) = term(474) * (-4.0d+0) 
term(475) = term(475) * 8.0d+0 
term(476) = term(476) * 8.0d+0 
term(477) = term(477) * (-16.0d+0) 
term(478) = term(478) * 6.0d+0 
term(479) = term(479) * (-12.0d+0) 
term(480) = term(480) * (-6.0d+0) 
term(481) = term(481) * 6.0d+0 
term(482) = term(482) * 12.0d+0 
term(483) = term(483) * (-12.0d+0) 
term(484) = term(484) * 6.0d+0 
term(485) = term(485) * (-12.0d+0) 
term(486) = term(486) * 6.0d+0 
term(487) = term(487) * (-12.0d+0) 
term(488) = term(488) * 6.0d+0 
term(489) = term(489) * (-6.0d+0) 
term(490) = term(490) * (-12.0d+0) 
term(491) = term(491) * 12.0d+0 
term(492) = term(492) * 6.0d+0 
term(493) = term(493) * (-12.0d+0) 
term(494) = term(494) * (-6.0d+0) 
term(495) = term(495) * 12.0d+0 
term(496) = term(496) * (-4.0d+0) 
term(497) = term(497) * 8.0d+0 
term(498) = term(498) * 8.0d+0 
term(499) = term(499) * (-16.0d+0) 
term(500) = term(500) * (-4.0d+0) 
term(501) = term(501) * 8.0d+0 
term(502) = term(502) * 8.0d+0 
term(503) = term(503) * (-16.0d+0) 
term(504) = term(504) * 4.0d+0 
term(505) = term(505) * (-8.0d+0) 
term(506) = term(506) * (-2.0d+0) 
term(507) = term(507) * 4.0d+0 
term(508) = term(508) * (-2.0d+0) 
term(509) = term(509) * 4.0d+0 
term(510) = term(510) * 4.0d+0 
term(511) = term(511) * (-8.0d+0) 
term(512) = term(512) * (-6.0d+0) 
term(513) = term(513) * 6.0d+0 
term(514) = term(514) * 12.0d+0 
term(515) = term(515) * (-12.0d+0) 
term(516) = term(516) * (-4.0d+0) 
term(517) = term(517) * 8.0d+0 
term(518) = term(518) * (-4.0d+0) 
term(519) = term(519) * 8.0d+0 
term(520) = term(520) * 8.0d+0 
term(521) = term(521) * (-16.0d+0) 
term(522) = term(522) * 8.0d+0 
term(523) = term(523) * (-16.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
term(524) = term(524) + wm_interm_15_triplet(i, j) * wm_interm_59_triplet(p, i, j, q)
term(525) = term(525) + wm_interm_15_triplet(i, j) * wm_interm_59_triplet(p, i, q, j)
term(526) = term(526) + wm_interm_14_triplet(i, j) * wm_interm_66_triplet(p, i, q, j)
term(527) = term(527) + wm_interm_14_triplet(i, j) * wm_interm_66_triplet(p, i, j, q)
term(528) = term(528) + wm_interm_67_triplet(p, i, q, j) * wm_interm_68_triplet(j, i)
term(529) = term(529) + wm_interm_67_triplet(p, i, q, j) * wm_interm_69_triplet(j, i)
term(530) = term(530) + wm_interm_18_triplet(i, j) * wm_interm_66_triplet(p, i, q, j)
term(531) = term(531) + wm_interm_19_triplet(i, j) * wm_interm_66_triplet(p, i, q, j)
term(532) = term(532) + wm_interm_18_triplet(i, j) * wm_interm_66_triplet(p, i, j, q)
term(533) = term(533) + wm_interm_19_triplet(i, j) * wm_interm_66_triplet(p, i, j, q)
term(534) = term(534) + wm_interm_23_triplet(i, j) * wm_interm_66_triplet(p, i, q, j)
term(535) = term(535) + wm_interm_24_triplet(i, j) * wm_interm_66_triplet(p, i, q, j)
term(536) = term(536) + wm_interm_25_triplet(i, j) * wm_interm_66_triplet(p, i, q, j)
term(537) = term(537) + wm_interm_26_triplet(i, j) * wm_interm_66_triplet(p, i, q, j)
term(538) = term(538) + wm_interm_23_triplet(i, j) * wm_interm_66_triplet(p, i, j, q)
term(539) = term(539) + wm_interm_24_triplet(i, j) * wm_interm_66_triplet(p, i, j, q)
term(540) = term(540) + wm_interm_25_triplet(i, j) * wm_interm_66_triplet(p, i, j, q)
term(541) = term(541) + wm_interm_26_triplet(i, j) * wm_interm_66_triplet(p, i, j, q)
term(542) = term(542) + wm_interm_18_triplet(i, j) * wm_interm_35_triplet(p, i, q, j)
term(543) = term(543) + wm_interm_19_triplet(i, j) * wm_interm_35_triplet(p, i, q, j)
term(544) = term(544) + wm_interm_23_triplet(i, j) * wm_interm_35_triplet(p, i, q, j)
term(545) = term(545) + wm_interm_24_triplet(i, j) * wm_interm_35_triplet(p, i, q, j)
term(546) = term(546) + wm_interm_25_triplet(i, j) * wm_interm_35_triplet(p, i, q, j)
term(547) = term(547) + wm_interm_26_triplet(i, j) * wm_interm_35_triplet(p, i, q, j)
term(548) = term(548) + wm_interm_37_triplet(i, j) * wm_interm_59_triplet(p, i, j, q)
term(549) = term(549) + wm_interm_37_triplet(i, j) * wm_interm_59_triplet(p, i, q, j)
term(550) = term(550) + wm_interm_36_triplet(i, j) * wm_interm_59_triplet(p, i, j, q)
term(551) = term(551) + wm_interm_36_triplet(i, j) * wm_interm_59_triplet(p, i, q, j)
term(552) = term(552) + wm_interm_29_triplet(p, i, q, j) * wm_interm_37_triplet(i, j)
term(553) = term(553) + wm_interm_29_triplet(p, i, q, j) * wm_interm_36_triplet(i, j)
term(554) = term(554) + wm_interm_34_triplet(p, i, q, j) * wm_interm_37_triplet(i, j)
term(555) = term(555) + wm_interm_34_triplet(p, q, i, j) * wm_interm_37_triplet(i, j)
term(556) = term(556) + wm_interm_37_triplet(i, j) * wm_interm_83_triplet(p, q, i, j)
term(557) = term(557) + wm_interm_37_triplet(i, j) * wm_interm_83_triplet(p, i, q, j)
term(558) = term(558) + wm_interm_34_triplet(p, i, q, j) * wm_interm_36_triplet(i, j)
term(559) = term(559) + wm_interm_34_triplet(p, q, i, j) * wm_interm_36_triplet(i, j)
term(560) = term(560) + wm_interm_36_triplet(i, j) * wm_interm_83_triplet(p, q, i, j)
term(561) = term(561) + wm_interm_36_triplet(i, j) * wm_interm_83_triplet(p, i, q, j)
term(562) = term(562) + wm_interm_18_triplet(i, j) * wm_interm_50_triplet(p, i, q, j)
term(563) = term(563) + wm_interm_18_triplet(i, j) * wm_interm_50_triplet(p, q, i, j)
term(564) = term(564) + wm_interm_19_triplet(i, j) * wm_interm_50_triplet(p, i, q, j)
term(565) = term(565) + wm_interm_19_triplet(i, j) * wm_interm_50_triplet(p, q, i, j)
term(566) = term(566) + wm_interm_18_triplet(i, j) * wm_interm_49_triplet(p, q, i, j)
term(567) = term(567) + wm_interm_18_triplet(i, j) * wm_interm_49_triplet(p, i, q, j)
term(568) = term(568) + wm_interm_19_triplet(i, j) * wm_interm_49_triplet(p, q, i, j)
term(569) = term(569) + wm_interm_19_triplet(i, j) * wm_interm_49_triplet(p, i, q, j)
term(570) = term(570) + wm_interm_23_triplet(i, j) * wm_interm_50_triplet(p, i, q, j)
term(571) = term(571) + wm_interm_24_triplet(i, j) * wm_interm_50_triplet(p, i, q, j)
term(572) = term(572) + wm_interm_23_triplet(i, j) * wm_interm_50_triplet(p, q, i, j)
term(573) = term(573) + wm_interm_24_triplet(i, j) * wm_interm_50_triplet(p, q, i, j)
term(574) = term(574) + wm_interm_25_triplet(i, j) * wm_interm_50_triplet(p, i, q, j)
term(575) = term(575) + wm_interm_25_triplet(i, j) * wm_interm_50_triplet(p, q, i, j)
term(576) = term(576) + wm_interm_26_triplet(i, j) * wm_interm_50_triplet(p, i, q, j)
term(577) = term(577) + wm_interm_26_triplet(i, j) * wm_interm_50_triplet(p, q, i, j)
term(578) = term(578) + wm_interm_23_triplet(i, j) * wm_interm_49_triplet(p, q, i, j)
term(579) = term(579) + wm_interm_24_triplet(i, j) * wm_interm_49_triplet(p, q, i, j)
term(580) = term(580) + wm_interm_23_triplet(i, j) * wm_interm_49_triplet(p, i, q, j)
term(581) = term(581) + wm_interm_24_triplet(i, j) * wm_interm_49_triplet(p, i, q, j)
term(582) = term(582) + wm_interm_25_triplet(i, j) * wm_interm_49_triplet(p, q, i, j)
term(583) = term(583) + wm_interm_25_triplet(i, j) * wm_interm_49_triplet(p, i, q, j)
term(584) = term(584) + wm_interm_26_triplet(i, j) * wm_interm_49_triplet(p, q, i, j)
term(585) = term(585) + wm_interm_26_triplet(i, j) * wm_interm_49_triplet(p, i, q, j)
term(586) = term(586) + wm_interm_51_triplet(i, j) * wm_interm_59_triplet(p, i, j, q)
term(587) = term(587) + wm_interm_51_triplet(i, j) * wm_interm_59_triplet(p, i, q, j)
term(588) = term(588) + wm_interm_52_triplet(i, j) * wm_interm_59_triplet(p, i, j, q)
term(589) = term(589) + wm_interm_52_triplet(i, j) * wm_interm_59_triplet(p, i, q, j)
term(590) = term(590) + wm_interm_29_triplet(p, i, q, j) * wm_interm_51_triplet(i, j)
term(591) = term(591) + wm_interm_29_triplet(p, i, q, j) * wm_interm_52_triplet(i, j)
term(592) = term(592) + wm_interm_34_triplet(p, i, q, j) * wm_interm_51_triplet(i, j)
term(593) = term(593) + wm_interm_34_triplet(p, q, i, j) * wm_interm_51_triplet(i, j)
term(594) = term(594) + wm_interm_51_triplet(i, j) * wm_interm_83_triplet(p, q, i, j)
term(595) = term(595) + wm_interm_51_triplet(i, j) * wm_interm_83_triplet(p, i, q, j)
term(596) = term(596) + wm_interm_34_triplet(p, i, q, j) * wm_interm_52_triplet(i, j)
term(597) = term(597) + wm_interm_34_triplet(p, q, i, j) * wm_interm_52_triplet(i, j)
term(598) = term(598) + wm_interm_52_triplet(i, j) * wm_interm_83_triplet(p, q, i, j)
term(599) = term(599) + wm_interm_52_triplet(i, j) * wm_interm_83_triplet(p, i, q, j)
end do 
end do 

term(524) = term(524) * (-4.0d+0) 
term(525) = term(525) * 2.0d+0 
term(526) = term(526) * 2.0d+0 
term(527) = term(527) * (-4.0d+0) 
term(528) = term(528) * (-6.0d+0) 
term(529) = term(529) * 12.0d+0 
term(530) = term(530) * (-3.0d+0) 
term(531) = term(531) * 3.0d+0 
term(532) = term(532) * 6.0d+0 
term(533) = term(533) * (-6.0d+0) 
term(534) = term(534) * (-2.0d+0) 
term(535) = term(535) * 4.0d+0 
term(536) = term(536) * (-2.0d+0) 
term(537) = term(537) * 4.0d+0 
term(538) = term(538) * 4.0d+0 
term(539) = term(539) * (-8.0d+0) 
term(540) = term(540) * 4.0d+0 
term(541) = term(541) * (-8.0d+0) 
term(542) = term(542) * 9.0d+0 
term(543) = term(543) * (-9.0d+0) 
term(544) = term(544) * 6.0d+0 
term(545) = term(545) * (-12.0d+0) 
term(546) = term(546) * 6.0d+0 
term(547) = term(547) * (-12.0d+0) 
term(548) = term(548) * (-6.0d+0) 
term(549) = term(549) * 3.0d+0 
term(550) = term(550) * 6.0d+0 
term(551) = term(551) * (-3.0d+0) 
term(552) = term(552) * 9.0d+0 
term(553) = term(553) * (-9.0d+0) 
term(554) = term(554) * 3.0d+0 
term(555) = term(555) * (-6.0d+0) 
term(556) = term(556) * 3.0d+0 
term(557) = term(557) * (-6.0d+0) 
term(558) = term(558) * (-3.0d+0) 
term(559) = term(559) * 6.0d+0 
term(560) = term(560) * (-3.0d+0) 
term(561) = term(561) * 6.0d+0 
term(562) = term(562) * (-3.0d+0) 
term(563) = term(563) * 6.0d+0 
term(564) = term(564) * 3.0d+0 
term(565) = term(565) * (-6.0d+0) 
term(566) = term(566) * (-3.0d+0) 
term(567) = term(567) * 6.0d+0 
term(568) = term(568) * 3.0d+0 
term(569) = term(569) * (-6.0d+0) 
term(570) = term(570) * (-2.0d+0) 
term(571) = term(571) * 4.0d+0 
term(572) = term(572) * 4.0d+0 
term(573) = term(573) * (-8.0d+0) 
term(574) = term(574) * (-2.0d+0) 
term(575) = term(575) * 4.0d+0 
term(576) = term(576) * 4.0d+0 
term(577) = term(577) * (-8.0d+0) 
term(578) = term(578) * (-2.0d+0) 
term(579) = term(579) * 4.0d+0 
term(580) = term(580) * 4.0d+0 
term(581) = term(581) * (-8.0d+0) 
term(582) = term(582) * (-2.0d+0) 
term(583) = term(583) * 4.0d+0 
term(584) = term(584) * 4.0d+0 
term(585) = term(585) * (-8.0d+0) 
term(586) = term(586) * 8.0d+0 
term(587) = term(587) * (-4.0d+0) 
term(588) = term(588) * (-16.0d+0) 
term(589) = term(589) * 8.0d+0 
term(590) = term(590) * (-12.0d+0) 
term(591) = term(591) * 24.0d+0 
term(592) = term(592) * (-4.0d+0) 
term(593) = term(593) * 8.0d+0 
term(594) = term(594) * (-4.0d+0) 
term(595) = term(595) * 8.0d+0 
term(596) = term(596) * 8.0d+0 
term(597) = term(597) * (-16.0d+0) 
term(598) = term(598) * 8.0d+0 
term(599) = term(599) * (-16.0d+0) 

do i = 1, nocc 
term(600) = term(600) + r1(vrdav_Rl, p,i) * wm_interm_14_triplet(i, q)
term(601) = term(601) + r1(vrdav_Rr, p,i) * wm_interm_15_triplet(i, q)
term(602) = term(602) + r1(vrdav_Rl, p,i) * wm_interm_18_triplet(i, q)
term(603) = term(603) + r1(vrdav_Rl, p,i) * wm_interm_19_triplet(i, q)
term(604) = term(604) + r1(vrdav_Rl, p,i) * wm_interm_23_triplet(i, q)
term(605) = term(605) + r1(vrdav_Rl, p,i) * wm_interm_24_triplet(i, q)
term(606) = term(606) + r1(vrdav_Rl, p,i) * wm_interm_25_triplet(i, q)
term(607) = term(607) + r1(vrdav_Rl, p,i) * wm_interm_26_triplet(i, q)
term(608) = term(608) + r1(vrdav_Rr, p,i) * wm_interm_36_triplet(i, q)
term(609) = term(609) + r1(vrdav_Rr, p,i) * wm_interm_37_triplet(i, q)
term(610) = term(610) + r1(vrdav_Rr, p,i) * wm_interm_51_triplet(i, q)
term(611) = term(611) + r1(vrdav_Rr, p,i) * wm_interm_52_triplet(i, q)
term(612) = term(612) + wm_interm_15_triplet(q, i) * wm_interm_57_triplet(p, i)
term(613) = term(613) + wm_interm_15_triplet(q, i) * wm_interm_58_triplet(p, i)
term(614) = term(614) + wm_interm_3_triplet(p, i) * wm_interm_68_triplet(i, q)
term(615) = term(615) + wm_interm_3_triplet(p, i) * wm_interm_69_triplet(i, q)
term(616) = term(616) + wm_interm_4_triplet(p, i) * wm_interm_68_triplet(i, q)
term(617) = term(617) + wm_interm_5_triplet(p, i) * wm_interm_68_triplet(i, q)
term(618) = term(618) + wm_interm_4_triplet(p, i) * wm_interm_69_triplet(i, q)
term(619) = term(619) + wm_interm_5_triplet(p, i) * wm_interm_69_triplet(i, q)
term(620) = term(620) + wm_interm_37_triplet(q, i) * wm_interm_57_triplet(p, i)
term(621) = term(621) + wm_interm_37_triplet(q, i) * wm_interm_58_triplet(p, i)
term(622) = term(622) + wm_interm_36_triplet(q, i) * wm_interm_57_triplet(p, i)
term(623) = term(623) + wm_interm_36_triplet(q, i) * wm_interm_58_triplet(p, i)
term(624) = term(624) + wm_interm_0_triplet(p, i) * wm_interm_36_triplet(i, q)
term(625) = term(625) + wm_interm_0_triplet(p, i) * wm_interm_37_triplet(i, q)
term(626) = term(626) + wm_interm_1_triplet(p, i) * wm_interm_36_triplet(i, q)
term(627) = term(627) + wm_interm_1_triplet(p, i) * wm_interm_37_triplet(i, q)
term(628) = term(628) + wm_interm_2_triplet(p, i) * wm_interm_36_triplet(i, q)
term(629) = term(629) + wm_interm_2_triplet(p, i) * wm_interm_37_triplet(i, q)
term(630) = term(630) + wm_interm_51_triplet(q, i) * wm_interm_57_triplet(p, i)
term(631) = term(631) + wm_interm_51_triplet(q, i) * wm_interm_58_triplet(p, i)
term(632) = term(632) + wm_interm_52_triplet(q, i) * wm_interm_57_triplet(p, i)
term(633) = term(633) + wm_interm_52_triplet(q, i) * wm_interm_58_triplet(p, i)
term(634) = term(634) + wm_interm_0_triplet(p, i) * wm_interm_51_triplet(i, q)
term(635) = term(635) + wm_interm_0_triplet(p, i) * wm_interm_52_triplet(i, q)
term(636) = term(636) + wm_interm_1_triplet(p, i) * wm_interm_51_triplet(i, q)
term(637) = term(637) + wm_interm_1_triplet(p, i) * wm_interm_52_triplet(i, q)
term(638) = term(638) + wm_interm_2_triplet(p, i) * wm_interm_51_triplet(i, q)
term(639) = term(639) + wm_interm_2_triplet(p, i) * wm_interm_52_triplet(i, q)
end do 

term(600) = term(600) * 2.0d+0 
term(601) = term(601) * 2.0d+0 
term(602) = term(602) * (-3.0d+0) 
term(603) = term(603) * 3.0d+0 
term(604) = term(604) * (-2.0d+0) 
term(605) = term(605) * 4.0d+0 
term(606) = term(606) * (-2.0d+0) 
term(607) = term(607) * 4.0d+0 
term(608) = term(608) * (-3.0d+0) 
term(609) = term(609) * 3.0d+0 
term(610) = term(610) * (-4.0d+0) 
term(611) = term(611) * 8.0d+0 
term(612) = term(612) * 2.0d+0 
term(613) = term(613) * (-4.0d+0) 
term(614) = term(614) * 6.0d+0 
term(615) = term(615) * (-12.0d+0) 
term(616) = term(616) * (-4.0d+0) 
term(617) = term(617) * 8.0d+0 
term(618) = term(618) * 8.0d+0 
term(619) = term(619) * (-16.0d+0) 
term(620) = term(620) * (-3.0d+0) 
term(621) = term(621) * 6.0d+0 
term(622) = term(622) * 3.0d+0 
term(623) = term(623) * (-6.0d+0) 
term(624) = term(624) * (-9.0d+0) 
term(625) = term(625) * 9.0d+0 
term(626) = term(626) * (-12.0d+0) 
term(627) = term(627) * 12.0d+0 
term(628) = term(628) * 6.0d+0 
term(629) = term(629) * (-6.0d+0) 
term(630) = term(630) * (-4.0d+0) 
term(631) = term(631) * 8.0d+0 
term(632) = term(632) * 8.0d+0 
term(633) = term(633) * (-16.0d+0) 
term(634) = term(634) * (-12.0d+0) 
term(635) = term(635) * 24.0d+0 
term(636) = term(636) * (-16.0d+0) 
term(637) = term(637) * 32.0d+0 
term(638) = term(638) * 8.0d+0 
term(639) = term(639) * (-16.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(640) = term(640) + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, p,q,b,i) * wm_interm_70_triplet(b, a)
term(641) = term(641) + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, p,q,b,i) * wm_interm_71_triplet(b, a)
term(642) = term(642) + r1(vrdav_Rr, a,i) * s2(b,p,q,i) * wm_interm_38_triplet(a, b)
term(643) = term(643) + r1(vrdav_Rr, a,i) * s2(b,p,q,i) * wm_interm_39_triplet(a, b)
term(644) = term(644) + r1(vrdav_Rr, a,i) * s2(b,p,q,i) * wm_interm_53_triplet(a, b)
term(645) = term(645) + r1(vrdav_Rr, a,i) * s2(b,p,q,i) * wm_interm_54_triplet(a, b)
end do 
end do 
end do 

term(640) = term(640) * (-12.0d+0) 
term(641) = term(641) * 6.0d+0 
term(642) = term(642) * 3.0d+0 
term(643) = term(643) * (-3.0d+0) 
term(644) = term(644) * (-4.0d+0) 
term(645) = term(645) * 8.0d+0 

do i = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(646) = term(646) + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, b,q,p,i) * wm_interm_70_triplet(b, a)
term(647) = term(647) + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, b,q,p,i) * wm_interm_71_triplet(b, a)
term(648) = term(648) + r1(vrdav_Rl, b,i) * t2(a,p,q,i) * wm_interm_17_triplet(b, a)
term(649) = term(649) + r1(vrdav_Rl, b,i) * t2(a,p,q,i) * wm_interm_20_triplet(b, a)
term(650) = term(650) + r1(vrdav_Rl, b,i) * t2(a,p,q,i) * wm_interm_21_triplet(b, a)
term(651) = term(651) + r1(vrdav_Rl, b,i) * t2(a,p,q,i) * wm_interm_22_triplet(b, a)
term(652) = term(652) + r2p(vrdav_Rl, p,q,a,i) * r1(vrdav_Rr, b,i) * wm_interm_70_triplet(b, a)
term(653) = term(653) + r2p(vrdav_Rl, p,q,a,i) * r1(vrdav_Rr, b,i) * wm_interm_71_triplet(b, a)
term(654) = term(654) + r2m(vrdav_Rl, a,q,p,i) * r1(vrdav_Rr, b,i) * wm_interm_70_triplet(b, a)
term(655) = term(655) + r2m(vrdav_Rl, a,q,p,i) * r1(vrdav_Rr, b,i) * wm_interm_71_triplet(b, a)
end do 
end do 
end do 

term(646) = term(646) * 8.0d+0 
term(647) = term(647) * (-4.0d+0) 
term(648) = term(648) * 3.0d+0 
term(649) = term(649) * (-3.0d+0) 
term(650) = term(650) * (-4.0d+0) 
term(651) = term(651) * 8.0d+0 
term(652) = term(652) * (-12.0d+0) 
term(653) = term(653) * 6.0d+0 
term(654) = term(654) * 8.0d+0 
term(655) = term(655) * (-4.0d+0) 

do a = nocc + 1, nactive 
do i = 1, nocc 
do b = nocc + 1, nactive 
term(656) = term(656) + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, b,i,p,q) * wm_interm_70_triplet(b, a)
term(657) = term(657) + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, b,i,p,q) * wm_interm_71_triplet(b, a)
term(658) = term(658) + r2m(vrdav_Rl, a,i,p,q) * r1(vrdav_Rr, b,i) * wm_interm_70_triplet(b, a)
term(659) = term(659) + r2m(vrdav_Rl, a,i,p,q) * r1(vrdav_Rr, b,i) * wm_interm_71_triplet(b, a)
end do 
end do 
end do 

term(656) = term(656) * (-16.0d+0) 
term(657) = term(657) * 8.0d+0 
term(658) = term(658) * (-16.0d+0) 
term(659) = term(659) * 8.0d+0 


    calc_D_vo_wm_triplet = zero
    do s = 0, 659
    calc_D_vo_wm_triplet = calc_D_vo_wm_triplet + term(s)
    end do

    end function calc_D_vo_wm_triplet
    
    function calc_D_vv_wm_triplet(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vv_wm_triplet
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
    real(F64), dimension(0:125) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(0) = term(0) + wm_interm_56_triplet(q, i, j, k) * wm_interm_67_triplet(p, k, j, i)
term(1) = term(1) + wm_interm_56_triplet(q, i, j, k) * wm_interm_72_triplet(p, k, j, i)
term(2) = term(2) + wm_interm_60_triplet(q, i, j, k) * wm_interm_80_triplet(p, j, k, i)
term(3) = term(3) + wm_interm_60_triplet(q, i, j, k) * wm_interm_80_triplet(p, k, j, i)
term(4) = term(4) + wm_interm_60_triplet(q, i, j, k) * wm_interm_89_triplet(p, j, k, i)
term(5) = term(5) + wm_interm_60_triplet(q, i, j, k) * wm_interm_89_triplet(p, k, j, i)
term(6) = term(6) + wm_interm_60_triplet(q, i, j, k) * wm_interm_90_triplet(p, k, j, i)
term(7) = term(7) + wm_interm_60_triplet(q, i, j, k) * wm_interm_90_triplet(p, j, k, i)
end do 
end do 
end do 

term(0) = term(0) * (-3.0d+0) 
term(1) = term(1) * (-8.0d+0) 
term(2) = term(2) * (-3.0d+0) 
term(3) = term(3) * 3.0d+0 
term(4) = term(4) * 2.0d+0 
term(5) = term(5) * (-4.0d+0) 
term(6) = term(6) * 2.0d+0 
term(7) = term(7) * (-4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(8) = term(8) + wm_interm_56_triplet(q, i, j, k) * wm_interm_67_triplet(p, k, i, j)
term(9) = term(9) + wm_interm_56_triplet(q, i, j, k) * wm_interm_72_triplet(p, k, i, j)
term(10) = term(10) + wm_interm_59_triplet(p, i, j, k) * wm_interm_66_triplet(q, i, j, k)
term(11) = term(11) + wm_interm_59_triplet(p, i, j, k) * wm_interm_66_triplet(q, i, k, j)
end do 
end do 
end do 

term(8) = term(8) * 3.0d+0 
term(9) = term(9) * 4.0d+0 
term(10) = term(10) * (-4.0d+0) 
term(11) = term(11) * 2.0d+0 

do k = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
do j = 1, nocc 
term(12) = term(12) + r2p(vrdav_Rl, p,j,a,k) * t2(a,q,k,i) * wm_interm_14_triplet(j, i)
end do 
end do 
end do 
end do 

term(12) = term(12) * (-6.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
do k = 1, nocc 
do j = 1, nocc 
term(13) = term(13) + r2p(vrdav_Rl, p,j,a,i) * t2(a,q,k,i) * wm_interm_14_triplet(j, k)
end do 
end do 
end do 
end do 

term(13) = term(13) * 6.0d+0 

do i = 1, nocc 
do a = nocc + 1, nactive 
do j = 1, nocc 
term(14) = term(14) + r2p(vrdav_Rl, q,j,a,i) * r1(vrdav_Rr, p,i) * wm_interm_61_triplet(a, j)
term(15) = term(15) + r2p(vrdav_Rl, q,j,a,i) * r1(vrdav_Rr, p,i) * wm_interm_65_triplet(a, j)
end do 
end do 
end do 

term(14) = term(14) * 6.0d+0 
term(15) = term(15) * (-12.0d+0) 

do a = nocc + 1, nactive 
term(16) = term(16) + wm_interm_16_triplet(q, a) * wm_interm_17_triplet(p, a)
term(17) = term(17) + wm_interm_16_triplet(q, a) * wm_interm_20_triplet(p, a)
term(18) = term(18) + wm_interm_16_triplet(q, a) * wm_interm_21_triplet(p, a)
term(19) = term(19) + wm_interm_16_triplet(q, a) * wm_interm_22_triplet(p, a)
term(20) = term(20) + wm_interm_62_triplet(a, p) * wm_interm_70_triplet(q, a)
term(21) = term(21) + wm_interm_62_triplet(a, p) * wm_interm_71_triplet(q, a)
term(22) = term(22) + wm_interm_16_triplet(a, q) * wm_interm_17_triplet(a, p)
term(23) = term(23) + wm_interm_16_triplet(a, q) * wm_interm_20_triplet(a, p)
term(24) = term(24) + wm_interm_16_triplet(a, q) * wm_interm_21_triplet(a, p)
term(25) = term(25) + wm_interm_16_triplet(a, q) * wm_interm_22_triplet(a, p)
term(26) = term(26) + wm_interm_62_triplet(p, a) * wm_interm_70_triplet(a, q)
term(27) = term(27) + wm_interm_62_triplet(p, a) * wm_interm_71_triplet(a, q)
term(28) = term(28) + wm_interm_12_triplet(p, a) * wm_interm_38_triplet(q, a)
term(29) = term(29) + wm_interm_12_triplet(p, a) * wm_interm_39_triplet(q, a)
term(30) = term(30) + wm_interm_12_triplet(a, p) * wm_interm_38_triplet(a, q)
term(31) = term(31) + wm_interm_12_triplet(a, p) * wm_interm_39_triplet(a, q)
term(32) = term(32) + wm_interm_12_triplet(p, a) * wm_interm_53_triplet(q, a)
term(33) = term(33) + wm_interm_12_triplet(p, a) * wm_interm_54_triplet(q, a)
term(34) = term(34) + wm_interm_12_triplet(a, p) * wm_interm_53_triplet(a, q)
term(35) = term(35) + wm_interm_12_triplet(a, p) * wm_interm_54_triplet(a, q)
end do 

term(16) = term(16) * (-3.0d+0) 
term(17) = term(17) * 3.0d+0 
term(18) = term(18) * 4.0d+0 
term(19) = term(19) * (-8.0d+0) 
term(20) = term(20) * (-4.0d+0) 
term(21) = term(21) * 2.0d+0 
term(22) = term(22) * (-3.0d+0) 
term(23) = term(23) * 3.0d+0 
term(24) = term(24) * 4.0d+0 
term(25) = term(25) * (-8.0d+0) 
term(26) = term(26) * (-4.0d+0) 
term(27) = term(27) * 2.0d+0 
term(28) = term(28) * 3.0d+0 
term(29) = term(29) * (-3.0d+0) 
term(30) = term(30) * (-3.0d+0) 
term(31) = term(31) * 3.0d+0 
term(32) = term(32) * 4.0d+0 
term(33) = term(33) * (-8.0d+0) 
term(34) = term(34) * 4.0d+0 
term(35) = term(35) * (-8.0d+0) 

term(36) = term(36) + wm_interm_13_triplet * wm_interm_70_triplet(p, q)
term(37) = term(37) + wm_interm_13_triplet * wm_interm_71_triplet(p, q)

term(36) = term(36) * 8.0d+0 
term(37) = term(37) * (-4.0d+0) 

do j = 1, nocc 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(38) = term(38) + r2p(vrdav_Rl, p,i,a,j) * r2p(vrdav_Rr, q,i,a,j)
term(39) = term(39) + r2p(vrdav_Rl, p,i,a,j) * r2m(vrdav_Rr, a,i,q,j)
term(40) = term(40) + r2m(vrdav_Rl, a,i,p,j) * r2p(vrdav_Rr, q,i,a,j)
end do 
end do 
end do 

term(38) = term(38) * 6.0d+0 
term(39) = term(39) * (-6.0d+0) 
term(40) = term(40) * (-6.0d+0) 

do i = 1, nocc 
term(41) = term(41) + r1(vrdav_Rl, q,i) * wm_interm_0_triplet(p, i)
term(42) = term(42) + r1(vrdav_Rl, q,i) * wm_interm_1_triplet(p, i)
term(43) = term(43) + r1(vrdav_Rl, q,i) * wm_interm_2_triplet(p, i)
term(44) = term(44) + s1(q,i) * wm_interm_3_triplet(p, i)
term(45) = term(45) + s1(q,i) * wm_interm_4_triplet(p, i)
term(46) = term(46) + s1(q,i) * wm_interm_5_triplet(p, i)
term(47) = term(47) + r1(vrdav_Rr, p,i) * wm_interm_6_triplet(q, i)
term(48) = term(48) + t1(q,i) * wm_interm_7_triplet(p, i)
term(49) = term(49) + r1(vrdav_Rr, p,i) * wm_interm_8_triplet(q, i)
term(50) = term(50) + r1(vrdav_Rr, p,i) * wm_interm_9_triplet(q, i)
term(51) = term(51) + t1(q,i) * wm_interm_10_triplet(p, i)
term(52) = term(52) + t1(q,i) * wm_interm_11_triplet(p, i)
term(53) = term(53) + wm_interm_57_triplet(p, i) * wm_interm_64_triplet(q, i)
term(54) = term(54) + wm_interm_58_triplet(p, i) * wm_interm_64_triplet(q, i)
term(55) = term(55) + wm_interm_58_triplet(p, i) * wm_interm_63_triplet(q, i)
term(56) = term(56) + wm_interm_57_triplet(p, i) * wm_interm_63_triplet(q, i)
term(57) = term(57) + wm_interm_61_triplet(q, i) * wm_interm_7_triplet(p, i)
term(58) = term(58) + wm_interm_65_triplet(q, i) * wm_interm_7_triplet(p, i)
term(59) = term(59) + wm_interm_10_triplet(p, i) * wm_interm_61_triplet(q, i)
term(60) = term(60) + wm_interm_11_triplet(p, i) * wm_interm_61_triplet(q, i)
term(61) = term(61) + wm_interm_10_triplet(p, i) * wm_interm_65_triplet(q, i)
term(62) = term(62) + wm_interm_11_triplet(p, i) * wm_interm_65_triplet(q, i)
term(63) = term(63) + r1(vrdav_Rl, p,i) * r1(vrdav_Rr, q,i)
end do 

term(41) = term(41) * 6.0d+0 
term(42) = term(42) * 8.0d+0 
term(43) = term(43) * (-4.0d+0) 
term(44) = term(44) * (-6.0d+0) 
term(45) = term(45) * 4.0d+0 
term(46) = term(46) * (-8.0d+0) 
term(47) = term(47) * 6.0d+0 
term(48) = term(48) * (-6.0d+0) 
term(49) = term(49) * (-8.0d+0) 
term(50) = term(50) * 4.0d+0 
term(51) = term(51) * 4.0d+0 
term(52) = term(52) * (-8.0d+0) 
term(53) = term(53) * 2.0d+0 
term(54) = term(54) * (-4.0d+0) 
term(55) = term(55) * 8.0d+0 
term(56) = term(56) * (-4.0d+0) 
term(57) = term(57) * (-6.0d+0) 
term(58) = term(58) * 12.0d+0 
term(59) = term(59) * 4.0d+0 
term(60) = term(60) * (-8.0d+0) 
term(61) = term(61) * (-8.0d+0) 
term(62) = term(62) * 16.0d+0 
term(63) = term(63) * 2.0d+0 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(64) = term(64) + r2p(vrdav_Rl, p,i,a,j) * r2m(vrdav_Rr, a,j,q,i)
term(65) = term(65) + r2m(vrdav_Rl, a,j,p,i) * r2p(vrdav_Rr, q,i,a,j)
term(66) = term(66) + r2m(vrdav_Rl, a,j,p,i) * r2m(vrdav_Rr, a,i,q,j)
term(67) = term(67) + r2m(vrdav_Rl, a,i,p,j) * r2m(vrdav_Rr, a,j,q,i)
term(68) = term(68) + r2m(vrdav_Rl, a,i,p,j) * r2m(vrdav_Rr, a,i,q,j)
term(69) = term(69) + r1(vrdav_Rr, p,i) * s2(a,q,i,j) * wm_interm_64_triplet(a, j)
term(70) = term(70) + r1(vrdav_Rr, p,i) * s2(a,q,i,j) * wm_interm_63_triplet(a, j)
term(71) = term(71) + s2(a,p,i,j) * t1(q,i) * wm_interm_3_triplet(a, j)
term(72) = term(72) + s2(a,p,i,j) * t1(q,i) * wm_interm_4_triplet(a, j)
term(73) = term(73) + s2(a,p,i,j) * t1(q,i) * wm_interm_5_triplet(a, j)
term(74) = term(74) + r1(vrdav_Rl, p,i) * t2(a,q,i,j) * wm_interm_57_triplet(a, j)
term(75) = term(75) + r1(vrdav_Rl, p,i) * t2(a,q,i,j) * wm_interm_58_triplet(a, j)
term(76) = term(76) + s1(p,i) * t2(a,q,i,j) * wm_interm_7_triplet(a, j)
term(77) = term(77) + r2m(vrdav_Rl, a,i,q,j) * r1(vrdav_Rr, p,i) * wm_interm_61_triplet(a, j)
term(78) = term(78) + r2m(vrdav_Rl, a,i,q,j) * r1(vrdav_Rr, p,i) * wm_interm_65_triplet(a, j)
term(79) = term(79) + s1(p,i) * t2(a,q,i,j) * wm_interm_11_triplet(a, j)
term(80) = term(80) + s1(p,i) * t2(a,q,i,j) * wm_interm_10_triplet(a, j)
end do 
end do 
end do 

term(64) = term(64) * 6.0d+0 
term(65) = term(65) * 6.0d+0 
term(66) = term(66) * (-4.0d+0) 
term(67) = term(67) * (-4.0d+0) 
term(68) = term(68) * 8.0d+0 
term(69) = term(69) * 2.0d+0 
term(70) = term(70) * (-4.0d+0) 
term(71) = term(71) * (-6.0d+0) 
term(72) = term(72) * 4.0d+0 
term(73) = term(73) * (-8.0d+0) 
term(74) = term(74) * 2.0d+0 
term(75) = term(75) * (-4.0d+0) 
term(76) = term(76) * (-6.0d+0) 
term(77) = term(77) * 4.0d+0 
term(78) = term(78) * (-8.0d+0) 
term(79) = term(79) * (-8.0d+0) 
term(80) = term(80) * 4.0d+0 

do j = 1, nocc 
do i = 1, nocc 
term(81) = term(81) + r1(vrdav_Rl, q,j) * r1(vrdav_Rr, p,i) * wm_interm_68_triplet(i, j)
term(82) = term(82) + r1(vrdav_Rl, q,j) * r1(vrdav_Rr, p,i) * wm_interm_69_triplet(i, j)
term(83) = term(83) + r1(vrdav_Rl, p,i) * t1(q,j) * wm_interm_18_triplet(i, j)
term(84) = term(84) + r1(vrdav_Rl, p,i) * t1(q,j) * wm_interm_19_triplet(i, j)
term(85) = term(85) + r1(vrdav_Rl, p,i) * t1(q,j) * wm_interm_23_triplet(i, j)
term(86) = term(86) + r1(vrdav_Rl, p,i) * t1(q,j) * wm_interm_24_triplet(i, j)
term(87) = term(87) + r1(vrdav_Rl, p,i) * t1(q,j) * wm_interm_25_triplet(i, j)
term(88) = term(88) + r1(vrdav_Rl, p,i) * t1(q,j) * wm_interm_26_triplet(i, j)
term(89) = term(89) + r1(vrdav_Rr, p,i) * s1(q,j) * wm_interm_37_triplet(i, j)
term(90) = term(90) + r1(vrdav_Rr, p,i) * s1(q,j) * wm_interm_36_triplet(i, j)
term(91) = term(91) + r1(vrdav_Rr, p,i) * s1(q,j) * wm_interm_51_triplet(i, j)
term(92) = term(92) + r1(vrdav_Rr, p,i) * s1(q,j) * wm_interm_52_triplet(i, j)
end do 
end do 

term(81) = term(81) * 2.0d+0 
term(82) = term(82) * (-4.0d+0) 
term(83) = term(83) * 3.0d+0 
term(84) = term(84) * (-3.0d+0) 
term(85) = term(85) * 2.0d+0 
term(86) = term(86) * (-4.0d+0) 
term(87) = term(87) * 2.0d+0 
term(88) = term(88) * (-4.0d+0) 
term(89) = term(89) * (-3.0d+0) 
term(90) = term(90) * 3.0d+0 
term(91) = term(91) * 4.0d+0 
term(92) = term(92) * (-8.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(93) = term(93) + r2m(vrdav_Rl, a,j,p,i) * r2m(vrdav_Rr, a,j,q,i)
term(94) = term(94) + r1(vrdav_Rr, p,i) * s2(a,q,j,i) * wm_interm_64_triplet(a, j)
term(95) = term(95) + r1(vrdav_Rr, p,i) * s2(a,q,j,i) * wm_interm_63_triplet(a, j)
term(96) = term(96) + s2(a,p,j,i) * t1(q,i) * wm_interm_3_triplet(a, j)
term(97) = term(97) + s2(a,p,j,i) * t1(q,i) * wm_interm_4_triplet(a, j)
term(98) = term(98) + s2(a,p,j,i) * t1(q,i) * wm_interm_5_triplet(a, j)
term(99) = term(99) + r1(vrdav_Rl, p,i) * t2(a,q,j,i) * wm_interm_58_triplet(a, j)
term(100) = term(100) + r1(vrdav_Rl, p,i) * t2(a,q,j,i) * wm_interm_57_triplet(a, j)
term(101) = term(101) + s1(p,i) * t2(a,q,j,i) * wm_interm_7_triplet(a, j)
term(102) = term(102) + r2m(vrdav_Rl, a,j,q,i) * r1(vrdav_Rr, p,i) * wm_interm_61_triplet(a, j)
term(103) = term(103) + r2m(vrdav_Rl, a,j,q,i) * r1(vrdav_Rr, p,i) * wm_interm_65_triplet(a, j)
term(104) = term(104) + s1(p,i) * t2(a,q,j,i) * wm_interm_10_triplet(a, j)
term(105) = term(105) + s1(p,i) * t2(a,q,j,i) * wm_interm_11_triplet(a, j)
end do 
end do 
end do 

term(93) = term(93) * 8.0d+0 
term(94) = term(94) * (-4.0d+0) 
term(95) = term(95) * 8.0d+0 
term(96) = term(96) * 12.0d+0 
term(97) = term(97) * (-8.0d+0) 
term(98) = term(98) * 16.0d+0 
term(99) = term(99) * 8.0d+0 
term(100) = term(100) * (-4.0d+0) 
term(101) = term(101) * 12.0d+0 
term(102) = term(102) * (-8.0d+0) 
term(103) = term(103) * 16.0d+0 
term(104) = term(104) * (-8.0d+0) 
term(105) = term(105) * 16.0d+0 

do j = 1, nocc 
do a = nocc + 1, nactive 
do k = 1, nocc 
do i = 1, nocc 
term(106) = term(106) + r2p(vrdav_Rr, p,i,a,j) * s2(a,q,k,j) * wm_interm_15_triplet(i, k)
end do 
end do 
end do 
end do 

term(106) = term(106) * 6.0d+0 

do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(107) = term(107) + r2p(vrdav_Rr, p,i,a,j) * s2(a,q,j,k) * wm_interm_15_triplet(i, k)
end do 
end do 
end do 
end do 

term(107) = term(107) * (-6.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(108) = term(108) + r2m(vrdav_Rr, a,j,p,i) * s2(a,q,k,j) * wm_interm_15_triplet(i, k)
term(109) = term(109) + r2m(vrdav_Rr, a,i,p,j) * s2(a,q,k,i) * wm_interm_15_triplet(j, k)
term(110) = term(110) + s2(a,p,k,j) * t2(a,q,j,i) * wm_interm_55_triplet(i, k)
term(111) = term(111) + s2(a,p,k,j) * t2(a,q,k,i) * wm_interm_55_triplet(i, j)
term(112) = term(112) + s2(a,p,i,j) * t2(a,q,k,i) * wm_interm_55_triplet(k, j)
term(113) = term(113) + r2m(vrdav_Rl, a,k,p,j) * t2(a,q,j,i) * wm_interm_14_triplet(k, i)
term(114) = term(114) + r2m(vrdav_Rl, a,k,p,j) * t2(a,q,k,i) * wm_interm_14_triplet(j, i)
term(115) = term(115) + r2m(vrdav_Rl, a,i,p,j) * t2(a,q,k,i) * wm_interm_14_triplet(j, k)
end do 
end do 
end do 
end do 

term(108) = term(108) * 2.0d+0 
term(109) = term(109) * 2.0d+0 
term(110) = term(110) * 2.0d+0 
term(111) = term(111) * (-4.0d+0) 
term(112) = term(112) * 2.0d+0 
term(113) = term(113) * 2.0d+0 
term(114) = term(114) * (-4.0d+0) 
term(115) = term(115) * 4.0d+0 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(116) = term(116) + r2m(vrdav_Rr, a,j,p,i) * s2(a,q,k,i) * wm_interm_15_triplet(j, k)
term(117) = term(117) + s2(a,p,j,i) * t2(a,q,k,i) * wm_interm_55_triplet(k, j)
term(118) = term(118) + r2m(vrdav_Rl, a,j,p,i) * t2(a,q,k,i) * wm_interm_14_triplet(j, k)
end do 
end do 
end do 
end do 

term(116) = term(116) * (-4.0d+0) 
term(117) = term(117) * (-4.0d+0) 
term(118) = term(118) * (-8.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(119) = term(119) + r2m(vrdav_Rr, a,j,p,i) * s2(a,q,i,k) * wm_interm_15_triplet(j, k)
term(120) = term(120) + r2m(vrdav_Rr, a,j,p,i) * s2(a,q,j,k) * wm_interm_15_triplet(i, k)
term(121) = term(121) + r2m(vrdav_Rl, a,j,p,k) * t2(a,q,k,i) * wm_interm_14_triplet(j, i)
term(122) = term(122) + r2m(vrdav_Rl, a,j,p,k) * t2(a,q,j,i) * wm_interm_14_triplet(k, i)
end do 
end do 
end do 
end do 

term(119) = term(119) * 2.0d+0 
term(120) = term(120) * (-4.0d+0) 
term(121) = term(121) * 2.0d+0 
term(122) = term(122) * (-4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(123) = term(123) + r2m(vrdav_Rr, a,i,p,j) * s2(a,q,j,k) * wm_interm_15_triplet(i, k)
term(124) = term(124) + r2m(vrdav_Rr, a,i,p,j) * s2(a,q,i,k) * wm_interm_15_triplet(j, k)
end do 
end do 
end do 
end do 

term(123) = term(123) * 2.0d+0 
term(124) = term(124) * (-4.0d+0) 

do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(125) = term(125) + r2m(vrdav_Rr, a,i,p,j) * s2(a,q,k,j) * wm_interm_15_triplet(i, k)
end do 
end do 
end do 
end do 

term(125) = term(125) * (-4.0d+0) 


    calc_D_vv_wm_triplet = zero
    do s = 0, 125
    calc_D_vv_wm_triplet = calc_D_vv_wm_triplet + term(s)
    end do

    end function calc_D_vv_wm_triplet
    





  

!       subroutine wm_triplet_intermediates_ccsd_init(nocc, nactive)
!     integer, intent(in) :: nocc
!     integer, intent(in) :: nactive
!     allocate(wm_interm_0_triplet(nocc+1: nactive, 1: nocc))
! allocate(wm_interm_1_triplet(nocc+1: nactive, 1: nocc))
! allocate(wm_interm_2_triplet(nocc+1: nactive, 1: nocc))
! allocate(wm_interm_3_triplet(nocc+1: nactive, 1: nocc))
! allocate(wm_interm_4_triplet(nocc+1: nactive, 1: nocc))
! allocate(wm_interm_5_triplet(nocc+1: nactive, 1: nocc))
! allocate(wm_interm_6_triplet(nocc+1: nactive, 1: nocc))
! allocate(wm_interm_7_triplet(nocc+1: nactive, 1: nocc))
! allocate(wm_interm_8_triplet(nocc+1: nactive, 1: nocc))
! allocate(wm_interm_9_triplet(nocc+1: nactive, 1: nocc))
! allocate(wm_interm_10_triplet(nocc+1: nactive, 1: nocc))
! allocate(wm_interm_11_triplet(nocc+1: nactive, 1: nocc))
! allocate(wm_interm_12_triplet(nocc+1: nactive, nocc+1: nactive))
! allocate(wm_interm_14_triplet(1: nocc, 1: nocc))
! allocate(wm_interm_15_triplet(1: nocc, 1: nocc))
! allocate(wm_interm_16_triplet(nocc+1: nactive, nocc+1: nactive))
! allocate(wm_interm_17_triplet(nocc+1: nactive, nocc+1: nactive))
! allocate(wm_interm_18_triplet(1: nocc, 1: nocc))
! allocate(wm_interm_19_triplet(1: nocc, 1: nocc))
! allocate(wm_interm_20_triplet(nocc+1: nactive, nocc+1: nactive))
! allocate(wm_interm_21_triplet(nocc+1: nactive, nocc+1: nactive))
! allocate(wm_interm_22_triplet(nocc+1: nactive, nocc+1: nactive))
! allocate(wm_interm_23_triplet(1: nocc, 1: nocc))
! allocate(wm_interm_24_triplet(1: nocc, 1: nocc))
! allocate(wm_interm_25_triplet(1: nocc, 1: nocc))
! allocate(wm_interm_26_triplet(1: nocc, 1: nocc))
! allocate(wm_interm_27_triplet(1: nocc, 1: nocc))
! allocate(wm_interm_29_triplet(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
! allocate(wm_interm_30_triplet(1: nocc, 1: nocc))
! allocate(wm_interm_31_triplet(1: nocc, 1: nocc))
! allocate(wm_interm_34_triplet(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
! allocate(wm_interm_35_triplet(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
! allocate(wm_interm_36_triplet(1: nocc, 1: nocc))
! allocate(wm_interm_37_triplet(1: nocc, 1: nocc))
! allocate(wm_interm_38_triplet(nocc+1: nactive, nocc+1: nactive))
! allocate(wm_interm_39_triplet(nocc+1: nactive, nocc+1: nactive))
! allocate(wm_interm_40_triplet(1: nocc, 1: nocc))
! allocate(wm_interm_41_triplet(1: nocc, 1: nocc))
! allocate(wm_interm_43_triplet(1: nocc, 1: nocc))
! allocate(wm_interm_44_triplet(1: nocc, 1: nocc))
! allocate(wm_interm_45_triplet(1: nocc, 1: nocc))
! allocate(wm_interm_46_triplet(1: nocc, 1: nocc))
! allocate(wm_interm_49_triplet(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
! allocate(wm_interm_50_triplet(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
! allocate(wm_interm_51_triplet(1: nocc, 1: nocc))
! allocate(wm_interm_52_triplet(1: nocc, 1: nocc))
! allocate(wm_interm_53_triplet(nocc+1: nactive, nocc+1: nactive))
! allocate(wm_interm_54_triplet(nocc+1: nactive, nocc+1: nactive))
! allocate(wm_interm_55_triplet(1: nocc, 1: nocc))
! allocate(wm_interm_56_triplet(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
! allocate(wm_interm_57_triplet(nocc+1: nactive, 1: nocc))
! allocate(wm_interm_58_triplet(nocc+1: nactive, 1: nocc))
! allocate(wm_interm_59_triplet(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
! allocate(wm_interm_60_triplet(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
! allocate(wm_interm_61_triplet(nocc+1: nactive, 1: nocc))
! allocate(wm_interm_62_triplet(nocc+1: nactive, nocc+1: nactive))
! allocate(wm_interm_63_triplet(nocc+1: nactive, 1: nocc))
! allocate(wm_interm_64_triplet(nocc+1: nactive, 1: nocc))
! allocate(wm_interm_65_triplet(nocc+1: nactive, 1: nocc))
! allocate(wm_interm_66_triplet(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
! allocate(wm_interm_67_triplet(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
! allocate(wm_interm_68_triplet(1: nocc, 1: nocc))
! allocate(wm_interm_69_triplet(1: nocc, 1: nocc))
! allocate(wm_interm_70_triplet(nocc+1: nactive, nocc+1: nactive))
! allocate(wm_interm_71_triplet(nocc+1: nactive, nocc+1: nactive))
! allocate(wm_interm_72_triplet(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
! allocate(wm_interm_73_triplet(1: nocc, 1: nocc, 1: nocc, 1: nocc))
! allocate(wm_interm_74_triplet(1: nocc, 1: nocc, 1: nocc, 1: nocc))
! allocate(wm_interm_75_triplet(nocc+1: nactive, nocc+1: nactive))
! allocate(wm_interm_76_triplet(nocc+1: nactive, nocc+1: nactive))
! allocate(wm_interm_77_triplet(1: nocc, 1: nocc, 1: nocc, 1: nocc))
! allocate(wm_interm_78_triplet(1: nocc, 1: nocc, 1: nocc, 1: nocc))
! allocate(wm_interm_79_triplet(1: nocc, 1: nocc, 1: nocc, 1: nocc))
! allocate(wm_interm_80_triplet(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
! allocate(wm_interm_81_triplet(1: nocc, 1: nocc, 1: nocc, 1: nocc))
! allocate(wm_interm_82_triplet(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
! allocate(wm_interm_83_triplet(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
! allocate(wm_interm_84_triplet(nocc+1: nactive, nocc+1: nactive))
! allocate(wm_interm_85_triplet(nocc+1: nactive, nocc+1: nactive))
! allocate(wm_interm_86_triplet(nocc+1: nactive, nocc+1: nactive))
! allocate(wm_interm_87_triplet(1: nocc, 1: nocc, 1: nocc, 1: nocc))
! allocate(wm_interm_88_triplet(1: nocc, 1: nocc, 1: nocc, 1: nocc))
! allocate(wm_interm_89_triplet(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
! allocate(wm_interm_90_triplet(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
! allocate(wm_interm_91_triplet(1: nocc, 1: nocc, 1: nocc, 1: nocc))
! allocate(wm_interm_92_triplet(nocc+1: nactive, nocc+1: nactive))
! allocate(wm_interm_93_triplet(nocc+1: nactive, nocc+1: nactive))
! allocate(wm_interm_94_triplet(nocc+1: nactive, nocc+1: nactive))
! allocate(wm_interm_95_triplet(nocc+1: nactive, nocc+1: nactive))
! allocate(wm_interm_96_triplet(nocc+1: nactive, nocc+1: nactive))
! allocate(wm_interm_97_triplet(nocc+1: nactive, nocc+1: nactive))
! wm_interm_0_triplet = zero 
! wm_interm_1_triplet = zero 
! wm_interm_2_triplet = zero 
! wm_interm_3_triplet = zero 
! wm_interm_4_triplet = zero 
! wm_interm_5_triplet = zero 
! wm_interm_6_triplet = zero 
! wm_interm_7_triplet = zero 
! wm_interm_8_triplet = zero 
! wm_interm_9_triplet = zero 
! wm_interm_10_triplet = zero 
! wm_interm_11_triplet = zero 
! wm_interm_12_triplet = zero 
! wm_interm_13_triplet = zero 
! wm_interm_14_triplet = zero 
! wm_interm_15_triplet = zero 
! wm_interm_16_triplet = zero 
! wm_interm_17_triplet = zero 
! wm_interm_18_triplet = zero 
! wm_interm_19_triplet = zero 
! wm_interm_20_triplet = zero 
! wm_interm_21_triplet = zero 
! wm_interm_22_triplet = zero 
! wm_interm_23_triplet = zero 
! wm_interm_24_triplet = zero 
! wm_interm_25_triplet = zero 
! wm_interm_26_triplet = zero 
! wm_interm_27_triplet = zero 
! wm_interm_28_triplet = zero 
! wm_interm_29_triplet = zero 
! wm_interm_30_triplet = zero 
! wm_interm_31_triplet = zero 
! wm_interm_32_triplet = zero 
! wm_interm_33_triplet = zero 
! wm_interm_34_triplet = zero 
! wm_interm_35_triplet = zero 
! wm_interm_36_triplet = zero 
! wm_interm_37_triplet = zero 
! wm_interm_38_triplet = zero 
! wm_interm_39_triplet = zero 
! wm_interm_40_triplet = zero 
! wm_interm_41_triplet = zero 
! wm_interm_42_triplet = zero 
! wm_interm_43_triplet = zero 
! wm_interm_44_triplet = zero 
! wm_interm_45_triplet = zero 
! wm_interm_46_triplet = zero 
! wm_interm_47_triplet = zero 
! wm_interm_48_triplet = zero 
! wm_interm_49_triplet = zero 
! wm_interm_50_triplet = zero 
! wm_interm_51_triplet = zero 
! wm_interm_52_triplet = zero 
! wm_interm_53_triplet = zero 
! wm_interm_54_triplet = zero 
! wm_interm_55_triplet = zero 
! wm_interm_56_triplet = zero 
! wm_interm_57_triplet = zero 
! wm_interm_58_triplet = zero 
! wm_interm_59_triplet = zero 
! wm_interm_60_triplet = zero 
! wm_interm_61_triplet = zero 
! wm_interm_62_triplet = zero 
! wm_interm_63_triplet = zero 
! wm_interm_64_triplet = zero 
! wm_interm_65_triplet = zero 
! wm_interm_66_triplet = zero 
! wm_interm_67_triplet = zero 
! wm_interm_68_triplet = zero 
! wm_interm_69_triplet = zero 
! wm_interm_70_triplet = zero 
! wm_interm_71_triplet = zero 
! wm_interm_72_triplet = zero 
! wm_interm_73_triplet = zero 
! wm_interm_74_triplet = zero 
! wm_interm_75_triplet = zero 
! wm_interm_76_triplet = zero 
! wm_interm_77_triplet = zero 
! wm_interm_78_triplet = zero 
! wm_interm_79_triplet = zero 
! wm_interm_80_triplet = zero 
! wm_interm_81_triplet = zero 
! wm_interm_82_triplet = zero 
! wm_interm_83_triplet = zero 
! wm_interm_84_triplet = zero 
! wm_interm_85_triplet = zero 
! wm_interm_86_triplet = zero 
! wm_interm_87_triplet = zero 
! wm_interm_88_triplet = zero 
! wm_interm_89_triplet = zero 
! wm_interm_90_triplet = zero 
! wm_interm_91_triplet = zero 
! wm_interm_92_triplet = zero 
! wm_interm_93_triplet = zero 
! wm_interm_94_triplet = zero 
! wm_interm_95_triplet = zero 
! wm_interm_96_triplet = zero 
! wm_interm_97_triplet = zero 

! end subroutine wm_triplet_intermediates_ccsd_init
    
!     subroutine wm_triplet_intermediates_ccsd_free
!     deallocate(wm_interm_0_triplet)
! deallocate(wm_interm_1_triplet)
! deallocate(wm_interm_2_triplet)
! deallocate(wm_interm_3_triplet)
! deallocate(wm_interm_4_triplet)
! deallocate(wm_interm_5_triplet)
! deallocate(wm_interm_6_triplet)
! deallocate(wm_interm_7_triplet)
! deallocate(wm_interm_8_triplet)
! deallocate(wm_interm_9_triplet)
! deallocate(wm_interm_10_triplet)
! deallocate(wm_interm_11_triplet)
! deallocate(wm_interm_12_triplet)
! deallocate(wm_interm_14_triplet)
! deallocate(wm_interm_15_triplet)
! deallocate(wm_interm_16_triplet)
! deallocate(wm_interm_17_triplet)
! deallocate(wm_interm_18_triplet)
! deallocate(wm_interm_19_triplet)
! deallocate(wm_interm_20_triplet)
! deallocate(wm_interm_21_triplet)
! deallocate(wm_interm_22_triplet)
! deallocate(wm_interm_23_triplet)
! deallocate(wm_interm_24_triplet)
! deallocate(wm_interm_25_triplet)
! deallocate(wm_interm_26_triplet)
! deallocate(wm_interm_27_triplet)
! deallocate(wm_interm_29_triplet)
! deallocate(wm_interm_30_triplet)
! deallocate(wm_interm_31_triplet)
! deallocate(wm_interm_34_triplet)
! deallocate(wm_interm_35_triplet)
! deallocate(wm_interm_36_triplet)
! deallocate(wm_interm_37_triplet)
! deallocate(wm_interm_38_triplet)
! deallocate(wm_interm_39_triplet)
! deallocate(wm_interm_40_triplet)
! deallocate(wm_interm_41_triplet)
! deallocate(wm_interm_43_triplet)
! deallocate(wm_interm_44_triplet)
! deallocate(wm_interm_45_triplet)
! deallocate(wm_interm_46_triplet)
! deallocate(wm_interm_49_triplet)
! deallocate(wm_interm_50_triplet)
! deallocate(wm_interm_51_triplet)
! deallocate(wm_interm_52_triplet)
! deallocate(wm_interm_53_triplet)
! deallocate(wm_interm_54_triplet)
! deallocate(wm_interm_55_triplet)
! deallocate(wm_interm_56_triplet)
! deallocate(wm_interm_57_triplet)
! deallocate(wm_interm_58_triplet)
! deallocate(wm_interm_59_triplet)
! deallocate(wm_interm_60_triplet)
! deallocate(wm_interm_61_triplet)
! deallocate(wm_interm_62_triplet)
! deallocate(wm_interm_63_triplet)
! deallocate(wm_interm_64_triplet)
! deallocate(wm_interm_65_triplet)
! deallocate(wm_interm_66_triplet)
! deallocate(wm_interm_67_triplet)
! deallocate(wm_interm_68_triplet)
! deallocate(wm_interm_69_triplet)
! deallocate(wm_interm_70_triplet)
! deallocate(wm_interm_71_triplet)
! deallocate(wm_interm_72_triplet)
! deallocate(wm_interm_73_triplet)
! deallocate(wm_interm_74_triplet)
! deallocate(wm_interm_75_triplet)
! deallocate(wm_interm_76_triplet)
! deallocate(wm_interm_77_triplet)
! deallocate(wm_interm_78_triplet)
! deallocate(wm_interm_79_triplet)
! deallocate(wm_interm_80_triplet)
! deallocate(wm_interm_81_triplet)
! deallocate(wm_interm_82_triplet)
! deallocate(wm_interm_83_triplet)
! deallocate(wm_interm_84_triplet)
! deallocate(wm_interm_85_triplet)
! deallocate(wm_interm_86_triplet)
! deallocate(wm_interm_87_triplet)
! deallocate(wm_interm_88_triplet)
! deallocate(wm_interm_89_triplet)
! deallocate(wm_interm_90_triplet)
! deallocate(wm_interm_91_triplet)
! deallocate(wm_interm_92_triplet)
! deallocate(wm_interm_93_triplet)
! deallocate(wm_interm_94_triplet)
! deallocate(wm_interm_95_triplet)
! deallocate(wm_interm_96_triplet)
! deallocate(wm_interm_97_triplet)

! end subroutine wm_triplet_intermediates_ccsd_free
    
!     subroutine wm_triplet_intermediates_ccsd(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr)
!     integer, intent(in) :: nocc, nactive
!     real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
!     real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
!     real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
!     real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
!     double precision, dimension(:), intent(in) :: vrdav_Rl
!     double precision, dimension(:), intent(in) :: vrdav_Rr
!     real(F64) :: sum
!     integer :: a, i, b, j, c, k, l 

!     !$omp parallel private(a, i, b, j, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do b = nocc + 1, nactive 
! do j = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r2p(vrdav_Rr, b,j,a,i) * s1(a,i)
! end do 
! end do 
! wm_interm_0_triplet(b, j) = wm_interm_0_triplet(b, j) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, b, j, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do b = nocc + 1, nactive 
! do j = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r2m(vrdav_Rr, a,i,b,j) * s1(a,i)
! end do 
! end do 
! wm_interm_1_triplet(b, j) = wm_interm_1_triplet(b, j) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, b, j, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do b = nocc + 1, nactive 
! do j = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r2m(vrdav_Rr, a,j,b,i) * s1(a,i)
! end do 
! end do 
! wm_interm_2_triplet(b, j) = wm_interm_2_triplet(b, j) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, b, j, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do b = nocc + 1, nactive 
! do j = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, b,j,a,i)
! end do 
! end do 
! wm_interm_3_triplet(b, j) = wm_interm_3_triplet(b, j) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, b, j, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do b = nocc + 1, nactive 
! do j = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, a,j,b,i)
! end do 
! end do 
! wm_interm_4_triplet(b, j) = wm_interm_4_triplet(b, j) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, b, j, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do b = nocc + 1, nactive 
! do j = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, a,i,b,j)
! end do 
! end do 
! wm_interm_5_triplet(b, j) = wm_interm_5_triplet(b, j) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, b, j, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do b = nocc + 1, nactive 
! do j = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r2p(vrdav_Rl, b,j,a,i) * t1(a,i)
! end do 
! end do 
! wm_interm_6_triplet(b, j) = wm_interm_6_triplet(b, j) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, b, j, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do b = nocc + 1, nactive 
! do j = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r2p(vrdav_Rl, b,j,a,i) * r1(vrdav_Rr, a,i)
! end do 
! end do 
! wm_interm_7_triplet(b, j) = wm_interm_7_triplet(b, j) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, b, j, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do b = nocc + 1, nactive 
! do j = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r2m(vrdav_Rl, a,i,b,j) * t1(a,i)
! end do 
! end do 
! wm_interm_8_triplet(b, j) = wm_interm_8_triplet(b, j) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, b, j, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do b = nocc + 1, nactive 
! do j = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r2m(vrdav_Rl, a,j,b,i) * t1(a,i)
! end do 
! end do 
! wm_interm_9_triplet(b, j) = wm_interm_9_triplet(b, j) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, b, j, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do b = nocc + 1, nactive 
! do j = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r2m(vrdav_Rl, a,j,b,i) * r1(vrdav_Rr, a,i)
! end do 
! end do 
! wm_interm_10_triplet(b, j) = wm_interm_10_triplet(b, j) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, b, j, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do b = nocc + 1, nactive 
! do j = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r2m(vrdav_Rl, a,i,b,j) * r1(vrdav_Rr, a,i)
! end do 
! end do 
! wm_interm_11_triplet(b, j) = wm_interm_11_triplet(b, j) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(i, a, b, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do a = nocc + 1, nactive 
! do b = nocc + 1, nactive 
! sum = zero 
! do i = 1, nocc 
! sum = sum + r1(vrdav_Rr, a,i) * s1(b,i)
! end do 
! wm_interm_12_triplet(a, b) = wm_interm_12_triplet(a, b) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! sum = zero 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r1(vrdav_Rl, a,i) * r1(vrdav_Rr, a,i)
! end do 
! end do 
! wm_interm_13_triplet = wm_interm_13_triplet + sum 

! !$omp parallel private(a, i, j, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do i = 1, nocc 
! do j = 1, nocc 
! sum = zero 
! do a = nocc + 1, nactive 
! sum = sum + r1(vrdav_Rr, a,i) * s1(a,j)
! end do 
! wm_interm_14_triplet(i, j) = wm_interm_14_triplet(i, j) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, j, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do i = 1, nocc 
! do j = 1, nocc 
! sum = zero 
! do a = nocc + 1, nactive 
! sum = sum + r1(vrdav_Rl, a,i) * t1(a,j)
! end do 
! wm_interm_15_triplet(i, j) = wm_interm_15_triplet(i, j) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(i, a, b, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do a = nocc + 1, nactive 
! do b = nocc + 1, nactive 
! sum = zero 
! do i = 1, nocc 
! sum = sum + r1(vrdav_Rl, a,i) * t1(b,i)
! end do 
! wm_interm_16_triplet(a, b) = wm_interm_16_triplet(a, b) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, j, b, c, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do b = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! sum = zero 
! do j = 1, nocc 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r2p(vrdav_Rr, b,i,a,j) * s2(a,c,j,i)
! end do 
! end do 
! end do 
! wm_interm_17_triplet(b, c) = wm_interm_17_triplet(b, c) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do j = 1, nocc 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + r2p(vrdav_Rr, a,j,b,i) * s2(a,b,i,k)
! end do 
! end do 
! end do 
! wm_interm_18_triplet(j, k) = wm_interm_18_triplet(j, k) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do j = 1, nocc 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + r2p(vrdav_Rr, a,j,b,i) * s2(a,b,k,i)
! end do 
! end do 
! end do 
! wm_interm_19_triplet(j, k) = wm_interm_19_triplet(j, k) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, j, b, c, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do b = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! sum = zero 
! do j = 1, nocc 
! do a = nocc + 1, nactive 
! do i = 1, nocc 
! sum = sum + r2p(vrdav_Rr, b,i,a,j) * s2(a,c,i,j)
! end do 
! end do 
! end do 
! wm_interm_20_triplet(b, c) = wm_interm_20_triplet(b, c) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, j, b, c, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do b = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! sum = zero 
! do j = 1, nocc 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r2m(vrdav_Rr, a,j,b,i) * s2(a,c,i,j)
! end do 
! end do 
! end do 
! wm_interm_21_triplet(b, c) = wm_interm_21_triplet(b, c) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, j, b, c, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do b = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! sum = zero 
! do i = 1, nocc 
! do j = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r2m(vrdav_Rr, a,j,b,i) * s2(a,c,j,i)
! end do 
! end do 
! end do 
! wm_interm_22_triplet(b, c) = wm_interm_22_triplet(b, c) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do j = 1, nocc 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + r2m(vrdav_Rr, a,j,b,i) * s2(a,b,i,k)
! end do 
! end do 
! end do 
! wm_interm_23_triplet(j, k) = wm_interm_23_triplet(j, k) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do j = 1, nocc 
! do k = 1, nocc 
! sum = zero 
! do b = nocc + 1, nactive 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r2m(vrdav_Rr, a,i,b,j) * s2(a,b,i,k)
! end do 
! end do 
! end do 
! wm_interm_24_triplet(j, k) = wm_interm_24_triplet(j, k) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do j = 1, nocc 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + r2m(vrdav_Rr, a,i,b,j) * s2(a,b,k,i)
! end do 
! end do 
! end do 
! wm_interm_25_triplet(j, k) = wm_interm_25_triplet(j, k) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do j = 1, nocc 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + r2m(vrdav_Rr, a,j,b,i) * s2(a,b,k,i)
! end do 
! end do 
! end do 
! wm_interm_26_triplet(j, k) = wm_interm_26_triplet(j, k) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do j = 1, nocc 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + r2p(vrdav_Rl, a,j,b,i) * r2p(vrdav_Rr, a,k,b,i)
! end do 
! end do 
! end do 
! wm_interm_27_triplet(j, k) = wm_interm_27_triplet(j, k) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! sum = zero 
! do j = 1, nocc 
! do b = nocc + 1, nactive 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r2p(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, a,i,b,j)
! end do 
! end do 
! end do 
! end do 
! wm_interm_28_triplet = wm_interm_28_triplet + sum 

! !$omp parallel private(a, b, i, j, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do b = nocc + 1, nactive 
! do i = 1, nocc 
! do j = 1, nocc 
! do k = 1, nocc 
! sum = zero 
! do a = nocc + 1, nactive 
! sum = sum + r2p(vrdav_Rr, b,i,a,j) * s1(a,k)
! end do 
! wm_interm_29_triplet(b, i, j, k) = wm_interm_29_triplet(b, i, j, k) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do j = 1, nocc 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + r2p(vrdav_Rl, a,j,b,i) * r2m(vrdav_Rr, a,i,b,k)
! end do 
! end do 
! end do 
! wm_interm_30_triplet(j, k) = wm_interm_30_triplet(j, k) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do j = 1, nocc 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + r2p(vrdav_Rl, a,j,b,i) * r2m(vrdav_Rr, a,k,b,i)
! end do 
! end do 
! end do 
! wm_interm_31_triplet(j, k) = wm_interm_31_triplet(j, k) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! sum = zero 
! do j = 1, nocc 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + r2p(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,j,b,i)
! end do 
! end do 
! end do 
! end do 
! wm_interm_32_triplet = wm_interm_32_triplet + sum 

! sum = zero 
! do j = 1, nocc 
! do b = nocc + 1, nactive 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r2p(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,i,b,j)
! end do 
! end do 
! end do 
! end do 
! wm_interm_33_triplet = wm_interm_33_triplet + sum 

! !$omp parallel private(a, b, j, i, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do b = nocc + 1, nactive 
! do j = 1, nocc 
! do i = 1, nocc 
! do k = 1, nocc 
! sum = zero 
! do a = nocc + 1, nactive 
! sum = sum + r2m(vrdav_Rr, a,j,b,i) * s1(a,k)
! end do 
! wm_interm_34_triplet(b, j, i, k) = wm_interm_34_triplet(b, j, i, k) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do b = nocc + 1, nactive 
! do i = 1, nocc 
! do j = 1, nocc 
! do k = 1, nocc 
! sum = zero 
! do a = nocc + 1, nactive 
! sum = sum + r2p(vrdav_Rl, b,i,a,j) * t1(a,k)
! end do 
! wm_interm_35_triplet(b, i, j, k) = wm_interm_35_triplet(b, i, j, k) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do j = 1, nocc 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + r2p(vrdav_Rl, b,j,a,i) * t2(a,b,k,i)
! end do 
! end do 
! end do 
! wm_interm_36_triplet(j, k) = wm_interm_36_triplet(j, k) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do j = 1, nocc 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + r2p(vrdav_Rl, a,j,b,i) * t2(a,b,k,i)
! end do 
! end do 
! end do 
! wm_interm_37_triplet(j, k) = wm_interm_37_triplet(j, k) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, j, b, c, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do b = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! sum = zero 
! do j = 1, nocc 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r2p(vrdav_Rl, b,i,a,j) * t2(a,c,j,i)
! end do 
! end do 
! end do 
! wm_interm_38_triplet(b, c) = wm_interm_38_triplet(b, c) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, j, b, c, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do b = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! sum = zero 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! do j = 1, nocc 
! sum = sum + r2p(vrdav_Rl, b,j,a,i) * t2(a,c,j,i)
! end do 
! end do 
! end do 
! wm_interm_39_triplet(b, c) = wm_interm_39_triplet(b, c) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do j = 1, nocc 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + r2m(vrdav_Rl, a,j,b,i) * r2p(vrdav_Rr, a,k,b,i)
! end do 
! end do 
! end do 
! wm_interm_40_triplet(j, k) = wm_interm_40_triplet(j, k) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do j = 1, nocc 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, a,k,b,i)
! end do 
! end do 
! end do 
! wm_interm_41_triplet(j, k) = wm_interm_41_triplet(j, k) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! sum = zero 
! do j = 1, nocc 
! do b = nocc + 1, nactive 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, a,i,b,j)
! end do 
! end do 
! end do 
! end do 
! wm_interm_42_triplet = wm_interm_42_triplet + sum 

! !$omp parallel private(a, b, i, j, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do j = 1, nocc 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + r2m(vrdav_Rl, a,j,b,i) * r2m(vrdav_Rr, a,i,b,k)
! end do 
! end do 
! end do 
! wm_interm_43_triplet(j, k) = wm_interm_43_triplet(j, k) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do j = 1, nocc 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + r2m(vrdav_Rl, a,j,b,i) * r2m(vrdav_Rr, a,k,b,i)
! end do 
! end do 
! end do 
! wm_interm_44_triplet(j, k) = wm_interm_44_triplet(j, k) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do j = 1, nocc 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,k,b,i)
! end do 
! end do 
! end do 
! wm_interm_45_triplet(j, k) = wm_interm_45_triplet(j, k) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do j = 1, nocc 
! do k = 1, nocc 
! sum = zero 
! do b = nocc + 1, nactive 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,i,b,k)
! end do 
! end do 
! end do 
! wm_interm_46_triplet(j, k) = wm_interm_46_triplet(j, k) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! sum = zero 
! do j = 1, nocc 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,j,b,i)
! end do 
! end do 
! end do 
! end do 
! wm_interm_47_triplet = wm_interm_47_triplet + sum 

! sum = zero 
! do j = 1, nocc 
! do b = nocc + 1, nactive 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,i,b,j)
! end do 
! end do 
! end do 
! end do 
! wm_interm_48_triplet = wm_interm_48_triplet + sum 

! !$omp parallel private(a, b, i, j, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do b = nocc + 1, nactive 
! do i = 1, nocc 
! do j = 1, nocc 
! do k = 1, nocc 
! sum = zero 
! do a = nocc + 1, nactive 
! sum = sum + r2m(vrdav_Rl, a,i,b,j) * t1(a,k)
! end do 
! wm_interm_49_triplet(b, i, j, k) = wm_interm_49_triplet(b, i, j, k) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, j, i, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do b = nocc + 1, nactive 
! do j = 1, nocc 
! do i = 1, nocc 
! do k = 1, nocc 
! sum = zero 
! do a = nocc + 1, nactive 
! sum = sum + r2m(vrdav_Rl, a,j,b,i) * t1(a,k)
! end do 
! wm_interm_50_triplet(b, j, i, k) = wm_interm_50_triplet(b, j, i, k) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do j = 1, nocc 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + r2m(vrdav_Rl, a,i,b,j) * t2(a,b,k,i)
! end do 
! end do 
! end do 
! wm_interm_51_triplet(j, k) = wm_interm_51_triplet(j, k) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do j = 1, nocc 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + r2m(vrdav_Rl, a,j,b,i) * t2(a,b,k,i)
! end do 
! end do 
! end do 
! wm_interm_52_triplet(j, k) = wm_interm_52_triplet(j, k) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, j, b, c, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do b = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! sum = zero 
! do j = 1, nocc 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r2m(vrdav_Rl, a,i,b,j) * t2(a,c,j,i)
! end do 
! end do 
! end do 
! wm_interm_53_triplet(b, c) = wm_interm_53_triplet(b, c) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, j, b, c, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do b = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! sum = zero 
! do i = 1, nocc 
! do j = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r2m(vrdav_Rl, a,j,b,i) * t2(a,c,j,i)
! end do 
! end do 
! end do 
! wm_interm_54_triplet(b, c) = wm_interm_54_triplet(b, c) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, j, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do i = 1, nocc 
! do j = 1, nocc 
! sum = zero 
! do a = nocc + 1, nactive 
! sum = sum + r1(vrdav_Rl, a,i) * r1(vrdav_Rr, a,j)
! end do 
! wm_interm_55_triplet(i, j) = wm_interm_55_triplet(i, j) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, j, i, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do b = nocc + 1, nactive 
! do j = 1, nocc 
! do i = 1, nocc 
! do k = 1, nocc 
! sum = zero 
! do a = nocc + 1, nactive 
! sum = sum + s2(a,b,j,i) * t1(a,k)
! end do 
! wm_interm_56_triplet(b, j, i, k) = wm_interm_56_triplet(b, j, i, k) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, b, j, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do b = nocc + 1, nactive 
! do j = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r1(vrdav_Rr, a,i) * s2(a,b,j,i)
! end do 
! end do 
! wm_interm_57_triplet(b, j) = wm_interm_57_triplet(b, j) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, b, j, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do b = nocc + 1, nactive 
! do j = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r1(vrdav_Rr, a,i) * s2(a,b,i,j)
! end do 
! end do 
! wm_interm_58_triplet(b, j) = wm_interm_58_triplet(b, j) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do b = nocc + 1, nactive 
! do i = 1, nocc 
! do j = 1, nocc 
! do k = 1, nocc 
! sum = zero 
! do a = nocc + 1, nactive 
! sum = sum + r1(vrdav_Rr, a,i) * s2(a,b,j,k)
! end do 
! wm_interm_59_triplet(b, i, j, k) = wm_interm_59_triplet(b, i, j, k) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do b = nocc + 1, nactive 
! do i = 1, nocc 
! do j = 1, nocc 
! do k = 1, nocc 
! sum = zero 
! do a = nocc + 1, nactive 
! sum = sum + s1(a,i) * t2(a,b,j,k)
! end do 
! wm_interm_60_triplet(b, i, j, k) = wm_interm_60_triplet(b, i, j, k) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, b, j, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do b = nocc + 1, nactive 
! do j = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + s1(a,i) * t2(a,b,j,i)
! end do 
! end do 
! wm_interm_61_triplet(b, j) = wm_interm_61_triplet(b, j) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(i, a, b, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do a = nocc + 1, nactive 
! do b = nocc + 1, nactive 
! sum = zero 
! do i = 1, nocc 
! sum = sum + r1(vrdav_Rl, a,i) * r1(vrdav_Rr, b,i)
! end do 
! wm_interm_62_triplet(a, b) = wm_interm_62_triplet(a, b) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, b, j, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do b = nocc + 1, nactive 
! do j = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r1(vrdav_Rl, a,i) * t2(a,b,i,j)
! end do 
! end do 
! wm_interm_63_triplet(b, j) = wm_interm_63_triplet(b, j) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, b, j, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do b = nocc + 1, nactive 
! do j = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r1(vrdav_Rl, a,i) * t2(a,b,j,i)
! end do 
! end do 
! wm_interm_64_triplet(b, j) = wm_interm_64_triplet(b, j) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, b, j, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do b = nocc + 1, nactive 
! do j = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + s1(a,i) * t2(a,b,i,j)
! end do 
! end do 
! wm_interm_65_triplet(b, j) = wm_interm_65_triplet(b, j) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do b = nocc + 1, nactive 
! do i = 1, nocc 
! do j = 1, nocc 
! do k = 1, nocc 
! sum = zero 
! do a = nocc + 1, nactive 
! sum = sum + r1(vrdav_Rl, a,i) * t2(a,b,j,k)
! end do 
! wm_interm_66_triplet(b, i, j, k) = wm_interm_66_triplet(b, i, j, k) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do b = nocc + 1, nactive 
! do i = 1, nocc 
! do j = 1, nocc 
! do k = 1, nocc 
! sum = zero 
! do a = nocc + 1, nactive 
! sum = sum + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, b,j,a,k)
! end do 
! wm_interm_67_triplet(b, i, j, k) = wm_interm_67_triplet(b, i, j, k) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do j = 1, nocc 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + s2(a,b,i,j) * t2(a,b,k,i)
! end do 
! end do 
! end do 
! wm_interm_68_triplet(j, k) = wm_interm_68_triplet(j, k) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do j = 1, nocc 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + s2(a,b,j,i) * t2(a,b,k,i)
! end do 
! end do 
! end do 
! wm_interm_69_triplet(j, k) = wm_interm_69_triplet(j, k) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, j, b, c, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do b = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! sum = zero 
! do i = 1, nocc 
! do j = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + s2(a,b,j,i) * t2(a,c,j,i)
! end do 
! end do 
! end do 
! wm_interm_70_triplet(b, c) = wm_interm_70_triplet(b, c) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, j, b, c, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do b = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! sum = zero 
! do j = 1, nocc 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + s2(a,b,i,j) * t2(a,c,j,i)
! end do 
! end do 
! end do 
! wm_interm_71_triplet(b, c) = wm_interm_71_triplet(b, c) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do b = nocc + 1, nactive 
! do i = 1, nocc 
! do j = 1, nocc 
! do k = 1, nocc 
! sum = zero 
! do a = nocc + 1, nactive 
! sum = sum + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, a,j,b,k)
! end do 
! wm_interm_72_triplet(b, i, j, k) = wm_interm_72_triplet(b, i, j, k) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, k, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do i = 1, nocc 
! do j = 1, nocc 
! do k = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + s2(a,b,i,j) * t2(a,b,k,l)
! end do 
! end do 
! wm_interm_73_triplet(i, j, k, l) = wm_interm_73_triplet(i, j, k, l) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, k, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do i = 1, nocc 
! do j = 1, nocc 
! do k = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + r2p(vrdav_Rr, a,i,b,j) * s2(a,b,k,l)
! end do 
! end do 
! wm_interm_74_triplet(i, j, k, l) = wm_interm_74_triplet(i, j, k, l) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, j, b, c, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do b = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! sum = zero 
! do j = 1, nocc 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r2m(vrdav_Rr, a,i,b,j) * s2(a,c,j,i)
! end do 
! end do 
! end do 
! wm_interm_75_triplet(b, c) = wm_interm_75_triplet(b, c) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, j, b, c, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do b = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! sum = zero 
! do j = 1, nocc 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r2m(vrdav_Rr, a,i,b,j) * s2(a,c,i,j)
! end do 
! end do 
! end do 
! wm_interm_76_triplet(b, c) = wm_interm_76_triplet(b, c) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, k, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do i = 1, nocc 
! do j = 1, nocc 
! do k = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + r2m(vrdav_Rr, a,i,b,j) * s2(a,b,k,l)
! end do 
! end do 
! wm_interm_77_triplet(i, j, k, l) = wm_interm_77_triplet(i, j, k, l) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, k, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do i = 1, nocc 
! do j = 1, nocc 
! do k = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + r2p(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, a,k,b,l)
! end do 
! end do 
! wm_interm_78_triplet(i, j, k, l) = wm_interm_78_triplet(i, j, k, l) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, k, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do i = 1, nocc 
! do j = 1, nocc 
! do k = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + r2p(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,k,b,l)
! end do 
! end do 
! wm_interm_79_triplet(i, j, k, l) = wm_interm_79_triplet(i, j, k, l) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do b = nocc + 1, nactive 
! do i = 1, nocc 
! do j = 1, nocc 
! do k = 1, nocc 
! sum = zero 
! do a = nocc + 1, nactive 
! sum = sum + r2p(vrdav_Rl, b,i,a,j) * r1(vrdav_Rr, a,k)
! end do 
! wm_interm_80_triplet(b, i, j, k) = wm_interm_80_triplet(b, i, j, k) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, k, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do i = 1, nocc 
! do j = 1, nocc 
! do k = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + r2p(vrdav_Rl, a,i,b,j) * t2(a,b,k,l)
! end do 
! end do 
! wm_interm_81_triplet(i, j, k, l) = wm_interm_81_triplet(i, j, k, l) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, k, j, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do b = nocc + 1, nactive 
! do i = 1, nocc 
! do k = 1, nocc 
! do j = 1, nocc 
! sum = zero 
! do a = nocc + 1, nactive 
! sum = sum + s1(a,i) * t2(a,b,k,j)
! end do 
! wm_interm_82_triplet(b, i, k, j) = wm_interm_82_triplet(b, i, k, j) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do b = nocc + 1, nactive 
! do i = 1, nocc 
! do j = 1, nocc 
! do k = 1, nocc 
! sum = zero 
! do a = nocc + 1, nactive 
! sum = sum + r2m(vrdav_Rr, a,i,b,j) * s1(a,k)
! end do 
! wm_interm_83_triplet(b, i, j, k) = wm_interm_83_triplet(b, i, j, k) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, j, b, c, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do b = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! sum = zero 
! do j = 1, nocc 
! do a = nocc + 1, nactive 
! do i = 1, nocc 
! sum = sum + r2p(vrdav_Rl, b,i,a,j) * r2p(vrdav_Rr, c,i,a,j)
! end do 
! end do 
! end do 
! wm_interm_84_triplet(b, c) = wm_interm_84_triplet(b, c) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, j, b, c, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do b = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! sum = zero 
! do j = 1, nocc 
! do a = nocc + 1, nactive 
! do i = 1, nocc 
! sum = sum + r2p(vrdav_Rl, b,i,a,j) * r2m(vrdav_Rr, a,i,c,j)
! end do 
! end do 
! end do 
! wm_interm_85_triplet(b, c) = wm_interm_85_triplet(b, c) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, j, b, c, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do b = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! sum = zero 
! do j = 1, nocc 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r2p(vrdav_Rl, b,i,a,j) * r2m(vrdav_Rr, a,j,c,i)
! end do 
! end do 
! end do 
! wm_interm_86_triplet(b, c) = wm_interm_86_triplet(b, c) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, k, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do i = 1, nocc 
! do j = 1, nocc 
! do k = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, a,k,b,l)
! end do 
! end do 
! wm_interm_87_triplet(i, j, k, l) = wm_interm_87_triplet(i, j, k, l) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, k, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do i = 1, nocc 
! do j = 1, nocc 
! do k = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,k,b,l)
! end do 
! end do 
! wm_interm_88_triplet(i, j, k, l) = wm_interm_88_triplet(i, j, k, l) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, j, i, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do b = nocc + 1, nactive 
! do j = 1, nocc 
! do i = 1, nocc 
! do k = 1, nocc 
! sum = zero 
! do a = nocc + 1, nactive 
! sum = sum + r2m(vrdav_Rl, a,j,b,i) * r1(vrdav_Rr, a,k)
! end do 
! wm_interm_89_triplet(b, j, i, k) = wm_interm_89_triplet(b, j, i, k) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do b = nocc + 1, nactive 
! do i = 1, nocc 
! do j = 1, nocc 
! do k = 1, nocc 
! sum = zero 
! do a = nocc + 1, nactive 
! sum = sum + r2m(vrdav_Rl, a,i,b,j) * r1(vrdav_Rr, a,k)
! end do 
! wm_interm_90_triplet(b, i, j, k) = wm_interm_90_triplet(b, i, j, k) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, k, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do i = 1, nocc 
! do j = 1, nocc 
! do k = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + r2m(vrdav_Rl, a,i,b,j) * t2(a,b,k,l)
! end do 
! end do 
! wm_interm_91_triplet(i, j, k, l) = wm_interm_91_triplet(i, j, k, l) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, j, b, c, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do b = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! sum = zero 
! do j = 1, nocc 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r2m(vrdav_Rl, a,j,b,i) * r2p(vrdav_Rr, c,i,a,j)
! end do 
! end do 
! end do 
! wm_interm_92_triplet(b, c) = wm_interm_92_triplet(b, c) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, j, b, c, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do b = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! sum = zero 
! do j = 1, nocc 
! do a = nocc + 1, nactive 
! do i = 1, nocc 
! sum = sum + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, c,i,a,j)
! end do 
! end do 
! end do 
! wm_interm_93_triplet(b, c) = wm_interm_93_triplet(b, c) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, j, b, c, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do b = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! sum = zero 
! do j = 1, nocc 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r2m(vrdav_Rl, a,j,b,i) * r2m(vrdav_Rr, a,i,c,j)
! end do 
! end do 
! end do 
! wm_interm_94_triplet(b, c) = wm_interm_94_triplet(b, c) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, j, b, c, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do b = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! sum = zero 
! do i = 1, nocc 
! do j = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r2m(vrdav_Rl, a,j,b,i) * r2m(vrdav_Rr, a,j,c,i)
! end do 
! end do 
! end do 
! wm_interm_95_triplet(b, c) = wm_interm_95_triplet(b, c) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, j, b, c, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do b = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! sum = zero 
! do j = 1, nocc 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,j,c,i)
! end do 
! end do 
! end do 
! wm_interm_96_triplet(b, c) = wm_interm_96_triplet(b, c) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, j, b, c, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do b = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! sum = zero 
! do j = 1, nocc 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,i,c,j)
! end do 
! end do 
! end do 
! wm_interm_97_triplet(b, c) = wm_interm_97_triplet(b, c) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 



!     end subroutine wm_triplet_intermediates_ccsd
    
!     function calc_D_oo_wm_triplet(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
!     real(F64) :: calc_D_oo_wm_triplet
!     integer, intent(in) :: nocc, nactive
!     real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
!     real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
!     real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
!     real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
!     double precision, dimension(:), intent(in) :: vrdav_Rl
!     double precision, dimension(:), intent(in) :: vrdav_Rr
!     integer, intent(in) :: k1, k2
!     integer, intent(in) :: p, q
!     integer :: s , j, i, a, b 
!     real(F64), dimension(0:178) :: term ;
!     term = 0.d+0 

!     term = 0.d+0 
!     do j = 1, nocc 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! term(0) = term(0) + wm_interm_56_triplet(a, q, i, j) * wm_interm_67_triplet(a, j, p, i)
! term(1) = term(1) + wm_interm_56_triplet(a, i, q, j) * wm_interm_67_triplet(a, j, p, i)
! term(2) = term(2) + wm_interm_56_triplet(a, q, i, j) * wm_interm_72_triplet(a, j, p, i)
! term(3) = term(3) + wm_interm_56_triplet(a, q, i, j) * wm_interm_72_triplet(a, j, i, p)
! term(4) = term(4) + wm_interm_56_triplet(a, i, q, j) * wm_interm_72_triplet(a, j, i, p)
! term(5) = term(5) + wm_interm_56_triplet(a, i, q, j) * wm_interm_72_triplet(a, j, p, i)
! term(6) = term(6) + wm_interm_59_triplet(a, p, i, j) * wm_interm_66_triplet(a, q, i, j)
! term(7) = term(7) + wm_interm_59_triplet(a, p, i, j) * wm_interm_66_triplet(a, q, j, i)
! term(8) = term(8) + wm_interm_56_triplet(a, i, j, q) * wm_interm_67_triplet(a, p, i, j)
! term(9) = term(9) + wm_interm_56_triplet(a, i, j, q) * wm_interm_72_triplet(a, p, i, j)
! term(10) = term(10) + wm_interm_59_triplet(a, i, j, p) * wm_interm_66_triplet(a, i, j, q)
! term(11) = term(11) + wm_interm_59_triplet(a, i, p, j) * wm_interm_66_triplet(a, i, q, j)
! term(12) = term(12) + wm_interm_59_triplet(a, i, j, p) * wm_interm_66_triplet(a, i, q, j)
! term(13) = term(13) + wm_interm_59_triplet(a, i, p, j) * wm_interm_66_triplet(a, i, j, q)
! term(14) = term(14) + wm_interm_80_triplet(a, i, j, p) * wm_interm_82_triplet(a, q, i, j)
! term(15) = term(15) + wm_interm_80_triplet(a, i, p, j) * wm_interm_82_triplet(a, j, i, q)
! term(16) = term(16) + wm_interm_80_triplet(a, i, p, j) * wm_interm_82_triplet(a, j, q, i)
! term(17) = term(17) + wm_interm_82_triplet(a, q, i, j) * wm_interm_89_triplet(a, j, i, p)
! term(18) = term(18) + wm_interm_82_triplet(a, q, i, j) * wm_interm_90_triplet(a, j, i, p)
! term(19) = term(19) + wm_interm_82_triplet(a, q, i, j) * wm_interm_90_triplet(a, i, j, p)
! term(20) = term(20) + wm_interm_82_triplet(a, q, i, j) * wm_interm_89_triplet(a, i, j, p)
! term(21) = term(21) + wm_interm_82_triplet(a, i, q, j) * wm_interm_89_triplet(a, j, p, i)
! term(22) = term(22) + wm_interm_82_triplet(a, i, q, j) * wm_interm_89_triplet(a, p, j, i)
! term(23) = term(23) + wm_interm_82_triplet(a, i, q, j) * wm_interm_90_triplet(a, p, j, i)
! term(24) = term(24) + wm_interm_82_triplet(a, i, q, j) * wm_interm_90_triplet(a, j, p, i)
! end do 
! end do 
! end do 

! term(0) = term(0) * (-6.0d+0) 
! term(1) = term(1) * 6.0d+0 
! term(2) = term(2) * (-4.0d+0) 
! term(3) = term(3) * 8.0d+0 
! term(4) = term(4) * (-4.0d+0) 
! term(5) = term(5) * 8.0d+0 
! term(6) = term(6) * 4.0d+0 
! term(7) = term(7) * (-2.0d+0) 
! term(8) = term(8) * (-3.0d+0) 
! term(9) = term(9) * (-4.0d+0) 
! term(10) = term(10) * 4.0d+0 
! term(11) = term(11) * 4.0d+0 
! term(12) = term(12) * (-2.0d+0) 
! term(13) = term(13) * (-2.0d+0) 
! term(14) = term(14) * (-3.0d+0) 
! term(15) = term(15) * (-6.0d+0) 
! term(16) = term(16) * 6.0d+0 
! term(17) = term(17) * (-2.0d+0) 
! term(18) = term(18) * 4.0d+0 
! term(19) = term(19) * (-2.0d+0) 
! term(20) = term(20) * 4.0d+0 
! term(21) = term(21) * (-2.0d+0) 
! term(22) = term(22) * 4.0d+0 
! term(23) = term(23) * (-2.0d+0) 
! term(24) = term(24) * 4.0d+0 

! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! term(25) = term(25) + r2p(vrdav_Rl, a,p,b,i) * r2p(vrdav_Rr, a,q,b,i)
! term(26) = term(26) + r2p(vrdav_Rl, a,p,b,i) * r2m(vrdav_Rr, a,i,b,q)
! term(27) = term(27) + r2p(vrdav_Rl, a,p,b,i) * r2m(vrdav_Rr, a,q,b,i)
! term(28) = term(28) + r2m(vrdav_Rl, a,p,b,i) * r2p(vrdav_Rr, a,q,b,i)
! term(29) = term(29) + r2m(vrdav_Rl, a,i,b,p) * r2p(vrdav_Rr, a,q,b,i)
! term(30) = term(30) + r2m(vrdav_Rl, a,p,b,i) * r2m(vrdav_Rr, a,i,b,q)
! term(31) = term(31) + r2m(vrdav_Rl, a,p,b,i) * r2m(vrdav_Rr, a,q,b,i)
! term(32) = term(32) + r2m(vrdav_Rl, a,i,b,p) * r2m(vrdav_Rr, a,q,b,i)
! end do 
! end do 
! end do 

! term(25) = term(25) * (-6.0d+0) 
! term(26) = term(26) * 6.0d+0 
! term(27) = term(27) * (-6.0d+0) 
! term(28) = term(28) * (-6.0d+0) 
! term(29) = term(29) * 6.0d+0 
! term(30) = term(30) * 4.0d+0 
! term(31) = term(31) * (-8.0d+0) 
! term(32) = term(32) * 4.0d+0 

! do i = 1, nocc 
! do j = 1, nocc 
! do a = nocc + 1, nactive 
! term(33) = term(33) + wm_interm_56_triplet(a, i, j, q) * wm_interm_67_triplet(a, p, j, i)
! term(34) = term(34) + wm_interm_56_triplet(a, i, j, q) * wm_interm_72_triplet(a, p, j, i)
! term(35) = term(35) + wm_interm_80_triplet(a, i, j, p) * wm_interm_82_triplet(a, q, j, i)
! term(36) = term(36) + wm_interm_82_triplet(a, i, j, q) * wm_interm_89_triplet(a, p, j, i)
! term(37) = term(37) + wm_interm_82_triplet(a, i, j, q) * wm_interm_90_triplet(a, p, j, i)
! term(38) = term(38) + wm_interm_82_triplet(a, i, j, q) * wm_interm_89_triplet(a, j, p, i)
! term(39) = term(39) + wm_interm_82_triplet(a, i, j, q) * wm_interm_90_triplet(a, j, p, i)
! end do 
! end do 
! end do 

! term(33) = term(33) * 3.0d+0 
! term(34) = term(34) * 8.0d+0 
! term(35) = term(35) * 3.0d+0 
! term(36) = term(36) * (-2.0d+0) 
! term(37) = term(37) * 4.0d+0 
! term(38) = term(38) * 4.0d+0 
! term(39) = term(39) * (-2.0d+0) 

! do a = nocc + 1, nactive 
! term(40) = term(40) + r1(vrdav_Rl, a,q) * wm_interm_0_triplet(a, p)
! term(41) = term(41) + r1(vrdav_Rl, a,q) * wm_interm_1_triplet(a, p)
! term(42) = term(42) + r1(vrdav_Rl, a,q) * wm_interm_2_triplet(a, p)
! term(43) = term(43) + s1(a,q) * wm_interm_3_triplet(a, p)
! term(44) = term(44) + s1(a,q) * wm_interm_4_triplet(a, p)
! term(45) = term(45) + s1(a,q) * wm_interm_5_triplet(a, p)
! term(46) = term(46) + r1(vrdav_Rr, a,p) * wm_interm_6_triplet(a, q)
! term(47) = term(47) + t1(a,q) * wm_interm_7_triplet(a, p)
! term(48) = term(48) + r1(vrdav_Rr, a,p) * wm_interm_8_triplet(a, q)
! term(49) = term(49) + r1(vrdav_Rr, a,p) * wm_interm_9_triplet(a, q)
! term(50) = term(50) + t1(a,q) * wm_interm_10_triplet(a, p)
! term(51) = term(51) + t1(a,q) * wm_interm_11_triplet(a, p)
! term(52) = term(52) + wm_interm_58_triplet(a, p) * wm_interm_63_triplet(a, q)
! term(53) = term(53) + wm_interm_57_triplet(a, p) * wm_interm_64_triplet(a, q)
! term(54) = term(54) + wm_interm_58_triplet(a, p) * wm_interm_64_triplet(a, q)
! term(55) = term(55) + wm_interm_57_triplet(a, p) * wm_interm_63_triplet(a, q)
! term(56) = term(56) + wm_interm_61_triplet(a, q) * wm_interm_7_triplet(a, p)
! term(57) = term(57) + wm_interm_65_triplet(a, q) * wm_interm_7_triplet(a, p)
! term(58) = term(58) + wm_interm_11_triplet(a, p) * wm_interm_61_triplet(a, q)
! term(59) = term(59) + wm_interm_10_triplet(a, p) * wm_interm_61_triplet(a, q)
! term(60) = term(60) + wm_interm_10_triplet(a, p) * wm_interm_65_triplet(a, q)
! term(61) = term(61) + wm_interm_11_triplet(a, p) * wm_interm_65_triplet(a, q)
! term(62) = term(62) + r1(vrdav_Rl, a,p) * r1(vrdav_Rr, a,q)
! end do 

! term(40) = term(40) * (-6.0d+0) 
! term(41) = term(41) * (-8.0d+0) 
! term(42) = term(42) * 4.0d+0 
! term(43) = term(43) * 6.0d+0 
! term(44) = term(44) * (-4.0d+0) 
! term(45) = term(45) * 8.0d+0 
! term(46) = term(46) * (-6.0d+0) 
! term(47) = term(47) * 6.0d+0 
! term(48) = term(48) * 8.0d+0 
! term(49) = term(49) * (-4.0d+0) 
! term(50) = term(50) * (-4.0d+0) 
! term(51) = term(51) * 8.0d+0 
! term(52) = term(52) * (-8.0d+0) 
! term(53) = term(53) * (-2.0d+0) 
! term(54) = term(54) * 4.0d+0 
! term(55) = term(55) * 4.0d+0 
! term(56) = term(56) * 6.0d+0 
! term(57) = term(57) * (-12.0d+0) 
! term(58) = term(58) * 8.0d+0 
! term(59) = term(59) * (-4.0d+0) 
! term(60) = term(60) * 8.0d+0 
! term(61) = term(61) * (-16.0d+0) 
! term(62) = term(62) * (-2.0d+0) 

! do i = 1, nocc 
! do j = 1, nocc 
! term(63) = term(63) + wm_interm_55_triplet(i, j) * wm_interm_73_triplet(j, p, i, q)
! term(64) = term(64) + wm_interm_55_triplet(i, j) * wm_interm_73_triplet(p, j, i, q)
! end do 
! end do 

! term(63) = term(63) * 4.0d+0 
! term(64) = term(64) * (-2.0d+0) 

! do j = 1, nocc 
! do i = 1, nocc 
! term(65) = term(65) + wm_interm_15_triplet(i, j) * wm_interm_74_triplet(i, p, q, j)
! term(66) = term(66) + wm_interm_15_triplet(i, j) * wm_interm_74_triplet(i, p, j, q)
! term(67) = term(67) + wm_interm_15_triplet(i, j) * wm_interm_18_triplet(i, j)
! term(68) = term(68) + wm_interm_15_triplet(i, j) * wm_interm_19_triplet(i, j)
! term(69) = term(69) + wm_interm_15_triplet(i, j) * wm_interm_77_triplet(i, p, q, j)
! term(70) = term(70) + wm_interm_15_triplet(i, j) * wm_interm_77_triplet(p, i, q, j)
! term(71) = term(71) + wm_interm_15_triplet(i, j) * wm_interm_77_triplet(p, i, j, q)
! term(72) = term(72) + wm_interm_15_triplet(i, j) * wm_interm_77_triplet(i, p, j, q)
! term(73) = term(73) + wm_interm_15_triplet(i, j) * wm_interm_23_triplet(i, j)
! term(74) = term(74) + wm_interm_15_triplet(i, j) * wm_interm_24_triplet(i, j)
! term(75) = term(75) + wm_interm_15_triplet(i, j) * wm_interm_25_triplet(i, j)
! term(76) = term(76) + wm_interm_15_triplet(i, j) * wm_interm_26_triplet(i, j)
! term(77) = term(77) + wm_interm_55_triplet(i, j) * wm_interm_68_triplet(j, i)
! term(78) = term(78) + wm_interm_55_triplet(i, j) * wm_interm_69_triplet(j, i)
! term(79) = term(79) + wm_interm_14_triplet(i, j) * wm_interm_37_triplet(i, j)
! term(80) = term(80) + wm_interm_14_triplet(i, j) * wm_interm_36_triplet(i, j)
! term(81) = term(81) + wm_interm_14_triplet(i, j) * wm_interm_81_triplet(i, p, q, j)
! term(82) = term(82) + wm_interm_14_triplet(i, j) * wm_interm_81_triplet(i, p, j, q)
! term(83) = term(83) + wm_interm_14_triplet(i, j) * wm_interm_51_triplet(i, j)
! term(84) = term(84) + wm_interm_14_triplet(i, j) * wm_interm_52_triplet(i, j)
! term(85) = term(85) + wm_interm_14_triplet(i, j) * wm_interm_91_triplet(i, p, q, j)
! term(86) = term(86) + wm_interm_14_triplet(i, j) * wm_interm_91_triplet(p, i, q, j)
! term(87) = term(87) + wm_interm_14_triplet(i, j) * wm_interm_91_triplet(p, i, j, q)
! term(88) = term(88) + wm_interm_14_triplet(i, j) * wm_interm_91_triplet(i, p, j, q)
! end do 
! end do 

! term(65) = term(65) * (-3.0d+0) 
! term(66) = term(66) * 3.0d+0 
! term(67) = term(67) * 6.0d+0 
! term(68) = term(68) * (-6.0d+0) 
! term(69) = term(69) * (-2.0d+0) 
! term(70) = term(70) * 4.0d+0 
! term(71) = term(71) * (-2.0d+0) 
! term(72) = term(72) * 4.0d+0 
! term(73) = term(73) * 4.0d+0 
! term(74) = term(74) * (-8.0d+0) 
! term(75) = term(75) * 4.0d+0 
! term(76) = term(76) * (-8.0d+0) 
! term(77) = term(77) * 4.0d+0 
! term(78) = term(78) * (-8.0d+0) 
! term(79) = term(79) * (-6.0d+0) 
! term(80) = term(80) * 6.0d+0 
! term(81) = term(81) * (-3.0d+0) 
! term(82) = term(82) * 3.0d+0 
! term(83) = term(83) * 8.0d+0 
! term(84) = term(84) * (-16.0d+0) 
! term(85) = term(85) * (-2.0d+0) 
! term(86) = term(86) * 4.0d+0 
! term(87) = term(87) * (-2.0d+0) 
! term(88) = term(88) * 4.0d+0 

! do b = nocc + 1, nactive 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! term(89) = term(89) + r2m(vrdav_Rl, a,i,b,p) * r2m(vrdav_Rr, a,i,b,q)
! end do 
! end do 
! end do 

! term(89) = term(89) * (-8.0d+0) 

! do i = 1, nocc 
! term(90) = term(90) + wm_interm_15_triplet(q, i) * wm_interm_18_triplet(p, i)
! term(91) = term(91) + wm_interm_15_triplet(q, i) * wm_interm_19_triplet(p, i)
! term(92) = term(92) + wm_interm_15_triplet(q, i) * wm_interm_23_triplet(p, i)
! term(93) = term(93) + wm_interm_15_triplet(q, i) * wm_interm_24_triplet(p, i)
! term(94) = term(94) + wm_interm_15_triplet(q, i) * wm_interm_25_triplet(p, i)
! term(95) = term(95) + wm_interm_15_triplet(q, i) * wm_interm_26_triplet(p, i)
! term(96) = term(96) + wm_interm_55_triplet(i, p) * wm_interm_68_triplet(q, i)
! term(97) = term(97) + wm_interm_55_triplet(i, p) * wm_interm_69_triplet(q, i)
! term(98) = term(98) + wm_interm_15_triplet(i, q) * wm_interm_18_triplet(i, p)
! term(99) = term(99) + wm_interm_15_triplet(i, q) * wm_interm_19_triplet(i, p)
! term(100) = term(100) + wm_interm_15_triplet(i, q) * wm_interm_23_triplet(i, p)
! term(101) = term(101) + wm_interm_15_triplet(i, q) * wm_interm_24_triplet(i, p)
! term(102) = term(102) + wm_interm_15_triplet(i, q) * wm_interm_25_triplet(i, p)
! term(103) = term(103) + wm_interm_15_triplet(i, q) * wm_interm_26_triplet(i, p)
! term(104) = term(104) + wm_interm_55_triplet(p, i) * wm_interm_68_triplet(i, q)
! term(105) = term(105) + wm_interm_55_triplet(p, i) * wm_interm_69_triplet(i, q)
! term(106) = term(106) + wm_interm_14_triplet(p, i) * wm_interm_37_triplet(q, i)
! term(107) = term(107) + wm_interm_14_triplet(p, i) * wm_interm_36_triplet(q, i)
! term(108) = term(108) + wm_interm_14_triplet(i, p) * wm_interm_36_triplet(i, q)
! term(109) = term(109) + wm_interm_14_triplet(i, p) * wm_interm_37_triplet(i, q)
! term(110) = term(110) + wm_interm_14_triplet(p, i) * wm_interm_51_triplet(q, i)
! term(111) = term(111) + wm_interm_14_triplet(p, i) * wm_interm_52_triplet(q, i)
! term(112) = term(112) + wm_interm_14_triplet(i, p) * wm_interm_51_triplet(i, q)
! term(113) = term(113) + wm_interm_14_triplet(i, p) * wm_interm_52_triplet(i, q)
! end do 

! term(90) = term(90) * (-3.0d+0) 
! term(91) = term(91) * 3.0d+0 
! term(92) = term(92) * (-2.0d+0) 
! term(93) = term(93) * 4.0d+0 
! term(94) = term(94) * (-2.0d+0) 
! term(95) = term(95) * 4.0d+0 
! term(96) = term(96) * (-2.0d+0) 
! term(97) = term(97) * 4.0d+0 
! term(98) = term(98) * (-3.0d+0) 
! term(99) = term(99) * 3.0d+0 
! term(100) = term(100) * (-2.0d+0) 
! term(101) = term(101) * 4.0d+0 
! term(102) = term(102) * (-2.0d+0) 
! term(103) = term(103) * 4.0d+0 
! term(104) = term(104) * (-2.0d+0) 
! term(105) = term(105) * 4.0d+0 
! term(106) = term(106) * (-3.0d+0) 
! term(107) = term(107) * 3.0d+0 
! term(108) = term(108) * (-3.0d+0) 
! term(109) = term(109) * 3.0d+0 
! term(110) = term(110) * (-4.0d+0) 
! term(111) = term(111) * 8.0d+0 
! term(112) = term(112) * (-4.0d+0) 
! term(113) = term(113) * 8.0d+0 

! do i = 1, nocc 
! do a = nocc + 1, nactive 
! term(114) = term(114) + r1(vrdav_Rl, a,i) * wm_interm_0_triplet(a, i)
! term(115) = term(115) + r1(vrdav_Rl, a,i) * wm_interm_1_triplet(a, i)
! term(116) = term(116) + r1(vrdav_Rl, a,i) * wm_interm_2_triplet(a, i)
! term(117) = term(117) + r1(vrdav_Rr, a,i) * wm_interm_6_triplet(a, i)
! term(118) = term(118) + r1(vrdav_Rr, a,i) * wm_interm_8_triplet(a, i)
! term(119) = term(119) + r1(vrdav_Rr, a,i) * wm_interm_9_triplet(a, i)
! term(120) = term(120) + wm_interm_59_triplet(a, p, i, q) * wm_interm_64_triplet(a, i)
! term(121) = term(121) + wm_interm_57_triplet(a, i) * wm_interm_64_triplet(a, i)
! term(122) = term(122) + wm_interm_59_triplet(a, p, q, i) * wm_interm_64_triplet(a, i)
! term(123) = term(123) + wm_interm_58_triplet(a, i) * wm_interm_64_triplet(a, i)
! term(124) = term(124) + wm_interm_59_triplet(a, p, q, i) * wm_interm_63_triplet(a, i)
! term(125) = term(125) + wm_interm_58_triplet(a, i) * wm_interm_63_triplet(a, i)
! term(126) = term(126) + wm_interm_59_triplet(a, p, i, q) * wm_interm_63_triplet(a, i)
! term(127) = term(127) + wm_interm_57_triplet(a, i) * wm_interm_63_triplet(a, i)
! term(128) = term(128) + wm_interm_3_triplet(a, i) * wm_interm_56_triplet(a, i, p, q)
! term(129) = term(129) + wm_interm_3_triplet(a, i) * wm_interm_56_triplet(a, p, i, q)
! term(130) = term(130) + wm_interm_4_triplet(a, i) * wm_interm_56_triplet(a, i, p, q)
! term(131) = term(131) + wm_interm_56_triplet(a, i, p, q) * wm_interm_5_triplet(a, i)
! term(132) = term(132) + wm_interm_4_triplet(a, i) * wm_interm_56_triplet(a, p, i, q)
! term(133) = term(133) + wm_interm_56_triplet(a, p, i, q) * wm_interm_5_triplet(a, i)
! term(134) = term(134) + wm_interm_57_triplet(a, i) * wm_interm_66_triplet(a, p, i, q)
! term(135) = term(135) + wm_interm_58_triplet(a, i) * wm_interm_66_triplet(a, p, i, q)
! term(136) = term(136) + wm_interm_58_triplet(a, i) * wm_interm_66_triplet(a, p, q, i)
! term(137) = term(137) + wm_interm_57_triplet(a, i) * wm_interm_66_triplet(a, p, q, i)
! term(138) = term(138) + wm_interm_61_triplet(a, i) * wm_interm_80_triplet(a, i, q, p)
! term(139) = term(139) + wm_interm_61_triplet(a, i) * wm_interm_7_triplet(a, i)
! term(140) = term(140) + wm_interm_65_triplet(a, i) * wm_interm_80_triplet(a, i, q, p)
! term(141) = term(141) + wm_interm_65_triplet(a, i) * wm_interm_7_triplet(a, i)
! term(142) = term(142) + wm_interm_7_triplet(a, i) * wm_interm_82_triplet(a, p, i, q)
! term(143) = term(143) + wm_interm_7_triplet(a, i) * wm_interm_82_triplet(a, p, q, i)
! term(144) = term(144) + wm_interm_61_triplet(a, i) * wm_interm_90_triplet(a, q, i, p)
! term(145) = term(145) + wm_interm_11_triplet(a, i) * wm_interm_61_triplet(a, i)
! term(146) = term(146) + wm_interm_65_triplet(a, i) * wm_interm_90_triplet(a, q, i, p)
! term(147) = term(147) + wm_interm_11_triplet(a, i) * wm_interm_65_triplet(a, i)
! term(148) = term(148) + wm_interm_61_triplet(a, i) * wm_interm_89_triplet(a, i, q, p)
! term(149) = term(149) + wm_interm_61_triplet(a, i) * wm_interm_90_triplet(a, i, q, p)
! term(150) = term(150) + wm_interm_10_triplet(a, i) * wm_interm_61_triplet(a, i)
! term(151) = term(151) + wm_interm_65_triplet(a, i) * wm_interm_90_triplet(a, i, q, p)
! term(152) = term(152) + wm_interm_10_triplet(a, i) * wm_interm_65_triplet(a, i)
! term(153) = term(153) + wm_interm_61_triplet(a, i) * wm_interm_89_triplet(a, q, i, p)
! term(154) = term(154) + wm_interm_65_triplet(a, i) * wm_interm_89_triplet(a, q, i, p)
! term(155) = term(155) + wm_interm_65_triplet(a, i) * wm_interm_89_triplet(a, i, q, p)
! term(156) = term(156) + wm_interm_10_triplet(a, i) * wm_interm_82_triplet(a, p, i, q)
! term(157) = term(157) + wm_interm_11_triplet(a, i) * wm_interm_82_triplet(a, p, i, q)
! term(158) = term(158) + wm_interm_10_triplet(a, i) * wm_interm_82_triplet(a, p, q, i)
! term(159) = term(159) + wm_interm_11_triplet(a, i) * wm_interm_82_triplet(a, p, q, i)
! end do 
! end do 

! term(114) = term(114) * 12.0d+0 
! term(115) = term(115) * 16.0d+0 
! term(116) = term(116) * (-8.0d+0) 
! term(117) = term(117) * (-12.0d+0) 
! term(118) = term(118) * (-16.0d+0) 
! term(119) = term(119) * 8.0d+0 
! term(120) = term(120) * (-2.0d+0) 
! term(121) = term(121) * 4.0d+0 
! term(122) = term(122) * 4.0d+0 
! term(123) = term(123) * (-8.0d+0) 
! term(124) = term(124) * (-8.0d+0) 
! term(125) = term(125) * 16.0d+0 
! term(126) = term(126) * 4.0d+0 
! term(127) = term(127) * (-8.0d+0) 
! term(128) = term(128) * (-12.0d+0) 
! term(129) = term(129) * 6.0d+0 
! term(130) = term(130) * 8.0d+0 
! term(131) = term(131) * (-16.0d+0) 
! term(132) = term(132) * (-4.0d+0) 
! term(133) = term(133) * 8.0d+0 
! term(134) = term(134) * (-2.0d+0) 
! term(135) = term(135) * 4.0d+0 
! term(136) = term(136) * (-8.0d+0) 
! term(137) = term(137) * 4.0d+0 
! term(138) = term(138) * (-6.0d+0) 
! term(139) = term(139) * (-12.0d+0) 
! term(140) = term(140) * 12.0d+0 
! term(141) = term(141) * 24.0d+0 
! term(142) = term(142) * (-12.0d+0) 
! term(143) = term(143) * 6.0d+0 
! term(144) = term(144) * 4.0d+0 
! term(145) = term(145) * (-16.0d+0) 
! term(146) = term(146) * (-8.0d+0) 
! term(147) = term(147) * 32.0d+0 
! term(148) = term(148) * 4.0d+0 
! term(149) = term(149) * (-2.0d+0) 
! term(150) = term(150) * 8.0d+0 
! term(151) = term(151) * 4.0d+0 
! term(152) = term(152) * (-16.0d+0) 
! term(153) = term(153) * (-2.0d+0) 
! term(154) = term(154) * 4.0d+0 
! term(155) = term(155) * (-8.0d+0) 
! term(156) = term(156) * 8.0d+0 
! term(157) = term(157) * (-16.0d+0) 
! term(158) = term(158) * (-4.0d+0) 
! term(159) = term(159) * 8.0d+0 

! term(160) = term(160) + wm_interm_28_triplet
! term(161) = term(161) + wm_interm_32_triplet
! term(162) = term(162) + wm_interm_33_triplet
! term(163) = term(163) + wm_interm_42_triplet
! term(164) = term(164) + wm_interm_47_triplet
! term(165) = term(165) + wm_interm_48_triplet
! term(166) = term(166) + wm_interm_13_triplet
! term(167) = term(167) + wm_interm_13_triplet * wm_interm_69_triplet(p, q)
! term(168) = term(168) + wm_interm_13_triplet * wm_interm_68_triplet(p, q)

! term(160) = term(160) * 6.0d+0 
! term(161) = term(161) * (-4.0d+0) 
! term(162) = term(162) * 8.0d+0 
! term(163) = term(163) * 12.0d+0 
! term(164) = term(164) * (-8.0d+0) 
! term(165) = term(165) * 16.0d+0 
! term(166) = term(166) * 4.0d+0 
! term(167) = term(167) * (-8.0d+0) 
! term(168) = term(168) * 4.0d+0 

! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! term(169) = term(169) + wm_interm_16_triplet(a, b) * wm_interm_17_triplet(a, b)
! term(170) = term(170) + wm_interm_16_triplet(a, b) * wm_interm_20_triplet(a, b)
! term(171) = term(171) + wm_interm_16_triplet(a, b) * wm_interm_21_triplet(a, b)
! term(172) = term(172) + wm_interm_16_triplet(a, b) * wm_interm_22_triplet(a, b)
! term(173) = term(173) + wm_interm_62_triplet(a, b) * wm_interm_70_triplet(b, a)
! term(174) = term(174) + wm_interm_62_triplet(a, b) * wm_interm_71_triplet(b, a)
! term(175) = term(175) + wm_interm_12_triplet(a, b) * wm_interm_38_triplet(a, b)
! term(176) = term(176) + wm_interm_12_triplet(a, b) * wm_interm_39_triplet(a, b)
! term(177) = term(177) + wm_interm_12_triplet(a, b) * wm_interm_53_triplet(a, b)
! term(178) = term(178) + wm_interm_12_triplet(a, b) * wm_interm_54_triplet(a, b)
! end do 
! end do 

! term(169) = term(169) * (-6.0d+0) 
! term(170) = term(170) * 6.0d+0 
! term(171) = term(171) * 8.0d+0 
! term(172) = term(172) * (-16.0d+0) 
! term(173) = term(173) * (-8.0d+0) 
! term(174) = term(174) * 4.0d+0 
! term(175) = term(175) * (-6.0d+0) 
! term(176) = term(176) * 6.0d+0 
! term(177) = term(177) * 8.0d+0 
! term(178) = term(178) * (-16.0d+0) 


!     calc_D_oo_wm_triplet = zero
!     do s = 0, 178
!     calc_D_oo_wm_triplet = calc_D_oo_wm_triplet + term(s)
!     end do

!     end function calc_D_oo_wm_triplet
    
!     function calc_D_ov_wm_triplet(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
!     real(F64) :: calc_D_ov_wm_triplet
!     integer, intent(in) :: nocc, nactive
!     real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
!     real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
!     real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
!     real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
!     double precision, dimension(:), intent(in) :: vrdav_Rl
!     double precision, dimension(:), intent(in) :: vrdav_Rr
!     integer, intent(in) :: k1, k2
!     integer, intent(in) :: p, q
!     integer :: s , k, j, i, a, b 
!     real(F64), dimension(0:247) :: term 
!     term = 0.d+0 

!     term = 0.d+0 
!     do k = 1, nocc 
! do j = 1, nocc 
! do i = 1, nocc 
! term(0) = term(0) + wm_interm_66_triplet(q, i, j, k) * wm_interm_74_triplet(i, p, j, k)
! term(1) = term(1) + wm_interm_66_triplet(q, i, j, k) * wm_interm_74_triplet(i, p, k, j)
! term(2) = term(2) + wm_interm_66_triplet(q, i, j, k) * wm_interm_77_triplet(p, i, j, k)
! term(3) = term(3) + wm_interm_66_triplet(q, i, j, k) * wm_interm_77_triplet(i, p, j, k)
! term(4) = term(4) + wm_interm_66_triplet(q, i, j, k) * wm_interm_77_triplet(i, p, k, j)
! term(5) = term(5) + wm_interm_66_triplet(q, i, j, k) * wm_interm_77_triplet(p, i, k, j)
! term(6) = term(6) + wm_interm_35_triplet(q, i, j, k) * wm_interm_74_triplet(i, j, p, k)
! term(7) = term(7) + wm_interm_35_triplet(q, i, j, k) * wm_interm_74_triplet(i, j, k, p)
! term(8) = term(8) + wm_interm_35_triplet(q, i, j, k) * wm_interm_77_triplet(j, i, p, k)
! term(9) = term(9) + wm_interm_35_triplet(q, i, j, k) * wm_interm_77_triplet(i, j, p, k)
! term(10) = term(10) + wm_interm_35_triplet(q, i, j, k) * wm_interm_77_triplet(i, j, k, p)
! term(11) = term(11) + wm_interm_35_triplet(q, i, j, k) * wm_interm_77_triplet(j, i, k, p)
! term(12) = term(12) + wm_interm_60_triplet(q, i, j, k) * wm_interm_79_triplet(k, j, i, p)
! term(13) = term(13) + wm_interm_60_triplet(q, i, j, k) * wm_interm_79_triplet(j, k, i, p)
! term(14) = term(14) + wm_interm_50_triplet(q, i, j, k) * wm_interm_74_triplet(i, j, p, k)
! term(15) = term(15) + wm_interm_50_triplet(q, i, j, k) * wm_interm_74_triplet(i, j, k, p)
! term(16) = term(16) + wm_interm_49_triplet(q, i, j, k) * wm_interm_74_triplet(i, j, p, k)
! term(17) = term(17) + wm_interm_49_triplet(q, i, j, k) * wm_interm_74_triplet(i, j, k, p)
! term(18) = term(18) + wm_interm_50_triplet(q, i, j, k) * wm_interm_77_triplet(j, i, p, k)
! term(19) = term(19) + wm_interm_50_triplet(q, i, j, k) * wm_interm_77_triplet(i, j, p, k)
! term(20) = term(20) + wm_interm_50_triplet(q, i, j, k) * wm_interm_77_triplet(i, j, k, p)
! term(21) = term(21) + wm_interm_50_triplet(q, i, j, k) * wm_interm_77_triplet(j, i, k, p)
! term(22) = term(22) + wm_interm_49_triplet(q, i, j, k) * wm_interm_77_triplet(i, j, p, k)
! term(23) = term(23) + wm_interm_49_triplet(q, i, j, k) * wm_interm_77_triplet(j, i, p, k)
! term(24) = term(24) + wm_interm_49_triplet(q, i, j, k) * wm_interm_77_triplet(j, i, k, p)
! term(25) = term(25) + wm_interm_49_triplet(q, i, j, k) * wm_interm_77_triplet(i, j, k, p)
! term(26) = term(26) + wm_interm_60_triplet(q, i, j, k) * wm_interm_88_triplet(k, j, i, p)
! term(27) = term(27) + wm_interm_60_triplet(q, i, j, k) * wm_interm_88_triplet(j, k, i, p)
! end do 
! end do 
! end do 

! term(0) = term(0) * 3.0d+0 
! term(1) = term(1) * (-3.0d+0) 
! term(2) = term(2) * (-2.0d+0) 
! term(3) = term(3) * 4.0d+0 
! term(4) = term(4) * (-2.0d+0) 
! term(5) = term(5) * 4.0d+0 
! term(6) = term(6) * (-3.0d+0) 
! term(7) = term(7) * 3.0d+0 
! term(8) = term(8) * 3.0d+0 
! term(9) = term(9) * (-3.0d+0) 
! term(10) = term(10) * 3.0d+0 
! term(11) = term(11) * (-3.0d+0) 
! term(12) = term(12) * (-2.0d+0) 
! term(13) = term(13) * 4.0d+0 
! term(14) = term(14) * 3.0d+0 
! term(15) = term(15) * (-3.0d+0) 
! term(16) = term(16) * (-3.0d+0) 
! term(17) = term(17) * 3.0d+0 
! term(18) = term(18) * (-2.0d+0) 
! term(19) = term(19) * 4.0d+0 
! term(20) = term(20) * (-2.0d+0) 
! term(21) = term(21) * 4.0d+0 
! term(22) = term(22) * (-2.0d+0) 
! term(23) = term(23) * 4.0d+0 
! term(24) = term(24) * (-2.0d+0) 
! term(25) = term(25) * 4.0d+0 
! term(26) = term(26) * (-4.0d+0) 
! term(27) = term(27) * 8.0d+0 

! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do j = 1, nocc 
! do k = 1, nocc 
! do a = nocc + 1, nactive 
! term(28) = term(28) + s2(a,b,j,i) * t2(b,q,k,i) * wm_interm_72_triplet(a, k, j, p)
! end do 
! end do 
! end do 
! end do 
! end do 

! term(28) = term(28) * (-4.0d+0) 

! do k = 1, nocc 
! do j = 1, nocc 
! do b = nocc + 1, nactive 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! term(29) = term(29) + r2m(vrdav_Rl, a,i,q,j) * r2m(vrdav_Rr, a,i,b,k) * wm_interm_56_triplet(b, p, k, j)
! term(30) = term(30) + r2m(vrdav_Rl, a,i,q,j) * r2m(vrdav_Rr, a,i,b,k) * wm_interm_56_triplet(b, k, p, j)
! term(31) = term(31) + r2m(vrdav_Rl, a,i,q,j) * t2(a,b,k,j) * wm_interm_59_triplet(b, i, p, k)
! term(32) = term(32) + r2m(vrdav_Rl, a,i,q,j) * t2(a,b,j,k) * wm_interm_59_triplet(b, i, p, k)
! term(33) = term(33) + r2m(vrdav_Rl, a,i,q,j) * t2(a,b,i,k) * wm_interm_59_triplet(b, j, p, k)
! term(34) = term(34) + r2m(vrdav_Rl, a,i,q,j) * t2(a,b,j,k) * wm_interm_59_triplet(b, i, k, p)
! term(35) = term(35) + r2m(vrdav_Rl, a,i,q,j) * t2(a,b,i,k) * wm_interm_59_triplet(b, j, k, p)
! end do 
! end do 
! end do 
! end do 
! end do 

! term(29) = term(29) * 8.0d+0 
! term(30) = term(30) * (-16.0d+0) 
! term(31) = term(31) * (-2.0d+0) 
! term(32) = term(32) * 4.0d+0 
! term(33) = term(33) * (-8.0d+0) 
! term(34) = term(34) * (-2.0d+0) 
! term(35) = term(35) * 4.0d+0 

! do j = 1, nocc 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! do k = 1, nocc 
! do b = nocc + 1, nactive 
! term(36) = term(36) + r2p(vrdav_Rl, q,i,a,j) * r2p(vrdav_Rr, b,k,a,j) * wm_interm_56_triplet(b, p, k, i)
! term(37) = term(37) + r2p(vrdav_Rl, q,i,a,j) * r2p(vrdav_Rr, b,k,a,j) * wm_interm_56_triplet(b, k, p, i)
! term(38) = term(38) + r2m(vrdav_Rl, a,j,q,i) * r2p(vrdav_Rr, b,k,a,i) * wm_interm_56_triplet(b, p, k, j)
! term(39) = term(39) + r2m(vrdav_Rl, a,j,q,i) * r2p(vrdav_Rr, b,k,a,i) * wm_interm_56_triplet(b, k, p, j)
! term(40) = term(40) + r2m(vrdav_Rl, a,i,q,j) * r2p(vrdav_Rr, b,k,a,i) * wm_interm_56_triplet(b, p, k, j)
! term(41) = term(41) + r2m(vrdav_Rl, a,i,q,j) * r2p(vrdav_Rr, b,k,a,i) * wm_interm_56_triplet(b, k, p, j)
! term(42) = term(42) + r2m(vrdav_Rl, a,j,q,i) * r2p(vrdav_Rr, b,k,a,j) * wm_interm_56_triplet(b, p, k, i)
! term(43) = term(43) + r2m(vrdav_Rl, a,i,q,j) * r2p(vrdav_Rr, b,k,a,j) * wm_interm_56_triplet(b, p, k, i)
! term(44) = term(44) + r2m(vrdav_Rl, a,j,q,i) * r2p(vrdav_Rr, b,k,a,j) * wm_interm_56_triplet(b, k, p, i)
! term(45) = term(45) + r2m(vrdav_Rl, a,i,q,j) * r2p(vrdav_Rr, b,k,a,j) * wm_interm_56_triplet(b, k, p, i)
! end do 
! end do 
! end do 
! end do 
! end do 

! term(36) = term(36) * 12.0d+0 
! term(37) = term(37) * (-18.0d+0) 
! term(38) = term(38) * (-6.0d+0) 
! term(39) = term(39) * 6.0d+0 
! term(40) = term(40) * 6.0d+0 
! term(41) = term(41) * (-12.0d+0) 
! term(42) = term(42) * 6.0d+0 
! term(43) = term(43) * (-6.0d+0) 
! term(44) = term(44) * (-12.0d+0) 
! term(45) = term(45) * 6.0d+0 

! do a = nocc + 1, nactive 
! term(46) = term(46) + r1(vrdav_Rl, a,p) * wm_interm_12_triplet(a, q)
! term(47) = term(47) + r1(vrdav_Rr, a,p) * wm_interm_16_triplet(a, q)
! term(48) = term(48) + r1(vrdav_Rl, a,p) * wm_interm_17_triplet(a, q)
! term(49) = term(49) + r1(vrdav_Rl, a,p) * wm_interm_20_triplet(a, q)
! term(50) = term(50) + r1(vrdav_Rl, a,p) * wm_interm_21_triplet(a, q)
! term(51) = term(51) + r1(vrdav_Rl, a,p) * wm_interm_22_triplet(a, q)
! term(52) = term(52) + r1(vrdav_Rr, a,p) * wm_interm_38_triplet(a, q)
! term(53) = term(53) + r1(vrdav_Rr, a,p) * wm_interm_39_triplet(a, q)
! term(54) = term(54) + r1(vrdav_Rr, a,p) * wm_interm_53_triplet(a, q)
! term(55) = term(55) + r1(vrdav_Rr, a,p) * wm_interm_54_triplet(a, q)
! term(56) = term(56) + wm_interm_16_triplet(q, a) * wm_interm_57_triplet(a, p)
! term(57) = term(57) + wm_interm_16_triplet(q, a) * wm_interm_58_triplet(a, p)
! term(58) = term(58) + wm_interm_3_triplet(a, p) * wm_interm_70_triplet(a, q)
! term(59) = term(59) + wm_interm_3_triplet(a, p) * wm_interm_71_triplet(a, q)
! term(60) = term(60) + wm_interm_4_triplet(a, p) * wm_interm_70_triplet(a, q)
! term(61) = term(61) + wm_interm_5_triplet(a, p) * wm_interm_70_triplet(a, q)
! term(62) = term(62) + wm_interm_4_triplet(a, p) * wm_interm_71_triplet(a, q)
! term(63) = term(63) + wm_interm_5_triplet(a, p) * wm_interm_71_triplet(a, q)
! term(64) = term(64) + wm_interm_38_triplet(q, a) * wm_interm_57_triplet(a, p)
! term(65) = term(65) + wm_interm_38_triplet(q, a) * wm_interm_58_triplet(a, p)
! term(66) = term(66) + wm_interm_39_triplet(q, a) * wm_interm_57_triplet(a, p)
! term(67) = term(67) + wm_interm_39_triplet(q, a) * wm_interm_58_triplet(a, p)
! term(68) = term(68) + wm_interm_0_triplet(a, p) * wm_interm_38_triplet(a, q)
! term(69) = term(69) + wm_interm_0_triplet(a, p) * wm_interm_39_triplet(a, q)
! term(70) = term(70) + wm_interm_1_triplet(a, p) * wm_interm_38_triplet(a, q)
! term(71) = term(71) + wm_interm_1_triplet(a, p) * wm_interm_39_triplet(a, q)
! term(72) = term(72) + wm_interm_2_triplet(a, p) * wm_interm_38_triplet(a, q)
! term(73) = term(73) + wm_interm_2_triplet(a, p) * wm_interm_39_triplet(a, q)
! term(74) = term(74) + wm_interm_53_triplet(q, a) * wm_interm_57_triplet(a, p)
! term(75) = term(75) + wm_interm_54_triplet(q, a) * wm_interm_57_triplet(a, p)
! term(76) = term(76) + wm_interm_53_triplet(q, a) * wm_interm_58_triplet(a, p)
! term(77) = term(77) + wm_interm_54_triplet(q, a) * wm_interm_58_triplet(a, p)
! term(78) = term(78) + wm_interm_0_triplet(a, p) * wm_interm_53_triplet(a, q)
! term(79) = term(79) + wm_interm_0_triplet(a, p) * wm_interm_54_triplet(a, q)
! term(80) = term(80) + wm_interm_1_triplet(a, p) * wm_interm_53_triplet(a, q)
! term(81) = term(81) + wm_interm_1_triplet(a, p) * wm_interm_54_triplet(a, q)
! term(82) = term(82) + wm_interm_2_triplet(a, p) * wm_interm_53_triplet(a, q)
! term(83) = term(83) + wm_interm_2_triplet(a, p) * wm_interm_54_triplet(a, q)
! end do 

! term(46) = term(46) * 2.0d+0 
! term(47) = term(47) * 2.0d+0 
! term(48) = term(48) * 3.0d+0 
! term(49) = term(49) * (-3.0d+0) 
! term(50) = term(50) * (-4.0d+0) 
! term(51) = term(51) * 8.0d+0 
! term(52) = term(52) * 3.0d+0 
! term(53) = term(53) * (-3.0d+0) 
! term(54) = term(54) * (-4.0d+0) 
! term(55) = term(55) * 8.0d+0 
! term(56) = term(56) * 2.0d+0 
! term(57) = term(57) * (-4.0d+0) 
! term(58) = term(58) * (-12.0d+0) 
! term(59) = term(59) * 6.0d+0 
! term(60) = term(60) * 8.0d+0 
! term(61) = term(61) * (-16.0d+0) 
! term(62) = term(62) * (-4.0d+0) 
! term(63) = term(63) * 8.0d+0 
! term(64) = term(64) * (-3.0d+0) 
! term(65) = term(65) * 6.0d+0 
! term(66) = term(66) * 3.0d+0 
! term(67) = term(67) * (-6.0d+0) 
! term(68) = term(68) * 9.0d+0 
! term(69) = term(69) * (-9.0d+0) 
! term(70) = term(70) * 12.0d+0 
! term(71) = term(71) * (-12.0d+0) 
! term(72) = term(72) * (-6.0d+0) 
! term(73) = term(73) * 6.0d+0 
! term(74) = term(74) * (-4.0d+0) 
! term(75) = term(75) * 8.0d+0 
! term(76) = term(76) * 8.0d+0 
! term(77) = term(77) * (-16.0d+0) 
! term(78) = term(78) * (-12.0d+0) 
! term(79) = term(79) * 24.0d+0 
! term(80) = term(80) * (-16.0d+0) 
! term(81) = term(81) * 32.0d+0 
! term(82) = term(82) * 8.0d+0 
! term(83) = term(83) * (-16.0d+0) 

! do j = 1, nocc 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! do k = 1, nocc 
! term(84) = term(84) + r2p(vrdav_Rl, q,i,a,j) * r2m(vrdav_Rr, a,k,b,j) * wm_interm_56_triplet(b, p, k, i)
! term(85) = term(85) + r2p(vrdav_Rl, q,i,a,j) * r2m(vrdav_Rr, a,k,b,j) * wm_interm_56_triplet(b, k, p, i)
! end do 
! end do 
! end do 
! end do 
! end do 

! term(84) = term(84) * (-12.0d+0) 
! term(85) = term(85) * 12.0d+0 

! do k = 1, nocc 
! do i = 1, nocc 
! do j = 1, nocc 
! term(86) = term(86) + wm_interm_73_triplet(i, p, j, k) * wm_interm_80_triplet(q, j, k, i)
! term(87) = term(87) + wm_interm_73_triplet(p, i, j, k) * wm_interm_80_triplet(q, j, k, i)
! term(88) = term(88) + wm_interm_60_triplet(q, i, j, k) * wm_interm_78_triplet(k, j, p, i)
! term(89) = term(89) + wm_interm_60_triplet(q, i, j, k) * wm_interm_78_triplet(j, k, p, i)
! term(90) = term(90) + wm_interm_60_triplet(q, i, j, k) * wm_interm_79_triplet(k, j, p, i)
! term(91) = term(91) + wm_interm_60_triplet(q, i, j, k) * wm_interm_79_triplet(j, k, p, i)
! term(92) = term(92) + wm_interm_73_triplet(i, p, j, k) * wm_interm_89_triplet(q, j, k, i)
! term(93) = term(93) + wm_interm_73_triplet(p, i, j, k) * wm_interm_89_triplet(q, j, k, i)
! term(94) = term(94) + wm_interm_73_triplet(p, i, j, k) * wm_interm_90_triplet(q, j, k, i)
! term(95) = term(95) + wm_interm_73_triplet(i, p, j, k) * wm_interm_90_triplet(q, j, k, i)
! term(96) = term(96) + wm_interm_60_triplet(q, i, j, k) * wm_interm_87_triplet(k, j, p, i)
! term(97) = term(97) + wm_interm_60_triplet(q, i, j, k) * wm_interm_87_triplet(j, k, p, i)
! term(98) = term(98) + wm_interm_60_triplet(q, i, j, k) * wm_interm_88_triplet(k, j, p, i)
! term(99) = term(99) + wm_interm_60_triplet(q, i, j, k) * wm_interm_88_triplet(j, k, p, i)
! end do 
! end do 
! end do 

! term(86) = term(86) * 3.0d+0 
! term(87) = term(87) * (-3.0d+0) 
! term(88) = term(88) * 3.0d+0 
! term(89) = term(89) * (-3.0d+0) 
! term(90) = term(90) * 4.0d+0 
! term(91) = term(91) * (-2.0d+0) 
! term(92) = term(92) * (-2.0d+0) 
! term(93) = term(93) * 4.0d+0 
! term(94) = term(94) * (-2.0d+0) 
! term(95) = term(95) * 4.0d+0 
! term(96) = term(96) * 6.0d+0 
! term(97) = term(97) * (-6.0d+0) 
! term(98) = term(98) * 8.0d+0 
! term(99) = term(99) * (-4.0d+0) 

! do j = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! do k = 1, nocc 
! do i = 1, nocc 
! term(100) = term(100) + r2p(vrdav_Rl, q,i,a,j) * t2(a,b,k,j) * wm_interm_59_triplet(b, i, k, p)
! end do 
! end do 
! end do 
! end do 
! end do 

! term(100) = term(100) * (-6.0d+0) 

! do k = 1, nocc 
! do j = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! do i = 1, nocc 
! term(101) = term(101) + r2p(vrdav_Rl, q,i,a,j) * t2(a,b,k,j) * wm_interm_59_triplet(b, i, p, k)
! term(102) = term(102) + r2p(vrdav_Rl, q,i,a,j) * t2(a,b,j,k) * wm_interm_59_triplet(b, i, p, k)
! term(103) = term(103) + r2p(vrdav_Rl, q,i,a,j) * t2(a,b,j,k) * wm_interm_59_triplet(b, i, k, p)
! end do 
! end do 
! end do 
! end do 
! end do 

! term(101) = term(101) * 6.0d+0 
! term(102) = term(102) * (-12.0d+0) 
! term(103) = term(103) * 6.0d+0 

! do i = 1, nocc 
! term(104) = term(104) + s1(q,i) * wm_interm_27_triplet(p, i)
! term(105) = term(105) + s1(q,i) * wm_interm_30_triplet(p, i)
! term(106) = term(106) + s1(q,i) * wm_interm_31_triplet(p, i)
! term(107) = term(107) + t1(q,i) * wm_interm_27_triplet(i, p)
! term(108) = term(108) + t1(q,i) * wm_interm_30_triplet(i, p)
! term(109) = term(109) + t1(q,i) * wm_interm_31_triplet(i, p)
! term(110) = term(110) + s1(q,i) * wm_interm_40_triplet(p, i)
! term(111) = term(111) + s1(q,i) * wm_interm_41_triplet(p, i)
! term(112) = term(112) + s1(q,i) * wm_interm_43_triplet(p, i)
! term(113) = term(113) + s1(q,i) * wm_interm_44_triplet(p, i)
! term(114) = term(114) + s1(q,i) * wm_interm_45_triplet(p, i)
! term(115) = term(115) + s1(q,i) * wm_interm_46_triplet(p, i)
! term(116) = term(116) + t1(q,i) * wm_interm_40_triplet(i, p)
! term(117) = term(117) + t1(q,i) * wm_interm_41_triplet(i, p)
! term(118) = term(118) + t1(q,i) * wm_interm_43_triplet(i, p)
! term(119) = term(119) + t1(q,i) * wm_interm_44_triplet(i, p)
! term(120) = term(120) + t1(q,i) * wm_interm_45_triplet(i, p)
! term(121) = term(121) + t1(q,i) * wm_interm_46_triplet(i, p)
! term(122) = term(122) + wm_interm_55_triplet(i, p) * wm_interm_61_triplet(q, i)
! term(123) = term(123) + wm_interm_14_triplet(p, i) * wm_interm_63_triplet(q, i)
! term(124) = term(124) + wm_interm_55_triplet(i, p) * wm_interm_65_triplet(q, i)
! term(125) = term(125) + wm_interm_14_triplet(p, i) * wm_interm_64_triplet(q, i)
! term(126) = term(126) + wm_interm_18_triplet(p, i) * wm_interm_63_triplet(q, i)
! term(127) = term(127) + wm_interm_19_triplet(p, i) * wm_interm_63_triplet(q, i)
! term(128) = term(128) + wm_interm_18_triplet(p, i) * wm_interm_64_triplet(q, i)
! term(129) = term(129) + wm_interm_19_triplet(p, i) * wm_interm_64_triplet(q, i)
! term(130) = term(130) + wm_interm_23_triplet(p, i) * wm_interm_63_triplet(q, i)
! term(131) = term(131) + wm_interm_24_triplet(p, i) * wm_interm_63_triplet(q, i)
! term(132) = term(132) + wm_interm_25_triplet(p, i) * wm_interm_63_triplet(q, i)
! term(133) = term(133) + wm_interm_26_triplet(p, i) * wm_interm_63_triplet(q, i)
! term(134) = term(134) + wm_interm_23_triplet(p, i) * wm_interm_64_triplet(q, i)
! term(135) = term(135) + wm_interm_24_triplet(p, i) * wm_interm_64_triplet(q, i)
! term(136) = term(136) + wm_interm_25_triplet(p, i) * wm_interm_64_triplet(q, i)
! term(137) = term(137) + wm_interm_26_triplet(p, i) * wm_interm_64_triplet(q, i)
! term(138) = term(138) + wm_interm_68_triplet(p, i) * wm_interm_7_triplet(q, i)
! term(139) = term(139) + wm_interm_69_triplet(p, i) * wm_interm_7_triplet(q, i)
! term(140) = term(140) + wm_interm_18_triplet(i, p) * wm_interm_6_triplet(q, i)
! term(141) = term(141) + wm_interm_19_triplet(i, p) * wm_interm_6_triplet(q, i)
! term(142) = term(142) + wm_interm_23_triplet(i, p) * wm_interm_6_triplet(q, i)
! term(143) = term(143) + wm_interm_24_triplet(i, p) * wm_interm_6_triplet(q, i)
! term(144) = term(144) + wm_interm_25_triplet(i, p) * wm_interm_6_triplet(q, i)
! term(145) = term(145) + wm_interm_26_triplet(i, p) * wm_interm_6_triplet(q, i)
! term(146) = term(146) + wm_interm_27_triplet(i, p) * wm_interm_61_triplet(q, i)
! term(147) = term(147) + wm_interm_27_triplet(i, p) * wm_interm_65_triplet(q, i)
! term(148) = term(148) + wm_interm_30_triplet(i, p) * wm_interm_61_triplet(q, i)
! term(149) = term(149) + wm_interm_31_triplet(i, p) * wm_interm_61_triplet(q, i)
! term(150) = term(150) + wm_interm_30_triplet(i, p) * wm_interm_65_triplet(q, i)
! term(151) = term(151) + wm_interm_31_triplet(i, p) * wm_interm_65_triplet(q, i)
! term(152) = term(152) + wm_interm_10_triplet(q, i) * wm_interm_68_triplet(p, i)
! term(153) = term(153) + wm_interm_10_triplet(q, i) * wm_interm_69_triplet(p, i)
! term(154) = term(154) + wm_interm_11_triplet(q, i) * wm_interm_68_triplet(p, i)
! term(155) = term(155) + wm_interm_11_triplet(q, i) * wm_interm_69_triplet(p, i)
! term(156) = term(156) + wm_interm_18_triplet(i, p) * wm_interm_8_triplet(q, i)
! term(157) = term(157) + wm_interm_19_triplet(i, p) * wm_interm_8_triplet(q, i)
! term(158) = term(158) + wm_interm_18_triplet(i, p) * wm_interm_9_triplet(q, i)
! term(159) = term(159) + wm_interm_19_triplet(i, p) * wm_interm_9_triplet(q, i)
! term(160) = term(160) + wm_interm_23_triplet(i, p) * wm_interm_8_triplet(q, i)
! term(161) = term(161) + wm_interm_24_triplet(i, p) * wm_interm_8_triplet(q, i)
! term(162) = term(162) + wm_interm_25_triplet(i, p) * wm_interm_8_triplet(q, i)
! term(163) = term(163) + wm_interm_26_triplet(i, p) * wm_interm_8_triplet(q, i)
! term(164) = term(164) + wm_interm_23_triplet(i, p) * wm_interm_9_triplet(q, i)
! term(165) = term(165) + wm_interm_24_triplet(i, p) * wm_interm_9_triplet(q, i)
! term(166) = term(166) + wm_interm_25_triplet(i, p) * wm_interm_9_triplet(q, i)
! term(167) = term(167) + wm_interm_26_triplet(i, p) * wm_interm_9_triplet(q, i)
! term(168) = term(168) + wm_interm_40_triplet(i, p) * wm_interm_61_triplet(q, i)
! term(169) = term(169) + wm_interm_41_triplet(i, p) * wm_interm_61_triplet(q, i)
! term(170) = term(170) + wm_interm_40_triplet(i, p) * wm_interm_65_triplet(q, i)
! term(171) = term(171) + wm_interm_41_triplet(i, p) * wm_interm_65_triplet(q, i)
! term(172) = term(172) + wm_interm_43_triplet(i, p) * wm_interm_61_triplet(q, i)
! term(173) = term(173) + wm_interm_44_triplet(i, p) * wm_interm_61_triplet(q, i)
! term(174) = term(174) + wm_interm_45_triplet(i, p) * wm_interm_61_triplet(q, i)
! term(175) = term(175) + wm_interm_46_triplet(i, p) * wm_interm_61_triplet(q, i)
! term(176) = term(176) + wm_interm_43_triplet(i, p) * wm_interm_65_triplet(q, i)
! term(177) = term(177) + wm_interm_44_triplet(i, p) * wm_interm_65_triplet(q, i)
! term(178) = term(178) + wm_interm_45_triplet(i, p) * wm_interm_65_triplet(q, i)
! term(179) = term(179) + wm_interm_46_triplet(i, p) * wm_interm_65_triplet(q, i)
! end do 

! term(104) = term(104) * 6.0d+0 
! term(105) = term(105) * 6.0d+0 
! term(106) = term(106) * (-6.0d+0) 
! term(107) = term(107) * 6.0d+0 
! term(108) = term(108) * 6.0d+0 
! term(109) = term(109) * (-6.0d+0) 
! term(110) = term(110) * (-6.0d+0) 
! term(111) = term(111) * 6.0d+0 
! term(112) = term(112) * (-4.0d+0) 
! term(113) = term(113) * 8.0d+0 
! term(114) = term(114) * (-4.0d+0) 
! term(115) = term(115) * 8.0d+0 
! term(116) = term(116) * (-6.0d+0) 
! term(117) = term(117) * 6.0d+0 
! term(118) = term(118) * (-4.0d+0) 
! term(119) = term(119) * 8.0d+0 
! term(120) = term(120) * (-4.0d+0) 
! term(121) = term(121) * 8.0d+0 
! term(122) = term(122) * 2.0d+0 
! term(123) = term(123) * (-4.0d+0) 
! term(124) = term(124) * (-4.0d+0) 
! term(125) = term(125) * 2.0d+0 
! term(126) = term(126) * 6.0d+0 
! term(127) = term(127) * (-6.0d+0) 
! term(128) = term(128) * (-3.0d+0) 
! term(129) = term(129) * 3.0d+0 
! term(130) = term(130) * 4.0d+0 
! term(131) = term(131) * (-8.0d+0) 
! term(132) = term(132) * 4.0d+0 
! term(133) = term(133) * (-8.0d+0) 
! term(134) = term(134) * (-2.0d+0) 
! term(135) = term(135) * 4.0d+0 
! term(136) = term(136) * (-2.0d+0) 
! term(137) = term(137) * 4.0d+0 
! term(138) = term(138) * 6.0d+0 
! term(139) = term(139) * (-12.0d+0) 
! term(140) = term(140) * (-9.0d+0) 
! term(141) = term(141) * 9.0d+0 
! term(142) = term(142) * (-6.0d+0) 
! term(143) = term(143) * 12.0d+0 
! term(144) = term(144) * (-6.0d+0) 
! term(145) = term(145) * 12.0d+0 
! term(146) = term(146) * 6.0d+0 
! term(147) = term(147) * (-12.0d+0) 
! term(148) = term(148) * 6.0d+0 
! term(149) = term(149) * (-6.0d+0) 
! term(150) = term(150) * (-12.0d+0) 
! term(151) = term(151) * 12.0d+0 
! term(152) = term(152) * (-4.0d+0) 
! term(153) = term(153) * 8.0d+0 
! term(154) = term(154) * 8.0d+0 
! term(155) = term(155) * (-16.0d+0) 
! term(156) = term(156) * 12.0d+0 
! term(157) = term(157) * (-12.0d+0) 
! term(158) = term(158) * (-6.0d+0) 
! term(159) = term(159) * 6.0d+0 
! term(160) = term(160) * 8.0d+0 
! term(161) = term(161) * (-16.0d+0) 
! term(162) = term(162) * 8.0d+0 
! term(163) = term(163) * (-16.0d+0) 
! term(164) = term(164) * (-4.0d+0) 
! term(165) = term(165) * 8.0d+0 
! term(166) = term(166) * (-4.0d+0) 
! term(167) = term(167) * 8.0d+0 
! term(168) = term(168) * (-6.0d+0) 
! term(169) = term(169) * 6.0d+0 
! term(170) = term(170) * 12.0d+0 
! term(171) = term(171) * (-12.0d+0) 
! term(172) = term(172) * (-4.0d+0) 
! term(173) = term(173) * 8.0d+0 
! term(174) = term(174) * (-4.0d+0) 
! term(175) = term(175) * 8.0d+0 
! term(176) = term(176) * 8.0d+0 
! term(177) = term(177) * (-16.0d+0) 
! term(178) = term(178) * 8.0d+0 
! term(179) = term(179) * (-16.0d+0) 

! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do k = 1, nocc 
! do j = 1, nocc 
! do a = nocc + 1, nactive 
! term(180) = term(180) + r2m(vrdav_Rl, a,j,q,i) * t2(a,b,k,i) * wm_interm_59_triplet(b, j, k, p)
! end do 
! end do 
! end do 
! end do 
! end do 

! term(180) = term(180) * 4.0d+0 

! do j = 1, nocc 
! do b = nocc + 1, nactive 
! do k = 1, nocc 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! term(181) = term(181) + r2m(vrdav_Rl, a,i,q,j) * t2(a,b,k,j) * wm_interm_59_triplet(b, i, k, p)
! end do 
! end do 
! end do 
! end do 
! end do 

! term(181) = term(181) * 4.0d+0 

! do j = 1, nocc 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do k = 1, nocc 
! do a = nocc + 1, nactive 
! term(182) = term(182) + s2(a,b,i,j) * t2(b,q,k,i) * wm_interm_67_triplet(a, k, p, j)
! term(183) = term(183) + s2(a,b,j,i) * t2(b,q,k,i) * wm_interm_67_triplet(a, k, p, j)
! term(184) = term(184) + s2(a,b,i,j) * t2(b,q,k,i) * wm_interm_72_triplet(a, k, p, j)
! term(185) = term(185) + s2(a,b,i,j) * t2(b,q,k,i) * wm_interm_72_triplet(a, k, j, p)
! term(186) = term(186) + s2(a,b,j,i) * t2(b,q,k,i) * wm_interm_72_triplet(a, k, p, j)
! term(187) = term(187) + r2m(vrdav_Rl, a,j,q,i) * r2m(vrdav_Rr, a,k,b,i) * wm_interm_56_triplet(b, p, k, j)
! term(188) = term(188) + r2m(vrdav_Rl, a,j,q,i) * r2m(vrdav_Rr, a,k,b,i) * wm_interm_56_triplet(b, k, p, j)
! term(189) = term(189) + r2m(vrdav_Rl, a,i,q,j) * r2m(vrdav_Rr, a,k,b,i) * wm_interm_56_triplet(b, p, k, j)
! term(190) = term(190) + r2m(vrdav_Rl, a,i,q,j) * r2m(vrdav_Rr, a,k,b,i) * wm_interm_56_triplet(b, k, p, j)
! term(191) = term(191) + r2m(vrdav_Rl, a,j,q,i) * r2m(vrdav_Rr, a,k,b,j) * wm_interm_56_triplet(b, p, k, i)
! term(192) = term(192) + r2m(vrdav_Rl, a,i,q,j) * r2m(vrdav_Rr, a,k,b,j) * wm_interm_56_triplet(b, p, k, i)
! term(193) = term(193) + r2m(vrdav_Rl, a,j,q,i) * r2m(vrdav_Rr, a,k,b,j) * wm_interm_56_triplet(b, k, p, i)
! term(194) = term(194) + r2m(vrdav_Rl, a,i,q,j) * r2m(vrdav_Rr, a,k,b,j) * wm_interm_56_triplet(b, k, p, i)
! term(195) = term(195) + r2m(vrdav_Rl, a,j,q,i) * t2(a,b,k,j) * wm_interm_59_triplet(b, i, k, p)
! term(196) = term(196) + r2m(vrdav_Rl, a,i,q,j) * t2(a,b,k,i) * wm_interm_59_triplet(b, j, k, p)
! end do 
! end do 
! end do 
! end do 
! end do 

! term(182) = term(182) * (-6.0d+0) 
! term(183) = term(183) * 6.0d+0 
! term(184) = term(184) * (-4.0d+0) 
! term(185) = term(185) * 8.0d+0 
! term(186) = term(186) * 8.0d+0 
! term(187) = term(187) * 8.0d+0 
! term(188) = term(188) * (-4.0d+0) 
! term(189) = term(189) * (-4.0d+0) 
! term(190) = term(190) * 8.0d+0 
! term(191) = term(191) * (-4.0d+0) 
! term(192) = term(192) * 8.0d+0 
! term(193) = term(193) * 8.0d+0 
! term(194) = term(194) * (-4.0d+0) 
! term(195) = term(195) * (-2.0d+0) 
! term(196) = term(196) * (-2.0d+0) 

! do k = 1, nocc 
! do j = 1, nocc 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! term(197) = term(197) + s2(a,b,j,k) * t2(b,q,k,i) * wm_interm_67_triplet(a, i, p, j)
! term(198) = term(198) + s2(a,b,j,k) * t2(b,q,k,i) * wm_interm_72_triplet(a, i, p, j)
! term(199) = term(199) + r2p(vrdav_Rl, q,i,a,j) * r2m(vrdav_Rr, a,j,b,k) * wm_interm_56_triplet(b, p, k, i)
! term(200) = term(200) + r2p(vrdav_Rl, q,i,a,j) * r2m(vrdav_Rr, a,j,b,k) * wm_interm_56_triplet(b, k, p, i)
! term(201) = term(201) + r2m(vrdav_Rl, a,j,q,i) * r2m(vrdav_Rr, a,i,b,k) * wm_interm_56_triplet(b, p, k, j)
! term(202) = term(202) + r2m(vrdav_Rl, a,j,q,i) * r2m(vrdav_Rr, a,i,b,k) * wm_interm_56_triplet(b, k, p, j)
! term(203) = term(203) + r2m(vrdav_Rl, a,i,q,j) * r2m(vrdav_Rr, a,j,b,k) * wm_interm_56_triplet(b, p, k, i)
! term(204) = term(204) + r2m(vrdav_Rl, a,i,q,j) * r2m(vrdav_Rr, a,j,b,k) * wm_interm_56_triplet(b, k, p, i)
! term(205) = term(205) + r2m(vrdav_Rl, a,j,q,i) * t2(a,b,k,j) * wm_interm_59_triplet(b, i, p, k)
! term(206) = term(206) + r2m(vrdav_Rl, a,i,q,j) * t2(a,b,k,i) * wm_interm_59_triplet(b, j, p, k)
! term(207) = term(207) + r2m(vrdav_Rl, a,i,b,j) * t2(a,q,k,i) * wm_interm_29_triplet(b, j, p, k)
! term(208) = term(208) + r2m(vrdav_Rl, a,i,b,j) * t2(b,q,k,i) * wm_interm_29_triplet(a, j, p, k)
! term(209) = term(209) + r2m(vrdav_Rl, a,i,b,j) * t2(a,q,k,i) * wm_interm_34_triplet(b, p, j, k)
! term(210) = term(210) + r2m(vrdav_Rl, a,i,b,j) * t2(a,q,k,i) * wm_interm_34_triplet(b, j, p, k)
! term(211) = term(211) + r2m(vrdav_Rl, a,i,b,j) * t2(b,q,k,i) * wm_interm_34_triplet(a, j, p, k)
! term(212) = term(212) + r2m(vrdav_Rl, a,i,b,j) * t2(b,q,k,i) * wm_interm_34_triplet(a, p, j, k)
! end do 
! end do 
! end do 
! end do 
! end do 

! term(197) = term(197) * (-12.0d+0) 
! term(198) = term(198) * (-16.0d+0) 
! term(199) = term(199) * 12.0d+0 
! term(200) = term(200) * (-24.0d+0) 
! term(201) = term(201) * (-4.0d+0) 
! term(202) = term(202) * 8.0d+0 
! term(203) = term(203) * (-4.0d+0) 
! term(204) = term(204) * 8.0d+0 
! term(205) = term(205) * 4.0d+0 
! term(206) = term(206) * 4.0d+0 
! term(207) = term(207) * 6.0d+0 
! term(208) = term(208) * (-6.0d+0) 
! term(209) = term(209) * (-4.0d+0) 
! term(210) = term(210) * 8.0d+0 
! term(211) = term(211) * (-4.0d+0) 
! term(212) = term(212) * 8.0d+0 

! do k = 1, nocc 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do j = 1, nocc 
! do a = nocc + 1, nactive 
! term(213) = term(213) + s2(a,b,j,k) * t2(b,q,j,i) * wm_interm_67_triplet(a, i, p, k)
! term(214) = term(214) + s2(a,b,j,k) * t2(b,q,k,i) * wm_interm_72_triplet(a, i, j, p)
! term(215) = term(215) + s2(a,b,j,k) * t2(b,q,j,i) * wm_interm_72_triplet(a, i, k, p)
! term(216) = term(216) + s2(a,b,j,k) * t2(b,q,j,i) * wm_interm_72_triplet(a, i, p, k)
! term(217) = term(217) + r2p(vrdav_Rl, a,j,b,k) * t2(b,q,k,i) * wm_interm_29_triplet(a, j, p, i)
! term(218) = term(218) + r2p(vrdav_Rl, a,j,b,i) * t2(b,q,k,i) * wm_interm_29_triplet(a, j, p, k)
! term(219) = term(219) + r2p(vrdav_Rl, a,j,b,k) * t2(b,q,k,i) * wm_interm_34_triplet(a, p, j, i)
! term(220) = term(220) + r2p(vrdav_Rl, a,j,b,k) * t2(b,q,k,i) * wm_interm_34_triplet(a, j, p, i)
! term(221) = term(221) + r2p(vrdav_Rl, a,j,b,i) * t2(b,q,k,i) * wm_interm_34_triplet(a, p, j, k)
! term(222) = term(222) + r2p(vrdav_Rl, a,j,b,i) * t2(b,q,k,i) * wm_interm_34_triplet(a, j, p, k)
! term(223) = term(223) + r2m(vrdav_Rl, a,j,q,i) * r2m(vrdav_Rr, a,j,b,k) * wm_interm_56_triplet(b, p, k, i)
! term(224) = term(224) + r2m(vrdav_Rl, a,j,q,i) * r2m(vrdav_Rr, a,j,b,k) * wm_interm_56_triplet(b, k, p, i)
! term(225) = term(225) + r2m(vrdav_Rl, a,j,q,i) * t2(a,b,j,k) * wm_interm_59_triplet(b, i, p, k)
! term(226) = term(226) + r2m(vrdav_Rl, a,j,q,i) * t2(a,b,j,k) * wm_interm_59_triplet(b, i, k, p)
! term(227) = term(227) + r2m(vrdav_Rl, a,j,q,i) * t2(a,b,k,i) * wm_interm_59_triplet(b, j, p, k)
! term(228) = term(228) + r2m(vrdav_Rl, a,j,q,i) * t2(a,b,i,k) * wm_interm_59_triplet(b, j, p, k)
! term(229) = term(229) + r2m(vrdav_Rl, a,j,q,i) * t2(a,b,i,k) * wm_interm_59_triplet(b, j, k, p)
! term(230) = term(230) + r2m(vrdav_Rl, a,j,b,k) * t2(a,q,k,i) * wm_interm_29_triplet(b, j, p, i)
! term(231) = term(231) + r2m(vrdav_Rl, a,j,b,k) * t2(a,q,j,i) * wm_interm_29_triplet(b, k, p, i)
! term(232) = term(232) + r2m(vrdav_Rl, a,j,b,k) * t2(b,q,j,i) * wm_interm_29_triplet(a, k, p, i)
! term(233) = term(233) + r2m(vrdav_Rl, a,j,b,k) * t2(b,q,k,i) * wm_interm_29_triplet(a, j, p, i)
! term(234) = term(234) + r2m(vrdav_Rl, a,j,b,i) * t2(a,q,k,i) * wm_interm_29_triplet(b, j, p, k)
! term(235) = term(235) + r2m(vrdav_Rl, a,j,b,i) * t2(b,q,k,i) * wm_interm_29_triplet(a, j, p, k)
! term(236) = term(236) + r2m(vrdav_Rl, a,j,b,k) * t2(a,q,k,i) * wm_interm_34_triplet(b, p, j, i)
! term(237) = term(237) + r2m(vrdav_Rl, a,j,b,k) * t2(a,q,k,i) * wm_interm_34_triplet(b, j, p, i)
! term(238) = term(238) + r2m(vrdav_Rl, a,j,b,k) * t2(a,q,j,i) * wm_interm_34_triplet(b, p, k, i)
! term(239) = term(239) + r2m(vrdav_Rl, a,j,b,k) * t2(a,q,j,i) * wm_interm_34_triplet(b, k, p, i)
! term(240) = term(240) + r2m(vrdav_Rl, a,j,b,k) * t2(b,q,j,i) * wm_interm_34_triplet(a, p, k, i)
! term(241) = term(241) + r2m(vrdav_Rl, a,j,b,k) * t2(b,q,j,i) * wm_interm_34_triplet(a, k, p, i)
! term(242) = term(242) + r2m(vrdav_Rl, a,j,b,k) * t2(b,q,k,i) * wm_interm_34_triplet(a, p, j, i)
! term(243) = term(243) + r2m(vrdav_Rl, a,j,b,k) * t2(b,q,k,i) * wm_interm_34_triplet(a, j, p, i)
! term(244) = term(244) + r2m(vrdav_Rl, a,j,b,i) * t2(a,q,k,i) * wm_interm_34_triplet(b, j, p, k)
! term(245) = term(245) + r2m(vrdav_Rl, a,j,b,i) * t2(a,q,k,i) * wm_interm_34_triplet(b, p, j, k)
! term(246) = term(246) + r2m(vrdav_Rl, a,j,b,i) * t2(b,q,k,i) * wm_interm_34_triplet(a, p, j, k)
! term(247) = term(247) + r2m(vrdav_Rl, a,j,b,i) * t2(b,q,k,i) * wm_interm_34_triplet(a, j, p, k)
! end do 
! end do 
! end do 
! end do 
! end do 

! term(213) = term(213) * 6.0d+0 
! term(214) = term(214) * 8.0d+0 
! term(215) = term(215) * (-4.0d+0) 
! term(216) = term(216) * 8.0d+0 
! term(217) = term(217) * (-18.0d+0) 
! term(218) = term(218) * 12.0d+0 
! term(219) = term(219) * 12.0d+0 
! term(220) = term(220) * (-24.0d+0) 
! term(221) = term(221) * (-12.0d+0) 
! term(222) = term(222) * 12.0d+0 
! term(223) = term(223) * 8.0d+0 
! term(224) = term(224) * (-16.0d+0) 
! term(225) = term(225) * (-8.0d+0) 
! term(226) = term(226) * 4.0d+0 
! term(227) = term(227) * (-2.0d+0) 
! term(228) = term(228) * 4.0d+0 
! term(229) = term(229) * (-2.0d+0) 
! term(230) = term(230) * 6.0d+0 
! term(231) = term(231) * (-12.0d+0) 
! term(232) = term(232) * 6.0d+0 
! term(233) = term(233) * (-12.0d+0) 
! term(234) = term(234) * (-6.0d+0) 
! term(235) = term(235) * 6.0d+0 
! term(236) = term(236) * (-4.0d+0) 
! term(237) = term(237) * 8.0d+0 
! term(238) = term(238) * 8.0d+0 
! term(239) = term(239) * (-16.0d+0) 
! term(240) = term(240) * (-4.0d+0) 
! term(241) = term(241) * 8.0d+0 
! term(242) = term(242) * 8.0d+0 
! term(243) = term(243) * (-16.0d+0) 
! term(244) = term(244) * (-4.0d+0) 
! term(245) = term(245) * 8.0d+0 
! term(246) = term(246) * (-4.0d+0) 
! term(247) = term(247) * 8.0d+0 


!     calc_D_ov_wm_triplet = zero
!     do s = 0, 247
!     calc_D_ov_wm_triplet = calc_D_ov_wm_triplet + term(s)
!     end do

!     end function calc_D_ov_wm_triplet
    
!     function calc_D_vo_wm_triplet(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
!     real(F64) :: calc_D_vo_wm_triplet
!     integer, intent(in) :: nocc, nactive
!     real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
!     real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
!     real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
!     real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
!     double precision, dimension(:), intent(in) :: vrdav_Rl
!     double precision, dimension(:), intent(in) :: vrdav_Rr
!     integer, intent(in) :: k1, k2
!     integer, intent(in) :: p, q
!     integer :: s , j, a, i, k, b 
!     real(F64), dimension(0:659) :: term 
!     term = 0.d+0 

!     term = 0.d+0 
!     do j = 1, nocc 
! do a = nocc + 1, nactive 
! do i = 1, nocc 
! term(0) = term(0) + r2p(vrdav_Rl, p,i,a,j) * wm_interm_29_triplet(a, i, j, q)
! term(1) = term(1) + r2p(vrdav_Rl, p,i,a,j) * wm_interm_34_triplet(a, i, j, q)
! term(2) = term(2) + r2p(vrdav_Rl, p,i,a,j) * wm_interm_34_triplet(a, j, i, q)
! term(3) = term(3) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_35_triplet(a, i, j, q)
! term(4) = term(4) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_49_triplet(a, i, j, q)
! term(5) = term(5) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_50_triplet(a, i, j, q)
! end do 
! end do 
! end do 

! term(0) = term(0) * (-6.0d+0) 
! term(1) = term(1) * (-6.0d+0) 
! term(2) = term(2) * 6.0d+0 
! term(3) = term(3) * (-6.0d+0) 
! term(4) = term(4) * 6.0d+0 
! term(5) = term(5) * (-6.0d+0) 

! do j = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! do i = 1, nocc 
! term(6) = term(6) + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,q,a,j) * wm_interm_61_triplet(b, i)
! term(7) = term(7) + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,q,a,j) * wm_interm_65_triplet(b, i)
! end do 
! end do 
! end do 
! end do 

! term(6) = term(6) * 6.0d+0 
! term(7) = term(7) * (-12.0d+0) 

! do j = 1, nocc 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! term(8) = term(8) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_35_triplet(a, i, j, q)
! term(9) = term(9) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_35_triplet(a, j, i, q)
! term(10) = term(10) + r2m(vrdav_Rl, a,i,p,j) * wm_interm_29_triplet(a, i, j, q)
! term(11) = term(11) + r2m(vrdav_Rl, a,i,p,j) * wm_interm_34_triplet(a, j, i, q)
! term(12) = term(12) + r2m(vrdav_Rl, a,i,p,j) * wm_interm_34_triplet(a, i, j, q)
! term(13) = term(13) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_49_triplet(a, i, j, q)
! term(14) = term(14) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_49_triplet(a, j, i, q)
! term(15) = term(15) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_50_triplet(a, j, i, q)
! term(16) = term(16) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_50_triplet(a, i, j, q)
! end do 
! end do 
! end do 

! term(8) = term(8) * 6.0d+0 
! term(9) = term(9) * (-6.0d+0) 
! term(10) = term(10) * 6.0d+0 
! term(11) = term(11) * (-4.0d+0) 
! term(12) = term(12) * 8.0d+0 
! term(13) = term(13) * (-4.0d+0) 
! term(14) = term(14) * 8.0d+0 
! term(15) = term(15) * (-4.0d+0) 
! term(16) = term(16) * 8.0d+0 

! do i = 1, nocc 
! do j = 1, nocc 
! do a = nocc + 1, nactive 
! term(17) = term(17) + r2m(vrdav_Rl, a,j,p,i) * wm_interm_29_triplet(a, i, j, q)
! term(18) = term(18) + r2m(vrdav_Rl, a,j,p,i) * wm_interm_34_triplet(a, i, j, q)
! term(19) = term(19) + r2m(vrdav_Rl, a,j,p,i) * wm_interm_34_triplet(a, j, i, q)
! end do 
! end do 
! end do 

! term(17) = term(17) * (-6.0d+0) 
! term(18) = term(18) * (-4.0d+0) 
! term(19) = term(19) * 8.0d+0 

! do k = 1, nocc 
! do j = 1, nocc 
! do i = 1, nocc 
! term(20) = term(20) + wm_interm_67_triplet(p, i, j, k) * wm_interm_73_triplet(k, j, i, q)
! term(21) = term(21) + wm_interm_67_triplet(p, i, j, k) * wm_interm_73_triplet(j, k, i, q)
! term(22) = term(22) + wm_interm_72_triplet(p, i, j, k) * wm_interm_73_triplet(k, j, i, q)
! term(23) = term(23) + wm_interm_72_triplet(p, i, j, k) * wm_interm_73_triplet(j, k, i, q)
! term(24) = term(24) + wm_interm_56_triplet(p, i, j, k) * wm_interm_78_triplet(k, q, i, j)
! term(25) = term(25) + wm_interm_56_triplet(p, i, j, k) * wm_interm_79_triplet(k, q, i, j)
! term(26) = term(26) + wm_interm_59_triplet(p, i, j, k) * wm_interm_81_triplet(i, q, j, k)
! term(27) = term(27) + wm_interm_59_triplet(p, i, j, k) * wm_interm_81_triplet(i, q, k, j)
! term(28) = term(28) + wm_interm_29_triplet(p, i, j, k) * wm_interm_81_triplet(i, j, k, q)
! term(29) = term(29) + wm_interm_29_triplet(p, i, j, k) * wm_interm_81_triplet(i, j, q, k)
! term(30) = term(30) + wm_interm_81_triplet(i, j, k, q) * wm_interm_83_triplet(p, i, j, k)
! term(31) = term(31) + wm_interm_81_triplet(i, j, q, k) * wm_interm_83_triplet(p, i, j, k)
! term(32) = term(32) + wm_interm_34_triplet(p, i, j, k) * wm_interm_81_triplet(j, i, q, k)
! term(33) = term(33) + wm_interm_34_triplet(p, i, j, k) * wm_interm_81_triplet(i, j, q, k)
! term(34) = term(34) + wm_interm_34_triplet(p, i, j, k) * wm_interm_81_triplet(i, j, k, q)
! term(35) = term(35) + wm_interm_34_triplet(p, i, j, k) * wm_interm_81_triplet(j, i, k, q)
! term(36) = term(36) + wm_interm_56_triplet(p, i, j, k) * wm_interm_87_triplet(q, k, i, j)
! term(37) = term(37) + wm_interm_56_triplet(p, i, j, k) * wm_interm_87_triplet(k, q, i, j)
! term(38) = term(38) + wm_interm_56_triplet(p, i, j, k) * wm_interm_88_triplet(q, k, i, j)
! term(39) = term(39) + wm_interm_56_triplet(p, i, j, k) * wm_interm_88_triplet(k, q, i, j)
! term(40) = term(40) + wm_interm_59_triplet(p, i, j, k) * wm_interm_91_triplet(q, i, j, k)
! term(41) = term(41) + wm_interm_59_triplet(p, i, j, k) * wm_interm_91_triplet(i, q, j, k)
! term(42) = term(42) + wm_interm_59_triplet(p, i, j, k) * wm_interm_91_triplet(i, q, k, j)
! term(43) = term(43) + wm_interm_59_triplet(p, i, j, k) * wm_interm_91_triplet(q, i, k, j)
! term(44) = term(44) + wm_interm_29_triplet(p, i, j, k) * wm_interm_91_triplet(i, j, k, q)
! term(45) = term(45) + wm_interm_29_triplet(p, i, j, k) * wm_interm_91_triplet(i, j, q, k)
! term(46) = term(46) + wm_interm_83_triplet(p, i, j, k) * wm_interm_91_triplet(j, i, k, q)
! term(47) = term(47) + wm_interm_83_triplet(p, i, j, k) * wm_interm_91_triplet(i, j, k, q)
! term(48) = term(48) + wm_interm_83_triplet(p, i, j, k) * wm_interm_91_triplet(i, j, q, k)
! term(49) = term(49) + wm_interm_83_triplet(p, i, j, k) * wm_interm_91_triplet(j, i, q, k)
! term(50) = term(50) + wm_interm_34_triplet(p, i, j, k) * wm_interm_91_triplet(j, i, q, k)
! term(51) = term(51) + wm_interm_34_triplet(p, i, j, k) * wm_interm_91_triplet(i, j, q, k)
! term(52) = term(52) + wm_interm_34_triplet(p, i, j, k) * wm_interm_91_triplet(i, j, k, q)
! term(53) = term(53) + wm_interm_34_triplet(p, i, j, k) * wm_interm_91_triplet(j, i, k, q)
! end do 
! end do 
! end do 

! term(20) = term(20) * (-3.0d+0) 
! term(21) = term(21) * 3.0d+0 
! term(22) = term(22) * (-4.0d+0) 
! term(23) = term(23) * 8.0d+0 
! term(24) = term(24) * (-3.0d+0) 
! term(25) = term(25) * (-6.0d+0) 
! term(26) = term(26) * 3.0d+0 
! term(27) = term(27) * (-3.0d+0) 
! term(28) = term(28) * (-3.0d+0) 
! term(29) = term(29) * 3.0d+0 
! term(30) = term(30) * 2.0d+0 
! term(31) = -term(31) 
! term(32) = -term(32) 
! term(33) = term(33) * 2.0d+0 
! term(34) = -term(34) 
! term(35) = term(35) * 2.0d+0 
! term(36) = term(36) * 3.0d+0 
! term(37) = term(37) * (-3.0d+0) 
! term(38) = term(38) * 8.0d+0 
! term(39) = term(39) * (-4.0d+0) 
! term(40) = term(40) * (-2.0d+0) 
! term(41) = term(41) * 4.0d+0 
! term(42) = term(42) * (-2.0d+0) 
! term(43) = term(43) * 4.0d+0 
! term(44) = term(44) * (-6.0d+0) 
! term(45) = term(45) * 6.0d+0 
! term(46) = term(46) * (-2.0d+0) 
! term(47) = term(47) * 4.0d+0 
! term(48) = term(48) * (-2.0d+0) 
! term(49) = term(49) * 4.0d+0 
! term(50) = term(50) * (-2.0d+0) 
! term(51) = term(51) * 4.0d+0 
! term(52) = term(52) * (-2.0d+0) 
! term(53) = term(53) * 4.0d+0 

! do k = 1, nocc 
! do i = 1, nocc 
! do j = 1, nocc 
! term(54) = term(54) + wm_interm_56_triplet(p, i, j, k) * wm_interm_78_triplet(k, q, j, i)
! term(55) = term(55) + wm_interm_56_triplet(p, i, j, k) * wm_interm_79_triplet(k, q, j, i)
! term(56) = term(56) + wm_interm_81_triplet(i, j, k, q) * wm_interm_83_triplet(p, j, i, k)
! term(57) = term(57) + wm_interm_81_triplet(i, j, q, k) * wm_interm_83_triplet(p, j, i, k)
! term(58) = term(58) + wm_interm_56_triplet(p, i, j, k) * wm_interm_87_triplet(q, k, j, i)
! term(59) = term(59) + wm_interm_56_triplet(p, i, j, k) * wm_interm_87_triplet(k, q, j, i)
! term(60) = term(60) + wm_interm_56_triplet(p, i, j, k) * wm_interm_88_triplet(q, k, j, i)
! term(61) = term(61) + wm_interm_56_triplet(p, i, j, k) * wm_interm_88_triplet(k, q, j, i)
! end do 
! end do 
! end do 

! term(54) = term(54) * 3.0d+0 
! term(55) = term(55) * 6.0d+0 
! term(56) = -term(56) 
! term(57) = term(57) * 2.0d+0 
! term(58) = term(58) * (-3.0d+0) 
! term(59) = term(59) * 3.0d+0 
! term(60) = term(60) * (-4.0d+0) 
! term(61) = term(61) * 8.0d+0 

! do a = nocc + 1, nactive 
! term(62) = term(62) + wm_interm_61_triplet(a, q) * wm_interm_62_triplet(a, p)
! term(63) = term(63) + wm_interm_12_triplet(p, a) * wm_interm_63_triplet(a, q)
! term(64) = term(64) + wm_interm_12_triplet(p, a) * wm_interm_64_triplet(a, q)
! term(65) = term(65) + wm_interm_62_triplet(a, p) * wm_interm_65_triplet(a, q)
! term(66) = term(66) + wm_interm_17_triplet(p, a) * wm_interm_63_triplet(a, q)
! term(67) = term(67) + wm_interm_20_triplet(p, a) * wm_interm_63_triplet(a, q)
! term(68) = term(68) + wm_interm_17_triplet(p, a) * wm_interm_64_triplet(a, q)
! term(69) = term(69) + wm_interm_20_triplet(p, a) * wm_interm_64_triplet(a, q)
! term(70) = term(70) + wm_interm_63_triplet(a, q) * wm_interm_75_triplet(p, a)
! term(71) = term(71) + wm_interm_63_triplet(a, q) * wm_interm_76_triplet(p, a)
! term(72) = term(72) + wm_interm_21_triplet(p, a) * wm_interm_63_triplet(a, q)
! term(73) = term(73) + wm_interm_22_triplet(p, a) * wm_interm_63_triplet(a, q)
! term(74) = term(74) + wm_interm_64_triplet(a, q) * wm_interm_75_triplet(p, a)
! term(75) = term(75) + wm_interm_64_triplet(a, q) * wm_interm_76_triplet(p, a)
! term(76) = term(76) + wm_interm_21_triplet(p, a) * wm_interm_64_triplet(a, q)
! term(77) = term(77) + wm_interm_22_triplet(p, a) * wm_interm_64_triplet(a, q)
! term(78) = term(78) + wm_interm_70_triplet(p, a) * wm_interm_7_triplet(a, q)
! term(79) = term(79) + wm_interm_71_triplet(p, a) * wm_interm_7_triplet(a, q)
! term(80) = term(80) + wm_interm_17_triplet(a, p) * wm_interm_6_triplet(a, q)
! term(81) = term(81) + wm_interm_20_triplet(a, p) * wm_interm_6_triplet(a, q)
! term(82) = term(82) + wm_interm_21_triplet(a, p) * wm_interm_6_triplet(a, q)
! term(83) = term(83) + wm_interm_22_triplet(a, p) * wm_interm_6_triplet(a, q)
! term(84) = term(84) + wm_interm_61_triplet(a, q) * wm_interm_84_triplet(a, p)
! term(85) = term(85) + wm_interm_65_triplet(a, q) * wm_interm_84_triplet(a, p)
! term(86) = term(86) + wm_interm_61_triplet(a, q) * wm_interm_85_triplet(a, p)
! term(87) = term(87) + wm_interm_61_triplet(a, q) * wm_interm_86_triplet(a, p)
! term(88) = term(88) + wm_interm_65_triplet(a, q) * wm_interm_85_triplet(a, p)
! term(89) = term(89) + wm_interm_65_triplet(a, q) * wm_interm_86_triplet(a, p)
! term(90) = term(90) + wm_interm_10_triplet(a, q) * wm_interm_70_triplet(p, a)
! term(91) = term(91) + wm_interm_10_triplet(a, q) * wm_interm_71_triplet(p, a)
! term(92) = term(92) + wm_interm_11_triplet(a, q) * wm_interm_70_triplet(p, a)
! term(93) = term(93) + wm_interm_11_triplet(a, q) * wm_interm_71_triplet(p, a)
! term(94) = term(94) + wm_interm_17_triplet(a, p) * wm_interm_8_triplet(a, q)
! term(95) = term(95) + wm_interm_17_triplet(a, p) * wm_interm_9_triplet(a, q)
! term(96) = term(96) + wm_interm_20_triplet(a, p) * wm_interm_8_triplet(a, q)
! term(97) = term(97) + wm_interm_20_triplet(a, p) * wm_interm_9_triplet(a, q)
! term(98) = term(98) + wm_interm_21_triplet(a, p) * wm_interm_8_triplet(a, q)
! term(99) = term(99) + wm_interm_22_triplet(a, p) * wm_interm_8_triplet(a, q)
! term(100) = term(100) + wm_interm_21_triplet(a, p) * wm_interm_9_triplet(a, q)
! term(101) = term(101) + wm_interm_22_triplet(a, p) * wm_interm_9_triplet(a, q)
! term(102) = term(102) + wm_interm_61_triplet(a, q) * wm_interm_92_triplet(a, p)
! term(103) = term(103) + wm_interm_61_triplet(a, q) * wm_interm_93_triplet(a, p)
! term(104) = term(104) + wm_interm_65_triplet(a, q) * wm_interm_92_triplet(a, p)
! term(105) = term(105) + wm_interm_65_triplet(a, q) * wm_interm_93_triplet(a, p)
! term(106) = term(106) + wm_interm_61_triplet(a, q) * wm_interm_94_triplet(a, p)
! term(107) = term(107) + wm_interm_61_triplet(a, q) * wm_interm_95_triplet(a, p)
! term(108) = term(108) + wm_interm_61_triplet(a, q) * wm_interm_96_triplet(a, p)
! term(109) = term(109) + wm_interm_61_triplet(a, q) * wm_interm_97_triplet(a, p)
! term(110) = term(110) + wm_interm_65_triplet(a, q) * wm_interm_94_triplet(a, p)
! term(111) = term(111) + wm_interm_65_triplet(a, q) * wm_interm_95_triplet(a, p)
! term(112) = term(112) + wm_interm_65_triplet(a, q) * wm_interm_96_triplet(a, p)
! term(113) = term(113) + wm_interm_65_triplet(a, q) * wm_interm_97_triplet(a, p)
! end do 

! term(62) = term(62) * 2.0d+0 
! term(63) = term(63) * (-4.0d+0) 
! term(64) = term(64) * 2.0d+0 
! term(65) = term(65) * (-4.0d+0) 
! term(66) = term(66) * (-6.0d+0) 
! term(67) = term(67) * 6.0d+0 
! term(68) = term(68) * 3.0d+0 
! term(69) = term(69) * (-3.0d+0) 
! term(70) = term(70) * 4.0d+0 
! term(71) = term(71) * (-8.0d+0) 
! term(72) = term(72) * 4.0d+0 
! term(73) = term(73) * (-8.0d+0) 
! term(74) = term(74) * (-2.0d+0) 
! term(75) = term(75) * 4.0d+0 
! term(76) = term(76) * (-2.0d+0) 
! term(77) = term(77) * 4.0d+0 
! term(78) = term(78) * (-12.0d+0) 
! term(79) = term(79) * 6.0d+0 
! term(80) = term(80) * 9.0d+0 
! term(81) = term(81) * (-9.0d+0) 
! term(82) = term(82) * (-12.0d+0) 
! term(83) = term(83) * 24.0d+0 
! term(84) = term(84) * 6.0d+0 
! term(85) = term(85) * (-12.0d+0) 
! term(86) = term(86) * 6.0d+0 
! term(87) = term(87) * (-6.0d+0) 
! term(88) = term(88) * (-12.0d+0) 
! term(89) = term(89) * 12.0d+0 
! term(90) = term(90) * 8.0d+0 
! term(91) = term(91) * (-4.0d+0) 
! term(92) = term(92) * (-16.0d+0) 
! term(93) = term(93) * 8.0d+0 
! term(94) = term(94) * (-12.0d+0) 
! term(95) = term(95) * 6.0d+0 
! term(96) = term(96) * 12.0d+0 
! term(97) = term(97) * (-6.0d+0) 
! term(98) = term(98) * 16.0d+0 
! term(99) = term(99) * (-32.0d+0) 
! term(100) = term(100) * (-8.0d+0) 
! term(101) = term(101) * 16.0d+0 
! term(102) = term(102) * (-6.0d+0) 
! term(103) = term(103) * 6.0d+0 
! term(104) = term(104) * 12.0d+0 
! term(105) = term(105) * (-12.0d+0) 
! term(106) = term(106) * (-4.0d+0) 
! term(107) = term(107) * 8.0d+0 
! term(108) = term(108) * (-4.0d+0) 
! term(109) = term(109) * 8.0d+0 
! term(110) = term(110) * 8.0d+0 
! term(111) = term(111) * (-16.0d+0) 
! term(112) = term(112) * 8.0d+0 
! term(113) = term(113) * (-16.0d+0) 

! do b = nocc + 1, nactive 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! term(114) = term(114) + r1(vrdav_Rl, a,i) * t2(b,p,i,q) * wm_interm_17_triplet(a, b)
! term(115) = term(115) + r1(vrdav_Rl, a,i) * t2(b,p,i,q) * wm_interm_20_triplet(a, b)
! term(116) = term(116) + r1(vrdav_Rl, a,i) * t2(b,p,i,q) * wm_interm_21_triplet(a, b)
! term(117) = term(117) + r1(vrdav_Rl, a,i) * t2(b,p,i,q) * wm_interm_22_triplet(a, b)
! term(118) = term(118) + r1(vrdav_Rr, a,i) * s2(b,p,i,q) * wm_interm_38_triplet(a, b)
! term(119) = term(119) + r1(vrdav_Rr, a,i) * s2(b,p,i,q) * wm_interm_39_triplet(a, b)
! term(120) = term(120) + r1(vrdav_Rr, a,i) * s2(b,p,i,q) * wm_interm_53_triplet(a, b)
! term(121) = term(121) + r1(vrdav_Rr, a,i) * s2(b,p,i,q) * wm_interm_54_triplet(a, b)
! end do 
! end do 
! end do 

! term(114) = term(114) * (-6.0d+0) 
! term(115) = term(115) * 6.0d+0 
! term(116) = term(116) * 8.0d+0 
! term(117) = term(117) * (-16.0d+0) 
! term(118) = term(118) * (-6.0d+0) 
! term(119) = term(119) * 6.0d+0 
! term(120) = term(120) * 8.0d+0 
! term(121) = term(121) * (-16.0d+0) 

! do j = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! do k = 1, nocc 
! do i = 1, nocc 
! term(122) = term(122) + r2p(vrdav_Rr, p,i,a,j) * s2(a,b,k,j) * wm_interm_66_triplet(b, i, k, q)
! end do 
! end do 
! end do 
! end do 
! end do 

! term(122) = term(122) * (-6.0d+0) 

! do k = 1, nocc 
! do j = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! do i = 1, nocc 
! term(123) = term(123) + r2p(vrdav_Rr, p,i,a,j) * s2(a,b,j,k) * wm_interm_66_triplet(b, i, k, q)
! term(124) = term(124) + r2p(vrdav_Rr, p,i,a,j) * s2(a,b,j,k) * wm_interm_66_triplet(b, i, q, k)
! term(125) = term(125) + r2p(vrdav_Rr, p,i,a,j) * s2(a,b,k,j) * wm_interm_66_triplet(b, i, q, k)
! term(126) = term(126) + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,q,a,k) * wm_interm_82_triplet(b, k, i, j)
! end do 
! end do 
! end do 
! end do 
! end do 

! term(123) = term(123) * 6.0d+0 
! term(124) = term(124) * (-12.0d+0) 
! term(125) = term(125) * 6.0d+0 
! term(126) = term(126) * 6.0d+0 

! do j = 1, nocc 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do k = 1, nocc 
! do a = nocc + 1, nactive 
! term(127) = term(127) + r2m(vrdav_Rr, a,i,p,j) * s2(a,b,k,i) * wm_interm_66_triplet(b, j, k, q)
! term(128) = term(128) + r2m(vrdav_Rr, a,j,p,i) * s2(a,b,k,j) * wm_interm_66_triplet(b, i, k, q)
! term(129) = term(129) + s2(a,p,j,i) * t2(a,b,k,i) * wm_interm_80_triplet(b, k, q, j)
! term(130) = term(130) + s2(a,p,j,i) * t2(a,b,k,j) * wm_interm_80_triplet(b, k, q, i)
! term(131) = term(131) + r2p(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,k,p,q) * wm_interm_82_triplet(a, k, j, i)
! term(132) = term(132) + r2p(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,k,b,j) * wm_interm_82_triplet(a, k, q, i)
! term(133) = term(133) + r2p(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,k,p,j) * wm_interm_82_triplet(a, k, q, i)
! term(134) = term(134) + s2(a,p,j,i) * t2(a,b,k,i) * wm_interm_90_triplet(b, q, k, j)
! term(135) = term(135) + s2(a,p,j,i) * t2(a,b,k,j) * wm_interm_90_triplet(b, q, k, i)
! term(136) = term(136) + s2(a,p,j,i) * t2(a,b,k,i) * wm_interm_89_triplet(b, k, q, j)
! term(137) = term(137) + s2(a,p,j,i) * t2(a,b,k,i) * wm_interm_90_triplet(b, k, q, j)
! term(138) = term(138) + s2(a,p,j,i) * t2(a,b,k,j) * wm_interm_89_triplet(b, k, q, i)
! term(139) = term(139) + s2(a,p,j,i) * t2(a,b,k,j) * wm_interm_90_triplet(b, k, q, i)
! term(140) = term(140) + s2(a,p,j,i) * t2(a,b,k,i) * wm_interm_89_triplet(b, q, k, j)
! term(141) = term(141) + s2(a,p,j,i) * t2(a,b,k,j) * wm_interm_89_triplet(b, q, k, i)
! term(142) = term(142) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,k,p,q) * wm_interm_82_triplet(a, k, j, i)
! term(143) = term(143) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,k,p,q) * wm_interm_82_triplet(b, k, j, i)
! term(144) = term(144) + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,k,b,i) * wm_interm_82_triplet(a, k, j, q)
! term(145) = term(145) + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,k,b,j) * wm_interm_82_triplet(a, k, q, i)
! term(146) = term(146) + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,k,b,i) * wm_interm_82_triplet(a, k, q, j)
! term(147) = term(147) + r2m(vrdav_Rl, a,k,b,j) * t2(a,p,q,i) * wm_interm_29_triplet(b, j, k, i)
! term(148) = term(148) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,k,p,i) * wm_interm_82_triplet(a, k, j, q)
! term(149) = term(149) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,k,p,i) * wm_interm_82_triplet(b, k, j, q)
! term(150) = term(150) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,k,p,j) * wm_interm_82_triplet(a, k, q, i)
! term(151) = term(151) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,k,p,j) * wm_interm_82_triplet(b, k, q, i)
! term(152) = term(152) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,k,p,i) * wm_interm_82_triplet(a, k, q, j)
! term(153) = term(153) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,k,p,i) * wm_interm_82_triplet(b, k, q, j)
! term(154) = term(154) + r2m(vrdav_Rl, a,k,b,j) * t2(a,p,q,i) * wm_interm_34_triplet(b, k, j, i)
! term(155) = term(155) + r2m(vrdav_Rl, a,k,b,j) * t2(a,p,q,i) * wm_interm_34_triplet(b, j, k, i)
! end do 
! end do 
! end do 
! end do 
! end do 

! term(127) = term(127) * (-2.0d+0) 
! term(128) = term(128) * (-2.0d+0) 
! term(129) = term(129) * 6.0d+0 
! term(130) = term(130) * (-6.0d+0) 
! term(131) = term(131) * 12.0d+0 
! term(132) = term(132) * 12.0d+0 
! term(133) = term(133) * (-12.0d+0) 
! term(134) = term(134) * (-2.0d+0) 
! term(135) = term(135) * 4.0d+0 
! term(136) = term(136) * (-2.0d+0) 
! term(137) = term(137) * 4.0d+0 
! term(138) = term(138) * 4.0d+0 
! term(139) = term(139) * (-2.0d+0) 
! term(140) = term(140) * 4.0d+0 
! term(141) = term(141) * (-2.0d+0) 
! term(142) = term(142) * 8.0d+0 
! term(143) = term(143) * (-16.0d+0) 
! term(144) = term(144) * 6.0d+0 
! term(145) = term(145) * 6.0d+0 
! term(146) = term(146) * (-6.0d+0) 
! term(147) = term(147) * 6.0d+0 
! term(148) = term(148) * (-4.0d+0) 
! term(149) = term(149) * 8.0d+0 
! term(150) = term(150) * (-4.0d+0) 
! term(151) = term(151) * 8.0d+0 
! term(152) = term(152) * 8.0d+0 
! term(153) = term(153) * (-4.0d+0) 
! term(154) = term(154) * (-4.0d+0) 
! term(155) = term(155) * 8.0d+0 

! do j = 1, nocc 
! do b = nocc + 1, nactive 
! do k = 1, nocc 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! term(156) = term(156) + r2m(vrdav_Rr, a,i,p,j) * s2(a,b,k,j) * wm_interm_66_triplet(b, i, k, q)
! end do 
! end do 
! end do 
! end do 
! end do 

! term(156) = term(156) * 4.0d+0 

! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do k = 1, nocc 
! do j = 1, nocc 
! do a = nocc + 1, nactive 
! term(157) = term(157) + r2m(vrdav_Rr, a,j,p,i) * s2(a,b,k,i) * wm_interm_66_triplet(b, j, k, q)
! end do 
! end do 
! end do 
! end do 
! end do 

! term(157) = term(157) * 4.0d+0 

! do k = 1, nocc 
! do j = 1, nocc 
! do b = nocc + 1, nactive 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! term(158) = term(158) + r2m(vrdav_Rr, a,i,p,j) * s2(a,b,j,k) * wm_interm_66_triplet(b, i, k, q)
! term(159) = term(159) + r2m(vrdav_Rr, a,i,p,j) * s2(a,b,i,k) * wm_interm_66_triplet(b, j, k, q)
! term(160) = term(160) + r2m(vrdav_Rr, a,i,p,j) * s2(a,b,j,k) * wm_interm_66_triplet(b, i, q, k)
! term(161) = term(161) + r2m(vrdav_Rr, a,i,p,j) * s2(a,b,i,k) * wm_interm_66_triplet(b, j, q, k)
! term(162) = term(162) + r2m(vrdav_Rr, a,i,p,j) * s2(a,b,k,j) * wm_interm_66_triplet(b, i, q, k)
! term(163) = term(163) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,k,j) * wm_interm_35_triplet(a, i, q, k)
! term(164) = term(164) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,j,k) * wm_interm_35_triplet(a, i, q, k)
! term(165) = term(165) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,k,q) * wm_interm_35_triplet(a, i, j, k)
! term(166) = term(166) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_35_triplet(a, i, j, k)
! term(167) = term(167) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,k,j) * wm_interm_35_triplet(a, i, q, k)
! term(168) = term(168) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,i,k) * wm_interm_35_triplet(a, j, q, k)
! term(169) = term(169) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,j,k) * wm_interm_35_triplet(a, i, q, k)
! term(170) = term(170) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,k,q) * wm_interm_35_triplet(a, j, i, k)
! term(171) = term(171) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,k,q) * wm_interm_35_triplet(a, i, j, k)
! term(172) = term(172) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_35_triplet(a, j, i, k)
! term(173) = term(173) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_35_triplet(a, i, j, k)
! term(174) = term(174) + r2p(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,q,b,k) * wm_interm_82_triplet(a, k, i, j)
! term(175) = term(175) + r2p(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,q,p,k) * wm_interm_82_triplet(a, k, i, j)
! term(176) = term(176) + r2p(vrdav_Rl, a,i,b,j) * t2(b,p,k,q) * wm_interm_29_triplet(a, i, j, k)
! term(177) = term(177) + r2p(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,j,p,k) * wm_interm_82_triplet(a, k, i, q)
! term(178) = term(178) + r2p(vrdav_Rl, a,i,b,j) * t2(b,p,k,q) * wm_interm_34_triplet(a, i, j, k)
! term(179) = term(179) + r2p(vrdav_Rl, a,i,b,j) * t2(b,p,k,q) * wm_interm_34_triplet(a, j, i, k)
! term(180) = term(180) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,k,j) * wm_interm_50_triplet(a, q, i, k)
! term(181) = term(181) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,j,k) * wm_interm_50_triplet(a, q, i, k)
! term(182) = term(182) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,k,j) * wm_interm_50_triplet(a, i, q, k)
! term(183) = term(183) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,k,q) * wm_interm_50_triplet(a, i, j, k)
! term(184) = term(184) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_50_triplet(a, i, j, k)
! term(185) = term(185) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,j,k) * wm_interm_50_triplet(a, i, q, k)
! term(186) = term(186) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,k,j) * wm_interm_49_triplet(a, q, i, k)
! term(187) = term(187) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,k,j) * wm_interm_49_triplet(a, i, q, k)
! term(188) = term(188) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,k,q) * wm_interm_49_triplet(a, i, j, k)
! term(189) = term(189) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_49_triplet(a, i, j, k)
! term(190) = term(190) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,j,k) * wm_interm_49_triplet(a, i, q, k)
! term(191) = term(191) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,j,k) * wm_interm_49_triplet(a, q, i, k)
! term(192) = term(192) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,k,j) * wm_interm_50_triplet(a, q, i, k)
! term(193) = term(193) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,i,k) * wm_interm_50_triplet(a, q, j, k)
! term(194) = term(194) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,j,k) * wm_interm_50_triplet(a, q, i, k)
! term(195) = term(195) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,k,j) * wm_interm_50_triplet(a, i, q, k)
! term(196) = term(196) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,k,q) * wm_interm_50_triplet(a, j, i, k)
! term(197) = term(197) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,k,q) * wm_interm_50_triplet(a, i, j, k)
! term(198) = term(198) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_50_triplet(a, j, i, k)
! term(199) = term(199) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_50_triplet(a, i, j, k)
! term(200) = term(200) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,i,k) * wm_interm_50_triplet(a, j, q, k)
! term(201) = term(201) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,j,k) * wm_interm_50_triplet(a, i, q, k)
! term(202) = term(202) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,k,j) * wm_interm_49_triplet(a, q, i, k)
! term(203) = term(203) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,k,j) * wm_interm_49_triplet(a, i, q, k)
! term(204) = term(204) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,k,q) * wm_interm_49_triplet(a, i, j, k)
! term(205) = term(205) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,k,q) * wm_interm_49_triplet(a, j, i, k)
! term(206) = term(206) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_49_triplet(a, i, j, k)
! term(207) = term(207) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_49_triplet(a, j, i, k)
! term(208) = term(208) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,i,k) * wm_interm_49_triplet(a, j, q, k)
! term(209) = term(209) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,j,k) * wm_interm_49_triplet(a, i, q, k)
! term(210) = term(210) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,i,k) * wm_interm_49_triplet(a, q, j, k)
! term(211) = term(211) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,j,k) * wm_interm_49_triplet(a, q, i, k)
! term(212) = term(212) + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,q,b,k) * wm_interm_82_triplet(a, k, i, j)
! term(213) = term(213) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,q,p,k) * wm_interm_82_triplet(b, k, i, j)
! term(214) = term(214) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,q,p,k) * wm_interm_82_triplet(a, k, i, j)
! term(215) = term(215) + r2m(vrdav_Rl, a,i,b,j) * t2(a,p,k,q) * wm_interm_29_triplet(b, i, j, k)
! term(216) = term(216) + r2m(vrdav_Rl, a,i,b,j) * t2(b,p,k,q) * wm_interm_29_triplet(a, i, j, k)
! term(217) = term(217) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,i,p,k) * wm_interm_82_triplet(a, k, j, q)
! term(218) = term(218) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,j,p,k) * wm_interm_82_triplet(a, k, i, q)
! term(219) = term(219) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,j,p,k) * wm_interm_82_triplet(b, k, i, q)
! term(220) = term(220) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,i,p,k) * wm_interm_82_triplet(b, k, j, q)
! term(221) = term(221) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,i,p,k) * wm_interm_82_triplet(a, k, q, j)
! term(222) = term(222) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,i,p,k) * wm_interm_82_triplet(b, k, q, j)
! term(223) = term(223) + r2m(vrdav_Rl, a,i,b,j) * t2(a,p,k,q) * wm_interm_34_triplet(b, i, j, k)
! term(224) = term(224) + r2m(vrdav_Rl, a,i,b,j) * t2(a,p,k,q) * wm_interm_34_triplet(b, j, i, k)
! term(225) = term(225) + r2m(vrdav_Rl, a,i,b,j) * t2(b,p,k,q) * wm_interm_34_triplet(a, j, i, k)
! term(226) = term(226) + r2m(vrdav_Rl, a,i,b,j) * t2(b,p,k,q) * wm_interm_34_triplet(a, i, j, k)
! end do 
! end do 
! end do 
! end do 
! end do 

! term(158) = term(158) * (-2.0d+0) 
! term(159) = term(159) * 4.0d+0 
! term(160) = term(160) * 4.0d+0 
! term(161) = term(161) * (-8.0d+0) 
! term(162) = term(162) * (-2.0d+0) 
! term(163) = term(163) * 12.0d+0 
! term(164) = term(164) * (-18.0d+0) 
! term(165) = term(165) * (-12.0d+0) 
! term(166) = term(166) * 6.0d+0 
! term(167) = term(167) * 12.0d+0 
! term(168) = term(168) * 12.0d+0 
! term(169) = term(169) * (-24.0d+0) 
! term(170) = term(170) * 12.0d+0 
! term(171) = term(171) * (-12.0d+0) 
! term(172) = term(172) * (-6.0d+0) 
! term(173) = term(173) * 6.0d+0 
! term(174) = term(174) * (-9.0d+0) 
! term(175) = term(175) * 6.0d+0 
! term(176) = term(176) * (-12.0d+0) 
! term(177) = term(177) * (-24.0d+0) 
! term(178) = term(178) * (-12.0d+0) 
! term(179) = term(179) * 12.0d+0 
! term(180) = term(180) * (-6.0d+0) 
! term(181) = term(181) * 6.0d+0 
! term(182) = term(182) * 6.0d+0 
! term(183) = term(183) * (-12.0d+0) 
! term(184) = term(184) * 6.0d+0 
! term(185) = term(185) * (-12.0d+0) 
! term(186) = term(186) * 6.0d+0 
! term(187) = term(187) * (-6.0d+0) 
! term(188) = term(188) * 12.0d+0 
! term(189) = term(189) * (-6.0d+0) 
! term(190) = term(190) * 6.0d+0 
! term(191) = term(191) * (-12.0d+0) 
! term(192) = term(192) * (-4.0d+0) 
! term(193) = term(193) * (-4.0d+0) 
! term(194) = term(194) * 8.0d+0 
! term(195) = term(195) * 8.0d+0 
! term(196) = term(196) * 8.0d+0 
! term(197) = term(197) * (-16.0d+0) 
! term(198) = term(198) * (-4.0d+0) 
! term(199) = term(199) * 8.0d+0 
! term(200) = term(200) * 8.0d+0 
! term(201) = term(201) * (-16.0d+0) 
! term(202) = term(202) * 8.0d+0 
! term(203) = term(203) * (-4.0d+0) 
! term(204) = term(204) * 8.0d+0 
! term(205) = term(205) * (-16.0d+0) 
! term(206) = term(206) * (-4.0d+0) 
! term(207) = term(207) * 8.0d+0 
! term(208) = term(208) * (-4.0d+0) 
! term(209) = term(209) * 8.0d+0 
! term(210) = term(210) * 8.0d+0 
! term(211) = term(211) * (-16.0d+0) 
! term(212) = term(212) * (-12.0d+0) 
! term(213) = term(213) * (-4.0d+0) 
! term(214) = term(214) * 8.0d+0 
! term(215) = term(215) * 12.0d+0 
! term(216) = term(216) * (-12.0d+0) 
! term(217) = term(217) * 8.0d+0 
! term(218) = term(218) * (-16.0d+0) 
! term(219) = term(219) * 8.0d+0 
! term(220) = term(220) * (-16.0d+0) 
! term(221) = term(221) * (-4.0d+0) 
! term(222) = term(222) * 8.0d+0 
! term(223) = term(223) * 8.0d+0 
! term(224) = term(224) * (-16.0d+0) 
! term(225) = term(225) * 8.0d+0 
! term(226) = term(226) * (-16.0d+0) 

! do j = 1, nocc 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! term(227) = term(227) + r2p(vrdav_Rr, p,q,a,i) * s2(a,b,i,j) * wm_interm_64_triplet(b, j)
! term(228) = term(228) + r2p(vrdav_Rr, p,q,a,i) * s2(a,b,i,j) * wm_interm_63_triplet(b, j)
! term(229) = term(229) + r2m(vrdav_Rr, a,q,p,i) * s2(a,b,i,j) * wm_interm_64_triplet(b, j)
! term(230) = term(230) + r2m(vrdav_Rr, a,q,p,i) * s2(a,b,i,j) * wm_interm_63_triplet(b, j)
! term(231) = term(231) + s2(a,b,i,j) * t2(a,p,q,i) * wm_interm_3_triplet(b, j)
! term(232) = term(232) + s2(a,b,i,j) * t2(a,p,q,i) * wm_interm_4_triplet(b, j)
! term(233) = term(233) + s2(a,b,i,j) * t2(a,p,q,i) * wm_interm_5_triplet(b, j)
! term(234) = term(234) + r2p(vrdav_Rl, p,q,a,i) * t2(a,b,i,j) * wm_interm_58_triplet(b, j)
! term(235) = term(235) + r2p(vrdav_Rl, p,q,a,i) * t2(a,b,i,j) * wm_interm_57_triplet(b, j)
! term(236) = term(236) + s2(a,p,q,i) * t2(a,b,i,j) * wm_interm_7_triplet(b, j)
! term(237) = term(237) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,q,i) * wm_interm_6_triplet(a, j)
! term(238) = term(238) + s2(a,p,q,i) * t2(a,b,i,j) * wm_interm_11_triplet(b, j)
! term(239) = term(239) + r2m(vrdav_Rl, a,q,p,i) * t2(a,b,i,j) * wm_interm_58_triplet(b, j)
! term(240) = term(240) + s2(a,p,q,i) * t2(a,b,i,j) * wm_interm_10_triplet(b, j)
! term(241) = term(241) + r2m(vrdav_Rl, a,q,p,i) * t2(a,b,i,j) * wm_interm_57_triplet(b, j)
! term(242) = term(242) + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,q,a,i) * wm_interm_61_triplet(b, j)
! term(243) = term(243) + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,q,a,i) * wm_interm_65_triplet(b, j)
! term(244) = term(244) + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,q,b,i) * wm_interm_61_triplet(a, j)
! term(245) = term(245) + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,q,b,i) * wm_interm_65_triplet(a, j)
! term(246) = term(246) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,q,p,i) * wm_interm_61_triplet(b, j)
! term(247) = term(247) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,q,p,i) * wm_interm_65_triplet(b, j)
! term(248) = term(248) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,q,p,i) * wm_interm_61_triplet(a, j)
! term(249) = term(249) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,q,p,i) * wm_interm_65_triplet(a, j)
! term(250) = term(250) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,q,i) * wm_interm_8_triplet(a, j)
! term(251) = term(251) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,q,i) * wm_interm_9_triplet(a, j)
! term(252) = term(252) + r2m(vrdav_Rl, a,i,b,j) * t2(a,p,q,i) * wm_interm_0_triplet(b, j)
! term(253) = term(253) + r2m(vrdav_Rl, a,i,b,j) * t2(a,p,q,i) * wm_interm_1_triplet(b, j)
! term(254) = term(254) + r2m(vrdav_Rl, a,i,b,j) * t2(a,p,q,i) * wm_interm_2_triplet(b, j)
! end do 
! end do 
! end do 
! end do 

! term(227) = term(227) * (-12.0d+0) 
! term(228) = term(228) * 24.0d+0 
! term(229) = term(229) * 8.0d+0 
! term(230) = term(230) * (-16.0d+0) 
! term(231) = term(231) * (-12.0d+0) 
! term(232) = term(232) * 8.0d+0 
! term(233) = term(233) * (-16.0d+0) 
! term(234) = term(234) * 24.0d+0 
! term(235) = term(235) * (-12.0d+0) 
! term(236) = term(236) * (-12.0d+0) 
! term(237) = term(237) * 12.0d+0 
! term(238) = term(238) * (-16.0d+0) 
! term(239) = term(239) * (-16.0d+0) 
! term(240) = term(240) * 8.0d+0 
! term(241) = term(241) * 8.0d+0 
! term(242) = term(242) * (-12.0d+0) 
! term(243) = term(243) * 24.0d+0 
! term(244) = term(244) * 6.0d+0 
! term(245) = term(245) * (-12.0d+0) 
! term(246) = term(246) * 8.0d+0 
! term(247) = term(247) * (-16.0d+0) 
! term(248) = term(248) * (-4.0d+0) 
! term(249) = term(249) * 8.0d+0 
! term(250) = term(250) * 16.0d+0 
! term(251) = term(251) * (-8.0d+0) 
! term(252) = term(252) * 24.0d+0 
! term(253) = term(253) * 32.0d+0 
! term(254) = term(254) * (-16.0d+0) 

! do k = 1, nocc 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do j = 1, nocc 
! do a = nocc + 1, nactive 
! term(255) = term(255) + r2m(vrdav_Rr, a,j,p,i) * s2(a,b,i,k) * wm_interm_66_triplet(b, j, k, q)
! term(256) = term(256) + r2m(vrdav_Rr, a,j,p,i) * s2(a,b,j,k) * wm_interm_66_triplet(b, i, k, q)
! term(257) = term(257) + r2m(vrdav_Rr, a,j,p,i) * s2(a,b,i,k) * wm_interm_66_triplet(b, j, q, k)
! term(258) = term(258) + r2m(vrdav_Rr, a,j,p,i) * s2(a,b,j,k) * wm_interm_66_triplet(b, i, q, k)
! term(259) = term(259) + r2m(vrdav_Rr, a,j,p,i) * s2(a,b,k,i) * wm_interm_66_triplet(b, j, q, k)
! term(260) = term(260) + s2(a,p,j,i) * t2(a,b,j,k) * wm_interm_80_triplet(b, k, q, i)
! term(261) = term(261) + r2m(vrdav_Rl, a,i,p,q) * r2m(vrdav_Rr, a,j,b,k) * wm_interm_56_triplet(b, j, k, i)
! term(262) = term(262) + r2m(vrdav_Rl, a,i,p,q) * r2m(vrdav_Rr, a,j,b,k) * wm_interm_56_triplet(b, k, j, i)
! term(263) = term(263) + r2m(vrdav_Rl, a,q,p,i) * r2m(vrdav_Rr, a,j,b,k) * wm_interm_56_triplet(b, j, k, i)
! term(264) = term(264) + r2m(vrdav_Rl, a,q,p,i) * r2m(vrdav_Rr, a,j,b,k) * wm_interm_56_triplet(b, k, j, i)
! term(265) = term(265) + s2(a,p,j,i) * t2(a,b,j,k) * wm_interm_90_triplet(b, q, k, i)
! term(266) = term(266) + s2(a,p,j,i) * t2(a,b,j,k) * wm_interm_90_triplet(b, k, q, i)
! term(267) = term(267) + s2(a,p,j,i) * t2(a,b,j,k) * wm_interm_89_triplet(b, q, k, i)
! term(268) = term(268) + s2(a,p,j,i) * t2(a,b,j,k) * wm_interm_89_triplet(b, k, q, i)
! term(269) = term(269) + r2m(vrdav_Rl, a,j,b,k) * t2(a,p,q,i) * wm_interm_29_triplet(b, j, k, i)
! term(270) = term(270) + r2m(vrdav_Rl, a,j,b,k) * t2(a,p,q,i) * wm_interm_34_triplet(b, j, k, i)
! term(271) = term(271) + r2m(vrdav_Rl, a,j,b,k) * t2(a,p,q,i) * wm_interm_34_triplet(b, k, j, i)
! end do 
! end do 
! end do 
! end do 
! end do 

! term(255) = term(255) * (-2.0d+0) 
! term(256) = term(256) * 4.0d+0 
! term(257) = term(257) * 4.0d+0 
! term(258) = term(258) * (-8.0d+0) 
! term(259) = term(259) * (-2.0d+0) 
! term(260) = term(260) * 12.0d+0 
! term(261) = term(261) * 16.0d+0 
! term(262) = term(262) * (-32.0d+0) 
! term(263) = term(263) * (-8.0d+0) 
! term(264) = term(264) * 16.0d+0 
! term(265) = term(265) * (-8.0d+0) 
! term(266) = term(266) * 4.0d+0 
! term(267) = term(267) * 4.0d+0 
! term(268) = term(268) * (-8.0d+0) 
! term(269) = term(269) * (-6.0d+0) 
! term(270) = term(270) * (-4.0d+0) 
! term(271) = term(271) * 8.0d+0 

! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do j = 1, nocc 
! do a = nocc + 1, nactive 
! term(272) = term(272) + r2m(vrdav_Rr, a,q,p,i) * s2(a,b,j,i) * wm_interm_64_triplet(b, j)
! term(273) = term(273) + r2m(vrdav_Rr, a,i,p,q) * s2(a,b,j,i) * wm_interm_64_triplet(b, j)
! term(274) = term(274) + r2m(vrdav_Rr, a,q,p,i) * s2(a,b,j,i) * wm_interm_63_triplet(b, j)
! term(275) = term(275) + r2m(vrdav_Rr, a,i,p,q) * s2(a,b,j,i) * wm_interm_63_triplet(b, j)
! term(276) = term(276) + s2(a,b,j,i) * t2(a,p,q,i) * wm_interm_3_triplet(b, j)
! term(277) = term(277) + s2(a,b,j,i) * t2(a,p,q,i) * wm_interm_4_triplet(b, j)
! term(278) = term(278) + s2(a,b,j,i) * t2(a,p,q,i) * wm_interm_5_triplet(b, j)
! term(279) = term(279) + s2(a,p,q,i) * t2(a,b,j,i) * wm_interm_7_triplet(b, j)
! term(280) = term(280) + s2(a,p,i,q) * t2(a,b,j,i) * wm_interm_7_triplet(b, j)
! term(281) = term(281) + r2m(vrdav_Rl, a,i,p,q) * t2(a,b,j,i) * wm_interm_57_triplet(b, j)
! term(282) = term(282) + r2m(vrdav_Rl, a,i,p,q) * t2(a,b,j,i) * wm_interm_58_triplet(b, j)
! term(283) = term(283) + s2(a,p,q,i) * t2(a,b,j,i) * wm_interm_11_triplet(b, j)
! term(284) = term(284) + s2(a,p,i,q) * t2(a,b,j,i) * wm_interm_11_triplet(b, j)
! term(285) = term(285) + s2(a,p,q,i) * t2(a,b,j,i) * wm_interm_10_triplet(b, j)
! term(286) = term(286) + r2m(vrdav_Rl, a,q,p,i) * t2(a,b,j,i) * wm_interm_57_triplet(b, j)
! term(287) = term(287) + s2(a,p,i,q) * t2(a,b,j,i) * wm_interm_10_triplet(b, j)
! term(288) = term(288) + r2m(vrdav_Rl, a,q,p,i) * t2(a,b,j,i) * wm_interm_58_triplet(b, j)
! term(289) = term(289) + r2m(vrdav_Rl, a,j,b,i) * t2(a,p,q,i) * wm_interm_0_triplet(b, j)
! term(290) = term(290) + r2m(vrdav_Rl, a,j,b,i) * t2(a,p,q,i) * wm_interm_1_triplet(b, j)
! term(291) = term(291) + r2m(vrdav_Rl, a,j,b,i) * t2(a,p,q,i) * wm_interm_2_triplet(b, j)
! end do 
! end do 
! end do 
! end do 

! term(272) = term(272) * (-4.0d+0) 
! term(273) = term(273) * 8.0d+0 
! term(274) = term(274) * 8.0d+0 
! term(275) = term(275) * (-16.0d+0) 
! term(276) = term(276) * 6.0d+0 
! term(277) = term(277) * (-4.0d+0) 
! term(278) = term(278) * 8.0d+0 
! term(279) = term(279) * 6.0d+0 
! term(280) = term(280) * (-12.0d+0) 
! term(281) = term(281) * 8.0d+0 
! term(282) = term(282) * (-16.0d+0) 
! term(283) = term(283) * 8.0d+0 
! term(284) = term(284) * (-16.0d+0) 
! term(285) = term(285) * (-4.0d+0) 
! term(286) = term(286) * (-4.0d+0) 
! term(287) = term(287) * 8.0d+0 
! term(288) = term(288) * 8.0d+0 
! term(289) = term(289) * (-12.0d+0) 
! term(290) = term(290) * (-16.0d+0) 
! term(291) = term(291) * 8.0d+0 

! do j = 1, nocc 
! do b = nocc + 1, nactive 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! term(292) = term(292) + r2m(vrdav_Rr, a,i,p,q) * s2(a,b,i,j) * wm_interm_64_triplet(b, j)
! term(293) = term(293) + r2m(vrdav_Rr, a,i,p,q) * s2(a,b,i,j) * wm_interm_63_triplet(b, j)
! term(294) = term(294) + s2(a,b,i,j) * t2(b,p,j,q) * wm_interm_3_triplet(a, i)
! term(295) = term(295) + s2(a,b,i,j) * t2(b,p,i,q) * wm_interm_3_triplet(a, j)
! term(296) = term(296) + s2(a,b,i,j) * t2(b,p,j,q) * wm_interm_4_triplet(a, i)
! term(297) = term(297) + s2(a,b,i,j) * t2(b,p,j,q) * wm_interm_5_triplet(a, i)
! term(298) = term(298) + s2(a,b,i,j) * t2(b,p,i,q) * wm_interm_4_triplet(a, j)
! term(299) = term(299) + s2(a,b,i,j) * t2(b,p,i,q) * wm_interm_5_triplet(a, j)
! term(300) = term(300) + s2(a,p,i,q) * t2(a,b,i,j) * wm_interm_7_triplet(b, j)
! term(301) = term(301) + r2p(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,q,b,j) * wm_interm_61_triplet(a, i)
! term(302) = term(302) + r2p(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,q,b,j) * wm_interm_65_triplet(a, i)
! term(303) = term(303) + r2p(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,q,p,j) * wm_interm_61_triplet(a, i)
! term(304) = term(304) + r2p(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,j,p,q) * wm_interm_61_triplet(a, i)
! term(305) = term(305) + r2p(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,q,p,j) * wm_interm_65_triplet(a, i)
! term(306) = term(306) + r2p(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,j,p,q) * wm_interm_65_triplet(a, i)
! term(307) = term(307) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,j,q) * wm_interm_6_triplet(a, i)
! term(308) = term(308) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,q,j) * wm_interm_6_triplet(a, i)
! term(309) = term(309) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,i,q) * wm_interm_6_triplet(a, j)
! term(310) = term(310) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,j,q) * wm_interm_6_triplet(a, i)
! term(311) = term(311) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,q,j) * wm_interm_6_triplet(a, i)
! term(312) = term(312) + r2p(vrdav_Rl, a,i,b,j) * t2(b,p,j,q) * wm_interm_0_triplet(a, i)
! term(313) = term(313) + r2p(vrdav_Rl, a,i,b,j) * t2(b,p,j,q) * wm_interm_1_triplet(a, i)
! term(314) = term(314) + r2p(vrdav_Rl, a,i,b,j) * t2(b,p,j,q) * wm_interm_2_triplet(a, i)
! term(315) = term(315) + r2m(vrdav_Rl, a,i,p,q) * t2(a,b,i,j) * wm_interm_58_triplet(b, j)
! term(316) = term(316) + s2(a,p,i,q) * t2(a,b,i,j) * wm_interm_11_triplet(b, j)
! term(317) = term(317) + r2m(vrdav_Rl, a,i,p,q) * t2(a,b,i,j) * wm_interm_57_triplet(b, j)
! term(318) = term(318) + s2(a,p,i,q) * t2(a,b,i,j) * wm_interm_10_triplet(b, j)
! term(319) = term(319) + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,q,b,j) * wm_interm_61_triplet(a, i)
! term(320) = term(320) + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,q,b,j) * wm_interm_65_triplet(a, i)
! term(321) = term(321) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,i,p,q) * wm_interm_61_triplet(b, j)
! term(322) = term(322) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,i,p,q) * wm_interm_65_triplet(b, j)
! term(323) = term(323) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,q,p,j) * wm_interm_61_triplet(a, i)
! term(324) = term(324) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,j,p,q) * wm_interm_61_triplet(a, i)
! term(325) = term(325) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,q,p,j) * wm_interm_61_triplet(b, i)
! term(326) = term(326) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,j,p,q) * wm_interm_61_triplet(b, i)
! term(327) = term(327) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,q,p,j) * wm_interm_65_triplet(b, i)
! term(328) = term(328) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,j,p,q) * wm_interm_65_triplet(b, i)
! term(329) = term(329) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,i,p,q) * wm_interm_61_triplet(a, j)
! term(330) = term(330) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,i,p,q) * wm_interm_65_triplet(a, j)
! term(331) = term(331) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,q,p,j) * wm_interm_65_triplet(a, i)
! term(332) = term(332) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,j,p,q) * wm_interm_65_triplet(a, i)
! term(333) = term(333) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,j,q) * wm_interm_8_triplet(a, i)
! term(334) = term(334) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,j,q) * wm_interm_9_triplet(a, i)
! term(335) = term(335) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,q,j) * wm_interm_8_triplet(a, i)
! term(336) = term(336) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,q,j) * wm_interm_9_triplet(a, i)
! term(337) = term(337) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,i,q) * wm_interm_8_triplet(a, j)
! term(338) = term(338) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,j,q) * wm_interm_8_triplet(a, i)
! term(339) = term(339) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,i,q) * wm_interm_9_triplet(a, j)
! term(340) = term(340) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,j,q) * wm_interm_9_triplet(a, i)
! term(341) = term(341) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,q,j) * wm_interm_8_triplet(a, i)
! term(342) = term(342) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,q,j) * wm_interm_9_triplet(a, i)
! term(343) = term(343) + r2m(vrdav_Rl, a,i,b,j) * t2(a,p,j,q) * wm_interm_0_triplet(b, i)
! term(344) = term(344) + r2m(vrdav_Rl, a,i,b,j) * t2(a,p,i,q) * wm_interm_0_triplet(b, j)
! term(345) = term(345) + r2m(vrdav_Rl, a,i,b,j) * t2(b,p,i,q) * wm_interm_0_triplet(a, j)
! term(346) = term(346) + r2m(vrdav_Rl, a,i,b,j) * t2(b,p,j,q) * wm_interm_0_triplet(a, i)
! term(347) = term(347) + r2m(vrdav_Rl, a,i,b,j) * t2(a,p,j,q) * wm_interm_1_triplet(b, i)
! term(348) = term(348) + r2m(vrdav_Rl, a,i,b,j) * t2(a,p,i,q) * wm_interm_1_triplet(b, j)
! term(349) = term(349) + r2m(vrdav_Rl, a,i,b,j) * t2(b,p,i,q) * wm_interm_1_triplet(a, j)
! term(350) = term(350) + r2m(vrdav_Rl, a,i,b,j) * t2(b,p,j,q) * wm_interm_1_triplet(a, i)
! term(351) = term(351) + r2m(vrdav_Rl, a,i,b,j) * t2(a,p,j,q) * wm_interm_2_triplet(b, i)
! term(352) = term(352) + r2m(vrdav_Rl, a,i,b,j) * t2(a,p,i,q) * wm_interm_2_triplet(b, j)
! term(353) = term(353) + r2m(vrdav_Rl, a,i,b,j) * t2(b,p,i,q) * wm_interm_2_triplet(a, j)
! term(354) = term(354) + r2m(vrdav_Rl, a,i,b,j) * t2(b,p,j,q) * wm_interm_2_triplet(a, i)
! end do 
! end do 
! end do 
! end do 

! term(292) = term(292) * (-16.0d+0) 
! term(293) = term(293) * 32.0d+0 
! term(294) = term(294) * 24.0d+0 
! term(295) = term(295) * (-12.0d+0) 
! term(296) = term(296) * (-16.0d+0) 
! term(297) = term(297) * 32.0d+0 
! term(298) = term(298) * 8.0d+0 
! term(299) = term(299) * (-16.0d+0) 
! term(300) = term(300) * 24.0d+0 
! term(301) = term(301) * (-18.0d+0) 
! term(302) = term(302) * 36.0d+0 
! term(303) = term(303) * 12.0d+0 
! term(304) = term(304) * (-24.0d+0) 
! term(305) = term(305) * (-24.0d+0) 
! term(306) = term(306) * 48.0d+0 
! term(307) = term(307) * 36.0d+0 
! term(308) = term(308) * (-18.0d+0) 
! term(309) = term(309) * (-24.0d+0) 
! term(310) = term(310) * 48.0d+0 
! term(311) = term(311) * (-24.0d+0) 
! term(312) = term(312) * (-36.0d+0) 
! term(313) = term(313) * (-48.0d+0) 
! term(314) = term(314) * 24.0d+0 
! term(315) = term(315) * 32.0d+0 
! term(316) = term(316) * 32.0d+0 
! term(317) = term(317) * (-16.0d+0) 
! term(318) = term(318) * (-16.0d+0) 
! term(319) = term(319) * (-12.0d+0) 
! term(320) = term(320) * 24.0d+0 
! term(321) = term(321) * (-16.0d+0) 
! term(322) = term(322) * 32.0d+0 
! term(323) = term(323) * 8.0d+0 
! term(324) = term(324) * (-16.0d+0) 
! term(325) = term(325) * (-4.0d+0) 
! term(326) = term(326) * 8.0d+0 
! term(327) = term(327) * 8.0d+0 
! term(328) = term(328) * (-16.0d+0) 
! term(329) = term(329) * 8.0d+0 
! term(330) = term(330) * (-16.0d+0) 
! term(331) = term(331) * (-16.0d+0) 
! term(332) = term(332) * 32.0d+0 
! term(333) = term(333) * 48.0d+0 
! term(334) = term(334) * (-24.0d+0) 
! term(335) = term(335) * (-24.0d+0) 
! term(336) = term(336) * 12.0d+0 
! term(337) = term(337) * (-32.0d+0) 
! term(338) = term(338) * 64.0d+0 
! term(339) = term(339) * 16.0d+0 
! term(340) = term(340) * (-32.0d+0) 
! term(341) = term(341) * (-32.0d+0) 
! term(342) = term(342) * 16.0d+0 
! term(343) = term(343) * 12.0d+0 
! term(344) = term(344) * (-24.0d+0) 
! term(345) = term(345) * 12.0d+0 
! term(346) = term(346) * (-24.0d+0) 
! term(347) = term(347) * 16.0d+0 
! term(348) = term(348) * (-32.0d+0) 
! term(349) = term(349) * 16.0d+0 
! term(350) = term(350) * (-32.0d+0) 
! term(351) = term(351) * (-8.0d+0) 
! term(352) = term(352) * 16.0d+0 
! term(353) = term(353) * (-8.0d+0) 
! term(354) = term(354) * 16.0d+0 

! do k = 1, nocc 
! do j = 1, nocc 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! term(355) = term(355) + r2m(vrdav_Rr, a,i,p,j) * s2(a,b,k,i) * wm_interm_66_triplet(b, j, q, k)
! term(356) = term(356) + r2m(vrdav_Rr, a,j,p,i) * s2(a,b,k,j) * wm_interm_66_triplet(b, i, q, k)
! term(357) = term(357) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,k,i) * wm_interm_35_triplet(a, j, q, k)
! term(358) = term(358) + s2(a,p,j,i) * t2(a,b,i,k) * wm_interm_80_triplet(b, k, q, j)
! term(359) = term(359) + r2p(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,q,b,k) * wm_interm_82_triplet(a, k, j, i)
! term(360) = term(360) + r2p(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,q,p,k) * wm_interm_82_triplet(a, k, j, i)
! term(361) = term(361) + r2p(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,j,p,k) * wm_interm_82_triplet(a, k, q, i)
! term(362) = term(362) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,k,i) * wm_interm_50_triplet(a, q, j, k)
! term(363) = term(363) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,k,i) * wm_interm_50_triplet(a, j, q, k)
! term(364) = term(364) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,k,i) * wm_interm_49_triplet(a, q, j, k)
! term(365) = term(365) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,k,i) * wm_interm_49_triplet(a, j, q, k)
! term(366) = term(366) + s2(a,p,j,i) * t2(a,b,i,k) * wm_interm_90_triplet(b, q, k, j)
! term(367) = term(367) + s2(a,p,j,i) * t2(a,b,i,k) * wm_interm_90_triplet(b, k, q, j)
! term(368) = term(368) + s2(a,p,j,i) * t2(a,b,i,k) * wm_interm_89_triplet(b, q, k, j)
! term(369) = term(369) + s2(a,p,j,i) * t2(a,b,i,k) * wm_interm_89_triplet(b, k, q, j)
! term(370) = term(370) + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,q,b,k) * wm_interm_82_triplet(a, k, j, i)
! term(371) = term(371) + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,q,a,k) * wm_interm_82_triplet(b, k, j, i)
! term(372) = term(372) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,q,p,k) * wm_interm_82_triplet(a, k, j, i)
! term(373) = term(373) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,q,p,k) * wm_interm_82_triplet(b, k, j, i)
! term(374) = term(374) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,j,p,k) * wm_interm_82_triplet(a, k, q, i)
! term(375) = term(375) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,j,p,k) * wm_interm_82_triplet(b, k, q, i)
! end do 
! end do 
! end do 
! end do 
! end do 

! term(355) = term(355) * 4.0d+0 
! term(356) = term(356) * 4.0d+0 
! term(357) = term(357) * (-12.0d+0) 
! term(358) = term(358) * (-6.0d+0) 
! term(359) = term(359) * 9.0d+0 
! term(360) = term(360) * (-6.0d+0) 
! term(361) = term(361) * 12.0d+0 
! term(362) = term(362) * 8.0d+0 
! term(363) = term(363) * (-4.0d+0) 
! term(364) = term(364) * (-4.0d+0) 
! term(365) = term(365) * 8.0d+0 
! term(366) = term(366) * 4.0d+0 
! term(367) = term(367) * (-2.0d+0) 
! term(368) = term(368) * (-2.0d+0) 
! term(369) = term(369) * 4.0d+0 
! term(370) = term(370) * 6.0d+0 
! term(371) = term(371) * (-12.0d+0) 
! term(372) = term(372) * (-4.0d+0) 
! term(373) = term(373) * 8.0d+0 
! term(374) = term(374) * 8.0d+0 
! term(375) = term(375) * (-4.0d+0) 

! do k = 1, nocc 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! do j = 1, nocc 
! do b = nocc + 1, nactive 
! term(376) = term(376) + r2p(vrdav_Rl, p,q,a,i) * r2p(vrdav_Rr, b,j,a,k) * wm_interm_56_triplet(b, k, j, i)
! term(377) = term(377) + r2p(vrdav_Rl, p,q,a,i) * r2p(vrdav_Rr, b,j,a,k) * wm_interm_56_triplet(b, j, k, i)
! term(378) = term(378) + r2p(vrdav_Rl, b,j,a,k) * t2(a,p,q,i) * wm_interm_29_triplet(b, j, k, i)
! term(379) = term(379) + r2p(vrdav_Rl, b,j,a,k) * t2(a,p,q,i) * wm_interm_34_triplet(b, j, k, i)
! term(380) = term(380) + r2p(vrdav_Rl, b,j,a,k) * t2(a,p,q,i) * wm_interm_34_triplet(b, k, j, i)
! term(381) = term(381) + r2m(vrdav_Rl, a,i,p,q) * r2p(vrdav_Rr, b,j,a,k) * wm_interm_56_triplet(b, k, j, i)
! term(382) = term(382) + r2m(vrdav_Rl, a,q,p,i) * r2p(vrdav_Rr, b,j,a,k) * wm_interm_56_triplet(b, k, j, i)
! term(383) = term(383) + r2m(vrdav_Rl, a,i,p,q) * r2p(vrdav_Rr, b,j,a,k) * wm_interm_56_triplet(b, j, k, i)
! term(384) = term(384) + r2m(vrdav_Rl, a,q,p,i) * r2p(vrdav_Rr, b,j,a,k) * wm_interm_56_triplet(b, j, k, i)
! end do 
! end do 
! end do 
! end do 
! end do 

! term(376) = term(376) * 9.0d+0 
! term(377) = term(377) * (-9.0d+0) 
! term(378) = term(378) * 6.0d+0 
! term(379) = term(379) * 6.0d+0 
! term(380) = term(380) * (-6.0d+0) 
! term(381) = term(381) * 12.0d+0 
! term(382) = term(382) * (-6.0d+0) 
! term(383) = term(383) * (-12.0d+0) 
! term(384) = term(384) * 6.0d+0 

! do k = 1, nocc 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! do j = 1, nocc 
! term(385) = term(385) + r2p(vrdav_Rl, p,q,a,i) * r2m(vrdav_Rr, a,j,b,k) * wm_interm_56_triplet(b, j, k, i)
! term(386) = term(386) + r2p(vrdav_Rl, p,q,a,i) * r2m(vrdav_Rr, a,j,b,k) * wm_interm_56_triplet(b, k, j, i)
! end do 
! end do 
! end do 
! end do 
! end do 

! term(385) = term(385) * 12.0d+0 
! term(386) = term(386) * (-24.0d+0) 

! do j = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! do i = 1, nocc 
! do k = 1, nocc 
! term(387) = term(387) + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,k,a,j) * wm_interm_82_triplet(b, k, i, q)
! end do 
! end do 
! end do 
! end do 
! end do 

! term(387) = term(387) * 6.0d+0 

! do i = 1, nocc 
! do a = nocc + 1, nactive 
! term(388) = term(388) + s2(a,p,i,q) * wm_interm_3_triplet(a, i)
! term(389) = term(389) + s2(a,p,q,i) * wm_interm_3_triplet(a, i)
! term(390) = term(390) + s2(a,p,i,q) * wm_interm_4_triplet(a, i)
! term(391) = term(391) + s2(a,p,i,q) * wm_interm_5_triplet(a, i)
! term(392) = term(392) + s2(a,p,q,i) * wm_interm_4_triplet(a, i)
! term(393) = term(393) + s2(a,p,q,i) * wm_interm_5_triplet(a, i)
! term(394) = term(394) + r2p(vrdav_Rl, p,q,a,i) * wm_interm_0_triplet(a, i)
! term(395) = term(395) + r2p(vrdav_Rl, p,q,a,i) * wm_interm_1_triplet(a, i)
! term(396) = term(396) + r2p(vrdav_Rl, p,q,a,i) * wm_interm_2_triplet(a, i)
! term(397) = term(397) + r2p(vrdav_Rr, p,q,a,i) * wm_interm_6_triplet(a, i)
! term(398) = term(398) + r2m(vrdav_Rr, a,q,p,i) * wm_interm_6_triplet(a, i)
! term(399) = term(399) + r2m(vrdav_Rr, a,i,p,q) * wm_interm_6_triplet(a, i)
! term(400) = term(400) + t2(a,p,q,i) * wm_interm_7_triplet(a, i)
! term(401) = term(401) + t2(a,p,i,q) * wm_interm_7_triplet(a, i)
! term(402) = term(402) + r2m(vrdav_Rl, a,q,p,i) * wm_interm_0_triplet(a, i)
! term(403) = term(403) + r2m(vrdav_Rl, a,i,p,q) * wm_interm_0_triplet(a, i)
! term(404) = term(404) + r2m(vrdav_Rl, a,q,p,i) * wm_interm_1_triplet(a, i)
! term(405) = term(405) + r2m(vrdav_Rl, a,i,p,q) * wm_interm_1_triplet(a, i)
! term(406) = term(406) + r2m(vrdav_Rl, a,q,p,i) * wm_interm_2_triplet(a, i)
! term(407) = term(407) + r2m(vrdav_Rl, a,i,p,q) * wm_interm_2_triplet(a, i)
! term(408) = term(408) + r2p(vrdav_Rr, p,q,a,i) * wm_interm_8_triplet(a, i)
! term(409) = term(409) + r2p(vrdav_Rr, p,q,a,i) * wm_interm_9_triplet(a, i)
! term(410) = term(410) + r2m(vrdav_Rr, a,q,p,i) * wm_interm_8_triplet(a, i)
! term(411) = term(411) + r2m(vrdav_Rr, a,i,p,q) * wm_interm_8_triplet(a, i)
! term(412) = term(412) + r2m(vrdav_Rr, a,q,p,i) * wm_interm_9_triplet(a, i)
! term(413) = term(413) + r2m(vrdav_Rr, a,i,p,q) * wm_interm_9_triplet(a, i)
! term(414) = term(414) + t2(a,p,q,i) * wm_interm_10_triplet(a, i)
! term(415) = term(415) + t2(a,p,q,i) * wm_interm_11_triplet(a, i)
! term(416) = term(416) + t2(a,p,i,q) * wm_interm_10_triplet(a, i)
! term(417) = term(417) + t2(a,p,i,q) * wm_interm_11_triplet(a, i)
! term(418) = term(418) + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, p,q,a,i)
! term(419) = term(419) + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, a,q,p,i)
! term(420) = term(420) + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, a,i,p,q)
! term(421) = term(421) + r2p(vrdav_Rl, p,q,a,i) * r1(vrdav_Rr, a,i)
! term(422) = term(422) + r2m(vrdav_Rl, a,q,p,i) * r1(vrdav_Rr, a,i)
! term(423) = term(423) + r2m(vrdav_Rl, a,i,p,q) * r1(vrdav_Rr, a,i)
! end do 
! end do 

! term(388) = term(388) * (-12.0d+0) 
! term(389) = term(389) * 6.0d+0 
! term(390) = term(390) * 8.0d+0 
! term(391) = term(391) * (-16.0d+0) 
! term(392) = term(392) * (-4.0d+0) 
! term(393) = term(393) * 8.0d+0 
! term(394) = term(394) * 18.0d+0 
! term(395) = term(395) * 24.0d+0 
! term(396) = term(396) * (-12.0d+0) 
! term(397) = term(397) * (-18.0d+0) 
! term(398) = term(398) * 12.0d+0 
! term(399) = term(399) * (-24.0d+0) 
! term(400) = term(400) * 6.0d+0 
! term(401) = term(401) * (-12.0d+0) 
! term(402) = term(402) * (-12.0d+0) 
! term(403) = term(403) * 24.0d+0 
! term(404) = term(404) * (-16.0d+0) 
! term(405) = term(405) * 32.0d+0 
! term(406) = term(406) * 8.0d+0 
! term(407) = term(407) * (-16.0d+0) 
! term(408) = term(408) * (-24.0d+0) 
! term(409) = term(409) * 12.0d+0 
! term(410) = term(410) * 16.0d+0 
! term(411) = term(411) * (-32.0d+0) 
! term(412) = term(412) * (-8.0d+0) 
! term(413) = term(413) * 16.0d+0 
! term(414) = term(414) * (-4.0d+0) 
! term(415) = term(415) * 8.0d+0 
! term(416) = term(416) * 8.0d+0 
! term(417) = term(417) * (-16.0d+0) 
! term(418) = term(418) * 6.0d+0 
! term(419) = term(419) * (-4.0d+0) 
! term(420) = term(420) * 8.0d+0 
! term(421) = term(421) * 6.0d+0 
! term(422) = term(422) * (-4.0d+0) 
! term(423) = term(423) * 8.0d+0 

! do j = 1, nocc 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! do k = 1, nocc 
! term(424) = term(424) + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,k,a,i) * wm_interm_82_triplet(b, k, j, q)
! term(425) = term(425) + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,k,a,j) * wm_interm_82_triplet(b, k, q, i)
! term(426) = term(426) + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,k,a,i) * wm_interm_82_triplet(b, k, q, j)
! end do 
! end do 
! end do 
! end do 
! end do 

! term(424) = term(424) * (-12.0d+0) 
! term(425) = term(425) * (-6.0d+0) 
! term(426) = term(426) * 6.0d+0 

! term(427) = term(427) + s1(p,q) * wm_interm_13_triplet
! term(428) = term(428) + t1(p,q) * wm_interm_13_triplet
! term(429) = term(429) + s1(p,q) * wm_interm_28_triplet
! term(430) = term(430) + s1(p,q) * wm_interm_32_triplet
! term(431) = term(431) + s1(p,q) * wm_interm_33_triplet
! term(432) = term(432) + t1(p,q) * wm_interm_28_triplet
! term(433) = term(433) + t1(p,q) * wm_interm_32_triplet
! term(434) = term(434) + t1(p,q) * wm_interm_33_triplet
! term(435) = term(435) + s1(p,q) * wm_interm_42_triplet
! term(436) = term(436) + s1(p,q) * wm_interm_47_triplet
! term(437) = term(437) + s1(p,q) * wm_interm_48_triplet
! term(438) = term(438) + t1(p,q) * wm_interm_42_triplet
! term(439) = term(439) + t1(p,q) * wm_interm_47_triplet
! term(440) = term(440) + t1(p,q) * wm_interm_48_triplet
! term(441) = term(441) + wm_interm_13_triplet * wm_interm_61_triplet(p, q)
! term(442) = term(442) + wm_interm_13_triplet * wm_interm_65_triplet(p, q)
! term(443) = term(443) + wm_interm_28_triplet * wm_interm_61_triplet(p, q)
! term(444) = term(444) + wm_interm_28_triplet * wm_interm_65_triplet(p, q)
! term(445) = term(445) + wm_interm_32_triplet * wm_interm_61_triplet(p, q)
! term(446) = term(446) + wm_interm_33_triplet * wm_interm_61_triplet(p, q)
! term(447) = term(447) + wm_interm_32_triplet * wm_interm_65_triplet(p, q)
! term(448) = term(448) + wm_interm_33_triplet * wm_interm_65_triplet(p, q)
! term(449) = term(449) + wm_interm_42_triplet * wm_interm_61_triplet(p, q)
! term(450) = term(450) + wm_interm_42_triplet * wm_interm_65_triplet(p, q)
! term(451) = term(451) + wm_interm_47_triplet * wm_interm_61_triplet(p, q)
! term(452) = term(452) + wm_interm_48_triplet * wm_interm_61_triplet(p, q)
! term(453) = term(453) + wm_interm_47_triplet * wm_interm_65_triplet(p, q)
! term(454) = term(454) + wm_interm_48_triplet * wm_interm_65_triplet(p, q)

! term(427) = term(427) * (-4.0d+0) 
! term(428) = term(428) * (-4.0d+0) 
! term(429) = term(429) * (-6.0d+0) 
! term(430) = term(430) * 4.0d+0 
! term(431) = term(431) * (-8.0d+0) 
! term(432) = term(432) * (-6.0d+0) 
! term(433) = term(433) * 4.0d+0 
! term(434) = term(434) * (-8.0d+0) 
! term(435) = term(435) * (-12.0d+0) 
! term(436) = term(436) * 8.0d+0 
! term(437) = term(437) * (-16.0d+0) 
! term(438) = term(438) * (-12.0d+0) 
! term(439) = term(439) * 8.0d+0 
! term(440) = term(440) * (-16.0d+0) 
! term(441) = term(441) * (-4.0d+0) 
! term(442) = term(442) * 8.0d+0 
! term(443) = term(443) * (-6.0d+0) 
! term(444) = term(444) * 12.0d+0 
! term(445) = term(445) * 4.0d+0 
! term(446) = term(446) * (-8.0d+0) 
! term(447) = term(447) * (-8.0d+0) 
! term(448) = term(448) * 16.0d+0 
! term(449) = term(449) * (-12.0d+0) 
! term(450) = term(450) * 24.0d+0 
! term(451) = term(451) * 8.0d+0 
! term(452) = term(452) * (-16.0d+0) 
! term(453) = term(453) * (-16.0d+0) 
! term(454) = term(454) * 32.0d+0 

! do j = 1, nocc 
! do b = nocc + 1, nactive 
! do i = 1, nocc 
! do k = 1, nocc 
! do a = nocc + 1, nactive 
! term(455) = term(455) + r2p(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,k,p,q) * wm_interm_82_triplet(a, k, i, j)
! term(456) = term(456) + r2p(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,k,b,j) * wm_interm_82_triplet(a, k, i, q)
! term(457) = term(457) + r2p(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,k,p,j) * wm_interm_82_triplet(a, k, i, q)
! term(458) = term(458) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,k,p,q) * wm_interm_82_triplet(b, k, i, j)
! term(459) = term(459) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,k,p,q) * wm_interm_82_triplet(a, k, i, j)
! term(460) = term(460) + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,k,b,j) * wm_interm_82_triplet(a, k, i, q)
! term(461) = term(461) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,k,p,j) * wm_interm_82_triplet(a, k, i, q)
! term(462) = term(462) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,k,p,j) * wm_interm_82_triplet(b, k, i, q)
! end do 
! end do 
! end do 
! end do 
! end do 

! term(455) = term(455) * (-12.0d+0) 
! term(456) = term(456) * (-18.0d+0) 
! term(457) = term(457) * 12.0d+0 
! term(458) = term(458) * 8.0d+0 
! term(459) = term(459) * (-16.0d+0) 
! term(460) = term(460) * (-12.0d+0) 
! term(461) = term(461) * 8.0d+0 
! term(462) = term(462) * (-4.0d+0) 

! do i = 1, nocc 
! do a = nocc + 1, nactive 
! do j = 1, nocc 
! do b = nocc + 1, nactive 
! term(463) = term(463) + r2p(vrdav_Rl, b,j,a,i) * t2(a,p,q,i) * wm_interm_0_triplet(b, j)
! term(464) = term(464) + r2p(vrdav_Rl, b,j,a,i) * t2(a,p,q,i) * wm_interm_1_triplet(b, j)
! term(465) = term(465) + r2p(vrdav_Rl, b,j,a,i) * t2(a,p,q,i) * wm_interm_2_triplet(b, j)
! end do 
! end do 
! end do 
! end do 

! term(463) = term(463) * 18.0d+0 
! term(464) = term(464) * 24.0d+0 
! term(465) = term(465) * (-12.0d+0) 

! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! do j = 1, nocc 
! term(466) = term(466) + r2p(vrdav_Rr, p,q,a,i) * s2(a,b,j,i) * wm_interm_64_triplet(b, j)
! term(467) = term(467) + r2p(vrdav_Rr, p,q,a,i) * s2(a,b,j,i) * wm_interm_63_triplet(b, j)
! term(468) = term(468) + r2p(vrdav_Rl, p,q,a,i) * t2(a,b,j,i) * wm_interm_57_triplet(b, j)
! term(469) = term(469) + r2p(vrdav_Rl, p,q,a,i) * t2(a,b,j,i) * wm_interm_58_triplet(b, j)
! end do 
! end do 
! end do 
! end do 

! term(466) = term(466) * 6.0d+0 
! term(467) = term(467) * (-12.0d+0) 
! term(468) = term(468) * 6.0d+0 
! term(469) = term(469) * (-12.0d+0) 

! do i = 1, nocc 
! do j = 1, nocc 
! term(470) = term(470) + wm_interm_55_triplet(i, j) * wm_interm_56_triplet(p, j, q, i)
! term(471) = term(471) + wm_interm_55_triplet(i, j) * wm_interm_56_triplet(p, q, j, i)
! term(472) = term(472) + wm_interm_55_triplet(i, j) * wm_interm_60_triplet(p, j, q, i)
! term(473) = term(473) + wm_interm_55_triplet(i, j) * wm_interm_60_triplet(p, j, i, q)
! term(474) = term(474) + wm_interm_68_triplet(i, j) * wm_interm_72_triplet(p, j, q, i)
! term(475) = term(475) + wm_interm_68_triplet(i, j) * wm_interm_72_triplet(p, j, i, q)
! term(476) = term(476) + wm_interm_69_triplet(i, j) * wm_interm_72_triplet(p, j, q, i)
! term(477) = term(477) + wm_interm_69_triplet(i, j) * wm_interm_72_triplet(p, j, i, q)
! term(478) = term(478) + wm_interm_27_triplet(i, j) * wm_interm_56_triplet(p, j, q, i)
! term(479) = term(479) + wm_interm_27_triplet(i, j) * wm_interm_56_triplet(p, q, j, i)
! term(480) = term(480) + wm_interm_31_triplet(i, j) * wm_interm_56_triplet(p, j, q, i)
! term(481) = term(481) + wm_interm_30_triplet(i, j) * wm_interm_56_triplet(p, j, q, i)
! term(482) = term(482) + wm_interm_31_triplet(i, j) * wm_interm_56_triplet(p, q, j, i)
! term(483) = term(483) + wm_interm_30_triplet(i, j) * wm_interm_56_triplet(p, q, j, i)
! term(484) = term(484) + wm_interm_68_triplet(i, j) * wm_interm_80_triplet(p, j, q, i)
! term(485) = term(485) + wm_interm_69_triplet(i, j) * wm_interm_80_triplet(p, j, q, i)
! term(486) = term(486) + wm_interm_27_triplet(i, j) * wm_interm_60_triplet(p, j, q, i)
! term(487) = term(487) + wm_interm_27_triplet(i, j) * wm_interm_60_triplet(p, j, i, q)
! term(488) = term(488) + wm_interm_30_triplet(i, j) * wm_interm_60_triplet(p, j, q, i)
! term(489) = term(489) + wm_interm_31_triplet(i, j) * wm_interm_60_triplet(p, j, q, i)
! term(490) = term(490) + wm_interm_30_triplet(i, j) * wm_interm_60_triplet(p, j, i, q)
! term(491) = term(491) + wm_interm_31_triplet(i, j) * wm_interm_60_triplet(p, j, i, q)
! term(492) = term(492) + wm_interm_41_triplet(i, j) * wm_interm_56_triplet(p, j, q, i)
! term(493) = term(493) + wm_interm_41_triplet(i, j) * wm_interm_56_triplet(p, q, j, i)
! term(494) = term(494) + wm_interm_40_triplet(i, j) * wm_interm_56_triplet(p, j, q, i)
! term(495) = term(495) + wm_interm_40_triplet(i, j) * wm_interm_56_triplet(p, q, j, i)
! term(496) = term(496) + wm_interm_45_triplet(i, j) * wm_interm_56_triplet(p, j, q, i)
! term(497) = term(497) + wm_interm_46_triplet(i, j) * wm_interm_56_triplet(p, j, q, i)
! term(498) = term(498) + wm_interm_45_triplet(i, j) * wm_interm_56_triplet(p, q, j, i)
! term(499) = term(499) + wm_interm_46_triplet(i, j) * wm_interm_56_triplet(p, q, j, i)
! term(500) = term(500) + wm_interm_43_triplet(i, j) * wm_interm_56_triplet(p, j, q, i)
! term(501) = term(501) + wm_interm_44_triplet(i, j) * wm_interm_56_triplet(p, j, q, i)
! term(502) = term(502) + wm_interm_43_triplet(i, j) * wm_interm_56_triplet(p, q, j, i)
! term(503) = term(503) + wm_interm_44_triplet(i, j) * wm_interm_56_triplet(p, q, j, i)
! term(504) = term(504) + wm_interm_68_triplet(i, j) * wm_interm_89_triplet(p, q, j, i)
! term(505) = term(505) + wm_interm_69_triplet(i, j) * wm_interm_89_triplet(p, q, j, i)
! term(506) = term(506) + wm_interm_68_triplet(i, j) * wm_interm_90_triplet(p, q, j, i)
! term(507) = term(507) + wm_interm_69_triplet(i, j) * wm_interm_90_triplet(p, q, j, i)
! term(508) = term(508) + wm_interm_68_triplet(i, j) * wm_interm_89_triplet(p, j, q, i)
! term(509) = term(509) + wm_interm_68_triplet(i, j) * wm_interm_90_triplet(p, j, q, i)
! term(510) = term(510) + wm_interm_69_triplet(i, j) * wm_interm_89_triplet(p, j, q, i)
! term(511) = term(511) + wm_interm_69_triplet(i, j) * wm_interm_90_triplet(p, j, q, i)
! term(512) = term(512) + wm_interm_40_triplet(i, j) * wm_interm_60_triplet(p, j, q, i)
! term(513) = term(513) + wm_interm_41_triplet(i, j) * wm_interm_60_triplet(p, j, q, i)
! term(514) = term(514) + wm_interm_40_triplet(i, j) * wm_interm_60_triplet(p, j, i, q)
! term(515) = term(515) + wm_interm_41_triplet(i, j) * wm_interm_60_triplet(p, j, i, q)
! term(516) = term(516) + wm_interm_43_triplet(i, j) * wm_interm_60_triplet(p, j, q, i)
! term(517) = term(517) + wm_interm_44_triplet(i, j) * wm_interm_60_triplet(p, j, q, i)
! term(518) = term(518) + wm_interm_45_triplet(i, j) * wm_interm_60_triplet(p, j, q, i)
! term(519) = term(519) + wm_interm_46_triplet(i, j) * wm_interm_60_triplet(p, j, q, i)
! term(520) = term(520) + wm_interm_43_triplet(i, j) * wm_interm_60_triplet(p, j, i, q)
! term(521) = term(521) + wm_interm_44_triplet(i, j) * wm_interm_60_triplet(p, j, i, q)
! term(522) = term(522) + wm_interm_45_triplet(i, j) * wm_interm_60_triplet(p, j, i, q)
! term(523) = term(523) + wm_interm_46_triplet(i, j) * wm_interm_60_triplet(p, j, i, q)
! end do 
! end do 

! term(470) = term(470) * 2.0d+0 
! term(471) = term(471) * (-4.0d+0) 
! term(472) = term(472) * 2.0d+0 
! term(473) = term(473) * (-4.0d+0) 
! term(474) = term(474) * (-4.0d+0) 
! term(475) = term(475) * 8.0d+0 
! term(476) = term(476) * 8.0d+0 
! term(477) = term(477) * (-16.0d+0) 
! term(478) = term(478) * 6.0d+0 
! term(479) = term(479) * (-12.0d+0) 
! term(480) = term(480) * (-6.0d+0) 
! term(481) = term(481) * 6.0d+0 
! term(482) = term(482) * 12.0d+0 
! term(483) = term(483) * (-12.0d+0) 
! term(484) = term(484) * 6.0d+0 
! term(485) = term(485) * (-12.0d+0) 
! term(486) = term(486) * 6.0d+0 
! term(487) = term(487) * (-12.0d+0) 
! term(488) = term(488) * 6.0d+0 
! term(489) = term(489) * (-6.0d+0) 
! term(490) = term(490) * (-12.0d+0) 
! term(491) = term(491) * 12.0d+0 
! term(492) = term(492) * 6.0d+0 
! term(493) = term(493) * (-12.0d+0) 
! term(494) = term(494) * (-6.0d+0) 
! term(495) = term(495) * 12.0d+0 
! term(496) = term(496) * (-4.0d+0) 
! term(497) = term(497) * 8.0d+0 
! term(498) = term(498) * 8.0d+0 
! term(499) = term(499) * (-16.0d+0) 
! term(500) = term(500) * (-4.0d+0) 
! term(501) = term(501) * 8.0d+0 
! term(502) = term(502) * 8.0d+0 
! term(503) = term(503) * (-16.0d+0) 
! term(504) = term(504) * 4.0d+0 
! term(505) = term(505) * (-8.0d+0) 
! term(506) = term(506) * (-2.0d+0) 
! term(507) = term(507) * 4.0d+0 
! term(508) = term(508) * (-2.0d+0) 
! term(509) = term(509) * 4.0d+0 
! term(510) = term(510) * 4.0d+0 
! term(511) = term(511) * (-8.0d+0) 
! term(512) = term(512) * (-6.0d+0) 
! term(513) = term(513) * 6.0d+0 
! term(514) = term(514) * 12.0d+0 
! term(515) = term(515) * (-12.0d+0) 
! term(516) = term(516) * (-4.0d+0) 
! term(517) = term(517) * 8.0d+0 
! term(518) = term(518) * (-4.0d+0) 
! term(519) = term(519) * 8.0d+0 
! term(520) = term(520) * 8.0d+0 
! term(521) = term(521) * (-16.0d+0) 
! term(522) = term(522) * 8.0d+0 
! term(523) = term(523) * (-16.0d+0) 

! do j = 1, nocc 
! do i = 1, nocc 
! term(524) = term(524) + wm_interm_15_triplet(i, j) * wm_interm_59_triplet(p, i, j, q)
! term(525) = term(525) + wm_interm_15_triplet(i, j) * wm_interm_59_triplet(p, i, q, j)
! term(526) = term(526) + wm_interm_14_triplet(i, j) * wm_interm_66_triplet(p, i, q, j)
! term(527) = term(527) + wm_interm_14_triplet(i, j) * wm_interm_66_triplet(p, i, j, q)
! term(528) = term(528) + wm_interm_67_triplet(p, i, q, j) * wm_interm_68_triplet(j, i)
! term(529) = term(529) + wm_interm_67_triplet(p, i, q, j) * wm_interm_69_triplet(j, i)
! term(530) = term(530) + wm_interm_18_triplet(i, j) * wm_interm_66_triplet(p, i, q, j)
! term(531) = term(531) + wm_interm_19_triplet(i, j) * wm_interm_66_triplet(p, i, q, j)
! term(532) = term(532) + wm_interm_18_triplet(i, j) * wm_interm_66_triplet(p, i, j, q)
! term(533) = term(533) + wm_interm_19_triplet(i, j) * wm_interm_66_triplet(p, i, j, q)
! term(534) = term(534) + wm_interm_23_triplet(i, j) * wm_interm_66_triplet(p, i, q, j)
! term(535) = term(535) + wm_interm_24_triplet(i, j) * wm_interm_66_triplet(p, i, q, j)
! term(536) = term(536) + wm_interm_25_triplet(i, j) * wm_interm_66_triplet(p, i, q, j)
! term(537) = term(537) + wm_interm_26_triplet(i, j) * wm_interm_66_triplet(p, i, q, j)
! term(538) = term(538) + wm_interm_23_triplet(i, j) * wm_interm_66_triplet(p, i, j, q)
! term(539) = term(539) + wm_interm_24_triplet(i, j) * wm_interm_66_triplet(p, i, j, q)
! term(540) = term(540) + wm_interm_25_triplet(i, j) * wm_interm_66_triplet(p, i, j, q)
! term(541) = term(541) + wm_interm_26_triplet(i, j) * wm_interm_66_triplet(p, i, j, q)
! term(542) = term(542) + wm_interm_18_triplet(i, j) * wm_interm_35_triplet(p, i, q, j)
! term(543) = term(543) + wm_interm_19_triplet(i, j) * wm_interm_35_triplet(p, i, q, j)
! term(544) = term(544) + wm_interm_23_triplet(i, j) * wm_interm_35_triplet(p, i, q, j)
! term(545) = term(545) + wm_interm_24_triplet(i, j) * wm_interm_35_triplet(p, i, q, j)
! term(546) = term(546) + wm_interm_25_triplet(i, j) * wm_interm_35_triplet(p, i, q, j)
! term(547) = term(547) + wm_interm_26_triplet(i, j) * wm_interm_35_triplet(p, i, q, j)
! term(548) = term(548) + wm_interm_37_triplet(i, j) * wm_interm_59_triplet(p, i, j, q)
! term(549) = term(549) + wm_interm_37_triplet(i, j) * wm_interm_59_triplet(p, i, q, j)
! term(550) = term(550) + wm_interm_36_triplet(i, j) * wm_interm_59_triplet(p, i, j, q)
! term(551) = term(551) + wm_interm_36_triplet(i, j) * wm_interm_59_triplet(p, i, q, j)
! term(552) = term(552) + wm_interm_29_triplet(p, i, q, j) * wm_interm_37_triplet(i, j)
! term(553) = term(553) + wm_interm_29_triplet(p, i, q, j) * wm_interm_36_triplet(i, j)
! term(554) = term(554) + wm_interm_34_triplet(p, i, q, j) * wm_interm_37_triplet(i, j)
! term(555) = term(555) + wm_interm_34_triplet(p, q, i, j) * wm_interm_37_triplet(i, j)
! term(556) = term(556) + wm_interm_37_triplet(i, j) * wm_interm_83_triplet(p, q, i, j)
! term(557) = term(557) + wm_interm_37_triplet(i, j) * wm_interm_83_triplet(p, i, q, j)
! term(558) = term(558) + wm_interm_34_triplet(p, i, q, j) * wm_interm_36_triplet(i, j)
! term(559) = term(559) + wm_interm_34_triplet(p, q, i, j) * wm_interm_36_triplet(i, j)
! term(560) = term(560) + wm_interm_36_triplet(i, j) * wm_interm_83_triplet(p, q, i, j)
! term(561) = term(561) + wm_interm_36_triplet(i, j) * wm_interm_83_triplet(p, i, q, j)
! term(562) = term(562) + wm_interm_18_triplet(i, j) * wm_interm_50_triplet(p, i, q, j)
! term(563) = term(563) + wm_interm_18_triplet(i, j) * wm_interm_50_triplet(p, q, i, j)
! term(564) = term(564) + wm_interm_19_triplet(i, j) * wm_interm_50_triplet(p, i, q, j)
! term(565) = term(565) + wm_interm_19_triplet(i, j) * wm_interm_50_triplet(p, q, i, j)
! term(566) = term(566) + wm_interm_18_triplet(i, j) * wm_interm_49_triplet(p, q, i, j)
! term(567) = term(567) + wm_interm_18_triplet(i, j) * wm_interm_49_triplet(p, i, q, j)
! term(568) = term(568) + wm_interm_19_triplet(i, j) * wm_interm_49_triplet(p, q, i, j)
! term(569) = term(569) + wm_interm_19_triplet(i, j) * wm_interm_49_triplet(p, i, q, j)
! term(570) = term(570) + wm_interm_23_triplet(i, j) * wm_interm_50_triplet(p, i, q, j)
! term(571) = term(571) + wm_interm_24_triplet(i, j) * wm_interm_50_triplet(p, i, q, j)
! term(572) = term(572) + wm_interm_23_triplet(i, j) * wm_interm_50_triplet(p, q, i, j)
! term(573) = term(573) + wm_interm_24_triplet(i, j) * wm_interm_50_triplet(p, q, i, j)
! term(574) = term(574) + wm_interm_25_triplet(i, j) * wm_interm_50_triplet(p, i, q, j)
! term(575) = term(575) + wm_interm_25_triplet(i, j) * wm_interm_50_triplet(p, q, i, j)
! term(576) = term(576) + wm_interm_26_triplet(i, j) * wm_interm_50_triplet(p, i, q, j)
! term(577) = term(577) + wm_interm_26_triplet(i, j) * wm_interm_50_triplet(p, q, i, j)
! term(578) = term(578) + wm_interm_23_triplet(i, j) * wm_interm_49_triplet(p, q, i, j)
! term(579) = term(579) + wm_interm_24_triplet(i, j) * wm_interm_49_triplet(p, q, i, j)
! term(580) = term(580) + wm_interm_23_triplet(i, j) * wm_interm_49_triplet(p, i, q, j)
! term(581) = term(581) + wm_interm_24_triplet(i, j) * wm_interm_49_triplet(p, i, q, j)
! term(582) = term(582) + wm_interm_25_triplet(i, j) * wm_interm_49_triplet(p, q, i, j)
! term(583) = term(583) + wm_interm_25_triplet(i, j) * wm_interm_49_triplet(p, i, q, j)
! term(584) = term(584) + wm_interm_26_triplet(i, j) * wm_interm_49_triplet(p, q, i, j)
! term(585) = term(585) + wm_interm_26_triplet(i, j) * wm_interm_49_triplet(p, i, q, j)
! term(586) = term(586) + wm_interm_51_triplet(i, j) * wm_interm_59_triplet(p, i, j, q)
! term(587) = term(587) + wm_interm_51_triplet(i, j) * wm_interm_59_triplet(p, i, q, j)
! term(588) = term(588) + wm_interm_52_triplet(i, j) * wm_interm_59_triplet(p, i, j, q)
! term(589) = term(589) + wm_interm_52_triplet(i, j) * wm_interm_59_triplet(p, i, q, j)
! term(590) = term(590) + wm_interm_29_triplet(p, i, q, j) * wm_interm_51_triplet(i, j)
! term(591) = term(591) + wm_interm_29_triplet(p, i, q, j) * wm_interm_52_triplet(i, j)
! term(592) = term(592) + wm_interm_34_triplet(p, i, q, j) * wm_interm_51_triplet(i, j)
! term(593) = term(593) + wm_interm_34_triplet(p, q, i, j) * wm_interm_51_triplet(i, j)
! term(594) = term(594) + wm_interm_51_triplet(i, j) * wm_interm_83_triplet(p, q, i, j)
! term(595) = term(595) + wm_interm_51_triplet(i, j) * wm_interm_83_triplet(p, i, q, j)
! term(596) = term(596) + wm_interm_34_triplet(p, i, q, j) * wm_interm_52_triplet(i, j)
! term(597) = term(597) + wm_interm_34_triplet(p, q, i, j) * wm_interm_52_triplet(i, j)
! term(598) = term(598) + wm_interm_52_triplet(i, j) * wm_interm_83_triplet(p, q, i, j)
! term(599) = term(599) + wm_interm_52_triplet(i, j) * wm_interm_83_triplet(p, i, q, j)
! end do 
! end do 

! term(524) = term(524) * (-4.0d+0) 
! term(525) = term(525) * 2.0d+0 
! term(526) = term(526) * 2.0d+0 
! term(527) = term(527) * (-4.0d+0) 
! term(528) = term(528) * (-6.0d+0) 
! term(529) = term(529) * 12.0d+0 
! term(530) = term(530) * (-3.0d+0) 
! term(531) = term(531) * 3.0d+0 
! term(532) = term(532) * 6.0d+0 
! term(533) = term(533) * (-6.0d+0) 
! term(534) = term(534) * (-2.0d+0) 
! term(535) = term(535) * 4.0d+0 
! term(536) = term(536) * (-2.0d+0) 
! term(537) = term(537) * 4.0d+0 
! term(538) = term(538) * 4.0d+0 
! term(539) = term(539) * (-8.0d+0) 
! term(540) = term(540) * 4.0d+0 
! term(541) = term(541) * (-8.0d+0) 
! term(542) = term(542) * 9.0d+0 
! term(543) = term(543) * (-9.0d+0) 
! term(544) = term(544) * 6.0d+0 
! term(545) = term(545) * (-12.0d+0) 
! term(546) = term(546) * 6.0d+0 
! term(547) = term(547) * (-12.0d+0) 
! term(548) = term(548) * (-6.0d+0) 
! term(549) = term(549) * 3.0d+0 
! term(550) = term(550) * 6.0d+0 
! term(551) = term(551) * (-3.0d+0) 
! term(552) = term(552) * 9.0d+0 
! term(553) = term(553) * (-9.0d+0) 
! term(554) = term(554) * 3.0d+0 
! term(555) = term(555) * (-6.0d+0) 
! term(556) = term(556) * 3.0d+0 
! term(557) = term(557) * (-6.0d+0) 
! term(558) = term(558) * (-3.0d+0) 
! term(559) = term(559) * 6.0d+0 
! term(560) = term(560) * (-3.0d+0) 
! term(561) = term(561) * 6.0d+0 
! term(562) = term(562) * (-3.0d+0) 
! term(563) = term(563) * 6.0d+0 
! term(564) = term(564) * 3.0d+0 
! term(565) = term(565) * (-6.0d+0) 
! term(566) = term(566) * (-3.0d+0) 
! term(567) = term(567) * 6.0d+0 
! term(568) = term(568) * 3.0d+0 
! term(569) = term(569) * (-6.0d+0) 
! term(570) = term(570) * (-2.0d+0) 
! term(571) = term(571) * 4.0d+0 
! term(572) = term(572) * 4.0d+0 
! term(573) = term(573) * (-8.0d+0) 
! term(574) = term(574) * (-2.0d+0) 
! term(575) = term(575) * 4.0d+0 
! term(576) = term(576) * 4.0d+0 
! term(577) = term(577) * (-8.0d+0) 
! term(578) = term(578) * (-2.0d+0) 
! term(579) = term(579) * 4.0d+0 
! term(580) = term(580) * 4.0d+0 
! term(581) = term(581) * (-8.0d+0) 
! term(582) = term(582) * (-2.0d+0) 
! term(583) = term(583) * 4.0d+0 
! term(584) = term(584) * 4.0d+0 
! term(585) = term(585) * (-8.0d+0) 
! term(586) = term(586) * 8.0d+0 
! term(587) = term(587) * (-4.0d+0) 
! term(588) = term(588) * (-16.0d+0) 
! term(589) = term(589) * 8.0d+0 
! term(590) = term(590) * (-12.0d+0) 
! term(591) = term(591) * 24.0d+0 
! term(592) = term(592) * (-4.0d+0) 
! term(593) = term(593) * 8.0d+0 
! term(594) = term(594) * (-4.0d+0) 
! term(595) = term(595) * 8.0d+0 
! term(596) = term(596) * 8.0d+0 
! term(597) = term(597) * (-16.0d+0) 
! term(598) = term(598) * 8.0d+0 
! term(599) = term(599) * (-16.0d+0) 

! do i = 1, nocc 
! term(600) = term(600) + r1(vrdav_Rl, p,i) * wm_interm_14_triplet(i, q)
! term(601) = term(601) + r1(vrdav_Rr, p,i) * wm_interm_15_triplet(i, q)
! term(602) = term(602) + r1(vrdav_Rl, p,i) * wm_interm_18_triplet(i, q)
! term(603) = term(603) + r1(vrdav_Rl, p,i) * wm_interm_19_triplet(i, q)
! term(604) = term(604) + r1(vrdav_Rl, p,i) * wm_interm_23_triplet(i, q)
! term(605) = term(605) + r1(vrdav_Rl, p,i) * wm_interm_24_triplet(i, q)
! term(606) = term(606) + r1(vrdav_Rl, p,i) * wm_interm_25_triplet(i, q)
! term(607) = term(607) + r1(vrdav_Rl, p,i) * wm_interm_26_triplet(i, q)
! term(608) = term(608) + r1(vrdav_Rr, p,i) * wm_interm_36_triplet(i, q)
! term(609) = term(609) + r1(vrdav_Rr, p,i) * wm_interm_37_triplet(i, q)
! term(610) = term(610) + r1(vrdav_Rr, p,i) * wm_interm_51_triplet(i, q)
! term(611) = term(611) + r1(vrdav_Rr, p,i) * wm_interm_52_triplet(i, q)
! term(612) = term(612) + wm_interm_15_triplet(q, i) * wm_interm_57_triplet(p, i)
! term(613) = term(613) + wm_interm_15_triplet(q, i) * wm_interm_58_triplet(p, i)
! term(614) = term(614) + wm_interm_3_triplet(p, i) * wm_interm_68_triplet(i, q)
! term(615) = term(615) + wm_interm_3_triplet(p, i) * wm_interm_69_triplet(i, q)
! term(616) = term(616) + wm_interm_4_triplet(p, i) * wm_interm_68_triplet(i, q)
! term(617) = term(617) + wm_interm_5_triplet(p, i) * wm_interm_68_triplet(i, q)
! term(618) = term(618) + wm_interm_4_triplet(p, i) * wm_interm_69_triplet(i, q)
! term(619) = term(619) + wm_interm_5_triplet(p, i) * wm_interm_69_triplet(i, q)
! term(620) = term(620) + wm_interm_37_triplet(q, i) * wm_interm_57_triplet(p, i)
! term(621) = term(621) + wm_interm_37_triplet(q, i) * wm_interm_58_triplet(p, i)
! term(622) = term(622) + wm_interm_36_triplet(q, i) * wm_interm_57_triplet(p, i)
! term(623) = term(623) + wm_interm_36_triplet(q, i) * wm_interm_58_triplet(p, i)
! term(624) = term(624) + wm_interm_0_triplet(p, i) * wm_interm_36_triplet(i, q)
! term(625) = term(625) + wm_interm_0_triplet(p, i) * wm_interm_37_triplet(i, q)
! term(626) = term(626) + wm_interm_1_triplet(p, i) * wm_interm_36_triplet(i, q)
! term(627) = term(627) + wm_interm_1_triplet(p, i) * wm_interm_37_triplet(i, q)
! term(628) = term(628) + wm_interm_2_triplet(p, i) * wm_interm_36_triplet(i, q)
! term(629) = term(629) + wm_interm_2_triplet(p, i) * wm_interm_37_triplet(i, q)
! term(630) = term(630) + wm_interm_51_triplet(q, i) * wm_interm_57_triplet(p, i)
! term(631) = term(631) + wm_interm_51_triplet(q, i) * wm_interm_58_triplet(p, i)
! term(632) = term(632) + wm_interm_52_triplet(q, i) * wm_interm_57_triplet(p, i)
! term(633) = term(633) + wm_interm_52_triplet(q, i) * wm_interm_58_triplet(p, i)
! term(634) = term(634) + wm_interm_0_triplet(p, i) * wm_interm_51_triplet(i, q)
! term(635) = term(635) + wm_interm_0_triplet(p, i) * wm_interm_52_triplet(i, q)
! term(636) = term(636) + wm_interm_1_triplet(p, i) * wm_interm_51_triplet(i, q)
! term(637) = term(637) + wm_interm_1_triplet(p, i) * wm_interm_52_triplet(i, q)
! term(638) = term(638) + wm_interm_2_triplet(p, i) * wm_interm_51_triplet(i, q)
! term(639) = term(639) + wm_interm_2_triplet(p, i) * wm_interm_52_triplet(i, q)
! end do 

! term(600) = term(600) * 2.0d+0 
! term(601) = term(601) * 2.0d+0 
! term(602) = term(602) * (-3.0d+0) 
! term(603) = term(603) * 3.0d+0 
! term(604) = term(604) * (-2.0d+0) 
! term(605) = term(605) * 4.0d+0 
! term(606) = term(606) * (-2.0d+0) 
! term(607) = term(607) * 4.0d+0 
! term(608) = term(608) * (-3.0d+0) 
! term(609) = term(609) * 3.0d+0 
! term(610) = term(610) * (-4.0d+0) 
! term(611) = term(611) * 8.0d+0 
! term(612) = term(612) * 2.0d+0 
! term(613) = term(613) * (-4.0d+0) 
! term(614) = term(614) * 6.0d+0 
! term(615) = term(615) * (-12.0d+0) 
! term(616) = term(616) * (-4.0d+0) 
! term(617) = term(617) * 8.0d+0 
! term(618) = term(618) * 8.0d+0 
! term(619) = term(619) * (-16.0d+0) 
! term(620) = term(620) * (-3.0d+0) 
! term(621) = term(621) * 6.0d+0 
! term(622) = term(622) * 3.0d+0 
! term(623) = term(623) * (-6.0d+0) 
! term(624) = term(624) * (-9.0d+0) 
! term(625) = term(625) * 9.0d+0 
! term(626) = term(626) * (-12.0d+0) 
! term(627) = term(627) * 12.0d+0 
! term(628) = term(628) * 6.0d+0 
! term(629) = term(629) * (-6.0d+0) 
! term(630) = term(630) * (-4.0d+0) 
! term(631) = term(631) * 8.0d+0 
! term(632) = term(632) * 8.0d+0 
! term(633) = term(633) * (-16.0d+0) 
! term(634) = term(634) * (-12.0d+0) 
! term(635) = term(635) * 24.0d+0 
! term(636) = term(636) * (-16.0d+0) 
! term(637) = term(637) * 32.0d+0 
! term(638) = term(638) * 8.0d+0 
! term(639) = term(639) * (-16.0d+0) 

! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! term(640) = term(640) + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, p,q,b,i) * wm_interm_70_triplet(b, a)
! term(641) = term(641) + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, p,q,b,i) * wm_interm_71_triplet(b, a)
! term(642) = term(642) + r1(vrdav_Rr, a,i) * s2(b,p,q,i) * wm_interm_38_triplet(a, b)
! term(643) = term(643) + r1(vrdav_Rr, a,i) * s2(b,p,q,i) * wm_interm_39_triplet(a, b)
! term(644) = term(644) + r1(vrdav_Rr, a,i) * s2(b,p,q,i) * wm_interm_53_triplet(a, b)
! term(645) = term(645) + r1(vrdav_Rr, a,i) * s2(b,p,q,i) * wm_interm_54_triplet(a, b)
! end do 
! end do 
! end do 

! term(640) = term(640) * (-12.0d+0) 
! term(641) = term(641) * 6.0d+0 
! term(642) = term(642) * 3.0d+0 
! term(643) = term(643) * (-3.0d+0) 
! term(644) = term(644) * (-4.0d+0) 
! term(645) = term(645) * 8.0d+0 

! do i = 1, nocc 
! do a = nocc + 1, nactive 
! do b = nocc + 1, nactive 
! term(646) = term(646) + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, b,q,p,i) * wm_interm_70_triplet(b, a)
! term(647) = term(647) + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, b,q,p,i) * wm_interm_71_triplet(b, a)
! term(648) = term(648) + r1(vrdav_Rl, b,i) * t2(a,p,q,i) * wm_interm_17_triplet(b, a)
! term(649) = term(649) + r1(vrdav_Rl, b,i) * t2(a,p,q,i) * wm_interm_20_triplet(b, a)
! term(650) = term(650) + r1(vrdav_Rl, b,i) * t2(a,p,q,i) * wm_interm_21_triplet(b, a)
! term(651) = term(651) + r1(vrdav_Rl, b,i) * t2(a,p,q,i) * wm_interm_22_triplet(b, a)
! term(652) = term(652) + r2p(vrdav_Rl, p,q,a,i) * r1(vrdav_Rr, b,i) * wm_interm_70_triplet(b, a)
! term(653) = term(653) + r2p(vrdav_Rl, p,q,a,i) * r1(vrdav_Rr, b,i) * wm_interm_71_triplet(b, a)
! term(654) = term(654) + r2m(vrdav_Rl, a,q,p,i) * r1(vrdav_Rr, b,i) * wm_interm_70_triplet(b, a)
! term(655) = term(655) + r2m(vrdav_Rl, a,q,p,i) * r1(vrdav_Rr, b,i) * wm_interm_71_triplet(b, a)
! end do 
! end do 
! end do 

! term(646) = term(646) * 8.0d+0 
! term(647) = term(647) * (-4.0d+0) 
! term(648) = term(648) * 3.0d+0 
! term(649) = term(649) * (-3.0d+0) 
! term(650) = term(650) * (-4.0d+0) 
! term(651) = term(651) * 8.0d+0 
! term(652) = term(652) * (-12.0d+0) 
! term(653) = term(653) * 6.0d+0 
! term(654) = term(654) * 8.0d+0 
! term(655) = term(655) * (-4.0d+0) 

! do a = nocc + 1, nactive 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! term(656) = term(656) + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, b,i,p,q) * wm_interm_70_triplet(b, a)
! term(657) = term(657) + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, b,i,p,q) * wm_interm_71_triplet(b, a)
! term(658) = term(658) + r2m(vrdav_Rl, a,i,p,q) * r1(vrdav_Rr, b,i) * wm_interm_70_triplet(b, a)
! term(659) = term(659) + r2m(vrdav_Rl, a,i,p,q) * r1(vrdav_Rr, b,i) * wm_interm_71_triplet(b, a)
! end do 
! end do 
! end do 

! term(656) = term(656) * (-16.0d+0) 
! term(657) = term(657) * 8.0d+0 
! term(658) = term(658) * (-16.0d+0) 
! term(659) = term(659) * 8.0d+0 


!     calc_D_vo_wm_triplet = zero
!     do s = 0, 659
!     calc_D_vo_wm_triplet = calc_D_vo_wm_triplet + term(s)
!     end do

!     end function calc_D_vo_wm_triplet
    
!     function calc_D_vv_wm_triplet(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
!     real(F64) :: calc_D_vv_wm_triplet
!     integer, intent(in) :: nocc, nactive
!     real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
!     real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
!     real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
!     real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
!     double precision, dimension(:), intent(in) :: vrdav_Rl
!     double precision, dimension(:), intent(in) :: vrdav_Rr
!     integer, intent(in) :: k1, k2
!     integer, intent(in) :: p, q
!     integer :: s , k, i, j, a, b 
!     real(F64), dimension(0:125) :: term 
!     term = 0.d+0 

!     term = 0.d+0 
!     do k = 1, nocc 
! do i = 1, nocc 
! do j = 1, nocc 
! term(0) = term(0) + wm_interm_56_triplet(q, i, j, k) * wm_interm_67_triplet(p, k, j, i)
! term(1) = term(1) + wm_interm_56_triplet(q, i, j, k) * wm_interm_72_triplet(p, k, j, i)
! term(2) = term(2) + wm_interm_60_triplet(q, i, j, k) * wm_interm_80_triplet(p, j, k, i)
! term(3) = term(3) + wm_interm_60_triplet(q, i, j, k) * wm_interm_80_triplet(p, k, j, i)
! term(4) = term(4) + wm_interm_60_triplet(q, i, j, k) * wm_interm_89_triplet(p, j, k, i)
! term(5) = term(5) + wm_interm_60_triplet(q, i, j, k) * wm_interm_89_triplet(p, k, j, i)
! term(6) = term(6) + wm_interm_60_triplet(q, i, j, k) * wm_interm_90_triplet(p, k, j, i)
! term(7) = term(7) + wm_interm_60_triplet(q, i, j, k) * wm_interm_90_triplet(p, j, k, i)
! end do 
! end do 
! end do 

! term(0) = term(0) * (-3.0d+0) 
! term(1) = term(1) * (-8.0d+0) 
! term(2) = term(2) * (-3.0d+0) 
! term(3) = term(3) * 3.0d+0 
! term(4) = term(4) * 2.0d+0 
! term(5) = term(5) * (-4.0d+0) 
! term(6) = term(6) * 2.0d+0 
! term(7) = term(7) * (-4.0d+0) 

! do k = 1, nocc 
! do j = 1, nocc 
! do i = 1, nocc 
! term(8) = term(8) + wm_interm_56_triplet(q, i, j, k) * wm_interm_67_triplet(p, k, i, j)
! term(9) = term(9) + wm_interm_56_triplet(q, i, j, k) * wm_interm_72_triplet(p, k, i, j)
! term(10) = term(10) + wm_interm_59_triplet(p, i, j, k) * wm_interm_66_triplet(q, i, j, k)
! term(11) = term(11) + wm_interm_59_triplet(p, i, j, k) * wm_interm_66_triplet(q, i, k, j)
! end do 
! end do 
! end do 

! term(8) = term(8) * 3.0d+0 
! term(9) = term(9) * 4.0d+0 
! term(10) = term(10) * (-4.0d+0) 
! term(11) = term(11) * 2.0d+0 

! do k = 1, nocc 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! do j = 1, nocc 
! term(12) = term(12) + r2p(vrdav_Rl, p,j,a,k) * t2(a,q,k,i) * wm_interm_14_triplet(j, i)
! end do 
! end do 
! end do 
! end do 

! term(12) = term(12) * (-6.0d+0) 

! do i = 1, nocc 
! do a = nocc + 1, nactive 
! do k = 1, nocc 
! do j = 1, nocc 
! term(13) = term(13) + r2p(vrdav_Rl, p,j,a,i) * t2(a,q,k,i) * wm_interm_14_triplet(j, k)
! end do 
! end do 
! end do 
! end do 

! term(13) = term(13) * 6.0d+0 

! do i = 1, nocc 
! do a = nocc + 1, nactive 
! do j = 1, nocc 
! term(14) = term(14) + r2p(vrdav_Rl, q,j,a,i) * r1(vrdav_Rr, p,i) * wm_interm_61_triplet(a, j)
! term(15) = term(15) + r2p(vrdav_Rl, q,j,a,i) * r1(vrdav_Rr, p,i) * wm_interm_65_triplet(a, j)
! end do 
! end do 
! end do 

! term(14) = term(14) * 6.0d+0 
! term(15) = term(15) * (-12.0d+0) 

! do a = nocc + 1, nactive 
! term(16) = term(16) + wm_interm_16_triplet(q, a) * wm_interm_17_triplet(p, a)
! term(17) = term(17) + wm_interm_16_triplet(q, a) * wm_interm_20_triplet(p, a)
! term(18) = term(18) + wm_interm_16_triplet(q, a) * wm_interm_21_triplet(p, a)
! term(19) = term(19) + wm_interm_16_triplet(q, a) * wm_interm_22_triplet(p, a)
! term(20) = term(20) + wm_interm_62_triplet(a, p) * wm_interm_70_triplet(q, a)
! term(21) = term(21) + wm_interm_62_triplet(a, p) * wm_interm_71_triplet(q, a)
! term(22) = term(22) + wm_interm_16_triplet(a, q) * wm_interm_17_triplet(a, p)
! term(23) = term(23) + wm_interm_16_triplet(a, q) * wm_interm_20_triplet(a, p)
! term(24) = term(24) + wm_interm_16_triplet(a, q) * wm_interm_21_triplet(a, p)
! term(25) = term(25) + wm_interm_16_triplet(a, q) * wm_interm_22_triplet(a, p)
! term(26) = term(26) + wm_interm_62_triplet(p, a) * wm_interm_70_triplet(a, q)
! term(27) = term(27) + wm_interm_62_triplet(p, a) * wm_interm_71_triplet(a, q)
! term(28) = term(28) + wm_interm_12_triplet(p, a) * wm_interm_38_triplet(q, a)
! term(29) = term(29) + wm_interm_12_triplet(p, a) * wm_interm_39_triplet(q, a)
! term(30) = term(30) + wm_interm_12_triplet(a, p) * wm_interm_38_triplet(a, q)
! term(31) = term(31) + wm_interm_12_triplet(a, p) * wm_interm_39_triplet(a, q)
! term(32) = term(32) + wm_interm_12_triplet(p, a) * wm_interm_53_triplet(q, a)
! term(33) = term(33) + wm_interm_12_triplet(p, a) * wm_interm_54_triplet(q, a)
! term(34) = term(34) + wm_interm_12_triplet(a, p) * wm_interm_53_triplet(a, q)
! term(35) = term(35) + wm_interm_12_triplet(a, p) * wm_interm_54_triplet(a, q)
! end do 

! term(16) = term(16) * (-3.0d+0) 
! term(17) = term(17) * 3.0d+0 
! term(18) = term(18) * 4.0d+0 
! term(19) = term(19) * (-8.0d+0) 
! term(20) = term(20) * (-4.0d+0) 
! term(21) = term(21) * 2.0d+0 
! term(22) = term(22) * (-3.0d+0) 
! term(23) = term(23) * 3.0d+0 
! term(24) = term(24) * 4.0d+0 
! term(25) = term(25) * (-8.0d+0) 
! term(26) = term(26) * (-4.0d+0) 
! term(27) = term(27) * 2.0d+0 
! term(28) = term(28) * 3.0d+0 
! term(29) = term(29) * (-3.0d+0) 
! term(30) = term(30) * (-3.0d+0) 
! term(31) = term(31) * 3.0d+0 
! term(32) = term(32) * 4.0d+0 
! term(33) = term(33) * (-8.0d+0) 
! term(34) = term(34) * 4.0d+0 
! term(35) = term(35) * (-8.0d+0) 

! term(36) = term(36) + wm_interm_13_triplet * wm_interm_70_triplet(p, q)
! term(37) = term(37) + wm_interm_13_triplet * wm_interm_71_triplet(p, q)

! term(36) = term(36) * 8.0d+0 
! term(37) = term(37) * (-4.0d+0) 

! do j = 1, nocc 
! do a = nocc + 1, nactive 
! do i = 1, nocc 
! term(38) = term(38) + r2p(vrdav_Rl, p,i,a,j) * r2p(vrdav_Rr, q,i,a,j)
! term(39) = term(39) + r2p(vrdav_Rl, p,i,a,j) * r2m(vrdav_Rr, a,i,q,j)
! term(40) = term(40) + r2m(vrdav_Rl, a,i,p,j) * r2p(vrdav_Rr, q,i,a,j)
! end do 
! end do 
! end do 

! term(38) = term(38) * 6.0d+0 
! term(39) = term(39) * (-6.0d+0) 
! term(40) = term(40) * (-6.0d+0) 

! do i = 1, nocc 
! term(41) = term(41) + r1(vrdav_Rl, q,i) * wm_interm_0_triplet(p, i)
! term(42) = term(42) + r1(vrdav_Rl, q,i) * wm_interm_1_triplet(p, i)
! term(43) = term(43) + r1(vrdav_Rl, q,i) * wm_interm_2_triplet(p, i)
! term(44) = term(44) + s1(q,i) * wm_interm_3_triplet(p, i)
! term(45) = term(45) + s1(q,i) * wm_interm_4_triplet(p, i)
! term(46) = term(46) + s1(q,i) * wm_interm_5_triplet(p, i)
! term(47) = term(47) + r1(vrdav_Rr, p,i) * wm_interm_6_triplet(q, i)
! term(48) = term(48) + t1(q,i) * wm_interm_7_triplet(p, i)
! term(49) = term(49) + r1(vrdav_Rr, p,i) * wm_interm_8_triplet(q, i)
! term(50) = term(50) + r1(vrdav_Rr, p,i) * wm_interm_9_triplet(q, i)
! term(51) = term(51) + t1(q,i) * wm_interm_10_triplet(p, i)
! term(52) = term(52) + t1(q,i) * wm_interm_11_triplet(p, i)
! term(53) = term(53) + wm_interm_57_triplet(p, i) * wm_interm_64_triplet(q, i)
! term(54) = term(54) + wm_interm_58_triplet(p, i) * wm_interm_64_triplet(q, i)
! term(55) = term(55) + wm_interm_58_triplet(p, i) * wm_interm_63_triplet(q, i)
! term(56) = term(56) + wm_interm_57_triplet(p, i) * wm_interm_63_triplet(q, i)
! term(57) = term(57) + wm_interm_61_triplet(q, i) * wm_interm_7_triplet(p, i)
! term(58) = term(58) + wm_interm_65_triplet(q, i) * wm_interm_7_triplet(p, i)
! term(59) = term(59) + wm_interm_10_triplet(p, i) * wm_interm_61_triplet(q, i)
! term(60) = term(60) + wm_interm_11_triplet(p, i) * wm_interm_61_triplet(q, i)
! term(61) = term(61) + wm_interm_10_triplet(p, i) * wm_interm_65_triplet(q, i)
! term(62) = term(62) + wm_interm_11_triplet(p, i) * wm_interm_65_triplet(q, i)
! term(63) = term(63) + r1(vrdav_Rl, p,i) * r1(vrdav_Rr, q,i)
! end do 

! term(41) = term(41) * 6.0d+0 
! term(42) = term(42) * 8.0d+0 
! term(43) = term(43) * (-4.0d+0) 
! term(44) = term(44) * (-6.0d+0) 
! term(45) = term(45) * 4.0d+0 
! term(46) = term(46) * (-8.0d+0) 
! term(47) = term(47) * 6.0d+0 
! term(48) = term(48) * (-6.0d+0) 
! term(49) = term(49) * (-8.0d+0) 
! term(50) = term(50) * 4.0d+0 
! term(51) = term(51) * 4.0d+0 
! term(52) = term(52) * (-8.0d+0) 
! term(53) = term(53) * 2.0d+0 
! term(54) = term(54) * (-4.0d+0) 
! term(55) = term(55) * 8.0d+0 
! term(56) = term(56) * (-4.0d+0) 
! term(57) = term(57) * (-6.0d+0) 
! term(58) = term(58) * 12.0d+0 
! term(59) = term(59) * 4.0d+0 
! term(60) = term(60) * (-8.0d+0) 
! term(61) = term(61) * (-8.0d+0) 
! term(62) = term(62) * 16.0d+0 
! term(63) = term(63) * 2.0d+0 

! do j = 1, nocc 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! term(64) = term(64) + r2p(vrdav_Rl, p,i,a,j) * r2m(vrdav_Rr, a,j,q,i)
! term(65) = term(65) + r2m(vrdav_Rl, a,j,p,i) * r2p(vrdav_Rr, q,i,a,j)
! term(66) = term(66) + r2m(vrdav_Rl, a,j,p,i) * r2m(vrdav_Rr, a,i,q,j)
! term(67) = term(67) + r2m(vrdav_Rl, a,i,p,j) * r2m(vrdav_Rr, a,j,q,i)
! term(68) = term(68) + r2m(vrdav_Rl, a,i,p,j) * r2m(vrdav_Rr, a,i,q,j)
! term(69) = term(69) + r1(vrdav_Rr, p,i) * s2(a,q,i,j) * wm_interm_64_triplet(a, j)
! term(70) = term(70) + r1(vrdav_Rr, p,i) * s2(a,q,i,j) * wm_interm_63_triplet(a, j)
! term(71) = term(71) + s2(a,p,i,j) * t1(q,i) * wm_interm_3_triplet(a, j)
! term(72) = term(72) + s2(a,p,i,j) * t1(q,i) * wm_interm_4_triplet(a, j)
! term(73) = term(73) + s2(a,p,i,j) * t1(q,i) * wm_interm_5_triplet(a, j)
! term(74) = term(74) + r1(vrdav_Rl, p,i) * t2(a,q,i,j) * wm_interm_57_triplet(a, j)
! term(75) = term(75) + r1(vrdav_Rl, p,i) * t2(a,q,i,j) * wm_interm_58_triplet(a, j)
! term(76) = term(76) + s1(p,i) * t2(a,q,i,j) * wm_interm_7_triplet(a, j)
! term(77) = term(77) + r2m(vrdav_Rl, a,i,q,j) * r1(vrdav_Rr, p,i) * wm_interm_61_triplet(a, j)
! term(78) = term(78) + r2m(vrdav_Rl, a,i,q,j) * r1(vrdav_Rr, p,i) * wm_interm_65_triplet(a, j)
! term(79) = term(79) + s1(p,i) * t2(a,q,i,j) * wm_interm_11_triplet(a, j)
! term(80) = term(80) + s1(p,i) * t2(a,q,i,j) * wm_interm_10_triplet(a, j)
! end do 
! end do 
! end do 

! term(64) = term(64) * 6.0d+0 
! term(65) = term(65) * 6.0d+0 
! term(66) = term(66) * (-4.0d+0) 
! term(67) = term(67) * (-4.0d+0) 
! term(68) = term(68) * 8.0d+0 
! term(69) = term(69) * 2.0d+0 
! term(70) = term(70) * (-4.0d+0) 
! term(71) = term(71) * (-6.0d+0) 
! term(72) = term(72) * 4.0d+0 
! term(73) = term(73) * (-8.0d+0) 
! term(74) = term(74) * 2.0d+0 
! term(75) = term(75) * (-4.0d+0) 
! term(76) = term(76) * (-6.0d+0) 
! term(77) = term(77) * 4.0d+0 
! term(78) = term(78) * (-8.0d+0) 
! term(79) = term(79) * (-8.0d+0) 
! term(80) = term(80) * 4.0d+0 

! do j = 1, nocc 
! do i = 1, nocc 
! term(81) = term(81) + r1(vrdav_Rl, q,j) * r1(vrdav_Rr, p,i) * wm_interm_68_triplet(i, j)
! term(82) = term(82) + r1(vrdav_Rl, q,j) * r1(vrdav_Rr, p,i) * wm_interm_69_triplet(i, j)
! term(83) = term(83) + r1(vrdav_Rl, p,i) * t1(q,j) * wm_interm_18_triplet(i, j)
! term(84) = term(84) + r1(vrdav_Rl, p,i) * t1(q,j) * wm_interm_19_triplet(i, j)
! term(85) = term(85) + r1(vrdav_Rl, p,i) * t1(q,j) * wm_interm_23_triplet(i, j)
! term(86) = term(86) + r1(vrdav_Rl, p,i) * t1(q,j) * wm_interm_24_triplet(i, j)
! term(87) = term(87) + r1(vrdav_Rl, p,i) * t1(q,j) * wm_interm_25_triplet(i, j)
! term(88) = term(88) + r1(vrdav_Rl, p,i) * t1(q,j) * wm_interm_26_triplet(i, j)
! term(89) = term(89) + r1(vrdav_Rr, p,i) * s1(q,j) * wm_interm_37_triplet(i, j)
! term(90) = term(90) + r1(vrdav_Rr, p,i) * s1(q,j) * wm_interm_36_triplet(i, j)
! term(91) = term(91) + r1(vrdav_Rr, p,i) * s1(q,j) * wm_interm_51_triplet(i, j)
! term(92) = term(92) + r1(vrdav_Rr, p,i) * s1(q,j) * wm_interm_52_triplet(i, j)
! end do 
! end do 

! term(81) = term(81) * 2.0d+0 
! term(82) = term(82) * (-4.0d+0) 
! term(83) = term(83) * 3.0d+0 
! term(84) = term(84) * (-3.0d+0) 
! term(85) = term(85) * 2.0d+0 
! term(86) = term(86) * (-4.0d+0) 
! term(87) = term(87) * 2.0d+0 
! term(88) = term(88) * (-4.0d+0) 
! term(89) = term(89) * (-3.0d+0) 
! term(90) = term(90) * 3.0d+0 
! term(91) = term(91) * 4.0d+0 
! term(92) = term(92) * (-8.0d+0) 

! do i = 1, nocc 
! do j = 1, nocc 
! do a = nocc + 1, nactive 
! term(93) = term(93) + r2m(vrdav_Rl, a,j,p,i) * r2m(vrdav_Rr, a,j,q,i)
! term(94) = term(94) + r1(vrdav_Rr, p,i) * s2(a,q,j,i) * wm_interm_64_triplet(a, j)
! term(95) = term(95) + r1(vrdav_Rr, p,i) * s2(a,q,j,i) * wm_interm_63_triplet(a, j)
! term(96) = term(96) + s2(a,p,j,i) * t1(q,i) * wm_interm_3_triplet(a, j)
! term(97) = term(97) + s2(a,p,j,i) * t1(q,i) * wm_interm_4_triplet(a, j)
! term(98) = term(98) + s2(a,p,j,i) * t1(q,i) * wm_interm_5_triplet(a, j)
! term(99) = term(99) + r1(vrdav_Rl, p,i) * t2(a,q,j,i) * wm_interm_58_triplet(a, j)
! term(100) = term(100) + r1(vrdav_Rl, p,i) * t2(a,q,j,i) * wm_interm_57_triplet(a, j)
! term(101) = term(101) + s1(p,i) * t2(a,q,j,i) * wm_interm_7_triplet(a, j)
! term(102) = term(102) + r2m(vrdav_Rl, a,j,q,i) * r1(vrdav_Rr, p,i) * wm_interm_61_triplet(a, j)
! term(103) = term(103) + r2m(vrdav_Rl, a,j,q,i) * r1(vrdav_Rr, p,i) * wm_interm_65_triplet(a, j)
! term(104) = term(104) + s1(p,i) * t2(a,q,j,i) * wm_interm_10_triplet(a, j)
! term(105) = term(105) + s1(p,i) * t2(a,q,j,i) * wm_interm_11_triplet(a, j)
! end do 
! end do 
! end do 

! term(93) = term(93) * 8.0d+0 
! term(94) = term(94) * (-4.0d+0) 
! term(95) = term(95) * 8.0d+0 
! term(96) = term(96) * 12.0d+0 
! term(97) = term(97) * (-8.0d+0) 
! term(98) = term(98) * 16.0d+0 
! term(99) = term(99) * 8.0d+0 
! term(100) = term(100) * (-4.0d+0) 
! term(101) = term(101) * 12.0d+0 
! term(102) = term(102) * (-8.0d+0) 
! term(103) = term(103) * 16.0d+0 
! term(104) = term(104) * (-8.0d+0) 
! term(105) = term(105) * 16.0d+0 

! do j = 1, nocc 
! do a = nocc + 1, nactive 
! do k = 1, nocc 
! do i = 1, nocc 
! term(106) = term(106) + r2p(vrdav_Rr, p,i,a,j) * s2(a,q,k,j) * wm_interm_15_triplet(i, k)
! end do 
! end do 
! end do 
! end do 

! term(106) = term(106) * 6.0d+0 

! do k = 1, nocc 
! do j = 1, nocc 
! do a = nocc + 1, nactive 
! do i = 1, nocc 
! term(107) = term(107) + r2p(vrdav_Rr, p,i,a,j) * s2(a,q,j,k) * wm_interm_15_triplet(i, k)
! end do 
! end do 
! end do 
! end do 

! term(107) = term(107) * (-6.0d+0) 

! do j = 1, nocc 
! do i = 1, nocc 
! do k = 1, nocc 
! do a = nocc + 1, nactive 
! term(108) = term(108) + r2m(vrdav_Rr, a,j,p,i) * s2(a,q,k,j) * wm_interm_15_triplet(i, k)
! term(109) = term(109) + r2m(vrdav_Rr, a,i,p,j) * s2(a,q,k,i) * wm_interm_15_triplet(j, k)
! term(110) = term(110) + s2(a,p,k,j) * t2(a,q,j,i) * wm_interm_55_triplet(i, k)
! term(111) = term(111) + s2(a,p,k,j) * t2(a,q,k,i) * wm_interm_55_triplet(i, j)
! term(112) = term(112) + s2(a,p,i,j) * t2(a,q,k,i) * wm_interm_55_triplet(k, j)
! term(113) = term(113) + r2m(vrdav_Rl, a,k,p,j) * t2(a,q,j,i) * wm_interm_14_triplet(k, i)
! term(114) = term(114) + r2m(vrdav_Rl, a,k,p,j) * t2(a,q,k,i) * wm_interm_14_triplet(j, i)
! term(115) = term(115) + r2m(vrdav_Rl, a,i,p,j) * t2(a,q,k,i) * wm_interm_14_triplet(j, k)
! end do 
! end do 
! end do 
! end do 

! term(108) = term(108) * 2.0d+0 
! term(109) = term(109) * 2.0d+0 
! term(110) = term(110) * 2.0d+0 
! term(111) = term(111) * (-4.0d+0) 
! term(112) = term(112) * 2.0d+0 
! term(113) = term(113) * 2.0d+0 
! term(114) = term(114) * (-4.0d+0) 
! term(115) = term(115) * 4.0d+0 

! do i = 1, nocc 
! do k = 1, nocc 
! do j = 1, nocc 
! do a = nocc + 1, nactive 
! term(116) = term(116) + r2m(vrdav_Rr, a,j,p,i) * s2(a,q,k,i) * wm_interm_15_triplet(j, k)
! term(117) = term(117) + s2(a,p,j,i) * t2(a,q,k,i) * wm_interm_55_triplet(k, j)
! term(118) = term(118) + r2m(vrdav_Rl, a,j,p,i) * t2(a,q,k,i) * wm_interm_14_triplet(j, k)
! end do 
! end do 
! end do 
! end do 

! term(116) = term(116) * (-4.0d+0) 
! term(117) = term(117) * (-4.0d+0) 
! term(118) = term(118) * (-8.0d+0) 

! do k = 1, nocc 
! do i = 1, nocc 
! do j = 1, nocc 
! do a = nocc + 1, nactive 
! term(119) = term(119) + r2m(vrdav_Rr, a,j,p,i) * s2(a,q,i,k) * wm_interm_15_triplet(j, k)
! term(120) = term(120) + r2m(vrdav_Rr, a,j,p,i) * s2(a,q,j,k) * wm_interm_15_triplet(i, k)
! term(121) = term(121) + r2m(vrdav_Rl, a,j,p,k) * t2(a,q,k,i) * wm_interm_14_triplet(j, i)
! term(122) = term(122) + r2m(vrdav_Rl, a,j,p,k) * t2(a,q,j,i) * wm_interm_14_triplet(k, i)
! end do 
! end do 
! end do 
! end do 

! term(119) = term(119) * 2.0d+0 
! term(120) = term(120) * (-4.0d+0) 
! term(121) = term(121) * 2.0d+0 
! term(122) = term(122) * (-4.0d+0) 

! do k = 1, nocc 
! do j = 1, nocc 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! term(123) = term(123) + r2m(vrdav_Rr, a,i,p,j) * s2(a,q,j,k) * wm_interm_15_triplet(i, k)
! term(124) = term(124) + r2m(vrdav_Rr, a,i,p,j) * s2(a,q,i,k) * wm_interm_15_triplet(j, k)
! end do 
! end do 
! end do 
! end do 

! term(123) = term(123) * 2.0d+0 
! term(124) = term(124) * (-4.0d+0) 

! do j = 1, nocc 
! do k = 1, nocc 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! term(125) = term(125) + r2m(vrdav_Rr, a,i,p,j) * s2(a,q,k,j) * wm_interm_15_triplet(i, k)
! end do 
! end do 
! end do 
! end do 

! term(125) = term(125) * (-4.0d+0) 


!     calc_D_vv_wm_triplet = zero
!     do s = 0, 125
!     calc_D_vv_wm_triplet = calc_D_vv_wm_triplet + term(s)
!     end do

!     end function calc_D_vv_wm_triplet
    
    end module density_exc_exc_functions_triplet
    
