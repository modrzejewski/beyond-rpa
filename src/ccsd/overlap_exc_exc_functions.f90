module overlap_exc_exc_functions
        use ccsd_transformed_integrals
    use cc3_intermediates
    use arithmetic
    use s_gen
    use basis
    use eom_vectors
    
    implicit none
    !
    ! File generated automatically on 2016-08-16 09:51:54
    !
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
 
    real(F64), dimension(:, :), allocatable :: wmo_interm_0 
real(F64), dimension(:, :), allocatable :: wmo_interm_1 
real(F64), dimension(:, :), allocatable :: wmo_interm_2 
real(F64), dimension(:, :), allocatable :: wmo_interm_3 
real(F64), dimension(:, :), allocatable :: wmo_interm_4 
real(F64), dimension(:, :), allocatable :: wmo_interm_5 
real(F64), dimension(:, :), allocatable :: wmo_interm_6 
real(F64), dimension(:, :), allocatable :: wmo_interm_7 
real(F64), dimension(:, :), allocatable :: wmo_interm_8 
real(F64), dimension(:, :), allocatable :: wmo_interm_9 
real(F64), dimension(:, :), allocatable :: wmo_interm_10 
real(F64), dimension(:, :), allocatable :: wmo_interm_11 
real(F64), dimension(:, :), allocatable :: wmo_interm_12 
real(F64), dimension(:, :), allocatable :: wmo_interm_13 
real(F64), dimension(:, :), allocatable :: wmo_interm_14 
real(F64), dimension(:, :), allocatable :: wmo_interm_15 
real(F64), dimension(:, :), allocatable :: wmo_interm_16 
real(F64), dimension(:, :), allocatable :: wmo_interm_17 
real(F64), dimension(:, :), allocatable :: wmo_interm_18 
real(F64), dimension(:, :), allocatable :: wmo_interm_19 
real(F64), dimension(:, :), allocatable :: wmo_interm_20 
real(F64), dimension(:, :), allocatable :: wmo_interm_21 
real(F64), dimension(:, :), allocatable :: wmo_interm_22 
real(F64), dimension(:, :), allocatable :: wmo_interm_23 
real(F64), dimension(:, :), allocatable :: wmo_interm_24 
real(F64), dimension(:, :), allocatable :: wmo_interm_25 
real(F64), dimension(:, :), allocatable :: wmo_interm_26 
real(F64), dimension(:, :), allocatable :: wmo_interm_27 
real(F64), dimension(:, :), allocatable :: wmo_interm_28 
real(F64), dimension(:, :), allocatable :: wmo_interm_29 
real(F64), dimension(:, :), allocatable :: wmo_interm_30 
real(F64), dimension(:, :), allocatable :: wmo_interm_31 
real(F64), dimension(:, :), allocatable :: wmo_interm_32 
real(F64), dimension(:, :), allocatable :: wmo_interm_33 
real(F64), dimension(:, :), allocatable :: wmo_interm_34 
real(F64), dimension(:, :), allocatable :: wmo_interm_35 
real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_36 
real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_37 
real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_38 
real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_39 
real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_40 
real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_41 
real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_42 
real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_43 
real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_44 
real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_45 
real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_46 
real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_47 

  real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_0 
real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_1 
real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_2 
real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_3 
real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_4 
real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_5 
real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_6 
real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_7 
real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_8 
real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_9 
real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_10 
real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_11 
real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_12 
real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_13 
real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_14 
real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_15 
real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_16 
real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_17 
real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_18 
real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_19 
real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_20 
real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_21 
real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_22 
real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_23 
real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_24 
real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_25 
real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_26 
real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_27 
real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_28 
real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_29 
real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_30 
real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_31 
real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_32 
real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_33 


!    real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_0 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_1 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_2 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_3 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_4 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_5 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_6 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_7 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_8 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_9 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_10 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_11 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_12 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_13 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_14 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_15 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_16 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_17 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_18 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_19 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_20 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_21 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_22 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_23 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_24 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_25 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_26 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_27 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_28 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_29 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_30 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_31 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_32 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_33 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_34 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_35 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_36 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_37 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_38 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_39 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_40 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_41 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_42 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_43 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_44 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_45 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_46 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_47 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_48 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_49 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_50 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_51 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_52 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_53 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_54 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_55 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_56 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_57 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_58 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_59 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_60 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_61 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_62 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_63 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_64 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_65 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_66 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_67 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_68 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_69 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_70 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_71 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_72 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_73 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_74 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_75 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_76 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_77 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_78 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_79 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_80 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_81 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_82 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_83 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_84 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_85 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_86 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_87 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_88 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_89 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_90 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_91 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_92 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_93 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_94 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_95 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_96 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_97 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_98 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_99 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_100


 ! real(F64), dimension(:, :), allocatable :: wmo_interm_0_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_1_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_2_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_3_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_4_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_5_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_6_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_7_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_8_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_9_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_10_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_11_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_12_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_13_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_14_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_15_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_16_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_17_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_18_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_19_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_20_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_21_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_22_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_23_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_24_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_25_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_26_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_27_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_28_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_29_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_30_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_31_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_32_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_33_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_34_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_35_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_36_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_37_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_38_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_39_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_40_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_41_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_42_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_43_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_44_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_45_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_46_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_47_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_48_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_49_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_50_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_51_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_52_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_53_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_54_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_55_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_56_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_57_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_58_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_59_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_60_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_61_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_62_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_63_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_64_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_65_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_66_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_67_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_68_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_69_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_70_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_71_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_72_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_73_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_74_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_75_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_76_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_77_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_78_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_79_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_80_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_81_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_82_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_83_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_84_triplet 

! !   real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_0_triplet 
! ! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_1_triplet 
! ! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_2_triplet 
! ! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_3_triplet 
! ! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_4_triplet 
! ! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_5_triplet 
! ! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_6_triplet 
! ! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_7_triplet 
! ! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_8_triplet 
! ! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_9_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_10_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_11_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_12_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_13_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_14_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_15_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_16_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_17_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_18_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_19_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_20_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_21_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_22_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_23_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_24_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_25_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_26_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_27_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_28_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_29_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_30_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_31_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_32_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_33_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_34_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_35_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_36_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_37_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_38_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_39_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_40_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_41_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_42_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_43_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_44_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_45_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_46_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_47_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_48_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_49_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_50_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_51_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_52_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_53_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_54_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_55_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_56_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_57_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_58_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_59_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_60_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_61_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_62_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_63_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_64_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_65_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_66_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_67_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_68_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_69_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_70_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_71_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_72_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_73_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_74_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_75_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_76_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_77_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_78_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_79_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_80_triplet 
! real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_81_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_82_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_83_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_84_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_85_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_86_triplet 
! real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_87_triplet 

    real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_0_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_1_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_2_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_3_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_4_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_5_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_6_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_7_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_8_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_9_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_10_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_11_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_12_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_13_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_14_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_15_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_16_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_17_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_18_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_19_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_20_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_21_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_22_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_23_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_24_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_25_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_26_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_27_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_28_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_29_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_30_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_31_triplet 
real(F64), dimension(:, :), allocatable :: wmo_interm_cc3_32_triplet 
real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_33_triplet 
real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_34_triplet 
real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_35_triplet 
real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_36_triplet 
real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_37_triplet 
real(F64), dimension(:, :, :, :), allocatable :: wmo_interm_cc3_38_triplet 

    contains

    subroutine d_overlap_ccsd_init(nocc, nactive)
    integer, intent(in) :: nocc
    integer, intent(in) :: nactive
    allocate(wmo_interm_0(1: nocc, 1: nocc))
allocate(wmo_interm_1(1: nocc, 1: nocc))
allocate(wmo_interm_2(1: nocc, 1: nocc))
allocate(wmo_interm_3(1: nocc, 1: nocc))
allocate(wmo_interm_4(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_5(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_6(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_7(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_8(1: nocc, 1: nocc))
allocate(wmo_interm_9(1: nocc, 1: nocc))
allocate(wmo_interm_10(nocc+1: nactive, nocc+1: nactive))
allocate(wmo_interm_11(nocc+1: nactive, nocc+1: nactive))
allocate(wmo_interm_12(nocc+1: nactive, nocc+1: nactive))
allocate(wmo_interm_13(nocc+1: nactive, nocc+1: nactive))
allocate(wmo_interm_14(nocc+1: nactive, nocc+1: nactive))
allocate(wmo_interm_15(nocc+1: nactive, nocc+1: nactive))
allocate(wmo_interm_16(1: nocc, 1: nocc))
allocate(wmo_interm_17(1: nocc, 1: nocc))
allocate(wmo_interm_18(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_19(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_20(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_21(nocc+1: nactive, nocc+1: nactive))
allocate(wmo_interm_22(nocc+1: nactive, nocc+1: nactive))
allocate(wmo_interm_23(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_24(nocc+1: nactive, nocc+1: nactive))
allocate(wmo_interm_25(1: nocc, 1: nocc))
allocate(wmo_interm_26(1: nocc, 1: nocc))
allocate(wmo_interm_27(1: nocc, 1: nocc))
allocate(wmo_interm_28(1: nocc, 1: nocc))
allocate(wmo_interm_29(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_30(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_31(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_32(nocc+1: nactive, nocc+1: nactive))
allocate(wmo_interm_33(nocc+1: nactive, nocc+1: nactive))
allocate(wmo_interm_34(nocc+1: nactive, nocc+1: nactive))
allocate(wmo_interm_35(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_36(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wmo_interm_37(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wmo_interm_38(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wmo_interm_39(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wmo_interm_40(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wmo_interm_41(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wmo_interm_42(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wmo_interm_43(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wmo_interm_44(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wmo_interm_45(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wmo_interm_46(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wmo_interm_47(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
wmo_interm_0 = zero 
wmo_interm_1 = zero 
wmo_interm_2 = zero 
wmo_interm_3 = zero 
wmo_interm_4 = zero 
wmo_interm_5 = zero 
wmo_interm_6 = zero 
wmo_interm_7 = zero 
wmo_interm_8 = zero 
wmo_interm_9 = zero 
wmo_interm_10 = zero 
wmo_interm_11 = zero 
wmo_interm_12 = zero 
wmo_interm_13 = zero 
wmo_interm_14 = zero 
wmo_interm_15 = zero 
wmo_interm_16 = zero 
wmo_interm_17 = zero 
wmo_interm_18 = zero 
wmo_interm_19 = zero 
wmo_interm_20 = zero 
wmo_interm_21 = zero 
wmo_interm_22 = zero 
wmo_interm_23 = zero 
wmo_interm_24 = zero 
wmo_interm_25 = zero 
wmo_interm_26 = zero 
wmo_interm_27 = zero 
wmo_interm_28 = zero 
wmo_interm_29 = zero 
wmo_interm_30 = zero 
wmo_interm_31 = zero 
wmo_interm_32 = zero 
wmo_interm_33 = zero 
wmo_interm_34 = zero 
wmo_interm_35 = zero 
wmo_interm_36 = zero 
wmo_interm_37 = zero 
wmo_interm_38 = zero 
wmo_interm_39 = zero 
wmo_interm_40 = zero 
wmo_interm_41 = zero 
wmo_interm_42 = zero 
wmo_interm_43 = zero 
wmo_interm_44 = zero 
wmo_interm_45 = zero 
wmo_interm_46 = zero 
wmo_interm_47 = zero 

    end subroutine d_overlap_ccsd_init
    
    subroutine d_overlap_ccsd_free
    deallocate(wmo_interm_0)
deallocate(wmo_interm_1)
deallocate(wmo_interm_2)
deallocate(wmo_interm_3)
deallocate(wmo_interm_4)
deallocate(wmo_interm_5)
deallocate(wmo_interm_6)
deallocate(wmo_interm_7)
deallocate(wmo_interm_8)
deallocate(wmo_interm_9)
deallocate(wmo_interm_10)
deallocate(wmo_interm_11)
deallocate(wmo_interm_12)
deallocate(wmo_interm_13)
deallocate(wmo_interm_14)
deallocate(wmo_interm_15)
deallocate(wmo_interm_16)
deallocate(wmo_interm_17)
deallocate(wmo_interm_18)
deallocate(wmo_interm_19)
deallocate(wmo_interm_20)
deallocate(wmo_interm_21)
deallocate(wmo_interm_22)
deallocate(wmo_interm_23)
deallocate(wmo_interm_24)
deallocate(wmo_interm_25)
deallocate(wmo_interm_26)
deallocate(wmo_interm_27)
deallocate(wmo_interm_28)
deallocate(wmo_interm_29)
deallocate(wmo_interm_30)
deallocate(wmo_interm_31)
deallocate(wmo_interm_32)
deallocate(wmo_interm_33)
deallocate(wmo_interm_34)
deallocate(wmo_interm_35)
deallocate(wmo_interm_36)
deallocate(wmo_interm_37)
deallocate(wmo_interm_38)
deallocate(wmo_interm_39)
deallocate(wmo_interm_40)
deallocate(wmo_interm_41)
deallocate(wmo_interm_42)
deallocate(wmo_interm_43)
deallocate(wmo_interm_44)
deallocate(wmo_interm_45)
deallocate(wmo_interm_46)
deallocate(wmo_interm_47)

    end subroutine d_overlap_ccsd_free
    
    subroutine d_overlap_intermediates_ccsd(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr)
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    double precision, dimension(:), intent(in) :: vrdav_Rl
    double precision, dimension(:), intent(in) :: vrdav_Rr
    real(F64) :: sum
    integer :: a, i, j, b, k, c, l 

    !$omp parallel private(a, i, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do i = 1, nocc 
do j = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * r1(vrdav_Rr, a,j)
end do 
wmo_interm_0(i, j) = wmo_interm_0(i, j) + sum 
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
wmo_interm_1(i, j) = wmo_interm_1(i, j) + sum 
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
wmo_interm_2(i, j) = wmo_interm_2(i, j) + sum 
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
wmo_interm_3(i, j) = wmo_interm_3(i, j) + sum 
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
wmo_interm_4(b, j) = wmo_interm_4(b, j) + sum 
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
sum = sum + r2(vrdav_Rr, a,i,b,j) * s1(a,i)
end do 
end do 
wmo_interm_5(b, j) = wmo_interm_5(b, j) + sum 
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
sum = sum + r2(vrdav_Rr, a,j,b,i) * s1(a,i)
end do 
end do 
wmo_interm_6(b, j) = wmo_interm_6(b, j) + sum 
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
wmo_interm_7(b, j) = wmo_interm_7(b, j) + sum 
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
sum = sum + r2(vrdav_Rl, a,j,b,i) * r2(vrdav_Rr, a,i,b,k)
end do 
end do 
end do 
wmo_interm_8(j, k) = wmo_interm_8(j, k) + sum 
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
sum = sum + r2(vrdav_Rl, a,j,b,i) * r2(vrdav_Rr, a,k,b,i)
end do 
end do 
end do 
wmo_interm_9(j, k) = wmo_interm_9(j, k) + sum 
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
sum = sum + r2(vrdav_Rl, a,j,b,i) * r2(vrdav_Rr, a,i,c,j)
end do 
end do 
end do 
wmo_interm_10(b, c) = wmo_interm_10(b, c) + sum 
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
wmo_interm_11(a, b) = wmo_interm_11(a, b) + sum 
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
sum = sum + r2(vrdav_Rl, a,j,b,i) * r2(vrdav_Rr, a,j,c,i)
end do 
end do 
end do 
wmo_interm_12(b, c) = wmo_interm_12(b, c) + sum 
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
wmo_interm_13(a, b) = wmo_interm_13(a, b) + sum 
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
sum = sum + r2(vrdav_Rr, a,j,b,i) * s2(a,c,i,j)
end do 
end do 
end do 
wmo_interm_14(b, c) = wmo_interm_14(b, c) + sum 
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
sum = sum + r2(vrdav_Rr, a,j,b,i) * s2(a,c,j,i)
end do 
end do 
end do 
wmo_interm_15(b, c) = wmo_interm_15(b, c) + sum 
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
sum = sum + r2(vrdav_Rr, a,j,b,i) * s2(a,b,i,k)
end do 
end do 
end do 
wmo_interm_16(j, k) = wmo_interm_16(j, k) + sum 
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
sum = sum + r2(vrdav_Rr, a,j,b,i) * s2(a,b,k,i)
end do 
end do 
end do 
wmo_interm_17(j, k) = wmo_interm_17(j, k) + sum 
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
sum = sum + r2(vrdav_Rl, a,i,b,j) * r1(vrdav_Rr, a,i)
end do 
end do 
wmo_interm_18(b, j) = wmo_interm_18(b, j) + sum 
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
wmo_interm_19(b, j) = wmo_interm_19(b, j) + sum 
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
wmo_interm_20(b, j) = wmo_interm_20(b, j) + sum 
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
wmo_interm_21(b, c) = wmo_interm_21(b, c) + sum 
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
wmo_interm_22(a, b) = wmo_interm_22(a, b) + sum 
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
wmo_interm_23(b, j) = wmo_interm_23(b, j) + sum 
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
wmo_interm_24(b, c) = wmo_interm_24(b, c) + sum 
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
wmo_interm_25(j, k) = wmo_interm_25(j, k) + sum 
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
wmo_interm_26(j, k) = wmo_interm_26(j, k) + sum 
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
wmo_interm_27(j, k) = wmo_interm_27(j, k) + sum 
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
wmo_interm_28(j, k) = wmo_interm_28(j, k) + sum 
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
wmo_interm_29(b, j) = wmo_interm_29(b, j) + sum 
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
wmo_interm_30(b, j) = wmo_interm_30(b, j) + sum 
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
wmo_interm_31(b, j) = wmo_interm_31(b, j) + sum 
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
wmo_interm_32(a, b) = wmo_interm_32(a, b) + sum 
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
wmo_interm_33(b, c) = wmo_interm_33(b, c) + sum 
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
wmo_interm_34(b, c) = wmo_interm_34(b, c) + sum 
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
wmo_interm_35(b, j) = wmo_interm_35(b, j) + sum 
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
sum = sum + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,k,b,l)
end do 
end do 
wmo_interm_36(i, j, k, l) = wmo_interm_36(i, j, k, l) + sum 
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
wmo_interm_37(i, j, k, l) = wmo_interm_37(i, j, k, l) + sum 
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
wmo_interm_38(i, j, k, l) = wmo_interm_38(i, j, k, l) + sum 
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
sum = sum + r2(vrdav_Rr, a,i,b,j) * s2(a,b,k,l)
end do 
end do 
wmo_interm_39(i, j, k, l) = wmo_interm_39(i, j, k, l) + sum 
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
sum = sum + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,k,c,i)
end do 
end do 
wmo_interm_40(b, c, j, k) = wmo_interm_40(b, c, j, k) + sum 
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
wmo_interm_41(b, c, j, k) = wmo_interm_41(b, c, j, k) + sum 
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
sum = sum + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,i,c,k)
end do 
end do 
wmo_interm_42(b, c, j, k) = wmo_interm_42(b, c, j, k) + sum 
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
wmo_interm_43(b, c, j, k) = wmo_interm_43(b, c, j, k) + sum 
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
wmo_interm_44(b, c, j, k) = wmo_interm_44(b, c, j, k) + sum 
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
wmo_interm_45(b, c, j, k) = wmo_interm_45(b, c, j, k) + sum 
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
sum = sum + r2(vrdav_Rl, a,j,b,i) * r2(vrdav_Rr, a,i,c,k)
end do 
end do 
wmo_interm_46(b, c, j, k) = wmo_interm_46(b, c, j, k) + sum 
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
sum = sum + r2(vrdav_Rl, a,j,b,i) * r2(vrdav_Rr, a,k,c,i)
end do 
end do 
wmo_interm_47(b, c, j, k) = wmo_interm_47(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 



    end subroutine d_overlap_intermediates_ccsd

    subroutine d_overlap_cc3_init(nocc, nactive)
      integer, intent(in) :: nocc
      integer, intent(in) :: nactive
    allocate(wmo_interm_cc3_0(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_cc3_1(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_cc3_2(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_cc3_3(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_cc3_4(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_cc3_5(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_cc3_6(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_cc3_7(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_cc3_8(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_cc3_9(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_cc3_10(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_cc3_11(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_cc3_12(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_cc3_13(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_cc3_14(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_cc3_15(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_cc3_16(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_cc3_17(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_cc3_18(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_cc3_19(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_cc3_20(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wmo_interm_cc3_21(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wmo_interm_cc3_22(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wmo_interm_cc3_23(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wmo_interm_cc3_24(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wmo_interm_cc3_25(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wmo_interm_cc3_26(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wmo_interm_cc3_27(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wmo_interm_cc3_28(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wmo_interm_cc3_29(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wmo_interm_cc3_30(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wmo_interm_cc3_31(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wmo_interm_cc3_32(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wmo_interm_cc3_33(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
wmo_interm_cc3_0 = zero 
wmo_interm_cc3_1 = zero 
wmo_interm_cc3_2 = zero 
wmo_interm_cc3_3 = zero 
wmo_interm_cc3_4 = zero 
wmo_interm_cc3_5 = zero 
wmo_interm_cc3_6 = zero 
wmo_interm_cc3_7 = zero 
wmo_interm_cc3_8 = zero 
wmo_interm_cc3_9 = zero 
wmo_interm_cc3_10 = zero 
wmo_interm_cc3_11 = zero 
wmo_interm_cc3_12 = zero 
wmo_interm_cc3_13 = zero 
wmo_interm_cc3_14 = zero 
wmo_interm_cc3_15 = zero 
wmo_interm_cc3_16 = zero 
wmo_interm_cc3_17 = zero 
wmo_interm_cc3_18 = zero 
wmo_interm_cc3_19 = zero 
wmo_interm_cc3_20 = zero 
wmo_interm_cc3_21 = zero 
wmo_interm_cc3_22 = zero 
wmo_interm_cc3_23 = zero 
wmo_interm_cc3_24 = zero 
wmo_interm_cc3_25 = zero 
wmo_interm_cc3_26 = zero 
wmo_interm_cc3_27 = zero 
wmo_interm_cc3_28 = zero 
wmo_interm_cc3_29 = zero 
wmo_interm_cc3_30 = zero 
wmo_interm_cc3_31 = zero 
wmo_interm_cc3_32 = zero 
wmo_interm_cc3_33 = zero 



!     allocate(wmo_interm_cc3_0(nocc+1: nactive, nocc+1: nactive))
! allocate(wmo_interm_cc3_1(nocc+1: nactive, nocc+1: nactive))
! allocate(wmo_interm_cc3_2(nocc+1: nactive, nocc+1: nactive))
! allocate(wmo_interm_cc3_3(nocc+1: nactive, nocc+1: nactive))
! allocate(wmo_interm_cc3_4(nocc+1: nactive, nocc+1: nactive))
! allocate(wmo_interm_cc3_5(nocc+1: nactive, nocc+1: nactive))
! allocate(wmo_interm_cc3_6(nocc+1: nactive, nocc+1: nactive))
! allocate(wmo_interm_cc3_7(1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_8(1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_9(1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_10(1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_11(1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_12(1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_13(1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_14(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_15(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_16(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_17(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_18(nocc+1: nactive, 1: nocc))
! allocate(wmo_interm_cc3_19(nocc+1: nactive, 1: nocc))
! allocate(wmo_interm_cc3_20(nocc+1: nactive, 1: nocc))
! allocate(wmo_interm_cc3_21(nocc+1: nactive, 1: nocc))
! allocate(wmo_interm_cc3_22(nocc+1: nactive, 1: nocc))
! allocate(wmo_interm_cc3_23(nocc+1: nactive, 1: nocc))
! allocate(wmo_interm_cc3_24(nocc+1: nactive, 1: nocc))
! allocate(wmo_interm_cc3_25(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_26(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_27(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_28(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_29(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_30(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_31(nocc+1: nactive, 1: nocc))
! allocate(wmo_interm_cc3_32(nocc+1: nactive, nocc+1: nactive))
! allocate(wmo_interm_cc3_33(nocc+1: nactive, nocc+1: nactive))
! allocate(wmo_interm_cc3_34(nocc+1: nactive, nocc+1: nactive))
! allocate(wmo_interm_cc3_35(nocc+1: nactive, nocc+1: nactive))
! allocate(wmo_interm_cc3_36(nocc+1: nactive, nocc+1: nactive))
! allocate(wmo_interm_cc3_37(nocc+1: nactive, nocc+1: nactive))
! allocate(wmo_interm_cc3_38(nocc+1: nactive, nocc+1: nactive))
! allocate(wmo_interm_cc3_39(1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_40(1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_41(1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_42(1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_43(1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_44(1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_45(1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_46(nocc+1: nactive, 1: nocc))
! allocate(wmo_interm_cc3_47(nocc+1: nactive, 1: nocc))
! allocate(wmo_interm_cc3_48(nocc+1: nactive, 1: nocc))
! allocate(wmo_interm_cc3_49(nocc+1: nactive, 1: nocc))
! allocate(wmo_interm_cc3_50(nocc+1: nactive, 1: nocc))
! allocate(wmo_interm_cc3_51(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_52(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_53(nocc+1: nactive, 1: nocc))
! allocate(wmo_interm_cc3_54(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_55(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_56(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_57(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_58(nocc+1: nactive, 1: nocc))
! allocate(wmo_interm_cc3_59(nocc+1: nactive, 1: nocc))
! allocate(wmo_interm_cc3_60(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_61(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_62(nocc+1: nactive, 1: nocc))
! allocate(wmo_interm_cc3_63(nocc+1: nactive, 1: nocc))
! allocate(wmo_interm_cc3_64(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_65(nocc+1: nactive, 1: nocc))
! allocate(wmo_interm_cc3_66(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_67(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_68(nocc+1: nactive, 1: nocc))
! allocate(wmo_interm_cc3_69(1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_70(1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_71(1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_72(1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_73(1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_74(1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_75(1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_76(1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_77(1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_78(1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_79(1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_80(nocc+1: nactive, nocc+1: nactive))
! allocate(wmo_interm_cc3_81(nocc+1: nactive, nocc+1: nactive))
! allocate(wmo_interm_cc3_82(nocc+1: nactive, nocc+1: nactive))
! allocate(wmo_interm_cc3_83(nocc+1: nactive, nocc+1: nactive))
! allocate(wmo_interm_cc3_84(nocc+1: nactive, nocc+1: nactive))
! allocate(wmo_interm_cc3_85(nocc+1: nactive, nocc+1: nactive))
! allocate(wmo_interm_cc3_86(nocc+1: nactive, nocc+1: nactive))
! allocate(wmo_interm_cc3_87(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_88(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_89(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_90(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_91(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_92(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_93(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_94(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_95(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_96(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_97(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_98(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_99(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_100(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
! wmo_interm_cc3_0 = zero 
! wmo_interm_cc3_1 = zero 
! wmo_interm_cc3_2 = zero 
! wmo_interm_cc3_3 = zero 
! wmo_interm_cc3_4 = zero 
! wmo_interm_cc3_5 = zero 
! wmo_interm_cc3_6 = zero 
! wmo_interm_cc3_7 = zero 
! wmo_interm_cc3_8 = zero 
! wmo_interm_cc3_9 = zero 
! wmo_interm_cc3_10 = zero 
! wmo_interm_cc3_11 = zero 
! wmo_interm_cc3_12 = zero 
! wmo_interm_cc3_13 = zero 
! wmo_interm_cc3_14 = zero 
! wmo_interm_cc3_15 = zero 
! wmo_interm_cc3_16 = zero 
! wmo_interm_cc3_17 = zero 
! wmo_interm_cc3_18 = zero 
! wmo_interm_cc3_19 = zero 
! wmo_interm_cc3_20 = zero 
! wmo_interm_cc3_21 = zero 
! wmo_interm_cc3_22 = zero 
! wmo_interm_cc3_23 = zero 
! wmo_interm_cc3_24 = zero 
! wmo_interm_cc3_25 = zero 
! wmo_interm_cc3_26 = zero 
! wmo_interm_cc3_27 = zero 
! wmo_interm_cc3_28 = zero 
! wmo_interm_cc3_29 = zero 
! wmo_interm_cc3_30 = zero 
! wmo_interm_cc3_31 = zero 
! wmo_interm_cc3_32 = zero 
! wmo_interm_cc3_33 = zero 
! wmo_interm_cc3_34 = zero 
! wmo_interm_cc3_35 = zero 
! wmo_interm_cc3_36 = zero 
! wmo_interm_cc3_37 = zero 
! wmo_interm_cc3_38 = zero 
! wmo_interm_cc3_39 = zero 
! wmo_interm_cc3_40 = zero 
! wmo_interm_cc3_41 = zero 
! wmo_interm_cc3_42 = zero 
! wmo_interm_cc3_43 = zero 
! wmo_interm_cc3_44 = zero 
! wmo_interm_cc3_45 = zero 
! wmo_interm_cc3_46 = zero 
! wmo_interm_cc3_47 = zero 
! wmo_interm_cc3_48 = zero 
! wmo_interm_cc3_49 = zero 
! wmo_interm_cc3_50 = zero 
! wmo_interm_cc3_51 = zero 
! wmo_interm_cc3_52 = zero 
! wmo_interm_cc3_53 = zero 
! wmo_interm_cc3_54 = zero 
! wmo_interm_cc3_55 = zero 
! wmo_interm_cc3_56 = zero 
! wmo_interm_cc3_57 = zero 
! wmo_interm_cc3_58 = zero 
! wmo_interm_cc3_59 = zero 
! wmo_interm_cc3_60 = zero 
! wmo_interm_cc3_61 = zero 
! wmo_interm_cc3_62 = zero 
! wmo_interm_cc3_63 = zero 
! wmo_interm_cc3_64 = zero 
! wmo_interm_cc3_65 = zero 
! wmo_interm_cc3_66 = zero 
! wmo_interm_cc3_67 = zero 
! wmo_interm_cc3_68 = zero 
! wmo_interm_cc3_69 = zero 
! wmo_interm_cc3_70 = zero 
! wmo_interm_cc3_71 = zero 
! wmo_interm_cc3_72 = zero 
! wmo_interm_cc3_73 = zero 
! wmo_interm_cc3_74 = zero 
! wmo_interm_cc3_75 = zero 
! wmo_interm_cc3_76 = zero 
! wmo_interm_cc3_77 = zero 
! wmo_interm_cc3_78 = zero 
! wmo_interm_cc3_79 = zero 
! wmo_interm_cc3_80 = zero 
! wmo_interm_cc3_81 = zero 
! wmo_interm_cc3_82 = zero 
! wmo_interm_cc3_83 = zero 
! wmo_interm_cc3_84 = zero 
! wmo_interm_cc3_85 = zero 
! wmo_interm_cc3_86 = zero 
! wmo_interm_cc3_87 = zero 
! wmo_interm_cc3_88 = zero 
! wmo_interm_cc3_89 = zero 
! wmo_interm_cc3_90 = zero 
! wmo_interm_cc3_91 = zero 
! wmo_interm_cc3_92 = zero 
! wmo_interm_cc3_93 = zero 
! wmo_interm_cc3_94 = zero 
! wmo_interm_cc3_95 = zero 
! wmo_interm_cc3_96 = zero 
! wmo_interm_cc3_97 = zero 
! wmo_interm_cc3_98 = zero 
! wmo_interm_cc3_99 = zero 
! wmo_interm_cc3_100 = zero 

    end subroutine d_overlap_cc3_init
    
    subroutine d_overlap_cc3_free
    deallocate(wmo_interm_cc3_0)
deallocate(wmo_interm_cc3_1)
deallocate(wmo_interm_cc3_2)
deallocate(wmo_interm_cc3_3)
deallocate(wmo_interm_cc3_4)
deallocate(wmo_interm_cc3_5)
deallocate(wmo_interm_cc3_6)
deallocate(wmo_interm_cc3_7)
deallocate(wmo_interm_cc3_8)
deallocate(wmo_interm_cc3_9)
deallocate(wmo_interm_cc3_10)
deallocate(wmo_interm_cc3_11)
deallocate(wmo_interm_cc3_12)
deallocate(wmo_interm_cc3_13)
deallocate(wmo_interm_cc3_14)
deallocate(wmo_interm_cc3_15)
deallocate(wmo_interm_cc3_16)
deallocate(wmo_interm_cc3_17)
deallocate(wmo_interm_cc3_18)
deallocate(wmo_interm_cc3_19)
deallocate(wmo_interm_cc3_20)
deallocate(wmo_interm_cc3_21)
deallocate(wmo_interm_cc3_22)
deallocate(wmo_interm_cc3_23)
deallocate(wmo_interm_cc3_24)
deallocate(wmo_interm_cc3_25)
deallocate(wmo_interm_cc3_26)
deallocate(wmo_interm_cc3_27)
deallocate(wmo_interm_cc3_28)
deallocate(wmo_interm_cc3_29)
deallocate(wmo_interm_cc3_30)
deallocate(wmo_interm_cc3_31)
deallocate(wmo_interm_cc3_32)
deallocate(wmo_interm_cc3_33)
! deallocate(wmo_interm_cc3_34)
! deallocate(wmo_interm_cc3_35)
! deallocate(wmo_interm_cc3_36)
! deallocate(wmo_interm_cc3_37)
! deallocate(wmo_interm_cc3_38)
! deallocate(wmo_interm_cc3_39)
! deallocate(wmo_interm_cc3_40)
! deallocate(wmo_interm_cc3_41)
! deallocate(wmo_interm_cc3_42)
! deallocate(wmo_interm_cc3_43)
! deallocate(wmo_interm_cc3_44)
! deallocate(wmo_interm_cc3_45)
! deallocate(wmo_interm_cc3_46)
! deallocate(wmo_interm_cc3_47)
! deallocate(wmo_interm_cc3_48)
! deallocate(wmo_interm_cc3_49)
! deallocate(wmo_interm_cc3_50)
! deallocate(wmo_interm_cc3_51)
! deallocate(wmo_interm_cc3_52)
! deallocate(wmo_interm_cc3_53)
! deallocate(wmo_interm_cc3_54)
! deallocate(wmo_interm_cc3_55)
! deallocate(wmo_interm_cc3_56)
! deallocate(wmo_interm_cc3_57)
! deallocate(wmo_interm_cc3_58)
! deallocate(wmo_interm_cc3_59)
! deallocate(wmo_interm_cc3_60)
! deallocate(wmo_interm_cc3_61)
! deallocate(wmo_interm_cc3_62)
! deallocate(wmo_interm_cc3_63)
! deallocate(wmo_interm_cc3_64)
! deallocate(wmo_interm_cc3_65)
! deallocate(wmo_interm_cc3_66)
! deallocate(wmo_interm_cc3_67)
! deallocate(wmo_interm_cc3_68)
! deallocate(wmo_interm_cc3_69)
! deallocate(wmo_interm_cc3_70)
! deallocate(wmo_interm_cc3_71)
! deallocate(wmo_interm_cc3_72)
! deallocate(wmo_interm_cc3_73)
! deallocate(wmo_interm_cc3_74)
! deallocate(wmo_interm_cc3_75)
! deallocate(wmo_interm_cc3_76)
! deallocate(wmo_interm_cc3_77)
! deallocate(wmo_interm_cc3_78)
! deallocate(wmo_interm_cc3_79)
! deallocate(wmo_interm_cc3_80)
! deallocate(wmo_interm_cc3_81)
! deallocate(wmo_interm_cc3_82)
! deallocate(wmo_interm_cc3_83)
! deallocate(wmo_interm_cc3_84)
! deallocate(wmo_interm_cc3_85)
! deallocate(wmo_interm_cc3_86)
! deallocate(wmo_interm_cc3_87)
! deallocate(wmo_interm_cc3_88)
! deallocate(wmo_interm_cc3_89)
! deallocate(wmo_interm_cc3_90)
! deallocate(wmo_interm_cc3_91)
! deallocate(wmo_interm_cc3_92)
! deallocate(wmo_interm_cc3_93)
! deallocate(wmo_interm_cc3_94)
! deallocate(wmo_interm_cc3_95)
! deallocate(wmo_interm_cc3_96)
! deallocate(wmo_interm_cc3_97)
! deallocate(wmo_interm_cc3_98)
! deallocate(wmo_interm_cc3_99)
! deallocate(wmo_interm_cc3_100)

    end subroutine d_overlap_cc3_free
    
    subroutine d_overlap_intermediates_cc3(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr)
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    double precision, dimension(:), intent(in) :: vrdav_Rl
    double precision, dimension(:), intent(in) :: vrdav_Rr
    real(F64) :: sum
    integer :: a, i, b, j, c, k 

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
wmo_interm_cc3_0(b, j) = wmo_interm_cc3_0(b, j) + sum 
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
sum = sum + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,k,i,j)
end do 
end do 
end do 
end do 
wmo_interm_cc3_1(c, k) = wmo_interm_cc3_1(c, k) + sum 
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
sum = sum + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,k,j,i)
end do 
end do 
end do 
end do 
wmo_interm_cc3_2(c, k) = wmo_interm_cc3_2(c, k) + sum 
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
sum = sum + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,j,k,i)
end do 
end do 
end do 
end do 
wmo_interm_cc3_3(c, k) = wmo_interm_cc3_3(c, k) + sum 
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
sum = sum + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,i,k,j)
end do 
end do 
end do 
end do 
wmo_interm_cc3_4(c, k) = wmo_interm_cc3_4(c, k) + sum 
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
sum = sum + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,j,i,k)
end do 
end do 
end do 
end do 
wmo_interm_cc3_5(c, k) = wmo_interm_cc3_5(c, k) + sum 
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
sum = sum + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,i,j,k)
end do 
end do 
end do 
end do 
wmo_interm_cc3_6(c, k) = wmo_interm_cc3_6(c, k) + sum 
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
wmo_interm_cc3_7(b, j) = wmo_interm_cc3_7(b, j) + sum 
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
sum = sum + r2(vrdav_Rl, a,i,b,j) * r1(vrdav_Rr, a,i)
end do 
end do 
wmo_interm_cc3_8(b, j) = wmo_interm_cc3_8(b, j) + sum 
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
wmo_interm_cc3_9(c, k) = wmo_interm_cc3_9(c, k) + sum 
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
wmo_interm_cc3_10(c, k) = wmo_interm_cc3_10(c, k) + sum 
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
wmo_interm_cc3_11(c, k) = wmo_interm_cc3_11(c, k) + sum 
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
wmo_interm_cc3_12(c, k) = wmo_interm_cc3_12(c, k) + sum 
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
wmo_interm_cc3_13(b, j) = wmo_interm_cc3_13(b, j) + sum 
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
wmo_interm_cc3_14(c, k) = wmo_interm_cc3_14(c, k) + sum 
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
wmo_interm_cc3_15(b, j) = wmo_interm_cc3_15(b, j) + sum 
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
wmo_interm_cc3_16(c, k) = wmo_interm_cc3_16(c, k) + sum 
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
wmo_interm_cc3_17(b, j) = wmo_interm_cc3_17(b, j) + sum 
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
wmo_interm_cc3_18(c, k) = wmo_interm_cc3_18(c, k) + sum 
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
wmo_interm_cc3_19(c, k) = wmo_interm_cc3_19(c, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wmo_interm_cc3_20(b, c, j, k) = wmo_interm_cc3_20(b, c, j, k) + sum 
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
sum = sum + s1(a,i) * t3(nocc, nactive, a,b,c,k,j,i)
end do 
end do 
wmo_interm_cc3_21(b, c, k, j) = wmo_interm_cc3_21(b, c, k, j) + sum 
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
sum = sum + s1(a,i) * t3(nocc, nactive, a,b,c,k,i,j)
end do 
end do 
wmo_interm_cc3_22(b, c, k, j) = wmo_interm_cc3_22(b, c, k, j) + sum 
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
sum = sum + s1(a,i) * t3(nocc, nactive, a,b,c,i,j,k)
end do 
end do 
wmo_interm_cc3_23(b, c, j, k) = wmo_interm_cc3_23(b, c, j, k) + sum 
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
wmo_interm_cc3_24(b, c, k, j) = wmo_interm_cc3_24(b, c, k, j) + sum 
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
wmo_interm_cc3_25(b, c, j, k) = wmo_interm_cc3_25(b, c, j, k) + sum 
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
wmo_interm_cc3_26(b, c, j, k) = wmo_interm_cc3_26(b, c, j, k) + sum 
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
wmo_interm_cc3_27(b, c, k, j) = wmo_interm_cc3_27(b, c, k, j) + sum 
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
wmo_interm_cc3_28(b, c, j, k) = wmo_interm_cc3_28(b, c, j, k) + sum 
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
wmo_interm_cc3_29(b, c, j, k) = wmo_interm_cc3_29(b, c, j, k) + sum 
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
wmo_interm_cc3_30(b, c, j, k) = wmo_interm_cc3_30(b, c, j, k) + sum 
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
sum = sum + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,c,i,j,k)
end do 
end do 
wmo_interm_cc3_31(b, c, j, k) = wmo_interm_cc3_31(b, c, j, k) + sum 
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
wmo_interm_cc3_32(b, c, j, k) = wmo_interm_cc3_32(b, c, j, k) + sum 
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
wmo_interm_cc3_33(b, c, j, k) = wmo_interm_cc3_33(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!     !$omp parallel private(i, a, b, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do a = nocc + 1, nactive 
! do b = nocc + 1, nactive 
! sum = zero 
! do i = 1, nocc 
! sum = sum + r1(vrdav_Rl, a,i) * t1(b,i)
! end do 
! wmo_interm_cc3_0(a, b) = wmo_interm_cc3_0(a, b) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, k, c, d, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do c = nocc + 1, nactive 
! do d = nocc + 1, nactive 
! sum = zero 
! do k = 1, nocc 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do j = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r3(vrdav_Rr, a,j,b,k,c,i) * t3(nocc, nactive, a,b,d,i,j,k)
! end do 
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_1(c, d) = wmo_interm_cc3_1(c, d) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, k, c, d, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do c = nocc + 1, nactive 
! do d = nocc + 1, nactive 
! sum = zero 
! do i = 1, nocc 
! do k = 1, nocc 
! do b = nocc + 1, nactive 
! do j = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r3(vrdav_Rr, a,j,b,k,c,i) * t3(nocc, nactive, a,b,d,k,j,i)
! end do 
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_2(c, d) = wmo_interm_cc3_2(c, d) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, k, c, d, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do c = nocc + 1, nactive 
! do d = nocc + 1, nactive 
! sum = zero 
! do j = 1, nocc 
! do i = 1, nocc 
! do k = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + r3(vrdav_Rr, a,j,b,k,c,i) * t3(nocc, nactive, a,b,d,k,i,j)
! end do 
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_3(c, d) = wmo_interm_cc3_3(c, d) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, k, c, d, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do c = nocc + 1, nactive 
! do d = nocc + 1, nactive 
! sum = zero 
! do j = 1, nocc 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do k = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r3(vrdav_Rr, a,j,b,k,c,i) * t3(nocc, nactive, a,b,d,i,k,j)
! end do 
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_4(c, d) = wmo_interm_cc3_4(c, d) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, k, c, d, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do c = nocc + 1, nactive 
! do d = nocc + 1, nactive 
! sum = zero 
! do k = 1, nocc 
! do i = 1, nocc 
! do j = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + r3(vrdav_Rr, a,j,b,k,c,i) * t3(nocc, nactive, a,b,d,j,i,k)
! end do 
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_5(c, d) = wmo_interm_cc3_5(c, d) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, k, c, d, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do c = nocc + 1, nactive 
! do d = nocc + 1, nactive 
! sum = zero 
! do i = 1, nocc 
! do j = 1, nocc 
! do b = nocc + 1, nactive 
! do k = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r3(vrdav_Rr, a,j,b,k,c,i) * t3(nocc, nactive, a,b,d,j,k,i)
! end do 
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_6(c, d) = wmo_interm_cc3_6(c, d) + sum 
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
! wmo_interm_cc3_7(i, j) = wmo_interm_cc3_7(i, j) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, c, i, j, k, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do k = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do j = 1, nocc 
! do c = nocc + 1, nactive 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + r3(vrdav_Rr, a,k,b,i,c,j) * t3(nocc, nactive, a,b,c,i,j,l)
! end do 
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_8(k, l) = wmo_interm_cc3_8(k, l) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, c, i, j, k, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do k = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do j = 1, nocc 
! do i = 1, nocc 
! do c = nocc + 1, nactive 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + r3(vrdav_Rr, a,k,b,i,c,j) * t3(nocc, nactive, a,b,c,l,j,i)
! end do 
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_9(k, l) = wmo_interm_cc3_9(k, l) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, c, i, j, k, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do k = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do j = 1, nocc 
! do c = nocc + 1, nactive 
! do b = nocc + 1, nactive 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r3(vrdav_Rr, a,k,b,i,c,j) * t3(nocc, nactive, a,b,c,j,i,l)
! end do 
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_10(k, l) = wmo_interm_cc3_10(k, l) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, c, i, j, k, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do k = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do j = 1, nocc 
! do c = nocc + 1, nactive 
! do b = nocc + 1, nactive 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r3(vrdav_Rr, a,k,b,i,c,j) * t3(nocc, nactive, a,b,c,l,i,j)
! end do 
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_11(k, l) = wmo_interm_cc3_11(k, l) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, c, i, j, k, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do k = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do j = 1, nocc 
! do i = 1, nocc 
! do c = nocc + 1, nactive 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + r3(vrdav_Rr, a,k,b,i,c,j) * t3(nocc, nactive, a,b,c,j,l,i)
! end do 
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_12(k, l) = wmo_interm_cc3_12(k, l) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, c, i, j, k, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do k = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do j = 1, nocc 
! do c = nocc + 1, nactive 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + r3(vrdav_Rr, a,k,b,i,c,j) * t3(nocc, nactive, a,b,c,i,l,j)
! end do 
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_13(k, l) = wmo_interm_cc3_13(k, l) + sum 
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
! sum = sum + r1(vrdav_Rl, a,i) * r2(vrdav_Rr, a,j,b,k)
! end do 
! wmo_interm_cc3_14(b, i, j, k) = wmo_interm_cc3_14(b, i, j, k) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, c, j, k, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do c = nocc + 1, nactive 
! do j = 1, nocc 
! do k = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + t3(nocc, nactive, a,b,c,j,k,i) * t2(a,b,l,i)
! end do 
! end do 
! end do 
! wmo_interm_cc3_15(c, j, k, l) = wmo_interm_cc3_15(c, j, k, l) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, c, k, j, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do c = nocc + 1, nactive 
! do k = 1, nocc 
! do j = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + t3(nocc, nactive, a,b,c,i,k,j) * t2(a,b,l,i)
! end do 
! end do 
! end do 
! wmo_interm_cc3_16(c, k, j, l) = wmo_interm_cc3_16(c, k, j, l) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, c, k, j, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do c = nocc + 1, nactive 
! do k = 1, nocc 
! do j = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do b = nocc + 1, nactive 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + t3(nocc, nactive, a,b,c,k,i,j) * t2(a,b,l,i)
! end do 
! end do 
! end do 
! wmo_interm_cc3_17(c, k, j, l) = wmo_interm_cc3_17(c, k, j, l) + sum 
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
! sum = sum + r1(vrdav_Rl, a,i) * t2(a,b,j,i)
! end do 
! end do 
! wmo_interm_cc3_18(b, j) = wmo_interm_cc3_18(b, j) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, c, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do c = nocc + 1, nactive 
! do k = 1, nocc 
! sum = zero 
! do j = 1, nocc 
! do b = nocc + 1, nactive 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,k,i,j)
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_19(c, k) = wmo_interm_cc3_19(c, k) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, c, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do c = nocc + 1, nactive 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do j = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,k,j,i)
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_20(c, k) = wmo_interm_cc3_20(c, k) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, c, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do c = nocc + 1, nactive 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do j = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,j,k,i)
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_21(c, k) = wmo_interm_cc3_21(c, k) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, c, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do c = nocc + 1, nactive 
! do k = 1, nocc 
! sum = zero 
! do j = 1, nocc 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,i,k,j)
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_22(c, k) = wmo_interm_cc3_22(c, k) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, c, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do c = nocc + 1, nactive 
! do k = 1, nocc 
! sum = zero 
! do j = 1, nocc 
! do b = nocc + 1, nactive 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,j,i,k)
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_23(c, k) = wmo_interm_cc3_23(c, k) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, c, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do c = nocc + 1, nactive 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do j = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,i,j,k)
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_24(c, k) = wmo_interm_cc3_24(c, k) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, c, k, j, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do c = nocc + 1, nactive 
! do k = 1, nocc 
! do j = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + t3(nocc, nactive, a,b,c,k,j,i) * t2(a,b,l,i)
! end do 
! end do 
! end do 
! wmo_interm_cc3_25(c, k, j, l) = wmo_interm_cc3_25(c, k, j, l) + sum 
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
! sum = sum + r1(vrdav_Rl, a,i) * t2(a,b,j,k)
! end do 
! wmo_interm_cc3_26(b, i, j, k) = wmo_interm_cc3_26(b, i, j, k) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, c, j, k, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do c = nocc + 1, nactive 
! do j = 1, nocc 
! do k = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + r2(vrdav_Rr, a,j,b,i) * t3(nocc, nactive, a,b,c,k,l,i)
! end do 
! end do 
! end do 
! wmo_interm_cc3_27(c, j, k, l) = wmo_interm_cc3_27(c, j, k, l) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, c, j, l, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do c = nocc + 1, nactive 
! do j = 1, nocc 
! do l = 1, nocc 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + r2(vrdav_Rr, a,j,b,i) * t3(nocc, nactive, a,b,c,l,k,i)
! end do 
! end do 
! end do 
! wmo_interm_cc3_28(c, j, l, k) = wmo_interm_cc3_28(c, j, l, k) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, c, j, k, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do c = nocc + 1, nactive 
! do j = 1, nocc 
! do k = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + r2(vrdav_Rr, a,j,b,i) * t3(nocc, nactive, a,b,c,i,k,l)
! end do 
! end do 
! end do 
! wmo_interm_cc3_29(c, j, k, l) = wmo_interm_cc3_29(c, j, k, l) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, c, j, k, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do c = nocc + 1, nactive 
! do j = 1, nocc 
! do k = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do b = nocc + 1, nactive 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r2(vrdav_Rr, a,j,b,i) * t3(nocc, nactive, a,b,c,k,i,l)
! end do 
! end do 
! end do 
! wmo_interm_cc3_30(c, j, k, l) = wmo_interm_cc3_30(c, j, k, l) + sum 
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
! sum = sum + r1(vrdav_Rl, a,i) * t2(a,b,i,j)
! end do 
! end do 
! wmo_interm_cc3_31(b, j) = wmo_interm_cc3_31(b, j) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, k, c, d, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do c = nocc + 1, nactive 
! do d = nocc + 1, nactive 
! sum = zero 
! do j = 1, nocc 
! do i = 1, nocc 
! do k = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + r3(vrdav_Rl, a,j,b,k,c,i) * t3(nocc, nactive, a,b,d,k,i,j)
! end do 
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_32(c, d) = wmo_interm_cc3_32(c, d) + sum 
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
! wmo_interm_cc3_33(a, b) = wmo_interm_cc3_33(a, b) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, k, c, d, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do c = nocc + 1, nactive 
! do d = nocc + 1, nactive 
! sum = zero 
! do j = 1, nocc 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do k = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r3(vrdav_Rl, a,j,b,k,c,i) * t3(nocc, nactive, a,b,d,i,k,j)
! end do 
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_34(c, d) = wmo_interm_cc3_34(c, d) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, k, c, d, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do c = nocc + 1, nactive 
! do d = nocc + 1, nactive 
! sum = zero 
! do i = 1, nocc 
! do k = 1, nocc 
! do b = nocc + 1, nactive 
! do j = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r3(vrdav_Rl, a,j,b,k,c,i) * t3(nocc, nactive, a,b,d,k,j,i)
! end do 
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_35(c, d) = wmo_interm_cc3_35(c, d) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, k, c, d, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do c = nocc + 1, nactive 
! do d = nocc + 1, nactive 
! sum = zero 
! do i = 1, nocc 
! do j = 1, nocc 
! do b = nocc + 1, nactive 
! do k = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r3(vrdav_Rl, a,j,b,k,c,i) * t3(nocc, nactive, a,b,d,j,k,i)
! end do 
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_36(c, d) = wmo_interm_cc3_36(c, d) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, k, c, d, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do c = nocc + 1, nactive 
! do d = nocc + 1, nactive 
! sum = zero 
! do j = 1, nocc 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do k = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r3(vrdav_Rl, a,k,b,j,c,i) * t3(nocc, nactive, a,b,d,i,k,j)
! end do 
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_37(c, d) = wmo_interm_cc3_37(c, d) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, k, c, d, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do c = nocc + 1, nactive 
! do d = nocc + 1, nactive 
! sum = zero 
! do j = 1, nocc 
! do i = 1, nocc 
! do k = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + r3(vrdav_Rl, a,k,b,j,c,i) * t3(nocc, nactive, a,b,d,k,i,j)
! end do 
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_38(c, d) = wmo_interm_cc3_38(c, d) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, c, i, j, k, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do k = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do j = 1, nocc 
! do i = 1, nocc 
! do c = nocc + 1, nactive 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + r3(vrdav_Rl, a,k,b,j,c,i) * t3(nocc, nactive, a,b,c,i,l,j)
! end do 
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_39(k, l) = wmo_interm_cc3_39(k, l) + sum 
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
! sum = sum + r1(vrdav_Rr, a,i) * s1(a,j)
! end do 
! wmo_interm_cc3_40(i, j) = wmo_interm_cc3_40(i, j) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, c, i, j, k, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do k = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do j = 1, nocc 
! do i = 1, nocc 
! do c = nocc + 1, nactive 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + r3(vrdav_Rl, a,k,b,i,c,j) * t3(nocc, nactive, a,b,c,l,j,i)
! end do 
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_41(k, l) = wmo_interm_cc3_41(k, l) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, c, i, j, k, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do k = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do j = 1, nocc 
! do i = 1, nocc 
! do c = nocc + 1, nactive 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + r3(vrdav_Rl, a,k,b,i,c,j) * t3(nocc, nactive, a,b,c,j,l,i)
! end do 
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_42(k, l) = wmo_interm_cc3_42(k, l) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, c, i, j, k, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do k = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do j = 1, nocc 
! do c = nocc + 1, nactive 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + r3(vrdav_Rl, a,k,b,i,c,j) * t3(nocc, nactive, a,b,c,i,l,j)
! end do 
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_43(k, l) = wmo_interm_cc3_43(k, l) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, c, i, j, k, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do k = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do j = 1, nocc 
! do c = nocc + 1, nactive 
! do b = nocc + 1, nactive 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r3(vrdav_Rl, a,k,b,i,c,j) * t3(nocc, nactive, a,b,c,l,i,j)
! end do 
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_44(k, l) = wmo_interm_cc3_44(k, l) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, c, i, j, k, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do k = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do c = nocc + 1, nactive 
! do j = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + r3(vrdav_Rl, a,k,b,j,c,i) * t3(nocc, nactive, a,b,c,j,l,i)
! end do 
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_45(k, l) = wmo_interm_cc3_45(k, l) + sum 
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
! sum = sum + r2(vrdav_Rl, a,i,b,j) * r1(vrdav_Rr, a,i)
! end do 
! end do 
! wmo_interm_cc3_46(b, j) = wmo_interm_cc3_46(b, j) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, c, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do c = nocc + 1, nactive 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do j = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + s2(a,b,j,i) * t3(nocc, nactive, a,b,c,k,j,i)
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_47(c, k) = wmo_interm_cc3_47(c, k) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, c, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do c = nocc + 1, nactive 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do j = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + s2(a,b,i,j) * t3(nocc, nactive, a,b,c,k,j,i)
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_48(c, k) = wmo_interm_cc3_48(c, k) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, c, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do c = nocc + 1, nactive 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do j = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + s2(a,b,i,j) * t3(nocc, nactive, a,b,c,i,j,k)
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_49(c, k) = wmo_interm_cc3_49(c, k) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, c, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do c = nocc + 1, nactive 
! do k = 1, nocc 
! sum = zero 
! do j = 1, nocc 
! do b = nocc + 1, nactive 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + s2(a,b,i,j) * t3(nocc, nactive, a,b,c,j,i,k)
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_50(c, k) = wmo_interm_cc3_50(c, k) + sum 
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
! sum = sum + r2(vrdav_Rl, a,j,b,i) * r1(vrdav_Rr, a,k)
! end do 
! wmo_interm_cc3_51(b, j, i, k) = wmo_interm_cc3_51(b, j, i, k) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, c, j, l, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do c = nocc + 1, nactive 
! do j = 1, nocc 
! do l = 1, nocc 
! do k = 1, nocc 
! sum = zero 
! do b = nocc + 1, nactive 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + s2(a,b,i,j) * t3(nocc, nactive, a,b,c,l,i,k)
! end do 
! end do 
! end do 
! wmo_interm_cc3_52(c, j, l, k) = wmo_interm_cc3_52(c, j, l, k) + sum 
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
! sum = sum + r2(vrdav_Rl, a,j,b,i) * r1(vrdav_Rr, a,i)
! end do 
! end do 
! wmo_interm_cc3_53(b, j) = wmo_interm_cc3_53(b, j) + sum 
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
! sum = sum + r2(vrdav_Rl, a,i,b,j) * r1(vrdav_Rr, a,k)
! end do 
! wmo_interm_cc3_54(b, i, j, k) = wmo_interm_cc3_54(b, i, j, k) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, c, j, k, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do c = nocc + 1, nactive 
! do j = 1, nocc 
! do k = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + s2(a,b,j,i) * t3(nocc, nactive, a,b,c,k,l,i)
! end do 
! end do 
! end do 
! wmo_interm_cc3_55(c, j, k, l) = wmo_interm_cc3_55(c, j, k, l) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, c, j, k, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do c = nocc + 1, nactive 
! do j = 1, nocc 
! do k = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + s2(a,b,i,j) * t3(nocc, nactive, a,b,c,k,l,i)
! end do 
! end do 
! end do 
! wmo_interm_cc3_56(c, j, k, l) = wmo_interm_cc3_56(c, j, k, l) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, c, j, l, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do c = nocc + 1, nactive 
! do j = 1, nocc 
! do l = 1, nocc 
! do k = 1, nocc 
! sum = zero 
! do b = nocc + 1, nactive 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + s2(a,b,j,i) * t3(nocc, nactive, a,b,c,l,i,k)
! end do 
! end do 
! end do 
! wmo_interm_cc3_57(c, j, l, k) = wmo_interm_cc3_57(c, j, l, k) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, c, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do c = nocc + 1, nactive 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do j = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, a,b,c,k,j,i)
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_58(c, k) = wmo_interm_cc3_58(c, k) + sum 
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
! wmo_interm_cc3_59(b, j) = wmo_interm_cc3_59(b, j) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, c, j, l, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do c = nocc + 1, nactive 
! do j = 1, nocc 
! do l = 1, nocc 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, a,b,c,l,k,i)
! end do 
! end do 
! end do 
! wmo_interm_cc3_60(c, j, l, k) = wmo_interm_cc3_60(c, j, l, k) + sum 
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
! sum = sum + r1(vrdav_Rr, a,i) * s2(a,b,j,k)
! end do 
! wmo_interm_cc3_61(b, i, j, k) = wmo_interm_cc3_61(b, i, j, k) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, c, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do c = nocc + 1, nactive 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do j = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,b,c,k,j,i)
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_62(c, k) = wmo_interm_cc3_62(c, k) + sum 
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
! wmo_interm_cc3_63(b, j) = wmo_interm_cc3_63(b, j) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, c, j, k, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do c = nocc + 1, nactive 
! do j = 1, nocc 
! do k = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, a,b,c,i,k,l)
! end do 
! end do 
! end do 
! wmo_interm_cc3_64(c, j, k, l) = wmo_interm_cc3_64(c, j, k, l) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, c, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do c = nocc + 1, nactive 
! do k = 1, nocc 
! sum = zero 
! do j = 1, nocc 
! do b = nocc + 1, nactive 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,b,c,j,i,k)
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_65(c, k) = wmo_interm_cc3_65(c, k) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, c, j, k, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do c = nocc + 1, nactive 
! do j = 1, nocc 
! do k = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, a,b,c,k,l,i)
! end do 
! end do 
! end do 
! wmo_interm_cc3_66(c, j, k, l) = wmo_interm_cc3_66(c, j, k, l) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, c, j, k, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do c = nocc + 1, nactive 
! do j = 1, nocc 
! do k = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do b = nocc + 1, nactive 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, a,b,c,k,i,l)
! end do 
! end do 
! end do 
! wmo_interm_cc3_67(c, j, k, l) = wmo_interm_cc3_67(c, j, k, l) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, c, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do c = nocc + 1, nactive 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do j = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,b,c,i,j,k)
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_68(c, k) = wmo_interm_cc3_68(c, k) + sum 
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
! wmo_interm_cc3_69(i, j) = wmo_interm_cc3_69(i, j) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, c, i, j, k, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do k = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do j = 1, nocc 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + t3(nocc, nactive, a,b,c,i,j,k) * t3(nocc, nactive, a,b,c,l,i,j)
! end do 
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_70(k, l) = wmo_interm_cc3_70(k, l) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, c, i, j, k, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do k = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do j = 1, nocc 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + t3(nocc, nactive, a,b,c,k,i,j) * t3(nocc, nactive, a,b,c,l,j,i)
! end do 
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_71(k, l) = wmo_interm_cc3_71(k, l) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, c, i, j, k, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do k = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do j = 1, nocc 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + t3(nocc, nactive, a,b,c,i,k,j) * t3(nocc, nactive, a,b,c,l,j,i)
! end do 
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_72(k, l) = wmo_interm_cc3_72(k, l) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, c, i, j, k, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do k = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do j = 1, nocc 
! do c = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + t3(nocc, nactive, a,b,c,i,j,k) * t3(nocc, nactive, a,b,c,l,j,i)
! end do 
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_73(k, l) = wmo_interm_cc3_73(k, l) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, c, i, j, k, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do k = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do j = 1, nocc 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + t3(nocc, nactive, a,b,c,i,k,j) * t3(nocc, nactive, a,b,c,l,i,j)
! end do 
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_74(k, l) = wmo_interm_cc3_74(k, l) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, c, i, j, k, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do k = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do j = 1, nocc 
! do b = nocc + 1, nactive 
! do i = 1, nocc 
! do c = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + t3(nocc, nactive, a,b,c,k,i,j) * t3(nocc, nactive, a,b,c,l,i,j)
! end do 
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_75(k, l) = wmo_interm_cc3_75(k, l) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, c, i, j, k, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do k = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do j = 1, nocc 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + t3(nocc, nactive, a,b,c,j,k,i) * t3(nocc, nactive, a,b,c,l,i,j)
! end do 
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_76(k, l) = wmo_interm_cc3_76(k, l) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, c, i, j, k, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do k = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do j = 1, nocc 
! do b = nocc + 1, nactive 
! do i = 1, nocc 
! do c = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + t3(nocc, nactive, a,b,c,j,i,k) * t3(nocc, nactive, a,b,c,l,i,j)
! end do 
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_77(k, l) = wmo_interm_cc3_77(k, l) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, c, i, j, k, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do k = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do j = 1, nocc 
! do b = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + t3(nocc, nactive, a,b,c,j,i,k) * t3(nocc, nactive, a,b,c,l,j,i)
! end do 
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_78(k, l) = wmo_interm_cc3_78(k, l) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, c, i, j, k, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do k = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do j = 1, nocc 
! do b = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + t3(nocc, nactive, a,b,c,j,k,i) * t3(nocc, nactive, a,b,c,l,j,i)
! end do 
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_79(k, l) = wmo_interm_cc3_79(k, l) + sum 
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
! wmo_interm_cc3_80(a, b) = wmo_interm_cc3_80(a, b) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, k, c, d, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do c = nocc + 1, nactive 
! do d = nocc + 1, nactive 
! sum = zero 
! do i = 1, nocc 
! do k = 1, nocc 
! do j = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + t3(nocc, nactive, a,b,c,j,k,i) * t3(nocc, nactive, a,b,d,k,j,i)
! end do 
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_81(c, d) = wmo_interm_cc3_81(c, d) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, k, c, d, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do c = nocc + 1, nactive 
! do d = nocc + 1, nactive 
! sum = zero 
! do j = 1, nocc 
! do i = 1, nocc 
! do k = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + t3(nocc, nactive, a,b,c,i,k,j) * t3(nocc, nactive, a,b,d,k,j,i)
! end do 
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_82(c, d) = wmo_interm_cc3_82(c, d) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, k, c, d, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do c = nocc + 1, nactive 
! do d = nocc + 1, nactive 
! sum = zero 
! do i = 1, nocc 
! do j = 1, nocc 
! do b = nocc + 1, nactive 
! do k = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + t3(nocc, nactive, a,b,c,j,k,i) * t3(nocc, nactive, a,b,d,j,k,i)
! end do 
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_83(c, d) = wmo_interm_cc3_83(c, d) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, k, c, d, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do c = nocc + 1, nactive 
! do d = nocc + 1, nactive 
! sum = zero 
! do j = 1, nocc 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do k = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + t3(nocc, nactive, a,b,c,i,k,j) * t3(nocc, nactive, a,b,d,j,k,i)
! end do 
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_84(c, d) = wmo_interm_cc3_84(c, d) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, k, c, d, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do c = nocc + 1, nactive 
! do d = nocc + 1, nactive 
! sum = zero 
! do j = 1, nocc 
! do i = 1, nocc 
! do k = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + t3(nocc, nactive, a,b,c,k,i,j) * t3(nocc, nactive, a,b,d,j,k,i)
! end do 
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_85(c, d) = wmo_interm_cc3_85(c, d) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, k, c, d, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do c = nocc + 1, nactive 
! do d = nocc + 1, nactive 
! sum = zero 
! do j = 1, nocc 
! do i = 1, nocc 
! do k = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + t3(nocc, nactive, a,b,c,k,i,j) * t3(nocc, nactive, a,b,d,k,j,i)
! end do 
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_86(c, d) = wmo_interm_cc3_86(c, d) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, b, c, j, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do b = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! do j = 1, nocc 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r3(vrdav_Rl, a,i,b,j,c,k) * r1(vrdav_Rr, a,i)
! end do 
! end do 
! wmo_interm_cc3_87(b, c, j, k) = wmo_interm_cc3_87(b, c, j, k) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, b, c, k, j, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do b = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! do k = 1, nocc 
! do j = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + s1(a,i) * t3(nocc, nactive, a,b,c,k,j,i)
! end do 
! end do 
! wmo_interm_cc3_88(b, c, k, j) = wmo_interm_cc3_88(b, c, k, j) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, b, c, k, j, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do b = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! do k = 1, nocc 
! do j = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + s1(a,i) * t3(nocc, nactive, a,b,c,k,i,j)
! end do 
! end do 
! wmo_interm_cc3_89(b, c, k, j) = wmo_interm_cc3_89(b, c, k, j) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, b, c, j, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do b = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! do j = 1, nocc 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + s1(a,i) * t3(nocc, nactive, a,b,c,i,j,k)
! end do 
! end do 
! wmo_interm_cc3_90(b, c, j, k) = wmo_interm_cc3_90(b, c, j, k) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, b, c, k, j, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do b = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! do k = 1, nocc 
! do j = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r3(vrdav_Rl, a,k,b,j,c,i) * r1(vrdav_Rr, a,i)
! end do 
! end do 
! wmo_interm_cc3_91(b, c, k, j) = wmo_interm_cc3_91(b, c, k, j) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, b, c, j, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do b = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! do j = 1, nocc 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r3(vrdav_Rl, a,j,b,i,c,k) * r1(vrdav_Rr, a,i)
! end do 
! end do 
! wmo_interm_cc3_92(b, c, j, k) = wmo_interm_cc3_92(b, c, j, k) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, b, c, j, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do b = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! do j = 1, nocc 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r3(vrdav_Rl, a,j,b,k,c,i) * r1(vrdav_Rr, a,i)
! end do 
! end do 
! wmo_interm_cc3_93(b, c, j, k) = wmo_interm_cc3_93(b, c, j, k) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, b, c, k, j, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do b = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! do k = 1, nocc 
! do j = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r3(vrdav_Rl, a,k,b,i,c,j) * r1(vrdav_Rr, a,i)
! end do 
! end do 
! wmo_interm_cc3_94(b, c, k, j) = wmo_interm_cc3_94(b, c, k, j) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, b, c, j, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do b = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! do j = 1, nocc 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r1(vrdav_Rl, a,i) * t3(nocc, nactive, a,b,c,j,i,k)
! end do 
! end do 
! wmo_interm_cc3_95(b, c, j, k) = wmo_interm_cc3_95(b, c, j, k) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, b, c, j, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do b = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! do j = 1, nocc 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,c,j,i,k)
! end do 
! end do 
! wmo_interm_cc3_96(b, c, j, k) = wmo_interm_cc3_96(b, c, j, k) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, b, c, j, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do b = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! do j = 1, nocc 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,c,j,k,i)
! end do 
! end do 
! wmo_interm_cc3_97(b, c, j, k) = wmo_interm_cc3_97(b, c, j, k) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, b, c, j, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do b = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! do j = 1, nocc 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,c,i,j,k)
! end do 
! end do 
! wmo_interm_cc3_98(b, c, j, k) = wmo_interm_cc3_98(b, c, j, k) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, b, c, j, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do b = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! do j = 1, nocc 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r1(vrdav_Rl, a,i) * t3(nocc, nactive, a,b,c,j,k,i)
! end do 
! end do 
! wmo_interm_cc3_99(b, c, j, k) = wmo_interm_cc3_99(b, c, j, k) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, b, c, j, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do b = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! do j = 1, nocc 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r1(vrdav_Rl, a,i) * t3(nocc, nactive, a,b,c,i,j,k)
! end do 
! end do 
! wmo_interm_cc3_100(b, c, j, k) = wmo_interm_cc3_100(b, c, j, k) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 



    end subroutine d_overlap_intermediates_cc3

   subroutine d_overlap_triplet_ccsd_init(nocc, nactive)
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

  
    ! allocate(wmo_interm_0_triplet(1: nocc, 1: nocc))
! allocate(wmo_interm_1_triplet(1: nocc, 1: nocc))
! allocate(wmo_interm_2_triplet(1: nocc, 1: nocc))
! allocate(wmo_interm_3_triplet(1: nocc, 1: nocc))
! allocate(wmo_interm_4_triplet(nocc+1: nactive, 1: nocc))
! allocate(wmo_interm_5_triplet(nocc+1: nactive, 1: nocc))
! allocate(wmo_interm_6_triplet(nocc+1: nactive, 1: nocc))
! allocate(wmo_interm_7_triplet(nocc+1: nactive, 1: nocc))
! allocate(wmo_interm_8_triplet(nocc+1: nactive, 1: nocc))
! allocate(wmo_interm_9_triplet(nocc+1: nactive, 1: nocc))
! allocate(wmo_interm_10_triplet(1: nocc, 1: nocc))
! allocate(wmo_interm_11_triplet(nocc+1: nactive, nocc+1: nactive))
! allocate(wmo_interm_12_triplet(nocc+1: nactive, nocc+1: nactive))
! allocate(wmo_interm_13_triplet(1: nocc, 1: nocc))
! allocate(wmo_interm_14_triplet(1: nocc, 1: nocc))
! allocate(wmo_interm_15_triplet(nocc+1: nactive, nocc+1: nactive))
! allocate(wmo_interm_16_triplet(nocc+1: nactive, nocc+1: nactive))
! allocate(wmo_interm_17_triplet(1: nocc, 1: nocc))
! allocate(wmo_interm_18_triplet(nocc+1: nactive, nocc+1: nactive))
! allocate(wmo_interm_19_triplet(1: nocc, 1: nocc))
! allocate(wmo_interm_20_triplet(nocc+1: nactive, nocc+1: nactive))
! allocate(wmo_interm_21_triplet(1: nocc, 1: nocc))
! allocate(wmo_interm_22_triplet(1: nocc, 1: nocc))
! allocate(wmo_interm_23_triplet(nocc+1: nactive, nocc+1: nactive))
! allocate(wmo_interm_24_triplet(nocc+1: nactive, nocc+1: nactive))
! allocate(wmo_interm_25_triplet(1: nocc, 1: nocc))
! allocate(wmo_interm_26_triplet(1: nocc, 1: nocc))
! allocate(wmo_interm_27_triplet(nocc+1: nactive, nocc+1: nactive))
! allocate(wmo_interm_28_triplet(nocc+1: nactive, nocc+1: nactive))
! allocate(wmo_interm_29_triplet(nocc+1: nactive, nocc+1: nactive))
! allocate(wmo_interm_30_triplet(nocc+1: nactive, nocc+1: nactive))
! allocate(wmo_interm_31_triplet(nocc+1: nactive, nocc+1: nactive))
! allocate(wmo_interm_32_triplet(1: nocc, 1: nocc))
! allocate(wmo_interm_33_triplet(1: nocc, 1: nocc))
! allocate(wmo_interm_34_triplet(nocc+1: nactive, nocc+1: nactive))
! allocate(wmo_interm_35_triplet(nocc+1: nactive, nocc+1: nactive))
! allocate(wmo_interm_36_triplet(1: nocc, 1: nocc))
! allocate(wmo_interm_37_triplet(1: nocc, 1: nocc))
! allocate(wmo_interm_38_triplet(1: nocc, 1: nocc))
! allocate(wmo_interm_39_triplet(1: nocc, 1: nocc))
! allocate(wmo_interm_40_triplet(nocc+1: nactive, 1: nocc))
! allocate(wmo_interm_41_triplet(nocc+1: nactive, 1: nocc))
! allocate(wmo_interm_42_triplet(nocc+1: nactive, 1: nocc))
! allocate(wmo_interm_43_triplet(nocc+1: nactive, nocc+1: nactive))
! allocate(wmo_interm_44_triplet(nocc+1: nactive, nocc+1: nactive))
! allocate(wmo_interm_45_triplet(nocc+1: nactive, nocc+1: nactive))
! allocate(wmo_interm_46_triplet(1: nocc, 1: nocc))
! allocate(wmo_interm_47_triplet(1: nocc, 1: nocc))
! allocate(wmo_interm_48_triplet(nocc+1: nactive, 1: nocc))
! allocate(wmo_interm_49_triplet(nocc+1: nactive, nocc+1: nactive))
! allocate(wmo_interm_50_triplet(nocc+1: nactive, 1: nocc))
! allocate(wmo_interm_51_triplet(nocc+1: nactive, nocc+1: nactive))
! allocate(wmo_interm_52_triplet(1: nocc, 1: nocc))
! allocate(wmo_interm_53_triplet(1: nocc, 1: nocc))
! allocate(wmo_interm_54_triplet(1: nocc, 1: nocc))
! allocate(wmo_interm_55_triplet(1: nocc, 1: nocc))
! allocate(wmo_interm_56_triplet(nocc+1: nactive, 1: nocc))
! allocate(wmo_interm_57_triplet(nocc+1: nactive, 1: nocc))
! allocate(wmo_interm_58_triplet(nocc+1: nactive, 1: nocc))
! allocate(wmo_interm_59_triplet(nocc+1: nactive, nocc+1: nactive))
! allocate(wmo_interm_60_triplet(nocc+1: nactive, nocc+1: nactive))
! allocate(wmo_interm_61_triplet(nocc+1: nactive, nocc+1: nactive))
! allocate(wmo_interm_62_triplet(nocc+1: nactive, 1: nocc))
! allocate(wmo_interm_63_triplet(1: nocc, 1: nocc, 1: nocc, 1: nocc))
! allocate(wmo_interm_64_triplet(1: nocc, 1: nocc, 1: nocc, 1: nocc))
! allocate(wmo_interm_65_triplet(1: nocc, 1: nocc, 1: nocc, 1: nocc))
! allocate(wmo_interm_66_triplet(1: nocc, 1: nocc, 1: nocc, 1: nocc))
! allocate(wmo_interm_67_triplet(1: nocc, 1: nocc, 1: nocc, 1: nocc))
! allocate(wmo_interm_68_triplet(1: nocc, 1: nocc, 1: nocc, 1: nocc))
! allocate(wmo_interm_69_triplet(1: nocc, 1: nocc, 1: nocc, 1: nocc))
! allocate(wmo_interm_70_triplet(1: nocc, 1: nocc, 1: nocc, 1: nocc))
! allocate(wmo_interm_71_triplet(1: nocc, 1: nocc, 1: nocc, 1: nocc))
! allocate(wmo_interm_72_triplet(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
! allocate(wmo_interm_73_triplet(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
! allocate(wmo_interm_74_triplet(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
! allocate(wmo_interm_75_triplet(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
! allocate(wmo_interm_76_triplet(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
! allocate(wmo_interm_77_triplet(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
! allocate(wmo_interm_78_triplet(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
! allocate(wmo_interm_79_triplet(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
! allocate(wmo_interm_80_triplet(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
! allocate(wmo_interm_81_triplet(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
! allocate(wmo_interm_82_triplet(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
! allocate(wmo_interm_83_triplet(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
! allocate(wmo_interm_84_triplet(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
! wmo_interm_0_triplet = zero 
! wmo_interm_1_triplet = zero 
! wmo_interm_2_triplet = zero 
! wmo_interm_3_triplet = zero 
! wmo_interm_4_triplet = zero 
! wmo_interm_5_triplet = zero 
! wmo_interm_6_triplet = zero 
! wmo_interm_7_triplet = zero 
! wmo_interm_8_triplet = zero 
! wmo_interm_9_triplet = zero 
! wmo_interm_10_triplet = zero 
! wmo_interm_11_triplet = zero 
! wmo_interm_12_triplet = zero 
! wmo_interm_13_triplet = zero 
! wmo_interm_14_triplet = zero 
! wmo_interm_15_triplet = zero 
! wmo_interm_16_triplet = zero 
! wmo_interm_17_triplet = zero 
! wmo_interm_18_triplet = zero 
! wmo_interm_19_triplet = zero 
! wmo_interm_20_triplet = zero 
! wmo_interm_21_triplet = zero 
! wmo_interm_22_triplet = zero 
! wmo_interm_23_triplet = zero 
! wmo_interm_24_triplet = zero 
! wmo_interm_25_triplet = zero 
! wmo_interm_26_triplet = zero 
! wmo_interm_27_triplet = zero 
! wmo_interm_28_triplet = zero 
! wmo_interm_29_triplet = zero 
! wmo_interm_30_triplet = zero 
! wmo_interm_31_triplet = zero 
! wmo_interm_32_triplet = zero 
! wmo_interm_33_triplet = zero 
! wmo_interm_34_triplet = zero 
! wmo_interm_35_triplet = zero 
! wmo_interm_36_triplet = zero 
! wmo_interm_37_triplet = zero 
! wmo_interm_38_triplet = zero 
! wmo_interm_39_triplet = zero 
! wmo_interm_40_triplet = zero 
! wmo_interm_41_triplet = zero 
! wmo_interm_42_triplet = zero 
! wmo_interm_43_triplet = zero 
! wmo_interm_44_triplet = zero 
! wmo_interm_45_triplet = zero 
! wmo_interm_46_triplet = zero 
! wmo_interm_47_triplet = zero 
! wmo_interm_48_triplet = zero 
! wmo_interm_49_triplet = zero 
! wmo_interm_50_triplet = zero 
! wmo_interm_51_triplet = zero 
! wmo_interm_52_triplet = zero 
! wmo_interm_53_triplet = zero 
! wmo_interm_54_triplet = zero 
! wmo_interm_55_triplet = zero 
! wmo_interm_56_triplet = zero 
! wmo_interm_57_triplet = zero 
! wmo_interm_58_triplet = zero 
! wmo_interm_59_triplet = zero 
! wmo_interm_60_triplet = zero 
! wmo_interm_61_triplet = zero 
! wmo_interm_62_triplet = zero 
! wmo_interm_63_triplet = zero 
! wmo_interm_64_triplet = zero 
! wmo_interm_65_triplet = zero 
! wmo_interm_66_triplet = zero 
! wmo_interm_67_triplet = zero 
! wmo_interm_68_triplet = zero 
! wmo_interm_69_triplet = zero 
! wmo_interm_70_triplet = zero 
! wmo_interm_71_triplet = zero 
! wmo_interm_72_triplet = zero 
! wmo_interm_73_triplet = zero 
! wmo_interm_74_triplet = zero 
! wmo_interm_75_triplet = zero 
! wmo_interm_76_triplet = zero 
! wmo_interm_77_triplet = zero 
! wmo_interm_78_triplet = zero 
! wmo_interm_79_triplet = zero 
! wmo_interm_80_triplet = zero 
! wmo_interm_81_triplet = zero 
! wmo_interm_82_triplet = zero 
! wmo_interm_83_triplet = zero 
! wmo_interm_84_triplet = zero 

     end subroutine d_overlap_triplet_ccsd_init
    
    subroutine d_overlap_triplet_ccsd_free

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

 !   deallocate(wmo_interm_0_triplet)
! deallocate(wmo_interm_1_triplet)
! deallocate(wmo_interm_2_triplet)
! deallocate(wmo_interm_3_triplet)
! deallocate(wmo_interm_4_triplet)
! deallocate(wmo_interm_5_triplet)
! deallocate(wmo_interm_6_triplet)
! deallocate(wmo_interm_7_triplet)
! deallocate(wmo_interm_8_triplet)
! deallocate(wmo_interm_9_triplet)
! deallocate(wmo_interm_10_triplet)
! deallocate(wmo_interm_11_triplet)
! deallocate(wmo_interm_12_triplet)
! deallocate(wmo_interm_13_triplet)
! deallocate(wmo_interm_14_triplet)
! deallocate(wmo_interm_15_triplet)
! deallocate(wmo_interm_16_triplet)
! deallocate(wmo_interm_17_triplet)
! deallocate(wmo_interm_18_triplet)
! deallocate(wmo_interm_19_triplet)
! deallocate(wmo_interm_20_triplet)
! deallocate(wmo_interm_21_triplet)
! deallocate(wmo_interm_22_triplet)
! deallocate(wmo_interm_23_triplet)
! deallocate(wmo_interm_24_triplet)
! deallocate(wmo_interm_25_triplet)
! deallocate(wmo_interm_26_triplet)
! deallocate(wmo_interm_27_triplet)
! deallocate(wmo_interm_28_triplet)
! deallocate(wmo_interm_29_triplet)
! deallocate(wmo_interm_30_triplet)
! deallocate(wmo_interm_31_triplet)
! deallocate(wmo_interm_32_triplet)
! deallocate(wmo_interm_33_triplet)
! deallocate(wmo_interm_34_triplet)
! deallocate(wmo_interm_35_triplet)
! deallocate(wmo_interm_36_triplet)
! deallocate(wmo_interm_37_triplet)
! deallocate(wmo_interm_38_triplet)
! deallocate(wmo_interm_39_triplet)
! deallocate(wmo_interm_40_triplet)
! deallocate(wmo_interm_41_triplet)
! deallocate(wmo_interm_42_triplet)
! deallocate(wmo_interm_43_triplet)
! deallocate(wmo_interm_44_triplet)
! deallocate(wmo_interm_45_triplet)
! deallocate(wmo_interm_46_triplet)
! deallocate(wmo_interm_47_triplet)
! deallocate(wmo_interm_48_triplet)
! deallocate(wmo_interm_49_triplet)
! deallocate(wmo_interm_50_triplet)
! deallocate(wmo_interm_51_triplet)
! deallocate(wmo_interm_52_triplet)
! deallocate(wmo_interm_53_triplet)
! deallocate(wmo_interm_54_triplet)
! deallocate(wmo_interm_55_triplet)
! deallocate(wmo_interm_56_triplet)
! deallocate(wmo_interm_57_triplet)
! deallocate(wmo_interm_58_triplet)
! deallocate(wmo_interm_59_triplet)
! deallocate(wmo_interm_60_triplet)
! deallocate(wmo_interm_61_triplet)
! deallocate(wmo_interm_62_triplet)
! deallocate(wmo_interm_63_triplet)
! deallocate(wmo_interm_64_triplet)
! deallocate(wmo_interm_65_triplet)
! deallocate(wmo_interm_66_triplet)
! deallocate(wmo_interm_67_triplet)
! deallocate(wmo_interm_68_triplet)
! deallocate(wmo_interm_69_triplet)
! deallocate(wmo_interm_70_triplet)
! deallocate(wmo_interm_71_triplet)
! deallocate(wmo_interm_72_triplet)
! deallocate(wmo_interm_73_triplet)
! deallocate(wmo_interm_74_triplet)
! deallocate(wmo_interm_75_triplet)
! deallocate(wmo_interm_76_triplet)
! deallocate(wmo_interm_77_triplet)
! deallocate(wmo_interm_78_triplet)
! deallocate(wmo_interm_79_triplet)
! deallocate(wmo_interm_80_triplet)
! deallocate(wmo_interm_81_triplet)
! deallocate(wmo_interm_82_triplet)
! deallocate(wmo_interm_83_triplet)
! deallocate(wmo_interm_84_triplet)

     end subroutine d_overlap_triplet_ccsd_free
    
    subroutine d_overlap_intermediates_triplet_ccsd(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr)
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    double precision, dimension(:), intent(in) :: vrdav_Rl
    double precision, dimension(:), intent(in) :: vrdav_Rr
    real(F64) :: sum
    integer :: a, i, j, b, k, c, l 

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

     

!     !$omp parallel private(a, i, j, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do i = 1, nocc 
! do j = 1, nocc 
! sum = zero 
! do a = nocc + 1, nactive 
! sum = sum + r1(vrdav_Rl, a,i) * r1(vrdav_Rr, a,j)
! end do 
! wmo_interm_0_triplet(i, j) = wmo_interm_0_triplet(i, j) + sum 
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
! sum = sum + s1(a,i) * t1(a,j)
! end do 
! wmo_interm_1_triplet(i, j) = wmo_interm_1_triplet(i, j) + sum 
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
! wmo_interm_2_triplet(i, j) = wmo_interm_2_triplet(i, j) + sum 
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
! sum = sum + r1(vrdav_Rr, a,i) * s1(a,j)
! end do 
! wmo_interm_3_triplet(i, j) = wmo_interm_3_triplet(i, j) + sum 
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
! wmo_interm_4_triplet(b, j) = wmo_interm_4_triplet(b, j) + sum 
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
! sum = sum + r2p(vrdav_Rr, b,j,a,i) * s1(a,i)
! end do 
! end do 
! wmo_interm_5_triplet(b, j) = wmo_interm_5_triplet(b, j) + sum 
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
! wmo_interm_6_triplet(b, j) = wmo_interm_6_triplet(b, j) + sum 
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
! wmo_interm_7_triplet(b, j) = wmo_interm_7_triplet(b, j) + sum 
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
! wmo_interm_8_triplet(b, j) = wmo_interm_8_triplet(b, j) + sum 
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
! wmo_interm_9_triplet(b, j) = wmo_interm_9_triplet(b, j) + sum 
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
! wmo_interm_10_triplet(j, k) = wmo_interm_10_triplet(j, k) + sum 
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
! wmo_interm_11_triplet(b, c) = wmo_interm_11_triplet(b, c) + sum 
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
! sum = sum + s1(a,i) * t1(b,i)
! end do 
! wmo_interm_12_triplet(a, b) = wmo_interm_12_triplet(a, b) + sum 
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
! wmo_interm_13_triplet(j, k) = wmo_interm_13_triplet(j, k) + sum 
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
! wmo_interm_14_triplet(j, k) = wmo_interm_14_triplet(j, k) + sum 
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
! wmo_interm_15_triplet(b, c) = wmo_interm_15_triplet(b, c) + sum 
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
! wmo_interm_16_triplet(b, c) = wmo_interm_16_triplet(b, c) + sum 
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
! wmo_interm_17_triplet(j, k) = wmo_interm_17_triplet(j, k) + sum 
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
! wmo_interm_18_triplet(b, c) = wmo_interm_18_triplet(b, c) + sum 
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
! wmo_interm_19_triplet(j, k) = wmo_interm_19_triplet(j, k) + sum 
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
! wmo_interm_20_triplet(b, c) = wmo_interm_20_triplet(b, c) + sum 
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
! wmo_interm_21_triplet(j, k) = wmo_interm_21_triplet(j, k) + sum 
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
! wmo_interm_22_triplet(j, k) = wmo_interm_22_triplet(j, k) + sum 
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
! wmo_interm_23_triplet(b, c) = wmo_interm_23_triplet(b, c) + sum 
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
! wmo_interm_24_triplet(b, c) = wmo_interm_24_triplet(b, c) + sum 
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
! sum = sum + r2m(vrdav_Rl, a,j,b,i) * r2m(vrdav_Rr, a,i,b,k)
! end do 
! end do 
! end do 
! wmo_interm_25_triplet(j, k) = wmo_interm_25_triplet(j, k) + sum 
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
! wmo_interm_26_triplet(j, k) = wmo_interm_26_triplet(j, k) + sum 
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
! wmo_interm_27_triplet(b, c) = wmo_interm_27_triplet(b, c) + sum 
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
! wmo_interm_28_triplet(b, c) = wmo_interm_28_triplet(b, c) + sum 
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
! wmo_interm_29_triplet(a, b) = wmo_interm_29_triplet(a, b) + sum 
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
! wmo_interm_30_triplet(b, c) = wmo_interm_30_triplet(b, c) + sum 
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
! wmo_interm_31_triplet(b, c) = wmo_interm_31_triplet(b, c) + sum 
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
! wmo_interm_32_triplet(j, k) = wmo_interm_32_triplet(j, k) + sum 
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
! wmo_interm_33_triplet(j, k) = wmo_interm_33_triplet(j, k) + sum 
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
! wmo_interm_34_triplet(b, c) = wmo_interm_34_triplet(b, c) + sum 
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
! wmo_interm_35_triplet(b, c) = wmo_interm_35_triplet(b, c) + sum 
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
! wmo_interm_36_triplet(j, k) = wmo_interm_36_triplet(j, k) + sum 
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
! wmo_interm_37_triplet(j, k) = wmo_interm_37_triplet(j, k) + sum 
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
! wmo_interm_38_triplet(j, k) = wmo_interm_38_triplet(j, k) + sum 
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
! wmo_interm_39_triplet(j, k) = wmo_interm_39_triplet(j, k) + sum 
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
! wmo_interm_40_triplet(b, j) = wmo_interm_40_triplet(b, j) + sum 
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
! wmo_interm_41_triplet(b, j) = wmo_interm_41_triplet(b, j) + sum 
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
! wmo_interm_42_triplet(b, j) = wmo_interm_42_triplet(b, j) + sum 
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
! wmo_interm_43_triplet(b, c) = wmo_interm_43_triplet(b, c) + sum 
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
! wmo_interm_44_triplet(a, b) = wmo_interm_44_triplet(a, b) + sum 
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
! wmo_interm_45_triplet(b, c) = wmo_interm_45_triplet(b, c) + sum 
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
! wmo_interm_46_triplet(j, k) = wmo_interm_46_triplet(j, k) + sum 
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
! wmo_interm_47_triplet(j, k) = wmo_interm_47_triplet(j, k) + sum 
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
! wmo_interm_48_triplet(b, j) = wmo_interm_48_triplet(b, j) + sum 
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
! wmo_interm_49_triplet(b, c) = wmo_interm_49_triplet(b, c) + sum 
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
! wmo_interm_50_triplet(b, j) = wmo_interm_50_triplet(b, j) + sum 
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
! wmo_interm_51_triplet(b, c) = wmo_interm_51_triplet(b, c) + sum 
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
! wmo_interm_52_triplet(j, k) = wmo_interm_52_triplet(j, k) + sum 
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
! wmo_interm_53_triplet(j, k) = wmo_interm_53_triplet(j, k) + sum 
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
! wmo_interm_54_triplet(j, k) = wmo_interm_54_triplet(j, k) + sum 
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
! wmo_interm_55_triplet(j, k) = wmo_interm_55_triplet(j, k) + sum 
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
! wmo_interm_56_triplet(b, j) = wmo_interm_56_triplet(b, j) + sum 
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
! wmo_interm_57_triplet(b, j) = wmo_interm_57_triplet(b, j) + sum 
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
! wmo_interm_58_triplet(b, j) = wmo_interm_58_triplet(b, j) + sum 
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
! wmo_interm_59_triplet(a, b) = wmo_interm_59_triplet(a, b) + sum 
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
! wmo_interm_60_triplet(b, c) = wmo_interm_60_triplet(b, c) + sum 
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
! wmo_interm_61_triplet(b, c) = wmo_interm_61_triplet(b, c) + sum 
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
! wmo_interm_62_triplet(b, j) = wmo_interm_62_triplet(b, j) + sum 
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
! wmo_interm_63_triplet(i, j, k, l) = wmo_interm_63_triplet(i, j, k, l) + sum 
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
! wmo_interm_64_triplet(i, j, k, l) = wmo_interm_64_triplet(i, j, k, l) + sum 
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
! wmo_interm_65_triplet(i, j, k, l) = wmo_interm_65_triplet(i, j, k, l) + sum 
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
! wmo_interm_66_triplet(i, j, k, l) = wmo_interm_66_triplet(i, j, k, l) + sum 
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
! wmo_interm_67_triplet(i, j, k, l) = wmo_interm_67_triplet(i, j, k, l) + sum 
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
! sum = sum + r2m(vrdav_Rr, a,i,b,j) * s2(a,b,k,l)
! end do 
! end do 
! wmo_interm_68_triplet(i, j, k, l) = wmo_interm_68_triplet(i, j, k, l) + sum 
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
! sum = sum + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, a,k,b,l)
! end do 
! end do 
! wmo_interm_69_triplet(i, j, k, l) = wmo_interm_69_triplet(i, j, k, l) + sum 
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
! wmo_interm_70_triplet(i, j, k, l) = wmo_interm_70_triplet(i, j, k, l) + sum 
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
! wmo_interm_71_triplet(i, j, k, l) = wmo_interm_71_triplet(i, j, k, l) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, b, c, j, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do b = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! do j = 1, nocc 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r2p(vrdav_Rl, b,j,a,i) * r2p(vrdav_Rr, c,k,a,i)
! end do 
! end do 
! wmo_interm_72_triplet(b, c, j, k) = wmo_interm_72_triplet(b, c, j, k) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, b, c, j, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do b = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! do j = 1, nocc 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + s2(a,b,j,i) * t2(a,c,k,i)
! end do 
! end do 
! wmo_interm_73_triplet(b, c, j, k) = wmo_interm_73_triplet(b, c, j, k) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, b, c, j, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do b = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! do j = 1, nocc 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + s2(a,b,i,j) * t2(a,c,k,i)
! end do 
! end do 
! wmo_interm_74_triplet(b, c, j, k) = wmo_interm_74_triplet(b, c, j, k) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, b, c, j, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do b = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! do j = 1, nocc 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + s2(a,b,i,j) * t2(a,c,i,k)
! end do 
! end do 
! wmo_interm_75_triplet(b, c, j, k) = wmo_interm_75_triplet(b, c, j, k) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, b, c, j, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do b = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! do j = 1, nocc 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + s2(a,b,j,i) * t2(a,c,i,k)
! end do 
! end do 
! wmo_interm_76_triplet(b, c, j, k) = wmo_interm_76_triplet(b, c, j, k) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, b, c, j, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do b = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! do j = 1, nocc 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r2p(vrdav_Rl, b,j,a,i) * r2m(vrdav_Rr, a,k,c,i)
! end do 
! end do 
! wmo_interm_77_triplet(b, c, j, k) = wmo_interm_77_triplet(b, c, j, k) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, b, c, j, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do b = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! do j = 1, nocc 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r2p(vrdav_Rl, b,j,a,i) * r2m(vrdav_Rr, a,i,c,k)
! end do 
! end do 
! wmo_interm_78_triplet(b, c, j, k) = wmo_interm_78_triplet(b, c, j, k) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, b, c, j, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do b = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! do j = 1, nocc 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, c,k,a,i)
! end do 
! end do 
! wmo_interm_79_triplet(b, c, j, k) = wmo_interm_79_triplet(b, c, j, k) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, b, c, j, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do b = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! do j = 1, nocc 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r2m(vrdav_Rl, a,j,b,i) * r2p(vrdav_Rr, c,k,a,i)
! end do 
! end do 
! wmo_interm_80_triplet(b, c, j, k) = wmo_interm_80_triplet(b, c, j, k) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, b, c, j, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do b = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! do j = 1, nocc 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,k,c,i)
! end do 
! end do 
! wmo_interm_81_triplet(b, c, j, k) = wmo_interm_81_triplet(b, c, j, k) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, b, c, j, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do b = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! do j = 1, nocc 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,i,c,k)
! end do 
! end do 
! wmo_interm_82_triplet(b, c, j, k) = wmo_interm_82_triplet(b, c, j, k) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, b, c, j, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do b = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! do j = 1, nocc 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r2m(vrdav_Rl, a,j,b,i) * r2m(vrdav_Rr, a,i,c,k)
! end do 
! end do 
! wmo_interm_83_triplet(b, c, j, k) = wmo_interm_83_triplet(b, c, j, k) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, b, c, j, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do b = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! do j = 1, nocc 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r2m(vrdav_Rl, a,j,b,i) * r2m(vrdav_Rr, a,k,c,i)
! end do 
! end do 
! wmo_interm_84_triplet(b, c, j, k) = wmo_interm_84_triplet(b, c, j, k) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 



    end subroutine d_overlap_intermediates_triplet_ccsd

    
  subroutine d_overlap_triplet_cc3_init(nocc, nactive)
    integer, intent(in) :: nocc
    integer, intent(in) :: nactive
   
  allocate(wmo_interm_cc3_0_triplet(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_cc3_1_triplet(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_cc3_2_triplet(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_cc3_3_triplet(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_cc3_4_triplet(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_cc3_5_triplet(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_cc3_6_triplet(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_cc3_7_triplet(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_cc3_8_triplet(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_cc3_9_triplet(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_cc3_10_triplet(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_cc3_11_triplet(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_cc3_12_triplet(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_cc3_13_triplet(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_cc3_14_triplet(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_cc3_15_triplet(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_cc3_16_triplet(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_cc3_17_triplet(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_cc3_18_triplet(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_cc3_19_triplet(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_cc3_20_triplet(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_cc3_21_triplet(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_cc3_22_triplet(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_cc3_23_triplet(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_cc3_24_triplet(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_cc3_25_triplet(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_cc3_26_triplet(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_cc3_27_triplet(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_cc3_28_triplet(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_cc3_29_triplet(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_cc3_30_triplet(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_cc3_31_triplet(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_cc3_32_triplet(nocc+1: nactive, 1: nocc))
allocate(wmo_interm_cc3_33_triplet(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wmo_interm_cc3_34_triplet(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wmo_interm_cc3_35_triplet(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wmo_interm_cc3_36_triplet(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wmo_interm_cc3_37_triplet(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wmo_interm_cc3_38_triplet(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
wmo_interm_cc3_0_triplet = zero 
wmo_interm_cc3_1_triplet = zero 
wmo_interm_cc3_2_triplet = zero 
wmo_interm_cc3_3_triplet = zero 
wmo_interm_cc3_4_triplet = zero 
wmo_interm_cc3_5_triplet = zero 
wmo_interm_cc3_6_triplet = zero 
wmo_interm_cc3_7_triplet = zero 
wmo_interm_cc3_8_triplet = zero 
wmo_interm_cc3_9_triplet = zero 
wmo_interm_cc3_10_triplet = zero 
wmo_interm_cc3_11_triplet = zero 
wmo_interm_cc3_12_triplet = zero 
wmo_interm_cc3_13_triplet = zero 
wmo_interm_cc3_14_triplet = zero 
wmo_interm_cc3_15_triplet = zero 
wmo_interm_cc3_16_triplet = zero 
wmo_interm_cc3_17_triplet = zero 
wmo_interm_cc3_18_triplet = zero 
wmo_interm_cc3_19_triplet = zero 
wmo_interm_cc3_20_triplet = zero 
wmo_interm_cc3_21_triplet = zero 
wmo_interm_cc3_22_triplet = zero 
wmo_interm_cc3_23_triplet = zero 
wmo_interm_cc3_24_triplet = zero 
wmo_interm_cc3_25_triplet = zero 
wmo_interm_cc3_26_triplet = zero 
wmo_interm_cc3_27_triplet = zero 
wmo_interm_cc3_28_triplet = zero 
wmo_interm_cc3_29_triplet = zero 
wmo_interm_cc3_30_triplet = zero 
wmo_interm_cc3_31_triplet = zero 
wmo_interm_cc3_32_triplet = zero 
wmo_interm_cc3_33_triplet = zero 
wmo_interm_cc3_34_triplet = zero 
wmo_interm_cc3_35_triplet = zero 
wmo_interm_cc3_36_triplet = zero 
wmo_interm_cc3_37_triplet = zero 
wmo_interm_cc3_38_triplet = zero 
!     allocate(wmo_interm_cc3_0_triplet(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_1_triplet(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_2_triplet(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_3_triplet(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_4_triplet(nocc+1: nactive, 1: nocc))
! allocate(wmo_interm_cc3_5_triplet(nocc+1: nactive, 1: nocc))
! allocate(wmo_interm_cc3_6_triplet(nocc+1: nactive, 1: nocc))
! allocate(wmo_interm_cc3_7_triplet(nocc+1: nactive, 1: nocc))
! allocate(wmo_interm_cc3_8_triplet(nocc+1: nactive, 1: nocc))
! allocate(wmo_interm_cc3_9_triplet(nocc+1: nactive, 1: nocc))
! allocate(wmo_interm_cc3_10_triplet(nocc+1: nactive, 1: nocc))
! allocate(wmo_interm_cc3_11_triplet(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_12_triplet(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_13_triplet(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_14_triplet(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_15_triplet(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_16_triplet(nocc+1: nactive, 1: nocc))
! allocate(wmo_interm_cc3_17_triplet(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_18_triplet(nocc+1: nactive, 1: nocc))
! allocate(wmo_interm_cc3_19_triplet(nocc+1: nactive, 1: nocc))
! allocate(wmo_interm_cc3_20_triplet(nocc+1: nactive, 1: nocc))
! allocate(wmo_interm_cc3_21_triplet(nocc+1: nactive, 1: nocc))
! allocate(wmo_interm_cc3_22_triplet(nocc+1: nactive, 1: nocc))
! allocate(wmo_interm_cc3_23_triplet(nocc+1: nactive, 1: nocc))
! allocate(wmo_interm_cc3_24_triplet(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_25_triplet(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_26_triplet(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_27_triplet(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_28_triplet(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_29_triplet(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_30_triplet(nocc+1: nactive, 1: nocc))
! allocate(wmo_interm_cc3_31_triplet(nocc+1: nactive, 1: nocc))
! allocate(wmo_interm_cc3_32_triplet(nocc+1: nactive, 1: nocc))
! allocate(wmo_interm_cc3_33_triplet(nocc+1: nactive, 1: nocc))
! allocate(wmo_interm_cc3_34_triplet(nocc+1: nactive, 1: nocc))
! allocate(wmo_interm_cc3_35_triplet(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_36_triplet(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_37_triplet(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_38_triplet(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_39_triplet(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_40_triplet(nocc+1: nactive, 1: nocc))
! allocate(wmo_interm_cc3_41_triplet(nocc+1: nactive, 1: nocc))
! allocate(wmo_interm_cc3_42_triplet(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_43_triplet(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_44_triplet(nocc+1: nactive, 1: nocc))
! allocate(wmo_interm_cc3_45_triplet(nocc+1: nactive, 1: nocc))
! allocate(wmo_interm_cc3_46_triplet(nocc+1: nactive, 1: nocc))
! allocate(wmo_interm_cc3_47_triplet(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_48_triplet(nocc+1: nactive, 1: nocc))
! allocate(wmo_interm_cc3_49_triplet(nocc+1: nactive, 1: nocc))
! allocate(wmo_interm_cc3_50_triplet(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_51_triplet(nocc+1: nactive, 1: nocc))
! allocate(wmo_interm_cc3_52_triplet(nocc+1: nactive, 1: nocc))
! allocate(wmo_interm_cc3_53_triplet(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_54_triplet(nocc+1: nactive, 1: nocc))
! allocate(wmo_interm_cc3_55_triplet(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_56_triplet(nocc+1: nactive, 1: nocc))
! allocate(wmo_interm_cc3_57_triplet(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_58_triplet(nocc+1: nactive, 1: nocc))
! allocate(wmo_interm_cc3_59_triplet(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_60_triplet(nocc+1: nactive, 1: nocc))
! allocate(wmo_interm_cc3_61_triplet(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_62_triplet(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_63_triplet(nocc+1: nactive, 1: nocc))
! allocate(wmo_interm_cc3_64_triplet(1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_65_triplet(1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_66_triplet(1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_67_triplet(1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_68_triplet(1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_69_triplet(1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_70_triplet(1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_71_triplet(1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_72_triplet(1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_73_triplet(1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_74_triplet(1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_75_triplet(nocc+1: nactive, nocc+1: nactive))
! allocate(wmo_interm_cc3_76_triplet(nocc+1: nactive, nocc+1: nactive))
! allocate(wmo_interm_cc3_77_triplet(nocc+1: nactive, nocc+1: nactive))
! allocate(wmo_interm_cc3_78_triplet(nocc+1: nactive, nocc+1: nactive))
! allocate(wmo_interm_cc3_79_triplet(nocc+1: nactive, nocc+1: nactive))
! allocate(wmo_interm_cc3_80_triplet(nocc+1: nactive, nocc+1: nactive))
! allocate(wmo_interm_cc3_81_triplet(nocc+1: nactive, nocc+1: nactive))
! allocate(wmo_interm_cc3_82_triplet(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_83_triplet(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_84_triplet(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_85_triplet(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_86_triplet(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
! allocate(wmo_interm_cc3_87_triplet(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
! wmo_interm_cc3_0_triplet = zero 
! wmo_interm_cc3_1_triplet = zero 
! wmo_interm_cc3_2_triplet = zero 
! wmo_interm_cc3_3_triplet = zero 
! wmo_interm_cc3_4_triplet = zero 
! wmo_interm_cc3_5_triplet = zero 
! wmo_interm_cc3_6_triplet = zero 
! wmo_interm_cc3_7_triplet = zero 
! wmo_interm_cc3_8_triplet = zero 
! wmo_interm_cc3_9_triplet = zero 
! wmo_interm_cc3_10_triplet = zero 
! wmo_interm_cc3_11_triplet = zero 
! wmo_interm_cc3_12_triplet = zero 
! wmo_interm_cc3_13_triplet = zero 
! wmo_interm_cc3_14_triplet = zero 
! wmo_interm_cc3_15_triplet = zero 
! wmo_interm_cc3_16_triplet = zero 
! wmo_interm_cc3_17_triplet = zero 
! wmo_interm_cc3_18_triplet = zero 
! wmo_interm_cc3_19_triplet = zero 
! wmo_interm_cc3_20_triplet = zero 
! wmo_interm_cc3_21_triplet = zero 
! wmo_interm_cc3_22_triplet = zero 
! wmo_interm_cc3_23_triplet = zero 
! wmo_interm_cc3_24_triplet = zero 
! wmo_interm_cc3_25_triplet = zero 
! wmo_interm_cc3_26_triplet = zero 
! wmo_interm_cc3_27_triplet = zero 
! wmo_interm_cc3_28_triplet = zero 
! wmo_interm_cc3_29_triplet = zero 
! wmo_interm_cc3_30_triplet = zero 
! wmo_interm_cc3_31_triplet = zero 
! wmo_interm_cc3_32_triplet = zero 
! wmo_interm_cc3_33_triplet = zero 
! wmo_interm_cc3_34_triplet = zero 
! wmo_interm_cc3_35_triplet = zero 
! wmo_interm_cc3_36_triplet = zero 
! wmo_interm_cc3_37_triplet = zero 
! wmo_interm_cc3_38_triplet = zero 
! wmo_interm_cc3_39_triplet = zero 
! wmo_interm_cc3_40_triplet = zero 
! wmo_interm_cc3_41_triplet = zero 
! wmo_interm_cc3_42_triplet = zero 
! wmo_interm_cc3_43_triplet = zero 
! wmo_interm_cc3_44_triplet = zero 
! wmo_interm_cc3_45_triplet = zero 
! wmo_interm_cc3_46_triplet = zero 
! wmo_interm_cc3_47_triplet = zero 
! wmo_interm_cc3_48_triplet = zero 
! wmo_interm_cc3_49_triplet = zero 
! wmo_interm_cc3_50_triplet = zero 
! wmo_interm_cc3_51_triplet = zero 
! wmo_interm_cc3_52_triplet = zero 
! wmo_interm_cc3_53_triplet = zero 
! wmo_interm_cc3_54_triplet = zero 
! wmo_interm_cc3_55_triplet = zero 
! wmo_interm_cc3_56_triplet = zero 
! wmo_interm_cc3_57_triplet = zero 
! wmo_interm_cc3_58_triplet = zero 
! wmo_interm_cc3_59_triplet = zero 
! wmo_interm_cc3_60_triplet = zero 
! wmo_interm_cc3_61_triplet = zero 
! wmo_interm_cc3_62_triplet = zero 
! wmo_interm_cc3_63_triplet = zero 
! wmo_interm_cc3_64_triplet = zero 
! wmo_interm_cc3_65_triplet = zero 
! wmo_interm_cc3_66_triplet = zero 
! wmo_interm_cc3_67_triplet = zero 
! wmo_interm_cc3_68_triplet = zero 
! wmo_interm_cc3_69_triplet = zero 
! wmo_interm_cc3_70_triplet = zero 
! wmo_interm_cc3_71_triplet = zero 
! wmo_interm_cc3_72_triplet = zero 
! wmo_interm_cc3_73_triplet = zero 
! wmo_interm_cc3_74_triplet = zero 
! wmo_interm_cc3_75_triplet = zero 
! wmo_interm_cc3_76_triplet = zero 
! wmo_interm_cc3_77_triplet = zero 
! wmo_interm_cc3_78_triplet = zero 
! wmo_interm_cc3_79_triplet = zero 
! wmo_interm_cc3_80_triplet = zero 
! wmo_interm_cc3_81_triplet = zero 
! wmo_interm_cc3_82_triplet = zero 
! wmo_interm_cc3_83_triplet = zero 
! wmo_interm_cc3_84_triplet = zero 
! wmo_interm_cc3_85_triplet = zero 
! wmo_interm_cc3_86_triplet = zero 
! wmo_interm_cc3_87_triplet = zero 

    end subroutine d_overlap_triplet_cc3_init
    
    subroutine d_overlap_triplet_cc3_free
    deallocate(wmo_interm_cc3_0_triplet)
deallocate(wmo_interm_cc3_1_triplet)
deallocate(wmo_interm_cc3_2_triplet)
deallocate(wmo_interm_cc3_3_triplet)
deallocate(wmo_interm_cc3_4_triplet)
deallocate(wmo_interm_cc3_5_triplet)
deallocate(wmo_interm_cc3_6_triplet)
deallocate(wmo_interm_cc3_7_triplet)
deallocate(wmo_interm_cc3_8_triplet)
deallocate(wmo_interm_cc3_9_triplet)
deallocate(wmo_interm_cc3_10_triplet)
deallocate(wmo_interm_cc3_11_triplet)
deallocate(wmo_interm_cc3_12_triplet)
deallocate(wmo_interm_cc3_13_triplet)
deallocate(wmo_interm_cc3_14_triplet)
deallocate(wmo_interm_cc3_15_triplet)
deallocate(wmo_interm_cc3_16_triplet)
deallocate(wmo_interm_cc3_17_triplet)
deallocate(wmo_interm_cc3_18_triplet)
deallocate(wmo_interm_cc3_19_triplet)
deallocate(wmo_interm_cc3_20_triplet)
deallocate(wmo_interm_cc3_21_triplet)
deallocate(wmo_interm_cc3_22_triplet)
deallocate(wmo_interm_cc3_23_triplet)
deallocate(wmo_interm_cc3_24_triplet)
deallocate(wmo_interm_cc3_25_triplet)
deallocate(wmo_interm_cc3_26_triplet)
deallocate(wmo_interm_cc3_27_triplet)
deallocate(wmo_interm_cc3_28_triplet)
deallocate(wmo_interm_cc3_29_triplet)
deallocate(wmo_interm_cc3_30_triplet)
deallocate(wmo_interm_cc3_31_triplet)
deallocate(wmo_interm_cc3_32_triplet)
deallocate(wmo_interm_cc3_33_triplet)
deallocate(wmo_interm_cc3_34_triplet)
deallocate(wmo_interm_cc3_35_triplet)
deallocate(wmo_interm_cc3_36_triplet)
deallocate(wmo_interm_cc3_37_triplet)
deallocate(wmo_interm_cc3_38_triplet)
! deallocate(wmo_interm_cc3_39_triplet)
! deallocate(wmo_interm_cc3_40_triplet)
! deallocate(wmo_interm_cc3_41_triplet)
! deallocate(wmo_interm_cc3_42_triplet)
! deallocate(wmo_interm_cc3_43_triplet)
! deallocate(wmo_interm_cc3_44_triplet)
! deallocate(wmo_interm_cc3_45_triplet)
! deallocate(wmo_interm_cc3_46_triplet)
! deallocate(wmo_interm_cc3_47_triplet)
! deallocate(wmo_interm_cc3_48_triplet)
! deallocate(wmo_interm_cc3_49_triplet)
! deallocate(wmo_interm_cc3_50_triplet)
! deallocate(wmo_interm_cc3_51_triplet)
! deallocate(wmo_interm_cc3_52_triplet)
! deallocate(wmo_interm_cc3_53_triplet)
! deallocate(wmo_interm_cc3_54_triplet)
! deallocate(wmo_interm_cc3_55_triplet)
! deallocate(wmo_interm_cc3_56_triplet)
! deallocate(wmo_interm_cc3_57_triplet)
! deallocate(wmo_interm_cc3_58_triplet)
! deallocate(wmo_interm_cc3_59_triplet)
! deallocate(wmo_interm_cc3_60_triplet)
! deallocate(wmo_interm_cc3_61_triplet)
! deallocate(wmo_interm_cc3_62_triplet)
! deallocate(wmo_interm_cc3_63_triplet)
! deallocate(wmo_interm_cc3_64_triplet)
! deallocate(wmo_interm_cc3_65_triplet)
! deallocate(wmo_interm_cc3_66_triplet)
! deallocate(wmo_interm_cc3_67_triplet)
! deallocate(wmo_interm_cc3_68_triplet)
! deallocate(wmo_interm_cc3_69_triplet)
! deallocate(wmo_interm_cc3_70_triplet)
! deallocate(wmo_interm_cc3_71_triplet)
! deallocate(wmo_interm_cc3_72_triplet)
! deallocate(wmo_interm_cc3_73_triplet)
! deallocate(wmo_interm_cc3_74_triplet)
! deallocate(wmo_interm_cc3_75_triplet)
! deallocate(wmo_interm_cc3_76_triplet)
! deallocate(wmo_interm_cc3_77_triplet)
! deallocate(wmo_interm_cc3_78_triplet)
! deallocate(wmo_interm_cc3_79_triplet)
! deallocate(wmo_interm_cc3_80_triplet)
! deallocate(wmo_interm_cc3_81_triplet)
! deallocate(wmo_interm_cc3_82_triplet)
! deallocate(wmo_interm_cc3_83_triplet)
! deallocate(wmo_interm_cc3_84_triplet)
! deallocate(wmo_interm_cc3_85_triplet)
! deallocate(wmo_interm_cc3_86_triplet)
! deallocate(wmo_interm_cc3_87_triplet)

    end subroutine d_overlap_triplet_cc3_free
    
    subroutine d_overlap_intermediates_triplet_cc3(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr)
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    double precision, dimension(:), intent(in) :: vrdav_Rl
    double precision, dimension(:), intent(in) :: vrdav_Rr
    real(F64) :: sum
!    integer :: a, b, i, j, k, c, l, d 
    integer :: a, i, b, j, c, k 

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
wmo_interm_cc3_0_triplet(b, j) = wmo_interm_cc3_0_triplet(b, j) + sum 
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
wmo_interm_cc3_1_triplet(c, k) = wmo_interm_cc3_1_triplet(c, k) + sum 
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
wmo_interm_cc3_2_triplet(c, k) = wmo_interm_cc3_2_triplet(c, k) + sum 
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
wmo_interm_cc3_3_triplet(c, k) = wmo_interm_cc3_3_triplet(c, k) + sum 
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
wmo_interm_cc3_4_triplet(c, k) = wmo_interm_cc3_4_triplet(c, k) + sum 
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
wmo_interm_cc3_5_triplet(c, k) = wmo_interm_cc3_5_triplet(c, k) + sum 
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
wmo_interm_cc3_6_triplet(c, k) = wmo_interm_cc3_6_triplet(c, k) + sum 
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
wmo_interm_cc3_7_triplet(b, j) = wmo_interm_cc3_7_triplet(b, j) + sum 
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
wmo_interm_cc3_8_triplet(c, k) = wmo_interm_cc3_8_triplet(c, k) + sum 
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
wmo_interm_cc3_9_triplet(c, k) = wmo_interm_cc3_9_triplet(c, k) + sum 
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
wmo_interm_cc3_10_triplet(c, k) = wmo_interm_cc3_10_triplet(c, k) + sum 
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
wmo_interm_cc3_11_triplet(c, k) = wmo_interm_cc3_11_triplet(c, k) + sum 
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
wmo_interm_cc3_12_triplet(c, k) = wmo_interm_cc3_12_triplet(c, k) + sum 
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
wmo_interm_cc3_13_triplet(c, k) = wmo_interm_cc3_13_triplet(c, k) + sum 
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
wmo_interm_cc3_14_triplet(b, j) = wmo_interm_cc3_14_triplet(b, j) + sum 
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
wmo_interm_cc3_15_triplet(c, k) = wmo_interm_cc3_15_triplet(c, k) + sum 
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
wmo_interm_cc3_16_triplet(c, k) = wmo_interm_cc3_16_triplet(c, k) + sum 
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
wmo_interm_cc3_17_triplet(c, k) = wmo_interm_cc3_17_triplet(c, k) + sum 
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
wmo_interm_cc3_18_triplet(c, k) = wmo_interm_cc3_18_triplet(c, k) + sum 
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
wmo_interm_cc3_19_triplet(c, k) = wmo_interm_cc3_19_triplet(c, k) + sum 
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
wmo_interm_cc3_20_triplet(b, j) = wmo_interm_cc3_20_triplet(b, j) + sum 
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
wmo_interm_cc3_21_triplet(c, k) = wmo_interm_cc3_21_triplet(c, k) + sum 
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
wmo_interm_cc3_22_triplet(b, j) = wmo_interm_cc3_22_triplet(b, j) + sum 
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
wmo_interm_cc3_23_triplet(c, k) = wmo_interm_cc3_23_triplet(c, k) + sum 
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
wmo_interm_cc3_24_triplet(c, k) = wmo_interm_cc3_24_triplet(c, k) + sum 
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
wmo_interm_cc3_25_triplet(c, k) = wmo_interm_cc3_25_triplet(c, k) + sum 
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
wmo_interm_cc3_26_triplet(c, k) = wmo_interm_cc3_26_triplet(c, k) + sum 
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
wmo_interm_cc3_27_triplet(b, j) = wmo_interm_cc3_27_triplet(b, j) + sum 
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
wmo_interm_cc3_28_triplet(b, j) = wmo_interm_cc3_28_triplet(b, j) + sum 
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
wmo_interm_cc3_29_triplet(c, k) = wmo_interm_cc3_29_triplet(c, k) + sum 
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
wmo_interm_cc3_30_triplet(c, k) = wmo_interm_cc3_30_triplet(c, k) + sum 
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
wmo_interm_cc3_31_triplet(c, k) = wmo_interm_cc3_31_triplet(c, k) + sum 
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
wmo_interm_cc3_32_triplet(c, k) = wmo_interm_cc3_32_triplet(c, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, c, j, k, sum)& 
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
wmo_interm_cc3_33_triplet(b, c, j, k) = wmo_interm_cc3_33_triplet(b, c, j, k) + sum 
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
wmo_interm_cc3_34_triplet(b, c, j, k) = wmo_interm_cc3_34_triplet(b, c, j, k) + sum 
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
wmo_interm_cc3_35_triplet(b, c, j, k) = wmo_interm_cc3_35_triplet(b, c, j, k) + sum 
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
sum = sum + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,c,i,j,k)
end do 
end do 
wmo_interm_cc3_36_triplet(b, c, j, k) = wmo_interm_cc3_36_triplet(b, c, j, k) + sum 
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
wmo_interm_cc3_37_triplet(b, c, j, k) = wmo_interm_cc3_37_triplet(b, c, j, k) + sum 
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
wmo_interm_cc3_38_triplet(b, c, j, k) = wmo_interm_cc3_38_triplet(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!     !$omp parallel private(a, b, i, j, k, sum)& 
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
! wmo_interm_cc3_0_triplet(b, i, j, k) = wmo_interm_cc3_0_triplet(b, i, j, k) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, c, j, k, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do c = nocc + 1, nactive 
! do j = 1, nocc 
! do k = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + t3(nocc, nactive, a,b,c,j,k,i) * t2(a,b,l,i)
! end do 
! end do 
! end do 
! wmo_interm_cc3_1_triplet(c, j, k, l) = wmo_interm_cc3_1_triplet(c, j, k, l) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, c, k, j, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do c = nocc + 1, nactive 
! do k = 1, nocc 
! do j = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + t3(nocc, nactive, a,b,c,i,k,j) * t2(a,b,l,i)
! end do 
! end do 
! end do 
! wmo_interm_cc3_2_triplet(c, k, j, l) = wmo_interm_cc3_2_triplet(c, k, j, l) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, c, k, j, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do c = nocc + 1, nactive 
! do k = 1, nocc 
! do j = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do b = nocc + 1, nactive 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + t3(nocc, nactive, a,b,c,k,i,j) * t2(a,b,l,i)
! end do 
! end do 
! end do 
! wmo_interm_cc3_3_triplet(c, k, j, l) = wmo_interm_cc3_3_triplet(c, k, j, l) + sum 
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
! sum = sum + r1(vrdav_Rl, a,i) * t2(a,b,j,i)
! end do 
! end do 
! wmo_interm_cc3_4_triplet(b, j) = wmo_interm_cc3_4_triplet(b, j) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, c, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do c = nocc + 1, nactive 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do j = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,k,j,i)
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_5_triplet(c, k) = wmo_interm_cc3_5_triplet(c, k) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, c, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do c = nocc + 1, nactive 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do j = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,j,k,i)
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_6_triplet(c, k) = wmo_interm_cc3_6_triplet(c, k) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, c, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do c = nocc + 1, nactive 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do j = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,i,j,k)
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_7_triplet(c, k) = wmo_interm_cc3_7_triplet(c, k) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, c, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do c = nocc + 1, nactive 
! do k = 1, nocc 
! sum = zero 
! do j = 1, nocc 
! do b = nocc + 1, nactive 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,j,i,k)
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_8_triplet(c, k) = wmo_interm_cc3_8_triplet(c, k) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, c, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do c = nocc + 1, nactive 
! do k = 1, nocc 
! sum = zero 
! do j = 1, nocc 
! do b = nocc + 1, nactive 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,k,i,j)
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_9_triplet(c, k) = wmo_interm_cc3_9_triplet(c, k) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, c, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do c = nocc + 1, nactive 
! do k = 1, nocc 
! sum = zero 
! do j = 1, nocc 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,i,k,j)
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_10_triplet(c, k) = wmo_interm_cc3_10_triplet(c, k) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, c, k, j, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do c = nocc + 1, nactive 
! do k = 1, nocc 
! do j = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + t3(nocc, nactive, a,b,c,k,j,i) * t2(a,b,l,i)
! end do 
! end do 
! end do 
! wmo_interm_cc3_11_triplet(c, k, j, l) = wmo_interm_cc3_11_triplet(c, k, j, l) + sum 
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
! sum = sum + r1(vrdav_Rl, a,i) * t2(a,b,j,k)
! end do 
! wmo_interm_cc3_12_triplet(b, i, j, k) = wmo_interm_cc3_12_triplet(b, i, j, k) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, c, j, k, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do c = nocc + 1, nactive 
! do j = 1, nocc 
! do k = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + r2p(vrdav_Rr, a,j,b,i) * t3(nocc, nactive, a,b,c,k,l,i)
! end do 
! end do 
! end do 
! wmo_interm_cc3_13_triplet(c, j, k, l) = wmo_interm_cc3_13_triplet(c, j, k, l) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, c, j, k, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do c = nocc + 1, nactive 
! do j = 1, nocc 
! do k = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + r2p(vrdav_Rr, a,j,b,i) * t3(nocc, nactive, a,b,c,i,k,l)
! end do 
! end do 
! end do 
! wmo_interm_cc3_14_triplet(c, j, k, l) = wmo_interm_cc3_14_triplet(c, j, k, l) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, c, j, k, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do c = nocc + 1, nactive 
! do j = 1, nocc 
! do k = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do b = nocc + 1, nactive 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r2p(vrdav_Rr, a,j,b,i) * t3(nocc, nactive, a,b,c,k,i,l)
! end do 
! end do 
! end do 
! wmo_interm_cc3_15_triplet(c, j, k, l) = wmo_interm_cc3_15_triplet(c, j, k, l) + sum 
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
! sum = sum + r1(vrdav_Rl, a,i) * t2(a,b,i,j)
! end do 
! end do 
! wmo_interm_cc3_16_triplet(b, j) = wmo_interm_cc3_16_triplet(b, j) + sum 
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
! wmo_interm_cc3_17_triplet(b, i, j, k) = wmo_interm_cc3_17_triplet(b, i, j, k) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, c, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do c = nocc + 1, nactive 
! do k = 1, nocc 
! sum = zero 
! do j = 1, nocc 
! do b = nocc + 1, nactive 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r2m(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,k,i,j)
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_18_triplet(c, k) = wmo_interm_cc3_18_triplet(c, k) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, c, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do c = nocc + 1, nactive 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do j = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r2m(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,k,j,i)
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_19_triplet(c, k) = wmo_interm_cc3_19_triplet(c, k) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, c, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do c = nocc + 1, nactive 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do j = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + r2m(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,j,k,i)
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_20_triplet(c, k) = wmo_interm_cc3_20_triplet(c, k) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, c, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do c = nocc + 1, nactive 
! do k = 1, nocc 
! sum = zero 
! do j = 1, nocc 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + r2m(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,i,k,j)
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_21_triplet(c, k) = wmo_interm_cc3_21_triplet(c, k) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, c, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do c = nocc + 1, nactive 
! do k = 1, nocc 
! sum = zero 
! do j = 1, nocc 
! do b = nocc + 1, nactive 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r2m(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,j,i,k)
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_22_triplet(c, k) = wmo_interm_cc3_22_triplet(c, k) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, c, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do c = nocc + 1, nactive 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do j = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r2m(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,i,j,k)
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_23_triplet(c, k) = wmo_interm_cc3_23_triplet(c, k) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, c, j, k, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do c = nocc + 1, nactive 
! do j = 1, nocc 
! do k = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + r2m(vrdav_Rr, a,j,b,i) * t3(nocc, nactive, a,b,c,k,l,i)
! end do 
! end do 
! end do 
! wmo_interm_cc3_24_triplet(c, j, k, l) = wmo_interm_cc3_24_triplet(c, j, k, l) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, c, j, k, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do c = nocc + 1, nactive 
! do j = 1, nocc 
! do k = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + r2m(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,k,l,i)
! end do 
! end do 
! end do 
! wmo_interm_cc3_25_triplet(c, j, k, l) = wmo_interm_cc3_25_triplet(c, j, k, l) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, c, j, k, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do c = nocc + 1, nactive 
! do j = 1, nocc 
! do k = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + r2m(vrdav_Rr, a,j,b,i) * t3(nocc, nactive, a,b,c,i,k,l)
! end do 
! end do 
! end do 
! wmo_interm_cc3_26_triplet(c, j, k, l) = wmo_interm_cc3_26_triplet(c, j, k, l) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, c, j, k, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do c = nocc + 1, nactive 
! do j = 1, nocc 
! do k = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + r2m(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,i,k,l)
! end do 
! end do 
! end do 
! wmo_interm_cc3_27_triplet(c, j, k, l) = wmo_interm_cc3_27_triplet(c, j, k, l) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, c, j, k, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do c = nocc + 1, nactive 
! do j = 1, nocc 
! do k = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do b = nocc + 1, nactive 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r2m(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,k,i,l)
! end do 
! end do 
! end do 
! wmo_interm_cc3_28_triplet(c, j, k, l) = wmo_interm_cc3_28_triplet(c, j, k, l) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, c, j, k, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do c = nocc + 1, nactive 
! do j = 1, nocc 
! do k = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do b = nocc + 1, nactive 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r2m(vrdav_Rr, a,j,b,i) * t3(nocc, nactive, a,b,c,k,i,l)
! end do 
! end do 
! end do 
! wmo_interm_cc3_29_triplet(c, j, k, l) = wmo_interm_cc3_29_triplet(c, j, k, l) + sum 
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
! sum = sum + r2p(vrdav_Rl, b,j,a,i) * r1(vrdav_Rr, a,i)
! end do 
! end do 
! wmo_interm_cc3_30_triplet(b, j) = wmo_interm_cc3_30_triplet(b, j) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, c, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do c = nocc + 1, nactive 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do j = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + s2(a,b,j,i) * t3(nocc, nactive, a,b,c,k,j,i)
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_31_triplet(c, k) = wmo_interm_cc3_31_triplet(c, k) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, c, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do c = nocc + 1, nactive 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do j = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + s2(a,b,i,j) * t3(nocc, nactive, a,b,c,k,j,i)
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_32_triplet(c, k) = wmo_interm_cc3_32_triplet(c, k) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, c, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do c = nocc + 1, nactive 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do j = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + s2(a,b,i,j) * t3(nocc, nactive, a,b,c,i,j,k)
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_33_triplet(c, k) = wmo_interm_cc3_33_triplet(c, k) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, c, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do c = nocc + 1, nactive 
! do k = 1, nocc 
! sum = zero 
! do j = 1, nocc 
! do b = nocc + 1, nactive 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + s2(a,b,i,j) * t3(nocc, nactive, a,b,c,j,i,k)
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_34_triplet(c, k) = wmo_interm_cc3_34_triplet(c, k) + sum 
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
! wmo_interm_cc3_35_triplet(b, i, j, k) = wmo_interm_cc3_35_triplet(b, i, j, k) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, c, j, l, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do c = nocc + 1, nactive 
! do j = 1, nocc 
! do l = 1, nocc 
! do k = 1, nocc 
! sum = zero 
! do b = nocc + 1, nactive 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + s2(a,b,i,j) * t3(nocc, nactive, a,b,c,l,i,k)
! end do 
! end do 
! end do 
! wmo_interm_cc3_36_triplet(c, j, l, k) = wmo_interm_cc3_36_triplet(c, j, l, k) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, c, j, k, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do c = nocc + 1, nactive 
! do j = 1, nocc 
! do k = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + s2(a,b,j,i) * t3(nocc, nactive, a,b,c,k,l,i)
! end do 
! end do 
! end do 
! wmo_interm_cc3_37_triplet(c, j, k, l) = wmo_interm_cc3_37_triplet(c, j, k, l) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, c, j, k, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do c = nocc + 1, nactive 
! do j = 1, nocc 
! do k = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + s2(a,b,i,j) * t3(nocc, nactive, a,b,c,k,l,i)
! end do 
! end do 
! end do 
! wmo_interm_cc3_38_triplet(c, j, k, l) = wmo_interm_cc3_38_triplet(c, j, k, l) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, c, j, l, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do c = nocc + 1, nactive 
! do j = 1, nocc 
! do l = 1, nocc 
! do k = 1, nocc 
! sum = zero 
! do b = nocc + 1, nactive 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + s2(a,b,j,i) * t3(nocc, nactive, a,b,c,l,i,k)
! end do 
! end do 
! end do 
! wmo_interm_cc3_39_triplet(c, j, l, k) = wmo_interm_cc3_39_triplet(c, j, l, k) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, c, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do c = nocc + 1, nactive 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do j = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r2p(vrdav_Rl, b,i,a,j) * t3(nocc, nactive, a,b,c,k,j,i)
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_40_triplet(c, k) = wmo_interm_cc3_40_triplet(c, k) + sum 
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
! wmo_interm_cc3_41_triplet(b, j) = wmo_interm_cc3_41_triplet(b, j) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, c, j, k, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do c = nocc + 1, nactive 
! do j = 1, nocc 
! do k = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + r2p(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, a,b,c,k,l,i)
! end do 
! end do 
! end do 
! wmo_interm_cc3_42_triplet(c, j, k, l) = wmo_interm_cc3_42_triplet(c, j, k, l) + sum 
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
! sum = sum + r1(vrdav_Rr, a,i) * s2(a,b,j,k)
! end do 
! wmo_interm_cc3_43_triplet(b, i, j, k) = wmo_interm_cc3_43_triplet(b, i, j, k) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, c, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do c = nocc + 1, nactive 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do j = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r2p(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,b,c,k,j,i)
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_44_triplet(c, k) = wmo_interm_cc3_44_triplet(c, k) + sum 
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
! wmo_interm_cc3_45_triplet(b, j) = wmo_interm_cc3_45_triplet(b, j) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, c, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do c = nocc + 1, nactive 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! do j = 1, nocc 
! sum = sum + r2p(vrdav_Rl, b,j,a,i) * t3(nocc, nactive, a,b,c,k,j,i)
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_46_triplet(c, k) = wmo_interm_cc3_46_triplet(c, k) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, c, j, k, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do c = nocc + 1, nactive 
! do j = 1, nocc 
! do k = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do b = nocc + 1, nactive 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r2p(vrdav_Rl, b,j,a,i) * t3(nocc, nactive, a,b,c,k,i,l)
! end do 
! end do 
! end do 
! wmo_interm_cc3_47_triplet(c, j, k, l) = wmo_interm_cc3_47_triplet(c, j, k, l) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, c, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do c = nocc + 1, nactive 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do j = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r2p(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,b,c,i,j,k)
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_48_triplet(c, k) = wmo_interm_cc3_48_triplet(c, k) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, c, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do c = nocc + 1, nactive 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do j = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r2p(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, a,b,c,k,j,i)
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_49_triplet(c, k) = wmo_interm_cc3_49_triplet(c, k) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, c, j, k, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do c = nocc + 1, nactive 
! do j = 1, nocc 
! do k = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do b = nocc + 1, nactive 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r2p(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, a,b,c,k,i,l)
! end do 
! end do 
! end do 
! wmo_interm_cc3_50_triplet(c, j, k, l) = wmo_interm_cc3_50_triplet(c, j, k, l) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, c, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do c = nocc + 1, nactive 
! do k = 1, nocc 
! sum = zero 
! do j = 1, nocc 
! do b = nocc + 1, nactive 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r2p(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,b,c,j,i,k)
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_51_triplet(c, k) = wmo_interm_cc3_51_triplet(c, k) + sum 
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
! wmo_interm_cc3_52_triplet(b, j) = wmo_interm_cc3_52_triplet(b, j) + sum 
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
! wmo_interm_cc3_53_triplet(b, j, i, k) = wmo_interm_cc3_53_triplet(b, j, i, k) + sum 
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
! sum = sum + r2m(vrdav_Rl, a,j,b,i) * r1(vrdav_Rr, a,i)
! end do 
! end do 
! wmo_interm_cc3_54_triplet(b, j) = wmo_interm_cc3_54_triplet(b, j) + sum 
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
! wmo_interm_cc3_55_triplet(b, i, j, k) = wmo_interm_cc3_55_triplet(b, i, j, k) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, c, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do c = nocc + 1, nactive 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do j = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r2m(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, a,b,c,k,j,i)
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_56_triplet(c, k) = wmo_interm_cc3_56_triplet(c, k) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, c, j, k, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do c = nocc + 1, nactive 
! do j = 1, nocc 
! do k = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + r2m(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,b,c,k,l,i)
! end do 
! end do 
! end do 
! wmo_interm_cc3_57_triplet(c, j, k, l) = wmo_interm_cc3_57_triplet(c, j, k, l) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, c, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do c = nocc + 1, nactive 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do j = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r2m(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,b,c,k,j,i)
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_58_triplet(c, k) = wmo_interm_cc3_58_triplet(c, k) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, c, j, k, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do c = nocc + 1, nactive 
! do j = 1, nocc 
! do k = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do b = nocc + 1, nactive 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r2m(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,b,c,k,i,l)
! end do 
! end do 
! end do 
! wmo_interm_cc3_59_triplet(c, j, k, l) = wmo_interm_cc3_59_triplet(c, j, k, l) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, c, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do c = nocc + 1, nactive 
! do k = 1, nocc 
! sum = zero 
! do j = 1, nocc 
! do b = nocc + 1, nactive 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r2m(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,b,c,j,i,k)
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_60_triplet(c, k) = wmo_interm_cc3_60_triplet(c, k) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, c, j, k, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do c = nocc + 1, nactive 
! do j = 1, nocc 
! do k = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + r2m(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, a,b,c,k,l,i)
! end do 
! end do 
! end do 
! wmo_interm_cc3_61_triplet(c, j, k, l) = wmo_interm_cc3_61_triplet(c, j, k, l) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, c, j, k, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do c = nocc + 1, nactive 
! do j = 1, nocc 
! do k = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do b = nocc + 1, nactive 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r2m(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, a,b,c,k,i,l)
! end do 
! end do 
! end do 
! wmo_interm_cc3_62_triplet(c, j, k, l) = wmo_interm_cc3_62_triplet(c, j, k, l) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, c, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do c = nocc + 1, nactive 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do j = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r2m(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,b,c,i,j,k)
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_63_triplet(c, k) = wmo_interm_cc3_63_triplet(c, k) + sum 
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
! wmo_interm_cc3_64_triplet(i, j) = wmo_interm_cc3_64_triplet(i, j) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, c, i, j, k, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do k = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do j = 1, nocc 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + t3(nocc, nactive, a,b,c,i,j,k) * t3(nocc, nactive, a,b,c,l,i,j)
! end do 
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_65_triplet(k, l) = wmo_interm_cc3_65_triplet(k, l) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, c, i, j, k, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do k = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do j = 1, nocc 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + t3(nocc, nactive, a,b,c,k,i,j) * t3(nocc, nactive, a,b,c,l,j,i)
! end do 
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_66_triplet(k, l) = wmo_interm_cc3_66_triplet(k, l) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, c, i, j, k, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do k = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do j = 1, nocc 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + t3(nocc, nactive, a,b,c,i,k,j) * t3(nocc, nactive, a,b,c,l,j,i)
! end do 
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_67_triplet(k, l) = wmo_interm_cc3_67_triplet(k, l) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, c, i, j, k, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do k = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do j = 1, nocc 
! do c = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + t3(nocc, nactive, a,b,c,i,j,k) * t3(nocc, nactive, a,b,c,l,j,i)
! end do 
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_68_triplet(k, l) = wmo_interm_cc3_68_triplet(k, l) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, c, i, j, k, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do k = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do j = 1, nocc 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + t3(nocc, nactive, a,b,c,i,k,j) * t3(nocc, nactive, a,b,c,l,i,j)
! end do 
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_69_triplet(k, l) = wmo_interm_cc3_69_triplet(k, l) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, c, i, j, k, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do k = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do j = 1, nocc 
! do b = nocc + 1, nactive 
! do i = 1, nocc 
! do c = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + t3(nocc, nactive, a,b,c,k,i,j) * t3(nocc, nactive, a,b,c,l,i,j)
! end do 
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_70_triplet(k, l) = wmo_interm_cc3_70_triplet(k, l) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, c, i, j, k, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do k = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do j = 1, nocc 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + t3(nocc, nactive, a,b,c,j,k,i) * t3(nocc, nactive, a,b,c,l,i,j)
! end do 
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_71_triplet(k, l) = wmo_interm_cc3_71_triplet(k, l) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, c, i, j, k, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do k = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do j = 1, nocc 
! do b = nocc + 1, nactive 
! do i = 1, nocc 
! do c = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + t3(nocc, nactive, a,b,c,j,i,k) * t3(nocc, nactive, a,b,c,l,i,j)
! end do 
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_72_triplet(k, l) = wmo_interm_cc3_72_triplet(k, l) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, c, i, j, k, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do k = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do j = 1, nocc 
! do b = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + t3(nocc, nactive, a,b,c,j,i,k) * t3(nocc, nactive, a,b,c,l,j,i)
! end do 
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_73_triplet(k, l) = wmo_interm_cc3_73_triplet(k, l) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, c, i, j, k, l, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do k = 1, nocc 
! do l = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do j = 1, nocc 
! do b = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + t3(nocc, nactive, a,b,c,j,k,i) * t3(nocc, nactive, a,b,c,l,j,i)
! end do 
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_74_triplet(k, l) = wmo_interm_cc3_74_triplet(k, l) + sum 
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
! wmo_interm_cc3_75_triplet(a, b) = wmo_interm_cc3_75_triplet(a, b) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, k, c, d, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do c = nocc + 1, nactive 
! do d = nocc + 1, nactive 
! sum = zero 
! do i = 1, nocc 
! do k = 1, nocc 
! do j = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + t3(nocc, nactive, a,b,c,j,k,i) * t3(nocc, nactive, a,b,d,k,j,i)
! end do 
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_76_triplet(c, d) = wmo_interm_cc3_76_triplet(c, d) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, k, c, d, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do c = nocc + 1, nactive 
! do d = nocc + 1, nactive 
! sum = zero 
! do j = 1, nocc 
! do i = 1, nocc 
! do k = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + t3(nocc, nactive, a,b,c,i,k,j) * t3(nocc, nactive, a,b,d,k,j,i)
! end do 
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_77_triplet(c, d) = wmo_interm_cc3_77_triplet(c, d) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, k, c, d, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do c = nocc + 1, nactive 
! do d = nocc + 1, nactive 
! sum = zero 
! do i = 1, nocc 
! do j = 1, nocc 
! do b = nocc + 1, nactive 
! do k = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + t3(nocc, nactive, a,b,c,j,k,i) * t3(nocc, nactive, a,b,d,j,k,i)
! end do 
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_78_triplet(c, d) = wmo_interm_cc3_78_triplet(c, d) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, k, c, d, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do c = nocc + 1, nactive 
! do d = nocc + 1, nactive 
! sum = zero 
! do j = 1, nocc 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do k = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + t3(nocc, nactive, a,b,c,i,k,j) * t3(nocc, nactive, a,b,d,j,k,i)
! end do 
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_79_triplet(c, d) = wmo_interm_cc3_79_triplet(c, d) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, k, c, d, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do c = nocc + 1, nactive 
! do d = nocc + 1, nactive 
! sum = zero 
! do j = 1, nocc 
! do i = 1, nocc 
! do k = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + t3(nocc, nactive, a,b,c,k,i,j) * t3(nocc, nactive, a,b,d,j,k,i)
! end do 
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_80_triplet(c, d) = wmo_interm_cc3_80_triplet(c, d) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, b, i, j, k, c, d, sum)& 
! !$omp default(shared)
! !$omp do collapse(2)
! do c = nocc + 1, nactive 
! do d = nocc + 1, nactive 
! sum = zero 
! do j = 1, nocc 
! do i = 1, nocc 
! do k = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! sum = sum + t3(nocc, nactive, a,b,c,k,i,j) * t3(nocc, nactive, a,b,d,k,j,i)
! end do 
! end do 
! end do 
! end do 
! end do 
! wmo_interm_cc3_81_triplet(c, d) = wmo_interm_cc3_81_triplet(c, d) + sum 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, b, c, j, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do b = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! do j = 1, nocc 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r1(vrdav_Rl, a,i) * t3(nocc, nactive, a,b,c,j,i,k)
! end do 
! end do 
! wmo_interm_cc3_82_triplet(b, c, j, k) = wmo_interm_cc3_82_triplet(b, c, j, k) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, b, c, j, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do b = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! do j = 1, nocc 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,c,j,i,k)
! end do 
! end do 
! wmo_interm_cc3_83_triplet(b, c, j, k) = wmo_interm_cc3_83_triplet(b, c, j, k) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, b, c, j, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do b = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! do j = 1, nocc 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,c,j,k,i)
! end do 
! end do 
! wmo_interm_cc3_84_triplet(b, c, j, k) = wmo_interm_cc3_84_triplet(b, c, j, k) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, b, c, j, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do b = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! do j = 1, nocc 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r1(vrdav_Rr, a,i) * t3(nocc, nactive, a,b,c,i,j,k)
! end do 
! end do 
! wmo_interm_cc3_85_triplet(b, c, j, k) = wmo_interm_cc3_85_triplet(b, c, j, k) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, b, c, j, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do b = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! do j = 1, nocc 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r1(vrdav_Rl, a,i) * t3(nocc, nactive, a,b,c,j,k,i)
! end do 
! end do 
! wmo_interm_cc3_86_triplet(b, c, j, k) = wmo_interm_cc3_86_triplet(b, c, j, k) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 

! !$omp parallel private(a, i, b, c, j, k, sum)& 
! !$omp default(shared)
! !$omp do collapse(4)
! do b = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! do j = 1, nocc 
! do k = 1, nocc 
! sum = zero 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! sum = sum + r1(vrdav_Rl, a,i) * t3(nocc, nactive, a,b,c,i,j,k)
! end do 
! end do 
! wmo_interm_cc3_87_triplet(b, c, j, k) = wmo_interm_cc3_87_triplet(b, c, j, k) + sum 
! end do 
! end do 
! end do 
! end do 
! !$omp end do nowait 
! !$omp end parallel 



    end subroutine d_overlap_intermediates_triplet_cc3



  function d_overlap_ccsd(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr)
    double precision :: d_overlap_ccsd
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2
    double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: t1
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2
    double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: s1
    double precision, dimension(:), intent(in) :: vrdav_Rl                                                                                                                              
    double precision, dimension(:), intent(in) :: vrdav_Rr 
    integer :: s ,j,i,a,b,k,c,l,d
    double precision, dimension(0:224) :: term
    term = 0.d+0
    do j = 1, nocc 
do i = 1, nocc 
term(0) = term(0) + wmo_interm_0(i, j) * wmo_interm_1(j, i)
term(1) = term(1) + wmo_interm_2(i, j) * wmo_interm_3(i, j)
term(2) = term(2) + wmo_interm_0(i, j) * wmo_interm_27(j, i)
term(3) = term(3) + wmo_interm_0(i, j) * wmo_interm_28(j, i)
term(4) = term(4) + wmo_interm_0(i, j) * wmo_interm_27(j, i)
term(5) = term(5) + wmo_interm_0(i, j) * wmo_interm_28(j, i)
end do 
end do 

term(0) = term(0) * (-2.0d+0) 
term(1) = term(1) * (-2.0d+0) 
term(3) = term(3) * (-2.0d+0) 
term(5) = term(5) * (-2.0d+0) 

do l = 1, nocc 
do j = 1, nocc 
do c = nocc + 1, nactive 
term(6) = term(6) + s1(c,l) * wmo_interm_25(j, l) * wmo_interm_5(c, j)
term(7) = term(7) + s1(c,l) * wmo_interm_25(j, l) * wmo_interm_6(c, j)
term(8) = term(8) + s1(c,l) * wmo_interm_26(j, l) * wmo_interm_5(c, j)
term(9) = term(9) + s1(c,l) * wmo_interm_26(j, l) * wmo_interm_6(c, j)
end do 
end do 
end do 

term(7) = term(7) * (-0.5d+0) 
term(8) = term(8) * (-2.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(10) = term(10) + wmo_interm_4(a, i) * wmo_interm_5(a, i)
term(11) = term(11) + wmo_interm_4(a, i) * wmo_interm_6(a, i)
term(12) = term(12) + wmo_interm_18(a, i) * wmo_interm_19(a, i)
term(13) = term(13) + wmo_interm_18(a, i) * wmo_interm_20(a, i)
term(14) = term(14) + r1(vrdav_Rl, a,i) * r1(vrdav_Rr, a,i)
end do 
end do 

term(10) = term(10) * (-4.0d+0) 
term(11) = term(11) * 2.0d+0 
term(12) = term(12) * (-2.0d+0) 
term(13) = term(13) * 4.0d+0 
term(14) = term(14) * 2.0d+0 

do j = 1, nocc 
do a = nocc + 1, nactive 
term(15) = term(15) + wmo_interm_7(a, j) * wmo_interm_5(a, j)
term(16) = term(16) + wmo_interm_7(a, j) * wmo_interm_6(a, j)
term(17) = term(17) + wmo_interm_23(a, j) * wmo_interm_19(a, j)
term(18) = term(18) + wmo_interm_23(a, j) * wmo_interm_20(a, j)
end do 
end do 

term(15) = term(15) * 2.0d+0 
term(16) = -term(16) 
term(18) = term(18) * (-2.0d+0) 

do l = 1, nocc 
do i = 1, nocc 
do c = nocc + 1, nactive 
term(19) = term(19) + s1(c,l) * wmo_interm_26(i, l) * wmo_interm_5(c, i)
term(20) = term(20) + s1(c,l) * wmo_interm_26(i, l) * wmo_interm_6(c, i)
term(21) = term(21) + s1(c,l) * wmo_interm_25(i, l) * wmo_interm_5(c, i)
term(22) = term(22) + s1(c,l) * wmo_interm_25(i, l) * wmo_interm_6(c, i)
end do 
end do 
end do 

term(19) = term(19) * (-2.0d+0) 
term(22) = term(22) * (-0.5d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
term(23) = term(23) + wmo_interm_7(b, i) * wmo_interm_5(b, i)
term(24) = term(24) + wmo_interm_7(b, i) * wmo_interm_6(b, i)
term(25) = term(25) + wmo_interm_23(b, i) * wmo_interm_19(b, i)
term(26) = term(26) + wmo_interm_23(b, i) * wmo_interm_20(b, i)
end do 
end do 

term(23) = term(23) * 2.0d+0 
term(24) = -term(24) 
term(26) = term(26) * (-2.0d+0) 

do j = 1, nocc 
do b = nocc + 1, nactive 
term(27) = term(27) + wmo_interm_4(b, j) * wmo_interm_5(b, j)
term(28) = term(28) + wmo_interm_4(b, j) * wmo_interm_6(b, j)
term(29) = term(29) + wmo_interm_18(b, j) * wmo_interm_19(b, j)
term(30) = term(30) + wmo_interm_18(b, j) * wmo_interm_20(b, j)
end do 
end do 

term(27) = term(27) * (-4.0d+0) 
term(28) = term(28) * 2.0d+0 
term(29) = term(29) * (-2.0d+0) 
term(30) = term(30) * 4.0d+0 

do l = 1, nocc 
do i = 1, nocc 
do d = nocc + 1, nactive 
term(31) = term(31) + s1(d,l) * wmo_interm_26(i, l) * wmo_interm_6(d, i)
term(32) = term(32) + s1(d,l) * wmo_interm_26(i, l) * wmo_interm_5(d, i)
term(33) = term(33) + s1(d,l) * wmo_interm_25(i, l) * wmo_interm_6(d, i)
term(34) = term(34) + s1(d,l) * wmo_interm_25(i, l) * wmo_interm_5(d, i)
end do 
end do 
end do 

term(32) = term(32) * (-2.0d+0) 
term(33) = term(33) * (-0.5d+0) 

do k = 1, nocc 
do j = 1, nocc 
term(35) = term(35) + wmo_interm_8(j, k) * wmo_interm_1(k, j)
term(36) = term(36) + wmo_interm_9(j, k) * wmo_interm_1(k, j)
term(37) = term(37) + wmo_interm_25(j, k) * wmo_interm_3(j, k)
term(38) = term(38) + wmo_interm_26(j, k) * wmo_interm_3(j, k)
term(39) = term(39) + wmo_interm_8(j, k) * wmo_interm_27(k, j)
term(40) = term(40) + wmo_interm_9(j, k) * wmo_interm_27(k, j)
term(41) = term(41) + wmo_interm_8(j, k) * wmo_interm_28(k, j)
term(42) = term(42) + wmo_interm_9(j, k) * wmo_interm_28(k, j)
term(43) = term(43) + wmo_interm_8(j, k) * wmo_interm_27(k, j)
term(44) = term(44) + wmo_interm_9(j, k) * wmo_interm_27(k, j)
term(45) = term(45) + wmo_interm_8(j, k) * wmo_interm_28(k, j)
term(46) = term(46) + wmo_interm_9(j, k) * wmo_interm_28(k, j)
end do 
end do 

term(36) = term(36) * (-2.0d+0) 
term(38) = term(38) * (-2.0d+0) 
term(39) = term(39) * (-0.5d+0) 
term(42) = term(42) * (-2.0d+0) 
term(43) = term(43) * (-0.5d+0) 
term(46) = term(46) * (-2.0d+0) 

do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(47) = term(47) + wmo_interm_10(b, c) * wmo_interm_11(c, b)
term(48) = term(48) + wmo_interm_12(b, c) * wmo_interm_11(c, b)
term(49) = term(49) + wmo_interm_21(b, c) * wmo_interm_22(b, c)
term(50) = term(50) + wmo_interm_24(b, c) * wmo_interm_22(b, c)
term(51) = term(51) + wmo_interm_10(b, c) * wmo_interm_33(c, b)
term(52) = term(52) + wmo_interm_12(b, c) * wmo_interm_33(c, b)
term(53) = term(53) + wmo_interm_10(b, c) * wmo_interm_34(c, b)
term(54) = term(54) + wmo_interm_12(b, c) * wmo_interm_34(c, b)
end do 
end do 

term(48) = term(48) * (-2.0d+0) 
term(50) = term(50) * (-2.0d+0) 
term(51) = term(51) * 2.0d+0 
term(52) = term(52) * (-4.0d+0) 
term(53) = -term(53) 
term(54) = term(54) * 2.0d+0 

do l = 1, nocc 
do j = 1, nocc 
do d = nocc + 1, nactive 
term(55) = term(55) + s1(d,l) * wmo_interm_25(j, l) * wmo_interm_6(d, j)
term(56) = term(56) + s1(d,l) * wmo_interm_25(j, l) * wmo_interm_5(d, j)
term(57) = term(57) + s1(d,l) * wmo_interm_26(j, l) * wmo_interm_6(d, j)
term(58) = term(58) + s1(d,l) * wmo_interm_26(j, l) * wmo_interm_5(d, j)
end do 
end do 
end do 

term(55) = term(55) * (-0.5d+0) 
term(58) = term(58) * (-2.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
term(59) = term(59) + wmo_interm_8(i, k) * wmo_interm_1(k, i)
term(60) = term(60) + wmo_interm_9(i, k) * wmo_interm_1(k, i)
term(61) = term(61) + wmo_interm_2(i, k) * wmo_interm_16(i, k)
term(62) = term(62) + wmo_interm_2(i, k) * wmo_interm_17(i, k)
term(63) = term(63) + wmo_interm_2(i, k) * wmo_interm_16(i, k)
term(64) = term(64) + wmo_interm_2(i, k) * wmo_interm_17(i, k)
term(65) = term(65) + wmo_interm_26(i, k) * wmo_interm_3(i, k)
term(66) = term(66) + wmo_interm_25(i, k) * wmo_interm_3(i, k)
term(67) = term(67) + wmo_interm_8(i, k) * wmo_interm_27(k, i)
term(68) = term(68) + wmo_interm_9(i, k) * wmo_interm_27(k, i)
term(69) = term(69) + wmo_interm_8(i, k) * wmo_interm_28(k, i)
term(70) = term(70) + wmo_interm_9(i, k) * wmo_interm_28(k, i)
term(71) = term(71) + wmo_interm_8(i, k) * wmo_interm_28(k, i)
term(72) = term(72) + wmo_interm_9(i, k) * wmo_interm_28(k, i)
term(73) = term(73) + wmo_interm_8(i, k) * wmo_interm_27(k, i)
term(74) = term(74) + wmo_interm_9(i, k) * wmo_interm_27(k, i)
end do 
end do 

term(60) = term(60) * (-2.0d+0) 
term(62) = term(62) * (-2.0d+0) 
term(64) = term(64) * (-2.0d+0) 
term(65) = term(65) * (-2.0d+0) 
term(67) = term(67) * (-0.5d+0) 
term(70) = term(70) * (-2.0d+0) 
term(72) = term(72) * (-2.0d+0) 
term(73) = term(73) * (-0.5d+0) 

do c = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(75) = term(75) + wmo_interm_10(a, c) * wmo_interm_11(c, a)
term(76) = term(76) + wmo_interm_12(a, c) * wmo_interm_11(c, a)
term(77) = term(77) + wmo_interm_13(a, c) * wmo_interm_14(a, c)
term(78) = term(78) + wmo_interm_13(a, c) * wmo_interm_15(a, c)
term(79) = term(79) + wmo_interm_24(a, c) * wmo_interm_22(a, c)
term(80) = term(80) + wmo_interm_21(a, c) * wmo_interm_22(a, c)
term(81) = term(81) + wmo_interm_10(a, c) * wmo_interm_33(c, a)
term(82) = term(82) + wmo_interm_12(a, c) * wmo_interm_33(c, a)
term(83) = term(83) + wmo_interm_10(a, c) * wmo_interm_34(c, a)
term(84) = term(84) + wmo_interm_12(a, c) * wmo_interm_34(c, a)
end do 
end do 

term(76) = term(76) * (-2.0d+0) 
term(77) = term(77) * 2.0d+0 
term(78) = term(78) * (-4.0d+0) 
term(79) = term(79) * (-2.0d+0) 
term(81) = term(81) * 2.0d+0 
term(82) = term(82) * (-4.0d+0) 
term(83) = -term(83) 
term(84) = term(84) * 2.0d+0 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(85) = term(85) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,j,b,i)
end do 
end do 
end do 
end do 

term(85) = -term(85) 

do j = 1, nocc 
do d = nocc + 1, nactive 
do l = 1, nocc 
do b = nocc + 1, nactive 
term(86) = term(86) + s2(b,d,l,j) * wmo_interm_29(d, l) * wmo_interm_5(b, j)
term(87) = term(87) + s2(b,d,l,j) * wmo_interm_35(d, l) * wmo_interm_5(b, j)
end do 
end do 
end do 
end do 

term(86) = term(86) * 2.0d+0 
term(87) = term(87) * (-4.0d+0) 

do l = 1, nocc 
do d = nocc + 1, nactive 
do j = 1, nocc 
do b = nocc + 1, nactive 
term(88) = term(88) + s2(b,d,j,l) * wmo_interm_29(d, l) * wmo_interm_5(b, j)
term(89) = term(89) + s2(b,d,j,l) * wmo_interm_35(d, l) * wmo_interm_5(b, j)
end do 
end do 
end do 
end do 

term(88) = term(88) * (-4.0d+0) 
term(89) = term(89) * 8.0d+0 

do k = 1, nocc 
do d = nocc + 1, nactive 
do l = 1, nocc 
do b = nocc + 1, nactive 
term(90) = term(90) + s2(b,d,l,k) * wmo_interm_29(d, l) * wmo_interm_6(b, k)
term(91) = term(91) + s2(b,d,l,k) * wmo_interm_35(d, l) * wmo_interm_6(b, k)
end do 
end do 
end do 
end do 

term(90) = -term(90) 
term(91) = term(91) * 2.0d+0 

do l = 1, nocc 
do d = nocc + 1, nactive 
do k = 1, nocc 
do b = nocc + 1, nactive 
term(92) = term(92) + s2(b,d,k,l) * wmo_interm_29(d, l) * wmo_interm_6(b, k)
term(93) = term(93) + s2(b,d,k,l) * wmo_interm_35(d, l) * wmo_interm_6(b, k)
end do 
end do 
end do 
end do 

term(92) = term(92) * 2.0d+0 
term(93) = term(93) * (-4.0d+0) 

do j = 1, nocc 
do d = nocc + 1, nactive 
do l = 1, nocc 
do c = nocc + 1, nactive 
term(94) = term(94) + s2(c,d,l,j) * wmo_interm_29(d, l) * wmo_interm_6(c, j)
term(95) = term(95) + s2(c,d,l,j) * wmo_interm_35(d, l) * wmo_interm_6(c, j)
end do 
end do 
end do 
end do 

term(94) = -term(94) 
term(95) = term(95) * 2.0d+0 

do k = 1, nocc 
do d = nocc + 1, nactive 
do l = 1, nocc 
do c = nocc + 1, nactive 
term(96) = term(96) + s2(c,d,l,k) * wmo_interm_29(d, l) * wmo_interm_5(c, k)
term(97) = term(97) + s2(c,d,l,k) * wmo_interm_35(d, l) * wmo_interm_5(c, k)
end do 
end do 
end do 
end do 

term(96) = term(96) * 2.0d+0 
term(97) = term(97) * (-4.0d+0) 

do l = 1, nocc 
do d = nocc + 1, nactive 
do j = 1, nocc 
do c = nocc + 1, nactive 
term(98) = term(98) + s2(c,d,j,l) * wmo_interm_29(d, l) * wmo_interm_6(c, j)
term(99) = term(99) + s2(c,d,j,l) * wmo_interm_35(d, l) * wmo_interm_6(c, j)
end do 
end do 
end do 
end do 

term(98) = term(98) * 2.0d+0 
term(99) = term(99) * (-4.0d+0) 

do l = 1, nocc 
do d = nocc + 1, nactive 
do k = 1, nocc 
do c = nocc + 1, nactive 
term(100) = term(100) + s2(c,d,k,l) * wmo_interm_29(d, l) * wmo_interm_5(c, k)
term(101) = term(101) + s2(c,d,k,l) * wmo_interm_35(d, l) * wmo_interm_5(c, k)
end do 
end do 
end do 
end do 

term(100) = term(100) * (-4.0d+0) 
term(101) = term(101) * 8.0d+0 

do a = nocc + 1, nactive 
do i = 1, nocc 
do b = nocc + 1, nactive 
term(102) = term(102) + r1(vrdav_Rl, a,i) * wmo_interm_5(b, i) * wmo_interm_33(b, a)
term(103) = term(103) + r1(vrdav_Rl, a,i) * wmo_interm_5(b, i) * wmo_interm_34(b, a)
term(104) = term(104) + r1(vrdav_Rl, a,i) * wmo_interm_6(b, i) * wmo_interm_33(b, a)
term(105) = term(105) + r1(vrdav_Rl, a,i) * wmo_interm_6(b, i) * wmo_interm_34(b, a)
end do 
end do 
end do 

term(102) = term(102) * (-4.0d+0) 
term(103) = term(103) * 2.0d+0 
term(104) = term(104) * 2.0d+0 
term(105) = -term(105) 

do k = 1, nocc 
do c = nocc + 1, nactive 
term(106) = term(106) + wmo_interm_29(c, k) * wmo_interm_30(c, k)
term(107) = term(107) + wmo_interm_29(c, k) * wmo_interm_31(c, k)
term(108) = term(108) + wmo_interm_35(c, k) * wmo_interm_31(c, k)
term(109) = term(109) + wmo_interm_35(c, k) * wmo_interm_30(c, k)
end do 
end do 

term(106) = term(106) * 2.0d+0 
term(107) = term(107) * (-4.0d+0) 
term(108) = term(108) * 8.0d+0 
term(109) = term(109) * (-4.0d+0) 

do a = nocc + 1, nactive 
do i = 1, nocc 
do c = nocc + 1, nactive 
term(110) = term(110) + r1(vrdav_Rl, a,i) * wmo_interm_6(c, i) * wmo_interm_33(c, a)
term(111) = term(111) + r1(vrdav_Rl, a,i) * wmo_interm_5(c, i) * wmo_interm_33(c, a)
term(112) = term(112) + r1(vrdav_Rl, a,i) * wmo_interm_6(c, i) * wmo_interm_34(c, a)
term(113) = term(113) + r1(vrdav_Rl, a,i) * wmo_interm_5(c, i) * wmo_interm_34(c, a)
end do 
end do 
end do 

term(110) = term(110) * 2.0d+0 
term(111) = term(111) * (-4.0d+0) 
term(112) = -term(112) 
term(113) = term(113) * 2.0d+0 

do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(114) = term(114) + wmo_interm_32(a, b) * wmo_interm_33(b, a)
term(115) = term(115) + wmo_interm_32(a, b) * wmo_interm_34(b, a)
end do 
end do 

term(114) = term(114) * (-4.0d+0) 
term(115) = term(115) * 2.0d+0 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(116) = term(116) + r1(vrdav_Rl, a,i) * wmo_interm_5(a, j) * wmo_interm_1(j, i)
term(117) = term(117) + r1(vrdav_Rl, a,i) * wmo_interm_5(a, j) * wmo_interm_27(j, i)
term(118) = term(118) + r1(vrdav_Rl, a,i) * wmo_interm_5(a, j) * wmo_interm_28(j, i)
term(119) = term(119) + r1(vrdav_Rl, a,i) * wmo_interm_5(a, j) * wmo_interm_27(j, i)
term(120) = term(120) + r1(vrdav_Rl, a,i) * wmo_interm_5(a, j) * wmo_interm_28(j, i)
end do 
end do 
end do 

term(116) = term(116) * (-4.0d+0) 
term(117) = term(117) * 2.0d+0 
term(118) = term(118) * (-4.0d+0) 
term(119) = term(119) * 2.0d+0 
term(120) = term(120) * (-4.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(121) = term(121) + r1(vrdav_Rl, a,i) * wmo_interm_6(a, k) * wmo_interm_1(k, i)
term(122) = term(122) + r1(vrdav_Rl, a,i) * wmo_interm_6(a, k) * wmo_interm_27(k, i)
term(123) = term(123) + r1(vrdav_Rl, a,i) * wmo_interm_6(a, k) * wmo_interm_28(k, i)
term(124) = term(124) + r1(vrdav_Rl, a,i) * wmo_interm_6(a, k) * wmo_interm_27(k, i)
term(125) = term(125) + r1(vrdav_Rl, a,i) * wmo_interm_6(a, k) * wmo_interm_28(k, i)
end do 
end do 
end do 

term(121) = term(121) * 2.0d+0 
term(122) = -term(122) 
term(123) = term(123) * 2.0d+0 
term(124) = -term(124) 
term(125) = term(125) * 2.0d+0 

do k = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
term(126) = term(126) + s1(b,k) * wmo_interm_2(i, k) * wmo_interm_5(b, i)
term(127) = term(127) + s1(b,k) * wmo_interm_2(i, k) * wmo_interm_6(b, i)
end do 
end do 
end do 

term(126) = term(126) * (-2.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do c = nocc + 1, nactive 
term(128) = term(128) + s1(c,k) * wmo_interm_2(i, k) * wmo_interm_6(c, i)
term(129) = term(129) + s1(c,k) * wmo_interm_2(i, k) * wmo_interm_5(c, i)
end do 
end do 
end do 

term(129) = term(129) * (-2.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(130) = term(130) + wmo_interm_40(b, c, j, k) * wmo_interm_41(c, b, k, j)
term(131) = term(131) + wmo_interm_42(b, c, j, k) * wmo_interm_41(c, b, k, j)
term(132) = term(132) + wmo_interm_40(b, c, j, k) * wmo_interm_43(c, b, k, j)
term(133) = term(133) + wmo_interm_42(b, c, j, k) * wmo_interm_43(c, b, k, j)
term(134) = term(134) + wmo_interm_40(b, c, j, k) * wmo_interm_44(c, b, k, j)
term(135) = term(135) + wmo_interm_42(b, c, j, k) * wmo_interm_44(c, b, k, j)
term(136) = term(136) + wmo_interm_40(b, c, j, k) * wmo_interm_45(c, b, k, j)
term(137) = term(137) + wmo_interm_42(b, c, j, k) * wmo_interm_45(c, b, k, j)
end do 
end do 
end do 
end do 

term(130) = -term(130) 
term(131) = term(131) * 2.0d+0 
term(132) = term(132) * 2.0d+0 
term(133) = term(133) * (-4.0d+0) 
term(134) = term(134) * (-4.0d+0) 
term(135) = term(135) * 8.0d+0 
term(136) = term(136) * 2.0d+0 
term(137) = term(137) * (-4.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do c = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(138) = term(138) + wmo_interm_40(a, c, i, k) * wmo_interm_41(c, a, k, i)
term(139) = term(139) + wmo_interm_42(a, c, i, k) * wmo_interm_41(c, a, k, i)
term(140) = term(140) + wmo_interm_40(a, c, i, k) * wmo_interm_43(c, a, k, i)
term(141) = term(141) + wmo_interm_42(a, c, i, k) * wmo_interm_43(c, a, k, i)
term(142) = term(142) + wmo_interm_40(a, c, i, k) * wmo_interm_44(c, a, k, i)
term(143) = term(143) + wmo_interm_42(a, c, i, k) * wmo_interm_44(c, a, k, i)
term(144) = term(144) + wmo_interm_40(a, c, i, k) * wmo_interm_45(c, a, k, i)
term(145) = term(145) + wmo_interm_42(a, c, i, k) * wmo_interm_45(c, a, k, i)
end do 
end do 
end do 
end do 

term(138) = -term(138) 
term(139) = term(139) * 2.0d+0 
term(140) = term(140) * 2.0d+0 
term(141) = term(141) * (-4.0d+0) 
term(142) = term(142) * (-4.0d+0) 
term(143) = term(143) * 8.0d+0 
term(144) = term(144) * 2.0d+0 
term(145) = term(145) * (-4.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(146) = term(146) + wmo_interm_46(b, c, i, k) * wmo_interm_41(c, b, k, i)
term(147) = term(147) + wmo_interm_47(b, c, i, k) * wmo_interm_41(c, b, k, i)
term(148) = term(148) + wmo_interm_47(b, c, i, k) * wmo_interm_43(c, b, k, i)
term(149) = term(149) + wmo_interm_46(b, c, i, k) * wmo_interm_43(c, b, k, i)
term(150) = term(150) + wmo_interm_47(b, c, i, k) * wmo_interm_44(c, b, k, i)
term(151) = term(151) + wmo_interm_46(b, c, i, k) * wmo_interm_44(c, b, k, i)
term(152) = term(152) + wmo_interm_47(b, c, i, k) * wmo_interm_45(c, b, k, i)
term(153) = term(153) + wmo_interm_46(b, c, i, k) * wmo_interm_45(c, b, k, i)
end do 
end do 
end do 
end do 

term(146) = -term(146) 
term(147) = term(147) * 2.0d+0 
term(148) = -term(148) 
term(149) = term(149) * 2.0d+0 
term(150) = term(150) * 2.0d+0 
term(151) = term(151) * (-4.0d+0) 
term(152) = -term(152) 
term(153) = term(153) * 2.0d+0 

do l = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
term(154) = term(154) + wmo_interm_36(i, j, k, l) * wmo_interm_37(l, k, i, j)
term(155) = term(155) + wmo_interm_36(i, j, k, l) * wmo_interm_37(k, l, i, j)
end do 
end do 
end do 
end do 

term(154) = term(154) * (-0.5d+0) 

do k = 1, nocc 
do j = 1, nocc 
do c = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(156) = term(156) + wmo_interm_46(a, c, j, k) * wmo_interm_41(c, a, k, j)
term(157) = term(157) + wmo_interm_47(a, c, j, k) * wmo_interm_41(c, a, k, j)
term(158) = term(158) + wmo_interm_47(a, c, j, k) * wmo_interm_43(c, a, k, j)
term(159) = term(159) + wmo_interm_46(a, c, j, k) * wmo_interm_43(c, a, k, j)
term(160) = term(160) + wmo_interm_47(a, c, j, k) * wmo_interm_44(c, a, k, j)
term(161) = term(161) + wmo_interm_46(a, c, j, k) * wmo_interm_44(c, a, k, j)
term(162) = term(162) + wmo_interm_47(a, c, j, k) * wmo_interm_45(c, a, k, j)
term(163) = term(163) + wmo_interm_46(a, c, j, k) * wmo_interm_45(c, a, k, j)
end do 
end do 
end do 
end do 

term(156) = -term(156) 
term(157) = term(157) * 2.0d+0 
term(158) = -term(158) 
term(159) = term(159) * 2.0d+0 
term(160) = term(160) * 2.0d+0 
term(161) = term(161) * (-4.0d+0) 
term(162) = -term(162) 
term(163) = term(163) * 2.0d+0 

do d = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(164) = term(164) + wmo_interm_21(b, d) * wmo_interm_14(b, d)
term(165) = term(165) + wmo_interm_21(b, d) * wmo_interm_15(b, d)
term(166) = term(166) + wmo_interm_24(b, d) * wmo_interm_14(b, d)
term(167) = term(167) + wmo_interm_24(b, d) * wmo_interm_15(b, d)
end do 
end do 

term(164) = -term(164) 
term(165) = term(165) * 2.0d+0 
term(166) = term(166) * 2.0d+0 
term(167) = term(167) * (-4.0d+0) 

do d = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(168) = term(168) + wmo_interm_24(a, d) * wmo_interm_14(a, d)
term(169) = term(169) + wmo_interm_24(a, d) * wmo_interm_15(a, d)
term(170) = term(170) + wmo_interm_21(a, d) * wmo_interm_14(a, d)
term(171) = term(171) + wmo_interm_21(a, d) * wmo_interm_15(a, d)
end do 
end do 

term(168) = term(168) * 2.0d+0 
term(169) = term(169) * (-4.0d+0) 
term(170) = -term(170) 
term(171) = term(171) * 2.0d+0 

do l = 1, nocc 
do j = 1, nocc 
term(172) = term(172) + wmo_interm_25(j, l) * wmo_interm_16(j, l)
term(173) = term(173) + wmo_interm_25(j, l) * wmo_interm_17(j, l)
term(174) = term(174) + wmo_interm_25(j, l) * wmo_interm_16(j, l)
term(175) = term(175) + wmo_interm_25(j, l) * wmo_interm_17(j, l)
term(176) = term(176) + wmo_interm_26(j, l) * wmo_interm_16(j, l)
term(177) = term(177) + wmo_interm_26(j, l) * wmo_interm_17(j, l)
term(178) = term(178) + wmo_interm_26(j, l) * wmo_interm_16(j, l)
term(179) = term(179) + wmo_interm_26(j, l) * wmo_interm_17(j, l)
end do 
end do 

term(172) = term(172) * (-0.5d+0) 
term(174) = term(174) * (-0.5d+0) 
term(177) = term(177) * (-2.0d+0) 
term(179) = term(179) * (-2.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(180) = term(180) + wmo_interm_38(i, j, l, k) * wmo_interm_39(i, j, k, l)
term(181) = term(181) + wmo_interm_38(i, j, l, k) * wmo_interm_39(j, i, k, l)
term(182) = term(182) + wmo_interm_38(i, j, k, l) * wmo_interm_39(j, i, k, l)
term(183) = term(183) + wmo_interm_38(i, j, k, l) * wmo_interm_39(i, j, k, l)
end do 
end do 
end do 
end do 

term(180) = term(180) * (-0.5d+0) 
term(182) = term(182) * (-0.5d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(184) = term(184) + r1(vrdav_Rl, a,i) * r2(vrdav_Rr, a,j,b,i) * s1(b,j)
end do 
end do 
end do 
end do 

term(184) = term(184) * (-2.0d+0) 

do l = 1, nocc 
do i = 1, nocc 
term(185) = term(185) + wmo_interm_26(i, l) * wmo_interm_16(i, l)
term(186) = term(186) + wmo_interm_26(i, l) * wmo_interm_17(i, l)
term(187) = term(187) + wmo_interm_26(i, l) * wmo_interm_16(i, l)
term(188) = term(188) + wmo_interm_26(i, l) * wmo_interm_17(i, l)
term(189) = term(189) + wmo_interm_25(i, l) * wmo_interm_16(i, l)
term(190) = term(190) + wmo_interm_25(i, l) * wmo_interm_17(i, l)
term(191) = term(191) + wmo_interm_25(i, l) * wmo_interm_16(i, l)
term(192) = term(192) + wmo_interm_25(i, l) * wmo_interm_17(i, l)
end do 
end do 

term(186) = term(186) * (-2.0d+0) 
term(188) = term(188) * (-2.0d+0) 
term(189) = term(189) * (-0.5d+0) 
term(191) = term(191) * (-0.5d+0) 

do l = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
term(193) = term(193) + wmo_interm_36(i, j, k, l) * wmo_interm_37(k, l, j, i)
term(194) = term(194) + wmo_interm_36(i, j, k, l) * wmo_interm_37(l, k, j, i)
end do 
end do 
end do 
end do 

term(193) = term(193) * (-0.5d+0) 

do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(195) = term(195) + r2(vrdav_Rl, a,i,b,j) * wmo_interm_5(a, i) * wmo_interm_19(b, j)
term(196) = term(196) + r2(vrdav_Rl, a,i,b,j) * wmo_interm_6(a, i) * wmo_interm_19(b, j)
term(197) = term(197) + r2(vrdav_Rl, a,i,b,j) * wmo_interm_5(a, i) * wmo_interm_20(b, j)
term(198) = term(198) + r2(vrdav_Rl, a,i,b,j) * wmo_interm_6(a, i) * wmo_interm_20(b, j)
term(199) = term(199) + r2(vrdav_Rl, a,i,b,j) * wmo_interm_5(b, j) * wmo_interm_19(a, i)
term(200) = term(200) + r2(vrdav_Rl, a,i,b,j) * wmo_interm_5(a, j) * wmo_interm_19(b, i)
term(201) = term(201) + r2(vrdav_Rl, a,i,b,j) * wmo_interm_6(b, j) * wmo_interm_19(a, i)
term(202) = term(202) + r2(vrdav_Rl, a,i,b,j) * wmo_interm_6(a, j) * wmo_interm_19(b, i)
term(203) = term(203) + r2(vrdav_Rl, a,i,b,j) * wmo_interm_5(a, j) * wmo_interm_20(b, i)
term(204) = term(204) + r2(vrdav_Rl, a,i,b,j) * wmo_interm_6(a, j) * wmo_interm_20(b, i)
term(205) = term(205) + r2(vrdav_Rl, a,i,b,j) * wmo_interm_5(b, i) * wmo_interm_19(a, j)
term(206) = term(206) + r2(vrdav_Rl, a,i,b,j) * wmo_interm_6(b, i) * wmo_interm_19(a, j)
term(207) = term(207) + r2(vrdav_Rl, a,i,b,j) * wmo_interm_5(b, i) * wmo_interm_20(a, j)
term(208) = term(208) + r2(vrdav_Rl, a,i,b,j) * wmo_interm_5(b, j) * wmo_interm_20(a, i)
term(209) = term(209) + r2(vrdav_Rl, a,i,b,j) * wmo_interm_6(b, i) * wmo_interm_20(a, j)
term(210) = term(210) + r2(vrdav_Rl, a,i,b,j) * wmo_interm_6(b, j) * wmo_interm_20(a, i)
term(211) = term(211) + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,i,b,j)
term(212) = term(212) + r1(vrdav_Rl, a,i) * r2(vrdav_Rr, a,i,b,j) * s1(b,j)
term(213) = term(213) + r2(vrdav_Rl, a,i,b,j) * r1(vrdav_Rr, a,i) * t1(b,j)
term(214) = term(214) + r2(vrdav_Rl, a,i,b,j) * r1(vrdav_Rr, a,j) * t1(b,i)
term(215) = term(215) + r2(vrdav_Rl, a,i,b,j) * r1(vrdav_Rr, b,i) * t1(a,j)
term(216) = term(216) + r2(vrdav_Rl, a,i,b,j) * r1(vrdav_Rr, b,j) * t1(a,i)
end do 
end do 
end do 
end do 

term(195) = term(195) * (-4.0d+0) 
term(196) = term(196) * 2.0d+0 
term(197) = term(197) * 8.0d+0 
term(198) = term(198) * (-4.0d+0) 
term(199) = term(199) * (-4.0d+0) 
term(200) = term(200) * 2.0d+0 
term(201) = term(201) * 2.0d+0 
term(202) = -term(202) 
term(203) = term(203) * (-4.0d+0) 
term(204) = term(204) * 2.0d+0 
term(205) = term(205) * 2.0d+0 
term(206) = -term(206) 
term(207) = term(207) * (-4.0d+0) 
term(208) = term(208) * 8.0d+0 
term(209) = term(209) * 2.0d+0 
term(210) = term(210) * (-4.0d+0) 
term(211) = term(211) * 2.0d+0 
term(212) = term(212) * 4.0d+0 
term(213) = term(213) * (-2.0d+0) 
term(216) = term(216) * (-2.0d+0) 

do d = nocc + 1, nactive 
do k = 1, nocc 
do b = nocc + 1, nactive 
term(217) = term(217) + s1(d,k) * wmo_interm_21(b, d) * wmo_interm_5(b, k)
term(218) = term(218) + s1(d,k) * wmo_interm_24(b, d) * wmo_interm_5(b, k)
end do 
end do 
end do 

term(217) = term(217) * 2.0d+0 
term(218) = term(218) * (-4.0d+0) 

do d = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(219) = term(219) + s1(d,k) * wmo_interm_24(a, d) * wmo_interm_5(a, k)
term(220) = term(220) + s1(d,k) * wmo_interm_21(a, d) * wmo_interm_5(a, k)
end do 
end do 
end do 

term(219) = term(219) * (-4.0d+0) 
term(220) = term(220) * 2.0d+0 

do d = nocc + 1, nactive 
do l = 1, nocc 
do b = nocc + 1, nactive 
term(221) = term(221) + s1(d,l) * wmo_interm_21(b, d) * wmo_interm_6(b, l)
term(222) = term(222) + s1(d,l) * wmo_interm_24(b, d) * wmo_interm_6(b, l)
end do 
end do 
end do 

term(221) = -term(221) 
term(222) = term(222) * 2.0d+0 

do d = nocc + 1, nactive 
do l = 1, nocc 
do a = nocc + 1, nactive 
term(223) = term(223) + s1(d,l) * wmo_interm_24(a, d) * wmo_interm_6(a, l)
term(224) = term(224) + s1(d,l) * wmo_interm_21(a, d) * wmo_interm_6(a, l)
end do 
end do 
end do 

term(223) = term(223) * 2.0d+0 
term(224) = -term(224) 


    d_overlap_ccsd = 0.d+0
    do s = 0, 224
    d_overlap_ccsd = d_overlap_ccsd + term(s)
    end do                                                                                                  
    end function d_overlap_ccsd


    
    function d_overlap_cc3(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr)
    double precision :: d_overlap_cc3
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2
    double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: t1
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2
    double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: s1
    double precision, dimension(:), intent(in) :: vrdav_Rl                                                                                                                              
    double precision, dimension(:), intent(in) :: vrdav_Rr 
    integer :: s ,l,d,j,b,i,a,k,c
    double precision, dimension(0:119) :: term
    term = 0.d+0
    do l = 1, nocc 
do d = nocc + 1, nactive 
term(0) = term(0) + wmo_interm_cc3_0(d, l) * wmo_interm_cc3_1(d, l)
term(1) = term(1) + wmo_interm_cc3_0(d, l) * wmo_interm_cc3_2(d, l)
term(2) = term(2) + wmo_interm_cc3_0(d, l) * wmo_interm_cc3_3(d, l)
term(3) = term(3) + wmo_interm_cc3_0(d, l) * wmo_interm_cc3_4(d, l)
term(4) = term(4) + wmo_interm_cc3_0(d, l) * wmo_interm_cc3_5(d, l)
term(5) = term(5) + wmo_interm_cc3_0(d, l) * wmo_interm_cc3_6(d, l)
term(6) = term(6) + wmo_interm_cc3_7(d, l) * wmo_interm_cc3_5(d, l)
term(7) = term(7) + wmo_interm_cc3_7(d, l) * wmo_interm_cc3_6(d, l)
term(8) = term(8) + wmo_interm_cc3_7(d, l) * wmo_interm_cc3_3(d, l)
term(9) = term(9) + wmo_interm_cc3_7(d, l) * wmo_interm_cc3_4(d, l)
term(10) = term(10) + wmo_interm_cc3_7(d, l) * wmo_interm_cc3_1(d, l)
term(11) = term(11) + wmo_interm_cc3_7(d, l) * wmo_interm_cc3_2(d, l)
term(12) = term(12) + wmo_interm_cc3_14(d, l) * wmo_interm_cc3_15(d, l)
term(13) = term(13) + wmo_interm_cc3_16(d, l) * wmo_interm_cc3_15(d, l)
term(14) = term(14) + wmo_interm_cc3_14(d, l) * wmo_interm_cc3_17(d, l)
term(15) = term(15) + wmo_interm_cc3_16(d, l) * wmo_interm_cc3_17(d, l)
term(16) = term(16) + wmo_interm_cc3_14(d, l) * wmo_interm_cc3_15(d, l)
term(17) = term(17) + wmo_interm_cc3_14(d, l) * wmo_interm_cc3_17(d, l)
term(18) = term(18) + wmo_interm_cc3_18(d, l) * wmo_interm_cc3_17(d, l)
term(19) = term(19) + wmo_interm_cc3_18(d, l) * wmo_interm_cc3_15(d, l)
term(20) = term(20) + wmo_interm_cc3_16(d, l) * wmo_interm_cc3_15(d, l)
term(21) = term(21) + wmo_interm_cc3_16(d, l) * wmo_interm_cc3_17(d, l)
term(22) = term(22) + wmo_interm_cc3_19(d, l) * wmo_interm_cc3_17(d, l)
term(23) = term(23) + wmo_interm_cc3_19(d, l) * wmo_interm_cc3_15(d, l)
end do 
end do 

term(0) = -term(0) 
term(1) = term(1) * 1.9999999999999998d+0 
term(2) = -term(2) 
term(3) = term(3) * 1.9999999999999998d+0 
term(4) = term(4) * 1.9999999999999998d+0 
term(5) = term(5) * (-3.9999999999999996d+0) 
term(6) = term(6) * (-3.9999999999999996d+0) 
term(7) = term(7) * 7.999999999999999d+0 
term(8) = term(8) * 2.0d+0 
term(9) = term(9) * (-4.0d+0) 
term(10) = term(10) * 2.0d+0 
term(11) = term(11) * (-4.0d+0) 
term(12) = -term(12) 
term(13) = term(13) * 2.0d+0 
term(14) = term(14) * 2.0d+0 
term(15) = term(15) * (-4.0d+0) 
term(16) = -term(16) 
term(17) = term(17) * 2.0d+0 
term(18) = term(18) * (-4.0d+0) 
term(19) = term(19) * 2.0d+0 
term(20) = term(20) * 2.0d+0 
term(21) = term(21) * (-4.0d+0) 
term(22) = term(22) * 8.0d+0 
term(23) = term(23) * (-4.0d+0) 

do j = 1, nocc 
do a = nocc + 1, nactive 
term(24) = term(24) + wmo_interm_cc3_13(a, j) * wmo_interm_cc3_9(a, j)
term(25) = term(25) + wmo_interm_cc3_13(a, j) * wmo_interm_cc3_10(a, j)
term(26) = term(26) + wmo_interm_cc3_13(a, j) * wmo_interm_cc3_9(a, j)
term(27) = term(27) + wmo_interm_cc3_13(a, j) * wmo_interm_cc3_10(a, j)
term(28) = term(28) + wmo_interm_cc3_13(a, j) * wmo_interm_cc3_11(a, j)
term(29) = term(29) + wmo_interm_cc3_13(a, j) * wmo_interm_cc3_12(a, j)
end do 
end do 

term(24) = term(24) * (-0.49999999999999994d+0) 
term(26) = term(26) * (-0.49999999999999994d+0) 
term(28) = term(28) * (-1.9999999999999998d+0) 

do k = 1, nocc 
do j = 1, nocc 
do c = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(30) = term(30) + wmo_interm_cc3_25(a, c, j, k) * wmo_interm_cc3_22(a, c, k, j)
term(31) = term(31) + wmo_interm_cc3_25(a, c, j, k) * wmo_interm_cc3_22(a, c, j, k)
term(32) = term(32) + wmo_interm_cc3_25(a, c, j, k) * wmo_interm_cc3_21(a, c, j, k)
term(33) = term(33) + wmo_interm_cc3_25(a, c, j, k) * wmo_interm_cc3_23(a, c, j, k)
term(34) = term(34) + wmo_interm_cc3_25(a, c, j, k) * wmo_interm_cc3_21(a, c, k, j)
term(35) = term(35) + wmo_interm_cc3_25(a, c, j, k) * wmo_interm_cc3_23(a, c, k, j)
end do 
end do 
end do 
end do 

term(30) = term(30) * 0.6666666666666667d+0 
term(31) = term(31) * (-0.33333333333333337d+0) 
term(32) = term(32) * 0.6666666666666667d+0 
term(33) = term(33) * (-1.3333333333333335d+0) 
term(34) = term(34) * (-0.33333333333333337d+0) 
term(35) = term(35) * 0.6666666666666667d+0 

do l = 1, nocc 
do k = 1, nocc 
do d = nocc + 1, nactive 
do c = nocc + 1, nactive 
term(36) = term(36) + wmo_interm_cc3_28(c, d, k, l) * wmo_interm_cc3_29(c, d, l, k)
term(37) = term(37) + wmo_interm_cc3_28(c, d, k, l) * wmo_interm_cc3_30(c, d, l, k)
term(38) = term(38) + wmo_interm_cc3_28(c, d, l, k) * wmo_interm_cc3_31(c, d, k, l)
term(39) = term(39) + wmo_interm_cc3_28(c, d, k, l) * wmo_interm_cc3_31(c, d, k, l)
term(40) = term(40) + wmo_interm_cc3_32(c, d, k, l) * wmo_interm_cc3_30(c, d, l, k)
term(41) = term(41) + wmo_interm_cc3_32(c, d, k, l) * wmo_interm_cc3_31(c, d, k, l)
term(42) = term(42) + wmo_interm_cc3_32(c, d, k, l) * wmo_interm_cc3_29(c, d, l, k)
term(43) = term(43) + wmo_interm_cc3_32(c, d, l, k) * wmo_interm_cc3_31(c, d, k, l)
term(44) = term(44) + wmo_interm_cc3_33(c, d, k, l) * wmo_interm_cc3_30(c, d, l, k)
term(45) = term(45) + wmo_interm_cc3_33(c, d, k, l) * wmo_interm_cc3_31(c, d, k, l)
term(46) = term(46) + wmo_interm_cc3_33(c, d, k, l) * wmo_interm_cc3_29(c, d, l, k)
term(47) = term(47) + wmo_interm_cc3_33(c, d, l, k) * wmo_interm_cc3_31(c, d, k, l)
end do 
end do 
end do 
end do 

term(36) = -term(36) 
term(37) = term(37) * 2.0000000000000004d+0 
term(38) = term(38) * 2.0000000000000004d+0 
term(39) = term(39) * (-4.000000000000001d+0) 
term(40) = -term(40) 
term(41) = term(41) * 2.0000000000000004d+0 
term(42) = term(42) * 2.0000000000000004d+0 
term(43) = term(43) * (-4.000000000000001d+0) 
term(44) = term(44) * (-4.666666666666668d+0) 
term(45) = term(45) * 8.000000000000002d+0 
term(46) = term(46) * 2.0000000000000004d+0 
term(47) = term(47) * (-4.000000000000001d+0) 

do k = 1, nocc 
do l = 1, nocc 
do d = nocc + 1, nactive 
do c = nocc + 1, nactive 
term(48) = term(48) + wmo_interm_cc3_28(c, d, l, k) * wmo_interm_cc3_30(c, d, l, k)
term(49) = term(49) + wmo_interm_cc3_28(c, d, l, k) * wmo_interm_cc3_29(c, d, l, k)
term(50) = term(50) + wmo_interm_cc3_32(c, d, l, k) * wmo_interm_cc3_29(c, d, l, k)
term(51) = term(51) + wmo_interm_cc3_32(c, d, l, k) * wmo_interm_cc3_30(c, d, l, k)
term(52) = term(52) + wmo_interm_cc3_33(c, d, l, k) * wmo_interm_cc3_30(c, d, l, k)
term(53) = term(53) + wmo_interm_cc3_33(c, d, l, k) * wmo_interm_cc3_29(c, d, l, k)
end do 
end do 
end do 
end do 

term(48) = -term(48) 
term(49) = term(49) * 2.0000000000000004d+0 
term(50) = -term(50) 
term(51) = term(51) * 2.0000000000000004d+0 
term(52) = term(52) * 2.0000000000000004d+0 
term(53) = term(53) * (-3.333333333333334d+0) 

do j = 1, nocc 
do b = nocc + 1, nactive 
term(54) = term(54) + wmo_interm_cc3_8(b, j) * wmo_interm_cc3_9(b, j)
term(55) = term(55) + wmo_interm_cc3_8(b, j) * wmo_interm_cc3_10(b, j)
term(56) = term(56) + wmo_interm_cc3_8(b, j) * wmo_interm_cc3_9(b, j)
term(57) = term(57) + wmo_interm_cc3_8(b, j) * wmo_interm_cc3_10(b, j)
term(58) = term(58) + wmo_interm_cc3_8(b, j) * wmo_interm_cc3_11(b, j)
term(59) = term(59) + wmo_interm_cc3_8(b, j) * wmo_interm_cc3_12(b, j)
end do 
end do 

term(55) = term(55) * (-1.9999999999999998d+0) 
term(57) = term(57) * (-1.9999999999999998d+0) 
term(58) = term(58) * 3.9999999999999996d+0 
term(59) = term(59) * (-1.9999999999999998d+0) 

do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(60) = term(60) + wmo_interm_cc3_27(a, b, j, k) * wmo_interm_cc3_22(a, b, k, j)
term(61) = term(61) + wmo_interm_cc3_27(a, b, j, k) * wmo_interm_cc3_22(a, b, j, k)
term(62) = term(62) + wmo_interm_cc3_27(a, b, j, k) * wmo_interm_cc3_21(a, b, j, k)
term(63) = term(63) + wmo_interm_cc3_27(a, b, j, k) * wmo_interm_cc3_23(a, b, j, k)
term(64) = term(64) + wmo_interm_cc3_27(a, b, j, k) * wmo_interm_cc3_21(a, b, k, j)
term(65) = term(65) + wmo_interm_cc3_27(a, b, j, k) * wmo_interm_cc3_23(a, b, k, j)
end do 
end do 
end do 
end do 

term(60) = term(60) * (-0.33333333333333337d+0) 
term(61) = term(61) * 0.6666666666666667d+0 
term(62) = term(62) * (-0.33333333333333337d+0) 
term(63) = term(63) * 0.6666666666666667d+0 
term(64) = term(64) * 0.6666666666666667d+0 
term(65) = term(65) * (-1.3333333333333335d+0) 

do k = 1, nocc 
do j = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(66) = term(66) + wmo_interm_cc3_20(b, c, j, k) * wmo_interm_cc3_21(b, c, k, j)
term(67) = term(67) + wmo_interm_cc3_20(b, c, j, k) * wmo_interm_cc3_22(b, c, k, j)
term(68) = term(68) + wmo_interm_cc3_20(b, c, j, k) * wmo_interm_cc3_22(b, c, j, k)
term(69) = term(69) + wmo_interm_cc3_20(b, c, j, k) * wmo_interm_cc3_23(b, c, k, j)
term(70) = term(70) + wmo_interm_cc3_20(b, c, j, k) * wmo_interm_cc3_21(b, c, j, k)
term(71) = term(71) + wmo_interm_cc3_20(b, c, j, k) * wmo_interm_cc3_23(b, c, j, k)
end do 
end do 
end do 
end do 

term(66) = term(66) * 0.6666666666666667d+0 
term(67) = term(67) * (-1.3333333333333335d+0) 
term(68) = term(68) * 0.6666666666666667d+0 
term(69) = term(69) * (-1.3333333333333335d+0) 
term(70) = term(70) * (-1.3333333333333335d+0) 
term(71) = term(71) * 2.666666666666667d+0 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(72) = term(72) + wmo_interm_cc3_8(a, i) * wmo_interm_cc3_9(a, i)
term(73) = term(73) + wmo_interm_cc3_8(a, i) * wmo_interm_cc3_10(a, i)
term(74) = term(74) + wmo_interm_cc3_8(a, i) * wmo_interm_cc3_9(a, i)
term(75) = term(75) + wmo_interm_cc3_8(a, i) * wmo_interm_cc3_10(a, i)
term(76) = term(76) + wmo_interm_cc3_8(a, i) * wmo_interm_cc3_11(a, i)
term(77) = term(77) + wmo_interm_cc3_8(a, i) * wmo_interm_cc3_12(a, i)
end do 
end do 

term(73) = term(73) * (-1.9999999999999998d+0) 
term(75) = term(75) * (-1.9999999999999998d+0) 
term(76) = term(76) * 3.9999999999999996d+0 
term(77) = term(77) * (-1.9999999999999998d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
term(78) = term(78) + wmo_interm_cc3_13(b, i) * wmo_interm_cc3_9(b, i)
term(79) = term(79) + wmo_interm_cc3_13(b, i) * wmo_interm_cc3_10(b, i)
term(80) = term(80) + wmo_interm_cc3_13(b, i) * wmo_interm_cc3_11(b, i)
term(81) = term(81) + wmo_interm_cc3_13(b, i) * wmo_interm_cc3_12(b, i)
term(82) = term(82) + wmo_interm_cc3_13(b, i) * wmo_interm_cc3_10(b, i)
term(83) = term(83) + wmo_interm_cc3_13(b, i) * wmo_interm_cc3_9(b, i)
end do 
end do 

term(78) = term(78) * (-0.49999999999999994d+0) 
term(80) = term(80) * (-1.9999999999999998d+0) 
term(83) = term(83) * (-0.5d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(84) = term(84) + wmo_interm_cc3_20(a, b, i, j) * wmo_interm_cc3_21(a, b, j, i)
term(85) = term(85) + wmo_interm_cc3_20(a, b, i, j) * wmo_interm_cc3_22(a, b, j, i)
term(86) = term(86) + wmo_interm_cc3_20(a, b, i, j) * wmo_interm_cc3_22(a, b, i, j)
term(87) = term(87) + wmo_interm_cc3_20(a, b, i, j) * wmo_interm_cc3_23(a, b, j, i)
term(88) = term(88) + wmo_interm_cc3_20(a, b, i, j) * wmo_interm_cc3_21(a, b, i, j)
term(89) = term(89) + wmo_interm_cc3_20(a, b, i, j) * wmo_interm_cc3_23(a, b, i, j)
end do 
end do 
end do 
end do 

term(84) = term(84) * 0.6666666666666667d+0 
term(85) = term(85) * (-1.3333333333333335d+0) 
term(86) = term(86) * 0.6666666666666667d+0 
term(87) = term(87) * (-1.3333333333333335d+0) 
term(88) = term(88) * (-1.3333333333333335d+0) 
term(89) = term(89) * 2.666666666666667d+0 

do j = 1, nocc 
do i = 1, nocc 
do c = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(90) = term(90) + wmo_interm_cc3_24(a, c, i, j) * wmo_interm_cc3_21(a, c, j, i)
term(91) = term(91) + wmo_interm_cc3_24(a, c, i, j) * wmo_interm_cc3_22(a, c, j, i)
term(92) = term(92) + wmo_interm_cc3_24(a, c, i, j) * wmo_interm_cc3_22(a, c, i, j)
term(93) = term(93) + wmo_interm_cc3_24(a, c, i, j) * wmo_interm_cc3_23(a, c, j, i)
term(94) = term(94) + wmo_interm_cc3_24(a, c, i, j) * wmo_interm_cc3_21(a, c, i, j)
term(95) = term(95) + wmo_interm_cc3_24(a, c, i, j) * wmo_interm_cc3_23(a, c, i, j)
end do 
end do 
end do 
end do 

term(90) = term(90) * (-0.33333333333333337d+0) 
term(91) = term(91) * 0.6666666666666667d+0 
term(92) = term(92) * (-0.33333333333333337d+0) 
term(93) = term(93) * 0.6666666666666667d+0 
term(94) = term(94) * 0.6666666666666667d+0 
term(95) = term(95) * (-1.3333333333333335d+0) 

do k = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(96) = term(96) + wmo_interm_cc3_24(a, b, i, k) * wmo_interm_cc3_21(a, b, k, i)
term(97) = term(97) + wmo_interm_cc3_24(a, b, i, k) * wmo_interm_cc3_22(a, b, k, i)
term(98) = term(98) + wmo_interm_cc3_24(a, b, i, k) * wmo_interm_cc3_22(a, b, i, k)
term(99) = term(99) + wmo_interm_cc3_24(a, b, i, k) * wmo_interm_cc3_23(a, b, k, i)
term(100) = term(100) + wmo_interm_cc3_24(a, b, i, k) * wmo_interm_cc3_21(a, b, i, k)
term(101) = term(101) + wmo_interm_cc3_24(a, b, i, k) * wmo_interm_cc3_23(a, b, i, k)
end do 
end do 
end do 
end do 

term(96) = term(96) * (-0.33333333333333337d+0) 
term(97) = term(97) * 0.6666666666666667d+0 
term(98) = term(98) * (-0.33333333333333337d+0) 
term(99) = term(99) * 0.6666666666666667d+0 
term(100) = term(100) * 0.6666666666666667d+0 
term(101) = term(101) * (-1.3333333333333335d+0) 

do k = 1, nocc 
do i = 1, nocc 
do c = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(102) = term(102) + wmo_interm_cc3_20(a, c, i, k) * wmo_interm_cc3_21(a, c, k, i)
term(103) = term(103) + wmo_interm_cc3_20(a, c, i, k) * wmo_interm_cc3_22(a, c, k, i)
term(104) = term(104) + wmo_interm_cc3_20(a, c, i, k) * wmo_interm_cc3_22(a, c, i, k)
term(105) = term(105) + wmo_interm_cc3_20(a, c, i, k) * wmo_interm_cc3_23(a, c, k, i)
term(106) = term(106) + wmo_interm_cc3_20(a, c, i, k) * wmo_interm_cc3_21(a, c, i, k)
term(107) = term(107) + wmo_interm_cc3_20(a, c, i, k) * wmo_interm_cc3_23(a, c, i, k)
end do 
end do 
end do 
end do 

term(102) = term(102) * 0.6666666666666667d+0 
term(103) = term(103) * (-1.3333333333333335d+0) 
term(104) = term(104) * 0.6666666666666667d+0 
term(105) = term(105) * (-1.3333333333333335d+0) 
term(106) = term(106) * (-1.3333333333333335d+0) 
term(107) = term(107) * 2.666666666666667d+0 

do k = 1, nocc 
do i = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(108) = term(108) + wmo_interm_cc3_25(b, c, i, k) * wmo_interm_cc3_21(b, c, k, i)
term(109) = term(109) + wmo_interm_cc3_25(b, c, i, k) * wmo_interm_cc3_22(b, c, k, i)
term(110) = term(110) + wmo_interm_cc3_25(b, c, i, k) * wmo_interm_cc3_22(b, c, i, k)
term(111) = term(111) + wmo_interm_cc3_25(b, c, i, k) * wmo_interm_cc3_23(b, c, k, i)
term(112) = term(112) + wmo_interm_cc3_25(b, c, i, k) * wmo_interm_cc3_21(b, c, i, k)
term(113) = term(113) + wmo_interm_cc3_25(b, c, i, k) * wmo_interm_cc3_23(b, c, i, k)
end do 
end do 
end do 
end do 

term(108) = term(108) * (-0.33333333333333337d+0) 
term(109) = term(109) * 0.6666666666666667d+0 
term(110) = term(110) * (-0.33333333333333337d+0) 
term(111) = term(111) * 0.6666666666666667d+0 
term(112) = term(112) * 0.6666666666666667d+0 
term(113) = term(113) * (-1.3333333333333335d+0) 

do j = 1, nocc 
do i = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(114) = term(114) + wmo_interm_cc3_26(b, c, i, j) * wmo_interm_cc3_21(b, c, j, i)
term(115) = term(115) + wmo_interm_cc3_26(b, c, i, j) * wmo_interm_cc3_22(b, c, j, i)
term(116) = term(116) + wmo_interm_cc3_26(b, c, i, j) * wmo_interm_cc3_22(b, c, i, j)
term(117) = term(117) + wmo_interm_cc3_26(b, c, i, j) * wmo_interm_cc3_21(b, c, i, j)
term(118) = term(118) + wmo_interm_cc3_26(b, c, i, j) * wmo_interm_cc3_23(b, c, j, i)
term(119) = term(119) + wmo_interm_cc3_26(b, c, i, j) * wmo_interm_cc3_23(b, c, i, j)
end do 
end do 
end do 
end do 

term(114) = term(114) * 0.6666666666666667d+0 
term(115) = term(115) * (-0.33333333333333337d+0) 
term(116) = term(116) * 0.6666666666666667d+0 
term(117) = term(117) * (-0.33333333333333337d+0) 
term(118) = term(118) * (-1.3333333333333335d+0) 
term(119) = term(119) * 0.6666666666666667d+0 


    d_overlap_cc3 = 0.d+0
    do s = 0, 119
    d_overlap_cc3 = d_overlap_cc3 + term(s)
    end do                                                                                                  
    end function d_overlap_cc3
    

    

    



!     function d_overlap_cc3(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr)
!     double precision :: d_overlap_cc3
!     integer, intent(in) :: nocc, nactive
!     double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2
!     double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: t1
!     double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2
!     double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: s1
!     double precision, dimension(:), intent(in) :: vrdav_Rl                                                                                                                              
!     double precision, dimension(:), intent(in) :: vrdav_Rr 
!     integer :: s ,d,a,l,i,k,j,b,c
!     double precision, dimension(0:263) :: term
!     term = 0.d+0
!     do d = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! term(0) = term(0) + wmo_interm_cc3_0(a, d) * wmo_interm_cc3_1(a, d)
! term(1) = term(1) + wmo_interm_cc3_0(a, d) * wmo_interm_cc3_2(a, d)
! term(2) = term(2) + wmo_interm_cc3_0(a, d) * wmo_interm_cc3_3(a, d)
! term(3) = term(3) + wmo_interm_cc3_0(a, d) * wmo_interm_cc3_4(a, d)
! term(4) = term(4) + wmo_interm_cc3_0(a, d) * wmo_interm_cc3_5(a, d)
! term(5) = term(5) + wmo_interm_cc3_0(a, d) * wmo_interm_cc3_6(a, d)
! term(6) = term(6) + wmo_interm_cc3_35(a, d) * wmo_interm_cc3_33(a, d)
! term(7) = term(7) + wmo_interm_cc3_36(a, d) * wmo_interm_cc3_33(a, d)
! term(8) = term(8) + wmo_interm_cc3_32(a, d) * wmo_interm_cc3_33(a, d)
! term(9) = term(9) + wmo_interm_cc3_34(a, d) * wmo_interm_cc3_33(a, d)
! term(10) = term(10) + wmo_interm_cc3_38(a, d) * wmo_interm_cc3_33(a, d)
! term(11) = term(11) + wmo_interm_cc3_37(a, d) * wmo_interm_cc3_33(a, d)
! end do 
! end do 

! term(0) = -term(0) 
! term(1) = term(1) * 2.0000000000000004d+0 
! term(2) = -term(2) 
! term(3) = term(3) * 2.0000000000000004d+0 
! term(4) = term(4) * 2.0000000000000004d+0 
! term(5) = term(5) * (-4.000000000000001d+0) 
! term(6) = term(6) * 0.6666666666666666d+0 
! term(7) = term(7) * (-1.3333333333333333d+0) 
! term(8) = term(8) * (-0.33333333333333337d+0) 
! term(9) = term(9) * 0.6666666666666667d+0 
! term(10) = term(10) * 0.6666666666666667d+0 
! term(11) = term(11) * (-0.33333333333333337d+0) 

! do l = 1, nocc 
! do i = 1, nocc 
! term(12) = term(12) + wmo_interm_cc3_7(i, l) * wmo_interm_cc3_8(i, l)
! term(13) = term(13) + wmo_interm_cc3_7(i, l) * wmo_interm_cc3_9(i, l)
! term(14) = term(14) + wmo_interm_cc3_7(i, l) * wmo_interm_cc3_8(i, l)
! term(15) = term(15) + wmo_interm_cc3_7(i, l) * wmo_interm_cc3_9(i, l)
! term(16) = term(16) + wmo_interm_cc3_7(i, l) * wmo_interm_cc3_8(i, l)
! term(17) = term(17) + wmo_interm_cc3_7(i, l) * wmo_interm_cc3_10(i, l)
! term(18) = term(18) + wmo_interm_cc3_7(i, l) * wmo_interm_cc3_10(i, l)
! term(19) = term(19) + wmo_interm_cc3_7(i, l) * wmo_interm_cc3_11(i, l)
! term(20) = term(20) + wmo_interm_cc3_7(i, l) * wmo_interm_cc3_12(i, l)
! term(21) = term(21) + wmo_interm_cc3_7(i, l) * wmo_interm_cc3_10(i, l)
! term(22) = term(22) + wmo_interm_cc3_7(i, l) * wmo_interm_cc3_13(i, l)
! term(23) = term(23) + wmo_interm_cc3_7(i, l) * wmo_interm_cc3_11(i, l)
! term(24) = term(24) + wmo_interm_cc3_7(i, l) * wmo_interm_cc3_12(i, l)
! term(25) = term(25) + wmo_interm_cc3_7(i, l) * wmo_interm_cc3_13(i, l)
! term(26) = term(26) + wmo_interm_cc3_7(i, l) * wmo_interm_cc3_12(i, l)
! term(27) = term(27) + wmo_interm_cc3_7(i, l) * wmo_interm_cc3_13(i, l)
! term(28) = term(28) + wmo_interm_cc3_7(i, l) * wmo_interm_cc3_9(i, l)
! term(29) = term(29) + wmo_interm_cc3_7(i, l) * wmo_interm_cc3_11(i, l)
! term(30) = term(30) + wmo_interm_cc3_41(i, l) * wmo_interm_cc3_40(i, l)
! term(31) = term(31) + wmo_interm_cc3_44(i, l) * wmo_interm_cc3_40(i, l)
! term(32) = term(32) + wmo_interm_cc3_39(i, l) * wmo_interm_cc3_40(i, l)
! term(33) = term(33) + wmo_interm_cc3_43(i, l) * wmo_interm_cc3_40(i, l)
! term(34) = term(34) + wmo_interm_cc3_45(i, l) * wmo_interm_cc3_40(i, l)
! term(35) = term(35) + wmo_interm_cc3_42(i, l) * wmo_interm_cc3_40(i, l)
! end do 
! end do 

! term(12) = term(12) * (-0.3333333333333333d+0) 
! term(13) = term(13) * 0.6666666666666666d+0 
! term(14) = term(14) * (-0.3333333333333333d+0) 
! term(15) = term(15) * 0.6666666666666666d+0 
! term(16) = term(16) * (-0.3333333333333333d+0) 
! term(17) = term(17) * 0.6666666666666666d+0 
! term(18) = term(18) * 0.6666666666666666d+0 
! term(19) = term(19) * (-1.3333333333333333d+0) 
! term(20) = term(20) * (-0.3333333333333333d+0) 
! term(21) = term(21) * 0.6666666666666666d+0 
! term(22) = term(22) * 0.6666666666666666d+0 
! term(23) = term(23) * (-1.3333333333333333d+0) 
! term(24) = term(24) * (-0.3333333333333333d+0) 
! term(25) = term(25) * 0.6666666666666666d+0 
! term(26) = term(26) * (-0.3333333333333333d+0) 
! term(27) = term(27) * 0.6666666666666666d+0 
! term(28) = term(28) * 0.6666666666666666d+0 
! term(29) = term(29) * (-1.3333333333333333d+0) 
! term(30) = term(30) * 0.6666666666666666d+0 
! term(31) = term(31) * (-1.3333333333333333d+0) 
! term(32) = term(32) * (-0.33333333333333337d+0) 
! term(33) = term(33) * 0.6666666666666667d+0 
! term(34) = term(34) * 0.6666666666666667d+0 
! term(35) = term(35) * (-0.33333333333333337d+0) 

! do k = 1, nocc 
! do i = 1, nocc 
! do j = 1, nocc 
! do b = nocc + 1, nactive 
! term(36) = term(36) + wmo_interm_cc3_14(b, i, j, k) * wmo_interm_cc3_15(b, k, j, i)
! term(37) = term(37) + wmo_interm_cc3_14(b, i, j, k) * wmo_interm_cc3_15(b, j, k, i)
! term(38) = term(38) + wmo_interm_cc3_14(b, i, j, k) * wmo_interm_cc3_16(b, j, k, i)
! term(39) = term(39) + wmo_interm_cc3_14(b, i, j, k) * wmo_interm_cc3_16(b, k, j, i)
! term(40) = term(40) + wmo_interm_cc3_14(b, i, j, k) * wmo_interm_cc3_17(b, j, k, i)
! term(41) = term(41) + wmo_interm_cc3_14(b, i, j, k) * wmo_interm_cc3_17(b, k, j, i)
! term(42) = term(42) + wmo_interm_cc3_14(b, i, j, k) * wmo_interm_cc3_16(b, j, k, i)
! term(43) = term(43) + wmo_interm_cc3_14(b, i, j, k) * wmo_interm_cc3_16(b, k, j, i)
! term(44) = term(44) + wmo_interm_cc3_14(b, i, j, k) * wmo_interm_cc3_25(b, j, k, i)
! term(45) = term(45) + wmo_interm_cc3_14(b, i, j, k) * wmo_interm_cc3_25(b, k, j, i)
! term(46) = term(46) + wmo_interm_cc3_14(b, i, j, k) * wmo_interm_cc3_17(b, j, k, i)
! term(47) = term(47) + wmo_interm_cc3_14(b, i, j, k) * wmo_interm_cc3_17(b, k, j, i)
! term(48) = term(48) + wmo_interm_cc3_54(b, i, j, k) * wmo_interm_cc3_52(b, k, j, i)
! term(49) = term(49) + wmo_interm_cc3_54(b, i, j, k) * wmo_interm_cc3_57(b, k, j, i)
! term(50) = term(50) + wmo_interm_cc3_54(b, i, j, k) * wmo_interm_cc3_55(b, k, j, i)
! term(51) = term(51) + wmo_interm_cc3_54(b, i, j, k) * wmo_interm_cc3_56(b, k, j, i)
! term(52) = term(52) + wmo_interm_cc3_54(b, i, j, k) * wmo_interm_cc3_57(b, k, j, i)
! term(53) = term(53) + wmo_interm_cc3_54(b, i, j, k) * wmo_interm_cc3_52(b, k, j, i)
! end do 
! end do 
! end do 
! end do 

! term(36) = -term(36) 
! term(37) = term(37) * 1.9999999999999998d+0 
! term(38) = -term(38) 
! term(39) = term(39) * 1.9999999999999998d+0 
! term(40) = term(40) * 1.9999999999999998d+0 
! term(41) = term(41) * (-3.9999999999999996d+0) 
! term(42) = -term(42) 
! term(43) = term(43) * 1.9999999999999998d+0 
! term(44) = -term(44) 
! term(45) = term(45) * 1.9999999999999998d+0 
! term(46) = term(46) * 1.9999999999999998d+0 
! term(47) = term(47) * (-3.9999999999999996d+0) 
! term(49) = term(49) * (-1.9999999999999998d+0) 
! term(50) = term(50) * (-0.49999999999999994d+0) 
! term(52) = term(52) * (-1.9999999999999998d+0) 

! do l = 1, nocc 
! do d = nocc + 1, nactive 
! term(54) = term(54) + wmo_interm_cc3_18(d, l) * wmo_interm_cc3_19(d, l)
! term(55) = term(55) + wmo_interm_cc3_18(d, l) * wmo_interm_cc3_20(d, l)
! term(56) = term(56) + wmo_interm_cc3_18(d, l) * wmo_interm_cc3_21(d, l)
! term(57) = term(57) + wmo_interm_cc3_18(d, l) * wmo_interm_cc3_22(d, l)
! term(58) = term(58) + wmo_interm_cc3_18(d, l) * wmo_interm_cc3_23(d, l)
! term(59) = term(59) + wmo_interm_cc3_18(d, l) * wmo_interm_cc3_24(d, l)
! term(60) = term(60) + wmo_interm_cc3_31(d, l) * wmo_interm_cc3_23(d, l)
! term(61) = term(61) + wmo_interm_cc3_31(d, l) * wmo_interm_cc3_24(d, l)
! term(62) = term(62) + wmo_interm_cc3_31(d, l) * wmo_interm_cc3_21(d, l)
! term(63) = term(63) + wmo_interm_cc3_31(d, l) * wmo_interm_cc3_22(d, l)
! term(64) = term(64) + wmo_interm_cc3_31(d, l) * wmo_interm_cc3_19(d, l)
! term(65) = term(65) + wmo_interm_cc3_31(d, l) * wmo_interm_cc3_20(d, l)
! term(66) = term(66) + wmo_interm_cc3_58(d, l) * wmo_interm_cc3_59(d, l)
! term(67) = term(67) + wmo_interm_cc3_62(d, l) * wmo_interm_cc3_59(d, l)
! term(68) = term(68) + wmo_interm_cc3_58(d, l) * wmo_interm_cc3_63(d, l)
! term(69) = term(69) + wmo_interm_cc3_62(d, l) * wmo_interm_cc3_63(d, l)
! term(70) = term(70) + wmo_interm_cc3_58(d, l) * wmo_interm_cc3_59(d, l)
! term(71) = term(71) + wmo_interm_cc3_58(d, l) * wmo_interm_cc3_63(d, l)
! term(72) = term(72) + wmo_interm_cc3_65(d, l) * wmo_interm_cc3_63(d, l)
! term(73) = term(73) + wmo_interm_cc3_65(d, l) * wmo_interm_cc3_59(d, l)
! term(74) = term(74) + wmo_interm_cc3_62(d, l) * wmo_interm_cc3_59(d, l)
! term(75) = term(75) + wmo_interm_cc3_62(d, l) * wmo_interm_cc3_63(d, l)
! term(76) = term(76) + wmo_interm_cc3_68(d, l) * wmo_interm_cc3_63(d, l)
! term(77) = term(77) + wmo_interm_cc3_68(d, l) * wmo_interm_cc3_59(d, l)
! end do 
! end do 

! term(54) = -term(54) 
! term(55) = term(55) * 1.9999999999999998d+0 
! term(56) = -term(56) 
! term(57) = term(57) * 1.9999999999999998d+0 
! term(58) = term(58) * 1.9999999999999998d+0 
! term(59) = term(59) * (-3.9999999999999996d+0) 
! term(60) = term(60) * (-3.9999999999999996d+0) 
! term(61) = term(61) * 7.999999999999999d+0 
! term(62) = term(62) * 2.0d+0 
! term(63) = term(63) * (-4.0d+0) 
! term(64) = term(64) * 2.0d+0 
! term(65) = term(65) * (-4.0d+0) 
! term(66) = -term(66) 
! term(67) = term(67) * 2.0d+0 
! term(68) = term(68) * 2.0d+0 
! term(69) = term(69) * (-4.0d+0) 
! term(70) = -term(70) 
! term(71) = term(71) * 2.0d+0 
! term(72) = term(72) * (-4.0d+0) 
! term(73) = term(73) * 2.0d+0 
! term(74) = term(74) * 2.0d+0 
! term(75) = term(75) * (-4.0d+0) 
! term(76) = term(76) * 8.0d+0 
! term(77) = term(77) * (-4.0d+0) 

! do j = 1, nocc 
! do a = nocc + 1, nactive 
! term(78) = term(78) + wmo_interm_cc3_53(a, j) * wmo_interm_cc3_47(a, j)
! term(79) = term(79) + wmo_interm_cc3_53(a, j) * wmo_interm_cc3_48(a, j)
! term(80) = term(80) + wmo_interm_cc3_53(a, j) * wmo_interm_cc3_47(a, j)
! term(81) = term(81) + wmo_interm_cc3_53(a, j) * wmo_interm_cc3_48(a, j)
! term(82) = term(82) + wmo_interm_cc3_53(a, j) * wmo_interm_cc3_49(a, j)
! term(83) = term(83) + wmo_interm_cc3_53(a, j) * wmo_interm_cc3_50(a, j)
! end do 
! end do 

! term(78) = term(78) * (-0.49999999999999994d+0) 
! term(80) = term(80) * (-0.49999999999999994d+0) 
! term(82) = term(82) * (-1.9999999999999998d+0) 

! do j = 1, nocc 
! do i = 1, nocc 
! term(84) = term(84) + wmo_interm_cc3_69(i, j) * wmo_interm_cc3_70(j, i)
! term(85) = term(85) + wmo_interm_cc3_69(i, j) * wmo_interm_cc3_71(j, i)
! term(86) = term(86) + wmo_interm_cc3_69(i, j) * wmo_interm_cc3_72(j, i)
! term(87) = term(87) + wmo_interm_cc3_69(i, j) * wmo_interm_cc3_73(j, i)
! term(88) = term(88) + wmo_interm_cc3_69(i, j) * wmo_interm_cc3_74(j, i)
! term(89) = term(89) + wmo_interm_cc3_69(i, j) * wmo_interm_cc3_75(j, i)
! term(90) = term(90) + wmo_interm_cc3_69(i, j) * wmo_interm_cc3_76(j, i)
! term(91) = term(91) + wmo_interm_cc3_69(i, j) * wmo_interm_cc3_77(j, i)
! term(92) = term(92) + wmo_interm_cc3_69(i, j) * wmo_interm_cc3_78(j, i)
! term(93) = term(93) + wmo_interm_cc3_69(i, j) * wmo_interm_cc3_79(j, i)
! term(94) = term(94) + wmo_interm_cc3_69(i, j) * wmo_interm_cc3_71(j, i)
! term(95) = term(95) + wmo_interm_cc3_69(i, j) * wmo_interm_cc3_75(j, i)
! term(96) = term(96) + wmo_interm_cc3_69(i, j) * wmo_interm_cc3_77(j, i)
! term(97) = term(97) + wmo_interm_cc3_69(i, j) * wmo_interm_cc3_75(j, i)
! term(98) = term(98) + wmo_interm_cc3_69(i, j) * wmo_interm_cc3_78(j, i)
! term(99) = term(99) + wmo_interm_cc3_69(i, j) * wmo_interm_cc3_74(j, i)
! term(100) = term(100) + wmo_interm_cc3_69(i, j) * wmo_interm_cc3_72(j, i)
! term(101) = term(101) + wmo_interm_cc3_69(i, j) * wmo_interm_cc3_71(j, i)
! end do 
! end do 

! term(84) = term(84) * (-0.3333333333333333d+0) 
! term(85) = term(85) * 0.6666666666666666d+0 
! term(86) = term(86) * (-0.3333333333333333d+0) 
! term(87) = term(87) * 0.6666666666666666d+0 
! term(88) = term(88) * 0.6666666666666666d+0 
! term(89) = term(89) * (-1.3333333333333333d+0) 
! term(90) = term(90) * (-0.3333333333333333d+0) 
! term(91) = term(91) * 0.6666666666666666d+0 
! term(92) = term(92) * (-0.3333333333333333d+0) 
! term(93) = term(93) * 0.6666666666666666d+0 
! term(94) = term(94) * 0.6666666666666666d+0 
! term(95) = term(95) * (-1.3333333333333333d+0) 
! term(96) = term(96) * 0.6666666666666666d+0 
! term(97) = term(97) * (-1.3333333333333333d+0) 
! term(98) = term(98) * (-0.3333333333333333d+0) 
! term(99) = term(99) * 0.6666666666666666d+0 
! term(100) = term(100) * (-0.3333333333333333d+0) 
! term(101) = term(101) * 0.6666666666666666d+0 

! do l = 1, nocc 
! do k = 1, nocc 
! do i = 1, nocc 
! do d = nocc + 1, nactive 
! term(102) = term(102) + wmo_interm_cc3_26(d, i, l, k) * wmo_interm_cc3_27(d, i, k, l)
! term(103) = term(103) + wmo_interm_cc3_26(d, i, l, k) * wmo_interm_cc3_28(d, i, k, l)
! term(104) = term(104) + wmo_interm_cc3_26(d, i, k, l) * wmo_interm_cc3_28(d, i, k, l)
! term(105) = term(105) + wmo_interm_cc3_26(d, i, k, l) * wmo_interm_cc3_27(d, i, k, l)
! term(106) = term(106) + wmo_interm_cc3_26(d, i, k, l) * wmo_interm_cc3_29(d, i, l, k)
! term(107) = term(107) + wmo_interm_cc3_26(d, i, k, l) * wmo_interm_cc3_30(d, i, l, k)
! term(108) = term(108) + wmo_interm_cc3_26(d, i, k, l) * wmo_interm_cc3_29(d, i, l, k)
! term(109) = term(109) + wmo_interm_cc3_26(d, i, k, l) * wmo_interm_cc3_30(d, i, l, k)
! term(110) = term(110) + wmo_interm_cc3_66(d, i, k, l) * wmo_interm_cc3_61(d, i, l, k)
! term(111) = term(111) + wmo_interm_cc3_67(d, i, k, l) * wmo_interm_cc3_61(d, i, l, k)
! term(112) = term(112) + wmo_interm_cc3_64(d, i, k, l) * wmo_interm_cc3_61(d, i, l, k)
! end do 
! end do 
! end do 
! end do 

! term(102) = -term(102) 
! term(103) = term(103) * 2.0d+0 
! term(104) = -term(104) 
! term(105) = term(105) * 2.0d+0 
! term(106) = -term(106) 
! term(107) = term(107) * 2.0d+0 
! term(108) = -term(108) 
! term(109) = term(109) * 2.0d+0 
! term(110) = -term(110) 
! term(111) = term(111) * 2.0d+0 
! term(112) = -term(112) 

! do k = 1, nocc 
! do l = 1, nocc 
! do i = 1, nocc 
! do d = nocc + 1, nactive 
! term(113) = term(113) + wmo_interm_cc3_26(d, i, l, k) * wmo_interm_cc3_29(d, i, l, k)
! term(114) = term(114) + wmo_interm_cc3_26(d, i, l, k) * wmo_interm_cc3_30(d, i, l, k)
! term(115) = term(115) + wmo_interm_cc3_26(d, i, l, k) * wmo_interm_cc3_29(d, i, l, k)
! term(116) = term(116) + wmo_interm_cc3_26(d, i, l, k) * wmo_interm_cc3_30(d, i, l, k)
! term(117) = term(117) + wmo_interm_cc3_66(d, i, l, k) * wmo_interm_cc3_61(d, i, l, k)
! term(118) = term(118) + wmo_interm_cc3_67(d, i, l, k) * wmo_interm_cc3_61(d, i, l, k)
! term(119) = term(119) + wmo_interm_cc3_64(d, i, l, k) * wmo_interm_cc3_61(d, i, l, k)
! end do 
! end do 
! end do 
! end do 

! term(113) = term(113) * 1.9999999999999998d+0 
! term(114) = term(114) * (-3.9999999999999996d+0) 
! term(115) = term(115) * 1.9999999999999998d+0 
! term(116) = term(116) * (-3.9999999999999996d+0) 
! term(117) = term(117) * 2.0d+0 
! term(118) = term(118) * (-4.0d+0) 
! term(119) = term(119) * 2.0d+0 

! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! term(120) = term(120) + wmo_interm_cc3_80(a, b) * wmo_interm_cc3_81(b, a)
! term(121) = term(121) + wmo_interm_cc3_80(a, b) * wmo_interm_cc3_82(b, a)
! term(122) = term(122) + wmo_interm_cc3_80(a, b) * wmo_interm_cc3_83(b, a)
! term(123) = term(123) + wmo_interm_cc3_80(a, b) * wmo_interm_cc3_84(b, a)
! term(124) = term(124) + wmo_interm_cc3_80(a, b) * wmo_interm_cc3_85(b, a)
! term(125) = term(125) + wmo_interm_cc3_80(a, b) * wmo_interm_cc3_86(b, a)
! end do 
! end do 

! term(120) = term(120) * 2.0000000000000004d+0 
! term(121) = -term(121) 
! term(122) = term(122) * (-4.000000000000001d+0) 
! term(123) = term(123) * 2.0000000000000004d+0 
! term(124) = -term(124) 
! term(125) = term(125) * 2.0000000000000004d+0 

! do d = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! term(126) = term(126) + wmo_interm_cc3_32(c, d) * wmo_interm_cc3_33(c, d)
! term(127) = term(127) + wmo_interm_cc3_34(c, d) * wmo_interm_cc3_33(c, d)
! term(128) = term(128) + wmo_interm_cc3_37(c, d) * wmo_interm_cc3_33(c, d)
! term(129) = term(129) + wmo_interm_cc3_35(c, d) * wmo_interm_cc3_33(c, d)
! term(130) = term(130) + wmo_interm_cc3_38(c, d) * wmo_interm_cc3_33(c, d)
! term(131) = term(131) + wmo_interm_cc3_36(c, d) * wmo_interm_cc3_33(c, d)
! end do 
! end do 

! term(126) = term(126) * (-0.33333333333333337d+0) 
! term(127) = term(127) * 0.6666666666666667d+0 
! term(128) = term(128) * (-0.33333333333333337d+0) 
! term(129) = term(129) * 0.6666666666666667d+0 
! term(130) = term(130) * 0.6666666666666667d+0 
! term(131) = term(131) * (-1.3333333333333335d+0) 

! do d = nocc + 1, nactive 
! do b = nocc + 1, nactive 
! term(132) = term(132) + wmo_interm_cc3_34(b, d) * wmo_interm_cc3_33(b, d)
! term(133) = term(133) + wmo_interm_cc3_32(b, d) * wmo_interm_cc3_33(b, d)
! term(134) = term(134) + wmo_interm_cc3_35(b, d) * wmo_interm_cc3_33(b, d)
! term(135) = term(135) + wmo_interm_cc3_37(b, d) * wmo_interm_cc3_33(b, d)
! term(136) = term(136) + wmo_interm_cc3_36(b, d) * wmo_interm_cc3_33(b, d)
! term(137) = term(137) + wmo_interm_cc3_38(b, d) * wmo_interm_cc3_33(b, d)
! end do 
! end do 

! term(132) = term(132) * 0.6666666666666667d+0 
! term(133) = term(133) * (-0.33333333333333337d+0) 
! term(134) = term(134) * 0.6666666666666667d+0 
! term(135) = term(135) * (-0.33333333333333337d+0) 
! term(136) = term(136) * (-1.3333333333333335d+0) 
! term(137) = term(137) * 0.6666666666666667d+0 

! do k = 1, nocc 
! do j = 1, nocc 
! do c = nocc + 1, nactive 
! do b = nocc + 1, nactive 
! term(138) = term(138) + wmo_interm_cc3_87(b, c, j, k) * wmo_interm_cc3_88(b, c, k, j)
! term(139) = term(139) + wmo_interm_cc3_87(b, c, j, k) * wmo_interm_cc3_89(b, c, k, j)
! term(140) = term(140) + wmo_interm_cc3_87(b, c, j, k) * wmo_interm_cc3_89(b, c, j, k)
! term(141) = term(141) + wmo_interm_cc3_87(b, c, j, k) * wmo_interm_cc3_90(b, c, k, j)
! term(142) = term(142) + wmo_interm_cc3_87(b, c, j, k) * wmo_interm_cc3_88(b, c, j, k)
! term(143) = term(143) + wmo_interm_cc3_87(b, c, j, k) * wmo_interm_cc3_90(b, c, j, k)
! end do 
! end do 
! end do 
! end do 

! term(138) = term(138) * 0.6666666666666667d+0 
! term(139) = term(139) * (-1.3333333333333335d+0) 
! term(140) = term(140) * 0.6666666666666667d+0 
! term(141) = term(141) * (-1.3333333333333335d+0) 
! term(142) = term(142) * (-1.3333333333333335d+0) 
! term(143) = term(143) * 2.666666666666667d+0 

! do j = 1, nocc 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! term(144) = term(144) + wmo_interm_cc3_87(a, b, i, j) * wmo_interm_cc3_88(a, b, j, i)
! term(145) = term(145) + wmo_interm_cc3_87(a, b, i, j) * wmo_interm_cc3_89(a, b, j, i)
! term(146) = term(146) + wmo_interm_cc3_87(a, b, i, j) * wmo_interm_cc3_89(a, b, i, j)
! term(147) = term(147) + wmo_interm_cc3_87(a, b, i, j) * wmo_interm_cc3_90(a, b, j, i)
! term(148) = term(148) + wmo_interm_cc3_87(a, b, i, j) * wmo_interm_cc3_88(a, b, i, j)
! term(149) = term(149) + wmo_interm_cc3_87(a, b, i, j) * wmo_interm_cc3_90(a, b, i, j)
! end do 
! end do 
! end do 
! end do 

! term(144) = term(144) * 0.6666666666666667d+0 
! term(145) = term(145) * (-1.3333333333333335d+0) 
! term(146) = term(146) * 0.6666666666666667d+0 
! term(147) = term(147) * (-1.3333333333333335d+0) 
! term(148) = term(148) * (-1.3333333333333335d+0) 
! term(149) = term(149) * 2.666666666666667d+0 

! do j = 1, nocc 
! do i = 1, nocc 
! do c = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! term(150) = term(150) + wmo_interm_cc3_91(a, c, i, j) * wmo_interm_cc3_88(a, c, j, i)
! term(151) = term(151) + wmo_interm_cc3_91(a, c, i, j) * wmo_interm_cc3_89(a, c, j, i)
! term(152) = term(152) + wmo_interm_cc3_91(a, c, i, j) * wmo_interm_cc3_89(a, c, i, j)
! term(153) = term(153) + wmo_interm_cc3_91(a, c, i, j) * wmo_interm_cc3_90(a, c, j, i)
! term(154) = term(154) + wmo_interm_cc3_91(a, c, i, j) * wmo_interm_cc3_88(a, c, i, j)
! term(155) = term(155) + wmo_interm_cc3_91(a, c, i, j) * wmo_interm_cc3_90(a, c, i, j)
! end do 
! end do 
! end do 
! end do 

! term(150) = term(150) * (-0.33333333333333337d+0) 
! term(151) = term(151) * 0.6666666666666667d+0 
! term(152) = term(152) * (-0.33333333333333337d+0) 
! term(153) = term(153) * 0.6666666666666667d+0 
! term(154) = term(154) * 0.6666666666666667d+0 
! term(155) = term(155) * (-1.3333333333333335d+0) 

! do k = 1, nocc 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! term(156) = term(156) + wmo_interm_cc3_91(a, b, i, k) * wmo_interm_cc3_88(a, b, k, i)
! term(157) = term(157) + wmo_interm_cc3_91(a, b, i, k) * wmo_interm_cc3_89(a, b, k, i)
! term(158) = term(158) + wmo_interm_cc3_91(a, b, i, k) * wmo_interm_cc3_89(a, b, i, k)
! term(159) = term(159) + wmo_interm_cc3_91(a, b, i, k) * wmo_interm_cc3_90(a, b, k, i)
! term(160) = term(160) + wmo_interm_cc3_91(a, b, i, k) * wmo_interm_cc3_88(a, b, i, k)
! term(161) = term(161) + wmo_interm_cc3_91(a, b, i, k) * wmo_interm_cc3_90(a, b, i, k)
! end do 
! end do 
! end do 
! end do 

! term(156) = term(156) * (-0.33333333333333337d+0) 
! term(157) = term(157) * 0.6666666666666667d+0 
! term(158) = term(158) * (-0.33333333333333337d+0) 
! term(159) = term(159) * 0.6666666666666667d+0 
! term(160) = term(160) * 0.6666666666666667d+0 
! term(161) = term(161) * (-1.3333333333333335d+0) 

! do l = 1, nocc 
! do j = 1, nocc 
! term(162) = term(162) + wmo_interm_cc3_39(j, l) * wmo_interm_cc3_40(j, l)
! term(163) = term(163) + wmo_interm_cc3_41(j, l) * wmo_interm_cc3_40(j, l)
! term(164) = term(164) + wmo_interm_cc3_42(j, l) * wmo_interm_cc3_40(j, l)
! term(165) = term(165) + wmo_interm_cc3_43(j, l) * wmo_interm_cc3_40(j, l)
! term(166) = term(166) + wmo_interm_cc3_45(j, l) * wmo_interm_cc3_40(j, l)
! term(167) = term(167) + wmo_interm_cc3_44(j, l) * wmo_interm_cc3_40(j, l)
! end do 
! end do 

! term(162) = term(162) * (-0.33333333333333337d+0) 
! term(163) = term(163) * 0.6666666666666667d+0 
! term(164) = term(164) * (-0.33333333333333337d+0) 
! term(165) = term(165) * 0.6666666666666667d+0 
! term(166) = term(166) * 0.6666666666666667d+0 
! term(167) = term(167) * (-1.3333333333333335d+0) 

! do l = 1, nocc 
! do k = 1, nocc 
! term(168) = term(168) + wmo_interm_cc3_41(k, l) * wmo_interm_cc3_40(k, l)
! term(169) = term(169) + wmo_interm_cc3_39(k, l) * wmo_interm_cc3_40(k, l)
! term(170) = term(170) + wmo_interm_cc3_43(k, l) * wmo_interm_cc3_40(k, l)
! term(171) = term(171) + wmo_interm_cc3_42(k, l) * wmo_interm_cc3_40(k, l)
! term(172) = term(172) + wmo_interm_cc3_44(k, l) * wmo_interm_cc3_40(k, l)
! term(173) = term(173) + wmo_interm_cc3_45(k, l) * wmo_interm_cc3_40(k, l)
! end do 
! end do 

! term(168) = term(168) * 0.6666666666666667d+0 
! term(169) = term(169) * (-0.33333333333333337d+0) 
! term(170) = term(170) * 0.6666666666666667d+0 
! term(171) = term(171) * (-0.33333333333333337d+0) 
! term(172) = term(172) * (-1.3333333333333335d+0) 
! term(173) = term(173) * 0.6666666666666667d+0 

! do k = 1, nocc 
! do i = 1, nocc 
! do c = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! term(174) = term(174) + wmo_interm_cc3_87(a, c, i, k) * wmo_interm_cc3_88(a, c, k, i)
! term(175) = term(175) + wmo_interm_cc3_87(a, c, i, k) * wmo_interm_cc3_89(a, c, k, i)
! term(176) = term(176) + wmo_interm_cc3_87(a, c, i, k) * wmo_interm_cc3_89(a, c, i, k)
! term(177) = term(177) + wmo_interm_cc3_87(a, c, i, k) * wmo_interm_cc3_90(a, c, k, i)
! term(178) = term(178) + wmo_interm_cc3_87(a, c, i, k) * wmo_interm_cc3_88(a, c, i, k)
! term(179) = term(179) + wmo_interm_cc3_87(a, c, i, k) * wmo_interm_cc3_90(a, c, i, k)
! end do 
! end do 
! end do 
! end do 

! term(174) = term(174) * 0.6666666666666667d+0 
! term(175) = term(175) * (-1.3333333333333335d+0) 
! term(176) = term(176) * 0.6666666666666667d+0 
! term(177) = term(177) * (-1.3333333333333335d+0) 
! term(178) = term(178) * (-1.3333333333333335d+0) 
! term(179) = term(179) * 2.666666666666667d+0 

! do k = 1, nocc 
! do i = 1, nocc 
! do c = nocc + 1, nactive 
! do b = nocc + 1, nactive 
! term(180) = term(180) + wmo_interm_cc3_92(b, c, i, k) * wmo_interm_cc3_88(b, c, k, i)
! term(181) = term(181) + wmo_interm_cc3_92(b, c, i, k) * wmo_interm_cc3_89(b, c, k, i)
! term(182) = term(182) + wmo_interm_cc3_92(b, c, i, k) * wmo_interm_cc3_89(b, c, i, k)
! term(183) = term(183) + wmo_interm_cc3_92(b, c, i, k) * wmo_interm_cc3_90(b, c, k, i)
! term(184) = term(184) + wmo_interm_cc3_92(b, c, i, k) * wmo_interm_cc3_88(b, c, i, k)
! term(185) = term(185) + wmo_interm_cc3_92(b, c, i, k) * wmo_interm_cc3_90(b, c, i, k)
! end do 
! end do 
! end do 
! end do 

! term(180) = term(180) * (-0.33333333333333337d+0) 
! term(181) = term(181) * 0.6666666666666667d+0 
! term(182) = term(182) * (-0.33333333333333337d+0) 
! term(183) = term(183) * 0.6666666666666667d+0 
! term(184) = term(184) * 0.6666666666666667d+0 
! term(185) = term(185) * (-1.3333333333333335d+0) 

! do j = 1, nocc 
! do i = 1, nocc 
! do c = nocc + 1, nactive 
! do b = nocc + 1, nactive 
! term(186) = term(186) + wmo_interm_cc3_93(b, c, i, j) * wmo_interm_cc3_88(b, c, j, i)
! term(187) = term(187) + wmo_interm_cc3_93(b, c, i, j) * wmo_interm_cc3_89(b, c, j, i)
! term(188) = term(188) + wmo_interm_cc3_93(b, c, i, j) * wmo_interm_cc3_89(b, c, i, j)
! term(189) = term(189) + wmo_interm_cc3_93(b, c, i, j) * wmo_interm_cc3_88(b, c, i, j)
! term(190) = term(190) + wmo_interm_cc3_93(b, c, i, j) * wmo_interm_cc3_90(b, c, j, i)
! term(191) = term(191) + wmo_interm_cc3_93(b, c, i, j) * wmo_interm_cc3_90(b, c, i, j)
! end do 
! end do 
! end do 
! end do 

! term(186) = term(186) * 0.6666666666666667d+0 
! term(187) = term(187) * (-0.33333333333333337d+0) 
! term(188) = term(188) * 0.6666666666666667d+0 
! term(189) = term(189) * (-0.33333333333333337d+0) 
! term(190) = term(190) * (-1.3333333333333335d+0) 
! term(191) = term(191) * 0.6666666666666667d+0 

! do j = 1, nocc 
! do b = nocc + 1, nactive 
! term(192) = term(192) + wmo_interm_cc3_46(b, j) * wmo_interm_cc3_47(b, j)
! term(193) = term(193) + wmo_interm_cc3_46(b, j) * wmo_interm_cc3_48(b, j)
! term(194) = term(194) + wmo_interm_cc3_46(b, j) * wmo_interm_cc3_47(b, j)
! term(195) = term(195) + wmo_interm_cc3_46(b, j) * wmo_interm_cc3_48(b, j)
! term(196) = term(196) + wmo_interm_cc3_46(b, j) * wmo_interm_cc3_49(b, j)
! term(197) = term(197) + wmo_interm_cc3_46(b, j) * wmo_interm_cc3_50(b, j)
! end do 
! end do 

! term(193) = term(193) * (-1.9999999999999998d+0) 
! term(195) = term(195) * (-1.9999999999999998d+0) 
! term(196) = term(196) * 3.9999999999999996d+0 
! term(197) = term(197) * (-1.9999999999999998d+0) 

! do k = 1, nocc 
! do j = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! term(198) = term(198) + wmo_interm_cc3_94(a, b, j, k) * wmo_interm_cc3_89(a, b, k, j)
! term(199) = term(199) + wmo_interm_cc3_94(a, b, j, k) * wmo_interm_cc3_89(a, b, j, k)
! term(200) = term(200) + wmo_interm_cc3_94(a, b, j, k) * wmo_interm_cc3_88(a, b, j, k)
! term(201) = term(201) + wmo_interm_cc3_94(a, b, j, k) * wmo_interm_cc3_90(a, b, j, k)
! term(202) = term(202) + wmo_interm_cc3_94(a, b, j, k) * wmo_interm_cc3_88(a, b, k, j)
! term(203) = term(203) + wmo_interm_cc3_94(a, b, j, k) * wmo_interm_cc3_90(a, b, k, j)
! end do 
! end do 
! end do 
! end do 

! term(198) = term(198) * (-0.33333333333333337d+0) 
! term(199) = term(199) * 0.6666666666666667d+0 
! term(200) = term(200) * (-0.33333333333333337d+0) 
! term(201) = term(201) * 0.6666666666666667d+0 
! term(202) = term(202) * 0.6666666666666667d+0 
! term(203) = term(203) * (-1.3333333333333335d+0) 

! do k = 1, nocc 
! do j = 1, nocc 
! do c = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! term(204) = term(204) + wmo_interm_cc3_92(a, c, j, k) * wmo_interm_cc3_89(a, c, k, j)
! term(205) = term(205) + wmo_interm_cc3_92(a, c, j, k) * wmo_interm_cc3_89(a, c, j, k)
! term(206) = term(206) + wmo_interm_cc3_92(a, c, j, k) * wmo_interm_cc3_88(a, c, j, k)
! term(207) = term(207) + wmo_interm_cc3_92(a, c, j, k) * wmo_interm_cc3_90(a, c, j, k)
! term(208) = term(208) + wmo_interm_cc3_92(a, c, j, k) * wmo_interm_cc3_88(a, c, k, j)
! term(209) = term(209) + wmo_interm_cc3_92(a, c, j, k) * wmo_interm_cc3_90(a, c, k, j)
! end do 
! end do 
! end do 
! end do 

! term(204) = term(204) * 0.6666666666666667d+0 
! term(205) = term(205) * (-0.33333333333333337d+0) 
! term(206) = term(206) * 0.6666666666666667d+0 
! term(207) = term(207) * (-1.3333333333333335d+0) 
! term(208) = term(208) * (-0.33333333333333337d+0) 
! term(209) = term(209) * 0.6666666666666667d+0 

! do k = 1, nocc 
! do i = 1, nocc 
! do j = 1, nocc 
! do a = nocc + 1, nactive 
! term(210) = term(210) + wmo_interm_cc3_51(a, i, j, k) * wmo_interm_cc3_52(a, k, j, i)
! term(211) = term(211) + wmo_interm_cc3_51(a, i, j, k) * wmo_interm_cc3_57(a, k, j, i)
! term(212) = term(212) + wmo_interm_cc3_51(a, i, j, k) * wmo_interm_cc3_56(a, k, j, i)
! term(213) = term(213) + wmo_interm_cc3_51(a, i, j, k) * wmo_interm_cc3_55(a, k, j, i)
! term(214) = term(214) + wmo_interm_cc3_51(a, i, j, k) * wmo_interm_cc3_57(a, k, j, i)
! term(215) = term(215) + wmo_interm_cc3_51(a, i, j, k) * wmo_interm_cc3_52(a, k, j, i)
! end do 
! end do 
! end do 
! end do 

! term(210) = term(210) * (-0.5833333333333333d+0) 
! term(212) = term(212) * (-0.49999999999999994d+0) 
! term(215) = term(215) * (-0.41666666666666663d+0) 

! do i = 1, nocc 
! do a = nocc + 1, nactive 
! term(216) = term(216) + wmo_interm_cc3_46(a, i) * wmo_interm_cc3_47(a, i)
! term(217) = term(217) + wmo_interm_cc3_46(a, i) * wmo_interm_cc3_48(a, i)
! term(218) = term(218) + wmo_interm_cc3_46(a, i) * wmo_interm_cc3_47(a, i)
! term(219) = term(219) + wmo_interm_cc3_46(a, i) * wmo_interm_cc3_48(a, i)
! term(220) = term(220) + wmo_interm_cc3_46(a, i) * wmo_interm_cc3_49(a, i)
! term(221) = term(221) + wmo_interm_cc3_46(a, i) * wmo_interm_cc3_50(a, i)
! end do 
! end do 

! term(217) = term(217) * (-1.9999999999999998d+0) 
! term(219) = term(219) * (-1.9999999999999998d+0) 
! term(220) = term(220) * 3.9999999999999996d+0 
! term(221) = term(221) * (-1.9999999999999998d+0) 

! do i = 1, nocc 
! do b = nocc + 1, nactive 
! term(222) = term(222) + wmo_interm_cc3_53(b, i) * wmo_interm_cc3_47(b, i)
! term(223) = term(223) + wmo_interm_cc3_53(b, i) * wmo_interm_cc3_48(b, i)
! term(224) = term(224) + wmo_interm_cc3_53(b, i) * wmo_interm_cc3_49(b, i)
! term(225) = term(225) + wmo_interm_cc3_53(b, i) * wmo_interm_cc3_50(b, i)
! term(226) = term(226) + wmo_interm_cc3_53(b, i) * wmo_interm_cc3_48(b, i)
! term(227) = term(227) + wmo_interm_cc3_53(b, i) * wmo_interm_cc3_47(b, i)
! end do 
! end do 

! term(222) = term(222) * (-0.49999999999999994d+0) 
! term(224) = term(224) * (-1.9999999999999998d+0) 
! term(227) = term(227) * (-0.5d+0) 

! do k = 1, nocc 
! do j = 1, nocc 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! term(228) = term(228) + wmo_interm_cc3_51(a, i, j, k) * wmo_interm_cc3_55(a, k, i, j)
! term(229) = term(229) + wmo_interm_cc3_51(a, i, j, k) * wmo_interm_cc3_56(a, k, i, j)
! term(230) = term(230) + wmo_interm_cc3_51(a, i, j, k) * wmo_interm_cc3_52(a, k, i, j)
! term(231) = term(231) + wmo_interm_cc3_51(a, i, j, k) * wmo_interm_cc3_57(a, k, i, j)
! term(232) = term(232) + wmo_interm_cc3_51(a, i, j, k) * wmo_interm_cc3_57(a, k, i, j)
! term(233) = term(233) + wmo_interm_cc3_51(a, i, j, k) * wmo_interm_cc3_52(a, k, i, j)
! end do 
! end do 
! end do 
! end do 

! term(228) = term(228) * (-0.49999999999999994d+0) 
! term(230) = term(230) * 1.1666666666666665d+0 
! term(231) = term(231) * (-1.9999999999999998d+0) 
! term(232) = term(232) * (-1.9999999999999998d+0) 
! term(233) = term(233) * 0.8333333333333333d+0 

! do k = 1, nocc 
! do j = 1, nocc 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! term(234) = term(234) + wmo_interm_cc3_54(b, i, j, k) * wmo_interm_cc3_56(b, k, i, j)
! term(235) = term(235) + wmo_interm_cc3_54(b, i, j, k) * wmo_interm_cc3_55(b, k, i, j)
! term(236) = term(236) + wmo_interm_cc3_54(b, i, j, k) * wmo_interm_cc3_52(b, k, i, j)
! term(237) = term(237) + wmo_interm_cc3_54(b, i, j, k) * wmo_interm_cc3_57(b, k, i, j)
! term(238) = term(238) + wmo_interm_cc3_54(b, i, j, k) * wmo_interm_cc3_57(b, k, i, j)
! term(239) = term(239) + wmo_interm_cc3_54(b, i, j, k) * wmo_interm_cc3_52(b, k, i, j)
! end do 
! end do 
! end do 
! end do 

! term(234) = term(234) * (-0.49999999999999994d+0) 
! term(236) = term(236) * (-0.5833333333333333d+0) 
! term(239) = term(239) * (-0.41666666666666663d+0) 

! do l = 1, nocc 
! do k = 1, nocc 
! do j = 1, nocc 
! do d = nocc + 1, nactive 
! term(240) = term(240) + wmo_interm_cc3_60(d, j, k, l) * wmo_interm_cc3_61(d, j, l, k)
! term(241) = term(241) + wmo_interm_cc3_64(d, j, k, l) * wmo_interm_cc3_61(d, j, l, k)
! term(242) = term(242) + wmo_interm_cc3_67(d, j, k, l) * wmo_interm_cc3_61(d, j, l, k)
! end do 
! end do 
! end do 
! end do 

! term(240) = term(240) * 2.0d+0 
! term(241) = -term(241) 
! term(242) = term(242) * 2.0d+0 

! do k = 1, nocc 
! do l = 1, nocc 
! do j = 1, nocc 
! do d = nocc + 1, nactive 
! term(243) = term(243) + wmo_interm_cc3_60(d, j, l, k) * wmo_interm_cc3_61(d, j, l, k)
! term(244) = term(244) + wmo_interm_cc3_64(d, j, l, k) * wmo_interm_cc3_61(d, j, l, k)
! term(245) = term(245) + wmo_interm_cc3_67(d, j, l, k) * wmo_interm_cc3_61(d, j, l, k)
! end do 
! end do 
! end do 
! end do 

! term(243) = -term(243) 
! term(244) = term(244) * 2.0d+0 
! term(245) = term(245) * (-4.0d+0) 

! do l = 1, nocc 
! do k = 1, nocc 
! do d = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! term(246) = term(246) + wmo_interm_cc3_95(c, d, k, l) * wmo_interm_cc3_96(c, d, l, k)
! term(247) = term(247) + wmo_interm_cc3_95(c, d, k, l) * wmo_interm_cc3_97(c, d, l, k)
! term(248) = term(248) + wmo_interm_cc3_95(c, d, l, k) * wmo_interm_cc3_98(c, d, k, l)
! term(249) = term(249) + wmo_interm_cc3_95(c, d, k, l) * wmo_interm_cc3_98(c, d, k, l)
! term(250) = term(250) + wmo_interm_cc3_99(c, d, k, l) * wmo_interm_cc3_97(c, d, l, k)
! term(251) = term(251) + wmo_interm_cc3_99(c, d, k, l) * wmo_interm_cc3_98(c, d, k, l)
! term(252) = term(252) + wmo_interm_cc3_99(c, d, k, l) * wmo_interm_cc3_96(c, d, l, k)
! term(253) = term(253) + wmo_interm_cc3_99(c, d, l, k) * wmo_interm_cc3_98(c, d, k, l)
! term(254) = term(254) + wmo_interm_cc3_100(c, d, k, l) * wmo_interm_cc3_97(c, d, l, k)
! term(255) = term(255) + wmo_interm_cc3_100(c, d, k, l) * wmo_interm_cc3_98(c, d, k, l)
! term(256) = term(256) + wmo_interm_cc3_100(c, d, k, l) * wmo_interm_cc3_96(c, d, l, k)
! term(257) = term(257) + wmo_interm_cc3_100(c, d, l, k) * wmo_interm_cc3_98(c, d, k, l)
! end do 
! end do 
! end do 
! end do 

! term(246) = -term(246) 
! term(247) = term(247) * 2.0000000000000004d+0 
! term(248) = term(248) * 2.0000000000000004d+0 
! term(249) = term(249) * (-4.000000000000001d+0) 
! term(250) = -term(250) 
! term(251) = term(251) * 2.0000000000000004d+0 
! term(252) = term(252) * 2.0000000000000004d+0 
! term(253) = term(253) * (-4.000000000000001d+0) 
! term(254) = term(254) * (-4.666666666666668d+0) 
! term(255) = term(255) * 8.000000000000002d+0 
! term(256) = term(256) * 2.0000000000000004d+0 
! term(257) = term(257) * (-4.000000000000001d+0) 

! do k = 1, nocc 
! do l = 1, nocc 
! do d = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! term(258) = term(258) + wmo_interm_cc3_95(c, d, l, k) * wmo_interm_cc3_97(c, d, l, k)
! term(259) = term(259) + wmo_interm_cc3_95(c, d, l, k) * wmo_interm_cc3_96(c, d, l, k)
! term(260) = term(260) + wmo_interm_cc3_99(c, d, l, k) * wmo_interm_cc3_96(c, d, l, k)
! term(261) = term(261) + wmo_interm_cc3_99(c, d, l, k) * wmo_interm_cc3_97(c, d, l, k)
! term(262) = term(262) + wmo_interm_cc3_100(c, d, l, k) * wmo_interm_cc3_97(c, d, l, k)
! term(263) = term(263) + wmo_interm_cc3_100(c, d, l, k) * wmo_interm_cc3_96(c, d, l, k)
! end do 
! end do 
! end do 
! end do 

! term(258) = -term(258) 
! term(259) = term(259) * 2.0000000000000004d+0 
! term(260) = -term(260) 
! term(261) = term(261) * 2.0000000000000004d+0 
! term(262) = term(262) * 2.0000000000000004d+0 
! term(263) = term(263) * (-3.333333333333334d+0) 


!     d_overlap_cc3 = 0.d+0
!     do s = 0, 263
!     d_overlap_cc3 = d_overlap_cc3 + term(s)
!     end do                                                                                                  
!     end function d_overlap_cc3

   function d_overlap_ccsd_triplet(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr)
    double precision :: d_overlap_ccsd_triplet
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


    d_overlap_ccsd_triplet = 0.d+0
    do s = 0, 355
    d_overlap_ccsd_triplet = d_overlap_ccsd_triplet + term(s)
    end do                                                                                                  


                                                                                               

!    integer :: s ,j,i,b,a,k,c,l,d
!    double precision, dimension(0:434) :: term
!     term = 0.d+0
!     do j = 1, nocc 
! do i = 1, nocc 
! term(0) = term(0) + wmo_interm_0_triplet(i, j) * wmo_interm_1_triplet(j, i)
! term(1) = term(1) + wmo_interm_2_triplet(i, j) * wmo_interm_3_triplet(i, j)
! term(2) = term(2) + wmo_interm_0_triplet(i, j) * wmo_interm_54_triplet(j, i)
! term(3) = term(3) + wmo_interm_0_triplet(i, j) * wmo_interm_55_triplet(j, i)
! term(4) = term(4) + wmo_interm_0_triplet(i, j) * wmo_interm_54_triplet(j, i)
! term(5) = term(5) + wmo_interm_0_triplet(i, j) * wmo_interm_55_triplet(j, i)
! end do 
! end do 

! term(0) = term(0) * (-2.0d+0) 
! term(1) = term(1) * (-2.0d+0) 
! term(3) = term(3) * (-2.0d+0) 
! term(5) = term(5) * (-2.0d+0) 

! do d = nocc + 1, nactive 
! do k = 1, nocc 
! do a = nocc + 1, nactive 
! term(6) = term(6) + s1(d,k) * wmo_interm_51_triplet(a, d) * wmo_interm_5_triplet(a, k)
! term(7) = term(7) + s1(d,k) * wmo_interm_49_triplet(a, d) * wmo_interm_5_triplet(a, k)
! term(8) = term(8) + s1(d,k) * wmo_interm_51_triplet(a, d) * wmo_interm_6_triplet(a, k)
! term(9) = term(9) + s1(d,k) * wmo_interm_49_triplet(a, d) * wmo_interm_6_triplet(a, k)
! end do 
! end do 
! end do 

! term(6) = term(6) * 12.0d+0 
! term(7) = term(7) * (-6.0d+0) 
! term(8) = term(8) * (-16.0d+0) 
! term(9) = term(9) * 8.0d+0 

! do j = 1, nocc 
! do b = nocc + 1, nactive 
! term(10) = term(10) + wmo_interm_4_triplet(b, j) * wmo_interm_5_triplet(b, j)
! term(11) = term(11) + wmo_interm_4_triplet(b, j) * wmo_interm_6_triplet(b, j)
! term(12) = term(12) + wmo_interm_4_triplet(b, j) * wmo_interm_7_triplet(b, j)
! term(13) = term(13) + wmo_interm_8_triplet(b, j) * wmo_interm_5_triplet(b, j)
! term(14) = term(14) + wmo_interm_8_triplet(b, j) * wmo_interm_6_triplet(b, j)
! term(15) = term(15) + wmo_interm_8_triplet(b, j) * wmo_interm_7_triplet(b, j)
! term(16) = term(16) + wmo_interm_48_triplet(b, j) * wmo_interm_41_triplet(b, j)
! term(17) = term(17) + wmo_interm_48_triplet(b, j) * wmo_interm_42_triplet(b, j)
! end do 
! end do 

! term(10) = term(10) * (-18.0d+0) 
! term(11) = term(11) * (-24.0d+0) 
! term(12) = term(12) * 12.0d+0 
! term(13) = term(13) * (-12.0d+0) 
! term(14) = term(14) * (-16.0d+0) 
! term(15) = term(15) * 8.0d+0 
! term(16) = term(16) * (-4.0d+0) 
! term(17) = term(17) * 8.0d+0 

! do i = 1, nocc 
! do a = nocc + 1, nactive 
! term(18) = term(18) + wmo_interm_8_triplet(a, i) * wmo_interm_5_triplet(a, i)
! term(19) = term(19) + wmo_interm_8_triplet(a, i) * wmo_interm_6_triplet(a, i)
! term(20) = term(20) + wmo_interm_8_triplet(a, i) * wmo_interm_7_triplet(a, i)
! term(21) = term(21) + wmo_interm_40_triplet(a, i) * wmo_interm_41_triplet(a, i)
! term(22) = term(22) + wmo_interm_40_triplet(a, i) * wmo_interm_42_triplet(a, i)
! term(23) = term(23) + wmo_interm_48_triplet(a, i) * wmo_interm_41_triplet(a, i)
! term(24) = term(24) + wmo_interm_48_triplet(a, i) * wmo_interm_42_triplet(a, i)
! term(25) = term(25) + r1(vrdav_Rl, a,i) * r1(vrdav_Rr, a,i)
! end do 
! end do 

! term(18) = term(18) * (-12.0d+0) 
! term(19) = term(19) * (-16.0d+0) 
! term(20) = term(20) * 8.0d+0 
! term(21) = term(21) * (-6.0d+0) 
! term(22) = term(22) * 12.0d+0 
! term(23) = term(23) * (-4.0d+0) 
! term(24) = term(24) * 8.0d+0 
! term(25) = term(25) * 2.0d+0 

! do j = 1, nocc 
! do a = nocc + 1, nactive 
! term(26) = term(26) + wmo_interm_9_triplet(a, j) * wmo_interm_5_triplet(a, j)
! term(27) = term(27) + wmo_interm_9_triplet(a, j) * wmo_interm_6_triplet(a, j)
! term(28) = term(28) + wmo_interm_9_triplet(a, j) * wmo_interm_7_triplet(a, j)
! term(29) = term(29) + wmo_interm_50_triplet(a, j) * wmo_interm_41_triplet(a, j)
! term(30) = term(30) + wmo_interm_50_triplet(a, j) * wmo_interm_42_triplet(a, j)
! end do 
! end do 

! term(26) = term(26) * 6.0d+0 
! term(27) = term(27) * 8.0d+0 
! term(28) = term(28) * (-4.0d+0) 
! term(29) = term(29) * 2.0d+0 
! term(30) = term(30) * (-4.0d+0) 

! do i = 1, nocc 
! do b = nocc + 1, nactive 
! term(31) = term(31) + wmo_interm_9_triplet(b, i) * wmo_interm_5_triplet(b, i)
! term(32) = term(32) + wmo_interm_9_triplet(b, i) * wmo_interm_6_triplet(b, i)
! term(33) = term(33) + wmo_interm_9_triplet(b, i) * wmo_interm_7_triplet(b, i)
! term(34) = term(34) + wmo_interm_50_triplet(b, i) * wmo_interm_41_triplet(b, i)
! term(35) = term(35) + wmo_interm_50_triplet(b, i) * wmo_interm_42_triplet(b, i)
! end do 
! end do 

! term(31) = term(31) * 6.0d+0 
! term(32) = term(32) * 8.0d+0 
! term(33) = term(33) * (-4.0d+0) 
! term(34) = term(34) * 2.0d+0 
! term(35) = term(35) * (-4.0d+0) 

! do l = 1, nocc 
! do i = 1, nocc 
! do d = nocc + 1, nactive 
! term(36) = term(36) + s1(d,l) * wmo_interm_53_triplet(i, l) * wmo_interm_5_triplet(d, i)
! term(37) = term(37) + s1(d,l) * wmo_interm_52_triplet(i, l) * wmo_interm_5_triplet(d, i)
! term(38) = term(38) + s1(d,l) * wmo_interm_53_triplet(i, l) * wmo_interm_7_triplet(d, i)
! term(39) = term(39) + s1(d,l) * wmo_interm_53_triplet(i, l) * wmo_interm_6_triplet(d, i)
! term(40) = term(40) + s1(d,l) * wmo_interm_52_triplet(i, l) * wmo_interm_7_triplet(d, i)
! term(41) = term(41) + s1(d,l) * wmo_interm_52_triplet(i, l) * wmo_interm_6_triplet(d, i)
! end do 
! end do 
! end do 

! term(36) = term(36) * (-6.0d+0) 
! term(37) = term(37) * 3.0d+0 
! term(38) = term(38) * 4.0d+0 
! term(39) = term(39) * (-8.0d+0) 
! term(40) = term(40) * (-2.0d+0) 
! term(41) = term(41) * 4.0d+0 

! do k = 1, nocc 
! do i = 1, nocc 
! term(42) = term(42) + wmo_interm_10_triplet(i, k) * wmo_interm_1_triplet(k, i)
! term(43) = term(43) + wmo_interm_13_triplet(i, k) * wmo_interm_1_triplet(k, i)
! term(44) = term(44) + wmo_interm_14_triplet(i, k) * wmo_interm_1_triplet(k, i)
! term(45) = term(45) + wmo_interm_19_triplet(i, k) * wmo_interm_1_triplet(k, i)
! term(46) = term(46) + wmo_interm_25_triplet(i, k) * wmo_interm_1_triplet(k, i)
! term(47) = term(47) + wmo_interm_26_triplet(i, k) * wmo_interm_1_triplet(k, i)
! term(48) = term(48) + wmo_interm_2_triplet(i, k) * wmo_interm_32_triplet(i, k)
! term(49) = term(49) + wmo_interm_2_triplet(i, k) * wmo_interm_33_triplet(i, k)
! term(50) = term(50) + wmo_interm_2_triplet(i, k) * wmo_interm_36_triplet(i, k)
! term(51) = term(51) + wmo_interm_2_triplet(i, k) * wmo_interm_37_triplet(i, k)
! term(52) = term(52) + wmo_interm_2_triplet(i, k) * wmo_interm_38_triplet(i, k)
! term(53) = term(53) + wmo_interm_2_triplet(i, k) * wmo_interm_39_triplet(i, k)
! term(54) = term(54) + wmo_interm_53_triplet(i, k) * wmo_interm_3_triplet(i, k)
! term(55) = term(55) + wmo_interm_52_triplet(i, k) * wmo_interm_3_triplet(i, k)
! term(56) = term(56) + wmo_interm_10_triplet(i, k) * wmo_interm_54_triplet(k, i)
! term(57) = term(57) + wmo_interm_10_triplet(i, k) * wmo_interm_55_triplet(k, i)
! term(58) = term(58) + wmo_interm_10_triplet(i, k) * wmo_interm_54_triplet(k, i)
! term(59) = term(59) + wmo_interm_10_triplet(i, k) * wmo_interm_55_triplet(k, i)
! term(60) = term(60) + wmo_interm_13_triplet(i, k) * wmo_interm_54_triplet(k, i)
! term(61) = term(61) + wmo_interm_14_triplet(i, k) * wmo_interm_54_triplet(k, i)
! term(62) = term(62) + wmo_interm_13_triplet(i, k) * wmo_interm_55_triplet(k, i)
! term(63) = term(63) + wmo_interm_14_triplet(i, k) * wmo_interm_55_triplet(k, i)
! term(64) = term(64) + wmo_interm_13_triplet(i, k) * wmo_interm_54_triplet(k, i)
! term(65) = term(65) + wmo_interm_14_triplet(i, k) * wmo_interm_54_triplet(k, i)
! term(66) = term(66) + wmo_interm_13_triplet(i, k) * wmo_interm_55_triplet(k, i)
! term(67) = term(67) + wmo_interm_14_triplet(i, k) * wmo_interm_55_triplet(k, i)
! term(68) = term(68) + wmo_interm_19_triplet(i, k) * wmo_interm_54_triplet(k, i)
! term(69) = term(69) + wmo_interm_19_triplet(i, k) * wmo_interm_55_triplet(k, i)
! term(70) = term(70) + wmo_interm_19_triplet(i, k) * wmo_interm_55_triplet(k, i)
! term(71) = term(71) + wmo_interm_19_triplet(i, k) * wmo_interm_54_triplet(k, i)
! term(72) = term(72) + wmo_interm_25_triplet(i, k) * wmo_interm_54_triplet(k, i)
! term(73) = term(73) + wmo_interm_26_triplet(i, k) * wmo_interm_54_triplet(k, i)
! term(74) = term(74) + wmo_interm_25_triplet(i, k) * wmo_interm_55_triplet(k, i)
! term(75) = term(75) + wmo_interm_26_triplet(i, k) * wmo_interm_55_triplet(k, i)
! term(76) = term(76) + wmo_interm_25_triplet(i, k) * wmo_interm_55_triplet(k, i)
! term(77) = term(77) + wmo_interm_26_triplet(i, k) * wmo_interm_55_triplet(k, i)
! term(78) = term(78) + wmo_interm_25_triplet(i, k) * wmo_interm_54_triplet(k, i)
! term(79) = term(79) + wmo_interm_26_triplet(i, k) * wmo_interm_54_triplet(k, i)
! end do 
! end do 

! term(42) = term(42) * (-6.0d+0) 
! term(43) = term(43) * (-6.0d+0) 
! term(44) = term(44) * 6.0d+0 
! term(45) = term(45) * (-6.0d+0) 
! term(46) = term(46) * 4.0d+0 
! term(47) = term(47) * (-8.0d+0) 
! term(48) = term(48) * (-3.0d+0) 
! term(49) = term(49) * 3.0d+0 
! term(50) = term(50) * 2.0d+0 
! term(51) = term(51) * (-4.0d+0) 
! term(52) = term(52) * 2.0d+0 
! term(53) = term(53) * (-4.0d+0) 
! term(54) = term(54) * (-4.0d+0) 
! term(55) = term(55) * 2.0d+0 
! term(56) = term(56) * 3.0d+0 
! term(57) = term(57) * (-6.0d+0) 
! term(58) = term(58) * 3.0d+0 
! term(59) = term(59) * (-6.0d+0) 
! term(60) = term(60) * 3.0d+0 
! term(61) = term(61) * (-3.0d+0) 
! term(62) = term(62) * (-6.0d+0) 
! term(63) = term(63) * 6.0d+0 
! term(64) = term(64) * 3.0d+0 
! term(65) = term(65) * (-3.0d+0) 
! term(66) = term(66) * (-6.0d+0) 
! term(67) = term(67) * 6.0d+0 
! term(68) = term(68) * 3.0d+0 
! term(69) = term(69) * (-6.0d+0) 
! term(70) = term(70) * (-6.0d+0) 
! term(71) = term(71) * 3.0d+0 
! term(72) = term(72) * (-2.0d+0) 
! term(73) = term(73) * 4.0d+0 
! term(74) = term(74) * 4.0d+0 
! term(75) = term(75) * (-8.0d+0) 
! term(76) = term(76) * 4.0d+0 
! term(77) = term(77) * (-8.0d+0) 
! term(78) = term(78) * (-2.0d+0) 
! term(79) = term(79) * 4.0d+0 

! do c = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! term(80) = term(80) + wmo_interm_11_triplet(a, c) * wmo_interm_12_triplet(c, a)
! term(81) = term(81) + wmo_interm_15_triplet(a, c) * wmo_interm_12_triplet(c, a)
! term(82) = term(82) + wmo_interm_16_triplet(a, c) * wmo_interm_12_triplet(c, a)
! term(83) = term(83) + wmo_interm_20_triplet(a, c) * wmo_interm_12_triplet(c, a)
! term(84) = term(84) + wmo_interm_27_triplet(a, c) * wmo_interm_12_triplet(c, a)
! term(85) = term(85) + wmo_interm_28_triplet(a, c) * wmo_interm_12_triplet(c, a)
! term(86) = term(86) + wmo_interm_29_triplet(a, c) * wmo_interm_30_triplet(a, c)
! term(87) = term(87) + wmo_interm_29_triplet(a, c) * wmo_interm_31_triplet(a, c)
! term(88) = term(88) + wmo_interm_29_triplet(a, c) * wmo_interm_34_triplet(a, c)
! term(89) = term(89) + wmo_interm_29_triplet(a, c) * wmo_interm_35_triplet(a, c)
! term(90) = term(90) + wmo_interm_51_triplet(a, c) * wmo_interm_44_triplet(a, c)
! term(91) = term(91) + wmo_interm_49_triplet(a, c) * wmo_interm_44_triplet(a, c)
! term(92) = term(92) + wmo_interm_11_triplet(a, c) * wmo_interm_60_triplet(c, a)
! term(93) = term(93) + wmo_interm_11_triplet(a, c) * wmo_interm_61_triplet(c, a)
! term(94) = term(94) + wmo_interm_15_triplet(a, c) * wmo_interm_60_triplet(c, a)
! term(95) = term(95) + wmo_interm_16_triplet(a, c) * wmo_interm_60_triplet(c, a)
! term(96) = term(96) + wmo_interm_15_triplet(a, c) * wmo_interm_61_triplet(c, a)
! term(97) = term(97) + wmo_interm_16_triplet(a, c) * wmo_interm_61_triplet(c, a)
! term(98) = term(98) + wmo_interm_20_triplet(a, c) * wmo_interm_60_triplet(c, a)
! term(99) = term(99) + wmo_interm_20_triplet(a, c) * wmo_interm_61_triplet(c, a)
! term(100) = term(100) + wmo_interm_27_triplet(a, c) * wmo_interm_60_triplet(c, a)
! term(101) = term(101) + wmo_interm_28_triplet(a, c) * wmo_interm_60_triplet(c, a)
! term(102) = term(102) + wmo_interm_27_triplet(a, c) * wmo_interm_61_triplet(c, a)
! term(103) = term(103) + wmo_interm_28_triplet(a, c) * wmo_interm_61_triplet(c, a)
! end do 
! end do 

! term(80) = term(80) * (-6.0d+0) 
! term(81) = term(81) * (-6.0d+0) 
! term(82) = term(82) * 6.0d+0 
! term(83) = term(83) * (-6.0d+0) 
! term(84) = term(84) * 4.0d+0 
! term(85) = term(85) * (-8.0d+0) 
! term(86) = term(86) * 3.0d+0 
! term(87) = term(87) * (-3.0d+0) 
! term(88) = term(88) * 4.0d+0 
! term(89) = term(89) * (-8.0d+0) 
! term(90) = term(90) * (-4.0d+0) 
! term(91) = term(91) * 2.0d+0 
! term(92) = term(92) * (-12.0d+0) 
! term(93) = term(93) * 6.0d+0 
! term(94) = term(94) * (-12.0d+0) 
! term(95) = term(95) * 12.0d+0 
! term(96) = term(96) * 6.0d+0 
! term(97) = term(97) * (-6.0d+0) 
! term(98) = term(98) * (-12.0d+0) 
! term(99) = term(99) * 6.0d+0 
! term(100) = term(100) * 8.0d+0 
! term(101) = term(101) * (-16.0d+0) 
! term(102) = term(102) * (-4.0d+0) 
! term(103) = term(103) * 8.0d+0 

! do d = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! term(104) = term(104) + wmo_interm_51_triplet(a, d) * wmo_interm_30_triplet(a, d)
! term(105) = term(105) + wmo_interm_51_triplet(a, d) * wmo_interm_31_triplet(a, d)
! term(106) = term(106) + wmo_interm_49_triplet(a, d) * wmo_interm_30_triplet(a, d)
! term(107) = term(107) + wmo_interm_49_triplet(a, d) * wmo_interm_31_triplet(a, d)
! term(108) = term(108) + wmo_interm_51_triplet(a, d) * wmo_interm_34_triplet(a, d)
! term(109) = term(109) + wmo_interm_51_triplet(a, d) * wmo_interm_35_triplet(a, d)
! term(110) = term(110) + wmo_interm_49_triplet(a, d) * wmo_interm_34_triplet(a, d)
! term(111) = term(111) + wmo_interm_49_triplet(a, d) * wmo_interm_35_triplet(a, d)
! end do 
! end do 

! term(104) = term(104) * 6.0d+0 
! term(105) = term(105) * (-6.0d+0) 
! term(106) = term(106) * (-3.0d+0) 
! term(107) = term(107) * 3.0d+0 
! term(108) = term(108) * 8.0d+0 
! term(109) = term(109) * (-16.0d+0) 
! term(110) = term(110) * (-4.0d+0) 
! term(111) = term(111) * 8.0d+0 

! do k = 1, nocc 
! do j = 1, nocc 
! term(112) = term(112) + wmo_interm_17_triplet(j, k) * wmo_interm_1_triplet(k, j)
! term(113) = term(113) + wmo_interm_21_triplet(j, k) * wmo_interm_1_triplet(k, j)
! term(114) = term(114) + wmo_interm_22_triplet(j, k) * wmo_interm_1_triplet(k, j)
! term(115) = term(115) + wmo_interm_46_triplet(j, k) * wmo_interm_3_triplet(j, k)
! term(116) = term(116) + wmo_interm_47_triplet(j, k) * wmo_interm_3_triplet(j, k)
! term(117) = term(117) + wmo_interm_52_triplet(j, k) * wmo_interm_3_triplet(j, k)
! term(118) = term(118) + wmo_interm_53_triplet(j, k) * wmo_interm_3_triplet(j, k)
! term(119) = term(119) + wmo_interm_17_triplet(j, k) * wmo_interm_54_triplet(k, j)
! term(120) = term(120) + wmo_interm_17_triplet(j, k) * wmo_interm_55_triplet(k, j)
! term(121) = term(121) + wmo_interm_17_triplet(j, k) * wmo_interm_54_triplet(k, j)
! term(122) = term(122) + wmo_interm_17_triplet(j, k) * wmo_interm_55_triplet(k, j)
! term(123) = term(123) + wmo_interm_21_triplet(j, k) * wmo_interm_54_triplet(k, j)
! term(124) = term(124) + wmo_interm_22_triplet(j, k) * wmo_interm_54_triplet(k, j)
! term(125) = term(125) + wmo_interm_21_triplet(j, k) * wmo_interm_55_triplet(k, j)
! term(126) = term(126) + wmo_interm_22_triplet(j, k) * wmo_interm_55_triplet(k, j)
! term(127) = term(127) + wmo_interm_21_triplet(j, k) * wmo_interm_54_triplet(k, j)
! term(128) = term(128) + wmo_interm_22_triplet(j, k) * wmo_interm_54_triplet(k, j)
! term(129) = term(129) + wmo_interm_21_triplet(j, k) * wmo_interm_55_triplet(k, j)
! term(130) = term(130) + wmo_interm_22_triplet(j, k) * wmo_interm_55_triplet(k, j)
! end do 
! end do 

! term(112) = term(112) * 6.0d+0 
! term(113) = term(113) * 4.0d+0 
! term(114) = term(114) * (-8.0d+0) 
! term(115) = term(115) * (-3.0d+0) 
! term(116) = term(116) * 3.0d+0 
! term(117) = term(117) * 2.0d+0 
! term(118) = term(118) * (-4.0d+0) 
! term(119) = term(119) * (-3.0d+0) 
! term(120) = term(120) * 6.0d+0 
! term(121) = term(121) * (-3.0d+0) 
! term(122) = term(122) * 6.0d+0 
! term(123) = term(123) * (-2.0d+0) 
! term(124) = term(124) * 4.0d+0 
! term(125) = term(125) * 4.0d+0 
! term(126) = term(126) * (-8.0d+0) 
! term(127) = term(127) * (-2.0d+0) 
! term(128) = term(128) * 4.0d+0 
! term(129) = term(129) * 4.0d+0 
! term(130) = term(130) * (-8.0d+0) 

! do c = nocc + 1, nactive 
! do b = nocc + 1, nactive 
! term(131) = term(131) + wmo_interm_18_triplet(b, c) * wmo_interm_12_triplet(c, b)
! term(132) = term(132) + wmo_interm_23_triplet(b, c) * wmo_interm_12_triplet(c, b)
! term(133) = term(133) + wmo_interm_24_triplet(b, c) * wmo_interm_12_triplet(c, b)
! term(134) = term(134) + wmo_interm_43_triplet(b, c) * wmo_interm_44_triplet(b, c)
! term(135) = term(135) + wmo_interm_45_triplet(b, c) * wmo_interm_44_triplet(b, c)
! term(136) = term(136) + wmo_interm_49_triplet(b, c) * wmo_interm_44_triplet(b, c)
! term(137) = term(137) + wmo_interm_51_triplet(b, c) * wmo_interm_44_triplet(b, c)
! term(138) = term(138) + wmo_interm_18_triplet(b, c) * wmo_interm_60_triplet(c, b)
! term(139) = term(139) + wmo_interm_18_triplet(b, c) * wmo_interm_61_triplet(c, b)
! term(140) = term(140) + wmo_interm_23_triplet(b, c) * wmo_interm_60_triplet(c, b)
! term(141) = term(141) + wmo_interm_24_triplet(b, c) * wmo_interm_60_triplet(c, b)
! term(142) = term(142) + wmo_interm_23_triplet(b, c) * wmo_interm_61_triplet(c, b)
! term(143) = term(143) + wmo_interm_24_triplet(b, c) * wmo_interm_61_triplet(c, b)
! end do 
! end do 

! term(131) = term(131) * 6.0d+0 
! term(132) = term(132) * 4.0d+0 
! term(133) = term(133) * (-8.0d+0) 
! term(134) = term(134) * 3.0d+0 
! term(135) = term(135) * (-3.0d+0) 
! term(136) = term(136) * 2.0d+0 
! term(137) = term(137) * (-4.0d+0) 
! term(138) = term(138) * 12.0d+0 
! term(139) = term(139) * (-6.0d+0) 
! term(140) = term(140) * 8.0d+0 
! term(141) = term(141) * (-16.0d+0) 
! term(142) = term(142) * (-4.0d+0) 
! term(143) = term(143) * 8.0d+0 

! do d = nocc + 1, nactive 
! do l = 1, nocc 
! do a = nocc + 1, nactive 
! term(144) = term(144) + s1(d,l) * wmo_interm_51_triplet(a, d) * wmo_interm_7_triplet(a, l)
! term(145) = term(145) + s1(d,l) * wmo_interm_49_triplet(a, d) * wmo_interm_7_triplet(a, l)
! end do 
! end do 
! end do 

! term(144) = term(144) * 8.0d+0 
! term(145) = term(145) * (-4.0d+0) 

! do l = 1, nocc 
! do i = 1, nocc 
! term(146) = term(146) + wmo_interm_53_triplet(i, l) * wmo_interm_32_triplet(i, l)
! term(147) = term(147) + wmo_interm_53_triplet(i, l) * wmo_interm_33_triplet(i, l)
! term(148) = term(148) + wmo_interm_52_triplet(i, l) * wmo_interm_32_triplet(i, l)
! term(149) = term(149) + wmo_interm_52_triplet(i, l) * wmo_interm_33_triplet(i, l)
! term(150) = term(150) + wmo_interm_53_triplet(i, l) * wmo_interm_36_triplet(i, l)
! term(151) = term(151) + wmo_interm_53_triplet(i, l) * wmo_interm_37_triplet(i, l)
! term(152) = term(152) + wmo_interm_53_triplet(i, l) * wmo_interm_38_triplet(i, l)
! term(153) = term(153) + wmo_interm_53_triplet(i, l) * wmo_interm_39_triplet(i, l)
! term(154) = term(154) + wmo_interm_52_triplet(i, l) * wmo_interm_36_triplet(i, l)
! term(155) = term(155) + wmo_interm_52_triplet(i, l) * wmo_interm_37_triplet(i, l)
! term(156) = term(156) + wmo_interm_52_triplet(i, l) * wmo_interm_38_triplet(i, l)
! term(157) = term(157) + wmo_interm_52_triplet(i, l) * wmo_interm_39_triplet(i, l)
! end do 
! end do 

! term(146) = term(146) * (-6.0d+0) 
! term(147) = term(147) * 6.0d+0 
! term(148) = term(148) * 3.0d+0 
! term(149) = term(149) * (-3.0d+0) 
! term(150) = term(150) * 4.0d+0 
! term(151) = term(151) * (-8.0d+0) 
! term(152) = term(152) * 4.0d+0 
! term(153) = term(153) * (-8.0d+0) 
! term(154) = term(154) * (-2.0d+0) 
! term(155) = term(155) * 4.0d+0 
! term(156) = term(156) * (-2.0d+0) 
! term(157) = term(157) * 4.0d+0 

! do j = 1, nocc 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! term(158) = term(158) + r2p(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,j,b,i)
! term(159) = term(159) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,j,b,i)
! end do 
! end do 
! end do 
! end do 

! term(158) = term(158) * (-3.0d+0) 
! term(159) = term(159) * (-4.0d+0) 

! do d = nocc + 1, nactive 
! do l = 1, nocc 
! do b = nocc + 1, nactive 
! term(160) = term(160) + s1(d,l) * wmo_interm_43_triplet(b, d) * wmo_interm_7_triplet(b, l)
! term(161) = term(161) + s1(d,l) * wmo_interm_45_triplet(b, d) * wmo_interm_7_triplet(b, l)
! term(162) = term(162) + s1(d,l) * wmo_interm_49_triplet(b, d) * wmo_interm_7_triplet(b, l)
! term(163) = term(163) + s1(d,l) * wmo_interm_51_triplet(b, d) * wmo_interm_7_triplet(b, l)
! end do 
! end do 
! end do 

! term(160) = term(160) * (-6.0d+0) 
! term(161) = term(161) * 6.0d+0 
! term(162) = term(162) * (-4.0d+0) 
! term(163) = term(163) * 8.0d+0 

! do i = 1, nocc 
! do a = nocc + 1, nactive 
! do j = 1, nocc 
! do b = nocc + 1, nactive 
! term(164) = term(164) + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, b,j,a,i) * s1(b,j)
! end do 
! end do 
! end do 
! end do 

! term(164) = term(164) * 6.0d+0 

! do l = 1, nocc 
! do i = 1, nocc 
! do c = nocc + 1, nactive 
! term(165) = term(165) + s1(c,l) * wmo_interm_53_triplet(i, l) * wmo_interm_5_triplet(c, i)
! term(166) = term(166) + s1(c,l) * wmo_interm_52_triplet(i, l) * wmo_interm_5_triplet(c, i)
! term(167) = term(167) + s1(c,l) * wmo_interm_53_triplet(i, l) * wmo_interm_6_triplet(c, i)
! term(168) = term(168) + s1(c,l) * wmo_interm_53_triplet(i, l) * wmo_interm_7_triplet(c, i)
! term(169) = term(169) + s1(c,l) * wmo_interm_52_triplet(i, l) * wmo_interm_6_triplet(c, i)
! term(170) = term(170) + s1(c,l) * wmo_interm_52_triplet(i, l) * wmo_interm_7_triplet(c, i)
! end do 
! end do 
! end do 

! term(165) = term(165) * 6.0d+0 
! term(166) = term(166) * (-3.0d+0) 
! term(167) = term(167) * (-8.0d+0) 
! term(168) = term(168) * 4.0d+0 
! term(169) = term(169) * 4.0d+0 
! term(170) = term(170) * (-2.0d+0) 

! do i = 1, nocc 
! do b = nocc + 1, nactive 
! do j = 1, nocc 
! do a = nocc + 1, nactive 
! term(171) = term(171) + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, a,j,b,i) * s1(b,j)
! end do 
! end do 
! end do 
! end do 

! term(171) = term(171) * (-4.0d+0) 

! do j = 1, nocc 
! do d = nocc + 1, nactive 
! do l = 1, nocc 
! do b = nocc + 1, nactive 
! term(172) = term(172) + s2(b,d,l,j) * wmo_interm_56_triplet(d, l) * wmo_interm_5_triplet(b, j)
! term(173) = term(173) + s2(b,d,l,j) * wmo_interm_62_triplet(d, l) * wmo_interm_5_triplet(b, j)
! term(174) = term(174) + s2(b,d,l,j) * wmo_interm_56_triplet(d, l) * wmo_interm_6_triplet(b, j)
! term(175) = term(175) + s2(b,d,l,j) * wmo_interm_62_triplet(d, l) * wmo_interm_6_triplet(b, j)
! end do 
! end do 
! end do 
! end do 

! term(172) = term(172) * 3.0d+0 
! term(173) = term(173) * (-6.0d+0) 
! term(174) = term(174) * 4.0d+0 
! term(175) = term(175) * (-8.0d+0) 

! do l = 1, nocc 
! do d = nocc + 1, nactive 
! do j = 1, nocc 
! do b = nocc + 1, nactive 
! term(176) = term(176) + s2(b,d,j,l) * wmo_interm_56_triplet(d, l) * wmo_interm_5_triplet(b, j)
! term(177) = term(177) + s2(b,d,j,l) * wmo_interm_62_triplet(d, l) * wmo_interm_5_triplet(b, j)
! term(178) = term(178) + s2(b,d,j,l) * wmo_interm_56_triplet(d, l) * wmo_interm_6_triplet(b, j)
! term(179) = term(179) + s2(b,d,j,l) * wmo_interm_62_triplet(d, l) * wmo_interm_6_triplet(b, j)
! end do 
! end do 
! end do 
! end do 

! term(176) = term(176) * (-6.0d+0) 
! term(177) = term(177) * 12.0d+0 
! term(178) = term(178) * (-8.0d+0) 
! term(179) = term(179) * 16.0d+0 

! do j = 1, nocc 
! do d = nocc + 1, nactive 
! do l = 1, nocc 
! do c = nocc + 1, nactive 
! term(180) = term(180) + s2(c,d,l,j) * wmo_interm_56_triplet(d, l) * wmo_interm_5_triplet(c, j)
! term(181) = term(181) + s2(c,d,l,j) * wmo_interm_62_triplet(d, l) * wmo_interm_5_triplet(c, j)
! term(182) = term(182) + s2(c,d,l,j) * wmo_interm_56_triplet(d, l) * wmo_interm_7_triplet(c, j)
! term(183) = term(183) + s2(c,d,l,j) * wmo_interm_62_triplet(d, l) * wmo_interm_7_triplet(c, j)
! end do 
! end do 
! end do 
! end do 

! term(180) = term(180) * (-3.0d+0) 
! term(181) = term(181) * 6.0d+0 
! term(182) = term(182) * (-2.0d+0) 
! term(183) = term(183) * 4.0d+0 

! do l = 1, nocc 
! do d = nocc + 1, nactive 
! do j = 1, nocc 
! do c = nocc + 1, nactive 
! term(184) = term(184) + s2(c,d,j,l) * wmo_interm_56_triplet(d, l) * wmo_interm_5_triplet(c, j)
! term(185) = term(185) + s2(c,d,j,l) * wmo_interm_62_triplet(d, l) * wmo_interm_5_triplet(c, j)
! term(186) = term(186) + s2(c,d,j,l) * wmo_interm_56_triplet(d, l) * wmo_interm_7_triplet(c, j)
! term(187) = term(187) + s2(c,d,j,l) * wmo_interm_62_triplet(d, l) * wmo_interm_7_triplet(c, j)
! end do 
! end do 
! end do 
! end do 

! term(184) = term(184) * 6.0d+0 
! term(185) = term(185) * (-12.0d+0) 
! term(186) = term(186) * 4.0d+0 
! term(187) = term(187) * (-8.0d+0) 

! do a = nocc + 1, nactive 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! term(188) = term(188) + r1(vrdav_Rl, a,i) * wmo_interm_5_triplet(b, i) * wmo_interm_60_triplet(b, a)
! term(189) = term(189) + r1(vrdav_Rl, a,i) * wmo_interm_5_triplet(b, i) * wmo_interm_61_triplet(b, a)
! term(190) = term(190) + r1(vrdav_Rl, a,i) * wmo_interm_6_triplet(b, i) * wmo_interm_60_triplet(b, a)
! term(191) = term(191) + r1(vrdav_Rl, a,i) * wmo_interm_6_triplet(b, i) * wmo_interm_61_triplet(b, a)
! term(192) = term(192) + r1(vrdav_Rl, a,i) * wmo_interm_7_triplet(b, i) * wmo_interm_60_triplet(b, a)
! term(193) = term(193) + r1(vrdav_Rl, a,i) * wmo_interm_7_triplet(b, i) * wmo_interm_61_triplet(b, a)
! end do 
! end do 
! end do 

! term(188) = term(188) * 6.0d+0 
! term(189) = term(189) * (-3.0d+0) 
! term(190) = term(190) * (-8.0d+0) 
! term(191) = term(191) * 4.0d+0 
! term(192) = term(192) * 4.0d+0 
! term(193) = term(193) * (-2.0d+0) 

! do k = 1, nocc 
! do i = 1, nocc 
! do c = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! term(194) = term(194) + wmo_interm_72_triplet(a, c, i, k) * wmo_interm_73_triplet(c, a, k, i)
! term(195) = term(195) + wmo_interm_72_triplet(a, c, i, k) * wmo_interm_74_triplet(c, a, k, i)
! term(196) = term(196) + wmo_interm_72_triplet(a, c, i, k) * wmo_interm_75_triplet(c, a, k, i)
! term(197) = term(197) + wmo_interm_72_triplet(a, c, i, k) * wmo_interm_76_triplet(c, a, k, i)
! term(198) = term(198) + wmo_interm_77_triplet(a, c, i, k) * wmo_interm_73_triplet(c, a, k, i)
! term(199) = term(199) + wmo_interm_78_triplet(a, c, i, k) * wmo_interm_73_triplet(c, a, k, i)
! term(200) = term(200) + wmo_interm_77_triplet(a, c, i, k) * wmo_interm_74_triplet(c, a, k, i)
! term(201) = term(201) + wmo_interm_78_triplet(a, c, i, k) * wmo_interm_74_triplet(c, a, k, i)
! term(202) = term(202) + wmo_interm_77_triplet(a, c, i, k) * wmo_interm_75_triplet(c, a, k, i)
! term(203) = term(203) + wmo_interm_78_triplet(a, c, i, k) * wmo_interm_75_triplet(c, a, k, i)
! term(204) = term(204) + wmo_interm_77_triplet(a, c, i, k) * wmo_interm_76_triplet(c, a, k, i)
! term(205) = term(205) + wmo_interm_78_triplet(a, c, i, k) * wmo_interm_76_triplet(c, a, k, i)
! term(206) = term(206) + wmo_interm_79_triplet(a, c, i, k) * wmo_interm_73_triplet(c, a, k, i)
! term(207) = term(207) + wmo_interm_79_triplet(a, c, i, k) * wmo_interm_74_triplet(c, a, k, i)
! term(208) = term(208) + wmo_interm_79_triplet(a, c, i, k) * wmo_interm_75_triplet(c, a, k, i)
! term(209) = term(209) + wmo_interm_79_triplet(a, c, i, k) * wmo_interm_76_triplet(c, a, k, i)
! term(210) = term(210) + wmo_interm_81_triplet(a, c, i, k) * wmo_interm_73_triplet(c, a, k, i)
! term(211) = term(211) + wmo_interm_82_triplet(a, c, i, k) * wmo_interm_73_triplet(c, a, k, i)
! term(212) = term(212) + wmo_interm_81_triplet(a, c, i, k) * wmo_interm_74_triplet(c, a, k, i)
! term(213) = term(213) + wmo_interm_82_triplet(a, c, i, k) * wmo_interm_74_triplet(c, a, k, i)
! term(214) = term(214) + wmo_interm_81_triplet(a, c, i, k) * wmo_interm_75_triplet(c, a, k, i)
! term(215) = term(215) + wmo_interm_82_triplet(a, c, i, k) * wmo_interm_75_triplet(c, a, k, i)
! term(216) = term(216) + wmo_interm_81_triplet(a, c, i, k) * wmo_interm_76_triplet(c, a, k, i)
! term(217) = term(217) + wmo_interm_82_triplet(a, c, i, k) * wmo_interm_76_triplet(c, a, k, i)
! end do 
! end do 
! end do 
! end do 

! term(194) = term(194) * 12.0d+0 
! term(195) = term(195) * (-18.0d+0) 
! term(196) = term(196) * 36.0d+0 
! term(197) = term(197) * (-18.0d+0) 
! term(198) = term(198) * (-12.0d+0) 
! term(199) = term(199) * 12.0d+0 
! term(200) = term(200) * 12.0d+0 
! term(201) = term(201) * (-24.0d+0) 
! term(202) = term(202) * (-24.0d+0) 
! term(203) = term(203) * 48.0d+0 
! term(204) = term(204) * 12.0d+0 
! term(205) = term(205) * (-24.0d+0) 
! term(206) = term(206) * 6.0d+0 
! term(207) = term(207) * (-12.0d+0) 
! term(208) = term(208) * 24.0d+0 
! term(209) = term(209) * (-12.0d+0) 
! term(210) = term(210) * (-4.0d+0) 
! term(211) = term(211) * 8.0d+0 
! term(212) = term(212) * 8.0d+0 
! term(213) = term(213) * (-16.0d+0) 
! term(214) = term(214) * (-16.0d+0) 
! term(215) = term(215) * 32.0d+0 
! term(216) = term(216) * 8.0d+0 
! term(217) = term(217) * (-16.0d+0) 

! do a = nocc + 1, nactive 
! do i = 1, nocc 
! do c = nocc + 1, nactive 
! term(218) = term(218) + r1(vrdav_Rl, a,i) * wmo_interm_5_triplet(c, i) * wmo_interm_60_triplet(c, a)
! term(219) = term(219) + r1(vrdav_Rl, a,i) * wmo_interm_5_triplet(c, i) * wmo_interm_61_triplet(c, a)
! term(220) = term(220) + r1(vrdav_Rl, a,i) * wmo_interm_7_triplet(c, i) * wmo_interm_60_triplet(c, a)
! term(221) = term(221) + r1(vrdav_Rl, a,i) * wmo_interm_6_triplet(c, i) * wmo_interm_60_triplet(c, a)
! term(222) = term(222) + r1(vrdav_Rl, a,i) * wmo_interm_7_triplet(c, i) * wmo_interm_61_triplet(c, a)
! term(223) = term(223) + r1(vrdav_Rl, a,i) * wmo_interm_6_triplet(c, i) * wmo_interm_61_triplet(c, a)
! end do 
! end do 
! end do 

! term(218) = term(218) * (-6.0d+0) 
! term(219) = term(219) * 3.0d+0 
! term(220) = term(220) * 4.0d+0 
! term(221) = term(221) * (-8.0d+0) 
! term(222) = term(222) * (-2.0d+0) 
! term(223) = term(223) * 4.0d+0 

! do k = 1, nocc 
! do c = nocc + 1, nactive 
! term(224) = term(224) + wmo_interm_56_triplet(c, k) * wmo_interm_57_triplet(c, k)
! term(225) = term(225) + wmo_interm_56_triplet(c, k) * wmo_interm_58_triplet(c, k)
! term(226) = term(226) + wmo_interm_62_triplet(c, k) * wmo_interm_58_triplet(c, k)
! term(227) = term(227) + wmo_interm_62_triplet(c, k) * wmo_interm_57_triplet(c, k)
! end do 
! end do 

! term(224) = term(224) * 2.0d+0 
! term(225) = term(225) * (-4.0d+0) 
! term(226) = term(226) * 8.0d+0 
! term(227) = term(227) * (-4.0d+0) 

! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! term(228) = term(228) + wmo_interm_59_triplet(a, b) * wmo_interm_60_triplet(b, a)
! term(229) = term(229) + wmo_interm_59_triplet(a, b) * wmo_interm_61_triplet(b, a)
! end do 
! end do 

! term(228) = term(228) * (-4.0d+0) 
! term(229) = term(229) * 2.0d+0 

! do j = 1, nocc 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! term(230) = term(230) + r1(vrdav_Rl, a,i) * wmo_interm_5_triplet(a, j) * wmo_interm_1_triplet(j, i)
! term(231) = term(231) + r1(vrdav_Rl, a,i) * wmo_interm_6_triplet(a, j) * wmo_interm_1_triplet(j, i)
! term(232) = term(232) + r1(vrdav_Rl, a,i) * wmo_interm_5_triplet(a, j) * wmo_interm_54_triplet(j, i)
! term(233) = term(233) + r1(vrdav_Rl, a,i) * wmo_interm_5_triplet(a, j) * wmo_interm_55_triplet(j, i)
! term(234) = term(234) + r1(vrdav_Rl, a,i) * wmo_interm_5_triplet(a, j) * wmo_interm_54_triplet(j, i)
! term(235) = term(235) + r1(vrdav_Rl, a,i) * wmo_interm_5_triplet(a, j) * wmo_interm_55_triplet(j, i)
! term(236) = term(236) + r1(vrdav_Rl, a,i) * wmo_interm_6_triplet(a, j) * wmo_interm_54_triplet(j, i)
! term(237) = term(237) + r1(vrdav_Rl, a,i) * wmo_interm_6_triplet(a, j) * wmo_interm_55_triplet(j, i)
! term(238) = term(238) + r1(vrdav_Rl, a,i) * wmo_interm_6_triplet(a, j) * wmo_interm_54_triplet(j, i)
! term(239) = term(239) + r1(vrdav_Rl, a,i) * wmo_interm_6_triplet(a, j) * wmo_interm_55_triplet(j, i)
! end do 
! end do 
! end do 

! term(230) = term(230) * 6.0d+0 
! term(231) = term(231) * (-8.0d+0) 
! term(232) = term(232) * (-3.0d+0) 
! term(233) = term(233) * 6.0d+0 
! term(234) = term(234) * (-3.0d+0) 
! term(235) = term(235) * 6.0d+0 
! term(236) = term(236) * 4.0d+0 
! term(237) = term(237) * (-8.0d+0) 
! term(238) = term(238) * 4.0d+0 
! term(239) = term(239) * (-8.0d+0) 

! do k = 1, nocc 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! term(240) = term(240) + s1(b,k) * wmo_interm_2_triplet(i, k) * wmo_interm_5_triplet(b, i)
! term(241) = term(241) + s1(b,k) * wmo_interm_2_triplet(i, k) * wmo_interm_6_triplet(b, i)
! term(242) = term(242) + s1(b,k) * wmo_interm_2_triplet(i, k) * wmo_interm_7_triplet(b, i)
! end do 
! end do 
! end do 

! term(240) = term(240) * 3.0d+0 
! term(241) = term(241) * (-4.0d+0) 
! term(242) = term(242) * 2.0d+0 

! do k = 1, nocc 
! do i = 1, nocc 
! do c = nocc + 1, nactive 
! term(243) = term(243) + s1(c,k) * wmo_interm_2_triplet(i, k) * wmo_interm_5_triplet(c, i)
! term(244) = term(244) + s1(c,k) * wmo_interm_2_triplet(i, k) * wmo_interm_7_triplet(c, i)
! term(245) = term(245) + s1(c,k) * wmo_interm_2_triplet(i, k) * wmo_interm_6_triplet(c, i)
! end do 
! end do 
! end do 

! term(243) = term(243) * (-3.0d+0) 
! term(244) = term(244) * 2.0d+0 
! term(245) = term(245) * (-4.0d+0) 

! do k = 1, nocc 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! term(246) = term(246) + r1(vrdav_Rl, a,i) * wmo_interm_7_triplet(a, k) * wmo_interm_1_triplet(k, i)
! term(247) = term(247) + r1(vrdav_Rl, a,i) * wmo_interm_7_triplet(a, k) * wmo_interm_54_triplet(k, i)
! term(248) = term(248) + r1(vrdav_Rl, a,i) * wmo_interm_7_triplet(a, k) * wmo_interm_55_triplet(k, i)
! term(249) = term(249) + r1(vrdav_Rl, a,i) * wmo_interm_7_triplet(a, k) * wmo_interm_54_triplet(k, i)
! term(250) = term(250) + r1(vrdav_Rl, a,i) * wmo_interm_7_triplet(a, k) * wmo_interm_55_triplet(k, i)
! end do 
! end do 
! end do 

! term(246) = term(246) * 4.0d+0 
! term(247) = term(247) * (-2.0d+0) 
! term(248) = term(248) * 4.0d+0 
! term(249) = term(249) * (-2.0d+0) 
! term(250) = term(250) * 4.0d+0 

! do l = 1, nocc 
! do d = nocc + 1, nactive 
! do k = 1, nocc 
! do c = nocc + 1, nactive 
! term(251) = term(251) + s2(c,d,k,l) * wmo_interm_56_triplet(d, l) * wmo_interm_6_triplet(c, k)
! term(252) = term(252) + s2(c,d,k,l) * wmo_interm_62_triplet(d, l) * wmo_interm_6_triplet(c, k)
! end do 
! end do 
! end do 
! end do 

! term(251) = term(251) * (-8.0d+0) 
! term(252) = term(252) * 16.0d+0 

! do k = 1, nocc 
! do d = nocc + 1, nactive 
! do l = 1, nocc 
! do b = nocc + 1, nactive 
! term(253) = term(253) + s2(b,d,l,k) * wmo_interm_56_triplet(d, l) * wmo_interm_7_triplet(b, k)
! term(254) = term(254) + s2(b,d,l,k) * wmo_interm_62_triplet(d, l) * wmo_interm_7_triplet(b, k)
! end do 
! end do 
! end do 
! end do 

! term(253) = term(253) * (-2.0d+0) 
! term(254) = term(254) * 4.0d+0 

! do j = 1, nocc 
! do b = nocc + 1, nactive 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! term(255) = term(255) + r2p(vrdav_Rl, a,i,b,j) * wmo_interm_5_triplet(b, j) * wmo_interm_41_triplet(a, i)
! term(256) = term(256) + r2p(vrdav_Rl, a,i,b,j) * wmo_interm_5_triplet(b, j) * wmo_interm_42_triplet(a, i)
! term(257) = term(257) + r2p(vrdav_Rl, a,i,b,j) * wmo_interm_6_triplet(b, j) * wmo_interm_41_triplet(a, i)
! term(258) = term(258) + r2p(vrdav_Rl, a,i,b,j) * wmo_interm_7_triplet(b, j) * wmo_interm_41_triplet(a, i)
! term(259) = term(259) + r2p(vrdav_Rl, a,i,b,j) * wmo_interm_6_triplet(b, j) * wmo_interm_42_triplet(a, i)
! term(260) = term(260) + r2p(vrdav_Rl, a,i,b,j) * wmo_interm_7_triplet(b, j) * wmo_interm_42_triplet(a, i)
! term(261) = term(261) + r2m(vrdav_Rl, a,i,b,j) * wmo_interm_5_triplet(a, i) * wmo_interm_41_triplet(b, j)
! term(262) = term(262) + r2m(vrdav_Rl, a,i,b,j) * wmo_interm_5_triplet(a, i) * wmo_interm_42_triplet(b, j)
! term(263) = term(263) + r2m(vrdav_Rl, a,i,b,j) * wmo_interm_5_triplet(b, j) * wmo_interm_41_triplet(a, i)
! term(264) = term(264) + r2m(vrdav_Rl, a,i,b,j) * wmo_interm_5_triplet(a, j) * wmo_interm_41_triplet(b, i)
! term(265) = term(265) + r2m(vrdav_Rl, a,i,b,j) * wmo_interm_5_triplet(a, j) * wmo_interm_42_triplet(b, i)
! term(266) = term(266) + r2m(vrdav_Rl, a,i,b,j) * wmo_interm_5_triplet(b, i) * wmo_interm_41_triplet(a, j)
! term(267) = term(267) + r2m(vrdav_Rl, a,i,b,j) * wmo_interm_5_triplet(b, i) * wmo_interm_42_triplet(a, j)
! term(268) = term(268) + r2m(vrdav_Rl, a,i,b,j) * wmo_interm_5_triplet(b, j) * wmo_interm_42_triplet(a, i)
! term(269) = term(269) + r2m(vrdav_Rl, a,i,b,j) * wmo_interm_6_triplet(a, i) * wmo_interm_41_triplet(b, j)
! term(270) = term(270) + r2m(vrdav_Rl, a,i,b,j) * wmo_interm_7_triplet(a, i) * wmo_interm_41_triplet(b, j)
! term(271) = term(271) + r2m(vrdav_Rl, a,i,b,j) * wmo_interm_6_triplet(a, i) * wmo_interm_42_triplet(b, j)
! term(272) = term(272) + r2m(vrdav_Rl, a,i,b,j) * wmo_interm_7_triplet(a, i) * wmo_interm_42_triplet(b, j)
! term(273) = term(273) + r2m(vrdav_Rl, a,i,b,j) * wmo_interm_6_triplet(b, j) * wmo_interm_41_triplet(a, i)
! term(274) = term(274) + r2m(vrdav_Rl, a,i,b,j) * wmo_interm_6_triplet(a, j) * wmo_interm_41_triplet(b, i)
! term(275) = term(275) + r2m(vrdav_Rl, a,i,b,j) * wmo_interm_7_triplet(b, j) * wmo_interm_41_triplet(a, i)
! term(276) = term(276) + r2m(vrdav_Rl, a,i,b,j) * wmo_interm_7_triplet(a, j) * wmo_interm_41_triplet(b, i)
! term(277) = term(277) + r2m(vrdav_Rl, a,i,b,j) * wmo_interm_6_triplet(a, j) * wmo_interm_42_triplet(b, i)
! term(278) = term(278) + r2m(vrdav_Rl, a,i,b,j) * wmo_interm_7_triplet(a, j) * wmo_interm_42_triplet(b, i)
! term(279) = term(279) + r2m(vrdav_Rl, a,i,b,j) * wmo_interm_6_triplet(b, i) * wmo_interm_41_triplet(a, j)
! term(280) = term(280) + r2m(vrdav_Rl, a,i,b,j) * wmo_interm_7_triplet(b, i) * wmo_interm_41_triplet(a, j)
! term(281) = term(281) + r2m(vrdav_Rl, a,i,b,j) * wmo_interm_6_triplet(b, i) * wmo_interm_42_triplet(a, j)
! term(282) = term(282) + r2m(vrdav_Rl, a,i,b,j) * wmo_interm_6_triplet(b, j) * wmo_interm_42_triplet(a, i)
! term(283) = term(283) + r2m(vrdav_Rl, a,i,b,j) * wmo_interm_7_triplet(b, i) * wmo_interm_42_triplet(a, j)
! term(284) = term(284) + r2m(vrdav_Rl, a,i,b,j) * wmo_interm_7_triplet(b, j) * wmo_interm_42_triplet(a, i)
! term(285) = term(285) + r2p(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, a,i,b,j)
! term(286) = term(286) + r2p(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,i,b,j)
! term(287) = term(287) + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, a,i,b,j)
! term(288) = term(288) + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,i,b,j)
! term(289) = term(289) + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, a,i,b,j) * s1(b,j)
! term(290) = term(290) + r2p(vrdav_Rl, a,i,b,j) * r1(vrdav_Rr, b,j) * t1(a,i)
! term(291) = term(291) + r2m(vrdav_Rl, a,i,b,j) * r1(vrdav_Rr, a,i) * t1(b,j)
! term(292) = term(292) + r2m(vrdav_Rl, a,i,b,j) * r1(vrdav_Rr, a,j) * t1(b,i)
! term(293) = term(293) + r2m(vrdav_Rl, a,i,b,j) * r1(vrdav_Rr, b,i) * t1(a,j)
! term(294) = term(294) + r2m(vrdav_Rl, a,i,b,j) * r1(vrdav_Rr, b,j) * t1(a,i)
! end do 
! end do 
! end do 
! end do 

! term(255) = term(255) * (-18.0d+0) 
! term(256) = term(256) * 36.0d+0 
! term(257) = term(257) * (-24.0d+0) 
! term(258) = term(258) * 12.0d+0 
! term(259) = term(259) * 48.0d+0 
! term(260) = term(260) * (-24.0d+0) 
! term(261) = term(261) * (-12.0d+0) 
! term(262) = term(262) * 24.0d+0 
! term(263) = term(263) * (-12.0d+0) 
! term(264) = term(264) * 6.0d+0 
! term(265) = term(265) * (-12.0d+0) 
! term(266) = term(266) * 6.0d+0 
! term(267) = term(267) * (-12.0d+0) 
! term(268) = term(268) * 24.0d+0 
! term(269) = term(269) * (-16.0d+0) 
! term(270) = term(270) * 8.0d+0 
! term(271) = term(271) * 32.0d+0 
! term(272) = term(272) * (-16.0d+0) 
! term(273) = term(273) * (-16.0d+0) 
! term(274) = term(274) * 8.0d+0 
! term(275) = term(275) * 8.0d+0 
! term(276) = term(276) * (-4.0d+0) 
! term(277) = term(277) * (-16.0d+0) 
! term(278) = term(278) * 8.0d+0 
! term(279) = term(279) * 8.0d+0 
! term(280) = term(280) * (-4.0d+0) 
! term(281) = term(281) * (-16.0d+0) 
! term(282) = term(282) * 32.0d+0 
! term(283) = term(283) * 8.0d+0 
! term(284) = term(284) * (-16.0d+0) 
! term(285) = term(285) * 3.0d+0 
! term(286) = term(286) * 3.0d+0 
! term(287) = term(287) * 6.0d+0 
! term(288) = term(288) * 8.0d+0 
! term(289) = term(289) * 8.0d+0 
! term(290) = term(290) * (-6.0d+0) 
! term(291) = term(291) * (-4.0d+0) 
! term(292) = term(292) * 2.0d+0 
! term(293) = term(293) * 2.0d+0 
! term(294) = term(294) * (-4.0d+0) 

! do l = 1, nocc 
! do d = nocc + 1, nactive 
! do k = 1, nocc 
! do b = nocc + 1, nactive 
! term(295) = term(295) + s2(b,d,k,l) * wmo_interm_56_triplet(d, l) * wmo_interm_7_triplet(b, k)
! term(296) = term(296) + s2(b,d,k,l) * wmo_interm_62_triplet(d, l) * wmo_interm_7_triplet(b, k)
! end do 
! end do 
! end do 
! end do 

! term(295) = term(295) * 4.0d+0 
! term(296) = term(296) * (-8.0d+0) 

! do l = 1, nocc 
! do j = 1, nocc 
! do k = 1, nocc 
! do i = 1, nocc 
! term(297) = term(297) + wmo_interm_63_triplet(i, j, k, l) * wmo_interm_64_triplet(k, l, i, j)
! term(298) = term(298) + wmo_interm_63_triplet(i, j, k, l) * wmo_interm_64_triplet(l, k, i, j)
! term(299) = term(299) + wmo_interm_67_triplet(i, j, k, l) * wmo_interm_64_triplet(l, k, i, j)
! term(300) = term(300) + wmo_interm_67_triplet(i, j, k, l) * wmo_interm_64_triplet(k, l, i, j)
! term(301) = term(301) + wmo_interm_69_triplet(i, j, k, l) * wmo_interm_64_triplet(k, l, i, j)
! term(302) = term(302) + wmo_interm_69_triplet(i, j, k, l) * wmo_interm_64_triplet(l, k, i, j)
! term(303) = term(303) + wmo_interm_71_triplet(i, j, k, l) * wmo_interm_64_triplet(l, k, i, j)
! term(304) = term(304) + wmo_interm_71_triplet(i, j, k, l) * wmo_interm_64_triplet(k, l, i, j)
! end do 
! end do 
! end do 
! end do 

! term(297) = term(297) * 0.75d+0 
! term(298) = term(298) * (-0.75d+0) 
! term(299) = term(299) * (-1.5d+0) 
! term(300) = term(300) * 1.5d+0 
! term(301) = term(301) * 1.5d+0 
! term(302) = term(302) * (-1.5d+0) 
! term(303) = term(303) * (-2.0d+0) 
! term(304) = term(304) * 4.0d+0 

! do d = nocc + 1, nactive 
! do b = nocc + 1, nactive 
! term(305) = term(305) + wmo_interm_43_triplet(b, d) * wmo_interm_30_triplet(b, d)
! term(306) = term(306) + wmo_interm_43_triplet(b, d) * wmo_interm_31_triplet(b, d)
! term(307) = term(307) + wmo_interm_45_triplet(b, d) * wmo_interm_30_triplet(b, d)
! term(308) = term(308) + wmo_interm_45_triplet(b, d) * wmo_interm_31_triplet(b, d)
! term(309) = term(309) + wmo_interm_43_triplet(b, d) * wmo_interm_34_triplet(b, d)
! term(310) = term(310) + wmo_interm_43_triplet(b, d) * wmo_interm_35_triplet(b, d)
! term(311) = term(311) + wmo_interm_45_triplet(b, d) * wmo_interm_34_triplet(b, d)
! term(312) = term(312) + wmo_interm_45_triplet(b, d) * wmo_interm_35_triplet(b, d)
! term(313) = term(313) + wmo_interm_49_triplet(b, d) * wmo_interm_30_triplet(b, d)
! term(314) = term(314) + wmo_interm_49_triplet(b, d) * wmo_interm_31_triplet(b, d)
! term(315) = term(315) + wmo_interm_51_triplet(b, d) * wmo_interm_30_triplet(b, d)
! term(316) = term(316) + wmo_interm_51_triplet(b, d) * wmo_interm_31_triplet(b, d)
! term(317) = term(317) + wmo_interm_49_triplet(b, d) * wmo_interm_34_triplet(b, d)
! term(318) = term(318) + wmo_interm_49_triplet(b, d) * wmo_interm_35_triplet(b, d)
! term(319) = term(319) + wmo_interm_51_triplet(b, d) * wmo_interm_34_triplet(b, d)
! term(320) = term(320) + wmo_interm_51_triplet(b, d) * wmo_interm_35_triplet(b, d)
! end do 
! end do 

! term(305) = term(305) * (-4.5d+0) 
! term(306) = term(306) * 4.5d+0 
! term(307) = term(307) * 4.5d+0 
! term(308) = term(308) * (-4.5d+0) 
! term(309) = term(309) * (-6.0d+0) 
! term(310) = term(310) * 12.0d+0 
! term(311) = term(311) * 6.0d+0 
! term(312) = term(312) * (-12.0d+0) 
! term(313) = term(313) * (-3.0d+0) 
! term(314) = term(314) * 3.0d+0 
! term(315) = term(315) * 6.0d+0 
! term(316) = term(316) * (-6.0d+0) 
! term(317) = term(317) * (-4.0d+0) 
! term(318) = term(318) * 8.0d+0 
! term(319) = term(319) * 8.0d+0 
! term(320) = term(320) * (-16.0d+0) 

! do l = 1, nocc 
! do j = 1, nocc 
! term(321) = term(321) + wmo_interm_46_triplet(j, l) * wmo_interm_32_triplet(j, l)
! term(322) = term(322) + wmo_interm_46_triplet(j, l) * wmo_interm_33_triplet(j, l)
! term(323) = term(323) + wmo_interm_47_triplet(j, l) * wmo_interm_32_triplet(j, l)
! term(324) = term(324) + wmo_interm_47_triplet(j, l) * wmo_interm_33_triplet(j, l)
! term(325) = term(325) + wmo_interm_46_triplet(j, l) * wmo_interm_36_triplet(j, l)
! term(326) = term(326) + wmo_interm_46_triplet(j, l) * wmo_interm_37_triplet(j, l)
! term(327) = term(327) + wmo_interm_46_triplet(j, l) * wmo_interm_38_triplet(j, l)
! term(328) = term(328) + wmo_interm_46_triplet(j, l) * wmo_interm_39_triplet(j, l)
! term(329) = term(329) + wmo_interm_47_triplet(j, l) * wmo_interm_36_triplet(j, l)
! term(330) = term(330) + wmo_interm_47_triplet(j, l) * wmo_interm_37_triplet(j, l)
! term(331) = term(331) + wmo_interm_47_triplet(j, l) * wmo_interm_38_triplet(j, l)
! term(332) = term(332) + wmo_interm_47_triplet(j, l) * wmo_interm_39_triplet(j, l)
! term(333) = term(333) + wmo_interm_52_triplet(j, l) * wmo_interm_32_triplet(j, l)
! term(334) = term(334) + wmo_interm_52_triplet(j, l) * wmo_interm_33_triplet(j, l)
! term(335) = term(335) + wmo_interm_53_triplet(j, l) * wmo_interm_32_triplet(j, l)
! term(336) = term(336) + wmo_interm_53_triplet(j, l) * wmo_interm_33_triplet(j, l)
! term(337) = term(337) + wmo_interm_52_triplet(j, l) * wmo_interm_36_triplet(j, l)
! term(338) = term(338) + wmo_interm_52_triplet(j, l) * wmo_interm_37_triplet(j, l)
! term(339) = term(339) + wmo_interm_52_triplet(j, l) * wmo_interm_38_triplet(j, l)
! term(340) = term(340) + wmo_interm_52_triplet(j, l) * wmo_interm_39_triplet(j, l)
! term(341) = term(341) + wmo_interm_53_triplet(j, l) * wmo_interm_36_triplet(j, l)
! term(342) = term(342) + wmo_interm_53_triplet(j, l) * wmo_interm_37_triplet(j, l)
! term(343) = term(343) + wmo_interm_53_triplet(j, l) * wmo_interm_38_triplet(j, l)
! term(344) = term(344) + wmo_interm_53_triplet(j, l) * wmo_interm_39_triplet(j, l)
! end do 
! end do 

! term(321) = term(321) * (-4.5d+0) 
! term(322) = term(322) * 4.5d+0 
! term(323) = term(323) * 4.5d+0 
! term(324) = term(324) * (-4.5d+0) 
! term(325) = term(325) * 3.0d+0 
! term(326) = term(326) * (-6.0d+0) 
! term(327) = term(327) * 3.0d+0 
! term(328) = term(328) * (-6.0d+0) 
! term(329) = term(329) * (-3.0d+0) 
! term(330) = term(330) * 6.0d+0 
! term(331) = term(331) * (-3.0d+0) 
! term(332) = term(332) * 6.0d+0 
! term(333) = term(333) * 3.0d+0 
! term(334) = term(334) * (-3.0d+0) 
! term(335) = term(335) * (-6.0d+0) 
! term(336) = term(336) * 6.0d+0 
! term(337) = term(337) * (-2.0d+0) 
! term(338) = term(338) * 4.0d+0 
! term(339) = term(339) * (-2.0d+0) 
! term(340) = term(340) * 4.0d+0 
! term(341) = term(341) * 4.0d+0 
! term(342) = term(342) * (-8.0d+0) 
! term(343) = term(343) * 4.0d+0 
! term(344) = term(344) * (-8.0d+0) 

! do l = 1, nocc 
! do k = 1, nocc 
! do j = 1, nocc 
! do i = 1, nocc 
! term(345) = term(345) + wmo_interm_65_triplet(i, j, l, k) * wmo_interm_66_triplet(i, j, k, l)
! term(346) = term(346) + wmo_interm_65_triplet(i, j, k, l) * wmo_interm_66_triplet(i, j, k, l)
! term(347) = term(347) + wmo_interm_65_triplet(i, j, l, k) * wmo_interm_68_triplet(i, j, k, l)
! term(348) = term(348) + wmo_interm_65_triplet(i, j, l, k) * wmo_interm_68_triplet(j, i, k, l)
! term(349) = term(349) + wmo_interm_65_triplet(i, j, k, l) * wmo_interm_68_triplet(j, i, k, l)
! term(350) = term(350) + wmo_interm_65_triplet(i, j, k, l) * wmo_interm_68_triplet(i, j, k, l)
! term(351) = term(351) + wmo_interm_70_triplet(i, j, l, k) * wmo_interm_66_triplet(i, j, k, l)
! term(352) = term(352) + wmo_interm_70_triplet(i, j, k, l) * wmo_interm_66_triplet(i, j, k, l)
! term(353) = term(353) + wmo_interm_70_triplet(i, j, l, k) * wmo_interm_68_triplet(i, j, k, l)
! term(354) = term(354) + wmo_interm_70_triplet(i, j, l, k) * wmo_interm_68_triplet(j, i, k, l)
! term(355) = term(355) + wmo_interm_70_triplet(i, j, k, l) * wmo_interm_68_triplet(j, i, k, l)
! term(356) = term(356) + wmo_interm_70_triplet(i, j, k, l) * wmo_interm_68_triplet(i, j, k, l)
! end do 
! end do 
! end do 
! end do 

! term(345) = term(345) * (-1.5d+0) 
! term(346) = term(346) * 1.5d+0 
! term(347) = term(347) * (-1.5d+0) 
! term(348) = term(348) * 1.5d+0 
! term(349) = term(349) * (-1.5d+0) 
! term(350) = term(350) * 1.5d+0 
! term(351) = term(351) * (-3.0d+0) 
! term(352) = term(352) * 3.0d+0 
! term(353) = term(353) * (-2.0d+0) 
! term(354) = term(354) * 4.0d+0 
! term(355) = term(355) * (-2.0d+0) 
! term(356) = term(356) * 4.0d+0 

! do d = nocc + 1, nactive 
! do k = 1, nocc 
! do b = nocc + 1, nactive 
! term(357) = term(357) + s1(d,k) * wmo_interm_43_triplet(b, d) * wmo_interm_5_triplet(b, k)
! term(358) = term(358) + s1(d,k) * wmo_interm_45_triplet(b, d) * wmo_interm_5_triplet(b, k)
! term(359) = term(359) + s1(d,k) * wmo_interm_43_triplet(b, d) * wmo_interm_6_triplet(b, k)
! term(360) = term(360) + s1(d,k) * wmo_interm_45_triplet(b, d) * wmo_interm_6_triplet(b, k)
! term(361) = term(361) + s1(d,k) * wmo_interm_49_triplet(b, d) * wmo_interm_5_triplet(b, k)
! term(362) = term(362) + s1(d,k) * wmo_interm_51_triplet(b, d) * wmo_interm_5_triplet(b, k)
! term(363) = term(363) + s1(d,k) * wmo_interm_49_triplet(b, d) * wmo_interm_6_triplet(b, k)
! term(364) = term(364) + s1(d,k) * wmo_interm_51_triplet(b, d) * wmo_interm_6_triplet(b, k)
! end do 
! end do 
! end do 

! term(357) = term(357) * (-9.0d+0) 
! term(358) = term(358) * 9.0d+0 
! term(359) = term(359) * 12.0d+0 
! term(360) = term(360) * (-12.0d+0) 
! term(361) = term(361) * (-6.0d+0) 
! term(362) = term(362) * 12.0d+0 
! term(363) = term(363) * 8.0d+0 
! term(364) = term(364) * (-16.0d+0) 

! do k = 1, nocc 
! do d = nocc + 1, nactive 
! do l = 1, nocc 
! do c = nocc + 1, nactive 
! term(365) = term(365) + s2(c,d,l,k) * wmo_interm_56_triplet(d, l) * wmo_interm_6_triplet(c, k)
! term(366) = term(366) + s2(c,d,l,k) * wmo_interm_62_triplet(d, l) * wmo_interm_6_triplet(c, k)
! end do 
! end do 
! end do 
! end do 

! term(365) = term(365) * 4.0d+0 
! term(366) = term(366) * (-8.0d+0) 

! do l = 1, nocc 
! do i = 1, nocc 
! do k = 1, nocc 
! do j = 1, nocc 
! term(367) = term(367) + wmo_interm_63_triplet(i, j, k, l) * wmo_interm_64_triplet(k, l, j, i)
! term(368) = term(368) + wmo_interm_63_triplet(i, j, k, l) * wmo_interm_64_triplet(l, k, j, i)
! term(369) = term(369) + wmo_interm_67_triplet(i, j, k, l) * wmo_interm_64_triplet(k, l, j, i)
! term(370) = term(370) + wmo_interm_67_triplet(i, j, k, l) * wmo_interm_64_triplet(l, k, j, i)
! term(371) = term(371) + wmo_interm_69_triplet(i, j, k, l) * wmo_interm_64_triplet(k, l, j, i)
! term(372) = term(372) + wmo_interm_69_triplet(i, j, k, l) * wmo_interm_64_triplet(l, k, j, i)
! term(373) = term(373) + wmo_interm_71_triplet(i, j, k, l) * wmo_interm_64_triplet(k, l, j, i)
! term(374) = term(374) + wmo_interm_71_triplet(i, j, k, l) * wmo_interm_64_triplet(l, k, j, i)
! end do 
! end do 
! end do 
! end do 

! term(367) = term(367) * (-0.75d+0) 
! term(368) = term(368) * 0.75d+0 
! term(369) = term(369) * (-1.5d+0) 
! term(370) = term(370) * 1.5d+0 
! term(371) = term(371) * (-1.5d+0) 
! term(372) = term(372) * 1.5d+0 
! term(373) = term(373) * (-2.0d+0) 
! term(374) = term(374) * 4.0d+0 

! do l = 1, nocc 
! do j = 1, nocc 
! do d = nocc + 1, nactive 
! term(375) = term(375) + s1(d,l) * wmo_interm_46_triplet(j, l) * wmo_interm_5_triplet(d, j)
! term(376) = term(376) + s1(d,l) * wmo_interm_47_triplet(j, l) * wmo_interm_5_triplet(d, j)
! term(377) = term(377) + s1(d,l) * wmo_interm_46_triplet(j, l) * wmo_interm_7_triplet(d, j)
! term(378) = term(378) + s1(d,l) * wmo_interm_46_triplet(j, l) * wmo_interm_6_triplet(d, j)
! term(379) = term(379) + s1(d,l) * wmo_interm_47_triplet(j, l) * wmo_interm_7_triplet(d, j)
! term(380) = term(380) + s1(d,l) * wmo_interm_47_triplet(j, l) * wmo_interm_6_triplet(d, j)
! term(381) = term(381) + s1(d,l) * wmo_interm_52_triplet(j, l) * wmo_interm_5_triplet(d, j)
! term(382) = term(382) + s1(d,l) * wmo_interm_53_triplet(j, l) * wmo_interm_5_triplet(d, j)
! term(383) = term(383) + s1(d,l) * wmo_interm_52_triplet(j, l) * wmo_interm_7_triplet(d, j)
! term(384) = term(384) + s1(d,l) * wmo_interm_52_triplet(j, l) * wmo_interm_6_triplet(d, j)
! term(385) = term(385) + s1(d,l) * wmo_interm_53_triplet(j, l) * wmo_interm_7_triplet(d, j)
! term(386) = term(386) + s1(d,l) * wmo_interm_53_triplet(j, l) * wmo_interm_6_triplet(d, j)
! end do 
! end do 
! end do 

! term(375) = term(375) * (-4.5d+0) 
! term(376) = term(376) * 4.5d+0 
! term(377) = term(377) * 3.0d+0 
! term(378) = term(378) * (-6.0d+0) 
! term(379) = term(379) * (-3.0d+0) 
! term(380) = term(380) * 6.0d+0 
! term(381) = term(381) * 3.0d+0 
! term(382) = term(382) * (-6.0d+0) 
! term(383) = term(383) * (-2.0d+0) 
! term(384) = term(384) * 4.0d+0 
! term(385) = term(385) * 4.0d+0 
! term(386) = term(386) * (-8.0d+0) 

! do l = 1, nocc 
! do j = 1, nocc 
! do c = nocc + 1, nactive 
! term(387) = term(387) + s1(c,l) * wmo_interm_46_triplet(j, l) * wmo_interm_5_triplet(c, j)
! term(388) = term(388) + s1(c,l) * wmo_interm_47_triplet(j, l) * wmo_interm_5_triplet(c, j)
! term(389) = term(389) + s1(c,l) * wmo_interm_46_triplet(j, l) * wmo_interm_6_triplet(c, j)
! term(390) = term(390) + s1(c,l) * wmo_interm_46_triplet(j, l) * wmo_interm_7_triplet(c, j)
! term(391) = term(391) + s1(c,l) * wmo_interm_47_triplet(j, l) * wmo_interm_6_triplet(c, j)
! term(392) = term(392) + s1(c,l) * wmo_interm_47_triplet(j, l) * wmo_interm_7_triplet(c, j)
! term(393) = term(393) + s1(c,l) * wmo_interm_52_triplet(j, l) * wmo_interm_5_triplet(c, j)
! term(394) = term(394) + s1(c,l) * wmo_interm_53_triplet(j, l) * wmo_interm_5_triplet(c, j)
! term(395) = term(395) + s1(c,l) * wmo_interm_52_triplet(j, l) * wmo_interm_6_triplet(c, j)
! term(396) = term(396) + s1(c,l) * wmo_interm_52_triplet(j, l) * wmo_interm_7_triplet(c, j)
! term(397) = term(397) + s1(c,l) * wmo_interm_53_triplet(j, l) * wmo_interm_6_triplet(c, j)
! term(398) = term(398) + s1(c,l) * wmo_interm_53_triplet(j, l) * wmo_interm_7_triplet(c, j)
! end do 
! end do 
! end do 

! term(387) = term(387) * 4.5d+0 
! term(388) = term(388) * (-4.5d+0) 
! term(389) = term(389) * (-6.0d+0) 
! term(390) = term(390) * 3.0d+0 
! term(391) = term(391) * 6.0d+0 
! term(392) = term(392) * (-3.0d+0) 
! term(393) = term(393) * (-3.0d+0) 
! term(394) = term(394) * 6.0d+0 
! term(395) = term(395) * 4.0d+0 
! term(396) = term(396) * (-2.0d+0) 
! term(397) = term(397) * (-8.0d+0) 
! term(398) = term(398) * 4.0d+0 

! do k = 1, nocc 
! do j = 1, nocc 
! do c = nocc + 1, nactive 
! do b = nocc + 1, nactive 
! term(399) = term(399) + wmo_interm_79_triplet(b, c, j, k) * wmo_interm_73_triplet(c, b, k, j)
! term(400) = term(400) + wmo_interm_79_triplet(b, c, j, k) * wmo_interm_74_triplet(c, b, k, j)
! term(401) = term(401) + wmo_interm_79_triplet(b, c, j, k) * wmo_interm_75_triplet(c, b, k, j)
! term(402) = term(402) + wmo_interm_79_triplet(b, c, j, k) * wmo_interm_76_triplet(c, b, k, j)
! term(403) = term(403) + wmo_interm_81_triplet(b, c, j, k) * wmo_interm_73_triplet(c, b, k, j)
! term(404) = term(404) + wmo_interm_82_triplet(b, c, j, k) * wmo_interm_73_triplet(c, b, k, j)
! term(405) = term(405) + wmo_interm_81_triplet(b, c, j, k) * wmo_interm_74_triplet(c, b, k, j)
! term(406) = term(406) + wmo_interm_82_triplet(b, c, j, k) * wmo_interm_74_triplet(c, b, k, j)
! term(407) = term(407) + wmo_interm_81_triplet(b, c, j, k) * wmo_interm_75_triplet(c, b, k, j)
! term(408) = term(408) + wmo_interm_82_triplet(b, c, j, k) * wmo_interm_75_triplet(c, b, k, j)
! term(409) = term(409) + wmo_interm_81_triplet(b, c, j, k) * wmo_interm_76_triplet(c, b, k, j)
! term(410) = term(410) + wmo_interm_82_triplet(b, c, j, k) * wmo_interm_76_triplet(c, b, k, j)
! end do 
! end do 
! end do 
! end do 

! term(399) = term(399) * 6.0d+0 
! term(400) = term(400) * (-12.0d+0) 
! term(401) = term(401) * 24.0d+0 
! term(402) = term(402) * (-12.0d+0) 
! term(403) = term(403) * (-4.0d+0) 
! term(404) = term(404) * 8.0d+0 
! term(405) = term(405) * 8.0d+0 
! term(406) = term(406) * (-16.0d+0) 
! term(407) = term(407) * (-16.0d+0) 
! term(408) = term(408) * 32.0d+0 
! term(409) = term(409) * 8.0d+0 
! term(410) = term(410) * (-16.0d+0) 

! do k = 1, nocc 
! do i = 1, nocc 
! do c = nocc + 1, nactive 
! do b = nocc + 1, nactive 
! term(411) = term(411) + wmo_interm_80_triplet(b, c, i, k) * wmo_interm_73_triplet(c, b, k, i)
! term(412) = term(412) + wmo_interm_80_triplet(b, c, i, k) * wmo_interm_74_triplet(c, b, k, i)
! term(413) = term(413) + wmo_interm_80_triplet(b, c, i, k) * wmo_interm_75_triplet(c, b, k, i)
! term(414) = term(414) + wmo_interm_80_triplet(b, c, i, k) * wmo_interm_76_triplet(c, b, k, i)
! term(415) = term(415) + wmo_interm_83_triplet(b, c, i, k) * wmo_interm_73_triplet(c, b, k, i)
! term(416) = term(416) + wmo_interm_84_triplet(b, c, i, k) * wmo_interm_73_triplet(c, b, k, i)
! term(417) = term(417) + wmo_interm_84_triplet(b, c, i, k) * wmo_interm_74_triplet(c, b, k, i)
! term(418) = term(418) + wmo_interm_83_triplet(b, c, i, k) * wmo_interm_74_triplet(c, b, k, i)
! term(419) = term(419) + wmo_interm_84_triplet(b, c, i, k) * wmo_interm_75_triplet(c, b, k, i)
! term(420) = term(420) + wmo_interm_83_triplet(b, c, i, k) * wmo_interm_75_triplet(c, b, k, i)
! term(421) = term(421) + wmo_interm_84_triplet(b, c, i, k) * wmo_interm_76_triplet(c, b, k, i)
! term(422) = term(422) + wmo_interm_83_triplet(b, c, i, k) * wmo_interm_76_triplet(c, b, k, i)
! end do 
! end do 
! end do 
! end do 

! term(411) = term(411) * (-6.0d+0) 
! term(412) = term(412) * 6.0d+0 
! term(413) = term(413) * (-12.0d+0) 
! term(414) = term(414) * 6.0d+0 
! term(415) = term(415) * (-4.0d+0) 
! term(416) = term(416) * 8.0d+0 
! term(417) = term(417) * (-4.0d+0) 
! term(418) = term(418) * 8.0d+0 
! term(419) = term(419) * 8.0d+0 
! term(420) = term(420) * (-16.0d+0) 
! term(421) = term(421) * (-4.0d+0) 
! term(422) = term(422) * 8.0d+0 

! do k = 1, nocc 
! do j = 1, nocc 
! do c = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! term(423) = term(423) + wmo_interm_80_triplet(a, c, j, k) * wmo_interm_73_triplet(c, a, k, j)
! term(424) = term(424) + wmo_interm_80_triplet(a, c, j, k) * wmo_interm_74_triplet(c, a, k, j)
! term(425) = term(425) + wmo_interm_80_triplet(a, c, j, k) * wmo_interm_75_triplet(c, a, k, j)
! term(426) = term(426) + wmo_interm_80_triplet(a, c, j, k) * wmo_interm_76_triplet(c, a, k, j)
! term(427) = term(427) + wmo_interm_83_triplet(a, c, j, k) * wmo_interm_73_triplet(c, a, k, j)
! term(428) = term(428) + wmo_interm_84_triplet(a, c, j, k) * wmo_interm_73_triplet(c, a, k, j)
! term(429) = term(429) + wmo_interm_84_triplet(a, c, j, k) * wmo_interm_74_triplet(c, a, k, j)
! term(430) = term(430) + wmo_interm_83_triplet(a, c, j, k) * wmo_interm_74_triplet(c, a, k, j)
! term(431) = term(431) + wmo_interm_84_triplet(a, c, j, k) * wmo_interm_75_triplet(c, a, k, j)
! term(432) = term(432) + wmo_interm_83_triplet(a, c, j, k) * wmo_interm_75_triplet(c, a, k, j)
! term(433) = term(433) + wmo_interm_84_triplet(a, c, j, k) * wmo_interm_76_triplet(c, a, k, j)
! term(434) = term(434) + wmo_interm_83_triplet(a, c, j, k) * wmo_interm_76_triplet(c, a, k, j)
! end do 
! end do 
! end do 
! end do 

! term(423) = term(423) * (-6.0d+0) 
! term(424) = term(424) * 6.0d+0 
! term(425) = term(425) * (-12.0d+0) 
! term(426) = term(426) * 6.0d+0 
! term(427) = term(427) * (-4.0d+0) 
! term(428) = term(428) * 8.0d+0 
! term(429) = term(429) * (-4.0d+0) 
! term(430) = term(430) * 8.0d+0 
! term(431) = term(431) * 8.0d+0 
! term(432) = term(432) * (-16.0d+0) 
! term(433) = term(433) * (-4.0d+0) 
! term(434) = term(434) * 8.0d+0 


!     d_overlap_ccsd_triplet = 0.d+0
!     do s = 0, 434
!     d_overlap_ccsd_triplet = d_overlap_ccsd_triplet + term(s)
!     end do                                
    
    end function d_overlap_ccsd_triplet


    
    function d_overlap_cc3_triplet(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr)
    double precision :: d_overlap_cc3_triplet
    integer, intent(in) :: nocc, nactive
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2
    double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: t1
    double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2
    double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: s1
    double precision, dimension(:), intent(in) :: vrdav_Rl                                                                                                                              
    double precision, dimension(:), intent(in) :: vrdav_Rr 
    integer :: s ,l,d,i,a,j,b,k,c
    double precision, dimension(0:95) :: term
    term = 0.d+0
    do l = 1, nocc 
do d = nocc + 1, nactive 
term(0) = term(0) + wmo_interm_cc3_0_triplet(d, l) * wmo_interm_cc3_1_triplet(d, l)
term(1) = term(1) + wmo_interm_cc3_0_triplet(d, l) * wmo_interm_cc3_2_triplet(d, l)
term(2) = term(2) + wmo_interm_cc3_0_triplet(d, l) * wmo_interm_cc3_3_triplet(d, l)
term(3) = term(3) + wmo_interm_cc3_0_triplet(d, l) * wmo_interm_cc3_4_triplet(d, l)
term(4) = term(4) + wmo_interm_cc3_0_triplet(d, l) * wmo_interm_cc3_5_triplet(d, l)
term(5) = term(5) + wmo_interm_cc3_0_triplet(d, l) * wmo_interm_cc3_6_triplet(d, l)
term(6) = term(6) + wmo_interm_cc3_7_triplet(d, l) * wmo_interm_cc3_3_triplet(d, l)
term(7) = term(7) + wmo_interm_cc3_7_triplet(d, l) * wmo_interm_cc3_4_triplet(d, l)
term(8) = term(8) + wmo_interm_cc3_7_triplet(d, l) * wmo_interm_cc3_2_triplet(d, l)
term(9) = term(9) + wmo_interm_cc3_7_triplet(d, l) * wmo_interm_cc3_1_triplet(d, l)
term(10) = term(10) + wmo_interm_cc3_7_triplet(d, l) * wmo_interm_cc3_5_triplet(d, l)
term(11) = term(11) + wmo_interm_cc3_7_triplet(d, l) * wmo_interm_cc3_6_triplet(d, l)
term(12) = term(12) + wmo_interm_cc3_0_triplet(d, l) * wmo_interm_cc3_8_triplet(d, l)
term(13) = term(13) + wmo_interm_cc3_0_triplet(d, l) * wmo_interm_cc3_9_triplet(d, l)
term(14) = term(14) + wmo_interm_cc3_0_triplet(d, l) * wmo_interm_cc3_10_triplet(d, l)
term(15) = term(15) + wmo_interm_cc3_0_triplet(d, l) * wmo_interm_cc3_11_triplet(d, l)
term(16) = term(16) + wmo_interm_cc3_0_triplet(d, l) * wmo_interm_cc3_12_triplet(d, l)
term(17) = term(17) + wmo_interm_cc3_0_triplet(d, l) * wmo_interm_cc3_13_triplet(d, l)
term(18) = term(18) + wmo_interm_cc3_7_triplet(d, l) * wmo_interm_cc3_12_triplet(d, l)
term(19) = term(19) + wmo_interm_cc3_7_triplet(d, l) * wmo_interm_cc3_13_triplet(d, l)
term(20) = term(20) + wmo_interm_cc3_7_triplet(d, l) * wmo_interm_cc3_10_triplet(d, l)
term(21) = term(21) + wmo_interm_cc3_7_triplet(d, l) * wmo_interm_cc3_11_triplet(d, l)
term(22) = term(22) + wmo_interm_cc3_7_triplet(d, l) * wmo_interm_cc3_8_triplet(d, l)
term(23) = term(23) + wmo_interm_cc3_7_triplet(d, l) * wmo_interm_cc3_9_triplet(d, l)
term(24) = term(24) + wmo_interm_cc3_19_triplet(d, l) * wmo_interm_cc3_20_triplet(d, l)
term(25) = term(25) + wmo_interm_cc3_21_triplet(d, l) * wmo_interm_cc3_20_triplet(d, l)
term(26) = term(26) + wmo_interm_cc3_21_triplet(d, l) * wmo_interm_cc3_22_triplet(d, l)
term(27) = term(27) + wmo_interm_cc3_19_triplet(d, l) * wmo_interm_cc3_22_triplet(d, l)
term(28) = term(28) + wmo_interm_cc3_23_triplet(d, l) * wmo_interm_cc3_20_triplet(d, l)
term(29) = term(29) + wmo_interm_cc3_23_triplet(d, l) * wmo_interm_cc3_22_triplet(d, l)
term(30) = term(30) + wmo_interm_cc3_24_triplet(d, l) * wmo_interm_cc3_22_triplet(d, l)
term(31) = term(31) + wmo_interm_cc3_24_triplet(d, l) * wmo_interm_cc3_20_triplet(d, l)
term(32) = term(32) + wmo_interm_cc3_25_triplet(d, l) * wmo_interm_cc3_20_triplet(d, l)
term(33) = term(33) + wmo_interm_cc3_25_triplet(d, l) * wmo_interm_cc3_22_triplet(d, l)
term(34) = term(34) + wmo_interm_cc3_26_triplet(d, l) * wmo_interm_cc3_22_triplet(d, l)
term(35) = term(35) + wmo_interm_cc3_26_triplet(d, l) * wmo_interm_cc3_20_triplet(d, l)
term(36) = term(36) + wmo_interm_cc3_29_triplet(d, l) * wmo_interm_cc3_20_triplet(d, l)
term(37) = term(37) + wmo_interm_cc3_30_triplet(d, l) * wmo_interm_cc3_20_triplet(d, l)
term(38) = term(38) + wmo_interm_cc3_29_triplet(d, l) * wmo_interm_cc3_22_triplet(d, l)
term(39) = term(39) + wmo_interm_cc3_30_triplet(d, l) * wmo_interm_cc3_22_triplet(d, l)
term(40) = term(40) + wmo_interm_cc3_29_triplet(d, l) * wmo_interm_cc3_20_triplet(d, l)
term(41) = term(41) + wmo_interm_cc3_29_triplet(d, l) * wmo_interm_cc3_22_triplet(d, l)
term(42) = term(42) + wmo_interm_cc3_31_triplet(d, l) * wmo_interm_cc3_22_triplet(d, l)
term(43) = term(43) + wmo_interm_cc3_31_triplet(d, l) * wmo_interm_cc3_20_triplet(d, l)
term(44) = term(44) + wmo_interm_cc3_30_triplet(d, l) * wmo_interm_cc3_20_triplet(d, l)
term(45) = term(45) + wmo_interm_cc3_30_triplet(d, l) * wmo_interm_cc3_22_triplet(d, l)
term(46) = term(46) + wmo_interm_cc3_32_triplet(d, l) * wmo_interm_cc3_22_triplet(d, l)
term(47) = term(47) + wmo_interm_cc3_32_triplet(d, l) * wmo_interm_cc3_20_triplet(d, l)
end do 
end do 

term(0) = term(0) * 1.5d+0 
term(1) = term(1) * (-1.5d+0) 
term(2) = term(2) * (-3.0d+0) 
term(3) = term(3) * 3.0d+0 
term(4) = term(4) * (-1.5d+0) 
term(5) = term(5) * 1.5d+0 
term(6) = term(6) * 6.0d+0 
term(7) = term(7) * (-6.0d+0) 
term(8) = term(8) * 3.0d+0 
term(9) = term(9) * (-3.0d+0) 
term(10) = term(10) * 3.0d+0 
term(11) = term(11) * (-3.0d+0) 
term(12) = term(12) * (-1.9999999999999998d+0) 
term(13) = term(13) * 3.9999999999999996d+0 
term(14) = term(14) * (-1.9999999999999998d+0) 
term(15) = term(15) * 3.9999999999999996d+0 
term(16) = term(16) * 3.9999999999999996d+0 
term(17) = term(17) * (-7.999999999999999d+0) 
term(18) = term(18) * (-7.999999999999999d+0) 
term(19) = term(19) * 15.999999999999998d+0 
term(20) = term(20) * 4.0d+0 
term(21) = term(21) * (-8.0d+0) 
term(22) = term(22) * 4.0d+0 
term(23) = term(23) * (-8.0d+0) 
term(24) = term(24) * (-1.5000000000000004d+0) 
term(25) = term(25) * 1.4999999999999996d+0 
term(26) = term(26) * (-2.999999999999999d+0) 
term(27) = term(27) * 3.000000000000001d+0 
term(28) = term(28) * 1.5d+0 
term(29) = term(29) * (-3.0d+0) 
term(30) = term(30) * 6.0d+0 
term(31) = term(31) * (-3.0d+0) 
term(32) = term(32) * (-1.5d+0) 
term(33) = term(33) * 3.0d+0 
term(34) = term(34) * (-6.0d+0) 
term(35) = term(35) * 3.0d+0 
term(36) = term(36) * (-2.0d+0) 
term(37) = term(37) * 4.0d+0 
term(38) = term(38) * 4.0d+0 
term(39) = term(39) * (-8.0d+0) 
term(40) = term(40) * (-2.0d+0) 
term(41) = term(41) * 4.0d+0 
term(42) = term(42) * (-8.0d+0) 
term(43) = term(43) * 4.0d+0 
term(44) = term(44) * 4.0d+0 
term(45) = term(45) * (-8.0d+0) 
term(46) = term(46) * 16.0d+0 
term(47) = term(47) * (-8.0d+0) 

do j = 1, nocc 
do b = nocc + 1, nactive 
term(48) = term(48) + wmo_interm_cc3_27_triplet(b, j) * wmo_interm_cc3_15_triplet(b, j)
term(49) = term(49) + wmo_interm_cc3_27_triplet(b, j) * wmo_interm_cc3_16_triplet(b, j)
term(50) = term(50) + wmo_interm_cc3_27_triplet(b, j) * wmo_interm_cc3_15_triplet(b, j)
term(51) = term(51) + wmo_interm_cc3_27_triplet(b, j) * wmo_interm_cc3_16_triplet(b, j)
term(52) = term(52) + wmo_interm_cc3_27_triplet(b, j) * wmo_interm_cc3_17_triplet(b, j)
term(53) = term(53) + wmo_interm_cc3_27_triplet(b, j) * wmo_interm_cc3_18_triplet(b, j)
end do 
end do 

term(48) = term(48) * 1.9999999999999998d+0 
term(49) = term(49) * (-3.9999999999999996d+0) 
term(50) = term(50) * 1.9999999999999998d+0 
term(51) = term(51) * (-3.9999999999999996d+0) 
term(52) = term(52) * 7.999999999999999d+0 
term(53) = term(53) * (-3.9999999999999996d+0) 

do l = 1, nocc 
do k = 1, nocc 
do d = nocc + 1, nactive 
do c = nocc + 1, nactive 
term(54) = term(54) + wmo_interm_cc3_33_triplet(c, d, k, l) * wmo_interm_cc3_34_triplet(c, d, l, k)
term(55) = term(55) + wmo_interm_cc3_33_triplet(c, d, k, l) * wmo_interm_cc3_35_triplet(c, d, l, k)
term(56) = term(56) + wmo_interm_cc3_33_triplet(c, d, l, k) * wmo_interm_cc3_36_triplet(c, d, k, l)
term(57) = term(57) + wmo_interm_cc3_33_triplet(c, d, k, l) * wmo_interm_cc3_36_triplet(c, d, k, l)
term(58) = term(58) + wmo_interm_cc3_37_triplet(c, d, k, l) * wmo_interm_cc3_35_triplet(c, d, l, k)
term(59) = term(59) + wmo_interm_cc3_37_triplet(c, d, k, l) * wmo_interm_cc3_36_triplet(c, d, k, l)
term(60) = term(60) + wmo_interm_cc3_37_triplet(c, d, k, l) * wmo_interm_cc3_34_triplet(c, d, l, k)
term(61) = term(61) + wmo_interm_cc3_37_triplet(c, d, l, k) * wmo_interm_cc3_36_triplet(c, d, k, l)
term(62) = term(62) + wmo_interm_cc3_38_triplet(c, d, k, l) * wmo_interm_cc3_35_triplet(c, d, l, k)
term(63) = term(63) + wmo_interm_cc3_38_triplet(c, d, k, l) * wmo_interm_cc3_36_triplet(c, d, k, l)
term(64) = term(64) + wmo_interm_cc3_38_triplet(c, d, k, l) * wmo_interm_cc3_34_triplet(c, d, l, k)
term(65) = term(65) + wmo_interm_cc3_38_triplet(c, d, l, k) * wmo_interm_cc3_36_triplet(c, d, k, l)
end do 
end do 
end do 
end do 

term(54) = -term(54) 
term(55) = term(55) * 2.0000000000000004d+0 
term(56) = term(56) * 2.0000000000000004d+0 
term(57) = term(57) * (-4.000000000000001d+0) 
term(58) = -term(58) 
term(59) = term(59) * 2.0000000000000004d+0 
term(60) = term(60) * 2.0000000000000004d+0 
term(61) = term(61) * (-4.000000000000001d+0) 
term(62) = term(62) * (-4.666666666666668d+0) 
term(63) = term(63) * 8.000000000000002d+0 
term(64) = term(64) * 2.0000000000000004d+0 
term(65) = term(65) * (-4.000000000000001d+0) 

do k = 1, nocc 
do l = 1, nocc 
do d = nocc + 1, nactive 
do c = nocc + 1, nactive 
term(66) = term(66) + wmo_interm_cc3_33_triplet(c, d, l, k) * wmo_interm_cc3_35_triplet(c, d, l, k)
term(67) = term(67) + wmo_interm_cc3_33_triplet(c, d, l, k) * wmo_interm_cc3_34_triplet(c, d, l, k)
term(68) = term(68) + wmo_interm_cc3_37_triplet(c, d, l, k) * wmo_interm_cc3_34_triplet(c, d, l, k)
term(69) = term(69) + wmo_interm_cc3_37_triplet(c, d, l, k) * wmo_interm_cc3_35_triplet(c, d, l, k)
term(70) = term(70) + wmo_interm_cc3_38_triplet(c, d, l, k) * wmo_interm_cc3_35_triplet(c, d, l, k)
term(71) = term(71) + wmo_interm_cc3_38_triplet(c, d, l, k) * wmo_interm_cc3_34_triplet(c, d, l, k)
end do 
end do 
end do 
end do 

term(66) = -term(66) 
term(67) = term(67) * 2.0000000000000004d+0 
term(68) = -term(68) 
term(69) = term(69) * 2.0000000000000004d+0 
term(70) = term(70) * 2.0000000000000004d+0 
term(71) = term(71) * (-3.333333333333334d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
term(72) = term(72) + wmo_interm_cc3_28_triplet(b, i) * wmo_interm_cc3_15_triplet(b, i)
term(73) = term(73) + wmo_interm_cc3_28_triplet(b, i) * wmo_interm_cc3_16_triplet(b, i)
term(74) = term(74) + wmo_interm_cc3_28_triplet(b, i) * wmo_interm_cc3_17_triplet(b, i)
term(75) = term(75) + wmo_interm_cc3_28_triplet(b, i) * wmo_interm_cc3_18_triplet(b, i)
term(76) = term(76) + wmo_interm_cc3_28_triplet(b, i) * wmo_interm_cc3_16_triplet(b, i)
term(77) = term(77) + wmo_interm_cc3_28_triplet(b, i) * wmo_interm_cc3_15_triplet(b, i)
end do 
end do 

term(72) = -term(72) 
term(73) = term(73) * 1.9999999999999998d+0 
term(74) = term(74) * (-3.9999999999999996d+0) 
term(75) = term(75) * 1.9999999999999998d+0 
term(76) = term(76) * 1.9999999999999998d+0 
term(77) = -term(77) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(78) = term(78) + wmo_interm_cc3_14_triplet(a, i) * wmo_interm_cc3_15_triplet(a, i)
term(79) = term(79) + wmo_interm_cc3_14_triplet(a, i) * wmo_interm_cc3_16_triplet(a, i)
term(80) = term(80) + wmo_interm_cc3_14_triplet(a, i) * wmo_interm_cc3_15_triplet(a, i)
term(81) = term(81) + wmo_interm_cc3_14_triplet(a, i) * wmo_interm_cc3_16_triplet(a, i)
term(82) = term(82) + wmo_interm_cc3_14_triplet(a, i) * wmo_interm_cc3_17_triplet(a, i)
term(83) = term(83) + wmo_interm_cc3_14_triplet(a, i) * wmo_interm_cc3_18_triplet(a, i)
term(84) = term(84) + wmo_interm_cc3_27_triplet(a, i) * wmo_interm_cc3_15_triplet(a, i)
term(85) = term(85) + wmo_interm_cc3_27_triplet(a, i) * wmo_interm_cc3_16_triplet(a, i)
term(86) = term(86) + wmo_interm_cc3_27_triplet(a, i) * wmo_interm_cc3_15_triplet(a, i)
term(87) = term(87) + wmo_interm_cc3_27_triplet(a, i) * wmo_interm_cc3_16_triplet(a, i)
term(88) = term(88) + wmo_interm_cc3_27_triplet(a, i) * wmo_interm_cc3_17_triplet(a, i)
term(89) = term(89) + wmo_interm_cc3_27_triplet(a, i) * wmo_interm_cc3_18_triplet(a, i)
end do 
end do 

term(78) = term(78) * 3.000000000000001d+0 
term(79) = term(79) * (-6.000000000000002d+0) 
term(80) = term(80) * 3.0000000000000004d+0 
term(81) = term(81) * (-6.000000000000002d+0) 
term(82) = term(82) * 12.0d+0 
term(83) = term(83) * (-6.0d+0) 
term(84) = term(84) * 1.9999999999999998d+0 
term(85) = term(85) * (-3.9999999999999996d+0) 
term(86) = term(86) * 1.9999999999999998d+0 
term(87) = term(87) * (-3.9999999999999996d+0) 
term(88) = term(88) * 7.999999999999999d+0 
term(89) = term(89) * (-3.9999999999999996d+0) 

do j = 1, nocc 
do a = nocc + 1, nactive 
term(90) = term(90) + wmo_interm_cc3_28_triplet(a, j) * wmo_interm_cc3_15_triplet(a, j)
term(91) = term(91) + wmo_interm_cc3_28_triplet(a, j) * wmo_interm_cc3_16_triplet(a, j)
term(92) = term(92) + wmo_interm_cc3_28_triplet(a, j) * wmo_interm_cc3_15_triplet(a, j)
term(93) = term(93) + wmo_interm_cc3_28_triplet(a, j) * wmo_interm_cc3_16_triplet(a, j)
term(94) = term(94) + wmo_interm_cc3_28_triplet(a, j) * wmo_interm_cc3_17_triplet(a, j)
term(95) = term(95) + wmo_interm_cc3_28_triplet(a, j) * wmo_interm_cc3_18_triplet(a, j)
end do 
end do 

term(90) = -term(90) 
term(91) = term(91) * 1.9999999999999998d+0 
term(92) = -term(92) 
term(93) = term(93) * 1.9999999999999998d+0 
term(94) = term(94) * (-3.9999999999999996d+0) 
term(95) = term(95) * 1.9999999999999998d+0 


    d_overlap_cc3_triplet = 0.d+0
    do s = 0, 95
    d_overlap_cc3_triplet = d_overlap_cc3_triplet + term(s)
    end do                                                                                                  
    end function d_overlap_cc3_triplet
    

    

!  function d_overlap_cc3_triplet(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr)
!     double precision :: d_overlap_cc3_triplet
!     integer, intent(in) :: nocc, nactive
!     double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2
!     double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: t1
!     double precision, dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2
!     double precision, dimension(nocc+1:nactive,nocc), intent(in)                  :: s1
!     double precision, dimension(:), intent(in) :: vrdav_Rl                                                                                                                              
!     double precision, dimension(:), intent(in) :: vrdav_Rr 
!     integer :: s ,k,i,j,b,l,d,a,c
!     double precision, dimension(0:215) :: term
!     term = 0.d+0
!     do k = 1, nocc 
! do i = 1, nocc 
! do j = 1, nocc 
! do b = nocc + 1, nactive 
! term(0) = term(0) + wmo_interm_cc3_0_triplet(b, i, j, k) * wmo_interm_cc3_1_triplet(b, j, k, i)
! term(1) = term(1) + wmo_interm_cc3_0_triplet(b, i, j, k) * wmo_interm_cc3_2_triplet(b, k, j, i)
! term(2) = term(2) + wmo_interm_cc3_0_triplet(b, i, j, k) * wmo_interm_cc3_3_triplet(b, k, j, i)
! term(3) = term(3) + wmo_interm_cc3_0_triplet(b, i, j, k) * wmo_interm_cc3_2_triplet(b, j, k, i)
! term(4) = term(4) + wmo_interm_cc3_0_triplet(b, i, j, k) * wmo_interm_cc3_3_triplet(b, j, k, i)
! term(5) = term(5) + wmo_interm_cc3_0_triplet(b, i, j, k) * wmo_interm_cc3_1_triplet(b, k, j, i)
! term(6) = term(6) + wmo_interm_cc3_0_triplet(b, i, j, k) * wmo_interm_cc3_2_triplet(b, j, k, i)
! term(7) = term(7) + wmo_interm_cc3_0_triplet(b, i, j, k) * wmo_interm_cc3_2_triplet(b, k, j, i)
! term(8) = term(8) + wmo_interm_cc3_0_triplet(b, i, j, k) * wmo_interm_cc3_11_triplet(b, j, k, i)
! term(9) = term(9) + wmo_interm_cc3_0_triplet(b, i, j, k) * wmo_interm_cc3_3_triplet(b, j, k, i)
! term(10) = term(10) + wmo_interm_cc3_0_triplet(b, i, j, k) * wmo_interm_cc3_3_triplet(b, k, j, i)
! term(11) = term(11) + wmo_interm_cc3_0_triplet(b, i, j, k) * wmo_interm_cc3_11_triplet(b, k, j, i)
! term(12) = term(12) + wmo_interm_cc3_17_triplet(b, i, j, k) * wmo_interm_cc3_1_triplet(b, k, j, i)
! term(13) = term(13) + wmo_interm_cc3_17_triplet(b, i, j, k) * wmo_interm_cc3_1_triplet(b, j, k, i)
! term(14) = term(14) + wmo_interm_cc3_17_triplet(b, i, j, k) * wmo_interm_cc3_2_triplet(b, j, k, i)
! term(15) = term(15) + wmo_interm_cc3_17_triplet(b, i, j, k) * wmo_interm_cc3_2_triplet(b, k, j, i)
! term(16) = term(16) + wmo_interm_cc3_17_triplet(b, i, j, k) * wmo_interm_cc3_3_triplet(b, j, k, i)
! term(17) = term(17) + wmo_interm_cc3_17_triplet(b, i, j, k) * wmo_interm_cc3_3_triplet(b, k, j, i)
! term(18) = term(18) + wmo_interm_cc3_17_triplet(b, i, j, k) * wmo_interm_cc3_2_triplet(b, j, k, i)
! term(19) = term(19) + wmo_interm_cc3_17_triplet(b, i, j, k) * wmo_interm_cc3_2_triplet(b, k, j, i)
! term(20) = term(20) + wmo_interm_cc3_17_triplet(b, i, j, k) * wmo_interm_cc3_11_triplet(b, j, k, i)
! term(21) = term(21) + wmo_interm_cc3_17_triplet(b, i, j, k) * wmo_interm_cc3_11_triplet(b, k, j, i)
! term(22) = term(22) + wmo_interm_cc3_17_triplet(b, i, j, k) * wmo_interm_cc3_3_triplet(b, j, k, i)
! term(23) = term(23) + wmo_interm_cc3_17_triplet(b, i, j, k) * wmo_interm_cc3_3_triplet(b, k, j, i)
! term(24) = term(24) + wmo_interm_cc3_55_triplet(b, i, j, k) * wmo_interm_cc3_36_triplet(b, k, j, i)
! term(25) = term(25) + wmo_interm_cc3_55_triplet(b, i, j, k) * wmo_interm_cc3_39_triplet(b, k, j, i)
! term(26) = term(26) + wmo_interm_cc3_55_triplet(b, i, j, k) * wmo_interm_cc3_37_triplet(b, k, j, i)
! term(27) = term(27) + wmo_interm_cc3_55_triplet(b, i, j, k) * wmo_interm_cc3_38_triplet(b, k, j, i)
! term(28) = term(28) + wmo_interm_cc3_55_triplet(b, i, j, k) * wmo_interm_cc3_39_triplet(b, k, j, i)
! term(29) = term(29) + wmo_interm_cc3_55_triplet(b, i, j, k) * wmo_interm_cc3_36_triplet(b, k, j, i)
! end do 
! end do 
! end do 
! end do 

! term(0) = term(0) * (-2.5d+0) 
! term(1) = -term(1) 
! term(2) = term(2) * 4.0d+0 
! term(3) = term(3) * 2.0d+0 
! term(4) = term(4) * (-2.0d+0) 
! term(5) = term(5) * 0.5d+0 
! term(7) = term(7) * (-2.0d+0) 
! term(8) = term(8) * 2.5d+0 
! term(9) = term(9) * (-4.0d+0) 
! term(10) = term(10) * 2.0d+0 
! term(11) = term(11) * (-0.5d+0) 
! term(12) = term(12) * (-1.9999999999999998d+0) 
! term(13) = term(13) * 3.9999999999999996d+0 
! term(14) = term(14) * (-1.9999999999999998d+0) 
! term(15) = term(15) * 3.9999999999999996d+0 
! term(16) = term(16) * 3.9999999999999996d+0 
! term(17) = term(17) * (-7.999999999999999d+0) 
! term(18) = term(18) * (-1.9999999999999998d+0) 
! term(19) = term(19) * 3.9999999999999996d+0 
! term(20) = term(20) * (-1.9999999999999998d+0) 
! term(21) = term(21) * 3.9999999999999996d+0 
! term(22) = term(22) * 3.9999999999999996d+0 
! term(23) = term(23) * (-7.999999999999999d+0) 
! term(24) = term(24) * 1.9999999999999998d+0 
! term(25) = term(25) * (-3.9999999999999996d+0) 
! term(26) = -term(26) 
! term(27) = term(27) * 1.9999999999999998d+0 
! term(28) = term(28) * (-3.9999999999999996d+0) 
! term(29) = term(29) * 1.9999999999999998d+0 

! do i = 1, nocc 
! do a = nocc + 1, nactive 
! term(30) = term(30) + wmo_interm_cc3_30_triplet(a, i) * wmo_interm_cc3_31_triplet(a, i)
! term(31) = term(31) + wmo_interm_cc3_30_triplet(a, i) * wmo_interm_cc3_32_triplet(a, i)
! term(32) = term(32) + wmo_interm_cc3_30_triplet(a, i) * wmo_interm_cc3_31_triplet(a, i)
! term(33) = term(33) + wmo_interm_cc3_30_triplet(a, i) * wmo_interm_cc3_32_triplet(a, i)
! term(34) = term(34) + wmo_interm_cc3_30_triplet(a, i) * wmo_interm_cc3_33_triplet(a, i)
! term(35) = term(35) + wmo_interm_cc3_30_triplet(a, i) * wmo_interm_cc3_34_triplet(a, i)
! term(36) = term(36) + wmo_interm_cc3_52_triplet(a, i) * wmo_interm_cc3_31_triplet(a, i)
! term(37) = term(37) + wmo_interm_cc3_52_triplet(a, i) * wmo_interm_cc3_32_triplet(a, i)
! term(38) = term(38) + wmo_interm_cc3_52_triplet(a, i) * wmo_interm_cc3_31_triplet(a, i)
! term(39) = term(39) + wmo_interm_cc3_52_triplet(a, i) * wmo_interm_cc3_32_triplet(a, i)
! term(40) = term(40) + wmo_interm_cc3_52_triplet(a, i) * wmo_interm_cc3_33_triplet(a, i)
! term(41) = term(41) + wmo_interm_cc3_52_triplet(a, i) * wmo_interm_cc3_34_triplet(a, i)
! end do 
! end do 

! term(30) = term(30) * 3.000000000000001d+0 
! term(31) = term(31) * (-6.000000000000002d+0) 
! term(32) = term(32) * 3.0000000000000004d+0 
! term(33) = term(33) * (-6.000000000000002d+0) 
! term(34) = term(34) * 12.0d+0 
! term(35) = term(35) * (-6.0d+0) 
! term(36) = term(36) * 1.9999999999999998d+0 
! term(37) = term(37) * (-3.9999999999999996d+0) 
! term(38) = term(38) * 1.9999999999999998d+0 
! term(39) = term(39) * (-3.9999999999999996d+0) 
! term(40) = term(40) * 7.999999999999999d+0 
! term(41) = term(41) * (-3.9999999999999996d+0) 

! do l = 1, nocc 
! do d = nocc + 1, nactive 
! term(42) = term(42) + wmo_interm_cc3_4_triplet(d, l) * wmo_interm_cc3_5_triplet(d, l)
! term(43) = term(43) + wmo_interm_cc3_4_triplet(d, l) * wmo_interm_cc3_6_triplet(d, l)
! term(44) = term(44) + wmo_interm_cc3_4_triplet(d, l) * wmo_interm_cc3_7_triplet(d, l)
! term(45) = term(45) + wmo_interm_cc3_4_triplet(d, l) * wmo_interm_cc3_8_triplet(d, l)
! term(46) = term(46) + wmo_interm_cc3_4_triplet(d, l) * wmo_interm_cc3_9_triplet(d, l)
! term(47) = term(47) + wmo_interm_cc3_4_triplet(d, l) * wmo_interm_cc3_10_triplet(d, l)
! term(48) = term(48) + wmo_interm_cc3_16_triplet(d, l) * wmo_interm_cc3_7_triplet(d, l)
! term(49) = term(49) + wmo_interm_cc3_16_triplet(d, l) * wmo_interm_cc3_8_triplet(d, l)
! term(50) = term(50) + wmo_interm_cc3_16_triplet(d, l) * wmo_interm_cc3_6_triplet(d, l)
! term(51) = term(51) + wmo_interm_cc3_16_triplet(d, l) * wmo_interm_cc3_5_triplet(d, l)
! term(52) = term(52) + wmo_interm_cc3_16_triplet(d, l) * wmo_interm_cc3_9_triplet(d, l)
! term(53) = term(53) + wmo_interm_cc3_16_triplet(d, l) * wmo_interm_cc3_10_triplet(d, l)
! term(54) = term(54) + wmo_interm_cc3_4_triplet(d, l) * wmo_interm_cc3_18_triplet(d, l)
! term(55) = term(55) + wmo_interm_cc3_4_triplet(d, l) * wmo_interm_cc3_19_triplet(d, l)
! term(56) = term(56) + wmo_interm_cc3_4_triplet(d, l) * wmo_interm_cc3_20_triplet(d, l)
! term(57) = term(57) + wmo_interm_cc3_4_triplet(d, l) * wmo_interm_cc3_21_triplet(d, l)
! term(58) = term(58) + wmo_interm_cc3_4_triplet(d, l) * wmo_interm_cc3_22_triplet(d, l)
! term(59) = term(59) + wmo_interm_cc3_4_triplet(d, l) * wmo_interm_cc3_23_triplet(d, l)
! term(60) = term(60) + wmo_interm_cc3_16_triplet(d, l) * wmo_interm_cc3_22_triplet(d, l)
! term(61) = term(61) + wmo_interm_cc3_16_triplet(d, l) * wmo_interm_cc3_23_triplet(d, l)
! term(62) = term(62) + wmo_interm_cc3_16_triplet(d, l) * wmo_interm_cc3_20_triplet(d, l)
! term(63) = term(63) + wmo_interm_cc3_16_triplet(d, l) * wmo_interm_cc3_21_triplet(d, l)
! term(64) = term(64) + wmo_interm_cc3_16_triplet(d, l) * wmo_interm_cc3_18_triplet(d, l)
! term(65) = term(65) + wmo_interm_cc3_16_triplet(d, l) * wmo_interm_cc3_19_triplet(d, l)
! term(66) = term(66) + wmo_interm_cc3_40_triplet(d, l) * wmo_interm_cc3_41_triplet(d, l)
! term(67) = term(67) + wmo_interm_cc3_44_triplet(d, l) * wmo_interm_cc3_41_triplet(d, l)
! term(68) = term(68) + wmo_interm_cc3_44_triplet(d, l) * wmo_interm_cc3_45_triplet(d, l)
! term(69) = term(69) + wmo_interm_cc3_40_triplet(d, l) * wmo_interm_cc3_45_triplet(d, l)
! term(70) = term(70) + wmo_interm_cc3_46_triplet(d, l) * wmo_interm_cc3_41_triplet(d, l)
! term(71) = term(71) + wmo_interm_cc3_46_triplet(d, l) * wmo_interm_cc3_45_triplet(d, l)
! term(72) = term(72) + wmo_interm_cc3_48_triplet(d, l) * wmo_interm_cc3_45_triplet(d, l)
! term(73) = term(73) + wmo_interm_cc3_48_triplet(d, l) * wmo_interm_cc3_41_triplet(d, l)
! term(74) = term(74) + wmo_interm_cc3_49_triplet(d, l) * wmo_interm_cc3_41_triplet(d, l)
! term(75) = term(75) + wmo_interm_cc3_49_triplet(d, l) * wmo_interm_cc3_45_triplet(d, l)
! term(76) = term(76) + wmo_interm_cc3_51_triplet(d, l) * wmo_interm_cc3_45_triplet(d, l)
! term(77) = term(77) + wmo_interm_cc3_51_triplet(d, l) * wmo_interm_cc3_41_triplet(d, l)
! term(78) = term(78) + wmo_interm_cc3_56_triplet(d, l) * wmo_interm_cc3_41_triplet(d, l)
! term(79) = term(79) + wmo_interm_cc3_58_triplet(d, l) * wmo_interm_cc3_41_triplet(d, l)
! term(80) = term(80) + wmo_interm_cc3_56_triplet(d, l) * wmo_interm_cc3_45_triplet(d, l)
! term(81) = term(81) + wmo_interm_cc3_58_triplet(d, l) * wmo_interm_cc3_45_triplet(d, l)
! term(82) = term(82) + wmo_interm_cc3_56_triplet(d, l) * wmo_interm_cc3_41_triplet(d, l)
! term(83) = term(83) + wmo_interm_cc3_56_triplet(d, l) * wmo_interm_cc3_45_triplet(d, l)
! term(84) = term(84) + wmo_interm_cc3_60_triplet(d, l) * wmo_interm_cc3_45_triplet(d, l)
! term(85) = term(85) + wmo_interm_cc3_60_triplet(d, l) * wmo_interm_cc3_41_triplet(d, l)
! term(86) = term(86) + wmo_interm_cc3_58_triplet(d, l) * wmo_interm_cc3_41_triplet(d, l)
! term(87) = term(87) + wmo_interm_cc3_58_triplet(d, l) * wmo_interm_cc3_45_triplet(d, l)
! term(88) = term(88) + wmo_interm_cc3_63_triplet(d, l) * wmo_interm_cc3_45_triplet(d, l)
! term(89) = term(89) + wmo_interm_cc3_63_triplet(d, l) * wmo_interm_cc3_41_triplet(d, l)
! end do 
! end do 

! term(42) = term(42) * 1.5d+0 
! term(43) = term(43) * (-1.5d+0) 
! term(44) = term(44) * (-3.0d+0) 
! term(45) = term(45) * 3.0d+0 
! term(46) = term(46) * (-1.5d+0) 
! term(47) = term(47) * 1.5d+0 
! term(48) = term(48) * 6.0d+0 
! term(49) = term(49) * (-6.0d+0) 
! term(50) = term(50) * 3.0d+0 
! term(51) = term(51) * (-3.0d+0) 
! term(52) = term(52) * 3.0d+0 
! term(53) = term(53) * (-3.0d+0) 
! term(54) = term(54) * (-1.9999999999999998d+0) 
! term(55) = term(55) * 3.9999999999999996d+0 
! term(56) = term(56) * (-1.9999999999999998d+0) 
! term(57) = term(57) * 3.9999999999999996d+0 
! term(58) = term(58) * 3.9999999999999996d+0 
! term(59) = term(59) * (-7.999999999999999d+0) 
! term(60) = term(60) * (-7.999999999999999d+0) 
! term(61) = term(61) * 15.999999999999998d+0 
! term(62) = term(62) * 4.0d+0 
! term(63) = term(63) * (-8.0d+0) 
! term(64) = term(64) * 4.0d+0 
! term(65) = term(65) * (-8.0d+0) 
! term(66) = term(66) * (-1.5000000000000004d+0) 
! term(67) = term(67) * 1.4999999999999996d+0 
! term(68) = term(68) * (-2.999999999999999d+0) 
! term(69) = term(69) * 3.000000000000001d+0 
! term(70) = term(70) * 1.5d+0 
! term(71) = term(71) * (-3.0d+0) 
! term(72) = term(72) * 6.0d+0 
! term(73) = term(73) * (-3.0d+0) 
! term(74) = term(74) * (-1.5d+0) 
! term(75) = term(75) * 3.0d+0 
! term(76) = term(76) * (-6.0d+0) 
! term(77) = term(77) * 3.0d+0 
! term(78) = term(78) * (-2.0d+0) 
! term(79) = term(79) * 4.0d+0 
! term(80) = term(80) * 4.0d+0 
! term(81) = term(81) * (-8.0d+0) 
! term(82) = term(82) * (-2.0d+0) 
! term(83) = term(83) * 4.0d+0 
! term(84) = term(84) * (-8.0d+0) 
! term(85) = term(85) * 4.0d+0 
! term(86) = term(86) * 4.0d+0 
! term(87) = term(87) * (-8.0d+0) 
! term(88) = term(88) * 16.0d+0 
! term(89) = term(89) * (-8.0d+0) 

! do l = 1, nocc 
! do k = 1, nocc 
! do d = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! term(90) = term(90) + wmo_interm_cc3_82_triplet(c, d, k, l) * wmo_interm_cc3_83_triplet(c, d, l, k)
! term(91) = term(91) + wmo_interm_cc3_82_triplet(c, d, k, l) * wmo_interm_cc3_84_triplet(c, d, l, k)
! term(92) = term(92) + wmo_interm_cc3_82_triplet(c, d, l, k) * wmo_interm_cc3_85_triplet(c, d, k, l)
! term(93) = term(93) + wmo_interm_cc3_82_triplet(c, d, k, l) * wmo_interm_cc3_85_triplet(c, d, k, l)
! term(94) = term(94) + wmo_interm_cc3_86_triplet(c, d, k, l) * wmo_interm_cc3_84_triplet(c, d, l, k)
! term(95) = term(95) + wmo_interm_cc3_86_triplet(c, d, k, l) * wmo_interm_cc3_85_triplet(c, d, k, l)
! term(96) = term(96) + wmo_interm_cc3_86_triplet(c, d, k, l) * wmo_interm_cc3_83_triplet(c, d, l, k)
! term(97) = term(97) + wmo_interm_cc3_86_triplet(c, d, l, k) * wmo_interm_cc3_85_triplet(c, d, k, l)
! term(98) = term(98) + wmo_interm_cc3_87_triplet(c, d, k, l) * wmo_interm_cc3_84_triplet(c, d, l, k)
! term(99) = term(99) + wmo_interm_cc3_87_triplet(c, d, k, l) * wmo_interm_cc3_85_triplet(c, d, k, l)
! term(100) = term(100) + wmo_interm_cc3_87_triplet(c, d, k, l) * wmo_interm_cc3_83_triplet(c, d, l, k)
! term(101) = term(101) + wmo_interm_cc3_87_triplet(c, d, l, k) * wmo_interm_cc3_85_triplet(c, d, k, l)
! end do 
! end do 
! end do 
! end do 

! term(90) = -term(90) 
! term(91) = term(91) * 2.0000000000000004d+0 
! term(92) = term(92) * 2.0000000000000004d+0 
! term(93) = term(93) * (-4.000000000000001d+0) 
! term(94) = -term(94) 
! term(95) = term(95) * 2.0000000000000004d+0 
! term(96) = term(96) * 2.0000000000000004d+0 
! term(97) = term(97) * (-4.000000000000001d+0) 
! term(98) = term(98) * (-4.666666666666668d+0) 
! term(99) = term(99) * 8.000000000000002d+0 
! term(100) = term(100) * 2.0000000000000004d+0 
! term(101) = term(101) * (-4.000000000000001d+0) 

! do j = 1, nocc 
! do b = nocc + 1, nactive 
! term(102) = term(102) + wmo_interm_cc3_52_triplet(b, j) * wmo_interm_cc3_31_triplet(b, j)
! term(103) = term(103) + wmo_interm_cc3_52_triplet(b, j) * wmo_interm_cc3_32_triplet(b, j)
! term(104) = term(104) + wmo_interm_cc3_52_triplet(b, j) * wmo_interm_cc3_31_triplet(b, j)
! term(105) = term(105) + wmo_interm_cc3_52_triplet(b, j) * wmo_interm_cc3_32_triplet(b, j)
! term(106) = term(106) + wmo_interm_cc3_52_triplet(b, j) * wmo_interm_cc3_33_triplet(b, j)
! term(107) = term(107) + wmo_interm_cc3_52_triplet(b, j) * wmo_interm_cc3_34_triplet(b, j)
! end do 
! end do 

! term(102) = term(102) * 1.9999999999999998d+0 
! term(103) = term(103) * (-3.9999999999999996d+0) 
! term(104) = term(104) * 1.9999999999999998d+0 
! term(105) = term(105) * (-3.9999999999999996d+0) 
! term(106) = term(106) * 7.999999999999999d+0 
! term(107) = term(107) * (-3.9999999999999996d+0) 

! do k = 1, nocc 
! do i = 1, nocc 
! do j = 1, nocc 
! do a = nocc + 1, nactive 
! term(108) = term(108) + wmo_interm_cc3_35_triplet(a, i, j, k) * wmo_interm_cc3_36_triplet(a, k, j, i)
! term(109) = term(109) + wmo_interm_cc3_35_triplet(a, i, j, k) * wmo_interm_cc3_39_triplet(a, k, j, i)
! term(110) = term(110) + wmo_interm_cc3_35_triplet(a, i, j, k) * wmo_interm_cc3_38_triplet(a, k, j, i)
! term(111) = term(111) + wmo_interm_cc3_35_triplet(a, i, j, k) * wmo_interm_cc3_37_triplet(a, k, j, i)
! term(112) = term(112) + wmo_interm_cc3_35_triplet(a, i, j, k) * wmo_interm_cc3_39_triplet(a, k, j, i)
! term(113) = term(113) + wmo_interm_cc3_35_triplet(a, i, j, k) * wmo_interm_cc3_36_triplet(a, k, j, i)
! term(114) = term(114) + wmo_interm_cc3_53_triplet(a, i, j, k) * wmo_interm_cc3_36_triplet(a, k, j, i)
! term(115) = term(115) + wmo_interm_cc3_53_triplet(a, i, j, k) * wmo_interm_cc3_39_triplet(a, k, j, i)
! term(116) = term(116) + wmo_interm_cc3_53_triplet(a, i, j, k) * wmo_interm_cc3_38_triplet(a, k, j, i)
! term(117) = term(117) + wmo_interm_cc3_53_triplet(a, i, j, k) * wmo_interm_cc3_37_triplet(a, k, j, i)
! term(118) = term(118) + wmo_interm_cc3_53_triplet(a, i, j, k) * wmo_interm_cc3_39_triplet(a, k, j, i)
! term(119) = term(119) + wmo_interm_cc3_53_triplet(a, i, j, k) * wmo_interm_cc3_36_triplet(a, k, j, i)
! end do 
! end do 
! end do 
! end do 

! term(108) = term(108) * (-1.5833333333333333d+0) 
! term(109) = term(109) * 3.0d+0 
! term(110) = term(110) * (-1.5000000000000002d+0) 
! term(111) = term(111) * 1.4999999999999996d+0 
! term(112) = term(112) * 3.0000000000000004d+0 
! term(113) = term(113) * (-1.4166666666666667d+0) 
! term(114) = term(114) * (-1.1666666666666665d+0) 
! term(115) = term(115) * 1.9999999999999998d+0 
! term(116) = -term(116) 
! term(117) = term(117) * 1.9999999999999998d+0 
! term(118) = term(118) * 1.9999999999999998d+0 
! term(119) = term(119) * (-0.8333333333333333d+0) 

! do k = 1, nocc 
! do j = 1, nocc 
! do i = 1, nocc 
! do a = nocc + 1, nactive 
! term(120) = term(120) + wmo_interm_cc3_35_triplet(a, i, j, k) * wmo_interm_cc3_37_triplet(a, k, i, j)
! term(121) = term(121) + wmo_interm_cc3_35_triplet(a, i, j, k) * wmo_interm_cc3_38_triplet(a, k, i, j)
! term(122) = term(122) + wmo_interm_cc3_35_triplet(a, i, j, k) * wmo_interm_cc3_36_triplet(a, k, i, j)
! term(123) = term(123) + wmo_interm_cc3_35_triplet(a, i, j, k) * wmo_interm_cc3_39_triplet(a, k, i, j)
! term(124) = term(124) + wmo_interm_cc3_35_triplet(a, i, j, k) * wmo_interm_cc3_39_triplet(a, k, i, j)
! term(125) = term(125) + wmo_interm_cc3_35_triplet(a, i, j, k) * wmo_interm_cc3_36_triplet(a, k, i, j)
! term(126) = term(126) + wmo_interm_cc3_53_triplet(a, i, j, k) * wmo_interm_cc3_37_triplet(a, k, i, j)
! term(127) = term(127) + wmo_interm_cc3_53_triplet(a, i, j, k) * wmo_interm_cc3_38_triplet(a, k, i, j)
! term(128) = term(128) + wmo_interm_cc3_53_triplet(a, i, j, k) * wmo_interm_cc3_36_triplet(a, k, i, j)
! term(129) = term(129) + wmo_interm_cc3_53_triplet(a, i, j, k) * wmo_interm_cc3_39_triplet(a, k, i, j)
! term(130) = term(130) + wmo_interm_cc3_53_triplet(a, i, j, k) * wmo_interm_cc3_39_triplet(a, k, i, j)
! term(131) = term(131) + wmo_interm_cc3_53_triplet(a, i, j, k) * wmo_interm_cc3_36_triplet(a, k, i, j)
! end do 
! end do 
! end do 
! end do 

! term(120) = term(120) * (-1.5d+0) 
! term(121) = term(121) * 1.4999999999999998d+0 
! term(122) = term(122) * 1.75d+0 
! term(123) = term(123) * (-3.0d+0) 
! term(124) = term(124) * (-3.0d+0) 
! term(125) = term(125) * 1.25d+0 
! term(126) = -term(126) 
! term(127) = term(127) * 1.9999999999999998d+0 
! term(128) = term(128) * 2.333333333333333d+0 
! term(129) = term(129) * (-3.9999999999999996d+0) 
! term(130) = term(130) * (-3.9999999999999996d+0) 
! term(131) = term(131) * 1.6666666666666665d+0 

! do j = 1, nocc 
! do a = nocc + 1, nactive 
! term(132) = term(132) + wmo_interm_cc3_54_triplet(a, j) * wmo_interm_cc3_31_triplet(a, j)
! term(133) = term(133) + wmo_interm_cc3_54_triplet(a, j) * wmo_interm_cc3_32_triplet(a, j)
! term(134) = term(134) + wmo_interm_cc3_54_triplet(a, j) * wmo_interm_cc3_31_triplet(a, j)
! term(135) = term(135) + wmo_interm_cc3_54_triplet(a, j) * wmo_interm_cc3_32_triplet(a, j)
! term(136) = term(136) + wmo_interm_cc3_54_triplet(a, j) * wmo_interm_cc3_33_triplet(a, j)
! term(137) = term(137) + wmo_interm_cc3_54_triplet(a, j) * wmo_interm_cc3_34_triplet(a, j)
! end do 
! end do 

! term(132) = -term(132) 
! term(133) = term(133) * 1.9999999999999998d+0 
! term(134) = -term(134) 
! term(135) = term(135) * 1.9999999999999998d+0 
! term(136) = term(136) * (-3.9999999999999996d+0) 
! term(137) = term(137) * 1.9999999999999998d+0 

! do l = 1, nocc 
! do k = 1, nocc 
! do j = 1, nocc 
! do d = nocc + 1, nactive 
! term(138) = term(138) + wmo_interm_cc3_42_triplet(d, j, k, l) * wmo_interm_cc3_43_triplet(d, j, l, k)
! term(139) = term(139) + wmo_interm_cc3_47_triplet(d, j, k, l) * wmo_interm_cc3_43_triplet(d, j, l, k)
! term(140) = term(140) + wmo_interm_cc3_50_triplet(d, j, k, l) * wmo_interm_cc3_43_triplet(d, j, l, k)
! term(141) = term(141) + wmo_interm_cc3_57_triplet(d, j, k, l) * wmo_interm_cc3_43_triplet(d, j, l, k)
! term(142) = term(142) + wmo_interm_cc3_59_triplet(d, j, k, l) * wmo_interm_cc3_43_triplet(d, j, l, k)
! term(143) = term(143) + wmo_interm_cc3_62_triplet(d, j, k, l) * wmo_interm_cc3_43_triplet(d, j, l, k)
! end do 
! end do 
! end do 
! end do 

! term(138) = term(138) * 2.999999999999999d+0 
! term(139) = term(139) * 2.999999999999999d+0 
! term(140) = term(140) * (-3.0d+0) 
! term(141) = term(141) * 4.0d+0 
! term(142) = term(142) * (-2.0d+0) 
! term(143) = term(143) * 4.0d+0 

! do i = 1, nocc 
! do b = nocc + 1, nactive 
! term(144) = term(144) + wmo_interm_cc3_54_triplet(b, i) * wmo_interm_cc3_31_triplet(b, i)
! term(145) = term(145) + wmo_interm_cc3_54_triplet(b, i) * wmo_interm_cc3_32_triplet(b, i)
! term(146) = term(146) + wmo_interm_cc3_54_triplet(b, i) * wmo_interm_cc3_33_triplet(b, i)
! term(147) = term(147) + wmo_interm_cc3_54_triplet(b, i) * wmo_interm_cc3_34_triplet(b, i)
! term(148) = term(148) + wmo_interm_cc3_54_triplet(b, i) * wmo_interm_cc3_32_triplet(b, i)
! term(149) = term(149) + wmo_interm_cc3_54_triplet(b, i) * wmo_interm_cc3_31_triplet(b, i)
! end do 
! end do 

! term(144) = -term(144) 
! term(145) = term(145) * 1.9999999999999998d+0 
! term(146) = term(146) * (-3.9999999999999996d+0) 
! term(147) = term(147) * 1.9999999999999998d+0 
! term(148) = term(148) * 1.9999999999999998d+0 
! term(149) = -term(149) 

! do k = 1, nocc 
! do l = 1, nocc 
! do j = 1, nocc 
! do d = nocc + 1, nactive 
! term(150) = term(150) + wmo_interm_cc3_42_triplet(d, j, l, k) * wmo_interm_cc3_43_triplet(d, j, l, k)
! term(151) = term(151) + wmo_interm_cc3_47_triplet(d, j, l, k) * wmo_interm_cc3_43_triplet(d, j, l, k)
! term(152) = term(152) + wmo_interm_cc3_50_triplet(d, j, l, k) * wmo_interm_cc3_43_triplet(d, j, l, k)
! term(153) = term(153) + wmo_interm_cc3_57_triplet(d, j, l, k) * wmo_interm_cc3_43_triplet(d, j, l, k)
! term(154) = term(154) + wmo_interm_cc3_59_triplet(d, j, l, k) * wmo_interm_cc3_43_triplet(d, j, l, k)
! term(155) = term(155) + wmo_interm_cc3_62_triplet(d, j, l, k) * wmo_interm_cc3_43_triplet(d, j, l, k)
! end do 
! end do 
! end do 
! end do 

! term(150) = term(150) * (-3.0000000000000004d+0) 
! term(151) = term(151) * (-5.999999999999998d+0) 
! term(152) = term(152) * 6.0d+0 
! term(153) = term(153) * (-2.0d+0) 
! term(154) = term(154) * 4.0d+0 
! term(155) = term(155) * (-8.0d+0) 

! do l = 1, nocc 
! do k = 1, nocc 
! do i = 1, nocc 
! do d = nocc + 1, nactive 
! term(156) = term(156) + wmo_interm_cc3_12_triplet(d, i, l, k) * wmo_interm_cc3_13_triplet(d, i, k, l)
! term(157) = term(157) + wmo_interm_cc3_12_triplet(d, i, k, l) * wmo_interm_cc3_13_triplet(d, i, k, l)
! term(158) = term(158) + wmo_interm_cc3_12_triplet(d, i, k, l) * wmo_interm_cc3_15_triplet(d, i, l, k)
! term(159) = term(159) + wmo_interm_cc3_12_triplet(d, i, k, l) * wmo_interm_cc3_14_triplet(d, i, l, k)
! term(160) = term(160) + wmo_interm_cc3_12_triplet(d, i, l, k) * wmo_interm_cc3_24_triplet(d, i, k, l)
! term(161) = term(161) + wmo_interm_cc3_12_triplet(d, i, l, k) * wmo_interm_cc3_25_triplet(d, i, k, l)
! term(162) = term(162) + wmo_interm_cc3_12_triplet(d, i, k, l) * wmo_interm_cc3_25_triplet(d, i, k, l)
! term(163) = term(163) + wmo_interm_cc3_12_triplet(d, i, k, l) * wmo_interm_cc3_24_triplet(d, i, k, l)
! term(164) = term(164) + wmo_interm_cc3_12_triplet(d, i, k, l) * wmo_interm_cc3_28_triplet(d, i, l, k)
! term(165) = term(165) + wmo_interm_cc3_12_triplet(d, i, k, l) * wmo_interm_cc3_29_triplet(d, i, l, k)
! term(166) = term(166) + wmo_interm_cc3_12_triplet(d, i, k, l) * wmo_interm_cc3_26_triplet(d, i, l, k)
! term(167) = term(167) + wmo_interm_cc3_12_triplet(d, i, k, l) * wmo_interm_cc3_27_triplet(d, i, l, k)
! term(168) = term(168) + wmo_interm_cc3_61_triplet(d, i, k, l) * wmo_interm_cc3_43_triplet(d, i, l, k)
! term(169) = term(169) + wmo_interm_cc3_62_triplet(d, i, k, l) * wmo_interm_cc3_43_triplet(d, i, l, k)
! term(170) = term(170) + wmo_interm_cc3_59_triplet(d, i, k, l) * wmo_interm_cc3_43_triplet(d, i, l, k)
! end do 
! end do 
! end do 
! end do 

! term(156) = term(156) * 3.0d+0 
! term(157) = term(157) * (-3.0d+0) 
! term(158) = term(158) * (-3.0d+0) 
! term(159) = term(159) * 3.0d+0 
! term(160) = term(160) * (-2.0d+0) 
! term(161) = term(161) * 4.0d+0 
! term(162) = term(162) * (-2.0d+0) 
! term(163) = term(163) * 4.0d+0 
! term(164) = term(164) * (-2.0d+0) 
! term(165) = term(165) * 4.0d+0 
! term(166) = term(166) * (-2.0d+0) 
! term(167) = term(167) * 4.0d+0 
! term(168) = term(168) * (-2.0d+0) 
! term(169) = term(169) * 4.0d+0 
! term(170) = term(170) * (-2.0d+0) 

! do k = 1, nocc 
! do l = 1, nocc 
! do d = nocc + 1, nactive 
! do c = nocc + 1, nactive 
! term(171) = term(171) + wmo_interm_cc3_82_triplet(c, d, l, k) * wmo_interm_cc3_84_triplet(c, d, l, k)
! term(172) = term(172) + wmo_interm_cc3_82_triplet(c, d, l, k) * wmo_interm_cc3_83_triplet(c, d, l, k)
! term(173) = term(173) + wmo_interm_cc3_86_triplet(c, d, l, k) * wmo_interm_cc3_83_triplet(c, d, l, k)
! term(174) = term(174) + wmo_interm_cc3_86_triplet(c, d, l, k) * wmo_interm_cc3_84_triplet(c, d, l, k)
! term(175) = term(175) + wmo_interm_cc3_87_triplet(c, d, l, k) * wmo_interm_cc3_84_triplet(c, d, l, k)
! term(176) = term(176) + wmo_interm_cc3_87_triplet(c, d, l, k) * wmo_interm_cc3_83_triplet(c, d, l, k)
! end do 
! end do 
! end do 
! end do 

! term(171) = -term(171) 
! term(172) = term(172) * 2.0000000000000004d+0 
! term(173) = -term(173) 
! term(174) = term(174) * 2.0000000000000004d+0 
! term(175) = term(175) * 2.0000000000000004d+0 
! term(176) = term(176) * (-3.333333333333334d+0) 

! do k = 1, nocc 
! do l = 1, nocc 
! do i = 1, nocc 
! do d = nocc + 1, nactive 
! term(177) = term(177) + wmo_interm_cc3_12_triplet(d, i, l, k) * wmo_interm_cc3_14_triplet(d, i, l, k)
! term(178) = term(178) + wmo_interm_cc3_12_triplet(d, i, l, k) * wmo_interm_cc3_15_triplet(d, i, l, k)
! term(179) = term(179) + wmo_interm_cc3_12_triplet(d, i, l, k) * wmo_interm_cc3_26_triplet(d, i, l, k)
! term(180) = term(180) + wmo_interm_cc3_12_triplet(d, i, l, k) * wmo_interm_cc3_27_triplet(d, i, l, k)
! term(181) = term(181) + wmo_interm_cc3_12_triplet(d, i, l, k) * wmo_interm_cc3_28_triplet(d, i, l, k)
! term(182) = term(182) + wmo_interm_cc3_12_triplet(d, i, l, k) * wmo_interm_cc3_29_triplet(d, i, l, k)
! term(183) = term(183) + wmo_interm_cc3_61_triplet(d, i, l, k) * wmo_interm_cc3_43_triplet(d, i, l, k)
! term(184) = term(184) + wmo_interm_cc3_62_triplet(d, i, l, k) * wmo_interm_cc3_43_triplet(d, i, l, k)
! term(185) = term(185) + wmo_interm_cc3_59_triplet(d, i, l, k) * wmo_interm_cc3_43_triplet(d, i, l, k)
! end do 
! end do 
! end do 
! end do 

! term(177) = term(177) * (-6.0d+0) 
! term(178) = term(178) * 6.0d+0 
! term(179) = term(179) * 3.9999999999999996d+0 
! term(180) = term(180) * (-7.999999999999999d+0) 
! term(181) = term(181) * 3.9999999999999996d+0 
! term(182) = term(182) * (-7.999999999999999d+0) 
! term(183) = term(183) * 4.0d+0 
! term(184) = term(184) * (-8.0d+0) 
! term(185) = term(185) * 4.0d+0 

! do k = 1, nocc 
! do j = 1, nocc 
! do i = 1, nocc 
! do b = nocc + 1, nactive 
! term(186) = term(186) + wmo_interm_cc3_55_triplet(b, i, j, k) * wmo_interm_cc3_38_triplet(b, k, i, j)
! term(187) = term(187) + wmo_interm_cc3_55_triplet(b, i, j, k) * wmo_interm_cc3_37_triplet(b, k, i, j)
! term(188) = term(188) + wmo_interm_cc3_55_triplet(b, i, j, k) * wmo_interm_cc3_36_triplet(b, k, i, j)
! term(189) = term(189) + wmo_interm_cc3_55_triplet(b, i, j, k) * wmo_interm_cc3_39_triplet(b, k, i, j)
! term(190) = term(190) + wmo_interm_cc3_55_triplet(b, i, j, k) * wmo_interm_cc3_39_triplet(b, k, i, j)
! term(191) = term(191) + wmo_interm_cc3_55_triplet(b, i, j, k) * wmo_interm_cc3_36_triplet(b, k, i, j)
! end do 
! end do 
! end do 
! end do 

! term(186) = -term(186) 
! term(187) = term(187) * 1.9999999999999998d+0 
! term(188) = term(188) * (-1.1666666666666665d+0) 
! term(189) = term(189) * 1.9999999999999998d+0 
! term(190) = term(190) * 1.9999999999999998d+0 
! term(191) = term(191) * (-0.8333333333333333d+0) 

! do j = 1, nocc 
! do i = 1, nocc 
! term(192) = term(192) + wmo_interm_cc3_64_triplet(i, j) * wmo_interm_cc3_65_triplet(j, i)
! term(193) = term(193) + wmo_interm_cc3_64_triplet(i, j) * wmo_interm_cc3_66_triplet(j, i)
! term(194) = term(194) + wmo_interm_cc3_64_triplet(i, j) * wmo_interm_cc3_67_triplet(j, i)
! term(195) = term(195) + wmo_interm_cc3_64_triplet(i, j) * wmo_interm_cc3_68_triplet(j, i)
! term(196) = term(196) + wmo_interm_cc3_64_triplet(i, j) * wmo_interm_cc3_69_triplet(j, i)
! term(197) = term(197) + wmo_interm_cc3_64_triplet(i, j) * wmo_interm_cc3_70_triplet(j, i)
! term(198) = term(198) + wmo_interm_cc3_64_triplet(i, j) * wmo_interm_cc3_71_triplet(j, i)
! term(199) = term(199) + wmo_interm_cc3_64_triplet(i, j) * wmo_interm_cc3_72_triplet(j, i)
! term(200) = term(200) + wmo_interm_cc3_64_triplet(i, j) * wmo_interm_cc3_73_triplet(j, i)
! term(201) = term(201) + wmo_interm_cc3_64_triplet(i, j) * wmo_interm_cc3_74_triplet(j, i)
! term(202) = term(202) + wmo_interm_cc3_64_triplet(i, j) * wmo_interm_cc3_66_triplet(j, i)
! term(203) = term(203) + wmo_interm_cc3_64_triplet(i, j) * wmo_interm_cc3_70_triplet(j, i)
! term(204) = term(204) + wmo_interm_cc3_64_triplet(i, j) * wmo_interm_cc3_72_triplet(j, i)
! term(205) = term(205) + wmo_interm_cc3_64_triplet(i, j) * wmo_interm_cc3_70_triplet(j, i)
! term(206) = term(206) + wmo_interm_cc3_64_triplet(i, j) * wmo_interm_cc3_73_triplet(j, i)
! term(207) = term(207) + wmo_interm_cc3_64_triplet(i, j) * wmo_interm_cc3_69_triplet(j, i)
! term(208) = term(208) + wmo_interm_cc3_64_triplet(i, j) * wmo_interm_cc3_67_triplet(j, i)
! term(209) = term(209) + wmo_interm_cc3_64_triplet(i, j) * wmo_interm_cc3_66_triplet(j, i)
! end do 
! end do 

! term(192) = term(192) * (-0.3333333333333333d+0) 
! term(193) = term(193) * 0.6666666666666666d+0 
! term(194) = term(194) * (-0.3333333333333333d+0) 
! term(195) = term(195) * 0.6666666666666666d+0 
! term(196) = term(196) * 0.6666666666666666d+0 
! term(197) = term(197) * (-1.3333333333333333d+0) 
! term(198) = term(198) * (-0.3333333333333333d+0) 
! term(199) = term(199) * 0.6666666666666666d+0 
! term(200) = term(200) * (-0.3333333333333333d+0) 
! term(201) = term(201) * 0.6666666666666666d+0 
! term(202) = term(202) * 0.6666666666666666d+0 
! term(203) = term(203) * (-1.3333333333333333d+0) 
! term(204) = term(204) * 0.6666666666666666d+0 
! term(205) = term(205) * (-1.3333333333333333d+0) 
! term(206) = term(206) * (-0.3333333333333333d+0) 
! term(207) = term(207) * 0.6666666666666666d+0 
! term(208) = term(208) * (-0.3333333333333333d+0) 
! term(209) = term(209) * 0.6666666666666666d+0 

! do b = nocc + 1, nactive 
! do a = nocc + 1, nactive 
! term(210) = term(210) + wmo_interm_cc3_75_triplet(a, b) * wmo_interm_cc3_76_triplet(b, a)
! term(211) = term(211) + wmo_interm_cc3_75_triplet(a, b) * wmo_interm_cc3_77_triplet(b, a)
! term(212) = term(212) + wmo_interm_cc3_75_triplet(a, b) * wmo_interm_cc3_78_triplet(b, a)
! term(213) = term(213) + wmo_interm_cc3_75_triplet(a, b) * wmo_interm_cc3_79_triplet(b, a)
! term(214) = term(214) + wmo_interm_cc3_75_triplet(a, b) * wmo_interm_cc3_80_triplet(b, a)
! term(215) = term(215) + wmo_interm_cc3_75_triplet(a, b) * wmo_interm_cc3_81_triplet(b, a)
! end do 
! end do 

! term(210) = term(210) * 2.0000000000000004d+0 
! term(211) = -term(211) 
! term(212) = term(212) * (-4.000000000000001d+0) 
! term(213) = term(213) * 2.0000000000000004d+0 
! term(214) = -term(214) 
! term(215) = term(215) * 2.0000000000000004d+0 


!     d_overlap_cc3_triplet = 0.d+0
!     do s = 0, 215
!     d_overlap_cc3_triplet = d_overlap_cc3_triplet + term(s)
!     end do                                                                                                  
!     end function d_overlap_cc3_triplet
end module overlap_exc_exc_functions
