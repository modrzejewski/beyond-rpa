module density_exc_exc_functions_cc3_pt4
      

    use ccsd_transformed_integrals
    use cc3_intermediates
    use arithmetic
    use s_gen
    use basis
    use eom_vectors
    
    implicit none
    !
    ! File generated automatically on 2016-08-16 08:34:05
    !
    real(F64), dimension(:, :), allocatable :: wm_interm_cc3_0_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_1_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_2_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_3_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_4_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_5_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_6_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_7_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_8_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_9_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_10_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_11_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_12_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_13_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_14_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_15_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_16_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_17_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_18_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_19_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_20_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_21_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_22_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_23_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_24_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_25_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_26_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_27_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_28_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_29_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_30_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_31_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_32_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_33_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_34_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_35_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_36_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_37_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_38_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_39_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_40_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_41_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_42_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_43_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_44_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_45_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_46_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_47_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_48_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_49_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_cc3_50_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_51_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_52_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_53_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_54_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_55_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_56_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_57_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_58_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_59_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_60_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_61_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_62_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_63_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_64_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_65_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_66_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_67_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_68_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_69_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_70_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_cc3_71_pt4 

    contains
    
    subroutine wm_intermediates_cc3_init_pt4(nocc, nactive)
    integer, intent(in) :: nocc
    integer, intent(in) :: nactive
    allocate(wm_interm_cc3_0_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_cc3_1_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_2_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_3_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_4_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_5_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_6_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_7_pt4(1: nocc, 1: nocc))
allocate(wm_interm_cc3_8_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_9_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_10_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_11_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_12_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_13_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_14_pt4(1: nocc, 1: nocc))
allocate(wm_interm_cc3_15_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_16_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_17_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_cc3_18_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_19_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_20_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_21_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_22_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_23_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_24_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_25_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_cc3_26_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_27_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_28_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_29_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_30_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_31_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_32_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_33_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_34_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_35_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_36_pt4(1: nocc, 1: nocc))
allocate(wm_interm_cc3_37_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_38_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_39_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_40_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_41_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_42_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_43_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_44_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_45_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_46_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_47_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_48_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_49_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_50_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_cc3_51_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_52_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_53_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_54_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_55_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_56_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_57_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_58_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_59_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_60_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_61_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_62_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_63_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_64_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_65_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_66_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_67_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_68_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_69_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_70_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_cc3_71_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
wm_interm_cc3_0_pt4 = zero 
wm_interm_cc3_1_pt4 = zero 
wm_interm_cc3_2_pt4 = zero 
wm_interm_cc3_3_pt4 = zero 
wm_interm_cc3_4_pt4 = zero 
wm_interm_cc3_5_pt4 = zero 
wm_interm_cc3_6_pt4 = zero 
wm_interm_cc3_7_pt4 = zero 
wm_interm_cc3_8_pt4 = zero 
wm_interm_cc3_9_pt4 = zero 
wm_interm_cc3_10_pt4 = zero 
wm_interm_cc3_11_pt4 = zero 
wm_interm_cc3_12_pt4 = zero 
wm_interm_cc3_13_pt4 = zero 
wm_interm_cc3_14_pt4 = zero 
wm_interm_cc3_15_pt4 = zero 
wm_interm_cc3_16_pt4 = zero 
wm_interm_cc3_17_pt4 = zero 
wm_interm_cc3_18_pt4 = zero 
wm_interm_cc3_19_pt4 = zero 
wm_interm_cc3_20_pt4 = zero 
wm_interm_cc3_21_pt4 = zero 
wm_interm_cc3_22_pt4 = zero 
wm_interm_cc3_23_pt4 = zero 
wm_interm_cc3_24_pt4 = zero 
wm_interm_cc3_25_pt4 = zero 
wm_interm_cc3_26_pt4 = zero 
wm_interm_cc3_27_pt4 = zero 
wm_interm_cc3_28_pt4 = zero 
wm_interm_cc3_29_pt4 = zero 
wm_interm_cc3_30_pt4 = zero 
wm_interm_cc3_31_pt4 = zero 
wm_interm_cc3_32_pt4 = zero 
wm_interm_cc3_33_pt4 = zero 
wm_interm_cc3_34_pt4 = zero 
wm_interm_cc3_35_pt4 = zero 
wm_interm_cc3_36_pt4 = zero 
wm_interm_cc3_37_pt4 = zero 
wm_interm_cc3_38_pt4 = zero 
wm_interm_cc3_39_pt4 = zero 
wm_interm_cc3_40_pt4 = zero 
wm_interm_cc3_41_pt4 = zero 
wm_interm_cc3_42_pt4 = zero 
wm_interm_cc3_43_pt4 = zero 
wm_interm_cc3_44_pt4 = zero 
wm_interm_cc3_45_pt4 = zero 
wm_interm_cc3_46_pt4 = zero 
wm_interm_cc3_47_pt4 = zero 
wm_interm_cc3_48_pt4 = zero 
wm_interm_cc3_49_pt4 = zero 
wm_interm_cc3_50_pt4 = zero 
wm_interm_cc3_51_pt4 = zero 
wm_interm_cc3_52_pt4 = zero 
wm_interm_cc3_53_pt4 = zero 
wm_interm_cc3_54_pt4 = zero 
wm_interm_cc3_55_pt4 = zero 
wm_interm_cc3_56_pt4 = zero 
wm_interm_cc3_57_pt4 = zero 
wm_interm_cc3_58_pt4 = zero 
wm_interm_cc3_59_pt4 = zero 
wm_interm_cc3_60_pt4 = zero 
wm_interm_cc3_61_pt4 = zero 
wm_interm_cc3_62_pt4 = zero 
wm_interm_cc3_63_pt4 = zero 
wm_interm_cc3_64_pt4 = zero 
wm_interm_cc3_65_pt4 = zero 
wm_interm_cc3_66_pt4 = zero 
wm_interm_cc3_67_pt4 = zero 
wm_interm_cc3_68_pt4 = zero 
wm_interm_cc3_69_pt4 = zero 
wm_interm_cc3_70_pt4 = zero 
wm_interm_cc3_71_pt4 = zero 

    end subroutine wm_intermediates_cc3_init_pt4
    
    subroutine wm_intermediates_cc3_free_pt4
    deallocate(wm_interm_cc3_0_pt4)
deallocate(wm_interm_cc3_1_pt4)
deallocate(wm_interm_cc3_2_pt4)
deallocate(wm_interm_cc3_3_pt4)
deallocate(wm_interm_cc3_4_pt4)
deallocate(wm_interm_cc3_5_pt4)
deallocate(wm_interm_cc3_6_pt4)
deallocate(wm_interm_cc3_7_pt4)
deallocate(wm_interm_cc3_8_pt4)
deallocate(wm_interm_cc3_9_pt4)
deallocate(wm_interm_cc3_10_pt4)
deallocate(wm_interm_cc3_11_pt4)
deallocate(wm_interm_cc3_12_pt4)
deallocate(wm_interm_cc3_13_pt4)
deallocate(wm_interm_cc3_14_pt4)
deallocate(wm_interm_cc3_15_pt4)
deallocate(wm_interm_cc3_16_pt4)
deallocate(wm_interm_cc3_17_pt4)
deallocate(wm_interm_cc3_18_pt4)
deallocate(wm_interm_cc3_19_pt4)
deallocate(wm_interm_cc3_20_pt4)
deallocate(wm_interm_cc3_21_pt4)
deallocate(wm_interm_cc3_22_pt4)
deallocate(wm_interm_cc3_23_pt4)
deallocate(wm_interm_cc3_24_pt4)
deallocate(wm_interm_cc3_25_pt4)
deallocate(wm_interm_cc3_26_pt4)
deallocate(wm_interm_cc3_27_pt4)
deallocate(wm_interm_cc3_28_pt4)
deallocate(wm_interm_cc3_29_pt4)
deallocate(wm_interm_cc3_30_pt4)
deallocate(wm_interm_cc3_31_pt4)
deallocate(wm_interm_cc3_32_pt4)
deallocate(wm_interm_cc3_33_pt4)
deallocate(wm_interm_cc3_34_pt4)
deallocate(wm_interm_cc3_35_pt4)
deallocate(wm_interm_cc3_36_pt4)
deallocate(wm_interm_cc3_37_pt4)
deallocate(wm_interm_cc3_38_pt4)
deallocate(wm_interm_cc3_39_pt4)
deallocate(wm_interm_cc3_40_pt4)
deallocate(wm_interm_cc3_41_pt4)
deallocate(wm_interm_cc3_42_pt4)
deallocate(wm_interm_cc3_43_pt4)
deallocate(wm_interm_cc3_44_pt4)
deallocate(wm_interm_cc3_45_pt4)
deallocate(wm_interm_cc3_46_pt4)
deallocate(wm_interm_cc3_47_pt4)
deallocate(wm_interm_cc3_48_pt4)
deallocate(wm_interm_cc3_49_pt4)
deallocate(wm_interm_cc3_50_pt4)
deallocate(wm_interm_cc3_51_pt4)
deallocate(wm_interm_cc3_52_pt4)
deallocate(wm_interm_cc3_53_pt4)
deallocate(wm_interm_cc3_54_pt4)
deallocate(wm_interm_cc3_55_pt4)
deallocate(wm_interm_cc3_56_pt4)
deallocate(wm_interm_cc3_57_pt4)
deallocate(wm_interm_cc3_58_pt4)
deallocate(wm_interm_cc3_59_pt4)
deallocate(wm_interm_cc3_60_pt4)
deallocate(wm_interm_cc3_61_pt4)
deallocate(wm_interm_cc3_62_pt4)
deallocate(wm_interm_cc3_63_pt4)
deallocate(wm_interm_cc3_64_pt4)
deallocate(wm_interm_cc3_65_pt4)
deallocate(wm_interm_cc3_66_pt4)
deallocate(wm_interm_cc3_67_pt4)
deallocate(wm_interm_cc3_68_pt4)
deallocate(wm_interm_cc3_69_pt4)
deallocate(wm_interm_cc3_70_pt4)
deallocate(wm_interm_cc3_71_pt4)

    end subroutine wm_intermediates_cc3_free_pt4
    
    subroutine wm_intermediates_cc3_pt4(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr)
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
wm_interm_cc3_0_pt4(a, b) = wm_interm_cc3_0_pt4(a, b) + sum 
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
wm_interm_cc3_1_pt4(c, k) = wm_interm_cc3_1_pt4(c, k) + sum 
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
wm_interm_cc3_2_pt4(c, k) = wm_interm_cc3_2_pt4(c, k) + sum 
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
wm_interm_cc3_3_pt4(c, k) = wm_interm_cc3_3_pt4(c, k) + sum 
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
wm_interm_cc3_4_pt4(c, k) = wm_interm_cc3_4_pt4(c, k) + sum 
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
wm_interm_cc3_5_pt4(c, k) = wm_interm_cc3_5_pt4(c, k) + sum 
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
wm_interm_cc3_6_pt4(c, k) = wm_interm_cc3_6_pt4(c, k) + sum 
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
wm_interm_cc3_7_pt4(i, j) = wm_interm_cc3_7_pt4(i, j) + sum 
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
sum = sum + r2(vrdav_Rr, a,j,b,i) * t3(nocc, nactive, a,b,c,k,l,i)
end do 
end do 
end do 
wm_interm_cc3_8_pt4(c, j, k, l) = wm_interm_cc3_8_pt4(c, j, k, l) + sum 
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
sum = sum + r2(vrdav_Rr, a,j,b,i) * t3(nocc, nactive, a,b,c,i,k,l)
end do 
end do 
end do 
wm_interm_cc3_9_pt4(c, j, k, l) = wm_interm_cc3_9_pt4(c, j, k, l) + sum 
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
sum = sum + r2(vrdav_Rr, a,j,b,i) * t3(nocc, nactive, a,b,c,k,i,l)
end do 
end do 
end do 
wm_interm_cc3_10_pt4(c, j, k, l) = wm_interm_cc3_10_pt4(c, j, k, l) + sum 
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
wm_interm_cc3_11_pt4(b, j) = wm_interm_cc3_11_pt4(b, j) + sum 
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
wm_interm_cc3_12_pt4(b, j) = wm_interm_cc3_12_pt4(b, j) + sum 
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
wm_interm_cc3_13_pt4(c, j, l, k) = wm_interm_cc3_13_pt4(c, j, l, k) + sum 
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
wm_interm_cc3_14_pt4(i, j) = wm_interm_cc3_14_pt4(i, j) + sum 
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
wm_interm_cc3_15_pt4(c, j, k, l) = wm_interm_cc3_15_pt4(c, j, k, l) + sum 
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
wm_interm_cc3_16_pt4(c, j, k, l) = wm_interm_cc3_16_pt4(c, j, k, l) + sum 
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
wm_interm_cc3_17_pt4(a, b) = wm_interm_cc3_17_pt4(a, b) + sum 
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
wm_interm_cc3_18_pt4(c, j, k, l) = wm_interm_cc3_18_pt4(c, j, k, l) + sum 
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
sum = sum + r2(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,j,b,i,c,k)
end do 
end do 
end do 
end do 
wm_interm_cc3_19_pt4(c, k) = wm_interm_cc3_19_pt4(c, k) + sum 
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
wm_interm_cc3_20_pt4(c, k) = wm_interm_cc3_20_pt4(c, k) + sum 
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
wm_interm_cc3_21_pt4(c, k) = wm_interm_cc3_21_pt4(c, k) + sum 
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
wm_interm_cc3_22_pt4(c, k) = wm_interm_cc3_22_pt4(c, k) + sum 
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
wm_interm_cc3_23_pt4(b, i, j, k) = wm_interm_cc3_23_pt4(b, i, j, k) + sum 
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
wm_interm_cc3_24_pt4(b, j, i, k) = wm_interm_cc3_24_pt4(b, j, i, k) + sum 
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
wm_interm_cc3_25_pt4(a, b) = wm_interm_cc3_25_pt4(a, b) + sum 
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
wm_interm_cc3_26_pt4(b, j) = wm_interm_cc3_26_pt4(b, j) + sum 
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
wm_interm_cc3_27_pt4(b, j) = wm_interm_cc3_27_pt4(b, j) + sum 
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
wm_interm_cc3_28_pt4(c, j, k, l) = wm_interm_cc3_28_pt4(c, j, k, l) + sum 
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
wm_interm_cc3_29_pt4(c, j, k, l) = wm_interm_cc3_29_pt4(c, j, k, l) + sum 
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
wm_interm_cc3_30_pt4(c, k) = wm_interm_cc3_30_pt4(c, k) + sum 
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
wm_interm_cc3_31_pt4(c, k) = wm_interm_cc3_31_pt4(c, k) + sum 
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
wm_interm_cc3_32_pt4(c, k) = wm_interm_cc3_32_pt4(c, k) + sum 
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
wm_interm_cc3_33_pt4(c, j, l, k) = wm_interm_cc3_33_pt4(c, j, l, k) + sum 
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
sum = sum + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,b,c,k,j,i)
end do 
end do 
end do 
end do 
wm_interm_cc3_34_pt4(c, k) = wm_interm_cc3_34_pt4(c, k) + sum 
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
wm_interm_cc3_35_pt4(c, j, k, l) = wm_interm_cc3_35_pt4(c, j, k, l) + sum 
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
wm_interm_cc3_36_pt4(i, j) = wm_interm_cc3_36_pt4(i, j) + sum 
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
wm_interm_cc3_37_pt4(b, j, i, k) = wm_interm_cc3_37_pt4(b, j, i, k) + sum 
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
wm_interm_cc3_38_pt4(b, i, j, k) = wm_interm_cc3_38_pt4(b, i, j, k) + sum 
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
sum = sum + r2(vrdav_Rr, a,i,b,j) * s1(a,i)
end do 
end do 
wm_interm_cc3_39_pt4(b, j) = wm_interm_cc3_39_pt4(b, j) + sum 
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
wm_interm_cc3_40_pt4(b, j) = wm_interm_cc3_40_pt4(b, j) + sum 
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
sum = sum + r3(vrdav_Rl, a,j,b,k,c,i) * r2(vrdav_Rr, a,l,b,i)
end do 
end do 
end do 
wm_interm_cc3_41_pt4(c, j, k, l) = wm_interm_cc3_41_pt4(c, j, k, l) + sum 
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
sum = sum + r3(vrdav_Rl, a,j,b,k,c,i) * r2(vrdav_Rr, a,i,b,l)
end do 
end do 
end do 
wm_interm_cc3_42_pt4(c, j, k, l) = wm_interm_cc3_42_pt4(c, j, k, l) + sum 
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
sum = sum + r3(vrdav_Rl, a,k,b,j,c,i) * r2(vrdav_Rr, a,j,b,i)
end do 
end do 
end do 
end do 
wm_interm_cc3_43_pt4(c, k) = wm_interm_cc3_43_pt4(c, k) + sum 
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
sum = sum + r3(vrdav_Rl, a,k,b,j,c,i) * r2(vrdav_Rr, a,i,b,j)
end do 
end do 
end do 
end do 
wm_interm_cc3_44_pt4(c, k) = wm_interm_cc3_44_pt4(c, k) + sum 
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
sum = sum + r3(vrdav_Rl, a,k,b,i,c,j) * r2(vrdav_Rr, a,i,b,l)
end do 
end do 
end do 
wm_interm_cc3_45_pt4(c, k, j, l) = wm_interm_cc3_45_pt4(c, k, j, l) + sum 
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
sum = sum + r3(vrdav_Rl, a,k,b,i,c,j) * r2(vrdav_Rr, a,l,b,i)
end do 
end do 
end do 
wm_interm_cc3_46_pt4(c, k, j, l) = wm_interm_cc3_46_pt4(c, k, j, l) + sum 
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
sum = sum + r3(vrdav_Rl, a,j,b,i,c,k) * r2(vrdav_Rr, a,i,b,l)
end do 
end do 
end do 
wm_interm_cc3_47_pt4(c, j, k, l) = wm_interm_cc3_47_pt4(c, j, k, l) + sum 
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
sum = sum + r3(vrdav_Rl, a,j,b,i,c,k) * r2(vrdav_Rr, a,l,b,i)
end do 
end do 
end do 
wm_interm_cc3_48_pt4(c, j, k, l) = wm_interm_cc3_48_pt4(c, j, k, l) + sum 
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
sum = sum + r3(vrdav_Rl, a,i,b,j,c,k) * r2(vrdav_Rr, a,j,b,i)
end do 
end do 
end do 
end do 
wm_interm_cc3_49_pt4(c, k) = wm_interm_cc3_49_pt4(c, k) + sum 
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
sum = sum + r3(vrdav_Rl, a,i,b,j,c,k) * r2(vrdav_Rr, a,i,b,j)
end do 
end do 
end do 
end do 
wm_interm_cc3_50_pt4(c, k) = wm_interm_cc3_50_pt4(c, k) + sum 
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
sum = sum + r2(vrdav_Rr, a,j,b,i) * s1(a,k)
end do 
wm_interm_cc3_51_pt4(b, j, i, k) = wm_interm_cc3_51_pt4(b, j, i, k) + sum 
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
wm_interm_cc3_52_pt4(b, c, j, k) = wm_interm_cc3_52_pt4(b, c, j, k) + sum 
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
wm_interm_cc3_53_pt4(b, c, k, j) = wm_interm_cc3_53_pt4(b, c, k, j) + sum 
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
wm_interm_cc3_54_pt4(b, c, k, j) = wm_interm_cc3_54_pt4(b, c, k, j) + sum 
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
wm_interm_cc3_55_pt4(b, c, j, k) = wm_interm_cc3_55_pt4(b, c, j, k) + sum 
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
wm_interm_cc3_56_pt4(b, c, j, k) = wm_interm_cc3_56_pt4(b, c, j, k) + sum 
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
wm_interm_cc3_57_pt4(b, c, k, j) = wm_interm_cc3_57_pt4(b, c, k, j) + sum 
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
wm_interm_cc3_58_pt4(b, c, k, j) = wm_interm_cc3_58_pt4(b, c, k, j) + sum 
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
wm_interm_cc3_59_pt4(b, c, j, k) = wm_interm_cc3_59_pt4(b, c, j, k) + sum 
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
wm_interm_cc3_60_pt4(b, c, j, k) = wm_interm_cc3_60_pt4(b, c, j, k) + sum 
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
wm_interm_cc3_61_pt4(a, b, i, j) = wm_interm_cc3_61_pt4(a, b, i, j) + sum 
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
wm_interm_cc3_62_pt4(b, c, j, k) = wm_interm_cc3_62_pt4(b, c, j, k) + sum 
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
wm_interm_cc3_63_pt4(a, b, i, j) = wm_interm_cc3_63_pt4(a, b, i, j) + sum 
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
wm_interm_cc3_64_pt4(b, c, j, k) = wm_interm_cc3_64_pt4(b, c, j, k) + sum 
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
wm_interm_cc3_65_pt4(a, b, i, j) = wm_interm_cc3_65_pt4(a, b, i, j) + sum 
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
wm_interm_cc3_66_pt4(b, c, j, k) = wm_interm_cc3_66_pt4(b, c, j, k) + sum 
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
wm_interm_cc3_67_pt4(a, b, i, j) = wm_interm_cc3_67_pt4(a, b, i, j) + sum 
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
wm_interm_cc3_68_pt4(b, c, j, k) = wm_interm_cc3_68_pt4(b, c, j, k) + sum 
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
wm_interm_cc3_69_pt4(b, c, j, k) = wm_interm_cc3_69_pt4(b, c, j, k) + sum 
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
wm_interm_cc3_70_pt4(b, c, k, j) = wm_interm_cc3_70_pt4(b, c, k, j) + sum 
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
wm_interm_cc3_71_pt4(b, c, k, j) = wm_interm_cc3_71_pt4(b, c, k, j) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 



    end subroutine wm_intermediates_cc3_pt4
    

        
    function calc_D_oo_wm_cc3_pt4(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_oo_wm_cc3_pt4
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
term(0) = term(0) + wm_interm_cc3_52_pt4(a, b, p, i) * wm_interm_cc3_61_pt4(a, b, q, i)
term(1) = term(1) + wm_interm_cc3_52_pt4(a, b, i, p) * wm_interm_cc3_61_pt4(a, b, q, i)
term(2) = term(2) + wm_interm_cc3_53_pt4(a, b, p, i) * wm_interm_cc3_61_pt4(a, b, q, i)
term(3) = term(3) + wm_interm_cc3_53_pt4(a, b, i, p) * wm_interm_cc3_61_pt4(a, b, q, i)
term(4) = term(4) + wm_interm_cc3_54_pt4(a, b, p, i) * wm_interm_cc3_61_pt4(a, b, q, i)
term(5) = term(5) + wm_interm_cc3_54_pt4(a, b, i, p) * wm_interm_cc3_61_pt4(a, b, q, i)
term(6) = term(6) + wm_interm_cc3_63_pt4(a, b, q, i) * wm_interm_cc3_64_pt4(a, b, p, i)
term(7) = term(7) + wm_interm_cc3_62_pt4(a, b, p, i) * wm_interm_cc3_63_pt4(a, b, q, i)
term(8) = term(8) + wm_interm_cc3_63_pt4(a, b, q, i) * wm_interm_cc3_64_pt4(a, b, i, p)
term(9) = term(9) + wm_interm_cc3_63_pt4(a, b, i, q) * wm_interm_cc3_64_pt4(a, b, i, p)
term(10) = term(10) + wm_interm_cc3_63_pt4(a, b, i, q) * wm_interm_cc3_64_pt4(a, b, p, i)
term(11) = term(11) + wm_interm_cc3_62_pt4(a, b, i, p) * wm_interm_cc3_63_pt4(a, b, q, i)
term(12) = term(12) + wm_interm_cc3_62_pt4(a, b, p, i) * wm_interm_cc3_63_pt4(a, b, i, q)
term(13) = term(13) + wm_interm_cc3_62_pt4(a, b, i, p) * wm_interm_cc3_63_pt4(a, b, i, q)
term(14) = term(14) + wm_interm_cc3_56_pt4(a, b, q, i) * wm_interm_cc3_65_pt4(a, b, p, i)
term(15) = term(15) + wm_interm_cc3_59_pt4(a, b, q, i) * wm_interm_cc3_65_pt4(a, b, p, i)
term(16) = term(16) + wm_interm_cc3_57_pt4(a, b, q, i) * wm_interm_cc3_65_pt4(a, b, p, i)
term(17) = term(17) + wm_interm_cc3_59_pt4(a, b, i, q) * wm_interm_cc3_65_pt4(a, b, p, i)
term(18) = term(18) + wm_interm_cc3_60_pt4(a, b, q, i) * wm_interm_cc3_65_pt4(a, b, p, i)
term(19) = term(19) + wm_interm_cc3_56_pt4(a, b, i, q) * wm_interm_cc3_65_pt4(a, b, p, i)
term(20) = term(20) + wm_interm_cc3_57_pt4(a, b, i, q) * wm_interm_cc3_65_pt4(a, b, p, i)
term(21) = term(21) + wm_interm_cc3_59_pt4(a, b, q, i) * wm_interm_cc3_65_pt4(b, a, p, i)
term(22) = term(22) + wm_interm_cc3_56_pt4(a, b, q, i) * wm_interm_cc3_65_pt4(b, a, p, i)
term(23) = term(23) + wm_interm_cc3_60_pt4(a, b, q, i) * wm_interm_cc3_65_pt4(b, a, p, i)
term(24) = term(24) + wm_interm_cc3_56_pt4(a, b, i, q) * wm_interm_cc3_65_pt4(b, a, p, i)
term(25) = term(25) + wm_interm_cc3_60_pt4(a, b, i, q) * wm_interm_cc3_65_pt4(b, a, p, i)
term(26) = term(26) + wm_interm_cc3_59_pt4(a, b, i, q) * wm_interm_cc3_65_pt4(b, a, p, i)
term(27) = term(27) + wm_interm_cc3_60_pt4(a, b, i, q) * wm_interm_cc3_65_pt4(a, b, p, i)
term(28) = term(28) + wm_interm_cc3_67_pt4(a, b, q, i) * wm_interm_cc3_68_pt4(a, b, p, i)
term(29) = term(29) + wm_interm_cc3_66_pt4(a, b, p, i) * wm_interm_cc3_67_pt4(a, b, q, i)
term(30) = term(30) + wm_interm_cc3_67_pt4(a, b, q, i) * wm_interm_cc3_68_pt4(a, b, i, p)
term(31) = term(31) + wm_interm_cc3_67_pt4(a, b, q, i) * wm_interm_cc3_70_pt4(a, b, p, i)
term(32) = term(32) + wm_interm_cc3_67_pt4(a, b, q, i) * wm_interm_cc3_69_pt4(a, b, p, i)
term(33) = term(33) + wm_interm_cc3_66_pt4(a, b, i, p) * wm_interm_cc3_67_pt4(a, b, q, i)
term(34) = term(34) + wm_interm_cc3_67_pt4(a, b, q, i) * wm_interm_cc3_71_pt4(a, b, p, i)
term(35) = term(35) + wm_interm_cc3_67_pt4(a, b, q, i) * wm_interm_cc3_71_pt4(a, b, i, p)
term(36) = term(36) + wm_interm_cc3_67_pt4(a, b, i, q) * wm_interm_cc3_68_pt4(a, b, p, i)
term(37) = term(37) + wm_interm_cc3_67_pt4(a, b, i, q) * wm_interm_cc3_69_pt4(a, b, p, i)
term(38) = term(38) + wm_interm_cc3_66_pt4(a, b, p, i) * wm_interm_cc3_67_pt4(a, b, i, q)
term(39) = term(39) + wm_interm_cc3_66_pt4(a, b, i, p) * wm_interm_cc3_67_pt4(a, b, i, q)
term(40) = term(40) + wm_interm_cc3_67_pt4(a, b, q, i) * wm_interm_cc3_70_pt4(a, b, i, p)
term(41) = term(41) + wm_interm_cc3_67_pt4(a, b, q, i) * wm_interm_cc3_69_pt4(a, b, i, p)
term(42) = term(42) + wm_interm_cc3_67_pt4(a, b, i, q) * wm_interm_cc3_69_pt4(a, b, i, p)
term(43) = term(43) + wm_interm_cc3_67_pt4(a, b, i, q) * wm_interm_cc3_68_pt4(a, b, i, p)
term(44) = term(44) + wm_interm_cc3_67_pt4(a, b, i, q) * wm_interm_cc3_71_pt4(a, b, i, p)
term(45) = term(45) + wm_interm_cc3_67_pt4(a, b, i, q) * wm_interm_cc3_70_pt4(a, b, p, i)
term(46) = term(46) + wm_interm_cc3_67_pt4(a, b, i, q) * wm_interm_cc3_71_pt4(a, b, p, i)
term(47) = term(47) + wm_interm_cc3_67_pt4(a, b, i, q) * wm_interm_cc3_70_pt4(a, b, i, p)
end do 
end do 
end do 

term(0) = term(0) * 7.999999999999999d+0 
term(1) = term(1) * (-3.9999999999999996d+0) 
term(2) = term(2) * (-4.0d+0) 
term(3) = term(3) * 2.0d+0 
term(4) = term(4) * 1.9999999999999998d+0 
term(5) = term(5) * (-3.9999999999999996d+0) 
term(6) = term(6) * (-4.0d+0) 
term(7) = term(7) * 4.0d+0 
term(8) = term(8) * 2.0d+0 
term(9) = term(9) * (-4.0d+0) 
term(10) = term(10) * 2.0d+0 
term(11) = term(11) * (-2.0d+0) 
term(12) = term(12) * (-2.0d+0) 
term(13) = term(13) * 4.0d+0 
term(14) = term(14) * (-5.333333333333333d+0) 
term(15) = term(15) * 2.6666666666666665d+0 
term(16) = term(16) * 1.3333333333333333d+0 
term(17) = term(17) * (-1.3333333333333333d+0) 
term(18) = term(18) * (-0.6666666666666666d+0) 
term(19) = term(19) * 2.6666666666666665d+0 
term(20) = term(20) * (-0.6666666666666666d+0) 
term(21) = term(21) * (-0.6666666666666666d+0) 
term(22) = term(22) * 1.3333333333333333d+0 
term(23) = term(23) * 1.3333333333333333d+0 
term(24) = term(24) * (-2.6666666666666665d+0) 
term(25) = term(25) * (-0.6666666666666666d+0) 
term(26) = term(26) * 1.3333333333333333d+0 
term(27) = term(27) * 1.3333333333333333d+0 
term(28) = term(28) * 1.3333333333333333d+0 
term(29) = term(29) * (-4.0d+0) 
term(30) = term(30) * (-0.6666666666666666d+0) 
term(31) = term(31) * 1.3333333333333333d+0 
term(32) = term(32) * (-0.3333333333333333d+0) 
term(33) = term(33) * 2.0d+0 
term(34) = term(34) * (-0.3333333333333333d+0) 
term(35) = term(35) * 0.6666666666666666d+0 
term(36) = term(36) * (-0.6666666666666666d+0) 
term(37) = term(37) * 0.6666666666666666d+0 
term(38) = term(38) * 2.0d+0 
term(39) = term(39) * (-4.0d+0) 
term(40) = term(40) * (-0.6666666666666666d+0) 
term(41) = term(41) * 0.6666666666666666d+0 
term(42) = term(42) * (-0.3333333333333333d+0) 
term(43) = term(43) * 1.3333333333333333d+0 
term(44) = term(44) * (-0.3333333333333333d+0) 
term(45) = term(45) * (-0.6666666666666666d+0) 
term(46) = term(46) * 0.6666666666666666d+0 
term(47) = term(47) * 1.3333333333333333d+0 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(48) = term(48) + wm_interm_cc3_62_pt4(a, b, i, j) * wm_interm_cc3_63_pt4(a, b, i, j)
term(49) = term(49) + wm_interm_cc3_63_pt4(a, b, i, j) * wm_interm_cc3_64_pt4(a, b, i, j)
term(50) = term(50) + wm_interm_cc3_62_pt4(a, b, i, j) * wm_interm_cc3_63_pt4(a, b, j, i)
term(51) = term(51) + wm_interm_cc3_63_pt4(a, b, i, j) * wm_interm_cc3_64_pt4(a, b, j, i)
term(52) = term(52) + wm_interm_cc3_66_pt4(a, b, i, j) * wm_interm_cc3_67_pt4(a, b, i, j)
term(53) = term(53) + wm_interm_cc3_67_pt4(a, b, i, j) * wm_interm_cc3_68_pt4(a, b, i, j)
term(54) = term(54) + wm_interm_cc3_66_pt4(a, b, i, j) * wm_interm_cc3_67_pt4(a, b, j, i)
term(55) = term(55) + wm_interm_cc3_67_pt4(a, b, i, j) * wm_interm_cc3_69_pt4(a, b, i, j)
term(56) = term(56) + wm_interm_cc3_67_pt4(a, b, i, j) * wm_interm_cc3_68_pt4(a, b, j, i)
term(57) = term(57) + wm_interm_cc3_67_pt4(a, b, i, j) * wm_interm_cc3_70_pt4(a, b, i, j)
term(58) = term(58) + wm_interm_cc3_67_pt4(a, b, i, j) * wm_interm_cc3_69_pt4(a, b, j, i)
term(59) = term(59) + wm_interm_cc3_67_pt4(a, b, i, j) * wm_interm_cc3_71_pt4(a, b, i, j)
term(60) = term(60) + wm_interm_cc3_67_pt4(a, b, i, j) * wm_interm_cc3_71_pt4(a, b, j, i)
term(61) = term(61) + wm_interm_cc3_67_pt4(a, b, i, j) * wm_interm_cc3_70_pt4(a, b, j, i)
end do 
end do 
end do 
end do 

term(48) = term(48) * (-15.999999999999998d+0) 
term(49) = term(49) * 16.0d+0 
term(50) = term(50) * 8.0d+0 
term(51) = term(51) * (-8.0d+0) 
term(52) = term(52) * 16.0d+0 
term(53) = term(53) * (-5.333333333333333d+0) 
term(54) = term(54) * (-8.0d+0) 
term(55) = term(55) * 1.3333333333333333d+0 
term(56) = term(56) * 2.6666666666666665d+0 
term(57) = term(57) * (-5.333333333333333d+0) 
term(58) = term(58) * (-2.6666666666666665d+0) 
term(59) = term(59) * 1.3333333333333333d+0 
term(60) = term(60) * (-2.6666666666666665d+0) 
term(61) = term(61) * 2.6666666666666665d+0 


    calc_D_oo_wm_cc3_pt4 = zero
    do s = 0, 61
    calc_D_oo_wm_cc3_pt4 = calc_D_oo_wm_cc3_pt4 + term(s)
    end do

    end function calc_D_oo_wm_cc3_pt4
    
    function calc_D_ov_wm_cc3_pt4(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_ov_wm_cc3_pt4
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
    real(F64), dimension(0:69) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do a = nocc + 1, nactive 
term(0) = term(0) + wm_interm_cc3_0_pt4(q, a) * wm_interm_cc3_1_pt4(a, p)
term(1) = term(1) + wm_interm_cc3_0_pt4(q, a) * wm_interm_cc3_2_pt4(a, p)
term(2) = term(2) + wm_interm_cc3_0_pt4(q, a) * wm_interm_cc3_3_pt4(a, p)
term(3) = term(3) + wm_interm_cc3_0_pt4(q, a) * wm_interm_cc3_4_pt4(a, p)
term(4) = term(4) + wm_interm_cc3_0_pt4(q, a) * wm_interm_cc3_5_pt4(a, p)
term(5) = term(5) + wm_interm_cc3_0_pt4(q, a) * wm_interm_cc3_6_pt4(a, p)
term(6) = term(6) + wm_interm_cc3_17_pt4(a, q) * wm_interm_cc3_21_pt4(a, p)
term(7) = term(7) + wm_interm_cc3_17_pt4(a, q) * wm_interm_cc3_22_pt4(a, p)
term(8) = term(8) + wm_interm_cc3_17_pt4(a, q) * wm_interm_cc3_19_pt4(a, p)
term(9) = term(9) + wm_interm_cc3_17_pt4(a, q) * wm_interm_cc3_20_pt4(a, p)
end do 

term(0) = -term(0) 
term(1) = term(1) * 2.0d+0 
term(2) = -term(2) 
term(3) = term(3) * 2.0d+0 
term(4) = term(4) * 2.0d+0 
term(5) = term(5) * (-4.0d+0) 
term(6) = term(6) * (-1.9999999999999998d+0) 
term(7) = term(7) * 3.9999999999999996d+0 
term(8) = term(8) * 1.9999999999999998d+0 
term(9) = term(9) * (-3.9999999999999996d+0) 

do i = 1, nocc 
term(10) = term(10) + wm_interm_cc3_30_pt4(q, i) * wm_interm_cc3_36_pt4(p, i)
term(11) = term(11) + wm_interm_cc3_31_pt4(q, i) * wm_interm_cc3_36_pt4(p, i)
term(12) = term(12) + wm_interm_cc3_32_pt4(q, i) * wm_interm_cc3_36_pt4(p, i)
term(13) = term(13) + wm_interm_cc3_34_pt4(q, i) * wm_interm_cc3_36_pt4(p, i)
term(14) = term(14) + wm_interm_cc3_14_pt4(p, i) * wm_interm_cc3_43_pt4(q, i)
term(15) = term(15) + wm_interm_cc3_14_pt4(p, i) * wm_interm_cc3_44_pt4(q, i)
term(16) = term(16) + wm_interm_cc3_14_pt4(p, i) * wm_interm_cc3_49_pt4(q, i)
term(17) = term(17) + wm_interm_cc3_14_pt4(p, i) * wm_interm_cc3_50_pt4(q, i)
end do 

term(10) = term(10) * 2.0d+0 
term(11) = term(11) * (-4.0d+0) 
term(12) = term(12) * (-1.9999999999999998d+0) 
term(13) = term(13) * 3.9999999999999996d+0 
term(14) = term(14) * (-1.9999999999999998d+0) 
term(15) = term(15) * 3.9999999999999996d+0 
term(16) = term(16) * 2.0d+0 
term(17) = term(17) * (-4.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(18) = term(18) + s1(a,j) * s1(q,i) * wm_interm_cc3_15_pt4(a, p, j, i)
term(19) = term(19) + s1(a,j) * s1(q,i) * wm_interm_cc3_18_pt4(a, p, j, i)
term(20) = term(20) + s1(a,j) * s1(q,i) * wm_interm_cc3_16_pt4(a, p, j, i)
term(21) = term(21) + s1(a,j) * s1(q,i) * wm_interm_cc3_13_pt4(a, p, j, i)
term(22) = term(22) + t1(a,j) * t1(q,i) * wm_interm_cc3_47_pt4(a, j, i, p)
term(23) = term(23) + t1(a,j) * t1(q,i) * wm_interm_cc3_48_pt4(a, j, i, p)
term(24) = term(24) + t1(a,j) * t1(q,i) * wm_interm_cc3_45_pt4(a, j, i, p)
term(25) = term(25) + t1(a,j) * t1(q,i) * wm_interm_cc3_46_pt4(a, j, i, p)
term(26) = term(26) + t1(a,j) * t1(q,i) * wm_interm_cc3_41_pt4(a, j, i, p)
term(27) = term(27) + t1(a,j) * t1(q,i) * wm_interm_cc3_42_pt4(a, j, i, p)
end do 
end do 
end do 

term(18) = term(18) * 2.0d+0 
term(20) = term(20) * (-4.0d+0) 
term(21) = term(21) * (-2.0d+0) 
term(22) = -term(22) 
term(23) = term(23) * 2.0d+0 
term(24) = term(24) * 2.0d+0 
term(25) = term(25) * (-4.0d+0) 
term(26) = -term(26) 
term(27) = term(27) * 2.0d+0 

do i = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(28) = term(28) + s1(a,i) * t3(nocc, nactive, a,b,q,k,j,i) * wm_interm_cc3_37_pt4(b, k, j, p)
term(29) = term(29) + s1(a,i) * t3(nocc, nactive, a,b,q,k,j,i) * wm_interm_cc3_38_pt4(b, k, j, p)
term(30) = term(30) + s1(a,i) * t3(nocc, nactive, a,b,q,k,j,i) * wm_interm_cc3_37_pt4(b, j, k, p)
term(31) = term(31) + s1(a,i) * t3(nocc, nactive, a,b,q,k,j,i) * wm_interm_cc3_38_pt4(b, j, k, p)
term(32) = term(32) + s1(a,j) * t3(nocc, nactive, a,b,q,k,j,i) * wm_interm_cc3_37_pt4(b, i, k, p)
term(33) = term(33) + s1(a,j) * t3(nocc, nactive, a,b,q,k,j,i) * wm_interm_cc3_38_pt4(b, i, k, p)
term(34) = term(34) + s1(a,j) * t3(nocc, nactive, a,b,q,k,j,i) * wm_interm_cc3_37_pt4(b, k, i, p)
term(35) = term(35) + s1(a,j) * t3(nocc, nactive, a,b,q,k,j,i) * wm_interm_cc3_38_pt4(b, k, i, p)
term(36) = term(36) + r3(vrdav_Rl, a,j,b,k,q,i) * t1(b,k) * wm_interm_cc3_51_pt4(a, i, j, p)
term(37) = term(37) + r3(vrdav_Rl, a,j,b,k,q,i) * t1(b,k) * wm_interm_cc3_51_pt4(a, j, i, p)
term(38) = term(38) + r3(vrdav_Rl, a,j,b,k,q,i) * t1(b,j) * wm_interm_cc3_51_pt4(a, i, k, p)
term(39) = term(39) + r3(vrdav_Rl, a,j,b,k,q,i) * t1(b,j) * wm_interm_cc3_51_pt4(a, k, i, p)
term(40) = term(40) + r3(vrdav_Rl, a,j,b,k,q,i) * t1(b,i) * wm_interm_cc3_51_pt4(a, k, j, p)
term(41) = term(41) + r3(vrdav_Rl, a,j,b,k,q,i) * t1(b,i) * wm_interm_cc3_51_pt4(a, j, k, p)
term(42) = term(42) + r3(vrdav_Rl, a,j,b,k,q,i) * t1(a,k) * wm_interm_cc3_51_pt4(b, i, j, p)
term(43) = term(43) + r3(vrdav_Rl, a,j,b,k,q,i) * t1(a,k) * wm_interm_cc3_51_pt4(b, j, i, p)
term(44) = term(44) + r3(vrdav_Rl, a,j,b,k,q,i) * t1(a,j) * wm_interm_cc3_51_pt4(b, i, k, p)
term(45) = term(45) + r3(vrdav_Rl, a,j,b,k,q,i) * t1(a,j) * wm_interm_cc3_51_pt4(b, k, i, p)
term(46) = term(46) + r3(vrdav_Rl, a,j,b,k,q,i) * t1(a,i) * wm_interm_cc3_51_pt4(b, j, k, p)
term(47) = term(47) + r3(vrdav_Rl, a,j,b,k,q,i) * t1(a,i) * wm_interm_cc3_51_pt4(b, k, j, p)
end do 
end do 
end do 
end do 
end do 

term(28) = -term(28) 
term(29) = term(29) * 1.9999999999999998d+0 
term(30) = term(30) * 1.9999999999999998d+0 
term(31) = -term(31) 
term(32) = -term(32) 
term(33) = term(33) * 1.9999999999999998d+0 
term(34) = term(34) * 1.9999999999999998d+0 
term(35) = -term(35) 
term(36) = term(36) * 2.0d+0 
term(37) = term(37) * (-4.0d+0) 
term(38) = -term(38) 
term(39) = term(39) * 2.0d+0 
term(40) = -term(40) 
term(41) = term(41) * 2.0d+0 
term(42) = -term(42) 
term(43) = term(43) * 2.0d+0 
term(44) = term(44) * 2.0d+0 
term(45) = term(45) * (-4.0d+0) 
term(46) = -term(46) 
term(47) = term(47) * 2.0d+0 

do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(48) = term(48) + s1(a,j) * t3(nocc, nactive, a,b,q,j,k,i) * wm_interm_cc3_37_pt4(b, i, k, p)
term(49) = term(49) + s1(a,j) * t3(nocc, nactive, a,b,q,j,k,i) * wm_interm_cc3_38_pt4(b, i, k, p)
term(50) = term(50) + s1(a,j) * t3(nocc, nactive, a,b,q,j,k,i) * wm_interm_cc3_37_pt4(b, k, i, p)
term(51) = term(51) + s1(a,j) * t3(nocc, nactive, a,b,q,j,k,i) * wm_interm_cc3_38_pt4(b, k, i, p)
end do 
end do 
end do 
end do 
end do 

term(48) = term(48) * 1.9999999999999998d+0 
term(49) = term(49) * (-3.9999999999999996d+0) 
term(50) = term(50) * (-3.9999999999999996d+0) 
term(51) = term(51) * 1.9999999999999998d+0 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(52) = term(52) + s1(a,j) * s1(q,i) * wm_interm_cc3_15_pt4(a, p, i, j)
term(53) = term(53) + s1(a,j) * s1(q,i) * wm_interm_cc3_16_pt4(a, p, i, j)
term(54) = term(54) + s1(a,j) * s1(q,i) * wm_interm_cc3_18_pt4(a, p, i, j)
term(55) = term(55) + s1(a,j) * s1(q,i) * wm_interm_cc3_13_pt4(a, p, i, j)
term(56) = term(56) + s1(a,j) * t1(q,i) * wm_interm_cc3_15_pt4(a, i, p, j)
term(57) = term(57) + s1(a,j) * t1(q,i) * wm_interm_cc3_16_pt4(a, i, p, j)
term(58) = term(58) + s1(a,j) * t1(q,i) * wm_interm_cc3_15_pt4(a, i, j, p)
term(59) = term(59) + s1(a,j) * t1(q,i) * wm_interm_cc3_18_pt4(a, i, p, j)
term(60) = term(60) + s1(a,j) * t1(q,i) * wm_interm_cc3_18_pt4(a, i, j, p)
term(61) = term(61) + s1(a,j) * t1(q,i) * wm_interm_cc3_16_pt4(a, i, j, p)
term(62) = term(62) + s1(a,j) * t1(q,i) * wm_interm_cc3_13_pt4(a, i, p, j)
term(63) = term(63) + s1(a,j) * t1(q,i) * wm_interm_cc3_13_pt4(a, i, j, p)
term(64) = term(64) + t1(a,j) * t1(q,i) * wm_interm_cc3_47_pt4(a, i, j, p)
term(65) = term(65) + t1(a,j) * t1(q,i) * wm_interm_cc3_48_pt4(a, i, j, p)
term(66) = term(66) + t1(a,j) * t1(q,i) * wm_interm_cc3_42_pt4(a, i, j, p)
term(67) = term(67) + t1(a,j) * t1(q,i) * wm_interm_cc3_41_pt4(a, i, j, p)
term(68) = term(68) + t1(a,j) * t1(q,i) * wm_interm_cc3_45_pt4(a, i, j, p)
term(69) = term(69) + t1(a,j) * t1(q,i) * wm_interm_cc3_46_pt4(a, i, j, p)
end do 
end do 
end do 

term(52) = term(52) * (-4.0d+0) 
term(53) = term(53) * 8.0d+0 
term(54) = term(54) * (-2.0d+0) 
term(56) = term(56) * (-4.0d+0) 
term(57) = term(57) * 8.0d+0 
term(58) = term(58) * 2.0d+0 
term(59) = term(59) * (-2.0d+0) 
term(61) = term(61) * (-4.0d+0) 
term(63) = term(63) * (-2.0d+0) 
term(64) = term(64) * 2.0d+0 
term(65) = term(65) * (-4.0d+0) 
term(66) = -term(66) 
term(67) = term(67) * 2.0d+0 
term(68) = -term(68) 
term(69) = term(69) * 2.0d+0 


    calc_D_ov_wm_cc3_pt4 = zero
    do s = 0, 69
    calc_D_ov_wm_cc3_pt4 = calc_D_ov_wm_cc3_pt4 + term(s)
    end do

    end function calc_D_ov_wm_cc3_pt4
    
    function calc_D_vo_wm_cc3_pt4(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vo_wm_cc3_pt4
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
    real(F64), dimension(0:245) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(0) = term(0) + r3(vrdav_Rr, a,q,b,j,p,i) * s1(b,j) * wm_interm_cc3_11_pt4(a, i)
term(1) = term(1) + r3(vrdav_Rr, a,q,b,j,p,i) * s1(a,j) * wm_interm_cc3_11_pt4(b, i)
term(2) = term(2) + r3(vrdav_Rr, a,q,b,j,p,i) * s1(b,i) * wm_interm_cc3_11_pt4(a, j)
term(3) = term(3) + r3(vrdav_Rr, a,q,b,j,p,i) * s1(a,i) * wm_interm_cc3_11_pt4(b, j)
term(4) = term(4) + r3(vrdav_Rr, a,q,b,j,p,i) * s1(b,j) * wm_interm_cc3_12_pt4(a, i)
term(5) = term(5) + r3(vrdav_Rr, a,q,b,j,p,i) * s1(a,j) * wm_interm_cc3_12_pt4(b, i)
term(6) = term(6) + r3(vrdav_Rr, a,q,b,j,p,i) * s1(b,i) * wm_interm_cc3_12_pt4(a, j)
term(7) = term(7) + r3(vrdav_Rr, a,q,b,j,p,i) * s1(a,i) * wm_interm_cc3_12_pt4(b, j)
term(8) = term(8) + r3(vrdav_Rl, a,q,b,j,p,i) * t1(b,j) * wm_interm_cc3_39_pt4(a, i)
term(9) = term(9) + r3(vrdav_Rl, a,q,b,j,p,i) * t1(b,j) * wm_interm_cc3_40_pt4(a, i)
term(10) = term(10) + r3(vrdav_Rl, a,q,b,j,p,i) * t1(a,j) * wm_interm_cc3_39_pt4(b, i)
term(11) = term(11) + r3(vrdav_Rl, a,q,b,j,p,i) * t1(b,i) * wm_interm_cc3_39_pt4(a, j)
term(12) = term(12) + r3(vrdav_Rl, a,q,b,j,p,i) * t1(a,i) * wm_interm_cc3_39_pt4(b, j)
term(13) = term(13) + r3(vrdav_Rl, a,q,b,j,p,i) * t1(a,j) * wm_interm_cc3_40_pt4(b, i)
term(14) = term(14) + r3(vrdav_Rl, a,q,b,j,p,i) * t1(b,i) * wm_interm_cc3_40_pt4(a, j)
term(15) = term(15) + r3(vrdav_Rl, a,q,b,j,p,i) * t1(a,i) * wm_interm_cc3_40_pt4(b, j)
term(16) = term(16) + r2(vrdav_Rl, a,q,p,i) * s1(b,j) * wm_interm_cc3_52_pt4(a, b, j, i)
term(17) = term(17) + r2(vrdav_Rl, a,i,p,q) * s1(b,j) * wm_interm_cc3_52_pt4(a, b, j, i)
term(18) = term(18) + r2(vrdav_Rr, a,q,p,i) * t1(b,j) * wm_interm_cc3_56_pt4(a, b, j, i)
term(19) = term(19) + r2(vrdav_Rr, a,i,p,q) * t1(b,j) * wm_interm_cc3_56_pt4(a, b, j, i)
term(20) = term(20) + r2(vrdav_Rr, a,q,p,i) * t1(b,j) * wm_interm_cc3_57_pt4(a, b, j, i)
term(21) = term(21) + r2(vrdav_Rr, a,i,p,q) * t1(b,j) * wm_interm_cc3_57_pt4(a, b, j, i)
term(22) = term(22) + r2(vrdav_Rr, a,q,p,i) * t1(b,j) * wm_interm_cc3_58_pt4(a, b, j, i)
term(23) = term(23) + r2(vrdav_Rr, a,i,p,q) * t1(b,j) * wm_interm_cc3_58_pt4(a, b, j, i)
term(24) = term(24) + r2(vrdav_Rr, b,j,p,i) * t1(a,q) * wm_interm_cc3_56_pt4(a, b, j, i)
term(25) = term(25) + r2(vrdav_Rr, b,j,p,i) * t1(a,q) * wm_interm_cc3_57_pt4(a, b, j, i)
term(26) = term(26) + r2(vrdav_Rr, b,j,p,i) * t1(a,q) * wm_interm_cc3_59_pt4(a, b, j, i)
term(27) = term(27) + r2(vrdav_Rr, b,j,p,i) * t1(a,q) * wm_interm_cc3_58_pt4(a, b, j, i)
term(28) = term(28) + r2(vrdav_Rr, b,j,p,i) * t1(a,q) * wm_interm_cc3_60_pt4(a, b, j, i)
end do 
end do 
end do 
end do 

term(0) = term(0) * 8.0d+0 
term(1) = term(1) * (-4.0d+0) 
term(2) = term(2) * (-4.0d+0) 
term(3) = term(3) * 8.0d+0 
term(4) = term(4) * (-4.0d+0) 
term(5) = term(5) * 2.0d+0 
term(6) = term(6) * 2.0d+0 
term(7) = term(7) * (-4.0d+0) 
term(8) = term(8) * 7.999999999999999d+0 
term(9) = term(9) * (-3.9999999999999996d+0) 
term(10) = term(10) * (-3.9999999999999996d+0) 
term(11) = term(11) * (-3.9999999999999996d+0) 
term(12) = term(12) * 7.999999999999999d+0 
term(13) = term(13) * 1.9999999999999998d+0 
term(14) = term(14) * 1.9999999999999998d+0 
term(15) = term(15) * (-3.9999999999999996d+0) 
term(16) = term(16) * (-8.0d+0) 
term(17) = term(17) * 16.0d+0 
term(18) = term(18) * 1.3333333333333333d+0 
term(19) = term(19) * (-2.6666666666666665d+0) 
term(20) = term(20) * (-0.6666666666666666d+0) 
term(21) = term(21) * 1.3333333333333333d+0 
term(22) = term(22) * 1.3333333333333333d+0 
term(23) = term(23) * (-2.6666666666666665d+0) 
term(24) = term(24) * 2.0d+0 
term(25) = term(25) * (-0.6666666666666666d+0) 
term(26) = term(26) * (-0.6666666666666666d+0) 
term(27) = term(27) * 0.6666666666666666d+0 
term(28) = term(28) * 0.6666666666666666d+0 

do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(29) = term(29) + r3(vrdav_Rr, a,i,b,j,p,q) * s1(b,j) * wm_interm_cc3_11_pt4(a, i)
term(30) = term(30) + r3(vrdav_Rr, a,i,b,j,p,q) * s1(b,i) * wm_interm_cc3_11_pt4(a, j)
term(31) = term(31) + r3(vrdav_Rr, a,i,b,j,p,q) * s1(b,j) * wm_interm_cc3_12_pt4(a, i)
term(32) = term(32) + r3(vrdav_Rr, a,i,b,j,p,q) * s1(b,i) * wm_interm_cc3_12_pt4(a, j)
term(33) = term(33) + s1(a,i) * t3(nocc, nactive, a,b,p,j,i,q) * wm_interm_cc3_26_pt4(b, j)
term(34) = term(34) + s1(a,i) * t3(nocc, nactive, a,b,p,j,i,q) * wm_interm_cc3_27_pt4(b, j)
term(35) = term(35) + r3(vrdav_Rl, a,i,b,j,p,q) * t1(b,j) * wm_interm_cc3_39_pt4(a, i)
term(36) = term(36) + r3(vrdav_Rl, a,i,b,j,p,q) * t1(b,j) * wm_interm_cc3_40_pt4(a, i)
term(37) = term(37) + r3(vrdav_Rl, a,i,b,j,p,q) * t1(b,i) * wm_interm_cc3_39_pt4(a, j)
term(38) = term(38) + r3(vrdav_Rl, a,i,b,j,p,q) * t1(b,i) * wm_interm_cc3_40_pt4(a, j)
term(39) = term(39) + r3(vrdav_Rl, a,i,b,j,p,q) * t1(a,j) * wm_interm_cc3_39_pt4(b, i)
term(40) = term(40) + r3(vrdav_Rl, a,i,b,j,p,q) * t1(a,i) * wm_interm_cc3_39_pt4(b, j)
term(41) = term(41) + r3(vrdav_Rl, a,i,b,j,p,q) * t1(a,j) * wm_interm_cc3_40_pt4(b, i)
term(42) = term(42) + r3(vrdav_Rl, a,i,b,j,p,q) * t1(a,i) * wm_interm_cc3_40_pt4(b, j)
end do 
end do 
end do 
end do 

term(29) = term(29) * (-16.0d+0) 
term(30) = term(30) * 8.0d+0 
term(31) = term(31) * 8.0d+0 
term(32) = term(32) * (-4.0d+0) 
term(33) = term(33) * (-7.999999999999999d+0) 
term(34) = term(34) * 3.9999999999999996d+0 
term(35) = term(35) * (-8.0d+0) 
term(36) = term(36) * 4.0d+0 
term(37) = term(37) * 4.0d+0 
term(38) = term(38) * (-2.0d+0) 
term(39) = term(39) * 4.0d+0 
term(40) = term(40) * (-8.0d+0) 
term(41) = term(41) * (-2.0d+0) 
term(42) = term(42) * 4.0d+0 

do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do c = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(43) = term(43) + r2(vrdav_Rl, b,i,c,j) * t3(nocc, nactive, a,b,p,q,j,i) * wm_interm_cc3_25_pt4(c, a)
end do 
end do 
end do 
end do 
end do 

term(43) = -term(43) 

do i = 1, nocc 
do c = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(44) = term(44) + r2(vrdav_Rl, b,i,c,j) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_cc3_25_pt4(b, a)
term(45) = term(45) + r2(vrdav_Rl, b,j,c,i) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_cc3_25_pt4(b, a)
end do 
end do 
end do 
end do 
end do 

term(44) = term(44) * 1.9999999999999998d+0 
term(45) = -term(45) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(46) = term(46) + r2(vrdav_Rl, b,j,c,i) * t3(nocc, nactive, a,b,p,q,j,i) * wm_interm_cc3_25_pt4(c, a)
end do 
end do 
end do 
end do 
end do 

term(46) = term(46) * 1.9999999999999998d+0 

do i = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(47) = term(47) + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_cc3_25_pt4(b, c)
end do 
end do 
end do 
end do 
end do 

term(47) = term(47) * (-1.9999999999999998d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(48) = term(48) + s1(a,j) * t3(nocc, nactive, a,b,p,q,j,i) * wm_interm_cc3_26_pt4(b, i)
term(49) = term(49) + s1(a,j) * t3(nocc, nactive, a,b,p,q,j,i) * wm_interm_cc3_27_pt4(b, i)
term(50) = term(50) + s1(a,i) * t3(nocc, nactive, a,b,p,q,j,i) * wm_interm_cc3_27_pt4(b, j)
term(51) = term(51) + s1(a,i) * t3(nocc, nactive, a,b,p,q,j,i) * wm_interm_cc3_26_pt4(b, j)
term(52) = term(52) + s1(b,i) * t3(nocc, nactive, a,b,p,q,j,i) * wm_interm_cc3_26_pt4(a, j)
term(53) = term(53) + s1(b,i) * t3(nocc, nactive, a,b,p,q,j,i) * wm_interm_cc3_27_pt4(a, j)
term(54) = term(54) + s1(b,j) * t3(nocc, nactive, a,b,p,q,j,i) * wm_interm_cc3_27_pt4(a, i)
term(55) = term(55) + s1(b,j) * t3(nocc, nactive, a,b,p,q,j,i) * wm_interm_cc3_26_pt4(a, i)
term(56) = term(56) + s1(a,i) * t3(nocc, nactive, a,b,p,i,j,q) * wm_interm_cc3_27_pt4(b, j)
term(57) = term(57) + s1(a,i) * t3(nocc, nactive, a,b,p,i,j,q) * wm_interm_cc3_26_pt4(b, j)
end do 
end do 
end do 
end do 

term(48) = term(48) * 3.9999999999999996d+0 
term(49) = term(49) * (-1.9999999999999998d+0) 
term(50) = term(50) * 3.9999999999999996d+0 
term(51) = term(51) * (-7.999999999999999d+0) 
term(52) = term(52) * 3.9999999999999996d+0 
term(53) = term(53) * (-1.9999999999999998d+0) 
term(54) = term(54) * 3.9999999999999996d+0 
term(55) = term(55) * (-7.999999999999999d+0) 
term(56) = term(56) * (-7.999999999999999d+0) 
term(57) = term(57) * 15.999999999999998d+0 

do i = 1, nocc 
term(58) = term(58) + wm_interm_cc3_3_pt4(p, i) * wm_interm_cc3_7_pt4(q, i)
term(59) = term(59) + wm_interm_cc3_4_pt4(p, i) * wm_interm_cc3_7_pt4(q, i)
term(60) = term(60) + wm_interm_cc3_1_pt4(p, i) * wm_interm_cc3_7_pt4(q, i)
term(61) = term(61) + wm_interm_cc3_2_pt4(p, i) * wm_interm_cc3_7_pt4(q, i)
term(62) = term(62) + wm_interm_cc3_5_pt4(p, i) * wm_interm_cc3_7_pt4(q, i)
term(63) = term(63) + wm_interm_cc3_6_pt4(p, i) * wm_interm_cc3_7_pt4(q, i)
term(64) = term(64) + wm_interm_cc3_14_pt4(i, q) * wm_interm_cc3_21_pt4(p, i)
term(65) = term(65) + wm_interm_cc3_14_pt4(i, q) * wm_interm_cc3_22_pt4(p, i)
term(66) = term(66) + wm_interm_cc3_14_pt4(i, q) * wm_interm_cc3_19_pt4(p, i)
term(67) = term(67) + wm_interm_cc3_14_pt4(i, q) * wm_interm_cc3_20_pt4(p, i)
end do 

term(58) = -term(58) 
term(59) = term(59) * 2.0d+0 
term(60) = -term(60) 
term(61) = term(61) * 2.0d+0 
term(62) = term(62) * 2.0d+0 
term(63) = term(63) * (-4.0d+0) 
term(64) = term(64) * (-1.9999999999999998d+0) 
term(65) = term(65) * 3.9999999999999996d+0 
term(66) = term(66) * 1.9999999999999998d+0 
term(67) = term(67) * (-3.9999999999999996d+0) 

do a = nocc + 1, nactive 
term(68) = term(68) + wm_interm_cc3_25_pt4(p, a) * wm_interm_cc3_30_pt4(a, q)
term(69) = term(69) + wm_interm_cc3_25_pt4(p, a) * wm_interm_cc3_31_pt4(a, q)
term(70) = term(70) + wm_interm_cc3_25_pt4(p, a) * wm_interm_cc3_32_pt4(a, q)
term(71) = term(71) + wm_interm_cc3_25_pt4(p, a) * wm_interm_cc3_34_pt4(a, q)
term(72) = term(72) + wm_interm_cc3_17_pt4(p, a) * wm_interm_cc3_43_pt4(a, q)
term(73) = term(73) + wm_interm_cc3_17_pt4(p, a) * wm_interm_cc3_44_pt4(a, q)
term(74) = term(74) + wm_interm_cc3_17_pt4(p, a) * wm_interm_cc3_49_pt4(a, q)
term(75) = term(75) + wm_interm_cc3_17_pt4(p, a) * wm_interm_cc3_50_pt4(a, q)
end do 

term(68) = term(68) * 2.0d+0 
term(69) = term(69) * (-4.0d+0) 
term(70) = term(70) * (-1.9999999999999998d+0) 
term(71) = term(71) * 3.9999999999999996d+0 
term(72) = term(72) * (-1.9999999999999998d+0) 
term(73) = term(73) * 3.9999999999999996d+0 
term(74) = term(74) * 2.0d+0 
term(75) = term(75) * (-4.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
term(76) = term(76) + wm_interm_cc3_14_pt4(i, j) * wm_interm_cc3_15_pt4(p, j, q, i)
term(77) = term(77) + wm_interm_cc3_14_pt4(i, j) * wm_interm_cc3_16_pt4(p, j, q, i)
term(78) = term(78) + wm_interm_cc3_14_pt4(i, j) * wm_interm_cc3_15_pt4(p, j, i, q)
term(79) = term(79) + wm_interm_cc3_14_pt4(i, j) * wm_interm_cc3_16_pt4(p, j, i, q)
term(80) = term(80) + wm_interm_cc3_14_pt4(i, j) * wm_interm_cc3_18_pt4(p, j, q, i)
term(81) = term(81) + wm_interm_cc3_14_pt4(i, j) * wm_interm_cc3_18_pt4(p, j, i, q)
term(82) = term(82) + wm_interm_cc3_14_pt4(i, j) * wm_interm_cc3_41_pt4(p, q, j, i)
term(83) = term(83) + wm_interm_cc3_14_pt4(i, j) * wm_interm_cc3_42_pt4(p, q, j, i)
term(84) = term(84) + wm_interm_cc3_14_pt4(i, j) * wm_interm_cc3_45_pt4(p, q, j, i)
term(85) = term(85) + wm_interm_cc3_14_pt4(i, j) * wm_interm_cc3_46_pt4(p, q, j, i)
term(86) = term(86) + wm_interm_cc3_14_pt4(i, j) * wm_interm_cc3_47_pt4(p, q, j, i)
term(87) = term(87) + wm_interm_cc3_14_pt4(i, j) * wm_interm_cc3_48_pt4(p, q, j, i)
term(88) = term(88) + wm_interm_cc3_14_pt4(i, j) * wm_interm_cc3_42_pt4(p, j, q, i)
term(89) = term(89) + wm_interm_cc3_14_pt4(i, j) * wm_interm_cc3_41_pt4(p, j, q, i)
term(90) = term(90) + wm_interm_cc3_14_pt4(i, j) * wm_interm_cc3_45_pt4(p, j, q, i)
term(91) = term(91) + wm_interm_cc3_14_pt4(i, j) * wm_interm_cc3_46_pt4(p, j, q, i)
term(92) = term(92) + wm_interm_cc3_14_pt4(i, j) * wm_interm_cc3_47_pt4(p, j, q, i)
term(93) = term(93) + wm_interm_cc3_14_pt4(i, j) * wm_interm_cc3_48_pt4(p, j, q, i)
end do 
end do 

term(76) = term(76) * (-1.9999999999999998d+0) 
term(77) = term(77) * 3.9999999999999996d+0 
term(78) = term(78) * 3.9999999999999996d+0 
term(79) = term(79) * (-7.999999999999999d+0) 
term(80) = -term(80) 
term(81) = term(81) * 1.9999999999999998d+0 
term(82) = -term(82) 
term(83) = term(83) * 2.0d+0 
term(84) = term(84) * 2.0d+0 
term(85) = term(85) * (-4.0d+0) 
term(86) = -term(86) 
term(87) = term(87) * 2.0d+0 
term(88) = -term(88) 
term(89) = term(89) * 2.0d+0 
term(90) = -term(90) 
term(91) = term(91) * 2.0d+0 
term(92) = term(92) * 2.0d+0 
term(93) = term(93) * (-4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(94) = term(94) + r2(vrdav_Rl, a,q,p,i) * s1(b,j) * wm_interm_cc3_52_pt4(a, b, i, j)
term(95) = term(95) + r2(vrdav_Rl, a,i,p,q) * s1(b,j) * wm_interm_cc3_52_pt4(a, b, i, j)
term(96) = term(96) + r2(vrdav_Rl, a,q,p,i) * s1(b,j) * wm_interm_cc3_53_pt4(a, b, i, j)
term(97) = term(97) + r2(vrdav_Rl, a,i,p,q) * s1(b,j) * wm_interm_cc3_53_pt4(a, b, i, j)
term(98) = term(98) + r2(vrdav_Rl, a,q,p,i) * s1(b,j) * wm_interm_cc3_54_pt4(a, b, i, j)
term(99) = term(99) + r2(vrdav_Rl, a,i,p,q) * s1(b,j) * wm_interm_cc3_54_pt4(a, b, i, j)
term(100) = term(100) + r2(vrdav_Rr, a,q,p,i) * t1(b,j) * wm_interm_cc3_56_pt4(a, b, i, j)
term(101) = term(101) + r2(vrdav_Rr, a,i,p,q) * t1(b,j) * wm_interm_cc3_56_pt4(a, b, i, j)
term(102) = term(102) + r2(vrdav_Rr, a,q,p,i) * t1(b,j) * wm_interm_cc3_57_pt4(a, b, i, j)
term(103) = term(103) + r2(vrdav_Rr, a,i,p,q) * t1(b,j) * wm_interm_cc3_57_pt4(a, b, i, j)
term(104) = term(104) + r2(vrdav_Rr, a,q,p,i) * t1(b,j) * wm_interm_cc3_58_pt4(a, b, i, j)
term(105) = term(105) + r2(vrdav_Rr, a,i,p,q) * t1(b,j) * wm_interm_cc3_58_pt4(a, b, i, j)
term(106) = term(106) + r2(vrdav_Rr, b,j,p,i) * t1(a,q) * wm_interm_cc3_56_pt4(a, b, i, j)
term(107) = term(107) + r2(vrdav_Rr, b,j,p,i) * t1(a,q) * wm_interm_cc3_57_pt4(a, b, i, j)
term(108) = term(108) + r2(vrdav_Rr, b,j,p,i) * t1(a,q) * wm_interm_cc3_59_pt4(a, b, i, j)
term(109) = term(109) + r2(vrdav_Rr, b,j,p,i) * t1(a,q) * wm_interm_cc3_58_pt4(a, b, i, j)
term(110) = term(110) + r2(vrdav_Rr, b,j,p,i) * t1(a,q) * wm_interm_cc3_60_pt4(a, b, i, j)
end do 
end do 
end do 
end do 

term(94) = term(94) * 7.999999999999999d+0 
term(95) = term(95) * (-15.999999999999998d+0) 
term(96) = term(96) * (-4.0d+0) 
term(97) = term(97) * 8.0d+0 
term(98) = term(98) * 4.0d+0 
term(99) = term(99) * (-8.0d+0) 
term(100) = term(100) * (-2.6666666666666665d+0) 
term(101) = term(101) * 5.333333333333333d+0 
term(102) = term(102) * 1.3333333333333333d+0 
term(103) = term(103) * (-2.6666666666666665d+0) 
term(104) = term(104) * (-0.6666666666666666d+0) 
term(105) = term(105) * 1.3333333333333333d+0 
term(106) = term(106) * (-4.0d+0) 
term(107) = term(107) * 1.3333333333333333d+0 
term(108) = term(108) * 1.3333333333333333d+0 
term(109) = term(109) * (-0.3333333333333333d+0) 
term(110) = term(110) * (-0.3333333333333333d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(111) = term(111) + r2(vrdav_Rl, b,j,p,i) * s1(a,q) * wm_interm_cc3_52_pt4(b, a, i, j)
term(112) = term(112) + r2(vrdav_Rl, b,j,p,i) * s1(a,q) * wm_interm_cc3_55_pt4(b, a, i, j)
term(113) = term(113) + r2(vrdav_Rl, b,j,p,i) * s1(a,q) * wm_interm_cc3_54_pt4(b, a, i, j)
term(114) = term(114) + r2(vrdav_Rl, b,j,p,i) * s1(a,q) * wm_interm_cc3_53_pt4(b, a, i, j)
term(115) = term(115) + r2(vrdav_Rr, a,q,p,i) * t1(b,j) * wm_interm_cc3_56_pt4(b, a, i, j)
term(116) = term(116) + r2(vrdav_Rr, a,i,p,q) * t1(b,j) * wm_interm_cc3_56_pt4(b, a, i, j)
term(117) = term(117) + r2(vrdav_Rr, a,q,p,i) * t1(b,j) * wm_interm_cc3_57_pt4(b, a, i, j)
term(118) = term(118) + r2(vrdav_Rr, a,i,p,q) * t1(b,j) * wm_interm_cc3_57_pt4(b, a, i, j)
term(119) = term(119) + r2(vrdav_Rr, a,q,p,i) * t1(b,j) * wm_interm_cc3_59_pt4(b, a, i, j)
term(120) = term(120) + r2(vrdav_Rr, a,i,p,q) * t1(b,j) * wm_interm_cc3_59_pt4(b, a, i, j)
term(121) = term(121) + r2(vrdav_Rr, a,q,p,i) * t1(b,j) * wm_interm_cc3_58_pt4(b, a, i, j)
term(122) = term(122) + r2(vrdav_Rr, a,i,p,q) * t1(b,j) * wm_interm_cc3_58_pt4(b, a, i, j)
term(123) = term(123) + r2(vrdav_Rr, b,j,p,i) * t1(a,q) * wm_interm_cc3_56_pt4(b, a, i, j)
term(124) = term(124) + r2(vrdav_Rr, b,j,p,i) * t1(a,q) * wm_interm_cc3_57_pt4(b, a, i, j)
term(125) = term(125) + r2(vrdav_Rr, b,j,p,i) * t1(a,q) * wm_interm_cc3_58_pt4(b, a, i, j)
term(126) = term(126) + r2(vrdav_Rr, b,j,p,i) * t1(a,q) * wm_interm_cc3_59_pt4(b, a, i, j)
term(127) = term(127) + r2(vrdav_Rr, b,j,p,i) * t1(a,q) * wm_interm_cc3_60_pt4(b, a, i, j)
end do 
end do 
end do 
end do 

term(111) = term(111) * (-4.0d+0) 
term(113) = term(113) * (-2.0d+0) 
term(114) = term(114) * 2.0d+0 
term(115) = term(115) * 2.6666666666666665d+0 
term(116) = term(116) * (-5.333333333333333d+0) 
term(117) = term(117) * (-1.3333333333333333d+0) 
term(118) = term(118) * 2.6666666666666665d+0 
term(119) = term(119) * (-0.6666666666666666d+0) 
term(120) = term(120) * 1.3333333333333333d+0 
term(121) = term(121) * 1.3333333333333333d+0 
term(122) = term(122) * (-2.6666666666666665d+0) 
term(123) = term(123) * 2.0d+0 
term(124) = term(124) * (-0.6666666666666666d+0) 
term(125) = term(125) * 0.6666666666666666d+0 
term(126) = term(126) * (-0.6666666666666666d+0) 
term(127) = term(127) * 0.6666666666666666d+0 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(128) = term(128) + r2(vrdav_Rl, b,j,p,i) * s1(a,q) * wm_interm_cc3_52_pt4(b, a, j, i)
term(129) = term(129) + r2(vrdav_Rl, b,j,p,i) * s1(a,q) * wm_interm_cc3_54_pt4(b, a, j, i)
term(130) = term(130) + r2(vrdav_Rl, b,j,p,i) * s1(a,q) * wm_interm_cc3_55_pt4(b, a, j, i)
term(131) = term(131) + r2(vrdav_Rl, b,j,p,i) * s1(a,q) * wm_interm_cc3_53_pt4(b, a, j, i)
term(132) = term(132) + r2(vrdav_Rr, a,q,p,i) * t1(b,j) * wm_interm_cc3_56_pt4(b, a, j, i)
term(133) = term(133) + r2(vrdav_Rr, a,i,p,q) * t1(b,j) * wm_interm_cc3_56_pt4(b, a, j, i)
term(134) = term(134) + r2(vrdav_Rr, a,q,p,i) * t1(b,j) * wm_interm_cc3_57_pt4(b, a, j, i)
term(135) = term(135) + r2(vrdav_Rr, a,i,p,q) * t1(b,j) * wm_interm_cc3_57_pt4(b, a, j, i)
term(136) = term(136) + r2(vrdav_Rr, a,q,p,i) * t1(b,j) * wm_interm_cc3_59_pt4(b, a, j, i)
term(137) = term(137) + r2(vrdav_Rr, a,i,p,q) * t1(b,j) * wm_interm_cc3_59_pt4(b, a, j, i)
term(138) = term(138) + r2(vrdav_Rr, a,q,p,i) * t1(b,j) * wm_interm_cc3_58_pt4(b, a, j, i)
term(139) = term(139) + r2(vrdav_Rr, a,i,p,q) * t1(b,j) * wm_interm_cc3_58_pt4(b, a, j, i)
term(140) = term(140) + r2(vrdav_Rr, b,j,p,i) * t1(a,q) * wm_interm_cc3_56_pt4(b, a, j, i)
term(141) = term(141) + r2(vrdav_Rr, b,j,p,i) * t1(a,q) * wm_interm_cc3_57_pt4(b, a, j, i)
term(142) = term(142) + r2(vrdav_Rr, b,j,p,i) * t1(a,q) * wm_interm_cc3_58_pt4(b, a, j, i)
term(143) = term(143) + r2(vrdav_Rr, b,j,p,i) * t1(a,q) * wm_interm_cc3_59_pt4(b, a, j, i)
term(144) = term(144) + r2(vrdav_Rr, b,j,p,i) * t1(a,q) * wm_interm_cc3_60_pt4(b, a, j, i)
end do 
end do 
end do 
end do 

term(128) = term(128) * 8.0d+0 
term(130) = term(130) * (-2.0d+0) 
term(131) = term(131) * (-4.0d+0) 
term(132) = term(132) * (-5.333333333333333d+0) 
term(133) = term(133) * 10.666666666666666d+0 
term(134) = term(134) * 2.6666666666666665d+0 
term(135) = term(135) * (-5.333333333333333d+0) 
term(136) = term(136) * 1.3333333333333333d+0 
term(137) = term(137) * (-2.6666666666666665d+0) 
term(138) = term(138) * (-0.6666666666666666d+0) 
term(139) = term(139) * 1.3333333333333333d+0 
term(140) = term(140) * (-4.0d+0) 
term(141) = term(141) * 1.3333333333333333d+0 
term(142) = term(142) * (-0.3333333333333333d+0) 
term(143) = term(143) * 1.3333333333333333d+0 
term(144) = term(144) * (-0.3333333333333333d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(145) = term(145) + s1(a,i) * s1(p,q) * wm_interm_cc3_19_pt4(a, i)
term(146) = term(146) + s1(a,i) * s1(p,q) * wm_interm_cc3_20_pt4(a, i)
term(147) = term(147) + s1(a,i) * s1(p,q) * wm_interm_cc3_21_pt4(a, i)
term(148) = term(148) + s1(a,i) * s1(p,q) * wm_interm_cc3_22_pt4(a, i)
term(149) = term(149) + s1(p,i) * s1(a,q) * wm_interm_cc3_21_pt4(a, i)
term(150) = term(150) + s1(p,i) * s1(a,q) * wm_interm_cc3_22_pt4(a, i)
term(151) = term(151) + s1(p,i) * s1(a,q) * wm_interm_cc3_19_pt4(a, i)
term(152) = term(152) + s1(p,i) * s1(a,q) * wm_interm_cc3_20_pt4(a, i)
term(153) = term(153) + s1(a,i) * t1(p,q) * wm_interm_cc3_19_pt4(a, i)
term(154) = term(154) + s1(a,i) * t1(p,q) * wm_interm_cc3_20_pt4(a, i)
term(155) = term(155) + s1(a,i) * t1(p,q) * wm_interm_cc3_21_pt4(a, i)
term(156) = term(156) + s1(a,i) * t1(p,q) * wm_interm_cc3_22_pt4(a, i)
term(157) = term(157) + s1(p,q) * t1(a,i) * wm_interm_cc3_49_pt4(a, i)
term(158) = term(158) + s1(p,q) * t1(a,i) * wm_interm_cc3_50_pt4(a, i)
term(159) = term(159) + s1(p,q) * t1(a,i) * wm_interm_cc3_43_pt4(a, i)
term(160) = term(160) + s1(p,q) * t1(a,i) * wm_interm_cc3_44_pt4(a, i)
term(161) = term(161) + t1(a,i) * t1(p,q) * wm_interm_cc3_49_pt4(a, i)
term(162) = term(162) + t1(a,i) * t1(p,q) * wm_interm_cc3_50_pt4(a, i)
term(163) = term(163) + t1(a,i) * t1(p,q) * wm_interm_cc3_43_pt4(a, i)
term(164) = term(164) + t1(a,i) * t1(p,q) * wm_interm_cc3_44_pt4(a, i)
term(165) = term(165) + t1(p,i) * t1(a,q) * wm_interm_cc3_43_pt4(a, i)
term(166) = term(166) + t1(p,i) * t1(a,q) * wm_interm_cc3_44_pt4(a, i)
term(167) = term(167) + t1(p,i) * t1(a,q) * wm_interm_cc3_49_pt4(a, i)
term(168) = term(168) + t1(p,i) * t1(a,q) * wm_interm_cc3_50_pt4(a, i)
end do 
end do 

term(145) = term(145) * 4.0d+0 
term(146) = term(146) * (-8.0d+0) 
term(147) = term(147) * (-4.0d+0) 
term(148) = term(148) * 8.0d+0 
term(149) = term(149) * 3.9999999999999996d+0 
term(150) = term(150) * (-7.999999999999999d+0) 
term(151) = term(151) * (-3.9999999999999996d+0) 
term(152) = term(152) * 7.999999999999999d+0 
term(153) = term(153) * 4.0d+0 
term(154) = term(154) * (-8.0d+0) 
term(155) = term(155) * (-4.0d+0) 
term(156) = term(156) * 8.0d+0 
term(157) = term(157) * (-4.0d+0) 
term(158) = term(158) * 8.0d+0 
term(159) = term(159) * 3.9999999999999996d+0 
term(160) = term(160) * (-7.999999999999999d+0) 
term(161) = term(161) * (-4.0d+0) 
term(162) = term(162) * 8.0d+0 
term(163) = term(163) * 3.9999999999999996d+0 
term(164) = term(164) * (-7.999999999999999d+0) 
term(165) = term(165) * (-3.9999999999999996d+0) 
term(166) = term(166) * 7.999999999999999d+0 
term(167) = term(167) * 4.0d+0 
term(168) = term(168) * (-8.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(169) = term(169) + r1(vrdav_Rr, p,i) * s1(a,j) * wm_interm_cc3_28_pt4(a, i, j, q)
term(170) = term(170) + r1(vrdav_Rr, p,i) * s1(a,j) * wm_interm_cc3_29_pt4(a, i, j, q)
term(171) = term(171) + r1(vrdav_Rr, p,i) * s1(a,j) * wm_interm_cc3_33_pt4(a, i, j, q)
term(172) = term(172) + r1(vrdav_Rr, p,i) * s1(a,j) * wm_interm_cc3_33_pt4(a, i, q, j)
term(173) = term(173) + r1(vrdav_Rr, p,i) * s1(a,j) * wm_interm_cc3_35_pt4(a, i, j, q)
term(174) = term(174) + r1(vrdav_Rr, p,i) * s1(a,j) * wm_interm_cc3_28_pt4(a, i, q, j)
term(175) = term(175) + r1(vrdav_Rr, p,i) * s1(a,j) * wm_interm_cc3_29_pt4(a, i, q, j)
term(176) = term(176) + r1(vrdav_Rr, p,i) * s1(a,j) * wm_interm_cc3_35_pt4(a, i, q, j)
end do 
end do 
end do 

term(169) = term(169) * (-1.9999999999999998d+0) 
term(170) = term(170) * 3.9999999999999996d+0 
term(171) = term(171) * 1.9999999999999998d+0 
term(172) = -term(172) 
term(173) = -term(173) 
term(174) = term(174) * 3.9999999999999996d+0 
term(175) = term(175) * (-7.999999999999999d+0) 
term(176) = term(176) * 1.9999999999999998d+0 

do i = 1, nocc 
do c = nocc + 1, nactive 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(177) = term(177) + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,p,q,j,i) * wm_interm_cc3_0_pt4(a, c)
term(178) = term(178) + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,p,i,j,q) * wm_interm_cc3_0_pt4(a, c)
term(179) = term(179) + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,c,p,q,j,i) * wm_interm_cc3_25_pt4(b, c)
term(180) = term(180) + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,c,p,i,j,q) * wm_interm_cc3_25_pt4(b, c)
term(181) = term(181) + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, b,c,p,i,j,q) * wm_interm_cc3_25_pt4(a, c)
end do 
end do 
end do 
end do 
end do 

term(177) = term(177) * (-2.0d+0) 
term(178) = term(178) * 4.0d+0 
term(179) = term(179) * 3.9999999999999996d+0 
term(180) = term(180) * (-3.9999999999999996d+0) 
term(181) = term(181) * 1.9999999999999998d+0 

do j = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(182) = term(182) + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,p,q,i,j) * wm_interm_cc3_0_pt4(a, c)
term(183) = term(183) + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,p,j,i,q) * wm_interm_cc3_0_pt4(a, c)
term(184) = term(184) + r2(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,i,c,j,p,q) * wm_interm_cc3_17_pt4(c, b)
term(185) = term(185) + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,c,p,j,i,q) * wm_interm_cc3_25_pt4(b, c)
term(186) = term(186) + r2(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, b,c,p,j,i,q) * wm_interm_cc3_25_pt4(a, c)
term(187) = term(187) + r3(vrdav_Rl, a,i,b,j,p,q) * r2(vrdav_Rr, a,i,c,j) * wm_interm_cc3_17_pt4(c, b)
end do 
end do 
end do 
end do 
end do 

term(182) = term(182) * 4.0d+0 
term(183) = term(183) * (-8.0d+0) 
term(184) = term(184) * (-3.9999999999999996d+0) 
term(185) = term(185) * 1.9999999999999998d+0 
term(186) = term(186) * (-3.9999999999999996d+0) 
term(187) = term(187) * (-4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(188) = term(188) + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,p,i,q,j) * wm_interm_cc3_0_pt4(a, c)
term(189) = term(189) + r2(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,j,c,i,p,q) * wm_interm_cc3_17_pt4(c, b)
term(190) = term(190) + r2(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, b,j,c,i,p,q) * wm_interm_cc3_17_pt4(c, a)
term(191) = term(191) + r3(vrdav_Rl, a,i,b,j,p,q) * r2(vrdav_Rr, a,j,c,i) * wm_interm_cc3_17_pt4(c, b)
term(192) = term(192) + r3(vrdav_Rl, a,i,b,j,p,q) * r2(vrdav_Rr, b,j,c,i) * wm_interm_cc3_17_pt4(c, a)
end do 
end do 
end do 
end do 
end do 

term(188) = term(188) * (-2.0d+0) 
term(189) = term(189) * 1.9999999999999998d+0 
term(190) = term(190) * (-3.9999999999999996d+0) 
term(191) = term(191) * 2.0d+0 
term(192) = term(192) * (-4.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(193) = term(193) + r2(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, b,c,p,j,q,i) * wm_interm_cc3_0_pt4(a, c)
term(194) = term(194) + r2(vrdav_Rl, b,i,c,j) * r3(vrdav_Rr, a,q,b,j,p,i) * wm_interm_cc3_17_pt4(a, c)
term(195) = term(195) + r2(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,q,c,j,p,i) * wm_interm_cc3_17_pt4(c, b)
term(196) = term(196) + r2(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,q,c,j,p,i) * wm_interm_cc3_17_pt4(c, b)
term(197) = term(197) + r2(vrdav_Rl, b,j,c,i) * r3(vrdav_Rr, a,q,b,j,p,i) * wm_interm_cc3_17_pt4(a, c)
term(198) = term(198) + r2(vrdav_Rl, b,i,c,j) * r3(vrdav_Rr, a,q,c,j,p,i) * wm_interm_cc3_17_pt4(a, b)
term(199) = term(199) + r2(vrdav_Rl, b,j,c,i) * r3(vrdav_Rr, a,q,c,j,p,i) * wm_interm_cc3_17_pt4(a, b)
term(200) = term(200) + r3(vrdav_Rl, a,q,b,j,p,i) * r2(vrdav_Rr, a,j,c,i) * wm_interm_cc3_17_pt4(c, b)
term(201) = term(201) + r3(vrdav_Rl, a,q,b,j,p,i) * r2(vrdav_Rr, a,i,c,j) * wm_interm_cc3_17_pt4(c, b)
term(202) = term(202) + r3(vrdav_Rl, a,q,b,j,p,i) * r2(vrdav_Rr, b,i,c,j) * wm_interm_cc3_17_pt4(c, a)
term(203) = term(203) + r3(vrdav_Rl, a,q,b,j,p,i) * r2(vrdav_Rr, b,j,c,i) * wm_interm_cc3_17_pt4(c, a)
end do 
end do 
end do 
end do 
end do 

term(193) = term(193) * 4.0d+0 
term(194) = -term(194) 
term(195) = term(195) * 3.9999999999999996d+0 
term(196) = term(196) * (-1.9999999999999998d+0) 
term(197) = term(197) * 1.9999999999999998d+0 
term(198) = term(198) * 1.9999999999999998d+0 
term(199) = -term(199) 
term(200) = term(200) * (-1.9999999999999998d+0) 
term(201) = term(201) * 3.9999999999999996d+0 
term(202) = term(202) * (-1.9999999999999998d+0) 
term(203) = term(203) * 3.9999999999999996d+0 

do j = 1, nocc 
do i = 1, nocc 
term(204) = term(204) + wm_interm_cc3_7_pt4(i, j) * wm_interm_cc3_8_pt4(p, i, q, j)
term(205) = term(205) + wm_interm_cc3_7_pt4(i, j) * wm_interm_cc3_8_pt4(p, i, j, q)
term(206) = term(206) + wm_interm_cc3_7_pt4(i, j) * wm_interm_cc3_9_pt4(p, i, j, q)
term(207) = term(207) + wm_interm_cc3_10_pt4(p, i, j, q) * wm_interm_cc3_7_pt4(i, j)
term(208) = term(208) + wm_interm_cc3_7_pt4(i, j) * wm_interm_cc3_9_pt4(p, i, q, j)
term(209) = term(209) + wm_interm_cc3_10_pt4(p, i, q, j) * wm_interm_cc3_7_pt4(i, j)
term(210) = term(210) + wm_interm_cc3_13_pt4(p, i, j, q) * wm_interm_cc3_14_pt4(j, i)
term(211) = term(211) + wm_interm_cc3_13_pt4(p, i, q, j) * wm_interm_cc3_14_pt4(j, i)
term(212) = term(212) + wm_interm_cc3_35_pt4(p, i, q, j) * wm_interm_cc3_36_pt4(i, j)
term(213) = term(213) + wm_interm_cc3_33_pt4(p, i, q, j) * wm_interm_cc3_36_pt4(i, j)
term(214) = term(214) + wm_interm_cc3_33_pt4(p, i, j, q) * wm_interm_cc3_36_pt4(i, j)
term(215) = term(215) + wm_interm_cc3_35_pt4(p, i, j, q) * wm_interm_cc3_36_pt4(i, j)
term(216) = term(216) + wm_interm_cc3_28_pt4(p, i, q, j) * wm_interm_cc3_36_pt4(i, j)
term(217) = term(217) + wm_interm_cc3_29_pt4(p, i, q, j) * wm_interm_cc3_36_pt4(i, j)
term(218) = term(218) + wm_interm_cc3_28_pt4(p, i, j, q) * wm_interm_cc3_36_pt4(i, j)
term(219) = term(219) + wm_interm_cc3_29_pt4(p, i, j, q) * wm_interm_cc3_36_pt4(i, j)
end do 
end do 

term(204) = term(204) * (-2.0d+0) 
term(205) = term(205) * 4.0d+0 
term(206) = term(206) * 4.0d+0 
term(207) = term(207) * (-8.0d+0) 
term(208) = term(208) * (-2.0d+0) 
term(209) = term(209) * 4.0d+0 
term(210) = -term(210) 
term(211) = term(211) * 1.9999999999999998d+0 
term(212) = -term(212) 
term(213) = term(213) * 1.9999999999999998d+0 
term(214) = -term(214) 
term(215) = term(215) * 1.9999999999999998d+0 
term(216) = term(216) * (-1.9999999999999998d+0) 
term(217) = term(217) * 3.9999999999999996d+0 
term(218) = term(218) * 3.9999999999999996d+0 
term(219) = term(219) * (-7.999999999999999d+0) 

do j = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(220) = term(220) + r2(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, b,i,c,j,p,q) * wm_interm_cc3_17_pt4(c, a)
term(221) = term(221) + r3(vrdav_Rl, a,i,b,j,p,q) * r2(vrdav_Rr, b,i,c,j) * wm_interm_cc3_17_pt4(c, a)
end do 
end do 
end do 
end do 
end do 

term(220) = term(220) * 1.9999999999999998d+0 
term(221) = term(221) * 2.0d+0 

do i = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(222) = term(222) + r3(vrdav_Rr, a,j,b,k,p,i) * s1(b,k) * wm_interm_cc3_23_pt4(a, j, i, q)
term(223) = term(223) + r3(vrdav_Rr, a,j,b,k,p,i) * s1(b,k) * wm_interm_cc3_23_pt4(a, i, j, q)
term(224) = term(224) + r3(vrdav_Rr, a,j,b,k,p,i) * s1(b,k) * wm_interm_cc3_24_pt4(a, i, j, q)
term(225) = term(225) + r3(vrdav_Rr, a,j,b,k,p,i) * s1(b,k) * wm_interm_cc3_24_pt4(a, j, i, q)
term(226) = term(226) + r3(vrdav_Rr, a,j,b,k,p,i) * s1(b,j) * wm_interm_cc3_23_pt4(a, k, i, q)
term(227) = term(227) + r3(vrdav_Rr, a,j,b,k,p,i) * s1(b,j) * wm_interm_cc3_23_pt4(a, i, k, q)
term(228) = term(228) + r3(vrdav_Rr, a,j,b,k,p,i) * s1(b,j) * wm_interm_cc3_24_pt4(a, i, k, q)
term(229) = term(229) + r3(vrdav_Rr, a,j,b,k,p,i) * s1(b,j) * wm_interm_cc3_24_pt4(a, k, i, q)
term(230) = term(230) + r3(vrdav_Rr, a,j,b,k,p,i) * s1(b,i) * wm_interm_cc3_24_pt4(a, k, j, q)
term(231) = term(231) + r3(vrdav_Rr, a,j,b,k,p,i) * s1(b,i) * wm_interm_cc3_24_pt4(a, j, k, q)
term(232) = term(232) + r3(vrdav_Rr, a,j,b,k,p,i) * s1(b,i) * wm_interm_cc3_23_pt4(a, j, k, q)
term(233) = term(233) + r3(vrdav_Rr, a,j,b,k,p,i) * s1(b,i) * wm_interm_cc3_23_pt4(a, k, j, q)
end do 
end do 
end do 
end do 
end do 

term(222) = term(222) * (-2.0d+0) 
term(223) = term(223) * 4.0d+0 
term(224) = term(224) * (-2.0d+0) 
term(225) = term(225) * 4.0d+0 
term(227) = term(227) * (-2.0d+0) 
term(229) = term(229) * (-2.0d+0) 
term(231) = term(231) * (-2.0d+0) 
term(233) = term(233) * (-2.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(234) = term(234) + s1(p,i) * t1(a,j) * wm_interm_cc3_47_pt4(a, q, j, i)
term(235) = term(235) + s1(p,i) * t1(a,j) * wm_interm_cc3_48_pt4(a, q, j, i)
term(236) = term(236) + s1(p,i) * t1(a,j) * wm_interm_cc3_42_pt4(a, q, j, i)
term(237) = term(237) + s1(p,i) * t1(a,j) * wm_interm_cc3_41_pt4(a, q, j, i)
term(238) = term(238) + s1(p,i) * t1(a,j) * wm_interm_cc3_47_pt4(a, j, q, i)
term(239) = term(239) + s1(p,i) * t1(a,j) * wm_interm_cc3_48_pt4(a, j, q, i)
term(240) = term(240) + s1(p,i) * t1(a,j) * wm_interm_cc3_45_pt4(a, q, j, i)
term(241) = term(241) + s1(p,i) * t1(a,j) * wm_interm_cc3_46_pt4(a, q, j, i)
term(242) = term(242) + s1(p,i) * t1(a,j) * wm_interm_cc3_45_pt4(a, j, q, i)
term(243) = term(243) + s1(p,i) * t1(a,j) * wm_interm_cc3_46_pt4(a, j, q, i)
term(244) = term(244) + s1(p,i) * t1(a,j) * wm_interm_cc3_41_pt4(a, j, q, i)
term(245) = term(245) + s1(p,i) * t1(a,j) * wm_interm_cc3_42_pt4(a, j, q, i)
end do 
end do 
end do 

term(234) = term(234) * 2.0d+0 
term(235) = term(235) * (-4.0d+0) 
term(236) = -term(236) 
term(237) = term(237) * 2.0d+0 
term(238) = -term(238) 
term(239) = term(239) * 2.0d+0 
term(240) = -term(240) 
term(241) = term(241) * 2.0d+0 
term(242) = term(242) * 2.0d+0 
term(243) = term(243) * (-4.0d+0) 
term(244) = -term(244) 
term(245) = term(245) * 2.0d+0 


    calc_D_vo_wm_cc3_pt4 = zero
    do s = 0, 245
    calc_D_vo_wm_cc3_pt4 = calc_D_vo_wm_cc3_pt4 + term(s)
    end do

    end function calc_D_vo_wm_cc3_pt4
    
    function calc_D_vv_wm_cc3_pt4(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vv_wm_cc3_pt4
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
term(0) = term(0) + wm_interm_cc3_52_pt4(p, a, i, j) * wm_interm_cc3_61_pt4(q, a, i, j)
term(1) = term(1) + wm_interm_cc3_52_pt4(p, a, i, j) * wm_interm_cc3_61_pt4(q, a, j, i)
term(2) = term(2) + wm_interm_cc3_53_pt4(p, a, i, j) * wm_interm_cc3_61_pt4(q, a, i, j)
term(3) = term(3) + wm_interm_cc3_54_pt4(p, a, i, j) * wm_interm_cc3_61_pt4(q, a, i, j)
term(4) = term(4) + wm_interm_cc3_63_pt4(a, q, i, j) * wm_interm_cc3_64_pt4(p, a, j, i)
term(5) = term(5) + wm_interm_cc3_62_pt4(p, a, i, j) * wm_interm_cc3_63_pt4(a, q, j, i)
term(6) = term(6) + wm_interm_cc3_63_pt4(a, q, i, j) * wm_interm_cc3_64_pt4(p, a, i, j)
term(7) = term(7) + wm_interm_cc3_63_pt4(a, q, i, j) * wm_interm_cc3_64_pt4(a, p, i, j)
term(8) = term(8) + wm_interm_cc3_63_pt4(a, q, i, j) * wm_interm_cc3_64_pt4(a, p, j, i)
term(9) = term(9) + wm_interm_cc3_62_pt4(p, a, i, j) * wm_interm_cc3_63_pt4(a, q, i, j)
term(10) = term(10) + wm_interm_cc3_62_pt4(a, p, i, j) * wm_interm_cc3_63_pt4(a, q, j, i)
term(11) = term(11) + wm_interm_cc3_62_pt4(a, p, i, j) * wm_interm_cc3_63_pt4(a, q, i, j)
term(12) = term(12) + wm_interm_cc3_56_pt4(q, a, i, j) * wm_interm_cc3_65_pt4(p, a, i, j)
term(13) = term(13) + wm_interm_cc3_59_pt4(q, a, i, j) * wm_interm_cc3_65_pt4(p, a, i, j)
term(14) = term(14) + wm_interm_cc3_57_pt4(q, a, i, j) * wm_interm_cc3_65_pt4(p, a, i, j)
term(15) = term(15) + wm_interm_cc3_59_pt4(q, a, i, j) * wm_interm_cc3_65_pt4(p, a, j, i)
term(16) = term(16) + wm_interm_cc3_60_pt4(q, a, i, j) * wm_interm_cc3_65_pt4(p, a, i, j)
term(17) = term(17) + wm_interm_cc3_56_pt4(q, a, i, j) * wm_interm_cc3_65_pt4(p, a, j, i)
term(18) = term(18) + wm_interm_cc3_57_pt4(q, a, i, j) * wm_interm_cc3_65_pt4(p, a, j, i)
term(19) = term(19) + wm_interm_cc3_59_pt4(a, q, i, j) * wm_interm_cc3_65_pt4(p, a, i, j)
term(20) = term(20) + wm_interm_cc3_56_pt4(a, q, i, j) * wm_interm_cc3_65_pt4(p, a, i, j)
term(21) = term(21) + wm_interm_cc3_60_pt4(a, q, i, j) * wm_interm_cc3_65_pt4(p, a, i, j)
term(22) = term(22) + wm_interm_cc3_56_pt4(a, q, i, j) * wm_interm_cc3_65_pt4(p, a, j, i)
term(23) = term(23) + wm_interm_cc3_60_pt4(a, q, i, j) * wm_interm_cc3_65_pt4(p, a, j, i)
term(24) = term(24) + wm_interm_cc3_59_pt4(a, q, i, j) * wm_interm_cc3_65_pt4(p, a, j, i)
term(25) = term(25) + wm_interm_cc3_60_pt4(q, a, i, j) * wm_interm_cc3_65_pt4(p, a, j, i)
term(26) = term(26) + wm_interm_cc3_67_pt4(a, q, i, j) * wm_interm_cc3_68_pt4(p, a, j, i)
term(27) = term(27) + wm_interm_cc3_66_pt4(p, a, i, j) * wm_interm_cc3_67_pt4(a, q, j, i)
term(28) = term(28) + wm_interm_cc3_67_pt4(a, q, i, j) * wm_interm_cc3_68_pt4(p, a, i, j)
term(29) = term(29) + wm_interm_cc3_67_pt4(a, q, i, j) * wm_interm_cc3_70_pt4(p, a, j, i)
term(30) = term(30) + wm_interm_cc3_67_pt4(a, q, i, j) * wm_interm_cc3_69_pt4(p, a, j, i)
term(31) = term(31) + wm_interm_cc3_66_pt4(p, a, i, j) * wm_interm_cc3_67_pt4(a, q, i, j)
term(32) = term(32) + wm_interm_cc3_67_pt4(a, q, i, j) * wm_interm_cc3_71_pt4(p, a, j, i)
term(33) = term(33) + wm_interm_cc3_67_pt4(a, q, i, j) * wm_interm_cc3_71_pt4(p, a, i, j)
term(34) = term(34) + wm_interm_cc3_67_pt4(a, q, i, j) * wm_interm_cc3_68_pt4(a, p, j, i)
term(35) = term(35) + wm_interm_cc3_67_pt4(a, q, i, j) * wm_interm_cc3_69_pt4(a, p, j, i)
term(36) = term(36) + wm_interm_cc3_66_pt4(a, p, i, j) * wm_interm_cc3_67_pt4(a, q, j, i)
term(37) = term(37) + wm_interm_cc3_66_pt4(a, p, i, j) * wm_interm_cc3_67_pt4(a, q, i, j)
term(38) = term(38) + wm_interm_cc3_67_pt4(a, q, i, j) * wm_interm_cc3_70_pt4(p, a, i, j)
term(39) = term(39) + wm_interm_cc3_67_pt4(a, q, i, j) * wm_interm_cc3_69_pt4(p, a, i, j)
term(40) = term(40) + wm_interm_cc3_67_pt4(a, q, i, j) * wm_interm_cc3_69_pt4(a, p, i, j)
term(41) = term(41) + wm_interm_cc3_67_pt4(a, q, i, j) * wm_interm_cc3_68_pt4(a, p, i, j)
term(42) = term(42) + wm_interm_cc3_67_pt4(a, q, i, j) * wm_interm_cc3_71_pt4(a, p, i, j)
term(43) = term(43) + wm_interm_cc3_67_pt4(a, q, i, j) * wm_interm_cc3_70_pt4(a, p, j, i)
term(44) = term(44) + wm_interm_cc3_67_pt4(a, q, i, j) * wm_interm_cc3_71_pt4(a, p, j, i)
term(45) = term(45) + wm_interm_cc3_67_pt4(a, q, i, j) * wm_interm_cc3_70_pt4(a, p, i, j)
end do 
end do 
end do 

term(0) = term(0) * (-7.999999999999999d+0) 
term(1) = term(1) * 8.0d+0 
term(2) = term(2) * 4.0d+0 
term(3) = term(3) * (-4.0d+0) 
term(4) = term(4) * 4.0d+0 
term(5) = term(5) * (-4.0d+0) 
term(6) = term(6) * (-2.0d+0) 
term(7) = term(7) * 4.0d+0 
term(8) = term(8) * (-2.0d+0) 
term(9) = term(9) * 2.0d+0 
term(10) = term(10) * 2.0d+0 
term(11) = term(11) * (-4.0d+0) 
term(12) = term(12) * 5.333333333333333d+0 
term(13) = term(13) * (-2.6666666666666665d+0) 
term(14) = term(14) * (-1.3333333333333333d+0) 
term(15) = term(15) * 1.3333333333333333d+0 
term(16) = term(16) * 0.6666666666666666d+0 
term(17) = term(17) * (-2.6666666666666665d+0) 
term(18) = term(18) * 0.6666666666666666d+0 
term(19) = term(19) * 0.6666666666666666d+0 
term(20) = term(20) * (-1.3333333333333333d+0) 
term(21) = term(21) * (-1.3333333333333333d+0) 
term(22) = term(22) * 2.6666666666666665d+0 
term(23) = term(23) * 0.6666666666666666d+0 
term(24) = term(24) * (-1.3333333333333333d+0) 
term(25) = term(25) * (-1.3333333333333333d+0) 
term(26) = term(26) * (-1.3333333333333333d+0) 
term(27) = term(27) * 4.0d+0 
term(28) = term(28) * 0.6666666666666666d+0 
term(29) = term(29) * (-1.3333333333333333d+0) 
term(30) = term(30) * 0.3333333333333333d+0 
term(31) = term(31) * (-2.0d+0) 
term(32) = term(32) * 0.3333333333333333d+0 
term(33) = term(33) * (-0.6666666666666666d+0) 
term(34) = term(34) * 0.6666666666666666d+0 
term(35) = term(35) * (-0.6666666666666666d+0) 
term(36) = term(36) * (-2.0d+0) 
term(37) = term(37) * 4.0d+0 
term(38) = term(38) * 0.6666666666666666d+0 
term(39) = term(39) * (-0.6666666666666666d+0) 
term(40) = term(40) * 0.3333333333333333d+0 
term(41) = term(41) * (-1.3333333333333333d+0) 
term(42) = term(42) * 0.3333333333333333d+0 
term(43) = term(43) * 0.6666666666666666d+0 
term(44) = term(44) * (-0.6666666666666666d+0) 
term(45) = term(45) * (-1.3333333333333333d+0) 


    calc_D_vv_wm_cc3_pt4 = zero
    do s = 0, 45
    calc_D_vv_wm_cc3_pt4 = calc_D_vv_wm_cc3_pt4 + term(s)
    end do

    end function calc_D_vv_wm_cc3_pt4
    

    
end module density_exc_exc_functions_cc3_pt4
