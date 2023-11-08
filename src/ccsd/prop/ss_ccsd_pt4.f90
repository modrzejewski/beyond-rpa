module ss_ccsd_pt4
 use ccsd_transformed_integrals
    use cc3_intermediates
    use arithmetic
    use s_gen
    use basis
    use eom_vectors

        
    implicit none
    !
    ! File generated automatically on 2018-04-18 11:20:49
    !
    real(F64), dimension(:, :), allocatable :: wm_interm_0_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_1_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_2_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_3_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_4_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_5_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_6_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_7_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_8_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_9_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_10_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_11_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_12_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_13_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_14_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_15_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_16_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_17_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_18_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_19_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_20_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_21_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_22_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_23_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_24_pt4 
real(F64) :: wm_interm_25_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_26_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_27_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_28_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_29_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_30_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_31_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_32_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_33_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_34_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_35_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_36_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_37_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_38_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_39_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_40_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_41_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_42_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_43_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_44_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_45_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_46_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_47_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_48_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_49_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_50_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_51_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_52_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_53_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_54_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_55_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_56_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_57_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_58_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_59_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_60_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_61_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_62_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_63_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_64_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_65_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_66_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_67_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_68_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_69_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_70_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_71_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_72_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_73_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_74_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_75_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_76_pt4 
real(F64) :: wm_interm_77_pt4 
real(F64) :: wm_interm_78_pt4 

    contains
    
    subroutine wm_intermediates_ccsd_init_pt4(nocc, nactive)
    integer, intent(in) :: nocc
    integer, intent(in) :: nactive
    allocate(wm_interm_0_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_1_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_2_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_3_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_4_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_5_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_6_pt4(1: nocc, 1: nocc))
allocate(wm_interm_7_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_8_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_9_pt4(1: nocc, 1: nocc))
allocate(wm_interm_10_pt4(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_11_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_12_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_13_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_14_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_15_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_16_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_17_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_18_pt4(1: nocc, 1: nocc))
allocate(wm_interm_19_pt4(1: nocc, 1: nocc))
allocate(wm_interm_20_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_21_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_22_pt4(1: nocc, 1: nocc))
allocate(wm_interm_23_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_24_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_26_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_27_pt4(1: nocc, 1: nocc))
allocate(wm_interm_28_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_29_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_30_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_31_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_32_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_33_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_34_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_35_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_36_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_37_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_38_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_39_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_40_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_41_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_42_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_43_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_44_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_45_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_46_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_47_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_48_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_49_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_50_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_51_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_52_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_53_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_54_pt4(1: nocc, 1: nocc))
allocate(wm_interm_55_pt4(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_56_pt4(1: nocc, 1: nocc))
allocate(wm_interm_57_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_58_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_59_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_60_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_61_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_62_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_63_pt4(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_64_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_65_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_66_pt4(1: nocc, 1: nocc))
allocate(wm_interm_67_pt4(1: nocc, 1: nocc))
allocate(wm_interm_68_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_69_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_70_pt4(1: nocc, 1: nocc))
allocate(wm_interm_71_pt4(1: nocc, 1: nocc))
allocate(wm_interm_72_pt4(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_73_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_74_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_75_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_76_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
wm_interm_0_pt4 = zero 
wm_interm_1_pt4 = zero 
wm_interm_2_pt4 = zero 
wm_interm_3_pt4 = zero 
wm_interm_4_pt4 = zero 
wm_interm_5_pt4 = zero 
wm_interm_6_pt4 = zero 
wm_interm_7_pt4 = zero 
wm_interm_8_pt4 = zero 
wm_interm_9_pt4 = zero 
wm_interm_10_pt4 = zero 
wm_interm_11_pt4 = zero 
wm_interm_12_pt4 = zero 
wm_interm_13_pt4 = zero 
wm_interm_14_pt4 = zero 
wm_interm_15_pt4 = zero 
wm_interm_16_pt4 = zero 
wm_interm_17_pt4 = zero 
wm_interm_18_pt4 = zero 
wm_interm_19_pt4 = zero 
wm_interm_20_pt4 = zero 
wm_interm_21_pt4 = zero 
wm_interm_22_pt4 = zero 
wm_interm_23_pt4 = zero 
wm_interm_24_pt4 = zero 
wm_interm_25_pt4 = zero 
wm_interm_26_pt4 = zero 
wm_interm_27_pt4 = zero 
wm_interm_28_pt4 = zero 
wm_interm_29_pt4 = zero 
wm_interm_30_pt4 = zero 
wm_interm_31_pt4 = zero 
wm_interm_32_pt4 = zero 
wm_interm_33_pt4 = zero 
wm_interm_34_pt4 = zero 
wm_interm_35_pt4 = zero 
wm_interm_36_pt4 = zero 
wm_interm_37_pt4 = zero 
wm_interm_38_pt4 = zero 
wm_interm_39_pt4 = zero 
wm_interm_40_pt4 = zero 
wm_interm_41_pt4 = zero 
wm_interm_42_pt4 = zero 
wm_interm_43_pt4 = zero 
wm_interm_44_pt4 = zero 
wm_interm_45_pt4 = zero 
wm_interm_46_pt4 = zero 
wm_interm_47_pt4 = zero 
wm_interm_48_pt4 = zero 
wm_interm_49_pt4 = zero 
wm_interm_50_pt4 = zero 
wm_interm_51_pt4 = zero 
wm_interm_52_pt4 = zero 
wm_interm_53_pt4 = zero 
wm_interm_54_pt4 = zero 
wm_interm_55_pt4 = zero 
wm_interm_56_pt4 = zero 
wm_interm_57_pt4 = zero 
wm_interm_58_pt4 = zero 
wm_interm_59_pt4 = zero 
wm_interm_60_pt4 = zero 
wm_interm_61_pt4 = zero 
wm_interm_62_pt4 = zero 
wm_interm_63_pt4 = zero 
wm_interm_64_pt4 = zero 
wm_interm_65_pt4 = zero 
wm_interm_66_pt4 = zero 
wm_interm_67_pt4 = zero 
wm_interm_68_pt4 = zero 
wm_interm_69_pt4 = zero 
wm_interm_70_pt4 = zero 
wm_interm_71_pt4 = zero 
wm_interm_72_pt4 = zero 
wm_interm_73_pt4 = zero 
wm_interm_74_pt4 = zero 
wm_interm_75_pt4 = zero 
wm_interm_76_pt4 = zero 
wm_interm_77_pt4 = zero 
wm_interm_78_pt4 = zero 

    end subroutine wm_intermediates_ccsd_init_pt4
    
    subroutine wm_intermediates_ccsd_free_pt4
    deallocate(wm_interm_0_pt4)
deallocate(wm_interm_1_pt4)
deallocate(wm_interm_2_pt4)
deallocate(wm_interm_3_pt4)
deallocate(wm_interm_4_pt4)
deallocate(wm_interm_5_pt4)
deallocate(wm_interm_6_pt4)
deallocate(wm_interm_7_pt4)
deallocate(wm_interm_8_pt4)
deallocate(wm_interm_9_pt4)
deallocate(wm_interm_10_pt4)
deallocate(wm_interm_11_pt4)
deallocate(wm_interm_12_pt4)
deallocate(wm_interm_13_pt4)
deallocate(wm_interm_14_pt4)
deallocate(wm_interm_15_pt4)
deallocate(wm_interm_16_pt4)
deallocate(wm_interm_17_pt4)
deallocate(wm_interm_18_pt4)
deallocate(wm_interm_19_pt4)
deallocate(wm_interm_20_pt4)
deallocate(wm_interm_21_pt4)
deallocate(wm_interm_22_pt4)
deallocate(wm_interm_23_pt4)
deallocate(wm_interm_24_pt4)
deallocate(wm_interm_26_pt4)
deallocate(wm_interm_27_pt4)
deallocate(wm_interm_28_pt4)
deallocate(wm_interm_29_pt4)
deallocate(wm_interm_30_pt4)
deallocate(wm_interm_31_pt4)
deallocate(wm_interm_32_pt4)
deallocate(wm_interm_33_pt4)
deallocate(wm_interm_34_pt4)
deallocate(wm_interm_35_pt4)
deallocate(wm_interm_36_pt4)
deallocate(wm_interm_37_pt4)
deallocate(wm_interm_38_pt4)
deallocate(wm_interm_39_pt4)
deallocate(wm_interm_40_pt4)
deallocate(wm_interm_41_pt4)
deallocate(wm_interm_42_pt4)
deallocate(wm_interm_43_pt4)
deallocate(wm_interm_44_pt4)
deallocate(wm_interm_45_pt4)
deallocate(wm_interm_46_pt4)
deallocate(wm_interm_47_pt4)
deallocate(wm_interm_48_pt4)
deallocate(wm_interm_49_pt4)
deallocate(wm_interm_50_pt4)
deallocate(wm_interm_51_pt4)
deallocate(wm_interm_52_pt4)
deallocate(wm_interm_53_pt4)
deallocate(wm_interm_54_pt4)
deallocate(wm_interm_55_pt4)
deallocate(wm_interm_56_pt4)
deallocate(wm_interm_57_pt4)
deallocate(wm_interm_58_pt4)
deallocate(wm_interm_59_pt4)
deallocate(wm_interm_60_pt4)
deallocate(wm_interm_61_pt4)
deallocate(wm_interm_62_pt4)
deallocate(wm_interm_63_pt4)
deallocate(wm_interm_64_pt4)
deallocate(wm_interm_65_pt4)
deallocate(wm_interm_66_pt4)
deallocate(wm_interm_67_pt4)
deallocate(wm_interm_68_pt4)
deallocate(wm_interm_69_pt4)
deallocate(wm_interm_70_pt4)
deallocate(wm_interm_71_pt4)
deallocate(wm_interm_72_pt4)
deallocate(wm_interm_73_pt4)
deallocate(wm_interm_74_pt4)
deallocate(wm_interm_75_pt4)
deallocate(wm_interm_76_pt4)

    end subroutine wm_intermediates_ccsd_free_pt4
    
    subroutine wm_intermediates_ccsd_pt4(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr)
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
wm_interm_0_pt4(a, b) = wm_interm_0_pt4(a, b) + sum 
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
wm_interm_1_pt4(b, c, j, k) = wm_interm_1_pt4(b, c, j, k) + sum 
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
wm_interm_2_pt4(b, j) = wm_interm_2_pt4(b, j) + sum 
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
wm_interm_3_pt4(b, i, j, k) = wm_interm_3_pt4(b, i, j, k) + sum 
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
wm_interm_4_pt4(b, j) = wm_interm_4_pt4(b, j) + sum 
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
wm_interm_5_pt4(b, c, j, k) = wm_interm_5_pt4(b, c, j, k) + sum 
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
wm_interm_6_pt4(i, j) = wm_interm_6_pt4(i, j) + sum 
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
wm_interm_7_pt4(b, j) = wm_interm_7_pt4(b, j) + sum 
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
wm_interm_8_pt4(b, j) = wm_interm_8_pt4(b, j) + sum 
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
wm_interm_9_pt4(i, j) = wm_interm_9_pt4(i, j) + sum 
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
wm_interm_10_pt4(i, j, k, l) = wm_interm_10_pt4(i, j, k, l) + sum 
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
wm_interm_11_pt4(b, c, j, k) = wm_interm_11_pt4(b, c, j, k) + sum 
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
wm_interm_12_pt4(b, c, j, k) = wm_interm_12_pt4(b, c, j, k) + sum 
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
wm_interm_13_pt4(b, i, j, k) = wm_interm_13_pt4(b, i, j, k) + sum 
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
wm_interm_14_pt4(b, i, j, k) = wm_interm_14_pt4(b, i, j, k) + sum 
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
wm_interm_15_pt4(b, j) = wm_interm_15_pt4(b, j) + sum 
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
wm_interm_16_pt4(a, b) = wm_interm_16_pt4(a, b) + sum 
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
wm_interm_17_pt4(b, j) = wm_interm_17_pt4(b, j) + sum 
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
wm_interm_18_pt4(j, k) = wm_interm_18_pt4(j, k) + sum 
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
wm_interm_19_pt4(j, k) = wm_interm_19_pt4(j, k) + sum 
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
wm_interm_20_pt4(b, c) = wm_interm_20_pt4(b, c) + sum 
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
wm_interm_21_pt4(b, c) = wm_interm_21_pt4(b, c) + sum 
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
wm_interm_22_pt4(i, j) = wm_interm_22_pt4(i, j) + sum 
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
wm_interm_23_pt4(b, j, i, k) = wm_interm_23_pt4(b, j, i, k) + sum 
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
wm_interm_24_pt4(a, b) = wm_interm_24_pt4(a, b) + sum 
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
wm_interm_25_pt4 = wm_interm_25_pt4 + sum 
!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * r2(vrdav_Rr, a,j,b,k)
end do 
wm_interm_26_pt4(b, i, j, k) = wm_interm_26_pt4(b, i, j, k) + sum 
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
wm_interm_27_pt4(i, j) = wm_interm_27_pt4(i, j) + sum 
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
wm_interm_28_pt4(b, j, i, k) = wm_interm_28_pt4(b, j, i, k) + sum 
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
sum = sum + r2(vrdav_Rr, a,i,b,j) * s1(a,k)
end do 
wm_interm_29_pt4(b, i, j, k) = wm_interm_29_pt4(b, i, j, k) + sum 
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
wm_interm_30_pt4(a, b, i, j) = wm_interm_30_pt4(a, b, i, j) + sum 
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
wm_interm_31_pt4(b, j) = wm_interm_31_pt4(b, j) + sum 
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
sum = sum + r1(vrdav_Rl, a,i) * r2(vrdav_Rr, a,i,b,j)
end do 
end do 
wm_interm_32_pt4(b, j) = wm_interm_32_pt4(b, j) + sum 
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
wm_interm_33_pt4(a, b, i, j) = wm_interm_33_pt4(a, b, i, j) + sum 
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
sum = sum + r2(vrdav_Rr, a,j,b,i) * s1(a,i)
end do 
end do 
wm_interm_34_pt4(b, j) = wm_interm_34_pt4(b, j) + sum 
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
sum = sum + r1(vrdav_Rl, a,i) * r2(vrdav_Rr, a,j,b,i)
end do 
end do 
wm_interm_35_pt4(b, j) = wm_interm_35_pt4(b, j) + sum 
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
wm_interm_36_pt4(a, b, i, j) = wm_interm_36_pt4(a, b, i, j) + sum 
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
wm_interm_37_pt4(a, b) = wm_interm_37_pt4(a, b) + sum 
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
wm_interm_38_pt4(b, j) = wm_interm_38_pt4(b, j) + sum 
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
wm_interm_39_pt4(b, j, i, k) = wm_interm_39_pt4(b, j, i, k) + sum 
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
wm_interm_40_pt4(b, i, j, k) = wm_interm_40_pt4(b, i, j, k) + sum 
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
sum = sum + r2(vrdav_Rl, a,i,b,j) * r1(vrdav_Rr, a,i)
end do 
end do 
wm_interm_41_pt4(b, j) = wm_interm_41_pt4(b, j) + sum 
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
wm_interm_42_pt4(b, j, i, k) = wm_interm_42_pt4(b, j, i, k) + sum 
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
wm_interm_43_pt4(b, i, j, k) = wm_interm_43_pt4(b, i, j, k) + sum 
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
wm_interm_44_pt4(b, j) = wm_interm_44_pt4(b, j) + sum 
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
wm_interm_45_pt4(b, j) = wm_interm_45_pt4(b, j) + sum 
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
wm_interm_46_pt4(a, b, i, j) = wm_interm_46_pt4(a, b, i, j) + sum 
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
wm_interm_47_pt4(a, b, i, j) = wm_interm_47_pt4(a, b, i, j) + sum 
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
wm_interm_48_pt4(b, c, j, k) = wm_interm_48_pt4(b, c, j, k) + sum 
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
wm_interm_49_pt4(b, c, j, k) = wm_interm_49_pt4(b, c, j, k) + sum 
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
wm_interm_50_pt4(b, c, j, k) = wm_interm_50_pt4(b, c, j, k) + sum 
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
wm_interm_51_pt4(b, c) = wm_interm_51_pt4(b, c) + sum 
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
wm_interm_52_pt4(b, c) = wm_interm_52_pt4(b, c) + sum 
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
wm_interm_53_pt4(b, c, j, k) = wm_interm_53_pt4(b, c, j, k) + sum 
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
wm_interm_54_pt4(j, k) = wm_interm_54_pt4(j, k) + sum 
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
wm_interm_55_pt4(i, j, k, l) = wm_interm_55_pt4(i, j, k, l) + sum 
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
wm_interm_56_pt4(j, k) = wm_interm_56_pt4(j, k) + sum 
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
wm_interm_57_pt4(b, c, j, k) = wm_interm_57_pt4(b, c, j, k) + sum 
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
wm_interm_58_pt4(b, c, j, k) = wm_interm_58_pt4(b, c, j, k) + sum 
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
sum = sum + r2(vrdav_Rr, a,j,b,i) * s2(a,c,i,k)
end do 
end do 
wm_interm_59_pt4(b, c, j, k) = wm_interm_59_pt4(b, c, j, k) + sum 
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
sum = sum + r2(vrdav_Rr, a,i,b,j) * s2(a,c,i,k)
end do 
end do 
wm_interm_60_pt4(b, c, j, k) = wm_interm_60_pt4(b, c, j, k) + sum 
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
sum = sum + r2(vrdav_Rr, a,i,b,j) * s2(a,c,k,i)
end do 
end do 
wm_interm_61_pt4(b, c, j, k) = wm_interm_61_pt4(b, c, j, k) + sum 
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
sum = sum + r2(vrdav_Rr, a,j,b,i) * s2(a,c,k,i)
end do 
end do 
wm_interm_62_pt4(b, c, j, k) = wm_interm_62_pt4(b, c, j, k) + sum 
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
sum = sum + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,k,b,l)
end do 
end do 
wm_interm_63_pt4(i, j, k, l) = wm_interm_63_pt4(i, j, k, l) + sum 
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
wm_interm_64_pt4(b, c, j, k) = wm_interm_64_pt4(b, c, j, k) + sum 
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
wm_interm_65_pt4(b, c, j, k) = wm_interm_65_pt4(b, c, j, k) + sum 
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
sum = sum + r2(vrdav_Rl, a,j,b,i) * r2(vrdav_Rr, a,i,b,k)
end do 
end do 
end do 
wm_interm_66_pt4(j, k) = wm_interm_66_pt4(j, k) + sum 
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
wm_interm_67_pt4(j, k) = wm_interm_67_pt4(j, k) + sum 
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
wm_interm_68_pt4(b, c) = wm_interm_68_pt4(b, c) + sum 
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
wm_interm_69_pt4(b, c) = wm_interm_69_pt4(b, c) + sum 
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
wm_interm_70_pt4(j, k) = wm_interm_70_pt4(j, k) + sum 
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
wm_interm_71_pt4(j, k) = wm_interm_71_pt4(j, k) + sum 
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
wm_interm_72_pt4(i, j, k, l) = wm_interm_72_pt4(i, j, k, l) + sum 
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
wm_interm_73_pt4(b, i, k, j) = wm_interm_73_pt4(b, i, k, j) + sum 
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
sum = sum + r2(vrdav_Rl, a,j,b,i) * r2(vrdav_Rr, a,i,c,j)
end do 
end do 
end do 
wm_interm_74_pt4(b, c) = wm_interm_74_pt4(b, c) + sum 
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
wm_interm_75_pt4(b, c) = wm_interm_75_pt4(b, c) + sum 
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
wm_interm_76_pt4(b, i, j, k) = wm_interm_76_pt4(b, i, j, k) + sum 
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
sum = sum + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,j,b,i)
end do 
end do 
end do 
end do 
wm_interm_77_pt4 = wm_interm_77_pt4 + sum 
sum = zero 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,i,b,j)
end do 
end do 
end do 
end do 
wm_interm_78_pt4 = wm_interm_78_pt4 + sum 


    end subroutine wm_intermediates_ccsd_pt4
    
    
    function calc_D_oo_wm_pt4(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_oo_wm_pt4
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , k, i, j, a, b, l 
    real(F64), dimension(0:742) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(0) = term(0) + s1(a,i) * wm_interm_1_pt4(b,a,j,k) * wm_interm_26_pt4(b,k,j,i)
term(1) = term(1) + s1(a,i) * wm_interm_11_pt4(b,a,j,k) * wm_interm_26_pt4(b,k,j,i)
term(2) = term(2) + s1(a,i) * wm_interm_12_pt4(b,a,j,k) * wm_interm_26_pt4(b,k,j,i)
end do 
end do 
end do 
end do 
end do 

term(0) = term(0) * (-8.0d+0) 
term(1) = term(1) * (-8.0d+0) 
term(2) = term(2) * (16.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(3) = term(3) + s1(a,i) * wm_interm_1_pt4(b,a,j,k) * wm_interm_26_pt4(b,k,i,j)
term(4) = term(4) + s1(a,i) * wm_interm_26_pt4(b,j,i,k) * wm_interm_5_pt4(b,a,k,j)
term(5) = term(5) + s1(a,i) * wm_interm_11_pt4(b,a,j,k) * wm_interm_26_pt4(b,k,i,j)
term(6) = term(6) + s1(a,i) * wm_interm_12_pt4(b,a,j,k) * wm_interm_26_pt4(b,k,i,j)
end do 
end do 
end do 
end do 
end do 

term(3) = term(3) * (16.0d+0) 
term(4) = term(4) * (-8.0d+0) 
term(5) = term(5) * (16.0d+0) 
term(6) = term(6) * (-32.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(7) = term(7) + s1(a,i) * wm_interm_26_pt4(b,j,k,i) * wm_interm_5_pt4(b,a,k,j)
end do 
end do 
end do 
end do 
end do 

term(7) = term(7) * (16.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(8) = term(8) + s1(a,i) * wm_interm_10_pt4(j,k,l,i) * wm_interm_26_pt4(a,l,k,j)
end do 
end do 
end do 
end do 
end do 

term(8) = term(8) * (-8.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(9) = term(9) + s1(a,i) * wm_interm_10_pt4(j,k,l,i) * wm_interm_26_pt4(a,l,j,k)
end do 
end do 
end do 
end do 
end do 

term(9) = term(9) * (16.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(10) = term(10) + s1(a,q) * wm_interm_1_pt4(b,a,i,j) * wm_interm_26_pt4(b,j,i,p)
term(11) = term(11) + s1(a,q) * wm_interm_1_pt4(b,a,i,j) * wm_interm_26_pt4(b,j,p,i)
term(12) = term(12) + s1(a,q) * wm_interm_26_pt4(b,i,p,j) * wm_interm_5_pt4(b,a,j,i)
term(13) = term(13) + s1(a,i) * wm_interm_1_pt4(b,a,q,j) * wm_interm_26_pt4(b,j,p,i)
term(14) = term(14) + s1(a,i) * wm_interm_1_pt4(b,a,q,j) * wm_interm_26_pt4(b,j,i,p)
term(15) = term(15) + s1(a,i) * wm_interm_26_pt4(b,j,i,p) * wm_interm_5_pt4(b,a,q,j)
term(16) = term(16) + s1(a,i) * wm_interm_26_pt4(b,j,p,i) * wm_interm_5_pt4(b,a,q,j)
term(17) = term(17) + s1(a,q) * wm_interm_11_pt4(b,a,i,j) * wm_interm_26_pt4(b,j,i,p)
term(18) = term(18) + s1(a,q) * wm_interm_11_pt4(b,a,i,j) * wm_interm_26_pt4(b,j,p,i)
term(19) = term(19) + s1(a,q) * wm_interm_12_pt4(b,a,i,j) * wm_interm_26_pt4(b,j,i,p)
term(20) = term(20) + s1(a,q) * wm_interm_12_pt4(b,a,i,j) * wm_interm_26_pt4(b,j,p,i)
term(21) = term(21) + s1(a,i) * wm_interm_11_pt4(b,a,q,j) * wm_interm_26_pt4(b,j,p,i)
term(22) = term(22) + s1(a,i) * wm_interm_11_pt4(b,a,q,j) * wm_interm_26_pt4(b,j,i,p)
term(23) = term(23) + s1(a,i) * wm_interm_12_pt4(b,a,q,j) * wm_interm_26_pt4(b,j,p,i)
term(24) = term(24) + s1(a,i) * wm_interm_12_pt4(b,a,q,j) * wm_interm_26_pt4(b,j,i,p)
term(25) = term(25) + s1(a,i) * wm_interm_1_pt4(b,a,j,q) * wm_interm_26_pt4(b,p,i,j)
term(26) = term(26) + s1(a,i) * wm_interm_26_pt4(b,p,i,j) * wm_interm_5_pt4(b,a,j,q)
term(27) = term(27) + s1(a,i) * wm_interm_11_pt4(b,a,j,q) * wm_interm_26_pt4(b,p,i,j)
term(28) = term(28) + s1(a,i) * wm_interm_12_pt4(b,a,j,q) * wm_interm_26_pt4(b,p,i,j)
end do 
end do 
end do 
end do 

term(10) = term(10) * (4.0d+0) 
term(11) = term(11) * (-8.0d+0) 
term(12) = term(12) * (4.0d+0) 
term(13) = term(13) * (4.0d+0) 
term(14) = term(14) * (-8.0d+0) 
term(15) = term(15) * (4.0d+0) 
term(16) = term(16) * (-8.0d+0) 
term(17) = term(17) * (4.0d+0) 
term(18) = term(18) * (-8.0d+0) 
term(19) = term(19) * (-8.0d+0) 
term(20) = term(20) * (16.0d+0) 
term(21) = term(21) * (4.0d+0) 
term(22) = term(22) * (-8.0d+0) 
term(23) = term(23) * (-8.0d+0) 
term(24) = term(24) * (16.0d+0) 
term(25) = term(25) * (-8.0d+0) 
term(26) = term(26) * (4.0d+0) 
term(27) = term(27) * (-8.0d+0) 
term(28) = term(28) * (16.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(29) = term(29) + s1(a,i) * wm_interm_14_pt4(a,j,k,l) * wm_interm_72_pt4(j,i,l,k)
term(30) = term(30) + s1(a,i) * wm_interm_14_pt4(a,j,k,l) * wm_interm_72_pt4(j,i,k,l)
term(31) = term(31) + t1(a,i) * wm_interm_13_pt4(a,j,k,l) * wm_interm_55_pt4(j,i,l,k)
term(32) = term(32) + t1(a,i) * wm_interm_13_pt4(a,j,k,l) * wm_interm_55_pt4(j,i,k,l)
term(33) = term(33) + t1(a,i) * wm_interm_13_pt4(a,j,k,l) * wm_interm_55_pt4(i,j,k,l)
term(34) = term(34) + t1(a,i) * wm_interm_13_pt4(a,j,k,l) * wm_interm_55_pt4(i,j,l,k)
end do 
end do 
end do 
end do 
end do 

term(29) = term(29) * (-8.0d+0) 
term(30) = term(30) * (16.0d+0) 
term(31) = term(31) * (4.0d+0) 
term(32) = term(32) * (-8.0d+0) 
term(33) = term(33) * (4.0d+0) 
term(34) = term(34) * (-8.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(35) = term(35) + wm_interm_31_pt4(a,i) * wm_interm_44_pt4(a,i)
term(36) = term(36) + wm_interm_34_pt4(a,i) * wm_interm_44_pt4(a,i)
term(37) = term(37) + wm_interm_31_pt4(a,i) * wm_interm_45_pt4(a,i)
term(38) = term(38) + wm_interm_34_pt4(a,i) * wm_interm_45_pt4(a,i)
term(39) = term(39) + wm_interm_28_pt4(a,p,i,q) * wm_interm_44_pt4(a,i)
term(40) = term(40) + wm_interm_28_pt4(a,i,p,q) * wm_interm_44_pt4(a,i)
term(41) = term(41) + wm_interm_28_pt4(a,p,i,q) * wm_interm_45_pt4(a,i)
term(42) = term(42) + wm_interm_28_pt4(a,i,p,q) * wm_interm_45_pt4(a,i)
term(43) = term(43) + wm_interm_31_pt4(a,i) * wm_interm_43_pt4(a,i,p,q)
term(44) = term(44) + wm_interm_31_pt4(a,i) * wm_interm_43_pt4(a,p,i,q)
term(45) = term(45) + wm_interm_31_pt4(a,i) * wm_interm_42_pt4(a,p,i,q)
term(46) = term(46) + wm_interm_31_pt4(a,i) * wm_interm_42_pt4(a,i,p,q)
term(47) = term(47) + wm_interm_34_pt4(a,i) * wm_interm_43_pt4(a,i,p,q)
term(48) = term(48) + wm_interm_34_pt4(a,i) * wm_interm_43_pt4(a,p,i,q)
term(49) = term(49) + wm_interm_34_pt4(a,i) * wm_interm_42_pt4(a,p,i,q)
term(50) = term(50) + wm_interm_34_pt4(a,i) * wm_interm_42_pt4(a,i,p,q)
term(51) = term(51) + r1(vrdav_Rl, a,i) * wm_interm_18_pt4(q,i) * wm_interm_31_pt4(a,p)
term(52) = term(52) + r1(vrdav_Rl, a,i) * wm_interm_19_pt4(q,i) * wm_interm_31_pt4(a,p)
term(53) = term(53) + r1(vrdav_Rl, a,i) * wm_interm_18_pt4(q,i) * wm_interm_34_pt4(a,p)
term(54) = term(54) + r1(vrdav_Rl, a,i) * wm_interm_19_pt4(q,i) * wm_interm_34_pt4(a,p)
term(55) = term(55) + r1(vrdav_Rl, a,q) * wm_interm_70_pt4(p,i) * wm_interm_8_pt4(a,i)
term(56) = term(56) + r1(vrdav_Rl, a,q) * wm_interm_71_pt4(p,i) * wm_interm_8_pt4(a,i)
term(57) = term(57) + s1(a,q) * wm_interm_15_pt4(a,i) * wm_interm_70_pt4(p,i)
term(58) = term(58) + s1(a,q) * wm_interm_15_pt4(a,i) * wm_interm_71_pt4(p,i)
term(59) = term(59) + r1(vrdav_Rl, a,q) * wm_interm_70_pt4(p,i) * wm_interm_7_pt4(a,i)
term(60) = term(60) + r1(vrdav_Rl, a,q) * wm_interm_71_pt4(p,i) * wm_interm_7_pt4(a,i)
term(61) = term(61) + s1(a,q) * wm_interm_17_pt4(a,i) * wm_interm_70_pt4(p,i)
term(62) = term(62) + s1(a,q) * wm_interm_17_pt4(a,i) * wm_interm_71_pt4(p,i)
term(63) = term(63) + r1(vrdav_Rl, a,p) * wm_interm_18_pt4(i,q) * wm_interm_31_pt4(a,i)
term(64) = term(64) + s1(a,i) * wm_interm_18_pt4(p,q) * wm_interm_32_pt4(a,i)
term(65) = term(65) + r1(vrdav_Rl, a,p) * wm_interm_19_pt4(i,q) * wm_interm_31_pt4(a,i)
term(66) = term(66) + s1(a,i) * wm_interm_19_pt4(p,q) * wm_interm_32_pt4(a,i)
term(67) = term(67) + r1(vrdav_Rl, a,p) * wm_interm_18_pt4(i,q) * wm_interm_34_pt4(a,i)
term(68) = term(68) + s1(a,i) * wm_interm_18_pt4(p,q) * wm_interm_35_pt4(a,i)
term(69) = term(69) + r1(vrdav_Rl, a,p) * wm_interm_19_pt4(i,q) * wm_interm_34_pt4(a,i)
term(70) = term(70) + s1(a,i) * wm_interm_19_pt4(p,q) * wm_interm_35_pt4(a,i)
term(71) = term(71) + s1(a,i) * wm_interm_17_pt4(a,q) * wm_interm_70_pt4(i,p)
term(72) = term(72) + r1(vrdav_Rl, a,i) * wm_interm_70_pt4(i,p) * wm_interm_7_pt4(a,q)
term(73) = term(73) + s1(a,i) * wm_interm_17_pt4(a,q) * wm_interm_71_pt4(i,p)
term(74) = term(74) + r1(vrdav_Rl, a,i) * wm_interm_71_pt4(i,p) * wm_interm_7_pt4(a,q)
term(75) = term(75) + r1(vrdav_Rl, a,i) * wm_interm_70_pt4(i,p) * wm_interm_8_pt4(a,q)
term(76) = term(76) + r1(vrdav_Rl, a,i) * wm_interm_71_pt4(i,p) * wm_interm_8_pt4(a,q)
term(77) = term(77) + s1(a,i) * wm_interm_15_pt4(a,q) * wm_interm_70_pt4(i,p)
term(78) = term(78) + s1(a,i) * wm_interm_15_pt4(a,q) * wm_interm_71_pt4(i,p)
term(79) = term(79) + s1(a,p) * wm_interm_18_pt4(i,q) * wm_interm_35_pt4(a,i)
term(80) = term(80) + s1(a,p) * wm_interm_18_pt4(i,q) * wm_interm_32_pt4(a,i)
term(81) = term(81) + s1(a,p) * wm_interm_19_pt4(i,q) * wm_interm_35_pt4(a,i)
term(82) = term(82) + s1(a,p) * wm_interm_19_pt4(i,q) * wm_interm_32_pt4(a,i)
term(83) = term(83) + r1(vrdav_Rr, a,p) * wm_interm_18_pt4(q,i) * wm_interm_44_pt4(a,i)
term(84) = term(84) + r1(vrdav_Rr, a,p) * wm_interm_19_pt4(q,i) * wm_interm_44_pt4(a,i)
term(85) = term(85) + r1(vrdav_Rr, a,p) * wm_interm_18_pt4(q,i) * wm_interm_45_pt4(a,i)
term(86) = term(86) + r1(vrdav_Rr, a,p) * wm_interm_19_pt4(q,i) * wm_interm_45_pt4(a,i)
term(87) = term(87) + t1(a,q) * wm_interm_18_pt4(p,i) * wm_interm_38_pt4(a,i)
term(88) = term(88) + t1(a,q) * wm_interm_19_pt4(p,i) * wm_interm_38_pt4(a,i)
term(89) = term(89) + t1(a,q) * wm_interm_18_pt4(p,i) * wm_interm_41_pt4(a,i)
term(90) = term(90) + t1(a,q) * wm_interm_19_pt4(p,i) * wm_interm_41_pt4(a,i)
term(91) = term(91) + t1(a,q) * wm_interm_2_pt4(a,i) * wm_interm_54_pt4(p,i)
term(92) = term(92) + t1(a,q) * wm_interm_4_pt4(a,i) * wm_interm_54_pt4(p,i)
term(93) = term(93) + t1(a,q) * wm_interm_2_pt4(a,i) * wm_interm_56_pt4(p,i)
term(94) = term(94) + t1(a,q) * wm_interm_4_pt4(a,i) * wm_interm_56_pt4(p,i)
term(95) = term(95) + t1(a,i) * wm_interm_4_pt4(a,p) * wm_interm_56_pt4(i,q)
term(96) = term(96) + t1(a,i) * wm_interm_2_pt4(a,p) * wm_interm_56_pt4(i,q)
term(97) = term(97) + t1(a,i) * wm_interm_4_pt4(a,p) * wm_interm_54_pt4(i,q)
term(98) = term(98) + t1(a,i) * wm_interm_2_pt4(a,p) * wm_interm_54_pt4(i,q)
term(99) = term(99) + t1(a,i) * wm_interm_18_pt4(i,q) * wm_interm_38_pt4(a,p)
term(100) = term(100) + t1(a,i) * wm_interm_19_pt4(i,q) * wm_interm_38_pt4(a,p)
term(101) = term(101) + t1(a,i) * wm_interm_18_pt4(i,q) * wm_interm_41_pt4(a,p)
term(102) = term(102) + t1(a,i) * wm_interm_19_pt4(i,q) * wm_interm_41_pt4(a,p)
term(103) = term(103) + r1(vrdav_Rr, a,i) * wm_interm_18_pt4(i,q) * wm_interm_44_pt4(a,p)
term(104) = term(104) + t1(a,i) * wm_interm_18_pt4(p,q) * wm_interm_41_pt4(a,i)
term(105) = term(105) + r1(vrdav_Rr, a,i) * wm_interm_19_pt4(i,q) * wm_interm_44_pt4(a,p)
term(106) = term(106) + t1(a,i) * wm_interm_19_pt4(p,q) * wm_interm_41_pt4(a,i)
term(107) = term(107) + r1(vrdav_Rr, a,i) * wm_interm_18_pt4(i,q) * wm_interm_45_pt4(a,p)
term(108) = term(108) + t1(a,i) * wm_interm_18_pt4(p,q) * wm_interm_38_pt4(a,i)
term(109) = term(109) + r1(vrdav_Rr, a,i) * wm_interm_19_pt4(i,q) * wm_interm_45_pt4(a,p)
term(110) = term(110) + t1(a,i) * wm_interm_19_pt4(p,q) * wm_interm_38_pt4(a,i)
end do 
end do 

term(35) = term(35) * (-16.0d+0) 
term(36) = term(36) * (8.0d+0) 
term(37) = term(37) * (8.0d+0) 
term(38) = term(38) * (-4.0d+0) 
term(39) = term(39) * (4.0d+0) 
term(40) = term(40) * (-8.0d+0) 
term(41) = term(41) * (-2.0d+0) 
term(42) = term(42) * (4.0d+0) 
term(43) = term(43) * (-2.0d+0) 
term(44) = term(44) * (4.0d+0) 
term(45) = term(45) * (-2.0d+0) 
term(46) = term(46) * (4.0d+0) 
term(48) = term(48) * (-2.0d+0) 
term(50) = term(50) * (-2.0d+0) 
term(51) = term(51) * (-4.0d+0) 
term(52) = term(52) * (8.0d+0) 
term(53) = term(53) * (2.0d+0) 
term(54) = term(54) * (-4.0d+0) 
term(55) = term(55) * (4.0d+0) 
term(56) = term(56) * (-8.0d+0) 
term(57) = term(57) * (-8.0d+0) 
term(58) = term(58) * (16.0d+0) 
term(59) = term(59) * (-8.0d+0) 
term(60) = term(60) * (16.0d+0) 
term(61) = term(61) * (4.0d+0) 
term(62) = term(62) * (-8.0d+0) 
term(63) = term(63) * (-4.0d+0) 
term(64) = term(64) * (8.0d+0) 
term(65) = term(65) * (8.0d+0) 
term(66) = term(66) * (-16.0d+0) 
term(67) = term(67) * (2.0d+0) 
term(68) = term(68) * (-4.0d+0) 
term(69) = term(69) * (-4.0d+0) 
term(70) = term(70) * (8.0d+0) 
term(71) = term(71) * (4.0d+0) 
term(72) = term(72) * (-8.0d+0) 
term(73) = term(73) * (-8.0d+0) 
term(74) = term(74) * (16.0d+0) 
term(75) = term(75) * (4.0d+0) 
term(76) = term(76) * (-8.0d+0) 
term(77) = term(77) * (-8.0d+0) 
term(78) = term(78) * (16.0d+0) 
term(79) = term(79) * (4.0d+0) 
term(80) = term(80) * (-8.0d+0) 
term(81) = term(81) * (-8.0d+0) 
term(82) = term(82) * (16.0d+0) 
term(83) = term(83) * (8.0d+0) 
term(84) = term(84) * (-16.0d+0) 
term(85) = term(85) * (-4.0d+0) 
term(86) = term(86) * (8.0d+0) 
term(87) = term(87) * (-2.0d+0) 
term(88) = term(88) * (4.0d+0) 
term(89) = term(89) * (4.0d+0) 
term(90) = term(90) * (-8.0d+0) 
term(91) = term(91) * (4.0d+0) 
term(92) = term(92) * (-2.0d+0) 
term(93) = term(93) * (-8.0d+0) 
term(94) = term(94) * (4.0d+0) 
term(95) = term(95) * (4.0d+0) 
term(96) = term(96) * (-8.0d+0) 
term(97) = term(97) * (-2.0d+0) 
term(98) = term(98) * (4.0d+0) 
term(99) = term(99) * (-2.0d+0) 
term(100) = term(100) * (4.0d+0) 
term(101) = term(101) * (4.0d+0) 
term(102) = term(102) * (-8.0d+0) 
term(103) = term(103) * (4.0d+0) 
term(104) = term(104) * (-8.0d+0) 
term(105) = term(105) * (-8.0d+0) 
term(106) = term(106) * (16.0d+0) 
term(107) = term(107) * (-2.0d+0) 
term(108) = term(108) * (4.0d+0) 
term(109) = term(109) * (4.0d+0) 
term(110) = term(110) * (-8.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(111) = term(111) + s1(a,i) * wm_interm_14_pt4(b,j,k,i) * wm_interm_59_pt4(a,b,j,k)
term(112) = term(112) + s1(a,i) * wm_interm_14_pt4(b,j,k,i) * wm_interm_60_pt4(a,b,j,k)
term(113) = term(113) + s1(a,i) * wm_interm_14_pt4(b,j,k,i) * wm_interm_61_pt4(a,b,j,k)
term(114) = term(114) + s1(a,i) * wm_interm_14_pt4(b,j,k,i) * wm_interm_62_pt4(a,b,j,k)
end do 
end do 
end do 
end do 
end do 

term(111) = term(111) * (-8.0d+0) 
term(112) = term(112) * (16.0d+0) 
term(113) = term(113) * (-8.0d+0) 
term(114) = term(114) * (16.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(115) = term(115) + r1(vrdav_Rl, a,p) * wm_interm_11_pt4(b,a,i,q) * wm_interm_31_pt4(b,i)
term(116) = term(116) + r1(vrdav_Rl, a,p) * wm_interm_12_pt4(b,a,i,q) * wm_interm_31_pt4(b,i)
term(117) = term(117) + r1(vrdav_Rl, a,p) * wm_interm_11_pt4(b,a,i,q) * wm_interm_34_pt4(b,i)
term(118) = term(118) + r1(vrdav_Rl, a,p) * wm_interm_12_pt4(b,a,i,q) * wm_interm_34_pt4(b,i)
term(119) = term(119) + r1(vrdav_Rl, a,p) * wm_interm_1_pt4(b,a,i,q) * wm_interm_31_pt4(b,i)
term(120) = term(120) + r1(vrdav_Rl, a,p) * wm_interm_31_pt4(b,i) * wm_interm_5_pt4(b,a,i,q)
term(121) = term(121) + r1(vrdav_Rl, a,p) * wm_interm_1_pt4(b,a,i,q) * wm_interm_34_pt4(b,i)
term(122) = term(122) + r1(vrdav_Rl, a,p) * wm_interm_34_pt4(b,i) * wm_interm_5_pt4(b,a,i,q)
term(123) = term(123) + s1(a,p) * wm_interm_1_pt4(b,a,i,q) * wm_interm_35_pt4(b,i)
term(124) = term(124) + s1(a,p) * wm_interm_1_pt4(b,a,i,q) * wm_interm_32_pt4(b,i)
term(125) = term(125) + s1(a,p) * wm_interm_35_pt4(b,i) * wm_interm_5_pt4(b,a,i,q)
term(126) = term(126) + s1(a,p) * wm_interm_32_pt4(b,i) * wm_interm_5_pt4(b,a,i,q)
term(127) = term(127) + s1(a,p) * wm_interm_11_pt4(b,a,i,q) * wm_interm_35_pt4(b,i)
term(128) = term(128) + s1(a,p) * wm_interm_11_pt4(b,a,i,q) * wm_interm_32_pt4(b,i)
term(129) = term(129) + s1(a,p) * wm_interm_12_pt4(b,a,i,q) * wm_interm_35_pt4(b,i)
term(130) = term(130) + s1(a,p) * wm_interm_12_pt4(b,a,i,q) * wm_interm_32_pt4(b,i)
end do 
end do 
end do 

term(115) = term(115) * (8.0d+0) 
term(116) = term(116) * (-16.0d+0) 
term(117) = term(117) * (-4.0d+0) 
term(118) = term(118) * (8.0d+0) 
term(119) = term(119) * (8.0d+0) 
term(120) = term(120) * (-4.0d+0) 
term(121) = term(121) * (-4.0d+0) 
term(122) = term(122) * (2.0d+0) 
term(123) = term(123) * (-8.0d+0) 
term(124) = term(124) * (16.0d+0) 
term(125) = term(125) * (4.0d+0) 
term(126) = term(126) * (-8.0d+0) 
term(127) = term(127) * (-8.0d+0) 
term(128) = term(128) * (16.0d+0) 
term(129) = term(129) * (16.0d+0) 
term(130) = term(130) * (-32.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
term(131) = term(131) + wm_interm_27_pt4(i,j) * wm_interm_63_pt4(q,j,i,p)
term(132) = term(132) + wm_interm_27_pt4(i,j) * wm_interm_63_pt4(q,j,p,i)
term(133) = term(133) + wm_interm_27_pt4(i,j) * wm_interm_63_pt4(j,q,p,i)
term(134) = term(134) + wm_interm_27_pt4(i,j) * wm_interm_63_pt4(j,q,i,p)
end do 
end do 

term(131) = term(131) * (-1.0d+0) 
term(132) = term(132) * (2.0d+0) 
term(133) = term(133) * (-1.0d+0) 
term(134) = term(134) * (2.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(135) = term(135) + s1(a,i) * wm_interm_14_pt4(b,j,i,k) * wm_interm_62_pt4(a,b,j,k)
term(136) = term(136) + s1(a,i) * wm_interm_14_pt4(b,j,i,k) * wm_interm_61_pt4(a,b,j,k)
term(137) = term(137) + s1(a,i) * wm_interm_14_pt4(b,j,i,k) * wm_interm_59_pt4(a,b,j,k)
term(138) = term(138) + s1(a,i) * wm_interm_14_pt4(b,j,i,k) * wm_interm_60_pt4(a,b,j,k)
term(139) = term(139) + t1(a,i) * wm_interm_11_pt4(a,b,j,k) * wm_interm_40_pt4(b,k,i,j)
term(140) = term(140) + t1(a,i) * wm_interm_12_pt4(a,b,j,k) * wm_interm_40_pt4(b,k,i,j)
term(141) = term(141) + t1(a,i) * wm_interm_11_pt4(a,b,j,k) * wm_interm_39_pt4(b,k,i,j)
term(142) = term(142) + t1(a,i) * wm_interm_12_pt4(a,b,j,k) * wm_interm_39_pt4(b,k,i,j)
term(143) = term(143) + t1(a,i) * wm_interm_1_pt4(a,b,j,k) * wm_interm_40_pt4(b,k,i,j)
term(144) = term(144) + t1(a,i) * wm_interm_40_pt4(b,j,i,k) * wm_interm_5_pt4(a,b,k,j)
term(145) = term(145) + t1(a,i) * wm_interm_39_pt4(b,j,i,k) * wm_interm_5_pt4(a,b,k,j)
term(146) = term(146) + t1(a,i) * wm_interm_1_pt4(a,b,j,k) * wm_interm_39_pt4(b,k,i,j)
term(147) = term(147) + r1(vrdav_Rr, a,i) * wm_interm_11_pt4(a,b,j,k) * wm_interm_42_pt4(b,k,i,j)
term(148) = term(148) + r1(vrdav_Rr, a,i) * wm_interm_12_pt4(a,b,j,k) * wm_interm_42_pt4(b,k,i,j)
term(149) = term(149) + r1(vrdav_Rr, a,i) * wm_interm_42_pt4(b,j,i,k) * wm_interm_5_pt4(a,b,k,j)
term(150) = term(150) + r1(vrdav_Rr, a,i) * wm_interm_1_pt4(a,b,j,k) * wm_interm_42_pt4(b,k,i,j)
term(151) = term(151) + r1(vrdav_Rr, a,i) * wm_interm_11_pt4(a,b,j,k) * wm_interm_43_pt4(b,k,i,j)
term(152) = term(152) + r1(vrdav_Rr, a,i) * wm_interm_12_pt4(a,b,j,k) * wm_interm_43_pt4(b,k,i,j)
term(153) = term(153) + r1(vrdav_Rr, a,i) * wm_interm_1_pt4(a,b,j,k) * wm_interm_43_pt4(b,k,i,j)
term(154) = term(154) + r1(vrdav_Rr, a,i) * wm_interm_43_pt4(b,j,i,k) * wm_interm_5_pt4(a,b,k,j)
end do 
end do 
end do 
end do 
end do 

term(135) = term(135) * (-8.0d+0) 
term(136) = term(136) * (16.0d+0) 
term(137) = term(137) * (16.0d+0) 
term(138) = term(138) * (-32.0d+0) 
term(139) = term(139) * (4.0d+0) 
term(140) = term(140) * (-8.0d+0) 
term(141) = term(141) * (-8.0d+0) 
term(142) = term(142) * (16.0d+0) 
term(143) = term(143) * (4.0d+0) 
term(144) = term(144) * (-8.0d+0) 
term(145) = term(145) * (4.0d+0) 
term(146) = term(146) * (-8.0d+0) 
term(147) = term(147) * (-8.0d+0) 
term(148) = term(148) * (16.0d+0) 
term(149) = term(149) * (4.0d+0) 
term(150) = term(150) * (-8.0d+0) 
term(151) = term(151) * (4.0d+0) 
term(152) = term(152) * (-8.0d+0) 
term(153) = term(153) * (4.0d+0) 
term(154) = term(154) * (-8.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
term(155) = term(155) + wm_interm_27_pt4(i,j) * wm_interm_66_pt4(j,i)
term(156) = term(156) + wm_interm_27_pt4(i,j) * wm_interm_67_pt4(j,i)
term(157) = term(157) + wm_interm_27_pt4(i,j) * wm_interm_6_pt4(j,i)
term(158) = term(158) + wm_interm_22_pt4(i,j) * wm_interm_9_pt4(i,j)
end do 
end do 

term(155) = term(155) * (4.0d+0) 
term(156) = term(156) * (-8.0d+0) 
term(157) = term(157) * (-4.0d+0) 
term(158) = term(158) * (-4.0d+0) 

do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(159) = term(159) + wm_interm_37_pt4(a,b) * wm_interm_58_pt4(b,a,q,p)
term(160) = term(160) + wm_interm_37_pt4(a,b) * wm_interm_57_pt4(b,a,q,p)
term(161) = term(161) + wm_interm_37_pt4(a,b) * wm_interm_64_pt4(b,a,q,p)
term(162) = term(162) + wm_interm_37_pt4(a,b) * wm_interm_65_pt4(b,a,q,p)
term(163) = term(163) + wm_interm_37_pt4(a,b) * wm_interm_74_pt4(b,a)
term(164) = term(164) + wm_interm_37_pt4(a,b) * wm_interm_75_pt4(b,a)
term(165) = term(165) + wm_interm_36_pt4(a,b,p,q) * wm_interm_74_pt4(b,a)
term(166) = term(166) + wm_interm_36_pt4(a,b,p,q) * wm_interm_75_pt4(b,a)
term(167) = term(167) + r1(vrdav_Rl, b,q) * wm_interm_20_pt4(a,b) * wm_interm_31_pt4(a,p)
term(168) = term(168) + r1(vrdav_Rl, b,q) * wm_interm_21_pt4(a,b) * wm_interm_31_pt4(a,p)
term(169) = term(169) + r1(vrdav_Rl, b,q) * wm_interm_20_pt4(a,b) * wm_interm_34_pt4(a,p)
term(170) = term(170) + r1(vrdav_Rl, b,q) * wm_interm_21_pt4(a,b) * wm_interm_34_pt4(a,p)
term(171) = term(171) + r1(vrdav_Rl, a,p) * wm_interm_68_pt4(a,b) * wm_interm_7_pt4(b,q)
term(172) = term(172) + r1(vrdav_Rl, a,p) * wm_interm_69_pt4(a,b) * wm_interm_7_pt4(b,q)
term(173) = term(173) + r1(vrdav_Rl, a,p) * wm_interm_68_pt4(a,b) * wm_interm_8_pt4(b,q)
term(174) = term(174) + r1(vrdav_Rl, a,p) * wm_interm_69_pt4(a,b) * wm_interm_8_pt4(b,q)
term(175) = term(175) + s1(a,p) * wm_interm_17_pt4(b,q) * wm_interm_68_pt4(a,b)
term(176) = term(176) + s1(a,p) * wm_interm_17_pt4(b,q) * wm_interm_69_pt4(a,b)
term(177) = term(177) + s1(a,p) * wm_interm_15_pt4(b,q) * wm_interm_68_pt4(a,b)
term(178) = term(178) + s1(a,p) * wm_interm_15_pt4(b,q) * wm_interm_69_pt4(a,b)
term(179) = term(179) + r1(vrdav_Rr, a,p) * wm_interm_20_pt4(a,b) * wm_interm_44_pt4(b,q)
term(180) = term(180) + r1(vrdav_Rr, a,p) * wm_interm_21_pt4(a,b) * wm_interm_44_pt4(b,q)
term(181) = term(181) + r1(vrdav_Rr, a,p) * wm_interm_20_pt4(a,b) * wm_interm_45_pt4(b,q)
term(182) = term(182) + r1(vrdav_Rr, a,p) * wm_interm_21_pt4(a,b) * wm_interm_45_pt4(b,q)
end do 
end do 

term(159) = term(159) * (-2.0d+0) 
term(160) = term(160) * (4.0d+0) 
term(161) = term(161) * (-2.0d+0) 
term(162) = term(162) * (4.0d+0) 
term(163) = term(163) * (4.0d+0) 
term(164) = term(164) * (-8.0d+0) 
term(165) = term(165) * (-2.0d+0) 
term(166) = term(166) * (4.0d+0) 
term(167) = term(167) * (-4.0d+0) 
term(168) = term(168) * (8.0d+0) 
term(169) = term(169) * (2.0d+0) 
term(170) = term(170) * (-4.0d+0) 
term(171) = term(171) * (-8.0d+0) 
term(172) = term(172) * (16.0d+0) 
term(173) = term(173) * (4.0d+0) 
term(174) = term(174) * (-8.0d+0) 
term(175) = term(175) * (4.0d+0) 
term(176) = term(176) * (-8.0d+0) 
term(177) = term(177) * (-8.0d+0) 
term(178) = term(178) * (16.0d+0) 
term(179) = term(179) * (8.0d+0) 
term(180) = term(180) * (-16.0d+0) 
term(181) = term(181) * (-4.0d+0) 
term(182) = term(182) * (8.0d+0) 

do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(183) = term(183) + s1(a,i) * wm_interm_10_pt4(j,k,i,q) * wm_interm_26_pt4(a,p,k,j)
term(184) = term(184) + s1(a,p) * wm_interm_10_pt4(i,j,k,q) * wm_interm_26_pt4(a,k,i,j)
term(185) = term(185) + t1(a,i) * wm_interm_10_pt4(j,i,k,q) * wm_interm_40_pt4(a,k,p,j)
term(186) = term(186) + t1(a,i) * wm_interm_10_pt4(i,j,k,q) * wm_interm_40_pt4(a,k,p,j)
term(187) = term(187) + t1(a,i) * wm_interm_10_pt4(i,j,k,q) * wm_interm_40_pt4(a,p,k,j)
term(188) = term(188) + t1(a,i) * wm_interm_10_pt4(j,i,k,q) * wm_interm_40_pt4(a,p,k,j)
term(189) = term(189) + t1(a,i) * wm_interm_10_pt4(i,j,k,q) * wm_interm_39_pt4(a,p,k,j)
term(190) = term(190) + t1(a,i) * wm_interm_10_pt4(i,j,k,q) * wm_interm_39_pt4(a,k,p,j)
term(191) = term(191) + t1(a,i) * wm_interm_10_pt4(j,i,k,q) * wm_interm_39_pt4(a,k,p,j)
term(192) = term(192) + t1(a,i) * wm_interm_10_pt4(j,i,k,q) * wm_interm_39_pt4(a,p,k,j)
end do 
end do 
end do 
end do 

term(183) = term(183) * (-4.0d+0) 
term(184) = term(184) * (-8.0d+0) 
term(185) = term(185) * (2.0d+0) 
term(186) = term(186) * (-1.0d+0) 
term(187) = term(187) * (2.0d+0) 
term(188) = term(188) * (-1.0d+0) 
term(189) = term(189) * (-1.0d+0) 
term(190) = term(190) * (2.0d+0) 
term(191) = term(191) * (-1.0d+0) 
term(192) = term(192) * (2.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(193) = term(193) + r1(vrdav_Rl, a,i) * wm_interm_18_pt4(j,i) * wm_interm_31_pt4(a,j)
term(194) = term(194) + r1(vrdav_Rl, a,i) * wm_interm_19_pt4(j,i) * wm_interm_31_pt4(a,j)
term(195) = term(195) + r1(vrdav_Rl, a,i) * wm_interm_18_pt4(j,i) * wm_interm_34_pt4(a,j)
term(196) = term(196) + r1(vrdav_Rl, a,i) * wm_interm_19_pt4(j,i) * wm_interm_34_pt4(a,j)
term(197) = term(197) + r1(vrdav_Rl, a,i) * wm_interm_72_pt4(i,p,q,j) * wm_interm_8_pt4(a,j)
term(198) = term(198) + r1(vrdav_Rl, a,i) * wm_interm_72_pt4(i,p,j,q) * wm_interm_8_pt4(a,j)
term(199) = term(199) + r1(vrdav_Rl, a,i) * wm_interm_70_pt4(i,j) * wm_interm_8_pt4(a,j)
term(200) = term(200) + r1(vrdav_Rl, a,i) * wm_interm_71_pt4(i,j) * wm_interm_8_pt4(a,j)
term(201) = term(201) + s1(a,i) * wm_interm_15_pt4(a,j) * wm_interm_72_pt4(p,i,j,q)
term(202) = term(202) + s1(a,i) * wm_interm_15_pt4(a,j) * wm_interm_72_pt4(i,p,j,q)
term(203) = term(203) + s1(a,i) * wm_interm_15_pt4(a,j) * wm_interm_70_pt4(i,j)
term(204) = term(204) + s1(a,i) * wm_interm_15_pt4(a,j) * wm_interm_71_pt4(i,j)
term(205) = term(205) + s1(a,i) * wm_interm_15_pt4(a,j) * wm_interm_72_pt4(i,p,q,j)
term(206) = term(206) + s1(a,i) * wm_interm_15_pt4(a,j) * wm_interm_72_pt4(p,i,q,j)
term(207) = term(207) + r1(vrdav_Rl, a,i) * wm_interm_72_pt4(i,p,q,j) * wm_interm_7_pt4(a,j)
term(208) = term(208) + r1(vrdav_Rl, a,i) * wm_interm_72_pt4(i,p,j,q) * wm_interm_7_pt4(a,j)
term(209) = term(209) + r1(vrdav_Rl, a,i) * wm_interm_70_pt4(i,j) * wm_interm_7_pt4(a,j)
term(210) = term(210) + r1(vrdav_Rl, a,i) * wm_interm_71_pt4(i,j) * wm_interm_7_pt4(a,j)
term(211) = term(211) + s1(a,i) * wm_interm_17_pt4(a,j) * wm_interm_72_pt4(p,i,j,q)
term(212) = term(212) + s1(a,i) * wm_interm_17_pt4(a,j) * wm_interm_72_pt4(i,p,j,q)
term(213) = term(213) + s1(a,i) * wm_interm_17_pt4(a,j) * wm_interm_70_pt4(i,j)
term(214) = term(214) + s1(a,i) * wm_interm_17_pt4(a,j) * wm_interm_71_pt4(i,j)
term(215) = term(215) + s1(a,i) * wm_interm_17_pt4(a,j) * wm_interm_72_pt4(i,p,q,j)
term(216) = term(216) + s1(a,i) * wm_interm_17_pt4(a,j) * wm_interm_72_pt4(p,i,q,j)
term(217) = term(217) + s1(a,p) * wm_interm_14_pt4(a,i,q,j) * wm_interm_70_pt4(i,j)
term(218) = term(218) + s1(a,p) * wm_interm_14_pt4(a,i,q,j) * wm_interm_71_pt4(i,j)
term(219) = term(219) + s1(a,i) * wm_interm_14_pt4(a,p,q,j) * wm_interm_70_pt4(i,j)
term(220) = term(220) + s1(a,i) * wm_interm_14_pt4(a,p,q,j) * wm_interm_71_pt4(i,j)
term(221) = term(221) + s1(a,i) * wm_interm_14_pt4(a,p,j,q) * wm_interm_70_pt4(i,j)
term(222) = term(222) + s1(a,p) * wm_interm_14_pt4(a,i,j,q) * wm_interm_70_pt4(i,j)
term(223) = term(223) + s1(a,i) * wm_interm_14_pt4(a,p,j,q) * wm_interm_71_pt4(i,j)
term(224) = term(224) + s1(a,p) * wm_interm_14_pt4(a,i,j,q) * wm_interm_71_pt4(i,j)
term(225) = term(225) + t1(a,i) * wm_interm_13_pt4(a,p,j,q) * wm_interm_56_pt4(i,j)
term(226) = term(226) + t1(a,i) * wm_interm_4_pt4(a,j) * wm_interm_56_pt4(i,j)
term(227) = term(227) + t1(a,i) * wm_interm_13_pt4(a,p,q,j) * wm_interm_56_pt4(i,j)
term(228) = term(228) + t1(a,i) * wm_interm_2_pt4(a,j) * wm_interm_56_pt4(i,j)
term(229) = term(229) + t1(a,i) * wm_interm_13_pt4(a,p,j,q) * wm_interm_54_pt4(i,j)
term(230) = term(230) + t1(a,i) * wm_interm_4_pt4(a,j) * wm_interm_54_pt4(i,j)
term(231) = term(231) + t1(a,i) * wm_interm_13_pt4(a,p,q,j) * wm_interm_54_pt4(i,j)
term(232) = term(232) + t1(a,i) * wm_interm_2_pt4(a,j) * wm_interm_54_pt4(i,j)
term(233) = term(233) + t1(a,i) * wm_interm_18_pt4(i,j) * wm_interm_40_pt4(a,q,j,p)
term(234) = term(234) + t1(a,i) * wm_interm_19_pt4(i,j) * wm_interm_40_pt4(a,q,j,p)
term(235) = term(235) + t1(a,i) * wm_interm_18_pt4(i,j) * wm_interm_41_pt4(a,j)
term(236) = term(236) + t1(a,i) * wm_interm_19_pt4(i,j) * wm_interm_41_pt4(a,j)
term(237) = term(237) + t1(a,i) * wm_interm_18_pt4(i,j) * wm_interm_40_pt4(a,j,q,p)
term(238) = term(238) + t1(a,i) * wm_interm_18_pt4(i,j) * wm_interm_38_pt4(a,j)
term(239) = term(239) + r1(vrdav_Rr, a,i) * wm_interm_18_pt4(i,j) * wm_interm_44_pt4(a,j)
term(240) = term(240) + t1(a,i) * wm_interm_19_pt4(i,j) * wm_interm_40_pt4(a,j,q,p)
term(241) = term(241) + t1(a,i) * wm_interm_19_pt4(i,j) * wm_interm_38_pt4(a,j)
term(242) = term(242) + r1(vrdav_Rr, a,i) * wm_interm_19_pt4(i,j) * wm_interm_44_pt4(a,j)
term(243) = term(243) + r1(vrdav_Rr, a,i) * wm_interm_18_pt4(i,j) * wm_interm_45_pt4(a,j)
term(244) = term(244) + r1(vrdav_Rr, a,i) * wm_interm_19_pt4(i,j) * wm_interm_45_pt4(a,j)
term(245) = term(245) + t1(a,i) * wm_interm_18_pt4(i,j) * wm_interm_39_pt4(a,q,j,p)
term(246) = term(246) + t1(a,i) * wm_interm_19_pt4(i,j) * wm_interm_39_pt4(a,q,j,p)
term(247) = term(247) + t1(a,i) * wm_interm_18_pt4(i,j) * wm_interm_39_pt4(a,j,q,p)
term(248) = term(248) + t1(a,i) * wm_interm_19_pt4(i,j) * wm_interm_39_pt4(a,j,q,p)
term(249) = term(249) + t1(a,q) * wm_interm_13_pt4(a,i,p,j) * wm_interm_54_pt4(i,j)
term(250) = term(250) + t1(a,q) * wm_interm_13_pt4(a,i,j,p) * wm_interm_54_pt4(i,j)
term(251) = term(251) + t1(a,q) * wm_interm_13_pt4(a,i,p,j) * wm_interm_56_pt4(i,j)
term(252) = term(252) + t1(a,q) * wm_interm_13_pt4(a,i,j,p) * wm_interm_56_pt4(i,j)
term(253) = term(253) + t1(a,i) * wm_interm_2_pt4(a,j) * wm_interm_55_pt4(p,i,j,q)
term(254) = term(254) + t1(a,i) * wm_interm_4_pt4(a,j) * wm_interm_55_pt4(p,i,j,q)
term(255) = term(255) + t1(a,i) * wm_interm_4_pt4(a,j) * wm_interm_55_pt4(p,i,q,j)
term(256) = term(256) + t1(a,i) * wm_interm_2_pt4(a,j) * wm_interm_55_pt4(p,i,q,j)
term(257) = term(257) + t1(a,i) * wm_interm_10_pt4(p,i,j,q) * wm_interm_38_pt4(a,j)
term(258) = term(258) + t1(a,i) * wm_interm_10_pt4(i,p,j,q) * wm_interm_38_pt4(a,j)
term(259) = term(259) + t1(a,i) * wm_interm_10_pt4(i,p,j,q) * wm_interm_41_pt4(a,j)
term(260) = term(260) + t1(a,i) * wm_interm_10_pt4(p,i,j,q) * wm_interm_41_pt4(a,j)
term(261) = term(261) + t1(a,i) * wm_interm_4_pt4(a,j) * wm_interm_55_pt4(i,p,j,q)
term(262) = term(262) + t1(a,i) * wm_interm_2_pt4(a,j) * wm_interm_55_pt4(i,p,j,q)
term(263) = term(263) + t1(a,i) * wm_interm_2_pt4(a,j) * wm_interm_55_pt4(i,p,q,j)
term(264) = term(264) + t1(a,i) * wm_interm_4_pt4(a,j) * wm_interm_55_pt4(i,p,q,j)
term(265) = term(265) + r1(vrdav_Rr, a,i) * wm_interm_10_pt4(p,i,j,q) * wm_interm_44_pt4(a,j)
term(266) = term(266) + r1(vrdav_Rr, a,i) * wm_interm_10_pt4(i,p,j,q) * wm_interm_44_pt4(a,j)
term(267) = term(267) + r1(vrdav_Rr, a,i) * wm_interm_10_pt4(p,i,j,q) * wm_interm_45_pt4(a,j)
term(268) = term(268) + r1(vrdav_Rr, a,i) * wm_interm_10_pt4(i,p,j,q) * wm_interm_45_pt4(a,j)
end do 
end do 
end do 

term(193) = term(193) * (8.0d+0) 
term(194) = term(194) * (-16.0d+0) 
term(195) = term(195) * (-4.0d+0) 
term(196) = term(196) * (8.0d+0) 
term(197) = term(197) * (4.0d+0) 
term(198) = term(198) * (-8.0d+0) 
term(199) = term(199) * (-8.0d+0) 
term(200) = term(200) * (16.0d+0) 
term(201) = term(201) * (-4.0d+0) 
term(202) = term(202) * (8.0d+0) 
term(203) = term(203) * (16.0d+0) 
term(204) = term(204) * (-32.0d+0) 
term(205) = term(205) * (-4.0d+0) 
term(206) = term(206) * (8.0d+0) 
term(207) = term(207) * (-8.0d+0) 
term(208) = term(208) * (16.0d+0) 
term(209) = term(209) * (16.0d+0) 
term(210) = term(210) * (-32.0d+0) 
term(211) = term(211) * (2.0d+0) 
term(212) = term(212) * (-4.0d+0) 
term(213) = term(213) * (-8.0d+0) 
term(214) = term(214) * (16.0d+0) 
term(215) = term(215) * (2.0d+0) 
term(216) = term(216) * (-4.0d+0) 
term(217) = term(217) * (4.0d+0) 
term(218) = term(218) * (-8.0d+0) 
term(219) = term(219) * (-8.0d+0) 
term(220) = term(220) * (16.0d+0) 
term(221) = term(221) * (4.0d+0) 
term(222) = term(222) * (-8.0d+0) 
term(223) = term(223) * (-8.0d+0) 
term(224) = term(224) * (16.0d+0) 
term(225) = term(225) * (8.0d+0) 
term(226) = term(226) * (-16.0d+0) 
term(227) = term(227) * (-16.0d+0) 
term(228) = term(228) * (32.0d+0) 
term(229) = term(229) * (-4.0d+0) 
term(230) = term(230) * (8.0d+0) 
term(231) = term(231) * (8.0d+0) 
term(232) = term(232) * (-16.0d+0) 
term(233) = term(233) * (4.0d+0) 
term(234) = term(234) * (-8.0d+0) 
term(235) = term(235) * (-16.0d+0) 
term(236) = term(236) * (32.0d+0) 
term(237) = term(237) * (-2.0d+0) 
term(238) = term(238) * (8.0d+0) 
term(239) = term(239) * (-16.0d+0) 
term(240) = term(240) * (4.0d+0) 
term(241) = term(241) * (-16.0d+0) 
term(242) = term(242) * (32.0d+0) 
term(243) = term(243) * (8.0d+0) 
term(244) = term(244) * (-16.0d+0) 
term(245) = term(245) * (-2.0d+0) 
term(246) = term(246) * (4.0d+0) 
term(247) = term(247) * (4.0d+0) 
term(248) = term(248) * (-8.0d+0) 
term(249) = term(249) * (-2.0d+0) 
term(250) = term(250) * (4.0d+0) 
term(251) = term(251) * (4.0d+0) 
term(252) = term(252) * (-8.0d+0) 
term(253) = term(253) * (2.0d+0) 
term(254) = term(254) * (-1.0d+0) 
term(255) = term(255) * (2.0d+0) 
term(256) = term(256) * (-4.0d+0) 
term(257) = term(257) * (-2.0d+0) 
term(258) = term(258) * (4.0d+0) 
term(259) = term(259) * (-8.0d+0) 
term(260) = term(260) * (4.0d+0) 
term(261) = term(261) * (2.0d+0) 
term(262) = term(262) * (-4.0d+0) 
term(263) = term(263) * (2.0d+0) 
term(264) = term(264) * (-1.0d+0) 
term(265) = term(265) * (4.0d+0) 
term(266) = term(266) * (-8.0d+0) 
term(267) = term(267) * (-2.0d+0) 
term(268) = term(268) * (4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(269) = term(269) + s1(a,i) * wm_interm_10_pt4(j,q,k,i) * wm_interm_26_pt4(a,k,p,j)
term(270) = term(270) + s1(a,i) * wm_interm_10_pt4(q,j,k,i) * wm_interm_26_pt4(a,k,p,j)
term(271) = term(271) + s1(a,i) * wm_interm_10_pt4(j,k,q,i) * wm_interm_26_pt4(a,p,k,j)
end do 
end do 
end do 
end do 

term(269) = term(269) * (4.0d+0) 
term(270) = term(270) * (-8.0d+0) 
term(271) = term(271) * (2.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(272) = term(272) + r1(vrdav_Rr, a,i) * wm_interm_11_pt4(a,b,j,k) * wm_interm_42_pt4(b,i,k,j)
term(273) = term(273) + r1(vrdav_Rr, a,i) * wm_interm_12_pt4(a,b,j,k) * wm_interm_42_pt4(b,i,k,j)
term(274) = term(274) + r1(vrdav_Rr, a,i) * wm_interm_1_pt4(a,b,j,k) * wm_interm_42_pt4(b,i,k,j)
term(275) = term(275) + r1(vrdav_Rr, a,i) * wm_interm_42_pt4(b,i,j,k) * wm_interm_5_pt4(a,b,k,j)
term(276) = term(276) + t1(a,i) * wm_interm_11_pt4(a,b,j,k) * wm_interm_40_pt4(b,i,k,j)
term(277) = term(277) + t1(a,i) * wm_interm_12_pt4(a,b,j,k) * wm_interm_40_pt4(b,i,k,j)
term(278) = term(278) + t1(a,i) * wm_interm_11_pt4(a,b,j,k) * wm_interm_39_pt4(b,i,k,j)
term(279) = term(279) + t1(a,i) * wm_interm_12_pt4(a,b,j,k) * wm_interm_39_pt4(b,i,k,j)
term(280) = term(280) + t1(a,i) * wm_interm_1_pt4(a,b,j,k) * wm_interm_39_pt4(b,i,k,j)
term(281) = term(281) + t1(a,i) * wm_interm_39_pt4(b,i,j,k) * wm_interm_5_pt4(a,b,k,j)
term(282) = term(282) + t1(a,i) * wm_interm_40_pt4(b,i,j,k) * wm_interm_5_pt4(a,b,k,j)
term(283) = term(283) + t1(a,i) * wm_interm_1_pt4(a,b,j,k) * wm_interm_40_pt4(b,i,k,j)
term(284) = term(284) + r1(vrdav_Rr, a,i) * wm_interm_11_pt4(a,b,j,k) * wm_interm_43_pt4(b,i,k,j)
term(285) = term(285) + r1(vrdav_Rr, a,i) * wm_interm_12_pt4(a,b,j,k) * wm_interm_43_pt4(b,i,k,j)
term(286) = term(286) + r1(vrdav_Rr, a,i) * wm_interm_43_pt4(b,i,j,k) * wm_interm_5_pt4(a,b,k,j)
term(287) = term(287) + r1(vrdav_Rr, a,i) * wm_interm_1_pt4(a,b,j,k) * wm_interm_43_pt4(b,i,k,j)
end do 
end do 
end do 
end do 
end do 

term(272) = term(272) * (4.0d+0) 
term(273) = term(273) * (-8.0d+0) 
term(274) = term(274) * (4.0d+0) 
term(275) = term(275) * (-8.0d+0) 
term(276) = term(276) * (-8.0d+0) 
term(277) = term(277) * (16.0d+0) 
term(278) = term(278) * (4.0d+0) 
term(279) = term(279) * (-8.0d+0) 
term(280) = term(280) * (4.0d+0) 
term(281) = term(281) * (-8.0d+0) 
term(282) = term(282) * (4.0d+0) 
term(283) = term(283) * (-8.0d+0) 
term(284) = term(284) * (-8.0d+0) 
term(285) = term(285) * (16.0d+0) 
term(286) = term(286) * (4.0d+0) 
term(287) = term(287) * (-8.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(288) = term(288) + s1(a,i) * wm_interm_10_pt4(j,q,k,i) * wm_interm_26_pt4(a,k,j,p)
term(289) = term(289) + s1(a,i) * wm_interm_10_pt4(q,j,k,i) * wm_interm_26_pt4(a,k,j,p)
term(290) = term(290) + s1(a,p) * wm_interm_10_pt4(i,j,k,q) * wm_interm_26_pt4(a,k,j,i)
end do 
end do 
end do 
end do 

term(288) = term(288) * (-8.0d+0) 
term(289) = term(289) * (4.0d+0) 
term(290) = term(290) * (4.0d+0) 

do i = 1, nocc 
term(291) = term(291) + wm_interm_27_pt4(q,i) * wm_interm_66_pt4(i,p)
term(292) = term(292) + wm_interm_27_pt4(q,i) * wm_interm_67_pt4(i,p)
term(293) = term(293) + wm_interm_27_pt4(i,q) * wm_interm_66_pt4(p,i)
term(294) = term(294) + wm_interm_27_pt4(i,q) * wm_interm_67_pt4(p,i)
term(295) = term(295) + wm_interm_27_pt4(q,i) * wm_interm_6_pt4(i,p)
term(296) = term(296) + wm_interm_22_pt4(q,i) * wm_interm_9_pt4(p,i)
term(297) = term(297) + wm_interm_27_pt4(i,q) * wm_interm_6_pt4(p,i)
term(298) = term(298) + wm_interm_22_pt4(i,q) * wm_interm_9_pt4(i,p)
end do 

term(291) = term(291) * (-2.0d+0) 
term(292) = term(292) * (4.0d+0) 
term(293) = term(293) * (-2.0d+0) 
term(294) = term(294) * (4.0d+0) 
term(295) = term(295) * (2.0d+0) 
term(296) = term(296) * (2.0d+0) 
term(297) = term(297) * (2.0d+0) 
term(298) = term(298) * (2.0d+0) 

do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(299) = term(299) + t1(b,q) * wm_interm_20_pt4(b,a) * wm_interm_38_pt4(a,p)
term(300) = term(300) + t1(b,q) * wm_interm_21_pt4(b,a) * wm_interm_38_pt4(a,p)
term(301) = term(301) + t1(b,q) * wm_interm_20_pt4(b,a) * wm_interm_41_pt4(a,p)
term(302) = term(302) + t1(b,q) * wm_interm_21_pt4(b,a) * wm_interm_41_pt4(a,p)
term(303) = term(303) + t1(b,q) * wm_interm_4_pt4(a,p) * wm_interm_51_pt4(b,a)
term(304) = term(304) + t1(b,q) * wm_interm_2_pt4(a,p) * wm_interm_51_pt4(b,a)
term(305) = term(305) + t1(b,q) * wm_interm_4_pt4(a,p) * wm_interm_52_pt4(b,a)
term(306) = term(306) + t1(b,q) * wm_interm_2_pt4(a,p) * wm_interm_52_pt4(b,a)
end do 
end do 

term(299) = term(299) * (-2.0d+0) 
term(300) = term(300) * (4.0d+0) 
term(301) = term(301) * (4.0d+0) 
term(302) = term(302) * (-8.0d+0) 
term(303) = term(303) * (4.0d+0) 
term(304) = term(304) * (-8.0d+0) 
term(305) = term(305) * (-2.0d+0) 
term(306) = term(306) * (4.0d+0) 

do l = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(307) = term(307) + t1(a,i) * wm_interm_10_pt4(i,j,k,l) * wm_interm_40_pt4(a,k,l,j)
term(308) = term(308) + t1(a,i) * wm_interm_10_pt4(j,i,k,l) * wm_interm_40_pt4(a,k,l,j)
term(309) = term(309) + t1(a,i) * wm_interm_10_pt4(j,i,k,l) * wm_interm_39_pt4(a,k,l,j)
term(310) = term(310) + t1(a,i) * wm_interm_10_pt4(i,j,k,l) * wm_interm_39_pt4(a,k,l,j)
end do 
end do 
end do 
end do 
end do 

term(307) = term(307) * (4.0d+0) 
term(308) = term(308) * (-8.0d+0) 
term(309) = term(309) * (4.0d+0) 
term(310) = term(310) * (-8.0d+0) 

do j = 1, nocc 
do a = nocc + 1, nactive 
do i = 1, nocc 
do b = nocc + 1, nactive 
term(311) = term(311) + r1(vrdav_Rl, a,i) * wm_interm_1_pt4(b,a,q,j) * wm_interm_28_pt4(b,i,p,j)
term(312) = term(312) + r1(vrdav_Rl, a,i) * wm_interm_1_pt4(b,a,q,j) * wm_interm_29_pt4(b,i,p,j)
term(313) = term(313) + r1(vrdav_Rl, a,i) * wm_interm_29_pt4(b,i,p,j) * wm_interm_5_pt4(b,a,q,j)
term(314) = term(314) + r1(vrdav_Rl, a,i) * wm_interm_28_pt4(b,i,p,j) * wm_interm_5_pt4(b,a,q,j)
term(315) = term(315) + r1(vrdav_Rl, a,i) * wm_interm_11_pt4(b,a,q,j) * wm_interm_28_pt4(b,i,p,j)
term(316) = term(316) + r1(vrdav_Rl, a,i) * wm_interm_11_pt4(b,a,q,j) * wm_interm_29_pt4(b,i,p,j)
term(317) = term(317) + r1(vrdav_Rl, a,i) * wm_interm_12_pt4(b,a,q,j) * wm_interm_28_pt4(b,i,p,j)
term(318) = term(318) + r1(vrdav_Rl, a,i) * wm_interm_12_pt4(b,a,q,j) * wm_interm_29_pt4(b,i,p,j)
end do 
end do 
end do 
end do 

term(311) = term(311) * (4.0d+0) 
term(312) = term(312) * (-8.0d+0) 
term(313) = term(313) * (4.0d+0) 
term(314) = term(314) * (-8.0d+0) 
term(315) = term(315) * (4.0d+0) 
term(316) = term(316) * (-8.0d+0) 
term(317) = term(317) * (-8.0d+0) 
term(318) = term(318) * (16.0d+0) 

do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(319) = term(319) + s2(a,b,i,q) * wm_interm_15_pt4(b,i) * wm_interm_31_pt4(a,p)
term(320) = term(320) + s2(a,b,i,q) * wm_interm_15_pt4(b,i) * wm_interm_34_pt4(a,p)
term(321) = term(321) + s2(a,b,i,q) * wm_interm_17_pt4(b,i) * wm_interm_31_pt4(a,p)
term(322) = term(322) + s2(a,b,i,q) * wm_interm_17_pt4(b,i) * wm_interm_34_pt4(a,p)
term(323) = term(323) + r1(vrdav_Rl, a,i) * wm_interm_59_pt4(a,b,p,q) * wm_interm_7_pt4(b,i)
term(324) = term(324) + r1(vrdav_Rl, a,i) * wm_interm_60_pt4(a,b,p,q) * wm_interm_7_pt4(b,i)
term(325) = term(325) + r1(vrdav_Rl, a,i) * wm_interm_61_pt4(a,b,p,q) * wm_interm_7_pt4(b,i)
term(326) = term(326) + r1(vrdav_Rl, a,i) * wm_interm_62_pt4(a,b,p,q) * wm_interm_7_pt4(b,i)
term(327) = term(327) + r1(vrdav_Rl, a,i) * wm_interm_68_pt4(a,b) * wm_interm_7_pt4(b,i)
term(328) = term(328) + r1(vrdav_Rl, a,i) * wm_interm_69_pt4(a,b) * wm_interm_7_pt4(b,i)
term(329) = term(329) + r1(vrdav_Rl, a,i) * wm_interm_59_pt4(a,b,p,q) * wm_interm_8_pt4(b,i)
term(330) = term(330) + r1(vrdav_Rl, a,i) * wm_interm_60_pt4(a,b,p,q) * wm_interm_8_pt4(b,i)
term(331) = term(331) + r1(vrdav_Rl, a,i) * wm_interm_61_pt4(a,b,p,q) * wm_interm_8_pt4(b,i)
term(332) = term(332) + r1(vrdav_Rl, a,i) * wm_interm_62_pt4(a,b,p,q) * wm_interm_8_pt4(b,i)
term(333) = term(333) + r1(vrdav_Rl, a,i) * wm_interm_68_pt4(a,b) * wm_interm_8_pt4(b,i)
term(334) = term(334) + r1(vrdav_Rl, a,i) * wm_interm_69_pt4(a,b) * wm_interm_8_pt4(b,i)
term(335) = term(335) + s1(a,i) * wm_interm_15_pt4(b,i) * wm_interm_59_pt4(a,b,p,q)
term(336) = term(336) + s1(a,i) * wm_interm_15_pt4(b,i) * wm_interm_60_pt4(a,b,p,q)
term(337) = term(337) + s1(a,i) * wm_interm_15_pt4(b,i) * wm_interm_61_pt4(a,b,p,q)
term(338) = term(338) + s1(a,i) * wm_interm_15_pt4(b,i) * wm_interm_62_pt4(a,b,p,q)
term(339) = term(339) + s1(a,i) * wm_interm_15_pt4(b,i) * wm_interm_68_pt4(a,b)
term(340) = term(340) + s1(a,i) * wm_interm_15_pt4(b,i) * wm_interm_69_pt4(a,b)
term(341) = term(341) + s1(a,i) * wm_interm_17_pt4(b,i) * wm_interm_59_pt4(a,b,p,q)
term(342) = term(342) + s1(a,i) * wm_interm_17_pt4(b,i) * wm_interm_60_pt4(a,b,p,q)
term(343) = term(343) + s1(a,i) * wm_interm_17_pt4(b,i) * wm_interm_61_pt4(a,b,p,q)
term(344) = term(344) + s1(a,i) * wm_interm_17_pt4(b,i) * wm_interm_62_pt4(a,b,p,q)
term(345) = term(345) + s1(a,i) * wm_interm_17_pt4(b,i) * wm_interm_68_pt4(a,b)
term(346) = term(346) + s1(a,i) * wm_interm_17_pt4(b,i) * wm_interm_69_pt4(a,b)
term(347) = term(347) + s2(a,b,i,p) * wm_interm_17_pt4(a,q) * wm_interm_31_pt4(b,i)
term(348) = term(348) + s2(a,b,i,p) * wm_interm_17_pt4(a,q) * wm_interm_34_pt4(b,i)
term(349) = term(349) + s2(a,b,i,p) * wm_interm_15_pt4(a,q) * wm_interm_31_pt4(b,i)
term(350) = term(350) + s2(a,b,i,p) * wm_interm_15_pt4(a,q) * wm_interm_34_pt4(b,i)
term(351) = term(351) + s2(a,b,i,p) * wm_interm_35_pt4(b,i) * wm_interm_7_pt4(a,q)
term(352) = term(352) + s2(a,b,i,p) * wm_interm_32_pt4(b,i) * wm_interm_7_pt4(a,q)
term(353) = term(353) + s2(a,b,i,p) * wm_interm_35_pt4(b,i) * wm_interm_8_pt4(a,q)
term(354) = term(354) + s2(a,b,i,p) * wm_interm_32_pt4(b,i) * wm_interm_8_pt4(a,q)
term(355) = term(355) + t1(a,i) * wm_interm_20_pt4(a,b) * wm_interm_41_pt4(b,i)
term(356) = term(356) + t1(a,i) * wm_interm_21_pt4(a,b) * wm_interm_41_pt4(b,i)
term(357) = term(357) + t1(a,i) * wm_interm_20_pt4(a,b) * wm_interm_38_pt4(b,i)
term(358) = term(358) + t1(a,i) * wm_interm_21_pt4(a,b) * wm_interm_38_pt4(b,i)
term(359) = term(359) + r1(vrdav_Rr, a,i) * wm_interm_20_pt4(a,b) * wm_interm_44_pt4(b,i)
term(360) = term(360) + r1(vrdav_Rr, a,i) * wm_interm_21_pt4(a,b) * wm_interm_44_pt4(b,i)
term(361) = term(361) + t1(a,i) * wm_interm_4_pt4(b,i) * wm_interm_51_pt4(a,b)
term(362) = term(362) + t1(a,i) * wm_interm_2_pt4(b,i) * wm_interm_51_pt4(a,b)
term(363) = term(363) + t1(a,i) * wm_interm_2_pt4(b,i) * wm_interm_52_pt4(a,b)
term(364) = term(364) + t1(a,i) * wm_interm_4_pt4(b,i) * wm_interm_52_pt4(a,b)
term(365) = term(365) + t1(a,i) * wm_interm_20_pt4(a,b) * wm_interm_40_pt4(b,i,q,p)
term(366) = term(366) + t1(a,i) * wm_interm_21_pt4(a,b) * wm_interm_40_pt4(b,i,q,p)
term(367) = term(367) + r1(vrdav_Rr, a,i) * wm_interm_20_pt4(a,b) * wm_interm_45_pt4(b,i)
term(368) = term(368) + r1(vrdav_Rr, a,i) * wm_interm_21_pt4(a,b) * wm_interm_45_pt4(b,i)
term(369) = term(369) + t1(a,i) * wm_interm_20_pt4(a,b) * wm_interm_39_pt4(b,i,q,p)
term(370) = term(370) + t1(a,i) * wm_interm_21_pt4(a,b) * wm_interm_39_pt4(b,i,q,p)
term(371) = term(371) + r1(vrdav_Rr, a,i) * wm_interm_20_pt4(a,b) * wm_interm_43_pt4(b,i,p,q)
term(372) = term(372) + r1(vrdav_Rr, a,i) * wm_interm_21_pt4(a,b) * wm_interm_43_pt4(b,i,p,q)
term(373) = term(373) + r1(vrdav_Rr, a,i) * wm_interm_20_pt4(a,b) * wm_interm_42_pt4(b,i,p,q)
term(374) = term(374) + r1(vrdav_Rr, a,i) * wm_interm_21_pt4(a,b) * wm_interm_42_pt4(b,i,p,q)
term(375) = term(375) + r1(vrdav_Rr, a,i) * wm_interm_23_pt4(b,i,p,q) * wm_interm_51_pt4(a,b)
term(376) = term(376) + r1(vrdav_Rr, a,i) * wm_interm_23_pt4(b,i,p,q) * wm_interm_52_pt4(a,b)
term(377) = term(377) + t1(a,i) * wm_interm_38_pt4(b,i) * wm_interm_5_pt4(a,b,p,q)
term(378) = term(378) + t1(a,i) * wm_interm_1_pt4(a,b,p,q) * wm_interm_38_pt4(b,i)
term(379) = term(379) + t1(a,i) * wm_interm_11_pt4(a,b,p,q) * wm_interm_38_pt4(b,i)
term(380) = term(380) + t1(a,i) * wm_interm_12_pt4(a,b,p,q) * wm_interm_38_pt4(b,i)
term(381) = term(381) + t1(a,i) * wm_interm_1_pt4(a,b,p,q) * wm_interm_41_pt4(b,i)
term(382) = term(382) + t1(a,i) * wm_interm_41_pt4(b,i) * wm_interm_5_pt4(a,b,p,q)
term(383) = term(383) + t1(a,i) * wm_interm_11_pt4(a,b,p,q) * wm_interm_41_pt4(b,i)
term(384) = term(384) + t1(a,i) * wm_interm_12_pt4(a,b,p,q) * wm_interm_41_pt4(b,i)
term(385) = term(385) + t1(a,i) * wm_interm_2_pt4(b,i) * wm_interm_49_pt4(a,b,p,q)
term(386) = term(386) + t1(a,i) * wm_interm_49_pt4(a,b,p,q) * wm_interm_4_pt4(b,i)
term(387) = term(387) + t1(a,i) * wm_interm_2_pt4(b,i) * wm_interm_48_pt4(a,b,p,q)
term(388) = term(388) + t1(a,i) * wm_interm_48_pt4(a,b,p,q) * wm_interm_4_pt4(b,i)
term(389) = term(389) + t1(a,i) * wm_interm_2_pt4(b,i) * wm_interm_53_pt4(a,b,p,q)
term(390) = term(390) + t1(a,i) * wm_interm_4_pt4(b,i) * wm_interm_53_pt4(a,b,p,q)
term(391) = term(391) + t1(a,i) * wm_interm_2_pt4(b,i) * wm_interm_50_pt4(a,b,p,q)
term(392) = term(392) + t1(a,i) * wm_interm_4_pt4(b,i) * wm_interm_50_pt4(a,b,p,q)
term(393) = term(393) + r1(vrdav_Rr, a,i) * wm_interm_11_pt4(a,b,p,q) * wm_interm_44_pt4(b,i)
term(394) = term(394) + t2(a,b,i,q) * wm_interm_44_pt4(b,i) * wm_interm_4_pt4(a,p)
term(395) = term(395) + r1(vrdav_Rr, a,i) * wm_interm_12_pt4(a,b,p,q) * wm_interm_44_pt4(b,i)
term(396) = term(396) + t2(a,b,i,q) * wm_interm_2_pt4(a,p) * wm_interm_44_pt4(b,i)
term(397) = term(397) + r1(vrdav_Rr, a,i) * wm_interm_11_pt4(a,b,p,q) * wm_interm_45_pt4(b,i)
term(398) = term(398) + t2(a,b,i,q) * wm_interm_45_pt4(b,i) * wm_interm_4_pt4(a,p)
term(399) = term(399) + r1(vrdav_Rr, a,i) * wm_interm_12_pt4(a,b,p,q) * wm_interm_45_pt4(b,i)
term(400) = term(400) + t2(a,b,i,q) * wm_interm_2_pt4(a,p) * wm_interm_45_pt4(b,i)
term(401) = term(401) + r1(vrdav_Rr, a,i) * wm_interm_1_pt4(a,b,p,q) * wm_interm_44_pt4(b,i)
term(402) = term(402) + t2(a,b,i,q) * wm_interm_2_pt4(b,i) * wm_interm_44_pt4(a,p)
term(403) = term(403) + t2(a,b,i,q) * wm_interm_44_pt4(a,p) * wm_interm_4_pt4(b,i)
term(404) = term(404) + r1(vrdav_Rr, a,i) * wm_interm_44_pt4(b,i) * wm_interm_5_pt4(a,b,p,q)
term(405) = term(405) + r1(vrdav_Rr, a,i) * wm_interm_1_pt4(a,b,p,q) * wm_interm_45_pt4(b,i)
term(406) = term(406) + t2(a,b,i,q) * wm_interm_2_pt4(b,i) * wm_interm_45_pt4(a,p)
term(407) = term(407) + t2(a,b,i,q) * wm_interm_45_pt4(a,p) * wm_interm_4_pt4(b,i)
term(408) = term(408) + r1(vrdav_Rr, a,i) * wm_interm_45_pt4(b,i) * wm_interm_5_pt4(a,b,p,q)
end do 
end do 
end do 

term(319) = term(319) * (8.0d+0) 
term(320) = term(320) * (-4.0d+0) 
term(321) = term(321) * (-4.0d+0) 
term(322) = term(322) * (2.0d+0) 
term(323) = term(323) * (-8.0d+0) 
term(324) = term(324) * (16.0d+0) 
term(325) = term(325) * (-8.0d+0) 
term(326) = term(326) * (16.0d+0) 
term(327) = term(327) * (16.0d+0) 
term(328) = term(328) * (-32.0d+0) 
term(329) = term(329) * (4.0d+0) 
term(330) = term(330) * (-8.0d+0) 
term(331) = term(331) * (4.0d+0) 
term(332) = term(332) * (-8.0d+0) 
term(333) = term(333) * (-8.0d+0) 
term(334) = term(334) * (16.0d+0) 
term(335) = term(335) * (-8.0d+0) 
term(336) = term(336) * (16.0d+0) 
term(337) = term(337) * (-8.0d+0) 
term(338) = term(338) * (16.0d+0) 
term(339) = term(339) * (16.0d+0) 
term(340) = term(340) * (-32.0d+0) 
term(341) = term(341) * (4.0d+0) 
term(342) = term(342) * (-8.0d+0) 
term(343) = term(343) * (4.0d+0) 
term(344) = term(344) * (-8.0d+0) 
term(345) = term(345) * (-8.0d+0) 
term(346) = term(346) * (16.0d+0) 
term(347) = term(347) * (-4.0d+0) 
term(348) = term(348) * (2.0d+0) 
term(349) = term(349) * (8.0d+0) 
term(350) = term(350) * (-4.0d+0) 
term(351) = term(351) * (-8.0d+0) 
term(352) = term(352) * (16.0d+0) 
term(353) = term(353) * (4.0d+0) 
term(354) = term(354) * (-8.0d+0) 
term(355) = term(355) * (-16.0d+0) 
term(356) = term(356) * (32.0d+0) 
term(357) = term(357) * (8.0d+0) 
term(358) = term(358) * (-16.0d+0) 
term(359) = term(359) * (-16.0d+0) 
term(360) = term(360) * (32.0d+0) 
term(361) = term(361) * (-16.0d+0) 
term(362) = term(362) * (32.0d+0) 
term(363) = term(363) * (-16.0d+0) 
term(364) = term(364) * (8.0d+0) 
term(365) = term(365) * (-2.0d+0) 
term(366) = term(366) * (4.0d+0) 
term(367) = term(367) * (8.0d+0) 
term(368) = term(368) * (-16.0d+0) 
term(369) = term(369) * (4.0d+0) 
term(370) = term(370) * (-8.0d+0) 
term(371) = term(371) * (-1.0d+0) 
term(372) = term(372) * (2.0d+0) 
term(373) = term(373) * (2.0d+0) 
term(374) = term(374) * (-4.0d+0) 
term(375) = term(375) * (-8.0d+0) 
term(376) = term(376) * (4.0d+0) 
term(377) = term(377) * (4.0d+0) 
term(378) = term(378) * (-2.0d+0) 
term(379) = term(379) * (-2.0d+0) 
term(380) = term(380) * (4.0d+0) 
term(381) = term(381) * (4.0d+0) 
term(382) = term(382) * (-8.0d+0) 
term(383) = term(383) * (4.0d+0) 
term(384) = term(384) * (-8.0d+0) 
term(385) = term(385) * (4.0d+0) 
term(386) = term(386) * (-2.0d+0) 
term(387) = term(387) * (-8.0d+0) 
term(388) = term(388) * (4.0d+0) 
term(389) = term(389) * (4.0d+0) 
term(390) = term(390) * (-2.0d+0) 
term(391) = term(391) * (-8.0d+0) 
term(392) = term(392) * (4.0d+0) 
term(393) = term(393) * (4.0d+0) 
term(394) = term(394) * (4.0d+0) 
term(395) = term(395) * (-8.0d+0) 
term(396) = term(396) * (-8.0d+0) 
term(397) = term(397) * (-2.0d+0) 
term(398) = term(398) * (-2.0d+0) 
term(399) = term(399) * (4.0d+0) 
term(400) = term(400) * (4.0d+0) 
term(401) = term(401) * (4.0d+0) 
term(402) = term(402) * (-8.0d+0) 
term(403) = term(403) * (4.0d+0) 
term(404) = term(404) * (-8.0d+0) 
term(405) = term(405) * (-2.0d+0) 
term(406) = term(406) * (4.0d+0) 
term(407) = term(407) * (-2.0d+0) 
term(408) = term(408) * (4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(409) = term(409) + s1(a,i) * wm_interm_14_pt4(b,q,j,i) * wm_interm_59_pt4(a,b,p,j)
term(410) = term(410) + s1(a,i) * wm_interm_14_pt4(b,q,j,i) * wm_interm_60_pt4(a,b,p,j)
term(411) = term(411) + s1(a,i) * wm_interm_14_pt4(b,q,j,i) * wm_interm_61_pt4(a,b,p,j)
term(412) = term(412) + s1(a,i) * wm_interm_14_pt4(b,q,j,i) * wm_interm_62_pt4(a,b,p,j)
term(413) = term(413) + s1(a,i) * wm_interm_14_pt4(b,q,i,j) * wm_interm_62_pt4(a,b,p,j)
term(414) = term(414) + s1(a,i) * wm_interm_14_pt4(b,q,i,j) * wm_interm_61_pt4(a,b,p,j)
term(415) = term(415) + s1(a,i) * wm_interm_14_pt4(b,q,i,j) * wm_interm_59_pt4(a,b,p,j)
term(416) = term(416) + s1(a,i) * wm_interm_14_pt4(b,q,i,j) * wm_interm_60_pt4(a,b,p,j)
term(417) = term(417) + r1(vrdav_Rl, a,i) * wm_interm_3_pt4(b,j,i,q) * wm_interm_62_pt4(a,b,j,p)
term(418) = term(418) + r1(vrdav_Rl, a,i) * wm_interm_3_pt4(b,j,i,q) * wm_interm_61_pt4(a,b,j,p)
term(419) = term(419) + r1(vrdav_Rl, a,i) * wm_interm_3_pt4(b,j,i,q) * wm_interm_59_pt4(a,b,j,p)
term(420) = term(420) + r1(vrdav_Rl, a,i) * wm_interm_3_pt4(b,j,i,q) * wm_interm_60_pt4(a,b,j,p)
term(421) = term(421) + s1(a,i) * wm_interm_14_pt4(b,j,i,q) * wm_interm_62_pt4(a,b,j,p)
term(422) = term(422) + s1(a,i) * wm_interm_14_pt4(b,j,i,q) * wm_interm_61_pt4(a,b,j,p)
term(423) = term(423) + s1(a,i) * wm_interm_14_pt4(b,j,i,q) * wm_interm_59_pt4(a,b,j,p)
term(424) = term(424) + s1(a,i) * wm_interm_14_pt4(b,j,i,q) * wm_interm_60_pt4(a,b,j,p)
term(425) = term(425) + s1(a,p) * wm_interm_14_pt4(b,i,q,j) * wm_interm_62_pt4(a,b,i,j)
term(426) = term(426) + s1(a,p) * wm_interm_14_pt4(b,i,q,j) * wm_interm_61_pt4(a,b,i,j)
term(427) = term(427) + s1(a,p) * wm_interm_14_pt4(b,i,q,j) * wm_interm_59_pt4(a,b,i,j)
term(428) = term(428) + s1(a,p) * wm_interm_14_pt4(b,i,q,j) * wm_interm_60_pt4(a,b,i,j)
term(429) = term(429) + s1(a,p) * wm_interm_14_pt4(b,i,j,q) * wm_interm_59_pt4(a,b,i,j)
term(430) = term(430) + s1(a,p) * wm_interm_14_pt4(b,i,j,q) * wm_interm_60_pt4(a,b,i,j)
term(431) = term(431) + s1(a,p) * wm_interm_14_pt4(b,i,j,q) * wm_interm_61_pt4(a,b,i,j)
term(432) = term(432) + s1(a,p) * wm_interm_14_pt4(b,i,j,q) * wm_interm_62_pt4(a,b,i,j)
term(433) = term(433) + t1(a,i) * wm_interm_11_pt4(a,b,q,j) * wm_interm_40_pt4(b,j,i,p)
term(434) = term(434) + t1(a,i) * wm_interm_12_pt4(a,b,q,j) * wm_interm_40_pt4(b,j,i,p)
term(435) = term(435) + t1(a,i) * wm_interm_11_pt4(a,b,q,j) * wm_interm_39_pt4(b,j,i,p)
term(436) = term(436) + t1(a,i) * wm_interm_12_pt4(a,b,q,j) * wm_interm_39_pt4(b,j,i,p)
term(437) = term(437) + t1(a,i) * wm_interm_1_pt4(a,b,q,j) * wm_interm_40_pt4(b,j,i,p)
term(438) = term(438) + t1(a,i) * wm_interm_40_pt4(b,j,i,p) * wm_interm_5_pt4(a,b,q,j)
term(439) = term(439) + t1(a,i) * wm_interm_39_pt4(b,j,i,p) * wm_interm_5_pt4(a,b,q,j)
term(440) = term(440) + t1(a,i) * wm_interm_1_pt4(a,b,q,j) * wm_interm_39_pt4(b,j,i,p)
term(441) = term(441) + r1(vrdav_Rr, a,p) * wm_interm_11_pt4(a,b,i,j) * wm_interm_42_pt4(b,q,j,i)
term(442) = term(442) + r1(vrdav_Rr, a,p) * wm_interm_11_pt4(a,b,i,j) * wm_interm_42_pt4(b,j,q,i)
term(443) = term(443) + r1(vrdav_Rr, a,p) * wm_interm_12_pt4(a,b,i,j) * wm_interm_42_pt4(b,q,j,i)
term(444) = term(444) + r1(vrdav_Rr, a,p) * wm_interm_12_pt4(a,b,i,j) * wm_interm_42_pt4(b,j,q,i)
term(445) = term(445) + r1(vrdav_Rr, a,p) * wm_interm_1_pt4(a,b,i,j) * wm_interm_42_pt4(b,q,j,i)
term(446) = term(446) + r1(vrdav_Rr, a,p) * wm_interm_42_pt4(b,q,i,j) * wm_interm_5_pt4(a,b,j,i)
term(447) = term(447) + r1(vrdav_Rr, a,p) * wm_interm_42_pt4(b,i,q,j) * wm_interm_5_pt4(a,b,j,i)
term(448) = term(448) + r1(vrdav_Rr, a,p) * wm_interm_1_pt4(a,b,i,j) * wm_interm_42_pt4(b,j,q,i)
term(449) = term(449) + r1(vrdav_Rr, a,p) * wm_interm_11_pt4(a,b,i,j) * wm_interm_43_pt4(b,q,j,i)
term(450) = term(450) + r1(vrdav_Rr, a,p) * wm_interm_12_pt4(a,b,i,j) * wm_interm_43_pt4(b,q,j,i)
term(451) = term(451) + r1(vrdav_Rr, a,p) * wm_interm_11_pt4(a,b,i,j) * wm_interm_43_pt4(b,j,q,i)
term(452) = term(452) + r1(vrdav_Rr, a,p) * wm_interm_12_pt4(a,b,i,j) * wm_interm_43_pt4(b,j,q,i)
term(453) = term(453) + r1(vrdav_Rr, a,p) * wm_interm_43_pt4(b,q,i,j) * wm_interm_5_pt4(a,b,j,i)
term(454) = term(454) + r1(vrdav_Rr, a,p) * wm_interm_1_pt4(a,b,i,j) * wm_interm_43_pt4(b,q,j,i)
term(455) = term(455) + r1(vrdav_Rr, a,p) * wm_interm_1_pt4(a,b,i,j) * wm_interm_43_pt4(b,j,q,i)
term(456) = term(456) + r1(vrdav_Rr, a,p) * wm_interm_43_pt4(b,i,q,j) * wm_interm_5_pt4(a,b,j,i)
term(457) = term(457) + t1(a,q) * wm_interm_11_pt4(a,b,i,j) * wm_interm_40_pt4(b,p,j,i)
term(458) = term(458) + t1(a,q) * wm_interm_12_pt4(a,b,i,j) * wm_interm_40_pt4(b,p,j,i)
term(459) = term(459) + r1(vrdav_Rr, a,i) * wm_interm_11_pt4(a,b,p,j) * wm_interm_43_pt4(b,j,i,q)
term(460) = term(460) + r1(vrdav_Rr, a,i) * wm_interm_12_pt4(a,b,p,j) * wm_interm_43_pt4(b,j,i,q)
term(461) = term(461) + t1(a,q) * wm_interm_11_pt4(a,b,i,j) * wm_interm_40_pt4(b,j,p,i)
term(462) = term(462) + t1(a,q) * wm_interm_12_pt4(a,b,i,j) * wm_interm_40_pt4(b,j,p,i)
term(463) = term(463) + t1(a,q) * wm_interm_11_pt4(a,b,i,j) * wm_interm_39_pt4(b,p,j,i)
term(464) = term(464) + t1(a,q) * wm_interm_12_pt4(a,b,i,j) * wm_interm_39_pt4(b,p,j,i)
term(465) = term(465) + r1(vrdav_Rr, a,i) * wm_interm_11_pt4(a,b,p,j) * wm_interm_42_pt4(b,j,i,q)
term(466) = term(466) + r1(vrdav_Rr, a,i) * wm_interm_12_pt4(a,b,p,j) * wm_interm_42_pt4(b,j,i,q)
term(467) = term(467) + t1(a,q) * wm_interm_11_pt4(a,b,i,j) * wm_interm_39_pt4(b,j,p,i)
term(468) = term(468) + t1(a,q) * wm_interm_12_pt4(a,b,i,j) * wm_interm_39_pt4(b,j,p,i)
term(469) = term(469) + t1(a,q) * wm_interm_1_pt4(a,b,i,j) * wm_interm_39_pt4(b,p,j,i)
term(470) = term(470) + t1(a,q) * wm_interm_39_pt4(b,p,i,j) * wm_interm_5_pt4(a,b,j,i)
term(471) = term(471) + t1(a,q) * wm_interm_40_pt4(b,p,i,j) * wm_interm_5_pt4(a,b,j,i)
term(472) = term(472) + t1(a,q) * wm_interm_1_pt4(a,b,i,j) * wm_interm_40_pt4(b,p,j,i)
term(473) = term(473) + r1(vrdav_Rr, a,i) * wm_interm_1_pt4(a,b,p,j) * wm_interm_43_pt4(b,j,i,q)
term(474) = term(474) + r1(vrdav_Rr, a,i) * wm_interm_43_pt4(b,j,i,q) * wm_interm_5_pt4(a,b,p,j)
term(475) = term(475) + t1(a,q) * wm_interm_40_pt4(b,i,p,j) * wm_interm_5_pt4(a,b,j,i)
term(476) = term(476) + t1(a,q) * wm_interm_1_pt4(a,b,i,j) * wm_interm_40_pt4(b,j,p,i)
term(477) = term(477) + r1(vrdav_Rr, a,i) * wm_interm_42_pt4(b,j,i,q) * wm_interm_5_pt4(a,b,p,j)
term(478) = term(478) + r1(vrdav_Rr, a,i) * wm_interm_1_pt4(a,b,p,j) * wm_interm_42_pt4(b,j,i,q)
term(479) = term(479) + t1(a,q) * wm_interm_39_pt4(b,i,p,j) * wm_interm_5_pt4(a,b,j,i)
term(480) = term(480) + t1(a,q) * wm_interm_1_pt4(a,b,i,j) * wm_interm_39_pt4(b,j,p,i)
term(481) = term(481) + t1(a,i) * wm_interm_39_pt4(b,p,i,j) * wm_interm_5_pt4(a,b,j,q)
term(482) = term(482) + r1(vrdav_Rr, a,i) * wm_interm_48_pt4(a,b,j,q) * wm_interm_76_pt4(b,p,i,j)
term(483) = term(483) + t1(a,i) * wm_interm_1_pt4(a,b,j,q) * wm_interm_39_pt4(b,p,i,j)
term(484) = term(484) + r1(vrdav_Rr, a,i) * wm_interm_23_pt4(b,p,i,j) * wm_interm_53_pt4(a,b,j,q)
term(485) = term(485) + r1(vrdav_Rr, a,i) * wm_interm_23_pt4(b,p,i,j) * wm_interm_50_pt4(a,b,j,q)
term(486) = term(486) + r1(vrdav_Rr, a,i) * wm_interm_49_pt4(a,b,j,q) * wm_interm_76_pt4(b,p,i,j)
term(487) = term(487) + t1(a,i) * wm_interm_11_pt4(a,b,j,q) * wm_interm_39_pt4(b,p,i,j)
term(488) = term(488) + t1(a,i) * wm_interm_12_pt4(a,b,j,q) * wm_interm_39_pt4(b,p,i,j)
term(489) = term(489) + t1(a,i) * wm_interm_1_pt4(a,b,j,q) * wm_interm_40_pt4(b,p,i,j)
term(490) = term(490) + t1(a,i) * wm_interm_40_pt4(b,p,i,j) * wm_interm_5_pt4(a,b,j,q)
term(491) = term(491) + t1(a,i) * wm_interm_11_pt4(a,b,j,q) * wm_interm_40_pt4(b,p,i,j)
term(492) = term(492) + t1(a,i) * wm_interm_12_pt4(a,b,j,q) * wm_interm_40_pt4(b,p,i,j)
term(493) = term(493) + t1(a,i) * wm_interm_13_pt4(b,j,i,p) * wm_interm_49_pt4(a,b,j,q)
term(494) = term(494) + r1(vrdav_Rr, a,i) * wm_interm_42_pt4(b,p,i,j) * wm_interm_5_pt4(a,b,j,q)
term(495) = term(495) + r1(vrdav_Rr, a,i) * wm_interm_1_pt4(a,b,j,q) * wm_interm_42_pt4(b,p,i,j)
term(496) = term(496) + t1(a,i) * wm_interm_13_pt4(b,j,i,p) * wm_interm_48_pt4(a,b,j,q)
term(497) = term(497) + t1(a,i) * wm_interm_13_pt4(b,j,i,p) * wm_interm_53_pt4(a,b,j,q)
term(498) = term(498) + r1(vrdav_Rr, a,i) * wm_interm_11_pt4(a,b,j,q) * wm_interm_42_pt4(b,p,i,j)
term(499) = term(499) + t1(a,i) * wm_interm_13_pt4(b,j,i,p) * wm_interm_50_pt4(a,b,j,q)
term(500) = term(500) + r1(vrdav_Rr, a,i) * wm_interm_12_pt4(a,b,j,q) * wm_interm_42_pt4(b,p,i,j)
term(501) = term(501) + r1(vrdav_Rr, a,i) * wm_interm_50_pt4(a,b,j,q) * wm_interm_76_pt4(b,p,i,j)
term(502) = term(502) + r1(vrdav_Rr, a,i) * wm_interm_53_pt4(a,b,j,q) * wm_interm_76_pt4(b,p,i,j)
term(503) = term(503) + r1(vrdav_Rr, a,i) * wm_interm_11_pt4(a,b,j,q) * wm_interm_43_pt4(b,p,i,j)
term(504) = term(504) + r1(vrdav_Rr, a,i) * wm_interm_43_pt4(b,p,i,j) * wm_interm_5_pt4(a,b,j,q)
term(505) = term(505) + r1(vrdav_Rr, a,i) * wm_interm_1_pt4(a,b,j,q) * wm_interm_43_pt4(b,p,i,j)
term(506) = term(506) + r1(vrdav_Rr, a,i) * wm_interm_12_pt4(a,b,j,q) * wm_interm_43_pt4(b,p,i,j)
end do 
end do 
end do 
end do 

term(409) = term(409) * (4.0d+0) 
term(410) = term(410) * (-8.0d+0) 
term(411) = term(411) * (4.0d+0) 
term(412) = term(412) * (-8.0d+0) 
term(413) = term(413) * (4.0d+0) 
term(414) = term(414) * (-8.0d+0) 
term(415) = term(415) * (-8.0d+0) 
term(416) = term(416) * (16.0d+0) 
term(417) = term(417) * (4.0d+0) 
term(418) = term(418) * (-8.0d+0) 
term(419) = term(419) * (-8.0d+0) 
term(420) = term(420) * (16.0d+0) 
term(421) = term(421) * (4.0d+0) 
term(422) = term(422) * (-8.0d+0) 
term(423) = term(423) * (-8.0d+0) 
term(424) = term(424) * (16.0d+0) 
term(425) = term(425) * (4.0d+0) 
term(426) = term(426) * (-8.0d+0) 
term(427) = term(427) * (-8.0d+0) 
term(428) = term(428) * (16.0d+0) 
term(429) = term(429) * (4.0d+0) 
term(430) = term(430) * (-8.0d+0) 
term(431) = term(431) * (4.0d+0) 
term(432) = term(432) * (-8.0d+0) 
term(433) = term(433) * (-2.0d+0) 
term(434) = term(434) * (4.0d+0) 
term(435) = term(435) * (4.0d+0) 
term(436) = term(436) * (-8.0d+0) 
term(437) = term(437) * (-2.0d+0) 
term(438) = term(438) * (4.0d+0) 
term(439) = term(439) * (-2.0d+0) 
term(440) = term(440) * (4.0d+0) 
term(441) = term(441) * (-2.0d+0) 
term(442) = term(442) * (4.0d+0) 
term(443) = term(443) * (4.0d+0) 
term(444) = term(444) * (-8.0d+0) 
term(445) = term(445) * (-2.0d+0) 
term(446) = term(446) * (4.0d+0) 
term(447) = term(447) * (-2.0d+0) 
term(448) = term(448) * (4.0d+0) 
term(449) = term(449) * (4.0d+0) 
term(450) = term(450) * (-8.0d+0) 
term(451) = term(451) * (-2.0d+0) 
term(452) = term(452) * (4.0d+0) 
term(453) = term(453) * (-2.0d+0) 
term(454) = term(454) * (4.0d+0) 
term(455) = term(455) * (-2.0d+0) 
term(456) = term(456) * (4.0d+0) 
term(457) = term(457) * (2.0d+0) 
term(458) = term(458) * (-4.0d+0) 
term(459) = term(459) * (-1.0d+0) 
term(460) = term(460) * (2.0d+0) 
term(461) = term(461) * (-1.0d+0) 
term(462) = term(462) * (2.0d+0) 
term(463) = term(463) * (-1.0d+0) 
term(464) = term(464) * (2.0d+0) 
term(465) = term(465) * (2.0d+0) 
term(466) = term(466) * (-4.0d+0) 
term(467) = term(467) * (2.0d+0) 
term(468) = term(468) * (-4.0d+0) 
term(469) = term(469) * (-1.0d+0) 
term(470) = term(470) * (2.0d+0) 
term(471) = term(471) * (-1.0d+0) 
term(472) = term(472) * (2.0d+0) 
term(473) = term(473) * (-1.0d+0) 
term(474) = term(474) * (2.0d+0) 
term(475) = term(475) * (2.0d+0) 
term(476) = term(476) * (-1.0d+0) 
term(477) = term(477) * (-1.0d+0) 
term(478) = term(478) * (2.0d+0) 
term(479) = term(479) * (-1.0d+0) 
term(480) = term(480) * (2.0d+0) 
term(481) = term(481) * (-1.0d+0) 
term(482) = term(482) * (4.0d+0) 
term(483) = term(483) * (2.0d+0) 
term(484) = term(484) * (2.0d+0) 
term(485) = term(485) * (-1.0d+0) 
term(486) = term(486) * (-2.0d+0) 
term(487) = term(487) * (2.0d+0) 
term(488) = term(488) * (-4.0d+0) 
term(489) = term(489) * (-1.0d+0) 
term(490) = term(490) * (2.0d+0) 
term(491) = term(491) * (-1.0d+0) 
term(492) = term(492) * (2.0d+0) 
term(493) = term(493) * (4.0d+0) 
term(494) = term(494) * (-1.0d+0) 
term(495) = term(495) * (2.0d+0) 
term(496) = term(496) * (-8.0d+0) 
term(497) = term(497) * (4.0d+0) 
term(498) = term(498) * (2.0d+0) 
term(499) = term(499) * (-2.0d+0) 
term(500) = term(500) * (-4.0d+0) 
term(501) = term(501) * (2.0d+0) 
term(502) = term(502) * (-1.0d+0) 
term(503) = term(503) * (-1.0d+0) 
term(504) = term(504) * (2.0d+0) 
term(505) = term(505) * (-1.0d+0) 
term(506) = term(506) * (2.0d+0) 

do a = nocc + 1, nactive 
do i = 1, nocc 
do b = nocc + 1, nactive 
term(507) = term(507) + r1(vrdav_Rl, a,i) * wm_interm_20_pt4(b,a) * wm_interm_31_pt4(b,i)
term(508) = term(508) + r1(vrdav_Rl, a,i) * wm_interm_21_pt4(b,a) * wm_interm_31_pt4(b,i)
term(509) = term(509) + r1(vrdav_Rl, a,i) * wm_interm_20_pt4(b,a) * wm_interm_34_pt4(b,i)
term(510) = term(510) + r1(vrdav_Rl, a,i) * wm_interm_21_pt4(b,a) * wm_interm_34_pt4(b,i)
term(511) = term(511) + r1(vrdav_Rl, a,i) * wm_interm_20_pt4(b,a) * wm_interm_29_pt4(b,i,p,q)
term(512) = term(512) + r1(vrdav_Rl, a,i) * wm_interm_20_pt4(b,a) * wm_interm_28_pt4(b,i,p,q)
term(513) = term(513) + r1(vrdav_Rl, a,i) * wm_interm_21_pt4(b,a) * wm_interm_29_pt4(b,i,p,q)
term(514) = term(514) + r1(vrdav_Rl, a,i) * wm_interm_21_pt4(b,a) * wm_interm_28_pt4(b,i,p,q)
term(515) = term(515) + r1(vrdav_Rl, a,i) * wm_interm_11_pt4(b,a,p,q) * wm_interm_31_pt4(b,i)
term(516) = term(516) + r1(vrdav_Rl, a,i) * wm_interm_12_pt4(b,a,p,q) * wm_interm_31_pt4(b,i)
term(517) = term(517) + r1(vrdav_Rl, a,i) * wm_interm_11_pt4(b,a,p,q) * wm_interm_34_pt4(b,i)
term(518) = term(518) + r1(vrdav_Rl, a,i) * wm_interm_12_pt4(b,a,p,q) * wm_interm_34_pt4(b,i)
term(519) = term(519) + r1(vrdav_Rl, a,i) * wm_interm_1_pt4(b,a,p,q) * wm_interm_31_pt4(b,i)
term(520) = term(520) + r1(vrdav_Rl, a,i) * wm_interm_31_pt4(b,i) * wm_interm_5_pt4(b,a,p,q)
term(521) = term(521) + r1(vrdav_Rl, a,i) * wm_interm_1_pt4(b,a,p,q) * wm_interm_34_pt4(b,i)
term(522) = term(522) + r1(vrdav_Rl, a,i) * wm_interm_34_pt4(b,i) * wm_interm_5_pt4(b,a,p,q)
term(523) = term(523) + s1(a,i) * wm_interm_1_pt4(b,a,p,q) * wm_interm_35_pt4(b,i)
term(524) = term(524) + s1(a,i) * wm_interm_1_pt4(b,a,p,q) * wm_interm_32_pt4(b,i)
term(525) = term(525) + s1(a,i) * wm_interm_35_pt4(b,i) * wm_interm_5_pt4(b,a,p,q)
term(526) = term(526) + s1(a,i) * wm_interm_32_pt4(b,i) * wm_interm_5_pt4(b,a,p,q)
term(527) = term(527) + s1(a,i) * wm_interm_11_pt4(b,a,p,q) * wm_interm_35_pt4(b,i)
term(528) = term(528) + s1(a,i) * wm_interm_11_pt4(b,a,p,q) * wm_interm_32_pt4(b,i)
term(529) = term(529) + s1(a,i) * wm_interm_12_pt4(b,a,p,q) * wm_interm_35_pt4(b,i)
term(530) = term(530) + s1(a,i) * wm_interm_12_pt4(b,a,p,q) * wm_interm_32_pt4(b,i)
end do 
end do 
end do 

term(507) = term(507) * (8.0d+0) 
term(508) = term(508) * (-16.0d+0) 
term(509) = term(509) * (-4.0d+0) 
term(510) = term(510) * (8.0d+0) 
term(511) = term(511) * (-2.0d+0) 
term(512) = term(512) * (4.0d+0) 
term(513) = term(513) * (4.0d+0) 
term(514) = term(514) * (-8.0d+0) 
term(515) = term(515) * (-4.0d+0) 
term(516) = term(516) * (8.0d+0) 
term(517) = term(517) * (2.0d+0) 
term(518) = term(518) * (-4.0d+0) 
term(519) = term(519) * (-4.0d+0) 
term(520) = term(520) * (8.0d+0) 
term(521) = term(521) * (2.0d+0) 
term(522) = term(522) * (-4.0d+0) 
term(523) = term(523) * (4.0d+0) 
term(524) = term(524) * (-8.0d+0) 
term(525) = term(525) * (-8.0d+0) 
term(526) = term(526) * (16.0d+0) 
term(527) = term(527) * (4.0d+0) 
term(528) = term(528) * (-8.0d+0) 
term(529) = term(529) * (-8.0d+0) 
term(530) = term(530) * (16.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(531) = term(531) + s1(a,q) * wm_interm_18_pt4(i,j) * wm_interm_26_pt4(a,j,p,i)
term(532) = term(532) + s1(a,q) * wm_interm_18_pt4(i,j) * wm_interm_26_pt4(a,j,i,p)
term(533) = term(533) + s1(a,q) * wm_interm_19_pt4(i,j) * wm_interm_26_pt4(a,j,p,i)
term(534) = term(534) + s1(a,q) * wm_interm_19_pt4(i,j) * wm_interm_26_pt4(a,j,i,p)
term(535) = term(535) + r1(vrdav_Rl, a,i) * wm_interm_10_pt4(p,j,i,q) * wm_interm_31_pt4(a,j)
term(536) = term(536) + r1(vrdav_Rl, a,i) * wm_interm_10_pt4(j,p,i,q) * wm_interm_31_pt4(a,j)
term(537) = term(537) + r1(vrdav_Rl, a,i) * wm_interm_10_pt4(p,j,i,q) * wm_interm_34_pt4(a,j)
term(538) = term(538) + r1(vrdav_Rl, a,i) * wm_interm_10_pt4(j,p,i,q) * wm_interm_34_pt4(a,j)
term(539) = term(539) + s1(a,i) * wm_interm_10_pt4(j,p,q,i) * wm_interm_35_pt4(a,j)
term(540) = term(540) + s1(a,i) * wm_interm_10_pt4(j,p,q,i) * wm_interm_32_pt4(a,j)
term(541) = term(541) + s1(a,i) * wm_interm_10_pt4(p,j,q,i) * wm_interm_35_pt4(a,j)
term(542) = term(542) + s1(a,i) * wm_interm_10_pt4(p,j,q,i) * wm_interm_32_pt4(a,j)
term(543) = term(543) + s1(a,i) * wm_interm_10_pt4(p,j,i,q) * wm_interm_35_pt4(a,j)
term(544) = term(544) + s1(a,i) * wm_interm_10_pt4(p,j,i,q) * wm_interm_32_pt4(a,j)
term(545) = term(545) + s1(a,i) * wm_interm_10_pt4(j,p,i,q) * wm_interm_35_pt4(a,j)
term(546) = term(546) + s1(a,i) * wm_interm_10_pt4(j,p,i,q) * wm_interm_32_pt4(a,j)
term(547) = term(547) + t1(a,q) * wm_interm_18_pt4(i,j) * wm_interm_39_pt4(a,p,j,i)
term(548) = term(548) + t1(a,q) * wm_interm_19_pt4(i,j) * wm_interm_39_pt4(a,p,j,i)
term(549) = term(549) + t1(a,q) * wm_interm_18_pt4(i,j) * wm_interm_40_pt4(a,p,j,i)
term(550) = term(550) + t1(a,q) * wm_interm_19_pt4(i,j) * wm_interm_40_pt4(a,p,j,i)
term(551) = term(551) + t1(a,q) * wm_interm_18_pt4(i,j) * wm_interm_39_pt4(a,j,p,i)
term(552) = term(552) + t1(a,q) * wm_interm_19_pt4(i,j) * wm_interm_39_pt4(a,j,p,i)
term(553) = term(553) + t1(a,q) * wm_interm_18_pt4(i,j) * wm_interm_40_pt4(a,j,p,i)
term(554) = term(554) + t1(a,q) * wm_interm_19_pt4(i,j) * wm_interm_40_pt4(a,j,p,i)
end do 
end do 
end do 

term(531) = term(531) * (-2.0d+0) 
term(532) = term(532) * (4.0d+0) 
term(533) = term(533) * (4.0d+0) 
term(534) = term(534) * (-8.0d+0) 
term(535) = term(535) * (-4.0d+0) 
term(536) = term(536) * (8.0d+0) 
term(537) = term(537) * (2.0d+0) 
term(538) = term(538) * (-4.0d+0) 
term(540) = term(540) * (-2.0d+0) 
term(541) = term(541) * (-4.0d+0) 
term(542) = term(542) * (8.0d+0) 
term(543) = term(543) * (3.0d+0) 
term(544) = term(544) * (-6.0d+0) 
term(545) = term(545) * (-4.0d+0) 
term(546) = term(546) * (8.0d+0) 
term(547) = term(547) * (2.0d+0) 
term(548) = term(548) * (-4.0d+0) 
term(549) = term(549) * (-1.0d+0) 
term(550) = term(550) * (2.0d+0) 
term(551) = term(551) * (-1.0d+0) 
term(552) = term(552) * (2.0d+0) 
term(553) = term(553) * (2.0d+0) 
term(554) = term(554) * (-4.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(555) = term(555) + s2(a,b,q,i) * wm_interm_15_pt4(b,i) * wm_interm_31_pt4(a,p)
term(556) = term(556) + s2(a,b,q,i) * wm_interm_15_pt4(b,i) * wm_interm_34_pt4(a,p)
term(557) = term(557) + s2(a,b,q,i) * wm_interm_17_pt4(b,i) * wm_interm_31_pt4(a,p)
term(558) = term(558) + s2(a,b,q,i) * wm_interm_17_pt4(b,i) * wm_interm_34_pt4(a,p)
term(559) = term(559) + s1(a,q) * wm_interm_15_pt4(b,i) * wm_interm_62_pt4(a,b,p,i)
term(560) = term(560) + s1(a,q) * wm_interm_15_pt4(b,i) * wm_interm_61_pt4(a,b,p,i)
term(561) = term(561) + s1(a,q) * wm_interm_15_pt4(b,i) * wm_interm_59_pt4(a,b,p,i)
term(562) = term(562) + s1(a,q) * wm_interm_15_pt4(b,i) * wm_interm_60_pt4(a,b,p,i)
term(563) = term(563) + s1(a,q) * wm_interm_17_pt4(b,i) * wm_interm_62_pt4(a,b,p,i)
term(564) = term(564) + s1(a,q) * wm_interm_17_pt4(b,i) * wm_interm_61_pt4(a,b,p,i)
term(565) = term(565) + s1(a,q) * wm_interm_17_pt4(b,i) * wm_interm_59_pt4(a,b,p,i)
term(566) = term(566) + s1(a,q) * wm_interm_17_pt4(b,i) * wm_interm_60_pt4(a,b,p,i)
term(567) = term(567) + s2(a,b,p,i) * wm_interm_17_pt4(a,q) * wm_interm_31_pt4(b,i)
term(568) = term(568) + s2(a,b,p,i) * wm_interm_17_pt4(a,q) * wm_interm_34_pt4(b,i)
term(569) = term(569) + s2(a,b,p,i) * wm_interm_15_pt4(a,q) * wm_interm_31_pt4(b,i)
term(570) = term(570) + s2(a,b,p,i) * wm_interm_15_pt4(a,q) * wm_interm_34_pt4(b,i)
term(571) = term(571) + r1(vrdav_Rl, a,i) * wm_interm_3_pt4(b,p,i,q) * wm_interm_68_pt4(a,b)
term(572) = term(572) + r1(vrdav_Rl, a,i) * wm_interm_3_pt4(b,p,i,q) * wm_interm_69_pt4(a,b)
term(573) = term(573) + s2(a,b,p,i) * wm_interm_35_pt4(b,i) * wm_interm_7_pt4(a,q)
term(574) = term(574) + s2(a,b,p,i) * wm_interm_32_pt4(b,i) * wm_interm_7_pt4(a,q)
term(575) = term(575) + s2(a,b,p,i) * wm_interm_35_pt4(b,i) * wm_interm_8_pt4(a,q)
term(576) = term(576) + s2(a,b,p,i) * wm_interm_32_pt4(b,i) * wm_interm_8_pt4(a,q)
term(577) = term(577) + s1(a,i) * wm_interm_14_pt4(b,p,q,i) * wm_interm_68_pt4(a,b)
term(578) = term(578) + s1(a,i) * wm_interm_14_pt4(b,p,q,i) * wm_interm_69_pt4(a,b)
term(579) = term(579) + s1(a,i) * wm_interm_14_pt4(b,p,i,q) * wm_interm_68_pt4(a,b)
term(580) = term(580) + s1(a,i) * wm_interm_14_pt4(b,p,i,q) * wm_interm_69_pt4(a,b)
term(581) = term(581) + r1(vrdav_Rl, a,i) * wm_interm_3_pt4(b,p,q,i) * wm_interm_68_pt4(a,b)
term(582) = term(582) + r1(vrdav_Rl, a,i) * wm_interm_3_pt4(b,p,q,i) * wm_interm_69_pt4(a,b)
term(583) = term(583) + t1(a,i) * wm_interm_20_pt4(a,b) * wm_interm_40_pt4(b,q,i,p)
term(584) = term(584) + t1(a,i) * wm_interm_21_pt4(a,b) * wm_interm_40_pt4(b,q,i,p)
term(585) = term(585) + t1(a,i) * wm_interm_20_pt4(a,b) * wm_interm_39_pt4(b,q,i,p)
term(586) = term(586) + t1(a,i) * wm_interm_21_pt4(a,b) * wm_interm_39_pt4(b,q,i,p)
term(587) = term(587) + r1(vrdav_Rr, a,p) * wm_interm_11_pt4(a,b,q,i) * wm_interm_44_pt4(b,i)
term(588) = term(588) + r1(vrdav_Rr, a,p) * wm_interm_12_pt4(a,b,q,i) * wm_interm_44_pt4(b,i)
term(589) = term(589) + t1(a,i) * wm_interm_13_pt4(b,p,i,q) * wm_interm_51_pt4(a,b)
term(590) = term(590) + t1(a,i) * wm_interm_13_pt4(b,p,q,i) * wm_interm_51_pt4(a,b)
term(591) = term(591) + t1(a,i) * wm_interm_13_pt4(b,p,q,i) * wm_interm_52_pt4(a,b)
term(592) = term(592) + t1(a,i) * wm_interm_13_pt4(b,p,i,q) * wm_interm_52_pt4(a,b)
term(593) = term(593) + r1(vrdav_Rr, a,p) * wm_interm_44_pt4(b,i) * wm_interm_5_pt4(a,b,q,i)
term(594) = term(594) + r1(vrdav_Rr, a,p) * wm_interm_1_pt4(a,b,q,i) * wm_interm_44_pt4(b,i)
term(595) = term(595) + r1(vrdav_Rr, a,p) * wm_interm_11_pt4(a,b,q,i) * wm_interm_45_pt4(b,i)
term(596) = term(596) + r1(vrdav_Rr, a,p) * wm_interm_12_pt4(a,b,q,i) * wm_interm_45_pt4(b,i)
term(597) = term(597) + r1(vrdav_Rr, a,p) * wm_interm_45_pt4(b,i) * wm_interm_5_pt4(a,b,q,i)
term(598) = term(598) + r1(vrdav_Rr, a,p) * wm_interm_1_pt4(a,b,q,i) * wm_interm_45_pt4(b,i)
term(599) = term(599) + r1(vrdav_Rr, a,i) * wm_interm_20_pt4(a,b) * wm_interm_43_pt4(b,p,i,q)
term(600) = term(600) + t1(a,q) * wm_interm_48_pt4(a,b,p,i) * wm_interm_4_pt4(b,i)
term(601) = term(601) + r1(vrdav_Rr, a,i) * wm_interm_21_pt4(a,b) * wm_interm_43_pt4(b,p,i,q)
term(602) = term(602) + t1(a,q) * wm_interm_2_pt4(b,i) * wm_interm_48_pt4(a,b,p,i)
term(603) = term(603) + t1(a,q) * wm_interm_11_pt4(a,b,p,i) * wm_interm_41_pt4(b,i)
term(604) = term(604) + t1(a,q) * wm_interm_12_pt4(a,b,p,i) * wm_interm_41_pt4(b,i)
term(605) = term(605) + t1(a,q) * wm_interm_49_pt4(a,b,p,i) * wm_interm_4_pt4(b,i)
term(606) = term(606) + t1(a,q) * wm_interm_2_pt4(b,i) * wm_interm_49_pt4(a,b,p,i)
term(607) = term(607) + t1(a,q) * wm_interm_11_pt4(a,b,p,i) * wm_interm_38_pt4(b,i)
term(608) = term(608) + t1(a,q) * wm_interm_12_pt4(a,b,p,i) * wm_interm_38_pt4(b,i)
term(609) = term(609) + r1(vrdav_Rr, a,i) * wm_interm_20_pt4(a,b) * wm_interm_42_pt4(b,p,i,q)
term(610) = term(610) + r1(vrdav_Rr, a,i) * wm_interm_21_pt4(a,b) * wm_interm_42_pt4(b,p,i,q)
term(611) = term(611) + t1(a,q) * wm_interm_2_pt4(b,i) * wm_interm_50_pt4(a,b,p,i)
term(612) = term(612) + t1(a,q) * wm_interm_1_pt4(a,b,p,i) * wm_interm_38_pt4(b,i)
term(613) = term(613) + t1(a,q) * wm_interm_4_pt4(b,i) * wm_interm_50_pt4(a,b,p,i)
term(614) = term(614) + t1(a,q) * wm_interm_38_pt4(b,i) * wm_interm_5_pt4(a,b,p,i)
term(615) = term(615) + r1(vrdav_Rr, a,i) * wm_interm_23_pt4(b,p,i,q) * wm_interm_51_pt4(a,b)
term(616) = term(616) + r1(vrdav_Rr, a,i) * wm_interm_23_pt4(b,p,i,q) * wm_interm_52_pt4(a,b)
term(617) = term(617) + t1(a,q) * wm_interm_4_pt4(b,i) * wm_interm_53_pt4(a,b,p,i)
term(618) = term(618) + t1(a,q) * wm_interm_2_pt4(b,i) * wm_interm_53_pt4(a,b,p,i)
term(619) = term(619) + t1(a,q) * wm_interm_41_pt4(b,i) * wm_interm_5_pt4(a,b,p,i)
term(620) = term(620) + t1(a,q) * wm_interm_1_pt4(a,b,p,i) * wm_interm_41_pt4(b,i)
term(621) = term(621) + t2(a,b,q,i) * wm_interm_44_pt4(a,p) * wm_interm_4_pt4(b,i)
term(622) = term(622) + t2(a,b,q,i) * wm_interm_2_pt4(b,i) * wm_interm_44_pt4(a,p)
term(623) = term(623) + t2(a,b,q,i) * wm_interm_45_pt4(a,p) * wm_interm_4_pt4(b,i)
term(624) = term(624) + t2(a,b,q,i) * wm_interm_2_pt4(b,i) * wm_interm_45_pt4(a,p)
term(625) = term(625) + t2(a,b,q,i) * wm_interm_44_pt4(b,i) * wm_interm_4_pt4(a,p)
term(626) = term(626) + t2(a,b,q,i) * wm_interm_2_pt4(a,p) * wm_interm_44_pt4(b,i)
term(627) = term(627) + t2(a,b,q,i) * wm_interm_45_pt4(b,i) * wm_interm_4_pt4(a,p)
term(628) = term(628) + t2(a,b,q,i) * wm_interm_2_pt4(a,p) * wm_interm_45_pt4(b,i)
end do 
end do 
end do 

term(555) = term(555) * (-16.0d+0) 
term(556) = term(556) * (8.0d+0) 
term(557) = term(557) * (8.0d+0) 
term(558) = term(558) * (-4.0d+0) 
term(559) = term(559) * (4.0d+0) 
term(560) = term(560) * (-8.0d+0) 
term(561) = term(561) * (-8.0d+0) 
term(562) = term(562) * (16.0d+0) 
term(563) = term(563) * (-2.0d+0) 
term(564) = term(564) * (4.0d+0) 
term(565) = term(565) * (4.0d+0) 
term(566) = term(566) * (-8.0d+0) 
term(567) = term(567) * (8.0d+0) 
term(568) = term(568) * (-4.0d+0) 
term(569) = term(569) * (-16.0d+0) 
term(570) = term(570) * (8.0d+0) 
term(571) = term(571) * (4.0d+0) 
term(572) = term(572) * (-8.0d+0) 
term(573) = term(573) * (16.0d+0) 
term(574) = term(574) * (-32.0d+0) 
term(575) = term(575) * (-8.0d+0) 
term(576) = term(576) * (16.0d+0) 
term(577) = term(577) * (-8.0d+0) 
term(578) = term(578) * (16.0d+0) 
term(579) = term(579) * (4.0d+0) 
term(580) = term(580) * (-8.0d+0) 
term(581) = term(581) * (-8.0d+0) 
term(582) = term(582) * (16.0d+0) 
term(583) = term(583) * (4.0d+0) 
term(584) = term(584) * (-8.0d+0) 
term(585) = term(585) * (-2.0d+0) 
term(586) = term(586) * (4.0d+0) 
term(587) = term(587) * (-16.0d+0) 
term(588) = term(588) * (32.0d+0) 
term(589) = term(589) * (8.0d+0) 
term(590) = term(590) * (-16.0d+0) 
term(591) = term(591) * (8.0d+0) 
term(592) = term(592) * (-4.0d+0) 
term(593) = term(593) * (8.0d+0) 
term(594) = term(594) * (-16.0d+0) 
term(595) = term(595) * (8.0d+0) 
term(596) = term(596) * (-16.0d+0) 
term(597) = term(597) * (-4.0d+0) 
term(598) = term(598) * (8.0d+0) 
term(599) = term(599) * (2.0d+0) 
term(600) = term(600) * (-8.0d+0) 
term(601) = term(601) * (-4.0d+0) 
term(602) = term(602) * (16.0d+0) 
term(603) = term(603) * (-8.0d+0) 
term(604) = term(604) * (16.0d+0) 
term(605) = term(605) * (4.0d+0) 
term(606) = term(606) * (-8.0d+0) 
term(607) = term(607) * (4.0d+0) 
term(608) = term(608) * (-8.0d+0) 
term(609) = term(609) * (-1.0d+0) 
term(610) = term(610) * (2.0d+0) 
term(611) = term(611) * (4.0d+0) 
term(612) = term(612) * (4.0d+0) 
term(613) = term(613) * (-2.0d+0) 
term(614) = term(614) * (-2.0d+0) 
term(615) = term(615) * (4.0d+0) 
term(616) = term(616) * (-2.0d+0) 
term(617) = term(617) * (4.0d+0) 
term(618) = term(618) * (-8.0d+0) 
term(619) = term(619) * (4.0d+0) 
term(620) = term(620) * (-8.0d+0) 
term(621) = term(621) * (-8.0d+0) 
term(622) = term(622) * (16.0d+0) 
term(623) = term(623) * (4.0d+0) 
term(624) = term(624) * (-8.0d+0) 
term(625) = term(625) * (-8.0d+0) 
term(626) = term(626) * (16.0d+0) 
term(627) = term(627) * (4.0d+0) 
term(628) = term(628) * (-8.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(629) = term(629) + s1(a,q) * wm_interm_14_pt4(a,i,j,k) * wm_interm_72_pt4(i,p,k,j)
term(630) = term(630) + s1(a,q) * wm_interm_14_pt4(a,i,j,k) * wm_interm_72_pt4(i,p,j,k)
term(631) = term(631) + s1(a,i) * wm_interm_14_pt4(a,q,j,k) * wm_interm_72_pt4(p,i,k,j)
term(632) = term(632) + s1(a,i) * wm_interm_14_pt4(a,q,j,k) * wm_interm_72_pt4(i,p,k,j)
term(633) = term(633) + s1(a,i) * wm_interm_14_pt4(a,q,j,k) * wm_interm_72_pt4(i,p,j,k)
term(634) = term(634) + s1(a,i) * wm_interm_14_pt4(a,q,j,k) * wm_interm_72_pt4(p,i,j,k)
term(635) = term(635) + s1(a,i) * wm_interm_14_pt4(a,j,q,k) * wm_interm_72_pt4(j,i,k,p)
term(636) = term(636) + s1(a,i) * wm_interm_14_pt4(a,j,q,k) * wm_interm_72_pt4(j,i,p,k)
term(637) = term(637) + s1(a,i) * wm_interm_10_pt4(j,k,i,q) * wm_interm_26_pt4(a,p,j,k)
term(638) = term(638) + s1(a,i) * wm_interm_14_pt4(a,j,k,q) * wm_interm_72_pt4(j,i,p,k)
term(639) = term(639) + s1(a,i) * wm_interm_14_pt4(a,j,k,q) * wm_interm_72_pt4(j,i,k,p)
term(640) = term(640) + t1(a,i) * wm_interm_13_pt4(a,p,j,k) * wm_interm_55_pt4(q,i,k,j)
term(641) = term(641) + t1(a,i) * wm_interm_13_pt4(a,p,j,k) * wm_interm_55_pt4(q,i,j,k)
term(642) = term(642) + t1(a,i) * wm_interm_10_pt4(i,q,j,k) * wm_interm_40_pt4(a,j,k,p)
term(643) = term(643) + t1(a,i) * wm_interm_10_pt4(q,i,j,k) * wm_interm_40_pt4(a,j,k,p)
term(644) = term(644) + t1(a,i) * wm_interm_13_pt4(a,p,j,k) * wm_interm_55_pt4(i,q,j,k)
term(645) = term(645) + t1(a,i) * wm_interm_13_pt4(a,p,j,k) * wm_interm_55_pt4(i,q,k,j)
term(646) = term(646) + t1(a,i) * wm_interm_10_pt4(q,i,j,k) * wm_interm_39_pt4(a,j,k,p)
term(647) = term(647) + t1(a,i) * wm_interm_10_pt4(i,q,j,k) * wm_interm_39_pt4(a,j,k,p)
term(648) = term(648) + t1(a,q) * wm_interm_13_pt4(a,i,j,k) * wm_interm_55_pt4(p,i,j,k)
term(649) = term(649) + t1(a,q) * wm_interm_13_pt4(a,i,j,k) * wm_interm_55_pt4(i,p,j,k)
term(650) = term(650) + t1(a,q) * wm_interm_13_pt4(a,i,j,k) * wm_interm_55_pt4(i,p,k,j)
term(651) = term(651) + t1(a,q) * wm_interm_13_pt4(a,i,j,k) * wm_interm_55_pt4(p,i,k,j)
term(652) = term(652) + t1(a,i) * wm_interm_13_pt4(a,j,p,k) * wm_interm_55_pt4(j,i,k,q)
term(653) = term(653) + t1(a,i) * wm_interm_13_pt4(a,j,k,p) * wm_interm_55_pt4(j,i,k,q)
term(654) = term(654) + t1(a,i) * wm_interm_13_pt4(a,j,k,p) * wm_interm_55_pt4(j,i,q,k)
term(655) = term(655) + t1(a,i) * wm_interm_13_pt4(a,j,p,k) * wm_interm_55_pt4(j,i,q,k)
term(656) = term(656) + t1(a,i) * wm_interm_13_pt4(a,j,k,p) * wm_interm_55_pt4(i,j,k,q)
term(657) = term(657) + t1(a,i) * wm_interm_13_pt4(a,j,p,k) * wm_interm_55_pt4(i,j,k,q)
term(658) = term(658) + t1(a,i) * wm_interm_13_pt4(a,j,p,k) * wm_interm_55_pt4(i,j,q,k)
term(659) = term(659) + t1(a,i) * wm_interm_13_pt4(a,j,k,p) * wm_interm_55_pt4(i,j,q,k)
end do 
end do 
end do 
end do 

term(629) = term(629) * (4.0d+0) 
term(630) = term(630) * (-8.0d+0) 
term(631) = term(631) * (2.0d+0) 
term(632) = term(632) * (-4.0d+0) 
term(633) = term(633) * (2.0d+0) 
term(634) = term(634) * (-4.0d+0) 
term(635) = term(635) * (4.0d+0) 
term(636) = term(636) * (-8.0d+0) 
term(637) = term(637) * (2.0d+0) 
term(638) = term(638) * (4.0d+0) 
term(639) = term(639) * (-8.0d+0) 
term(640) = term(640) * (-2.0d+0) 
term(641) = term(641) * (4.0d+0) 
term(642) = term(642) * (-2.0d+0) 
term(643) = term(643) * (4.0d+0) 
term(644) = term(644) * (-2.0d+0) 
term(645) = term(645) * (4.0d+0) 
term(646) = term(646) * (-2.0d+0) 
term(647) = term(647) * (4.0d+0) 
term(648) = term(648) * (-1.0d+0) 
term(649) = term(649) * (2.0d+0) 
term(650) = term(650) * (-1.0d+0) 
term(651) = term(651) * (2.0d+0) 
term(652) = term(652) * (-1.0d+0) 
term(653) = term(653) * (2.0d+0) 
term(654) = term(654) * (-1.0d+0) 
term(655) = term(655) * (2.0d+0) 
term(656) = term(656) * (-1.0d+0) 
term(657) = term(657) * (2.0d+0) 
term(658) = term(658) * (-1.0d+0) 
term(659) = term(659) * (2.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(660) = term(660) + s1(a,i) * wm_interm_14_pt4(b,j,q,i) * wm_interm_59_pt4(a,b,j,p)
term(661) = term(661) + s1(a,i) * wm_interm_14_pt4(b,j,q,i) * wm_interm_60_pt4(a,b,j,p)
term(662) = term(662) + s1(a,i) * wm_interm_14_pt4(b,j,q,i) * wm_interm_61_pt4(a,b,j,p)
term(663) = term(663) + s1(a,i) * wm_interm_14_pt4(b,j,q,i) * wm_interm_62_pt4(a,b,j,p)
term(664) = term(664) + r1(vrdav_Rl, a,i) * wm_interm_3_pt4(b,j,q,i) * wm_interm_61_pt4(a,b,j,p)
term(665) = term(665) + r1(vrdav_Rl, a,i) * wm_interm_3_pt4(b,j,q,i) * wm_interm_62_pt4(a,b,j,p)
term(666) = term(666) + r1(vrdav_Rl, a,i) * wm_interm_3_pt4(b,j,q,i) * wm_interm_59_pt4(a,b,j,p)
term(667) = term(667) + r1(vrdav_Rl, a,i) * wm_interm_3_pt4(b,j,q,i) * wm_interm_60_pt4(a,b,j,p)
term(668) = term(668) + t1(a,i) * wm_interm_13_pt4(b,j,p,i) * wm_interm_49_pt4(a,b,j,q)
term(669) = term(669) + t1(a,i) * wm_interm_13_pt4(b,j,p,i) * wm_interm_48_pt4(a,b,j,q)
term(670) = term(670) + t1(a,i) * wm_interm_13_pt4(b,j,p,i) * wm_interm_53_pt4(a,b,j,q)
term(671) = term(671) + t1(a,i) * wm_interm_13_pt4(b,j,p,i) * wm_interm_50_pt4(a,b,j,q)
end do 
end do 
end do 
end do 

term(660) = term(660) * (4.0d+0) 
term(661) = term(661) * (-8.0d+0) 
term(662) = term(662) * (4.0d+0) 
term(663) = term(663) * (-8.0d+0) 
term(664) = term(664) * (4.0d+0) 
term(665) = term(665) * (-8.0d+0) 
term(666) = term(666) * (4.0d+0) 
term(667) = term(667) * (-8.0d+0) 
term(668) = term(668) * (-2.0d+0) 
term(669) = term(669) * (4.0d+0) 
term(670) = term(670) * (-2.0d+0) 
term(671) = term(671) * (4.0d+0) 

do a = nocc + 1, nactive 
term(672) = term(672) + wm_interm_31_pt4(a,p) * wm_interm_44_pt4(a,q)
term(673) = term(673) + wm_interm_34_pt4(a,p) * wm_interm_44_pt4(a,q)
term(674) = term(674) + wm_interm_31_pt4(a,p) * wm_interm_45_pt4(a,q)
term(675) = term(675) + wm_interm_34_pt4(a,p) * wm_interm_45_pt4(a,q)
end do 

term(672) = term(672) * (8.0d+0) 
term(673) = term(673) * (-4.0d+0) 
term(674) = term(674) * (-4.0d+0) 
term(675) = term(675) * (2.0d+0) 

term(676) = term(676) + wm_interm_27_pt4(p,q) * wm_interm_77_pt4
term(677) = term(677) + wm_interm_27_pt4(p,q) * wm_interm_78_pt4
term(678) = term(678) + wm_interm_25_pt4 * wm_interm_27_pt4(p,q)

term(676) = term(676) * (2.0d+0) 
term(677) = term(677) * (-4.0d+0) 
term(678) = term(678) * (-4.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(679) = term(679) + s1(a,q) * wm_interm_26_pt4(b,i,j,p) * wm_interm_5_pt4(b,a,j,i)
term(680) = term(680) + s1(a,i) * wm_interm_1_pt4(b,a,j,q) * wm_interm_26_pt4(b,p,j,i)
term(681) = term(681) + s1(a,i) * wm_interm_26_pt4(b,p,j,i) * wm_interm_5_pt4(b,a,j,q)
term(682) = term(682) + s1(a,i) * wm_interm_11_pt4(b,a,j,q) * wm_interm_26_pt4(b,p,j,i)
term(683) = term(683) + s1(a,i) * wm_interm_12_pt4(b,a,j,q) * wm_interm_26_pt4(b,p,j,i)
end do 
end do 
end do 
end do 

term(679) = term(679) * (-8.0d+0) 
term(680) = term(680) * (4.0d+0) 
term(681) = term(681) * (-8.0d+0) 
term(682) = term(682) * (4.0d+0) 
term(683) = term(683) * (-8.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(684) = term(684) + s1(a,i) * wm_interm_10_pt4(j,k,q,i) * wm_interm_26_pt4(a,p,j,k)
term(685) = term(685) + t1(a,q) * wm_interm_10_pt4(i,p,j,k) * wm_interm_39_pt4(a,j,k,i)
term(686) = term(686) + t1(a,q) * wm_interm_10_pt4(p,i,j,k) * wm_interm_39_pt4(a,j,k,i)
term(687) = term(687) + t1(a,q) * wm_interm_10_pt4(p,i,j,k) * wm_interm_40_pt4(a,j,k,i)
term(688) = term(688) + t1(a,q) * wm_interm_10_pt4(i,p,j,k) * wm_interm_40_pt4(a,j,k,i)
end do 
end do 
end do 
end do 

term(684) = term(684) * (-4.0d+0) 
term(685) = term(685) * (-1.0d+0) 
term(686) = term(686) * (2.0d+0) 
term(687) = term(687) * (-1.0d+0) 
term(688) = term(688) * (2.0d+0) 

do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(689) = term(689) + s2(a,b,i,j) * wm_interm_15_pt4(b,i) * wm_interm_31_pt4(a,j)
term(690) = term(690) + s2(a,b,i,j) * wm_interm_15_pt4(b,j) * wm_interm_31_pt4(a,i)
term(691) = term(691) + s2(a,b,i,j) * wm_interm_15_pt4(b,i) * wm_interm_34_pt4(a,j)
term(692) = term(692) + s2(a,b,i,j) * wm_interm_15_pt4(b,j) * wm_interm_34_pt4(a,i)
term(693) = term(693) + s2(a,b,i,j) * wm_interm_17_pt4(b,i) * wm_interm_31_pt4(a,j)
term(694) = term(694) + s2(a,b,i,j) * wm_interm_17_pt4(b,j) * wm_interm_31_pt4(a,i)
term(695) = term(695) + s2(a,b,i,j) * wm_interm_17_pt4(b,i) * wm_interm_34_pt4(a,j)
term(696) = term(696) + s2(a,b,i,j) * wm_interm_17_pt4(b,j) * wm_interm_34_pt4(a,i)
term(697) = term(697) + t2(a,b,i,j) * wm_interm_44_pt4(b,j) * wm_interm_4_pt4(a,i)
term(698) = term(698) + t2(a,b,i,j) * wm_interm_2_pt4(a,i) * wm_interm_44_pt4(b,j)
term(699) = term(699) + t2(a,b,i,j) * wm_interm_44_pt4(b,i) * wm_interm_4_pt4(a,j)
term(700) = term(700) + t2(a,b,i,j) * wm_interm_2_pt4(a,j) * wm_interm_44_pt4(b,i)
term(701) = term(701) + t1(a,i) * wm_interm_11_pt4(a,b,q,j) * wm_interm_40_pt4(b,i,j,p)
term(702) = term(702) + t1(a,i) * wm_interm_12_pt4(a,b,q,j) * wm_interm_40_pt4(b,i,j,p)
term(703) = term(703) + t2(a,b,i,j) * wm_interm_45_pt4(b,j) * wm_interm_4_pt4(a,i)
term(704) = term(704) + t2(a,b,i,j) * wm_interm_2_pt4(a,i) * wm_interm_45_pt4(b,j)
term(705) = term(705) + t1(a,i) * wm_interm_11_pt4(a,b,q,j) * wm_interm_39_pt4(b,i,j,p)
term(706) = term(706) + t1(a,i) * wm_interm_12_pt4(a,b,q,j) * wm_interm_39_pt4(b,i,j,p)
term(707) = term(707) + t2(a,b,i,j) * wm_interm_45_pt4(b,i) * wm_interm_4_pt4(a,j)
term(708) = term(708) + t2(a,b,i,j) * wm_interm_2_pt4(a,j) * wm_interm_45_pt4(b,i)
term(709) = term(709) + t1(a,i) * wm_interm_1_pt4(a,b,q,j) * wm_interm_39_pt4(b,i,j,p)
term(710) = term(710) + t1(a,i) * wm_interm_39_pt4(b,i,j,p) * wm_interm_5_pt4(a,b,q,j)
term(711) = term(711) + t1(a,i) * wm_interm_40_pt4(b,i,j,p) * wm_interm_5_pt4(a,b,q,j)
term(712) = term(712) + t1(a,i) * wm_interm_1_pt4(a,b,q,j) * wm_interm_40_pt4(b,i,j,p)
term(713) = term(713) + r1(vrdav_Rr, a,i) * wm_interm_11_pt4(a,b,p,j) * wm_interm_43_pt4(b,i,j,q)
term(714) = term(714) + r1(vrdav_Rr, a,i) * wm_interm_12_pt4(a,b,p,j) * wm_interm_43_pt4(b,i,j,q)
term(715) = term(715) + r1(vrdav_Rr, a,i) * wm_interm_11_pt4(a,b,p,j) * wm_interm_42_pt4(b,i,j,q)
term(716) = term(716) + r1(vrdav_Rr, a,i) * wm_interm_12_pt4(a,b,p,j) * wm_interm_42_pt4(b,i,j,q)
term(717) = term(717) + r1(vrdav_Rr, a,i) * wm_interm_1_pt4(a,b,p,j) * wm_interm_42_pt4(b,i,j,q)
term(718) = term(718) + r1(vrdav_Rr, a,i) * wm_interm_42_pt4(b,i,j,q) * wm_interm_5_pt4(a,b,p,j)
term(719) = term(719) + r1(vrdav_Rr, a,i) * wm_interm_43_pt4(b,i,j,q) * wm_interm_5_pt4(a,b,p,j)
term(720) = term(720) + r1(vrdav_Rr, a,i) * wm_interm_1_pt4(a,b,p,j) * wm_interm_43_pt4(b,i,j,q)
term(721) = term(721) + r1(vrdav_Rr, a,i) * wm_interm_48_pt4(a,b,j,q) * wm_interm_76_pt4(b,i,p,j)
term(722) = term(722) + r1(vrdav_Rr, a,i) * wm_interm_23_pt4(b,i,p,j) * wm_interm_53_pt4(a,b,j,q)
term(723) = term(723) + r1(vrdav_Rr, a,i) * wm_interm_23_pt4(b,i,p,j) * wm_interm_50_pt4(a,b,j,q)
term(724) = term(724) + r1(vrdav_Rr, a,i) * wm_interm_49_pt4(a,b,j,q) * wm_interm_76_pt4(b,i,p,j)
term(725) = term(725) + r1(vrdav_Rr, a,i) * wm_interm_42_pt4(b,i,p,j) * wm_interm_5_pt4(a,b,j,q)
term(726) = term(726) + r1(vrdav_Rr, a,i) * wm_interm_1_pt4(a,b,j,q) * wm_interm_42_pt4(b,i,p,j)
term(727) = term(727) + r1(vrdav_Rr, a,i) * wm_interm_11_pt4(a,b,j,q) * wm_interm_42_pt4(b,i,p,j)
term(728) = term(728) + r1(vrdav_Rr, a,i) * wm_interm_12_pt4(a,b,j,q) * wm_interm_42_pt4(b,i,p,j)
term(729) = term(729) + t1(a,i) * wm_interm_1_pt4(a,b,j,q) * wm_interm_39_pt4(b,i,p,j)
term(730) = term(730) + t1(a,i) * wm_interm_39_pt4(b,i,p,j) * wm_interm_5_pt4(a,b,j,q)
term(731) = term(731) + t1(a,i) * wm_interm_11_pt4(a,b,j,q) * wm_interm_39_pt4(b,i,p,j)
term(732) = term(732) + t1(a,i) * wm_interm_11_pt4(a,b,j,q) * wm_interm_40_pt4(b,i,p,j)
term(733) = term(733) + t1(a,i) * wm_interm_40_pt4(b,i,p,j) * wm_interm_5_pt4(a,b,j,q)
term(734) = term(734) + r1(vrdav_Rr, a,i) * wm_interm_50_pt4(a,b,j,q) * wm_interm_76_pt4(b,i,p,j)
term(735) = term(735) + t1(a,i) * wm_interm_12_pt4(a,b,j,q) * wm_interm_39_pt4(b,i,p,j)
term(736) = term(736) + t1(a,i) * wm_interm_1_pt4(a,b,j,q) * wm_interm_40_pt4(b,i,p,j)
term(737) = term(737) + r1(vrdav_Rr, a,i) * wm_interm_53_pt4(a,b,j,q) * wm_interm_76_pt4(b,i,p,j)
term(738) = term(738) + t1(a,i) * wm_interm_12_pt4(a,b,j,q) * wm_interm_40_pt4(b,i,p,j)
term(739) = term(739) + r1(vrdav_Rr, a,i) * wm_interm_11_pt4(a,b,j,q) * wm_interm_43_pt4(b,i,p,j)
term(740) = term(740) + r1(vrdav_Rr, a,i) * wm_interm_43_pt4(b,i,p,j) * wm_interm_5_pt4(a,b,j,q)
term(741) = term(741) + r1(vrdav_Rr, a,i) * wm_interm_1_pt4(a,b,j,q) * wm_interm_43_pt4(b,i,p,j)
term(742) = term(742) + r1(vrdav_Rr, a,i) * wm_interm_12_pt4(a,b,j,q) * wm_interm_43_pt4(b,i,p,j)
end do 
end do 
end do 
end do 

term(689) = term(689) * (-16.0d+0) 
term(690) = term(690) * (32.0d+0) 
term(691) = term(691) * (8.0d+0) 
term(692) = term(692) * (-16.0d+0) 
term(693) = term(693) * (8.0d+0) 
term(694) = term(694) * (-16.0d+0) 
term(695) = term(695) * (-4.0d+0) 
term(696) = term(696) * (8.0d+0) 
term(697) = term(697) * (32.0d+0) 
term(698) = term(698) * (-64.0d+0) 
term(699) = term(699) * (-16.0d+0) 
term(700) = term(700) * (32.0d+0) 
term(701) = term(701) * (4.0d+0) 
term(702) = term(702) * (-8.0d+0) 
term(703) = term(703) * (-16.0d+0) 
term(704) = term(704) * (32.0d+0) 
term(705) = term(705) * (-2.0d+0) 
term(706) = term(706) * (4.0d+0) 
term(707) = term(707) * (8.0d+0) 
term(708) = term(708) * (-16.0d+0) 
term(709) = term(709) * (-2.0d+0) 
term(710) = term(710) * (4.0d+0) 
term(711) = term(711) * (-2.0d+0) 
term(712) = term(712) * (4.0d+0) 
term(713) = term(713) * (2.0d+0) 
term(714) = term(714) * (-4.0d+0) 
term(715) = term(715) * (-1.0d+0) 
term(716) = term(716) * (2.0d+0) 
term(717) = term(717) * (-1.0d+0) 
term(718) = term(718) * (2.0d+0) 
term(719) = term(719) * (-1.0d+0) 
term(720) = term(720) * (2.0d+0) 
term(721) = term(721) * (-8.0d+0) 
term(722) = term(722) * (-1.0d+0) 
term(723) = term(723) * (2.0d+0) 
term(724) = term(724) * (4.0d+0) 
term(725) = term(725) * (2.0d+0) 
term(726) = term(726) * (-1.0d+0) 
term(727) = term(727) * (-1.0d+0) 
term(728) = term(728) * (2.0d+0) 
term(729) = term(729) * (-1.0d+0) 
term(730) = term(730) * (2.0d+0) 
term(731) = term(731) * (-1.0d+0) 
term(732) = term(732) * (2.0d+0) 
term(733) = term(733) * (-1.0d+0) 
term(734) = term(734) * (-1.0d+0) 
term(735) = term(735) * (2.0d+0) 
term(736) = term(736) * (2.0d+0) 
term(737) = term(737) * (2.0d+0) 
term(738) = term(738) * (-4.0d+0) 
term(739) = term(739) * (2.0d+0) 
term(740) = term(740) * (-1.0d+0) 
term(741) = term(741) * (2.0d+0) 
term(742) = term(742) * (-4.0d+0) 


    calc_D_oo_wm_pt4 = zero
    do s = 0, 742
    calc_D_oo_wm_pt4 = calc_D_oo_wm_pt4 + term(s)
    end do

    end function calc_D_oo_wm_pt4
    
    function calc_D_ov_wm_pt4(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_ov_wm_pt4
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , l, i, k, j, a, b 
    real(F64), dimension(0:463) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do l = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + r2(vrdav_Rl, a,j,q,i) * wm_interm_72_pt4(i,j,k,l) * wm_interm_73_pt4(a,p,k,l)
term(1) = term(1) + r2(vrdav_Rl, a,j,q,i) * wm_interm_72_pt4(j,i,k,l) * wm_interm_73_pt4(a,p,k,l)
end do 
end do 
end do 
end do 
end do 

term(0) = term(0) * (2.0d+0) 
term(1) = term(1) * (-4.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(2) = term(2) + r2(vrdav_Rl, a,j,q,i) * wm_interm_72_pt4(i,j,k,l) * wm_interm_73_pt4(a,p,l,k)
term(3) = term(3) + r2(vrdav_Rl, a,j,q,i) * wm_interm_72_pt4(j,i,k,l) * wm_interm_73_pt4(a,p,l,k)
end do 
end do 
end do 
end do 
end do 

term(2) = term(2) * (-4.0d+0) 
term(3) = term(3) * (2.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(4) = term(4) + r2(vrdav_Rl, a,j,q,i) * wm_interm_1_pt4(b,a,p,k) * wm_interm_29_pt4(b,i,j,k)
term(5) = term(5) + r2(vrdav_Rl, a,j,q,i) * wm_interm_1_pt4(b,a,p,k) * wm_interm_28_pt4(b,i,j,k)
term(6) = term(6) + r2(vrdav_Rl, a,j,q,i) * wm_interm_28_pt4(b,i,j,k) * wm_interm_5_pt4(b,a,p,k)
term(7) = term(7) + r2(vrdav_Rl, a,j,q,i) * wm_interm_29_pt4(b,i,j,k) * wm_interm_5_pt4(b,a,p,k)
term(8) = term(8) + r2(vrdav_Rl, a,j,q,i) * wm_interm_11_pt4(b,a,p,k) * wm_interm_29_pt4(b,i,j,k)
term(9) = term(9) + r2(vrdav_Rl, a,j,q,i) * wm_interm_11_pt4(b,a,p,k) * wm_interm_28_pt4(b,i,j,k)
term(10) = term(10) + r2(vrdav_Rl, a,j,q,i) * wm_interm_12_pt4(b,a,p,k) * wm_interm_29_pt4(b,i,j,k)
term(11) = term(11) + r2(vrdav_Rl, a,j,q,i) * wm_interm_12_pt4(b,a,p,k) * wm_interm_28_pt4(b,i,j,k)
term(12) = term(12) + t2(a,q,j,i) * wm_interm_42_pt4(b,i,j,k) * wm_interm_59_pt4(b,a,p,k)
term(13) = term(13) + t2(a,q,j,i) * wm_interm_42_pt4(b,i,j,k) * wm_interm_60_pt4(b,a,p,k)
term(14) = term(14) + t2(a,q,j,i) * wm_interm_42_pt4(b,i,j,k) * wm_interm_62_pt4(b,a,p,k)
term(15) = term(15) + t2(a,q,j,i) * wm_interm_42_pt4(b,i,j,k) * wm_interm_61_pt4(b,a,p,k)
term(16) = term(16) + t2(a,q,j,i) * wm_interm_43_pt4(b,i,j,k) * wm_interm_59_pt4(b,a,p,k)
term(17) = term(17) + t2(a,q,j,i) * wm_interm_43_pt4(b,i,j,k) * wm_interm_60_pt4(b,a,p,k)
term(18) = term(18) + t2(a,q,j,i) * wm_interm_43_pt4(b,i,j,k) * wm_interm_61_pt4(b,a,p,k)
term(19) = term(19) + t2(a,q,j,i) * wm_interm_43_pt4(b,i,j,k) * wm_interm_62_pt4(b,a,p,k)
end do 
end do 
end do 
end do 
end do 

term(4) = term(4) * (2.0d+0) 
term(5) = term(5) * (-4.0d+0) 
term(6) = term(6) * (2.0d+0) 
term(7) = term(7) * (-4.0d+0) 
term(8) = term(8) * (2.0d+0) 
term(9) = term(9) * (-4.0d+0) 
term(10) = term(10) * (-4.0d+0) 
term(11) = term(11) * (8.0d+0) 
term(12) = term(12) * (2.0d+0) 
term(13) = term(13) * (-4.0d+0) 
term(14) = term(14) * (-1.0d+0) 
term(15) = term(15) * (2.0d+0) 
term(16) = term(16) * (-1.0d+0) 
term(17) = term(17) * (2.0d+0) 
term(18) = term(18) * (-1.0d+0) 
term(19) = term(19) * (2.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(20) = term(20) + r2(vrdav_Rl, a,j,q,i) * wm_interm_10_pt4(k,p,i,j) * wm_interm_31_pt4(a,k)
term(21) = term(21) + r2(vrdav_Rl, a,j,q,i) * wm_interm_10_pt4(p,k,i,j) * wm_interm_31_pt4(a,k)
term(22) = term(22) + r2(vrdav_Rl, a,j,q,i) * wm_interm_10_pt4(k,p,i,j) * wm_interm_34_pt4(a,k)
term(23) = term(23) + r2(vrdav_Rl, a,j,q,i) * wm_interm_10_pt4(p,k,i,j) * wm_interm_34_pt4(a,k)
term(24) = term(24) + s1(a,i) * wm_interm_10_pt4(j,p,k,i) * wm_interm_58_pt4(q,a,k,j)
term(25) = term(25) + s1(a,i) * wm_interm_10_pt4(j,p,k,i) * wm_interm_57_pt4(q,a,k,j)
term(26) = term(26) + s1(a,i) * wm_interm_10_pt4(p,j,k,i) * wm_interm_57_pt4(q,a,k,j)
term(27) = term(27) + s1(a,i) * wm_interm_10_pt4(p,j,k,i) * wm_interm_58_pt4(q,a,k,j)
term(28) = term(28) + s1(a,i) * wm_interm_10_pt4(j,p,k,i) * wm_interm_64_pt4(q,a,k,j)
term(29) = term(29) + s1(a,i) * wm_interm_10_pt4(j,p,k,i) * wm_interm_65_pt4(q,a,k,j)
term(30) = term(30) + s1(a,i) * wm_interm_10_pt4(p,j,k,i) * wm_interm_64_pt4(q,a,k,j)
term(31) = term(31) + s1(a,i) * wm_interm_10_pt4(p,j,k,i) * wm_interm_65_pt4(q,a,k,j)
term(32) = term(32) + t2(a,q,j,i) * wm_interm_44_pt4(a,k) * wm_interm_72_pt4(k,p,i,j)
term(33) = term(33) + t2(a,q,j,i) * wm_interm_45_pt4(a,k) * wm_interm_72_pt4(k,p,i,j)
end do 
end do 
end do 
end do 

term(20) = term(20) * (-2.0d+0) 
term(21) = term(21) * (4.0d+0) 
term(23) = term(23) * (-2.0d+0) 
term(24) = term(24) * (4.0d+0) 
term(25) = term(25) * (-8.0d+0) 
term(26) = term(26) * (4.0d+0) 
term(27) = term(27) * (-8.0d+0) 
term(28) = term(28) * (4.0d+0) 
term(29) = term(29) * (-8.0d+0) 
term(30) = term(30) * (-8.0d+0) 
term(31) = term(31) * (16.0d+0) 
term(32) = term(32) * (4.0d+0) 
term(33) = term(33) * (-2.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(34) = term(34) + r2(vrdav_Rl, a,j,q,i) * wm_interm_10_pt4(p,k,j,i) * wm_interm_31_pt4(a,k)
term(35) = term(35) + r2(vrdav_Rl, a,j,q,i) * wm_interm_10_pt4(k,p,j,i) * wm_interm_31_pt4(a,k)
term(36) = term(36) + r2(vrdav_Rl, a,j,q,i) * wm_interm_10_pt4(p,k,j,i) * wm_interm_34_pt4(a,k)
term(37) = term(37) + r2(vrdav_Rl, a,j,q,i) * wm_interm_10_pt4(k,p,j,i) * wm_interm_34_pt4(a,k)
term(38) = term(38) + t2(a,q,j,i) * wm_interm_44_pt4(a,k) * wm_interm_72_pt4(k,p,j,i)
term(39) = term(39) + t2(a,q,j,i) * wm_interm_45_pt4(a,k) * wm_interm_72_pt4(k,p,j,i)
end do 
end do 
end do 
end do 

term(34) = term(34) * (-2.0d+0) 
term(35) = term(35) * (4.0d+0) 
term(37) = term(37) * (-2.0d+0) 
term(38) = term(38) * (-8.0d+0) 
term(39) = term(39) * (4.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
do j = 1, nocc 
do b = nocc + 1, nactive 
term(40) = term(40) + r2(vrdav_Rl, a,j,q,i) * wm_interm_1_pt4(b,a,p,k) * wm_interm_28_pt4(b,j,i,k)
term(41) = term(41) + r2(vrdav_Rl, a,j,q,i) * wm_interm_1_pt4(b,a,p,k) * wm_interm_29_pt4(b,j,i,k)
term(42) = term(42) + r2(vrdav_Rl, a,j,q,i) * wm_interm_29_pt4(b,j,i,k) * wm_interm_5_pt4(b,a,p,k)
term(43) = term(43) + r2(vrdav_Rl, a,j,q,i) * wm_interm_28_pt4(b,j,i,k) * wm_interm_5_pt4(b,a,p,k)
term(44) = term(44) + r2(vrdav_Rl, a,j,q,i) * wm_interm_11_pt4(b,a,p,k) * wm_interm_28_pt4(b,j,i,k)
term(45) = term(45) + r2(vrdav_Rl, a,j,q,i) * wm_interm_11_pt4(b,a,p,k) * wm_interm_29_pt4(b,j,i,k)
term(46) = term(46) + r2(vrdav_Rl, a,j,q,i) * wm_interm_12_pt4(b,a,p,k) * wm_interm_28_pt4(b,j,i,k)
term(47) = term(47) + r2(vrdav_Rl, a,j,q,i) * wm_interm_12_pt4(b,a,p,k) * wm_interm_29_pt4(b,j,i,k)
term(48) = term(48) + t2(a,q,j,i) * wm_interm_42_pt4(b,j,i,k) * wm_interm_59_pt4(b,a,p,k)
term(49) = term(49) + t2(a,q,j,i) * wm_interm_42_pt4(b,j,i,k) * wm_interm_60_pt4(b,a,p,k)
term(50) = term(50) + t2(a,q,j,i) * wm_interm_42_pt4(b,j,i,k) * wm_interm_61_pt4(b,a,p,k)
term(51) = term(51) + t2(a,q,j,i) * wm_interm_42_pt4(b,j,i,k) * wm_interm_62_pt4(b,a,p,k)
term(52) = term(52) + t2(a,q,j,i) * wm_interm_43_pt4(b,j,i,k) * wm_interm_59_pt4(b,a,p,k)
term(53) = term(53) + t2(a,q,j,i) * wm_interm_43_pt4(b,j,i,k) * wm_interm_60_pt4(b,a,p,k)
term(54) = term(54) + t2(a,q,j,i) * wm_interm_43_pt4(b,j,i,k) * wm_interm_62_pt4(b,a,p,k)
term(55) = term(55) + t2(a,q,j,i) * wm_interm_43_pt4(b,j,i,k) * wm_interm_61_pt4(b,a,p,k)
end do 
end do 
end do 
end do 
end do 

term(40) = term(40) * (2.0d+0) 
term(41) = term(41) * (-4.0d+0) 
term(42) = term(42) * (2.0d+0) 
term(43) = term(43) * (-4.0d+0) 
term(44) = term(44) * (2.0d+0) 
term(45) = term(45) * (-4.0d+0) 
term(46) = term(46) * (-4.0d+0) 
term(47) = term(47) * (8.0d+0) 
term(48) = term(48) * (-1.0d+0) 
term(49) = term(49) * (2.0d+0) 
term(50) = term(50) * (-1.0d+0) 
term(51) = term(51) * (2.0d+0) 
term(52) = term(52) * (2.0d+0) 
term(53) = term(53) * (-4.0d+0) 
term(54) = term(54) * (-1.0d+0) 
term(55) = term(55) * (2.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(56) = term(56) + r2(vrdav_Rl, a,j,q,i) * wm_interm_72_pt4(i,j,k,p) * wm_interm_8_pt4(a,k)
term(57) = term(57) + r2(vrdav_Rl, a,j,q,i) * wm_interm_72_pt4(i,j,k,p) * wm_interm_7_pt4(a,k)
term(58) = term(58) + r2(vrdav_Rl, a,j,q,i) * wm_interm_72_pt4(j,i,k,p) * wm_interm_8_pt4(a,k)
term(59) = term(59) + r2(vrdav_Rl, a,j,q,i) * wm_interm_72_pt4(j,i,k,p) * wm_interm_7_pt4(a,k)
end do 
end do 
end do 
end do 

term(56) = term(56) * (2.0d+0) 
term(57) = term(57) * (-4.0d+0) 
term(58) = term(58) * (-4.0d+0) 
term(59) = term(59) * (8.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(60) = term(60) + r2(vrdav_Rl, a,j,q,i) * wm_interm_72_pt4(i,j,p,k) * wm_interm_8_pt4(a,k)
term(61) = term(61) + r2(vrdav_Rl, a,j,q,i) * wm_interm_72_pt4(i,j,p,k) * wm_interm_7_pt4(a,k)
term(62) = term(62) + r2(vrdav_Rl, a,j,q,i) * wm_interm_72_pt4(j,i,p,k) * wm_interm_8_pt4(a,k)
term(63) = term(63) + r2(vrdav_Rl, a,j,q,i) * wm_interm_72_pt4(j,i,p,k) * wm_interm_7_pt4(a,k)
term(64) = term(64) + t1(a,i) * wm_interm_49_pt4(a,q,j,k) * wm_interm_72_pt4(j,p,k,i)
term(65) = term(65) + t1(a,i) * wm_interm_50_pt4(a,q,j,k) * wm_interm_72_pt4(j,p,k,i)
term(66) = term(66) + t1(a,i) * wm_interm_53_pt4(a,q,j,k) * wm_interm_72_pt4(j,p,k,i)
term(67) = term(67) + t1(a,i) * wm_interm_48_pt4(a,q,j,k) * wm_interm_72_pt4(j,p,k,i)
end do 
end do 
end do 
end do 

term(60) = term(60) * (-4.0d+0) 
term(61) = term(61) * (8.0d+0) 
term(62) = term(62) * (2.0d+0) 
term(63) = term(63) * (-4.0d+0) 
term(64) = term(64) * (-2.0d+0) 
term(65) = term(65) * (4.0d+0) 
term(66) = term(66) * (-2.0d+0) 
term(67) = term(67) * (4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(68) = term(68) + s1(a,i) * wm_interm_48_pt4(q,a,j,k) * wm_interm_72_pt4(j,i,k,p)
term(69) = term(69) + s1(a,i) * wm_interm_48_pt4(q,a,j,k) * wm_interm_72_pt4(j,i,p,k)
term(70) = term(70) + s1(a,i) * wm_interm_49_pt4(q,a,j,k) * wm_interm_72_pt4(j,i,k,p)
term(71) = term(71) + s1(a,i) * wm_interm_49_pt4(q,a,j,k) * wm_interm_72_pt4(j,i,p,k)
term(72) = term(72) + s1(a,i) * wm_interm_53_pt4(q,a,j,k) * wm_interm_72_pt4(j,i,k,p)
term(73) = term(73) + s1(a,i) * wm_interm_53_pt4(q,a,j,k) * wm_interm_72_pt4(j,i,p,k)
term(74) = term(74) + s1(a,i) * wm_interm_50_pt4(q,a,j,k) * wm_interm_72_pt4(j,i,p,k)
term(75) = term(75) + s1(a,i) * wm_interm_50_pt4(q,a,j,k) * wm_interm_72_pt4(j,i,k,p)
end do 
end do 
end do 
end do 

term(68) = term(68) * (-8.0d+0) 
term(69) = term(69) * (16.0d+0) 
term(70) = term(70) * (4.0d+0) 
term(71) = term(71) * (-8.0d+0) 
term(72) = term(72) * (4.0d+0) 
term(73) = term(73) * (-8.0d+0) 
term(74) = term(74) * (4.0d+0) 
term(75) = term(75) * (-8.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
term(76) = term(76) + r1(vrdav_Rl, q,j) * wm_interm_18_pt4(i,j) * wm_interm_9_pt4(i,p)
term(77) = term(77) + r1(vrdav_Rl, q,j) * wm_interm_19_pt4(i,j) * wm_interm_9_pt4(i,p)
term(78) = term(78) + t1(q,j) * wm_interm_18_pt4(j,i) * wm_interm_6_pt4(i,p)
term(79) = term(79) + t1(q,j) * wm_interm_19_pt4(j,i) * wm_interm_6_pt4(i,p)
term(80) = term(80) + t1(q,j) * wm_interm_18_pt4(j,i) * wm_interm_66_pt4(i,p)
term(81) = term(81) + t1(q,j) * wm_interm_18_pt4(j,i) * wm_interm_67_pt4(i,p)
term(82) = term(82) + t1(q,j) * wm_interm_19_pt4(j,i) * wm_interm_66_pt4(i,p)
term(83) = term(83) + t1(q,j) * wm_interm_19_pt4(j,i) * wm_interm_67_pt4(i,p)
term(84) = term(84) + t1(q,j) * wm_interm_54_pt4(j,i) * wm_interm_70_pt4(p,i)
term(85) = term(85) + t1(q,j) * wm_interm_54_pt4(j,i) * wm_interm_71_pt4(p,i)
term(86) = term(86) + t1(q,j) * wm_interm_56_pt4(j,i) * wm_interm_70_pt4(p,i)
term(87) = term(87) + t1(q,j) * wm_interm_56_pt4(j,i) * wm_interm_71_pt4(p,i)
end do 
end do 

term(76) = term(76) * (2.0d+0) 
term(77) = term(77) * (-4.0d+0) 
term(78) = term(78) * (2.0d+0) 
term(79) = term(79) * (-4.0d+0) 
term(80) = term(80) * (-2.0d+0) 
term(81) = term(81) * (4.0d+0) 
term(82) = term(82) * (4.0d+0) 
term(83) = term(83) * (-8.0d+0) 
term(84) = term(84) * (-2.0d+0) 
term(85) = term(85) * (4.0d+0) 
term(86) = term(86) * (4.0d+0) 
term(87) = term(87) * (-8.0d+0) 

do a = nocc + 1, nactive 
do i = 1, nocc 
term(88) = term(88) + s1(a,i) * wm_interm_51_pt4(q,a) * wm_interm_70_pt4(i,p)
term(89) = term(89) + s1(a,i) * wm_interm_52_pt4(q,a) * wm_interm_70_pt4(i,p)
term(90) = term(90) + s1(a,i) * wm_interm_51_pt4(q,a) * wm_interm_71_pt4(i,p)
term(91) = term(91) + s1(a,i) * wm_interm_52_pt4(q,a) * wm_interm_71_pt4(i,p)
end do 
end do 

term(88) = term(88) * (4.0d+0) 
term(89) = term(89) * (-2.0d+0) 
term(90) = term(90) * (-8.0d+0) 
term(91) = term(91) * (4.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(92) = term(92) + s1(a,i) * wm_interm_48_pt4(q,b,j,i) * wm_interm_59_pt4(a,b,j,p)
term(93) = term(93) + s1(a,i) * wm_interm_48_pt4(q,b,j,i) * wm_interm_60_pt4(a,b,j,p)
term(94) = term(94) + s1(a,i) * wm_interm_48_pt4(q,b,j,i) * wm_interm_61_pt4(a,b,j,p)
term(95) = term(95) + s1(a,i) * wm_interm_48_pt4(q,b,j,i) * wm_interm_62_pt4(a,b,j,p)
term(96) = term(96) + s1(a,i) * wm_interm_49_pt4(q,b,j,i) * wm_interm_59_pt4(a,b,j,p)
term(97) = term(97) + s1(a,i) * wm_interm_49_pt4(q,b,j,i) * wm_interm_60_pt4(a,b,j,p)
term(98) = term(98) + s1(a,i) * wm_interm_49_pt4(q,b,j,i) * wm_interm_61_pt4(a,b,j,p)
term(99) = term(99) + s1(a,i) * wm_interm_49_pt4(q,b,j,i) * wm_interm_62_pt4(a,b,j,p)
term(100) = term(100) + r2(vrdav_Rl, a,j,q,i) * wm_interm_3_pt4(b,p,j,i) * wm_interm_68_pt4(a,b)
term(101) = term(101) + r2(vrdav_Rl, a,j,q,i) * wm_interm_3_pt4(b,p,j,i) * wm_interm_69_pt4(a,b)
term(102) = term(102) + s1(a,i) * wm_interm_53_pt4(q,b,j,i) * wm_interm_59_pt4(a,b,j,p)
term(103) = term(103) + s1(a,i) * wm_interm_53_pt4(q,b,j,i) * wm_interm_60_pt4(a,b,j,p)
term(104) = term(104) + s1(a,i) * wm_interm_53_pt4(q,b,j,i) * wm_interm_61_pt4(a,b,j,p)
term(105) = term(105) + s1(a,i) * wm_interm_53_pt4(q,b,j,i) * wm_interm_62_pt4(a,b,j,p)
term(106) = term(106) + s1(a,i) * wm_interm_50_pt4(q,b,j,i) * wm_interm_62_pt4(a,b,j,p)
term(107) = term(107) + s1(a,i) * wm_interm_50_pt4(q,b,j,i) * wm_interm_61_pt4(a,b,j,p)
term(108) = term(108) + s1(a,i) * wm_interm_50_pt4(q,b,j,i) * wm_interm_59_pt4(a,b,j,p)
term(109) = term(109) + s1(a,i) * wm_interm_50_pt4(q,b,j,i) * wm_interm_60_pt4(a,b,j,p)
end do 
end do 
end do 
end do 

term(92) = term(92) * (-8.0d+0) 
term(93) = term(93) * (16.0d+0) 
term(94) = term(94) * (-8.0d+0) 
term(95) = term(95) * (16.0d+0) 
term(96) = term(96) * (4.0d+0) 
term(97) = term(97) * (-8.0d+0) 
term(98) = term(98) * (4.0d+0) 
term(99) = term(99) * (-8.0d+0) 
term(100) = term(100) * (4.0d+0) 
term(101) = term(101) * (-8.0d+0) 
term(102) = term(102) * (4.0d+0) 
term(103) = term(103) * (-8.0d+0) 
term(104) = term(104) * (4.0d+0) 
term(105) = term(105) * (-8.0d+0) 
term(106) = term(106) * (4.0d+0) 
term(107) = term(107) * (-8.0d+0) 
term(108) = term(108) * (-8.0d+0) 
term(109) = term(109) * (16.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(110) = term(110) + s1(a,p) * wm_interm_1_pt4(b,a,i,j) * wm_interm_57_pt4(q,b,j,i)
term(111) = term(111) + s1(a,p) * wm_interm_1_pt4(b,a,i,j) * wm_interm_58_pt4(q,b,j,i)
term(112) = term(112) + s1(a,p) * wm_interm_58_pt4(q,b,i,j) * wm_interm_5_pt4(b,a,j,i)
term(113) = term(113) + s1(a,p) * wm_interm_57_pt4(q,b,i,j) * wm_interm_5_pt4(b,a,j,i)
term(114) = term(114) + s1(a,i) * wm_interm_1_pt4(b,a,p,j) * wm_interm_58_pt4(q,b,j,i)
term(115) = term(115) + s1(a,i) * wm_interm_1_pt4(b,a,p,j) * wm_interm_57_pt4(q,b,j,i)
term(116) = term(116) + s1(a,i) * wm_interm_57_pt4(q,b,j,i) * wm_interm_5_pt4(b,a,p,j)
term(117) = term(117) + s1(a,i) * wm_interm_58_pt4(q,b,j,i) * wm_interm_5_pt4(b,a,p,j)
term(118) = term(118) + s1(a,i) * wm_interm_1_pt4(b,a,p,j) * wm_interm_64_pt4(q,b,j,i)
term(119) = term(119) + s1(a,i) * wm_interm_1_pt4(b,a,p,j) * wm_interm_65_pt4(q,b,j,i)
term(120) = term(120) + s1(a,p) * wm_interm_1_pt4(b,a,i,j) * wm_interm_64_pt4(q,b,j,i)
term(121) = term(121) + s1(a,p) * wm_interm_1_pt4(b,a,i,j) * wm_interm_65_pt4(q,b,j,i)
term(122) = term(122) + s1(a,p) * wm_interm_5_pt4(b,a,i,j) * wm_interm_64_pt4(q,b,j,i)
term(123) = term(123) + s1(a,p) * wm_interm_5_pt4(b,a,i,j) * wm_interm_65_pt4(q,b,j,i)
term(124) = term(124) + s1(a,i) * wm_interm_5_pt4(b,a,p,j) * wm_interm_64_pt4(q,b,j,i)
term(125) = term(125) + s1(a,i) * wm_interm_5_pt4(b,a,p,j) * wm_interm_65_pt4(q,b,j,i)
term(126) = term(126) + s1(a,p) * wm_interm_11_pt4(b,a,i,j) * wm_interm_57_pt4(q,b,j,i)
term(127) = term(127) + s1(a,p) * wm_interm_11_pt4(b,a,i,j) * wm_interm_58_pt4(q,b,j,i)
term(128) = term(128) + s1(a,p) * wm_interm_12_pt4(b,a,i,j) * wm_interm_57_pt4(q,b,j,i)
term(129) = term(129) + s1(a,p) * wm_interm_12_pt4(b,a,i,j) * wm_interm_58_pt4(q,b,j,i)
term(130) = term(130) + s1(a,i) * wm_interm_11_pt4(b,a,p,j) * wm_interm_58_pt4(q,b,j,i)
term(131) = term(131) + s1(a,i) * wm_interm_11_pt4(b,a,p,j) * wm_interm_57_pt4(q,b,j,i)
term(132) = term(132) + s1(a,i) * wm_interm_12_pt4(b,a,p,j) * wm_interm_58_pt4(q,b,j,i)
term(133) = term(133) + s1(a,i) * wm_interm_12_pt4(b,a,p,j) * wm_interm_57_pt4(q,b,j,i)
term(134) = term(134) + s1(a,i) * wm_interm_11_pt4(b,a,p,j) * wm_interm_64_pt4(q,b,j,i)
term(135) = term(135) + s1(a,i) * wm_interm_11_pt4(b,a,p,j) * wm_interm_65_pt4(q,b,j,i)
term(136) = term(136) + s1(a,p) * wm_interm_11_pt4(b,a,i,j) * wm_interm_64_pt4(q,b,j,i)
term(137) = term(137) + s1(a,p) * wm_interm_11_pt4(b,a,i,j) * wm_interm_65_pt4(q,b,j,i)
term(138) = term(138) + s1(a,i) * wm_interm_12_pt4(b,a,p,j) * wm_interm_64_pt4(q,b,j,i)
term(139) = term(139) + s1(a,i) * wm_interm_12_pt4(b,a,p,j) * wm_interm_65_pt4(q,b,j,i)
term(140) = term(140) + s1(a,p) * wm_interm_12_pt4(b,a,i,j) * wm_interm_64_pt4(q,b,j,i)
term(141) = term(141) + s1(a,p) * wm_interm_12_pt4(b,a,i,j) * wm_interm_65_pt4(q,b,j,i)
term(142) = term(142) + r2(vrdav_Rl, a,j,q,i) * wm_interm_3_pt4(b,p,i,j) * wm_interm_68_pt4(a,b)
term(143) = term(143) + r2(vrdav_Rl, a,j,q,i) * wm_interm_3_pt4(b,p,i,j) * wm_interm_69_pt4(a,b)
term(144) = term(144) + s1(a,p) * wm_interm_48_pt4(q,b,i,j) * wm_interm_62_pt4(a,b,i,j)
term(145) = term(145) + s1(a,p) * wm_interm_48_pt4(q,b,i,j) * wm_interm_61_pt4(a,b,i,j)
term(146) = term(146) + s1(a,p) * wm_interm_48_pt4(q,b,i,j) * wm_interm_59_pt4(a,b,i,j)
term(147) = term(147) + s1(a,p) * wm_interm_48_pt4(q,b,i,j) * wm_interm_60_pt4(a,b,i,j)
term(148) = term(148) + s1(a,p) * wm_interm_49_pt4(q,b,i,j) * wm_interm_62_pt4(a,b,i,j)
term(149) = term(149) + s1(a,p) * wm_interm_49_pt4(q,b,i,j) * wm_interm_61_pt4(a,b,i,j)
term(150) = term(150) + s1(a,p) * wm_interm_49_pt4(q,b,i,j) * wm_interm_59_pt4(a,b,i,j)
term(151) = term(151) + s1(a,p) * wm_interm_49_pt4(q,b,i,j) * wm_interm_60_pt4(a,b,i,j)
term(152) = term(152) + s1(a,p) * wm_interm_50_pt4(q,b,i,j) * wm_interm_59_pt4(a,b,i,j)
term(153) = term(153) + s1(a,p) * wm_interm_50_pt4(q,b,i,j) * wm_interm_60_pt4(a,b,i,j)
term(154) = term(154) + s1(a,p) * wm_interm_50_pt4(q,b,i,j) * wm_interm_61_pt4(a,b,i,j)
term(155) = term(155) + s1(a,p) * wm_interm_50_pt4(q,b,i,j) * wm_interm_62_pt4(a,b,i,j)
term(156) = term(156) + s1(a,p) * wm_interm_53_pt4(q,b,i,j) * wm_interm_62_pt4(a,b,i,j)
term(157) = term(157) + s1(a,p) * wm_interm_53_pt4(q,b,i,j) * wm_interm_61_pt4(a,b,i,j)
term(158) = term(158) + s1(a,p) * wm_interm_53_pt4(q,b,i,j) * wm_interm_59_pt4(a,b,i,j)
term(159) = term(159) + s1(a,p) * wm_interm_53_pt4(q,b,i,j) * wm_interm_60_pt4(a,b,i,j)
term(160) = term(160) + t1(q,i) * wm_interm_48_pt4(a,b,i,j) * wm_interm_62_pt4(a,b,p,j)
term(161) = term(161) + t1(q,i) * wm_interm_48_pt4(a,b,i,j) * wm_interm_61_pt4(a,b,p,j)
term(162) = term(162) + t1(q,i) * wm_interm_48_pt4(a,b,i,j) * wm_interm_59_pt4(a,b,p,j)
term(163) = term(163) + t1(q,i) * wm_interm_48_pt4(a,b,i,j) * wm_interm_60_pt4(a,b,p,j)
term(164) = term(164) + t1(q,i) * wm_interm_11_pt4(a,b,i,j) * wm_interm_64_pt4(b,a,j,p)
term(165) = term(165) + t1(q,i) * wm_interm_11_pt4(a,b,i,j) * wm_interm_65_pt4(b,a,j,p)
term(166) = term(166) + t1(q,i) * wm_interm_12_pt4(a,b,i,j) * wm_interm_64_pt4(b,a,j,p)
term(167) = term(167) + t1(q,i) * wm_interm_12_pt4(a,b,i,j) * wm_interm_65_pt4(b,a,j,p)
term(168) = term(168) + t1(q,i) * wm_interm_49_pt4(a,b,i,j) * wm_interm_62_pt4(a,b,p,j)
term(169) = term(169) + t1(q,i) * wm_interm_49_pt4(a,b,i,j) * wm_interm_61_pt4(a,b,p,j)
term(170) = term(170) + t1(q,i) * wm_interm_49_pt4(a,b,i,j) * wm_interm_59_pt4(a,b,p,j)
term(171) = term(171) + t1(q,i) * wm_interm_49_pt4(a,b,i,j) * wm_interm_60_pt4(a,b,p,j)
term(172) = term(172) + t1(q,i) * wm_interm_11_pt4(a,b,i,j) * wm_interm_57_pt4(b,a,j,p)
term(173) = term(173) + t1(q,i) * wm_interm_11_pt4(a,b,i,j) * wm_interm_58_pt4(b,a,j,p)
term(174) = term(174) + t1(q,i) * wm_interm_12_pt4(a,b,i,j) * wm_interm_57_pt4(b,a,j,p)
term(175) = term(175) + t1(q,i) * wm_interm_12_pt4(a,b,i,j) * wm_interm_58_pt4(b,a,j,p)
term(176) = term(176) + t1(q,i) * wm_interm_50_pt4(a,b,i,j) * wm_interm_59_pt4(a,b,p,j)
term(177) = term(177) + t1(q,i) * wm_interm_50_pt4(a,b,i,j) * wm_interm_60_pt4(a,b,p,j)
term(178) = term(178) + t1(q,i) * wm_interm_1_pt4(a,b,i,j) * wm_interm_57_pt4(b,a,j,p)
term(179) = term(179) + t1(q,i) * wm_interm_1_pt4(a,b,i,j) * wm_interm_58_pt4(b,a,j,p)
term(180) = term(180) + t1(q,i) * wm_interm_58_pt4(a,b,j,p) * wm_interm_5_pt4(b,a,i,j)
term(181) = term(181) + t1(q,i) * wm_interm_57_pt4(a,b,j,p) * wm_interm_5_pt4(b,a,i,j)
term(182) = term(182) + t1(q,i) * wm_interm_50_pt4(a,b,i,j) * wm_interm_61_pt4(a,b,p,j)
term(183) = term(183) + t1(q,i) * wm_interm_50_pt4(a,b,i,j) * wm_interm_62_pt4(a,b,p,j)
term(184) = term(184) + t1(q,i) * wm_interm_53_pt4(a,b,i,j) * wm_interm_62_pt4(a,b,p,j)
term(185) = term(185) + t1(q,i) * wm_interm_53_pt4(a,b,i,j) * wm_interm_61_pt4(a,b,p,j)
term(186) = term(186) + t1(q,i) * wm_interm_53_pt4(a,b,i,j) * wm_interm_59_pt4(a,b,p,j)
term(187) = term(187) + t1(q,i) * wm_interm_53_pt4(a,b,i,j) * wm_interm_60_pt4(a,b,p,j)
term(188) = term(188) + t1(q,i) * wm_interm_5_pt4(a,b,i,j) * wm_interm_64_pt4(b,a,j,p)
term(189) = term(189) + t1(q,i) * wm_interm_5_pt4(a,b,i,j) * wm_interm_65_pt4(b,a,j,p)
term(190) = term(190) + t1(q,i) * wm_interm_1_pt4(a,b,i,j) * wm_interm_64_pt4(b,a,j,p)
term(191) = term(191) + t1(q,i) * wm_interm_1_pt4(a,b,i,j) * wm_interm_65_pt4(b,a,j,p)
term(192) = term(192) + t1(a,i) * wm_interm_57_pt4(a,b,j,p) * wm_interm_5_pt4(b,q,i,j)
term(193) = term(193) + t1(a,i) * wm_interm_58_pt4(a,b,j,p) * wm_interm_5_pt4(b,q,i,j)
term(194) = term(194) + t1(a,i) * wm_interm_1_pt4(b,q,i,j) * wm_interm_58_pt4(a,b,j,p)
term(195) = term(195) + t1(a,i) * wm_interm_1_pt4(b,q,i,j) * wm_interm_57_pt4(a,b,j,p)
term(196) = term(196) + t1(a,i) * wm_interm_11_pt4(b,q,i,j) * wm_interm_58_pt4(a,b,j,p)
term(197) = term(197) + t1(a,i) * wm_interm_11_pt4(b,q,i,j) * wm_interm_57_pt4(a,b,j,p)
term(198) = term(198) + t1(a,i) * wm_interm_12_pt4(b,q,i,j) * wm_interm_58_pt4(a,b,j,p)
term(199) = term(199) + t1(a,i) * wm_interm_12_pt4(b,q,i,j) * wm_interm_57_pt4(a,b,j,p)
term(200) = term(200) + t1(a,i) * wm_interm_11_pt4(b,q,i,j) * wm_interm_64_pt4(a,b,j,p)
term(201) = term(201) + t1(a,i) * wm_interm_11_pt4(b,q,i,j) * wm_interm_65_pt4(a,b,j,p)
term(202) = term(202) + t1(a,i) * wm_interm_5_pt4(b,q,i,j) * wm_interm_64_pt4(a,b,j,p)
term(203) = term(203) + t1(a,i) * wm_interm_5_pt4(b,q,i,j) * wm_interm_65_pt4(a,b,j,p)
term(204) = term(204) + t1(a,i) * wm_interm_1_pt4(b,q,i,j) * wm_interm_64_pt4(a,b,j,p)
term(205) = term(205) + t1(a,i) * wm_interm_1_pt4(b,q,i,j) * wm_interm_65_pt4(a,b,j,p)
term(206) = term(206) + t1(a,i) * wm_interm_12_pt4(b,q,i,j) * wm_interm_64_pt4(a,b,j,p)
term(207) = term(207) + t1(a,i) * wm_interm_12_pt4(b,q,i,j) * wm_interm_65_pt4(a,b,j,p)
end do 
end do 
end do 
end do 

term(110) = term(110) * (4.0d+0) 
term(111) = term(111) * (-8.0d+0) 
term(112) = term(112) * (4.0d+0) 
term(113) = term(113) * (-8.0d+0) 
term(114) = term(114) * (4.0d+0) 
term(115) = term(115) * (-8.0d+0) 
term(116) = term(116) * (4.0d+0) 
term(117) = term(117) * (-8.0d+0) 
term(118) = term(118) * (4.0d+0) 
term(119) = term(119) * (-8.0d+0) 
term(120) = term(120) * (-8.0d+0) 
term(121) = term(121) * (16.0d+0) 
term(122) = term(122) * (4.0d+0) 
term(123) = term(123) * (-8.0d+0) 
term(124) = term(124) * (-8.0d+0) 
term(125) = term(125) * (16.0d+0) 
term(126) = term(126) * (4.0d+0) 
term(127) = term(127) * (-8.0d+0) 
term(128) = term(128) * (-8.0d+0) 
term(129) = term(129) * (16.0d+0) 
term(130) = term(130) * (4.0d+0) 
term(131) = term(131) * (-8.0d+0) 
term(132) = term(132) * (-8.0d+0) 
term(133) = term(133) * (16.0d+0) 
term(134) = term(134) * (4.0d+0) 
term(135) = term(135) * (-8.0d+0) 
term(136) = term(136) * (-8.0d+0) 
term(137) = term(137) * (16.0d+0) 
term(138) = term(138) * (-8.0d+0) 
term(139) = term(139) * (16.0d+0) 
term(140) = term(140) * (16.0d+0) 
term(141) = term(141) * (-32.0d+0) 
term(142) = term(142) * (-8.0d+0) 
term(143) = term(143) * (16.0d+0) 
term(144) = term(144) * (4.0d+0) 
term(145) = term(145) * (-8.0d+0) 
term(146) = term(146) * (-8.0d+0) 
term(147) = term(147) * (16.0d+0) 
term(148) = term(148) * (-2.0d+0) 
term(149) = term(149) * (4.0d+0) 
term(150) = term(150) * (4.0d+0) 
term(151) = term(151) * (-8.0d+0) 
term(152) = term(152) * (-2.0d+0) 
term(153) = term(153) * (4.0d+0) 
term(154) = term(154) * (-2.0d+0) 
term(155) = term(155) * (4.0d+0) 
term(156) = term(156) * (-2.0d+0) 
term(157) = term(157) * (4.0d+0) 
term(158) = term(158) * (4.0d+0) 
term(159) = term(159) * (-8.0d+0) 
term(160) = term(160) * (4.0d+0) 
term(161) = term(161) * (-8.0d+0) 
term(162) = term(162) * (-8.0d+0) 
term(163) = term(163) * (16.0d+0) 
term(164) = term(164) * (4.0d+0) 
term(165) = term(165) * (-8.0d+0) 
term(166) = term(166) * (-8.0d+0) 
term(167) = term(167) * (16.0d+0) 
term(168) = term(168) * (-2.0d+0) 
term(169) = term(169) * (4.0d+0) 
term(170) = term(170) * (4.0d+0) 
term(171) = term(171) * (-8.0d+0) 
term(172) = term(172) * (-2.0d+0) 
term(173) = term(173) * (4.0d+0) 
term(174) = term(174) * (4.0d+0) 
term(175) = term(175) * (-8.0d+0) 
term(176) = term(176) * (-2.0d+0) 
term(177) = term(177) * (4.0d+0) 
term(178) = term(178) * (-2.0d+0) 
term(179) = term(179) * (4.0d+0) 
term(180) = term(180) * (-2.0d+0) 
term(181) = term(181) * (4.0d+0) 
term(182) = term(182) * (-2.0d+0) 
term(183) = term(183) * (4.0d+0) 
term(184) = term(184) * (-2.0d+0) 
term(185) = term(185) * (4.0d+0) 
term(186) = term(186) * (4.0d+0) 
term(187) = term(187) * (-8.0d+0) 
term(188) = term(188) * (-2.0d+0) 
term(189) = term(189) * (4.0d+0) 
term(190) = term(190) * (4.0d+0) 
term(191) = term(191) * (-8.0d+0) 
term(192) = term(192) * (-2.0d+0) 
term(193) = term(193) * (4.0d+0) 
term(194) = term(194) * (-2.0d+0) 
term(195) = term(195) * (4.0d+0) 
term(196) = term(196) * (-2.0d+0) 
term(197) = term(197) * (4.0d+0) 
term(198) = term(198) * (4.0d+0) 
term(199) = term(199) * (-8.0d+0) 
term(200) = term(200) * (-2.0d+0) 
term(201) = term(201) * (4.0d+0) 
term(202) = term(202) * (4.0d+0) 
term(203) = term(203) * (-8.0d+0) 
term(204) = term(204) * (-2.0d+0) 
term(205) = term(205) * (4.0d+0) 
term(206) = term(206) * (4.0d+0) 
term(207) = term(207) * (-8.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(208) = term(208) + r2(vrdav_Rl, a,j,q,i) * wm_interm_3_pt4(b,k,j,i) * wm_interm_62_pt4(a,b,k,p)
term(209) = term(209) + r2(vrdav_Rl, a,j,q,i) * wm_interm_3_pt4(b,k,j,i) * wm_interm_61_pt4(a,b,k,p)
term(210) = term(210) + r2(vrdav_Rl, a,j,q,i) * wm_interm_3_pt4(b,k,j,i) * wm_interm_59_pt4(a,b,k,p)
term(211) = term(211) + r2(vrdav_Rl, a,j,q,i) * wm_interm_3_pt4(b,k,j,i) * wm_interm_60_pt4(a,b,k,p)
end do 
end do 
end do 
end do 
end do 

term(208) = term(208) * (4.0d+0) 
term(209) = term(209) * (-8.0d+0) 
term(210) = term(210) * (-8.0d+0) 
term(211) = term(211) * (16.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(212) = term(212) + t1(a,i) * wm_interm_53_pt4(b,q,i,j) * wm_interm_59_pt4(b,a,p,j)
term(213) = term(213) + t1(a,i) * wm_interm_53_pt4(b,q,i,j) * wm_interm_60_pt4(b,a,p,j)
term(214) = term(214) + t1(a,i) * wm_interm_48_pt4(b,q,i,j) * wm_interm_59_pt4(b,a,p,j)
term(215) = term(215) + t1(a,i) * wm_interm_48_pt4(b,q,i,j) * wm_interm_60_pt4(b,a,p,j)
term(216) = term(216) + t1(a,i) * wm_interm_53_pt4(b,q,i,j) * wm_interm_61_pt4(b,a,p,j)
term(217) = term(217) + t1(a,i) * wm_interm_53_pt4(b,q,i,j) * wm_interm_62_pt4(b,a,p,j)
term(218) = term(218) + t1(a,i) * wm_interm_48_pt4(b,q,i,j) * wm_interm_61_pt4(b,a,p,j)
term(219) = term(219) + t1(a,i) * wm_interm_48_pt4(b,q,i,j) * wm_interm_62_pt4(b,a,p,j)
term(220) = term(220) + t1(a,i) * wm_interm_49_pt4(b,q,i,j) * wm_interm_59_pt4(b,a,p,j)
term(221) = term(221) + t1(a,i) * wm_interm_49_pt4(b,q,i,j) * wm_interm_60_pt4(b,a,p,j)
term(222) = term(222) + t1(a,i) * wm_interm_49_pt4(b,q,i,j) * wm_interm_61_pt4(b,a,p,j)
term(223) = term(223) + t1(a,i) * wm_interm_49_pt4(b,q,i,j) * wm_interm_62_pt4(b,a,p,j)
term(224) = term(224) + t1(a,i) * wm_interm_50_pt4(b,q,i,j) * wm_interm_62_pt4(b,a,p,j)
term(225) = term(225) + t1(a,i) * wm_interm_50_pt4(b,q,i,j) * wm_interm_61_pt4(b,a,p,j)
term(226) = term(226) + t1(a,i) * wm_interm_50_pt4(b,q,i,j) * wm_interm_59_pt4(b,a,p,j)
term(227) = term(227) + t1(a,i) * wm_interm_50_pt4(b,q,i,j) * wm_interm_60_pt4(b,a,p,j)
term(228) = term(228) + t2(a,q,j,i) * wm_interm_44_pt4(b,i) * wm_interm_62_pt4(b,a,p,j)
term(229) = term(229) + t2(a,q,j,i) * wm_interm_44_pt4(b,i) * wm_interm_61_pt4(b,a,p,j)
term(230) = term(230) + t2(a,q,j,i) * wm_interm_44_pt4(b,i) * wm_interm_59_pt4(b,a,p,j)
term(231) = term(231) + t2(a,q,j,i) * wm_interm_44_pt4(b,i) * wm_interm_60_pt4(b,a,p,j)
term(232) = term(232) + t2(a,q,j,i) * wm_interm_45_pt4(b,i) * wm_interm_62_pt4(b,a,p,j)
term(233) = term(233) + t2(a,q,j,i) * wm_interm_45_pt4(b,i) * wm_interm_61_pt4(b,a,p,j)
term(234) = term(234) + t2(a,q,j,i) * wm_interm_45_pt4(b,i) * wm_interm_59_pt4(b,a,p,j)
term(235) = term(235) + t2(a,q,j,i) * wm_interm_45_pt4(b,i) * wm_interm_60_pt4(b,a,p,j)
end do 
end do 
end do 
end do 

term(212) = term(212) * (-2.0d+0) 
term(213) = term(213) * (4.0d+0) 
term(214) = term(214) * (4.0d+0) 
term(215) = term(215) * (-8.0d+0) 
term(216) = term(216) * (-2.0d+0) 
term(217) = term(217) * (4.0d+0) 
term(218) = term(218) * (4.0d+0) 
term(219) = term(219) * (-8.0d+0) 
term(220) = term(220) * (-2.0d+0) 
term(221) = term(221) * (4.0d+0) 
term(222) = term(222) * (-2.0d+0) 
term(223) = term(223) * (4.0d+0) 
term(224) = term(224) * (-2.0d+0) 
term(225) = term(225) * (4.0d+0) 
term(226) = term(226) * (4.0d+0) 
term(227) = term(227) * (-8.0d+0) 
term(228) = term(228) * (4.0d+0) 
term(229) = term(229) * (-8.0d+0) 
term(230) = term(230) * (-8.0d+0) 
term(231) = term(231) * (16.0d+0) 
term(232) = term(232) * (-2.0d+0) 
term(233) = term(233) * (4.0d+0) 
term(234) = term(234) * (4.0d+0) 
term(235) = term(235) * (-8.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
do j = 1, nocc 
do b = nocc + 1, nactive 
term(236) = term(236) + r2(vrdav_Rl, a,j,q,i) * wm_interm_20_pt4(b,a) * wm_interm_29_pt4(b,j,i,p)
term(237) = term(237) + r2(vrdav_Rl, a,j,q,i) * wm_interm_20_pt4(b,a) * wm_interm_28_pt4(b,j,i,p)
term(238) = term(238) + r2(vrdav_Rl, a,j,q,i) * wm_interm_21_pt4(b,a) * wm_interm_29_pt4(b,j,i,p)
term(239) = term(239) + r2(vrdav_Rl, a,j,q,i) * wm_interm_21_pt4(b,a) * wm_interm_28_pt4(b,j,i,p)
term(240) = term(240) + t2(a,q,j,i) * wm_interm_44_pt4(b,j) * wm_interm_61_pt4(b,a,p,i)
term(241) = term(241) + t2(a,q,j,i) * wm_interm_44_pt4(b,j) * wm_interm_62_pt4(b,a,p,i)
term(242) = term(242) + t2(a,q,j,i) * wm_interm_44_pt4(b,j) * wm_interm_59_pt4(b,a,p,i)
term(243) = term(243) + t2(a,q,j,i) * wm_interm_44_pt4(b,j) * wm_interm_60_pt4(b,a,p,i)
term(244) = term(244) + t2(a,q,j,i) * wm_interm_45_pt4(b,j) * wm_interm_61_pt4(b,a,p,i)
term(245) = term(245) + t2(a,q,j,i) * wm_interm_45_pt4(b,j) * wm_interm_62_pt4(b,a,p,i)
term(246) = term(246) + t2(a,q,j,i) * wm_interm_45_pt4(b,j) * wm_interm_59_pt4(b,a,p,i)
term(247) = term(247) + t2(a,q,j,i) * wm_interm_45_pt4(b,j) * wm_interm_60_pt4(b,a,p,i)
end do 
end do 
end do 
end do 

term(236) = term(236) * (-1.0d+0) 
term(237) = term(237) * (2.0d+0) 
term(238) = term(238) * (2.0d+0) 
term(239) = term(239) * (-4.0d+0) 
term(240) = term(240) * (4.0d+0) 
term(241) = term(241) * (-8.0d+0) 
term(242) = term(242) * (4.0d+0) 
term(243) = term(243) * (-8.0d+0) 
term(244) = term(244) * (-2.0d+0) 
term(245) = term(245) * (4.0d+0) 
term(246) = term(246) * (-2.0d+0) 
term(247) = term(247) * (4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(248) = term(248) + r2(vrdav_Rl, a,j,q,i) * wm_interm_3_pt4(b,k,i,j) * wm_interm_59_pt4(a,b,k,p)
term(249) = term(249) + r2(vrdav_Rl, a,j,q,i) * wm_interm_3_pt4(b,k,i,j) * wm_interm_60_pt4(a,b,k,p)
term(250) = term(250) + r2(vrdav_Rl, a,j,q,i) * wm_interm_3_pt4(b,k,i,j) * wm_interm_61_pt4(a,b,k,p)
term(251) = term(251) + r2(vrdav_Rl, a,j,q,i) * wm_interm_3_pt4(b,k,i,j) * wm_interm_62_pt4(a,b,k,p)
end do 
end do 
end do 
end do 
end do 

term(248) = term(248) * (4.0d+0) 
term(249) = term(249) * (-8.0d+0) 
term(250) = term(250) * (4.0d+0) 
term(251) = term(251) * (-8.0d+0) 

do a = nocc + 1, nactive 
term(252) = term(252) + wm_interm_24_pt4(a,q) * wm_interm_31_pt4(a,p)
term(253) = term(253) + wm_interm_24_pt4(a,q) * wm_interm_34_pt4(a,p)
term(254) = term(254) + wm_interm_35_pt4(a,p) * wm_interm_37_pt4(a,q)
term(255) = term(255) + wm_interm_32_pt4(a,p) * wm_interm_37_pt4(a,q)
end do 

term(252) = term(252) * (4.0d+0) 
term(253) = term(253) * (-2.0d+0) 
term(254) = term(254) * (2.0d+0) 
term(255) = term(255) * (-4.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(256) = term(256) + wm_interm_30_pt4(a,q,p,i) * wm_interm_31_pt4(a,i)
term(257) = term(257) + wm_interm_30_pt4(a,q,p,i) * wm_interm_34_pt4(a,i)
term(258) = term(258) + wm_interm_44_pt4(a,i) * wm_interm_46_pt4(a,q,p,i)
term(259) = term(259) + wm_interm_45_pt4(a,i) * wm_interm_46_pt4(a,q,p,i)
term(260) = term(260) + r1(vrdav_Rl, q,i) * wm_interm_4_pt4(a,p) * wm_interm_7_pt4(a,i)
term(261) = term(261) + r1(vrdav_Rl, q,i) * wm_interm_2_pt4(a,p) * wm_interm_7_pt4(a,i)
term(262) = term(262) + r1(vrdav_Rl, q,i) * wm_interm_4_pt4(a,p) * wm_interm_8_pt4(a,i)
term(263) = term(263) + r1(vrdav_Rl, q,i) * wm_interm_2_pt4(a,p) * wm_interm_8_pt4(a,i)
term(264) = term(264) + t1(a,i) * wm_interm_20_pt4(a,q) * wm_interm_6_pt4(i,p)
term(265) = term(265) + t1(a,i) * wm_interm_21_pt4(a,q) * wm_interm_6_pt4(i,p)
term(266) = term(266) + t1(a,i) * wm_interm_20_pt4(a,q) * wm_interm_66_pt4(i,p)
term(267) = term(267) + t1(a,i) * wm_interm_20_pt4(a,q) * wm_interm_67_pt4(i,p)
term(268) = term(268) + t1(a,i) * wm_interm_21_pt4(a,q) * wm_interm_66_pt4(i,p)
term(269) = term(269) + t1(a,i) * wm_interm_21_pt4(a,q) * wm_interm_67_pt4(i,p)
term(270) = term(270) + t1(a,i) * wm_interm_52_pt4(a,q) * wm_interm_70_pt4(p,i)
term(271) = term(271) + t1(a,i) * wm_interm_52_pt4(a,q) * wm_interm_71_pt4(p,i)
term(272) = term(272) + t1(a,i) * wm_interm_51_pt4(a,q) * wm_interm_70_pt4(p,i)
term(273) = term(273) + t1(a,i) * wm_interm_51_pt4(a,q) * wm_interm_71_pt4(p,i)
end do 
end do 

term(256) = term(256) * (4.0d+0) 
term(257) = term(257) * (-2.0d+0) 
term(258) = term(258) * (-4.0d+0) 
term(259) = term(259) * (2.0d+0) 
term(260) = term(260) * (8.0d+0) 
term(261) = term(261) * (-16.0d+0) 
term(262) = term(262) * (-4.0d+0) 
term(263) = term(263) * (8.0d+0) 
term(264) = term(264) * (2.0d+0) 
term(265) = term(265) * (-4.0d+0) 
term(266) = term(266) * (-2.0d+0) 
term(267) = term(267) * (4.0d+0) 
term(268) = term(268) * (4.0d+0) 
term(269) = term(269) * (-8.0d+0) 
term(270) = term(270) * (-2.0d+0) 
term(271) = term(271) * (4.0d+0) 
term(272) = term(272) * (4.0d+0) 
term(273) = term(273) * (-8.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(274) = term(274) + r2(vrdav_Rl, a,j,q,i) * wm_interm_10_pt4(k,l,i,j) * wm_interm_28_pt4(a,k,l,p)
end do 
end do 
end do 
end do 
end do 

term(274) = term(274) * (-1.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
do a = nocc + 1, nactive 
term(275) = term(275) + r2(vrdav_Rl, a,j,q,i) * wm_interm_10_pt4(k,l,i,j) * wm_interm_28_pt4(a,l,k,p)
end do 
end do 
end do 
end do 
end do 

term(275) = term(275) * (2.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do l = 1, nocc 
do a = nocc + 1, nactive 
term(276) = term(276) + r2(vrdav_Rl, a,j,q,i) * wm_interm_10_pt4(k,l,j,i) * wm_interm_28_pt4(a,l,k,p)
end do 
end do 
end do 
end do 
end do 

term(276) = term(276) * (-1.0d+0) 

do i = 1, nocc 
do l = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(277) = term(277) + r2(vrdav_Rl, a,j,q,i) * wm_interm_10_pt4(k,l,j,i) * wm_interm_28_pt4(a,k,l,p)
end do 
end do 
end do 
end do 
end do 

term(277) = term(277) * (2.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(278) = term(278) + t2(a,q,j,i) * wm_interm_23_pt4(b,i,j,k) * wm_interm_57_pt4(a,b,k,p)
term(279) = term(279) + t2(a,q,j,i) * wm_interm_23_pt4(b,i,j,k) * wm_interm_58_pt4(a,b,k,p)
term(280) = term(280) + t2(a,q,j,i) * wm_interm_23_pt4(b,i,j,k) * wm_interm_64_pt4(a,b,k,p)
term(281) = term(281) + t2(a,q,j,i) * wm_interm_23_pt4(b,i,j,k) * wm_interm_65_pt4(a,b,k,p)
end do 
end do 
end do 
end do 
end do 

term(278) = term(278) * (-2.0d+0) 
term(279) = term(279) * (4.0d+0) 
term(280) = term(280) * (4.0d+0) 
term(281) = term(281) * (-8.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(282) = term(282) + t2(a,q,j,i) * wm_interm_23_pt4(b,j,i,k) * wm_interm_58_pt4(a,b,k,p)
term(283) = term(283) + t2(a,q,j,i) * wm_interm_23_pt4(b,j,i,k) * wm_interm_57_pt4(a,b,k,p)
term(284) = term(284) + t2(a,q,j,i) * wm_interm_23_pt4(b,j,i,k) * wm_interm_64_pt4(a,b,k,p)
term(285) = term(285) + t2(a,q,j,i) * wm_interm_23_pt4(b,j,i,k) * wm_interm_65_pt4(a,b,k,p)
end do 
end do 
end do 
end do 
end do 

term(282) = term(282) * (-2.0d+0) 
term(283) = term(283) * (4.0d+0) 
term(284) = term(284) * (-2.0d+0) 
term(285) = term(285) * (4.0d+0) 

do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
term(286) = term(286) + t1(q,i) * wm_interm_18_pt4(j,k) * wm_interm_63_pt4(i,k,j,p)
term(287) = term(287) + t1(q,i) * wm_interm_18_pt4(j,k) * wm_interm_63_pt4(i,k,p,j)
term(288) = term(288) + t1(q,i) * wm_interm_19_pt4(j,k) * wm_interm_63_pt4(i,k,j,p)
term(289) = term(289) + t1(q,i) * wm_interm_19_pt4(j,k) * wm_interm_63_pt4(i,k,p,j)
term(290) = term(290) + t1(q,i) * wm_interm_18_pt4(j,k) * wm_interm_63_pt4(k,i,p,j)
term(291) = term(291) + t1(q,i) * wm_interm_19_pt4(j,k) * wm_interm_63_pt4(k,i,p,j)
term(292) = term(292) + t1(q,i) * wm_interm_18_pt4(j,k) * wm_interm_63_pt4(k,i,j,p)
term(293) = term(293) + t1(q,i) * wm_interm_19_pt4(j,k) * wm_interm_63_pt4(k,i,j,p)
end do 
end do 
end do 

term(286) = term(286) * (-1.0d+0) 
term(287) = term(287) * (2.0d+0) 
term(288) = term(288) * (2.0d+0) 
term(289) = term(289) * (-4.0d+0) 
term(290) = term(290) * (-1.0d+0) 
term(291) = term(291) * (2.0d+0) 
term(292) = term(292) * (2.0d+0) 
term(293) = term(293) * (-4.0d+0) 

do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(294) = term(294) + r1(vrdav_Rr, a,p) * wm_interm_20_pt4(a,b) * wm_interm_24_pt4(b,q)
term(295) = term(295) + r1(vrdav_Rr, a,p) * wm_interm_21_pt4(a,b) * wm_interm_24_pt4(b,q)
term(296) = term(296) + s1(a,p) * wm_interm_51_pt4(q,b) * wm_interm_68_pt4(a,b)
term(297) = term(297) + s1(a,p) * wm_interm_51_pt4(q,b) * wm_interm_69_pt4(a,b)
term(298) = term(298) + s1(a,p) * wm_interm_52_pt4(q,b) * wm_interm_68_pt4(a,b)
term(299) = term(299) + s1(a,p) * wm_interm_52_pt4(q,b) * wm_interm_69_pt4(a,b)
end do 
end do 

term(294) = term(294) * (2.0d+0) 
term(295) = term(295) * (-4.0d+0) 
term(296) = term(296) * (4.0d+0) 
term(297) = term(297) * (-8.0d+0) 
term(298) = term(298) * (-2.0d+0) 
term(299) = term(299) * (4.0d+0) 

do i = 1, nocc 
term(300) = term(300) + wm_interm_27_pt4(p,i) * wm_interm_38_pt4(q,i)
term(301) = term(301) + wm_interm_27_pt4(p,i) * wm_interm_41_pt4(q,i)
term(302) = term(302) + wm_interm_44_pt4(q,i) * wm_interm_9_pt4(i,p)
term(303) = term(303) + wm_interm_45_pt4(q,i) * wm_interm_9_pt4(i,p)
end do 

term(300) = term(300) * (2.0d+0) 
term(301) = term(301) * (-4.0d+0) 
term(302) = term(302) * (-4.0d+0) 
term(303) = term(303) * (2.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(304) = term(304) + t1(q,i) * wm_interm_55_pt4(j,i,k,l) * wm_interm_72_pt4(j,p,l,k)
term(305) = term(305) + t1(q,i) * wm_interm_55_pt4(j,i,k,l) * wm_interm_72_pt4(j,p,k,l)
end do 
end do 
end do 
end do 

term(304) = term(304) * (-1.0d+0) 
term(305) = term(305) * (2.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(306) = term(306) + r1(vrdav_Rl, q,i) * wm_interm_2_pt4(a,j) * wm_interm_3_pt4(a,p,j,i)
term(307) = term(307) + r1(vrdav_Rl, q,i) * wm_interm_3_pt4(a,p,j,i) * wm_interm_4_pt4(a,j)
term(308) = term(308) + t1(q,i) * wm_interm_13_pt4(a,p,j,i) * wm_interm_15_pt4(a,j)
term(309) = term(309) + t1(q,i) * wm_interm_13_pt4(a,p,j,i) * wm_interm_17_pt4(a,j)
term(310) = term(310) + r1(vrdav_Rr, a,p) * wm_interm_22_pt4(i,j) * wm_interm_5_pt4(a,q,j,i)
term(311) = term(311) + r2(vrdav_Rl, a,j,q,i) * wm_interm_18_pt4(p,i) * wm_interm_31_pt4(a,j)
term(312) = term(312) + r2(vrdav_Rl, a,j,q,i) * wm_interm_19_pt4(p,i) * wm_interm_31_pt4(a,j)
term(313) = term(313) + r2(vrdav_Rl, a,j,q,i) * wm_interm_18_pt4(p,i) * wm_interm_34_pt4(a,j)
term(314) = term(314) + r2(vrdav_Rl, a,j,q,i) * wm_interm_19_pt4(p,i) * wm_interm_34_pt4(a,j)
term(315) = term(315) + s1(a,p) * wm_interm_18_pt4(i,j) * wm_interm_58_pt4(q,a,j,i)
term(316) = term(316) + s1(a,p) * wm_interm_18_pt4(i,j) * wm_interm_57_pt4(q,a,j,i)
term(317) = term(317) + s1(a,p) * wm_interm_19_pt4(i,j) * wm_interm_58_pt4(q,a,j,i)
term(318) = term(318) + s1(a,p) * wm_interm_19_pt4(i,j) * wm_interm_57_pt4(q,a,j,i)
term(319) = term(319) + s1(a,p) * wm_interm_18_pt4(i,j) * wm_interm_64_pt4(q,a,j,i)
term(320) = term(320) + s1(a,p) * wm_interm_19_pt4(i,j) * wm_interm_64_pt4(q,a,j,i)
term(321) = term(321) + s1(a,p) * wm_interm_18_pt4(i,j) * wm_interm_65_pt4(q,a,j,i)
term(322) = term(322) + s1(a,p) * wm_interm_19_pt4(i,j) * wm_interm_65_pt4(q,a,j,i)
term(323) = term(323) + r2(vrdav_Rl, a,j,q,i) * wm_interm_70_pt4(i,p) * wm_interm_7_pt4(a,j)
term(324) = term(324) + r2(vrdav_Rl, a,j,q,i) * wm_interm_71_pt4(i,p) * wm_interm_7_pt4(a,j)
term(325) = term(325) + r2(vrdav_Rl, a,j,q,i) * wm_interm_70_pt4(i,p) * wm_interm_8_pt4(a,j)
term(326) = term(326) + r2(vrdav_Rl, a,j,q,i) * wm_interm_71_pt4(i,p) * wm_interm_8_pt4(a,j)
term(327) = term(327) + t2(a,q,j,i) * wm_interm_44_pt4(a,j) * wm_interm_70_pt4(p,i)
term(328) = term(328) + t2(a,q,j,i) * wm_interm_44_pt4(a,j) * wm_interm_71_pt4(p,i)
term(329) = term(329) + t2(a,q,j,i) * wm_interm_45_pt4(a,j) * wm_interm_70_pt4(p,i)
term(330) = term(330) + t2(a,q,j,i) * wm_interm_45_pt4(a,j) * wm_interm_71_pt4(p,i)
end do 
end do 
end do 

term(306) = term(306) * (8.0d+0) 
term(307) = term(307) * (-4.0d+0) 
term(308) = term(308) * (-4.0d+0) 
term(309) = term(309) * (2.0d+0) 
term(310) = term(310) * (-4.0d+0) 
term(311) = term(311) * (8.0d+0) 
term(312) = term(312) * (-16.0d+0) 
term(313) = term(313) * (-4.0d+0) 
term(314) = term(314) * (8.0d+0) 
term(315) = term(315) * (-2.0d+0) 
term(316) = term(316) * (4.0d+0) 
term(317) = term(317) * (4.0d+0) 
term(318) = term(318) * (-8.0d+0) 
term(319) = term(319) * (-2.0d+0) 
term(320) = term(320) * (4.0d+0) 
term(321) = term(321) * (4.0d+0) 
term(322) = term(322) * (-8.0d+0) 
term(323) = term(323) * (-8.0d+0) 
term(324) = term(324) * (16.0d+0) 
term(325) = term(325) * (4.0d+0) 
term(326) = term(326) * (-8.0d+0) 
term(327) = term(327) * (-8.0d+0) 
term(328) = term(328) * (16.0d+0) 
term(329) = term(329) * (4.0d+0) 
term(330) = term(330) * (-8.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(331) = term(331) + r2(vrdav_Rl, a,j,q,i) * wm_interm_20_pt4(b,a) * wm_interm_28_pt4(b,i,j,p)
term(332) = term(332) + r2(vrdav_Rl, a,j,q,i) * wm_interm_20_pt4(b,a) * wm_interm_29_pt4(b,i,j,p)
term(333) = term(333) + r2(vrdav_Rl, a,j,q,i) * wm_interm_21_pt4(b,a) * wm_interm_28_pt4(b,i,j,p)
term(334) = term(334) + r2(vrdav_Rl, a,j,q,i) * wm_interm_21_pt4(b,a) * wm_interm_29_pt4(b,i,j,p)
end do 
end do 
end do 
end do 

term(331) = term(331) * (-1.0d+0) 
term(332) = term(332) * (2.0d+0) 
term(333) = term(333) * (2.0d+0) 
term(334) = term(334) * (-4.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
term(335) = term(335) + t1(q,i) * wm_interm_54_pt4(j,k) * wm_interm_72_pt4(j,p,k,i)
term(336) = term(336) + t1(q,i) * wm_interm_56_pt4(j,k) * wm_interm_72_pt4(j,p,k,i)
end do 
end do 
end do 

term(335) = term(335) * (4.0d+0) 
term(336) = term(336) * (-8.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(337) = term(337) + r1(vrdav_Rl, q,i) * wm_interm_3_pt4(a,p,i,j) * wm_interm_4_pt4(a,j)
term(338) = term(338) + r1(vrdav_Rl, q,i) * wm_interm_2_pt4(a,j) * wm_interm_3_pt4(a,p,i,j)
term(339) = term(339) + t1(q,i) * wm_interm_13_pt4(a,p,i,j) * wm_interm_15_pt4(a,j)
term(340) = term(340) + t1(q,i) * wm_interm_13_pt4(a,p,i,j) * wm_interm_17_pt4(a,j)
term(341) = term(341) + r1(vrdav_Rr, a,p) * wm_interm_11_pt4(a,q,i,j) * wm_interm_22_pt4(j,i)
term(342) = term(342) + r1(vrdav_Rr, a,p) * wm_interm_1_pt4(a,q,i,j) * wm_interm_22_pt4(j,i)
term(343) = term(343) + r1(vrdav_Rr, a,p) * wm_interm_12_pt4(a,q,i,j) * wm_interm_22_pt4(j,i)
term(344) = term(344) + r2(vrdav_Rl, a,i,q,j) * wm_interm_18_pt4(p,i) * wm_interm_31_pt4(a,j)
term(345) = term(345) + r2(vrdav_Rl, a,i,q,j) * wm_interm_19_pt4(p,i) * wm_interm_31_pt4(a,j)
term(346) = term(346) + r2(vrdav_Rl, a,i,q,j) * wm_interm_18_pt4(p,i) * wm_interm_34_pt4(a,j)
term(347) = term(347) + r2(vrdav_Rl, a,i,q,j) * wm_interm_19_pt4(p,i) * wm_interm_34_pt4(a,j)
term(348) = term(348) + s1(a,p) * wm_interm_48_pt4(q,a,i,j) * wm_interm_70_pt4(i,j)
term(349) = term(349) + s1(a,p) * wm_interm_48_pt4(q,a,i,j) * wm_interm_71_pt4(i,j)
term(350) = term(350) + s1(a,p) * wm_interm_49_pt4(q,a,i,j) * wm_interm_70_pt4(i,j)
term(351) = term(351) + s1(a,p) * wm_interm_49_pt4(q,a,i,j) * wm_interm_71_pt4(i,j)
term(352) = term(352) + s1(a,p) * wm_interm_53_pt4(q,a,i,j) * wm_interm_70_pt4(i,j)
term(353) = term(353) + s1(a,p) * wm_interm_53_pt4(q,a,i,j) * wm_interm_71_pt4(i,j)
term(354) = term(354) + s1(a,p) * wm_interm_50_pt4(q,a,i,j) * wm_interm_70_pt4(i,j)
term(355) = term(355) + s1(a,p) * wm_interm_50_pt4(q,a,i,j) * wm_interm_71_pt4(i,j)
term(356) = term(356) + r2(vrdav_Rl, a,i,q,j) * wm_interm_70_pt4(i,p) * wm_interm_7_pt4(a,j)
term(357) = term(357) + r2(vrdav_Rl, a,i,q,j) * wm_interm_71_pt4(i,p) * wm_interm_7_pt4(a,j)
term(358) = term(358) + r2(vrdav_Rl, a,i,q,j) * wm_interm_70_pt4(i,p) * wm_interm_8_pt4(a,j)
term(359) = term(359) + r2(vrdav_Rl, a,i,q,j) * wm_interm_71_pt4(i,p) * wm_interm_8_pt4(a,j)
term(360) = term(360) + t2(a,q,i,j) * wm_interm_44_pt4(a,j) * wm_interm_70_pt4(p,i)
term(361) = term(361) + t2(a,q,i,j) * wm_interm_44_pt4(a,j) * wm_interm_71_pt4(p,i)
term(362) = term(362) + t2(a,q,i,j) * wm_interm_45_pt4(a,j) * wm_interm_70_pt4(p,i)
term(363) = term(363) + t2(a,q,i,j) * wm_interm_45_pt4(a,j) * wm_interm_71_pt4(p,i)
end do 
end do 
end do 

term(337) = term(337) * (8.0d+0) 
term(338) = term(338) * (-16.0d+0) 
term(339) = term(339) * (8.0d+0) 
term(340) = term(340) * (-4.0d+0) 
term(341) = term(341) * (2.0d+0) 
term(342) = term(342) * (2.0d+0) 
term(343) = term(343) * (-4.0d+0) 
term(344) = term(344) * (-4.0d+0) 
term(345) = term(345) * (8.0d+0) 
term(346) = term(346) * (2.0d+0) 
term(347) = term(347) * (-4.0d+0) 
term(348) = term(348) * (-8.0d+0) 
term(349) = term(349) * (16.0d+0) 
term(350) = term(350) * (4.0d+0) 
term(351) = term(351) * (-8.0d+0) 
term(352) = term(352) * (4.0d+0) 
term(353) = term(353) * (-8.0d+0) 
term(354) = term(354) * (-8.0d+0) 
term(355) = term(355) * (16.0d+0) 
term(356) = term(356) * (4.0d+0) 
term(357) = term(357) * (-8.0d+0) 
term(358) = term(358) * (-2.0d+0) 
term(359) = term(359) * (4.0d+0) 
term(360) = term(360) * (4.0d+0) 
term(361) = term(361) * (-8.0d+0) 
term(362) = term(362) * (-2.0d+0) 
term(363) = term(363) * (4.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(364) = term(364) + r1(vrdav_Rl, q,i) * wm_interm_10_pt4(j,p,i,k) * wm_interm_9_pt4(j,k)
term(365) = term(365) + r1(vrdav_Rl, q,i) * wm_interm_10_pt4(p,j,i,k) * wm_interm_9_pt4(j,k)
term(366) = term(366) + t1(q,i) * wm_interm_54_pt4(j,k) * wm_interm_72_pt4(j,p,i,k)
term(367) = term(367) + t1(q,i) * wm_interm_56_pt4(j,k) * wm_interm_72_pt4(j,p,i,k)
end do 
end do 
end do 

term(364) = term(364) * (-4.0d+0) 
term(365) = term(365) * (8.0d+0) 
term(366) = term(366) * (-2.0d+0) 
term(367) = term(367) * (4.0d+0) 

do l = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
term(368) = term(368) + t1(q,i) * wm_interm_10_pt4(j,i,k,l) * wm_interm_63_pt4(k,l,p,j)
term(369) = term(369) + t1(q,i) * wm_interm_10_pt4(i,j,k,l) * wm_interm_63_pt4(k,l,p,j)
end do 
end do 
end do 
end do 

term(368) = term(368) * (-1.0d+0) 
term(369) = term(369) * (2.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(370) = term(370) + t1(a,i) * wm_interm_13_pt4(a,p,j,k) * wm_interm_14_pt4(q,i,j,k)
term(371) = term(371) + t1(a,i) * wm_interm_13_pt4(a,p,j,k) * wm_interm_14_pt4(q,i,k,j)
term(372) = term(372) + t1(a,i) * wm_interm_5_pt4(a,q,j,k) * wm_interm_63_pt4(k,i,p,j)
term(373) = term(373) + t1(a,i) * wm_interm_5_pt4(a,q,j,k) * wm_interm_63_pt4(k,i,j,p)
term(374) = term(374) + t1(a,i) * wm_interm_1_pt4(a,q,j,k) * wm_interm_63_pt4(k,i,j,p)
term(375) = term(375) + t1(a,i) * wm_interm_1_pt4(a,q,j,k) * wm_interm_63_pt4(k,i,p,j)
term(376) = term(376) + t1(a,i) * wm_interm_11_pt4(a,q,j,k) * wm_interm_63_pt4(k,i,j,p)
term(377) = term(377) + t1(a,i) * wm_interm_11_pt4(a,q,j,k) * wm_interm_63_pt4(k,i,p,j)
term(378) = term(378) + t1(a,i) * wm_interm_12_pt4(a,q,j,k) * wm_interm_63_pt4(k,i,j,p)
term(379) = term(379) + t1(a,i) * wm_interm_12_pt4(a,q,j,k) * wm_interm_63_pt4(k,i,p,j)
term(380) = term(380) + t1(a,i) * wm_interm_49_pt4(a,q,j,k) * wm_interm_72_pt4(j,p,i,k)
term(381) = term(381) + t1(a,i) * wm_interm_50_pt4(a,q,j,k) * wm_interm_72_pt4(j,p,i,k)
term(382) = term(382) + t1(a,i) * wm_interm_53_pt4(a,q,j,k) * wm_interm_72_pt4(j,p,i,k)
term(383) = term(383) + t1(a,i) * wm_interm_48_pt4(a,q,j,k) * wm_interm_72_pt4(j,p,i,k)
term(384) = term(384) + t1(a,i) * wm_interm_11_pt4(a,q,j,k) * wm_interm_63_pt4(i,k,p,j)
term(385) = term(385) + t1(a,i) * wm_interm_11_pt4(a,q,j,k) * wm_interm_63_pt4(i,k,j,p)
term(386) = term(386) + t1(a,i) * wm_interm_5_pt4(a,q,j,k) * wm_interm_63_pt4(i,k,j,p)
term(387) = term(387) + t1(a,i) * wm_interm_5_pt4(a,q,j,k) * wm_interm_63_pt4(i,k,p,j)
term(388) = term(388) + t1(a,i) * wm_interm_1_pt4(a,q,j,k) * wm_interm_63_pt4(i,k,p,j)
term(389) = term(389) + t1(a,i) * wm_interm_1_pt4(a,q,j,k) * wm_interm_63_pt4(i,k,j,p)
term(390) = term(390) + t1(a,i) * wm_interm_12_pt4(a,q,j,k) * wm_interm_63_pt4(i,k,p,j)
term(391) = term(391) + t1(a,i) * wm_interm_12_pt4(a,q,j,k) * wm_interm_63_pt4(i,k,j,p)
end do 
end do 
end do 
end do 

term(370) = term(370) * (2.0d+0) 
term(371) = term(371) * (-4.0d+0) 
term(372) = term(372) * (-1.0d+0) 
term(373) = term(373) * (2.0d+0) 
term(374) = term(374) * (-1.0d+0) 
term(375) = term(375) * (2.0d+0) 
term(376) = term(376) * (-1.0d+0) 
term(377) = term(377) * (2.0d+0) 
term(378) = term(378) * (2.0d+0) 
term(379) = term(379) * (-4.0d+0) 
term(380) = term(380) * (4.0d+0) 
term(381) = term(381) * (-2.0d+0) 
term(382) = term(382) * (4.0d+0) 
term(383) = term(383) * (-8.0d+0) 
term(384) = term(384) * (-1.0d+0) 
term(385) = term(385) * (2.0d+0) 
term(386) = term(386) * (-1.0d+0) 
term(387) = term(387) * (2.0d+0) 
term(388) = term(388) * (-1.0d+0) 
term(389) = term(389) * (2.0d+0) 
term(390) = term(390) * (2.0d+0) 
term(391) = term(391) * (-4.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(392) = term(392) + s2(a,b,i,p) * wm_interm_31_pt4(a,j) * wm_interm_48_pt4(q,b,j,i)
term(393) = term(393) + s2(a,b,p,i) * wm_interm_31_pt4(a,j) * wm_interm_48_pt4(q,b,j,i)
term(394) = term(394) + s2(a,b,i,p) * wm_interm_34_pt4(a,j) * wm_interm_48_pt4(q,b,j,i)
term(395) = term(395) + s2(a,b,p,i) * wm_interm_34_pt4(a,j) * wm_interm_48_pt4(q,b,j,i)
term(396) = term(396) + s2(a,b,i,p) * wm_interm_31_pt4(a,j) * wm_interm_49_pt4(q,b,j,i)
term(397) = term(397) + s2(a,b,p,i) * wm_interm_31_pt4(a,j) * wm_interm_49_pt4(q,b,j,i)
term(398) = term(398) + s2(a,b,i,p) * wm_interm_34_pt4(a,j) * wm_interm_49_pt4(q,b,j,i)
term(399) = term(399) + s2(a,b,p,i) * wm_interm_34_pt4(a,j) * wm_interm_49_pt4(q,b,j,i)
term(400) = term(400) + s2(a,b,p,i) * wm_interm_31_pt4(a,j) * wm_interm_50_pt4(q,b,j,i)
term(401) = term(401) + s2(a,b,i,p) * wm_interm_31_pt4(a,j) * wm_interm_50_pt4(q,b,j,i)
term(402) = term(402) + s2(a,b,i,p) * wm_interm_31_pt4(a,j) * wm_interm_53_pt4(q,b,j,i)
term(403) = term(403) + s2(a,b,p,i) * wm_interm_31_pt4(a,j) * wm_interm_53_pt4(q,b,j,i)
term(404) = term(404) + s2(a,b,p,i) * wm_interm_34_pt4(a,j) * wm_interm_50_pt4(q,b,j,i)
term(405) = term(405) + s2(a,b,i,p) * wm_interm_34_pt4(a,j) * wm_interm_50_pt4(q,b,j,i)
term(406) = term(406) + s2(a,b,i,p) * wm_interm_34_pt4(a,j) * wm_interm_53_pt4(q,b,j,i)
term(407) = term(407) + s2(a,b,p,i) * wm_interm_34_pt4(a,j) * wm_interm_53_pt4(q,b,j,i)
term(408) = term(408) + s2(a,b,p,i) * wm_interm_57_pt4(q,b,j,i) * wm_interm_7_pt4(a,j)
term(409) = term(409) + s2(a,b,p,i) * wm_interm_58_pt4(q,b,j,i) * wm_interm_7_pt4(a,j)
term(410) = term(410) + s2(a,b,i,p) * wm_interm_58_pt4(q,b,j,i) * wm_interm_7_pt4(a,j)
term(411) = term(411) + s2(a,b,i,p) * wm_interm_57_pt4(q,b,j,i) * wm_interm_7_pt4(a,j)
term(412) = term(412) + s2(a,b,i,p) * wm_interm_64_pt4(q,b,j,i) * wm_interm_7_pt4(a,j)
term(413) = term(413) + s2(a,b,i,p) * wm_interm_65_pt4(q,b,j,i) * wm_interm_7_pt4(a,j)
term(414) = term(414) + s2(a,b,p,i) * wm_interm_64_pt4(q,b,j,i) * wm_interm_7_pt4(a,j)
term(415) = term(415) + s2(a,b,p,i) * wm_interm_65_pt4(q,b,j,i) * wm_interm_7_pt4(a,j)
term(416) = term(416) + s2(a,b,p,i) * wm_interm_57_pt4(q,b,j,i) * wm_interm_8_pt4(a,j)
term(417) = term(417) + s2(a,b,p,i) * wm_interm_58_pt4(q,b,j,i) * wm_interm_8_pt4(a,j)
term(418) = term(418) + s2(a,b,i,p) * wm_interm_58_pt4(q,b,j,i) * wm_interm_8_pt4(a,j)
term(419) = term(419) + s2(a,b,i,p) * wm_interm_57_pt4(q,b,j,i) * wm_interm_8_pt4(a,j)
term(420) = term(420) + s2(a,b,i,p) * wm_interm_64_pt4(q,b,j,i) * wm_interm_8_pt4(a,j)
term(421) = term(421) + s2(a,b,i,p) * wm_interm_65_pt4(q,b,j,i) * wm_interm_8_pt4(a,j)
term(422) = term(422) + s2(a,b,p,i) * wm_interm_64_pt4(q,b,j,i) * wm_interm_8_pt4(a,j)
term(423) = term(423) + s2(a,b,p,i) * wm_interm_65_pt4(q,b,j,i) * wm_interm_8_pt4(a,j)
end do 
end do 
end do 
end do 

term(392) = term(392) * (8.0d+0) 
term(393) = term(393) * (-16.0d+0) 
term(394) = term(394) * (-4.0d+0) 
term(395) = term(395) * (8.0d+0) 
term(396) = term(396) * (-4.0d+0) 
term(397) = term(397) * (8.0d+0) 
term(398) = term(398) * (2.0d+0) 
term(399) = term(399) * (-4.0d+0) 
term(400) = term(400) * (-4.0d+0) 
term(401) = term(401) * (8.0d+0) 
term(402) = term(402) * (-4.0d+0) 
term(403) = term(403) * (8.0d+0) 
term(404) = term(404) * (2.0d+0) 
term(405) = term(405) * (-4.0d+0) 
term(406) = term(406) * (2.0d+0) 
term(407) = term(407) * (-4.0d+0) 
term(408) = term(408) * (-8.0d+0) 
term(409) = term(409) * (16.0d+0) 
term(410) = term(410) * (-8.0d+0) 
term(411) = term(411) * (16.0d+0) 
term(412) = term(412) * (-8.0d+0) 
term(413) = term(413) * (16.0d+0) 
term(414) = term(414) * (16.0d+0) 
term(415) = term(415) * (-32.0d+0) 
term(416) = term(416) * (4.0d+0) 
term(417) = term(417) * (-8.0d+0) 
term(418) = term(418) * (4.0d+0) 
term(419) = term(419) * (-8.0d+0) 
term(420) = term(420) * (4.0d+0) 
term(421) = term(421) * (-8.0d+0) 
term(422) = term(422) * (-8.0d+0) 
term(423) = term(423) * (16.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(424) = term(424) + t1(q,i) * wm_interm_10_pt4(j,i,k,l) * wm_interm_63_pt4(k,l,j,p)
term(425) = term(425) + t1(q,i) * wm_interm_10_pt4(i,j,k,l) * wm_interm_63_pt4(k,l,j,p)
term(426) = term(426) + t1(q,i) * wm_interm_55_pt4(i,j,k,l) * wm_interm_72_pt4(j,p,k,l)
term(427) = term(427) + t1(q,i) * wm_interm_55_pt4(i,j,k,l) * wm_interm_72_pt4(j,p,l,k)
end do 
end do 
end do 
end do 

term(424) = term(424) * (2.0d+0) 
term(425) = term(425) * (-1.0d+0) 
term(426) = term(426) * (-1.0d+0) 
term(427) = term(427) * (2.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(428) = term(428) + r1(vrdav_Rl, q,i) * wm_interm_0_pt4(a,b) * wm_interm_1_pt4(a,b,p,i)
term(429) = term(429) + r1(vrdav_Rl, q,i) * wm_interm_0_pt4(a,b) * wm_interm_5_pt4(a,b,p,i)
term(430) = term(430) + r1(vrdav_Rl, q,i) * wm_interm_0_pt4(a,b) * wm_interm_11_pt4(a,b,p,i)
term(431) = term(431) + r1(vrdav_Rl, q,i) * wm_interm_0_pt4(a,b) * wm_interm_12_pt4(a,b,p,i)
term(432) = term(432) + s2(a,b,p,i) * wm_interm_31_pt4(b,i) * wm_interm_51_pt4(q,a)
term(433) = term(433) + s2(a,b,p,i) * wm_interm_31_pt4(b,i) * wm_interm_52_pt4(q,a)
term(434) = term(434) + s2(a,b,p,i) * wm_interm_34_pt4(b,i) * wm_interm_51_pt4(q,a)
term(435) = term(435) + s2(a,b,p,i) * wm_interm_34_pt4(b,i) * wm_interm_52_pt4(q,a)
term(436) = term(436) + t1(q,i) * wm_interm_20_pt4(a,b) * wm_interm_58_pt4(b,a,i,p)
term(437) = term(437) + t1(q,i) * wm_interm_20_pt4(a,b) * wm_interm_57_pt4(b,a,i,p)
term(438) = term(438) + t1(q,i) * wm_interm_21_pt4(a,b) * wm_interm_58_pt4(b,a,i,p)
term(439) = term(439) + t1(q,i) * wm_interm_21_pt4(a,b) * wm_interm_57_pt4(b,a,i,p)
term(440) = term(440) + t1(q,i) * wm_interm_20_pt4(a,b) * wm_interm_64_pt4(b,a,i,p)
term(441) = term(441) + t1(q,i) * wm_interm_20_pt4(a,b) * wm_interm_65_pt4(b,a,i,p)
term(442) = term(442) + t1(q,i) * wm_interm_21_pt4(a,b) * wm_interm_64_pt4(b,a,i,p)
term(443) = term(443) + t1(q,i) * wm_interm_21_pt4(a,b) * wm_interm_65_pt4(b,a,i,p)
term(444) = term(444) + t1(q,i) * wm_interm_51_pt4(a,b) * wm_interm_59_pt4(a,b,p,i)
term(445) = term(445) + t1(q,i) * wm_interm_51_pt4(a,b) * wm_interm_60_pt4(a,b,p,i)
term(446) = term(446) + t1(q,i) * wm_interm_51_pt4(a,b) * wm_interm_61_pt4(a,b,p,i)
term(447) = term(447) + t1(q,i) * wm_interm_51_pt4(a,b) * wm_interm_62_pt4(a,b,p,i)
term(448) = term(448) + t1(q,i) * wm_interm_52_pt4(a,b) * wm_interm_59_pt4(a,b,p,i)
term(449) = term(449) + t1(q,i) * wm_interm_52_pt4(a,b) * wm_interm_60_pt4(a,b,p,i)
term(450) = term(450) + t1(q,i) * wm_interm_52_pt4(a,b) * wm_interm_61_pt4(a,b,p,i)
term(451) = term(451) + t1(q,i) * wm_interm_52_pt4(a,b) * wm_interm_62_pt4(a,b,p,i)
term(452) = term(452) + r2(vrdav_Rr, a,p,b,i) * wm_interm_20_pt4(b,q) * wm_interm_44_pt4(a,i)
term(453) = term(453) + r2(vrdav_Rr, a,p,b,i) * wm_interm_20_pt4(a,q) * wm_interm_44_pt4(b,i)
term(454) = term(454) + r2(vrdav_Rr, a,p,b,i) * wm_interm_21_pt4(b,q) * wm_interm_44_pt4(a,i)
term(455) = term(455) + r2(vrdav_Rr, a,p,b,i) * wm_interm_21_pt4(a,q) * wm_interm_44_pt4(b,i)
term(456) = term(456) + r2(vrdav_Rr, a,p,b,i) * wm_interm_20_pt4(b,q) * wm_interm_45_pt4(a,i)
term(457) = term(457) + r2(vrdav_Rr, a,p,b,i) * wm_interm_20_pt4(a,q) * wm_interm_45_pt4(b,i)
term(458) = term(458) + r2(vrdav_Rr, a,p,b,i) * wm_interm_21_pt4(b,q) * wm_interm_45_pt4(a,i)
term(459) = term(459) + r2(vrdav_Rr, a,p,b,i) * wm_interm_21_pt4(a,q) * wm_interm_45_pt4(b,i)
end do 
end do 
end do 

term(428) = term(428) * (-4.0d+0) 
term(429) = term(429) * (8.0d+0) 
term(430) = term(430) * (-4.0d+0) 
term(431) = term(431) * (8.0d+0) 
term(432) = term(432) * (-16.0d+0) 
term(433) = term(433) * (8.0d+0) 
term(434) = term(434) * (8.0d+0) 
term(435) = term(435) * (-4.0d+0) 
term(436) = term(436) * (-2.0d+0) 
term(437) = term(437) * (4.0d+0) 
term(438) = term(438) * (4.0d+0) 
term(439) = term(439) * (-8.0d+0) 
term(440) = term(440) * (-2.0d+0) 
term(441) = term(441) * (4.0d+0) 
term(442) = term(442) * (4.0d+0) 
term(443) = term(443) * (-8.0d+0) 
term(444) = term(444) * (4.0d+0) 
term(445) = term(445) * (-8.0d+0) 
term(446) = term(446) * (4.0d+0) 
term(447) = term(447) * (-8.0d+0) 
term(448) = term(448) * (-2.0d+0) 
term(449) = term(449) * (4.0d+0) 
term(450) = term(450) * (-2.0d+0) 
term(451) = term(451) * (4.0d+0) 
term(452) = term(452) * (4.0d+0) 
term(453) = term(453) * (-8.0d+0) 
term(454) = term(454) * (-8.0d+0) 
term(455) = term(455) * (16.0d+0) 
term(456) = term(456) * (-2.0d+0) 
term(457) = term(457) * (4.0d+0) 
term(458) = term(458) * (4.0d+0) 
term(459) = term(459) * (-8.0d+0) 

do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(460) = term(460) + s2(a,b,i,p) * wm_interm_31_pt4(b,i) * wm_interm_51_pt4(q,a)
term(461) = term(461) + s2(a,b,i,p) * wm_interm_31_pt4(b,i) * wm_interm_52_pt4(q,a)
term(462) = term(462) + s2(a,b,i,p) * wm_interm_34_pt4(b,i) * wm_interm_51_pt4(q,a)
term(463) = term(463) + s2(a,b,i,p) * wm_interm_34_pt4(b,i) * wm_interm_52_pt4(q,a)
end do 
end do 
end do 

term(460) = term(460) * (8.0d+0) 
term(461) = term(461) * (-4.0d+0) 
term(462) = term(462) * (-4.0d+0) 
term(463) = term(463) * (2.0d+0) 


    calc_D_ov_wm_pt4 = zero
    do s = 0, 463
    calc_D_ov_wm_pt4 = calc_D_ov_wm_pt4 + term(s)
    end do

    end function calc_D_ov_wm_pt4
    
    function calc_D_vo_wm_pt4(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vo_wm_pt4
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
    real(F64), dimension(0:1515) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(0) = term(0) + r2(vrdav_Rl, a,i,p,q) * wm_interm_3_pt4(b,j,k,i) * wm_interm_59_pt4(a,b,j,k)
term(1) = term(1) + r2(vrdav_Rl, a,i,p,q) * wm_interm_3_pt4(b,j,k,i) * wm_interm_60_pt4(a,b,j,k)
term(2) = term(2) + r2(vrdav_Rl, a,i,p,q) * wm_interm_3_pt4(b,j,k,i) * wm_interm_61_pt4(a,b,j,k)
term(3) = term(3) + r2(vrdav_Rl, a,i,p,q) * wm_interm_3_pt4(b,j,k,i) * wm_interm_62_pt4(a,b,j,k)
term(4) = term(4) + r2(vrdav_Rl, a,q,p,i) * wm_interm_3_pt4(b,j,k,i) * wm_interm_59_pt4(a,b,j,k)
term(5) = term(5) + r2(vrdav_Rl, a,q,p,i) * wm_interm_3_pt4(b,j,k,i) * wm_interm_60_pt4(a,b,j,k)
term(6) = term(6) + r2(vrdav_Rl, a,q,p,i) * wm_interm_3_pt4(b,j,k,i) * wm_interm_61_pt4(a,b,j,k)
term(7) = term(7) + r2(vrdav_Rl, a,q,p,i) * wm_interm_3_pt4(b,j,k,i) * wm_interm_62_pt4(a,b,j,k)
term(8) = term(8) + r2(vrdav_Rl, a,q,p,i) * wm_interm_3_pt4(b,j,i,k) * wm_interm_62_pt4(a,b,j,k)
term(9) = term(9) + r2(vrdav_Rl, a,q,p,i) * wm_interm_3_pt4(b,j,i,k) * wm_interm_61_pt4(a,b,j,k)
term(10) = term(10) + r2(vrdav_Rl, a,q,p,i) * wm_interm_3_pt4(b,j,i,k) * wm_interm_59_pt4(a,b,j,k)
term(11) = term(11) + r2(vrdav_Rl, a,q,p,i) * wm_interm_3_pt4(b,j,i,k) * wm_interm_60_pt4(a,b,j,k)
term(12) = term(12) + r2(vrdav_Rr, a,j,p,i) * wm_interm_48_pt4(a,b,k,q) * wm_interm_76_pt4(b,i,j,k)
term(13) = term(13) + r2(vrdav_Rr, a,j,p,i) * wm_interm_23_pt4(b,i,j,k) * wm_interm_53_pt4(a,b,k,q)
term(14) = term(14) + r2(vrdav_Rr, a,j,p,i) * wm_interm_23_pt4(b,i,j,k) * wm_interm_50_pt4(a,b,k,q)
term(15) = term(15) + r2(vrdav_Rr, a,j,p,i) * wm_interm_49_pt4(a,b,k,q) * wm_interm_76_pt4(b,i,j,k)
term(16) = term(16) + r2(vrdav_Rr, a,j,p,i) * wm_interm_42_pt4(b,i,j,k) * wm_interm_5_pt4(a,b,k,q)
term(17) = term(17) + r2(vrdav_Rr, a,j,p,i) * wm_interm_1_pt4(a,b,k,q) * wm_interm_42_pt4(b,i,j,k)
term(18) = term(18) + r2(vrdav_Rr, a,j,p,i) * wm_interm_11_pt4(a,b,k,q) * wm_interm_42_pt4(b,i,j,k)
term(19) = term(19) + r2(vrdav_Rr, a,j,p,i) * wm_interm_12_pt4(a,b,k,q) * wm_interm_42_pt4(b,i,j,k)
term(20) = term(20) + r2(vrdav_Rr, a,j,p,i) * wm_interm_50_pt4(a,b,k,q) * wm_interm_76_pt4(b,i,j,k)
term(21) = term(21) + r2(vrdav_Rr, a,j,p,i) * wm_interm_53_pt4(a,b,k,q) * wm_interm_76_pt4(b,i,j,k)
term(22) = term(22) + r2(vrdav_Rr, a,j,p,i) * wm_interm_11_pt4(a,b,k,q) * wm_interm_43_pt4(b,i,j,k)
term(23) = term(23) + r2(vrdav_Rr, a,j,p,i) * wm_interm_43_pt4(b,i,j,k) * wm_interm_5_pt4(a,b,k,q)
term(24) = term(24) + r2(vrdav_Rr, a,j,p,i) * wm_interm_1_pt4(a,b,k,q) * wm_interm_43_pt4(b,i,j,k)
term(25) = term(25) + r2(vrdav_Rr, a,j,p,i) * wm_interm_12_pt4(a,b,k,q) * wm_interm_43_pt4(b,i,j,k)
end do 
end do 
end do 
end do 
end do 

term(0) = term(0) * (-8.0d+0) 
term(1) = term(1) * (16.0d+0) 
term(2) = term(2) * (-8.0d+0) 
term(3) = term(3) * (16.0d+0) 
term(4) = term(4) * (4.0d+0) 
term(5) = term(5) * (-8.0d+0) 
term(6) = term(6) * (4.0d+0) 
term(7) = term(7) * (-8.0d+0) 
term(8) = term(8) * (4.0d+0) 
term(9) = term(9) * (-8.0d+0) 
term(10) = term(10) * (-8.0d+0) 
term(11) = term(11) * (16.0d+0) 
term(12) = term(12) * (4.0d+0) 
term(13) = term(13) * (2.0d+0) 
term(14) = term(14) * (-1.0d+0) 
term(15) = term(15) * (-2.0d+0) 
term(16) = term(16) * (-1.0d+0) 
term(17) = term(17) * (2.0d+0) 
term(18) = term(18) * (2.0d+0) 
term(19) = term(19) * (-4.0d+0) 
term(20) = term(20) * (2.0d+0) 
term(21) = term(21) * (-1.0d+0) 
term(22) = term(22) * (-1.0d+0) 
term(23) = term(23) * (2.0d+0) 
term(24) = term(24) * (-1.0d+0) 
term(25) = term(25) * (2.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(26) = term(26) + s1(p,i) * wm_interm_55_pt4(j,q,i,k) * wm_interm_70_pt4(j,k)
term(27) = term(27) + s1(p,i) * wm_interm_55_pt4(j,q,i,k) * wm_interm_71_pt4(j,k)
term(28) = term(28) + s1(p,i) * wm_interm_55_pt4(q,j,i,k) * wm_interm_70_pt4(j,k)
term(29) = term(29) + s1(p,i) * wm_interm_55_pt4(q,j,i,k) * wm_interm_71_pt4(j,k)
term(30) = term(30) + s1(p,i) * wm_interm_54_pt4(j,k) * wm_interm_72_pt4(j,i,q,k)
term(31) = term(31) + s1(p,i) * wm_interm_54_pt4(j,k) * wm_interm_72_pt4(j,i,k,q)
term(32) = term(32) + s1(p,i) * wm_interm_56_pt4(j,k) * wm_interm_72_pt4(j,i,q,k)
term(33) = term(33) + s1(p,i) * wm_interm_56_pt4(j,k) * wm_interm_72_pt4(j,i,k,q)
term(34) = term(34) + t1(p,i) * wm_interm_55_pt4(j,i,q,k) * wm_interm_70_pt4(j,k)
term(35) = term(35) + t1(p,i) * wm_interm_55_pt4(j,i,k,q) * wm_interm_70_pt4(j,k)
term(36) = term(36) + t1(p,i) * wm_interm_55_pt4(j,i,q,k) * wm_interm_71_pt4(j,k)
term(37) = term(37) + t1(p,i) * wm_interm_55_pt4(j,i,k,q) * wm_interm_71_pt4(j,k)
end do 
end do 
end do 

term(26) = term(26) * (2.0d+0) 
term(27) = term(27) * (-4.0d+0) 
term(28) = term(28) * (-4.0d+0) 
term(29) = term(29) * (8.0d+0) 
term(30) = term(30) * (-2.0d+0) 
term(31) = term(31) * (4.0d+0) 
term(32) = term(32) * (4.0d+0) 
term(33) = term(33) * (-8.0d+0) 
term(34) = term(34) * (-2.0d+0) 
term(35) = term(35) * (4.0d+0) 
term(36) = term(36) * (4.0d+0) 
term(37) = term(37) * (-8.0d+0) 

do i = 1, nocc 
term(38) = term(38) + t1(p,i) * wm_interm_18_pt4(i,q) * wm_interm_25_pt4
term(39) = term(39) + t1(p,i) * wm_interm_19_pt4(i,q) * wm_interm_25_pt4
term(40) = term(40) + t1(p,i) * wm_interm_18_pt4(i,q) * wm_interm_77_pt4
term(41) = term(41) + t1(p,i) * wm_interm_18_pt4(i,q) * wm_interm_78_pt4
term(42) = term(42) + t1(p,i) * wm_interm_19_pt4(i,q) * wm_interm_77_pt4
term(43) = term(43) + t1(p,i) * wm_interm_19_pt4(i,q) * wm_interm_78_pt4
term(44) = term(44) + wm_interm_22_pt4(i,q) * wm_interm_31_pt4(p,i)
term(45) = term(45) + wm_interm_22_pt4(i,q) * wm_interm_34_pt4(p,i)
term(46) = term(46) + wm_interm_27_pt4(i,q) * wm_interm_35_pt4(p,i)
term(47) = term(47) + wm_interm_27_pt4(i,q) * wm_interm_32_pt4(p,i)
end do 

term(38) = term(38) * (-8.0d+0) 
term(39) = term(39) * (16.0d+0) 
term(40) = term(40) * (4.0d+0) 
term(41) = term(41) * (-8.0d+0) 
term(42) = term(42) * (-8.0d+0) 
term(43) = term(43) * (16.0d+0) 
term(44) = term(44) * (4.0d+0) 
term(45) = term(45) * (-2.0d+0) 
term(46) = term(46) * (2.0d+0) 
term(47) = term(47) * (-4.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
term(48) = term(48) + wm_interm_27_pt4(i,j) * wm_interm_39_pt4(p,q,j,i)
term(49) = term(49) + wm_interm_27_pt4(i,j) * wm_interm_40_pt4(p,q,j,i)
term(50) = term(50) + wm_interm_27_pt4(i,j) * wm_interm_39_pt4(p,j,q,i)
term(51) = term(51) + wm_interm_27_pt4(i,j) * wm_interm_40_pt4(p,j,q,i)
term(52) = term(52) + s1(p,i) * wm_interm_54_pt4(j,i) * wm_interm_70_pt4(j,q)
term(53) = term(53) + s1(p,i) * wm_interm_56_pt4(j,i) * wm_interm_70_pt4(j,q)
term(54) = term(54) + s1(p,i) * wm_interm_54_pt4(j,i) * wm_interm_71_pt4(j,q)
term(55) = term(55) + s1(p,i) * wm_interm_56_pt4(j,i) * wm_interm_71_pt4(j,q)
term(56) = term(56) + t1(p,i) * wm_interm_54_pt4(j,q) * wm_interm_70_pt4(j,i)
term(57) = term(57) + t1(p,i) * wm_interm_56_pt4(j,q) * wm_interm_70_pt4(j,i)
term(58) = term(58) + t1(p,i) * wm_interm_54_pt4(j,q) * wm_interm_71_pt4(j,i)
term(59) = term(59) + t1(p,i) * wm_interm_56_pt4(j,q) * wm_interm_71_pt4(j,i)
end do 
end do 

term(48) = term(48) * (-2.0d+0) 
term(51) = term(51) * (-2.0d+0) 
term(52) = term(52) * (-2.0d+0) 
term(53) = term(53) * (4.0d+0) 
term(54) = term(54) * (4.0d+0) 
term(55) = term(55) * (-8.0d+0) 
term(56) = term(56) * (-4.0d+0) 
term(57) = term(57) * (8.0d+0) 
term(58) = term(58) * (8.0d+0) 
term(59) = term(59) * (-16.0d+0) 

do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(60) = term(60) + t2(a,b,i,j) * wm_interm_44_pt4(b,i) * wm_interm_62_pt4(p,a,q,j)
term(61) = term(61) + t2(a,b,i,j) * wm_interm_44_pt4(b,i) * wm_interm_61_pt4(p,a,q,j)
term(62) = term(62) + t2(a,b,i,j) * wm_interm_44_pt4(b,i) * wm_interm_59_pt4(p,a,q,j)
term(63) = term(63) + t2(a,b,i,j) * wm_interm_44_pt4(b,i) * wm_interm_60_pt4(p,a,q,j)
term(64) = term(64) + t2(a,b,i,j) * wm_interm_45_pt4(b,i) * wm_interm_62_pt4(p,a,q,j)
term(65) = term(65) + t2(a,b,i,j) * wm_interm_45_pt4(b,i) * wm_interm_61_pt4(p,a,q,j)
term(66) = term(66) + t2(a,b,i,j) * wm_interm_45_pt4(b,i) * wm_interm_59_pt4(p,a,q,j)
term(67) = term(67) + t2(a,b,i,j) * wm_interm_45_pt4(b,i) * wm_interm_60_pt4(p,a,q,j)
end do 
end do 
end do 
end do 

term(60) = term(60) * (8.0d+0) 
term(61) = term(61) * (-16.0d+0) 
term(62) = term(62) * (-16.0d+0) 
term(63) = term(63) * (32.0d+0) 
term(64) = term(64) * (-4.0d+0) 
term(65) = term(65) * (8.0d+0) 
term(66) = term(66) * (8.0d+0) 
term(67) = term(67) * (-16.0d+0) 

do l = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(68) = term(68) + r2(vrdav_Rl, a,i,p,q) * wm_interm_10_pt4(j,k,i,l) * wm_interm_28_pt4(a,k,j,l)
term(69) = term(69) + s2(a,p,q,i) * wm_interm_3_pt4(a,j,k,l) * wm_interm_63_pt4(l,k,i,j)
term(70) = term(70) + s2(a,p,j,i) * wm_interm_3_pt4(a,q,k,l) * wm_interm_63_pt4(l,k,i,j)
term(71) = term(71) + s2(a,p,j,i) * wm_interm_3_pt4(a,q,k,l) * wm_interm_63_pt4(k,l,i,j)
term(72) = term(72) + s2(a,p,q,i) * wm_interm_3_pt4(a,j,k,l) * wm_interm_63_pt4(k,l,i,j)
term(73) = term(73) + r2(vrdav_Rr, a,q,p,i) * wm_interm_23_pt4(a,j,k,l) * wm_interm_55_pt4(i,l,k,j)
term(74) = term(74) + r2(vrdav_Rr, a,q,p,i) * wm_interm_10_pt4(j,i,k,l) * wm_interm_42_pt4(a,k,l,j)
term(75) = term(75) + r2(vrdav_Rr, a,q,p,i) * wm_interm_10_pt4(i,j,k,l) * wm_interm_42_pt4(a,k,l,j)
term(76) = term(76) + r2(vrdav_Rr, a,q,p,i) * wm_interm_23_pt4(a,j,k,l) * wm_interm_55_pt4(l,i,k,j)
term(77) = term(77) + r2(vrdav_Rr, a,q,p,i) * wm_interm_10_pt4(i,j,k,l) * wm_interm_43_pt4(a,k,l,j)
term(78) = term(78) + r2(vrdav_Rr, a,q,p,i) * wm_interm_10_pt4(j,i,k,l) * wm_interm_43_pt4(a,k,l,j)
term(79) = term(79) + t2(a,p,q,i) * wm_interm_63_pt4(i,j,k,l) * wm_interm_76_pt4(a,k,l,j)
term(80) = term(80) + t2(a,p,q,i) * wm_interm_63_pt4(i,j,k,l) * wm_interm_76_pt4(a,l,k,j)
term(81) = term(81) + t2(a,p,q,i) * wm_interm_63_pt4(j,i,k,l) * wm_interm_76_pt4(a,l,k,j)
term(82) = term(82) + t2(a,p,q,i) * wm_interm_63_pt4(j,i,k,l) * wm_interm_76_pt4(a,k,l,j)
end do 
end do 
end do 
end do 
end do 

term(68) = term(68) * (-8.0d+0) 
term(69) = term(69) * (-4.0d+0) 
term(70) = term(70) * (2.0d+0) 
term(71) = term(71) * (-4.0d+0) 
term(72) = term(72) * (2.0d+0) 
term(73) = term(73) * (-2.0d+0) 
term(74) = term(74) * (-2.0d+0) 
term(75) = term(75) * (4.0d+0) 
term(76) = term(76) * (4.0d+0) 
term(77) = term(77) * (-2.0d+0) 
term(78) = term(78) * (4.0d+0) 
term(79) = term(79) * (-1.0d+0) 
term(80) = term(80) * (2.0d+0) 
term(81) = term(81) * (-1.0d+0) 
term(82) = term(82) * (2.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(83) = term(83) + r2(vrdav_Rl, a,i,p,q) * wm_interm_10_pt4(j,k,i,l) * wm_interm_28_pt4(a,j,k,l)
term(84) = term(84) + r2(vrdav_Rl, a,q,p,i) * wm_interm_72_pt4(i,j,k,l) * wm_interm_73_pt4(a,j,l,k)
term(85) = term(85) + s2(a,p,i,q) * wm_interm_29_pt4(a,j,k,l) * wm_interm_55_pt4(j,k,i,l)
term(86) = term(86) + s2(a,p,i,q) * wm_interm_28_pt4(a,j,k,l) * wm_interm_55_pt4(j,k,i,l)
term(87) = term(87) + r2(vrdav_Rr, a,q,p,i) * wm_interm_23_pt4(a,j,k,l) * wm_interm_55_pt4(i,l,j,k)
term(88) = term(88) + r2(vrdav_Rr, a,q,p,i) * wm_interm_23_pt4(a,j,k,l) * wm_interm_55_pt4(l,i,j,k)
term(89) = term(89) + t2(a,p,i,q) * wm_interm_42_pt4(a,j,k,l) * wm_interm_72_pt4(j,k,i,l)
term(90) = term(90) + t2(a,p,i,q) * wm_interm_43_pt4(a,j,k,l) * wm_interm_72_pt4(j,k,i,l)
end do 
end do 
end do 
end do 
end do 

term(83) = term(83) * (16.0d+0) 
term(84) = term(84) * (4.0d+0) 
term(85) = term(85) * (-4.0d+0) 
term(86) = term(86) * (8.0d+0) 
term(87) = term(87) * (4.0d+0) 
term(88) = term(88) * (-2.0d+0) 
term(89) = term(89) * (-4.0d+0) 
term(90) = term(90) * (2.0d+0) 

do l = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(91) = term(91) + r2(vrdav_Rl, a,q,p,i) * wm_interm_10_pt4(j,k,i,l) * wm_interm_28_pt4(a,k,j,l)
end do 
end do 
end do 
end do 
end do 

term(91) = term(91) * (4.0d+0) 

do l = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(92) = term(92) + r2(vrdav_Rl, a,q,p,i) * wm_interm_10_pt4(j,k,i,l) * wm_interm_28_pt4(a,j,k,l)
term(93) = term(93) + r2(vrdav_Rl, a,q,p,i) * wm_interm_72_pt4(i,j,k,l) * wm_interm_73_pt4(a,j,k,l)
term(94) = term(94) + s2(a,p,q,i) * wm_interm_3_pt4(a,j,k,l) * wm_interm_63_pt4(l,k,j,i)
term(95) = term(95) + s2(a,p,j,i) * wm_interm_3_pt4(a,q,k,l) * wm_interm_63_pt4(l,k,j,i)
term(96) = term(96) + s2(a,p,i,q) * wm_interm_3_pt4(a,j,k,l) * wm_interm_63_pt4(l,k,j,i)
term(97) = term(97) + s2(a,p,j,i) * wm_interm_3_pt4(a,q,k,l) * wm_interm_63_pt4(k,l,j,i)
term(98) = term(98) + s2(a,p,q,i) * wm_interm_3_pt4(a,j,k,l) * wm_interm_63_pt4(k,l,j,i)
term(99) = term(99) + s2(a,p,i,q) * wm_interm_3_pt4(a,j,k,l) * wm_interm_63_pt4(k,l,j,i)
term(100) = term(100) + s2(a,p,q,i) * wm_interm_29_pt4(a,j,k,l) * wm_interm_55_pt4(j,k,i,l)
term(101) = term(101) + s2(a,p,q,i) * wm_interm_28_pt4(a,j,k,l) * wm_interm_55_pt4(j,k,i,l)
term(102) = term(102) + s2(a,p,q,i) * wm_interm_28_pt4(a,j,k,l) * wm_interm_55_pt4(j,k,l,i)
term(103) = term(103) + s2(a,p,q,i) * wm_interm_29_pt4(a,j,k,l) * wm_interm_55_pt4(j,k,l,i)
term(104) = term(104) + s2(a,p,i,q) * wm_interm_28_pt4(a,j,k,l) * wm_interm_55_pt4(j,k,l,i)
term(105) = term(105) + s2(a,p,i,q) * wm_interm_29_pt4(a,j,k,l) * wm_interm_55_pt4(j,k,l,i)
term(106) = term(106) + r2(vrdav_Rr, a,j,p,i) * wm_interm_10_pt4(j,i,k,l) * wm_interm_43_pt4(a,k,l,q)
term(107) = term(107) + r2(vrdav_Rr, a,j,p,i) * wm_interm_10_pt4(i,j,k,l) * wm_interm_43_pt4(a,k,l,q)
term(108) = term(108) + r2(vrdav_Rr, a,j,p,i) * wm_interm_10_pt4(i,j,k,l) * wm_interm_42_pt4(a,k,l,q)
term(109) = term(109) + r2(vrdav_Rr, a,j,p,i) * wm_interm_10_pt4(j,i,k,l) * wm_interm_42_pt4(a,k,l,q)
term(110) = term(110) + r2(vrdav_Rr, a,j,p,i) * wm_interm_23_pt4(a,k,l,q) * wm_interm_55_pt4(i,j,k,l)
term(111) = term(111) + r2(vrdav_Rr, a,j,p,i) * wm_interm_23_pt4(a,k,l,q) * wm_interm_55_pt4(j,i,k,l)
term(112) = term(112) + t2(a,p,i,q) * wm_interm_42_pt4(a,j,k,l) * wm_interm_72_pt4(j,k,l,i)
term(113) = term(113) + t2(a,p,i,q) * wm_interm_43_pt4(a,j,k,l) * wm_interm_72_pt4(j,k,l,i)
term(114) = term(114) + t2(a,p,q,i) * wm_interm_42_pt4(a,j,k,l) * wm_interm_72_pt4(j,k,l,i)
term(115) = term(115) + t2(a,p,q,i) * wm_interm_42_pt4(a,j,k,l) * wm_interm_72_pt4(j,k,i,l)
term(116) = term(116) + t2(a,p,q,i) * wm_interm_43_pt4(a,j,k,l) * wm_interm_72_pt4(j,k,i,l)
term(117) = term(117) + t2(a,p,q,i) * wm_interm_43_pt4(a,j,k,l) * wm_interm_72_pt4(j,k,l,i)
term(118) = term(118) + t2(a,p,j,i) * wm_interm_23_pt4(a,k,l,q) * wm_interm_63_pt4(j,i,k,l)
term(119) = term(119) + t2(a,p,j,i) * wm_interm_23_pt4(a,k,l,q) * wm_interm_63_pt4(i,j,k,l)
end do 
end do 
end do 
end do 
end do 

term(92) = term(92) * (-8.0d+0) 
term(93) = term(93) * (-8.0d+0) 
term(94) = term(94) * (2.0d+0) 
term(95) = term(95) * (-4.0d+0) 
term(96) = term(96) * (-4.0d+0) 
term(97) = term(97) * (2.0d+0) 
term(98) = term(98) * (-4.0d+0) 
term(99) = term(99) * (8.0d+0) 
term(100) = term(100) * (2.0d+0) 
term(101) = term(101) * (-4.0d+0) 
term(102) = term(102) * (2.0d+0) 
term(103) = term(103) * (-4.0d+0) 
term(104) = term(104) * (-4.0d+0) 
term(105) = term(105) * (8.0d+0) 
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
term(118) = term(118) * (4.0d+0) 
term(119) = term(119) * (-2.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(120) = term(120) + r2(vrdav_Rl, a,i,p,q) * wm_interm_3_pt4(b,j,i,k) * wm_interm_62_pt4(a,b,j,k)
term(121) = term(121) + r2(vrdav_Rl, a,i,p,q) * wm_interm_3_pt4(b,j,i,k) * wm_interm_61_pt4(a,b,j,k)
term(122) = term(122) + r2(vrdav_Rl, a,i,p,q) * wm_interm_3_pt4(b,j,i,k) * wm_interm_59_pt4(a,b,j,k)
term(123) = term(123) + r2(vrdav_Rl, a,i,p,q) * wm_interm_3_pt4(b,j,i,k) * wm_interm_60_pt4(a,b,j,k)
term(124) = term(124) + r2(vrdav_Rr, a,q,p,i) * wm_interm_49_pt4(a,b,j,k) * wm_interm_76_pt4(b,k,i,j)
term(125) = term(125) + r2(vrdav_Rr, a,i,p,q) * wm_interm_49_pt4(a,b,j,k) * wm_interm_76_pt4(b,k,i,j)
term(126) = term(126) + r2(vrdav_Rr, a,q,p,i) * wm_interm_49_pt4(a,b,j,k) * wm_interm_76_pt4(b,i,k,j)
term(127) = term(127) + r2(vrdav_Rr, a,q,p,i) * wm_interm_48_pt4(a,b,j,k) * wm_interm_76_pt4(b,k,i,j)
term(128) = term(128) + r2(vrdav_Rr, a,i,p,q) * wm_interm_48_pt4(a,b,j,k) * wm_interm_76_pt4(b,k,i,j)
term(129) = term(129) + r2(vrdav_Rr, a,q,p,i) * wm_interm_48_pt4(a,b,j,k) * wm_interm_76_pt4(b,i,k,j)
term(130) = term(130) + r2(vrdav_Rr, a,q,p,i) * wm_interm_23_pt4(b,j,i,k) * wm_interm_50_pt4(a,b,k,j)
term(131) = term(131) + r2(vrdav_Rr, a,i,p,q) * wm_interm_23_pt4(b,j,i,k) * wm_interm_50_pt4(a,b,k,j)
term(132) = term(132) + r2(vrdav_Rr, a,q,p,i) * wm_interm_23_pt4(b,i,j,k) * wm_interm_50_pt4(a,b,k,j)
term(133) = term(133) + r2(vrdav_Rr, a,q,p,i) * wm_interm_23_pt4(b,i,j,k) * wm_interm_53_pt4(a,b,k,j)
term(134) = term(134) + r2(vrdav_Rr, a,q,p,i) * wm_interm_23_pt4(b,j,i,k) * wm_interm_53_pt4(a,b,k,j)
term(135) = term(135) + r2(vrdav_Rr, a,i,p,q) * wm_interm_23_pt4(b,j,i,k) * wm_interm_53_pt4(a,b,k,j)
term(136) = term(136) + r2(vrdav_Rr, a,q,p,i) * wm_interm_11_pt4(a,b,j,k) * wm_interm_42_pt4(b,i,k,j)
term(137) = term(137) + r2(vrdav_Rr, a,q,p,i) * wm_interm_11_pt4(a,b,j,k) * wm_interm_42_pt4(b,k,i,j)
term(138) = term(138) + r2(vrdav_Rr, a,i,p,q) * wm_interm_11_pt4(a,b,j,k) * wm_interm_42_pt4(b,k,i,j)
term(139) = term(139) + r2(vrdav_Rr, a,q,p,i) * wm_interm_12_pt4(a,b,j,k) * wm_interm_42_pt4(b,i,k,j)
term(140) = term(140) + r2(vrdav_Rr, a,q,p,i) * wm_interm_12_pt4(a,b,j,k) * wm_interm_42_pt4(b,k,i,j)
term(141) = term(141) + r2(vrdav_Rr, a,i,p,q) * wm_interm_12_pt4(a,b,j,k) * wm_interm_42_pt4(b,k,i,j)
term(142) = term(142) + r2(vrdav_Rr, a,q,p,i) * wm_interm_1_pt4(a,b,j,k) * wm_interm_42_pt4(b,i,k,j)
term(143) = term(143) + r2(vrdav_Rr, a,q,p,i) * wm_interm_42_pt4(b,i,j,k) * wm_interm_5_pt4(a,b,k,j)
term(144) = term(144) + r2(vrdav_Rr, a,q,p,i) * wm_interm_42_pt4(b,j,i,k) * wm_interm_5_pt4(a,b,k,j)
term(145) = term(145) + r2(vrdav_Rr, a,i,p,q) * wm_interm_42_pt4(b,j,i,k) * wm_interm_5_pt4(a,b,k,j)
term(146) = term(146) + r2(vrdav_Rr, a,q,p,i) * wm_interm_1_pt4(a,b,j,k) * wm_interm_42_pt4(b,k,i,j)
term(147) = term(147) + r2(vrdav_Rr, a,i,p,q) * wm_interm_1_pt4(a,b,j,k) * wm_interm_42_pt4(b,k,i,j)
term(148) = term(148) + r2(vrdav_Rr, a,q,p,i) * wm_interm_50_pt4(a,b,j,k) * wm_interm_76_pt4(b,i,k,j)
term(149) = term(149) + r2(vrdav_Rr, a,q,p,i) * wm_interm_50_pt4(a,b,j,k) * wm_interm_76_pt4(b,k,i,j)
term(150) = term(150) + r2(vrdav_Rr, a,i,p,q) * wm_interm_50_pt4(a,b,j,k) * wm_interm_76_pt4(b,k,i,j)
term(151) = term(151) + r2(vrdav_Rr, a,q,p,i) * wm_interm_53_pt4(a,b,j,k) * wm_interm_76_pt4(b,k,i,j)
term(152) = term(152) + r2(vrdav_Rr, a,i,p,q) * wm_interm_53_pt4(a,b,j,k) * wm_interm_76_pt4(b,k,i,j)
term(153) = term(153) + r2(vrdav_Rr, a,q,p,i) * wm_interm_53_pt4(a,b,j,k) * wm_interm_76_pt4(b,i,k,j)
term(154) = term(154) + r2(vrdav_Rr, a,q,p,i) * wm_interm_11_pt4(a,b,j,k) * wm_interm_43_pt4(b,i,k,j)
term(155) = term(155) + r2(vrdav_Rr, a,q,p,i) * wm_interm_12_pt4(a,b,j,k) * wm_interm_43_pt4(b,i,k,j)
term(156) = term(156) + r2(vrdav_Rr, a,q,p,i) * wm_interm_11_pt4(a,b,j,k) * wm_interm_43_pt4(b,k,i,j)
term(157) = term(157) + r2(vrdav_Rr, a,i,p,q) * wm_interm_11_pt4(a,b,j,k) * wm_interm_43_pt4(b,k,i,j)
term(158) = term(158) + r2(vrdav_Rr, a,q,p,i) * wm_interm_12_pt4(a,b,j,k) * wm_interm_43_pt4(b,k,i,j)
term(159) = term(159) + r2(vrdav_Rr, a,i,p,q) * wm_interm_12_pt4(a,b,j,k) * wm_interm_43_pt4(b,k,i,j)
term(160) = term(160) + r2(vrdav_Rr, a,q,p,i) * wm_interm_43_pt4(b,i,j,k) * wm_interm_5_pt4(a,b,k,j)
term(161) = term(161) + r2(vrdav_Rr, a,q,p,i) * wm_interm_1_pt4(a,b,j,k) * wm_interm_43_pt4(b,i,k,j)
term(162) = term(162) + r2(vrdav_Rr, a,q,p,i) * wm_interm_1_pt4(a,b,j,k) * wm_interm_43_pt4(b,k,i,j)
term(163) = term(163) + r2(vrdav_Rr, a,i,p,q) * wm_interm_1_pt4(a,b,j,k) * wm_interm_43_pt4(b,k,i,j)
term(164) = term(164) + r2(vrdav_Rr, a,q,p,i) * wm_interm_43_pt4(b,j,i,k) * wm_interm_5_pt4(a,b,k,j)
term(165) = term(165) + r2(vrdav_Rr, a,i,p,q) * wm_interm_43_pt4(b,j,i,k) * wm_interm_5_pt4(a,b,k,j)
term(166) = term(166) + t2(a,p,i,q) * wm_interm_23_pt4(b,j,i,k) * wm_interm_57_pt4(a,b,k,j)
term(167) = term(167) + t2(a,p,i,q) * wm_interm_23_pt4(b,j,i,k) * wm_interm_58_pt4(a,b,k,j)
term(168) = term(168) + t2(a,p,i,q) * wm_interm_23_pt4(b,j,i,k) * wm_interm_64_pt4(a,b,k,j)
term(169) = term(169) + t2(a,p,i,q) * wm_interm_23_pt4(b,j,i,k) * wm_interm_65_pt4(a,b,k,j)
term(170) = term(170) + t2(a,p,q,i) * wm_interm_23_pt4(b,j,i,k) * wm_interm_57_pt4(a,b,k,j)
term(171) = term(171) + t2(a,p,q,i) * wm_interm_23_pt4(b,j,i,k) * wm_interm_58_pt4(a,b,k,j)
term(172) = term(172) + t2(a,p,q,i) * wm_interm_23_pt4(b,i,j,k) * wm_interm_58_pt4(a,b,k,j)
term(173) = term(173) + t2(a,p,q,i) * wm_interm_23_pt4(b,i,j,k) * wm_interm_57_pt4(a,b,k,j)
term(174) = term(174) + t2(a,p,q,i) * wm_interm_23_pt4(b,i,j,k) * wm_interm_64_pt4(a,b,k,j)
term(175) = term(175) + t2(a,p,q,i) * wm_interm_23_pt4(b,i,j,k) * wm_interm_65_pt4(a,b,k,j)
term(176) = term(176) + t2(a,p,q,i) * wm_interm_23_pt4(b,j,i,k) * wm_interm_64_pt4(a,b,k,j)
term(177) = term(177) + t2(a,p,q,i) * wm_interm_23_pt4(b,j,i,k) * wm_interm_65_pt4(a,b,k,j)
end do 
end do 
end do 
end do 
end do 

term(120) = term(120) * (-8.0d+0) 
term(121) = term(121) * (16.0d+0) 
term(122) = term(122) * (16.0d+0) 
term(123) = term(123) * (-32.0d+0) 
term(124) = term(124) * (-4.0d+0) 
term(125) = term(125) * (8.0d+0) 
term(126) = term(126) * (8.0d+0) 
term(127) = term(127) * (8.0d+0) 
term(128) = term(128) * (-16.0d+0) 
term(129) = term(129) * (-16.0d+0) 
term(130) = term(130) * (-2.0d+0) 
term(131) = term(131) * (4.0d+0) 
term(132) = term(132) * (4.0d+0) 
term(133) = term(133) * (-2.0d+0) 
term(134) = term(134) * (4.0d+0) 
term(135) = term(135) * (-8.0d+0) 
term(136) = term(136) * (-2.0d+0) 
term(137) = term(137) * (4.0d+0) 
term(138) = term(138) * (-8.0d+0) 
term(139) = term(139) * (4.0d+0) 
term(140) = term(140) * (-8.0d+0) 
term(141) = term(141) * (16.0d+0) 
term(142) = term(142) * (-2.0d+0) 
term(143) = term(143) * (4.0d+0) 
term(144) = term(144) * (-2.0d+0) 
term(145) = term(145) * (4.0d+0) 
term(146) = term(146) * (4.0d+0) 
term(147) = term(147) * (-8.0d+0) 
term(148) = term(148) * (-2.0d+0) 
term(149) = term(149) * (4.0d+0) 
term(150) = term(150) * (-8.0d+0) 
term(151) = term(151) * (-2.0d+0) 
term(152) = term(152) * (4.0d+0) 
term(153) = term(153) * (4.0d+0) 
term(154) = term(154) * (4.0d+0) 
term(155) = term(155) * (-8.0d+0) 
term(156) = term(156) * (-2.0d+0) 
term(157) = term(157) * (4.0d+0) 
term(158) = term(158) * (4.0d+0) 
term(159) = term(159) * (-8.0d+0) 
term(160) = term(160) * (-2.0d+0) 
term(161) = term(161) * (4.0d+0) 
term(162) = term(162) * (-2.0d+0) 
term(163) = term(163) * (4.0d+0) 
term(164) = term(164) * (4.0d+0) 
term(165) = term(165) * (-8.0d+0) 
term(166) = term(166) * (4.0d+0) 
term(167) = term(167) * (-8.0d+0) 
term(168) = term(168) * (-8.0d+0) 
term(169) = term(169) * (16.0d+0) 
term(170) = term(170) * (-2.0d+0) 
term(171) = term(171) * (4.0d+0) 
term(172) = term(172) * (-2.0d+0) 
term(173) = term(173) * (4.0d+0) 
term(174) = term(174) * (-2.0d+0) 
term(175) = term(175) * (4.0d+0) 
term(176) = term(176) * (4.0d+0) 
term(177) = term(177) * (-8.0d+0) 

do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(178) = term(178) + r1(vrdav_Rl, b,q) * wm_interm_0_pt4(a,p) * wm_interm_20_pt4(a,b)
term(179) = term(179) + s1(p,q) * wm_interm_16_pt4(a,b) * wm_interm_20_pt4(b,a)
term(180) = term(180) + r1(vrdav_Rl, b,q) * wm_interm_0_pt4(a,p) * wm_interm_21_pt4(a,b)
term(181) = term(181) + s1(p,q) * wm_interm_16_pt4(a,b) * wm_interm_21_pt4(b,a)
term(182) = term(182) + t1(p,q) * wm_interm_16_pt4(a,b) * wm_interm_20_pt4(b,a)
term(183) = term(183) + t1(p,q) * wm_interm_16_pt4(a,b) * wm_interm_21_pt4(b,a)
term(184) = term(184) + s1(p,q) * wm_interm_20_pt4(a,b) * wm_interm_74_pt4(b,a)
term(185) = term(185) + s1(p,q) * wm_interm_20_pt4(a,b) * wm_interm_75_pt4(b,a)
term(186) = term(186) + s1(p,q) * wm_interm_21_pt4(a,b) * wm_interm_74_pt4(b,a)
term(187) = term(187) + s1(p,q) * wm_interm_21_pt4(a,b) * wm_interm_75_pt4(b,a)
term(188) = term(188) + s1(p,q) * wm_interm_51_pt4(a,b) * wm_interm_68_pt4(a,b)
term(189) = term(189) + s1(p,q) * wm_interm_51_pt4(a,b) * wm_interm_69_pt4(a,b)
term(190) = term(190) + s1(p,q) * wm_interm_52_pt4(a,b) * wm_interm_68_pt4(a,b)
term(191) = term(191) + s1(p,q) * wm_interm_52_pt4(a,b) * wm_interm_69_pt4(a,b)
term(192) = term(192) + s1(b,q) * wm_interm_20_pt4(p,a) * wm_interm_74_pt4(a,b)
term(193) = term(193) + s1(b,q) * wm_interm_21_pt4(p,a) * wm_interm_74_pt4(a,b)
term(194) = term(194) + s1(b,q) * wm_interm_20_pt4(p,a) * wm_interm_75_pt4(a,b)
term(195) = term(195) + s1(b,q) * wm_interm_21_pt4(p,a) * wm_interm_75_pt4(a,b)
term(196) = term(196) + s1(b,q) * wm_interm_51_pt4(a,b) * wm_interm_68_pt4(a,p)
term(197) = term(197) + s1(b,q) * wm_interm_51_pt4(a,b) * wm_interm_69_pt4(a,p)
term(198) = term(198) + s1(b,q) * wm_interm_52_pt4(a,b) * wm_interm_68_pt4(a,p)
term(199) = term(199) + s1(b,q) * wm_interm_52_pt4(a,b) * wm_interm_69_pt4(a,p)
term(200) = term(200) + t1(p,q) * wm_interm_20_pt4(a,b) * wm_interm_74_pt4(b,a)
term(201) = term(201) + t1(p,q) * wm_interm_20_pt4(a,b) * wm_interm_75_pt4(b,a)
term(202) = term(202) + t1(p,q) * wm_interm_21_pt4(a,b) * wm_interm_74_pt4(b,a)
term(203) = term(203) + t1(p,q) * wm_interm_21_pt4(a,b) * wm_interm_75_pt4(b,a)
term(204) = term(204) + t1(p,q) * wm_interm_51_pt4(a,b) * wm_interm_68_pt4(a,b)
term(205) = term(205) + t1(p,q) * wm_interm_51_pt4(a,b) * wm_interm_69_pt4(a,b)
term(206) = term(206) + t1(p,q) * wm_interm_52_pt4(a,b) * wm_interm_68_pt4(a,b)
term(207) = term(207) + t1(p,q) * wm_interm_52_pt4(a,b) * wm_interm_69_pt4(a,b)
term(208) = term(208) + t1(b,q) * wm_interm_52_pt4(a,p) * wm_interm_68_pt4(a,b)
term(209) = term(209) + t1(b,q) * wm_interm_52_pt4(a,p) * wm_interm_69_pt4(a,b)
term(210) = term(210) + t1(b,q) * wm_interm_51_pt4(a,p) * wm_interm_68_pt4(a,b)
term(211) = term(211) + t1(b,q) * wm_interm_51_pt4(a,p) * wm_interm_69_pt4(a,b)
end do 
end do 

term(178) = term(178) * (2.0d+0) 
term(179) = term(179) * (-4.0d+0) 
term(180) = term(180) * (-4.0d+0) 
term(181) = term(181) * (8.0d+0) 
term(182) = term(182) * (-4.0d+0) 
term(183) = term(183) * (8.0d+0) 
term(184) = term(184) * (4.0d+0) 
term(185) = term(185) * (-8.0d+0) 
term(186) = term(186) * (-8.0d+0) 
term(187) = term(187) * (16.0d+0) 
term(188) = term(188) * (-8.0d+0) 
term(189) = term(189) * (16.0d+0) 
term(190) = term(190) * (4.0d+0) 
term(191) = term(191) * (-8.0d+0) 
term(192) = term(192) * (-2.0d+0) 
term(193) = term(193) * (4.0d+0) 
term(194) = term(194) * (4.0d+0) 
term(195) = term(195) * (-8.0d+0) 
term(196) = term(196) * (4.0d+0) 
term(197) = term(197) * (-8.0d+0) 
term(198) = term(198) * (-2.0d+0) 
term(199) = term(199) * (4.0d+0) 
term(200) = term(200) * (4.0d+0) 
term(201) = term(201) * (-8.0d+0) 
term(202) = term(202) * (-8.0d+0) 
term(203) = term(203) * (16.0d+0) 
term(204) = term(204) * (-8.0d+0) 
term(205) = term(205) * (16.0d+0) 
term(206) = term(206) * (4.0d+0) 
term(207) = term(207) * (-8.0d+0) 
term(208) = term(208) * (-4.0d+0) 
term(209) = term(209) * (8.0d+0) 
term(210) = term(210) * (8.0d+0) 
term(211) = term(211) * (-16.0d+0) 

do a = nocc + 1, nactive 
do i = 1, nocc 
term(212) = term(212) + r1(vrdav_Rl, a,i) * wm_interm_20_pt4(p,a) * wm_interm_9_pt4(i,q)
term(213) = term(213) + r1(vrdav_Rl, a,i) * wm_interm_21_pt4(p,a) * wm_interm_9_pt4(i,q)
term(214) = term(214) + t1(a,i) * wm_interm_56_pt4(i,q) * wm_interm_68_pt4(p,a)
term(215) = term(215) + t1(a,i) * wm_interm_56_pt4(i,q) * wm_interm_69_pt4(p,a)
term(216) = term(216) + t1(a,i) * wm_interm_54_pt4(i,q) * wm_interm_68_pt4(p,a)
term(217) = term(217) + t1(a,i) * wm_interm_54_pt4(i,q) * wm_interm_69_pt4(p,a)
end do 
end do 

term(212) = term(212) * (2.0d+0) 
term(213) = term(213) * (-4.0d+0) 
term(214) = term(214) * (4.0d+0) 
term(215) = term(215) * (-8.0d+0) 
term(216) = term(216) * (-2.0d+0) 
term(217) = term(217) * (4.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
term(218) = term(218) + s1(p,i) * wm_interm_10_pt4(q,j,k,i) * wm_interm_6_pt4(k,j)
term(219) = term(219) + s1(p,i) * wm_interm_10_pt4(j,q,k,i) * wm_interm_6_pt4(k,j)
term(220) = term(220) + s1(p,i) * wm_interm_10_pt4(q,j,k,i) * wm_interm_66_pt4(k,j)
term(221) = term(221) + s1(p,i) * wm_interm_10_pt4(q,j,k,i) * wm_interm_67_pt4(k,j)
term(222) = term(222) + s1(p,i) * wm_interm_10_pt4(j,q,k,i) * wm_interm_66_pt4(k,j)
term(223) = term(223) + s1(p,i) * wm_interm_10_pt4(j,q,k,i) * wm_interm_67_pt4(k,j)
term(224) = term(224) + s1(p,i) * wm_interm_55_pt4(q,j,k,i) * wm_interm_70_pt4(j,k)
term(225) = term(225) + s1(p,i) * wm_interm_55_pt4(q,j,k,i) * wm_interm_71_pt4(j,k)
term(226) = term(226) + s1(p,i) * wm_interm_55_pt4(j,q,k,i) * wm_interm_70_pt4(j,k)
term(227) = term(227) + s1(p,i) * wm_interm_55_pt4(j,q,k,i) * wm_interm_71_pt4(j,k)
end do 
end do 
end do 

term(218) = term(218) * (-4.0d+0) 
term(219) = term(219) * (8.0d+0) 
term(220) = term(220) * (4.0d+0) 
term(221) = term(221) * (-8.0d+0) 
term(222) = term(222) * (-8.0d+0) 
term(223) = term(223) * (16.0d+0) 
term(224) = term(224) * (2.0d+0) 
term(225) = term(225) * (-4.0d+0) 
term(226) = term(226) * (-4.0d+0) 
term(227) = term(227) * (8.0d+0) 

do j = 1, nocc 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(228) = term(228) + s1(a,i) * wm_interm_5_pt4(p,a,q,j) * wm_interm_6_pt4(j,i)
term(229) = term(229) + s1(a,i) * wm_interm_1_pt4(p,a,q,j) * wm_interm_6_pt4(j,i)
term(230) = term(230) + s1(a,i) * wm_interm_11_pt4(p,a,q,j) * wm_interm_6_pt4(j,i)
term(231) = term(231) + s1(a,i) * wm_interm_12_pt4(p,a,q,j) * wm_interm_6_pt4(j,i)
term(232) = term(232) + r1(vrdav_Rl, a,i) * wm_interm_1_pt4(p,a,q,j) * wm_interm_9_pt4(i,j)
term(233) = term(233) + r1(vrdav_Rl, a,i) * wm_interm_5_pt4(p,a,q,j) * wm_interm_9_pt4(i,j)
term(234) = term(234) + r1(vrdav_Rl, a,i) * wm_interm_11_pt4(p,a,q,j) * wm_interm_9_pt4(i,j)
term(235) = term(235) + r1(vrdav_Rl, a,i) * wm_interm_12_pt4(p,a,q,j) * wm_interm_9_pt4(i,j)
term(236) = term(236) + s1(a,i) * wm_interm_5_pt4(p,a,q,j) * wm_interm_66_pt4(j,i)
term(237) = term(237) + s1(a,i) * wm_interm_5_pt4(p,a,q,j) * wm_interm_67_pt4(j,i)
term(238) = term(238) + s1(a,i) * wm_interm_1_pt4(p,a,q,j) * wm_interm_66_pt4(j,i)
term(239) = term(239) + s1(a,i) * wm_interm_1_pt4(p,a,q,j) * wm_interm_67_pt4(j,i)
term(240) = term(240) + s1(a,i) * wm_interm_11_pt4(p,a,q,j) * wm_interm_66_pt4(j,i)
term(241) = term(241) + s1(a,i) * wm_interm_12_pt4(p,a,q,j) * wm_interm_66_pt4(j,i)
term(242) = term(242) + s1(a,i) * wm_interm_11_pt4(p,a,q,j) * wm_interm_67_pt4(j,i)
term(243) = term(243) + s1(a,i) * wm_interm_12_pt4(p,a,q,j) * wm_interm_67_pt4(j,i)
term(244) = term(244) + s1(a,i) * wm_interm_48_pt4(p,a,q,j) * wm_interm_70_pt4(i,j)
term(245) = term(245) + s1(a,i) * wm_interm_48_pt4(p,a,q,j) * wm_interm_71_pt4(i,j)
term(246) = term(246) + s1(a,i) * wm_interm_49_pt4(p,a,q,j) * wm_interm_70_pt4(i,j)
term(247) = term(247) + s1(a,i) * wm_interm_49_pt4(p,a,q,j) * wm_interm_71_pt4(i,j)
term(248) = term(248) + s1(a,i) * wm_interm_53_pt4(p,a,q,j) * wm_interm_70_pt4(i,j)
term(249) = term(249) + s1(a,i) * wm_interm_53_pt4(p,a,q,j) * wm_interm_71_pt4(i,j)
term(250) = term(250) + s1(a,i) * wm_interm_50_pt4(p,a,q,j) * wm_interm_70_pt4(i,j)
term(251) = term(251) + s1(a,i) * wm_interm_50_pt4(p,a,q,j) * wm_interm_71_pt4(i,j)
term(252) = term(252) + t1(a,i) * wm_interm_56_pt4(i,j) * wm_interm_62_pt4(p,a,q,j)
term(253) = term(253) + t1(a,i) * wm_interm_56_pt4(i,j) * wm_interm_61_pt4(p,a,q,j)
term(254) = term(254) + t1(a,i) * wm_interm_56_pt4(i,j) * wm_interm_59_pt4(p,a,q,j)
term(255) = term(255) + t1(a,i) * wm_interm_56_pt4(i,j) * wm_interm_60_pt4(p,a,q,j)
term(256) = term(256) + t1(a,i) * wm_interm_54_pt4(i,j) * wm_interm_62_pt4(p,a,q,j)
term(257) = term(257) + t1(a,i) * wm_interm_54_pt4(i,j) * wm_interm_61_pt4(p,a,q,j)
term(258) = term(258) + t1(a,i) * wm_interm_54_pt4(i,j) * wm_interm_59_pt4(p,a,q,j)
term(259) = term(259) + t1(a,i) * wm_interm_54_pt4(i,j) * wm_interm_60_pt4(p,a,q,j)
end do 
end do 
end do 

term(228) = term(228) * (-4.0d+0) 
term(229) = term(229) * (8.0d+0) 
term(230) = term(230) * (8.0d+0) 
term(231) = term(231) * (-16.0d+0) 
term(232) = term(232) * (8.0d+0) 
term(233) = term(233) * (-4.0d+0) 
term(234) = term(234) * (8.0d+0) 
term(235) = term(235) * (-16.0d+0) 
term(236) = term(236) * (4.0d+0) 
term(237) = term(237) * (-8.0d+0) 
term(238) = term(238) * (-8.0d+0) 
term(239) = term(239) * (16.0d+0) 
term(240) = term(240) * (-8.0d+0) 
term(241) = term(241) * (16.0d+0) 
term(242) = term(242) * (16.0d+0) 
term(243) = term(243) * (-32.0d+0) 
term(244) = term(244) * (16.0d+0) 
term(245) = term(245) * (-32.0d+0) 
term(246) = term(246) * (-8.0d+0) 
term(247) = term(247) * (16.0d+0) 
term(248) = term(248) * (-8.0d+0) 
term(249) = term(249) * (16.0d+0) 
term(250) = term(250) * (4.0d+0) 
term(251) = term(251) * (-8.0d+0) 
term(252) = term(252) * (8.0d+0) 
term(253) = term(253) * (-16.0d+0) 
term(254) = term(254) * (-16.0d+0) 
term(255) = term(255) * (32.0d+0) 
term(256) = term(256) * (-4.0d+0) 
term(257) = term(257) * (8.0d+0) 
term(258) = term(258) * (8.0d+0) 
term(259) = term(259) * (-16.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(260) = term(260) + s1(a,q) * wm_interm_5_pt4(p,a,i,j) * wm_interm_6_pt4(j,i)
term(261) = term(261) + s1(a,q) * wm_interm_1_pt4(p,a,i,j) * wm_interm_6_pt4(j,i)
term(262) = term(262) + s2(a,p,i,q) * wm_interm_6_pt4(j,i) * wm_interm_7_pt4(a,j)
term(263) = term(263) + s2(a,p,i,q) * wm_interm_6_pt4(j,i) * wm_interm_8_pt4(a,j)
term(264) = term(264) + s1(a,q) * wm_interm_11_pt4(p,a,i,j) * wm_interm_6_pt4(j,i)
term(265) = term(265) + s1(a,q) * wm_interm_12_pt4(p,a,i,j) * wm_interm_6_pt4(j,i)
term(266) = term(266) + r1(vrdav_Rl, a,i) * wm_interm_13_pt4(p,i,q,j) * wm_interm_8_pt4(a,j)
term(267) = term(267) + r1(vrdav_Rl, a,i) * wm_interm_13_pt4(p,i,j,q) * wm_interm_8_pt4(a,j)
term(268) = term(268) + s1(a,i) * wm_interm_13_pt4(p,i,q,j) * wm_interm_15_pt4(a,j)
term(269) = term(269) + s1(a,i) * wm_interm_13_pt4(p,i,j,q) * wm_interm_15_pt4(a,j)
term(270) = term(270) + r1(vrdav_Rl, a,i) * wm_interm_13_pt4(p,i,q,j) * wm_interm_7_pt4(a,j)
term(271) = term(271) + r1(vrdav_Rl, a,i) * wm_interm_13_pt4(p,i,j,q) * wm_interm_7_pt4(a,j)
term(272) = term(272) + r1(vrdav_Rl, a,q) * wm_interm_1_pt4(p,a,i,j) * wm_interm_9_pt4(i,j)
term(273) = term(273) + r1(vrdav_Rl, a,q) * wm_interm_5_pt4(p,a,i,j) * wm_interm_9_pt4(i,j)
term(274) = term(274) + s2(a,p,i,q) * wm_interm_15_pt4(a,j) * wm_interm_9_pt4(i,j)
term(275) = term(275) + s1(p,i) * wm_interm_14_pt4(a,q,i,j) * wm_interm_4_pt4(a,j)
term(276) = term(276) + s1(p,i) * wm_interm_14_pt4(a,q,i,j) * wm_interm_2_pt4(a,j)
term(277) = term(277) + r1(vrdav_Rl, a,q) * wm_interm_11_pt4(p,a,i,j) * wm_interm_9_pt4(i,j)
term(278) = term(278) + r1(vrdav_Rl, a,q) * wm_interm_12_pt4(p,a,i,j) * wm_interm_9_pt4(i,j)
term(279) = term(279) + s1(a,i) * wm_interm_13_pt4(p,i,q,j) * wm_interm_17_pt4(a,j)
term(280) = term(280) + s1(a,i) * wm_interm_13_pt4(p,i,j,q) * wm_interm_17_pt4(a,j)
term(281) = term(281) + s2(a,p,i,q) * wm_interm_17_pt4(a,j) * wm_interm_9_pt4(i,j)
term(282) = term(282) + s1(p,i) * wm_interm_13_pt4(a,i,j,q) * wm_interm_15_pt4(a,j)
term(283) = term(283) + s1(p,i) * wm_interm_13_pt4(a,i,q,j) * wm_interm_15_pt4(a,j)
term(284) = term(284) + s1(p,i) * wm_interm_13_pt4(a,i,j,q) * wm_interm_17_pt4(a,j)
term(285) = term(285) + s1(p,i) * wm_interm_13_pt4(a,i,q,j) * wm_interm_17_pt4(a,j)
term(286) = term(286) + s2(a,p,i,j) * wm_interm_15_pt4(a,j) * wm_interm_9_pt4(i,q)
term(287) = term(287) + s2(a,p,i,j) * wm_interm_17_pt4(a,j) * wm_interm_9_pt4(i,q)
term(288) = term(288) + r1(vrdav_Rr, p,i) * wm_interm_15_pt4(a,j) * wm_interm_23_pt4(a,i,j,q)
term(289) = term(289) + r1(vrdav_Rr, p,i) * wm_interm_17_pt4(a,j) * wm_interm_23_pt4(a,i,j,q)
term(290) = term(290) + t1(a,i) * wm_interm_14_pt4(p,i,j,q) * wm_interm_4_pt4(a,j)
term(291) = term(291) + t1(a,i) * wm_interm_14_pt4(p,i,j,q) * wm_interm_2_pt4(a,j)
term(292) = term(292) + t1(a,i) * wm_interm_11_pt4(a,p,j,q) * wm_interm_6_pt4(i,j)
term(293) = term(293) + t1(a,i) * wm_interm_12_pt4(a,p,j,q) * wm_interm_6_pt4(i,j)
term(294) = term(294) + r1(vrdav_Rr, a,i) * wm_interm_11_pt4(a,p,j,q) * wm_interm_22_pt4(i,j)
term(295) = term(295) + t2(a,p,i,q) * wm_interm_22_pt4(i,j) * wm_interm_2_pt4(a,j)
term(296) = term(296) + t2(a,p,i,q) * wm_interm_22_pt4(i,j) * wm_interm_4_pt4(a,j)
term(297) = term(297) + r1(vrdav_Rr, a,i) * wm_interm_12_pt4(a,p,j,q) * wm_interm_22_pt4(i,j)
term(298) = term(298) + t1(a,i) * wm_interm_14_pt4(p,i,q,j) * wm_interm_4_pt4(a,j)
term(299) = term(299) + t1(a,i) * wm_interm_14_pt4(p,i,q,j) * wm_interm_2_pt4(a,j)
term(300) = term(300) + t1(a,i) * wm_interm_5_pt4(a,p,j,q) * wm_interm_6_pt4(i,j)
term(301) = term(301) + t1(a,i) * wm_interm_1_pt4(a,p,j,q) * wm_interm_6_pt4(i,j)
term(302) = term(302) + r1(vrdav_Rr, a,i) * wm_interm_22_pt4(i,j) * wm_interm_5_pt4(a,p,j,q)
term(303) = term(303) + r1(vrdav_Rr, a,i) * wm_interm_1_pt4(a,p,j,q) * wm_interm_22_pt4(i,j)
term(304) = term(304) + t1(a,q) * wm_interm_11_pt4(a,p,i,j) * wm_interm_6_pt4(j,i)
term(305) = term(305) + t1(a,q) * wm_interm_1_pt4(a,p,i,j) * wm_interm_6_pt4(j,i)
term(306) = term(306) + t1(a,q) * wm_interm_5_pt4(a,p,i,j) * wm_interm_6_pt4(j,i)
term(307) = term(307) + t1(a,q) * wm_interm_12_pt4(a,p,i,j) * wm_interm_6_pt4(j,i)
term(308) = term(308) + t1(p,i) * wm_interm_14_pt4(a,i,q,j) * wm_interm_4_pt4(a,j)
term(309) = term(309) + t1(p,i) * wm_interm_14_pt4(a,i,q,j) * wm_interm_2_pt4(a,j)
term(310) = term(310) + t1(p,i) * wm_interm_14_pt4(a,i,j,q) * wm_interm_2_pt4(a,j)
term(311) = term(311) + t1(p,i) * wm_interm_14_pt4(a,i,j,q) * wm_interm_4_pt4(a,j)
term(312) = term(312) + r2(vrdav_Rl, a,i,p,q) * wm_interm_18_pt4(j,i) * wm_interm_31_pt4(a,j)
term(313) = term(313) + r2(vrdav_Rl, a,i,p,q) * wm_interm_19_pt4(j,i) * wm_interm_31_pt4(a,j)
term(314) = term(314) + r2(vrdav_Rl, a,i,p,q) * wm_interm_18_pt4(j,i) * wm_interm_34_pt4(a,j)
term(315) = term(315) + r2(vrdav_Rl, a,i,p,q) * wm_interm_19_pt4(j,i) * wm_interm_34_pt4(a,j)
term(316) = term(316) + s2(a,p,i,j) * wm_interm_31_pt4(a,j) * wm_interm_54_pt4(q,i)
term(317) = term(317) + s2(a,p,i,q) * wm_interm_31_pt4(a,j) * wm_interm_54_pt4(j,i)
term(318) = term(318) + s2(a,p,i,j) * wm_interm_31_pt4(a,j) * wm_interm_56_pt4(q,i)
term(319) = term(319) + s2(a,p,i,q) * wm_interm_31_pt4(a,j) * wm_interm_56_pt4(j,i)
term(320) = term(320) + s2(a,p,i,j) * wm_interm_34_pt4(a,j) * wm_interm_54_pt4(q,i)
term(321) = term(321) + s2(a,p,i,q) * wm_interm_34_pt4(a,j) * wm_interm_54_pt4(j,i)
term(322) = term(322) + s2(a,p,i,j) * wm_interm_34_pt4(a,j) * wm_interm_56_pt4(q,i)
term(323) = term(323) + s2(a,p,i,q) * wm_interm_34_pt4(a,j) * wm_interm_56_pt4(j,i)
term(324) = term(324) + s1(a,q) * wm_interm_5_pt4(p,a,i,j) * wm_interm_66_pt4(j,i)
term(325) = term(325) + s1(a,q) * wm_interm_5_pt4(p,a,i,j) * wm_interm_67_pt4(j,i)
term(326) = term(326) + s1(a,q) * wm_interm_1_pt4(p,a,i,j) * wm_interm_66_pt4(j,i)
term(327) = term(327) + s1(a,q) * wm_interm_1_pt4(p,a,i,j) * wm_interm_67_pt4(j,i)
term(328) = term(328) + s2(a,p,i,q) * wm_interm_66_pt4(j,i) * wm_interm_7_pt4(a,j)
term(329) = term(329) + s2(a,p,i,q) * wm_interm_67_pt4(j,i) * wm_interm_7_pt4(a,j)
term(330) = term(330) + s2(a,p,i,q) * wm_interm_66_pt4(j,i) * wm_interm_8_pt4(a,j)
term(331) = term(331) + s2(a,p,i,q) * wm_interm_67_pt4(j,i) * wm_interm_8_pt4(a,j)
term(332) = term(332) + s1(a,q) * wm_interm_11_pt4(p,a,i,j) * wm_interm_66_pt4(j,i)
term(333) = term(333) + s1(a,q) * wm_interm_11_pt4(p,a,i,j) * wm_interm_67_pt4(j,i)
term(334) = term(334) + s1(a,q) * wm_interm_12_pt4(p,a,i,j) * wm_interm_66_pt4(j,i)
term(335) = term(335) + s1(a,q) * wm_interm_12_pt4(p,a,i,j) * wm_interm_67_pt4(j,i)
term(336) = term(336) + r2(vrdav_Rl, a,i,p,q) * wm_interm_70_pt4(i,j) * wm_interm_8_pt4(a,j)
term(337) = term(337) + r2(vrdav_Rl, a,i,p,q) * wm_interm_71_pt4(i,j) * wm_interm_8_pt4(a,j)
term(338) = term(338) + r2(vrdav_Rl, a,i,p,q) * wm_interm_70_pt4(i,j) * wm_interm_7_pt4(a,j)
term(339) = term(339) + r2(vrdav_Rl, a,i,p,q) * wm_interm_71_pt4(i,j) * wm_interm_7_pt4(a,j)
term(340) = term(340) + t1(a,i) * wm_interm_18_pt4(i,j) * wm_interm_64_pt4(a,p,j,q)
term(341) = term(341) + t1(a,i) * wm_interm_19_pt4(i,j) * wm_interm_64_pt4(a,p,j,q)
term(342) = term(342) + t1(a,i) * wm_interm_18_pt4(i,j) * wm_interm_65_pt4(a,p,j,q)
term(343) = term(343) + t1(a,i) * wm_interm_19_pt4(i,j) * wm_interm_65_pt4(a,p,j,q)
term(344) = term(344) + t1(a,i) * wm_interm_18_pt4(i,j) * wm_interm_57_pt4(a,p,j,q)
term(345) = term(345) + t1(a,i) * wm_interm_18_pt4(i,j) * wm_interm_58_pt4(a,p,j,q)
term(346) = term(346) + r2(vrdav_Rr, a,i,p,q) * wm_interm_18_pt4(i,j) * wm_interm_44_pt4(a,j)
term(347) = term(347) + t1(a,i) * wm_interm_19_pt4(i,j) * wm_interm_57_pt4(a,p,j,q)
term(348) = term(348) + t1(a,i) * wm_interm_19_pt4(i,j) * wm_interm_58_pt4(a,p,j,q)
term(349) = term(349) + r2(vrdav_Rr, a,i,p,q) * wm_interm_19_pt4(i,j) * wm_interm_44_pt4(a,j)
term(350) = term(350) + r2(vrdav_Rr, a,i,p,q) * wm_interm_18_pt4(i,j) * wm_interm_45_pt4(a,j)
term(351) = term(351) + r2(vrdav_Rr, a,i,p,q) * wm_interm_19_pt4(i,j) * wm_interm_45_pt4(a,j)
term(352) = term(352) + s1(a,q) * wm_interm_54_pt4(i,j) * wm_interm_59_pt4(a,p,i,j)
term(353) = term(353) + s1(a,q) * wm_interm_54_pt4(i,j) * wm_interm_60_pt4(a,p,i,j)
term(354) = term(354) + s1(a,q) * wm_interm_54_pt4(i,j) * wm_interm_61_pt4(a,p,i,j)
term(355) = term(355) + s1(a,q) * wm_interm_54_pt4(i,j) * wm_interm_62_pt4(a,p,i,j)
term(356) = term(356) + s1(a,q) * wm_interm_56_pt4(i,j) * wm_interm_59_pt4(a,p,i,j)
term(357) = term(357) + s1(a,q) * wm_interm_56_pt4(i,j) * wm_interm_60_pt4(a,p,i,j)
term(358) = term(358) + s1(a,q) * wm_interm_56_pt4(i,j) * wm_interm_61_pt4(a,p,i,j)
term(359) = term(359) + s1(a,q) * wm_interm_56_pt4(i,j) * wm_interm_62_pt4(a,p,i,j)
term(360) = term(360) + s1(a,i) * wm_interm_54_pt4(j,i) * wm_interm_62_pt4(a,p,j,q)
term(361) = term(361) + s1(a,i) * wm_interm_54_pt4(j,i) * wm_interm_61_pt4(a,p,j,q)
term(362) = term(362) + s1(a,i) * wm_interm_56_pt4(j,i) * wm_interm_62_pt4(a,p,j,q)
term(363) = term(363) + s1(a,i) * wm_interm_56_pt4(j,i) * wm_interm_61_pt4(a,p,j,q)
term(364) = term(364) + s1(a,i) * wm_interm_54_pt4(j,i) * wm_interm_59_pt4(a,p,j,q)
term(365) = term(365) + s1(a,i) * wm_interm_54_pt4(j,i) * wm_interm_60_pt4(a,p,j,q)
term(366) = term(366) + s1(a,i) * wm_interm_56_pt4(j,i) * wm_interm_59_pt4(a,p,j,q)
term(367) = term(367) + s1(a,i) * wm_interm_56_pt4(j,i) * wm_interm_60_pt4(a,p,j,q)
term(368) = term(368) + t1(a,q) * wm_interm_54_pt4(i,j) * wm_interm_59_pt4(p,a,i,j)
term(369) = term(369) + t1(a,q) * wm_interm_54_pt4(i,j) * wm_interm_60_pt4(p,a,i,j)
term(370) = term(370) + t1(a,q) * wm_interm_54_pt4(i,j) * wm_interm_61_pt4(p,a,i,j)
term(371) = term(371) + t1(a,q) * wm_interm_54_pt4(i,j) * wm_interm_62_pt4(p,a,i,j)
term(372) = term(372) + t1(a,q) * wm_interm_56_pt4(i,j) * wm_interm_59_pt4(p,a,i,j)
term(373) = term(373) + t1(a,q) * wm_interm_56_pt4(i,j) * wm_interm_60_pt4(p,a,i,j)
term(374) = term(374) + t1(a,q) * wm_interm_56_pt4(i,j) * wm_interm_61_pt4(p,a,i,j)
term(375) = term(375) + t1(a,q) * wm_interm_56_pt4(i,j) * wm_interm_62_pt4(p,a,i,j)
term(376) = term(376) + t1(a,i) * wm_interm_11_pt4(a,p,j,q) * wm_interm_66_pt4(i,j)
term(377) = term(377) + t1(a,i) * wm_interm_11_pt4(a,p,j,q) * wm_interm_67_pt4(i,j)
term(378) = term(378) + t1(a,i) * wm_interm_12_pt4(a,p,j,q) * wm_interm_66_pt4(i,j)
term(379) = term(379) + t1(a,i) * wm_interm_12_pt4(a,p,j,q) * wm_interm_67_pt4(i,j)
term(380) = term(380) + t1(a,i) * wm_interm_49_pt4(a,p,j,q) * wm_interm_70_pt4(j,i)
term(381) = term(381) + t1(a,i) * wm_interm_49_pt4(a,p,j,q) * wm_interm_71_pt4(j,i)
term(382) = term(382) + t1(a,i) * wm_interm_48_pt4(a,p,j,q) * wm_interm_70_pt4(j,i)
term(383) = term(383) + t1(a,i) * wm_interm_48_pt4(a,p,j,q) * wm_interm_71_pt4(j,i)
term(384) = term(384) + t1(a,i) * wm_interm_5_pt4(a,p,j,q) * wm_interm_66_pt4(i,j)
term(385) = term(385) + t1(a,i) * wm_interm_5_pt4(a,p,j,q) * wm_interm_67_pt4(i,j)
term(386) = term(386) + t1(a,i) * wm_interm_1_pt4(a,p,j,q) * wm_interm_66_pt4(i,j)
term(387) = term(387) + t1(a,i) * wm_interm_1_pt4(a,p,j,q) * wm_interm_67_pt4(i,j)
term(388) = term(388) + t1(a,i) * wm_interm_50_pt4(a,p,j,q) * wm_interm_70_pt4(j,i)
term(389) = term(389) + t1(a,i) * wm_interm_50_pt4(a,p,j,q) * wm_interm_71_pt4(j,i)
term(390) = term(390) + t1(a,i) * wm_interm_53_pt4(a,p,j,q) * wm_interm_70_pt4(j,i)
term(391) = term(391) + t1(a,i) * wm_interm_53_pt4(a,p,j,q) * wm_interm_71_pt4(j,i)
term(392) = term(392) + t2(a,p,i,q) * wm_interm_44_pt4(a,j) * wm_interm_70_pt4(j,i)
term(393) = term(393) + t2(a,p,i,q) * wm_interm_44_pt4(a,j) * wm_interm_71_pt4(j,i)
term(394) = term(394) + t2(a,p,i,q) * wm_interm_45_pt4(a,j) * wm_interm_70_pt4(j,i)
term(395) = term(395) + t2(a,p,i,q) * wm_interm_45_pt4(a,j) * wm_interm_71_pt4(j,i)
term(396) = term(396) + t1(a,q) * wm_interm_11_pt4(a,p,i,j) * wm_interm_66_pt4(j,i)
term(397) = term(397) + t1(a,q) * wm_interm_11_pt4(a,p,i,j) * wm_interm_67_pt4(j,i)
term(398) = term(398) + t1(a,q) * wm_interm_1_pt4(a,p,i,j) * wm_interm_66_pt4(j,i)
term(399) = term(399) + t1(a,q) * wm_interm_1_pt4(a,p,i,j) * wm_interm_67_pt4(j,i)
term(400) = term(400) + t1(a,q) * wm_interm_5_pt4(a,p,i,j) * wm_interm_66_pt4(j,i)
term(401) = term(401) + t1(a,q) * wm_interm_5_pt4(a,p,i,j) * wm_interm_67_pt4(j,i)
term(402) = term(402) + t1(a,q) * wm_interm_12_pt4(a,p,i,j) * wm_interm_66_pt4(j,i)
term(403) = term(403) + t1(a,q) * wm_interm_12_pt4(a,p,i,j) * wm_interm_67_pt4(j,i)
term(404) = term(404) + t1(a,q) * wm_interm_53_pt4(a,p,i,j) * wm_interm_70_pt4(i,j)
term(405) = term(405) + t1(a,q) * wm_interm_50_pt4(a,p,i,j) * wm_interm_70_pt4(i,j)
term(406) = term(406) + t1(a,q) * wm_interm_53_pt4(a,p,i,j) * wm_interm_71_pt4(i,j)
term(407) = term(407) + t1(a,q) * wm_interm_50_pt4(a,p,i,j) * wm_interm_71_pt4(i,j)
term(408) = term(408) + t1(a,q) * wm_interm_49_pt4(a,p,i,j) * wm_interm_70_pt4(i,j)
term(409) = term(409) + t1(a,q) * wm_interm_48_pt4(a,p,i,j) * wm_interm_70_pt4(i,j)
term(410) = term(410) + t1(a,q) * wm_interm_49_pt4(a,p,i,j) * wm_interm_71_pt4(i,j)
term(411) = term(411) + t1(a,q) * wm_interm_48_pt4(a,p,i,j) * wm_interm_71_pt4(i,j)
end do 
end do 
end do 

term(260) = term(260) * (8.0d+0) 
term(261) = term(261) * (-4.0d+0) 
term(262) = term(262) * (-16.0d+0) 
term(263) = term(263) * (8.0d+0) 
term(264) = term(264) * (-4.0d+0) 
term(265) = term(265) * (8.0d+0) 
term(266) = term(266) * (-4.0d+0) 
term(267) = term(267) * (8.0d+0) 
term(268) = term(268) * (8.0d+0) 
term(269) = term(269) * (-16.0d+0) 
term(270) = term(270) * (8.0d+0) 
term(271) = term(271) * (-16.0d+0) 
term(272) = term(272) * (-4.0d+0) 
term(273) = term(273) * (8.0d+0) 
term(274) = term(274) * (-16.0d+0) 
term(275) = term(275) * (8.0d+0) 
term(276) = term(276) * (-16.0d+0) 
term(277) = term(277) * (-4.0d+0) 
term(278) = term(278) * (8.0d+0) 
term(279) = term(279) * (-4.0d+0) 
term(280) = term(280) * (8.0d+0) 
term(281) = term(281) * (8.0d+0) 
term(282) = term(282) * (-4.0d+0) 
term(283) = term(283) * (8.0d+0) 
term(284) = term(284) * (2.0d+0) 
term(285) = term(285) * (-4.0d+0) 
term(286) = term(286) * (-4.0d+0) 
term(287) = term(287) * (2.0d+0) 
term(288) = term(288) * (-4.0d+0) 
term(289) = term(289) * (2.0d+0) 
term(290) = term(290) * (-4.0d+0) 
term(291) = term(291) * (8.0d+0) 
term(292) = term(292) * (-4.0d+0) 
term(293) = term(293) * (8.0d+0) 
term(294) = term(294) * (-4.0d+0) 
term(295) = term(295) * (8.0d+0) 
term(296) = term(296) * (-4.0d+0) 
term(297) = term(297) * (8.0d+0) 
term(298) = term(298) * (2.0d+0) 
term(299) = term(299) * (-4.0d+0) 
term(300) = term(300) * (2.0d+0) 
term(301) = term(301) * (-4.0d+0) 
term(302) = term(302) * (2.0d+0) 
term(303) = term(303) * (-4.0d+0) 
term(304) = term(304) * (4.0d+0) 
term(305) = term(305) * (4.0d+0) 
term(306) = term(306) * (-8.0d+0) 
term(307) = term(307) * (-8.0d+0) 
term(308) = term(308) * (-8.0d+0) 
term(309) = term(309) * (16.0d+0) 
term(310) = term(310) * (-8.0d+0) 
term(311) = term(311) * (4.0d+0) 
term(312) = term(312) * (8.0d+0) 
term(313) = term(313) * (-16.0d+0) 
term(314) = term(314) * (-4.0d+0) 
term(315) = term(315) * (8.0d+0) 
term(316) = term(316) * (-4.0d+0) 
term(317) = term(317) * (8.0d+0) 
term(318) = term(318) * (8.0d+0) 
term(319) = term(319) * (-16.0d+0) 
term(320) = term(320) * (2.0d+0) 
term(321) = term(321) * (-4.0d+0) 
term(322) = term(322) * (-4.0d+0) 
term(323) = term(323) * (8.0d+0) 
term(324) = term(324) * (-8.0d+0) 
term(325) = term(325) * (16.0d+0) 
term(326) = term(326) * (4.0d+0) 
term(327) = term(327) * (-8.0d+0) 
term(328) = term(328) * (16.0d+0) 
term(329) = term(329) * (-32.0d+0) 
term(330) = term(330) * (-8.0d+0) 
term(331) = term(331) * (16.0d+0) 
term(332) = term(332) * (4.0d+0) 
term(333) = term(333) * (-8.0d+0) 
term(334) = term(334) * (-8.0d+0) 
term(335) = term(335) * (16.0d+0) 
term(336) = term(336) * (-8.0d+0) 
term(337) = term(337) * (16.0d+0) 
term(338) = term(338) * (16.0d+0) 
term(339) = term(339) * (-32.0d+0) 
term(340) = term(340) * (8.0d+0) 
term(341) = term(341) * (-16.0d+0) 
term(342) = term(342) * (-16.0d+0) 
term(343) = term(343) * (32.0d+0) 
term(344) = term(344) * (-4.0d+0) 
term(345) = term(345) * (8.0d+0) 
term(346) = term(346) * (-16.0d+0) 
term(347) = term(347) * (8.0d+0) 
term(348) = term(348) * (-16.0d+0) 
term(349) = term(349) * (32.0d+0) 
term(350) = term(350) * (8.0d+0) 
term(351) = term(351) * (-16.0d+0) 
term(352) = term(352) * (-2.0d+0) 
term(353) = term(353) * (4.0d+0) 
term(354) = term(354) * (-2.0d+0) 
term(355) = term(355) * (4.0d+0) 
term(356) = term(356) * (4.0d+0) 
term(357) = term(357) * (-8.0d+0) 
term(358) = term(358) * (4.0d+0) 
term(359) = term(359) * (-8.0d+0) 
term(360) = term(360) * (-2.0d+0) 
term(361) = term(361) * (4.0d+0) 
term(362) = term(362) * (4.0d+0) 
term(363) = term(363) * (-8.0d+0) 
term(364) = term(364) * (4.0d+0) 
term(365) = term(365) * (-8.0d+0) 
term(366) = term(366) * (-8.0d+0) 
term(367) = term(367) * (16.0d+0) 
term(368) = term(368) * (-2.0d+0) 
term(369) = term(369) * (4.0d+0) 
term(370) = term(370) * (-2.0d+0) 
term(371) = term(371) * (4.0d+0) 
term(372) = term(372) * (4.0d+0) 
term(373) = term(373) * (-8.0d+0) 
term(374) = term(374) * (4.0d+0) 
term(375) = term(375) * (-8.0d+0) 
term(376) = term(376) * (4.0d+0) 
term(377) = term(377) * (-8.0d+0) 
term(378) = term(378) * (-8.0d+0) 
term(379) = term(379) * (16.0d+0) 
term(380) = term(380) * (4.0d+0) 
term(381) = term(381) * (-8.0d+0) 
term(382) = term(382) * (-8.0d+0) 
term(383) = term(383) * (16.0d+0) 
term(384) = term(384) * (-2.0d+0) 
term(385) = term(385) * (4.0d+0) 
term(386) = term(386) * (4.0d+0) 
term(387) = term(387) * (-8.0d+0) 
term(388) = term(388) * (-2.0d+0) 
term(389) = term(389) * (4.0d+0) 
term(390) = term(390) * (4.0d+0) 
term(391) = term(391) * (-8.0d+0) 
term(392) = term(392) * (-8.0d+0) 
term(393) = term(393) * (16.0d+0) 
term(394) = term(394) * (4.0d+0) 
term(395) = term(395) * (-8.0d+0) 
term(396) = term(396) * (-4.0d+0) 
term(397) = term(397) * (8.0d+0) 
term(398) = term(398) * (-4.0d+0) 
term(399) = term(399) * (8.0d+0) 
term(400) = term(400) * (8.0d+0) 
term(401) = term(401) * (-16.0d+0) 
term(402) = term(402) * (8.0d+0) 
term(403) = term(403) * (-16.0d+0) 
term(404) = term(404) * (-4.0d+0) 
term(405) = term(405) * (8.0d+0) 
term(406) = term(406) * (8.0d+0) 
term(407) = term(407) * (-16.0d+0) 
term(408) = term(408) * (-4.0d+0) 
term(409) = term(409) * (8.0d+0) 
term(410) = term(410) * (8.0d+0) 
term(411) = term(411) * (-16.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(412) = term(412) + wm_interm_32_pt4(a,i) * wm_interm_33_pt4(a,p,i,q)
term(413) = term(413) + wm_interm_33_pt4(a,p,i,q) * wm_interm_35_pt4(a,i)
term(414) = term(414) + wm_interm_30_pt4(p,a,i,q) * wm_interm_31_pt4(a,i)
term(415) = term(415) + wm_interm_30_pt4(p,a,i,q) * wm_interm_34_pt4(a,i)
term(416) = term(416) + wm_interm_33_pt4(a,p,q,i) * wm_interm_35_pt4(a,i)
term(417) = term(417) + wm_interm_32_pt4(a,i) * wm_interm_33_pt4(a,p,q,i)
term(418) = term(418) + wm_interm_32_pt4(a,i) * wm_interm_36_pt4(a,p,i,q)
term(419) = term(419) + wm_interm_35_pt4(a,i) * wm_interm_36_pt4(a,p,i,q)
term(420) = term(420) + wm_interm_36_pt4(p,a,q,i) * wm_interm_41_pt4(a,i)
term(421) = term(421) + wm_interm_36_pt4(p,a,q,i) * wm_interm_38_pt4(a,i)
term(422) = term(422) + wm_interm_44_pt4(a,i) * wm_interm_46_pt4(p,a,i,q)
term(423) = term(423) + wm_interm_45_pt4(a,i) * wm_interm_46_pt4(p,a,i,q)
term(424) = term(424) + wm_interm_41_pt4(a,i) * wm_interm_47_pt4(a,p,i,q)
term(425) = term(425) + wm_interm_38_pt4(a,i) * wm_interm_47_pt4(a,p,i,q)
term(426) = term(426) + wm_interm_38_pt4(a,i) * wm_interm_47_pt4(a,p,q,i)
term(427) = term(427) + wm_interm_41_pt4(a,i) * wm_interm_47_pt4(a,p,q,i)
term(428) = term(428) + r1(vrdav_Rl, a,q) * wm_interm_2_pt4(p,i) * wm_interm_8_pt4(a,i)
term(429) = term(429) + r1(vrdav_Rl, a,q) * wm_interm_4_pt4(p,i) * wm_interm_8_pt4(a,i)
term(430) = term(430) + s1(a,q) * wm_interm_15_pt4(a,i) * wm_interm_2_pt4(p,i)
term(431) = term(431) + s1(a,q) * wm_interm_15_pt4(a,i) * wm_interm_4_pt4(p,i)
term(432) = term(432) + r1(vrdav_Rl, a,q) * wm_interm_2_pt4(p,i) * wm_interm_7_pt4(a,i)
term(433) = term(433) + r1(vrdav_Rl, a,q) * wm_interm_4_pt4(p,i) * wm_interm_7_pt4(a,i)
term(434) = term(434) + s1(p,i) * wm_interm_15_pt4(a,i) * wm_interm_4_pt4(a,q)
term(435) = term(435) + s1(p,i) * wm_interm_15_pt4(a,i) * wm_interm_2_pt4(a,q)
term(436) = term(436) + s1(a,q) * wm_interm_17_pt4(a,i) * wm_interm_2_pt4(p,i)
term(437) = term(437) + s1(a,q) * wm_interm_17_pt4(a,i) * wm_interm_4_pt4(p,i)
term(438) = term(438) + s1(p,i) * wm_interm_17_pt4(a,i) * wm_interm_4_pt4(a,q)
term(439) = term(439) + s1(p,i) * wm_interm_17_pt4(a,i) * wm_interm_2_pt4(a,q)
term(440) = term(440) + s1(p,q) * wm_interm_15_pt4(a,i) * wm_interm_4_pt4(a,i)
term(441) = term(441) + s1(p,q) * wm_interm_15_pt4(a,i) * wm_interm_2_pt4(a,i)
term(442) = term(442) + s1(p,q) * wm_interm_17_pt4(a,i) * wm_interm_4_pt4(a,i)
term(443) = term(443) + s1(p,q) * wm_interm_17_pt4(a,i) * wm_interm_2_pt4(a,i)
term(444) = term(444) + t1(p,q) * wm_interm_15_pt4(a,i) * wm_interm_4_pt4(a,i)
term(445) = term(445) + t1(p,q) * wm_interm_15_pt4(a,i) * wm_interm_2_pt4(a,i)
term(446) = term(446) + t1(p,q) * wm_interm_17_pt4(a,i) * wm_interm_4_pt4(a,i)
term(447) = term(447) + t1(p,q) * wm_interm_17_pt4(a,i) * wm_interm_2_pt4(a,i)
term(448) = term(448) + r1(vrdav_Rr, a,i) * wm_interm_20_pt4(a,p) * wm_interm_22_pt4(i,q)
term(449) = term(449) + r1(vrdav_Rr, a,i) * wm_interm_21_pt4(a,p) * wm_interm_22_pt4(i,q)
term(450) = term(450) + t1(a,q) * wm_interm_17_pt4(p,i) * wm_interm_2_pt4(a,i)
term(451) = term(451) + t1(a,q) * wm_interm_17_pt4(p,i) * wm_interm_4_pt4(a,i)
term(452) = term(452) + t1(a,q) * wm_interm_15_pt4(p,i) * wm_interm_4_pt4(a,i)
term(453) = term(453) + t1(a,q) * wm_interm_15_pt4(p,i) * wm_interm_2_pt4(a,i)
term(454) = term(454) + t1(p,i) * wm_interm_17_pt4(a,q) * wm_interm_4_pt4(a,i)
term(455) = term(455) + t1(p,i) * wm_interm_17_pt4(a,q) * wm_interm_2_pt4(a,i)
term(456) = term(456) + t1(p,i) * wm_interm_15_pt4(a,q) * wm_interm_4_pt4(a,i)
term(457) = term(457) + t1(p,i) * wm_interm_15_pt4(a,q) * wm_interm_2_pt4(a,i)
term(458) = term(458) + s1(a,i) * wm_interm_54_pt4(q,i) * wm_interm_68_pt4(a,p)
term(459) = term(459) + s1(a,i) * wm_interm_54_pt4(q,i) * wm_interm_69_pt4(a,p)
term(460) = term(460) + s1(a,i) * wm_interm_56_pt4(q,i) * wm_interm_68_pt4(a,p)
term(461) = term(461) + s1(a,i) * wm_interm_56_pt4(q,i) * wm_interm_69_pt4(a,p)
term(462) = term(462) + t1(a,i) * wm_interm_18_pt4(i,q) * wm_interm_74_pt4(a,p)
term(463) = term(463) + t1(a,i) * wm_interm_19_pt4(i,q) * wm_interm_74_pt4(a,p)
term(464) = term(464) + t1(a,i) * wm_interm_18_pt4(i,q) * wm_interm_75_pt4(a,p)
term(465) = term(465) + t1(a,i) * wm_interm_19_pt4(i,q) * wm_interm_75_pt4(a,p)
end do 
end do 

term(412) = term(412) * (-8.0d+0) 
term(413) = term(413) * (4.0d+0) 
term(414) = term(414) * (4.0d+0) 
term(415) = term(415) * (-2.0d+0) 
term(416) = term(416) * (-4.0d+0) 
term(417) = term(417) * (8.0d+0) 
term(418) = term(418) * (-8.0d+0) 
term(419) = term(419) * (4.0d+0) 
term(420) = term(420) * (8.0d+0) 
term(421) = term(421) * (-4.0d+0) 
term(422) = term(422) * (-4.0d+0) 
term(423) = term(423) * (2.0d+0) 
term(424) = term(424) * (8.0d+0) 
term(425) = term(425) * (-4.0d+0) 
term(426) = term(426) * (4.0d+0) 
term(427) = term(427) * (-8.0d+0) 
term(428) = term(428) * (8.0d+0) 
term(429) = term(429) * (-4.0d+0) 
term(430) = term(430) * (-16.0d+0) 
term(431) = term(431) * (8.0d+0) 
term(432) = term(432) * (-16.0d+0) 
term(433) = term(433) * (8.0d+0) 
term(434) = term(434) * (8.0d+0) 
term(435) = term(435) * (-16.0d+0) 
term(436) = term(436) * (8.0d+0) 
term(437) = term(437) * (-4.0d+0) 
term(438) = term(438) * (-4.0d+0) 
term(439) = term(439) * (8.0d+0) 
term(440) = term(440) * (8.0d+0) 
term(441) = term(441) * (-16.0d+0) 
term(442) = term(442) * (-4.0d+0) 
term(443) = term(443) * (8.0d+0) 
term(444) = term(444) * (8.0d+0) 
term(445) = term(445) * (-16.0d+0) 
term(446) = term(446) * (-4.0d+0) 
term(447) = term(447) * (8.0d+0) 
term(448) = term(448) * (4.0d+0) 
term(449) = term(449) * (-8.0d+0) 
term(450) = term(450) * (-8.0d+0) 
term(451) = term(451) * (4.0d+0) 
term(452) = term(452) * (-8.0d+0) 
term(453) = term(453) * (16.0d+0) 
term(454) = term(454) * (4.0d+0) 
term(455) = term(455) * (-8.0d+0) 
term(456) = term(456) * (-8.0d+0) 
term(457) = term(457) * (16.0d+0) 
term(458) = term(458) * (-2.0d+0) 
term(459) = term(459) * (4.0d+0) 
term(460) = term(460) * (4.0d+0) 
term(461) = term(461) * (-8.0d+0) 
term(462) = term(462) * (-2.0d+0) 
term(463) = term(463) * (4.0d+0) 
term(464) = term(464) * (4.0d+0) 
term(465) = term(465) * (-8.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(466) = term(466) + r2(vrdav_Rl, a,i,p,q) * wm_interm_72_pt4(i,j,k,l) * wm_interm_73_pt4(a,j,l,k)
term(467) = term(467) + r2(vrdav_Rl, a,i,p,q) * wm_interm_72_pt4(i,j,k,l) * wm_interm_73_pt4(a,j,k,l)
term(468) = term(468) + r2(vrdav_Rr, a,i,p,q) * wm_interm_23_pt4(a,j,k,l) * wm_interm_55_pt4(i,l,j,k)
term(469) = term(469) + r2(vrdav_Rr, a,i,p,q) * wm_interm_23_pt4(a,j,k,l) * wm_interm_55_pt4(l,i,j,k)
term(470) = term(470) + t2(a,p,i,q) * wm_interm_23_pt4(a,j,k,l) * wm_interm_63_pt4(i,l,j,k)
end do 
end do 
end do 
end do 
end do 

term(466) = term(466) * (-8.0d+0) 
term(467) = term(467) * (16.0d+0) 
term(468) = term(468) * (-8.0d+0) 
term(469) = term(469) * (4.0d+0) 
term(470) = term(470) * (-4.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(471) = term(471) + s2(a,p,j,i) * wm_interm_58_pt4(b,a,q,k) * wm_interm_73_pt4(b,k,j,i)
term(472) = term(472) + s2(a,p,j,i) * wm_interm_57_pt4(b,a,q,k) * wm_interm_73_pt4(b,k,j,i)
term(473) = term(473) + s2(a,p,j,i) * wm_interm_64_pt4(b,a,q,k) * wm_interm_73_pt4(b,k,j,i)
term(474) = term(474) + s2(a,p,j,i) * wm_interm_65_pt4(b,a,q,k) * wm_interm_73_pt4(b,k,j,i)
term(475) = term(475) + s2(a,p,q,i) * wm_interm_64_pt4(b,a,j,k) * wm_interm_73_pt4(b,k,j,i)
term(476) = term(476) + s2(a,p,q,i) * wm_interm_65_pt4(b,a,j,k) * wm_interm_73_pt4(b,k,j,i)
term(477) = term(477) + s2(a,p,i,q) * wm_interm_64_pt4(b,a,j,k) * wm_interm_73_pt4(b,k,j,i)
term(478) = term(478) + s2(a,p,i,q) * wm_interm_65_pt4(b,a,j,k) * wm_interm_73_pt4(b,k,j,i)
term(479) = term(479) + r2(vrdav_Rl, a,q,p,i) * wm_interm_1_pt4(b,a,j,k) * wm_interm_28_pt4(b,i,j,k)
term(480) = term(480) + r2(vrdav_Rl, a,q,p,i) * wm_interm_1_pt4(b,a,j,k) * wm_interm_29_pt4(b,i,j,k)
term(481) = term(481) + r2(vrdav_Rl, a,q,p,i) * wm_interm_29_pt4(b,i,j,k) * wm_interm_5_pt4(b,a,j,k)
term(482) = term(482) + r2(vrdav_Rl, a,q,p,i) * wm_interm_28_pt4(b,i,j,k) * wm_interm_5_pt4(b,a,j,k)
term(483) = term(483) + s2(a,p,j,i) * wm_interm_28_pt4(b,i,j,k) * wm_interm_48_pt4(b,a,q,k)
term(484) = term(484) + s2(a,p,q,i) * wm_interm_28_pt4(b,i,j,k) * wm_interm_48_pt4(b,a,j,k)
term(485) = term(485) + s2(a,p,q,i) * wm_interm_28_pt4(b,j,i,k) * wm_interm_48_pt4(b,a,j,k)
term(486) = term(486) + r2(vrdav_Rl, a,q,p,i) * wm_interm_11_pt4(b,a,j,k) * wm_interm_28_pt4(b,i,j,k)
term(487) = term(487) + r2(vrdav_Rl, a,q,p,i) * wm_interm_11_pt4(b,a,j,k) * wm_interm_29_pt4(b,i,j,k)
term(488) = term(488) + r2(vrdav_Rl, a,q,p,i) * wm_interm_12_pt4(b,a,j,k) * wm_interm_28_pt4(b,i,j,k)
term(489) = term(489) + r2(vrdav_Rl, a,q,p,i) * wm_interm_12_pt4(b,a,j,k) * wm_interm_29_pt4(b,i,j,k)
term(490) = term(490) + s2(a,p,q,i) * wm_interm_57_pt4(b,a,j,k) * wm_interm_73_pt4(b,k,j,i)
term(491) = term(491) + s2(a,p,q,i) * wm_interm_58_pt4(b,a,j,k) * wm_interm_73_pt4(b,k,j,i)
term(492) = term(492) + s2(a,p,i,q) * wm_interm_57_pt4(b,a,j,k) * wm_interm_73_pt4(b,k,j,i)
term(493) = term(493) + s2(a,p,i,q) * wm_interm_58_pt4(b,a,j,k) * wm_interm_73_pt4(b,k,j,i)
term(494) = term(494) + s2(a,p,j,i) * wm_interm_28_pt4(b,i,j,k) * wm_interm_49_pt4(b,a,q,k)
term(495) = term(495) + s2(a,p,q,i) * wm_interm_28_pt4(b,i,j,k) * wm_interm_49_pt4(b,a,j,k)
term(496) = term(496) + s2(a,p,q,i) * wm_interm_28_pt4(b,j,i,k) * wm_interm_49_pt4(b,a,j,k)
term(497) = term(497) + s2(a,p,j,i) * wm_interm_28_pt4(b,i,j,k) * wm_interm_50_pt4(b,a,q,k)
term(498) = term(498) + s2(a,p,j,i) * wm_interm_28_pt4(b,i,j,k) * wm_interm_53_pt4(b,a,q,k)
term(499) = term(499) + s2(a,p,q,i) * wm_interm_28_pt4(b,j,i,k) * wm_interm_50_pt4(b,a,j,k)
term(500) = term(500) + s2(a,p,q,i) * wm_interm_28_pt4(b,i,j,k) * wm_interm_50_pt4(b,a,j,k)
term(501) = term(501) + s2(a,p,q,i) * wm_interm_28_pt4(b,i,j,k) * wm_interm_53_pt4(b,a,j,k)
term(502) = term(502) + s2(a,p,q,i) * wm_interm_28_pt4(b,j,i,k) * wm_interm_53_pt4(b,a,j,k)
term(503) = term(503) + t2(a,p,q,i) * wm_interm_42_pt4(b,i,j,k) * wm_interm_59_pt4(b,a,j,k)
term(504) = term(504) + t2(a,p,q,i) * wm_interm_42_pt4(b,i,j,k) * wm_interm_60_pt4(b,a,j,k)
term(505) = term(505) + t2(a,p,q,i) * wm_interm_42_pt4(b,i,j,k) * wm_interm_61_pt4(b,a,j,k)
term(506) = term(506) + t2(a,p,q,i) * wm_interm_42_pt4(b,i,j,k) * wm_interm_62_pt4(b,a,j,k)
term(507) = term(507) + t2(a,p,q,i) * wm_interm_42_pt4(b,j,i,k) * wm_interm_59_pt4(b,a,j,k)
term(508) = term(508) + t2(a,p,q,i) * wm_interm_42_pt4(b,j,i,k) * wm_interm_60_pt4(b,a,j,k)
term(509) = term(509) + t2(a,p,q,i) * wm_interm_42_pt4(b,j,i,k) * wm_interm_62_pt4(b,a,j,k)
term(510) = term(510) + t2(a,p,q,i) * wm_interm_42_pt4(b,j,i,k) * wm_interm_61_pt4(b,a,j,k)
term(511) = term(511) + t2(a,p,q,i) * wm_interm_43_pt4(b,j,i,k) * wm_interm_59_pt4(b,a,j,k)
term(512) = term(512) + t2(a,p,q,i) * wm_interm_43_pt4(b,j,i,k) * wm_interm_60_pt4(b,a,j,k)
term(513) = term(513) + t2(a,p,q,i) * wm_interm_43_pt4(b,i,j,k) * wm_interm_59_pt4(b,a,j,k)
term(514) = term(514) + t2(a,p,q,i) * wm_interm_43_pt4(b,i,j,k) * wm_interm_60_pt4(b,a,j,k)
term(515) = term(515) + t2(a,p,q,i) * wm_interm_43_pt4(b,i,j,k) * wm_interm_62_pt4(b,a,j,k)
term(516) = term(516) + t2(a,p,q,i) * wm_interm_43_pt4(b,i,j,k) * wm_interm_61_pt4(b,a,j,k)
term(517) = term(517) + t2(a,p,q,i) * wm_interm_43_pt4(b,j,i,k) * wm_interm_61_pt4(b,a,j,k)
term(518) = term(518) + t2(a,p,q,i) * wm_interm_43_pt4(b,j,i,k) * wm_interm_62_pt4(b,a,j,k)
end do 
end do 
end do 
end do 
end do 

term(471) = term(471) * (4.0d+0) 
term(472) = term(472) * (-8.0d+0) 
term(473) = term(473) * (4.0d+0) 
term(474) = term(474) * (-8.0d+0) 
term(475) = term(475) * (-8.0d+0) 
term(476) = term(476) * (16.0d+0) 
term(477) = term(477) * (16.0d+0) 
term(478) = term(478) * (-32.0d+0) 
term(479) = term(479) * (4.0d+0) 
term(480) = term(480) * (-8.0d+0) 
term(481) = term(481) * (4.0d+0) 
term(482) = term(482) * (-8.0d+0) 
term(483) = term(483) * (16.0d+0) 
term(484) = term(484) * (-8.0d+0) 
term(485) = term(485) * (16.0d+0) 
term(486) = term(486) * (4.0d+0) 
term(487) = term(487) * (-8.0d+0) 
term(488) = term(488) * (-8.0d+0) 
term(489) = term(489) * (16.0d+0) 
term(490) = term(490) * (4.0d+0) 
term(491) = term(491) * (-8.0d+0) 
term(492) = term(492) * (-8.0d+0) 
term(493) = term(493) * (16.0d+0) 
term(494) = term(494) * (-8.0d+0) 
term(495) = term(495) * (4.0d+0) 
term(496) = term(496) * (-8.0d+0) 
term(497) = term(497) * (4.0d+0) 
term(498) = term(498) * (-8.0d+0) 
term(499) = term(499) * (4.0d+0) 
term(500) = term(500) * (-8.0d+0) 
term(501) = term(501) * (4.0d+0) 
term(502) = term(502) * (-8.0d+0) 
term(503) = term(503) * (-1.0d+0) 
term(504) = term(504) * (2.0d+0) 
term(505) = term(505) * (-1.0d+0) 
term(506) = term(506) * (2.0d+0) 
term(507) = term(507) * (2.0d+0) 
term(508) = term(508) * (-4.0d+0) 
term(509) = term(509) * (-1.0d+0) 
term(510) = term(510) * (2.0d+0) 
term(511) = term(511) * (-1.0d+0) 
term(512) = term(512) * (2.0d+0) 
term(513) = term(513) * (2.0d+0) 
term(514) = term(514) * (-4.0d+0) 
term(515) = term(515) * (-1.0d+0) 
term(516) = term(516) * (2.0d+0) 
term(517) = term(517) * (-1.0d+0) 
term(518) = term(518) * (2.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(519) = term(519) + s2(a,p,j,i) * wm_interm_57_pt4(b,a,q,k) * wm_interm_73_pt4(b,k,i,j)
term(520) = term(520) + s2(a,p,j,i) * wm_interm_58_pt4(b,a,q,k) * wm_interm_73_pt4(b,k,i,j)
term(521) = term(521) + s2(a,p,j,i) * wm_interm_64_pt4(b,a,q,k) * wm_interm_73_pt4(b,k,i,j)
term(522) = term(522) + s2(a,p,j,i) * wm_interm_65_pt4(b,a,q,k) * wm_interm_73_pt4(b,k,i,j)
term(523) = term(523) + s2(a,p,i,q) * wm_interm_28_pt4(b,j,i,k) * wm_interm_48_pt4(b,a,j,k)
term(524) = term(524) + s2(a,p,i,q) * wm_interm_28_pt4(b,j,i,k) * wm_interm_49_pt4(b,a,j,k)
term(525) = term(525) + s2(a,p,q,i) * wm_interm_58_pt4(b,a,j,k) * wm_interm_73_pt4(b,k,i,j)
term(526) = term(526) + s2(a,p,q,i) * wm_interm_57_pt4(b,a,j,k) * wm_interm_73_pt4(b,k,i,j)
term(527) = term(527) + s2(a,p,q,i) * wm_interm_64_pt4(b,a,j,k) * wm_interm_73_pt4(b,k,i,j)
term(528) = term(528) + s2(a,p,q,i) * wm_interm_65_pt4(b,a,j,k) * wm_interm_73_pt4(b,k,i,j)
term(529) = term(529) + s2(a,p,i,q) * wm_interm_58_pt4(b,a,j,k) * wm_interm_73_pt4(b,k,i,j)
term(530) = term(530) + s2(a,p,i,q) * wm_interm_57_pt4(b,a,j,k) * wm_interm_73_pt4(b,k,i,j)
term(531) = term(531) + s2(a,p,i,q) * wm_interm_64_pt4(b,a,j,k) * wm_interm_73_pt4(b,k,i,j)
term(532) = term(532) + s2(a,p,i,q) * wm_interm_65_pt4(b,a,j,k) * wm_interm_73_pt4(b,k,i,j)
term(533) = term(533) + s2(a,p,i,q) * wm_interm_28_pt4(b,j,i,k) * wm_interm_50_pt4(b,a,j,k)
term(534) = term(534) + s2(a,p,i,q) * wm_interm_28_pt4(b,j,i,k) * wm_interm_53_pt4(b,a,j,k)
term(535) = term(535) + t2(a,p,i,q) * wm_interm_42_pt4(b,j,i,k) * wm_interm_59_pt4(b,a,j,k)
term(536) = term(536) + t2(a,p,i,q) * wm_interm_42_pt4(b,j,i,k) * wm_interm_60_pt4(b,a,j,k)
term(537) = term(537) + t2(a,p,i,q) * wm_interm_42_pt4(b,j,i,k) * wm_interm_62_pt4(b,a,j,k)
term(538) = term(538) + t2(a,p,i,q) * wm_interm_42_pt4(b,j,i,k) * wm_interm_61_pt4(b,a,j,k)
term(539) = term(539) + t2(a,p,i,q) * wm_interm_43_pt4(b,j,i,k) * wm_interm_59_pt4(b,a,j,k)
term(540) = term(540) + t2(a,p,i,q) * wm_interm_43_pt4(b,j,i,k) * wm_interm_60_pt4(b,a,j,k)
term(541) = term(541) + t2(a,p,i,q) * wm_interm_43_pt4(b,j,i,k) * wm_interm_61_pt4(b,a,j,k)
term(542) = term(542) + t2(a,p,i,q) * wm_interm_43_pt4(b,j,i,k) * wm_interm_62_pt4(b,a,j,k)
end do 
end do 
end do 
end do 
end do 

term(519) = term(519) * (4.0d+0) 
term(520) = term(520) * (-8.0d+0) 
term(521) = term(521) * (-8.0d+0) 
term(522) = term(522) * (16.0d+0) 
term(523) = term(523) * (-32.0d+0) 
term(524) = term(524) * (16.0d+0) 
term(525) = term(525) * (4.0d+0) 
term(526) = term(526) * (-8.0d+0) 
term(527) = term(527) * (4.0d+0) 
term(528) = term(528) * (-8.0d+0) 
term(529) = term(529) * (-8.0d+0) 
term(530) = term(530) * (16.0d+0) 
term(531) = term(531) * (-8.0d+0) 
term(532) = term(532) * (16.0d+0) 
term(533) = term(533) * (-8.0d+0) 
term(534) = term(534) * (16.0d+0) 
term(535) = term(535) * (-4.0d+0) 
term(536) = term(536) * (8.0d+0) 
term(537) = term(537) * (2.0d+0) 
term(538) = term(538) * (-4.0d+0) 
term(539) = term(539) * (2.0d+0) 
term(540) = term(540) * (-4.0d+0) 
term(541) = term(541) * (2.0d+0) 
term(542) = term(542) * (-4.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(543) = term(543) + s1(p,i) * wm_interm_16_pt4(a,b) * wm_interm_1_pt4(b,a,q,i)
term(544) = term(544) + s1(p,i) * wm_interm_16_pt4(a,b) * wm_interm_5_pt4(b,a,q,i)
term(545) = term(545) + s1(p,i) * wm_interm_11_pt4(a,b,q,i) * wm_interm_16_pt4(b,a)
term(546) = term(546) + s1(p,i) * wm_interm_12_pt4(a,b,q,i) * wm_interm_16_pt4(b,a)
term(547) = term(547) + r1(vrdav_Rr, p,i) * wm_interm_1_pt4(a,b,i,q) * wm_interm_24_pt4(b,a)
term(548) = term(548) + r1(vrdav_Rr, p,i) * wm_interm_24_pt4(a,b) * wm_interm_5_pt4(b,a,i,q)
term(549) = term(549) + r1(vrdav_Rr, p,i) * wm_interm_11_pt4(a,b,i,q) * wm_interm_24_pt4(b,a)
term(550) = term(550) + r1(vrdav_Rr, p,i) * wm_interm_12_pt4(a,b,i,q) * wm_interm_24_pt4(b,a)
term(551) = term(551) + t1(p,i) * wm_interm_11_pt4(a,b,i,q) * wm_interm_16_pt4(b,a)
term(552) = term(552) + t1(p,i) * wm_interm_12_pt4(a,b,i,q) * wm_interm_16_pt4(b,a)
term(553) = term(553) + t1(p,i) * wm_interm_16_pt4(a,b) * wm_interm_1_pt4(b,a,i,q)
term(554) = term(554) + t1(p,i) * wm_interm_16_pt4(a,b) * wm_interm_5_pt4(b,a,i,q)
term(555) = term(555) + r2(vrdav_Rl, a,q,b,i) * wm_interm_20_pt4(p,a) * wm_interm_31_pt4(b,i)
term(556) = term(556) + r2(vrdav_Rl, a,q,b,i) * wm_interm_21_pt4(p,a) * wm_interm_31_pt4(b,i)
term(557) = term(557) + r2(vrdav_Rl, a,q,b,i) * wm_interm_20_pt4(p,a) * wm_interm_34_pt4(b,i)
term(558) = term(558) + r2(vrdav_Rl, a,q,b,i) * wm_interm_21_pt4(p,a) * wm_interm_34_pt4(b,i)
term(559) = term(559) + r2(vrdav_Rl, a,q,p,i) * wm_interm_68_pt4(a,b) * wm_interm_7_pt4(b,i)
term(560) = term(560) + r2(vrdav_Rl, a,q,p,i) * wm_interm_69_pt4(a,b) * wm_interm_7_pt4(b,i)
term(561) = term(561) + r2(vrdav_Rl, a,q,p,i) * wm_interm_68_pt4(a,b) * wm_interm_8_pt4(b,i)
term(562) = term(562) + r2(vrdav_Rl, a,q,p,i) * wm_interm_69_pt4(a,b) * wm_interm_8_pt4(b,i)
term(563) = term(563) + s1(a,i) * wm_interm_48_pt4(p,b,q,i) * wm_interm_68_pt4(a,b)
term(564) = term(564) + s1(a,i) * wm_interm_48_pt4(p,b,q,i) * wm_interm_69_pt4(a,b)
term(565) = term(565) + s1(p,i) * wm_interm_48_pt4(a,b,q,i) * wm_interm_68_pt4(a,b)
term(566) = term(566) + s1(p,i) * wm_interm_48_pt4(a,b,q,i) * wm_interm_69_pt4(a,b)
term(567) = term(567) + s1(p,i) * wm_interm_1_pt4(a,b,q,i) * wm_interm_74_pt4(b,a)
term(568) = term(568) + s1(p,i) * wm_interm_1_pt4(a,b,q,i) * wm_interm_75_pt4(b,a)
term(569) = term(569) + s1(a,i) * wm_interm_1_pt4(p,b,q,i) * wm_interm_74_pt4(b,a)
term(570) = term(570) + s1(a,i) * wm_interm_1_pt4(p,b,q,i) * wm_interm_75_pt4(b,a)
term(571) = term(571) + s1(p,i) * wm_interm_5_pt4(a,b,q,i) * wm_interm_74_pt4(b,a)
term(572) = term(572) + s1(p,i) * wm_interm_5_pt4(a,b,q,i) * wm_interm_75_pt4(b,a)
term(573) = term(573) + s1(a,i) * wm_interm_5_pt4(p,b,q,i) * wm_interm_74_pt4(b,a)
term(574) = term(574) + s1(a,i) * wm_interm_5_pt4(p,b,q,i) * wm_interm_75_pt4(b,a)
term(575) = term(575) + s1(p,i) * wm_interm_11_pt4(a,b,q,i) * wm_interm_74_pt4(b,a)
term(576) = term(576) + s1(p,i) * wm_interm_11_pt4(a,b,q,i) * wm_interm_75_pt4(b,a)
term(577) = term(577) + s1(p,i) * wm_interm_12_pt4(a,b,q,i) * wm_interm_74_pt4(b,a)
term(578) = term(578) + s1(p,i) * wm_interm_12_pt4(a,b,q,i) * wm_interm_75_pt4(b,a)
term(579) = term(579) + s1(a,i) * wm_interm_11_pt4(p,b,q,i) * wm_interm_74_pt4(b,a)
term(580) = term(580) + s1(a,i) * wm_interm_12_pt4(p,b,q,i) * wm_interm_74_pt4(b,a)
term(581) = term(581) + s1(a,i) * wm_interm_11_pt4(p,b,q,i) * wm_interm_75_pt4(b,a)
term(582) = term(582) + s1(a,i) * wm_interm_12_pt4(p,b,q,i) * wm_interm_75_pt4(b,a)
term(583) = term(583) + s1(a,i) * wm_interm_49_pt4(p,b,q,i) * wm_interm_68_pt4(a,b)
term(584) = term(584) + s1(a,i) * wm_interm_49_pt4(p,b,q,i) * wm_interm_69_pt4(a,b)
term(585) = term(585) + s1(p,i) * wm_interm_49_pt4(a,b,q,i) * wm_interm_68_pt4(a,b)
term(586) = term(586) + s1(p,i) * wm_interm_49_pt4(a,b,q,i) * wm_interm_69_pt4(a,b)
term(587) = term(587) + s1(p,i) * wm_interm_50_pt4(a,b,q,i) * wm_interm_68_pt4(a,b)
term(588) = term(588) + s1(p,i) * wm_interm_50_pt4(a,b,q,i) * wm_interm_69_pt4(a,b)
term(589) = term(589) + s1(p,i) * wm_interm_53_pt4(a,b,q,i) * wm_interm_68_pt4(a,b)
term(590) = term(590) + s1(p,i) * wm_interm_53_pt4(a,b,q,i) * wm_interm_69_pt4(a,b)
term(591) = term(591) + s1(a,i) * wm_interm_53_pt4(p,b,q,i) * wm_interm_68_pt4(a,b)
term(592) = term(592) + s1(a,i) * wm_interm_53_pt4(p,b,q,i) * wm_interm_69_pt4(a,b)
term(593) = term(593) + s1(a,i) * wm_interm_50_pt4(p,b,q,i) * wm_interm_68_pt4(a,b)
term(594) = term(594) + s1(a,i) * wm_interm_50_pt4(p,b,q,i) * wm_interm_69_pt4(a,b)
term(595) = term(595) + t1(a,i) * wm_interm_20_pt4(a,b) * wm_interm_64_pt4(b,p,i,q)
term(596) = term(596) + t1(a,i) * wm_interm_21_pt4(a,b) * wm_interm_64_pt4(b,p,i,q)
term(597) = term(597) + t1(a,i) * wm_interm_20_pt4(a,b) * wm_interm_65_pt4(b,p,i,q)
term(598) = term(598) + t1(a,i) * wm_interm_21_pt4(a,b) * wm_interm_65_pt4(b,p,i,q)
term(599) = term(599) + t1(a,i) * wm_interm_20_pt4(a,b) * wm_interm_57_pt4(b,p,i,q)
term(600) = term(600) + t1(a,i) * wm_interm_20_pt4(a,b) * wm_interm_58_pt4(b,p,i,q)
term(601) = term(601) + t1(a,i) * wm_interm_21_pt4(a,b) * wm_interm_57_pt4(b,p,i,q)
term(602) = term(602) + t1(a,i) * wm_interm_21_pt4(a,b) * wm_interm_58_pt4(b,p,i,q)
term(603) = term(603) + r2(vrdav_Rr, a,q,p,i) * wm_interm_20_pt4(a,b) * wm_interm_44_pt4(b,i)
term(604) = term(604) + r2(vrdav_Rr, a,q,p,i) * wm_interm_21_pt4(a,b) * wm_interm_44_pt4(b,i)
term(605) = term(605) + t1(a,i) * wm_interm_51_pt4(a,b) * wm_interm_62_pt4(p,b,q,i)
term(606) = term(606) + t1(a,i) * wm_interm_51_pt4(a,b) * wm_interm_61_pt4(p,b,q,i)
term(607) = term(607) + t1(a,i) * wm_interm_51_pt4(a,b) * wm_interm_59_pt4(p,b,q,i)
term(608) = term(608) + t1(a,i) * wm_interm_51_pt4(a,b) * wm_interm_60_pt4(p,b,q,i)
term(609) = term(609) + t1(a,i) * wm_interm_52_pt4(a,b) * wm_interm_59_pt4(p,b,q,i)
term(610) = term(610) + t1(a,i) * wm_interm_52_pt4(a,b) * wm_interm_60_pt4(p,b,q,i)
term(611) = term(611) + t1(a,i) * wm_interm_52_pt4(a,b) * wm_interm_62_pt4(p,b,q,i)
term(612) = term(612) + t1(a,i) * wm_interm_52_pt4(a,b) * wm_interm_61_pt4(p,b,q,i)
term(613) = term(613) + r2(vrdav_Rr, a,q,p,i) * wm_interm_20_pt4(a,b) * wm_interm_45_pt4(b,i)
term(614) = term(614) + r2(vrdav_Rr, a,q,p,i) * wm_interm_21_pt4(a,b) * wm_interm_45_pt4(b,i)
term(615) = term(615) + s1(p,i) * wm_interm_20_pt4(a,b) * wm_interm_58_pt4(b,a,q,i)
term(616) = term(616) + s1(p,i) * wm_interm_20_pt4(a,b) * wm_interm_57_pt4(b,a,q,i)
term(617) = term(617) + s1(p,i) * wm_interm_21_pt4(a,b) * wm_interm_58_pt4(b,a,q,i)
term(618) = term(618) + s1(p,i) * wm_interm_21_pt4(a,b) * wm_interm_57_pt4(b,a,q,i)
term(619) = term(619) + s1(p,i) * wm_interm_20_pt4(a,b) * wm_interm_64_pt4(b,a,q,i)
term(620) = term(620) + s1(p,i) * wm_interm_20_pt4(a,b) * wm_interm_65_pt4(b,a,q,i)
term(621) = term(621) + s1(p,i) * wm_interm_21_pt4(a,b) * wm_interm_64_pt4(b,a,q,i)
term(622) = term(622) + s1(p,i) * wm_interm_21_pt4(a,b) * wm_interm_65_pt4(b,a,q,i)
term(623) = term(623) + s1(p,i) * wm_interm_51_pt4(a,b) * wm_interm_59_pt4(a,b,i,q)
term(624) = term(624) + s1(p,i) * wm_interm_51_pt4(a,b) * wm_interm_60_pt4(a,b,i,q)
term(625) = term(625) + s1(p,i) * wm_interm_51_pt4(a,b) * wm_interm_61_pt4(a,b,i,q)
term(626) = term(626) + s1(p,i) * wm_interm_51_pt4(a,b) * wm_interm_62_pt4(a,b,i,q)
term(627) = term(627) + s1(p,i) * wm_interm_52_pt4(a,b) * wm_interm_59_pt4(a,b,i,q)
term(628) = term(628) + s1(p,i) * wm_interm_52_pt4(a,b) * wm_interm_60_pt4(a,b,i,q)
term(629) = term(629) + s1(p,i) * wm_interm_52_pt4(a,b) * wm_interm_61_pt4(a,b,i,q)
term(630) = term(630) + s1(p,i) * wm_interm_52_pt4(a,b) * wm_interm_62_pt4(a,b,i,q)
term(631) = term(631) + r2(vrdav_Rl, a,q,b,i) * wm_interm_68_pt4(a,p) * wm_interm_7_pt4(b,i)
term(632) = term(632) + r2(vrdav_Rl, a,q,b,i) * wm_interm_69_pt4(a,p) * wm_interm_7_pt4(b,i)
term(633) = term(633) + r2(vrdav_Rl, a,q,b,i) * wm_interm_68_pt4(a,p) * wm_interm_8_pt4(b,i)
term(634) = term(634) + r2(vrdav_Rl, a,q,b,i) * wm_interm_69_pt4(a,p) * wm_interm_8_pt4(b,i)
term(635) = term(635) + t1(a,i) * wm_interm_11_pt4(b,p,i,q) * wm_interm_74_pt4(a,b)
term(636) = term(636) + t1(a,i) * wm_interm_11_pt4(b,p,i,q) * wm_interm_75_pt4(a,b)
term(637) = term(637) + t1(a,i) * wm_interm_12_pt4(b,p,i,q) * wm_interm_74_pt4(a,b)
term(638) = term(638) + t1(a,i) * wm_interm_12_pt4(b,p,i,q) * wm_interm_75_pt4(a,b)
term(639) = term(639) + t1(a,i) * wm_interm_5_pt4(b,p,i,q) * wm_interm_74_pt4(a,b)
term(640) = term(640) + t1(a,i) * wm_interm_5_pt4(b,p,i,q) * wm_interm_75_pt4(a,b)
term(641) = term(641) + t1(a,i) * wm_interm_1_pt4(b,p,i,q) * wm_interm_74_pt4(a,b)
term(642) = term(642) + t1(a,i) * wm_interm_1_pt4(b,p,i,q) * wm_interm_75_pt4(a,b)
term(643) = term(643) + t2(a,b,q,i) * wm_interm_44_pt4(b,i) * wm_interm_68_pt4(p,a)
term(644) = term(644) + t2(a,b,q,i) * wm_interm_44_pt4(b,i) * wm_interm_69_pt4(p,a)
term(645) = term(645) + t2(a,b,q,i) * wm_interm_45_pt4(b,i) * wm_interm_68_pt4(p,a)
term(646) = term(646) + t2(a,b,q,i) * wm_interm_45_pt4(b,i) * wm_interm_69_pt4(p,a)
term(647) = term(647) + t1(p,i) * wm_interm_11_pt4(a,b,i,q) * wm_interm_74_pt4(b,a)
term(648) = term(648) + t1(p,i) * wm_interm_11_pt4(a,b,i,q) * wm_interm_75_pt4(b,a)
term(649) = term(649) + t1(p,i) * wm_interm_12_pt4(a,b,i,q) * wm_interm_74_pt4(b,a)
term(650) = term(650) + t1(p,i) * wm_interm_12_pt4(a,b,i,q) * wm_interm_75_pt4(b,a)
term(651) = term(651) + t1(p,i) * wm_interm_53_pt4(a,b,i,q) * wm_interm_68_pt4(a,b)
term(652) = term(652) + t1(p,i) * wm_interm_53_pt4(a,b,i,q) * wm_interm_69_pt4(a,b)
term(653) = term(653) + t1(p,i) * wm_interm_50_pt4(a,b,i,q) * wm_interm_68_pt4(a,b)
term(654) = term(654) + t1(p,i) * wm_interm_50_pt4(a,b,i,q) * wm_interm_69_pt4(a,b)
term(655) = term(655) + t1(p,i) * wm_interm_1_pt4(a,b,i,q) * wm_interm_74_pt4(b,a)
term(656) = term(656) + t1(p,i) * wm_interm_1_pt4(a,b,i,q) * wm_interm_75_pt4(b,a)
term(657) = term(657) + t1(p,i) * wm_interm_5_pt4(a,b,i,q) * wm_interm_74_pt4(b,a)
term(658) = term(658) + t1(p,i) * wm_interm_5_pt4(a,b,i,q) * wm_interm_75_pt4(b,a)
term(659) = term(659) + t1(p,i) * wm_interm_49_pt4(a,b,i,q) * wm_interm_68_pt4(a,b)
term(660) = term(660) + t1(p,i) * wm_interm_49_pt4(a,b,i,q) * wm_interm_69_pt4(a,b)
term(661) = term(661) + t1(p,i) * wm_interm_48_pt4(a,b,i,q) * wm_interm_68_pt4(a,b)
term(662) = term(662) + t1(p,i) * wm_interm_48_pt4(a,b,i,q) * wm_interm_69_pt4(a,b)
end do 
end do 
end do 

term(543) = term(543) * (-4.0d+0) 
term(544) = term(544) * (8.0d+0) 
term(545) = term(545) * (-4.0d+0) 
term(546) = term(546) * (8.0d+0) 
term(547) = term(547) * (2.0d+0) 
term(548) = term(548) * (-4.0d+0) 
term(549) = term(549) * (2.0d+0) 
term(550) = term(550) * (-4.0d+0) 
term(551) = term(551) * (4.0d+0) 
term(552) = term(552) * (-8.0d+0) 
term(553) = term(553) * (4.0d+0) 
term(554) = term(554) * (-8.0d+0) 
term(555) = term(555) * (8.0d+0) 
term(556) = term(556) * (-16.0d+0) 
term(557) = term(557) * (-4.0d+0) 
term(558) = term(558) * (8.0d+0) 
term(559) = term(559) * (-8.0d+0) 
term(560) = term(560) * (16.0d+0) 
term(561) = term(561) * (4.0d+0) 
term(562) = term(562) * (-8.0d+0) 
term(563) = term(563) * (16.0d+0) 
term(564) = term(564) * (-32.0d+0) 
term(565) = term(565) * (-8.0d+0) 
term(566) = term(566) * (16.0d+0) 
term(567) = term(567) * (4.0d+0) 
term(568) = term(568) * (-8.0d+0) 
term(569) = term(569) * (-8.0d+0) 
term(570) = term(570) * (16.0d+0) 
term(571) = term(571) * (-8.0d+0) 
term(572) = term(572) * (16.0d+0) 
term(573) = term(573) * (4.0d+0) 
term(574) = term(574) * (-8.0d+0) 
term(575) = term(575) * (4.0d+0) 
term(576) = term(576) * (-8.0d+0) 
term(577) = term(577) * (-8.0d+0) 
term(578) = term(578) * (16.0d+0) 
term(579) = term(579) * (-8.0d+0) 
term(580) = term(580) * (16.0d+0) 
term(581) = term(581) * (16.0d+0) 
term(582) = term(582) * (-32.0d+0) 
term(583) = term(583) * (-8.0d+0) 
term(584) = term(584) * (16.0d+0) 
term(585) = term(585) * (4.0d+0) 
term(586) = term(586) * (-8.0d+0) 
term(587) = term(587) * (-8.0d+0) 
term(588) = term(588) * (16.0d+0) 
term(589) = term(589) * (4.0d+0) 
term(590) = term(590) * (-8.0d+0) 
term(591) = term(591) * (-8.0d+0) 
term(592) = term(592) * (16.0d+0) 
term(593) = term(593) * (4.0d+0) 
term(594) = term(594) * (-8.0d+0) 
term(595) = term(595) * (8.0d+0) 
term(596) = term(596) * (-16.0d+0) 
term(597) = term(597) * (-16.0d+0) 
term(598) = term(598) * (32.0d+0) 
term(599) = term(599) * (-4.0d+0) 
term(600) = term(600) * (8.0d+0) 
term(601) = term(601) * (8.0d+0) 
term(602) = term(602) * (-16.0d+0) 
term(603) = term(603) * (8.0d+0) 
term(604) = term(604) * (-16.0d+0) 
term(605) = term(605) * (8.0d+0) 
term(606) = term(606) * (-16.0d+0) 
term(607) = term(607) * (-16.0d+0) 
term(608) = term(608) * (32.0d+0) 
term(609) = term(609) * (8.0d+0) 
term(610) = term(610) * (-16.0d+0) 
term(611) = term(611) * (-4.0d+0) 
term(612) = term(612) * (8.0d+0) 
term(613) = term(613) * (-4.0d+0) 
term(614) = term(614) * (8.0d+0) 
term(615) = term(615) * (-2.0d+0) 
term(616) = term(616) * (4.0d+0) 
term(617) = term(617) * (4.0d+0) 
term(618) = term(618) * (-8.0d+0) 
term(619) = term(619) * (-2.0d+0) 
term(620) = term(620) * (4.0d+0) 
term(621) = term(621) * (4.0d+0) 
term(622) = term(622) * (-8.0d+0) 
term(623) = term(623) * (4.0d+0) 
term(624) = term(624) * (-8.0d+0) 
term(625) = term(625) * (4.0d+0) 
term(626) = term(626) * (-8.0d+0) 
term(627) = term(627) * (-2.0d+0) 
term(628) = term(628) * (4.0d+0) 
term(629) = term(629) * (-2.0d+0) 
term(630) = term(630) * (4.0d+0) 
term(631) = term(631) * (-8.0d+0) 
term(632) = term(632) * (16.0d+0) 
term(633) = term(633) * (4.0d+0) 
term(634) = term(634) * (-8.0d+0) 
term(635) = term(635) * (4.0d+0) 
term(636) = term(636) * (-8.0d+0) 
term(637) = term(637) * (-8.0d+0) 
term(638) = term(638) * (16.0d+0) 
term(639) = term(639) * (-2.0d+0) 
term(640) = term(640) * (4.0d+0) 
term(641) = term(641) * (4.0d+0) 
term(642) = term(642) * (-8.0d+0) 
term(643) = term(643) * (-8.0d+0) 
term(644) = term(644) * (16.0d+0) 
term(645) = term(645) * (4.0d+0) 
term(646) = term(646) * (-8.0d+0) 
term(647) = term(647) * (-4.0d+0) 
term(648) = term(648) * (8.0d+0) 
term(649) = term(649) * (8.0d+0) 
term(650) = term(650) * (-16.0d+0) 
term(651) = term(651) * (-4.0d+0) 
term(652) = term(652) * (8.0d+0) 
term(653) = term(653) * (8.0d+0) 
term(654) = term(654) * (-16.0d+0) 
term(655) = term(655) * (-4.0d+0) 
term(656) = term(656) * (8.0d+0) 
term(657) = term(657) * (8.0d+0) 
term(658) = term(658) * (-16.0d+0) 
term(659) = term(659) * (-4.0d+0) 
term(660) = term(660) * (8.0d+0) 
term(661) = term(661) * (8.0d+0) 
term(662) = term(662) * (-16.0d+0) 

do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(663) = term(663) + t1(b,q) * wm_interm_16_pt4(a,p) * wm_interm_20_pt4(b,a)
term(664) = term(664) + t1(b,q) * wm_interm_16_pt4(a,p) * wm_interm_21_pt4(b,a)
term(665) = term(665) + t1(b,q) * wm_interm_20_pt4(b,a) * wm_interm_74_pt4(a,p)
term(666) = term(666) + t1(b,q) * wm_interm_21_pt4(b,a) * wm_interm_74_pt4(a,p)
term(667) = term(667) + t1(b,q) * wm_interm_20_pt4(b,a) * wm_interm_75_pt4(a,p)
term(668) = term(668) + t1(b,q) * wm_interm_21_pt4(b,a) * wm_interm_75_pt4(a,p)
term(669) = term(669) + t1(b,q) * wm_interm_51_pt4(b,a) * wm_interm_68_pt4(p,a)
term(670) = term(670) + t1(b,q) * wm_interm_51_pt4(b,a) * wm_interm_69_pt4(p,a)
term(671) = term(671) + t1(b,q) * wm_interm_52_pt4(b,a) * wm_interm_68_pt4(p,a)
term(672) = term(672) + t1(b,q) * wm_interm_52_pt4(b,a) * wm_interm_69_pt4(p,a)
term(673) = term(673) + t1(b,q) * wm_interm_20_pt4(a,p) * wm_interm_74_pt4(b,a)
term(674) = term(674) + t1(b,q) * wm_interm_20_pt4(a,p) * wm_interm_75_pt4(b,a)
term(675) = term(675) + t1(b,q) * wm_interm_21_pt4(a,p) * wm_interm_74_pt4(b,a)
term(676) = term(676) + t1(b,q) * wm_interm_21_pt4(a,p) * wm_interm_75_pt4(b,a)
end do 
end do 

term(663) = term(663) * (2.0d+0) 
term(664) = term(664) * (-4.0d+0) 
term(665) = term(665) * (-2.0d+0) 
term(666) = term(666) * (4.0d+0) 
term(667) = term(667) * (4.0d+0) 
term(668) = term(668) * (-8.0d+0) 
term(669) = term(669) * (4.0d+0) 
term(670) = term(670) * (-8.0d+0) 
term(671) = term(671) * (-2.0d+0) 
term(672) = term(672) * (4.0d+0) 
term(673) = term(673) * (-4.0d+0) 
term(674) = term(674) * (8.0d+0) 
term(675) = term(675) * (8.0d+0) 
term(676) = term(676) * (-16.0d+0) 

do a = nocc + 1, nactive 
term(677) = term(677) + t1(a,q) * wm_interm_20_pt4(a,p) * wm_interm_25_pt4
term(678) = term(678) + t1(a,q) * wm_interm_21_pt4(a,p) * wm_interm_25_pt4
term(679) = term(679) + t1(a,q) * wm_interm_20_pt4(a,p) * wm_interm_77_pt4
term(680) = term(680) + t1(a,q) * wm_interm_20_pt4(a,p) * wm_interm_78_pt4
term(681) = term(681) + t1(a,q) * wm_interm_21_pt4(a,p) * wm_interm_77_pt4
term(682) = term(682) + t1(a,q) * wm_interm_21_pt4(a,p) * wm_interm_78_pt4
term(683) = term(683) + wm_interm_37_pt4(p,a) * wm_interm_38_pt4(a,q)
term(684) = term(684) + wm_interm_37_pt4(p,a) * wm_interm_41_pt4(a,q)
term(685) = term(685) + wm_interm_0_pt4(a,p) * wm_interm_44_pt4(a,q)
term(686) = term(686) + wm_interm_0_pt4(a,p) * wm_interm_45_pt4(a,q)
end do 

term(677) = term(677) * (-8.0d+0) 
term(678) = term(678) * (16.0d+0) 
term(679) = term(679) * (4.0d+0) 
term(680) = term(680) * (-8.0d+0) 
term(681) = term(681) * (-8.0d+0) 
term(682) = term(682) * (16.0d+0) 
term(683) = term(683) * (2.0d+0) 
term(684) = term(684) * (-4.0d+0) 
term(685) = term(685) * (-4.0d+0) 
term(686) = term(686) * (2.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(687) = term(687) + s1(p,i) * wm_interm_48_pt4(a,b,j,i) * wm_interm_62_pt4(a,b,j,q)
term(688) = term(688) + s1(p,i) * wm_interm_48_pt4(a,b,j,i) * wm_interm_61_pt4(a,b,j,q)
term(689) = term(689) + s1(p,i) * wm_interm_48_pt4(a,b,j,i) * wm_interm_59_pt4(a,b,j,q)
term(690) = term(690) + s1(p,i) * wm_interm_48_pt4(a,b,j,i) * wm_interm_60_pt4(a,b,j,q)
term(691) = term(691) + s1(p,i) * wm_interm_49_pt4(a,b,j,i) * wm_interm_62_pt4(a,b,j,q)
term(692) = term(692) + s1(p,i) * wm_interm_49_pt4(a,b,j,i) * wm_interm_61_pt4(a,b,j,q)
term(693) = term(693) + s1(p,i) * wm_interm_49_pt4(a,b,j,i) * wm_interm_59_pt4(a,b,j,q)
term(694) = term(694) + s1(p,i) * wm_interm_49_pt4(a,b,j,i) * wm_interm_60_pt4(a,b,j,q)
term(695) = term(695) + s1(p,i) * wm_interm_50_pt4(a,b,j,i) * wm_interm_59_pt4(a,b,j,q)
term(696) = term(696) + s1(p,i) * wm_interm_50_pt4(a,b,j,i) * wm_interm_60_pt4(a,b,j,q)
term(697) = term(697) + s1(p,i) * wm_interm_50_pt4(a,b,j,i) * wm_interm_61_pt4(a,b,j,q)
term(698) = term(698) + s1(p,i) * wm_interm_50_pt4(a,b,j,i) * wm_interm_62_pt4(a,b,j,q)
term(699) = term(699) + s1(p,i) * wm_interm_53_pt4(a,b,j,i) * wm_interm_62_pt4(a,b,j,q)
term(700) = term(700) + s1(p,i) * wm_interm_53_pt4(a,b,j,i) * wm_interm_61_pt4(a,b,j,q)
term(701) = term(701) + s1(p,i) * wm_interm_53_pt4(a,b,j,i) * wm_interm_59_pt4(a,b,j,q)
term(702) = term(702) + s1(p,i) * wm_interm_53_pt4(a,b,j,i) * wm_interm_60_pt4(a,b,j,q)
term(703) = term(703) + r2(vrdav_Rr, a,j,p,i) * wm_interm_20_pt4(a,b) * wm_interm_43_pt4(b,i,j,q)
term(704) = term(704) + r2(vrdav_Rr, a,j,p,i) * wm_interm_21_pt4(a,b) * wm_interm_43_pt4(b,i,j,q)
term(705) = term(705) + r2(vrdav_Rr, a,j,p,i) * wm_interm_20_pt4(a,b) * wm_interm_42_pt4(b,i,j,q)
term(706) = term(706) + r2(vrdav_Rr, a,j,p,i) * wm_interm_21_pt4(a,b) * wm_interm_42_pt4(b,i,j,q)
term(707) = term(707) + r2(vrdav_Rr, a,j,p,i) * wm_interm_23_pt4(b,i,j,q) * wm_interm_51_pt4(a,b)
term(708) = term(708) + r2(vrdav_Rr, a,j,p,i) * wm_interm_23_pt4(b,i,j,q) * wm_interm_52_pt4(a,b)
term(709) = term(709) + t1(a,i) * wm_interm_49_pt4(a,b,j,q) * wm_interm_59_pt4(p,b,j,i)
term(710) = term(710) + t1(a,i) * wm_interm_49_pt4(a,b,j,q) * wm_interm_60_pt4(p,b,j,i)
term(711) = term(711) + t1(a,i) * wm_interm_49_pt4(a,b,j,q) * wm_interm_61_pt4(p,b,j,i)
term(712) = term(712) + t1(a,i) * wm_interm_49_pt4(a,b,j,q) * wm_interm_62_pt4(p,b,j,i)
term(713) = term(713) + t1(a,i) * wm_interm_48_pt4(a,b,j,q) * wm_interm_59_pt4(p,b,j,i)
term(714) = term(714) + t1(a,i) * wm_interm_48_pt4(a,b,j,q) * wm_interm_60_pt4(p,b,j,i)
term(715) = term(715) + t1(a,i) * wm_interm_48_pt4(a,b,j,q) * wm_interm_61_pt4(p,b,j,i)
term(716) = term(716) + t1(a,i) * wm_interm_48_pt4(a,b,j,q) * wm_interm_62_pt4(p,b,j,i)
term(717) = term(717) + t1(a,i) * wm_interm_53_pt4(a,b,j,q) * wm_interm_59_pt4(p,b,j,i)
term(718) = term(718) + t1(a,i) * wm_interm_53_pt4(a,b,j,q) * wm_interm_60_pt4(p,b,j,i)
term(719) = term(719) + t1(a,i) * wm_interm_53_pt4(a,b,j,q) * wm_interm_61_pt4(p,b,j,i)
term(720) = term(720) + t1(a,i) * wm_interm_53_pt4(a,b,j,q) * wm_interm_62_pt4(p,b,j,i)
term(721) = term(721) + t1(a,i) * wm_interm_50_pt4(a,b,j,q) * wm_interm_59_pt4(p,b,j,i)
term(722) = term(722) + t1(a,i) * wm_interm_50_pt4(a,b,j,q) * wm_interm_60_pt4(p,b,j,i)
term(723) = term(723) + t1(a,i) * wm_interm_50_pt4(a,b,j,q) * wm_interm_62_pt4(p,b,j,i)
term(724) = term(724) + t1(a,i) * wm_interm_50_pt4(a,b,j,q) * wm_interm_61_pt4(p,b,j,i)
term(725) = term(725) + t2(a,p,j,i) * wm_interm_23_pt4(b,i,j,q) * wm_interm_74_pt4(a,b)
term(726) = term(726) + t2(a,p,j,i) * wm_interm_23_pt4(b,i,j,q) * wm_interm_75_pt4(a,b)
term(727) = term(727) + t1(p,i) * wm_interm_53_pt4(a,b,j,q) * wm_interm_62_pt4(a,b,j,i)
term(728) = term(728) + t1(p,i) * wm_interm_53_pt4(a,b,j,q) * wm_interm_61_pt4(a,b,j,i)
term(729) = term(729) + t1(p,i) * wm_interm_50_pt4(a,b,j,q) * wm_interm_61_pt4(a,b,j,i)
term(730) = term(730) + t1(p,i) * wm_interm_50_pt4(a,b,j,q) * wm_interm_62_pt4(a,b,j,i)
term(731) = term(731) + t1(p,i) * wm_interm_53_pt4(a,b,j,q) * wm_interm_59_pt4(a,b,j,i)
term(732) = term(732) + t1(p,i) * wm_interm_53_pt4(a,b,j,q) * wm_interm_60_pt4(a,b,j,i)
term(733) = term(733) + t1(p,i) * wm_interm_50_pt4(a,b,j,q) * wm_interm_59_pt4(a,b,j,i)
term(734) = term(734) + t1(p,i) * wm_interm_50_pt4(a,b,j,q) * wm_interm_60_pt4(a,b,j,i)
term(735) = term(735) + t1(p,i) * wm_interm_49_pt4(a,b,j,q) * wm_interm_62_pt4(a,b,j,i)
term(736) = term(736) + t1(p,i) * wm_interm_49_pt4(a,b,j,q) * wm_interm_61_pt4(a,b,j,i)
term(737) = term(737) + t1(p,i) * wm_interm_48_pt4(a,b,j,q) * wm_interm_62_pt4(a,b,j,i)
term(738) = term(738) + t1(p,i) * wm_interm_48_pt4(a,b,j,q) * wm_interm_61_pt4(a,b,j,i)
term(739) = term(739) + t1(p,i) * wm_interm_49_pt4(a,b,j,q) * wm_interm_59_pt4(a,b,j,i)
term(740) = term(740) + t1(p,i) * wm_interm_49_pt4(a,b,j,q) * wm_interm_60_pt4(a,b,j,i)
term(741) = term(741) + t1(p,i) * wm_interm_48_pt4(a,b,j,q) * wm_interm_59_pt4(a,b,j,i)
term(742) = term(742) + t1(p,i) * wm_interm_48_pt4(a,b,j,q) * wm_interm_60_pt4(a,b,j,i)
end do 
end do 
end do 
end do 

term(687) = term(687) * (-8.0d+0) 
term(688) = term(688) * (16.0d+0) 
term(689) = term(689) * (16.0d+0) 
term(690) = term(690) * (-32.0d+0) 
term(691) = term(691) * (4.0d+0) 
term(692) = term(692) * (-8.0d+0) 
term(693) = term(693) * (-8.0d+0) 
term(694) = term(694) * (16.0d+0) 
term(695) = term(695) * (4.0d+0) 
term(696) = term(696) * (-8.0d+0) 
term(697) = term(697) * (4.0d+0) 
term(698) = term(698) * (-8.0d+0) 
term(699) = term(699) * (4.0d+0) 
term(700) = term(700) * (-8.0d+0) 
term(701) = term(701) * (-8.0d+0) 
term(702) = term(702) * (16.0d+0) 
term(703) = term(703) * (2.0d+0) 
term(704) = term(704) * (-4.0d+0) 
term(705) = term(705) * (-1.0d+0) 
term(706) = term(706) * (2.0d+0) 
term(707) = term(707) * (4.0d+0) 
term(708) = term(708) * (-2.0d+0) 
term(709) = term(709) * (-2.0d+0) 
term(710) = term(710) * (4.0d+0) 
term(711) = term(711) * (-2.0d+0) 
term(712) = term(712) * (4.0d+0) 
term(713) = term(713) * (4.0d+0) 
term(714) = term(714) * (-8.0d+0) 
term(715) = term(715) * (4.0d+0) 
term(716) = term(716) * (-8.0d+0) 
term(717) = term(717) * (-2.0d+0) 
term(718) = term(718) * (4.0d+0) 
term(719) = term(719) * (-2.0d+0) 
term(720) = term(720) * (4.0d+0) 
term(721) = term(721) * (4.0d+0) 
term(722) = term(722) * (-8.0d+0) 
term(723) = term(723) * (-2.0d+0) 
term(724) = term(724) * (4.0d+0) 
term(725) = term(725) * (-4.0d+0) 
term(726) = term(726) * (8.0d+0) 
term(727) = term(727) * (-4.0d+0) 
term(728) = term(728) * (8.0d+0) 
term(729) = term(729) * (-4.0d+0) 
term(730) = term(730) * (8.0d+0) 
term(731) = term(731) * (8.0d+0) 
term(732) = term(732) * (-16.0d+0) 
term(733) = term(733) * (-4.0d+0) 
term(734) = term(734) * (8.0d+0) 
term(735) = term(735) * (-4.0d+0) 
term(736) = term(736) * (8.0d+0) 
term(737) = term(737) * (8.0d+0) 
term(738) = term(738) * (-16.0d+0) 
term(739) = term(739) * (8.0d+0) 
term(740) = term(740) * (-16.0d+0) 
term(741) = term(741) * (-16.0d+0) 
term(742) = term(742) * (32.0d+0) 

do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(743) = term(743) + r2(vrdav_Rl, a,i,p,q) * wm_interm_68_pt4(a,b) * wm_interm_7_pt4(b,i)
term(744) = term(744) + r2(vrdav_Rl, a,i,p,q) * wm_interm_69_pt4(a,b) * wm_interm_7_pt4(b,i)
term(745) = term(745) + r2(vrdav_Rl, a,i,p,q) * wm_interm_68_pt4(a,b) * wm_interm_8_pt4(b,i)
term(746) = term(746) + r2(vrdav_Rl, a,i,p,q) * wm_interm_69_pt4(a,b) * wm_interm_8_pt4(b,i)
term(747) = term(747) + r2(vrdav_Rr, a,i,p,q) * wm_interm_20_pt4(a,b) * wm_interm_44_pt4(b,i)
term(748) = term(748) + r2(vrdav_Rr, a,i,p,q) * wm_interm_21_pt4(a,b) * wm_interm_44_pt4(b,i)
term(749) = term(749) + r2(vrdav_Rr, a,i,p,q) * wm_interm_20_pt4(a,b) * wm_interm_45_pt4(b,i)
term(750) = term(750) + r2(vrdav_Rr, a,i,p,q) * wm_interm_21_pt4(a,b) * wm_interm_45_pt4(b,i)
term(751) = term(751) + r2(vrdav_Rl, a,i,b,q) * wm_interm_68_pt4(a,p) * wm_interm_7_pt4(b,i)
term(752) = term(752) + r2(vrdav_Rl, a,i,b,q) * wm_interm_69_pt4(a,p) * wm_interm_7_pt4(b,i)
term(753) = term(753) + r2(vrdav_Rl, a,i,b,q) * wm_interm_68_pt4(a,p) * wm_interm_8_pt4(b,i)
term(754) = term(754) + r2(vrdav_Rl, a,i,b,q) * wm_interm_69_pt4(a,p) * wm_interm_8_pt4(b,i)
end do 
end do 
end do 

term(743) = term(743) * (16.0d+0) 
term(744) = term(744) * (-32.0d+0) 
term(745) = term(745) * (-8.0d+0) 
term(746) = term(746) * (16.0d+0) 
term(747) = term(747) * (-16.0d+0) 
term(748) = term(748) * (32.0d+0) 
term(749) = term(749) * (8.0d+0) 
term(750) = term(750) * (-16.0d+0) 
term(751) = term(751) * (4.0d+0) 
term(752) = term(752) * (-8.0d+0) 
term(753) = term(753) * (-2.0d+0) 
term(754) = term(754) * (4.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
do j = 1, nocc 
do b = nocc + 1, nactive 
term(755) = term(755) + s2(a,p,j,i) * wm_interm_28_pt4(b,j,i,k) * wm_interm_48_pt4(b,a,q,k)
term(756) = term(756) + s2(a,p,j,i) * wm_interm_28_pt4(b,j,i,k) * wm_interm_49_pt4(b,a,q,k)
term(757) = term(757) + s2(a,p,j,i) * wm_interm_28_pt4(b,j,i,k) * wm_interm_50_pt4(b,a,q,k)
term(758) = term(758) + s2(a,p,j,i) * wm_interm_28_pt4(b,j,i,k) * wm_interm_53_pt4(b,a,q,k)
end do 
end do 
end do 
end do 
end do 

term(755) = term(755) * (-8.0d+0) 
term(756) = term(756) * (4.0d+0) 
term(757) = term(757) * (-8.0d+0) 
term(758) = term(758) * (4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(759) = term(759) + s1(a,i) * wm_interm_13_pt4(p,i,j,k) * wm_interm_14_pt4(a,q,j,k)
term(760) = term(760) + s1(a,q) * wm_interm_13_pt4(p,i,j,k) * wm_interm_14_pt4(a,i,j,k)
term(761) = term(761) + s1(a,q) * wm_interm_13_pt4(p,i,j,k) * wm_interm_14_pt4(a,i,k,j)
term(762) = term(762) + s1(a,i) * wm_interm_13_pt4(p,i,j,k) * wm_interm_14_pt4(a,q,k,j)
term(763) = term(763) + t1(a,q) * wm_interm_13_pt4(a,i,j,k) * wm_interm_14_pt4(p,i,k,j)
term(764) = term(764) + t1(a,q) * wm_interm_13_pt4(a,i,j,k) * wm_interm_14_pt4(p,i,j,k)
term(765) = term(765) + s1(a,i) * wm_interm_5_pt4(p,a,j,k) * wm_interm_63_pt4(q,k,i,j)
term(766) = term(766) + s1(a,i) * wm_interm_1_pt4(p,a,j,k) * wm_interm_63_pt4(q,k,i,j)
term(767) = term(767) + s1(a,i) * wm_interm_11_pt4(p,a,j,k) * wm_interm_63_pt4(q,k,i,j)
term(768) = term(768) + s1(a,i) * wm_interm_12_pt4(p,a,j,k) * wm_interm_63_pt4(q,k,i,j)
term(769) = term(769) + s1(a,i) * wm_interm_1_pt4(p,a,j,k) * wm_interm_63_pt4(k,q,i,j)
term(770) = term(770) + s1(a,i) * wm_interm_5_pt4(p,a,j,k) * wm_interm_63_pt4(k,q,i,j)
term(771) = term(771) + s1(a,i) * wm_interm_11_pt4(p,a,j,k) * wm_interm_63_pt4(k,q,i,j)
term(772) = term(772) + s1(a,i) * wm_interm_12_pt4(p,a,j,k) * wm_interm_63_pt4(k,q,i,j)
term(773) = term(773) + s1(a,i) * wm_interm_55_pt4(q,j,i,k) * wm_interm_59_pt4(a,p,j,k)
term(774) = term(774) + s1(a,i) * wm_interm_55_pt4(q,j,i,k) * wm_interm_60_pt4(a,p,j,k)
term(775) = term(775) + s1(a,i) * wm_interm_55_pt4(q,j,i,k) * wm_interm_61_pt4(a,p,j,k)
term(776) = term(776) + s1(a,i) * wm_interm_55_pt4(q,j,i,k) * wm_interm_62_pt4(a,p,j,k)
term(777) = term(777) + s1(a,i) * wm_interm_55_pt4(j,q,i,k) * wm_interm_59_pt4(a,p,j,k)
term(778) = term(778) + s1(a,i) * wm_interm_55_pt4(j,q,i,k) * wm_interm_60_pt4(a,p,j,k)
term(779) = term(779) + s1(a,i) * wm_interm_55_pt4(j,q,i,k) * wm_interm_62_pt4(a,p,j,k)
term(780) = term(780) + s1(a,i) * wm_interm_55_pt4(j,q,i,k) * wm_interm_61_pt4(a,p,j,k)
end do 
end do 
end do 
end do 

term(759) = term(759) * (-4.0d+0) 
term(760) = term(760) * (8.0d+0) 
term(761) = term(761) * (-4.0d+0) 
term(762) = term(762) * (8.0d+0) 
term(763) = term(763) * (4.0d+0) 
term(764) = term(764) * (-8.0d+0) 
term(765) = term(765) * (-4.0d+0) 
term(766) = term(766) * (2.0d+0) 
term(767) = term(767) * (2.0d+0) 
term(768) = term(768) * (-4.0d+0) 
term(769) = term(769) * (-4.0d+0) 
term(770) = term(770) * (2.0d+0) 
term(771) = term(771) * (-4.0d+0) 
term(772) = term(772) * (8.0d+0) 
term(773) = term(773) * (2.0d+0) 
term(774) = term(774) * (-4.0d+0) 
term(775) = term(775) * (2.0d+0) 
term(776) = term(776) * (-4.0d+0) 
term(777) = term(777) * (-4.0d+0) 
term(778) = term(778) * (8.0d+0) 
term(779) = term(779) * (2.0d+0) 
term(780) = term(780) * (-4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(781) = term(781) + t1(a,i) * wm_interm_55_pt4(j,i,k,q) * wm_interm_59_pt4(p,a,j,k)
term(782) = term(782) + t1(a,i) * wm_interm_55_pt4(j,i,k,q) * wm_interm_60_pt4(p,a,j,k)
term(783) = term(783) + t1(a,i) * wm_interm_55_pt4(j,i,k,q) * wm_interm_61_pt4(p,a,j,k)
term(784) = term(784) + t1(a,i) * wm_interm_55_pt4(j,i,k,q) * wm_interm_62_pt4(p,a,j,k)
term(785) = term(785) + t1(a,i) * wm_interm_55_pt4(j,i,q,k) * wm_interm_62_pt4(p,a,j,k)
term(786) = term(786) + t1(a,i) * wm_interm_55_pt4(j,i,q,k) * wm_interm_61_pt4(p,a,j,k)
term(787) = term(787) + t1(a,i) * wm_interm_55_pt4(j,i,q,k) * wm_interm_59_pt4(p,a,j,k)
term(788) = term(788) + t1(a,i) * wm_interm_55_pt4(j,i,q,k) * wm_interm_60_pt4(p,a,j,k)
term(789) = term(789) + t1(a,i) * wm_interm_55_pt4(i,j,k,q) * wm_interm_62_pt4(p,a,j,k)
term(790) = term(790) + t1(a,i) * wm_interm_55_pt4(i,j,k,q) * wm_interm_61_pt4(p,a,j,k)
term(791) = term(791) + t1(a,i) * wm_interm_55_pt4(i,j,k,q) * wm_interm_59_pt4(p,a,j,k)
term(792) = term(792) + t1(a,i) * wm_interm_55_pt4(i,j,k,q) * wm_interm_60_pt4(p,a,j,k)
term(793) = term(793) + t1(a,i) * wm_interm_55_pt4(i,j,q,k) * wm_interm_59_pt4(p,a,j,k)
term(794) = term(794) + t1(a,i) * wm_interm_55_pt4(i,j,q,k) * wm_interm_60_pt4(p,a,j,k)
term(795) = term(795) + t1(a,i) * wm_interm_55_pt4(i,j,q,k) * wm_interm_61_pt4(p,a,j,k)
term(796) = term(796) + t1(a,i) * wm_interm_55_pt4(i,j,q,k) * wm_interm_62_pt4(p,a,j,k)
end do 
end do 
end do 
end do 

term(781) = term(781) * (-1.0d+0) 
term(782) = term(782) * (2.0d+0) 
term(783) = term(783) * (-1.0d+0) 
term(784) = term(784) * (2.0d+0) 
term(785) = term(785) * (-1.0d+0) 
term(786) = term(786) * (2.0d+0) 
term(787) = term(787) * (2.0d+0) 
term(788) = term(788) * (-4.0d+0) 
term(789) = term(789) * (-1.0d+0) 
term(790) = term(790) * (2.0d+0) 
term(791) = term(791) * (2.0d+0) 
term(792) = term(792) * (-4.0d+0) 
term(793) = term(793) * (-1.0d+0) 
term(794) = term(794) * (2.0d+0) 
term(795) = term(795) * (-1.0d+0) 
term(796) = term(796) * (2.0d+0) 

do i = 1, nocc 
do l = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(797) = term(797) + s2(a,p,j,i) * wm_interm_29_pt4(a,k,l,q) * wm_interm_55_pt4(k,l,j,i)
term(798) = term(798) + s2(a,p,j,i) * wm_interm_28_pt4(a,k,l,q) * wm_interm_55_pt4(k,l,j,i)
term(799) = term(799) + t2(a,p,j,i) * wm_interm_42_pt4(a,k,l,q) * wm_interm_72_pt4(k,l,j,i)
term(800) = term(800) + t2(a,p,j,i) * wm_interm_43_pt4(a,k,l,q) * wm_interm_72_pt4(k,l,j,i)
end do 
end do 
end do 
end do 
end do 

term(797) = term(797) * (-1.0d+0) 
term(798) = term(798) * (2.0d+0) 
term(799) = term(799) * (4.0d+0) 
term(800) = term(800) * (-2.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(801) = term(801) + s2(a,p,q,i) * wm_interm_6_pt4(j,i) * wm_interm_7_pt4(a,j)
term(802) = term(802) + s2(a,p,q,i) * wm_interm_6_pt4(j,i) * wm_interm_8_pt4(a,j)
term(803) = term(803) + s1(p,i) * wm_interm_14_pt4(a,q,j,i) * wm_interm_2_pt4(a,j)
term(804) = term(804) + s1(p,i) * wm_interm_14_pt4(a,q,j,i) * wm_interm_4_pt4(a,j)
term(805) = term(805) + s2(a,p,q,i) * wm_interm_15_pt4(a,j) * wm_interm_9_pt4(i,j)
term(806) = term(806) + s2(a,p,q,i) * wm_interm_17_pt4(a,j) * wm_interm_9_pt4(i,j)
term(807) = term(807) + s2(a,p,j,i) * wm_interm_15_pt4(a,j) * wm_interm_9_pt4(i,q)
term(808) = term(808) + s2(a,p,j,i) * wm_interm_17_pt4(a,j) * wm_interm_9_pt4(i,q)
term(809) = term(809) + r1(vrdav_Rr, p,i) * wm_interm_15_pt4(a,j) * wm_interm_23_pt4(a,j,i,q)
term(810) = term(810) + r1(vrdav_Rr, p,i) * wm_interm_17_pt4(a,j) * wm_interm_23_pt4(a,j,i,q)
term(811) = term(811) + t2(a,p,q,i) * wm_interm_22_pt4(i,j) * wm_interm_2_pt4(a,j)
term(812) = term(812) + t2(a,p,q,i) * wm_interm_22_pt4(i,j) * wm_interm_4_pt4(a,j)
term(813) = term(813) + t2(a,p,j,i) * wm_interm_22_pt4(i,q) * wm_interm_4_pt4(a,j)
term(814) = term(814) + t2(a,p,j,i) * wm_interm_22_pt4(j,q) * wm_interm_4_pt4(a,i)
term(815) = term(815) + t2(a,p,j,i) * wm_interm_22_pt4(j,q) * wm_interm_2_pt4(a,i)
term(816) = term(816) + t2(a,p,j,i) * wm_interm_22_pt4(i,q) * wm_interm_2_pt4(a,j)
term(817) = term(817) + r2(vrdav_Rl, a,q,p,i) * wm_interm_18_pt4(j,i) * wm_interm_31_pt4(a,j)
term(818) = term(818) + r2(vrdav_Rl, a,q,p,i) * wm_interm_19_pt4(j,i) * wm_interm_31_pt4(a,j)
term(819) = term(819) + r2(vrdav_Rl, a,q,p,i) * wm_interm_18_pt4(j,i) * wm_interm_34_pt4(a,j)
term(820) = term(820) + r2(vrdav_Rl, a,q,p,i) * wm_interm_19_pt4(j,i) * wm_interm_34_pt4(a,j)
term(821) = term(821) + s2(a,p,q,i) * wm_interm_31_pt4(a,j) * wm_interm_54_pt4(j,i)
term(822) = term(822) + s2(a,p,j,i) * wm_interm_31_pt4(a,j) * wm_interm_54_pt4(q,i)
term(823) = term(823) + s2(a,p,q,i) * wm_interm_31_pt4(a,j) * wm_interm_56_pt4(j,i)
term(824) = term(824) + s2(a,p,j,i) * wm_interm_31_pt4(a,j) * wm_interm_56_pt4(q,i)
term(825) = term(825) + s2(a,p,q,i) * wm_interm_34_pt4(a,j) * wm_interm_54_pt4(j,i)
term(826) = term(826) + s2(a,p,j,i) * wm_interm_34_pt4(a,j) * wm_interm_54_pt4(q,i)
term(827) = term(827) + s2(a,p,q,i) * wm_interm_34_pt4(a,j) * wm_interm_56_pt4(j,i)
term(828) = term(828) + s2(a,p,j,i) * wm_interm_34_pt4(a,j) * wm_interm_56_pt4(q,i)
term(829) = term(829) + s2(a,p,q,i) * wm_interm_66_pt4(j,i) * wm_interm_7_pt4(a,j)
term(830) = term(830) + s2(a,p,q,i) * wm_interm_67_pt4(j,i) * wm_interm_7_pt4(a,j)
term(831) = term(831) + s2(a,p,q,i) * wm_interm_66_pt4(j,i) * wm_interm_8_pt4(a,j)
term(832) = term(832) + s2(a,p,q,i) * wm_interm_67_pt4(j,i) * wm_interm_8_pt4(a,j)
term(833) = term(833) + r2(vrdav_Rl, a,q,p,i) * wm_interm_70_pt4(i,j) * wm_interm_8_pt4(a,j)
term(834) = term(834) + r2(vrdav_Rl, a,q,p,i) * wm_interm_71_pt4(i,j) * wm_interm_8_pt4(a,j)
term(835) = term(835) + r2(vrdav_Rl, a,q,p,i) * wm_interm_70_pt4(i,j) * wm_interm_7_pt4(a,j)
term(836) = term(836) + r2(vrdav_Rl, a,q,p,i) * wm_interm_71_pt4(i,j) * wm_interm_7_pt4(a,j)
term(837) = term(837) + r2(vrdav_Rr, a,q,p,i) * wm_interm_18_pt4(i,j) * wm_interm_44_pt4(a,j)
term(838) = term(838) + r2(vrdav_Rr, a,q,p,i) * wm_interm_19_pt4(i,j) * wm_interm_44_pt4(a,j)
term(839) = term(839) + r2(vrdav_Rr, a,q,p,i) * wm_interm_18_pt4(i,j) * wm_interm_45_pt4(a,j)
term(840) = term(840) + r2(vrdav_Rr, a,q,p,i) * wm_interm_19_pt4(i,j) * wm_interm_45_pt4(a,j)
term(841) = term(841) + t1(a,q) * wm_interm_18_pt4(i,j) * wm_interm_58_pt4(a,p,j,i)
term(842) = term(842) + t1(a,q) * wm_interm_18_pt4(i,j) * wm_interm_57_pt4(a,p,j,i)
term(843) = term(843) + t1(a,q) * wm_interm_19_pt4(i,j) * wm_interm_58_pt4(a,p,j,i)
term(844) = term(844) + t1(a,q) * wm_interm_19_pt4(i,j) * wm_interm_57_pt4(a,p,j,i)
term(845) = term(845) + t1(a,q) * wm_interm_18_pt4(i,j) * wm_interm_64_pt4(a,p,j,i)
term(846) = term(846) + t1(a,q) * wm_interm_19_pt4(i,j) * wm_interm_64_pt4(a,p,j,i)
term(847) = term(847) + t1(a,q) * wm_interm_18_pt4(i,j) * wm_interm_65_pt4(a,p,j,i)
term(848) = term(848) + t1(a,q) * wm_interm_19_pt4(i,j) * wm_interm_65_pt4(a,p,j,i)
term(849) = term(849) + r2(vrdav_Rr, a,j,p,i) * wm_interm_18_pt4(j,q) * wm_interm_44_pt4(a,i)
term(850) = term(850) + r2(vrdav_Rr, a,j,p,i) * wm_interm_18_pt4(i,q) * wm_interm_44_pt4(a,j)
term(851) = term(851) + r2(vrdav_Rr, a,j,p,i) * wm_interm_19_pt4(j,q) * wm_interm_44_pt4(a,i)
term(852) = term(852) + r2(vrdav_Rr, a,j,p,i) * wm_interm_19_pt4(i,q) * wm_interm_44_pt4(a,j)
term(853) = term(853) + r2(vrdav_Rr, a,j,p,i) * wm_interm_18_pt4(j,q) * wm_interm_45_pt4(a,i)
term(854) = term(854) + r2(vrdav_Rr, a,j,p,i) * wm_interm_18_pt4(i,q) * wm_interm_45_pt4(a,j)
term(855) = term(855) + r2(vrdav_Rr, a,j,p,i) * wm_interm_19_pt4(j,q) * wm_interm_45_pt4(a,i)
term(856) = term(856) + r2(vrdav_Rr, a,j,p,i) * wm_interm_19_pt4(i,q) * wm_interm_45_pt4(a,j)
term(857) = term(857) + t2(a,p,q,i) * wm_interm_44_pt4(a,j) * wm_interm_70_pt4(j,i)
term(858) = term(858) + t2(a,p,q,i) * wm_interm_44_pt4(a,j) * wm_interm_71_pt4(j,i)
term(859) = term(859) + t2(a,p,q,i) * wm_interm_45_pt4(a,j) * wm_interm_70_pt4(j,i)
term(860) = term(860) + t2(a,p,q,i) * wm_interm_45_pt4(a,j) * wm_interm_71_pt4(j,i)
end do 
end do 
end do 

term(801) = term(801) * (8.0d+0) 
term(802) = term(802) * (-4.0d+0) 
term(803) = term(803) * (8.0d+0) 
term(804) = term(804) * (-4.0d+0) 
term(805) = term(805) * (8.0d+0) 
term(806) = term(806) * (-4.0d+0) 
term(807) = term(807) * (8.0d+0) 
term(808) = term(808) * (-4.0d+0) 
term(809) = term(809) * (8.0d+0) 
term(810) = term(810) * (-4.0d+0) 
term(811) = term(811) * (-4.0d+0) 
term(812) = term(812) * (2.0d+0) 
term(813) = term(813) * (-8.0d+0) 
term(814) = term(814) * (4.0d+0) 
term(815) = term(815) * (-8.0d+0) 
term(816) = term(816) * (16.0d+0) 
term(817) = term(817) * (-4.0d+0) 
term(818) = term(818) * (8.0d+0) 
term(819) = term(819) * (2.0d+0) 
term(820) = term(820) * (-4.0d+0) 
term(821) = term(821) * (-4.0d+0) 
term(822) = term(822) * (8.0d+0) 
term(823) = term(823) * (8.0d+0) 
term(824) = term(824) * (-16.0d+0) 
term(825) = term(825) * (2.0d+0) 
term(826) = term(826) * (-4.0d+0) 
term(827) = term(827) * (-4.0d+0) 
term(828) = term(828) * (8.0d+0) 
term(829) = term(829) * (-8.0d+0) 
term(830) = term(830) * (16.0d+0) 
term(831) = term(831) * (4.0d+0) 
term(832) = term(832) * (-8.0d+0) 
term(833) = term(833) * (4.0d+0) 
term(834) = term(834) * (-8.0d+0) 
term(835) = term(835) * (-8.0d+0) 
term(836) = term(836) * (16.0d+0) 
term(837) = term(837) * (8.0d+0) 
term(838) = term(838) * (-16.0d+0) 
term(839) = term(839) * (-4.0d+0) 
term(840) = term(840) * (8.0d+0) 
term(841) = term(841) * (-2.0d+0) 
term(842) = term(842) * (4.0d+0) 
term(843) = term(843) * (4.0d+0) 
term(844) = term(844) * (-8.0d+0) 
term(845) = term(845) * (-2.0d+0) 
term(846) = term(846) * (4.0d+0) 
term(847) = term(847) * (4.0d+0) 
term(848) = term(848) * (-8.0d+0) 
term(849) = term(849) * (4.0d+0) 
term(850) = term(850) * (-8.0d+0) 
term(851) = term(851) * (-8.0d+0) 
term(852) = term(852) * (16.0d+0) 
term(853) = term(853) * (-2.0d+0) 
term(854) = term(854) * (4.0d+0) 
term(855) = term(855) * (4.0d+0) 
term(856) = term(856) * (-8.0d+0) 
term(857) = term(857) * (4.0d+0) 
term(858) = term(858) * (-8.0d+0) 
term(859) = term(859) * (-2.0d+0) 
term(860) = term(860) * (4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(861) = term(861) + s2(a,b,i,j) * wm_interm_31_pt4(a,j) * wm_interm_48_pt4(p,b,q,i)
term(862) = term(862) + s2(a,b,i,j) * wm_interm_34_pt4(a,j) * wm_interm_48_pt4(p,b,q,i)
term(863) = term(863) + s2(a,b,i,j) * wm_interm_31_pt4(a,j) * wm_interm_49_pt4(p,b,q,i)
term(864) = term(864) + s2(a,b,i,j) * wm_interm_34_pt4(a,j) * wm_interm_49_pt4(p,b,q,i)
term(865) = term(865) + s2(a,b,i,j) * wm_interm_31_pt4(a,j) * wm_interm_50_pt4(p,b,q,i)
term(866) = term(866) + s2(a,b,i,j) * wm_interm_31_pt4(a,j) * wm_interm_53_pt4(p,b,q,i)
term(867) = term(867) + s2(a,b,i,j) * wm_interm_34_pt4(a,j) * wm_interm_50_pt4(p,b,q,i)
term(868) = term(868) + s2(a,b,i,j) * wm_interm_34_pt4(a,j) * wm_interm_53_pt4(p,b,q,i)
term(869) = term(869) + s1(p,i) * wm_interm_1_pt4(a,b,j,i) * wm_interm_57_pt4(b,a,q,j)
term(870) = term(870) + s1(p,i) * wm_interm_1_pt4(a,b,j,i) * wm_interm_58_pt4(b,a,q,j)
term(871) = term(871) + s1(a,i) * wm_interm_1_pt4(p,b,j,i) * wm_interm_58_pt4(b,a,q,j)
term(872) = term(872) + s1(a,i) * wm_interm_1_pt4(p,b,j,i) * wm_interm_57_pt4(b,a,q,j)
term(873) = term(873) + s1(p,i) * wm_interm_58_pt4(a,b,q,j) * wm_interm_5_pt4(b,a,j,i)
term(874) = term(874) + s1(p,i) * wm_interm_57_pt4(a,b,q,j) * wm_interm_5_pt4(b,a,j,i)
term(875) = term(875) + s1(a,i) * wm_interm_57_pt4(b,a,q,j) * wm_interm_5_pt4(p,b,j,i)
term(876) = term(876) + s1(a,i) * wm_interm_58_pt4(b,a,q,j) * wm_interm_5_pt4(p,b,j,i)
term(877) = term(877) + s1(p,i) * wm_interm_1_pt4(a,b,j,i) * wm_interm_64_pt4(b,a,q,j)
term(878) = term(878) + s1(p,i) * wm_interm_1_pt4(a,b,j,i) * wm_interm_65_pt4(b,a,q,j)
term(879) = term(879) + s1(a,i) * wm_interm_1_pt4(p,b,j,i) * wm_interm_64_pt4(b,a,q,j)
term(880) = term(880) + s1(a,i) * wm_interm_1_pt4(p,b,j,i) * wm_interm_65_pt4(b,a,q,j)
term(881) = term(881) + s1(p,i) * wm_interm_5_pt4(a,b,j,i) * wm_interm_64_pt4(b,a,q,j)
term(882) = term(882) + s1(p,i) * wm_interm_5_pt4(a,b,j,i) * wm_interm_65_pt4(b,a,q,j)
term(883) = term(883) + s1(a,i) * wm_interm_5_pt4(p,b,j,i) * wm_interm_64_pt4(b,a,q,j)
term(884) = term(884) + s1(a,i) * wm_interm_5_pt4(p,b,j,i) * wm_interm_65_pt4(b,a,q,j)
term(885) = term(885) + s1(p,i) * wm_interm_11_pt4(a,b,j,i) * wm_interm_57_pt4(b,a,q,j)
term(886) = term(886) + s1(p,i) * wm_interm_11_pt4(a,b,j,i) * wm_interm_58_pt4(b,a,q,j)
term(887) = term(887) + s1(p,i) * wm_interm_12_pt4(a,b,j,i) * wm_interm_57_pt4(b,a,q,j)
term(888) = term(888) + s1(p,i) * wm_interm_12_pt4(a,b,j,i) * wm_interm_58_pt4(b,a,q,j)
term(889) = term(889) + s1(a,i) * wm_interm_11_pt4(p,b,j,i) * wm_interm_58_pt4(b,a,q,j)
term(890) = term(890) + s1(a,i) * wm_interm_12_pt4(p,b,j,i) * wm_interm_58_pt4(b,a,q,j)
term(891) = term(891) + s1(a,i) * wm_interm_11_pt4(p,b,j,i) * wm_interm_57_pt4(b,a,q,j)
term(892) = term(892) + s1(a,i) * wm_interm_12_pt4(p,b,j,i) * wm_interm_57_pt4(b,a,q,j)
term(893) = term(893) + s1(p,i) * wm_interm_11_pt4(a,b,j,i) * wm_interm_64_pt4(b,a,q,j)
term(894) = term(894) + s1(p,i) * wm_interm_11_pt4(a,b,j,i) * wm_interm_65_pt4(b,a,q,j)
term(895) = term(895) + s1(p,i) * wm_interm_12_pt4(a,b,j,i) * wm_interm_64_pt4(b,a,q,j)
term(896) = term(896) + s1(p,i) * wm_interm_12_pt4(a,b,j,i) * wm_interm_65_pt4(b,a,q,j)
term(897) = term(897) + s1(a,i) * wm_interm_11_pt4(p,b,j,i) * wm_interm_64_pt4(b,a,q,j)
term(898) = term(898) + s1(a,i) * wm_interm_11_pt4(p,b,j,i) * wm_interm_65_pt4(b,a,q,j)
term(899) = term(899) + s1(a,i) * wm_interm_12_pt4(p,b,j,i) * wm_interm_64_pt4(b,a,q,j)
term(900) = term(900) + s1(a,i) * wm_interm_12_pt4(p,b,j,i) * wm_interm_65_pt4(b,a,q,j)
term(901) = term(901) + t2(a,b,i,j) * wm_interm_44_pt4(b,j) * wm_interm_62_pt4(p,a,q,i)
term(902) = term(902) + t2(a,b,i,j) * wm_interm_44_pt4(b,j) * wm_interm_61_pt4(p,a,q,i)
term(903) = term(903) + t2(a,b,i,j) * wm_interm_44_pt4(b,j) * wm_interm_59_pt4(p,a,q,i)
term(904) = term(904) + t2(a,b,i,j) * wm_interm_44_pt4(b,j) * wm_interm_60_pt4(p,a,q,i)
term(905) = term(905) + t2(a,b,i,j) * wm_interm_45_pt4(b,j) * wm_interm_62_pt4(p,a,q,i)
term(906) = term(906) + t2(a,b,i,j) * wm_interm_45_pt4(b,j) * wm_interm_61_pt4(p,a,q,i)
term(907) = term(907) + t2(a,b,i,j) * wm_interm_45_pt4(b,j) * wm_interm_59_pt4(p,a,q,i)
term(908) = term(908) + t2(a,b,i,j) * wm_interm_45_pt4(b,j) * wm_interm_60_pt4(p,a,q,i)
term(909) = term(909) + s1(p,i) * wm_interm_48_pt4(a,b,q,j) * wm_interm_62_pt4(a,b,i,j)
term(910) = term(910) + s1(p,i) * wm_interm_48_pt4(a,b,q,j) * wm_interm_61_pt4(a,b,i,j)
term(911) = term(911) + s1(p,i) * wm_interm_48_pt4(a,b,q,j) * wm_interm_59_pt4(a,b,i,j)
term(912) = term(912) + s1(p,i) * wm_interm_48_pt4(a,b,q,j) * wm_interm_60_pt4(a,b,i,j)
term(913) = term(913) + s1(p,i) * wm_interm_11_pt4(a,b,q,j) * wm_interm_64_pt4(b,a,j,i)
term(914) = term(914) + s1(p,i) * wm_interm_11_pt4(a,b,q,j) * wm_interm_65_pt4(b,a,j,i)
term(915) = term(915) + s1(p,q) * wm_interm_11_pt4(a,b,i,j) * wm_interm_64_pt4(b,a,j,i)
term(916) = term(916) + s1(p,q) * wm_interm_11_pt4(a,b,i,j) * wm_interm_65_pt4(b,a,j,i)
term(917) = term(917) + s1(p,i) * wm_interm_12_pt4(a,b,q,j) * wm_interm_64_pt4(b,a,j,i)
term(918) = term(918) + s1(p,i) * wm_interm_12_pt4(a,b,q,j) * wm_interm_65_pt4(b,a,j,i)
term(919) = term(919) + s1(p,q) * wm_interm_12_pt4(a,b,i,j) * wm_interm_64_pt4(b,a,j,i)
term(920) = term(920) + s1(p,q) * wm_interm_12_pt4(a,b,i,j) * wm_interm_65_pt4(b,a,j,i)
term(921) = term(921) + s1(p,i) * wm_interm_49_pt4(a,b,q,j) * wm_interm_62_pt4(a,b,i,j)
term(922) = term(922) + s1(p,i) * wm_interm_49_pt4(a,b,q,j) * wm_interm_61_pt4(a,b,i,j)
term(923) = term(923) + s1(p,i) * wm_interm_49_pt4(a,b,q,j) * wm_interm_59_pt4(a,b,i,j)
term(924) = term(924) + s1(p,i) * wm_interm_49_pt4(a,b,q,j) * wm_interm_60_pt4(a,b,i,j)
term(925) = term(925) + s1(p,i) * wm_interm_11_pt4(a,b,q,j) * wm_interm_57_pt4(b,a,j,i)
term(926) = term(926) + s1(p,i) * wm_interm_11_pt4(a,b,q,j) * wm_interm_58_pt4(b,a,j,i)
term(927) = term(927) + s1(p,q) * wm_interm_11_pt4(a,b,i,j) * wm_interm_57_pt4(b,a,j,i)
term(928) = term(928) + s1(p,q) * wm_interm_11_pt4(a,b,i,j) * wm_interm_58_pt4(b,a,j,i)
term(929) = term(929) + s1(p,i) * wm_interm_12_pt4(a,b,q,j) * wm_interm_57_pt4(b,a,j,i)
term(930) = term(930) + s1(p,i) * wm_interm_12_pt4(a,b,q,j) * wm_interm_58_pt4(b,a,j,i)
term(931) = term(931) + s1(p,q) * wm_interm_12_pt4(a,b,i,j) * wm_interm_57_pt4(b,a,j,i)
term(932) = term(932) + s1(p,q) * wm_interm_12_pt4(a,b,i,j) * wm_interm_58_pt4(b,a,j,i)
term(933) = term(933) + s1(p,i) * wm_interm_50_pt4(a,b,q,j) * wm_interm_59_pt4(a,b,i,j)
term(934) = term(934) + s1(p,i) * wm_interm_50_pt4(a,b,q,j) * wm_interm_60_pt4(a,b,i,j)
term(935) = term(935) + s1(p,i) * wm_interm_1_pt4(a,b,q,j) * wm_interm_57_pt4(b,a,j,i)
term(936) = term(936) + s1(p,i) * wm_interm_1_pt4(a,b,q,j) * wm_interm_58_pt4(b,a,j,i)
term(937) = term(937) + s1(p,q) * wm_interm_1_pt4(a,b,i,j) * wm_interm_57_pt4(b,a,j,i)
term(938) = term(938) + s1(p,q) * wm_interm_1_pt4(a,b,i,j) * wm_interm_58_pt4(b,a,j,i)
term(939) = term(939) + s1(p,i) * wm_interm_58_pt4(a,b,j,i) * wm_interm_5_pt4(b,a,q,j)
term(940) = term(940) + s1(p,i) * wm_interm_57_pt4(a,b,j,i) * wm_interm_5_pt4(b,a,q,j)
term(941) = term(941) + s1(p,i) * wm_interm_50_pt4(a,b,q,j) * wm_interm_61_pt4(a,b,i,j)
term(942) = term(942) + s1(p,i) * wm_interm_50_pt4(a,b,q,j) * wm_interm_62_pt4(a,b,i,j)
term(943) = term(943) + s1(p,q) * wm_interm_58_pt4(a,b,i,j) * wm_interm_5_pt4(b,a,j,i)
term(944) = term(944) + s1(p,q) * wm_interm_57_pt4(a,b,i,j) * wm_interm_5_pt4(b,a,j,i)
term(945) = term(945) + s1(p,i) * wm_interm_53_pt4(a,b,q,j) * wm_interm_62_pt4(a,b,i,j)
term(946) = term(946) + s1(p,i) * wm_interm_53_pt4(a,b,q,j) * wm_interm_61_pt4(a,b,i,j)
term(947) = term(947) + s1(p,i) * wm_interm_53_pt4(a,b,q,j) * wm_interm_59_pt4(a,b,i,j)
term(948) = term(948) + s1(p,i) * wm_interm_53_pt4(a,b,q,j) * wm_interm_60_pt4(a,b,i,j)
term(949) = term(949) + s1(p,i) * wm_interm_5_pt4(a,b,q,j) * wm_interm_64_pt4(b,a,j,i)
term(950) = term(950) + s1(p,i) * wm_interm_5_pt4(a,b,q,j) * wm_interm_65_pt4(b,a,j,i)
term(951) = term(951) + s1(p,q) * wm_interm_5_pt4(a,b,i,j) * wm_interm_64_pt4(b,a,j,i)
term(952) = term(952) + s1(p,q) * wm_interm_5_pt4(a,b,i,j) * wm_interm_65_pt4(b,a,j,i)
term(953) = term(953) + s1(p,i) * wm_interm_1_pt4(a,b,q,j) * wm_interm_64_pt4(b,a,j,i)
term(954) = term(954) + s1(p,i) * wm_interm_1_pt4(a,b,q,j) * wm_interm_65_pt4(b,a,j,i)
term(955) = term(955) + s1(p,q) * wm_interm_1_pt4(a,b,i,j) * wm_interm_64_pt4(b,a,j,i)
term(956) = term(956) + s1(p,q) * wm_interm_1_pt4(a,b,i,j) * wm_interm_65_pt4(b,a,j,i)
term(957) = term(957) + s1(a,q) * wm_interm_11_pt4(p,b,i,j) * wm_interm_64_pt4(b,a,j,i)
term(958) = term(958) + s1(a,q) * wm_interm_12_pt4(p,b,i,j) * wm_interm_64_pt4(b,a,j,i)
term(959) = term(959) + s1(a,q) * wm_interm_11_pt4(p,b,i,j) * wm_interm_65_pt4(b,a,j,i)
term(960) = term(960) + s1(a,q) * wm_interm_12_pt4(p,b,i,j) * wm_interm_65_pt4(b,a,j,i)
term(961) = term(961) + s1(a,q) * wm_interm_11_pt4(p,b,i,j) * wm_interm_57_pt4(b,a,j,i)
term(962) = term(962) + s1(a,q) * wm_interm_12_pt4(p,b,i,j) * wm_interm_57_pt4(b,a,j,i)
term(963) = term(963) + s1(a,q) * wm_interm_11_pt4(p,b,i,j) * wm_interm_58_pt4(b,a,j,i)
term(964) = term(964) + s1(a,q) * wm_interm_12_pt4(p,b,i,j) * wm_interm_58_pt4(b,a,j,i)
term(965) = term(965) + s1(a,q) * wm_interm_1_pt4(p,b,i,j) * wm_interm_57_pt4(b,a,j,i)
term(966) = term(966) + s1(a,q) * wm_interm_1_pt4(p,b,i,j) * wm_interm_58_pt4(b,a,j,i)
term(967) = term(967) + s1(a,q) * wm_interm_58_pt4(b,a,i,j) * wm_interm_5_pt4(p,b,j,i)
term(968) = term(968) + s1(a,q) * wm_interm_57_pt4(b,a,i,j) * wm_interm_5_pt4(p,b,j,i)
term(969) = term(969) + s1(a,q) * wm_interm_5_pt4(p,b,i,j) * wm_interm_64_pt4(b,a,j,i)
term(970) = term(970) + s1(a,q) * wm_interm_1_pt4(p,b,i,j) * wm_interm_64_pt4(b,a,j,i)
term(971) = term(971) + s1(a,q) * wm_interm_5_pt4(p,b,i,j) * wm_interm_65_pt4(b,a,j,i)
term(972) = term(972) + s1(a,q) * wm_interm_1_pt4(p,b,i,j) * wm_interm_65_pt4(b,a,j,i)
term(973) = term(973) + t1(a,q) * wm_interm_48_pt4(a,b,i,j) * wm_interm_62_pt4(p,b,i,j)
term(974) = term(974) + t1(a,q) * wm_interm_48_pt4(a,b,i,j) * wm_interm_61_pt4(p,b,i,j)
term(975) = term(975) + t1(a,q) * wm_interm_48_pt4(a,b,i,j) * wm_interm_59_pt4(p,b,i,j)
term(976) = term(976) + t1(a,q) * wm_interm_48_pt4(a,b,i,j) * wm_interm_60_pt4(p,b,i,j)
term(977) = term(977) + t1(a,q) * wm_interm_11_pt4(a,b,i,j) * wm_interm_64_pt4(b,p,j,i)
term(978) = term(978) + t1(a,q) * wm_interm_12_pt4(a,b,i,j) * wm_interm_64_pt4(b,p,j,i)
term(979) = term(979) + t1(a,q) * wm_interm_11_pt4(a,b,i,j) * wm_interm_65_pt4(b,p,j,i)
term(980) = term(980) + t1(a,q) * wm_interm_12_pt4(a,b,i,j) * wm_interm_65_pt4(b,p,j,i)
term(981) = term(981) + t1(a,q) * wm_interm_49_pt4(a,b,i,j) * wm_interm_62_pt4(p,b,i,j)
term(982) = term(982) + t1(a,q) * wm_interm_49_pt4(a,b,i,j) * wm_interm_61_pt4(p,b,i,j)
term(983) = term(983) + t1(a,q) * wm_interm_49_pt4(a,b,i,j) * wm_interm_59_pt4(p,b,i,j)
term(984) = term(984) + t1(a,q) * wm_interm_49_pt4(a,b,i,j) * wm_interm_60_pt4(p,b,i,j)
term(985) = term(985) + t1(a,q) * wm_interm_11_pt4(a,b,i,j) * wm_interm_57_pt4(b,p,j,i)
term(986) = term(986) + t1(a,q) * wm_interm_12_pt4(a,b,i,j) * wm_interm_57_pt4(b,p,j,i)
term(987) = term(987) + t1(a,q) * wm_interm_11_pt4(a,b,i,j) * wm_interm_58_pt4(b,p,j,i)
term(988) = term(988) + t1(a,q) * wm_interm_12_pt4(a,b,i,j) * wm_interm_58_pt4(b,p,j,i)
term(989) = term(989) + t1(a,q) * wm_interm_50_pt4(a,b,i,j) * wm_interm_59_pt4(p,b,i,j)
term(990) = term(990) + t1(a,q) * wm_interm_50_pt4(a,b,i,j) * wm_interm_60_pt4(p,b,i,j)
term(991) = term(991) + t1(a,q) * wm_interm_1_pt4(a,b,i,j) * wm_interm_57_pt4(b,p,j,i)
term(992) = term(992) + t1(a,q) * wm_interm_1_pt4(a,b,i,j) * wm_interm_58_pt4(b,p,j,i)
term(993) = term(993) + t1(a,q) * wm_interm_50_pt4(a,b,i,j) * wm_interm_61_pt4(p,b,i,j)
term(994) = term(994) + t1(a,q) * wm_interm_50_pt4(a,b,i,j) * wm_interm_62_pt4(p,b,i,j)
term(995) = term(995) + t1(a,q) * wm_interm_58_pt4(b,p,i,j) * wm_interm_5_pt4(a,b,j,i)
term(996) = term(996) + t1(a,q) * wm_interm_57_pt4(b,p,i,j) * wm_interm_5_pt4(a,b,j,i)
term(997) = term(997) + t1(a,q) * wm_interm_53_pt4(a,b,i,j) * wm_interm_62_pt4(p,b,i,j)
term(998) = term(998) + t1(a,q) * wm_interm_53_pt4(a,b,i,j) * wm_interm_61_pt4(p,b,i,j)
term(999) = term(999) + t1(a,q) * wm_interm_53_pt4(a,b,i,j) * wm_interm_59_pt4(p,b,i,j)
term(1000) = term(1000) + t1(a,q) * wm_interm_53_pt4(a,b,i,j) * wm_interm_60_pt4(p,b,i,j)
term(1001) = term(1001) + t1(a,q) * wm_interm_5_pt4(a,b,i,j) * wm_interm_64_pt4(b,p,j,i)
term(1002) = term(1002) + t1(a,q) * wm_interm_1_pt4(a,b,i,j) * wm_interm_64_pt4(b,p,j,i)
term(1003) = term(1003) + t1(a,q) * wm_interm_5_pt4(a,b,i,j) * wm_interm_65_pt4(b,p,j,i)
term(1004) = term(1004) + t1(a,q) * wm_interm_1_pt4(a,b,i,j) * wm_interm_65_pt4(b,p,j,i)
term(1005) = term(1005) + t1(p,q) * wm_interm_11_pt4(a,b,i,j) * wm_interm_64_pt4(b,a,j,i)
term(1006) = term(1006) + t1(p,q) * wm_interm_11_pt4(a,b,i,j) * wm_interm_65_pt4(b,a,j,i)
term(1007) = term(1007) + t1(p,q) * wm_interm_12_pt4(a,b,i,j) * wm_interm_64_pt4(b,a,j,i)
term(1008) = term(1008) + t1(p,q) * wm_interm_12_pt4(a,b,i,j) * wm_interm_65_pt4(b,a,j,i)
term(1009) = term(1009) + t1(p,q) * wm_interm_11_pt4(a,b,i,j) * wm_interm_57_pt4(b,a,j,i)
term(1010) = term(1010) + t1(p,q) * wm_interm_11_pt4(a,b,i,j) * wm_interm_58_pt4(b,a,j,i)
term(1011) = term(1011) + t1(p,q) * wm_interm_12_pt4(a,b,i,j) * wm_interm_57_pt4(b,a,j,i)
term(1012) = term(1012) + t1(p,q) * wm_interm_12_pt4(a,b,i,j) * wm_interm_58_pt4(b,a,j,i)
term(1013) = term(1013) + t1(p,q) * wm_interm_1_pt4(a,b,i,j) * wm_interm_57_pt4(b,a,j,i)
term(1014) = term(1014) + t1(p,q) * wm_interm_1_pt4(a,b,i,j) * wm_interm_58_pt4(b,a,j,i)
term(1015) = term(1015) + t1(p,q) * wm_interm_58_pt4(a,b,i,j) * wm_interm_5_pt4(b,a,j,i)
term(1016) = term(1016) + t1(p,q) * wm_interm_57_pt4(a,b,i,j) * wm_interm_5_pt4(b,a,j,i)
term(1017) = term(1017) + t1(p,q) * wm_interm_5_pt4(a,b,i,j) * wm_interm_64_pt4(b,a,j,i)
term(1018) = term(1018) + t1(p,q) * wm_interm_5_pt4(a,b,i,j) * wm_interm_65_pt4(b,a,j,i)
term(1019) = term(1019) + t1(p,q) * wm_interm_1_pt4(a,b,i,j) * wm_interm_64_pt4(b,a,j,i)
term(1020) = term(1020) + t1(p,q) * wm_interm_1_pt4(a,b,i,j) * wm_interm_65_pt4(b,a,j,i)
term(1021) = term(1021) + t1(a,i) * wm_interm_57_pt4(b,p,i,j) * wm_interm_5_pt4(a,b,j,q)
term(1022) = term(1022) + t1(a,i) * wm_interm_58_pt4(b,p,i,j) * wm_interm_5_pt4(a,b,j,q)
term(1023) = term(1023) + t1(a,i) * wm_interm_1_pt4(a,b,j,q) * wm_interm_58_pt4(b,p,i,j)
term(1024) = term(1024) + t1(a,i) * wm_interm_1_pt4(a,b,j,q) * wm_interm_57_pt4(b,p,i,j)
term(1025) = term(1025) + t1(a,i) * wm_interm_11_pt4(a,b,j,q) * wm_interm_58_pt4(b,p,i,j)
term(1026) = term(1026) + t1(a,i) * wm_interm_11_pt4(a,b,j,q) * wm_interm_57_pt4(b,p,i,j)
term(1027) = term(1027) + t1(a,i) * wm_interm_12_pt4(a,b,j,q) * wm_interm_58_pt4(b,p,i,j)
term(1028) = term(1028) + t1(a,i) * wm_interm_12_pt4(a,b,j,q) * wm_interm_57_pt4(b,p,i,j)
term(1029) = term(1029) + t1(a,i) * wm_interm_1_pt4(a,b,j,q) * wm_interm_64_pt4(b,p,i,j)
term(1030) = term(1030) + t1(a,i) * wm_interm_5_pt4(a,b,j,q) * wm_interm_64_pt4(b,p,i,j)
term(1031) = term(1031) + t1(a,i) * wm_interm_11_pt4(a,b,j,q) * wm_interm_64_pt4(b,p,i,j)
term(1032) = term(1032) + t1(a,i) * wm_interm_12_pt4(a,b,j,q) * wm_interm_64_pt4(b,p,i,j)
term(1033) = term(1033) + t1(a,i) * wm_interm_1_pt4(a,b,j,q) * wm_interm_65_pt4(b,p,i,j)
term(1034) = term(1034) + t1(a,i) * wm_interm_5_pt4(a,b,j,q) * wm_interm_65_pt4(b,p,i,j)
term(1035) = term(1035) + t1(a,i) * wm_interm_11_pt4(a,b,j,q) * wm_interm_65_pt4(b,p,i,j)
term(1036) = term(1036) + t1(a,i) * wm_interm_12_pt4(a,b,j,q) * wm_interm_65_pt4(b,p,i,j)
term(1037) = term(1037) + t1(a,q) * wm_interm_11_pt4(b,p,i,j) * wm_interm_57_pt4(a,b,j,i)
term(1038) = term(1038) + t1(a,q) * wm_interm_11_pt4(b,p,i,j) * wm_interm_58_pt4(a,b,j,i)
term(1039) = term(1039) + t1(a,q) * wm_interm_11_pt4(b,p,i,j) * wm_interm_64_pt4(a,b,j,i)
term(1040) = term(1040) + t1(a,q) * wm_interm_11_pt4(b,p,i,j) * wm_interm_65_pt4(a,b,j,i)
term(1041) = term(1041) + t1(a,q) * wm_interm_5_pt4(b,p,i,j) * wm_interm_64_pt4(a,b,j,i)
term(1042) = term(1042) + t1(a,q) * wm_interm_5_pt4(b,p,i,j) * wm_interm_65_pt4(a,b,j,i)
term(1043) = term(1043) + t1(a,q) * wm_interm_58_pt4(a,b,i,j) * wm_interm_5_pt4(b,p,j,i)
term(1044) = term(1044) + t1(a,q) * wm_interm_57_pt4(a,b,i,j) * wm_interm_5_pt4(b,p,j,i)
term(1045) = term(1045) + t1(a,q) * wm_interm_1_pt4(b,p,i,j) * wm_interm_64_pt4(a,b,j,i)
term(1046) = term(1046) + t1(a,q) * wm_interm_1_pt4(b,p,i,j) * wm_interm_65_pt4(a,b,j,i)
term(1047) = term(1047) + t1(a,q) * wm_interm_1_pt4(b,p,i,j) * wm_interm_57_pt4(a,b,j,i)
term(1048) = term(1048) + t1(a,q) * wm_interm_1_pt4(b,p,i,j) * wm_interm_58_pt4(a,b,j,i)
term(1049) = term(1049) + t1(a,q) * wm_interm_12_pt4(b,p,i,j) * wm_interm_57_pt4(a,b,j,i)
term(1050) = term(1050) + t1(a,q) * wm_interm_12_pt4(b,p,i,j) * wm_interm_58_pt4(a,b,j,i)
term(1051) = term(1051) + t1(a,q) * wm_interm_12_pt4(b,p,i,j) * wm_interm_64_pt4(a,b,j,i)
term(1052) = term(1052) + t1(a,q) * wm_interm_12_pt4(b,p,i,j) * wm_interm_65_pt4(a,b,j,i)
term(1053) = term(1053) + t1(p,i) * wm_interm_11_pt4(a,b,j,q) * wm_interm_57_pt4(b,a,i,j)
term(1054) = term(1054) + t1(p,i) * wm_interm_11_pt4(a,b,j,q) * wm_interm_58_pt4(b,a,i,j)
term(1055) = term(1055) + t1(p,i) * wm_interm_11_pt4(a,b,j,q) * wm_interm_64_pt4(b,a,i,j)
term(1056) = term(1056) + t1(p,i) * wm_interm_11_pt4(a,b,j,q) * wm_interm_65_pt4(b,a,i,j)
term(1057) = term(1057) + t1(p,i) * wm_interm_12_pt4(a,b,j,q) * wm_interm_57_pt4(b,a,i,j)
term(1058) = term(1058) + t1(p,i) * wm_interm_12_pt4(a,b,j,q) * wm_interm_58_pt4(b,a,i,j)
term(1059) = term(1059) + t1(p,i) * wm_interm_12_pt4(a,b,j,q) * wm_interm_64_pt4(b,a,i,j)
term(1060) = term(1060) + t1(p,i) * wm_interm_12_pt4(a,b,j,q) * wm_interm_65_pt4(b,a,i,j)
term(1061) = term(1061) + t1(p,i) * wm_interm_1_pt4(a,b,j,q) * wm_interm_57_pt4(b,a,i,j)
term(1062) = term(1062) + t1(p,i) * wm_interm_1_pt4(a,b,j,q) * wm_interm_58_pt4(b,a,i,j)
term(1063) = term(1063) + t1(p,i) * wm_interm_1_pt4(a,b,j,q) * wm_interm_64_pt4(b,a,i,j)
term(1064) = term(1064) + t1(p,i) * wm_interm_1_pt4(a,b,j,q) * wm_interm_65_pt4(b,a,i,j)
term(1065) = term(1065) + t1(p,i) * wm_interm_5_pt4(a,b,j,q) * wm_interm_64_pt4(b,a,i,j)
term(1066) = term(1066) + t1(p,i) * wm_interm_5_pt4(a,b,j,q) * wm_interm_65_pt4(b,a,i,j)
term(1067) = term(1067) + t1(p,i) * wm_interm_58_pt4(a,b,i,j) * wm_interm_5_pt4(b,a,j,q)
term(1068) = term(1068) + t1(p,i) * wm_interm_57_pt4(a,b,i,j) * wm_interm_5_pt4(b,a,j,q)
end do 
end do 
end do 
end do 

term(861) = term(861) * (-16.0d+0) 
term(862) = term(862) * (8.0d+0) 
term(863) = term(863) * (8.0d+0) 
term(864) = term(864) * (-4.0d+0) 
term(865) = term(865) * (-4.0d+0) 
term(866) = term(866) * (8.0d+0) 
term(867) = term(867) * (2.0d+0) 
term(868) = term(868) * (-4.0d+0) 
term(869) = term(869) * (4.0d+0) 
term(870) = term(870) * (-8.0d+0) 
term(871) = term(871) * (4.0d+0) 
term(872) = term(872) * (-8.0d+0) 
term(873) = term(873) * (4.0d+0) 
term(874) = term(874) * (-8.0d+0) 
term(875) = term(875) * (4.0d+0) 
term(876) = term(876) * (-8.0d+0) 
term(877) = term(877) * (-8.0d+0) 
term(878) = term(878) * (16.0d+0) 
term(879) = term(879) * (4.0d+0) 
term(880) = term(880) * (-8.0d+0) 
term(881) = term(881) * (4.0d+0) 
term(882) = term(882) * (-8.0d+0) 
term(883) = term(883) * (-8.0d+0) 
term(884) = term(884) * (16.0d+0) 
term(885) = term(885) * (4.0d+0) 
term(886) = term(886) * (-8.0d+0) 
term(887) = term(887) * (-8.0d+0) 
term(888) = term(888) * (16.0d+0) 
term(889) = term(889) * (4.0d+0) 
term(890) = term(890) * (-8.0d+0) 
term(891) = term(891) * (-8.0d+0) 
term(892) = term(892) * (16.0d+0) 
term(893) = term(893) * (-8.0d+0) 
term(894) = term(894) * (16.0d+0) 
term(895) = term(895) * (16.0d+0) 
term(896) = term(896) * (-32.0d+0) 
term(897) = term(897) * (4.0d+0) 
term(898) = term(898) * (-8.0d+0) 
term(899) = term(899) * (-8.0d+0) 
term(900) = term(900) * (16.0d+0) 
term(901) = term(901) * (-16.0d+0) 
term(902) = term(902) * (32.0d+0) 
term(903) = term(903) * (32.0d+0) 
term(904) = term(904) * (-64.0d+0) 
term(905) = term(905) * (8.0d+0) 
term(906) = term(906) * (-16.0d+0) 
term(907) = term(907) * (-16.0d+0) 
term(908) = term(908) * (32.0d+0) 
term(909) = term(909) * (4.0d+0) 
term(910) = term(910) * (-8.0d+0) 
term(911) = term(911) * (-8.0d+0) 
term(912) = term(912) * (16.0d+0) 
term(913) = term(913) * (4.0d+0) 
term(914) = term(914) * (-8.0d+0) 
term(915) = term(915) * (-8.0d+0) 
term(916) = term(916) * (16.0d+0) 
term(917) = term(917) * (-8.0d+0) 
term(918) = term(918) * (16.0d+0) 
term(919) = term(919) * (16.0d+0) 
term(920) = term(920) * (-32.0d+0) 
term(921) = term(921) * (-2.0d+0) 
term(922) = term(922) * (4.0d+0) 
term(923) = term(923) * (4.0d+0) 
term(924) = term(924) * (-8.0d+0) 
term(925) = term(925) * (-2.0d+0) 
term(926) = term(926) * (4.0d+0) 
term(927) = term(927) * (4.0d+0) 
term(928) = term(928) * (-8.0d+0) 
term(929) = term(929) * (4.0d+0) 
term(930) = term(930) * (-8.0d+0) 
term(931) = term(931) * (-8.0d+0) 
term(932) = term(932) * (16.0d+0) 
term(933) = term(933) * (-2.0d+0) 
term(934) = term(934) * (4.0d+0) 
term(935) = term(935) * (-2.0d+0) 
term(936) = term(936) * (4.0d+0) 
term(937) = term(937) * (4.0d+0) 
term(938) = term(938) * (-8.0d+0) 
term(939) = term(939) * (-2.0d+0) 
term(940) = term(940) * (4.0d+0) 
term(941) = term(941) * (-2.0d+0) 
term(942) = term(942) * (4.0d+0) 
term(943) = term(943) * (4.0d+0) 
term(944) = term(944) * (-8.0d+0) 
term(945) = term(945) * (-2.0d+0) 
term(946) = term(946) * (4.0d+0) 
term(947) = term(947) * (4.0d+0) 
term(948) = term(948) * (-8.0d+0) 
term(949) = term(949) * (-2.0d+0) 
term(950) = term(950) * (4.0d+0) 
term(951) = term(951) * (4.0d+0) 
term(952) = term(952) * (-8.0d+0) 
term(953) = term(953) * (4.0d+0) 
term(954) = term(954) * (-8.0d+0) 
term(955) = term(955) * (-8.0d+0) 
term(956) = term(956) * (16.0d+0) 
term(957) = term(957) * (4.0d+0) 
term(958) = term(958) * (-8.0d+0) 
term(959) = term(959) * (-8.0d+0) 
term(960) = term(960) * (16.0d+0) 
term(961) = term(961) * (-2.0d+0) 
term(962) = term(962) * (4.0d+0) 
term(963) = term(963) * (4.0d+0) 
term(964) = term(964) * (-8.0d+0) 
term(965) = term(965) * (-2.0d+0) 
term(966) = term(966) * (4.0d+0) 
term(967) = term(967) * (-2.0d+0) 
term(968) = term(968) * (4.0d+0) 
term(969) = term(969) * (-2.0d+0) 
term(970) = term(970) * (4.0d+0) 
term(971) = term(971) * (4.0d+0) 
term(972) = term(972) * (-8.0d+0) 
term(973) = term(973) * (4.0d+0) 
term(974) = term(974) * (-8.0d+0) 
term(975) = term(975) * (-8.0d+0) 
term(976) = term(976) * (16.0d+0) 
term(977) = term(977) * (4.0d+0) 
term(978) = term(978) * (-8.0d+0) 
term(979) = term(979) * (-8.0d+0) 
term(980) = term(980) * (16.0d+0) 
term(981) = term(981) * (-2.0d+0) 
term(982) = term(982) * (4.0d+0) 
term(983) = term(983) * (4.0d+0) 
term(984) = term(984) * (-8.0d+0) 
term(985) = term(985) * (-2.0d+0) 
term(986) = term(986) * (4.0d+0) 
term(987) = term(987) * (4.0d+0) 
term(988) = term(988) * (-8.0d+0) 
term(989) = term(989) * (-2.0d+0) 
term(990) = term(990) * (4.0d+0) 
term(991) = term(991) * (-2.0d+0) 
term(992) = term(992) * (4.0d+0) 
term(993) = term(993) * (-2.0d+0) 
term(994) = term(994) * (4.0d+0) 
term(995) = term(995) * (-2.0d+0) 
term(996) = term(996) * (4.0d+0) 
term(997) = term(997) * (-2.0d+0) 
term(998) = term(998) * (4.0d+0) 
term(999) = term(999) * (4.0d+0) 
term(1000) = term(1000) * (-8.0d+0) 
term(1001) = term(1001) * (-2.0d+0) 
term(1002) = term(1002) * (4.0d+0) 
term(1003) = term(1003) * (4.0d+0) 
term(1004) = term(1004) * (-8.0d+0) 
term(1005) = term(1005) * (-8.0d+0) 
term(1006) = term(1006) * (16.0d+0) 
term(1007) = term(1007) * (16.0d+0) 
term(1008) = term(1008) * (-32.0d+0) 
term(1009) = term(1009) * (4.0d+0) 
term(1010) = term(1010) * (-8.0d+0) 
term(1011) = term(1011) * (-8.0d+0) 
term(1012) = term(1012) * (16.0d+0) 
term(1013) = term(1013) * (4.0d+0) 
term(1014) = term(1014) * (-8.0d+0) 
term(1015) = term(1015) * (4.0d+0) 
term(1016) = term(1016) * (-8.0d+0) 
term(1017) = term(1017) * (4.0d+0) 
term(1018) = term(1018) * (-8.0d+0) 
term(1019) = term(1019) * (-8.0d+0) 
term(1020) = term(1020) * (16.0d+0) 
term(1021) = term(1021) * (-2.0d+0) 
term(1022) = term(1022) * (4.0d+0) 
term(1023) = term(1023) * (-2.0d+0) 
term(1024) = term(1024) * (4.0d+0) 
term(1025) = term(1025) * (-2.0d+0) 
term(1026) = term(1026) * (4.0d+0) 
term(1027) = term(1027) * (4.0d+0) 
term(1028) = term(1028) * (-8.0d+0) 
term(1029) = term(1029) * (-2.0d+0) 
term(1030) = term(1030) * (4.0d+0) 
term(1031) = term(1031) * (-2.0d+0) 
term(1032) = term(1032) * (4.0d+0) 
term(1033) = term(1033) * (4.0d+0) 
term(1034) = term(1034) * (-8.0d+0) 
term(1035) = term(1035) * (4.0d+0) 
term(1036) = term(1036) * (-8.0d+0) 
term(1037) = term(1037) * (-4.0d+0) 
term(1038) = term(1038) * (8.0d+0) 
term(1039) = term(1039) * (8.0d+0) 
term(1040) = term(1040) * (-16.0d+0) 
term(1041) = term(1041) * (-4.0d+0) 
term(1042) = term(1042) * (8.0d+0) 
term(1043) = term(1043) * (-4.0d+0) 
term(1044) = term(1044) * (8.0d+0) 
term(1045) = term(1045) * (8.0d+0) 
term(1046) = term(1046) * (-16.0d+0) 
term(1047) = term(1047) * (-4.0d+0) 
term(1048) = term(1048) * (8.0d+0) 
term(1049) = term(1049) * (8.0d+0) 
term(1050) = term(1050) * (-16.0d+0) 
term(1051) = term(1051) * (-16.0d+0) 
term(1052) = term(1052) * (32.0d+0) 
term(1053) = term(1053) * (-4.0d+0) 
term(1054) = term(1054) * (8.0d+0) 
term(1055) = term(1055) * (8.0d+0) 
term(1056) = term(1056) * (-16.0d+0) 
term(1057) = term(1057) * (8.0d+0) 
term(1058) = term(1058) * (-16.0d+0) 
term(1059) = term(1059) * (-16.0d+0) 
term(1060) = term(1060) * (32.0d+0) 
term(1061) = term(1061) * (-4.0d+0) 
term(1062) = term(1062) * (8.0d+0) 
term(1063) = term(1063) * (8.0d+0) 
term(1064) = term(1064) * (-16.0d+0) 
term(1065) = term(1065) * (-4.0d+0) 
term(1066) = term(1066) * (8.0d+0) 
term(1067) = term(1067) * (-4.0d+0) 
term(1068) = term(1068) * (8.0d+0) 

do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(1069) = term(1069) + s2(a,b,i,j) * wm_interm_31_pt4(a,i) * wm_interm_48_pt4(p,b,q,j)
term(1070) = term(1070) + s2(a,b,i,j) * wm_interm_34_pt4(a,i) * wm_interm_48_pt4(p,b,q,j)
term(1071) = term(1071) + s2(a,b,i,j) * wm_interm_31_pt4(a,i) * wm_interm_49_pt4(p,b,q,j)
term(1072) = term(1072) + s2(a,b,i,j) * wm_interm_34_pt4(a,i) * wm_interm_49_pt4(p,b,q,j)
term(1073) = term(1073) + s2(a,b,i,j) * wm_interm_31_pt4(a,i) * wm_interm_50_pt4(p,b,q,j)
term(1074) = term(1074) + s2(a,b,i,j) * wm_interm_31_pt4(a,i) * wm_interm_53_pt4(p,b,q,j)
term(1075) = term(1075) + s2(a,b,i,j) * wm_interm_34_pt4(a,i) * wm_interm_50_pt4(p,b,q,j)
term(1076) = term(1076) + s2(a,b,i,j) * wm_interm_34_pt4(a,i) * wm_interm_53_pt4(p,b,q,j)
end do 
end do 
end do 
end do 

term(1069) = term(1069) * (32.0d+0) 
term(1070) = term(1070) * (-16.0d+0) 
term(1071) = term(1071) * (-16.0d+0) 
term(1072) = term(1072) * (8.0d+0) 
term(1073) = term(1073) * (8.0d+0) 
term(1074) = term(1074) * (-16.0d+0) 
term(1075) = term(1075) * (-4.0d+0) 
term(1076) = term(1076) * (8.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
do j = 1, nocc 
do b = nocc + 1, nactive 
term(1077) = term(1077) + s2(a,p,j,i) * wm_interm_31_pt4(b,j) * wm_interm_48_pt4(b,a,q,i)
term(1078) = term(1078) + s2(a,p,j,i) * wm_interm_34_pt4(b,j) * wm_interm_48_pt4(b,a,q,i)
term(1079) = term(1079) + s2(a,p,j,i) * wm_interm_31_pt4(b,j) * wm_interm_49_pt4(b,a,q,i)
term(1080) = term(1080) + s2(a,p,j,i) * wm_interm_34_pt4(b,j) * wm_interm_49_pt4(b,a,q,i)
term(1081) = term(1081) + s2(a,p,j,i) * wm_interm_31_pt4(b,j) * wm_interm_50_pt4(b,a,q,i)
term(1082) = term(1082) + s2(a,p,j,i) * wm_interm_31_pt4(b,j) * wm_interm_53_pt4(b,a,q,i)
term(1083) = term(1083) + s2(a,p,j,i) * wm_interm_34_pt4(b,j) * wm_interm_50_pt4(b,a,q,i)
term(1084) = term(1084) + s2(a,p,j,i) * wm_interm_34_pt4(b,j) * wm_interm_53_pt4(b,a,q,i)
term(1085) = term(1085) + s2(a,p,j,i) * wm_interm_58_pt4(b,a,q,i) * wm_interm_8_pt4(b,j)
term(1086) = term(1086) + s2(a,p,j,i) * wm_interm_57_pt4(b,a,q,i) * wm_interm_8_pt4(b,j)
term(1087) = term(1087) + s2(a,p,j,i) * wm_interm_64_pt4(b,a,q,i) * wm_interm_8_pt4(b,j)
term(1088) = term(1088) + s2(a,p,j,i) * wm_interm_65_pt4(b,a,q,i) * wm_interm_8_pt4(b,j)
term(1089) = term(1089) + s2(a,p,j,i) * wm_interm_58_pt4(b,a,q,i) * wm_interm_7_pt4(b,j)
term(1090) = term(1090) + s2(a,p,j,i) * wm_interm_57_pt4(b,a,q,i) * wm_interm_7_pt4(b,j)
term(1091) = term(1091) + s2(a,p,j,i) * wm_interm_64_pt4(b,a,q,i) * wm_interm_7_pt4(b,j)
term(1092) = term(1092) + s2(a,p,j,i) * wm_interm_65_pt4(b,a,q,i) * wm_interm_7_pt4(b,j)
term(1093) = term(1093) + s2(a,p,j,i) * wm_interm_28_pt4(b,j,i,q) * wm_interm_51_pt4(b,a)
term(1094) = term(1094) + s2(a,p,j,i) * wm_interm_28_pt4(b,j,i,q) * wm_interm_52_pt4(b,a)
term(1095) = term(1095) + t2(a,p,j,i) * wm_interm_43_pt4(b,j,i,q) * wm_interm_68_pt4(b,a)
term(1096) = term(1096) + t2(a,p,j,i) * wm_interm_43_pt4(b,j,i,q) * wm_interm_69_pt4(b,a)
term(1097) = term(1097) + t2(a,p,j,i) * wm_interm_42_pt4(b,j,i,q) * wm_interm_68_pt4(b,a)
term(1098) = term(1098) + t2(a,p,j,i) * wm_interm_42_pt4(b,j,i,q) * wm_interm_69_pt4(b,a)
end do 
end do 
end do 
end do 

term(1077) = term(1077) * (8.0d+0) 
term(1078) = term(1078) * (-4.0d+0) 
term(1079) = term(1079) * (-4.0d+0) 
term(1080) = term(1080) * (2.0d+0) 
term(1081) = term(1081) * (8.0d+0) 
term(1082) = term(1082) * (-4.0d+0) 
term(1083) = term(1083) * (-4.0d+0) 
term(1084) = term(1084) * (2.0d+0) 
term(1085) = term(1085) * (4.0d+0) 
term(1086) = term(1086) * (-8.0d+0) 
term(1087) = term(1087) * (4.0d+0) 
term(1088) = term(1088) * (-8.0d+0) 
term(1089) = term(1089) * (-8.0d+0) 
term(1090) = term(1090) * (16.0d+0) 
term(1091) = term(1091) * (-8.0d+0) 
term(1092) = term(1092) * (16.0d+0) 
term(1093) = term(1093) * (-8.0d+0) 
term(1094) = term(1094) * (4.0d+0) 
term(1095) = term(1095) * (-2.0d+0) 
term(1096) = term(1096) * (4.0d+0) 
term(1097) = term(1097) * (4.0d+0) 
term(1098) = term(1098) * (-8.0d+0) 

do l = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
term(1099) = term(1099) + s1(p,i) * wm_interm_55_pt4(j,k,l,i) * wm_interm_72_pt4(j,k,q,l)
term(1100) = term(1100) + s1(p,i) * wm_interm_55_pt4(j,k,i,l) * wm_interm_72_pt4(j,k,l,q)
term(1101) = term(1101) + s1(p,i) * wm_interm_55_pt4(j,k,i,l) * wm_interm_72_pt4(j,k,q,l)
term(1102) = term(1102) + s1(p,i) * wm_interm_10_pt4(j,q,k,l) * wm_interm_63_pt4(k,l,j,i)
term(1103) = term(1103) + s1(p,i) * wm_interm_10_pt4(q,j,k,l) * wm_interm_63_pt4(k,l,j,i)
term(1104) = term(1104) + s1(p,q) * wm_interm_10_pt4(i,j,k,l) * wm_interm_63_pt4(k,l,j,i)
term(1105) = term(1105) + t1(p,q) * wm_interm_10_pt4(i,j,k,l) * wm_interm_63_pt4(k,l,j,i)
term(1106) = term(1106) + t1(p,i) * wm_interm_55_pt4(j,k,l,q) * wm_interm_72_pt4(j,k,i,l)
term(1107) = term(1107) + t1(p,i) * wm_interm_55_pt4(j,k,q,l) * wm_interm_72_pt4(j,k,l,i)
term(1108) = term(1108) + t1(p,i) * wm_interm_55_pt4(j,k,q,l) * wm_interm_72_pt4(j,k,i,l)
end do 
end do 
end do 
end do 

term(1099) = term(1099) * (2.0d+0) 
term(1100) = term(1100) * (2.0d+0) 
term(1101) = term(1101) * (-4.0d+0) 
term(1102) = term(1102) * (2.0d+0) 
term(1103) = term(1103) * (-1.0d+0) 
term(1104) = term(1104) * (2.0d+0) 
term(1105) = term(1105) * (2.0d+0) 
term(1106) = term(1106) * (-2.0d+0) 
term(1107) = term(1107) * (-2.0d+0) 
term(1108) = term(1108) * (4.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(1109) = term(1109) + s2(a,p,q,i) * wm_interm_31_pt4(b,j) * wm_interm_48_pt4(b,a,j,i)
term(1110) = term(1110) + s2(a,p,i,q) * wm_interm_31_pt4(b,j) * wm_interm_48_pt4(b,a,j,i)
term(1111) = term(1111) + s2(a,p,q,i) * wm_interm_34_pt4(b,j) * wm_interm_48_pt4(b,a,j,i)
term(1112) = term(1112) + s2(a,p,i,q) * wm_interm_34_pt4(b,j) * wm_interm_48_pt4(b,a,j,i)
term(1113) = term(1113) + s2(a,p,q,i) * wm_interm_31_pt4(b,j) * wm_interm_49_pt4(b,a,j,i)
term(1114) = term(1114) + s2(a,p,i,q) * wm_interm_31_pt4(b,j) * wm_interm_49_pt4(b,a,j,i)
term(1115) = term(1115) + s2(a,p,q,i) * wm_interm_34_pt4(b,j) * wm_interm_49_pt4(b,a,j,i)
term(1116) = term(1116) + s2(a,p,i,q) * wm_interm_34_pt4(b,j) * wm_interm_49_pt4(b,a,j,i)
term(1117) = term(1117) + s2(a,p,i,q) * wm_interm_31_pt4(b,j) * wm_interm_50_pt4(b,a,j,i)
term(1118) = term(1118) + s2(a,p,q,i) * wm_interm_31_pt4(b,j) * wm_interm_50_pt4(b,a,j,i)
term(1119) = term(1119) + s2(a,p,q,i) * wm_interm_31_pt4(b,j) * wm_interm_53_pt4(b,a,j,i)
term(1120) = term(1120) + s2(a,p,i,q) * wm_interm_31_pt4(b,j) * wm_interm_53_pt4(b,a,j,i)
term(1121) = term(1121) + s2(a,p,i,q) * wm_interm_34_pt4(b,j) * wm_interm_50_pt4(b,a,j,i)
term(1122) = term(1122) + s2(a,p,q,i) * wm_interm_34_pt4(b,j) * wm_interm_50_pt4(b,a,j,i)
term(1123) = term(1123) + s2(a,p,q,i) * wm_interm_34_pt4(b,j) * wm_interm_53_pt4(b,a,j,i)
term(1124) = term(1124) + s2(a,p,i,q) * wm_interm_34_pt4(b,j) * wm_interm_53_pt4(b,a,j,i)
term(1125) = term(1125) + s2(a,p,j,i) * wm_interm_73_pt4(b,q,j,i) * wm_interm_74_pt4(b,a)
term(1126) = term(1126) + s2(a,p,j,i) * wm_interm_73_pt4(b,q,j,i) * wm_interm_75_pt4(b,a)
term(1127) = term(1127) + s2(a,p,j,i) * wm_interm_28_pt4(b,i,j,q) * wm_interm_51_pt4(b,a)
term(1128) = term(1128) + s2(a,p,j,i) * wm_interm_28_pt4(b,i,j,q) * wm_interm_52_pt4(b,a)
term(1129) = term(1129) + s2(a,p,q,i) * wm_interm_64_pt4(b,a,j,i) * wm_interm_7_pt4(b,j)
term(1130) = term(1130) + s2(a,p,q,i) * wm_interm_65_pt4(b,a,j,i) * wm_interm_7_pt4(b,j)
term(1131) = term(1131) + s2(a,p,i,q) * wm_interm_64_pt4(b,a,j,i) * wm_interm_7_pt4(b,j)
term(1132) = term(1132) + s2(a,p,i,q) * wm_interm_65_pt4(b,a,j,i) * wm_interm_7_pt4(b,j)
term(1133) = term(1133) + s2(a,p,q,i) * wm_interm_57_pt4(b,a,j,i) * wm_interm_7_pt4(b,j)
term(1134) = term(1134) + s2(a,p,q,i) * wm_interm_58_pt4(b,a,j,i) * wm_interm_7_pt4(b,j)
term(1135) = term(1135) + s2(a,p,i,q) * wm_interm_57_pt4(b,a,j,i) * wm_interm_7_pt4(b,j)
term(1136) = term(1136) + s2(a,p,i,q) * wm_interm_58_pt4(b,a,j,i) * wm_interm_7_pt4(b,j)
term(1137) = term(1137) + s2(a,p,q,i) * wm_interm_57_pt4(b,a,j,i) * wm_interm_8_pt4(b,j)
term(1138) = term(1138) + s2(a,p,q,i) * wm_interm_58_pt4(b,a,j,i) * wm_interm_8_pt4(b,j)
term(1139) = term(1139) + s2(a,p,q,i) * wm_interm_64_pt4(b,a,j,i) * wm_interm_8_pt4(b,j)
term(1140) = term(1140) + s2(a,p,q,i) * wm_interm_65_pt4(b,a,j,i) * wm_interm_8_pt4(b,j)
term(1141) = term(1141) + s2(a,p,i,q) * wm_interm_57_pt4(b,a,j,i) * wm_interm_8_pt4(b,j)
term(1142) = term(1142) + s2(a,p,i,q) * wm_interm_58_pt4(b,a,j,i) * wm_interm_8_pt4(b,j)
term(1143) = term(1143) + s2(a,p,i,q) * wm_interm_64_pt4(b,a,j,i) * wm_interm_8_pt4(b,j)
term(1144) = term(1144) + s2(a,p,i,q) * wm_interm_65_pt4(b,a,j,i) * wm_interm_8_pt4(b,j)
term(1145) = term(1145) + t2(a,p,i,q) * wm_interm_44_pt4(b,j) * wm_interm_62_pt4(b,a,j,i)
term(1146) = term(1146) + t2(a,p,i,q) * wm_interm_44_pt4(b,j) * wm_interm_61_pt4(b,a,j,i)
term(1147) = term(1147) + t2(a,p,i,q) * wm_interm_44_pt4(b,j) * wm_interm_59_pt4(b,a,j,i)
term(1148) = term(1148) + t2(a,p,i,q) * wm_interm_44_pt4(b,j) * wm_interm_60_pt4(b,a,j,i)
term(1149) = term(1149) + t2(a,p,i,q) * wm_interm_45_pt4(b,j) * wm_interm_62_pt4(b,a,j,i)
term(1150) = term(1150) + t2(a,p,i,q) * wm_interm_45_pt4(b,j) * wm_interm_61_pt4(b,a,j,i)
term(1151) = term(1151) + t2(a,p,i,q) * wm_interm_45_pt4(b,j) * wm_interm_59_pt4(b,a,j,i)
term(1152) = term(1152) + t2(a,p,i,q) * wm_interm_45_pt4(b,j) * wm_interm_60_pt4(b,a,j,i)
term(1153) = term(1153) + t2(a,p,q,i) * wm_interm_44_pt4(b,j) * wm_interm_62_pt4(b,a,j,i)
term(1154) = term(1154) + t2(a,p,q,i) * wm_interm_44_pt4(b,j) * wm_interm_61_pt4(b,a,j,i)
term(1155) = term(1155) + t2(a,p,q,i) * wm_interm_44_pt4(b,j) * wm_interm_59_pt4(b,a,j,i)
term(1156) = term(1156) + t2(a,p,q,i) * wm_interm_44_pt4(b,j) * wm_interm_60_pt4(b,a,j,i)
term(1157) = term(1157) + t2(a,p,q,i) * wm_interm_45_pt4(b,j) * wm_interm_62_pt4(b,a,j,i)
term(1158) = term(1158) + t2(a,p,q,i) * wm_interm_45_pt4(b,j) * wm_interm_61_pt4(b,a,j,i)
term(1159) = term(1159) + t2(a,p,q,i) * wm_interm_45_pt4(b,j) * wm_interm_59_pt4(b,a,j,i)
term(1160) = term(1160) + t2(a,p,q,i) * wm_interm_45_pt4(b,j) * wm_interm_60_pt4(b,a,j,i)
term(1161) = term(1161) + t2(a,p,j,i) * wm_interm_43_pt4(b,i,j,q) * wm_interm_68_pt4(b,a)
term(1162) = term(1162) + t2(a,p,j,i) * wm_interm_43_pt4(b,i,j,q) * wm_interm_69_pt4(b,a)
term(1163) = term(1163) + t2(a,p,j,i) * wm_interm_42_pt4(b,i,j,q) * wm_interm_68_pt4(b,a)
term(1164) = term(1164) + t2(a,p,j,i) * wm_interm_42_pt4(b,i,j,q) * wm_interm_69_pt4(b,a)
end do 
end do 
end do 
end do 

term(1109) = term(1109) * (-16.0d+0) 
term(1110) = term(1110) * (32.0d+0) 
term(1111) = term(1111) * (8.0d+0) 
term(1112) = term(1112) * (-16.0d+0) 
term(1113) = term(1113) * (8.0d+0) 
term(1114) = term(1114) * (-16.0d+0) 
term(1115) = term(1115) * (-4.0d+0) 
term(1116) = term(1116) * (8.0d+0) 
term(1117) = term(1117) * (8.0d+0) 
term(1118) = term(1118) * (-4.0d+0) 
term(1119) = term(1119) * (8.0d+0) 
term(1120) = term(1120) * (-16.0d+0) 
term(1121) = term(1121) * (-4.0d+0) 
term(1122) = term(1122) * (2.0d+0) 
term(1123) = term(1123) * (-4.0d+0) 
term(1124) = term(1124) * (8.0d+0) 
term(1125) = term(1125) * (-8.0d+0) 
term(1126) = term(1126) * (16.0d+0) 
term(1127) = term(1127) * (4.0d+0) 
term(1128) = term(1128) * (-2.0d+0) 
term(1129) = term(1129) * (-8.0d+0) 
term(1130) = term(1130) * (16.0d+0) 
term(1131) = term(1131) * (16.0d+0) 
term(1132) = term(1132) * (-32.0d+0) 
term(1133) = term(1133) * (4.0d+0) 
term(1134) = term(1134) * (-8.0d+0) 
term(1135) = term(1135) * (-8.0d+0) 
term(1136) = term(1136) * (16.0d+0) 
term(1137) = term(1137) * (-2.0d+0) 
term(1138) = term(1138) * (4.0d+0) 
term(1139) = term(1139) * (4.0d+0) 
term(1140) = term(1140) * (-8.0d+0) 
term(1141) = term(1141) * (4.0d+0) 
term(1142) = term(1142) * (-8.0d+0) 
term(1143) = term(1143) * (-8.0d+0) 
term(1144) = term(1144) * (16.0d+0) 
term(1145) = term(1145) * (-8.0d+0) 
term(1146) = term(1146) * (16.0d+0) 
term(1147) = term(1147) * (16.0d+0) 
term(1148) = term(1148) * (-32.0d+0) 
term(1149) = term(1149) * (4.0d+0) 
term(1150) = term(1150) * (-8.0d+0) 
term(1151) = term(1151) * (-8.0d+0) 
term(1152) = term(1152) * (16.0d+0) 
term(1153) = term(1153) * (4.0d+0) 
term(1154) = term(1154) * (-8.0d+0) 
term(1155) = term(1155) * (-8.0d+0) 
term(1156) = term(1156) * (16.0d+0) 
term(1157) = term(1157) * (-2.0d+0) 
term(1158) = term(1158) * (4.0d+0) 
term(1159) = term(1159) * (4.0d+0) 
term(1160) = term(1160) * (-8.0d+0) 
term(1161) = term(1161) * (4.0d+0) 
term(1162) = term(1162) * (-8.0d+0) 
term(1163) = term(1163) * (-2.0d+0) 
term(1164) = term(1164) * (4.0d+0) 

do j = 1, nocc 
do l = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
term(1165) = term(1165) + t1(p,i) * wm_interm_10_pt4(j,k,l,q) * wm_interm_63_pt4(l,i,k,j)
term(1166) = term(1166) + t1(p,i) * wm_interm_10_pt4(j,k,l,q) * wm_interm_63_pt4(i,l,k,j)
end do 
end do 
end do 
end do 

term(1165) = term(1165) * (-2.0d+0) 
term(1166) = term(1166) * (4.0d+0) 

do k = 1, nocc 
do l = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(1167) = term(1167) + t1(p,i) * wm_interm_10_pt4(j,k,l,q) * wm_interm_63_pt4(l,i,j,k)
term(1168) = term(1168) + t1(p,i) * wm_interm_10_pt4(j,k,l,q) * wm_interm_63_pt4(i,l,j,k)
end do 
end do 
end do 
end do 

term(1167) = term(1167) * (4.0d+0) 
term(1168) = term(1168) * (-2.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(1169) = term(1169) + r2(vrdav_Rr, a,j,p,i) * wm_interm_48_pt4(a,b,k,q) * wm_interm_76_pt4(b,j,i,k)
term(1170) = term(1170) + r2(vrdav_Rr, a,j,p,i) * wm_interm_23_pt4(b,j,i,k) * wm_interm_53_pt4(a,b,k,q)
term(1171) = term(1171) + r2(vrdav_Rr, a,j,p,i) * wm_interm_23_pt4(b,j,i,k) * wm_interm_50_pt4(a,b,k,q)
term(1172) = term(1172) + r2(vrdav_Rr, a,j,p,i) * wm_interm_49_pt4(a,b,k,q) * wm_interm_76_pt4(b,j,i,k)
term(1173) = term(1173) + r2(vrdav_Rr, a,j,p,i) * wm_interm_42_pt4(b,j,i,k) * wm_interm_5_pt4(a,b,k,q)
term(1174) = term(1174) + r2(vrdav_Rr, a,j,p,i) * wm_interm_1_pt4(a,b,k,q) * wm_interm_42_pt4(b,j,i,k)
term(1175) = term(1175) + r2(vrdav_Rr, a,j,p,i) * wm_interm_11_pt4(a,b,k,q) * wm_interm_42_pt4(b,j,i,k)
term(1176) = term(1176) + r2(vrdav_Rr, a,j,p,i) * wm_interm_12_pt4(a,b,k,q) * wm_interm_42_pt4(b,j,i,k)
term(1177) = term(1177) + r2(vrdav_Rr, a,j,p,i) * wm_interm_50_pt4(a,b,k,q) * wm_interm_76_pt4(b,j,i,k)
term(1178) = term(1178) + r2(vrdav_Rr, a,j,p,i) * wm_interm_53_pt4(a,b,k,q) * wm_interm_76_pt4(b,j,i,k)
term(1179) = term(1179) + r2(vrdav_Rr, a,j,p,i) * wm_interm_11_pt4(a,b,k,q) * wm_interm_43_pt4(b,j,i,k)
term(1180) = term(1180) + r2(vrdav_Rr, a,j,p,i) * wm_interm_43_pt4(b,j,i,k) * wm_interm_5_pt4(a,b,k,q)
term(1181) = term(1181) + r2(vrdav_Rr, a,j,p,i) * wm_interm_1_pt4(a,b,k,q) * wm_interm_43_pt4(b,j,i,k)
term(1182) = term(1182) + r2(vrdav_Rr, a,j,p,i) * wm_interm_12_pt4(a,b,k,q) * wm_interm_43_pt4(b,j,i,k)
end do 
end do 
end do 
end do 
end do 

term(1169) = term(1169) * (-8.0d+0) 
term(1170) = term(1170) * (-1.0d+0) 
term(1171) = term(1171) * (2.0d+0) 
term(1172) = term(1172) * (4.0d+0) 
term(1173) = term(1173) * (2.0d+0) 
term(1174) = term(1174) * (-1.0d+0) 
term(1175) = term(1175) * (-1.0d+0) 
term(1176) = term(1176) * (2.0d+0) 
term(1177) = term(1177) * (-1.0d+0) 
term(1178) = term(1178) * (2.0d+0) 
term(1179) = term(1179) * (2.0d+0) 
term(1180) = term(1180) * (-1.0d+0) 
term(1181) = term(1181) * (2.0d+0) 
term(1182) = term(1182) * (-4.0d+0) 

do l = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
term(1183) = term(1183) + s1(p,i) * wm_interm_10_pt4(j,q,k,l) * wm_interm_63_pt4(k,l,i,j)
term(1184) = term(1184) + s1(p,i) * wm_interm_10_pt4(q,j,k,l) * wm_interm_63_pt4(k,l,i,j)
term(1185) = term(1185) + s1(p,q) * wm_interm_10_pt4(i,j,k,l) * wm_interm_63_pt4(k,l,i,j)
term(1186) = term(1186) + t1(p,q) * wm_interm_10_pt4(i,j,k,l) * wm_interm_63_pt4(k,l,i,j)
end do 
end do 
end do 
end do 

term(1183) = term(1183) * (-1.0d+0) 
term(1184) = term(1184) * (2.0d+0) 
term(1185) = term(1185) * (-4.0d+0) 
term(1186) = term(1186) * (-4.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
term(1187) = term(1187) + s1(p,i) * wm_interm_18_pt4(j,k) * wm_interm_63_pt4(q,k,j,i)
term(1188) = term(1188) + s1(p,i) * wm_interm_19_pt4(j,k) * wm_interm_63_pt4(q,k,j,i)
term(1189) = term(1189) + s1(p,i) * wm_interm_18_pt4(j,k) * wm_interm_63_pt4(k,q,j,i)
term(1190) = term(1190) + s1(p,i) * wm_interm_19_pt4(j,k) * wm_interm_63_pt4(k,q,j,i)
end do 
end do 
end do 

term(1187) = term(1187) * (-1.0d+0) 
term(1188) = term(1188) * (2.0d+0) 
term(1189) = term(1189) * (2.0d+0) 
term(1190) = term(1190) * (-4.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(1191) = term(1191) + s1(p,i) * wm_interm_55_pt4(j,q,k,l) * wm_interm_72_pt4(j,i,l,k)
term(1192) = term(1192) + s1(p,i) * wm_interm_55_pt4(j,q,k,l) * wm_interm_72_pt4(j,i,k,l)
end do 
end do 
end do 
end do 

term(1191) = term(1191) * (-1.0d+0) 
term(1192) = term(1192) * (2.0d+0) 

do l = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(1193) = term(1193) + s2(a,p,i,q) * wm_interm_3_pt4(a,j,k,l) * wm_interm_63_pt4(l,k,i,j)
term(1194) = term(1194) + s2(a,p,i,q) * wm_interm_3_pt4(a,j,k,l) * wm_interm_63_pt4(k,l,i,j)
term(1195) = term(1195) + r2(vrdav_Rr, a,i,p,q) * wm_interm_23_pt4(a,j,k,l) * wm_interm_55_pt4(i,l,k,j)
term(1196) = term(1196) + r2(vrdav_Rr, a,i,p,q) * wm_interm_10_pt4(j,i,k,l) * wm_interm_42_pt4(a,k,l,j)
term(1197) = term(1197) + r2(vrdav_Rr, a,i,p,q) * wm_interm_10_pt4(i,j,k,l) * wm_interm_42_pt4(a,k,l,j)
term(1198) = term(1198) + r2(vrdav_Rr, a,i,p,q) * wm_interm_23_pt4(a,j,k,l) * wm_interm_55_pt4(l,i,k,j)
term(1199) = term(1199) + r2(vrdav_Rr, a,i,p,q) * wm_interm_10_pt4(i,j,k,l) * wm_interm_43_pt4(a,k,l,j)
term(1200) = term(1200) + r2(vrdav_Rr, a,i,p,q) * wm_interm_10_pt4(j,i,k,l) * wm_interm_43_pt4(a,k,l,j)
term(1201) = term(1201) + t2(a,p,i,q) * wm_interm_23_pt4(a,j,k,l) * wm_interm_63_pt4(i,l,k,j)
term(1202) = term(1202) + t2(a,p,i,q) * wm_interm_63_pt4(j,i,k,l) * wm_interm_76_pt4(a,l,k,j)
term(1203) = term(1203) + t2(a,p,i,q) * wm_interm_63_pt4(j,i,k,l) * wm_interm_76_pt4(a,k,l,j)
end do 
end do 
end do 
end do 
end do 

term(1193) = term(1193) * (8.0d+0) 
term(1194) = term(1194) * (-4.0d+0) 
term(1195) = term(1195) * (4.0d+0) 
term(1196) = term(1196) * (4.0d+0) 
term(1197) = term(1197) * (-8.0d+0) 
term(1198) = term(1198) * (-8.0d+0) 
term(1199) = term(1199) * (4.0d+0) 
term(1200) = term(1200) * (-8.0d+0) 
term(1201) = term(1201) * (2.0d+0) 
term(1202) = term(1202) * (2.0d+0) 
term(1203) = term(1203) * (-4.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(1204) = term(1204) + s1(p,i) * wm_interm_55_pt4(q,j,k,l) * wm_interm_72_pt4(j,i,k,l)
term(1205) = term(1205) + s1(p,i) * wm_interm_55_pt4(q,j,k,l) * wm_interm_72_pt4(j,i,l,k)
term(1206) = term(1206) + s1(p,q) * wm_interm_55_pt4(i,j,k,l) * wm_interm_72_pt4(i,j,l,k)
term(1207) = term(1207) + s1(p,q) * wm_interm_55_pt4(i,j,k,l) * wm_interm_72_pt4(i,j,k,l)
term(1208) = term(1208) + t1(p,q) * wm_interm_55_pt4(i,j,k,l) * wm_interm_72_pt4(i,j,l,k)
term(1209) = term(1209) + t1(p,q) * wm_interm_55_pt4(i,j,k,l) * wm_interm_72_pt4(i,j,k,l)
end do 
end do 
end do 
end do 

term(1204) = term(1204) * (-1.0d+0) 
term(1205) = term(1205) * (2.0d+0) 
term(1206) = term(1206) * (2.0d+0) 
term(1207) = term(1207) * (-4.0d+0) 
term(1208) = term(1208) * (2.0d+0) 
term(1209) = term(1209) * (-4.0d+0) 

do i = 1, nocc 
do l = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
term(1210) = term(1210) + s1(p,i) * wm_interm_55_pt4(j,k,l,i) * wm_interm_72_pt4(j,k,l,q)
term(1211) = term(1211) + t1(p,i) * wm_interm_55_pt4(j,k,l,q) * wm_interm_72_pt4(j,k,l,i)
end do 
end do 
end do 
end do 

term(1210) = term(1210) * (-4.0d+0) 
term(1211) = term(1211) * (4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(1212) = term(1212) + r1(vrdav_Rr, p,i) * wm_interm_10_pt4(j,i,k,q) * wm_interm_22_pt4(k,j)
term(1213) = term(1213) + r1(vrdav_Rr, p,i) * wm_interm_10_pt4(i,j,k,q) * wm_interm_22_pt4(k,j)
term(1214) = term(1214) + t1(p,i) * wm_interm_10_pt4(i,j,k,q) * wm_interm_6_pt4(k,j)
term(1215) = term(1215) + t1(p,i) * wm_interm_10_pt4(j,i,k,q) * wm_interm_6_pt4(k,j)
term(1216) = term(1216) + t1(p,i) * wm_interm_10_pt4(i,j,k,q) * wm_interm_66_pt4(k,j)
term(1217) = term(1217) + t1(p,i) * wm_interm_10_pt4(i,j,k,q) * wm_interm_67_pt4(k,j)
term(1218) = term(1218) + t1(p,i) * wm_interm_10_pt4(j,i,k,q) * wm_interm_66_pt4(k,j)
term(1219) = term(1219) + t1(p,i) * wm_interm_10_pt4(j,i,k,q) * wm_interm_67_pt4(k,j)
term(1220) = term(1220) + t1(p,i) * wm_interm_55_pt4(i,j,q,k) * wm_interm_70_pt4(j,k)
term(1221) = term(1221) + t1(p,i) * wm_interm_55_pt4(i,j,k,q) * wm_interm_70_pt4(j,k)
term(1222) = term(1222) + t1(p,i) * wm_interm_55_pt4(i,j,q,k) * wm_interm_71_pt4(j,k)
term(1223) = term(1223) + t1(p,i) * wm_interm_55_pt4(i,j,k,q) * wm_interm_71_pt4(j,k)
end do 
end do 
end do 

term(1212) = term(1212) * (-4.0d+0) 
term(1213) = term(1213) * (2.0d+0) 
term(1214) = term(1214) * (4.0d+0) 
term(1215) = term(1215) * (-8.0d+0) 
term(1216) = term(1216) * (-4.0d+0) 
term(1217) = term(1217) * (8.0d+0) 
term(1218) = term(1218) * (8.0d+0) 
term(1219) = term(1219) * (-16.0d+0) 
term(1220) = term(1220) * (4.0d+0) 
term(1221) = term(1221) * (-2.0d+0) 
term(1222) = term(1222) * (-8.0d+0) 
term(1223) = term(1223) * (4.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(1224) = term(1224) + r2(vrdav_Rr, a,j,p,i) * wm_interm_23_pt4(a,k,l,q) * wm_interm_55_pt4(j,i,l,k)
term(1225) = term(1225) + r2(vrdav_Rr, a,j,p,i) * wm_interm_23_pt4(a,k,l,q) * wm_interm_55_pt4(i,j,l,k)
term(1226) = term(1226) + t2(a,p,j,i) * wm_interm_23_pt4(a,k,l,q) * wm_interm_63_pt4(j,i,l,k)
term(1227) = term(1227) + t2(a,p,j,i) * wm_interm_23_pt4(a,k,l,q) * wm_interm_63_pt4(i,j,l,k)
end do 
end do 
end do 
end do 
end do 

term(1224) = term(1224) * (-1.0d+0) 
term(1225) = term(1225) * (2.0d+0) 
term(1226) = term(1226) * (-2.0d+0) 
term(1227) = term(1227) * (4.0d+0) 

do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(1228) = term(1228) + t1(a,i) * wm_interm_10_pt4(j,i,k,q) * wm_interm_58_pt4(a,p,k,j)
term(1229) = term(1229) + t1(a,i) * wm_interm_10_pt4(j,i,k,q) * wm_interm_57_pt4(a,p,k,j)
term(1230) = term(1230) + t1(a,i) * wm_interm_10_pt4(i,j,k,q) * wm_interm_57_pt4(a,p,k,j)
term(1231) = term(1231) + t1(a,i) * wm_interm_10_pt4(i,j,k,q) * wm_interm_58_pt4(a,p,k,j)
term(1232) = term(1232) + t1(a,i) * wm_interm_10_pt4(i,j,k,q) * wm_interm_64_pt4(a,p,k,j)
term(1233) = term(1233) + t1(a,i) * wm_interm_10_pt4(j,i,k,q) * wm_interm_64_pt4(a,p,k,j)
term(1234) = term(1234) + t1(a,i) * wm_interm_10_pt4(i,j,k,q) * wm_interm_65_pt4(a,p,k,j)
term(1235) = term(1235) + t1(a,i) * wm_interm_10_pt4(j,i,k,q) * wm_interm_65_pt4(a,p,k,j)
end do 
end do 
end do 
end do 

term(1228) = term(1228) * (-2.0d+0) 
term(1229) = term(1229) * (4.0d+0) 
term(1230) = term(1230) * (-2.0d+0) 
term(1231) = term(1231) * (4.0d+0) 
term(1232) = term(1232) * (4.0d+0) 
term(1233) = term(1233) * (-2.0d+0) 
term(1234) = term(1234) * (-8.0d+0) 
term(1235) = term(1235) * (4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(1236) = term(1236) + s2(a,p,j,i) * wm_interm_31_pt4(b,i) * wm_interm_48_pt4(b,a,q,j)
term(1237) = term(1237) + s2(a,p,j,i) * wm_interm_34_pt4(b,i) * wm_interm_48_pt4(b,a,q,j)
term(1238) = term(1238) + s2(a,p,j,i) * wm_interm_31_pt4(b,i) * wm_interm_49_pt4(b,a,q,j)
term(1239) = term(1239) + s2(a,p,j,i) * wm_interm_34_pt4(b,i) * wm_interm_49_pt4(b,a,q,j)
term(1240) = term(1240) + s2(a,p,j,i) * wm_interm_31_pt4(b,i) * wm_interm_50_pt4(b,a,q,j)
term(1241) = term(1241) + s2(a,p,j,i) * wm_interm_31_pt4(b,i) * wm_interm_53_pt4(b,a,q,j)
term(1242) = term(1242) + s2(a,p,j,i) * wm_interm_34_pt4(b,i) * wm_interm_50_pt4(b,a,q,j)
term(1243) = term(1243) + s2(a,p,j,i) * wm_interm_34_pt4(b,i) * wm_interm_53_pt4(b,a,q,j)
term(1244) = term(1244) + s2(a,p,j,i) * wm_interm_57_pt4(b,a,q,j) * wm_interm_8_pt4(b,i)
term(1245) = term(1245) + s2(a,p,j,i) * wm_interm_58_pt4(b,a,q,j) * wm_interm_8_pt4(b,i)
term(1246) = term(1246) + s1(a,i) * wm_interm_48_pt4(b,a,q,j) * wm_interm_59_pt4(b,p,i,j)
term(1247) = term(1247) + s1(a,i) * wm_interm_48_pt4(b,a,q,j) * wm_interm_60_pt4(b,p,i,j)
term(1248) = term(1248) + s1(a,i) * wm_interm_48_pt4(b,a,q,j) * wm_interm_61_pt4(b,p,i,j)
term(1249) = term(1249) + s1(a,i) * wm_interm_48_pt4(b,a,q,j) * wm_interm_62_pt4(b,p,i,j)
term(1250) = term(1250) + s2(a,p,j,i) * wm_interm_64_pt4(b,a,q,j) * wm_interm_8_pt4(b,i)
term(1251) = term(1251) + s2(a,p,j,i) * wm_interm_65_pt4(b,a,q,j) * wm_interm_8_pt4(b,i)
term(1252) = term(1252) + s1(a,q) * wm_interm_48_pt4(b,a,i,j) * wm_interm_59_pt4(b,p,i,j)
term(1253) = term(1253) + s1(a,q) * wm_interm_48_pt4(b,a,i,j) * wm_interm_60_pt4(b,p,i,j)
term(1254) = term(1254) + s2(a,p,j,i) * wm_interm_73_pt4(b,q,i,j) * wm_interm_74_pt4(b,a)
term(1255) = term(1255) + s2(a,p,j,i) * wm_interm_73_pt4(b,q,i,j) * wm_interm_75_pt4(b,a)
term(1256) = term(1256) + s1(a,q) * wm_interm_48_pt4(b,a,i,j) * wm_interm_62_pt4(b,p,i,j)
term(1257) = term(1257) + s1(a,q) * wm_interm_48_pt4(b,a,i,j) * wm_interm_61_pt4(b,p,i,j)
term(1258) = term(1258) + s2(a,p,j,i) * wm_interm_57_pt4(b,a,q,j) * wm_interm_7_pt4(b,i)
term(1259) = term(1259) + s2(a,p,j,i) * wm_interm_58_pt4(b,a,q,j) * wm_interm_7_pt4(b,i)
term(1260) = term(1260) + s2(a,p,j,i) * wm_interm_64_pt4(b,a,q,j) * wm_interm_7_pt4(b,i)
term(1261) = term(1261) + s2(a,p,j,i) * wm_interm_65_pt4(b,a,q,j) * wm_interm_7_pt4(b,i)
term(1262) = term(1262) + s1(a,i) * wm_interm_49_pt4(b,a,q,j) * wm_interm_59_pt4(b,p,i,j)
term(1263) = term(1263) + s1(a,i) * wm_interm_49_pt4(b,a,q,j) * wm_interm_60_pt4(b,p,i,j)
term(1264) = term(1264) + s1(a,i) * wm_interm_49_pt4(b,a,q,j) * wm_interm_61_pt4(b,p,i,j)
term(1265) = term(1265) + s1(a,i) * wm_interm_49_pt4(b,a,q,j) * wm_interm_62_pt4(b,p,i,j)
term(1266) = term(1266) + s1(a,q) * wm_interm_49_pt4(b,a,i,j) * wm_interm_59_pt4(b,p,i,j)
term(1267) = term(1267) + s1(a,q) * wm_interm_49_pt4(b,a,i,j) * wm_interm_60_pt4(b,p,i,j)
term(1268) = term(1268) + s1(a,q) * wm_interm_49_pt4(b,a,i,j) * wm_interm_62_pt4(b,p,i,j)
term(1269) = term(1269) + s1(a,q) * wm_interm_49_pt4(b,a,i,j) * wm_interm_61_pt4(b,p,i,j)
term(1270) = term(1270) + s1(a,q) * wm_interm_50_pt4(b,a,i,j) * wm_interm_59_pt4(b,p,i,j)
term(1271) = term(1271) + s1(a,q) * wm_interm_50_pt4(b,a,i,j) * wm_interm_60_pt4(b,p,i,j)
term(1272) = term(1272) + s1(a,i) * wm_interm_50_pt4(b,a,q,j) * wm_interm_59_pt4(b,p,i,j)
term(1273) = term(1273) + s1(a,i) * wm_interm_50_pt4(b,a,q,j) * wm_interm_60_pt4(b,p,i,j)
term(1274) = term(1274) + s1(a,i) * wm_interm_53_pt4(b,a,q,j) * wm_interm_59_pt4(b,p,i,j)
term(1275) = term(1275) + s1(a,i) * wm_interm_53_pt4(b,a,q,j) * wm_interm_60_pt4(b,p,i,j)
term(1276) = term(1276) + s1(a,q) * wm_interm_53_pt4(b,a,i,j) * wm_interm_59_pt4(b,p,i,j)
term(1277) = term(1277) + s1(a,q) * wm_interm_53_pt4(b,a,i,j) * wm_interm_60_pt4(b,p,i,j)
term(1278) = term(1278) + s1(a,i) * wm_interm_50_pt4(b,a,q,j) * wm_interm_62_pt4(b,p,i,j)
term(1279) = term(1279) + s1(a,i) * wm_interm_50_pt4(b,a,q,j) * wm_interm_61_pt4(b,p,i,j)
term(1280) = term(1280) + s1(a,i) * wm_interm_53_pt4(b,a,q,j) * wm_interm_61_pt4(b,p,i,j)
term(1281) = term(1281) + s1(a,i) * wm_interm_53_pt4(b,a,q,j) * wm_interm_62_pt4(b,p,i,j)
term(1282) = term(1282) + s1(a,q) * wm_interm_50_pt4(b,a,i,j) * wm_interm_61_pt4(b,p,i,j)
term(1283) = term(1283) + s1(a,q) * wm_interm_50_pt4(b,a,i,j) * wm_interm_62_pt4(b,p,i,j)
term(1284) = term(1284) + s1(a,q) * wm_interm_53_pt4(b,a,i,j) * wm_interm_62_pt4(b,p,i,j)
term(1285) = term(1285) + s1(a,q) * wm_interm_53_pt4(b,a,i,j) * wm_interm_61_pt4(b,p,i,j)
term(1286) = term(1286) + t1(a,q) * wm_interm_50_pt4(b,p,i,j) * wm_interm_59_pt4(b,a,i,j)
term(1287) = term(1287) + t1(a,q) * wm_interm_50_pt4(b,p,i,j) * wm_interm_60_pt4(b,a,i,j)
term(1288) = term(1288) + t1(a,q) * wm_interm_53_pt4(b,p,i,j) * wm_interm_59_pt4(b,a,i,j)
term(1289) = term(1289) + t1(a,q) * wm_interm_53_pt4(b,p,i,j) * wm_interm_60_pt4(b,a,i,j)
term(1290) = term(1290) + t1(a,q) * wm_interm_53_pt4(b,p,i,j) * wm_interm_62_pt4(b,a,i,j)
term(1291) = term(1291) + t1(a,q) * wm_interm_53_pt4(b,p,i,j) * wm_interm_61_pt4(b,a,i,j)
term(1292) = term(1292) + t1(a,q) * wm_interm_50_pt4(b,p,i,j) * wm_interm_61_pt4(b,a,i,j)
term(1293) = term(1293) + t1(a,q) * wm_interm_50_pt4(b,p,i,j) * wm_interm_62_pt4(b,a,i,j)
term(1294) = term(1294) + t1(a,q) * wm_interm_49_pt4(b,p,i,j) * wm_interm_62_pt4(b,a,i,j)
term(1295) = term(1295) + t1(a,q) * wm_interm_49_pt4(b,p,i,j) * wm_interm_61_pt4(b,a,i,j)
term(1296) = term(1296) + t1(a,q) * wm_interm_48_pt4(b,p,i,j) * wm_interm_62_pt4(b,a,i,j)
term(1297) = term(1297) + t1(a,q) * wm_interm_48_pt4(b,p,i,j) * wm_interm_61_pt4(b,a,i,j)
term(1298) = term(1298) + t1(a,q) * wm_interm_49_pt4(b,p,i,j) * wm_interm_59_pt4(b,a,i,j)
term(1299) = term(1299) + t1(a,q) * wm_interm_49_pt4(b,p,i,j) * wm_interm_60_pt4(b,a,i,j)
term(1300) = term(1300) + t1(a,q) * wm_interm_48_pt4(b,p,i,j) * wm_interm_59_pt4(b,a,i,j)
term(1301) = term(1301) + t1(a,q) * wm_interm_48_pt4(b,p,i,j) * wm_interm_60_pt4(b,a,i,j)
end do 
end do 
end do 
end do 

term(1236) = term(1236) * (-16.0d+0) 
term(1237) = term(1237) * (8.0d+0) 
term(1238) = term(1238) * (8.0d+0) 
term(1239) = term(1239) * (-4.0d+0) 
term(1240) = term(1240) * (-4.0d+0) 
term(1241) = term(1241) * (8.0d+0) 
term(1242) = term(1242) * (2.0d+0) 
term(1243) = term(1243) * (-4.0d+0) 
term(1244) = term(1244) * (4.0d+0) 
term(1245) = term(1245) * (-8.0d+0) 
term(1246) = term(1246) * (-8.0d+0) 
term(1247) = term(1247) * (16.0d+0) 
term(1248) = term(1248) * (-8.0d+0) 
term(1249) = term(1249) * (16.0d+0) 
term(1250) = term(1250) * (-8.0d+0) 
term(1251) = term(1251) * (16.0d+0) 
term(1252) = term(1252) * (16.0d+0) 
term(1253) = term(1253) * (-32.0d+0) 
term(1254) = term(1254) * (4.0d+0) 
term(1255) = term(1255) * (-8.0d+0) 
term(1256) = term(1256) * (-8.0d+0) 
term(1257) = term(1257) * (16.0d+0) 
term(1258) = term(1258) * (-8.0d+0) 
term(1259) = term(1259) * (16.0d+0) 
term(1260) = term(1260) * (16.0d+0) 
term(1261) = term(1261) * (-32.0d+0) 
term(1262) = term(1262) * (4.0d+0) 
term(1263) = term(1263) * (-8.0d+0) 
term(1264) = term(1264) * (4.0d+0) 
term(1265) = term(1265) * (-8.0d+0) 
term(1266) = term(1266) * (-8.0d+0) 
term(1267) = term(1267) * (16.0d+0) 
term(1268) = term(1268) * (4.0d+0) 
term(1269) = term(1269) * (-8.0d+0) 
term(1270) = term(1270) * (4.0d+0) 
term(1271) = term(1271) * (-8.0d+0) 
term(1272) = term(1272) * (-8.0d+0) 
term(1273) = term(1273) * (16.0d+0) 
term(1274) = term(1274) * (4.0d+0) 
term(1275) = term(1275) * (-8.0d+0) 
term(1276) = term(1276) * (-8.0d+0) 
term(1277) = term(1277) * (16.0d+0) 
term(1278) = term(1278) * (4.0d+0) 
term(1279) = term(1279) * (-8.0d+0) 
term(1280) = term(1280) * (4.0d+0) 
term(1281) = term(1281) * (-8.0d+0) 
term(1282) = term(1282) * (4.0d+0) 
term(1283) = term(1283) * (-8.0d+0) 
term(1284) = term(1284) * (4.0d+0) 
term(1285) = term(1285) * (-8.0d+0) 
term(1286) = term(1286) * (-4.0d+0) 
term(1287) = term(1287) * (8.0d+0) 
term(1288) = term(1288) * (8.0d+0) 
term(1289) = term(1289) * (-16.0d+0) 
term(1290) = term(1290) * (-4.0d+0) 
term(1291) = term(1291) * (8.0d+0) 
term(1292) = term(1292) * (-4.0d+0) 
term(1293) = term(1293) * (8.0d+0) 
term(1294) = term(1294) * (-4.0d+0) 
term(1295) = term(1295) * (8.0d+0) 
term(1296) = term(1296) * (8.0d+0) 
term(1297) = term(1297) * (-16.0d+0) 
term(1298) = term(1298) * (8.0d+0) 
term(1299) = term(1299) * (-16.0d+0) 
term(1300) = term(1300) * (-16.0d+0) 
term(1301) = term(1301) * (32.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(1302) = term(1302) + s2(a,p,j,i) * wm_interm_28_pt4(a,k,l,q) * wm_interm_55_pt4(k,l,i,j)
term(1303) = term(1303) + s2(a,p,j,i) * wm_interm_29_pt4(a,k,l,q) * wm_interm_55_pt4(k,l,i,j)
term(1304) = term(1304) + t2(a,p,j,i) * wm_interm_42_pt4(a,k,l,q) * wm_interm_72_pt4(k,l,i,j)
term(1305) = term(1305) + t2(a,p,j,i) * wm_interm_43_pt4(a,k,l,q) * wm_interm_72_pt4(k,l,i,j)
end do 
end do 
end do 
end do 
end do 

term(1302) = term(1302) * (-1.0d+0) 
term(1303) = term(1303) * (2.0d+0) 
term(1304) = term(1304) * (-2.0d+0) 
term(1305) = term(1305) * (4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
term(1306) = term(1306) + wm_interm_26_pt4(p,i,q,j) * wm_interm_27_pt4(j,i)
term(1307) = term(1307) + wm_interm_26_pt4(p,i,j,q) * wm_interm_27_pt4(j,i)
term(1308) = term(1308) + wm_interm_22_pt4(i,j) * wm_interm_28_pt4(p,i,q,j)
term(1309) = term(1309) + wm_interm_22_pt4(i,j) * wm_interm_29_pt4(p,i,q,j)
term(1310) = term(1310) + wm_interm_42_pt4(p,i,q,j) * wm_interm_9_pt4(i,j)
term(1311) = term(1311) + wm_interm_42_pt4(p,q,i,j) * wm_interm_9_pt4(i,j)
term(1312) = term(1312) + wm_interm_43_pt4(p,q,i,j) * wm_interm_9_pt4(i,j)
term(1313) = term(1313) + wm_interm_43_pt4(p,i,q,j) * wm_interm_9_pt4(i,j)
term(1314) = term(1314) + s1(p,i) * wm_interm_18_pt4(q,j) * wm_interm_6_pt4(j,i)
term(1315) = term(1315) + s1(p,q) * wm_interm_18_pt4(i,j) * wm_interm_6_pt4(j,i)
term(1316) = term(1316) + s1(p,i) * wm_interm_19_pt4(q,j) * wm_interm_6_pt4(j,i)
term(1317) = term(1317) + s1(p,q) * wm_interm_19_pt4(i,j) * wm_interm_6_pt4(j,i)
term(1318) = term(1318) + r1(vrdav_Rr, p,i) * wm_interm_18_pt4(i,j) * wm_interm_22_pt4(j,q)
term(1319) = term(1319) + r1(vrdav_Rr, p,i) * wm_interm_19_pt4(i,j) * wm_interm_22_pt4(j,q)
term(1320) = term(1320) + t1(p,q) * wm_interm_18_pt4(i,j) * wm_interm_6_pt4(j,i)
term(1321) = term(1321) + t1(p,q) * wm_interm_19_pt4(i,j) * wm_interm_6_pt4(j,i)
term(1322) = term(1322) + r1(vrdav_Rr, p,i) * wm_interm_18_pt4(j,q) * wm_interm_22_pt4(i,j)
term(1323) = term(1323) + r1(vrdav_Rr, p,i) * wm_interm_19_pt4(j,q) * wm_interm_22_pt4(i,j)
term(1324) = term(1324) + t1(p,i) * wm_interm_18_pt4(j,q) * wm_interm_6_pt4(i,j)
term(1325) = term(1325) + t1(p,i) * wm_interm_19_pt4(j,q) * wm_interm_6_pt4(i,j)
term(1326) = term(1326) + s1(p,i) * wm_interm_18_pt4(q,j) * wm_interm_66_pt4(j,i)
term(1327) = term(1327) + s1(p,i) * wm_interm_18_pt4(q,j) * wm_interm_67_pt4(j,i)
term(1328) = term(1328) + s1(p,q) * wm_interm_18_pt4(i,j) * wm_interm_66_pt4(j,i)
term(1329) = term(1329) + s1(p,q) * wm_interm_18_pt4(i,j) * wm_interm_67_pt4(j,i)
term(1330) = term(1330) + s1(p,i) * wm_interm_19_pt4(q,j) * wm_interm_66_pt4(j,i)
term(1331) = term(1331) + s1(p,i) * wm_interm_19_pt4(q,j) * wm_interm_67_pt4(j,i)
term(1332) = term(1332) + s1(p,q) * wm_interm_19_pt4(i,j) * wm_interm_66_pt4(j,i)
term(1333) = term(1333) + s1(p,q) * wm_interm_19_pt4(i,j) * wm_interm_67_pt4(j,i)
term(1334) = term(1334) + s1(p,i) * wm_interm_54_pt4(q,j) * wm_interm_70_pt4(i,j)
term(1335) = term(1335) + s1(p,i) * wm_interm_54_pt4(q,j) * wm_interm_71_pt4(i,j)
term(1336) = term(1336) + s1(p,q) * wm_interm_54_pt4(i,j) * wm_interm_70_pt4(i,j)
term(1337) = term(1337) + s1(p,q) * wm_interm_54_pt4(i,j) * wm_interm_71_pt4(i,j)
term(1338) = term(1338) + s1(p,i) * wm_interm_56_pt4(q,j) * wm_interm_70_pt4(i,j)
term(1339) = term(1339) + s1(p,i) * wm_interm_56_pt4(q,j) * wm_interm_71_pt4(i,j)
term(1340) = term(1340) + s1(p,q) * wm_interm_56_pt4(i,j) * wm_interm_70_pt4(i,j)
term(1341) = term(1341) + s1(p,q) * wm_interm_56_pt4(i,j) * wm_interm_71_pt4(i,j)
term(1342) = term(1342) + t1(p,q) * wm_interm_18_pt4(i,j) * wm_interm_66_pt4(j,i)
term(1343) = term(1343) + t1(p,q) * wm_interm_18_pt4(i,j) * wm_interm_67_pt4(j,i)
term(1344) = term(1344) + t1(p,q) * wm_interm_19_pt4(i,j) * wm_interm_66_pt4(j,i)
term(1345) = term(1345) + t1(p,q) * wm_interm_19_pt4(i,j) * wm_interm_67_pt4(j,i)
term(1346) = term(1346) + t1(p,q) * wm_interm_54_pt4(i,j) * wm_interm_70_pt4(i,j)
term(1347) = term(1347) + t1(p,q) * wm_interm_54_pt4(i,j) * wm_interm_71_pt4(i,j)
term(1348) = term(1348) + t1(p,q) * wm_interm_56_pt4(i,j) * wm_interm_70_pt4(i,j)
term(1349) = term(1349) + t1(p,q) * wm_interm_56_pt4(i,j) * wm_interm_71_pt4(i,j)
term(1350) = term(1350) + t1(p,i) * wm_interm_18_pt4(j,q) * wm_interm_66_pt4(i,j)
term(1351) = term(1351) + t1(p,i) * wm_interm_18_pt4(j,q) * wm_interm_67_pt4(i,j)
term(1352) = term(1352) + t1(p,i) * wm_interm_19_pt4(j,q) * wm_interm_66_pt4(i,j)
term(1353) = term(1353) + t1(p,i) * wm_interm_19_pt4(j,q) * wm_interm_67_pt4(i,j)
end do 
end do 

term(1306) = term(1306) * (2.0d+0) 
term(1307) = term(1307) * (-4.0d+0) 
term(1308) = term(1308) * (2.0d+0) 
term(1309) = term(1309) * (-4.0d+0) 
term(1311) = term(1311) * (-2.0d+0) 
term(1313) = term(1313) * (-2.0d+0) 
term(1314) = term(1314) * (2.0d+0) 
term(1315) = term(1315) * (-4.0d+0) 
term(1316) = term(1316) * (-4.0d+0) 
term(1317) = term(1317) * (8.0d+0) 
term(1318) = term(1318) * (2.0d+0) 
term(1319) = term(1319) * (-4.0d+0) 
term(1320) = term(1320) * (-4.0d+0) 
term(1321) = term(1321) * (8.0d+0) 
term(1322) = term(1322) * (2.0d+0) 
term(1323) = term(1323) * (-4.0d+0) 
term(1324) = term(1324) * (4.0d+0) 
term(1325) = term(1325) * (-8.0d+0) 
term(1326) = term(1326) * (-2.0d+0) 
term(1327) = term(1327) * (4.0d+0) 
term(1328) = term(1328) * (4.0d+0) 
term(1329) = term(1329) * (-8.0d+0) 
term(1330) = term(1330) * (4.0d+0) 
term(1331) = term(1331) * (-8.0d+0) 
term(1332) = term(1332) * (-8.0d+0) 
term(1333) = term(1333) * (16.0d+0) 
term(1334) = term(1334) * (-2.0d+0) 
term(1335) = term(1335) * (4.0d+0) 
term(1336) = term(1336) * (4.0d+0) 
term(1337) = term(1337) * (-8.0d+0) 
term(1338) = term(1338) * (4.0d+0) 
term(1339) = term(1339) * (-8.0d+0) 
term(1340) = term(1340) * (-8.0d+0) 
term(1341) = term(1341) * (16.0d+0) 
term(1342) = term(1342) * (4.0d+0) 
term(1343) = term(1343) * (-8.0d+0) 
term(1344) = term(1344) * (-8.0d+0) 
term(1345) = term(1345) * (16.0d+0) 
term(1346) = term(1346) * (4.0d+0) 
term(1347) = term(1347) * (-8.0d+0) 
term(1348) = term(1348) * (-8.0d+0) 
term(1349) = term(1349) * (16.0d+0) 
term(1350) = term(1350) * (-4.0d+0) 
term(1351) = term(1351) * (8.0d+0) 
term(1352) = term(1352) * (8.0d+0) 
term(1353) = term(1353) * (-16.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
do i = 1, nocc 
do b = nocc + 1, nactive 
term(1354) = term(1354) + r2(vrdav_Rl, a,i,p,q) * wm_interm_1_pt4(b,a,j,k) * wm_interm_28_pt4(b,i,j,k)
term(1355) = term(1355) + r2(vrdav_Rl, a,i,p,q) * wm_interm_1_pt4(b,a,j,k) * wm_interm_29_pt4(b,i,j,k)
term(1356) = term(1356) + r2(vrdav_Rl, a,i,p,q) * wm_interm_29_pt4(b,i,j,k) * wm_interm_5_pt4(b,a,j,k)
term(1357) = term(1357) + r2(vrdav_Rl, a,i,p,q) * wm_interm_28_pt4(b,i,j,k) * wm_interm_5_pt4(b,a,j,k)
term(1358) = term(1358) + s2(a,p,i,q) * wm_interm_28_pt4(b,i,j,k) * wm_interm_48_pt4(b,a,j,k)
term(1359) = term(1359) + r2(vrdav_Rl, a,i,p,q) * wm_interm_11_pt4(b,a,j,k) * wm_interm_28_pt4(b,i,j,k)
term(1360) = term(1360) + r2(vrdav_Rl, a,i,p,q) * wm_interm_11_pt4(b,a,j,k) * wm_interm_29_pt4(b,i,j,k)
term(1361) = term(1361) + r2(vrdav_Rl, a,i,p,q) * wm_interm_12_pt4(b,a,j,k) * wm_interm_28_pt4(b,i,j,k)
term(1362) = term(1362) + r2(vrdav_Rl, a,i,p,q) * wm_interm_12_pt4(b,a,j,k) * wm_interm_29_pt4(b,i,j,k)
term(1363) = term(1363) + s2(a,p,i,q) * wm_interm_28_pt4(b,i,j,k) * wm_interm_49_pt4(b,a,j,k)
term(1364) = term(1364) + s2(a,p,i,q) * wm_interm_28_pt4(b,i,j,k) * wm_interm_50_pt4(b,a,j,k)
term(1365) = term(1365) + s2(a,p,i,q) * wm_interm_28_pt4(b,i,j,k) * wm_interm_53_pt4(b,a,j,k)
term(1366) = term(1366) + t2(a,p,i,q) * wm_interm_42_pt4(b,i,j,k) * wm_interm_59_pt4(b,a,j,k)
term(1367) = term(1367) + t2(a,p,i,q) * wm_interm_42_pt4(b,i,j,k) * wm_interm_60_pt4(b,a,j,k)
term(1368) = term(1368) + t2(a,p,i,q) * wm_interm_42_pt4(b,i,j,k) * wm_interm_61_pt4(b,a,j,k)
term(1369) = term(1369) + t2(a,p,i,q) * wm_interm_42_pt4(b,i,j,k) * wm_interm_62_pt4(b,a,j,k)
term(1370) = term(1370) + t2(a,p,i,q) * wm_interm_43_pt4(b,i,j,k) * wm_interm_59_pt4(b,a,j,k)
term(1371) = term(1371) + t2(a,p,i,q) * wm_interm_43_pt4(b,i,j,k) * wm_interm_60_pt4(b,a,j,k)
term(1372) = term(1372) + t2(a,p,i,q) * wm_interm_43_pt4(b,i,j,k) * wm_interm_62_pt4(b,a,j,k)
term(1373) = term(1373) + t2(a,p,i,q) * wm_interm_43_pt4(b,i,j,k) * wm_interm_61_pt4(b,a,j,k)
end do 
end do 
end do 
end do 
end do 

term(1354) = term(1354) * (-8.0d+0) 
term(1355) = term(1355) * (16.0d+0) 
term(1356) = term(1356) * (-8.0d+0) 
term(1357) = term(1357) * (16.0d+0) 
term(1358) = term(1358) * (16.0d+0) 
term(1359) = term(1359) * (-8.0d+0) 
term(1360) = term(1360) * (16.0d+0) 
term(1361) = term(1361) * (16.0d+0) 
term(1362) = term(1362) * (-32.0d+0) 
term(1363) = term(1363) * (-8.0d+0) 
term(1364) = term(1364) * (16.0d+0) 
term(1365) = term(1365) * (-8.0d+0) 
term(1366) = term(1366) * (2.0d+0) 
term(1367) = term(1367) * (-4.0d+0) 
term(1368) = term(1368) * (2.0d+0) 
term(1369) = term(1369) * (-4.0d+0) 
term(1370) = term(1370) * (-4.0d+0) 
term(1371) = term(1371) * (8.0d+0) 
term(1372) = term(1372) * (2.0d+0) 
term(1373) = term(1373) * (-4.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(1374) = term(1374) + s2(a,p,j,i) * wm_interm_31_pt4(a,k) * wm_interm_55_pt4(q,k,j,i)
term(1375) = term(1375) + s2(a,p,j,i) * wm_interm_31_pt4(a,k) * wm_interm_55_pt4(k,q,j,i)
term(1376) = term(1376) + s2(a,p,j,i) * wm_interm_34_pt4(a,k) * wm_interm_55_pt4(q,k,j,i)
term(1377) = term(1377) + s2(a,p,j,i) * wm_interm_34_pt4(a,k) * wm_interm_55_pt4(k,q,j,i)
term(1378) = term(1378) + s2(a,p,j,i) * wm_interm_63_pt4(q,k,j,i) * wm_interm_7_pt4(a,k)
term(1379) = term(1379) + s2(a,p,j,i) * wm_interm_63_pt4(q,k,j,i) * wm_interm_8_pt4(a,k)
term(1380) = term(1380) + s2(a,p,j,i) * wm_interm_63_pt4(k,q,j,i) * wm_interm_7_pt4(a,k)
term(1381) = term(1381) + s2(a,p,j,i) * wm_interm_63_pt4(k,q,j,i) * wm_interm_8_pt4(a,k)
end do 
end do 
end do 
end do 

term(1374) = term(1374) * (-2.0d+0) 
term(1375) = term(1375) * (4.0d+0) 
term(1377) = term(1377) * (-2.0d+0) 
term(1378) = term(1378) * (-4.0d+0) 
term(1379) = term(1379) * (2.0d+0) 
term(1380) = term(1380) * (8.0d+0) 
term(1381) = term(1381) * (-4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(1382) = term(1382) + s2(a,p,j,i) * wm_interm_31_pt4(a,k) * wm_interm_55_pt4(k,q,i,j)
term(1383) = term(1383) + s2(a,p,j,i) * wm_interm_31_pt4(a,k) * wm_interm_55_pt4(q,k,i,j)
term(1384) = term(1384) + s2(a,p,j,i) * wm_interm_34_pt4(a,k) * wm_interm_55_pt4(k,q,i,j)
term(1385) = term(1385) + s2(a,p,j,i) * wm_interm_34_pt4(a,k) * wm_interm_55_pt4(q,k,i,j)
term(1386) = term(1386) + s2(a,p,j,i) * wm_interm_63_pt4(q,k,i,j) * wm_interm_7_pt4(a,k)
term(1387) = term(1387) + s2(a,p,j,i) * wm_interm_63_pt4(q,k,i,j) * wm_interm_8_pt4(a,k)
term(1388) = term(1388) + s2(a,p,j,i) * wm_interm_63_pt4(k,q,i,j) * wm_interm_7_pt4(a,k)
term(1389) = term(1389) + s2(a,p,j,i) * wm_interm_63_pt4(k,q,i,j) * wm_interm_8_pt4(a,k)
end do 
end do 
end do 
end do 

term(1382) = term(1382) * (-2.0d+0) 
term(1383) = term(1383) * (4.0d+0) 
term(1385) = term(1385) * (-2.0d+0) 
term(1386) = term(1386) * (8.0d+0) 
term(1387) = term(1387) * (-4.0d+0) 
term(1388) = term(1388) * (-4.0d+0) 
term(1389) = term(1389) * (2.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(1390) = term(1390) + r2(vrdav_Rr, a,j,p,i) * wm_interm_10_pt4(i,j,k,q) * wm_interm_44_pt4(a,k)
term(1391) = term(1391) + r2(vrdav_Rr, a,j,p,i) * wm_interm_10_pt4(j,i,k,q) * wm_interm_44_pt4(a,k)
term(1392) = term(1392) + r2(vrdav_Rr, a,j,p,i) * wm_interm_10_pt4(i,j,k,q) * wm_interm_45_pt4(a,k)
term(1393) = term(1393) + r2(vrdav_Rr, a,j,p,i) * wm_interm_10_pt4(j,i,k,q) * wm_interm_45_pt4(a,k)
end do 
end do 
end do 
end do 

term(1390) = term(1390) * (4.0d+0) 
term(1391) = term(1391) * (-8.0d+0) 
term(1392) = term(1392) * (-2.0d+0) 
term(1393) = term(1393) * (4.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(1394) = term(1394) + s1(a,i) * wm_interm_5_pt4(p,a,j,k) * wm_interm_63_pt4(q,k,j,i)
term(1395) = term(1395) + s1(a,i) * wm_interm_1_pt4(p,a,j,k) * wm_interm_63_pt4(q,k,j,i)
term(1396) = term(1396) + s1(a,i) * wm_interm_11_pt4(p,a,j,k) * wm_interm_63_pt4(q,k,j,i)
term(1397) = term(1397) + s1(a,i) * wm_interm_12_pt4(p,a,j,k) * wm_interm_63_pt4(q,k,j,i)
term(1398) = term(1398) + s1(a,i) * wm_interm_1_pt4(p,a,j,k) * wm_interm_63_pt4(k,q,j,i)
term(1399) = term(1399) + s1(a,i) * wm_interm_5_pt4(p,a,j,k) * wm_interm_63_pt4(k,q,j,i)
term(1400) = term(1400) + s1(a,i) * wm_interm_11_pt4(p,a,j,k) * wm_interm_63_pt4(k,q,j,i)
term(1401) = term(1401) + s1(a,i) * wm_interm_12_pt4(p,a,j,k) * wm_interm_63_pt4(k,q,j,i)
term(1402) = term(1402) + s1(a,i) * wm_interm_55_pt4(j,q,k,i) * wm_interm_59_pt4(a,p,j,k)
term(1403) = term(1403) + s1(a,i) * wm_interm_55_pt4(j,q,k,i) * wm_interm_60_pt4(a,p,j,k)
term(1404) = term(1404) + s1(a,i) * wm_interm_55_pt4(q,j,k,i) * wm_interm_59_pt4(a,p,j,k)
term(1405) = term(1405) + s1(a,i) * wm_interm_55_pt4(q,j,k,i) * wm_interm_60_pt4(a,p,j,k)
term(1406) = term(1406) + s1(a,i) * wm_interm_55_pt4(q,j,k,i) * wm_interm_62_pt4(a,p,j,k)
term(1407) = term(1407) + s1(a,i) * wm_interm_55_pt4(q,j,k,i) * wm_interm_61_pt4(a,p,j,k)
term(1408) = term(1408) + s1(a,i) * wm_interm_55_pt4(j,q,k,i) * wm_interm_61_pt4(a,p,j,k)
term(1409) = term(1409) + s1(a,i) * wm_interm_55_pt4(j,q,k,i) * wm_interm_62_pt4(a,p,j,k)
end do 
end do 
end do 
end do 

term(1394) = term(1394) * (2.0d+0) 
term(1395) = term(1395) * (-4.0d+0) 
term(1396) = term(1396) * (-4.0d+0) 
term(1397) = term(1397) * (8.0d+0) 
term(1398) = term(1398) * (2.0d+0) 
term(1399) = term(1399) * (-4.0d+0) 
term(1400) = term(1400) * (2.0d+0) 
term(1401) = term(1401) * (-4.0d+0) 
term(1402) = term(1402) * (2.0d+0) 
term(1403) = term(1403) * (-4.0d+0) 
term(1404) = term(1404) * (-4.0d+0) 
term(1405) = term(1405) * (8.0d+0) 
term(1406) = term(1406) * (2.0d+0) 
term(1407) = term(1407) * (-4.0d+0) 
term(1408) = term(1408) * (2.0d+0) 
term(1409) = term(1409) * (-4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(1410) = term(1410) + r2(vrdav_Rr, a,i,p,q) * wm_interm_49_pt4(a,b,j,k) * wm_interm_76_pt4(b,i,k,j)
term(1411) = term(1411) + r2(vrdav_Rr, a,i,p,q) * wm_interm_48_pt4(a,b,j,k) * wm_interm_76_pt4(b,i,k,j)
term(1412) = term(1412) + r2(vrdav_Rr, a,i,p,q) * wm_interm_23_pt4(b,i,j,k) * wm_interm_50_pt4(a,b,k,j)
term(1413) = term(1413) + r2(vrdav_Rr, a,i,p,q) * wm_interm_23_pt4(b,i,j,k) * wm_interm_53_pt4(a,b,k,j)
term(1414) = term(1414) + r2(vrdav_Rr, a,i,p,q) * wm_interm_11_pt4(a,b,j,k) * wm_interm_42_pt4(b,i,k,j)
term(1415) = term(1415) + r2(vrdav_Rr, a,i,p,q) * wm_interm_12_pt4(a,b,j,k) * wm_interm_42_pt4(b,i,k,j)
term(1416) = term(1416) + r2(vrdav_Rr, a,i,p,q) * wm_interm_1_pt4(a,b,j,k) * wm_interm_42_pt4(b,i,k,j)
term(1417) = term(1417) + r2(vrdav_Rr, a,i,p,q) * wm_interm_42_pt4(b,i,j,k) * wm_interm_5_pt4(a,b,k,j)
term(1418) = term(1418) + r2(vrdav_Rr, a,i,p,q) * wm_interm_50_pt4(a,b,j,k) * wm_interm_76_pt4(b,i,k,j)
term(1419) = term(1419) + r2(vrdav_Rr, a,i,p,q) * wm_interm_53_pt4(a,b,j,k) * wm_interm_76_pt4(b,i,k,j)
term(1420) = term(1420) + r2(vrdav_Rr, a,i,p,q) * wm_interm_11_pt4(a,b,j,k) * wm_interm_43_pt4(b,i,k,j)
term(1421) = term(1421) + r2(vrdav_Rr, a,i,p,q) * wm_interm_12_pt4(a,b,j,k) * wm_interm_43_pt4(b,i,k,j)
term(1422) = term(1422) + r2(vrdav_Rr, a,i,p,q) * wm_interm_43_pt4(b,i,j,k) * wm_interm_5_pt4(a,b,k,j)
term(1423) = term(1423) + r2(vrdav_Rr, a,i,p,q) * wm_interm_1_pt4(a,b,j,k) * wm_interm_43_pt4(b,i,k,j)
term(1424) = term(1424) + t2(a,p,i,q) * wm_interm_23_pt4(b,i,j,k) * wm_interm_58_pt4(a,b,k,j)
term(1425) = term(1425) + t2(a,p,i,q) * wm_interm_23_pt4(b,i,j,k) * wm_interm_57_pt4(a,b,k,j)
term(1426) = term(1426) + t2(a,p,i,q) * wm_interm_23_pt4(b,i,j,k) * wm_interm_64_pt4(a,b,k,j)
term(1427) = term(1427) + t2(a,p,i,q) * wm_interm_23_pt4(b,i,j,k) * wm_interm_65_pt4(a,b,k,j)
end do 
end do 
end do 
end do 
end do 

term(1410) = term(1410) * (-16.0d+0) 
term(1411) = term(1411) * (32.0d+0) 
term(1412) = term(1412) * (-8.0d+0) 
term(1413) = term(1413) * (4.0d+0) 
term(1414) = term(1414) * (4.0d+0) 
term(1415) = term(1415) * (-8.0d+0) 
term(1416) = term(1416) * (4.0d+0) 
term(1417) = term(1417) * (-8.0d+0) 
term(1418) = term(1418) * (4.0d+0) 
term(1419) = term(1419) * (-8.0d+0) 
term(1420) = term(1420) * (-8.0d+0) 
term(1421) = term(1421) * (16.0d+0) 
term(1422) = term(1422) * (4.0d+0) 
term(1423) = term(1423) * (-8.0d+0) 
term(1424) = term(1424) * (4.0d+0) 
term(1425) = term(1425) * (-8.0d+0) 
term(1426) = term(1426) * (4.0d+0) 
term(1427) = term(1427) * (-8.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(1428) = term(1428) + r2(vrdav_Rr, a,j,p,i) * wm_interm_20_pt4(a,b) * wm_interm_43_pt4(b,j,i,q)
term(1429) = term(1429) + r2(vrdav_Rr, a,j,p,i) * wm_interm_21_pt4(a,b) * wm_interm_43_pt4(b,j,i,q)
term(1430) = term(1430) + r2(vrdav_Rr, a,j,p,i) * wm_interm_20_pt4(a,b) * wm_interm_42_pt4(b,j,i,q)
term(1431) = term(1431) + r2(vrdav_Rr, a,j,p,i) * wm_interm_21_pt4(a,b) * wm_interm_42_pt4(b,j,i,q)
term(1432) = term(1432) + r2(vrdav_Rr, a,j,p,i) * wm_interm_23_pt4(b,j,i,q) * wm_interm_51_pt4(a,b)
term(1433) = term(1433) + r2(vrdav_Rr, a,j,p,i) * wm_interm_23_pt4(b,j,i,q) * wm_interm_52_pt4(a,b)
term(1434) = term(1434) + t2(a,b,q,i) * wm_interm_44_pt4(a,j) * wm_interm_62_pt4(p,b,j,i)
term(1435) = term(1435) + t2(a,b,q,i) * wm_interm_44_pt4(a,j) * wm_interm_61_pt4(p,b,j,i)
term(1436) = term(1436) + t2(a,b,q,i) * wm_interm_44_pt4(a,j) * wm_interm_59_pt4(p,b,j,i)
term(1437) = term(1437) + t2(a,b,q,i) * wm_interm_44_pt4(a,j) * wm_interm_60_pt4(p,b,j,i)
term(1438) = term(1438) + t2(a,b,q,i) * wm_interm_45_pt4(a,j) * wm_interm_62_pt4(p,b,j,i)
term(1439) = term(1439) + t2(a,b,q,i) * wm_interm_45_pt4(a,j) * wm_interm_61_pt4(p,b,j,i)
term(1440) = term(1440) + t2(a,b,q,i) * wm_interm_45_pt4(a,j) * wm_interm_59_pt4(p,b,j,i)
term(1441) = term(1441) + t2(a,b,q,i) * wm_interm_45_pt4(a,j) * wm_interm_60_pt4(p,b,j,i)
term(1442) = term(1442) + t2(a,b,q,i) * wm_interm_44_pt4(b,j) * wm_interm_59_pt4(p,a,j,i)
term(1443) = term(1443) + t2(a,b,q,i) * wm_interm_44_pt4(b,j) * wm_interm_60_pt4(p,a,j,i)
term(1444) = term(1444) + t2(a,b,q,i) * wm_interm_44_pt4(b,j) * wm_interm_61_pt4(p,a,j,i)
term(1445) = term(1445) + t2(a,b,q,i) * wm_interm_44_pt4(b,j) * wm_interm_62_pt4(p,a,j,i)
term(1446) = term(1446) + t2(a,b,q,i) * wm_interm_45_pt4(b,j) * wm_interm_59_pt4(p,a,j,i)
term(1447) = term(1447) + t2(a,b,q,i) * wm_interm_45_pt4(b,j) * wm_interm_60_pt4(p,a,j,i)
term(1448) = term(1448) + t2(a,b,q,i) * wm_interm_45_pt4(b,j) * wm_interm_61_pt4(p,a,j,i)
term(1449) = term(1449) + t2(a,b,q,i) * wm_interm_45_pt4(b,j) * wm_interm_62_pt4(p,a,j,i)
term(1450) = term(1450) + t2(a,p,j,i) * wm_interm_23_pt4(b,j,i,q) * wm_interm_74_pt4(a,b)
term(1451) = term(1451) + t2(a,p,j,i) * wm_interm_23_pt4(b,j,i,q) * wm_interm_75_pt4(a,b)
end do 
end do 
end do 
end do 

term(1428) = term(1428) * (-1.0d+0) 
term(1429) = term(1429) * (2.0d+0) 
term(1430) = term(1430) * (2.0d+0) 
term(1431) = term(1431) * (-4.0d+0) 
term(1432) = term(1432) * (-8.0d+0) 
term(1433) = term(1433) * (4.0d+0) 
term(1434) = term(1434) * (4.0d+0) 
term(1435) = term(1435) * (-8.0d+0) 
term(1436) = term(1436) * (-8.0d+0) 
term(1437) = term(1437) * (16.0d+0) 
term(1438) = term(1438) * (-2.0d+0) 
term(1439) = term(1439) * (4.0d+0) 
term(1440) = term(1440) * (4.0d+0) 
term(1441) = term(1441) * (-8.0d+0) 
term(1442) = term(1442) * (4.0d+0) 
term(1443) = term(1443) * (-8.0d+0) 
term(1444) = term(1444) * (4.0d+0) 
term(1445) = term(1445) * (-8.0d+0) 
term(1446) = term(1446) * (-2.0d+0) 
term(1447) = term(1447) * (4.0d+0) 
term(1448) = term(1448) * (-2.0d+0) 
term(1449) = term(1449) * (4.0d+0) 
term(1450) = term(1450) * (8.0d+0) 
term(1451) = term(1451) * (-16.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do j = 1, nocc 
term(1452) = term(1452) + s1(p,i) * wm_interm_10_pt4(j,k,l,i) * wm_interm_63_pt4(q,l,j,k)
term(1453) = term(1453) + s1(p,i) * wm_interm_10_pt4(j,k,l,i) * wm_interm_63_pt4(l,q,j,k)
end do 
end do 
end do 
end do 

term(1452) = term(1452) * (2.0d+0) 
term(1453) = term(1453) * (-4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do k = 1, nocc 
term(1454) = term(1454) + s1(p,i) * wm_interm_10_pt4(j,k,l,i) * wm_interm_63_pt4(q,l,k,j)
term(1455) = term(1455) + s1(p,i) * wm_interm_10_pt4(j,k,l,i) * wm_interm_63_pt4(l,q,k,j)
end do 
end do 
end do 
end do 

term(1454) = term(1454) * (-4.0d+0) 
term(1455) = term(1455) * (2.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(1456) = term(1456) + r2(vrdav_Rl, a,q,p,i) * wm_interm_20_pt4(b,a) * wm_interm_31_pt4(b,i)
term(1457) = term(1457) + r2(vrdav_Rl, a,q,p,i) * wm_interm_21_pt4(b,a) * wm_interm_31_pt4(b,i)
term(1458) = term(1458) + r2(vrdav_Rl, a,q,p,i) * wm_interm_20_pt4(b,a) * wm_interm_34_pt4(b,i)
term(1459) = term(1459) + r2(vrdav_Rl, a,q,p,i) * wm_interm_21_pt4(b,a) * wm_interm_34_pt4(b,i)
term(1460) = term(1460) + s2(a,p,q,i) * wm_interm_31_pt4(b,i) * wm_interm_51_pt4(b,a)
term(1461) = term(1461) + s2(a,p,q,i) * wm_interm_31_pt4(b,i) * wm_interm_52_pt4(b,a)
term(1462) = term(1462) + s2(a,p,q,i) * wm_interm_34_pt4(b,i) * wm_interm_51_pt4(b,a)
term(1463) = term(1463) + s2(a,p,q,i) * wm_interm_34_pt4(b,i) * wm_interm_52_pt4(b,a)
term(1464) = term(1464) + s2(a,p,q,i) * wm_interm_74_pt4(b,a) * wm_interm_8_pt4(b,i)
term(1465) = term(1465) + s2(a,p,q,i) * wm_interm_75_pt4(b,a) * wm_interm_8_pt4(b,i)
term(1466) = term(1466) + s2(a,p,q,i) * wm_interm_74_pt4(b,a) * wm_interm_7_pt4(b,i)
term(1467) = term(1467) + s2(a,p,q,i) * wm_interm_75_pt4(b,a) * wm_interm_7_pt4(b,i)
term(1468) = term(1468) + s1(a,i) * wm_interm_51_pt4(b,a) * wm_interm_62_pt4(b,p,i,q)
term(1469) = term(1469) + s1(a,i) * wm_interm_51_pt4(b,a) * wm_interm_61_pt4(b,p,i,q)
term(1470) = term(1470) + s1(a,i) * wm_interm_52_pt4(b,a) * wm_interm_62_pt4(b,p,i,q)
term(1471) = term(1471) + s1(a,i) * wm_interm_52_pt4(b,a) * wm_interm_61_pt4(b,p,i,q)
term(1472) = term(1472) + s1(a,i) * wm_interm_51_pt4(b,a) * wm_interm_59_pt4(b,p,i,q)
term(1473) = term(1473) + s1(a,i) * wm_interm_51_pt4(b,a) * wm_interm_60_pt4(b,p,i,q)
term(1474) = term(1474) + s1(a,i) * wm_interm_52_pt4(b,a) * wm_interm_59_pt4(b,p,i,q)
term(1475) = term(1475) + s1(a,i) * wm_interm_52_pt4(b,a) * wm_interm_60_pt4(b,p,i,q)
term(1476) = term(1476) + t1(a,i) * wm_interm_48_pt4(b,p,i,q) * wm_interm_68_pt4(b,a)
term(1477) = term(1477) + t1(a,i) * wm_interm_48_pt4(b,p,i,q) * wm_interm_69_pt4(b,a)
term(1478) = term(1478) + t1(a,i) * wm_interm_49_pt4(b,p,i,q) * wm_interm_68_pt4(b,a)
term(1479) = term(1479) + t1(a,i) * wm_interm_49_pt4(b,p,i,q) * wm_interm_69_pt4(b,a)
term(1480) = term(1480) + t1(a,i) * wm_interm_53_pt4(b,p,i,q) * wm_interm_68_pt4(b,a)
term(1481) = term(1481) + t1(a,i) * wm_interm_53_pt4(b,p,i,q) * wm_interm_69_pt4(b,a)
term(1482) = term(1482) + t1(a,i) * wm_interm_50_pt4(b,p,i,q) * wm_interm_68_pt4(b,a)
term(1483) = term(1483) + t1(a,i) * wm_interm_50_pt4(b,p,i,q) * wm_interm_69_pt4(b,a)
term(1484) = term(1484) + t2(a,p,q,i) * wm_interm_44_pt4(b,i) * wm_interm_68_pt4(b,a)
term(1485) = term(1485) + t2(a,p,q,i) * wm_interm_44_pt4(b,i) * wm_interm_69_pt4(b,a)
term(1486) = term(1486) + t2(a,p,q,i) * wm_interm_45_pt4(b,i) * wm_interm_68_pt4(b,a)
term(1487) = term(1487) + t2(a,p,q,i) * wm_interm_45_pt4(b,i) * wm_interm_69_pt4(b,a)
end do 
end do 
end do 

term(1456) = term(1456) * (-4.0d+0) 
term(1457) = term(1457) * (8.0d+0) 
term(1458) = term(1458) * (2.0d+0) 
term(1459) = term(1459) * (-4.0d+0) 
term(1460) = term(1460) * (8.0d+0) 
term(1461) = term(1461) * (-4.0d+0) 
term(1462) = term(1462) * (-4.0d+0) 
term(1463) = term(1463) * (2.0d+0) 
term(1464) = term(1464) * (4.0d+0) 
term(1465) = term(1465) * (-8.0d+0) 
term(1466) = term(1466) * (-8.0d+0) 
term(1467) = term(1467) * (16.0d+0) 
term(1468) = term(1468) * (4.0d+0) 
term(1469) = term(1469) * (-8.0d+0) 
term(1470) = term(1470) * (-2.0d+0) 
term(1471) = term(1471) * (4.0d+0) 
term(1472) = term(1472) * (-8.0d+0) 
term(1473) = term(1473) * (16.0d+0) 
term(1474) = term(1474) * (4.0d+0) 
term(1475) = term(1475) * (-8.0d+0) 
term(1476) = term(1476) * (-8.0d+0) 
term(1477) = term(1477) * (16.0d+0) 
term(1478) = term(1478) * (4.0d+0) 
term(1479) = term(1479) * (-8.0d+0) 
term(1480) = term(1480) * (4.0d+0) 
term(1481) = term(1481) * (-8.0d+0) 
term(1482) = term(1482) * (-2.0d+0) 
term(1483) = term(1483) * (4.0d+0) 
term(1484) = term(1484) * (4.0d+0) 
term(1485) = term(1485) * (-8.0d+0) 
term(1486) = term(1486) * (-2.0d+0) 
term(1487) = term(1487) * (4.0d+0) 

do a = nocc + 1, nactive 
do i = 1, nocc 
do b = nocc + 1, nactive 
term(1488) = term(1488) + r2(vrdav_Rl, a,i,p,q) * wm_interm_20_pt4(b,a) * wm_interm_31_pt4(b,i)
term(1489) = term(1489) + r2(vrdav_Rl, a,i,p,q) * wm_interm_21_pt4(b,a) * wm_interm_31_pt4(b,i)
term(1490) = term(1490) + r2(vrdav_Rl, a,i,p,q) * wm_interm_20_pt4(b,a) * wm_interm_34_pt4(b,i)
term(1491) = term(1491) + r2(vrdav_Rl, a,i,p,q) * wm_interm_21_pt4(b,a) * wm_interm_34_pt4(b,i)
term(1492) = term(1492) + s2(a,p,i,q) * wm_interm_31_pt4(b,i) * wm_interm_51_pt4(b,a)
term(1493) = term(1493) + s2(a,p,i,q) * wm_interm_31_pt4(b,i) * wm_interm_52_pt4(b,a)
term(1494) = term(1494) + s2(a,p,i,q) * wm_interm_34_pt4(b,i) * wm_interm_51_pt4(b,a)
term(1495) = term(1495) + s2(a,p,i,q) * wm_interm_34_pt4(b,i) * wm_interm_52_pt4(b,a)
term(1496) = term(1496) + s2(a,p,i,q) * wm_interm_74_pt4(b,a) * wm_interm_8_pt4(b,i)
term(1497) = term(1497) + s2(a,p,i,q) * wm_interm_75_pt4(b,a) * wm_interm_8_pt4(b,i)
term(1498) = term(1498) + s2(a,p,i,q) * wm_interm_74_pt4(b,a) * wm_interm_7_pt4(b,i)
term(1499) = term(1499) + s2(a,p,i,q) * wm_interm_75_pt4(b,a) * wm_interm_7_pt4(b,i)
term(1500) = term(1500) + t2(a,p,i,q) * wm_interm_44_pt4(b,i) * wm_interm_68_pt4(b,a)
term(1501) = term(1501) + t2(a,p,i,q) * wm_interm_44_pt4(b,i) * wm_interm_69_pt4(b,a)
term(1502) = term(1502) + t2(a,p,i,q) * wm_interm_45_pt4(b,i) * wm_interm_68_pt4(b,a)
term(1503) = term(1503) + t2(a,p,i,q) * wm_interm_45_pt4(b,i) * wm_interm_69_pt4(b,a)
end do 
end do 
end do 

term(1488) = term(1488) * (8.0d+0) 
term(1489) = term(1489) * (-16.0d+0) 
term(1490) = term(1490) * (-4.0d+0) 
term(1491) = term(1491) * (8.0d+0) 
term(1492) = term(1492) * (-16.0d+0) 
term(1493) = term(1493) * (8.0d+0) 
term(1494) = term(1494) * (8.0d+0) 
term(1495) = term(1495) * (-4.0d+0) 
term(1496) = term(1496) * (-8.0d+0) 
term(1497) = term(1497) * (16.0d+0) 
term(1498) = term(1498) * (16.0d+0) 
term(1499) = term(1499) * (-32.0d+0) 
term(1500) = term(1500) * (-8.0d+0) 
term(1501) = term(1501) * (16.0d+0) 
term(1502) = term(1502) * (4.0d+0) 
term(1503) = term(1503) * (-8.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
term(1504) = term(1504) + s1(p,i) * wm_interm_18_pt4(j,k) * wm_interm_63_pt4(q,k,i,j)
term(1505) = term(1505) + s1(p,i) * wm_interm_19_pt4(j,k) * wm_interm_63_pt4(q,k,i,j)
term(1506) = term(1506) + s1(p,i) * wm_interm_18_pt4(j,k) * wm_interm_63_pt4(k,q,i,j)
term(1507) = term(1507) + s1(p,i) * wm_interm_19_pt4(j,k) * wm_interm_63_pt4(k,q,i,j)
end do 
end do 
end do 

term(1504) = term(1504) * (2.0d+0) 
term(1505) = term(1505) * (-4.0d+0) 
term(1506) = term(1506) * (-1.0d+0) 
term(1507) = term(1507) * (2.0d+0) 

do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(1508) = term(1508) + r2(vrdav_Rl, a,i,b,q) * wm_interm_20_pt4(p,a) * wm_interm_31_pt4(b,i)
term(1509) = term(1509) + r2(vrdav_Rl, a,i,b,q) * wm_interm_21_pt4(p,a) * wm_interm_31_pt4(b,i)
term(1510) = term(1510) + r2(vrdav_Rl, a,i,b,q) * wm_interm_20_pt4(p,a) * wm_interm_34_pt4(b,i)
term(1511) = term(1511) + r2(vrdav_Rl, a,i,b,q) * wm_interm_21_pt4(p,a) * wm_interm_34_pt4(b,i)
term(1512) = term(1512) + t2(a,b,i,q) * wm_interm_44_pt4(b,i) * wm_interm_68_pt4(p,a)
term(1513) = term(1513) + t2(a,b,i,q) * wm_interm_44_pt4(b,i) * wm_interm_69_pt4(p,a)
term(1514) = term(1514) + t2(a,b,i,q) * wm_interm_45_pt4(b,i) * wm_interm_68_pt4(p,a)
term(1515) = term(1515) + t2(a,b,i,q) * wm_interm_45_pt4(b,i) * wm_interm_69_pt4(p,a)
end do 
end do 
end do 

term(1508) = term(1508) * (-4.0d+0) 
term(1509) = term(1509) * (8.0d+0) 
term(1510) = term(1510) * (2.0d+0) 
term(1511) = term(1511) * (-4.0d+0) 
term(1512) = term(1512) * (4.0d+0) 
term(1513) = term(1513) * (-8.0d+0) 
term(1514) = term(1514) * (-2.0d+0) 
term(1515) = term(1515) * (4.0d+0) 


    calc_D_vo_wm_pt4 = zero
    do s = 0, 1515
    calc_D_vo_wm_pt4 = calc_D_vo_wm_pt4 + term(s)
    end do

    end function calc_D_vo_wm_pt4
    
    function calc_D_vv_wm_pt4(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vv_wm_pt4
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
    real(F64), dimension(0:568) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(0) = term(0) + wm_interm_28_pt4(p,i,j,k) * wm_interm_42_pt4(q,j,i,k)
term(1) = term(1) + wm_interm_28_pt4(p,i,j,k) * wm_interm_42_pt4(q,i,j,k)
term(2) = term(2) + wm_interm_28_pt4(p,i,j,k) * wm_interm_43_pt4(q,i,j,k)
term(3) = term(3) + wm_interm_28_pt4(p,i,j,k) * wm_interm_43_pt4(q,j,i,k)
term(4) = term(4) + r1(vrdav_Rr, p,i) * wm_interm_23_pt4(q,i,j,k) * wm_interm_56_pt4(k,j)
term(5) = term(5) + r1(vrdav_Rr, p,i) * wm_interm_23_pt4(q,i,j,k) * wm_interm_54_pt4(k,j)
end do 
end do 
end do 

term(1) = term(1) * (-2.0d+0) 
term(3) = term(3) * (-2.0d+0) 
term(4) = term(4) * (16.0d+0) 
term(5) = term(5) * (-8.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do k = 1, nocc 
term(6) = term(6) + s1(q,i) * wm_interm_10_pt4(j,k,l,i) * wm_interm_26_pt4(p,l,k,j)
end do 
end do 
end do 
end do 

term(6) = term(6) * (-4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(7) = term(7) + wm_interm_36_pt4(q,a,i,j) * wm_interm_64_pt4(a,p,j,i)
term(8) = term(8) + wm_interm_36_pt4(q,a,i,j) * wm_interm_65_pt4(a,p,j,i)
term(9) = term(9) + wm_interm_36_pt4(q,a,i,j) * wm_interm_57_pt4(a,p,j,i)
term(10) = term(10) + wm_interm_36_pt4(q,a,i,j) * wm_interm_58_pt4(a,p,j,i)
term(11) = term(11) + wm_interm_36_pt4(a,q,i,j) * wm_interm_58_pt4(p,a,j,i)
term(12) = term(12) + wm_interm_36_pt4(a,q,i,j) * wm_interm_65_pt4(p,a,j,i)
term(13) = term(13) + wm_interm_36_pt4(a,q,i,j) * wm_interm_57_pt4(p,a,j,i)
term(14) = term(14) + wm_interm_36_pt4(a,q,i,j) * wm_interm_64_pt4(p,a,j,i)
term(15) = term(15) + s2(a,q,i,j) * wm_interm_15_pt4(a,j) * wm_interm_31_pt4(p,i)
term(16) = term(16) + s2(a,q,i,j) * wm_interm_15_pt4(a,j) * wm_interm_34_pt4(p,i)
term(17) = term(17) + s2(a,q,i,j) * wm_interm_17_pt4(a,j) * wm_interm_31_pt4(p,i)
term(18) = term(18) + s2(a,q,i,j) * wm_interm_17_pt4(a,j) * wm_interm_34_pt4(p,i)
term(19) = term(19) + r1(vrdav_Rl, a,i) * wm_interm_59_pt4(p,q,i,j) * wm_interm_8_pt4(a,j)
term(20) = term(20) + r1(vrdav_Rl, a,i) * wm_interm_60_pt4(p,q,i,j) * wm_interm_8_pt4(a,j)
term(21) = term(21) + r1(vrdav_Rl, a,i) * wm_interm_61_pt4(p,q,i,j) * wm_interm_8_pt4(a,j)
term(22) = term(22) + r1(vrdav_Rl, a,i) * wm_interm_62_pt4(p,q,i,j) * wm_interm_8_pt4(a,j)
term(23) = term(23) + s1(a,i) * wm_interm_15_pt4(a,j) * wm_interm_59_pt4(p,q,i,j)
term(24) = term(24) + s1(a,i) * wm_interm_15_pt4(a,j) * wm_interm_60_pt4(p,q,i,j)
term(25) = term(25) + s1(a,i) * wm_interm_15_pt4(a,j) * wm_interm_61_pt4(p,q,i,j)
term(26) = term(26) + s1(a,i) * wm_interm_15_pt4(a,j) * wm_interm_62_pt4(p,q,i,j)
term(27) = term(27) + r1(vrdav_Rl, a,i) * wm_interm_59_pt4(p,q,i,j) * wm_interm_7_pt4(a,j)
term(28) = term(28) + r1(vrdav_Rl, a,i) * wm_interm_60_pt4(p,q,i,j) * wm_interm_7_pt4(a,j)
term(29) = term(29) + r1(vrdav_Rl, a,i) * wm_interm_61_pt4(p,q,i,j) * wm_interm_7_pt4(a,j)
term(30) = term(30) + r1(vrdav_Rl, a,i) * wm_interm_62_pt4(p,q,i,j) * wm_interm_7_pt4(a,j)
term(31) = term(31) + s1(a,i) * wm_interm_17_pt4(a,j) * wm_interm_59_pt4(p,q,i,j)
term(32) = term(32) + s1(a,i) * wm_interm_17_pt4(a,j) * wm_interm_60_pt4(p,q,i,j)
term(33) = term(33) + s1(a,i) * wm_interm_17_pt4(a,j) * wm_interm_61_pt4(p,q,i,j)
term(34) = term(34) + s1(a,i) * wm_interm_17_pt4(a,j) * wm_interm_62_pt4(p,q,i,j)
term(35) = term(35) + s1(q,i) * wm_interm_15_pt4(a,j) * wm_interm_62_pt4(p,a,i,j)
term(36) = term(36) + s1(q,i) * wm_interm_15_pt4(a,j) * wm_interm_61_pt4(p,a,i,j)
term(37) = term(37) + s1(q,i) * wm_interm_15_pt4(a,j) * wm_interm_59_pt4(p,a,i,j)
term(38) = term(38) + s1(q,i) * wm_interm_15_pt4(a,j) * wm_interm_60_pt4(p,a,i,j)
term(39) = term(39) + s1(q,i) * wm_interm_17_pt4(a,j) * wm_interm_62_pt4(p,a,i,j)
term(40) = term(40) + s1(q,i) * wm_interm_17_pt4(a,j) * wm_interm_61_pt4(p,a,i,j)
term(41) = term(41) + s1(q,i) * wm_interm_17_pt4(a,j) * wm_interm_59_pt4(p,a,i,j)
term(42) = term(42) + s1(q,i) * wm_interm_17_pt4(a,j) * wm_interm_60_pt4(p,a,i,j)
term(43) = term(43) + s2(a,p,i,j) * wm_interm_17_pt4(q,i) * wm_interm_31_pt4(a,j)
term(44) = term(44) + s2(a,p,i,j) * wm_interm_17_pt4(q,i) * wm_interm_34_pt4(a,j)
term(45) = term(45) + s2(a,p,i,j) * wm_interm_15_pt4(q,i) * wm_interm_31_pt4(a,j)
term(46) = term(46) + s2(a,p,i,j) * wm_interm_15_pt4(q,i) * wm_interm_34_pt4(a,j)
term(47) = term(47) + s2(a,p,i,j) * wm_interm_35_pt4(a,j) * wm_interm_8_pt4(q,i)
term(48) = term(48) + s2(a,p,i,j) * wm_interm_32_pt4(a,j) * wm_interm_8_pt4(q,i)
term(49) = term(49) + s2(a,p,i,j) * wm_interm_35_pt4(a,j) * wm_interm_7_pt4(q,i)
term(50) = term(50) + s2(a,p,i,j) * wm_interm_32_pt4(a,j) * wm_interm_7_pt4(q,i)
term(51) = term(51) + r1(vrdav_Rr, p,i) * wm_interm_11_pt4(q,a,i,j) * wm_interm_44_pt4(a,j)
term(52) = term(52) + r1(vrdav_Rr, p,i) * wm_interm_12_pt4(q,a,i,j) * wm_interm_44_pt4(a,j)
term(53) = term(53) + r1(vrdav_Rr, p,i) * wm_interm_44_pt4(a,j) * wm_interm_5_pt4(q,a,i,j)
term(54) = term(54) + r1(vrdav_Rr, p,i) * wm_interm_1_pt4(q,a,i,j) * wm_interm_44_pt4(a,j)
term(55) = term(55) + r1(vrdav_Rr, p,i) * wm_interm_11_pt4(q,a,i,j) * wm_interm_45_pt4(a,j)
term(56) = term(56) + r1(vrdav_Rr, p,i) * wm_interm_12_pt4(q,a,i,j) * wm_interm_45_pt4(a,j)
term(57) = term(57) + r1(vrdav_Rr, p,i) * wm_interm_45_pt4(a,j) * wm_interm_5_pt4(q,a,i,j)
term(58) = term(58) + r1(vrdav_Rr, p,i) * wm_interm_1_pt4(q,a,i,j) * wm_interm_45_pt4(a,j)
term(59) = term(59) + t1(q,i) * wm_interm_48_pt4(p,a,i,j) * wm_interm_4_pt4(a,j)
term(60) = term(60) + t1(q,i) * wm_interm_2_pt4(a,j) * wm_interm_48_pt4(p,a,i,j)
term(61) = term(61) + t1(q,i) * wm_interm_11_pt4(p,a,i,j) * wm_interm_41_pt4(a,j)
term(62) = term(62) + t1(q,i) * wm_interm_12_pt4(p,a,i,j) * wm_interm_41_pt4(a,j)
term(63) = term(63) + t1(q,i) * wm_interm_49_pt4(p,a,i,j) * wm_interm_4_pt4(a,j)
term(64) = term(64) + t1(q,i) * wm_interm_2_pt4(a,j) * wm_interm_49_pt4(p,a,i,j)
term(65) = term(65) + t1(q,i) * wm_interm_11_pt4(p,a,i,j) * wm_interm_38_pt4(a,j)
term(66) = term(66) + t1(q,i) * wm_interm_12_pt4(p,a,i,j) * wm_interm_38_pt4(a,j)
term(67) = term(67) + t1(q,i) * wm_interm_2_pt4(a,j) * wm_interm_50_pt4(p,a,i,j)
term(68) = term(68) + t1(q,i) * wm_interm_1_pt4(p,a,i,j) * wm_interm_38_pt4(a,j)
term(69) = term(69) + t1(q,i) * wm_interm_4_pt4(a,j) * wm_interm_50_pt4(p,a,i,j)
term(70) = term(70) + t1(q,i) * wm_interm_38_pt4(a,j) * wm_interm_5_pt4(p,a,i,j)
term(71) = term(71) + t1(q,i) * wm_interm_4_pt4(a,j) * wm_interm_53_pt4(p,a,i,j)
term(72) = term(72) + t1(q,i) * wm_interm_2_pt4(a,j) * wm_interm_53_pt4(p,a,i,j)
term(73) = term(73) + t1(q,i) * wm_interm_41_pt4(a,j) * wm_interm_5_pt4(p,a,i,j)
term(74) = term(74) + t1(q,i) * wm_interm_1_pt4(p,a,i,j) * wm_interm_41_pt4(a,j)
term(75) = term(75) + t1(a,i) * wm_interm_2_pt4(a,j) * wm_interm_53_pt4(p,q,i,j)
term(76) = term(76) + t1(a,i) * wm_interm_4_pt4(a,j) * wm_interm_53_pt4(p,q,i,j)
term(77) = term(77) + t1(a,i) * wm_interm_48_pt4(p,q,i,j) * wm_interm_4_pt4(a,j)
term(78) = term(78) + t1(a,i) * wm_interm_2_pt4(a,j) * wm_interm_48_pt4(p,q,i,j)
term(79) = term(79) + t1(a,i) * wm_interm_49_pt4(p,q,i,j) * wm_interm_4_pt4(a,j)
term(80) = term(80) + t1(a,i) * wm_interm_2_pt4(a,j) * wm_interm_49_pt4(p,q,i,j)
term(81) = term(81) + t1(a,i) * wm_interm_4_pt4(a,j) * wm_interm_50_pt4(p,q,i,j)
term(82) = term(82) + t1(a,i) * wm_interm_2_pt4(a,j) * wm_interm_50_pt4(p,q,i,j)
term(83) = term(83) + t1(a,i) * wm_interm_1_pt4(p,q,i,j) * wm_interm_38_pt4(a,j)
term(84) = term(84) + t1(a,i) * wm_interm_38_pt4(a,j) * wm_interm_5_pt4(p,q,i,j)
term(85) = term(85) + t1(a,i) * wm_interm_11_pt4(p,q,i,j) * wm_interm_38_pt4(a,j)
term(86) = term(86) + t1(a,i) * wm_interm_12_pt4(p,q,i,j) * wm_interm_38_pt4(a,j)
term(87) = term(87) + t1(a,i) * wm_interm_11_pt4(p,q,i,j) * wm_interm_41_pt4(a,j)
term(88) = term(88) + t1(a,i) * wm_interm_41_pt4(a,j) * wm_interm_5_pt4(p,q,i,j)
term(89) = term(89) + t1(a,i) * wm_interm_1_pt4(p,q,i,j) * wm_interm_41_pt4(a,j)
term(90) = term(90) + t1(a,i) * wm_interm_12_pt4(p,q,i,j) * wm_interm_41_pt4(a,j)
term(91) = term(91) + r1(vrdav_Rr, a,i) * wm_interm_11_pt4(p,q,i,j) * wm_interm_44_pt4(a,j)
term(92) = term(92) + r1(vrdav_Rr, a,i) * wm_interm_1_pt4(p,q,i,j) * wm_interm_44_pt4(a,j)
term(93) = term(93) + r1(vrdav_Rr, a,i) * wm_interm_44_pt4(a,j) * wm_interm_5_pt4(p,q,i,j)
term(94) = term(94) + r1(vrdav_Rr, a,i) * wm_interm_12_pt4(p,q,i,j) * wm_interm_44_pt4(a,j)
term(95) = term(95) + t2(a,q,i,j) * wm_interm_44_pt4(p,i) * wm_interm_4_pt4(a,j)
term(96) = term(96) + t2(a,q,i,j) * wm_interm_2_pt4(a,j) * wm_interm_44_pt4(p,i)
term(97) = term(97) + r1(vrdav_Rr, a,i) * wm_interm_11_pt4(p,q,i,j) * wm_interm_45_pt4(a,j)
term(98) = term(98) + r1(vrdav_Rr, a,i) * wm_interm_1_pt4(p,q,i,j) * wm_interm_45_pt4(a,j)
term(99) = term(99) + r1(vrdav_Rr, a,i) * wm_interm_45_pt4(a,j) * wm_interm_5_pt4(p,q,i,j)
term(100) = term(100) + r1(vrdav_Rr, a,i) * wm_interm_12_pt4(p,q,i,j) * wm_interm_45_pt4(a,j)
term(101) = term(101) + t2(a,q,i,j) * wm_interm_45_pt4(p,i) * wm_interm_4_pt4(a,j)
term(102) = term(102) + t2(a,q,i,j) * wm_interm_2_pt4(a,j) * wm_interm_45_pt4(p,i)
term(103) = term(103) + t2(a,q,i,j) * wm_interm_2_pt4(p,i) * wm_interm_44_pt4(a,j)
term(104) = term(104) + t2(a,q,i,j) * wm_interm_44_pt4(a,j) * wm_interm_4_pt4(p,i)
term(105) = term(105) + t2(a,q,i,j) * wm_interm_2_pt4(p,i) * wm_interm_45_pt4(a,j)
term(106) = term(106) + t2(a,q,i,j) * wm_interm_45_pt4(a,j) * wm_interm_4_pt4(p,i)
end do 
end do 
end do 

term(7) = term(7) * (-4.0d+0) 
term(8) = term(8) * (8.0d+0) 
term(9) = term(9) * (2.0d+0) 
term(10) = term(10) * (-4.0d+0) 
term(11) = term(11) * (4.0d+0) 
term(12) = term(12) * (-8.0d+0) 
term(13) = term(13) * (-2.0d+0) 
term(14) = term(14) * (4.0d+0) 
term(15) = term(15) * (-8.0d+0) 
term(16) = term(16) * (4.0d+0) 
term(17) = term(17) * (4.0d+0) 
term(18) = term(18) * (-2.0d+0) 
term(19) = term(19) * (-4.0d+0) 
term(20) = term(20) * (8.0d+0) 
term(21) = term(21) * (-4.0d+0) 
term(22) = term(22) * (8.0d+0) 
term(23) = term(23) * (8.0d+0) 
term(24) = term(24) * (-16.0d+0) 
term(25) = term(25) * (8.0d+0) 
term(26) = term(26) * (-16.0d+0) 
term(27) = term(27) * (8.0d+0) 
term(28) = term(28) * (-16.0d+0) 
term(29) = term(29) * (8.0d+0) 
term(30) = term(30) * (-16.0d+0) 
term(31) = term(31) * (-4.0d+0) 
term(32) = term(32) * (8.0d+0) 
term(33) = term(33) * (-4.0d+0) 
term(34) = term(34) * (8.0d+0) 
term(35) = term(35) * (-4.0d+0) 
term(36) = term(36) * (8.0d+0) 
term(37) = term(37) * (8.0d+0) 
term(38) = term(38) * (-16.0d+0) 
term(39) = term(39) * (2.0d+0) 
term(40) = term(40) * (-4.0d+0) 
term(41) = term(41) * (-4.0d+0) 
term(42) = term(42) * (8.0d+0) 
term(43) = term(43) * (4.0d+0) 
term(44) = term(44) * (-2.0d+0) 
term(45) = term(45) * (-8.0d+0) 
term(46) = term(46) * (4.0d+0) 
term(47) = term(47) * (-4.0d+0) 
term(48) = term(48) * (8.0d+0) 
term(49) = term(49) * (8.0d+0) 
term(50) = term(50) * (-16.0d+0) 
term(51) = term(51) * (16.0d+0) 
term(52) = term(52) * (-32.0d+0) 
term(53) = term(53) * (-8.0d+0) 
term(54) = term(54) * (16.0d+0) 
term(55) = term(55) * (-8.0d+0) 
term(56) = term(56) * (16.0d+0) 
term(57) = term(57) * (4.0d+0) 
term(58) = term(58) * (-8.0d+0) 
term(59) = term(59) * (8.0d+0) 
term(60) = term(60) * (-16.0d+0) 
term(61) = term(61) * (8.0d+0) 
term(62) = term(62) * (-16.0d+0) 
term(63) = term(63) * (-4.0d+0) 
term(64) = term(64) * (8.0d+0) 
term(65) = term(65) * (-4.0d+0) 
term(66) = term(66) * (8.0d+0) 
term(67) = term(67) * (-4.0d+0) 
term(68) = term(68) * (-4.0d+0) 
term(69) = term(69) * (2.0d+0) 
term(70) = term(70) * (2.0d+0) 
term(71) = term(71) * (-4.0d+0) 
term(72) = term(72) * (8.0d+0) 
term(73) = term(73) * (-4.0d+0) 
term(74) = term(74) * (8.0d+0) 
term(75) = term(75) * (-4.0d+0) 
term(76) = term(76) * (2.0d+0) 
term(77) = term(77) * (-4.0d+0) 
term(78) = term(78) * (8.0d+0) 
term(79) = term(79) * (2.0d+0) 
term(80) = term(80) * (-4.0d+0) 
term(81) = term(81) * (-4.0d+0) 
term(82) = term(82) * (8.0d+0) 
term(83) = term(83) * (2.0d+0) 
term(84) = term(84) * (-4.0d+0) 
term(85) = term(85) * (2.0d+0) 
term(86) = term(86) * (-4.0d+0) 
term(87) = term(87) * (-4.0d+0) 
term(88) = term(88) * (8.0d+0) 
term(89) = term(89) * (-4.0d+0) 
term(90) = term(90) * (8.0d+0) 
term(91) = term(91) * (-4.0d+0) 
term(92) = term(92) * (-4.0d+0) 
term(93) = term(93) * (8.0d+0) 
term(94) = term(94) * (8.0d+0) 
term(95) = term(95) * (-4.0d+0) 
term(96) = term(96) * (8.0d+0) 
term(97) = term(97) * (2.0d+0) 
term(98) = term(98) * (2.0d+0) 
term(99) = term(99) * (-4.0d+0) 
term(100) = term(100) * (-4.0d+0) 
term(101) = term(101) * (2.0d+0) 
term(102) = term(102) * (-4.0d+0) 
term(103) = term(103) * (8.0d+0) 
term(104) = term(104) * (-4.0d+0) 
term(105) = term(105) * (-4.0d+0) 
term(106) = term(106) * (2.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(107) = term(107) + s1(a,i) * wm_interm_20_pt4(p,q) * wm_interm_32_pt4(a,i)
term(108) = term(108) + r1(vrdav_Rl, p,i) * wm_interm_20_pt4(a,q) * wm_interm_31_pt4(a,i)
term(109) = term(109) + s1(a,i) * wm_interm_21_pt4(p,q) * wm_interm_32_pt4(a,i)
term(110) = term(110) + r1(vrdav_Rl, p,i) * wm_interm_21_pt4(a,q) * wm_interm_31_pt4(a,i)
term(111) = term(111) + s1(a,i) * wm_interm_20_pt4(p,q) * wm_interm_35_pt4(a,i)
term(112) = term(112) + r1(vrdav_Rl, p,i) * wm_interm_20_pt4(a,q) * wm_interm_34_pt4(a,i)
term(113) = term(113) + s1(a,i) * wm_interm_21_pt4(p,q) * wm_interm_35_pt4(a,i)
term(114) = term(114) + r1(vrdav_Rl, p,i) * wm_interm_21_pt4(a,q) * wm_interm_34_pt4(a,i)
term(115) = term(115) + r1(vrdav_Rl, a,i) * wm_interm_68_pt4(a,p) * wm_interm_8_pt4(q,i)
term(116) = term(116) + r1(vrdav_Rl, a,i) * wm_interm_69_pt4(a,p) * wm_interm_8_pt4(q,i)
term(117) = term(117) + r1(vrdav_Rl, a,i) * wm_interm_68_pt4(a,p) * wm_interm_7_pt4(q,i)
term(118) = term(118) + r1(vrdav_Rl, a,i) * wm_interm_69_pt4(a,p) * wm_interm_7_pt4(q,i)
term(119) = term(119) + s1(a,i) * wm_interm_17_pt4(q,i) * wm_interm_68_pt4(a,p)
term(120) = term(120) + s1(a,i) * wm_interm_17_pt4(q,i) * wm_interm_69_pt4(a,p)
term(121) = term(121) + s1(p,i) * wm_interm_20_pt4(a,q) * wm_interm_35_pt4(a,i)
term(122) = term(122) + s1(p,i) * wm_interm_20_pt4(a,q) * wm_interm_32_pt4(a,i)
term(123) = term(123) + s1(p,i) * wm_interm_21_pt4(a,q) * wm_interm_35_pt4(a,i)
term(124) = term(124) + s1(p,i) * wm_interm_21_pt4(a,q) * wm_interm_32_pt4(a,i)
term(125) = term(125) + s1(a,i) * wm_interm_15_pt4(q,i) * wm_interm_68_pt4(a,p)
term(126) = term(126) + s1(a,i) * wm_interm_15_pt4(q,i) * wm_interm_69_pt4(a,p)
term(127) = term(127) + t1(a,i) * wm_interm_20_pt4(a,q) * wm_interm_38_pt4(p,i)
term(128) = term(128) + t1(a,i) * wm_interm_21_pt4(a,q) * wm_interm_38_pt4(p,i)
term(129) = term(129) + t1(a,i) * wm_interm_20_pt4(a,q) * wm_interm_41_pt4(p,i)
term(130) = term(130) + t1(a,i) * wm_interm_21_pt4(a,q) * wm_interm_41_pt4(p,i)
term(131) = term(131) + t1(a,i) * wm_interm_2_pt4(p,i) * wm_interm_52_pt4(a,q)
term(132) = term(132) + t1(a,i) * wm_interm_4_pt4(p,i) * wm_interm_52_pt4(a,q)
term(133) = term(133) + t1(a,i) * wm_interm_2_pt4(p,i) * wm_interm_51_pt4(a,q)
term(134) = term(134) + t1(a,i) * wm_interm_4_pt4(p,i) * wm_interm_51_pt4(a,q)
term(135) = term(135) + t1(a,i) * wm_interm_20_pt4(p,q) * wm_interm_41_pt4(a,i)
term(136) = term(136) + r1(vrdav_Rr, a,i) * wm_interm_20_pt4(a,q) * wm_interm_44_pt4(p,i)
term(137) = term(137) + t1(a,i) * wm_interm_21_pt4(p,q) * wm_interm_41_pt4(a,i)
term(138) = term(138) + r1(vrdav_Rr, a,i) * wm_interm_21_pt4(a,q) * wm_interm_44_pt4(p,i)
term(139) = term(139) + t1(a,i) * wm_interm_20_pt4(p,q) * wm_interm_38_pt4(a,i)
term(140) = term(140) + r1(vrdav_Rr, a,i) * wm_interm_20_pt4(a,q) * wm_interm_45_pt4(p,i)
term(141) = term(141) + t1(a,i) * wm_interm_21_pt4(p,q) * wm_interm_38_pt4(a,i)
term(142) = term(142) + r1(vrdav_Rr, a,i) * wm_interm_21_pt4(a,q) * wm_interm_45_pt4(p,i)
end do 
end do 

term(107) = term(107) * (-8.0d+0) 
term(108) = term(108) * (4.0d+0) 
term(109) = term(109) * (16.0d+0) 
term(110) = term(110) * (-8.0d+0) 
term(111) = term(111) * (4.0d+0) 
term(112) = term(112) * (-2.0d+0) 
term(113) = term(113) * (-8.0d+0) 
term(114) = term(114) * (4.0d+0) 
term(115) = term(115) * (-4.0d+0) 
term(116) = term(116) * (8.0d+0) 
term(117) = term(117) * (8.0d+0) 
term(118) = term(118) * (-16.0d+0) 
term(119) = term(119) * (-4.0d+0) 
term(120) = term(120) * (8.0d+0) 
term(121) = term(121) * (-4.0d+0) 
term(122) = term(122) * (8.0d+0) 
term(123) = term(123) * (8.0d+0) 
term(124) = term(124) * (-16.0d+0) 
term(125) = term(125) * (8.0d+0) 
term(126) = term(126) * (-16.0d+0) 
term(127) = term(127) * (2.0d+0) 
term(128) = term(128) * (-4.0d+0) 
term(129) = term(129) * (-4.0d+0) 
term(130) = term(130) * (8.0d+0) 
term(131) = term(131) * (-4.0d+0) 
term(132) = term(132) * (2.0d+0) 
term(133) = term(133) * (8.0d+0) 
term(134) = term(134) * (-4.0d+0) 
term(135) = term(135) * (8.0d+0) 
term(136) = term(136) * (-4.0d+0) 
term(137) = term(137) * (-16.0d+0) 
term(138) = term(138) * (8.0d+0) 
term(139) = term(139) * (-4.0d+0) 
term(140) = term(140) * (2.0d+0) 
term(141) = term(141) * (8.0d+0) 
term(142) = term(142) * (-4.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(143) = term(143) + s2(a,q,j,i) * wm_interm_15_pt4(a,j) * wm_interm_31_pt4(p,i)
term(144) = term(144) + s2(a,q,j,i) * wm_interm_15_pt4(a,j) * wm_interm_34_pt4(p,i)
term(145) = term(145) + s2(a,q,j,i) * wm_interm_17_pt4(a,j) * wm_interm_31_pt4(p,i)
term(146) = term(146) + s2(a,q,j,i) * wm_interm_17_pt4(a,j) * wm_interm_34_pt4(p,i)
term(147) = term(147) + r1(vrdav_Rl, a,i) * wm_interm_11_pt4(p,q,j,i) * wm_interm_31_pt4(a,j)
term(148) = term(148) + r1(vrdav_Rl, p,i) * wm_interm_11_pt4(a,q,j,i) * wm_interm_31_pt4(a,j)
term(149) = term(149) + r1(vrdav_Rl, a,i) * wm_interm_1_pt4(p,q,j,i) * wm_interm_31_pt4(a,j)
term(150) = term(150) + r1(vrdav_Rl, a,i) * wm_interm_31_pt4(a,j) * wm_interm_5_pt4(p,q,j,i)
term(151) = term(151) + r1(vrdav_Rl, a,i) * wm_interm_12_pt4(p,q,j,i) * wm_interm_31_pt4(a,j)
term(152) = term(152) + r1(vrdav_Rl, p,i) * wm_interm_31_pt4(a,j) * wm_interm_5_pt4(a,q,j,i)
term(153) = term(153) + r1(vrdav_Rl, p,i) * wm_interm_1_pt4(a,q,j,i) * wm_interm_31_pt4(a,j)
term(154) = term(154) + r1(vrdav_Rl, p,i) * wm_interm_12_pt4(a,q,j,i) * wm_interm_31_pt4(a,j)
term(155) = term(155) + r1(vrdav_Rl, a,i) * wm_interm_11_pt4(p,q,j,i) * wm_interm_34_pt4(a,j)
term(156) = term(156) + r1(vrdav_Rl, p,i) * wm_interm_11_pt4(a,q,j,i) * wm_interm_34_pt4(a,j)
term(157) = term(157) + r1(vrdav_Rl, a,i) * wm_interm_1_pt4(p,q,j,i) * wm_interm_34_pt4(a,j)
term(158) = term(158) + r1(vrdav_Rl, a,i) * wm_interm_34_pt4(a,j) * wm_interm_5_pt4(p,q,j,i)
term(159) = term(159) + r1(vrdav_Rl, a,i) * wm_interm_12_pt4(p,q,j,i) * wm_interm_34_pt4(a,j)
term(160) = term(160) + r1(vrdav_Rl, p,i) * wm_interm_34_pt4(a,j) * wm_interm_5_pt4(a,q,j,i)
term(161) = term(161) + r1(vrdav_Rl, p,i) * wm_interm_1_pt4(a,q,j,i) * wm_interm_34_pt4(a,j)
term(162) = term(162) + r1(vrdav_Rl, p,i) * wm_interm_12_pt4(a,q,j,i) * wm_interm_34_pt4(a,j)
term(163) = term(163) + s2(a,p,j,i) * wm_interm_17_pt4(q,i) * wm_interm_31_pt4(a,j)
term(164) = term(164) + s2(a,p,j,i) * wm_interm_17_pt4(q,i) * wm_interm_34_pt4(a,j)
term(165) = term(165) + s2(a,p,j,i) * wm_interm_15_pt4(q,i) * wm_interm_31_pt4(a,j)
term(166) = term(166) + s2(a,p,j,i) * wm_interm_15_pt4(q,i) * wm_interm_34_pt4(a,j)
term(167) = term(167) + s2(a,p,j,i) * wm_interm_35_pt4(a,j) * wm_interm_8_pt4(q,i)
term(168) = term(168) + s2(a,p,j,i) * wm_interm_32_pt4(a,j) * wm_interm_8_pt4(q,i)
term(169) = term(169) + s2(a,p,j,i) * wm_interm_35_pt4(a,j) * wm_interm_7_pt4(q,i)
term(170) = term(170) + s2(a,p,j,i) * wm_interm_32_pt4(a,j) * wm_interm_7_pt4(q,i)
term(171) = term(171) + s1(p,i) * wm_interm_35_pt4(a,j) * wm_interm_5_pt4(a,q,j,i)
term(172) = term(172) + s1(p,i) * wm_interm_32_pt4(a,j) * wm_interm_5_pt4(a,q,j,i)
term(173) = term(173) + s1(p,i) * wm_interm_1_pt4(a,q,j,i) * wm_interm_35_pt4(a,j)
term(174) = term(174) + s1(p,i) * wm_interm_1_pt4(a,q,j,i) * wm_interm_32_pt4(a,j)
term(175) = term(175) + s1(a,i) * wm_interm_1_pt4(p,q,j,i) * wm_interm_35_pt4(a,j)
term(176) = term(176) + s1(a,i) * wm_interm_1_pt4(p,q,j,i) * wm_interm_32_pt4(a,j)
term(177) = term(177) + s1(a,i) * wm_interm_35_pt4(a,j) * wm_interm_5_pt4(p,q,j,i)
term(178) = term(178) + s1(a,i) * wm_interm_32_pt4(a,j) * wm_interm_5_pt4(p,q,j,i)
term(179) = term(179) + s1(p,i) * wm_interm_11_pt4(a,q,j,i) * wm_interm_35_pt4(a,j)
term(180) = term(180) + s1(p,i) * wm_interm_11_pt4(a,q,j,i) * wm_interm_32_pt4(a,j)
term(181) = term(181) + s1(p,i) * wm_interm_12_pt4(a,q,j,i) * wm_interm_35_pt4(a,j)
term(182) = term(182) + s1(p,i) * wm_interm_12_pt4(a,q,j,i) * wm_interm_32_pt4(a,j)
term(183) = term(183) + s1(a,i) * wm_interm_11_pt4(p,q,j,i) * wm_interm_35_pt4(a,j)
term(184) = term(184) + s1(a,i) * wm_interm_11_pt4(p,q,j,i) * wm_interm_32_pt4(a,j)
term(185) = term(185) + s1(a,i) * wm_interm_12_pt4(p,q,j,i) * wm_interm_35_pt4(a,j)
term(186) = term(186) + s1(a,i) * wm_interm_12_pt4(p,q,j,i) * wm_interm_32_pt4(a,j)
term(187) = term(187) + t2(a,q,j,i) * wm_interm_44_pt4(p,i) * wm_interm_4_pt4(a,j)
term(188) = term(188) + t2(a,q,j,i) * wm_interm_2_pt4(a,j) * wm_interm_44_pt4(p,i)
term(189) = term(189) + t2(a,q,j,i) * wm_interm_45_pt4(p,i) * wm_interm_4_pt4(a,j)
term(190) = term(190) + t2(a,q,j,i) * wm_interm_2_pt4(a,j) * wm_interm_45_pt4(p,i)
term(191) = term(191) + t2(a,q,j,i) * wm_interm_44_pt4(a,j) * wm_interm_4_pt4(p,i)
term(192) = term(192) + t2(a,q,j,i) * wm_interm_2_pt4(p,i) * wm_interm_44_pt4(a,j)
term(193) = term(193) + t2(a,q,j,i) * wm_interm_45_pt4(a,j) * wm_interm_4_pt4(p,i)
term(194) = term(194) + t2(a,q,j,i) * wm_interm_2_pt4(p,i) * wm_interm_45_pt4(a,j)
end do 
end do 
end do 

term(143) = term(143) * (16.0d+0) 
term(144) = term(144) * (-8.0d+0) 
term(145) = term(145) * (-8.0d+0) 
term(146) = term(146) * (4.0d+0) 
term(147) = term(147) * (4.0d+0) 
term(148) = term(148) * (-8.0d+0) 
term(149) = term(149) * (4.0d+0) 
term(150) = term(150) * (-8.0d+0) 
term(151) = term(151) * (-8.0d+0) 
term(152) = term(152) * (4.0d+0) 
term(153) = term(153) * (-8.0d+0) 
term(154) = term(154) * (16.0d+0) 
term(155) = term(155) * (-2.0d+0) 
term(156) = term(156) * (4.0d+0) 
term(157) = term(157) * (-2.0d+0) 
term(158) = term(158) * (4.0d+0) 
term(159) = term(159) * (4.0d+0) 
term(160) = term(160) * (-2.0d+0) 
term(161) = term(161) * (4.0d+0) 
term(162) = term(162) * (-8.0d+0) 
term(163) = term(163) * (-8.0d+0) 
term(164) = term(164) * (4.0d+0) 
term(165) = term(165) * (16.0d+0) 
term(166) = term(166) * (-8.0d+0) 
term(167) = term(167) * (8.0d+0) 
term(168) = term(168) * (-16.0d+0) 
term(169) = term(169) * (-16.0d+0) 
term(170) = term(170) * (32.0d+0) 
term(171) = term(171) * (-4.0d+0) 
term(172) = term(172) * (8.0d+0) 
term(173) = term(173) * (8.0d+0) 
term(174) = term(174) * (-16.0d+0) 
term(175) = term(175) * (-4.0d+0) 
term(176) = term(176) * (8.0d+0) 
term(177) = term(177) * (8.0d+0) 
term(178) = term(178) * (-16.0d+0) 
term(179) = term(179) * (8.0d+0) 
term(180) = term(180) * (-16.0d+0) 
term(181) = term(181) * (-16.0d+0) 
term(182) = term(182) * (32.0d+0) 
term(183) = term(183) * (-4.0d+0) 
term(184) = term(184) * (8.0d+0) 
term(185) = term(185) * (8.0d+0) 
term(186) = term(186) * (-16.0d+0) 
term(187) = term(187) * (8.0d+0) 
term(188) = term(188) * (-16.0d+0) 
term(189) = term(189) * (-4.0d+0) 
term(190) = term(190) * (8.0d+0) 
term(191) = term(191) * (8.0d+0) 
term(192) = term(192) * (-16.0d+0) 
term(193) = term(193) * (-4.0d+0) 
term(194) = term(194) * (8.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(195) = term(195) + r1(vrdav_Rl, q,i) * wm_interm_3_pt4(a,j,k,i) * wm_interm_59_pt4(p,a,j,k)
term(196) = term(196) + r1(vrdav_Rl, q,i) * wm_interm_3_pt4(a,j,k,i) * wm_interm_60_pt4(p,a,j,k)
term(197) = term(197) + r1(vrdav_Rl, q,i) * wm_interm_3_pt4(a,j,k,i) * wm_interm_61_pt4(p,a,j,k)
term(198) = term(198) + r1(vrdav_Rl, q,i) * wm_interm_3_pt4(a,j,k,i) * wm_interm_62_pt4(p,a,j,k)
term(199) = term(199) + s1(a,i) * wm_interm_1_pt4(q,a,j,k) * wm_interm_26_pt4(p,k,j,i)
term(200) = term(200) + s1(a,i) * wm_interm_11_pt4(q,a,j,k) * wm_interm_26_pt4(p,k,j,i)
term(201) = term(201) + s1(a,i) * wm_interm_12_pt4(q,a,j,k) * wm_interm_26_pt4(p,k,j,i)
term(202) = term(202) + s1(q,i) * wm_interm_14_pt4(a,j,k,i) * wm_interm_59_pt4(p,a,j,k)
term(203) = term(203) + s1(q,i) * wm_interm_14_pt4(a,j,k,i) * wm_interm_60_pt4(p,a,j,k)
term(204) = term(204) + s1(q,i) * wm_interm_14_pt4(a,j,k,i) * wm_interm_61_pt4(p,a,j,k)
term(205) = term(205) + s1(q,i) * wm_interm_14_pt4(a,j,k,i) * wm_interm_62_pt4(p,a,j,k)
term(206) = term(206) + s2(a,q,j,i) * wm_interm_15_pt4(a,k) * wm_interm_28_pt4(p,j,i,k)
term(207) = term(207) + s2(a,q,j,i) * wm_interm_15_pt4(a,k) * wm_interm_28_pt4(p,i,j,k)
term(208) = term(208) + s2(a,q,j,i) * wm_interm_15_pt4(a,k) * wm_interm_29_pt4(p,i,j,k)
term(209) = term(209) + s2(a,q,j,i) * wm_interm_15_pt4(a,k) * wm_interm_29_pt4(p,j,i,k)
term(210) = term(210) + s2(a,q,j,i) * wm_interm_17_pt4(a,k) * wm_interm_28_pt4(p,j,i,k)
term(211) = term(211) + s2(a,q,j,i) * wm_interm_17_pt4(a,k) * wm_interm_28_pt4(p,i,j,k)
term(212) = term(212) + s2(a,q,j,i) * wm_interm_17_pt4(a,k) * wm_interm_29_pt4(p,i,j,k)
term(213) = term(213) + s2(a,q,j,i) * wm_interm_17_pt4(a,k) * wm_interm_29_pt4(p,j,i,k)
term(214) = term(214) + r1(vrdav_Rl, a,i) * wm_interm_3_pt4(q,j,k,i) * wm_interm_59_pt4(a,p,j,k)
term(215) = term(215) + r1(vrdav_Rl, a,i) * wm_interm_3_pt4(q,j,k,i) * wm_interm_60_pt4(a,p,j,k)
term(216) = term(216) + r1(vrdav_Rl, a,i) * wm_interm_3_pt4(q,j,k,i) * wm_interm_61_pt4(a,p,j,k)
term(217) = term(217) + r1(vrdav_Rl, a,i) * wm_interm_3_pt4(q,j,k,i) * wm_interm_62_pt4(a,p,j,k)
term(218) = term(218) + s1(p,i) * wm_interm_11_pt4(a,q,j,k) * wm_interm_26_pt4(a,k,j,i)
term(219) = term(219) + s1(p,i) * wm_interm_1_pt4(a,q,j,k) * wm_interm_26_pt4(a,k,j,i)
term(220) = term(220) + s1(p,i) * wm_interm_12_pt4(a,q,j,k) * wm_interm_26_pt4(a,k,j,i)
term(221) = term(221) + s1(a,i) * wm_interm_14_pt4(q,j,k,i) * wm_interm_59_pt4(a,p,j,k)
term(222) = term(222) + s1(a,i) * wm_interm_14_pt4(q,j,k,i) * wm_interm_60_pt4(a,p,j,k)
term(223) = term(223) + s1(a,i) * wm_interm_14_pt4(q,j,k,i) * wm_interm_61_pt4(a,p,j,k)
term(224) = term(224) + s1(a,i) * wm_interm_14_pt4(q,j,k,i) * wm_interm_62_pt4(a,p,j,k)
term(225) = term(225) + t1(q,i) * wm_interm_13_pt4(a,j,k,i) * wm_interm_48_pt4(p,a,j,k)
term(226) = term(226) + t1(q,i) * wm_interm_13_pt4(a,j,k,i) * wm_interm_49_pt4(p,a,j,k)
term(227) = term(227) + t1(q,i) * wm_interm_13_pt4(a,j,k,i) * wm_interm_50_pt4(p,a,j,k)
term(228) = term(228) + t1(q,i) * wm_interm_13_pt4(a,j,k,i) * wm_interm_53_pt4(p,a,j,k)
term(229) = term(229) + t2(a,q,j,i) * wm_interm_23_pt4(p,i,j,k) * wm_interm_38_pt4(a,k)
term(230) = term(230) + t2(a,q,j,i) * wm_interm_23_pt4(p,j,i,k) * wm_interm_38_pt4(a,k)
term(231) = term(231) + t2(a,q,j,i) * wm_interm_23_pt4(p,j,i,k) * wm_interm_41_pt4(a,k)
term(232) = term(232) + t2(a,q,j,i) * wm_interm_23_pt4(p,i,j,k) * wm_interm_41_pt4(a,k)
term(233) = term(233) + t1(a,i) * wm_interm_13_pt4(p,j,k,i) * wm_interm_49_pt4(a,q,j,k)
term(234) = term(234) + t1(a,i) * wm_interm_13_pt4(p,j,k,i) * wm_interm_53_pt4(a,q,j,k)
term(235) = term(235) + t1(a,i) * wm_interm_13_pt4(p,j,k,i) * wm_interm_50_pt4(a,q,j,k)
term(236) = term(236) + t1(a,i) * wm_interm_13_pt4(p,j,k,i) * wm_interm_48_pt4(a,q,j,k)
term(237) = term(237) + t2(a,q,j,i) * wm_interm_2_pt4(a,k) * wm_interm_42_pt4(p,j,i,k)
term(238) = term(238) + t2(a,q,j,i) * wm_interm_42_pt4(p,j,i,k) * wm_interm_4_pt4(a,k)
term(239) = term(239) + t2(a,q,j,i) * wm_interm_2_pt4(a,k) * wm_interm_42_pt4(p,i,j,k)
term(240) = term(240) + t2(a,q,j,i) * wm_interm_42_pt4(p,i,j,k) * wm_interm_4_pt4(a,k)
term(241) = term(241) + t2(a,q,j,i) * wm_interm_2_pt4(a,k) * wm_interm_43_pt4(p,i,j,k)
term(242) = term(242) + t2(a,q,j,i) * wm_interm_2_pt4(a,k) * wm_interm_43_pt4(p,j,i,k)
term(243) = term(243) + t2(a,q,j,i) * wm_interm_43_pt4(p,j,i,k) * wm_interm_4_pt4(a,k)
term(244) = term(244) + t2(a,q,j,i) * wm_interm_43_pt4(p,i,j,k) * wm_interm_4_pt4(a,k)
end do 
end do 
end do 
end do 

term(195) = term(195) * (-4.0d+0) 
term(196) = term(196) * (8.0d+0) 
term(197) = term(197) * (-4.0d+0) 
term(198) = term(198) * (8.0d+0) 
term(199) = term(199) * (-4.0d+0) 
term(200) = term(200) * (-4.0d+0) 
term(201) = term(201) * (8.0d+0) 
term(202) = term(202) * (-4.0d+0) 
term(203) = term(203) * (8.0d+0) 
term(204) = term(204) * (-4.0d+0) 
term(205) = term(205) * (8.0d+0) 
term(206) = term(206) * (4.0d+0) 
term(207) = term(207) * (-8.0d+0) 
term(208) = term(208) * (4.0d+0) 
term(209) = term(209) * (-8.0d+0) 
term(210) = term(210) * (-2.0d+0) 
term(211) = term(211) * (4.0d+0) 
term(212) = term(212) * (-2.0d+0) 
term(213) = term(213) * (4.0d+0) 
term(214) = term(214) * (-4.0d+0) 
term(215) = term(215) * (8.0d+0) 
term(216) = term(216) * (-4.0d+0) 
term(217) = term(217) * (8.0d+0) 
term(218) = term(218) * (-4.0d+0) 
term(219) = term(219) * (-4.0d+0) 
term(220) = term(220) * (8.0d+0) 
term(221) = term(221) * (-4.0d+0) 
term(222) = term(222) * (8.0d+0) 
term(223) = term(223) * (-4.0d+0) 
term(224) = term(224) * (8.0d+0) 
term(225) = term(225) * (-4.0d+0) 
term(226) = term(226) * (2.0d+0) 
term(227) = term(227) * (-4.0d+0) 
term(228) = term(228) * (2.0d+0) 
term(229) = term(229) * (-4.0d+0) 
term(230) = term(230) * (2.0d+0) 
term(231) = term(231) * (-4.0d+0) 
term(232) = term(232) * (8.0d+0) 
term(233) = term(233) * (2.0d+0) 
term(234) = term(234) * (2.0d+0) 
term(235) = term(235) * (-4.0d+0) 
term(236) = term(236) * (-4.0d+0) 
term(237) = term(237) * (-2.0d+0) 
term(239) = term(239) * (4.0d+0) 
term(240) = term(240) * (-2.0d+0) 
term(241) = term(241) * (-2.0d+0) 
term(242) = term(242) * (4.0d+0) 
term(243) = term(243) * (-2.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(245) = term(245) + r1(vrdav_Rl, p,i) * wm_interm_3_pt4(q,j,i,k) * wm_interm_70_pt4(j,k)
term(246) = term(246) + r1(vrdav_Rl, p,i) * wm_interm_3_pt4(q,j,i,k) * wm_interm_71_pt4(j,k)
term(247) = term(247) + s1(p,i) * wm_interm_14_pt4(q,j,i,k) * wm_interm_70_pt4(j,k)
term(248) = term(248) + s1(p,i) * wm_interm_14_pt4(q,j,i,k) * wm_interm_71_pt4(j,k)
term(249) = term(249) + r1(vrdav_Rr, p,i) * wm_interm_23_pt4(q,j,i,k) * wm_interm_56_pt4(k,j)
term(250) = term(250) + r1(vrdav_Rr, p,i) * wm_interm_23_pt4(q,j,i,k) * wm_interm_54_pt4(k,j)
term(251) = term(251) + t1(q,i) * wm_interm_13_pt4(p,j,i,k) * wm_interm_54_pt4(j,k)
term(252) = term(252) + t1(q,i) * wm_interm_13_pt4(p,j,i,k) * wm_interm_56_pt4(j,k)
end do 
end do 
end do 

term(245) = term(245) * (-4.0d+0) 
term(246) = term(246) * (8.0d+0) 
term(247) = term(247) * (-4.0d+0) 
term(248) = term(248) * (8.0d+0) 
term(249) = term(249) * (-8.0d+0) 
term(250) = term(250) * (4.0d+0) 
term(251) = term(251) * (2.0d+0) 
term(252) = term(252) * (-4.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(253) = term(253) + r2(vrdav_Rr, a,j,p,i) * s1(b,j) * s2(a,q,l,k) * wm_interm_14_pt4(b,i,l,k)
term(254) = term(254) + r2(vrdav_Rr, a,j,p,i) * s1(b,i) * s2(a,q,l,k) * wm_interm_14_pt4(b,j,l,k)
term(255) = term(255) + s1(a,j) * s2(b,p,l,k) * t2(b,q,j,i) * wm_interm_26_pt4(a,i,l,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(253) = term(253) * (-4.0d+0) 
term(254) = term(254) * (8.0d+0) 
term(255) = term(255) * (-4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(256) = term(256) + s1(a,i) * wm_interm_26_pt4(p,j,i,k) * wm_interm_5_pt4(q,a,k,j)
term(257) = term(257) + s1(a,i) * wm_interm_1_pt4(q,a,j,k) * wm_interm_26_pt4(p,k,i,j)
term(258) = term(258) + r1(vrdav_Rl, q,i) * wm_interm_3_pt4(a,j,i,k) * wm_interm_62_pt4(p,a,j,k)
term(259) = term(259) + r1(vrdav_Rl, q,i) * wm_interm_3_pt4(a,j,i,k) * wm_interm_61_pt4(p,a,j,k)
term(260) = term(260) + r1(vrdav_Rl, q,i) * wm_interm_3_pt4(a,j,i,k) * wm_interm_59_pt4(p,a,j,k)
term(261) = term(261) + r1(vrdav_Rl, q,i) * wm_interm_3_pt4(a,j,i,k) * wm_interm_60_pt4(p,a,j,k)
term(262) = term(262) + s1(a,i) * wm_interm_11_pt4(q,a,j,k) * wm_interm_26_pt4(p,k,i,j)
term(263) = term(263) + s1(a,i) * wm_interm_12_pt4(q,a,j,k) * wm_interm_26_pt4(p,k,i,j)
term(264) = term(264) + s1(q,i) * wm_interm_14_pt4(a,j,i,k) * wm_interm_62_pt4(p,a,j,k)
term(265) = term(265) + s1(q,i) * wm_interm_14_pt4(a,j,i,k) * wm_interm_61_pt4(p,a,j,k)
term(266) = term(266) + s1(q,i) * wm_interm_14_pt4(a,j,i,k) * wm_interm_59_pt4(p,a,j,k)
term(267) = term(267) + s1(q,i) * wm_interm_14_pt4(a,j,i,k) * wm_interm_60_pt4(p,a,j,k)
term(268) = term(268) + r1(vrdav_Rl, a,i) * wm_interm_3_pt4(q,j,i,k) * wm_interm_62_pt4(a,p,j,k)
term(269) = term(269) + r1(vrdav_Rl, a,i) * wm_interm_3_pt4(q,j,i,k) * wm_interm_61_pt4(a,p,j,k)
term(270) = term(270) + r1(vrdav_Rl, a,i) * wm_interm_3_pt4(q,j,i,k) * wm_interm_59_pt4(a,p,j,k)
term(271) = term(271) + r1(vrdav_Rl, a,i) * wm_interm_3_pt4(q,j,i,k) * wm_interm_60_pt4(a,p,j,k)
term(272) = term(272) + r1(vrdav_Rl, p,i) * wm_interm_1_pt4(a,q,j,k) * wm_interm_28_pt4(a,i,j,k)
term(273) = term(273) + r1(vrdav_Rl, p,i) * wm_interm_1_pt4(a,q,j,k) * wm_interm_29_pt4(a,i,j,k)
term(274) = term(274) + r1(vrdav_Rl, p,i) * wm_interm_29_pt4(a,i,j,k) * wm_interm_5_pt4(a,q,j,k)
term(275) = term(275) + r1(vrdav_Rl, p,i) * wm_interm_28_pt4(a,i,j,k) * wm_interm_5_pt4(a,q,j,k)
term(276) = term(276) + s1(a,i) * wm_interm_14_pt4(q,j,i,k) * wm_interm_62_pt4(a,p,j,k)
term(277) = term(277) + s1(a,i) * wm_interm_14_pt4(q,j,i,k) * wm_interm_61_pt4(a,p,j,k)
term(278) = term(278) + s1(a,i) * wm_interm_14_pt4(q,j,i,k) * wm_interm_59_pt4(a,p,j,k)
term(279) = term(279) + s1(a,i) * wm_interm_14_pt4(q,j,i,k) * wm_interm_60_pt4(a,p,j,k)
term(280) = term(280) + s1(p,i) * wm_interm_11_pt4(a,q,j,k) * wm_interm_26_pt4(a,k,i,j)
term(281) = term(281) + s1(p,i) * wm_interm_26_pt4(a,j,i,k) * wm_interm_5_pt4(a,q,k,j)
term(282) = term(282) + s1(p,i) * wm_interm_1_pt4(a,q,j,k) * wm_interm_26_pt4(a,k,i,j)
term(283) = term(283) + s1(p,i) * wm_interm_12_pt4(a,q,j,k) * wm_interm_26_pt4(a,k,i,j)
term(284) = term(284) + r1(vrdav_Rl, p,i) * wm_interm_11_pt4(a,q,j,k) * wm_interm_28_pt4(a,i,j,k)
term(285) = term(285) + r1(vrdav_Rl, p,i) * wm_interm_11_pt4(a,q,j,k) * wm_interm_29_pt4(a,i,j,k)
term(286) = term(286) + r1(vrdav_Rl, p,i) * wm_interm_12_pt4(a,q,j,k) * wm_interm_28_pt4(a,i,j,k)
term(287) = term(287) + r1(vrdav_Rl, p,i) * wm_interm_12_pt4(a,q,j,k) * wm_interm_29_pt4(a,i,j,k)
term(288) = term(288) + r1(vrdav_Rr, p,i) * wm_interm_49_pt4(q,a,j,k) * wm_interm_76_pt4(a,k,i,j)
term(289) = term(289) + r1(vrdav_Rr, p,i) * wm_interm_48_pt4(q,a,j,k) * wm_interm_76_pt4(a,k,i,j)
term(290) = term(290) + r1(vrdav_Rr, p,i) * wm_interm_23_pt4(a,j,i,k) * wm_interm_50_pt4(q,a,k,j)
term(291) = term(291) + r1(vrdav_Rr, p,i) * wm_interm_23_pt4(a,j,i,k) * wm_interm_53_pt4(q,a,k,j)
term(292) = term(292) + r1(vrdav_Rr, p,i) * wm_interm_11_pt4(q,a,j,k) * wm_interm_42_pt4(a,k,i,j)
term(293) = term(293) + r1(vrdav_Rr, p,i) * wm_interm_12_pt4(q,a,j,k) * wm_interm_42_pt4(a,k,i,j)
term(294) = term(294) + r1(vrdav_Rr, p,i) * wm_interm_42_pt4(a,j,i,k) * wm_interm_5_pt4(q,a,k,j)
term(295) = term(295) + r1(vrdav_Rr, p,i) * wm_interm_1_pt4(q,a,j,k) * wm_interm_42_pt4(a,k,i,j)
term(296) = term(296) + r1(vrdav_Rr, p,i) * wm_interm_50_pt4(q,a,j,k) * wm_interm_76_pt4(a,k,i,j)
term(297) = term(297) + r1(vrdav_Rr, p,i) * wm_interm_53_pt4(q,a,j,k) * wm_interm_76_pt4(a,k,i,j)
term(298) = term(298) + r1(vrdav_Rr, p,i) * wm_interm_11_pt4(q,a,j,k) * wm_interm_43_pt4(a,k,i,j)
term(299) = term(299) + r1(vrdav_Rr, p,i) * wm_interm_12_pt4(q,a,j,k) * wm_interm_43_pt4(a,k,i,j)
term(300) = term(300) + r1(vrdav_Rr, p,i) * wm_interm_1_pt4(q,a,j,k) * wm_interm_43_pt4(a,k,i,j)
term(301) = term(301) + r1(vrdav_Rr, p,i) * wm_interm_43_pt4(a,j,i,k) * wm_interm_5_pt4(q,a,k,j)
term(302) = term(302) + t1(q,i) * wm_interm_13_pt4(a,j,i,k) * wm_interm_48_pt4(p,a,j,k)
term(303) = term(303) + t1(q,i) * wm_interm_13_pt4(a,j,i,k) * wm_interm_49_pt4(p,a,j,k)
term(304) = term(304) + t1(q,i) * wm_interm_11_pt4(p,a,j,k) * wm_interm_40_pt4(a,k,i,j)
term(305) = term(305) + t1(q,i) * wm_interm_12_pt4(p,a,j,k) * wm_interm_40_pt4(a,k,i,j)
term(306) = term(306) + t1(q,i) * wm_interm_11_pt4(p,a,j,k) * wm_interm_39_pt4(a,k,i,j)
term(307) = term(307) + t1(q,i) * wm_interm_12_pt4(p,a,j,k) * wm_interm_39_pt4(a,k,i,j)
term(308) = term(308) + t1(q,i) * wm_interm_13_pt4(a,j,i,k) * wm_interm_50_pt4(p,a,j,k)
term(309) = term(309) + t1(q,i) * wm_interm_13_pt4(a,j,i,k) * wm_interm_53_pt4(p,a,j,k)
term(310) = term(310) + t1(q,i) * wm_interm_40_pt4(a,j,i,k) * wm_interm_5_pt4(p,a,k,j)
term(311) = term(311) + t1(q,i) * wm_interm_1_pt4(p,a,j,k) * wm_interm_40_pt4(a,k,i,j)
term(312) = term(312) + t1(q,i) * wm_interm_39_pt4(a,j,i,k) * wm_interm_5_pt4(p,a,k,j)
term(313) = term(313) + t1(q,i) * wm_interm_1_pt4(p,a,j,k) * wm_interm_39_pt4(a,k,i,j)
term(314) = term(314) + r1(vrdav_Rr, a,i) * wm_interm_23_pt4(p,i,j,k) * wm_interm_53_pt4(a,q,k,j)
term(315) = term(315) + r1(vrdav_Rr, a,i) * wm_interm_23_pt4(p,i,j,k) * wm_interm_48_pt4(a,q,k,j)
term(316) = term(316) + r1(vrdav_Rr, a,i) * wm_interm_23_pt4(p,j,i,k) * wm_interm_53_pt4(a,q,k,j)
term(317) = term(317) + r1(vrdav_Rr, a,i) * wm_interm_23_pt4(p,j,i,k) * wm_interm_48_pt4(a,q,k,j)
term(318) = term(318) + t1(a,i) * wm_interm_39_pt4(p,j,i,k) * wm_interm_5_pt4(a,q,k,j)
term(319) = term(319) + t1(a,i) * wm_interm_1_pt4(a,q,j,k) * wm_interm_39_pt4(p,k,i,j)
term(320) = term(320) + t1(a,i) * wm_interm_1_pt4(a,q,j,k) * wm_interm_40_pt4(p,k,i,j)
term(321) = term(321) + t1(a,i) * wm_interm_40_pt4(p,j,i,k) * wm_interm_5_pt4(a,q,k,j)
term(322) = term(322) + r1(vrdav_Rr, a,i) * wm_interm_23_pt4(p,i,j,k) * wm_interm_49_pt4(a,q,k,j)
term(323) = term(323) + r1(vrdav_Rr, a,i) * wm_interm_23_pt4(p,j,i,k) * wm_interm_49_pt4(a,q,k,j)
term(324) = term(324) + t1(a,i) * wm_interm_11_pt4(a,q,j,k) * wm_interm_39_pt4(p,k,i,j)
term(325) = term(325) + t1(a,i) * wm_interm_12_pt4(a,q,j,k) * wm_interm_39_pt4(p,k,i,j)
term(326) = term(326) + r1(vrdav_Rr, a,i) * wm_interm_23_pt4(p,j,i,k) * wm_interm_50_pt4(a,q,k,j)
term(327) = term(327) + r1(vrdav_Rr, a,i) * wm_interm_23_pt4(p,i,j,k) * wm_interm_50_pt4(a,q,k,j)
term(328) = term(328) + t1(a,i) * wm_interm_11_pt4(a,q,j,k) * wm_interm_40_pt4(p,k,i,j)
term(329) = term(329) + t1(a,i) * wm_interm_12_pt4(a,q,j,k) * wm_interm_40_pt4(p,k,i,j)
term(330) = term(330) + t1(a,i) * wm_interm_13_pt4(p,j,i,k) * wm_interm_49_pt4(a,q,j,k)
term(331) = term(331) + t1(a,i) * wm_interm_13_pt4(p,j,i,k) * wm_interm_50_pt4(a,q,j,k)
term(332) = term(332) + t1(a,i) * wm_interm_13_pt4(p,j,i,k) * wm_interm_53_pt4(a,q,j,k)
term(333) = term(333) + t1(a,i) * wm_interm_13_pt4(p,j,i,k) * wm_interm_48_pt4(a,q,j,k)
term(334) = term(334) + r1(vrdav_Rr, a,i) * wm_interm_42_pt4(p,j,i,k) * wm_interm_5_pt4(a,q,k,j)
term(335) = term(335) + r1(vrdav_Rr, a,i) * wm_interm_1_pt4(a,q,j,k) * wm_interm_42_pt4(p,k,i,j)
term(336) = term(336) + r1(vrdav_Rr, a,i) * wm_interm_11_pt4(a,q,j,k) * wm_interm_42_pt4(p,i,k,j)
term(337) = term(337) + r1(vrdav_Rr, a,i) * wm_interm_11_pt4(a,q,j,k) * wm_interm_42_pt4(p,k,i,j)
term(338) = term(338) + r1(vrdav_Rr, a,i) * wm_interm_42_pt4(p,i,j,k) * wm_interm_5_pt4(a,q,k,j)
term(339) = term(339) + r1(vrdav_Rr, a,i) * wm_interm_1_pt4(a,q,j,k) * wm_interm_42_pt4(p,i,k,j)
term(340) = term(340) + r1(vrdav_Rr, a,i) * wm_interm_12_pt4(a,q,j,k) * wm_interm_42_pt4(p,i,k,j)
term(341) = term(341) + r1(vrdav_Rr, a,i) * wm_interm_12_pt4(a,q,j,k) * wm_interm_42_pt4(p,k,i,j)
term(342) = term(342) + t1(a,i) * wm_interm_11_pt4(a,q,j,k) * wm_interm_39_pt4(p,i,k,j)
term(343) = term(343) + t1(a,i) * wm_interm_11_pt4(a,q,j,k) * wm_interm_40_pt4(p,i,k,j)
term(344) = term(344) + t1(a,i) * wm_interm_1_pt4(a,q,j,k) * wm_interm_39_pt4(p,i,k,j)
term(345) = term(345) + t1(a,i) * wm_interm_39_pt4(p,i,j,k) * wm_interm_5_pt4(a,q,k,j)
term(346) = term(346) + t1(a,i) * wm_interm_12_pt4(a,q,j,k) * wm_interm_39_pt4(p,i,k,j)
term(347) = term(347) + t1(a,i) * wm_interm_40_pt4(p,i,j,k) * wm_interm_5_pt4(a,q,k,j)
term(348) = term(348) + t1(a,i) * wm_interm_1_pt4(a,q,j,k) * wm_interm_40_pt4(p,i,k,j)
term(349) = term(349) + t1(a,i) * wm_interm_12_pt4(a,q,j,k) * wm_interm_40_pt4(p,i,k,j)
term(350) = term(350) + r1(vrdav_Rr, a,i) * wm_interm_43_pt4(p,i,j,k) * wm_interm_5_pt4(a,q,k,j)
term(351) = term(351) + r1(vrdav_Rr, a,i) * wm_interm_11_pt4(a,q,j,k) * wm_interm_43_pt4(p,k,i,j)
term(352) = term(352) + r1(vrdav_Rr, a,i) * wm_interm_11_pt4(a,q,j,k) * wm_interm_43_pt4(p,i,k,j)
term(353) = term(353) + r1(vrdav_Rr, a,i) * wm_interm_43_pt4(p,j,i,k) * wm_interm_5_pt4(a,q,k,j)
term(354) = term(354) + r1(vrdav_Rr, a,i) * wm_interm_1_pt4(a,q,j,k) * wm_interm_43_pt4(p,k,i,j)
term(355) = term(355) + r1(vrdav_Rr, a,i) * wm_interm_12_pt4(a,q,j,k) * wm_interm_43_pt4(p,k,i,j)
term(356) = term(356) + r1(vrdav_Rr, a,i) * wm_interm_1_pt4(a,q,j,k) * wm_interm_43_pt4(p,i,k,j)
term(357) = term(357) + r1(vrdav_Rr, a,i) * wm_interm_12_pt4(a,q,j,k) * wm_interm_43_pt4(p,i,k,j)
end do 
end do 
end do 
end do 

term(256) = term(256) * (-4.0d+0) 
term(257) = term(257) * (8.0d+0) 
term(258) = term(258) * (-4.0d+0) 
term(259) = term(259) * (8.0d+0) 
term(260) = term(260) * (8.0d+0) 
term(261) = term(261) * (-16.0d+0) 
term(262) = term(262) * (8.0d+0) 
term(263) = term(263) * (-16.0d+0) 
term(264) = term(264) * (-4.0d+0) 
term(265) = term(265) * (8.0d+0) 
term(266) = term(266) * (8.0d+0) 
term(267) = term(267) * (-16.0d+0) 
term(268) = term(268) * (-4.0d+0) 
term(269) = term(269) * (8.0d+0) 
term(270) = term(270) * (8.0d+0) 
term(271) = term(271) * (-16.0d+0) 
term(272) = term(272) * (-4.0d+0) 
term(273) = term(273) * (8.0d+0) 
term(274) = term(274) * (-4.0d+0) 
term(275) = term(275) * (8.0d+0) 
term(276) = term(276) * (-4.0d+0) 
term(277) = term(277) * (8.0d+0) 
term(278) = term(278) * (8.0d+0) 
term(279) = term(279) * (-16.0d+0) 
term(280) = term(280) * (8.0d+0) 
term(281) = term(281) * (-4.0d+0) 
term(282) = term(282) * (8.0d+0) 
term(283) = term(283) * (-16.0d+0) 
term(284) = term(284) * (-4.0d+0) 
term(285) = term(285) * (8.0d+0) 
term(286) = term(286) * (8.0d+0) 
term(287) = term(287) * (-16.0d+0) 
term(288) = term(288) * (4.0d+0) 
term(289) = term(289) * (-8.0d+0) 
term(290) = term(290) * (2.0d+0) 
term(291) = term(291) * (-4.0d+0) 
term(292) = term(292) * (-4.0d+0) 
term(293) = term(293) * (8.0d+0) 
term(294) = term(294) * (2.0d+0) 
term(295) = term(295) * (-4.0d+0) 
term(296) = term(296) * (-4.0d+0) 
term(297) = term(297) * (2.0d+0) 
term(298) = term(298) * (2.0d+0) 
term(299) = term(299) * (-4.0d+0) 
term(300) = term(300) * (2.0d+0) 
term(301) = term(301) * (-4.0d+0) 
term(302) = term(302) * (8.0d+0) 
term(303) = term(303) * (-4.0d+0) 
term(305) = term(305) * (-2.0d+0) 
term(306) = term(306) * (-2.0d+0) 
term(307) = term(307) * (4.0d+0) 
term(308) = term(308) * (2.0d+0) 
term(309) = term(309) * (-4.0d+0) 
term(310) = term(310) * (-2.0d+0) 
term(313) = term(313) * (-2.0d+0) 
term(314) = term(314) * (2.0d+0) 
term(315) = term(315) * (-4.0d+0) 
term(316) = term(316) * (-4.0d+0) 
term(317) = term(317) * (8.0d+0) 
term(319) = term(319) * (-2.0d+0) 
term(321) = term(321) * (-2.0d+0) 
term(322) = term(322) * (2.0d+0) 
term(323) = term(323) * (-4.0d+0) 
term(324) = term(324) * (-2.0d+0) 
term(325) = term(325) * (4.0d+0) 
term(326) = term(326) * (2.0d+0) 
term(327) = term(327) * (-4.0d+0) 
term(329) = term(329) * (-2.0d+0) 
term(330) = term(330) * (-4.0d+0) 
term(331) = term(331) * (2.0d+0) 
term(332) = term(332) * (-4.0d+0) 
term(333) = term(333) * (8.0d+0) 
term(335) = term(335) * (-2.0d+0) 
term(337) = term(337) * (-2.0d+0) 
term(338) = term(338) * (-2.0d+0) 
term(340) = term(340) * (-2.0d+0) 
term(341) = term(341) * (4.0d+0) 
term(343) = term(343) * (-2.0d+0) 
term(345) = term(345) * (-2.0d+0) 
term(346) = term(346) * (-2.0d+0) 
term(348) = term(348) * (-2.0d+0) 
term(349) = term(349) * (4.0d+0) 
term(352) = term(352) * (-2.0d+0) 
term(353) = term(353) * (-2.0d+0) 
term(355) = term(355) * (-2.0d+0) 
term(356) = term(356) * (-2.0d+0) 
term(357) = term(357) * (4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(358) = term(358) + s1(a,i) * wm_interm_26_pt4(p,j,k,i) * wm_interm_5_pt4(q,a,k,j)
term(359) = term(359) + s2(a,q,j,i) * wm_interm_26_pt4(p,k,i,j) * wm_interm_7_pt4(a,k)
term(360) = term(360) + s2(a,q,j,i) * wm_interm_26_pt4(p,k,i,j) * wm_interm_8_pt4(a,k)
term(361) = term(361) + s2(a,p,j,i) * wm_interm_14_pt4(q,k,i,j) * wm_interm_31_pt4(a,k)
term(362) = term(362) + s2(a,p,j,i) * wm_interm_14_pt4(q,k,i,j) * wm_interm_34_pt4(a,k)
term(363) = term(363) + s2(a,p,j,i) * wm_interm_35_pt4(a,k) * wm_interm_3_pt4(q,k,i,j)
term(364) = term(364) + s2(a,p,j,i) * wm_interm_32_pt4(a,k) * wm_interm_3_pt4(q,k,i,j)
term(365) = term(365) + s1(p,i) * wm_interm_26_pt4(a,j,k,i) * wm_interm_5_pt4(a,q,k,j)
term(366) = term(366) + t2(a,q,j,i) * wm_interm_13_pt4(p,k,i,j) * wm_interm_44_pt4(a,k)
term(367) = term(367) + t2(a,q,j,i) * wm_interm_13_pt4(p,k,i,j) * wm_interm_45_pt4(a,k)
end do 
end do 
end do 
end do 

term(358) = term(358) * (8.0d+0) 
term(359) = term(359) * (8.0d+0) 
term(360) = term(360) * (-4.0d+0) 
term(361) = term(361) * (4.0d+0) 
term(362) = term(362) * (-2.0d+0) 
term(363) = term(363) * (-4.0d+0) 
term(364) = term(364) * (8.0d+0) 
term(365) = term(365) * (8.0d+0) 
term(366) = term(366) * (-4.0d+0) 
term(367) = term(367) * (2.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(368) = term(368) + t2(a,q,j,i) * wm_interm_30_pt4(b,p,i,j) * wm_interm_68_pt4(b,a)
term(369) = term(369) + t2(a,q,j,i) * wm_interm_30_pt4(b,p,i,j) * wm_interm_69_pt4(b,a)
term(370) = term(370) + r2(vrdav_Rl, a,j,p,i) * wm_interm_20_pt4(b,a) * wm_interm_46_pt4(b,q,i,j)
term(371) = term(371) + r2(vrdav_Rl, a,j,p,i) * wm_interm_21_pt4(b,a) * wm_interm_46_pt4(b,q,i,j)
term(372) = term(372) + s2(a,p,j,i) * wm_interm_46_pt4(b,q,i,j) * wm_interm_51_pt4(b,a)
term(373) = term(373) + s2(a,p,j,i) * wm_interm_46_pt4(b,q,i,j) * wm_interm_52_pt4(b,a)
end do 
end do 
end do 
end do 

term(368) = term(368) * (-4.0d+0) 
term(369) = term(369) * (8.0d+0) 
term(370) = term(370) * (2.0d+0) 
term(371) = term(371) * (-4.0d+0) 
term(372) = term(372) * (-4.0d+0) 
term(373) = term(373) * (2.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(374) = term(374) + s1(a,i) * s2(b,p,k,j) * t2(b,q,l,i) * wm_interm_26_pt4(a,l,j,k)
term(375) = term(375) + s2(a,p,k,j) * t1(b,j) * t2(a,q,l,i) * wm_interm_40_pt4(b,l,i,k)
term(376) = term(376) + s2(a,p,k,j) * t1(b,j) * t2(a,q,l,i) * wm_interm_40_pt4(b,i,l,k)
term(377) = term(377) + s2(a,p,k,j) * t1(b,j) * t2(a,q,l,i) * wm_interm_39_pt4(b,l,i,k)
term(378) = term(378) + s2(a,p,k,j) * t1(b,j) * t2(a,q,l,i) * wm_interm_39_pt4(b,i,l,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(374) = term(374) * (-4.0d+0) 
term(375) = term(375) * (-2.0d+0) 
term(378) = term(378) * (-2.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(379) = term(379) + s1(a,i) * s2(b,p,k,j) * t2(b,q,l,i) * wm_interm_26_pt4(a,l,k,j)
end do 
end do 
end do 
end do 
end do 
end do 

term(379) = term(379) * (8.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(380) = term(380) + r1(vrdav_Rl, q,i) * wm_interm_10_pt4(j,k,i,l) * wm_interm_28_pt4(p,j,k,l)
end do 
end do 
end do 
end do 

term(380) = term(380) * (8.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(381) = term(381) + r2(vrdav_Rr, a,j,p,i) * s1(b,i) * s2(a,q,l,k) * wm_interm_14_pt4(b,j,k,l)
term(382) = term(382) + r2(vrdav_Rr, a,j,p,i) * s1(b,j) * s2(a,q,l,k) * wm_interm_14_pt4(b,i,k,l)
term(383) = term(383) + s1(a,j) * s2(b,p,l,k) * t2(b,q,j,i) * wm_interm_26_pt4(a,i,k,l)
end do 
end do 
end do 
end do 
end do 
end do 

term(381) = term(381) * (-4.0d+0) 
term(382) = term(382) * (8.0d+0) 
term(383) = term(383) * (8.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do j = 1, nocc 
term(384) = term(384) + s1(q,i) * wm_interm_10_pt4(j,k,l,i) * wm_interm_26_pt4(p,l,j,k)
end do 
end do 
end do 
end do 

term(384) = term(384) * (8.0d+0) 

do a = nocc + 1, nactive 
do i = 1, nocc 
term(385) = term(385) + r1(vrdav_Rl, a,i) * wm_interm_20_pt4(q,a) * wm_interm_31_pt4(p,i)
term(386) = term(386) + r1(vrdav_Rl, a,i) * wm_interm_21_pt4(q,a) * wm_interm_31_pt4(p,i)
term(387) = term(387) + r1(vrdav_Rl, a,i) * wm_interm_20_pt4(q,a) * wm_interm_34_pt4(p,i)
term(388) = term(388) + r1(vrdav_Rl, a,i) * wm_interm_21_pt4(q,a) * wm_interm_34_pt4(p,i)
term(389) = term(389) + r1(vrdav_Rl, q,i) * wm_interm_68_pt4(p,a) * wm_interm_7_pt4(a,i)
term(390) = term(390) + r1(vrdav_Rl, q,i) * wm_interm_69_pt4(p,a) * wm_interm_7_pt4(a,i)
term(391) = term(391) + r1(vrdav_Rl, q,i) * wm_interm_68_pt4(p,a) * wm_interm_8_pt4(a,i)
term(392) = term(392) + r1(vrdav_Rl, q,i) * wm_interm_69_pt4(p,a) * wm_interm_8_pt4(a,i)
term(393) = term(393) + s1(q,i) * wm_interm_15_pt4(a,i) * wm_interm_68_pt4(p,a)
term(394) = term(394) + s1(q,i) * wm_interm_15_pt4(a,i) * wm_interm_69_pt4(p,a)
term(395) = term(395) + s1(q,i) * wm_interm_17_pt4(a,i) * wm_interm_68_pt4(p,a)
term(396) = term(396) + s1(q,i) * wm_interm_17_pt4(a,i) * wm_interm_69_pt4(p,a)
term(397) = term(397) + r1(vrdav_Rr, p,i) * wm_interm_20_pt4(q,a) * wm_interm_44_pt4(a,i)
term(398) = term(398) + r1(vrdav_Rr, p,i) * wm_interm_21_pt4(q,a) * wm_interm_44_pt4(a,i)
term(399) = term(399) + r1(vrdav_Rr, p,i) * wm_interm_20_pt4(q,a) * wm_interm_45_pt4(a,i)
term(400) = term(400) + r1(vrdav_Rr, p,i) * wm_interm_21_pt4(q,a) * wm_interm_45_pt4(a,i)
term(401) = term(401) + t1(q,i) * wm_interm_20_pt4(p,a) * wm_interm_38_pt4(a,i)
term(402) = term(402) + t1(q,i) * wm_interm_21_pt4(p,a) * wm_interm_38_pt4(a,i)
term(403) = term(403) + t1(q,i) * wm_interm_20_pt4(p,a) * wm_interm_41_pt4(a,i)
term(404) = term(404) + t1(q,i) * wm_interm_21_pt4(p,a) * wm_interm_41_pt4(a,i)
term(405) = term(405) + t1(q,i) * wm_interm_4_pt4(a,i) * wm_interm_51_pt4(p,a)
term(406) = term(406) + t1(q,i) * wm_interm_2_pt4(a,i) * wm_interm_51_pt4(p,a)
term(407) = term(407) + t1(q,i) * wm_interm_4_pt4(a,i) * wm_interm_52_pt4(p,a)
term(408) = term(408) + t1(q,i) * wm_interm_2_pt4(a,i) * wm_interm_52_pt4(p,a)
end do 
end do 

term(385) = term(385) * (4.0d+0) 
term(386) = term(386) * (-8.0d+0) 
term(387) = term(387) * (-2.0d+0) 
term(388) = term(388) * (4.0d+0) 
term(389) = term(389) * (8.0d+0) 
term(390) = term(390) * (-16.0d+0) 
term(391) = term(391) * (-4.0d+0) 
term(392) = term(392) * (8.0d+0) 
term(393) = term(393) * (8.0d+0) 
term(394) = term(394) * (-16.0d+0) 
term(395) = term(395) * (-4.0d+0) 
term(396) = term(396) * (8.0d+0) 
term(397) = term(397) * (-8.0d+0) 
term(398) = term(398) * (16.0d+0) 
term(399) = term(399) * (4.0d+0) 
term(400) = term(400) * (-8.0d+0) 
term(401) = term(401) * (2.0d+0) 
term(402) = term(402) * (-4.0d+0) 
term(403) = term(403) * (-4.0d+0) 
term(404) = term(404) * (8.0d+0) 
term(405) = term(405) * (-4.0d+0) 
term(406) = term(406) * (8.0d+0) 
term(407) = term(407) * (2.0d+0) 
term(408) = term(408) * (-4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
term(409) = term(409) + s1(q,i) * wm_interm_18_pt4(j,k) * wm_interm_26_pt4(p,k,i,j)
term(410) = term(410) + s1(q,i) * wm_interm_19_pt4(j,k) * wm_interm_26_pt4(p,k,i,j)
term(411) = term(411) + r1(vrdav_Rr, p,i) * wm_interm_18_pt4(j,k) * wm_interm_42_pt4(q,k,i,j)
term(412) = term(412) + r1(vrdav_Rr, p,i) * wm_interm_19_pt4(j,k) * wm_interm_42_pt4(q,k,i,j)
term(413) = term(413) + r1(vrdav_Rr, p,i) * wm_interm_18_pt4(j,k) * wm_interm_43_pt4(q,k,i,j)
term(414) = term(414) + r1(vrdav_Rr, p,i) * wm_interm_19_pt4(j,k) * wm_interm_43_pt4(q,k,i,j)
term(415) = term(415) + t1(q,i) * wm_interm_18_pt4(j,k) * wm_interm_39_pt4(p,k,i,j)
term(416) = term(416) + t1(q,i) * wm_interm_19_pt4(j,k) * wm_interm_39_pt4(p,k,i,j)
term(417) = term(417) + t1(q,i) * wm_interm_18_pt4(j,k) * wm_interm_40_pt4(p,k,i,j)
term(418) = term(418) + t1(q,i) * wm_interm_19_pt4(j,k) * wm_interm_40_pt4(p,k,i,j)
end do 
end do 
end do 

term(409) = term(409) * (2.0d+0) 
term(410) = term(410) * (-4.0d+0) 
term(411) = term(411) * (2.0d+0) 
term(412) = term(412) * (-4.0d+0) 
term(413) = term(413) * (-4.0d+0) 
term(414) = term(414) * (8.0d+0) 
term(416) = term(416) * (-2.0d+0) 
term(417) = term(417) * (-2.0d+0) 
term(418) = term(418) * (4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(419) = term(419) + r2(vrdav_Rr, a,j,p,i) * wm_interm_20_pt4(a,b) * wm_interm_30_pt4(b,q,i,j)
term(420) = term(420) + r2(vrdav_Rr, a,j,p,i) * wm_interm_21_pt4(a,b) * wm_interm_30_pt4(b,q,i,j)
term(421) = term(421) + t2(a,q,j,i) * wm_interm_30_pt4(p,b,i,j) * wm_interm_68_pt4(b,a)
term(422) = term(422) + t2(a,q,j,i) * wm_interm_30_pt4(p,b,i,j) * wm_interm_69_pt4(b,a)
term(423) = term(423) + r2(vrdav_Rl, a,j,q,i) * wm_interm_20_pt4(b,a) * wm_interm_46_pt4(p,b,i,j)
term(424) = term(424) + r2(vrdav_Rl, a,j,q,i) * wm_interm_21_pt4(b,a) * wm_interm_46_pt4(p,b,i,j)
term(425) = term(425) + s2(a,q,j,i) * wm_interm_46_pt4(p,b,i,j) * wm_interm_51_pt4(b,a)
term(426) = term(426) + s2(a,q,j,i) * wm_interm_46_pt4(p,b,i,j) * wm_interm_52_pt4(b,a)
end do 
end do 
end do 
end do 

term(419) = term(419) * (2.0d+0) 
term(420) = term(420) * (-4.0d+0) 
term(421) = term(421) * (8.0d+0) 
term(422) = term(422) * (-16.0d+0) 
term(423) = term(423) * (-8.0d+0) 
term(424) = term(424) * (16.0d+0) 
term(425) = term(425) * (16.0d+0) 
term(426) = term(426) * (-8.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
term(427) = term(427) + r1(vrdav_Rl, p,i) * wm_interm_3_pt4(q,j,k,i) * wm_interm_70_pt4(j,k)
term(428) = term(428) + r1(vrdav_Rl, p,i) * wm_interm_3_pt4(q,j,k,i) * wm_interm_71_pt4(j,k)
term(429) = term(429) + s1(p,i) * wm_interm_14_pt4(q,j,k,i) * wm_interm_70_pt4(j,k)
term(430) = term(430) + s1(p,i) * wm_interm_14_pt4(q,j,k,i) * wm_interm_71_pt4(j,k)
term(431) = term(431) + t1(q,i) * wm_interm_13_pt4(p,j,k,i) * wm_interm_54_pt4(j,k)
term(432) = term(432) + t1(q,i) * wm_interm_13_pt4(p,j,k,i) * wm_interm_56_pt4(j,k)
end do 
end do 
end do 

term(427) = term(427) * (8.0d+0) 
term(428) = term(428) * (-16.0d+0) 
term(429) = term(429) * (8.0d+0) 
term(430) = term(430) * (-16.0d+0) 
term(431) = term(431) * (-4.0d+0) 
term(432) = term(432) * (8.0d+0) 

term(433) = term(433) + wm_interm_37_pt4(p,q) * wm_interm_77_pt4
term(434) = term(434) + wm_interm_37_pt4(p,q) * wm_interm_78_pt4
term(435) = term(435) + wm_interm_25_pt4 * wm_interm_37_pt4(p,q)

term(433) = term(433) * (-2.0d+0) 
term(434) = term(434) * (4.0d+0) 
term(435) = term(435) * (4.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(436) = term(436) + t2(a,q,j,i) * wm_interm_30_pt4(b,p,j,i) * wm_interm_68_pt4(b,a)
term(437) = term(437) + t2(a,q,j,i) * wm_interm_30_pt4(b,p,j,i) * wm_interm_69_pt4(b,a)
term(438) = term(438) + r2(vrdav_Rl, a,j,p,i) * wm_interm_20_pt4(b,a) * wm_interm_46_pt4(b,q,j,i)
term(439) = term(439) + r2(vrdav_Rl, a,j,p,i) * wm_interm_21_pt4(b,a) * wm_interm_46_pt4(b,q,j,i)
term(440) = term(440) + s2(a,p,j,i) * wm_interm_46_pt4(b,q,j,i) * wm_interm_51_pt4(b,a)
term(441) = term(441) + s2(a,p,j,i) * wm_interm_46_pt4(b,q,j,i) * wm_interm_52_pt4(b,a)
end do 
end do 
end do 
end do 

term(436) = term(436) * (8.0d+0) 
term(437) = term(437) * (-16.0d+0) 
term(438) = term(438) * (-4.0d+0) 
term(439) = term(439) * (8.0d+0) 
term(440) = term(440) * (8.0d+0) 
term(441) = term(441) * (-4.0d+0) 

do i = 1, nocc 
term(442) = term(442) + wm_interm_31_pt4(p,i) * wm_interm_44_pt4(q,i)
term(443) = term(443) + wm_interm_34_pt4(p,i) * wm_interm_44_pt4(q,i)
term(444) = term(444) + wm_interm_31_pt4(p,i) * wm_interm_45_pt4(q,i)
term(445) = term(445) + wm_interm_34_pt4(p,i) * wm_interm_45_pt4(q,i)
end do 

term(442) = term(442) * (-8.0d+0) 
term(443) = term(443) * (4.0d+0) 
term(444) = term(444) * (4.0d+0) 
term(445) = term(445) * (-2.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(446) = term(446) + r1(vrdav_Rl, a,i) * wm_interm_1_pt4(q,a,j,k) * wm_interm_28_pt4(p,i,j,k)
term(447) = term(447) + r1(vrdav_Rl, a,i) * wm_interm_1_pt4(q,a,j,k) * wm_interm_29_pt4(p,i,j,k)
term(448) = term(448) + r1(vrdav_Rl, a,i) * wm_interm_29_pt4(p,i,j,k) * wm_interm_5_pt4(q,a,j,k)
term(449) = term(449) + r1(vrdav_Rl, a,i) * wm_interm_28_pt4(p,i,j,k) * wm_interm_5_pt4(q,a,j,k)
term(450) = term(450) + r1(vrdav_Rl, a,i) * wm_interm_11_pt4(q,a,j,k) * wm_interm_28_pt4(p,i,j,k)
term(451) = term(451) + r1(vrdav_Rl, a,i) * wm_interm_12_pt4(q,a,j,k) * wm_interm_28_pt4(p,i,j,k)
term(452) = term(452) + r1(vrdav_Rl, a,i) * wm_interm_11_pt4(q,a,j,k) * wm_interm_29_pt4(p,i,j,k)
term(453) = term(453) + r1(vrdav_Rl, a,i) * wm_interm_12_pt4(q,a,j,k) * wm_interm_29_pt4(p,i,j,k)
term(454) = term(454) + r1(vrdav_Rr, p,i) * wm_interm_49_pt4(q,a,j,k) * wm_interm_76_pt4(a,i,k,j)
term(455) = term(455) + r1(vrdav_Rr, p,i) * wm_interm_48_pt4(q,a,j,k) * wm_interm_76_pt4(a,i,k,j)
term(456) = term(456) + r1(vrdav_Rr, p,i) * wm_interm_23_pt4(a,i,j,k) * wm_interm_50_pt4(q,a,k,j)
term(457) = term(457) + r1(vrdav_Rr, p,i) * wm_interm_23_pt4(a,i,j,k) * wm_interm_53_pt4(q,a,k,j)
term(458) = term(458) + r1(vrdav_Rr, p,i) * wm_interm_11_pt4(q,a,j,k) * wm_interm_42_pt4(a,i,k,j)
term(459) = term(459) + r1(vrdav_Rr, p,i) * wm_interm_12_pt4(q,a,j,k) * wm_interm_42_pt4(a,i,k,j)
term(460) = term(460) + r1(vrdav_Rr, p,i) * wm_interm_1_pt4(q,a,j,k) * wm_interm_42_pt4(a,i,k,j)
term(461) = term(461) + r1(vrdav_Rr, p,i) * wm_interm_42_pt4(a,i,j,k) * wm_interm_5_pt4(q,a,k,j)
term(462) = term(462) + r1(vrdav_Rr, p,i) * wm_interm_50_pt4(q,a,j,k) * wm_interm_76_pt4(a,i,k,j)
term(463) = term(463) + r1(vrdav_Rr, p,i) * wm_interm_53_pt4(q,a,j,k) * wm_interm_76_pt4(a,i,k,j)
term(464) = term(464) + r1(vrdav_Rr, p,i) * wm_interm_11_pt4(q,a,j,k) * wm_interm_43_pt4(a,i,k,j)
term(465) = term(465) + r1(vrdav_Rr, p,i) * wm_interm_12_pt4(q,a,j,k) * wm_interm_43_pt4(a,i,k,j)
term(466) = term(466) + r1(vrdav_Rr, p,i) * wm_interm_43_pt4(a,i,j,k) * wm_interm_5_pt4(q,a,k,j)
term(467) = term(467) + r1(vrdav_Rr, p,i) * wm_interm_1_pt4(q,a,j,k) * wm_interm_43_pt4(a,i,k,j)
term(468) = term(468) + t1(q,i) * wm_interm_11_pt4(p,a,j,k) * wm_interm_40_pt4(a,i,k,j)
term(469) = term(469) + t1(q,i) * wm_interm_12_pt4(p,a,j,k) * wm_interm_40_pt4(a,i,k,j)
term(470) = term(470) + t1(q,i) * wm_interm_11_pt4(p,a,j,k) * wm_interm_39_pt4(a,i,k,j)
term(471) = term(471) + t1(q,i) * wm_interm_12_pt4(p,a,j,k) * wm_interm_39_pt4(a,i,k,j)
term(472) = term(472) + t1(q,i) * wm_interm_1_pt4(p,a,j,k) * wm_interm_39_pt4(a,i,k,j)
term(473) = term(473) + t1(q,i) * wm_interm_39_pt4(a,i,j,k) * wm_interm_5_pt4(p,a,k,j)
term(474) = term(474) + t1(q,i) * wm_interm_40_pt4(a,i,j,k) * wm_interm_5_pt4(p,a,k,j)
term(475) = term(475) + t1(q,i) * wm_interm_1_pt4(p,a,j,k) * wm_interm_40_pt4(a,i,k,j)
end do 
end do 
end do 
end do 

term(446) = term(446) * (-4.0d+0) 
term(447) = term(447) * (8.0d+0) 
term(448) = term(448) * (-4.0d+0) 
term(449) = term(449) * (8.0d+0) 
term(450) = term(450) * (-4.0d+0) 
term(451) = term(451) * (8.0d+0) 
term(452) = term(452) * (8.0d+0) 
term(453) = term(453) * (-16.0d+0) 
term(454) = term(454) * (-8.0d+0) 
term(455) = term(455) * (16.0d+0) 
term(456) = term(456) * (-4.0d+0) 
term(457) = term(457) * (2.0d+0) 
term(458) = term(458) * (2.0d+0) 
term(459) = term(459) * (-4.0d+0) 
term(460) = term(460) * (2.0d+0) 
term(461) = term(461) * (-4.0d+0) 
term(462) = term(462) * (2.0d+0) 
term(463) = term(463) * (-4.0d+0) 
term(464) = term(464) * (-4.0d+0) 
term(465) = term(465) * (8.0d+0) 
term(466) = term(466) * (2.0d+0) 
term(467) = term(467) * (-4.0d+0) 
term(468) = term(468) * (-2.0d+0) 
term(469) = term(469) * (4.0d+0) 
term(471) = term(471) * (-2.0d+0) 
term(473) = term(473) * (-2.0d+0) 
term(475) = term(475) * (-2.0d+0) 

do a = nocc + 1, nactive 
term(476) = term(476) + wm_interm_37_pt4(q,a) * wm_interm_74_pt4(a,p)
term(477) = term(477) + wm_interm_37_pt4(q,a) * wm_interm_75_pt4(a,p)
term(478) = term(478) + wm_interm_37_pt4(a,q) * wm_interm_74_pt4(p,a)
term(479) = term(479) + wm_interm_37_pt4(a,q) * wm_interm_75_pt4(p,a)
term(480) = term(480) + wm_interm_0_pt4(p,a) * wm_interm_24_pt4(q,a)
term(481) = term(481) + wm_interm_16_pt4(a,p) * wm_interm_37_pt4(q,a)
term(482) = term(482) + wm_interm_0_pt4(a,p) * wm_interm_24_pt4(a,q)
term(483) = term(483) + wm_interm_16_pt4(p,a) * wm_interm_37_pt4(a,q)
end do 

term(476) = term(476) * (2.0d+0) 
term(477) = term(477) * (-4.0d+0) 
term(478) = term(478) * (2.0d+0) 
term(479) = term(479) * (-4.0d+0) 
term(480) = term(480) * (-2.0d+0) 
term(481) = term(481) * (-2.0d+0) 
term(482) = term(482) * (-2.0d+0) 
term(483) = term(483) * (-2.0d+0) 

do l = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
term(484) = term(484) + r1(vrdav_Rl, q,i) * wm_interm_10_pt4(j,k,i,l) * wm_interm_28_pt4(p,k,j,l)
end do 
end do 
end do 
end do 

term(484) = term(484) * (-4.0d+0) 

do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
term(485) = term(485) + r1(vrdav_Rr, p,i) * wm_interm_18_pt4(j,k) * wm_interm_42_pt4(q,i,k,j)
term(486) = term(486) + r1(vrdav_Rr, p,i) * wm_interm_19_pt4(j,k) * wm_interm_42_pt4(q,i,k,j)
term(487) = term(487) + r1(vrdav_Rr, p,i) * wm_interm_18_pt4(j,k) * wm_interm_43_pt4(q,i,k,j)
term(488) = term(488) + r1(vrdav_Rr, p,i) * wm_interm_19_pt4(j,k) * wm_interm_43_pt4(q,i,k,j)
term(489) = term(489) + t1(q,i) * wm_interm_18_pt4(j,k) * wm_interm_39_pt4(p,i,k,j)
term(490) = term(490) + t1(q,i) * wm_interm_19_pt4(j,k) * wm_interm_39_pt4(p,i,k,j)
term(491) = term(491) + t1(q,i) * wm_interm_18_pt4(j,k) * wm_interm_40_pt4(p,i,k,j)
term(492) = term(492) + t1(q,i) * wm_interm_19_pt4(j,k) * wm_interm_40_pt4(p,i,k,j)
end do 
end do 
end do 

term(485) = term(485) * (-4.0d+0) 
term(486) = term(486) * (8.0d+0) 
term(487) = term(487) * (2.0d+0) 
term(488) = term(488) * (-4.0d+0) 
term(489) = term(489) * (-2.0d+0) 
term(490) = term(490) * (4.0d+0) 
term(492) = term(492) * (-2.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(493) = term(493) + s2(a,q,j,i) * wm_interm_26_pt4(p,k,j,i) * wm_interm_7_pt4(a,k)
term(494) = term(494) + s2(a,q,j,i) * wm_interm_26_pt4(p,k,j,i) * wm_interm_8_pt4(a,k)
term(495) = term(495) + s2(a,p,j,i) * wm_interm_14_pt4(q,k,j,i) * wm_interm_31_pt4(a,k)
term(496) = term(496) + s2(a,p,j,i) * wm_interm_14_pt4(q,k,j,i) * wm_interm_34_pt4(a,k)
term(497) = term(497) + s2(a,p,j,i) * wm_interm_35_pt4(a,k) * wm_interm_3_pt4(q,k,j,i)
term(498) = term(498) + s2(a,p,j,i) * wm_interm_32_pt4(a,k) * wm_interm_3_pt4(q,k,j,i)
term(499) = term(499) + t2(a,q,j,i) * wm_interm_13_pt4(p,k,j,i) * wm_interm_44_pt4(a,k)
term(500) = term(500) + t2(a,q,j,i) * wm_interm_13_pt4(p,k,j,i) * wm_interm_45_pt4(a,k)
end do 
end do 
end do 
end do 

term(493) = term(493) * (-16.0d+0) 
term(494) = term(494) * (8.0d+0) 
term(495) = term(495) * (-8.0d+0) 
term(496) = term(496) * (4.0d+0) 
term(497) = term(497) * (8.0d+0) 
term(498) = term(498) * (-16.0d+0) 
term(499) = term(499) * (8.0d+0) 
term(500) = term(500) * (-4.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
term(501) = term(501) + wm_interm_27_pt4(i,j) * wm_interm_58_pt4(q,p,j,i)
term(502) = term(502) + wm_interm_27_pt4(i,j) * wm_interm_57_pt4(q,p,j,i)
term(503) = term(503) + wm_interm_27_pt4(i,j) * wm_interm_64_pt4(q,p,j,i)
term(504) = term(504) + wm_interm_27_pt4(i,j) * wm_interm_65_pt4(q,p,j,i)
end do 
end do 

term(501) = term(501) * (2.0d+0) 
term(502) = term(502) * (-4.0d+0) 
term(503) = term(503) * (2.0d+0) 
term(504) = term(504) * (-4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
term(505) = term(505) + wm_interm_36_pt4(p,q,i,j) * wm_interm_66_pt4(j,i)
term(506) = term(506) + wm_interm_36_pt4(p,q,i,j) * wm_interm_67_pt4(j,i)
term(507) = term(507) + r1(vrdav_Rl, q,j) * wm_interm_18_pt4(i,j) * wm_interm_31_pt4(p,i)
term(508) = term(508) + r1(vrdav_Rl, q,j) * wm_interm_19_pt4(i,j) * wm_interm_31_pt4(p,i)
term(509) = term(509) + r1(vrdav_Rl, q,j) * wm_interm_18_pt4(i,j) * wm_interm_34_pt4(p,i)
term(510) = term(510) + r1(vrdav_Rl, q,j) * wm_interm_19_pt4(i,j) * wm_interm_34_pt4(p,i)
term(511) = term(511) + r1(vrdav_Rl, p,i) * wm_interm_70_pt4(i,j) * wm_interm_8_pt4(q,j)
term(512) = term(512) + r1(vrdav_Rl, p,i) * wm_interm_71_pt4(i,j) * wm_interm_8_pt4(q,j)
term(513) = term(513) + r1(vrdav_Rl, p,i) * wm_interm_70_pt4(i,j) * wm_interm_7_pt4(q,j)
term(514) = term(514) + r1(vrdav_Rl, p,i) * wm_interm_71_pt4(i,j) * wm_interm_7_pt4(q,j)
term(515) = term(515) + s1(p,i) * wm_interm_17_pt4(q,j) * wm_interm_70_pt4(i,j)
term(516) = term(516) + s1(p,i) * wm_interm_17_pt4(q,j) * wm_interm_71_pt4(i,j)
term(517) = term(517) + s1(p,i) * wm_interm_15_pt4(q,j) * wm_interm_70_pt4(i,j)
term(518) = term(518) + s1(p,i) * wm_interm_15_pt4(q,j) * wm_interm_71_pt4(i,j)
term(519) = term(519) + r1(vrdav_Rr, p,i) * wm_interm_18_pt4(i,j) * wm_interm_44_pt4(q,j)
term(520) = term(520) + r1(vrdav_Rr, p,i) * wm_interm_19_pt4(i,j) * wm_interm_44_pt4(q,j)
term(521) = term(521) + r1(vrdav_Rr, p,i) * wm_interm_18_pt4(i,j) * wm_interm_45_pt4(q,j)
term(522) = term(522) + r1(vrdav_Rr, p,i) * wm_interm_19_pt4(i,j) * wm_interm_45_pt4(q,j)
term(523) = term(523) + t1(q,j) * wm_interm_18_pt4(j,i) * wm_interm_38_pt4(p,i)
term(524) = term(524) + t1(q,j) * wm_interm_19_pt4(j,i) * wm_interm_38_pt4(p,i)
term(525) = term(525) + t1(q,j) * wm_interm_18_pt4(j,i) * wm_interm_41_pt4(p,i)
term(526) = term(526) + t1(q,j) * wm_interm_19_pt4(j,i) * wm_interm_41_pt4(p,i)
term(527) = term(527) + t1(q,j) * wm_interm_2_pt4(p,i) * wm_interm_54_pt4(j,i)
term(528) = term(528) + t1(q,j) * wm_interm_4_pt4(p,i) * wm_interm_54_pt4(j,i)
term(529) = term(529) + t1(q,j) * wm_interm_2_pt4(p,i) * wm_interm_56_pt4(j,i)
term(530) = term(530) + t1(q,j) * wm_interm_4_pt4(p,i) * wm_interm_56_pt4(j,i)
end do 
end do 

term(505) = term(505) * (2.0d+0) 
term(506) = term(506) * (-4.0d+0) 
term(507) = term(507) * (4.0d+0) 
term(508) = term(508) * (-8.0d+0) 
term(509) = term(509) * (-2.0d+0) 
term(510) = term(510) * (4.0d+0) 
term(511) = term(511) * (-4.0d+0) 
term(512) = term(512) * (8.0d+0) 
term(513) = term(513) * (8.0d+0) 
term(514) = term(514) * (-16.0d+0) 
term(515) = term(515) * (-4.0d+0) 
term(516) = term(516) * (8.0d+0) 
term(517) = term(517) * (8.0d+0) 
term(518) = term(518) * (-16.0d+0) 
term(519) = term(519) * (-8.0d+0) 
term(520) = term(520) * (16.0d+0) 
term(521) = term(521) * (4.0d+0) 
term(522) = term(522) * (-8.0d+0) 
term(523) = term(523) * (2.0d+0) 
term(524) = term(524) * (-4.0d+0) 
term(525) = term(525) * (-4.0d+0) 
term(526) = term(526) * (8.0d+0) 
term(527) = term(527) * (-4.0d+0) 
term(528) = term(528) * (2.0d+0) 
term(529) = term(529) * (8.0d+0) 
term(530) = term(530) * (-4.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
term(531) = term(531) + s1(q,i) * wm_interm_18_pt4(j,k) * wm_interm_26_pt4(p,k,j,i)
term(532) = term(532) + s1(q,i) * wm_interm_19_pt4(j,k) * wm_interm_26_pt4(p,k,j,i)
end do 
end do 
end do 

term(531) = term(531) * (-4.0d+0) 
term(532) = term(532) * (8.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(533) = term(533) + r1(vrdav_Rl, p,i) * wm_interm_3_pt4(q,j,k,l) * wm_interm_72_pt4(i,j,k,l)
term(534) = term(534) + r1(vrdav_Rl, p,i) * wm_interm_3_pt4(q,j,k,l) * wm_interm_72_pt4(i,j,l,k)
term(535) = term(535) + s1(p,i) * wm_interm_14_pt4(q,j,k,l) * wm_interm_72_pt4(j,i,l,k)
term(536) = term(536) + s1(p,i) * wm_interm_14_pt4(q,j,k,l) * wm_interm_72_pt4(j,i,k,l)
term(537) = term(537) + r1(vrdav_Rr, p,i) * wm_interm_23_pt4(q,j,k,l) * wm_interm_55_pt4(i,l,j,k)
term(538) = term(538) + r1(vrdav_Rr, p,i) * wm_interm_23_pt4(q,j,k,l) * wm_interm_55_pt4(l,i,j,k)
term(539) = term(539) + t1(q,i) * wm_interm_13_pt4(p,j,k,l) * wm_interm_55_pt4(i,j,k,l)
term(540) = term(540) + t1(q,i) * wm_interm_13_pt4(p,j,k,l) * wm_interm_55_pt4(j,i,k,l)
term(541) = term(541) + t1(q,i) * wm_interm_13_pt4(p,j,k,l) * wm_interm_55_pt4(j,i,l,k)
term(542) = term(542) + t1(q,i) * wm_interm_13_pt4(p,j,k,l) * wm_interm_55_pt4(i,j,l,k)
end do 
end do 
end do 
end do 

term(533) = term(533) * (-4.0d+0) 
term(534) = term(534) * (8.0d+0) 
term(535) = term(535) * (-4.0d+0) 
term(536) = term(536) * (8.0d+0) 
term(537) = term(537) * (-4.0d+0) 
term(538) = term(538) * (2.0d+0) 
term(540) = term(540) * (-2.0d+0) 
term(542) = term(542) * (-2.0d+0) 

do l = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(543) = term(543) + r2(vrdav_Rl, a,k,p,i) * t1(b,i) * t2(a,q,l,j) * wm_interm_13_pt4(b,k,j,l)
term(544) = term(544) + r2(vrdav_Rl, a,k,p,i) * t1(b,k) * t2(a,q,l,j) * wm_interm_13_pt4(b,i,j,l)
end do 
end do 
end do 
end do 
end do 
end do 

term(543) = term(543) * (2.0d+0) 
term(544) = term(544) * (-4.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(545) = term(545) + r2(vrdav_Rr, a,j,p,i) * wm_interm_20_pt4(a,b) * wm_interm_30_pt4(b,q,j,i)
term(546) = term(546) + r2(vrdav_Rr, a,j,p,i) * wm_interm_21_pt4(a,b) * wm_interm_30_pt4(b,q,j,i)
term(547) = term(547) + t2(a,q,j,i) * wm_interm_30_pt4(p,b,j,i) * wm_interm_68_pt4(b,a)
term(548) = term(548) + t2(a,q,j,i) * wm_interm_30_pt4(p,b,j,i) * wm_interm_69_pt4(b,a)
term(549) = term(549) + r2(vrdav_Rl, a,j,q,i) * wm_interm_20_pt4(b,a) * wm_interm_46_pt4(p,b,j,i)
term(550) = term(550) + r2(vrdav_Rl, a,j,q,i) * wm_interm_21_pt4(b,a) * wm_interm_46_pt4(p,b,j,i)
term(551) = term(551) + s2(a,q,j,i) * wm_interm_46_pt4(p,b,j,i) * wm_interm_51_pt4(b,a)
term(552) = term(552) + s2(a,q,j,i) * wm_interm_46_pt4(p,b,j,i) * wm_interm_52_pt4(b,a)
end do 
end do 
end do 
end do 

term(545) = term(545) * (-4.0d+0) 
term(546) = term(546) * (8.0d+0) 
term(547) = term(547) * (-4.0d+0) 
term(548) = term(548) * (8.0d+0) 
term(549) = term(549) * (4.0d+0) 
term(550) = term(550) * (-8.0d+0) 
term(551) = term(551) * (-8.0d+0) 
term(552) = term(552) * (4.0d+0) 

do l = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
term(553) = term(553) + r1(vrdav_Rr, p,i) * wm_interm_23_pt4(q,j,k,l) * wm_interm_55_pt4(i,l,k,j)
term(554) = term(554) + r1(vrdav_Rr, p,i) * wm_interm_10_pt4(j,i,k,l) * wm_interm_42_pt4(q,k,l,j)
term(555) = term(555) + r1(vrdav_Rr, p,i) * wm_interm_10_pt4(i,j,k,l) * wm_interm_42_pt4(q,k,l,j)
term(556) = term(556) + r1(vrdav_Rr, p,i) * wm_interm_23_pt4(q,j,k,l) * wm_interm_55_pt4(l,i,k,j)
term(557) = term(557) + r1(vrdav_Rr, p,i) * wm_interm_10_pt4(i,j,k,l) * wm_interm_43_pt4(q,k,l,j)
term(558) = term(558) + r1(vrdav_Rr, p,i) * wm_interm_10_pt4(j,i,k,l) * wm_interm_43_pt4(q,k,l,j)
term(559) = term(559) + t1(q,i) * wm_interm_10_pt4(j,i,k,l) * wm_interm_39_pt4(p,k,l,j)
term(560) = term(560) + t1(q,i) * wm_interm_10_pt4(i,j,k,l) * wm_interm_39_pt4(p,k,l,j)
term(561) = term(561) + t1(q,i) * wm_interm_10_pt4(i,j,k,l) * wm_interm_40_pt4(p,k,l,j)
term(562) = term(562) + t1(q,i) * wm_interm_10_pt4(j,i,k,l) * wm_interm_40_pt4(p,k,l,j)
end do 
end do 
end do 
end do 

term(553) = term(553) * (2.0d+0) 
term(554) = term(554) * (2.0d+0) 
term(555) = term(555) * (-4.0d+0) 
term(556) = term(556) * (-4.0d+0) 
term(557) = term(557) * (2.0d+0) 
term(558) = term(558) * (-4.0d+0) 
term(560) = term(560) * (-2.0d+0) 
term(562) = term(562) * (-2.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(563) = term(563) + r2(vrdav_Rl, a,k,p,i) * t1(b,i) * t2(a,q,l,j) * wm_interm_13_pt4(b,k,l,j)
term(564) = term(564) + r2(vrdav_Rl, a,k,p,i) * t1(b,k) * t2(a,q,l,j) * wm_interm_13_pt4(b,i,l,j)
term(565) = term(565) + s2(a,p,k,j) * t1(b,k) * t2(a,q,l,i) * wm_interm_40_pt4(b,l,i,j)
term(566) = term(566) + s2(a,p,k,j) * t1(b,k) * t2(a,q,l,i) * wm_interm_40_pt4(b,i,l,j)
term(567) = term(567) + s2(a,p,k,j) * t1(b,k) * t2(a,q,l,i) * wm_interm_39_pt4(b,i,l,j)
term(568) = term(568) + s2(a,p,k,j) * t1(b,k) * t2(a,q,l,i) * wm_interm_39_pt4(b,l,i,j)
end do 
end do 
end do 
end do 
end do 
end do 

term(563) = term(563) * (-4.0d+0) 
term(564) = term(564) * (2.0d+0) 
term(566) = term(566) * (-2.0d+0) 
term(568) = term(568) * (-2.0d+0) 


    calc_D_vv_wm_pt4 = zero
    do s = 0, 568
    calc_D_vv_wm_pt4 = calc_D_vv_wm_pt4 + term(s)
    end do

    end function calc_D_vv_wm_pt4
    

    
    

  end module ss_ccsd_pt4
