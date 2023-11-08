module ss_cc3_pt012
 use ccsd_transformed_integrals
    use cc3_intermediates
    use arithmetic
    use s_gen
    use basis
    use eom_vectors

    implicit none
       !
    ! File generated automatically on 2018-04-19 15:34:28
    !
        real(F64), dimension(:, :), allocatable :: wm_interm_0_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_1_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_2_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_3_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_4_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_5_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_6_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_7_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_8_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_9_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_10_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_11_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_12_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_13_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_14_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_15_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_16_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_17_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_18_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_19_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_20_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_21_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_22_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_23_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_24_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_25_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_26_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_27_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_28_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_29_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_30_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_31_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_32_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_33_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_34_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_35_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_36_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_37_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_38_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_39_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_40_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_41_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_42_pt1

  real(F64), dimension(:, :, :, :), allocatable :: wm_interm_0_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_1_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_2_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_3_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_4_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_5_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_6_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_7_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_8_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_9_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_10_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_11_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_12_pt2 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_13_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_14_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_15_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_16_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_17_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_18_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_19_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_20_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_21_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_22_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_23_pt2 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_24_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_25_pt2 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_26_pt2 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_27_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_28_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_29_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_30_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_31_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_32_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_33_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_34_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_35_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_36_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_37_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_38_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_39_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_40_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_41_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_42_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_43_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_44_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_45_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_46_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_47_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_48_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_49_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_50_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_51_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_52_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_53_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_54_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_55_pt2 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_56_pt2 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_57_pt2 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_58_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_59_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_60_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_61_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_62_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_63_pt2 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_64_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_65_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_66_pt2 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_67_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_68_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_69_pt2 
real(F64), dimension(:, :, :, :, :, :), allocatable :: wm_interm_70_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_71_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_72_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_73_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_74_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_75_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_76_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_77_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_78_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_79_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_80_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_81_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_82_pt2

    contains
    
    subroutine wm_intermediates_cc3_init_pt0(nocc, nactive)
    integer, intent(in) :: nocc
    integer, intent(in) :: nactive
    
    end subroutine wm_intermediates_cc3_init_pt0
    
    subroutine wm_intermediates_cc3_free_pt0
    
    end subroutine wm_intermediates_cc3_free_pt0
    
    subroutine wm_intermediates_cc3_pt0(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr)
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    real(F64) :: sum
    

  end subroutine wm_intermediates_cc3_pt0

      subroutine wm_intermediates_cc3_init_pt1(nocc, nactive)
    integer, intent(in) :: nocc
    integer, intent(in) :: nactive
    allocate(wm_interm_0_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_1_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_2_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_3_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_4_pt1(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_5_pt1(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_6_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_7_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_8_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_9_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_10_pt1(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_11_pt1(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_12_pt1(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_13_pt1(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_14_pt1(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_15_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_16_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_17_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_18_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_19_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_20_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_21_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_22_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_23_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_24_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_25_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_26_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_27_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_28_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_29_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_30_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_31_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_32_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_33_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_34_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_35_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_36_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_37_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_38_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_39_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_40_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_41_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_42_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
wm_interm_0_pt1 = zero 
wm_interm_1_pt1 = zero 
wm_interm_2_pt1 = zero 
wm_interm_3_pt1 = zero 
wm_interm_4_pt1 = zero 
wm_interm_5_pt1 = zero 
wm_interm_6_pt1 = zero 
wm_interm_7_pt1 = zero 
wm_interm_8_pt1 = zero 
wm_interm_9_pt1 = zero 
wm_interm_10_pt1 = zero 
wm_interm_11_pt1 = zero 
wm_interm_12_pt1 = zero 
wm_interm_13_pt1 = zero 
wm_interm_14_pt1 = zero 
wm_interm_15_pt1 = zero 
wm_interm_16_pt1 = zero 
wm_interm_17_pt1 = zero 
wm_interm_18_pt1 = zero 
wm_interm_19_pt1 = zero 
wm_interm_20_pt1 = zero 
wm_interm_21_pt1 = zero 
wm_interm_22_pt1 = zero 
wm_interm_23_pt1 = zero 
wm_interm_24_pt1 = zero 
wm_interm_25_pt1 = zero 
wm_interm_26_pt1 = zero 
wm_interm_27_pt1 = zero 
wm_interm_28_pt1 = zero 
wm_interm_29_pt1 = zero 
wm_interm_30_pt1 = zero 
wm_interm_31_pt1 = zero 
wm_interm_32_pt1 = zero 
wm_interm_33_pt1 = zero 
wm_interm_34_pt1 = zero 
wm_interm_35_pt1 = zero 
wm_interm_36_pt1 = zero 
wm_interm_37_pt1 = zero 
wm_interm_38_pt1 = zero 
wm_interm_39_pt1 = zero 
wm_interm_40_pt1 = zero 
wm_interm_41_pt1 = zero 
wm_interm_42_pt1 = zero 

    end subroutine wm_intermediates_cc3_init_pt1
    
    subroutine wm_intermediates_cc3_free_pt1
    deallocate(wm_interm_0_pt1)
deallocate(wm_interm_1_pt1)
deallocate(wm_interm_2_pt1)
deallocate(wm_interm_3_pt1)
deallocate(wm_interm_4_pt1)
deallocate(wm_interm_5_pt1)
deallocate(wm_interm_6_pt1)
deallocate(wm_interm_7_pt1)
deallocate(wm_interm_8_pt1)
deallocate(wm_interm_9_pt1)
deallocate(wm_interm_10_pt1)
deallocate(wm_interm_11_pt1)
deallocate(wm_interm_12_pt1)
deallocate(wm_interm_13_pt1)
deallocate(wm_interm_14_pt1)
deallocate(wm_interm_15_pt1)
deallocate(wm_interm_16_pt1)
deallocate(wm_interm_17_pt1)
deallocate(wm_interm_18_pt1)
deallocate(wm_interm_19_pt1)
deallocate(wm_interm_20_pt1)
deallocate(wm_interm_21_pt1)
deallocate(wm_interm_22_pt1)
deallocate(wm_interm_23_pt1)
deallocate(wm_interm_24_pt1)
deallocate(wm_interm_25_pt1)
deallocate(wm_interm_26_pt1)
deallocate(wm_interm_27_pt1)
deallocate(wm_interm_28_pt1)
deallocate(wm_interm_29_pt1)
deallocate(wm_interm_30_pt1)
deallocate(wm_interm_31_pt1)
deallocate(wm_interm_32_pt1)
deallocate(wm_interm_33_pt1)
deallocate(wm_interm_34_pt1)
deallocate(wm_interm_35_pt1)
deallocate(wm_interm_36_pt1)
deallocate(wm_interm_37_pt1)
deallocate(wm_interm_38_pt1)
deallocate(wm_interm_39_pt1)
deallocate(wm_interm_40_pt1)
deallocate(wm_interm_41_pt1)
deallocate(wm_interm_42_pt1)

    end subroutine wm_intermediates_cc3_free_pt1
    
    subroutine wm_intermediates_cc3_pt1(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr)
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    real(F64) :: sum
    integer :: a, b, i, j, c, k, l 

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
sum = sum + r3(vrdav_Rr, a,k,b,j,c,i) * s2(a,b,i,j)
end do 
end do 
end do 
end do 
wm_interm_0_pt1(c, k) = wm_interm_0_pt1(c, k) + sum 
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
sum = sum + r3(vrdav_Rr, a,i,b,j,c,k) * s2(a,b,i,j)
end do 
end do 
end do 
end do 
wm_interm_1_pt1(c, k) = wm_interm_1_pt1(c, k) + sum 
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
sum = sum + r3(vrdav_Rr, a,k,b,j,c,i) * s2(a,b,j,i)
end do 
end do 
end do 
end do 
wm_interm_2_pt1(c, k) = wm_interm_2_pt1(c, k) + sum 
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
sum = sum + r3(vrdav_Rr, a,i,b,j,c,k) * s2(a,b,j,i)
end do 
end do 
end do 
end do 
wm_interm_3_pt1(c, k) = wm_interm_3_pt1(c, k) + sum 
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
wm_interm_4_pt1(b, c, j, k) = wm_interm_4_pt1(b, c, j, k) + sum 
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
wm_interm_5_pt1(b, c, j, k) = wm_interm_5_pt1(b, c, j, k) + sum 
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
sum = sum + r3(vrdav_Rl, a,k,b,j,c,i) * t2(a,b,i,j)
end do 
end do 
end do 
end do 
wm_interm_6_pt1(c, k) = wm_interm_6_pt1(c, k) + sum 
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
sum = sum + r3(vrdav_Rl, a,i,b,j,c,k) * t2(a,b,i,j)
end do 
end do 
end do 
end do 
wm_interm_7_pt1(c, k) = wm_interm_7_pt1(c, k) + sum 
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
sum = sum + r3(vrdav_Rl, a,k,b,j,c,i) * t2(a,b,j,i)
end do 
end do 
end do 
end do 
wm_interm_8_pt1(c, k) = wm_interm_8_pt1(c, k) + sum 
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
sum = sum + r3(vrdav_Rl, a,i,b,j,c,k) * t2(a,b,j,i)
end do 
end do 
end do 
end do 
wm_interm_9_pt1(c, k) = wm_interm_9_pt1(c, k) + sum 
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
wm_interm_10_pt1(b, c, k, j) = wm_interm_10_pt1(b, c, k, j) + sum 
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
wm_interm_11_pt1(b, c, j, k) = wm_interm_11_pt1(b, c, j, k) + sum 
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
wm_interm_12_pt1(b, c, k, j) = wm_interm_12_pt1(b, c, k, j) + sum 
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
wm_interm_13_pt1(b, c, j, k) = wm_interm_13_pt1(b, c, j, k) + sum 
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
wm_interm_14_pt1(b, c, j, k) = wm_interm_14_pt1(b, c, j, k) + sum 
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
sum = sum + r2(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,k,b,j,c,i)
end do 
end do 
end do 
end do 
wm_interm_15_pt1(c, k) = wm_interm_15_pt1(c, k) + sum 
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
wm_interm_16_pt1(c, k) = wm_interm_16_pt1(c, k) + sum 
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
wm_interm_17_pt1(c, k) = wm_interm_17_pt1(c, k) + sum 
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
wm_interm_18_pt1(c, k) = wm_interm_18_pt1(c, k) + sum 
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
wm_interm_19_pt1(c, j, k, l) = wm_interm_19_pt1(c, j, k, l) + sum 
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
wm_interm_20_pt1(c, j, k, l) = wm_interm_20_pt1(c, j, k, l) + sum 
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
sum = sum + r2(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,k,b,l,c,i)
end do 
end do 
end do 
wm_interm_21_pt1(c, j, k, l) = wm_interm_21_pt1(c, j, k, l) + sum 
end do 
end do 
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
wm_interm_22_pt1(c, j, l, k) = wm_interm_22_pt1(c, j, l, k) + sum 
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
sum = sum + r3(vrdav_Rr, a,k,b,i,c,j) * s2(a,b,i,l)
end do 
end do 
end do 
wm_interm_23_pt1(c, k, j, l) = wm_interm_23_pt1(c, k, j, l) + sum 
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
sum = sum + r3(vrdav_Rr, a,j,b,k,c,i) * s2(a,b,i,l)
end do 
end do 
end do 
wm_interm_24_pt1(c, j, k, l) = wm_interm_24_pt1(c, j, k, l) + sum 
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
sum = sum + r3(vrdav_Rr, a,k,b,i,c,j) * s2(a,b,l,i)
end do 
end do 
end do 
wm_interm_25_pt1(c, k, j, l) = wm_interm_25_pt1(c, k, j, l) + sum 
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
sum = sum + r3(vrdav_Rr, a,j,b,k,c,i) * s2(a,b,l,i)
end do 
end do 
end do 
wm_interm_26_pt1(c, j, k, l) = wm_interm_26_pt1(c, j, k, l) + sum 
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
sum = sum + r3(vrdav_Rl, a,j,b,i,c,k) * t2(a,b,i,l)
end do 
end do 
end do 
wm_interm_27_pt1(c, j, k, l) = wm_interm_27_pt1(c, j, k, l) + sum 
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
sum = sum + r3(vrdav_Rl, a,j,b,k,c,i) * t2(a,b,i,l)
end do 
end do 
end do 
wm_interm_28_pt1(c, j, k, l) = wm_interm_28_pt1(c, j, k, l) + sum 
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
sum = sum + r3(vrdav_Rl, a,j,b,i,c,k) * t2(a,b,l,i)
end do 
end do 
end do 
wm_interm_29_pt1(c, j, k, l) = wm_interm_29_pt1(c, j, k, l) + sum 
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
sum = sum + r3(vrdav_Rl, a,k,b,i,c,j) * t2(a,b,l,i)
end do 
end do 
end do 
wm_interm_30_pt1(c, k, j, l) = wm_interm_30_pt1(c, k, j, l) + sum 
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
sum = sum + r3(vrdav_Rl, a,k,b,i,c,j) * t2(a,b,i,l)
end do 
end do 
end do 
wm_interm_31_pt1(c, k, j, l) = wm_interm_31_pt1(c, k, j, l) + sum 
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
sum = sum + r3(vrdav_Rl, a,j,b,k,c,i) * t2(a,b,l,i)
end do 
end do 
end do 
wm_interm_32_pt1(c, j, k, l) = wm_interm_32_pt1(c, j, k, l) + sum 
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
sum = sum + r3(vrdav_Rl, a,k,b,i,c,j) * r2(vrdav_Rr, a,i,b,l)
end do 
end do 
end do 
wm_interm_33_pt1(c, k, j, l) = wm_interm_33_pt1(c, k, j, l) + sum 
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
wm_interm_34_pt1(c, k, j, l) = wm_interm_34_pt1(c, k, j, l) + sum 
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
sum = sum + r3(vrdav_Rl, a,j,b,k,c,i) * r2(vrdav_Rr, a,l,b,i)
end do 
end do 
end do 
wm_interm_35_pt1(c, j, k, l) = wm_interm_35_pt1(c, j, k, l) + sum 
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
wm_interm_36_pt1(c, j, k, l) = wm_interm_36_pt1(c, j, k, l) + sum 
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
wm_interm_37_pt1(c, k) = wm_interm_37_pt1(c, k) + sum 
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
wm_interm_38_pt1(c, k) = wm_interm_38_pt1(c, k) + sum 
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
wm_interm_39_pt1(c, k) = wm_interm_39_pt1(c, k) + sum 
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
wm_interm_40_pt1(c, k) = wm_interm_40_pt1(c, k) + sum 
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
wm_interm_41_pt1(c, j, k, l) = wm_interm_41_pt1(c, j, k, l) + sum 
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
wm_interm_42_pt1(c, j, k, l) = wm_interm_42_pt1(c, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 



end subroutine wm_intermediates_cc3_pt1

   subroutine wm_intermediates_cc3_init_pt2(nocc, nactive)
    integer, intent(in) :: nocc
    integer, intent(in) :: nactive
    allocate(wm_interm_0_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_1_pt2(1: nocc, 1: nocc))
allocate(wm_interm_2_pt2(1: nocc, 1: nocc))
allocate(wm_interm_3_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_4_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_5_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_6_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_7_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_8_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_9_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_10_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_11_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_12_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_13_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_14_pt2(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_15_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_16_pt2(1: nocc, 1: nocc))
allocate(wm_interm_17_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_18_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_19_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_20_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_21_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_22_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_23_pt2(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_24_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_25_pt2(1: nocc, 1: nocc))
allocate(wm_interm_26_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_27_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_28_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_29_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_30_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_31_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_32_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_33_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_34_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_35_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_36_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_37_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_38_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_39_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_40_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_41_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_42_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_43_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_44_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_45_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_46_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_47_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_48_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_49_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_50_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_51_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_52_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_53_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_54_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_55_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_56_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_57_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_58_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_59_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_60_pt2(1: nocc, 1: nocc))
allocate(wm_interm_61_pt2(1: nocc, 1: nocc))
allocate(wm_interm_62_pt2(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_63_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_64_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_65_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_66_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_67_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_68_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_69_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_70_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_71_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_72_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_73_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_74_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_75_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_76_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_77_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_78_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_79_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_80_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_81_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_82_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
wm_interm_0_pt2 = zero 
wm_interm_1_pt2 = zero 
wm_interm_2_pt2 = zero 
wm_interm_3_pt2 = zero 
wm_interm_4_pt2 = zero 
wm_interm_5_pt2 = zero 
wm_interm_6_pt2 = zero 
wm_interm_7_pt2 = zero 
wm_interm_8_pt2 = zero 
wm_interm_9_pt2 = zero 
wm_interm_10_pt2 = zero 
wm_interm_11_pt2 = zero 
wm_interm_12_pt2 = zero 
wm_interm_13_pt2 = zero 
wm_interm_14_pt2 = zero 
wm_interm_15_pt2 = zero 
wm_interm_16_pt2 = zero 
wm_interm_17_pt2 = zero 
wm_interm_18_pt2 = zero 
wm_interm_19_pt2 = zero 
wm_interm_20_pt2 = zero 
wm_interm_21_pt2 = zero 
wm_interm_22_pt2 = zero 
wm_interm_23_pt2 = zero 
wm_interm_24_pt2 = zero 
wm_interm_25_pt2 = zero 
wm_interm_26_pt2 = zero 
wm_interm_27_pt2 = zero 
wm_interm_28_pt2 = zero 
wm_interm_29_pt2 = zero 
wm_interm_30_pt2 = zero 
wm_interm_31_pt2 = zero 
wm_interm_32_pt2 = zero 
wm_interm_33_pt2 = zero 
wm_interm_34_pt2 = zero 
wm_interm_35_pt2 = zero 
wm_interm_36_pt2 = zero 
wm_interm_37_pt2 = zero 
wm_interm_38_pt2 = zero 
wm_interm_39_pt2 = zero 
wm_interm_40_pt2 = zero 
wm_interm_41_pt2 = zero 
wm_interm_42_pt2 = zero 
wm_interm_43_pt2 = zero 
wm_interm_44_pt2 = zero 
wm_interm_45_pt2 = zero 
wm_interm_46_pt2 = zero 
wm_interm_47_pt2 = zero 
wm_interm_48_pt2 = zero 
wm_interm_49_pt2 = zero 
wm_interm_50_pt2 = zero 
wm_interm_51_pt2 = zero 
wm_interm_52_pt2 = zero 
wm_interm_53_pt2 = zero 
wm_interm_54_pt2 = zero 
wm_interm_55_pt2 = zero 
wm_interm_56_pt2 = zero 
wm_interm_57_pt2 = zero 
wm_interm_58_pt2 = zero 
wm_interm_59_pt2 = zero 
wm_interm_60_pt2 = zero 
wm_interm_61_pt2 = zero 
wm_interm_62_pt2 = zero 
wm_interm_63_pt2 = zero 
wm_interm_64_pt2 = zero 
wm_interm_65_pt2 = zero 
wm_interm_66_pt2 = zero 
wm_interm_67_pt2 = zero 
wm_interm_68_pt2 = zero 
wm_interm_69_pt2 = zero 
wm_interm_70_pt2 = zero 
wm_interm_71_pt2 = zero 
wm_interm_72_pt2 = zero 
wm_interm_73_pt2 = zero 
wm_interm_74_pt2 = zero 
wm_interm_75_pt2 = zero 
wm_interm_76_pt2 = zero 
wm_interm_77_pt2 = zero 
wm_interm_78_pt2 = zero 
wm_interm_79_pt2 = zero 
wm_interm_80_pt2 = zero 
wm_interm_81_pt2 = zero 
wm_interm_82_pt2 = zero 

    end subroutine wm_intermediates_cc3_init_pt2
    
    subroutine wm_intermediates_cc3_free_pt2
    deallocate(wm_interm_0_pt2)
deallocate(wm_interm_1_pt2)
deallocate(wm_interm_2_pt2)
deallocate(wm_interm_3_pt2)
deallocate(wm_interm_4_pt2)
deallocate(wm_interm_5_pt2)
deallocate(wm_interm_6_pt2)
deallocate(wm_interm_7_pt2)
deallocate(wm_interm_8_pt2)
deallocate(wm_interm_9_pt2)
deallocate(wm_interm_10_pt2)
deallocate(wm_interm_11_pt2)
deallocate(wm_interm_12_pt2)
deallocate(wm_interm_13_pt2)
deallocate(wm_interm_14_pt2)
deallocate(wm_interm_15_pt2)
deallocate(wm_interm_16_pt2)
deallocate(wm_interm_17_pt2)
deallocate(wm_interm_18_pt2)
deallocate(wm_interm_19_pt2)
deallocate(wm_interm_20_pt2)
deallocate(wm_interm_21_pt2)
deallocate(wm_interm_22_pt2)
deallocate(wm_interm_23_pt2)
deallocate(wm_interm_24_pt2)
deallocate(wm_interm_25_pt2)
deallocate(wm_interm_26_pt2)
deallocate(wm_interm_27_pt2)
deallocate(wm_interm_28_pt2)
deallocate(wm_interm_29_pt2)
deallocate(wm_interm_30_pt2)
deallocate(wm_interm_31_pt2)
deallocate(wm_interm_32_pt2)
deallocate(wm_interm_33_pt2)
deallocate(wm_interm_34_pt2)
deallocate(wm_interm_35_pt2)
deallocate(wm_interm_36_pt2)
deallocate(wm_interm_37_pt2)
deallocate(wm_interm_38_pt2)
deallocate(wm_interm_39_pt2)
deallocate(wm_interm_40_pt2)
deallocate(wm_interm_41_pt2)
deallocate(wm_interm_42_pt2)
deallocate(wm_interm_43_pt2)
deallocate(wm_interm_44_pt2)
deallocate(wm_interm_45_pt2)
deallocate(wm_interm_46_pt2)
deallocate(wm_interm_47_pt2)
deallocate(wm_interm_48_pt2)
deallocate(wm_interm_49_pt2)
deallocate(wm_interm_50_pt2)
deallocate(wm_interm_51_pt2)
deallocate(wm_interm_52_pt2)
deallocate(wm_interm_53_pt2)
deallocate(wm_interm_54_pt2)
deallocate(wm_interm_55_pt2)
deallocate(wm_interm_56_pt2)
deallocate(wm_interm_57_pt2)
deallocate(wm_interm_58_pt2)
deallocate(wm_interm_59_pt2)
deallocate(wm_interm_60_pt2)
deallocate(wm_interm_61_pt2)
deallocate(wm_interm_62_pt2)
deallocate(wm_interm_63_pt2)
deallocate(wm_interm_64_pt2)
deallocate(wm_interm_65_pt2)
deallocate(wm_interm_66_pt2)
deallocate(wm_interm_67_pt2)
deallocate(wm_interm_68_pt2)
deallocate(wm_interm_69_pt2)
deallocate(wm_interm_70_pt2)
deallocate(wm_interm_71_pt2)
deallocate(wm_interm_72_pt2)
deallocate(wm_interm_73_pt2)
deallocate(wm_interm_74_pt2)
deallocate(wm_interm_75_pt2)
deallocate(wm_interm_76_pt2)
deallocate(wm_interm_77_pt2)
deallocate(wm_interm_78_pt2)
deallocate(wm_interm_79_pt2)
deallocate(wm_interm_80_pt2)
deallocate(wm_interm_81_pt2)
deallocate(wm_interm_82_pt2)

    end subroutine wm_intermediates_cc3_free_pt2
    
    subroutine wm_intermediates_cc3_pt2(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr)
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    real(F64) :: sum
    integer :: a, b, i, c, j, l, k, m 

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
wm_interm_0_pt2(c, j, l, k) = wm_interm_0_pt2(c, j, l, k) + sum 
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
wm_interm_1_pt2(j, k) = wm_interm_1_pt2(j, k) + sum 
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
wm_interm_2_pt2(j, k) = wm_interm_2_pt2(j, k) + sum 
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
wm_interm_3_pt2(c, j, k, l) = wm_interm_3_pt2(c, j, k, l) + sum 
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
wm_interm_4_pt2(c, j, k, l) = wm_interm_4_pt2(c, j, k, l) + sum 
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
wm_interm_5_pt2(b, c) = wm_interm_5_pt2(b, c) + sum 
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
wm_interm_6_pt2(b, c, j, k) = wm_interm_6_pt2(b, c, j, k) + sum 
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
sum = sum + s2(a,b,j,i) * t2(a,c,j,i)
end do 
end do 
end do 
wm_interm_7_pt2(b, c) = wm_interm_7_pt2(b, c) + sum 
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
wm_interm_8_pt2(b, c, j, k) = wm_interm_8_pt2(b, c, j, k) + sum 
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
sum = sum + r2(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,k,b,l,c,i)
end do 
end do 
end do 
wm_interm_9_pt2(c, j, k, l) = wm_interm_9_pt2(c, j, k, l) + sum 
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
wm_interm_10_pt2(b, c, j, k) = wm_interm_10_pt2(b, c, j, k) + sum 
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
wm_interm_11_pt2(b, c) = wm_interm_11_pt2(b, c) + sum 
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
wm_interm_12_pt2(b, c) = wm_interm_12_pt2(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, c, i, j, k, l, m, sum)& 
!$omp default(shared)
!$omp do collapse(6)
do c = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
do m = 1, nocc 
sum = zero 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,k,b,l,c,m)
end do 
end do 
wm_interm_13_pt2(c, i, j, k, l, m) = wm_interm_13_pt2(c, i, j, k, l, m) + sum 
end do 
end do 
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
wm_interm_14_pt2(i, j, k, l) = wm_interm_14_pt2(i, j, k, l) + sum 
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
wm_interm_15_pt2(b, c, j, k) = wm_interm_15_pt2(b, c, j, k) + sum 
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
wm_interm_16_pt2(j, k) = wm_interm_16_pt2(j, k) + sum 
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
sum = sum + r3(vrdav_Rr, a,k,b,i,c,j) * s2(a,b,i,l)
end do 
end do 
end do 
wm_interm_17_pt2(c, k, j, l) = wm_interm_17_pt2(c, k, j, l) + sum 
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
sum = sum + r3(vrdav_Rr, a,k,b,i,c,j) * s2(a,b,l,i)
end do 
end do 
end do 
wm_interm_18_pt2(c, k, j, l) = wm_interm_18_pt2(c, k, j, l) + sum 
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
sum = sum + r3(vrdav_Rr, a,j,b,k,c,i) * s2(a,b,i,l)
end do 
end do 
end do 
wm_interm_19_pt2(c, j, k, l) = wm_interm_19_pt2(c, j, k, l) + sum 
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
sum = sum + r3(vrdav_Rr, a,j,b,k,c,i) * s2(a,b,l,i)
end do 
end do 
end do 
wm_interm_20_pt2(c, j, k, l) = wm_interm_20_pt2(c, j, k, l) + sum 
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
sum = sum + r3(vrdav_Rr, a,j,b,i,c,k) * s2(a,b,i,l)
end do 
end do 
end do 
wm_interm_21_pt2(c, j, k, l) = wm_interm_21_pt2(c, j, k, l) + sum 
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
sum = sum + r3(vrdav_Rr, a,j,b,i,c,k) * s2(a,b,l,i)
end do 
end do 
end do 
wm_interm_22_pt2(c, j, k, l) = wm_interm_22_pt2(c, j, k, l) + sum 
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
wm_interm_23_pt2(i, j, k, l) = wm_interm_23_pt2(i, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, c, i, k, j, l, m, sum)& 
!$omp default(shared)
!$omp do collapse(6)
do c = nocc + 1, nactive 
do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do l = 1, nocc 
do m = 1, nocc 
sum = zero 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rr, a,i,b,k,c,j) * s2(a,b,l,m)
end do 
end do 
wm_interm_24_pt2(c, i, k, j, l, m) = wm_interm_24_pt2(c, i, k, j, l, m) + sum 
end do 
end do 
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
wm_interm_25_pt2(j, k) = wm_interm_25_pt2(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, c, j, k, i, l, m, sum)& 
!$omp default(shared)
!$omp do collapse(6)
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do m = 1, nocc 
sum = zero 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rr, a,j,b,k,c,i) * s2(a,b,l,m)
end do 
end do 
wm_interm_26_pt2(c, j, k, i, l, m) = wm_interm_26_pt2(c, j, k, i, l, m) + sum 
end do 
end do 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, c, i, j, k, l, m, sum)& 
!$omp default(shared)
!$omp do collapse(6)
do c = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
do m = 1, nocc 
sum = zero 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rr, a,i,b,j,c,k) * s2(a,b,l,m)
end do 
end do 
wm_interm_27_pt2(c, i, j, k, l, m) = wm_interm_27_pt2(c, i, j, k, l, m) + sum 
end do 
end do 
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
sum = sum + r3(vrdav_Rr, a,k,b,j,c,i) * s2(a,b,i,j)
end do 
end do 
end do 
end do 
wm_interm_28_pt2(c, k) = wm_interm_28_pt2(c, k) + sum 
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
sum = sum + r3(vrdav_Rr, a,i,b,j,c,k) * s2(a,b,i,j)
end do 
end do 
end do 
end do 
wm_interm_29_pt2(c, k) = wm_interm_29_pt2(c, k) + sum 
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
sum = sum + r3(vrdav_Rr, a,k,b,j,c,i) * s2(a,b,j,i)
end do 
end do 
end do 
end do 
wm_interm_30_pt2(c, k) = wm_interm_30_pt2(c, k) + sum 
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
sum = sum + r3(vrdav_Rr, a,i,b,j,c,k) * s2(a,b,j,i)
end do 
end do 
end do 
end do 
wm_interm_31_pt2(c, k) = wm_interm_31_pt2(c, k) + sum 
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
wm_interm_32_pt2(c, k) = wm_interm_32_pt2(c, k) + sum 
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
wm_interm_33_pt2(c, k) = wm_interm_33_pt2(c, k) + sum 
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
wm_interm_34_pt2(c, k) = wm_interm_34_pt2(c, k) + sum 
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
wm_interm_35_pt2(c, k) = wm_interm_35_pt2(c, k) + sum 
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
wm_interm_36_pt2(b, c, j, k) = wm_interm_36_pt2(b, c, j, k) + sum 
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
wm_interm_37_pt2(b, c, j, k) = wm_interm_37_pt2(b, c, j, k) + sum 
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
wm_interm_38_pt2(b, c, j, k) = wm_interm_38_pt2(b, c, j, k) + sum 
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
wm_interm_39_pt2(b, c, j, k) = wm_interm_39_pt2(b, c, j, k) + sum 
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
sum = sum + r3(vrdav_Rl, a,j,b,k,c,i) * r2(vrdav_Rr, a,l,b,i)
end do 
end do 
end do 
wm_interm_40_pt2(c, j, k, l) = wm_interm_40_pt2(c, j, k, l) + sum 
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
wm_interm_41_pt2(c, j, k, l) = wm_interm_41_pt2(c, j, k, l) + sum 
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
wm_interm_42_pt2(c, k) = wm_interm_42_pt2(c, k) + sum 
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
wm_interm_43_pt2(c, k) = wm_interm_43_pt2(c, k) + sum 
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
wm_interm_44_pt2(c, k, j, l) = wm_interm_44_pt2(c, k, j, l) + sum 
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
wm_interm_45_pt2(c, k, j, l) = wm_interm_45_pt2(c, k, j, l) + sum 
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
wm_interm_46_pt2(c, j, k, l) = wm_interm_46_pt2(c, j, k, l) + sum 
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
wm_interm_47_pt2(c, j, k, l) = wm_interm_47_pt2(c, j, k, l) + sum 
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
wm_interm_48_pt2(c, k) = wm_interm_48_pt2(c, k) + sum 
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
wm_interm_49_pt2(c, k) = wm_interm_49_pt2(c, k) + sum 
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
wm_interm_50_pt2(b, c, j, k) = wm_interm_50_pt2(b, c, j, k) + sum 
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
wm_interm_51_pt2(b, c, j, k) = wm_interm_51_pt2(b, c, j, k) + sum 
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
wm_interm_52_pt2(b, c, j, k) = wm_interm_52_pt2(b, c, j, k) + sum 
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
wm_interm_53_pt2(b, c, j, k) = wm_interm_53_pt2(b, c, j, k) + sum 
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
sum = sum + r2(vrdav_Rr, a,j,b,i) * s2(a,c,i,j)
end do 
end do 
end do 
wm_interm_54_pt2(b, c) = wm_interm_54_pt2(b, c) + sum 
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
wm_interm_55_pt2(b, c) = wm_interm_55_pt2(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, c, j, k, i, l, m, sum)& 
!$omp default(shared)
!$omp do collapse(6)
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do m = 1, nocc 
sum = zero 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,j,b,k,c,i) * r2(vrdav_Rr, a,l,b,m)
end do 
end do 
wm_interm_56_pt2(c, j, k, i, l, m) = wm_interm_56_pt2(c, j, k, i, l, m) + sum 
end do 
end do 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, c, i, k, j, l, m, sum)& 
!$omp default(shared)
!$omp do collapse(6)
do c = nocc + 1, nactive 
do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do l = 1, nocc 
do m = 1, nocc 
sum = zero 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,i,b,k,c,j) * r2(vrdav_Rr, a,l,b,m)
end do 
end do 
wm_interm_57_pt2(c, i, k, j, l, m) = wm_interm_57_pt2(c, i, k, j, l, m) + sum 
end do 
end do 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, c, i, j, k, l, m, sum)& 
!$omp default(shared)
!$omp do collapse(6)
do c = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
do m = 1, nocc 
sum = zero 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,i,b,j,c,k) * r2(vrdav_Rr, a,l,b,m)
end do 
end do 
wm_interm_58_pt2(c, i, j, k, l, m) = wm_interm_58_pt2(c, i, j, k, l, m) + sum 
end do 
end do 
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
sum = sum + r3(vrdav_Rl, a,j,b,k,c,i) * t2(a,b,i,l)
end do 
end do 
end do 
wm_interm_59_pt2(c, j, k, l) = wm_interm_59_pt2(c, j, k, l) + sum 
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
sum = sum + r2(vrdav_Rr, a,j,b,i) * s2(a,b,i,k)
end do 
end do 
end do 
wm_interm_60_pt2(j, k) = wm_interm_60_pt2(j, k) + sum 
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
wm_interm_61_pt2(j, k) = wm_interm_61_pt2(j, k) + sum 
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
wm_interm_62_pt2(i, j, k, l) = wm_interm_62_pt2(i, j, k, l) + sum 
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
sum = sum + r3(vrdav_Rl, a,j,b,i,c,k) * t2(a,b,i,l)
end do 
end do 
end do 
wm_interm_63_pt2(c, j, k, l) = wm_interm_63_pt2(c, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, c, i, k, j, l, m, sum)& 
!$omp default(shared)
!$omp do collapse(6)
do c = nocc + 1, nactive 
do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do l = 1, nocc 
do m = 1, nocc 
sum = zero 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,i,b,k,c,j) * t2(a,b,l,m)
end do 
end do 
wm_interm_64_pt2(c, i, k, j, l, m) = wm_interm_64_pt2(c, i, k, j, l, m) + sum 
end do 
end do 
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
sum = sum + r3(vrdav_Rl, a,k,b,i,c,j) * t2(a,b,l,i)
end do 
end do 
end do 
wm_interm_65_pt2(c, k, j, l) = wm_interm_65_pt2(c, k, j, l) + sum 
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
sum = sum + r3(vrdav_Rl, a,k,b,i,c,j) * t2(a,b,i,l)
end do 
end do 
end do 
wm_interm_66_pt2(c, k, j, l) = wm_interm_66_pt2(c, k, j, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, c, j, k, i, l, m, sum)& 
!$omp default(shared)
!$omp do collapse(6)
do c = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do m = 1, nocc 
sum = zero 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,j,b,k,c,i) * t2(a,b,l,m)
end do 
end do 
wm_interm_67_pt2(c, j, k, i, l, m) = wm_interm_67_pt2(c, j, k, i, l, m) + sum 
end do 
end do 
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
sum = sum + r3(vrdav_Rl, a,j,b,i,c,k) * t2(a,b,l,i)
end do 
end do 
end do 
wm_interm_68_pt2(c, j, k, l) = wm_interm_68_pt2(c, j, k, l) + sum 
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
sum = sum + r3(vrdav_Rl, a,j,b,k,c,i) * t2(a,b,l,i)
end do 
end do 
end do 
wm_interm_69_pt2(c, j, k, l) = wm_interm_69_pt2(c, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, c, i, j, k, l, m, sum)& 
!$omp default(shared)
!$omp do collapse(6)
do c = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
do m = 1, nocc 
sum = zero 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r3(vrdav_Rl, a,i,b,j,c,k) * t2(a,b,l,m)
end do 
end do 
wm_interm_70_pt2(c, i, j, k, l, m) = wm_interm_70_pt2(c, i, j, k, l, m) + sum 
end do 
end do 
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
sum = sum + r3(vrdav_Rl, a,k,b,j,c,i) * t2(a,b,i,j)
end do 
end do 
end do 
end do 
wm_interm_71_pt2(c, k) = wm_interm_71_pt2(c, k) + sum 
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
sum = sum + r3(vrdav_Rl, a,i,b,j,c,k) * t2(a,b,i,j)
end do 
end do 
end do 
end do 
wm_interm_72_pt2(c, k) = wm_interm_72_pt2(c, k) + sum 
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
sum = sum + r3(vrdav_Rl, a,k,b,j,c,i) * t2(a,b,j,i)
end do 
end do 
end do 
end do 
wm_interm_73_pt2(c, k) = wm_interm_73_pt2(c, k) + sum 
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
sum = sum + r3(vrdav_Rl, a,i,b,j,c,k) * t2(a,b,j,i)
end do 
end do 
end do 
end do 
wm_interm_74_pt2(c, k) = wm_interm_74_pt2(c, k) + sum 
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
wm_interm_75_pt2(b, c, j, k) = wm_interm_75_pt2(b, c, j, k) + sum 
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
wm_interm_76_pt2(b, c, k, j) = wm_interm_76_pt2(b, c, k, j) + sum 
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
wm_interm_77_pt2(b, c, k, j) = wm_interm_77_pt2(b, c, k, j) + sum 
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
wm_interm_78_pt2(b, c, j, k) = wm_interm_78_pt2(b, c, j, k) + sum 
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
wm_interm_79_pt2(b, c, k, j) = wm_interm_79_pt2(b, c, k, j) + sum 
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
wm_interm_80_pt2(b, c, j, k) = wm_interm_80_pt2(b, c, j, k) + sum 
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
wm_interm_81_pt2(b, c, k, j) = wm_interm_81_pt2(b, c, k, j) + sum 
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
wm_interm_82_pt2(b, c, j, k) = wm_interm_82_pt2(b, c, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 



end subroutine wm_intermediates_cc3_pt2



    
    function calc_D_oo_wm_cc3_pt0(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_oo_wm_cc3_pt0
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , j, i, c, b, a 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do j = 1, nocc 
do i = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(0) = term(0) + r3(vrdav_Rl, a,p,b,j,c,i) * r3(vrdav_Rr, a,i,b,q,c,j)
term(1) = term(1) + r3(vrdav_Rl, a,p,b,i,c,j) * r3(vrdav_Rr, a,j,b,q,c,i)
term(2) = term(2) + r3(vrdav_Rl, a,p,b,i,c,j) * r3(vrdav_Rr, a,q,b,j,c,i)
end do 
end do 
end do 
end do 
end do 

term(0) = term(0) * (-0.9999999999999998d+0) 
term(1) = term(1) * (-0.9999999999999998d+0) 
term(2) = term(2) * (1.9999999999999996d+0) 

do j = 1, nocc 
do c = nocc + 1, nactive 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(3) = term(3) + r3(vrdav_Rl, a,p,b,i,c,j) * r3(vrdav_Rr, a,i,b,q,c,j)
term(4) = term(4) + r3(vrdav_Rl, a,p,b,i,c,j) * r3(vrdav_Rr, a,q,b,i,c,j)
end do 
end do 
end do 
end do 
end do 

term(3) = term(3) * (1.9999999999999996d+0) 
term(4) = term(4) * (-3.999999999999999d+0) 

do i = 1, nocc 
do c = nocc + 1, nactive 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(5) = term(5) + r3(vrdav_Rl, a,p,b,j,c,i) * r3(vrdav_Rr, a,j,b,q,c,i)
end do 
end do 
end do 
end do 
end do 

term(5) = term(5) * (1.9999999999999996d+0) 


    calc_D_oo_wm_cc3_pt0 = zero
    do s = 0, 5
    calc_D_oo_wm_cc3_pt0 = calc_D_oo_wm_cc3_pt0 + term(s)
    end do

    end function calc_D_oo_wm_cc3_pt0
    
    function calc_D_ov_wm_cc3_pt0(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_ov_wm_cc3_pt0
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
    
    calc_D_ov_wm_cc3_pt0 = zero
    do s = 0, 0
    calc_D_ov_wm_cc3_pt0 = calc_D_ov_wm_cc3_pt0 + term(s)
    end do

    end function calc_D_ov_wm_cc3_pt0
    
    function calc_D_vo_wm_cc3_pt0(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vo_wm_cc3_pt0
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , i, j, b, a 
    real(F64), dimension(0:7) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(0) = term(0) + r2(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,q,b,j,p,i)
term(1) = term(1) + r2(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,q,b,j,p,i)
term(2) = term(2) + r3(vrdav_Rl, a,q,b,j,p,i) * r2(vrdav_Rr, a,j,b,i)
term(3) = term(3) + r3(vrdav_Rl, a,q,b,j,p,i) * r2(vrdav_Rr, a,i,b,j)
end do 
end do 
end do 
end do 

term(0) = term(0) * (1.9999999999999998d+0) 
term(1) = term(1) * (-3.9999999999999996d+0) 
term(2) = term(2) * (1.9999999999999998d+0) 
term(3) = term(3) * (-3.9999999999999996d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(4) = term(4) + r2(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,j,b,i,p,q)
term(5) = term(5) + r3(vrdav_Rl, a,i,b,j,p,q) * r2(vrdav_Rr, a,j,b,i)
end do 
end do 
end do 
end do 

term(4) = term(4) * (-1.9999999999999998d+0) 
term(5) = term(5) * (-2.0d+0) 

do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(6) = term(6) + r2(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,i,b,j,p,q)
term(7) = term(7) + r3(vrdav_Rl, a,i,b,j,p,q) * r2(vrdav_Rr, a,i,b,j)
end do 
end do 
end do 
end do 

term(6) = term(6) * (3.9999999999999996d+0) 
term(7) = term(7) * (4.0d+0) 


    calc_D_vo_wm_cc3_pt0 = zero
    do s = 0, 7
    calc_D_vo_wm_cc3_pt0 = calc_D_vo_wm_cc3_pt0 + term(s)
    end do

    end function calc_D_vo_wm_cc3_pt0
    
    function calc_D_vv_wm_cc3_pt0(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vv_wm_cc3_pt0
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , j, i, k, b, a 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(0) = term(0) + r3(vrdav_Rl, a,j,b,k,p,i) * r3(vrdav_Rr, a,k,b,i,q,j)
term(1) = term(1) + r3(vrdav_Rl, a,j,b,k,p,i) * r3(vrdav_Rr, a,i,b,k,q,j)
term(2) = term(2) + r3(vrdav_Rl, a,k,b,j,p,i) * r3(vrdav_Rr, a,i,b,k,q,j)
end do 
end do 
end do 
end do 
end do 

term(1) = term(1) * (-1.9999999999999996d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(3) = term(3) + r3(vrdav_Rl, a,k,b,j,p,i) * r3(vrdav_Rr, a,k,b,i,q,j)
end do 
end do 
end do 
end do 
end do 

term(3) = term(3) * (-1.9999999999999996d+0) 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(4) = term(4) + r3(vrdav_Rl, a,j,b,k,p,i) * r3(vrdav_Rr, a,k,b,j,q,i)
end do 
end do 
end do 
end do 
end do 

term(4) = term(4) * (-1.9999999999999996d+0) 

do i = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(5) = term(5) + r3(vrdav_Rl, a,j,b,k,p,i) * r3(vrdav_Rr, a,j,b,k,q,i)
end do 
end do 
end do 
end do 
end do 

term(5) = term(5) * (3.999999999999999d+0) 


    calc_D_vv_wm_cc3_pt0 = zero
    do s = 0, 5
    calc_D_vv_wm_cc3_pt0 = calc_D_vv_wm_cc3_pt0 + term(s)
    end do

    end function calc_D_vv_wm_cc3_pt0
    
    
    function calc_D_oo_wm_cc3_pt1(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_oo_wm_cc3_pt1
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
    real(F64), dimension(0:43) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(0) = term(0) + s2(a,b,i,q) * wm_interm_4_pt1(a,b,p,i)
term(1) = term(1) + s2(a,b,q,i) * wm_interm_5_pt1(a,b,i,p)
term(2) = term(2) + s2(a,b,q,i) * wm_interm_4_pt1(a,b,i,p)
term(3) = term(3) + s2(a,b,q,i) * wm_interm_4_pt1(a,b,p,i)
term(4) = term(4) + s2(a,b,q,i) * wm_interm_5_pt1(a,b,p,i)
term(5) = term(5) + s2(a,b,i,q) * wm_interm_5_pt1(a,b,p,i)
term(6) = term(6) + t2(a,b,q,i) * wm_interm_10_pt1(b,a,i,p)
term(7) = term(7) + t2(a,b,q,i) * wm_interm_11_pt1(b,a,p,i)
term(8) = term(8) + t2(a,b,q,i) * wm_interm_12_pt1(b,a,p,i)
term(9) = term(9) + t2(a,b,q,i) * wm_interm_13_pt1(b,a,i,p)
term(10) = term(10) + t2(a,b,q,i) * wm_interm_10_pt1(b,a,p,i)
term(11) = term(11) + t2(a,b,q,i) * wm_interm_12_pt1(b,a,i,p)
term(12) = term(12) + t2(a,b,q,i) * wm_interm_11_pt1(b,a,i,p)
term(13) = term(13) + t2(a,b,q,i) * wm_interm_13_pt1(b,a,p,i)
term(14) = term(14) + t2(a,b,q,i) * wm_interm_12_pt1(a,b,i,p)
term(15) = term(15) + t2(a,b,q,i) * wm_interm_11_pt1(a,b,i,p)
term(16) = term(16) + t2(a,b,q,i) * wm_interm_13_pt1(a,b,i,p)
term(17) = term(17) + t2(a,b,q,i) * wm_interm_14_pt1(a,b,i,p)
term(18) = term(18) + t2(a,b,q,i) * wm_interm_14_pt1(b,a,i,p)
term(19) = term(19) + t2(a,b,q,i) * wm_interm_10_pt1(a,b,p,i)
term(20) = term(20) + t2(a,b,q,i) * wm_interm_12_pt1(a,b,p,i)
term(21) = term(21) + t2(a,b,q,i) * wm_interm_10_pt1(a,b,i,p)
term(22) = term(22) + t2(a,b,q,i) * wm_interm_11_pt1(a,b,p,i)
term(23) = term(23) + t2(a,b,q,i) * wm_interm_13_pt1(a,b,p,i)
term(24) = term(24) + t2(a,b,q,i) * wm_interm_14_pt1(b,a,p,i)
term(25) = term(25) + t2(a,b,q,i) * wm_interm_14_pt1(a,b,p,i)
end do 
end do 
end do 

term(0) = term(0) * (1.9999999999999998d+0) 
term(1) = term(1) * (-2.0d+0) 
term(2) = term(2) * (1.9999999999999998d+0) 
term(3) = term(3) * (-3.9999999999999996d+0) 
term(4) = term(4) * (4.0d+0) 
term(5) = term(5) * (-2.0d+0) 
term(6) = term(6) * (0.3333333333333333d+0) 
term(7) = term(7) * (-2.0d+0) 
term(8) = term(8) * (0.6666666666666666d+0) 
term(9) = term(9) * (-1.3333333333333333d+0) 
term(10) = term(10) * (-0.6666666666666666d+0) 
term(11) = term(11) * (-1.3333333333333333d+0) 
term(12) = term(12) * (4.0d+0) 
term(13) = term(13) * (0.6666666666666666d+0) 
term(14) = term(14) * (0.6666666666666666d+0) 
term(15) = term(15) * (-2.0d+0) 
term(16) = term(16) * (0.6666666666666666d+0) 
term(17) = term(17) * (-0.6666666666666666d+0) 
term(18) = term(18) * (0.3333333333333333d+0) 
term(19) = term(19) * (0.3333333333333333d+0) 
term(20) = term(20) * (-1.3333333333333333d+0) 
term(21) = term(21) * (-0.6666666666666666d+0) 
term(22) = term(22) * (4.0d+0) 
term(23) = term(23) * (-1.3333333333333333d+0) 
term(24) = term(24) * (-0.6666666666666666d+0) 
term(25) = term(25) * (0.3333333333333333d+0) 

do a = nocc + 1, nactive 
term(26) = term(26) + r1(vrdav_Rl, a,q) * wm_interm_0_pt1(a,p)
term(27) = term(27) + r1(vrdav_Rl, a,q) * wm_interm_1_pt1(a,p)
term(28) = term(28) + r1(vrdav_Rl, a,q) * wm_interm_2_pt1(a,p)
term(29) = term(29) + r1(vrdav_Rl, a,q) * wm_interm_3_pt1(a,p)
term(30) = term(30) + r1(vrdav_Rr, a,p) * wm_interm_6_pt1(a,q)
term(31) = term(31) + r1(vrdav_Rr, a,p) * wm_interm_7_pt1(a,q)
term(32) = term(32) + r1(vrdav_Rr, a,p) * wm_interm_8_pt1(a,q)
term(33) = term(33) + r1(vrdav_Rr, a,p) * wm_interm_9_pt1(a,q)
end do 

term(26) = term(26) * (4.0d+0) 
term(27) = term(27) * (-4.0d+0) 
term(28) = term(28) * (-2.0d+0) 
term(29) = term(29) * (2.0d+0) 
term(30) = term(30) * (-3.9999999999999996d+0) 
term(31) = term(31) * (4.0d+0) 
term(32) = term(32) * (1.9999999999999998d+0) 
term(33) = term(33) * (-2.0d+0) 

do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(34) = term(34) + s2(a,b,i,q) * wm_interm_4_pt1(a,b,i,p)
term(35) = term(35) + s2(a,b,i,q) * wm_interm_5_pt1(a,b,i,p)
end do 
end do 
end do 

term(34) = term(34) * (-3.9999999999999996d+0) 
term(35) = term(35) * (4.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(36) = term(36) + r1(vrdav_Rl, a,i) * wm_interm_0_pt1(a,i)
term(37) = term(37) + r1(vrdav_Rl, a,i) * wm_interm_1_pt1(a,i)
term(38) = term(38) + r1(vrdav_Rl, a,i) * wm_interm_2_pt1(a,i)
term(39) = term(39) + r1(vrdav_Rl, a,i) * wm_interm_3_pt1(a,i)
term(40) = term(40) + r1(vrdav_Rr, a,i) * wm_interm_6_pt1(a,i)
term(41) = term(41) + r1(vrdav_Rr, a,i) * wm_interm_7_pt1(a,i)
term(42) = term(42) + r1(vrdav_Rr, a,i) * wm_interm_8_pt1(a,i)
term(43) = term(43) + r1(vrdav_Rr, a,i) * wm_interm_9_pt1(a,i)
end do 
end do 

term(36) = term(36) * (-8.0d+0) 
term(37) = term(37) * (8.0d+0) 
term(38) = term(38) * (4.0d+0) 
term(39) = term(39) * (-4.0d+0) 
term(40) = term(40) * (7.999999999999999d+0) 
term(41) = term(41) * (-8.0d+0) 
term(42) = term(42) * (-3.9999999999999996d+0) 
term(43) = term(43) * (4.0d+0) 


    calc_D_oo_wm_cc3_pt1 = zero
    do s = 0, 43
    calc_D_oo_wm_cc3_pt1 = calc_D_oo_wm_cc3_pt1 + term(s)
    end do

    end function calc_D_oo_wm_cc3_pt1
    
    function calc_D_ov_wm_cc3_pt1(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_ov_wm_cc3_pt1
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , i, j, a, b 
    real(F64), dimension(0:19) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + s2(a,q,j,i) * wm_interm_19_pt1(a,p,j,i)
term(1) = term(1) + s2(a,q,j,i) * wm_interm_20_pt1(a,p,j,i)
term(2) = term(2) + s2(a,q,j,i) * wm_interm_21_pt1(a,p,j,i)
term(3) = term(3) + s2(a,q,j,i) * wm_interm_22_pt1(a,p,j,i)
term(4) = term(4) + t2(a,q,j,i) * wm_interm_33_pt1(a,i,j,p)
term(5) = term(5) + t2(a,q,j,i) * wm_interm_34_pt1(a,i,j,p)
term(6) = term(6) + t2(a,q,j,i) * wm_interm_35_pt1(a,j,i,p)
term(7) = term(7) + t2(a,q,j,i) * wm_interm_36_pt1(a,j,i,p)
term(8) = term(8) + t2(a,q,j,i) * wm_interm_36_pt1(a,i,j,p)
term(9) = term(9) + t2(a,q,j,i) * wm_interm_35_pt1(a,i,j,p)
term(10) = term(10) + t2(a,q,j,i) * wm_interm_33_pt1(a,j,i,p)
term(11) = term(11) + t2(a,q,j,i) * wm_interm_34_pt1(a,j,i,p)
term(12) = term(12) + t2(a,q,j,i) * wm_interm_41_pt1(a,j,i,p)
term(13) = term(13) + t2(a,q,j,i) * wm_interm_42_pt1(a,j,i,p)
term(14) = term(14) + t2(a,q,j,i) * wm_interm_41_pt1(a,i,j,p)
term(15) = term(15) + t2(a,q,j,i) * wm_interm_42_pt1(a,i,j,p)
end do 
end do 
end do 

term(0) = term(0) * (1.9999999999999998d+0) 
term(1) = term(1) * (-3.9999999999999996d+0) 
term(3) = term(3) * (-1.9999999999999998d+0) 
term(5) = term(5) * (-2.0d+0) 
term(7) = term(7) * (-2.0d+0) 
term(9) = term(9) * (-2.0d+0) 
term(10) = term(10) * (-2.0d+0) 
term(11) = term(11) * (4.0d+0) 
term(13) = term(13) * (-2.0d+0) 
term(14) = term(14) * (-2.0d+0) 
term(15) = term(15) * (4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(16) = term(16) + s2(a,q,j,i) * wm_interm_21_pt1(a,p,i,j)
term(17) = term(17) + s2(a,q,j,i) * wm_interm_19_pt1(a,p,i,j)
term(18) = term(18) + s2(a,q,j,i) * wm_interm_20_pt1(a,p,i,j)
term(19) = term(19) + s2(a,q,j,i) * wm_interm_22_pt1(a,p,i,j)
end do 
end do 
end do 

term(16) = term(16) * (-1.9999999999999998d+0) 
term(17) = term(17) * (-3.9999999999999996d+0) 
term(18) = term(18) * (7.999999999999999d+0) 


    calc_D_ov_wm_cc3_pt1 = zero
    do s = 0, 19
    calc_D_ov_wm_cc3_pt1 = calc_D_ov_wm_cc3_pt1 + term(s)
    end do

    end function calc_D_ov_wm_cc3_pt1
    
    function calc_D_vo_wm_cc3_pt1(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vo_wm_cc3_pt1
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , i, j, a, b 
    real(F64), dimension(0:51) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + r2(vrdav_Rl, a,j,p,i) * wm_interm_23_pt1(a,i,j,q)
term(1) = term(1) + r2(vrdav_Rl, a,j,p,i) * wm_interm_23_pt1(a,j,i,q)
term(2) = term(2) + r2(vrdav_Rl, a,j,p,i) * wm_interm_24_pt1(a,i,j,q)
term(3) = term(3) + r2(vrdav_Rl, a,j,p,i) * wm_interm_25_pt1(a,i,j,q)
term(4) = term(4) + r2(vrdav_Rl, a,j,p,i) * wm_interm_26_pt1(a,i,j,q)
term(5) = term(5) + r2(vrdav_Rl, a,j,p,i) * wm_interm_25_pt1(a,j,i,q)
term(6) = term(6) + r2(vrdav_Rl, a,j,p,i) * wm_interm_26_pt1(a,j,i,q)
term(7) = term(7) + r2(vrdav_Rl, a,j,p,i) * wm_interm_24_pt1(a,j,i,q)
term(8) = term(8) + r2(vrdav_Rr, a,j,p,i) * wm_interm_27_pt1(a,j,i,q)
term(9) = term(9) + r2(vrdav_Rr, a,j,p,i) * wm_interm_27_pt1(a,i,j,q)
term(10) = term(10) + r2(vrdav_Rr, a,j,p,i) * wm_interm_28_pt1(a,i,j,q)
term(11) = term(11) + r2(vrdav_Rr, a,j,p,i) * wm_interm_28_pt1(a,j,i,q)
term(12) = term(12) + r2(vrdav_Rr, a,j,p,i) * wm_interm_29_pt1(a,j,i,q)
term(13) = term(13) + r2(vrdav_Rr, a,j,p,i) * wm_interm_29_pt1(a,i,j,q)
term(14) = term(14) + r2(vrdav_Rr, a,j,p,i) * wm_interm_30_pt1(a,i,j,q)
term(15) = term(15) + r2(vrdav_Rr, a,j,p,i) * wm_interm_30_pt1(a,j,i,q)
term(16) = term(16) + r2(vrdav_Rr, a,j,p,i) * wm_interm_31_pt1(a,i,j,q)
term(17) = term(17) + r2(vrdav_Rr, a,j,p,i) * wm_interm_31_pt1(a,j,i,q)
term(18) = term(18) + r2(vrdav_Rr, a,j,p,i) * wm_interm_32_pt1(a,j,i,q)
term(19) = term(19) + r2(vrdav_Rr, a,j,p,i) * wm_interm_32_pt1(a,i,j,q)
end do 
end do 
end do 

term(0) = term(0) * (1.9999999999999998d+0) 
term(1) = term(1) * (-3.9999999999999996d+0) 
term(3) = term(3) * (-3.9999999999999996d+0) 
term(4) = term(4) * (-1.9999999999999998d+0) 
term(5) = term(5) * (7.999999999999999d+0) 
term(7) = term(7) * (-1.9999999999999998d+0) 
term(9) = term(9) * (-2.0d+0) 
term(11) = term(11) * (-2.0d+0) 
term(12) = term(12) * (-2.0d+0) 
term(13) = term(13) * (4.0d+0) 
term(14) = term(14) * (-2.0d+0) 
term(15) = term(15) * (4.0d+0) 
term(17) = term(17) * (-2.0d+0) 
term(19) = term(19) * (-2.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(20) = term(20) + r2(vrdav_Rl, a,q,p,i) * wm_interm_0_pt1(a,i)
term(21) = term(21) + r2(vrdav_Rl, a,q,p,i) * wm_interm_1_pt1(a,i)
term(22) = term(22) + r2(vrdav_Rl, a,i,p,q) * wm_interm_0_pt1(a,i)
term(23) = term(23) + r2(vrdav_Rl, a,i,p,q) * wm_interm_1_pt1(a,i)
term(24) = term(24) + r2(vrdav_Rl, a,q,p,i) * wm_interm_2_pt1(a,i)
term(25) = term(25) + r2(vrdav_Rl, a,i,p,q) * wm_interm_2_pt1(a,i)
term(26) = term(26) + r2(vrdav_Rl, a,q,p,i) * wm_interm_3_pt1(a,i)
term(27) = term(27) + r2(vrdav_Rl, a,i,p,q) * wm_interm_3_pt1(a,i)
term(28) = term(28) + s2(a,p,q,i) * wm_interm_15_pt1(a,i)
term(29) = term(29) + s2(a,p,q,i) * wm_interm_16_pt1(a,i)
term(30) = term(30) + s2(a,p,q,i) * wm_interm_17_pt1(a,i)
term(31) = term(31) + s2(a,p,q,i) * wm_interm_18_pt1(a,i)
term(32) = term(32) + s2(a,p,i,q) * wm_interm_15_pt1(a,i)
term(33) = term(33) + s2(a,p,i,q) * wm_interm_16_pt1(a,i)
term(34) = term(34) + s2(a,p,i,q) * wm_interm_17_pt1(a,i)
term(35) = term(35) + s2(a,p,i,q) * wm_interm_18_pt1(a,i)
term(36) = term(36) + r2(vrdav_Rr, a,q,p,i) * wm_interm_6_pt1(a,i)
term(37) = term(37) + r2(vrdav_Rr, a,i,p,q) * wm_interm_6_pt1(a,i)
term(38) = term(38) + r2(vrdav_Rr, a,q,p,i) * wm_interm_7_pt1(a,i)
term(39) = term(39) + r2(vrdav_Rr, a,i,p,q) * wm_interm_7_pt1(a,i)
term(40) = term(40) + r2(vrdav_Rr, a,q,p,i) * wm_interm_8_pt1(a,i)
term(41) = term(41) + r2(vrdav_Rr, a,i,p,q) * wm_interm_8_pt1(a,i)
term(42) = term(42) + r2(vrdav_Rr, a,q,p,i) * wm_interm_9_pt1(a,i)
term(43) = term(43) + r2(vrdav_Rr, a,i,p,q) * wm_interm_9_pt1(a,i)
term(44) = term(44) + t2(a,p,i,q) * wm_interm_37_pt1(a,i)
term(45) = term(45) + t2(a,p,i,q) * wm_interm_38_pt1(a,i)
term(46) = term(46) + t2(a,p,i,q) * wm_interm_39_pt1(a,i)
term(47) = term(47) + t2(a,p,i,q) * wm_interm_40_pt1(a,i)
term(48) = term(48) + t2(a,p,q,i) * wm_interm_37_pt1(a,i)
term(49) = term(49) + t2(a,p,q,i) * wm_interm_38_pt1(a,i)
term(50) = term(50) + t2(a,p,q,i) * wm_interm_39_pt1(a,i)
term(51) = term(51) + t2(a,p,q,i) * wm_interm_40_pt1(a,i)
end do 
end do 

term(20) = term(20) * (4.0d+0) 
term(21) = term(21) * (-4.0d+0) 
term(22) = term(22) * (-8.0d+0) 
term(23) = term(23) * (8.0d+0) 
term(24) = term(24) * (-2.0d+0) 
term(25) = term(25) * (4.0d+0) 
term(26) = term(26) * (2.0d+0) 
term(27) = term(27) * (-4.0d+0) 
term(28) = term(28) * (1.9999999999999998d+0) 
term(29) = term(29) * (-3.9999999999999996d+0) 
term(30) = term(30) * (-1.9999999999999998d+0) 
term(31) = term(31) * (3.9999999999999996d+0) 
term(32) = term(32) * (-3.9999999999999996d+0) 
term(33) = term(33) * (7.999999999999999d+0) 
term(34) = term(34) * (3.9999999999999996d+0) 
term(35) = term(35) * (-7.999999999999999d+0) 
term(36) = term(36) * (-3.9999999999999996d+0) 
term(37) = term(37) * (7.999999999999999d+0) 
term(38) = term(38) * (4.0d+0) 
term(39) = term(39) * (-8.0d+0) 
term(40) = term(40) * (1.9999999999999998d+0) 
term(41) = term(41) * (-3.9999999999999996d+0) 
term(42) = term(42) * (-2.0d+0) 
term(43) = term(43) * (4.0d+0) 
term(44) = term(44) * (-3.9999999999999996d+0) 
term(45) = term(45) * (7.999999999999999d+0) 
term(46) = term(46) * (4.0d+0) 
term(47) = term(47) * (-8.0d+0) 
term(48) = term(48) * (1.9999999999999998d+0) 
term(49) = term(49) * (-3.9999999999999996d+0) 
term(50) = term(50) * (-2.0d+0) 
term(51) = term(51) * (4.0d+0) 


    calc_D_vo_wm_cc3_pt1 = zero
    do s = 0, 51
    calc_D_vo_wm_cc3_pt1 = calc_D_vo_wm_cc3_pt1 + term(s)
    end do

    end function calc_D_vo_wm_cc3_pt1
    
    function calc_D_vv_wm_cc3_pt1(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vv_wm_cc3_pt1
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
    real(F64), dimension(0:35) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + s2(a,q,j,i) * wm_interm_4_pt1(a,p,i,j)
term(1) = term(1) + s2(a,q,j,i) * wm_interm_4_pt1(p,a,i,j)
term(2) = term(2) + s2(a,q,j,i) * wm_interm_5_pt1(p,a,i,j)
term(3) = term(3) + s2(a,q,j,i) * wm_interm_5_pt1(a,p,i,j)
term(4) = term(4) + t2(a,q,j,i) * wm_interm_11_pt1(a,p,i,j)
term(5) = term(5) + t2(a,q,j,i) * wm_interm_12_pt1(a,p,i,j)
term(6) = term(6) + t2(a,q,j,i) * wm_interm_10_pt1(a,p,i,j)
term(7) = term(7) + t2(a,q,j,i) * wm_interm_13_pt1(a,p,i,j)
term(8) = term(8) + t2(a,q,j,i) * wm_interm_10_pt1(p,a,i,j)
term(9) = term(9) + t2(a,q,j,i) * wm_interm_12_pt1(p,a,i,j)
term(10) = term(10) + t2(a,q,j,i) * wm_interm_11_pt1(p,a,i,j)
term(11) = term(11) + t2(a,q,j,i) * wm_interm_13_pt1(p,a,i,j)
term(12) = term(12) + t2(a,q,j,i) * wm_interm_14_pt1(a,p,i,j)
term(13) = term(13) + t2(a,q,j,i) * wm_interm_14_pt1(p,a,i,j)
end do 
end do 
end do 

term(0) = term(0) * (-1.9999999999999998d+0) 
term(1) = term(1) * (3.9999999999999996d+0) 
term(2) = term(2) * (-4.0d+0) 
term(3) = term(3) * (2.0d+0) 
term(4) = term(4) * (2.0d+0) 
term(5) = term(5) * (-0.6666666666666666d+0) 
term(6) = term(6) * (0.6666666666666666d+0) 
term(7) = term(7) * (-0.6666666666666666d+0) 
term(8) = term(8) * (-0.3333333333333333d+0) 
term(9) = term(9) * (1.3333333333333333d+0) 
term(10) = term(10) * (-4.0d+0) 
term(11) = term(11) * (1.3333333333333333d+0) 
term(12) = term(12) * (0.6666666666666666d+0) 
term(13) = term(13) * (-0.3333333333333333d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(14) = term(14) + s2(a,q,j,i) * wm_interm_5_pt1(p,a,j,i)
term(15) = term(15) + s2(a,q,j,i) * wm_interm_4_pt1(p,a,j,i)
term(16) = term(16) + s2(a,q,j,i) * wm_interm_4_pt1(a,p,j,i)
term(17) = term(17) + s2(a,q,j,i) * wm_interm_5_pt1(a,p,j,i)
term(18) = term(18) + t2(a,q,j,i) * wm_interm_10_pt1(a,p,j,i)
term(19) = term(19) + t2(a,q,j,i) * wm_interm_13_pt1(a,p,j,i)
term(20) = term(20) + t2(a,q,j,i) * wm_interm_12_pt1(a,p,j,i)
term(21) = term(21) + t2(a,q,j,i) * wm_interm_11_pt1(a,p,j,i)
term(22) = term(22) + t2(a,q,j,i) * wm_interm_12_pt1(p,a,j,i)
term(23) = term(23) + t2(a,q,j,i) * wm_interm_11_pt1(p,a,j,i)
term(24) = term(24) + t2(a,q,j,i) * wm_interm_13_pt1(p,a,j,i)
term(25) = term(25) + t2(a,q,j,i) * wm_interm_14_pt1(p,a,j,i)
term(26) = term(26) + t2(a,q,j,i) * wm_interm_14_pt1(a,p,j,i)
term(27) = term(27) + t2(a,q,j,i) * wm_interm_10_pt1(p,a,j,i)
end do 
end do 
end do 

term(14) = term(14) * (2.0d+0) 
term(15) = term(15) * (-1.9999999999999998d+0) 
term(16) = term(16) * (3.9999999999999996d+0) 
term(17) = term(17) * (-4.0d+0) 
term(18) = term(18) * (-0.3333333333333333d+0) 
term(19) = term(19) * (1.3333333333333333d+0) 
term(20) = term(20) * (1.3333333333333333d+0) 
term(21) = term(21) * (-4.0d+0) 
term(22) = term(22) * (-0.6666666666666666d+0) 
term(23) = term(23) * (2.0d+0) 
term(24) = term(24) * (-0.6666666666666666d+0) 
term(25) = term(25) * (0.6666666666666666d+0) 
term(26) = term(26) * (-0.3333333333333333d+0) 
term(27) = term(27) * (0.6666666666666666d+0) 

do i = 1, nocc 
term(28) = term(28) + r1(vrdav_Rl, q,i) * wm_interm_0_pt1(p,i)
term(29) = term(29) + r1(vrdav_Rl, q,i) * wm_interm_1_pt1(p,i)
term(30) = term(30) + r1(vrdav_Rl, q,i) * wm_interm_2_pt1(p,i)
term(31) = term(31) + r1(vrdav_Rl, q,i) * wm_interm_3_pt1(p,i)
term(32) = term(32) + r1(vrdav_Rr, p,i) * wm_interm_6_pt1(q,i)
term(33) = term(33) + r1(vrdav_Rr, p,i) * wm_interm_7_pt1(q,i)
term(34) = term(34) + r1(vrdav_Rr, p,i) * wm_interm_8_pt1(q,i)
term(35) = term(35) + r1(vrdav_Rr, p,i) * wm_interm_9_pt1(q,i)
end do 

term(28) = term(28) * (-4.0d+0) 
term(29) = term(29) * (4.0d+0) 
term(30) = term(30) * (2.0d+0) 
term(31) = term(31) * (-2.0d+0) 
term(32) = term(32) * (3.9999999999999996d+0) 
term(33) = term(33) * (-4.0d+0) 
term(34) = term(34) * (-1.9999999999999998d+0) 
term(35) = term(35) * (2.0d+0) 


    calc_D_vv_wm_cc3_pt1 = zero
    do s = 0, 35
    calc_D_vv_wm_cc3_pt1 = calc_D_vv_wm_cc3_pt1 + term(s)
    end do

    end function calc_D_vv_wm_cc3_pt1
    
    
    function calc_D_oo_wm_cc3_pt2(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_oo_wm_cc3_pt2
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
    real(F64), dimension(0:47) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(0) = term(0) + r2(vrdav_Rl, a,q,b,i) * wm_interm_75_pt2(a,b,i,p)
term(1) = term(1) + r2(vrdav_Rl, a,q,b,i) * wm_interm_75_pt2(a,b,p,i)
term(2) = term(2) + r2(vrdav_Rl, a,q,b,i) * wm_interm_75_pt2(b,a,p,i)
term(3) = term(3) + r2(vrdav_Rl, a,q,b,i) * wm_interm_75_pt2(b,a,i,p)
term(4) = term(4) + r2(vrdav_Rl, a,q,b,i) * wm_interm_76_pt2(a,b,i,p)
term(5) = term(5) + r2(vrdav_Rl, a,q,b,i) * wm_interm_76_pt2(a,b,p,i)
term(6) = term(6) + r2(vrdav_Rl, a,q,b,i) * wm_interm_77_pt2(a,b,p,i)
term(7) = term(7) + r2(vrdav_Rl, a,q,b,i) * wm_interm_77_pt2(a,b,i,p)
term(8) = term(8) + r2(vrdav_Rl, a,q,b,i) * wm_interm_76_pt2(b,a,p,i)
term(9) = term(9) + r2(vrdav_Rl, a,q,b,i) * wm_interm_76_pt2(b,a,i,p)
term(10) = term(10) + r2(vrdav_Rl, a,q,b,i) * wm_interm_77_pt2(b,a,i,p)
term(11) = term(11) + r2(vrdav_Rl, a,q,b,i) * wm_interm_77_pt2(b,a,p,i)
term(12) = term(12) + r2(vrdav_Rr, a,p,b,i) * wm_interm_78_pt2(b,a,q,i)
term(13) = term(13) + r2(vrdav_Rr, a,p,b,i) * wm_interm_78_pt2(a,b,q,i)
term(14) = term(14) + r2(vrdav_Rr, a,p,b,i) * wm_interm_78_pt2(a,b,i,q)
term(15) = term(15) + r2(vrdav_Rr, a,p,b,i) * wm_interm_78_pt2(b,a,i,q)
term(16) = term(16) + r2(vrdav_Rr, a,p,b,i) * wm_interm_79_pt2(b,a,q,i)
term(17) = term(17) + r2(vrdav_Rr, a,p,b,i) * wm_interm_79_pt2(a,b,q,i)
term(18) = term(18) + r2(vrdav_Rr, a,p,b,i) * wm_interm_79_pt2(a,b,i,q)
term(19) = term(19) + r2(vrdav_Rr, a,p,b,i) * wm_interm_79_pt2(b,a,i,q)
term(20) = term(20) + r2(vrdav_Rr, a,p,b,i) * wm_interm_80_pt2(b,a,q,i)
term(21) = term(21) + r2(vrdav_Rr, a,p,b,i) * wm_interm_80_pt2(a,b,q,i)
term(22) = term(22) + r2(vrdav_Rr, a,p,b,i) * wm_interm_81_pt2(a,b,q,i)
term(23) = term(23) + r2(vrdav_Rr, a,p,b,i) * wm_interm_81_pt2(b,a,q,i)
term(24) = term(24) + r2(vrdav_Rr, a,p,b,i) * wm_interm_80_pt2(a,b,i,q)
term(25) = term(25) + r2(vrdav_Rr, a,p,b,i) * wm_interm_80_pt2(b,a,i,q)
term(26) = term(26) + r2(vrdav_Rr, a,p,b,i) * wm_interm_81_pt2(b,a,i,q)
term(27) = term(27) + r2(vrdav_Rr, a,p,b,i) * wm_interm_81_pt2(a,b,i,q)
term(28) = term(28) + r2(vrdav_Rr, a,p,b,i) * wm_interm_82_pt2(b,a,i,q)
term(29) = term(29) + r2(vrdav_Rr, a,p,b,i) * wm_interm_82_pt2(a,b,i,q)
term(30) = term(30) + r2(vrdav_Rr, a,p,b,i) * wm_interm_82_pt2(a,b,q,i)
term(31) = term(31) + r2(vrdav_Rr, a,p,b,i) * wm_interm_82_pt2(b,a,q,i)
end do 
end do 
end do 

term(0) = term(0) * (2.0d+0) 
term(1) = term(1) * (-4.0d+0) 
term(2) = term(2) * (2.0d+0) 
term(3) = term(3) * (-4.0d+0) 
term(4) = term(4) * (-1.0d+0) 
term(5) = term(5) * (2.0d+0) 
term(6) = term(6) * (-1.0d+0) 
term(7) = term(7) * (2.0d+0) 
term(8) = term(8) * (-1.0d+0) 
term(9) = term(9) * (2.0d+0) 
term(10) = term(10) * (-1.0d+0) 
term(11) = term(11) * (2.0d+0) 
term(12) = term(12) * (-2.0d+0) 
term(13) = term(13) * (4.0d+0) 
term(14) = term(14) * (-2.0d+0) 
term(15) = term(15) * (4.0d+0) 
term(16) = term(16) * (0.6666666666666666d+0) 
term(17) = term(17) * (-1.3333333333333333d+0) 
term(18) = term(18) * (0.6666666666666666d+0) 
term(19) = term(19) * (-1.3333333333333333d+0) 
term(20) = term(20) * (0.6666666666666666d+0) 
term(21) = term(21) * (-1.3333333333333333d+0) 
term(22) = term(22) * (0.3333333333333333d+0) 
term(23) = term(23) * (-0.6666666666666666d+0) 
term(24) = term(24) * (0.6666666666666666d+0) 
term(25) = term(25) * (-1.3333333333333333d+0) 
term(26) = term(26) * (0.3333333333333333d+0) 
term(27) = term(27) * (-0.6666666666666666d+0) 
term(28) = term(28) * (0.3333333333333333d+0) 
term(29) = term(29) * (-0.6666666666666666d+0) 
term(30) = term(30) * (0.3333333333333333d+0) 
term(31) = term(31) * (-0.6666666666666666d+0) 

do a = nocc + 1, nactive 
term(32) = term(32) + s1(a,q) * wm_interm_32_pt2(a,p)
term(33) = term(33) + s1(a,q) * wm_interm_33_pt2(a,p)
term(34) = term(34) + s1(a,q) * wm_interm_34_pt2(a,p)
term(35) = term(35) + s1(a,q) * wm_interm_35_pt2(a,p)
term(36) = term(36) + t1(a,q) * wm_interm_42_pt2(a,p)
term(37) = term(37) + t1(a,q) * wm_interm_43_pt2(a,p)
term(38) = term(38) + t1(a,q) * wm_interm_48_pt2(a,p)
term(39) = term(39) + t1(a,q) * wm_interm_49_pt2(a,p)
end do 

term(32) = term(32) * (1.9999999999999998d+0) 
term(33) = term(33) * (-3.9999999999999996d+0) 
term(34) = term(34) * (-1.9999999999999998d+0) 
term(35) = term(35) * (3.9999999999999996d+0) 
term(36) = term(36) * (1.9999999999999998d+0) 
term(37) = term(37) * (-3.9999999999999996d+0) 
term(38) = term(38) * (-2.0d+0) 
term(39) = term(39) * (4.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(40) = term(40) + s1(a,i) * wm_interm_34_pt2(a,i)
term(41) = term(41) + s1(a,i) * wm_interm_35_pt2(a,i)
term(42) = term(42) + s1(a,i) * wm_interm_32_pt2(a,i)
term(43) = term(43) + s1(a,i) * wm_interm_33_pt2(a,i)
term(44) = term(44) + t1(a,i) * wm_interm_48_pt2(a,i)
term(45) = term(45) + t1(a,i) * wm_interm_49_pt2(a,i)
term(46) = term(46) + t1(a,i) * wm_interm_42_pt2(a,i)
term(47) = term(47) + t1(a,i) * wm_interm_43_pt2(a,i)
end do 
end do 

term(40) = term(40) * (-4.0d+0) 
term(41) = term(41) * (8.0d+0) 
term(42) = term(42) * (4.0d+0) 
term(43) = term(43) * (-8.0d+0) 
term(44) = term(44) * (4.0d+0) 
term(45) = term(45) * (-8.0d+0) 
term(46) = term(46) * (-3.9999999999999996d+0) 
term(47) = term(47) * (7.999999999999999d+0) 


    calc_D_oo_wm_cc3_pt2 = zero
    do s = 0, 47
    calc_D_oo_wm_cc3_pt2 = calc_D_oo_wm_cc3_pt2 + term(s)
    end do

    end function calc_D_oo_wm_cc3_pt2
    
    function calc_D_ov_wm_cc3_pt2(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_ov_wm_cc3_pt2
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , j, i, a, k, b, l, c 
    real(F64), dimension(0:231) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + wm_interm_36_pt2(a,q,i,j) * wm_interm_3_pt2(a,j,i,p)
term(1) = term(1) + wm_interm_36_pt2(a,q,i,j) * wm_interm_4_pt2(a,j,i,p)
term(2) = term(2) + wm_interm_36_pt2(a,q,i,j) * wm_interm_9_pt2(a,j,i,p)
term(3) = term(3) + wm_interm_36_pt2(a,q,i,j) * wm_interm_9_pt2(a,j,p,i)
term(4) = term(4) + wm_interm_36_pt2(a,q,i,j) * wm_interm_3_pt2(a,j,p,i)
term(5) = term(5) + wm_interm_36_pt2(a,q,i,j) * wm_interm_4_pt2(a,j,p,i)
term(6) = term(6) + wm_interm_0_pt2(a,i,p,j) * wm_interm_36_pt2(a,q,j,i)
term(7) = term(7) + wm_interm_39_pt2(a,q,i,j) * wm_interm_3_pt2(a,j,p,i)
term(8) = term(8) + wm_interm_39_pt2(a,q,i,j) * wm_interm_4_pt2(a,j,p,i)
term(9) = term(9) + wm_interm_39_pt2(a,q,i,j) * wm_interm_9_pt2(a,j,p,i)
term(10) = term(10) + wm_interm_39_pt2(a,q,i,j) * wm_interm_9_pt2(a,j,i,p)
term(11) = term(11) + wm_interm_39_pt2(a,q,i,j) * wm_interm_3_pt2(a,j,i,p)
term(12) = term(12) + wm_interm_39_pt2(a,q,i,j) * wm_interm_4_pt2(a,j,i,p)
term(13) = term(13) + wm_interm_0_pt2(a,i,p,j) * wm_interm_39_pt2(a,q,j,i)
term(14) = term(14) + wm_interm_38_pt2(a,q,i,j) * wm_interm_3_pt2(a,j,i,p)
term(15) = term(15) + wm_interm_38_pt2(a,q,i,j) * wm_interm_4_pt2(a,j,i,p)
term(16) = term(16) + wm_interm_38_pt2(a,q,i,j) * wm_interm_9_pt2(a,j,i,p)
term(17) = term(17) + wm_interm_38_pt2(a,q,i,j) * wm_interm_9_pt2(a,j,p,i)
term(18) = term(18) + wm_interm_38_pt2(a,q,i,j) * wm_interm_3_pt2(a,j,p,i)
term(19) = term(19) + wm_interm_38_pt2(a,q,i,j) * wm_interm_4_pt2(a,j,p,i)
term(20) = term(20) + wm_interm_37_pt2(a,q,i,j) * wm_interm_3_pt2(a,j,i,p)
term(21) = term(21) + wm_interm_37_pt2(a,q,i,j) * wm_interm_4_pt2(a,j,i,p)
term(22) = term(22) + wm_interm_37_pt2(a,q,i,j) * wm_interm_9_pt2(a,j,i,p)
term(23) = term(23) + wm_interm_37_pt2(a,q,i,j) * wm_interm_9_pt2(a,j,p,i)
term(24) = term(24) + wm_interm_37_pt2(a,q,i,j) * wm_interm_3_pt2(a,j,p,i)
term(25) = term(25) + wm_interm_37_pt2(a,q,i,j) * wm_interm_4_pt2(a,j,p,i)
term(26) = term(26) + wm_interm_0_pt2(a,i,p,j) * wm_interm_38_pt2(a,q,j,i)
term(27) = term(27) + wm_interm_0_pt2(a,i,p,j) * wm_interm_37_pt2(a,q,j,i)
term(28) = term(28) + wm_interm_15_pt2(a,q,i,j) * wm_interm_17_pt2(a,p,i,j)
term(29) = term(29) + wm_interm_15_pt2(a,q,i,j) * wm_interm_17_pt2(a,i,p,j)
term(30) = term(30) + wm_interm_10_pt2(a,q,i,j) * wm_interm_17_pt2(a,i,p,j)
term(31) = term(31) + wm_interm_10_pt2(a,q,i,j) * wm_interm_17_pt2(a,p,i,j)
term(32) = term(32) + wm_interm_15_pt2(a,q,i,j) * wm_interm_20_pt2(a,i,p,j)
term(33) = term(33) + wm_interm_15_pt2(a,q,i,j) * wm_interm_18_pt2(a,p,i,j)
term(34) = term(34) + wm_interm_15_pt2(a,q,i,j) * wm_interm_19_pt2(a,i,p,j)
term(35) = term(35) + wm_interm_15_pt2(a,q,i,j) * wm_interm_18_pt2(a,i,p,j)
term(36) = term(36) + wm_interm_10_pt2(a,q,i,j) * wm_interm_19_pt2(a,i,p,j)
term(37) = term(37) + wm_interm_10_pt2(a,q,i,j) * wm_interm_18_pt2(a,i,p,j)
term(38) = term(38) + wm_interm_10_pt2(a,q,i,j) * wm_interm_20_pt2(a,i,p,j)
term(39) = term(39) + wm_interm_10_pt2(a,q,i,j) * wm_interm_18_pt2(a,p,i,j)
term(40) = term(40) + wm_interm_17_pt2(a,p,i,j) * wm_interm_8_pt2(a,q,i,j)
term(41) = term(41) + wm_interm_17_pt2(a,i,p,j) * wm_interm_8_pt2(a,q,i,j)
term(42) = term(42) + wm_interm_17_pt2(a,p,i,j) * wm_interm_6_pt2(a,q,i,j)
term(43) = term(43) + wm_interm_17_pt2(a,i,p,j) * wm_interm_6_pt2(a,q,i,j)
term(44) = term(44) + wm_interm_20_pt2(a,i,p,j) * wm_interm_8_pt2(a,q,i,j)
term(45) = term(45) + wm_interm_18_pt2(a,p,i,j) * wm_interm_8_pt2(a,q,i,j)
term(46) = term(46) + wm_interm_19_pt2(a,i,p,j) * wm_interm_8_pt2(a,q,i,j)
term(47) = term(47) + wm_interm_18_pt2(a,i,p,j) * wm_interm_8_pt2(a,q,i,j)
term(48) = term(48) + wm_interm_20_pt2(a,i,p,j) * wm_interm_6_pt2(a,q,i,j)
term(49) = term(49) + wm_interm_18_pt2(a,p,i,j) * wm_interm_6_pt2(a,q,i,j)
term(50) = term(50) + wm_interm_19_pt2(a,i,p,j) * wm_interm_6_pt2(a,q,i,j)
term(51) = term(51) + wm_interm_18_pt2(a,i,p,j) * wm_interm_6_pt2(a,q,i,j)
end do 
end do 
end do 

term(0) = term(0) * (1.9999999999999998d+0) 
term(1) = term(1) * (-3.9999999999999996d+0) 
term(3) = term(3) * (-1.9999999999999998d+0) 
term(4) = term(4) * (-3.9999999999999996d+0) 
term(5) = term(5) * (7.999999999999999d+0) 
term(7) = term(7) * (1.9999999999999998d+0) 
term(8) = term(8) * (-3.9999999999999996d+0) 
term(10) = term(10) * (-1.9999999999999998d+0) 
term(11) = term(11) * (-3.9999999999999996d+0) 
term(12) = term(12) * (7.999999999999999d+0) 
term(13) = term(13) * (-1.9999999999999998d+0) 
term(14) = term(14) * (1.9999999999999998d+0) 
term(15) = term(15) * (-3.9999999999999996d+0) 
term(17) = term(17) * (-1.9999999999999998d+0) 
term(18) = term(18) * (-3.9999999999999996d+0) 
term(19) = term(19) * (7.999999999999999d+0) 
term(20) = term(20) * (-3.9999999999999996d+0) 
term(21) = term(21) * (7.999999999999999d+0) 
term(22) = term(22) * (-1.9999999999999998d+0) 
term(23) = term(23) * (3.9999999999999996d+0) 
term(24) = term(24) * (7.999999999999999d+0) 
term(25) = term(25) * (-15.999999999999998d+0) 
term(27) = term(27) * (-1.9999999999999998d+0) 
term(28) = term(28) * (1.9999999999999998d+0) 
term(29) = term(29) * (-4.0d+0) 
term(30) = term(30) * (2.0d+0) 
term(31) = term(31) * (-3.9999999999999996d+0) 
term(32) = term(32) * (1.9999999999999998d+0) 
term(33) = term(33) * (-3.9999999999999996d+0) 
term(34) = term(34) * (-3.9999999999999996d+0) 
term(35) = term(35) * (8.0d+0) 
term(36) = term(36) * (1.9999999999999998d+0) 
term(37) = term(37) * (-4.0d+0) 
term(38) = term(38) * (-3.9999999999999996d+0) 
term(39) = term(39) * (7.999999999999999d+0) 
term(40) = term(40) * (1.9999999999999998d+0) 
term(41) = term(41) * (-4.0d+0) 
term(42) = term(42) * (-3.9999999999999996d+0) 
term(43) = term(43) * (8.0d+0) 
term(44) = term(44) * (1.9999999999999998d+0) 
term(45) = term(45) * (-3.9999999999999996d+0) 
term(46) = term(46) * (-3.9999999999999996d+0) 
term(47) = term(47) * (8.0d+0) 
term(48) = term(48) * (-3.9999999999999996d+0) 
term(49) = term(49) * (7.999999999999999d+0) 
term(50) = term(50) * (7.999999999999999d+0) 
term(51) = term(51) * (-16.0d+0) 

do i = 1, nocc 
do l = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(52) = term(52) + r2(vrdav_Rl, a,j,b,k) * t2(a,q,l,i) * wm_interm_26_pt2(b,j,k,p,l,i)
term(53) = term(53) + r2(vrdav_Rl, a,j,b,k) * t2(a,q,l,i) * wm_interm_26_pt2(b,p,j,k,l,i)
term(54) = term(54) + r2(vrdav_Rl, a,j,b,k) * t2(a,q,l,i) * wm_interm_26_pt2(b,k,j,p,l,i)
term(55) = term(55) + r2(vrdav_Rl, a,j,b,k) * t2(b,q,l,i) * wm_interm_26_pt2(a,p,j,k,l,i)
term(56) = term(56) + r2(vrdav_Rl, a,j,b,k) * t2(b,q,l,i) * wm_interm_26_pt2(a,k,j,p,l,i)
term(57) = term(57) + r2(vrdav_Rl, a,j,b,k) * t2(b,q,l,i) * wm_interm_26_pt2(a,j,k,p,l,i)
end do 
end do 
end do 
end do 
end do 
end do 

term(52) = term(52) * (-1.9999999999999998d+0) 
term(53) = term(53) * (-1.9999999999999998d+0) 
term(54) = term(54) * (3.9999999999999996d+0) 
term(56) = term(56) * (-1.9999999999999998d+0) 
term(57) = term(57) * (3.9999999999999996d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(58) = term(58) + wm_interm_59_pt2(q,i,j,k) * wm_interm_62_pt2(i,j,k,p)
term(59) = term(59) + wm_interm_59_pt2(q,i,j,k) * wm_interm_62_pt2(i,j,p,k)
term(60) = term(60) + wm_interm_62_pt2(i,j,p,k) * wm_interm_63_pt2(q,i,j,k)
term(61) = term(61) + wm_interm_62_pt2(i,j,k,p) * wm_interm_63_pt2(q,i,j,k)
term(62) = term(62) + wm_interm_62_pt2(i,j,k,p) * wm_interm_65_pt2(q,i,j,k)
term(63) = term(63) + wm_interm_62_pt2(i,j,p,k) * wm_interm_65_pt2(q,i,j,k)
term(64) = term(64) + wm_interm_62_pt2(i,j,k,p) * wm_interm_66_pt2(q,i,j,k)
term(65) = term(65) + wm_interm_62_pt2(i,j,p,k) * wm_interm_66_pt2(q,i,j,k)
term(66) = term(66) + wm_interm_62_pt2(i,j,p,k) * wm_interm_68_pt2(q,i,j,k)
term(67) = term(67) + wm_interm_62_pt2(i,j,k,p) * wm_interm_68_pt2(q,i,j,k)
term(68) = term(68) + wm_interm_62_pt2(i,j,p,k) * wm_interm_69_pt2(q,i,j,k)
term(69) = term(69) + wm_interm_62_pt2(i,j,k,p) * wm_interm_69_pt2(q,i,j,k)
end do 
end do 
end do 

term(59) = term(59) * (-2.0d+0) 
term(61) = term(61) * (-2.0d+0) 
term(62) = term(62) * (-2.0d+0) 
term(63) = term(63) * (4.0d+0) 
term(65) = term(65) * (-2.0d+0) 
term(66) = term(66) * (-2.0d+0) 
term(67) = term(67) * (4.0d+0) 
term(69) = term(69) * (-2.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(70) = term(70) + wm_interm_0_pt2(a,i,j,p) * wm_interm_36_pt2(a,q,j,i)
term(71) = term(71) + wm_interm_0_pt2(a,i,j,p) * wm_interm_39_pt2(a,q,j,i)
term(72) = term(72) + wm_interm_0_pt2(a,i,j,p) * wm_interm_38_pt2(a,q,j,i)
term(73) = term(73) + wm_interm_0_pt2(a,i,j,p) * wm_interm_37_pt2(a,q,j,i)
end do 
end do 
end do 

term(70) = term(70) * (-1.9999999999999998d+0) 
term(72) = term(72) * (-1.9999999999999998d+0) 
term(73) = term(73) * (3.9999999999999996d+0) 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(74) = term(74) + r3(vrdav_Rl, a,j,b,k,q,i) * r2(vrdav_Rr, a,i,c,j) * wm_interm_36_pt2(c,b,p,k)
term(75) = term(75) + r3(vrdav_Rl, a,j,b,k,q,i) * r2(vrdav_Rr, a,i,c,j) * wm_interm_37_pt2(c,b,p,k)
term(76) = term(76) + r3(vrdav_Rl, a,j,b,k,q,i) * r2(vrdav_Rr, a,i,c,k) * wm_interm_36_pt2(c,b,p,j)
term(77) = term(77) + r3(vrdav_Rl, a,j,b,k,q,i) * r2(vrdav_Rr, a,k,c,i) * wm_interm_36_pt2(c,b,p,j)
term(78) = term(78) + r3(vrdav_Rl, a,j,b,k,q,i) * r2(vrdav_Rr, a,i,c,k) * wm_interm_37_pt2(c,b,p,j)
term(79) = term(79) + r3(vrdav_Rl, a,j,b,k,q,i) * r2(vrdav_Rr, a,k,c,i) * wm_interm_37_pt2(c,b,p,j)
term(80) = term(80) + r3(vrdav_Rl, a,j,b,k,q,i) * r2(vrdav_Rr, a,k,c,j) * wm_interm_36_pt2(c,b,p,i)
term(81) = term(81) + r3(vrdav_Rl, a,j,b,k,q,i) * r2(vrdav_Rr, a,k,c,j) * wm_interm_37_pt2(c,b,p,i)
term(82) = term(82) + r3(vrdav_Rl, a,j,b,k,q,i) * r2(vrdav_Rr, b,i,c,j) * wm_interm_36_pt2(c,a,p,k)
term(83) = term(83) + r3(vrdav_Rl, a,j,b,k,q,i) * r2(vrdav_Rr, b,i,c,j) * wm_interm_37_pt2(c,a,p,k)
term(84) = term(84) + r3(vrdav_Rl, a,j,b,k,q,i) * r2(vrdav_Rr, b,i,c,k) * wm_interm_36_pt2(c,a,p,j)
term(85) = term(85) + r3(vrdav_Rl, a,j,b,k,q,i) * r2(vrdav_Rr, b,k,c,i) * wm_interm_36_pt2(c,a,p,j)
term(86) = term(86) + r3(vrdav_Rl, a,j,b,k,q,i) * r2(vrdav_Rr, b,i,c,k) * wm_interm_37_pt2(c,a,p,j)
term(87) = term(87) + r3(vrdav_Rl, a,j,b,k,q,i) * r2(vrdav_Rr, b,k,c,i) * wm_interm_37_pt2(c,a,p,j)
term(88) = term(88) + r3(vrdav_Rl, a,j,b,k,q,i) * r2(vrdav_Rr, b,k,c,j) * wm_interm_36_pt2(c,a,p,i)
term(89) = term(89) + r3(vrdav_Rl, a,j,b,k,q,i) * r2(vrdav_Rr, b,k,c,j) * wm_interm_37_pt2(c,a,p,i)
term(90) = term(90) + r3(vrdav_Rl, a,j,b,k,q,i) * r2(vrdav_Rr, b,i,c,j) * wm_interm_38_pt2(c,a,p,k)
term(91) = term(91) + r3(vrdav_Rl, a,j,b,k,q,i) * r2(vrdav_Rr, b,i,c,j) * wm_interm_39_pt2(c,a,p,k)
term(92) = term(92) + r3(vrdav_Rl, a,j,b,k,q,i) * r2(vrdav_Rr, b,k,c,j) * wm_interm_38_pt2(c,a,p,i)
term(93) = term(93) + r3(vrdav_Rl, a,j,b,k,q,i) * r2(vrdav_Rr, b,k,c,j) * wm_interm_39_pt2(c,a,p,i)
term(94) = term(94) + r3(vrdav_Rl, a,j,b,k,q,i) * r2(vrdav_Rr, a,i,c,j) * wm_interm_39_pt2(c,b,p,k)
term(95) = term(95) + r3(vrdav_Rl, a,j,b,k,q,i) * r2(vrdav_Rr, a,i,c,j) * wm_interm_38_pt2(c,b,p,k)
term(96) = term(96) + r3(vrdav_Rl, a,j,b,k,q,i) * r2(vrdav_Rr, a,k,c,j) * wm_interm_38_pt2(c,b,p,i)
term(97) = term(97) + r3(vrdav_Rl, a,j,b,k,q,i) * r2(vrdav_Rr, a,k,c,j) * wm_interm_39_pt2(c,b,p,i)
term(98) = term(98) + r3(vrdav_Rl, a,j,b,k,q,i) * r2(vrdav_Rr, a,i,c,k) * wm_interm_38_pt2(c,b,p,j)
term(99) = term(99) + r3(vrdav_Rl, a,j,b,k,q,i) * r2(vrdav_Rr, a,k,c,i) * wm_interm_38_pt2(c,b,p,j)
term(100) = term(100) + r3(vrdav_Rl, a,j,b,k,q,i) * r2(vrdav_Rr, a,k,c,i) * wm_interm_39_pt2(c,b,p,j)
term(101) = term(101) + r3(vrdav_Rl, a,j,b,k,q,i) * r2(vrdav_Rr, a,i,c,k) * wm_interm_39_pt2(c,b,p,j)
term(102) = term(102) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,k,j) * wm_interm_52_pt2(a,c,i,p)
term(103) = term(103) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,k,j) * wm_interm_53_pt2(a,c,i,p)
term(104) = term(104) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,k,j) * wm_interm_51_pt2(a,c,i,p)
term(105) = term(105) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,k,j) * wm_interm_50_pt2(a,c,i,p)
term(106) = term(106) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,i,j) * wm_interm_50_pt2(b,c,k,p)
term(107) = term(107) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,i,j) * wm_interm_51_pt2(b,c,k,p)
term(108) = term(108) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,i,j) * wm_interm_52_pt2(b,c,k,p)
term(109) = term(109) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,i,j) * wm_interm_53_pt2(b,c,k,p)
term(110) = term(110) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,i,j) * wm_interm_52_pt2(a,c,k,p)
term(111) = term(111) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,i,j) * wm_interm_53_pt2(a,c,k,p)
term(112) = term(112) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,i,j) * wm_interm_51_pt2(a,c,k,p)
term(113) = term(113) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,i,j) * wm_interm_50_pt2(a,c,k,p)
term(114) = term(114) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,k,j) * wm_interm_52_pt2(b,c,i,p)
term(115) = term(115) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,k,j) * wm_interm_53_pt2(b,c,i,p)
term(116) = term(116) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,k,j) * wm_interm_51_pt2(b,c,i,p)
term(117) = term(117) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,k,j) * wm_interm_50_pt2(b,c,i,p)
term(118) = term(118) + r3(vrdav_Rl, a,j,b,k,q,i) * r2(vrdav_Rr, b,i,c,k) * wm_interm_39_pt2(c,a,p,j)
term(119) = term(119) + r3(vrdav_Rl, a,j,b,k,q,i) * r2(vrdav_Rr, b,k,c,i) * wm_interm_39_pt2(c,a,p,j)
term(120) = term(120) + r3(vrdav_Rl, a,j,b,k,q,i) * r2(vrdav_Rr, b,i,c,k) * wm_interm_38_pt2(c,a,p,j)
term(121) = term(121) + r3(vrdav_Rl, a,j,b,k,q,i) * r2(vrdav_Rr, b,k,c,i) * wm_interm_38_pt2(c,a,p,j)
end do 
end do 
end do 
end do 
end do 
end do 

term(74) = term(74) * (-2.0d+0) 
term(75) = term(75) * (4.0d+0) 
term(77) = term(77) * (-2.0d+0) 
term(78) = term(78) * (-2.0d+0) 
term(79) = term(79) * (4.0d+0) 
term(81) = term(81) * (-2.0d+0) 
term(83) = term(83) * (-2.0d+0) 
term(84) = term(84) * (-2.0d+0) 
term(85) = term(85) * (4.0d+0) 
term(86) = term(86) * (4.0d+0) 
term(87) = term(87) * (-8.0d+0) 
term(88) = term(88) * (-2.0d+0) 
term(89) = term(89) * (4.0d+0) 
term(91) = term(91) * (-2.0d+0) 
term(92) = term(92) * (-2.0d+0) 
term(93) = term(93) * (4.0d+0) 
term(95) = term(95) * (-2.0d+0) 
term(97) = term(97) * (-2.0d+0) 
term(99) = term(99) * (-2.0d+0) 
term(101) = term(101) * (-2.0d+0) 
term(102) = term(102) * (-2.0d+0) 
term(103) = term(103) * (4.0d+0) 
term(104) = term(104) * (-2.0d+0) 
term(105) = term(105) * (4.0d+0) 
term(107) = term(107) * (-2.0d+0) 
term(108) = term(108) * (-2.0d+0) 
term(109) = term(109) * (4.0d+0) 
term(111) = term(111) * (-2.0d+0) 
term(113) = term(113) * (-2.0d+0) 
term(115) = term(115) * (-2.0d+0) 
term(117) = term(117) * (-2.0d+0) 
term(119) = term(119) * (-2.0d+0) 
term(120) = term(120) * (-2.0d+0) 
term(121) = term(121) * (4.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(122) = term(122) + r3(vrdav_Rl, a,j,b,k,q,i) * r2(vrdav_Rr, a,j,c,i) * wm_interm_36_pt2(c,b,p,k)
term(123) = term(123) + r3(vrdav_Rl, a,j,b,k,q,i) * r2(vrdav_Rr, a,j,c,i) * wm_interm_37_pt2(c,b,p,k)
term(124) = term(124) + r3(vrdav_Rl, a,j,b,k,q,i) * r2(vrdav_Rr, a,j,c,k) * wm_interm_36_pt2(c,b,p,i)
term(125) = term(125) + r3(vrdav_Rl, a,j,b,k,q,i) * r2(vrdav_Rr, a,j,c,k) * wm_interm_37_pt2(c,b,p,i)
term(126) = term(126) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,i,k) * wm_interm_52_pt2(b,c,j,p)
term(127) = term(127) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,i,k) * wm_interm_53_pt2(b,c,j,p)
term(128) = term(128) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,i,k) * wm_interm_51_pt2(b,c,j,p)
term(129) = term(129) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,i,k) * wm_interm_50_pt2(b,c,j,p)
term(130) = term(130) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,k,i) * wm_interm_50_pt2(b,c,j,p)
term(131) = term(131) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,k,i) * wm_interm_51_pt2(b,c,j,p)
term(132) = term(132) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,k,i) * wm_interm_52_pt2(b,c,j,p)
term(133) = term(133) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,k,i) * wm_interm_53_pt2(b,c,j,p)
term(134) = term(134) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,j,k) * wm_interm_52_pt2(b,c,i,p)
term(135) = term(135) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,j,k) * wm_interm_53_pt2(b,c,i,p)
term(136) = term(136) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,j,k) * wm_interm_51_pt2(b,c,i,p)
term(137) = term(137) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,j,k) * wm_interm_50_pt2(b,c,i,p)
term(138) = term(138) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,j,i) * wm_interm_50_pt2(b,c,k,p)
term(139) = term(139) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,j,i) * wm_interm_51_pt2(b,c,k,p)
term(140) = term(140) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,j,i) * wm_interm_52_pt2(b,c,k,p)
term(141) = term(141) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(a,c,j,i) * wm_interm_53_pt2(b,c,k,p)
term(142) = term(142) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,i,k) * wm_interm_50_pt2(a,c,j,p)
term(143) = term(143) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,i,k) * wm_interm_51_pt2(a,c,j,p)
term(144) = term(144) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,i,k) * wm_interm_52_pt2(a,c,j,p)
term(145) = term(145) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,i,k) * wm_interm_53_pt2(a,c,j,p)
term(146) = term(146) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,j,k) * wm_interm_52_pt2(a,c,i,p)
term(147) = term(147) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,j,k) * wm_interm_53_pt2(a,c,i,p)
term(148) = term(148) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,j,k) * wm_interm_51_pt2(a,c,i,p)
term(149) = term(149) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,j,k) * wm_interm_50_pt2(a,c,i,p)
term(150) = term(150) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,j,i) * wm_interm_50_pt2(a,c,k,p)
term(151) = term(151) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,j,i) * wm_interm_51_pt2(a,c,k,p)
term(152) = term(152) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,j,i) * wm_interm_52_pt2(a,c,k,p)
term(153) = term(153) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,j,i) * wm_interm_53_pt2(a,c,k,p)
term(154) = term(154) + r3(vrdav_Rl, a,j,b,k,q,i) * r2(vrdav_Rr, a,j,c,i) * wm_interm_39_pt2(c,b,p,k)
term(155) = term(155) + r3(vrdav_Rl, a,j,b,k,q,i) * r2(vrdav_Rr, a,j,c,i) * wm_interm_38_pt2(c,b,p,k)
term(156) = term(156) + r3(vrdav_Rl, a,j,b,k,q,i) * r2(vrdav_Rr, a,j,c,k) * wm_interm_38_pt2(c,b,p,i)
term(157) = term(157) + r3(vrdav_Rl, a,j,b,k,q,i) * r2(vrdav_Rr, a,j,c,k) * wm_interm_39_pt2(c,b,p,i)
term(158) = term(158) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,k,i) * wm_interm_50_pt2(a,c,j,p)
term(159) = term(159) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,k,i) * wm_interm_51_pt2(a,c,j,p)
term(160) = term(160) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,k,i) * wm_interm_52_pt2(a,c,j,p)
term(161) = term(161) + r3(vrdav_Rl, a,j,b,k,q,i) * t2(b,c,k,i) * wm_interm_53_pt2(a,c,j,p)
end do 
end do 
end do 
end do 
end do 
end do 

term(122) = term(122) * (4.0d+0) 
term(123) = term(123) * (-8.0d+0) 
term(124) = term(124) * (-2.0d+0) 
term(125) = term(125) * (4.0d+0) 
term(127) = term(127) * (-2.0d+0) 
term(129) = term(129) * (-2.0d+0) 
term(131) = term(131) * (-2.0d+0) 
term(132) = term(132) * (-2.0d+0) 
term(133) = term(133) * (4.0d+0) 
term(134) = term(134) * (-2.0d+0) 
term(135) = term(135) * (4.0d+0) 
term(136) = term(136) * (-2.0d+0) 
term(137) = term(137) * (4.0d+0) 
term(138) = term(138) * (-2.0d+0) 
term(139) = term(139) * (4.0d+0) 
term(140) = term(140) * (4.0d+0) 
term(141) = term(141) * (-8.0d+0) 
term(143) = term(143) * (-2.0d+0) 
term(144) = term(144) * (-2.0d+0) 
term(145) = term(145) * (4.0d+0) 
term(147) = term(147) * (-2.0d+0) 
term(149) = term(149) * (-2.0d+0) 
term(151) = term(151) * (-2.0d+0) 
term(152) = term(152) * (-2.0d+0) 
term(153) = term(153) * (4.0d+0) 
term(154) = term(154) * (-2.0d+0) 
term(155) = term(155) * (4.0d+0) 
term(156) = term(156) * (-2.0d+0) 
term(157) = term(157) * (4.0d+0) 
term(158) = term(158) * (-2.0d+0) 
term(159) = term(159) * (4.0d+0) 
term(160) = term(160) * (4.0d+0) 
term(161) = term(161) * (-8.0d+0) 

do i = 1, nocc 
term(162) = term(162) + wm_interm_1_pt2(p,i) * wm_interm_42_pt2(q,i)
term(163) = term(163) + wm_interm_1_pt2(p,i) * wm_interm_43_pt2(q,i)
term(164) = term(164) + wm_interm_2_pt2(p,i) * wm_interm_42_pt2(q,i)
term(165) = term(165) + wm_interm_2_pt2(p,i) * wm_interm_43_pt2(q,i)
term(166) = term(166) + wm_interm_1_pt2(p,i) * wm_interm_48_pt2(q,i)
term(167) = term(167) + wm_interm_2_pt2(p,i) * wm_interm_48_pt2(q,i)
term(168) = term(168) + wm_interm_1_pt2(p,i) * wm_interm_49_pt2(q,i)
term(169) = term(169) + wm_interm_2_pt2(p,i) * wm_interm_49_pt2(q,i)
term(170) = term(170) + wm_interm_60_pt2(i,p) * wm_interm_71_pt2(q,i)
term(171) = term(171) + wm_interm_60_pt2(i,p) * wm_interm_72_pt2(q,i)
term(172) = term(172) + wm_interm_61_pt2(i,p) * wm_interm_71_pt2(q,i)
term(173) = term(173) + wm_interm_61_pt2(i,p) * wm_interm_72_pt2(q,i)
term(174) = term(174) + wm_interm_60_pt2(i,p) * wm_interm_73_pt2(q,i)
term(175) = term(175) + wm_interm_60_pt2(i,p) * wm_interm_74_pt2(q,i)
term(176) = term(176) + wm_interm_61_pt2(i,p) * wm_interm_73_pt2(q,i)
term(177) = term(177) + wm_interm_61_pt2(i,p) * wm_interm_74_pt2(q,i)
end do 

term(162) = term(162) * (1.9999999999999998d+0) 
term(163) = term(163) * (-3.9999999999999996d+0) 
term(164) = term(164) * (-3.9999999999999996d+0) 
term(165) = term(165) * (7.999999999999999d+0) 
term(166) = term(166) * (-2.0d+0) 
term(167) = term(167) * (4.0d+0) 
term(168) = term(168) * (4.0d+0) 
term(169) = term(169) * (-8.0d+0) 
term(170) = term(170) * (-3.9999999999999996d+0) 
term(171) = term(171) * (4.0d+0) 
term(172) = term(172) * (7.999999999999999d+0) 
term(173) = term(173) * (-8.0d+0) 
term(174) = term(174) * (1.9999999999999998d+0) 
term(175) = term(175) * (-2.0d+0) 
term(176) = term(176) * (-3.9999999999999996d+0) 
term(177) = term(177) * (4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do l = 1, nocc 
do a = nocc + 1, nactive 
term(178) = term(178) + s2(a,b,j,k) * t2(b,q,l,i) * wm_interm_13_pt2(a,l,i,p,j,k)
term(179) = term(179) + s2(a,b,j,k) * t2(b,q,l,i) * wm_interm_13_pt2(a,l,i,j,p,k)
term(180) = term(180) + s2(a,b,j,k) * t2(b,q,l,i) * wm_interm_13_pt2(a,l,i,j,k,p)
term(181) = term(181) + s2(a,b,j,k) * t2(b,q,l,i) * wm_interm_13_pt2(a,i,l,j,k,p)
term(182) = term(182) + s2(a,b,j,k) * t2(b,q,l,i) * wm_interm_13_pt2(a,i,l,j,p,k)
term(183) = term(183) + s2(a,b,j,k) * t2(b,q,l,i) * wm_interm_13_pt2(a,i,l,p,j,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(179) = term(179) * (-1.9999999999999998d+0) 
term(181) = term(181) * (-1.9999999999999998d+0) 
term(183) = term(183) * (-1.9999999999999998d+0) 

do a = nocc + 1, nactive 
term(184) = term(184) + wm_interm_12_pt2(a,q) * wm_interm_28_pt2(a,p)
term(185) = term(185) + wm_interm_12_pt2(a,q) * wm_interm_29_pt2(a,p)
term(186) = term(186) + wm_interm_11_pt2(a,q) * wm_interm_28_pt2(a,p)
term(187) = term(187) + wm_interm_11_pt2(a,q) * wm_interm_29_pt2(a,p)
term(188) = term(188) + wm_interm_12_pt2(a,q) * wm_interm_30_pt2(a,p)
term(189) = term(189) + wm_interm_11_pt2(a,q) * wm_interm_30_pt2(a,p)
term(190) = term(190) + wm_interm_12_pt2(a,q) * wm_interm_31_pt2(a,p)
term(191) = term(191) + wm_interm_11_pt2(a,q) * wm_interm_31_pt2(a,p)
term(192) = term(192) + wm_interm_32_pt2(a,p) * wm_interm_5_pt2(a,q)
term(193) = term(193) + wm_interm_33_pt2(a,p) * wm_interm_5_pt2(a,q)
term(194) = term(194) + wm_interm_34_pt2(a,p) * wm_interm_5_pt2(a,q)
term(195) = term(195) + wm_interm_35_pt2(a,p) * wm_interm_5_pt2(a,q)
term(196) = term(196) + wm_interm_32_pt2(a,p) * wm_interm_7_pt2(a,q)
term(197) = term(197) + wm_interm_33_pt2(a,p) * wm_interm_7_pt2(a,q)
term(198) = term(198) + wm_interm_34_pt2(a,p) * wm_interm_7_pt2(a,q)
term(199) = term(199) + wm_interm_35_pt2(a,p) * wm_interm_7_pt2(a,q)
end do 

term(184) = term(184) * (4.0d+0) 
term(185) = term(185) * (-3.9999999999999996d+0) 
term(186) = term(186) * (-8.0d+0) 
term(187) = term(187) * (7.999999999999999d+0) 
term(188) = term(188) * (-2.0d+0) 
term(189) = term(189) * (4.0d+0) 
term(190) = term(190) * (2.0d+0) 
term(191) = term(191) * (-4.0d+0) 
term(192) = term(192) * (1.9999999999999998d+0) 
term(193) = term(193) * (-3.9999999999999996d+0) 
term(194) = term(194) * (-1.9999999999999998d+0) 
term(195) = term(195) * (3.9999999999999996d+0) 
term(196) = term(196) * (-3.9999999999999996d+0) 
term(197) = term(197) * (7.999999999999999d+0) 
term(198) = term(198) * (3.9999999999999996d+0) 
term(199) = term(199) * (-7.999999999999999d+0) 

do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do l = 1, nocc 
do a = nocc + 1, nactive 
term(200) = term(200) + s2(a,b,j,k) * t2(b,q,l,i) * wm_interm_13_pt2(a,l,i,k,j,p)
term(201) = term(201) + s2(a,b,j,k) * t2(b,q,l,i) * wm_interm_13_pt2(a,l,i,p,k,j)
term(202) = term(202) + s2(a,b,j,k) * t2(b,q,l,i) * wm_interm_13_pt2(a,l,i,k,p,j)
term(203) = term(203) + s2(a,b,j,k) * t2(b,q,l,i) * wm_interm_13_pt2(a,i,l,k,j,p)
term(204) = term(204) + s2(a,b,j,k) * t2(b,q,l,i) * wm_interm_13_pt2(a,i,l,k,p,j)
term(205) = term(205) + s2(a,b,j,k) * t2(b,q,l,i) * wm_interm_13_pt2(a,i,l,p,k,j)
end do 
end do 
end do 
end do 
end do 
end do 

term(200) = term(200) * (-1.9999999999999998d+0) 
term(201) = term(201) * (-1.9999999999999998d+0) 
term(202) = term(202) * (3.9999999999999996d+0) 
term(204) = term(204) * (-1.9999999999999998d+0) 
term(205) = term(205) * (3.9999999999999996d+0) 

do i = 1, nocc 
do k = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do j = 1, nocc 
term(206) = term(206) + r3(vrdav_Rl, a,j,b,k,q,i) * r2(vrdav_Rr, b,j,c,i) * wm_interm_36_pt2(c,a,p,k)
term(207) = term(207) + r3(vrdav_Rl, a,j,b,k,q,i) * r2(vrdav_Rr, b,j,c,i) * wm_interm_37_pt2(c,a,p,k)
term(208) = term(208) + r3(vrdav_Rl, a,j,b,k,q,i) * r2(vrdav_Rr, b,j,c,k) * wm_interm_36_pt2(c,a,p,i)
term(209) = term(209) + r3(vrdav_Rl, a,j,b,k,q,i) * r2(vrdav_Rr, b,j,c,k) * wm_interm_37_pt2(c,a,p,i)
term(210) = term(210) + r3(vrdav_Rl, a,j,b,k,q,i) * r2(vrdav_Rr, b,j,c,i) * wm_interm_38_pt2(c,a,p,k)
term(211) = term(211) + r3(vrdav_Rl, a,j,b,k,q,i) * r2(vrdav_Rr, b,j,c,i) * wm_interm_39_pt2(c,a,p,k)
term(212) = term(212) + r3(vrdav_Rl, a,j,b,k,q,i) * r2(vrdav_Rr, b,j,c,k) * wm_interm_38_pt2(c,a,p,i)
term(213) = term(213) + r3(vrdav_Rl, a,j,b,k,q,i) * r2(vrdav_Rr, b,j,c,k) * wm_interm_39_pt2(c,a,p,i)
end do 
end do 
end do 
end do 
end do 
end do 

term(206) = term(206) * (-2.0d+0) 
term(207) = term(207) * (4.0d+0) 
term(209) = term(209) * (-2.0d+0) 
term(210) = term(210) * (-2.0d+0) 
term(213) = term(213) * (-2.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(214) = term(214) + wm_interm_14_pt2(i,p,j,k) * wm_interm_44_pt2(q,j,k,i)
term(215) = term(215) + wm_interm_14_pt2(i,p,j,k) * wm_interm_45_pt2(q,j,k,i)
term(216) = term(216) + wm_interm_14_pt2(p,i,j,k) * wm_interm_44_pt2(q,j,k,i)
term(217) = term(217) + wm_interm_14_pt2(p,i,j,k) * wm_interm_45_pt2(q,j,k,i)
term(218) = term(218) + wm_interm_14_pt2(i,p,j,k) * wm_interm_41_pt2(q,j,k,i)
term(219) = term(219) + wm_interm_14_pt2(i,p,j,k) * wm_interm_40_pt2(q,j,k,i)
term(220) = term(220) + wm_interm_14_pt2(p,i,j,k) * wm_interm_40_pt2(q,j,k,i)
term(221) = term(221) + wm_interm_14_pt2(p,i,j,k) * wm_interm_41_pt2(q,j,k,i)
term(222) = term(222) + wm_interm_14_pt2(p,i,j,k) * wm_interm_46_pt2(q,j,k,i)
term(223) = term(223) + wm_interm_14_pt2(i,p,j,k) * wm_interm_46_pt2(q,j,k,i)
term(224) = term(224) + wm_interm_14_pt2(p,i,j,k) * wm_interm_47_pt2(q,j,k,i)
term(225) = term(225) + wm_interm_14_pt2(i,p,j,k) * wm_interm_47_pt2(q,j,k,i)
end do 
end do 
end do 

term(215) = term(215) * (-2.0d+0) 
term(216) = term(216) * (-2.0d+0) 
term(217) = term(217) * (4.0d+0) 
term(219) = term(219) * (-2.0d+0) 
term(221) = term(221) * (-2.0d+0) 
term(223) = term(223) * (-2.0d+0) 
term(224) = term(224) * (-2.0d+0) 
term(225) = term(225) * (4.0d+0) 

do l = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(226) = term(226) + r2(vrdav_Rl, a,j,b,k) * t2(a,q,l,i) * wm_interm_26_pt2(b,j,k,p,i,l)
term(227) = term(227) + r2(vrdav_Rl, a,j,b,k) * t2(a,q,l,i) * wm_interm_26_pt2(b,k,j,p,i,l)
term(228) = term(228) + r2(vrdav_Rl, a,j,b,k) * t2(b,q,l,i) * wm_interm_26_pt2(a,k,j,p,i,l)
term(229) = term(229) + r2(vrdav_Rl, a,j,b,k) * t2(b,q,l,i) * wm_interm_26_pt2(a,j,k,p,i,l)
term(230) = term(230) + r2(vrdav_Rl, a,j,b,k) * t2(a,q,l,i) * wm_interm_26_pt2(b,p,j,k,i,l)
term(231) = term(231) + r2(vrdav_Rl, a,j,b,k) * t2(b,q,l,i) * wm_interm_26_pt2(a,p,j,k,i,l)
end do 
end do 
end do 
end do 
end do 
end do 

term(227) = term(227) * (-1.9999999999999998d+0) 
term(229) = term(229) * (-1.9999999999999998d+0) 
term(231) = term(231) * (-1.9999999999999998d+0) 


    calc_D_ov_wm_cc3_pt2 = zero
    do s = 0, 231
    calc_D_ov_wm_cc3_pt2 = calc_D_ov_wm_cc3_pt2 + term(s)
    end do

    end function calc_D_ov_wm_cc3_pt2
    
    function calc_D_vo_wm_cc3_pt2(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vo_wm_cc3_pt2
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , i, k, j, c, b, a, l 
    real(F64), dimension(0:757) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(0) = term(0) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,k,j) * wm_interm_6_pt2(b,c,i,k)
term(1) = term(1) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,k,j) * wm_interm_6_pt2(a,c,i,k)
term(2) = term(2) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,j,k) * wm_interm_6_pt2(b,c,i,k)
term(3) = term(3) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,j,k) * wm_interm_6_pt2(a,c,i,k)
term(4) = term(4) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,k,i) * wm_interm_6_pt2(a,c,j,k)
term(5) = term(5) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,k,i) * wm_interm_6_pt2(b,c,j,k)
term(6) = term(6) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,i,k) * wm_interm_6_pt2(a,c,j,k)
term(7) = term(7) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,i,k) * wm_interm_6_pt2(b,c,j,k)
term(8) = term(8) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,k,j) * wm_interm_8_pt2(b,c,i,k)
term(9) = term(9) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,k,j) * wm_interm_8_pt2(a,c,i,k)
term(10) = term(10) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,j,k) * wm_interm_8_pt2(b,c,i,k)
term(11) = term(11) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,j,k) * wm_interm_8_pt2(a,c,i,k)
term(12) = term(12) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,k,i) * wm_interm_8_pt2(a,c,j,k)
term(13) = term(13) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,k,i) * wm_interm_8_pt2(b,c,j,k)
term(14) = term(14) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,i,k) * wm_interm_8_pt2(a,c,j,k)
term(15) = term(15) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,i,k) * wm_interm_8_pt2(b,c,j,k)
term(16) = term(16) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,j,k) * wm_interm_10_pt2(b,c,i,k)
term(17) = term(17) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,j,k) * wm_interm_10_pt2(a,c,i,k)
term(18) = term(18) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,i,k) * wm_interm_10_pt2(a,c,j,k)
term(19) = term(19) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,i,k) * wm_interm_10_pt2(b,c,j,k)
term(20) = term(20) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,k,i) * wm_interm_10_pt2(b,c,j,k)
term(21) = term(21) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,k,i) * wm_interm_10_pt2(a,c,j,k)
term(22) = term(22) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,k,j) * wm_interm_10_pt2(a,c,i,k)
term(23) = term(23) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,k,j) * wm_interm_10_pt2(b,c,i,k)
term(24) = term(24) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,k,j) * wm_interm_15_pt2(b,c,i,k)
term(25) = term(25) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,k,j) * wm_interm_15_pt2(a,c,i,k)
term(26) = term(26) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,j,k) * wm_interm_15_pt2(b,c,i,k)
term(27) = term(27) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,j,k) * wm_interm_15_pt2(a,c,i,k)
term(28) = term(28) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,k,i) * wm_interm_15_pt2(a,c,j,k)
term(29) = term(29) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,k,i) * wm_interm_15_pt2(b,c,j,k)
term(30) = term(30) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,i,k) * wm_interm_15_pt2(a,c,j,k)
term(31) = term(31) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,i,k) * wm_interm_15_pt2(b,c,j,k)
term(32) = term(32) + r2(vrdav_Rl, a,j,b,k) * r3(vrdav_Rr, b,k,c,j,p,i) * wm_interm_36_pt2(c,a,i,q)
term(33) = term(33) + r2(vrdav_Rl, a,j,b,k) * r3(vrdav_Rr, a,k,c,j,p,i) * wm_interm_36_pt2(c,b,i,q)
term(34) = term(34) + r2(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, b,j,c,k,p,i) * wm_interm_36_pt2(c,a,k,q)
term(35) = term(35) + r2(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,j,c,k,p,i) * wm_interm_36_pt2(c,b,k,q)
term(36) = term(36) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,i,j) * wm_interm_15_pt2(a,c,k,q)
term(37) = term(37) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,i,j) * wm_interm_10_pt2(a,c,k,q)
term(38) = term(38) + r2(vrdav_Rl, a,j,b,k) * r3(vrdav_Rr, b,k,c,j,p,i) * wm_interm_37_pt2(c,a,i,q)
term(39) = term(39) + r2(vrdav_Rl, a,j,b,k) * r3(vrdav_Rr, a,k,c,j,p,i) * wm_interm_37_pt2(c,b,i,q)
term(40) = term(40) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,k,j) * wm_interm_15_pt2(a,c,i,q)
term(41) = term(41) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,k,j) * wm_interm_10_pt2(a,c,i,q)
term(42) = term(42) + r2(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, b,j,c,k,p,i) * wm_interm_37_pt2(c,a,k,q)
term(43) = term(43) + r2(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,j,c,k,p,i) * wm_interm_37_pt2(c,b,k,q)
term(44) = term(44) + r2(vrdav_Rl, a,j,b,k) * r3(vrdav_Rr, b,k,c,j,p,i) * wm_interm_38_pt2(c,a,i,q)
term(45) = term(45) + r2(vrdav_Rl, a,j,b,k) * r3(vrdav_Rr, a,k,c,j,p,i) * wm_interm_38_pt2(c,b,i,q)
term(46) = term(46) + r2(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, b,j,c,k,p,i) * wm_interm_38_pt2(c,a,k,q)
term(47) = term(47) + r2(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,j,c,k,p,i) * wm_interm_38_pt2(c,b,k,q)
term(48) = term(48) + r2(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, b,j,c,k,p,i) * wm_interm_39_pt2(c,a,k,q)
term(49) = term(49) + r2(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,j,c,k,p,i) * wm_interm_39_pt2(c,b,k,q)
term(50) = term(50) + r2(vrdav_Rl, a,j,b,k) * r3(vrdav_Rr, b,k,c,j,p,i) * wm_interm_39_pt2(c,a,i,q)
term(51) = term(51) + r2(vrdav_Rl, a,j,b,k) * r3(vrdav_Rr, a,k,c,j,p,i) * wm_interm_39_pt2(c,b,i,q)
term(52) = term(52) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,i,j) * wm_interm_8_pt2(a,c,k,q)
term(53) = term(53) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,i,j) * wm_interm_6_pt2(a,c,k,q)
term(54) = term(54) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,k,j) * wm_interm_8_pt2(a,c,i,q)
term(55) = term(55) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,k,j) * wm_interm_6_pt2(a,c,i,q)
term(56) = term(56) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(b,c,j,k) * wm_interm_50_pt2(a,c,i,k)
term(57) = term(57) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(b,c,j,k) * wm_interm_51_pt2(a,c,i,k)
term(58) = term(58) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(b,c,j,k) * wm_interm_52_pt2(a,c,i,k)
term(59) = term(59) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(b,c,j,k) * wm_interm_53_pt2(a,c,i,k)
term(60) = term(60) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(a,c,j,k) * wm_interm_50_pt2(b,c,i,k)
term(61) = term(61) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(a,c,j,k) * wm_interm_51_pt2(b,c,i,k)
term(62) = term(62) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(a,c,j,k) * wm_interm_52_pt2(b,c,i,k)
term(63) = term(63) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(a,c,j,k) * wm_interm_53_pt2(b,c,i,k)
term(64) = term(64) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(b,c,i,k) * wm_interm_50_pt2(a,c,j,k)
term(65) = term(65) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(b,c,i,k) * wm_interm_51_pt2(a,c,j,k)
term(66) = term(66) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(b,c,i,k) * wm_interm_52_pt2(a,c,j,k)
term(67) = term(67) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(b,c,i,k) * wm_interm_53_pt2(a,c,j,k)
term(68) = term(68) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(a,c,i,k) * wm_interm_50_pt2(b,c,j,k)
term(69) = term(69) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(a,c,i,k) * wm_interm_51_pt2(b,c,j,k)
term(70) = term(70) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(a,c,i,k) * wm_interm_52_pt2(b,c,j,k)
term(71) = term(71) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(a,c,i,k) * wm_interm_53_pt2(b,c,j,k)
term(72) = term(72) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(a,c,k,j) * wm_interm_52_pt2(b,c,i,k)
term(73) = term(73) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(a,c,k,j) * wm_interm_53_pt2(b,c,i,k)
term(74) = term(74) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(a,c,k,j) * wm_interm_51_pt2(b,c,i,k)
term(75) = term(75) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(a,c,k,j) * wm_interm_50_pt2(b,c,i,k)
term(76) = term(76) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(b,c,k,i) * wm_interm_52_pt2(a,c,j,k)
term(77) = term(77) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(b,c,k,i) * wm_interm_53_pt2(a,c,j,k)
term(78) = term(78) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(b,c,k,i) * wm_interm_51_pt2(a,c,j,k)
term(79) = term(79) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(b,c,k,i) * wm_interm_50_pt2(a,c,j,k)
term(80) = term(80) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(a,c,k,i) * wm_interm_52_pt2(b,c,j,k)
term(81) = term(81) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(a,c,k,i) * wm_interm_53_pt2(b,c,j,k)
term(82) = term(82) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(a,c,k,i) * wm_interm_50_pt2(b,c,j,k)
term(83) = term(83) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(a,c,k,i) * wm_interm_51_pt2(b,c,j,k)
term(84) = term(84) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(b,c,k,j) * wm_interm_50_pt2(a,c,i,k)
term(85) = term(85) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(b,c,k,j) * wm_interm_51_pt2(a,c,i,k)
term(86) = term(86) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(b,c,k,j) * wm_interm_52_pt2(a,c,i,k)
term(87) = term(87) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(b,c,k,j) * wm_interm_53_pt2(a,c,i,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(0) = term(0) * (-3.9999999999999996d+0) 
term(1) = term(1) * (7.999999999999999d+0) 
term(2) = term(2) * (7.999999999999999d+0) 
term(3) = term(3) * (-15.999999999999998d+0) 
term(4) = term(4) * (-3.9999999999999996d+0) 
term(5) = term(5) * (8.0d+0) 
term(6) = term(6) * (7.999999999999999d+0) 
term(7) = term(7) * (-16.0d+0) 
term(8) = term(8) * (1.9999999999999998d+0) 
term(9) = term(9) * (-3.9999999999999996d+0) 
term(10) = term(10) * (-3.9999999999999996d+0) 
term(11) = term(11) * (7.999999999999999d+0) 
term(12) = term(12) * (1.9999999999999998d+0) 
term(13) = term(13) * (-4.0d+0) 
term(14) = term(14) * (-3.9999999999999996d+0) 
term(15) = term(15) * (8.0d+0) 
term(16) = term(16) * (1.9999999999999998d+0) 
term(17) = term(17) * (-3.9999999999999996d+0) 
term(18) = term(18) * (1.9999999999999998d+0) 
term(19) = term(19) * (-4.0d+0) 
term(20) = term(20) * (2.0d+0) 
term(21) = term(21) * (-3.9999999999999996d+0) 
term(22) = term(22) * (1.9999999999999998d+0) 
term(23) = term(23) * (-3.9999999999999996d+0) 
term(24) = term(24) * (1.9999999999999998d+0) 
term(25) = term(25) * (-3.9999999999999996d+0) 
term(26) = term(26) * (-3.9999999999999996d+0) 
term(27) = term(27) * (7.999999999999999d+0) 
term(28) = term(28) * (1.9999999999999998d+0) 
term(29) = term(29) * (-4.0d+0) 
term(30) = term(30) * (-3.9999999999999996d+0) 
term(31) = term(31) * (8.0d+0) 
term(32) = term(32) * (-1.9999999999999998d+0) 
term(34) = term(34) * (3.9999999999999996d+0) 
term(35) = term(35) * (-1.9999999999999998d+0) 
term(36) = term(36) * (1.9999999999999998d+0) 
term(37) = term(37) * (-3.9999999999999996d+0) 
term(38) = term(38) * (3.9999999999999996d+0) 
term(39) = term(39) * (-1.9999999999999998d+0) 
term(40) = term(40) * (-3.9999999999999996d+0) 
term(41) = term(41) * (7.999999999999999d+0) 
term(42) = term(42) * (-7.999999999999999d+0) 
term(43) = term(43) * (3.9999999999999996d+0) 
term(44) = term(44) * (-1.9999999999999998d+0) 
term(46) = term(46) * (3.9999999999999996d+0) 
term(47) = term(47) * (-1.9999999999999998d+0) 
term(48) = term(48) * (-1.9999999999999998d+0) 
term(50) = term(50) * (3.9999999999999996d+0) 
term(51) = term(51) * (-1.9999999999999998d+0) 
term(52) = term(52) * (1.9999999999999998d+0) 
term(53) = term(53) * (-3.9999999999999996d+0) 
term(54) = term(54) * (-3.9999999999999996d+0) 
term(55) = term(55) * (7.999999999999999d+0) 
term(56) = term(56) * (-3.9999999999999996d+0) 
term(57) = term(57) * (7.999999999999999d+0) 
term(58) = term(58) * (7.999999999999999d+0) 
term(59) = term(59) * (-15.999999999999998d+0) 
term(60) = term(60) * (1.9999999999999998d+0) 
term(61) = term(61) * (-3.9999999999999996d+0) 
term(62) = term(62) * (-3.9999999999999996d+0) 
term(63) = term(63) * (7.999999999999999d+0) 
term(64) = term(64) * (1.9999999999999998d+0) 
term(65) = term(65) * (-3.9999999999999996d+0) 
term(66) = term(66) * (-3.9999999999999996d+0) 
term(67) = term(67) * (7.999999999999999d+0) 
term(68) = term(68) * (-3.9999999999999996d+0) 
term(69) = term(69) * (7.999999999999999d+0) 
term(70) = term(70) * (7.999999999999999d+0) 
term(71) = term(71) * (-15.999999999999998d+0) 
term(72) = term(72) * (1.9999999999999998d+0) 
term(73) = term(73) * (-3.9999999999999996d+0) 
term(74) = term(74) * (1.9999999999999998d+0) 
term(75) = term(75) * (-3.9999999999999996d+0) 
term(76) = term(76) * (1.9999999999999998d+0) 
term(77) = term(77) * (-3.9999999999999996d+0) 
term(78) = term(78) * (1.9999999999999998d+0) 
term(79) = term(79) * (-3.9999999999999996d+0) 
term(80) = term(80) * (-3.9999999999999996d+0) 
term(81) = term(81) * (7.999999999999999d+0) 
term(82) = term(82) * (1.9999999999999998d+0) 
term(83) = term(83) * (-3.9999999999999996d+0) 
term(84) = term(84) * (1.9999999999999998d+0) 
term(85) = term(85) * (-3.9999999999999996d+0) 
term(86) = term(86) * (-3.9999999999999996d+0) 
term(87) = term(87) * (7.999999999999999d+0) 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
term(88) = term(88) + wm_interm_14_pt2(i,j,k,q) * wm_interm_3_pt2(p,k,j,i)
term(89) = term(89) + wm_interm_14_pt2(i,j,k,q) * wm_interm_4_pt2(p,k,j,i)
term(90) = term(90) + wm_interm_14_pt2(i,j,k,q) * wm_interm_9_pt2(p,k,j,i)
end do 
end do 
end do 

term(88) = term(88) * (1.9999999999999998d+0) 
term(89) = term(89) * (-3.9999999999999996d+0) 

do l = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(91) = term(91) + wm_interm_13_pt2(p,i,j,q,k,l) * wm_interm_14_pt2(k,l,i,j)
term(92) = term(92) + wm_interm_13_pt2(p,i,j,k,q,l) * wm_interm_14_pt2(k,l,i,j)
term(93) = term(93) + wm_interm_13_pt2(p,i,j,k,q,l) * wm_interm_14_pt2(l,k,i,j)
term(94) = term(94) + wm_interm_13_pt2(p,i,j,k,l,q) * wm_interm_14_pt2(l,k,i,j)
term(95) = term(95) + wm_interm_13_pt2(p,i,j,q,k,l) * wm_interm_14_pt2(l,k,i,j)
term(96) = term(96) + wm_interm_13_pt2(p,i,j,k,l,q) * wm_interm_14_pt2(k,l,i,j)
term(97) = term(97) + wm_interm_23_pt2(i,j,k,l) * wm_interm_24_pt2(p,i,j,q,k,l)
term(98) = term(98) + wm_interm_23_pt2(i,j,k,l) * wm_interm_26_pt2(p,i,j,q,k,l)
term(99) = term(99) + wm_interm_23_pt2(i,j,k,l) * wm_interm_27_pt2(p,i,j,q,k,l)
term(100) = term(100) + wm_interm_62_pt2(i,j,k,l) * wm_interm_64_pt2(p,i,j,q,k,l)
term(101) = term(101) + wm_interm_62_pt2(i,j,k,l) * wm_interm_64_pt2(p,q,i,j,k,l)
term(102) = term(102) + wm_interm_62_pt2(i,j,k,l) * wm_interm_64_pt2(p,i,q,j,k,l)
term(103) = term(103) + wm_interm_62_pt2(i,j,k,l) * wm_interm_67_pt2(p,i,j,q,k,l)
term(104) = term(104) + wm_interm_62_pt2(i,j,k,l) * wm_interm_67_pt2(p,i,q,j,k,l)
term(105) = term(105) + wm_interm_62_pt2(i,j,k,l) * wm_interm_67_pt2(p,q,i,j,k,l)
term(106) = term(106) + wm_interm_62_pt2(i,j,k,l) * wm_interm_70_pt2(p,i,q,j,k,l)
term(107) = term(107) + wm_interm_62_pt2(i,j,k,l) * wm_interm_70_pt2(p,q,i,j,k,l)
term(108) = term(108) + wm_interm_62_pt2(i,j,k,l) * wm_interm_70_pt2(p,i,j,q,k,l)
end do 
end do 
end do 
end do 

term(92) = term(92) * (-1.9999999999999998d+0) 
term(94) = term(94) * (-1.9999999999999998d+0) 
term(95) = term(95) * (-1.9999999999999998d+0) 
term(96) = term(96) * (3.9999999999999996d+0) 
term(97) = term(97) * (-1.9999999999999998d+0) 
term(99) = term(99) * (3.9999999999999996d+0) 
term(100) = term(100) * (-0.6666666666666666d+0) 
term(101) = term(101) * (-0.6666666666666666d+0) 
term(102) = term(102) * (1.3333333333333333d+0) 
term(103) = term(103) * (0.3333333333333333d+0) 
term(104) = term(104) * (-0.6666666666666666d+0) 
term(105) = term(105) * (1.3333333333333333d+0) 
term(106) = term(106) * (-0.6666666666666666d+0) 
term(107) = term(107) * (0.3333333333333333d+0) 
term(108) = term(108) * (1.3333333333333333d+0) 

do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
term(109) = term(109) + wm_interm_14_pt2(i,j,k,q) * wm_interm_9_pt2(p,k,i,j)
term(110) = term(110) + wm_interm_14_pt2(i,j,k,q) * wm_interm_3_pt2(p,k,i,j)
term(111) = term(111) + wm_interm_14_pt2(i,j,k,q) * wm_interm_4_pt2(p,k,i,j)
end do 
end do 
end do 

term(109) = term(109) * (-1.9999999999999998d+0) 
term(110) = term(110) * (-3.9999999999999996d+0) 
term(111) = term(111) * (7.999999999999999d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(112) = term(112) + wm_interm_0_pt2(p,i,j,k) * wm_interm_14_pt2(j,k,i,q)
term(113) = term(113) + wm_interm_0_pt2(p,i,j,k) * wm_interm_14_pt2(k,j,i,q)
term(114) = term(114) + wm_interm_19_pt2(p,i,j,k) * wm_interm_23_pt2(i,j,k,q)
term(115) = term(115) + wm_interm_20_pt2(p,i,j,k) * wm_interm_23_pt2(i,j,k,q)
term(116) = term(116) + wm_interm_20_pt2(p,i,j,k) * wm_interm_23_pt2(i,j,q,k)
term(117) = term(117) + wm_interm_19_pt2(p,i,j,k) * wm_interm_23_pt2(i,j,q,k)
term(118) = term(118) + wm_interm_21_pt2(p,i,j,k) * wm_interm_23_pt2(i,j,q,k)
term(119) = term(119) + wm_interm_17_pt2(p,i,j,k) * wm_interm_23_pt2(i,j,q,k)
term(120) = term(120) + wm_interm_17_pt2(p,i,j,k) * wm_interm_23_pt2(i,j,k,q)
term(121) = term(121) + wm_interm_21_pt2(p,i,j,k) * wm_interm_23_pt2(i,j,k,q)
term(122) = term(122) + wm_interm_22_pt2(p,i,j,k) * wm_interm_23_pt2(i,j,q,k)
term(123) = term(123) + wm_interm_18_pt2(p,i,j,k) * wm_interm_23_pt2(i,j,q,k)
term(124) = term(124) + wm_interm_18_pt2(p,i,j,k) * wm_interm_23_pt2(i,j,k,q)
term(125) = term(125) + wm_interm_22_pt2(p,i,j,k) * wm_interm_23_pt2(i,j,k,q)
end do 
end do 
end do 

term(113) = term(113) * (-1.9999999999999998d+0) 
term(115) = term(115) * (-1.9999999999999998d+0) 
term(117) = term(117) * (-1.9999999999999998d+0) 
term(119) = term(119) * (-1.9999999999999998d+0) 
term(121) = term(121) * (-1.9999999999999998d+0) 
term(122) = term(122) * (-1.9999999999999998d+0) 
term(123) = term(123) * (3.9999999999999996d+0) 
term(124) = term(124) * (-1.9999999999999998d+0) 
term(125) = term(125) * (3.9999999999999996d+0) 

do k = 1, nocc 
do j = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(126) = term(126) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,k,j) * wm_interm_6_pt2(a,c,i,k)
term(127) = term(127) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,i,k) * wm_interm_6_pt2(a,c,j,k)
term(128) = term(128) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,j,k) * wm_interm_6_pt2(a,c,i,k)
term(129) = term(129) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,k,j) * wm_interm_8_pt2(a,c,i,k)
term(130) = term(130) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,i,k) * wm_interm_8_pt2(a,c,j,k)
term(131) = term(131) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,j,k) * wm_interm_8_pt2(a,c,i,k)
term(132) = term(132) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,i,k) * wm_interm_10_pt2(a,c,j,k)
term(133) = term(133) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,j,k) * wm_interm_10_pt2(a,c,i,k)
term(134) = term(134) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,k,j) * wm_interm_10_pt2(a,c,i,k)
term(135) = term(135) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,k,j) * wm_interm_15_pt2(a,c,i,k)
term(136) = term(136) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,i,k) * wm_interm_15_pt2(a,c,j,k)
term(137) = term(137) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,j,k) * wm_interm_15_pt2(a,c,i,k)
term(138) = term(138) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(b,c,j,k) * wm_interm_50_pt2(a,c,i,k)
term(139) = term(139) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(b,c,j,k) * wm_interm_51_pt2(a,c,i,k)
term(140) = term(140) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(b,c,j,k) * wm_interm_52_pt2(a,c,i,k)
term(141) = term(141) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(b,c,j,k) * wm_interm_53_pt2(a,c,i,k)
term(142) = term(142) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(b,c,i,k) * wm_interm_50_pt2(a,c,j,k)
term(143) = term(143) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(b,c,i,k) * wm_interm_51_pt2(a,c,j,k)
term(144) = term(144) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(b,c,i,k) * wm_interm_52_pt2(a,c,j,k)
term(145) = term(145) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(b,c,i,k) * wm_interm_53_pt2(a,c,j,k)
term(146) = term(146) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(a,c,j,k) * wm_interm_50_pt2(b,c,i,k)
term(147) = term(147) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(a,c,j,k) * wm_interm_51_pt2(b,c,i,k)
term(148) = term(148) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(a,c,j,k) * wm_interm_52_pt2(b,c,i,k)
term(149) = term(149) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(a,c,j,k) * wm_interm_53_pt2(b,c,i,k)
term(150) = term(150) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(a,c,i,k) * wm_interm_50_pt2(b,c,j,k)
term(151) = term(151) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(a,c,i,k) * wm_interm_51_pt2(b,c,j,k)
term(152) = term(152) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(a,c,i,k) * wm_interm_52_pt2(b,c,j,k)
term(153) = term(153) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(a,c,i,k) * wm_interm_53_pt2(b,c,j,k)
term(154) = term(154) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(a,c,k,j) * wm_interm_52_pt2(b,c,i,k)
term(155) = term(155) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(a,c,k,j) * wm_interm_53_pt2(b,c,i,k)
term(156) = term(156) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(a,c,k,j) * wm_interm_51_pt2(b,c,i,k)
term(157) = term(157) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(a,c,k,j) * wm_interm_50_pt2(b,c,i,k)
term(158) = term(158) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(b,c,k,j) * wm_interm_50_pt2(a,c,i,k)
term(159) = term(159) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(b,c,k,j) * wm_interm_51_pt2(a,c,i,k)
term(160) = term(160) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(b,c,k,j) * wm_interm_52_pt2(a,c,i,k)
term(161) = term(161) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(b,c,k,j) * wm_interm_53_pt2(a,c,i,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(126) = term(126) * (-16.0d+0) 
term(127) = term(127) * (-15.999999999999998d+0) 
term(128) = term(128) * (32.0d+0) 
term(129) = term(129) * (8.0d+0) 
term(130) = term(130) * (7.999999999999999d+0) 
term(131) = term(131) * (-16.0d+0) 
term(132) = term(132) * (-3.9999999999999996d+0) 
term(133) = term(133) * (8.0d+0) 
term(134) = term(134) * (-4.0d+0) 
term(135) = term(135) * (8.0d+0) 
term(136) = term(136) * (7.999999999999999d+0) 
term(137) = term(137) * (-16.0d+0) 
term(138) = term(138) * (4.0d+0) 
term(139) = term(139) * (-8.0d+0) 
term(140) = term(140) * (-8.0d+0) 
term(141) = term(141) * (16.0d+0) 
term(142) = term(142) * (-2.0d+0) 
term(143) = term(143) * (4.0d+0) 
term(144) = term(144) * (4.0d+0) 
term(145) = term(145) * (-8.0d+0) 
term(146) = term(146) * (-2.0d+0) 
term(147) = term(147) * (4.0d+0) 
term(148) = term(148) * (4.0d+0) 
term(149) = term(149) * (-8.0d+0) 
term(150) = term(150) * (4.0d+0) 
term(151) = term(151) * (-8.0d+0) 
term(152) = term(152) * (-8.0d+0) 
term(153) = term(153) * (16.0d+0) 
term(154) = term(154) * (-2.0d+0) 
term(155) = term(155) * (4.0d+0) 
term(156) = term(156) * (-2.0d+0) 
term(157) = term(157) * (4.0d+0) 
term(158) = term(158) * (-2.0d+0) 
term(159) = term(159) * (4.0d+0) 
term(160) = term(160) * (4.0d+0) 
term(161) = term(161) * (-8.0d+0) 

do k = 1, nocc 
do l = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(162) = term(162) + wm_interm_23_pt2(i,j,k,l) * wm_interm_24_pt2(p,i,j,q,l,k)
term(163) = term(163) + wm_interm_23_pt2(i,j,k,l) * wm_interm_26_pt2(p,i,j,q,l,k)
term(164) = term(164) + wm_interm_23_pt2(i,j,k,l) * wm_interm_27_pt2(p,i,j,q,l,k)
term(165) = term(165) + wm_interm_62_pt2(i,j,k,l) * wm_interm_64_pt2(p,i,j,q,l,k)
term(166) = term(166) + wm_interm_62_pt2(i,j,k,l) * wm_interm_64_pt2(p,q,i,j,l,k)
term(167) = term(167) + wm_interm_62_pt2(i,j,k,l) * wm_interm_64_pt2(p,i,q,j,l,k)
term(168) = term(168) + wm_interm_62_pt2(i,j,k,l) * wm_interm_67_pt2(p,i,j,q,l,k)
term(169) = term(169) + wm_interm_62_pt2(i,j,k,l) * wm_interm_67_pt2(p,i,q,j,l,k)
term(170) = term(170) + wm_interm_62_pt2(i,j,k,l) * wm_interm_67_pt2(p,q,i,j,l,k)
term(171) = term(171) + wm_interm_62_pt2(i,j,k,l) * wm_interm_70_pt2(p,i,q,j,l,k)
term(172) = term(172) + wm_interm_62_pt2(i,j,k,l) * wm_interm_70_pt2(p,q,i,j,l,k)
term(173) = term(173) + wm_interm_62_pt2(i,j,k,l) * wm_interm_70_pt2(p,i,j,q,l,k)
end do 
end do 
end do 
end do 

term(163) = term(163) * (-1.9999999999999998d+0) 
term(164) = term(164) * (-1.9999999999999998d+0) 
term(165) = term(165) * (0.3333333333333333d+0) 
term(166) = term(166) * (0.3333333333333333d+0) 
term(167) = term(167) * (-0.6666666666666666d+0) 
term(168) = term(168) * (-0.6666666666666666d+0) 
term(169) = term(169) * (0.3333333333333333d+0) 
term(170) = term(170) * (-0.6666666666666666d+0) 
term(171) = term(171) * (0.3333333333333333d+0) 
term(172) = term(172) * (-0.6666666666666666d+0) 
term(173) = term(173) * (-0.6666666666666666d+0) 

do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(174) = term(174) + r2(vrdav_Rl, a,i,b,j) * t2(a,p,k,q) * wm_interm_17_pt2(b,i,j,k)
term(175) = term(175) + r2(vrdav_Rl, a,i,b,j) * t2(a,p,k,q) * wm_interm_17_pt2(b,j,i,k)
term(176) = term(176) + r2(vrdav_Rl, a,i,b,j) * t2(b,p,k,q) * wm_interm_17_pt2(a,j,i,k)
term(177) = term(177) + r2(vrdav_Rl, a,i,b,j) * t2(b,p,k,q) * wm_interm_17_pt2(a,i,j,k)
term(178) = term(178) + r2(vrdav_Rl, a,i,b,j) * t2(a,p,k,q) * wm_interm_19_pt2(b,i,j,k)
term(179) = term(179) + r2(vrdav_Rl, a,i,b,j) * t2(a,p,k,q) * wm_interm_18_pt2(b,i,j,k)
term(180) = term(180) + r2(vrdav_Rl, a,i,b,j) * t2(a,p,k,q) * wm_interm_20_pt2(b,i,j,k)
term(181) = term(181) + r2(vrdav_Rl, a,i,b,j) * t2(a,p,k,q) * wm_interm_18_pt2(b,j,i,k)
term(182) = term(182) + r2(vrdav_Rl, a,i,b,j) * t2(b,p,k,q) * wm_interm_20_pt2(a,i,j,k)
term(183) = term(183) + r2(vrdav_Rl, a,i,b,j) * t2(b,p,k,q) * wm_interm_18_pt2(a,j,i,k)
term(184) = term(184) + r2(vrdav_Rl, a,i,b,j) * t2(b,p,k,q) * wm_interm_19_pt2(a,i,j,k)
term(185) = term(185) + r2(vrdav_Rl, a,i,b,j) * t2(b,p,k,q) * wm_interm_18_pt2(a,i,j,k)
term(186) = term(186) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_59_pt2(a,j,i,k)
term(187) = term(187) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_59_pt2(a,i,j,k)
term(188) = term(188) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,k,q) * wm_interm_59_pt2(a,j,i,k)
term(189) = term(189) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,k,q) * wm_interm_59_pt2(a,i,j,k)
term(190) = term(190) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_63_pt2(a,i,j,k)
term(191) = term(191) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_63_pt2(a,j,i,k)
term(192) = term(192) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,k,q) * wm_interm_63_pt2(a,i,j,k)
term(193) = term(193) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,k,q) * wm_interm_63_pt2(a,j,i,k)
term(194) = term(194) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_65_pt2(a,j,i,k)
term(195) = term(195) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_65_pt2(a,i,j,k)
term(196) = term(196) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,k,q) * wm_interm_65_pt2(a,j,i,k)
term(197) = term(197) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,k,q) * wm_interm_65_pt2(a,i,j,k)
term(198) = term(198) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_66_pt2(a,j,i,k)
term(199) = term(199) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_66_pt2(a,i,j,k)
term(200) = term(200) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,k,q) * wm_interm_66_pt2(a,j,i,k)
term(201) = term(201) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,k,q) * wm_interm_66_pt2(a,i,j,k)
term(202) = term(202) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_68_pt2(a,i,j,k)
term(203) = term(203) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_68_pt2(a,j,i,k)
term(204) = term(204) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,k,q) * wm_interm_68_pt2(a,i,j,k)
term(205) = term(205) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,k,q) * wm_interm_68_pt2(a,j,i,k)
term(206) = term(206) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_69_pt2(a,i,j,k)
term(207) = term(207) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_69_pt2(a,j,i,k)
term(208) = term(208) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,k,q) * wm_interm_69_pt2(a,i,j,k)
term(209) = term(209) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,k,q) * wm_interm_69_pt2(a,j,i,k)
end do 
end do 
end do 
end do 
end do 

term(174) = term(174) * (-1.9999999999999998d+0) 
term(175) = term(175) * (3.9999999999999996d+0) 
term(176) = term(176) * (-1.9999999999999998d+0) 
term(177) = term(177) * (3.9999999999999996d+0) 
term(178) = term(178) * (-1.9999999999999998d+0) 
term(179) = term(179) * (3.9999999999999996d+0) 
term(180) = term(180) * (3.9999999999999996d+0) 
term(181) = term(181) * (-7.999999999999999d+0) 
term(182) = term(182) * (-1.9999999999999998d+0) 
term(183) = term(183) * (3.9999999999999996d+0) 
term(184) = term(184) * (3.9999999999999996d+0) 
term(185) = term(185) * (-7.999999999999999d+0) 
term(187) = term(187) * (-2.0d+0) 
term(188) = term(188) * (-2.0d+0) 
term(189) = term(189) * (4.0d+0) 
term(191) = term(191) * (-2.0d+0) 
term(192) = term(192) * (-2.0d+0) 
term(193) = term(193) * (4.0d+0) 
term(194) = term(194) * (-2.0d+0) 
term(195) = term(195) * (4.0d+0) 
term(196) = term(196) * (4.0d+0) 
term(197) = term(197) * (-8.0d+0) 
term(199) = term(199) * (-2.0d+0) 
term(200) = term(200) * (-2.0d+0) 
term(201) = term(201) * (4.0d+0) 
term(202) = term(202) * (-2.0d+0) 
term(203) = term(203) * (4.0d+0) 
term(204) = term(204) * (4.0d+0) 
term(205) = term(205) * (-8.0d+0) 
term(207) = term(207) * (-2.0d+0) 
term(208) = term(208) * (-2.0d+0) 
term(209) = term(209) * (4.0d+0) 

do a = nocc + 1, nactive 
term(210) = term(210) + wm_interm_42_pt2(a,q) * wm_interm_5_pt2(p,a)
term(211) = term(211) + wm_interm_42_pt2(a,q) * wm_interm_7_pt2(p,a)
term(212) = term(212) + wm_interm_43_pt2(a,q) * wm_interm_5_pt2(p,a)
term(213) = term(213) + wm_interm_43_pt2(a,q) * wm_interm_7_pt2(p,a)
term(214) = term(214) + wm_interm_48_pt2(a,q) * wm_interm_5_pt2(p,a)
term(215) = term(215) + wm_interm_48_pt2(a,q) * wm_interm_7_pt2(p,a)
term(216) = term(216) + wm_interm_49_pt2(a,q) * wm_interm_5_pt2(p,a)
term(217) = term(217) + wm_interm_49_pt2(a,q) * wm_interm_7_pt2(p,a)
term(218) = term(218) + wm_interm_54_pt2(a,p) * wm_interm_71_pt2(a,q)
term(219) = term(219) + wm_interm_55_pt2(a,p) * wm_interm_71_pt2(a,q)
term(220) = term(220) + wm_interm_54_pt2(a,p) * wm_interm_72_pt2(a,q)
term(221) = term(221) + wm_interm_55_pt2(a,p) * wm_interm_72_pt2(a,q)
term(222) = term(222) + wm_interm_54_pt2(a,p) * wm_interm_73_pt2(a,q)
term(223) = term(223) + wm_interm_55_pt2(a,p) * wm_interm_73_pt2(a,q)
term(224) = term(224) + wm_interm_54_pt2(a,p) * wm_interm_74_pt2(a,q)
term(225) = term(225) + wm_interm_55_pt2(a,p) * wm_interm_74_pt2(a,q)
end do 

term(210) = term(210) * (1.9999999999999998d+0) 
term(211) = term(211) * (-3.9999999999999996d+0) 
term(212) = term(212) * (-3.9999999999999996d+0) 
term(213) = term(213) * (7.999999999999999d+0) 
term(214) = term(214) * (-2.0d+0) 
term(215) = term(215) * (4.0d+0) 
term(216) = term(216) * (4.0d+0) 
term(217) = term(217) * (-8.0d+0) 
term(218) = term(218) * (-3.9999999999999996d+0) 
term(219) = term(219) * (7.999999999999999d+0) 
term(220) = term(220) * (4.0d+0) 
term(221) = term(221) * (-8.0d+0) 
term(222) = term(222) * (1.9999999999999998d+0) 
term(223) = term(223) * (-3.9999999999999996d+0) 
term(224) = term(224) * (-2.0d+0) 
term(225) = term(225) * (4.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do l = 1, nocc 
do k = 1, nocc 
term(226) = term(226) + wm_interm_14_pt2(i,j,k,l) * wm_interm_56_pt2(p,k,q,l,j,i)
term(227) = term(227) + wm_interm_14_pt2(i,j,k,l) * wm_interm_57_pt2(p,k,q,l,j,i)
term(228) = term(228) + wm_interm_14_pt2(i,j,k,l) * wm_interm_58_pt2(p,k,q,l,j,i)
term(229) = term(229) + wm_interm_14_pt2(i,j,k,l) * wm_interm_56_pt2(p,q,k,l,j,i)
term(230) = term(230) + wm_interm_14_pt2(i,j,k,l) * wm_interm_57_pt2(p,q,k,l,j,i)
term(231) = term(231) + wm_interm_14_pt2(i,j,k,l) * wm_interm_58_pt2(p,q,k,l,j,i)
term(232) = term(232) + wm_interm_14_pt2(i,j,k,l) * wm_interm_56_pt2(p,k,l,q,j,i)
term(233) = term(233) + wm_interm_14_pt2(i,j,k,l) * wm_interm_57_pt2(p,k,l,q,j,i)
term(234) = term(234) + wm_interm_14_pt2(i,j,k,l) * wm_interm_58_pt2(p,k,l,q,j,i)
end do 
end do 
end do 
end do 

term(226) = term(226) * (0.3333333333333333d+0) 
term(227) = term(227) * (-0.6666666666666666d+0) 
term(228) = term(228) * (0.3333333333333333d+0) 
term(229) = term(229) * (-0.6666666666666666d+0) 
term(230) = term(230) * (0.3333333333333333d+0) 
term(231) = term(231) * (-0.6666666666666666d+0) 
term(232) = term(232) * (-0.6666666666666666d+0) 
term(233) = term(233) * (0.3333333333333333d+0) 
term(234) = term(234) * (-0.6666666666666666d+0) 

do j = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do k = 1, nocc 
term(235) = term(235) + wm_interm_14_pt2(i,j,k,l) * wm_interm_56_pt2(p,k,q,l,i,j)
term(236) = term(236) + wm_interm_14_pt2(i,j,k,l) * wm_interm_57_pt2(p,k,q,l,i,j)
term(237) = term(237) + wm_interm_14_pt2(i,j,k,l) * wm_interm_58_pt2(p,k,q,l,i,j)
term(238) = term(238) + wm_interm_14_pt2(i,j,k,l) * wm_interm_56_pt2(p,q,k,l,i,j)
term(239) = term(239) + wm_interm_14_pt2(i,j,k,l) * wm_interm_57_pt2(p,q,k,l,i,j)
term(240) = term(240) + wm_interm_14_pt2(i,j,k,l) * wm_interm_58_pt2(p,q,k,l,i,j)
term(241) = term(241) + wm_interm_14_pt2(i,j,k,l) * wm_interm_56_pt2(p,k,l,q,i,j)
term(242) = term(242) + wm_interm_14_pt2(i,j,k,l) * wm_interm_57_pt2(p,k,l,q,i,j)
term(243) = term(243) + wm_interm_14_pt2(i,j,k,l) * wm_interm_58_pt2(p,k,l,q,i,j)
end do 
end do 
end do 
end do 

term(235) = term(235) * (-0.6666666666666666d+0) 
term(236) = term(236) * (1.3333333333333333d+0) 
term(237) = term(237) * (-0.6666666666666666d+0) 
term(238) = term(238) * (1.3333333333333333d+0) 
term(239) = term(239) * (-0.6666666666666666d+0) 
term(240) = term(240) * (0.3333333333333333d+0) 
term(241) = term(241) * (0.3333333333333333d+0) 
term(242) = term(242) * (-0.6666666666666666d+0) 
term(243) = term(243) * (1.3333333333333333d+0) 

do i = 1, nocc 
do j = 1, nocc 
do l = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(244) = term(244) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_56_pt2(b,q,k,l,j,i)
term(245) = term(245) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_56_pt2(b,k,q,l,j,i)
term(246) = term(246) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_57_pt2(b,k,q,l,j,i)
term(247) = term(247) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_58_pt2(b,k,q,l,j,i)
term(248) = term(248) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_57_pt2(b,q,k,l,j,i)
term(249) = term(249) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_58_pt2(b,q,k,l,j,i)
end do 
end do 
end do 
end do 
end do 
end do 

term(244) = term(244) * (0.3333333333333333d+0) 
term(245) = term(245) * (-0.6666666666666666d+0) 
term(246) = term(246) * (0.3333333333333333d+0) 
term(247) = term(247) * (-0.6666666666666666d+0) 
term(248) = term(248) * (-0.6666666666666666d+0) 
term(249) = term(249) * (0.3333333333333333d+0) 

do j = 1, nocc 
do i = 1, nocc 
term(250) = term(250) + wm_interm_0_pt2(p,i,j,q) * wm_interm_1_pt2(j,i)
term(251) = term(251) + wm_interm_0_pt2(p,i,q,j) * wm_interm_1_pt2(j,i)
term(252) = term(252) + wm_interm_0_pt2(p,i,j,q) * wm_interm_2_pt2(j,i)
term(253) = term(253) + wm_interm_0_pt2(p,i,q,j) * wm_interm_2_pt2(j,i)
term(254) = term(254) + wm_interm_16_pt2(i,j) * wm_interm_17_pt2(p,i,q,j)
term(255) = term(255) + wm_interm_16_pt2(i,j) * wm_interm_18_pt2(p,i,q,j)
term(256) = term(256) + wm_interm_16_pt2(i,j) * wm_interm_19_pt2(p,i,q,j)
term(257) = term(257) + wm_interm_16_pt2(i,j) * wm_interm_20_pt2(p,i,q,j)
term(258) = term(258) + wm_interm_16_pt2(i,j) * wm_interm_21_pt2(p,i,q,j)
term(259) = term(259) + wm_interm_16_pt2(i,j) * wm_interm_22_pt2(p,i,q,j)
term(260) = term(260) + wm_interm_17_pt2(p,i,q,j) * wm_interm_25_pt2(i,j)
term(261) = term(261) + wm_interm_18_pt2(p,i,q,j) * wm_interm_25_pt2(i,j)
term(262) = term(262) + wm_interm_19_pt2(p,i,q,j) * wm_interm_25_pt2(i,j)
term(263) = term(263) + wm_interm_20_pt2(p,i,q,j) * wm_interm_25_pt2(i,j)
term(264) = term(264) + wm_interm_21_pt2(p,i,q,j) * wm_interm_25_pt2(i,j)
term(265) = term(265) + wm_interm_22_pt2(p,i,q,j) * wm_interm_25_pt2(i,j)
term(266) = term(266) + wm_interm_59_pt2(p,i,q,j) * wm_interm_60_pt2(i,j)
term(267) = term(267) + wm_interm_59_pt2(p,i,q,j) * wm_interm_61_pt2(i,j)
term(268) = term(268) + wm_interm_59_pt2(p,q,i,j) * wm_interm_60_pt2(i,j)
term(269) = term(269) + wm_interm_59_pt2(p,q,i,j) * wm_interm_61_pt2(i,j)
term(270) = term(270) + wm_interm_60_pt2(i,j) * wm_interm_63_pt2(p,q,i,j)
term(271) = term(271) + wm_interm_61_pt2(i,j) * wm_interm_63_pt2(p,q,i,j)
term(272) = term(272) + wm_interm_60_pt2(i,j) * wm_interm_63_pt2(p,i,q,j)
term(273) = term(273) + wm_interm_61_pt2(i,j) * wm_interm_63_pt2(p,i,q,j)
term(274) = term(274) + wm_interm_60_pt2(i,j) * wm_interm_65_pt2(p,i,q,j)
term(275) = term(275) + wm_interm_61_pt2(i,j) * wm_interm_65_pt2(p,i,q,j)
term(276) = term(276) + wm_interm_60_pt2(i,j) * wm_interm_65_pt2(p,q,i,j)
term(277) = term(277) + wm_interm_61_pt2(i,j) * wm_interm_65_pt2(p,q,i,j)
term(278) = term(278) + wm_interm_60_pt2(i,j) * wm_interm_66_pt2(p,i,q,j)
term(279) = term(279) + wm_interm_61_pt2(i,j) * wm_interm_66_pt2(p,i,q,j)
term(280) = term(280) + wm_interm_60_pt2(i,j) * wm_interm_66_pt2(p,q,i,j)
term(281) = term(281) + wm_interm_61_pt2(i,j) * wm_interm_66_pt2(p,q,i,j)
term(282) = term(282) + wm_interm_60_pt2(i,j) * wm_interm_68_pt2(p,q,i,j)
term(283) = term(283) + wm_interm_61_pt2(i,j) * wm_interm_68_pt2(p,q,i,j)
term(284) = term(284) + wm_interm_60_pt2(i,j) * wm_interm_68_pt2(p,i,q,j)
term(285) = term(285) + wm_interm_61_pt2(i,j) * wm_interm_68_pt2(p,i,q,j)
term(286) = term(286) + wm_interm_60_pt2(i,j) * wm_interm_69_pt2(p,q,i,j)
term(287) = term(287) + wm_interm_61_pt2(i,j) * wm_interm_69_pt2(p,q,i,j)
term(288) = term(288) + wm_interm_60_pt2(i,j) * wm_interm_69_pt2(p,i,q,j)
term(289) = term(289) + wm_interm_61_pt2(i,j) * wm_interm_69_pt2(p,i,q,j)
end do 
end do 

term(251) = term(251) * (-1.9999999999999998d+0) 
term(252) = term(252) * (-1.9999999999999998d+0) 
term(253) = term(253) * (3.9999999999999996d+0) 
term(254) = term(254) * (2.0d+0) 
term(255) = term(255) * (-4.0d+0) 
term(256) = term(256) * (2.0d+0) 
term(257) = term(257) * (-4.0d+0) 
term(258) = term(258) * (-4.0d+0) 
term(259) = term(259) * (8.0d+0) 
term(260) = term(260) * (-4.0d+0) 
term(261) = term(261) * (8.0d+0) 
term(262) = term(262) * (-4.0d+0) 
term(263) = term(263) * (8.0d+0) 
term(264) = term(264) * (8.0d+0) 
term(265) = term(265) * (-16.0d+0) 
term(267) = term(267) * (-2.0d+0) 
term(268) = term(268) * (-2.0d+0) 
term(269) = term(269) * (4.0d+0) 
term(271) = term(271) * (-2.0d+0) 
term(272) = term(272) * (-2.0d+0) 
term(273) = term(273) * (4.0d+0) 
term(274) = term(274) * (-2.0d+0) 
term(275) = term(275) * (4.0d+0) 
term(276) = term(276) * (4.0d+0) 
term(277) = term(277) * (-8.0d+0) 
term(279) = term(279) * (-2.0d+0) 
term(280) = term(280) * (-2.0d+0) 
term(281) = term(281) * (4.0d+0) 
term(282) = term(282) * (-2.0d+0) 
term(283) = term(283) * (4.0d+0) 
term(284) = term(284) * (4.0d+0) 
term(285) = term(285) * (-8.0d+0) 
term(287) = term(287) * (-2.0d+0) 
term(288) = term(288) * (-2.0d+0) 
term(289) = term(289) * (4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(290) = term(290) + wm_interm_36_pt2(p,a,i,j) * wm_interm_46_pt2(a,q,j,i)
term(291) = term(291) + wm_interm_37_pt2(p,a,i,j) * wm_interm_46_pt2(a,q,j,i)
term(292) = term(292) + wm_interm_36_pt2(p,a,i,j) * wm_interm_47_pt2(a,q,j,i)
term(293) = term(293) + wm_interm_37_pt2(p,a,i,j) * wm_interm_47_pt2(a,q,j,i)
term(294) = term(294) + wm_interm_36_pt2(p,a,i,j) * wm_interm_41_pt2(a,q,j,i)
term(295) = term(295) + wm_interm_37_pt2(p,a,i,j) * wm_interm_41_pt2(a,q,j,i)
term(296) = term(296) + wm_interm_36_pt2(p,a,i,j) * wm_interm_40_pt2(a,q,j,i)
term(297) = term(297) + wm_interm_37_pt2(p,a,i,j) * wm_interm_40_pt2(a,q,j,i)
term(298) = term(298) + wm_interm_36_pt2(p,a,i,j) * wm_interm_46_pt2(a,j,q,i)
term(299) = term(299) + wm_interm_36_pt2(p,a,i,j) * wm_interm_47_pt2(a,j,q,i)
term(300) = term(300) + wm_interm_37_pt2(p,a,i,j) * wm_interm_46_pt2(a,j,q,i)
term(301) = term(301) + wm_interm_37_pt2(p,a,i,j) * wm_interm_47_pt2(a,j,q,i)
term(302) = term(302) + wm_interm_36_pt2(p,a,i,j) * wm_interm_44_pt2(a,q,j,i)
term(303) = term(303) + wm_interm_36_pt2(p,a,i,j) * wm_interm_45_pt2(a,q,j,i)
term(304) = term(304) + wm_interm_37_pt2(p,a,i,j) * wm_interm_44_pt2(a,q,j,i)
term(305) = term(305) + wm_interm_37_pt2(p,a,i,j) * wm_interm_45_pt2(a,q,j,i)
term(306) = term(306) + wm_interm_36_pt2(p,a,i,j) * wm_interm_44_pt2(a,j,q,i)
term(307) = term(307) + wm_interm_36_pt2(p,a,i,j) * wm_interm_45_pt2(a,j,q,i)
term(308) = term(308) + wm_interm_37_pt2(p,a,i,j) * wm_interm_44_pt2(a,j,q,i)
term(309) = term(309) + wm_interm_37_pt2(p,a,i,j) * wm_interm_45_pt2(a,j,q,i)
term(310) = term(310) + wm_interm_36_pt2(p,a,i,j) * wm_interm_40_pt2(a,j,q,i)
term(311) = term(311) + wm_interm_36_pt2(p,a,i,j) * wm_interm_41_pt2(a,j,q,i)
term(312) = term(312) + wm_interm_37_pt2(p,a,i,j) * wm_interm_40_pt2(a,j,q,i)
term(313) = term(313) + wm_interm_37_pt2(p,a,i,j) * wm_interm_41_pt2(a,j,q,i)
term(314) = term(314) + wm_interm_38_pt2(p,a,i,j) * wm_interm_41_pt2(a,q,j,i)
term(315) = term(315) + wm_interm_38_pt2(p,a,i,j) * wm_interm_40_pt2(a,q,j,i)
term(316) = term(316) + wm_interm_39_pt2(p,a,i,j) * wm_interm_40_pt2(a,q,j,i)
term(317) = term(317) + wm_interm_39_pt2(p,a,i,j) * wm_interm_41_pt2(a,q,j,i)
term(318) = term(318) + wm_interm_38_pt2(p,a,i,j) * wm_interm_44_pt2(a,q,j,i)
term(319) = term(319) + wm_interm_38_pt2(p,a,i,j) * wm_interm_45_pt2(a,q,j,i)
term(320) = term(320) + wm_interm_39_pt2(p,a,i,j) * wm_interm_44_pt2(a,q,j,i)
term(321) = term(321) + wm_interm_39_pt2(p,a,i,j) * wm_interm_45_pt2(a,q,j,i)
term(322) = term(322) + wm_interm_39_pt2(p,a,i,j) * wm_interm_46_pt2(a,q,j,i)
term(323) = term(323) + wm_interm_38_pt2(p,a,i,j) * wm_interm_46_pt2(a,q,j,i)
term(324) = term(324) + wm_interm_39_pt2(p,a,i,j) * wm_interm_47_pt2(a,q,j,i)
term(325) = term(325) + wm_interm_38_pt2(p,a,i,j) * wm_interm_47_pt2(a,q,j,i)
term(326) = term(326) + wm_interm_52_pt2(a,p,i,j) * wm_interm_59_pt2(a,q,i,j)
term(327) = term(327) + wm_interm_53_pt2(a,p,i,j) * wm_interm_59_pt2(a,q,i,j)
term(328) = term(328) + wm_interm_51_pt2(a,p,i,j) * wm_interm_59_pt2(a,q,i,j)
term(329) = term(329) + wm_interm_50_pt2(a,p,i,j) * wm_interm_59_pt2(a,q,i,j)
term(330) = term(330) + wm_interm_52_pt2(a,p,i,j) * wm_interm_59_pt2(a,i,q,j)
term(331) = term(331) + wm_interm_53_pt2(a,p,i,j) * wm_interm_59_pt2(a,i,q,j)
term(332) = term(332) + wm_interm_50_pt2(a,p,i,j) * wm_interm_59_pt2(a,i,q,j)
term(333) = term(333) + wm_interm_51_pt2(a,p,i,j) * wm_interm_59_pt2(a,i,q,j)
term(334) = term(334) + wm_interm_52_pt2(a,p,i,j) * wm_interm_63_pt2(a,i,q,j)
term(335) = term(335) + wm_interm_53_pt2(a,p,i,j) * wm_interm_63_pt2(a,i,q,j)
term(336) = term(336) + wm_interm_51_pt2(a,p,i,j) * wm_interm_63_pt2(a,i,q,j)
term(337) = term(337) + wm_interm_50_pt2(a,p,i,j) * wm_interm_63_pt2(a,i,q,j)
term(338) = term(338) + wm_interm_52_pt2(a,p,i,j) * wm_interm_65_pt2(a,q,i,j)
term(339) = term(339) + wm_interm_53_pt2(a,p,i,j) * wm_interm_65_pt2(a,q,i,j)
term(340) = term(340) + wm_interm_51_pt2(a,p,i,j) * wm_interm_65_pt2(a,q,i,j)
term(341) = term(341) + wm_interm_50_pt2(a,p,i,j) * wm_interm_65_pt2(a,q,i,j)
term(342) = term(342) + wm_interm_52_pt2(a,p,i,j) * wm_interm_63_pt2(a,q,i,j)
term(343) = term(343) + wm_interm_53_pt2(a,p,i,j) * wm_interm_63_pt2(a,q,i,j)
term(344) = term(344) + wm_interm_50_pt2(a,p,i,j) * wm_interm_63_pt2(a,q,i,j)
term(345) = term(345) + wm_interm_51_pt2(a,p,i,j) * wm_interm_63_pt2(a,q,i,j)
term(346) = term(346) + wm_interm_52_pt2(a,p,i,j) * wm_interm_65_pt2(a,i,q,j)
term(347) = term(347) + wm_interm_53_pt2(a,p,i,j) * wm_interm_65_pt2(a,i,q,j)
term(348) = term(348) + wm_interm_50_pt2(a,p,i,j) * wm_interm_65_pt2(a,i,q,j)
term(349) = term(349) + wm_interm_51_pt2(a,p,i,j) * wm_interm_65_pt2(a,i,q,j)
term(350) = term(350) + wm_interm_38_pt2(p,a,i,j) * wm_interm_40_pt2(a,j,q,i)
term(351) = term(351) + wm_interm_38_pt2(p,a,i,j) * wm_interm_41_pt2(a,j,q,i)
term(352) = term(352) + wm_interm_39_pt2(p,a,i,j) * wm_interm_41_pt2(a,j,q,i)
term(353) = term(353) + wm_interm_39_pt2(p,a,i,j) * wm_interm_40_pt2(a,j,q,i)
term(354) = term(354) + wm_interm_39_pt2(p,a,i,j) * wm_interm_46_pt2(a,j,q,i)
term(355) = term(355) + wm_interm_38_pt2(p,a,i,j) * wm_interm_46_pt2(a,j,q,i)
term(356) = term(356) + wm_interm_39_pt2(p,a,i,j) * wm_interm_47_pt2(a,j,q,i)
term(357) = term(357) + wm_interm_38_pt2(p,a,i,j) * wm_interm_47_pt2(a,j,q,i)
term(358) = term(358) + wm_interm_52_pt2(a,p,i,j) * wm_interm_66_pt2(a,q,i,j)
term(359) = term(359) + wm_interm_53_pt2(a,p,i,j) * wm_interm_66_pt2(a,q,i,j)
term(360) = term(360) + wm_interm_51_pt2(a,p,i,j) * wm_interm_66_pt2(a,q,i,j)
term(361) = term(361) + wm_interm_50_pt2(a,p,i,j) * wm_interm_66_pt2(a,q,i,j)
term(362) = term(362) + wm_interm_52_pt2(a,p,i,j) * wm_interm_66_pt2(a,i,q,j)
term(363) = term(363) + wm_interm_53_pt2(a,p,i,j) * wm_interm_66_pt2(a,i,q,j)
term(364) = term(364) + wm_interm_50_pt2(a,p,i,j) * wm_interm_66_pt2(a,i,q,j)
term(365) = term(365) + wm_interm_51_pt2(a,p,i,j) * wm_interm_66_pt2(a,i,q,j)
term(366) = term(366) + wm_interm_52_pt2(a,p,i,j) * wm_interm_68_pt2(a,i,q,j)
term(367) = term(367) + wm_interm_53_pt2(a,p,i,j) * wm_interm_68_pt2(a,i,q,j)
term(368) = term(368) + wm_interm_52_pt2(a,p,i,j) * wm_interm_68_pt2(a,q,i,j)
term(369) = term(369) + wm_interm_53_pt2(a,p,i,j) * wm_interm_68_pt2(a,q,i,j)
term(370) = term(370) + wm_interm_50_pt2(a,p,i,j) * wm_interm_68_pt2(a,q,i,j)
term(371) = term(371) + wm_interm_51_pt2(a,p,i,j) * wm_interm_68_pt2(a,q,i,j)
term(372) = term(372) + wm_interm_51_pt2(a,p,i,j) * wm_interm_68_pt2(a,i,q,j)
term(373) = term(373) + wm_interm_50_pt2(a,p,i,j) * wm_interm_68_pt2(a,i,q,j)
term(374) = term(374) + wm_interm_39_pt2(p,a,i,j) * wm_interm_44_pt2(a,j,q,i)
term(375) = term(375) + wm_interm_39_pt2(p,a,i,j) * wm_interm_45_pt2(a,j,q,i)
term(376) = term(376) + wm_interm_38_pt2(p,a,i,j) * wm_interm_44_pt2(a,j,q,i)
term(377) = term(377) + wm_interm_38_pt2(p,a,i,j) * wm_interm_45_pt2(a,j,q,i)
term(378) = term(378) + wm_interm_52_pt2(a,p,i,j) * wm_interm_69_pt2(a,i,q,j)
term(379) = term(379) + wm_interm_53_pt2(a,p,i,j) * wm_interm_69_pt2(a,i,q,j)
term(380) = term(380) + wm_interm_52_pt2(a,p,i,j) * wm_interm_69_pt2(a,q,i,j)
term(381) = term(381) + wm_interm_53_pt2(a,p,i,j) * wm_interm_69_pt2(a,q,i,j)
term(382) = term(382) + wm_interm_50_pt2(a,p,i,j) * wm_interm_69_pt2(a,q,i,j)
term(383) = term(383) + wm_interm_51_pt2(a,p,i,j) * wm_interm_69_pt2(a,q,i,j)
term(384) = term(384) + wm_interm_51_pt2(a,p,i,j) * wm_interm_69_pt2(a,i,q,j)
term(385) = term(385) + wm_interm_50_pt2(a,p,i,j) * wm_interm_69_pt2(a,i,q,j)
end do 
end do 
end do 

term(290) = term(290) * (-2.0d+0) 
term(291) = term(291) * (4.0d+0) 
term(292) = term(292) * (4.0d+0) 
term(293) = term(293) * (-8.0d+0) 
term(295) = term(295) * (-2.0d+0) 
term(296) = term(296) * (-2.0d+0) 
term(297) = term(297) * (4.0d+0) 
term(299) = term(299) * (-2.0d+0) 
term(300) = term(300) * (-2.0d+0) 
term(301) = term(301) * (4.0d+0) 
term(303) = term(303) * (-2.0d+0) 
term(304) = term(304) * (-2.0d+0) 
term(305) = term(305) * (4.0d+0) 
term(306) = term(306) * (-2.0d+0) 
term(307) = term(307) * (4.0d+0) 
term(308) = term(308) * (4.0d+0) 
term(309) = term(309) * (-8.0d+0) 
term(311) = term(311) * (-2.0d+0) 
term(312) = term(312) * (-2.0d+0) 
term(313) = term(313) * (4.0d+0) 
term(315) = term(315) * (-2.0d+0) 
term(317) = term(317) * (-2.0d+0) 
term(319) = term(319) * (-2.0d+0) 
term(320) = term(320) * (-2.0d+0) 
term(321) = term(321) * (4.0d+0) 
term(323) = term(323) * (-2.0d+0) 
term(324) = term(324) * (-2.0d+0) 
term(325) = term(325) * (4.0d+0) 
term(327) = term(327) * (-2.0d+0) 
term(329) = term(329) * (-2.0d+0) 
term(330) = term(330) * (-2.0d+0) 
term(331) = term(331) * (4.0d+0) 
term(333) = term(333) * (-2.0d+0) 
term(335) = term(335) * (-2.0d+0) 
term(337) = term(337) * (-2.0d+0) 
term(338) = term(338) * (-2.0d+0) 
term(339) = term(339) * (4.0d+0) 
term(340) = term(340) * (-2.0d+0) 
term(341) = term(341) * (4.0d+0) 
term(342) = term(342) * (-2.0d+0) 
term(343) = term(343) * (4.0d+0) 
term(345) = term(345) * (-2.0d+0) 
term(346) = term(346) * (4.0d+0) 
term(347) = term(347) * (-8.0d+0) 
term(348) = term(348) * (-2.0d+0) 
term(349) = term(349) * (4.0d+0) 
term(351) = term(351) * (-2.0d+0) 
term(353) = term(353) * (-2.0d+0) 
term(354) = term(354) * (-2.0d+0) 
term(356) = term(356) * (4.0d+0) 
term(357) = term(357) * (-2.0d+0) 
term(359) = term(359) * (-2.0d+0) 
term(361) = term(361) * (-2.0d+0) 
term(362) = term(362) * (-2.0d+0) 
term(363) = term(363) * (4.0d+0) 
term(365) = term(365) * (-2.0d+0) 
term(366) = term(366) * (-2.0d+0) 
term(367) = term(367) * (4.0d+0) 
term(368) = term(368) * (4.0d+0) 
term(369) = term(369) * (-8.0d+0) 
term(370) = term(370) * (-2.0d+0) 
term(371) = term(371) * (4.0d+0) 
term(372) = term(372) * (-2.0d+0) 
term(373) = term(373) * (4.0d+0) 
term(375) = term(375) * (-2.0d+0) 
term(376) = term(376) * (-2.0d+0) 
term(377) = term(377) * (4.0d+0) 
term(379) = term(379) * (-2.0d+0) 
term(380) = term(380) * (-2.0d+0) 
term(381) = term(381) * (4.0d+0) 
term(383) = term(383) * (-2.0d+0) 
term(385) = term(385) * (-2.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
term(386) = term(386) + wm_interm_1_pt2(i,j) * wm_interm_3_pt2(p,j,q,i)
term(387) = term(387) + wm_interm_1_pt2(i,j) * wm_interm_4_pt2(p,j,q,i)
term(388) = term(388) + wm_interm_1_pt2(i,j) * wm_interm_3_pt2(p,j,i,q)
term(389) = term(389) + wm_interm_1_pt2(i,j) * wm_interm_4_pt2(p,j,i,q)
term(390) = term(390) + wm_interm_2_pt2(i,j) * wm_interm_3_pt2(p,j,q,i)
term(391) = term(391) + wm_interm_2_pt2(i,j) * wm_interm_4_pt2(p,j,q,i)
term(392) = term(392) + wm_interm_2_pt2(i,j) * wm_interm_3_pt2(p,j,i,q)
term(393) = term(393) + wm_interm_2_pt2(i,j) * wm_interm_4_pt2(p,j,i,q)
term(394) = term(394) + wm_interm_1_pt2(i,j) * wm_interm_9_pt2(p,j,q,i)
term(395) = term(395) + wm_interm_2_pt2(i,j) * wm_interm_9_pt2(p,j,q,i)
term(396) = term(396) + wm_interm_1_pt2(i,j) * wm_interm_9_pt2(p,j,i,q)
term(397) = term(397) + wm_interm_2_pt2(i,j) * wm_interm_9_pt2(p,j,i,q)
term(398) = term(398) + wm_interm_1_pt2(i,j) * wm_interm_40_pt2(p,q,j,i)
term(399) = term(399) + wm_interm_1_pt2(i,j) * wm_interm_41_pt2(p,q,j,i)
term(400) = term(400) + wm_interm_2_pt2(i,j) * wm_interm_40_pt2(p,q,j,i)
term(401) = term(401) + wm_interm_2_pt2(i,j) * wm_interm_41_pt2(p,q,j,i)
term(402) = term(402) + wm_interm_1_pt2(i,j) * wm_interm_44_pt2(p,q,j,i)
term(403) = term(403) + wm_interm_1_pt2(i,j) * wm_interm_45_pt2(p,q,j,i)
term(404) = term(404) + wm_interm_2_pt2(i,j) * wm_interm_44_pt2(p,q,j,i)
term(405) = term(405) + wm_interm_2_pt2(i,j) * wm_interm_45_pt2(p,q,j,i)
term(406) = term(406) + wm_interm_1_pt2(i,j) * wm_interm_46_pt2(p,q,j,i)
term(407) = term(407) + wm_interm_2_pt2(i,j) * wm_interm_46_pt2(p,q,j,i)
term(408) = term(408) + wm_interm_1_pt2(i,j) * wm_interm_47_pt2(p,q,j,i)
term(409) = term(409) + wm_interm_2_pt2(i,j) * wm_interm_47_pt2(p,q,j,i)
term(410) = term(410) + wm_interm_1_pt2(i,j) * wm_interm_41_pt2(p,j,q,i)
term(411) = term(411) + wm_interm_2_pt2(i,j) * wm_interm_41_pt2(p,j,q,i)
term(412) = term(412) + wm_interm_1_pt2(i,j) * wm_interm_40_pt2(p,j,q,i)
term(413) = term(413) + wm_interm_2_pt2(i,j) * wm_interm_40_pt2(p,j,q,i)
term(414) = term(414) + wm_interm_1_pt2(i,j) * wm_interm_44_pt2(p,j,q,i)
term(415) = term(415) + wm_interm_1_pt2(i,j) * wm_interm_45_pt2(p,j,q,i)
term(416) = term(416) + wm_interm_2_pt2(i,j) * wm_interm_44_pt2(p,j,q,i)
term(417) = term(417) + wm_interm_2_pt2(i,j) * wm_interm_45_pt2(p,j,q,i)
term(418) = term(418) + wm_interm_1_pt2(i,j) * wm_interm_46_pt2(p,j,q,i)
term(419) = term(419) + wm_interm_1_pt2(i,j) * wm_interm_47_pt2(p,j,q,i)
term(420) = term(420) + wm_interm_2_pt2(i,j) * wm_interm_46_pt2(p,j,q,i)
term(421) = term(421) + wm_interm_2_pt2(i,j) * wm_interm_47_pt2(p,j,q,i)
end do 
end do 

term(386) = term(386) * (1.9999999999999998d+0) 
term(387) = term(387) * (-3.9999999999999996d+0) 
term(388) = term(388) * (-3.9999999999999996d+0) 
term(389) = term(389) * (7.999999999999999d+0) 
term(390) = term(390) * (-3.9999999999999996d+0) 
term(391) = term(391) * (7.999999999999999d+0) 
term(392) = term(392) * (7.999999999999999d+0) 
term(393) = term(393) * (-15.999999999999998d+0) 
term(395) = term(395) * (-1.9999999999999998d+0) 
term(396) = term(396) * (-1.9999999999999998d+0) 
term(397) = term(397) * (3.9999999999999996d+0) 
term(399) = term(399) * (-2.0d+0) 
term(400) = term(400) * (-2.0d+0) 
term(401) = term(401) * (4.0d+0) 
term(402) = term(402) * (-2.0d+0) 
term(403) = term(403) * (4.0d+0) 
term(404) = term(404) * (4.0d+0) 
term(405) = term(405) * (-8.0d+0) 
term(407) = term(407) * (-2.0d+0) 
term(408) = term(408) * (-2.0d+0) 
term(409) = term(409) * (4.0d+0) 
term(411) = term(411) * (-2.0d+0) 
term(412) = term(412) * (-2.0d+0) 
term(413) = term(413) * (4.0d+0) 
term(415) = term(415) * (-2.0d+0) 
term(416) = term(416) * (-2.0d+0) 
term(417) = term(417) * (4.0d+0) 
term(418) = term(418) * (-2.0d+0) 
term(419) = term(419) * (4.0d+0) 
term(420) = term(420) * (4.0d+0) 
term(421) = term(421) * (-8.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(422) = term(422) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_56_pt2(b,q,k,l,i,j)
term(423) = term(423) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_56_pt2(b,k,q,l,i,j)
term(424) = term(424) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_57_pt2(b,k,q,l,i,j)
term(425) = term(425) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_58_pt2(b,k,q,l,i,j)
term(426) = term(426) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_57_pt2(b,q,k,l,i,j)
term(427) = term(427) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_58_pt2(b,q,k,l,i,j)
end do 
end do 
end do 
end do 
end do 
end do 

term(422) = term(422) * (-0.6666666666666666d+0) 
term(423) = term(423) * (1.3333333333333333d+0) 
term(424) = term(424) * (-0.6666666666666666d+0) 
term(425) = term(425) * (0.3333333333333333d+0) 
term(426) = term(426) * (1.3333333333333333d+0) 
term(427) = term(427) * (-0.6666666666666666d+0) 

do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do l = 1, nocc 
do a = nocc + 1, nactive 
term(428) = term(428) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_58_pt2(b,l,q,k,i,j)
term(429) = term(429) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_58_pt2(b,q,l,k,i,j)
term(430) = term(430) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_56_pt2(b,q,l,k,i,j)
term(431) = term(431) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_56_pt2(b,k,l,q,i,j)
term(432) = term(432) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_57_pt2(b,k,l,q,i,j)
term(433) = term(433) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_58_pt2(b,k,l,q,i,j)
term(434) = term(434) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_57_pt2(b,q,l,k,i,j)
term(435) = term(435) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_58_pt2(b,l,k,q,i,j)
term(436) = term(436) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_57_pt2(b,l,q,k,i,j)
term(437) = term(437) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_57_pt2(b,l,k,q,i,j)
term(438) = term(438) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_56_pt2(b,l,k,q,i,j)
term(439) = term(439) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_56_pt2(b,l,q,k,i,j)
end do 
end do 
end do 
end do 
end do 
end do 

term(428) = term(428) * (-0.6666666666666666d+0) 
term(429) = term(429) * (1.3333333333333333d+0) 
term(430) = term(430) * (0.3333333333333333d+0) 
term(431) = term(431) * (-0.6666666666666666d+0) 
term(432) = term(432) * (0.3333333333333333d+0) 
term(433) = term(433) * (-0.6666666666666666d+0) 
term(434) = term(434) * (-0.6666666666666666d+0) 
term(435) = term(435) * (0.3333333333333333d+0) 
term(436) = term(436) * (0.3333333333333333d+0) 
term(437) = term(437) * (-0.6666666666666666d+0) 
term(438) = term(438) * (0.3333333333333333d+0) 
term(439) = term(439) * (-0.6666666666666666d+0) 

do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do l = 1, nocc 
do a = nocc + 1, nactive 
term(440) = term(440) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_58_pt2(b,l,q,k,j,i)
term(441) = term(441) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_58_pt2(b,q,l,k,j,i)
term(442) = term(442) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_56_pt2(b,q,l,k,j,i)
term(443) = term(443) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_56_pt2(b,k,l,q,j,i)
term(444) = term(444) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_57_pt2(b,k,l,q,j,i)
term(445) = term(445) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_58_pt2(b,k,l,q,j,i)
term(446) = term(446) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_57_pt2(b,q,l,k,j,i)
term(447) = term(447) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_58_pt2(b,l,k,q,j,i)
term(448) = term(448) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_57_pt2(b,l,q,k,j,i)
term(449) = term(449) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_57_pt2(b,l,k,q,j,i)
term(450) = term(450) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_56_pt2(b,l,k,q,j,i)
term(451) = term(451) + s2(a,p,j,i) * t2(a,b,l,k) * wm_interm_56_pt2(b,l,q,k,j,i)
end do 
end do 
end do 
end do 
end do 
end do 

term(440) = term(440) * (1.3333333333333333d+0) 
term(441) = term(441) * (-0.6666666666666666d+0) 
term(442) = term(442) * (-0.6666666666666666d+0) 
term(443) = term(443) * (1.3333333333333333d+0) 
term(444) = term(444) * (-0.6666666666666666d+0) 
term(445) = term(445) * (0.3333333333333333d+0) 
term(446) = term(446) * (0.3333333333333333d+0) 
term(447) = term(447) * (-0.6666666666666666d+0) 
term(448) = term(448) * (-0.6666666666666666d+0) 
term(449) = term(449) * (1.3333333333333333d+0) 
term(450) = term(450) * (-0.6666666666666666d+0) 
term(451) = term(451) * (0.3333333333333333d+0) 

do k = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(452) = term(452) + r2(vrdav_Rl, a,j,b,k) * t2(a,p,q,i) * wm_interm_17_pt2(b,j,k,i)
term(453) = term(453) + r2(vrdav_Rl, a,j,b,k) * t2(a,p,q,i) * wm_interm_17_pt2(b,k,j,i)
term(454) = term(454) + r2(vrdav_Rl, a,j,b,k) * t2(a,p,q,i) * wm_interm_19_pt2(b,j,k,i)
term(455) = term(455) + r2(vrdav_Rl, a,j,b,k) * t2(a,p,q,i) * wm_interm_18_pt2(b,j,k,i)
term(456) = term(456) + r2(vrdav_Rl, a,j,b,k) * t2(a,p,q,i) * wm_interm_20_pt2(b,j,k,i)
term(457) = term(457) + r2(vrdav_Rl, a,j,b,k) * t2(a,p,q,i) * wm_interm_18_pt2(b,k,j,i)
end do 
end do 
end do 
end do 
end do 

term(453) = term(453) * (-1.9999999999999998d+0) 
term(455) = term(455) * (-1.9999999999999998d+0) 
term(456) = term(456) * (-1.9999999999999998d+0) 
term(457) = term(457) * (3.9999999999999996d+0) 

do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(458) = term(458) + s2(a,b,i,j) * t2(b,p,k,q) * wm_interm_3_pt2(a,k,i,j)
term(459) = term(459) + s2(a,b,i,j) * t2(b,p,k,q) * wm_interm_4_pt2(a,k,i,j)
term(460) = term(460) + s2(a,b,i,j) * t2(b,p,k,q) * wm_interm_9_pt2(a,k,i,j)
term(461) = term(461) + s2(a,b,i,j) * t2(b,p,k,q) * wm_interm_0_pt2(a,k,i,j)
end do 
end do 
end do 
end do 
end do 

term(458) = term(458) * (-3.9999999999999996d+0) 
term(459) = term(459) * (7.999999999999999d+0) 
term(460) = term(460) * (-1.9999999999999998d+0) 
term(461) = term(461) * (3.9999999999999996d+0) 

do i = 1, nocc 
do j = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(462) = term(462) + r2(vrdav_Rl, b,i,c,j) * r3(vrdav_Rr, a,q,b,j,p,i) * wm_interm_5_pt2(a,c)
term(463) = term(463) + r2(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,q,c,j,p,i) * wm_interm_5_pt2(c,b)
term(464) = term(464) + r2(vrdav_Rl, b,i,c,j) * r3(vrdav_Rr, a,q,b,j,p,i) * wm_interm_7_pt2(a,c)
term(465) = term(465) + r2(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,q,c,j,p,i) * wm_interm_7_pt2(c,b)
term(466) = term(466) + r2(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,q,c,j,p,i) * wm_interm_5_pt2(c,b)
term(467) = term(467) + r2(vrdav_Rl, b,j,c,i) * r3(vrdav_Rr, a,q,b,j,p,i) * wm_interm_5_pt2(a,c)
term(468) = term(468) + r2(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,q,c,j,p,i) * wm_interm_7_pt2(c,b)
term(469) = term(469) + r2(vrdav_Rl, b,j,c,i) * r3(vrdav_Rr, a,q,b,j,p,i) * wm_interm_7_pt2(a,c)
term(470) = term(470) + r2(vrdav_Rl, b,j,c,i) * r3(vrdav_Rr, a,q,c,j,p,i) * wm_interm_5_pt2(a,b)
term(471) = term(471) + r2(vrdav_Rl, b,j,c,i) * r3(vrdav_Rr, a,q,c,j,p,i) * wm_interm_7_pt2(a,b)
term(472) = term(472) + r2(vrdav_Rl, b,i,c,j) * r3(vrdav_Rr, a,q,c,j,p,i) * wm_interm_5_pt2(a,b)
term(473) = term(473) + r2(vrdav_Rl, b,i,c,j) * r3(vrdav_Rr, a,q,c,j,p,i) * wm_interm_7_pt2(a,b)
term(474) = term(474) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,j,i) * wm_interm_11_pt2(b,c)
term(475) = term(475) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,j,i) * wm_interm_11_pt2(a,c)
term(476) = term(476) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,i,j) * wm_interm_11_pt2(a,c)
term(477) = term(477) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,i,j) * wm_interm_11_pt2(b,c)
term(478) = term(478) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,j,i) * wm_interm_12_pt2(b,c)
term(479) = term(479) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,j,i) * wm_interm_12_pt2(a,c)
term(480) = term(480) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(b,c,i,j) * wm_interm_12_pt2(a,c)
term(481) = term(481) + r3(vrdav_Rr, a,q,b,j,p,i) * s2(a,c,i,j) * wm_interm_12_pt2(b,c)
term(482) = term(482) + r3(vrdav_Rl, a,q,b,j,p,i) * r2(vrdav_Rr, a,j,c,i) * wm_interm_5_pt2(c,b)
term(483) = term(483) + r3(vrdav_Rl, a,q,b,j,p,i) * r2(vrdav_Rr, a,i,c,j) * wm_interm_5_pt2(c,b)
term(484) = term(484) + r3(vrdav_Rl, a,q,b,j,p,i) * r2(vrdav_Rr, a,j,c,i) * wm_interm_7_pt2(c,b)
term(485) = term(485) + r3(vrdav_Rl, a,q,b,j,p,i) * r2(vrdav_Rr, a,i,c,j) * wm_interm_7_pt2(c,b)
term(486) = term(486) + r3(vrdav_Rl, a,q,b,j,p,i) * r2(vrdav_Rr, b,i,c,j) * wm_interm_5_pt2(c,a)
term(487) = term(487) + r3(vrdav_Rl, a,q,b,j,p,i) * r2(vrdav_Rr, b,j,c,i) * wm_interm_5_pt2(c,a)
term(488) = term(488) + r3(vrdav_Rl, a,q,b,j,p,i) * r2(vrdav_Rr, b,i,c,j) * wm_interm_7_pt2(c,a)
term(489) = term(489) + r3(vrdav_Rl, a,q,b,j,p,i) * r2(vrdav_Rr, b,j,c,i) * wm_interm_7_pt2(c,a)
term(490) = term(490) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(a,c,i,j) * wm_interm_54_pt2(b,c)
term(491) = term(491) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(a,c,i,j) * wm_interm_55_pt2(b,c)
term(492) = term(492) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(a,c,j,i) * wm_interm_54_pt2(b,c)
term(493) = term(493) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(a,c,j,i) * wm_interm_55_pt2(b,c)
term(494) = term(494) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(b,c,j,i) * wm_interm_54_pt2(a,c)
term(495) = term(495) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(b,c,j,i) * wm_interm_55_pt2(a,c)
term(496) = term(496) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(b,c,i,j) * wm_interm_54_pt2(a,c)
term(497) = term(497) + r3(vrdav_Rl, a,q,b,j,p,i) * t2(b,c,i,j) * wm_interm_55_pt2(a,c)
end do 
end do 
end do 
end do 
end do 

term(463) = term(463) * (-3.9999999999999996d+0) 
term(464) = term(464) * (-1.9999999999999998d+0) 
term(465) = term(465) * (7.999999999999999d+0) 
term(466) = term(466) * (1.9999999999999998d+0) 
term(467) = term(467) * (-1.9999999999999998d+0) 
term(468) = term(468) * (-3.9999999999999996d+0) 
term(469) = term(469) * (3.9999999999999996d+0) 
term(471) = term(471) * (-1.9999999999999998d+0) 
term(472) = term(472) * (-1.9999999999999998d+0) 
term(473) = term(473) * (3.9999999999999996d+0) 
term(474) = term(474) * (-4.0d+0) 
term(475) = term(475) * (8.0d+0) 
term(476) = term(476) * (-4.0d+0) 
term(477) = term(477) * (8.0d+0) 
term(478) = term(478) * (2.0d+0) 
term(479) = term(479) * (-4.0d+0) 
term(480) = term(480) * (2.0d+0) 
term(481) = term(481) * (-4.0d+0) 
term(482) = term(482) * (1.9999999999999998d+0) 
term(483) = term(483) * (-3.9999999999999996d+0) 
term(484) = term(484) * (-3.9999999999999996d+0) 
term(485) = term(485) * (7.999999999999999d+0) 
term(486) = term(486) * (1.9999999999999998d+0) 
term(487) = term(487) * (-3.9999999999999996d+0) 
term(488) = term(488) * (-3.9999999999999996d+0) 
term(489) = term(489) * (7.999999999999999d+0) 
term(490) = term(490) * (-3.9999999999999996d+0) 
term(491) = term(491) * (7.999999999999999d+0) 
term(492) = term(492) * (1.9999999999999998d+0) 
term(493) = term(493) * (-3.9999999999999996d+0) 
term(494) = term(494) * (-3.9999999999999996d+0) 
term(495) = term(495) * (7.999999999999999d+0) 
term(496) = term(496) * (1.9999999999999998d+0) 
term(497) = term(497) * (-3.9999999999999996d+0) 

do j = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(498) = term(498) + r2(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,i,c,j,p,q) * wm_interm_5_pt2(c,b)
term(499) = term(499) + r2(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,i,c,j,p,q) * wm_interm_7_pt2(c,b)
term(500) = term(500) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,i,j) * wm_interm_11_pt2(a,c)
term(501) = term(501) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,i,j) * wm_interm_12_pt2(a,c)
term(502) = term(502) + r3(vrdav_Rl, a,i,b,j,p,q) * r2(vrdav_Rr, a,i,c,j) * wm_interm_5_pt2(c,b)
term(503) = term(503) + r3(vrdav_Rl, a,i,b,j,p,q) * r2(vrdav_Rr, a,i,c,j) * wm_interm_7_pt2(c,b)
term(504) = term(504) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(a,c,i,j) * wm_interm_54_pt2(b,c)
term(505) = term(505) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(a,c,i,j) * wm_interm_55_pt2(b,c)
term(506) = term(506) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(b,c,i,j) * wm_interm_54_pt2(a,c)
term(507) = term(507) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(b,c,i,j) * wm_interm_55_pt2(a,c)
end do 
end do 
end do 
end do 
end do 

term(498) = term(498) * (3.9999999999999996d+0) 
term(499) = term(499) * (-7.999999999999999d+0) 
term(500) = term(500) * (8.0d+0) 
term(501) = term(501) * (-4.0d+0) 
term(502) = term(502) * (4.0d+0) 
term(503) = term(503) * (-8.0d+0) 
term(504) = term(504) * (4.0d+0) 
term(505) = term(505) * (-8.0d+0) 
term(506) = term(506) * (-2.0d+0) 
term(507) = term(507) * (4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(508) = term(508) + r2(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,j,c,i,p,q) * wm_interm_5_pt2(c,b)
term(509) = term(509) + r2(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,j,c,i,p,q) * wm_interm_7_pt2(c,b)
term(510) = term(510) + r2(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, b,j,c,i,p,q) * wm_interm_5_pt2(c,a)
term(511) = term(511) + r2(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, b,j,c,i,p,q) * wm_interm_7_pt2(c,a)
term(512) = term(512) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,j,i) * wm_interm_11_pt2(a,c)
term(513) = term(513) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,j,i) * wm_interm_12_pt2(a,c)
term(514) = term(514) + r3(vrdav_Rl, a,i,b,j,p,q) * r2(vrdav_Rr, a,j,c,i) * wm_interm_5_pt2(c,b)
term(515) = term(515) + r3(vrdav_Rl, a,i,b,j,p,q) * r2(vrdav_Rr, a,j,c,i) * wm_interm_7_pt2(c,b)
term(516) = term(516) + r3(vrdav_Rl, a,i,b,j,p,q) * r2(vrdav_Rr, b,j,c,i) * wm_interm_5_pt2(c,a)
term(517) = term(517) + r3(vrdav_Rl, a,i,b,j,p,q) * r2(vrdav_Rr, b,j,c,i) * wm_interm_7_pt2(c,a)
term(518) = term(518) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(b,c,j,i) * wm_interm_54_pt2(a,c)
term(519) = term(519) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(b,c,j,i) * wm_interm_55_pt2(a,c)
term(520) = term(520) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(a,c,j,i) * wm_interm_54_pt2(b,c)
term(521) = term(521) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(a,c,j,i) * wm_interm_55_pt2(b,c)
end do 
end do 
end do 
end do 
end do 

term(508) = term(508) * (-1.9999999999999998d+0) 
term(509) = term(509) * (3.9999999999999996d+0) 
term(510) = term(510) * (3.9999999999999996d+0) 
term(511) = term(511) * (-7.999999999999999d+0) 
term(512) = term(512) * (-16.0d+0) 
term(513) = term(513) * (8.0d+0) 
term(514) = term(514) * (-2.0d+0) 
term(515) = term(515) * (4.0d+0) 
term(516) = term(516) * (4.0d+0) 
term(517) = term(517) * (-8.0d+0) 
term(518) = term(518) * (4.0d+0) 
term(519) = term(519) * (-8.0d+0) 
term(520) = term(520) * (-2.0d+0) 
term(521) = term(521) * (4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(522) = term(522) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,k,i) * wm_interm_6_pt2(a,c,j,k)
term(523) = term(523) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,k,i) * wm_interm_8_pt2(a,c,j,k)
term(524) = term(524) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,k,i) * wm_interm_10_pt2(a,c,j,k)
term(525) = term(525) + r3(vrdav_Rr, a,i,b,j,p,q) * s2(b,c,k,i) * wm_interm_15_pt2(a,c,j,k)
term(526) = term(526) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(b,c,k,i) * wm_interm_52_pt2(a,c,j,k)
term(527) = term(527) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(b,c,k,i) * wm_interm_53_pt2(a,c,j,k)
term(528) = term(528) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(b,c,k,i) * wm_interm_51_pt2(a,c,j,k)
term(529) = term(529) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(b,c,k,i) * wm_interm_50_pt2(a,c,j,k)
term(530) = term(530) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(a,c,k,i) * wm_interm_50_pt2(b,c,j,k)
term(531) = term(531) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(a,c,k,i) * wm_interm_51_pt2(b,c,j,k)
term(532) = term(532) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(a,c,k,i) * wm_interm_52_pt2(b,c,j,k)
term(533) = term(533) + r3(vrdav_Rl, a,i,b,j,p,q) * t2(a,c,k,i) * wm_interm_53_pt2(b,c,j,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(522) = term(522) * (7.999999999999999d+0) 
term(523) = term(523) * (-3.9999999999999996d+0) 
term(524) = term(524) * (7.999999999999999d+0) 
term(525) = term(525) * (-3.9999999999999996d+0) 
term(526) = term(526) * (-2.0d+0) 
term(527) = term(527) * (4.0d+0) 
term(528) = term(528) * (-2.0d+0) 
term(529) = term(529) * (4.0d+0) 
term(530) = term(530) * (-2.0d+0) 
term(531) = term(531) * (4.0d+0) 
term(532) = term(532) * (4.0d+0) 
term(533) = term(533) * (-8.0d+0) 

do j = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(534) = term(534) + r2(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, b,i,c,j,p,q) * wm_interm_5_pt2(c,a)
term(535) = term(535) + r2(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, b,i,c,j,p,q) * wm_interm_7_pt2(c,a)
term(536) = term(536) + r3(vrdav_Rl, a,i,b,j,p,q) * r2(vrdav_Rr, b,i,c,j) * wm_interm_5_pt2(c,a)
term(537) = term(537) + r3(vrdav_Rl, a,i,b,j,p,q) * r2(vrdav_Rr, b,i,c,j) * wm_interm_7_pt2(c,a)
end do 
end do 
end do 
end do 
end do 

term(534) = term(534) * (-1.9999999999999998d+0) 
term(535) = term(535) * (3.9999999999999996d+0) 
term(536) = term(536) * (-2.0d+0) 
term(537) = term(537) * (4.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(538) = term(538) + wm_interm_28_pt2(a,i) * wm_interm_8_pt2(a,p,i,q)
term(539) = term(539) + wm_interm_29_pt2(a,i) * wm_interm_8_pt2(a,p,i,q)
term(540) = term(540) + wm_interm_28_pt2(a,i) * wm_interm_6_pt2(a,p,i,q)
term(541) = term(541) + wm_interm_29_pt2(a,i) * wm_interm_6_pt2(a,p,i,q)
term(542) = term(542) + wm_interm_30_pt2(a,i) * wm_interm_8_pt2(a,p,i,q)
term(543) = term(543) + wm_interm_30_pt2(a,i) * wm_interm_6_pt2(a,p,i,q)
term(544) = term(544) + wm_interm_31_pt2(a,i) * wm_interm_8_pt2(a,p,i,q)
term(545) = term(545) + wm_interm_31_pt2(a,i) * wm_interm_6_pt2(a,p,i,q)
term(546) = term(546) + wm_interm_10_pt2(a,p,i,q) * wm_interm_28_pt2(a,i)
term(547) = term(547) + wm_interm_10_pt2(a,p,i,q) * wm_interm_29_pt2(a,i)
term(548) = term(548) + wm_interm_15_pt2(a,p,i,q) * wm_interm_28_pt2(a,i)
term(549) = term(549) + wm_interm_15_pt2(a,p,i,q) * wm_interm_29_pt2(a,i)
term(550) = term(550) + wm_interm_10_pt2(a,p,i,q) * wm_interm_30_pt2(a,i)
term(551) = term(551) + wm_interm_15_pt2(a,p,i,q) * wm_interm_30_pt2(a,i)
term(552) = term(552) + wm_interm_10_pt2(a,p,i,q) * wm_interm_31_pt2(a,i)
term(553) = term(553) + wm_interm_15_pt2(a,p,i,q) * wm_interm_31_pt2(a,i)
term(554) = term(554) + wm_interm_32_pt2(a,i) * wm_interm_36_pt2(a,p,i,q)
term(555) = term(555) + wm_interm_33_pt2(a,i) * wm_interm_36_pt2(a,p,i,q)
term(556) = term(556) + wm_interm_34_pt2(a,i) * wm_interm_36_pt2(a,p,i,q)
term(557) = term(557) + wm_interm_35_pt2(a,i) * wm_interm_36_pt2(a,p,i,q)
term(558) = term(558) + wm_interm_32_pt2(a,i) * wm_interm_37_pt2(a,p,i,q)
term(559) = term(559) + wm_interm_33_pt2(a,i) * wm_interm_37_pt2(a,p,i,q)
term(560) = term(560) + wm_interm_34_pt2(a,i) * wm_interm_37_pt2(a,p,i,q)
term(561) = term(561) + wm_interm_35_pt2(a,i) * wm_interm_37_pt2(a,p,i,q)
term(562) = term(562) + wm_interm_32_pt2(a,i) * wm_interm_39_pt2(a,p,i,q)
term(563) = term(563) + wm_interm_33_pt2(a,i) * wm_interm_39_pt2(a,p,i,q)
term(564) = term(564) + wm_interm_34_pt2(a,i) * wm_interm_39_pt2(a,p,i,q)
term(565) = term(565) + wm_interm_35_pt2(a,i) * wm_interm_39_pt2(a,p,i,q)
term(566) = term(566) + wm_interm_32_pt2(a,i) * wm_interm_38_pt2(a,p,i,q)
term(567) = term(567) + wm_interm_33_pt2(a,i) * wm_interm_38_pt2(a,p,i,q)
term(568) = term(568) + wm_interm_34_pt2(a,i) * wm_interm_38_pt2(a,p,i,q)
term(569) = term(569) + wm_interm_35_pt2(a,i) * wm_interm_38_pt2(a,p,i,q)
term(570) = term(570) + wm_interm_36_pt2(p,a,q,i) * wm_interm_48_pt2(a,i)
term(571) = term(571) + wm_interm_37_pt2(p,a,q,i) * wm_interm_48_pt2(a,i)
term(572) = term(572) + wm_interm_36_pt2(p,a,q,i) * wm_interm_49_pt2(a,i)
term(573) = term(573) + wm_interm_37_pt2(p,a,q,i) * wm_interm_49_pt2(a,i)
term(574) = term(574) + wm_interm_36_pt2(p,a,q,i) * wm_interm_42_pt2(a,i)
term(575) = term(575) + wm_interm_37_pt2(p,a,q,i) * wm_interm_42_pt2(a,i)
term(576) = term(576) + wm_interm_36_pt2(p,a,q,i) * wm_interm_43_pt2(a,i)
term(577) = term(577) + wm_interm_37_pt2(p,a,q,i) * wm_interm_43_pt2(a,i)
term(578) = term(578) + wm_interm_38_pt2(p,a,q,i) * wm_interm_42_pt2(a,i)
term(579) = term(579) + wm_interm_38_pt2(p,a,q,i) * wm_interm_43_pt2(a,i)
term(580) = term(580) + wm_interm_39_pt2(p,a,q,i) * wm_interm_42_pt2(a,i)
term(581) = term(581) + wm_interm_39_pt2(p,a,q,i) * wm_interm_43_pt2(a,i)
term(582) = term(582) + wm_interm_39_pt2(p,a,q,i) * wm_interm_48_pt2(a,i)
term(583) = term(583) + wm_interm_38_pt2(p,a,q,i) * wm_interm_48_pt2(a,i)
term(584) = term(584) + wm_interm_39_pt2(p,a,q,i) * wm_interm_49_pt2(a,i)
term(585) = term(585) + wm_interm_38_pt2(p,a,q,i) * wm_interm_49_pt2(a,i)
term(586) = term(586) + wm_interm_50_pt2(a,p,i,q) * wm_interm_71_pt2(a,i)
term(587) = term(587) + wm_interm_51_pt2(a,p,i,q) * wm_interm_71_pt2(a,i)
term(588) = term(588) + wm_interm_50_pt2(a,p,i,q) * wm_interm_72_pt2(a,i)
term(589) = term(589) + wm_interm_51_pt2(a,p,i,q) * wm_interm_72_pt2(a,i)
term(590) = term(590) + wm_interm_52_pt2(a,p,i,q) * wm_interm_71_pt2(a,i)
term(591) = term(591) + wm_interm_53_pt2(a,p,i,q) * wm_interm_71_pt2(a,i)
term(592) = term(592) + wm_interm_52_pt2(a,p,i,q) * wm_interm_72_pt2(a,i)
term(593) = term(593) + wm_interm_53_pt2(a,p,i,q) * wm_interm_72_pt2(a,i)
term(594) = term(594) + wm_interm_50_pt2(a,p,i,q) * wm_interm_73_pt2(a,i)
term(595) = term(595) + wm_interm_51_pt2(a,p,i,q) * wm_interm_73_pt2(a,i)
term(596) = term(596) + wm_interm_50_pt2(a,p,i,q) * wm_interm_74_pt2(a,i)
term(597) = term(597) + wm_interm_51_pt2(a,p,i,q) * wm_interm_74_pt2(a,i)
term(598) = term(598) + wm_interm_52_pt2(a,p,i,q) * wm_interm_73_pt2(a,i)
term(599) = term(599) + wm_interm_53_pt2(a,p,i,q) * wm_interm_73_pt2(a,i)
term(600) = term(600) + wm_interm_52_pt2(a,p,i,q) * wm_interm_74_pt2(a,i)
term(601) = term(601) + wm_interm_53_pt2(a,p,i,q) * wm_interm_74_pt2(a,i)
end do 
end do 

term(538) = term(538) * (-8.0d+0) 
term(539) = term(539) * (7.999999999999999d+0) 
term(540) = term(540) * (16.0d+0) 
term(541) = term(541) * (-15.999999999999998d+0) 
term(542) = term(542) * (4.0d+0) 
term(543) = term(543) * (-8.0d+0) 
term(544) = term(544) * (-4.0d+0) 
term(545) = term(545) * (8.0d+0) 
term(546) = term(546) * (4.0d+0) 
term(547) = term(547) * (-3.9999999999999996d+0) 
term(548) = term(548) * (-8.0d+0) 
term(549) = term(549) * (7.999999999999999d+0) 
term(550) = term(550) * (-2.0d+0) 
term(551) = term(551) * (4.0d+0) 
term(552) = term(552) * (2.0d+0) 
term(553) = term(553) * (-4.0d+0) 
term(554) = term(554) * (-3.9999999999999996d+0) 
term(555) = term(555) * (7.999999999999999d+0) 
term(556) = term(556) * (3.9999999999999996d+0) 
term(557) = term(557) * (-7.999999999999999d+0) 
term(558) = term(558) * (7.999999999999999d+0) 
term(559) = term(559) * (-15.999999999999998d+0) 
term(560) = term(560) * (-7.999999999999999d+0) 
term(561) = term(561) * (15.999999999999998d+0) 
term(562) = term(562) * (1.9999999999999998d+0) 
term(563) = term(563) * (-3.9999999999999996d+0) 
term(564) = term(564) * (-1.9999999999999998d+0) 
term(565) = term(565) * (3.9999999999999996d+0) 
term(566) = term(566) * (-3.9999999999999996d+0) 
term(567) = term(567) * (7.999999999999999d+0) 
term(568) = term(568) * (3.9999999999999996d+0) 
term(569) = term(569) * (-7.999999999999999d+0) 
term(570) = term(570) * (4.0d+0) 
term(571) = term(571) * (-8.0d+0) 
term(572) = term(572) * (-8.0d+0) 
term(573) = term(573) * (16.0d+0) 
term(574) = term(574) * (-3.9999999999999996d+0) 
term(575) = term(575) * (7.999999999999999d+0) 
term(576) = term(576) * (7.999999999999999d+0) 
term(577) = term(577) * (-15.999999999999998d+0) 
term(578) = term(578) * (-3.9999999999999996d+0) 
term(579) = term(579) * (7.999999999999999d+0) 
term(580) = term(580) * (1.9999999999999998d+0) 
term(581) = term(581) * (-3.9999999999999996d+0) 
term(582) = term(582) * (-2.0d+0) 
term(583) = term(583) * (4.0d+0) 
term(584) = term(584) * (4.0d+0) 
term(585) = term(585) * (-8.0d+0) 
term(586) = term(586) * (-3.9999999999999996d+0) 
term(587) = term(587) * (7.999999999999999d+0) 
term(588) = term(588) * (4.0d+0) 
term(589) = term(589) * (-8.0d+0) 
term(590) = term(590) * (7.999999999999999d+0) 
term(591) = term(591) * (-15.999999999999998d+0) 
term(592) = term(592) * (-8.0d+0) 
term(593) = term(593) * (16.0d+0) 
term(594) = term(594) * (1.9999999999999998d+0) 
term(595) = term(595) * (-3.9999999999999996d+0) 
term(596) = term(596) * (-2.0d+0) 
term(597) = term(597) * (4.0d+0) 
term(598) = term(598) * (-3.9999999999999996d+0) 
term(599) = term(599) * (7.999999999999999d+0) 
term(600) = term(600) * (4.0d+0) 
term(601) = term(601) * (-8.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do j = 1, nocc 
term(602) = term(602) + r2(vrdav_Rl, a,j,b,k) * r3(vrdav_Rr, b,j,c,k,p,i) * wm_interm_36_pt2(c,a,i,q)
term(603) = term(603) + r2(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, b,j,c,k,p,i) * wm_interm_36_pt2(c,a,k,q)
term(604) = term(604) + r2(vrdav_Rl, a,j,b,k) * r3(vrdav_Rr, b,j,c,k,p,i) * wm_interm_37_pt2(c,a,i,q)
term(605) = term(605) + r2(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, b,j,c,k,p,i) * wm_interm_37_pt2(c,a,k,q)
term(606) = term(606) + r2(vrdav_Rl, a,j,b,k) * r3(vrdav_Rr, b,j,c,k,p,i) * wm_interm_38_pt2(c,a,i,q)
term(607) = term(607) + r2(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, b,j,c,k,p,i) * wm_interm_38_pt2(c,a,k,q)
term(608) = term(608) + r2(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, b,j,c,k,p,i) * wm_interm_39_pt2(c,a,k,q)
term(609) = term(609) + r2(vrdav_Rl, a,j,b,k) * r3(vrdav_Rr, b,j,c,k,p,i) * wm_interm_39_pt2(c,a,i,q)
end do 
end do 
end do 
end do 
end do 
end do 

term(603) = term(603) * (-1.9999999999999998d+0) 
term(604) = term(604) * (-1.9999999999999998d+0) 
term(605) = term(605) * (3.9999999999999996d+0) 
term(607) = term(607) * (-1.9999999999999998d+0) 
term(609) = term(609) * (-1.9999999999999998d+0) 

do i = 1, nocc 
term(610) = term(610) + wm_interm_16_pt2(i,q) * wm_interm_28_pt2(p,i)
term(611) = term(611) + wm_interm_16_pt2(i,q) * wm_interm_29_pt2(p,i)
term(612) = term(612) + wm_interm_25_pt2(i,q) * wm_interm_28_pt2(p,i)
term(613) = term(613) + wm_interm_25_pt2(i,q) * wm_interm_29_pt2(p,i)
term(614) = term(614) + wm_interm_16_pt2(i,q) * wm_interm_30_pt2(p,i)
term(615) = term(615) + wm_interm_25_pt2(i,q) * wm_interm_30_pt2(p,i)
term(616) = term(616) + wm_interm_16_pt2(i,q) * wm_interm_31_pt2(p,i)
term(617) = term(617) + wm_interm_25_pt2(i,q) * wm_interm_31_pt2(p,i)
term(618) = term(618) + wm_interm_1_pt2(i,q) * wm_interm_32_pt2(p,i)
term(619) = term(619) + wm_interm_1_pt2(i,q) * wm_interm_33_pt2(p,i)
term(620) = term(620) + wm_interm_1_pt2(i,q) * wm_interm_34_pt2(p,i)
term(621) = term(621) + wm_interm_1_pt2(i,q) * wm_interm_35_pt2(p,i)
term(622) = term(622) + wm_interm_2_pt2(i,q) * wm_interm_32_pt2(p,i)
term(623) = term(623) + wm_interm_2_pt2(i,q) * wm_interm_33_pt2(p,i)
term(624) = term(624) + wm_interm_2_pt2(i,q) * wm_interm_34_pt2(p,i)
term(625) = term(625) + wm_interm_2_pt2(i,q) * wm_interm_35_pt2(p,i)
end do 

term(610) = term(610) * (4.0d+0) 
term(611) = term(611) * (-3.9999999999999996d+0) 
term(612) = term(612) * (-8.0d+0) 
term(613) = term(613) * (7.999999999999999d+0) 
term(614) = term(614) * (-2.0d+0) 
term(615) = term(615) * (4.0d+0) 
term(616) = term(616) * (2.0d+0) 
term(617) = term(617) * (-4.0d+0) 
term(618) = term(618) * (1.9999999999999998d+0) 
term(619) = term(619) * (-3.9999999999999996d+0) 
term(620) = term(620) * (-1.9999999999999998d+0) 
term(621) = term(621) * (3.9999999999999996d+0) 
term(622) = term(622) * (-3.9999999999999996d+0) 
term(623) = term(623) * (7.999999999999999d+0) 
term(624) = term(624) * (3.9999999999999996d+0) 
term(625) = term(625) * (-7.999999999999999d+0) 

do i = 1, nocc 
do k = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(626) = term(626) + r2(vrdav_Rl, a,j,b,k) * r3(vrdav_Rr, a,j,c,k,p,i) * wm_interm_36_pt2(c,b,i,q)
term(627) = term(627) + r2(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,j,c,k,p,i) * wm_interm_36_pt2(c,b,k,q)
term(628) = term(628) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,i,k) * wm_interm_15_pt2(a,c,j,q)
term(629) = term(629) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,i,k) * wm_interm_10_pt2(a,c,j,q)
term(630) = term(630) + r2(vrdav_Rl, a,j,b,k) * r3(vrdav_Rr, a,j,c,k,p,i) * wm_interm_37_pt2(c,b,i,q)
term(631) = term(631) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,j,k) * wm_interm_15_pt2(a,c,i,q)
term(632) = term(632) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,j,i) * wm_interm_15_pt2(a,c,k,q)
term(633) = term(633) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,k,i) * wm_interm_15_pt2(a,c,j,q)
term(634) = term(634) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,j,i) * wm_interm_10_pt2(a,c,k,q)
term(635) = term(635) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,k,i) * wm_interm_10_pt2(a,c,j,q)
term(636) = term(636) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,j,k) * wm_interm_10_pt2(a,c,i,q)
term(637) = term(637) + r2(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,j,c,k,p,i) * wm_interm_37_pt2(c,b,k,q)
term(638) = term(638) + r2(vrdav_Rl, a,j,b,k) * r3(vrdav_Rr, a,j,c,k,p,i) * wm_interm_38_pt2(c,b,i,q)
term(639) = term(639) + r2(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,j,c,k,p,i) * wm_interm_38_pt2(c,b,k,q)
term(640) = term(640) + r2(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,j,c,k,p,i) * wm_interm_39_pt2(c,b,k,q)
term(641) = term(641) + r2(vrdav_Rl, a,j,b,k) * r3(vrdav_Rr, a,j,c,k,p,i) * wm_interm_39_pt2(c,b,i,q)
term(642) = term(642) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,i,k) * wm_interm_8_pt2(a,c,j,q)
term(643) = term(643) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,i,k) * wm_interm_6_pt2(a,c,j,q)
term(644) = term(644) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,j,k) * wm_interm_8_pt2(a,c,i,q)
term(645) = term(645) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,j,i) * wm_interm_8_pt2(a,c,k,q)
term(646) = term(646) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,k,i) * wm_interm_8_pt2(a,c,j,q)
term(647) = term(647) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,j,k) * wm_interm_6_pt2(a,c,i,q)
term(648) = term(648) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,j,i) * wm_interm_6_pt2(a,c,k,q)
term(649) = term(649) + r3(vrdav_Rr, a,j,b,k,p,i) * s2(b,c,k,i) * wm_interm_6_pt2(a,c,j,q)
end do 
end do 
end do 
end do 
end do 
end do 

term(626) = term(626) * (-1.9999999999999998d+0) 
term(627) = term(627) * (3.9999999999999996d+0) 
term(628) = term(628) * (-4.0d+0) 
term(629) = term(629) * (2.0d+0) 
term(630) = term(630) * (3.9999999999999996d+0) 
term(631) = term(631) * (1.9999999999999998d+0) 
term(632) = term(632) * (-3.9999999999999996d+0) 
term(633) = term(633) * (8.0d+0) 
term(634) = term(634) * (1.9999999999999998d+0) 
term(635) = term(635) * (-4.0d+0) 
term(636) = term(636) * (-3.9999999999999996d+0) 
term(637) = term(637) * (-7.999999999999999d+0) 
term(638) = term(638) * (-1.9999999999999998d+0) 
term(639) = term(639) * (3.9999999999999996d+0) 
term(640) = term(640) * (-1.9999999999999998d+0) 
term(641) = term(641) * (3.9999999999999996d+0) 
term(642) = term(642) * (-4.0d+0) 
term(643) = term(643) * (8.0d+0) 
term(644) = term(644) * (1.9999999999999998d+0) 
term(645) = term(645) * (-3.9999999999999996d+0) 
term(646) = term(646) * (8.0d+0) 
term(647) = term(647) * (-3.9999999999999996d+0) 
term(648) = term(648) * (7.999999999999999d+0) 
term(649) = term(649) * (-16.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(650) = term(650) + r1(vrdav_Rl, a,i) * r3(vrdav_Rr, a,q,b,j,p,i) * s1(b,j)
term(651) = term(651) + r1(vrdav_Rl, b,i) * r3(vrdav_Rr, a,q,b,j,p,i) * s1(a,j)
term(652) = term(652) + r1(vrdav_Rl, a,j) * r3(vrdav_Rr, a,q,b,j,p,i) * s1(b,i)
term(653) = term(653) + r1(vrdav_Rl, b,j) * r3(vrdav_Rr, a,q,b,j,p,i) * s1(a,i)
term(654) = term(654) + r3(vrdav_Rl, a,q,b,j,p,i) * r1(vrdav_Rr, a,i) * t1(b,j)
term(655) = term(655) + r3(vrdav_Rl, a,q,b,j,p,i) * r1(vrdav_Rr, b,i) * t1(a,j)
term(656) = term(656) + r3(vrdav_Rl, a,q,b,j,p,i) * r1(vrdav_Rr, a,j) * t1(b,i)
term(657) = term(657) + r3(vrdav_Rl, a,q,b,j,p,i) * r1(vrdav_Rr, b,j) * t1(a,i)
end do 
end do 
end do 
end do 

term(650) = term(650) * (-4.0d+0) 
term(651) = term(651) * (2.0d+0) 
term(652) = term(652) * (2.0d+0) 
term(653) = term(653) * (-4.0d+0) 
term(654) = term(654) * (3.9999999999999996d+0) 
term(655) = term(655) * (-1.9999999999999998d+0) 
term(656) = term(656) * (-1.9999999999999998d+0) 
term(657) = term(657) * (3.9999999999999996d+0) 

do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(658) = term(658) + r1(vrdav_Rl, a,i) * r3(vrdav_Rr, a,i,b,j,p,q) * s1(b,j)
term(659) = term(659) + r3(vrdav_Rl, a,i,b,j,p,q) * r1(vrdav_Rr, a,i) * t1(b,j)
term(660) = term(660) + r3(vrdav_Rl, a,i,b,j,p,q) * r1(vrdav_Rr, a,j) * t1(b,i)
term(661) = term(661) + r3(vrdav_Rl, a,i,b,j,p,q) * r1(vrdav_Rr, b,i) * t1(a,j)
term(662) = term(662) + r3(vrdav_Rl, a,i,b,j,p,q) * r1(vrdav_Rr, b,j) * t1(a,i)
end do 
end do 
end do 
end do 

term(658) = term(658) * (8.0d+0) 
term(659) = term(659) * (-4.0d+0) 
term(660) = term(660) * (2.0d+0) 
term(661) = term(661) * (2.0d+0) 
term(662) = term(662) * (-4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(663) = term(663) + s2(a,b,k,j) * t2(a,p,q,i) * wm_interm_3_pt2(b,i,j,k)
term(664) = term(664) + s2(a,b,k,j) * t2(a,p,q,i) * wm_interm_4_pt2(b,i,j,k)
term(665) = term(665) + s2(a,b,k,j) * t2(a,p,q,i) * wm_interm_9_pt2(b,i,j,k)
term(666) = term(666) + s2(a,b,k,j) * t2(a,p,q,i) * wm_interm_0_pt2(b,i,j,k)
end do 
end do 
end do 
end do 
end do 

term(663) = term(663) * (1.9999999999999998d+0) 
term(664) = term(664) * (-3.9999999999999996d+0) 
term(666) = term(666) * (-1.9999999999999998d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(667) = term(667) + r1(vrdav_Rl, a,i) * r3(vrdav_Rr, a,j,b,i,p,q) * s1(b,j)
end do 
end do 
end do 
end do 

term(667) = term(667) * (-4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(668) = term(668) + s2(a,b,i,j) * t2(b,p,k,q) * wm_interm_9_pt2(a,k,j,i)
term(669) = term(669) + s2(a,b,i,j) * t2(b,p,k,q) * wm_interm_3_pt2(a,k,j,i)
term(670) = term(670) + s2(a,b,i,j) * t2(b,p,k,q) * wm_interm_4_pt2(a,k,j,i)
term(671) = term(671) + s2(a,b,i,j) * t2(b,p,k,q) * wm_interm_0_pt2(a,k,j,i)
term(672) = term(672) + s2(a,b,k,j) * t2(a,p,q,i) * wm_interm_9_pt2(b,i,k,j)
term(673) = term(673) + s2(a,b,k,j) * t2(a,p,q,i) * wm_interm_3_pt2(b,i,k,j)
term(674) = term(674) + s2(a,b,k,j) * t2(a,p,q,i) * wm_interm_4_pt2(b,i,k,j)
term(675) = term(675) + s2(a,b,k,j) * t2(a,p,q,i) * wm_interm_0_pt2(b,i,k,j)
term(676) = term(676) + r2(vrdav_Rl, a,k,b,j) * t2(a,p,q,i) * wm_interm_17_pt2(b,k,j,i)
term(677) = term(677) + r2(vrdav_Rl, a,k,b,j) * t2(a,p,q,i) * wm_interm_17_pt2(b,j,k,i)
term(678) = term(678) + r2(vrdav_Rl, a,k,b,j) * t2(a,p,q,i) * wm_interm_20_pt2(b,j,k,i)
term(679) = term(679) + r2(vrdav_Rl, a,k,b,j) * t2(a,p,q,i) * wm_interm_18_pt2(b,k,j,i)
term(680) = term(680) + r2(vrdav_Rl, a,k,b,j) * t2(a,p,q,i) * wm_interm_19_pt2(b,j,k,i)
term(681) = term(681) + r2(vrdav_Rl, a,k,b,j) * t2(a,p,q,i) * wm_interm_18_pt2(b,j,k,i)
term(682) = term(682) + s2(a,p,q,i) * t2(a,b,k,j) * wm_interm_40_pt2(b,j,k,i)
term(683) = term(683) + s2(a,p,q,i) * t2(a,b,k,j) * wm_interm_41_pt2(b,j,k,i)
term(684) = term(684) + s2(a,p,i,q) * t2(a,b,k,j) * wm_interm_40_pt2(b,j,k,i)
term(685) = term(685) + s2(a,p,i,q) * t2(a,b,k,j) * wm_interm_41_pt2(b,j,k,i)
term(686) = term(686) + s2(a,p,q,i) * t2(a,b,k,j) * wm_interm_44_pt2(b,j,k,i)
term(687) = term(687) + s2(a,p,q,i) * t2(a,b,k,j) * wm_interm_45_pt2(b,j,k,i)
term(688) = term(688) + s2(a,p,i,q) * t2(a,b,k,j) * wm_interm_44_pt2(b,j,k,i)
term(689) = term(689) + s2(a,p,i,q) * t2(a,b,k,j) * wm_interm_45_pt2(b,j,k,i)
term(690) = term(690) + s2(a,p,q,i) * t2(a,b,k,j) * wm_interm_46_pt2(b,j,k,i)
term(691) = term(691) + s2(a,p,i,q) * t2(a,b,k,j) * wm_interm_46_pt2(b,j,k,i)
term(692) = term(692) + s2(a,p,q,i) * t2(a,b,k,j) * wm_interm_47_pt2(b,j,k,i)
term(693) = term(693) + s2(a,p,i,q) * t2(a,b,k,j) * wm_interm_47_pt2(b,j,k,i)
term(694) = term(694) + s2(a,p,q,i) * t2(a,b,k,j) * wm_interm_46_pt2(b,k,j,i)
term(695) = term(695) + s2(a,p,q,i) * t2(a,b,k,j) * wm_interm_47_pt2(b,k,j,i)
term(696) = term(696) + s2(a,p,i,q) * t2(a,b,k,j) * wm_interm_46_pt2(b,k,j,i)
term(697) = term(697) + s2(a,p,i,q) * t2(a,b,k,j) * wm_interm_47_pt2(b,k,j,i)
term(698) = term(698) + s2(a,p,q,i) * t2(a,b,k,j) * wm_interm_41_pt2(b,k,j,i)
term(699) = term(699) + s2(a,p,q,i) * t2(a,b,k,j) * wm_interm_40_pt2(b,k,j,i)
term(700) = term(700) + s2(a,p,i,q) * t2(a,b,k,j) * wm_interm_41_pt2(b,k,j,i)
term(701) = term(701) + s2(a,p,i,q) * t2(a,b,k,j) * wm_interm_40_pt2(b,k,j,i)
term(702) = term(702) + s2(a,p,q,i) * t2(a,b,k,j) * wm_interm_44_pt2(b,k,j,i)
term(703) = term(703) + s2(a,p,q,i) * t2(a,b,k,j) * wm_interm_45_pt2(b,k,j,i)
term(704) = term(704) + s2(a,p,i,q) * t2(a,b,k,j) * wm_interm_44_pt2(b,k,j,i)
term(705) = term(705) + s2(a,p,i,q) * t2(a,b,k,j) * wm_interm_45_pt2(b,k,j,i)
end do 
end do 
end do 
end do 
end do 

term(668) = term(668) * (3.9999999999999996d+0) 
term(669) = term(669) * (7.999999999999999d+0) 
term(670) = term(670) * (-15.999999999999998d+0) 
term(671) = term(671) * (-1.9999999999999998d+0) 
term(672) = term(672) * (-1.9999999999999998d+0) 
term(673) = term(673) * (-3.9999999999999996d+0) 
term(674) = term(674) * (7.999999999999999d+0) 
term(677) = term(677) * (-1.9999999999999998d+0) 
term(679) = term(679) * (-1.9999999999999998d+0) 
term(680) = term(680) * (-1.9999999999999998d+0) 
term(681) = term(681) * (3.9999999999999996d+0) 
term(683) = term(683) * (-2.0d+0) 
term(684) = term(684) * (-2.0d+0) 
term(685) = term(685) * (4.0d+0) 
term(686) = term(686) * (-2.0d+0) 
term(687) = term(687) * (4.0d+0) 
term(688) = term(688) * (4.0d+0) 
term(689) = term(689) * (-8.0d+0) 
term(691) = term(691) * (-2.0d+0) 
term(692) = term(692) * (-2.0d+0) 
term(693) = term(693) * (4.0d+0) 
term(694) = term(694) * (-2.0d+0) 
term(695) = term(695) * (4.0d+0) 
term(696) = term(696) * (4.0d+0) 
term(697) = term(697) * (-8.0d+0) 
term(699) = term(699) * (-2.0d+0) 
term(700) = term(700) * (-2.0d+0) 
term(701) = term(701) * (4.0d+0) 
term(703) = term(703) * (-2.0d+0) 
term(704) = term(704) * (-2.0d+0) 
term(705) = term(705) * (4.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do c = nocc + 1, nactive 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(706) = term(706) + r2(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, b,k,c,j,p,i) * wm_interm_36_pt2(c,a,k,q)
term(707) = term(707) + r2(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, b,k,c,j,p,i) * wm_interm_36_pt2(c,a,k,q)
term(708) = term(708) + r2(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,k,c,j,p,i) * wm_interm_36_pt2(c,b,k,q)
term(709) = term(709) + r2(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,k,c,j,p,i) * wm_interm_36_pt2(c,b,k,q)
term(710) = term(710) + r2(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, b,k,c,j,p,i) * wm_interm_37_pt2(c,a,k,q)
term(711) = term(711) + r2(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, b,k,c,j,p,i) * wm_interm_37_pt2(c,a,k,q)
term(712) = term(712) + r2(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,k,c,j,p,i) * wm_interm_37_pt2(c,b,k,q)
term(713) = term(713) + r2(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,k,c,j,p,i) * wm_interm_37_pt2(c,b,k,q)
term(714) = term(714) + r2(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, b,k,c,j,p,i) * wm_interm_38_pt2(c,a,k,q)
term(715) = term(715) + r2(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, b,k,c,j,p,i) * wm_interm_38_pt2(c,a,k,q)
term(716) = term(716) + r2(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,k,c,j,p,i) * wm_interm_38_pt2(c,b,k,q)
term(717) = term(717) + r2(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,k,c,j,p,i) * wm_interm_38_pt2(c,b,k,q)
term(718) = term(718) + r2(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, b,k,c,j,p,i) * wm_interm_39_pt2(c,a,k,q)
term(719) = term(719) + r2(vrdav_Rl, a,i,b,j) * r3(vrdav_Rr, a,k,c,j,p,i) * wm_interm_39_pt2(c,b,k,q)
term(720) = term(720) + r2(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, b,k,c,j,p,i) * wm_interm_39_pt2(c,a,k,q)
term(721) = term(721) + r2(vrdav_Rl, a,j,b,i) * r3(vrdav_Rr, a,k,c,j,p,i) * wm_interm_39_pt2(c,b,k,q)
end do 
end do 
end do 
end do 
end do 
end do 

term(707) = term(707) * (-1.9999999999999998d+0) 
term(709) = term(709) * (-1.9999999999999998d+0) 
term(710) = term(710) * (-1.9999999999999998d+0) 
term(711) = term(711) * (3.9999999999999996d+0) 
term(712) = term(712) * (-1.9999999999999998d+0) 
term(713) = term(713) * (3.9999999999999996d+0) 
term(715) = term(715) * (-1.9999999999999998d+0) 
term(717) = term(717) * (-1.9999999999999998d+0) 
term(719) = term(719) * (-1.9999999999999998d+0) 
term(720) = term(720) * (-1.9999999999999998d+0) 

do k = 1, nocc 
do l = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(722) = term(722) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_64_pt2(a,i,q,j,l,k)
term(723) = term(723) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_64_pt2(a,q,i,j,l,k)
term(724) = term(724) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_64_pt2(a,i,j,q,l,k)
term(725) = term(725) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_64_pt2(a,j,i,q,l,k)
term(726) = term(726) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_67_pt2(a,q,i,j,l,k)
term(727) = term(727) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_67_pt2(a,i,q,j,l,k)
term(728) = term(728) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_67_pt2(a,j,i,q,l,k)
term(729) = term(729) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_67_pt2(a,i,j,q,l,k)
term(730) = term(730) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_70_pt2(a,i,j,q,l,k)
term(731) = term(731) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_70_pt2(a,j,i,q,l,k)
term(732) = term(732) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_70_pt2(a,q,i,j,l,k)
term(733) = term(733) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_70_pt2(a,i,q,j,l,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(722) = term(722) * (0.3333333333333333d+0) 
term(723) = term(723) * (-0.6666666666666666d+0) 
term(724) = term(724) * (-0.6666666666666666d+0) 
term(725) = term(725) * (1.3333333333333333d+0) 
term(726) = term(726) * (0.3333333333333333d+0) 
term(727) = term(727) * (-0.6666666666666666d+0) 
term(728) = term(728) * (-0.6666666666666666d+0) 
term(729) = term(729) * (1.3333333333333333d+0) 
term(730) = term(730) * (0.3333333333333333d+0) 
term(731) = term(731) * (-0.6666666666666666d+0) 
term(732) = term(732) * (0.3333333333333333d+0) 
term(733) = term(733) * (-0.6666666666666666d+0) 

do k = 1, nocc 
do l = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(734) = term(734) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_64_pt2(a,j,q,i,l,k)
term(735) = term(735) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_64_pt2(a,q,j,i,l,k)
term(736) = term(736) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_67_pt2(a,q,j,i,l,k)
term(737) = term(737) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_67_pt2(a,j,q,i,l,k)
term(738) = term(738) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_70_pt2(a,q,j,i,l,k)
term(739) = term(739) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_70_pt2(a,j,q,i,l,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(734) = term(734) * (-0.6666666666666666d+0) 
term(735) = term(735) * (0.3333333333333333d+0) 
term(736) = term(736) * (-0.6666666666666666d+0) 
term(737) = term(737) * (0.3333333333333333d+0) 
term(738) = term(738) * (-0.6666666666666666d+0) 
term(739) = term(739) * (1.3333333333333333d+0) 

do l = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(740) = term(740) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_64_pt2(a,j,q,i,k,l)
term(741) = term(741) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_64_pt2(a,q,j,i,k,l)
term(742) = term(742) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_67_pt2(a,q,j,i,k,l)
term(743) = term(743) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_67_pt2(a,j,q,i,k,l)
term(744) = term(744) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_70_pt2(a,j,q,i,k,l)
term(745) = term(745) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_70_pt2(a,q,j,i,k,l)
end do 
end do 
end do 
end do 
end do 
end do 

term(740) = term(740) * (0.3333333333333333d+0) 
term(741) = term(741) * (-0.6666666666666666d+0) 
term(742) = term(742) * (0.3333333333333333d+0) 
term(743) = term(743) * (-0.6666666666666666d+0) 
term(744) = term(744) * (-0.6666666666666666d+0) 
term(745) = term(745) * (1.3333333333333333d+0) 

do l = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(746) = term(746) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_64_pt2(a,i,q,j,k,l)
term(747) = term(747) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_64_pt2(a,i,j,q,k,l)
term(748) = term(748) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_64_pt2(a,j,i,q,k,l)
term(749) = term(749) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_64_pt2(a,q,i,j,k,l)
term(750) = term(750) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_67_pt2(a,q,i,j,k,l)
term(751) = term(751) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_67_pt2(a,j,i,q,k,l)
term(752) = term(752) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_67_pt2(a,i,j,q,k,l)
term(753) = term(753) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_67_pt2(a,i,q,j,k,l)
term(754) = term(754) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_70_pt2(a,i,q,j,k,l)
term(755) = term(755) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_70_pt2(a,j,i,q,k,l)
term(756) = term(756) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_70_pt2(a,i,j,q,k,l)
term(757) = term(757) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,l,k) * wm_interm_70_pt2(a,q,i,j,k,l)
end do 
end do 
end do 
end do 
end do 
end do 

term(746) = term(746) * (-0.6666666666666666d+0) 
term(747) = term(747) * (0.3333333333333333d+0) 
term(748) = term(748) * (-0.6666666666666666d+0) 
term(749) = term(749) * (1.3333333333333333d+0) 
term(750) = term(750) * (-0.6666666666666666d+0) 
term(751) = term(751) * (0.3333333333333333d+0) 
term(752) = term(752) * (-0.6666666666666666d+0) 
term(753) = term(753) * (1.3333333333333333d+0) 
term(754) = term(754) * (0.3333333333333333d+0) 
term(755) = term(755) * (0.3333333333333333d+0) 
term(756) = term(756) * (-0.6666666666666666d+0) 
term(757) = term(757) * (-0.6666666666666666d+0) 


    calc_D_vo_wm_cc3_pt2 = zero
    do s = 0, 757
    calc_D_vo_wm_cc3_pt2 = calc_D_vo_wm_cc3_pt2 + term(s)
    end do

    end function calc_D_vo_wm_cc3_pt2
    
    function calc_D_vv_wm_cc3_pt2(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vv_wm_cc3_pt2
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
    real(F64), dimension(0:33) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + r2(vrdav_Rl, a,j,q,i) * wm_interm_75_pt2(a,p,i,j)
term(1) = term(1) + r2(vrdav_Rl, a,j,q,i) * wm_interm_77_pt2(a,p,i,j)
term(2) = term(2) + r2(vrdav_Rl, a,j,q,i) * wm_interm_76_pt2(a,p,i,j)
term(3) = term(3) + r2(vrdav_Rr, a,j,p,i) * wm_interm_78_pt2(q,a,i,j)
term(4) = term(4) + r2(vrdav_Rr, a,j,p,i) * wm_interm_78_pt2(a,q,i,j)
term(5) = term(5) + r2(vrdav_Rr, a,j,p,i) * wm_interm_79_pt2(q,a,i,j)
term(6) = term(6) + r2(vrdav_Rr, a,j,p,i) * wm_interm_79_pt2(a,q,i,j)
term(7) = term(7) + r2(vrdav_Rr, a,j,p,i) * wm_interm_80_pt2(q,a,i,j)
term(8) = term(8) + r2(vrdav_Rr, a,j,p,i) * wm_interm_81_pt2(q,a,i,j)
term(9) = term(9) + r2(vrdav_Rr, a,j,p,i) * wm_interm_81_pt2(a,q,i,j)
term(10) = term(10) + r2(vrdav_Rr, a,j,p,i) * wm_interm_80_pt2(a,q,i,j)
term(11) = term(11) + r2(vrdav_Rr, a,j,p,i) * wm_interm_82_pt2(q,a,i,j)
term(12) = term(12) + r2(vrdav_Rr, a,j,p,i) * wm_interm_82_pt2(a,q,i,j)
end do 
end do 
end do 

term(0) = term(0) * (-4.0d+0) 
term(1) = term(1) * (-4.0d+0) 
term(2) = term(2) * (2.0d+0) 
term(3) = term(3) * (-4.0d+0) 
term(4) = term(4) * (2.0d+0) 
term(5) = term(5) * (1.3333333333333333d+0) 
term(6) = term(6) * (-0.6666666666666666d+0) 
term(7) = term(7) * (1.3333333333333333d+0) 
term(8) = term(8) * (-0.3333333333333333d+0) 
term(9) = term(9) * (0.6666666666666666d+0) 
term(10) = term(10) * (-0.6666666666666666d+0) 
term(11) = term(11) * (-0.3333333333333333d+0) 
term(12) = term(12) * (0.6666666666666666d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(13) = term(13) + r2(vrdav_Rl, a,j,q,i) * wm_interm_75_pt2(a,p,j,i)
term(14) = term(14) + r2(vrdav_Rl, a,j,q,i) * wm_interm_77_pt2(a,p,j,i)
term(15) = term(15) + r2(vrdav_Rl, a,j,q,i) * wm_interm_76_pt2(a,p,j,i)
term(16) = term(16) + r2(vrdav_Rr, a,j,p,i) * wm_interm_78_pt2(q,a,j,i)
term(17) = term(17) + r2(vrdav_Rr, a,j,p,i) * wm_interm_78_pt2(a,q,j,i)
term(18) = term(18) + r2(vrdav_Rr, a,j,p,i) * wm_interm_79_pt2(q,a,j,i)
term(19) = term(19) + r2(vrdav_Rr, a,j,p,i) * wm_interm_79_pt2(a,q,j,i)
term(20) = term(20) + r2(vrdav_Rr, a,j,p,i) * wm_interm_80_pt2(q,a,j,i)
term(21) = term(21) + r2(vrdav_Rr, a,j,p,i) * wm_interm_81_pt2(q,a,j,i)
term(22) = term(22) + r2(vrdav_Rr, a,j,p,i) * wm_interm_81_pt2(a,q,j,i)
term(23) = term(23) + r2(vrdav_Rr, a,j,p,i) * wm_interm_80_pt2(a,q,j,i)
term(24) = term(24) + r2(vrdav_Rr, a,j,p,i) * wm_interm_82_pt2(q,a,j,i)
term(25) = term(25) + r2(vrdav_Rr, a,j,p,i) * wm_interm_82_pt2(a,q,j,i)
end do 
end do 
end do 

term(13) = term(13) * (8.0d+0) 
term(14) = term(14) * (2.0d+0) 
term(15) = term(15) * (-4.0d+0) 
term(16) = term(16) * (2.0d+0) 
term(17) = term(17) * (-4.0d+0) 
term(18) = term(18) * (-0.6666666666666666d+0) 
term(19) = term(19) * (1.3333333333333333d+0) 
term(20) = term(20) * (-0.6666666666666666d+0) 
term(21) = term(21) * (0.6666666666666666d+0) 
term(22) = term(22) * (-0.3333333333333333d+0) 
term(23) = term(23) * (1.3333333333333333d+0) 
term(24) = term(24) * (0.6666666666666666d+0) 
term(25) = term(25) * (-0.3333333333333333d+0) 

do i = 1, nocc 
term(26) = term(26) + s1(q,i) * wm_interm_32_pt2(p,i)
term(27) = term(27) + s1(q,i) * wm_interm_33_pt2(p,i)
term(28) = term(28) + s1(q,i) * wm_interm_34_pt2(p,i)
term(29) = term(29) + s1(q,i) * wm_interm_35_pt2(p,i)
term(30) = term(30) + t1(q,i) * wm_interm_42_pt2(p,i)
term(31) = term(31) + t1(q,i) * wm_interm_43_pt2(p,i)
term(32) = term(32) + t1(q,i) * wm_interm_48_pt2(p,i)
term(33) = term(33) + t1(q,i) * wm_interm_49_pt2(p,i)
end do 

term(26) = term(26) * (-1.9999999999999998d+0) 
term(27) = term(27) * (3.9999999999999996d+0) 
term(28) = term(28) * (1.9999999999999998d+0) 
term(29) = term(29) * (-3.9999999999999996d+0) 
term(30) = term(30) * (-1.9999999999999998d+0) 
term(31) = term(31) * (3.9999999999999996d+0) 
term(32) = term(32) * (2.0d+0) 
term(33) = term(33) * (-4.0d+0) 


    calc_D_vv_wm_cc3_pt2 = zero
    do s = 0, 33
    calc_D_vv_wm_cc3_pt2 = calc_D_vv_wm_cc3_pt2 + term(s)
    end do

    end function calc_D_vv_wm_cc3_pt2
    

  
      end module ss_cc3_pt012
