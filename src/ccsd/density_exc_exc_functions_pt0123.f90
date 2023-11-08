module density_exc_exc_functions_pt0123

    use ccsd_transformed_integrals
    use cc3_intermediates
    use arithmetic
    use s_gen
    use basis
    use eom_vectors
    
    implicit none
    !
    ! File generated automatically on 2018-03-30 15:42:11
    !
    real(F64), dimension(:, :), allocatable :: wm_interm_0_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_1_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_2_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_3_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_4_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_5_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_6_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_7_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_8_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_9_pt1 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_10_pt1 

    real(F64), dimension(:, :, :, :), allocatable :: wm_interm_0_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_1_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_2_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_3_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_4_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_5_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_6_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_7_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_8_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_9_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_10_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_11_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_12_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_13_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_14_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_15_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_16_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_17_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_18_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_19_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_20_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_21_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_22_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_23_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_24_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_25_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_26_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_27_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_28_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_29_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_30_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_31_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_32_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_33_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_34_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_35_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_36_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_37_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_38_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_39_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_40_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_41_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_42_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_43_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_44_pt2 
real(F64) :: wm_interm_45_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_46_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_47_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_48_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_49_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_50_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_51_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_52_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_53_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_54_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_55_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_56_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_57_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_58_pt2 
real(F64) :: wm_interm_59_pt2 
real(F64) :: wm_interm_60_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_61_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_62_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_63_pt2 


real(F64), dimension(:, :, :, :), allocatable :: wm_interm_0_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_1_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_2_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_3_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_4_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_5_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_6_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_7_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_8_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_9_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_10_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_11_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_12_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_13_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_14_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_15_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_16_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_17_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_18_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_19_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_20_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_21_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_22_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_23_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_24_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_25_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_26_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_27_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_28_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_29_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_30_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_31_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_32_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_33_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_34_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_35_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_36_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_37_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_38_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_39_pt3 
real(F64) :: wm_interm_40_pt3 
real(F64) :: wm_interm_41_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_42_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_43_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_44_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_45_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_46_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_47_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_48_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_49_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_50_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_51_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_52_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_53_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_54_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_55_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_56_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_57_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_58_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_59_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_60_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_61_pt3 
real(F64) :: wm_interm_62_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_63_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_64_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_65_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_66_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_67_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_68_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_69_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_70_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_71_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_72_pt3 

    
    contains
    
    subroutine wm_intermediates_ccsd_init_pt0(nocc, nactive)
    integer, intent(in) :: nocc
    integer, intent(in) :: nactive
    
    end subroutine wm_intermediates_ccsd_init_pt0
    
    subroutine wm_intermediates_ccsd_free_pt0
    
    end subroutine wm_intermediates_ccsd_free_pt0
    
    subroutine wm_intermediates_ccsd_pt0(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr)
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    real(F64) :: sum

    end subroutine wm_intermediates_ccsd_pt0

    subroutine wm_intermediates_ccsd_init_pt1(nocc, nactive)
    integer, intent(in) :: nocc
    integer, intent(in) :: nactive
    allocate(wm_interm_0_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_1_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_2_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_3_pt1(1: nocc, 1: nocc))
allocate(wm_interm_4_pt1(1: nocc, 1: nocc))
allocate(wm_interm_5_pt1(1: nocc, 1: nocc))
allocate(wm_interm_6_pt1(1: nocc, 1: nocc))
allocate(wm_interm_7_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_8_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_9_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_10_pt1(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
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

    end subroutine wm_intermediates_ccsd_init_pt1
    
    subroutine wm_intermediates_ccsd_free_pt1
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

    end subroutine wm_intermediates_ccsd_free_pt1
    
    subroutine wm_intermediates_ccsd_pt1(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr)
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    real(F64) :: sum
    integer :: a, i, b, j, k 

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
wm_interm_0_pt1(b, j) = wm_interm_0_pt1(b, j) + sum 
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
wm_interm_1_pt1(b, j) = wm_interm_1_pt1(b, j) + sum 
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
sum = sum + r1(vrdav_Rl, a,i) * r2(vrdav_Rr, a,j,b,k)
end do 
wm_interm_2_pt1(b, i, j, k) = wm_interm_2_pt1(b, i, j, k) + sum 
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
wm_interm_3_pt1(j, k) = wm_interm_3_pt1(j, k) + sum 
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
wm_interm_4_pt1(j, k) = wm_interm_4_pt1(j, k) + sum 
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
wm_interm_5_pt1(j, k) = wm_interm_5_pt1(j, k) + sum 
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
wm_interm_6_pt1(j, k) = wm_interm_6_pt1(j, k) + sum 
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
wm_interm_7_pt1(b, j, i, k) = wm_interm_7_pt1(b, j, i, k) + sum 
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
sum = sum + r2(vrdav_Rl, a,j,b,i) * r1(vrdav_Rr, a,i)
end do 
end do 
wm_interm_8_pt1(b, j) = wm_interm_8_pt1(b, j) + sum 
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
wm_interm_9_pt1(b, j) = wm_interm_9_pt1(b, j) + sum 
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
wm_interm_10_pt1(b, i, j, k) = wm_interm_10_pt1(b, i, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 



    end subroutine wm_intermediates_ccsd_pt1

    subroutine wm_intermediates_ccsd_init_pt2(nocc, nactive)
    integer, intent(in) :: nocc
    integer, intent(in) :: nactive
    allocate(wm_interm_0_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_1_pt2(1: nocc, 1: nocc))
allocate(wm_interm_2_pt2(1: nocc, 1: nocc))
allocate(wm_interm_3_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_4_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_5_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_6_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_7_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_8_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_9_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_10_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_11_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_12_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_13_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_14_pt2(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_15_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_16_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_17_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_18_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_19_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_20_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_21_pt2(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_22_pt2(1: nocc, 1: nocc))
allocate(wm_interm_23_pt2(1: nocc, 1: nocc))
allocate(wm_interm_24_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_25_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_26_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_27_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_28_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_29_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_30_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_31_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_32_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_33_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_34_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_35_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_36_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_37_pt2(1: nocc, 1: nocc))
allocate(wm_interm_38_pt2(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_39_pt2(1: nocc, 1: nocc))
allocate(wm_interm_40_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_41_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_42_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_43_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_44_pt2(1: nocc, 1: nocc))
allocate(wm_interm_46_pt2(1: nocc, 1: nocc))
allocate(wm_interm_47_pt2(1: nocc, 1: nocc))
allocate(wm_interm_48_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_49_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_50_pt2(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_51_pt2(1: nocc, 1: nocc))
allocate(wm_interm_52_pt2(1: nocc, 1: nocc))
allocate(wm_interm_53_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_54_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_55_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_56_pt2(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_57_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_58_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_61_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_62_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_63_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
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

    end subroutine wm_intermediates_ccsd_init_pt2
    
    subroutine wm_intermediates_ccsd_free_pt2
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
deallocate(wm_interm_61_pt2)
deallocate(wm_interm_62_pt2)
deallocate(wm_interm_63_pt2)

    end subroutine wm_intermediates_ccsd_free_pt2
    
    subroutine wm_intermediates_ccsd_pt2(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr)
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    real(F64) :: sum
    integer :: a, b, i, j, k, c, l 

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
wm_interm_0_pt2(b, i, j, k) = wm_interm_0_pt2(b, i, j, k) + sum 
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
wm_interm_3_pt2(b, c) = wm_interm_3_pt2(b, c) + sum 
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
wm_interm_4_pt2(b, j) = wm_interm_4_pt2(b, j) + sum 
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
wm_interm_5_pt2(b, c, j, k) = wm_interm_5_pt2(b, c, j, k) + sum 
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
sum = sum + r2(vrdav_Rr, a,j,b,i) * s2(a,c,i,k)
end do 
end do 
wm_interm_8_pt2(b, c, j, k) = wm_interm_8_pt2(b, c, j, k) + sum 
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
wm_interm_9_pt2(b, c, j, k) = wm_interm_9_pt2(b, c, j, k) + sum 
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
wm_interm_10_pt2(b, j) = wm_interm_10_pt2(b, j) + sum 
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
wm_interm_11_pt2(b, j) = wm_interm_11_pt2(b, j) + sum 
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
wm_interm_12_pt2(b, j) = wm_interm_12_pt2(b, j) + sum 
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
wm_interm_13_pt2(b, i, j, k) = wm_interm_13_pt2(b, i, j, k) + sum 
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
wm_interm_15_pt2(b, c) = wm_interm_15_pt2(b, c) + sum 
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
wm_interm_16_pt2(b, c) = wm_interm_16_pt2(b, c) + sum 
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
wm_interm_17_pt2(b, c, j, k) = wm_interm_17_pt2(b, c, j, k) + sum 
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
wm_interm_18_pt2(b, c, j, k) = wm_interm_18_pt2(b, c, j, k) + sum 
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
wm_interm_19_pt2(b, c, j, k) = wm_interm_19_pt2(b, c, j, k) + sum 
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
wm_interm_20_pt2(b, c, j, k) = wm_interm_20_pt2(b, c, j, k) + sum 
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
wm_interm_21_pt2(i, j, k, l) = wm_interm_21_pt2(i, j, k, l) + sum 
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
wm_interm_22_pt2(j, k) = wm_interm_22_pt2(j, k) + sum 
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
wm_interm_23_pt2(j, k) = wm_interm_23_pt2(j, k) + sum 
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
wm_interm_24_pt2(b, j) = wm_interm_24_pt2(b, j) + sum 
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
wm_interm_25_pt2(b, j, i, k) = wm_interm_25_pt2(b, j, i, k) + sum 
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
wm_interm_26_pt2(b, i, j, k) = wm_interm_26_pt2(b, i, j, k) + sum 
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
wm_interm_27_pt2(b, j) = wm_interm_27_pt2(b, j) + sum 
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
wm_interm_28_pt2(b, c, j, k) = wm_interm_28_pt2(b, c, j, k) + sum 
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
wm_interm_29_pt2(b, i, j, k) = wm_interm_29_pt2(b, i, j, k) + sum 
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
wm_interm_30_pt2(b, j) = wm_interm_30_pt2(b, j) + sum 
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
wm_interm_31_pt2(b, j) = wm_interm_31_pt2(b, j) + sum 
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
wm_interm_32_pt2(b, c, j, k) = wm_interm_32_pt2(b, c, j, k) + sum 
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
wm_interm_33_pt2(b, c, j, k) = wm_interm_33_pt2(b, c, j, k) + sum 
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
wm_interm_34_pt2(b, c) = wm_interm_34_pt2(b, c) + sum 
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
wm_interm_35_pt2(b, c) = wm_interm_35_pt2(b, c) + sum 
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
wm_interm_36_pt2(b, c, j, k) = wm_interm_36_pt2(b, c, j, k) + sum 
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
wm_interm_37_pt2(j, k) = wm_interm_37_pt2(j, k) + sum 
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
wm_interm_38_pt2(i, j, k, l) = wm_interm_38_pt2(i, j, k, l) + sum 
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
wm_interm_39_pt2(j, k) = wm_interm_39_pt2(j, k) + sum 
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
wm_interm_40_pt2(b, j) = wm_interm_40_pt2(b, j) + sum 
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
wm_interm_41_pt2(b, j) = wm_interm_41_pt2(b, j) + sum 
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
wm_interm_42_pt2(b, j) = wm_interm_42_pt2(b, j) + sum 
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
wm_interm_43_pt2(b, j) = wm_interm_43_pt2(b, j) + sum 
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
wm_interm_44_pt2(i, j) = wm_interm_44_pt2(i, j) + sum 
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
wm_interm_45_pt2 = wm_interm_45_pt2 + sum 
!$omp parallel private(a, i, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do i = 1, nocc 
do j = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rr, a,i) * s1(a,j)
end do 
wm_interm_46_pt2(i, j) = wm_interm_46_pt2(i, j) + sum 
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
wm_interm_47_pt2(i, j) = wm_interm_47_pt2(i, j) + sum 
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
sum = sum + r1(vrdav_Rl, a,i) * r1(vrdav_Rr, b,j)
wm_interm_48_pt2(a, b, i, j) = wm_interm_48_pt2(a, b, i, j) + sum 
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
sum = sum + r1(vrdav_Rl, a,i) * r1(vrdav_Rr, b,i)
end do 
wm_interm_49_pt2(a, b) = wm_interm_49_pt2(a, b) + sum 
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
wm_interm_50_pt2(i, j, k, l) = wm_interm_50_pt2(i, j, k, l) + sum 
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
wm_interm_51_pt2(j, k) = wm_interm_51_pt2(j, k) + sum 
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
wm_interm_52_pt2(j, k) = wm_interm_52_pt2(j, k) + sum 
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
wm_interm_53_pt2(b, c, j, k) = wm_interm_53_pt2(b, c, j, k) + sum 
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
wm_interm_54_pt2(b, c, j, k) = wm_interm_54_pt2(b, c, j, k) + sum 
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
wm_interm_55_pt2(b, c, j, k) = wm_interm_55_pt2(b, c, j, k) + sum 
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
wm_interm_56_pt2(b, c, j, k) = wm_interm_56_pt2(b, c, j, k) + sum 
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
wm_interm_57_pt2(b, c) = wm_interm_57_pt2(b, c) + sum 
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
wm_interm_58_pt2(b, c) = wm_interm_58_pt2(b, c) + sum 
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
wm_interm_59_pt2 = wm_interm_59_pt2 + sum 
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
wm_interm_60_pt2 = wm_interm_60_pt2 + sum 
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
wm_interm_61_pt2(b, j, i, k) = wm_interm_61_pt2(b, j, i, k) + sum 
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
wm_interm_62_pt2(b, i, j, k) = wm_interm_62_pt2(b, i, j, k) + sum 
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
wm_interm_63_pt2(b, j, i, k) = wm_interm_63_pt2(b, j, i, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 



    end subroutine wm_intermediates_ccsd_pt2

    subroutine wm_intermediates_ccsd_init_pt3(nocc, nactive)
    integer, intent(in) :: nocc
    integer, intent(in) :: nactive
    allocate(wm_interm_0_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_1_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_2_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_3_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_4_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_5_pt3(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_6_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_7_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_8_pt3(1: nocc, 1: nocc))
allocate(wm_interm_9_pt3(1: nocc, 1: nocc))
allocate(wm_interm_10_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_11_pt3(1: nocc, 1: nocc))
allocate(wm_interm_12_pt3(1: nocc, 1: nocc))
allocate(wm_interm_13_pt3(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_14_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_15_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_16_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_17_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_18_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_19_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_20_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_21_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_22_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_23_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_24_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_25_pt3(1: nocc, 1: nocc))
allocate(wm_interm_26_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_27_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_28_pt3(1: nocc, 1: nocc))
allocate(wm_interm_29_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_30_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_31_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_32_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_33_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_34_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_35_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_36_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_37_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_38_pt3(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_39_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_42_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_43_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_44_pt3(1: nocc, 1: nocc))
allocate(wm_interm_45_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_46_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_47_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_48_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_49_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_50_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_51_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_52_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_53_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_54_pt3(1: nocc, 1: nocc))
allocate(wm_interm_55_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_56_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_57_pt3(1: nocc, 1: nocc))
allocate(wm_interm_58_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_59_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_60_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_61_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_63_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_64_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_65_pt3(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_66_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_67_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_68_pt3(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_69_pt3(1: nocc, 1: nocc))
allocate(wm_interm_70_pt3(1: nocc, 1: nocc))
allocate(wm_interm_71_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_72_pt3(nocc+1: nactive, nocc+1: nactive))
wm_interm_0_pt3 = zero 
wm_interm_1_pt3 = zero 
wm_interm_2_pt3 = zero 
wm_interm_3_pt3 = zero 
wm_interm_4_pt3 = zero 
wm_interm_5_pt3 = zero 
wm_interm_6_pt3 = zero 
wm_interm_7_pt3 = zero 
wm_interm_8_pt3 = zero 
wm_interm_9_pt3 = zero 
wm_interm_10_pt3 = zero 
wm_interm_11_pt3 = zero 
wm_interm_12_pt3 = zero 
wm_interm_13_pt3 = zero 
wm_interm_14_pt3 = zero 
wm_interm_15_pt3 = zero 
wm_interm_16_pt3 = zero 
wm_interm_17_pt3 = zero 
wm_interm_18_pt3 = zero 
wm_interm_19_pt3 = zero 
wm_interm_20_pt3 = zero 
wm_interm_21_pt3 = zero 
wm_interm_22_pt3 = zero 
wm_interm_23_pt3 = zero 
wm_interm_24_pt3 = zero 
wm_interm_25_pt3 = zero 
wm_interm_26_pt3 = zero 
wm_interm_27_pt3 = zero 
wm_interm_28_pt3 = zero 
wm_interm_29_pt3 = zero 
wm_interm_30_pt3 = zero 
wm_interm_31_pt3 = zero 
wm_interm_32_pt3 = zero 
wm_interm_33_pt3 = zero 
wm_interm_34_pt3 = zero 
wm_interm_35_pt3 = zero 
wm_interm_36_pt3 = zero 
wm_interm_37_pt3 = zero 
wm_interm_38_pt3 = zero 
wm_interm_39_pt3 = zero 
wm_interm_40_pt3 = zero 
wm_interm_41_pt3 = zero 
wm_interm_42_pt3 = zero 
wm_interm_43_pt3 = zero 
wm_interm_44_pt3 = zero 
wm_interm_45_pt3 = zero 
wm_interm_46_pt3 = zero 
wm_interm_47_pt3 = zero 
wm_interm_48_pt3 = zero 
wm_interm_49_pt3 = zero 
wm_interm_50_pt3 = zero 
wm_interm_51_pt3 = zero 
wm_interm_52_pt3 = zero 
wm_interm_53_pt3 = zero 
wm_interm_54_pt3 = zero 
wm_interm_55_pt3 = zero 
wm_interm_56_pt3 = zero 
wm_interm_57_pt3 = zero 
wm_interm_58_pt3 = zero 
wm_interm_59_pt3 = zero 
wm_interm_60_pt3 = zero 
wm_interm_61_pt3 = zero 
wm_interm_62_pt3 = zero 
wm_interm_63_pt3 = zero 
wm_interm_64_pt3 = zero 
wm_interm_65_pt3 = zero 
wm_interm_66_pt3 = zero 
wm_interm_67_pt3 = zero 
wm_interm_68_pt3 = zero 
wm_interm_69_pt3 = zero 
wm_interm_70_pt3 = zero 
wm_interm_71_pt3 = zero 
wm_interm_72_pt3 = zero 

    end subroutine wm_intermediates_ccsd_init_pt3
    
    subroutine wm_intermediates_ccsd_free_pt3
    deallocate(wm_interm_0_pt3)
deallocate(wm_interm_1_pt3)
deallocate(wm_interm_2_pt3)
deallocate(wm_interm_3_pt3)
deallocate(wm_interm_4_pt3)
deallocate(wm_interm_5_pt3)
deallocate(wm_interm_6_pt3)
deallocate(wm_interm_7_pt3)
deallocate(wm_interm_8_pt3)
deallocate(wm_interm_9_pt3)
deallocate(wm_interm_10_pt3)
deallocate(wm_interm_11_pt3)
deallocate(wm_interm_12_pt3)
deallocate(wm_interm_13_pt3)
deallocate(wm_interm_14_pt3)
deallocate(wm_interm_15_pt3)
deallocate(wm_interm_16_pt3)
deallocate(wm_interm_17_pt3)
deallocate(wm_interm_18_pt3)
deallocate(wm_interm_19_pt3)
deallocate(wm_interm_20_pt3)
deallocate(wm_interm_21_pt3)
deallocate(wm_interm_22_pt3)
deallocate(wm_interm_23_pt3)
deallocate(wm_interm_24_pt3)
deallocate(wm_interm_25_pt3)
deallocate(wm_interm_26_pt3)
deallocate(wm_interm_27_pt3)
deallocate(wm_interm_28_pt3)
deallocate(wm_interm_29_pt3)
deallocate(wm_interm_30_pt3)
deallocate(wm_interm_31_pt3)
deallocate(wm_interm_32_pt3)
deallocate(wm_interm_33_pt3)
deallocate(wm_interm_34_pt3)
deallocate(wm_interm_35_pt3)
deallocate(wm_interm_36_pt3)
deallocate(wm_interm_37_pt3)
deallocate(wm_interm_38_pt3)
deallocate(wm_interm_39_pt3)
deallocate(wm_interm_42_pt3)
deallocate(wm_interm_43_pt3)
deallocate(wm_interm_44_pt3)
deallocate(wm_interm_45_pt3)
deallocate(wm_interm_46_pt3)
deallocate(wm_interm_47_pt3)
deallocate(wm_interm_48_pt3)
deallocate(wm_interm_49_pt3)
deallocate(wm_interm_50_pt3)
deallocate(wm_interm_51_pt3)
deallocate(wm_interm_52_pt3)
deallocate(wm_interm_53_pt3)
deallocate(wm_interm_54_pt3)
deallocate(wm_interm_55_pt3)
deallocate(wm_interm_56_pt3)
deallocate(wm_interm_57_pt3)
deallocate(wm_interm_58_pt3)
deallocate(wm_interm_59_pt3)
deallocate(wm_interm_60_pt3)
deallocate(wm_interm_61_pt3)
deallocate(wm_interm_63_pt3)
deallocate(wm_interm_64_pt3)
deallocate(wm_interm_65_pt3)
deallocate(wm_interm_66_pt3)
deallocate(wm_interm_67_pt3)
deallocate(wm_interm_68_pt3)
deallocate(wm_interm_69_pt3)
deallocate(wm_interm_70_pt3)
deallocate(wm_interm_71_pt3)
deallocate(wm_interm_72_pt3)

    end subroutine wm_intermediates_ccsd_free_pt3
    
    subroutine wm_intermediates_ccsd_pt3(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr)
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
sum = sum + r2(vrdav_Rl, a,j,b,i) * r2(vrdav_Rr, a,k,c,i)
end do 
end do 
wm_interm_0_pt3(b, c, j, k) = wm_interm_0_pt3(b, c, j, k) + sum 
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
wm_interm_1_pt3(b, j, i, k) = wm_interm_1_pt3(b, j, i, k) + sum 
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
wm_interm_2_pt3(b, c, j, k) = wm_interm_2_pt3(b, c, j, k) + sum 
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
wm_interm_3_pt3(b, c) = wm_interm_3_pt3(b, c) + sum 
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
wm_interm_4_pt3(b, c) = wm_interm_4_pt3(b, c) + sum 
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
wm_interm_5_pt3(i, j, k, l) = wm_interm_5_pt3(i, j, k, l) + sum 
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
wm_interm_6_pt3(b, c, j, k) = wm_interm_6_pt3(b, c, j, k) + sum 
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
wm_interm_7_pt3(b, c, j, k) = wm_interm_7_pt3(b, c, j, k) + sum 
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
wm_interm_8_pt3(j, k) = wm_interm_8_pt3(j, k) + sum 
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
wm_interm_9_pt3(j, k) = wm_interm_9_pt3(j, k) + sum 
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
wm_interm_10_pt3(b, j, i, k) = wm_interm_10_pt3(b, j, i, k) + sum 
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
wm_interm_11_pt3(j, k) = wm_interm_11_pt3(j, k) + sum 
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
wm_interm_12_pt3(j, k) = wm_interm_12_pt3(j, k) + sum 
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
wm_interm_13_pt3(i, j, k, l) = wm_interm_13_pt3(i, j, k, l) + sum 
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
wm_interm_14_pt3(b, c, j, k) = wm_interm_14_pt3(b, c, j, k) + sum 
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
wm_interm_15_pt3(b, c, j, k) = wm_interm_15_pt3(b, c, j, k) + sum 
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
wm_interm_16_pt3(b, c, j, k) = wm_interm_16_pt3(b, c, j, k) + sum 
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
wm_interm_17_pt3(b, c, j, k) = wm_interm_17_pt3(b, c, j, k) + sum 
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
wm_interm_18_pt3(b, c) = wm_interm_18_pt3(b, c) + sum 
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
wm_interm_19_pt3(b, c) = wm_interm_19_pt3(b, c) + sum 
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
wm_interm_20_pt3(b, i, j, k) = wm_interm_20_pt3(b, i, j, k) + sum 
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
wm_interm_21_pt3(b, j) = wm_interm_21_pt3(b, j) + sum 
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
wm_interm_22_pt3(b, j) = wm_interm_22_pt3(b, j) + sum 
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
wm_interm_23_pt3(b, c) = wm_interm_23_pt3(b, c) + sum 
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
wm_interm_24_pt3(b, c) = wm_interm_24_pt3(b, c) + sum 
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
wm_interm_25_pt3(j, k) = wm_interm_25_pt3(j, k) + sum 
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
wm_interm_26_pt3(b, j, i, k) = wm_interm_26_pt3(b, j, i, k) + sum 
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
wm_interm_27_pt3(b, i, j, k) = wm_interm_27_pt3(b, i, j, k) + sum 
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
wm_interm_28_pt3(j, k) = wm_interm_28_pt3(j, k) + sum 
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
wm_interm_29_pt3(b, j) = wm_interm_29_pt3(b, j) + sum 
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
wm_interm_30_pt3(b, j) = wm_interm_30_pt3(b, j) + sum 
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
wm_interm_31_pt3(b, j) = wm_interm_31_pt3(b, j) + sum 
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
wm_interm_32_pt3(b, j) = wm_interm_32_pt3(b, j) + sum 
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
wm_interm_33_pt3(b, c, j, k) = wm_interm_33_pt3(b, c, j, k) + sum 
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
wm_interm_34_pt3(b, c, j, k) = wm_interm_34_pt3(b, c, j, k) + sum 
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
wm_interm_35_pt3(b, c, j, k) = wm_interm_35_pt3(b, c, j, k) + sum 
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
wm_interm_36_pt3(b, c, j, k) = wm_interm_36_pt3(b, c, j, k) + sum 
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
wm_interm_37_pt3(b, i, k, j) = wm_interm_37_pt3(b, i, k, j) + sum 
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
wm_interm_38_pt3(i, j, k, l) = wm_interm_38_pt3(i, j, k, l) + sum 
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
wm_interm_39_pt3(b, i, j, k) = wm_interm_39_pt3(b, i, j, k) + sum 
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
wm_interm_40_pt3 = wm_interm_40_pt3 + sum 
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
wm_interm_41_pt3 = wm_interm_41_pt3 + sum 
!$omp parallel private(i, a, b, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
sum = zero 
do i = 1, nocc 
sum = sum + r1(vrdav_Rl, a,i) * t1(b,i)
end do 
wm_interm_42_pt3(a, b) = wm_interm_42_pt3(a, b) + sum 
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
sum = sum + r1(vrdav_Rl, a,i) * r2(vrdav_Rr, a,j,b,k)
end do 
wm_interm_43_pt3(b, i, j, k) = wm_interm_43_pt3(b, i, j, k) + sum 
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
wm_interm_44_pt3(i, j) = wm_interm_44_pt3(i, j) + sum 
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
wm_interm_45_pt3(b, j) = wm_interm_45_pt3(b, j) + sum 
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
wm_interm_46_pt3(b, j) = wm_interm_46_pt3(b, j) + sum 
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
wm_interm_47_pt3(a, b, i, j) = wm_interm_47_pt3(a, b, i, j) + sum 
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
wm_interm_48_pt3(b, i, j, k) = wm_interm_48_pt3(b, i, j, k) + sum 
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
wm_interm_49_pt3(b, j) = wm_interm_49_pt3(b, j) + sum 
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
wm_interm_50_pt3(a, b, i, j) = wm_interm_50_pt3(a, b, i, j) + sum 
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
sum = sum + r2(vrdav_Rl, a,j,b,i) * r1(vrdav_Rr, a,i)
end do 
end do 
wm_interm_51_pt3(b, j) = wm_interm_51_pt3(b, j) + sum 
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
wm_interm_52_pt3(b, j, i, k) = wm_interm_52_pt3(b, j, i, k) + sum 
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
wm_interm_53_pt3(a, b) = wm_interm_53_pt3(a, b) + sum 
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
wm_interm_54_pt3(i, j) = wm_interm_54_pt3(i, j) + sum 
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
wm_interm_55_pt3(b, j) = wm_interm_55_pt3(b, j) + sum 
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
wm_interm_56_pt3(b, j) = wm_interm_56_pt3(b, j) + sum 
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
wm_interm_57_pt3(i, j) = wm_interm_57_pt3(i, j) + sum 
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
wm_interm_58_pt3(b, i, j, k) = wm_interm_58_pt3(b, i, j, k) + sum 
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
wm_interm_59_pt3(b, j) = wm_interm_59_pt3(b, j) + sum 
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
wm_interm_60_pt3(a, b) = wm_interm_60_pt3(a, b) + sum 
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
wm_interm_61_pt3(b, j) = wm_interm_61_pt3(b, j) + sum 
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
wm_interm_62_pt3 = wm_interm_62_pt3 + sum 
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
wm_interm_63_pt3(b, i, j, k) = wm_interm_63_pt3(b, i, j, k) + sum 
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
wm_interm_64_pt3(b, c, j, k) = wm_interm_64_pt3(b, c, j, k) + sum 
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
wm_interm_65_pt3(i, j, k, l) = wm_interm_65_pt3(i, j, k, l) + sum 
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
wm_interm_66_pt3(b, c, j, k) = wm_interm_66_pt3(b, c, j, k) + sum 
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
wm_interm_67_pt3(b, c, j, k) = wm_interm_67_pt3(b, c, j, k) + sum 
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
wm_interm_68_pt3(b, c, j, k) = wm_interm_68_pt3(b, c, j, k) + sum 
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
wm_interm_69_pt3(j, k) = wm_interm_69_pt3(j, k) + sum 
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
wm_interm_70_pt3(j, k) = wm_interm_70_pt3(j, k) + sum 
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
wm_interm_71_pt3(b, c) = wm_interm_71_pt3(b, c) + sum 
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
wm_interm_72_pt3(b, c) = wm_interm_72_pt3(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 



    end subroutine wm_intermediates_ccsd_pt3
        
    
    function calc_D_oo_wm_pt0(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_oo_wm_pt0
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
    real(F64), dimension(0:2) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(0) = term(0) + r2(vrdav_Rl, a,p,b,i) * r2(vrdav_Rr, a,i,b,q)
term(1) = term(1) + r2(vrdav_Rl, a,p,b,i) * r2(vrdav_Rr, a,q,b,i)
end do 
end do 
end do 

term(0) = term(0) * (2.0d+0) 
term(1) = term(1) * (-4.0d+0) 

do a = nocc + 1, nactive 
term(2) = term(2) + r1(vrdav_Rl, a,p) * r1(vrdav_Rr, a,q)
end do 

term(2) = term(2) * (-2.0d+0) 


    calc_D_oo_wm_pt0 = zero
    do s = 0, 2
    calc_D_oo_wm_pt0 = calc_D_oo_wm_pt0 + term(s)
    end do

    end function calc_D_oo_wm_pt0
    
    function calc_D_ov_wm_pt0(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_ov_wm_pt0
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
    
    calc_D_ov_wm_pt0 = zero
    do s = 0, 0
    calc_D_ov_wm_pt0 = calc_D_ov_wm_pt0 + term(s)
    end do

    end function calc_D_ov_wm_pt0
    
    function calc_D_vo_wm_pt0(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vo_wm_pt0
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
    real(F64), dimension(0:3) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do i = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + r1(vrdav_Rl, a,i) * r2(vrdav_Rr, a,q,p,i)
term(1) = term(1) + r1(vrdav_Rl, a,i) * r2(vrdav_Rr, a,i,p,q)
term(2) = term(2) + r2(vrdav_Rl, a,q,p,i) * r1(vrdav_Rr, a,i)
term(3) = term(3) + r2(vrdav_Rl, a,i,p,q) * r1(vrdav_Rr, a,i)
end do 
end do 

term(0) = term(0) * (-2.0d+0) 
term(1) = term(1) * (4.0d+0) 
term(2) = term(2) * (-2.0d+0) 
term(3) = term(3) * (4.0d+0) 


    calc_D_vo_wm_pt0 = zero
    do s = 0, 3
    calc_D_vo_wm_pt0 = calc_D_vo_wm_pt0 + term(s)
    end do

    end function calc_D_vo_wm_pt0
    
    function calc_D_vv_wm_pt0(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vv_wm_pt0
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
    real(F64), dimension(0:2) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + r2(vrdav_Rl, a,j,p,i) * r2(vrdav_Rr, a,i,q,j)
end do 
end do 
end do 

term(0) = term(0) * (-2.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(1) = term(1) + r2(vrdav_Rl, a,j,p,i) * r2(vrdav_Rr, a,j,q,i)
end do 
end do 
end do 

term(1) = term(1) * (4.0d+0) 

do i = 1, nocc 
term(2) = term(2) + r1(vrdav_Rl, p,i) * r1(vrdav_Rr, q,i)
end do 

term(2) = term(2) * (2.0d+0) 


    calc_D_vv_wm_pt0 = zero
    do s = 0, 2
    calc_D_vv_wm_pt0 = calc_D_vv_wm_pt0 + term(s)
    end do

    end function calc_D_vv_wm_pt0
    

    function calc_D_oo_wm_pt1(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_oo_wm_pt1
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
    real(F64), dimension(0:-1) :: term 
    term = 0.d+0 

    term = 0.d+0 
    
    calc_D_oo_wm_pt1 = zero
    do s = 0, -1
    calc_D_oo_wm_pt1 = calc_D_oo_wm_pt1 + term(s)
    end do

    end function calc_D_oo_wm_pt1
    
    function calc_D_ov_wm_pt1(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_ov_wm_pt1
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
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + s2(a,q,j,i) * wm_interm_2_pt1(a,p,j,i)
term(1) = term(1) + t2(a,q,j,i) * wm_interm_7_pt1(a,i,j,p)
term(2) = term(2) + t2(a,q,j,i) * wm_interm_7_pt1(a,j,i,p)
term(3) = term(3) + t2(a,q,j,i) * wm_interm_10_pt1(a,j,i,p)
term(4) = term(4) + t2(a,q,j,i) * wm_interm_10_pt1(a,i,j,p)
end do 
end do 
end do 

term(0) = term(0) * (-2.0d+0) 
term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (2.0d+0) 
term(3) = term(3) * (-1.0d+0) 
term(4) = term(4) * (2.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(5) = term(5) + s2(a,q,j,i) * wm_interm_2_pt1(a,p,i,j)
end do 
end do 
end do 

term(5) = term(5) * (4.0d+0) 


    calc_D_ov_wm_pt1 = zero
    do s = 0, 5
    calc_D_ov_wm_pt1 = calc_D_ov_wm_pt1 + term(s)
    end do

    end function calc_D_ov_wm_pt1
    
    function calc_D_vo_wm_pt1(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vo_wm_pt1
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
    real(F64), dimension(0:11) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do i = 1, nocc 
term(0) = term(0) + r1(vrdav_Rl, p,i) * wm_interm_3_pt1(i,q)
term(1) = term(1) + r1(vrdav_Rl, p,i) * wm_interm_4_pt1(i,q)
term(2) = term(2) + r1(vrdav_Rr, p,i) * wm_interm_5_pt1(i,q)
term(3) = term(3) + r1(vrdav_Rr, p,i) * wm_interm_6_pt1(i,q)
end do 

term(0) = term(0) * (-2.0d+0) 
term(1) = term(1) * (4.0d+0) 
term(2) = term(2) * (-2.0d+0) 
term(3) = term(3) * (4.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(4) = term(4) + s2(a,p,q,i) * wm_interm_0_pt1(a,i)
term(5) = term(5) + s2(a,p,q,i) * wm_interm_1_pt1(a,i)
term(6) = term(6) + s2(a,p,i,q) * wm_interm_0_pt1(a,i)
term(7) = term(7) + s2(a,p,i,q) * wm_interm_1_pt1(a,i)
term(8) = term(8) + t2(a,p,i,q) * wm_interm_8_pt1(a,i)
term(9) = term(9) + t2(a,p,i,q) * wm_interm_9_pt1(a,i)
term(10) = term(10) + t2(a,p,q,i) * wm_interm_8_pt1(a,i)
term(11) = term(11) + t2(a,p,q,i) * wm_interm_9_pt1(a,i)
end do 
end do 

term(4) = term(4) * (-2.0d+0) 
term(5) = term(5) * (4.0d+0) 
term(6) = term(6) * (4.0d+0) 
term(7) = term(7) * (-8.0d+0) 
term(8) = term(8) * (4.0d+0) 
term(9) = term(9) * (-8.0d+0) 
term(10) = term(10) * (-2.0d+0) 
term(11) = term(11) * (4.0d+0) 


    calc_D_vo_wm_pt1 = zero
    do s = 0, 11
    calc_D_vo_wm_pt1 = calc_D_vo_wm_pt1 + term(s)
    end do

    end function calc_D_vo_wm_pt1
    
    function calc_D_vv_wm_pt1(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vv_wm_pt1
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
    real(F64), dimension(0:-1) :: term 
    term = 0.d+0 

    term = 0.d+0 
    
    calc_D_vv_wm_pt1 = zero
    do s = 0, -1
    calc_D_vv_wm_pt1 = calc_D_vv_wm_pt1 + term(s)
    end do

    end function calc_D_vv_wm_pt1
    
    
    function calc_D_oo_wm_pt2(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_oo_wm_pt2
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , i, b, a, k, j, l 
    real(F64), dimension(0:237) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(0) = term(0) + wm_interm_28_pt2(a,b,q,i) * wm_interm_5_pt2(a,b,p,i)
term(1) = term(1) + wm_interm_28_pt2(a,b,q,i) * wm_interm_6_pt2(a,b,p,i)
term(2) = term(2) + wm_interm_28_pt2(a,b,q,i) * wm_interm_8_pt2(a,b,p,i)
term(3) = term(3) + wm_interm_28_pt2(a,b,q,i) * wm_interm_9_pt2(a,b,p,i)
term(4) = term(4) + wm_interm_17_pt2(a,b,q,i) * wm_interm_55_pt2(b,a,i,p)
term(5) = term(5) + wm_interm_17_pt2(a,b,q,i) * wm_interm_56_pt2(b,a,i,p)
term(6) = term(6) + wm_interm_20_pt2(a,b,q,i) * wm_interm_55_pt2(b,a,i,p)
term(7) = term(7) + wm_interm_20_pt2(a,b,q,i) * wm_interm_56_pt2(b,a,i,p)
term(8) = term(8) + wm_interm_32_pt2(a,b,q,i) * wm_interm_5_pt2(a,b,p,i)
term(9) = term(9) + wm_interm_32_pt2(a,b,q,i) * wm_interm_6_pt2(a,b,p,i)
term(10) = term(10) + wm_interm_32_pt2(a,b,q,i) * wm_interm_8_pt2(a,b,p,i)
term(11) = term(11) + wm_interm_32_pt2(a,b,q,i) * wm_interm_9_pt2(a,b,p,i)
term(12) = term(12) + wm_interm_17_pt2(a,b,q,i) * wm_interm_54_pt2(b,a,i,p)
term(13) = term(13) + wm_interm_17_pt2(a,b,q,i) * wm_interm_53_pt2(b,a,i,p)
term(14) = term(14) + wm_interm_20_pt2(a,b,q,i) * wm_interm_54_pt2(b,a,i,p)
term(15) = term(15) + wm_interm_20_pt2(a,b,q,i) * wm_interm_53_pt2(b,a,i,p)
term(16) = term(16) + wm_interm_33_pt2(a,b,q,i) * wm_interm_8_pt2(a,b,p,i)
term(17) = term(17) + wm_interm_33_pt2(a,b,q,i) * wm_interm_9_pt2(a,b,p,i)
term(18) = term(18) + wm_interm_19_pt2(a,b,q,i) * wm_interm_54_pt2(b,a,i,p)
term(19) = term(19) + wm_interm_19_pt2(a,b,q,i) * wm_interm_53_pt2(b,a,i,p)
term(20) = term(20) + wm_interm_18_pt2(a,b,q,i) * wm_interm_53_pt2(b,a,i,p)
term(21) = term(21) + wm_interm_18_pt2(a,b,q,i) * wm_interm_54_pt2(b,a,i,p)
term(22) = term(22) + wm_interm_33_pt2(a,b,q,i) * wm_interm_6_pt2(a,b,p,i)
term(23) = term(23) + wm_interm_33_pt2(a,b,q,i) * wm_interm_5_pt2(a,b,p,i)
term(24) = term(24) + wm_interm_36_pt2(a,b,q,i) * wm_interm_5_pt2(a,b,p,i)
term(25) = term(25) + wm_interm_36_pt2(a,b,q,i) * wm_interm_6_pt2(a,b,p,i)
term(26) = term(26) + wm_interm_36_pt2(a,b,q,i) * wm_interm_8_pt2(a,b,p,i)
term(27) = term(27) + wm_interm_36_pt2(a,b,q,i) * wm_interm_9_pt2(a,b,p,i)
term(28) = term(28) + wm_interm_18_pt2(a,b,q,i) * wm_interm_55_pt2(b,a,i,p)
term(29) = term(29) + wm_interm_18_pt2(a,b,q,i) * wm_interm_56_pt2(b,a,i,p)
term(30) = term(30) + wm_interm_19_pt2(a,b,q,i) * wm_interm_55_pt2(b,a,i,p)
term(31) = term(31) + wm_interm_19_pt2(a,b,q,i) * wm_interm_56_pt2(b,a,i,p)
term(32) = term(32) + wm_interm_17_pt2(a,b,i,q) * wm_interm_54_pt2(b,a,p,i)
term(33) = term(33) + wm_interm_17_pt2(a,b,i,q) * wm_interm_53_pt2(b,a,p,i)
term(34) = term(34) + wm_interm_17_pt2(a,b,i,q) * wm_interm_55_pt2(b,a,p,i)
term(35) = term(35) + wm_interm_17_pt2(a,b,i,q) * wm_interm_56_pt2(b,a,p,i)
term(36) = term(36) + wm_interm_36_pt2(a,b,i,q) * wm_interm_5_pt2(a,b,i,p)
term(37) = term(37) + wm_interm_36_pt2(a,b,i,q) * wm_interm_6_pt2(a,b,i,p)
term(38) = term(38) + wm_interm_33_pt2(a,b,i,q) * wm_interm_6_pt2(a,b,i,p)
term(39) = term(39) + wm_interm_33_pt2(a,b,i,q) * wm_interm_5_pt2(a,b,i,p)
term(40) = term(40) + wm_interm_36_pt2(a,b,i,q) * wm_interm_8_pt2(a,b,i,p)
term(41) = term(41) + wm_interm_36_pt2(a,b,i,q) * wm_interm_9_pt2(a,b,i,p)
term(42) = term(42) + wm_interm_33_pt2(a,b,i,q) * wm_interm_8_pt2(a,b,i,p)
term(43) = term(43) + wm_interm_33_pt2(a,b,i,q) * wm_interm_9_pt2(a,b,i,p)
term(44) = term(44) + wm_interm_20_pt2(a,b,i,q) * wm_interm_54_pt2(b,a,p,i)
term(45) = term(45) + wm_interm_20_pt2(a,b,i,q) * wm_interm_53_pt2(b,a,p,i)
term(46) = term(46) + wm_interm_20_pt2(a,b,i,q) * wm_interm_55_pt2(b,a,p,i)
term(47) = term(47) + wm_interm_20_pt2(a,b,i,q) * wm_interm_56_pt2(b,a,p,i)
term(48) = term(48) + wm_interm_19_pt2(a,b,i,q) * wm_interm_54_pt2(b,a,p,i)
term(49) = term(49) + wm_interm_19_pt2(a,b,i,q) * wm_interm_53_pt2(b,a,p,i)
term(50) = term(50) + wm_interm_19_pt2(a,b,i,q) * wm_interm_55_pt2(b,a,p,i)
term(51) = term(51) + wm_interm_19_pt2(a,b,i,q) * wm_interm_56_pt2(b,a,p,i)
term(52) = term(52) + wm_interm_18_pt2(a,b,i,q) * wm_interm_55_pt2(b,a,p,i)
term(53) = term(53) + wm_interm_18_pt2(a,b,i,q) * wm_interm_56_pt2(b,a,p,i)
term(54) = term(54) + wm_interm_18_pt2(a,b,i,q) * wm_interm_53_pt2(b,a,p,i)
term(55) = term(55) + wm_interm_18_pt2(a,b,i,q) * wm_interm_54_pt2(b,a,p,i)
term(56) = term(56) + wm_interm_32_pt2(a,b,i,q) * wm_interm_5_pt2(a,b,i,p)
term(57) = term(57) + wm_interm_32_pt2(a,b,i,q) * wm_interm_6_pt2(a,b,i,p)
term(58) = term(58) + wm_interm_28_pt2(a,b,i,q) * wm_interm_5_pt2(a,b,i,p)
term(59) = term(59) + wm_interm_28_pt2(a,b,i,q) * wm_interm_6_pt2(a,b,i,p)
term(60) = term(60) + wm_interm_32_pt2(a,b,i,q) * wm_interm_8_pt2(a,b,i,p)
term(61) = term(61) + wm_interm_32_pt2(a,b,i,q) * wm_interm_9_pt2(a,b,i,p)
term(62) = term(62) + wm_interm_28_pt2(a,b,i,q) * wm_interm_8_pt2(a,b,i,p)
term(63) = term(63) + wm_interm_28_pt2(a,b,i,q) * wm_interm_9_pt2(a,b,i,p)
end do 
end do 
end do 

term(0) = term(0) * (-4.0d+0) 
term(1) = term(1) * (8.0d+0) 
term(2) = term(2) * (8.0d+0) 
term(3) = term(3) * (-16.0d+0) 
term(4) = term(4) * (-4.0d+0) 
term(5) = term(5) * (8.0d+0) 
term(6) = term(6) * (8.0d+0) 
term(7) = term(7) * (-16.0d+0) 
term(8) = term(8) * (2.0d+0) 
term(9) = term(9) * (-4.0d+0) 
term(10) = term(10) * (-4.0d+0) 
term(11) = term(11) * (8.0d+0) 
term(12) = term(12) * (2.0d+0) 
term(13) = term(13) * (-4.0d+0) 
term(14) = term(14) * (-4.0d+0) 
term(15) = term(15) * (8.0d+0) 
term(16) = term(16) * (2.0d+0) 
term(17) = term(17) * (-4.0d+0) 
term(18) = term(18) * (2.0d+0) 
term(19) = term(19) * (-4.0d+0) 
term(20) = term(20) * (2.0d+0) 
term(21) = term(21) * (-4.0d+0) 
term(22) = term(22) * (2.0d+0) 
term(23) = term(23) * (-4.0d+0) 
term(24) = term(24) * (2.0d+0) 
term(25) = term(25) * (-4.0d+0) 
term(26) = term(26) * (-4.0d+0) 
term(27) = term(27) * (8.0d+0) 
term(28) = term(28) * (2.0d+0) 
term(29) = term(29) * (-4.0d+0) 
term(30) = term(30) * (-4.0d+0) 
term(31) = term(31) * (8.0d+0) 
term(32) = term(32) * (2.0d+0) 
term(33) = term(33) * (-4.0d+0) 
term(34) = term(34) * (-4.0d+0) 
term(35) = term(35) * (8.0d+0) 
term(36) = term(36) * (2.0d+0) 
term(37) = term(37) * (-4.0d+0) 
term(38) = term(38) * (2.0d+0) 
term(39) = term(39) * (-4.0d+0) 
term(40) = term(40) * (-4.0d+0) 
term(41) = term(41) * (8.0d+0) 
term(42) = term(42) * (2.0d+0) 
term(43) = term(43) * (-4.0d+0) 
term(44) = term(44) * (-4.0d+0) 
term(45) = term(45) * (8.0d+0) 
term(46) = term(46) * (8.0d+0) 
term(47) = term(47) * (-16.0d+0) 
term(48) = term(48) * (2.0d+0) 
term(49) = term(49) * (-4.0d+0) 
term(50) = term(50) * (-4.0d+0) 
term(51) = term(51) * (8.0d+0) 
term(52) = term(52) * (2.0d+0) 
term(53) = term(53) * (-4.0d+0) 
term(54) = term(54) * (2.0d+0) 
term(55) = term(55) * (-4.0d+0) 
term(56) = term(56) * (2.0d+0) 
term(57) = term(57) * (-4.0d+0) 
term(58) = term(58) * (-4.0d+0) 
term(59) = term(59) * (8.0d+0) 
term(60) = term(60) * (-4.0d+0) 
term(61) = term(61) * (8.0d+0) 
term(62) = term(62) * (8.0d+0) 
term(63) = term(63) * (-16.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
term(64) = term(64) + wm_interm_1_pt2(i,j) * wm_interm_50_pt2(q,j,i,p)
term(65) = term(65) + wm_interm_1_pt2(i,j) * wm_interm_50_pt2(q,j,p,i)
term(66) = term(66) + wm_interm_2_pt2(i,j) * wm_interm_50_pt2(q,j,i,p)
term(67) = term(67) + wm_interm_2_pt2(i,j) * wm_interm_50_pt2(q,j,p,i)
term(68) = term(68) + wm_interm_1_pt2(i,j) * wm_interm_50_pt2(j,q,p,i)
term(69) = term(69) + wm_interm_2_pt2(i,j) * wm_interm_50_pt2(j,q,p,i)
term(70) = term(70) + wm_interm_1_pt2(i,j) * wm_interm_50_pt2(j,q,i,p)
term(71) = term(71) + wm_interm_2_pt2(i,j) * wm_interm_50_pt2(j,q,i,p)
end do 
end do 

term(65) = term(65) * (-2.0d+0) 
term(66) = term(66) * (-2.0d+0) 
term(67) = term(67) * (4.0d+0) 
term(69) = term(69) * (-2.0d+0) 
term(70) = term(70) * (-2.0d+0) 
term(71) = term(71) * (4.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(72) = term(72) + wm_interm_21_pt2(i,j,k,l) * wm_interm_38_pt2(i,j,l,k)
term(73) = term(73) + wm_interm_21_pt2(i,j,k,l) * wm_interm_38_pt2(i,j,k,l)
end do 
end do 
end do 
end do 

term(72) = term(72) * (-2.0d+0) 
term(73) = term(73) * (4.0d+0) 

do a = nocc + 1, nactive 
term(74) = term(74) + r1(vrdav_Rl, a,q) * wm_interm_40_pt2(a,p)
term(75) = term(75) + r1(vrdav_Rl, a,q) * wm_interm_41_pt2(a,p)
term(76) = term(76) + s1(a,q) * wm_interm_11_pt2(a,p)
term(77) = term(77) + s1(a,q) * wm_interm_12_pt2(a,p)
term(78) = term(78) + r1(vrdav_Rr, a,p) * wm_interm_42_pt2(a,q)
term(79) = term(79) + r1(vrdav_Rr, a,p) * wm_interm_43_pt2(a,q)
term(80) = term(80) + t1(a,q) * wm_interm_24_pt2(a,p)
term(81) = term(81) + t1(a,q) * wm_interm_27_pt2(a,p)
term(82) = term(82) + wm_interm_10_pt2(a,q) * wm_interm_30_pt2(a,p)
term(83) = term(83) + wm_interm_10_pt2(a,q) * wm_interm_31_pt2(a,p)
term(84) = term(84) + wm_interm_30_pt2(a,p) * wm_interm_4_pt2(a,q)
term(85) = term(85) + wm_interm_31_pt2(a,p) * wm_interm_4_pt2(a,q)
end do 

term(74) = term(74) * (-4.0d+0) 
term(75) = term(75) * (2.0d+0) 
term(76) = term(76) * (-2.0d+0) 
term(77) = term(77) * (4.0d+0) 
term(78) = term(78) * (4.0d+0) 
term(79) = term(79) * (-2.0d+0) 
term(80) = term(80) * (-2.0d+0) 
term(81) = term(81) * (4.0d+0) 
term(82) = term(82) * (-2.0d+0) 
term(83) = term(83) * (4.0d+0) 
term(84) = term(84) * (4.0d+0) 
term(85) = term(85) * (-8.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
term(86) = term(86) + wm_interm_1_pt2(i,j) * wm_interm_44_pt2(j,i)
term(87) = term(87) + wm_interm_2_pt2(i,j) * wm_interm_44_pt2(j,i)
term(88) = term(88) + wm_interm_14_pt2(p,i,j,q) * wm_interm_44_pt2(j,i)
term(89) = term(89) + wm_interm_14_pt2(i,p,j,q) * wm_interm_44_pt2(j,i)
term(90) = term(90) + wm_interm_1_pt2(i,j) * wm_interm_51_pt2(j,i)
term(91) = term(91) + wm_interm_1_pt2(i,j) * wm_interm_52_pt2(j,i)
term(92) = term(92) + wm_interm_2_pt2(i,j) * wm_interm_51_pt2(j,i)
term(93) = term(93) + wm_interm_2_pt2(i,j) * wm_interm_52_pt2(j,i)
term(94) = term(94) + wm_interm_21_pt2(i,p,q,j) * wm_interm_37_pt2(i,j)
term(95) = term(95) + wm_interm_21_pt2(i,p,j,q) * wm_interm_37_pt2(i,j)
term(96) = term(96) + wm_interm_22_pt2(i,j) * wm_interm_37_pt2(i,j)
term(97) = term(97) + wm_interm_23_pt2(i,j) * wm_interm_37_pt2(i,j)
term(98) = term(98) + wm_interm_21_pt2(i,p,q,j) * wm_interm_39_pt2(i,j)
term(99) = term(99) + wm_interm_21_pt2(i,p,j,q) * wm_interm_39_pt2(i,j)
term(100) = term(100) + wm_interm_22_pt2(i,j) * wm_interm_39_pt2(i,j)
term(101) = term(101) + wm_interm_23_pt2(i,j) * wm_interm_39_pt2(i,j)
term(102) = term(102) + wm_interm_14_pt2(p,i,j,q) * wm_interm_51_pt2(j,i)
term(103) = term(103) + wm_interm_14_pt2(p,i,j,q) * wm_interm_52_pt2(j,i)
term(104) = term(104) + wm_interm_14_pt2(i,p,j,q) * wm_interm_51_pt2(j,i)
term(105) = term(105) + wm_interm_14_pt2(i,p,j,q) * wm_interm_52_pt2(j,i)
term(106) = term(106) + wm_interm_22_pt2(i,j) * wm_interm_38_pt2(i,p,q,j)
term(107) = term(107) + wm_interm_22_pt2(i,j) * wm_interm_38_pt2(p,i,q,j)
term(108) = term(108) + wm_interm_22_pt2(i,j) * wm_interm_38_pt2(p,i,j,q)
term(109) = term(109) + wm_interm_22_pt2(i,j) * wm_interm_38_pt2(i,p,j,q)
term(110) = term(110) + wm_interm_23_pt2(i,j) * wm_interm_38_pt2(i,p,q,j)
term(111) = term(111) + wm_interm_23_pt2(i,j) * wm_interm_38_pt2(p,i,q,j)
term(112) = term(112) + wm_interm_23_pt2(i,j) * wm_interm_38_pt2(p,i,j,q)
term(113) = term(113) + wm_interm_23_pt2(i,j) * wm_interm_38_pt2(i,p,j,q)
end do 
end do 

term(86) = term(86) * (4.0d+0) 
term(87) = term(87) * (-8.0d+0) 
term(88) = term(88) * (-2.0d+0) 
term(89) = term(89) * (4.0d+0) 
term(90) = term(90) * (-4.0d+0) 
term(91) = term(91) * (8.0d+0) 
term(92) = term(92) * (8.0d+0) 
term(93) = term(93) * (-16.0d+0) 
term(94) = term(94) * (2.0d+0) 
term(95) = term(95) * (-4.0d+0) 
term(96) = term(96) * (-4.0d+0) 
term(97) = term(97) * (8.0d+0) 
term(98) = term(98) * (-4.0d+0) 
term(99) = term(99) * (8.0d+0) 
term(100) = term(100) * (8.0d+0) 
term(101) = term(101) * (-16.0d+0) 
term(102) = term(102) * (2.0d+0) 
term(103) = term(103) * (-4.0d+0) 
term(104) = term(104) * (-4.0d+0) 
term(105) = term(105) * (8.0d+0) 
term(107) = term(107) * (-2.0d+0) 
term(109) = term(109) * (-2.0d+0) 
term(110) = term(110) * (-2.0d+0) 
term(111) = term(111) * (4.0d+0) 
term(112) = term(112) * (-2.0d+0) 
term(113) = term(113) * (4.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
term(114) = term(114) + wm_interm_14_pt2(i,j,k,q) * wm_interm_50_pt2(k,p,j,i)
term(115) = term(115) + wm_interm_14_pt2(i,j,k,q) * wm_interm_50_pt2(p,k,j,i)
end do 
end do 
end do 

term(115) = term(115) * (-2.0d+0) 

do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(116) = term(116) + wm_interm_3_pt2(a,b) * wm_interm_48_pt2(b,a,q,p)
term(117) = term(117) + wm_interm_3_pt2(a,b) * wm_interm_49_pt2(b,a)
term(118) = term(118) + wm_interm_48_pt2(a,b,q,p) * wm_interm_7_pt2(b,a)
term(119) = term(119) + wm_interm_49_pt2(a,b) * wm_interm_7_pt2(b,a)
term(120) = term(120) + wm_interm_17_pt2(a,b,p,q) * wm_interm_49_pt2(b,a)
term(121) = term(121) + wm_interm_20_pt2(a,b,p,q) * wm_interm_49_pt2(b,a)
term(122) = term(122) + wm_interm_19_pt2(a,b,p,q) * wm_interm_49_pt2(b,a)
term(123) = term(123) + wm_interm_18_pt2(a,b,p,q) * wm_interm_49_pt2(b,a)
term(124) = term(124) + wm_interm_3_pt2(a,b) * wm_interm_53_pt2(b,a,q,p)
term(125) = term(125) + wm_interm_3_pt2(a,b) * wm_interm_54_pt2(b,a,q,p)
term(126) = term(126) + wm_interm_53_pt2(a,b,q,p) * wm_interm_7_pt2(b,a)
term(127) = term(127) + wm_interm_54_pt2(a,b,q,p) * wm_interm_7_pt2(b,a)
term(128) = term(128) + wm_interm_3_pt2(a,b) * wm_interm_55_pt2(b,a,q,p)
term(129) = term(129) + wm_interm_3_pt2(a,b) * wm_interm_56_pt2(b,a,q,p)
term(130) = term(130) + wm_interm_3_pt2(a,b) * wm_interm_57_pt2(b,a)
term(131) = term(131) + wm_interm_3_pt2(a,b) * wm_interm_58_pt2(b,a)
term(132) = term(132) + wm_interm_55_pt2(a,b,q,p) * wm_interm_7_pt2(b,a)
term(133) = term(133) + wm_interm_56_pt2(a,b,q,p) * wm_interm_7_pt2(b,a)
term(134) = term(134) + wm_interm_57_pt2(a,b) * wm_interm_7_pt2(b,a)
term(135) = term(135) + wm_interm_58_pt2(a,b) * wm_interm_7_pt2(b,a)
term(136) = term(136) + wm_interm_34_pt2(a,b) * wm_interm_8_pt2(a,b,p,q)
term(137) = term(137) + wm_interm_34_pt2(a,b) * wm_interm_9_pt2(a,b,p,q)
term(138) = term(138) + wm_interm_34_pt2(a,b) * wm_interm_6_pt2(a,b,p,q)
term(139) = term(139) + wm_interm_34_pt2(a,b) * wm_interm_5_pt2(a,b,p,q)
term(140) = term(140) + wm_interm_15_pt2(a,b) * wm_interm_34_pt2(a,b)
term(141) = term(141) + wm_interm_16_pt2(a,b) * wm_interm_34_pt2(a,b)
term(142) = term(142) + wm_interm_35_pt2(a,b) * wm_interm_8_pt2(a,b,p,q)
term(143) = term(143) + wm_interm_35_pt2(a,b) * wm_interm_9_pt2(a,b,p,q)
term(144) = term(144) + wm_interm_35_pt2(a,b) * wm_interm_6_pt2(a,b,p,q)
term(145) = term(145) + wm_interm_35_pt2(a,b) * wm_interm_5_pt2(a,b,p,q)
term(146) = term(146) + wm_interm_15_pt2(a,b) * wm_interm_35_pt2(a,b)
term(147) = term(147) + wm_interm_16_pt2(a,b) * wm_interm_35_pt2(a,b)
term(148) = term(148) + wm_interm_17_pt2(a,b,p,q) * wm_interm_57_pt2(b,a)
term(149) = term(149) + wm_interm_17_pt2(a,b,p,q) * wm_interm_58_pt2(b,a)
term(150) = term(150) + wm_interm_20_pt2(a,b,p,q) * wm_interm_57_pt2(b,a)
term(151) = term(151) + wm_interm_20_pt2(a,b,p,q) * wm_interm_58_pt2(b,a)
term(152) = term(152) + wm_interm_15_pt2(a,b) * wm_interm_36_pt2(a,b,p,q)
term(153) = term(153) + wm_interm_16_pt2(a,b) * wm_interm_36_pt2(a,b,p,q)
term(154) = term(154) + wm_interm_15_pt2(a,b) * wm_interm_33_pt2(a,b,p,q)
term(155) = term(155) + wm_interm_16_pt2(a,b) * wm_interm_33_pt2(a,b,p,q)
term(156) = term(156) + wm_interm_19_pt2(a,b,p,q) * wm_interm_57_pt2(b,a)
term(157) = term(157) + wm_interm_19_pt2(a,b,p,q) * wm_interm_58_pt2(b,a)
term(158) = term(158) + wm_interm_18_pt2(a,b,p,q) * wm_interm_57_pt2(b,a)
term(159) = term(159) + wm_interm_18_pt2(a,b,p,q) * wm_interm_58_pt2(b,a)
term(160) = term(160) + wm_interm_15_pt2(a,b) * wm_interm_32_pt2(a,b,p,q)
term(161) = term(161) + wm_interm_16_pt2(a,b) * wm_interm_32_pt2(a,b,p,q)
term(162) = term(162) + wm_interm_15_pt2(a,b) * wm_interm_28_pt2(a,b,p,q)
term(163) = term(163) + wm_interm_16_pt2(a,b) * wm_interm_28_pt2(a,b,p,q)
end do 
end do 

term(116) = term(116) * (-2.0d+0) 
term(117) = term(117) * (4.0d+0) 
term(118) = term(118) * (4.0d+0) 
term(119) = term(119) * (-8.0d+0) 
term(120) = term(120) * (-2.0d+0) 
term(121) = term(121) * (4.0d+0) 
term(122) = term(122) * (-2.0d+0) 
term(123) = term(123) * (4.0d+0) 
term(124) = term(124) * (2.0d+0) 
term(125) = term(125) * (-4.0d+0) 
term(126) = term(126) * (-4.0d+0) 
term(127) = term(127) * (8.0d+0) 
term(128) = term(128) * (2.0d+0) 
term(129) = term(129) * (-4.0d+0) 
term(130) = term(130) * (-4.0d+0) 
term(131) = term(131) * (8.0d+0) 
term(132) = term(132) * (-4.0d+0) 
term(133) = term(133) * (8.0d+0) 
term(134) = term(134) * (8.0d+0) 
term(135) = term(135) * (-16.0d+0) 
term(136) = term(136) * (-4.0d+0) 
term(137) = term(137) * (8.0d+0) 
term(138) = term(138) * (-4.0d+0) 
term(139) = term(139) * (8.0d+0) 
term(140) = term(140) * (8.0d+0) 
term(141) = term(141) * (-16.0d+0) 
term(142) = term(142) * (2.0d+0) 
term(143) = term(143) * (-4.0d+0) 
term(144) = term(144) * (2.0d+0) 
term(145) = term(145) * (-4.0d+0) 
term(146) = term(146) * (-4.0d+0) 
term(147) = term(147) * (8.0d+0) 
term(148) = term(148) * (2.0d+0) 
term(149) = term(149) * (-4.0d+0) 
term(150) = term(150) * (-4.0d+0) 
term(151) = term(151) * (8.0d+0) 
term(152) = term(152) * (2.0d+0) 
term(153) = term(153) * (-4.0d+0) 
term(154) = term(154) * (-4.0d+0) 
term(155) = term(155) * (8.0d+0) 
term(156) = term(156) * (2.0d+0) 
term(157) = term(157) * (-4.0d+0) 
term(158) = term(158) * (-4.0d+0) 
term(159) = term(159) * (8.0d+0) 
term(160) = term(160) * (2.0d+0) 
term(161) = term(161) * (-4.0d+0) 
term(162) = term(162) * (-4.0d+0) 
term(163) = term(163) * (8.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(164) = term(164) + s1(a,i) * wm_interm_12_pt2(a,i)
term(165) = term(165) + s1(a,i) * wm_interm_11_pt2(a,i)
term(166) = term(166) + t1(a,i) * wm_interm_27_pt2(a,i)
term(167) = term(167) + t1(a,i) * wm_interm_24_pt2(a,i)
term(168) = term(168) + wm_interm_29_pt2(a,p,i,q) * wm_interm_4_pt2(a,i)
term(169) = term(169) + wm_interm_30_pt2(a,i) * wm_interm_4_pt2(a,i)
term(170) = term(170) + wm_interm_29_pt2(a,p,q,i) * wm_interm_4_pt2(a,i)
term(171) = term(171) + wm_interm_31_pt2(a,i) * wm_interm_4_pt2(a,i)
term(172) = term(172) + wm_interm_10_pt2(a,i) * wm_interm_29_pt2(a,p,i,q)
term(173) = term(173) + wm_interm_10_pt2(a,i) * wm_interm_30_pt2(a,i)
term(174) = term(174) + wm_interm_10_pt2(a,i) * wm_interm_29_pt2(a,p,q,i)
term(175) = term(175) + wm_interm_10_pt2(a,i) * wm_interm_31_pt2(a,i)
term(176) = term(176) + wm_interm_13_pt2(a,p,q,i) * wm_interm_30_pt2(a,i)
term(177) = term(177) + wm_interm_13_pt2(a,p,q,i) * wm_interm_31_pt2(a,i)
term(178) = term(178) + wm_interm_13_pt2(a,p,i,q) * wm_interm_31_pt2(a,i)
term(179) = term(179) + wm_interm_13_pt2(a,p,i,q) * wm_interm_30_pt2(a,i)
end do 
end do 

term(164) = term(164) * (8.0d+0) 
term(165) = term(165) * (-4.0d+0) 
term(166) = term(166) * (-8.0d+0) 
term(167) = term(167) * (4.0d+0) 
term(168) = term(168) * (4.0d+0) 
term(169) = term(169) * (-8.0d+0) 
term(170) = term(170) * (-8.0d+0) 
term(171) = term(171) * (16.0d+0) 
term(172) = term(172) * (-2.0d+0) 
term(173) = term(173) * (4.0d+0) 
term(174) = term(174) * (4.0d+0) 
term(175) = term(175) * (-8.0d+0) 
term(176) = term(176) * (4.0d+0) 
term(177) = term(177) * (-8.0d+0) 
term(178) = term(178) * (4.0d+0) 
term(179) = term(179) * (-2.0d+0) 

do i = 1, nocc 
term(180) = term(180) + wm_interm_1_pt2(q,i) * wm_interm_44_pt2(i,p)
term(181) = term(181) + wm_interm_2_pt2(q,i) * wm_interm_44_pt2(i,p)
term(182) = term(182) + wm_interm_1_pt2(i,q) * wm_interm_44_pt2(p,i)
term(183) = term(183) + wm_interm_2_pt2(i,q) * wm_interm_44_pt2(p,i)
term(184) = term(184) + wm_interm_1_pt2(q,i) * wm_interm_51_pt2(i,p)
term(185) = term(185) + wm_interm_1_pt2(q,i) * wm_interm_52_pt2(i,p)
term(186) = term(186) + wm_interm_2_pt2(q,i) * wm_interm_51_pt2(i,p)
term(187) = term(187) + wm_interm_2_pt2(q,i) * wm_interm_52_pt2(i,p)
term(188) = term(188) + wm_interm_22_pt2(p,i) * wm_interm_37_pt2(q,i)
term(189) = term(189) + wm_interm_23_pt2(p,i) * wm_interm_37_pt2(q,i)
term(190) = term(190) + wm_interm_22_pt2(p,i) * wm_interm_39_pt2(q,i)
term(191) = term(191) + wm_interm_23_pt2(p,i) * wm_interm_39_pt2(q,i)
term(192) = term(192) + wm_interm_1_pt2(i,q) * wm_interm_51_pt2(p,i)
term(193) = term(193) + wm_interm_1_pt2(i,q) * wm_interm_52_pt2(p,i)
term(194) = term(194) + wm_interm_2_pt2(i,q) * wm_interm_51_pt2(p,i)
term(195) = term(195) + wm_interm_2_pt2(i,q) * wm_interm_52_pt2(p,i)
term(196) = term(196) + wm_interm_22_pt2(i,p) * wm_interm_37_pt2(i,q)
term(197) = term(197) + wm_interm_22_pt2(i,p) * wm_interm_39_pt2(i,q)
term(198) = term(198) + wm_interm_23_pt2(i,p) * wm_interm_37_pt2(i,q)
term(199) = term(199) + wm_interm_23_pt2(i,p) * wm_interm_39_pt2(i,q)
end do 

term(180) = term(180) * (-2.0d+0) 
term(181) = term(181) * (4.0d+0) 
term(182) = term(182) * (-2.0d+0) 
term(183) = term(183) * (4.0d+0) 
term(184) = term(184) * (2.0d+0) 
term(185) = term(185) * (-4.0d+0) 
term(186) = term(186) * (-4.0d+0) 
term(187) = term(187) * (8.0d+0) 
term(188) = term(188) * (2.0d+0) 
term(189) = term(189) * (-4.0d+0) 
term(190) = term(190) * (-4.0d+0) 
term(191) = term(191) * (8.0d+0) 
term(192) = term(192) * (2.0d+0) 
term(193) = term(193) * (-4.0d+0) 
term(194) = term(194) * (-4.0d+0) 
term(195) = term(195) * (8.0d+0) 
term(196) = term(196) * (2.0d+0) 
term(197) = term(197) * (-4.0d+0) 
term(198) = term(198) * (-4.0d+0) 
term(199) = term(199) * (8.0d+0) 

do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
term(200) = term(200) + wm_interm_14_pt2(i,j,k,q) * wm_interm_50_pt2(k,p,i,j)
term(201) = term(201) + wm_interm_14_pt2(i,j,k,q) * wm_interm_50_pt2(p,k,i,j)
end do 
end do 
end do 

term(200) = term(200) * (-2.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(202) = term(202) + wm_interm_17_pt2(a,b,i,j) * wm_interm_55_pt2(b,a,j,i)
term(203) = term(203) + wm_interm_17_pt2(a,b,i,j) * wm_interm_56_pt2(b,a,j,i)
term(204) = term(204) + wm_interm_20_pt2(a,b,i,j) * wm_interm_55_pt2(b,a,j,i)
term(205) = term(205) + wm_interm_20_pt2(a,b,i,j) * wm_interm_56_pt2(b,a,j,i)
term(206) = term(206) + wm_interm_17_pt2(a,b,i,j) * wm_interm_54_pt2(b,a,j,i)
term(207) = term(207) + wm_interm_17_pt2(a,b,i,j) * wm_interm_53_pt2(b,a,j,i)
term(208) = term(208) + wm_interm_20_pt2(a,b,i,j) * wm_interm_54_pt2(b,a,j,i)
term(209) = term(209) + wm_interm_20_pt2(a,b,i,j) * wm_interm_53_pt2(b,a,j,i)
term(210) = term(210) + wm_interm_19_pt2(a,b,i,j) * wm_interm_54_pt2(b,a,j,i)
term(211) = term(211) + wm_interm_19_pt2(a,b,i,j) * wm_interm_53_pt2(b,a,j,i)
term(212) = term(212) + wm_interm_18_pt2(a,b,i,j) * wm_interm_53_pt2(b,a,j,i)
term(213) = term(213) + wm_interm_18_pt2(a,b,i,j) * wm_interm_54_pt2(b,a,j,i)
term(214) = term(214) + wm_interm_18_pt2(a,b,i,j) * wm_interm_55_pt2(b,a,j,i)
term(215) = term(215) + wm_interm_18_pt2(a,b,i,j) * wm_interm_56_pt2(b,a,j,i)
term(216) = term(216) + wm_interm_19_pt2(a,b,i,j) * wm_interm_55_pt2(b,a,j,i)
term(217) = term(217) + wm_interm_19_pt2(a,b,i,j) * wm_interm_56_pt2(b,a,j,i)
end do 
end do 
end do 
end do 

term(202) = term(202) * (8.0d+0) 
term(203) = term(203) * (-16.0d+0) 
term(204) = term(204) * (-16.0d+0) 
term(205) = term(205) * (32.0d+0) 
term(206) = term(206) * (-4.0d+0) 
term(207) = term(207) * (8.0d+0) 
term(208) = term(208) * (8.0d+0) 
term(209) = term(209) * (-16.0d+0) 
term(210) = term(210) * (-4.0d+0) 
term(211) = term(211) * (8.0d+0) 
term(212) = term(212) * (-4.0d+0) 
term(213) = term(213) * (8.0d+0) 
term(214) = term(214) * (-4.0d+0) 
term(215) = term(215) * (8.0d+0) 
term(216) = term(216) * (8.0d+0) 
term(217) = term(217) * (-16.0d+0) 

term(218) = term(218) + wm_interm_1_pt2(p,q) * wm_interm_45_pt2
term(219) = term(219) + wm_interm_2_pt2(p,q) * wm_interm_45_pt2
term(220) = term(220) + wm_interm_1_pt2(p,q) * wm_interm_59_pt2
term(221) = term(221) + wm_interm_1_pt2(p,q) * wm_interm_60_pt2
term(222) = term(222) + wm_interm_2_pt2(p,q) * wm_interm_59_pt2
term(223) = term(223) + wm_interm_2_pt2(p,q) * wm_interm_60_pt2

term(218) = term(218) * (4.0d+0) 
term(219) = term(219) * (-8.0d+0) 
term(220) = term(220) * (-2.0d+0) 
term(221) = term(221) * (4.0d+0) 
term(222) = term(222) * (4.0d+0) 
term(223) = term(223) * (-8.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(224) = term(224) + wm_interm_14_pt2(i,q,j,k) * wm_interm_50_pt2(j,k,p,i)
term(225) = term(225) + wm_interm_14_pt2(q,i,j,k) * wm_interm_50_pt2(j,k,p,i)
end do 
end do 
end do 

term(225) = term(225) * (-2.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(226) = term(226) + wm_interm_14_pt2(i,q,j,k) * wm_interm_50_pt2(j,k,i,p)
term(227) = term(227) + wm_interm_14_pt2(q,i,j,k) * wm_interm_50_pt2(j,k,i,p)
term(228) = term(228) + wm_interm_21_pt2(i,p,j,k) * wm_interm_38_pt2(i,q,k,j)
term(229) = term(229) + wm_interm_21_pt2(i,p,j,k) * wm_interm_38_pt2(i,q,j,k)
term(230) = term(230) + wm_interm_21_pt2(i,p,j,k) * wm_interm_38_pt2(q,i,j,k)
term(231) = term(231) + wm_interm_21_pt2(i,p,j,k) * wm_interm_38_pt2(q,i,k,j)
term(232) = term(232) + wm_interm_21_pt2(i,j,p,k) * wm_interm_38_pt2(i,j,k,q)
term(233) = term(233) + wm_interm_21_pt2(i,j,k,p) * wm_interm_38_pt2(i,j,k,q)
term(234) = term(234) + wm_interm_21_pt2(i,j,k,p) * wm_interm_38_pt2(i,j,q,k)
term(235) = term(235) + wm_interm_21_pt2(i,j,p,k) * wm_interm_38_pt2(i,j,q,k)
end do 
end do 
end do 

term(226) = term(226) * (-2.0d+0) 
term(229) = term(229) * (-2.0d+0) 
term(231) = term(231) * (-2.0d+0) 
term(233) = term(233) * (-2.0d+0) 
term(235) = term(235) * (-2.0d+0) 

do l = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
term(236) = term(236) + wm_interm_14_pt2(i,j,k,l) * wm_interm_50_pt2(k,l,j,i)
end do 
end do 
end do 
end do 

term(236) = term(236) * (-2.0d+0) 

do l = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
term(237) = term(237) + wm_interm_14_pt2(i,j,k,l) * wm_interm_50_pt2(k,l,i,j)
end do 
end do 
end do 
end do 

term(237) = term(237) * (4.0d+0) 


    calc_D_oo_wm_pt2 = zero
    do s = 0, 237
    calc_D_oo_wm_pt2 = calc_D_oo_wm_pt2 + term(s)
    end do

    end function calc_D_oo_wm_pt2
    
    function calc_D_ov_wm_pt2(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_ov_wm_pt2
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , i, j, a, k, b 
    real(F64), dimension(0:43) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + wm_interm_0_pt2(a,i,j,p) * wm_interm_17_pt2(a,q,j,i)
term(1) = term(1) + wm_interm_0_pt2(a,i,j,p) * wm_interm_18_pt2(a,q,j,i)
term(2) = term(2) + wm_interm_0_pt2(a,i,j,p) * wm_interm_19_pt2(a,q,j,i)
term(3) = term(3) + wm_interm_0_pt2(a,i,j,p) * wm_interm_20_pt2(a,q,j,i)
end do 
end do 
end do 

term(0) = term(0) * (-2.0d+0) 
term(1) = term(1) * (4.0d+0) 
term(2) = term(2) * (-2.0d+0) 
term(3) = term(3) * (4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(4) = term(4) + wm_interm_0_pt2(a,i,p,j) * wm_interm_17_pt2(a,q,j,i)
term(5) = term(5) + wm_interm_0_pt2(a,i,p,j) * wm_interm_18_pt2(a,q,j,i)
term(6) = term(6) + wm_interm_0_pt2(a,i,p,j) * wm_interm_19_pt2(a,q,j,i)
term(7) = term(7) + wm_interm_0_pt2(a,i,p,j) * wm_interm_20_pt2(a,q,j,i)
term(8) = term(8) + wm_interm_28_pt2(q,a,i,j) * wm_interm_29_pt2(a,i,j,p)
term(9) = term(9) + wm_interm_28_pt2(q,a,i,j) * wm_interm_29_pt2(a,i,p,j)
term(10) = term(10) + wm_interm_29_pt2(a,i,j,p) * wm_interm_32_pt2(q,a,i,j)
term(11) = term(11) + wm_interm_29_pt2(a,i,p,j) * wm_interm_32_pt2(q,a,i,j)
term(12) = term(12) + wm_interm_29_pt2(a,i,p,j) * wm_interm_33_pt2(q,a,i,j)
term(13) = term(13) + wm_interm_29_pt2(a,i,j,p) * wm_interm_33_pt2(q,a,i,j)
term(14) = term(14) + wm_interm_29_pt2(a,i,j,p) * wm_interm_36_pt2(q,a,i,j)
term(15) = term(15) + wm_interm_29_pt2(a,i,p,j) * wm_interm_36_pt2(q,a,i,j)
end do 
end do 
end do 

term(4) = term(4) * (4.0d+0) 
term(5) = term(5) * (-2.0d+0) 
term(6) = term(6) * (4.0d+0) 
term(7) = term(7) * (-8.0d+0) 
term(8) = term(8) * (4.0d+0) 
term(9) = term(9) * (-8.0d+0) 
term(10) = term(10) * (-2.0d+0) 
term(11) = term(11) * (4.0d+0) 
term(12) = term(12) * (-2.0d+0) 
term(13) = term(13) * (4.0d+0) 
term(14) = term(14) * (-2.0d+0) 
term(15) = term(15) * (4.0d+0) 

do a = nocc + 1, nactive 
term(16) = term(16) + wm_interm_11_pt2(a,p) * wm_interm_3_pt2(a,q)
term(17) = term(17) + wm_interm_12_pt2(a,p) * wm_interm_3_pt2(a,q)
term(18) = term(18) + wm_interm_11_pt2(a,p) * wm_interm_7_pt2(a,q)
term(19) = term(19) + wm_interm_12_pt2(a,p) * wm_interm_7_pt2(a,q)
term(20) = term(20) + wm_interm_30_pt2(a,p) * wm_interm_34_pt2(q,a)
term(21) = term(21) + wm_interm_31_pt2(a,p) * wm_interm_34_pt2(q,a)
term(22) = term(22) + wm_interm_30_pt2(a,p) * wm_interm_35_pt2(q,a)
term(23) = term(23) + wm_interm_31_pt2(a,p) * wm_interm_35_pt2(q,a)
end do 

term(16) = term(16) * (-2.0d+0) 
term(17) = term(17) * (4.0d+0) 
term(18) = term(18) * (4.0d+0) 
term(19) = term(19) * (-8.0d+0) 
term(20) = term(20) * (4.0d+0) 
term(21) = term(21) * (-8.0d+0) 
term(22) = term(22) * (-2.0d+0) 
term(23) = term(23) * (4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(24) = term(24) + wm_interm_13_pt2(q,i,j,k) * wm_interm_21_pt2(i,p,k,j)
term(25) = term(25) + wm_interm_13_pt2(q,i,j,k) * wm_interm_21_pt2(i,p,j,k)
end do 
end do 
end do 

term(24) = term(24) * (-2.0d+0) 
term(25) = term(25) * (4.0d+0) 

do i = 1, nocc 
term(26) = term(26) + wm_interm_10_pt2(q,i) * wm_interm_22_pt2(p,i)
term(27) = term(27) + wm_interm_10_pt2(q,i) * wm_interm_23_pt2(p,i)
term(28) = term(28) + wm_interm_22_pt2(p,i) * wm_interm_4_pt2(q,i)
term(29) = term(29) + wm_interm_23_pt2(p,i) * wm_interm_4_pt2(q,i)
term(30) = term(30) + wm_interm_1_pt2(p,i) * wm_interm_24_pt2(q,i)
term(31) = term(31) + wm_interm_24_pt2(q,i) * wm_interm_2_pt2(p,i)
term(32) = term(32) + wm_interm_1_pt2(p,i) * wm_interm_27_pt2(q,i)
term(33) = term(33) + wm_interm_27_pt2(q,i) * wm_interm_2_pt2(p,i)
term(34) = term(34) + s1(q,i) * wm_interm_44_pt2(p,i)
term(35) = term(35) + t1(q,i) * wm_interm_44_pt2(i,p)
term(36) = term(36) + s1(q,i) * wm_interm_51_pt2(p,i)
term(37) = term(37) + s1(q,i) * wm_interm_52_pt2(p,i)
term(38) = term(38) + t1(q,i) * wm_interm_51_pt2(i,p)
term(39) = term(39) + t1(q,i) * wm_interm_52_pt2(i,p)
end do 

term(26) = term(26) * (-2.0d+0) 
term(27) = term(27) * (4.0d+0) 
term(28) = term(28) * (4.0d+0) 
term(29) = term(29) * (-8.0d+0) 
term(30) = term(30) * (-2.0d+0) 
term(31) = term(31) * (4.0d+0) 
term(32) = term(32) * (4.0d+0) 
term(33) = term(33) * (-8.0d+0) 
term(34) = term(34) * (2.0d+0) 
term(35) = term(35) * (2.0d+0) 
term(36) = term(36) * (-2.0d+0) 
term(37) = term(37) * (4.0d+0) 
term(38) = term(38) * (-2.0d+0) 
term(39) = term(39) * (4.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(40) = term(40) + wm_interm_14_pt2(i,p,j,k) * wm_interm_25_pt2(q,j,k,i)
term(41) = term(41) + wm_interm_14_pt2(p,i,j,k) * wm_interm_25_pt2(q,j,k,i)
term(42) = term(42) + wm_interm_14_pt2(p,i,j,k) * wm_interm_26_pt2(q,j,k,i)
term(43) = term(43) + wm_interm_14_pt2(i,p,j,k) * wm_interm_26_pt2(q,j,k,i)
end do 
end do 
end do 

term(40) = term(40) * (-1.0d+0) 
term(41) = term(41) * (2.0d+0) 
term(42) = term(42) * (-1.0d+0) 
term(43) = term(43) * (2.0d+0) 


    calc_D_ov_wm_pt2 = zero
    do s = 0, 43
    calc_D_ov_wm_pt2 = calc_D_ov_wm_pt2 + term(s)
    end do

    end function calc_D_ov_wm_pt2
    
    function calc_D_vo_wm_pt2(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vo_wm_pt2
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , j, i, a, k, b 
    real(F64), dimension(0:135) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + wm_interm_13_pt2(a,i,q,j) * wm_interm_5_pt2(p,a,i,j)
term(1) = term(1) + wm_interm_13_pt2(a,i,q,j) * wm_interm_6_pt2(p,a,i,j)
term(2) = term(2) + wm_interm_13_pt2(a,i,q,j) * wm_interm_8_pt2(p,a,i,j)
term(3) = term(3) + wm_interm_13_pt2(a,i,q,j) * wm_interm_9_pt2(p,a,i,j)
term(4) = term(4) + wm_interm_13_pt2(a,i,j,q) * wm_interm_8_pt2(p,a,i,j)
term(5) = term(5) + wm_interm_13_pt2(a,i,j,q) * wm_interm_9_pt2(p,a,i,j)
term(6) = term(6) + wm_interm_13_pt2(a,i,j,q) * wm_interm_6_pt2(p,a,i,j)
term(7) = term(7) + wm_interm_13_pt2(a,i,j,q) * wm_interm_5_pt2(p,a,i,j)
term(8) = term(8) + wm_interm_17_pt2(p,a,i,j) * wm_interm_26_pt2(a,q,j,i)
term(9) = term(9) + wm_interm_20_pt2(p,a,i,j) * wm_interm_26_pt2(a,q,j,i)
term(10) = term(10) + wm_interm_17_pt2(p,a,i,j) * wm_interm_26_pt2(a,j,q,i)
term(11) = term(11) + wm_interm_20_pt2(p,a,i,j) * wm_interm_26_pt2(a,j,q,i)
term(12) = term(12) + wm_interm_17_pt2(p,a,i,j) * wm_interm_25_pt2(a,q,j,i)
term(13) = term(13) + wm_interm_20_pt2(p,a,i,j) * wm_interm_25_pt2(a,q,j,i)
term(14) = term(14) + wm_interm_17_pt2(p,a,i,j) * wm_interm_25_pt2(a,j,q,i)
term(15) = term(15) + wm_interm_20_pt2(p,a,i,j) * wm_interm_25_pt2(a,j,q,i)
term(16) = term(16) + wm_interm_19_pt2(p,a,i,j) * wm_interm_25_pt2(a,q,j,i)
term(17) = term(17) + wm_interm_18_pt2(p,a,i,j) * wm_interm_25_pt2(a,q,j,i)
term(18) = term(18) + wm_interm_18_pt2(p,a,i,j) * wm_interm_26_pt2(a,q,j,i)
term(19) = term(19) + wm_interm_19_pt2(p,a,i,j) * wm_interm_26_pt2(a,q,j,i)
term(20) = term(20) + wm_interm_18_pt2(p,a,i,j) * wm_interm_26_pt2(a,j,q,i)
term(21) = term(21) + wm_interm_19_pt2(p,a,i,j) * wm_interm_26_pt2(a,j,q,i)
term(22) = term(22) + wm_interm_18_pt2(p,a,i,j) * wm_interm_25_pt2(a,j,q,i)
term(23) = term(23) + wm_interm_19_pt2(p,a,i,j) * wm_interm_25_pt2(a,j,q,i)
end do 
end do 
end do 

term(0) = term(0) * (-2.0d+0) 
term(1) = term(1) * (4.0d+0) 
term(2) = term(2) * (4.0d+0) 
term(3) = term(3) * (-8.0d+0) 
term(4) = term(4) * (-2.0d+0) 
term(5) = term(5) * (4.0d+0) 
term(6) = term(6) * (-2.0d+0) 
term(7) = term(7) * (4.0d+0) 
term(8) = term(8) * (2.0d+0) 
term(9) = term(9) * (-4.0d+0) 
term(10) = term(10) * (-1.0d+0) 
term(11) = term(11) * (2.0d+0) 
term(12) = term(12) * (-1.0d+0) 
term(13) = term(13) * (2.0d+0) 
term(14) = term(14) * (2.0d+0) 
term(15) = term(15) * (-4.0d+0) 
term(16) = term(16) * (-1.0d+0) 
term(17) = term(17) * (2.0d+0) 
term(18) = term(18) * (-1.0d+0) 
term(19) = term(19) * (2.0d+0) 
term(20) = term(20) * (2.0d+0) 
term(21) = term(21) * (-1.0d+0) 
term(22) = term(22) * (-1.0d+0) 
term(23) = term(23) * (2.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(24) = term(24) + wm_interm_0_pt2(p,i,j,k) * wm_interm_14_pt2(k,j,i,q)
term(25) = term(25) + wm_interm_0_pt2(p,i,j,k) * wm_interm_14_pt2(j,k,i,q)
term(26) = term(26) + wm_interm_29_pt2(p,i,j,k) * wm_interm_38_pt2(q,i,j,k)
term(27) = term(27) + wm_interm_29_pt2(p,i,j,k) * wm_interm_38_pt2(i,q,j,k)
term(28) = term(28) + wm_interm_29_pt2(p,i,j,k) * wm_interm_38_pt2(i,q,k,j)
term(29) = term(29) + wm_interm_29_pt2(p,i,j,k) * wm_interm_38_pt2(q,i,k,j)
end do 
end do 
end do 

term(24) = term(24) * (-2.0d+0) 
term(25) = term(25) * (4.0d+0) 
term(26) = term(26) * (-1.0d+0) 
term(27) = term(27) * (2.0d+0) 
term(28) = term(28) * (-1.0d+0) 
term(29) = term(29) * (2.0d+0) 

do i = 1, nocc 
term(30) = term(30) + wm_interm_11_pt2(p,i) * wm_interm_1_pt2(i,q)
term(31) = term(31) + wm_interm_12_pt2(p,i) * wm_interm_1_pt2(i,q)
term(32) = term(32) + wm_interm_11_pt2(p,i) * wm_interm_2_pt2(i,q)
term(33) = term(33) + wm_interm_12_pt2(p,i) * wm_interm_2_pt2(i,q)
term(34) = term(34) + wm_interm_31_pt2(p,i) * wm_interm_37_pt2(q,i)
term(35) = term(35) + wm_interm_30_pt2(p,i) * wm_interm_37_pt2(q,i)
term(36) = term(36) + wm_interm_31_pt2(p,i) * wm_interm_39_pt2(q,i)
term(37) = term(37) + wm_interm_30_pt2(p,i) * wm_interm_39_pt2(q,i)
term(38) = term(38) + r1(vrdav_Rl, p,i) * wm_interm_46_pt2(i,q)
term(39) = term(39) + r1(vrdav_Rr, p,i) * wm_interm_47_pt2(i,q)
end do 

term(30) = term(30) * (-2.0d+0) 
term(31) = term(31) * (4.0d+0) 
term(32) = term(32) * (4.0d+0) 
term(33) = term(33) * (-8.0d+0) 
term(34) = term(34) * (4.0d+0) 
term(35) = term(35) * (-2.0d+0) 
term(36) = term(36) * (-8.0d+0) 
term(37) = term(37) * (4.0d+0) 
term(38) = term(38) * (2.0d+0) 
term(39) = term(39) * (2.0d+0) 

do a = nocc + 1, nactive 
term(40) = term(40) + wm_interm_10_pt2(a,q) * wm_interm_15_pt2(p,a)
term(41) = term(41) + wm_interm_10_pt2(a,q) * wm_interm_16_pt2(p,a)
term(42) = term(42) + wm_interm_15_pt2(p,a) * wm_interm_4_pt2(a,q)
term(43) = term(43) + wm_interm_16_pt2(p,a) * wm_interm_4_pt2(a,q)
term(44) = term(44) + wm_interm_24_pt2(a,q) * wm_interm_3_pt2(p,a)
term(45) = term(45) + wm_interm_24_pt2(a,q) * wm_interm_7_pt2(p,a)
term(46) = term(46) + wm_interm_27_pt2(a,q) * wm_interm_3_pt2(p,a)
term(47) = term(47) + wm_interm_27_pt2(a,q) * wm_interm_7_pt2(p,a)
end do 

term(40) = term(40) * (-2.0d+0) 
term(41) = term(41) * (4.0d+0) 
term(42) = term(42) * (4.0d+0) 
term(43) = term(43) * (-8.0d+0) 
term(44) = term(44) * (-2.0d+0) 
term(45) = term(45) * (4.0d+0) 
term(46) = term(46) * (4.0d+0) 
term(47) = term(47) * (-8.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
term(48) = term(48) + wm_interm_1_pt2(i,j) * wm_interm_25_pt2(p,q,j,i)
term(49) = term(49) + wm_interm_1_pt2(i,j) * wm_interm_26_pt2(p,q,j,i)
term(50) = term(50) + wm_interm_1_pt2(i,j) * wm_interm_25_pt2(p,j,q,i)
term(51) = term(51) + wm_interm_1_pt2(i,j) * wm_interm_26_pt2(p,j,q,i)
end do 
end do 

term(48) = term(48) * (2.0d+0) 
term(49) = term(49) * (-1.0d+0) 
term(50) = term(50) * (-1.0d+0) 
term(51) = term(51) * (2.0d+0) 

term(52) = term(52) + s1(p,q) * wm_interm_45_pt2
term(53) = term(53) + t1(p,q) * wm_interm_45_pt2
term(54) = term(54) + s1(p,q) * wm_interm_59_pt2
term(55) = term(55) + s1(p,q) * wm_interm_60_pt2
term(56) = term(56) + t1(p,q) * wm_interm_59_pt2
term(57) = term(57) + t1(p,q) * wm_interm_60_pt2

term(52) = term(52) * (-4.0d+0) 
term(53) = term(53) * (-4.0d+0) 
term(54) = term(54) * (2.0d+0) 
term(55) = term(55) * (-4.0d+0) 
term(56) = term(56) * (2.0d+0) 
term(57) = term(57) * (-4.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(58) = term(58) + r1(vrdav_Rl, b,i) * t2(a,p,q,i) * wm_interm_15_pt2(b,a)
term(59) = term(59) + r1(vrdav_Rl, b,i) * t2(a,p,q,i) * wm_interm_16_pt2(b,a)
term(60) = term(60) + r2(vrdav_Rl, a,q,p,i) * r1(vrdav_Rr, b,i) * wm_interm_3_pt2(b,a)
term(61) = term(61) + r2(vrdav_Rl, a,q,p,i) * r1(vrdav_Rr, b,i) * wm_interm_7_pt2(b,a)
end do 
end do 
end do 

term(58) = term(58) * (-2.0d+0) 
term(59) = term(59) * (4.0d+0) 
term(60) = term(60) * (-2.0d+0) 
term(61) = term(61) * (4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
term(62) = term(62) + wm_interm_0_pt2(p,i,q,j) * wm_interm_1_pt2(j,i)
term(63) = term(63) + wm_interm_0_pt2(p,i,j,q) * wm_interm_1_pt2(j,i)
term(64) = term(64) + wm_interm_0_pt2(p,i,q,j) * wm_interm_2_pt2(j,i)
term(65) = term(65) + wm_interm_0_pt2(p,i,j,q) * wm_interm_2_pt2(j,i)
term(66) = term(66) + wm_interm_13_pt2(p,i,j,q) * wm_interm_22_pt2(i,j)
term(67) = term(67) + wm_interm_13_pt2(p,i,j,q) * wm_interm_23_pt2(i,j)
term(68) = term(68) + wm_interm_13_pt2(p,i,q,j) * wm_interm_22_pt2(i,j)
term(69) = term(69) + wm_interm_13_pt2(p,i,q,j) * wm_interm_23_pt2(i,j)
term(70) = term(70) + wm_interm_25_pt2(p,q,i,j) * wm_interm_2_pt2(j,i)
term(71) = term(71) + wm_interm_26_pt2(p,q,i,j) * wm_interm_2_pt2(j,i)
term(72) = term(72) + wm_interm_25_pt2(p,i,q,j) * wm_interm_2_pt2(j,i)
term(73) = term(73) + wm_interm_26_pt2(p,i,q,j) * wm_interm_2_pt2(j,i)
term(74) = term(74) + wm_interm_29_pt2(p,i,q,j) * wm_interm_37_pt2(i,j)
term(75) = term(75) + wm_interm_29_pt2(p,i,j,q) * wm_interm_37_pt2(i,j)
term(76) = term(76) + wm_interm_29_pt2(p,i,q,j) * wm_interm_39_pt2(i,j)
term(77) = term(77) + wm_interm_29_pt2(p,i,j,q) * wm_interm_39_pt2(i,j)
end do 
end do 

term(62) = term(62) * (-2.0d+0) 
term(63) = term(63) * (4.0d+0) 
term(64) = term(64) * (4.0d+0) 
term(65) = term(65) * (-8.0d+0) 
term(66) = term(66) * (4.0d+0) 
term(67) = term(67) * (-8.0d+0) 
term(68) = term(68) * (-2.0d+0) 
term(69) = term(69) * (4.0d+0) 
term(70) = term(70) * (-4.0d+0) 
term(71) = term(71) * (2.0d+0) 
term(72) = term(72) * (2.0d+0) 
term(73) = term(73) * (-4.0d+0) 
term(74) = term(74) * (-2.0d+0) 
term(75) = term(75) * (4.0d+0) 
term(76) = term(76) * (4.0d+0) 
term(77) = term(77) * (-8.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(78) = term(78) + r1(vrdav_Rl, b,i) * r2(vrdav_Rr, a,q,p,i) * wm_interm_3_pt2(a,b)
term(79) = term(79) + r1(vrdav_Rl, b,i) * r2(vrdav_Rr, a,q,p,i) * wm_interm_7_pt2(a,b)
term(80) = term(80) + r1(vrdav_Rr, a,i) * s2(b,p,q,i) * wm_interm_34_pt2(a,b)
term(81) = term(81) + r1(vrdav_Rr, a,i) * s2(b,p,q,i) * wm_interm_35_pt2(a,b)
end do 
end do 
end do 

term(78) = term(78) * (-2.0d+0) 
term(79) = term(79) * (4.0d+0) 
term(80) = term(80) * (4.0d+0) 
term(81) = term(81) * (-2.0d+0) 

do a = nocc + 1, nactive 
do i = 1, nocc 
do b = nocc + 1, nactive 
term(82) = term(82) + r1(vrdav_Rl, a,i) * r2(vrdav_Rr, b,i,p,q) * wm_interm_3_pt2(b,a)
term(83) = term(83) + r1(vrdav_Rl, a,i) * r2(vrdav_Rr, b,i,p,q) * wm_interm_7_pt2(b,a)
term(84) = term(84) + r2(vrdav_Rl, a,i,p,q) * r1(vrdav_Rr, b,i) * wm_interm_3_pt2(b,a)
term(85) = term(85) + r2(vrdav_Rl, a,i,p,q) * r1(vrdav_Rr, b,i) * wm_interm_7_pt2(b,a)
end do 
end do 
end do 

term(82) = term(82) * (4.0d+0) 
term(83) = term(83) * (-8.0d+0) 
term(84) = term(84) * (4.0d+0) 
term(85) = term(85) * (-8.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(86) = term(86) + wm_interm_4_pt2(a,i) * wm_interm_5_pt2(p,a,q,i)
term(87) = term(87) + wm_interm_4_pt2(a,i) * wm_interm_6_pt2(p,a,q,i)
term(88) = term(88) + wm_interm_4_pt2(a,i) * wm_interm_8_pt2(p,a,q,i)
term(89) = term(89) + wm_interm_4_pt2(a,i) * wm_interm_9_pt2(p,a,q,i)
term(90) = term(90) + wm_interm_10_pt2(a,i) * wm_interm_5_pt2(p,a,q,i)
term(91) = term(91) + wm_interm_10_pt2(a,i) * wm_interm_6_pt2(p,a,q,i)
term(92) = term(92) + wm_interm_10_pt2(a,i) * wm_interm_8_pt2(p,a,q,i)
term(93) = term(93) + wm_interm_10_pt2(a,i) * wm_interm_9_pt2(p,a,q,i)
term(94) = term(94) + wm_interm_11_pt2(a,i) * wm_interm_17_pt2(a,p,i,q)
term(95) = term(95) + wm_interm_12_pt2(a,i) * wm_interm_17_pt2(a,p,i,q)
term(96) = term(96) + wm_interm_11_pt2(a,i) * wm_interm_20_pt2(a,p,i,q)
term(97) = term(97) + wm_interm_12_pt2(a,i) * wm_interm_20_pt2(a,p,i,q)
term(98) = term(98) + wm_interm_11_pt2(a,i) * wm_interm_18_pt2(a,p,i,q)
term(99) = term(99) + wm_interm_12_pt2(a,i) * wm_interm_18_pt2(a,p,i,q)
term(100) = term(100) + wm_interm_11_pt2(a,i) * wm_interm_19_pt2(a,p,i,q)
term(101) = term(101) + wm_interm_12_pt2(a,i) * wm_interm_19_pt2(a,p,i,q)
term(102) = term(102) + wm_interm_28_pt2(p,a,q,i) * wm_interm_30_pt2(a,i)
term(103) = term(103) + wm_interm_28_pt2(p,a,q,i) * wm_interm_31_pt2(a,i)
term(104) = term(104) + wm_interm_17_pt2(p,a,q,i) * wm_interm_27_pt2(a,i)
term(105) = term(105) + wm_interm_20_pt2(p,a,q,i) * wm_interm_27_pt2(a,i)
term(106) = term(106) + wm_interm_30_pt2(a,i) * wm_interm_32_pt2(p,a,q,i)
term(107) = term(107) + wm_interm_31_pt2(a,i) * wm_interm_32_pt2(p,a,q,i)
term(108) = term(108) + wm_interm_17_pt2(p,a,q,i) * wm_interm_24_pt2(a,i)
term(109) = term(109) + wm_interm_20_pt2(p,a,q,i) * wm_interm_24_pt2(a,i)
term(110) = term(110) + wm_interm_31_pt2(a,i) * wm_interm_33_pt2(p,a,q,i)
term(111) = term(111) + wm_interm_19_pt2(p,a,q,i) * wm_interm_24_pt2(a,i)
term(112) = term(112) + wm_interm_30_pt2(a,i) * wm_interm_33_pt2(p,a,q,i)
term(113) = term(113) + wm_interm_18_pt2(p,a,q,i) * wm_interm_24_pt2(a,i)
term(114) = term(114) + wm_interm_30_pt2(a,i) * wm_interm_36_pt2(p,a,q,i)
term(115) = term(115) + wm_interm_31_pt2(a,i) * wm_interm_36_pt2(p,a,q,i)
term(116) = term(116) + wm_interm_18_pt2(p,a,q,i) * wm_interm_27_pt2(a,i)
term(117) = term(117) + wm_interm_19_pt2(p,a,q,i) * wm_interm_27_pt2(a,i)
term(118) = term(118) + r2(vrdav_Rl, a,q,p,i) * wm_interm_40_pt2(a,i)
term(119) = term(119) + r2(vrdav_Rl, a,i,p,q) * wm_interm_40_pt2(a,i)
term(120) = term(120) + r2(vrdav_Rl, a,q,p,i) * wm_interm_41_pt2(a,i)
term(121) = term(121) + r2(vrdav_Rl, a,i,p,q) * wm_interm_41_pt2(a,i)
term(122) = term(122) + r2(vrdav_Rr, a,q,p,i) * wm_interm_42_pt2(a,i)
term(123) = term(123) + r2(vrdav_Rr, a,i,p,q) * wm_interm_42_pt2(a,i)
term(124) = term(124) + r2(vrdav_Rr, a,q,p,i) * wm_interm_43_pt2(a,i)
term(125) = term(125) + r2(vrdav_Rr, a,i,p,q) * wm_interm_43_pt2(a,i)
end do 
end do 

term(86) = term(86) * (4.0d+0) 
term(87) = term(87) * (-8.0d+0) 
term(88) = term(88) * (-8.0d+0) 
term(89) = term(89) * (16.0d+0) 
term(90) = term(90) * (-2.0d+0) 
term(91) = term(91) * (4.0d+0) 
term(92) = term(92) * (4.0d+0) 
term(93) = term(93) * (-8.0d+0) 
term(94) = term(94) * (4.0d+0) 
term(95) = term(95) * (-8.0d+0) 
term(96) = term(96) * (-8.0d+0) 
term(97) = term(97) * (16.0d+0) 
term(98) = term(98) * (-2.0d+0) 
term(99) = term(99) * (4.0d+0) 
term(100) = term(100) * (4.0d+0) 
term(101) = term(101) * (-8.0d+0) 
term(102) = term(102) * (-8.0d+0) 
term(103) = term(103) * (16.0d+0) 
term(104) = term(104) * (-8.0d+0) 
term(105) = term(105) * (16.0d+0) 
term(106) = term(106) * (4.0d+0) 
term(107) = term(107) * (-8.0d+0) 
term(108) = term(108) * (4.0d+0) 
term(109) = term(109) * (-8.0d+0) 
term(110) = term(110) * (4.0d+0) 
term(111) = term(111) * (4.0d+0) 
term(112) = term(112) * (-2.0d+0) 
term(113) = term(113) * (-2.0d+0) 
term(114) = term(114) * (4.0d+0) 
term(115) = term(115) * (-8.0d+0) 
term(116) = term(116) * (4.0d+0) 
term(117) = term(117) * (-8.0d+0) 
term(118) = term(118) * (-4.0d+0) 
term(119) = term(119) * (8.0d+0) 
term(120) = term(120) * (2.0d+0) 
term(121) = term(121) * (-4.0d+0) 
term(122) = term(122) * (4.0d+0) 
term(123) = term(123) * (-8.0d+0) 
term(124) = term(124) * (-2.0d+0) 
term(125) = term(125) * (4.0d+0) 

do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(126) = term(126) + r1(vrdav_Rl, a,i) * t2(b,p,i,q) * wm_interm_15_pt2(a,b)
term(127) = term(127) + r1(vrdav_Rl, a,i) * t2(b,p,i,q) * wm_interm_16_pt2(a,b)
term(128) = term(128) + r1(vrdav_Rr, a,i) * s2(b,p,i,q) * wm_interm_34_pt2(a,b)
term(129) = term(129) + r1(vrdav_Rr, a,i) * s2(b,p,i,q) * wm_interm_35_pt2(a,b)
end do 
end do 
end do 

term(126) = term(126) * (4.0d+0) 
term(127) = term(127) * (-8.0d+0) 
term(128) = term(128) * (-8.0d+0) 
term(129) = term(129) * (4.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(130) = term(130) + r2(vrdav_Rl, a,j,p,i) * wm_interm_61_pt2(a,i,j,q)
term(131) = term(131) + r2(vrdav_Rl, a,j,p,i) * wm_interm_61_pt2(a,j,i,q)
term(132) = term(132) + r2(vrdav_Rr, a,j,p,i) * wm_interm_62_pt2(a,j,i,q)
term(133) = term(133) + r2(vrdav_Rr, a,j,p,i) * wm_interm_62_pt2(a,i,j,q)
term(134) = term(134) + r2(vrdav_Rr, a,j,p,i) * wm_interm_63_pt2(a,i,j,q)
term(135) = term(135) + r2(vrdav_Rr, a,j,p,i) * wm_interm_63_pt2(a,j,i,q)
end do 
end do 
end do 

term(130) = term(130) * (-2.0d+0) 
term(131) = term(131) * (4.0d+0) 
term(132) = term(132) * (-1.0d+0) 
term(133) = term(133) * (2.0d+0) 
term(134) = term(134) * (-1.0d+0) 
term(135) = term(135) * (2.0d+0) 


    calc_D_vo_wm_pt2 = zero
    do s = 0, 135
    calc_D_vo_wm_pt2 = calc_D_vo_wm_pt2 + term(s)
    end do

    end function calc_D_vo_wm_pt2
    
    function calc_D_vv_wm_pt2(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vv_wm_pt2
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
    real(F64), dimension(0:181) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + wm_interm_17_pt2(q,a,i,j) * wm_interm_48_pt2(a,p,j,i)
term(1) = term(1) + wm_interm_20_pt2(q,a,i,j) * wm_interm_48_pt2(a,p,j,i)
term(2) = term(2) + wm_interm_18_pt2(q,a,i,j) * wm_interm_48_pt2(a,p,j,i)
term(3) = term(3) + wm_interm_19_pt2(q,a,i,j) * wm_interm_48_pt2(a,p,j,i)
term(4) = term(4) + wm_interm_17_pt2(a,q,i,j) * wm_interm_48_pt2(p,a,j,i)
term(5) = term(5) + wm_interm_18_pt2(a,q,i,j) * wm_interm_48_pt2(p,a,j,i)
term(6) = term(6) + wm_interm_19_pt2(a,q,i,j) * wm_interm_48_pt2(p,a,j,i)
term(7) = term(7) + wm_interm_20_pt2(a,q,i,j) * wm_interm_48_pt2(p,a,j,i)
term(8) = term(8) + wm_interm_28_pt2(q,a,i,j) * wm_interm_5_pt2(p,a,i,j)
term(9) = term(9) + wm_interm_28_pt2(q,a,i,j) * wm_interm_6_pt2(p,a,i,j)
term(10) = term(10) + wm_interm_28_pt2(q,a,i,j) * wm_interm_8_pt2(p,a,i,j)
term(11) = term(11) + wm_interm_28_pt2(q,a,i,j) * wm_interm_9_pt2(p,a,i,j)
term(12) = term(12) + wm_interm_17_pt2(q,a,i,j) * wm_interm_55_pt2(a,p,j,i)
term(13) = term(13) + wm_interm_20_pt2(q,a,i,j) * wm_interm_55_pt2(a,p,j,i)
term(14) = term(14) + wm_interm_17_pt2(q,a,i,j) * wm_interm_56_pt2(a,p,j,i)
term(15) = term(15) + wm_interm_20_pt2(q,a,i,j) * wm_interm_56_pt2(a,p,j,i)
term(16) = term(16) + wm_interm_32_pt2(q,a,i,j) * wm_interm_5_pt2(p,a,i,j)
term(17) = term(17) + wm_interm_32_pt2(q,a,i,j) * wm_interm_6_pt2(p,a,i,j)
term(18) = term(18) + wm_interm_32_pt2(q,a,i,j) * wm_interm_8_pt2(p,a,i,j)
term(19) = term(19) + wm_interm_32_pt2(q,a,i,j) * wm_interm_9_pt2(p,a,i,j)
term(20) = term(20) + wm_interm_17_pt2(q,a,i,j) * wm_interm_54_pt2(a,p,j,i)
term(21) = term(21) + wm_interm_20_pt2(q,a,i,j) * wm_interm_54_pt2(a,p,j,i)
term(22) = term(22) + wm_interm_17_pt2(q,a,i,j) * wm_interm_53_pt2(a,p,j,i)
term(23) = term(23) + wm_interm_20_pt2(q,a,i,j) * wm_interm_53_pt2(a,p,j,i)
term(24) = term(24) + wm_interm_33_pt2(q,a,i,j) * wm_interm_8_pt2(p,a,i,j)
term(25) = term(25) + wm_interm_33_pt2(q,a,i,j) * wm_interm_9_pt2(p,a,i,j)
term(26) = term(26) + wm_interm_19_pt2(q,a,i,j) * wm_interm_54_pt2(a,p,j,i)
term(27) = term(27) + wm_interm_19_pt2(q,a,i,j) * wm_interm_53_pt2(a,p,j,i)
term(28) = term(28) + wm_interm_33_pt2(q,a,i,j) * wm_interm_6_pt2(p,a,i,j)
term(29) = term(29) + wm_interm_33_pt2(q,a,i,j) * wm_interm_5_pt2(p,a,i,j)
term(30) = term(30) + wm_interm_18_pt2(q,a,i,j) * wm_interm_53_pt2(a,p,j,i)
term(31) = term(31) + wm_interm_18_pt2(q,a,i,j) * wm_interm_54_pt2(a,p,j,i)
term(32) = term(32) + wm_interm_36_pt2(q,a,i,j) * wm_interm_5_pt2(p,a,i,j)
term(33) = term(33) + wm_interm_36_pt2(q,a,i,j) * wm_interm_6_pt2(p,a,i,j)
term(34) = term(34) + wm_interm_36_pt2(q,a,i,j) * wm_interm_8_pt2(p,a,i,j)
term(35) = term(35) + wm_interm_36_pt2(q,a,i,j) * wm_interm_9_pt2(p,a,i,j)
term(36) = term(36) + wm_interm_18_pt2(q,a,i,j) * wm_interm_55_pt2(a,p,j,i)
term(37) = term(37) + wm_interm_19_pt2(q,a,i,j) * wm_interm_55_pt2(a,p,j,i)
term(38) = term(38) + wm_interm_18_pt2(q,a,i,j) * wm_interm_56_pt2(a,p,j,i)
term(39) = term(39) + wm_interm_19_pt2(q,a,i,j) * wm_interm_56_pt2(a,p,j,i)
term(40) = term(40) + wm_interm_17_pt2(a,q,i,j) * wm_interm_54_pt2(p,a,j,i)
term(41) = term(41) + wm_interm_17_pt2(a,q,i,j) * wm_interm_53_pt2(p,a,j,i)
term(42) = term(42) + wm_interm_17_pt2(a,q,i,j) * wm_interm_55_pt2(p,a,j,i)
term(43) = term(43) + wm_interm_17_pt2(a,q,i,j) * wm_interm_56_pt2(p,a,j,i)
term(44) = term(44) + wm_interm_18_pt2(a,q,i,j) * wm_interm_55_pt2(p,a,j,i)
term(45) = term(45) + wm_interm_18_pt2(a,q,i,j) * wm_interm_56_pt2(p,a,j,i)
term(46) = term(46) + wm_interm_18_pt2(a,q,i,j) * wm_interm_53_pt2(p,a,j,i)
term(47) = term(47) + wm_interm_18_pt2(a,q,i,j) * wm_interm_54_pt2(p,a,j,i)
term(48) = term(48) + wm_interm_19_pt2(a,q,i,j) * wm_interm_55_pt2(p,a,j,i)
term(49) = term(49) + wm_interm_19_pt2(a,q,i,j) * wm_interm_56_pt2(p,a,j,i)
term(50) = term(50) + wm_interm_19_pt2(a,q,i,j) * wm_interm_54_pt2(p,a,j,i)
term(51) = term(51) + wm_interm_19_pt2(a,q,i,j) * wm_interm_53_pt2(p,a,j,i)
term(52) = term(52) + wm_interm_20_pt2(a,q,i,j) * wm_interm_54_pt2(p,a,j,i)
term(53) = term(53) + wm_interm_20_pt2(a,q,i,j) * wm_interm_53_pt2(p,a,j,i)
term(54) = term(54) + wm_interm_20_pt2(a,q,i,j) * wm_interm_55_pt2(p,a,j,i)
term(55) = term(55) + wm_interm_20_pt2(a,q,i,j) * wm_interm_56_pt2(p,a,j,i)
term(56) = term(56) + wm_interm_33_pt2(a,q,i,j) * wm_interm_8_pt2(a,p,i,j)
term(57) = term(57) + wm_interm_33_pt2(a,q,i,j) * wm_interm_9_pt2(a,p,i,j)
term(58) = term(58) + wm_interm_36_pt2(a,q,i,j) * wm_interm_8_pt2(a,p,i,j)
term(59) = term(59) + wm_interm_36_pt2(a,q,i,j) * wm_interm_9_pt2(a,p,i,j)
term(60) = term(60) + wm_interm_36_pt2(a,q,i,j) * wm_interm_5_pt2(a,p,i,j)
term(61) = term(61) + wm_interm_36_pt2(a,q,i,j) * wm_interm_6_pt2(a,p,i,j)
term(62) = term(62) + wm_interm_33_pt2(a,q,i,j) * wm_interm_6_pt2(a,p,i,j)
term(63) = term(63) + wm_interm_33_pt2(a,q,i,j) * wm_interm_5_pt2(a,p,i,j)
term(64) = term(64) + wm_interm_32_pt2(a,q,i,j) * wm_interm_5_pt2(a,p,i,j)
term(65) = term(65) + wm_interm_32_pt2(a,q,i,j) * wm_interm_6_pt2(a,p,i,j)
term(66) = term(66) + wm_interm_28_pt2(a,q,i,j) * wm_interm_5_pt2(a,p,i,j)
term(67) = term(67) + wm_interm_28_pt2(a,q,i,j) * wm_interm_6_pt2(a,p,i,j)
term(68) = term(68) + wm_interm_32_pt2(a,q,i,j) * wm_interm_8_pt2(a,p,i,j)
term(69) = term(69) + wm_interm_32_pt2(a,q,i,j) * wm_interm_9_pt2(a,p,i,j)
term(70) = term(70) + wm_interm_28_pt2(a,q,i,j) * wm_interm_8_pt2(a,p,i,j)
term(71) = term(71) + wm_interm_28_pt2(a,q,i,j) * wm_interm_9_pt2(a,p,i,j)
end do 
end do 
end do 

term(0) = term(0) * (-4.0d+0) 
term(1) = term(1) * (8.0d+0) 
term(2) = term(2) * (2.0d+0) 
term(3) = term(3) * (-4.0d+0) 
term(4) = term(4) * (-4.0d+0) 
term(5) = term(5) * (2.0d+0) 
term(6) = term(6) * (-4.0d+0) 
term(7) = term(7) * (8.0d+0) 
term(8) = term(8) * (4.0d+0) 
term(9) = term(9) * (-8.0d+0) 
term(10) = term(10) * (-8.0d+0) 
term(11) = term(11) * (16.0d+0) 
term(12) = term(12) * (4.0d+0) 
term(13) = term(13) * (-8.0d+0) 
term(14) = term(14) * (-8.0d+0) 
term(15) = term(15) * (16.0d+0) 
term(16) = term(16) * (-2.0d+0) 
term(17) = term(17) * (4.0d+0) 
term(18) = term(18) * (4.0d+0) 
term(19) = term(19) * (-8.0d+0) 
term(20) = term(20) * (-2.0d+0) 
term(21) = term(21) * (4.0d+0) 
term(22) = term(22) * (4.0d+0) 
term(23) = term(23) * (-8.0d+0) 
term(24) = term(24) * (-2.0d+0) 
term(25) = term(25) * (4.0d+0) 
term(26) = term(26) * (-2.0d+0) 
term(27) = term(27) * (4.0d+0) 
term(28) = term(28) * (-2.0d+0) 
term(29) = term(29) * (4.0d+0) 
term(30) = term(30) * (-2.0d+0) 
term(31) = term(31) * (4.0d+0) 
term(32) = term(32) * (-2.0d+0) 
term(33) = term(33) * (4.0d+0) 
term(34) = term(34) * (4.0d+0) 
term(35) = term(35) * (-8.0d+0) 
term(36) = term(36) * (-2.0d+0) 
term(37) = term(37) * (4.0d+0) 
term(38) = term(38) * (4.0d+0) 
term(39) = term(39) * (-8.0d+0) 
term(40) = term(40) * (-2.0d+0) 
term(41) = term(41) * (4.0d+0) 
term(42) = term(42) * (4.0d+0) 
term(43) = term(43) * (-8.0d+0) 
term(44) = term(44) * (-2.0d+0) 
term(45) = term(45) * (4.0d+0) 
term(46) = term(46) * (-2.0d+0) 
term(47) = term(47) * (4.0d+0) 
term(48) = term(48) * (4.0d+0) 
term(49) = term(49) * (-8.0d+0) 
term(50) = term(50) * (-2.0d+0) 
term(51) = term(51) * (4.0d+0) 
term(52) = term(52) * (4.0d+0) 
term(53) = term(53) * (-8.0d+0) 
term(54) = term(54) * (-8.0d+0) 
term(55) = term(55) * (16.0d+0) 
term(56) = term(56) * (-2.0d+0) 
term(57) = term(57) * (4.0d+0) 
term(58) = term(58) * (4.0d+0) 
term(59) = term(59) * (-8.0d+0) 
term(60) = term(60) * (-2.0d+0) 
term(61) = term(61) * (4.0d+0) 
term(62) = term(62) * (-2.0d+0) 
term(63) = term(63) * (4.0d+0) 
term(64) = term(64) * (-2.0d+0) 
term(65) = term(65) * (4.0d+0) 
term(66) = term(66) * (4.0d+0) 
term(67) = term(67) * (-8.0d+0) 
term(68) = term(68) * (4.0d+0) 
term(69) = term(69) * (-8.0d+0) 
term(70) = term(70) * (-8.0d+0) 
term(71) = term(71) * (16.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
term(72) = term(72) + wm_interm_1_pt2(i,j) * wm_interm_48_pt2(q,p,j,i)
term(73) = term(73) + wm_interm_2_pt2(i,j) * wm_interm_48_pt2(q,p,j,i)
term(74) = term(74) + wm_interm_1_pt2(i,j) * wm_interm_53_pt2(q,p,j,i)
term(75) = term(75) + wm_interm_1_pt2(i,j) * wm_interm_54_pt2(q,p,j,i)
term(76) = term(76) + wm_interm_2_pt2(i,j) * wm_interm_53_pt2(q,p,j,i)
term(77) = term(77) + wm_interm_2_pt2(i,j) * wm_interm_54_pt2(q,p,j,i)
term(78) = term(78) + wm_interm_1_pt2(i,j) * wm_interm_55_pt2(q,p,j,i)
term(79) = term(79) + wm_interm_2_pt2(i,j) * wm_interm_55_pt2(q,p,j,i)
term(80) = term(80) + wm_interm_1_pt2(i,j) * wm_interm_56_pt2(q,p,j,i)
term(81) = term(81) + wm_interm_2_pt2(i,j) * wm_interm_56_pt2(q,p,j,i)
end do 
end do 

term(72) = term(72) * (2.0d+0) 
term(73) = term(73) * (-4.0d+0) 
term(74) = term(74) * (-2.0d+0) 
term(75) = term(75) * (4.0d+0) 
term(76) = term(76) * (4.0d+0) 
term(77) = term(77) * (-8.0d+0) 
term(78) = term(78) * (-2.0d+0) 
term(79) = term(79) * (4.0d+0) 
term(80) = term(80) * (4.0d+0) 
term(81) = term(81) * (-8.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do a = nocc + 1, nactive 
term(82) = term(82) + r2(vrdav_Rl, a,k,q,j) * r2(vrdav_Rr, a,l,p,i) * wm_interm_14_pt2(l,i,j,k)
term(83) = term(83) + r2(vrdav_Rl, a,k,q,j) * r2(vrdav_Rr, a,l,p,i) * wm_interm_14_pt2(i,l,j,k)
term(84) = term(84) + s2(a,p,k,j) * t2(a,q,l,i) * wm_interm_50_pt2(l,i,j,k)
term(85) = term(85) + s2(a,p,k,j) * t2(a,q,l,i) * wm_interm_50_pt2(i,l,j,k)
end do 
end do 
end do 
end do 
end do 

term(82) = term(82) * (-1.0d+0) 
term(83) = term(83) * (2.0d+0) 
term(84) = term(84) * (-1.0d+0) 
term(85) = term(85) * (2.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(86) = term(86) + wm_interm_13_pt2(q,i,j,k) * wm_interm_29_pt2(p,i,k,j)
term(87) = term(87) + wm_interm_13_pt2(q,i,j,k) * wm_interm_29_pt2(p,i,j,k)
end do 
end do 
end do 

term(86) = term(86) * (2.0d+0) 
term(87) = term(87) * (-4.0d+0) 

term(88) = term(88) + wm_interm_3_pt2(p,q) * wm_interm_45_pt2
term(89) = term(89) + wm_interm_45_pt2 * wm_interm_7_pt2(p,q)
term(90) = term(90) + wm_interm_3_pt2(p,q) * wm_interm_59_pt2
term(91) = term(91) + wm_interm_3_pt2(p,q) * wm_interm_60_pt2
term(92) = term(92) + wm_interm_59_pt2 * wm_interm_7_pt2(p,q)
term(93) = term(93) + wm_interm_60_pt2 * wm_interm_7_pt2(p,q)

term(88) = term(88) * (-4.0d+0) 
term(89) = term(89) * (8.0d+0) 
term(90) = term(90) * (2.0d+0) 
term(91) = term(91) * (-4.0d+0) 
term(92) = term(92) * (-4.0d+0) 
term(93) = term(93) * (8.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(94) = term(94) + r2(vrdav_Rr, a,j,p,i) * s2(a,q,l,k) * wm_interm_38_pt2(j,i,k,l)
term(95) = term(95) + r2(vrdav_Rr, a,j,p,i) * s2(a,q,l,k) * wm_interm_38_pt2(i,j,k,l)
end do 
end do 
end do 
end do 
end do 

term(94) = term(94) * (-1.0d+0) 
term(95) = term(95) * (2.0d+0) 

do l = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(96) = term(96) + r2(vrdav_Rl, a,k,p,i) * t2(a,q,l,j) * wm_interm_21_pt2(k,i,j,l)
term(97) = term(97) + r2(vrdav_Rl, a,k,p,i) * t2(a,q,l,j) * wm_interm_21_pt2(i,k,j,l)
end do 
end do 
end do 
end do 
end do 

term(96) = term(96) * (-1.0d+0) 
term(97) = term(97) * (2.0d+0) 

do i = 1, nocc 
term(98) = term(98) + r1(vrdav_Rl, q,i) * wm_interm_40_pt2(p,i)
term(99) = term(99) + r1(vrdav_Rl, q,i) * wm_interm_41_pt2(p,i)
term(100) = term(100) + s1(q,i) * wm_interm_11_pt2(p,i)
term(101) = term(101) + s1(q,i) * wm_interm_12_pt2(p,i)
term(102) = term(102) + r1(vrdav_Rr, p,i) * wm_interm_42_pt2(q,i)
term(103) = term(103) + r1(vrdav_Rr, p,i) * wm_interm_43_pt2(q,i)
term(104) = term(104) + t1(q,i) * wm_interm_24_pt2(p,i)
term(105) = term(105) + t1(q,i) * wm_interm_27_pt2(p,i)
term(106) = term(106) + wm_interm_10_pt2(q,i) * wm_interm_31_pt2(p,i)
term(107) = term(107) + wm_interm_10_pt2(q,i) * wm_interm_30_pt2(p,i)
term(108) = term(108) + wm_interm_30_pt2(p,i) * wm_interm_4_pt2(q,i)
term(109) = term(109) + wm_interm_31_pt2(p,i) * wm_interm_4_pt2(q,i)
end do 

term(98) = term(98) * (4.0d+0) 
term(99) = term(99) * (-2.0d+0) 
term(100) = term(100) * (2.0d+0) 
term(101) = term(101) * (-4.0d+0) 
term(102) = term(102) * (-4.0d+0) 
term(103) = term(103) * (2.0d+0) 
term(104) = term(104) * (2.0d+0) 
term(105) = term(105) * (-4.0d+0) 
term(106) = term(106) * (-4.0d+0) 
term(107) = term(107) * (2.0d+0) 
term(108) = term(108) * (-4.0d+0) 
term(109) = term(109) * (8.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
do a = nocc + 1, nactive 
term(110) = term(110) + r2(vrdav_Rl, a,k,q,j) * r2(vrdav_Rr, a,l,p,i) * wm_interm_14_pt2(i,l,k,j)
term(111) = term(111) + r2(vrdav_Rl, a,k,q,j) * r2(vrdav_Rr, a,l,p,i) * wm_interm_14_pt2(l,i,k,j)
term(112) = term(112) + s2(a,p,k,j) * t2(a,q,l,i) * wm_interm_50_pt2(l,i,k,j)
term(113) = term(113) + s2(a,p,k,j) * t2(a,q,l,i) * wm_interm_50_pt2(i,l,k,j)
end do 
end do 
end do 
end do 
end do 

term(110) = term(110) * (-1.0d+0) 
term(111) = term(111) * (2.0d+0) 
term(112) = term(112) * (2.0d+0) 
term(113) = term(113) * (-1.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
term(114) = term(114) + wm_interm_17_pt2(p,q,i,j) * wm_interm_44_pt2(j,i)
term(115) = term(115) + wm_interm_19_pt2(p,q,i,j) * wm_interm_44_pt2(j,i)
term(116) = term(116) + wm_interm_18_pt2(p,q,i,j) * wm_interm_44_pt2(j,i)
term(117) = term(117) + wm_interm_20_pt2(p,q,i,j) * wm_interm_44_pt2(j,i)
term(118) = term(118) + wm_interm_37_pt2(i,j) * wm_interm_8_pt2(p,q,i,j)
term(119) = term(119) + wm_interm_37_pt2(i,j) * wm_interm_9_pt2(p,q,i,j)
term(120) = term(120) + wm_interm_37_pt2(i,j) * wm_interm_6_pt2(p,q,i,j)
term(121) = term(121) + wm_interm_37_pt2(i,j) * wm_interm_5_pt2(p,q,i,j)
term(122) = term(122) + wm_interm_39_pt2(i,j) * wm_interm_8_pt2(p,q,i,j)
term(123) = term(123) + wm_interm_39_pt2(i,j) * wm_interm_9_pt2(p,q,i,j)
term(124) = term(124) + wm_interm_39_pt2(i,j) * wm_interm_6_pt2(p,q,i,j)
term(125) = term(125) + wm_interm_39_pt2(i,j) * wm_interm_5_pt2(p,q,i,j)
term(126) = term(126) + wm_interm_17_pt2(p,q,i,j) * wm_interm_51_pt2(j,i)
term(127) = term(127) + wm_interm_17_pt2(p,q,i,j) * wm_interm_52_pt2(j,i)
term(128) = term(128) + wm_interm_19_pt2(p,q,i,j) * wm_interm_51_pt2(j,i)
term(129) = term(129) + wm_interm_19_pt2(p,q,i,j) * wm_interm_52_pt2(j,i)
term(130) = term(130) + wm_interm_18_pt2(p,q,i,j) * wm_interm_51_pt2(j,i)
term(131) = term(131) + wm_interm_18_pt2(p,q,i,j) * wm_interm_52_pt2(j,i)
term(132) = term(132) + wm_interm_20_pt2(p,q,i,j) * wm_interm_51_pt2(j,i)
term(133) = term(133) + wm_interm_20_pt2(p,q,i,j) * wm_interm_52_pt2(j,i)
term(134) = term(134) + wm_interm_22_pt2(i,j) * wm_interm_36_pt2(p,q,i,j)
term(135) = term(135) + wm_interm_22_pt2(i,j) * wm_interm_33_pt2(p,q,i,j)
term(136) = term(136) + wm_interm_23_pt2(i,j) * wm_interm_36_pt2(p,q,i,j)
term(137) = term(137) + wm_interm_23_pt2(i,j) * wm_interm_33_pt2(p,q,i,j)
term(138) = term(138) + wm_interm_22_pt2(i,j) * wm_interm_32_pt2(p,q,i,j)
term(139) = term(139) + wm_interm_22_pt2(i,j) * wm_interm_28_pt2(p,q,i,j)
term(140) = term(140) + wm_interm_23_pt2(i,j) * wm_interm_32_pt2(p,q,i,j)
term(141) = term(141) + wm_interm_23_pt2(i,j) * wm_interm_28_pt2(p,q,i,j)
end do 
end do 

term(114) = term(114) * (2.0d+0) 
term(115) = term(115) * (2.0d+0) 
term(116) = term(116) * (-4.0d+0) 
term(117) = term(117) * (-4.0d+0) 
term(118) = term(118) * (-2.0d+0) 
term(119) = term(119) * (4.0d+0) 
term(120) = term(120) * (-2.0d+0) 
term(121) = term(121) * (4.0d+0) 
term(122) = term(122) * (4.0d+0) 
term(123) = term(123) * (-8.0d+0) 
term(124) = term(124) * (4.0d+0) 
term(125) = term(125) * (-8.0d+0) 
term(126) = term(126) * (-2.0d+0) 
term(127) = term(127) * (4.0d+0) 
term(128) = term(128) * (-2.0d+0) 
term(129) = term(129) * (4.0d+0) 
term(130) = term(130) * (4.0d+0) 
term(131) = term(131) * (-8.0d+0) 
term(132) = term(132) * (4.0d+0) 
term(133) = term(133) * (-8.0d+0) 
term(134) = term(134) * (-2.0d+0) 
term(135) = term(135) * (4.0d+0) 
term(136) = term(136) * (4.0d+0) 
term(137) = term(137) * (-8.0d+0) 
term(138) = term(138) * (-2.0d+0) 
term(139) = term(139) * (4.0d+0) 
term(140) = term(140) * (4.0d+0) 
term(141) = term(141) * (-8.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(142) = term(142) + r2(vrdav_Rl, a,k,p,i) * t2(a,q,l,j) * wm_interm_21_pt2(k,i,l,j)
term(143) = term(143) + r2(vrdav_Rl, a,k,p,i) * t2(a,q,l,j) * wm_interm_21_pt2(i,k,l,j)
end do 
end do 
end do 
end do 
end do 

term(142) = term(142) * (2.0d+0) 
term(143) = term(143) * (-1.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(144) = term(144) + r2(vrdav_Rl, a,i,q,j) * r2(vrdav_Rr, b,j,p,i) * wm_interm_3_pt2(b,a)
term(145) = term(145) + r2(vrdav_Rl, a,i,q,j) * r2(vrdav_Rr, b,j,p,i) * wm_interm_7_pt2(b,a)
term(146) = term(146) + s2(a,p,i,j) * t2(b,q,j,i) * wm_interm_57_pt2(b,a)
term(147) = term(147) + s2(a,p,i,j) * t2(b,q,j,i) * wm_interm_58_pt2(b,a)
end do 
end do 
end do 
end do 

term(144) = term(144) * (-2.0d+0) 
term(145) = term(145) * (4.0d+0) 
term(146) = term(146) * (-2.0d+0) 
term(147) = term(147) * (4.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
do j = 1, nocc 
do b = nocc + 1, nactive 
term(148) = term(148) + r2(vrdav_Rl, a,j,q,i) * r2(vrdav_Rr, b,j,p,i) * wm_interm_3_pt2(b,a)
term(149) = term(149) + r2(vrdav_Rl, a,j,q,i) * r2(vrdav_Rr, b,j,p,i) * wm_interm_7_pt2(b,a)
term(150) = term(150) + s2(a,p,j,i) * t2(b,q,j,i) * wm_interm_57_pt2(b,a)
term(151) = term(151) + s2(a,p,j,i) * t2(b,q,j,i) * wm_interm_58_pt2(b,a)
end do 
end do 
end do 
end do 

term(148) = term(148) * (4.0d+0) 
term(149) = term(149) * (-8.0d+0) 
term(150) = term(150) * (4.0d+0) 
term(151) = term(151) * (-8.0d+0) 

do a = nocc + 1, nactive 
term(152) = term(152) + wm_interm_3_pt2(q,a) * wm_interm_49_pt2(a,p)
term(153) = term(153) + wm_interm_49_pt2(a,p) * wm_interm_7_pt2(q,a)
term(154) = term(154) + wm_interm_3_pt2(a,q) * wm_interm_49_pt2(p,a)
term(155) = term(155) + wm_interm_49_pt2(p,a) * wm_interm_7_pt2(a,q)
term(156) = term(156) + wm_interm_3_pt2(q,a) * wm_interm_57_pt2(a,p)
term(157) = term(157) + wm_interm_57_pt2(a,p) * wm_interm_7_pt2(q,a)
term(158) = term(158) + wm_interm_3_pt2(q,a) * wm_interm_58_pt2(a,p)
term(159) = term(159) + wm_interm_58_pt2(a,p) * wm_interm_7_pt2(q,a)
term(160) = term(160) + wm_interm_15_pt2(p,a) * wm_interm_34_pt2(q,a)
term(161) = term(161) + wm_interm_16_pt2(p,a) * wm_interm_34_pt2(q,a)
term(162) = term(162) + wm_interm_15_pt2(p,a) * wm_interm_35_pt2(q,a)
term(163) = term(163) + wm_interm_16_pt2(p,a) * wm_interm_35_pt2(q,a)
term(164) = term(164) + wm_interm_3_pt2(a,q) * wm_interm_57_pt2(p,a)
term(165) = term(165) + wm_interm_3_pt2(a,q) * wm_interm_58_pt2(p,a)
term(166) = term(166) + wm_interm_57_pt2(p,a) * wm_interm_7_pt2(a,q)
term(167) = term(167) + wm_interm_58_pt2(p,a) * wm_interm_7_pt2(a,q)
term(168) = term(168) + wm_interm_15_pt2(a,p) * wm_interm_35_pt2(a,q)
term(169) = term(169) + wm_interm_16_pt2(a,p) * wm_interm_35_pt2(a,q)
term(170) = term(170) + wm_interm_15_pt2(a,p) * wm_interm_34_pt2(a,q)
term(171) = term(171) + wm_interm_16_pt2(a,p) * wm_interm_34_pt2(a,q)
end do 

term(152) = term(152) * (2.0d+0) 
term(153) = term(153) * (-4.0d+0) 
term(154) = term(154) * (2.0d+0) 
term(155) = term(155) * (-4.0d+0) 
term(156) = term(156) * (-2.0d+0) 
term(157) = term(157) * (4.0d+0) 
term(158) = term(158) * (4.0d+0) 
term(159) = term(159) * (-8.0d+0) 
term(160) = term(160) * (4.0d+0) 
term(161) = term(161) * (-8.0d+0) 
term(162) = term(162) * (-2.0d+0) 
term(163) = term(163) * (4.0d+0) 
term(164) = term(164) * (-2.0d+0) 
term(165) = term(165) * (4.0d+0) 
term(166) = term(166) * (4.0d+0) 
term(167) = term(167) * (-8.0d+0) 
term(168) = term(168) * (-2.0d+0) 
term(169) = term(169) * (4.0d+0) 
term(170) = term(170) * (4.0d+0) 
term(171) = term(171) * (-8.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(172) = term(172) + r2(vrdav_Rr, a,j,p,i) * s2(b,q,i,j) * wm_interm_34_pt2(a,b)
term(173) = term(173) + r2(vrdav_Rr, a,j,p,i) * s2(b,q,i,j) * wm_interm_35_pt2(a,b)
term(174) = term(174) + r2(vrdav_Rl, a,j,p,i) * t2(b,q,i,j) * wm_interm_15_pt2(a,b)
term(175) = term(175) + r2(vrdav_Rl, a,j,p,i) * t2(b,q,i,j) * wm_interm_16_pt2(a,b)
end do 
end do 
end do 
end do 

term(172) = term(172) * (4.0d+0) 
term(173) = term(173) * (-2.0d+0) 
term(174) = term(174) * (-2.0d+0) 
term(175) = term(175) * (4.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(176) = term(176) + r2(vrdav_Rr, a,j,p,i) * s2(b,q,j,i) * wm_interm_34_pt2(a,b)
term(177) = term(177) + r2(vrdav_Rr, a,j,p,i) * s2(b,q,j,i) * wm_interm_35_pt2(a,b)
term(178) = term(178) + r2(vrdav_Rl, a,j,p,i) * t2(b,q,j,i) * wm_interm_15_pt2(a,b)
term(179) = term(179) + r2(vrdav_Rl, a,j,p,i) * t2(b,q,j,i) * wm_interm_16_pt2(a,b)
end do 
end do 
end do 
end do 

term(176) = term(176) * (-8.0d+0) 
term(177) = term(177) * (4.0d+0) 
term(178) = term(178) * (4.0d+0) 
term(179) = term(179) * (-8.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(180) = term(180) + r2(vrdav_Rr, a,j,p,i) * s2(a,q,l,k) * wm_interm_38_pt2(i,j,l,k)
term(181) = term(181) + r2(vrdav_Rr, a,j,p,i) * s2(a,q,l,k) * wm_interm_38_pt2(j,i,l,k)
end do 
end do 
end do 
end do 
end do 

term(180) = term(180) * (-1.0d+0) 
term(181) = term(181) * (2.0d+0) 


    calc_D_vv_wm_pt2 = zero
    do s = 0, 181
    calc_D_vv_wm_pt2 = calc_D_vv_wm_pt2 + term(s)
    end do

    end function calc_D_vv_wm_pt2
    
    function calc_D_oo_wm_pt3(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_oo_wm_pt3
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
    real(F64), dimension(0:57) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do i = 1, nocc 
term(0) = term(0) + wm_interm_11_pt3(p,i) * wm_interm_44_pt3(q,i)
term(1) = term(1) + wm_interm_12_pt3(p,i) * wm_interm_44_pt3(q,i)
term(2) = term(2) + wm_interm_11_pt3(i,p) * wm_interm_44_pt3(i,q)
term(3) = term(3) + wm_interm_12_pt3(i,p) * wm_interm_44_pt3(i,q)
term(4) = term(4) + wm_interm_25_pt3(q,i) * wm_interm_54_pt3(p,i)
term(5) = term(5) + wm_interm_28_pt3(q,i) * wm_interm_54_pt3(p,i)
term(6) = term(6) + wm_interm_25_pt3(i,q) * wm_interm_54_pt3(i,p)
term(7) = term(7) + wm_interm_28_pt3(i,q) * wm_interm_54_pt3(i,p)
end do 

term(0) = term(0) * (-2.0d+0) 
term(1) = term(1) * (4.0d+0) 
term(2) = term(2) * (-2.0d+0) 
term(3) = term(3) * (4.0d+0) 
term(4) = term(4) * (-2.0d+0) 
term(5) = term(5) * (4.0d+0) 
term(6) = term(6) * (-2.0d+0) 
term(7) = term(7) * (4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
term(8) = term(8) + wm_interm_13_pt3(i,p,q,j) * wm_interm_44_pt3(i,j)
term(9) = term(9) + wm_interm_13_pt3(i,p,j,q) * wm_interm_44_pt3(i,j)
term(10) = term(10) + wm_interm_11_pt3(i,j) * wm_interm_44_pt3(i,j)
term(11) = term(11) + wm_interm_12_pt3(i,j) * wm_interm_44_pt3(i,j)
term(12) = term(12) + wm_interm_25_pt3(i,j) * wm_interm_54_pt3(i,j)
term(13) = term(13) + wm_interm_28_pt3(i,j) * wm_interm_54_pt3(i,j)
term(14) = term(14) + wm_interm_38_pt3(i,p,q,j) * wm_interm_54_pt3(i,j)
term(15) = term(15) + wm_interm_38_pt3(p,i,q,j) * wm_interm_54_pt3(i,j)
term(16) = term(16) + wm_interm_38_pt3(p,i,j,q) * wm_interm_54_pt3(i,j)
term(17) = term(17) + wm_interm_38_pt3(i,p,j,q) * wm_interm_54_pt3(i,j)
end do 
end do 

term(8) = term(8) * (-2.0d+0) 
term(9) = term(9) * (4.0d+0) 
term(10) = term(10) * (4.0d+0) 
term(11) = term(11) * (-8.0d+0) 
term(12) = term(12) * (4.0d+0) 
term(13) = term(13) * (-8.0d+0) 
term(14) = term(14) * (-1.0d+0) 
term(15) = term(15) * (2.0d+0) 
term(16) = term(16) * (-1.0d+0) 
term(17) = term(17) * (2.0d+0) 

do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(18) = term(18) + wm_interm_14_pt3(a,b,p,q) * wm_interm_42_pt3(a,b)
term(19) = term(19) + wm_interm_15_pt3(a,b,p,q) * wm_interm_42_pt3(a,b)
term(20) = term(20) + wm_interm_16_pt3(a,b,p,q) * wm_interm_42_pt3(a,b)
term(21) = term(21) + wm_interm_17_pt3(a,b,p,q) * wm_interm_42_pt3(a,b)
term(22) = term(22) + wm_interm_3_pt3(a,b) * wm_interm_42_pt3(a,b)
term(23) = term(23) + wm_interm_42_pt3(a,b) * wm_interm_4_pt3(a,b)
term(24) = term(24) + wm_interm_3_pt3(a,b) * wm_interm_47_pt3(a,b,p,q)
term(25) = term(25) + wm_interm_47_pt3(a,b,p,q) * wm_interm_4_pt3(a,b)
term(26) = term(26) + wm_interm_23_pt3(a,b) * wm_interm_50_pt3(a,b,p,q)
term(27) = term(27) + wm_interm_23_pt3(a,b) * wm_interm_53_pt3(a,b)
term(28) = term(28) + wm_interm_24_pt3(a,b) * wm_interm_50_pt3(a,b,p,q)
term(29) = term(29) + wm_interm_24_pt3(a,b) * wm_interm_53_pt3(a,b)
term(30) = term(30) + wm_interm_36_pt3(a,b,p,q) * wm_interm_53_pt3(a,b)
term(31) = term(31) + wm_interm_35_pt3(a,b,p,q) * wm_interm_53_pt3(a,b)
term(32) = term(32) + wm_interm_33_pt3(a,b,p,q) * wm_interm_53_pt3(a,b)
term(33) = term(33) + wm_interm_34_pt3(a,b,p,q) * wm_interm_53_pt3(a,b)
end do 
end do 

term(18) = term(18) * (-2.0d+0) 
term(19) = term(19) * (4.0d+0) 
term(20) = term(20) * (-2.0d+0) 
term(21) = term(21) * (4.0d+0) 
term(22) = term(22) * (4.0d+0) 
term(23) = term(23) * (-8.0d+0) 
term(24) = term(24) * (-2.0d+0) 
term(25) = term(25) * (4.0d+0) 
term(26) = term(26) * (4.0d+0) 
term(27) = term(27) * (-8.0d+0) 
term(28) = term(28) * (-2.0d+0) 
term(29) = term(29) * (4.0d+0) 
term(30) = term(30) * (-2.0d+0) 
term(31) = term(31) * (4.0d+0) 
term(32) = term(32) * (-2.0d+0) 
term(33) = term(33) * (4.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(34) = term(34) + wm_interm_1_pt3(a,p,i,q) * wm_interm_45_pt3(a,i)
term(35) = term(35) + wm_interm_1_pt3(a,p,i,q) * wm_interm_46_pt3(a,i)
term(36) = term(36) + wm_interm_1_pt3(a,i,p,q) * wm_interm_45_pt3(a,i)
term(37) = term(37) + wm_interm_1_pt3(a,i,p,q) * wm_interm_46_pt3(a,i)
term(38) = term(38) + wm_interm_21_pt3(a,i) * wm_interm_48_pt3(a,q,i,p)
term(39) = term(39) + wm_interm_21_pt3(a,i) * wm_interm_49_pt3(a,i)
term(40) = term(40) + wm_interm_21_pt3(a,i) * wm_interm_48_pt3(a,i,q,p)
term(41) = term(41) + wm_interm_21_pt3(a,i) * wm_interm_51_pt3(a,i)
term(42) = term(42) + wm_interm_21_pt3(a,i) * wm_interm_52_pt3(a,q,i,p)
term(43) = term(43) + wm_interm_21_pt3(a,i) * wm_interm_52_pt3(a,i,q,p)
term(44) = term(44) + wm_interm_22_pt3(a,i) * wm_interm_52_pt3(a,q,i,p)
term(45) = term(45) + wm_interm_22_pt3(a,i) * wm_interm_51_pt3(a,i)
term(46) = term(46) + wm_interm_22_pt3(a,i) * wm_interm_48_pt3(a,q,i,p)
term(47) = term(47) + wm_interm_22_pt3(a,i) * wm_interm_49_pt3(a,i)
term(48) = term(48) + wm_interm_22_pt3(a,i) * wm_interm_48_pt3(a,i,q,p)
term(49) = term(49) + wm_interm_22_pt3(a,i) * wm_interm_52_pt3(a,i,q,p)
term(50) = term(50) + wm_interm_37_pt3(a,p,q,i) * wm_interm_49_pt3(a,i)
term(51) = term(51) + wm_interm_37_pt3(a,p,q,i) * wm_interm_51_pt3(a,i)
term(52) = term(52) + wm_interm_37_pt3(a,p,i,q) * wm_interm_51_pt3(a,i)
term(53) = term(53) + wm_interm_37_pt3(a,p,i,q) * wm_interm_49_pt3(a,i)
end do 
end do 

term(34) = term(34) * (-2.0d+0) 
term(35) = term(35) * (4.0d+0) 
term(36) = term(36) * (4.0d+0) 
term(37) = term(37) * (-8.0d+0) 
term(38) = term(38) * (-4.0d+0) 
term(39) = term(39) * (16.0d+0) 
term(40) = term(40) * (2.0d+0) 
term(41) = term(41) * (-8.0d+0) 
term(42) = term(42) * (2.0d+0) 
term(43) = term(43) * (-4.0d+0) 
term(44) = term(44) * (-1.0d+0) 
term(45) = term(45) * (4.0d+0) 
term(46) = term(46) * (2.0d+0) 
term(47) = term(47) * (-8.0d+0) 
term(48) = term(48) * (-1.0d+0) 
term(49) = term(49) * (2.0d+0) 
term(50) = term(50) * (4.0d+0) 
term(51) = term(51) * (-2.0d+0) 
term(52) = term(52) * (4.0d+0) 
term(53) = term(53) * (-8.0d+0) 

do a = nocc + 1, nactive 
term(54) = term(54) + wm_interm_21_pt3(a,q) * wm_interm_51_pt3(a,p)
term(55) = term(55) + wm_interm_21_pt3(a,q) * wm_interm_49_pt3(a,p)
term(56) = term(56) + wm_interm_22_pt3(a,q) * wm_interm_51_pt3(a,p)
term(57) = term(57) + wm_interm_22_pt3(a,q) * wm_interm_49_pt3(a,p)
end do 

term(54) = term(54) * (4.0d+0) 
term(55) = term(55) * (-8.0d+0) 
term(56) = term(56) * (-2.0d+0) 
term(57) = term(57) * (4.0d+0) 


    calc_D_oo_wm_pt3 = zero
    do s = 0, 57
    calc_D_oo_wm_pt3 = calc_D_oo_wm_pt3 + term(s)
    end do

    end function calc_D_oo_wm_pt3
    
    function calc_D_ov_wm_pt3(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_ov_wm_pt3
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , j, i, a, k, l, b 
    real(F64), dimension(0:153) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + wm_interm_0_pt3(q,a,i,j) * wm_interm_1_pt3(a,j,p,i)
term(1) = term(1) + wm_interm_1_pt3(a,i,p,j) * wm_interm_2_pt3(q,a,j,i)
term(2) = term(2) + wm_interm_1_pt3(a,p,i,j) * wm_interm_2_pt3(q,a,j,i)
term(3) = term(3) + wm_interm_0_pt3(q,a,i,j) * wm_interm_1_pt3(a,p,j,i)
term(4) = term(4) + wm_interm_1_pt3(a,p,i,j) * wm_interm_6_pt3(q,a,j,i)
term(5) = term(5) + wm_interm_1_pt3(a,p,i,j) * wm_interm_7_pt3(q,a,j,i)
term(6) = term(6) + wm_interm_1_pt3(a,i,p,j) * wm_interm_6_pt3(q,a,j,i)
term(7) = term(7) + wm_interm_1_pt3(a,i,p,j) * wm_interm_7_pt3(q,a,j,i)
term(8) = term(8) + wm_interm_26_pt3(a,p,i,j) * wm_interm_36_pt3(a,q,i,j)
term(9) = term(9) + wm_interm_26_pt3(a,i,p,j) * wm_interm_36_pt3(a,q,i,j)
term(10) = term(10) + wm_interm_26_pt3(a,i,p,j) * wm_interm_35_pt3(a,q,i,j)
term(11) = term(11) + wm_interm_26_pt3(a,p,i,j) * wm_interm_35_pt3(a,q,i,j)
term(12) = term(12) + wm_interm_26_pt3(a,p,i,j) * wm_interm_33_pt3(a,q,i,j)
term(13) = term(13) + wm_interm_26_pt3(a,i,p,j) * wm_interm_33_pt3(a,q,i,j)
term(14) = term(14) + wm_interm_26_pt3(a,p,i,j) * wm_interm_34_pt3(a,q,i,j)
term(15) = term(15) + wm_interm_26_pt3(a,i,p,j) * wm_interm_34_pt3(a,q,i,j)
term(16) = term(16) + r1(vrdav_Rr, a,p) * wm_interm_36_pt3(a,q,i,j) * wm_interm_69_pt3(j,i)
term(17) = term(17) + r1(vrdav_Rr, a,p) * wm_interm_34_pt3(a,q,i,j) * wm_interm_69_pt3(j,i)
term(18) = term(18) + r1(vrdav_Rr, a,p) * wm_interm_36_pt3(a,q,i,j) * wm_interm_70_pt3(j,i)
term(19) = term(19) + r1(vrdav_Rr, a,p) * wm_interm_34_pt3(a,q,i,j) * wm_interm_70_pt3(j,i)
term(20) = term(20) + r1(vrdav_Rr, a,p) * wm_interm_33_pt3(a,q,i,j) * wm_interm_69_pt3(j,i)
term(21) = term(21) + r1(vrdav_Rr, a,p) * wm_interm_33_pt3(a,q,i,j) * wm_interm_70_pt3(j,i)
term(22) = term(22) + r1(vrdav_Rr, a,p) * wm_interm_35_pt3(a,q,i,j) * wm_interm_69_pt3(j,i)
term(23) = term(23) + r1(vrdav_Rr, a,p) * wm_interm_35_pt3(a,q,i,j) * wm_interm_70_pt3(j,i)
end do 
end do 
end do 

term(0) = term(0) * (-2.0d+0) 
term(1) = term(1) * (4.0d+0) 
term(2) = term(2) * (-2.0d+0) 
term(3) = term(3) * (4.0d+0) 
term(4) = term(4) * (-2.0d+0) 
term(5) = term(5) * (4.0d+0) 
term(6) = term(6) * (4.0d+0) 
term(7) = term(7) * (-8.0d+0) 
term(8) = term(8) * (-2.0d+0) 
term(9) = term(9) * (4.0d+0) 
term(10) = term(10) * (-2.0d+0) 
term(11) = term(11) * (4.0d+0) 
term(12) = term(12) * (-2.0d+0) 
term(13) = term(13) * (4.0d+0) 
term(14) = term(14) * (4.0d+0) 
term(15) = term(15) * (-8.0d+0) 
term(16) = term(16) * (-2.0d+0) 
term(17) = term(17) * (4.0d+0) 
term(18) = term(18) * (4.0d+0) 
term(19) = term(19) * (-8.0d+0) 
term(20) = term(20) * (-2.0d+0) 
term(21) = term(21) * (4.0d+0) 
term(22) = term(22) * (4.0d+0) 
term(23) = term(23) * (-8.0d+0) 

do l = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
term(24) = term(24) + r1(vrdav_Rl, q,i) * wm_interm_13_pt3(j,k,l,p) * wm_interm_65_pt3(j,k,i,l)
term(25) = term(25) + r1(vrdav_Rl, q,i) * wm_interm_13_pt3(j,k,l,p) * wm_interm_65_pt3(k,j,i,l)
term(26) = term(26) + r1(vrdav_Rl, q,i) * wm_interm_13_pt3(j,k,p,l) * wm_interm_65_pt3(k,j,i,l)
term(27) = term(27) + r1(vrdav_Rl, q,i) * wm_interm_13_pt3(j,k,p,l) * wm_interm_65_pt3(j,k,i,l)
end do 
end do 
end do 
end do 

term(24) = term(24) * (2.0d+0) 
term(25) = term(25) * (-4.0d+0) 
term(26) = term(26) * (2.0d+0) 
term(27) = term(27) * (-4.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(28) = term(28) + t2(a,q,j,i) * wm_interm_48_pt3(b,j,i,p) * wm_interm_71_pt3(a,b)
term(29) = term(29) + t2(a,q,j,i) * wm_interm_48_pt3(b,j,i,p) * wm_interm_72_pt3(a,b)
term(30) = term(30) + t2(a,q,j,i) * wm_interm_52_pt3(b,j,i,p) * wm_interm_71_pt3(a,b)
term(31) = term(31) + t2(a,q,j,i) * wm_interm_52_pt3(b,j,i,p) * wm_interm_72_pt3(a,b)
end do 
end do 
end do 
end do 

term(28) = term(28) * (-1.0d+0) 
term(29) = term(29) * (2.0d+0) 
term(30) = term(30) * (2.0d+0) 
term(31) = term(31) * (-4.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(32) = term(32) + r1(vrdav_Rr, a,p) * wm_interm_25_pt3(i,j) * wm_interm_67_pt3(a,q,j,i)
term(33) = term(33) + r1(vrdav_Rr, a,p) * wm_interm_25_pt3(i,j) * wm_interm_66_pt3(a,q,j,i)
term(34) = term(34) + r1(vrdav_Rr, a,p) * wm_interm_25_pt3(i,j) * wm_interm_64_pt3(a,q,j,i)
term(35) = term(35) + r1(vrdav_Rr, a,p) * wm_interm_25_pt3(i,j) * wm_interm_68_pt3(a,q,j,i)
term(36) = term(36) + r1(vrdav_Rr, a,p) * wm_interm_28_pt3(i,j) * wm_interm_67_pt3(a,q,j,i)
term(37) = term(37) + r1(vrdav_Rr, a,p) * wm_interm_28_pt3(i,j) * wm_interm_66_pt3(a,q,j,i)
term(38) = term(38) + r1(vrdav_Rr, a,p) * wm_interm_28_pt3(i,j) * wm_interm_64_pt3(a,q,j,i)
term(39) = term(39) + r1(vrdav_Rr, a,p) * wm_interm_28_pt3(i,j) * wm_interm_68_pt3(a,q,j,i)
end do 
end do 
end do 

term(32) = term(32) * (-2.0d+0) 
term(33) = term(33) * (4.0d+0) 
term(34) = term(34) * (-2.0d+0) 
term(35) = term(35) * (4.0d+0) 
term(36) = term(36) * (4.0d+0) 
term(37) = term(37) * (-8.0d+0) 
term(38) = term(38) * (4.0d+0) 
term(39) = term(39) * (-8.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(40) = term(40) + wm_interm_10_pt3(q,i,j,k) * wm_interm_13_pt3(i,j,k,p)
term(41) = term(41) + wm_interm_10_pt3(q,i,j,k) * wm_interm_13_pt3(i,j,p,k)
term(42) = term(42) + wm_interm_13_pt3(i,j,p,k) * wm_interm_20_pt3(q,i,j,k)
term(43) = term(43) + wm_interm_13_pt3(i,j,k,p) * wm_interm_20_pt3(q,i,j,k)
term(44) = term(44) + wm_interm_39_pt3(q,i,j,k) * wm_interm_5_pt3(j,k,i,p)
term(45) = term(45) + wm_interm_39_pt3(q,i,j,k) * wm_interm_5_pt3(k,j,i,p)
end do 
end do 
end do 

term(40) = term(40) * (-1.0d+0) 
term(41) = term(41) * (2.0d+0) 
term(42) = term(42) * (-1.0d+0) 
term(43) = term(43) * (2.0d+0) 
term(44) = term(44) * (2.0d+0) 
term(45) = term(45) * (-1.0d+0) 

do l = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(46) = term(46) + t2(a,q,j,i) * wm_interm_52_pt3(a,k,l,p) * wm_interm_65_pt3(i,j,k,l)
term(47) = term(47) + t2(a,q,j,i) * wm_interm_52_pt3(a,k,l,p) * wm_interm_65_pt3(j,i,k,l)
term(48) = term(48) + t2(a,q,j,i) * wm_interm_48_pt3(a,k,l,p) * wm_interm_65_pt3(j,i,k,l)
term(49) = term(49) + t2(a,q,j,i) * wm_interm_48_pt3(a,k,l,p) * wm_interm_65_pt3(i,j,k,l)
term(50) = term(50) + t2(a,q,j,i) * wm_interm_38_pt3(j,i,k,l) * wm_interm_58_pt3(a,p,k,l)
term(51) = term(51) + t2(a,q,j,i) * wm_interm_38_pt3(i,j,k,l) * wm_interm_58_pt3(a,p,k,l)
end do 
end do 
end do 
end do 
end do 

term(46) = term(46) * (-1.0d+0) 
term(47) = term(47) * (2.0d+0) 
term(48) = term(48) * (-1.0d+0) 
term(49) = term(49) * (2.0d+0) 
term(50) = term(50) * (-1.0d+0) 
term(51) = term(51) * (2.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(52) = term(52) + wm_interm_39_pt3(q,i,j,k) * wm_interm_5_pt3(j,k,p,i)
term(53) = term(53) + wm_interm_39_pt3(q,i,j,k) * wm_interm_5_pt3(k,j,p,i)
term(54) = term(54) + r1(vrdav_Rl, q,i) * wm_interm_11_pt3(j,k) * wm_interm_65_pt3(j,p,i,k)
term(55) = term(55) + r1(vrdav_Rl, q,i) * wm_interm_12_pt3(j,k) * wm_interm_65_pt3(j,p,i,k)
term(56) = term(56) + r1(vrdav_Rl, q,i) * wm_interm_11_pt3(j,k) * wm_interm_65_pt3(p,j,i,k)
term(57) = term(57) + r1(vrdav_Rl, q,i) * wm_interm_12_pt3(j,k) * wm_interm_65_pt3(p,j,i,k)
end do 
end do 
end do 

term(52) = term(52) * (-1.0d+0) 
term(53) = term(53) * (2.0d+0) 
term(54) = term(54) * (4.0d+0) 
term(55) = term(55) * (-8.0d+0) 
term(56) = term(56) * (-8.0d+0) 
term(57) = term(57) * (16.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(58) = term(58) + r1(vrdav_Rr, a,p) * wm_interm_36_pt3(b,q,i,j) * wm_interm_67_pt3(a,b,j,i)
term(59) = term(59) + r1(vrdav_Rr, a,p) * wm_interm_34_pt3(b,q,i,j) * wm_interm_67_pt3(a,b,j,i)
term(60) = term(60) + r1(vrdav_Rr, a,p) * wm_interm_36_pt3(b,q,i,j) * wm_interm_68_pt3(a,b,j,i)
term(61) = term(61) + r1(vrdav_Rr, a,p) * wm_interm_34_pt3(b,q,i,j) * wm_interm_68_pt3(a,b,j,i)
term(62) = term(62) + r1(vrdav_Rr, a,p) * wm_interm_34_pt3(a,b,i,j) * wm_interm_66_pt3(b,q,j,i)
term(63) = term(63) + r1(vrdav_Rr, a,p) * wm_interm_34_pt3(a,b,i,j) * wm_interm_64_pt3(b,q,j,i)
term(64) = term(64) + r1(vrdav_Rr, a,p) * wm_interm_34_pt3(a,b,i,j) * wm_interm_67_pt3(b,q,j,i)
term(65) = term(65) + r1(vrdav_Rr, a,p) * wm_interm_34_pt3(a,b,i,j) * wm_interm_68_pt3(b,q,j,i)
term(66) = term(66) + r1(vrdav_Rr, a,p) * wm_interm_35_pt3(b,q,i,j) * wm_interm_67_pt3(a,b,j,i)
term(67) = term(67) + r1(vrdav_Rr, a,p) * wm_interm_33_pt3(b,q,i,j) * wm_interm_67_pt3(a,b,j,i)
term(68) = term(68) + r1(vrdav_Rr, a,p) * wm_interm_35_pt3(b,q,i,j) * wm_interm_68_pt3(a,b,j,i)
term(69) = term(69) + r1(vrdav_Rr, a,p) * wm_interm_33_pt3(b,q,i,j) * wm_interm_68_pt3(a,b,j,i)
term(70) = term(70) + r1(vrdav_Rr, a,p) * wm_interm_33_pt3(a,b,i,j) * wm_interm_66_pt3(b,q,j,i)
term(71) = term(71) + r1(vrdav_Rr, a,p) * wm_interm_33_pt3(a,b,i,j) * wm_interm_64_pt3(b,q,j,i)
term(72) = term(72) + r1(vrdav_Rr, a,p) * wm_interm_33_pt3(a,b,i,j) * wm_interm_67_pt3(b,q,j,i)
term(73) = term(73) + r1(vrdav_Rr, a,p) * wm_interm_33_pt3(a,b,i,j) * wm_interm_68_pt3(b,q,j,i)
term(74) = term(74) + r1(vrdav_Rr, a,p) * wm_interm_35_pt3(b,q,i,j) * wm_interm_64_pt3(a,b,j,i)
term(75) = term(75) + r1(vrdav_Rr, a,p) * wm_interm_33_pt3(b,q,i,j) * wm_interm_64_pt3(a,b,j,i)
term(76) = term(76) + r1(vrdav_Rr, a,p) * wm_interm_35_pt3(a,b,i,j) * wm_interm_64_pt3(b,q,j,i)
term(77) = term(77) + r1(vrdav_Rr, a,p) * wm_interm_35_pt3(a,b,i,j) * wm_interm_68_pt3(b,q,j,i)
term(78) = term(78) + r1(vrdav_Rr, a,p) * wm_interm_33_pt3(b,q,i,j) * wm_interm_66_pt3(a,b,j,i)
term(79) = term(79) + r1(vrdav_Rr, a,p) * wm_interm_35_pt3(b,q,i,j) * wm_interm_66_pt3(a,b,j,i)
term(80) = term(80) + t2(a,q,j,i) * wm_interm_23_pt3(a,b) * wm_interm_58_pt3(b,p,i,j)
term(81) = term(81) + r1(vrdav_Rr, a,p) * wm_interm_35_pt3(a,b,i,j) * wm_interm_67_pt3(b,q,j,i)
term(82) = term(82) + r1(vrdav_Rr, a,p) * wm_interm_35_pt3(a,b,i,j) * wm_interm_66_pt3(b,q,j,i)
term(83) = term(83) + t2(a,q,j,i) * wm_interm_24_pt3(a,b) * wm_interm_58_pt3(b,p,i,j)
term(84) = term(84) + r1(vrdav_Rr, a,p) * wm_interm_36_pt3(b,q,i,j) * wm_interm_66_pt3(a,b,j,i)
term(85) = term(85) + r1(vrdav_Rr, a,p) * wm_interm_34_pt3(b,q,i,j) * wm_interm_66_pt3(a,b,j,i)
term(86) = term(86) + r1(vrdav_Rr, a,p) * wm_interm_36_pt3(b,q,i,j) * wm_interm_64_pt3(a,b,j,i)
term(87) = term(87) + r1(vrdav_Rr, a,p) * wm_interm_34_pt3(b,q,i,j) * wm_interm_64_pt3(a,b,j,i)
term(88) = term(88) + r1(vrdav_Rr, a,p) * wm_interm_36_pt3(a,b,i,j) * wm_interm_66_pt3(b,q,j,i)
term(89) = term(89) + r1(vrdav_Rr, a,p) * wm_interm_36_pt3(a,b,i,j) * wm_interm_64_pt3(b,q,j,i)
term(90) = term(90) + r1(vrdav_Rr, a,p) * wm_interm_36_pt3(a,b,i,j) * wm_interm_67_pt3(b,q,j,i)
term(91) = term(91) + r1(vrdav_Rr, a,p) * wm_interm_36_pt3(a,b,i,j) * wm_interm_68_pt3(b,q,j,i)
end do 
end do 
end do 
end do 

term(58) = term(58) * (4.0d+0) 
term(59) = term(59) * (-8.0d+0) 
term(60) = term(60) * (-8.0d+0) 
term(61) = term(61) * (16.0d+0) 
term(62) = term(62) * (4.0d+0) 
term(63) = term(63) * (-8.0d+0) 
term(64) = term(64) * (-8.0d+0) 
term(65) = term(65) * (16.0d+0) 
term(66) = term(66) * (-2.0d+0) 
term(67) = term(67) * (4.0d+0) 
term(68) = term(68) * (4.0d+0) 
term(69) = term(69) * (-8.0d+0) 
term(70) = term(70) * (-2.0d+0) 
term(71) = term(71) * (4.0d+0) 
term(72) = term(72) * (4.0d+0) 
term(73) = term(73) * (-8.0d+0) 
term(74) = term(74) * (-2.0d+0) 
term(75) = term(75) * (4.0d+0) 
term(76) = term(76) * (-2.0d+0) 
term(77) = term(77) * (4.0d+0) 
term(78) = term(78) * (-2.0d+0) 
term(79) = term(79) * (4.0d+0) 
term(80) = term(80) * (-8.0d+0) 
term(81) = term(81) * (-2.0d+0) 
term(82) = term(82) * (4.0d+0) 
term(83) = term(83) * (4.0d+0) 
term(84) = term(84) * (-2.0d+0) 
term(85) = term(85) * (4.0d+0) 
term(86) = term(86) * (4.0d+0) 
term(87) = term(87) * (-8.0d+0) 
term(88) = term(88) * (-2.0d+0) 
term(89) = term(89) * (4.0d+0) 
term(90) = term(90) * (4.0d+0) 
term(91) = term(91) * (-8.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(92) = term(92) + t2(a,q,j,i) * wm_interm_38_pt3(i,j,k,l) * wm_interm_58_pt3(a,p,l,k)
term(93) = term(93) + t2(a,q,j,i) * wm_interm_38_pt3(j,i,k,l) * wm_interm_58_pt3(a,p,l,k)
end do 
end do 
end do 
end do 
end do 

term(92) = term(92) * (-1.0d+0) 
term(93) = term(93) * (2.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
term(94) = term(94) + r1(vrdav_Rl, q,j) * wm_interm_11_pt3(i,p) * wm_interm_69_pt3(i,j)
term(95) = term(95) + r1(vrdav_Rl, q,j) * wm_interm_11_pt3(i,p) * wm_interm_70_pt3(i,j)
term(96) = term(96) + r1(vrdav_Rl, q,j) * wm_interm_12_pt3(i,p) * wm_interm_69_pt3(i,j)
term(97) = term(97) + r1(vrdav_Rl, q,j) * wm_interm_12_pt3(i,p) * wm_interm_70_pt3(i,j)
end do 
end do 

term(94) = term(94) * (-2.0d+0) 
term(95) = term(95) * (4.0d+0) 
term(96) = term(96) * (4.0d+0) 
term(97) = term(97) * (-8.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(98) = term(98) + r1(vrdav_Rl, q,i) * wm_interm_3_pt3(a,b) * wm_interm_64_pt3(a,b,p,i)
term(99) = term(99) + r1(vrdav_Rl, q,i) * wm_interm_4_pt3(a,b) * wm_interm_64_pt3(a,b,p,i)
term(100) = term(100) + r1(vrdav_Rl, q,i) * wm_interm_3_pt3(a,b) * wm_interm_66_pt3(a,b,p,i)
term(101) = term(101) + r1(vrdav_Rl, q,i) * wm_interm_4_pt3(a,b) * wm_interm_66_pt3(a,b,p,i)
term(102) = term(102) + r1(vrdav_Rl, q,i) * wm_interm_3_pt3(a,b) * wm_interm_67_pt3(a,b,p,i)
term(103) = term(103) + r1(vrdav_Rl, q,i) * wm_interm_4_pt3(a,b) * wm_interm_67_pt3(a,b,p,i)
term(104) = term(104) + r1(vrdav_Rl, q,i) * wm_interm_3_pt3(a,b) * wm_interm_68_pt3(a,b,p,i)
term(105) = term(105) + r1(vrdav_Rl, q,i) * wm_interm_4_pt3(a,b) * wm_interm_68_pt3(a,b,p,i)
end do 
end do 
end do 

term(98) = term(98) * (4.0d+0) 
term(99) = term(99) * (-8.0d+0) 
term(100) = term(100) * (-8.0d+0) 
term(101) = term(101) * (16.0d+0) 
term(102) = term(102) * (4.0d+0) 
term(103) = term(103) * (-8.0d+0) 
term(104) = term(104) * (-8.0d+0) 
term(105) = term(105) * (16.0d+0) 

do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(106) = term(106) + r1(vrdav_Rr, a,p) * wm_interm_24_pt3(b,q) * wm_interm_71_pt3(a,b)
term(107) = term(107) + r1(vrdav_Rr, a,p) * wm_interm_23_pt3(b,q) * wm_interm_71_pt3(a,b)
term(108) = term(108) + r1(vrdav_Rr, a,p) * wm_interm_24_pt3(b,q) * wm_interm_72_pt3(a,b)
term(109) = term(109) + r1(vrdav_Rr, a,p) * wm_interm_23_pt3(b,q) * wm_interm_72_pt3(a,b)
term(110) = term(110) + r1(vrdav_Rr, a,p) * wm_interm_23_pt3(a,b) * wm_interm_71_pt3(b,q)
term(111) = term(111) + r1(vrdav_Rr, a,p) * wm_interm_23_pt3(a,b) * wm_interm_72_pt3(b,q)
term(112) = term(112) + r1(vrdav_Rr, a,p) * wm_interm_24_pt3(a,b) * wm_interm_71_pt3(b,q)
term(113) = term(113) + r1(vrdav_Rr, a,p) * wm_interm_24_pt3(a,b) * wm_interm_72_pt3(b,q)
end do 
end do 

term(106) = term(106) * (-2.0d+0) 
term(107) = term(107) * (4.0d+0) 
term(108) = term(108) * (4.0d+0) 
term(109) = term(109) * (-8.0d+0) 
term(110) = term(110) * (4.0d+0) 
term(111) = term(111) * (-8.0d+0) 
term(112) = term(112) * (-2.0d+0) 
term(113) = term(113) * (4.0d+0) 

do i = 1, nocc 
term(114) = term(114) + wm_interm_11_pt3(i,p) * wm_interm_29_pt3(q,i)
term(115) = term(115) + wm_interm_12_pt3(i,p) * wm_interm_29_pt3(q,i)
term(116) = term(116) + wm_interm_11_pt3(i,p) * wm_interm_30_pt3(q,i)
term(117) = term(117) + wm_interm_12_pt3(i,p) * wm_interm_30_pt3(q,i)
term(118) = term(118) + wm_interm_22_pt3(q,i) * wm_interm_8_pt3(i,p)
term(119) = term(119) + wm_interm_22_pt3(q,i) * wm_interm_9_pt3(i,p)
term(120) = term(120) + wm_interm_21_pt3(q,i) * wm_interm_8_pt3(i,p)
term(121) = term(121) + wm_interm_21_pt3(q,i) * wm_interm_9_pt3(i,p)
term(122) = term(122) + wm_interm_22_pt3(q,i) * wm_interm_57_pt3(i,p)
term(123) = term(123) + wm_interm_21_pt3(q,i) * wm_interm_57_pt3(i,p)
term(124) = term(124) + wm_interm_54_pt3(p,i) * wm_interm_59_pt3(q,i)
term(125) = term(125) + wm_interm_54_pt3(p,i) * wm_interm_61_pt3(q,i)
end do 

term(114) = term(114) * (4.0d+0) 
term(115) = term(115) * (-8.0d+0) 
term(116) = term(116) * (-2.0d+0) 
term(117) = term(117) * (4.0d+0) 
term(118) = term(118) * (-2.0d+0) 
term(119) = term(119) * (4.0d+0) 
term(120) = term(120) * (4.0d+0) 
term(121) = term(121) * (-8.0d+0) 
term(122) = term(122) * (2.0d+0) 
term(123) = term(123) * (-4.0d+0) 
term(124) = term(124) * (2.0d+0) 
term(125) = term(125) * (-4.0d+0) 

do a = nocc + 1, nactive 
term(126) = term(126) + wm_interm_24_pt3(a,q) * wm_interm_31_pt3(a,p)
term(127) = term(127) + wm_interm_23_pt3(a,q) * wm_interm_31_pt3(a,p)
term(128) = term(128) + wm_interm_24_pt3(a,q) * wm_interm_32_pt3(a,p)
term(129) = term(129) + wm_interm_23_pt3(a,q) * wm_interm_32_pt3(a,p)
term(130) = term(130) + wm_interm_42_pt3(q,a) * wm_interm_55_pt3(a,p)
term(131) = term(131) + wm_interm_42_pt3(q,a) * wm_interm_56_pt3(a,p)
end do 

term(126) = term(126) * (-4.0d+0) 
term(127) = term(127) * (8.0d+0) 
term(128) = term(128) * (2.0d+0) 
term(129) = term(129) * (-4.0d+0) 
term(130) = term(130) * (2.0d+0) 
term(131) = term(131) * (-4.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(132) = term(132) + r1(vrdav_Rl, q,i) * wm_interm_17_pt3(a,b,j,p) * wm_interm_64_pt3(a,b,j,i)
term(133) = term(133) + r1(vrdav_Rl, q,i) * wm_interm_16_pt3(a,b,j,p) * wm_interm_64_pt3(a,b,j,i)
term(134) = term(134) + r1(vrdav_Rl, q,i) * wm_interm_14_pt3(a,b,j,p) * wm_interm_64_pt3(a,b,j,i)
term(135) = term(135) + r1(vrdav_Rl, q,i) * wm_interm_15_pt3(a,b,j,p) * wm_interm_64_pt3(a,b,j,i)
term(136) = term(136) + r1(vrdav_Rl, q,i) * wm_interm_14_pt3(a,b,j,p) * wm_interm_66_pt3(a,b,j,i)
term(137) = term(137) + r1(vrdav_Rl, q,i) * wm_interm_15_pt3(a,b,j,p) * wm_interm_66_pt3(a,b,j,i)
term(138) = term(138) + r1(vrdav_Rl, q,i) * wm_interm_16_pt3(a,b,j,p) * wm_interm_66_pt3(a,b,j,i)
term(139) = term(139) + r1(vrdav_Rl, q,i) * wm_interm_17_pt3(a,b,j,p) * wm_interm_66_pt3(a,b,j,i)
term(140) = term(140) + r1(vrdav_Rl, q,i) * wm_interm_17_pt3(a,b,j,p) * wm_interm_67_pt3(a,b,j,i)
term(141) = term(141) + r1(vrdav_Rl, q,i) * wm_interm_16_pt3(a,b,j,p) * wm_interm_67_pt3(a,b,j,i)
term(142) = term(142) + r1(vrdav_Rl, q,i) * wm_interm_14_pt3(a,b,j,p) * wm_interm_67_pt3(a,b,j,i)
term(143) = term(143) + r1(vrdav_Rl, q,i) * wm_interm_15_pt3(a,b,j,p) * wm_interm_67_pt3(a,b,j,i)
term(144) = term(144) + r1(vrdav_Rl, q,i) * wm_interm_17_pt3(a,b,j,p) * wm_interm_68_pt3(a,b,j,i)
term(145) = term(145) + r1(vrdav_Rl, q,i) * wm_interm_16_pt3(a,b,j,p) * wm_interm_68_pt3(a,b,j,i)
term(146) = term(146) + r1(vrdav_Rl, q,i) * wm_interm_14_pt3(a,b,j,p) * wm_interm_68_pt3(a,b,j,i)
term(147) = term(147) + r1(vrdav_Rl, q,i) * wm_interm_15_pt3(a,b,j,p) * wm_interm_68_pt3(a,b,j,i)
term(148) = term(148) + t2(a,q,j,i) * wm_interm_48_pt3(b,i,j,p) * wm_interm_71_pt3(a,b)
term(149) = term(149) + t2(a,q,j,i) * wm_interm_48_pt3(b,i,j,p) * wm_interm_72_pt3(a,b)
term(150) = term(150) + t2(a,q,j,i) * wm_interm_52_pt3(b,i,j,p) * wm_interm_71_pt3(a,b)
term(151) = term(151) + t2(a,q,j,i) * wm_interm_52_pt3(b,i,j,p) * wm_interm_72_pt3(a,b)
term(152) = term(152) + t2(a,q,j,i) * wm_interm_23_pt3(a,b) * wm_interm_58_pt3(b,p,j,i)
term(153) = term(153) + t2(a,q,j,i) * wm_interm_24_pt3(a,b) * wm_interm_58_pt3(b,p,j,i)
end do 
end do 
end do 
end do 

term(132) = term(132) * (4.0d+0) 
term(133) = term(133) * (-8.0d+0) 
term(134) = term(134) * (-8.0d+0) 
term(135) = term(135) * (16.0d+0) 
term(136) = term(136) * (4.0d+0) 
term(137) = term(137) * (-8.0d+0) 
term(138) = term(138) * (4.0d+0) 
term(139) = term(139) * (-8.0d+0) 
term(140) = term(140) * (4.0d+0) 
term(141) = term(141) * (-8.0d+0) 
term(142) = term(142) * (-8.0d+0) 
term(143) = term(143) * (16.0d+0) 
term(144) = term(144) * (-8.0d+0) 
term(145) = term(145) * (16.0d+0) 
term(146) = term(146) * (16.0d+0) 
term(147) = term(147) * (-32.0d+0) 
term(148) = term(148) * (2.0d+0) 
term(149) = term(149) * (-4.0d+0) 
term(150) = term(150) * (-1.0d+0) 
term(151) = term(151) * (2.0d+0) 
term(152) = term(152) * (4.0d+0) 
term(153) = term(153) * (-2.0d+0) 


    calc_D_ov_wm_pt3 = zero
    do s = 0, 153
    calc_D_ov_wm_pt3 = calc_D_ov_wm_pt3 + term(s)
    end do

    end function calc_D_ov_wm_pt3
    
    function calc_D_vo_wm_pt3(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vo_wm_pt3
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
    real(F64), dimension(0:807) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(0) = term(0) + wm_interm_1_pt3(p,i,j,k) * wm_interm_5_pt3(q,k,j,i)
term(1) = term(1) + wm_interm_1_pt3(p,i,j,k) * wm_interm_5_pt3(k,q,j,i)
end do 
end do 
end do 

term(0) = term(0) * (-1.0d+0) 
term(1) = term(1) * (2.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(2) = term(2) + wm_interm_1_pt3(p,i,j,k) * wm_interm_5_pt3(q,k,i,j)
term(3) = term(3) + wm_interm_1_pt3(p,i,j,k) * wm_interm_5_pt3(k,q,i,j)
term(4) = term(4) + wm_interm_27_pt3(p,i,j,k) * wm_interm_38_pt3(i,j,q,k)
term(5) = term(5) + wm_interm_26_pt3(p,i,j,k) * wm_interm_38_pt3(i,j,q,k)
term(6) = term(6) + wm_interm_26_pt3(p,i,j,k) * wm_interm_38_pt3(i,j,k,q)
term(7) = term(7) + wm_interm_27_pt3(p,i,j,k) * wm_interm_38_pt3(i,j,k,q)
term(8) = term(8) + r1(vrdav_Rr, p,i) * wm_interm_38_pt3(i,j,k,q) * wm_interm_69_pt3(k,j)
term(9) = term(9) + r1(vrdav_Rr, p,i) * wm_interm_38_pt3(i,j,k,q) * wm_interm_70_pt3(k,j)
term(10) = term(10) + r1(vrdav_Rr, p,i) * wm_interm_38_pt3(i,j,q,k) * wm_interm_69_pt3(k,j)
term(11) = term(11) + r1(vrdav_Rr, p,i) * wm_interm_38_pt3(i,j,q,k) * wm_interm_70_pt3(k,j)
term(12) = term(12) + r1(vrdav_Rr, p,i) * wm_interm_38_pt3(j,i,k,q) * wm_interm_69_pt3(k,j)
term(13) = term(13) + r1(vrdav_Rr, p,i) * wm_interm_38_pt3(j,i,k,q) * wm_interm_70_pt3(k,j)
term(14) = term(14) + r1(vrdav_Rr, p,i) * wm_interm_38_pt3(j,i,q,k) * wm_interm_69_pt3(k,j)
term(15) = term(15) + r1(vrdav_Rr, p,i) * wm_interm_38_pt3(j,i,q,k) * wm_interm_70_pt3(k,j)
end do 
end do 
end do 

term(2) = term(2) * (2.0d+0) 
term(3) = term(3) * (-1.0d+0) 
term(4) = term(4) * (-1.0d+0) 
term(5) = term(5) * (2.0d+0) 
term(6) = term(6) * (-1.0d+0) 
term(7) = term(7) * (2.0d+0) 
term(8) = term(8) * (-1.0d+0) 
term(9) = term(9) * (2.0d+0) 
term(10) = term(10) * (2.0d+0) 
term(11) = term(11) * (-4.0d+0) 
term(12) = term(12) * (2.0d+0) 
term(13) = term(13) * (-4.0d+0) 
term(14) = term(14) * (-1.0d+0) 
term(15) = term(15) * (2.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(16) = term(16) + wm_interm_10_pt3(a,q,i,j) * wm_interm_14_pt3(a,p,i,j)
term(17) = term(17) + wm_interm_10_pt3(a,q,i,j) * wm_interm_15_pt3(a,p,i,j)
term(18) = term(18) + wm_interm_10_pt3(a,q,i,j) * wm_interm_16_pt3(a,p,i,j)
term(19) = term(19) + wm_interm_10_pt3(a,q,i,j) * wm_interm_17_pt3(a,p,i,j)
term(20) = term(20) + wm_interm_10_pt3(a,i,q,j) * wm_interm_14_pt3(a,p,i,j)
term(21) = term(21) + wm_interm_10_pt3(a,i,q,j) * wm_interm_15_pt3(a,p,i,j)
term(22) = term(22) + wm_interm_10_pt3(a,i,q,j) * wm_interm_17_pt3(a,p,i,j)
term(23) = term(23) + wm_interm_10_pt3(a,i,q,j) * wm_interm_16_pt3(a,p,i,j)
term(24) = term(24) + wm_interm_14_pt3(a,p,i,j) * wm_interm_20_pt3(a,i,q,j)
term(25) = term(25) + wm_interm_15_pt3(a,p,i,j) * wm_interm_20_pt3(a,i,q,j)
term(26) = term(26) + wm_interm_14_pt3(a,p,i,j) * wm_interm_20_pt3(a,q,i,j)
term(27) = term(27) + wm_interm_15_pt3(a,p,i,j) * wm_interm_20_pt3(a,q,i,j)
term(28) = term(28) + wm_interm_17_pt3(a,p,i,j) * wm_interm_20_pt3(a,q,i,j)
term(29) = term(29) + wm_interm_16_pt3(a,p,i,j) * wm_interm_20_pt3(a,q,i,j)
term(30) = term(30) + wm_interm_16_pt3(a,p,i,j) * wm_interm_20_pt3(a,i,q,j)
term(31) = term(31) + wm_interm_17_pt3(a,p,i,j) * wm_interm_20_pt3(a,i,q,j)
term(32) = term(32) + wm_interm_37_pt3(a,i,q,j) * wm_interm_6_pt3(a,p,j,i)
term(33) = term(33) + wm_interm_37_pt3(a,i,q,j) * wm_interm_7_pt3(a,p,j,i)
term(34) = term(34) + wm_interm_2_pt3(a,p,i,j) * wm_interm_37_pt3(a,j,q,i)
term(35) = term(35) + wm_interm_0_pt3(a,p,i,j) * wm_interm_37_pt3(a,j,q,i)
term(36) = term(36) + wm_interm_0_pt3(a,p,i,j) * wm_interm_37_pt3(a,j,i,q)
term(37) = term(37) + wm_interm_2_pt3(a,p,i,j) * wm_interm_37_pt3(a,j,i,q)
term(38) = term(38) + r1(vrdav_Rl, a,q) * wm_interm_11_pt3(i,j) * wm_interm_64_pt3(p,a,i,j)
term(39) = term(39) + r1(vrdav_Rl, a,q) * wm_interm_12_pt3(i,j) * wm_interm_64_pt3(p,a,i,j)
term(40) = term(40) + r1(vrdav_Rl, a,q) * wm_interm_11_pt3(i,j) * wm_interm_66_pt3(p,a,i,j)
term(41) = term(41) + r1(vrdav_Rl, a,q) * wm_interm_12_pt3(i,j) * wm_interm_66_pt3(p,a,i,j)
term(42) = term(42) + s2(a,p,i,q) * wm_interm_11_pt3(i,j) * wm_interm_61_pt3(a,j)
term(43) = term(43) + s2(a,p,i,q) * wm_interm_12_pt3(i,j) * wm_interm_61_pt3(a,j)
term(44) = term(44) + r1(vrdav_Rl, a,q) * wm_interm_11_pt3(i,j) * wm_interm_67_pt3(p,a,i,j)
term(45) = term(45) + r1(vrdav_Rl, a,q) * wm_interm_12_pt3(i,j) * wm_interm_67_pt3(p,a,i,j)
term(46) = term(46) + r1(vrdav_Rl, a,q) * wm_interm_11_pt3(i,j) * wm_interm_68_pt3(p,a,i,j)
term(47) = term(47) + r1(vrdav_Rl, a,q) * wm_interm_12_pt3(i,j) * wm_interm_68_pt3(p,a,i,j)
term(48) = term(48) + s2(a,p,i,q) * wm_interm_11_pt3(i,j) * wm_interm_59_pt3(a,j)
term(49) = term(49) + s2(a,p,i,q) * wm_interm_12_pt3(i,j) * wm_interm_59_pt3(a,j)
term(50) = term(50) + r1(vrdav_Rl, a,i) * wm_interm_17_pt3(a,p,j,q) * wm_interm_69_pt3(j,i)
term(51) = term(51) + r1(vrdav_Rl, a,i) * wm_interm_16_pt3(a,p,j,q) * wm_interm_69_pt3(j,i)
term(52) = term(52) + r1(vrdav_Rl, a,i) * wm_interm_17_pt3(a,p,j,q) * wm_interm_70_pt3(j,i)
term(53) = term(53) + r1(vrdav_Rl, a,i) * wm_interm_16_pt3(a,p,j,q) * wm_interm_70_pt3(j,i)
term(54) = term(54) + r1(vrdav_Rl, a,i) * wm_interm_14_pt3(a,p,j,q) * wm_interm_69_pt3(j,i)
term(55) = term(55) + r1(vrdav_Rl, a,i) * wm_interm_15_pt3(a,p,j,q) * wm_interm_69_pt3(j,i)
term(56) = term(56) + r1(vrdav_Rl, a,i) * wm_interm_14_pt3(a,p,j,q) * wm_interm_70_pt3(j,i)
term(57) = term(57) + r1(vrdav_Rl, a,i) * wm_interm_15_pt3(a,p,j,q) * wm_interm_70_pt3(j,i)
term(58) = term(58) + s2(a,p,i,j) * wm_interm_11_pt3(i,q) * wm_interm_61_pt3(a,j)
term(59) = term(59) + s2(a,p,i,j) * wm_interm_12_pt3(i,q) * wm_interm_61_pt3(a,j)
term(60) = term(60) + s2(a,p,i,j) * wm_interm_11_pt3(i,q) * wm_interm_59_pt3(a,j)
term(61) = term(61) + s2(a,p,i,j) * wm_interm_12_pt3(i,q) * wm_interm_59_pt3(a,j)
term(62) = term(62) + t2(a,p,i,q) * wm_interm_51_pt3(a,j) * wm_interm_69_pt3(i,j)
term(63) = term(63) + r1(vrdav_Rr, a,i) * wm_interm_34_pt3(a,p,j,q) * wm_interm_69_pt3(i,j)
term(64) = term(64) + t2(a,p,i,q) * wm_interm_51_pt3(a,j) * wm_interm_70_pt3(i,j)
term(65) = term(65) + r1(vrdav_Rr, a,i) * wm_interm_34_pt3(a,p,j,q) * wm_interm_70_pt3(i,j)
term(66) = term(66) + r1(vrdav_Rr, a,i) * wm_interm_33_pt3(a,p,j,q) * wm_interm_69_pt3(i,j)
term(67) = term(67) + r1(vrdav_Rr, a,i) * wm_interm_33_pt3(a,p,j,q) * wm_interm_70_pt3(i,j)
term(68) = term(68) + t2(a,p,i,q) * wm_interm_49_pt3(a,j) * wm_interm_69_pt3(i,j)
term(69) = term(69) + t2(a,p,i,q) * wm_interm_49_pt3(a,j) * wm_interm_70_pt3(i,j)
term(70) = term(70) + r1(vrdav_Rr, a,i) * wm_interm_25_pt3(i,j) * wm_interm_67_pt3(a,p,j,q)
term(71) = term(71) + t2(a,p,i,q) * wm_interm_25_pt3(i,j) * wm_interm_56_pt3(a,j)
term(72) = term(72) + t2(a,p,i,q) * wm_interm_25_pt3(i,j) * wm_interm_55_pt3(a,j)
term(73) = term(73) + r1(vrdav_Rr, a,i) * wm_interm_25_pt3(i,j) * wm_interm_68_pt3(a,p,j,q)
term(74) = term(74) + r1(vrdav_Rr, a,i) * wm_interm_28_pt3(i,j) * wm_interm_67_pt3(a,p,j,q)
term(75) = term(75) + t2(a,p,i,q) * wm_interm_28_pt3(i,j) * wm_interm_56_pt3(a,j)
term(76) = term(76) + t2(a,p,i,q) * wm_interm_28_pt3(i,j) * wm_interm_55_pt3(a,j)
term(77) = term(77) + r1(vrdav_Rr, a,i) * wm_interm_28_pt3(i,j) * wm_interm_68_pt3(a,p,j,q)
term(78) = term(78) + r1(vrdav_Rr, a,i) * wm_interm_36_pt3(a,p,j,q) * wm_interm_69_pt3(i,j)
term(79) = term(79) + r1(vrdav_Rr, a,i) * wm_interm_36_pt3(a,p,j,q) * wm_interm_70_pt3(i,j)
term(80) = term(80) + r1(vrdav_Rr, a,i) * wm_interm_35_pt3(a,p,j,q) * wm_interm_69_pt3(i,j)
term(81) = term(81) + r1(vrdav_Rr, a,i) * wm_interm_35_pt3(a,p,j,q) * wm_interm_70_pt3(i,j)
term(82) = term(82) + r1(vrdav_Rr, a,i) * wm_interm_25_pt3(i,j) * wm_interm_66_pt3(a,p,j,q)
term(83) = term(83) + r1(vrdav_Rr, a,i) * wm_interm_25_pt3(i,j) * wm_interm_64_pt3(a,p,j,q)
term(84) = term(84) + r1(vrdav_Rr, a,i) * wm_interm_28_pt3(i,j) * wm_interm_66_pt3(a,p,j,q)
term(85) = term(85) + r1(vrdav_Rr, a,i) * wm_interm_28_pt3(i,j) * wm_interm_64_pt3(a,p,j,q)
end do 
end do 
end do 

term(16) = term(16) * (-1.0d+0) 
term(17) = term(17) * (2.0d+0) 
term(18) = term(18) * (-1.0d+0) 
term(19) = term(19) * (2.0d+0) 
term(20) = term(20) * (2.0d+0) 
term(21) = term(21) * (-4.0d+0) 
term(22) = term(22) * (-1.0d+0) 
term(23) = term(23) * (2.0d+0) 
term(24) = term(24) * (-1.0d+0) 
term(25) = term(25) * (2.0d+0) 
term(26) = term(26) * (2.0d+0) 
term(27) = term(27) * (-4.0d+0) 
term(28) = term(28) * (-1.0d+0) 
term(29) = term(29) * (2.0d+0) 
term(30) = term(30) * (-1.0d+0) 
term(31) = term(31) * (2.0d+0) 
term(32) = term(32) * (-2.0d+0) 
term(33) = term(33) * (4.0d+0) 
term(34) = term(34) * (-2.0d+0) 
term(35) = term(35) * (4.0d+0) 
term(36) = term(36) * (-2.0d+0) 
term(37) = term(37) * (4.0d+0) 
term(38) = term(38) * (4.0d+0) 
term(39) = term(39) * (-8.0d+0) 
term(40) = term(40) * (-8.0d+0) 
term(41) = term(41) * (16.0d+0) 
term(42) = term(42) * (16.0d+0) 
term(43) = term(43) * (-32.0d+0) 
term(44) = term(44) * (4.0d+0) 
term(45) = term(45) * (-8.0d+0) 
term(46) = term(46) * (-8.0d+0) 
term(47) = term(47) * (16.0d+0) 
term(48) = term(48) * (-8.0d+0) 
term(49) = term(49) * (16.0d+0) 
term(50) = term(50) * (-2.0d+0) 
term(51) = term(51) * (4.0d+0) 
term(52) = term(52) * (4.0d+0) 
term(53) = term(53) * (-8.0d+0) 
term(54) = term(54) * (4.0d+0) 
term(55) = term(55) * (-8.0d+0) 
term(56) = term(56) * (-8.0d+0) 
term(57) = term(57) * (16.0d+0) 
term(58) = term(58) * (4.0d+0) 
term(59) = term(59) * (-8.0d+0) 
term(60) = term(60) * (-2.0d+0) 
term(61) = term(61) * (4.0d+0) 
term(62) = term(62) * (4.0d+0) 
term(63) = term(63) * (-8.0d+0) 
term(64) = term(64) * (-8.0d+0) 
term(65) = term(65) * (16.0d+0) 
term(66) = term(66) * (4.0d+0) 
term(67) = term(67) * (-8.0d+0) 
term(68) = term(68) * (-8.0d+0) 
term(69) = term(69) * (16.0d+0) 
term(70) = term(70) * (4.0d+0) 
term(71) = term(71) * (-8.0d+0) 
term(72) = term(72) * (4.0d+0) 
term(73) = term(73) * (-8.0d+0) 
term(74) = term(74) * (-8.0d+0) 
term(75) = term(75) * (16.0d+0) 
term(76) = term(76) * (-8.0d+0) 
term(77) = term(77) * (16.0d+0) 
term(78) = term(78) * (4.0d+0) 
term(79) = term(79) * (-8.0d+0) 
term(80) = term(80) * (-2.0d+0) 
term(81) = term(81) * (4.0d+0) 
term(82) = term(82) * (-2.0d+0) 
term(83) = term(83) * (4.0d+0) 
term(84) = term(84) * (4.0d+0) 
term(85) = term(85) * (-8.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(86) = term(86) + s2(a,p,q,i) * wm_interm_43_pt3(b,j,i,k) * wm_interm_64_pt3(b,a,k,j)
term(87) = term(87) + s2(a,p,q,i) * wm_interm_43_pt3(b,j,i,k) * wm_interm_66_pt3(b,a,k,j)
term(88) = term(88) + s2(a,p,j,i) * wm_interm_43_pt3(b,k,i,j) * wm_interm_64_pt3(b,a,q,k)
term(89) = term(89) + s2(a,p,i,q) * wm_interm_43_pt3(b,j,i,k) * wm_interm_64_pt3(b,a,k,j)
term(90) = term(90) + s2(a,p,i,q) * wm_interm_43_pt3(b,j,i,k) * wm_interm_66_pt3(b,a,k,j)
term(91) = term(91) + s2(a,p,j,i) * wm_interm_43_pt3(b,k,i,j) * wm_interm_66_pt3(b,a,q,k)
term(92) = term(92) + s2(a,p,q,i) * wm_interm_43_pt3(b,j,i,k) * wm_interm_67_pt3(b,a,k,j)
term(93) = term(93) + s2(a,p,q,i) * wm_interm_43_pt3(b,j,i,k) * wm_interm_68_pt3(b,a,k,j)
term(94) = term(94) + s2(a,p,j,i) * wm_interm_43_pt3(b,k,i,j) * wm_interm_67_pt3(b,a,q,k)
term(95) = term(95) + s2(a,p,i,q) * wm_interm_43_pt3(b,j,i,k) * wm_interm_67_pt3(b,a,k,j)
term(96) = term(96) + s2(a,p,j,i) * wm_interm_43_pt3(b,k,i,j) * wm_interm_68_pt3(b,a,q,k)
term(97) = term(97) + s2(a,p,i,q) * wm_interm_43_pt3(b,j,i,k) * wm_interm_68_pt3(b,a,k,j)
end do 
end do 
end do 
end do 
end do 

term(86) = term(86) * (-8.0d+0) 
term(87) = term(87) * (4.0d+0) 
term(88) = term(88) * (4.0d+0) 
term(89) = term(89) * (16.0d+0) 
term(90) = term(90) * (-8.0d+0) 
term(91) = term(91) * (-8.0d+0) 
term(92) = term(92) * (-8.0d+0) 
term(93) = term(93) * (16.0d+0) 
term(94) = term(94) * (4.0d+0) 
term(95) = term(95) * (16.0d+0) 
term(96) = term(96) * (-8.0d+0) 
term(97) = term(97) * (-32.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(98) = term(98) + wm_interm_21_pt3(a,i) * wm_interm_6_pt3(a,p,i,q)
term(99) = term(99) + wm_interm_21_pt3(a,i) * wm_interm_7_pt3(a,p,i,q)
term(100) = term(100) + wm_interm_0_pt3(a,p,i,q) * wm_interm_21_pt3(a,i)
term(101) = term(101) + wm_interm_21_pt3(a,i) * wm_interm_2_pt3(a,p,i,q)
term(102) = term(102) + wm_interm_0_pt3(a,p,i,q) * wm_interm_22_pt3(a,i)
term(103) = term(103) + wm_interm_22_pt3(a,i) * wm_interm_2_pt3(a,p,i,q)
term(104) = term(104) + wm_interm_22_pt3(a,i) * wm_interm_6_pt3(a,p,i,q)
term(105) = term(105) + wm_interm_22_pt3(a,i) * wm_interm_7_pt3(a,p,i,q)
term(106) = term(106) + wm_interm_17_pt3(a,p,i,q) * wm_interm_29_pt3(a,i)
term(107) = term(107) + wm_interm_16_pt3(a,p,i,q) * wm_interm_29_pt3(a,i)
term(108) = term(108) + wm_interm_14_pt3(a,p,i,q) * wm_interm_29_pt3(a,i)
term(109) = term(109) + wm_interm_15_pt3(a,p,i,q) * wm_interm_29_pt3(a,i)
term(110) = term(110) + wm_interm_17_pt3(a,p,i,q) * wm_interm_30_pt3(a,i)
term(111) = term(111) + wm_interm_16_pt3(a,p,i,q) * wm_interm_30_pt3(a,i)
term(112) = term(112) + wm_interm_14_pt3(a,p,i,q) * wm_interm_30_pt3(a,i)
term(113) = term(113) + wm_interm_15_pt3(a,p,i,q) * wm_interm_30_pt3(a,i)
term(114) = term(114) + wm_interm_31_pt3(a,i) * wm_interm_33_pt3(a,p,i,q)
term(115) = term(115) + wm_interm_31_pt3(a,i) * wm_interm_34_pt3(a,p,i,q)
term(116) = term(116) + wm_interm_32_pt3(a,i) * wm_interm_33_pt3(a,p,i,q)
term(117) = term(117) + wm_interm_32_pt3(a,i) * wm_interm_34_pt3(a,p,i,q)
term(118) = term(118) + wm_interm_31_pt3(a,i) * wm_interm_35_pt3(a,p,i,q)
term(119) = term(119) + wm_interm_31_pt3(a,i) * wm_interm_36_pt3(a,p,i,q)
term(120) = term(120) + wm_interm_32_pt3(a,i) * wm_interm_35_pt3(a,p,i,q)
term(121) = term(121) + wm_interm_32_pt3(a,i) * wm_interm_36_pt3(a,p,i,q)
term(122) = term(122) + r1(vrdav_Rl, a,i) * wm_interm_3_pt3(a,p) * wm_interm_69_pt3(q,i)
term(123) = term(123) + r1(vrdav_Rl, a,i) * wm_interm_4_pt3(a,p) * wm_interm_69_pt3(q,i)
term(124) = term(124) + r1(vrdav_Rl, a,i) * wm_interm_3_pt3(a,p) * wm_interm_70_pt3(q,i)
term(125) = term(125) + r1(vrdav_Rl, a,i) * wm_interm_4_pt3(a,p) * wm_interm_70_pt3(q,i)
term(126) = term(126) + r1(vrdav_Rr, a,i) * wm_interm_25_pt3(i,q) * wm_interm_71_pt3(a,p)
term(127) = term(127) + r1(vrdav_Rr, a,i) * wm_interm_28_pt3(i,q) * wm_interm_71_pt3(a,p)
term(128) = term(128) + r1(vrdav_Rr, a,i) * wm_interm_25_pt3(i,q) * wm_interm_72_pt3(a,p)
term(129) = term(129) + r1(vrdav_Rr, a,i) * wm_interm_28_pt3(i,q) * wm_interm_72_pt3(a,p)
term(130) = term(130) + r1(vrdav_Rr, a,i) * wm_interm_24_pt3(a,p) * wm_interm_69_pt3(i,q)
term(131) = term(131) + r1(vrdav_Rr, a,i) * wm_interm_23_pt3(a,p) * wm_interm_69_pt3(i,q)
term(132) = term(132) + r1(vrdav_Rr, a,i) * wm_interm_24_pt3(a,p) * wm_interm_70_pt3(i,q)
term(133) = term(133) + r1(vrdav_Rr, a,i) * wm_interm_23_pt3(a,p) * wm_interm_70_pt3(i,q)
end do 
end do 

term(98) = term(98) * (-8.0d+0) 
term(99) = term(99) * (16.0d+0) 
term(100) = term(100) * (4.0d+0) 
term(101) = term(101) * (-8.0d+0) 
term(102) = term(102) * (-2.0d+0) 
term(103) = term(103) * (4.0d+0) 
term(104) = term(104) * (4.0d+0) 
term(105) = term(105) * (-8.0d+0) 
term(106) = term(106) * (4.0d+0) 
term(107) = term(107) * (-8.0d+0) 
term(108) = term(108) * (-8.0d+0) 
term(109) = term(109) * (16.0d+0) 
term(110) = term(110) * (-2.0d+0) 
term(111) = term(111) * (4.0d+0) 
term(112) = term(112) * (4.0d+0) 
term(113) = term(113) * (-8.0d+0) 
term(114) = term(114) * (8.0d+0) 
term(115) = term(115) * (-16.0d+0) 
term(116) = term(116) * (-4.0d+0) 
term(117) = term(117) * (8.0d+0) 
term(118) = term(118) * (-4.0d+0) 
term(119) = term(119) * (8.0d+0) 
term(120) = term(120) * (2.0d+0) 
term(121) = term(121) * (-4.0d+0) 
term(122) = term(122) * (-2.0d+0) 
term(123) = term(123) * (4.0d+0) 
term(124) = term(124) * (4.0d+0) 
term(125) = term(125) * (-8.0d+0) 
term(126) = term(126) * (-4.0d+0) 
term(127) = term(127) * (8.0d+0) 
term(128) = term(128) * (8.0d+0) 
term(129) = term(129) * (-16.0d+0) 
term(130) = term(130) * (-4.0d+0) 
term(131) = term(131) * (8.0d+0) 
term(132) = term(132) * (8.0d+0) 
term(133) = term(133) * (-16.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(134) = term(134) + s2(a,p,j,i) * wm_interm_43_pt3(b,k,j,i) * wm_interm_64_pt3(b,a,q,k)
term(135) = term(135) + s2(a,p,j,i) * wm_interm_43_pt3(b,k,j,i) * wm_interm_66_pt3(b,a,q,k)
term(136) = term(136) + s2(a,p,j,i) * wm_interm_43_pt3(b,k,j,i) * wm_interm_67_pt3(b,a,q,k)
term(137) = term(137) + s2(a,p,j,i) * wm_interm_43_pt3(b,k,j,i) * wm_interm_68_pt3(b,a,q,k)
end do 
end do 
end do 
end do 
end do 

term(134) = term(134) * (-8.0d+0) 
term(135) = term(135) * (4.0d+0) 
term(136) = term(136) * (-8.0d+0) 
term(137) = term(137) * (16.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(138) = term(138) + r1(vrdav_Rl, a,i) * wm_interm_14_pt3(a,p,j,k) * wm_interm_65_pt3(q,j,i,k)
term(139) = term(139) + r1(vrdav_Rl, a,i) * wm_interm_15_pt3(a,p,j,k) * wm_interm_65_pt3(q,j,i,k)
term(140) = term(140) + r1(vrdav_Rl, a,i) * wm_interm_14_pt3(a,p,j,k) * wm_interm_65_pt3(j,q,i,k)
term(141) = term(141) + r1(vrdav_Rl, a,i) * wm_interm_15_pt3(a,p,j,k) * wm_interm_65_pt3(j,q,i,k)
term(142) = term(142) + r1(vrdav_Rl, a,i) * wm_interm_17_pt3(a,p,j,k) * wm_interm_65_pt3(j,q,i,k)
term(143) = term(143) + r1(vrdav_Rl, a,i) * wm_interm_16_pt3(a,p,j,k) * wm_interm_65_pt3(j,q,i,k)
term(144) = term(144) + r1(vrdav_Rl, a,i) * wm_interm_16_pt3(a,p,j,k) * wm_interm_65_pt3(q,j,i,k)
term(145) = term(145) + r1(vrdav_Rl, a,i) * wm_interm_17_pt3(a,p,j,k) * wm_interm_65_pt3(q,j,i,k)
term(146) = term(146) + r1(vrdav_Rr, a,i) * wm_interm_38_pt3(i,j,q,k) * wm_interm_67_pt3(a,p,k,j)
term(147) = term(147) + r1(vrdav_Rr, a,i) * wm_interm_38_pt3(j,i,q,k) * wm_interm_67_pt3(a,p,k,j)
term(148) = term(148) + r1(vrdav_Rr, a,i) * wm_interm_38_pt3(j,i,q,k) * wm_interm_64_pt3(a,p,k,j)
term(149) = term(149) + r1(vrdav_Rr, a,i) * wm_interm_38_pt3(i,j,q,k) * wm_interm_64_pt3(a,p,k,j)
term(150) = term(150) + r1(vrdav_Rr, a,i) * wm_interm_38_pt3(i,j,q,k) * wm_interm_68_pt3(a,p,k,j)
term(151) = term(151) + r1(vrdav_Rr, a,i) * wm_interm_38_pt3(j,i,q,k) * wm_interm_68_pt3(a,p,k,j)
term(152) = term(152) + r1(vrdav_Rr, a,i) * wm_interm_38_pt3(j,i,q,k) * wm_interm_66_pt3(a,p,k,j)
term(153) = term(153) + r1(vrdav_Rr, a,i) * wm_interm_38_pt3(i,j,q,k) * wm_interm_66_pt3(a,p,k,j)
term(154) = term(154) + r1(vrdav_Rr, a,i) * wm_interm_36_pt3(a,p,j,k) * wm_interm_65_pt3(k,i,j,q)
term(155) = term(155) + r1(vrdav_Rr, a,i) * wm_interm_35_pt3(a,p,j,k) * wm_interm_65_pt3(k,i,j,q)
term(156) = term(156) + r1(vrdav_Rr, a,i) * wm_interm_35_pt3(a,p,j,k) * wm_interm_65_pt3(i,k,j,q)
term(157) = term(157) + r1(vrdav_Rr, a,i) * wm_interm_36_pt3(a,p,j,k) * wm_interm_65_pt3(i,k,j,q)
term(158) = term(158) + r1(vrdav_Rr, a,i) * wm_interm_33_pt3(a,p,j,k) * wm_interm_65_pt3(k,i,j,q)
term(159) = term(159) + r1(vrdav_Rr, a,i) * wm_interm_33_pt3(a,p,j,k) * wm_interm_65_pt3(i,k,j,q)
term(160) = term(160) + r1(vrdav_Rr, a,i) * wm_interm_34_pt3(a,p,j,k) * wm_interm_65_pt3(k,i,j,q)
term(161) = term(161) + r1(vrdav_Rr, a,i) * wm_interm_34_pt3(a,p,j,k) * wm_interm_65_pt3(i,k,j,q)
end do 
end do 
end do 
end do 

term(138) = term(138) * (4.0d+0) 
term(139) = term(139) * (-8.0d+0) 
term(140) = term(140) * (-8.0d+0) 
term(141) = term(141) * (16.0d+0) 
term(142) = term(142) * (4.0d+0) 
term(143) = term(143) * (-8.0d+0) 
term(144) = term(144) * (4.0d+0) 
term(145) = term(145) * (-8.0d+0) 
term(146) = term(146) * (-2.0d+0) 
term(147) = term(147) * (4.0d+0) 
term(148) = term(148) * (4.0d+0) 
term(149) = term(149) * (-2.0d+0) 
term(150) = term(150) * (4.0d+0) 
term(151) = term(151) * (-8.0d+0) 
term(152) = term(152) * (-2.0d+0) 
term(153) = term(153) * (4.0d+0) 
term(154) = term(154) * (-4.0d+0) 
term(155) = term(155) * (8.0d+0) 
term(156) = term(156) * (-4.0d+0) 
term(157) = term(157) * (8.0d+0) 
term(158) = term(158) * (-4.0d+0) 
term(159) = term(159) * (8.0d+0) 
term(160) = term(160) * (8.0d+0) 
term(161) = term(161) * (-16.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(162) = term(162) + s2(a,p,q,i) * wm_interm_13_pt3(j,i,k,l) * wm_interm_63_pt3(a,j,l,k)
term(163) = term(163) + s2(a,p,j,i) * wm_interm_13_pt3(i,j,k,l) * wm_interm_63_pt3(a,q,l,k)
term(164) = term(164) + s2(a,p,j,i) * wm_interm_13_pt3(j,i,k,l) * wm_interm_63_pt3(a,q,l,k)
term(165) = term(165) + t2(a,p,q,i) * wm_interm_52_pt3(a,j,k,l) * wm_interm_65_pt3(l,i,j,k)
term(166) = term(166) + t2(a,p,q,i) * wm_interm_52_pt3(a,j,k,l) * wm_interm_65_pt3(i,l,j,k)
term(167) = term(167) + t2(a,p,q,i) * wm_interm_48_pt3(a,j,k,l) * wm_interm_65_pt3(i,l,j,k)
term(168) = term(168) + t2(a,p,q,i) * wm_interm_48_pt3(a,j,k,l) * wm_interm_65_pt3(l,i,j,k)
term(169) = term(169) + t2(a,p,q,i) * wm_interm_38_pt3(j,i,k,l) * wm_interm_58_pt3(a,j,l,k)
term(170) = term(170) + t2(a,p,q,i) * wm_interm_38_pt3(i,j,k,l) * wm_interm_58_pt3(a,j,l,k)
end do 
end do 
end do 
end do 
end do 

term(162) = term(162) * (4.0d+0) 
term(163) = term(163) * (2.0d+0) 
term(164) = term(164) * (-4.0d+0) 
term(165) = term(165) * (-1.0d+0) 
term(166) = term(166) * (2.0d+0) 
term(167) = term(167) * (-1.0d+0) 
term(168) = term(168) * (2.0d+0) 
term(169) = term(169) * (-1.0d+0) 
term(170) = term(170) * (2.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(171) = term(171) + r1(vrdav_Rl, a,i) * wm_interm_17_pt3(a,b,j,q) * wm_interm_66_pt3(p,b,j,i)
term(172) = term(172) + r1(vrdav_Rl, a,i) * wm_interm_16_pt3(a,b,j,q) * wm_interm_66_pt3(p,b,j,i)
term(173) = term(173) + r1(vrdav_Rl, a,i) * wm_interm_14_pt3(a,b,j,q) * wm_interm_66_pt3(p,b,j,i)
term(174) = term(174) + r1(vrdav_Rl, a,i) * wm_interm_15_pt3(a,b,j,q) * wm_interm_66_pt3(p,b,j,i)
term(175) = term(175) + r1(vrdav_Rl, a,i) * wm_interm_14_pt3(a,b,j,q) * wm_interm_64_pt3(p,b,j,i)
term(176) = term(176) + r1(vrdav_Rl, a,i) * wm_interm_15_pt3(a,b,j,q) * wm_interm_64_pt3(p,b,j,i)
term(177) = term(177) + r1(vrdav_Rl, a,i) * wm_interm_16_pt3(a,b,j,q) * wm_interm_64_pt3(p,b,j,i)
term(178) = term(178) + r1(vrdav_Rl, a,i) * wm_interm_17_pt3(a,b,j,q) * wm_interm_64_pt3(p,b,j,i)
term(179) = term(179) + r1(vrdav_Rl, a,i) * wm_interm_14_pt3(a,b,j,q) * wm_interm_67_pt3(p,b,j,i)
term(180) = term(180) + r1(vrdav_Rl, a,i) * wm_interm_15_pt3(a,b,j,q) * wm_interm_67_pt3(p,b,j,i)
term(181) = term(181) + r1(vrdav_Rl, a,i) * wm_interm_14_pt3(a,b,j,q) * wm_interm_68_pt3(p,b,j,i)
term(182) = term(182) + r1(vrdav_Rl, a,i) * wm_interm_15_pt3(a,b,j,q) * wm_interm_68_pt3(p,b,j,i)
term(183) = term(183) + r1(vrdav_Rl, a,i) * wm_interm_16_pt3(a,b,j,q) * wm_interm_67_pt3(p,b,j,i)
term(184) = term(184) + r1(vrdav_Rl, a,i) * wm_interm_17_pt3(a,b,j,q) * wm_interm_67_pt3(p,b,j,i)
term(185) = term(185) + r1(vrdav_Rl, a,i) * wm_interm_16_pt3(a,b,j,q) * wm_interm_68_pt3(p,b,j,i)
term(186) = term(186) + r1(vrdav_Rl, a,i) * wm_interm_17_pt3(a,b,j,q) * wm_interm_68_pt3(p,b,j,i)
term(187) = term(187) + s2(a,p,j,i) * wm_interm_3_pt3(a,b) * wm_interm_63_pt3(b,q,j,i)
term(188) = term(188) + s2(a,p,j,i) * wm_interm_4_pt3(a,b) * wm_interm_63_pt3(b,q,j,i)
term(189) = term(189) + t2(a,p,j,i) * wm_interm_51_pt3(b,i) * wm_interm_67_pt3(a,b,j,q)
term(190) = term(190) + t2(a,p,j,i) * wm_interm_49_pt3(b,i) * wm_interm_67_pt3(a,b,j,q)
term(191) = term(191) + t2(a,p,j,i) * wm_interm_51_pt3(b,i) * wm_interm_66_pt3(a,b,j,q)
term(192) = term(192) + t2(a,p,j,i) * wm_interm_49_pt3(b,i) * wm_interm_66_pt3(a,b,j,q)
term(193) = term(193) + t2(a,p,j,i) * wm_interm_51_pt3(b,i) * wm_interm_68_pt3(a,b,j,q)
term(194) = term(194) + t2(a,p,j,i) * wm_interm_49_pt3(b,i) * wm_interm_68_pt3(a,b,j,q)
term(195) = term(195) + t2(a,p,j,i) * wm_interm_51_pt3(b,i) * wm_interm_64_pt3(a,b,j,q)
term(196) = term(196) + t2(a,p,j,i) * wm_interm_49_pt3(b,i) * wm_interm_64_pt3(a,b,j,q)
term(197) = term(197) + t2(a,p,j,i) * wm_interm_35_pt3(a,b,j,q) * wm_interm_55_pt3(b,i)
term(198) = term(198) + t2(a,p,j,i) * wm_interm_36_pt3(a,b,j,q) * wm_interm_55_pt3(b,i)
term(199) = term(199) + t2(a,p,j,i) * wm_interm_35_pt3(a,b,j,q) * wm_interm_56_pt3(b,i)
term(200) = term(200) + t2(a,p,j,i) * wm_interm_36_pt3(a,b,j,q) * wm_interm_56_pt3(b,i)
term(201) = term(201) + t2(a,p,j,i) * wm_interm_33_pt3(a,b,j,q) * wm_interm_55_pt3(b,i)
term(202) = term(202) + t2(a,p,j,i) * wm_interm_34_pt3(a,b,j,q) * wm_interm_55_pt3(b,i)
term(203) = term(203) + t2(a,p,j,i) * wm_interm_33_pt3(a,b,j,q) * wm_interm_56_pt3(b,i)
term(204) = term(204) + t2(a,p,j,i) * wm_interm_34_pt3(a,b,j,q) * wm_interm_56_pt3(b,i)
end do 
end do 
end do 
end do 

term(171) = term(171) * (4.0d+0) 
term(172) = term(172) * (-8.0d+0) 
term(173) = term(173) * (-8.0d+0) 
term(174) = term(174) * (16.0d+0) 
term(175) = term(175) * (4.0d+0) 
term(176) = term(176) * (-8.0d+0) 
term(177) = term(177) * (4.0d+0) 
term(178) = term(178) * (-8.0d+0) 
term(179) = term(179) * (4.0d+0) 
term(180) = term(180) * (-8.0d+0) 
term(181) = term(181) * (-8.0d+0) 
term(182) = term(182) * (16.0d+0) 
term(183) = term(183) * (4.0d+0) 
term(184) = term(184) * (-8.0d+0) 
term(185) = term(185) * (-8.0d+0) 
term(186) = term(186) * (16.0d+0) 
term(187) = term(187) * (4.0d+0) 
term(188) = term(188) * (-8.0d+0) 
term(189) = term(189) * (8.0d+0) 
term(190) = term(190) * (-16.0d+0) 
term(191) = term(191) * (-4.0d+0) 
term(192) = term(192) * (8.0d+0) 
term(193) = term(193) * (-16.0d+0) 
term(194) = term(194) * (32.0d+0) 
term(195) = term(195) * (8.0d+0) 
term(196) = term(196) * (-16.0d+0) 
term(197) = term(197) * (-2.0d+0) 
term(198) = term(198) * (4.0d+0) 
term(199) = term(199) * (4.0d+0) 
term(200) = term(200) * (-8.0d+0) 
term(201) = term(201) * (4.0d+0) 
term(202) = term(202) * (-8.0d+0) 
term(203) = term(203) * (-8.0d+0) 
term(204) = term(204) * (16.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(205) = term(205) + r1(vrdav_Rl, a,i) * wm_interm_14_pt3(b,p,i,j) * wm_interm_64_pt3(b,a,q,j)
term(206) = term(206) + r1(vrdav_Rl, a,i) * wm_interm_15_pt3(b,p,i,j) * wm_interm_64_pt3(b,a,q,j)
term(207) = term(207) + r1(vrdav_Rl, a,q) * wm_interm_14_pt3(b,p,i,j) * wm_interm_64_pt3(b,a,i,j)
term(208) = term(208) + r1(vrdav_Rl, a,q) * wm_interm_15_pt3(b,p,i,j) * wm_interm_64_pt3(b,a,i,j)
term(209) = term(209) + r1(vrdav_Rl, a,q) * wm_interm_17_pt3(b,p,i,j) * wm_interm_64_pt3(b,a,i,j)
term(210) = term(210) + r1(vrdav_Rl, a,q) * wm_interm_16_pt3(b,p,i,j) * wm_interm_64_pt3(b,a,i,j)
term(211) = term(211) + r1(vrdav_Rl, a,i) * wm_interm_16_pt3(b,p,i,j) * wm_interm_64_pt3(b,a,q,j)
term(212) = term(212) + r1(vrdav_Rl, a,i) * wm_interm_17_pt3(b,p,i,j) * wm_interm_64_pt3(b,a,q,j)
term(213) = term(213) + r1(vrdav_Rl, a,q) * wm_interm_14_pt3(b,p,i,j) * wm_interm_66_pt3(b,a,i,j)
term(214) = term(214) + r1(vrdav_Rl, a,q) * wm_interm_15_pt3(b,p,i,j) * wm_interm_66_pt3(b,a,i,j)
term(215) = term(215) + r1(vrdav_Rl, a,i) * wm_interm_14_pt3(b,p,i,j) * wm_interm_66_pt3(b,a,q,j)
term(216) = term(216) + r1(vrdav_Rl, a,i) * wm_interm_15_pt3(b,p,i,j) * wm_interm_66_pt3(b,a,q,j)
term(217) = term(217) + r1(vrdav_Rl, a,i) * wm_interm_17_pt3(b,p,i,j) * wm_interm_66_pt3(b,a,q,j)
term(218) = term(218) + r1(vrdav_Rl, a,i) * wm_interm_16_pt3(b,p,i,j) * wm_interm_66_pt3(b,a,q,j)
term(219) = term(219) + r1(vrdav_Rl, a,q) * wm_interm_16_pt3(b,p,i,j) * wm_interm_66_pt3(b,a,i,j)
term(220) = term(220) + r1(vrdav_Rl, a,q) * wm_interm_17_pt3(b,p,i,j) * wm_interm_66_pt3(b,a,i,j)
term(221) = term(221) + r1(vrdav_Rl, a,i) * wm_interm_14_pt3(b,p,i,j) * wm_interm_67_pt3(b,a,q,j)
term(222) = term(222) + r1(vrdav_Rl, a,i) * wm_interm_15_pt3(b,p,i,j) * wm_interm_67_pt3(b,a,q,j)
term(223) = term(223) + r1(vrdav_Rl, a,q) * wm_interm_14_pt3(b,p,i,j) * wm_interm_67_pt3(b,a,i,j)
term(224) = term(224) + r1(vrdav_Rl, a,q) * wm_interm_15_pt3(b,p,i,j) * wm_interm_67_pt3(b,a,i,j)
term(225) = term(225) + r1(vrdav_Rl, a,i) * wm_interm_14_pt3(b,p,i,j) * wm_interm_68_pt3(b,a,q,j)
term(226) = term(226) + r1(vrdav_Rl, a,i) * wm_interm_15_pt3(b,p,i,j) * wm_interm_68_pt3(b,a,q,j)
term(227) = term(227) + r1(vrdav_Rl, a,q) * wm_interm_14_pt3(b,p,i,j) * wm_interm_68_pt3(b,a,i,j)
term(228) = term(228) + r1(vrdav_Rl, a,q) * wm_interm_15_pt3(b,p,i,j) * wm_interm_68_pt3(b,a,i,j)
term(229) = term(229) + r1(vrdav_Rl, a,q) * wm_interm_17_pt3(b,p,i,j) * wm_interm_67_pt3(b,a,i,j)
term(230) = term(230) + r1(vrdav_Rl, a,q) * wm_interm_16_pt3(b,p,i,j) * wm_interm_67_pt3(b,a,i,j)
term(231) = term(231) + r1(vrdav_Rl, a,q) * wm_interm_17_pt3(b,p,i,j) * wm_interm_68_pt3(b,a,i,j)
term(232) = term(232) + r1(vrdav_Rl, a,q) * wm_interm_16_pt3(b,p,i,j) * wm_interm_68_pt3(b,a,i,j)
term(233) = term(233) + r1(vrdav_Rl, a,i) * wm_interm_16_pt3(b,p,i,j) * wm_interm_67_pt3(b,a,q,j)
term(234) = term(234) + r1(vrdav_Rl, a,i) * wm_interm_17_pt3(b,p,i,j) * wm_interm_67_pt3(b,a,q,j)
term(235) = term(235) + r1(vrdav_Rl, a,i) * wm_interm_16_pt3(b,p,i,j) * wm_interm_68_pt3(b,a,q,j)
term(236) = term(236) + r1(vrdav_Rl, a,i) * wm_interm_17_pt3(b,p,i,j) * wm_interm_68_pt3(b,a,q,j)
end do 
end do 
end do 
end do 

term(205) = term(205) * (4.0d+0) 
term(206) = term(206) * (-8.0d+0) 
term(207) = term(207) * (-8.0d+0) 
term(208) = term(208) * (16.0d+0) 
term(209) = term(209) * (4.0d+0) 
term(210) = term(210) * (-8.0d+0) 
term(211) = term(211) * (4.0d+0) 
term(212) = term(212) * (-8.0d+0) 
term(213) = term(213) * (4.0d+0) 
term(214) = term(214) * (-8.0d+0) 
term(215) = term(215) * (-8.0d+0) 
term(216) = term(216) * (16.0d+0) 
term(217) = term(217) * (4.0d+0) 
term(218) = term(218) * (-8.0d+0) 
term(219) = term(219) * (4.0d+0) 
term(220) = term(220) * (-8.0d+0) 
term(221) = term(221) * (4.0d+0) 
term(222) = term(222) * (-8.0d+0) 
term(223) = term(223) * (-8.0d+0) 
term(224) = term(224) * (16.0d+0) 
term(225) = term(225) * (-8.0d+0) 
term(226) = term(226) * (16.0d+0) 
term(227) = term(227) * (16.0d+0) 
term(228) = term(228) * (-32.0d+0) 
term(229) = term(229) * (4.0d+0) 
term(230) = term(230) * (-8.0d+0) 
term(231) = term(231) * (-8.0d+0) 
term(232) = term(232) * (16.0d+0) 
term(233) = term(233) * (4.0d+0) 
term(234) = term(234) * (-8.0d+0) 
term(235) = term(235) * (-8.0d+0) 
term(236) = term(236) * (16.0d+0) 

do k = 1, nocc 
do l = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(237) = term(237) + r1(vrdav_Rr, p,i) * wm_interm_38_pt3(j,k,l,q) * wm_interm_65_pt3(i,l,j,k)
term(238) = term(238) + r1(vrdav_Rr, p,i) * wm_interm_38_pt3(j,k,l,q) * wm_interm_65_pt3(l,i,j,k)
end do 
end do 
end do 
end do 

term(237) = term(237) * (-1.0d+0) 
term(238) = term(238) * (2.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(239) = term(239) + s2(a,p,j,i) * wm_interm_13_pt3(j,i,q,k) * wm_interm_61_pt3(a,k)
term(240) = term(240) + s2(a,p,j,i) * wm_interm_13_pt3(i,j,q,k) * wm_interm_61_pt3(a,k)
term(241) = term(241) + s2(a,p,j,i) * wm_interm_13_pt3(j,i,q,k) * wm_interm_59_pt3(a,k)
term(242) = term(242) + s2(a,p,j,i) * wm_interm_13_pt3(i,j,q,k) * wm_interm_59_pt3(a,k)
term(243) = term(243) + t2(a,p,j,i) * wm_interm_38_pt3(j,i,q,k) * wm_interm_55_pt3(a,k)
term(244) = term(244) + t2(a,p,j,i) * wm_interm_38_pt3(i,j,q,k) * wm_interm_55_pt3(a,k)
term(245) = term(245) + t2(a,p,j,i) * wm_interm_38_pt3(j,i,q,k) * wm_interm_56_pt3(a,k)
term(246) = term(246) + t2(a,p,j,i) * wm_interm_38_pt3(i,j,q,k) * wm_interm_56_pt3(a,k)
end do 
end do 
end do 
end do 

term(239) = term(239) * (-4.0d+0) 
term(240) = term(240) * (8.0d+0) 
term(241) = term(241) * (2.0d+0) 
term(242) = term(242) * (-4.0d+0) 
term(243) = term(243) * (-2.0d+0) 
term(244) = term(244) * (4.0d+0) 
term(245) = term(245) * (4.0d+0) 
term(246) = term(246) * (-8.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(247) = term(247) + r1(vrdav_Rr, p,i) * wm_interm_38_pt3(j,k,q,l) * wm_interm_65_pt3(l,i,j,k)
term(248) = term(248) + r1(vrdav_Rr, p,i) * wm_interm_38_pt3(j,k,q,l) * wm_interm_65_pt3(i,l,j,k)
term(249) = term(249) + r1(vrdav_Rr, p,i) * wm_interm_38_pt3(j,i,k,l) * wm_interm_65_pt3(l,k,j,q)
term(250) = term(250) + r1(vrdav_Rr, p,i) * wm_interm_38_pt3(i,j,k,l) * wm_interm_65_pt3(l,k,j,q)
term(251) = term(251) + r1(vrdav_Rr, p,i) * wm_interm_38_pt3(i,j,k,l) * wm_interm_65_pt3(k,l,j,q)
term(252) = term(252) + r1(vrdav_Rr, p,i) * wm_interm_38_pt3(j,i,k,l) * wm_interm_65_pt3(k,l,j,q)
end do 
end do 
end do 
end do 

term(247) = term(247) * (-1.0d+0) 
term(248) = term(248) * (2.0d+0) 
term(249) = term(249) * (-1.0d+0) 
term(250) = term(250) * (2.0d+0) 
term(251) = term(251) * (-1.0d+0) 
term(252) = term(252) * (2.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(253) = term(253) + s2(a,p,q,i) * wm_interm_43_pt3(b,j,k,i) * wm_interm_64_pt3(b,a,k,j)
term(254) = term(254) + s2(a,p,q,i) * wm_interm_43_pt3(b,j,k,i) * wm_interm_66_pt3(b,a,k,j)
term(255) = term(255) + s2(a,p,i,q) * wm_interm_43_pt3(b,j,k,i) * wm_interm_64_pt3(b,a,k,j)
term(256) = term(256) + s2(a,p,i,q) * wm_interm_43_pt3(b,j,k,i) * wm_interm_66_pt3(b,a,k,j)
term(257) = term(257) + s2(a,p,q,i) * wm_interm_43_pt3(b,j,k,i) * wm_interm_67_pt3(b,a,k,j)
term(258) = term(258) + s2(a,p,q,i) * wm_interm_43_pt3(b,j,k,i) * wm_interm_68_pt3(b,a,k,j)
term(259) = term(259) + s2(a,p,i,q) * wm_interm_43_pt3(b,j,k,i) * wm_interm_67_pt3(b,a,k,j)
term(260) = term(260) + s2(a,p,i,q) * wm_interm_43_pt3(b,j,k,i) * wm_interm_68_pt3(b,a,k,j)
end do 
end do 
end do 
end do 
end do 

term(253) = term(253) * (4.0d+0) 
term(254) = term(254) * (-8.0d+0) 
term(255) = term(255) * (-8.0d+0) 
term(256) = term(256) * (16.0d+0) 
term(257) = term(257) * (4.0d+0) 
term(258) = term(258) * (-8.0d+0) 
term(259) = term(259) * (-8.0d+0) 
term(260) = term(260) * (16.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(261) = term(261) + s2(a,p,i,q) * wm_interm_17_pt3(a,b,j,k) * wm_interm_63_pt3(b,j,i,k)
term(262) = term(262) + s2(a,p,i,q) * wm_interm_16_pt3(a,b,j,k) * wm_interm_63_pt3(b,j,i,k)
term(263) = term(263) + s2(a,p,i,q) * wm_interm_14_pt3(a,b,j,k) * wm_interm_63_pt3(b,j,i,k)
term(264) = term(264) + s2(a,p,i,q) * wm_interm_15_pt3(a,b,j,k) * wm_interm_63_pt3(b,j,i,k)
term(265) = term(265) + t2(a,p,i,q) * wm_interm_34_pt3(a,b,j,k) * wm_interm_58_pt3(b,j,i,k)
term(266) = term(266) + t2(a,p,i,q) * wm_interm_33_pt3(a,b,j,k) * wm_interm_58_pt3(b,j,i,k)
term(267) = term(267) + t2(a,p,i,q) * wm_interm_48_pt3(b,j,i,k) * wm_interm_67_pt3(a,b,k,j)
term(268) = term(268) + t2(a,p,i,q) * wm_interm_48_pt3(b,j,i,k) * wm_interm_68_pt3(a,b,k,j)
term(269) = term(269) + t2(a,p,i,q) * wm_interm_52_pt3(b,j,i,k) * wm_interm_67_pt3(a,b,k,j)
term(270) = term(270) + t2(a,p,i,q) * wm_interm_52_pt3(b,j,i,k) * wm_interm_68_pt3(a,b,k,j)
term(271) = term(271) + t2(a,p,i,q) * wm_interm_35_pt3(a,b,j,k) * wm_interm_58_pt3(b,j,i,k)
term(272) = term(272) + t2(a,p,i,q) * wm_interm_36_pt3(a,b,j,k) * wm_interm_58_pt3(b,j,i,k)
term(273) = term(273) + t2(a,p,i,q) * wm_interm_48_pt3(b,j,i,k) * wm_interm_66_pt3(a,b,k,j)
term(274) = term(274) + t2(a,p,i,q) * wm_interm_48_pt3(b,j,i,k) * wm_interm_64_pt3(a,b,k,j)
term(275) = term(275) + t2(a,p,i,q) * wm_interm_52_pt3(b,j,i,k) * wm_interm_66_pt3(a,b,k,j)
term(276) = term(276) + t2(a,p,i,q) * wm_interm_52_pt3(b,j,i,k) * wm_interm_64_pt3(a,b,k,j)
term(277) = term(277) + t2(a,p,q,i) * wm_interm_48_pt3(b,i,j,k) * wm_interm_67_pt3(a,b,k,j)
term(278) = term(278) + t2(a,p,q,i) * wm_interm_48_pt3(b,i,j,k) * wm_interm_68_pt3(a,b,k,j)
term(279) = term(279) + t2(a,p,q,i) * wm_interm_48_pt3(b,j,i,k) * wm_interm_67_pt3(a,b,k,j)
term(280) = term(280) + t2(a,p,q,i) * wm_interm_48_pt3(b,j,i,k) * wm_interm_68_pt3(a,b,k,j)
term(281) = term(281) + t2(a,p,q,i) * wm_interm_52_pt3(b,i,j,k) * wm_interm_67_pt3(a,b,k,j)
term(282) = term(282) + t2(a,p,q,i) * wm_interm_52_pt3(b,i,j,k) * wm_interm_68_pt3(a,b,k,j)
term(283) = term(283) + t2(a,p,q,i) * wm_interm_52_pt3(b,j,i,k) * wm_interm_67_pt3(a,b,k,j)
term(284) = term(284) + t2(a,p,q,i) * wm_interm_52_pt3(b,j,i,k) * wm_interm_68_pt3(a,b,k,j)
term(285) = term(285) + t2(a,p,q,i) * wm_interm_52_pt3(b,i,j,k) * wm_interm_64_pt3(a,b,k,j)
term(286) = term(286) + t2(a,p,q,i) * wm_interm_52_pt3(b,i,j,k) * wm_interm_66_pt3(a,b,k,j)
term(287) = term(287) + t2(a,p,q,i) * wm_interm_48_pt3(b,i,j,k) * wm_interm_66_pt3(a,b,k,j)
term(288) = term(288) + t2(a,p,q,i) * wm_interm_48_pt3(b,i,j,k) * wm_interm_64_pt3(a,b,k,j)
term(289) = term(289) + t2(a,p,q,i) * wm_interm_48_pt3(b,j,i,k) * wm_interm_66_pt3(a,b,k,j)
term(290) = term(290) + t2(a,p,q,i) * wm_interm_48_pt3(b,j,i,k) * wm_interm_64_pt3(a,b,k,j)
term(291) = term(291) + t2(a,p,q,i) * wm_interm_52_pt3(b,j,i,k) * wm_interm_66_pt3(a,b,k,j)
term(292) = term(292) + t2(a,p,q,i) * wm_interm_52_pt3(b,j,i,k) * wm_interm_64_pt3(a,b,k,j)
end do 
end do 
end do 
end do 
end do 

term(261) = term(261) * (-8.0d+0) 
term(262) = term(262) * (16.0d+0) 
term(263) = term(263) * (16.0d+0) 
term(264) = term(264) * (-32.0d+0) 
term(265) = term(265) * (16.0d+0) 
term(266) = term(266) * (-8.0d+0) 
term(267) = term(267) * (2.0d+0) 
term(268) = term(268) * (-4.0d+0) 
term(269) = term(269) * (-4.0d+0) 
term(270) = term(270) * (8.0d+0) 
term(271) = term(271) * (4.0d+0) 
term(272) = term(272) * (-8.0d+0) 
term(273) = term(273) * (-4.0d+0) 
term(274) = term(274) * (2.0d+0) 
term(275) = term(275) * (2.0d+0) 
term(276) = term(276) * (-4.0d+0) 
term(277) = term(277) * (2.0d+0) 
term(278) = term(278) * (-4.0d+0) 
term(279) = term(279) * (-1.0d+0) 
term(280) = term(280) * (2.0d+0) 
term(281) = term(281) * (-1.0d+0) 
term(282) = term(282) * (2.0d+0) 
term(283) = term(283) * (2.0d+0) 
term(284) = term(284) * (-4.0d+0) 
term(285) = term(285) * (-1.0d+0) 
term(286) = term(286) * (2.0d+0) 
term(287) = term(287) * (-1.0d+0) 
term(288) = term(288) * (2.0d+0) 
term(289) = term(289) * (2.0d+0) 
term(290) = term(290) * (-1.0d+0) 
term(291) = term(291) * (-1.0d+0) 
term(292) = term(292) * (2.0d+0) 

do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(293) = term(293) + r1(vrdav_Rr, a,i) * wm_interm_38_pt3(j,i,k,q) * wm_interm_67_pt3(a,p,k,j)
term(294) = term(294) + r1(vrdav_Rr, a,i) * wm_interm_38_pt3(i,j,k,q) * wm_interm_67_pt3(a,p,k,j)
term(295) = term(295) + r1(vrdav_Rr, a,i) * wm_interm_38_pt3(j,i,k,q) * wm_interm_64_pt3(a,p,k,j)
term(296) = term(296) + r1(vrdav_Rr, a,i) * wm_interm_38_pt3(j,i,k,q) * wm_interm_68_pt3(a,p,k,j)
term(297) = term(297) + r1(vrdav_Rr, a,i) * wm_interm_38_pt3(i,j,k,q) * wm_interm_64_pt3(a,p,k,j)
term(298) = term(298) + r1(vrdav_Rr, a,i) * wm_interm_38_pt3(i,j,k,q) * wm_interm_68_pt3(a,p,k,j)
term(299) = term(299) + r1(vrdav_Rr, a,i) * wm_interm_38_pt3(j,i,k,q) * wm_interm_66_pt3(a,p,k,j)
term(300) = term(300) + r1(vrdav_Rr, a,i) * wm_interm_38_pt3(i,j,k,q) * wm_interm_66_pt3(a,p,k,j)
end do 
end do 
end do 
end do 

term(293) = term(293) * (-2.0d+0) 
term(294) = term(294) * (4.0d+0) 
term(295) = term(295) * (-2.0d+0) 
term(296) = term(296) * (4.0d+0) 
term(297) = term(297) * (4.0d+0) 
term(298) = term(298) * (-8.0d+0) 
term(299) = term(299) * (4.0d+0) 
term(300) = term(300) * (-2.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(301) = term(301) + s2(a,p,i,q) * wm_interm_13_pt3(j,i,k,l) * wm_interm_63_pt3(a,j,l,k)
term(302) = term(302) + s2(a,p,i,q) * wm_interm_13_pt3(j,i,k,l) * wm_interm_63_pt3(a,j,k,l)
term(303) = term(303) + t2(a,p,i,q) * wm_interm_52_pt3(a,j,k,l) * wm_interm_65_pt3(l,i,j,k)
term(304) = term(304) + t2(a,p,i,q) * wm_interm_52_pt3(a,j,k,l) * wm_interm_65_pt3(i,l,j,k)
term(305) = term(305) + t2(a,p,i,q) * wm_interm_48_pt3(a,j,k,l) * wm_interm_65_pt3(i,l,j,k)
term(306) = term(306) + t2(a,p,i,q) * wm_interm_48_pt3(a,j,k,l) * wm_interm_65_pt3(l,i,j,k)
term(307) = term(307) + t2(a,p,i,q) * wm_interm_38_pt3(i,j,k,l) * wm_interm_58_pt3(a,j,k,l)
term(308) = term(308) + t2(a,p,i,q) * wm_interm_38_pt3(j,i,k,l) * wm_interm_58_pt3(a,j,k,l)
term(309) = term(309) + t2(a,p,i,q) * wm_interm_38_pt3(j,i,k,l) * wm_interm_58_pt3(a,j,l,k)
term(310) = term(310) + t2(a,p,i,q) * wm_interm_38_pt3(i,j,k,l) * wm_interm_58_pt3(a,j,l,k)
end do 
end do 
end do 
end do 
end do 

term(301) = term(301) * (-8.0d+0) 
term(302) = term(302) * (16.0d+0) 
term(303) = term(303) * (2.0d+0) 
term(304) = term(304) * (-4.0d+0) 
term(305) = term(305) * (2.0d+0) 
term(306) = term(306) * (-4.0d+0) 
term(307) = term(307) * (2.0d+0) 
term(308) = term(308) * (-4.0d+0) 
term(309) = term(309) * (2.0d+0) 
term(310) = term(310) * (-4.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(311) = term(311) + r2(vrdav_Rl, a,q,p,i) * t1(b,i) * wm_interm_3_pt3(a,b)
term(312) = term(312) + r2(vrdav_Rl, a,q,p,i) * t1(b,i) * wm_interm_4_pt3(a,b)
term(313) = term(313) + r2(vrdav_Rr, a,q,p,i) * s1(b,i) * wm_interm_23_pt3(a,b)
term(314) = term(314) + r2(vrdav_Rr, a,q,p,i) * s1(b,i) * wm_interm_24_pt3(a,b)
term(315) = term(315) + s1(b,i) * t2(a,p,q,i) * wm_interm_18_pt3(a,b)
term(316) = term(316) + s1(b,i) * t2(a,p,q,i) * wm_interm_19_pt3(a,b)
term(317) = term(317) + r1(vrdav_Rl, a,i) * wm_interm_3_pt3(a,b) * wm_interm_66_pt3(p,b,q,i)
term(318) = term(318) + r1(vrdav_Rl, a,i) * wm_interm_4_pt3(a,b) * wm_interm_66_pt3(p,b,q,i)
term(319) = term(319) + r1(vrdav_Rl, a,i) * wm_interm_3_pt3(a,b) * wm_interm_64_pt3(p,b,q,i)
term(320) = term(320) + r1(vrdav_Rl, a,i) * wm_interm_4_pt3(a,b) * wm_interm_64_pt3(p,b,q,i)
term(321) = term(321) + r1(vrdav_Rl, a,i) * wm_interm_3_pt3(a,b) * wm_interm_67_pt3(p,b,q,i)
term(322) = term(322) + r1(vrdav_Rl, a,i) * wm_interm_4_pt3(a,b) * wm_interm_67_pt3(p,b,q,i)
term(323) = term(323) + r1(vrdav_Rl, a,i) * wm_interm_3_pt3(a,b) * wm_interm_68_pt3(p,b,q,i)
term(324) = term(324) + r1(vrdav_Rl, a,i) * wm_interm_4_pt3(a,b) * wm_interm_68_pt3(p,b,q,i)
term(325) = term(325) + s2(a,p,q,i) * wm_interm_3_pt3(a,b) * wm_interm_61_pt3(b,i)
term(326) = term(326) + s2(a,p,q,i) * wm_interm_4_pt3(a,b) * wm_interm_61_pt3(b,i)
term(327) = term(327) + s2(a,p,q,i) * wm_interm_3_pt3(a,b) * wm_interm_59_pt3(b,i)
term(328) = term(328) + s2(a,p,q,i) * wm_interm_4_pt3(a,b) * wm_interm_59_pt3(b,i)
term(329) = term(329) + s2(a,b,q,i) * wm_interm_3_pt3(a,p) * wm_interm_61_pt3(b,i)
term(330) = term(330) + s2(a,b,q,i) * wm_interm_4_pt3(a,p) * wm_interm_61_pt3(b,i)
term(331) = term(331) + s2(a,b,q,i) * wm_interm_3_pt3(a,p) * wm_interm_59_pt3(b,i)
term(332) = term(332) + s2(a,b,q,i) * wm_interm_4_pt3(a,p) * wm_interm_59_pt3(b,i)
term(333) = term(333) + r1(vrdav_Rr, p,i) * wm_interm_33_pt3(a,b,i,q) * wm_interm_71_pt3(b,a)
term(334) = term(334) + r1(vrdav_Rr, p,i) * wm_interm_34_pt3(a,b,i,q) * wm_interm_71_pt3(b,a)
term(335) = term(335) + r1(vrdav_Rr, p,i) * wm_interm_33_pt3(a,b,i,q) * wm_interm_72_pt3(b,a)
term(336) = term(336) + r1(vrdav_Rr, p,i) * wm_interm_34_pt3(a,b,i,q) * wm_interm_72_pt3(b,a)
term(337) = term(337) + r1(vrdav_Rr, p,i) * wm_interm_36_pt3(a,b,i,q) * wm_interm_71_pt3(b,a)
term(338) = term(338) + r1(vrdav_Rr, p,i) * wm_interm_36_pt3(a,b,i,q) * wm_interm_72_pt3(b,a)
term(339) = term(339) + r1(vrdav_Rr, p,i) * wm_interm_35_pt3(a,b,i,q) * wm_interm_71_pt3(b,a)
term(340) = term(340) + r1(vrdav_Rr, p,i) * wm_interm_35_pt3(a,b,i,q) * wm_interm_72_pt3(b,a)
term(341) = term(341) + r1(vrdav_Rr, p,i) * wm_interm_23_pt3(a,b) * wm_interm_64_pt3(b,a,i,q)
term(342) = term(342) + r1(vrdav_Rr, p,i) * wm_interm_23_pt3(a,b) * wm_interm_66_pt3(b,a,i,q)
term(343) = term(343) + r1(vrdav_Rr, p,i) * wm_interm_23_pt3(a,b) * wm_interm_67_pt3(b,a,i,q)
term(344) = term(344) + r1(vrdav_Rr, p,i) * wm_interm_23_pt3(a,b) * wm_interm_68_pt3(b,a,i,q)
term(345) = term(345) + r1(vrdav_Rr, p,i) * wm_interm_24_pt3(a,b) * wm_interm_64_pt3(b,a,i,q)
term(346) = term(346) + r1(vrdav_Rr, p,i) * wm_interm_24_pt3(a,b) * wm_interm_66_pt3(b,a,i,q)
term(347) = term(347) + r1(vrdav_Rr, p,i) * wm_interm_24_pt3(a,b) * wm_interm_67_pt3(b,a,i,q)
term(348) = term(348) + r1(vrdav_Rr, p,i) * wm_interm_24_pt3(a,b) * wm_interm_68_pt3(b,a,i,q)
term(349) = term(349) + r1(vrdav_Rr, a,i) * wm_interm_33_pt3(b,p,i,q) * wm_interm_71_pt3(a,b)
term(350) = term(350) + r1(vrdav_Rr, a,i) * wm_interm_34_pt3(b,p,i,q) * wm_interm_71_pt3(a,b)
term(351) = term(351) + r1(vrdav_Rr, a,i) * wm_interm_33_pt3(b,p,i,q) * wm_interm_72_pt3(a,b)
term(352) = term(352) + r1(vrdav_Rr, a,i) * wm_interm_34_pt3(b,p,i,q) * wm_interm_72_pt3(a,b)
term(353) = term(353) + r1(vrdav_Rr, a,i) * wm_interm_23_pt3(a,b) * wm_interm_67_pt3(b,p,i,q)
term(354) = term(354) + r1(vrdav_Rr, a,i) * wm_interm_23_pt3(a,b) * wm_interm_68_pt3(b,p,i,q)
term(355) = term(355) + r1(vrdav_Rr, a,i) * wm_interm_24_pt3(a,b) * wm_interm_67_pt3(b,p,i,q)
term(356) = term(356) + r1(vrdav_Rr, a,i) * wm_interm_24_pt3(a,b) * wm_interm_68_pt3(b,p,i,q)
term(357) = term(357) + r1(vrdav_Rr, a,i) * wm_interm_35_pt3(b,p,i,q) * wm_interm_71_pt3(a,b)
term(358) = term(358) + r1(vrdav_Rr, a,i) * wm_interm_36_pt3(b,p,i,q) * wm_interm_71_pt3(a,b)
term(359) = term(359) + r1(vrdav_Rr, a,i) * wm_interm_35_pt3(b,p,i,q) * wm_interm_72_pt3(a,b)
term(360) = term(360) + r1(vrdav_Rr, a,i) * wm_interm_36_pt3(b,p,i,q) * wm_interm_72_pt3(a,b)
term(361) = term(361) + t2(a,p,q,i) * wm_interm_51_pt3(b,i) * wm_interm_71_pt3(a,b)
term(362) = term(362) + t2(a,p,q,i) * wm_interm_51_pt3(b,i) * wm_interm_72_pt3(a,b)
term(363) = term(363) + t2(a,p,q,i) * wm_interm_49_pt3(b,i) * wm_interm_71_pt3(a,b)
term(364) = term(364) + t2(a,p,q,i) * wm_interm_49_pt3(b,i) * wm_interm_72_pt3(a,b)
term(365) = term(365) + t2(a,p,q,i) * wm_interm_23_pt3(a,b) * wm_interm_55_pt3(b,i)
term(366) = term(366) + t2(a,p,q,i) * wm_interm_23_pt3(a,b) * wm_interm_56_pt3(b,i)
term(367) = term(367) + r1(vrdav_Rr, a,i) * wm_interm_23_pt3(a,b) * wm_interm_66_pt3(b,p,i,q)
term(368) = term(368) + r1(vrdav_Rr, a,i) * wm_interm_23_pt3(a,b) * wm_interm_64_pt3(b,p,i,q)
term(369) = term(369) + t2(a,p,q,i) * wm_interm_24_pt3(a,b) * wm_interm_55_pt3(b,i)
term(370) = term(370) + t2(a,p,q,i) * wm_interm_24_pt3(a,b) * wm_interm_56_pt3(b,i)
term(371) = term(371) + r1(vrdav_Rr, a,i) * wm_interm_24_pt3(a,b) * wm_interm_66_pt3(b,p,i,q)
term(372) = term(372) + r1(vrdav_Rr, a,i) * wm_interm_24_pt3(a,b) * wm_interm_64_pt3(b,p,i,q)
term(373) = term(373) + t2(a,b,q,i) * wm_interm_51_pt3(b,i) * wm_interm_71_pt3(a,p)
term(374) = term(374) + t2(a,b,q,i) * wm_interm_49_pt3(b,i) * wm_interm_71_pt3(a,p)
term(375) = term(375) + t2(a,b,q,i) * wm_interm_51_pt3(b,i) * wm_interm_72_pt3(a,p)
term(376) = term(376) + t2(a,b,q,i) * wm_interm_49_pt3(b,i) * wm_interm_72_pt3(a,p)
term(377) = term(377) + t2(a,b,q,i) * wm_interm_24_pt3(a,p) * wm_interm_55_pt3(b,i)
term(378) = term(378) + t2(a,b,q,i) * wm_interm_23_pt3(a,p) * wm_interm_55_pt3(b,i)
term(379) = term(379) + t2(a,b,q,i) * wm_interm_24_pt3(a,p) * wm_interm_56_pt3(b,i)
term(380) = term(380) + t2(a,b,q,i) * wm_interm_23_pt3(a,p) * wm_interm_56_pt3(b,i)
end do 
end do 
end do 

term(311) = term(311) * (-2.0d+0) 
term(312) = term(312) * (4.0d+0) 
term(313) = term(313) * (4.0d+0) 
term(314) = term(314) * (-2.0d+0) 
term(315) = term(315) * (-2.0d+0) 
term(316) = term(316) * (4.0d+0) 
term(317) = term(317) * (4.0d+0) 
term(318) = term(318) * (-8.0d+0) 
term(319) = term(319) * (-8.0d+0) 
term(320) = term(320) * (16.0d+0) 
term(321) = term(321) * (-8.0d+0) 
term(322) = term(322) * (16.0d+0) 
term(323) = term(323) * (16.0d+0) 
term(324) = term(324) * (-32.0d+0) 
term(325) = term(325) * (-8.0d+0) 
term(326) = term(326) * (16.0d+0) 
term(327) = term(327) * (4.0d+0) 
term(328) = term(328) * (-8.0d+0) 
term(329) = term(329) * (-8.0d+0) 
term(330) = term(330) * (16.0d+0) 
term(331) = term(331) * (4.0d+0) 
term(332) = term(332) * (-8.0d+0) 
term(333) = term(333) * (-2.0d+0) 
term(334) = term(334) * (4.0d+0) 
term(335) = term(335) * (4.0d+0) 
term(336) = term(336) * (-8.0d+0) 
term(337) = term(337) * (-2.0d+0) 
term(338) = term(338) * (4.0d+0) 
term(339) = term(339) * (4.0d+0) 
term(340) = term(340) * (-8.0d+0) 
term(341) = term(341) * (4.0d+0) 
term(342) = term(342) * (-8.0d+0) 
term(343) = term(343) * (4.0d+0) 
term(344) = term(344) * (-8.0d+0) 
term(345) = term(345) * (-2.0d+0) 
term(346) = term(346) * (4.0d+0) 
term(347) = term(347) * (-2.0d+0) 
term(348) = term(348) * (4.0d+0) 
term(349) = term(349) * (4.0d+0) 
term(350) = term(350) * (-8.0d+0) 
term(351) = term(351) * (-8.0d+0) 
term(352) = term(352) * (16.0d+0) 
term(353) = term(353) * (-8.0d+0) 
term(354) = term(354) * (16.0d+0) 
term(355) = term(355) * (4.0d+0) 
term(356) = term(356) * (-8.0d+0) 
term(357) = term(357) * (-2.0d+0) 
term(358) = term(358) * (4.0d+0) 
term(359) = term(359) * (4.0d+0) 
term(360) = term(360) * (-8.0d+0) 
term(361) = term(361) * (-2.0d+0) 
term(362) = term(362) * (4.0d+0) 
term(363) = term(363) * (4.0d+0) 
term(364) = term(364) * (-8.0d+0) 
term(365) = term(365) * (4.0d+0) 
term(366) = term(366) * (-8.0d+0) 
term(367) = term(367) * (4.0d+0) 
term(368) = term(368) * (-8.0d+0) 
term(369) = term(369) * (-2.0d+0) 
term(370) = term(370) * (4.0d+0) 
term(371) = term(371) * (-2.0d+0) 
term(372) = term(372) * (4.0d+0) 
term(373) = term(373) * (8.0d+0) 
term(374) = term(374) * (-16.0d+0) 
term(375) = term(375) * (-16.0d+0) 
term(376) = term(376) * (32.0d+0) 
term(377) = term(377) * (8.0d+0) 
term(378) = term(378) * (-16.0d+0) 
term(379) = term(379) * (-16.0d+0) 
term(380) = term(380) * (32.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(381) = term(381) + t2(a,p,j,i) * wm_interm_49_pt3(b,j) * wm_interm_67_pt3(a,b,i,q)
term(382) = term(382) + t2(a,p,j,i) * wm_interm_51_pt3(b,j) * wm_interm_67_pt3(a,b,i,q)
term(383) = term(383) + t2(a,p,j,i) * wm_interm_49_pt3(b,j) * wm_interm_68_pt3(a,b,i,q)
term(384) = term(384) + t2(a,p,j,i) * wm_interm_51_pt3(b,j) * wm_interm_68_pt3(a,b,i,q)
term(385) = term(385) + t2(a,p,j,i) * wm_interm_49_pt3(b,j) * wm_interm_64_pt3(a,b,i,q)
term(386) = term(386) + t2(a,p,j,i) * wm_interm_51_pt3(b,j) * wm_interm_64_pt3(a,b,i,q)
term(387) = term(387) + t2(a,p,j,i) * wm_interm_49_pt3(b,j) * wm_interm_66_pt3(a,b,i,q)
term(388) = term(388) + t2(a,p,j,i) * wm_interm_51_pt3(b,j) * wm_interm_66_pt3(a,b,i,q)
term(389) = term(389) + t2(a,p,j,i) * wm_interm_36_pt3(a,b,i,q) * wm_interm_56_pt3(b,j)
term(390) = term(390) + t2(a,p,j,i) * wm_interm_35_pt3(a,b,i,q) * wm_interm_56_pt3(b,j)
term(391) = term(391) + t2(a,p,j,i) * wm_interm_33_pt3(a,b,i,q) * wm_interm_56_pt3(b,j)
term(392) = term(392) + t2(a,p,j,i) * wm_interm_34_pt3(a,b,i,q) * wm_interm_56_pt3(b,j)
term(393) = term(393) + t2(a,p,j,i) * wm_interm_36_pt3(a,b,i,q) * wm_interm_55_pt3(b,j)
term(394) = term(394) + t2(a,p,j,i) * wm_interm_35_pt3(a,b,i,q) * wm_interm_55_pt3(b,j)
term(395) = term(395) + t2(a,p,j,i) * wm_interm_33_pt3(a,b,i,q) * wm_interm_55_pt3(b,j)
term(396) = term(396) + t2(a,p,j,i) * wm_interm_34_pt3(a,b,i,q) * wm_interm_55_pt3(b,j)
end do 
end do 
end do 
end do 

term(381) = term(381) * (8.0d+0) 
term(382) = term(382) * (-4.0d+0) 
term(383) = term(383) * (-16.0d+0) 
term(384) = term(384) * (8.0d+0) 
term(385) = term(385) * (8.0d+0) 
term(386) = term(386) * (-4.0d+0) 
term(387) = term(387) * (-16.0d+0) 
term(388) = term(388) * (8.0d+0) 
term(389) = term(389) * (4.0d+0) 
term(390) = term(390) * (-8.0d+0) 
term(391) = term(391) * (4.0d+0) 
term(392) = term(392) * (-8.0d+0) 
term(393) = term(393) * (-2.0d+0) 
term(394) = term(394) * (4.0d+0) 
term(395) = term(395) * (-2.0d+0) 
term(396) = term(396) * (4.0d+0) 

do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
term(397) = term(397) + r1(vrdav_Rr, p,i) * wm_interm_25_pt3(j,k) * wm_interm_65_pt3(k,i,j,q)
term(398) = term(398) + r1(vrdav_Rr, p,i) * wm_interm_25_pt3(j,k) * wm_interm_65_pt3(i,k,j,q)
term(399) = term(399) + r1(vrdav_Rr, p,i) * wm_interm_28_pt3(j,k) * wm_interm_65_pt3(k,i,j,q)
term(400) = term(400) + r1(vrdav_Rr, p,i) * wm_interm_28_pt3(j,k) * wm_interm_65_pt3(i,k,j,q)
end do 
end do 
end do 

term(397) = term(397) * (4.0d+0) 
term(398) = term(398) * (-2.0d+0) 
term(399) = term(399) * (-8.0d+0) 
term(400) = term(400) * (4.0d+0) 

do a = nocc + 1, nactive 
do i = 1, nocc 
do b = nocc + 1, nactive 
term(401) = term(401) + s2(a,p,i,q) * t1(b,i) * wm_interm_18_pt3(b,a)
term(402) = term(402) + s2(a,p,i,q) * t1(b,i) * wm_interm_19_pt3(b,a)
term(403) = term(403) + s1(a,i) * t2(b,p,i,q) * wm_interm_18_pt3(b,a)
term(404) = term(404) + s1(a,i) * t2(b,p,i,q) * wm_interm_19_pt3(b,a)
end do 
end do 
end do 

term(401) = term(401) * (4.0d+0) 
term(402) = term(402) * (-8.0d+0) 
term(403) = term(403) * (4.0d+0) 
term(404) = term(404) * (-8.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(405) = term(405) + s2(a,p,q,i) * wm_interm_14_pt3(a,b,j,k) * wm_interm_63_pt3(b,j,k,i)
term(406) = term(406) + s2(a,p,q,i) * wm_interm_15_pt3(a,b,j,k) * wm_interm_63_pt3(b,j,k,i)
term(407) = term(407) + s2(a,p,i,q) * wm_interm_14_pt3(a,b,j,k) * wm_interm_63_pt3(b,j,k,i)
term(408) = term(408) + s2(a,p,i,q) * wm_interm_15_pt3(a,b,j,k) * wm_interm_63_pt3(b,j,k,i)
term(409) = term(409) + s2(a,p,q,i) * wm_interm_16_pt3(a,b,j,k) * wm_interm_63_pt3(b,j,k,i)
term(410) = term(410) + s2(a,p,q,i) * wm_interm_17_pt3(a,b,j,k) * wm_interm_63_pt3(b,j,k,i)
term(411) = term(411) + s2(a,p,i,q) * wm_interm_16_pt3(a,b,j,k) * wm_interm_63_pt3(b,j,k,i)
term(412) = term(412) + s2(a,p,i,q) * wm_interm_17_pt3(a,b,j,k) * wm_interm_63_pt3(b,j,k,i)
term(413) = term(413) + s2(a,p,q,i) * wm_interm_17_pt3(a,b,j,k) * wm_interm_63_pt3(b,j,i,k)
term(414) = term(414) + s2(a,p,q,i) * wm_interm_16_pt3(a,b,j,k) * wm_interm_63_pt3(b,j,i,k)
term(415) = term(415) + s2(a,p,q,i) * wm_interm_14_pt3(a,b,j,k) * wm_interm_63_pt3(b,j,i,k)
term(416) = term(416) + s2(a,p,q,i) * wm_interm_15_pt3(a,b,j,k) * wm_interm_63_pt3(b,j,i,k)
term(417) = term(417) + t2(a,p,i,q) * wm_interm_34_pt3(a,b,j,k) * wm_interm_58_pt3(b,j,k,i)
term(418) = term(418) + t2(a,p,i,q) * wm_interm_33_pt3(a,b,j,k) * wm_interm_58_pt3(b,j,k,i)
term(419) = term(419) + t2(a,p,i,q) * wm_interm_35_pt3(a,b,j,k) * wm_interm_58_pt3(b,j,k,i)
term(420) = term(420) + t2(a,p,i,q) * wm_interm_36_pt3(a,b,j,k) * wm_interm_58_pt3(b,j,k,i)
term(421) = term(421) + t2(a,p,q,i) * wm_interm_34_pt3(a,b,j,k) * wm_interm_58_pt3(b,j,k,i)
term(422) = term(422) + t2(a,p,q,i) * wm_interm_34_pt3(a,b,j,k) * wm_interm_58_pt3(b,j,i,k)
term(423) = term(423) + t2(a,p,q,i) * wm_interm_33_pt3(a,b,j,k) * wm_interm_58_pt3(b,j,k,i)
term(424) = term(424) + t2(a,p,q,i) * wm_interm_33_pt3(a,b,j,k) * wm_interm_58_pt3(b,j,i,k)
term(425) = term(425) + t2(a,p,q,i) * wm_interm_35_pt3(a,b,j,k) * wm_interm_58_pt3(b,j,i,k)
term(426) = term(426) + t2(a,p,q,i) * wm_interm_35_pt3(a,b,j,k) * wm_interm_58_pt3(b,j,k,i)
term(427) = term(427) + t2(a,p,q,i) * wm_interm_36_pt3(a,b,j,k) * wm_interm_58_pt3(b,j,k,i)
term(428) = term(428) + t2(a,p,q,i) * wm_interm_36_pt3(a,b,j,k) * wm_interm_58_pt3(b,j,i,k)
term(429) = term(429) + t2(a,p,j,i) * wm_interm_52_pt3(b,i,j,k) * wm_interm_67_pt3(a,b,k,q)
term(430) = term(430) + t2(a,p,j,i) * wm_interm_52_pt3(b,i,j,k) * wm_interm_68_pt3(a,b,k,q)
term(431) = term(431) + t2(a,p,j,i) * wm_interm_48_pt3(b,i,j,k) * wm_interm_67_pt3(a,b,k,q)
term(432) = term(432) + t2(a,p,j,i) * wm_interm_48_pt3(b,i,j,k) * wm_interm_68_pt3(a,b,k,q)
term(433) = term(433) + t2(a,p,j,i) * wm_interm_52_pt3(b,i,j,k) * wm_interm_64_pt3(a,b,k,q)
term(434) = term(434) + t2(a,p,j,i) * wm_interm_48_pt3(b,i,j,k) * wm_interm_64_pt3(a,b,k,q)
term(435) = term(435) + t2(a,p,j,i) * wm_interm_52_pt3(b,i,j,k) * wm_interm_66_pt3(a,b,k,q)
term(436) = term(436) + t2(a,p,j,i) * wm_interm_48_pt3(b,i,j,k) * wm_interm_66_pt3(a,b,k,q)
end do 
end do 
end do 
end do 
end do 

term(405) = term(405) * (4.0d+0) 
term(406) = term(406) * (-8.0d+0) 
term(407) = term(407) * (-8.0d+0) 
term(408) = term(408) * (16.0d+0) 
term(409) = term(409) * (4.0d+0) 
term(410) = term(410) * (-8.0d+0) 
term(411) = term(411) * (-8.0d+0) 
term(412) = term(412) * (16.0d+0) 
term(413) = term(413) * (4.0d+0) 
term(414) = term(414) * (-8.0d+0) 
term(415) = term(415) * (-8.0d+0) 
term(416) = term(416) * (16.0d+0) 
term(417) = term(417) * (-8.0d+0) 
term(418) = term(418) * (4.0d+0) 
term(419) = term(419) * (-8.0d+0) 
term(420) = term(420) * (4.0d+0) 
term(421) = term(421) * (4.0d+0) 
term(422) = term(422) * (-8.0d+0) 
term(423) = term(423) * (-2.0d+0) 
term(424) = term(424) * (4.0d+0) 
term(425) = term(425) * (-2.0d+0) 
term(426) = term(426) * (4.0d+0) 
term(427) = term(427) * (-2.0d+0) 
term(428) = term(428) * (4.0d+0) 
term(429) = term(429) * (4.0d+0) 
term(430) = term(430) * (-8.0d+0) 
term(431) = term(431) * (-2.0d+0) 
term(432) = term(432) * (4.0d+0) 
term(433) = term(433) * (4.0d+0) 
term(434) = term(434) * (-2.0d+0) 
term(435) = term(435) * (-2.0d+0) 
term(436) = term(436) * (4.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(437) = term(437) + wm_interm_37_pt3(a,i,j,q) * wm_interm_6_pt3(a,p,j,i)
term(438) = term(438) + wm_interm_37_pt3(a,i,j,q) * wm_interm_7_pt3(a,p,j,i)
term(439) = term(439) + s2(a,p,q,i) * wm_interm_11_pt3(i,j) * wm_interm_61_pt3(a,j)
term(440) = term(440) + s2(a,p,q,i) * wm_interm_12_pt3(i,j) * wm_interm_61_pt3(a,j)
term(441) = term(441) + s2(a,p,q,i) * wm_interm_11_pt3(i,j) * wm_interm_59_pt3(a,j)
term(442) = term(442) + s2(a,p,q,i) * wm_interm_12_pt3(i,j) * wm_interm_59_pt3(a,j)
term(443) = term(443) + s2(a,p,j,i) * wm_interm_11_pt3(i,q) * wm_interm_61_pt3(a,j)
term(444) = term(444) + s2(a,p,j,i) * wm_interm_12_pt3(i,q) * wm_interm_61_pt3(a,j)
term(445) = term(445) + s2(a,p,j,i) * wm_interm_11_pt3(i,q) * wm_interm_59_pt3(a,j)
term(446) = term(446) + s2(a,p,j,i) * wm_interm_12_pt3(i,q) * wm_interm_59_pt3(a,j)
term(447) = term(447) + t2(a,p,q,i) * wm_interm_51_pt3(a,j) * wm_interm_69_pt3(i,j)
term(448) = term(448) + t2(a,p,q,i) * wm_interm_51_pt3(a,j) * wm_interm_70_pt3(i,j)
term(449) = term(449) + t2(a,p,q,i) * wm_interm_49_pt3(a,j) * wm_interm_69_pt3(i,j)
term(450) = term(450) + t2(a,p,q,i) * wm_interm_49_pt3(a,j) * wm_interm_70_pt3(i,j)
term(451) = term(451) + t2(a,p,q,i) * wm_interm_25_pt3(i,j) * wm_interm_56_pt3(a,j)
term(452) = term(452) + t2(a,p,q,i) * wm_interm_25_pt3(i,j) * wm_interm_55_pt3(a,j)
term(453) = term(453) + t2(a,p,q,i) * wm_interm_28_pt3(i,j) * wm_interm_56_pt3(a,j)
term(454) = term(454) + t2(a,p,q,i) * wm_interm_28_pt3(i,j) * wm_interm_55_pt3(a,j)
term(455) = term(455) + t2(a,p,j,i) * wm_interm_25_pt3(i,q) * wm_interm_55_pt3(a,j)
term(456) = term(456) + t2(a,p,j,i) * wm_interm_28_pt3(i,q) * wm_interm_55_pt3(a,j)
term(457) = term(457) + t2(a,p,j,i) * wm_interm_25_pt3(j,q) * wm_interm_55_pt3(a,i)
term(458) = term(458) + t2(a,p,j,i) * wm_interm_28_pt3(j,q) * wm_interm_55_pt3(a,i)
term(459) = term(459) + t2(a,p,j,i) * wm_interm_25_pt3(j,q) * wm_interm_56_pt3(a,i)
term(460) = term(460) + t2(a,p,j,i) * wm_interm_28_pt3(j,q) * wm_interm_56_pt3(a,i)
term(461) = term(461) + t2(a,p,j,i) * wm_interm_25_pt3(i,q) * wm_interm_56_pt3(a,j)
term(462) = term(462) + t2(a,p,j,i) * wm_interm_28_pt3(i,q) * wm_interm_56_pt3(a,j)
term(463) = term(463) + t2(a,p,j,i) * wm_interm_51_pt3(a,i) * wm_interm_69_pt3(j,q)
term(464) = term(464) + t2(a,p,j,i) * wm_interm_49_pt3(a,i) * wm_interm_69_pt3(j,q)
term(465) = term(465) + t2(a,p,j,i) * wm_interm_51_pt3(a,i) * wm_interm_70_pt3(j,q)
term(466) = term(466) + t2(a,p,j,i) * wm_interm_49_pt3(a,i) * wm_interm_70_pt3(j,q)
term(467) = term(467) + t2(a,p,j,i) * wm_interm_51_pt3(a,j) * wm_interm_69_pt3(i,q)
term(468) = term(468) + t2(a,p,j,i) * wm_interm_49_pt3(a,j) * wm_interm_69_pt3(i,q)
term(469) = term(469) + t2(a,p,j,i) * wm_interm_51_pt3(a,j) * wm_interm_70_pt3(i,q)
term(470) = term(470) + t2(a,p,j,i) * wm_interm_49_pt3(a,j) * wm_interm_70_pt3(i,q)
end do 
end do 
end do 

term(437) = term(437) * (4.0d+0) 
term(438) = term(438) * (-8.0d+0) 
term(439) = term(439) * (-8.0d+0) 
term(440) = term(440) * (16.0d+0) 
term(441) = term(441) * (4.0d+0) 
term(442) = term(442) * (-8.0d+0) 
term(443) = term(443) * (-8.0d+0) 
term(444) = term(444) * (16.0d+0) 
term(445) = term(445) * (4.0d+0) 
term(446) = term(446) * (-8.0d+0) 
term(447) = term(447) * (-2.0d+0) 
term(448) = term(448) * (4.0d+0) 
term(449) = term(449) * (4.0d+0) 
term(450) = term(450) * (-8.0d+0) 
term(451) = term(451) * (4.0d+0) 
term(452) = term(452) * (-2.0d+0) 
term(453) = term(453) * (-8.0d+0) 
term(454) = term(454) * (4.0d+0) 
term(455) = term(455) * (8.0d+0) 
term(456) = term(456) * (-16.0d+0) 
term(457) = term(457) * (-4.0d+0) 
term(458) = term(458) * (8.0d+0) 
term(459) = term(459) * (8.0d+0) 
term(460) = term(460) * (-16.0d+0) 
term(461) = term(461) * (-16.0d+0) 
term(462) = term(462) * (32.0d+0) 
term(463) = term(463) * (-4.0d+0) 
term(464) = term(464) * (8.0d+0) 
term(465) = term(465) * (8.0d+0) 
term(466) = term(466) * (-16.0d+0) 
term(467) = term(467) * (8.0d+0) 
term(468) = term(468) * (-16.0d+0) 
term(469) = term(469) * (-16.0d+0) 
term(470) = term(470) * (32.0d+0) 

term(471) = term(471) + wm_interm_21_pt3(p,q) * wm_interm_40_pt3
term(472) = term(472) + wm_interm_21_pt3(p,q) * wm_interm_41_pt3
term(473) = term(473) + wm_interm_22_pt3(p,q) * wm_interm_40_pt3
term(474) = term(474) + wm_interm_22_pt3(p,q) * wm_interm_41_pt3
term(475) = term(475) + wm_interm_21_pt3(p,q) * wm_interm_62_pt3
term(476) = term(476) + wm_interm_22_pt3(p,q) * wm_interm_62_pt3

term(471) = term(471) * (-4.0d+0) 
term(472) = term(472) * (8.0d+0) 
term(473) = term(473) * (2.0d+0) 
term(474) = term(474) * (-4.0d+0) 
term(475) = term(475) * (8.0d+0) 
term(476) = term(476) * (-4.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(477) = term(477) + s2(a,p,j,i) * wm_interm_17_pt3(a,b,k,q) * wm_interm_63_pt3(b,k,j,i)
term(478) = term(478) + s2(a,p,j,i) * wm_interm_16_pt3(a,b,k,q) * wm_interm_63_pt3(b,k,j,i)
term(479) = term(479) + s2(a,p,j,i) * wm_interm_14_pt3(a,b,k,q) * wm_interm_63_pt3(b,k,j,i)
term(480) = term(480) + s2(a,p,j,i) * wm_interm_15_pt3(a,b,k,q) * wm_interm_63_pt3(b,k,j,i)
term(481) = term(481) + t2(a,p,j,i) * wm_interm_36_pt3(a,b,k,q) * wm_interm_58_pt3(b,k,j,i)
term(482) = term(482) + t2(a,p,j,i) * wm_interm_35_pt3(a,b,k,q) * wm_interm_58_pt3(b,k,j,i)
term(483) = term(483) + t2(a,p,j,i) * wm_interm_33_pt3(a,b,k,q) * wm_interm_58_pt3(b,k,j,i)
term(484) = term(484) + t2(a,p,j,i) * wm_interm_34_pt3(a,b,k,q) * wm_interm_58_pt3(b,k,j,i)
end do 
end do 
end do 
end do 
end do 

term(477) = term(477) * (4.0d+0) 
term(478) = term(478) * (-8.0d+0) 
term(479) = term(479) * (-8.0d+0) 
term(480) = term(480) * (16.0d+0) 
term(481) = term(481) * (8.0d+0) 
term(482) = term(482) * (-4.0d+0) 
term(483) = term(483) * (8.0d+0) 
term(484) = term(484) * (-16.0d+0) 

do a = nocc + 1, nactive 
term(485) = term(485) + wm_interm_29_pt3(a,q) * wm_interm_3_pt3(a,p)
term(486) = term(486) + wm_interm_29_pt3(a,q) * wm_interm_4_pt3(a,p)
term(487) = term(487) + wm_interm_30_pt3(a,q) * wm_interm_3_pt3(a,p)
term(488) = term(488) + wm_interm_30_pt3(a,q) * wm_interm_4_pt3(a,p)
term(489) = term(489) + wm_interm_18_pt3(a,p) * wm_interm_21_pt3(a,q)
term(490) = term(490) + wm_interm_19_pt3(a,p) * wm_interm_21_pt3(a,q)
term(491) = term(491) + wm_interm_18_pt3(a,p) * wm_interm_22_pt3(a,q)
term(492) = term(492) + wm_interm_19_pt3(a,p) * wm_interm_22_pt3(a,q)
term(493) = term(493) + wm_interm_53_pt3(p,a) * wm_interm_59_pt3(a,q)
term(494) = term(494) + wm_interm_21_pt3(a,q) * wm_interm_60_pt3(a,p)
term(495) = term(495) + wm_interm_22_pt3(a,q) * wm_interm_60_pt3(a,p)
term(496) = term(496) + wm_interm_53_pt3(p,a) * wm_interm_61_pt3(a,q)
end do 

term(485) = term(485) * (4.0d+0) 
term(486) = term(486) * (-8.0d+0) 
term(487) = term(487) * (-2.0d+0) 
term(488) = term(488) * (4.0d+0) 
term(489) = term(489) * (4.0d+0) 
term(490) = term(490) * (-8.0d+0) 
term(491) = term(491) * (-2.0d+0) 
term(492) = term(492) * (4.0d+0) 
term(493) = term(493) * (2.0d+0) 
term(494) = term(494) * (-4.0d+0) 
term(495) = term(495) * (2.0d+0) 
term(496) = term(496) * (-4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(497) = term(497) + r1(vrdav_Rl, a,i) * wm_interm_13_pt3(i,j,q,k) * wm_interm_64_pt3(p,a,j,k)
term(498) = term(498) + r1(vrdav_Rl, a,i) * wm_interm_13_pt3(i,j,k,q) * wm_interm_64_pt3(p,a,j,k)
term(499) = term(499) + r1(vrdav_Rl, a,i) * wm_interm_13_pt3(i,j,k,q) * wm_interm_66_pt3(p,a,j,k)
term(500) = term(500) + r1(vrdav_Rl, a,i) * wm_interm_13_pt3(i,j,q,k) * wm_interm_66_pt3(p,a,j,k)
term(501) = term(501) + r1(vrdav_Rl, a,i) * wm_interm_13_pt3(i,j,q,k) * wm_interm_67_pt3(p,a,j,k)
term(502) = term(502) + r1(vrdav_Rl, a,i) * wm_interm_13_pt3(i,j,k,q) * wm_interm_67_pt3(p,a,j,k)
term(503) = term(503) + r1(vrdav_Rl, a,i) * wm_interm_13_pt3(i,j,q,k) * wm_interm_68_pt3(p,a,j,k)
term(504) = term(504) + r1(vrdav_Rl, a,i) * wm_interm_13_pt3(i,j,k,q) * wm_interm_68_pt3(p,a,j,k)
end do 
end do 
end do 
end do 

term(497) = term(497) * (4.0d+0) 
term(498) = term(498) * (-8.0d+0) 
term(499) = term(499) * (4.0d+0) 
term(500) = term(500) * (-8.0d+0) 
term(501) = term(501) * (4.0d+0) 
term(502) = term(502) * (-8.0d+0) 
term(503) = term(503) * (-8.0d+0) 
term(504) = term(504) * (16.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(505) = term(505) + s2(a,p,q,i) * t1(b,i) * wm_interm_18_pt3(b,a)
term(506) = term(506) + s2(a,p,q,i) * t1(b,i) * wm_interm_19_pt3(b,a)
term(507) = term(507) + r1(vrdav_Rl, a,i) * wm_interm_17_pt3(b,p,i,q) * wm_interm_71_pt3(b,a)
term(508) = term(508) + r1(vrdav_Rl, a,i) * wm_interm_16_pt3(b,p,i,q) * wm_interm_71_pt3(b,a)
term(509) = term(509) + r1(vrdav_Rl, a,i) * wm_interm_17_pt3(b,p,i,q) * wm_interm_72_pt3(b,a)
term(510) = term(510) + r1(vrdav_Rl, a,i) * wm_interm_16_pt3(b,p,i,q) * wm_interm_72_pt3(b,a)
term(511) = term(511) + r1(vrdav_Rl, a,i) * wm_interm_14_pt3(b,p,i,q) * wm_interm_71_pt3(b,a)
term(512) = term(512) + r1(vrdav_Rl, a,i) * wm_interm_15_pt3(b,p,i,q) * wm_interm_71_pt3(b,a)
term(513) = term(513) + r1(vrdav_Rl, a,i) * wm_interm_14_pt3(b,p,i,q) * wm_interm_72_pt3(b,a)
term(514) = term(514) + r1(vrdav_Rl, a,i) * wm_interm_15_pt3(b,p,i,q) * wm_interm_72_pt3(b,a)
end do 
end do 
end do 

term(505) = term(505) * (-2.0d+0) 
term(506) = term(506) * (4.0d+0) 
term(507) = term(507) * (-2.0d+0) 
term(508) = term(508) * (4.0d+0) 
term(509) = term(509) * (4.0d+0) 
term(510) = term(510) * (-8.0d+0) 
term(511) = term(511) * (4.0d+0) 
term(512) = term(512) * (-8.0d+0) 
term(513) = term(513) * (-8.0d+0) 
term(514) = term(514) * (16.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(515) = term(515) + s2(a,p,j,i) * wm_interm_14_pt3(a,b,k,q) * wm_interm_63_pt3(b,k,i,j)
term(516) = term(516) + s2(a,p,j,i) * wm_interm_15_pt3(a,b,k,q) * wm_interm_63_pt3(b,k,i,j)
term(517) = term(517) + s2(a,p,j,i) * wm_interm_16_pt3(a,b,k,q) * wm_interm_63_pt3(b,k,i,j)
term(518) = term(518) + s2(a,p,j,i) * wm_interm_17_pt3(a,b,k,q) * wm_interm_63_pt3(b,k,i,j)
term(519) = term(519) + t2(a,p,j,i) * wm_interm_36_pt3(a,b,k,q) * wm_interm_58_pt3(b,k,i,j)
term(520) = term(520) + t2(a,p,j,i) * wm_interm_35_pt3(a,b,k,q) * wm_interm_58_pt3(b,k,i,j)
term(521) = term(521) + t2(a,p,j,i) * wm_interm_33_pt3(a,b,k,q) * wm_interm_58_pt3(b,k,i,j)
term(522) = term(522) + t2(a,p,j,i) * wm_interm_34_pt3(a,b,k,q) * wm_interm_58_pt3(b,k,i,j)
end do 
end do 
end do 
end do 
end do 

term(515) = term(515) * (4.0d+0) 
term(516) = term(516) * (-8.0d+0) 
term(517) = term(517) * (4.0d+0) 
term(518) = term(518) * (-8.0d+0) 
term(519) = term(519) * (-4.0d+0) 
term(520) = term(520) * (8.0d+0) 
term(521) = term(521) * (-4.0d+0) 
term(522) = term(522) * (8.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(523) = term(523) + t2(a,p,j,i) * wm_interm_52_pt3(b,j,i,k) * wm_interm_67_pt3(a,b,k,q)
term(524) = term(524) + t2(a,p,j,i) * wm_interm_48_pt3(b,j,i,k) * wm_interm_67_pt3(a,b,k,q)
term(525) = term(525) + t2(a,p,j,i) * wm_interm_52_pt3(b,j,i,k) * wm_interm_68_pt3(a,b,k,q)
term(526) = term(526) + t2(a,p,j,i) * wm_interm_48_pt3(b,j,i,k) * wm_interm_68_pt3(a,b,k,q)
term(527) = term(527) + t2(a,p,j,i) * wm_interm_52_pt3(b,j,i,k) * wm_interm_64_pt3(a,b,k,q)
term(528) = term(528) + t2(a,p,j,i) * wm_interm_48_pt3(b,j,i,k) * wm_interm_64_pt3(a,b,k,q)
term(529) = term(529) + t2(a,p,j,i) * wm_interm_52_pt3(b,j,i,k) * wm_interm_66_pt3(a,b,k,q)
term(530) = term(530) + t2(a,p,j,i) * wm_interm_48_pt3(b,j,i,k) * wm_interm_66_pt3(a,b,k,q)
end do 
end do 
end do 
end do 
end do 

term(523) = term(523) * (-2.0d+0) 
term(524) = term(524) * (4.0d+0) 
term(525) = term(525) * (4.0d+0) 
term(526) = term(526) * (-8.0d+0) 
term(527) = term(527) * (-2.0d+0) 
term(528) = term(528) * (4.0d+0) 
term(529) = term(529) * (4.0d+0) 
term(530) = term(530) * (-2.0d+0) 

do i = 1, nocc 
term(531) = term(531) + wm_interm_25_pt3(i,q) * wm_interm_31_pt3(p,i)
term(532) = term(532) + wm_interm_28_pt3(i,q) * wm_interm_31_pt3(p,i)
term(533) = term(533) + wm_interm_25_pt3(i,q) * wm_interm_32_pt3(p,i)
term(534) = term(534) + wm_interm_28_pt3(i,q) * wm_interm_32_pt3(p,i)
term(535) = term(535) + wm_interm_44_pt3(q,i) * wm_interm_56_pt3(p,i)
term(536) = term(536) + wm_interm_44_pt3(q,i) * wm_interm_55_pt3(p,i)
end do 

term(531) = term(531) * (-4.0d+0) 
term(532) = term(532) * (8.0d+0) 
term(533) = term(533) * (2.0d+0) 
term(534) = term(534) * (-4.0d+0) 
term(535) = term(535) * (-4.0d+0) 
term(536) = term(536) * (2.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(537) = term(537) + s2(a,b,q,i) * wm_interm_14_pt3(a,p,i,j) * wm_interm_61_pt3(b,j)
term(538) = term(538) + s2(a,b,q,i) * wm_interm_15_pt3(a,p,i,j) * wm_interm_61_pt3(b,j)
term(539) = term(539) + s2(a,b,q,i) * wm_interm_16_pt3(a,p,i,j) * wm_interm_61_pt3(b,j)
term(540) = term(540) + s2(a,b,q,i) * wm_interm_17_pt3(a,p,i,j) * wm_interm_61_pt3(b,j)
term(541) = term(541) + s2(a,p,j,i) * wm_interm_3_pt3(a,b) * wm_interm_63_pt3(b,q,i,j)
term(542) = term(542) + s2(a,p,j,i) * wm_interm_4_pt3(a,b) * wm_interm_63_pt3(b,q,i,j)
term(543) = term(543) + s2(a,b,q,i) * wm_interm_14_pt3(a,p,i,j) * wm_interm_59_pt3(b,j)
term(544) = term(544) + s2(a,b,q,i) * wm_interm_15_pt3(a,p,i,j) * wm_interm_59_pt3(b,j)
term(545) = term(545) + s2(a,b,q,i) * wm_interm_16_pt3(a,p,i,j) * wm_interm_59_pt3(b,j)
term(546) = term(546) + s2(a,b,q,i) * wm_interm_17_pt3(a,p,i,j) * wm_interm_59_pt3(b,j)
term(547) = term(547) + r1(vrdav_Rr, p,i) * wm_interm_34_pt3(a,b,i,j) * wm_interm_66_pt3(b,a,j,q)
term(548) = term(548) + r1(vrdav_Rr, p,i) * wm_interm_34_pt3(a,b,j,q) * wm_interm_67_pt3(b,a,i,j)
term(549) = term(549) + r1(vrdav_Rr, p,i) * wm_interm_34_pt3(a,b,i,j) * wm_interm_64_pt3(b,a,j,q)
term(550) = term(550) + r1(vrdav_Rr, p,i) * wm_interm_34_pt3(a,b,j,q) * wm_interm_68_pt3(b,a,i,j)
term(551) = term(551) + r1(vrdav_Rr, p,i) * wm_interm_36_pt3(a,b,j,q) * wm_interm_67_pt3(b,a,i,j)
term(552) = term(552) + r1(vrdav_Rr, p,i) * wm_interm_36_pt3(a,b,j,q) * wm_interm_68_pt3(b,a,i,j)
term(553) = term(553) + r1(vrdav_Rr, p,i) * wm_interm_34_pt3(a,b,i,j) * wm_interm_67_pt3(b,a,j,q)
term(554) = term(554) + r1(vrdav_Rr, p,i) * wm_interm_34_pt3(a,b,i,j) * wm_interm_68_pt3(b,a,j,q)
term(555) = term(555) + r1(vrdav_Rr, p,i) * wm_interm_33_pt3(a,b,i,j) * wm_interm_66_pt3(b,a,j,q)
term(556) = term(556) + r1(vrdav_Rr, p,i) * wm_interm_33_pt3(a,b,j,q) * wm_interm_67_pt3(b,a,i,j)
term(557) = term(557) + r1(vrdav_Rr, p,i) * wm_interm_33_pt3(a,b,i,j) * wm_interm_64_pt3(b,a,j,q)
term(558) = term(558) + r1(vrdav_Rr, p,i) * wm_interm_33_pt3(a,b,j,q) * wm_interm_68_pt3(b,a,i,j)
term(559) = term(559) + r1(vrdav_Rr, p,i) * wm_interm_35_pt3(a,b,j,q) * wm_interm_67_pt3(b,a,i,j)
term(560) = term(560) + r1(vrdav_Rr, p,i) * wm_interm_35_pt3(a,b,j,q) * wm_interm_68_pt3(b,a,i,j)
term(561) = term(561) + r1(vrdav_Rr, p,i) * wm_interm_33_pt3(a,b,i,j) * wm_interm_67_pt3(b,a,j,q)
term(562) = term(562) + r1(vrdav_Rr, p,i) * wm_interm_33_pt3(a,b,i,j) * wm_interm_68_pt3(b,a,j,q)
term(563) = term(563) + r1(vrdav_Rr, p,i) * wm_interm_35_pt3(a,b,i,j) * wm_interm_64_pt3(b,a,j,q)
term(564) = term(564) + r1(vrdav_Rr, p,i) * wm_interm_33_pt3(a,b,j,q) * wm_interm_64_pt3(b,a,i,j)
term(565) = term(565) + r1(vrdav_Rr, p,i) * wm_interm_35_pt3(a,b,j,q) * wm_interm_64_pt3(b,a,i,j)
term(566) = term(566) + r1(vrdav_Rr, p,i) * wm_interm_35_pt3(a,b,i,j) * wm_interm_68_pt3(b,a,j,q)
term(567) = term(567) + r1(vrdav_Rr, p,i) * wm_interm_33_pt3(a,b,j,q) * wm_interm_66_pt3(b,a,i,j)
term(568) = term(568) + r1(vrdav_Rr, p,i) * wm_interm_35_pt3(a,b,i,j) * wm_interm_66_pt3(b,a,j,q)
term(569) = term(569) + r1(vrdav_Rr, p,i) * wm_interm_35_pt3(a,b,i,j) * wm_interm_67_pt3(b,a,j,q)
term(570) = term(570) + r1(vrdav_Rr, p,i) * wm_interm_35_pt3(a,b,j,q) * wm_interm_66_pt3(b,a,i,j)
term(571) = term(571) + r1(vrdav_Rr, p,i) * wm_interm_36_pt3(a,b,i,j) * wm_interm_66_pt3(b,a,j,q)
term(572) = term(572) + r1(vrdav_Rr, p,i) * wm_interm_34_pt3(a,b,j,q) * wm_interm_66_pt3(b,a,i,j)
term(573) = term(573) + r1(vrdav_Rr, p,i) * wm_interm_36_pt3(a,b,i,j) * wm_interm_64_pt3(b,a,j,q)
term(574) = term(574) + r1(vrdav_Rr, p,i) * wm_interm_34_pt3(a,b,j,q) * wm_interm_64_pt3(b,a,i,j)
term(575) = term(575) + r1(vrdav_Rr, p,i) * wm_interm_36_pt3(a,b,j,q) * wm_interm_66_pt3(b,a,i,j)
term(576) = term(576) + r1(vrdav_Rr, p,i) * wm_interm_36_pt3(a,b,j,q) * wm_interm_64_pt3(b,a,i,j)
term(577) = term(577) + r1(vrdav_Rr, p,i) * wm_interm_36_pt3(a,b,i,j) * wm_interm_67_pt3(b,a,j,q)
term(578) = term(578) + r1(vrdav_Rr, p,i) * wm_interm_36_pt3(a,b,i,j) * wm_interm_68_pt3(b,a,j,q)
term(579) = term(579) + t2(a,p,i,q) * wm_interm_49_pt3(b,j) * wm_interm_67_pt3(a,b,i,j)
term(580) = term(580) + t2(a,p,i,q) * wm_interm_49_pt3(b,j) * wm_interm_68_pt3(a,b,i,j)
term(581) = term(581) + t2(a,p,i,q) * wm_interm_51_pt3(b,j) * wm_interm_67_pt3(a,b,i,j)
term(582) = term(582) + t2(a,p,i,q) * wm_interm_51_pt3(b,j) * wm_interm_68_pt3(a,b,i,j)
term(583) = term(583) + t2(a,p,i,q) * wm_interm_33_pt3(a,b,i,j) * wm_interm_55_pt3(b,j)
term(584) = term(584) + t2(a,p,i,q) * wm_interm_33_pt3(a,b,i,j) * wm_interm_56_pt3(b,j)
term(585) = term(585) + t2(a,p,i,q) * wm_interm_34_pt3(a,b,i,j) * wm_interm_55_pt3(b,j)
term(586) = term(586) + t2(a,p,i,q) * wm_interm_34_pt3(a,b,i,j) * wm_interm_56_pt3(b,j)
term(587) = term(587) + t2(a,p,i,q) * wm_interm_35_pt3(a,b,i,j) * wm_interm_56_pt3(b,j)
term(588) = term(588) + t2(a,p,i,q) * wm_interm_51_pt3(b,j) * wm_interm_64_pt3(a,b,i,j)
term(589) = term(589) + t2(a,p,i,q) * wm_interm_35_pt3(a,b,i,j) * wm_interm_55_pt3(b,j)
term(590) = term(590) + t2(a,p,i,q) * wm_interm_51_pt3(b,j) * wm_interm_66_pt3(a,b,i,j)
term(591) = term(591) + t2(a,p,i,q) * wm_interm_49_pt3(b,j) * wm_interm_66_pt3(a,b,i,j)
term(592) = term(592) + t2(a,p,i,q) * wm_interm_49_pt3(b,j) * wm_interm_64_pt3(a,b,i,j)
term(593) = term(593) + t2(a,p,i,q) * wm_interm_36_pt3(a,b,i,j) * wm_interm_55_pt3(b,j)
term(594) = term(594) + t2(a,p,i,q) * wm_interm_36_pt3(a,b,i,j) * wm_interm_56_pt3(b,j)
term(595) = term(595) + t2(a,p,q,i) * wm_interm_49_pt3(b,j) * wm_interm_67_pt3(a,b,i,j)
term(596) = term(596) + t2(a,p,q,i) * wm_interm_49_pt3(b,j) * wm_interm_68_pt3(a,b,i,j)
term(597) = term(597) + t2(a,p,q,i) * wm_interm_51_pt3(b,j) * wm_interm_67_pt3(a,b,i,j)
term(598) = term(598) + t2(a,p,q,i) * wm_interm_51_pt3(b,j) * wm_interm_68_pt3(a,b,i,j)
term(599) = term(599) + t2(a,p,q,i) * wm_interm_33_pt3(a,b,i,j) * wm_interm_55_pt3(b,j)
term(600) = term(600) + t2(a,p,q,i) * wm_interm_33_pt3(a,b,i,j) * wm_interm_56_pt3(b,j)
term(601) = term(601) + t2(a,p,q,i) * wm_interm_34_pt3(a,b,i,j) * wm_interm_55_pt3(b,j)
term(602) = term(602) + t2(a,p,q,i) * wm_interm_34_pt3(a,b,i,j) * wm_interm_56_pt3(b,j)
term(603) = term(603) + t2(a,p,q,i) * wm_interm_35_pt3(a,b,i,j) * wm_interm_56_pt3(b,j)
term(604) = term(604) + t2(a,p,q,i) * wm_interm_51_pt3(b,j) * wm_interm_64_pt3(a,b,i,j)
term(605) = term(605) + t2(a,p,q,i) * wm_interm_35_pt3(a,b,i,j) * wm_interm_55_pt3(b,j)
term(606) = term(606) + t2(a,p,q,i) * wm_interm_51_pt3(b,j) * wm_interm_66_pt3(a,b,i,j)
term(607) = term(607) + t2(a,p,q,i) * wm_interm_49_pt3(b,j) * wm_interm_66_pt3(a,b,i,j)
term(608) = term(608) + t2(a,p,q,i) * wm_interm_49_pt3(b,j) * wm_interm_64_pt3(a,b,i,j)
term(609) = term(609) + t2(a,p,q,i) * wm_interm_36_pt3(a,b,i,j) * wm_interm_55_pt3(b,j)
term(610) = term(610) + t2(a,p,q,i) * wm_interm_36_pt3(a,b,i,j) * wm_interm_56_pt3(b,j)
term(611) = term(611) + r1(vrdav_Rr, a,i) * wm_interm_36_pt3(a,b,j,q) * wm_interm_67_pt3(b,p,i,j)
term(612) = term(612) + r1(vrdav_Rr, a,i) * wm_interm_35_pt3(a,b,j,q) * wm_interm_67_pt3(b,p,i,j)
term(613) = term(613) + r1(vrdav_Rr, a,i) * wm_interm_33_pt3(a,b,j,q) * wm_interm_67_pt3(b,p,i,j)
term(614) = term(614) + r1(vrdav_Rr, a,i) * wm_interm_34_pt3(a,b,j,q) * wm_interm_67_pt3(b,p,i,j)
term(615) = term(615) + r1(vrdav_Rr, a,i) * wm_interm_35_pt3(a,b,j,q) * wm_interm_66_pt3(b,p,i,j)
term(616) = term(616) + r1(vrdav_Rr, a,i) * wm_interm_35_pt3(a,b,j,q) * wm_interm_64_pt3(b,p,i,j)
term(617) = term(617) + r1(vrdav_Rr, a,i) * wm_interm_36_pt3(a,b,j,q) * wm_interm_64_pt3(b,p,i,j)
term(618) = term(618) + r1(vrdav_Rr, a,i) * wm_interm_36_pt3(a,b,j,q) * wm_interm_66_pt3(b,p,i,j)
term(619) = term(619) + r1(vrdav_Rr, a,i) * wm_interm_36_pt3(a,b,j,q) * wm_interm_68_pt3(b,p,i,j)
term(620) = term(620) + r1(vrdav_Rr, a,i) * wm_interm_35_pt3(a,b,j,q) * wm_interm_68_pt3(b,p,i,j)
term(621) = term(621) + r1(vrdav_Rr, a,i) * wm_interm_33_pt3(a,b,j,q) * wm_interm_64_pt3(b,p,i,j)
term(622) = term(622) + r1(vrdav_Rr, a,i) * wm_interm_33_pt3(a,b,j,q) * wm_interm_66_pt3(b,p,i,j)
term(623) = term(623) + r1(vrdav_Rr, a,i) * wm_interm_34_pt3(a,b,j,q) * wm_interm_64_pt3(b,p,i,j)
term(624) = term(624) + r1(vrdav_Rr, a,i) * wm_interm_34_pt3(a,b,j,q) * wm_interm_66_pt3(b,p,i,j)
term(625) = term(625) + r1(vrdav_Rr, a,i) * wm_interm_33_pt3(a,b,j,q) * wm_interm_68_pt3(b,p,i,j)
term(626) = term(626) + r1(vrdav_Rr, a,i) * wm_interm_34_pt3(a,b,j,q) * wm_interm_68_pt3(b,p,i,j)
term(627) = term(627) + r1(vrdav_Rr, a,i) * wm_interm_36_pt3(b,p,i,j) * wm_interm_67_pt3(a,b,j,q)
term(628) = term(628) + r1(vrdav_Rr, a,i) * wm_interm_35_pt3(b,p,i,j) * wm_interm_67_pt3(a,b,j,q)
term(629) = term(629) + t2(a,b,q,i) * wm_interm_36_pt3(a,p,i,j) * wm_interm_56_pt3(b,j)
term(630) = term(630) + t2(a,b,q,i) * wm_interm_35_pt3(a,p,i,j) * wm_interm_56_pt3(b,j)
term(631) = term(631) + r1(vrdav_Rr, a,i) * wm_interm_35_pt3(b,p,i,j) * wm_interm_66_pt3(a,b,j,q)
term(632) = term(632) + r1(vrdav_Rr, a,i) * wm_interm_36_pt3(b,p,i,j) * wm_interm_66_pt3(a,b,j,q)
term(633) = term(633) + t2(a,b,q,i) * wm_interm_35_pt3(b,p,i,j) * wm_interm_56_pt3(a,j)
term(634) = term(634) + t2(a,b,q,i) * wm_interm_36_pt3(b,p,i,j) * wm_interm_56_pt3(a,j)
term(635) = term(635) + t2(a,b,q,i) * wm_interm_35_pt3(a,p,i,j) * wm_interm_55_pt3(b,j)
term(636) = term(636) + r1(vrdav_Rr, a,i) * wm_interm_36_pt3(b,p,i,j) * wm_interm_68_pt3(a,b,j,q)
term(637) = term(637) + r1(vrdav_Rr, a,i) * wm_interm_35_pt3(b,p,i,j) * wm_interm_68_pt3(a,b,j,q)
term(638) = term(638) + t2(a,b,q,i) * wm_interm_36_pt3(a,p,i,j) * wm_interm_55_pt3(b,j)
term(639) = term(639) + r1(vrdav_Rr, a,i) * wm_interm_36_pt3(b,p,i,j) * wm_interm_64_pt3(a,b,j,q)
term(640) = term(640) + r1(vrdav_Rr, a,i) * wm_interm_35_pt3(b,p,i,j) * wm_interm_64_pt3(a,b,j,q)
term(641) = term(641) + t2(a,b,q,i) * wm_interm_35_pt3(b,p,i,j) * wm_interm_55_pt3(a,j)
term(642) = term(642) + t2(a,b,q,i) * wm_interm_36_pt3(b,p,i,j) * wm_interm_55_pt3(a,j)
term(643) = term(643) + r1(vrdav_Rr, a,i) * wm_interm_33_pt3(b,p,i,j) * wm_interm_67_pt3(a,b,j,q)
term(644) = term(644) + r1(vrdav_Rr, a,i) * wm_interm_34_pt3(b,p,i,j) * wm_interm_67_pt3(a,b,j,q)
term(645) = term(645) + t2(a,b,q,i) * wm_interm_33_pt3(a,p,i,j) * wm_interm_55_pt3(b,j)
term(646) = term(646) + r1(vrdav_Rr, a,i) * wm_interm_33_pt3(b,p,i,j) * wm_interm_68_pt3(a,b,j,q)
term(647) = term(647) + t2(a,b,q,i) * wm_interm_34_pt3(a,p,i,j) * wm_interm_55_pt3(b,j)
term(648) = term(648) + r1(vrdav_Rr, a,i) * wm_interm_34_pt3(b,p,i,j) * wm_interm_68_pt3(a,b,j,q)
term(649) = term(649) + t2(a,b,q,i) * wm_interm_33_pt3(a,p,i,j) * wm_interm_56_pt3(b,j)
term(650) = term(650) + t2(a,b,q,i) * wm_interm_34_pt3(a,p,i,j) * wm_interm_56_pt3(b,j)
term(651) = term(651) + r1(vrdav_Rr, a,i) * wm_interm_33_pt3(b,p,i,j) * wm_interm_64_pt3(a,b,j,q)
term(652) = term(652) + r1(vrdav_Rr, a,i) * wm_interm_34_pt3(b,p,i,j) * wm_interm_64_pt3(a,b,j,q)
term(653) = term(653) + r1(vrdav_Rr, a,i) * wm_interm_33_pt3(b,p,i,j) * wm_interm_66_pt3(a,b,j,q)
term(654) = term(654) + r1(vrdav_Rr, a,i) * wm_interm_34_pt3(b,p,i,j) * wm_interm_66_pt3(a,b,j,q)
term(655) = term(655) + t2(a,b,q,i) * wm_interm_33_pt3(b,p,i,j) * wm_interm_55_pt3(a,j)
term(656) = term(656) + t2(a,b,q,i) * wm_interm_34_pt3(b,p,i,j) * wm_interm_55_pt3(a,j)
term(657) = term(657) + t2(a,b,q,i) * wm_interm_33_pt3(b,p,i,j) * wm_interm_56_pt3(a,j)
term(658) = term(658) + t2(a,b,q,i) * wm_interm_34_pt3(b,p,i,j) * wm_interm_56_pt3(a,j)
end do 
end do 
end do 
end do 

term(537) = term(537) * (-8.0d+0) 
term(538) = term(538) * (16.0d+0) 
term(539) = term(539) * (-8.0d+0) 
term(540) = term(540) * (16.0d+0) 
term(541) = term(541) * (-8.0d+0) 
term(542) = term(542) * (16.0d+0) 
term(543) = term(543) * (4.0d+0) 
term(544) = term(544) * (-8.0d+0) 
term(545) = term(545) * (4.0d+0) 
term(546) = term(546) * (-8.0d+0) 
term(547) = term(547) * (4.0d+0) 
term(548) = term(548) * (-8.0d+0) 
term(549) = term(549) * (-8.0d+0) 
term(550) = term(550) * (16.0d+0) 
term(551) = term(551) * (4.0d+0) 
term(552) = term(552) * (-8.0d+0) 
term(553) = term(553) * (-8.0d+0) 
term(554) = term(554) * (16.0d+0) 
term(555) = term(555) * (-2.0d+0) 
term(556) = term(556) * (4.0d+0) 
term(557) = term(557) * (4.0d+0) 
term(558) = term(558) * (-8.0d+0) 
term(559) = term(559) * (-2.0d+0) 
term(560) = term(560) * (4.0d+0) 
term(561) = term(561) * (4.0d+0) 
term(562) = term(562) * (-8.0d+0) 
term(563) = term(563) * (-2.0d+0) 
term(564) = term(564) * (4.0d+0) 
term(565) = term(565) * (-2.0d+0) 
term(566) = term(566) * (4.0d+0) 
term(567) = term(567) * (-2.0d+0) 
term(568) = term(568) * (4.0d+0) 
term(569) = term(569) * (-2.0d+0) 
term(570) = term(570) * (4.0d+0) 
term(571) = term(571) * (-2.0d+0) 
term(572) = term(572) * (4.0d+0) 
term(573) = term(573) * (4.0d+0) 
term(574) = term(574) * (-8.0d+0) 
term(575) = term(575) * (-2.0d+0) 
term(576) = term(576) * (4.0d+0) 
term(577) = term(577) * (4.0d+0) 
term(578) = term(578) * (-8.0d+0) 
term(579) = term(579) * (16.0d+0) 
term(580) = term(580) * (-32.0d+0) 
term(581) = term(581) * (-8.0d+0) 
term(582) = term(582) * (16.0d+0) 
term(583) = term(583) * (-4.0d+0) 
term(584) = term(584) * (8.0d+0) 
term(585) = term(585) * (8.0d+0) 
term(586) = term(586) * (-16.0d+0) 
term(587) = term(587) * (-4.0d+0) 
term(588) = term(588) * (-8.0d+0) 
term(589) = term(589) * (2.0d+0) 
term(590) = term(590) * (4.0d+0) 
term(591) = term(591) * (-8.0d+0) 
term(592) = term(592) * (16.0d+0) 
term(593) = term(593) * (-4.0d+0) 
term(594) = term(594) * (8.0d+0) 
term(595) = term(595) * (-8.0d+0) 
term(596) = term(596) * (16.0d+0) 
term(597) = term(597) * (4.0d+0) 
term(598) = term(598) * (-8.0d+0) 
term(599) = term(599) * (2.0d+0) 
term(600) = term(600) * (-4.0d+0) 
term(601) = term(601) * (-4.0d+0) 
term(602) = term(602) * (8.0d+0) 
term(603) = term(603) * (2.0d+0) 
term(604) = term(604) * (4.0d+0) 
term(605) = term(605) * (-1.0d+0) 
term(606) = term(606) * (-2.0d+0) 
term(607) = term(607) * (4.0d+0) 
term(608) = term(608) * (-8.0d+0) 
term(609) = term(609) * (2.0d+0) 
term(610) = term(610) * (-4.0d+0) 
term(611) = term(611) * (-4.0d+0) 
term(612) = term(612) * (8.0d+0) 
term(613) = term(613) * (-4.0d+0) 
term(614) = term(614) * (8.0d+0) 
term(615) = term(615) * (-4.0d+0) 
term(616) = term(616) * (8.0d+0) 
term(617) = term(617) * (-4.0d+0) 
term(618) = term(618) * (8.0d+0) 
term(619) = term(619) * (8.0d+0) 
term(620) = term(620) * (-16.0d+0) 
term(621) = term(621) * (-4.0d+0) 
term(622) = term(622) * (8.0d+0) 
term(623) = term(623) * (8.0d+0) 
term(624) = term(624) * (-16.0d+0) 
term(625) = term(625) * (8.0d+0) 
term(626) = term(626) * (-16.0d+0) 
term(627) = term(627) * (-4.0d+0) 
term(628) = term(628) * (8.0d+0) 
term(629) = term(629) * (4.0d+0) 
term(630) = term(630) * (-8.0d+0) 
term(631) = term(631) * (-4.0d+0) 
term(632) = term(632) * (8.0d+0) 
term(633) = term(633) * (4.0d+0) 
term(634) = term(634) * (-8.0d+0) 
term(635) = term(635) * (4.0d+0) 
term(636) = term(636) * (8.0d+0) 
term(637) = term(637) * (-16.0d+0) 
term(638) = term(638) * (-2.0d+0) 
term(639) = term(639) * (-4.0d+0) 
term(640) = term(640) * (8.0d+0) 
term(641) = term(641) * (-2.0d+0) 
term(642) = term(642) * (4.0d+0) 
term(643) = term(643) * (-4.0d+0) 
term(644) = term(644) * (8.0d+0) 
term(645) = term(645) * (-2.0d+0) 
term(646) = term(646) * (8.0d+0) 
term(647) = term(647) * (4.0d+0) 
term(648) = term(648) * (-16.0d+0) 
term(649) = term(649) * (4.0d+0) 
term(650) = term(650) * (-8.0d+0) 
term(651) = term(651) * (-4.0d+0) 
term(652) = term(652) * (8.0d+0) 
term(653) = term(653) * (8.0d+0) 
term(654) = term(654) * (-16.0d+0) 
term(655) = term(655) * (4.0d+0) 
term(656) = term(656) * (-8.0d+0) 
term(657) = term(657) * (-8.0d+0) 
term(658) = term(658) * (16.0d+0) 

do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(659) = term(659) + r1(vrdav_Rl, b,q) * wm_interm_3_pt3(a,p) * wm_interm_71_pt3(a,b)
term(660) = term(660) + r1(vrdav_Rl, b,q) * wm_interm_4_pt3(a,p) * wm_interm_71_pt3(a,b)
term(661) = term(661) + r1(vrdav_Rl, b,q) * wm_interm_3_pt3(a,p) * wm_interm_72_pt3(a,b)
term(662) = term(662) + r1(vrdav_Rl, b,q) * wm_interm_4_pt3(a,p) * wm_interm_72_pt3(a,b)
end do 
end do 

term(659) = term(659) * (-2.0d+0) 
term(660) = term(660) * (4.0d+0) 
term(661) = term(661) * (4.0d+0) 
term(662) = term(662) * (-8.0d+0) 

do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(663) = term(663) + s2(a,b,i,q) * wm_interm_14_pt3(a,p,i,j) * wm_interm_61_pt3(b,j)
term(664) = term(664) + s2(a,b,i,q) * wm_interm_15_pt3(a,p,i,j) * wm_interm_61_pt3(b,j)
term(665) = term(665) + s2(a,b,i,q) * wm_interm_17_pt3(a,p,i,j) * wm_interm_61_pt3(b,j)
term(666) = term(666) + s2(a,b,i,q) * wm_interm_16_pt3(a,p,i,j) * wm_interm_61_pt3(b,j)
term(667) = term(667) + s2(a,b,i,q) * wm_interm_14_pt3(a,p,i,j) * wm_interm_59_pt3(b,j)
term(668) = term(668) + s2(a,b,i,q) * wm_interm_15_pt3(a,p,i,j) * wm_interm_59_pt3(b,j)
term(669) = term(669) + s2(a,b,i,q) * wm_interm_17_pt3(a,p,i,j) * wm_interm_59_pt3(b,j)
term(670) = term(670) + s2(a,b,i,q) * wm_interm_16_pt3(a,p,i,j) * wm_interm_59_pt3(b,j)
term(671) = term(671) + s2(a,b,i,j) * wm_interm_17_pt3(a,p,j,q) * wm_interm_61_pt3(b,i)
term(672) = term(672) + s2(a,b,i,j) * wm_interm_16_pt3(a,p,j,q) * wm_interm_61_pt3(b,i)
term(673) = term(673) + s2(a,b,i,j) * wm_interm_17_pt3(a,p,i,q) * wm_interm_61_pt3(b,j)
term(674) = term(674) + s2(a,b,i,j) * wm_interm_16_pt3(a,p,i,q) * wm_interm_61_pt3(b,j)
term(675) = term(675) + s2(a,b,i,j) * wm_interm_14_pt3(a,p,j,q) * wm_interm_61_pt3(b,i)
term(676) = term(676) + s2(a,b,i,j) * wm_interm_15_pt3(a,p,j,q) * wm_interm_61_pt3(b,i)
term(677) = term(677) + s2(a,b,i,j) * wm_interm_14_pt3(a,p,i,q) * wm_interm_61_pt3(b,j)
term(678) = term(678) + s2(a,b,i,j) * wm_interm_15_pt3(a,p,i,q) * wm_interm_61_pt3(b,j)
term(679) = term(679) + s2(a,b,i,j) * wm_interm_17_pt3(a,p,j,q) * wm_interm_59_pt3(b,i)
term(680) = term(680) + s2(a,b,i,j) * wm_interm_16_pt3(a,p,j,q) * wm_interm_59_pt3(b,i)
term(681) = term(681) + s2(a,b,i,j) * wm_interm_17_pt3(a,p,i,q) * wm_interm_59_pt3(b,j)
term(682) = term(682) + s2(a,b,i,j) * wm_interm_16_pt3(a,p,i,q) * wm_interm_59_pt3(b,j)
term(683) = term(683) + s2(a,b,i,j) * wm_interm_14_pt3(a,p,j,q) * wm_interm_59_pt3(b,i)
term(684) = term(684) + s2(a,b,i,j) * wm_interm_15_pt3(a,p,j,q) * wm_interm_59_pt3(b,i)
term(685) = term(685) + s2(a,b,i,j) * wm_interm_14_pt3(a,p,i,q) * wm_interm_59_pt3(b,j)
term(686) = term(686) + s2(a,b,i,j) * wm_interm_15_pt3(a,p,i,q) * wm_interm_59_pt3(b,j)
term(687) = term(687) + t2(a,b,i,j) * wm_interm_34_pt3(b,p,j,q) * wm_interm_55_pt3(a,i)
term(688) = term(688) + t2(a,b,i,j) * wm_interm_34_pt3(b,p,j,q) * wm_interm_56_pt3(a,i)
term(689) = term(689) + t2(a,b,i,j) * wm_interm_33_pt3(b,p,j,q) * wm_interm_55_pt3(a,i)
term(690) = term(690) + t2(a,b,i,j) * wm_interm_33_pt3(b,p,j,q) * wm_interm_56_pt3(a,i)
term(691) = term(691) + t2(a,b,i,j) * wm_interm_34_pt3(b,p,i,q) * wm_interm_55_pt3(a,j)
term(692) = term(692) + t2(a,b,i,j) * wm_interm_34_pt3(b,p,i,q) * wm_interm_56_pt3(a,j)
term(693) = term(693) + t2(a,b,i,j) * wm_interm_33_pt3(b,p,i,q) * wm_interm_56_pt3(a,j)
term(694) = term(694) + t2(a,b,i,j) * wm_interm_33_pt3(b,p,i,q) * wm_interm_55_pt3(a,j)
term(695) = term(695) + t2(a,b,i,j) * wm_interm_36_pt3(b,p,j,q) * wm_interm_55_pt3(a,i)
term(696) = term(696) + t2(a,b,i,j) * wm_interm_36_pt3(b,p,j,q) * wm_interm_56_pt3(a,i)
term(697) = term(697) + t2(a,b,i,j) * wm_interm_35_pt3(b,p,j,q) * wm_interm_55_pt3(a,i)
term(698) = term(698) + t2(a,b,i,j) * wm_interm_35_pt3(b,p,j,q) * wm_interm_56_pt3(a,i)
term(699) = term(699) + t2(a,b,i,j) * wm_interm_36_pt3(b,p,i,q) * wm_interm_55_pt3(a,j)
term(700) = term(700) + t2(a,b,i,j) * wm_interm_36_pt3(b,p,i,q) * wm_interm_56_pt3(a,j)
term(701) = term(701) + t2(a,b,i,j) * wm_interm_35_pt3(b,p,i,q) * wm_interm_56_pt3(a,j)
term(702) = term(702) + t2(a,b,i,j) * wm_interm_35_pt3(b,p,i,q) * wm_interm_55_pt3(a,j)
end do 
end do 
end do 
end do 

term(663) = term(663) * (16.0d+0) 
term(664) = term(664) * (-32.0d+0) 
term(665) = term(665) * (-8.0d+0) 
term(666) = term(666) * (16.0d+0) 
term(667) = term(667) * (-8.0d+0) 
term(668) = term(668) * (16.0d+0) 
term(669) = term(669) * (4.0d+0) 
term(670) = term(670) * (-8.0d+0) 
term(671) = term(671) * (4.0d+0) 
term(672) = term(672) * (-8.0d+0) 
term(673) = term(673) * (-8.0d+0) 
term(674) = term(674) * (16.0d+0) 
term(675) = term(675) * (-8.0d+0) 
term(676) = term(676) * (16.0d+0) 
term(677) = term(677) * (16.0d+0) 
term(678) = term(678) * (-32.0d+0) 
term(679) = term(679) * (-2.0d+0) 
term(680) = term(680) * (4.0d+0) 
term(681) = term(681) * (4.0d+0) 
term(682) = term(682) * (-8.0d+0) 
term(683) = term(683) * (4.0d+0) 
term(684) = term(684) * (-8.0d+0) 
term(685) = term(685) * (-8.0d+0) 
term(686) = term(686) * (16.0d+0) 
term(687) = term(687) * (8.0d+0) 
term(688) = term(688) * (-16.0d+0) 
term(689) = term(689) * (-4.0d+0) 
term(690) = term(690) * (8.0d+0) 
term(691) = term(691) * (-4.0d+0) 
term(692) = term(692) * (8.0d+0) 
term(693) = term(693) * (-4.0d+0) 
term(694) = term(694) * (2.0d+0) 
term(695) = term(695) * (-4.0d+0) 
term(696) = term(696) * (8.0d+0) 
term(697) = term(697) * (2.0d+0) 
term(698) = term(698) * (-4.0d+0) 
term(699) = term(699) * (2.0d+0) 
term(700) = term(700) * (-4.0d+0) 
term(701) = term(701) * (2.0d+0) 
term(702) = term(702) * (-1.0d+0) 

do a = nocc + 1, nactive 
do i = 1, nocc 
term(703) = term(703) + r1(vrdav_Rl, a,i) * wm_interm_11_pt3(i,q) * wm_interm_71_pt3(p,a)
term(704) = term(704) + r1(vrdav_Rl, a,i) * wm_interm_11_pt3(i,q) * wm_interm_72_pt3(p,a)
term(705) = term(705) + r1(vrdav_Rl, a,i) * wm_interm_12_pt3(i,q) * wm_interm_71_pt3(p,a)
term(706) = term(706) + r1(vrdav_Rl, a,i) * wm_interm_12_pt3(i,q) * wm_interm_72_pt3(p,a)
end do 
end do 

term(703) = term(703) * (-2.0d+0) 
term(704) = term(704) * (4.0d+0) 
term(705) = term(705) * (4.0d+0) 
term(706) = term(706) * (-8.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
term(707) = term(707) + wm_interm_1_pt3(p,i,q,j) * wm_interm_8_pt3(j,i)
term(708) = term(708) + wm_interm_1_pt3(p,i,q,j) * wm_interm_9_pt3(j,i)
term(709) = term(709) + wm_interm_1_pt3(p,q,i,j) * wm_interm_8_pt3(j,i)
term(710) = term(710) + wm_interm_1_pt3(p,q,i,j) * wm_interm_9_pt3(j,i)
term(711) = term(711) + wm_interm_10_pt3(p,i,q,j) * wm_interm_11_pt3(i,j)
term(712) = term(712) + wm_interm_10_pt3(p,i,q,j) * wm_interm_12_pt3(i,j)
term(713) = term(713) + wm_interm_10_pt3(p,q,i,j) * wm_interm_11_pt3(i,j)
term(714) = term(714) + wm_interm_10_pt3(p,q,i,j) * wm_interm_12_pt3(i,j)
term(715) = term(715) + wm_interm_11_pt3(i,j) * wm_interm_20_pt3(p,q,i,j)
term(716) = term(716) + wm_interm_12_pt3(i,j) * wm_interm_20_pt3(p,q,i,j)
term(717) = term(717) + wm_interm_11_pt3(i,j) * wm_interm_20_pt3(p,i,q,j)
term(718) = term(718) + wm_interm_12_pt3(i,j) * wm_interm_20_pt3(p,i,q,j)
term(719) = term(719) + wm_interm_25_pt3(i,j) * wm_interm_26_pt3(p,i,q,j)
term(720) = term(720) + wm_interm_25_pt3(i,j) * wm_interm_27_pt3(p,i,q,j)
term(721) = term(721) + wm_interm_26_pt3(p,i,q,j) * wm_interm_28_pt3(i,j)
term(722) = term(722) + wm_interm_27_pt3(p,i,q,j) * wm_interm_28_pt3(i,j)
term(723) = term(723) + wm_interm_39_pt3(p,i,j,q) * wm_interm_8_pt3(j,i)
term(724) = term(724) + wm_interm_39_pt3(p,i,j,q) * wm_interm_9_pt3(j,i)
term(725) = term(725) + wm_interm_39_pt3(p,i,q,j) * wm_interm_8_pt3(j,i)
term(726) = term(726) + wm_interm_39_pt3(p,i,q,j) * wm_interm_9_pt3(j,i)
term(727) = term(727) + wm_interm_1_pt3(p,i,q,j) * wm_interm_57_pt3(j,i)
term(728) = term(728) + wm_interm_1_pt3(p,q,i,j) * wm_interm_57_pt3(j,i)
term(729) = term(729) + wm_interm_44_pt3(i,j) * wm_interm_58_pt3(p,i,q,j)
term(730) = term(730) + wm_interm_44_pt3(i,j) * wm_interm_58_pt3(p,i,j,q)
term(731) = term(731) + wm_interm_39_pt3(p,i,j,q) * wm_interm_57_pt3(j,i)
term(732) = term(732) + wm_interm_54_pt3(i,j) * wm_interm_63_pt3(p,i,j,q)
term(733) = term(733) + wm_interm_39_pt3(p,i,q,j) * wm_interm_57_pt3(j,i)
term(734) = term(734) + wm_interm_54_pt3(i,j) * wm_interm_63_pt3(p,i,q,j)
term(735) = term(735) + r1(vrdav_Rr, p,i) * wm_interm_28_pt3(j,q) * wm_interm_69_pt3(i,j)
term(736) = term(736) + r1(vrdav_Rr, p,i) * wm_interm_28_pt3(j,q) * wm_interm_70_pt3(i,j)
term(737) = term(737) + r1(vrdav_Rr, p,i) * wm_interm_25_pt3(j,q) * wm_interm_69_pt3(i,j)
term(738) = term(738) + r1(vrdav_Rr, p,i) * wm_interm_25_pt3(j,q) * wm_interm_70_pt3(i,j)
term(739) = term(739) + r1(vrdav_Rr, p,i) * wm_interm_25_pt3(i,j) * wm_interm_69_pt3(j,q)
term(740) = term(740) + r1(vrdav_Rr, p,i) * wm_interm_25_pt3(i,j) * wm_interm_70_pt3(j,q)
term(741) = term(741) + r1(vrdav_Rr, p,i) * wm_interm_28_pt3(i,j) * wm_interm_69_pt3(j,q)
term(742) = term(742) + r1(vrdav_Rr, p,i) * wm_interm_28_pt3(i,j) * wm_interm_70_pt3(j,q)
end do 
end do 

term(707) = term(707) * (-2.0d+0) 
term(708) = term(708) * (4.0d+0) 
term(709) = term(709) * (4.0d+0) 
term(710) = term(710) * (-8.0d+0) 
term(711) = term(711) * (-1.0d+0) 
term(712) = term(712) * (2.0d+0) 
term(713) = term(713) * (2.0d+0) 
term(714) = term(714) * (-4.0d+0) 
term(715) = term(715) * (-1.0d+0) 
term(716) = term(716) * (2.0d+0) 
term(717) = term(717) * (2.0d+0) 
term(718) = term(718) * (-4.0d+0) 
term(719) = term(719) * (-2.0d+0) 
term(720) = term(720) * (4.0d+0) 
term(721) = term(721) * (4.0d+0) 
term(722) = term(722) * (-8.0d+0) 
term(723) = term(723) * (4.0d+0) 
term(724) = term(724) * (-8.0d+0) 
term(725) = term(725) * (-2.0d+0) 
term(726) = term(726) * (4.0d+0) 
term(727) = term(727) * (2.0d+0) 
term(728) = term(728) * (-4.0d+0) 
term(729) = term(729) * (2.0d+0) 
term(730) = term(730) * (-4.0d+0) 
term(731) = term(731) * (-4.0d+0) 
term(732) = term(732) * (-4.0d+0) 
term(733) = term(733) * (2.0d+0) 
term(734) = term(734) * (2.0d+0) 
term(735) = term(735) * (4.0d+0) 
term(736) = term(736) * (-8.0d+0) 
term(737) = term(737) * (-2.0d+0) 
term(738) = term(738) * (4.0d+0) 
term(739) = term(739) * (-2.0d+0) 
term(740) = term(740) * (4.0d+0) 
term(741) = term(741) * (4.0d+0) 
term(742) = term(742) * (-8.0d+0) 

do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(743) = term(743) + r2(vrdav_Rl, a,i,p,q) * t1(b,i) * wm_interm_3_pt3(a,b)
term(744) = term(744) + r2(vrdav_Rl, a,i,p,q) * t1(b,i) * wm_interm_4_pt3(a,b)
term(745) = term(745) + r2(vrdav_Rr, a,i,p,q) * s1(b,i) * wm_interm_23_pt3(a,b)
term(746) = term(746) + r2(vrdav_Rr, a,i,p,q) * s1(b,i) * wm_interm_24_pt3(a,b)
term(747) = term(747) + s2(a,p,i,q) * wm_interm_3_pt3(a,b) * wm_interm_61_pt3(b,i)
term(748) = term(748) + s2(a,p,i,q) * wm_interm_4_pt3(a,b) * wm_interm_61_pt3(b,i)
term(749) = term(749) + s2(a,p,i,q) * wm_interm_3_pt3(a,b) * wm_interm_59_pt3(b,i)
term(750) = term(750) + s2(a,p,i,q) * wm_interm_4_pt3(a,b) * wm_interm_59_pt3(b,i)
term(751) = term(751) + s2(a,b,i,q) * wm_interm_3_pt3(a,p) * wm_interm_61_pt3(b,i)
term(752) = term(752) + s2(a,b,i,q) * wm_interm_4_pt3(a,p) * wm_interm_61_pt3(b,i)
term(753) = term(753) + s2(a,b,i,q) * wm_interm_3_pt3(a,p) * wm_interm_59_pt3(b,i)
term(754) = term(754) + s2(a,b,i,q) * wm_interm_4_pt3(a,p) * wm_interm_59_pt3(b,i)
term(755) = term(755) + t2(a,p,i,q) * wm_interm_51_pt3(b,i) * wm_interm_71_pt3(a,b)
term(756) = term(756) + t2(a,p,i,q) * wm_interm_51_pt3(b,i) * wm_interm_72_pt3(a,b)
term(757) = term(757) + t2(a,p,i,q) * wm_interm_49_pt3(b,i) * wm_interm_71_pt3(a,b)
term(758) = term(758) + t2(a,p,i,q) * wm_interm_49_pt3(b,i) * wm_interm_72_pt3(a,b)
term(759) = term(759) + t2(a,p,i,q) * wm_interm_23_pt3(a,b) * wm_interm_55_pt3(b,i)
term(760) = term(760) + t2(a,p,i,q) * wm_interm_23_pt3(a,b) * wm_interm_56_pt3(b,i)
term(761) = term(761) + t2(a,p,i,q) * wm_interm_24_pt3(a,b) * wm_interm_55_pt3(b,i)
term(762) = term(762) + t2(a,p,i,q) * wm_interm_24_pt3(a,b) * wm_interm_56_pt3(b,i)
term(763) = term(763) + t2(a,b,i,q) * wm_interm_49_pt3(b,i) * wm_interm_71_pt3(a,p)
term(764) = term(764) + t2(a,b,i,q) * wm_interm_51_pt3(b,i) * wm_interm_71_pt3(a,p)
term(765) = term(765) + t2(a,b,i,q) * wm_interm_49_pt3(b,i) * wm_interm_72_pt3(a,p)
term(766) = term(766) + t2(a,b,i,q) * wm_interm_51_pt3(b,i) * wm_interm_72_pt3(a,p)
term(767) = term(767) + t2(a,b,i,q) * wm_interm_24_pt3(a,p) * wm_interm_56_pt3(b,i)
term(768) = term(768) + t2(a,b,i,q) * wm_interm_23_pt3(a,p) * wm_interm_56_pt3(b,i)
term(769) = term(769) + t2(a,b,i,q) * wm_interm_24_pt3(a,p) * wm_interm_55_pt3(b,i)
term(770) = term(770) + t2(a,b,i,q) * wm_interm_23_pt3(a,p) * wm_interm_55_pt3(b,i)
end do 
end do 
end do 

term(743) = term(743) * (4.0d+0) 
term(744) = term(744) * (-8.0d+0) 
term(745) = term(745) * (-8.0d+0) 
term(746) = term(746) * (4.0d+0) 
term(747) = term(747) * (16.0d+0) 
term(748) = term(748) * (-32.0d+0) 
term(749) = term(749) * (-8.0d+0) 
term(750) = term(750) * (16.0d+0) 
term(751) = term(751) * (4.0d+0) 
term(752) = term(752) * (-8.0d+0) 
term(753) = term(753) * (-2.0d+0) 
term(754) = term(754) * (4.0d+0) 
term(755) = term(755) * (4.0d+0) 
term(756) = term(756) * (-8.0d+0) 
term(757) = term(757) * (-8.0d+0) 
term(758) = term(758) * (16.0d+0) 
term(759) = term(759) * (-8.0d+0) 
term(760) = term(760) * (16.0d+0) 
term(761) = term(761) * (4.0d+0) 
term(762) = term(762) * (-8.0d+0) 
term(763) = term(763) * (8.0d+0) 
term(764) = term(764) * (-4.0d+0) 
term(765) = term(765) * (-16.0d+0) 
term(766) = term(766) * (8.0d+0) 
term(767) = term(767) * (8.0d+0) 
term(768) = term(768) * (-16.0d+0) 
term(769) = term(769) * (-4.0d+0) 
term(770) = term(770) * (8.0d+0) 

do l = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(771) = term(771) + s2(a,p,q,i) * wm_interm_43_pt3(a,j,k,l) * wm_interm_65_pt3(l,k,j,i)
term(772) = term(772) + s2(a,p,q,i) * wm_interm_43_pt3(a,j,k,l) * wm_interm_65_pt3(k,l,j,i)
term(773) = term(773) + s2(a,p,i,q) * wm_interm_43_pt3(a,j,k,l) * wm_interm_65_pt3(l,k,j,i)
term(774) = term(774) + s2(a,p,i,q) * wm_interm_43_pt3(a,j,k,l) * wm_interm_65_pt3(k,l,j,i)
term(775) = term(775) + s2(a,p,q,i) * wm_interm_13_pt3(j,i,k,l) * wm_interm_63_pt3(a,j,k,l)
term(776) = term(776) + s2(a,p,j,i) * wm_interm_13_pt3(j,i,k,l) * wm_interm_63_pt3(a,q,k,l)
term(777) = term(777) + s2(a,p,j,i) * wm_interm_13_pt3(i,j,k,l) * wm_interm_63_pt3(a,q,k,l)
term(778) = term(778) + t2(a,p,q,i) * wm_interm_38_pt3(i,j,k,l) * wm_interm_58_pt3(a,j,k,l)
term(779) = term(779) + t2(a,p,q,i) * wm_interm_38_pt3(j,i,k,l) * wm_interm_58_pt3(a,j,k,l)
end do 
end do 
end do 
end do 
end do 

term(771) = term(771) * (4.0d+0) 
term(772) = term(772) * (-8.0d+0) 
term(773) = term(773) * (-8.0d+0) 
term(774) = term(774) * (16.0d+0) 
term(775) = term(775) * (-8.0d+0) 
term(776) = term(776) * (2.0d+0) 
term(777) = term(777) * (-4.0d+0) 
term(778) = term(778) * (-1.0d+0) 
term(779) = term(779) * (2.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(780) = term(780) + s2(a,p,j,i) * wm_interm_13_pt3(i,j,k,q) * wm_interm_61_pt3(a,k)
term(781) = term(781) + s2(a,p,j,i) * wm_interm_13_pt3(j,i,k,q) * wm_interm_61_pt3(a,k)
term(782) = term(782) + s2(a,p,j,i) * wm_interm_13_pt3(i,j,k,q) * wm_interm_59_pt3(a,k)
term(783) = term(783) + s2(a,p,j,i) * wm_interm_13_pt3(j,i,k,q) * wm_interm_59_pt3(a,k)
term(784) = term(784) + t2(a,p,j,i) * wm_interm_38_pt3(j,i,k,q) * wm_interm_55_pt3(a,k)
term(785) = term(785) + t2(a,p,j,i) * wm_interm_38_pt3(i,j,k,q) * wm_interm_55_pt3(a,k)
term(786) = term(786) + t2(a,p,j,i) * wm_interm_38_pt3(j,i,k,q) * wm_interm_56_pt3(a,k)
term(787) = term(787) + t2(a,p,j,i) * wm_interm_38_pt3(i,j,k,q) * wm_interm_56_pt3(a,k)
term(788) = term(788) + t2(a,p,j,i) * wm_interm_51_pt3(a,k) * wm_interm_65_pt3(j,i,k,q)
term(789) = term(789) + t2(a,p,j,i) * wm_interm_49_pt3(a,k) * wm_interm_65_pt3(j,i,k,q)
term(790) = term(790) + t2(a,p,j,i) * wm_interm_49_pt3(a,k) * wm_interm_65_pt3(i,j,k,q)
term(791) = term(791) + t2(a,p,j,i) * wm_interm_51_pt3(a,k) * wm_interm_65_pt3(i,j,k,q)
end do 
end do 
end do 
end do 

term(780) = term(780) * (-4.0d+0) 
term(781) = term(781) * (8.0d+0) 
term(782) = term(782) * (2.0d+0) 
term(783) = term(783) * (-4.0d+0) 
term(784) = term(784) * (4.0d+0) 
term(785) = term(785) * (-2.0d+0) 
term(786) = term(786) * (-8.0d+0) 
term(787) = term(787) * (4.0d+0) 
term(788) = term(788) * (8.0d+0) 
term(789) = term(789) * (-16.0d+0) 
term(790) = term(790) * (8.0d+0) 
term(791) = term(791) * (-4.0d+0) 

do j = 1, nocc 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(792) = term(792) + r1(vrdav_Rl, a,i) * wm_interm_11_pt3(i,j) * wm_interm_64_pt3(p,a,q,j)
term(793) = term(793) + r1(vrdav_Rl, a,i) * wm_interm_12_pt3(i,j) * wm_interm_64_pt3(p,a,q,j)
term(794) = term(794) + r1(vrdav_Rl, a,i) * wm_interm_11_pt3(i,j) * wm_interm_66_pt3(p,a,q,j)
term(795) = term(795) + r1(vrdav_Rl, a,i) * wm_interm_12_pt3(i,j) * wm_interm_66_pt3(p,a,q,j)
term(796) = term(796) + r1(vrdav_Rl, a,i) * wm_interm_11_pt3(i,j) * wm_interm_67_pt3(p,a,q,j)
term(797) = term(797) + r1(vrdav_Rl, a,i) * wm_interm_12_pt3(i,j) * wm_interm_67_pt3(p,a,q,j)
term(798) = term(798) + r1(vrdav_Rl, a,i) * wm_interm_11_pt3(i,j) * wm_interm_68_pt3(p,a,q,j)
term(799) = term(799) + r1(vrdav_Rl, a,i) * wm_interm_12_pt3(i,j) * wm_interm_68_pt3(p,a,q,j)
end do 
end do 
end do 

term(792) = term(792) * (-8.0d+0) 
term(793) = term(793) * (16.0d+0) 
term(794) = term(794) * (4.0d+0) 
term(795) = term(795) * (-8.0d+0) 
term(796) = term(796) * (-8.0d+0) 
term(797) = term(797) * (16.0d+0) 
term(798) = term(798) * (16.0d+0) 
term(799) = term(799) * (-32.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(800) = term(800) + t2(a,p,i,q) * wm_interm_48_pt3(b,i,j,k) * wm_interm_67_pt3(a,b,k,j)
term(801) = term(801) + t2(a,p,i,q) * wm_interm_48_pt3(b,i,j,k) * wm_interm_68_pt3(a,b,k,j)
term(802) = term(802) + t2(a,p,i,q) * wm_interm_52_pt3(b,i,j,k) * wm_interm_67_pt3(a,b,k,j)
term(803) = term(803) + t2(a,p,i,q) * wm_interm_52_pt3(b,i,j,k) * wm_interm_68_pt3(a,b,k,j)
term(804) = term(804) + t2(a,p,i,q) * wm_interm_52_pt3(b,i,j,k) * wm_interm_64_pt3(a,b,k,j)
term(805) = term(805) + t2(a,p,i,q) * wm_interm_52_pt3(b,i,j,k) * wm_interm_66_pt3(a,b,k,j)
term(806) = term(806) + t2(a,p,i,q) * wm_interm_48_pt3(b,i,j,k) * wm_interm_66_pt3(a,b,k,j)
term(807) = term(807) + t2(a,p,i,q) * wm_interm_48_pt3(b,i,j,k) * wm_interm_64_pt3(a,b,k,j)
end do 
end do 
end do 
end do 
end do 

term(800) = term(800) * (-4.0d+0) 
term(801) = term(801) * (8.0d+0) 
term(802) = term(802) * (2.0d+0) 
term(803) = term(803) * (-4.0d+0) 
term(804) = term(804) * (2.0d+0) 
term(805) = term(805) * (-4.0d+0) 
term(806) = term(806) * (2.0d+0) 
term(807) = term(807) * (-4.0d+0) 


    calc_D_vo_wm_pt3 = zero
    do s = 0, 807
    calc_D_vo_wm_pt3 = calc_D_vo_wm_pt3 + term(s)
    end do

    end function calc_D_vo_wm_pt3
    
    function calc_D_vv_wm_pt3(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vv_wm_pt3
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , k, j, i, a, b 
    real(F64), dimension(0:41) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(0) = term(0) + wm_interm_1_pt3(q,i,j,k) * wm_interm_43_pt3(p,k,i,j)
end do 
end do 
end do 

term(0) = term(0) * (2.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(1) = term(1) + wm_interm_1_pt3(q,i,j,k) * wm_interm_43_pt3(p,k,j,i)
term(2) = term(2) + wm_interm_39_pt3(q,i,j,k) * wm_interm_52_pt3(p,j,k,i)
term(3) = term(3) + wm_interm_39_pt3(q,i,j,k) * wm_interm_48_pt3(p,j,k,i)
term(4) = term(4) + wm_interm_39_pt3(q,i,j,k) * wm_interm_52_pt3(p,k,j,i)
term(5) = term(5) + wm_interm_39_pt3(q,i,j,k) * wm_interm_48_pt3(p,k,j,i)
end do 
end do 
end do 

term(1) = term(1) * (-4.0d+0) 
term(3) = term(3) * (-2.0d+0) 
term(4) = term(4) * (-2.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(6) = term(6) + wm_interm_17_pt3(a,p,i,j) * wm_interm_47_pt3(a,q,i,j)
term(7) = term(7) + wm_interm_16_pt3(a,p,i,j) * wm_interm_47_pt3(a,q,i,j)
term(8) = term(8) + wm_interm_14_pt3(a,p,i,j) * wm_interm_47_pt3(a,q,i,j)
term(9) = term(9) + wm_interm_15_pt3(a,p,i,j) * wm_interm_47_pt3(a,q,i,j)
term(10) = term(10) + wm_interm_34_pt3(q,a,i,j) * wm_interm_50_pt3(p,a,i,j)
term(11) = term(11) + wm_interm_33_pt3(q,a,i,j) * wm_interm_50_pt3(p,a,i,j)
term(12) = term(12) + wm_interm_35_pt3(q,a,i,j) * wm_interm_50_pt3(p,a,i,j)
term(13) = term(13) + wm_interm_36_pt3(q,a,i,j) * wm_interm_50_pt3(p,a,i,j)
term(14) = term(14) + wm_interm_35_pt3(a,q,i,j) * wm_interm_50_pt3(a,p,i,j)
term(15) = term(15) + wm_interm_36_pt3(a,q,i,j) * wm_interm_50_pt3(a,p,i,j)
term(16) = term(16) + wm_interm_33_pt3(a,q,i,j) * wm_interm_50_pt3(a,p,i,j)
term(17) = term(17) + wm_interm_34_pt3(a,q,i,j) * wm_interm_50_pt3(a,p,i,j)
end do 
end do 
end do 

term(6) = term(6) * (2.0d+0) 
term(7) = term(7) * (-4.0d+0) 
term(8) = term(8) * (-4.0d+0) 
term(9) = term(9) * (8.0d+0) 
term(10) = term(10) * (8.0d+0) 
term(11) = term(11) * (-4.0d+0) 
term(12) = term(12) * (2.0d+0) 
term(13) = term(13) * (-4.0d+0) 
term(14) = term(14) * (2.0d+0) 
term(15) = term(15) * (-4.0d+0) 
term(16) = term(16) * (-4.0d+0) 
term(17) = term(17) * (8.0d+0) 

do i = 1, nocc 
term(18) = term(18) + wm_interm_22_pt3(q,i) * wm_interm_49_pt3(p,i)
term(19) = term(19) + wm_interm_22_pt3(q,i) * wm_interm_51_pt3(p,i)
term(20) = term(20) + wm_interm_21_pt3(q,i) * wm_interm_51_pt3(p,i)
term(21) = term(21) + wm_interm_21_pt3(q,i) * wm_interm_49_pt3(p,i)
end do 

term(18) = term(18) * (-4.0d+0) 
term(19) = term(19) * (2.0d+0) 
term(20) = term(20) * (-4.0d+0) 
term(21) = term(21) * (8.0d+0) 

do a = nocc + 1, nactive 
term(22) = term(22) + wm_interm_3_pt3(p,a) * wm_interm_42_pt3(q,a)
term(23) = term(23) + wm_interm_42_pt3(q,a) * wm_interm_4_pt3(p,a)
term(24) = term(24) + wm_interm_3_pt3(a,p) * wm_interm_42_pt3(a,q)
term(25) = term(25) + wm_interm_42_pt3(a,q) * wm_interm_4_pt3(a,p)
term(26) = term(26) + wm_interm_23_pt3(q,a) * wm_interm_53_pt3(p,a)
term(27) = term(27) + wm_interm_24_pt3(q,a) * wm_interm_53_pt3(p,a)
term(28) = term(28) + wm_interm_24_pt3(a,q) * wm_interm_53_pt3(a,p)
term(29) = term(29) + wm_interm_23_pt3(a,q) * wm_interm_53_pt3(a,p)
end do 

term(22) = term(22) * (2.0d+0) 
term(23) = term(23) * (-4.0d+0) 
term(24) = term(24) * (2.0d+0) 
term(25) = term(25) * (-4.0d+0) 
term(26) = term(26) * (-4.0d+0) 
term(27) = term(27) * (2.0d+0) 
term(28) = term(28) * (2.0d+0) 
term(29) = term(29) * (-4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
term(30) = term(30) + wm_interm_14_pt3(p,q,i,j) * wm_interm_44_pt3(i,j)
term(31) = term(31) + wm_interm_15_pt3(p,q,i,j) * wm_interm_44_pt3(i,j)
term(32) = term(32) + wm_interm_16_pt3(p,q,i,j) * wm_interm_44_pt3(i,j)
term(33) = term(33) + wm_interm_17_pt3(p,q,i,j) * wm_interm_44_pt3(i,j)
term(34) = term(34) + wm_interm_11_pt3(i,j) * wm_interm_47_pt3(p,q,i,j)
term(35) = term(35) + wm_interm_12_pt3(i,j) * wm_interm_47_pt3(p,q,i,j)
term(36) = term(36) + wm_interm_25_pt3(i,j) * wm_interm_50_pt3(p,q,i,j)
term(37) = term(37) + wm_interm_28_pt3(i,j) * wm_interm_50_pt3(p,q,i,j)
term(38) = term(38) + wm_interm_36_pt3(p,q,i,j) * wm_interm_54_pt3(i,j)
term(39) = term(39) + wm_interm_35_pt3(p,q,i,j) * wm_interm_54_pt3(i,j)
term(40) = term(40) + wm_interm_33_pt3(p,q,i,j) * wm_interm_54_pt3(i,j)
term(41) = term(41) + wm_interm_34_pt3(p,q,i,j) * wm_interm_54_pt3(i,j)
end do 
end do 

term(30) = term(30) * (2.0d+0) 
term(31) = term(31) * (-4.0d+0) 
term(32) = term(32) * (2.0d+0) 
term(33) = term(33) * (-4.0d+0) 
term(34) = term(34) * (2.0d+0) 
term(35) = term(35) * (-4.0d+0) 
term(36) = term(36) * (2.0d+0) 
term(37) = term(37) * (-4.0d+0) 
term(38) = term(38) * (2.0d+0) 
term(39) = term(39) * (-4.0d+0) 
term(40) = term(40) * (2.0d+0) 
term(41) = term(41) * (-4.0d+0) 


    calc_D_vv_wm_pt3 = zero
    do s = 0, 41
    calc_D_vv_wm_pt3 = calc_D_vv_wm_pt3 + term(s)
    end do

    end function calc_D_vv_wm_pt3
        
    
    
    
    
    



end module density_exc_exc_functions_pt0123
