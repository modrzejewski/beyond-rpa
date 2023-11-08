module density_exc_exc_functions_pt4
          use ccsd_transformed_integrals
    use cc3_intermediates
    use arithmetic
    use s_gen
    use basis
    use eom_vectors
    
    implicit none
    !
    ! File generated automatically on 2016-08-16 07:08:56
    !
    real(F64), dimension(:, :), allocatable :: wm_interm_0_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_1_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_2_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_3_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_4_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_5_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_6_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_7_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_8_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_9_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_10_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_11_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_12_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_13_pt4 
real(F64) :: wm_interm_14_pt4 
real(F64) :: wm_interm_15_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_16_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_17_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_18_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_19_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_20_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_21_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_22_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_23_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_24_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_25_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_26_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_27_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_28_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_29_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_30_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_31_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_32_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_33_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_34_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_35_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_36_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_37_pt4 
real(F64) :: wm_interm_38_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_39_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_40_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_41_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_42_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_43_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_44_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_45_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_46_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_47_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_48_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_49_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_50_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_51_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_52_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_53_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_54_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_55_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_56_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_57_pt4 

    contains
    
    subroutine wm_intermediates_ccsd_init_pt4(nocc, nactive)
    integer, intent(in) :: nocc
    integer, intent(in) :: nactive
    allocate(wm_interm_0_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_1_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_2_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_3_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_4_pt4(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_5_pt4(1: nocc, 1: nocc))
allocate(wm_interm_6_pt4(1: nocc, 1: nocc))
allocate(wm_interm_7_pt4(1: nocc, 1: nocc))
allocate(wm_interm_8_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_9_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_10_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_11_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_12_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_13_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_16_pt4(1: nocc, 1: nocc))
allocate(wm_interm_17_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_18_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_19_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_20_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_21_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_22_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_23_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_24_pt4(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_25_pt4(1: nocc, 1: nocc))
allocate(wm_interm_26_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_27_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_28_pt4(1: nocc, 1: nocc))
allocate(wm_interm_29_pt4(1: nocc, 1: nocc))
allocate(wm_interm_30_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_31_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_32_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_33_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_34_pt4(1: nocc, 1: nocc))
allocate(wm_interm_35_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_36_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_37_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_39_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_40_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_41_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_42_pt4(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_43_pt4(1: nocc, 1: nocc))
allocate(wm_interm_44_pt4(1: nocc, 1: nocc))
allocate(wm_interm_45_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_46_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_47_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_48_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_49_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_50_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_51_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_52_pt4(1: nocc, 1: nocc))
allocate(wm_interm_53_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_54_pt4(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_55_pt4(1: nocc, 1: nocc))
allocate(wm_interm_56_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_57_pt4(nocc+1: nactive, nocc+1: nactive))
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
deallocate(wm_interm_16_pt4)
deallocate(wm_interm_17_pt4)
deallocate(wm_interm_18_pt4)
deallocate(wm_interm_19_pt4)
deallocate(wm_interm_20_pt4)
deallocate(wm_interm_21_pt4)
deallocate(wm_interm_22_pt4)
deallocate(wm_interm_23_pt4)
deallocate(wm_interm_24_pt4)
deallocate(wm_interm_25_pt4)
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

    end subroutine wm_intermediates_ccsd_free_pt4
    
    subroutine wm_intermediates_ccsd_pt4(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr)
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
sum = sum + r2(vrdav_Rl, a,i,b,j) * t1(a,i)
end do 
end do 
wm_interm_0_pt4(b, j) = wm_interm_0_pt4(b, j) + sum 
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
wm_interm_1_pt4(b, j) = wm_interm_1_pt4(b, j) + sum 
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
wm_interm_2_pt4(b, j) = wm_interm_2_pt4(b, j) + sum 
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
wm_interm_3_pt4(b, j) = wm_interm_3_pt4(b, j) + sum 
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
wm_interm_4_pt4(i, j, k, l) = wm_interm_4_pt4(i, j, k, l) + sum 
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
wm_interm_5_pt4(i, j) = wm_interm_5_pt4(i, j) + sum 
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
wm_interm_6_pt4(j, k) = wm_interm_6_pt4(j, k) + sum 
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
wm_interm_7_pt4(j, k) = wm_interm_7_pt4(j, k) + sum 
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
wm_interm_8_pt4(b, j, i, k) = wm_interm_8_pt4(b, j, i, k) + sum 
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
sum = sum + r2(vrdav_Rr, a,j,b,i) * s1(a,k)
end do 
wm_interm_9_pt4(b, j, i, k) = wm_interm_9_pt4(b, j, i, k) + sum 
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
wm_interm_10_pt4(b, c) = wm_interm_10_pt4(b, c) + sum 
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
wm_interm_11_pt4(a, b) = wm_interm_11_pt4(a, b) + sum 
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
wm_interm_12_pt4(b, c) = wm_interm_12_pt4(b, c) + sum 
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
wm_interm_13_pt4(b, i, j, k) = wm_interm_13_pt4(b, i, j, k) + sum 
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
wm_interm_14_pt4 = wm_interm_14_pt4 + sum 
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
wm_interm_15_pt4 = wm_interm_15_pt4 + sum 
!$omp parallel private(a, i, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do i = 1, nocc 
do j = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * r1(vrdav_Rr, a,j)
end do 
wm_interm_16_pt4(i, j) = wm_interm_16_pt4(i, j) + sum 
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
wm_interm_17_pt4(b, j) = wm_interm_17_pt4(b, j) + sum 
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
wm_interm_18_pt4(b, i, j, k) = wm_interm_18_pt4(b, i, j, k) + sum 
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
wm_interm_19_pt4(b, j) = wm_interm_19_pt4(b, j) + sum 
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
wm_interm_20_pt4(b, i, j, k) = wm_interm_20_pt4(b, i, j, k) + sum 
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
wm_interm_21_pt4(b, j) = wm_interm_21_pt4(b, j) + sum 
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
wm_interm_22_pt4(b, j) = wm_interm_22_pt4(b, j) + sum 
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
wm_interm_23_pt4(b, j) = wm_interm_23_pt4(b, j) + sum 
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
wm_interm_24_pt4(i, j, k, l) = wm_interm_24_pt4(i, j, k, l) + sum 
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
wm_interm_25_pt4(i, j) = wm_interm_25_pt4(i, j) + sum 
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
wm_interm_26_pt4(b, i, j, k) = wm_interm_26_pt4(b, i, j, k) + sum 
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
wm_interm_27_pt4(b, j) = wm_interm_27_pt4(b, j) + sum 
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
wm_interm_28_pt4(j, k) = wm_interm_28_pt4(j, k) + sum 
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
wm_interm_29_pt4(j, k) = wm_interm_29_pt4(j, k) + sum 
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
wm_interm_30_pt4(a, b) = wm_interm_30_pt4(a, b) + sum 
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
wm_interm_31_pt4(b, c) = wm_interm_31_pt4(b, c) + sum 
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
wm_interm_32_pt4(a, b) = wm_interm_32_pt4(a, b) + sum 
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
wm_interm_33_pt4(b, c) = wm_interm_33_pt4(b, c) + sum 
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
wm_interm_34_pt4(i, j) = wm_interm_34_pt4(i, j) + sum 
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
wm_interm_35_pt4(b, j, i, k) = wm_interm_35_pt4(b, j, i, k) + sum 
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
wm_interm_36_pt4(a, b) = wm_interm_36_pt4(a, b) + sum 
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
wm_interm_37_pt4(b, i, j, k) = wm_interm_37_pt4(b, i, j, k) + sum 
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
wm_interm_38_pt4 = wm_interm_38_pt4 + sum 
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
wm_interm_39_pt4(b, i, j, k) = wm_interm_39_pt4(b, i, j, k) + sum 
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
wm_interm_40_pt4(b, c) = wm_interm_40_pt4(b, c) + sum 
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
wm_interm_41_pt4(b, c) = wm_interm_41_pt4(b, c) + sum 
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
wm_interm_42_pt4(i, j, k, l) = wm_interm_42_pt4(i, j, k, l) + sum 
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
wm_interm_43_pt4(j, k) = wm_interm_43_pt4(j, k) + sum 
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
wm_interm_44_pt4(j, k) = wm_interm_44_pt4(j, k) + sum 
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
wm_interm_45_pt4(b, i, k, j) = wm_interm_45_pt4(b, i, k, j) + sum 
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
wm_interm_46_pt4(b, i, j, k) = wm_interm_46_pt4(b, i, j, k) + sum 
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
sum = sum + r1(vrdav_Rl, a,i) * r2(vrdav_Rr, a,i,b,j)
end do 
end do 
wm_interm_47_pt4(b, j) = wm_interm_47_pt4(b, j) + sum 
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
wm_interm_48_pt4(b, j) = wm_interm_48_pt4(b, j) + sum 
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
wm_interm_49_pt4(b, i, j, k) = wm_interm_49_pt4(b, i, j, k) + sum 
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
wm_interm_50_pt4(b, j) = wm_interm_50_pt4(b, j) + sum 
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
wm_interm_51_pt4(b, j, i, k) = wm_interm_51_pt4(b, j, i, k) + sum 
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
wm_interm_52_pt4(j, k) = wm_interm_52_pt4(j, k) + sum 
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
wm_interm_53_pt4(b, j) = wm_interm_53_pt4(b, j) + sum 
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
wm_interm_54_pt4(i, j, k, l) = wm_interm_54_pt4(i, j, k, l) + sum 
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
wm_interm_55_pt4(j, k) = wm_interm_55_pt4(j, k) + sum 
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
wm_interm_56_pt4(b, c) = wm_interm_56_pt4(b, c) + sum 
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
wm_interm_57_pt4(b, c) = wm_interm_57_pt4(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 



    end subroutine wm_intermediates_ccsd_pt4

    function calc_D_oo_wm_pt4(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_oo_wm_pt4
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
    real(F64), dimension(0:800) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + wm_interm_8_pt4(a, q, i, j) * wm_interm_9_pt4(a, i, p, j)
term(1) = term(1) + wm_interm_8_pt4(a, q, i, j) * wm_interm_9_pt4(a, p, i, j)
term(2) = term(2) + wm_interm_8_pt4(a, i, q, j) * wm_interm_9_pt4(a, p, i, j)
term(3) = term(3) + wm_interm_8_pt4(a, i, q, j) * wm_interm_9_pt4(a, i, p, j)
term(4) = term(4) + wm_interm_13_pt4(a, q, i, j) * wm_interm_9_pt4(a, p, i, j)
term(5) = term(5) + wm_interm_13_pt4(a, q, i, j) * wm_interm_9_pt4(a, i, p, j)
term(6) = term(6) + wm_interm_13_pt4(a, i, q, j) * wm_interm_9_pt4(a, i, p, j)
term(7) = term(7) + wm_interm_13_pt4(a, i, q, j) * wm_interm_9_pt4(a, p, i, j)
term(8) = term(8) + wm_interm_13_pt4(a, i, j, q) * wm_interm_9_pt4(a, i, j, p)
term(9) = term(9) + wm_interm_13_pt4(a, i, j, q) * wm_interm_9_pt4(a, j, i, p)
term(10) = term(10) + wm_interm_8_pt4(a, i, j, q) * wm_interm_9_pt4(a, j, i, p)
term(11) = term(11) + wm_interm_8_pt4(a, i, j, q) * wm_interm_9_pt4(a, i, j, p)
term(12) = term(12) + r1(vrdav_Rl, a,i) * wm_interm_1_pt4(a, j) * wm_interm_28_pt4(j, i)
term(13) = term(13) + r1(vrdav_Rl, a,i) * wm_interm_1_pt4(a, j) * wm_interm_29_pt4(j, i)
term(14) = term(14) + r1(vrdav_Rl, a,i) * wm_interm_28_pt4(j, i) * wm_interm_2_pt4(a, j)
term(15) = term(15) + r1(vrdav_Rl, a,i) * wm_interm_29_pt4(j, i) * wm_interm_2_pt4(a, j)
term(16) = term(16) + s1(a,i) * wm_interm_22_pt4(a, j) * wm_interm_42_pt4(i, p, q, j)
term(17) = term(17) + s1(a,i) * wm_interm_22_pt4(a, j) * wm_interm_42_pt4(p, i, q, j)
term(18) = term(18) + s1(a,i) * wm_interm_22_pt4(a, j) * wm_interm_42_pt4(p, i, j, q)
term(19) = term(19) + s1(a,i) * wm_interm_22_pt4(a, j) * wm_interm_42_pt4(i, p, j, q)
term(20) = term(20) + s1(a,i) * wm_interm_22_pt4(a, j) * wm_interm_43_pt4(i, j)
term(21) = term(21) + s1(a,i) * wm_interm_22_pt4(a, j) * wm_interm_44_pt4(i, j)
term(22) = term(22) + r1(vrdav_Rl, a,i) * wm_interm_17_pt4(a, j) * wm_interm_42_pt4(i, p, q, j)
term(23) = term(23) + r1(vrdav_Rl, a,i) * wm_interm_17_pt4(a, j) * wm_interm_42_pt4(i, p, j, q)
term(24) = term(24) + r1(vrdav_Rl, a,i) * wm_interm_17_pt4(a, j) * wm_interm_43_pt4(i, j)
term(25) = term(25) + r1(vrdav_Rl, a,i) * wm_interm_17_pt4(a, j) * wm_interm_44_pt4(i, j)
term(26) = term(26) + r1(vrdav_Rl, a,i) * wm_interm_23_pt4(a, j) * wm_interm_42_pt4(i, p, q, j)
term(27) = term(27) + r1(vrdav_Rl, a,i) * wm_interm_23_pt4(a, j) * wm_interm_42_pt4(i, p, j, q)
term(28) = term(28) + r1(vrdav_Rl, a,i) * wm_interm_23_pt4(a, j) * wm_interm_43_pt4(i, j)
term(29) = term(29) + r1(vrdav_Rl, a,i) * wm_interm_23_pt4(a, j) * wm_interm_44_pt4(i, j)
term(30) = term(30) + s1(a,i) * wm_interm_27_pt4(a, j) * wm_interm_42_pt4(i, p, q, j)
term(31) = term(31) + s1(a,i) * wm_interm_27_pt4(a, j) * wm_interm_42_pt4(p, i, q, j)
term(32) = term(32) + s1(a,i) * wm_interm_27_pt4(a, j) * wm_interm_42_pt4(p, i, j, q)
term(33) = term(33) + s1(a,i) * wm_interm_27_pt4(a, j) * wm_interm_42_pt4(i, p, j, q)
term(34) = term(34) + s1(a,i) * wm_interm_27_pt4(a, j) * wm_interm_43_pt4(i, j)
term(35) = term(35) + s1(a,i) * wm_interm_27_pt4(a, j) * wm_interm_44_pt4(i, j)
term(36) = term(36) + s1(a,i) * wm_interm_26_pt4(a, p, j, q) * wm_interm_43_pt4(i, j)
term(37) = term(37) + s1(a,i) * wm_interm_26_pt4(a, p, j, q) * wm_interm_44_pt4(i, j)
term(38) = term(38) + s1(a,p) * wm_interm_26_pt4(a, i, j, q) * wm_interm_43_pt4(i, j)
term(39) = term(39) + s1(a,p) * wm_interm_26_pt4(a, i, j, q) * wm_interm_44_pt4(i, j)
term(40) = term(40) + s1(a,p) * wm_interm_26_pt4(a, i, q, j) * wm_interm_43_pt4(i, j)
term(41) = term(41) + s1(a,p) * wm_interm_26_pt4(a, i, q, j) * wm_interm_44_pt4(i, j)
term(42) = term(42) + s1(a,i) * wm_interm_26_pt4(a, p, q, j) * wm_interm_43_pt4(i, j)
term(43) = term(43) + s1(a,i) * wm_interm_26_pt4(a, p, q, j) * wm_interm_44_pt4(i, j)
term(44) = term(44) + t1(a,i) * wm_interm_20_pt4(a, p, j, q) * wm_interm_52_pt4(i, j)
term(45) = term(45) + t1(a,i) * wm_interm_19_pt4(a, j) * wm_interm_52_pt4(i, j)
term(46) = term(46) + t1(a,i) * wm_interm_20_pt4(a, p, q, j) * wm_interm_52_pt4(i, j)
term(47) = term(47) + t1(a,i) * wm_interm_21_pt4(a, j) * wm_interm_52_pt4(i, j)
term(48) = term(48) + t1(a,i) * wm_interm_20_pt4(a, p, j, q) * wm_interm_55_pt4(i, j)
term(49) = term(49) + t1(a,i) * wm_interm_19_pt4(a, j) * wm_interm_55_pt4(i, j)
term(50) = term(50) + t1(a,i) * wm_interm_20_pt4(a, p, q, j) * wm_interm_55_pt4(i, j)
term(51) = term(51) + t1(a,i) * wm_interm_21_pt4(a, j) * wm_interm_55_pt4(i, j)
term(52) = term(52) + t1(a,i) * wm_interm_29_pt4(i, j) * wm_interm_49_pt4(a, q, j, p)
term(53) = term(53) + t1(a,i) * wm_interm_29_pt4(i, j) * wm_interm_50_pt4(a, j)
term(54) = term(54) + t1(a,i) * wm_interm_28_pt4(i, j) * wm_interm_49_pt4(a, q, j, p)
term(55) = term(55) + t1(a,i) * wm_interm_28_pt4(i, j) * wm_interm_50_pt4(a, j)
term(56) = term(56) + t1(a,i) * wm_interm_29_pt4(i, j) * wm_interm_49_pt4(a, j, q, p)
term(57) = term(57) + t1(a,i) * wm_interm_29_pt4(i, j) * wm_interm_53_pt4(a, j)
term(58) = term(58) + t1(a,i) * wm_interm_28_pt4(i, j) * wm_interm_49_pt4(a, j, q, p)
term(59) = term(59) + t1(a,i) * wm_interm_28_pt4(i, j) * wm_interm_53_pt4(a, j)
term(60) = term(60) + r1(vrdav_Rr, a,i) * wm_interm_0_pt4(a, j) * wm_interm_28_pt4(i, j)
term(61) = term(61) + r1(vrdav_Rr, a,i) * wm_interm_0_pt4(a, j) * wm_interm_29_pt4(i, j)
term(62) = term(62) + r1(vrdav_Rr, a,i) * wm_interm_28_pt4(i, j) * wm_interm_3_pt4(a, j)
term(63) = term(63) + r1(vrdav_Rr, a,i) * wm_interm_29_pt4(i, j) * wm_interm_3_pt4(a, j)
term(64) = term(64) + t1(a,i) * wm_interm_28_pt4(i, j) * wm_interm_51_pt4(a, q, j, p)
term(65) = term(65) + t1(a,i) * wm_interm_29_pt4(i, j) * wm_interm_51_pt4(a, q, j, p)
term(66) = term(66) + t1(a,i) * wm_interm_29_pt4(i, j) * wm_interm_51_pt4(a, j, q, p)
term(67) = term(67) + t1(a,i) * wm_interm_28_pt4(i, j) * wm_interm_51_pt4(a, j, q, p)
term(68) = term(68) + t1(a,q) * wm_interm_20_pt4(a, i, j, p) * wm_interm_52_pt4(i, j)
term(69) = term(69) + t1(a,q) * wm_interm_20_pt4(a, i, p, j) * wm_interm_52_pt4(i, j)
term(70) = term(70) + t1(a,q) * wm_interm_20_pt4(a, i, j, p) * wm_interm_55_pt4(i, j)
term(71) = term(71) + t1(a,q) * wm_interm_20_pt4(a, i, p, j) * wm_interm_55_pt4(i, j)
term(72) = term(72) + t1(a,i) * wm_interm_19_pt4(a, j) * wm_interm_54_pt4(p, i, q, j)
term(73) = term(73) + t1(a,i) * wm_interm_21_pt4(a, j) * wm_interm_54_pt4(p, i, q, j)
term(74) = term(74) + t1(a,i) * wm_interm_19_pt4(a, j) * wm_interm_54_pt4(p, i, j, q)
term(75) = term(75) + t1(a,i) * wm_interm_21_pt4(a, j) * wm_interm_54_pt4(p, i, j, q)
term(76) = term(76) + t1(a,i) * wm_interm_24_pt4(i, p, j, q) * wm_interm_53_pt4(a, j)
term(77) = term(77) + t1(a,i) * wm_interm_24_pt4(p, i, j, q) * wm_interm_53_pt4(a, j)
term(78) = term(78) + t1(a,i) * wm_interm_24_pt4(i, p, j, q) * wm_interm_50_pt4(a, j)
term(79) = term(79) + t1(a,i) * wm_interm_24_pt4(p, i, j, q) * wm_interm_50_pt4(a, j)
term(80) = term(80) + t1(a,i) * wm_interm_19_pt4(a, j) * wm_interm_54_pt4(i, p, q, j)
term(81) = term(81) + t1(a,i) * wm_interm_19_pt4(a, j) * wm_interm_54_pt4(i, p, j, q)
term(82) = term(82) + t1(a,i) * wm_interm_21_pt4(a, j) * wm_interm_54_pt4(i, p, q, j)
term(83) = term(83) + t1(a,i) * wm_interm_21_pt4(a, j) * wm_interm_54_pt4(i, p, j, q)
term(84) = term(84) + r1(vrdav_Rr, a,i) * wm_interm_0_pt4(a, j) * wm_interm_24_pt4(i, p, j, q)
term(85) = term(85) + r1(vrdav_Rr, a,i) * wm_interm_24_pt4(i, p, j, q) * wm_interm_3_pt4(a, j)
term(86) = term(86) + r1(vrdav_Rr, a,i) * wm_interm_0_pt4(a, j) * wm_interm_24_pt4(p, i, j, q)
term(87) = term(87) + r1(vrdav_Rr, a,i) * wm_interm_24_pt4(p, i, j, q) * wm_interm_3_pt4(a, j)
end do 
end do 
end do 

term(0) = -term(0) 
term(1) = term(1) * 2.0d+0 
term(2) = -term(2) 
term(3) = term(3) * 2.0d+0 
term(4) = -term(4) 
term(5) = term(5) * 2.0d+0 
term(6) = -term(6) 
term(7) = term(7) * 2.0d+0 
term(8) = -term(8) 
term(9) = term(9) * 2.0d+0 
term(10) = -term(10) 
term(11) = term(11) * 2.0d+0 
term(12) = term(12) * 8.0d+0 
term(13) = term(13) * (-16.0d+0) 
term(14) = term(14) * (-4.0d+0) 
term(15) = term(15) * 8.0d+0 
term(16) = term(16) * 2.0d+0 
term(17) = term(17) * (-4.0d+0) 
term(18) = term(18) * 2.0d+0 
term(19) = term(19) * (-4.0d+0) 
term(20) = term(20) * (-8.0d+0) 
term(21) = term(21) * 16.0d+0 
term(22) = term(22) * 4.0d+0 
term(23) = term(23) * (-8.0d+0) 
term(24) = term(24) * (-8.0d+0) 
term(25) = term(25) * 16.0d+0 
term(26) = term(26) * (-8.0d+0) 
term(27) = term(27) * 16.0d+0 
term(28) = term(28) * 16.0d+0 
term(29) = term(29) * (-32.0d+0) 
term(30) = term(30) * (-4.0d+0) 
term(31) = term(31) * 8.0d+0 
term(32) = term(32) * (-4.0d+0) 
term(33) = term(33) * 8.0d+0 
term(34) = term(34) * 16.0d+0 
term(35) = term(35) * (-32.0d+0) 
term(36) = term(36) * 4.0d+0 
term(37) = term(37) * (-8.0d+0) 
term(38) = term(38) * (-8.0d+0) 
term(39) = term(39) * 16.0d+0 
term(40) = term(40) * 4.0d+0 
term(41) = term(41) * (-8.0d+0) 
term(42) = term(42) * (-8.0d+0) 
term(43) = term(43) * 16.0d+0 
term(44) = term(44) * (-4.0d+0) 
term(45) = term(45) * 8.0d+0 
term(46) = term(46) * 8.0d+0 
term(47) = term(47) * (-16.0d+0) 
term(48) = term(48) * 8.0d+0 
term(49) = term(49) * (-16.0d+0) 
term(50) = term(50) * (-16.0d+0) 
term(51) = term(51) * 32.0d+0 
term(52) = term(52) * (-8.0d+0) 
term(53) = term(53) * 32.0d+0 
term(54) = term(54) * 4.0d+0 
term(55) = term(55) * (-16.0d+0) 
term(56) = term(56) * 4.0d+0 
term(57) = term(57) * (-16.0d+0) 
term(58) = term(58) * (-2.0d+0) 
term(59) = term(59) * 8.0d+0 
term(60) = term(60) * (-16.0d+0) 
term(61) = term(61) * 32.0d+0 
term(62) = term(62) * 8.0d+0 
term(63) = term(63) * (-16.0d+0) 
term(64) = term(64) * (-2.0d+0) 
term(65) = term(65) * 4.0d+0 
term(66) = term(66) * (-8.0d+0) 
term(67) = term(67) * 4.0d+0 
term(68) = term(68) * 4.0d+0 
term(69) = term(69) * (-2.0d+0) 
term(70) = term(70) * (-8.0d+0) 
term(71) = term(71) * 4.0d+0 
term(72) = term(72) * 2.0d+0 
term(73) = term(73) * (-4.0d+0) 
term(74) = -term(74) 
term(75) = term(75) * 2.0d+0 
term(76) = term(76) * 4.0d+0 
term(77) = term(77) * (-2.0d+0) 
term(78) = term(78) * (-8.0d+0) 
term(79) = term(79) * 4.0d+0 
term(80) = -term(80) 
term(81) = term(81) * 2.0d+0 
term(82) = term(82) * 2.0d+0 
term(83) = term(83) * (-4.0d+0) 
term(84) = term(84) * (-8.0d+0) 
term(85) = term(85) * 4.0d+0 
term(86) = term(86) * 4.0d+0 
term(87) = term(87) * (-2.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(88) = term(88) + s1(a,q) * wm_interm_28_pt4(i, j) * wm_interm_39_pt4(a, j, p, i)
term(89) = term(89) + s1(a,q) * wm_interm_28_pt4(i, j) * wm_interm_39_pt4(a, j, i, p)
term(90) = term(90) + s1(a,q) * wm_interm_29_pt4(i, j) * wm_interm_39_pt4(a, j, p, i)
term(91) = term(91) + s1(a,q) * wm_interm_29_pt4(i, j) * wm_interm_39_pt4(a, j, i, p)
term(92) = term(92) + r1(vrdav_Rl, a,i) * wm_interm_1_pt4(a, j) * wm_interm_24_pt4(j, p, i, q)
term(93) = term(93) + r1(vrdav_Rl, a,i) * wm_interm_24_pt4(j, p, i, q) * wm_interm_2_pt4(a, j)
term(94) = term(94) + r1(vrdav_Rl, a,i) * wm_interm_1_pt4(a, j) * wm_interm_24_pt4(p, j, i, q)
term(95) = term(95) + r1(vrdav_Rl, a,i) * wm_interm_24_pt4(p, j, i, q) * wm_interm_2_pt4(a, j)
term(96) = term(96) + s1(a,i) * wm_interm_24_pt4(p, j, q, i) * wm_interm_48_pt4(a, j)
term(97) = term(97) + s1(a,i) * wm_interm_24_pt4(p, j, q, i) * wm_interm_47_pt4(a, j)
term(98) = term(98) + s1(a,i) * wm_interm_24_pt4(p, j, i, q) * wm_interm_48_pt4(a, j)
term(99) = term(99) + s1(a,i) * wm_interm_24_pt4(p, j, i, q) * wm_interm_47_pt4(a, j)
term(100) = term(100) + s1(a,i) * wm_interm_24_pt4(j, p, i, q) * wm_interm_48_pt4(a, j)
term(101) = term(101) + s1(a,i) * wm_interm_24_pt4(j, p, i, q) * wm_interm_47_pt4(a, j)
term(102) = term(102) + s1(a,i) * wm_interm_24_pt4(j, p, q, i) * wm_interm_48_pt4(a, j)
term(103) = term(103) + s1(a,i) * wm_interm_24_pt4(j, p, q, i) * wm_interm_47_pt4(a, j)
term(104) = term(104) + t1(a,q) * wm_interm_28_pt4(i, j) * wm_interm_51_pt4(a, p, j, i)
term(105) = term(105) + t1(a,q) * wm_interm_29_pt4(i, j) * wm_interm_51_pt4(a, p, j, i)
term(106) = term(106) + t1(a,q) * wm_interm_28_pt4(i, j) * wm_interm_49_pt4(a, p, j, i)
term(107) = term(107) + t1(a,q) * wm_interm_29_pt4(i, j) * wm_interm_49_pt4(a, p, j, i)
term(108) = term(108) + t1(a,q) * wm_interm_28_pt4(i, j) * wm_interm_51_pt4(a, j, p, i)
term(109) = term(109) + t1(a,q) * wm_interm_28_pt4(i, j) * wm_interm_49_pt4(a, j, p, i)
term(110) = term(110) + t1(a,q) * wm_interm_29_pt4(i, j) * wm_interm_51_pt4(a, j, p, i)
term(111) = term(111) + t1(a,q) * wm_interm_29_pt4(i, j) * wm_interm_49_pt4(a, j, p, i)
end do 
end do 
end do 

term(88) = term(88) * (-2.0d+0) 
term(89) = term(89) * 4.0d+0 
term(90) = term(90) * 4.0d+0 
term(91) = term(91) * (-8.0d+0) 
term(92) = term(92) * 8.0d+0 
term(93) = term(93) * (-4.0d+0) 
term(94) = term(94) * (-4.0d+0) 
term(95) = term(95) * 2.0d+0 
term(96) = term(96) * (-4.0d+0) 
term(97) = term(97) * 8.0d+0 
term(98) = term(98) * 2.0d+0 
term(99) = term(99) * (-4.0d+0) 
term(100) = term(100) * (-4.0d+0) 
term(101) = term(101) * 8.0d+0 
term(102) = term(102) * 2.0d+0 
term(103) = term(103) * (-4.0d+0) 
term(104) = term(104) * 2.0d+0 
term(105) = term(105) * (-4.0d+0) 
term(106) = -term(106) 
term(107) = term(107) * 2.0d+0 
term(108) = -term(108) 
term(109) = term(109) * 2.0d+0 
term(110) = term(110) * 2.0d+0 
term(111) = term(111) * (-4.0d+0) 

do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(112) = term(112) + s2(a,b,i,j) * wm_interm_1_pt4(a, j) * wm_interm_22_pt4(b, i)
term(113) = term(113) + s2(a,b,i,j) * wm_interm_1_pt4(a, i) * wm_interm_22_pt4(b, j)
term(114) = term(114) + s2(a,b,i,j) * wm_interm_22_pt4(b, i) * wm_interm_2_pt4(a, j)
term(115) = term(115) + s2(a,b,i,j) * wm_interm_22_pt4(b, j) * wm_interm_2_pt4(a, i)
term(116) = term(116) + s2(a,b,i,j) * wm_interm_1_pt4(a, i) * wm_interm_27_pt4(b, j)
term(117) = term(117) + s2(a,b,i,j) * wm_interm_27_pt4(b, j) * wm_interm_2_pt4(a, i)
term(118) = term(118) + s2(a,b,i,j) * wm_interm_1_pt4(a, j) * wm_interm_27_pt4(b, i)
term(119) = term(119) + s2(a,b,i,j) * wm_interm_27_pt4(b, i) * wm_interm_2_pt4(a, j)
term(120) = term(120) + s2(a,b,i,q) * wm_interm_22_pt4(b, j) * wm_interm_46_pt4(a, i, p, j)
term(121) = term(121) + s2(a,b,i,q) * wm_interm_22_pt4(b, j) * wm_interm_46_pt4(a, p, i, j)
term(122) = term(122) + s2(a,b,i,q) * wm_interm_22_pt4(b, j) * wm_interm_9_pt4(a, p, i, j)
term(123) = term(123) + s2(a,b,i,q) * wm_interm_22_pt4(b, j) * wm_interm_9_pt4(a, i, p, j)
term(124) = term(124) + s2(a,b,i,q) * wm_interm_27_pt4(b, j) * wm_interm_9_pt4(a, p, i, j)
term(125) = term(125) + s2(a,b,i,q) * wm_interm_27_pt4(b, j) * wm_interm_9_pt4(a, i, p, j)
term(126) = term(126) + s2(a,b,i,q) * wm_interm_27_pt4(b, j) * wm_interm_46_pt4(a, i, p, j)
term(127) = term(127) + s2(a,b,i,q) * wm_interm_27_pt4(b, j) * wm_interm_46_pt4(a, p, i, j)
term(128) = term(128) + s2(a,b,i,j) * wm_interm_22_pt4(b, i) * wm_interm_46_pt4(a, j, p, q)
term(129) = term(129) + s2(a,b,i,j) * wm_interm_22_pt4(b, i) * wm_interm_46_pt4(a, p, j, q)
term(130) = term(130) + s2(a,b,i,j) * wm_interm_22_pt4(b, j) * wm_interm_46_pt4(a, i, p, q)
term(131) = term(131) + s2(a,b,i,j) * wm_interm_22_pt4(b, j) * wm_interm_46_pt4(a, p, i, q)
term(132) = term(132) + s2(a,b,i,j) * wm_interm_22_pt4(b, i) * wm_interm_9_pt4(a, p, j, q)
term(133) = term(133) + s2(a,b,i,j) * wm_interm_22_pt4(b, j) * wm_interm_9_pt4(a, p, i, q)
term(134) = term(134) + s2(a,b,i,j) * wm_interm_22_pt4(b, i) * wm_interm_9_pt4(a, j, p, q)
term(135) = term(135) + s2(a,b,i,j) * wm_interm_22_pt4(b, j) * wm_interm_9_pt4(a, i, p, q)
term(136) = term(136) + s2(a,b,i,j) * wm_interm_27_pt4(b, j) * wm_interm_46_pt4(a, i, p, q)
term(137) = term(137) + s2(a,b,i,j) * wm_interm_27_pt4(b, j) * wm_interm_46_pt4(a, p, i, q)
term(138) = term(138) + s2(a,b,i,j) * wm_interm_27_pt4(b, j) * wm_interm_9_pt4(a, p, i, q)
term(139) = term(139) + s2(a,b,i,j) * wm_interm_27_pt4(b, j) * wm_interm_9_pt4(a, i, p, q)
term(140) = term(140) + s2(a,b,i,j) * wm_interm_27_pt4(b, i) * wm_interm_46_pt4(a, j, p, q)
term(141) = term(141) + s2(a,b,i,j) * wm_interm_27_pt4(b, i) * wm_interm_46_pt4(a, p, j, q)
term(142) = term(142) + s2(a,b,i,j) * wm_interm_27_pt4(b, i) * wm_interm_9_pt4(a, p, j, q)
term(143) = term(143) + s2(a,b,i,j) * wm_interm_27_pt4(b, i) * wm_interm_9_pt4(a, j, p, q)
term(144) = term(144) + s2(a,b,i,j) * wm_interm_1_pt4(a, j) * wm_interm_26_pt4(b, p, i, q)
term(145) = term(145) + s2(a,b,i,j) * wm_interm_1_pt4(a, i) * wm_interm_26_pt4(b, p, j, q)
term(146) = term(146) + s2(a,b,i,j) * wm_interm_26_pt4(b, p, i, q) * wm_interm_2_pt4(a, j)
term(147) = term(147) + s2(a,b,i,j) * wm_interm_26_pt4(b, p, j, q) * wm_interm_2_pt4(a, i)
term(148) = term(148) + s2(a,b,i,j) * wm_interm_1_pt4(a, i) * wm_interm_26_pt4(b, p, q, j)
term(149) = term(149) + s2(a,b,i,j) * wm_interm_26_pt4(b, p, q, j) * wm_interm_2_pt4(a, i)
term(150) = term(150) + s2(a,b,i,j) * wm_interm_18_pt4(a, p, q, j) * wm_interm_48_pt4(b, i)
term(151) = term(151) + s2(a,b,i,j) * wm_interm_18_pt4(a, p, q, j) * wm_interm_47_pt4(b, i)
term(152) = term(152) + s2(a,b,i,j) * wm_interm_18_pt4(a, p, j, q) * wm_interm_48_pt4(b, i)
term(153) = term(153) + s2(a,b,i,j) * wm_interm_18_pt4(a, p, j, q) * wm_interm_47_pt4(b, i)
term(154) = term(154) + s2(a,b,i,j) * wm_interm_18_pt4(a, p, i, q) * wm_interm_48_pt4(b, j)
term(155) = term(155) + s2(a,b,i,j) * wm_interm_18_pt4(a, p, i, q) * wm_interm_47_pt4(b, j)
term(156) = term(156) + t2(a,b,i,j) * wm_interm_0_pt4(b, i) * wm_interm_20_pt4(a, p, j, q)
term(157) = term(157) + t2(a,b,i,j) * wm_interm_0_pt4(b, i) * wm_interm_19_pt4(a, j)
term(158) = term(158) + t2(a,b,i,j) * wm_interm_0_pt4(b, i) * wm_interm_20_pt4(a, p, q, j)
term(159) = term(159) + t2(a,b,i,j) * wm_interm_0_pt4(b, i) * wm_interm_21_pt4(a, j)
term(160) = term(160) + t2(a,b,i,j) * wm_interm_0_pt4(b, j) * wm_interm_21_pt4(a, i)
term(161) = term(161) + t2(a,b,i,j) * wm_interm_0_pt4(b, j) * wm_interm_20_pt4(a, p, i, q)
term(162) = term(162) + t2(a,b,i,j) * wm_interm_0_pt4(b, j) * wm_interm_19_pt4(a, i)
term(163) = term(163) + t2(a,b,i,j) * wm_interm_20_pt4(a, p, j, q) * wm_interm_3_pt4(b, i)
term(164) = term(164) + t2(a,b,i,j) * wm_interm_19_pt4(a, j) * wm_interm_3_pt4(b, i)
term(165) = term(165) + t2(a,b,i,j) * wm_interm_20_pt4(a, p, q, j) * wm_interm_3_pt4(b, i)
term(166) = term(166) + t2(a,b,i,j) * wm_interm_21_pt4(a, j) * wm_interm_3_pt4(b, i)
term(167) = term(167) + t2(a,b,i,j) * wm_interm_21_pt4(a, i) * wm_interm_3_pt4(b, j)
term(168) = term(168) + t2(a,b,i,j) * wm_interm_20_pt4(a, p, i, q) * wm_interm_3_pt4(b, j)
term(169) = term(169) + t2(a,b,i,j) * wm_interm_19_pt4(a, i) * wm_interm_3_pt4(b, j)
term(170) = term(170) + t2(a,b,i,j) * wm_interm_13_pt4(b, p, i, q) * wm_interm_19_pt4(a, j)
term(171) = term(171) + t2(a,b,i,j) * wm_interm_13_pt4(b, p, i, q) * wm_interm_21_pt4(a, j)
term(172) = term(172) + t2(a,b,i,j) * wm_interm_35_pt4(a, p, j, q) * wm_interm_50_pt4(b, i)
term(173) = term(173) + t2(a,b,i,j) * wm_interm_35_pt4(a, j, p, q) * wm_interm_50_pt4(b, i)
term(174) = term(174) + t2(a,b,i,j) * wm_interm_13_pt4(b, p, j, q) * wm_interm_21_pt4(a, i)
term(175) = term(175) + t2(a,b,i,j) * wm_interm_35_pt4(a, i, p, q) * wm_interm_50_pt4(b, j)
term(176) = term(176) + t2(a,b,i,j) * wm_interm_13_pt4(b, p, j, q) * wm_interm_19_pt4(a, i)
term(177) = term(177) + t2(a,b,i,j) * wm_interm_35_pt4(a, p, i, q) * wm_interm_50_pt4(b, j)
term(178) = term(178) + t2(a,b,i,j) * wm_interm_35_pt4(a, p, j, q) * wm_interm_53_pt4(b, i)
term(179) = term(179) + t2(a,b,i,j) * wm_interm_13_pt4(b, i, p, q) * wm_interm_19_pt4(a, j)
term(180) = term(180) + t2(a,b,i,j) * wm_interm_19_pt4(a, j) * wm_interm_8_pt4(b, i, p, q)
term(181) = term(181) + t2(a,b,i,j) * wm_interm_35_pt4(a, j, p, q) * wm_interm_53_pt4(b, i)
term(182) = term(182) + t2(a,b,i,j) * wm_interm_21_pt4(a, j) * wm_interm_8_pt4(b, i, p, q)
term(183) = term(183) + t2(a,b,i,j) * wm_interm_13_pt4(b, i, p, q) * wm_interm_21_pt4(a, j)
term(184) = term(184) + t2(a,b,i,j) * wm_interm_35_pt4(a, i, p, q) * wm_interm_53_pt4(b, j)
term(185) = term(185) + t2(a,b,i,j) * wm_interm_13_pt4(b, j, p, q) * wm_interm_21_pt4(a, i)
term(186) = term(186) + t2(a,b,i,j) * wm_interm_35_pt4(a, p, i, q) * wm_interm_53_pt4(b, j)
term(187) = term(187) + t2(a,b,i,j) * wm_interm_13_pt4(b, j, p, q) * wm_interm_19_pt4(a, i)
term(188) = term(188) + t2(a,b,i,j) * wm_interm_19_pt4(a, j) * wm_interm_8_pt4(b, p, i, q)
term(189) = term(189) + t2(a,b,i,j) * wm_interm_21_pt4(a, j) * wm_interm_8_pt4(b, p, i, q)
term(190) = term(190) + t2(a,b,i,j) * wm_interm_21_pt4(a, i) * wm_interm_8_pt4(b, p, j, q)
term(191) = term(191) + t2(a,b,i,j) * wm_interm_21_pt4(a, i) * wm_interm_8_pt4(b, j, p, q)
term(192) = term(192) + t2(a,b,i,j) * wm_interm_19_pt4(a, i) * wm_interm_8_pt4(b, p, j, q)
term(193) = term(193) + t2(a,b,i,j) * wm_interm_19_pt4(a, i) * wm_interm_8_pt4(b, j, p, q)
end do 
end do 
end do 
end do 

term(112) = term(112) * 8.0d+0 
term(113) = term(113) * (-16.0d+0) 
term(114) = term(114) * (-4.0d+0) 
term(115) = term(115) * 8.0d+0 
term(116) = term(116) * 32.0d+0 
term(117) = term(117) * (-16.0d+0) 
term(118) = term(118) * (-16.0d+0) 
term(119) = term(119) * 8.0d+0 
term(120) = term(120) * 2.0d+0 
term(121) = term(121) * (-4.0d+0) 
term(122) = term(122) * 2.0d+0 
term(123) = term(123) * (-4.0d+0) 
term(124) = term(124) * (-4.0d+0) 
term(125) = term(125) * 8.0d+0 
term(126) = term(126) * (-4.0d+0) 
term(127) = term(127) * 8.0d+0 
term(128) = -term(128) 
term(129) = term(129) * 2.0d+0 
term(130) = term(130) * 2.0d+0 
term(131) = term(131) * (-4.0d+0) 
term(132) = -term(132) 
term(133) = term(133) * 2.0d+0 
term(134) = term(134) * 2.0d+0 
term(135) = term(135) * (-4.0d+0) 
term(136) = term(136) * (-4.0d+0) 
term(137) = term(137) * 8.0d+0 
term(138) = term(138) * (-4.0d+0) 
term(139) = term(139) * 8.0d+0 
term(140) = term(140) * 2.0d+0 
term(141) = term(141) * (-4.0d+0) 
term(142) = term(142) * 2.0d+0 
term(143) = term(143) * (-4.0d+0) 
term(144) = term(144) * (-4.0d+0) 
term(145) = term(145) * 8.0d+0 
term(146) = term(146) * 2.0d+0 
term(147) = term(147) * (-4.0d+0) 
term(148) = term(148) * (-16.0d+0) 
term(149) = term(149) * 8.0d+0 
term(150) = term(150) * (-8.0d+0) 
term(151) = term(151) * 16.0d+0 
term(152) = term(152) * 4.0d+0 
term(153) = term(153) * (-8.0d+0) 
term(154) = term(154) * (-8.0d+0) 
term(155) = term(155) * 16.0d+0 
term(156) = term(156) * 8.0d+0 
term(157) = term(157) * (-16.0d+0) 
term(158) = term(158) * (-16.0d+0) 
term(159) = term(159) * 32.0d+0 
term(160) = term(160) * (-64.0d+0) 
term(161) = term(161) * (-16.0d+0) 
term(162) = term(162) * 32.0d+0 
term(163) = term(163) * (-4.0d+0) 
term(164) = term(164) * 8.0d+0 
term(165) = term(165) * 8.0d+0 
term(166) = term(166) * (-16.0d+0) 
term(167) = term(167) * 32.0d+0 
term(168) = term(168) * 8.0d+0 
term(169) = term(169) * (-16.0d+0) 
term(170) = term(170) * 2.0d+0 
term(171) = term(171) * (-4.0d+0) 
term(172) = term(172) * 4.0d+0 
term(173) = term(173) * (-8.0d+0) 
term(174) = term(174) * 8.0d+0 
term(175) = term(175) * 16.0d+0 
term(176) = term(176) * (-4.0d+0) 
term(177) = term(177) * (-8.0d+0) 
term(178) = term(178) * (-2.0d+0) 
term(179) = -term(179) 
term(180) = term(180) * 2.0d+0 
term(181) = term(181) * 4.0d+0 
term(182) = term(182) * (-4.0d+0) 
term(183) = term(183) * 2.0d+0 
term(184) = term(184) * (-8.0d+0) 
term(185) = term(185) * (-4.0d+0) 
term(186) = term(186) * 4.0d+0 
term(187) = term(187) * 2.0d+0 
term(188) = -term(188) 
term(189) = term(189) * 2.0d+0 
term(190) = term(190) * (-4.0d+0) 
term(191) = term(191) * 8.0d+0 
term(192) = term(192) * 2.0d+0 
term(193) = term(193) * (-4.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(194) = term(194) + s2(a,b,i,q) * wm_interm_17_pt4(a, j) * wm_interm_39_pt4(b, j, p, i)
term(195) = term(195) + s2(a,b,q,i) * wm_interm_17_pt4(a, j) * wm_interm_39_pt4(b, j, i, p)
term(196) = term(196) + s2(a,b,q,i) * wm_interm_17_pt4(a, j) * wm_interm_39_pt4(b, j, p, i)
term(197) = term(197) + s2(a,b,i,q) * wm_interm_23_pt4(a, j) * wm_interm_39_pt4(b, j, p, i)
term(198) = term(198) + s2(a,b,q,i) * wm_interm_23_pt4(a, j) * wm_interm_39_pt4(b, j, i, p)
term(199) = term(199) + s2(a,b,q,i) * wm_interm_23_pt4(a, j) * wm_interm_39_pt4(b, j, p, i)
term(200) = term(200) + s2(a,b,p,i) * wm_interm_1_pt4(a, j) * wm_interm_26_pt4(b, j, q, i)
term(201) = term(201) + s2(a,b,i,p) * wm_interm_1_pt4(a, j) * wm_interm_26_pt4(b, j, q, i)
term(202) = term(202) + s2(a,b,p,i) * wm_interm_26_pt4(b, j, q, i) * wm_interm_2_pt4(a, j)
term(203) = term(203) + s2(a,b,i,p) * wm_interm_26_pt4(b, j, q, i) * wm_interm_2_pt4(a, j)
term(204) = term(204) + s2(a,b,p,i) * wm_interm_1_pt4(a, j) * wm_interm_26_pt4(b, j, i, q)
term(205) = term(205) + s2(a,b,p,i) * wm_interm_26_pt4(b, j, i, q) * wm_interm_2_pt4(a, j)
term(206) = term(206) + s2(a,b,i,p) * wm_interm_18_pt4(a, j, q, i) * wm_interm_48_pt4(b, j)
term(207) = term(207) + s2(a,b,i,p) * wm_interm_18_pt4(a, j, q, i) * wm_interm_47_pt4(b, j)
term(208) = term(208) + s2(a,b,p,i) * wm_interm_18_pt4(a, j, q, i) * wm_interm_48_pt4(b, j)
term(209) = term(209) + s2(a,b,p,i) * wm_interm_18_pt4(a, j, q, i) * wm_interm_47_pt4(b, j)
term(210) = term(210) + s2(a,b,p,i) * wm_interm_18_pt4(a, j, i, q) * wm_interm_48_pt4(b, j)
term(211) = term(211) + s2(a,b,p,i) * wm_interm_18_pt4(a, j, i, q) * wm_interm_47_pt4(b, j)
term(212) = term(212) + t2(a,b,q,i) * wm_interm_0_pt4(b, j) * wm_interm_20_pt4(a, j, i, p)
term(213) = term(213) + t2(a,b,q,i) * wm_interm_20_pt4(a, j, i, p) * wm_interm_3_pt4(b, j)
term(214) = term(214) + t2(a,b,q,i) * wm_interm_0_pt4(a, j) * wm_interm_20_pt4(b, j, p, i)
term(215) = term(215) + t2(a,b,q,i) * wm_interm_0_pt4(a, j) * wm_interm_20_pt4(b, j, i, p)
term(216) = term(216) + t2(a,b,q,i) * wm_interm_20_pt4(b, j, p, i) * wm_interm_3_pt4(a, j)
term(217) = term(217) + t2(a,b,q,i) * wm_interm_20_pt4(b, j, i, p) * wm_interm_3_pt4(a, j)
term(218) = term(218) + t2(a,b,q,i) * wm_interm_0_pt4(b, j) * wm_interm_20_pt4(a, j, p, i)
term(219) = term(219) + t2(a,b,q,i) * wm_interm_20_pt4(a, j, p, i) * wm_interm_3_pt4(b, j)
end do 
end do 
end do 
end do 

term(194) = term(194) * 4.0d+0 
term(195) = term(195) * 4.0d+0 
term(196) = term(196) * (-8.0d+0) 
term(197) = term(197) * (-8.0d+0) 
term(198) = term(198) * (-8.0d+0) 
term(199) = term(199) * 16.0d+0 
term(200) = term(200) * 8.0d+0 
term(201) = term(201) * (-4.0d+0) 
term(202) = term(202) * (-4.0d+0) 
term(203) = term(203) * 2.0d+0 
term(204) = term(204) * (-4.0d+0) 
term(205) = term(205) * 2.0d+0 
term(206) = term(206) * (-8.0d+0) 
term(207) = term(207) * 16.0d+0 
term(208) = term(208) * 4.0d+0 
term(209) = term(209) * (-8.0d+0) 
term(210) = term(210) * (-8.0d+0) 
term(211) = term(211) * 16.0d+0 
term(212) = term(212) * (-8.0d+0) 
term(213) = term(213) * 4.0d+0 
term(214) = term(214) * (-8.0d+0) 
term(215) = term(215) * 4.0d+0 
term(216) = term(216) * 4.0d+0 
term(217) = term(217) * (-2.0d+0) 
term(218) = term(218) * 4.0d+0 
term(219) = term(219) * (-2.0d+0) 

do b = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(220) = term(220) + s2(a,b,i,q) * wm_interm_17_pt4(a, j) * wm_interm_39_pt4(b, j, i, p)
term(221) = term(221) + s2(a,b,i,q) * wm_interm_23_pt4(a, j) * wm_interm_39_pt4(b, j, i, p)
term(222) = term(222) + s2(a,b,i,p) * wm_interm_1_pt4(a, j) * wm_interm_26_pt4(b, j, i, q)
term(223) = term(223) + s2(a,b,i,p) * wm_interm_26_pt4(b, j, i, q) * wm_interm_2_pt4(a, j)
term(224) = term(224) + s2(a,b,i,p) * wm_interm_18_pt4(a, j, i, q) * wm_interm_48_pt4(b, j)
term(225) = term(225) + s2(a,b,i,p) * wm_interm_18_pt4(a, j, i, q) * wm_interm_47_pt4(b, j)
end do 
end do 
end do 
end do 

term(220) = term(220) * (-8.0d+0) 
term(221) = term(221) * 16.0d+0 
term(222) = term(222) * 8.0d+0 
term(223) = term(223) * (-4.0d+0) 
term(224) = term(224) * 4.0d+0 
term(225) = term(225) * (-8.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(226) = term(226) + s1(a,i) * wm_interm_24_pt4(j, q, k, i) * wm_interm_39_pt4(a, k, p, j)
term(227) = term(227) + s1(a,i) * wm_interm_24_pt4(q, j, k, i) * wm_interm_39_pt4(a, k, p, j)
term(228) = term(228) + s1(a,i) * wm_interm_24_pt4(j, k, q, i) * wm_interm_39_pt4(a, p, k, j)
end do 
end do 
end do 
end do 

term(226) = term(226) * 4.0d+0 
term(227) = term(227) * (-8.0d+0) 
term(228) = term(228) * 2.0d+0 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(229) = term(229) + s1(a,i) * wm_interm_24_pt4(j, q, k, i) * wm_interm_39_pt4(a, k, j, p)
term(230) = term(230) + s1(a,i) * wm_interm_24_pt4(q, j, k, i) * wm_interm_39_pt4(a, k, j, p)
term(231) = term(231) + s1(a,p) * wm_interm_24_pt4(i, j, k, q) * wm_interm_39_pt4(a, k, j, i)
end do 
end do 
end do 
end do 

term(229) = term(229) * (-8.0d+0) 
term(230) = term(230) * 4.0d+0 
term(231) = term(231) * 4.0d+0 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(232) = term(232) + s2(a,b,q,i) * wm_interm_22_pt4(b, j) * wm_interm_46_pt4(a, p, i, j)
term(233) = term(233) + s2(a,b,q,i) * wm_interm_22_pt4(b, j) * wm_interm_46_pt4(a, i, p, j)
term(234) = term(234) + s2(a,b,q,i) * wm_interm_22_pt4(b, j) * wm_interm_9_pt4(a, i, p, j)
term(235) = term(235) + s2(a,b,q,i) * wm_interm_22_pt4(b, j) * wm_interm_9_pt4(a, p, i, j)
term(236) = term(236) + s2(a,b,q,i) * wm_interm_27_pt4(b, j) * wm_interm_9_pt4(a, i, p, j)
term(237) = term(237) + s2(a,b,q,i) * wm_interm_27_pt4(b, j) * wm_interm_9_pt4(a, p, i, j)
term(238) = term(238) + s2(a,b,q,i) * wm_interm_27_pt4(b, j) * wm_interm_46_pt4(a, p, i, j)
term(239) = term(239) + s2(a,b,q,i) * wm_interm_27_pt4(b, j) * wm_interm_46_pt4(a, i, p, j)
term(240) = term(240) + s2(a,b,i,j) * wm_interm_1_pt4(a, j) * wm_interm_26_pt4(b, p, q, i)
term(241) = term(241) + s2(a,b,i,j) * wm_interm_26_pt4(b, p, q, i) * wm_interm_2_pt4(a, j)
term(242) = term(242) + s2(a,b,i,j) * wm_interm_18_pt4(a, p, q, i) * wm_interm_48_pt4(b, j)
term(243) = term(243) + s2(a,b,i,j) * wm_interm_18_pt4(a, p, q, i) * wm_interm_47_pt4(b, j)
term(244) = term(244) + t2(a,b,i,j) * wm_interm_0_pt4(b, j) * wm_interm_20_pt4(a, p, q, i)
term(245) = term(245) + t2(a,b,i,j) * wm_interm_20_pt4(a, p, q, i) * wm_interm_3_pt4(b, j)
term(246) = term(246) + t2(a,b,q,i) * wm_interm_37_pt4(b, i, p, j) * wm_interm_53_pt4(a, j)
term(247) = term(247) + t2(a,b,q,i) * wm_interm_37_pt4(b, p, i, j) * wm_interm_53_pt4(a, j)
term(248) = term(248) + t2(a,b,q,i) * wm_interm_37_pt4(a, p, i, j) * wm_interm_53_pt4(b, j)
term(249) = term(249) + t2(a,b,q,i) * wm_interm_37_pt4(a, i, p, j) * wm_interm_53_pt4(b, j)
term(250) = term(250) + t2(a,b,q,i) * wm_interm_37_pt4(a, p, i, j) * wm_interm_50_pt4(b, j)
term(251) = term(251) + t2(a,b,q,i) * wm_interm_37_pt4(a, i, p, j) * wm_interm_50_pt4(b, j)
term(252) = term(252) + t2(a,b,q,i) * wm_interm_37_pt4(b, i, p, j) * wm_interm_50_pt4(a, j)
term(253) = term(253) + t2(a,b,q,i) * wm_interm_37_pt4(b, p, i, j) * wm_interm_50_pt4(a, j)
term(254) = term(254) + t2(a,b,q,i) * wm_interm_19_pt4(b, j) * wm_interm_8_pt4(a, i, p, j)
term(255) = term(255) + t2(a,b,q,i) * wm_interm_19_pt4(b, j) * wm_interm_8_pt4(a, p, i, j)
term(256) = term(256) + t2(a,b,q,i) * wm_interm_21_pt4(b, j) * wm_interm_8_pt4(a, i, p, j)
term(257) = term(257) + t2(a,b,q,i) * wm_interm_21_pt4(b, j) * wm_interm_8_pt4(a, p, i, j)
term(258) = term(258) + t2(a,b,q,i) * wm_interm_19_pt4(a, j) * wm_interm_8_pt4(b, p, i, j)
term(259) = term(259) + t2(a,b,q,i) * wm_interm_21_pt4(a, j) * wm_interm_8_pt4(b, p, i, j)
term(260) = term(260) + t2(a,b,q,i) * wm_interm_19_pt4(a, j) * wm_interm_8_pt4(b, i, p, j)
term(261) = term(261) + t2(a,b,q,i) * wm_interm_21_pt4(a, j) * wm_interm_8_pt4(b, i, p, j)
term(262) = term(262) + t2(a,b,q,i) * wm_interm_13_pt4(b, p, i, j) * wm_interm_19_pt4(a, j)
term(263) = term(263) + t2(a,b,q,i) * wm_interm_13_pt4(b, i, p, j) * wm_interm_19_pt4(a, j)
term(264) = term(264) + t2(a,b,q,i) * wm_interm_13_pt4(a, p, i, j) * wm_interm_19_pt4(b, j)
term(265) = term(265) + t2(a,b,q,i) * wm_interm_13_pt4(a, i, p, j) * wm_interm_19_pt4(b, j)
term(266) = term(266) + t2(a,b,q,i) * wm_interm_13_pt4(b, i, p, j) * wm_interm_21_pt4(a, j)
term(267) = term(267) + t2(a,b,q,i) * wm_interm_13_pt4(b, p, i, j) * wm_interm_21_pt4(a, j)
term(268) = term(268) + t2(a,b,q,i) * wm_interm_13_pt4(a, p, i, j) * wm_interm_21_pt4(b, j)
term(269) = term(269) + t2(a,b,q,i) * wm_interm_13_pt4(a, i, p, j) * wm_interm_21_pt4(b, j)
end do 
end do 
end do 
end do 

term(232) = term(232) * 2.0d+0 
term(233) = term(233) * (-4.0d+0) 
term(234) = term(234) * 2.0d+0 
term(235) = term(235) * (-4.0d+0) 
term(236) = term(236) * (-4.0d+0) 
term(237) = term(237) * 8.0d+0 
term(238) = term(238) * (-4.0d+0) 
term(239) = term(239) * 8.0d+0 
term(240) = term(240) * 8.0d+0 
term(241) = term(241) * (-4.0d+0) 
term(242) = term(242) * 16.0d+0 
term(243) = term(243) * (-32.0d+0) 
term(244) = term(244) * 32.0d+0 
term(245) = term(245) * (-16.0d+0) 
term(246) = term(246) * (-2.0d+0) 
term(247) = term(247) * 4.0d+0 
term(248) = term(248) * (-2.0d+0) 
term(249) = term(249) * 4.0d+0 
term(250) = term(250) * 4.0d+0 
term(251) = term(251) * (-8.0d+0) 
term(252) = term(252) * 4.0d+0 
term(253) = term(253) * (-8.0d+0) 
term(254) = -term(254) 
term(255) = term(255) * 2.0d+0 
term(256) = term(256) * 2.0d+0 
term(257) = term(257) * (-4.0d+0) 
term(258) = -term(258) 
term(259) = term(259) * 2.0d+0 
term(260) = term(260) * 2.0d+0 
term(261) = term(261) * (-4.0d+0) 
term(262) = term(262) * 2.0d+0 
term(263) = -term(263) 
term(264) = -term(264) 
term(265) = term(265) * 2.0d+0 
term(266) = term(266) * 2.0d+0 
term(267) = term(267) * (-4.0d+0) 
term(268) = term(268) * 2.0d+0 
term(269) = term(269) * (-4.0d+0) 

do l = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(270) = term(270) + s2(a,b,i,j) * wm_interm_18_pt4(a, k, j, l) * wm_interm_39_pt4(b, l, k, i)
term(271) = term(271) + s2(a,b,i,j) * wm_interm_26_pt4(b, k, l, i) * wm_interm_9_pt4(a, k, j, l)
term(272) = term(272) + s2(a,b,i,j) * wm_interm_26_pt4(b, k, l, i) * wm_interm_46_pt4(a, k, j, l)
term(273) = term(273) + t2(a,b,i,j) * wm_interm_20_pt4(a, k, l, i) * wm_interm_8_pt4(b, k, j, l)
term(274) = term(274) + t2(a,b,i,j) * wm_interm_20_pt4(a, k, l, i) * wm_interm_8_pt4(b, j, k, l)
term(275) = term(275) + t2(a,b,i,j) * wm_interm_13_pt4(b, k, j, l) * wm_interm_20_pt4(a, k, l, i)
term(276) = term(276) + t2(a,b,i,j) * wm_interm_13_pt4(b, j, k, l) * wm_interm_20_pt4(a, k, l, i)
end do 
end do 
end do 
end do 
end do 
end do 

term(270) = term(270) * (-8.0d+0) 
term(271) = term(271) * (-8.0d+0) 
term(272) = term(272) * 16.0d+0 
term(273) = term(273) * (-8.0d+0) 
term(274) = term(274) * 16.0d+0 
term(275) = term(275) * 16.0d+0 
term(276) = term(276) * (-8.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(277) = term(277) + s1(a,q) * wm_interm_26_pt4(a, i, j, k) * wm_interm_42_pt4(i, p, k, j)
term(278) = term(278) + s1(a,q) * wm_interm_26_pt4(a, i, j, k) * wm_interm_42_pt4(i, p, j, k)
term(279) = term(279) + s1(a,i) * wm_interm_26_pt4(a, q, j, k) * wm_interm_42_pt4(p, i, k, j)
term(280) = term(280) + s1(a,i) * wm_interm_26_pt4(a, q, j, k) * wm_interm_42_pt4(i, p, k, j)
term(281) = term(281) + s1(a,i) * wm_interm_26_pt4(a, q, j, k) * wm_interm_42_pt4(i, p, j, k)
term(282) = term(282) + s1(a,i) * wm_interm_26_pt4(a, q, j, k) * wm_interm_42_pt4(p, i, j, k)
term(283) = term(283) + s1(a,i) * wm_interm_26_pt4(a, j, k, q) * wm_interm_42_pt4(j, i, p, k)
term(284) = term(284) + s1(a,i) * wm_interm_26_pt4(a, j, k, q) * wm_interm_42_pt4(j, i, k, p)
term(285) = term(285) + s1(a,i) * wm_interm_26_pt4(a, j, q, k) * wm_interm_42_pt4(j, i, k, p)
term(286) = term(286) + s1(a,i) * wm_interm_26_pt4(a, j, q, k) * wm_interm_42_pt4(j, i, p, k)
term(287) = term(287) + s1(a,i) * wm_interm_24_pt4(j, k, i, q) * wm_interm_39_pt4(a, p, j, k)
term(288) = term(288) + t1(a,i) * wm_interm_20_pt4(a, p, j, k) * wm_interm_54_pt4(q, i, j, k)
term(289) = term(289) + t1(a,i) * wm_interm_20_pt4(a, p, j, k) * wm_interm_54_pt4(q, i, k, j)
term(290) = term(290) + t1(a,i) * wm_interm_24_pt4(i, q, j, k) * wm_interm_49_pt4(a, j, k, p)
term(291) = term(291) + t1(a,i) * wm_interm_24_pt4(q, i, j, k) * wm_interm_49_pt4(a, j, k, p)
term(292) = term(292) + t1(a,i) * wm_interm_20_pt4(a, p, j, k) * wm_interm_54_pt4(i, q, k, j)
term(293) = term(293) + t1(a,i) * wm_interm_20_pt4(a, p, j, k) * wm_interm_54_pt4(i, q, j, k)
term(294) = term(294) + t1(a,i) * wm_interm_24_pt4(i, q, j, k) * wm_interm_51_pt4(a, j, k, p)
term(295) = term(295) + t1(a,i) * wm_interm_24_pt4(q, i, j, k) * wm_interm_51_pt4(a, j, k, p)
term(296) = term(296) + t1(a,q) * wm_interm_20_pt4(a, i, j, k) * wm_interm_54_pt4(p, i, j, k)
term(297) = term(297) + t1(a,q) * wm_interm_20_pt4(a, i, j, k) * wm_interm_54_pt4(i, p, j, k)
term(298) = term(298) + t1(a,q) * wm_interm_20_pt4(a, i, j, k) * wm_interm_54_pt4(i, p, k, j)
term(299) = term(299) + t1(a,q) * wm_interm_20_pt4(a, i, j, k) * wm_interm_54_pt4(p, i, k, j)
term(300) = term(300) + t1(a,i) * wm_interm_20_pt4(a, j, p, k) * wm_interm_54_pt4(j, i, q, k)
term(301) = term(301) + t1(a,i) * wm_interm_20_pt4(a, j, k, p) * wm_interm_54_pt4(j, i, q, k)
term(302) = term(302) + t1(a,i) * wm_interm_20_pt4(a, j, p, k) * wm_interm_54_pt4(j, i, k, q)
term(303) = term(303) + t1(a,i) * wm_interm_20_pt4(a, j, k, p) * wm_interm_54_pt4(j, i, k, q)
term(304) = term(304) + t1(a,i) * wm_interm_20_pt4(a, j, p, k) * wm_interm_54_pt4(i, j, k, q)
term(305) = term(305) + t1(a,i) * wm_interm_20_pt4(a, j, p, k) * wm_interm_54_pt4(i, j, q, k)
term(306) = term(306) + t1(a,i) * wm_interm_20_pt4(a, j, k, p) * wm_interm_54_pt4(i, j, q, k)
term(307) = term(307) + t1(a,i) * wm_interm_20_pt4(a, j, k, p) * wm_interm_54_pt4(i, j, k, q)
end do 
end do 
end do 
end do 

term(277) = term(277) * 4.0d+0 
term(278) = term(278) * (-8.0d+0) 
term(279) = term(279) * 2.0d+0 
term(280) = term(280) * (-4.0d+0) 
term(281) = term(281) * 2.0d+0 
term(282) = term(282) * (-4.0d+0) 
term(283) = term(283) * 4.0d+0 
term(284) = term(284) * (-8.0d+0) 
term(285) = term(285) * 4.0d+0 
term(286) = term(286) * (-8.0d+0) 
term(287) = term(287) * 2.0d+0 
term(288) = term(288) * 4.0d+0 
term(289) = term(289) * (-2.0d+0) 
term(290) = term(290) * (-2.0d+0) 
term(291) = term(291) * 4.0d+0 
term(292) = term(292) * 4.0d+0 
term(293) = term(293) * (-2.0d+0) 
term(294) = term(294) * 4.0d+0 
term(295) = term(295) * (-2.0d+0) 
term(296) = -term(296) 
term(297) = term(297) * 2.0d+0 
term(298) = -term(298) 
term(299) = term(299) * 2.0d+0 
term(300) = term(300) * 2.0d+0 
term(301) = -term(301) 
term(302) = -term(302) 
term(303) = term(303) * 2.0d+0 
term(304) = term(304) * 2.0d+0 
term(305) = -term(305) 
term(306) = term(306) * 2.0d+0 
term(307) = -term(307) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(308) = term(308) + s2(a,b,q,i) * wm_interm_1_pt4(a, p) * wm_interm_22_pt4(b, i)
term(309) = term(309) + s2(a,b,q,i) * wm_interm_22_pt4(b, i) * wm_interm_2_pt4(a, p)
term(310) = term(310) + s2(a,b,q,i) * wm_interm_1_pt4(a, p) * wm_interm_27_pt4(b, i)
term(311) = term(311) + s2(a,b,q,i) * wm_interm_27_pt4(b, i) * wm_interm_2_pt4(a, p)
term(312) = term(312) + s2(a,b,p,i) * wm_interm_1_pt4(b, i) * wm_interm_27_pt4(a, q)
term(313) = term(313) + s2(a,b,p,i) * wm_interm_27_pt4(a, q) * wm_interm_2_pt4(b, i)
term(314) = term(314) + s2(a,b,p,i) * wm_interm_1_pt4(b, i) * wm_interm_22_pt4(a, q)
term(315) = term(315) + s2(a,b,p,i) * wm_interm_22_pt4(a, q) * wm_interm_2_pt4(b, i)
term(316) = term(316) + s2(a,b,p,i) * wm_interm_17_pt4(a, q) * wm_interm_48_pt4(b, i)
term(317) = term(317) + s2(a,b,p,i) * wm_interm_17_pt4(a, q) * wm_interm_47_pt4(b, i)
term(318) = term(318) + r1(vrdav_Rl, a,i) * wm_interm_18_pt4(b, p, q, i) * wm_interm_40_pt4(a, b)
term(319) = term(319) + r1(vrdav_Rl, a,i) * wm_interm_18_pt4(b, p, q, i) * wm_interm_41_pt4(a, b)
term(320) = term(320) + r1(vrdav_Rl, a,i) * wm_interm_18_pt4(b, p, i, q) * wm_interm_40_pt4(a, b)
term(321) = term(321) + r1(vrdav_Rl, a,i) * wm_interm_18_pt4(b, p, i, q) * wm_interm_41_pt4(a, b)
term(322) = term(322) + s2(a,b,p,i) * wm_interm_23_pt4(a, q) * wm_interm_48_pt4(b, i)
term(323) = term(323) + s2(a,b,p,i) * wm_interm_23_pt4(a, q) * wm_interm_47_pt4(b, i)
term(324) = term(324) + s1(a,i) * wm_interm_26_pt4(b, p, i, q) * wm_interm_40_pt4(a, b)
term(325) = term(325) + s1(a,i) * wm_interm_26_pt4(b, p, i, q) * wm_interm_41_pt4(a, b)
term(326) = term(326) + s1(a,i) * wm_interm_26_pt4(b, p, q, i) * wm_interm_40_pt4(a, b)
term(327) = term(327) + s1(a,i) * wm_interm_26_pt4(b, p, q, i) * wm_interm_41_pt4(a, b)
term(328) = term(328) + t1(a,i) * wm_interm_31_pt4(a, b) * wm_interm_49_pt4(b, q, i, p)
term(329) = term(329) + t1(a,i) * wm_interm_33_pt4(a, b) * wm_interm_49_pt4(b, q, i, p)
term(330) = term(330) + t1(a,i) * wm_interm_33_pt4(a, b) * wm_interm_51_pt4(b, q, i, p)
term(331) = term(331) + t1(a,i) * wm_interm_31_pt4(a, b) * wm_interm_51_pt4(b, q, i, p)
term(332) = term(332) + t1(a,i) * wm_interm_20_pt4(b, p, i, q) * wm_interm_56_pt4(a, b)
term(333) = term(333) + t1(a,i) * wm_interm_20_pt4(b, p, q, i) * wm_interm_56_pt4(a, b)
term(334) = term(334) + t1(a,i) * wm_interm_20_pt4(b, p, i, q) * wm_interm_57_pt4(a, b)
term(335) = term(335) + t1(a,i) * wm_interm_20_pt4(b, p, q, i) * wm_interm_57_pt4(a, b)
term(336) = term(336) + r1(vrdav_Rr, a,i) * wm_interm_13_pt4(b, p, i, q) * wm_interm_31_pt4(a, b)
term(337) = term(337) + r1(vrdav_Rr, a,i) * wm_interm_13_pt4(b, p, i, q) * wm_interm_33_pt4(a, b)
term(338) = term(338) + r1(vrdav_Rr, a,i) * wm_interm_35_pt4(b, p, i, q) * wm_interm_56_pt4(a, b)
term(339) = term(339) + r1(vrdav_Rr, a,i) * wm_interm_35_pt4(b, p, i, q) * wm_interm_57_pt4(a, b)
term(340) = term(340) + r1(vrdav_Rr, a,i) * wm_interm_31_pt4(a, b) * wm_interm_8_pt4(b, p, i, q)
term(341) = term(341) + r1(vrdav_Rr, a,i) * wm_interm_33_pt4(a, b) * wm_interm_8_pt4(b, p, i, q)
term(342) = term(342) + t2(a,b,q,i) * wm_interm_0_pt4(b, i) * wm_interm_21_pt4(a, p)
term(343) = term(343) + t2(a,b,q,i) * wm_interm_21_pt4(a, p) * wm_interm_3_pt4(b, i)
term(344) = term(344) + t2(a,b,q,i) * wm_interm_0_pt4(b, i) * wm_interm_19_pt4(a, p)
term(345) = term(345) + t2(a,b,q,i) * wm_interm_19_pt4(a, p) * wm_interm_3_pt4(b, i)
term(346) = term(346) + t2(a,b,q,i) * wm_interm_0_pt4(a, p) * wm_interm_21_pt4(b, i)
term(347) = term(347) + t2(a,b,q,i) * wm_interm_21_pt4(b, i) * wm_interm_3_pt4(a, p)
term(348) = term(348) + t2(a,b,q,i) * wm_interm_0_pt4(a, p) * wm_interm_19_pt4(b, i)
term(349) = term(349) + t2(a,b,q,i) * wm_interm_19_pt4(b, i) * wm_interm_3_pt4(a, p)
end do 
end do 
end do 

term(308) = term(308) * 8.0d+0 
term(309) = term(309) * (-4.0d+0) 
term(310) = term(310) * (-16.0d+0) 
term(311) = term(311) * 8.0d+0 
term(312) = term(312) * (-16.0d+0) 
term(313) = term(313) * 8.0d+0 
term(314) = term(314) * 8.0d+0 
term(315) = term(315) * (-4.0d+0) 
term(316) = term(316) * (-8.0d+0) 
term(317) = term(317) * 16.0d+0 
term(318) = term(318) * (-8.0d+0) 
term(319) = term(319) * 16.0d+0 
term(320) = term(320) * 4.0d+0 
term(321) = term(321) * (-8.0d+0) 
term(322) = term(322) * 16.0d+0 
term(323) = term(323) * (-32.0d+0) 
term(324) = term(324) * 4.0d+0 
term(325) = term(325) * (-8.0d+0) 
term(326) = term(326) * (-8.0d+0) 
term(327) = term(327) * 16.0d+0 
term(328) = term(328) * (-8.0d+0) 
term(329) = term(329) * 4.0d+0 
term(330) = term(330) * (-2.0d+0) 
term(331) = term(331) * 4.0d+0 
term(332) = term(332) * (-4.0d+0) 
term(333) = term(333) * 8.0d+0 
term(334) = term(334) * 8.0d+0 
term(335) = term(335) * (-16.0d+0) 
term(336) = term(336) * (-4.0d+0) 
term(337) = term(337) * 2.0d+0 
term(338) = term(338) * (-2.0d+0) 
term(339) = term(339) * 4.0d+0 
term(340) = term(340) * 2.0d+0 
term(341) = -term(341) 
term(342) = term(342) * 16.0d+0 
term(343) = term(343) * (-8.0d+0) 
term(344) = term(344) * (-8.0d+0) 
term(345) = term(345) * 4.0d+0 
term(346) = term(346) * 16.0d+0 
term(347) = term(347) * (-8.0d+0) 
term(348) = term(348) * (-8.0d+0) 
term(349) = term(349) * 4.0d+0 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do l = 1, nocc 
do a = nocc + 1, nactive 
term(350) = term(350) + s2(a,b,i,j) * wm_interm_18_pt4(a, k, l, i) * wm_interm_39_pt4(b, l, j, k)
end do 
end do 
end do 
end do 
end do 
end do 

term(350) = term(350) * 16.0d+0 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do l = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(351) = term(351) + s2(a,b,i,j) * wm_interm_18_pt4(a, k, l, i) * wm_interm_39_pt4(b, l, k, j)
term(352) = term(352) + s2(a,b,i,j) * wm_interm_18_pt4(a, k, l, j) * wm_interm_39_pt4(b, l, k, i)
end do 
end do 
end do 
end do 
end do 
end do 

term(351) = term(351) * (-32.0d+0) 
term(352) = term(352) * 16.0d+0 

do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do l = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(353) = term(353) + s2(a,b,i,j) * wm_interm_18_pt4(a, k, l, j) * wm_interm_39_pt4(b, l, i, k)
end do 
end do 
end do 
end do 
end do 
end do 

term(353) = term(353) * (-8.0d+0) 

do l = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(354) = term(354) + t1(a,i) * wm_interm_24_pt4(i, j, k, l) * wm_interm_49_pt4(a, k, l, j)
term(355) = term(355) + t1(a,i) * wm_interm_24_pt4(j, i, k, l) * wm_interm_49_pt4(a, k, l, j)
term(356) = term(356) + t1(a,i) * wm_interm_24_pt4(i, j, k, l) * wm_interm_51_pt4(a, k, l, j)
term(357) = term(357) + t1(a,i) * wm_interm_24_pt4(j, i, k, l) * wm_interm_51_pt4(a, k, l, j)
end do 
end do 
end do 
end do 
end do 

term(354) = term(354) * 4.0d+0 
term(355) = term(355) * (-8.0d+0) 
term(356) = term(356) * (-8.0d+0) 
term(357) = term(357) * 4.0d+0 

do l = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(358) = term(358) + s2(a,b,i,j) * wm_interm_18_pt4(a, k, j, l) * wm_interm_39_pt4(b, l, i, k)
term(359) = term(359) + s2(a,b,i,j) * wm_interm_18_pt4(a, k, i, l) * wm_interm_39_pt4(b, l, j, k)
term(360) = term(360) + t2(a,b,i,j) * wm_interm_35_pt4(a, k, j, l) * wm_interm_51_pt4(b, i, l, k)
term(361) = term(361) + t2(a,b,i,j) * wm_interm_35_pt4(a, j, k, l) * wm_interm_51_pt4(b, i, l, k)
term(362) = term(362) + t2(a,b,i,j) * wm_interm_37_pt4(a, i, k, l) * wm_interm_49_pt4(b, j, l, k)
term(363) = term(363) + t2(a,b,i,j) * wm_interm_37_pt4(a, k, i, l) * wm_interm_49_pt4(b, j, l, k)
term(364) = term(364) + t2(a,b,i,j) * wm_interm_35_pt4(a, j, k, l) * wm_interm_49_pt4(b, i, l, k)
term(365) = term(365) + t2(a,b,i,j) * wm_interm_35_pt4(a, k, j, l) * wm_interm_49_pt4(b, i, l, k)
term(366) = term(366) + t2(a,b,i,j) * wm_interm_37_pt4(a, i, k, l) * wm_interm_51_pt4(b, j, l, k)
term(367) = term(367) + t2(a,b,i,j) * wm_interm_37_pt4(a, k, i, l) * wm_interm_51_pt4(b, j, l, k)
term(368) = term(368) + t2(a,b,i,j) * wm_interm_37_pt4(a, j, k, l) * wm_interm_49_pt4(b, l, i, k)
term(369) = term(369) + t2(a,b,i,j) * wm_interm_37_pt4(a, k, j, l) * wm_interm_49_pt4(b, l, i, k)
term(370) = term(370) + t2(a,b,i,j) * wm_interm_37_pt4(a, k, i, l) * wm_interm_49_pt4(b, l, j, k)
term(371) = term(371) + t2(a,b,i,j) * wm_interm_37_pt4(a, i, k, l) * wm_interm_49_pt4(b, l, j, k)
term(372) = term(372) + t2(a,b,i,j) * wm_interm_37_pt4(a, i, k, l) * wm_interm_51_pt4(b, l, j, k)
term(373) = term(373) + t2(a,b,i,j) * wm_interm_37_pt4(a, k, i, l) * wm_interm_51_pt4(b, l, j, k)
term(374) = term(374) + t2(a,b,i,j) * wm_interm_37_pt4(a, j, k, l) * wm_interm_51_pt4(b, l, i, k)
term(375) = term(375) + t2(a,b,i,j) * wm_interm_37_pt4(a, k, j, l) * wm_interm_51_pt4(b, l, i, k)
end do 
end do 
end do 
end do 
end do 
end do 

term(358) = term(358) * 16.0d+0 
term(359) = term(359) * (-8.0d+0) 
term(360) = term(360) * 4.0d+0 
term(361) = term(361) * (-8.0d+0) 
term(362) = term(362) * 4.0d+0 
term(363) = term(363) * (-8.0d+0) 
term(364) = term(364) * 4.0d+0 
term(365) = term(365) * (-8.0d+0) 
term(366) = term(366) * (-8.0d+0) 
term(367) = term(367) * 16.0d+0 
term(368) = term(368) * 4.0d+0 
term(369) = term(369) * (-8.0d+0) 
term(370) = term(370) * 16.0d+0 
term(371) = term(371) * (-8.0d+0) 
term(372) = term(372) * 4.0d+0 
term(373) = term(373) * (-8.0d+0) 
term(374) = term(374) * (-8.0d+0) 
term(375) = term(375) * 4.0d+0 

do l = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(376) = term(376) + s2(a,b,i,j) * wm_interm_18_pt4(a, k, i, l) * wm_interm_39_pt4(b, l, k, j)
term(377) = term(377) + t2(a,b,i,j) * wm_interm_20_pt4(a, k, l, j) * wm_interm_8_pt4(b, i, k, l)
term(378) = term(378) + t2(a,b,i,j) * wm_interm_20_pt4(a, k, j, l) * wm_interm_8_pt4(b, i, k, l)
term(379) = term(379) + t2(a,b,i,j) * wm_interm_20_pt4(a, k, i, l) * wm_interm_8_pt4(b, j, k, l)
term(380) = term(380) + t2(a,b,i,j) * wm_interm_13_pt4(b, i, k, l) * wm_interm_20_pt4(a, k, j, l)
term(381) = term(381) + t2(a,b,i,j) * wm_interm_13_pt4(b, j, k, l) * wm_interm_20_pt4(a, k, i, l)
term(382) = term(382) + t2(a,b,i,j) * wm_interm_13_pt4(b, i, k, l) * wm_interm_20_pt4(a, k, l, j)
end do 
end do 
end do 
end do 
end do 
end do 

term(376) = term(376) * 16.0d+0 
term(377) = term(377) * (-8.0d+0) 
term(378) = term(378) * 4.0d+0 
term(379) = term(379) * (-8.0d+0) 
term(380) = term(380) * (-8.0d+0) 
term(381) = term(381) * 4.0d+0 
term(382) = term(382) * 4.0d+0 

do l = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(383) = term(383) + s2(a,b,i,j) * wm_interm_26_pt4(b, k, l, j) * wm_interm_9_pt4(a, k, i, l)
term(384) = term(384) + s2(a,b,i,j) * wm_interm_26_pt4(b, k, l, j) * wm_interm_46_pt4(a, k, i, l)
term(385) = term(385) + s2(a,b,i,j) * wm_interm_26_pt4(b, k, i, l) * wm_interm_46_pt4(a, k, j, l)
term(386) = term(386) + s2(a,b,i,j) * wm_interm_26_pt4(b, k, i, l) * wm_interm_9_pt4(a, k, j, l)
term(387) = term(387) + s2(a,b,i,j) * wm_interm_26_pt4(b, k, j, l) * wm_interm_9_pt4(a, k, i, l)
term(388) = term(388) + s2(a,b,i,j) * wm_interm_26_pt4(b, k, j, l) * wm_interm_46_pt4(a, k, i, l)
term(389) = term(389) + t2(a,b,i,j) * wm_interm_20_pt4(a, k, l, j) * wm_interm_8_pt4(b, k, i, l)
term(390) = term(390) + t2(a,b,i,j) * wm_interm_20_pt4(a, k, j, l) * wm_interm_8_pt4(b, k, i, l)
term(391) = term(391) + t2(a,b,i,j) * wm_interm_20_pt4(a, k, i, l) * wm_interm_8_pt4(b, k, j, l)
term(392) = term(392) + t2(a,b,i,j) * wm_interm_13_pt4(b, k, i, l) * wm_interm_20_pt4(a, k, j, l)
term(393) = term(393) + t2(a,b,i,j) * wm_interm_13_pt4(b, k, i, l) * wm_interm_20_pt4(a, k, l, j)
term(394) = term(394) + t2(a,b,i,j) * wm_interm_13_pt4(b, k, j, l) * wm_interm_20_pt4(a, k, i, l)
end do 
end do 
end do 
end do 
end do 
end do 

term(383) = term(383) * 16.0d+0 
term(384) = term(384) * (-32.0d+0) 
term(385) = term(385) * (-8.0d+0) 
term(386) = term(386) * 16.0d+0 
term(387) = term(387) * (-8.0d+0) 
term(388) = term(388) * 16.0d+0 
term(389) = term(389) * 4.0d+0 
term(390) = term(390) * (-8.0d+0) 
term(391) = term(391) * 4.0d+0 
term(392) = term(392) * 4.0d+0 
term(393) = term(393) * (-8.0d+0) 
term(394) = term(394) * (-8.0d+0) 

do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(395) = term(395) + t1(b,q) * wm_interm_31_pt4(b, a) * wm_interm_53_pt4(a, p)
term(396) = term(396) + t1(b,q) * wm_interm_33_pt4(b, a) * wm_interm_53_pt4(a, p)
term(397) = term(397) + t1(b,q) * wm_interm_31_pt4(b, a) * wm_interm_50_pt4(a, p)
term(398) = term(398) + t1(b,q) * wm_interm_33_pt4(b, a) * wm_interm_50_pt4(a, p)
term(399) = term(399) + t1(b,q) * wm_interm_19_pt4(a, p) * wm_interm_56_pt4(b, a)
term(400) = term(400) + t1(b,q) * wm_interm_19_pt4(a, p) * wm_interm_57_pt4(b, a)
term(401) = term(401) + t1(b,q) * wm_interm_21_pt4(a, p) * wm_interm_56_pt4(b, a)
term(402) = term(402) + t1(b,q) * wm_interm_21_pt4(a, p) * wm_interm_57_pt4(b, a)
end do 
end do 

term(395) = term(395) * 4.0d+0 
term(396) = term(396) * (-2.0d+0) 
term(397) = term(397) * (-8.0d+0) 
term(398) = term(398) * 4.0d+0 
term(399) = term(399) * (-2.0d+0) 
term(400) = term(400) * 4.0d+0 
term(401) = term(401) * 4.0d+0 
term(402) = term(402) * (-8.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(403) = term(403) + s2(a,b,i,j) * wm_interm_18_pt4(a, q, j, k) * wm_interm_39_pt4(b, k, i, p)
term(404) = term(404) + s2(a,b,i,q) * wm_interm_18_pt4(a, j, i, k) * wm_interm_39_pt4(b, k, p, j)
term(405) = term(405) + s2(a,b,i,j) * wm_interm_18_pt4(a, q, i, k) * wm_interm_39_pt4(b, k, j, p)
term(406) = term(406) + s2(a,b,i,j) * wm_interm_18_pt4(a, q, i, k) * wm_interm_39_pt4(b, k, p, j)
term(407) = term(407) + s2(a,b,i,j) * wm_interm_26_pt4(b, q, k, j) * wm_interm_9_pt4(a, p, i, k)
term(408) = term(408) + s2(a,b,i,j) * wm_interm_26_pt4(b, q, k, j) * wm_interm_9_pt4(a, i, p, k)
term(409) = term(409) + s2(a,b,i,j) * wm_interm_26_pt4(b, q, k, j) * wm_interm_46_pt4(a, i, p, k)
term(410) = term(410) + s2(a,b,i,j) * wm_interm_26_pt4(b, q, k, j) * wm_interm_46_pt4(a, p, i, k)
term(411) = term(411) + s2(a,b,i,j) * wm_interm_26_pt4(b, q, i, k) * wm_interm_46_pt4(a, p, j, k)
term(412) = term(412) + s2(a,b,i,j) * wm_interm_26_pt4(b, q, i, k) * wm_interm_46_pt4(a, j, p, k)
term(413) = term(413) + s2(a,b,i,j) * wm_interm_26_pt4(b, q, j, k) * wm_interm_46_pt4(a, i, p, k)
term(414) = term(414) + s2(a,b,i,j) * wm_interm_26_pt4(b, q, j, k) * wm_interm_46_pt4(a, p, i, k)
term(415) = term(415) + s2(a,b,i,j) * wm_interm_26_pt4(b, q, j, k) * wm_interm_9_pt4(a, p, i, k)
term(416) = term(416) + s2(a,b,i,j) * wm_interm_26_pt4(b, q, j, k) * wm_interm_9_pt4(a, i, p, k)
term(417) = term(417) + s2(a,b,i,j) * wm_interm_26_pt4(b, q, i, k) * wm_interm_9_pt4(a, j, p, k)
term(418) = term(418) + s2(a,b,i,j) * wm_interm_26_pt4(b, q, i, k) * wm_interm_9_pt4(a, p, j, k)
term(419) = term(419) + s2(a,b,i,p) * wm_interm_18_pt4(a, j, q, k) * wm_interm_39_pt4(b, k, i, j)
term(420) = term(420) + s2(a,b,i,j) * wm_interm_18_pt4(a, k, q, j) * wm_interm_39_pt4(b, p, i, k)
term(421) = term(421) + s2(a,b,i,j) * wm_interm_18_pt4(a, k, j, q) * wm_interm_39_pt4(b, p, i, k)
term(422) = term(422) + s2(a,b,i,j) * wm_interm_18_pt4(a, k, i, q) * wm_interm_39_pt4(b, p, j, k)
term(423) = term(423) + t2(a,b,i,j) * wm_interm_35_pt4(a, q, j, k) * wm_interm_51_pt4(b, i, k, p)
term(424) = term(424) + t2(a,b,i,j) * wm_interm_35_pt4(a, j, q, k) * wm_interm_51_pt4(b, i, k, p)
term(425) = term(425) + t2(a,b,i,j) * wm_interm_37_pt4(a, i, q, k) * wm_interm_49_pt4(b, j, k, p)
term(426) = term(426) + t2(a,b,i,j) * wm_interm_37_pt4(a, q, i, k) * wm_interm_49_pt4(b, j, k, p)
term(427) = term(427) + t2(a,b,i,j) * wm_interm_35_pt4(a, j, q, k) * wm_interm_49_pt4(b, i, k, p)
term(428) = term(428) + t2(a,b,i,j) * wm_interm_35_pt4(a, q, j, k) * wm_interm_49_pt4(b, i, k, p)
term(429) = term(429) + t2(a,b,i,j) * wm_interm_37_pt4(a, i, q, k) * wm_interm_51_pt4(b, j, k, p)
term(430) = term(430) + t2(a,b,i,j) * wm_interm_37_pt4(a, q, i, k) * wm_interm_51_pt4(b, j, k, p)
term(431) = term(431) + t2(a,b,i,j) * wm_interm_20_pt4(a, p, k, j) * wm_interm_8_pt4(b, i, q, k)
term(432) = term(432) + t2(a,b,i,j) * wm_interm_20_pt4(a, p, j, k) * wm_interm_8_pt4(b, i, q, k)
term(433) = term(433) + t2(a,b,i,j) * wm_interm_20_pt4(a, p, k, j) * wm_interm_8_pt4(b, q, i, k)
term(434) = term(434) + t2(a,b,i,j) * wm_interm_20_pt4(a, p, j, k) * wm_interm_8_pt4(b, q, i, k)
term(435) = term(435) + t2(a,b,i,j) * wm_interm_20_pt4(a, p, i, k) * wm_interm_8_pt4(b, q, j, k)
term(436) = term(436) + t2(a,b,i,j) * wm_interm_20_pt4(a, p, i, k) * wm_interm_8_pt4(b, j, q, k)
term(437) = term(437) + t2(a,b,i,j) * wm_interm_37_pt4(a, j, q, k) * wm_interm_49_pt4(b, k, i, p)
term(438) = term(438) + t2(a,b,i,j) * wm_interm_37_pt4(a, q, j, k) * wm_interm_49_pt4(b, k, i, p)
term(439) = term(439) + t2(a,b,i,j) * wm_interm_37_pt4(a, q, i, k) * wm_interm_49_pt4(b, k, j, p)
term(440) = term(440) + t2(a,b,i,j) * wm_interm_37_pt4(a, i, q, k) * wm_interm_49_pt4(b, k, j, p)
term(441) = term(441) + t2(a,b,i,j) * wm_interm_37_pt4(a, i, q, k) * wm_interm_51_pt4(b, k, j, p)
term(442) = term(442) + t2(a,b,i,j) * wm_interm_37_pt4(a, q, i, k) * wm_interm_51_pt4(b, k, j, p)
term(443) = term(443) + t2(a,b,i,j) * wm_interm_37_pt4(a, j, q, k) * wm_interm_51_pt4(b, k, i, p)
term(444) = term(444) + t2(a,b,i,j) * wm_interm_37_pt4(a, q, j, k) * wm_interm_51_pt4(b, k, i, p)
term(445) = term(445) + t2(a,b,i,j) * wm_interm_13_pt4(b, q, i, k) * wm_interm_20_pt4(a, p, j, k)
term(446) = term(446) + t2(a,b,i,j) * wm_interm_13_pt4(b, q, i, k) * wm_interm_20_pt4(a, p, k, j)
term(447) = term(447) + t2(a,b,i,j) * wm_interm_13_pt4(b, q, j, k) * wm_interm_20_pt4(a, p, i, k)
term(448) = term(448) + t2(a,b,i,j) * wm_interm_13_pt4(b, i, q, k) * wm_interm_20_pt4(a, p, j, k)
term(449) = term(449) + t2(a,b,i,j) * wm_interm_13_pt4(b, j, q, k) * wm_interm_20_pt4(a, p, i, k)
term(450) = term(450) + t2(a,b,i,j) * wm_interm_13_pt4(b, i, q, k) * wm_interm_20_pt4(a, p, k, j)
term(451) = term(451) + t2(a,b,i,j) * wm_interm_35_pt4(a, k, j, q) * wm_interm_49_pt4(b, p, i, k)
term(452) = term(452) + t2(a,b,i,j) * wm_interm_35_pt4(a, j, k, q) * wm_interm_49_pt4(b, p, i, k)
term(453) = term(453) + t2(a,b,i,j) * wm_interm_35_pt4(a, i, k, q) * wm_interm_49_pt4(b, p, j, k)
term(454) = term(454) + t2(a,b,i,j) * wm_interm_35_pt4(a, k, i, q) * wm_interm_49_pt4(b, p, j, k)
term(455) = term(455) + t2(a,b,i,j) * wm_interm_35_pt4(a, k, j, q) * wm_interm_51_pt4(b, i, p, k)
term(456) = term(456) + t2(a,b,i,j) * wm_interm_35_pt4(a, k, j, q) * wm_interm_49_pt4(b, i, p, k)
term(457) = term(457) + t2(a,b,i,j) * wm_interm_35_pt4(a, j, k, q) * wm_interm_51_pt4(b, i, p, k)
term(458) = term(458) + t2(a,b,i,j) * wm_interm_35_pt4(a, j, k, q) * wm_interm_49_pt4(b, i, p, k)
term(459) = term(459) + t2(a,b,i,j) * wm_interm_35_pt4(a, i, k, q) * wm_interm_49_pt4(b, j, p, k)
term(460) = term(460) + t2(a,b,i,j) * wm_interm_35_pt4(a, k, i, q) * wm_interm_49_pt4(b, j, p, k)
term(461) = term(461) + t2(a,b,i,j) * wm_interm_35_pt4(a, k, j, q) * wm_interm_51_pt4(b, p, i, k)
term(462) = term(462) + t2(a,b,i,j) * wm_interm_35_pt4(a, j, k, q) * wm_interm_51_pt4(b, p, i, k)
term(463) = term(463) + t2(a,b,i,j) * wm_interm_35_pt4(a, i, k, q) * wm_interm_51_pt4(b, p, j, k)
term(464) = term(464) + t2(a,b,i,j) * wm_interm_35_pt4(a, i, k, q) * wm_interm_51_pt4(b, j, p, k)
term(465) = term(465) + t2(a,b,i,j) * wm_interm_35_pt4(a, k, i, q) * wm_interm_51_pt4(b, p, j, k)
term(466) = term(466) + t2(a,b,i,j) * wm_interm_35_pt4(a, k, i, q) * wm_interm_51_pt4(b, j, p, k)
end do 
end do 
end do 
end do 
end do 

term(403) = term(403) * (-8.0d+0) 
term(404) = term(404) * 4.0d+0 
term(405) = term(405) * 4.0d+0 
term(406) = term(406) * (-8.0d+0) 
term(407) = term(407) * (-4.0d+0) 
term(408) = term(408) * 8.0d+0 
term(409) = term(409) * (-4.0d+0) 
term(410) = term(410) * 8.0d+0 
term(411) = term(411) * 2.0d+0 
term(412) = term(412) * (-4.0d+0) 
term(413) = term(413) * 2.0d+0 
term(414) = term(414) * (-4.0d+0) 
term(415) = term(415) * 2.0d+0 
term(416) = term(416) * (-4.0d+0) 
term(417) = term(417) * 2.0d+0 
term(418) = term(418) * (-4.0d+0) 
term(419) = term(419) * (-8.0d+0) 
term(420) = term(420) * 4.0d+0 
term(421) = term(421) * (-8.0d+0) 
term(422) = term(422) * 4.0d+0 
term(423) = term(423) * (-2.0d+0) 
term(424) = term(424) * 4.0d+0 
term(425) = term(425) * (-2.0d+0) 
term(426) = term(426) * 4.0d+0 
term(427) = term(427) * (-2.0d+0) 
term(428) = term(428) * 4.0d+0 
term(429) = term(429) * 4.0d+0 
term(430) = term(430) * (-8.0d+0) 
term(431) = term(431) * 4.0d+0 
term(432) = term(432) * (-2.0d+0) 
term(433) = term(433) * (-2.0d+0) 
term(434) = term(434) * 4.0d+0 
term(435) = term(435) * (-2.0d+0) 
term(436) = term(436) * 4.0d+0 
term(437) = term(437) * (-2.0d+0) 
term(438) = term(438) * 4.0d+0 
term(439) = term(439) * (-8.0d+0) 
term(440) = term(440) * 4.0d+0 
term(441) = term(441) * (-2.0d+0) 
term(442) = term(442) * 4.0d+0 
term(443) = term(443) * 4.0d+0 
term(444) = term(444) * (-2.0d+0) 
term(445) = term(445) * (-2.0d+0) 
term(446) = term(446) * 4.0d+0 
term(447) = term(447) * 4.0d+0 
term(448) = term(448) * 4.0d+0 
term(449) = term(449) * (-2.0d+0) 
term(450) = term(450) * (-2.0d+0) 
term(451) = -term(451) 
term(452) = term(452) * 2.0d+0 
term(453) = term(453) * (-4.0d+0) 
term(454) = term(454) * 2.0d+0 
term(455) = -term(455) 
term(456) = term(456) * 2.0d+0 
term(457) = term(457) * 2.0d+0 
term(458) = -term(458) 
term(459) = term(459) * 2.0d+0 
term(460) = -term(460) 
term(461) = term(461) * 2.0d+0 
term(462) = -term(462) 
term(463) = term(463) * 2.0d+0 
term(464) = term(464) * (-4.0d+0) 
term(465) = -term(465) 
term(466) = term(466) * 2.0d+0 

do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(467) = term(467) + s2(a,b,i,j) * wm_interm_26_pt4(b, k, i, q) * wm_interm_46_pt4(a, k, j, p)
term(468) = term(468) + s2(a,b,i,j) * wm_interm_26_pt4(b, k, i, q) * wm_interm_9_pt4(a, k, j, p)
term(469) = term(469) + s2(a,b,i,j) * wm_interm_26_pt4(b, k, j, q) * wm_interm_9_pt4(a, k, i, p)
term(470) = term(470) + s2(a,b,i,j) * wm_interm_26_pt4(b, k, j, q) * wm_interm_46_pt4(a, k, i, p)
term(471) = term(471) + s2(a,b,i,j) * wm_interm_26_pt4(b, k, q, j) * wm_interm_9_pt4(a, k, i, p)
term(472) = term(472) + s2(a,b,i,j) * wm_interm_26_pt4(b, k, q, j) * wm_interm_46_pt4(a, k, i, p)
term(473) = term(473) + t2(a,b,i,j) * wm_interm_13_pt4(b, k, i, q) * wm_interm_20_pt4(a, k, j, p)
term(474) = term(474) + t2(a,b,i,j) * wm_interm_13_pt4(b, k, i, q) * wm_interm_20_pt4(a, k, p, j)
term(475) = term(475) + t2(a,b,i,j) * wm_interm_13_pt4(b, k, j, q) * wm_interm_20_pt4(a, k, i, p)
term(476) = term(476) + t2(a,b,i,j) * wm_interm_20_pt4(a, k, j, p) * wm_interm_8_pt4(b, k, i, q)
term(477) = term(477) + t2(a,b,i,j) * wm_interm_20_pt4(a, k, p, j) * wm_interm_8_pt4(b, k, i, q)
term(478) = term(478) + t2(a,b,i,j) * wm_interm_20_pt4(a, k, i, p) * wm_interm_8_pt4(b, k, j, q)
end do 
end do 
end do 
end do 
end do 

term(467) = term(467) * 4.0d+0 
term(468) = term(468) * (-8.0d+0) 
term(469) = term(469) * 4.0d+0 
term(470) = term(470) * (-8.0d+0) 
term(471) = term(471) * (-8.0d+0) 
term(472) = term(472) * 16.0d+0 
term(473) = -term(473) 
term(474) = term(474) * 2.0d+0 
term(475) = term(475) * 2.0d+0 
term(476) = term(476) * 2.0d+0 
term(477) = -term(477) 
term(478) = -term(478) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(479) = term(479) + s2(a,b,i,q) * wm_interm_18_pt4(a, j, k, i) * wm_interm_39_pt4(b, k, p, j)
term(480) = term(480) + s2(a,b,i,j) * wm_interm_18_pt4(a, q, k, i) * wm_interm_39_pt4(b, k, j, p)
term(481) = term(481) + s2(a,b,i,j) * wm_interm_18_pt4(a, q, k, i) * wm_interm_39_pt4(b, k, p, j)
term(482) = term(482) + s2(a,b,q,i) * wm_interm_18_pt4(a, j, k, i) * wm_interm_39_pt4(b, k, p, j)
term(483) = term(483) + s2(a,b,i,j) * wm_interm_18_pt4(a, q, k, j) * wm_interm_39_pt4(b, k, p, i)
term(484) = term(484) + s2(a,b,i,j) * wm_interm_18_pt4(a, k, q, i) * wm_interm_39_pt4(b, p, k, j)
term(485) = term(485) + s2(a,b,i,j) * wm_interm_18_pt4(a, k, q, j) * wm_interm_39_pt4(b, p, k, i)
term(486) = term(486) + s2(a,b,i,j) * wm_interm_26_pt4(b, k, q, i) * wm_interm_9_pt4(a, k, j, p)
term(487) = term(487) + s2(a,b,i,j) * wm_interm_26_pt4(b, k, q, i) * wm_interm_46_pt4(a, k, j, p)
term(488) = term(488) + s2(a,b,i,j) * wm_interm_18_pt4(a, k, j, q) * wm_interm_39_pt4(b, p, k, i)
term(489) = term(489) + s2(a,b,p,i) * wm_interm_18_pt4(a, j, k, q) * wm_interm_39_pt4(b, k, i, j)
term(490) = term(490) + t2(a,b,i,j) * wm_interm_13_pt4(b, k, j, q) * wm_interm_20_pt4(a, k, p, i)
term(491) = term(491) + t2(a,b,i,j) * wm_interm_13_pt4(b, j, k, q) * wm_interm_20_pt4(a, k, p, i)
term(492) = term(492) + t2(a,b,i,j) * wm_interm_20_pt4(a, k, p, i) * wm_interm_8_pt4(b, k, j, q)
term(493) = term(493) + t2(a,b,i,j) * wm_interm_20_pt4(a, k, p, i) * wm_interm_8_pt4(b, j, k, q)
end do 
end do 
end do 
end do 
end do 

term(479) = term(479) * (-8.0d+0) 
term(480) = term(480) * (-8.0d+0) 
term(481) = term(481) * 16.0d+0 
term(482) = term(482) * 4.0d+0 
term(483) = term(483) * (-8.0d+0) 
term(484) = term(484) * 16.0d+0 
term(485) = term(485) * (-8.0d+0) 
term(486) = term(486) * 4.0d+0 
term(487) = term(487) * (-8.0d+0) 
term(488) = term(488) * 4.0d+0 
term(489) = term(489) * (-8.0d+0) 
term(490) = term(490) * (-4.0d+0) 
term(491) = term(491) * 2.0d+0 
term(492) = term(492) * 2.0d+0 
term(493) = term(493) * (-4.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(494) = term(494) + s2(a,b,i,q) * wm_interm_18_pt4(a, j, k, i) * wm_interm_39_pt4(b, k, j, p)
term(495) = term(495) + s2(a,b,q,i) * wm_interm_18_pt4(a, j, k, i) * wm_interm_39_pt4(b, k, j, p)
term(496) = term(496) + s2(a,b,p,i) * wm_interm_18_pt4(a, j, k, q) * wm_interm_39_pt4(b, k, j, i)
term(497) = term(497) + s2(a,b,i,p) * wm_interm_18_pt4(a, j, k, q) * wm_interm_39_pt4(b, k, j, i)
end do 
end do 
end do 
end do 
end do 

term(494) = term(494) * 16.0d+0 
term(495) = term(495) * (-8.0d+0) 
term(496) = term(496) * 16.0d+0 
term(497) = term(497) * (-8.0d+0) 

do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(498) = term(498) + s2(a,b,i,q) * wm_interm_1_pt4(a, p) * wm_interm_22_pt4(b, i)
term(499) = term(499) + s2(a,b,i,q) * wm_interm_22_pt4(b, i) * wm_interm_2_pt4(a, p)
term(500) = term(500) + s2(a,b,i,q) * wm_interm_1_pt4(a, p) * wm_interm_27_pt4(b, i)
term(501) = term(501) + s2(a,b,i,q) * wm_interm_27_pt4(b, i) * wm_interm_2_pt4(a, p)
term(502) = term(502) + r1(vrdav_Rl, a,i) * wm_interm_17_pt4(b, i) * wm_interm_40_pt4(a, b)
term(503) = term(503) + r1(vrdav_Rl, a,i) * wm_interm_17_pt4(b, i) * wm_interm_41_pt4(a, b)
term(504) = term(504) + r1(vrdav_Rl, a,i) * wm_interm_23_pt4(b, i) * wm_interm_40_pt4(a, b)
term(505) = term(505) + r1(vrdav_Rl, a,i) * wm_interm_23_pt4(b, i) * wm_interm_41_pt4(a, b)
term(506) = term(506) + s1(a,i) * wm_interm_22_pt4(b, i) * wm_interm_40_pt4(a, b)
term(507) = term(507) + s1(a,i) * wm_interm_22_pt4(b, i) * wm_interm_41_pt4(a, b)
term(508) = term(508) + s1(a,i) * wm_interm_27_pt4(b, i) * wm_interm_40_pt4(a, b)
term(509) = term(509) + s1(a,i) * wm_interm_27_pt4(b, i) * wm_interm_41_pt4(a, b)
term(510) = term(510) + s2(a,b,i,p) * wm_interm_1_pt4(b, i) * wm_interm_22_pt4(a, q)
term(511) = term(511) + s2(a,b,i,p) * wm_interm_22_pt4(a, q) * wm_interm_2_pt4(b, i)
term(512) = term(512) + s2(a,b,i,p) * wm_interm_1_pt4(b, i) * wm_interm_27_pt4(a, q)
term(513) = term(513) + s2(a,b,i,p) * wm_interm_27_pt4(a, q) * wm_interm_2_pt4(b, i)
term(514) = term(514) + s2(a,b,i,p) * wm_interm_17_pt4(a, q) * wm_interm_48_pt4(b, i)
term(515) = term(515) + s2(a,b,i,p) * wm_interm_17_pt4(a, q) * wm_interm_47_pt4(b, i)
term(516) = term(516) + s2(a,b,i,p) * wm_interm_23_pt4(a, q) * wm_interm_48_pt4(b, i)
term(517) = term(517) + s2(a,b,i,p) * wm_interm_23_pt4(a, q) * wm_interm_47_pt4(b, i)
term(518) = term(518) + t1(a,i) * wm_interm_31_pt4(a, b) * wm_interm_50_pt4(b, i)
term(519) = term(519) + t1(a,i) * wm_interm_33_pt4(a, b) * wm_interm_50_pt4(b, i)
term(520) = term(520) + t1(a,i) * wm_interm_33_pt4(a, b) * wm_interm_53_pt4(b, i)
term(521) = term(521) + t1(a,i) * wm_interm_31_pt4(a, b) * wm_interm_53_pt4(b, i)
term(522) = term(522) + t1(a,i) * wm_interm_19_pt4(b, i) * wm_interm_56_pt4(a, b)
term(523) = term(523) + t1(a,i) * wm_interm_21_pt4(b, i) * wm_interm_56_pt4(a, b)
term(524) = term(524) + t1(a,i) * wm_interm_19_pt4(b, i) * wm_interm_57_pt4(a, b)
term(525) = term(525) + t1(a,i) * wm_interm_21_pt4(b, i) * wm_interm_57_pt4(a, b)
term(526) = term(526) + r1(vrdav_Rr, a,i) * wm_interm_0_pt4(b, i) * wm_interm_31_pt4(a, b)
term(527) = term(527) + r1(vrdav_Rr, a,i) * wm_interm_0_pt4(b, i) * wm_interm_33_pt4(a, b)
term(528) = term(528) + t1(a,i) * wm_interm_31_pt4(a, b) * wm_interm_49_pt4(b, i, q, p)
term(529) = term(529) + t1(a,i) * wm_interm_33_pt4(a, b) * wm_interm_49_pt4(b, i, q, p)
term(530) = term(530) + t1(a,i) * wm_interm_31_pt4(a, b) * wm_interm_51_pt4(b, i, q, p)
term(531) = term(531) + r1(vrdav_Rr, a,i) * wm_interm_31_pt4(a, b) * wm_interm_3_pt4(b, i)
term(532) = term(532) + r1(vrdav_Rr, a,i) * wm_interm_33_pt4(a, b) * wm_interm_3_pt4(b, i)
term(533) = term(533) + t1(a,i) * wm_interm_33_pt4(a, b) * wm_interm_51_pt4(b, i, q, p)
term(534) = term(534) + r1(vrdav_Rr, a,i) * wm_interm_13_pt4(b, i, p, q) * wm_interm_31_pt4(a, b)
term(535) = term(535) + r1(vrdav_Rr, a,i) * wm_interm_13_pt4(b, i, p, q) * wm_interm_33_pt4(a, b)
term(536) = term(536) + r1(vrdav_Rr, a,i) * wm_interm_35_pt4(b, i, p, q) * wm_interm_56_pt4(a, b)
term(537) = term(537) + r1(vrdav_Rr, a,i) * wm_interm_35_pt4(b, i, p, q) * wm_interm_57_pt4(a, b)
term(538) = term(538) + r1(vrdav_Rr, a,i) * wm_interm_31_pt4(a, b) * wm_interm_8_pt4(b, i, p, q)
term(539) = term(539) + r1(vrdav_Rr, a,i) * wm_interm_33_pt4(a, b) * wm_interm_8_pt4(b, i, p, q)
term(540) = term(540) + t2(a,b,i,q) * wm_interm_0_pt4(a, p) * wm_interm_19_pt4(b, i)
term(541) = term(541) + t2(a,b,i,q) * wm_interm_0_pt4(a, p) * wm_interm_21_pt4(b, i)
term(542) = term(542) + t2(a,b,i,q) * wm_interm_19_pt4(b, i) * wm_interm_3_pt4(a, p)
term(543) = term(543) + t2(a,b,i,q) * wm_interm_21_pt4(b, i) * wm_interm_3_pt4(a, p)
term(544) = term(544) + t2(a,b,i,q) * wm_interm_0_pt4(b, i) * wm_interm_19_pt4(a, p)
term(545) = term(545) + t2(a,b,i,q) * wm_interm_0_pt4(b, i) * wm_interm_21_pt4(a, p)
term(546) = term(546) + t2(a,b,i,q) * wm_interm_19_pt4(a, p) * wm_interm_3_pt4(b, i)
term(547) = term(547) + t2(a,b,i,q) * wm_interm_21_pt4(a, p) * wm_interm_3_pt4(b, i)
end do 
end do 
end do 

term(498) = term(498) * (-4.0d+0) 
term(499) = term(499) * 2.0d+0 
term(500) = term(500) * 8.0d+0 
term(501) = term(501) * (-4.0d+0) 
term(502) = term(502) * (-8.0d+0) 
term(503) = term(503) * 16.0d+0 
term(504) = term(504) * 16.0d+0 
term(505) = term(505) * (-32.0d+0) 
term(506) = term(506) * (-8.0d+0) 
term(507) = term(507) * 16.0d+0 
term(508) = term(508) * 16.0d+0 
term(509) = term(509) * (-32.0d+0) 
term(510) = term(510) * (-4.0d+0) 
term(511) = term(511) * 2.0d+0 
term(512) = term(512) * 8.0d+0 
term(513) = term(513) * (-4.0d+0) 
term(514) = term(514) * 4.0d+0 
term(515) = term(515) * (-8.0d+0) 
term(516) = term(516) * (-8.0d+0) 
term(517) = term(517) * 16.0d+0 
term(518) = term(518) * 32.0d+0 
term(519) = term(519) * (-16.0d+0) 
term(520) = term(520) * 8.0d+0 
term(521) = term(521) * (-16.0d+0) 
term(522) = term(522) * 8.0d+0 
term(523) = term(523) * (-16.0d+0) 
term(524) = term(524) * (-16.0d+0) 
term(525) = term(525) * 32.0d+0 
term(526) = term(526) * 32.0d+0 
term(527) = term(527) * (-16.0d+0) 
term(528) = term(528) * 4.0d+0 
term(529) = term(529) * (-2.0d+0) 
term(530) = term(530) * (-8.0d+0) 
term(531) = term(531) * (-16.0d+0) 
term(532) = term(532) * 8.0d+0 
term(533) = term(533) * 4.0d+0 
term(534) = term(534) * 2.0d+0 
term(535) = -term(535) 
term(536) = term(536) * 4.0d+0 
term(537) = term(537) * (-8.0d+0) 
term(538) = term(538) * (-4.0d+0) 
term(539) = term(539) * 2.0d+0 
term(540) = term(540) * 4.0d+0 
term(541) = term(541) * (-8.0d+0) 
term(542) = term(542) * (-2.0d+0) 
term(543) = term(543) * 4.0d+0 
term(544) = term(544) * 4.0d+0 
term(545) = term(545) * (-8.0d+0) 
term(546) = term(546) * (-2.0d+0) 
term(547) = term(547) * 4.0d+0 

term(548) = term(548) + wm_interm_14_pt4 * wm_interm_5_pt4(p, q)
term(549) = term(549) + wm_interm_15_pt4 * wm_interm_5_pt4(p, q)
term(550) = term(550) + wm_interm_38_pt4 * wm_interm_5_pt4(p, q)

term(548) = term(548) * 2.0d+0 
term(549) = term(549) * (-4.0d+0) 
term(550) = term(550) * (-4.0d+0) 

do j = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(551) = term(551) + s2(a,b,i,j) * wm_interm_18_pt4(a, q, k, j) * wm_interm_39_pt4(b, k, i, p)
term(552) = term(552) + s2(a,b,i,j) * wm_interm_18_pt4(a, k, i, q) * wm_interm_39_pt4(b, p, k, j)
term(553) = term(553) + s2(a,b,i,p) * wm_interm_18_pt4(a, j, k, q) * wm_interm_39_pt4(b, k, i, j)
term(554) = term(554) + t2(a,b,i,j) * wm_interm_13_pt4(b, i, k, q) * wm_interm_20_pt4(a, k, j, p)
term(555) = term(555) + t2(a,b,i,j) * wm_interm_20_pt4(a, k, j, p) * wm_interm_8_pt4(b, i, k, q)
term(556) = term(556) + t2(a,b,i,j) * wm_interm_20_pt4(a, k, p, j) * wm_interm_8_pt4(b, i, k, q)
term(557) = term(557) + t2(a,b,i,j) * wm_interm_13_pt4(b, i, k, q) * wm_interm_20_pt4(a, k, p, j)
term(558) = term(558) + t2(a,b,i,j) * wm_interm_13_pt4(b, j, k, q) * wm_interm_20_pt4(a, k, i, p)
term(559) = term(559) + t2(a,b,i,j) * wm_interm_20_pt4(a, k, i, p) * wm_interm_8_pt4(b, j, k, q)
end do 
end do 
end do 
end do 
end do 

term(551) = term(551) * 4.0d+0 
term(552) = term(552) * (-8.0d+0) 
term(553) = term(553) * 4.0d+0 
term(554) = term(554) * 2.0d+0 
term(555) = -term(555) 
term(556) = term(556) * 2.0d+0 
term(557) = -term(557) 
term(558) = -term(558) 
term(559) = term(559) * 2.0d+0 

do a = nocc + 1, nactive 
term(560) = term(560) + wm_interm_0_pt4(a, q) * wm_interm_1_pt4(a, p)
term(561) = term(561) + wm_interm_0_pt4(a, q) * wm_interm_2_pt4(a, p)
term(562) = term(562) + wm_interm_1_pt4(a, p) * wm_interm_3_pt4(a, q)
term(563) = term(563) + wm_interm_2_pt4(a, p) * wm_interm_3_pt4(a, q)
end do 

term(560) = term(560) * 8.0d+0 
term(561) = term(561) * (-4.0d+0) 
term(562) = term(562) * (-4.0d+0) 
term(563) = term(563) * 2.0d+0 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(564) = term(564) + s2(a,b,i,j) * wm_interm_18_pt4(a, q, j, k) * wm_interm_39_pt4(b, k, p, i)
term(565) = term(565) + s2(a,b,q,i) * wm_interm_18_pt4(a, j, i, k) * wm_interm_39_pt4(b, k, p, j)
term(566) = term(566) + s2(a,b,i,j) * wm_interm_26_pt4(b, q, k, i) * wm_interm_9_pt4(a, p, j, k)
term(567) = term(567) + s2(a,b,i,j) * wm_interm_26_pt4(b, q, k, i) * wm_interm_9_pt4(a, j, p, k)
term(568) = term(568) + s2(a,b,i,j) * wm_interm_26_pt4(b, q, k, i) * wm_interm_46_pt4(a, j, p, k)
term(569) = term(569) + s2(a,b,i,j) * wm_interm_26_pt4(b, q, k, i) * wm_interm_46_pt4(a, p, j, k)
term(570) = term(570) + s2(a,b,p,i) * wm_interm_18_pt4(a, j, q, k) * wm_interm_39_pt4(b, k, i, j)
term(571) = term(571) + s2(a,b,i,j) * wm_interm_18_pt4(a, k, q, i) * wm_interm_39_pt4(b, p, j, k)
term(572) = term(572) + t2(a,b,i,j) * wm_interm_20_pt4(a, p, k, i) * wm_interm_8_pt4(b, q, j, k)
term(573) = term(573) + t2(a,b,i,j) * wm_interm_20_pt4(a, p, k, i) * wm_interm_8_pt4(b, j, q, k)
term(574) = term(574) + t2(a,b,i,j) * wm_interm_13_pt4(b, q, j, k) * wm_interm_20_pt4(a, p, k, i)
term(575) = term(575) + t2(a,b,i,j) * wm_interm_13_pt4(b, j, q, k) * wm_interm_20_pt4(a, p, k, i)
term(576) = term(576) + t2(a,b,q,i) * wm_interm_35_pt4(b, p, j, k) * wm_interm_51_pt4(a, i, k, j)
term(577) = term(577) + t2(a,b,q,i) * wm_interm_35_pt4(b, j, p, k) * wm_interm_51_pt4(a, i, k, j)
term(578) = term(578) + t2(a,b,q,i) * wm_interm_37_pt4(b, i, j, k) * wm_interm_51_pt4(a, p, k, j)
term(579) = term(579) + t2(a,b,q,i) * wm_interm_37_pt4(b, j, i, k) * wm_interm_51_pt4(a, p, k, j)
term(580) = term(580) + t2(a,b,q,i) * wm_interm_37_pt4(a, i, j, k) * wm_interm_51_pt4(b, p, k, j)
term(581) = term(581) + t2(a,b,q,i) * wm_interm_37_pt4(a, j, i, k) * wm_interm_51_pt4(b, p, k, j)
term(582) = term(582) + t2(a,b,q,i) * wm_interm_37_pt4(b, i, j, k) * wm_interm_49_pt4(a, p, k, j)
term(583) = term(583) + t2(a,b,q,i) * wm_interm_37_pt4(b, j, i, k) * wm_interm_49_pt4(a, p, k, j)
term(584) = term(584) + t2(a,b,q,i) * wm_interm_37_pt4(a, j, i, k) * wm_interm_49_pt4(b, p, k, j)
term(585) = term(585) + t2(a,b,q,i) * wm_interm_37_pt4(a, i, j, k) * wm_interm_49_pt4(b, p, k, j)
term(586) = term(586) + t2(a,b,q,i) * wm_interm_37_pt4(a, p, j, k) * wm_interm_51_pt4(b, i, k, j)
term(587) = term(587) + t2(a,b,q,i) * wm_interm_37_pt4(a, j, p, k) * wm_interm_51_pt4(b, i, k, j)
term(588) = term(588) + t2(a,b,q,i) * wm_interm_37_pt4(a, p, j, k) * wm_interm_49_pt4(b, i, k, j)
term(589) = term(589) + t2(a,b,q,i) * wm_interm_37_pt4(a, j, p, k) * wm_interm_49_pt4(b, i, k, j)
term(590) = term(590) + t2(a,b,q,i) * wm_interm_35_pt4(b, p, j, k) * wm_interm_49_pt4(a, i, k, j)
term(591) = term(591) + t2(a,b,q,i) * wm_interm_35_pt4(b, j, p, k) * wm_interm_49_pt4(a, i, k, j)
term(592) = term(592) + t2(a,b,q,i) * wm_interm_37_pt4(a, p, j, k) * wm_interm_51_pt4(b, k, i, j)
term(593) = term(593) + t2(a,b,q,i) * wm_interm_37_pt4(a, p, j, k) * wm_interm_49_pt4(b, k, i, j)
term(594) = term(594) + t2(a,b,q,i) * wm_interm_37_pt4(a, j, i, k) * wm_interm_51_pt4(b, k, p, j)
term(595) = term(595) + t2(a,b,q,i) * wm_interm_37_pt4(a, i, j, k) * wm_interm_51_pt4(b, k, p, j)
term(596) = term(596) + t2(a,b,q,i) * wm_interm_37_pt4(a, j, p, k) * wm_interm_51_pt4(b, k, i, j)
term(597) = term(597) + t2(a,b,q,i) * wm_interm_37_pt4(a, i, j, k) * wm_interm_49_pt4(b, k, p, j)
term(598) = term(598) + t2(a,b,q,i) * wm_interm_37_pt4(a, j, i, k) * wm_interm_49_pt4(b, k, p, j)
term(599) = term(599) + t2(a,b,q,i) * wm_interm_37_pt4(a, j, p, k) * wm_interm_49_pt4(b, k, i, j)
term(600) = term(600) + t2(a,b,q,i) * wm_interm_37_pt4(b, j, p, k) * wm_interm_51_pt4(a, k, i, j)
term(601) = term(601) + t2(a,b,q,i) * wm_interm_37_pt4(b, j, p, k) * wm_interm_49_pt4(a, k, i, j)
term(602) = term(602) + t2(a,b,q,i) * wm_interm_37_pt4(b, i, j, k) * wm_interm_51_pt4(a, k, p, j)
term(603) = term(603) + t2(a,b,q,i) * wm_interm_37_pt4(b, j, i, k) * wm_interm_51_pt4(a, k, p, j)
term(604) = term(604) + t2(a,b,q,i) * wm_interm_37_pt4(b, p, j, k) * wm_interm_51_pt4(a, k, i, j)
term(605) = term(605) + t2(a,b,q,i) * wm_interm_37_pt4(b, p, j, k) * wm_interm_49_pt4(a, k, i, j)
term(606) = term(606) + t2(a,b,q,i) * wm_interm_37_pt4(b, i, j, k) * wm_interm_49_pt4(a, k, p, j)
term(607) = term(607) + t2(a,b,q,i) * wm_interm_37_pt4(b, j, i, k) * wm_interm_49_pt4(a, k, p, j)
end do 
end do 
end do 
end do 
end do 

term(564) = term(564) * 4.0d+0 
term(565) = term(565) * (-8.0d+0) 
term(566) = term(566) * 2.0d+0 
term(567) = term(567) * (-4.0d+0) 
term(568) = term(568) * 2.0d+0 
term(569) = term(569) * (-4.0d+0) 
term(570) = term(570) * 4.0d+0 
term(571) = term(571) * (-8.0d+0) 
term(572) = term(572) * 4.0d+0 
term(573) = term(573) * (-8.0d+0) 
term(574) = term(574) * (-8.0d+0) 
term(575) = term(575) * 4.0d+0 
term(576) = term(576) * 2.0d+0 
term(577) = -term(577) 
term(578) = term(578) * 2.0d+0 
term(579) = term(579) * (-4.0d+0) 
term(580) = -term(580) 
term(581) = term(581) * 2.0d+0 
term(582) = -term(582) 
term(583) = term(583) * 2.0d+0 
term(584) = -term(584) 
term(585) = term(585) * 2.0d+0 
term(586) = term(586) * 2.0d+0 
term(587) = term(587) * (-4.0d+0) 
term(588) = -term(588) 
term(589) = term(589) * 2.0d+0 
term(590) = -term(590) 
term(591) = term(591) * 2.0d+0 
term(592) = -term(592) 
term(593) = term(593) * 2.0d+0 
term(594) = -term(594) 
term(595) = term(595) * 2.0d+0 
term(596) = term(596) * 2.0d+0 
term(597) = -term(597) 
term(598) = term(598) * 2.0d+0 
term(599) = term(599) * (-4.0d+0) 
term(600) = -term(600) 
term(601) = term(601) * 2.0d+0 
term(602) = -term(602) 
term(603) = term(603) * 2.0d+0 
term(604) = term(604) * 2.0d+0 
term(605) = -term(605) 
term(606) = term(606) * 2.0d+0 
term(607) = term(607) * (-4.0d+0) 

do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(608) = term(608) + s1(a,p) * wm_interm_24_pt4(i, j, k, q) * wm_interm_39_pt4(a, k, i, j)
term(609) = term(609) + s1(a,i) * wm_interm_24_pt4(j, k, i, q) * wm_interm_39_pt4(a, p, k, j)
term(610) = term(610) + t1(a,i) * wm_interm_24_pt4(j, i, k, q) * wm_interm_49_pt4(a, p, k, j)
term(611) = term(611) + t1(a,i) * wm_interm_24_pt4(i, j, k, q) * wm_interm_49_pt4(a, p, k, j)
term(612) = term(612) + t1(a,i) * wm_interm_24_pt4(i, j, k, q) * wm_interm_49_pt4(a, k, p, j)
term(613) = term(613) + t1(a,i) * wm_interm_24_pt4(j, i, k, q) * wm_interm_49_pt4(a, k, p, j)
term(614) = term(614) + t1(a,i) * wm_interm_24_pt4(i, j, k, q) * wm_interm_51_pt4(a, p, k, j)
term(615) = term(615) + t1(a,i) * wm_interm_24_pt4(i, j, k, q) * wm_interm_51_pt4(a, k, p, j)
term(616) = term(616) + t1(a,i) * wm_interm_24_pt4(j, i, k, q) * wm_interm_51_pt4(a, k, p, j)
term(617) = term(617) + t1(a,i) * wm_interm_24_pt4(j, i, k, q) * wm_interm_51_pt4(a, p, k, j)
end do 
end do 
end do 
end do 

term(608) = term(608) * (-8.0d+0) 
term(609) = term(609) * (-4.0d+0) 
term(610) = -term(610) 
term(611) = term(611) * 2.0d+0 
term(612) = -term(612) 
term(613) = term(613) * 2.0d+0 
term(614) = -term(614) 
term(615) = term(615) * 2.0d+0 
term(616) = -term(616) 
term(617) = term(617) * 2.0d+0 

do k = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(618) = term(618) + s2(a,b,q,i) * wm_interm_18_pt4(a, j, i, k) * wm_interm_39_pt4(b, k, j, p)
term(619) = term(619) + s2(a,b,q,i) * wm_interm_26_pt4(b, j, k, i) * wm_interm_9_pt4(a, j, p, k)
term(620) = term(620) + s2(a,b,q,i) * wm_interm_26_pt4(b, j, k, i) * wm_interm_46_pt4(a, j, p, k)
term(621) = term(621) + s2(a,b,i,q) * wm_interm_26_pt4(b, j, k, i) * wm_interm_9_pt4(a, j, p, k)
term(622) = term(622) + s2(a,b,i,q) * wm_interm_26_pt4(b, j, k, i) * wm_interm_46_pt4(a, j, p, k)
term(623) = term(623) + s2(a,b,q,i) * wm_interm_26_pt4(b, j, i, k) * wm_interm_9_pt4(a, j, p, k)
term(624) = term(624) + s2(a,b,q,i) * wm_interm_26_pt4(b, j, i, k) * wm_interm_46_pt4(a, j, p, k)
term(625) = term(625) + s2(a,b,i,p) * wm_interm_18_pt4(a, j, q, k) * wm_interm_39_pt4(b, k, j, i)
term(626) = term(626) + s2(a,b,p,i) * wm_interm_18_pt4(a, j, q, k) * wm_interm_39_pt4(b, k, j, i)
term(627) = term(627) + s2(a,b,p,i) * wm_interm_26_pt4(b, j, q, k) * wm_interm_46_pt4(a, j, i, k)
term(628) = term(628) + s2(a,b,p,i) * wm_interm_26_pt4(b, j, q, k) * wm_interm_9_pt4(a, j, i, k)
term(629) = term(629) + s2(a,b,p,i) * wm_interm_26_pt4(b, j, k, q) * wm_interm_9_pt4(a, j, i, k)
term(630) = term(630) + s2(a,b,p,i) * wm_interm_26_pt4(b, j, k, q) * wm_interm_46_pt4(a, j, i, k)
term(631) = term(631) + t2(a,b,q,i) * wm_interm_20_pt4(b, j, k, p) * wm_interm_8_pt4(a, j, i, k)
term(632) = term(632) + t2(a,b,q,i) * wm_interm_20_pt4(b, j, k, p) * wm_interm_8_pt4(a, i, j, k)
term(633) = term(633) + t2(a,b,q,i) * wm_interm_20_pt4(b, j, p, k) * wm_interm_8_pt4(a, i, j, k)
term(634) = term(634) + t2(a,b,q,i) * wm_interm_20_pt4(b, j, p, k) * wm_interm_8_pt4(a, j, i, k)
term(635) = term(635) + t2(a,b,q,i) * wm_interm_20_pt4(b, j, k, i) * wm_interm_8_pt4(a, j, p, k)
term(636) = term(636) + t2(a,b,q,i) * wm_interm_20_pt4(b, j, k, i) * wm_interm_8_pt4(a, p, j, k)
term(637) = term(637) + t2(a,b,q,i) * wm_interm_20_pt4(b, j, i, k) * wm_interm_8_pt4(a, j, p, k)
term(638) = term(638) + t2(a,b,q,i) * wm_interm_20_pt4(b, j, i, k) * wm_interm_8_pt4(a, p, j, k)
term(639) = term(639) + t2(a,b,q,i) * wm_interm_20_pt4(a, j, k, i) * wm_interm_8_pt4(b, p, j, k)
term(640) = term(640) + t2(a,b,q,i) * wm_interm_20_pt4(a, j, i, k) * wm_interm_8_pt4(b, p, j, k)
term(641) = term(641) + t2(a,b,q,i) * wm_interm_20_pt4(a, j, k, i) * wm_interm_8_pt4(b, j, p, k)
term(642) = term(642) + t2(a,b,q,i) * wm_interm_20_pt4(a, j, k, p) * wm_interm_8_pt4(b, j, i, k)
term(643) = term(643) + t2(a,b,q,i) * wm_interm_20_pt4(a, j, k, p) * wm_interm_8_pt4(b, i, j, k)
term(644) = term(644) + t2(a,b,q,i) * wm_interm_20_pt4(a, j, p, k) * wm_interm_8_pt4(b, j, i, k)
term(645) = term(645) + t2(a,b,q,i) * wm_interm_20_pt4(a, j, p, k) * wm_interm_8_pt4(b, i, j, k)
term(646) = term(646) + t2(a,b,q,i) * wm_interm_20_pt4(a, j, i, k) * wm_interm_8_pt4(b, j, p, k)
term(647) = term(647) + t2(a,b,q,i) * wm_interm_13_pt4(b, p, j, k) * wm_interm_20_pt4(a, j, k, i)
term(648) = term(648) + t2(a,b,q,i) * wm_interm_13_pt4(b, i, j, k) * wm_interm_20_pt4(a, j, k, p)
term(649) = term(649) + t2(a,b,q,i) * wm_interm_13_pt4(b, j, p, k) * wm_interm_20_pt4(a, j, k, i)
term(650) = term(650) + t2(a,b,q,i) * wm_interm_13_pt4(b, j, i, k) * wm_interm_20_pt4(a, j, k, p)
term(651) = term(651) + t2(a,b,q,i) * wm_interm_13_pt4(a, i, j, k) * wm_interm_20_pt4(b, j, k, p)
term(652) = term(652) + t2(a,b,q,i) * wm_interm_13_pt4(a, j, i, k) * wm_interm_20_pt4(b, j, k, p)
term(653) = term(653) + t2(a,b,q,i) * wm_interm_13_pt4(a, j, i, k) * wm_interm_20_pt4(b, j, p, k)
term(654) = term(654) + t2(a,b,q,i) * wm_interm_13_pt4(a, i, j, k) * wm_interm_20_pt4(b, j, p, k)
term(655) = term(655) + t2(a,b,q,i) * wm_interm_13_pt4(b, i, j, k) * wm_interm_20_pt4(a, j, p, k)
term(656) = term(656) + t2(a,b,q,i) * wm_interm_13_pt4(b, j, i, k) * wm_interm_20_pt4(a, j, p, k)
term(657) = term(657) + t2(a,b,q,i) * wm_interm_13_pt4(a, p, j, k) * wm_interm_20_pt4(b, j, k, i)
term(658) = term(658) + t2(a,b,q,i) * wm_interm_13_pt4(a, j, p, k) * wm_interm_20_pt4(b, j, k, i)
term(659) = term(659) + t2(a,b,q,i) * wm_interm_13_pt4(b, j, p, k) * wm_interm_20_pt4(a, j, i, k)
term(660) = term(660) + t2(a,b,q,i) * wm_interm_13_pt4(b, p, j, k) * wm_interm_20_pt4(a, j, i, k)
term(661) = term(661) + t2(a,b,q,i) * wm_interm_13_pt4(a, p, j, k) * wm_interm_20_pt4(b, j, i, k)
term(662) = term(662) + t2(a,b,q,i) * wm_interm_13_pt4(a, j, p, k) * wm_interm_20_pt4(b, j, i, k)
end do 
end do 
end do 
end do 
end do 

term(618) = term(618) * 4.0d+0 
term(619) = term(619) * (-8.0d+0) 
term(620) = term(620) * 16.0d+0 
term(621) = term(621) * 4.0d+0 
term(622) = term(622) * (-8.0d+0) 
term(623) = term(623) * 4.0d+0 
term(624) = term(624) * (-8.0d+0) 
term(625) = term(625) * 4.0d+0 
term(626) = term(626) * (-8.0d+0) 
term(627) = term(627) * 4.0d+0 
term(628) = term(628) * (-8.0d+0) 
term(629) = term(629) * 4.0d+0 
term(630) = term(630) * (-8.0d+0) 
term(631) = -term(631) 
term(632) = term(632) * 2.0d+0 
term(633) = -term(633) 
term(634) = term(634) * 2.0d+0 
term(635) = term(635) * 2.0d+0 
term(636) = term(636) * (-4.0d+0) 
term(637) = -term(637) 
term(638) = term(638) * 2.0d+0 
term(639) = term(639) * 2.0d+0 
term(640) = -term(640) 
term(641) = -term(641) 
term(642) = term(642) * 2.0d+0 
term(643) = term(643) * (-4.0d+0) 
term(644) = -term(644) 
term(645) = term(645) * 2.0d+0 
term(646) = term(646) * 2.0d+0 
term(647) = -term(647) 
term(648) = term(648) * 2.0d+0 
term(649) = term(649) * 2.0d+0 
term(650) = term(650) * (-4.0d+0) 
term(651) = -term(651) 
term(652) = term(652) * 2.0d+0 
term(653) = -term(653) 
term(654) = term(654) * 2.0d+0 
term(655) = -term(655) 
term(656) = term(656) * 2.0d+0 
term(657) = term(657) * 2.0d+0 
term(658) = term(658) * (-4.0d+0) 
term(659) = -term(659) 
term(660) = term(660) * 2.0d+0 
term(661) = -term(661) 
term(662) = term(662) * 2.0d+0 

do i = 1, nocc 
term(663) = term(663) + wm_interm_5_pt4(q, i) * wm_interm_6_pt4(i, p)
term(664) = term(664) + wm_interm_5_pt4(q, i) * wm_interm_7_pt4(i, p)
term(665) = term(665) + wm_interm_5_pt4(i, q) * wm_interm_6_pt4(p, i)
term(666) = term(666) + wm_interm_5_pt4(i, q) * wm_interm_7_pt4(p, i)
term(667) = term(667) + wm_interm_16_pt4(i, p) * wm_interm_5_pt4(q, i)
term(668) = term(668) + wm_interm_25_pt4(p, i) * wm_interm_34_pt4(q, i)
term(669) = term(669) + wm_interm_16_pt4(p, i) * wm_interm_5_pt4(i, q)
term(670) = term(670) + wm_interm_25_pt4(i, p) * wm_interm_34_pt4(i, q)
end do 

term(663) = term(663) * (-2.0d+0) 
term(664) = term(664) * 4.0d+0 
term(665) = term(665) * (-2.0d+0) 
term(666) = term(666) * 4.0d+0 
term(667) = term(667) * 2.0d+0 
term(668) = term(668) * 2.0d+0 
term(669) = term(669) * 2.0d+0 
term(670) = term(670) * 2.0d+0 

do k = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(671) = term(671) + s2(a,b,i,q) * wm_interm_18_pt4(a, j, i, k) * wm_interm_39_pt4(b, k, j, p)
end do 
end do 
end do 
end do 
end do 

term(671) = term(671) * (-8.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(672) = term(672) + s1(a,i) * wm_interm_24_pt4(j, k, l, i) * wm_interm_39_pt4(a, l, k, j)
end do 
end do 
end do 
end do 
end do 

term(672) = term(672) * (-8.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(673) = term(673) + s1(a,i) * wm_interm_24_pt4(j, k, l, i) * wm_interm_39_pt4(a, l, j, k)
end do 
end do 
end do 
end do 
end do 

term(673) = term(673) * 16.0d+0 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(674) = term(674) + wm_interm_0_pt4(a, i) * wm_interm_1_pt4(a, i)
term(675) = term(675) + wm_interm_0_pt4(a, i) * wm_interm_2_pt4(a, i)
term(676) = term(676) + wm_interm_1_pt4(a, i) * wm_interm_3_pt4(a, i)
term(677) = term(677) + wm_interm_2_pt4(a, i) * wm_interm_3_pt4(a, i)
term(678) = term(678) + wm_interm_0_pt4(a, i) * wm_interm_9_pt4(a, p, i, q)
term(679) = term(679) + wm_interm_0_pt4(a, i) * wm_interm_9_pt4(a, i, p, q)
term(680) = term(680) + wm_interm_3_pt4(a, i) * wm_interm_9_pt4(a, p, i, q)
term(681) = term(681) + wm_interm_3_pt4(a, i) * wm_interm_9_pt4(a, i, p, q)
term(682) = term(682) + wm_interm_13_pt4(a, i, p, q) * wm_interm_1_pt4(a, i)
term(683) = term(683) + wm_interm_13_pt4(a, p, i, q) * wm_interm_1_pt4(a, i)
term(684) = term(684) + wm_interm_1_pt4(a, i) * wm_interm_8_pt4(a, p, i, q)
term(685) = term(685) + wm_interm_1_pt4(a, i) * wm_interm_8_pt4(a, i, p, q)
term(686) = term(686) + wm_interm_13_pt4(a, i, p, q) * wm_interm_2_pt4(a, i)
term(687) = term(687) + wm_interm_13_pt4(a, p, i, q) * wm_interm_2_pt4(a, i)
term(688) = term(688) + wm_interm_2_pt4(a, i) * wm_interm_8_pt4(a, p, i, q)
term(689) = term(689) + wm_interm_2_pt4(a, i) * wm_interm_8_pt4(a, i, p, q)
term(690) = term(690) + r1(vrdav_Rl, a,i) * wm_interm_1_pt4(a, p) * wm_interm_28_pt4(q, i)
term(691) = term(691) + r1(vrdav_Rl, a,i) * wm_interm_1_pt4(a, p) * wm_interm_29_pt4(q, i)
term(692) = term(692) + r1(vrdav_Rl, a,i) * wm_interm_28_pt4(q, i) * wm_interm_2_pt4(a, p)
term(693) = term(693) + r1(vrdav_Rl, a,i) * wm_interm_29_pt4(q, i) * wm_interm_2_pt4(a, p)
term(694) = term(694) + s1(a,q) * wm_interm_22_pt4(a, i) * wm_interm_43_pt4(p, i)
term(695) = term(695) + s1(a,q) * wm_interm_22_pt4(a, i) * wm_interm_44_pt4(p, i)
term(696) = term(696) + r1(vrdav_Rl, a,q) * wm_interm_17_pt4(a, i) * wm_interm_43_pt4(p, i)
term(697) = term(697) + r1(vrdav_Rl, a,q) * wm_interm_17_pt4(a, i) * wm_interm_44_pt4(p, i)
term(698) = term(698) + r1(vrdav_Rl, a,q) * wm_interm_23_pt4(a, i) * wm_interm_43_pt4(p, i)
term(699) = term(699) + r1(vrdav_Rl, a,q) * wm_interm_23_pt4(a, i) * wm_interm_44_pt4(p, i)
term(700) = term(700) + s1(a,q) * wm_interm_27_pt4(a, i) * wm_interm_43_pt4(p, i)
term(701) = term(701) + s1(a,q) * wm_interm_27_pt4(a, i) * wm_interm_44_pt4(p, i)
term(702) = term(702) + r1(vrdav_Rl, a,p) * wm_interm_1_pt4(a, i) * wm_interm_28_pt4(i, q)
term(703) = term(703) + r1(vrdav_Rl, a,p) * wm_interm_1_pt4(a, i) * wm_interm_29_pt4(i, q)
term(704) = term(704) + s1(a,i) * wm_interm_29_pt4(p, q) * wm_interm_47_pt4(a, i)
term(705) = term(705) + r1(vrdav_Rl, a,p) * wm_interm_28_pt4(i, q) * wm_interm_2_pt4(a, i)
term(706) = term(706) + r1(vrdav_Rl, a,p) * wm_interm_29_pt4(i, q) * wm_interm_2_pt4(a, i)
term(707) = term(707) + s1(a,i) * wm_interm_29_pt4(p, q) * wm_interm_48_pt4(a, i)
term(708) = term(708) + s1(a,i) * wm_interm_28_pt4(p, q) * wm_interm_47_pt4(a, i)
term(709) = term(709) + s1(a,i) * wm_interm_28_pt4(p, q) * wm_interm_48_pt4(a, i)
term(710) = term(710) + r1(vrdav_Rl, a,i) * wm_interm_17_pt4(a, q) * wm_interm_43_pt4(i, p)
term(711) = term(711) + r1(vrdav_Rl, a,i) * wm_interm_17_pt4(a, q) * wm_interm_44_pt4(i, p)
term(712) = term(712) + s1(a,i) * wm_interm_27_pt4(a, q) * wm_interm_43_pt4(i, p)
term(713) = term(713) + s1(a,i) * wm_interm_27_pt4(a, q) * wm_interm_44_pt4(i, p)
term(714) = term(714) + s1(a,p) * wm_interm_28_pt4(i, q) * wm_interm_48_pt4(a, i)
term(715) = term(715) + s1(a,p) * wm_interm_28_pt4(i, q) * wm_interm_47_pt4(a, i)
term(716) = term(716) + s1(a,p) * wm_interm_29_pt4(i, q) * wm_interm_48_pt4(a, i)
term(717) = term(717) + s1(a,p) * wm_interm_29_pt4(i, q) * wm_interm_47_pt4(a, i)
term(718) = term(718) + s1(a,i) * wm_interm_22_pt4(a, q) * wm_interm_43_pt4(i, p)
term(719) = term(719) + s1(a,i) * wm_interm_22_pt4(a, q) * wm_interm_44_pt4(i, p)
term(720) = term(720) + r1(vrdav_Rl, a,i) * wm_interm_23_pt4(a, q) * wm_interm_43_pt4(i, p)
term(721) = term(721) + r1(vrdav_Rl, a,i) * wm_interm_23_pt4(a, q) * wm_interm_44_pt4(i, p)
term(722) = term(722) + r1(vrdav_Rr, a,p) * wm_interm_0_pt4(a, i) * wm_interm_28_pt4(q, i)
term(723) = term(723) + r1(vrdav_Rr, a,p) * wm_interm_0_pt4(a, i) * wm_interm_29_pt4(q, i)
term(724) = term(724) + r1(vrdav_Rr, a,p) * wm_interm_28_pt4(q, i) * wm_interm_3_pt4(a, i)
term(725) = term(725) + r1(vrdav_Rr, a,p) * wm_interm_29_pt4(q, i) * wm_interm_3_pt4(a, i)
term(726) = term(726) + t1(a,q) * wm_interm_28_pt4(p, i) * wm_interm_53_pt4(a, i)
term(727) = term(727) + t1(a,q) * wm_interm_29_pt4(p, i) * wm_interm_53_pt4(a, i)
term(728) = term(728) + t1(a,q) * wm_interm_28_pt4(p, i) * wm_interm_50_pt4(a, i)
term(729) = term(729) + t1(a,q) * wm_interm_29_pt4(p, i) * wm_interm_50_pt4(a, i)
term(730) = term(730) + t1(a,q) * wm_interm_19_pt4(a, i) * wm_interm_52_pt4(p, i)
term(731) = term(731) + t1(a,q) * wm_interm_21_pt4(a, i) * wm_interm_52_pt4(p, i)
term(732) = term(732) + t1(a,q) * wm_interm_19_pt4(a, i) * wm_interm_55_pt4(p, i)
term(733) = term(733) + t1(a,q) * wm_interm_21_pt4(a, i) * wm_interm_55_pt4(p, i)
term(734) = term(734) + t1(a,i) * wm_interm_19_pt4(a, p) * wm_interm_52_pt4(i, q)
term(735) = term(735) + t1(a,i) * wm_interm_21_pt4(a, p) * wm_interm_52_pt4(i, q)
term(736) = term(736) + t1(a,i) * wm_interm_19_pt4(a, p) * wm_interm_55_pt4(i, q)
term(737) = term(737) + t1(a,i) * wm_interm_21_pt4(a, p) * wm_interm_55_pt4(i, q)
term(738) = term(738) + t1(a,i) * wm_interm_29_pt4(i, q) * wm_interm_53_pt4(a, p)
term(739) = term(739) + t1(a,i) * wm_interm_28_pt4(i, q) * wm_interm_53_pt4(a, p)
term(740) = term(740) + t1(a,i) * wm_interm_28_pt4(i, q) * wm_interm_50_pt4(a, p)
term(741) = term(741) + t1(a,i) * wm_interm_29_pt4(i, q) * wm_interm_50_pt4(a, p)
term(742) = term(742) + r1(vrdav_Rr, a,i) * wm_interm_0_pt4(a, p) * wm_interm_28_pt4(i, q)
term(743) = term(743) + r1(vrdav_Rr, a,i) * wm_interm_0_pt4(a, p) * wm_interm_29_pt4(i, q)
term(744) = term(744) + t1(a,i) * wm_interm_29_pt4(p, q) * wm_interm_50_pt4(a, i)
term(745) = term(745) + r1(vrdav_Rr, a,i) * wm_interm_28_pt4(i, q) * wm_interm_3_pt4(a, p)
term(746) = term(746) + r1(vrdav_Rr, a,i) * wm_interm_29_pt4(i, q) * wm_interm_3_pt4(a, p)
term(747) = term(747) + t1(a,i) * wm_interm_29_pt4(p, q) * wm_interm_53_pt4(a, i)
term(748) = term(748) + t1(a,i) * wm_interm_28_pt4(p, q) * wm_interm_50_pt4(a, i)
term(749) = term(749) + t1(a,i) * wm_interm_28_pt4(p, q) * wm_interm_53_pt4(a, i)
end do 
end do 

term(674) = term(674) * (-16.0d+0) 
term(675) = term(675) * 8.0d+0 
term(676) = term(676) * 8.0d+0 
term(677) = term(677) * (-4.0d+0) 
term(678) = term(678) * 4.0d+0 
term(679) = term(679) * (-8.0d+0) 
term(680) = term(680) * (-2.0d+0) 
term(681) = term(681) * 4.0d+0 
term(682) = term(682) * (-2.0d+0) 
term(683) = term(683) * 4.0d+0 
term(684) = term(684) * (-2.0d+0) 
term(685) = term(685) * 4.0d+0 
term(687) = term(687) * (-2.0d+0) 
term(689) = term(689) * (-2.0d+0) 
term(690) = term(690) * (-4.0d+0) 
term(691) = term(691) * 8.0d+0 
term(692) = term(692) * 2.0d+0 
term(693) = term(693) * (-4.0d+0) 
term(694) = term(694) * 4.0d+0 
term(695) = term(695) * (-8.0d+0) 
term(696) = term(696) * 4.0d+0 
term(697) = term(697) * (-8.0d+0) 
term(698) = term(698) * (-8.0d+0) 
term(699) = term(699) * 16.0d+0 
term(700) = term(700) * (-8.0d+0) 
term(701) = term(701) * 16.0d+0 
term(702) = term(702) * (-4.0d+0) 
term(703) = term(703) * 8.0d+0 
term(704) = term(704) * (-16.0d+0) 
term(705) = term(705) * 2.0d+0 
term(706) = term(706) * (-4.0d+0) 
term(707) = term(707) * 8.0d+0 
term(708) = term(708) * 8.0d+0 
term(709) = term(709) * (-4.0d+0) 
term(710) = term(710) * 4.0d+0 
term(711) = term(711) * (-8.0d+0) 
term(712) = term(712) * (-8.0d+0) 
term(713) = term(713) * 16.0d+0 
term(714) = term(714) * 4.0d+0 
term(715) = term(715) * (-8.0d+0) 
term(716) = term(716) * (-8.0d+0) 
term(717) = term(717) * 16.0d+0 
term(718) = term(718) * 4.0d+0 
term(719) = term(719) * (-8.0d+0) 
term(720) = term(720) * (-8.0d+0) 
term(721) = term(721) * 16.0d+0 
term(722) = term(722) * 8.0d+0 
term(723) = term(723) * (-16.0d+0) 
term(724) = term(724) * (-4.0d+0) 
term(725) = term(725) * 8.0d+0 
term(726) = term(726) * (-2.0d+0) 
term(727) = term(727) * 4.0d+0 
term(728) = term(728) * 4.0d+0 
term(729) = term(729) * (-8.0d+0) 
term(730) = term(730) * (-2.0d+0) 
term(731) = term(731) * 4.0d+0 
term(732) = term(732) * 4.0d+0 
term(733) = term(733) * (-8.0d+0) 
term(734) = term(734) * (-2.0d+0) 
term(735) = term(735) * 4.0d+0 
term(736) = term(736) * 4.0d+0 
term(737) = term(737) * (-8.0d+0) 
term(738) = term(738) * 4.0d+0 
term(739) = term(739) * (-2.0d+0) 
term(740) = term(740) * 4.0d+0 
term(741) = term(741) * (-8.0d+0) 
term(742) = term(742) * 4.0d+0 
term(743) = term(743) * (-8.0d+0) 
term(744) = term(744) * 16.0d+0 
term(745) = term(745) * (-2.0d+0) 
term(746) = term(746) * 4.0d+0 
term(747) = term(747) * (-8.0d+0) 
term(748) = term(748) * (-8.0d+0) 
term(749) = term(749) * 4.0d+0 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(750) = term(750) + s1(a,i) * wm_interm_24_pt4(j, k, q, i) * wm_interm_39_pt4(a, p, j, k)
term(751) = term(751) + t1(a,q) * wm_interm_24_pt4(i, p, j, k) * wm_interm_51_pt4(a, j, k, i)
term(752) = term(752) + t1(a,q) * wm_interm_24_pt4(p, i, j, k) * wm_interm_51_pt4(a, j, k, i)
term(753) = term(753) + t1(a,q) * wm_interm_24_pt4(p, i, j, k) * wm_interm_49_pt4(a, j, k, i)
term(754) = term(754) + t1(a,q) * wm_interm_24_pt4(i, p, j, k) * wm_interm_49_pt4(a, j, k, i)
end do 
end do 
end do 
end do 

term(750) = term(750) * (-4.0d+0) 
term(751) = -term(751) 
term(752) = term(752) * 2.0d+0 
term(753) = -term(753) 
term(754) = term(754) * 2.0d+0 

do j = 1, nocc 
do i = 1, nocc 
term(755) = term(755) + wm_interm_4_pt4(q, i, j, p) * wm_interm_5_pt4(j, i)
term(756) = term(756) + wm_interm_4_pt4(q, i, p, j) * wm_interm_5_pt4(j, i)
term(757) = term(757) + wm_interm_5_pt4(i, j) * wm_interm_6_pt4(j, i)
term(758) = term(758) + wm_interm_5_pt4(i, j) * wm_interm_7_pt4(j, i)
term(759) = term(759) + wm_interm_4_pt4(i, q, p, j) * wm_interm_5_pt4(j, i)
term(760) = term(760) + wm_interm_4_pt4(i, q, j, p) * wm_interm_5_pt4(j, i)
term(761) = term(761) + wm_interm_16_pt4(i, j) * wm_interm_5_pt4(j, i)
term(762) = term(762) + wm_interm_25_pt4(i, j) * wm_interm_34_pt4(i, j)
end do 
end do 

term(755) = -term(755) 
term(756) = term(756) * 2.0d+0 
term(757) = term(757) * 4.0d+0 
term(758) = term(758) * (-8.0d+0) 
term(759) = -term(759) 
term(760) = term(760) * 2.0d+0 
term(761) = term(761) * (-4.0d+0) 
term(762) = term(762) * (-4.0d+0) 

do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(763) = term(763) + wm_interm_10_pt4(a, b) * wm_interm_11_pt4(b, a)
term(764) = term(764) + wm_interm_11_pt4(a, b) * wm_interm_12_pt4(b, a)
term(765) = term(765) + r1(vrdav_Rl, b,q) * wm_interm_1_pt4(a, p) * wm_interm_31_pt4(a, b)
term(766) = term(766) + r1(vrdav_Rl, b,q) * wm_interm_1_pt4(a, p) * wm_interm_33_pt4(a, b)
term(767) = term(767) + r1(vrdav_Rl, b,q) * wm_interm_2_pt4(a, p) * wm_interm_31_pt4(a, b)
term(768) = term(768) + r1(vrdav_Rl, b,q) * wm_interm_2_pt4(a, p) * wm_interm_33_pt4(a, b)
term(769) = term(769) + r1(vrdav_Rl, a,p) * wm_interm_17_pt4(b, q) * wm_interm_40_pt4(a, b)
term(770) = term(770) + r1(vrdav_Rl, a,p) * wm_interm_17_pt4(b, q) * wm_interm_41_pt4(a, b)
term(771) = term(771) + s1(a,p) * wm_interm_27_pt4(b, q) * wm_interm_40_pt4(a, b)
term(772) = term(772) + s1(a,p) * wm_interm_27_pt4(b, q) * wm_interm_41_pt4(a, b)
term(773) = term(773) + s1(a,p) * wm_interm_22_pt4(b, q) * wm_interm_40_pt4(a, b)
term(774) = term(774) + s1(a,p) * wm_interm_22_pt4(b, q) * wm_interm_41_pt4(a, b)
term(775) = term(775) + r1(vrdav_Rl, a,p) * wm_interm_23_pt4(b, q) * wm_interm_40_pt4(a, b)
term(776) = term(776) + r1(vrdav_Rl, a,p) * wm_interm_23_pt4(b, q) * wm_interm_41_pt4(a, b)
term(777) = term(777) + r1(vrdav_Rr, a,p) * wm_interm_0_pt4(b, q) * wm_interm_31_pt4(a, b)
term(778) = term(778) + r1(vrdav_Rr, a,p) * wm_interm_0_pt4(b, q) * wm_interm_33_pt4(a, b)
term(779) = term(779) + r1(vrdav_Rr, a,p) * wm_interm_31_pt4(a, b) * wm_interm_3_pt4(b, q)
term(780) = term(780) + r1(vrdav_Rr, a,p) * wm_interm_33_pt4(a, b) * wm_interm_3_pt4(b, q)
end do 
end do 

term(763) = term(763) * 4.0d+0 
term(764) = term(764) * (-8.0d+0) 
term(765) = term(765) * 8.0d+0 
term(766) = term(766) * (-4.0d+0) 
term(767) = term(767) * (-4.0d+0) 
term(768) = term(768) * 2.0d+0 
term(769) = term(769) * 4.0d+0 
term(770) = term(770) * (-8.0d+0) 
term(771) = term(771) * (-8.0d+0) 
term(772) = term(772) * 16.0d+0 
term(773) = term(773) * 4.0d+0 
term(774) = term(774) * (-8.0d+0) 
term(775) = term(775) * (-8.0d+0) 
term(776) = term(776) * 16.0d+0 
term(777) = term(777) * (-16.0d+0) 
term(778) = term(778) * 8.0d+0 
term(779) = term(779) * 8.0d+0 
term(780) = term(780) * (-4.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(781) = term(781) + s1(a,i) * wm_interm_26_pt4(a, j, k, l) * wm_interm_42_pt4(j, i, l, k)
term(782) = term(782) + s1(a,i) * wm_interm_26_pt4(a, j, k, l) * wm_interm_42_pt4(j, i, k, l)
term(783) = term(783) + t1(a,i) * wm_interm_20_pt4(a, j, k, l) * wm_interm_54_pt4(j, i, k, l)
term(784) = term(784) + t1(a,i) * wm_interm_20_pt4(a, j, k, l) * wm_interm_54_pt4(j, i, l, k)
term(785) = term(785) + t1(a,i) * wm_interm_20_pt4(a, j, k, l) * wm_interm_54_pt4(i, j, l, k)
term(786) = term(786) + t1(a,i) * wm_interm_20_pt4(a, j, k, l) * wm_interm_54_pt4(i, j, k, l)
end do 
end do 
end do 
end do 
end do 

term(781) = term(781) * (-8.0d+0) 
term(782) = term(782) * 16.0d+0 
term(783) = term(783) * (-8.0d+0) 
term(784) = term(784) * 4.0d+0 
term(785) = term(785) * (-8.0d+0) 
term(786) = term(786) * 4.0d+0 

do a = nocc + 1, nactive 
do i = 1, nocc 
do b = nocc + 1, nactive 
term(787) = term(787) + r1(vrdav_Rl, a,i) * wm_interm_1_pt4(b, i) * wm_interm_31_pt4(b, a)
term(788) = term(788) + r1(vrdav_Rl, a,i) * wm_interm_1_pt4(b, i) * wm_interm_33_pt4(b, a)
term(789) = term(789) + r1(vrdav_Rl, a,i) * wm_interm_2_pt4(b, i) * wm_interm_31_pt4(b, a)
term(790) = term(790) + r1(vrdav_Rl, a,i) * wm_interm_2_pt4(b, i) * wm_interm_33_pt4(b, a)
term(791) = term(791) + r1(vrdav_Rl, a,i) * wm_interm_31_pt4(b, a) * wm_interm_46_pt4(b, i, p, q)
term(792) = term(792) + r1(vrdav_Rl, a,i) * wm_interm_31_pt4(b, a) * wm_interm_9_pt4(b, i, p, q)
term(793) = term(793) + r1(vrdav_Rl, a,i) * wm_interm_33_pt4(b, a) * wm_interm_46_pt4(b, i, p, q)
term(794) = term(794) + r1(vrdav_Rl, a,i) * wm_interm_33_pt4(b, a) * wm_interm_9_pt4(b, i, p, q)
end do 
end do 
end do 

term(787) = term(787) * (-16.0d+0) 
term(788) = term(788) * 8.0d+0 
term(789) = term(789) * 8.0d+0 
term(790) = term(790) * (-4.0d+0) 
term(791) = term(791) * 4.0d+0 
term(792) = term(792) * (-8.0d+0) 
term(793) = term(793) * (-2.0d+0) 
term(794) = term(794) * 4.0d+0 

do k = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(795) = term(795) + s2(a,b,i,q) * wm_interm_26_pt4(b, j, i, k) * wm_interm_46_pt4(a, j, p, k)
term(796) = term(796) + s2(a,b,i,q) * wm_interm_26_pt4(b, j, i, k) * wm_interm_9_pt4(a, j, p, k)
term(797) = term(797) + s2(a,b,i,p) * wm_interm_26_pt4(b, j, k, q) * wm_interm_9_pt4(a, j, i, k)
term(798) = term(798) + s2(a,b,i,p) * wm_interm_26_pt4(b, j, k, q) * wm_interm_46_pt4(a, j, i, k)
term(799) = term(799) + s2(a,b,i,p) * wm_interm_26_pt4(b, j, q, k) * wm_interm_9_pt4(a, j, i, k)
term(800) = term(800) + s2(a,b,i,p) * wm_interm_26_pt4(b, j, q, k) * wm_interm_46_pt4(a, j, i, k)
end do 
end do 
end do 
end do 
end do 

term(795) = term(795) * 4.0d+0 
term(796) = term(796) * (-8.0d+0) 
term(797) = term(797) * (-8.0d+0) 
term(798) = term(798) * 16.0d+0 
term(799) = term(799) * 4.0d+0 
term(800) = term(800) * (-8.0d+0) 


    calc_D_oo_wm_pt4 = zero
    do s = 0, 800
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
    double precision, dimension(:), intent(in) :: vrdav_Rl
    double precision, dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , k, i, j, a, b 
    real(F64), dimension(0:43) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + r1(vrdav_Rl, q,i) * wm_interm_18_pt4(a, j, i, k) * wm_interm_20_pt4(a, j, p, k)
term(1) = term(1) + r1(vrdav_Rl, q,i) * wm_interm_18_pt4(a, j, i, k) * wm_interm_20_pt4(a, j, k, p)
term(2) = term(2) + r1(vrdav_Rl, q,i) * wm_interm_18_pt4(a, j, k, i) * wm_interm_20_pt4(a, j, p, k)
term(3) = term(3) + t2(a,q,j,i) * wm_interm_20_pt4(a, p, i, k) * wm_interm_34_pt4(j, k)
term(4) = term(4) + t2(a,q,j,i) * wm_interm_20_pt4(a, p, j, k) * wm_interm_34_pt4(i, k)
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
do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(5) = term(5) + r1(vrdav_Rl, q,i) * wm_interm_18_pt4(a, j, k, i) * wm_interm_20_pt4(a, j, k, p)
term(6) = term(6) + t2(a,q,j,i) * wm_interm_20_pt4(a, p, k, i) * wm_interm_34_pt4(j, k)
end do 
end do 
end do 
end do 

term(5) = term(5) * 8.0d+0 
term(6) = term(6) * 2.0d+0 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(7) = term(7) + t1(a,i) * wm_interm_20_pt4(a, p, j, k) * wm_interm_26_pt4(q, i, k, j)
term(8) = term(8) + t1(a,i) * wm_interm_20_pt4(a, p, j, k) * wm_interm_26_pt4(q, i, j, k)
end do 
end do 
end do 
end do 

term(7) = term(7) * (-4.0d+0) 
term(8) = term(8) * 2.0d+0 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(9) = term(9) + r1(vrdav_Rl, q,i) * wm_interm_24_pt4(j, p, i, k) * wm_interm_25_pt4(j, k)
term(10) = term(10) + r1(vrdav_Rl, q,i) * wm_interm_24_pt4(p, j, i, k) * wm_interm_25_pt4(j, k)
end do 
end do 
end do 

term(9) = term(9) * (-4.0d+0) 
term(10) = term(10) * 8.0d+0 

do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(11) = term(11) + t2(a,q,j,i) * wm_interm_20_pt4(a, p, k, j) * wm_interm_34_pt4(i, k)
end do 
end do 
end do 
end do 

term(11) = term(11) * (-4.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(12) = term(12) + r1(vrdav_Rl, q,i) * wm_interm_17_pt4(a, i) * wm_interm_19_pt4(a, p)
term(13) = term(13) + r1(vrdav_Rl, q,i) * wm_interm_17_pt4(a, i) * wm_interm_21_pt4(a, p)
term(14) = term(14) + r1(vrdav_Rl, q,i) * wm_interm_19_pt4(a, p) * wm_interm_23_pt4(a, i)
term(15) = term(15) + r1(vrdav_Rl, q,i) * wm_interm_21_pt4(a, p) * wm_interm_23_pt4(a, i)
term(16) = term(16) + t1(a,i) * wm_interm_16_pt4(i, p) * wm_interm_33_pt4(a, q)
term(17) = term(17) + t1(a,i) * wm_interm_16_pt4(i, p) * wm_interm_31_pt4(a, q)
term(18) = term(18) + r1(vrdav_Rl, a,p) * s1(q,i) * wm_interm_1_pt4(a, i)
term(19) = term(19) + r1(vrdav_Rl, a,p) * s1(q,i) * wm_interm_2_pt4(a, i)
term(20) = term(20) + r1(vrdav_Rr, a,p) * t1(q,i) * wm_interm_0_pt4(a, i)
term(21) = term(21) + r1(vrdav_Rr, a,p) * t1(q,i) * wm_interm_3_pt4(a, i)
end do 
end do 

term(12) = term(12) * (-4.0d+0) 
term(13) = term(13) * 8.0d+0 
term(14) = term(14) * 8.0d+0 
term(15) = term(15) * (-16.0d+0) 
term(16) = term(16) * 2.0d+0 
term(17) = term(17) * (-4.0d+0) 
term(18) = term(18) * 4.0d+0 
term(19) = term(19) * (-2.0d+0) 
term(20) = term(20) * (-4.0d+0) 
term(21) = term(21) * 2.0d+0 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(22) = term(22) + r1(vrdav_Rl, q,i) * wm_interm_18_pt4(a, p, i, j) * wm_interm_21_pt4(a, j)
term(23) = term(23) + r1(vrdav_Rl, q,i) * wm_interm_18_pt4(a, p, i, j) * wm_interm_19_pt4(a, j)
term(24) = term(24) + t1(q,i) * wm_interm_20_pt4(a, p, i, j) * wm_interm_22_pt4(a, j)
term(25) = term(25) + t1(q,i) * wm_interm_20_pt4(a, p, i, j) * wm_interm_27_pt4(a, j)
end do 
end do 
end do 

term(22) = term(22) * (-16.0d+0) 
term(23) = term(23) * 8.0d+0 
term(24) = term(24) * (-4.0d+0) 
term(25) = term(25) * 8.0d+0 

do j = 1, nocc 
do i = 1, nocc 
term(26) = term(26) + r1(vrdav_Rl, q,j) * wm_interm_25_pt4(i, p) * wm_interm_28_pt4(i, j)
term(27) = term(27) + r1(vrdav_Rl, q,j) * wm_interm_25_pt4(i, p) * wm_interm_29_pt4(i, j)
term(28) = term(28) + t1(q,j) * wm_interm_16_pt4(i, p) * wm_interm_28_pt4(j, i)
term(29) = term(29) + t1(q,j) * wm_interm_16_pt4(i, p) * wm_interm_29_pt4(j, i)
end do 
end do 

term(26) = term(26) * 2.0d+0 
term(27) = term(27) * (-4.0d+0) 
term(28) = term(28) * 2.0d+0 
term(29) = term(29) * (-4.0d+0) 

do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(30) = term(30) + r1(vrdav_Rr, a,p) * wm_interm_31_pt4(a, b) * wm_interm_36_pt4(b, q)
term(31) = term(31) + r1(vrdav_Rr, a,p) * wm_interm_33_pt4(a, b) * wm_interm_36_pt4(b, q)
end do 
end do 

term(30) = term(30) * (-4.0d+0) 
term(31) = term(31) * 2.0d+0 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(32) = term(32) + r1(vrdav_Rl, q,i) * wm_interm_18_pt4(a, p, j, i) * wm_interm_19_pt4(a, j)
term(33) = term(33) + r1(vrdav_Rl, q,i) * wm_interm_18_pt4(a, p, j, i) * wm_interm_21_pt4(a, j)
term(34) = term(34) + t1(q,i) * wm_interm_20_pt4(a, p, j, i) * wm_interm_22_pt4(a, j)
term(35) = term(35) + t1(q,i) * wm_interm_20_pt4(a, p, j, i) * wm_interm_27_pt4(a, j)
end do 
end do 
end do 

term(32) = term(32) * (-4.0d+0) 
term(33) = term(33) * 8.0d+0 
term(34) = term(34) * 2.0d+0 
term(35) = term(35) * (-4.0d+0) 

do a = nocc + 1, nactive 
term(36) = term(36) + wm_interm_1_pt4(a, p) * wm_interm_36_pt4(a, q)
term(37) = term(37) + wm_interm_2_pt4(a, p) * wm_interm_36_pt4(a, q)
term(38) = term(38) + wm_interm_11_pt4(a, q) * wm_interm_48_pt4(a, p)
term(39) = term(39) + wm_interm_11_pt4(a, q) * wm_interm_47_pt4(a, p)
end do 

term(36) = term(36) * 4.0d+0 
term(37) = term(37) * (-2.0d+0) 
term(38) = term(38) * 2.0d+0 
term(39) = term(39) * (-4.0d+0) 

do i = 1, nocc 
term(40) = term(40) + wm_interm_53_pt4(q, i) * wm_interm_5_pt4(p, i)
term(41) = term(41) + wm_interm_50_pt4(q, i) * wm_interm_5_pt4(p, i)
term(42) = term(42) + wm_interm_0_pt4(q, i) * wm_interm_25_pt4(i, p)
term(43) = term(43) + wm_interm_25_pt4(i, p) * wm_interm_3_pt4(q, i)
end do 

term(40) = term(40) * 2.0d+0 
term(41) = term(41) * (-4.0d+0) 
term(42) = term(42) * (-4.0d+0) 
term(43) = term(43) * 2.0d+0 


    calc_D_ov_wm_pt4 = zero
    do s = 0, 43
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
    double precision, dimension(:), intent(in) :: vrdav_Rl
    double precision, dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , j, i, k, a, b 
    real(F64), dimension(0:197) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + s2(a,p,j,i) * wm_interm_16_pt4(k, i) * wm_interm_18_pt4(a, q, k, j)
term(1) = term(1) + s2(a,p,q,i) * wm_interm_16_pt4(j, k) * wm_interm_18_pt4(a, k, i, j)
term(2) = term(2) + s2(a,p,i,q) * wm_interm_16_pt4(j, k) * wm_interm_18_pt4(a, k, i, j)
term(3) = term(3) + s2(a,p,j,i) * wm_interm_25_pt4(i, k) * wm_interm_26_pt4(a, q, k, j)
term(4) = term(4) + t2(a,p,q,i) * wm_interm_16_pt4(j, k) * wm_interm_37_pt4(a, i, k, j)
term(5) = term(5) + t2(a,p,q,i) * wm_interm_16_pt4(j, k) * wm_interm_37_pt4(a, k, i, j)
term(6) = term(6) + t2(a,p,i,q) * wm_interm_16_pt4(j, k) * wm_interm_37_pt4(a, k, i, j)
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
term(7) = term(7) + s2(a,p,i,q) * wm_interm_16_pt4(j, k) * wm_interm_18_pt4(a, k, j, i)
term(8) = term(8) + s2(a,p,q,i) * wm_interm_16_pt4(j, k) * wm_interm_18_pt4(a, k, j, i)
term(9) = term(9) + t2(a,p,j,i) * wm_interm_16_pt4(i, k) * wm_interm_37_pt4(a, k, j, q)
term(10) = term(10) + t2(a,p,j,i) * wm_interm_16_pt4(i, k) * wm_interm_35_pt4(a, k, j, q)
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
term(11) = term(11) + s2(a,p,j,i) * wm_interm_16_pt4(k, j) * wm_interm_18_pt4(a, q, k, i)
term(12) = term(12) + s2(a,p,i,q) * wm_interm_25_pt4(j, k) * wm_interm_26_pt4(a, j, k, i)
term(13) = term(13) + s2(a,p,j,i) * wm_interm_25_pt4(j, k) * wm_interm_26_pt4(a, q, k, i)
term(14) = term(14) + s2(a,p,q,i) * wm_interm_25_pt4(j, k) * wm_interm_26_pt4(a, j, k, i)
term(15) = term(15) + s1(p,i) * wm_interm_20_pt4(a, j, k, q) * wm_interm_26_pt4(a, j, k, i)
term(16) = term(16) + t2(a,p,q,i) * wm_interm_20_pt4(a, j, k, i) * wm_interm_34_pt4(j, k)
term(17) = term(17) + t2(a,p,i,q) * wm_interm_20_pt4(a, j, k, i) * wm_interm_34_pt4(j, k)
term(18) = term(18) + t2(a,p,j,i) * wm_interm_16_pt4(i, k) * wm_interm_37_pt4(a, j, k, q)
term(19) = term(19) + t2(a,p,j,i) * wm_interm_16_pt4(j, k) * wm_interm_35_pt4(a, k, i, q)
term(20) = term(20) + t2(a,p,j,i) * wm_interm_16_pt4(j, k) * wm_interm_35_pt4(a, i, k, q)
term(21) = term(21) + t2(a,p,j,i) * wm_interm_16_pt4(i, k) * wm_interm_35_pt4(a, j, k, q)
term(22) = term(22) + t1(p,i) * wm_interm_20_pt4(a, j, k, i) * wm_interm_26_pt4(a, j, k, q)
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

do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(23) = term(23) + t1(b,q) * wm_interm_31_pt4(b, a) * wm_interm_32_pt4(a, p)
term(24) = term(24) + t1(b,q) * wm_interm_32_pt4(a, p) * wm_interm_33_pt4(b, a)
end do 
end do 

term(23) = term(23) * (-4.0d+0) 
term(24) = term(24) * 2.0d+0 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(25) = term(25) + s2(a,p,j,i) * wm_interm_16_pt4(k, j) * wm_interm_18_pt4(a, q, i, k)
term(26) = term(26) + s2(a,p,j,i) * wm_interm_16_pt4(k, i) * wm_interm_18_pt4(a, q, j, k)
term(27) = term(27) + s1(p,i) * wm_interm_20_pt4(a, j, q, k) * wm_interm_26_pt4(a, j, i, k)
term(28) = term(28) + s1(p,i) * wm_interm_20_pt4(a, j, k, q) * wm_interm_26_pt4(a, j, i, k)
term(29) = term(29) + s1(p,i) * wm_interm_20_pt4(a, j, q, k) * wm_interm_26_pt4(a, j, k, i)
term(30) = term(30) + s2(a,p,j,i) * wm_interm_25_pt4(i, k) * wm_interm_26_pt4(a, q, j, k)
term(31) = term(31) + s2(a,p,i,q) * wm_interm_25_pt4(j, k) * wm_interm_26_pt4(a, j, i, k)
term(32) = term(32) + s2(a,p,q,i) * wm_interm_25_pt4(j, k) * wm_interm_26_pt4(a, j, i, k)
term(33) = term(33) + s2(a,p,j,i) * wm_interm_25_pt4(j, k) * wm_interm_26_pt4(a, q, i, k)
term(34) = term(34) + t2(a,p,q,i) * wm_interm_20_pt4(a, j, i, k) * wm_interm_34_pt4(j, k)
term(35) = term(35) + t2(a,p,i,q) * wm_interm_20_pt4(a, j, i, k) * wm_interm_34_pt4(j, k)
term(36) = term(36) + t1(p,i) * wm_interm_20_pt4(a, j, i, k) * wm_interm_26_pt4(a, j, q, k)
term(37) = term(37) + t1(p,i) * wm_interm_20_pt4(a, j, k, i) * wm_interm_26_pt4(a, j, q, k)
term(38) = term(38) + t1(p,i) * wm_interm_20_pt4(a, j, i, k) * wm_interm_26_pt4(a, j, k, q)
end do 
end do 
end do 
end do 

term(25) = term(25) * 8.0d+0 
term(26) = term(26) * (-4.0d+0) 
term(27) = term(27) * 8.0d+0 
term(28) = term(28) * (-4.0d+0) 
term(29) = term(29) * (-4.0d+0) 
term(30) = term(30) * (-4.0d+0) 
term(31) = term(31) * 8.0d+0 
term(32) = term(32) * (-4.0d+0) 
term(33) = term(33) * 8.0d+0 
term(34) = term(34) * 2.0d+0 
term(35) = term(35) * (-4.0d+0) 
term(36) = term(36) * (-8.0d+0) 
term(37) = term(37) * 4.0d+0 
term(38) = term(38) * 4.0d+0 

do a = nocc + 1, nactive 
do i = 1, nocc 
term(39) = term(39) + r1(vrdav_Rl, a,i) * wm_interm_25_pt4(i, q) * wm_interm_31_pt4(p, a)
term(40) = term(40) + r1(vrdav_Rl, a,i) * wm_interm_25_pt4(i, q) * wm_interm_33_pt4(p, a)
end do 
end do 

term(39) = term(39) * (-4.0d+0) 
term(40) = term(40) * 2.0d+0 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(41) = term(41) + s1(a,q) * wm_interm_19_pt4(p, i) * wm_interm_22_pt4(a, i)
term(42) = term(42) + s1(a,q) * wm_interm_21_pt4(p, i) * wm_interm_22_pt4(a, i)
term(43) = term(43) + s1(p,i) * wm_interm_19_pt4(a, q) * wm_interm_22_pt4(a, i)
term(44) = term(44) + s1(p,i) * wm_interm_21_pt4(a, q) * wm_interm_22_pt4(a, i)
term(45) = term(45) + r1(vrdav_Rl, a,q) * wm_interm_17_pt4(a, i) * wm_interm_19_pt4(p, i)
term(46) = term(46) + r1(vrdav_Rl, a,q) * wm_interm_17_pt4(a, i) * wm_interm_21_pt4(p, i)
term(47) = term(47) + r1(vrdav_Rl, a,q) * wm_interm_19_pt4(p, i) * wm_interm_23_pt4(a, i)
term(48) = term(48) + r1(vrdav_Rl, a,q) * wm_interm_21_pt4(p, i) * wm_interm_23_pt4(a, i)
term(49) = term(49) + s1(a,q) * wm_interm_19_pt4(p, i) * wm_interm_27_pt4(a, i)
term(50) = term(50) + s1(a,q) * wm_interm_21_pt4(p, i) * wm_interm_27_pt4(a, i)
term(51) = term(51) + s1(p,i) * wm_interm_19_pt4(a, q) * wm_interm_27_pt4(a, i)
term(52) = term(52) + s1(p,i) * wm_interm_21_pt4(a, q) * wm_interm_27_pt4(a, i)
term(53) = term(53) + s1(p,q) * wm_interm_19_pt4(a, i) * wm_interm_22_pt4(a, i)
term(54) = term(54) + s1(p,q) * wm_interm_21_pt4(a, i) * wm_interm_22_pt4(a, i)
term(55) = term(55) + s1(p,q) * wm_interm_21_pt4(a, i) * wm_interm_27_pt4(a, i)
term(56) = term(56) + s1(p,q) * wm_interm_19_pt4(a, i) * wm_interm_27_pt4(a, i)
term(57) = term(57) + t1(p,q) * wm_interm_19_pt4(a, i) * wm_interm_22_pt4(a, i)
term(58) = term(58) + t1(p,q) * wm_interm_21_pt4(a, i) * wm_interm_22_pt4(a, i)
term(59) = term(59) + t1(p,q) * wm_interm_21_pt4(a, i) * wm_interm_27_pt4(a, i)
term(60) = term(60) + t1(p,q) * wm_interm_19_pt4(a, i) * wm_interm_27_pt4(a, i)
term(61) = term(61) + t1(a,q) * wm_interm_19_pt4(a, i) * wm_interm_22_pt4(p, i)
term(62) = term(62) + t1(a,q) * wm_interm_21_pt4(a, i) * wm_interm_22_pt4(p, i)
term(63) = term(63) + t1(a,q) * wm_interm_21_pt4(a, i) * wm_interm_27_pt4(p, i)
term(64) = term(64) + r1(vrdav_Rr, a,i) * wm_interm_31_pt4(a, p) * wm_interm_34_pt4(i, q)
term(65) = term(65) + r1(vrdav_Rr, a,i) * wm_interm_33_pt4(a, p) * wm_interm_34_pt4(i, q)
term(66) = term(66) + t1(a,q) * wm_interm_19_pt4(a, i) * wm_interm_27_pt4(p, i)
term(67) = term(67) + t1(p,i) * wm_interm_21_pt4(a, i) * wm_interm_27_pt4(a, q)
term(68) = term(68) + t1(p,i) * wm_interm_19_pt4(a, i) * wm_interm_22_pt4(a, q)
term(69) = term(69) + t1(p,i) * wm_interm_21_pt4(a, i) * wm_interm_22_pt4(a, q)
term(70) = term(70) + t1(p,i) * wm_interm_19_pt4(a, i) * wm_interm_27_pt4(a, q)
term(71) = term(71) + r1(vrdav_Rl, a,i) * s1(p,q) * wm_interm_1_pt4(a, i)
term(72) = term(72) + r1(vrdav_Rl, a,i) * s1(p,q) * wm_interm_2_pt4(a, i)
term(73) = term(73) + r1(vrdav_Rl, p,i) * s1(a,q) * wm_interm_1_pt4(a, i)
term(74) = term(74) + r1(vrdav_Rl, a,i) * t1(p,q) * wm_interm_1_pt4(a, i)
term(75) = term(75) + r1(vrdav_Rl, a,i) * t1(p,q) * wm_interm_2_pt4(a, i)
term(76) = term(76) + r1(vrdav_Rr, a,i) * s1(p,q) * wm_interm_0_pt4(a, i)
term(77) = term(77) + r1(vrdav_Rr, a,i) * s1(p,q) * wm_interm_3_pt4(a, i)
term(78) = term(78) + r1(vrdav_Rr, p,i) * t1(a,q) * wm_interm_0_pt4(a, i)
term(79) = term(79) + r1(vrdav_Rr, p,i) * t1(a,q) * wm_interm_3_pt4(a, i)
term(80) = term(80) + r1(vrdav_Rr, a,i) * t1(p,q) * wm_interm_0_pt4(a, i)
term(81) = term(81) + r1(vrdav_Rr, a,i) * t1(p,q) * wm_interm_3_pt4(a, i)
end do 
end do 

term(41) = term(41) * (-4.0d+0) 
term(42) = term(42) * 8.0d+0 
term(43) = term(43) * (-4.0d+0) 
term(44) = term(44) * 8.0d+0 
term(45) = term(45) * (-4.0d+0) 
term(46) = term(46) * 8.0d+0 
term(47) = term(47) * 8.0d+0 
term(48) = term(48) * (-16.0d+0) 
term(49) = term(49) * 8.0d+0 
term(50) = term(50) * (-16.0d+0) 
term(51) = term(51) * 8.0d+0 
term(52) = term(52) * (-16.0d+0) 
term(53) = term(53) * (-4.0d+0) 
term(54) = term(54) * 8.0d+0 
term(55) = term(55) * (-16.0d+0) 
term(56) = term(56) * 8.0d+0 
term(57) = term(57) * (-4.0d+0) 
term(58) = term(58) * 8.0d+0 
term(59) = term(59) * (-16.0d+0) 
term(60) = term(60) * 8.0d+0 
term(61) = term(61) * 4.0d+0 
term(62) = term(62) * (-8.0d+0) 
term(63) = term(63) * 16.0d+0 
term(64) = term(64) * (-8.0d+0) 
term(65) = term(65) * 4.0d+0 
term(66) = term(66) * (-8.0d+0) 
term(67) = term(67) * 16.0d+0 
term(68) = term(68) * 4.0d+0 
term(69) = term(69) * (-8.0d+0) 
term(70) = term(70) * (-8.0d+0) 
term(71) = term(71) * (-8.0d+0) 
term(72) = term(72) * 4.0d+0 
term(73) = term(73) * 4.0d+0 
term(74) = term(74) * (-8.0d+0) 
term(75) = term(75) * 4.0d+0 
term(76) = term(76) * 8.0d+0 
term(77) = term(77) * (-4.0d+0) 
term(78) = term(78) * (-2.0d+0) 
term(80) = term(80) * 8.0d+0 
term(81) = term(81) * (-4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
term(82) = term(82) + wm_interm_39_pt4(p, i, q, j) * wm_interm_5_pt4(j, i)
term(83) = term(83) + wm_interm_39_pt4(p, i, j, q) * wm_interm_5_pt4(j, i)
term(84) = term(84) + wm_interm_34_pt4(i, j) * wm_interm_9_pt4(p, i, q, j)
term(85) = term(85) + wm_interm_34_pt4(i, j) * wm_interm_46_pt4(p, i, q, j)
term(86) = term(86) + wm_interm_51_pt4(p, q, i, j) * wm_interm_5_pt4(j, i)
term(87) = term(87) + wm_interm_49_pt4(p, q, i, j) * wm_interm_5_pt4(j, i)
term(88) = term(88) + wm_interm_25_pt4(i, j) * wm_interm_8_pt4(p, i, q, j)
term(89) = term(89) + wm_interm_25_pt4(i, j) * wm_interm_8_pt4(p, q, i, j)
term(90) = term(90) + wm_interm_51_pt4(p, i, q, j) * wm_interm_5_pt4(j, i)
term(91) = term(91) + wm_interm_49_pt4(p, i, q, j) * wm_interm_5_pt4(j, i)
term(92) = term(92) + wm_interm_13_pt4(p, q, i, j) * wm_interm_25_pt4(i, j)
term(93) = term(93) + wm_interm_13_pt4(p, i, q, j) * wm_interm_25_pt4(i, j)
term(94) = term(94) + s1(p,i) * wm_interm_16_pt4(j, i) * wm_interm_28_pt4(q, j)
term(95) = term(95) + s1(p,q) * wm_interm_16_pt4(i, j) * wm_interm_28_pt4(j, i)
term(96) = term(96) + s1(p,i) * wm_interm_16_pt4(j, i) * wm_interm_29_pt4(q, j)
term(97) = term(97) + s1(p,q) * wm_interm_16_pt4(i, j) * wm_interm_29_pt4(j, i)
term(98) = term(98) + r1(vrdav_Rr, p,i) * wm_interm_28_pt4(i, j) * wm_interm_34_pt4(j, q)
term(99) = term(99) + r1(vrdav_Rr, p,i) * wm_interm_29_pt4(i, j) * wm_interm_34_pt4(j, q)
term(100) = term(100) + t1(p,q) * wm_interm_16_pt4(i, j) * wm_interm_28_pt4(j, i)
term(101) = term(101) + t1(p,q) * wm_interm_16_pt4(i, j) * wm_interm_29_pt4(j, i)
term(102) = term(102) + r1(vrdav_Rr, p,i) * wm_interm_29_pt4(j, q) * wm_interm_34_pt4(i, j)
term(103) = term(103) + r1(vrdav_Rr, p,i) * wm_interm_28_pt4(j, q) * wm_interm_34_pt4(i, j)
term(104) = term(104) + t1(p,i) * wm_interm_16_pt4(i, j) * wm_interm_28_pt4(j, q)
term(105) = term(105) + t1(p,i) * wm_interm_16_pt4(i, j) * wm_interm_29_pt4(j, q)
end do 
end do 

term(82) = term(82) * 2.0d+0 
term(83) = term(83) * (-4.0d+0) 
term(84) = term(84) * 2.0d+0 
term(85) = term(85) * (-4.0d+0) 
term(86) = term(86) * (-2.0d+0) 
term(89) = term(89) * (-2.0d+0) 
term(91) = term(91) * (-2.0d+0) 
term(93) = term(93) * (-2.0d+0) 
term(94) = term(94) * 2.0d+0 
term(95) = term(95) * (-4.0d+0) 
term(96) = term(96) * (-4.0d+0) 
term(97) = term(97) * 8.0d+0 
term(98) = term(98) * 2.0d+0 
term(99) = term(99) * (-4.0d+0) 
term(100) = term(100) * (-4.0d+0) 
term(101) = term(101) * 8.0d+0 
term(102) = term(102) * (-4.0d+0) 
term(103) = term(103) * 2.0d+0 
term(104) = term(104) * 4.0d+0 
term(105) = term(105) * (-8.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(106) = term(106) + s2(a,p,q,i) * wm_interm_16_pt4(j, i) * wm_interm_17_pt4(a, j)
term(107) = term(107) + s2(a,p,q,i) * wm_interm_16_pt4(j, i) * wm_interm_23_pt4(a, j)
term(108) = term(108) + s2(a,p,q,i) * wm_interm_22_pt4(a, j) * wm_interm_25_pt4(i, j)
term(109) = term(109) + s1(p,i) * wm_interm_19_pt4(a, j) * wm_interm_26_pt4(a, q, j, i)
term(110) = term(110) + s1(p,i) * wm_interm_21_pt4(a, j) * wm_interm_26_pt4(a, q, j, i)
term(111) = term(111) + s2(a,p,q,i) * wm_interm_25_pt4(i, j) * wm_interm_27_pt4(a, j)
term(112) = term(112) + s2(a,p,j,i) * wm_interm_22_pt4(a, j) * wm_interm_25_pt4(i, q)
term(113) = term(113) + s2(a,p,j,i) * wm_interm_25_pt4(i, q) * wm_interm_27_pt4(a, j)
term(114) = term(114) + r1(vrdav_Rr, p,i) * wm_interm_22_pt4(a, j) * wm_interm_35_pt4(a, j, i, q)
term(115) = term(115) + r1(vrdav_Rr, p,i) * wm_interm_27_pt4(a, j) * wm_interm_35_pt4(a, j, i, q)
term(116) = term(116) + t2(a,p,q,i) * wm_interm_19_pt4(a, j) * wm_interm_34_pt4(i, j)
term(117) = term(117) + t2(a,p,q,i) * wm_interm_21_pt4(a, j) * wm_interm_34_pt4(i, j)
term(118) = term(118) + t2(a,p,j,i) * wm_interm_19_pt4(a, i) * wm_interm_34_pt4(j, q)
term(119) = term(119) + t2(a,p,j,i) * wm_interm_21_pt4(a, i) * wm_interm_34_pt4(j, q)
term(120) = term(120) + t2(a,p,j,i) * wm_interm_21_pt4(a, j) * wm_interm_34_pt4(i, q)
term(121) = term(121) + t2(a,p,j,i) * wm_interm_19_pt4(a, j) * wm_interm_34_pt4(i, q)
term(122) = term(122) + r1(vrdav_Rl, a,j) * s1(p,i) * wm_interm_9_pt4(a, j, i, q)
term(123) = term(123) + r1(vrdav_Rr, p,i) * t1(a,j) * wm_interm_8_pt4(a, j, i, q)
term(124) = term(124) + r1(vrdav_Rr, a,j) * t1(p,i) * wm_interm_13_pt4(a, j, i, q)
term(125) = term(125) + r1(vrdav_Rr, a,j) * t1(p,i) * wm_interm_8_pt4(a, j, i, q)
end do 
end do 
end do 

term(106) = term(106) * (-4.0d+0) 
term(107) = term(107) * 8.0d+0 
term(108) = term(108) * (-4.0d+0) 
term(109) = term(109) * (-4.0d+0) 
term(110) = term(110) * 8.0d+0 
term(111) = term(111) * 8.0d+0 
term(112) = term(112) * (-4.0d+0) 
term(113) = term(113) * 8.0d+0 
term(114) = term(114) * (-4.0d+0) 
term(115) = term(115) * 8.0d+0 
term(116) = term(116) * 2.0d+0 
term(117) = term(117) * (-4.0d+0) 
term(118) = term(118) * 4.0d+0 
term(119) = term(119) * (-8.0d+0) 
term(120) = term(120) * 16.0d+0 
term(121) = term(121) * (-8.0d+0) 
term(122) = term(122) * 8.0d+0 
term(123) = term(123) * (-2.0d+0) 
term(124) = term(124) * 2.0d+0 
term(125) = term(125) * (-4.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
term(126) = term(126) + s1(p,i) * wm_interm_16_pt4(j, k) * wm_interm_24_pt4(q, k, j, i)
term(127) = term(127) + s1(p,i) * wm_interm_16_pt4(j, k) * wm_interm_24_pt4(k, q, j, i)
end do 
end do 
end do 

term(126) = term(126) * (-4.0d+0) 
term(127) = term(127) * 8.0d+0 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(128) = term(128) + s1(a,i) * wm_interm_20_pt4(p, i, j, k) * wm_interm_26_pt4(a, q, j, k)
term(129) = term(129) + s1(a,q) * wm_interm_20_pt4(p, i, j, k) * wm_interm_26_pt4(a, i, j, k)
term(130) = term(130) + s1(a,q) * wm_interm_20_pt4(p, i, j, k) * wm_interm_26_pt4(a, i, k, j)
term(131) = term(131) + s1(a,i) * wm_interm_20_pt4(p, i, j, k) * wm_interm_26_pt4(a, q, k, j)
term(132) = term(132) + r1(vrdav_Rr, p,i) * wm_interm_26_pt4(a, j, q, k) * wm_interm_37_pt4(a, k, i, j)
term(133) = term(133) + r1(vrdav_Rr, p,i) * wm_interm_26_pt4(a, j, q, k) * wm_interm_37_pt4(a, i, k, j)
term(134) = term(134) + t1(a,q) * wm_interm_20_pt4(a, i, j, k) * wm_interm_26_pt4(p, i, j, k)
term(135) = term(135) + t1(a,q) * wm_interm_20_pt4(a, i, j, k) * wm_interm_26_pt4(p, i, k, j)
end do 
end do 
end do 
end do 

term(128) = term(128) * (-4.0d+0) 
term(129) = term(129) * 8.0d+0 
term(130) = term(130) * (-4.0d+0) 
term(131) = term(131) * 8.0d+0 
term(132) = term(132) * 2.0d+0 
term(133) = term(133) * (-4.0d+0) 
term(134) = term(134) * (-8.0d+0) 
term(135) = term(135) * 4.0d+0 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(136) = term(136) + r1(vrdav_Rr, p,i) * wm_interm_24_pt4(i, j, k, q) * wm_interm_34_pt4(k, j)
term(137) = term(137) + r1(vrdav_Rr, p,i) * wm_interm_24_pt4(j, i, k, q) * wm_interm_34_pt4(k, j)
end do 
end do 
end do 

term(136) = term(136) * 2.0d+0 
term(137) = term(137) * (-4.0d+0) 

do a = nocc + 1, nactive 
term(138) = term(138) + t1(a,q) * wm_interm_31_pt4(a, p) * wm_interm_38_pt4
term(139) = term(139) + t1(a,q) * wm_interm_33_pt4(a, p) * wm_interm_38_pt4
term(140) = term(140) + wm_interm_11_pt4(p, a) * wm_interm_53_pt4(a, q)
term(141) = term(141) + wm_interm_11_pt4(p, a) * wm_interm_50_pt4(a, q)
term(142) = term(142) + wm_interm_0_pt4(a, q) * wm_interm_30_pt4(a, p)
term(143) = term(143) + wm_interm_30_pt4(a, p) * wm_interm_3_pt4(a, q)
end do 

term(138) = term(138) * 16.0d+0 
term(139) = term(139) * (-8.0d+0) 
term(140) = term(140) * 2.0d+0 
term(141) = term(141) * (-4.0d+0) 
term(142) = term(142) * (-4.0d+0) 
term(143) = term(143) * 2.0d+0 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(144) = term(144) + s2(a,p,i,q) * wm_interm_16_pt4(j, i) * wm_interm_17_pt4(a, j)
term(145) = term(145) + s1(a,i) * wm_interm_20_pt4(p, i, j, q) * wm_interm_22_pt4(a, j)
term(146) = term(146) + s1(a,i) * wm_interm_20_pt4(p, i, q, j) * wm_interm_22_pt4(a, j)
term(147) = term(147) + s2(a,p,i,q) * wm_interm_16_pt4(j, i) * wm_interm_23_pt4(a, j)
term(148) = term(148) + s2(a,p,i,q) * wm_interm_22_pt4(a, j) * wm_interm_25_pt4(i, j)
term(149) = term(149) + s1(p,i) * wm_interm_21_pt4(a, j) * wm_interm_26_pt4(a, q, i, j)
term(150) = term(150) + r1(vrdav_Rl, a,i) * wm_interm_17_pt4(a, j) * wm_interm_20_pt4(p, i, j, q)
term(151) = term(151) + r1(vrdav_Rl, a,i) * wm_interm_17_pt4(a, j) * wm_interm_20_pt4(p, i, q, j)
term(152) = term(152) + s1(p,i) * wm_interm_19_pt4(a, j) * wm_interm_26_pt4(a, q, i, j)
term(153) = term(153) + r1(vrdav_Rl, a,i) * wm_interm_20_pt4(p, i, j, q) * wm_interm_23_pt4(a, j)
term(154) = term(154) + r1(vrdav_Rl, a,i) * wm_interm_20_pt4(p, i, q, j) * wm_interm_23_pt4(a, j)
term(155) = term(155) + s1(a,i) * wm_interm_20_pt4(p, i, j, q) * wm_interm_27_pt4(a, j)
term(156) = term(156) + s1(a,i) * wm_interm_20_pt4(p, i, q, j) * wm_interm_27_pt4(a, j)
term(157) = term(157) + s2(a,p,i,q) * wm_interm_25_pt4(i, j) * wm_interm_27_pt4(a, j)
term(158) = term(158) + s1(p,i) * wm_interm_20_pt4(a, i, j, q) * wm_interm_22_pt4(a, j)
term(159) = term(159) + s1(p,i) * wm_interm_20_pt4(a, i, q, j) * wm_interm_22_pt4(a, j)
term(160) = term(160) + s1(p,i) * wm_interm_20_pt4(a, i, q, j) * wm_interm_27_pt4(a, j)
term(161) = term(161) + s1(p,i) * wm_interm_20_pt4(a, i, j, q) * wm_interm_27_pt4(a, j)
term(162) = term(162) + s2(a,p,i,j) * wm_interm_22_pt4(a, j) * wm_interm_25_pt4(i, q)
term(163) = term(163) + s2(a,p,i,j) * wm_interm_25_pt4(i, q) * wm_interm_27_pt4(a, j)
term(164) = term(164) + r1(vrdav_Rr, p,i) * wm_interm_22_pt4(a, j) * wm_interm_35_pt4(a, i, j, q)
term(165) = term(165) + r1(vrdav_Rr, p,i) * wm_interm_27_pt4(a, j) * wm_interm_35_pt4(a, i, j, q)
term(166) = term(166) + t1(a,i) * wm_interm_19_pt4(a, j) * wm_interm_26_pt4(p, i, q, j)
term(167) = term(167) + t1(a,i) * wm_interm_21_pt4(a, j) * wm_interm_26_pt4(p, i, q, j)
term(168) = term(168) + t1(a,i) * wm_interm_19_pt4(a, j) * wm_interm_26_pt4(p, i, j, q)
term(169) = term(169) + t1(a,i) * wm_interm_21_pt4(a, j) * wm_interm_26_pt4(p, i, j, q)
term(170) = term(170) + t2(a,p,i,q) * wm_interm_19_pt4(a, j) * wm_interm_34_pt4(i, j)
term(171) = term(171) + t2(a,p,i,q) * wm_interm_21_pt4(a, j) * wm_interm_34_pt4(i, j)
term(172) = term(172) + t1(p,i) * wm_interm_19_pt4(a, j) * wm_interm_26_pt4(a, i, j, q)
term(173) = term(173) + t1(p,i) * wm_interm_21_pt4(a, j) * wm_interm_26_pt4(a, i, j, q)
term(174) = term(174) + t1(p,i) * wm_interm_21_pt4(a, j) * wm_interm_26_pt4(a, i, q, j)
term(175) = term(175) + t1(p,i) * wm_interm_19_pt4(a, j) * wm_interm_26_pt4(a, i, q, j)
term(176) = term(176) + r1(vrdav_Rl, p,i) * s1(a,j) * wm_interm_9_pt4(a, i, j, q)
term(177) = term(177) + r1(vrdav_Rl, a,j) * s1(p,i) * wm_interm_9_pt4(a, i, j, q)
term(178) = term(178) + r1(vrdav_Rr, p,i) * t1(a,j) * wm_interm_8_pt4(a, i, j, q)
term(179) = term(179) + r1(vrdav_Rr, a,j) * t1(p,i) * wm_interm_13_pt4(a, i, j, q)
term(180) = term(180) + r1(vrdav_Rr, a,j) * t1(p,i) * wm_interm_8_pt4(a, i, j, q)
end do 
end do 
end do 

term(144) = term(144) * 8.0d+0 
term(145) = term(145) * 8.0d+0 
term(146) = term(146) * (-4.0d+0) 
term(147) = term(147) * (-16.0d+0) 
term(148) = term(148) * 8.0d+0 
term(149) = term(149) * (-16.0d+0) 
term(150) = term(150) * 8.0d+0 
term(151) = term(151) * (-4.0d+0) 
term(152) = term(152) * 8.0d+0 
term(153) = term(153) * (-16.0d+0) 
term(154) = term(154) * 8.0d+0 
term(155) = term(155) * (-16.0d+0) 
term(156) = term(156) * 8.0d+0 
term(157) = term(157) * (-16.0d+0) 
term(158) = term(158) * 2.0d+0 
term(159) = term(159) * (-4.0d+0) 
term(160) = term(160) * 8.0d+0 
term(161) = term(161) * (-4.0d+0) 
term(162) = term(162) * 2.0d+0 
term(163) = term(163) * (-4.0d+0) 
term(164) = term(164) * 2.0d+0 
term(165) = term(165) * (-4.0d+0) 
term(166) = term(166) * 2.0d+0 
term(167) = term(167) * (-4.0d+0) 
term(168) = term(168) * (-4.0d+0) 
term(169) = term(169) * 8.0d+0 
term(170) = term(170) * (-4.0d+0) 
term(171) = term(171) * 8.0d+0 
term(172) = term(172) * 4.0d+0 
term(173) = term(173) * (-8.0d+0) 
term(174) = term(174) * 16.0d+0 
term(175) = term(175) * (-8.0d+0) 
term(176) = term(176) * (-2.0d+0) 
term(177) = term(177) * (-4.0d+0) 
term(179) = term(179) * (-4.0d+0) 
term(180) = term(180) * 2.0d+0 

do i = 1, nocc 
term(181) = term(181) + t1(p,i) * wm_interm_29_pt4(i, q) * wm_interm_38_pt4
term(182) = term(182) + t1(p,i) * wm_interm_28_pt4(i, q) * wm_interm_38_pt4
term(183) = term(183) + wm_interm_1_pt4(p, i) * wm_interm_34_pt4(i, q)
term(184) = term(184) + wm_interm_2_pt4(p, i) * wm_interm_34_pt4(i, q)
term(185) = term(185) + wm_interm_48_pt4(p, i) * wm_interm_5_pt4(i, q)
term(186) = term(186) + wm_interm_47_pt4(p, i) * wm_interm_5_pt4(i, q)
end do 

term(181) = term(181) * 16.0d+0 
term(182) = term(182) * (-8.0d+0) 
term(183) = term(183) * 4.0d+0 
term(184) = term(184) * (-2.0d+0) 
term(185) = term(185) * 2.0d+0 
term(186) = term(186) * (-4.0d+0) 

do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
term(187) = term(187) + t1(p,i) * wm_interm_16_pt4(j, k) * wm_interm_24_pt4(k, i, j, q)
term(188) = term(188) + t1(p,i) * wm_interm_16_pt4(j, k) * wm_interm_24_pt4(i, k, j, q)
end do 
end do 
end do 

term(187) = term(187) * (-8.0d+0) 
term(188) = term(188) * 4.0d+0 

do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(189) = term(189) + r1(vrdav_Rl, b,q) * wm_interm_30_pt4(a, p) * wm_interm_31_pt4(a, b)
term(190) = term(190) + s1(p,q) * wm_interm_31_pt4(a, b) * wm_interm_32_pt4(b, a)
term(191) = term(191) + r1(vrdav_Rl, b,q) * wm_interm_30_pt4(a, p) * wm_interm_33_pt4(a, b)
term(192) = term(192) + s1(p,q) * wm_interm_32_pt4(a, b) * wm_interm_33_pt4(b, a)
term(193) = term(193) + t1(p,q) * wm_interm_31_pt4(a, b) * wm_interm_32_pt4(b, a)
term(194) = term(194) + t1(p,q) * wm_interm_32_pt4(a, b) * wm_interm_33_pt4(b, a)
end do 
end do 

term(189) = term(189) * (-4.0d+0) 
term(190) = term(190) * 8.0d+0 
term(191) = term(191) * 2.0d+0 
term(192) = term(192) * (-4.0d+0) 
term(193) = term(193) * 8.0d+0 
term(194) = term(194) * (-4.0d+0) 

do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(195) = term(195) + r1(vrdav_Rr, p,i) * wm_interm_26_pt4(a, j, k, q) * wm_interm_37_pt4(a, i, k, j)
term(196) = term(196) + r1(vrdav_Rr, p,i) * wm_interm_26_pt4(a, j, k, q) * wm_interm_37_pt4(a, k, i, j)
term(197) = term(197) + t2(a,p,i,q) * wm_interm_16_pt4(j, k) * wm_interm_37_pt4(a, i, k, j)
end do 
end do 
end do 
end do 

term(195) = term(195) * 2.0d+0 
term(196) = term(196) * (-4.0d+0) 
term(197) = term(197) * (-4.0d+0) 


    calc_D_vo_wm_pt4 = zero
    do s = 0, 197
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
    double precision, dimension(:), intent(in) :: vrdav_Rl
    double precision, dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , k, j, i, a, b, l 
    real(F64), dimension(0:592) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(0) = term(0) + wm_interm_8_pt4(q, i, j, k) * wm_interm_9_pt4(p, j, i, k)
term(1) = term(1) + wm_interm_8_pt4(q, i, j, k) * wm_interm_9_pt4(p, i, j, k)
term(2) = term(2) + wm_interm_13_pt4(q, i, j, k) * wm_interm_9_pt4(p, i, j, k)
term(3) = term(3) + wm_interm_13_pt4(q, i, j, k) * wm_interm_9_pt4(p, j, i, k)
term(4) = term(4) + r1(vrdav_Rr, p,i) * wm_interm_35_pt4(q, i, j, k) * wm_interm_52_pt4(k, j)
term(5) = term(5) + r1(vrdav_Rr, p,i) * wm_interm_35_pt4(q, i, j, k) * wm_interm_55_pt4(k, j)
term(6) = term(6) + r1(vrdav_Rr, p,i) * wm_interm_13_pt4(q, i, j, k) * wm_interm_28_pt4(k, j)
term(7) = term(7) + r1(vrdav_Rr, p,i) * wm_interm_13_pt4(q, i, j, k) * wm_interm_29_pt4(k, j)
end do 
end do 
end do 

term(1) = term(1) * (-2.0d+0) 
term(3) = term(3) * (-2.0d+0) 
term(4) = term(4) * (-8.0d+0) 
term(5) = term(5) * 16.0d+0 
term(6) = term(6) * 2.0d+0 
term(7) = term(7) * (-4.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
do j = 1, nocc 
do b = nocc + 1, nactive 
term(8) = term(8) + r1(vrdav_Rl, a,i) * r2(vrdav_Rr, b,j,p,i) * s1(q,j) * wm_interm_31_pt4(b, a)
term(9) = term(9) + r1(vrdav_Rl, a,j) * r2(vrdav_Rr, b,j,p,i) * s1(q,i) * wm_interm_31_pt4(b, a)
term(10) = term(10) + r1(vrdav_Rl, a,i) * r2(vrdav_Rr, b,j,p,i) * s1(q,j) * wm_interm_33_pt4(b, a)
term(11) = term(11) + r1(vrdav_Rl, a,j) * r2(vrdav_Rr, b,j,p,i) * s1(q,i) * wm_interm_33_pt4(b, a)
term(12) = term(12) + r2(vrdav_Rl, a,j,q,i) * r1(vrdav_Rr, p,i) * t1(b,j) * wm_interm_31_pt4(b, a)
term(13) = term(13) + r2(vrdav_Rl, a,j,q,i) * r1(vrdav_Rr, p,i) * t1(b,j) * wm_interm_33_pt4(b, a)
term(14) = term(14) + r1(vrdav_Rr, p,i) * s2(a,q,j,i) * t1(b,j) * wm_interm_56_pt4(b, a)
term(15) = term(15) + r1(vrdav_Rr, p,i) * s2(a,q,j,i) * t1(b,j) * wm_interm_57_pt4(b, a)
term(16) = term(16) + r2(vrdav_Rl, a,j,p,i) * r1(vrdav_Rr, b,i) * t1(q,j) * wm_interm_31_pt4(b, a)
term(17) = term(17) + r2(vrdav_Rl, a,j,p,i) * r1(vrdav_Rr, b,j) * t1(q,i) * wm_interm_31_pt4(b, a)
term(18) = term(18) + r2(vrdav_Rl, a,j,p,i) * r1(vrdav_Rr, b,i) * t1(q,j) * wm_interm_33_pt4(b, a)
term(19) = term(19) + r2(vrdav_Rl, a,j,p,i) * r1(vrdav_Rr, b,j) * t1(q,i) * wm_interm_33_pt4(b, a)
end do 
end do 
end do 
end do 

term(8) = term(8) * (-4.0d+0) 
term(9) = term(9) * 8.0d+0 
term(10) = term(10) * 2.0d+0 
term(11) = term(11) * (-4.0d+0) 
term(12) = term(12) * 16.0d+0 
term(13) = term(13) * (-8.0d+0) 
term(14) = term(14) * (-8.0d+0) 
term(15) = term(15) * 16.0d+0 
term(16) = term(16) * (-4.0d+0) 
term(17) = term(17) * 8.0d+0 
term(18) = term(18) * 2.0d+0 
term(19) = term(19) * (-4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(20) = term(20) + s2(a,q,i,j) * wm_interm_1_pt4(p, i) * wm_interm_22_pt4(a, j)
term(21) = term(21) + s2(a,q,i,j) * wm_interm_22_pt4(a, j) * wm_interm_2_pt4(p, i)
term(22) = term(22) + s2(a,q,i,j) * wm_interm_1_pt4(p, i) * wm_interm_27_pt4(a, j)
term(23) = term(23) + s2(a,q,i,j) * wm_interm_27_pt4(a, j) * wm_interm_2_pt4(p, i)
term(24) = term(24) + s2(a,p,i,j) * wm_interm_1_pt4(a, j) * wm_interm_22_pt4(q, i)
term(25) = term(25) + s2(a,p,i,j) * wm_interm_22_pt4(q, i) * wm_interm_2_pt4(a, j)
term(26) = term(26) + s2(a,p,i,j) * wm_interm_1_pt4(a, j) * wm_interm_27_pt4(q, i)
term(27) = term(27) + s2(a,p,i,j) * wm_interm_27_pt4(q, i) * wm_interm_2_pt4(a, j)
term(28) = term(28) + s2(a,p,i,j) * wm_interm_17_pt4(q, i) * wm_interm_48_pt4(a, j)
term(29) = term(29) + s2(a,p,i,j) * wm_interm_17_pt4(q, i) * wm_interm_47_pt4(a, j)
term(30) = term(30) + s2(a,p,i,j) * wm_interm_23_pt4(q, i) * wm_interm_48_pt4(a, j)
term(31) = term(31) + s2(a,p,i,j) * wm_interm_23_pt4(q, i) * wm_interm_47_pt4(a, j)
term(32) = term(32) + t2(a,q,i,j) * wm_interm_0_pt4(a, j) * wm_interm_19_pt4(p, i)
term(33) = term(33) + t2(a,q,i,j) * wm_interm_0_pt4(a, j) * wm_interm_21_pt4(p, i)
term(34) = term(34) + t2(a,q,i,j) * wm_interm_19_pt4(p, i) * wm_interm_3_pt4(a, j)
term(35) = term(35) + t2(a,q,i,j) * wm_interm_21_pt4(p, i) * wm_interm_3_pt4(a, j)
term(36) = term(36) + t2(a,q,i,j) * wm_interm_0_pt4(p, i) * wm_interm_19_pt4(a, j)
term(37) = term(37) + t2(a,q,i,j) * wm_interm_0_pt4(p, i) * wm_interm_21_pt4(a, j)
term(38) = term(38) + t2(a,q,i,j) * wm_interm_19_pt4(a, j) * wm_interm_3_pt4(p, i)
term(39) = term(39) + t2(a,q,i,j) * wm_interm_21_pt4(a, j) * wm_interm_3_pt4(p, i)
end do 
end do 
end do 

term(20) = term(20) * 4.0d+0 
term(21) = term(21) * (-2.0d+0) 
term(22) = term(22) * (-8.0d+0) 
term(23) = term(23) * 4.0d+0 
term(24) = term(24) * 4.0d+0 
term(25) = term(25) * (-2.0d+0) 
term(26) = term(26) * (-8.0d+0) 
term(27) = term(27) * 4.0d+0 
term(28) = term(28) * (-4.0d+0) 
term(29) = term(29) * 8.0d+0 
term(30) = term(30) * 8.0d+0 
term(31) = term(31) * (-16.0d+0) 
term(32) = term(32) * (-4.0d+0) 
term(33) = term(33) * 8.0d+0 
term(34) = term(34) * 2.0d+0 
term(35) = term(35) * (-4.0d+0) 
term(36) = term(36) * (-4.0d+0) 
term(37) = term(37) * 8.0d+0 
term(38) = term(38) * 2.0d+0 
term(39) = term(39) * (-4.0d+0) 

do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(40) = term(40) + r1(vrdav_Rl, a,j) * s1(p,i) * t2(b,q,i,j) * wm_interm_40_pt4(a, b)
term(41) = term(41) + r1(vrdav_Rl, a,j) * s1(p,i) * t2(b,q,i,j) * wm_interm_41_pt4(a, b)
term(42) = term(42) + r1(vrdav_Rl, p,i) * s1(a,j) * t2(b,q,i,j) * wm_interm_40_pt4(a, b)
term(43) = term(43) + r1(vrdav_Rl, p,i) * s1(a,j) * t2(b,q,i,j) * wm_interm_41_pt4(a, b)
term(44) = term(44) + r1(vrdav_Rr, a,j) * s2(b,p,i,j) * t1(q,i) * wm_interm_56_pt4(a, b)
term(45) = term(45) + r1(vrdav_Rr, a,j) * s2(b,p,i,j) * t1(q,i) * wm_interm_57_pt4(a, b)
end do 
end do 
end do 
end do 

term(40) = term(40) * (-4.0d+0) 
term(41) = term(41) * 8.0d+0 
term(42) = term(42) * (-4.0d+0) 
term(43) = term(43) * 8.0d+0 
term(44) = term(44) * 2.0d+0 
term(45) = term(45) * (-4.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(46) = term(46) + r1(vrdav_Rl, a,j) * s1(p,i) * t2(b,q,j,i) * wm_interm_40_pt4(a, b)
term(47) = term(47) + r1(vrdav_Rl, a,j) * s1(p,i) * t2(b,q,j,i) * wm_interm_41_pt4(a, b)
term(48) = term(48) + r1(vrdav_Rl, p,i) * s1(a,j) * t2(b,q,j,i) * wm_interm_40_pt4(a, b)
term(49) = term(49) + r1(vrdav_Rl, p,i) * s1(a,j) * t2(b,q,j,i) * wm_interm_41_pt4(a, b)
term(50) = term(50) + r1(vrdav_Rr, a,j) * s2(b,p,j,i) * t1(q,i) * wm_interm_56_pt4(a, b)
term(51) = term(51) + r1(vrdav_Rr, a,j) * s2(b,p,j,i) * t1(q,i) * wm_interm_57_pt4(a, b)
end do 
end do 
end do 
end do 

term(46) = term(46) * 8.0d+0 
term(47) = term(47) * (-16.0d+0) 
term(48) = term(48) * 8.0d+0 
term(49) = term(49) * (-16.0d+0) 
term(50) = term(50) * (-4.0d+0) 
term(51) = term(51) * 8.0d+0 

do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
term(52) = term(52) + s1(q,i) * wm_interm_28_pt4(j, k) * wm_interm_39_pt4(p, k, i, j)
term(53) = term(53) + s1(q,i) * wm_interm_29_pt4(j, k) * wm_interm_39_pt4(p, k, i, j)
term(54) = term(54) + r1(vrdav_Rr, p,i) * wm_interm_29_pt4(j, k) * wm_interm_8_pt4(q, k, i, j)
term(55) = term(55) + r1(vrdav_Rr, p,i) * wm_interm_28_pt4(j, k) * wm_interm_8_pt4(q, k, i, j)
term(56) = term(56) + t1(q,i) * wm_interm_28_pt4(j, k) * wm_interm_51_pt4(p, k, i, j)
term(57) = term(57) + t1(q,i) * wm_interm_28_pt4(j, k) * wm_interm_49_pt4(p, k, i, j)
term(58) = term(58) + t1(q,i) * wm_interm_29_pt4(j, k) * wm_interm_51_pt4(p, k, i, j)
term(59) = term(59) + t1(q,i) * wm_interm_29_pt4(j, k) * wm_interm_49_pt4(p, k, i, j)
end do 
end do 
end do 

term(52) = term(52) * 2.0d+0 
term(53) = term(53) * (-4.0d+0) 
term(54) = term(54) * (-4.0d+0) 
term(55) = term(55) * 2.0d+0 
term(57) = term(57) * (-2.0d+0) 
term(58) = term(58) * (-2.0d+0) 
term(59) = term(59) * 4.0d+0 

do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
term(60) = term(60) + s1(q,i) * wm_interm_28_pt4(j, k) * wm_interm_39_pt4(p, k, j, i)
term(61) = term(61) + s1(q,i) * wm_interm_29_pt4(j, k) * wm_interm_39_pt4(p, k, j, i)
end do 
end do 
end do 

term(60) = term(60) * (-4.0d+0) 
term(61) = term(61) * 8.0d+0 

do a = nocc + 1, nactive 
do i = 1, nocc 
term(62) = term(62) + r1(vrdav_Rl, a,i) * wm_interm_1_pt4(p, i) * wm_interm_31_pt4(q, a)
term(63) = term(63) + r1(vrdav_Rl, a,i) * wm_interm_1_pt4(p, i) * wm_interm_33_pt4(q, a)
term(64) = term(64) + r1(vrdav_Rl, a,i) * wm_interm_2_pt4(p, i) * wm_interm_31_pt4(q, a)
term(65) = term(65) + r1(vrdav_Rl, a,i) * wm_interm_2_pt4(p, i) * wm_interm_33_pt4(q, a)
term(66) = term(66) + r1(vrdav_Rl, q,i) * wm_interm_17_pt4(a, i) * wm_interm_40_pt4(p, a)
term(67) = term(67) + r1(vrdav_Rl, q,i) * wm_interm_17_pt4(a, i) * wm_interm_41_pt4(p, a)
term(68) = term(68) + r1(vrdav_Rl, q,i) * wm_interm_23_pt4(a, i) * wm_interm_40_pt4(p, a)
term(69) = term(69) + r1(vrdav_Rl, q,i) * wm_interm_23_pt4(a, i) * wm_interm_41_pt4(p, a)
term(70) = term(70) + s1(q,i) * wm_interm_22_pt4(a, i) * wm_interm_40_pt4(p, a)
term(71) = term(71) + s1(q,i) * wm_interm_22_pt4(a, i) * wm_interm_41_pt4(p, a)
term(72) = term(72) + s1(q,i) * wm_interm_27_pt4(a, i) * wm_interm_40_pt4(p, a)
term(73) = term(73) + s1(q,i) * wm_interm_27_pt4(a, i) * wm_interm_41_pt4(p, a)
term(74) = term(74) + r1(vrdav_Rr, p,i) * wm_interm_0_pt4(a, i) * wm_interm_31_pt4(q, a)
term(75) = term(75) + r1(vrdav_Rr, p,i) * wm_interm_0_pt4(a, i) * wm_interm_33_pt4(q, a)
term(76) = term(76) + r1(vrdav_Rr, p,i) * wm_interm_31_pt4(q, a) * wm_interm_3_pt4(a, i)
term(77) = term(77) + r1(vrdav_Rr, p,i) * wm_interm_33_pt4(q, a) * wm_interm_3_pt4(a, i)
term(78) = term(78) + t1(q,i) * wm_interm_31_pt4(p, a) * wm_interm_53_pt4(a, i)
term(79) = term(79) + t1(q,i) * wm_interm_33_pt4(p, a) * wm_interm_53_pt4(a, i)
term(80) = term(80) + t1(q,i) * wm_interm_31_pt4(p, a) * wm_interm_50_pt4(a, i)
term(81) = term(81) + t1(q,i) * wm_interm_33_pt4(p, a) * wm_interm_50_pt4(a, i)
term(82) = term(82) + t1(q,i) * wm_interm_19_pt4(a, i) * wm_interm_56_pt4(p, a)
term(83) = term(83) + t1(q,i) * wm_interm_19_pt4(a, i) * wm_interm_57_pt4(p, a)
term(84) = term(84) + t1(q,i) * wm_interm_21_pt4(a, i) * wm_interm_56_pt4(p, a)
term(85) = term(85) + t1(q,i) * wm_interm_21_pt4(a, i) * wm_interm_57_pt4(p, a)
end do 
end do 

term(62) = term(62) * (-8.0d+0) 
term(63) = term(63) * 4.0d+0 
term(64) = term(64) * 4.0d+0 
term(65) = term(65) * (-2.0d+0) 
term(66) = term(66) * (-4.0d+0) 
term(67) = term(67) * 8.0d+0 
term(68) = term(68) * 8.0d+0 
term(69) = term(69) * (-16.0d+0) 
term(70) = term(70) * (-4.0d+0) 
term(71) = term(71) * 8.0d+0 
term(72) = term(72) * 8.0d+0 
term(73) = term(73) * (-16.0d+0) 
term(74) = term(74) * 16.0d+0 
term(75) = term(75) * (-8.0d+0) 
term(76) = term(76) * (-8.0d+0) 
term(77) = term(77) * 4.0d+0 
term(78) = term(78) * (-4.0d+0) 
term(79) = term(79) * 2.0d+0 
term(80) = term(80) * 8.0d+0 
term(81) = term(81) * (-4.0d+0) 
term(82) = term(82) * 2.0d+0 
term(83) = term(83) * (-4.0d+0) 
term(84) = term(84) * (-4.0d+0) 
term(85) = term(85) * 8.0d+0 

do j = 1, nocc 
do a = nocc + 1, nactive 
do i = 1, nocc 
do b = nocc + 1, nactive 
term(86) = term(86) + r2(vrdav_Rl, a,i,q,j) * r1(vrdav_Rr, p,i) * t1(b,j) * wm_interm_33_pt4(b, a)
term(87) = term(87) + r2(vrdav_Rl, a,i,q,j) * r1(vrdav_Rr, p,i) * t1(b,j) * wm_interm_31_pt4(b, a)
term(88) = term(88) + r1(vrdav_Rr, p,i) * s2(a,q,i,j) * t1(b,j) * wm_interm_56_pt4(b, a)
term(89) = term(89) + r1(vrdav_Rr, p,i) * s2(a,q,i,j) * t1(b,j) * wm_interm_57_pt4(b, a)
end do 
end do 
end do 
end do 

term(86) = term(86) * 4.0d+0 
term(87) = term(87) * (-8.0d+0) 
term(88) = term(88) * 4.0d+0 
term(89) = term(89) * (-8.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(90) = term(90) + r2(vrdav_Rr, a,j,p,i) * s1(q,i) * s2(a,b,k,j) * wm_interm_22_pt4(b, k)
term(91) = term(91) + r2(vrdav_Rr, a,j,p,i) * s1(q,i) * s2(a,b,k,j) * wm_interm_27_pt4(b, k)
term(92) = term(92) + r1(vrdav_Rr, p,i) * s2(a,q,j,i) * t2(a,b,k,j) * wm_interm_0_pt4(b, k)
term(93) = term(93) + r1(vrdav_Rr, p,i) * s2(a,q,j,i) * t2(a,b,k,j) * wm_interm_3_pt4(b, k)
term(94) = term(94) + r2(vrdav_Rl, a,j,p,i) * t1(q,i) * t2(a,b,k,j) * wm_interm_19_pt4(b, k)
term(95) = term(95) + r2(vrdav_Rl, a,j,p,i) * t1(q,i) * t2(a,b,k,j) * wm_interm_21_pt4(b, k)
term(96) = term(96) + s2(a,p,j,i) * t1(q,i) * t2(a,b,k,j) * wm_interm_50_pt4(b, k)
term(97) = term(97) + s2(a,p,j,i) * t1(q,i) * t2(a,b,k,j) * wm_interm_53_pt4(b, k)
end do 
end do 
end do 
end do 
end do 

term(90) = term(90) * (-4.0d+0) 
term(91) = term(91) * 8.0d+0 
term(92) = term(92) * 16.0d+0 
term(93) = term(93) * (-8.0d+0) 
term(94) = term(94) * (-4.0d+0) 
term(95) = term(95) * 8.0d+0 
term(96) = term(96) * 8.0d+0 
term(97) = term(97) * (-4.0d+0) 

term(98) = term(98) + wm_interm_11_pt4(p, q) * wm_interm_14_pt4
term(99) = term(99) + wm_interm_11_pt4(p, q) * wm_interm_15_pt4
term(100) = term(100) + wm_interm_11_pt4(p, q) * wm_interm_38_pt4

term(98) = term(98) * (-2.0d+0) 
term(99) = term(99) * 4.0d+0 
term(100) = term(100) * 4.0d+0 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(101) = term(101) + s1(p,i) * wm_interm_26_pt4(q, j, i, k) * wm_interm_43_pt4(j, k)
term(102) = term(102) + s1(p,i) * wm_interm_26_pt4(q, j, i, k) * wm_interm_44_pt4(j, k)
term(103) = term(103) + r1(vrdav_Rl, p,i) * wm_interm_18_pt4(q, j, i, k) * wm_interm_43_pt4(j, k)
term(104) = term(104) + r1(vrdav_Rl, p,i) * wm_interm_18_pt4(q, j, i, k) * wm_interm_44_pt4(j, k)
term(105) = term(105) + r1(vrdav_Rr, p,i) * wm_interm_35_pt4(q, j, i, k) * wm_interm_52_pt4(k, j)
term(106) = term(106) + r1(vrdav_Rr, p,i) * wm_interm_35_pt4(q, j, i, k) * wm_interm_55_pt4(k, j)
term(107) = term(107) + r1(vrdav_Rr, p,i) * wm_interm_13_pt4(q, j, i, k) * wm_interm_29_pt4(k, j)
term(108) = term(108) + r1(vrdav_Rr, p,i) * wm_interm_13_pt4(q, j, i, k) * wm_interm_28_pt4(k, j)
term(109) = term(109) + t1(q,i) * wm_interm_20_pt4(p, j, i, k) * wm_interm_52_pt4(j, k)
term(110) = term(110) + t1(q,i) * wm_interm_20_pt4(p, j, i, k) * wm_interm_55_pt4(j, k)
end do 
end do 
end do 

term(101) = term(101) * (-4.0d+0) 
term(102) = term(102) * 8.0d+0 
term(103) = term(103) * (-4.0d+0) 
term(104) = term(104) * 8.0d+0 
term(105) = term(105) * 4.0d+0 
term(106) = term(106) * (-8.0d+0) 
term(107) = term(107) * 8.0d+0 
term(108) = term(108) * (-4.0d+0) 
term(109) = term(109) * 2.0d+0 
term(110) = term(110) * (-4.0d+0) 

do i = 1, nocc 
term(111) = term(111) + wm_interm_0_pt4(q, i) * wm_interm_1_pt4(p, i)
term(112) = term(112) + wm_interm_0_pt4(q, i) * wm_interm_2_pt4(p, i)
term(113) = term(113) + wm_interm_1_pt4(p, i) * wm_interm_3_pt4(q, i)
term(114) = term(114) + wm_interm_2_pt4(p, i) * wm_interm_3_pt4(q, i)
end do 

term(111) = term(111) * (-8.0d+0) 
term(112) = term(112) * 4.0d+0 
term(113) = term(113) * 4.0d+0 
term(114) = term(114) * (-2.0d+0) 

do a = nocc + 1, nactive 
term(115) = term(115) + wm_interm_10_pt4(a, p) * wm_interm_11_pt4(q, a)
term(116) = term(116) + wm_interm_11_pt4(q, a) * wm_interm_12_pt4(a, p)
term(117) = term(117) + wm_interm_10_pt4(p, a) * wm_interm_11_pt4(a, q)
term(118) = term(118) + wm_interm_11_pt4(a, q) * wm_interm_12_pt4(p, a)
term(119) = term(119) + wm_interm_30_pt4(p, a) * wm_interm_36_pt4(q, a)
term(120) = term(120) + wm_interm_11_pt4(q, a) * wm_interm_32_pt4(a, p)
term(121) = term(121) + wm_interm_30_pt4(a, p) * wm_interm_36_pt4(a, q)
term(122) = term(122) + wm_interm_11_pt4(a, q) * wm_interm_32_pt4(p, a)
end do 

term(115) = term(115) * 2.0d+0 
term(116) = term(116) * (-4.0d+0) 
term(117) = term(117) * 2.0d+0 
term(118) = term(118) * (-4.0d+0) 
term(119) = term(119) * (-2.0d+0) 
term(120) = term(120) * (-2.0d+0) 
term(121) = term(121) * (-2.0d+0) 
term(122) = term(122) * (-2.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(123) = term(123) + r2(vrdav_Rl, a,k,q,j) * r2(vrdav_Rr, a,j,p,i) * wm_interm_5_pt4(i, k)
term(124) = term(124) + r2(vrdav_Rl, a,i,q,j) * r2(vrdav_Rr, a,k,p,i) * wm_interm_5_pt4(k, j)
term(125) = term(125) + r2(vrdav_Rl, a,k,q,j) * r2(vrdav_Rr, a,k,p,i) * wm_interm_5_pt4(i, j)
term(126) = term(126) + s2(a,q,j,i) * wm_interm_17_pt4(a, k) * wm_interm_39_pt4(p, k, i, j)
term(127) = term(127) + s2(a,q,j,i) * wm_interm_23_pt4(a, k) * wm_interm_39_pt4(p, k, i, j)
term(128) = term(128) + s2(a,p,j,i) * wm_interm_1_pt4(a, k) * wm_interm_26_pt4(q, k, i, j)
term(129) = term(129) + s2(a,p,j,i) * wm_interm_26_pt4(q, k, i, j) * wm_interm_2_pt4(a, k)
term(130) = term(130) + s2(a,p,j,i) * wm_interm_18_pt4(q, k, i, j) * wm_interm_48_pt4(a, k)
term(131) = term(131) + s2(a,p,j,i) * wm_interm_18_pt4(q, k, i, j) * wm_interm_47_pt4(a, k)
term(132) = term(132) + t2(a,q,j,i) * wm_interm_0_pt4(a, k) * wm_interm_20_pt4(p, k, i, j)
term(133) = term(133) + t2(a,q,j,i) * wm_interm_20_pt4(p, k, i, j) * wm_interm_3_pt4(a, k)
end do 
end do 
end do 
end do 

term(123) = term(123) * 2.0d+0 
term(124) = term(124) * 2.0d+0 
term(125) = term(125) * (-4.0d+0) 
term(126) = term(126) * (-4.0d+0) 
term(127) = term(127) * 8.0d+0 
term(128) = term(128) * 4.0d+0 
term(129) = term(129) * (-2.0d+0) 
term(130) = term(130) * (-4.0d+0) 
term(131) = term(131) * 8.0d+0 
term(132) = term(132) * (-4.0d+0) 
term(133) = term(133) * 2.0d+0 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(134) = term(134) + r2(vrdav_Rl, a,j,q,i) * r2(vrdav_Rr, a,k,p,i) * wm_interm_5_pt4(k, j)
end do 
end do 
end do 
end do 

term(134) = term(134) * (-4.0d+0) 

do j = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(135) = term(135) + r1(vrdav_Rl, p,i) * s2(a,b,k,j) * t2(b,q,i,j) * wm_interm_1_pt4(a, k)
term(136) = term(136) + r1(vrdav_Rl, p,i) * s2(a,b,k,j) * t2(b,q,i,j) * wm_interm_2_pt4(a, k)
term(137) = term(137) + s1(p,i) * s2(a,b,k,j) * t2(b,q,i,j) * wm_interm_48_pt4(a, k)
term(138) = term(138) + s1(p,i) * s2(a,b,k,j) * t2(b,q,i,j) * wm_interm_47_pt4(a, k)
term(139) = term(139) + r1(vrdav_Rr, p,i) * s2(a,q,i,j) * t2(a,b,k,j) * wm_interm_0_pt4(b, k)
term(140) = term(140) + r1(vrdav_Rr, p,i) * s2(a,q,i,j) * t2(a,b,k,j) * wm_interm_3_pt4(b, k)
term(141) = term(141) + s2(a,p,i,j) * t1(q,i) * t2(a,b,k,j) * wm_interm_50_pt4(b, k)
term(142) = term(142) + s2(a,p,i,j) * t1(q,i) * t2(a,b,k,j) * wm_interm_53_pt4(b, k)
end do 
end do 
end do 
end do 
end do 

term(135) = term(135) * (-8.0d+0) 
term(136) = term(136) * 4.0d+0 
term(137) = term(137) * 8.0d+0 
term(138) = term(138) * (-16.0d+0) 
term(139) = term(139) * (-8.0d+0) 
term(140) = term(140) * 4.0d+0 
term(141) = term(141) * (-4.0d+0) 
term(142) = term(142) * 2.0d+0 

do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(143) = term(143) + s2(a,q,j,i) * wm_interm_17_pt4(a, k) * wm_interm_39_pt4(p, k, j, i)
term(144) = term(144) + s2(a,q,j,i) * wm_interm_23_pt4(a, k) * wm_interm_39_pt4(p, k, j, i)
term(145) = term(145) + s2(a,p,j,i) * wm_interm_1_pt4(a, k) * wm_interm_26_pt4(q, k, j, i)
term(146) = term(146) + s2(a,p,j,i) * wm_interm_26_pt4(q, k, j, i) * wm_interm_2_pt4(a, k)
term(147) = term(147) + s2(a,p,j,i) * wm_interm_18_pt4(q, k, j, i) * wm_interm_48_pt4(a, k)
term(148) = term(148) + s2(a,p,j,i) * wm_interm_18_pt4(q, k, j, i) * wm_interm_47_pt4(a, k)
term(149) = term(149) + t2(a,q,j,i) * wm_interm_0_pt4(a, k) * wm_interm_20_pt4(p, k, j, i)
term(150) = term(150) + t2(a,q,j,i) * wm_interm_20_pt4(p, k, j, i) * wm_interm_3_pt4(a, k)
end do 
end do 
end do 
end do 

term(143) = term(143) * 8.0d+0 
term(144) = term(144) * (-16.0d+0) 
term(145) = term(145) * (-8.0d+0) 
term(146) = term(146) * 4.0d+0 
term(147) = term(147) * 8.0d+0 
term(148) = term(148) * (-16.0d+0) 
term(149) = term(149) * 8.0d+0 
term(150) = term(150) * (-4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
term(151) = term(151) + s1(p,i) * t1(q,j) * wm_interm_6_pt4(j, i)
term(152) = term(152) + s1(p,i) * t1(q,j) * wm_interm_7_pt4(j, i)
term(153) = term(153) + r1(vrdav_Rl, q,j) * wm_interm_1_pt4(p, i) * wm_interm_28_pt4(i, j)
term(154) = term(154) + r1(vrdav_Rl, q,j) * wm_interm_1_pt4(p, i) * wm_interm_29_pt4(i, j)
term(155) = term(155) + r1(vrdav_Rl, q,j) * wm_interm_28_pt4(i, j) * wm_interm_2_pt4(p, i)
term(156) = term(156) + r1(vrdav_Rl, q,j) * wm_interm_29_pt4(i, j) * wm_interm_2_pt4(p, i)
term(157) = term(157) + s1(p,i) * wm_interm_27_pt4(q, j) * wm_interm_43_pt4(i, j)
term(158) = term(158) + s1(p,i) * wm_interm_27_pt4(q, j) * wm_interm_44_pt4(i, j)
term(159) = term(159) + s1(p,i) * wm_interm_22_pt4(q, j) * wm_interm_43_pt4(i, j)
term(160) = term(160) + s1(p,i) * wm_interm_22_pt4(q, j) * wm_interm_44_pt4(i, j)
term(161) = term(161) + r1(vrdav_Rl, p,i) * wm_interm_17_pt4(q, j) * wm_interm_43_pt4(i, j)
term(162) = term(162) + r1(vrdav_Rl, p,i) * wm_interm_17_pt4(q, j) * wm_interm_44_pt4(i, j)
term(163) = term(163) + r1(vrdav_Rl, p,i) * wm_interm_23_pt4(q, j) * wm_interm_43_pt4(i, j)
term(164) = term(164) + r1(vrdav_Rl, p,i) * wm_interm_23_pt4(q, j) * wm_interm_44_pt4(i, j)
term(165) = term(165) + r1(vrdav_Rr, p,i) * wm_interm_0_pt4(q, j) * wm_interm_28_pt4(i, j)
term(166) = term(166) + r1(vrdav_Rr, p,i) * wm_interm_0_pt4(q, j) * wm_interm_29_pt4(i, j)
term(167) = term(167) + r1(vrdav_Rr, p,i) * wm_interm_28_pt4(i, j) * wm_interm_3_pt4(q, j)
term(168) = term(168) + r1(vrdav_Rr, p,i) * wm_interm_29_pt4(i, j) * wm_interm_3_pt4(q, j)
term(169) = term(169) + t1(q,j) * wm_interm_28_pt4(j, i) * wm_interm_53_pt4(p, i)
term(170) = term(170) + t1(q,j) * wm_interm_29_pt4(j, i) * wm_interm_53_pt4(p, i)
term(171) = term(171) + t1(q,j) * wm_interm_28_pt4(j, i) * wm_interm_50_pt4(p, i)
term(172) = term(172) + t1(q,j) * wm_interm_29_pt4(j, i) * wm_interm_50_pt4(p, i)
term(173) = term(173) + t1(q,j) * wm_interm_19_pt4(p, i) * wm_interm_52_pt4(j, i)
term(174) = term(174) + t1(q,j) * wm_interm_21_pt4(p, i) * wm_interm_52_pt4(j, i)
term(175) = term(175) + t1(q,j) * wm_interm_19_pt4(p, i) * wm_interm_55_pt4(j, i)
term(176) = term(176) + t1(q,j) * wm_interm_21_pt4(p, i) * wm_interm_55_pt4(j, i)
end do 
end do 

term(151) = term(151) * 2.0d+0 
term(152) = term(152) * (-4.0d+0) 
term(153) = term(153) * 4.0d+0 
term(154) = term(154) * (-8.0d+0) 
term(155) = term(155) * (-2.0d+0) 
term(156) = term(156) * 4.0d+0 
term(157) = term(157) * 8.0d+0 
term(158) = term(158) * (-16.0d+0) 
term(159) = term(159) * (-4.0d+0) 
term(160) = term(160) * 8.0d+0 
term(161) = term(161) * (-4.0d+0) 
term(162) = term(162) * 8.0d+0 
term(163) = term(163) * 8.0d+0 
term(164) = term(164) * (-16.0d+0) 
term(165) = term(165) * (-8.0d+0) 
term(166) = term(166) * 16.0d+0 
term(167) = term(167) * 4.0d+0 
term(168) = term(168) * (-8.0d+0) 
term(169) = term(169) * 2.0d+0 
term(170) = term(170) * (-4.0d+0) 
term(171) = term(171) * (-4.0d+0) 
term(172) = term(172) * 8.0d+0 
term(173) = term(173) * 2.0d+0 
term(174) = term(174) * (-4.0d+0) 
term(175) = term(175) * (-4.0d+0) 
term(176) = term(176) * 8.0d+0 

do j = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do k = 1, nocc 
term(177) = term(177) + s1(q,i) * wm_interm_24_pt4(j, k, l, i) * wm_interm_39_pt4(p, l, k, j)
end do 
end do 
end do 
end do 

term(177) = term(177) * (-4.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do j = 1, nocc 
term(178) = term(178) + s1(q,i) * wm_interm_24_pt4(j, k, l, i) * wm_interm_39_pt4(p, l, j, k)
end do 
end do 
end do 
end do 

term(178) = term(178) * 8.0d+0 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(179) = term(179) + s2(a,q,j,i) * wm_interm_22_pt4(a, k) * wm_interm_9_pt4(p, j, i, k)
term(180) = term(180) + s2(a,q,j,i) * wm_interm_22_pt4(a, k) * wm_interm_9_pt4(p, i, j, k)
term(181) = term(181) + s2(a,q,j,i) * wm_interm_22_pt4(a, k) * wm_interm_46_pt4(p, i, j, k)
term(182) = term(182) + s2(a,q,j,i) * wm_interm_22_pt4(a, k) * wm_interm_46_pt4(p, j, i, k)
term(183) = term(183) + s2(a,q,j,i) * wm_interm_27_pt4(a, k) * wm_interm_9_pt4(p, j, i, k)
term(184) = term(184) + s2(a,q,j,i) * wm_interm_27_pt4(a, k) * wm_interm_9_pt4(p, i, j, k)
term(185) = term(185) + s2(a,q,j,i) * wm_interm_27_pt4(a, k) * wm_interm_46_pt4(p, i, j, k)
term(186) = term(186) + s2(a,q,j,i) * wm_interm_27_pt4(a, k) * wm_interm_46_pt4(p, j, i, k)
term(187) = term(187) + t2(a,q,j,i) * wm_interm_35_pt4(p, j, i, k) * wm_interm_53_pt4(a, k)
term(188) = term(188) + t2(a,q,j,i) * wm_interm_35_pt4(p, i, j, k) * wm_interm_53_pt4(a, k)
term(189) = term(189) + t2(a,q,j,i) * wm_interm_35_pt4(p, j, i, k) * wm_interm_50_pt4(a, k)
term(190) = term(190) + t2(a,q,j,i) * wm_interm_35_pt4(p, i, j, k) * wm_interm_50_pt4(a, k)
term(191) = term(191) + t2(a,q,j,i) * wm_interm_19_pt4(a, k) * wm_interm_8_pt4(p, j, i, k)
term(192) = term(192) + t2(a,q,j,i) * wm_interm_19_pt4(a, k) * wm_interm_8_pt4(p, i, j, k)
term(193) = term(193) + t2(a,q,j,i) * wm_interm_21_pt4(a, k) * wm_interm_8_pt4(p, j, i, k)
term(194) = term(194) + t2(a,q,j,i) * wm_interm_21_pt4(a, k) * wm_interm_8_pt4(p, i, j, k)
term(195) = term(195) + t2(a,q,j,i) * wm_interm_13_pt4(p, i, j, k) * wm_interm_19_pt4(a, k)
term(196) = term(196) + t2(a,q,j,i) * wm_interm_13_pt4(p, j, i, k) * wm_interm_19_pt4(a, k)
term(197) = term(197) + t2(a,q,j,i) * wm_interm_13_pt4(p, i, j, k) * wm_interm_21_pt4(a, k)
term(198) = term(198) + t2(a,q,j,i) * wm_interm_13_pt4(p, j, i, k) * wm_interm_21_pt4(a, k)
end do 
end do 
end do 
end do 

term(179) = term(179) * (-2.0d+0) 
term(180) = term(180) * 4.0d+0 
term(181) = term(181) * (-2.0d+0) 
term(182) = term(182) * 4.0d+0 
term(183) = term(183) * 4.0d+0 
term(184) = term(184) * (-8.0d+0) 
term(185) = term(185) * 4.0d+0 
term(186) = term(186) * (-8.0d+0) 
term(187) = term(187) * 2.0d+0 
term(188) = term(188) * (-4.0d+0) 
term(189) = term(189) * (-4.0d+0) 
term(190) = term(190) * 8.0d+0 
term(192) = term(192) * (-2.0d+0) 
term(193) = term(193) * (-2.0d+0) 
term(194) = term(194) * 4.0d+0 
term(196) = term(196) * (-2.0d+0) 
term(197) = term(197) * (-2.0d+0) 
term(198) = term(198) * 4.0d+0 

do l = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
term(199) = term(199) + r1(vrdav_Rl, q,i) * wm_interm_24_pt4(j, k, i, l) * wm_interm_9_pt4(p, k, j, l)
end do 
end do 
end do 
end do 

term(199) = term(199) * (-4.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(200) = term(200) + r1(vrdav_Rl, q,i) * wm_interm_24_pt4(j, k, i, l) * wm_interm_9_pt4(p, j, k, l)
end do 
end do 
end do 
end do 

term(200) = term(200) * 8.0d+0 

do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(201) = term(201) + r2(vrdav_Rl, a,j,p,i) * r1(vrdav_Rr, b,k) * t2(a,q,l,i) * wm_interm_35_pt4(b, l, k, j)
term(202) = term(202) + r2(vrdav_Rl, a,j,p,i) * r1(vrdav_Rr, b,k) * t2(a,q,l,j) * wm_interm_35_pt4(b, l, k, i)
end do 
end do 
end do 
end do 
end do 
end do 

term(201) = term(201) * (-4.0d+0) 
term(202) = term(202) * 2.0d+0 

do l = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(203) = term(203) + s1(p,i) * wm_interm_26_pt4(q, j, k, l) * wm_interm_42_pt4(j, i, l, k)
term(204) = term(204) + s1(p,i) * wm_interm_26_pt4(q, j, k, l) * wm_interm_42_pt4(j, i, k, l)
term(205) = term(205) + r1(vrdav_Rl, p,i) * wm_interm_18_pt4(q, j, k, l) * wm_interm_42_pt4(i, j, k, l)
term(206) = term(206) + r1(vrdav_Rl, p,i) * wm_interm_18_pt4(q, j, k, l) * wm_interm_42_pt4(i, j, l, k)
term(207) = term(207) + r1(vrdav_Rr, p,i) * wm_interm_35_pt4(q, j, k, l) * wm_interm_54_pt4(i, l, j, k)
term(208) = term(208) + r1(vrdav_Rr, p,i) * wm_interm_35_pt4(q, j, k, l) * wm_interm_54_pt4(l, i, j, k)
term(209) = term(209) + r1(vrdav_Rr, p,i) * wm_interm_13_pt4(q, j, k, l) * wm_interm_24_pt4(l, i, j, k)
term(210) = term(210) + r1(vrdav_Rr, p,i) * wm_interm_13_pt4(q, j, k, l) * wm_interm_24_pt4(i, l, j, k)
term(211) = term(211) + t1(q,i) * wm_interm_20_pt4(p, j, k, l) * wm_interm_54_pt4(i, j, k, l)
term(212) = term(212) + t1(q,i) * wm_interm_20_pt4(p, j, k, l) * wm_interm_54_pt4(j, i, k, l)
term(213) = term(213) + t1(q,i) * wm_interm_20_pt4(p, j, k, l) * wm_interm_54_pt4(j, i, l, k)
term(214) = term(214) + t1(q,i) * wm_interm_20_pt4(p, j, k, l) * wm_interm_54_pt4(i, j, l, k)
end do 
end do 
end do 
end do 

term(203) = term(203) * (-4.0d+0) 
term(204) = term(204) * 8.0d+0 
term(205) = term(205) * (-4.0d+0) 
term(206) = term(206) * 8.0d+0 
term(207) = term(207) * (-4.0d+0) 
term(208) = term(208) * 2.0d+0 
term(209) = term(209) * (-4.0d+0) 
term(210) = term(210) * 2.0d+0 
term(212) = term(212) * (-2.0d+0) 
term(214) = term(214) * (-2.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(215) = term(215) + s1(a,i) * wm_interm_31_pt4(p, q) * wm_interm_47_pt4(a, i)
term(216) = term(216) + s1(a,i) * wm_interm_31_pt4(p, q) * wm_interm_48_pt4(a, i)
term(217) = term(217) + r1(vrdav_Rl, p,i) * wm_interm_1_pt4(a, i) * wm_interm_31_pt4(a, q)
term(218) = term(218) + r1(vrdav_Rl, p,i) * wm_interm_1_pt4(a, i) * wm_interm_33_pt4(a, q)
term(219) = term(219) + r1(vrdav_Rl, p,i) * wm_interm_2_pt4(a, i) * wm_interm_31_pt4(a, q)
term(220) = term(220) + r1(vrdav_Rl, p,i) * wm_interm_2_pt4(a, i) * wm_interm_33_pt4(a, q)
term(221) = term(221) + s1(a,i) * wm_interm_33_pt4(p, q) * wm_interm_47_pt4(a, i)
term(222) = term(222) + s1(a,i) * wm_interm_33_pt4(p, q) * wm_interm_48_pt4(a, i)
term(223) = term(223) + s1(p,i) * wm_interm_31_pt4(a, q) * wm_interm_48_pt4(a, i)
term(224) = term(224) + s1(p,i) * wm_interm_31_pt4(a, q) * wm_interm_47_pt4(a, i)
term(225) = term(225) + s1(p,i) * wm_interm_33_pt4(a, q) * wm_interm_48_pt4(a, i)
term(226) = term(226) + s1(p,i) * wm_interm_33_pt4(a, q) * wm_interm_47_pt4(a, i)
term(227) = term(227) + r1(vrdav_Rl, a,i) * wm_interm_17_pt4(q, i) * wm_interm_40_pt4(a, p)
term(228) = term(228) + r1(vrdav_Rl, a,i) * wm_interm_17_pt4(q, i) * wm_interm_41_pt4(a, p)
term(229) = term(229) + s1(a,i) * wm_interm_27_pt4(q, i) * wm_interm_40_pt4(a, p)
term(230) = term(230) + s1(a,i) * wm_interm_27_pt4(q, i) * wm_interm_41_pt4(a, p)
term(231) = term(231) + r1(vrdav_Rl, a,i) * wm_interm_23_pt4(q, i) * wm_interm_40_pt4(a, p)
term(232) = term(232) + r1(vrdav_Rl, a,i) * wm_interm_23_pt4(q, i) * wm_interm_41_pt4(a, p)
term(233) = term(233) + s1(a,i) * wm_interm_22_pt4(q, i) * wm_interm_40_pt4(a, p)
term(234) = term(234) + s1(a,i) * wm_interm_22_pt4(q, i) * wm_interm_41_pt4(a, p)
term(235) = term(235) + t1(a,i) * wm_interm_33_pt4(a, q) * wm_interm_53_pt4(p, i)
term(236) = term(236) + t1(a,i) * wm_interm_31_pt4(a, q) * wm_interm_53_pt4(p, i)
term(237) = term(237) + t1(a,i) * wm_interm_33_pt4(a, q) * wm_interm_50_pt4(p, i)
term(238) = term(238) + t1(a,i) * wm_interm_31_pt4(a, q) * wm_interm_50_pt4(p, i)
term(239) = term(239) + t1(a,i) * wm_interm_19_pt4(p, i) * wm_interm_56_pt4(a, q)
term(240) = term(240) + t1(a,i) * wm_interm_21_pt4(p, i) * wm_interm_56_pt4(a, q)
term(241) = term(241) + t1(a,i) * wm_interm_19_pt4(p, i) * wm_interm_57_pt4(a, q)
term(242) = term(242) + t1(a,i) * wm_interm_21_pt4(p, i) * wm_interm_57_pt4(a, q)
term(243) = term(243) + t1(a,i) * wm_interm_31_pt4(p, q) * wm_interm_50_pt4(a, i)
term(244) = term(244) + t1(a,i) * wm_interm_31_pt4(p, q) * wm_interm_53_pt4(a, i)
term(245) = term(245) + r1(vrdav_Rr, a,i) * wm_interm_0_pt4(p, i) * wm_interm_31_pt4(a, q)
term(246) = term(246) + r1(vrdav_Rr, a,i) * wm_interm_0_pt4(p, i) * wm_interm_33_pt4(a, q)
term(247) = term(247) + r1(vrdav_Rr, a,i) * wm_interm_31_pt4(a, q) * wm_interm_3_pt4(p, i)
term(248) = term(248) + r1(vrdav_Rr, a,i) * wm_interm_33_pt4(a, q) * wm_interm_3_pt4(p, i)
term(249) = term(249) + t1(a,i) * wm_interm_33_pt4(p, q) * wm_interm_50_pt4(a, i)
term(250) = term(250) + t1(a,i) * wm_interm_33_pt4(p, q) * wm_interm_53_pt4(a, i)
end do 
end do 

term(215) = term(215) * 16.0d+0 
term(216) = term(216) * (-8.0d+0) 
term(217) = term(217) * (-8.0d+0) 
term(218) = term(218) * 4.0d+0 
term(219) = term(219) * 4.0d+0 
term(220) = term(220) * (-2.0d+0) 
term(221) = term(221) * (-8.0d+0) 
term(222) = term(222) * 4.0d+0 
term(223) = term(223) * 8.0d+0 
term(224) = term(224) * (-16.0d+0) 
term(225) = term(225) * (-4.0d+0) 
term(226) = term(226) * 8.0d+0 
term(227) = term(227) * (-4.0d+0) 
term(228) = term(228) * 8.0d+0 
term(229) = term(229) * 8.0d+0 
term(230) = term(230) * (-16.0d+0) 
term(231) = term(231) * 8.0d+0 
term(232) = term(232) * (-16.0d+0) 
term(233) = term(233) * (-4.0d+0) 
term(234) = term(234) * 8.0d+0 
term(235) = term(235) * 2.0d+0 
term(236) = term(236) * (-4.0d+0) 
term(237) = term(237) * (-4.0d+0) 
term(238) = term(238) * 8.0d+0 
term(239) = term(239) * 2.0d+0 
term(240) = term(240) * (-4.0d+0) 
term(241) = term(241) * (-4.0d+0) 
term(242) = term(242) * 8.0d+0 
term(243) = term(243) * (-16.0d+0) 
term(244) = term(244) * 8.0d+0 
term(245) = term(245) * 8.0d+0 
term(246) = term(246) * (-4.0d+0) 
term(247) = term(247) * (-4.0d+0) 
term(248) = term(248) * 2.0d+0 
term(249) = term(249) * 8.0d+0 
term(250) = term(250) * (-4.0d+0) 

do l = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(251) = term(251) + r1(vrdav_Rl, q,j) * r2(vrdav_Rr, a,k,p,i) * s2(a,b,i,l) * wm_interm_18_pt4(b, k, j, l)
term(252) = term(252) + r1(vrdav_Rl, q,j) * r2(vrdav_Rr, a,k,p,i) * s2(a,b,k,l) * wm_interm_18_pt4(b, i, j, l)
term(253) = term(253) + r1(vrdav_Rl, q,j) * r2(vrdav_Rr, a,k,p,i) * s2(a,b,l,i) * wm_interm_18_pt4(b, k, j, l)
term(254) = term(254) + r2(vrdav_Rr, a,k,p,i) * s1(q,j) * s2(a,b,i,l) * wm_interm_26_pt4(b, k, j, l)
term(255) = term(255) + r2(vrdav_Rr, a,k,p,i) * s1(q,j) * s2(a,b,k,l) * wm_interm_26_pt4(b, i, j, l)
term(256) = term(256) + r2(vrdav_Rr, a,k,p,i) * s1(q,j) * s2(a,b,l,i) * wm_interm_26_pt4(b, k, j, l)
term(257) = term(257) + r2(vrdav_Rl, a,k,p,i) * t1(q,j) * t2(a,b,k,l) * wm_interm_20_pt4(b, i, j, l)
term(258) = term(258) + r2(vrdav_Rl, a,k,p,i) * t1(q,j) * t2(a,b,l,i) * wm_interm_20_pt4(b, k, j, l)
term(259) = term(259) + r2(vrdav_Rl, a,k,p,i) * t1(q,j) * t2(a,b,i,l) * wm_interm_20_pt4(b, k, j, l)
end do 
end do 
end do 
end do 
end do 
end do 

term(251) = term(251) * 8.0d+0 
term(252) = term(252) * (-16.0d+0) 
term(253) = term(253) * (-4.0d+0) 
term(254) = term(254) * 8.0d+0 
term(255) = term(255) * (-16.0d+0) 
term(256) = term(256) * (-4.0d+0) 
term(257) = term(257) * 8.0d+0 
term(258) = term(258) * 2.0d+0 
term(259) = term(259) * (-4.0d+0) 

do l = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
term(260) = term(260) + r1(vrdav_Rr, p,i) * wm_interm_35_pt4(q, j, k, l) * wm_interm_54_pt4(i, l, k, j)
term(261) = term(261) + r1(vrdav_Rr, p,i) * wm_interm_24_pt4(j, i, k, l) * wm_interm_8_pt4(q, k, l, j)
term(262) = term(262) + r1(vrdav_Rr, p,i) * wm_interm_24_pt4(i, j, k, l) * wm_interm_8_pt4(q, k, l, j)
term(263) = term(263) + r1(vrdav_Rr, p,i) * wm_interm_35_pt4(q, j, k, l) * wm_interm_54_pt4(l, i, k, j)
term(264) = term(264) + t1(q,i) * wm_interm_24_pt4(j, i, k, l) * wm_interm_51_pt4(p, k, l, j)
term(265) = term(265) + t1(q,i) * wm_interm_24_pt4(i, j, k, l) * wm_interm_51_pt4(p, k, l, j)
term(266) = term(266) + t1(q,i) * wm_interm_24_pt4(i, j, k, l) * wm_interm_49_pt4(p, k, l, j)
term(267) = term(267) + t1(q,i) * wm_interm_24_pt4(j, i, k, l) * wm_interm_49_pt4(p, k, l, j)
end do 
end do 
end do 
end do 

term(260) = term(260) * 2.0d+0 
term(261) = term(261) * 2.0d+0 
term(262) = term(262) * (-4.0d+0) 
term(263) = term(263) * (-4.0d+0) 
term(265) = term(265) * (-2.0d+0) 
term(267) = term(267) * (-2.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(268) = term(268) + r1(vrdav_Rl, a,j) * r2(vrdav_Rr, b,k,p,i) * s2(b,q,l,k) * wm_interm_45_pt4(a, i, l, j)
term(269) = term(269) + r2(vrdav_Rl, a,k,p,i) * r1(vrdav_Rr, b,l) * t2(a,q,i,j) * wm_interm_35_pt4(b, j, l, k)
term(270) = term(270) + r2(vrdav_Rl, a,k,p,i) * r1(vrdav_Rr, b,i) * t2(a,q,l,j) * wm_interm_35_pt4(b, j, l, k)
term(271) = term(271) + r2(vrdav_Rl, a,k,p,i) * r1(vrdav_Rr, b,i) * t2(a,q,l,j) * wm_interm_35_pt4(b, l, j, k)
term(272) = term(272) + r2(vrdav_Rl, a,k,p,i) * r1(vrdav_Rr, b,l) * t2(a,q,i,j) * wm_interm_35_pt4(b, l, j, k)
term(273) = term(273) + r1(vrdav_Rr, a,j) * s2(b,p,k,j) * t2(b,q,l,i) * wm_interm_8_pt4(a, i, l, k)
term(274) = term(274) + r1(vrdav_Rr, a,j) * s2(b,p,k,j) * t2(b,q,l,i) * wm_interm_8_pt4(a, l, i, k)
term(275) = term(275) + r1(vrdav_Rr, a,j) * s2(b,p,k,j) * t2(b,q,l,i) * wm_interm_13_pt4(a, i, l, k)
term(276) = term(276) + r1(vrdav_Rr, a,j) * s2(b,p,k,j) * t2(b,q,l,i) * wm_interm_13_pt4(a, l, i, k)
end do 
end do 
end do 
end do 
end do 
end do 

term(268) = term(268) * (-4.0d+0) 
term(269) = term(269) * 2.0d+0 
term(270) = term(270) * (-4.0d+0) 
term(271) = term(271) * 2.0d+0 
term(272) = term(272) * (-4.0d+0) 
term(273) = term(273) * (-2.0d+0) 
term(276) = term(276) * (-2.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(277) = term(277) + r1(vrdav_Rl, a,j) * r2(vrdav_Rr, b,k,p,i) * s2(b,q,l,i) * wm_interm_45_pt4(a, k, l, j)
term(278) = term(278) + r2(vrdav_Rl, a,j,p,i) * r1(vrdav_Rr, b,k) * t2(a,q,l,i) * wm_interm_35_pt4(b, k, l, j)
term(279) = term(279) + r2(vrdav_Rl, a,j,p,i) * r1(vrdav_Rr, b,k) * t2(a,q,l,j) * wm_interm_35_pt4(b, k, l, i)
term(280) = term(280) + r2(vrdav_Rl, a,k,p,i) * r1(vrdav_Rr, b,k) * t2(a,q,l,j) * wm_interm_35_pt4(b, j, l, i)
term(281) = term(281) + r2(vrdav_Rl, a,k,p,i) * r1(vrdav_Rr, b,l) * t2(a,q,k,j) * wm_interm_35_pt4(b, j, l, i)
term(282) = term(282) + r2(vrdav_Rl, a,k,p,i) * r1(vrdav_Rr, b,k) * t2(a,q,l,j) * wm_interm_35_pt4(b, l, j, i)
term(283) = term(283) + r2(vrdav_Rl, a,k,p,i) * r1(vrdav_Rr, b,l) * t2(a,q,k,j) * wm_interm_35_pt4(b, l, j, i)
end do 
end do 
end do 
end do 
end do 
end do 

term(277) = term(277) * 8.0d+0 
term(278) = term(278) * 8.0d+0 
term(279) = term(279) * (-4.0d+0) 
term(280) = term(280) * 2.0d+0 
term(281) = term(281) * (-4.0d+0) 
term(282) = term(282) * (-4.0d+0) 
term(283) = term(283) * 8.0d+0 

do l = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(284) = term(284) + r1(vrdav_Rl, a,j) * r2(vrdav_Rr, b,k,p,i) * s2(b,q,i,l) * wm_interm_45_pt4(a, k, l, j)
term(285) = term(285) + r1(vrdav_Rl, a,j) * r2(vrdav_Rr, b,k,p,i) * s2(b,q,k,l) * wm_interm_45_pt4(a, i, l, j)
term(286) = term(286) + r1(vrdav_Rl, a,i) * s2(b,p,k,j) * t2(b,q,l,i) * wm_interm_9_pt4(a, k, j, l)
term(287) = term(287) + r1(vrdav_Rl, a,i) * s2(b,p,k,j) * t2(b,q,l,i) * wm_interm_9_pt4(a, j, k, l)
end do 
end do 
end do 
end do 
end do 
end do 

term(284) = term(284) * (-4.0d+0) 
term(285) = term(285) * 8.0d+0 
term(286) = term(286) * (-4.0d+0) 
term(287) = term(287) * 8.0d+0 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
term(288) = term(288) + s1(p,i) * wm_interm_26_pt4(q, j, k, i) * wm_interm_43_pt4(j, k)
term(289) = term(289) + s1(p,i) * wm_interm_26_pt4(q, j, k, i) * wm_interm_44_pt4(j, k)
term(290) = term(290) + r1(vrdav_Rl, p,i) * wm_interm_18_pt4(q, j, k, i) * wm_interm_43_pt4(j, k)
term(291) = term(291) + r1(vrdav_Rl, p,i) * wm_interm_18_pt4(q, j, k, i) * wm_interm_44_pt4(j, k)
term(292) = term(292) + t1(q,i) * wm_interm_20_pt4(p, j, k, i) * wm_interm_52_pt4(j, k)
term(293) = term(293) + t1(q,i) * wm_interm_20_pt4(p, j, k, i) * wm_interm_55_pt4(j, k)
end do 
end do 
end do 

term(288) = term(288) * 8.0d+0 
term(289) = term(289) * (-16.0d+0) 
term(290) = term(290) * 8.0d+0 
term(291) = term(291) * (-16.0d+0) 
term(292) = term(292) * (-4.0d+0) 
term(293) = term(293) * 8.0d+0 

do l = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(294) = term(294) + r1(vrdav_Rl, q,j) * r2(vrdav_Rr, a,k,p,i) * s2(a,b,l,k) * wm_interm_18_pt4(b, i, j, l)
term(295) = term(295) + r2(vrdav_Rr, a,k,p,i) * s1(q,j) * s2(a,b,l,k) * wm_interm_26_pt4(b, i, j, l)
term(296) = term(296) + r2(vrdav_Rl, a,k,p,i) * t1(q,j) * t2(a,b,l,k) * wm_interm_20_pt4(b, i, j, l)
end do 
end do 
end do 
end do 
end do 
end do 

term(294) = term(294) * 8.0d+0 
term(295) = term(295) * 8.0d+0 
term(296) = term(296) * (-4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do l = 1, nocc 
do a = nocc + 1, nactive 
term(297) = term(297) + r1(vrdav_Rl, q,j) * r2(vrdav_Rr, a,k,p,i) * s2(a,b,l,k) * wm_interm_18_pt4(b, i, l, j)
term(298) = term(298) + r2(vrdav_Rr, a,k,p,i) * s1(q,j) * s2(a,b,l,k) * wm_interm_26_pt4(b, i, l, j)
term(299) = term(299) + s1(p,i) * s2(a,b,j,k) * t2(b,q,l,j) * wm_interm_39_pt4(a, l, k, i)
term(300) = term(300) + r2(vrdav_Rl, a,k,p,i) * t1(q,j) * t2(a,b,l,k) * wm_interm_20_pt4(b, i, l, j)
end do 
end do 
end do 
end do 
end do 
end do 

term(297) = term(297) * (-4.0d+0) 
term(298) = term(298) * (-4.0d+0) 
term(299) = term(299) * 8.0d+0 
term(300) = term(300) * 2.0d+0 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do l = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(301) = term(301) + r1(vrdav_Rl, q,j) * r2(vrdav_Rr, a,k,p,i) * s2(a,b,l,i) * wm_interm_18_pt4(b, k, l, j)
term(302) = term(302) + r2(vrdav_Rr, a,k,p,i) * s1(q,j) * s2(a,b,l,i) * wm_interm_26_pt4(b, k, l, j)
term(303) = term(303) + r2(vrdav_Rl, a,k,p,i) * t1(q,j) * t2(a,b,l,i) * wm_interm_20_pt4(b, k, l, j)
end do 
end do 
end do 
end do 
end do 
end do 

term(301) = term(301) * 8.0d+0 
term(302) = term(302) * 8.0d+0 
term(303) = term(303) * (-4.0d+0) 

do l = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(304) = term(304) + r1(vrdav_Rl, q,j) * r2(vrdav_Rr, a,k,p,i) * s2(a,b,i,l) * wm_interm_18_pt4(b, k, l, j)
term(305) = term(305) + r1(vrdav_Rl, q,j) * r2(vrdav_Rr, a,k,p,i) * s2(a,b,k,l) * wm_interm_18_pt4(b, i, l, j)
term(306) = term(306) + r2(vrdav_Rr, a,k,p,i) * s1(q,j) * s2(a,b,i,l) * wm_interm_26_pt4(b, k, l, j)
term(307) = term(307) + r2(vrdav_Rr, a,k,p,i) * s1(q,j) * s2(a,b,k,l) * wm_interm_26_pt4(b, i, l, j)
term(308) = term(308) + s1(p,i) * s2(a,b,k,l) * t2(b,q,l,j) * wm_interm_39_pt4(a, j, k, i)
term(309) = term(309) + s1(p,i) * s2(a,b,k,l) * t2(b,q,k,j) * wm_interm_39_pt4(a, j, l, i)
term(310) = term(310) + r2(vrdav_Rl, a,k,p,i) * t1(q,j) * t2(a,b,k,l) * wm_interm_20_pt4(b, i, l, j)
term(311) = term(311) + r2(vrdav_Rl, a,k,p,i) * t1(q,j) * t2(a,b,i,l) * wm_interm_20_pt4(b, k, l, j)
end do 
end do 
end do 
end do 
end do 
end do 

term(304) = term(304) * (-4.0d+0) 
term(305) = term(305) * 8.0d+0 
term(306) = term(306) * (-4.0d+0) 
term(307) = term(307) * 8.0d+0 
term(308) = term(308) * 8.0d+0 
term(309) = term(309) * (-4.0d+0) 
term(310) = term(310) * (-4.0d+0) 
term(311) = term(311) * 2.0d+0 

do k = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(312) = term(312) + r1(vrdav_Rl, a,i) * r2(vrdav_Rr, b,j,p,i) * s2(b,q,l,k) * wm_interm_45_pt4(a, j, l, k)
term(313) = term(313) + r1(vrdav_Rl, a,j) * r2(vrdav_Rr, b,j,p,i) * s2(b,q,l,k) * wm_interm_45_pt4(a, i, l, k)
term(314) = term(314) + r1(vrdav_Rl, a,j) * s2(b,p,l,k) * t2(b,q,k,i) * wm_interm_9_pt4(a, j, l, i)
term(315) = term(315) + r1(vrdav_Rl, a,j) * s2(b,p,l,k) * t2(b,q,j,i) * wm_interm_9_pt4(a, k, l, i)
term(316) = term(316) + r1(vrdav_Rl, a,j) * s2(b,p,l,k) * t2(b,q,j,i) * wm_interm_9_pt4(a, l, k, i)
term(317) = term(317) + r1(vrdav_Rl, a,j) * s2(b,p,l,k) * t2(b,q,l,i) * wm_interm_9_pt4(a, j, k, i)
term(318) = term(318) + r1(vrdav_Rr, a,j) * s2(b,p,j,k) * t2(b,q,l,i) * wm_interm_8_pt4(a, i, l, k)
term(319) = term(319) + r1(vrdav_Rr, a,j) * s2(b,p,j,k) * t2(b,q,l,i) * wm_interm_8_pt4(a, l, i, k)
term(320) = term(320) + r1(vrdav_Rr, a,j) * s2(b,p,k,i) * t2(b,q,l,i) * wm_interm_8_pt4(a, j, l, k)
term(321) = term(321) + r1(vrdav_Rr, a,j) * s2(b,p,i,k) * t2(b,q,l,i) * wm_interm_8_pt4(a, j, l, k)
term(322) = term(322) + r1(vrdav_Rr, a,j) * s2(b,p,l,k) * t2(b,q,l,i) * wm_interm_8_pt4(a, j, i, k)
term(323) = term(323) + r1(vrdav_Rr, a,j) * s2(b,p,k,i) * t2(b,q,l,i) * wm_interm_13_pt4(a, j, l, k)
term(324) = term(324) + r1(vrdav_Rr, a,j) * s2(b,p,i,k) * t2(b,q,l,i) * wm_interm_13_pt4(a, j, l, k)
term(325) = term(325) + r1(vrdav_Rr, a,j) * s2(b,p,j,k) * t2(b,q,l,i) * wm_interm_13_pt4(a, l, i, k)
term(326) = term(326) + r1(vrdav_Rr, a,j) * s2(b,p,l,k) * t2(b,q,l,i) * wm_interm_13_pt4(a, j, i, k)
term(327) = term(327) + r1(vrdav_Rr, a,j) * s2(b,p,j,k) * t2(b,q,l,i) * wm_interm_13_pt4(a, i, l, k)
end do 
end do 
end do 
end do 
end do 
end do 

term(312) = term(312) * (-4.0d+0) 
term(313) = term(313) * 8.0d+0 
term(314) = term(314) * 8.0d+0 
term(315) = term(315) * (-4.0d+0) 
term(316) = term(316) * 8.0d+0 
term(317) = term(317) * (-16.0d+0) 
term(319) = term(319) * (-2.0d+0) 
term(320) = term(320) * 4.0d+0 
term(321) = term(321) * (-2.0d+0) 
term(322) = term(322) * 4.0d+0 
term(323) = term(323) * (-2.0d+0) 
term(326) = term(326) * (-2.0d+0) 
term(327) = term(327) * (-2.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(328) = term(328) + r1(vrdav_Rl, a,j) * r2(vrdav_Rr, b,j,p,i) * s2(b,q,k,i) * wm_interm_17_pt4(a, k)
term(329) = term(329) + r1(vrdav_Rl, a,j) * r2(vrdav_Rr, b,j,p,i) * s2(b,q,k,i) * wm_interm_23_pt4(a, k)
term(330) = term(330) + r1(vrdav_Rl, a,j) * s2(b,p,k,i) * t2(b,q,j,i) * wm_interm_1_pt4(a, k)
term(331) = term(331) + r1(vrdav_Rl, a,j) * s2(b,p,k,i) * t2(b,q,j,i) * wm_interm_2_pt4(a, k)
term(332) = term(332) + r1(vrdav_Rr, a,j) * s2(b,p,j,i) * t2(b,q,k,i) * wm_interm_0_pt4(a, k)
term(333) = term(333) + r1(vrdav_Rr, a,j) * s2(b,p,j,i) * t2(b,q,k,i) * wm_interm_3_pt4(a, k)
end do 
end do 
end do 
end do 
end do 

term(328) = term(328) * 8.0d+0 
term(329) = term(329) * (-16.0d+0) 
term(330) = term(330) * (-8.0d+0) 
term(331) = term(331) * 4.0d+0 
term(332) = term(332) * 8.0d+0 
term(333) = term(333) * (-4.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(334) = term(334) + r1(vrdav_Rl, a,j) * r2(vrdav_Rr, b,j,p,i) * s2(b,q,l,k) * wm_interm_45_pt4(a, i, k, l)
term(335) = term(335) + r1(vrdav_Rl, a,i) * r2(vrdav_Rr, b,j,p,i) * s2(b,q,l,k) * wm_interm_45_pt4(a, j, k, l)
term(336) = term(336) + r1(vrdav_Rl, a,j) * r2(vrdav_Rr, b,k,p,i) * s2(b,q,l,k) * wm_interm_45_pt4(a, i, j, l)
term(337) = term(337) + r1(vrdav_Rl, a,j) * s2(b,p,i,k) * t2(b,q,l,i) * wm_interm_9_pt4(a, k, j, l)
term(338) = term(338) + r1(vrdav_Rl, a,j) * s2(b,p,i,k) * t2(b,q,l,i) * wm_interm_9_pt4(a, j, k, l)
term(339) = term(339) + r1(vrdav_Rr, a,j) * s2(b,p,l,k) * t2(b,q,k,i) * wm_interm_8_pt4(a, i, j, l)
term(340) = term(340) + r1(vrdav_Rr, a,j) * s2(b,p,l,k) * t2(b,q,k,i) * wm_interm_8_pt4(a, j, i, l)
term(341) = term(341) + r1(vrdav_Rr, a,j) * s2(b,p,l,k) * t2(b,q,k,i) * wm_interm_13_pt4(a, i, j, l)
term(342) = term(342) + r1(vrdav_Rr, a,j) * s2(b,p,l,k) * t2(b,q,k,i) * wm_interm_13_pt4(a, j, i, l)
end do 
end do 
end do 
end do 
end do 
end do 

term(334) = term(334) * (-4.0d+0) 
term(335) = term(335) * 8.0d+0 
term(336) = term(336) * 8.0d+0 
term(337) = term(337) * (-4.0d+0) 
term(338) = term(338) * 8.0d+0 
term(340) = term(340) * (-2.0d+0) 
term(341) = term(341) * (-2.0d+0) 

do l = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(343) = term(343) + r1(vrdav_Rl, a,j) * r2(vrdav_Rr, b,k,p,i) * s2(b,q,l,i) * wm_interm_45_pt4(a, k, j, l)
term(344) = term(344) + r1(vrdav_Rl, a,j) * r2(vrdav_Rr, b,k,p,i) * s2(b,q,i,l) * wm_interm_45_pt4(a, k, j, l)
term(345) = term(345) + r1(vrdav_Rl, a,j) * r2(vrdav_Rr, b,k,p,i) * s2(b,q,k,l) * wm_interm_45_pt4(a, i, j, l)
term(346) = term(346) + r1(vrdav_Rl, a,j) * s2(b,p,k,i) * t2(b,q,l,i) * wm_interm_9_pt4(a, k, j, l)
end do 
end do 
end do 
end do 
end do 
end do 

term(343) = term(343) * (-16.0d+0) 
term(344) = term(344) * 8.0d+0 
term(345) = term(345) * (-16.0d+0) 
term(346) = term(346) * 8.0d+0 

do i = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(347) = term(347) + r2(vrdav_Rr, a,j,p,i) * s1(q,j) * s2(a,b,k,i) * wm_interm_22_pt4(b, k)
term(348) = term(348) + r2(vrdav_Rr, a,j,p,i) * s1(q,j) * s2(a,b,k,i) * wm_interm_27_pt4(b, k)
term(349) = term(349) + r2(vrdav_Rl, a,j,p,i) * t1(q,j) * t2(a,b,k,i) * wm_interm_19_pt4(b, k)
term(350) = term(350) + r2(vrdav_Rl, a,j,p,i) * t1(q,j) * t2(a,b,k,i) * wm_interm_21_pt4(b, k)
end do 
end do 
end do 
end do 
end do 

term(347) = term(347) * 2.0d+0 
term(348) = term(348) * (-4.0d+0) 
term(349) = term(349) * 2.0d+0 
term(350) = term(350) * (-4.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do l = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(351) = term(351) + r1(vrdav_Rl, a,j) * s2(b,p,l,k) * t2(b,q,k,i) * wm_interm_9_pt4(a, l, j, i)
term(352) = term(352) + r1(vrdav_Rl, a,j) * s2(b,p,l,k) * t2(b,q,l,i) * wm_interm_9_pt4(a, k, j, i)
term(353) = term(353) + r1(vrdav_Rr, a,j) * s2(b,p,l,k) * t2(b,q,l,i) * wm_interm_8_pt4(a, i, j, k)
term(354) = term(354) + r1(vrdav_Rr, a,j) * s2(b,p,k,i) * t2(b,q,l,i) * wm_interm_8_pt4(a, l, j, k)
term(355) = term(355) + r1(vrdav_Rr, a,j) * s2(b,p,i,k) * t2(b,q,l,i) * wm_interm_8_pt4(a, l, j, k)
term(356) = term(356) + r1(vrdav_Rr, a,j) * s2(b,p,k,i) * t2(b,q,l,i) * wm_interm_13_pt4(a, l, j, k)
term(357) = term(357) + r1(vrdav_Rr, a,j) * s2(b,p,i,k) * t2(b,q,l,i) * wm_interm_13_pt4(a, l, j, k)
term(358) = term(358) + r1(vrdav_Rr, a,j) * s2(b,p,l,k) * t2(b,q,l,i) * wm_interm_13_pt4(a, i, j, k)
end do 
end do 
end do 
end do 
end do 
end do 

term(351) = term(351) * (-4.0d+0) 
term(352) = term(352) * 8.0d+0 
term(353) = term(353) * (-2.0d+0) 
term(354) = term(354) * (-2.0d+0) 
term(356) = term(356) * 4.0d+0 
term(357) = term(357) * (-2.0d+0) 
term(358) = term(358) * 4.0d+0 

do k = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(359) = term(359) + r2(vrdav_Rr, a,j,p,i) * s1(q,j) * s2(a,b,i,k) * wm_interm_22_pt4(b, k)
term(360) = term(360) + r2(vrdav_Rr, a,j,p,i) * s1(q,i) * s2(a,b,j,k) * wm_interm_22_pt4(b, k)
term(361) = term(361) + r2(vrdav_Rr, a,j,p,i) * s1(q,j) * s2(a,b,i,k) * wm_interm_27_pt4(b, k)
term(362) = term(362) + r2(vrdav_Rr, a,j,p,i) * s1(q,i) * s2(a,b,j,k) * wm_interm_27_pt4(b, k)
term(363) = term(363) + r1(vrdav_Rl, p,i) * s2(a,b,j,k) * t2(b,q,k,i) * wm_interm_1_pt4(a, j)
term(364) = term(364) + r1(vrdav_Rl, p,i) * s2(a,b,j,k) * t2(b,q,k,i) * wm_interm_2_pt4(a, j)
term(365) = term(365) + r1(vrdav_Rl, p,i) * s2(a,b,j,k) * t2(b,q,j,i) * wm_interm_1_pt4(a, k)
term(366) = term(366) + r1(vrdav_Rl, p,i) * s2(a,b,j,k) * t2(b,q,j,i) * wm_interm_2_pt4(a, k)
term(367) = term(367) + s1(p,i) * s2(a,b,j,k) * t2(b,q,k,i) * wm_interm_48_pt4(a, j)
term(368) = term(368) + s1(p,i) * s2(a,b,j,k) * t2(b,q,k,i) * wm_interm_47_pt4(a, j)
term(369) = term(369) + s1(p,i) * s2(a,b,j,k) * t2(b,q,j,i) * wm_interm_48_pt4(a, k)
term(370) = term(370) + s1(p,i) * s2(a,b,j,k) * t2(b,q,j,i) * wm_interm_47_pt4(a, k)
term(371) = term(371) + r1(vrdav_Rr, p,i) * s2(a,q,j,i) * t2(a,b,j,k) * wm_interm_0_pt4(b, k)
term(372) = term(372) + r1(vrdav_Rr, p,i) * s2(a,q,j,i) * t2(a,b,j,k) * wm_interm_3_pt4(b, k)
term(373) = term(373) + r2(vrdav_Rl, a,j,p,i) * t1(q,i) * t2(a,b,j,k) * wm_interm_21_pt4(b, k)
term(374) = term(374) + s2(a,p,j,i) * t1(q,i) * t2(a,b,j,k) * wm_interm_50_pt4(b, k)
term(375) = term(375) + r2(vrdav_Rl, a,j,p,i) * t1(q,i) * t2(a,b,j,k) * wm_interm_19_pt4(b, k)
term(376) = term(376) + s2(a,p,j,i) * t1(q,i) * t2(a,b,j,k) * wm_interm_53_pt4(b, k)
term(377) = term(377) + r2(vrdav_Rl, a,j,p,i) * t1(q,j) * t2(a,b,i,k) * wm_interm_21_pt4(b, k)
term(378) = term(378) + r2(vrdav_Rl, a,j,p,i) * t1(q,j) * t2(a,b,i,k) * wm_interm_19_pt4(b, k)
end do 
end do 
end do 
end do 
end do 

term(359) = term(359) * (-4.0d+0) 
term(360) = term(360) * 8.0d+0 
term(361) = term(361) * 8.0d+0 
term(362) = term(362) * (-16.0d+0) 
term(363) = term(363) * 16.0d+0 
term(364) = term(364) * (-8.0d+0) 
term(365) = term(365) * (-8.0d+0) 
term(366) = term(366) * 4.0d+0 
term(367) = term(367) * (-16.0d+0) 
term(368) = term(368) * 32.0d+0 
term(369) = term(369) * 8.0d+0 
term(370) = term(370) * (-16.0d+0) 
term(371) = term(371) * (-32.0d+0) 
term(372) = term(372) * 16.0d+0 
term(373) = term(373) * (-16.0d+0) 
term(374) = term(374) * (-16.0d+0) 
term(375) = term(375) * 8.0d+0 
term(376) = term(376) * 8.0d+0 
term(377) = term(377) * 8.0d+0 
term(378) = term(378) * (-4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do l = 1, nocc 
do a = nocc + 1, nactive 
term(379) = term(379) + s1(p,i) * s2(a,b,j,k) * t2(b,q,l,j) * wm_interm_39_pt4(a, l, i, k)
term(380) = term(380) + s1(p,i) * s2(a,b,k,j) * t2(b,q,l,j) * wm_interm_39_pt4(a, l, i, k)
term(381) = term(381) + r2(vrdav_Rl, a,k,q,j) * r1(vrdav_Rr, p,i) * t2(a,b,l,k) * wm_interm_35_pt4(b, l, i, j)
term(382) = term(382) + r2(vrdav_Rl, a,k,q,j) * r1(vrdav_Rr, p,i) * t2(a,b,l,j) * wm_interm_35_pt4(b, l, i, k)
term(383) = term(383) + r1(vrdav_Rr, p,i) * s2(a,q,k,j) * t2(a,b,l,k) * wm_interm_8_pt4(b, l, i, j)
term(384) = term(384) + r1(vrdav_Rr, p,i) * s2(a,q,k,j) * t2(a,b,l,j) * wm_interm_8_pt4(b, l, i, k)
term(385) = term(385) + r2(vrdav_Rl, a,k,q,j) * r1(vrdav_Rr, p,i) * t2(a,b,l,k) * wm_interm_37_pt4(b, l, i, j)
term(386) = term(386) + r2(vrdav_Rl, a,k,q,j) * r1(vrdav_Rr, p,i) * t2(a,b,l,j) * wm_interm_37_pt4(b, l, i, k)
term(387) = term(387) + r1(vrdav_Rr, p,i) * s2(a,q,k,j) * t2(a,b,l,j) * wm_interm_13_pt4(b, l, i, k)
term(388) = term(388) + r1(vrdav_Rr, p,i) * s2(a,q,k,j) * t2(a,b,l,k) * wm_interm_13_pt4(b, l, i, j)
term(389) = term(389) + s2(a,p,k,j) * t1(q,i) * t2(a,b,l,j) * wm_interm_51_pt4(b, l, i, k)
term(390) = term(390) + s2(a,p,k,j) * t1(q,i) * t2(a,b,l,j) * wm_interm_49_pt4(b, l, i, k)
term(391) = term(391) + s2(a,p,k,j) * t1(q,i) * t2(a,b,l,k) * wm_interm_51_pt4(b, l, i, j)
term(392) = term(392) + s2(a,p,k,j) * t1(q,i) * t2(a,b,l,k) * wm_interm_49_pt4(b, l, i, j)
end do 
end do 
end do 
end do 
end do 
end do 

term(379) = term(379) * (-4.0d+0) 
term(380) = term(380) * 8.0d+0 
term(381) = term(381) * (-4.0d+0) 
term(382) = term(382) * 2.0d+0 
term(383) = term(383) * (-4.0d+0) 
term(384) = term(384) * 2.0d+0 
term(385) = term(385) * 2.0d+0 
term(386) = term(386) * (-4.0d+0) 
term(387) = term(387) * (-4.0d+0) 
term(388) = term(388) * 2.0d+0 
term(390) = term(390) * (-2.0d+0) 
term(391) = term(391) * (-2.0d+0) 

do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
term(393) = term(393) + r1(vrdav_Rr, p,i) * wm_interm_29_pt4(j, k) * wm_interm_8_pt4(q, i, k, j)
term(394) = term(394) + r1(vrdav_Rr, p,i) * wm_interm_28_pt4(j, k) * wm_interm_8_pt4(q, i, k, j)
term(395) = term(395) + t1(q,i) * wm_interm_28_pt4(j, k) * wm_interm_51_pt4(p, i, k, j)
term(396) = term(396) + t1(q,i) * wm_interm_29_pt4(j, k) * wm_interm_51_pt4(p, i, k, j)
term(397) = term(397) + t1(q,i) * wm_interm_28_pt4(j, k) * wm_interm_49_pt4(p, i, k, j)
term(398) = term(398) + t1(q,i) * wm_interm_29_pt4(j, k) * wm_interm_49_pt4(p, i, k, j)
end do 
end do 
end do 

term(393) = term(393) * 8.0d+0 
term(394) = term(394) * (-4.0d+0) 
term(395) = term(395) * (-2.0d+0) 
term(396) = term(396) * 4.0d+0 
term(398) = term(398) * (-2.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do l = 1, nocc 
do a = nocc + 1, nactive 
term(399) = term(399) + s1(p,i) * s2(a,b,k,j) * t2(b,q,l,j) * wm_interm_39_pt4(a, l, k, i)
end do 
end do 
end do 
end do 
end do 
end do 

term(399) = term(399) * (-4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do a = nocc + 1, nactive 
term(400) = term(400) + s2(a,q,j,i) * wm_interm_18_pt4(a, k, l, j) * wm_interm_39_pt4(p, l, i, k)
term(401) = term(401) + s2(a,p,j,i) * wm_interm_18_pt4(q, k, l, j) * wm_interm_39_pt4(a, l, i, k)
end do 
end do 
end do 
end do 
end do 

term(400) = term(400) * 8.0d+0 
term(401) = term(401) * (-4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(402) = term(402) + s2(a,q,j,i) * wm_interm_18_pt4(a, k, l, j) * wm_interm_39_pt4(p, l, k, i)
term(403) = term(403) + s2(a,q,j,i) * wm_interm_18_pt4(a, k, l, i) * wm_interm_39_pt4(p, l, k, j)
term(404) = term(404) + s2(a,p,j,i) * wm_interm_18_pt4(q, k, l, i) * wm_interm_39_pt4(a, l, k, j)
term(405) = term(405) + s2(a,p,j,i) * wm_interm_18_pt4(q, k, l, j) * wm_interm_39_pt4(a, l, k, i)
end do 
end do 
end do 
end do 
end do 

term(402) = term(402) * (-16.0d+0) 
term(403) = term(403) * 8.0d+0 
term(404) = term(404) * (-16.0d+0) 
term(405) = term(405) * 8.0d+0 

do k = 1, nocc 
do i = 1, nocc 
do l = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(406) = term(406) + s2(a,q,j,i) * wm_interm_18_pt4(a, k, l, i) * wm_interm_39_pt4(p, l, j, k)
term(407) = term(407) + s2(a,p,j,i) * wm_interm_18_pt4(q, k, l, i) * wm_interm_39_pt4(a, l, j, k)
end do 
end do 
end do 
end do 
end do 

term(406) = term(406) * (-4.0d+0) 
term(407) = term(407) * 8.0d+0 

do l = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(408) = term(408) + s1(p,i) * s2(a,b,k,l) * t2(b,q,k,j) * wm_interm_39_pt4(a, j, i, l)
term(409) = term(409) + r2(vrdav_Rl, a,k,q,j) * r1(vrdav_Rr, p,i) * t2(a,b,k,l) * wm_interm_37_pt4(b, l, i, j)
term(410) = term(410) + r1(vrdav_Rr, p,i) * s2(a,q,k,j) * t2(a,b,k,l) * wm_interm_8_pt4(b, l, i, j)
term(411) = term(411) + r1(vrdav_Rr, p,i) * s2(a,q,k,j) * t2(a,b,k,l) * wm_interm_13_pt4(b, l, i, j)
term(412) = term(412) + s2(a,p,k,j) * t1(q,i) * t2(a,b,k,l) * wm_interm_49_pt4(b, l, i, j)
term(413) = term(413) + s2(a,p,k,j) * t1(q,i) * t2(a,b,k,l) * wm_interm_51_pt4(b, l, i, j)
end do 
end do 
end do 
end do 
end do 
end do 

term(408) = term(408) * 8.0d+0 
term(409) = term(409) * (-8.0d+0) 
term(410) = term(410) * 8.0d+0 
term(411) = term(411) * (-4.0d+0) 
term(412) = term(412) * (-2.0d+0) 
term(413) = term(413) * 4.0d+0 

do l = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(414) = term(414) + s2(a,q,j,i) * wm_interm_18_pt4(a, k, i, l) * wm_interm_39_pt4(p, l, k, j)
term(415) = term(415) + s2(a,q,j,i) * wm_interm_26_pt4(a, k, l, j) * wm_interm_9_pt4(p, k, i, l)
term(416) = term(416) + s2(a,q,j,i) * wm_interm_26_pt4(a, k, l, j) * wm_interm_46_pt4(p, k, i, l)
term(417) = term(417) + s2(a,p,j,i) * wm_interm_18_pt4(q, k, i, l) * wm_interm_39_pt4(a, l, k, j)
term(418) = term(418) + s2(a,p,j,i) * wm_interm_26_pt4(q, k, l, j) * wm_interm_9_pt4(a, k, i, l)
term(419) = term(419) + s2(a,p,j,i) * wm_interm_26_pt4(q, k, l, j) * wm_interm_46_pt4(a, k, i, l)
term(420) = term(420) + t2(a,q,j,i) * wm_interm_20_pt4(a, k, l, j) * wm_interm_8_pt4(p, k, i, l)
term(421) = term(421) + t2(a,q,j,i) * wm_interm_20_pt4(a, k, l, j) * wm_interm_8_pt4(p, i, k, l)
term(422) = term(422) + t2(a,q,j,i) * wm_interm_20_pt4(p, k, l, j) * wm_interm_8_pt4(a, i, k, l)
term(423) = term(423) + t2(a,q,j,i) * wm_interm_20_pt4(p, k, l, j) * wm_interm_8_pt4(a, k, i, l)
term(424) = term(424) + t2(a,q,j,i) * wm_interm_13_pt4(a, i, k, l) * wm_interm_20_pt4(p, k, l, j)
term(425) = term(425) + t2(a,q,j,i) * wm_interm_13_pt4(a, k, i, l) * wm_interm_20_pt4(p, k, l, j)
term(426) = term(426) + t2(a,q,j,i) * wm_interm_13_pt4(p, i, k, l) * wm_interm_20_pt4(a, k, l, j)
term(427) = term(427) + t2(a,q,j,i) * wm_interm_13_pt4(p, k, i, l) * wm_interm_20_pt4(a, k, l, j)
end do 
end do 
end do 
end do 
end do 

term(414) = term(414) * (-4.0d+0) 
term(415) = term(415) * 8.0d+0 
term(416) = term(416) * (-16.0d+0) 
term(417) = term(417) * 8.0d+0 
term(418) = term(418) * (-4.0d+0) 
term(419) = term(419) * 8.0d+0 
term(420) = term(420) * (-2.0d+0) 
term(421) = term(421) * 4.0d+0 
term(422) = term(422) * (-2.0d+0) 
term(425) = term(425) * (-2.0d+0) 
term(426) = term(426) * (-2.0d+0) 
term(427) = term(427) * 4.0d+0 

do l = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(428) = term(428) + s2(a,q,j,i) * wm_interm_18_pt4(a, k, i, l) * wm_interm_39_pt4(p, l, j, k)
term(429) = term(429) + s2(a,q,j,i) * wm_interm_18_pt4(a, k, j, l) * wm_interm_39_pt4(p, l, i, k)
term(430) = term(430) + s2(a,p,j,i) * wm_interm_18_pt4(q, k, j, l) * wm_interm_39_pt4(a, l, i, k)
term(431) = term(431) + s2(a,p,j,i) * wm_interm_18_pt4(q, k, i, l) * wm_interm_39_pt4(a, l, j, k)
term(432) = term(432) + t2(a,q,j,i) * wm_interm_35_pt4(a, i, k, l) * wm_interm_51_pt4(p, j, l, k)
term(433) = term(433) + t2(a,q,j,i) * wm_interm_35_pt4(a, k, i, l) * wm_interm_51_pt4(p, j, l, k)
term(434) = term(434) + t2(a,q,j,i) * wm_interm_37_pt4(a, j, k, l) * wm_interm_51_pt4(p, i, l, k)
term(435) = term(435) + t2(a,q,j,i) * wm_interm_37_pt4(a, k, j, l) * wm_interm_51_pt4(p, i, l, k)
term(436) = term(436) + t2(a,q,j,i) * wm_interm_35_pt4(p, k, j, l) * wm_interm_51_pt4(a, i, l, k)
term(437) = term(437) + t2(a,q,j,i) * wm_interm_35_pt4(p, j, k, l) * wm_interm_51_pt4(a, i, l, k)
term(438) = term(438) + t2(a,q,j,i) * wm_interm_37_pt4(a, j, k, l) * wm_interm_49_pt4(p, i, l, k)
term(439) = term(439) + t2(a,q,j,i) * wm_interm_37_pt4(a, k, j, l) * wm_interm_49_pt4(p, i, l, k)
term(440) = term(440) + t2(a,q,j,i) * wm_interm_35_pt4(p, j, k, l) * wm_interm_49_pt4(a, i, l, k)
term(441) = term(441) + t2(a,q,j,i) * wm_interm_35_pt4(p, k, j, l) * wm_interm_49_pt4(a, i, l, k)
term(442) = term(442) + t2(a,q,j,i) * wm_interm_35_pt4(p, k, i, l) * wm_interm_51_pt4(a, j, l, k)
term(443) = term(443) + t2(a,q,j,i) * wm_interm_35_pt4(p, i, k, l) * wm_interm_51_pt4(a, j, l, k)
term(444) = term(444) + t2(a,q,j,i) * wm_interm_35_pt4(p, k, i, l) * wm_interm_49_pt4(a, j, l, k)
term(445) = term(445) + t2(a,q,j,i) * wm_interm_35_pt4(p, i, k, l) * wm_interm_49_pt4(a, j, l, k)
term(446) = term(446) + t2(a,q,j,i) * wm_interm_35_pt4(a, i, k, l) * wm_interm_49_pt4(p, j, l, k)
term(447) = term(447) + t2(a,q,j,i) * wm_interm_35_pt4(a, k, i, l) * wm_interm_49_pt4(p, j, l, k)
term(448) = term(448) + t2(a,q,j,i) * wm_interm_35_pt4(p, k, i, l) * wm_interm_51_pt4(a, l, j, k)
term(449) = term(449) + t2(a,q,j,i) * wm_interm_35_pt4(p, k, i, l) * wm_interm_49_pt4(a, l, j, k)
term(450) = term(450) + t2(a,q,j,i) * wm_interm_35_pt4(p, j, k, l) * wm_interm_51_pt4(a, l, i, k)
term(451) = term(451) + t2(a,q,j,i) * wm_interm_35_pt4(p, k, j, l) * wm_interm_51_pt4(a, l, i, k)
term(452) = term(452) + t2(a,q,j,i) * wm_interm_35_pt4(p, i, k, l) * wm_interm_51_pt4(a, l, j, k)
term(453) = term(453) + t2(a,q,j,i) * wm_interm_35_pt4(p, k, j, l) * wm_interm_49_pt4(a, l, i, k)
term(454) = term(454) + t2(a,q,j,i) * wm_interm_35_pt4(p, j, k, l) * wm_interm_49_pt4(a, l, i, k)
term(455) = term(455) + t2(a,q,j,i) * wm_interm_35_pt4(p, i, k, l) * wm_interm_49_pt4(a, l, j, k)
term(456) = term(456) + t2(a,q,j,i) * wm_interm_37_pt4(a, k, i, l) * wm_interm_51_pt4(p, l, j, k)
term(457) = term(457) + t2(a,q,j,i) * wm_interm_37_pt4(a, k, i, l) * wm_interm_49_pt4(p, l, j, k)
term(458) = term(458) + t2(a,q,j,i) * wm_interm_37_pt4(a, j, k, l) * wm_interm_51_pt4(p, l, i, k)
term(459) = term(459) + t2(a,q,j,i) * wm_interm_37_pt4(a, k, j, l) * wm_interm_51_pt4(p, l, i, k)
term(460) = term(460) + t2(a,q,j,i) * wm_interm_37_pt4(a, i, k, l) * wm_interm_51_pt4(p, l, j, k)
term(461) = term(461) + t2(a,q,j,i) * wm_interm_37_pt4(a, i, k, l) * wm_interm_49_pt4(p, l, j, k)
term(462) = term(462) + t2(a,q,j,i) * wm_interm_37_pt4(a, j, k, l) * wm_interm_49_pt4(p, l, i, k)
term(463) = term(463) + t2(a,q,j,i) * wm_interm_37_pt4(a, k, j, l) * wm_interm_49_pt4(p, l, i, k)
end do 
end do 
end do 
end do 
end do 

term(428) = term(428) * 8.0d+0 
term(429) = term(429) * (-4.0d+0) 
term(430) = term(430) * 8.0d+0 
term(431) = term(431) * (-4.0d+0) 
term(432) = term(432) * (-2.0d+0) 
term(434) = term(434) * (-2.0d+0) 
term(435) = term(435) * 4.0d+0 
term(437) = term(437) * (-2.0d+0) 
term(439) = term(439) * (-2.0d+0) 
term(441) = term(441) * (-2.0d+0) 
term(442) = term(442) * (-2.0d+0) 
term(443) = term(443) * 4.0d+0 
term(445) = term(445) * (-2.0d+0) 
term(447) = term(447) * (-2.0d+0) 
term(449) = term(449) * (-2.0d+0) 
term(451) = term(451) * (-2.0d+0) 
term(452) = term(452) * (-2.0d+0) 
term(454) = term(454) * (-2.0d+0) 
term(455) = term(455) * 4.0d+0 
term(457) = term(457) * (-2.0d+0) 
term(459) = term(459) * (-2.0d+0) 
term(460) = term(460) * (-2.0d+0) 
term(462) = term(462) * (-2.0d+0) 
term(463) = term(463) * 4.0d+0 

do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(464) = term(464) + r1(vrdav_Rl, p,i) * s2(a,b,j,k) * t2(b,q,i,j) * wm_interm_1_pt4(a, k)
term(465) = term(465) + r1(vrdav_Rl, p,i) * s2(a,b,j,k) * t2(b,q,i,j) * wm_interm_2_pt4(a, k)
term(466) = term(466) + s1(p,i) * s2(a,b,j,k) * t2(b,q,i,j) * wm_interm_48_pt4(a, k)
term(467) = term(467) + s1(p,i) * s2(a,b,j,k) * t2(b,q,i,j) * wm_interm_47_pt4(a, k)
term(468) = term(468) + r1(vrdav_Rr, p,i) * s2(a,q,i,j) * t2(a,b,j,k) * wm_interm_0_pt4(b, k)
term(469) = term(469) + r1(vrdav_Rr, p,i) * s2(a,q,i,j) * t2(a,b,j,k) * wm_interm_3_pt4(b, k)
term(470) = term(470) + s2(a,p,i,j) * t1(q,i) * t2(a,b,j,k) * wm_interm_50_pt4(b, k)
term(471) = term(471) + s2(a,p,i,j) * t1(q,i) * t2(a,b,j,k) * wm_interm_53_pt4(b, k)
end do 
end do 
end do 
end do 
end do 

term(464) = term(464) * 4.0d+0 
term(465) = term(465) * (-2.0d+0) 
term(466) = term(466) * (-4.0d+0) 
term(467) = term(467) * 8.0d+0 
term(468) = term(468) * 16.0d+0 
term(469) = term(469) * (-8.0d+0) 
term(470) = term(470) * 8.0d+0 
term(471) = term(471) * (-4.0d+0) 

do l = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(472) = term(472) + s2(a,q,j,i) * wm_interm_18_pt4(a, k, j, l) * wm_interm_39_pt4(p, l, k, i)
term(473) = term(473) + s2(a,p,j,i) * wm_interm_18_pt4(q, k, j, l) * wm_interm_39_pt4(a, l, k, i)
term(474) = term(474) + t2(a,q,j,i) * wm_interm_20_pt4(a, k, l, i) * wm_interm_8_pt4(p, j, k, l)
term(475) = term(475) + t2(a,q,j,i) * wm_interm_20_pt4(a, k, i, l) * wm_interm_8_pt4(p, j, k, l)
term(476) = term(476) + t2(a,q,j,i) * wm_interm_20_pt4(a, k, j, l) * wm_interm_8_pt4(p, i, k, l)
term(477) = term(477) + t2(a,q,j,i) * wm_interm_20_pt4(p, k, j, l) * wm_interm_8_pt4(a, i, k, l)
term(478) = term(478) + t2(a,q,j,i) * wm_interm_20_pt4(p, k, l, i) * wm_interm_8_pt4(a, j, k, l)
term(479) = term(479) + t2(a,q,j,i) * wm_interm_20_pt4(p, k, i, l) * wm_interm_8_pt4(a, j, k, l)
term(480) = term(480) + t2(a,q,j,i) * wm_interm_13_pt4(a, j, k, l) * wm_interm_20_pt4(p, k, l, i)
term(481) = term(481) + t2(a,q,j,i) * wm_interm_13_pt4(p, j, k, l) * wm_interm_20_pt4(a, k, l, i)
term(482) = term(482) + t2(a,q,j,i) * wm_interm_13_pt4(p, j, k, l) * wm_interm_20_pt4(a, k, i, l)
term(483) = term(483) + t2(a,q,j,i) * wm_interm_13_pt4(a, j, k, l) * wm_interm_20_pt4(p, k, i, l)
term(484) = term(484) + t2(a,q,j,i) * wm_interm_13_pt4(a, i, k, l) * wm_interm_20_pt4(p, k, j, l)
term(485) = term(485) + t2(a,q,j,i) * wm_interm_13_pt4(p, i, k, l) * wm_interm_20_pt4(a, k, j, l)
end do 
end do 
end do 
end do 
end do 

term(472) = term(472) * 8.0d+0 
term(473) = term(473) * (-4.0d+0) 
term(474) = term(474) * (-2.0d+0) 
term(476) = term(476) * (-2.0d+0) 
term(478) = term(478) * 4.0d+0 
term(479) = term(479) * (-2.0d+0) 
term(480) = term(480) * (-2.0d+0) 
term(482) = term(482) * (-2.0d+0) 
term(484) = term(484) * (-2.0d+0) 

do l = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(486) = term(486) + s2(a,q,j,i) * wm_interm_26_pt4(a, k, l, i) * wm_interm_9_pt4(p, k, j, l)
term(487) = term(487) + s2(a,q,j,i) * wm_interm_26_pt4(a, k, l, i) * wm_interm_46_pt4(p, k, j, l)
term(488) = term(488) + s2(a,q,j,i) * wm_interm_26_pt4(a, k, j, l) * wm_interm_9_pt4(p, k, i, l)
term(489) = term(489) + s2(a,q,j,i) * wm_interm_26_pt4(a, k, j, l) * wm_interm_46_pt4(p, k, i, l)
term(490) = term(490) + s2(a,q,j,i) * wm_interm_26_pt4(a, k, i, l) * wm_interm_46_pt4(p, k, j, l)
term(491) = term(491) + s2(a,q,j,i) * wm_interm_26_pt4(a, k, i, l) * wm_interm_9_pt4(p, k, j, l)
term(492) = term(492) + s2(a,p,j,i) * wm_interm_26_pt4(q, k, j, l) * wm_interm_46_pt4(a, k, i, l)
term(493) = term(493) + s2(a,p,j,i) * wm_interm_26_pt4(q, k, j, l) * wm_interm_9_pt4(a, k, i, l)
term(494) = term(494) + s2(a,p,j,i) * wm_interm_26_pt4(q, k, i, l) * wm_interm_9_pt4(a, k, j, l)
term(495) = term(495) + s2(a,p,j,i) * wm_interm_26_pt4(q, k, i, l) * wm_interm_46_pt4(a, k, j, l)
term(496) = term(496) + s2(a,p,j,i) * wm_interm_26_pt4(q, k, l, i) * wm_interm_9_pt4(a, k, j, l)
term(497) = term(497) + s2(a,p,j,i) * wm_interm_26_pt4(q, k, l, i) * wm_interm_46_pt4(a, k, j, l)
term(498) = term(498) + t2(a,q,j,i) * wm_interm_20_pt4(a, k, l, i) * wm_interm_8_pt4(p, k, j, l)
term(499) = term(499) + t2(a,q,j,i) * wm_interm_20_pt4(a, k, i, l) * wm_interm_8_pt4(p, k, j, l)
term(500) = term(500) + t2(a,q,j,i) * wm_interm_20_pt4(a, k, j, l) * wm_interm_8_pt4(p, k, i, l)
term(501) = term(501) + t2(a,q,j,i) * wm_interm_20_pt4(p, k, l, i) * wm_interm_8_pt4(a, k, j, l)
term(502) = term(502) + t2(a,q,j,i) * wm_interm_20_pt4(p, k, i, l) * wm_interm_8_pt4(a, k, j, l)
term(503) = term(503) + t2(a,q,j,i) * wm_interm_20_pt4(p, k, j, l) * wm_interm_8_pt4(a, k, i, l)
term(504) = term(504) + t2(a,q,j,i) * wm_interm_13_pt4(a, k, j, l) * wm_interm_20_pt4(p, k, l, i)
term(505) = term(505) + t2(a,q,j,i) * wm_interm_13_pt4(p, k, j, l) * wm_interm_20_pt4(a, k, l, i)
term(506) = term(506) + t2(a,q,j,i) * wm_interm_13_pt4(p, k, j, l) * wm_interm_20_pt4(a, k, i, l)
term(507) = term(507) + t2(a,q,j,i) * wm_interm_13_pt4(a, k, j, l) * wm_interm_20_pt4(p, k, i, l)
term(508) = term(508) + t2(a,q,j,i) * wm_interm_13_pt4(a, k, i, l) * wm_interm_20_pt4(p, k, j, l)
term(509) = term(509) + t2(a,q,j,i) * wm_interm_13_pt4(p, k, i, l) * wm_interm_20_pt4(a, k, j, l)
end do 
end do 
end do 
end do 
end do 

term(486) = term(486) * (-4.0d+0) 
term(487) = term(487) * 8.0d+0 
term(488) = term(488) * (-4.0d+0) 
term(489) = term(489) * 8.0d+0 
term(490) = term(490) * (-4.0d+0) 
term(491) = term(491) * 8.0d+0 
term(492) = term(492) * (-4.0d+0) 
term(493) = term(493) * 8.0d+0 
term(494) = term(494) * (-4.0d+0) 
term(495) = term(495) * 8.0d+0 
term(496) = term(496) * 8.0d+0 
term(497) = term(497) * (-16.0d+0) 
term(499) = term(499) * (-2.0d+0) 
term(501) = term(501) * (-2.0d+0) 
term(503) = term(503) * (-2.0d+0) 
term(504) = term(504) * 4.0d+0 
term(505) = term(505) * (-2.0d+0) 
term(507) = term(507) * (-2.0d+0) 
term(509) = term(509) * (-2.0d+0) 

do l = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(510) = term(510) + s1(p,i) * s2(a,b,k,l) * t2(b,q,l,j) * wm_interm_39_pt4(a, j, i, k)
term(511) = term(511) + r1(vrdav_Rl, p,i) * s2(a,b,j,k) * t2(b,q,l,j) * wm_interm_46_pt4(a, i, k, l)
term(512) = term(512) + r1(vrdav_Rl, p,i) * s2(a,b,j,k) * t2(b,q,l,j) * wm_interm_9_pt4(a, i, k, l)
term(513) = term(513) + r2(vrdav_Rl, a,k,q,j) * r1(vrdav_Rr, p,i) * t2(a,b,j,l) * wm_interm_37_pt4(b, l, i, k)
term(514) = term(514) + r2(vrdav_Rl, a,k,q,j) * r1(vrdav_Rr, p,i) * t2(a,b,j,l) * wm_interm_37_pt4(b, i, l, k)
term(515) = term(515) + r1(vrdav_Rr, p,i) * s2(a,q,k,j) * t2(a,b,j,l) * wm_interm_8_pt4(b, i, l, k)
term(516) = term(516) + r1(vrdav_Rr, p,i) * s2(a,q,k,j) * t2(a,b,j,l) * wm_interm_8_pt4(b, l, i, k)
term(517) = term(517) + r1(vrdav_Rr, p,i) * s2(a,q,k,j) * t2(a,b,j,l) * wm_interm_13_pt4(b, i, l, k)
term(518) = term(518) + r1(vrdav_Rr, p,i) * s2(a,q,k,j) * t2(a,b,j,l) * wm_interm_13_pt4(b, l, i, k)
term(519) = term(519) + s2(a,p,k,j) * t1(q,i) * t2(a,b,j,l) * wm_interm_49_pt4(b, i, l, k)
term(520) = term(520) + s2(a,p,k,j) * t1(q,i) * t2(a,b,j,l) * wm_interm_49_pt4(b, l, i, k)
term(521) = term(521) + s2(a,p,k,j) * t1(q,i) * t2(a,b,j,l) * wm_interm_51_pt4(b, i, l, k)
term(522) = term(522) + s2(a,p,k,j) * t1(q,i) * t2(a,b,j,l) * wm_interm_51_pt4(b, l, i, k)
end do 
end do 
end do 
end do 
end do 
end do 

term(510) = term(510) * (-16.0d+0) 
term(511) = term(511) * (-4.0d+0) 
term(512) = term(512) * 8.0d+0 
term(513) = term(513) * 4.0d+0 
term(514) = term(514) * (-8.0d+0) 
term(515) = term(515) * 2.0d+0 
term(516) = term(516) * (-4.0d+0) 
term(517) = term(517) * (-4.0d+0) 
term(518) = term(518) * 2.0d+0 
term(519) = term(519) * (-2.0d+0) 
term(522) = term(522) * (-2.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(523) = term(523) + r1(vrdav_Rl, a,j) * r2(vrdav_Rr, b,j,p,i) * s2(b,q,i,k) * wm_interm_17_pt4(a, k)
term(524) = term(524) + r1(vrdav_Rl, a,i) * r2(vrdav_Rr, b,j,p,i) * s2(b,q,j,k) * wm_interm_17_pt4(a, k)
term(525) = term(525) + r1(vrdav_Rl, a,j) * r2(vrdav_Rr, b,j,p,i) * s2(b,q,i,k) * wm_interm_23_pt4(a, k)
term(526) = term(526) + r1(vrdav_Rl, a,i) * r2(vrdav_Rr, b,j,p,i) * s2(b,q,j,k) * wm_interm_23_pt4(a, k)
term(527) = term(527) + r1(vrdav_Rl, a,j) * s2(b,p,i,k) * t2(b,q,j,i) * wm_interm_1_pt4(a, k)
term(528) = term(528) + r1(vrdav_Rl, a,j) * s2(b,p,i,k) * t2(b,q,j,i) * wm_interm_2_pt4(a, k)
term(529) = term(529) + r1(vrdav_Rr, a,j) * s2(b,p,j,k) * t2(b,q,k,i) * wm_interm_0_pt4(a, i)
term(530) = term(530) + r1(vrdav_Rr, a,j) * s2(b,p,j,k) * t2(b,q,k,i) * wm_interm_3_pt4(a, i)
end do 
end do 
end do 
end do 
end do 

term(523) = term(523) * (-4.0d+0) 
term(524) = term(524) * 8.0d+0 
term(525) = term(525) * 8.0d+0 
term(526) = term(526) * (-16.0d+0) 
term(527) = term(527) * 4.0d+0 
term(528) = term(528) * (-2.0d+0) 
term(529) = term(529) * (-4.0d+0) 
term(530) = term(530) * 2.0d+0 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(531) = term(531) + r2(vrdav_Rr, a,j,p,i) * s1(q,j) * wm_interm_0_pt4(a, i)
term(532) = term(532) + r2(vrdav_Rr, a,j,p,i) * s1(q,i) * wm_interm_0_pt4(a, j)
term(533) = term(533) + r2(vrdav_Rr, a,j,p,i) * s1(q,j) * wm_interm_3_pt4(a, i)
term(534) = term(534) + r2(vrdav_Rr, a,j,p,i) * s1(q,i) * wm_interm_3_pt4(a, j)
term(535) = term(535) + r2(vrdav_Rl, a,j,p,i) * t1(q,j) * wm_interm_1_pt4(a, i)
term(536) = term(536) + r2(vrdav_Rl, a,j,p,i) * t1(q,i) * wm_interm_1_pt4(a, j)
term(537) = term(537) + r2(vrdav_Rl, a,j,p,i) * t1(q,j) * wm_interm_2_pt4(a, i)
term(538) = term(538) + r2(vrdav_Rl, a,j,p,i) * t1(q,i) * wm_interm_2_pt4(a, j)
term(539) = term(539) + s2(a,q,j,i) * wm_interm_1_pt4(p, i) * wm_interm_22_pt4(a, j)
term(540) = term(540) + s2(a,q,j,i) * wm_interm_22_pt4(a, j) * wm_interm_2_pt4(p, i)
term(541) = term(541) + s2(a,q,j,i) * wm_interm_1_pt4(p, i) * wm_interm_27_pt4(a, j)
term(542) = term(542) + s2(a,q,j,i) * wm_interm_27_pt4(a, j) * wm_interm_2_pt4(p, i)
term(543) = term(543) + s2(a,p,j,i) * wm_interm_1_pt4(a, j) * wm_interm_22_pt4(q, i)
term(544) = term(544) + s2(a,p,j,i) * wm_interm_1_pt4(a, j) * wm_interm_27_pt4(q, i)
term(545) = term(545) + s2(a,p,j,i) * wm_interm_22_pt4(q, i) * wm_interm_2_pt4(a, j)
term(546) = term(546) + s2(a,p,j,i) * wm_interm_27_pt4(q, i) * wm_interm_2_pt4(a, j)
term(547) = term(547) + s2(a,p,j,i) * wm_interm_17_pt4(q, i) * wm_interm_48_pt4(a, j)
term(548) = term(548) + s2(a,p,j,i) * wm_interm_17_pt4(q, i) * wm_interm_47_pt4(a, j)
term(549) = term(549) + s2(a,p,j,i) * wm_interm_23_pt4(q, i) * wm_interm_48_pt4(a, j)
term(550) = term(550) + s2(a,p,j,i) * wm_interm_23_pt4(q, i) * wm_interm_47_pt4(a, j)
term(551) = term(551) + t2(a,q,j,i) * wm_interm_0_pt4(a, j) * wm_interm_21_pt4(p, i)
term(552) = term(552) + t2(a,q,j,i) * wm_interm_21_pt4(p, i) * wm_interm_3_pt4(a, j)
term(553) = term(553) + t2(a,q,j,i) * wm_interm_0_pt4(a, j) * wm_interm_19_pt4(p, i)
term(554) = term(554) + t2(a,q,j,i) * wm_interm_19_pt4(p, i) * wm_interm_3_pt4(a, j)
term(555) = term(555) + t2(a,q,j,i) * wm_interm_0_pt4(p, i) * wm_interm_21_pt4(a, j)
term(556) = term(556) + t2(a,q,j,i) * wm_interm_21_pt4(a, j) * wm_interm_3_pt4(p, i)
term(557) = term(557) + t2(a,q,j,i) * wm_interm_0_pt4(p, i) * wm_interm_19_pt4(a, j)
term(558) = term(558) + t2(a,q,j,i) * wm_interm_19_pt4(a, j) * wm_interm_3_pt4(p, i)
end do 
end do 
end do 

term(531) = term(531) * (-4.0d+0) 
term(532) = term(532) * 8.0d+0 
term(533) = term(533) * 2.0d+0 
term(534) = term(534) * (-4.0d+0) 
term(535) = term(535) * 4.0d+0 
term(536) = term(536) * (-8.0d+0) 
term(537) = term(537) * (-2.0d+0) 
term(538) = term(538) * 4.0d+0 
term(539) = term(539) * (-8.0d+0) 
term(540) = term(540) * 4.0d+0 
term(541) = term(541) * 16.0d+0 
term(542) = term(542) * (-8.0d+0) 
term(543) = term(543) * (-8.0d+0) 
term(544) = term(544) * 16.0d+0 
term(545) = term(545) * 4.0d+0 
term(546) = term(546) * (-8.0d+0) 
term(547) = term(547) * 8.0d+0 
term(548) = term(548) * (-16.0d+0) 
term(549) = term(549) * (-16.0d+0) 
term(550) = term(550) * 32.0d+0 
term(551) = term(551) * (-16.0d+0) 
term(552) = term(552) * 8.0d+0 
term(553) = term(553) * 8.0d+0 
term(554) = term(554) * (-4.0d+0) 
term(555) = term(555) * (-16.0d+0) 
term(556) = term(556) * 8.0d+0 
term(557) = term(557) * 8.0d+0 
term(558) = term(558) * (-4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(559) = term(559) + r1(vrdav_Rl, a,i) * r2(vrdav_Rr, b,j,p,i) * s2(b,q,k,j) * wm_interm_17_pt4(a, k)
term(560) = term(560) + r1(vrdav_Rl, a,i) * r2(vrdav_Rr, b,j,p,i) * s2(b,q,k,j) * wm_interm_23_pt4(a, k)
term(561) = term(561) + r1(vrdav_Rl, a,i) * s2(b,p,k,j) * t2(b,q,j,i) * wm_interm_1_pt4(a, k)
term(562) = term(562) + r1(vrdav_Rl, a,i) * s2(b,p,k,j) * t2(b,q,k,i) * wm_interm_1_pt4(a, j)
term(563) = term(563) + r1(vrdav_Rl, a,i) * s2(b,p,k,j) * t2(b,q,j,i) * wm_interm_2_pt4(a, k)
term(564) = term(564) + r1(vrdav_Rl, a,i) * s2(b,p,k,j) * t2(b,q,k,i) * wm_interm_2_pt4(a, j)
term(565) = term(565) + r1(vrdav_Rr, a,j) * s2(b,p,k,j) * t2(b,q,k,i) * wm_interm_0_pt4(a, i)
term(566) = term(566) + r1(vrdav_Rr, a,j) * s2(b,p,k,j) * t2(b,q,k,i) * wm_interm_3_pt4(a, i)
term(567) = term(567) + r1(vrdav_Rr, a,j) * s2(b,p,i,j) * t2(b,q,k,i) * wm_interm_0_pt4(a, k)
term(568) = term(568) + r1(vrdav_Rr, a,j) * s2(b,p,i,j) * t2(b,q,k,i) * wm_interm_3_pt4(a, k)
end do 
end do 
end do 
end do 
end do 

term(559) = term(559) * (-4.0d+0) 
term(560) = term(560) * 8.0d+0 
term(561) = term(561) * 4.0d+0 
term(562) = term(562) * (-8.0d+0) 
term(563) = term(563) * (-2.0d+0) 
term(564) = term(564) * 4.0d+0 
term(565) = term(565) * 8.0d+0 
term(566) = term(566) * (-4.0d+0) 
term(567) = term(567) * (-4.0d+0) 
term(568) = term(568) * 2.0d+0 

do l = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(569) = term(569) + r1(vrdav_Rl, a,j) * s2(b,p,k,i) * t2(b,q,l,i) * wm_interm_9_pt4(a, j, k, l)
end do 
end do 
end do 
end do 
end do 
end do 

term(569) = term(569) * (-16.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do l = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(570) = term(570) + r2(vrdav_Rl, a,k,q,j) * r1(vrdav_Rr, p,i) * t2(a,b,l,k) * wm_interm_35_pt4(b, i, l, j)
term(571) = term(571) + r2(vrdav_Rl, a,k,q,j) * r1(vrdav_Rr, p,i) * t2(a,b,l,j) * wm_interm_35_pt4(b, i, l, k)
term(572) = term(572) + r1(vrdav_Rr, p,i) * s2(a,q,k,j) * t2(a,b,l,k) * wm_interm_8_pt4(b, i, l, j)
term(573) = term(573) + r1(vrdav_Rr, p,i) * s2(a,q,k,j) * t2(a,b,l,j) * wm_interm_8_pt4(b, i, l, k)
term(574) = term(574) + r2(vrdav_Rl, a,k,q,j) * r1(vrdav_Rr, p,i) * t2(a,b,l,k) * wm_interm_37_pt4(b, i, l, j)
term(575) = term(575) + r2(vrdav_Rl, a,k,q,j) * r1(vrdav_Rr, p,i) * t2(a,b,l,j) * wm_interm_37_pt4(b, i, l, k)
term(576) = term(576) + r1(vrdav_Rr, p,i) * s2(a,q,k,j) * t2(a,b,l,j) * wm_interm_13_pt4(b, i, l, k)
term(577) = term(577) + r1(vrdav_Rr, p,i) * s2(a,q,k,j) * t2(a,b,l,k) * wm_interm_13_pt4(b, i, l, j)
term(578) = term(578) + s2(a,p,k,j) * t1(q,i) * t2(a,b,l,j) * wm_interm_49_pt4(b, i, l, k)
term(579) = term(579) + s2(a,p,k,j) * t1(q,i) * t2(a,b,l,k) * wm_interm_49_pt4(b, i, l, j)
term(580) = term(580) + s2(a,p,k,j) * t1(q,i) * t2(a,b,l,j) * wm_interm_51_pt4(b, i, l, k)
term(581) = term(581) + s2(a,p,k,j) * t1(q,i) * t2(a,b,l,k) * wm_interm_51_pt4(b, i, l, j)
end do 
end do 
end do 
end do 
end do 
end do 

term(570) = term(570) * 2.0d+0 
term(571) = term(571) * (-4.0d+0) 
term(572) = term(572) * 2.0d+0 
term(573) = term(573) * (-4.0d+0) 
term(574) = term(574) * (-4.0d+0) 
term(575) = term(575) * 2.0d+0 
term(576) = term(576) * 2.0d+0 
term(577) = term(577) * (-4.0d+0) 
term(579) = term(579) * (-2.0d+0) 
term(580) = term(580) * (-2.0d+0) 

do l = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(582) = term(582) + r1(vrdav_Rl, p,i) * s2(a,b,k,l) * t2(b,q,l,j) * wm_interm_9_pt4(a, i, k, j)
term(583) = term(583) + r1(vrdav_Rl, p,i) * s2(a,b,k,l) * t2(b,q,l,j) * wm_interm_46_pt4(a, i, k, j)
term(584) = term(584) + r1(vrdav_Rl, p,i) * s2(a,b,k,l) * t2(b,q,k,j) * wm_interm_9_pt4(a, i, l, j)
term(585) = term(585) + r1(vrdav_Rl, p,i) * s2(a,b,k,l) * t2(b,q,k,j) * wm_interm_46_pt4(a, i, l, j)
term(586) = term(586) + r1(vrdav_Rl, p,i) * s2(a,b,k,j) * t2(b,q,l,j) * wm_interm_9_pt4(a, i, k, l)
term(587) = term(587) + r1(vrdav_Rl, p,i) * s2(a,b,k,j) * t2(b,q,l,j) * wm_interm_46_pt4(a, i, k, l)
term(588) = term(588) + r2(vrdav_Rl, a,k,q,j) * r1(vrdav_Rr, p,i) * t2(a,b,k,l) * wm_interm_37_pt4(b, i, l, j)
term(589) = term(589) + r1(vrdav_Rr, p,i) * s2(a,q,k,j) * t2(a,b,k,l) * wm_interm_8_pt4(b, i, l, j)
term(590) = term(590) + r1(vrdav_Rr, p,i) * s2(a,q,k,j) * t2(a,b,k,l) * wm_interm_13_pt4(b, i, l, j)
term(591) = term(591) + s2(a,p,k,j) * t1(q,i) * t2(a,b,k,l) * wm_interm_49_pt4(b, i, l, j)
term(592) = term(592) + s2(a,p,k,j) * t1(q,i) * t2(a,b,k,l) * wm_interm_51_pt4(b, i, l, j)
end do 
end do 
end do 
end do 
end do 
end do 

term(582) = term(582) * 8.0d+0 
term(583) = term(583) * (-16.0d+0) 
term(584) = term(584) * (-4.0d+0) 
term(585) = term(585) * 8.0d+0 
term(586) = term(586) * (-4.0d+0) 
term(587) = term(587) * 8.0d+0 
term(588) = term(588) * 16.0d+0 
term(589) = term(589) * (-4.0d+0) 
term(590) = term(590) * 8.0d+0 
term(591) = term(591) * 4.0d+0 
term(592) = term(592) * (-2.0d+0) 


    calc_D_vv_wm_pt4 = zero
    do s = 0, 592
    calc_D_vv_wm_pt4 = calc_D_vv_wm_pt4 + term(s)
    end do

    end function calc_D_vv_wm_pt4
    
end module density_exc_exc_functions_pt4
