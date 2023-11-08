module tt_cc3_pt4
 use ccsd_transformed_integrals
    use cc3_intermediates
    use arithmetic
    use s_gen
    use basis
    use eom_vectors

    implicit none

        !
    ! File generated automatically on 2018-04-18 00:07:34
    !
    real(F64), dimension(:, :), allocatable :: wm_interm_0_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_1_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_2_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_3_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_4_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_5_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_6_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_7_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_8_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_9_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_10_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_11_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_12_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_13_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_14_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_15_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_16_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_17_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_18_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_19_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_20_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_21_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_22_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_23_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_24_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_25_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_26_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_27_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_28_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_29_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_30_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_31_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_32_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_33_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_34_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_35_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_36_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_37_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_38_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_39_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_40_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_41_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_42_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_43_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_44_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_45_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_46_triplet_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_47_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_48_triplet_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_49_triplet_pt4 

    contains
    
    subroutine wm_triplet_intermediates_cc3_init_pt4(nocc, nactive)
    integer, intent(in) :: nocc
    integer, intent(in) :: nactive
    allocate(wm_interm_0_triplet_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_1_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_2_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_3_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_4_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_5_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_6_triplet_pt4(1: nocc, 1: nocc))
allocate(wm_interm_7_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_8_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_9_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_10_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_11_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_12_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_13_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_14_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_15_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_16_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_17_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_18_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_19_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_20_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_21_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_22_triplet_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_23_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_24_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_25_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_26_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_27_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_28_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_29_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_30_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_31_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_32_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_33_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_34_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_35_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_36_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_37_triplet_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_38_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_39_triplet_pt4(1: nocc, 1: nocc))
allocate(wm_interm_40_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_41_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_42_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_43_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_44_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_45_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_46_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_47_triplet_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_48_triplet_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_49_triplet_pt4(nocc+1: nactive, 1: nocc))
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

    end subroutine wm_triplet_intermediates_cc3_init_pt4
    
    subroutine wm_triplet_intermediates_cc3_free_pt4
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
deallocate(wm_interm_17_triplet_pt4)
deallocate(wm_interm_18_triplet_pt4)
deallocate(wm_interm_19_triplet_pt4)
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
deallocate(wm_interm_38_triplet_pt4)
deallocate(wm_interm_39_triplet_pt4)
deallocate(wm_interm_40_triplet_pt4)
deallocate(wm_interm_41_triplet_pt4)
deallocate(wm_interm_42_triplet_pt4)
deallocate(wm_interm_43_triplet_pt4)
deallocate(wm_interm_44_triplet_pt4)
deallocate(wm_interm_45_triplet_pt4)
deallocate(wm_interm_46_triplet_pt4)
deallocate(wm_interm_47_triplet_pt4)
deallocate(wm_interm_48_triplet_pt4)
deallocate(wm_interm_49_triplet_pt4)

    end subroutine wm_triplet_intermediates_cc3_free_pt4
    
    subroutine wm_triplet_intermediates_cc3_pt4(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr)
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
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
wm_interm_0_triplet_pt4(a, b) = wm_interm_0_triplet_pt4(a, b) + sum 
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
wm_interm_1_triplet_pt4(c, k) = wm_interm_1_triplet_pt4(c, k) + sum 
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
wm_interm_2_triplet_pt4(c, k) = wm_interm_2_triplet_pt4(c, k) + sum 
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
wm_interm_3_triplet_pt4(c, k) = wm_interm_3_triplet_pt4(c, k) + sum 
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
wm_interm_4_triplet_pt4(b, i, j, k) = wm_interm_4_triplet_pt4(b, i, j, k) + sum 
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
wm_interm_5_triplet_pt4(b, i, j, k) = wm_interm_5_triplet_pt4(b, i, j, k) + sum 
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
wm_interm_6_triplet_pt4(i, j) = wm_interm_6_triplet_pt4(i, j) + sum 
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
sum = sum + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,k,i,l)
end do 
end do 
end do 
wm_interm_7_triplet_pt4(c, j, k, l) = wm_interm_7_triplet_pt4(c, j, k, l) + sum 
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
sum = sum + r2p(vrdav_Rr, a,j,b,i) * t3(nocc, nactive, a,b,c,k,i,l)
end do 
end do 
end do 
wm_interm_8_triplet_pt4(c, j, k, l) = wm_interm_8_triplet_pt4(c, j, k, l) + sum 
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
sum = sum + r2p(vrdav_Rr, a,j,b,i) * t3(nocc, nactive, a,b,c,i,k,l)
end do 
end do 
end do 
wm_interm_9_triplet_pt4(c, j, k, l) = wm_interm_9_triplet_pt4(c, j, k, l) + sum 
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
sum = sum + r2p(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,k,l,i)
end do 
end do 
end do 
wm_interm_10_triplet_pt4(c, j, k, l) = wm_interm_10_triplet_pt4(c, j, k, l) + sum 
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
sum = sum + r2p(vrdav_Rr, a,j,b,i) * t3(nocc, nactive, a,b,c,k,l,i)
end do 
end do 
end do 
wm_interm_11_triplet_pt4(c, j, k, l) = wm_interm_11_triplet_pt4(c, j, k, l) + sum 
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
sum = sum + r2m(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,k,i,j)
end do 
end do 
end do 
end do 
wm_interm_12_triplet_pt4(c, k) = wm_interm_12_triplet_pt4(c, k) + sum 
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
wm_interm_13_triplet_pt4(c, k) = wm_interm_13_triplet_pt4(c, k) + sum 
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
wm_interm_14_triplet_pt4(c, k) = wm_interm_14_triplet_pt4(c, k) + sum 
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
wm_interm_15_triplet_pt4(b, i, j, k) = wm_interm_15_triplet_pt4(b, i, j, k) + sum 
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
sum = sum + r2m(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,k,i,l)
end do 
end do 
end do 
wm_interm_16_triplet_pt4(c, j, k, l) = wm_interm_16_triplet_pt4(c, j, k, l) + sum 
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
sum = sum + r2m(vrdav_Rr, a,j,b,i) * t3(nocc, nactive, a,b,c,k,i,l)
end do 
end do 
end do 
wm_interm_17_triplet_pt4(c, j, k, l) = wm_interm_17_triplet_pt4(c, j, k, l) + sum 
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
sum = sum + r2m(vrdav_Rr, a,j,b,i) * t3(nocc, nactive, a,b,c,i,k,l)
end do 
end do 
end do 
wm_interm_18_triplet_pt4(c, j, k, l) = wm_interm_18_triplet_pt4(c, j, k, l) + sum 
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
sum = sum + r2m(vrdav_Rr, a,i,b,j) * t3(nocc, nactive, a,b,c,k,l,i)
end do 
end do 
end do 
wm_interm_19_triplet_pt4(c, j, k, l) = wm_interm_19_triplet_pt4(c, j, k, l) + sum 
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
sum = sum + r2m(vrdav_Rr, a,j,b,i) * t3(nocc, nactive, a,b,c,k,l,i)
end do 
end do 
end do 
wm_interm_20_triplet_pt4(c, j, k, l) = wm_interm_20_triplet_pt4(c, j, k, l) + sum 
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
sum = sum + r2p(vrdav_Rl, b,i,a,j) * t3(nocc, nactive, a,b,c,k,j,i)
end do 
end do 
end do 
end do 
wm_interm_21_triplet_pt4(c, k) = wm_interm_21_triplet_pt4(c, k) + sum 
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
wm_interm_22_triplet_pt4(a, b) = wm_interm_22_triplet_pt4(a, b) + sum 
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
sum = sum + r2p(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,b,c,k,l,i)
end do 
end do 
end do 
wm_interm_23_triplet_pt4(c, j, k, l) = wm_interm_23_triplet_pt4(c, j, k, l) + sum 
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
wm_interm_24_triplet_pt4(a, b, i, j) = wm_interm_24_triplet_pt4(a, b, i, j) + sum 
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
sum = sum + r2p(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,b,c,k,j,i)
end do 
end do 
end do 
end do 
wm_interm_25_triplet_pt4(c, k) = wm_interm_25_triplet_pt4(c, k) + sum 
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
sum = sum + r2p(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, a,b,c,k,l,i)
end do 
end do 
end do 
wm_interm_26_triplet_pt4(c, j, k, l) = wm_interm_26_triplet_pt4(c, j, k, l) + sum 
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
sum = sum + r2p(vrdav_Rl, b,j,a,i) * t3(nocc, nactive, a,b,c,k,i,l)
end do 
end do 
end do 
wm_interm_27_triplet_pt4(c, j, k, l) = wm_interm_27_triplet_pt4(c, j, k, l) + sum 
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
sum = sum + r2p(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, a,b,c,k,j,i)
end do 
end do 
end do 
end do 
wm_interm_28_triplet_pt4(c, k) = wm_interm_28_triplet_pt4(c, k) + sum 
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
sum = sum + r2p(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,b,c,k,i,l)
end do 
end do 
end do 
wm_interm_29_triplet_pt4(c, j, k, l) = wm_interm_29_triplet_pt4(c, j, k, l) + sum 
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
sum = sum + r2p(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, a,b,c,k,i,l)
end do 
end do 
end do 
wm_interm_30_triplet_pt4(c, j, k, l) = wm_interm_30_triplet_pt4(c, j, k, l) + sum 
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
wm_interm_31_triplet_pt4(b, i, j, k) = wm_interm_31_triplet_pt4(b, i, j, k) + sum 
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
wm_interm_32_triplet_pt4(b, c, k, j) = wm_interm_32_triplet_pt4(b, c, k, j) + sum 
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
sum = sum + r2p(vrdav_Rl, a,i,b,j) * r1(vrdav_Rr, a,k)
end do 
wm_interm_33_triplet_pt4(b, i, j, k) = wm_interm_33_triplet_pt4(b, i, j, k) + sum 
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
sum = sum + s1(a,i) * t3(nocc, nactive, a,b,c,j,i,k)
end do 
end do 
wm_interm_34_triplet_pt4(b, c, j, k) = wm_interm_34_triplet_pt4(b, c, j, k) + sum 
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
sum = sum + r2p(vrdav_Rl, a,j,b,i) * r1(vrdav_Rr, a,i)
end do 
end do 
wm_interm_35_triplet_pt4(b, j) = wm_interm_35_triplet_pt4(b, j) + sum 
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
sum = sum + r2p(vrdav_Rl, b,i,a,j) * r1(vrdav_Rr, a,i)
end do 
end do 
wm_interm_36_triplet_pt4(b, j) = wm_interm_36_triplet_pt4(b, j) + sum 
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
wm_interm_37_triplet_pt4(b, c, j, k) = wm_interm_37_triplet_pt4(b, c, j, k) + sum 
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
sum = sum + r2p(vrdav_Rl, a,i,b,j) * r1(vrdav_Rr, a,i)
end do 
end do 
wm_interm_38_triplet_pt4(b, j) = wm_interm_38_triplet_pt4(b, j) + sum 
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
wm_interm_39_triplet_pt4(i, j) = wm_interm_39_triplet_pt4(i, j) + sum 
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
wm_interm_40_triplet_pt4(c, k) = wm_interm_40_triplet_pt4(c, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,b,c,k,l,i)
end do 
end do 
end do 
wm_interm_41_triplet_pt4(c, j, k, l) = wm_interm_41_triplet_pt4(c, j, k, l) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,b,c,k,j,i)
end do 
end do 
end do 
end do 
wm_interm_42_triplet_pt4(c, k) = wm_interm_42_triplet_pt4(c, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, a,b,c,k,l,i)
end do 
end do 
end do 
wm_interm_43_triplet_pt4(c, j, k, l) = wm_interm_43_triplet_pt4(c, j, k, l) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * t3(nocc, nactive, a,b,c,k,i,l)
end do 
end do 
end do 
wm_interm_44_triplet_pt4(c, j, k, l) = wm_interm_44_triplet_pt4(c, j, k, l) + sum 
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
sum = sum + r2m(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, a,b,c,k,i,l)
end do 
end do 
end do 
wm_interm_45_triplet_pt4(c, j, k, l) = wm_interm_45_triplet_pt4(c, j, k, l) + sum 
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
wm_interm_46_triplet_pt4(b, j, i, k) = wm_interm_46_triplet_pt4(b, j, i, k) + sum 
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
wm_interm_47_triplet_pt4(b, i, j, k) = wm_interm_47_triplet_pt4(b, i, j, k) + sum 
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
wm_interm_48_triplet_pt4(b, j) = wm_interm_48_triplet_pt4(b, j) + sum 
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
wm_interm_49_triplet_pt4(b, j) = wm_interm_49_triplet_pt4(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 



    end subroutine wm_triplet_intermediates_cc3_pt4
    

    
    function calc_D_oo_wm_triplet_cc3_pt4(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_oo_wm_triplet_cc3_pt4
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
    
    calc_D_oo_wm_triplet_cc3_pt4 = zero
    do s = 0, 0
    calc_D_oo_wm_triplet_cc3_pt4 = calc_D_oo_wm_triplet_cc3_pt4 + term(s)
    end do

    end function calc_D_oo_wm_triplet_cc3_pt4
    
    function calc_D_ov_wm_triplet_cc3_pt4(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_ov_wm_triplet_cc3_pt4
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
    real(F64), dimension(0:28) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + wm_interm_31_triplet_pt4(a,i,j,p) * wm_interm_32_triplet_pt4(a,q,j,i)
term(1) = term(1) + wm_interm_31_triplet_pt4(a,i,j,p) * wm_interm_34_triplet_pt4(a,q,j,i)
term(2) = term(2) + wm_interm_31_triplet_pt4(a,i,j,p) * wm_interm_37_triplet_pt4(a,q,j,i)
term(3) = term(3) + wm_interm_33_triplet_pt4(a,i,j,p) * wm_interm_34_triplet_pt4(a,q,j,i)
term(4) = term(4) + wm_interm_33_triplet_pt4(a,i,j,p) * wm_interm_37_triplet_pt4(a,q,j,i)
end do 
end do 
end do 

term(0) = term(0) * (-1.0d+0) 
term(1) = term(1) * (-0.9999999999999999d+0) 
term(2) = term(2) * (1.9999999999999998d+0) 
term(3) = term(3) * (1.9999999999999998d+0) 
term(4) = term(4) * (-3.9999999999999996d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(5) = term(5) + wm_interm_33_triplet_pt4(a,i,j,p) * wm_interm_34_triplet_pt4(a,q,i,j)
term(6) = term(6) + wm_interm_32_triplet_pt4(a,q,i,j) * wm_interm_33_triplet_pt4(a,j,i,p)
term(7) = term(7) + wm_interm_32_triplet_pt4(a,q,i,j) * wm_interm_33_triplet_pt4(a,i,j,p)
term(8) = term(8) + wm_interm_33_triplet_pt4(a,i,j,p) * wm_interm_37_triplet_pt4(a,q,i,j)
term(9) = term(9) + wm_interm_32_triplet_pt4(a,q,i,j) * wm_interm_46_triplet_pt4(a,j,i,p)
term(10) = term(10) + wm_interm_34_triplet_pt4(a,q,i,j) * wm_interm_47_triplet_pt4(a,i,j,p)
term(11) = term(11) + wm_interm_32_triplet_pt4(a,q,i,j) * wm_interm_47_triplet_pt4(a,j,i,p)
term(12) = term(12) + wm_interm_34_triplet_pt4(a,q,i,j) * wm_interm_46_triplet_pt4(a,j,i,p)
term(13) = term(13) + wm_interm_37_triplet_pt4(a,q,i,j) * wm_interm_46_triplet_pt4(a,j,i,p)
term(14) = term(14) + wm_interm_32_triplet_pt4(a,q,i,j) * wm_interm_47_triplet_pt4(a,i,j,p)
term(15) = term(15) + wm_interm_37_triplet_pt4(a,q,i,j) * wm_interm_47_triplet_pt4(a,i,j,p)
term(16) = term(16) + wm_interm_34_triplet_pt4(a,q,i,j) * wm_interm_47_triplet_pt4(a,j,i,p)
term(17) = term(17) + wm_interm_37_triplet_pt4(a,q,i,j) * wm_interm_47_triplet_pt4(a,j,i,p)
end do 
end do 
end do 

term(5) = term(5) * (-0.9999999999999999d+0) 
term(6) = term(6) * (2.0d+0) 
term(7) = term(7) * (-1.0d+0) 
term(8) = term(8) * (1.9999999999999998d+0) 
term(9) = term(9) * (-2.0d+0) 
term(10) = term(10) * (-1.9999999999999998d+0) 
term(11) = term(11) * (4.0d+0) 
term(12) = term(12) * (-1.9999999999999998d+0) 
term(13) = term(13) * (3.9999999999999996d+0) 
term(14) = term(14) * (-2.0d+0) 
term(15) = term(15) * (3.9999999999999996d+0) 
term(16) = term(16) * (3.9999999999999996d+0) 
term(17) = term(17) * (-7.999999999999999d+0) 

do a = nocc + 1, nactive 
term(18) = term(18) + wm_interm_0_triplet_pt4(q,a) * wm_interm_1_triplet_pt4(a,p)
term(19) = term(19) + wm_interm_0_triplet_pt4(q,a) * wm_interm_2_triplet_pt4(a,p)
term(20) = term(20) + wm_interm_0_triplet_pt4(q,a) * wm_interm_3_triplet_pt4(a,p)
term(21) = term(21) + wm_interm_0_triplet_pt4(q,a) * wm_interm_12_triplet_pt4(a,p)
term(22) = term(22) + wm_interm_0_triplet_pt4(q,a) * wm_interm_13_triplet_pt4(a,p)
term(23) = term(23) + wm_interm_0_triplet_pt4(q,a) * wm_interm_14_triplet_pt4(a,p)
end do 

term(18) = term(18) * (-1.0d+0) 
term(19) = term(19) * (2.0d+0) 
term(20) = term(20) * (-1.0d+0) 
term(21) = term(21) * (-2.0d+0) 
term(22) = term(22) * (4.0d+0) 
term(23) = term(23) * (-2.0d+0) 

do i = 1, nocc 
term(24) = term(24) + wm_interm_21_triplet_pt4(q,i) * wm_interm_39_triplet_pt4(p,i)
term(25) = term(25) + wm_interm_28_triplet_pt4(q,i) * wm_interm_39_triplet_pt4(p,i)
term(26) = term(26) + wm_interm_25_triplet_pt4(q,i) * wm_interm_39_triplet_pt4(p,i)
term(27) = term(27) + wm_interm_39_triplet_pt4(p,i) * wm_interm_40_triplet_pt4(q,i)
term(28) = term(28) + wm_interm_39_triplet_pt4(p,i) * wm_interm_42_triplet_pt4(q,i)
end do 

term(24) = term(24) * (-0.9999999999999999d+0) 
term(25) = term(25) * (-0.9999999999999999d+0) 
term(26) = term(26) * (1.9999999999999998d+0) 
term(27) = term(27) * (-3.9999999999999996d+0) 
term(28) = term(28) * (3.9999999999999996d+0) 


    calc_D_ov_wm_triplet_cc3_pt4 = zero
    do s = 0, 28
    calc_D_ov_wm_triplet_cc3_pt4 = calc_D_ov_wm_triplet_cc3_pt4 + term(s)
    end do

    end function calc_D_ov_wm_triplet_cc3_pt4
    
    function calc_D_vo_wm_triplet_cc3_pt4(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vo_wm_triplet_cc3_pt4
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , j, i, a, b, k 
    real(F64), dimension(0:123) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + wm_interm_23_triplet_pt4(a,i,q,j) * wm_interm_24_triplet_pt4(p,a,i,j)
term(1) = term(1) + wm_interm_24_triplet_pt4(p,a,i,j) * wm_interm_26_triplet_pt4(a,i,j,q)
term(2) = term(2) + wm_interm_24_triplet_pt4(p,a,i,j) * wm_interm_27_triplet_pt4(a,i,q,j)
term(3) = term(3) + wm_interm_24_triplet_pt4(p,a,i,j) * wm_interm_29_triplet_pt4(a,i,q,j)
term(4) = term(4) + wm_interm_24_triplet_pt4(p,a,i,j) * wm_interm_26_triplet_pt4(a,i,q,j)
term(5) = term(5) + wm_interm_24_triplet_pt4(p,a,i,j) * wm_interm_30_triplet_pt4(a,i,q,j)
term(6) = term(6) + wm_interm_24_triplet_pt4(p,a,i,j) * wm_interm_27_triplet_pt4(a,i,j,q)
term(7) = term(7) + wm_interm_24_triplet_pt4(p,a,i,j) * wm_interm_29_triplet_pt4(a,i,j,q)
term(8) = term(8) + wm_interm_24_triplet_pt4(p,a,i,j) * wm_interm_30_triplet_pt4(a,i,j,q)
term(9) = term(9) + wm_interm_24_triplet_pt4(p,a,i,j) * wm_interm_41_triplet_pt4(a,i,q,j)
term(10) = term(10) + wm_interm_24_triplet_pt4(p,a,i,j) * wm_interm_43_triplet_pt4(a,i,j,q)
term(11) = term(11) + wm_interm_24_triplet_pt4(p,a,i,j) * wm_interm_44_triplet_pt4(a,i,q,j)
term(12) = term(12) + wm_interm_24_triplet_pt4(p,a,i,j) * wm_interm_43_triplet_pt4(a,i,q,j)
term(13) = term(13) + wm_interm_24_triplet_pt4(p,a,i,j) * wm_interm_45_triplet_pt4(a,i,q,j)
term(14) = term(14) + wm_interm_24_triplet_pt4(p,a,i,j) * wm_interm_44_triplet_pt4(a,i,j,q)
term(15) = term(15) + wm_interm_24_triplet_pt4(p,a,i,j) * wm_interm_45_triplet_pt4(a,i,j,q)
end do 
end do 
end do 

term(0) = term(0) * (-0.9999999999999999d+0) 
term(1) = term(1) * (-0.9999999999999999d+0) 
term(2) = term(2) * (1.9999999999999998d+0) 
term(3) = term(3) * (1.9999999999999998d+0) 
term(4) = term(4) * (1.9999999999999998d+0) 
term(5) = term(5) * (-3.9999999999999996d+0) 
term(6) = term(6) * (-1.0d+0) 
term(7) = term(7) * (-1.0d+0) 
term(8) = term(8) * (2.0d+0) 
term(9) = term(9) * (-1.9999999999999998d+0) 
term(10) = term(10) * (-1.9999999999999998d+0) 
term(11) = term(11) * (7.999999999999999d+0) 
term(12) = term(12) * (3.9999999999999996d+0) 
term(13) = term(13) * (-7.999999999999999d+0) 
term(14) = term(14) * (-4.0d+0) 
term(15) = term(15) * (4.0d+0) 

do i = 1, nocc 
term(16) = term(16) + wm_interm_1_triplet_pt4(p,i) * wm_interm_6_triplet_pt4(q,i)
term(17) = term(17) + wm_interm_2_triplet_pt4(p,i) * wm_interm_6_triplet_pt4(q,i)
term(18) = term(18) + wm_interm_3_triplet_pt4(p,i) * wm_interm_6_triplet_pt4(q,i)
term(19) = term(19) + wm_interm_12_triplet_pt4(p,i) * wm_interm_6_triplet_pt4(q,i)
term(20) = term(20) + wm_interm_13_triplet_pt4(p,i) * wm_interm_6_triplet_pt4(q,i)
term(21) = term(21) + wm_interm_14_triplet_pt4(p,i) * wm_interm_6_triplet_pt4(q,i)
end do 

term(16) = term(16) * (-1.0d+0) 
term(17) = term(17) * (2.0d+0) 
term(18) = term(18) * (-1.0d+0) 
term(19) = term(19) * (-2.0d+0) 
term(20) = term(20) * (4.0d+0) 
term(21) = term(21) * (-2.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(22) = term(22) + t3(nocc, nactive, a,b,p,j,q,i) * t1(b,k) * wm_interm_4_triplet_pt4(a,k,j,i)
term(23) = term(23) + t3(nocc, nactive, a,b,p,j,q,i) * t1(b,k) * wm_interm_4_triplet_pt4(a,k,i,j)
term(24) = term(24) + t3(nocc, nactive, a,b,p,j,q,i) * t1(b,k) * wm_interm_5_triplet_pt4(a,k,i,j)
term(25) = term(25) + t3(nocc, nactive, a,b,p,j,q,i) * t1(b,k) * wm_interm_15_triplet_pt4(a,k,j,i)
term(26) = term(26) + t3(nocc, nactive, a,b,p,j,q,i) * t1(b,k) * wm_interm_15_triplet_pt4(a,k,i,j)
end do 
end do 
end do 
end do 
end do 

term(22) = term(22) * (-1.0d+0) 
term(23) = term(23) * (2.0d+0) 
term(24) = term(24) * (-1.0d+0) 
term(25) = term(25) * (-3.9999999999999996d+0) 
term(26) = term(26) * (4.0d+0) 

do a = nocc + 1, nactive 
term(27) = term(27) + wm_interm_21_triplet_pt4(a,q) * wm_interm_22_triplet_pt4(p,a)
term(28) = term(28) + wm_interm_22_triplet_pt4(p,a) * wm_interm_25_triplet_pt4(a,q)
term(29) = term(29) + wm_interm_22_triplet_pt4(p,a) * wm_interm_28_triplet_pt4(a,q)
term(30) = term(30) + wm_interm_22_triplet_pt4(p,a) * wm_interm_40_triplet_pt4(a,q)
term(31) = term(31) + wm_interm_22_triplet_pt4(p,a) * wm_interm_42_triplet_pt4(a,q)
end do 

term(27) = term(27) * (-0.9999999999999999d+0) 
term(28) = term(28) * (1.9999999999999998d+0) 
term(29) = term(29) * (-0.9999999999999999d+0) 
term(30) = term(30) * (-3.9999999999999996d+0) 
term(31) = term(31) * (3.9999999999999996d+0) 

do k = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(32) = term(32) + s1(a,i) * t3(nocc, nactive, a,b,p,k,j,q) * wm_interm_31_triplet_pt4(b,k,j,i)
term(33) = term(33) + s1(a,i) * t3(nocc, nactive, a,b,p,k,j,q) * wm_interm_33_triplet_pt4(b,k,j,i)
term(34) = term(34) + s1(a,i) * t3(nocc, nactive, a,b,p,k,j,q) * wm_interm_33_triplet_pt4(b,j,k,i)
term(35) = term(35) + s1(a,i) * t3(nocc, nactive, a,b,p,k,j,q) * wm_interm_46_triplet_pt4(b,k,j,i)
term(36) = term(36) + s1(a,i) * t3(nocc, nactive, a,b,p,k,j,q) * wm_interm_47_triplet_pt4(b,k,j,i)
term(37) = term(37) + s1(a,i) * t3(nocc, nactive, a,b,p,k,j,q) * wm_interm_47_triplet_pt4(b,j,k,i)
end do 
end do 
end do 
end do 
end do 

term(32) = term(32) * (1.9999999999999998d+0) 
term(33) = term(33) * (-3.9999999999999996d+0) 
term(34) = term(34) * (1.9999999999999998d+0) 
term(35) = term(35) * (3.9999999999999996d+0) 
term(36) = term(36) * (-7.999999999999999d+0) 
term(37) = term(37) * (3.9999999999999996d+0) 

do j = 1, nocc 
do i = 1, nocc 
term(38) = term(38) + wm_interm_6_triplet_pt4(i,j) * wm_interm_7_triplet_pt4(p,i,q,j)
term(39) = term(39) + wm_interm_6_triplet_pt4(i,j) * wm_interm_8_triplet_pt4(p,i,q,j)
term(40) = term(40) + wm_interm_6_triplet_pt4(i,j) * wm_interm_9_triplet_pt4(p,i,q,j)
term(41) = term(41) + wm_interm_10_triplet_pt4(p,i,j,q) * wm_interm_6_triplet_pt4(i,j)
term(42) = term(42) + wm_interm_11_triplet_pt4(p,i,j,q) * wm_interm_6_triplet_pt4(i,j)
term(43) = term(43) + wm_interm_6_triplet_pt4(i,j) * wm_interm_7_triplet_pt4(p,i,j,q)
term(44) = term(44) + wm_interm_6_triplet_pt4(i,j) * wm_interm_8_triplet_pt4(p,i,j,q)
term(45) = term(45) + wm_interm_11_triplet_pt4(p,i,q,j) * wm_interm_6_triplet_pt4(i,j)
term(46) = term(46) + wm_interm_6_triplet_pt4(i,j) * wm_interm_9_triplet_pt4(p,i,j,q)
term(47) = term(47) + wm_interm_16_triplet_pt4(p,i,q,j) * wm_interm_6_triplet_pt4(i,j)
term(48) = term(48) + wm_interm_17_triplet_pt4(p,i,q,j) * wm_interm_6_triplet_pt4(i,j)
term(49) = term(49) + wm_interm_18_triplet_pt4(p,i,q,j) * wm_interm_6_triplet_pt4(i,j)
term(50) = term(50) + wm_interm_19_triplet_pt4(p,i,j,q) * wm_interm_6_triplet_pt4(i,j)
term(51) = term(51) + wm_interm_20_triplet_pt4(p,i,j,q) * wm_interm_6_triplet_pt4(i,j)
term(52) = term(52) + wm_interm_16_triplet_pt4(p,i,j,q) * wm_interm_6_triplet_pt4(i,j)
term(53) = term(53) + wm_interm_17_triplet_pt4(p,i,j,q) * wm_interm_6_triplet_pt4(i,j)
term(54) = term(54) + wm_interm_20_triplet_pt4(p,i,q,j) * wm_interm_6_triplet_pt4(i,j)
term(55) = term(55) + wm_interm_18_triplet_pt4(p,i,j,q) * wm_interm_6_triplet_pt4(i,j)
term(56) = term(56) + wm_interm_27_triplet_pt4(p,i,j,q) * wm_interm_39_triplet_pt4(i,j)
term(57) = term(57) + wm_interm_29_triplet_pt4(p,i,j,q) * wm_interm_39_triplet_pt4(i,j)
term(58) = term(58) + wm_interm_30_triplet_pt4(p,i,j,q) * wm_interm_39_triplet_pt4(i,j)
term(59) = term(59) + wm_interm_26_triplet_pt4(p,i,q,j) * wm_interm_39_triplet_pt4(i,j)
term(60) = term(60) + wm_interm_23_triplet_pt4(p,i,j,q) * wm_interm_39_triplet_pt4(i,j)
term(61) = term(61) + wm_interm_26_triplet_pt4(p,i,j,q) * wm_interm_39_triplet_pt4(i,j)
term(62) = term(62) + wm_interm_27_triplet_pt4(p,i,q,j) * wm_interm_39_triplet_pt4(i,j)
term(63) = term(63) + wm_interm_29_triplet_pt4(p,i,q,j) * wm_interm_39_triplet_pt4(i,j)
term(64) = term(64) + wm_interm_30_triplet_pt4(p,i,q,j) * wm_interm_39_triplet_pt4(i,j)
term(65) = term(65) + wm_interm_39_triplet_pt4(i,j) * wm_interm_44_triplet_pt4(p,i,j,q)
term(66) = term(66) + wm_interm_39_triplet_pt4(i,j) * wm_interm_45_triplet_pt4(p,i,j,q)
term(67) = term(67) + wm_interm_39_triplet_pt4(i,j) * wm_interm_43_triplet_pt4(p,i,q,j)
term(68) = term(68) + wm_interm_39_triplet_pt4(i,j) * wm_interm_41_triplet_pt4(p,i,j,q)
term(69) = term(69) + wm_interm_39_triplet_pt4(i,j) * wm_interm_43_triplet_pt4(p,i,j,q)
term(70) = term(70) + wm_interm_39_triplet_pt4(i,j) * wm_interm_44_triplet_pt4(p,i,q,j)
term(71) = term(71) + wm_interm_39_triplet_pt4(i,j) * wm_interm_45_triplet_pt4(p,i,q,j)
end do 
end do 

term(38) = term(38) * (-1.0d+0) 
term(39) = term(39) * (2.0d+0) 
term(40) = term(40) * (-1.0d+0) 
term(41) = term(41) * (-1.0d+0) 
term(42) = term(42) * (2.0d+0) 
term(43) = term(43) * (2.0d+0) 
term(44) = term(44) * (-4.0d+0) 
term(45) = term(45) * (-1.0d+0) 
term(46) = term(46) * (2.0d+0) 
term(47) = term(47) * (-2.0d+0) 
term(48) = term(48) * (4.0d+0) 
term(49) = term(49) * (-2.0d+0) 
term(50) = term(50) * (-2.0d+0) 
term(51) = term(51) * (4.0d+0) 
term(52) = term(52) * (4.0d+0) 
term(53) = term(53) * (-8.0d+0) 
term(54) = term(54) * (-2.0d+0) 
term(55) = term(55) * (4.0d+0) 
term(56) = term(56) * (1.9999999999999998d+0) 
term(57) = term(57) * (1.9999999999999998d+0) 
term(58) = term(58) * (-3.9999999999999996d+0) 
term(59) = term(59) * (-0.9999999999999999d+0) 
term(60) = term(60) * (-0.9999999999999999d+0) 
term(61) = term(61) * (1.9999999999999998d+0) 
term(62) = term(62) * (-1.0d+0) 
term(63) = term(63) * (-1.0d+0) 
term(64) = term(64) * (2.0d+0) 
term(65) = term(65) * (7.999999999999999d+0) 
term(66) = term(66) * (-7.999999999999999d+0) 
term(67) = term(67) * (-1.9999999999999998d+0) 
term(68) = term(68) * (-1.9999999999999998d+0) 
term(69) = term(69) * (3.9999999999999996d+0) 
term(70) = term(70) * (-4.0d+0) 
term(71) = term(71) * (4.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(72) = term(72) + wm_interm_34_triplet_pt4(a,p,i,q) * wm_interm_35_triplet_pt4(a,i)
term(73) = term(73) + wm_interm_34_triplet_pt4(a,p,i,q) * wm_interm_36_triplet_pt4(a,i)
term(74) = term(74) + wm_interm_36_triplet_pt4(a,i) * wm_interm_37_triplet_pt4(a,p,i,q)
term(75) = term(75) + wm_interm_35_triplet_pt4(a,i) * wm_interm_37_triplet_pt4(a,p,i,q)
term(76) = term(76) + wm_interm_34_triplet_pt4(a,p,i,q) * wm_interm_38_triplet_pt4(a,i)
term(77) = term(77) + wm_interm_37_triplet_pt4(a,p,i,q) * wm_interm_38_triplet_pt4(a,i)
term(78) = term(78) + wm_interm_32_triplet_pt4(a,p,q,i) * wm_interm_35_triplet_pt4(a,i)
term(79) = term(79) + wm_interm_32_triplet_pt4(a,p,q,i) * wm_interm_36_triplet_pt4(a,i)
term(80) = term(80) + wm_interm_36_triplet_pt4(a,i) * wm_interm_37_triplet_pt4(a,p,q,i)
term(81) = term(81) + wm_interm_35_triplet_pt4(a,i) * wm_interm_37_triplet_pt4(a,p,q,i)
term(82) = term(82) + wm_interm_32_triplet_pt4(a,p,q,i) * wm_interm_38_triplet_pt4(a,i)
term(83) = term(83) + wm_interm_37_triplet_pt4(a,p,q,i) * wm_interm_38_triplet_pt4(a,i)
term(84) = term(84) + wm_interm_34_triplet_pt4(a,p,q,i) * wm_interm_36_triplet_pt4(a,i)
term(85) = term(85) + wm_interm_34_triplet_pt4(a,p,q,i) * wm_interm_35_triplet_pt4(a,i)
term(86) = term(86) + wm_interm_34_triplet_pt4(a,p,q,i) * wm_interm_38_triplet_pt4(a,i)
term(87) = term(87) + wm_interm_32_triplet_pt4(a,p,i,q) * wm_interm_36_triplet_pt4(a,i)
term(88) = term(88) + wm_interm_32_triplet_pt4(a,p,i,q) * wm_interm_35_triplet_pt4(a,i)
term(89) = term(89) + wm_interm_32_triplet_pt4(a,p,i,q) * wm_interm_38_triplet_pt4(a,i)
term(90) = term(90) + wm_interm_34_triplet_pt4(a,p,i,q) * wm_interm_48_triplet_pt4(a,i)
term(91) = term(91) + wm_interm_37_triplet_pt4(a,p,i,q) * wm_interm_48_triplet_pt4(a,i)
term(92) = term(92) + wm_interm_34_triplet_pt4(a,p,i,q) * wm_interm_49_triplet_pt4(a,i)
term(93) = term(93) + wm_interm_37_triplet_pt4(a,p,i,q) * wm_interm_49_triplet_pt4(a,i)
term(94) = term(94) + wm_interm_32_triplet_pt4(a,p,q,i) * wm_interm_48_triplet_pt4(a,i)
term(95) = term(95) + wm_interm_37_triplet_pt4(a,p,q,i) * wm_interm_48_triplet_pt4(a,i)
term(96) = term(96) + wm_interm_32_triplet_pt4(a,p,q,i) * wm_interm_49_triplet_pt4(a,i)
term(97) = term(97) + wm_interm_37_triplet_pt4(a,p,q,i) * wm_interm_49_triplet_pt4(a,i)
term(98) = term(98) + wm_interm_34_triplet_pt4(a,p,q,i) * wm_interm_48_triplet_pt4(a,i)
term(99) = term(99) + wm_interm_34_triplet_pt4(a,p,q,i) * wm_interm_49_triplet_pt4(a,i)
term(100) = term(100) + wm_interm_32_triplet_pt4(a,p,i,q) * wm_interm_48_triplet_pt4(a,i)
term(101) = term(101) + wm_interm_32_triplet_pt4(a,p,i,q) * wm_interm_49_triplet_pt4(a,i)
end do 
end do 

term(72) = term(72) * (1.9999999999999998d+0) 
term(73) = term(73) * (1.9999999999999998d+0) 
term(74) = term(74) * (-3.9999999999999996d+0) 
term(75) = term(75) * (-3.9999999999999996d+0) 
term(76) = term(76) * (-3.9999999999999996d+0) 
term(77) = term(77) * (7.999999999999999d+0) 
term(78) = term(78) * (-1.0d+0) 
term(79) = term(79) * (-1.0d+0) 
term(80) = term(80) * (1.9999999999999998d+0) 
term(81) = term(81) * (1.9999999999999998d+0) 
term(82) = term(82) * (2.0d+0) 
term(83) = term(83) * (-3.9999999999999996d+0) 
term(84) = term(84) * (-0.9999999999999999d+0) 
term(85) = term(85) * (-0.9999999999999999d+0) 
term(86) = term(86) * (1.9999999999999998d+0) 
term(87) = term(87) * (2.0d+0) 
term(88) = term(88) * (2.0d+0) 
term(89) = term(89) * (-4.0d+0) 
term(90) = term(90) * (7.999999999999999d+0) 
term(91) = term(91) * (-15.999999999999998d+0) 
term(92) = term(92) * (-7.999999999999999d+0) 
term(93) = term(93) * (15.999999999999998d+0) 
term(94) = term(94) * (-4.0d+0) 
term(95) = term(95) * (7.999999999999999d+0) 
term(96) = term(96) * (4.0d+0) 
term(97) = term(97) * (-7.999999999999999d+0) 
term(98) = term(98) * (-3.9999999999999996d+0) 
term(99) = term(99) * (3.9999999999999996d+0) 
term(100) = term(100) * (8.0d+0) 
term(101) = term(101) * (-8.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(102) = term(102) + t3(nocc, nactive, a,b,p,q,j,i) * t1(b,k) * wm_interm_4_triplet_pt4(a,k,i,j)
term(103) = term(103) + t3(nocc, nactive, a,b,p,q,j,i) * t1(b,k) * wm_interm_4_triplet_pt4(a,k,j,i)
term(104) = term(104) + t3(nocc, nactive, a,b,p,i,j,q) * t1(b,k) * wm_interm_4_triplet_pt4(a,k,i,j)
term(105) = term(105) + t3(nocc, nactive, a,b,p,i,j,q) * t1(b,k) * wm_interm_4_triplet_pt4(a,k,j,i)
term(106) = term(106) + t3(nocc, nactive, a,b,p,q,j,i) * t1(b,k) * wm_interm_5_triplet_pt4(a,k,j,i)
term(107) = term(107) + t3(nocc, nactive, a,b,p,i,j,q) * t1(b,k) * wm_interm_5_triplet_pt4(a,k,j,i)
term(108) = term(108) + t3(nocc, nactive, a,b,p,q,j,i) * t1(b,k) * wm_interm_15_triplet_pt4(a,k,i,j)
term(109) = term(109) + t3(nocc, nactive, a,b,p,q,j,i) * t1(b,k) * wm_interm_15_triplet_pt4(a,k,j,i)
term(110) = term(110) + t3(nocc, nactive, a,b,p,i,j,q) * t1(b,k) * wm_interm_15_triplet_pt4(a,k,i,j)
term(111) = term(111) + t3(nocc, nactive, a,b,p,i,j,q) * t1(b,k) * wm_interm_15_triplet_pt4(a,k,j,i)
term(112) = term(112) + s1(b,j) * t3(nocc, nactive, a,b,p,q,k,i) * wm_interm_31_triplet_pt4(a,k,i,j)
term(113) = term(113) + s1(b,j) * t3(nocc, nactive, a,b,p,q,k,i) * wm_interm_33_triplet_pt4(a,k,i,j)
term(114) = term(114) + s1(b,j) * t3(nocc, nactive, a,b,p,q,k,i) * wm_interm_33_triplet_pt4(a,i,k,j)
term(115) = term(115) + s1(a,j) * t3(nocc, nactive, a,b,p,q,k,i) * wm_interm_31_triplet_pt4(b,i,k,j)
term(116) = term(116) + s1(a,j) * t3(nocc, nactive, a,b,p,q,k,i) * wm_interm_33_triplet_pt4(b,k,i,j)
term(117) = term(117) + s1(a,j) * t3(nocc, nactive, a,b,p,q,k,i) * wm_interm_33_triplet_pt4(b,i,k,j)
term(118) = term(118) + s1(b,j) * t3(nocc, nactive, a,b,p,q,k,i) * wm_interm_46_triplet_pt4(a,k,i,j)
term(119) = term(119) + s1(b,j) * t3(nocc, nactive, a,b,p,q,k,i) * wm_interm_47_triplet_pt4(a,k,i,j)
term(120) = term(120) + s1(b,j) * t3(nocc, nactive, a,b,p,q,k,i) * wm_interm_47_triplet_pt4(a,i,k,j)
term(121) = term(121) + s1(a,j) * t3(nocc, nactive, a,b,p,q,k,i) * wm_interm_46_triplet_pt4(b,i,k,j)
term(122) = term(122) + s1(a,j) * t3(nocc, nactive, a,b,p,q,k,i) * wm_interm_47_triplet_pt4(b,k,i,j)
term(123) = term(123) + s1(a,j) * t3(nocc, nactive, a,b,p,q,k,i) * wm_interm_47_triplet_pt4(b,i,k,j)
end do 
end do 
end do 
end do 
end do 

term(102) = term(102) * (-1.0d+0) 
term(103) = term(103) * (2.0d+0) 
term(104) = term(104) * (2.0d+0) 
term(105) = term(105) * (-4.0d+0) 
term(106) = term(106) * (-1.0d+0) 
term(107) = term(107) * (2.0d+0) 
term(108) = term(108) * (-3.9999999999999996d+0) 
term(109) = term(109) * (4.0d+0) 
term(110) = term(110) * (7.999999999999999d+0) 
term(111) = term(111) * (-8.0d+0) 
term(112) = term(112) * (-0.9999999999999999d+0) 
term(113) = term(113) * (1.9999999999999998d+0) 
term(114) = term(114) * (-0.9999999999999999d+0) 
term(115) = term(115) * (-0.9999999999999999d+0) 
term(116) = term(116) * (-0.9999999999999999d+0) 
term(117) = term(117) * (1.9999999999999998d+0) 
term(118) = term(118) * (-1.9999999999999998d+0) 
term(119) = term(119) * (3.9999999999999996d+0) 
term(120) = term(120) * (-1.9999999999999998d+0) 
term(121) = term(121) * (-1.9999999999999998d+0) 
term(122) = term(122) * (-1.9999999999999998d+0) 
term(123) = term(123) * (3.9999999999999996d+0) 


    calc_D_vo_wm_triplet_cc3_pt4 = zero
    do s = 0, 123
    calc_D_vo_wm_triplet_cc3_pt4 = calc_D_vo_wm_triplet_cc3_pt4 + term(s)
    end do

    end function calc_D_vo_wm_triplet_cc3_pt4
    
    function calc_D_vv_wm_triplet_cc3_pt4(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vv_wm_triplet_cc3_pt4
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
    
    calc_D_vv_wm_triplet_cc3_pt4 = zero
    do s = 0, 0
    calc_D_vv_wm_triplet_cc3_pt4 = calc_D_vv_wm_triplet_cc3_pt4 + term(s)
    end do

    end function calc_D_vv_wm_triplet_cc3_pt4
    
        

  end module tt_cc3_pt4
