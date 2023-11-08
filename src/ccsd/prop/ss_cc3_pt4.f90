module ss_cc3_pt4
 use ccsd_transformed_integrals
    use cc3_intermediates
    use arithmetic
    use s_gen
    use basis
    use eom_vectors

        
    implicit none
        !
    ! File generated automatically on 2018-04-19 16:09:46
    !
    real(F64), dimension(:, :), allocatable :: wm_interm_0_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_1_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_2_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_3_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_4_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_5_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_6_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_7_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_8_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_9_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_10_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_11_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_12_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_13_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_14_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_15_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_16_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_17_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_18_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_19_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_20_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_21_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_22_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_23_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_24_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_25_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_26_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_27_pt4 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_28_pt4 
real(F64), dimension(:, :), allocatable :: wm_interm_29_pt4 

    contains
    
    subroutine wm_intermediates_cc3_init_pt4(nocc, nactive)
    integer, intent(in) :: nocc
    integer, intent(in) :: nactive
    allocate(wm_interm_0_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_1_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_2_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_3_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_4_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_5_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_6_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_7_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_8_pt4(1: nocc, 1: nocc))
allocate(wm_interm_9_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_10_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_11_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_12_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_13_pt4(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_14_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_15_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_16_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_17_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_18_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_19_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_20_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_21_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_22_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_23_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_24_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_25_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_26_pt4(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_27_pt4(nocc+1: nactive, 1: nocc))
allocate(wm_interm_28_pt4(nocc+1: nactive, nocc+1: nactive, 1: nocc, 1: nocc))
allocate(wm_interm_29_pt4(1: nocc, 1: nocc))
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

    end subroutine wm_intermediates_cc3_init_pt4
    
    subroutine wm_intermediates_cc3_free_pt4
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
deallocate(wm_interm_25_pt4)
deallocate(wm_interm_26_pt4)
deallocate(wm_interm_27_pt4)
deallocate(wm_interm_28_pt4)
deallocate(wm_interm_29_pt4)

    end subroutine wm_intermediates_cc3_free_pt4
    
    subroutine wm_intermediates_cc3_pt4(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr)
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
wm_interm_0_pt4(a, b) = wm_interm_0_pt4(a, b) + sum 
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
wm_interm_1_pt4(c, k) = wm_interm_1_pt4(c, k) + sum 
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
wm_interm_2_pt4(c, k) = wm_interm_2_pt4(c, k) + sum 
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
wm_interm_3_pt4(c, k) = wm_interm_3_pt4(c, k) + sum 
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
wm_interm_4_pt4(c, k) = wm_interm_4_pt4(c, k) + sum 
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
wm_interm_5_pt4(c, k) = wm_interm_5_pt4(c, k) + sum 
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
wm_interm_6_pt4(c, k) = wm_interm_6_pt4(c, k) + sum 
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
wm_interm_7_pt4(b, i, j, k) = wm_interm_7_pt4(b, i, j, k) + sum 
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
wm_interm_8_pt4(i, j) = wm_interm_8_pt4(i, j) + sum 
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
wm_interm_9_pt4(c, j, k, l) = wm_interm_9_pt4(c, j, k, l) + sum 
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
wm_interm_10_pt4(c, j, k, l) = wm_interm_10_pt4(c, j, k, l) + sum 
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
sum = sum + r2(vrdav_Rr, a,j,b,i) * t3(nocc, nactive, a,b,c,k,l,i)
end do 
end do 
end do 
wm_interm_11_pt4(c, j, k, l) = wm_interm_11_pt4(c, j, k, l) + sum 
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
sum = sum + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, a,b,c,k,j,i)
end do 
end do 
end do 
end do 
wm_interm_12_pt4(c, k) = wm_interm_12_pt4(c, k) + sum 
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
wm_interm_13_pt4(a, b) = wm_interm_13_pt4(a, b) + sum 
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
wm_interm_14_pt4(c, j, l, k) = wm_interm_14_pt4(c, j, l, k) + sum 
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
wm_interm_15_pt4(a, b, i, j) = wm_interm_15_pt4(a, b, i, j) + sum 
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
wm_interm_16_pt4(c, k) = wm_interm_16_pt4(c, k) + sum 
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
wm_interm_17_pt4(c, j, k, l) = wm_interm_17_pt4(c, j, k, l) + sum 
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
sum = sum + r2(vrdav_Rl, a,j,b,i) * t3(nocc, nactive, a,b,c,i,k,l)
end do 
end do 
end do 
wm_interm_18_pt4(c, j, k, l) = wm_interm_18_pt4(c, j, k, l) + sum 
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
wm_interm_19_pt4(c, j, k, l) = wm_interm_19_pt4(c, j, k, l) + sum 
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
wm_interm_20_pt4(c, k) = wm_interm_20_pt4(c, k) + sum 
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
wm_interm_21_pt4(c, k) = wm_interm_21_pt4(c, k) + sum 
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
wm_interm_22_pt4(b, j, i, k) = wm_interm_22_pt4(b, j, i, k) + sum 
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
wm_interm_23_pt4(b, c, k, j) = wm_interm_23_pt4(b, c, k, j) + sum 
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
wm_interm_24_pt4(b, c, j, k) = wm_interm_24_pt4(b, c, j, k) + sum 
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
wm_interm_25_pt4(b, j) = wm_interm_25_pt4(b, j) + sum 
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
sum = sum + r2(vrdav_Rl, a,j,b,i) * r1(vrdav_Rr, a,i)
end do 
end do 
wm_interm_27_pt4(b, j) = wm_interm_27_pt4(b, j) + sum 
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
wm_interm_28_pt4(b, c, j, k) = wm_interm_28_pt4(b, c, j, k) + sum 
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
wm_interm_29_pt4(i, j) = wm_interm_29_pt4(i, j) + sum 
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
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , a, i, b, j 
    real(F64), dimension(0:-1) :: term 
    term = 0.d+0 

    term = 0.d+0 
    
    calc_D_oo_wm_cc3_pt4 = zero
    do s = 0, -1
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
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , i, j, a, b 
    real(F64), dimension(0:21) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + wm_interm_22_pt4(a,i,j,p) * wm_interm_23_pt4(a,q,j,i)
term(1) = term(1) + wm_interm_22_pt4(a,i,j,p) * wm_interm_24_pt4(a,q,j,i)
term(2) = term(2) + wm_interm_22_pt4(a,i,j,p) * wm_interm_28_pt4(a,q,j,i)
term(3) = term(3) + wm_interm_26_pt4(a,i,j,p) * wm_interm_28_pt4(a,q,j,i)
end do 
end do 
end do 

term(0) = term(0) * (-1.0d+0) 
term(1) = term(1) * (-0.9999999999999999d+0) 
term(2) = term(2) * (1.9999999999999998d+0) 
term(3) = term(3) * (-3.9999999999999996d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(4) = term(4) + wm_interm_22_pt4(a,i,j,p) * wm_interm_24_pt4(a,q,i,j)
term(5) = term(5) + wm_interm_24_pt4(a,q,i,j) * wm_interm_26_pt4(a,i,j,p)
term(6) = term(6) + wm_interm_23_pt4(a,q,i,j) * wm_interm_26_pt4(a,j,i,p)
term(7) = term(7) + wm_interm_22_pt4(a,i,j,p) * wm_interm_23_pt4(a,q,i,j)
term(8) = term(8) + wm_interm_22_pt4(a,i,j,p) * wm_interm_28_pt4(a,q,i,j)
term(9) = term(9) + wm_interm_23_pt4(a,q,i,j) * wm_interm_26_pt4(a,i,j,p)
term(10) = term(10) + wm_interm_26_pt4(a,i,j,p) * wm_interm_28_pt4(a,q,i,j)
term(11) = term(11) + wm_interm_24_pt4(a,q,i,j) * wm_interm_26_pt4(a,j,i,p)
end do 
end do 
end do 

term(4) = term(4) * (1.9999999999999998d+0) 
term(5) = term(5) * (-0.9999999999999999d+0) 
term(6) = term(6) * (2.0d+0) 
term(7) = term(7) * (2.0d+0) 
term(8) = term(8) * (-3.9999999999999996d+0) 
term(9) = term(9) * (-1.0d+0) 
term(10) = term(10) * (1.9999999999999998d+0) 
term(11) = term(11) * (1.9999999999999998d+0) 

do i = 1, nocc 
term(12) = term(12) + wm_interm_12_pt4(q,i) * wm_interm_29_pt4(p,i)
term(13) = term(13) + wm_interm_16_pt4(q,i) * wm_interm_29_pt4(p,i)
term(14) = term(14) + wm_interm_20_pt4(q,i) * wm_interm_29_pt4(p,i)
term(15) = term(15) + wm_interm_21_pt4(q,i) * wm_interm_29_pt4(p,i)
end do 

term(12) = term(12) * (-1.9999999999999998d+0) 
term(13) = term(13) * (3.9999999999999996d+0) 
term(14) = term(14) * (2.0d+0) 
term(15) = term(15) * (-4.0d+0) 

do a = nocc + 1, nactive 
term(16) = term(16) + wm_interm_0_pt4(q,a) * wm_interm_1_pt4(a,p)
term(17) = term(17) + wm_interm_0_pt4(q,a) * wm_interm_2_pt4(a,p)
term(18) = term(18) + wm_interm_0_pt4(q,a) * wm_interm_3_pt4(a,p)
term(19) = term(19) + wm_interm_0_pt4(q,a) * wm_interm_4_pt4(a,p)
term(20) = term(20) + wm_interm_0_pt4(q,a) * wm_interm_5_pt4(a,p)
term(21) = term(21) + wm_interm_0_pt4(q,a) * wm_interm_6_pt4(a,p)
end do 

term(16) = term(16) * (-1.0d+0) 
term(17) = term(17) * (2.0d+0) 
term(18) = term(18) * (-1.0d+0) 
term(19) = term(19) * (2.0d+0) 
term(20) = term(20) * (2.0d+0) 
term(21) = term(21) * (-4.0d+0) 


    calc_D_ov_wm_cc3_pt4 = zero
    do s = 0, 21
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
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , j, i, a, b, k 
    real(F64), dimension(0:61) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + wm_interm_14_pt4(a,i,j,q) * wm_interm_15_pt4(p,a,i,j)
term(1) = term(1) + wm_interm_14_pt4(a,i,q,j) * wm_interm_15_pt4(p,a,i,j)
term(2) = term(2) + wm_interm_15_pt4(p,a,i,j) * wm_interm_17_pt4(a,i,j,q)
term(3) = term(3) + wm_interm_15_pt4(p,a,i,j) * wm_interm_18_pt4(a,i,q,j)
term(4) = term(4) + wm_interm_15_pt4(p,a,i,j) * wm_interm_19_pt4(a,i,q,j)
term(5) = term(5) + wm_interm_15_pt4(p,a,i,j) * wm_interm_17_pt4(a,i,q,j)
term(6) = term(6) + wm_interm_15_pt4(p,a,i,j) * wm_interm_18_pt4(a,i,j,q)
term(7) = term(7) + wm_interm_15_pt4(p,a,i,j) * wm_interm_19_pt4(a,i,j,q)
end do 
end do 
end do 

term(0) = term(0) * (1.9999999999999998d+0) 
term(1) = term(1) * (-0.9999999999999999d+0) 
term(2) = term(2) * (-0.9999999999999999d+0) 
term(3) = term(3) * (3.9999999999999996d+0) 
term(4) = term(4) * (-7.999999999999999d+0) 
term(5) = term(5) * (1.9999999999999998d+0) 
term(6) = term(6) * (-2.0d+0) 
term(7) = term(7) * (4.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(8) = term(8) + wm_interm_24_pt4(a,p,i,q) * wm_interm_25_pt4(a,i)
term(9) = term(9) + wm_interm_24_pt4(a,p,i,q) * wm_interm_27_pt4(a,i)
term(10) = term(10) + wm_interm_27_pt4(a,i) * wm_interm_28_pt4(a,p,i,q)
term(11) = term(11) + wm_interm_25_pt4(a,i) * wm_interm_28_pt4(a,p,i,q)
term(12) = term(12) + wm_interm_23_pt4(a,p,q,i) * wm_interm_25_pt4(a,i)
term(13) = term(13) + wm_interm_23_pt4(a,p,q,i) * wm_interm_27_pt4(a,i)
term(14) = term(14) + wm_interm_27_pt4(a,i) * wm_interm_28_pt4(a,p,q,i)
term(15) = term(15) + wm_interm_25_pt4(a,i) * wm_interm_28_pt4(a,p,q,i)
term(16) = term(16) + wm_interm_24_pt4(a,p,q,i) * wm_interm_27_pt4(a,i)
term(17) = term(17) + wm_interm_24_pt4(a,p,q,i) * wm_interm_25_pt4(a,i)
term(18) = term(18) + wm_interm_23_pt4(a,p,i,q) * wm_interm_27_pt4(a,i)
term(19) = term(19) + wm_interm_23_pt4(a,p,i,q) * wm_interm_25_pt4(a,i)
end do 
end do 

term(8) = term(8) * (-7.999999999999999d+0) 
term(9) = term(9) * (3.9999999999999996d+0) 
term(10) = term(10) * (-7.999999999999999d+0) 
term(11) = term(11) * (15.999999999999998d+0) 
term(12) = term(12) * (4.0d+0) 
term(13) = term(13) * (-2.0d+0) 
term(14) = term(14) * (3.9999999999999996d+0) 
term(15) = term(15) * (-7.999999999999999d+0) 
term(16) = term(16) * (-1.9999999999999998d+0) 
term(17) = term(17) * (3.9999999999999996d+0) 
term(18) = term(18) * (4.0d+0) 
term(19) = term(19) * (-8.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
term(20) = term(20) + wm_interm_8_pt4(i,j) * wm_interm_9_pt4(p,i,q,j)
term(21) = term(21) + wm_interm_10_pt4(p,i,q,j) * wm_interm_8_pt4(i,j)
term(22) = term(22) + wm_interm_11_pt4(p,i,q,j) * wm_interm_8_pt4(i,j)
term(23) = term(23) + wm_interm_11_pt4(p,i,j,q) * wm_interm_8_pt4(i,j)
term(24) = term(24) + wm_interm_8_pt4(i,j) * wm_interm_9_pt4(p,i,j,q)
term(25) = term(25) + wm_interm_10_pt4(p,i,j,q) * wm_interm_8_pt4(i,j)
term(26) = term(26) + wm_interm_18_pt4(p,i,j,q) * wm_interm_29_pt4(i,j)
term(27) = term(27) + wm_interm_19_pt4(p,i,j,q) * wm_interm_29_pt4(i,j)
term(28) = term(28) + wm_interm_17_pt4(p,i,q,j) * wm_interm_29_pt4(i,j)
term(29) = term(29) + wm_interm_14_pt4(p,i,q,j) * wm_interm_29_pt4(i,j)
term(30) = term(30) + wm_interm_14_pt4(p,i,j,q) * wm_interm_29_pt4(i,j)
term(31) = term(31) + wm_interm_17_pt4(p,i,j,q) * wm_interm_29_pt4(i,j)
term(32) = term(32) + wm_interm_18_pt4(p,i,q,j) * wm_interm_29_pt4(i,j)
term(33) = term(33) + wm_interm_19_pt4(p,i,q,j) * wm_interm_29_pt4(i,j)
end do 
end do 

term(20) = term(20) * (-2.0d+0) 
term(21) = term(21) * (4.0d+0) 
term(22) = term(22) * (-2.0d+0) 
term(23) = term(23) * (4.0d+0) 
term(24) = term(24) * (4.0d+0) 
term(25) = term(25) * (-8.0d+0) 
term(26) = term(26) * (3.9999999999999996d+0) 
term(27) = term(27) * (-7.999999999999999d+0) 
term(28) = term(28) * (-0.9999999999999999d+0) 
term(29) = term(29) * (1.9999999999999998d+0) 
term(30) = term(30) * (-0.9999999999999999d+0) 
term(31) = term(31) * (1.9999999999999998d+0) 
term(32) = term(32) * (-2.0d+0) 
term(33) = term(33) * (4.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(34) = term(34) + t3(nocc, nactive, a,b,p,q,j,i) * t1(b,k) * wm_interm_7_pt4(a,k,i,j)
term(35) = term(35) + t3(nocc, nactive, a,b,p,q,j,i) * t1(b,k) * wm_interm_7_pt4(a,k,j,i)
term(36) = term(36) + t3(nocc, nactive, a,b,p,i,j,q) * t1(b,k) * wm_interm_7_pt4(a,k,i,j)
term(37) = term(37) + t3(nocc, nactive, a,b,p,i,j,q) * t1(b,k) * wm_interm_7_pt4(a,k,j,i)
term(38) = term(38) + s1(b,j) * t3(nocc, nactive, a,b,p,q,k,i) * wm_interm_22_pt4(a,k,i,j)
term(39) = term(39) + s1(b,j) * t3(nocc, nactive, a,b,p,q,k,i) * wm_interm_26_pt4(a,k,i,j)
term(40) = term(40) + s1(b,j) * t3(nocc, nactive, a,b,p,q,k,i) * wm_interm_22_pt4(a,i,k,j)
term(41) = term(41) + s1(b,j) * t3(nocc, nactive, a,b,p,q,k,i) * wm_interm_26_pt4(a,i,k,j)
term(42) = term(42) + s1(a,j) * t3(nocc, nactive, a,b,p,q,k,i) * wm_interm_22_pt4(b,i,k,j)
term(43) = term(43) + s1(a,j) * t3(nocc, nactive, a,b,p,q,k,i) * wm_interm_22_pt4(b,k,i,j)
term(44) = term(44) + s1(a,j) * t3(nocc, nactive, a,b,p,q,k,i) * wm_interm_26_pt4(b,k,i,j)
term(45) = term(45) + s1(a,j) * t3(nocc, nactive, a,b,p,q,k,i) * wm_interm_26_pt4(b,i,k,j)
end do 
end do 
end do 
end do 
end do 

term(34) = term(34) * (-2.0d+0) 
term(35) = term(35) * (4.0d+0) 
term(36) = term(36) * (4.0d+0) 
term(37) = term(37) * (-8.0d+0) 
term(38) = term(38) * (-0.9999999999999999d+0) 
term(39) = term(39) * (1.9999999999999998d+0) 
term(40) = term(40) * (1.9999999999999998d+0) 
term(41) = term(41) * (-0.9999999999999999d+0) 
term(42) = term(42) * (-0.9999999999999999d+0) 
term(43) = term(43) * (1.9999999999999998d+0) 
term(44) = term(44) * (-0.9999999999999999d+0) 
term(45) = term(45) * (1.9999999999999998d+0) 

do i = 1, nocc 
term(46) = term(46) + wm_interm_5_pt4(p,i) * wm_interm_8_pt4(q,i)
term(47) = term(47) + wm_interm_6_pt4(p,i) * wm_interm_8_pt4(q,i)
term(48) = term(48) + wm_interm_1_pt4(p,i) * wm_interm_8_pt4(q,i)
term(49) = term(49) + wm_interm_2_pt4(p,i) * wm_interm_8_pt4(q,i)
term(50) = term(50) + wm_interm_3_pt4(p,i) * wm_interm_8_pt4(q,i)
term(51) = term(51) + wm_interm_4_pt4(p,i) * wm_interm_8_pt4(q,i)
end do 

term(46) = term(46) * (2.0d+0) 
term(47) = term(47) * (-4.0d+0) 
term(48) = term(48) * (-1.0d+0) 
term(49) = term(49) * (2.0d+0) 
term(50) = term(50) * (-1.0d+0) 
term(51) = term(51) * (2.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(52) = term(52) + t3(nocc, nactive, a,b,p,j,q,i) * t1(b,k) * wm_interm_7_pt4(a,k,j,i)
term(53) = term(53) + t3(nocc, nactive, a,b,p,j,q,i) * t1(b,k) * wm_interm_7_pt4(a,k,i,j)
end do 
end do 
end do 
end do 
end do 

term(52) = term(52) * (-2.0d+0) 
term(53) = term(53) * (4.0d+0) 

do k = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(54) = term(54) + s1(a,i) * t3(nocc, nactive, a,b,p,k,j,q) * wm_interm_22_pt4(b,k,j,i)
term(55) = term(55) + s1(a,i) * t3(nocc, nactive, a,b,p,k,j,q) * wm_interm_26_pt4(b,k,j,i)
term(56) = term(56) + s1(a,i) * t3(nocc, nactive, a,b,p,k,j,q) * wm_interm_22_pt4(b,j,k,i)
term(57) = term(57) + s1(a,i) * t3(nocc, nactive, a,b,p,k,j,q) * wm_interm_26_pt4(b,j,k,i)
end do 
end do 
end do 
end do 
end do 

term(54) = term(54) * (1.9999999999999998d+0) 
term(55) = term(55) * (-3.9999999999999996d+0) 
term(56) = term(56) * (-3.9999999999999996d+0) 
term(57) = term(57) * (1.9999999999999998d+0) 

do a = nocc + 1, nactive 
term(58) = term(58) + wm_interm_12_pt4(a,q) * wm_interm_13_pt4(p,a)
term(59) = term(59) + wm_interm_13_pt4(p,a) * wm_interm_16_pt4(a,q)
term(60) = term(60) + wm_interm_13_pt4(p,a) * wm_interm_20_pt4(a,q)
term(61) = term(61) + wm_interm_13_pt4(p,a) * wm_interm_21_pt4(a,q)
end do 

term(58) = term(58) * (-1.9999999999999998d+0) 
term(59) = term(59) * (3.9999999999999996d+0) 
term(60) = term(60) * (2.0d+0) 
term(61) = term(61) * (-4.0d+0) 


    calc_D_vo_wm_cc3_pt4 = zero
    do s = 0, 61
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
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , a, i, b, j 
    real(F64), dimension(0:-1) :: term 
    term = 0.d+0 

    term = 0.d+0 
    
    calc_D_vv_wm_cc3_pt4 = zero
    do s = 0, -1
    calc_D_vv_wm_cc3_pt4 = calc_D_vv_wm_cc3_pt4 + term(s)
    end do

    end function calc_D_vv_wm_cc3_pt4
    
    
    

  end module ss_cc3_pt4
